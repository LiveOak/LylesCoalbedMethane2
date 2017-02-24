<!-- rmarkdown v1 -->
Exploring Overlap
=================================================
This report looks explores the overlap of genes between basins

<!--  Set the working directory to the repository's base directory; this assumes the report is nested inside of two directories.-->


<!-- Set the report-wide options, and point to the external code file. -->

<!-- Load the packages.  Suppress the output when loading packages. --> 

```r
library(grid) #For graphing
library(magrittr)
requireNamespace("dplyr")
requireNamespace("plyr")
requireNamespace("scales") #For formating values in graphs
requireNamespace("RColorBrewer")
requireNamespace("gplots") #For a simple venn diagram
requireNamespace("VennDiagram")

#####################################
```

<!-- Load any Global functions and variables declared in the R file.  Suppress the output. --> 

```r
options(show.signif.stars=F) #Turn off the annotations on p-values

pathInputLong <- "./Data/Derived/Unpacked.csv"
pathOutputGenesByBasin <- "./Data/Derived/GeneByBasin.csv"
pathVennDirectory <- "./Analysis/GeneticOverlap/Figures"
pathVennTotal <- file.path(pathVennDirectory, "AllCategories.tiff")

basinOrder <- c("Illinois", "CookInlet", "Powder")
geochipBasinVersion <- c("CookInlet"=3.2,  "Illinois"=4.0, "Powder"=4.0)

paletteBasinDark <- scales::muted(c("Illinois"="#dcebce", "CookInlet"="#fedcd4", "Powder"="#e4e2ee"), l=80, c=60) #Matches Figs #1 & #2, but unmuted)

# dput(unique(dsLong$GeneCategory))
geneCategoryRecode <- c(
  "Antibiotic resistance"   = "AntibioticResistance",
  "Bacteria phage"          = "BacteriaPhage",
  "Bioleaching"             = "Bioleaching",
  "Carbon cycling"          = "CarbonCycling",
  "Methane Production"      = "MethaneProduction",
  "Energy process"          = "EnergyProcess",
  "Metal Resistance"        = "MetalResistance",
  "Nitrogen"                = "Nitrogen",
  "Organic Remediation"     = "OrganicRemediation",
  "other category"          = "Other",
  "Phosphorus"              = "Phosphorus",
  "Stress"                  = "Stress",
  "Sulfate Reduction"       = "SulfateReduction",
  "Sulfur Oxidation"        = "SulfurOxidation",
  "virulence"               = "Virulence",
  "Missing"                 = "Missing"
)


categoriesToDisplay <- c(
  "CarbonCycling", "MethaneProduction", "EnergyProcess", "MetalResistance",
  "Nitrogen", "OrganicRemediation", "Phosphorus", "SulfateReduction", "SulfurOxidation"
)

col_types <- readr::cols_only(
  GenbankID             = readr::col_character(),  # Force it to be read as a string.
  GeneName              = readr::col_character(),
  GeneCategory          = readr::col_character(),
  Replicate             = readr::col_integer(),
  Site                  = readr::col_integer(),
  Abundance             = readr::col_double(),
  Basin                 = readr::col_character(),
  AbundanceWithFloor    = readr::col_double()
)

#####################################
```

<!-- Declare any global functions specific to a Rmd output.  Suppress the output. --> 


<!-- Load the datasets.   -->

```r
dsLong <- readr::read_csv(pathInputLong, col_types=col_types)

# sapply(dsLong, class)
#####################################
```

<!-- Tweak the datasets.   -->

```r
dsLong <- dsLong %>%
  dplyr::arrange(Basin, GeneCategory)

dsLong$GeochipVersion <- as.numeric(plyr::revalue(dsLong$Basin, replace=geochipBasinVersion))
dsLong$GeneCategory <- plyr::revalue(dsLong$GeneCategory, replace=geneCategoryRecode)

#Reorder the substrates and basins.  The default alphabetical order isn't the most intuitive
dsLong$Basin <- factor(dsLong$Basin, levels=basinOrder)

dsLongV32 <- dsLong[dsLong$GeochipVersion==3.2, ]
dsLongV40 <- dsLong[dsLong$GeochipVersion==4.0, ]

dsLong$IsV32 <- (dsLong$GeochipVersion==3.2)

dsLongBasinUnique <- dsLong %>%
  dplyr::group_by(GenbankID, GeneCategory, Basin) %>%
  dplyr::summarize(
    PositiveProbeCount = n()
  )
  
table(dsLong$Basin)
```

```

 Illinois CookInlet    Powder 
   382800     18991     85032 
```

```r
#####################################
```

# Calculate Overlap

```r
dsLong %>%
  dplyr::group_by(GeochipVersion) %>%
  dplyr::summarize(
    Count = n()
  )
```

```
# A tibble: 2 × 2
  GeochipVersion  Count
           <dbl>  <int>
1            3.2  18991
2            4.0 467832
```

```r
setdiff(x=1:9, y=c(1,4,5))
```

```
[1] 2 3 6 7 8 9
```

```r
intersect(x=1:9, y=c(0,1,4,5))
```

```
[1] 1 4 5
```

```r
commonGenbankIDs <- intersect(dsLongV32$GenbankID, dsLongV40$GenbankID)
length(commonGenbankIDs)
```

```
[1] 3566
```

```r
print(
  dsLong %>%
    dplyr::group_by(GeneCategory, GeochipVersion) %>%
    dplyr::summarize(
      Count = n()
    )
  , n=40
)
```

```
Source: local data frame [27 x 3]
Groups: GeneCategory [?]

           GeneCategory GeochipVersion  Count
                  <chr>          <dbl>  <int>
1  AntibioticResistance            3.2   1038
2  AntibioticResistance            4.0  17479
3         BacteriaPhage            4.0   2862
4           Bioleaching            4.0   2952
5         CarbonCycling            3.2   2814
6         CarbonCycling            4.0  60046
7         EnergyProcess            3.2    365
8         EnergyProcess            4.0   5318
9       MetalResistance            3.2   3735
10      MetalResistance            4.0  57384
11    MethaneProduction            3.2     47
12    MethaneProduction            4.0   1203
13              Missing            3.2    293
14             Nitrogen            3.2   2118
15             Nitrogen            4.0  40909
16   OrganicRemediation            3.2   6737
17   OrganicRemediation            4.0 120595
18                Other            3.2    432
19                Other            4.0  10360
20           Phosphorus            3.2    432
21           Phosphorus            4.0   7560
22               Stress            4.0 105907
23     SulfateReduction            3.2    794
24     SulfateReduction            4.0  13223
25      SulfurOxidation            3.2    186
26      SulfurOxidation            4.0   3191
27            Virulence            4.0  18843
```

```r
dsLong %>%
  dplyr::group_by(GeneCategory) %>%
  dplyr::summarize(
    CountTotal = scales::comma(length(IsV32)),
    CountV32   = scales::comma(sum(IsV32)),
    CountV40   = scales::comma(sum(!IsV32))
  )
```

```
# A tibble: 16 × 4
           GeneCategory CountTotal CountV32 CountV40
                  <chr>      <chr>    <chr>    <chr>
1  AntibioticResistance     18,517    1,038   17,479
2         BacteriaPhage      2,862        0    2,862
3           Bioleaching      2,952        0    2,952
4         CarbonCycling     62,860    2,814   60,046
5         EnergyProcess      5,683      365    5,318
6       MetalResistance     61,119    3,735   57,384
7     MethaneProduction      1,250       47    1,203
8               Missing        293      293        0
9              Nitrogen     43,027    2,118   40,909
10   OrganicRemediation    127,332    6,737  120,595
11                Other     10,792      432   10,360
12           Phosphorus      7,992      432    7,560
13               Stress    105,907        0  105,907
14     SulfateReduction     14,017      794   13,223
15      SulfurOxidation      3,377      186    3,191
16            Virulence     18,843        0   18,843
```

```r
dsLongBasinProbeCount <- dsLongBasinUnique %>%
  dplyr::group_by(GeneCategory) %>%
  dplyr::summarize(
    Total     = scales::comma(length(Basin)),
    Illinois  = scales::comma(sum(Basin=="Illinois")),
    CookInlet = scales::comma(sum(Basin=="CookInlet")),
    Powder    = scales::comma(sum(Basin=="Powder"))
  )
dsLongBasinProbeCount
```

```
# A tibble: 16 × 5
           GeneCategory  Total Illinois CookInlet Powder
                  <chr>  <chr>    <chr>     <chr>  <chr>
1  AntibioticResistance  2,117      945       346    826
2         BacteriaPhage    314      146         0    168
3           Bioleaching    309      156         0    153
4         CarbonCycling  6,943    3,266       913  2,764
5         EnergyProcess    645      290       113    242
6       MetalResistance  6,946    3,128     1,202  2,616
7     MethaneProduction    130       69        19     42
8               Missing     96        0        96      0
9              Nitrogen  4,842    2,219       717  1,906
10   OrganicRemediation 13,966    6,665     2,146  5,155
11                Other  1,201      560       151    490
12           Phosphorus    892      412       135    345
13               Stress 10,689    5,741         0  4,948
14     SulfateReduction  1,562      729       261    572
15      SulfurOxidation    369      177        58    134
16            Virulence  1,914    1,017         0    897
```

```r
#####################################
```

# Venn Diagrams
The Venn diagrams are [available to download on GitHub](https://github.com/LiveOak/LylesCoalbedMethane2/tree/master/Analysis/GeneticOverlap/Figures) as tiff files.


```
[1] 1
```

![plot of chunk PlotVennDiagrams](Figures/PlotVennDiagrams-1.png)

# Display Table

To [download the CSV](https://github.com/LiveOak/LylesCoalbedMethane2/blob/master/Data/Derived/GeneByBasin.csv), please go to ./Data/Derived/GeneByBasin.csv at [https://github.com/LiveOak/LylesCoalbedMethane2](https://github.com/LiveOak/LylesCoalbedMethane2).


The Illinois Basin and Cook Inlet Gas Field each contained 5 sites that DNA was amplified, while the Powder River Basin contained 4.  See the paper and supplemental information for additional details. 


```r
ds_intersection <- dplyr::bind_rows(intersection_list, .id="category")
```

```
Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character
```

```r
ds_long_intersection_basin <- dsLong %>% 
  tibble::as_tibble() %>% 
  dplyr::right_join(ds_intersection, by="GenbankID" ) %>% 
  dplyr::group_by(GeneCategory, GenbankID, Basin) %>% 
  dplyr::summarize(
    unique_site_count    = dplyr::n_distinct(Site)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join( #Force it to take the V4.0 GeneName.  Sometimes the 3.2 and 4.0 versions differ.
    dsLong %>% 
      dplyr::filter(!IsV32) %>% 
      dplyr::distinct(GenbankID, GeneName),
    by="GenbankID"
  ) %>% 
  tidyr::spread(key=Basin, value=unique_site_count) %>% 
  dplyr::filter(GeneCategory %in% categoriesToDisplay)
```


```r
# webshot::install_phantomjs()
DT::datatable(
  data         = ds_long_intersection_basin,
  filter       = "bottom",
  caption      = "Site Count for Genes Found in All Three Basins",
  escape       = FALSE,
  options      = list(pageLength=20, dom = 'tip')
)
```

![plot of chunk display-table](Figures/display-table-1.png)

```r
ds_long_intersection_basin %>% 
  # head(20) %>%
  knitr::kable(
    x           = .,
    #col.names   = c("Model", "Year", "Referral Out (children)", "referral out (adults)", "asq3", "asqse", "audio refer", "visual screen", "edinburgh total", "violence screen", "injury education"),
    col.names   = c("Gene Category", "Genbank ID", "Gene Name", "Illinois Site Count", "Cook Inlet Site Count", "Powder Site Count"),
    align       = c("l", "r", "l", "r", "r", "r"),
    row.names   = FALSE,
    format      = "markdown"
  )
```



|Gene Category      | Genbank ID|Gene Name             | Illinois Site Count| Cook Inlet Site Count| Powder Site Count|
|:------------------|----------:|:---------------------|-------------------:|---------------------:|-----------------:|
|CarbonCycling      |   10175887|amyX                  |                   5|                     5|                 4|
|CarbonCycling      |   10184710|vdh                   |                   5|                     2|                 4|
|CarbonCycling      |  103488518|xylanase              |                   5|                     3|                 4|
|CarbonCycling      |  104304159|pmoA                  |                   5|                     5|                 4|
|CarbonCycling      |  106769175|FTHFS                 |                   5|                     2|                 4|
|CarbonCycling      |  106771910|pcc                   |                   5|                     2|                 4|
|CarbonCycling      |  106889690|ara                   |                   5|                     5|                 4|
|CarbonCycling      |  108460873|exochitinase          |                   5|                     2|                 4|
|CarbonCycling      |  108766126|AceA                  |                   5|                     3|                 4|
|CarbonCycling      |  109455731|vanA                  |                   5|                     5|                 4|
|CarbonCycling      |  109456619|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  110827739|ara                   |                   5|                     2|                 4|
|CarbonCycling      |  111021049|pcc                   |                   5|                     2|                 4|
|CarbonCycling      |  111220842|CDH                   |                   5|                     2|                 4|
|CarbonCycling      |  111223515|camDCAB               |                   5|                     2|                 4|
|CarbonCycling      |  111281411|pcc                   |                   5|                     5|                 4|
|CarbonCycling      |  111611107|pcc                   |                   5|                     5|                 4|
|CarbonCycling      |  112955899|endochitinase         |                   5|                     5|                 4|
|CarbonCycling      |  113731760|pcc                   |                   5|                     2|                 4|
|CarbonCycling      |  113897923|amyA                  |                   5|                     5|                 4|
|CarbonCycling      |  113941581|amyA                  |                   5|                     2|                 4|
|CarbonCycling      |  113942145|endochitinase         |                   5|                     3|                 4|
|CarbonCycling      |  114050057|vanA                  |                   5|                     5|                 4|
|CarbonCycling      |  114228535|phenol_oxidase        |                   5|                     2|                 4|
|CarbonCycling      |  114228552|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  114320324|rubisco               |                   5|                     2|                 4|
|CarbonCycling      |  114540602|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  114706526|xylA                  |                   5|                     5|                 4|
|CarbonCycling      |  114740501|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  115256957|pcc                   |                   5|                     2|                 4|
|CarbonCycling      |  115283026|cellobiase            |                   5|                     3|                 4|
|CarbonCycling      |  115369560|acetylglucosaminidase |                   5|                     2|                 4|
|CarbonCycling      |  115391876|vanA                  |                   5|                     2|                 4|
|CarbonCycling      |  115397631|phenol_oxidase        |                   5|                     5|                 4|
|CarbonCycling      |  116253959|acetylglucosaminidase |                   5|                     5|                 4|
|CarbonCycling      |  116609951|pcc                   |                   5|                     2|                 4|
|CarbonCycling      |  116623484|xylanase              |                   5|                     5|                 4|
|CarbonCycling      |  116624778|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  116669149|AceA                  |                   5|                     3|                 4|
|CarbonCycling      |  116671523|cellobiase            |                   5|                     5|                 4|
|CarbonCycling      |  117927612|pcc                   |                   5|                     3|                 4|
|CarbonCycling      |  117935080|exoglucanase          |                   5|                     2|                 4|
|CarbonCycling      |  117998696|pcc                   |                   5|                     5|                 4|
|CarbonCycling      |  118168639|CDH                   |                   5|                     5|                 4|
|CarbonCycling      |  118173204|pcc                   |                   5|                     2|                 4|
|CarbonCycling      |  118462569|CDH                   |                   5|                     3|                 4|
|CarbonCycling      |  118462966|CDH                   |                   5|                     5|                 4|
|CarbonCycling      |  118468324|CDH                   |                   5|                     5|                 4|
|CarbonCycling      |  118470516|limEH                 |                   5|                     2|                 4|
|CarbonCycling      |  118473311|amyA                  |                   5|                     2|                 4|
|CarbonCycling      |  118570209|pcc                   |                   5|                     2|                 4|
|CarbonCycling      |  118698615|rubisco               |                   5|                     5|                 4|
|CarbonCycling      |  118702815|vdh                   |                   5|                     5|                 4|
|CarbonCycling      |  118748744|vanA                  |                   5|                     5|                 4|
|CarbonCycling      |  118762970|vanA                  |                   5|                     2|                 4|
|CarbonCycling      |  119459083|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  119896978|pcc                   |                   5|                     3|                 4|
|CarbonCycling      |  119949946|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  119963087|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  120431228|phenol_oxidase        |                   5|                     2|                 4|
|CarbonCycling      |  120556372|AceA                  |                   5|                     3|                 4|
|CarbonCycling      |  121554010|pcc                   |                   5|                     3|                 4|
|CarbonCycling      |  121603385|pcc                   |                   5|                     2|                 4|
|CarbonCycling      |  121607822|pcc                   |                   5|                     5|                 4|
|CarbonCycling      |  121705728|endochitinase         |                   5|                     2|                 4|
|CarbonCycling      |  121715065|endochitinase         |                   5|                     5|                 4|
|CarbonCycling      |  123228556|phenol_oxidase        |                   5|                     2|                 4|
|CarbonCycling      |  124258127|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |    1244586|ara_fungi             |                   5|                     5|                 4|
|CarbonCycling      |  125862066|rubisco               |                   5|                     2|                 4|
|CarbonCycling      |  126032271|exochitinase          |                   5|                     2|                 4|
|CarbonCycling      |  126032285|endochitinase         |                   5|                     3|                 4|
|CarbonCycling      |  126105507|phenol_oxidase        |                   5|                     5|                 4|
|CarbonCycling      |  126236465|CDH                   |                   5|                     2|                 4|
|CarbonCycling      |   12666724|glucoamylase          |                   5|                     5|                 4|
|CarbonCycling      |  126740332|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  128168922|rubisco               |                   5|                     3|                 4|
|CarbonCycling      |  133738798|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  133778504|endochitinase         |                   5|                     3|                 4|
|CarbonCycling      |  133778506|endochitinase         |                   5|                     5|                 4|
|CarbonCycling      |  134094896|AceA                  |                   5|                     3|                 4|
|CarbonCycling      |   14024312|glucoamylase          |                   5|                     5|                 4|
|CarbonCycling      |   14026595|rubisco               |                   5|                     5|                 4|
|CarbonCycling      |  142852981|endochitinase         |                   5|                     5|                 4|
|CarbonCycling      |     144416|endoglucanase         |                   5|                     2|                 4|
|CarbonCycling      |     144770|endoglucanase         |                   5|                     2|                 4|
|CarbonCycling      |  145217052|limEH                 |                   5|                     2|                 4|
|CarbonCycling      |  145232927|endochitinase         |                   5|                     2|                 4|
|CarbonCycling      |  145557350|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  145569035|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  145577320|pulA                  |                   5|                     2|                 4|
|CarbonCycling      |  145596104|xylA                  |                   5|                     2|                 4|
|CarbonCycling      |  146308856|pcc                   |                   5|                     5|                 4|
|CarbonCycling      |  146337645|amyA                  |                   5|                     5|                 4|
|CarbonCycling      |  148499659|vanA                  |                   5|                     2|                 4|
|CarbonCycling      |  148554462|vanA                  |                   5|                     5|                 4|
|CarbonCycling      |  148556298|vanA                  |                   5|                     2|                 4|
|CarbonCycling      |  148559316|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  148657926|pcc                   |                   5|                     5|                 4|
|CarbonCycling      |  149210175|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  149377179|pcc                   |                   5|                     2|                 4|
|CarbonCycling      |  149917988|pcc                   |                   5|                     2|                 4|
|CarbonCycling      |  150014758|rubisco               |                   5|                     3|                 4|
|CarbonCycling      |  150014776|rubisco               |                   5|                     3|                 4|
|CarbonCycling      |  151362421|cellobiase            |                   5|                     5|                 4|
|CarbonCycling      |  151578025|pcc                   |                   5|                     3|                 4|
|CarbonCycling      |  151582326|cellobiase            |                   5|                     3|                 4|
|CarbonCycling      |  151938860|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  151939785|endochitinase         |                   5|                     2|                 4|
|CarbonCycling      |  152146172|rubisco               |                   5|                     2|                 4|
|CarbonCycling      |  152985552|vanA                  |                   5|                     5|                 4|
|CarbonCycling      |  153005821|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  153890549|ara                   |                   5|                     5|                 4|
|CarbonCycling      |  153893454|cellobiase            |                   5|                     5|                 4|
|CarbonCycling      |  154159886|pcc                   |                   5|                     5|                 4|
|CarbonCycling      |  154160944|rubisco               |                   5|                     5|                 4|
|CarbonCycling      |  154489166|pulA                  |                   5|                     5|                 4|
|CarbonCycling      |   15602407|amyA                  |                   5|                     5|                 4|
|CarbonCycling      |  156104506|pmoA                  |                   5|                     5|                 4|
|CarbonCycling      |  156740540|cellobiase            |                   5|                     5|                 4|
|CarbonCycling      |  156742580|xylA                  |                   5|                     2|                 4|
|CarbonCycling      |  157408057|amyA                  |                   5|                     5|                 4|
|CarbonCycling      |  157679123|rubisco               |                   5|                     5|                 4|
|CarbonCycling      |   15805432|pulA                  |                   5|                     2|                 4|
|CarbonCycling      |   15806175|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |   15807939|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  158110300|pcc                   |                   5|                     2|                 4|
|CarbonCycling      |  158330288|pcc                   |                   5|                     5|                 4|
|CarbonCycling      |   15966601|pulA                  |                   5|                     3|                 4|
|CarbonCycling      |   16041065|phenol_oxidase        |                   5|                     2|                 4|
|CarbonCycling      |   16078882|pcc                   |                   5|                     2|                 4|
|CarbonCycling      |   16126009|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  162284383|AceA                  |                   5|                     3|                 4|
|CarbonCycling      |  162952411|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |   16308743|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  169848373|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |    1790867|vanA                  |                   5|                     2|                 4|
|CarbonCycling      |  182678865|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  187928325|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  194540305|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  195941139|AceA                  |                   5|                     3|                 4|
|CarbonCycling      |  197210157|CODH                  |                   5|                     3|                 4|
|CarbonCycling      |  197210245|CODH                  |                   5|                     3|                 4|
|CarbonCycling      |  197210271|CODH                  |                   5|                     3|                 4|
|CarbonCycling      |  209516934|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |   21220991|cellobiase            |                   5|                     5|                 4|
|CarbonCycling      |   21223300|pcc                   |                   5|                     2|                 4|
|CarbonCycling      |  218717685|AceA                  |                   5|                     3|                 4|
|CarbonCycling      |   21929222|mnp                   |                   5|                     5|                 4|
|CarbonCycling      |   21929226|lip                   |                   5|                     5|                 4|
|CarbonCycling      |   22295628|xylanase              |                   5|                     5|                 4|
|CarbonCycling      |  224223697|rubisco               |                   5|                     3|                 4|
|CarbonCycling      |  224579026|rubisco               |                   5|                     3|                 4|
|CarbonCycling      |  224824616|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  225083263|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  225105854|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  225138011|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  225199160|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  226870051|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  226939578|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  227079376|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |  227329764|AceA                  |                   5|                     3|                 4|
|CarbonCycling      |  227428786|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |   23267183|endoglucanase         |                   5|                     5|                 4|
|CarbonCycling      |   27381288|cellobiase            |                   5|                     5|                 4|
|CarbonCycling      |   28273025|mnp                   |                   5|                     2|                 4|
|CarbonCycling      |   28808852|pcc                   |                   5|                     5|                 4|
|CarbonCycling      |   28809545|endochitinase         |                   5|                     2|                 4|
|CarbonCycling      |   28853329|vanA                  |                   5|                     2|                 4|
|CarbonCycling      |   28901448|amyA                  |                   5|                     2|                 4|
|CarbonCycling      |   29160303|exoglucanase          |                   5|                     5|                 4|
|CarbonCycling      |   29828693|amyA                  |                   5|                     2|                 4|
|CarbonCycling      |   30721807|FTHFS                 |                   5|                     2|                 4|
|CarbonCycling      |   31414751|endochitinase         |                   5|                     5|                 4|
|CarbonCycling      |   31747164|endoglucanase         |                   5|                     5|                 4|
|CarbonCycling      |   32351176|CODH                  |                   5|                     3|                 4|
|CarbonCycling      |   32397456|xylA                  |                   5|                     2|                 4|
|CarbonCycling      |    3334811|vanA                  |                   5|                     2|                 4|
|CarbonCycling      |   33567621|rubisco               |                   5|                     2|                 4|
|CarbonCycling      |   34102804|acetylglucosaminidase |                   5|                     2|                 4|
|CarbonCycling      |   37791159|phenol_oxidase        |                   5|                     2|                 4|
|CarbonCycling      |   39936374|amyA                  |                   5|                     2|                 4|
|CarbonCycling      |   39962604|amyA                  |                   5|                     5|                 4|
|CarbonCycling      |    4115621|exochitinase          |                   5|                     5|                 4|
|CarbonCycling      |      45543|rubisco               |                   5|                     2|                 4|
|CarbonCycling      |      45822|amyA                  |                   5|                     2|                 4|
|CarbonCycling      |   46111339|mnp                   |                   5|                     3|                 4|
|CarbonCycling      |   46392670|pmoA                  |                   5|                     2|                 4|
|CarbonCycling      |   50952845|lip                   |                   5|                     5|                 4|
|CarbonCycling      |   50954092|amyA                  |                   5|                     5|                 4|
|CarbonCycling      |   50982452|pmoA                  |                   5|                     3|                 4|
|CarbonCycling      |   50982468|pmoA                  |                   5|                     2|                 4|
|CarbonCycling      |   51870942|endochitinase         |                   5|                     5|                 4|
|CarbonCycling      |    5453414|pectinase             |                   5|                     2|                 4|
|CarbonCycling      |   55231751|amyA                  |                   5|                     2|                 4|
|CarbonCycling      |   55772530|pcc                   |                   5|                     5|                 4|
|CarbonCycling      |     559906|AceA                  |                   5|                     3|                 4|
|CarbonCycling      |    5616325|lip                   |                   5|                     5|                 4|
|CarbonCycling      |   56419238|amyA                  |                   5|                     5|                 4|
|CarbonCycling      |   56609602|amyA                  |                   5|                     2|                 4|
|CarbonCycling      |   56785440|phenol_oxidase        |                   5|                     5|                 4|
|CarbonCycling      |     578092|phenol_oxidase        |                   5|                     5|                 4|
|CarbonCycling      |   60475948|rubisco               |                   5|                     5|                 4|
|CarbonCycling      |   61968003|endochitinase         |                   5|                     2|                 4|
|CarbonCycling      |   62424588|AceA                  |                   5|                     3|                 4|
|CarbonCycling      |   66845524|endoglucanase         |                   5|                     3|                 4|
|CarbonCycling      |   66964859|amyA                  |                   5|                     5|                 4|
|CarbonCycling      |   67535763|pulA                  |                   5|                     5|                 4|
|CarbonCycling      |   67985747|ara                   |                   5|                     2|                 4|
|CarbonCycling      |   68055722|pulA                  |                   5|                     2|                 4|
|CarbonCycling      |   68198514|pcc                   |                   5|                     2|                 4|
|CarbonCycling      |   68234365|pulA                  |                   5|                     2|                 4|
|CarbonCycling      |    6910983|mnp                   |                   5|                     2|                 4|
|CarbonCycling      |   69301315|FTHFS                 |                   5|                     5|                 4|
|CarbonCycling      |   70729461|endochitinase         |                   5|                     2|                 4|
|CarbonCycling      |   70984567|AceA                  |                   5|                     3|                 4|
|CarbonCycling      |   70986957|cellobiase            |                   5|                     2|                 4|
|CarbonCycling      |   70993266|AceA                  |                   5|                     3|                 4|
|CarbonCycling      |    7107367|exoglucanase          |                   5|                     5|                 4|
|CarbonCycling      |   71366038|pcc                   |                   5|                     2|                 4|
|CarbonCycling      |   71368759|xylA                  |                   5|                     2|                 4|
|CarbonCycling      |    7188931|pmoA                  |                   5|                     5|                 4|
|CarbonCycling      |   71915736|xylA                  |                   5|                     2|                 4|
|CarbonCycling      |   72161237|amyA                  |                   5|                     5|                 4|
|CarbonCycling      |   72162954|pcc                   |                   5|                     2|                 4|
|CarbonCycling      |   76258492|pulA                  |                   5|                     3|                 4|
|CarbonCycling      |   76796557|amyA                  |                   5|                     2|                 4|
|CarbonCycling      |   76876845|phenol_oxidase        |                   5|                     5|                 4|
|CarbonCycling      |   77176916|endoglucanase         |                   5|                     5|                 4|
|CarbonCycling      |   77960187|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |   77963046|amyA                  |                   5|                     5|                 4|
|CarbonCycling      |   78365391|FTHFS                 |                   5|                     2|                 4|
|CarbonCycling      |   78693100|xylA                  |                   5|                     2|                 4|
|CarbonCycling      |   82741452|FTHFS                 |                   5|                     2|                 4|
|CarbonCycling      |   83312163|pulA                  |                   5|                     2|                 4|
|CarbonCycling      |   83364899|pcc                   |                   5|                     3|                 4|
|CarbonCycling      |   83369532|CODH                  |                   5|                     2|                 4|
|CarbonCycling      |   83773727|ara_fungi             |                   5|                     5|                 4|
|CarbonCycling      |   83842013|xylA                  |                   5|                     5|                 4|
|CarbonCycling      |   84359284|endochitinase         |                   5|                     2|                 4|
|CarbonCycling      |   84516932|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |   84689378|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |   84691156|amyA                  |                   5|                     5|                 4|
|CarbonCycling      |   85104727|amyA                  |                   5|                     5|                 4|
|CarbonCycling      |     854338|pectinase             |                   5|                     2|                 4|
|CarbonCycling      |   85703610|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |   86282167|xylanase              |                   5|                     5|                 4|
|CarbonCycling      |   86357847|pcc                   |                   5|                     2|                 4|
|CarbonCycling      |   86569029|pcc                   |                   5|                     5|                 4|
|CarbonCycling      |   86570282|vanA                  |                   5|                     3|                 4|
|CarbonCycling      |   86748076|rubisco               |                   5|                     5|                 4|
|CarbonCycling      |   86750309|CODH                  |                   5|                     5|                 4|
|CarbonCycling      |   87080641|amyA                  |                   5|                     2|                 4|
|CarbonCycling      |   87135187|vanA                  |                   5|                     2|                 4|
|CarbonCycling      |   87136287|pcc                   |                   5|                     2|                 4|
|CarbonCycling      |   87311919|amyA                  |                   5|                     5|                 4|
|CarbonCycling      |   88703386|pcc                   |                   5|                     2|                 4|
|CarbonCycling      |   88814399|xylA                  |                   5|                     5|                 4|
|CarbonCycling      |   88865992|vanA                  |                   5|                     2|                 4|
|CarbonCycling      |   89044975|ara                   |                   5|                     2|                 4|
|CarbonCycling      |   89067960|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |   89207709|pcc                   |                   5|                     5|                 4|
|CarbonCycling      |   89241998|rubisco               |                   5|                     2|                 4|
|CarbonCycling      |   89347328|pcc                   |                   5|                     2|                 4|
|CarbonCycling      |   89347395|pcc                   |                   5|                     2|                 4|
|CarbonCycling      |   89362129|rubisco               |                   5|                     2|                 4|
|CarbonCycling      |   89902345|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |   90104852|rubisco               |                   5|                     5|                 4|
|CarbonCycling      |   90106706|pcc                   |                   5|                     2|                 4|
|CarbonCycling      |   90198261|pcc                   |                   5|                     5|                 4|
|CarbonCycling      |   90304595|endochitinase         |                   5|                     5|                 4|
|CarbonCycling      |   90425497|AceB                  |                   5|                     3|                 4|
|CarbonCycling      |   90440222|AceA                  |                   5|                     3|                 4|
|CarbonCycling      |   91690340|rubisco               |                   5|                     5|                 4|
|CarbonCycling      |   91795559|vanA                  |                   5|                     5|                 4|
|CarbonCycling      |   91802339|rubisco               |                   5|                     2|                 4|
|CarbonCycling      |   92117653|pcc                   |                   5|                     5|                 4|
|CarbonCycling      |   92441508|amyA                  |                   5|                     2|                 4|
|CarbonCycling      |   92443198|pcc                   |                   5|                     2|                 4|
|CarbonCycling      |   92908605|endochitinase         |                   5|                     3|                 4|
|CarbonCycling      |   94495092|amyA                  |                   5|                     2|                 4|
|CarbonCycling      |   94984596|pcc                   |                   5|                     2|                 4|
|CarbonCycling      |   94984767|amyA                  |                   5|                     2|                 4|
|CarbonCycling      |   98977415|pcc                   |                   5|                     5|                 4|
|CarbonCycling      |  iegAssA14|AssA                  |                   5|                     3|                 4|
|CarbonCycling      |  iegcoxL23|CODH                  |                   5|                     3|                 4|
|CarbonCycling      |  iegcoxL40|CODH                  |                   5|                     3|                 4|
|EnergyProcess      |  119776603|cytochrome            |                   5|                     2|                 4|
|EnergyProcess      |  120600339|cytochrome            |                   5|                     2|                 4|
|EnergyProcess      |  126175709|cytochrome            |                   5|                     2|                 4|
|EnergyProcess      |  127511457|cytochrome            |                   5|                     2|                 4|
|EnergyProcess      |  146190249|P450                  |                   5|                     3|                 4|
|EnergyProcess      |  146277744|cytochrome            |                   5|                     5|                 4|
|EnergyProcess      |  146279248|cytochrome            |                   5|                     5|                 4|
|EnergyProcess      |  146281451|cytochrome            |                   5|                     2|                 4|
|EnergyProcess      |  146282618|cytochrome            |                   5|                     2|                 4|
|EnergyProcess      |  146283057|cytochrome            |                   5|                     2|                 4|
|EnergyProcess      |  148262604|cytochrome            |                   5|                     2|                 4|
|EnergyProcess      |   15074184|P450                  |                   5|                     3|                 4|
|EnergyProcess      |  157963794|cytochrome            |                   5|                     5|                 4|
|EnergyProcess      |  186883006|hydrogenase           |                   5|                     3|                 4|
|EnergyProcess      |  217498707|hydrogenase           |                   5|                     3|                 4|
|EnergyProcess      |  218757779|hydrogenase           |                   5|                     3|                 4|
|EnergyProcess      |  218888277|hydrogenase           |                   5|                     3|                 4|
|EnergyProcess      |  220904110|hydrogenase           |                   5|                     3|                 4|
|EnergyProcess      |   26986924|cytochrome            |                   5|                     5|                 4|
|EnergyProcess      |    3378678|hydrogenase           |                   5|                     2|                 4|
|EnergyProcess      |    3420957|hydrogenase           |                   5|                     2|                 4|
|EnergyProcess      |   39996164|cytochrome            |                   5|                     2|                 4|
|EnergyProcess      |    5020380|hydrogenase           |                   5|                     2|                 4|
|EnergyProcess      |   57233678|Ni_Fe_hydrogenase     |                   5|                     3|                 4|
|EnergyProcess      |   66043509|cytochrome            |                   5|                     2|                 4|
|EnergyProcess      |   77458718|cytochrome            |                   5|                     3|                 4|
|EnergyProcess      |   78193586|hydrogenase           |                   5|                     2|                 4|
|EnergyProcess      |   78221546|cytochrome            |                   5|                     2|                 4|
|EnergyProcess      |   78221556|cytochrome            |                   5|                     5|                 4|
|EnergyProcess      |   78221801|cytochrome            |                   5|                     2|                 4|
|EnergyProcess      |   78355627|cytochrome            |                   5|                     5|                 4|
|EnergyProcess      |   78358251|cytochrome            |                   5|                     5|                 4|
|EnergyProcess      |   86159087|cytochrome            |                   5|                     2|                 4|
|EnergyProcess      |   86159488|cytochrome            |                   5|                     2|                 4|
|MetalResistance    |  103487155|ZntA                  |                   5|                     2|                 4|
|MetalResistance    |  103487991|mer                   |                   5|                     2|                 4|
|MetalResistance    |  105897501|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |  106764027|CopA                  |                   5|                     2|                 4|
|MetalResistance    |  108467099|czcD                  |                   5|                     2|                 4|
|MetalResistance    |  108757552|merP                  |                   5|                     3|                 4|
|MetalResistance    |  108763297|ZntA                  |                   5|                     5|                 4|
|MetalResistance    |  108804635|pbrA                  |                   5|                     5|                 4|
|MetalResistance    |  108805480|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |  109626263|ArsA                  |                   5|                     5|                 4|
|MetalResistance    |  109642772|ChrA                  |                   5|                     2|                 4|
|MetalResistance    |  110285586|ZntA                  |                   5|                     2|                 4|
|MetalResistance    |  110286166|CadA                  |                   5|                     5|                 4|
|MetalResistance    |  110338015|CopA                  |                   5|                     2|                 4|
|MetalResistance    |  110347018|CadA                  |                   5|                     3|                 4|
|MetalResistance    |  110347183|czcA                  |                   5|                     2|                 4|
|MetalResistance    |  110600695|mer                   |                   5|                     5|                 4|
|MetalResistance    |  110623046|arsB                  |                   5|                     2|                 4|
|MetalResistance    |  110623050|arsB                  |                   5|                     2|                 4|
|MetalResistance    |  110827668|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |  111151982|TerD                  |                   5|                     2|                 4|
|MetalResistance    |  111223995|TerC                  |                   5|                     2|                 4|
|MetalResistance    |  111279206|TerC                  |                   5|                     2|                 4|
|MetalResistance    |  111282116|CopA                  |                   5|                     5|                 4|
|MetalResistance    |  111612839|CopA                  |                   5|                     2|                 4|
|MetalResistance    |  112800673|ZitB                  |                   5|                     5|                 4|
|MetalResistance    |  113529747|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |  113888806|czcD                  |                   5|                     2|                 4|
|MetalResistance    |  113890133|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |  113897440|ChrA                  |                   5|                     2|                 4|
|MetalResistance    |  113909532|czcA                  |                   5|                     2|                 4|
|MetalResistance    |  113955459|czcD                  |                   5|                     3|                 4|
|MetalResistance    |  114050077|arsB                  |                   5|                     5|                 4|
|MetalResistance    |  114307129|CadA                  |                   5|                     2|                 4|
|MetalResistance    |  114340168|ZntA                  |                   5|                     2|                 4|
|MetalResistance    |  114537780|mer                   |                   5|                     5|                 4|
|MetalResistance    |  114548443|TehB                  |                   5|                     2|                 4|
|MetalResistance    |  114548970|mer                   |                   5|                     5|                 4|
|MetalResistance    |  114707623|TerC                  |                   5|                     2|                 4|
|MetalResistance    |  114767551|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |  115258647|CadA                  |                   5|                     2|                 4|
|MetalResistance    |  115352641|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |  115366286|TerC                  |                   5|                     5|                 4|
|MetalResistance    |  115367244|CopA                  |                   5|                     2|                 4|
|MetalResistance    |  115375434|czcD                  |                   5|                     5|                 4|
|MetalResistance    |  115422656|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |  115469636|CadA                  |                   5|                     2|                 4|
|MetalResistance    |  115515989|CopA                  |                   5|                     5|                 4|
|MetalResistance    |  115524425|czcD                  |                   5|                     5|                 4|
|MetalResistance    |  116049493|ZntA                  |                   5|                     3|                 4|
|MetalResistance    |  116050225|arsB                  |                   5|                     2|                 4|
|MetalResistance    |  116073929|ZntA                  |                   5|                     2|                 4|
|MetalResistance    |  116099637|czcD                  |                   5|                     2|                 4|
|MetalResistance    |  116122570|TerC                  |                   5|                     2|                 4|
|MetalResistance    |  116268722|CopA                  |                   5|                     5|                 4|
|MetalResistance    |  116612750|CopA                  |                   5|                     5|                 4|
|MetalResistance    |  116612878|CadA                  |                   5|                     2|                 4|
|MetalResistance    |  116622615|ZntA                  |                   5|                     5|                 4|
|MetalResistance    |  116696037|TerC                  |                   5|                     2|                 4|
|MetalResistance    |  116698740|czcA                  |                   5|                     2|                 4|
|MetalResistance    |  116747543|ChrA                  |                   5|                     2|                 4|
|MetalResistance    |  116751190|CadA                  |                   5|                     2|                 4|
|MetalResistance    |   11691635|ArsC                  |                   5|                     5|                 4|
|MetalResistance    |  117610431|czcD                  |                   5|                     5|                 4|
|MetalResistance    |  117925649|CopA                  |                   5|                     5|                 4|
|MetalResistance    |  117926675|czcD                  |                   5|                     3|                 4|
|MetalResistance    |  117980996|ChrA                  |                   5|                     2|                 4|
|MetalResistance    |  117994352|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |  117998472|TehB                  |                   5|                     5|                 4|
|MetalResistance    |  117999268|czcA                  |                   5|                     5|                 4|
|MetalResistance    |  118002628|CopA                  |                   5|                     2|                 4|
|MetalResistance    |  118029236|czcD                  |                   5|                     2|                 4|
|MetalResistance    |  118029783|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |  118048714|pbrA                  |                   5|                     5|                 4|
|MetalResistance    |  118049483|czcA                  |                   5|                     3|                 4|
|MetalResistance    |  118051341|ChrA                  |                   5|                     2|                 4|
|MetalResistance    |  118051342|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |  118467822|arsB                  |                   5|                     2|                 4|
|MetalResistance    |  118467908|CadA                  |                   5|                     5|                 4|
|MetalResistance    |  118501979|CadA                  |                   5|                     2|                 4|
|MetalResistance    |  118504015|CopA                  |                   5|                     5|                 4|
|MetalResistance    |  118589390|CopA                  |                   5|                     5|                 4|
|MetalResistance    |  118658467|silC                  |                   5|                     5|                 4|
|MetalResistance    |  118666005|ChrA                  |                   5|                     2|                 4|
|MetalResistance    |  118668503|silC                  |                   5|                     2|                 4|
|MetalResistance    |  118669130|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |  118681426|czcA                  |                   5|                     2|                 4|
|MetalResistance    |  118696078|CadA                  |                   5|                     2|                 4|
|MetalResistance    |  118729447|czcA                  |                   5|                     5|                 4|
|MetalResistance    |  118730373|ArsC                  |                   5|                     5|                 4|
|MetalResistance    |  118731626|CadA                  |                   5|                     2|                 4|
|MetalResistance    |  118746881|ChrA                  |                   5|                     3|                 4|
|MetalResistance    |  119374842|CadA                  |                   5|                     5|                 4|
|MetalResistance    |  119376611|CadA                  |                   5|                     2|                 4|
|MetalResistance    |  119386067|czcD                  |                   5|                     5|                 4|
|MetalResistance    |  119456450|mer                   |                   5|                     5|                 4|
|MetalResistance    |  119483160|czcD                  |                   5|                     5|                 4|
|MetalResistance    |  119513368|ChrA                  |                   5|                     2|                 4|
|MetalResistance    |  119535241|CopA                  |                   5|                     5|                 4|
|MetalResistance    |  119537106|czcD                  |                   5|                     5|                 4|
|MetalResistance    |  119669551|czcA                  |                   5|                     5|                 4|
|MetalResistance    |  119671055|ArsC                  |                   5|                     5|                 4|
|MetalResistance    |  119714242|merB                  |                   5|                     5|                 4|
|MetalResistance    |  119715618|ChrA                  |                   5|                     2|                 4|
|MetalResistance    |  119818411|silC                  |                   5|                     2|                 4|
|MetalResistance    |  119872935|CopA                  |                   5|                     5|                 4|
|MetalResistance    |  119897709|czcD                  |                   5|                     5|                 4|
|MetalResistance    |  119899246|CopA                  |                   5|                     5|                 4|
|MetalResistance    |  120588670|CopA                  |                   5|                     5|                 4|
|MetalResistance    |  120594177|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |  120595854|CopA                  |                   5|                     2|                 4|
|MetalResistance    |  120597953|CadA                  |                   5|                     5|                 4|
|MetalResistance    |  120605814|CadA                  |                   5|                     2|                 4|
|MetalResistance    |  120606320|CadA                  |                   5|                     5|                 4|
|MetalResistance    |  120606683|ChrA                  |                   5|                     2|                 4|
|MetalResistance    |  121529687|ChrA                  |                   5|                     2|                 4|
|MetalResistance    |  121555348|ChrA                  |                   5|                     2|                 4|
|MetalResistance    |  121555349|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |  121594391|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |  121594932|CadA                  |                   5|                     5|                 4|
|MetalResistance    |  121605481|ArsC                  |                   5|                     5|                 4|
|MetalResistance    |  121605776|silC                  |                   5|                     5|                 4|
|MetalResistance    |  121606885|CadA                  |                   5|                     2|                 4|
|MetalResistance    |  123440611|CadA                  |                   5|                     5|                 4|
|MetalResistance    |  123443563|ArsC                  |                   5|                     5|                 4|
|MetalResistance    |  124363426|CadA                  |                   5|                     2|                 4|
|MetalResistance    |  124483586|czcD                  |                   5|                     2|                 4|
|MetalResistance    |  124521174|CadA                  |                   5|                     2|                 4|
|MetalResistance    |  124894210|ChrA                  |                   5|                     2|                 4|
|MetalResistance    |  126179296|CadA                  |                   5|                     2|                 4|
|MetalResistance    |  126355178|czcA                  |                   5|                     2|                 4|
|MetalResistance    |  126358823|czcC                  |                   5|                     5|                 4|
|MetalResistance    |  126627124|TerD                  |                   5|                     5|                 4|
|MetalResistance    |  126628241|CopA                  |                   5|                     5|                 4|
|MetalResistance    |  126629723|czcD                  |                   5|                     2|                 4|
|MetalResistance    |  126642955|czcA                  |                   5|                     2|                 4|
|MetalResistance    |  126667582|ArsC                  |                   5|                     2|                 4|
|MetalResistance    |  126707025|ChrA                  |                   5|                     2|                 4|
|MetalResistance    |  126709599|CadA                  |                   5|                     5|                 4|
|MetalResistance    |  126737435|nreB                  |                   5|                     2|                 4|
|MetalResistance    |   13092864|ZntA                  |                   5|                     3|                 4|
|MetalResistance    |   13093629|ZntA                  |                   5|                     2|                 4|
|MetalResistance    |  133740352|CopA                  |                   5|                     3|                 4|
|MetalResistance    |  134052085|CopA                  |                   5|                     2|                 4|
|MetalResistance    |  134100191|TerC                  |                   5|                     5|                 4|
|MetalResistance    |  134101899|TerC                  |                   5|                     2|                 4|
|MetalResistance    |  134103041|CopA                  |                   5|                     2|                 4|
|MetalResistance    |  134132818|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |   13425442|TerC                  |                   5|                     5|                 4|
|MetalResistance    |  134266357|Al                    |                   5|                     2|                 4|
|MetalResistance    |  134288165|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |  134291679|CadA                  |                   5|                     3|                 4|
|MetalResistance    |  134291701|silC                  |                   5|                     2|                 4|
|MetalResistance    |  134300814|CadA                  |                   5|                     5|                 4|
|MetalResistance    |   13472245|CopA                  |                   5|                     5|                 4|
|MetalResistance    |  139478541|mer                   |                   5|                     2|                 4|
|MetalResistance    |  139478551|mer                   |                   5|                     2|                 4|
|MetalResistance    |  139478563|mer                   |                   5|                     5|                 4|
|MetalResistance    |  144897350|CopA                  |                   5|                     5|                 4|
|MetalResistance    |  144899226|czcD                  |                   5|                     5|                 4|
|MetalResistance    |  145221509|CopA                  |                   5|                     5|                 4|
|MetalResistance    |  145221997|TerC                  |                   5|                     2|                 4|
|MetalResistance    |  145228503|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |  145555417|CopA                  |                   5|                     5|                 4|
|MetalResistance    |  145575474|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |  145590808|CopA                  |                   5|                     5|                 4|
|MetalResistance    |  145617404|czcD                  |                   5|                     2|                 4|
|MetalResistance    |  146190810|SilA                  |                   5|                     2|                 4|
|MetalResistance    |  146232455|silC                  |                   5|                     2|                 4|
|MetalResistance    |  146293063|CadA                  |                   5|                     5|                 4|
|MetalResistance    |  146313505|CopA                  |                   5|                     5|                 4|
|MetalResistance    |  146313536|TerC                  |                   5|                     5|                 4|
|MetalResistance    |  146407056|czcA                  |                   5|                     2|                 4|
|MetalResistance    |  146409525|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |  146410634|czcA                  |                   5|                     2|                 4|
|MetalResistance    |  146410775|merP                  |                   5|                     2|                 4|
|MetalResistance    |  147851801|Al                    |                   5|                     2|                 4|
|MetalResistance    |  147920132|CopA                  |                   5|                     2|                 4|
|MetalResistance    |  148254829|SilA                  |                   5|                     2|                 4|
|MetalResistance    |  148499903|CopA                  |                   5|                     2|                 4|
|MetalResistance    |  148500643|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |  148530047|silC                  |                   5|                     2|                 4|
|MetalResistance    |  148568682|CopA                  |                   5|                     5|                 4|
|MetalResistance    |  148570981|TerD                  |                   5|                     5|                 4|
|MetalResistance    |  148657873|CopA                  |                   5|                     3|                 4|
|MetalResistance    |  148829707|czcD                  |                   5|                     2|                 4|
|MetalResistance    |  148838027|czcD                  |                   5|                     2|                 4|
|MetalResistance    |  149361059|mer                   |                   5|                     5|                 4|
|MetalResistance    |  149810275|ArsC                  |                   5|                     2|                 4|
|MetalResistance    |  149811120|mer                   |                   5|                     2|                 4|
|MetalResistance    |  149822860|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |  149915408|CopA                  |                   5|                     2|                 4|
|MetalResistance    |  149951551|CadA                  |                   5|                     5|                 4|
|MetalResistance    |  150386993|CadA                  |                   5|                     2|                 4|
|MetalResistance    |  150387119|CopA                  |                   5|                     5|                 4|
|MetalResistance    |  150955842|TehB                  |                   5|                     5|                 4|
|MetalResistance    |  151281191|SilA                  |                   5|                     2|                 4|
|MetalResistance    |  151563940|ChrA                  |                   5|                     2|                 4|
|MetalResistance    |  151580204|czcA                  |                   5|                     5|                 4|
|MetalResistance    |  152030504|czcA                  |                   5|                     2|                 4|
|MetalResistance    |  152975463|CorC                  |                   5|                     5|                 4|
|MetalResistance    |  153004092|CopA                  |                   5|                     5|                 4|
|MetalResistance    |  153796047|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |  153797773|CopA                  |                   5|                     5|                 4|
|MetalResistance    |  153804427|CorC                  |                   5|                     5|                 4|
|MetalResistance    |  153891498|CadA                  |                   5|                     2|                 4|
|MetalResistance    |  153894158|CopA                  |                   5|                     2|                 4|
|MetalResistance    |  154161105|CopA                  |                   5|                     2|                 4|
|MetalResistance    |  154161419|czcA                  |                   5|                     3|                 4|
|MetalResistance    |  154161986|CadA                  |                   5|                     5|                 4|
|MetalResistance    |  154162182|ChrA                  |                   5|                     2|                 4|
|MetalResistance    |  154162903|CopA                  |                   5|                     3|                 4|
|MetalResistance    |  154250811|czcA                  |                   5|                     2|                 4|
|MetalResistance    |  154253120|ChrA                  |                   5|                     3|                 4|
|MetalResistance    |  156190103|CopA                  |                   5|                     5|                 4|
|MetalResistance    |  156231487|CadA                  |                   5|                     2|                 4|
|MetalResistance    |  156450777|czcA                  |                   5|                     2|                 4|
|MetalResistance    |  156864227|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |  157364688|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |  157370568|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |  157406168|czcA                  |                   5|                     5|                 4|
|MetalResistance    |   15791027|ZntA                  |                   5|                     5|                 4|
|MetalResistance    |  158113526|TerD                  |                   5|                     5|                 4|
|MetalResistance    |  158424656|ChrA                  |                   5|                     2|                 4|
|MetalResistance    |   16124558|czcD                  |                   5|                     5|                 4|
|MetalResistance    |   16125059|silC                  |                   5|                     2|                 4|
|MetalResistance    |   16412063|czcD                  |                   5|                     5|                 4|
|MetalResistance    |   17430780|silC                  |                   5|                     5|                 4|
|MetalResistance    |   17739598|CopA                  |                   5|                     2|                 4|
|MetalResistance    |   18076026|merP                  |                   5|                     5|                 4|
|MetalResistance    |   18076027|metC                  |                   5|                     5|                 4|
|MetalResistance    |   18076028|merP                  |                   5|                     2|                 4|
|MetalResistance    |   18313568|ZntA                  |                   5|                     2|                 4|
|MetalResistance    |    2052182|mer                   |                   5|                     5|                 4|
|MetalResistance    |   21110586|czcA                  |                   5|                     2|                 4|
|MetalResistance    |   21115235|czcD                  |                   5|                     5|                 4|
|MetalResistance    |   22295747|Al                    |                   5|                     2|                 4|
|MetalResistance    |   23465870|czcD                  |                   5|                     2|                 4|
|MetalResistance    |   23494189|ChrA                  |                   5|                     2|                 4|
|MetalResistance    |   26992063|SilA                  |                   5|                     5|                 4|
|MetalResistance    |   27348949|ZntA                  |                   5|                     2|                 4|
|MetalResistance    |   27381627|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |   27526400|silC                  |                   5|                     2|                 4|
|MetalResistance    |   28627880|CadA                  |                   5|                     5|                 4|
|MetalResistance    |   28850615|pbrT                  |                   5|                     2|                 4|
|MetalResistance    |   28855646|CopA                  |                   5|                     5|                 4|
|MetalResistance    |   29607602|TerZ                  |                   5|                     2|                 4|
|MetalResistance    |   29610016|TerZ                  |                   5|                     2|                 4|
|MetalResistance    |   33563759|ZntA                  |                   5|                     3|                 4|
|MetalResistance    |    3378201|TehB                  |                   5|                     2|                 4|
|MetalResistance    |   34102575|CopA                  |                   5|                     5|                 4|
|MetalResistance    |   35214191|czcA                  |                   5|                     2|                 4|
|MetalResistance    |   35214621|ZntA                  |                   5|                     5|                 4|
|MetalResistance    |   37200382|TehB                  |                   5|                     2|                 4|
|MetalResistance    |   37963677|ZntA                  |                   5|                     5|                 4|
|MetalResistance    |   39935125|czcA                  |                   5|                     5|                 4|
|MetalResistance    |   39936617|ArsC                  |                   5|                     2|                 4|
|MetalResistance    |   39997233|czcA                  |                   5|                     5|                 4|
|MetalResistance    |   41395795|TehB                  |                   5|                     5|                 4|
|MetalResistance    |   41398796|czcD                  |                   5|                     5|                 4|
|MetalResistance    |   44662914|TerZ                  |                   5|                     2|                 4|
|MetalResistance    |    4468699|TerC                  |                   5|                     2|                 4|
|MetalResistance    |   48927357|silC                  |                   5|                     5|                 4|
|MetalResistance    |    4981052|czcD                  |                   5|                     5|                 4|
|MetalResistance    |   50085095|TerZ                  |                   5|                     2|                 4|
|MetalResistance    |   50842178|CopA                  |                   5|                     3|                 4|
|MetalResistance    |   51451372|arsB                  |                   5|                     5|                 4|
|MetalResistance    |   52629459|czcA                  |                   5|                     2|                 4|
|MetalResistance    |   52784176|TerZ                  |                   5|                     2|                 4|
|MetalResistance    |   54018931|TehB                  |                   5|                     2|                 4|
|MetalResistance    |   54022124|ZntA                  |                   5|                     5|                 4|
|MetalResistance    |   54027542|TerD                  |                   5|                     5|                 4|
|MetalResistance    |   54027692|czcD                  |                   5|                     2|                 4|
|MetalResistance    |   54027836|ArsC                  |                   5|                     2|                 4|
|MetalResistance    |    5458357|CopA                  |                   5|                     2|                 4|
|MetalResistance    |   55229277|CopA                  |                   5|                     2|                 4|
|MetalResistance    |   56311794|ZntA                  |                   5|                     5|                 4|
|MetalResistance    |   56419942|czcD                  |                   5|                     2|                 4|
|MetalResistance    |   56478348|CopA                  |                   5|                     2|                 4|
|MetalResistance    |   56911395|czcD                  |                   5|                     2|                 4|
|MetalResistance    |   57016825|ZntA                  |                   5|                     5|                 4|
|MetalResistance    |   57224952|mer                   |                   5|                     2|                 4|
|MetalResistance    |   57235066|TerC                  |                   5|                     5|                 4|
|MetalResistance    |   58198973|ZntA                  |                   5|                     3|                 4|
|MetalResistance    |   62896434|merT                  |                   5|                     2|                 4|
|MetalResistance    |   62896435|merP                  |                   5|                     2|                 4|
|MetalResistance    |    6460232|ChrA                  |                   5|                     2|                 4|
|MetalResistance    |    6491822|mer                   |                   5|                     3|                 4|
|MetalResistance    |    6689151|CopA                  |                   5|                     2|                 4|
|MetalResistance    |    6689526|merT                  |                   5|                     5|                 4|
|MetalResistance    |   67087778|silC                  |                   5|                     2|                 4|
|MetalResistance    |   67532411|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |   67538514|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |   67855460|CopA                  |                   5|                     2|                 4|
|MetalResistance    |   67916589|CopA                  |                   5|                     2|                 4|
|MetalResistance    |   67939261|CopA                  |                   5|                     5|                 4|
|MetalResistance    |   68199429|CopA                  |                   5|                     3|                 4|
|MetalResistance    |   68203061|CopA                  |                   5|                     5|                 4|
|MetalResistance    |   68230223|TerC                  |                   5|                     2|                 4|
|MetalResistance    |   68344813|ChrA                  |                   5|                     2|                 4|
|MetalResistance    |   68518471|czcD                  |                   5|                     5|                 4|
|MetalResistance    |   69259015|CopA                  |                   5|                     5|                 4|
|MetalResistance    |   69279982|ArsC                  |                   5|                     2|                 4|
|MetalResistance    |   69937812|ArsC                  |                   5|                     2|                 4|
|MetalResistance    |   71279090|czcA                  |                   5|                     2|                 4|
|MetalResistance    |   71731088|czcD                  |                   5|                     2|                 4|
|MetalResistance    |   71740575|CopA                  |                   5|                     2|                 4|
|MetalResistance    |   71838500|ZntA                  |                   5|                     3|                 4|
|MetalResistance    |   71839530|czcA                  |                   5|                     5|                 4|
|MetalResistance    |   71847125|czcA                  |                   5|                     2|                 4|
|MetalResistance    |   71916431|czcD                  |                   5|                     2|                 4|
|MetalResistance    |    7210994|TerZ                  |                   5|                     2|                 4|
|MetalResistance    |   73662912|CadA                  |                   5|                     3|                 4|
|MetalResistance    |   74056840|czcA                  |                   5|                     2|                 4|
|MetalResistance    |   74058169|ChrA                  |                   5|                     2|                 4|
|MetalResistance    |   74422182|CadA                  |                   5|                     5|                 4|
|MetalResistance    |   75676104|ArsC                  |                   5|                     5|                 4|
|MetalResistance    |   76165912|ChrA                  |                   5|                     2|                 4|
|MetalResistance    |   77019684|czcD                  |                   5|                     2|                 4|
|MetalResistance    |   77019723|ArsC                  |                   5|                     2|                 4|
|MetalResistance    |   77165617|TerC                  |                   5|                     2|                 4|
|MetalResistance    |   77362237|silC                  |                   5|                     5|                 4|
|MetalResistance    |   77382984|silC                  |                   5|                     5|                 4|
|MetalResistance    |   77386373|ZitB                  |                   5|                     2|                 4|
|MetalResistance    |   77458043|CopA                  |                   5|                     2|                 4|
|MetalResistance    |    7769067|arsB                  |                   5|                     2|                 4|
|MetalResistance    |   77808421|czcA                  |                   5|                     2|                 4|
|MetalResistance    |   77814270|ArsC                  |                   5|                     2|                 4|
|MetalResistance    |   77815140|czcD                  |                   5|                     2|                 4|
|MetalResistance    |   77956875|ZitB                  |                   5|                     5|                 4|
|MetalResistance    |   77957884|CadA                  |                   5|                     2|                 4|
|MetalResistance    |   77958371|CopA                  |                   5|                     3|                 4|
|MetalResistance    |   77978516|CadA                  |                   5|                     5|                 4|
|MetalResistance    |   77996972|CopA                  |                   5|                     5|                 4|
|MetalResistance    |   78198348|ZntA                  |                   5|                     3|                 4|
|MetalResistance    |   78356360|ZntA                  |                   5|                     2|                 4|
|MetalResistance    |   78498745|czcA                  |                   5|                     5|                 4|
|MetalResistance    |   78515664|ChrA                  |                   5|                     2|                 4|
|MetalResistance    |   78609680|ZntA                  |                   5|                     3|                 4|
|MetalResistance    |   78695344|czcD                  |                   5|                     5|                 4|
|MetalResistance    |   78695669|czcA                  |                   5|                     2|                 4|
|MetalResistance    |   78697507|CopA                  |                   5|                     5|                 4|
|MetalResistance    |   83311377|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |   83368632|mer                   |                   5|                     5|                 4|
|MetalResistance    |   83592229|TerZ                  |                   5|                     2|                 4|
|MetalResistance    |   83746333|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |   83749132|ZntA                  |                   5|                     2|                 4|
|MetalResistance    |   83816696|ZntA                  |                   5|                     2|                 4|
|MetalResistance    |   83846568|ChrA                  |                   5|                     2|                 4|
|MetalResistance    |   83952813|CopA                  |                   5|                     3|                 4|
|MetalResistance    |   84382815|czcD                  |                   5|                     5|                 4|
|MetalResistance    |   84382828|czcD                  |                   5|                     2|                 4|
|MetalResistance    |   84386611|CadA                  |                   5|                     5|                 4|
|MetalResistance    |   84389266|CopA                  |                   5|                     2|                 4|
|MetalResistance    |   84499300|CopA                  |                   5|                     2|                 4|
|MetalResistance    |   84691528|ArsC                  |                   5|                     2|                 4|
|MetalResistance    |   84695870|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |   84704841|czcA                  |                   5|                     2|                 4|
|MetalResistance    |   84786792|czcD                  |                   5|                     2|                 4|
|MetalResistance    |   84786795|CadA                  |                   5|                     3|                 4|
|MetalResistance    |   85058228|ArsC                  |                   5|                     5|                 4|
|MetalResistance    |   85058869|ZitB                  |                   5|                     2|                 4|
|MetalResistance    |   85059831|ZntA                  |                   5|                     2|                 4|
|MetalResistance    |   85086375|ZntA                  |                   5|                     5|                 4|
|MetalResistance    |   85117384|ZntA                  |                   5|                     2|                 4|
|MetalResistance    |   85707406|CopA                  |                   5|                     2|                 4|
|MetalResistance    |   85713827|czcA                  |                   5|                     5|                 4|
|MetalResistance    |   85773958|czcA                  |                   5|                     2|                 4|
|MetalResistance    |   85823564|ZntA                  |                   5|                     2|                 4|
|MetalResistance    |   85858341|ZntA                  |                   5|                     5|                 4|
|MetalResistance    |   85860285|CorC                  |                   5|                     5|                 4|
|MetalResistance    |   86139699|czcD                  |                   5|                     5|                 4|
|MetalResistance    |   86165092|ZntA                  |                   5|                     5|                 4|
|MetalResistance    |   86165537|czcD                  |                   5|                     2|                 4|
|MetalResistance    |   86166949|ZntA                  |                   5|                     5|                 4|
|MetalResistance    |   86359309|ZntA                  |                   5|                     5|                 4|
|MetalResistance    |   86558602|pbrT                  |                   5|                     2|                 4|
|MetalResistance    |   86568613|TerZ                  |                   5|                     2|                 4|
|MetalResistance    |   86570482|czcD                  |                   5|                     3|                 4|
|MetalResistance    |   87302472|ZntA                  |                   5|                     5|                 4|
|MetalResistance    |   88191351|arsB                  |                   5|                     2|                 4|
|MetalResistance    |   88705723|czcD                  |                   5|                     2|                 4|
|MetalResistance    |   88791015|czcD                  |                   5|                     5|                 4|
|MetalResistance    |   88813119|CopA                  |                   5|                     3|                 4|
|MetalResistance    |   88817073|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |   88919055|CopA                  |                   5|                     2|                 4|
|MetalResistance    |   88925625|Al                    |                   5|                     2|                 4|
|MetalResistance    |   89056342|CopA                  |                   5|                     5|                 4|
|MetalResistance    |   89080255|TehB                  |                   5|                     2|                 4|
|MetalResistance    |   89082466|CopA                  |                   5|                     5|                 4|
|MetalResistance    |   89093170|ZntA                  |                   5|                     5|                 4|
|MetalResistance    |   89336704|ZntA                  |                   5|                     5|                 4|
|MetalResistance    |   89338509|TerC                  |                   5|                     3|                 4|
|MetalResistance    |   89896222|ZntA                  |                   5|                     2|                 4|
|MetalResistance    |   89897111|TehB                  |                   5|                     2|                 4|
|MetalResistance    |   89899662|ZntA                  |                   5|                     2|                 4|
|MetalResistance    |   90206686|TerC                  |                   5|                     2|                 4|
|MetalResistance    |   90305607|ZntA                  |                   5|                     2|                 4|
|MetalResistance    |   90413284|CadA                  |                   5|                     2|                 4|
|MetalResistance    |   90418216|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |   91201982|czcC                  |                   5|                     2|                 4|
|MetalResistance    |   91202321|czcC                  |                   5|                     2|                 4|
|MetalResistance    |   91692048|czcA                  |                   5|                     5|                 4|
|MetalResistance    |   91708977|CopA                  |                   5|                     2|                 4|
|MetalResistance    |   91710145|czcD                  |                   5|                     3|                 4|
|MetalResistance    |   91790145|ZntA                  |                   5|                     2|                 4|
|MetalResistance    |   91790694|CopA                  |                   5|                     2|                 4|
|MetalResistance    |   91793002|ArsC                  |                   5|                     2|                 4|
|MetalResistance    |   91978111|czcA                  |                   5|                     5|                 4|
|MetalResistance    |   92109658|CadA                  |                   5|                     3|                 4|
|MetalResistance    |   93006469|ZntA                  |                   5|                     2|                 4|
|MetalResistance    |   94264244|CopA                  |                   5|                     5|                 4|
|MetalResistance    |   94264964|CopA                  |                   5|                     5|                 4|
|MetalResistance    |   94310675|silC                  |                   5|                     5|                 4|
|MetalResistance    |   94422217|TehB                  |                   5|                     2|                 4|
|MetalResistance    |   94554100|pbrA                  |                   5|                     2|                 4|
|MetalResistance    |   94554270|pbrA                  |                   5|                     5|                 4|
|MetalResistance    |   94554604|czcD                  |                   5|                     2|                 4|
|MetalResistance    |   94968349|ChrA                  |                   5|                     5|                 4|
|MetalResistance    |   94972016|ArsC                  |                   5|                     2|                 4|
|MetalResistance    |   94972219|CadA                  |                   5|                     2|                 4|
|MetalResistance    |   94984197|CopA                  |                   5|                     2|                 4|
|MetalResistance    |   95111342|ArsC                  |                   5|                     2|                 4|
|MethaneProduction  |  124363917|mcrA                  |                   5|                     5|                 4|
|MethaneProduction  |   48527128|mcrA                  |                   5|                     2|                 4|
|Nitrogen           |   10863129|nifH                  |                   5|                     5|                 4|
|Nitrogen           |  109454988|nirS                  |                   5|                     5|                 4|
|Nitrogen           |  109632807|nirK                  |                   5|                     5|                 4|
|Nitrogen           |  109645564|nrfA                  |                   5|                     5|                 4|
|Nitrogen           |  109660030|ureC                  |                   5|                     2|                 4|
|Nitrogen           |  110551987|nifH                  |                   5|                     5|                 4|
|Nitrogen           |  110630458|nifH                  |                   5|                     2|                 4|
|Nitrogen           |  110630580|nifH                  |                   5|                     2|                 4|
|Nitrogen           |  110630622|nifH                  |                   5|                     5|                 4|
|Nitrogen           |  110632212|nirK                  |                   5|                     2|                 4|
|Nitrogen           |  110668150|nirA                  |                   5|                     3|                 4|
|Nitrogen           |  111919316|nosZ                  |                   5|                     2|                 4|
|Nitrogen           |  112462821|nirK                  |                   5|                     2|                 4|
|Nitrogen           |  112463043|nirK                  |                   5|                     2|                 4|
|Nitrogen           |  112463327|nirK                  |                   5|                     5|                 4|
|Nitrogen           |  112463779|nirK                  |                   5|                     2|                 4|
|Nitrogen           |  113941017|ureC                  |                   5|                     5|                 4|
|Nitrogen           |  114763950|ureC                  |                   5|                     2|                 4|
|Nitrogen           |  116048312|nifH                  |                   5|                     2|                 4|
|Nitrogen           |  116608920|ureC                  |                   5|                     5|                 4|
|Nitrogen           |  116633918|narG                  |                   5|                     2|                 4|
|Nitrogen           |  116634745|narG                  |                   5|                     2|                 4|
|Nitrogen           |  116698891|nrfA                  |                   5|                     5|                 4|
|Nitrogen           |  118618420|ureC                  |                   5|                     2|                 4|
|Nitrogen           |  118717115|nifH                  |                   5|                     2|                 4|
|Nitrogen           |  119371710|narG                  |                   5|                     2|                 4|
|Nitrogen           |  119671792|norB                  |                   5|                     3|                 4|
|Nitrogen           |  119961669|NirB                  |                   5|                     3|                 4|
|Nitrogen           |  120592852|ureC                  |                   5|                     5|                 4|
|Nitrogen           |  121495568|narG                  |                   5|                     2|                 4|
|Nitrogen           |  124488109|napA                  |                   5|                     2|                 4|
|Nitrogen           |  124488145|napA                  |                   5|                     2|                 4|
|Nitrogen           |  124488337|narG                  |                   5|                     5|                 4|
|Nitrogen           |    1255498|nifH                  |                   5|                     5|                 4|
|Nitrogen           |    1255504|nifH                  |                   5|                     5|                 4|
|Nitrogen           |   12744271|nosZ                  |                   5|                     5|                 4|
|Nitrogen           |  127511439|nrfA                  |                   5|                     5|                 4|
|Nitrogen           |  134097246|ureC                  |                   5|                     3|                 4|
|Nitrogen           |  139003172|nifH                  |                   5|                     5|                 4|
|Nitrogen           |  139003420|nifH                  |                   5|                     5|                 4|
|Nitrogen           |  139003429|nifH                  |                   5|                     2|                 4|
|Nitrogen           |  140713899|nasA                  |                   5|                     5|                 4|
|Nitrogen           |  144901049|ureC                  |                   5|                     2|                 4|
|Nitrogen           |  144901070|napA                  |                   5|                     2|                 4|
|Nitrogen           |  144944019|nifH                  |                   5|                     5|                 4|
|Nitrogen           |  145216424|ureC                  |                   5|                     3|                 4|
|Nitrogen           |  145319978|ureC                  |                   5|                     5|                 4|
|Nitrogen           |  146341632|nifH                  |                   5|                     5|                 4|
|Nitrogen           |  147832086|nifH                  |                   5|                     3|                 4|
|Nitrogen           |  148543345|nifH                  |                   5|                     2|                 4|
|Nitrogen           |  148568718|nifH                  |                   5|                     2|                 4|
|Nitrogen           |  149200581|nrfA                  |                   5|                     5|                 4|
|Nitrogen           |  149783097|nirK                  |                   5|                     5|                 4|
|Nitrogen           |  149783143|nirK                  |                   5|                     5|                 4|
|Nitrogen           |  150014349|nifH                  |                   5|                     5|                 4|
|Nitrogen           |  150029098|ureC                  |                   5|                     2|                 4|
|Nitrogen           |  150399896|nifH                  |                   5|                     2|                 4|
|Nitrogen           |  150955610|narG                  |                   5|                     2|                 4|
|Nitrogen           |  151280243|ureC                  |                   5|                     5|                 4|
|Nitrogen           |  151361599|nifH                  |                   5|                     2|                 4|
|Nitrogen           |  152964803|ureC                  |                   5|                     3|                 4|
|Nitrogen           |  153009769|ureC                  |                   5|                     2|                 4|
|Nitrogen           |  154151622|nifH                  |                   5|                     5|                 4|
|Nitrogen           |  154687780|ureC                  |                   5|                     2|                 4|
|Nitrogen           |  156191723|ureC                  |                   5|                     2|                 4|
|Nitrogen           |  157285383|nosZ                  |                   5|                     5|                 4|
|Nitrogen           |  157401497|nifH                  |                   5|                     5|                 4|
|Nitrogen           |  158141583|nrfA                  |                   5|                     2|                 4|
|Nitrogen           |  158522608|nifH                  |                   5|                     2|                 4|
|Nitrogen           |  161075759|NirB                  |                   5|                     3|                 4|
|Nitrogen           |   16506204|nasA                  |                   5|                     2|                 4|
|Nitrogen           |  166364582|nirA                  |                   5|                     3|                 4|
|Nitrogen           |   17129953|nirA                  |                   5|                     3|                 4|
|Nitrogen           |  172036138|NiR                   |                   5|                     3|                 4|
|Nitrogen           |   17228103|NiR                   |                   5|                     3|                 4|
|Nitrogen           |   17742853|nirK                  |                   5|                     5|                 4|
|Nitrogen           |  182413555|nirA                  |                   5|                     3|                 4|
|Nitrogen           |   19888076|nifH                  |                   5|                     2|                 4|
|Nitrogen           |  206576326|NirB                  |                   5|                     3|                 4|
|Nitrogen           |  209874150|NirB                  |                   5|                     3|                 4|
|Nitrogen           |  213925440|NirB                  |                   5|                     3|                 4|
|Nitrogen           |   22293726|ureC                  |                   5|                     2|                 4|
|Nitrogen           |   22651592|napA                  |                   5|                     2|                 4|
|Nitrogen           |  227069639|NiR                   |                   5|                     3|                 4|
|Nitrogen           |  227069655|NiR                   |                   5|                     3|                 4|
|Nitrogen           |  227069699|NiR                   |                   5|                     3|                 4|
|Nitrogen           |   26278728|narG                  |                   5|                     2|                 4|
|Nitrogen           |   26278770|narG                  |                   5|                     5|                 4|
|Nitrogen           |   26278782|narG                  |                   5|                     2|                 4|
|Nitrogen           |   26278818|narG                  |                   5|                     2|                 4|
|Nitrogen           |   26278878|narG                  |                   5|                     2|                 4|
|Nitrogen           |   26278888|narG                  |                   5|                     5|                 4|
|Nitrogen           |   29125948|nosZ                  |                   5|                     2|                 4|
|Nitrogen           |   29465996|norB                  |                   5|                     2|                 4|
|Nitrogen           |   29466006|norB                  |                   5|                     5|                 4|
|Nitrogen           |   29466024|norB                  |                   5|                     2|                 4|
|Nitrogen           |   29466066|norB                  |                   5|                     5|                 4|
|Nitrogen           |   29466088|norB                  |                   5|                     2|                 4|
|Nitrogen           |   29609322|NirB                  |                   5|                     3|                 4|
|Nitrogen           |   29652452|narG                  |                   5|                     2|                 4|
|Nitrogen           |   29652454|narG                  |                   5|                     2|                 4|
|Nitrogen           |   29652532|narG                  |                   5|                     2|                 4|
|Nitrogen           |   29652552|narG                  |                   5|                     2|                 4|
|Nitrogen           |   29652656|narG                  |                   5|                     3|                 4|
|Nitrogen           |   29832204|NirB                  |                   5|                     3|                 4|
|Nitrogen           |     312237|nifH                  |                   5|                     5|                 4|
|Nitrogen           |    3157544|nifH                  |                   5|                     2|                 4|
|Nitrogen           |    3157562|nifH                  |                   5|                     2|                 4|
|Nitrogen           |    3157662|nifH                  |                   5|                     5|                 4|
|Nitrogen           |    3157674|nifH                  |                   5|                     5|                 4|
|Nitrogen           |    3157722|nifH                  |                   5|                     3|                 4|
|Nitrogen           |    3164104|amoA                  |                   5|                     3|                 4|
|Nitrogen           |    3165372|nifH                  |                   5|                     2|                 4|
|Nitrogen           |   32307889|narG                  |                   5|                     2|                 4|
|Nitrogen           |   32307925|narG                  |                   5|                     2|                 4|
|Nitrogen           |   32307927|narG                  |                   5|                     2|                 4|
|Nitrogen           |   32307929|narG                  |                   5|                     5|                 4|
|Nitrogen           |   32470881|NiR                   |                   5|                     3|                 4|
|Nitrogen           |   32895084|nirS                  |                   5|                     5|                 4|
|Nitrogen           |   32895106|nirS                  |                   5|                     2|                 4|
|Nitrogen           |   33385703|nifH                  |                   5|                     5|                 4|
|Nitrogen           |   33391293|nosZ                  |                   5|                     2|                 4|
|Nitrogen           |   33391309|nosZ                  |                   5|                     2|                 4|
|Nitrogen           |   33391471|nosZ                  |                   5|                     2|                 4|
|Nitrogen           |   33391477|nosZ                  |                   5|                     2|                 4|
|Nitrogen           |   33391483|nosZ                  |                   5|                     5|                 4|
|Nitrogen           |   33415333|nasA                  |                   5|                     5|                 4|
|Nitrogen           |   34332835|narG                  |                   5|                     5|                 4|
|Nitrogen           |   34494872|ureC                  |                   5|                     2|                 4|
|Nitrogen           |   34497684|napA                  |                   5|                     5|                 4|
|Nitrogen           |   37548698|nifH                  |                   5|                     2|                 4|
|Nitrogen           |   37548718|nifH                  |                   5|                     5|                 4|
|Nitrogen           |   37924855|nifH                  |                   5|                     2|                 4|
|Nitrogen           |   38427002|narG                  |                   5|                     2|                 4|
|Nitrogen           |   41078903|nifH                  |                   5|                     2|                 4|
|Nitrogen           |   41349577|narG                  |                   5|                     5|                 4|
|Nitrogen           |    4454060|norB                  |                   5|                     5|                 4|
|Nitrogen           |   44829093|nifH                  |                   5|                     5|                 4|
|Nitrogen           |   45386179|narG                  |                   5|                     5|                 4|
|Nitrogen           |   45386181|narG                  |                   5|                     5|                 4|
|Nitrogen           |   45386199|narG                  |                   5|                     2|                 4|
|Nitrogen           |   45386203|narG                  |                   5|                     2|                 4|
|Nitrogen           |   45386205|narG                  |                   5|                     2|                 4|
|Nitrogen           |    4545090|nosZ                  |                   5|                     5|                 4|
|Nitrogen           |      46106|nifH                  |                   5|                     5|                 4|
|Nitrogen           |    4633555|nosZ                  |                   5|                     5|                 4|
|Nitrogen           |    4633573|nosZ                  |                   5|                     2|                 4|
|Nitrogen           |   46850218|nirS                  |                   5|                     5|                 4|
|Nitrogen           |   49611352|napA                  |                   5|                     2|                 4|
|Nitrogen           |   51534775|nirS                  |                   5|                     2|                 4|
|Nitrogen           |   55979055|nifH                  |                   5|                     2|                 4|
|Nitrogen           |   56798123|narG                  |                   5|                     2|                 4|
|Nitrogen           |   59892268|nifH                  |                   5|                     2|                 4|
|Nitrogen           |     600093|napA                  |                   5|                     5|                 4|
|Nitrogen           |   61808016|nosZ                  |                   5|                     2|                 4|
|Nitrogen           |   62003505|narG                  |                   5|                     2|                 4|
|Nitrogen           |   62390077|narG                  |                   5|                     2|                 4|
|Nitrogen           |   62825529|nifH                  |                   5|                     2|                 4|
|Nitrogen           |   63081982|nosZ                  |                   5|                     2|                 4|
|Nitrogen           |   63081994|nosZ                  |                   5|                     5|                 4|
|Nitrogen           |   66801853|narG                  |                   5|                     2|                 4|
|Nitrogen           |   66801859|narG                  |                   5|                     5|                 4|
|Nitrogen           |   66801983|narG                  |                   5|                     2|                 4|
|Nitrogen           |   67985209|ureC                  |                   5|                     5|                 4|
|Nitrogen           |   68135461|nifH                  |                   5|                     2|                 4|
|Nitrogen           |   68510348|nirS                  |                   5|                     2|                 4|
|Nitrogen           |   71402608|nasA                  |                   5|                     2|                 4|
|Nitrogen           |   71402610|nasA                  |                   5|                     5|                 4|
|Nitrogen           |   71402616|nasA                  |                   5|                     3|                 4|
|Nitrogen           |   73535201|nifH                  |                   5|                     2|                 4|
|Nitrogen           |   73762768|nirK                  |                   5|                     2|                 4|
|Nitrogen           |   73762890|nirK                  |                   5|                     2|                 4|
|Nitrogen           |   73762984|nirK                  |                   5|                     2|                 4|
|Nitrogen           |   73763046|nirK                  |                   5|                     2|                 4|
|Nitrogen           |   73763102|nirK                  |                   5|                     2|                 4|
|Nitrogen           |   74038370|nirS                  |                   5|                     2|                 4|
|Nitrogen           |   74038386|nirS                  |                   5|                     2|                 4|
|Nitrogen           |   74038472|nirK                  |                   5|                     2|                 4|
|Nitrogen           |   76056963|narG                  |                   5|                     2|                 4|
|Nitrogen           |   76057545|nosZ                  |                   5|                     5|                 4|
|Nitrogen           |   76058111|nosZ                  |                   5|                     2|                 4|
|Nitrogen           |   76557515|ureC                  |                   5|                     5|                 4|
|Nitrogen           |   76577376|nirS                  |                   5|                     2|                 4|
|Nitrogen           |   76577396|nirS                  |                   5|                     5|                 4|
|Nitrogen           |   76667345|nifH                  |                   5|                     5|                 4|
|Nitrogen           |   76667400|nifH                  |                   5|                     2|                 4|
|Nitrogen           |   77378443|nirS                  |                   5|                     5|                 4|
|Nitrogen           |   77378626|nirS                  |                   5|                     2|                 4|
|Nitrogen           |   77378701|nirS                  |                   5|                     2|                 4|
|Nitrogen           |   77698229|nosZ                  |                   5|                     2|                 4|
|Nitrogen           |   77820040|nirS                  |                   5|                     2|                 4|
|Nitrogen           |   77820052|nirS                  |                   5|                     2|                 4|
|Nitrogen           |   78057472|amoA                  |                   5|                     2|                 4|
|Nitrogen           |   78093528|narG                  |                   5|                     2|                 4|
|Nitrogen           |   78093554|narG                  |                   5|                     2|                 4|
|Nitrogen           |   78093574|narG                  |                   5|                     3|                 4|
|Nitrogen           |   78093582|narG                  |                   5|                     3|                 4|
|Nitrogen           |   78102419|nifH                  |                   5|                     2|                 4|
|Nitrogen           |   78214144|nasA                  |                   5|                     2|                 4|
|Nitrogen           |   78365334|nrfA                  |                   5|                     5|                 4|
|Nitrogen           |   81176591|nosZ                  |                   5|                     2|                 4|
|Nitrogen           |   81251653|nirS                  |                   5|                     5|                 4|
|Nitrogen           |   82570859|amoA                  |                   5|                     5|                 4|
|Nitrogen           |   83316730|nirK                  |                   5|                     2|                 4|
|Nitrogen           |   83316848|nirK                  |                   5|                     2|                 4|
|Nitrogen           |   83316998|nirK                  |                   5|                     5|                 4|
|Nitrogen           |   83317002|nirK                  |                   5|                     5|                 4|
|Nitrogen           |   83317102|nirK                  |                   5|                     2|                 4|
|Nitrogen           |   83318826|nirS                  |                   5|                     2|                 4|
|Nitrogen           |   83414383|gdh                   |                   5|                     5|                 4|
|Nitrogen           |   83647104|norB                  |                   5|                     5|                 4|
|Nitrogen           |   83724530|nasA                  |                   5|                     2|                 4|
|Nitrogen           |   84494383|nasA                  |                   5|                     2|                 4|
|Nitrogen           |   84693457|ureC                  |                   5|                     5|                 4|
|Nitrogen           |   84711534|nifH                  |                   5|                     2|                 4|
|Nitrogen           |   85069178|nifH                  |                   5|                     5|                 4|
|Nitrogen           |   85669952|ureC                  |                   5|                     5|                 4|
|Nitrogen           |   85687794|nifH                  |                   5|                     2|                 4|
|Nitrogen           |   87136659|nasA                  |                   5|                     5|                 4|
|Nitrogen           |   87251639|nirS                  |                   5|                     2|                 4|
|Nitrogen           |   87281040|nirK                  |                   5|                     5|                 4|
|Nitrogen           |   87281050|nirK                  |                   5|                     2|                 4|
|Nitrogen           |   87281376|nirS                  |                   5|                     5|                 4|
|Nitrogen           |   89340989|ureC                  |                   5|                     5|                 4|
|Nitrogen           |   89349520|ureC                  |                   5|                     2|                 4|
|Nitrogen           |   89512536|nifH                  |                   5|                     2|                 4|
|Nitrogen           |   89512616|nifH                  |                   5|                     2|                 4|
|Nitrogen           |   89512768|nifH                  |                   5|                     5|                 4|
|Nitrogen           |   89512880|nifH                  |                   5|                     5|                 4|
|Nitrogen           |   90420342|ureC                  |                   5|                     2|                 4|
|Nitrogen           |   91684421|ureC                  |                   5|                     2|                 4|
|Nitrogen           |   91807172|nosZ                  |                   5|                     2|                 4|
|Nitrogen           |   93353758|ureC                  |                   5|                     2|                 4|
|Nitrogen           |   94471237|narG                  |                   5|                     2|                 4|
|Nitrogen           |   94471271|narG                  |                   5|                     5|                 4|
|Nitrogen           |   94471277|narG                  |                   5|                     2|                 4|
|Nitrogen           |   94471355|narG                  |                   5|                     2|                 4|
|Nitrogen           |   95133480|nrfA                  |                   5|                     5|                 4|
|Nitrogen           |   99083045|nifH                  |                   5|                     2|                 4|
|Nitrogen           |   99083201|nifH                  |                   5|                     2|                 4|
|Nitrogen           |   99083239|nifH                  |                   5|                     2|                 4|
|Nitrogen           |   99083279|nifH                  |                   5|                     2|                 4|
|Nitrogen           |   99083293|nifH                  |                   5|                     5|                 4|
|Nitrogen           |   99083357|nifH                  |                   5|                     2|                 4|
|OrganicRemediation |   10174629|alkH                  |                   5|                     2|                 4|
|OrganicRemediation |   10441635|bphF1                 |                   5|                     2|                 4|
|OrganicRemediation |  104781693|nagI                  |                   5|                     5|                 4|
|OrganicRemediation |  104783570|hmgB                  |                   5|                     2|                 4|
|OrganicRemediation |   10580737|atzA                  |                   5|                     2|                 4|
|OrganicRemediation |  107015899|nitA                  |                   5|                     2|                 4|
|OrganicRemediation |  107027283|mdlA                  |                   5|                     5|                 4|
|OrganicRemediation |  108761575|phtA                  |                   5|                     5|                 4|
|OrganicRemediation |  108768762|benAB                 |                   5|                     3|                 4|
|OrganicRemediation |  108769877|tfdA                  |                   5|                     3|                 4|
|OrganicRemediation |  108801912|linB                  |                   5|                     2|                 4|
|OrganicRemediation |  108803396|mdlA                  |                   5|                     3|                 4|
|OrganicRemediation |  108805150|phdCI                 |                   5|                     5|                 4|
|OrganicRemediation |  109455707|HBH                   |                   5|                     2|                 4|
|OrganicRemediation |  109457038|pcaG                  |                   5|                     5|                 4|
|OrganicRemediation |  110162116|bphB                  |                   5|                     2|                 4|
|OrganicRemediation |  110284142|mdlC                  |                   5|                     5|                 4|
|OrganicRemediation |  110284454|nagG                  |                   5|                     2|                 4|
|OrganicRemediation |  110285385|pcaG                  |                   5|                     5|                 4|
|OrganicRemediation |  110285730|atzA                  |                   5|                     5|                 4|
|OrganicRemediation |   11037227|pcaG                  |                   5|                     2|                 4|
|OrganicRemediation |  110611605|mauAB                 |                   5|                     2|                 4|
|OrganicRemediation |  110634738|catB                  |                   5|                     5|                 4|
|OrganicRemediation |  110680036|dehH                  |                   5|                     2|                 4|
|OrganicRemediation |  110816912|nhh                   |                   5|                     2|                 4|
|OrganicRemediation |  110818270|nmoA                  |                   5|                     3|                 4|
|OrganicRemediation |  110818906|benAB                 |                   5|                     2|                 4|
|OrganicRemediation |  110819766|chnB                  |                   5|                     2|                 4|
|OrganicRemediation |  110820383|bphF1                 |                   5|                     2|                 4|
|OrganicRemediation |  110823847|bphF1                 |                   5|                     2|                 4|
|OrganicRemediation |  110825676|tftH                  |                   5|                     2|                 4|
|OrganicRemediation |  111017618|atzC                  |                   5|                     5|                 4|
|OrganicRemediation |  111024870|xylJ                  |                   5|                     5|                 4|
|OrganicRemediation |  111116464|xylG                  |                   5|                     2|                 4|
|OrganicRemediation |  111148637|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |  111150334|chnB                  |                   5|                     2|                 4|
|OrganicRemediation |  111152179|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |  111219909|phtA                  |                   5|                     2|                 4|
|OrganicRemediation |  111220927|trzN                  |                   5|                     2|                 4|
|OrganicRemediation |  111222973|bbsG                  |                   5|                     2|                 4|
|OrganicRemediation |  111223613|tfdA                  |                   5|                     2|                 4|
|OrganicRemediation |  111224155|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |  111224243|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |  111224754|nmoA                  |                   5|                     5|                 4|
|OrganicRemediation |  111281564|mdlA                  |                   5|                     5|                 4|
|OrganicRemediation |  111613455|phn                   |                   5|                     5|                 4|
|OrganicRemediation |  111617415|nitA                  |                   5|                     5|                 4|
|OrganicRemediation |  112821398|carA                  |                   5|                     2|                 4|
|OrganicRemediation |  112821609|mdlC                  |                   5|                     2|                 4|
|OrganicRemediation |  113473784|proO                  |                   5|                     2|                 4|
|OrganicRemediation |  113526420|bphA                  |                   5|                     2|                 4|
|OrganicRemediation |  113526479|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |  113526720|Catechol              |                   5|                     2|                 4|
|OrganicRemediation |  113529320|nagL                  |                   5|                     5|                 4|
|OrganicRemediation |  113724267|nhh                   |                   5|                     2|                 4|
|OrganicRemediation |  113866173|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |  113866660|tftH                  |                   5|                     3|                 4|
|OrganicRemediation |  113869328|trzN                  |                   5|                     5|                 4|
|OrganicRemediation |  113900683|atzA                  |                   5|                     2|                 4|
|OrganicRemediation |  114190027|tftH                  |                   5|                     5|                 4|
|OrganicRemediation |  114228090|nitA                  |                   5|                     2|                 4|
|OrganicRemediation |  114316760|exaA                  |                   5|                     3|                 4|
|OrganicRemediation |  114321506|phtA                  |                   5|                     5|                 4|
|OrganicRemediation |  114328667|nagK                  |                   5|                     2|                 4|
|OrganicRemediation |  114341130|mdlC                  |                   5|                     5|                 4|
|OrganicRemediation |  114537544|nagG                  |                   5|                     2|                 4|
|OrganicRemediation |  114541356|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |  114541523|hmgB                  |                   5|                     2|                 4|
|OrganicRemediation |  114542109|catB                  |                   5|                     2|                 4|
|OrganicRemediation |  114542306|bclA                  |                   5|                     3|                 4|
|OrganicRemediation |  114738227|hmgC                  |                   5|                     2|                 4|
|OrganicRemediation |  114761465|alkK                  |                   5|                     5|                 4|
|OrganicRemediation |  115253419|tftH                  |                   5|                     2|                 4|
|OrganicRemediation |  115253427|tftH                  |                   5|                     5|                 4|
|OrganicRemediation |  115253852|nahA                  |                   5|                     5|                 4|
|OrganicRemediation |  115254579|phn                   |                   5|                     2|                 4|
|OrganicRemediation |  115365025|PobA                  |                   5|                     5|                 4|
|OrganicRemediation |  115365761|hmgC                  |                   5|                     2|                 4|
|OrganicRemediation |  115368281|linB                  |                   5|                     3|                 4|
|OrganicRemediation |  115379096|todF                  |                   5|                     2|                 4|
|OrganicRemediation |  115379787|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |  115390791|pcpE                  |                   5|                     2|                 4|
|OrganicRemediation |  115402751|GCoADH                |                   5|                     2|                 4|
|OrganicRemediation |  115421752|nagG                  |                   5|                     5|                 4|
|OrganicRemediation |  115422863|nagK                  |                   5|                     2|                 4|
|OrganicRemediation |  115422994|nagI                  |                   5|                     2|                 4|
|OrganicRemediation |  115423046|Arylest               |                   5|                     3|                 4|
|OrganicRemediation |  115516566|bclA                  |                   5|                     2|                 4|
|OrganicRemediation |  115519753|HBH                   |                   5|                     2|                 4|
|OrganicRemediation |  115588271|mdlC                  |                   5|                     5|                 4|
|OrganicRemediation |  116073047|todF                  |                   5|                     2|                 4|
|OrganicRemediation |  116196686|tfdA                  |                   5|                     5|                 4|
|OrganicRemediation |  116227479|trzN                  |                   5|                     2|                 4|
|OrganicRemediation |  116228733|hmgA                  |                   5|                     2|                 4|
|OrganicRemediation |  116255378|linC                  |                   5|                     2|                 4|
|OrganicRemediation |  116621051|exaA                  |                   5|                     5|                 4|
|OrganicRemediation |  116624095|exaA                  |                   5|                     5|                 4|
|OrganicRemediation |  116695363|pcpA                  |                   5|                     2|                 4|
|OrganicRemediation |  116750382|atzA                  |                   5|                     5|                 4|
|OrganicRemediation |  117562695|tutFDG                |                   5|                     5|                 4|
|OrganicRemediation |  117619683|tfdA                  |                   5|                     2|                 4|
|OrganicRemediation |  117649506|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |  117980556|pcaG                  |                   5|                     5|                 4|
|OrganicRemediation |  117983924|nitA                  |                   5|                     2|                 4|
|OrganicRemediation |  117984481|benAB                 |                   5|                     5|                 4|
|OrganicRemediation |  117984652|mdlA                  |                   5|                     5|                 4|
|OrganicRemediation |  117985751|phtA                  |                   5|                     2|                 4|
|OrganicRemediation |  117985797|pnbA                  |                   5|                     5|                 4|
|OrganicRemediation |  117991419|atzC                  |                   5|                     2|                 4|
|OrganicRemediation |  117991693|tfdA                  |                   5|                     5|                 4|
|OrganicRemediation |  117992682|pcaG                  |                   5|                     5|                 4|
|OrganicRemediation |  117998411|tftH                  |                   5|                     2|                 4|
|OrganicRemediation |  117999746|tfdA                  |                   5|                     5|                 4|
|OrganicRemediation |  118003060|nagG                  |                   5|                     2|                 4|
|OrganicRemediation |  118006278|exaA                  |                   5|                     5|                 4|
|OrganicRemediation |  118007842|nmoA                  |                   5|                     5|                 4|
|OrganicRemediation |  118010934|catB                  |                   5|                     3|                 4|
|OrganicRemediation |  118028586|tfdA                  |                   5|                     2|                 4|
|OrganicRemediation |  118028954|Arylest               |                   5|                     3|                 4|
|OrganicRemediation |  118030226|hmgB                  |                   5|                     5|                 4|
|OrganicRemediation |  118036361|nmoA                  |                   5|                     5|                 4|
|OrganicRemediation |  118046180|catechol_B            |                   5|                     5|                 4|
|OrganicRemediation |  118048274|hmgB                  |                   5|                     5|                 4|
|OrganicRemediation |  118060426|nitA                  |                   5|                     3|                 4|
|OrganicRemediation |  118151833|bphA                  |                   5|                     5|                 4|
|OrganicRemediation |  118170179|bphC                  |                   5|                     2|                 4|
|OrganicRemediation |  118171327|mdlC                  |                   5|                     2|                 4|
|OrganicRemediation |  118171930|nhh                   |                   5|                     5|                 4|
|OrganicRemediation |  118172952|bphD                  |                   5|                     2|                 4|
|OrganicRemediation |  118462395|tfdA                  |                   5|                     2|                 4|
|OrganicRemediation |  118470455|trzE                  |                   5|                     5|                 4|
|OrganicRemediation |  118473742|pcpB                  |                   5|                     5|                 4|
|OrganicRemediation |  118502249|nitro                 |                   5|                     2|                 4|
|OrganicRemediation |  118590352|GCoADH                |                   5|                     5|                 4|
|OrganicRemediation |  118592435|phn                   |                   5|                     5|                 4|
|OrganicRemediation |  118616210|proO                  |                   5|                     2|                 4|
|OrganicRemediation |  118618327|trzN                  |                   5|                     2|                 4|
|OrganicRemediation |  118648765|nitA                  |                   5|                     2|                 4|
|OrganicRemediation |  118660516|atzC                  |                   5|                     2|                 4|
|OrganicRemediation |  118666265|mhpA                  |                   5|                     2|                 4|
|OrganicRemediation |  118666819|nagI                  |                   5|                     2|                 4|
|OrganicRemediation |  118667820|hmgB                  |                   5|                     2|                 4|
|OrganicRemediation |  118670769|tfdA                  |                   5|                     2|                 4|
|OrganicRemediation |  118672084|nhh                   |                   5|                     2|                 4|
|OrganicRemediation |  118673628|GCoADH                |                   5|                     3|                 4|
|OrganicRemediation |  118677392|nitro                 |                   5|                     2|                 4|
|OrganicRemediation |  118701835|nagK                  |                   5|                     5|                 4|
|OrganicRemediation |  118703430|mdlA                  |                   5|                     2|                 4|
|OrganicRemediation |  118703443|oxdB                  |                   5|                     3|                 4|
|OrganicRemediation |  118705772|mauAB                 |                   5|                     2|                 4|
|OrganicRemediation |  118706429|mdlC                  |                   5|                     2|                 4|
|OrganicRemediation |  118706460|hmgA                  |                   5|                     2|                 4|
|OrganicRemediation |  118706975|xylJ                  |                   5|                     5|                 4|
|OrganicRemediation |  118711202|mdlA                  |                   5|                     5|                 4|
|OrganicRemediation |  118719753|nitA                  |                   5|                     5|                 4|
|OrganicRemediation |  118729863|mdlA                  |                   5|                     2|                 4|
|OrganicRemediation |  118748889|hmgC                  |                   5|                     5|                 4|
|OrganicRemediation |  118750314|mdlA                  |                   5|                     5|                 4|
|OrganicRemediation |  118758099|pcaG                  |                   5|                     5|                 4|
|OrganicRemediation |  118758862|exaA                  |                   5|                     3|                 4|
|OrganicRemediation |  118760123|tfdA                  |                   5|                     5|                 4|
|OrganicRemediation |  118760441|tfdA                  |                   5|                     2|                 4|
|OrganicRemediation |  118761912|nitA                  |                   5|                     5|                 4|
|OrganicRemediation |  118772235|bclA                  |                   5|                     2|                 4|
|OrganicRemediation |  119194055|hdnO                  |                   5|                     2|                 4|
|OrganicRemediation |  119226112|pheA                  |                   5|                     2|                 4|
|OrganicRemediation |  119372765|mdlA                  |                   5|                     2|                 4|
|OrganicRemediation |  119376225|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |  119376333|nmoA                  |                   5|                     2|                 4|
|OrganicRemediation |  119376530|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |  119377281|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |  119396542|nitA                  |                   5|                     5|                 4|
|OrganicRemediation |  119401909|nagG                  |                   5|                     2|                 4|
|OrganicRemediation |  119451215|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |  119451939|pimF                  |                   5|                     3|                 4|
|OrganicRemediation |  119463866|todF                  |                   5|                     2|                 4|
|OrganicRemediation |  119468370|hmgB                  |                   5|                     3|                 4|
|OrganicRemediation |  119474775|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |  119483592|pheA                  |                   5|                     3|                 4|
|OrganicRemediation |  119497609|tfdA                  |                   5|                     2|                 4|
|OrganicRemediation |  119497641|oxdB                  |                   5|                     5|                 4|
|OrganicRemediation |  119504470|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |  119504526|exaA                  |                   5|                     2|                 4|
|OrganicRemediation |  119535710|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |  119536197|pimF                  |                   5|                     3|                 4|
|OrganicRemediation |  119670562|catechol_B            |                   5|                     5|                 4|
|OrganicRemediation |  119671145|tomA                  |                   5|                     5|                 4|
|OrganicRemediation |  119671242|proO                  |                   5|                     2|                 4|
|OrganicRemediation |  119671772|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |  119713673|linB                  |                   5|                     5|                 4|
|OrganicRemediation |  119715090|mdlA                  |                   5|                     2|                 4|
|OrganicRemediation |  119715861|trzN                  |                   5|                     2|                 4|
|OrganicRemediation |  119716380|bphF1                 |                   5|                     5|                 4|
|OrganicRemediation |  119817911|ohbAB                 |                   5|                     5|                 4|
|OrganicRemediation |  119817918|Catechol              |                   5|                     5|                 4|
|OrganicRemediation |  119819014|catB                  |                   5|                     2|                 4|
|OrganicRemediation |  119856034|atzB                  |                   5|                     5|                 4|
|OrganicRemediation |  119896759|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |  119898248|akbD                  |                   5|                     5|                 4|
|OrganicRemediation |  119899262|exaA                  |                   5|                     5|                 4|
|OrganicRemediation |  119947283|phn                   |                   5|                     3|                 4|
|OrganicRemediation |  119948118|GCoADH                |                   5|                     5|                 4|
|OrganicRemediation |  119954449|chnB                  |                   5|                     5|                 4|
|OrganicRemediation |  119954834|phtA                  |                   5|                     5|                 4|
|OrganicRemediation |  119955673|GCoADH                |                   5|                     2|                 4|
|OrganicRemediation |  119958183|HcaB                  |                   5|                     2|                 4|
|OrganicRemediation |  119960760|nmoA                  |                   5|                     5|                 4|
|OrganicRemediation |  119960877|pcaG                  |                   5|                     5|                 4|
|OrganicRemediation |  119963032|nmoA                  |                   5|                     2|                 4|
|OrganicRemediation |  120326688|phn                   |                   5|                     2|                 4|
|OrganicRemediation |  120587318|nagI                  |                   5|                     5|                 4|
|OrganicRemediation |  120588548|mdlA                  |                   5|                     2|                 4|
|OrganicRemediation |  120589303|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |  120593785|pimF                  |                   5|                     3|                 4|
|OrganicRemediation |  120595004|nagI                  |                   5|                     5|                 4|
|OrganicRemediation |  120595860|atzC                  |                   5|                     5|                 4|
|OrganicRemediation |  121530927|cbdA                  |                   5|                     5|                 4|
|OrganicRemediation |  121554547|phn                   |                   5|                     2|                 4|
|OrganicRemediation |  121556154|nahA                  |                   5|                     5|                 4|
|OrganicRemediation |  121604026|hmgB                  |                   5|                     5|                 4|
|OrganicRemediation |  121604041|GCoADH                |                   5|                     5|                 4|
|OrganicRemediation |  121605839|bclA                  |                   5|                     5|                 4|
|OrganicRemediation |  121609909|phn                   |                   5|                     5|                 4|
|OrganicRemediation |  121610344|Arylest               |                   5|                     5|                 4|
|OrganicRemediation |  121611130|nagI                  |                   5|                     3|                 4|
|OrganicRemediation |  121611522|tftH                  |                   5|                     2|                 4|
|OrganicRemediation |  121698955|chnB                  |                   5|                     2|                 4|
|OrganicRemediation |  121716444|BpH                   |                   5|                     5|                 4|
|OrganicRemediation |  121720022|Catechol              |                   5|                     5|                 4|
|OrganicRemediation |  121997927|todF                  |                   5|                     2|                 4|
|OrganicRemediation |  123978259|catB                  |                   5|                     2|                 4|
|OrganicRemediation |  124004655|alkB                  |                   5|                     2|                 4|
|OrganicRemediation |  124261240|pcpB                  |                   5|                     3|                 4|
|OrganicRemediation |  124261619|exaA                  |                   5|                     2|                 4|
|OrganicRemediation |  124875211|linB                  |                   5|                     5|                 4|
|OrganicRemediation |  124875943|mdlA                  |                   5|                     5|                 4|
|OrganicRemediation |   12539415|catB                  |                   5|                     2|                 4|
|OrganicRemediation |  126314902|Catechol              |                   5|                     2|                 4|
|OrganicRemediation |  126314905|pheA                  |                   5|                     2|                 4|
|OrganicRemediation |  126320482|mauAB                 |                   5|                     3|                 4|
|OrganicRemediation |  126360302|tomA                  |                   5|                     2|                 4|
|OrganicRemediation |  126628391|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |  126629956|ohbAB                 |                   5|                     5|                 4|
|OrganicRemediation |  126659614|phn                   |                   5|                     5|                 4|
|OrganicRemediation |  126706125|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |  126707782|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |  126707786|GCoADH                |                   5|                     2|                 4|
|OrganicRemediation |  126707826|tftH                  |                   5|                     5|                 4|
|OrganicRemediation |  126710588|pheA                  |                   5|                     2|                 4|
|OrganicRemediation |  126726584|tftH                  |                   5|                     2|                 4|
|OrganicRemediation |  126729627|nagG                  |                   5|                     5|                 4|
|OrganicRemediation |  126734307|PobA                  |                   5|                     5|                 4|
|OrganicRemediation |  126736475|bclA                  |                   5|                     2|                 4|
|OrganicRemediation |  126740618|PobA                  |                   5|                     2|                 4|
|OrganicRemediation |   13242054|phtA                  |                   5|                     2|                 4|
|OrganicRemediation |  133737398|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |  133753301|pcaG                  |                   5|                     2|                 4|
|OrganicRemediation |  133910667|pimF                  |                   5|                     3|                 4|
|OrganicRemediation |  133912249|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |  133913605|exaA                  |                   5|                     2|                 4|
|OrganicRemediation |  134094017|dehH                  |                   5|                     2|                 4|
|OrganicRemediation |  134099599|pcpB                  |                   5|                     2|                 4|
|OrganicRemediation |  134100479|hmgA                  |                   5|                     5|                 4|
|OrganicRemediation |  134142977|alkB                  |                   5|                     5|                 4|
|OrganicRemediation |   13421694|PhaB                  |                   5|                     2|                 4|
|OrganicRemediation |   13423933|BADH                  |                   5|                     2|                 4|
|OrganicRemediation |   13423947|pcaG                  |                   5|                     5|                 4|
|OrganicRemediation |   13423948|pcaG                  |                   5|                     2|                 4|
|OrganicRemediation |   13424093|hmgA                  |                   5|                     2|                 4|
|OrganicRemediation |   13474529|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |   13872512|nicdehydr             |                   5|                     2|                 4|
|OrganicRemediation |   13872675|alkB                  |                   5|                     5|                 4|
|OrganicRemediation |   13937459|atzA                  |                   5|                     5|                 4|
|OrganicRemediation |   14024489|linB                  |                   5|                     5|                 4|
|OrganicRemediation |   14025036|nmoA                  |                   5|                     3|                 4|
|OrganicRemediation |   14026765|pcaG                  |                   5|                     5|                 4|
|OrganicRemediation |   14192843|bphA                  |                   5|                     2|                 4|
|OrganicRemediation |   14289338|ohbAB                 |                   5|                     5|                 4|
|OrganicRemediation |    1437475|Catechol              |                   5|                     5|                 4|
|OrganicRemediation |  144947381|trzN                  |                   5|                     2|                 4|
|OrganicRemediation |  145216739|pimF                  |                   5|                     3|                 4|
|OrganicRemediation |   14523368|trzN                  |                   5|                     2|                 4|
|OrganicRemediation |  145233815|Catechol              |                   5|                     3|                 4|
|OrganicRemediation |  145238310|tftH                  |                   5|                     2|                 4|
|OrganicRemediation |  145306102|hmgB                  |                   5|                     5|                 4|
|OrganicRemediation |  145322730|bphA                  |                   5|                     2|                 4|
|OrganicRemediation |  145322744|catB                  |                   5|                     5|                 4|
|OrganicRemediation |  145322746|Catechol              |                   5|                     5|                 4|
|OrganicRemediation |  145555045|tfdA                  |                   5|                     5|                 4|
|OrganicRemediation |  145569839|pcaG                  |                   5|                     3|                 4|
|OrganicRemediation |  145570144|nitro                 |                   5|                     2|                 4|
|OrganicRemediation |  145570245|ohbAB                 |                   5|                     2|                 4|
|OrganicRemediation |  145570252|Catechol              |                   5|                     5|                 4|
|OrganicRemediation |  145604653|linB                  |                   5|                     2|                 4|
|OrganicRemediation |  145607671|nagI                  |                   5|                     5|                 4|
|OrganicRemediation |  146190249|P450                  |                  NA|                     2|                NA|
|OrganicRemediation |  146190611|hmgC                  |                   5|                     5|                 4|
|OrganicRemediation |  146190990|tfdA                  |                   5|                     2|                 4|
|OrganicRemediation |  146192094|Catechol              |                   5|                     2|                 4|
|OrganicRemediation |  146192287|Catechol              |                   5|                     2|                 4|
|OrganicRemediation |  146194009|nmoA                  |                   5|                     5|                 4|
|OrganicRemediation |  146274160|atzA                  |                   5|                     2|                 4|
|OrganicRemediation |  146277258|exaA                  |                   5|                     5|                 4|
|OrganicRemediation |  146282613|exaA                  |                   5|                     2|                 4|
|OrganicRemediation |  146282666|trzN                  |                   5|                     5|                 4|
|OrganicRemediation |  146308720|atzB                  |                   5|                     3|                 4|
|OrganicRemediation |  146323215|trzN                  |                   5|                     3|                 4|
|OrganicRemediation |  146340767|linB                  |                   5|                     2|                 4|
|OrganicRemediation |  146342325|exaA                  |                   5|                     2|                 4|
|OrganicRemediation |  146342967|mdlC                  |                   5|                     2|                 4|
|OrganicRemediation |  146401765|nhh                   |                   5|                     2|                 4|
|OrganicRemediation |  146402524|pcaG                  |                   5|                     5|                 4|
|OrganicRemediation |  146403580|phn                   |                   5|                     5|                 4|
|OrganicRemediation |  146405349|mdlC                  |                   5|                     2|                 4|
|OrganicRemediation |  146405786|atzC                  |                   5|                     3|                 4|
|OrganicRemediation |  146405992|exaA                  |                   5|                     2|                 4|
|OrganicRemediation |  146406344|PobA                  |                   5|                     2|                 4|
|OrganicRemediation |  146407164|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |  146407643|nhh                   |                   5|                     2|                 4|
|OrganicRemediation |  146409397|exaA                  |                   5|                     2|                 4|
|OrganicRemediation |  146409927|hmgC                  |                   5|                     2|                 4|
|OrganicRemediation |  146410039|mdlD                  |                   5|                     5|                 4|
|OrganicRemediation |  146410882|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |   14715450|xylG                  |                   5|                     2|                 4|
|OrganicRemediation |  147676913|bco                   |                   5|                     2|                 4|
|OrganicRemediation |  147829487|phn                   |                   5|                     2|                 4|
|OrganicRemediation |  148251653|mdlA                  |                   5|                     3|                 4|
|OrganicRemediation |  148253971|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |  148254095|pimF                  |                   5|                     3|                 4|
|OrganicRemediation |  148254171|HBH                   |                   5|                     2|                 4|
|OrganicRemediation |  148254987|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |  148255462|mdlC                  |                   5|                     2|                 4|
|OrganicRemediation |  148255491|linB                  |                   5|                     5|                 4|
|OrganicRemediation |  148264066|linB                  |                   5|                     2|                 4|
|OrganicRemediation |  148498414|tfdA                  |                   5|                     2|                 4|
|OrganicRemediation |  148498930|atzA                  |                   5|                     5|                 4|
|OrganicRemediation |  148498932|nagI                  |                   5|                     3|                 4|
|OrganicRemediation |  148498935|GCoADH                |                   5|                     5|                 4|
|OrganicRemediation |  148499185|Catechol              |                   5|                     5|                 4|
|OrganicRemediation |  148501343|mauAB                 |                   5|                     5|                 4|
|OrganicRemediation |  148502274|nahA                  |                   5|                     5|                 4|
|OrganicRemediation |  148518215|xlnD                  |                   5|                     3|                 4|
|OrganicRemediation |  148551013|tftH                  |                   5|                     2|                 4|
|OrganicRemediation |  148554700|tftH                  |                   5|                     2|                 4|
|OrganicRemediation |  148555965|nagI                  |                   5|                     5|                 4|
|OrganicRemediation |  148556254|hmgC                  |                   5|                     5|                 4|
|OrganicRemediation |  148557259|mauAB                 |                   5|                     3|                 4|
|OrganicRemediation |  149123831|hmgB                  |                   5|                     5|                 4|
|OrganicRemediation |  149141115|phdCI                 |                   5|                     5|                 4|
|OrganicRemediation |  149200797|phn                   |                   5|                     2|                 4|
|OrganicRemediation |  149203209|mdlA                  |                   5|                     5|                 4|
|OrganicRemediation |  149279372|catB                  |                   5|                     2|                 4|
|OrganicRemediation |  149811157|nagG                  |                   5|                     5|                 4|
|OrganicRemediation |  149812055|dehH                  |                   5|                     5|                 4|
|OrganicRemediation |  149812609|phn                   |                   5|                     2|                 4|
|OrganicRemediation |  149913902|nhh                   |                   5|                     2|                 4|
|OrganicRemediation |  149914858|GCoADH                |                   5|                     5|                 4|
|OrganicRemediation |  149914873|bclA                  |                   5|                     5|                 4|
|OrganicRemediation |  149917477|phdCI                 |                   5|                     5|                 4|
|OrganicRemediation |  149921726|GCoADH                |                   5|                     5|                 4|
|OrganicRemediation |     149983|adpB                  |                   5|                     2|                 4|
|OrganicRemediation |  150028717|nhh                   |                   5|                     2|                 4|
|OrganicRemediation |  150395819|exaA                  |                   5|                     5|                 4|
|OrganicRemediation |  150410129|hmgB                  |                   5|                     5|                 4|
|OrganicRemediation |   15073413|alkK                  |                   5|                     2|                 4|
|OrganicRemediation |   15074184|P450                  |                  NA|                     2|                NA|
|OrganicRemediation |   15074413|nagG                  |                   5|                     2|                 4|
|OrganicRemediation |   15075621|trzN                  |                   5|                     2|                 4|
|OrganicRemediation |   15076977|CDD                   |                   5|                     2|                 4|
|OrganicRemediation |  150837673|hmgC                  |                   5|                     5|                 4|
|OrganicRemediation |  150864666|tfdA                  |                   5|                     2|                 4|
|OrganicRemediation |  150956737|HcaACD                |                   5|                     3|                 4|
|OrganicRemediation |  151281305|atzB                  |                   5|                     5|                 4|
|OrganicRemediation |  151362106|nmoA                  |                   5|                     2|                 4|
|OrganicRemediation |  151362235|nitro                 |                   5|                     2|                 4|
|OrganicRemediation |  151559546|mdlA                  |                   5|                     2|                 4|
|OrganicRemediation |  151562937|pcaG                  |                   5|                     5|                 4|
|OrganicRemediation |   15157168|ChnA                  |                   5|                     5|                 4|
|OrganicRemediation |  151575684|phdCI                 |                   5|                     5|                 4|
|OrganicRemediation |  151577898|tfdA                  |                   5|                     3|                 4|
|OrganicRemediation |  152970670|mhpB                  |                   5|                     5|                 4|
|OrganicRemediation |  153888844|mdlC                  |                   5|                     2|                 4|
|OrganicRemediation |  153898402|AmiE                  |                   5|                     2|                 4|
|OrganicRemediation |  153901527|exaA                  |                   5|                     5|                 4|
|OrganicRemediation |  154156025|chnB                  |                   5|                     5|                 4|
|OrganicRemediation |  154156769|alkH                  |                   5|                     2|                 4|
|OrganicRemediation |  154159159|nmoA                  |                   5|                     3|                 4|
|OrganicRemediation |  154159345|phn                   |                   5|                     5|                 4|
|OrganicRemediation |  154160043|phdCI                 |                   5|                     5|                 4|
|OrganicRemediation |  154162660|trzE                  |                   5|                     2|                 4|
|OrganicRemediation |  154245081|Catechol              |                   5|                     2|                 4|
|OrganicRemediation |  154246624|PobA                  |                   5|                     5|                 4|
|OrganicRemediation |  154246680|atzB                  |                   5|                     5|                 4|
|OrganicRemediation |  154247259|hmgC                  |                   5|                     5|                 4|
|OrganicRemediation |  154251285|tfdA                  |                   5|                     2|                 4|
|OrganicRemediation |  154280953|Catechol              |                   5|                     2|                 4|
|OrganicRemediation |  156056144|Catechol              |                   5|                     5|                 4|
|OrganicRemediation |  156523540|todC                  |                   5|                     5|                 4|
|OrganicRemediation |  156523572|todC                  |                   5|                     2|                 4|
|OrganicRemediation |  157145830|dmsA                  |                   5|                     5|                 4|
|OrganicRemediation |  157368345|atzC                  |                   5|                     2|                 4|
|OrganicRemediation |  157371265|bphF1                 |                   5|                     5|                 4|
|OrganicRemediation |  157406625|chnB                  |                   5|                     2|                 4|
|OrganicRemediation |  157681309|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |  157693271|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |  157704433|dfbA                  |                   5|                     5|                 4|
|OrganicRemediation |   15791066|trzA                  |                   5|                     5|                 4|
|OrganicRemediation |  157912157|nhh                   |                   5|                     5|                 4|
|OrganicRemediation |  157913776|pcaG                  |                   5|                     5|                 4|
|OrganicRemediation |  158187218|hmgC                  |                   5|                     5|                 4|
|OrganicRemediation |  158315414|atzA                  |                   5|                     2|                 4|
|OrganicRemediation |  158331243|trzN                  |                   5|                     5|                 4|
|OrganicRemediation |  158331393|bphC                  |                   5|                     5|                 4|
|OrganicRemediation |  158331693|nmoA                  |                   5|                     5|                 4|
|OrganicRemediation |  158332056|nagG                  |                   5|                     2|                 4|
|OrganicRemediation |  158392731|exaA                  |                   5|                     5|                 4|
|OrganicRemediation |  158424782|nmoA                  |                   5|                     5|                 4|
|OrganicRemediation |  158439606|GcdB                  |                   5|                     2|                 4|
|OrganicRemediation |  158449904|bphF1                 |                   5|                     2|                 4|
|OrganicRemediation |   15890855|mdlA                  |                   5|                     2|                 4|
|OrganicRemediation |   16126643|4HBH                  |                   5|                     5|                 4|
|OrganicRemediation |    1652273|POBMO                 |                   5|                     3|                 4|
|OrganicRemediation |     170525|pheA                  |                   5|                     5|                 4|
|OrganicRemediation |   17431383|nagG                  |                   5|                     2|                 4|
|OrganicRemediation |    1764155|tfdB                  |                   5|                     5|                 4|
|OrganicRemediation |    1841362|tdnB                  |                   5|                     3|                 4|
|OrganicRemediation |   19073918|PceA                  |                   5|                     5|                 4|
|OrganicRemediation |   19915122|trzN                  |                   5|                     2|                 4|
|OrganicRemediation |   21221525|PobA                  |                   5|                     5|                 4|
|OrganicRemediation |   21239747|BMO                   |                   5|                     2|                 4|
|OrganicRemediation |   21242623|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |   21886618|cumA                  |                   5|                     2|                 4|
|OrganicRemediation |   22036072|bphA                  |                   5|                     5|                 4|
|OrganicRemediation |   22074061|phtA                  |                   5|                     5|                 4|
|OrganicRemediation |   22417101|pcpE                  |                   5|                     2|                 4|
|OrganicRemediation |   23014714|GCoADH                |                   5|                     2|                 4|
|OrganicRemediation |   23450983|bclA                  |                   5|                     5|                 4|
|OrganicRemediation |   23492828|BADH                  |                   5|                     2|                 4|
|OrganicRemediation |   23494061|pheA                  |                   5|                     5|                 4|
|OrganicRemediation |   24460036|cpnA                  |                   5|                     2|                 4|
|OrganicRemediation |   24460037|cpnA                  |                   5|                     2|                 4|
|OrganicRemediation |   24984794|Catechol              |                   5|                     5|                 4|
|OrganicRemediation |   24985196|PobA                  |                   5|                     2|                 4|
|OrganicRemediation |    2627157|bphF1                 |                   5|                     5|                 4|
|OrganicRemediation |   27348472|PhaB                  |                   5|                     2|                 4|
|OrganicRemediation |   27349245|pcpE                  |                   5|                     2|                 4|
|OrganicRemediation |   27350592|4HBH                  |                   5|                     3|                 4|
|OrganicRemediation |   27351087|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |   27351704|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |   27353880|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |   27354699|HBH                   |                   5|                     5|                 4|
|OrganicRemediation |   27355922|exaA                  |                   5|                     2|                 4|
|OrganicRemediation |   27375227|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |   27376430|alkK                  |                   5|                     2|                 4|
|OrganicRemediation |   27378647|fcbA                  |                   5|                     2|                 4|
|OrganicRemediation |   27381154|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |   27381531|PobA                  |                   5|                     5|                 4|
|OrganicRemediation |   27382949|HBH                   |                   5|                     3|                 4|
|OrganicRemediation |   27529597|dehH109               |                   5|                     3|                 4|
|OrganicRemediation |   28853963|hmgC                  |                   5|                     2|                 4|
|OrganicRemediation |   28869111|HBH                   |                   5|                     2|                 4|
|OrganicRemediation |   28918781|Catechol              |                   5|                     2|                 4|
|OrganicRemediation |   28921991|hmgB                  |                   5|                     2|                 4|
|OrganicRemediation |   28971849|xylL                  |                   5|                     3|                 4|
|OrganicRemediation |     294671|atzA                  |                   5|                     5|                 4|
|OrganicRemediation |   29605259|catechol_B            |                   5|                     5|                 4|
|OrganicRemediation |   29610316|atzA                  |                   5|                     5|                 4|
|OrganicRemediation |   29830406|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |    2983678|dmsA                  |                   5|                     5|                 4|
|OrganicRemediation |    3059181|bphC                  |                   5|                     2|                 4|
|OrganicRemediation |    3059212|bphB                  |                   5|                     3|                 4|
|OrganicRemediation |   31620028|alkB                  |                   5|                     2|                 4|
|OrganicRemediation |    3184044|bphA                  |                   5|                     2|                 4|
|OrganicRemediation |   32170713|nagG                  |                   5|                     5|                 4|
|OrganicRemediation |   32445505|tftH                  |                   5|                     2|                 4|
|OrganicRemediation |   32473476|alkJ                  |                   5|                     5|                 4|
|OrganicRemediation |   33333869|nidA                  |                   5|                     2|                 4|
|OrganicRemediation |   33571472|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |   33577871|cbaA                  |                   5|                     5|                 4|
|OrganicRemediation |   33601729|nagG                  |                   5|                     3|                 4|
|OrganicRemediation |   33603744|mhpC                  |                   5|                     5|                 4|
|OrganicRemediation |    3378273|BADH                  |                   5|                     2|                 4|
|OrganicRemediation |    3378419|nagG                  |                   5|                     2|                 4|
|OrganicRemediation |   34102341|atzA                  |                   5|                     2|                 4|
|OrganicRemediation |   34105223|GCoADH                |                   5|                     2|                 4|
|OrganicRemediation |   34331002|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |   34495548|alkK                  |                   5|                     5|                 4|
|OrganicRemediation |   34497008|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |   35213249|tfdA                  |                   5|                     5|                 4|
|OrganicRemediation |   35214406|akbD                  |                   5|                     2|                 4|
|OrganicRemediation |   37727205|hmgC                  |                   5|                     5|                 4|
|OrganicRemediation |   38194138|Xamo                  |                   5|                     2|                 4|
|OrganicRemediation |   38198157|Catechol              |                   5|                     2|                 4|
|OrganicRemediation |    3820518|phdCI                 |                   5|                     5|                 4|
|OrganicRemediation |   39649237|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |   39649685|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |   39649728|nhh                   |                   5|                     5|                 4|
|OrganicRemediation |   39934633|nitA                  |                   5|                     5|                 4|
|OrganicRemediation |   39934782|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |   39972653|mdlC                  |                   5|                     2|                 4|
|OrganicRemediation |   40286838|scnABC                |                   5|                     2|                 4|
|OrganicRemediation |   40742599|pheA                  |                   5|                     5|                 4|
|OrganicRemediation |   40746932|tfdA                  |                   5|                     5|                 4|
|OrganicRemediation |    4096595|pcaG                  |                   5|                     5|                 4|
|OrganicRemediation |   41406443|linB                  |                   5|                     2|                 4|
|OrganicRemediation |   41406647|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |   41408155|linB                  |                   5|                     2|                 4|
|OrganicRemediation |   42475466|bphC                  |                   5|                     5|                 4|
|OrganicRemediation |   42475470|bphC                  |                   5|                     5|                 4|
|OrganicRemediation |   42475484|bphC                  |                   5|                     5|                 4|
|OrganicRemediation |     425214|bphF1                 |                   5|                     5|                 4|
|OrganicRemediation |   42558703|trzN                  |                   5|                     5|                 4|
|OrganicRemediation |   42627732|Catechol              |                   5|                     5|                 4|
|OrganicRemediation |   45649067|bco                   |                   5|                     2|                 4|
|OrganicRemediation |    4580557|tfdB                  |                   5|                     2|                 4|
|OrganicRemediation |    4586274|cumA                  |                   5|                     2|                 4|
|OrganicRemediation |   46096526|hmgA                  |                   5|                     5|                 4|
|OrganicRemediation |   46111153|linB                  |                   5|                     2|                 4|
|OrganicRemediation |   46115524|nagI                  |                   5|                     2|                 4|
|OrganicRemediation |   46318036|tftH                  |                   5|                     3|                 4|
|OrganicRemediation |    4886552|PobA                  |                   5|                     5|                 4|
|OrganicRemediation |   49072886|phtA                  |                   5|                     5|                 4|
|OrganicRemediation |   49612946|trzE                  |                   5|                     5|                 4|
|OrganicRemediation |   50084607|mdlD                  |                   5|                     2|                 4|
|OrganicRemediation |   50121086|nmoA                  |                   5|                     3|                 4|
|OrganicRemediation |    5042259|hmgB                  |                   5|                     3|                 4|
|OrganicRemediation |   50545986|alkK                  |                   5|                     2|                 4|
|OrganicRemediation |   51243146|phtA                  |                   5|                     3|                 4|
|OrganicRemediation |   52747747|nitA                  |                   5|                     2|                 4|
|OrganicRemediation |   52785978|trzN                  |                   5|                     2|                 4|
|OrganicRemediation |   54013939|akbF                  |                   5|                     2|                 4|
|OrganicRemediation |   54016528|bphF1                 |                   5|                     5|                 4|
|OrganicRemediation |   54018310|GCoADH                |                   5|                     2|                 4|
|OrganicRemediation |   54018723|trzN                  |                   5|                     3|                 4|
|OrganicRemediation |   54023925|tfdA                  |                   5|                     5|                 4|
|OrganicRemediation |   54027279|hmgB                  |                   5|                     2|                 4|
|OrganicRemediation |   54302010|dmsA                  |                   5|                     5|                 4|
|OrganicRemediation |   54310848|phtA                  |                   5|                     2|                 4|
|OrganicRemediation |   54633216|pcpA                  |                   5|                     5|                 4|
|OrganicRemediation |   55229628|nitA                  |                   5|                     2|                 4|
|OrganicRemediation |   55230407|catB                  |                   5|                     2|                 4|
|OrganicRemediation |   56311637|ChnA                  |                   5|                     2|                 4|
|OrganicRemediation |   56312526|bbsG                  |                   5|                     5|                 4|
|OrganicRemediation |   56380369|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |   56421223|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |   56478726|ebdABC                |                   5|                     2|                 4|
|OrganicRemediation |   56677530|phn                   |                   5|                     2|                 4|
|OrganicRemediation |   56678387|nbaC                  |                   5|                     5|                 4|
|OrganicRemediation |   56679094|nagG                  |                   5|                     5|                 4|
|OrganicRemediation |   56680245|nagI                  |                   5|                     5|                 4|
|OrganicRemediation |   56695306|bphC                  |                   5|                     3|                 4|
|OrganicRemediation |   56698487|catB                  |                   5|                     2|                 4|
|OrganicRemediation |   56698616|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |   56708834|pcaG                  |                   5|                     5|                 4|
|OrganicRemediation |   56708836|4HBH                  |                   5|                     2|                 4|
|OrganicRemediation |   56709218|dehH109               |                   5|                     5|                 4|
|OrganicRemediation |   56710254|nitro                 |                   5|                     2|                 4|
|OrganicRemediation |   56798493|catechol_B            |                   5|                     5|                 4|
|OrganicRemediation |   56963827|atzA                  |                   5|                     2|                 4|
|OrganicRemediation |   57233678|Ni_Fe_hydrogenase     |                  NA|                     2|                NA|
|OrganicRemediation |   57233751|rd                    |                   5|                     5|                 4|
|OrganicRemediation |   58264298|nbaC                  |                   5|                     5|                 4|
|OrganicRemediation |   58269470|tfdA                  |                   5|                     5|                 4|
|OrganicRemediation |    5881858|chnB                  |                   5|                     3|                 4|
|OrganicRemediation |     598361|bphD                  |                   5|                     2|                 4|
|OrganicRemediation |   61611849|nagG                  |                   5|                     2|                 4|
|OrganicRemediation |   61611861|xylF                  |                   5|                     2|                 4|
|OrganicRemediation |   62391506|mdlD                  |                   5|                     2|                 4|
|OrganicRemediation |   62423205|mdlC                  |                   5|                     5|                 4|
|OrganicRemediation |   62424102|pheA                  |                   5|                     2|                 4|
|OrganicRemediation |   62426151|PobA                  |                   5|                     2|                 4|
|OrganicRemediation |   62468050|bco                   |                   5|                     2|                 4|
|OrganicRemediation |   62468052|bco                   |                   5|                     2|                 4|
|OrganicRemediation |   62468068|bco                   |                   5|                     2|                 4|
|OrganicRemediation |   63003127|bphA                  |                   5|                     2|                 4|
|OrganicRemediation |   63253985|ALN                   |                   5|                     2|                 4|
|OrganicRemediation |   63256070|pcaG                  |                   5|                     3|                 4|
|OrganicRemediation |   63256320|pheA                  |                   5|                     2|                 4|
|OrganicRemediation |   63257262|hmgA                  |                   5|                     2|                 4|
|OrganicRemediation |    6460912|linB                  |                   5|                     3|                 4|
|OrganicRemediation |    6469467|mdlA                  |                   5|                     3|                 4|
|OrganicRemediation |    6624274|bphF1                 |                   5|                     2|                 4|
|OrganicRemediation |   66351663|nhh                   |                   5|                     5|                 4|
|OrganicRemediation |   66573474|PhaB                  |                   5|                     2|                 4|
|OrganicRemediation |   67084895|HcaB                  |                   5|                     2|                 4|
|OrganicRemediation |   67156338|Catechol              |                   5|                     2|                 4|
|OrganicRemediation |   67158162|bphA                  |                   5|                     5|                 4|
|OrganicRemediation |   67159104|tfdA                  |                   5|                     2|                 4|
|OrganicRemediation |   67159150|nmoA                  |                   5|                     3|                 4|
|OrganicRemediation |   67159151|tfdA                  |                   5|                     5|                 4|
|OrganicRemediation |   67540632|pheA                  |                   5|                     5|                 4|
|OrganicRemediation |   67543188|Arylest               |                   5|                     3|                 4|
|OrganicRemediation |   67903158|tftH                  |                   5|                     2|                 4|
|OrganicRemediation |   67920647|dehH                  |                   5|                     2|                 4|
|OrganicRemediation |   68200092|nmoA                  |                   5|                     5|                 4|
|OrganicRemediation |   68231993|mdlC                  |                   5|                     2|                 4|
|OrganicRemediation |   68262858|GCoADH                |                   5|                     2|                 4|
|OrganicRemediation |   68344601|alkB                  |                   5|                     2|                 4|
|OrganicRemediation |   68345513|xylL                  |                   5|                     2|                 4|
|OrganicRemediation |   68345516|ohbAB                 |                   5|                     2|                 4|
|OrganicRemediation |   68345965|trzN                  |                   5|                     5|                 4|
|OrganicRemediation |   68536649|alkK                  |                   5|                     2|                 4|
|OrganicRemediation |   69289587|adpB                  |                   5|                     5|                 4|
|OrganicRemediation |   70984443|pheA                  |                   5|                     3|                 4|
|OrganicRemediation |   70989413|hmgC                  |                   5|                     2|                 4|
|OrganicRemediation |   71082703|phtA                  |                   5|                     2|                 4|
|OrganicRemediation |   71144796|linB                  |                   5|                     5|                 4|
|OrganicRemediation |   71159022|Xamo                  |                   5|                     2|                 4|
|OrganicRemediation |   71556174|nitro                 |                   5|                     2|                 4|
|OrganicRemediation |   71557669|phn                   |                   5|                     2|                 4|
|OrganicRemediation |   71558041|pcaG                  |                   5|                     2|                 4|
|OrganicRemediation |   71847594|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |   71848480|cumA                  |                   5|                     5|                 4|
|OrganicRemediation |   71849048|Xamo                  |                   5|                     5|                 4|
|OrganicRemediation |   71906013|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |   71915204|GCoADH                |                   5|                     2|                 4|
|OrganicRemediation |   71919878|cmuA                  |                   5|                     2|                 4|
|OrganicRemediation |   72121176|HBH                   |                   5|                     5|                 4|
|OrganicRemediation |   72121247|mauAB                 |                   5|                     2|                 4|
|OrganicRemediation |   72121305|Catechol              |                   5|                     2|                 4|
|OrganicRemediation |   72121326|tfdA                  |                   5|                     2|                 4|
|OrganicRemediation |   72161684|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |   73537680|mdlB                  |                   5|                     2|                 4|
|OrganicRemediation |   73537913|mauAB                 |                   5|                     5|                 4|
|OrganicRemediation |   73538380|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |   73542239|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |   73660999|rd                    |                   5|                     5|                 4|
|OrganicRemediation |   73671379|bphC                  |                   5|                     5|                 4|
|OrganicRemediation |   73671381|CDD                   |                   5|                     2|                 4|
|OrganicRemediation |   73749324|rd                    |                   5|                     2|                 4|
|OrganicRemediation |   74053796|todC                  |                   5|                     2|                 4|
|OrganicRemediation |   74053812|todC                  |                   5|                     5|                 4|
|OrganicRemediation |   74056460|trzN                  |                   5|                     5|                 4|
|OrganicRemediation |   74316965|atzA                  |                   5|                     2|                 4|
|OrganicRemediation |   74422044|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |   74422789|benAB                 |                   5|                     5|                 4|
|OrganicRemediation |   74484309|cmtAb                 |                   5|                     5|                 4|
|OrganicRemediation |    7573249|proO                  |                   5|                     2|                 4|
|OrganicRemediation |    7573260|PobA                  |                   5|                     5|                 4|
|OrganicRemediation |   76557015|mdlA                  |                   5|                     5|                 4|
|OrganicRemediation |   76558660|pimF                  |                   5|                     3|                 4|
|OrganicRemediation |   76803740|alkB                  |                   5|                     5|                 4|
|OrganicRemediation |   76882658|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |   77384103|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |   77454825|alkH                  |                   5|                     3|                 4|
|OrganicRemediation |   77456903|trzN                  |                   5|                     3|                 4|
|OrganicRemediation |   77457139|hmgC                  |                   5|                     5|                 4|
|OrganicRemediation |   77459186|Catechol              |                   5|                     5|                 4|
|OrganicRemediation |   77460699|PobA                  |                   5|                     2|                 4|
|OrganicRemediation |   77465534|pimF                  |                   5|                     3|                 4|
|OrganicRemediation |   77543332|arhA                  |                   5|                     2|                 4|
|OrganicRemediation |   78060920|atzC                  |                   5|                     2|                 4|
|OrganicRemediation |   78060999|bbsG                  |                   5|                     2|                 4|
|OrganicRemediation |   78194684|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |   78516023|pheA                  |                   5|                     2|                 4|
|OrganicRemediation |   78697312|tftH                  |                   5|                     5|                 4|
|OrganicRemediation |   78698389|mdlD                  |                   5|                     3|                 4|
|OrganicRemediation |   81296437|Xamo                  |                   5|                     5|                 4|
|OrganicRemediation |   82714914|nitro                 |                   5|                     2|                 4|
|OrganicRemediation |   83025982|alkB                  |                   5|                     3|                 4|
|OrganicRemediation |   83025990|alkB                  |                   5|                     5|                 4|
|OrganicRemediation |   83026002|alkB                  |                   5|                     5|                 4|
|OrganicRemediation |   83575287|phn                   |                   5|                     2|                 4|
|OrganicRemediation |   83576489|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |   83590236|dmsA                  |                   5|                     5|                 4|
|OrganicRemediation |   83593491|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |   83594442|pchCF                 |                   5|                     5|                 4|
|OrganicRemediation |   83653427|atzA                  |                   5|                     3|                 4|
|OrganicRemediation |   83716898|alkK                  |                   5|                     2|                 4|
|OrganicRemediation |   83723511|pcaG                  |                   5|                     5|                 4|
|OrganicRemediation |   83747378|chnB                  |                   5|                     5|                 4|
|OrganicRemediation |   83749278|nagL                  |                   5|                     5|                 4|
|OrganicRemediation |   83767284|hmgB                  |                   5|                     3|                 4|
|OrganicRemediation |   83815437|hmgB                  |                   5|                     2|                 4|
|OrganicRemediation |   83835800|Catechol              |                   5|                     5|                 4|
|OrganicRemediation |   83859093|alkK                  |                   5|                     2|                 4|
|OrganicRemediation |   83942278|nagG                  |                   5|                     2|                 4|
|OrganicRemediation |   83952019|bclA                  |                   5|                     5|                 4|
|OrganicRemediation |   84358870|PobA                  |                   5|                     3|                 4|
|OrganicRemediation |   84366045|pcaG                  |                   5|                     2|                 4|
|OrganicRemediation |   84388196|mdlC                  |                   5|                     3|                 4|
|OrganicRemediation |   84388832|4HBH                  |                   5|                     5|                 4|
|OrganicRemediation |   84392203|atzB                  |                   5|                     2|                 4|
|OrganicRemediation |   84392518|phn                   |                   5|                     2|                 4|
|OrganicRemediation |   84499456|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |   84502231|hmgB                  |                   5|                     5|                 4|
|OrganicRemediation |   84502903|nhh                   |                   5|                     3|                 4|
|OrganicRemediation |   84509022|4HBH                  |                   5|                     2|                 4|
|OrganicRemediation |   84510797|GCoADH                |                   5|                     2|                 4|
|OrganicRemediation |   84517124|Catechol              |                   5|                     2|                 4|
|OrganicRemediation |   84664257|exaA                  |                   5|                     2|                 4|
|OrganicRemediation |   84664340|phtA                  |                   5|                     2|                 4|
|OrganicRemediation |   84664413|POBMO                 |                   5|                     5|                 4|
|OrganicRemediation |   84693251|GCoADH                |                   5|                     2|                 4|
|OrganicRemediation |   84714081|Apc                   |                   5|                     5|                 4|
|OrganicRemediation |   84715412|hmgB                  |                   5|                     2|                 4|
|OrganicRemediation |   84796654|todC                  |                   5|                     2|                 4|
|OrganicRemediation |   85091600|mdlA                  |                   5|                     5|                 4|
|OrganicRemediation |   85097536|hmgA                  |                   5|                     2|                 4|
|OrganicRemediation |   85109751|pheA                  |                   5|                     5|                 4|
|OrganicRemediation |    8546916|dehH                  |                   5|                     5|                 4|
|OrganicRemediation |   85668318|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |   85687921|linC                  |                   5|                     2|                 4|
|OrganicRemediation |   85704595|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |   85823677|HBH                   |                   5|                     2|                 4|
|OrganicRemediation |   85825132|pheA                  |                   5|                     5|                 4|
|OrganicRemediation |   85835475|tfdA                  |                   5|                     2|                 4|
|OrganicRemediation |   85860482|alkK                  |                   5|                     5|                 4|
|OrganicRemediation |   86136004|dehH109               |                   5|                     2|                 4|
|OrganicRemediation |   86279927|phn                   |                   5|                     3|                 4|
|OrganicRemediation |   86281200|nmoA                  |                   5|                     2|                 4|
|OrganicRemediation |   86284849|alkK                  |                   5|                     2|                 4|
|OrganicRemediation |   86356552|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |   86358023|nagL                  |                   5|                     2|                 4|
|OrganicRemediation |   86360049|nmoA                  |                   5|                     5|                 4|
|OrganicRemediation |   86572416|phtA                  |                   5|                     3|                 4|
|OrganicRemediation |   86574218|phn                   |                   5|                     2|                 4|
|OrganicRemediation |   86574230|phn                   |                   5|                     5|                 4|
|OrganicRemediation |   86607670|phn                   |                   5|                     2|                 4|
|OrganicRemediation |   86748033|hmgA                  |                   5|                     3|                 4|
|OrganicRemediation |   87135378|ohbAB                 |                   5|                     2|                 4|
|OrganicRemediation |   87135383|mauAB                 |                   5|                     5|                 4|
|OrganicRemediation |   87199696|mdlC                  |                   5|                     2|                 4|
|OrganicRemediation |   87200449|PobA                  |                   5|                     5|                 4|
|OrganicRemediation |   87310939|ChnA                  |                   5|                     2|                 4|
|OrganicRemediation |   87331836|nahA                  |                   5|                     3|                 4|
|OrganicRemediation |   87331874|bphA                  |                   5|                     2|                 4|
|OrganicRemediation |   88182477|tftH                  |                   5|                     2|                 4|
|OrganicRemediation |   88701121|atzA                  |                   5|                     2|                 4|
|OrganicRemediation |   88701127|mauAB                 |                   5|                     5|                 4|
|OrganicRemediation |   88783391|catB                  |                   5|                     5|                 4|
|OrganicRemediation |   88799829|4HBH                  |                   5|                     5|                 4|
|OrganicRemediation |   88863253|alkB                  |                   5|                     5|                 4|
|OrganicRemediation |   88863487|nbaC                  |                   5|                     5|                 4|
|OrganicRemediation |   88866251|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |   88922410|pcaG                  |                   5|                     2|                 4|
|OrganicRemediation |   89045540|GCoADH                |                   5|                     5|                 4|
|OrganicRemediation |   89053930|hmgA                  |                   5|                     5|                 4|
|OrganicRemediation |   89055304|GCoADH                |                   5|                     5|                 4|
|OrganicRemediation |   89203127|trzN                  |                   5|                     5|                 4|
|OrganicRemediation |   89274952|bphD                  |                   5|                     5|                 4|
|OrganicRemediation |   89343847|proO                  |                   5|                     5|                 4|
|OrganicRemediation |   89350026|tfdA                  |                   5|                     5|                 4|
|OrganicRemediation |   89350104|pcaG                  |                   5|                     5|                 4|
|OrganicRemediation |   89358233|hmgC                  |                   5|                     5|                 4|
|OrganicRemediation |   89358829|nmoA                  |                   5|                     3|                 4|
|OrganicRemediation |   89360249|Catechol              |                   5|                     2|                 4|
|OrganicRemediation |   89893343|dmsA                  |                   5|                     5|                 4|
|OrganicRemediation |   90104565|GCoADH                |                   5|                     5|                 4|
|OrganicRemediation |   90202964|GCoADH                |                   5|                     2|                 4|
|OrganicRemediation |   90204944|mdlC                  |                   5|                     2|                 4|
|OrganicRemediation |   90420336|trzN                  |                   5|                     5|                 4|
|OrganicRemediation |   90422410|hmgA                  |                   5|                     2|                 4|
|OrganicRemediation |    9107071|linB                  |                   5|                     2|                 4|
|OrganicRemediation |   91686232|cmtAb                 |                   5|                     2|                 4|
|OrganicRemediation |   91687594|nmoA                  |                   5|                     2|                 4|
|OrganicRemediation |   91689980|nmoA                  |                   5|                     3|                 4|
|OrganicRemediation |   91690259|pimF                  |                   5|                     3|                 4|
|OrganicRemediation |   91690461|mhpA                  |                   5|                     2|                 4|
|OrganicRemediation |   91691075|linB                  |                   5|                     5|                 4|
|OrganicRemediation |   91693344|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |   91698903|pimF                  |                   5|                     3|                 4|
|OrganicRemediation |   91698997|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |   91699761|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |   91700848|phtA                  |                   5|                     5|                 4|
|OrganicRemediation |   91710691|nmoA                  |                   5|                     2|                 4|
|OrganicRemediation |   91779182|benAB                 |                   5|                     5|                 4|
|OrganicRemediation |   91779734|PhaB                  |                   5|                     2|                 4|
|OrganicRemediation |   91780299|nmoA                  |                   5|                     2|                 4|
|OrganicRemediation |   91782304|phtA                  |                   5|                     5|                 4|
|OrganicRemediation |   91783171|nagK                  |                   5|                     2|                 4|
|OrganicRemediation |   91783200|nagL                  |                   5|                     2|                 4|
|OrganicRemediation |   91783324|pimF                  |                   5|                     2|                 4|
|OrganicRemediation |   91786039|xylC                  |                   5|                     5|                 4|
|OrganicRemediation |   91786532|pcaG                  |                   5|                     2|                 4|
|OrganicRemediation |   91786891|hmgC                  |                   5|                     5|                 4|
|OrganicRemediation |   91787789|phtA                  |                   5|                     5|                 4|
|OrganicRemediation |   91787940|Catechol              |                   5|                     5|                 4|
|OrganicRemediation |   91790351|mdlD                  |                   5|                     5|                 4|
|OrganicRemediation |   91799285|pimF                  |                   5|                     5|                 4|
|OrganicRemediation |   91801739|mdlC                  |                   5|                     3|                 4|
|OrganicRemediation |   92116515|hmgB                  |                   5|                     2|                 4|
|OrganicRemediation |   92911379|alkB                  |                   5|                     2|                 4|
|OrganicRemediation |   93356493|proO                  |                   5|                     2|                 4|
|OrganicRemediation |   94312941|pcaG                  |                   5|                     2|                 4|
|OrganicRemediation |   94425274|hmgC                  |                   5|                     2|                 4|
|OrganicRemediation |   94481134|ohbAB                 |                   5|                     5|                 4|
|OrganicRemediation |   94971855|pcaG                  |                   5|                     5|                 4|
|OrganicRemediation |   94985251|exaA                  |                   5|                     5|                 4|
|OrganicRemediation |   95110646|mauAB                 |                   5|                     5|                 4|
|OrganicRemediation |   95110681|nagK                  |                   5|                     5|                 4|
|OrganicRemediation |   95111098|PobA                  |                   5|                     5|                 4|
|OrganicRemediation |   95111537|nitA                  |                   5|                     5|                 4|
|OrganicRemediation |   95114212|alkB                  |                   5|                     5|                 4|
|OrganicRemediation |    9622537|bbsG                  |                   5|                     5|                 4|
|OrganicRemediation |    9622538|bbs                   |                   5|                     5|                 4|
|OrganicRemediation |    9651047|xylJ                  |                   5|                     3|                 4|
|OrganicRemediation |    9651051|bphF1                 |                   5|                     3|                 4|
|OrganicRemediation |   99082078|bclA                  |                   5|                     2|                 4|
|OrganicRemediation |   99082486|alkB                  |                   5|                     2|                 4|
|Phosphorus         |   11139540|ppk                   |                   5|                     5|                 4|
|Phosphorus         |  111615554|ppx                   |                   5|                     5|                 4|
|Phosphorus         |  112803043|ppk                   |                   5|                     3|                 4|
|Phosphorus         |  113877828|ppx                   |                   5|                     2|                 4|
|Phosphorus         |  114799243|ppk                   |                   5|                     2|                 4|
|Phosphorus         |  115371933|ppk                   |                   5|                     2|                 4|
|Phosphorus         |  115422025|ppk                   |                   5|                     2|                 4|
|Phosphorus         |  116098698|ppx                   |                   5|                     2|                 4|
|Phosphorus         |  116106195|ppx                   |                   5|                     2|                 4|
|Phosphorus         |  116265013|ppx                   |                   5|                     2|                 4|
|Phosphorus         |  116333349|ppx                   |                   5|                     2|                 4|
|Phosphorus         |  116698616|ppx                   |                   5|                     2|                 4|
|Phosphorus         |   14027653|ppx                   |                   5|                     5|                 4|
|Phosphorus         |   17738975|ppx                   |                   5|                     5|                 4|
|Phosphorus         |   28270519|ppk                   |                   5|                     5|                 4|
|Phosphorus         |   28899777|ppx                   |                   5|                     2|                 4|
|Phosphorus         |   33571857|ppk                   |                   5|                     5|                 4|
|Phosphorus         |   36786085|ppk                   |                   5|                     5|                 4|
|Phosphorus         |   46915039|ppx                   |                   5|                     5|                 4|
|Phosphorus         |   49532025|ppx                   |                   5|                     5|                 4|
|Phosphorus         |   56479167|ppk                   |                   5|                     5|                 4|
|Phosphorus         |   56551299|ppx                   |                   5|                     3|                 4|
|Phosphorus         |   56696014|ppx                   |                   5|                     2|                 4|
|Phosphorus         |   62425089|ppx                   |                   5|                     5|                 4|
|Phosphorus         |   71735311|ppx                   |                   5|                     2|                 4|
|Phosphorus         |   77463325|ppx                   |                   5|                     5|                 4|
|Phosphorus         |   77814691|ppx                   |                   5|                     2|                 4|
|Phosphorus         |   78776860|ppx                   |                   5|                     2|                 4|
|Phosphorus         |   82947718|ppx                   |                   5|                     5|                 4|
|Phosphorus         |   83311938|ppx                   |                   5|                     5|                 4|
|Phosphorus         |   83846326|ppx                   |                   5|                     5|                 4|
|Phosphorus         |   84393644|ppx                   |                   5|                     5|                 4|
|Phosphorus         |   84495000|ppx                   |                   5|                     5|                 4|
|Phosphorus         |   86555596|ppx                   |                   5|                     2|                 4|
|Phosphorus         |   88863099|ppx                   |                   5|                     2|                 4|
|Phosphorus         |   92118063|ppx                   |                   5|                     5|                 4|
|Phosphorus         |   94422635|ppx                   |                   5|                     2|                 4|
|Phosphorus         |   95113224|ppx                   |                   5|                     5|                 4|
|Phosphorus         |   99030482|ppk                   |                   5|                     5|                 4|
|Phosphorus         |   99030503|ppk                   |                   5|                     3|                 4|
|Phosphorus         |   99030551|ppk                   |                   5|                     2|                 4|
|Phosphorus         |   99030581|ppk                   |                   5|                     2|                 4|
|Phosphorus         |   99030609|ppk                   |                   5|                     2|                 4|
|Phosphorus         |   99030630|ppk                   |                   5|                     5|                 4|
|Phosphorus         |   99030642|ppk                   |                   5|                     3|                 4|
|SulfateReduction   |  109290313|dsrA                  |                   5|                     2|                 4|
|SulfateReduction   |  109290330|dsrA                  |                   5|                     2|                 4|
|SulfateReduction   |  109452398|dsrA                  |                   5|                     2|                 4|
|SulfateReduction   |  109452545|dsrA                  |                   5|                     2|                 4|
|SulfateReduction   |  110598809|dsrA                  |                   5|                     2|                 4|
|SulfateReduction   |  114227201|dsrA                  |                   5|                     3|                 4|
|SulfateReduction   |  116061977|dsrA                  |                   5|                     5|                 4|
|SulfateReduction   |  116700521|dsrB                  |                   5|                     5|                 4|
|SulfateReduction   |  116743200|dsrB                  |                   5|                     5|                 4|
|SulfateReduction   |  118424304|dsrA                  |                   5|                     3|                 4|
|SulfateReduction   |  118424359|dsrB                  |                   5|                     2|                 4|
|SulfateReduction   |  118424497|dsrB                  |                   5|                     3|                 4|
|SulfateReduction   |  118424511|dsrA                  |                   5|                     2|                 4|
|SulfateReduction   |  118424524|dsrB                  |                   5|                     5|                 4|
|SulfateReduction   |  118424527|dsrB                  |                   5|                     2|                 4|
|SulfateReduction   |  118424532|dsrA                  |                   5|                     2|                 4|
|SulfateReduction   |  126249775|dsrA                  |                   5|                     5|                 4|
|SulfateReduction   |  126460064|dsrA                  |                   5|                     2|                 4|
|SulfateReduction   |  126635911|dsrB                  |                   5|                     2|                 4|
|SulfateReduction   |   13249523|dsrB                  |                   5|                     5|                 4|
|SulfateReduction   |   13249525|dsrB                  |                   5|                     5|                 4|
|SulfateReduction   |   13591679|dsrB                  |                   5|                     2|                 4|
|SulfateReduction   |   13898421|dsrA                  |                   5|                     5|                 4|
|SulfateReduction   |   13992710|dsrA                  |                   5|                     2|                 4|
|SulfateReduction   |   14389261|dsrB                  |                   5|                     5|                 4|
|SulfateReduction   |  144905828|dsrA                  |                   5|                     3|                 4|
|SulfateReduction   |  146188517|dsrA                  |                   5|                     5|                 4|
|SulfateReduction   |   15077498|dsrA                  |                   5|                     2|                 4|
|SulfateReduction   |   15077499|dsrB                  |                   5|                     5|                 4|
|SulfateReduction   |  151302120|APS_AprA              |                   5|                     3|                 4|
|SulfateReduction   |  151302138|APS_AprA              |                   5|                     3|                 4|
|SulfateReduction   |  151302302|AprA                  |                   5|                     3|                 4|
|SulfateReduction   |  151302314|APS_AprA              |                   5|                     3|                 4|
|SulfateReduction   |  153810913|dsrB                  |                   5|                     3|                 4|
|SulfateReduction   |  156857651|APS_AprA              |                   5|                     3|                 4|
|SulfateReduction   |  156857781|APS_AprA              |                   5|                     3|                 4|
|SulfateReduction   |  157679437|dsrA                  |                   5|                     2|                 4|
|SulfateReduction   |  157679439|dsrA                  |                   5|                     5|                 4|
|SulfateReduction   |  157679463|dsrA                  |                   5|                     5|                 4|
|SulfateReduction   |  158523516|dsrA                  |                   5|                     5|                 4|
|SulfateReduction   |  158523904|dsrA                  |                   5|                     2|                 4|
|SulfateReduction   |   18034302|dsrB                  |                   5|                     2|                 4|
|SulfateReduction   |   18034320|dsrB                  |                   5|                     5|                 4|
|SulfateReduction   |   18034328|dsrA                  |                   5|                     2|                 4|
|SulfateReduction   |  219808356|AprA                  |                   5|                     3|                 4|
|SulfateReduction   |  224027838|APS_AprA              |                   5|                     3|                 4|
|SulfateReduction   |  224027921|APS_AprA              |                   5|                     3|                 4|
|SulfateReduction   |   33320471|dsrA                  |                   5|                     2|                 4|
|SulfateReduction   |   34017186|dsrB                  |                   5|                     5|                 4|
|SulfateReduction   |   40253032|dsrB                  |                   5|                     5|                 4|
|SulfateReduction   |   46307850|dsrA                  |                   5|                     2|                 4|
|SulfateReduction   |   46307864|dsrA                  |                   5|                     2|                 4|
|SulfateReduction   |   46307888|dsrA                  |                   5|                     2|                 4|
|SulfateReduction   |   46307904|dsrA                  |                   5|                     2|                 4|
|SulfateReduction   |   46307971|dsrA                  |                   5|                     2|                 4|
|SulfateReduction   |   46307976|dsrA                  |                   5|                     2|                 4|
|SulfateReduction   |   46308014|dsrA                  |                   5|                     5|                 4|
|SulfateReduction   |   46520023|dsrA                  |                   5|                     3|                 4|
|SulfateReduction   |   46520033|dsrA                  |                   5|                     3|                 4|
|SulfateReduction   |   53854603|dsrA                  |                   5|                     5|                 4|
|SulfateReduction   |   55978127|dsrA                  |                   5|                     2|                 4|
|SulfateReduction   |   56694506|dsrA                  |                   5|                     5|                 4|
|SulfateReduction   |   62512311|dsrA                  |                   5|                     2|                 4|
|SulfateReduction   |   62512312|dsrB                  |                   5|                     3|                 4|
|SulfateReduction   |   82618379|dsrA                  |                   5|                     2|                 4|
|SulfateReduction   |   82940331|dsrA                  |                   5|                     2|                 4|
|SulfateReduction   |   84778331|dsrA                  |                   5|                     3|                 4|
|SulfateReduction   |    8671004|dsrB                  |                   5|                     3|                 4|
|SulfateReduction   |   90954571|dsrA                  |                   5|                     3|                 4|
|SulfateReduction   |   90954583|dsrA                  |                   5|                     5|                 4|
|SulfurOxidation    |  109454940|sox                   |                   5|                     2|                 4|
|SulfurOxidation    |  118734797|sox                   |                   5|                     5|                 4|
|SulfurOxidation    |  119377453|sox                   |                   5|                     2|                 4|
|SulfurOxidation    |  120593543|sox                   |                   5|                     5|                 4|
|SulfurOxidation    |  124260399|sox                   |                   5|                     2|                 4|
|SulfurOxidation    |  126727408|sox                   |                   5|                     5|                 4|
|SulfurOxidation    |  133738324|sox                   |                   5|                     5|                 4|
|SulfurOxidation    |  146343609|sox                   |                   5|                     5|                 4|
|SulfurOxidation    |  148254221|sox                   |                   5|                     5|                 4|
|SulfurOxidation    |  148255077|sox                   |                   5|                     5|                 4|
|SulfurOxidation    |  149120191|sox                   |                   5|                     5|                 4|
|SulfurOxidation    |   15625036|sox                   |                   5|                     2|                 4|
|SulfurOxidation    |  157406237|sox                   |                   5|                     5|                 4|
|SulfurOxidation    |   26986207|sox                   |                   5|                     5|                 4|
|SulfurOxidation    |   27349262|sox                   |                   5|                     2|                 4|
|SulfurOxidation    |   39648290|sox                   |                   5|                     2|                 4|
|SulfurOxidation    |   56677636|sox                   |                   5|                     5|                 4|
|SulfurOxidation    |   78172076|sox                   |                   5|                     2|                 4|
|SulfurOxidation    |    8517646|sox                   |                   5|                     2|                 4|
|SulfurOxidation    |   85669610|sox                   |                   5|                     2|                 4|
|SulfurOxidation    |   85775219|sox                   |                   5|                     5|                 4|
|SulfurOxidation    |   86137334|sox                   |                   5|                     2|                 4|
|SulfurOxidation    |   91802028|sox                   |                   5|                     2|                 4|

```r
readr::write_csv(ds_long_intersection_basin, pathOutputGenesByBasin)
# sum(ds_long_intersection_basin$GeneCategory=="CarbonCycling")
```

# Session Information
For the sake of documentation and reproducibility, the current report was build on a system using the following software.


```
Report created by wibeasley at 2017-02-23, 23:47 -0600
```

```
R version 3.3.1 (2016-06-21)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 16.04.2 LTS

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
 [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8    LC_PAPER=en_US.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] VennDiagram_1.6.17  futile.logger_1.4.3 bindrcpp_0.1        magrittr_1.5        knitr_1.15.1       

loaded via a namespace (and not attached):
 [1] Rcpp_0.12.9          bindr_0.1            munsell_0.4.3        colorspace_1.3-2     R6_2.2.0            
 [6] highr_0.6            stringr_1.2.0        plyr_1.8.4           dplyr_0.5.0.9000     caTools_1.17.1      
[11] tools_3.3.1          webshot_0.4.0        DT_0.2               KernSmooth_2.23-15   DBI_0.5-1           
[16] lambda.r_1.1.9       htmltools_0.3.5      gtools_3.5.0         yaml_2.1.14          digest_0.6.12       
[21] lazyeval_0.2.0       assertthat_0.1       tibble_1.2           tidyr_0.6.1          readr_1.0.0         
[26] RColorBrewer_1.1-2   htmlwidgets_0.8      futile.options_1.0.0 bitops_1.0-6         evaluate_0.10       
[31] gdata_2.17.0         stringi_1.1.2        gplots_3.0.1         scales_0.4.1         jsonlite_1.2        
```
