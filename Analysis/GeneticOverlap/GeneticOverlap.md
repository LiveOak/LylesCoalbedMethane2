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

#####################################
```

<!-- Declare any global functions specific to a Rmd output.  Suppress the output. --> 


<!-- Load the datasets.   -->

```r
dsLong <- read.csv(pathInputLong, stringsAsFactors=FALSE)

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
Source: local data frame [2 x 2]

  GeochipVersion  Count
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
Groups: GeneCategory

           GeneCategory GeochipVersion  Count
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
Source: local data frame [16 x 4]

           GeneCategory CountTotal CountV32 CountV40
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
Source: local data frame [16 x 5]

           GeneCategory  Total Illinois CookInlet Powder
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

```
[1] 1
```

```
./Analysis/GeneticOverlap/Figures/AntibioticResistance.tiff
./Analysis/GeneticOverlap/Figures/BacteriaPhage.tiff
./Analysis/GeneticOverlap/Figures/Bioleaching.tiff
./Analysis/GeneticOverlap/Figures/CarbonCycling.tiff
./Analysis/GeneticOverlap/Figures/EnergyProcess.tiff
./Analysis/GeneticOverlap/Figures/MetalResistance.tiff
./Analysis/GeneticOverlap/Figures/MethaneProduction.tiff
./Analysis/GeneticOverlap/Figures/Missing.tiff
./Analysis/GeneticOverlap/Figures/Nitrogen.tiff
./Analysis/GeneticOverlap/Figures/OrganicRemediation.tiff
./Analysis/GeneticOverlap/Figures/Other.tiff
./Analysis/GeneticOverlap/Figures/Phosphorus.tiff
./Analysis/GeneticOverlap/Figures/Stress.tiff
./Analysis/GeneticOverlap/Figures/SulfateReduction.tiff
./Analysis/GeneticOverlap/Figures/SulfurOxidation.tiff
./Analysis/GeneticOverlap/Figures/Virulence.tiff
```

![plot of chunk PlotVennDiagrams](Figures/PlotVennDiagrams-1.png) 

# Session Information
For the sake of documentation and reproducibility, the current report was build on a system using the following software.


```
Report created by Will at 2015-07-03, 16:55 -0500
```

```
R version 3.2.1 Patched (2015-06-18 r68542)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 8 x64 (build 9200)

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C                           LC_TIME=English_United States.1252    

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] VennDiagram_1.6.9 magrittr_1.5      knitr_1.10.5     

loaded via a namespace (and not attached):
 [1] Rcpp_0.11.6        gtools_3.5.0       dplyr_0.4.2        assertthat_0.1     bitops_1.0-6       R6_2.0.1          
 [7] plyr_1.8.3         DBI_0.3.1          formatR_1.2        evaluate_0.7       scales_0.2.5       KernSmooth_2.23-14
[13] gplots_2.17.0      stringi_0.5-5      lazyeval_0.1.10    gdata_2.16.1       RColorBrewer_1.1-2 tools_3.2.1       
[19] stringr_1.0.0      munsell_0.4.2      parallel_3.2.1     colorspace_1.2-6   caTools_1.17.1    
```
