Graphs of MCR vs Rate Relationship
=================================================
This report looks at the relationship between MCR genes and Adjusted Rates

<!--  Set the working directory to the repository's base directory; this assumes the report is nested inside of two directories.-->


<!-- Set the report-wide options, and point to the external code file. -->

<!-- Load the packages.  Suppress the output when loading packages. --> 

```r
require(knitr)
require(plyr)
require(scales) #For formating values in graphs
require(RColorBrewer)
require(grid) #For graphing
require(ggplot2) #For graphing
# require(mgcv, quietly=TRUE) #For the Generalized Additive Model that smooths the longitudinal graphs.
#####################################
```

<!-- Load any Global functions and variables declared in the R file.  Suppress the output. --> 

```r
options(show.signif.stars=F) #Turn off the annotations on p-values
options(stringsAsFactors=FALSE) #By default, character/string variables will NOT be automatically converted to factors.

pathInputLong <- "./Data/Derived/AllBasinsLong.csv"
pathInputWide <- "./Data/Derived/AllBasinsWide.csv"

basinOrder <- c("Illinois Basin", "Cook Inlet gas field", "Powder River Basin")
substrateOrder <- c("Formate", "Acetate", "Propionate", "Butyrate", "Valerate")
sitesToDrop <- c(7, 16, 17)

ReportTheme <- theme_bw() +
  theme(axis.ticks.length = grid::unit(0, "cm")) +
  theme(axis.text = element_text(color="gray40")) +
  theme(axis.title = element_text(color="gray40")) +
  theme(panel.border = element_rect(color="gray80")) +
  theme(axis.ticks = element_line(color="gray80")) +
  theme(strip.background=element_rect(color=NA, fill="gray95"))
#####################################
```

<!-- Declare any global functions specific to a Rmd output.  Suppress the output. --> 


<!-- Load the datasets.   -->

```r
# 'ds' stands for 'datasets'
dsLong <- read.csv(pathInputLong)
dsWide <- read.csv(pathInputWide)
# sapply(dsWide, class)

#####################################
```

<!-- Tweak the datasets.   -->

```r
#Drop the sites without microarray data
dsLong <- dsLong[!(dsLong$Site %in% sitesToDrop), ]
dsWide <- dsWide[!(dsWide$Site %in% sitesToDrop), ]

#Reorder the substrates and basins.  The default alphabetical order isn't the most intuitive
dsLong$Basin <- factor(dsLong$Basin, levels=basinOrder)
dsWide$Basin <- factor(dsWide$Basin, levels=basinOrder)
dsLong$Substrate <- factor(dsLong$Substrate, levels=substrateOrder)
dsWide$Substrate <- factor(dsWide$Substrate, levels=substrateOrder)

dsLongIllinois <- dsLong[dsLong$Basin=="Illinois Basin", ]
dsWideIllinois <- dsWide[dsWide$Basin=="Illinois Basin", ]

CalculateSiteRange <- function( d ) {
  data.frame(
    RateMin = min(d$AdjustedRate, na.rm=T),
    RateMax = max(d$AdjustedRate, na.rm=T),
    UniqueMcrGenesMin = min(d$UniqueMcrGenes, na.rm=T),
    UniqueMcrGenesMax = max(d$UniqueMcrGenes, na.rm=T),
    QuantityMcrGenesMin = min(d$QuantityMcrGenes, na.rm=T),
    QuantityMcrGenesMax = max(d$QuantityMcrGenes, na.rm=T)
  )
}
dsSiteRange <- plyr::ddply(.data=dsLong, .variables=c("Basin", "Site", "Substrate"), CalculateSiteRange)

dsCorrelation <- plyr::ddply(dsWide, c("Basin", "Substrate"), summarize, 
                             CorrRateUnique=cor(RateMean, UniqueMean, use="pairwise.complete.obs"),
                             CorrRateQuantity=cor(RateMean, QuantityMean, use="pairwise.complete.obs"))
dsCorrelation$CorrRateUniquePretty <- paste0("italic(r)==", round(dsCorrelation$CorrRateUnique, 2))
dsCorrelation$CorrRateQuantityPretty <- paste0("italic(r)==", round(dsCorrelation$CorrRateQuantity, 2))

sitesIllinois <- sort(unique(dsLong[dsLong$Basin=="Illinois Basin", "Site"]))
sitesCook <- sort(unique(dsLong[dsLong$Basin=="Cook Inlet gas field", "Site"]))
sitesPowder <- sort(unique(dsLong[dsLong$Basin=="Powder River Basin", "Site"]))

paletteSiteDark <- c(RColorBrewer::brewer.pal(n=length(sitesIllinois), "Dark2"), RColorBrewer::brewer.pal(n=length(sitesCook), "Set1"), RColorBrewer::brewer.pal(n=length(sitesPowder), "Dark2"))
names(paletteSiteDark) <- c(sitesIllinois, sitesCook, sitesPowder)

paletteSiteLight <- grDevices::adjustcolor(paletteSiteDark, alpha.f=.5)
names(paletteSiteLight) <- c(sitesIllinois, sitesCook, sitesPowder)

#####################################
```

## Notes


# Marginals

```r
ggplot(dsLongIllinois, aes(x=AdjustedRate)) + 
  geom_density() +
  facet_grid(Substrate~IncubationReplicate, scales="free_y") +
  ReportTheme +
  labs(title="Illinois Basin")
```

![plot of chunk Marginals](Figures/Marginals1.png) 

```r
ggplot(dsLongIllinois, aes(x=QuantityMcrGenes)) + 
  geom_density() +
  facet_grid(Substrate~MicroarraryReplicate, scales="free_y") +
  ReportTheme +
  labs(title="Illinois Basin")
```

![plot of chunk Marginals](Figures/Marginals2.png) 

```r
ggplot(dsLongIllinois, aes(x=UniqueMcrGenes)) + 
  geom_density() +
  facet_grid(Substrate~MicroarraryReplicate, scales="free_y") +
  ReportTheme +
  labs(title="Illinois Basin")
```

![plot of chunk Marginals](Figures/Marginals3.png) 

```r
#####################################
```

# Scatterplots
The first pair scatterplot matrices are for the *quantity* of genes, while the second pair is the count of *unique* genes.  Within each pair, the first scatterplot matrix has all replicates (2 incubation and 3 microarray replications per site), while the second within each pair shows a site's average across the replicates.

![plot of chunk Scatterplots](Figures/Scatterplots1.png) ![plot of chunk Scatterplots](Figures/Scatterplots2.png) ![plot of chunk Scatterplots](Figures/Scatterplots3.png) ![plot of chunk Scatterplots](Figures/Scatterplots4.png) 

The these scatterplots combine the site means, with the individual replicate measurements.  The rectangles show the range for each Substrate*Site group of measurements

![plot of chunk LayeredScatterplots](Figures/LayeredScatterplots1.png) ![plot of chunk LayeredScatterplots](Figures/LayeredScatterplots2.png) 

|Basin                |Substrate  | CorrRateUnique| CorrRateQuantity|
|:--------------------|:----------|--------------:|----------------:|
|Illinois Basin       |Formate    |        -0.1981|          -0.3652|
|Illinois Basin       |Acetate    |        -0.4134|          -0.5084|
|Illinois Basin       |Propionate |        -0.5498|          -0.5827|
|Illinois Basin       |Butyrate   |        -0.5554|          -0.5613|
|Illinois Basin       |Valerate   |        -0.5851|          -0.5878|
|Cook Inlet gas field |Formate    |         0.5709|           0.2731|
|Cook Inlet gas field |Acetate    |         0.5702|           0.2018|
|Cook Inlet gas field |Propionate |         0.2726|          -0.0720|
|Cook Inlet gas field |Butyrate   |         0.9834|           0.7040|
|Cook Inlet gas field |Valerate   |         0.7888|           0.2273|
|Powder River Basin   |Formate    |        -0.8813|          -0.9344|
|Powder River Basin   |Acetate    |         0.7355|           0.6244|
|Powder River Basin   |Propionate |         0.3246|           0.3564|
|Powder River Basin   |Butyrate   |         0.4556|           0.5487|
|Powder River Basin   |Valerate   |         0.7757|           0.7652|

# Questions
## Unanswered Questions
 1. - - - 
 
## Answered Questions
 1. - - - 
 
# Session Information
For the sake of documentation and reproducibility, the current report was build on a system using the following software.


```
Report created by Will at 2014-06-18, 15:54 -0500
```

```
R version 3.1.0 Patched (2014-06-15 r65949)
Platform: x86_64-w64-mingw32/x64 (64-bit)

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C                           LC_TIME=English_United States.1252    

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] ggplot2_1.0.0      RColorBrewer_1.0-5 scales_0.2.4       plyr_1.8.1         knitr_1.6         

loaded via a namespace (and not attached):
 [1] colorspace_1.2-4 digest_0.6.4     evaluate_0.5.5   formatR_0.10     gtable_0.1.2     labeling_0.2    
 [7] MASS_7.3-33      munsell_0.4.2    proto_0.3-10     Rcpp_0.11.2      reshape2_1.4     stringr_0.6.2   
[13] tools_3.1.0     
```
