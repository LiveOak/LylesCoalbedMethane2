



This report was automatically generated with the R package **knitr**
(version 1.10.5).


```r
# knitr::stitch_rmd(script="./Manipulate/Manipulate.R", output="./Manipulate/StitchedOutput/Manipulate.md")
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.

############################
#+ LoadSources

############################
#+ LoadPackages
# library(plyr) #This package is used, but I don't want to load it in the global namespace, in case it conflicts with a dplyr function.
library(dplyr)
library(reshape2)

############################
#+ DeclareGlobals
csvPackedIllinoisPath <- "./Data/Raw/IllinoisBasin.csv"
csvPackedCookInlet1Path <- "./Data/Raw/CookInlet1.csv"
csvPackedCookInlet2Path <- "./Data/Raw/CookInlet2.csv"
csvPackedPowderPath <- "./Data/Raw/PowderRiver.csv"

# csvChrisCategoriesPath <- "./Data/Raw/GenesReclassified.csv"
# csvCommonGenebankIDs <- "./Data/Raw/CommonGenebankIDs.csv"

csvUnpackedPath <- "./Data/Derived/Unpacked.csv"
csvUnpackedSummaryPath <- "./Data/Derived/UnpackedSummary.csv"

idNames <- c("GenbankID", "GeneName", "GeneCategory", "Replicate")
reclassifyAsMethaneProduction <- c("mcrA") #These are values in the GeneName field.
reclassifyAsSulfateReduction <- c("dsrA", "dsrB", "AprA", "APS_AprA", "APS_AprB") #These are values in the GeneName field.

############################
#+ LoadData
dsPackedIllinois <- read.csv(csvPackedIllinoisPath, stringsAsFactors=F)
dsPackedCookInlet1 <- read.csv(csvPackedCookInlet1Path, stringsAsFactors=F)
dsPackedCookInlet2 <- read.csv(csvPackedCookInlet2Path, stringsAsFactors=F)
dsPackedPowder <- read.csv(csvPackedPowderPath, stringsAsFactors=F)

############################
#+ TweakData

############################
#+ UnpackAndCombine
UnpackSite <- function( dWide, basinLabel ) {
  dLong <- reshape2::melt(dWide, id.vars=idNames, variable.name="Site", value.name="Abundance")
  dLong$Basin <- basinLabel
  return( dLong )
}

dsUnpackedIllinois <- UnpackSite(dsPackedIllinois, basinLabel="Illinois")
dsUnpackedCookInlet1 <- UnpackSite(dsPackedCookInlet1, basinLabel="CookInlet")
dsUnpackedCookInlet2 <- UnpackSite(dsPackedCookInlet2, basinLabel="CookInlet")
dsUnpackedPowder <- UnpackSite(dsPackedPowder, basinLabel="Powder")
rm(dsPackedIllinois, dsPackedCookInlet1, dsPackedCookInlet2, dsPackedPowder)

ds <- plyr::rbind.fill(dsUnpackedIllinois, dsUnpackedCookInlet1, dsUnpackedCookInlet2, dsUnpackedPowder)

rm(dsUnpackedIllinois, dsUnpackedCookInlet1, dsUnpackedCookInlet2, dsUnpackedPowder)
############################
#+ SetAbundanceFloorAndReclassify

#Set the floor of the abundance values
abundanceFloor <- min(ds$Abundance, na.rm=T)
ds$AbundanceWithFloor <- ifelse(is.na(ds$Abundance), abundanceFloor, ds$Abundance)

#Reclassify some gene categories
# sum(ds$GeneName %in% reclassifyAsSulfateReduction)
# unique(ds[ds$GeneName %in% reclassifyAsSulfateReduction, ]$Basin)
ds[ds$GeneName %in% reclassifyAsMethaneProduction, ]$GeneCategory <- "Methane Production"
ds[ds$GeneName %in% reclassifyAsSulfateReduction, ]$GeneCategory <- "Sulfate Reduction"

#Make `Site` numeric, after converting it from a factor
ds$Site <- as.character(ds$Site)
ds$Site <- as.integer(gsub(pattern="X(\\d{1,2})", replacement="\\1", x=ds$Site))

############################
#+ WriteUnpacked
write.csv(ds, file=csvUnpackedPath, row.names=F)

############################
#+ CreateSummary

#ds[]
# sum(ds$GeneName %in% reclassifyAsSulfateReduction)
# unique(ds[ds$GeneName %in% reclassifyAsSulfateReduction, ]$Basin)
# ds[ds$GeneName %in% reclassifyAsMethaneProduction, ]$GeneCategory <- "Methane Production"
# ds[ds$GeneName %in% reclassifyAsSulfateReduction, ]$GeneCategory <- "Sulfate Reduction"

#idsToReclassify <- unique(dsReclassified$GenbankID)
# for( reclassifyIndex in 1:nrow(dsReclassified)) {
#   ds[ds$GenbankID==dsReclassified$GenbankID[reclassifyIndex], 'GeneCategory'] <- dsReclassified$GeneCategory[reclassifyIndex]
# }
ProportionAboveFloor <- function( scores ) {
  #return( mean(scores[!is.na(scores)]>abundanceFloor) )
#   return( mean(scores>abundanceFloor) )
  return( mean(scores>abundanceFloor, na.rm=T) )
}
UniqueGenbankIDCount <- function( scores ) {
  #return( length(unique(as.numeric(as.character(scores)))) )
  return( length(unique(as.character(scores))) ) 
}

dsBasinGenbankIDCount <- aggregate(GenbankID ~ GeneCategory + Basin + Replicate, data=ds, FUN=UniqueGenbankIDCount, subset=!is.na(Abundance))
dsBasinGenbankIDCount <- plyr::rename(dsBasinGenbankIDCount, replace=c("GenbankID" ="UniqueGenbankIDCountInBasin"))
# # dsSummaryMeanRep <- aggregate(Abundance ~ GeneCategory + Basin + Site + Replicate, data=ds, FUN=mean)
# # dsSummaryMeanRep <- plyr::rename(dsSummaryMeanRep, replace=c("Abundance" ="AbundanceMean"))
#           
# dsSummaryMeanRep <- plyr::ddply(dsSummaryMeanRep, c("GeneCategory", "Basin", "Site"), summarize, AbundanceSE=sd(AbundanceMean))

#write a function that find the proprotions of the site's genes (in a gene category) that are above the floor
# Add that as a column in dsSummary

# dsSummary <- merge(x=dsSummaryMean, y=dsSummaryMedian, by=c("GeneCategory", "Basin", "Site"))#, suffixes=c("Mean", "Median"))
# dsSummary <- merge(x=dsSummary, y=dsSummaryProportionAboveFloor, by=c("GeneCategory", "Basin", "Site"))#, suffixes=c("Mean", "Median"))
# dsSummary <- merge(x=dsSummary, y=dsUniqueGenbankIDCount, by=c("GeneCategory", "Basin", "Site"))#, suffixes=c("Mean", "Median"))
# dsSummary <- merge(x=dsSummary, y=dsBasinGenbankIDCount, by=c("GeneCategory", "Basin"))#, suffixes=c("Mean", "Median"))
# dsSummary$AbudanceRelativeToBasin <- dsSummary$UniqueGenbankIDCount / dsSummary$UniqueGenbankIDCountInBasin

# dsSummary <- ds %>%
#   dplyr::group_by(GeneCategory, Basin, Site) %>%
#   dplyr::summarise(
#     AbundanceMean = mean(Abundance, na.rm=T),
#     AbundanceMedian = median(Abundance, na.rm=T),
#     ProportionAboveFloor = ProportionAboveFloor(Abundance),
#     UniqueGenbankIDCount = UniqueGenbankIDCount(GenbankID[!is.na(Abundance)])
#   )
# 
# dsSummary <- merge(x=dsSummary, y=dsBasinGenbankIDCount, by=c("GeneCategory", "Basin"))#, suffixes=c("Mean", "Median"))
# dsSummary$AbudanceRelativeToBasin <- dsSummary$UniqueGenbankIDCount / dsSummary$UniqueGenbankIDCountInBasin

dsSummaryWithRep <- ds %>%
  dplyr::group_by(GeneCategory, Basin, Site, Replicate) %>%
  dplyr::summarise(
    AbundanceMean = mean(Abundance, na.rm=T),
    AbundanceMedian = median(Abundance, na.rm=T),
    ProportionAboveFloor = ProportionAboveFloor(Abundance),
    UniqueGenbankIDCount = UniqueGenbankIDCount(GenbankID[!is.na(Abundance)])
  )

dsSummaryWithRep <- merge(x=dsSummaryWithRep, y=dsBasinGenbankIDCount, by=c("GeneCategory", "Basin", "Replicate"))
dsSummaryWithRep$AbudanceRelativeToBasin <- dsSummaryWithRep$UniqueGenbankIDCount / dsSummaryWithRep$UniqueGenbankIDCountInBasin


dsSummary <- dsSummaryWithRep %>%
  dplyr::group_by(GeneCategory, Basin, Site) %>%
  dplyr::summarise(
    AbundanceMean = mean(AbundanceMean, na.rm=T),
    AbudanceRelativeToBasinMean = mean(AbudanceRelativeToBasin, na.rm=T),
    AbudanceRelativeToBasinSE = sd(AbudanceRelativeToBasin, na.rm=T),
    AbudanceRelativeToBasinList = paste(AbudanceRelativeToBasin, collapse="-"),
    AbudanceRelativeToBasinCount = sum(!is.na(AbudanceRelativeToBasin))
  )

dsSummary <- as.data.frame(dsSummary)
 
dsSummary$SEHigh <- dsSummary$AbudanceRelativeToBasinMean + dsSummary$AbudanceRelativeToBasinSE
dsSummary$SELow <- dsSummary$AbudanceRelativeToBasinMean - dsSummary$AbudanceRelativeToBasinSE

# dsSummary2 <- plyr::ddply(dsSummaryWithRep, c("GeneCategory", "Basin", "Site"), summarize,
#             AbudanceRelativeToBasin = mean(AbudanceRelativeToBasin, na.rm=T),
#             AbudanceRelativeToBasinSE = sd(AbudanceRelativeToBasin, na.rm=T),
#             AbudanceRelativeToBasinList = paste(AbudanceRelativeToBasin, collapse="-"),
#             AbudanceRelativeToBasinCount = length(AbudanceRelativeToBasin))

geneCategories <- unique(ds$GeneCategory)
dsSummary$RankWithinCategory <- NA
for( category in geneCategories ) {
  match <- (dsSummary$GeneCategory==category)
  dsSummary[match, "RankWithinCategory"] <- order(dsSummary[match, "AbundanceMean"])
}

sites <- unique(ds$Site)
dsSummary$RankWithinSite <- NA
for( site in sites ) {
  match <- (dsSummary$Site==site)
  dsSummary[match, "RankWithinSite"] <- order(dsSummary[match, "AbundanceMean"])
}

############################
#+ WriteSummary
head(dsSummary, 15)
```

```
##             GeneCategory     Basin Site AbundanceMean
## 1  Antibiotic resistance CookInlet    9      5163.757
## 2  Antibiotic resistance CookInlet   10     10879.478
## 3  Antibiotic resistance CookInlet   11      9289.866
## 4  Antibiotic resistance  Illinois    1      3017.814
## 5  Antibiotic resistance  Illinois    2      9012.762
## 6  Antibiotic resistance  Illinois    3     15336.681
## 7  Antibiotic resistance  Illinois    4      7570.039
## 8  Antibiotic resistance  Illinois    5     10981.324
## 9  Antibiotic resistance    Powder   12     10183.370
## 10 Antibiotic resistance    Powder   13      8715.736
## 11 Antibiotic resistance    Powder   14      8591.464
## 12 Antibiotic resistance    Powder   15      1281.676
## 13        Bacteria phage  Illinois    1      1955.010
## 14        Bacteria phage  Illinois    2      7121.011
## 15        Bacteria phage  Illinois    3     11403.139
##    AbudanceRelativeToBasinMean AbudanceRelativeToBasinSE
## 1                    0.4017341                        NA
## 2                    0.3150289                        NA
## 3                    0.8757225                        NA
## 4                    0.2700972                0.06460522
## 5                    0.5569819                0.05004313
## 6                    0.7489949                0.01550588
## 7                    0.3078339                0.11813139
## 8                    0.5806057                0.06041361
## 9                    0.5126582                       NaN
## 10                   0.5316456                       NaN
## 11                   0.6341772                       NaN
## 12                   0.4050633                       NaN
## 13                   0.1912910                0.05757292
## 14                   0.5565561                0.02295708
## 15                   0.7215068                0.02895540
##                              AbudanceRelativeToBasinList
## 1                                      0.401734104046243
## 2                                      0.315028901734104
## 3                                      0.875722543352601
## 4                0.2-0.283040935672515-0.327250608272506
## 5   0.50828729281768-0.554385964912281-0.608272506082725
## 6   0.759116022099448-0.75672514619883-0.731143552311436
## 7  0.177900552486188-0.336842105263158-0.408759124087591
## 8  0.612154696132597-0.618713450292398-0.510948905109489
## 9                                      0.512658227848101
## 10                                     0.531645569620253
## 11                                     0.634177215189873
## 12                                      0.40506329113924
## 13 0.126865671641791-0.209302325581395-0.237704918032787
## 14 0.537313432835821-0.550387596899225-0.581967213114754
## 15 0.753731343283582-0.697674418604651-0.713114754098361
##    AbudanceRelativeToBasinCount    SEHigh     SELow RankWithinCategory
## 1                             1        NA        NA                 12
## 2                             1        NA        NA                  4
## 3                             1        NA        NA                  1
## 4                             3 0.3347024 0.2054920                  7
## 5                             3 0.6070251 0.5069388                 11
## 6                             3 0.7645008 0.7334890                 10
## 7                             3 0.4259653 0.1897025                  5
## 8                             3 0.6410193 0.5201921                  3
## 9                             1       NaN       NaN                  9
## 10                            1       NaN       NaN                  2
## 11                            1       NaN       NaN                  8
## 12                            1       NaN       NaN                  6
## 13                            3 0.2488639 0.1337181                  9
## 14                            3 0.5795132 0.5335990                  1
## 15                            3 0.7504622 0.6925514                  4
##    RankWithinSite
## 1               5
## 2               5
## 3              12
## 4               3
## 5               3
## 6               7
## 7               7
## 8               2
## 9               7
## 10              3
## 11              3
## 12              7
## 13              2
## 14              2
## 15              2
```

```r
write.csv(dsSummary, file=csvUnpackedSummaryPath, row.names=F)

table(dsSummary$GeneCategory, dsSummary$Basin)
```

```
##                        
##                         CookInlet Illinois Powder
##   Antibiotic resistance         3        5      4
##   Bacteria phage                0        5      4
##   Bioleaching                   0        5      4
##   Carbon cycling                5        5      4
##   Energy process                5        5      4
##   Metal Resistance              5        5      4
##   Methane Production            5        5      4
##   Missing                       5        0      0
##   Nitrogen                      5        5      4
##   Organic Remediation           5        5      4
##   other category                5        5      4
##   Phosphorus                    5        5      4
##   Stress                        0        5      4
##   Sulfate Reduction             5        5      4
##   Sulfur Oxidation              5        5      4
##   virulence                     0        5      4
```

```r
# CategoryMean <- function( df ) {
#   with(df, data.frame(
#     MeanRelativeAbundance = mean(df$AbudanceRelativeToBasin, na.rm=T)
#   ))
# }
# ddply(dsSummary, .(GeneCategory), CategoryMean)
```

The R session information (including the OS info, R version and all
packages used):


```r
sessionInfo()
```

```
## R version 3.2.0 Patched (2015-05-11 r68355)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 8 x64 (build 9200)
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] plyr_1.8.2     reshape2_1.4.1 dplyr_0.4.1    testit_0.4    
## [5] markdown_0.7.7 knitr_1.10.5  
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.11.6         highr_0.5           nloptr_1.0.4       
##  [4] RColorBrewer_1.1-2  formatR_1.2         git2r_0.10.1       
##  [7] bitops_1.0-6        tools_3.2.0         testthat_0.9.1.9001
## [10] digest_0.6.8        lme4_1.1-7          lubridate_1.3.3    
## [13] evaluate_0.7        memoise_0.2.1       gtable_0.1.2       
## [16] nlme_3.1-120        lattice_0.20-31     Matrix_1.2-0       
## [19] DBI_0.3.1           parallel_3.2.0      curl_0.6           
## [22] proto_0.3-10        stringr_1.0.0       httr_0.6.1         
## [25] rversions_1.0.0     devtools_1.8.0      grid_3.2.0         
## [28] XML_3.98-1.1        random_0.2.4        minqa_1.2.4        
## [31] ggplot2_1.0.1       magrittr_1.5        scales_0.2.4       
## [34] ggthemes_2.1.2      MASS_7.3-40         splines_3.2.0      
## [37] assertthat_0.1      colorspace_1.2-6    stringi_0.4-1      
## [40] lazyeval_0.1.10     RCurl_1.95-4.6      munsell_0.4.2      
## [43] crayon_1.2.1
```

```r
Sys.time()
```

```
## [1] "2015-05-20 22:57:35 CDT"
```

