



This report was automatically generated with the R package **knitr**
(version 1.8).


```r
rm(list=ls(all=TRUE))
library(ggplot2)
library(scales)
# library(plyr)
library(grid) #For 'unit' function in tick length setting
library(lawstat) #For Levene's test

# if( names(dev.cur()) != "null device" ) dev.off()
# deviceWidth <- 10 #20 #10 #6.5
# aspectRatio <- .7
# deviceHeight <- deviceWidth * aspectRatio
# windows(width=deviceWidth, height=deviceHeight)

csvSummaryPath <- "./Data/Derived/UnpackedSummary.csv"
#csvCommonGenebankIDs <- "./Data/Derived/CommonGenebankIDs.csv"
pngFacetedOutputPath <- "./Analysis/SiteUnivariate/FiguresSingle/BarChartFaceted.png"
pngOutputPath <- "./Analysis/SiteUnivariate/FiguresSingle/BarChart.png"

ds <- read.csv(csvSummaryPath)
#dsCommonGenebankIDs <- read.csv(csvCommonGenebankIDs)

#ds <- dsRawUnpacked
#ds <- dsRawUnpacked[dsRawUnpacked$GenbankID %in% dsCommonGenebankIDs$GeneBankID, ]
#ds <- ds[!is.na(ds$Abundance), ]

geneCategoriesToDrop <- c("Antibiotic resistance", "Bacteria phage", "Bioleaching", "other category", "Missing", 
  "Stress", "virulence") #"Sulfur Oxidation",
ds <- ds[!(ds$GeneCategory %in% geneCategoriesToDrop), ]

uniqueSites <- unique(ds$Site)

ds$GeneCategoryPretty <- plyr::revalue(ds$GeneCategory, replace = c(
  "Sulfur Oxidation" = "Sulfur Oxidation"
  , "Carbon cycling" = "Carbon Cycling"
  , "Energy process" = "Energy Process"
  , "Metal Resistance" = "Metal Resistance"
  , "Methane Production" = "Methane Production"
  , "Organic Remediation" = "Organic Remediation"
  , "Sulfate Reduction" = "Sulfate Reduction"
))

CategoryMean <- function( d ) {
  with(d, data.frame(
    Mean = mean(d$AbudanceRelativeToBasinMean, na.rm=T),
    Count = sum(!is.na(d$AbudanceRelativeToBasinMean)),
    SD = sd(d$AbudanceRelativeToBasinMean, na.rm=T),
    SE = sd(d$AbudanceRelativeToBasinMean, na.rm=T) / sqrt(sum(!is.na(d$AbudanceRelativeToBasinMean)))
  ))
}
dsCategory <- plyr::ddply(ds, "GeneCategory", CategoryMean)
dsCategoryPretty <- plyr::ddply(ds, "GeneCategoryPretty", CategoryMean)
dsCategoryPretty$MeanPretty <- paste0(round(dsCategoryPretty$Mean*100), "%")
# dsCategoryPretty$Label <- paste0(".", round(dsCategoryPretty$Mean*100), "%+-%.0", round(dsCategoryPretty$SE*100))
dsCategoryPretty$Label <- paste0("bar(italic(x))==.", round(dsCategoryPretty$Mean*100), "%+-%.0", round(dsCategoryPretty$SE*100))

#geneCategory <- "Methane Production"
#geneCategory <- "Organic Remediation"
#ds <- ds[ds$GeneCategory== geneCategory, ]

# g <- ggplot(data=ds, aes(x=Site, y=..density.., fill=Basin))
# #g <- ggplot(data=ds, aes(x=Site, y=..density.., fill=Basin))
# #g <- ggplot(data=ds, aes(x=Site, y=..density.., fill=Basin, group=GeneCategory))
# #g <- ggplot(data=ds, aes(x=Site, y=..count../sum(..count..), fill=Basin))
# #g <- ggplot(data=ds, aes(x=Site, fill=Basin))
# #g <- ggplot(data=ds, aes(x=Basin, fill=Basin))
# g + geom_bar(stat="bin", binwidth=1, color="gray70") +
#   scale_y_continuous("Relative Abundance") +
#   geom_text(stat="bin", binwidth=1,aes(label=..count..), size=4) +
#   facet_wrap( ~ GeneCategory) #+
#   #guid
#1+1+3.5+3.5+4+10+1
summary(ds)
```

```
##  GeneCategory          Basin                Site       
##  Length:126         Length:126         Min.   : 1.000  
##  Class :character   Class :character   1st Qu.: 4.000  
##  Mode  :character   Mode  :character   Median : 8.500  
##                                        Mean   : 8.071  
##                                        3rd Qu.:12.000  
##                                        Max.   :15.000  
##                                                        
##  AbundanceMean       AbudanceRelativeToBasinMean AbudanceRelativeToBasinSE
##  Min.   :    8.616   Min.   :0.05882             Min.   :0.00811          
##  1st Qu.: 3014.929   1st Qu.:0.27711             1st Qu.:0.03217          
##  Median : 7553.275   Median :0.51357             Median :0.04609          
##  Mean   : 7015.543   Mean   :0.45003             Mean   :0.05282          
##  3rd Qu.: 9662.006   3rd Qu.:0.60098             3rd Qu.:0.06181          
##  Max.   :24946.595   Max.   :0.75258             Max.   :0.12119          
##                                                  NA's   :81               
##  AbudanceRelativeToBasinList AbudanceRelativeToBasinCount     SEHigh      
##  Length:126                  Min.   :1.000                Min.   :0.2042  
##  Class :character            1st Qu.:1.000                1st Qu.:0.3620  
##  Mode  :character            Median :1.000                Median :0.5995  
##                              Mean   :1.714                Mean   :0.5266  
##                              3rd Qu.:3.000                3rd Qu.:0.6488  
##                              Max.   :3.000                Max.   :0.7875  
##                                                           NA's   :81      
##      SELow         RankWithinCategory RankWithinSite   GeneCategoryPretty
##  Min.   :0.04665   Min.   : 1.0       Min.   : 1.000   Length:126        
##  1st Qu.:0.18628   1st Qu.: 4.0       1st Qu.: 4.000   Class :character  
##  Median :0.51173   Median : 7.5       Median : 8.000   Mode  :character  
##  Mean   :0.42096   Mean   : 7.5       Mean   : 7.849                     
##  3rd Qu.:0.56492   3rd Qu.:11.0       3rd Qu.:11.000                     
##  Max.   :0.73228   Max.   :14.0       Max.   :15.000                     
##  NA's   :81
```

```r
# sort(unique(ds$GeneName))
# sort(unique(ds$Site))
# plyr::ddply(ds[ds$GeneCategory=="Methane Production", ], .(Site), numcolwise(length))
# plyr::ddply(ds[ds$GeneName=="mcrA", ], .(Site), numcolwise(length))

# png(pngOutputPath, width=10, height=7.5, units="in", bg="transparent", res=500, type="cairo-png")
# > rainbow_hcl(n=3, c = 50, l = 70)
# [1] "#E495A5" "#86B875" "#7DB0DD"# 
unique(ds$Basin)
```

```
## [1] "CookInlet" "Illinois"  "Powder"
```

```r
########################################
g <- ggplot(data=ds, aes(x=factor(Site), y=AbudanceRelativeToBasinMean, fill=Basin)) +
  geom_hline(mapping=aes(yintercept=Mean, color=NULL, fill=NULL), data=dsCategoryPretty,  color="gray90", size=1.5)   +
  geom_bar(stat="identity", binwidth=1, alpha=.7) +
  geom_errorbar(mapping=aes(ymin=SELow, ymax=SEHigh),  stat="identity", binwidth=1, alpha=.7) +
  #geom_text(mapping=aes(y=Mean, label=Label, color=NULL, fill=NULL), data=dsCategoryPretty, x=Inf, color="gray60", hjust=.5, 
  geom_text(mapping=aes(label=Label, color=NULL, fill=NULL), data=dsCategoryPretty, x=Inf, y=Inf, color="gray70", hjust=1.05, 
            vjust=1.4, size=4, parse=T) +
#           vjust=-.2, size=3, angle=90, parse=T) +
  #   geom_text(stat="identity", binwidth=1,aes(y=0, label=UniqueGenbankIDCount), size=3,  hjust=1, vjust=.3, angle=270) +
  facet_wrap( ~ GeneCategoryPretty) + #, scales="free_y") +
  scale_x_discrete() + #labels=c("1", "", "3", "", "5", "6", "8", "", "10", "", "12", "", "14", "")) + 
  scale_y_continuous(labels=percent) +
  #scale_fill_discrete(h.start=120, direction=1, limits=c("Illinois","Cook Inlet","Powder")) + #, labels=c("Illinois","Cook Inlet","Powder")) +
#   scale_fill_manual(values = c("#00ba38", "#f8766d", "#619cff"), #The original colors
#   scale_fill_manual(values = c("#dcebce", "#fedcd4", "#e4e2ee"), #Matches Figs #1 & #2 exactly.
#   scale_fill_manual(values =  muted(c("#00ba38", "#f8766d", "#619cff"), l=80, c=50),  #The original colors, muted
  scale_fill_manual(values = muted(c("#dcebce", "#fedcd4", "#e4e2ee"), l=80, c=60), #Matches Figs #1 & #2, but unmuted
                    limits=c("Illinois","CookInlet","Powder"), 
                    labels=c("Illinois Basin","Cook Inlet Gas Field","Powder River Basin")) + 
  labs(x="Coalfield Samples", y="Functional Gene Relative Abundance", fill=NULL, title=NULL) +
  theme_bw() +
  theme(legend.position="top", legend.margin = unit(-.5, "cm")) +
  theme(plot.margin=unit(c(0,.15,0,0), "cm")) +
  theme(axis.ticks.length = unit(0, "cm")) +
  theme(axis.text.x=element_text(colour="gray30", size=rel(.8))) + 
  theme(axis.text.y=element_text(colour="gray30", size=rel(.8))) +
  theme(axis.ticks.length = grid::unit(0, "cm"))
g
```

<img src="figure/BarChartSummary-Rmdauto-report-1.png" title="plot of chunk auto-report" alt="plot of chunk auto-report" style="display: block; margin: auto;" />

```r
#ggsave(pngFacetedOutputPath, plot=g, width=10, height=7.5, dpi=600)
ggsave(pngFacetedOutputPath, plot=g, width=17, height=13, units="cm", dpi=600)
```

<img src="figure/BarChartSummary-Rmdauto-report-2.png" title="plot of chunk auto-report" alt="plot of chunk auto-report" style="display: block; margin: auto;" />

```r
# plot(dsCategoryMean)


# if( createFactor )  
#   dsGraph$V1 <- factor(dsGraph[, variableName])
# else
#   dsGraph$V1 <- dsGraph[, variableName]
# 
# levels(dsGraph$V1) <- gsub("/", "/\n", levels(dsGraph$V1))
# levels(dsCategory$GeneCategory)
dsCategory$GeneCategory <- plyr::revalue(dsCategory$GeneCategory, replace=c(
  "Sulfur Oxidation" = "Sulfur Oxidation"
  , "Carbon cycling" = "Carbon Cycling"
  , "Energy process" = "Energy Process"
  , "Metal Resistance" = "Metal Resistance"
  , "Methane Production" = "Methane Production"
  , "Organic Remediation" = "Organic Remediation"
  , "Sulfate Reduction" = "Sulfate Reduction"
))

dsCategory$GeneCategoryPretty <- plyr::revalue(dsCategory$GeneCategory, replace=c(
  "Sulfur Oxidation" = "Sulfur\nOxidation"
  , "Carbon Cycling" = "Carbon\nCycling"
  , "Energy Process" = "Energy\nProcess"
  , "Metal Resistance" = "Metal\nResistance"
  , "Methane Production" = "Methane\nProduction"
  , "Organic Remediation" = "Organic\nRemediation"
  , "Sulfate Reduction" = "Sulfate\nReduction"
))
                                               
# dsCategory$GeneCategory <- ordered(dsCategory$GeneCategory, levels=rev(levels(dsCategory$GeneCategory)))
# dsCategory$GeneCategoryPretty <- ordered(dsCategory$GeneCategoryPretty, levels=rev(levels(dsCategory$GeneCategoryPretty)))

# if( skipMissing ) {
#   dsGraph <- dsGraph[dsGraph$V1 != "Missing", ]
#   dsGraph <- dsGraph[!is.na(dsGraph$V1), ]
# }

p <- ggplot(dsCategory, aes(x=GeneCategory, y=Mean, ymin=Mean-SE, ymax=Mean+SE, fill=GeneCategory, label=scales::comma(Mean)))
p <- p + geom_bar(stat="identity", alpha=.7) 
p <- p + geom_pointrange(stat="identity")

p <- p + scale_y_continuous(labels = percent_format())
p <- p + scale_fill_brewer(palette="Set3")
p <- p + labs(x=NULL, y="Mean Percentage")# of Functional Genes Across Coalfields")  #"Functional\nGene Category"

p <- p + theme_bw()
p <- p + theme(legend.position = "none") +
  theme(plot.margin=unit(c(0,.15,0,0.05), "cm")) +
  theme(axis.ticks.length = unit(0, "cm")) +
  theme(axis.text.x=element_text(colour="gray30", size=rel(1))) + 
  theme(axis.text.y=element_text(colour="gray30", size=rel(1)))  
#   p <- p + geom_text(stat="bin", hjust=hjust, size=labelSize)#, labels = comma_format(digits = 1))

p <- p + coord_flip()#ylim=ylim)
p 
```

<img src="figure/BarChartSummary-Rmdauto-report-3.png" title="plot of chunk auto-report" alt="plot of chunk auto-report" style="display: block; margin: auto;" />

```r
ggsave(pngOutputPath, plot=p, width=15, height=5, units="cm", dpi=600)


stats::bartlett.test(AbundanceMean ~ GeneCategory, ds)
```

```
## 
## 	Bartlett test of homogeneity of variances
## 
## data:  AbundanceMean by GeneCategory
## Bartlett's K-squared = 4.5941, df = 8, p-value = 0.7999
```

```r
# lawstat::levene.test(AbundanceMean ~ GeneCategory, ds)
table(ds$GeneCategory)
```

```
## 
##      Carbon cycling      Energy process    Metal Resistance 
##                  14                  14                  14 
##  Methane Production            Nitrogen Organic Remediation 
##                  14                  14                  14 
##          Phosphorus   Sulfate Reduction    Sulfur Oxidation 
##                  14                  14                  14
```

```r
ds$MethaneGene <- (ds$GeneCategory=="Methane Production")
summary(lm(AbundanceMean ~ GeneCategory, ds))
```

```
## 
## Call:
## lm(formula = AbundanceMean ~ GeneCategory, data = ds)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
##  -8928  -3739    816   2622  18645 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                       6524.0     1291.0   5.053 1.62e-06 ***
## GeneCategoryEnergy process         444.9     1825.8   0.244    0.808    
## GeneCategoryMetal Resistance       997.3     1825.8   0.546    0.586    
## GeneCategoryMethane Production    -222.4     1825.8  -0.122    0.903    
## GeneCategoryNitrogen              -282.5     1825.8  -0.155    0.877    
## GeneCategoryOrganic Remediation    423.7     1825.8   0.232    0.817    
## GeneCategoryPhosphorus             787.1     1825.8   0.431    0.667    
## GeneCategorySulfate Reduction     -137.1     1825.8  -0.075    0.940    
## GeneCategorySulfur Oxidation      2412.8     1825.8   1.322    0.189    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4831 on 117 degrees of freedom
## Multiple R-squared:  0.02882,	Adjusted R-squared:  -0.03759 
## F-statistic: 0.434 on 8 and 117 DF,  p-value: 0.8985
```

```r
summary(lm(AbundanceMean ~ MethaneGene, ds))
```

```
## 
## Call:
## lm(formula = AbundanceMean ~ MethaneGene, data = ds)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7096.0 -4006.7   587.7  2557.2 18644.9 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)       7104.8      449.3  15.814   <2e-16 ***
## MethaneGeneTRUE   -803.1     1347.8  -0.596    0.552    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4755 on 124 degrees of freedom
## Multiple R-squared:  0.002855,	Adjusted R-squared:  -0.005186 
## F-statistic: 0.3551 on 1 and 124 DF,  p-value: 0.5523
```

The R session information (including the OS info, R version and all
packages used):


```r
sessionInfo()
```

```
## R version 3.1.2 Patched (2014-12-11 r67168)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
##  [1] splines   stats4    grid      stats     graphics  grDevices utils    
##  [8] datasets  methods   base     
## 
## other attached packages:
## [1] reshape2_1.4.1 dplyr_0.3.0.2  lawstat_2.4.1  VGAM_0.9-6    
## [5] mvtnorm_1.0-1  scales_0.2.4   ggplot2_1.0.0 
## 
## loaded via a namespace (and not attached):
##  [1] assertthat_0.1     colorspace_1.2-4   DBI_0.3.1         
##  [4] digest_0.6.6       evaluate_0.5.5     formatR_1.0       
##  [7] gtable_0.1.2       htmltools_0.2.6    knitr_1.8         
## [10] labeling_0.3       lazyeval_0.1.9     magrittr_1.5      
## [13] MASS_7.3-35        munsell_0.4.2      parallel_3.1.2    
## [16] plyr_1.8.1         proto_0.3-10       RColorBrewer_1.1-2
## [19] Rcpp_0.11.3        rmarkdown_0.3.11   stringr_0.6.2     
## [22] tools_3.1.2
```

```r
Sys.time()
```

```
## [1] "2014-12-18 13:37:53 CST"
```

