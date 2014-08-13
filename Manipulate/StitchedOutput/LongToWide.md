



This report was automatically generated with the R package **knitr**
(version 1.6).


```r
# knitr::stitch_rmd(script="./Manipulate/LongToWide.R", output="./Manipulate/StitchedOutput/LongToWide.md")
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
############################
#+ LoadSources

############################
#+ LoadPackages
require(knitr)
require(plyr)
# require(dplyr)
# library(reshape)

############################
#+ DeclareGlobals
options(stringsAsFactors=FALSE) #By default, character/string variables will NOT be automatically converted to factors.

pathInput <- "./Data/Raw/AllBasinsLong.csv"
pathOutput <- "./Data/Derived/IllinoisBasinsWide.csv"

basinsToInclude <- c("Illinois Basin")

############################
#+ LoadData
dsLong <- read.csv(pathInput)
colnames(dsLong)
```

```
##  [1] "Incubation.Replicate"   "Site"                  
##  [3] "Basin"                  "Substrate"             
##  [5] "Rate"                   "Unamended.control.rate"
##  [7] "Adjusted.Rate"          "Microarrary.Replicate" 
##  [9] "Unique.mcr.genes"       "Quantity.mcrgenes"
```

```r
# Dataset description can be found at: http://stat.ethz.ch/R-manual/R-devel/library/datasets/html/mtcars.html
dsLong <- plyr::rename(dsLong, replace=c(
  "Incubation.Replicate" = "IncubationReplicate"
  , "Unamended.control.rate" = "UnamendedControlRate"
  , "Adjusted.Rate" = "AdjustedRate"
  , "Microarrary.Replicate" = "MicroarraryReplicate"
  , "Unique.mcr.genes" = "UniqueMcrGenes"
  , "Quantity.mcrgenes" = "QuantityMcrGenes"
))
############################
#+ TweakData
dsLong <- dsLong[dsLong$Basin %in% basinsToInclude, ]

dsLong$AdjustedRateZ <- scale(dsLong$AdjustedRate)
dsLong$UniqueMcrGenesZ <- scale(dsLong$UniqueMcrGenes)
dsLong$QuantityMcrGenesZ <- scale(dsLong$QuantityMcrGenes)
############################
#+ Widen, results='asis'

Widen <- function( d ) {
  
  dsIncubation1 <- d[d$IncubationReplicate=="O", ]
  dsIncubation2 <- d[d$IncubationReplicate=="X", ]
  dsMicroarray1 <- d[d$MicroarraryReplicate==1, ]
  dsMicroarray2 <- d[d$MicroarraryReplicate==2, ]
  dsMicroarray3 <- d[d$MicroarraryReplicate==3, ]
  
  data.frame(
    Rate1 = dsIncubation1$AdjustedRate[1],
    Rate2 = dsIncubation2$AdjustedRate[1],
    Unique1 = dsMicroarray1$UniqueMcrGenes[1],
    Unique2 = dsMicroarray2$UniqueMcrGenes[1],
    Unique3 = dsMicroarray3$UniqueMcrGenes[1],
    Quantity1 = dsMicroarray1$QuantityMcrGenes[1],
    Quantity2 = dsMicroarray2$QuantityMcrGenes[1],
    Quantity3 = dsMicroarray3$QuantityMcrGenes[1],
    
    RateZ1 = dsIncubation1$AdjustedRateZ[1],
    RateZ2 = dsIncubation2$AdjustedRateZ[1],
    UniqueZ1 = dsMicroarray1$UniqueMcrGenesZ[1],
    UniqueZ2 = dsMicroarray2$UniqueMcrGenesZ[1],
    UniqueZ3 = dsMicroarray3$UniqueMcrGenesZ[1],
    QuantityZ1 = dsMicroarray1$QuantityMcrGenesZ[1],
    QuantityZ2 = dsMicroarray2$QuantityMcrGenesZ[1],
    QuantityZ3 = dsMicroarray3$QuantityMcrGenesZ[1]
  )
}
dsWide <- plyr::ddply(dsLong, c("Substrate", "Basin", "Site"), Widen)


############################
#+ Average
dsWide$RateMean <- (dsWide$Rate1 + dsWide$Rate2) / 2
dsWide$QuantityMean <- (dsWide$Quantity1 + dsWide$Quantity2 + dsWide$Quantity3) / 3
dsWide$UniqueMean <- (dsWide$Unique1 + dsWide$Unique2 + dsWide$Unique3) / 3

############################
#+ PrintTable
knitr::kable(dsWide, format="markdown")
```

```
## 
## 
## |Substrate  |Basin          | Site|   Rate1|   Rate2| Unique1| Unique2| Unique3| Quantity1| Quantity2| Quantity3|  RateZ1|  RateZ2| UniqueZ1| UniqueZ2| UniqueZ3| QuantityZ1| QuantityZ2| QuantityZ3| RateMean| QuantityMean| UniqueMean|
## |:----------|:--------------|----:|-------:|-------:|-------:|-------:|-------:|---------:|---------:|---------:|-------:|-------:|--------:|--------:|--------:|----------:|----------:|----------:|--------:|------------:|----------:|
## |Acetate    |Illinois Basin |    1|  1.8140|  0.6703|       5|       9|      11|      6269|     11354|     41608|  2.9418|  0.8188|  -1.4000|  -1.0877|  -0.9316|    -1.0338|    -1.0047|    -0.8320|   1.2422|        19744|      8.333|
## |Acetate    |Illinois Basin |    2|  0.5988|  0.4485|      33|      35|      32|    258550|    289875|    181755|  0.6860|  0.4070|   0.7859|   0.9420|   0.7078|     0.4067|     0.5856|    -0.0318|   0.5236|       243393|     33.333|
## |Acetate    |Illinois Basin |    3|  0.0133|  0.0183|      42|      36|      36|    683143|    251808|    295654| -0.4008| -0.3915|   1.4885|   1.0201|   1.0201|     2.8310|     0.3682|     0.6186|   0.0158|       410202|     38.000|
## |Acetate    |Illinois Basin |    4|  0.0030|  0.0042|       2|      11|      12|      1546|     27209|    113529| -0.4199| -0.4177|  -1.6342|  -0.9316|  -0.8535|    -1.0607|    -0.9142|    -0.4213|   0.0036|        47428|      8.333|
## |Acetate    |Illinois Basin |    5|  0.0001| -0.0001|      28|      28|      24|    179871|    345036|    122603| -0.4254| -0.4256|   0.3955|   0.3955|   0.0833|    -0.0425|     0.9005|    -0.3695|   0.0000|       215837|     26.667|
## |Butyrate   |Illinois Basin |    1|  1.6953|  2.9446|       5|       9|      11|      6269|     11354|     41608|  2.7214|  5.0405|  -1.4000|  -1.0877|  -0.9316|    -1.0338|    -1.0047|    -0.8320|   2.3199|        19744|      8.333|
## |Butyrate   |Illinois Basin |    2| -0.0110|  0.2286|      33|      35|      32|    258550|    289875|    181755| -0.4459| -0.0012|   0.7859|   0.9420|   0.7078|     0.4067|     0.5856|    -0.0318|   0.1088|       243393|     33.333|
## |Butyrate   |Illinois Basin |    3|  0.1764|  0.0044|      42|      36|      36|    683143|    251808|    295654| -0.0981| -0.4173|   1.4885|   1.0201|   1.0201|     2.8310|     0.3682|     0.6186|   0.0904|       410202|     38.000|
## |Butyrate   |Illinois Basin |    4|  0.0053|  0.0016|       2|      11|      12|      1546|     27209|    113529| -0.4157| -0.4226|  -1.6342|  -0.9316|  -0.8535|    -1.0607|    -0.9142|    -0.4213|   0.0034|        47428|      8.333|
## |Butyrate   |Illinois Basin |    5|  0.0001|  0.0000|      28|      28|      24|    179871|    345036|    122603| -0.4254| -0.4255|   0.3955|   0.3955|   0.0833|    -0.0425|     0.9005|    -0.3695|   0.0000|       215837|     26.667|
## |Formate    |Illinois Basin |    1|  0.3768|  0.4703|       5|       9|      11|      6269|     11354|     41608|  0.2739|  0.4475|  -1.4000|  -1.0877|  -0.9316|    -1.0338|    -1.0047|    -0.8320|   0.4236|        19744|      8.333|
## |Formate    |Illinois Basin |    2|  0.3290|  0.3913|      33|      35|      32|    258550|    289875|    181755|  0.1852|  0.3009|   0.7859|   0.9420|   0.7078|     0.4067|     0.5856|    -0.0318|   0.3601|       243393|     33.333|
## |Formate    |Illinois Basin |    3|  0.0089|  0.0229|      42|      36|      36|    683143|    251808|    295654| -0.4090| -0.3830|   1.4885|   1.0201|   1.0201|     2.8310|     0.3682|     0.6186|   0.0159|       410202|     38.000|
## |Formate    |Illinois Basin |    4|  0.0121|  0.0099|       2|      11|      12|      1546|     27209|    113529| -0.4030| -0.4072|  -1.6342|  -0.9316|  -0.8535|    -1.0607|    -0.9142|    -0.4213|   0.0110|        47428|      8.333|
## |Formate    |Illinois Basin |    5|  0.0151|  0.0224|      28|      28|      24|    179871|    345036|    122603| -0.3975| -0.3839|   0.3955|   0.3955|   0.0833|    -0.0425|     0.9005|    -0.3695|   0.0187|       215837|     26.667|
## |Propionate |Illinois Basin |    1|  0.0809|  0.0768|       5|       9|      11|      6269|     11354|     41608| -0.2753| -0.2829|  -1.4000|  -1.0877|  -0.9316|    -1.0338|    -1.0047|    -0.8320|   0.0788|        19744|      8.333|
## |Propionate |Illinois Basin |    2|  0.0062|  0.0243|      33|      35|      32|    258550|    289875|    181755| -0.4140| -0.3804|   0.7859|   0.9420|   0.7078|     0.4067|     0.5856|    -0.0318|   0.0152|       243393|     33.333|
## |Propionate |Illinois Basin |    3|  0.0052|  0.0033|      42|      36|      36|    683143|    251808|    295654| -0.4158| -0.4194|   1.4885|   1.0201|   1.0201|     2.8310|     0.3682|     0.6186|   0.0042|       410202|     38.000|
## |Propionate |Illinois Basin |    4|  0.0099|  0.0009|       2|      11|      12|      1546|     27209|    113529| -0.4071| -0.4239|  -1.6342|  -0.9316|  -0.8535|    -1.0607|    -0.9142|    -0.4213|   0.0054|        47428|      8.333|
## |Propionate |Illinois Basin |    5|  0.0020|  0.0022|      28|      28|      24|    179871|    345036|    122603| -0.4218| -0.4214|   0.3955|   0.3955|   0.0833|    -0.0425|     0.9005|    -0.3695|   0.0021|       215837|     26.667|
## |Valerate   |Illinois Basin |    1|  0.4252|  0.5408|       5|       9|      11|      6269|     11354|     41608|  0.3638|  0.5784|  -1.4000|  -1.0877|  -0.9316|    -1.0338|    -1.0047|    -0.8320|   0.4830|        19744|      8.333|
## |Valerate   |Illinois Basin |    2| -0.0069|  0.0105|      33|      35|      32|    258550|    289875|    181755| -0.4383| -0.4060|   0.7859|   0.9420|   0.7078|     0.4067|     0.5856|    -0.0318|   0.0018|       243393|     33.333|
## |Valerate   |Illinois Basin |    3| -0.0011|  0.0040|      42|      36|      36|    683143|    251808|    295654| -0.4275| -0.4181|   1.4885|   1.0201|   1.0201|     2.8310|     0.3682|     0.6186|   0.0015|       410202|     38.000|
## |Valerate   |Illinois Basin |    4|  0.0002|  0.0016|       2|      11|      12|      1546|     27209|    113529| -0.4251| -0.4226|  -1.6342|  -0.9316|  -0.8535|    -1.0607|    -0.9142|    -0.4213|   0.0009|        47428|      8.333|
## |Valerate   |Illinois Basin |    5|  0.0009|  0.0000|      28|      28|      24|    179871|    345036|    122603| -0.4239| -0.4255|   0.3955|   0.3955|   0.0833|    -0.0425|     0.9005|    -0.3695|   0.0004|       215837|     26.667|
```

```r
############################
```

```r
# Save as a compress, binary R dataset.  It's no longer readable with a text editor, but it saves metadata (eg, factor information).
#saveRDS(dsWide, file=pathOutput, compress="xz")
write.csv(dsWide, file=pathOutput, row.names=F)
```

The R session information (including the OS info, R version and all
packages used):


```r
sessionInfo()
```

```
## R version 3.1.0 Patched (2014-05-24 r65737)
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
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] lavaan_0.5-16      OpenMx_1.4-3060    ggplot2_1.0.0     
## [4] RColorBrewer_1.0-5 plyr_1.8.1         knitr_1.6         
## 
## loaded via a namespace (and not attached):
##  [1] colorspace_1.2-4 digest_0.6.4     evaluate_0.5.5   formatR_0.10    
##  [5] grid_3.1.0       gtable_0.1.2     labeling_0.2     MASS_7.3-33     
##  [9] mnormt_1.4-7     munsell_0.4.2    pbivnorm_0.5-1   proto_0.3-10    
## [13] quadprog_1.5-5   Rcpp_0.11.2      reshape2_1.4     scales_0.2.4    
## [17] stats4_3.1.0     stringr_0.6.2    tools_3.1.0
```

```r
Sys.time()
```

```
## [1] "2014-06-17 13:22:36 CDT"
```

