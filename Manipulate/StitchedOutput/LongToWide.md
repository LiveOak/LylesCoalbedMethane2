



This report was automatically generated with the R package **knitr**
(version 1.6).


```r
# knitr::stitch_rmd(script="./Manipulate/LongToWide.R", output="./Manipulate/StitchedOutput/LongToWide.md")
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
############################
#+ LoadSources

############################
#+ LoadPackages
library(knitr)
library(plyr)
# library(dplyr)
# library(reshape)

############################
#+ DeclareGlobals
options(stringsAsFactors=FALSE) #By default, character/string variables will NOT be automatically converted to factors.

pathInput <- "./Data/Derived/AllBasinsLong.csv"
pathOutput <- "./Data/Derived/AllBasinsWide.csv"

# basinsToInclude <- c("Illinois Basin")

############################
#+ LoadData
dsLong <- read.csv(pathInput)
colnames(dsLong)
```

```
##  [1] "IncubationReplicate"                     
##  [2] "Site"                                    
##  [3] "Basin"                                   
##  [4] "Substrate"                               
##  [5] "Rate"                                    
##  [6] "UnamendedControlRate"                    
##  [7] "AdjustedRate"                            
##  [8] "MicroarraryReplicate"                    
##  [9] "UniqueMcrGenes"                          
## [10] "QuantityMcrGenes"                        
## [11] "TotalMethaneInMicroMoles"                
## [12] "TotalMethaneUnamendedControlInMicroMoles"
## [13] "TotalAdjusted"                           
## [14] "TotalAdjustedZ"                          
## [15] "AdjustedRateZ"                           
## [16] "UniqueMcrGenesZ"                         
## [17] "QuantityMcrGenesZ"
```

```r
############################
#+ TweakData
# dsLong <- dsLong[dsLong$Basin %in% basinsToInclude, ]

############################
#+ Widen, results='asis'

Widen <- function( d ) {  
  dsIncubation1 <- d[d$IncubationReplicate=="O", ]
  dsIncubation2 <- d[d$IncubationReplicate=="X", ]
  dsMicroarray1 <- d[d$MicroarraryReplicate==1, ]
  dsMicroarray2 <- d[d$MicroarraryReplicate==2, ]
  dsMicroarray3 <- d[d$MicroarraryReplicate==3, ]
  
  data.frame(
    TotalAdjusted1 = dsIncubation1$TotalAdjusted[1],
    TotalAdjusted2 = dsIncubation2$TotalAdjusted[1],
    Rate1 = dsIncubation1$AdjustedRate[1],
    Rate2 = dsIncubation2$AdjustedRate[1],
    Unique1 = dsMicroarray1$UniqueMcrGenes[1],
    Unique2 = dsMicroarray2$UniqueMcrGenes[1],
    Unique3 = dsMicroarray3$UniqueMcrGenes[1],
    Quantity1 = dsMicroarray1$QuantityMcrGenes[1],
    Quantity2 = dsMicroarray2$QuantityMcrGenes[1],
    Quantity3 = dsMicroarray3$QuantityMcrGenes[1],
    
    
    TotalAdjustedZ1 = dsIncubation1$TotalAdjustedZ[1],
    TotalAdjustedZ2 = dsIncubation2$TotalAdjustedZ[1],
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
dsWide <- plyr::ddply(dsLong, c("Substrate", "Site", "Basin"), Widen)

############################
#+ Average
# dsWide$RateMean <- (dsWide$Rate1 + dsWide$Rate2) / 2
# dsWide$QuantityMean <- (dsWide$Quantity1 + dsWide$Quantity2 + dsWide$Quantity3) / 3
# dsWide$UniqueMean <- (dsWide$Unique1 + dsWide$Unique2 + dsWide$Unique3) / 3

#This generalizes to the other basins:
dsWide$TotalAdjustedMean <- rowMeans(dsWide[, c("TotalAdjusted1", "TotalAdjusted2")])
dsWide$RateMean <- rowMeans(dsWide[, c("Rate1", "Rate2")])
dsWide$QuantityMean <- rowMeans(dsWide[, c("Quantity1", "Quantity2", "Quantity3")], na.rm=T)
dsWide$UniqueMean <- rowMeans(dsWide[, c("Unique1", "Unique2", "Unique3")], na.rm=T)

############################
#+ PrintTable
knitr::kable(dsWide, format="markdown")
```

```
## 
## 
## |Substrate  | Site|Basin                | TotalAdjusted1| TotalAdjusted2|   Rate1|   Rate2| Unique1| Unique2| Unique3| Quantity1| Quantity2| Quantity3| TotalAdjustedZ1| TotalAdjustedZ2|  RateZ1|  RateZ2| UniqueZ1| UniqueZ2| UniqueZ3| QuantityZ1| QuantityZ2| QuantityZ3| TotalAdjustedMean| RateMean| QuantityMean| UniqueMean|
## |:----------|----:|:--------------------|--------------:|--------------:|-------:|-------:|-------:|-------:|-------:|---------:|---------:|---------:|---------------:|---------------:|-------:|-------:|--------:|--------:|--------:|----------:|----------:|----------:|-----------------:|--------:|------------:|----------:|
## |Acetate    |    1|Illinois Basin       |        70.1890|        61.0579|  1.8140|  0.6703|       5|       9|      11|      6269|     11354|     41608|          1.0153|          0.8239|  3.5569|  1.0441|  -1.0733|  -0.7572|  -0.5992|    -0.9291|    -0.8957|    -0.6975|           65.6234|   1.2422|        19744|      8.333|
## |Acetate    |    2|Illinois Basin       |        52.5201|        39.9752|  0.5988|  0.4485|      33|      35|      32|    258550|    289875|    181755|          0.6450|          0.3822|  0.8870|  0.5568|   1.1392|   1.2972|   1.0601|     0.7239|     0.9292|     0.2207|           46.2476|   0.5236|       243393|     33.333|
## |Acetate    |    3|Illinois Basin       |         0.8078|         1.6655|  0.0133|  0.0183|      42|      36|      36|    683143|    251808|    295654|         -0.4386|         -0.4206| -0.3993| -0.3884|   1.8503|   1.3762|   1.3762|     3.5059|     0.6797|     0.9670|            1.2367|   0.0158|       410202|     38.000|
## |Acetate    |    4|Illinois Basin       |        -0.0370|         0.4260|  0.0030|  0.0042|       2|      11|      12|      1546|     27209|    113529|         -0.4563|         -0.4466| -0.4220| -0.4194|  -1.3104|  -0.5992|  -0.5202|    -0.9600|    -0.7919|    -0.2263|            0.1945|   0.0036|        47428|      8.333|
## |Acetate    |    5|Illinois Basin       |         0.0617|        -0.0331|  0.0001| -0.0001|      28|      28|      24|    179871|    345036|    122603|         -0.4542|         -0.4562| -0.4284| -0.4287|   0.7441|   0.7441|   0.4280|     0.2084|     1.2906|    -0.1668|            0.0143|   0.0000|       215837|     26.667|
## |Acetate    |    6|Cook Inlet gas field |        69.9661|        76.1629|  0.5903|  0.7562|       8|      NA|      NA|     45849|        NA|        NA|          1.0106|          1.1405|  0.8684|  1.2328|  -0.8363|       NA|       NA|    -0.6697|         NA|         NA|           73.0645|   0.6732|        45849|      8.000|
## |Acetate    |    7|Cook Inlet gas field |        95.0619|        70.1751|  0.5322|  0.1558|      NA|      NA|      NA|        NA|        NA|        NA|          1.5365|          1.0150|  0.7407| -0.0863|       NA|       NA|       NA|         NA|         NA|         NA|           82.6185|   0.3440|          NaN|        NaN|
## |Acetate    |    8|Cook Inlet gas field |        20.6180|       108.9457| -0.0020|  0.4583|      10|      NA|      NA|    149111|        NA|        NA|         -0.0235|          1.8274| -0.4330|  0.5783|  -0.6782|       NA|       NA|     0.0069|         NA|         NA|           64.7819|   0.2282|       149111|     10.000|
## |Acetate    |    9|Cook Inlet gas field |       -25.7046|         1.4274| -0.1582|  0.0010|       3|      NA|      NA|      7384|        NA|        NA|         -0.9941|         -0.4256| -0.7761| -0.4264|  -1.2314|       NA|       NA|    -0.9218|         NA|         NA|          -12.1386|  -0.0786|         7384|      3.000|
## |Acetate    |   10|Cook Inlet gas field |         0.6773|         2.2000|  0.1620|  0.1368|       1|      NA|      NA|      7639|        NA|        NA|         -0.4413|         -0.4094| -0.0726| -0.1280|  -1.3894|       NA|       NA|    -0.9201|         NA|         NA|            1.4387|   0.1494|         7639|      1.000|
## |Acetate    |   11|Cook Inlet gas field |         2.5136|         1.4242|  0.2188|  0.1140|       3|      NA|      NA|     74840|        NA|        NA|         -0.4028|         -0.4257|  0.0521| -0.1781|  -1.2314|       NA|       NA|    -0.4798|         NA|         NA|            1.9689|   0.1664|        74840|      3.000|
## |Acetate    |   12|Powder River Basin   |         0.8490|        -1.2111|  0.7317| -0.1895|      26|      NA|      NA|    184191|        NA|        NA|         -0.4377|         -0.4809|  1.1790| -0.8449|   0.5860|       NA|       NA|     0.2367|         NA|         NA|           -0.1810|   0.2711|       184191|     26.000|
## |Acetate    |   13|Powder River Basin   |         4.2715|       -19.2543|  0.0742|  0.0962|      21|      NA|      NA|    139691|        NA|        NA|         -0.3660|         -0.8590| -0.2655| -0.2172|   0.1910|       NA|       NA|    -0.0549|         NA|         NA|           -7.4914|   0.0852|       139691|     21.000|
## |Acetate    |   14|Powder River Basin   |        50.9257|         0.8260|  0.1563| -0.0894|      19|      NA|      NA|    126166|        NA|        NA|          0.6116|         -0.4382| -0.0852| -0.6250|   0.0329|       NA|       NA|    -0.1435|         NA|         NA|           25.8759|   0.0334|       126166|     19.000|
## |Acetate    |   15|Powder River Basin   |        -0.2234|         0.6642|  0.0867|  0.0303|      11|      NA|      NA|      8865|        NA|        NA|         -0.4602|         -0.4416| -0.2381| -0.3620|  -0.5992|       NA|       NA|    -0.9121|         NA|         NA|            0.2204|   0.0585|         8865|     11.000|
## |Acetate    |   16|Powder River Basin   |        -3.3397|        -2.1934|  0.0001|  0.0017|      NA|      NA|      NA|        NA|        NA|        NA|         -0.5255|         -0.5015| -0.4283| -0.4248|       NA|       NA|       NA|         NA|         NA|         NA|           -2.7665|   0.0009|          NaN|        NaN|
## |Acetate    |   17|Powder River Basin   |         0.6503|         0.2522| -0.0394| -0.0680|      NA|      NA|      NA|        NA|        NA|        NA|         -0.4419|         -0.4502| -0.5151| -0.5780|       NA|       NA|       NA|         NA|         NA|         NA|            0.4512|  -0.0537|          NaN|        NaN|
## |Butyrate   |    1|Illinois Basin       |       129.6273|       149.7161|  1.6953|  2.9446|       5|       9|      11|      6269|     11354|     41608|          2.2608|          2.6817|  3.2961|  6.0409|  -1.0733|  -0.7572|  -0.5992|    -0.9291|    -0.8957|    -0.6975|          139.6717|   2.3199|        19744|      8.333|
## |Butyrate   |    2|Illinois Basin       |         0.0867|         2.0702| -0.0110|  0.2286|      33|      35|      32|    258550|    289875|    181755|         -0.4537|         -0.4121| -0.4527|  0.0737|   1.1392|   1.2972|   1.0601|     0.7239|     0.9292|     0.2207|            1.0784|   0.1088|       243393|     33.333|
## |Butyrate   |    3|Illinois Basin       |         0.7940|         1.5715|  0.1764|  0.0044|      42|      36|      36|    683143|    251808|    295654|         -0.4389|         -0.4226| -0.0410| -0.4189|   1.8503|   1.3762|   1.3762|     3.5059|     0.6797|     0.9670|            1.1828|   0.0904|       410202|     38.000|
## |Butyrate   |    4|Illinois Basin       |        -0.1429|         0.6348|  0.0053|  0.0016|       2|      11|      12|      1546|     27209|    113529|         -0.4585|         -0.4422| -0.4169| -0.4251|  -1.3104|  -0.5992|  -0.5202|    -0.9600|    -0.7919|    -0.2263|            0.2459|   0.0034|        47428|      8.333|
## |Butyrate   |    5|Illinois Basin       |         0.4314|         0.1705|  0.0001|  0.0000|      28|      28|      24|    179871|    345036|    122603|         -0.4465|         -0.4519| -0.4284| -0.4286|   0.7441|   0.7441|   0.4280|     0.2084|     1.2906|    -0.1668|            0.3009|   0.0000|       215837|     26.667|
## |Butyrate   |    6|Cook Inlet gas field |       162.7133|       144.2344|  0.8555|  0.7140|       8|      NA|      NA|     45849|        NA|        NA|          2.9541|          2.5669|  1.4510|  1.1401|  -0.8363|       NA|       NA|    -0.6697|         NA|         NA|          153.4739|   0.7848|        45849|      8.000|
## |Butyrate   |    7|Cook Inlet gas field |       155.1670|       192.5134|  0.6771|  0.6990|      NA|      NA|      NA|        NA|        NA|        NA|          2.7959|          3.5785|  1.0591|  1.1072|       NA|       NA|       NA|         NA|         NA|         NA|          173.8402|   0.6881|          NaN|        NaN|
## |Butyrate   |    8|Cook Inlet gas field |       193.7845|       196.4597|  0.9130|  1.0483|      10|      NA|      NA|    149111|        NA|        NA|          3.6052|          3.6612|  1.5773|  1.8746|  -0.6782|       NA|       NA|     0.0069|         NA|         NA|          195.1221|   0.9806|       149111|     10.000|
## |Butyrate   |    9|Cook Inlet gas field |         4.0144|        99.8849|  0.0808|  0.2704|       3|      NA|      NA|      7384|        NA|        NA|         -0.3714|          1.6375| -0.2510|  0.1655|  -1.2314|       NA|       NA|    -0.9218|         NA|         NA|           51.9497|   0.1756|         7384|      3.000|
## |Butyrate   |   10|Cook Inlet gas field |         0.8784|        -0.0039|  0.0853|  0.0860|       1|      NA|      NA|      7639|        NA|        NA|         -0.4371|         -0.4556| -0.2412| -0.2396|  -1.3894|       NA|       NA|    -0.9201|         NA|         NA|            0.4372|   0.0856|         7639|      1.000|
## |Butyrate   |   11|Cook Inlet gas field |         5.2569|         2.8028|  0.1269|  0.0650|       3|      NA|      NA|     74840|        NA|        NA|         -0.3453|         -0.3968| -0.1498| -0.2858|  -1.2314|       NA|       NA|    -0.4798|         NA|         NA|            4.0298|   0.0960|        74840|      3.000|
## |Butyrate   |   12|Powder River Basin   |         1.3219|        -2.8565|  0.1106| -0.2378|      26|      NA|      NA|    184191|        NA|        NA|         -0.4278|         -0.5154| -0.1856| -0.9510|   0.5860|       NA|       NA|     0.2367|         NA|         NA|           -0.7673|  -0.0636|       184191|     26.000|
## |Butyrate   |   13|Powder River Basin   |         2.1215|       -28.5967| -0.0574| -0.1704|      21|      NA|      NA|    139691|        NA|        NA|         -0.4110|         -1.0547| -0.5547| -0.8029|   0.1910|       NA|       NA|    -0.0549|         NA|         NA|          -13.2376|  -0.1139|       139691|     21.000|
## |Butyrate   |   14|Powder River Basin   |        -0.1700|         0.9296| -0.1119|  0.1268|      19|      NA|      NA|    126166|        NA|        NA|         -0.4591|         -0.4360| -0.6744| -0.1500|   0.0329|       NA|       NA|    -0.1435|         NA|         NA|            0.3798|   0.0074|       126166|     19.000|
## |Butyrate   |   15|Powder River Basin   |        -0.7478|         1.9635| -0.1999| -0.0907|      11|      NA|      NA|      8865|        NA|        NA|         -0.4712|         -0.4143| -0.8678| -0.6278|  -0.5992|       NA|       NA|    -0.9121|         NA|         NA|            0.6079|  -0.1453|         8865|     11.000|
## |Butyrate   |   16|Powder River Basin   |        -9.7479|        -1.3968| -0.0026|  0.0069|      NA|      NA|      NA|        NA|        NA|        NA|         -0.6598|         -0.4848| -0.4343| -0.4134|       NA|       NA|       NA|         NA|         NA|         NA|           -5.5723|   0.0022|          NaN|        NaN|
## |Butyrate   |   17|Powder River Basin   |         1.5980|         0.1152| -0.0343| -0.0575|      NA|      NA|      NA|        NA|        NA|        NA|         -0.4220|         -0.4531| -0.5039| -0.5549|       NA|       NA|       NA|         NA|         NA|         NA|            0.8566|  -0.0459|          NaN|        NaN|
## |Formate    |    1|Illinois Basin       |        25.7586|        37.2298|  0.3768|  0.4703|       5|       9|      11|      6269|     11354|     41608|          0.0843|          0.3246|  0.3993|  0.6047|  -1.0733|  -0.7572|  -0.5992|    -0.9291|    -0.8957|    -0.6975|           31.4942|   0.4236|        19744|      8.333|
## |Formate    |    2|Illinois Basin       |        22.1011|        37.7395|  0.3290|  0.3913|      33|      35|      32|    258550|    289875|    181755|          0.0076|          0.3353|  0.2943|  0.4311|   1.1392|   1.2972|   1.0601|     0.7239|     0.9292|     0.2207|           29.9203|   0.3601|       243393|     33.333|
## |Formate    |    3|Illinois Basin       |         5.4158|         2.4626|  0.0089|  0.0229|      42|      36|      36|    683143|    251808|    295654|         -0.3420|         -0.4039| -0.4090| -0.3783|   1.8503|   1.3762|   1.3762|     3.5059|     0.6797|     0.9670|            3.9392|   0.0159|       410202|     38.000|
## |Formate    |    4|Illinois Basin       |         1.2223|         2.9119|  0.0121|  0.0099|       2|      11|      12|      1546|     27209|    113529|         -0.4299|         -0.3945| -0.4020| -0.4069|  -1.3104|  -0.5992|  -0.5202|    -0.9600|    -0.7919|    -0.2263|            2.0671|   0.0110|        47428|      8.333|
## |Formate    |    5|Illinois Basin       |         2.0774|         3.4311|  0.0151|  0.0224|      28|      28|      24|    179871|    345036|    122603|         -0.4120|         -0.3836| -0.3955| -0.3794|   0.7441|   0.7441|   0.4280|     0.2084|     1.2906|    -0.1668|            2.7543|   0.0187|       215837|     26.667|
## |Formate    |    6|Cook Inlet gas field |        21.4619|        27.8350|  0.2700|  0.4611|       8|      NA|      NA|     45849|        NA|        NA|         -0.0058|          0.1278|  0.1646|  0.5845|  -0.8363|       NA|       NA|    -0.6697|         NA|         NA|           24.6484|   0.3656|        45849|      8.000|
## |Formate    |    7|Cook Inlet gas field |        42.8987|        35.4459|  0.1702| -0.1051|      NA|      NA|      NA|        NA|        NA|        NA|          0.4434|          0.2873| -0.0546| -0.6595|       NA|       NA|       NA|         NA|         NA|         NA|           39.1723|   0.0325|          NaN|        NaN|
## |Formate    |    8|Cook Inlet gas field |         1.9605|        13.8149| -0.0537|  0.0288|      10|      NA|      NA|    149111|        NA|        NA|         -0.4144|         -0.1660| -0.5465| -0.3653|  -0.6782|       NA|       NA|     0.0069|         NA|         NA|            7.8877|  -0.0124|       149111|     10.000|
## |Formate    |    9|Cook Inlet gas field |       -24.3271|         0.6995| -0.1630| -0.0058|       3|      NA|      NA|      7384|        NA|        NA|         -0.9653|         -0.4408| -0.7867| -0.4413|  -1.2314|       NA|       NA|    -0.9218|         NA|         NA|          -11.8138|  -0.0844|         7384|      3.000|
## |Formate    |   10|Cook Inlet gas field |         1.3254|         1.3643| -0.3150| -0.2774|       1|      NA|      NA|      7639|        NA|        NA|         -0.4277|         -0.4269| -1.1206| -1.0380|  -1.3894|       NA|       NA|    -0.9201|         NA|         NA|            1.3448|  -0.2962|         7639|      1.000|
## |Formate    |   11|Cook Inlet gas field |         1.9186|        -3.2542|  0.1738|  0.0745|       3|      NA|      NA|     74840|        NA|        NA|         -0.4153|         -0.5237| -0.0467| -0.2649|  -1.2314|       NA|       NA|    -0.4798|         NA|         NA|           -0.6678|   0.1242|        74840|      3.000|
## |Formate    |   12|Powder River Basin   |        -0.0685|       -37.7289|  0.0194| -0.1955|      26|      NA|      NA|    184191|        NA|        NA|         -0.4569|         -1.2461| -0.3859| -0.8581|   0.5860|       NA|       NA|     0.2367|         NA|         NA|          -18.8987|  -0.0880|       184191|     26.000|
## |Formate    |   13|Powder River Basin   |        14.9440|       -16.1907|  0.0540|  0.0519|      21|      NA|      NA|    139691|        NA|        NA|         -0.1423|         -0.7948| -0.3099| -0.3145|   0.1910|       NA|       NA|    -0.0549|         NA|         NA|           -0.6233|   0.0530|       139691|     21.000|
## |Formate    |   14|Powder River Basin   |        -1.6098|        -1.9572| -0.0932| -0.0715|      19|      NA|      NA|    126166|        NA|        NA|         -0.4892|         -0.4965| -0.6333| -0.5857|   0.0329|       NA|       NA|    -0.1435|         NA|         NA|           -1.7835|  -0.0824|       126166|     19.000|
## |Formate    |   15|Powder River Basin   |         0.7079|         2.8761|  0.2641|  0.5154|      11|      NA|      NA|      8865|        NA|        NA|         -0.4407|         -0.3952|  0.1517|  0.7038|  -0.5992|       NA|       NA|    -0.9121|         NA|         NA|            1.7920|   0.3898|         8865|     11.000|
## |Formate    |   16|Powder River Basin   |         7.2721|        21.2900|  0.0014|  0.0105|      NA|      NA|      NA|        NA|        NA|        NA|         -0.3031|         -0.0094| -0.4255| -0.4055|       NA|       NA|       NA|         NA|         NA|         NA|           14.2811|   0.0060|          NaN|        NaN|
## |Formate    |   17|Powder River Basin   |        19.8531|        18.0170|  0.5876|  0.5327|      NA|      NA|      NA|        NA|        NA|        NA|         -0.0395|         -0.0780|  0.8624|  0.7418|       NA|       NA|       NA|         NA|         NA|         NA|           18.9351|   0.5601|          NaN|        NaN|
## |Propionate |    1|Illinois Basin       |        16.2225|        16.6922|  0.0809|  0.0768|       5|       9|      11|      6269|     11354|     41608|         -0.1156|         -0.1057| -0.2508| -0.2598|  -1.0733|  -0.7572|  -0.5992|    -0.9291|    -0.8957|    -0.6975|           16.4573|   0.0788|        19744|      8.333|
## |Propionate |    2|Illinois Basin       |         0.8048|         1.3417|  0.0062|  0.0243|      33|      35|      32|    258550|    289875|    181755|         -0.4386|         -0.4274| -0.4149| -0.3752|   1.1392|   1.2972|   1.0601|     0.7239|     0.9292|     0.2207|            1.0732|   0.0152|       243393|     33.333|
## |Propionate |    3|Illinois Basin       |         0.8122|         0.8916|  0.0052|  0.0033|      42|      36|      36|    683143|    251808|    295654|         -0.4385|         -0.4368| -0.4171| -0.4213|   1.8503|   1.3762|   1.3762|     3.5059|     0.6797|     0.9670|            0.8519|   0.0042|       410202|     38.000|
## |Propionate |    4|Illinois Basin       |         3.3597|         0.3365|  0.0099|  0.0009|       2|      11|      12|      1546|     27209|    113529|         -0.3851|         -0.4484| -0.4068| -0.4266|  -1.3104|  -0.5992|  -0.5202|    -0.9600|    -0.7919|    -0.2263|            1.8481|   0.0054|        47428|      8.333|
## |Propionate |    5|Illinois Basin       |         0.1016|         0.0291|  0.0020|  0.0022|      28|      28|      24|    179871|    345036|    122603|         -0.4534|         -0.4549| -0.4242| -0.4237|   0.7441|   0.7441|   0.4280|     0.2084|     1.2906|    -0.1668|            0.0654|   0.0021|       215837|     26.667|
## |Propionate |    6|Cook Inlet gas field |       115.1481|       141.5256|  0.5998|  0.5948|       8|      NA|      NA|     45849|        NA|        NA|          1.9574|          2.5101|  0.8892|  0.8782|  -0.8363|       NA|       NA|    -0.6697|         NA|         NA|          128.3369|   0.5973|        45849|      8.000|
## |Propionate |    7|Cook Inlet gas field |        28.8632|         2.4374|  0.4155| -0.0958|      NA|      NA|      NA|        NA|        NA|        NA|          0.1493|         -0.4044|  0.4843| -0.6390|       NA|       NA|       NA|         NA|         NA|         NA|           15.6503|   0.1598|          NaN|        NaN|
## |Propionate |    8|Cook Inlet gas field |         2.1226|         0.9188| -0.0128|  0.0323|      10|      NA|      NA|    149111|        NA|        NA|         -0.4110|         -0.4362| -0.4567| -0.3576|  -0.6782|       NA|       NA|     0.0069|         NA|         NA|            1.5207|   0.0098|       149111|     10.000|
## |Propionate |    9|Cook Inlet gas field |       -25.2084|        -1.5379| -0.1621| -0.0032|       3|      NA|      NA|      7384|        NA|        NA|         -0.9837|         -0.4877| -0.7847| -0.4356|  -1.2314|       NA|       NA|    -0.9218|         NA|         NA|          -13.3731|  -0.0826|         7384|      3.000|
## |Propionate |   10|Cook Inlet gas field |         0.3978|         1.3056|  0.0489|  0.2097|       1|      NA|      NA|      7639|        NA|        NA|         -0.4472|         -0.4281| -0.3211|  0.0322|  -1.3894|       NA|       NA|    -0.9201|         NA|         NA|            0.8517|   0.1293|         7639|      1.000|
## |Propionate |   11|Cook Inlet gas field |         4.3403|         0.4802|  0.2214|  0.1794|       3|      NA|      NA|     74840|        NA|        NA|         -0.3645|         -0.4454|  0.0579| -0.0344|  -1.2314|       NA|       NA|    -0.4798|         NA|         NA|            2.4102|   0.2004|        74840|      3.000|
## |Propionate |   12|Powder River Basin   |        20.2298|        11.8199|  0.1598| -0.0548|      26|      NA|      NA|    184191|        NA|        NA|         -0.0316|         -0.2078| -0.0775| -0.5490|   0.5860|       NA|       NA|     0.2367|         NA|         NA|           16.0249|   0.0525|       184191|     26.000|
## |Propionate |   13|Powder River Basin   |       114.4180|       133.7817|  0.5601|  0.9087|      21|      NA|      NA|    139691|        NA|        NA|          1.9421|          2.3478|  0.8020|  1.5679|   0.1910|       NA|       NA|    -0.0549|         NA|         NA|          124.0999|   0.7344|       139691|     21.000|
## |Propionate |   14|Powder River Basin   |         0.2102|         1.0688| -0.0416|  0.0167|      19|      NA|      NA|    126166|        NA|        NA|         -0.4511|         -0.4331| -0.5200| -0.3919|   0.0329|       NA|       NA|    -0.1435|         NA|         NA|            0.6395|  -0.0124|       126166|     19.000|
## |Propionate |   15|Powder River Basin   |         0.3178|         0.9294| -0.1072| -0.0506|      11|      NA|      NA|      8865|        NA|        NA|         -0.4488|         -0.4360| -0.6641| -0.5397|  -0.5992|       NA|       NA|    -0.9121|         NA|         NA|            0.6236|  -0.0789|         8865|     11.000|
## |Propionate |   16|Powder River Basin   |        -1.9315|        -1.2977|  0.0142| -0.0026|      NA|      NA|      NA|        NA|        NA|        NA|         -0.4960|         -0.4827| -0.3974| -0.4343|       NA|       NA|       NA|         NA|         NA|         NA|           -1.6146|   0.0058|          NaN|        NaN|
## |Propionate |   17|Powder River Basin   |         1.8734|         1.0859| -0.0253| -0.0627|      NA|      NA|      NA|        NA|        NA|        NA|         -0.4162|         -0.4327| -0.4842| -0.5663|       NA|       NA|       NA|         NA|         NA|         NA|            1.4796|  -0.0440|          NaN|        NaN|
## |Valerate   |    1|Illinois Basin       |         2.7736|       107.4474|  0.4252|  0.5408|       5|       9|      11|      6269|     11354|     41608|         -0.3974|          1.7960|  0.5056|  0.7596|  -1.0733|  -0.7572|  -0.5992|    -0.9291|    -0.8957|    -0.6975|           55.1105|   0.4830|        19744|      8.333|
## |Valerate   |    2|Illinois Basin       |         1.4692|         0.3414| -0.0069|  0.0105|      33|      35|      32|    258550|    289875|    181755|         -0.4247|         -0.4483| -0.4437| -0.4055|   1.1392|   1.2972|   1.0601|     0.7239|     0.9292|     0.2207|            0.9053|   0.0018|       243393|     33.333|
## |Valerate   |    3|Illinois Basin       |        -0.4760|         0.7238| -0.0011|  0.0040|      42|      36|      36|    683143|    251808|    295654|         -0.4655|         -0.4403| -0.4309| -0.4198|   1.8503|   1.3762|   1.3762|     3.5059|     0.6797|     0.9670|            0.1239|   0.0015|       410202|     38.000|
## |Valerate   |    4|Illinois Basin       |         0.2936|         0.4841|  0.0002|  0.0016|       2|      11|      12|      1546|     27209|    113529|         -0.4493|         -0.4453| -0.4281| -0.4251|  -1.3104|  -0.5992|  -0.5202|    -0.9600|    -0.7919|    -0.2263|            0.3889|   0.0009|        47428|      8.333|
## |Valerate   |    5|Illinois Basin       |         0.3247|         0.0077|  0.0009|  0.0000|      28|      28|      24|    179871|    345036|    122603|         -0.4487|         -0.4553| -0.4267| -0.4286|   0.7441|   0.7441|   0.4280|     0.2084|     1.2906|    -0.1668|            0.1662|   0.0004|       215837|     26.667|
## |Valerate   |    6|Cook Inlet gas field |       237.3463|       304.4931|  0.9296|  1.2078|       8|      NA|      NA|     45849|        NA|        NA|          4.5180|          5.9250|  1.6138|  2.2250|  -0.8363|       NA|       NA|    -0.6697|         NA|         NA|          270.9197|   1.0687|        45849|      8.000|
## |Valerate   |    7|Cook Inlet gas field |       116.9242|       103.1789|  0.6874|  0.4219|      NA|      NA|      NA|        NA|        NA|        NA|          1.9946|          1.7066|  1.0817|  0.4984|       NA|       NA|       NA|         NA|         NA|         NA|          110.0515|   0.5546|          NaN|        NaN|
## |Valerate   |    8|Cook Inlet gas field |       144.8385|       142.2629|  0.5684|  0.7229|      10|      NA|      NA|    149111|        NA|        NA|          2.5795|          2.5256|  0.8202|  1.1597|  -0.6782|       NA|       NA|     0.0069|         NA|         NA|          143.5507|   0.6457|       149111|     10.000|
## |Valerate   |    9|Cook Inlet gas field |        80.4129|        39.4625|  0.3017|  0.1852|       3|      NA|      NA|      7384|        NA|        NA|          1.2295|          0.3714|  0.2343| -0.0217|  -1.2314|       NA|       NA|    -0.9218|         NA|         NA|           59.9377|   0.2434|         7384|      3.000|
## |Valerate   |   10|Cook Inlet gas field |         1.0437|        -0.6695|  0.0829|  0.0182|       1|      NA|      NA|      7639|        NA|        NA|         -0.4336|         -0.4695| -0.2464| -0.3886|  -1.3894|       NA|       NA|    -0.9201|         NA|         NA|            0.1871|   0.0506|         7639|      1.000|
## |Valerate   |   11|Cook Inlet gas field |         0.5999|         0.8214| -0.5413| -0.0183|       3|      NA|      NA|     74840|        NA|        NA|         -0.4429|         -0.4383| -1.6178| -0.4688|  -1.2314|       NA|       NA|    -0.4798|         NA|         NA|            0.7106|  -0.2798|        74840|      3.000|
## |Valerate   |   12|Powder River Basin   |         1.4118|        -8.8193|  0.2698| -0.2432|      26|      NA|      NA|    184191|        NA|        NA|         -0.4259|         -0.6403|  0.1642| -0.9629|   0.5860|       NA|       NA|     0.2367|         NA|         NA|           -3.7038|   0.0133|       184191|     26.000|
## |Valerate   |   13|Powder River Basin   |        32.8544|       -21.8484| -0.0344|  0.1433|      21|      NA|      NA|    139691|        NA|        NA|          0.2330|         -0.9133| -0.5041| -0.1137|   0.1910|       NA|       NA|    -0.0549|         NA|         NA|            5.5030|   0.0544|       139691|     21.000|
## |Valerate   |   14|Powder River Basin   |        -0.0974|        -0.3722| -0.0789| -0.0478|      19|      NA|      NA|    126166|        NA|        NA|         -0.4575|         -0.4633| -0.6019| -0.5336|   0.0329|       NA|       NA|    -0.1435|         NA|         NA|           -0.2348|  -0.0634|       126166|     19.000|
## |Valerate   |   15|Powder River Basin   |        -0.6026|         2.0760| -0.0955| -0.1020|      11|      NA|      NA|      8865|        NA|        NA|         -0.4681|         -0.4120| -0.6384| -0.6527|  -0.5992|       NA|       NA|    -0.9121|         NA|         NA|            0.7367|  -0.0988|         8865|     11.000|
## |Valerate   |   16|Powder River Basin   |        -5.5836|        -0.1125| -0.0006|  0.0087|      NA|      NA|      NA|        NA|        NA|        NA|         -0.5725|         -0.4579| -0.4299| -0.4095|       NA|       NA|       NA|         NA|         NA|         NA|           -2.8481|   0.0040|          NaN|        NaN|
## |Valerate   |   17|Powder River Basin   |         1.4988|         0.4110| -0.0012| -0.0376|      NA|      NA|      NA|        NA|        NA|        NA|         -0.4241|         -0.4469| -0.4312| -0.5112|       NA|       NA|       NA|         NA|         NA|         NA|            0.9549|  -0.0194|          NaN|        NaN|
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
## R version 3.1.1 Patched (2014-09-17 r66626)
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
## [1] grid      stats     graphics  grDevices utils     datasets  methods  
## [8] base     
## 
## other attached packages:
## [1] ggplot2_1.0.0      RColorBrewer_1.0-5 scales_0.2.4      
## [4] plyr_1.8.1         knitr_1.6         
## 
## loaded via a namespace (and not attached):
##  [1] colorspace_1.2-4 digest_0.6.4     evaluate_0.5.5   formatR_1.0     
##  [5] gtable_0.1.2     htmltools_0.2.6  labeling_0.3     markdown_0.7.4  
##  [9] MASS_7.3-34      munsell_0.4.2    proto_0.3-10     Rcpp_0.11.2     
## [13] reshape2_1.4     rmarkdown_0.3.3  stringr_0.6.2    tools_3.1.1
```

```r
Sys.time()
```

```
## [1] "2014-09-21 13:35:04 CDT"
```

