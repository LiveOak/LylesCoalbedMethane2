



This report was automatically generated with the R package **knitr**
(version 1.6).


```r
# knitr::stitch_rmd(script="./Manipulate/GroomLong.R", output="./Manipulate/StitchedOutput/GroomLong.md")
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
pathOutput <- "./Data/Derived/AllBasinsLong.csv"

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
```

```
## Error: object 'basinsToInclude' not found
```

```r
dsLong$AdjustedRateZ <- scale(dsLong$AdjustedRate)
dsLong$UniqueMcrGenesZ <- scale(dsLong$UniqueMcrGenes)
dsLong$QuantityMcrGenesZ <- scale(dsLong$QuantityMcrGenes)


############################
#+ PrintTable
knitr::kable(dsLong, format="markdown")
```

```
## 
## 
## |IncubationReplicate | Site|Basin                |Substrate  |   Rate| UnamendedControlRate| AdjustedRate| MicroarraryReplicate| UniqueMcrGenes| QuantityMcrGenes| AdjustedRateZ| UniqueMcrGenesZ| QuantityMcrGenesZ|
## |:-------------------|----:|:--------------------|:----------|------:|--------------------:|------------:|--------------------:|--------------:|----------------:|-------------:|---------------:|-----------------:|
## |O                   |    1|Illinois Basin       |Formate    | 0.3783|               0.0015|       0.3768|                    1|              5|             6269|        0.3993|         -1.0733|           -0.9291|
## |X                   |    1|Illinois Basin       |Formate    | 0.4754|               0.0051|       0.4703|                    1|              5|             6269|        0.6047|         -1.0733|           -0.9291|
## |O                   |    1|Illinois Basin       |Acetate    | 1.8155|               0.0015|       1.8140|                    1|              5|             6269|        3.5569|         -1.0733|           -0.9291|
## |X                   |    1|Illinois Basin       |Acetate    | 0.6754|               0.0051|       0.6703|                    1|              5|             6269|        1.0441|         -1.0733|           -0.9291|
## |O                   |    1|Illinois Basin       |Propionate | 0.0824|               0.0015|       0.0809|                    1|              5|             6269|       -0.2508|         -1.0733|           -0.9291|
## |X                   |    1|Illinois Basin       |Propionate | 0.0819|               0.0051|       0.0768|                    1|              5|             6269|       -0.2598|         -1.0733|           -0.9291|
## |O                   |    1|Illinois Basin       |Butyrate   | 1.6968|               0.0015|       1.6953|                    1|              5|             6269|        3.2961|         -1.0733|           -0.9291|
## |X                   |    1|Illinois Basin       |Butyrate   | 2.9497|               0.0051|       2.9446|                    1|              5|             6269|        6.0409|         -1.0733|           -0.9291|
## |O                   |    1|Illinois Basin       |Valerate   | 0.4267|               0.0015|       0.4252|                    1|              5|             6269|        0.5056|         -1.0733|           -0.9291|
## |X                   |    1|Illinois Basin       |Valerate   | 0.5459|               0.0051|       0.5408|                    1|              5|             6269|        0.7596|         -1.0733|           -0.9291|
## |O                   |    2|Illinois Basin       |Formate    | 0.3467|               0.0177|       0.3290|                    1|             33|           258550|        0.2943|          1.1392|            0.7239|
## |X                   |    2|Illinois Basin       |Formate    | 0.4047|               0.0134|       0.3913|                    1|             33|           258550|        0.4311|          1.1392|            0.7239|
## |O                   |    2|Illinois Basin       |Acetate    | 0.6165|               0.0177|       0.5988|                    1|             33|           258550|        0.8870|          1.1392|            0.7239|
## |X                   |    2|Illinois Basin       |Acetate    | 0.4619|               0.0134|       0.4485|                    1|             33|           258550|        0.5568|          1.1392|            0.7239|
## |O                   |    2|Illinois Basin       |Propionate | 0.0239|               0.0177|       0.0062|                    1|             33|           258550|       -0.4149|          1.1392|            0.7239|
## |X                   |    2|Illinois Basin       |Propionate | 0.0377|               0.0134|       0.0243|                    1|             33|           258550|       -0.3752|          1.1392|            0.7239|
## |O                   |    2|Illinois Basin       |Butyrate   | 0.0067|               0.0177|      -0.0110|                    1|             33|           258550|       -0.4527|          1.1392|            0.7239|
## |X                   |    2|Illinois Basin       |Butyrate   | 0.2420|               0.0134|       0.2286|                    1|             33|           258550|        0.0737|          1.1392|            0.7239|
## |O                   |    2|Illinois Basin       |Valerate   | 0.0108|               0.0177|      -0.0069|                    1|             33|           258550|       -0.4437|          1.1392|            0.7239|
## |X                   |    2|Illinois Basin       |Valerate   | 0.0239|               0.0134|       0.0105|                    1|             33|           258550|       -0.4055|          1.1392|            0.7239|
## |O                   |    3|Illinois Basin       |Formate    | 0.0100|               0.0011|       0.0089|                    1|             42|           683143|       -0.4090|          1.8503|            3.5059|
## |X                   |    3|Illinois Basin       |Formate    | 0.0235|               0.0006|       0.0229|                    1|             42|           683143|       -0.3783|          1.8503|            3.5059|
## |O                   |    3|Illinois Basin       |Acetate    | 0.0144|               0.0011|       0.0133|                    1|             42|           683143|       -0.3993|          1.8503|            3.5059|
## |X                   |    3|Illinois Basin       |Acetate    | 0.0189|               0.0006|       0.0183|                    1|             42|           683143|       -0.3884|          1.8503|            3.5059|
## |O                   |    3|Illinois Basin       |Propionate | 0.0063|               0.0011|       0.0052|                    1|             42|           683143|       -0.4171|          1.8503|            3.5059|
## |X                   |    3|Illinois Basin       |Propionate | 0.0039|               0.0006|       0.0033|                    1|             42|           683143|       -0.4213|          1.8503|            3.5059|
## |O                   |    3|Illinois Basin       |Butyrate   | 0.1775|               0.0011|       0.1764|                    1|             42|           683143|       -0.0410|          1.8503|            3.5059|
## |X                   |    3|Illinois Basin       |Butyrate   | 0.0050|               0.0006|       0.0044|                    1|             42|           683143|       -0.4189|          1.8503|            3.5059|
## |O                   |    3|Illinois Basin       |Valerate   | 0.0000|               0.0011|      -0.0011|                    1|             42|           683143|       -0.4309|          1.8503|            3.5059|
## |X                   |    3|Illinois Basin       |Valerate   | 0.0046|               0.0006|       0.0040|                    1|             42|           683143|       -0.4198|          1.8503|            3.5059|
## |O                   |    4|Illinois Basin       |Formate    | 0.0138|               0.0017|       0.0121|                    1|              2|             1546|       -0.4020|         -1.3104|           -0.9600|
## |X                   |    4|Illinois Basin       |Formate    | 0.0099|               0.0000|       0.0099|                    1|              2|             1546|       -0.4069|         -1.3104|           -0.9600|
## |O                   |    4|Illinois Basin       |Acetate    | 0.0047|               0.0017|       0.0030|                    1|              2|             1546|       -0.4220|         -1.3104|           -0.9600|
## |X                   |    4|Illinois Basin       |Acetate    | 0.0042|               0.0000|       0.0042|                    1|              2|             1546|       -0.4194|         -1.3104|           -0.9600|
## |O                   |    4|Illinois Basin       |Propionate | 0.0116|               0.0017|       0.0099|                    1|              2|             1546|       -0.4068|         -1.3104|           -0.9600|
## |X                   |    4|Illinois Basin       |Propionate | 0.0009|               0.0000|       0.0009|                    1|              2|             1546|       -0.4266|         -1.3104|           -0.9600|
## |O                   |    4|Illinois Basin       |Butyrate   | 0.0070|               0.0017|       0.0053|                    1|              2|             1546|       -0.4169|         -1.3104|           -0.9600|
## |X                   |    4|Illinois Basin       |Butyrate   | 0.0016|               0.0000|       0.0016|                    1|              2|             1546|       -0.4251|         -1.3104|           -0.9600|
## |O                   |    4|Illinois Basin       |Valerate   | 0.0019|               0.0017|       0.0002|                    1|              2|             1546|       -0.4281|         -1.3104|           -0.9600|
## |X                   |    4|Illinois Basin       |Valerate   | 0.0016|               0.0000|       0.0016|                    1|              2|             1546|       -0.4251|         -1.3104|           -0.9600|
## |O                   |    5|Illinois Basin       |Formate    | 0.0151|               0.0000|       0.0151|                    1|             28|           179871|       -0.3955|          0.7441|            0.2084|
## |X                   |    5|Illinois Basin       |Formate    | 0.0225|               0.0001|       0.0224|                    1|             28|           179871|       -0.3794|          0.7441|            0.2084|
## |O                   |    5|Illinois Basin       |Acetate    | 0.0001|               0.0000|       0.0001|                    1|             28|           179871|       -0.4284|          0.7441|            0.2084|
## |X                   |    5|Illinois Basin       |Acetate    | 0.0000|               0.0001|      -0.0001|                    1|             28|           179871|       -0.4287|          0.7441|            0.2084|
## |O                   |    5|Illinois Basin       |Propionate | 0.0020|               0.0000|       0.0020|                    1|             28|           179871|       -0.4242|          0.7441|            0.2084|
## |X                   |    5|Illinois Basin       |Propionate | 0.0023|               0.0001|       0.0022|                    1|             28|           179871|       -0.4237|          0.7441|            0.2084|
## |O                   |    5|Illinois Basin       |Butyrate   | 0.0001|               0.0000|       0.0001|                    1|             28|           179871|       -0.4284|          0.7441|            0.2084|
## |X                   |    5|Illinois Basin       |Butyrate   | 0.0001|               0.0001|       0.0000|                    1|             28|           179871|       -0.4286|          0.7441|            0.2084|
## |O                   |    5|Illinois Basin       |Valerate   | 0.0009|               0.0000|       0.0009|                    1|             28|           179871|       -0.4267|          0.7441|            0.2084|
## |X                   |    5|Illinois Basin       |Valerate   | 0.0001|               0.0001|       0.0000|                    1|             28|           179871|       -0.4286|          0.7441|            0.2084|
## |O                   |    1|Illinois Basin       |Formate    | 0.3783|               0.0015|       0.3768|                    2|              9|            11354|        0.3993|         -0.7572|           -0.8957|
## |X                   |    1|Illinois Basin       |Formate    | 0.4754|               0.0051|       0.4703|                    2|              9|            11354|        0.6047|         -0.7572|           -0.8957|
## |O                   |    1|Illinois Basin       |Acetate    | 1.8155|               0.0015|       1.8140|                    2|              9|            11354|        3.5569|         -0.7572|           -0.8957|
## |X                   |    1|Illinois Basin       |Acetate    | 0.6754|               0.0051|       0.6703|                    2|              9|            11354|        1.0441|         -0.7572|           -0.8957|
## |O                   |    1|Illinois Basin       |Propionate | 0.0824|               0.0015|       0.0809|                    2|              9|            11354|       -0.2508|         -0.7572|           -0.8957|
## |X                   |    1|Illinois Basin       |Propionate | 0.0819|               0.0051|       0.0768|                    2|              9|            11354|       -0.2598|         -0.7572|           -0.8957|
## |O                   |    1|Illinois Basin       |Butyrate   | 1.6968|               0.0015|       1.6953|                    2|              9|            11354|        3.2961|         -0.7572|           -0.8957|
## |X                   |    1|Illinois Basin       |Butyrate   | 2.9497|               0.0051|       2.9446|                    2|              9|            11354|        6.0409|         -0.7572|           -0.8957|
## |O                   |    1|Illinois Basin       |Valerate   | 0.4267|               0.0015|       0.4252|                    2|              9|            11354|        0.5056|         -0.7572|           -0.8957|
## |X                   |    1|Illinois Basin       |Valerate   | 0.5459|               0.0051|       0.5408|                    2|              9|            11354|        0.7596|         -0.7572|           -0.8957|
## |O                   |    2|Illinois Basin       |Formate    | 0.3467|               0.0177|       0.3290|                    2|             35|           289875|        0.2943|          1.2972|            0.9292|
## |X                   |    2|Illinois Basin       |Formate    | 0.4047|               0.0134|       0.3913|                    2|             35|           289875|        0.4311|          1.2972|            0.9292|
## |O                   |    2|Illinois Basin       |Acetate    | 0.6165|               0.0177|       0.5988|                    2|             35|           289875|        0.8870|          1.2972|            0.9292|
## |X                   |    2|Illinois Basin       |Acetate    | 0.4619|               0.0134|       0.4485|                    2|             35|           289875|        0.5568|          1.2972|            0.9292|
## |O                   |    2|Illinois Basin       |Propionate | 0.0239|               0.0177|       0.0062|                    2|             35|           289875|       -0.4149|          1.2972|            0.9292|
## |X                   |    2|Illinois Basin       |Propionate | 0.0377|               0.0134|       0.0243|                    2|             35|           289875|       -0.3752|          1.2972|            0.9292|
## |O                   |    2|Illinois Basin       |Butyrate   | 0.0067|               0.0177|      -0.0110|                    2|             35|           289875|       -0.4527|          1.2972|            0.9292|
## |X                   |    2|Illinois Basin       |Butyrate   | 0.2420|               0.0134|       0.2286|                    2|             35|           289875|        0.0737|          1.2972|            0.9292|
## |O                   |    2|Illinois Basin       |Valerate   | 0.0108|               0.0177|      -0.0069|                    2|             35|           289875|       -0.4437|          1.2972|            0.9292|
## |X                   |    2|Illinois Basin       |Valerate   | 0.0239|               0.0134|       0.0105|                    2|             35|           289875|       -0.4055|          1.2972|            0.9292|
## |O                   |    3|Illinois Basin       |Formate    | 0.0100|               0.0011|       0.0089|                    2|             36|           251808|       -0.4090|          1.3762|            0.6797|
## |X                   |    3|Illinois Basin       |Formate    | 0.0235|               0.0006|       0.0229|                    2|             36|           251808|       -0.3783|          1.3762|            0.6797|
## |O                   |    3|Illinois Basin       |Acetate    | 0.0144|               0.0011|       0.0133|                    2|             36|           251808|       -0.3993|          1.3762|            0.6797|
## |X                   |    3|Illinois Basin       |Acetate    | 0.0189|               0.0006|       0.0183|                    2|             36|           251808|       -0.3884|          1.3762|            0.6797|
## |O                   |    3|Illinois Basin       |Propionate | 0.0063|               0.0011|       0.0052|                    2|             36|           251808|       -0.4171|          1.3762|            0.6797|
## |X                   |    3|Illinois Basin       |Propionate | 0.0039|               0.0006|       0.0033|                    2|             36|           251808|       -0.4213|          1.3762|            0.6797|
## |O                   |    3|Illinois Basin       |Butyrate   | 0.1775|               0.0011|       0.1764|                    2|             36|           251808|       -0.0410|          1.3762|            0.6797|
## |X                   |    3|Illinois Basin       |Butyrate   | 0.0050|               0.0006|       0.0044|                    2|             36|           251808|       -0.4189|          1.3762|            0.6797|
## |O                   |    3|Illinois Basin       |Valerate   | 0.0000|               0.0011|      -0.0011|                    2|             36|           251808|       -0.4309|          1.3762|            0.6797|
## |X                   |    3|Illinois Basin       |Valerate   | 0.0046|               0.0006|       0.0040|                    2|             36|           251808|       -0.4198|          1.3762|            0.6797|
## |O                   |    4|Illinois Basin       |Formate    | 0.0138|               0.0017|       0.0121|                    2|             11|            27209|       -0.4020|         -0.5992|           -0.7919|
## |X                   |    4|Illinois Basin       |Formate    | 0.0099|               0.0000|       0.0099|                    2|             11|            27209|       -0.4069|         -0.5992|           -0.7919|
## |O                   |    4|Illinois Basin       |Acetate    | 0.0047|               0.0017|       0.0030|                    2|             11|            27209|       -0.4220|         -0.5992|           -0.7919|
## |X                   |    4|Illinois Basin       |Acetate    | 0.0042|               0.0000|       0.0042|                    2|             11|            27209|       -0.4194|         -0.5992|           -0.7919|
## |O                   |    4|Illinois Basin       |Propionate | 0.0116|               0.0017|       0.0099|                    2|             11|            27209|       -0.4068|         -0.5992|           -0.7919|
## |X                   |    4|Illinois Basin       |Propionate | 0.0009|               0.0000|       0.0009|                    2|             11|            27209|       -0.4266|         -0.5992|           -0.7919|
## |O                   |    4|Illinois Basin       |Butyrate   | 0.0070|               0.0017|       0.0053|                    2|             11|            27209|       -0.4169|         -0.5992|           -0.7919|
## |X                   |    4|Illinois Basin       |Butyrate   | 0.0016|               0.0000|       0.0016|                    2|             11|            27209|       -0.4251|         -0.5992|           -0.7919|
## |O                   |    4|Illinois Basin       |Valerate   | 0.0019|               0.0017|       0.0002|                    2|             11|            27209|       -0.4281|         -0.5992|           -0.7919|
## |X                   |    4|Illinois Basin       |Valerate   | 0.0016|               0.0000|       0.0016|                    2|             11|            27209|       -0.4251|         -0.5992|           -0.7919|
## |O                   |    5|Illinois Basin       |Formate    | 0.0151|               0.0000|       0.0151|                    2|             28|           345036|       -0.3955|          0.7441|            1.2906|
## |X                   |    5|Illinois Basin       |Formate    | 0.0225|               0.0001|       0.0224|                    2|             28|           345036|       -0.3794|          0.7441|            1.2906|
## |O                   |    5|Illinois Basin       |Acetate    | 0.0001|               0.0000|       0.0001|                    2|             28|           345036|       -0.4284|          0.7441|            1.2906|
## |X                   |    5|Illinois Basin       |Acetate    | 0.0000|               0.0001|      -0.0001|                    2|             28|           345036|       -0.4287|          0.7441|            1.2906|
## |O                   |    5|Illinois Basin       |Propionate | 0.0020|               0.0000|       0.0020|                    2|             28|           345036|       -0.4242|          0.7441|            1.2906|
## |X                   |    5|Illinois Basin       |Propionate | 0.0023|               0.0001|       0.0022|                    2|             28|           345036|       -0.4237|          0.7441|            1.2906|
## |O                   |    5|Illinois Basin       |Butyrate   | 0.0001|               0.0000|       0.0001|                    2|             28|           345036|       -0.4284|          0.7441|            1.2906|
## |X                   |    5|Illinois Basin       |Butyrate   | 0.0001|               0.0001|       0.0000|                    2|             28|           345036|       -0.4286|          0.7441|            1.2906|
## |O                   |    5|Illinois Basin       |Valerate   | 0.0009|               0.0000|       0.0009|                    2|             28|           345036|       -0.4267|          0.7441|            1.2906|
## |X                   |    5|Illinois Basin       |Valerate   | 0.0001|               0.0001|       0.0000|                    2|             28|           345036|       -0.4286|          0.7441|            1.2906|
## |O                   |    1|Illinois Basin       |Formate    | 0.3783|               0.0015|       0.3768|                    3|             11|            41608|        0.3993|         -0.5992|           -0.6975|
## |X                   |    1|Illinois Basin       |Formate    | 0.4754|               0.0051|       0.4703|                    3|             11|            41608|        0.6047|         -0.5992|           -0.6975|
## |O                   |    1|Illinois Basin       |Acetate    | 1.8155|               0.0015|       1.8140|                    3|             11|            41608|        3.5569|         -0.5992|           -0.6975|
## |X                   |    1|Illinois Basin       |Acetate    | 0.6754|               0.0051|       0.6703|                    3|             11|            41608|        1.0441|         -0.5992|           -0.6975|
## |O                   |    1|Illinois Basin       |Propionate | 0.0824|               0.0015|       0.0809|                    3|             11|            41608|       -0.2508|         -0.5992|           -0.6975|
## |X                   |    1|Illinois Basin       |Propionate | 0.0819|               0.0051|       0.0768|                    3|             11|            41608|       -0.2598|         -0.5992|           -0.6975|
## |O                   |    1|Illinois Basin       |Butyrate   | 1.6968|               0.0015|       1.6953|                    3|             11|            41608|        3.2961|         -0.5992|           -0.6975|
## |X                   |    1|Illinois Basin       |Butyrate   | 2.9497|               0.0051|       2.9446|                    3|             11|            41608|        6.0409|         -0.5992|           -0.6975|
## |O                   |    1|Illinois Basin       |Valerate   | 0.4267|               0.0015|       0.4252|                    3|             11|            41608|        0.5056|         -0.5992|           -0.6975|
## |X                   |    1|Illinois Basin       |Valerate   | 0.5459|               0.0051|       0.5408|                    3|             11|            41608|        0.7596|         -0.5992|           -0.6975|
## |O                   |    2|Illinois Basin       |Formate    | 0.3467|               0.0177|       0.3290|                    3|             32|           181755|        0.2943|          1.0601|            0.2207|
## |X                   |    2|Illinois Basin       |Formate    | 0.4047|               0.0134|       0.3913|                    3|             32|           181755|        0.4311|          1.0601|            0.2207|
## |O                   |    2|Illinois Basin       |Acetate    | 0.6165|               0.0177|       0.5988|                    3|             32|           181755|        0.8870|          1.0601|            0.2207|
## |X                   |    2|Illinois Basin       |Acetate    | 0.4619|               0.0134|       0.4485|                    3|             32|           181755|        0.5568|          1.0601|            0.2207|
## |O                   |    2|Illinois Basin       |Propionate | 0.0239|               0.0177|       0.0062|                    3|             32|           181755|       -0.4149|          1.0601|            0.2207|
## |X                   |    2|Illinois Basin       |Propionate | 0.0377|               0.0134|       0.0243|                    3|             32|           181755|       -0.3752|          1.0601|            0.2207|
## |O                   |    2|Illinois Basin       |Butyrate   | 0.0067|               0.0177|      -0.0110|                    3|             32|           181755|       -0.4527|          1.0601|            0.2207|
## |X                   |    2|Illinois Basin       |Butyrate   | 0.2420|               0.0134|       0.2286|                    3|             32|           181755|        0.0737|          1.0601|            0.2207|
## |O                   |    2|Illinois Basin       |Valerate   | 0.0108|               0.0177|      -0.0069|                    3|             32|           181755|       -0.4437|          1.0601|            0.2207|
## |X                   |    2|Illinois Basin       |Valerate   | 0.0239|               0.0134|       0.0105|                    3|             32|           181755|       -0.4055|          1.0601|            0.2207|
## |O                   |    3|Illinois Basin       |Formate    | 0.0100|               0.0011|       0.0089|                    3|             36|           295654|       -0.4090|          1.3762|            0.9670|
## |X                   |    3|Illinois Basin       |Formate    | 0.0235|               0.0006|       0.0229|                    3|             36|           295654|       -0.3783|          1.3762|            0.9670|
## |O                   |    3|Illinois Basin       |Acetate    | 0.0144|               0.0011|       0.0133|                    3|             36|           295654|       -0.3993|          1.3762|            0.9670|
## |X                   |    3|Illinois Basin       |Acetate    | 0.0189|               0.0006|       0.0183|                    3|             36|           295654|       -0.3884|          1.3762|            0.9670|
## |O                   |    3|Illinois Basin       |Propionate | 0.0063|               0.0011|       0.0052|                    3|             36|           295654|       -0.4171|          1.3762|            0.9670|
## |X                   |    3|Illinois Basin       |Propionate | 0.0039|               0.0006|       0.0033|                    3|             36|           295654|       -0.4213|          1.3762|            0.9670|
## |O                   |    3|Illinois Basin       |Butyrate   | 0.1775|               0.0011|       0.1764|                    3|             36|           295654|       -0.0410|          1.3762|            0.9670|
## |X                   |    3|Illinois Basin       |Butyrate   | 0.0050|               0.0006|       0.0044|                    3|             36|           295654|       -0.4189|          1.3762|            0.9670|
## |O                   |    3|Illinois Basin       |Valerate   | 0.0000|               0.0011|      -0.0011|                    3|             36|           295654|       -0.4309|          1.3762|            0.9670|
## |X                   |    3|Illinois Basin       |Valerate   | 0.0046|               0.0006|       0.0040|                    3|             36|           295654|       -0.4198|          1.3762|            0.9670|
## |O                   |    4|Illinois Basin       |Formate    | 0.0138|               0.0017|       0.0121|                    3|             12|           113529|       -0.4020|         -0.5202|           -0.2263|
## |X                   |    4|Illinois Basin       |Formate    | 0.0099|               0.0000|       0.0099|                    3|             12|           113529|       -0.4069|         -0.5202|           -0.2263|
## |O                   |    4|Illinois Basin       |Acetate    | 0.0047|               0.0017|       0.0030|                    3|             12|           113529|       -0.4220|         -0.5202|           -0.2263|
## |X                   |    4|Illinois Basin       |Acetate    | 0.0042|               0.0000|       0.0042|                    3|             12|           113529|       -0.4194|         -0.5202|           -0.2263|
## |O                   |    4|Illinois Basin       |Propionate | 0.0116|               0.0017|       0.0099|                    3|             12|           113529|       -0.4068|         -0.5202|           -0.2263|
## |X                   |    4|Illinois Basin       |Propionate | 0.0009|               0.0000|       0.0009|                    3|             12|           113529|       -0.4266|         -0.5202|           -0.2263|
## |O                   |    4|Illinois Basin       |Butyrate   | 0.0070|               0.0017|       0.0053|                    3|             12|           113529|       -0.4169|         -0.5202|           -0.2263|
## |X                   |    4|Illinois Basin       |Butyrate   | 0.0016|               0.0000|       0.0016|                    3|             12|           113529|       -0.4251|         -0.5202|           -0.2263|
## |O                   |    4|Illinois Basin       |Valerate   | 0.0019|               0.0017|       0.0002|                    3|             12|           113529|       -0.4281|         -0.5202|           -0.2263|
## |X                   |    4|Illinois Basin       |Valerate   | 0.0016|               0.0000|       0.0016|                    3|             12|           113529|       -0.4251|         -0.5202|           -0.2263|
## |O                   |    5|Illinois Basin       |Formate    | 0.0151|               0.0000|       0.0151|                    3|             24|           122603|       -0.3955|          0.4280|           -0.1668|
## |X                   |    5|Illinois Basin       |Formate    | 0.0225|               0.0001|       0.0224|                    3|             24|           122603|       -0.3794|          0.4280|           -0.1668|
## |O                   |    5|Illinois Basin       |Acetate    | 0.0001|               0.0000|       0.0001|                    3|             24|           122603|       -0.4284|          0.4280|           -0.1668|
## |X                   |    5|Illinois Basin       |Acetate    | 0.0000|               0.0001|      -0.0001|                    3|             24|           122603|       -0.4287|          0.4280|           -0.1668|
## |O                   |    5|Illinois Basin       |Propionate | 0.0020|               0.0000|       0.0020|                    3|             24|           122603|       -0.4242|          0.4280|           -0.1668|
## |X                   |    5|Illinois Basin       |Propionate | 0.0023|               0.0001|       0.0022|                    3|             24|           122603|       -0.4237|          0.4280|           -0.1668|
## |O                   |    5|Illinois Basin       |Butyrate   | 0.0001|               0.0000|       0.0001|                    3|             24|           122603|       -0.4284|          0.4280|           -0.1668|
## |X                   |    5|Illinois Basin       |Butyrate   | 0.0001|               0.0001|       0.0000|                    3|             24|           122603|       -0.4286|          0.4280|           -0.1668|
## |O                   |    5|Illinois Basin       |Valerate   | 0.0009|               0.0000|       0.0009|                    3|             24|           122603|       -0.4267|          0.4280|           -0.1668|
## |X                   |    5|Illinois Basin       |Valerate   | 0.0001|               0.0001|       0.0000|                    3|             24|           122603|       -0.4286|          0.4280|           -0.1668|
## |O                   |    6|Cook Inlet gas field |Formate    | 0.4816|               0.2116|       0.2700|                    1|              8|            45849|        0.1646|         -0.8363|           -0.6697|
## |X                   |    6|Cook Inlet gas field |Formate    | 0.6315|               0.1704|       0.4611|                    1|              8|            45849|        0.5845|         -0.8363|           -0.6697|
## |O                   |    6|Cook Inlet gas field |Acetate    | 0.8019|               0.2116|       0.5903|                    1|              8|            45849|        0.8684|         -0.8363|           -0.6697|
## |X                   |    6|Cook Inlet gas field |Acetate    | 0.9266|               0.1704|       0.7562|                    1|              8|            45849|        1.2328|         -0.8363|           -0.6697|
## |O                   |    6|Cook Inlet gas field |Propionate | 0.8114|               0.2116|       0.5998|                    1|              8|            45849|        0.8892|         -0.8363|           -0.6697|
## |X                   |    6|Cook Inlet gas field |Propionate | 0.7652|               0.1704|       0.5948|                    1|              8|            45849|        0.8782|         -0.8363|           -0.6697|
## |O                   |    6|Cook Inlet gas field |Butyrate   | 1.0671|               0.2116|       0.8555|                    1|              8|            45849|        1.4510|         -0.8363|           -0.6697|
## |X                   |    6|Cook Inlet gas field |Butyrate   | 0.8844|               0.1704|       0.7140|                    1|              8|            45849|        1.1401|         -0.8363|           -0.6697|
## |O                   |    6|Cook Inlet gas field |Valerate   | 1.1412|               0.2116|       0.9296|                    1|              8|            45849|        1.6138|         -0.8363|           -0.6697|
## |X                   |    6|Cook Inlet gas field |Valerate   | 1.3782|               0.1704|       1.2078|                    1|              8|            45849|        2.2250|         -0.8363|           -0.6697|
## |O                   |    7|Cook Inlet gas field |Formate    | 0.2015|               0.0313|       0.1702|                    1|             NA|               NA|       -0.0546|              NA|                NA|
## |X                   |    7|Cook Inlet gas field |Formate    | 0.3170|               0.4221|      -0.1051|                    1|             NA|               NA|       -0.6595|              NA|                NA|
## |O                   |    7|Cook Inlet gas field |Acetate    | 0.5635|               0.0313|       0.5322|                    1|             NA|               NA|        0.7407|              NA|                NA|
## |X                   |    7|Cook Inlet gas field |Acetate    | 0.5779|               0.4221|       0.1558|                    1|             NA|               NA|       -0.0863|              NA|                NA|
## |O                   |    7|Cook Inlet gas field |Propionate | 0.4468|               0.0313|       0.4155|                    1|             NA|               NA|        0.4843|              NA|                NA|
## |X                   |    7|Cook Inlet gas field |Propionate | 0.3263|               0.4221|      -0.0958|                    1|             NA|               NA|       -0.6390|              NA|                NA|
## |O                   |    7|Cook Inlet gas field |Butyrate   | 0.7084|               0.0313|       0.6771|                    1|             NA|               NA|        1.0591|              NA|                NA|
## |X                   |    7|Cook Inlet gas field |Butyrate   | 1.1211|               0.4221|       0.6990|                    1|             NA|               NA|        1.1072|              NA|                NA|
## |O                   |    7|Cook Inlet gas field |Valerate   | 0.7187|               0.0313|       0.6874|                    1|             NA|               NA|        1.0817|              NA|                NA|
## |X                   |    7|Cook Inlet gas field |Valerate   | 0.8440|               0.4221|       0.4219|                    1|             NA|               NA|        0.4984|              NA|                NA|
## |O                   |    8|Cook Inlet gas field |Formate    | 0.0153|               0.0690|      -0.0537|                    1|             10|           149111|       -0.5465|         -0.6782|            0.0069|
## |X                   |    8|Cook Inlet gas field |Formate    | 0.0761|               0.0473|       0.0288|                    1|             10|           149111|       -0.3653|         -0.6782|            0.0069|
## |O                   |    8|Cook Inlet gas field |Acetate    | 0.0670|               0.0690|      -0.0020|                    1|             10|           149111|       -0.4330|         -0.6782|            0.0069|
## |X                   |    8|Cook Inlet gas field |Acetate    | 0.5056|               0.0473|       0.4583|                    1|             10|           149111|        0.5783|         -0.6782|            0.0069|
## |O                   |    8|Cook Inlet gas field |Propionate | 0.0562|               0.0690|      -0.0128|                    1|             10|           149111|       -0.4567|         -0.6782|            0.0069|
## |X                   |    8|Cook Inlet gas field |Propionate | 0.0796|               0.0473|       0.0323|                    1|             10|           149111|       -0.3576|         -0.6782|            0.0069|
## |O                   |    8|Cook Inlet gas field |Butyrate   | 0.9820|               0.0690|       0.9130|                    1|             10|           149111|        1.5773|         -0.6782|            0.0069|
## |X                   |    8|Cook Inlet gas field |Butyrate   | 1.0956|               0.0473|       1.0483|                    1|             10|           149111|        1.8746|         -0.6782|            0.0069|
## |O                   |    8|Cook Inlet gas field |Valerate   | 0.6374|               0.0690|       0.5684|                    1|             10|           149111|        0.8202|         -0.6782|            0.0069|
## |X                   |    8|Cook Inlet gas field |Valerate   | 0.7702|               0.0473|       0.7229|                    1|             10|           149111|        1.1597|         -0.6782|            0.0069|
## |O                   |    9|Cook Inlet gas field |Formate    | 0.0217|               0.1847|      -0.1630|                    1|              3|             7384|       -0.7867|         -1.2314|           -0.9218|
## |X                   |    9|Cook Inlet gas field |Formate    | 0.0155|               0.0213|      -0.0058|                    1|              3|             7384|       -0.4413|         -1.2314|           -0.9218|
## |O                   |    9|Cook Inlet gas field |Acetate    | 0.0265|               0.1847|      -0.1582|                    1|              3|             7384|       -0.7761|         -1.2314|           -0.9218|
## |X                   |    9|Cook Inlet gas field |Acetate    | 0.0223|               0.0213|       0.0010|                    1|              3|             7384|       -0.4264|         -1.2314|           -0.9218|
## |O                   |    9|Cook Inlet gas field |Propionate | 0.0226|               0.1847|      -0.1621|                    1|              3|             7384|       -0.7847|         -1.2314|           -0.9218|
## |X                   |    9|Cook Inlet gas field |Propionate | 0.0181|               0.0213|      -0.0032|                    1|              3|             7384|       -0.4356|         -1.2314|           -0.9218|
## |O                   |    9|Cook Inlet gas field |Butyrate   | 0.2655|               0.1847|       0.0808|                    1|              3|             7384|       -0.2510|         -1.2314|           -0.9218|
## |X                   |    9|Cook Inlet gas field |Butyrate   | 0.2917|               0.0213|       0.2704|                    1|              3|             7384|        0.1655|         -1.2314|           -0.9218|
## |O                   |    9|Cook Inlet gas field |Valerate   | 0.4864|               0.1847|       0.3017|                    1|              3|             7384|        0.2343|         -1.2314|           -0.9218|
## |X                   |    9|Cook Inlet gas field |Valerate   | 0.2065|               0.0213|       0.1852|                    1|              3|             7384|       -0.0217|         -1.2314|           -0.9218|
## |O                   |   10|Cook Inlet gas field |Formate    | 0.0167|               0.3317|      -0.3150|                    1|              1|             7639|       -1.1206|         -1.3894|           -0.9201|
## |X                   |   10|Cook Inlet gas field |Formate    | 0.0206|               0.2980|      -0.2774|                    1|              1|             7639|       -1.0380|         -1.3894|           -0.9201|
## |O                   |   10|Cook Inlet gas field |Acetate    | 0.4937|               0.3317|       0.1620|                    1|              1|             7639|       -0.0726|         -1.3894|           -0.9201|
## |X                   |   10|Cook Inlet gas field |Acetate    | 0.4348|               0.2980|       0.1368|                    1|              1|             7639|       -0.1280|         -1.3894|           -0.9201|
## |O                   |   10|Cook Inlet gas field |Propionate | 0.3806|               0.3317|       0.0489|                    1|              1|             7639|       -0.3211|         -1.3894|           -0.9201|
## |X                   |   10|Cook Inlet gas field |Propionate | 0.5077|               0.2980|       0.2097|                    1|              1|             7639|        0.0322|         -1.3894|           -0.9201|
## |O                   |   10|Cook Inlet gas field |Butyrate   | 0.4170|               0.3317|       0.0853|                    1|              1|             7639|       -0.2412|         -1.3894|           -0.9201|
## |X                   |   10|Cook Inlet gas field |Butyrate   | 0.3840|               0.2980|       0.0860|                    1|              1|             7639|       -0.2396|         -1.3894|           -0.9201|
## |O                   |   10|Cook Inlet gas field |Valerate   | 0.4146|               0.3317|       0.0829|                    1|              1|             7639|       -0.2464|         -1.3894|           -0.9201|
## |X                   |   10|Cook Inlet gas field |Valerate   | 0.3162|               0.2980|       0.0182|                    1|              1|             7639|       -0.3886|         -1.3894|           -0.9201|
## |O                   |   11|Cook Inlet gas field |Formate    | 0.7458|               0.5720|       0.1738|                    1|              3|            74840|       -0.0467|         -1.2314|           -0.4798|
## |X                   |   11|Cook Inlet gas field |Formate    | 0.7524|               0.6779|       0.0745|                    1|              3|            74840|       -0.2649|         -1.2314|           -0.4798|
## |O                   |   11|Cook Inlet gas field |Acetate    | 0.7908|               0.5720|       0.2188|                    1|              3|            74840|        0.0521|         -1.2314|           -0.4798|
## |X                   |   11|Cook Inlet gas field |Acetate    | 0.7919|               0.6779|       0.1140|                    1|              3|            74840|       -0.1781|         -1.2314|           -0.4798|
## |O                   |   11|Cook Inlet gas field |Propionate | 0.7934|               0.5720|       0.2214|                    1|              3|            74840|        0.0579|         -1.2314|           -0.4798|
## |X                   |   11|Cook Inlet gas field |Propionate | 0.8573|               0.6779|       0.1794|                    1|              3|            74840|       -0.0344|         -1.2314|           -0.4798|
## |O                   |   11|Cook Inlet gas field |Butyrate   | 0.6989|               0.5720|       0.1269|                    1|              3|            74840|       -0.1498|         -1.2314|           -0.4798|
## |X                   |   11|Cook Inlet gas field |Butyrate   | 0.7429|               0.6779|       0.0650|                    1|              3|            74840|       -0.2858|         -1.2314|           -0.4798|
## |O                   |   11|Cook Inlet gas field |Valerate   | 0.0307|               0.5720|      -0.5413|                    1|              3|            74840|       -1.6178|         -1.2314|           -0.4798|
## |X                   |   11|Cook Inlet gas field |Valerate   | 0.6596|               0.6779|      -0.0183|                    1|              3|            74840|       -0.4688|         -1.2314|           -0.4798|
## |O                   |   12|Powder River Basin   |Formate    | 0.0237|               0.0043|       0.0194|                    1|             26|           184191|       -0.3859|          0.5860|            0.2367|
## |X                   |   12|Powder River Basin   |Formate    | 0.0482|               0.2437|      -0.1955|                    1|             26|           184191|       -0.8581|          0.5860|            0.2367|
## |O                   |   12|Powder River Basin   |Acetate    | 0.7360|               0.0043|       0.7317|                    1|             26|           184191|        1.1790|          0.5860|            0.2367|
## |X                   |   12|Powder River Basin   |Acetate    | 0.0542|               0.2437|      -0.1895|                    1|             26|           184191|       -0.8449|          0.5860|            0.2367|
## |O                   |   12|Powder River Basin   |Propionate | 0.1641|               0.0043|       0.1598|                    1|             26|           184191|       -0.0775|          0.5860|            0.2367|
## |X                   |   12|Powder River Basin   |Propionate | 0.1889|               0.2437|      -0.0548|                    1|             26|           184191|       -0.5490|          0.5860|            0.2367|
## |O                   |   12|Powder River Basin   |Butyrate   | 0.1149|               0.0043|       0.1106|                    1|             26|           184191|       -0.1856|          0.5860|            0.2367|
## |X                   |   12|Powder River Basin   |Butyrate   | 0.0059|               0.2437|      -0.2378|                    1|             26|           184191|       -0.9510|          0.5860|            0.2367|
## |O                   |   12|Powder River Basin   |Valerate   | 0.2741|               0.0043|       0.2698|                    1|             26|           184191|        0.1642|          0.5860|            0.2367|
## |X                   |   12|Powder River Basin   |Valerate   | 0.0005|               0.2437|      -0.2432|                    1|             26|           184191|       -0.9629|          0.5860|            0.2367|
## |O                   |   13|Powder River Basin   |Formate    | 0.2639|               0.2099|       0.0540|                    1|             21|           139691|       -0.3099|          0.1910|           -0.0549|
## |X                   |   13|Powder River Basin   |Formate    | 0.2310|               0.1791|       0.0519|                    1|             21|           139691|       -0.3145|          0.1910|           -0.0549|
## |O                   |   13|Powder River Basin   |Acetate    | 0.2841|               0.2099|       0.0742|                    1|             21|           139691|       -0.2655|          0.1910|           -0.0549|
## |X                   |   13|Powder River Basin   |Acetate    | 0.2753|               0.1791|       0.0962|                    1|             21|           139691|       -0.2172|          0.1910|           -0.0549|
## |O                   |   13|Powder River Basin   |Propionate | 0.7700|               0.2099|       0.5601|                    1|             21|           139691|        0.8020|          0.1910|           -0.0549|
## |X                   |   13|Powder River Basin   |Propionate | 1.0878|               0.1791|       0.9087|                    1|             21|           139691|        1.5679|          0.1910|           -0.0549|
## |O                   |   13|Powder River Basin   |Butyrate   | 0.1525|               0.2099|      -0.0574|                    1|             21|           139691|       -0.5547|          0.1910|           -0.0549|
## |X                   |   13|Powder River Basin   |Butyrate   | 0.0087|               0.1791|      -0.1704|                    1|             21|           139691|       -0.8029|          0.1910|           -0.0549|
## |O                   |   13|Powder River Basin   |Valerate   | 0.1755|               0.2099|      -0.0344|                    1|             21|           139691|       -0.5041|          0.1910|           -0.0549|
## |X                   |   13|Powder River Basin   |Valerate   | 0.3224|               0.1791|       0.1433|                    1|             21|           139691|       -0.1137|          0.1910|           -0.0549|
## |O                   |   14|Powder River Basin   |Formate    | 0.0205|               0.1137|      -0.0932|                    1|             19|           126166|       -0.6333|          0.0329|           -0.1435|
## |X                   |   14|Powder River Basin   |Formate    | 0.0227|               0.0942|      -0.0715|                    1|             19|           126166|       -0.5857|          0.0329|           -0.1435|
## |O                   |   14|Powder River Basin   |Acetate    | 0.2700|               0.1137|       0.1563|                    1|             19|           126166|       -0.0852|          0.0329|           -0.1435|
## |X                   |   14|Powder River Basin   |Acetate    | 0.0048|               0.0942|      -0.0894|                    1|             19|           126166|       -0.6250|          0.0329|           -0.1435|
## |O                   |   14|Powder River Basin   |Propionate | 0.0721|               0.1137|      -0.0416|                    1|             19|           126166|       -0.5200|          0.0329|           -0.1435|
## |X                   |   14|Powder River Basin   |Propionate | 0.1109|               0.0942|       0.0167|                    1|             19|           126166|       -0.3919|          0.0329|           -0.1435|
## |O                   |   14|Powder River Basin   |Butyrate   | 0.0018|               0.1137|      -0.1119|                    1|             19|           126166|       -0.6744|          0.0329|           -0.1435|
## |X                   |   14|Powder River Basin   |Butyrate   | 0.2210|               0.0942|       0.1268|                    1|             19|           126166|       -0.1500|          0.0329|           -0.1435|
## |O                   |   14|Powder River Basin   |Valerate   | 0.0348|               0.1137|      -0.0789|                    1|             19|           126166|       -0.6019|          0.0329|           -0.1435|
## |X                   |   14|Powder River Basin   |Valerate   | 0.0464|               0.0942|      -0.0478|                    1|             19|           126166|       -0.5336|          0.0329|           -0.1435|
## |O                   |   15|Powder River Basin   |Formate    | 0.5206|               0.2565|       0.2641|                    1|             11|             8865|        0.1517|         -0.5992|           -0.9121|
## |X                   |   15|Powder River Basin   |Formate    | 0.7257|               0.2103|       0.5154|                    1|             11|             8865|        0.7038|         -0.5992|           -0.9121|
## |O                   |   15|Powder River Basin   |Acetate    | 0.3432|               0.2565|       0.0867|                    1|             11|             8865|       -0.2381|         -0.5992|           -0.9121|
## |X                   |   15|Powder River Basin   |Acetate    | 0.2406|               0.2103|       0.0303|                    1|             11|             8865|       -0.3620|         -0.5992|           -0.9121|
## |O                   |   15|Powder River Basin   |Propionate | 0.1493|               0.2565|      -0.1072|                    1|             11|             8865|       -0.6641|         -0.5992|           -0.9121|
## |X                   |   15|Powder River Basin   |Propionate | 0.1597|               0.2103|      -0.0506|                    1|             11|             8865|       -0.5397|         -0.5992|           -0.9121|
## |O                   |   15|Powder River Basin   |Butyrate   | 0.0566|               0.2565|      -0.1999|                    1|             11|             8865|       -0.8678|         -0.5992|           -0.9121|
## |X                   |   15|Powder River Basin   |Butyrate   | 0.1196|               0.2103|      -0.0907|                    1|             11|             8865|       -0.6278|         -0.5992|           -0.9121|
## |O                   |   15|Powder River Basin   |Valerate   | 0.1610|               0.2565|      -0.0955|                    1|             11|             8865|       -0.6384|         -0.5992|           -0.9121|
## |X                   |   15|Powder River Basin   |Valerate   | 0.1083|               0.2103|      -0.1020|                    1|             11|             8865|       -0.6527|         -0.5992|           -0.9121|
## |O                   |   16|Powder River Basin   |Formate    | 0.0144|               0.0130|       0.0014|                    1|             NA|               NA|       -0.4255|              NA|                NA|
## |X                   |   16|Powder River Basin   |Formate    | 0.0209|               0.0104|       0.0105|                    1|             NA|               NA|       -0.4055|              NA|                NA|
## |O                   |   16|Powder River Basin   |Acetate    | 0.0131|               0.0130|       0.0001|                    1|             NA|               NA|       -0.4283|              NA|                NA|
## |X                   |   16|Powder River Basin   |Acetate    | 0.0121|               0.0104|       0.0017|                    1|             NA|               NA|       -0.4248|              NA|                NA|
## |O                   |   16|Powder River Basin   |Propionate | 0.0272|               0.0130|       0.0142|                    1|             NA|               NA|       -0.3974|              NA|                NA|
## |X                   |   16|Powder River Basin   |Propionate | 0.0078|               0.0104|      -0.0026|                    1|             NA|               NA|       -0.4343|              NA|                NA|
## |O                   |   16|Powder River Basin   |Butyrate   | 0.0104|               0.0130|      -0.0026|                    1|             NA|               NA|       -0.4343|              NA|                NA|
## |X                   |   16|Powder River Basin   |Butyrate   | 0.0173|               0.0104|       0.0069|                    1|             NA|               NA|       -0.4134|              NA|                NA|
## |O                   |   16|Powder River Basin   |Valerate   | 0.0124|               0.0130|      -0.0006|                    1|             NA|               NA|       -0.4299|              NA|                NA|
## |X                   |   16|Powder River Basin   |Valerate   | 0.0191|               0.0104|       0.0087|                    1|             NA|               NA|       -0.4095|              NA|                NA|
## |O                   |   17|Powder River Basin   |Formate    | 0.7542|               0.1666|       0.5876|                    1|             NA|               NA|        0.8624|              NA|                NA|
## |X                   |   17|Powder River Basin   |Formate    | 0.7370|               0.2043|       0.5327|                    1|             NA|               NA|        0.7418|              NA|                NA|
## |O                   |   17|Powder River Basin   |Acetate    | 0.1272|               0.1666|      -0.0394|                    1|             NA|               NA|       -0.5151|              NA|                NA|
## |X                   |   17|Powder River Basin   |Acetate    | 0.1363|               0.2043|      -0.0680|                    1|             NA|               NA|       -0.5780|              NA|                NA|
## |O                   |   17|Powder River Basin   |Propionate | 0.1413|               0.1666|      -0.0253|                    1|             NA|               NA|       -0.4842|              NA|                NA|
## |X                   |   17|Powder River Basin   |Propionate | 0.1416|               0.2043|      -0.0627|                    1|             NA|               NA|       -0.5663|              NA|                NA|
## |O                   |   17|Powder River Basin   |Butyrate   | 0.1323|               0.1666|      -0.0343|                    1|             NA|               NA|       -0.5039|              NA|                NA|
## |X                   |   17|Powder River Basin   |Butyrate   | 0.1468|               0.2043|      -0.0575|                    1|             NA|               NA|       -0.5549|              NA|                NA|
## |O                   |   17|Powder River Basin   |Valerate   | 0.1654|               0.1666|      -0.0012|                    1|             NA|               NA|       -0.4312|              NA|                NA|
## |X                   |   17|Powder River Basin   |Valerate   | 0.1667|               0.2043|      -0.0376|                    1|             NA|               NA|       -0.5112|              NA|                NA|
```

```r
############################
```

```r
# Save as a compress, binary R dataset.  It's no longer readable with a text editor, but it saves metadata (eg, factor information).
#saveRDS(dsWide, file=pathOutput, compress="xz")
write.csv(dsLong, file=pathOutput, row.names=F)
```

The R session information (including the OS info, R version and all
packages used):


```r
sessionInfo()
```

```
## R version 3.1.0 Patched (2014-06-15 r65949)
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
## [1] plyr_1.8.1 knitr_1.6 
## 
## loaded via a namespace (and not attached):
## [1] evaluate_0.5.5 formatR_0.10   Rcpp_0.11.2    stringr_0.6.2 
## [5] tools_3.1.0
```

```r
Sys.time()
```

```
## [1] "2014-06-18 10:19:40 CDT"
```

