# knitr::stitch_rmd(script="./Manipulate/GroomLong.R", output="./Manipulate/StitchedOutput/GroomLong.md")
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

pathInput <- "./Data/Raw/AllBasinsLong.csv"
pathOutput <- "./Data/Derived/AllBasinsLong.csv"

############################
#+ LoadData
dsLong <- read.csv(pathInput)
colnames(dsLong)

# Dataset description can be found at: http://stat.ethz.ch/R-manual/R-devel/library/datasets/html/mtcars.html
dsLong <- plyr::rename(dsLong, replace=c(
  "Incubation.Replicate" = "IncubationReplicate"
  , "Unamended.control.rate" = "UnamendedControlRate"
  , "Adjusted.Rate" = "AdjustedRate"
  , "Microarrary.Replicate" = "MicroarraryReplicate"
  , "Unique.mcr.genes" = "UniqueMcrGenes"
  , "Quantity.mcrgenes" = "QuantityMcrGenes"  
  , "Total.Methane..microM." = "TotalMethaneInMicroMoles" 
  , "Unamended.control.total" = "TotalMethaneUnamendedControlInMicroMoles"
  , "Adjusted.Total" = "TotalAdjusted" #MethaneInMicroMoles
))
############################
#+ TweakData
# dsLong <- dsLong[dsLong$Basin %in% basinsToInclude, ]

dsLong$TotalAdjustedZ <- scale(dsLong$TotalAdjusted)
dsLong$AdjustedRateZ <- scale(dsLong$AdjustedRate)
dsLong$UniqueMcrGenesZ <- scale(dsLong$UniqueMcrGenes)
dsLong$QuantityMcrGenesZ <- scale(dsLong$QuantityMcrGenes)

############################
#+ PrintTable
knitr::kable(dsLong, format="markdown")

############################
## @knitr SaveToDisk
# Save as a compress, binary R dataset.  It's no longer readable with a text editor, but it saves metadata (eg, factor information).
#saveRDS(dsWide, file=pathOutput, compress="xz")

# Save as a CSV so it's readable with a text editor.
write.csv(dsLong, file=pathOutput, row.names=F)
