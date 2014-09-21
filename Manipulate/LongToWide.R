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

############################
## @knitr SaveToDisk
# Save as a compress, binary R dataset.  It's no longer readable with a text editor, but it saves metadata (eg, factor information).
#saveRDS(dsWide, file=pathOutput, compress="xz")
write.csv(dsWide, file=pathOutput, row.names=F)
