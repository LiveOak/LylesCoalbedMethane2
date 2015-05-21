# knitr::stitch_rmd(script="./Manipulate/LongToWide.R", output="./Manipulate/StitchedOutput/LongToWide.md")
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
############################
#+ LoadSources

############################
#+ LoadPackages
library(knitr)
# library(plyr)
# library(dplyr)
# library(reshape)

############################
#+ DeclareGlobals

pathInput <- "./Data/Derived/AllBasinsLong.csv"
pathOutput <- "./Data/Derived/AllBasinsWide.csv"

# basinsToInclude <- c("Illinois Basin")

############################
#+ LoadData
dsLong <- read.csv(pathInput, stringsAsFactors=FALSE)
colnames(dsLong)

############################
#+ TweakData
# dsLong <- dsLong[dsLong$Basin %in% basinsToInclude, ]

############################
#+ Widen, results='asis'

Widen <- function( d ) {  
  dIncubation1 <- d[d$IncubationReplicate=="O", ]
  dIncubation2 <- d[d$IncubationReplicate=="X", ]
  dMicroarray1 <- d[d$MicroarraryReplicate==1, ]
  dMicroarray2 <- d[d$MicroarraryReplicate==2, ]
  dMicroarray3 <- d[d$MicroarraryReplicate==3, ]
  
  data.frame(
    TotalAdjusted1 = dIncubation1$TotalAdjusted[1],
    TotalAdjusted2 = dIncubation2$TotalAdjusted[1],
    Rate1 = dIncubation1$AdjustedRate[1],
    Rate2 = dIncubation2$AdjustedRate[1],
    Unique1 = dMicroarray1$UniqueMcrGenes[1],
    Unique2 = dMicroarray2$UniqueMcrGenes[1],
    Unique3 = dMicroarray3$UniqueMcrGenes[1],
    Quantity1 = dMicroarray1$QuantityMcrGenes[1],
    Quantity2 = dMicroarray2$QuantityMcrGenes[1],
    Quantity3 = dMicroarray3$QuantityMcrGenes[1],
    
    TotalAdjustedZ1 = dIncubation1$TotalAdjustedZ[1],
    TotalAdjustedZ2 = dIncubation2$TotalAdjustedZ[1],
    RateZ1 = dIncubation1$AdjustedRateZ[1],
    RateZ2 = dIncubation2$AdjustedRateZ[1],
    UniqueZ1 = dMicroarray1$UniqueMcrGenesZ[1],
    UniqueZ2 = dMicroarray2$UniqueMcrGenesZ[1],
    UniqueZ3 = dMicroarray3$UniqueMcrGenesZ[1],
    QuantityZ1 = dMicroarray1$QuantityMcrGenesZ[1],
    QuantityZ2 = dMicroarray2$QuantityMcrGenesZ[1],
    QuantityZ3 = dMicroarray3$QuantityMcrGenesZ[1]
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
