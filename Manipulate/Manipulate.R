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
options(stringsAsFactors=FALSE) #By default, character/string variables will NOT be automatically converted to factors.

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
write.csv(dsSummary, file=csvUnpackedSummaryPath, row.names=F)

table(dsSummary$GeneCategory, dsSummary$Basin)

# CategoryMean <- function( df ) {
#   with(df, data.frame(
#     MeanRelativeAbundance = mean(df$AbudanceRelativeToBasin, na.rm=T)
#   ))
# }
# ddply(dsSummary, .(GeneCategory), CategoryMean)
