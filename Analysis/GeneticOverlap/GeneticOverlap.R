rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
#####################################
## @knitr LoadPackages
requireNamespace("dplyr")
requireNamespace("plyr")
requireNamespace("scales") #For formating values in graphs
requireNamespace("RColorBrewer")
requireNamespace("grid") #For graphing
requireNamespace("gplots") #For graphing
library(ggplot2) #For graphing
library(magrittr)
# library(mgcv, quietly=TRUE) #For the Generalized Additive Model that smooths the longitudinal graphs.
#####################################
## @knitr DeclareGlobals
options(show.signif.stars=F) #Turn off the annotations on p-values

pathInputLong <- "./Data/Derived/Unpacked.csv"
# pathInputWide <- "./Data/Derived/AllBasinsWide.csv"

basinOrder <- c("Illinois", "CookInlet", "Powder")
geochipBasinVersion <- c("CookInlet"=3.2,  "Illinois"=4.0, "Powder"=4.0)

ReportTheme <- theme_bw() +
  theme(axis.ticks.length = grid::unit(0, "cm")) +
  theme(axis.text = element_text(color="gray40")) +
  theme(axis.title = element_text(color="gray40")) +
  theme(panel.border = element_rect(color="gray80")) +
  theme(axis.ticks = element_line(color="gray80")) +
  theme(strip.background=element_rect(color=NA, fill="gray95"))

#####################################
## @knitr LoadData
# 'ds' stands for 'datasets'
dsLong <- read.csv(pathInputLong, stringsAsFactors=FALSE)

# sapply(dsLong, class)

#####################################
## @knitr TweakData
# a <- as.integer(dsLong$GenbankID)
# dsLong$GenbankID[is.na(a)]
# dsLong$GenbankID <- as.integer(dsLong$GenbankID)

dsLong$GeochipVersion <- as.numeric(plyr::revalue(dsLong$Basin, replace=geochipBasinVersion))

#Reorder the substrates and basins.  The default alphabetical order isn't the most intuitive
dsLong$Basin <- factor(dsLong$Basin, levels=basinOrder)

dsLongV32 <- dsLong[dsLong$GeochipVersion==3.2, ]
dsLongV40 <- dsLong[dsLong$GeochipVersion==4.0, ]

dsLong$IsV32 <- (dsLong$GeochipVersion==3.2)

dsLongBasinUnique <- dsLong %>%
  dplyr::group_by(GenbankID, GeneCategory, Basin) %>%
  dplyr::summarize(
    PositiveProbeCount = n()
  )
  
table(dsLong$Basin)

vennList <- list(
  "Illinois\n(4.0)" = dsLongBasinUnique[dsLongBasinUnique$Basin=="Illinois", ]$GenbankID,
  "Cook Inlet\n(3.2)" = dsLongBasinUnique[dsLongBasinUnique$Basin=="CookInlet", ]$GenbankID,
  "Powder\n(4.0)" = dsLongBasinUnique[dsLongBasinUnique$Basin=="Powder", ]$GenbankID
)
# dsLong$Basin
#####################################
## @knitr CalculateOverlap

dsLong %>%
  dplyr::group_by(GeochipVersion) %>%
  dplyr::summarize(
    Count = n()
  )

setdiff(x=1:9, y=c(1,4,5))
intersect(x=1:9, y=c(0,1,4,5))

commonGenbankIDs <- intersect(dsLongV32$GenbankID, dsLongV40$GenbankID)
length(commonGenbankIDs)

print(
  dsLong %>%
    dplyr::group_by(GeneCategory, GeochipVersion) %>%
    dplyr::summarize(
      Count = n()
    )
  , n=40
)

dsLong %>%
  dplyr::group_by(GeneCategory) %>%
  dplyr::summarize(
    CountTotal = scales::comma(length(IsV32)),
    CountV32 = scales::comma(sum(IsV32)),
    CountV40 = scales::comma(sum(!IsV32))
  )

dsLongBasinProbeCount <- dsLongBasinUnique %>%
  dplyr::group_by(GeneCategory) %>%
  dplyr::summarize(
    Total = scales::comma(length(Basin)),
    Illinois = scales::comma(sum(Basin=="Illinois")),
    CookInlet = scales::comma(sum(Basin=="CookInlet")),
    Powder = scales::comma(sum(Basin=="Powder"))
  )
dsLongBasinProbeCount

#####################################
## @knitr PlotVennDiagrams

gplots::venn(vennList)

#####################################
## @knitr Marginals

# ggplot(dsLongIllinois, aes(x=TotalAdjusted)) + 
#   geom_density() +
#   facet_grid(Substrate~IncubationReplicate, scales="free_y") +
#   ReportTheme +
#   labs(title="Illinois Basin")

#####################################
## @knitr Scatterplots

# gQuantityTotal <- ggplot(dsLongIllinois, aes(x=QuantityMcrGenes, y=TotalAdjusted, color=factor(Site), fill=factor(Site), shape=IncubationReplicate)) + 
#   geom_smooth(aes(color=NULL, fill=NULL, shape=NULL), method="loess") +
#   geom_smooth(aes(color=NULL, fill=NULL, shape=NULL), method="lm") +
#   geom_point(shape=21) +
#   scale_x_continuous(label=scales::comma) +
#   scale_color_manual(values=paletteSiteDark) +
#   scale_fill_manual(values=paletteSiteLight) +
#   facet_grid(Substrate~., scales="free_y") +
#   ReportTheme +
#   theme(legend.position="none") +
#   labs(title="Illinois Basin")
# gQuantityTotal

