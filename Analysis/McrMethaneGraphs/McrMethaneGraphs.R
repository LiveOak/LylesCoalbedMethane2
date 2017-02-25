rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
#####################################
## @knitr LoadPackages
library(knitr)
library(plyr)
library(scales) #For formating values in graphs
library(RColorBrewer)
library(grid) #For graphing
library(ggplot2) #For graphing
# library(mgcv, quietly=TRUE) #For the Generalized Additive Model that smooths the longitudinal graphs.
#####################################
## @knitr DeclareGlobals
options(show.signif.stars=F) #Turn off the annotations on p-values
options(stringsAsFactors=FALSE) #By default, character/string variables will NOT be automatically converted to factors.

pathInputLong <- "./Data/Derived/AllBasinsLong.csv"
pathInputWide <- "./Data/Derived/AllBasinsWide.csv"

basinOrder <- c("Illinois Basin", "Cook Inlet gas field", "Powder River Basin")
substrateOrder <- c("Formate", "Acetate", "Propionate", "Butyrate", "Valerate")
sitesToDrop <- c(7, 16, 17)

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
dsLong <- read.csv(pathInputLong)
dsWide <- read.csv(pathInputWide)
# sapply(dsWide, class)

#####################################
## @knitr TweakData

#Drop the sites without microarray data
dsLong <- dsLong[!(dsLong$Site %in% sitesToDrop), ]
dsWide <- dsWide[!(dsWide$Site %in% sitesToDrop), ]

#Reorder the substrates and basins.  The default alphabetical order isn't the most intuitive
dsLong$Basin <- factor(dsLong$Basin, levels=basinOrder)
dsWide$Basin <- factor(dsWide$Basin, levels=basinOrder)
dsLong$Substrate <- factor(dsLong$Substrate, levels=substrateOrder)
dsWide$Substrate <- factor(dsWide$Substrate, levels=substrateOrder)

dsLongIllinois <- dsLong[dsLong$Basin=="Illinois Basin", ]
dsWideIllinois <- dsWide[dsWide$Basin=="Illinois Basin", ]

CalculateSiteRange <- function( d ) {
  data.frame(
    RateMin = min(d$AdjustedRate, na.rm=T),
    RateMax = max(d$AdjustedRate, na.rm=T),
    TotalAdjustedMin = min(d$TotalAdjusted, na.rm=T),
    TotalAdjustedMax = max(d$TotalAdjusted, na.rm=T),
    UniqueMcrGenesMin = min(d$UniqueMcrGenes, na.rm=T),
    UniqueMcrGenesMax = max(d$UniqueMcrGenes, na.rm=T),
    QuantityMcrGenesMin = min(d$QuantityMcrGenes, na.rm=T),
    QuantityMcrGenesMax = max(d$QuantityMcrGenes, na.rm=T)
  )
}
dsSiteRange <- plyr::ddply(.data=dsLong, .variables=c("Basin", "Site", "Substrate"), CalculateSiteRange)

dsCorrelation <- plyr::ddply(dsWide, c("Basin", "Substrate"), summarize, 
                             CorrRateUnique=cor(RateMean, UniqueMean, use="pairwise.complete.obs"),
                             CorrRateQuantity=cor(RateMean, QuantityMean, use="pairwise.complete.obs"), 
                             CorrTotalUnique=cor(TotalAdjustedMean, UniqueMean, use="pairwise.complete.obs"),
                             CorrTotalQuantity=cor(TotalAdjustedMean, QuantityMean, use="pairwise.complete.obs"))
dsCorrelation$CorrRateUniquePretty <- paste0("italic(r)==", round(dsCorrelation$CorrRateUnique, 2))
dsCorrelation$CorrRateQuantityPretty <- paste0("italic(r)==", round(dsCorrelation$CorrRateQuantity, 2))
dsCorrelation$CorrTotalUniquePretty <- paste0("italic(r)==", round(dsCorrelation$CorrTotalUnique, 2))
dsCorrelation$CorrTotalQuantityPretty <- paste0("italic(r)==", round(dsCorrelation$CorrTotalQuantity, 2))

sitesIllinois <- sort(unique(dsLong[dsLong$Basin=="Illinois Basin", "Site"]))
sitesCook <- sort(unique(dsLong[dsLong$Basin=="Cook Inlet gas field", "Site"]))
sitesPowder <- sort(unique(dsLong[dsLong$Basin=="Powder River Basin", "Site"]))

paletteSiteDark <- c(RColorBrewer::brewer.pal(n=length(sitesIllinois), "Dark2"), RColorBrewer::brewer.pal(n=length(sitesCook), "Set1"), RColorBrewer::brewer.pal(n=length(sitesPowder), "Dark2"))
names(paletteSiteDark) <- c(sitesIllinois, sitesCook, sitesPowder)

paletteSiteLight <- grDevices::adjustcolor(paletteSiteDark, alpha.f=.5)
names(paletteSiteLight) <- c(sitesIllinois, sitesCook, sitesPowder)

#####################################
## @knitr Marginals

ggplot(dsLongIllinois, aes(x=TotalAdjusted)) + 
  geom_density() +
  facet_grid(Substrate~IncubationReplicate, scales="free_y") +
  ReportTheme +
  labs(title="Illinois Basin")

ggplot(dsLongIllinois, aes(x=AdjustedRate)) + 
  geom_density() +
  facet_grid(Substrate~IncubationReplicate, scales="free_y") +
  ReportTheme +
  labs(title="Illinois Basin")

ggplot(dsLongIllinois, aes(x=QuantityMcrGenes)) + 
  geom_density() +
  facet_grid(Substrate~MicroarraryReplicate, scales="free_y") +
  ReportTheme +
  labs(title="Illinois Basin")

ggplot(dsLongIllinois, aes(x=UniqueMcrGenes)) + 
  geom_density() +
  facet_grid(Substrate~MicroarraryReplicate, scales="free_y") +
  ReportTheme +
  labs(title="Illinois Basin")

#####################################
## @knitr Scatterplots

gQuantityTotal <- ggplot(dsLongIllinois, aes(x=QuantityMcrGenes, y=TotalAdjusted, color=factor(Site), fill=factor(Site))) + 
  geom_smooth(aes(color=NULL, fill=NULL), method="loess") +
  geom_smooth(aes(color=NULL, fill=NULL), method="lm") +
  geom_point(shape=21) +
  scale_x_continuous(label=scales::comma) +
  scale_color_manual(values=paletteSiteDark) +
  scale_fill_manual(values=paletteSiteLight) +
  facet_grid(Substrate~., scales="free_y") +
  ReportTheme +
  theme(legend.position="none") +
  labs(title="Illinois Basin")
gQuantityTotal

gQuantityRate <- ggplot(dsLongIllinois, aes(x=QuantityMcrGenes, y=AdjustedRate, color=factor(Site), fill=factor(Site))) + 
  geom_smooth(aes(color=NULL, fill=NULL), method="loess") +
  geom_smooth(aes(color=NULL, fill=NULL), method="lm") +
  geom_point(shape=21) +
  scale_x_continuous(label=scales::comma) +
  scale_color_manual(values=paletteSiteDark) +
  scale_fill_manual(values=paletteSiteLight) +
  facet_grid(Substrate~., scales="free_y") +
  ReportTheme +
  theme(legend.position="none") +
  labs(title="Illinois Basin")
gQuantityRate

gQuantitySite <- ggplot(dsWide, aes(x=QuantityMean, y=TotalAdjustedMean, label=Site, color=factor(Site))) + 
  #   geom_smooth(aes(color=NULL, label=NULL), method="loess") +
  geom_smooth(aes(color=NULL), method="lm", se=FALSE, color="gray30", linetype="F3") +
  geom_text() +
  scale_x_continuous(label=scales::comma) +
  scale_color_manual(values=paletteSiteDark) +
  facet_grid(Substrate~., scales="free_y") +
  ReportTheme +
  theme(legend.position="none") +
  labs(title="All Basins")
gQuantitySite

gQuantityRate %+% aes(x=UniqueMcrGenes)
gQuantitySite %+% aes(x=UniqueMean)

#####################################
## @knitr LayeredScatterplotsTotal
# gQuantitySite +
#   geom_point(data=dsLongIllinois, aes(x=QuantityMcrGenes, y=AdjustedRate, shape=IncubationReplicate, fill=factor(Site)), shape=21) +
#   geom_rect(data=dsSiteRange, aes(xmin=QuantityMcrGenesMin, xmax=QuantityMcrGenesMax, ymin=RateMin, ymax=RateMax, x=NULL, y=NULL)) +
#   scale_fill_manual(values=paletteSiteLight)

ggplot(dsWide, aes(label=Site, color=factor(Site))) +
  geom_text(data=dsCorrelation, aes(x=Inf, y=Inf, label=CorrTotalQuantityPretty), color="gray50", hjust=1, vjust=1, parse=T) +
  geom_smooth(aes(x=QuantityMean, y=TotalAdjustedMean, group=Substrate, fill=factor(Site)), method="lm", se=FALSE, color="gray30", linetype="F3") +
  geom_rect(data=dsSiteRange, aes(xmin=QuantityMcrGenesMin, xmax=QuantityMcrGenesMax, ymin=TotalAdjustedMin, ymax=TotalAdjustedMax), alpha=.1) +
  geom_point(data=dsLong, aes(x=QuantityMcrGenes, y=TotalAdjusted, shape=IncubationReplicate, fill=factor(Site)), shape=21) +
  geom_text(aes(x=QuantityMean, y=TotalAdjustedMean), alpha=1) +
  scale_x_continuous(label=scales::comma) +
  scale_color_manual(values=paletteSiteLight) +
  scale_fill_manual(values=paletteSiteLight) +
  facet_grid(Substrate~Basin, scales="free") +
  ReportTheme +
  theme(legend.position="none") +
  labs(x=expression(Signal*phantom(0)*intensity*phantom(0)*of*phantom(0)*italic(mcr)*phantom(0)*genes), y=expression(Methane*phantom(0)*(mu*mol)))

ggplot(dsWide, aes(label=Site, color=factor(Site))) +
  geom_text(data=dsCorrelation, aes(x=Inf, y=Inf, label=CorrTotalUniquePretty), color="gray50", hjust=1, vjust=1, parse=T) +
  geom_smooth(aes(x=UniqueMean, y=TotalAdjustedMean, fill=factor(Site), group=Substrate), method="lm", se=FALSE, color="gray30", linetype="F3") +
  geom_rect(data=dsSiteRange, aes(xmin=UniqueMcrGenesMin, xmax=UniqueMcrGenesMax, ymin=TotalAdjustedMin, ymax=TotalAdjustedMax), alpha=.1) +
  geom_point(data=dsLong, aes(x=UniqueMcrGenes, y=TotalAdjusted, shape=IncubationReplicate, fill=factor(Site)), shape=21) +
  geom_text(aes(x=UniqueMean, y=TotalAdjustedMean), alpha=1) +
  scale_x_continuous(label=scales::comma) +
  scale_color_manual(values=paletteSiteLight) +
  scale_fill_manual(values=paletteSiteLight) +
  facet_grid(Substrate~Basin, scales="free") +
  ReportTheme +
  theme(legend.position="none") +  
  labs(x=expression(Number*phantom(0)*of*phantom(0)*positive*phantom(0)*italic(mcr)*phantom(0)*probes), y=expression(Methane*phantom(0)*(mu*mol)))
  # labs(x=expression(Number*phantom(0)*of*phantom(0)*unique*phantom(0)*italic(mcr)*phantom(0)*genes), y=expression(Adjusted*phantom(0)*Total*phantom(0)*of*phantom(0)*methanogenesis*phantom(0)*(mu*mol/day)))
  # labs(x=expression(Number*phantom(0)*of*phantom(0)*unique*phantom(0)*italic(mcr)*phantom(0)*genes), y=expression(Rates*phantom(0)*of*phantom(0)*methanogenesis*phantom(0)*(mu*mol/day)))

prettyColumns <- grep("Pretty", x=colnames(dsCorrelation))
kable(dsCorrelation[, -prettyColumns], format="markdown")
#####################################
## @knitr LayeredScatterplotsRate
ggplot(dsWide, aes(label=Site, color=factor(Site))) +
  geom_text(data=dsCorrelation, aes(x=Inf, y=Inf, label=CorrRateQuantityPretty), color="gray50", hjust=1, vjust=1, parse=T) +
  geom_smooth(aes(x=QuantityMean, y=RateMean, group=Substrate), method="lm", se=FALSE, color="gray30", linetype="F3") +
  geom_rect(data=dsSiteRange, aes(xmin=QuantityMcrGenesMin, xmax=QuantityMcrGenesMax, ymin=RateMin, ymax=RateMax, fill=factor(Site)), alpha=.1) +
  geom_point(data=dsLong, aes(x=QuantityMcrGenes, y=AdjustedRate, shape=IncubationReplicate, fill=factor(Site)), shape=21) +
  geom_text(aes(x=QuantityMean, y=RateMean), alpha=1) +
  scale_x_continuous(label=scales::comma) +
  scale_color_manual(values=paletteSiteLight) +
  scale_fill_manual(values=paletteSiteLight) +
  facet_grid(Substrate~Basin, scales="free") +
  ReportTheme +
  theme(legend.position="none") +
  labs(x=expression(Signal*phantom(0)*intensity*phantom(0)*of*phantom(0)*italic(mcr)*phantom(0)*genes), y=expression(Rates*phantom(0)*of*phantom(0)*methanogenesis*phantom(0)*(mu*mol/day)))

ggplot(dsWide, aes(label=Site, color=factor(Site))) +
  geom_text(data=dsCorrelation, aes(x=Inf, y=Inf, label=CorrRateUniquePretty), color="gray50", hjust=1, vjust=1, parse=T) +
  geom_smooth(aes(x=UniqueMean, y=RateMean, group=Substrate), method="lm", se=FALSE, color="gray30", linetype="F3") +
  geom_rect(data=dsSiteRange, aes(xmin=UniqueMcrGenesMin, xmax=UniqueMcrGenesMax, ymin=RateMin, ymax=RateMax, fill=factor(Site)), alpha=.1) +
  geom_point(data=dsLong, aes(x=UniqueMcrGenes, y=AdjustedRate, shape=IncubationReplicate, fill=factor(Site)), shape=21) +
  geom_text(aes(x=UniqueMean, y=RateMean), alpha=1) +
  scale_x_continuous(label=scales::comma) +
  scale_color_manual(values=paletteSiteLight) +
  scale_fill_manual(values=paletteSiteLight) +
  facet_grid(Substrate~Basin, scales="free") +
  ReportTheme +
  theme(legend.position="none") +  
  labs(x=expression(Number*phantom(0)*of*phantom(0)*positive*phantom(0)*italic(mcr)*phantom(0)*probes), y=expression(Rates*phantom(0)*of*phantom(0)*methanogenesis*phantom(0)*(mu*mol/day)))
