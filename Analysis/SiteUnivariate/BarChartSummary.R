# "D:\Projects\Hlo\LylesCoalbedMethane2\Analysis\SiteUnivariate\BarChartSummary.R"

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
# sort(unique(ds$GeneName))
# sort(unique(ds$Site))
# plyr::ddply(ds[ds$GeneCategory=="Methane Production", ], .(Site), numcolwise(length))
# plyr::ddply(ds[ds$GeneName=="mcrA", ], .(Site), numcolwise(length))

# png(pngOutputPath, width=10, height=7.5, units="in", bg="transparent", res=500, type="cairo-png")
# > rainbow_hcl(n=3, c = 50, l = 70)
# [1] "#E495A5" "#86B875" "#7DB0DD"# 
unique(ds$Basin)

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
  theme(axis.text.y=element_text(colour="gray30", size=rel(.8)))  
g

#ggsave(pngFacetedOutputPath, plot=g, width=10, height=7.5, dpi=600)
ggsave(pngFacetedOutputPath, plot=g, width=17, height=13, units="cm", dpi=600)


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
ggsave(pngOutputPath, plot=p, width=15, height=5, units="cm", dpi=600)


stats::bartlett.test(AbundanceMean ~ GeneCategory, ds)
# lawstat::levene.test(AbundanceMean ~ GeneCategory, ds)
table(ds$GeneCategory)

ds$MethaneGene <- (ds$GeneCategory=="Methane Production")
summary(lm(AbundanceMean ~ GeneCategory, ds))
summary(lm(AbundanceMean ~ MethaneGene, ds))
