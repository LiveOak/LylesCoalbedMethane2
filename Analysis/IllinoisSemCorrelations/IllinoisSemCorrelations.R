rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
#####################################
## @knitr LoadPackages
# library(xtable)
library(knitr)
# library(plyr)
# library(scales) #For formating values in graphs
library(RColorBrewer)
library(ggplot2) #For graphing
library(lavaan) #For graphing
# library(OpenMx) #For graphing
# library(mgcv, quietly=TRUE) #For the Generalized Additive Model that smooths the longitudinal graphs.
#####################################
## @knitr DeclareGlobals
options(show.signif.stars=F) #Turn off the annotations on p-values

pathInputLong <- "./Data/Derived/AllBasinsLong.csv"
pathInputWide <- "./Data/Derived/AllBasinsWide.csv"

substrateOrder <- c("Formate", "Acetate", "Propionate", "Butyrate", "Valerate")
sitesToDrop <- c(7, 16, 17)

#####################################
## @knitr LoadData
# 'ds' stands for 'datasets'
dsLong <- read.csv(pathInputLong, stringsAsFactors=FALSE)
dsWide <- read.csv(pathInputWide, stringsAsFactors=FALSE)
# sapply(dsWide, class)

#####################################
## @knitr TweakData
dsWide$Quantity1 <- dsWide$Quantity1 / 10000
dsWide$Quantity2 <- dsWide$Quantity2 / 10000
dsWide$Quantity3 <- dsWide$Quantity3 / 10000

#Drop the sites without microarray data
dsLong <- dsLong[!(dsLong$Site %in% sitesToDrop), ]
dsWide <- dsWide[!(dsWide$Site %in% sitesToDrop), ]

dsLong$Substrate <- factor(dsLong$Substrate, levels=substrateOrder)
dsWide$Substrate <- factor(dsWide$Substrate, levels=substrateOrder)

dsLongIllinois <- dsLong[dsLong$Basin=="Illinois Basin", ]
dsWideIllinois <- dsWide[dsWide$Basin=="Illinois Basin", ]

# dsWide <- dsWide[dsWide$Substrate == "Formate", ]
# dsWide <- dsWide[dsWide$Substrate == "Acetate", ]
# dsWide <- dsWide[dsWide$Substrate == "Propionate", ]
# dsWide <- dsWide[dsWide$Substrate == "Butyrate", ]
# dsWide <- dsWide[dsWide$Substrate == "Valerate", ] 

#####################################
## @knitr Marginals

ggplot(dsLongIllinois, aes(x=TotalAdjusted)) + 
  geom_histogram(binwidth=5) +
  facet_grid(Substrate~IncubationReplicate, scales="free_y") +
  theme_bw() +
  labs(title="Illinois Basin")

ggplot(dsLongIllinois, aes(x=QuantityMcrGenes)) + 
  geom_histogram(binwidth=10000) +
  facet_grid(Substrate~MicroarraryReplicate, scales="free_y") +
  theme_bw() +
  labs(title="Illinois Basin")

ggplot(dsLongIllinois, aes(x=UniqueMcrGenes)) + 
  geom_histogram(binwidth=1) +
  facet_grid(Substrate~MicroarraryReplicate, scales="free_y") +
  theme_bw() +
  labs(title="Illinois Basin")

#####################################
## @knitr Models

# manifests <- c("QuantityZ1", "QuantityZ2", "QuantityZ3")
# manifests <- c("Quantity1", "Quantity2", "Quantity3")
# latents <- c("Quantity")
# factorModel <- mxModel("One Factor",
#                        type="RAM",
#                        manifestVars = manifests,
#                        latentVars = latents,
#                        mxPath(from=latents, to=manifests),
#                        mxPath(from=manifests, arrows=2),
#                        mxPath(from=latents, arrows=2,
#                               free=FALSE, values=1.0),
#                        mxPath(from="one", to=manifests, arrows=1, free=T),
#                        mxData(dsWide[, manifests], type="raw",
#                               numObs=nrow(dsWide)))
# summary(mxRun(factorModel))
# 
# cor(dsWide[,  c("QuantityZ1", "QuantityZ2", "QuantityZ3")])
# cor(dsWide[,  c("Quantity1", "Quantity2", "Quantity3")])

model <- "
  # measurement model
#   Rate =~ 1*RateZ1 + 1*RateZ2
#   Rate =~ 1*Rate1 + 1*Rate2
#   Quantity =~ 1*QuantityZ1 + 1*QuantityZ2 + 1*QuantityZ3
  Quantity =~ 1*Quantity1 + 1*Quantity2 + 1*Quantity3
#   Quantity =~ q*Quantity1 + q*Quantity2 + q*Quantity3
#   Rate =~ RateZ1 
#   Quantity =~ QuantityZ1 
#   Total =~ TotalAdjusted1
#   Total =~ TotalAdjusted2
  Total =~ 1*TotalAdjusted2
#   Total =~ 1*TotalAdjusted1 + 1*TotalAdjusted2
#   Total =~ t*TotalAdjusted1 + t*TotalAdjusted2
  
  # regressions
#   Rate ~ Quantity
  Total ~ Quantity

  # fix variances of factors
#     Rate ~~ 1*Rate
    Total ~~ vT*Total
    Quantity ~~ vQ*Quantity
"
# substrateOrder <- c("Formate")
# for( substrate in substrateOrder ) {
#   cat("============ ", substrate, " ==============\n")
#   dsSubstrate <- dsWide[dsWide$Substrate==substrate & dsWide$Basin=="Illinois Basin", ]
#   fit <- sem(model, data = dsSubstrate)
#   summary(fit, standardized = FALSE)
# }

dsSubstrate <- dsWide[dsWide$Substrate=="Formate" & dsWide$Basin=="Illinois Basin", ]
  fit <- sem(model, data = dsSubstrate)
  summary(fit, standardized = FALSE)

varTable(fit)
inspect(fit,"theta")

cor(dsSubstrate$TotalAdjusted1, dsSubstrate$TotalAdjusted2)
M <- cor(dsSubstrate[, c("TotalAdjusted1", "TotalAdjusted2", "Quantity1", "Quantity2", "Quantity3")])
corrplot::corrplot(M, addCoef.col = "black")


# # fit <- sem(model, data = dsWide)
# # summary(fit, standardized = FALSE)
