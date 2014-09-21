rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
#####################################
## @knitr LoadPackages
# library(xtable)
library(knitr)
library(plyr)
# library(scales) #For formating values in graphs
library(RColorBrewer)
library(ggplot2) #For graphing
# library(lavaan) #For graphing
# library(OpenMx) #For graphing
# library(mgcv, quietly=TRUE) #For the Generalized Additive Model that smooths the longitudinal graphs.
#####################################
## @knitr DeclareGlobals
options(show.signif.stars=F) #Turn off the annotations on p-values
options(stringsAsFactors=FALSE) #By default, character/string variables will NOT be automatically converted to factors.

pathInputLong <- "./Data/Derived/AllBasinsLong.csv"
pathInputWide <- "./Data/Derived/AllBasinsWide.csv"

substrateOrder <- c("Formate", "Acetate", "Propionate", "Butyrate", "Valerate")
sitesToDrop <- c(7, 16, 17)

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

ggplot(dsLongIllinois, aes(x=AdjustedRate)) + 
  geom_density() +
  facet_grid(Substrate~IncubationReplicate, scales="free_y") +
  theme_bw() +
  labs(title="Illinois Basin")

ggplot(dsLongIllinois, aes(x=QuantityMcrGenes)) + 
  geom_density() +
  facet_grid(Substrate~MicroarraryReplicate, scales="free_y") +
  theme_bw() +
  labs(title="Illinois Basin")

ggplot(dsLongIllinois, aes(x=UniqueMcrGenes)) + 
  geom_density() +
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

#  model <- "
#   # measurement model
# #   Rate =~ 1*RateZ1 + 1*RateZ2
# #   Rate =~ 1*Rate1 + 1*Rate2
#   Quantity =~ 1*QuantityZ1 + 1*QuantityZ2 + 1*QuantityZ3
# #   Quantity =~ q*Quantity1 + q*Quantity2 + q*Quantity3
#   Rate =~ RateZ1 
# #   Quantity =~ QuantityZ1 
#   
#   # regressions
#   Rate ~ Quantity
# 
#   # fix variances of factors
#     Rate ~~ 1*Rate
#     Quantity ~~ 1*Quantity
# "
# 
# for( substrate in substrateOrder ) {
#   cat("============ ", substrate, " ==============\n")
#   dsSubstrate <- dsWide[dsWide$Substrate==substrate, ]
#   fit <- sem(model, data = dsSubstrate)
#   summary(fit, standardized = FALSE)
# }
# # fit <- sem(model, data = dsWide)
# # summary(fit, standardized = FALSE)
