rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
#####################################
## @knitr LoadPackages
library(grid) #For graphing
library(magrittr)
requireNamespace("dplyr")
requireNamespace("plyr")
requireNamespace("scales") #For formating values in graphs
requireNamespace("RColorBrewer")
requireNamespace("gplots") #For a simple venn diagram
requireNamespace("VennDiagram")

#####################################
## @knitr DeclareGlobals
options(show.signif.stars=F) #Turn off the annotations on p-values

pathInputLong <- "./Data/Derived/Unpacked.csv"
pathOutputGenesByBasin <- "./Data/Derived/GeneByBasin.csv"
pathVennDirectory <- "./Analysis/GeneticOverlap/Figures"
pathVennTotal <- file.path(pathVennDirectory, "AllCategories.png")

basinOrder <- c("Illinois", "CookInlet", "Powder")
geochipBasinVersion <- c("CookInlet"=3.2,  "Illinois"=4.0, "Powder"=4.0)

paletteBasinDark <- scales::muted(c("Illinois"="#dcebce", "CookInlet"="#fedcd4", "Powder"="#e4e2ee"), l=80, c=60) #Matches Figs #1 & #2, but unmuted)

# dput(unique(dsLong$GeneCategory))
geneCategoryRecode <- c(
  "Antibiotic resistance"   = "AntibioticResistance",
  "Bacteria phage"          = "BacteriaPhage",
  "Bioleaching"             = "Bioleaching",
  "Carbon cycling"          = "CarbonCycling",
  "Methane Production"      = "MethaneProduction",
  "Energy process"          = "EnergyProcess",
  "Metal Resistance"        = "MetalResistance",
  "Nitrogen"                = "Nitrogen",
  "Organic Remediation"     = "OrganicRemediation",
  "other category"          = "Other",
  "Phosphorus"              = "Phosphorus",
  "Stress"                  = "Stress",
  "Sulfate Reduction"       = "SulfateReduction",
  "Sulfur Oxidation"        = "SulfurOxidation",
  "virulence"               = "Virulence",
  "Missing"                 = "Missing"
)


categoriesToDisplay <- c(
  "CarbonCycling", "MethaneProduction", "EnergyProcess", "MetalResistance",
  "Nitrogen", "OrganicRemediation", "Phosphorus", "SulfateReduction", "SulfurOxidation"
)

col_types <- readr::cols_only(
  GenbankID             = readr::col_character(),  # Force it to be read as a string.
  GeneName              = readr::col_character(),
  GeneCategory          = readr::col_character(),
  Replicate             = readr::col_integer(),
  Site                  = readr::col_integer(),
  Abundance             = readr::col_double(),
  Basin                 = readr::col_character(),
  AbundanceWithFloor    = readr::col_double()
)

#####################################
## @knitr LoadData
dsLong <- readr::read_csv(pathInputLong, col_types=col_types)

# sapply(dsLong, class)
#####################################
## @knitr TweakData
dsLong <- dsLong %>%
  dplyr::arrange(Basin, GeneCategory)

dsLong$GeochipVersion <- as.numeric(plyr::revalue(dsLong$Basin, replace=geochipBasinVersion))
dsLong$GeneCategory <- plyr::revalue(dsLong$GeneCategory, replace=geneCategoryRecode)

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
    CountV32   = scales::comma(sum(IsV32)),
    CountV40   = scales::comma(sum(!IsV32))
  )

dsLongBasinProbeCount <- dsLongBasinUnique %>%
  dplyr::group_by(GeneCategory) %>%
  dplyr::summarize(
    Total     = scales::comma(length(Basin)),
    Illinois  = scales::comma(sum(Basin=="Illinois")),
    CookInlet = scales::comma(sum(Basin=="CookInlet")),
    Powder    = scales::comma(sum(Basin=="Powder"))
  )
dsLongBasinProbeCount

#####################################
## @knitr PlotVennDiagrams
library(VennDiagram)  

plotVenn <- function( vennList, pathGraph, title_main ) {
  VennDiagram::venn.diagram(
    x           = vennList,
    filename    = pathGraph,
    main        = title_main,
    imagetype   = "png",
    height      = 1600,
    width       = 1600,
    resolution  = 600,
    cat.pos     = c(340, 20, 180),
    cat.dist    = c(0, 0, -.04),
    cat.col     = "gray40",
    cat.cex     = .8,
    cat.fontfamily= "sans serif",
    euler.d     = FALSE,
    scaled      = FALSE,
    col         = "transparent",
    fill        = paletteBasinDark,
    alpha       = 0.50,
    cex         = 1.4,
    fontface    = "bold",
    margin      = 0.0 
  )
}

vennListTotal <- list(
  "Illinois (v4.0)"   = dsLongBasinUnique[dsLongBasinUnique$Basin=="Illinois", ]$GenbankID,
  "Cook Inlet (v3.2)" = dsLongBasinUnique[dsLongBasinUnique$Basin=="CookInlet", ]$GenbankID,
  "Powder (v4.0)"     = dsLongBasinUnique[dsLongBasinUnique$Basin=="Powder", ]$GenbankID
)
plotVenn(vennListTotal, pathVennTotal, title_main = "All")

intersection_list <- list()# dplyr::n_distinct(dsLongBasinUnique$GeneCategory))
for( category in sort(unique(dsLongBasinUnique$GeneCategory)) ) {
  # category <- "CarbonCycling"
  pathVennCategory <- paste0(file.path(pathVennDirectory, category), ".png")
  message(pathVennCategory)
  
  vennListCategory <- list(
    "Illinois (V4.0)"   = dsLongBasinUnique[dsLongBasinUnique$Basin=="Illinois"  & dsLongBasinUnique$GeneCategory==category, ]$GenbankID,
    "Cook Inlet (V3.2)" = dsLongBasinUnique[dsLongBasinUnique$Basin=="CookInlet" & dsLongBasinUnique$GeneCategory==category, ]$GenbankID,
    "Powder (V4.0)"     = dsLongBasinUnique[dsLongBasinUnique$Basin=="Powder"    & dsLongBasinUnique$GeneCategory==category, ]$GenbankID
  )
  
  plotVenn(vennListCategory, pathVennCategory, title_main=category)
  
  #Create a list of data.frames
  intersection_list[[category]] <- data.frame(
    GenbankID = vennListCategory[[1]] %>% 
      intersect(vennListCategory[[2]]) %>% 
      intersect(vennListCategory[[3]])
  )
}

gplots::venn(vennListTotal)


#####################################
## @knitr DetermineIntersection

ds_intersection <- dplyr::bind_rows(intersection_list, .id="category")
ds_long_intersection_basin <- dsLong %>% 
  tibble::as_tibble() %>% 
  dplyr::right_join(ds_intersection, by="GenbankID" ) %>% 
  dplyr::group_by(GeneCategory, GenbankID, Basin) %>% 
  dplyr::summarize(
    unique_site_count    = dplyr::n_distinct(Site)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join( #Force it to take the V4.0 GeneName.  Sometimes the 3.2 and 4.0 versions differ.
    dsLong %>% 
      dplyr::filter(!IsV32) %>% 
      dplyr::distinct(GenbankID, GeneName),
    by="GenbankID"
  ) %>% 
  tidyr::spread(key=Basin, value=unique_site_count) %>% 
  dplyr::filter(GeneCategory %in% categoriesToDisplay)

# ---- display-table ----------------------------------------------------------
# webshot::install_phantomjs()
DT::datatable(
  data         = ds_long_intersection_basin,
  filter       = "bottom",
  caption      = "Site Count for Genes Found in All Three Basins",
  escape       = FALSE,
  options      = list(pageLength=20, dom = 'tip')
)

ds_long_intersection_basin %>% 
  # head(20) %>%
  knitr::kable(
    x           = .,
    #col.names   = c("Model", "Year", "Referral Out (children)", "referral out (adults)", "asq3", "asqse", "audio refer", "visual screen", "edinburgh total", "violence screen", "injury education"),
    col.names   = c("Gene Category", "Genbank ID", "Gene Name", "Illinois Site Count", "Cook Inlet Site Count", "Powder Site Count"),
    align       = c("l", "r", "l", "r", "r", "r"),
    row.names   = FALSE,
    format      = "markdown"
  )
    

readr::write_csv(ds_long_intersection_basin, pathOutputGenesByBasin)
# sum(ds_long_intersection_basin$GeneCategory=="CarbonCycling")
