# knitr::stitch_rmd(script="./UtilityScripts/Reproduce.R", output="./UtilityScripts/Reproduce.md")

###################################
#'  ---Reproducible Research---
###################################
#' When executed by R, this file will manipulate the original data sources (ie, ZZZZ)
#' to produce a groomed dataset suitable for analysis and graphing.

###################################
#' Clear memory from previous runs
base::rm(list=base::ls(all=TRUE))

###################################
#' Verify the working directory has been set correctly.  Much of the code assumes the working directory is the repository's root directory.
if( base::basename(base::getwd()) != "LylesCoalbedMethane2" ) {
  base::stop("The working directory should be set to the root of the package/repository.  ",
       "It's currently set to `", base::getwd(), "`.")
}
###################################
#' Install the necessary packages.
pathInstallPackages <- "./UtilityScripts/InstallPackages.R"
if( !file.exists(pathInstallPackages)) {
  base::stop("The file `", pathInstallPackages, "` was not found.  Make sure the working directory is set to the root of the repository.")
}
base::source(pathInstallPackages, local=new.env()) 

base::rm(pathInstallPackages)
###################################
#' Load the necessary packages.
base::library(base)
base::library(knitr)
base::library(markdown)
base::library(testit)

######################################################################################################
#' The following example comes from https://github.com/wibeasley/Wats.  Rename the paths appropriately.
#' 
#' 

###################################
#' Declare the paths of the necessary files.

#' The raw/input data files:
csvPackedIllinoisPath <- "./Data/Raw/IllinoisBasin.csv"
csvPackedCookInlet1Path <- "./Data/Raw/CookInlet1.csv"
csvPackedCookInlet2Path <- "./Data/Raw/CookInlet2.csv"
csvPackedPowderPath <- "./Data/Raw/PowderRiver.csv"

#' Code Files:
pathManipulate <- "./Manipulate/Manipulate.R"
pathGroomLong <- "./Manipulate/GroomLong.R"
pathLongToWide <- "./Manipulate/LongToWide.R"

#' Report Files:
# pathsReports <- base::file.path("./vignettes", c("MbrFigures.Rmd", "OkFertilityWithIntercensalEstimates.Rmd"))

###################################
#' Verify the necessary path can be found.

#' The raw/input data files:
testit::assert(base::file.exists(csvPackedIllinoisPath))
testit::assert(base::file.exists(csvPackedCookInlet1Path))
testit::assert(base::file.exists(csvPackedCookInlet2Path))
testit::assert(base::file.exists(csvPackedPowderPath))

#' Code Files:
testit::assert(base::file.exists(pathManipulate))
testit::assert(base::file.exists(pathGroomLong))
testit::assert(base::file.exists(pathLongToWide))

#' Report Files:
# testit::assert("The knitr Rmd files should exist.", base::file.exists(pathsReports))


####################################
#' Run the files that manipulate and analyze.

#' Execute code that restructures the data
knitr::stitch_rmd(script=pathManipulate, output="./Manipulate/StitchedOutput/Manipulate.md")
knitr::stitch_rmd(script=pathGroomLong, output="./Manipulate/StitchedOutput/GroomLong.md")
knitr::stitch_rmd(script=pathLongToWide, output="./Manipulate/StitchedOutput/LongToWide.md")

#' Assert that the intermediate files exist (the two files produced by `IsolateCensusPopsForGfr.R`)
# testit::assert("The yearly records should exist.", base::file.exists(pathCensusYearly))
# testit::assert("The monthly records should exist.", base::file.exists(pathCensusMonthly))

#' Execute code that combines the census and birth count data.
# base::source(pathCalculateGfr, local=base::new.env())

#' Verify that the two human readable datasets are present.
# testit::assert("The CSV for the 2005 Version should exist.", base::file.exists(pathDataForAnalaysis2005))
# testit::assert("The CSV for the 2014 Version should exist.", base::file.exists(pathDataForAnalaysis2014))

####################################
#' Build the reports
# for( pathRmd in pathsReports ) {
#   pathMd <- base::gsub(pattern=".Rmd$", replacement=".md", x=pathRmd)
#   pathHtml <- base::gsub(pattern=".Rmd$", replacement=".html", x=pathRmd)
#   knitr::knit(input=pathRmd, output=pathMd)
#   markdown::markdownToHTML(file=pathMd, output=pathHtml)
# }
