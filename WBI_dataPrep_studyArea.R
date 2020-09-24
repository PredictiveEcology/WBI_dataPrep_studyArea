## Everything in this file gets sourced during `simInit()`,
## and all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used without `sim$` as they are namespaced to the module,
## just like functions in R packages.
## If exact location is required, functions will be: `sim$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "WBI_dataPrep_studyArea",
  description = paste("this module prepares 3 sets of objects needed for fireSense in the WBI:",
                      "1. study areas and corresponding rasterToMatch (as well as large versions",
                      "2. species equivalencies tables and the sppEquiv column",
                      "3. projected and historical climate data for fitting and predicting fires.",
                      "Each is customized to the study area parameter passed as studyAreaName."),
  keywords = "",
  authors = structure(list(list(given = c("First", "Middle"), family = "Last", role = c("aut", "cre"), email = "email@example.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.4.9003", WBI_dataPrep_studyArea = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "WBI_dataPrep_studyArea.Rmd")),
  reqdPkgs = list('magrittr', 'raster', 'sf', 'PredictiveEcology/LandR@development'),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should this entire module be run with caching activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant")),
    defineParameter("studyAreaName", 'character', 'RIA', NA, NA,
                    paste("study area name for WB project - one of BC, AB, SK, YK, NWT, MB, or RIA"))
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    # expectsInput(objectName = NA, objectClass = NA, desc = NA, sourceURL = NA)
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = 'historicalClimateRasters', objectClass = 'RasterStack',
                  desc = 'historical MDC calculated from ClimateNA data'),
    createsOutput(objectName = 'projectedClimateRasters', objectClass = 'RasterStack',
                  desc = 'projected MDC calculated from ClimateNA data'),
    createsOutput(objectName = 'rasterToMatch', objectClass = 'RasterLayer', desc = 'template raster'),
    createsOutput(objectName = 'rasterToMatchLarge', objectClass = 'RasterLayer', desc = 'template raster for larger area'),
    createsOutput(objectName = 'sppEquiv', objectClass = 'data.table', desc = 'species equivalencies object'),
    createsOutput(objectName = 'studyArea', objectClass = 'SpatialPolygonsDataFrame', desc = 'study area shapefile'),
    createsOutput(objectName = 'studyAreaLarge', objectClass = 'SpatialPolygonsDataFrame', desc = 'study area large shapefile'),
    createsOutput(objectName = 'sppEquivCol', objectClass = 'character', desc = 'column that determines species names in LandR')

  )
))

## event types
#   - type `init` is required for initialization

doEvent.WBI_dataPrep_studyArea = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "WBI_dataPrep_studyArea", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "WBI_dataPrep_studyArea", "save")
    },
    plot = {

    },
    save = {

    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {

  dPath <- file.path('modules', currentModule(sim), 'data')
  cacheTags <- c(P(sim)$studyAreaName, currentModule(sim))
  if (P(sim)$studyAreaName == 'RIA') {

    #1. get rtm, rtml, sa, sal
    studyAreaUrl <- 'https://drive.google.com/file/d/1LxacDOobTrRUppamkGgVAUFIxNT4iiHU/view?usp=sharing'
    #originally, I thought this could be defined after the IF clause as Eliot suggested. But if RIA SA = SAL, or RTM = RTML, it falls apart
    sim$studyArea <- prepInputs(url = studyAreaUrl,
                                destinationPath = dPath,
                                userTags = c("studyArea", cacheTags)) %>%
      sf::st_as_sf(.) %>%
      .[.$TSA_NUMBER %in% c('40', '08', '41', '24', '16'),] %>%
      sf::st_buffer(., 0) %>%
      sf::as_Spatial(.) %>%
      raster::aggregate(.)
    sim$studyArea$studyAreaName <- "RIA"  #makes it a data.frame
    #FYI passing a custom function returns error (object 'studyAreaFun' not found even though its in quotes)
    sim$rasterToMatch <- LandR::prepInputsLCC(studyArea = sim$studyArea,
                                              destinationPath = dPath,
                                              filename2 = paste0(P(sim)$studyAreaName, '_rtm.tif'))
    sim$studyArea <- spTransform(sim$studyArea, crs(sim$rasterToMatch))
    sim$rasterToMatchLarge <- sim$rasterToMatch
    sim$studyAreaLarge <- sim$studyArea #for now

    #2. get species objects - putting this in a script as it might be long with 7 study Areas
    data("sppEquivalencies_CA", package = "LandR")
    sim$sppEquiv <- prepSppEquiv(studyArea = P(sim)$studyAreaName, sppEquiv = sppEquivalencies_CA)
    sim$sppEquivCol <- 'RIA'
    rm(sppEquivalencies_CA)


    #3. get climate objects urls - projectedMDC and historicalMDC
    projectedClimateUrl <- 'https://drive.google.com/file/d/1ErQhfE5IYGRV_2voeb5iStWt_h2D5cV3/view?usp=sharing'
    historicalClimateUrl <- 'https://drive.google.com/file/d/1DtB2_Gftl4R7T4yM9-mjVCCXF5nBXKqD/view?usp=sharing'

  } else {
    stop("no other study areas at the moment :( ")
  }

  #postProcses isn't handling stack - something with file names - either no stack name, or every raster has the same name.
  #either way it causes an error. tried omitting filename2 to no avail
  #TODO: fix postProcess or .prepareFileBackedRaster
  historicalClimateRasters <- prepInputs(url = historicalClimateUrl,
                                         destinationPath = dPath,
                                         # rasterToMatch = sim$rasterToMatch,
                                         # studyArea = sim$studyArea,
                                         fun = 'raster::stack',
                                         filename2 = paste0(P(sim)$studyAreaName, '_histClim.tif'),
                                         useCache = TRUE,
                                         userTags = c("histMDC", cacheTags))
  historicalClimateRasters <- Cache(raster::projectRaster, historicalClimateRasters, to = sim$rasterToMatch,
                                    userTags = c("reprojHistoricClimateRasters"))
  sim$historicalClimateRasters <- Cache(raster::mask, historicalClimateRasters, sim$studyArea,
                                        userTags = c("maskHistoricClimateRasters"))


  projectedClimateRasters <- prepInputs(url = projectedClimateUrl,
                                         destinationPath = dPath,
                                         # rasterToMatch = sim$rasterToMatch,
                                         # studyArea = sim$studyArea,
                                         fun = 'raster::stack',
                                         filename2 = paste0(P(sim)$studyAreaName, '_projClim.tif'),
                                         useCache = TRUE,
                                         userTags = c("histMDC", cacheTags))
  projectedClimateRasters <- Cache(raster::projectRaster, projectedClimateRasters, to = sim$rasterToMatch,
                                    userTags = c("reprojHistoricClimateRasters"))
  sim$projectedClimateRasters <- Cache(raster::mask, projectedClimateRasters, sim$studyArea,
                                        userTags = c("maskProjectedClimateRasters"))

  return(invisible(sim))
}

### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot(sim$object)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above
