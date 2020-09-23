## Everything in this file gets sourced during `simInit()`,
## and all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used without `sim$` as they are namespaced to the module,
## just like functions in R packages.
## If exact location is required, functions will be: `sim$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "WBI_dataPrep_studyArea",
  description = "",
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
    createsOutput(objectName = 'studyAreaLarge', objectClass = 'SpatialPolygonsDataFrame', desc = 'study area large shapefile')

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

  dPath <- file.path("data")
  cacheTags <- c(P(sim)$studyAreaName, currentModule(sim))
  if (P(sim)$studyAreaName == 'RIA') {

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
    #passing a custom function returns error (object 'studyAreaFun' not found)

    sim$rasterToMatch <- LandR::prepInputsLCC(studyArea = sim$studyArea,
                                              destinationPath = dPath,
                                              filename2 = paste0(P(sim)$studyAreaName, '_rtm.tif'))
    sim$studyArea <- spTransform(sim$studyArea, crs(sim$rasterToMatch))
    sim$rasterToMatchLarge <- sim$rasterToMatch
    sim$studyAreaLarge <- sim$studyArea #for now

    #get species objects - putting this in a script as it might be long with 7 study Areas
    data("sppEquivalencies_CA", package = "LandR")
    sppEquiv <- prepSppEquiv(studyArea = P(sim)$studyAreaName, sppEquiv = sppEquivalencies_CA)

    #get climate objects - projectedMDC and historicalMDC
    projectedClimateUrl <- 'https://drive.google.com/file/d/1ErQhfE5IYGRV_2voeb5iStWt_h2D5cV3/view?usp=sharing'
    historicalClimateUrl <- 'https://drive.google.com/file/d/1DtB2_Gftl4R7T4yM9-mjVCCXF5nBXKqD/view?usp=sharing'

  } else {
    stop("no other study areas at the moment :( ")
  }

  sim$historicalClimateRasters <- prepInputs(url = historicalClimateUrl,
                                             destinationPath = dPath,
                                             rasterToMatch = sim$rasterToMatch,
                                             studyArea = sim$studyArea,
                                             fun = 'raster::stack',
                                             useCache = TRUE,
                                             overwrite = TRUE,
                                             userTags = c("histMDC", cacheTags))

  sim$projectedClimateRasters <- prepInputs(url = projectedClimateUrl,
                                            destinationPath = dPath,
                                            fun = 'raster::stack',
                                            studyArea = sim$studyArea,
                                            rasterToMatch = sim$rasterToMatch,
                                            useCache = TRUE,
                                            overwrite = TRUE,
                                            userTags = c("projMDC", cacheTags))

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
