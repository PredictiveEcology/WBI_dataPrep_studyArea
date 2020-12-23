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
    defineParameter("historicalFireYears", 'numeric', default = 1991:2019, NA, NA,
                    desc = 'range of years captured by the historical climate data'),
    defineParameter("projectedFireYears", 'numeric', default = 2011:2100, NA, NA,
                    desc = 'range of years captured by the projected climate data'),
    defineParameter("studyAreaName", 'character', 'RIA', NA, NA,
                    paste("study area name for WB project - one of BC, AB, SK, YK, NWT, MB, or RIA"))
  ),
  inputObjects = bindrows(
  ),
  outputObjects = bindrows(
    createsOutput(objectName = 'historicalClimateRasters', objectClass = 'list',
                  desc = 'list of a single raster stack - historical MDC calculated from ClimateNA data'),
    createsOutput(objectName = 'projectedClimateRasters', objectClass = 'list',
                  desc = 'list of a single raster stack - projected MDC calculated from ClimateNA data'),
    createsOutput(objectName = 'rasterToMatch', objectClass = 'RasterLayer', desc = 'template raster'),
    createsOutput(objectName = 'rasterToMatchLarge', objectClass = 'RasterLayer', desc = 'template raster for larger area'),
    createsOutput(objectName = 'sppEquiv', objectClass = 'data.table', desc = 'species equivalencies object'),
    createsOutput(objectName = 'studyArea', objectClass = 'SpatialPolygonsDataFrame', desc = 'study area shapefile'),
    createsOutput(objectName = 'studyAreaLarge', objectClass = 'SpatialPolygonsDataFrame', desc = 'study area large shapefile'),
    createsOutput(objectName = 'sppEquivCol', objectClass = 'character', desc = 'column that determines species names in LandR'),
    createsOutput(objectName = 'sppColorVect', objectClass = 'character', desc = 'species colours for plotting'),
    createsOutput(objectName = 'standAgeMap2011', objectClass = 'RasterLayer', desc = 'KNN stand age map in 2011')
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

  dPath <- file.path('modules', currentModule(sim), 'data') ## TODO: allow use of 'inputs'
  cacheTags <- c(P(sim)$studyAreaName, currentModule(sim))

  #### Prep studa-area specific objects####
  #when adding study areas, add relevant climate urls, rtm and sa, and dont forget R script prepSppEquiv
  if (grepl('RIA', P(sim)$studyAreaName)) {

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
                                              useCache = P(sim)$.useCache,
                                              overwrite = TRUE,
                                              filename2 = paste0(P(sim)$studyAreaName, '_rtm.tif'))
    sim$studyArea <- spTransform(sim$studyArea, crs(sim$rasterToMatch))
    sim$rasterToMatchLarge <- sim$rasterToMatch
    sim$studyAreaLarge <- sim$studyArea #for now.. other SA/SAL will likely be different

    #2. get species objects - putting this in a script as it might be long with 7 study Areas
    sim$sppEquivCol <- 'RIA'

    #3. get climate objects urls - projectedMDC and historicalMDC
    projectedClimateUrl <- 'https://drive.google.com/file/d/1ErQhfE5IYGRV_2voeb5iStWt_h2D5cV3/view?usp=sharing'
    historicalClimateUrl <- 'https://drive.google.com/file/d/1vQXi10thWsDyLW-tu300ZMG655tHyE_-/view?usp=sharing'

  } else {
    stop("no other study areas at the moment :( ")
  }

  #### get study area objects ####
  data("sppEquivalencies_CA", package = "LandR")
  sim$sppEquiv <- prepSppEquiv(studyArea = P(sim)$studyAreaName, sppEquiv = sppEquivalencies_CA)


  #Paired handles 12 colours so it is safer compared to Accent's 8 max
  sim$sppColorVect <- LandR::sppColors(sppEquiv = sim$sppEquiv, sppEquivCol = sim$sppEquivCol, palette = 'Paired')

  #TODO: fix postProcess or .prepareFileBackedRaster, or amend this code once postProcess is handling stacks of file-backed rasters properly
  historicalMDC <- prepInputs(url = historicalClimateUrl,
                              destinationPath = dPath,
                              # rasterToMatch = sim$rasterToMatch,
                              # studyArea = sim$studyArea,
                              fun = 'raster::stack',
                              filename2 = file.path(dPath, paste0(P(sim)$studyAreaName, '_histClim.grd')),
                              useCache = P(sim)$.useCache,
                              userTags = c("histMDC", cacheTags))

  historicalMDC <- Cache(raster::projectRaster, historicalMDC, to = sim$rasterToMatch,
                         datatype = 'INT2U',
                         userTags = c("reprojHistoricClimateRasters"))

  historicalMDC <- Cache(raster::mask, historicalMDC, sim$studyArea,
                         userTags = c("maskHistoricClimateRasters"),
                         filename = file.path(dPath, paste0(P(sim)$studyAreaName, '_histMDC.grd')),
                         overwrite = TRUE)

  #The names need 'year' at the start.
  #The reason is not every year will have fires (data issue in RIA), so fireSense matches fires + climate rasters by year.
  historicalMDC <- updateStackYearNames(historicalMDC, Par$historicalFireYears)
  # names(historicalMDC) <- paste0('year', P(sim)$historicalFireYears) # Bad -- allows for index mismatching
  sim$historicalClimateRasters <- list('MDC' = historicalMDC)
  #as long as the names aren't preserved, there may be problems naming
  projectedMDC <- prepInputs(url = projectedClimateUrl,
                             destinationPath = dPath,
                             # rasterToMatch = sim$rasterToMatch,
                             # studyArea = sim$studyArea,
                             fun = 'raster::stack',
                             filename2 = file.path(dPath, paste0(P(sim)$studyAreaName, '_projClim.grd')),
                             useCache = P(sim)$.useCache,
                             userTags = c("histMDC", cacheTags))

  projectedMDC <- Cache(raster::projectRaster, projectedMDC, to = sim$rasterToMatch,
                        datatype = 'INT2U',
                        userTags = c("reprojProjectedMDC"))

  projectedMDC <- Cache(raster::mask, projectedMDC, sim$studyArea,
                        userTags = c("maskProjectedClimateRasters"),
                        filename = file.path(dPath, paste0(P(sim)$studyAreaName, '_projMDC.grd')),
                        overwrite = TRUE)

  projectedMDC <- updateStackYearNames(projectedMDC, Par$projectedFireYears)
  # names(projectedMDC) <- paste0('year', P(sim)$projectedFireYears) # Bad -- allows for index mismatching
  sim$projectedClimateRasters <- list('MDC' = projectedMDC)


  sim$standAgeMap2011 <- Cache(
    LandR::prepInputsStandAgeMap, ageURL = paste0("https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
                                                  "canada-forests-attributes_attributs-forests-canada/",
                                                  "2011-attributes_attributs-2011/",
                                                  "NFI_MODIS250m_2011_kNN_Structure_Stand_Age_v1.tif"),
    rasterToMatch = sim$rasterToMatch,
    studyArea = sim$studyArea,
    destinationPath = dPath,
    startTime = 2011,
    filename2 = .suffix("standAgeMap2011.tif", paste0("_", P(sim)$studyAreaName)),
    userTags = c("prepInputsStandAgeMap", P(sim)$studyAreaname)
  )

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
updateStackYearNames <- function(annualDataStack, desiredYears, parameterName) {
  objectName <- as.character(substitute(annualDataStack))
  parameterName <- as.character(substitute(desiredYears))[[3]]
  # Sanity check -- both objects must have 4 length year  in them
  hasYearStack <- all(grepl(grepTest4DigitYear, names(annualDataStack)))
  if (!isTRUE(hasYearStack))
    stop("The raster stack, ",objectName,", ", namingConventionTxt)
  hasYearParam <- all(grepl(grepTest4DigitYear, desiredYears))
  if (!isTRUE(hasYearParam))
    stop("The parameter, ",parameterName,", ", namingConventionTxt)
  annualDataStackInt <- as.integer(gsub(".*([[:digit:]]{4,4}).*", "\\1", names(annualDataStack)))
  if (!identical(annualDataStackInt, desiredYears))
    warning("User has provided a set of years for annual fire via P(sim)$",parameterName,", ",
            "that does not match the input dataset of ", objectName, ":\n\n",
            "names(",objectName,"): ", paste(names(annualDataStack), collapse = ", "), "\n\n",
            "P(sim)$",parameterName,": ", paste(paste0('year', desiredYears), collapse = ", "),
            "\n\n", "using the overlapping years provided by both")

  whichannualDataStackToKeep <- which(annualDataStackInt %in% desiredYears)
  annualDataStackInt <- annualDataStackInt[whichannualDataStackToKeep]
  namesWithYearPrepended <- paste0("year", annualDataStackInt)

  names(annualDataStack) <- namesWithYearPrepended
  annualDataStack
}

grepTest4DigitYear <- "[[:digit:]]{4,4}"
namingConventionTxt <- "does not have names that include the 4 digit year. Please use that naming convention"
