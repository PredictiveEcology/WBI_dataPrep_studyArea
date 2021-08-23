defineModule(sim, list(
  name = "WBI_dataPrep_studyArea",
  description = paste("this module prepares 3 sets of objects needed for fireSense in the WBI:",
                      "1. study areas and corresponding rasterToMatch (as well as large versions",
                      "2. species equivalencies tables and the sppEquiv column",
                      "3. projected and historical climate data for fitting and predicting fires.",
                      "Each is customized to the study area parameter passed as studyAreaName."),
  keywords = "",
  authors = c(
    person("Ian", "Eddy", email = "ian.eddy@canada.ca", role = "aut"),
    person("Alex M", "Chubaty", email = "achubaty@for-cast.ca", role = "aut")
  ),
  childModules = character(0),
  version = list(LandR = "0.0.11.9002"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "WBI_dataPrep_studyArea.Rmd")),
  reqdPkgs = list("magrittr", "raster", "sf", "sp",
                  "jimhester/archive",
                  "PredictiveEcology/reproducible@development (>= 1.2.6.9008)",
                  "PredictiveEcology/fireSenseUtils@development (>= 0.0.4.9014)",
                  "PredictiveEcology/LandR@development",
                  "PredictiveEcology/climateData@development"),
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
    defineParameter("bufferDist", "numeric", 20000, NA, NA,
                    "Distance (m) to buffer studyArea and rasterToMatch when creating 'Large' versions."),
    defineParameter("climateSSP", "numeric", 370, NA, NA,
                    "SSP emissions scenario for CanESM5: one of 245, 370, or 585."),
    defineParameter("historicalFireYears", "numeric", default = 1991:2020, NA, NA,
                    desc = "range of years captured by the historical climate data"),
    defineParameter("projectedFireYears", "numeric", default = 2011:2100, NA, NA,
                    desc = "range of years captured by the projected climate data"),
    defineParameter("studyAreaName", "character", "RIA", NA, NA,
                    paste("study area name for WB project - one of BC, AB, SK, YK, NWT, MB, or RIA"))
  ),
  inputObjects = bindrows(
  ),
  outputObjects = bindrows(
    createsOutput("historicalClimateRasters", objectClass = "list",
                  desc = "list of a single raster stack - historical MDC calculated from ClimateNA data"),
    createsOutput("projectedClimateRasters", objectClass = "list",
                  desc = "list of a single raster stack - projected MDC calculated from ClimateNA data"),
    createsOutput("rasterToMatch", objectClass = "RasterLayer",
                  desc = "template raster"),
    createsOutput("rasterToMatchLarge", objectClass = "RasterLayer",
                  desc = "template raster for larger area"),
    createsOutput("rasterToMatchReporting", objectClass = "RasterLayer",
                  desc = "template raster for reporting area"),
    createsOutput("sppColorVect", objectClass = "character",
                  desc = "species colours for plotting"),
    createsOutput("sppEquiv", objectClass = "character",
                  desc = "table of LandR species names equivalencies"),
    createsOutput("sppEquivCol", objectClass = "character",
                  desc = "name of column to use in sppEquiv"),
    createsOutput("standAgeMap2011", objectClass = "RasterLayer",
                  desc = "time since disturbance raster for year 2011"),
    createsOutput("studyArea", objectClass = "SpatialPolygonsDataFrame",
                  desc = "study area used for simulation (buffered to mitigate edge effects)"),
    createsOutput("studyAreaLarge", objectClass = "SpatialPolygonsDataFrame",
                  desc = "study area used for module parameterization (buffered)"),
    createsOutput("studyAreaReporting", objectClass = "SpatialPolygonsDataFrame",
                  desc = "study area used for reporting/post-processing")
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
    warning(paste("Undefined event type: \"", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  cacheTags <- c(P(sim)$studyAreaName, currentModule(sim))

  if (!P(sim)$climateSSP %in% c(245, 370, 585)) {
    stop("Invalid SSP scenario for CanESM5 climate model.\n",
         "climateSSP should be one of '245', '370', or '585'.")
  }

  #### Prep study-area specific objects ####
  ## when adding study areas, add relevant climate urls, rtm and sa, and don't forget R script prepSppEquiv
  allowedStudyAreas <- c("AB", "BC", "MB", "NT", "NU", "SK", "YT", ## prov/terr x BCR intersections
                         "RIA") ## custom boundaries
  provs <- c("British Columbia", "Alberta", "Saskatchewan", "Manitoba")
  terrs <- c("Yukon", "Northwest Territories", "Nunavut")
  WB <- c(provs, terrs)

  targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                     "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

  bcrzip <- "https://www.birdscanada.org/download/gislab/bcr_terrestrial_shape.zip"

  bcrshp <- Cache(prepInputs,
                  url = bcrzip,
                  destinationPath = dPath,
                  targetCRS = targetCRS,
                  useCache = P(sim)$.useCache,
                  fun = "sf::st_read")

  if (packageVersion("reproducible") >= "1.2.5") {
    fn1 <- function(x) {
      x <- readRDS(x)
      x <- st_as_sf(x)
      st_transform(x, targetCRS)
    }
  } else {
    fn1 <- "readRDS"
  }
  canProvs <- Cache(prepInputs,
                    "GADM",
                    #fun = "base::readRDS",
                    fun = fn1,
                    dlFun = "raster::getData",
                    country = "CAN", level = 1, path = dPath,
                    #targetCRS = targetCRS, ## TODO: fails on Windows
                    targetFile = "gadm36_CAN_1_sp.rds", ## TODO: this will change as GADM data update
                    destinationPath = dPath,
                    useCache = P(sim)$.useCache) #%>%
  if (packageVersion("reproducible") < "1.2.5") {
    canProvs <- st_as_sf(canProvs) %>%
      st_transform(., targetCRS)
  }

  bcrWB <- bcrshp[bcrshp$BCR %in% c(4, 6:8), ]
  provsWB <- canProvs[canProvs$NAME_1 %in% WB, ]

  WBstudyArea <- Cache(postProcess, provsWB, studyArea = bcrWB, useSAcrs = TRUE,
                       useCache = P(sim)$.useCache,
                       filename2 = NULL, overwrite = TRUE) %>%
    as_Spatial(.)

  ## all species considered in western boreal (will be subset later)
  data("sppEquivalencies_CA", package = "LandR", envir = environment())
  allWBIspp <- c("Abie_Bal", "Abie_Las", "Betu_Pap", "Lari_Lar",
                 "Pice_Eng", "Pice_Gla", "Pice_Mar",
                 "Pinu_Ban", "Pinu_Con", "Popu_Tre")
  sppEquiv <- sppEquivalencies_CA[KNN %in% allWBIspp]
  wbiSppToUse <- data.table(
    LandR = sppEquiv[, LandR],
    BC = c(FALSE, TRUE,  TRUE, TRUE, TRUE,  TRUE, TRUE, FALSE, TRUE,  TRUE),
    AB = c(TRUE,  TRUE,  TRUE, TRUE, FALSE, TRUE, TRUE, TRUE,  TRUE,  TRUE), # Pice_eng?
    SK = c(TRUE,  FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE,  FALSE, TRUE),
    MB = c(TRUE,  FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE,  FALSE, TRUE),
    YT = c(FALSE, TRUE,  TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE,  TRUE),
    NT = c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE,  FALSE, TRUE), ## run with NU, so needs to be same
    NU = c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE,  FALSE, TRUE)  ## run with NT, so needs to be same
  )
  sAN <- if (studyAreaName == "RIA") "BC" else studyAreaName
  sim$sppEquiv <- sppEquiv[which(wbiSppToUse[, ..sAN][[1]]), ] ## subset per study area
  sim$sppEquivCol <- "LandR"
  rm(sppEquivalencies_CA)

  ## studyArea-specific shapefiles and rasters
  if (grepl("RIA", P(sim)$studyAreaName)) {
    studyAreaUrl <- "https://drive.google.com/file/d/1LxacDOobTrRUppamkGgVAUFIxNT4iiHU/"
    ## originally, I thought this could be defined after the IF clause as Eliot suggested.
    ## But if RIA SA = SAL, or RTM = RTML, it falls apart.
    studyAreaNameLong <- "RIA"
    sim$studyArea <- Cache(prepInputs, url = studyAreaUrl,
                           destinationPath = dPath,
                           userTags = c("studyArea", cacheTags)) %>%
      sf::st_as_sf(.) %>%
      .[.$TSA_NUMBER %in% c("40", "08", "41", "24", "16"),] %>%
      sf::st_buffer(., 0) %>%
      sf::as_Spatial(.) %>%
      raster::aggregate(.)

    demUrl <- "https://drive.google.com/file/d/13sGg1X9DEOSkedg1m0PxcdJiuBESk072/"
    historicalClimateUrl <- "https://drive.google.com/file/d/1aO1nUnVrDqWkkDct05qfToay_1KL8P7Q/"
    projectedClimateUrl <- if (P(sim)$climateSSP == 245) {
      ""
    } else if (P(sim)$climateSSP == 370) {
      ""
    } else if (P(sim)$climateSSP == 585) {
      ""
    }
  } else if (grepl("AB", P(sim)$studyAreaName)) {
    studyAreaNameLong <- "Alberta"
    sim$studyArea <- WBstudyArea[WBstudyArea$NAME_1 == studyAreaNameLong, ]
    demUrl <- "https://drive.google.com/file/d/1g1SEU65zje6686pQXQzVVQQc44IXaznr/"
    historicalClimateUrl <- "https://drive.google.com/file/d/1we9GqEVAORWLbHi3it66VnCcvLu85QIk/"
    projectedClimateUrl <- if (P(sim)$climateSSP == 245) {
      "https://drive.google.com/file/d/1t66suhaHD7ePOagSO7VwyqNuAsqln5tD/"
    } else if (P(sim)$climateSSP == 370) {
      "https://drive.google.com/file/d/1HORzYTzhR1ICbKm_tM6vjzePWI684D52/"
    } else if (P(sim)$climateSSP == 585) {
      "https://drive.google.com/file/d/17Ol-1Ut1YX_MciDSnIkZwsEqq94rjfGm/"
    }
  } else if (grepl("BC", P(sim)$studyAreaName)) {
    studyAreaNameLong <- "British Columbia"
    sim$studyArea <- WBstudyArea[WBstudyArea$NAME_1 == studyAreaNameLong, ]
    demUrl <- "https://drive.google.com/file/d/1DaAYFr0z38qmbZcz452QPMP_fzwUIqLD/"
    historicalClimateUrl <- "https://drive.google.com/file/d/1usLRFsv2e4OkkfxKqZ7DA_7Go1CyvEUk/"
    projectedClimateUrl <- if (P(sim)$climateSSP == 245) {
      "https://drive.google.com/file/d/1UQo62wCV3TJdV8zKGMg8rqtlIckPsqos/"
    } else if (P(sim)$climateSSP == 370) {
      "https://drive.google.com/file/d/1Nr5v1IK-wX7erJ4IBui2elwEusOBV2G2/"
    } else if (P(sim)$climateSSP == 585) {
      "https://drive.google.com/file/d/10id-iJyvYdIc85EwWJ0SYL5nRTsjQbp_/"
    }
  } else if (grepl("MB", P(sim)$studyAreaName)) {
    studyAreaNameLong <- "Manitoba"
    sim$studyArea <- WBstudyArea[WBstudyArea$NAME_1 == studyAreaNameLong, ]
    demUrl <- "https://drive.google.com/file/d/1X7b2CE6QyCvik3UG9pUj6zc4b5UYZi8w/"
    historicalClimateUrl <- "https://drive.google.com/file/d/1bywYpz5kF4KBJUjARTx4WLcFS5Sij74l/"
    projectedClimateUrl <- if (P(sim)$climateSSP == 245) {
      "https://drive.google.com/file/d/1hauzy3WXzHrlbEc_FsdjVB0ZaDUZpeFb/"
    } else if (P(sim)$climateSSP == 370) {
      "https://drive.google.com/file/d/1GaC_4ws_LQ5cHKOJElxdtBuM6yohQUHE/"
    } else if (P(sim)$climateSSP == 585) {
      "https://drive.google.com/file/d/1ItKXfR8mEKcbgWIcdrhoG8Pgy7EAL-jm/"
    }
  } else if (grepl("NT|NU", P(sim)$studyAreaName)) {
    ## NOTE: run NT and NU together!
    message("NWT and NU will both be run together as a single study area.")
    studyAreaNameLong <- "Northwest Territories & Nunavut"
    sim$studyArea <- WBstudyArea[WBstudyArea$NAME_1 %in% c("Northwest Territories", "Nunavut"), ]
    demUrl <- "https://drive.google.com/file/d/13n8LjQJihy9kd3SniS91EuXunowbWOBa/"
    historicalClimateUrl <- "https://drive.google.com/file/d/1rZDW2lB9jUZISsSmKEFQYJJlXik8qaXT/"
    projectedClimateUrl <- if (P(sim)$climateSSP == 245) {
      "https://drive.google.com/file/d/1tXgQDJutWYHPwuNUMzbHK28Qn1SR05iC/"
    } else if (P(sim)$climateSSP == 370) {
      "https://drive.google.com/file/d/1prfyn7gyZQ5FWQvFaw7nHfgoa2m9YY33/"
    } else if (P(sim)$climateSSP == 585) {
      "https://drive.google.com/file/d/1Ob7qGMldjSQrAbQy-QIiIUg1-z98C1jS/"
    }
  } else if (grepl("SK", P(sim)$studyAreaName)) {
    studyAreaNameLong <- "Saskatchewan"
    sim$studyArea <- WBstudyArea[WBstudyArea$NAME_1 == studyAreaNameLong, ]
    demUrl <- "https://drive.google.com/file/d/1CooPdqc3SlVVU7y_BaPfZD0OXt42fjBC/"
    historicalClimateUrl <- "https://drive.google.com/file/d/1JikmJLdJhRsau3SysijLVQTAhkA6vtT8/"
    projectedClimateUrl <- if (P(sim)$climateSSP == 245) {
      "https://drive.google.com/file/d/18g7yPwcUeBp9mpzQwv8QBmICRqtFTYn4/"
    } else if (P(sim)$climateSSP == 370) {
      "https://drive.google.com/file/d/1n9yCqSk2QVindyyisXTK6-m-i03UzT6O/"
    } else if (P(sim)$climateSSP == 585) {
      "https://drive.google.com/file/d/1bmBaBFs8yVTIcAuL2ZhnPUOmGoakCE0b/"
    }
  } else if (grepl("YT", P(sim)$studyAreaName)) {
    studyAreaNameLong <- "Yukon"
    sim$studyArea <- WBstudyArea[WBstudyArea$NAME_1 == studyAreaNameLong, ]
    demUrl <- "https://drive.google.com/file/d/1CUMjLFGdGtwaQlErQ0Oq89ICUCcX641Q/"
    historicalClimateUrl <- "https://drive.google.com/file/d/1V8pMe2x-M36qoLpZuOlWlAsY4N9Gkm5X/"
    projectedClimateUrl <- if (P(sim)$climateSSP == 245) {
      "https://drive.google.com/file/d/1GGzORMtsxwUUwC9uBW9Hh4xNkdpRnaTn/"
    } else if (P(sim)$climateSSP == 370) {
      "https://drive.google.com/file/d/1wumRrz9GcsJ8P33UkmkL_d5sboLpLx1C/"
    } else if (P(sim)$climateSSP == 585) {
      "https://drive.google.com/file/d/1xe46diIGkah8X7AkmX1clmRwuGCIJjCh/"
    }
  } else {
    stop("studyAreaName must be one of: ", paste(allowedStudyAreas, collapse = ", "))
  }

  sim$studyArea <- spTransform(sim$studyArea, targetCRS)
  sim$studyArea$studyAreaName <- P(sim)$studyAreaName  # makes it a data.frame
  sim$studyAreaReporting <- sim$studyArea

  if (grepl("RIA", P(sim)$studyAreaName)) {
    ## NOTE: RIA uses the same unbuffered studyArea for parameterization, sims, and reporting
    sim$studyAreaLarge <- sim$studyArea
  } else {
    ## NOTE: studyArea and studyAreaLarge are the same [buffered] area
    sim$studyArea <- buffer(sim$studyArea, P(sim)$bufferDist)
    sim$studyAreaLarge <- sim$studyArea
  }

  sim$rasterToMatch <- Cache(LandR::prepInputsLCC,
                             year = 2005,
                             studyArea = sim$studyArea,
                             destinationPath = dPath,
                             useCache = P(sim)$.useCache,
                             overwrite = TRUE,
                             filename2 = paste0(P(sim)$studyAreaName, "_rtm.tif"))
  #sim$rasterToMatch[] <- sim$rasterToMatch[] ## bring raster to memory
  sim$rasterToMatchLarge <- sim$rasterToMatch

  # This was raster::mask -- but that sometimes doesn't work because of incorrect dispatch that
  #  conflicts with devtools::load_all("reproducible")
  sim$rasterToMatchReporting <- Cache(maskInputs, sim$rasterToMatch, sim$studyAreaReporting)

  ## Paired handles 12 colours so it is safer compared to Accent's 8 max
  sim$sppColorVect <- LandR::sppColors(sppEquiv = sim$sppEquiv, sppEquivCol = sim$sppEquivCol,
                                       palette = "Paired")

  stopifnot(getOption("reproducible.useNewDigestAlgorithm") == 2)

  ## get pre-made DEM to use with climate data
  dem <- Cache(prepInputs, url = demUrl, destinationPath = dPath,
               fun = "raster::raster",
               useCache = P(sim)$.useCache,
               userTags = c(paste0("DEM_", P(sim)$studyAreaName), cacheTags))

  ## HISTORIC CLIMATE DATA
  historicalClimatePath <- checkPath(file.path(dPath, "climate", "historic"), create = TRUE)
  historicalClimateArchive <- file.path(historicalClimatePath, paste0(studyAreaNameLong, ".zip"))
  historicalMDCfile <- file.path(historicalClimatePath, paste0("MDC_historical_", studyAreaName, ".grd"))

  ## need to download and extract w/o prepInputs to preserve folder structure!
  if (!file.exists(historicalClimateArchive)) {
    googledrive::drive_download(file = as_id(historicalClimateUrl), path = historicalClimateArchive)
  }
  archive::archive_extract(historicalClimateArchive, historicalClimatePath)

  historicalMDC <- Cache(makeMDC,
                         inputPath = file.path(historicalClimatePath, studyAreaNameLong),
                         years = P(sim)$historicalFireYears)
  historicalMDC <- Cache(postProcess,
                         historicalMDC,
                         rasterToMatch = sim$rasterToMatch,
                         studyArea = sim$studyArea,
                         datatype = "INT2U",
                         method = "bilinear",
                         filename2 = historicalMDCfile,
                         useCache = P(sim)$.useCache,
                         quick = "filename2", # Cache treats filenames as files; so it digests the file as an input
                         userTags = c(paste0("histMDC_", P(sim)$studyAreaName), cacheTags))

  ## The names need "year" at the start, because not every year will have fires (data issue in RIA),
  ## so fireSense matches fires + climate rasters by year.
  ## WARNING: names(historicalMDC) <- paste0('year', P(sim)$historicalFireYears) # Bad
  ##          |-> allows for index mismatching
  historicalMDC <- updateStackYearNames(historicalMDC, Par$historicalFireYears)
  #historicalMDC[] <- historicalMDC[] ## bring raster to memory

  sim$historicalClimateRasters <- list("MDC" = historicalMDC)

  ## FUTURE CLIMATE DATA
  projectedClimatePath <- checkPath(file.path(dPath, "climate", "future"), create = TRUE)
  projectedClimateArchive <- file.path(projectedClimatePath, paste0(studyAreaNameLong, ".zip"))
  projectedMDCfile <- file.path(projectedClimatePath,
                                paste0("MDC_future_CanESM5_ssp", P(sim)$climateSSP, "_", studyAreaName, ".grd"))

  ## need to download and extract w/o prepInputs to preserve folder structure!
  if (!file.exists(projectedClimateArchive)) {
    googledrive::drive_download(file = as_id(projectedClimateUrl), path = projectedClimateArchive)
  }
  archive::archive_extract(projectedClimateArchive, projectedClimatePath)

  projectedMDC <- Cache(makeMDC,
                        inputPath = file.path(projectedClimatePath, studyAreaNameLong),
                        years = P(sim)$projectedFireYears)
  projectedMDC <- Cache(postProcess,
                        projectedMDC,
                        rasterToMatch = sim$rasterToMatch,
                        studyArea = sim$studyArea,
                        datatype = "INT2U",
                        method = "bilinear",
                        filename2 = projectedMDCfile,
                        useCache = P(sim)$.useCache,
                        quick = "filename2", # Cache treats filenames as files; so it digests the file as an input
                        userTags = c(paste0("histMDC_", P(sim)$studyAreaName), cacheTags))

  ## WARNING: names(projectedMDC) <- paste0('year', P(sim)$projectedFireYears) # Bad
  ##          |-> allows for index mismatching
  projectedMDC <- updateStackYearNames(projectedMDC, Par$projectedFireYears)
  #projectedMDC[] <- projectedMDC[] ## bring raster to memory

  sim$projectedClimateRasters <- list("MDC" = projectedMDC)

  sim$standAgeMap2011 <- Cache(
    LandR::prepInputsStandAgeMap,
    ageURL = paste0("https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
                    "canada-forests-attributes_attributs-forests-canada/",
                    "2011-attributes_attributs-2011/",
                    "NFI_MODIS250m_2011_kNN_Structure_Stand_Age_v1.tif"),
    rasterToMatch = sim$rasterToMatchLarge,
    studyArea = sim$studyAreaLarge,
    destinationPath = dPath,
    startTime = 2011,
    filename2 = .suffix("standAgeMap_2011.tif", paste0("_", P(sim)$studyAreaName)),
    userTags = c("prepInputsStandAgeMap", P(sim)$studyAreaname)
  )

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
