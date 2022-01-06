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
  reqdPkgs = list("archive", "magrittr", "raster", "sf", "sp",
                  "PredictiveEcology/reproducible@development (>= 1.2.6.9008)",
                  "PredictiveEcology/fireSenseUtils@development (>= 0.0.4.9014)",
                  "PredictiveEcology/LandR@development",
                  "PredictiveEcology/climateData@development (>= 0.0.0.0.9002)"),
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
    defineParameter("climateGCM", "character", "CNRM-ESM2-1", NA, NA,
                    paste("Global Circulation Model to use for climate projections:",
                          "currently '13GCMs_ensemble', 'CanESM5', 'CNRM-ESM2-1', or 'CCSM4'.")),
    defineParameter("climateSSP", "numeric", 370, NA, NA,
                    "SSP emissions scenario for `climateGCM`: one of 245, 370, or 585.",
                    "[If using 'climateGCM = CCSM4', climateSSP must be one of 45 or 85.]"),
    defineParameter("historicalFireYears", "numeric", default = 1991:2020, NA, NA,
                    desc = "range of years captured by the historical climate data"),
    defineParameter("projectedFireYears", "numeric", default = 2011:2100, NA, NA,
                    desc = "range of years captured by the projected climate data"),
    defineParameter("studyAreaName", "character", "RIA", NA, NA,
                    paste("study area name for WB project - one of BC, AB, SK, YK, NWT, MB, or RIA"))
  ),
  inputObjects = bindrows(
    expectsInput("rasterToMatch", objectClass = "RasterLayer",
                 desc = "template raster", sourceURL = NA),
    expectsInput("rasterToMatchLarge", objectClass = "RasterLayer",
                 desc = "template raster for larger area", sourceURL = NA),
    expectsInput("rasterToMatchReporting", objectClass = "RasterLayer",
                 desc = "template raster for reporting area", sourceURL = NA),
    expectsInput("studyArea", objectClass = "SpatialPolygonsDataFrame",
                 desc = "study area used for simulation (buffered to mitigate edge effects)",
                 sourceURL = NA),
    expectsInput("studyAreaLarge", objectClass = "SpatialPolygonsDataFrame",
                 desc = "study area used for module parameterization (buffered)",
                 sourceURL = NA),
    expectsInput("studyAreaReporting", objectClass = "SpatialPolygonsDataFrame",
                 desc = "study area used for reporting/post-processing",
                 sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput("ATAstack", objectClass = "RasterStack",
                  desc = "annual projected mean annual temperature anomalies, units stored as tenth of a degree"),
    createsOutput("CMIstack", objectClass = "RasterStack",
                  desc = "annual projected mean climate moisture deficit"),
    createsOutput("CMInormal", objectClass = "RasterLayer",
                  desc = "Climate Moisture Index Normals from 1950-2010"),
    createsOutput("historicalClimateRasters", objectClass = "list",
                  desc = "list of a single raster stack - historical MDC calculated from ClimateNA data"),
    createsOutput("projectedClimateRasters", objectClass = "list",
                  desc = "list of a single raster stack - projected MDC calculated from ClimateNA data"),
    createsOutput("sppColorVect", objectClass = "character",
                  desc = "species colours for plotting"),
    createsOutput("sppEquiv", objectClass = "character",
                  desc = "table of LandR species names equivalencies"),
    createsOutput("sppEquivCol", objectClass = "character",
                  desc = "name of column to use in sppEquiv"),
    createsOutput("studyAreaPSP", objectClass = "SpatialPolygonsDataFrame",
                 desc = paste("this area will be used to subset PSP plots before building the statistical model.",
                              "Currently PSP datasets with repeat measures exist only for Saskatchewan,",
                              "Alberta, and Boreal British Columbia"))
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

  if (!P(sim)$climateGCM %in% c("13GCMs_ensemble", "CanESM5", "CNRM-ESM2-1", "CCSM4")) {
    stop("Invalid climate model specified.\n",
         "climateGCM should be one of '13GCMs_ensemble', 'CanESM5', 'CNRM-ESM2-1', or 'CCSM4'.")
  }

  if (!P(sim)$climateSSP %in% c(45, 85, 245, 370, 585)) {
    stop("Invalid SSP scenario for climate model ", P(sim)$climateGCM, ".\n",
         "climateSSP should be one of '245', '370', or '585' (or one of RCP '45' and '85').")
  }

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

  ## lookup table to get climate urls  based on studyArea, GCM, and SSP
  dt <- data.table::fread(file = file.path(dataPath(sim), "climateDataURLs.csv"))
  historicalClimateURL <- dt[studyArea == studyAreaName & type == "hist_monthly", GID]
  projectedClimateUrl <- dt[studyArea == studyAreaName &
                              GCM == P(sim)$climateGCM &
                              SSP == P(sim)$climateSSP &
                              type == "proj_monthly", GID]

  ## studyArea-specific shapefiles and rasters
  allowedStudyAreas <- c("AB", "BC", "MB", "NT", "NU", "SK", "YT", ## prov/terr x BCR intersections
                         "RIA") ## custom boundaries

  ## TODO: why isn't this saved? cache issue with .inputObjects??
  mod$studyAreaNameLong <- switch(P(sim)$studyAreaName,
                                  AB = "Alberta",
                                  BC = "british Columbia",
                                  SK = "Saskatchewan",
                                  MB = "Manitoba",
                                  NT = "Northwest Territories & Nunavut",
                                  NU = "Northwest Territories & Nunavut",
                                  YT = "Yukon",
                                  RIA = "RIA")

  if (grepl("RIA", P(sim)$studyAreaName)) {
    demUrl <- "https://drive.google.com/file/d/13sGg1X9DEOSkedg1m0PxcdJiuBESk072/"
  } else if (grepl("AB", P(sim)$studyAreaName)) {
    demUrl <- "https://drive.google.com/file/d/1g1SEU65zje6686pQXQzVVQQc44IXaznr/"
  } else if (grepl("BC", P(sim)$studyAreaName)) {
    demUrl <- "https://drive.google.com/file/d/1DaAYFr0z38qmbZcz452QPMP_fzwUIqLD/"
  } else if (grepl("MB", P(sim)$studyAreaName)) {
    demUrl <- "https://drive.google.com/file/d/1X7b2CE6QyCvik3UG9pUj6zc4b5UYZi8w/"
  } else if (grepl("NT|NU", P(sim)$studyAreaName)) {
    ## NOTE: run NT and NU together!
    demUrl <- "https://drive.google.com/file/d/13n8LjQJihy9kd3SniS91EuXunowbWOBa/"
  } else if (grepl("SK", P(sim)$studyAreaName)) {
    demUrl <- "https://drive.google.com/file/d/1CooPdqc3SlVVU7y_BaPfZD0OXt42fjBC/"
  } else if (grepl("YT", P(sim)$studyAreaName)) {
    demUrl <- "https://drive.google.com/file/d/1CUMjLFGdGtwaQlErQ0Oq89ICUCcX641Q/"
  } else {
    stop("studyAreaName must be one of: ", paste(allowedStudyAreas, collapse = ", "))
  }

  sim$studyArea$studyAreaName <- P(sim)$studyAreaName  # makes it a data.frame

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
  historicalClimateArchive <- file.path(historicalClimatePath, paste0(mod$studyAreaNameLong, ".zip"))
  historicalMDCfile <- file.path(historicalClimatePath, paste0("MDC_historical_", studyAreaName, ".grd"))

  ## need to download and extract w/o prepInputs to preserve folder structure!
  if (!file.exists(historicalClimateArchive)) {
    googledrive::drive_download(file = as_id(historicalClimateURL), path = historicalClimateArchive)
    archive::archive_extract(historicalClimateArchive, historicalClimatePath)
  }

  historicalMDC <- Cache(
    makeMDC,
    inputPath = file.path(historicalClimatePath, mod$studyAreaNameLong),
    years = P(sim)$historicalFireYears,
    quick = "inputPath"
  )
  historicalMDC <- Cache(
    postProcess,
    historicalMDC,
    rasterToMatch = sim$rasterToMatch,
    studyArea = sim$studyArea,
    datatype = "INT2U",
    method = "bilinear",
    filename2 = historicalMDCfile,
    useCache = P(sim)$.useCache,
    quick = "filename2", # Cache treats filenames as files; so it digests the file as an input
    userTags = c(paste0("histMDC_", P(sim)$studyAreaName), cacheTags)
  )

  ## The names need "year" at the start, because not every year will have fires (data issue in RIA),
  ## so fireSense matches fires + climate rasters by year.
  ## WARNING: names(historicalMDC) <- paste0('year', P(sim)$historicalFireYears) # Bad
  ##          |-> allows for index mismatching
  historicalMDC <- updateStackYearNames(historicalMDC, Par$historicalFireYears)
  #historicalMDC[] <- historicalMDC[] ## bring raster to memory

  compareRaster(historicalMDC, sim$rasterToMatch)

  sim$historicalClimateRasters <- list("MDC" = historicalMDC)

  ## FUTURE CLIMATE DATA
  projectedClimatePath <- checkPath(file.path(dPath, "climate", "future",
                                              paste0(P(sim)$climateGCM, "_ssp", P(sim)$climateSSP)), create = TRUE)
  projectedClimateArchive <- file.path(dirname(projectedClimatePath),
                                       paste0(mod$studyAreaNameLong, "_",
                                              P(sim)$climateGCM, "_ssp",
                                              P(sim)$climateSSP, ".zip"))
  projectedMDCfile <- file.path(dirname(projectedClimatePath),
                                paste0("MDC_future_", P(sim)$climateGCM,
                                       "_ssp", P(sim)$climateSSP, "_", studyAreaName, ".grd"))

  ## need to download and extract w/o prepInputs to preserve folder structure!
  if (!file.exists(projectedClimateArchive)) {
    googledrive::drive_download(file = as_id(projectedClimateUrl), path = projectedClimateArchive)
    archive::archive_extract(projectedClimateArchive, projectedClimatePath)
  }

  projectedMDC <- Cache(
    makeMDC,
    inputPath = file.path(projectedClimatePath, mod$studyAreaNameLong),
    years = P(sim)$projectedFireYears,
    quick = "inputPath"
  )
  projectedMDC <- Cache(
    postProcess,
    projectedMDC,
    rasterToMatch = sim$rasterToMatch,
    studyArea = sim$studyArea,
    datatype = "INT2U",
    method = "bilinear",
    filename2 = projectedMDCfile,
    useCache = P(sim)$.useCache,
    quick = "filename2", # Cache treats filenames as files; so it digests the file as an input
    userTags = c(paste0("projMDC_", P(sim)$studyAreaName),
                 paste0("projMDC_", P(sim)$climateGCM),
                 paste0("projMDC_SSP", P(sim)$climateSSP), cacheTags)
  )

  ## WARNING: names(projectedMDC) <- paste0('year', P(sim)$projectedFireYears) # Bad
  ##          |-> allows for index mismatching
  projectedMDC <- updateStackYearNames(projectedMDC, Par$projectedFireYears)
  #projectedMDC[] <- projectedMDC[] ## bring raster to memory

  sim$projectedClimateRasters <- list("MDC" = projectedMDC)

  ## CLIMATE DATA FOR gmcsDataPrep
  ## 1) get and unzip normals and projected annual
  ## 2) run makeLandRCS_1950_2010normals, it returns a raster stack with two layers, normal MAT, and normal CMI
  ## 3) assign normal CMI to sim
  ## 4) run makeLandRCS_projectedCMIandATA, with normal MAT as an input arg. It returns a list of raster stacks (projected ATA and CMI). Assign both to sim
  ## 5) Profit

  normalsClimateUrl <- dt[studyArea == studyAreaName & type == "hist_normals", GID]
  normalsClimatePath <- checkPath(file.path(historicalClimatePath, "normals"), create = TRUE)
  normalsClimateArchive <- file.path(normalsClimatePath, paste0(mod$studyAreaNameLong, "_normals.zip"))

  if (!file.exists(normalsClimateArchive)) {
    ## need to download and extract w/o prepInputs to preserve folder structure!
    googledrive::drive_download(file = as_id(normalsClimateUrl), path = normalsClimateArchive)
    archive::archive_extract(normalsClimateArchive, normalsClimatePath)
  }

  normals <- Cache(makeLandRCS_1950_2010_normals,
                   pathToNormalRasters = file.path(normalsClimatePath, mod$studyAreaNameLong),
                   rasterToMatch = sim$rasterToMatch)
  sim$CMInormal <- normals[["CMInormal"]]

  projAnnualClimateUrl <- dt[studyArea == studyAreaName &
                               GCM == P(sim)$climateGCM &
                               SSP == P(sim)$climateSSP &
                               type == "proj_annual", GID]
  projAnnualClimatePath <- checkPath(file.path(projectedClimatePath, "annual"), create = TRUE)
  projAnnualClimateArchive <- file.path(dirname(projAnnualClimatePath),
                                        paste0(mod$studyAreaNameLong, "_",
                                               P(sim)$climateGCM, "_ssp",
                                               P(sim)$climateSSP, "_annual.zip"))

  if (!file.exists(projAnnualClimateArchive)) {
    ## need to download and extract w/o prepInputs to preserve folder structure!
    googledrive::drive_download(file = as_id(projAnnualClimateUrl), path = projAnnualClimateArchive)
    archive::archive_extract(projAnnualClimateArchive, projAnnualClimatePath)
  }

  projCMIATA <- Cache(makeLandRCS_projectedCMIandATA,
                      normalMAT = normals[["MATnormal"]],
                      pathToFutureRasters = file.path(projAnnualClimatePath, mod$studyAreaNameLong),
                      years = P(sim)$projectedFireYears,
                      useCache = TRUE) ## TODO: this is very RAM heavy -- use GDAL?
  sim$ATAstack <- projCMIATA[["projectedATA"]]
  sim$CMIstack <- projCMIATA[["projectedCMI"]]

  ## only use ecozones in the WBI study area
  ecozonesToUse <- c(
    "Boreal Cordillera", "Boreal PLain", "Boreal Shield", "Hudson Plain",
    "Southern Arctic", "Taiga Cordillera", "Taiga Plain", "Taiga Shield"
  )

  ## ecozone boundaries within WBI
  ecozones_WB <- Cache(prepInputs,
                       targetFile = "ecozones.shp",
                       archive = asPath("ecozone_shp.zip"),
                       url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
                       alsoExtract = "similar",
                       destinationPath = dPath,
                       filename2 = NULL,
                       studyArea = mod$WBstudyArea,
                       overwrite = TRUE,
                       useSAcrs = TRUE,
                       fun = "sf::st_read",
                       userTags = c("prepInputsEcozones", currentModule(sim), cacheTags))
  ecozones_WB <- ecozones_WB[ecozones_WB$ZONE_NAME %in% ecozonesToUse, ] ## remove boundary artifacts

  ## ecozone boundaries current study area
  ecozones_SA <- Cache(prepInputs,
                       targetFile = "ecozones.shp",
                       archive = asPath("ecozone_shp.zip"),
                       url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
                       alsoExtract = "similar",
                       destinationPath = dPath,
                       filename2 = NULL,
                       studyArea = sim$studyAreaReporting,
                       overwrite = TRUE,
                       useSAcrs = TRUE,
                       fun = "sf::st_read",
                       userTags = c("prepInputsEcozones", currentModule(sim), cacheTags))
  ecozones_SA <- ecozones_SA[ecozones_SA$ZONE_NAME %in% ecozonesToUse, ] ## remove boundary artifacts

  ## use ecozone boundaries within WBI study area for parameterizing PSP for current study area
  sim$studyAreaPSP <- ecozones_WB[ecozones_WB$ZONE_NAME %in% ecozones_SA$ZONE_NAME, ] %>% as_Spatial()

  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  mod$studyAreaNameLong <- switch(P(sim)$studyAreaName,
                                  AB = "Alberta",
                                  BC = "british Columbia",
                                  SK = "Saskatchewan",
                                  MB = "Manitoba",
                                  NT = "Northwest Territories & Nunavut",
                                  NU = "Northwest Territories & Nunavut",
                                  YT = "Yukon",
                                  RIA = "RIA")

  mod$targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                         "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

  if (!suppliedElsewhere("studyArea", sim)) {
    #### Prep study-area specific objects ####
    ## when adding study areas, add relevant climate urls, rtm and sa, and don't forget R script prepSppEquiv

    provs <- c("British Columbia", "Alberta", "Saskatchewan", "Manitoba")
    terrs <- c("Yukon", "Northwest Territories", "Nunavut")
    WB <- c(provs, terrs)

    bcrzip <- "https://www.birdscanada.org/download/gislab/bcr_terrestrial_shape.zip"

    bcrshp <- Cache(prepInputs,
                    url = bcrzip,
                    destinationPath = dPath,
                    targetCRS = mod$targetCRS,
                    useCache = P(sim)$.useCache,
                    fun = "sf::st_read")

    if (packageVersion("reproducible") >= "1.2.5") {
      fn1 <- function(x) {
        x <- readRDS(x)
        x <- st_as_sf(x)
        st_transform(x, mod$targetCRS)
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
                      #targetCRS = mod$targetCRS, ## TODO: fails on Windows
                      targetFile = "gadm36_CAN_1_sp.rds", ## TODO: this will change as GADM data update
                      destinationPath = dPath,
                      useCache = P(sim)$.useCache) #%>%
    if (packageVersion("reproducible") < "1.2.5") {
      canProvs <- st_as_sf(canProvs) %>%
        st_transform(., mod$targetCRS)
    }

    bcrWB <- bcrshp[bcrshp$BCR %in% c(4, 6:8), ]
    provsWB <- canProvs[canProvs$NAME_1 %in% WB, ]

    mod$WBstudyArea <- Cache(postProcess, provsWB, studyArea = bcrWB, useSAcrs = TRUE,
                             useCache = P(sim)$.useCache,
                             filename2 = NULL, overwrite = TRUE) %>%
      as_Spatial(.)

    if (grepl("RIA", P(sim)$studyAreaName)) {
      studyAreaUrl <- "https://drive.google.com/file/d/1LxacDOobTrRUppamkGgVAUFIxNT4iiHU/"
      ## originally, I thought this could be defined after the IF clause as Eliot suggested.
      ## But if RIA SA = SAL, or RTM = RTML, it falls apart.
      sim$studyArea <- Cache(prepInputs, url = studyAreaUrl,
                             destinationPath = dPath,
                             userTags = c("studyArea", cacheTags)) %>%
        sf::st_as_sf(.) %>%
        .[.$TSA_NUMBER %in% c("40", "08", "41", "24", "16"),] %>%
        sf::st_buffer(., 0) %>%
        sf::as_Spatial(.) %>%
        raster::aggregate(.)
    } else if (grepl("NT|NU", P(sim)$studyAreaName)) {
      ## NOTE: run NT and NU together!
      message("NWT and NU will both be run together as a single study area.")
      sim$studyArea <- mod$WBstudyArea[mod$WBstudyArea$NAME_1 %in% c("Northwest Territories", "Nunavut"), ]
    } else {
      sim$studyArea <- mod$WBstudyArea[mod$WBstudyArea$NAME_1 == mod$studyAreaNameLong, ]
    }

    sim$studyArea <- spTransform(sim$studyArea, mod$targetCRS)
  }

  if (!suppliedElsewhere("studyAreaReporting", sim)) {
    sim$studyAreaReporting <- sim$studyArea
  }

  if (!suppliedElsewhere("studyArea", sim)) {
    ## NOTE: studyArea and studyAreaLarge are the same [buffered] area
    sim$studyArea <- buffer(sim$studyArea, P(sim)$bufferDist)
  }

  if (!suppliedElsewhere("studyAreaLarge", sim)) {
    if (grepl("RIA", P(sim)$studyAreaName)) {
      ## NOTE: RIA uses the same unbuffered studyArea for parameterization, sims, and reporting
      sim$studyAreaLarge <- sim$studyArea
    } else {
      sim$studyAreaLarge <- sim$studyArea
    }
  }

  if (!suppliedElsewhere("rasterToMatch", sim)) {
    sim$rasterToMatch <- Cache(LandR::prepInputsLCC,
                               year = 2005,
                               studyArea = sim$studyArea,
                               destinationPath = dPath,
                               useCache = P(sim)$.useCache,
                               overwrite = TRUE,
                               filename2 = paste0(P(sim)$studyAreaName, "_rtm.tif"))
    #sim$rasterToMatch[] <- sim$rasterToMatch[] ## bring raster to memory
  }

  if (!suppliedElsewhere("rasterToMatchLarge", sim)) {
    sim$rasterToMatchLarge <- sim$rasterToMatch
  }

  if (!suppliedElsewhere("rasterToMatchReporting", sim)) {
    # This was raster::mask -- but that sometimes doesn't work because of incorrect dispatch that
    #  conflicts with devtools::load_all("reproducible")
    sim$rasterToMatchReporting <- Cache(maskInputs, sim$rasterToMatch, sim$studyAreaReporting)
  }

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above
