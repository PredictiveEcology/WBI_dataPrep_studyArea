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
                  "PredictiveEcology/reproducible@development (>= 1.2.6.9008)",
                  "PredictiveEcology/fireSenseUtils@development (>= 0.0.4.9014)",
                  "PredictiveEcology/LandR@development"),
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
    defineParameter("historicalFireYears", "numeric", default = 1991:2019, NA, NA,
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
    sim$studyArea <- Cache(prepInputs, url = studyAreaUrl,
                           destinationPath = dPath,
                           userTags = c("studyArea", cacheTags)) %>%
      sf::st_as_sf(.) %>%
      .[.$TSA_NUMBER %in% c("40", "08", "41", "24", "16"),] %>%
      sf::st_buffer(., 0) %>%
      sf::as_Spatial(.) %>%
      raster::aggregate(.)

    historicalClimateUrl <- "https://drive.google.com/file/d/1vQXi10thWsDyLW-tu300ZMG655tHyE_-/"
    projectedClimateUrl <- "https://drive.google.com/file/d/1wTAHHFx0xcCiZerb0xxG4Vaq4flK4YqG/view?usp=sharing"
  } else if (grepl("AB", P(sim)$studyAreaName)) {
    sim$studyArea <- WBstudyArea[WBstudyArea$NAME_1 == "Alberta", ]
    historicalClimateUrl <- "https://drive.google.com/file/d/12DnBcvLZy_AtPH2fgDYWclqmYH6RxHUi/"
    projectedClimateUrl <- "https://drive.google.com/file/d/1tS4jYBa3gDUhH0LnC-_LVSoML1fwetq-/"
  } else if (grepl("BC", P(sim)$studyAreaName)) {
    sim$studyArea <- WBstudyArea[WBstudyArea$NAME_1 == "British Columbia", ]
    historicalClimateUrl <- "https://drive.google.com/file/d/1OCA0woTd4WTZl31-10M1MT6x8ukHApCR/"
    projectedClimateUrl <- "https://drive.google.com/file/d/1C4fG_YckuF0Wo_k6wDApsudQXfDj5i2N/"
  } else if (grepl("MB", P(sim)$studyAreaName)) {
    sim$studyArea <- WBstudyArea[WBstudyArea$NAME_1 == "Manitoba", ]
    historicalClimateUrl <- "https://drive.google.com/file/d/1fM_5d08J9LbMoTf5ssmC0EFE04eJhBUc/"
    projectedClimateUrl <- "https://drive.google.com/file/d/1Dr3C-YuWoOQp85qj3PhCJ4PKQPA_1qnG/"
  } else if (grepl("NT|NU", P(sim)$studyAreaName)) {
    ## NOTE: run NT and NU together!
    message("NWT and NU will both be run together as a single study area.")
    sim$studyArea <- WBstudyArea[WBstudyArea$NAME_1 %in% c("Northwest Territories", "Nunavut"), ]
    historicalClimateUrl <- "https://drive.google.com/file/d/1pTZMStaxE_rD-jvYk79uXi2Qka3niClL/"
    projectedClimateUrl <- "https://drive.google.com/file/d/1BbnsAxPlU6Uo7h0h0EBap5UyBLH_r7N_/"
  } else if (grepl("SK", P(sim)$studyAreaName)) {
    sim$studyArea <- WBstudyArea[WBstudyArea$NAME_1 == "Saskatchewan", ]
    historicalClimateUrl <- "https://drive.google.com/file/d/1xLS_m3zM_N92eUfxNfvmEdgTJFw19X2Q/"
    projectedClimateUrl <- "https://drive.google.com/file/d/1KnoPsxqwxJx7aPDEV7SzoLE5a52q7V34/"
  } else if (grepl("YT", P(sim)$studyAreaName)) {
    sim$studyArea <- WBstudyArea[WBstudyArea$NAME_1 == "Yukon", ]
    historicalClimateUrl <- "https://drive.google.com/file/d/1v5q1tIL01ht6HS63BMVOO4X1wcV-NuWO/"
    projectedClimateUrl <- "https://drive.google.com/file/d/18ALMK8nwRY6i7pHJpLLXMXbB55Xh4NmB/"
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
                             year = 2005, ## TODO: use 2010
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

  if (isTRUE(getOption("reproducible.useNewDigestAlgorithm") == 2)) { # new algo can handle in 1 step
    historicalMDC <- Cache(prepInputs, url = historicalClimateUrl,
                            destinationPath = dPath,
                            rasterToMatch = sim$rasterToMatch,
                            studyArea = sim$studyArea,
                            fun = "raster::stack",
                            datatype = "INT2U",
                            method = "bilinear",
                            filename2 = file.path(dPath, paste0(P(sim)$studyAreaName, "_histClim.grd")),
                            useCache = P(sim)$.useCache,
                            quick = "filename2", # Cache treats filenames as files; so it digests the file as an input
                            userTags = c(paste0("histMDC_", P(sim)$studyAreaName), cacheTags))
  } else {
    historicalMDC <- prepInputs(url = historicalClimateUrl,
                                destinationPath = dPath,
                                # rasterToMatch = sim$rasterToMatch,
                                # studyArea = sim$studyArea,
                                fun = "raster::stack",
                                filename2 = paste0(studyAreaName, "_histMDC_lonlat.grd"),
                                overwrite = TRUE,
                                useCache = P(sim)$.useCache,
                                userTags = c(paste0("histMDC_", P(sim)$studyAreaName), cacheTags))
    historicalMDC <- raster::stack(historicalMDC)
    historicalMDC <- Cache(raster::projectRaster, historicalMDC, to = sim$rasterToMatch,
                           datatype = "INT2U",
                           filename2 = paste0(studyAreaName, "_histMDC_lcc.grd"),
                           overwrite = TRUE,
                           userTags = c("reprojHistoricClimateRasters"))
    historicalMDC <- raster::stack(historicalMDC)
    historicalMDC <- Cache(raster::mask, historicalMDC, sim$studyArea,
                           userTags = c("maskHistoricClimateRasters"),
                           filename = file.path(dPath, paste0(studyAreaName, "_histMDC_lcc_masked.grd")),
                           overwrite = TRUE)
    historicalMDC <- raster::stack(historicalMDC)
  }

  ## The names need "year" at the start, because not every year will have fires (data issue in RIA),
  ## so fireSense matches fires + climate rasters by year.
  ## WARNING: names(historicalMDC) <- paste0('year', P(sim)$historicalFireYears) # Bad
  ##          |-> allows for index mismatching
  historicalMDC <- updateStackYearNames(historicalMDC, Par$historicalFireYears)
  #historicalMDC[] <- historicalMDC[] ## bring raster to memory

  sim$historicalClimateRasters <- list("MDC" = historicalMDC)

  if (isTRUE(getOption("reproducible.useNewDigestAlgorithm") == 2)) { # new algo can handle in 1 step
    projectedMDC <- Cache(prepInputs, url = projectedClimateUrl,
                          destinationPath = dPath,
                          rasterToMatch = sim$rasterToMatch,
                          studyArea = sim$studyArea,
                          fun = "raster::stack",
                          datatype = "INT2U",
                          filename2 = file.path(dPath, paste0(P(sim)$studyAreaName, "_projMDC.grd")),
                          omitArgs = "filename2",
                          method = "bilinear",
                          useCache = P(sim)$.useCache,
                          userTags = c("projMDC", cacheTags))

  } else {
    projectedMDC <- prepInputs(url = projectedClimateUrl,
                               destinationPath = dPath,
                               # rasterToMatch = sim$rasterToMatch,
                               # studyArea = sim$studyArea,
                               fun = "raster::stack",
                               filename2 = file.path(dPath, paste0(P(sim)$studyAreaName, "_projMDC_lonlat.grd")),
                               overwrite = TRUE,
                               useCache = P(sim)$.useCache,
                               userTags = c("projMDC", cacheTags))
    projectedMDC <- stack(projectedMDC)
    projectedMDC <- Cache(raster::projectRaster, projectedMDC, to = sim$rasterToMatch,
                          datatype = "INT2U",
                          filename2 = paste0(studyAreaName, "_projMDC_lcc.grd"),
                          overwrite = TRUE,
                          userTags = c("reprojProjectedMDC"))
    projectedMDC <- stack(projectedMDC)
    projectedMDC <- Cache(raster::mask, projectedMDC, sim$studyArea,
                          userTags = c("maskProjectedClimateRasters"),
                          filename = file.path(dPath, paste0(studyAreaName, "_projMDC_lcc_masked.grd")),
                          overwrite = TRUE)
    projectedMDC <- stack(projectedMDC)
  }

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
