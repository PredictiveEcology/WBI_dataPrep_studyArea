defineModule(sim, list(
  name = "WBI_dataPrep_studyArea",
  description = paste("Prepares 2 sets of objects needed for LandR-fireSense simulations in the WBI:",
                      "1. study areas and corresponding rasterToMatch (as well as large versions);",
                      "2. species equivalencies tables and the sppEquiv column;",
                      "Each is customized to the study area parameter passed as studyAreaName."),
  keywords = "",
  authors = c(
    person("Ian", "Eddy", email = "ian.eddy@nrcan-rncan.gc.ca", role = "aut"),
    person("Alex M", "Chubaty", email = "achubaty@for-cast.ca", role = "aut"),
    person("Eliot", "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = "ctb")
  ),
  childModules = character(0),
  version = list(WBI_dataPrep_studyArea = "0.1.0"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.md", "WBI_dataPrep_studyArea.Rmd")),
  reqdPkgs = list("magrittr", "raster", "sf", "sp",
                  "PredictiveEcology/reproducible@development (>= 1.2.8.9044)",
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
    defineParameter("studyAreaName", "character", "RIA", NA, NA,
                    paste("One of 'AB', 'BC', 'MB', 'NT', 'SK', 'YT', or 'RIA'."))
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
    createsOutput("sppColorVect", objectClass = "character",
                  desc = "species colours for plotting"),
    createsOutput("sppEquiv", objectClass = "data.table",
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

  sim$sppEquiv <- makeSppEquivWBI(P(sim)$studyAreaName)
  sim$sppEquivCol <- "LandR"

  ## studyArea-specific shapefiles and rasters
  allowedStudyAreas <- c("AB", "BC", "MB", "NT", "NU", "SK", "YT", ## prov/terr x BCR intersections
                         "RIA") ## custom boundaries

  ## TODO: why isn't this saved? cache issue with .inputObjects??
  mod$studyAreaNameLong <- switch(P(sim)$studyAreaName,
                                  AB = "Alberta",
                                  BC = "British Columbia",
                                  SK = "Saskatchewan",
                                  MB = "Manitoba",
                                  NT = "Northwest Territories & Nunavut",
                                  NU = "Northwest Territories & Nunavut",
                                  YT = "Yukon",
                                  RIA = "RIA")

  sim$studyArea$studyAreaName <- P(sim)$studyAreaName  # makes it a data.frame

  ## Paired handles 12 colours so it is safer compared to Accent's 8 max
  sim$sppColorVect <- LandR::sppColors(sppEquiv = sim$sppEquiv, sppEquivCol = sim$sppEquivCol,
                                       palette = "Paired")

  stopifnot(getOption("reproducible.useNewDigestAlgorithm") == 2)

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
  sim$studyAreaPSP <- ecozones_WB[ecozones_WB$ZONE_NAME %in% ecozones_SA$ZONE_NAME, ] %>% sf::as_Spatial()

  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  mod$studyAreaNameLong <- switch(P(sim)$studyAreaName,
                                  AB = "Alberta",
                                  BC = "British Columbia",
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
