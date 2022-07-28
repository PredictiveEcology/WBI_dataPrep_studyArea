makeWBIstudyArea <- function(destinationPath, targetCRS, useCache) {
  provs <- c("British Columbia", "Alberta", "Saskatchewan", "Manitoba")
  terrs <- c("Yukon", "Northwest Territories", "Nunavut")
  WB <- c(provs, terrs)

  bcrzip <- "https://www.birdscanada.org/download/gislab/bcr_terrestrial_shape.zip"

  bcrshp <- Cache(prepInputs,
                  url = bcrzip,
                  destinationPath = destinationPath,
                  targetCRS = targetCRS,
                  useCache = useCache,
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
                    country = "CAN", level = 1, path = destinationPath,
                    #targetCRS = targetCRS, ## TODO: fails on Windows
                    targetFile = "gadm36_CAN_1_sp.rds", ## TODO: this will change as GADM data update
                    destinationPath = destinationPath,
                    useCache = useCache) #%>%
  if (packageVersion("reproducible") < "1.2.5") {
    canProvs <- st_as_sf(canProvs) %>%
      st_transform(., targetCRS)
  }

  bcrWB <- bcrshp[bcrshp$BCR %in% c(4, 6:8), ]
  provsWB <- canProvs[canProvs$NAME_1 %in% WB, ]

  WBstudyArea <- Cache(postProcess, provsWB, studyArea = bcrWB, useSAcrs = TRUE,
                           useCache = useCache,
                           filename2 = NULL, overwrite = TRUE) %>%
    as_Spatial(.)

  return(WBstudyArea)
}
