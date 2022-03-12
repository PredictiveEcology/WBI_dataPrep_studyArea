makeSppEquivWBI <- function(studyAreaName) {
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

  sppEquivSA <- sppEquiv[which(wbiSppToUse[, ..sAN][[1]]), ] ## subset per study area

  return(sppEquivSA)
}
