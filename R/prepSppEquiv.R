#simple function for making sppEquivalencies objects tailed to each studyArea
prepSppEquiv <- function(studyArea, sppEquiv){

  if (studyArea == 'RIA'){

    sppEquiv[grep("Pin", LandR), `:=`(EN_generic_short = "Pine",
                                      EN_generic_full = "Pine",
                                      Leading = "Pine leading")]

    sppEquiv[, RIA := c(Pice_mar = "Pice_mar", Pice_gla = "Pice_gla",
                        Pinu_con = "Pinu_con", Popu_tre = "Popu_tre",
                        Betu_pap = "Betu_pap",
                        Pice_eng = "Pice_eng")[LandR]]
    sppEquiv[LANDIS_traits == "ABIE.LAS"]$RIA <- "Abie_las"

    sppEquiv <- sppEquiv[!LANDIS_traits == "PINU.CON.CON"]

    sppEquiv[RIA == "Abie_las", EN_generic_full := "Subalpine Fir"]
    sppEquiv[RIA == "Abie_las", EN_generic_short := "Fir"]
    sppEquiv[RIA == "Abie_las", Leading := "Fir leading"]
    sppEquiv[RIA == "Popu_tre", Leading := "Pop leading"]
    sppEquiv[RIA == "Betu_pap", EN_generic_short := "Betula"]
    sppEquiv[RIA == "Betu_pap",  Leading := "Betula leading"]
    sppEquiv[RIA == "Betu_pap",  EN_generic_full := "Paper birch"]
    sppEquiv[RIA == "Pice_eng", EN_generic_full := 'Engelmann Spruce']
    sppEquiv[RIA == 'Pice_eng', EN_generic_short  := "En Spruce"]

    sppEquiv <- sppEquiv[!is.na(RIA)]
    sppEquiv[RIA == "Pinu_con", KNN := "Pinu_Con"]

  } else {
    #add your sppEquiv Objects here
    stop('no other spp equiv at the moment :( ')
  }

  return(sppEquiv = sppEquiv)
}
