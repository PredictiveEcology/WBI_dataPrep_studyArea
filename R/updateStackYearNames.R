updateStackYearNames <- function(annualDataStack, desiredYears, parameterName) {
  objectName <- as.character(substitute(annualDataStack))
  parameterName <- as.character(substitute(desiredYears))[[3]]

  grepTest4DigitYear <- "[[:digit:]]{4,4}"
  namingConventionTxt <- paste("does not have names that include the 4 digit year.",
                               "Please use that naming convention.")

  # Sanity check -- both objects must have 4 length year in them
  hasYearStack <- all(grepl(grepTest4DigitYear, names(annualDataStack)))
  if (!isTRUE(hasYearStack))
    stop("The raster stack, ", objectName, ", ", namingConventionTxt)
  hasYearParam <- all(grepl(grepTest4DigitYear, desiredYears))
  if (!isTRUE(hasYearParam))
    stop("The parameter, ",parameterName,", ", namingConventionTxt)
  annualDataStackInt <- as.integer(gsub(".*([[:digit:]]{4,4}).*", "\\1", names(annualDataStack)))
  if (!identical(annualDataStackInt, desiredYears))
    warning("User has provided a set of years for annual fire via P(sim)$", parameterName,", ",
            "that does not match the input dataset of ", objectName, ":\n\n",
            "names(", objectName, "): ", paste(names(annualDataStack), collapse = ", "), "\n\n",
            "P(sim)$", parameterName, ": ", paste(paste0('year', desiredYears), collapse = ", "),
            "\n\n", "using the overlapping years provided by both.")

  whichannualDataStackToKeep <- which(annualDataStackInt %in% desiredYears)
  annualDataStackInt <- annualDataStackInt[whichannualDataStackToKeep]
  namesWithYearPrepended <- paste0("year", annualDataStackInt)

  names(annualDataStack) <- namesWithYearPrepended
  annualDataStack
}
