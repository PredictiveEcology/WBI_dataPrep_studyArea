---
title: "WBI_dataPrep_studyArea"
author: ""
date: "21 September 2020"
output:
  html_document:
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
---



# Overview

Provide an overview of what the module does / how to use the module.

Module documentation should be written so that others can use your module.
This is a template for module documentation, and should be changed to reflect your module.

## R Markdown

R Markdown syntax allows R code, outputs, and figures to be rendered in the documentation.

For help writing in R Markdown, see https://rmarkdown.rstudio.com/.

# Usage


```r
library(SpaDES.core)
```

```
## Loading required package: quickPlot
```

```
## Loading required package: reproducible
```

```
## 
## Attaching package: 'SpaDES.core'
```

```
## The following objects are masked from 'package:stats':
## 
##     end, start
```

```
## The following object is masked from 'package:utils':
## 
##     citation
```


```r
setPaths(modulePath = file.path('../'),
         inputPath = "data")
paths <- getPaths() # shows where the 4 relevant paths are

times <- list(start = 0, end = 10)

parameters <- list(
  WBI_data_Prep_studyArea = list(
  studyAreaName = 'RIA'
  #.progress = list(type = "text", interval = 1), # for a progress bar
  ## If there are further modules, each can have its own set of parameters:
  #module1 = list(param1 = value1, param2 = value2),
  #module2 = list(param1 = value1, param2 = value2)
))
modules <- list("WBI_dataPrep_studyArea")
objects <- list()
inputs <- list()
outputs <- list()

mySim <- simInit(times = times, params = parameters, modules = modules,
                 objects = objects, paths = paths)

mySimOut <- spades(mySim)
```

# Parameters

Provide a summary of user-visible parameters.


|paramName           |paramClass |default      |min |max |paramDesc                                                                                                                                                |
|:-------------------|:----------|:------------|:---|:---|:--------------------------------------------------------------------------------------------------------------------------------------------------------|
|.plotInitialTime    |numeric    |NA           |NA  |NA  |Describes the simulation time at which the first plot event should occur.                                                                                |
|.plotInterval       |numeric    |NA           |NA  |NA  |Describes the simulation time interval between plot events.                                                                                              |
|.saveInitialTime    |numeric    |NA           |NA  |NA  |Describes the simulation time at which the first save event should occur.                                                                                |
|.saveInterval       |numeric    |NA           |NA  |NA  |This describes the simulation time interval between save events.                                                                                         |
|.useCache           |logical    |FALSE        |NA  |NA  |Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant |
|bufferDist          |numeric    |20000        |NA  |NA  |Distance (m) to buffer studyArea and rasterToMatch when creating 'Large' versions.                                                                       |
|climateGCM          |character  |CNRM-ESM2-1  |NA  |NA  |Global Circulation Model to use for climate projections: currently '13GCMs_ensemble', 'CanESM5', 'CNRM-ESM2-1', or 'CCSM4'.                              |
|climateSSP          |numeric    |370          |NA  |NA  |SSP emissions scenario for `climateGCM`: one of 245, 370, or 585.[If using 'climateGCM = CCSM4', climateSSP must be one of 45 or 85.]                    |
|historicalFireYears |numeric    |1991, 19.... |NA  |NA  |range of years captured by the historical climate data                                                                                                   |
|projectedFireYears  |numeric    |2011, 20.... |NA  |NA  |range of years captured by the projected climate data                                                                                                    |
|studyAreaName       |character  |RIA          |NA  |NA  |study area name for WB project - one of BC, AB, SK, YK, NWT, MB, or RIA                                                                                  |

# Events

Describe what happens for each event type.

## Plotting

Write what is plotted.

## Saving

Write what is saved.

# Data dependencies

## Input data

How to obtain input data, and a description of the data required by the module.
If `sourceURL` is specified, `downloadData("WBI_dataPrep_studyArea", "..")` may be sufficient.


|desc |
|:----|

## Output data

Description of the module outputs.



# Links to other modules

Describe any anticipated linkages to other modules.
