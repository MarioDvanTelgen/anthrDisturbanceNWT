
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "anthrDisturbanceNWT",
  description = "This module updates an existing anthropogenic disturbance vector layer (base line) by adding features using a feature scheduler", #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = c(person(c("Mario", "Dennis"), "van Telgen", email = "mario.vantelgen@outlook.com", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.5", anthrDisturbanceNWT = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "anthrDisturbanceNWT.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName="studyArea", objectClass="SpatialPolygonsDataFrame", desc="SPDF of study area"),
    expectsInput(objectName = "anthrDisturb", objectClass = NA, desc = NA, sourceURL = NA),
    expectsInput(objectName = "anthrDisturbSchedule", objectClass = "dataframe", desc = "Table with object names, url and scheduled time unit used for discrete addition of anthropogenic disturbances", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName="anthrDisturb", objectClass = "SpatialLinesDataFrame", desc = "Anthropogenic disturbance vector data layer")
  )
))

## event types
#   - type `init` is required for initialization

doEvent.anthrDisturbanceNWT = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "anthrDisturbanceNWT", "development")
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "anthrDisturbanceNWT", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "anthrDisturbanceNWT", "save")
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      #plotFun(sim) # uncomment this, replace with object to plot
      # schedule future event(s)

      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "anthrDisturbanceNWT", "plot")

      # ! ----- STOP EDITING ----- ! #
    },
    save = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "anthrDisturbanceNWT", "save")

      # ! ----- STOP EDITING ----- ! #
    },
    development = {
      sim <- buildRoads(sim)

      sim <- scheduleEvent(sim, time(sim) + P(sim)$increment, "anthrDisturbanceNWT", "development")
    },
    event2 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "anthrDisturbanceNWT", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  # load development schedule
  sim$anthrDisturbSchedule <- read.table(file.path(modulePath(sim, currentModule(sim)),"/data/anthrDisturbanceSchedule.txt"), header = T)
  
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

buildRoads <- function(sim) {
  
  # check scheduler for planned disturbances (road construction, seismic lines, etc.)
  idx <- which(sim$anthrDisturbSchedule$startTime == time(sim))
  if(length(idx) > 0){ # If any, load and add to disturbance vector layer
    disturbToAdd <- sim$anthrDisturbSchedule[idx,]
    as.list(inputObjects(sim, currentModule(sim))[[1]][which(inputObjects(sim, currentModule(sim))[[1]] %in% objects(sim))])
    # get and list these input objects
      disturbToAdd <- lapply(disturbToAdd, function(x){
        reproducible::Cache(prepInputs,
                            url=x$url,
                            targetFile=x$fileName,
                            alsoExtract = "similar",
                            studyArea = sim$studyArea,
                            useSAcrs = TRUE,
                            overwrite = TRUE
        )})
    # combine new layers with existing anthrDisturb
    sim$anthrDisturb <- lapply(disturbToAdd, function(x) {rbind(sim$anthrDisturb, x)})
  }
  
  return(invisible(sim))
}

### template for your event2
Event2 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event2Test1 <- " this is test for event 2. " # for dummy unit test
  # sim$event2Test2 <- 777  # for dummy unit test


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  if(!suppliedElsewhere('studyArea', sim)){
    sim$studyArea <- reproducible::Cache(prepInputs, url = "https://drive.google.com/open?id=1LUxoY2-pgkCmmNH5goagBp3IMpj6YrdU",
                                         targetFile = "BCR6_EcoregionsNWT.shp",
                                         alsoExtract = "similar")
  }
  
  if(!suppliedElsewhere('Roads', sim)){
    RoadsUrl <- "http://www12.statcan.gc.ca/census-recensement/2011/geo/RNF-FRR/files-fichiers/grnf061r10a_e.zip"
    sim$Roads <- Cache(prepInputs, url = RoadsUrl,
                       targetFile = "grnf061r10a_e.shp", # Be explicit and tractable
                       alsoExtract = "similar",
                       studyArea = sim$studyArea,
                       useSAcrs = TRUE,
                       overwrite = TRUE)
  }
  
  if(!suppliedElsewhere('anthrDisturb', sim)){
    anthrDisturb.url <- "http://www.ec.gc.ca/data_donnees/STB-DGST/003/Boreal-ecosystem-anthropogenic-disturbance-vector-data-2008-2010.zip"
    sim$anthrDisturb <- Cache(
      prepInputs,
      url = anthrDisturb.url,
      targetFile = "EC_borealdisturbance_linear_2008_2010_FINAL_ALBERS.shp",
      alsoExtract = "similar",
      studyArea = sim$studyArea,
      useSAcrs = TRUE,
      overwrite = TRUE)
  }
  return(invisible(sim))
}
