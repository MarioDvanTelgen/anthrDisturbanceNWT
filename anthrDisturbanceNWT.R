
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "anthrDisturbanceNWT",
  description = "This module updates a provided anthropogenic disturbance vector layer by adding features at discrete time intervals using a disturbance feature update scheduler.",
  keywords = "Anthropogenic; disturbance; scheduler",
  authors = c(person(c("Mario", "Dennis"), "van Telgen", email = "mario.vantelgen@outlook.com", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.5", anthrDisturbanceNWT = "0.1.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "anthrDisturbanceNWT.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("features", "character", NA, NA, NA, "This describes the features to include"),
    defineParameter("startTime", "numeric", 0, NA, NA, "Simulation time at which to initiate aging"),
    defineParameter(".developmentInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between development events"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName="studyArea", objectClass="SpatialPolygonsDataFrame", desc="SPDF of study area"),
    expectsInput(objectName="anthrDisturb", objectClass = "SpatialLinesDataFrame", desc = "Anthropogenic disturbance vector data layer"),
    expectsInput(objectName="Roads", objectClass="SpatialLines*", desc="Road layer"),
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
      sim <- Init(sim)

      sim <- scheduleEvent(sim, P(sim)$startTime, "anthrDisturbanceNWT", "development")
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "anthrDisturbanceNWT", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "anthrDisturbanceNWT", "save")
    },
    plot = {
      #plotFun(sim) # uncomment this, replace with object to plot
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "anthrDisturbanceNWT", "plot")
    },
    save = {
      #Save(sim) # uncomment this, replace with object to save
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "anthrDisturbanceNWT", "save")
    },
    development = {
      sim <- addDisturbance(sim)
      
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.developmentInterval, "anthrDisturbanceNWT", "development")
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

Init <- function(sim) {
  # load development schedule
  sim$anthrDisturbSchedule <- read.table(file.path(modulePath(sim), currentModule(sim),
                                                   "data/anthrDisturbSchedule.txt"), header=T)
  
  # list all vector layer names (as character string) that need to be combined.
  inputObj <- list("anthrDisturb", "Roads")
  
  # get and list these input objects
  inputObjList <- lapply(inputObj, get, envir=sim)
  
  # Check and set Class IDs
  checkNames <- lapply(inputObjList, "names")
  idx <- unlist(lapply(checkNames, function(x){ !("Class" %in% x)}))
  if(any(idx)){
    idx <- which(idx)
    message(paste("No feature classes specified in lines object '", inputObj[[idx]],
                  "'. Column added called 'Class' classifying features as '", inputObj[[idx]] ,
                  "', which will be used for feature selection.", sep=""))
    inputObjList[[idx]]@data$Class <- inputObj[[idx]]
    params(sim)$anthrDisturbanceNWT$features <- c(params(sim)$anthrDisturbanceNWT$features, inputObj[[idx]])
  }
  
  sim$anthrDisturb <- prepLines(inputObjList, template=sim$studyArea)
  
  return(invisible(sim))
}

addDisturbance <- function(sim) {
  # check scheduler for planned disturbances (road construction, seismic lines, etc.)
  idx <- which(sim$anthrDisturbSchedule$planTime == time(sim))
  if(length(idx) > 0){ # If any, load and add to disturbance vector layer
    disturbToAdd <- lapply(idx, function(x) sim$anthrDisturbSchedule[x,])
    # get and list these input objects
      disturbToAdd <- lapply(disturbToAdd, function(x){
        if(is.na(as.character(x$url))){
        message(paste("no url provided for disturbance layer '", as.character(x$name), "'; will be ignored. Add url to 'anthrDisturbSchedule.txt'", sep=""))  
        } else {
          message(paste("'", as.character(x$name), "' is being prepared and added to 'anthrDisturb'", sep=""))
          tmp <- reproducible::Cache(prepInputs,
                              url=as.character(x$url),
                              targetFile=as.character(x$targetFile),
                              alsoExtract = "similar",
                              studyArea = sim$studyArea,
                              useSAcrs = TRUE,
                              overwrite = TRUE)
          idx <- which(tmp@data[,x$IDcol] == x$featureID)
          tmp <- tmp[idx,]
          return(tmp)
        }
        })
      # combine new layers with existing anthrDisturb
      disturbToAdd[[length(disturbToAdd)+1]] <- sim$anthrDisturb
      sim$anthrDisturb <- prepLines(disturbToAdd, template=sim$studyArea) # also works for single object
  } else {
    sim$anthrDisturb
  }
  
  return(invisible(sim))
}

### template for save events
Save <- function(sim) {
  # sim <- saveFiles(sim)
  
  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {
  # Plot(sim$object)
  
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
  
  # select features from anthrDisturb layer
  if(suppliedElsewhere("anthrDisturb", sim)){
    if(P(sim)$features[1] == "all" | is.na(P(sim)$features[1])){ # select if layers specified
      message("All features in 'sim$anthrDisturb' selected.")
    } else {
      idx <- P(sim)$features %in% sim$anthrDisturb$Class
      message(paste("c(", paste(P(sim)$features[which(idx)], collapse = ", "), ") in 'sim$anthrDisturb' selected."))
      idx <- sim$anthrDisturb$Class %in% P(sim)$features
      if(any(idx)){
        sim$anthrDisturb <- sim$anthrDisturb[idx,]
        idx <- !(P(sim)$features %in% sim$anthrDisturb$Class)
        if(any(idx)){
          message(paste("Feature class '", P(sim)$features[which(idx)], "' ignored. Does not occur in 'sim$anthrDisturb'", sep=""))
        }
      } else {
        stop(paste("Need to specify an existing feature class of 'anthrDisturb'"))
      }
    }
  }
  
  return(invisible(sim))
}
