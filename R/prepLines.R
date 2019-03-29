prepLines <- function(lines, template){
  # x: list of SpatialLines* objects
  # template: template for projection
  library(sp)
  # Coerce all objects to SpatialLinesDataFrame
  objClass <- lapply(lines, class)
  targetClass <- "SpatialLinesDataFrame" # SLDF cannot be binded to SpatialLines. All data needed here? or can drop?
  idx <- which(!objClass %in% targetClass)
  if(length(idx) > 0){ # evaluate/set class
    lines <- lapply(lines[idx], FUN= function(x) {sp::SpatialLinesDataFrame(x, data=data.frame(1:length(x)))} )
  }
  
  # Ensure equal CRS
  # Equalize inputObject CRS
  if(length(lines) > 1){
    # Check and match object projections
    crs <- unlist(lapply(lines, proj4string))
    idx <- !is.na(lapply(lines, proj4string))
    # missing projection
    if(any(idx)) { # any with projection?
      # if(length(lines) > 1) {
      idx <- crs != sp::proj4string(template)
      if(any(idx)){ # any projection other than template?
        idx <- which(crs != sp::proj4string(template))
        crs(lines[[idx]]) <- sp::proj4string(template)
      } else {
        message("CRS of imputObjects all equal to template; No need for reprojection")
      }
    } else {
      if(exists("projectCRS")) { # if no crs available, set to project crs
        crs(lines[[idx]]) <- projectCRS
      } 
    }
  }
  
  
  # bind all spatialLines objects that are provided in 'inputObjects' together
  # rownames have to be unique and number of columns equal
  # Manual adjustment of columns and rows
  tmp <- 0
  linesNames <- unique(unlist(lapply(lines, 'names')))
  lines <- lapply(lines, function(x){
    idx <- linesNames[!(linesNames %in% names(x))]
    df <- data.frame(matrix(NA, nrow=length(x), ncol=length(idx), dimnames=list(row_names=row.names(x), 
                                                                                col_names=idx)))
    x@data <- cbind(x@data,df)
    x <- x[, linesNames]
  })
  
  for(i in 1:length(lines)){
    row.names(lines[[i]]) <- as.character(as.numeric(row.names(lines[[i]])) + tmp)
    tmp <- length(lines[[i]])
  }
  
  do.call(rbind, lines) # also works for single object
  
}
