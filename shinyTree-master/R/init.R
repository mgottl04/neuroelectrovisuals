.global <- new.env()

initResourcePaths <- function() {
  if (is.null(.global$loaded)) {
    shiny::addResourcePath(
      prefix = 'shinyTree',
      directoryPath = system.file('www', package='shinyTree'))
    .global$loaded <- TRUE
  }
  shiny::HTML("")
}

# Parse incoming shinyTree input from the client
#' @importFrom methods loadMethod
.onAttach <- function(libname, pkgname){
  shiny::registerInputHandler("shinyTree", function(val, shinysession, name){
    jsonToAttr(val)    
  })
}

jsonToAttr <- function(json){
  ret <- list()
  
  if (! "text" %in% names(json)){
    # This is a top-level list, not a node.
    for (i in 1:length(json)){
      ret[[json[[i]]$text]] <- jsonToAttr(json[[i]])
      ret[[json[[i]]$text]] <- supplementAttr(ret[[json[[i]]$text]], json[[i]])
    }
    return(ret)
  }
  
  if (length(json$children) > 0){
    return(jsonToAttr(json[["children"]]))
  } else {
    ret <- 0
    ret <- supplementAttr(ret, json)    
    return(ret)
  }
}

supplementAttr <- function(ret, json){
  #attr(ret, "stselected") <- json$state$selected
  #attr(ret, "stdisabled") <- json$state$disabled
  #attr(ret, "stopened") <- json$state$opened
  print("Init:")
  print(json$state$opened)
}