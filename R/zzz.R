## Function to check whether python and required packages are available

.onAttach <- function(libname, pkgname){

  find.python <- NULL

  find.python.libraries <- NULL
  ## For unix
  if(.Platform$OS.type == "unix"){
    ## Check for mono
    is.python.present <- suppressWarnings(system("which python3", intern = TRUE, ignore.stderr = TRUE))
    if(length(is.python.present) == 0){
      find.python <- "Python not found on this Unix Platform (needed for certain functions) \n"
      ## In Mac seems to be in /usr/local/bin/python3
      ## In Ubuntu python seems to be installed in:
    }
  }else{
    if(grepl("Windows", Sys.info()[["sysname"]])){
      is.python.present <- suppressWarnings(Sys.which("python"))
      if(length(is.python.present) == 0){
        find.python <- "Python not found on this Windows Platform (needed for certain functions) \n"
      }
    }
  }


  if(length(find.python) > 0 && length(find.python.libraries) > 0){

    fax <- paste0("Found Python and related packages")

  }else{

    packageStartupMessage(find.python)
  }
}
