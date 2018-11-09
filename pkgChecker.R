packages.list <- c("R6","plyr","dplyr","caret","tictoc","mltools","here","parallel","RWeka","devtools","ggrepel","ModelMetrics","tools","tictoc")

cat("[PkgChecker][INFO] Package Manager\n")

checkPackages <- function(packages){
  new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)){ 
    cat("[PkgChecker][INFO]",length(new.packages)," packages needed to execute aplication\n Installing packages ...\n")
    suppressMessages(install.packages(new.packages,repos="https://ftp.cixug.es/CRAN/", verbose = FALSE))
  }
}

loadPackages <- function(packages){
  unload.packages <- packages[!(packages %in% (.packages() ) )  ]
  if(length(unload.packages) > 0){ 
    cat("[PkgChecker][INFO] ",length(unload.packages)," required packages not loaded\n Loading packages ...\n", sep="")
    for( lib in unload.packages ){
      cat("[PkgChecker][INFO] Loading: '",lib,"'\n", sep="")
      suppressWarnings(suppressMessages(suppressPackageStartupMessages(library(lib, verbose = FALSE, quietly = FALSE, warn.conflicts = FALSE,character.only = TRUE) )))
    }
  }else cat("[PkgChecker][INFO] All packages are loaded\n")
}

verifyandLoadPackages <- function(){
  checkPackages(packages.list)
  loadPackages(packages.list)
}

verifyandLoadPackages()