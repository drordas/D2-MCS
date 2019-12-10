packages.list <- c("R6","car","plyr","dplyr","caret","tictoc","mltools","here","rJava",
                   "RWeka","devtools","ggrepel","ModelMetrics","gridExtra", "ggplot2",
                   "lattice","FSelector","BBmisc","ggalt","reshape2",
                   "crayon", "infotheo", "parallel","crayon")

cat("[PkgChecker][INFO] Package Manager\n")

checkPackages <- function(packages){
  new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)){
    cat("[PkgChecker][INFO]",length(new.packages)," packages needed to execute aplication\n Installing packages ...\n")
    suppressMessages(install.packages(new.packages,repos="https://ftp.cixug.es/CRAN/",
                                      verbose = FALSE, quiet = TRUE, dependencies = TRUE))
  }
}

loadPackages <- function(packages, quiet = TRUE){
  unload.packages <- packages[!(packages %in% loaded_packages()$package )  ]
  if(length(unload.packages) > 0){
    cat("[PkgChecker][INFO] ",length(unload.packages)," required packages not loaded\n Loading packages ...\n", sep="", file = ifelse(quiet,"/dev/null",""))
    for( lib in unload.packages ){
      cat("[PkgChecker][INFO] Loading: '",lib,"'\n", sep="", file = ifelse(quiet,"/dev/null","") )
      suppressWarnings(suppressMessages(suppressPackageStartupMessages(library(lib, verbose = FALSE, quietly = TRUE,
                                                                               warn.conflicts = FALSE,character.only = TRUE) )))
    }
  }else cat("[PkgChecker][INFO] All packages are loaded\n",file = ifelse(quiet,"/dev/null",""))
}

verifyandLoadPackages <- function(){
  checkPackages(packages.list)
  loadPackages(packages.list)
}

verifyandLoadPackages()