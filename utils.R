##UTILS FUNCTIONS

loadPackages = function(pkgName){
  new.packages <- pkgName[!(pkgName %in% installed.packages()[,"Package"])]
  if(length(new.packages)){ 
    cat("[Model][INFO]",length(new.packages),"packages needed to execute aplication\n Installing packages ...")
    suppressMessages(install.packages( new.packages,repos="https://ftp.cixug.es/CRAN/", dependencies = TRUE,
                                       quiet = TRUE, verbose = FALSE))
  }
  lapply(pkgName, function(pkg){
    if (! pkg %in% loaded_packages() )
      require(pkg,character.only = TRUE,warn.conflicts = FALSE,quietly = TRUE)
  })
}

unloadPackages = function(pkgName){
  lapply(pkgName, function(pkg){
    if( (pkg %in% loaded_packages()$package) && (!pkg %in% c("dplyr","plyr")) ){
      pck.name <- paste0("package:",pkg)
      try(detach(pck.name, character.only = TRUE, unload = TRUE, force = TRUE), silent = TRUE)
    }
  })
}