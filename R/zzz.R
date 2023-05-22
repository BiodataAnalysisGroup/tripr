.onLoad <- function(libname, pkgname){
    # e <- new.env(parent = parent.env(environment()))
    utils::globalVariables(c("used_columns", "..XColumns", "..temp.names",
        "output_folder", "pi_distribution", "N.uniq.seq", "Gene", ":=",
        "freqTables_datasets", "motif_datasets", "motif_all")) #"logFile","out_log_file_path"
    
}

.onAttach <- function(libname, pkgname) {
    packageStartupMessage("Welcome to tripr!")
  
}


