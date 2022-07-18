.onLoad <- function(libname, pkgname){
    # e <- new.env(parent = parent.env(environment()))
    utils::globalVariables(c("used_columns", "..XColumns", "..temp.names",
        "output_folder", "pi_distribution", "logFile",
        "freqTables_datasets", "motif_datasets", "motif_all"))
    if (!file.exists(file.path(tempdir(), "/output"))) {
      
      
        fs::dir_create(file.path(tempdir(), "/output"),
                        mode = "u=rwx,go=rwx")
    }
    if (!file.exists(file.path(tempdir(), "/log_files"))) {
        fs::dir_create(file.path(tempdir(), "/log_files"),
                        mode = "u=rwx,go=rwx")
    }
    logfile()
    
}

.onAttach <- function(libname, pkgname) {
    packageStartupMessage("Welcome to tripr!")
}


