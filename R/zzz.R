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
    ## output folder name as system time
    
    if (save_tables_individually | save_lists_for_bookmark) {
      ## output folder name as system time
      output_path <- paste0(getwd(),
                            "/output_", format(Sys.time(), "%H.%M.%S"))
      message("Output will be saved in: ", fs::path(output_path))
      # output path
      e$output_folder <- paste0(fs::path(output_path), "/output_tables")
      if (!file.exists(paste0(e$output_folder))) {
        fs::dir_create(paste0(e$output_folder), mode = "u=rwx,go=rwx")
      }
    }
    
}

.onAttach <- function(libname, pkgname) {
    packageStartupMessage("Welcome to tripr!")
}


