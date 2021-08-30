.onLoad <- function(libname, pkgname){
    # e <- new.env(parent = parent.env(environment()))
    utils::globalVariables(c("used_columns", "..XColumns", "..temp.names",
        "output_folder", "pi_distribution", "logFile"))
    if (!file.exists(paste0(system.file("extdata", package="tripr"), 
            "/output"))) {
        fs::dir_create(paste0(system.file("extdata", package="tripr"), 
                            "output"),
                        mode = "u=rwx,go=rwx")
    }
    if (!file.exists(paste0(system.file("extdata", package="tripr"), 
            "/log_files"))) {
        fs::dir_create(paste0(system.file("extdata", package="tripr"), 
                            "log_files"),
                        mode = "u=rwx,go=rwx")
    }
    logFile <- paste0(system.file("extdata/log_files", package="tripr"), 
        "/log_file_", trunc(as.numeric(Sys.time())), ".txt")
    cat(paste0("Function", "\t", "Parameters", "\t", "Num of input rows", "\t", 
        "Num of input columns", "\t", "Start time", "\t", "End time", "\t", 
            "Memory used"), file = logFile, append = FALSE, sep = "\n")
    assign('logFile', logFile, envir = topenv())

    ## output folder name as system time
    output_path <- paste0(system.file("extdata/output", package="tripr"), 
        "/output_", format(Sys.time(), "%H%M%S"))
    # output path
    e$output_folder <- paste0(fs::path(output_path), "/output_tables")
    # Create output_folder directory .onLoad
    fs::dir_create(paste0(e$output_folder), mode = "u=rwx,go=rwx")
}

.onAttach <- function(libname, pkgname) {
    packageStartupMessage("Welcome to TRIP tool!")
}