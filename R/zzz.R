.onLoad <- function(libname, pkgname){
    # e <- new.env(parent = parent.env(environment()))
    utils::globalVariables(c("used_columns", "..XColumns", "..temp.names",
        "output_folder", "pi_distribution"))
}

.onAttach <- function(libname, pkgname) {
    packageStartupMessage("Welcome to TRIP tool!")
}