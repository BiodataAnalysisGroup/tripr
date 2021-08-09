.onLoad <- function(libname, pkgname){
    # e <- new.env(parent = parent.env(environment()))
    utils::globalVariables(c("used_columns"))
}

.onAttach <- function(libname, pkgname) {
    packageStartupMessage("Welcome to TRIP tool!")
}