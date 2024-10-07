#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#'
#' @return None
#' @examples
#' if (interactive()) {
#'     run_app(options = list(launch.browser = FALSE))
#' }
run_app <- function(onStart = NULL,
    options = list(launch.browser = TRUE),
    enableBookmarking = NULL,
    uiPattern = "/",
    ...) {
    ## output folder name as system time
    
    if (save_tables_individually | save_lists_for_bookmark) {
      ## output folder name as system time
      output_path <- paste0(file.path(getwd(),
                                      "/output_"), format(Sys.time(), "%Y.%m.%d_%H.%M.%S"))
      # out_log_file_path <- output_path
      message("Output will be saved in: ", fs::path(output_path))
      # output path
      e$output_folder <- paste0(fs::path(output_path), "/output_tables")
      if (!file.exists(paste0(e$output_folder))) {
        fs::dir_create(paste0(e$output_folder), mode = "u=rwx,go=rwx")
      }
      # e$out_log_file_path <- paste0(fs::path(output_path), "/log_files")
      # if (!file.exists(paste0(e$out_log_file_path))) {
      #   fs::dir_create(paste0(e$out_log_file_path), mode = "u=rwx,go=rwx")
      # }
      # logfile()
    }
  
    with_golem_options(
        app = shinyApp(
            ui = app_ui,
            server = app_server,
            onStart = onStart,
            options = options,
            enableBookmarking = enableBookmarking,
            uiPattern = uiPattern
        ),
        golem_opts = list(...)
    )
}
