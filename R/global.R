## global.R

## Declare global variables
used_columns <- NULL
output_folder <- NULL
..XColumns <- NULL
..temp.names <- NULL
Functionality <- NULL
dataInputColumnsTemp <- NULL
seq1 <- NULL
seq2 <- NULL
msg <- NULL
SpecificConvergentEvolution <- NULL
Freq <- NULL
freqTables_datasets <- NULL
motif_datasets <- NULL
motif_all <- NULL
Summary.V.REGION.identity.. <- NULL
Summary.Sequence <- NULL
cluster_id <- NULL
germline <- NULL
productive <- NULL
clonotype <- NULL
N <- NULL
Num_of_patients <- NULL
start_char <- NULL
end_char <- NULL
SHM_high_similarity <- NULL
filteredData_id <- NULL
mutational_status_table_allData <- NULL
cl_ids_logos <- NULL
frequenciesTables_results_cl <- NULL
mutation_results_nt <- NULL
higly_sim_clonotypes_barchart_down_threshold <- NULL
cdr3_length_distribution <- NULL
pi_distribution <- NULL
logo_plot <- NULL
logo_per_region <- NULL
Dataset <- NULL
logo_result_cl <- NULL
logo_per_region_cl <- NULL

tmp_path <- getwd() # change it to "/tmp" for server

num_of_cores <- parallel::detectCores(all.tests = FALSE, logical = TRUE) # change this to the custom number of threads

# logfile
if (!file.exists(paste0(tmp_path, "/inst/extdata/log_files"))) {
    dir.create(paste0(tmp_path, "/inst/extdata/log_files"))
}
logFile <- paste0(tmp_path, "/inst/extdata/log_files/log_file_", trunc(as.numeric(Sys.time())), ".txt")
cat(paste0("Function", "\t", "Parameters", "\t", "Num of input rows", "\t", "Num of input columns", "\t", "Start time", "\t", "End time", "\t", "Memory used"), file = logFile, append = FALSE, sep = "\n")

use_only_useful_columns <- TRUE # libraries.R TRUE
save_lists_for_bookmark <- FALSE # libraries.R FALSE
save_tables_individually_filter_in <- FALSE # libraries.R TRUE
save_tables_individually <- FALSE # libraries.R TRUE

if (save_tables_individually | save_lists_for_bookmark) {
    # output folder
    output_folder <- paste0(getwd(), "/inst/extdata/output/output_tables_", trunc(as.numeric(Sys.time())))
    if (!file.exists(paste0(output_folder))) {
        dir.create(paste0(output_folder))
    }
}

## Instead of simple assignments (used_columns <- list(), etc.)
## assign() fixes locked binding error when package is installed
assign("used_columns", list(), inherits = TRUE, envir = parent.env(environment()))
assign("all_used_columns", c(), inherits = TRUE, envir = parent.env(environment()))

## JS Code for enabling and diabling
jscode <- "shinyjs.disabletab =function(name){
$('ul li:has(a[data-value= name])').addClass('disabled');
$('.nav li.disabled a').prop('disabled',true)
}

shinyjs.enabletab =function(name){
$('.nav li.disabled a').prop('disabled',false)
$('ul li:has(a[data-value= name])').removeClass('disabled');
} "