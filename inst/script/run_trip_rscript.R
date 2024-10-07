#!/usr/bin/env Rscript

# For Rscript on Windows cmd
# $> Rscript run_trip_rscript.R

# # If it doesn't work, you have to find the path to Rscript
# $> path\to\Rscript.exe" path\to\run_trip_rscript.R


library(tripr)

message("See run_TRIP() documentation by typing '?tripr::run_TRIP'")

# Change the default values with your preferred ones

run_TRIP(
    # Replace the datapath with yours like (if it's on your working directory):
    # datapath=fs::path_wd("dataset"),
    datapath=fs::path_package("extdata", "dataset", package="tripr"),
    output_path=fs::path_home("Documents/tripr_output"), 
    filelist=c("1_Summary.txt", "2_IMGT-gapped-nt-sequences.txt", 
        "4_IMGT-gapped-AA-sequences.txt", "6_Junction.txt"),
    cell="Bcell", 
    throughput="High Throughput", 
    preselection="1,4C:W", 
    selection="5", 
    identity_range="85:100", 
    vgenes="", 
    dgenes="", 
    jgenes="", 
    cdr3_length_range="", 
    aminoacid="",
    pipeline="1", 
    select_clonotype="V Gene + CDR3 Amino Acids", 
    highly_sim_params=paste0("1-1 2-1 3-1 4-1 5-1 6-1 7-1 8-1 9-1 10-1 11-1 ",
        "12-1 13-1 14-1 15-2 16-2 17-2 18-2 19-2 20-2 21-2 23-2 24-2 25-2 ",
        "26-2 27-2 28-2 29-3 30-3 31-3 32-3 33-3 34-3 35-3 36-3 37-3 38-3 ",
        "39-3 40-3 41-3 42-3 43-3 44-3 45-3 46-3 47-3 48-3 49-3 50-3,1,Yes"), 
    shared_clonotypes_params="reads,1,Yes", 
    highly_shared_clonotypes_params="reads,1,Yes", 
    repertoires_params="1,4,6", 
    identity_groups="85:97,97:99,99:100,100:100",
    multiple_values_params="2:7,2:3,2:5,2:11", 
    alignment_params="1,both,1,2:20", 
    mutations_params="both,0.5,0.5,2:20")
    