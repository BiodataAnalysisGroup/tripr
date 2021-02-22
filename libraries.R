library(plyr)
library(dplyr)
library(data.table)   
library(stringr)
library(tidyr)
library(Biostrings)
library(plotly)
library(xtable)
library(plot3D)
library(gridExtra)
library(RColorBrewer)
library('stringdist')
library("parallel")

tmp_path <- getwd()

num_of_cores <- detectCores(all.tests = FALSE, logical = TRUE) #change this to the custom number of threads

if(!file.exists(paste0(tmp_path,"/log_files"))){ 
  dir.create(paste0(tmp_path,"/log_files"))
}
logFile = paste0(tmp_path,"/log_files/log_file ",trunc(as.numeric(Sys.time())),".txt")
cat(paste0("Function","\t","Parameters","\t","Num of input rows","\t","Num of input columns","\t","Start time","\t","End time","\t","Memory used"), file=logFile, append=FALSE, sep = "\n")

use_only_useful_columns <- T

save_tables_individually_filter_in <- T

save_lists_for_bookmark <- F  

save_tables_individually <- T
if (save_tables_individually | save_lists_for_bookmark){
  #output folder 
  output_folder=paste0(getwd(),"/output_tables ",trunc(as.numeric(Sys.time())))
  if(!file.exists(paste0(output_folder))){ 
    dir.create(paste0(output_folder))
  }
}

used_columns<-list()
all_used_columns<-c()

