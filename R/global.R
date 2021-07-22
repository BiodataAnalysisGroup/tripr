## global.R

tmp_path<-getwd() #change it to "/tmp" for server

num_of_cores <- parallel::detectCores(all.tests = FALSE, logical = TRUE) #change this to the custom number of threads

#logfile
if(!file.exists(paste0(tmp_path,"/inst/extdata/log_files"))){ 
  dir.create(paste0(tmp_path,"/inst/extdata/log_files"))
}
logFile = paste0(tmp_path,"/inst/extdata/log_files/log_file_",trunc(as.numeric(Sys.time())),".txt")
cat(paste0("Function","\t","Parameters","\t","Num of input rows","\t","Num of input columns","\t","Start time","\t","End time","\t","Memory used"), file=logFile, append=FALSE, sep = "\n")

use_only_useful_columns = TRUE #libraries.R TRUE
save_lists_for_bookmark = FALSE #libraries.R FALSE
save_tables_individually_filter_in = FALSE #libraries.R TRUE
save_tables_individually = FALSE #libraries.R TRUE

if (save_tables_individually | save_lists_for_bookmark){
  #output folder 
  output_folder=paste0(getwd(),"/inst/extdata/output/output_tables_",trunc(as.numeric(Sys.time())))
  if(!file.exists(paste0(output_folder))){ 
    dir.create(paste0(output_folder))
  }
}

used_columns<-list()
all_used_columns<-c()

##JS Code for enabling and diabling
jscode <- "shinyjs.disabletab =function(name){
$('ul li:has(a[data-value= name])').addClass('disabled');
$('.nav li.disabled a').prop('disabled',true)
}

shinyjs.enabletab =function(name){
$('.nav li.disabled a').prop('disabled',false)
$('ul li:has(a[data-value= name])').removeClass('disabled');
} " 
