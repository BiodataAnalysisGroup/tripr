library("parallel")

enableBookmarking(store = "server")

tmp_path<-getwd() #change it to "/tmp" for server

num_of_cores <- detectCores(all.tests = FALSE, logical = TRUE) #change this to the custom number of threads

#logfile
if(!file.exists(paste0(tmp_path,"/log_files"))){ 
  dir.create(paste0(tmp_path,"/log_files"))
}
logFile = paste0(tmp_path,"/log_files/log_file ",trunc(as.numeric(Sys.time())),".txt")
cat(paste0("Function","\t","Parameters","\t","Num of input rows","\t","Num of input columns","\t","Start time","\t","End time","\t","Memory used"), file=logFile, append=FALSE, sep = "\n")

use_only_useful_columns=T

save_lists_for_bookmark=F

save_tables_individually_filter_in <- F

save_tables_individually=F
if (save_tables_individually | save_lists_for_bookmark){
  #output folder 
  output_folder=paste0(getwd(),"/output_tables ",trunc(as.numeric(Sys.time())))
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

#save a list
#x <- list(x=1:10, y=(1:10))
#save(x, file="x.rdata")
#rm(x)

#load("x.rdata") #1
#y <- x          #2
#rm(x)           #3
