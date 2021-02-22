
testColumnNames<-function(name, files, datapath){ 
  d="Datasets loaded:" 
  for (i in 1:length(name)){
    d=paste(d,name[i])
  }
  #logfile
  #logFile = paste0(getwd(),"/log_file ",trunc(as.numeric(Sys.time())),".txt")
  #cat(paste0("Function","\t","Parameters","\t","Num of input rows","\t","Num of input columns","\t","Start time","\t","End time","\t","Memory used"), file=logFile, append=FALSE, sep = "\n")
  cat(paste0("testColumnNames","\t"), file=logFile, append=TRUE)
  cat(paste0(d,"\t"), file=logFile, append=TRUE)
  cat(paste0("read data","\t"), file=logFile, append=TRUE)
  cat(paste0("read data","\t"), file=logFile, append=TRUE)
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  
  #used columns
  if (use_only_useful_columns) input<-read.csv("param/used_columns_only_useful.csv", sep=";", stringsAsFactors=FALSE,header=F)
  else input<-read.csv("param/used_columns.csv", sep=";", stringsAsFactors=FALSE,header=F)
  
  used_columns<-list()
  all_used_columns<-c()
  for (i in seq(1,29,3)){
    file_name=paste0(input[i,1],".txt")
    input[i,1]=strsplit(input[i,1],"_")[[1]][2]
    input[i,1]=make.names(input[i,1])
    input[i+1,]=make.names(input[i+1,])
    a=input[i+1,which(input[i+1,]!="X")]
    a=paste0(input[i,1],".",a)
    used_columns[[input[i,1]]]=paste0(input[i,1],".",input[i+1,])
    
    if (file_name %in% files){
      all_used_columns=c(all_used_columns,a[which(!str_detect(a,".NA."))])
    }
    
  } 
  all_used_columns=c("dataName",all_used_columns)
  all_used_columns<<-all_used_columns
  used_columns<<-used_columns
  #save(used_columns,file=paste0(output_folder,"/used_columns.rData"))
  
  #filter_column contains the columns that are used for each one of the 9 filters with ids=1:9
  filter_column=c()
  if ("1_Summary.txt" %in% files){
    filter_column=c(used_columns[["Summary"]][3],used_columns[["Summary"]][18],used_columns[["Summary"]][2],used_columns[["Summary"]][18],used_columns[["Summary"]][4],used_columns[["Summary"]][3], used_columns[["Summary"]][8], used_columns[["Summary"]][11], used_columns[["Summary"]][15], used_columns[["Summary"]][18])
  }
  
  #for (i in 1:length(name))
  #name[i]<-strsplit(name[i],"_")[[1]][1]
  
  # Log start time and memory currently used
  start.time <- Sys.time()
  #mem_used()
  
  rawDataSet <- list()
  count_wrong=0
  worng_columns_id <- list()
  worng_columns_names <- list()
  wrong_dataset<-c()
  
  # Load datasets individually
  for (i in 1:length(name)){
    #name[i]=strsplit(name[i],"_")[[1]][1]
    firstSepData=T
    if (strsplit(name[i],"_")[[1]][1]!=name[i] && as.numeric(strsplit(name[i],"_")[[1]][2])>1) firstSepData=F
    rawDataSet[[name[i]]] <- data.frame(dataName=strsplit(name[i],"_")[[1]][1])
    worng_columns_names_temp<-c()
    worng_columns_id_temp<-c()
    
    num_initial_col=0
    
    for (j in 1:length(files)){
      b=strsplit(files[j],"_")
      b2<-gsub(".txt","",b[[1]][2])
      b2<-gsub("-",".",b2)
      var.name<-b2
      temp <- data.frame(assign(var.name,read.csv(paste0(datapath,"/",name[i],"/",files[j]),sep="\t", stringsAsFactors=FALSE)))
      
      num_initial_col=num_initial_col+ncol(temp)
      
      colnames(temp)<-paste0(b2, ".",colnames(temp))
      if (paste0(b2,".X") %in% colnames(temp)){
        temp <- temp[,-match(paste0(b2,".X"),names(temp))]
      }
      
      rawDataSet[[name[i]]] <- data.frame(rawDataSet[[name[i]]],temp)
      
      #Check the column names
      for (k in 1:length(used_columns[[b2]])){
        if ((used_columns[[b2]][k] %in% colnames(rawDataSet[[name[i]]])) || str_detect(used_columns[[b2]][k],".NA") || str_detect(used_columns[[b2]][k],".X")){
          #do nothing
        }else{
          message="wrong column names"
          tmp2=paste0(files[j],": ",gsub("[.]"," ",gsub(b2,"",used_columns[[b2]][k])))
          if (tmp2  %in% worng_columns_names_temp){
            
          }else{
            worng_columns_names_temp <-c(worng_columns_names_temp, tmp2 )
            worng_columns_id_temp <-c(worng_columns_id_temp,which(all_used_columns %in% used_columns[[b2]][k]))
          }
        }
      }
    }
    
    #mem_used()  # log memory used  
    
    #if ("Summary.Deleted.n.nt" %in% colnames(rawDataSet[[name[i]]])){
    #rawDataSet[[name[i]]] <- rawDataSet[[name[i]]] %>% select(-c(Summary.Deleted.n.nt))
    #}
    #if ("Summary.X" %in% colnames(rawDataSet[[i]])){
    #  rawDataSet[[i]] <- rawDataSet[[i]] %>% select(-c(Summary.X))
    #}
    #if ("Nt.sequences.V.REGION.reading.frame" %in% colnames(rawDataSet[[name[i]]])){
    #rawDataSet[[name[i]]] <- rawDataSet[[name[i]]] %>% select(-c(Nt.sequences.V.REGION.reading.frame))
    #}
    
    if (length(worng_columns_id_temp)>0){
      wrong_dataset<-c(wrong_dataset,name[i])
      count_wrong=count_wrong+1
      worng_columns_id[[count_wrong]] <- worng_columns_id_temp
      worng_columns_names[[count_wrong]] <- worng_columns_names_temp
    }else{
      #Drop all the columns that will not be used
      rawDataSet[[name[i]]] <- rawDataSet[[name[i]]] %>% select(all_used_columns)
    }
    
    
  }
  
  for (i in 1:length(name)){
    #name[i]=strsplit(name[i],"_")[[1]][1]
    firstSepData=T
    if (strsplit(name[i],"_")[[1]][1]!=name[i] && as.numeric(strsplit(name[i],"_")[[1]][2])==0){
      newName=strsplit(name[i],"_")[[1]][1]
      rawDataSet[[newName]]=rawDataSet[[name[i]]]
      rawDataSet[[name[i]]]<-NULL
    }
    if (strsplit(name[i],"_")[[1]][1]!=name[i] && as.numeric(strsplit(name[i],"_")[[1]][2])>0){
      newName=strsplit(name[i],"_")[[1]][1]
      rawDataSet[[newName]]=rbind(rawDataSet[[newName]],rawDataSet[[name[i]]])
      rawDataSet[[name[i]]]<-NULL
    }
    num_of_datasets=length(rawDataSet)
    
  }
  
  newDatasetNames=names(rawDataSet)
  
  k=c()
  #check if the column names are correct
  
  # log time end and memory used 
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  cat(pryr::mem_used(), file=logFile, append=TRUE, sep = "\n")
  
  confirm="Data Loaded!"

  if (length(worng_columns_id)>0)
    return(list("confirm"=confirm,"logFile"=logFile,"message"="wrong column names","wrong_dataset"=wrong_dataset, "worng_columns_id"=worng_columns_id, "worng_columns_names"=worng_columns_names, "rawDataSet"=rawDataSet))
  #Correct the wrong column names 
  return(list("confirm"=confirm,"logFile"=logFile,"newDatasetNames"=newDatasetNames,"message"="","wrong_dataset"=c(),"worng_columns_id"=c(),"worng_columns_names"=c(), "rawDataSet"=rawDataSet))
  
}

######################################################################################################################################

correctColumnNames <- function(files,rawDataSet, allDatasets, wrong_dataset, new_columns=list(), worng_columns_id=list(),name){
  #print(list(files,rawDataSet, allDatasets, wrong_dataset, new_columns, worng_columns_id,name))
  nr=0
  for (i in names(rawDataSet)){
    nr=nrow(rawDataSet[[i]])+nr
  }
  
  #logfile
  cat(paste0("correctColumnNames","\t"), file=logFile, append=TRUE)
  cat(paste0("wrong datasets",paste(wrong_dataset,sep = ","),"\t"), file=logFile, append=TRUE)
  cat(paste0(nr,"\t"), file=logFile, append=TRUE)
  cat(paste0(ncol(rawDataSet[[names(rawDataSet)[1]]]),"\t"), file=logFile, append=TRUE)
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  
  #filter_column contains the columns that are used for each one of the 9 filters with ids=1:9
  filter_column=c(used_columns[["Summary"]][3],used_columns[["Summary"]][18],used_columns[["Summary"]][2],used_columns[["Summary"]][18],used_columns[["Summary"]][4],used_columns[["Summary"]][3], used_columns[["Summary"]][8], used_columns[["Summary"]][11], used_columns[["Summary"]][15], used_columns[["Summary"]][18])
  
  #used columns
  input<-read.csv("param/used_columns.csv", sep=";", stringsAsFactors=FALSE,header=F)
  
  used_columns<-list()
  all_used_columns<-c()
  for (i in seq(1,29,3)){
    file_name=paste0(input[i,1],".txt")
    input[i,1]=strsplit(input[i,1],"_")[[1]][2]
    input[i,1]=make.names(input[i,1])
    input[i+1,]=make.names(input[i+1,])
    a=input[i+1,which(input[i+1,]!="X")]
    a=paste0(input[i,1],".",a)
    used_columns[[input[i,1]]]=a
    
    if (file_name %in% files){
      all_used_columns=c(all_used_columns,a)
    }
    
  } 
  all_used_columns=c("dataName",all_used_columns)
  
  # Log start time and memory currently used
  start.time <- Sys.time()
  #mem_used()
  correct=0

  w=0
  for (i in 1:length(wrong_dataset)){
    w=w+length(worng_columns_id[[i]])
    #update columns
    #k=match(wrong_dataset,allDatasets)
    #wrong column names

    for (j in 1:length(new_columns[[i]])){
      #filter_column[worng_columns_id[i]]=new_columns[i]
      #return(wrong_dataset)
      if (length(which(names(rawDataSet[[wrong_dataset[i]]])==new_columns[[i]][j]))>0)
        correct=correct+1
      names(rawDataSet[[wrong_dataset[i]]])[which(names(rawDataSet[[wrong_dataset[i]]])==new_columns[[i]][j])]<-all_used_columns[worng_columns_id[[i]][j]]
    }
  }
  
  if (correct==w){
    correct="yes"
    for (i in 1:length(name))
      rawDataSet[[name[i]]] <- rawDataSet[[name[i]]] %>% select(all_used_columns)
  }else{
    correct="no"
  }

  # log time end and memory used 
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  cat(pryr::mem_used(), file=logFile, append=TRUE, sep = "\n")
  
  return(list("rawDataSet"=rawDataSet,"correct"=correct))
  
}


######################################################################################################################################

imgtcleaning <- function(rawDataSet, name, allDatasets, files, cell_id=1, filter_id=c(1,2,3,4,5,6,7,8,9,10), filter_out_char1=" P", filter_out_char2="[*]|X|#|[.]", filter_in_char="productive", filterStart="^*",filterEnd="*$", identityLow=95, identityHigh=100, VGene="", JGene="", DGene="", lengthLow=7, lengthHigh=15,  aminoacid="CASSPPDTGELFF", seq1=1,seq2=2,Tcell) {
  a="Filter ids "
  for (i in 1:length(filter_id)){
    a=paste0(a,",",filter_id[i])
  }
  nr=0
  for (i in names(rawDataSet)){
    nr=nrow(rawDataSet[[i]])+nr
  }
  #logfile
  cat(paste0("imgtcleaning","\t"), file=logFile, append=TRUE)
  cat(paste0(a,"\t"), file=logFile, append=TRUE)
  cat(paste0(nr,"\t"), file=logFile, append=TRUE)
  cat(paste0(ncol(rawDataSet[[names(rawDataSet)[1]]]),"\t"), file=logFile, append=TRUE)
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  
  cleaning_criteria=c("Functional V-Gene", "CDR3 with no Special Characters","Productive Sequence", "Productive Sequences")
  
  #worng_columns_id=c(1,2)
  
  #return(paste0("filterStart= ",filterStart,"filterEnd= ",filterEnd,"VGene=",VGene,"lala"))
  
  #filter_column contains the columns that are used for each one of the 9 filters with ids=1:9
  filter_column=c(used_columns[["Summary"]][3],used_columns[["Summary"]][18],used_columns[["Summary"]][2],used_columns[["Summary"]][18],used_columns[["Summary"]][4],used_columns[["Summary"]][3], used_columns[["Summary"]][8], used_columns[["Summary"]][11], used_columns[["Summary"]][15], used_columns[["Summary"]][18],"Summary.V.REGION.identity....with.ins.del.events.")
  filterOut<-list()
  workflow<- matrix(0,length(filter_id),3)
  
  # Log start time and memory currently used
  start.time <- Sys.time() 
  #mem_used()
  
  #usedDatasetId=match(name,allDatasets)
  # Combine raw name
  for (i in 1:length(name)){
    if (i==1){
      allData <- rawDataSet[[name[i]]];
    }else{
      allData <- rbind(allData, rawDataSet[[name[i]]]);
    }
  }
  
  test_column=c(filter_column[1], filter_column[2], filter_column[5], used_columns[["Nt.sequences"]][1] )
  
  #used_column=c("dataName")
  #for (i in 1:length(filter_id)){
  #used_column=c(used_column,filter_column[filter_id[i]])
  #}
  
  #used_column=c(used_column,test_column)
  #return(names(allData))
  
  #Drop all the columns that will not be used
  #unusedColumns <- allData[,-match(used_column,names(allData))]
  #allData <- allData %>% select(used_column)
  
  # IMPORTANT: Datasets should start with T
  # dataSetIDs <- as.list(levels(unique(allData$dataName))) 
  
  allDataInitial<-allData
  workflow_datasets<-list()
  
  a=matrix(0,length(filter_id),3)
  for (j in 1:length(name)){
    workflow_datasets[[name[j]]]=a
  }
  
  #Take only the first gene (V, J D) (Separated with "or")
  a=which(str_detect(allData[[filter_column[1]]]," or|,"))
  if (length(a)>0){
    a2=strsplit(allData[[filter_column[1]]][a]," or|,") 
    allData[[filter_column[1]]][a]=as.character(ldply(a2,function(s){t(data.frame(unlist(s)))})[,1])
  }
  
  a=which(str_detect(allData[[filter_column[7]]]," or|,"))
  if (length(a)>0){
    a2=strsplit(allData[[filter_column[7]]][a]," or|,") 
    allData[[filter_column[7]]][a]=as.character(ldply(a2,function(s){t(data.frame(unlist(s)))})[,1])
  }
  
  if (!all(is.na(allData[[filter_column[8]]]))){
    a=which(str_detect(allData[[filter_column[8]]]," or|,"))
    if (length(a)>0){
      a2=strsplit(allData[[filter_column[8]]][a]," or|,") 
      allData[[filter_column[8]]][a]=as.character(ldply(a2,function(s){t(data.frame(unlist(s)))})[,1])
    }
  }
  
  #Remove (see comment) from AA.JUNCTION
  a=which(str_detect(allData[[filter_column[2]]]," [(]see"))
  if (length(a)>0){
    a2=strsplit(allData[[filter_column[2]]][a]," [(]see") 
    allData[[filter_column[2]]][a]=as.character(ldply(a2,function(s){t(data.frame(unlist(s)))})[,1])
  }
  
  # Apply the requested filters
  if (any(filter_id==1)){
    # unique(datapoint$1_Summary.txt.V.GENE.and.allele)  -> Filter out " P "  e.g. Homsap TRBV21-1*01 P (see comment)"
    i=which(filter_id==1)
    filterOut[[1]] <- allData %>% filter(str_detect(allData[[filter_column[1]]], filter_out_char1))
    if (nrow(filterOut[[1]])>0) filterOut[[1]]=cbind(filterOut[[1]],FilterId=cleaning_criteria[1])
    #workflow[i,3]=nrow(allData)-nrow(filterOut[[filter_id[i]]])
    
    allData <- allData %>% filter(!str_detect(allData[[filter_column[1]]], filter_out_char1))
    
    workflow[i,1]=filter_id[i]
    workflow[i,2]=nrow(filterOut[[1]])
    workflow[i,3]=nrow(allData)
    
    for (j in 1:length(name)){
      if (nrow(filterOut[[1]])>0) filterOut_datasets<-filterOut[[1]] %>% filter(filterOut[[1]]$dataName==name[j])
      else filterOut_datasets<-filterOut[[1]]
      if (i==1) prev=nrow(rawDataSet[[name[j]]])
      else prev=(workflow_datasets[[name[j]]][i-1,3])
      workflow_datasets[[name[j]]][i,3]=prev-nrow(filterOut_datasets) 
      workflow_datasets[[name[j]]][i,2]=nrow(filterOut_datasets)
      workflow_datasets[[name[j]]][i,1]=filter_id[i]
    }
    
  }
  
  if (any(filter_id==2)){
    i=which(filter_id==2)
    filterOut[[2]] <- allData %>% filter(str_detect(allData[[filter_column[2]]], filter_out_char2))
    if (nrow(filterOut[[2]])>0) filterOut[[2]]=cbind(filterOut[[2]],FilterId=cleaning_criteria[2])
    
    #workflow[i,3]=nrow(allData)-nrow(filterOut[[filter_id[i]]])
    allDataInitial_tmp=allData
    allData <- allData %>% filter(!str_detect(allData[[filter_column[2]]], filter_out_char2))
    
    workflow[i,1]=filter_id[i]
    workflow[i,2]=nrow(filterOut[[filter_id[i]]])
    workflow[i,3]=nrow(allData)
    
    for (j in 1:length(name)){
      if (nrow(filterOut[[2]])>0) filterOut_datasets<-filterOut[[2]] %>% filter(filterOut[[2]]$dataName==name[j])
      else filterOut_datasets<-filterOut[[2]]
      if (i==1) prev=nrow(rawDataSet[[name[j]]])
      else prev=workflow_datasets[[name[j]]][i-1,3]
      workflow_datasets[[name[j]]][i,3]=prev-nrow(filterOut_datasets) 
      workflow_datasets[[name[j]]][i,2]=nrow(filterOut_datasets)
      workflow_datasets[[name[j]]][i,1]=filter_id[i]
    }
  }
  
  if (any(filter_id==3)){
    # unique(datapoint$1_Summary.txt.Functionality)  -> Filter in "productive" and "productive (see comment)"
    i=which(filter_id==3)
    
    if (Tcell){
      filterOut[[3]] <- allData %>% filter(!str_detect(allData[[filter_column[3]]], "^productive$"))
      allData <- allData %>% filter(str_detect(allData[[filter_column[3]]], "^productive$"))
    }else{
      filterOut[[3]] <- allData %>% filter(!str_detect(allData[[filter_column[3]]], "^productive"))
      
      allData <- allData %>% filter(str_detect(allData[[filter_column[3]]], "^productive"))
      
      ins_del<-which(!is.na(allData[[filter_column[11]]]))
      
      if (length(ins_del)>0){
        #check if V-REGION deletions	or V-REGION insertions contain (cause frameshift)
        delete = allData[ins_del,] %>% filter(str_detect(allData[[used_columns[["Summary"]][21]]][ins_del],"[(]cause frameshift[)]"))
        delete2 = allData[ins_del,] %>% filter(str_detect(allData[[used_columns[["Summary"]][22]]][ins_del],"[(]cause frameshift[)]"))
        delete_all=unique(rbind(delete,delete2))
        
        filterOut[[3]]<-unique(rbind(filterOut[[3]],delete_all))
        allData<-allData %>% filter(!(allData[[used_columns[["Summary"]][1]]] %in%  filterOut[[3]][[used_columns[["Summary"]][1]]]))
      }
    }
    
    if (nrow(filterOut[[3]])>0) filterOut[[3]]=cbind(filterOut[[3]],FilterId=cleaning_criteria[3])
    
    workflow[i,3]=nrow(allData)
    workflow[i,1]=filter_id[i]
    workflow[i,2]=nrow(filterOut[[3]])
    
    for (j in 1:length(name)){
      if (nrow(filterOut[[3]])>0) filterOut_datasets<-filterOut[[3]] %>% filter(filterOut[[3]]$dataName==name[j])
      else filterOut_datasets<-filterOut[[3]]
      if (i==1) prev=nrow(rawDataSet[[name[j]]])
      else prev=(workflow_datasets[[name[j]]][i-1,3])
      workflow_datasets[[name[j]]][i,3]=(prev)-nrow(filterOut_datasets) 
      workflow_datasets[[name[j]]][i,2]=nrow(filterOut_datasets)
      workflow_datasets[[name[j]]][i,1]=filter_id[i]
    }
  }
  
  if (any(filter_id==4)){
    # datapoint$1_Summary.txt.AA.JUNCTION  -> Filter in "Start with C and end with F"
    i=which(filter_id==4)
    if (filterStart=="" && filterEnd==""){
      filterOut[[4]] <- allData %>% filter(!(str_detect(allData[[filter_column[4]]], filterStart)))
      #if (nrow(filterOut[[4]])>0) filterOut[[4]]=cbind(filterOut[[4]],FilterId=cleaning_criteria[4])
      #workflow[i,3]=nrow(allData)-nrow(filterOut[[filter_id[i]]])
    }else if (filterEnd==""){
      filterOut[[4]] <- allData %>% filter(!(str_detect(allData[[filter_column[4]]], filterStart)))
      #workflow[i,3]=nrow(allData)-nrow(filterOut[[filter_id[i]]])
      allData <- allData %>% filter((str_detect(allData[[filter_column[4]]], filterStart)))
    }else if (filterStart==""){
      filterOut[[4]] <- allData %>% filter(!str_detect(allData[[filter_column[4]]], filterEnd))
      #workflow[i,3]=nrow(allData)-nrow(filterOut[[filter_id[i]]])
      allData <- allData %>% filter(str_detect(allData[[filter_column[4]]], filterEnd))
    }else{
      filterOut[[4]] <- allData %>% filter(!(str_detect(allData[[filter_column[4]]], filterStart)))
      filterOut[[4]] <- rbind(filterOut[[4]],(allData %>% filter(!(str_detect(allData[[filter_column[4]]], filterEnd)))))
      #workflow[i,3]=nrow(allData)-nrow(filterOut[[filter_id[i]]])
      allData <- allData %>% filter((str_detect(allData[[filter_column[4]]], filterStart)))
      allData <- allData %>% filter(str_detect(allData[[filter_column[4]]], filterEnd))
    }
    if (nrow(filterOut[[4]])>0) filterOut[[4]]=cbind(filterOut[[4]],FilterId=cleaning_criteria[4])
    
    
    workflow[i,1]=filter_id[i]
    workflow[i,2]=nrow(filterOut[[4]])
    workflow[i,3]=nrow(allData)
    
    for (j in 1:length(name)){
      if (nrow(filterOut[[4]])>0) filterOut_datasets<-filterOut[[4]] %>% filter(filterOut[[4]]$dataName==name[j])
      else filterOut_datasets<-filterOut[[4]]
      if (i==1) prev=nrow(rawDataSet[[name[j]]])
      else prev=workflow_datasets[[name[j]]][i-1,3]
      workflow_datasets[[name[j]]][i,3]=nrow(allData %>% filter(allData$dataName==name[j]))
      workflow_datasets[[name[j]]][i,2]=prev-nrow(allData %>% filter(allData$dataName==name[j]))
      workflow_datasets[[name[j]]][i,1]=filter_id[i]
    }
  }
  
  filterOutSum<-c()
  if (length(filter_id)>0)
    for (i in 1:length(filter_id)){
      if (i==1)
        filterOutSum<-filterOut[[filter_id[1]]]
      else
        filterOutSum<-rbind(filterOutSum,filterOut[[filter_id[i]]])
    }
  
  a<-name[1]
  if (length(name)>1)
    for (i in 2:length(name)){a<-paste0(a,", ",name[i])}
  
  b<-filter_id[1]
  if (length(filter_id)>1)
    for (i in 2:length(filter_id)){b<-paste0(b,", ",filter_id[i])}
  
  #Separate allData to different tables
  initial_datasets<-list()
  for (i in 1:length(name)){
    initial_datasets[[name[i]]]<- allDataInitial %>% filter(allDataInitial$dataName==name[i])
  }
  
  cleaned_datasets<-list()
  if (length(allData)>0){
    for (i in 1:length(name)){
      cleaned_datasets[[name[i]]]<- allData %>% filter(allData$dataName==name[i])
    }
  }
  
  cleaned_out_datasets<-list()
  if (length(filterOutSum)>0){
    for (i in 1:length(name)){
      cleaned_out_datasets[[name[i]]]<- filterOutSum %>% filter(filterOutSum$dataName==name[i])
    }
  }
  
  confirm<-paste0("Datasets cleaned: ",a,". Cleaning Filters applied: ",b)
  
  # log time end and memory used 
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  cat(pryr::mem_used(), file=logFile, append=TRUE, sep = "\n")
  
  result=list("message"="","dim"=dim(allData), "workflow"=workflow, "workflow_datasets"=workflow_datasets, "allDataInitial"=allDataInitial, "allData"=allData, "filterOutSum"=filterOutSum, "initial_datasets"=initial_datasets, "cleaned_datasets"=cleaned_datasets, "cleaned_out_datasets"=cleaned_out_datasets, "confirm"=confirm)
  return(result)
  
}


######################################################################################################################################

imgtfilter <- function(rawDataSet,name, allData, cell_id=1, filter_id=c(5,6,7,8,9,10), filter_out_char1=" P", filter_out_char2="[:punct:]|X", filter_in_char="productive", filterStart="^*",filterEnd="*$", identityLow=95, identityHigh=100, VGene="", JGene="", DGene="", lengthLow=7, lengthHigh=15,  aminoacid="CASSPPDTGELFF", seq1=1,seq2=2) {
  #logfile
  a="Filter ids "
  for (i in 1:length(filter_id)){
    a=paste0(a,",",filter_id[i])
  }
  cat(paste0("imgtfilter","\t"), file=logFile, append=TRUE)
  cat(paste0(a,"\t"), file=logFile, append=TRUE)
  cat(paste0(nrow(allData),"\t"), file=logFile, append=TRUE)
  cat(paste0(ncol(rawDataSet[[names(rawDataSet)[1]]]),"\t"), file=logFile, append=TRUE)
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  
  cleaning_criteria=c("Functional V-Gene", "CDR3 with no Special Characters","Productive Sequence", "Productive Sequences")
  filtering_criteria=c("V-REGION identity %","Specific V Gene","Specific J Gene", "Specific D Gene","CDR3 length range","CDR3 length range")
  criteria=c(cleaning_criteria,filtering_criteria)
  
  #filter_column contains the columns that are used for each one of the 9 filters with ids=1:9
  filter_column=c(used_columns[["Summary"]][3],used_columns[["Summary"]][18],used_columns[["Summary"]][2],used_columns[["Summary"]][18],used_columns[["Summary"]][4],used_columns[["Summary"]][3], used_columns[["Summary"]][8], used_columns[["Summary"]][11], used_columns[["Summary"]][15], used_columns[["Summary"]][18], "Summary.V.REGION.identity....with.ins.del.events.")
  filterOut<-list()
  workflow<- matrix(0,length(filter_id),3)
  
  # Log start time and memory currently used
  start.time <- Sys.time()
  #mem_used()
  
  test_column=c(filter_column[1], filter_column[2], filter_column[5], used_columns[["Nt.sequences"]][1] )
  
  used_column=c("dataName")
  for (i in 1:length(filter_id)){
    used_column=c(used_column,filter_column[filter_id[i]])
  }
  
  #used_column=c(used_column,test_column)
  #return(names(allData))
  
  #Drop all the columns that will not be used
  #unusedColumns <- allData[,-match(used_column,names(allData))]
  #allData <- allData %>% select(used_column)
  
  # IMPORTANT: Datasets should start with T
  # dataSetIDs <- as.list(levels(unique(allData$dataName))) 
  
  allDataInitial<-allData
  
  workflow_datasets<-list()
  
  a=matrix(0,length(filter_id),3)
  for (j in 1:length(name)){
    workflow_datasets[[name[j]]]=a
  }
  
  # Apply the requested filters

  if (any(filter_id==5)){
    # datapoint$1_Summary.txt.V.REGION.identity..  -> Filter in value between 95 and 100
    i=which(filter_id==5)
    ins_del<-which(!is.na(allData[[filter_column[11]]]))
    
    if (length(ins_del)>0){
      allData[[filter_column[5]]][ins_del]<-allData[[filter_column[11]]][ins_del]
    }
    
    filterOut[[5]] <- allData[ which(allData[[filter_column[5]]] > identityHigh | allData[[filter_column[5]]] < identityLow),]
    allData <-  allData[ which(allData[[filter_column[5]]] <= identityHigh & allData[[filter_column[5]]] >= identityLow),]
    
    if (nrow(filterOut[[5]])>0) filterOut[[5]]=cbind(filterOut[[5]],FilterId=criteria[5])
    workflow[i,3]=nrow(allData)
    
    
    #write.table(filterOut5, file = "filterOut5.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
    
    workflow[i,1]=filter_id[i]-4
    workflow[i,2]=nrow(filterOut[[filter_id[i]]])
    
    for (j in 1:length(name)){
      if (nrow(filterOut[[5]])>0) filterOut_datasets<-filterOut[[5]] %>% filter(filterOut[[5]]$dataName==name[j])
      else filterOut_datasets<-filterOut[[5]]
      if (i==1){ prev=nrow(rawDataSet[[name[j]]])
      }
      else prev=workflow_datasets[[name[j]]][i-1,3]
      workflow_datasets[[name[j]]][i,3]=(prev)-nrow(filterOut_datasets) 
      workflow_datasets[[name[j]]][i,2]=nrow(filterOut_datasets)
      workflow_datasets[[name[j]]][i,1]=filter_id[i]-4
    }
  }
  
  if (any(filter_id==6)){
    # datapoint$1_Summary.txt.V-GENE.and.allele -> Filter in specific V Genes
    i=which(filter_id==6)
    filterOut[[6]] <- allData %>% filter(!str_detect(allData[[filter_column[6]]], gsub("[(]","[(]",gsub("[)]","[)]",gsub("[*]","[*]", VGene)))) )
    if (nrow(filterOut[[6]])>0) filterOut[[6]]=cbind(filterOut[[6]],FilterId=criteria[6])
    workflow[i,3]=nrow(allData)-nrow(filterOut[[filter_id[i]]])
    allData <- allData %>% filter(str_detect(allData[[filter_column[6]]], gsub("[(]","[(]",gsub("[)]","[)]",gsub("[*]","[*]", VGene)))) )
    
    #write.table(filterOut16, file = "filterOut6.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
    
    workflow[i,1]=filter_id[i]-4
    workflow[i,2]=nrow(filterOut[[filter_id[i]]])
    
    for (j in 1:length(name)){
      if (nrow(filterOut[[6]])>0) filterOut_datasets<-filterOut[[6]] %>% filter(filterOut[[6]]$dataName==name[j])
      else filterOut_datasets<-filterOut[[6]] 
      if (i==1) prev=nrow(rawDataSet[[name[j]]])
      else prev=workflow_datasets[[name[j]]][i-1,3]
      workflow_datasets[[name[j]]][i,3]=(prev)-nrow(filterOut_datasets) 
      workflow_datasets[[name[j]]][i,2]=nrow(filterOut_datasets)
      workflow_datasets[[name[j]]][i,1]=filter_id[i]-4
    }
  }
  
  if (any(filter_id==7)){
    # datapoint$1_Summary.txt.J-GENE.and.allele  -> Filter in specific J Genes
    i=which(filter_id==7)
    filterOut[[7]] <- allData %>% filter(!str_detect(allData[[filter_column[7]]], gsub("[(]","[(]",gsub("[)]","[)]",gsub("[*]","[*]", JGene)))) )
    if (nrow(filterOut[[7]])>0) filterOut[[7]]=cbind(filterOut[[7]],FilterId=criteria[7])
    workflow[i,3]=nrow(allData)-nrow(filterOut[[filter_id[i]]])
    allData <- allData %>% filter(str_detect(allData[[filter_column[7]]], gsub("[(]","[(]",gsub("[)]","[)]",gsub("[*]","[*]", JGene)))) )
    
    #write.table(filterOut7, file = "filterOut7.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
    
    workflow[i,1]=filter_id[i]-4
    workflow[i,2]=nrow(filterOut[[filter_id[i]]])
    
    for (j in 1:length(name)){
      if (nrow(filterOut[[7]])>0) filterOut_datasets<-filterOut[[7]] %>% filter(filterOut[[7]]$dataName==name[j])
      else filterOut_datasets<-filterOut[[7]]
      if (i==1) prev=nrow(rawDataSet[[name[j]]])
      else prev=workflow_datasets[[name[j]]][i-1,3]
      workflow_datasets[[name[j]]][i,3]=(prev)-nrow(filterOut_datasets) 
      workflow_datasets[[name[j]]][i,2]=nrow(filterOut_datasets)
      workflow_datasets[[name[j]]][i,1]=filter_id[i]-4
    }
  }
  
  if (any(filter_id==8)){
    # datapoint$1_Summary.txt.D-GENE.and.allele  -> Filter in specific D Genes
    i=which(filter_id==8)
    filterOut[[8]] <- allData %>% filter(!str_detect(allData[[filter_column[8]]], gsub("[(]","[(]",gsub("[)]","[)]",gsub("[*]","[*]", DGene)))) ) 
    if (nrow(filterOut[[8]])>0) filterOut[[8]]=cbind(filterOut[[8]],FilterId=criteria[8])
    workflow[i,3]=nrow(allData)-nrow(filterOut[[filter_id[i]]])
    allData <- allData %>% filter(str_detect(allData[[filter_column[8]]], gsub("[(]","[(]",gsub("[)]","[)]",gsub("[*]","[*]", DGene)))) )
    
    #write.table(filterOut8, file = "filterOut8.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
    
    workflow[i,1]=filter_id[i]-4
    workflow[i,2]=nrow(filterOut[[filter_id[i]]])
    
    for (j in 1:length(name)){
      if (nrow(filterOut[[8]])>0) filterOut_datasets<-filterOut[[8]] %>% filter(filterOut[[8]]$dataName==name[j])
      else  filterOut_datasets<-filterOut[[8]] 
      if (i==1) prev=nrow(rawDataSet[[name[j]]])
      else prev=workflow_datasets[[name[j]]][i-1,3]
      workflow_datasets[[name[j]]][i,3]=(prev)-nrow(filterOut_datasets) 
      workflow_datasets[[name[j]]][i,2]=nrow(filterOut_datasets)
      workflow_datasets[[name[j]]][i,1]=filter_id[i]-4
    }
  }
  
  if (any(filter_id==9)){
    # datapoint$CDR3-IMGT.length  -> Filter in value between 95 and 100
    i=which(filter_id==9)
    filterOut[[9]] <- allData[ which(as.numeric(allData[[filter_column[9]]]) > lengthHigh | as.numeric(allData[[filter_column[9]]]) < lengthLow),]
    if (nrow(filterOut[[9]])>0) filterOut[[9]]=cbind(filterOut[[9]],FilterId=criteria[9])
    workflow[i,3]=nrow(allData)-nrow(filterOut[[filter_id[i]]])
    allData <-  allData[ which(as.numeric(allData[[filter_column[9]]]) <= lengthHigh & as.numeric(allData[[filter_column[9]]]) >= lengthLow),]
    
    #return(lengthLow)
    #write.table(filterOut9, file = "filterOut9.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
    
    workflow[i,1]=filter_id[i]-4
    workflow[i,2]=nrow(filterOut[[filter_id[i]]])
    
    for (j in 1:length(name)){
      if (nrow(filterOut[[9]])>0) filterOut_datasets<-filterOut[[9]] %>% filter(filterOut[[9]]$dataName==name[j])
      else filterOut_datasets<-filterOut[[9]] 
      if (i==1) prev=nrow(rawDataSet[[name[j]]])
      else prev=workflow_datasets[[name[j]]][i-1,3]
      workflow_datasets[[name[j]]][i,3]=(prev)-nrow(filterOut_datasets) 
      workflow_datasets[[name[j]]][i,2]=nrow(filterOut_datasets)
      workflow_datasets[[name[j]]][i,1]=filter_id[i]-4
    }
  }
  
  if (any(filter_id==10)){
    # datapoint$1_Summary.txt.AA.JUNCTION  -> Filter in CDR3 containing specific amino-acid sequence
    i=which(filter_id==10)
    filterOut[[10]] <- allData %>% filter(!str_detect(allData[[filter_column[10]]], aminoacid))
    if (nrow(filterOut[[10]])>0) filterOut[[10]]=cbind(filterOut[[10]],FilterId=criteria[10])
    workflow[i,3]=nrow(allData)-nrow(filterOut[[filter_id[i]]])
    allData <- allData %>% filter(str_detect(allData[[filter_column[10]]], aminoacid))
    
    #write.table(filterOut10, file = "filterOut10.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
    
    workflow[i,1]=filter_id[i]-4
    workflow[i,2]=nrow(filterOut[[filter_id[i]]])
    
    for (j in 1:length(name)){
      if (nrow(filterOut[[10]])>0) filterOut_datasets<-filterOut[[10]] %>% filter(filterOut[[10]]$dataName==name[j])
      else  filterOut_datasets<-filterOut[[10]]
      if (i==1) prev=nrow(rawDataSet[[name[j]]])
      else prev=workflow_datasets[[name[j]]][i-1,3]
      workflow_datasets[[name[j]]][i,3]=(prev)-nrow(filterOut_datasets) 
      workflow_datasets[[name[j]]][i,2]=nrow(filterOut_datasets)
      workflow_datasets[[name[j]]][i,1]=filter_id[i]-4
    }
  }
  
  # Provide a 1_Summary.txt of the datasets
  # (allData %>% group_by(dataName) %>%  summarize(n()))[[2]][1]
  
  #write.table(allData, file = "allDataAfterFiltering.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
  
  
  #mem_used() # log memory used after filtering (no significant change should be evident)
  
  filterOutSum<-c()
  if (length(filter_id)>0)
    for (i in 1:length(filter_id)){
      if (i==1)
        filterOutSum<-filterOut[[filter_id[1]]]
      else
        filterOutSum<-rbind(filterOutSum,filterOut[[filter_id[i]]])
    }
  
  # Do the actual analysis - targeted tables
  
  a<-name[1]
  if (length(name)>1)
    for (i in 2:length(name)){a<-paste0(a,", ",name[i])}
  
  b<-filter_id[1]-4
  if (length(filter_id)>1)
    for (i in 2:length(filter_id)){b<-paste0(b,", ",(filter_id[i]-4))}
  
  #Delete (see comment) from genes
  a=which(str_detect(allData[[used_columns[["Summary"]][3]]]," or|,| [(]see"))
  if (length(a)>0){
    a2=strsplit(allData[[used_columns[["Summary"]][3]]][a]," or|,| [(]see") 
    allData[[used_columns[["Summary"]][3]]][a]=as.character(ldply(a2,function(s){t(data.frame(unlist(s)))})[,1])
  }
  
  a=which(str_detect(allData[[used_columns[["Summary"]][8]]]," or|,| [(]see"))
  if (length(a)>0){
    a2=strsplit(allData[[used_columns[["Summary"]][8]]][a]," or|,| [(]see") 
    allData[[used_columns[["Summary"]][8]]][a]=as.character(ldply(a2,function(s){t(data.frame(unlist(s)))})[,1])
  }
  
  if (!all(is.na(allData[[used_columns[["Summary"]][11]]]))){
    a=which(str_detect(allData[[used_columns[["Summary"]][11]]]," or|,| [(]see"))
    if (length(a)>0){
      a2=strsplit(allData[[used_columns[["Summary"]][11]]][a]," or|,| [(]see") 
      allData[[used_columns[["Summary"]][11]]][a]=as.character(ldply(a2,function(s){t(data.frame(unlist(s)))})[,1])
    }
  }
  
  
  #Separate allData to different tables
  initial_datasets<-list()
  for (i in 1:length(name)){
    initial_datasets[[name[i]]]<- allDataInitial %>% filter(allDataInitial$dataName==name[i])
  }
  
  filtered_datasets<-list()
  if (length(allData)>0){
    for (i in 1:length(name)){
      filtered_datasets[[name[i]]]<- allData %>% filter(allData$dataName==name[i])
      if (save_tables_individually_filter_in){
        write.table((filtered_datasets[[name[i]]]),paste0(output_folder,"/","filter_in_",name[i],".txt"),sep = "\t", row.names = FALSE, col.names = TRUE)
      }
    }
  }
  
  filtered_out_datasets<-list()
  if (length(filterOutSum)>0){
    for (i in 1:length(name)){
      filtered_out_datasets[[name[i]]]<- filterOutSum %>% filter(filterOutSum$dataName==name[i])
    }
  }
  
  a=""
  for (i in 1:length(name)){
    a=paste(a,name[i])
  }
  
  if (save_tables_individually_filter_in){
    write.table((allData),paste0(output_folder,"/","filter_in_All Data",".txt"),sep = "\t", row.names = FALSE, col.names = TRUE)
  }
  
  confirm<-paste0("Datasets filtered: ",a,". Filters applied: ",b)
  
  # log time end and memory used 
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  cat(pryr::mem_used(), file=logFile, append=TRUE, sep = "\n")
  
  result=list("message"="","dim"=dim(allData),"workflow"=workflow,"workflow_datasets"=workflow_datasets, "allDataInitial"=allDataInitial, "allData"=allData, "filterOutSum"=filterOutSum,"initial_datasets"=initial_datasets, "filtered_datasets"=filtered_datasets, "filtered_out_datasets"=filtered_out_datasets, "confirm"=confirm)
  return(result)
  
}



#################################################### Functions for Low throughput ####################################################

######################################################################################################################################

imgtcleaningLow <- function(rawDataSet, name, allDatasets, files, cell_id=1, filter_id=c(1,2,3,4,5,6,7,8,9,10), filter_out_char1=" P", filter_out_char2="[*]|X|#|[.]", filter_in_char="productive", filterStart="^*",filterEnd="*$", identityLow=95, identityHigh=100, VGene="", JGene="", DGene="", lengthLow=7, lengthHigh=15,  aminoacid="CASSPPDTGELFF", seq1=1,seq2=2,Tcell) {
  a="Filter ids "
  for (i in 1:length(filter_id)){
    a=paste0(a,",",filter_id[i])
  }
  nr=0
  for (i in names(rawDataSet)){
    nr=nrow(rawDataSet[[i]])+nr
  }
  #logfile
  cat(paste0("imgtcleaning","\t"), file=logFile, append=TRUE)
  cat(paste0(a,"\t"), file=logFile, append=TRUE)
  cat(paste0(nr,"\t"), file=logFile, append=TRUE)
  cat(paste0(ncol(rawDataSet[[names(rawDataSet)[1]]]),"\t"), file=logFile, append=TRUE)
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  
  cleaning_criteria=c("Functional V-Gene", "CDR3 with no Special Characters","Productive Sequence", "Productive Sequences")
  
  #worng_columns_id=c(1,2)
  
  #return(paste0("filterStart= ",filterStart,"filterEnd= ",filterEnd,"VGene=",VGene,"lala"))
  
  #filter_column contains the columns that are used for each one of the 9 filters with ids=1:9
  filter_column=c(used_columns[["Summary"]][3],used_columns[["Summary"]][18],used_columns[["Summary"]][2],used_columns[["Summary"]][18],used_columns[["Summary"]][4],used_columns[["Summary"]][3], used_columns[["Summary"]][8], used_columns[["Summary"]][11], used_columns[["Summary"]][15], used_columns[["Summary"]][18],"Summary.V.REGION.identity....with.ins.del.events.")
  filterOut<-list()
  filterIn<-list()
  workflow<- matrix(0,length(filter_id),3)
  
  # Log start time and memory currently used
  start.time <- Sys.time() 
  #mem_used()
  
  #usedDatasetId=match(name,allDatasets)
  # Combine raw name
  for (i in 1:length(name)){
    if (i==1){
      allData <- rawDataSet[[name[i]]];
    }else{
      allData <- rbind(allData, rawDataSet[[name[i]]]);
    }
  }
  
  test_column=c(filter_column[1], filter_column[2], filter_column[5], used_columns[["Nt.sequences"]][1] )
  
  used_column=c("dataName")
  for (i in 1:length(filter_id)){
    used_column=c(used_column,filter_column[filter_id[i]])
  }
  
  #used_column=c(used_column,test_column)
  #return(names(allData))
  
  #Drop all the columns that will not be used
  #unusedColumns <- allData[,-match(used_column,names(allData))]
  #allData <- allData %>% select(used_column)
  
  # IMPORTANT: Datasets should start with T
  # dataSetIDs <- as.list(levels(unique(allData$dataName))) 
  
  allDataInitial<-allData
  workflow_datasets<-list()
  
  a=matrix(0,length(filter_id),3)
  for (j in 1:length(name)){
    workflow_datasets[[name[j]]]=a
  }
  
  #Take only the first gene (V, J D) (Separated with "or")
  a=which(str_detect(allData[[filter_column[1]]]," or|,"))
  if (length(a)>0){
    a2=strsplit(allData[[filter_column[1]]][a]," or|,") 
    allData[[filter_column[1]]][a]=as.character(ldply(a2,function(s){t(data.frame(unlist(s)))})[,1])
  }
  
  a=which(str_detect(allData[[filter_column[7]]]," or|,"))
  if (length(a)>0){
    a2=strsplit(allData[[filter_column[7]]][a]," or|,") 
    allData[[filter_column[7]]][a]=as.character(ldply(a2,function(s){t(data.frame(unlist(s)))})[,1])
  }
  
  if (!all(is.na(allData[[filter_column[8]]]))){
    a=which(str_detect(allData[[filter_column[8]]]," or|,"))
    if (length(a)>0){
      a2=strsplit(allData[[filter_column[8]]][a]," or|,") 
      allData[[filter_column[8]]][a]=as.character(ldply(a2,function(s){t(data.frame(unlist(s)))})[,1])
    }
  }
  
  #Remove (see comment) from AA.JUNCTION
  a=which(str_detect(allData[[filter_column[2]]]," [(]see"))
  if (length(a)>0){
    a2=strsplit(allData[[filter_column[2]]][a]," [(]see") 
    allData[[filter_column[2]]][a]=as.character(ldply(a2,function(s){t(data.frame(unlist(s)))})[,1])
  }
  
  # Apply the requested filters
  
  if (any(filter_id==1)){
    # unique(datapoint$1_Summary.txt.V.GENE.and.allele)  -> Filter out " P "  e.g. Homsap TRBV21-1*01 P (see comment)"
    i=which(filter_id==1)
    filterOut[[1]] <- allDataInitial %>% filter(str_detect(allDataInitial[[filter_column[1]]], filter_out_char1))
    if (nrow(filterOut[[1]])>0) filterOut[[1]]=cbind(filterOut[[1]],FilterId=cleaning_criteria[1])
    workflow[i,3]=nrow(allDataInitial)-nrow(filterOut[[filter_id[i]]])
    
    allData <- allData %>% filter(!str_detect(allData[[filter_column[1]]], filter_out_char1))
    
    workflow[i,1]=filter_id[i]
    workflow[i,2]=nrow(filterOut[[filter_id[i]]])
    
    for (j in 1:length(name)){
      if (nrow(filterOut[[1]])>0) filterOut_datasets<-filterOut[[1]] %>% filter(filterOut[[1]]$dataName==name[j])
      else filterOut_datasets<-filterOut[[1]]
      if (i==1) prev=nrow(rawDataSet[[name[j]]])
      else prev=(workflow_datasets[[name[j]]][i-1,3])
      workflow_datasets[[name[j]]][i,3]=prev-nrow(filterOut_datasets) 
      workflow_datasets[[name[j]]][i,2]=nrow(filterOut_datasets)
      workflow_datasets[[name[j]]][i,1]=filter_id[i]
    }
    
  }
  
  if (any(filter_id==2)){
    i=which(filter_id==2)
    filterOut[[2]] <- allDataInitial %>% filter(str_detect(allDataInitial[[filter_column[2]]], filter_out_char2))
    if (nrow(filterOut[[2]])>0) filterOut[[2]]=cbind(filterOut[[2]],FilterId=cleaning_criteria[2])
    workflow[i,3]=nrow(allDataInitial)-nrow(filterOut[[filter_id[i]]])
    allData <- allData %>% filter(!str_detect(allData[[filter_column[2]]], filter_out_char2))
    
    workflow[i,1]=filter_id[i]
    workflow[i,2]=nrow(filterOut[[filter_id[i]]])
    
    for (j in 1:length(name)){
      if (nrow(filterOut[[2]])>0) filterOut_datasets<-filterOut[[2]] %>% filter(filterOut[[2]]$dataName==name[j])
      else filterOut_datasets<-filterOut[[2]]
      if (i==1) prev=nrow(rawDataSet[[name[j]]])
      else prev=workflow_datasets[[name[j]]][i-1,3]
      workflow_datasets[[name[j]]][i,3]=prev-nrow(filterOut_datasets) 
      workflow_datasets[[name[j]]][i,2]=nrow(filterOut_datasets)
      workflow_datasets[[name[j]]][i,1]=filter_id[i]
    }
  }
  
  if (any(filter_id==3)){
    # unique(datapoint$1_Summary.txt.Functionality)  -> Filter in "productive" and "productive (see comment)"
    i=which(filter_id==3)
    
    if (Tcell){
      filterOut[[3]] <- allData %>% filter(!str_detect(allData[[filter_column[3]]], "^productive$"))
      allData <- allData %>% filter(str_detect(allData[[filter_column[3]]], "^productive$"))
    }else{
      filterOut[[3]] <- allData %>% filter(!str_detect(allData[[filter_column[3]]], "^productive"))
      
      allData <- allData %>% filter(str_detect(allData[[filter_column[3]]], "^productive"))
      
      ins_del<-which(!is.na(allData[[filter_column[11]]]))
      
      if (length(ins_del)>0){
        #check if V-REGION deletions	or V-REGION insertions contain (cause frameshift)
        delete = allData[ins_del,] %>% filter(str_detect(allData[[used_columns[["Summary"]][21]]][ins_del],"[(]cause frameshift[)]"))
        delete2 = allData[ins_del,] %>% filter(str_detect(allData[[used_columns[["Summary"]][22]]][ins_del],"[(]cause frameshift[)]"))
        delete_all=unique(rbind(delete,delete2))
        
        filterOut[[3]]<-unique(rbind(filterOut[[3]],delete_all))
        allData<-allData %>% filter(!(allData[[used_columns[["Summary"]][1]]] %in%  filterOut[[3]][[used_columns[["Summary"]][1]]]))
      }
    }
    
    if (nrow(filterOut[[3]])>0) filterOut[[3]]=cbind(filterOut[[3]],FilterId=cleaning_criteria[3])
    
    workflow[i,3]=nrow(allDataInitial)-nrow(filterOut[[filter_id[i]]])
    workflow[i,1]=filter_id[i]
    workflow[i,2]=nrow(filterOut[[filter_id[i]]])
    
    for (j in 1:length(name)){
      if (nrow(filterOut[[3]])>0) filterOut_datasets<-filterOut[[3]] %>% filter(filterOut[[3]]$dataName==name[j])
      else filterOut_datasets<-filterOut[[3]]
      if (i==1) prev=nrow(rawDataSet[[name[j]]])
      else prev=(workflow_datasets[[name[j]]][i-1,3])
      workflow_datasets[[name[j]]][i,3]=(prev)-nrow(filterOut_datasets) 
      workflow_datasets[[name[j]]][i,2]=nrow(filterOut_datasets)
      workflow_datasets[[name[j]]][i,1]=filter_id[i]
    }
  }
  
  if (any(filter_id==4)){
    # datapoint$1_Summary.txt.AA.JUNCTION  -> Filter in "Start with C and end with F"
    i=which(filter_id==4)
    if (filterStart=="" && filterEnd==""){
      filterOut[[4]] <- allDataInitial %>% filter(!(str_detect(allDataInitial[[filter_column[4]]], filterStart)))
      #if (nrow(filterOut[[4]])>0) filterOut[[4]]=cbind(filterOut[[4]],FilterId=cleaning_criteria[4])
      workflow[i,3]=nrow(allDataInitial)-nrow(filterOut[[filter_id[i]]])
    }else if (filterEnd==""){
      filterOut[[4]] <- allDataInitial %>% filter(!(str_detect(allDataInitial[[filter_column[4]]], filterStart)))
      workflow[i,3]=nrow(allDataInitial)-nrow(filterOut[[filter_id[i]]])
      allData <- allData %>% filter((str_detect(allData[[filter_column[4]]], filterStart)))
    }else if (filterStart==""){
      filterOut[[4]] <- allDataInitial %>% filter(!str_detect(allDataInitial[[filter_column[4]]], filterEnd))
      workflow[i,3]=nrow(allDataInitial)-nrow(filterOut[[filter_id[i]]])
      allData <- allData %>% filter(str_detect(allData[[filter_column[4]]], filterEnd))
    }else{
      filterOut[[4]] <- allDataInitial %>% filter(!(str_detect(allDataInitial[[filter_column[4]]], filterStart)))
      filterOut[[4]] <- rbind(filterOut[[4]],(allDataInitial %>% filter(!(str_detect(allDataInitial[[filter_column[4]]], filterEnd)))))
      workflow[i,3]=nrow(allDataInitial)-nrow(filterOut[[filter_id[i]]])
      allData <- allData %>% filter((str_detect(allData[[filter_column[4]]], filterStart)))
      allData <- allData %>% filter(str_detect(allData[[filter_column[4]]], filterEnd))
    }
    if (nrow(filterOut[[4]])>0) filterOut[[4]]=cbind(filterOut[[4]],FilterId=cleaning_criteria[4])
    
    
    workflow[i,1]=filter_id[i]
    workflow[i,2]=nrow(filterOut[[filter_id[i]]])
    
    for (j in 1:length(name)){
      if (nrow(filterOut[[4]])>0) filterOut_datasets<-filterOut[[4]] %>% filter(filterOut[[4]]$dataName==name[j])
      else filterOut_datasets<-filterOut[[4]]
      if (i==1) prev=nrow(rawDataSet[[name[j]]])
      else prev=workflow_datasets[[name[j]]][i-1,3]
      workflow_datasets[[name[j]]][i,3]=nrow(allData %>% filter(allData$dataName==name[j]))
      workflow_datasets[[name[j]]][i,2]=prev-nrow(allData %>% filter(allData$dataName==name[j]))
      workflow_datasets[[name[j]]][i,1]=filter_id[i]
    }
  }
 
  filterOutSum<-c()
  if (length(filter_id)>0){
    for (i in 1:length(filter_id)){
      if (i==1)
        filterOutSum<-filterOut[[filter_id[1]]]
      else
        filterOutSum<-rbind(filterOutSum,filterOut[[filter_id[i]]])
    }

    #Handle conflictions
    if (nrow(filterOutSum)>0){
      filterOutSum<-aggregate(filterOutSum[,c(1,3:ncol(filterOutSum))], list(filterOutSum[,2]), function(x) paste0(unique(x)))
      colnames(filterOutSum)[1]=used_columns[["Summary"]][1]
      filterOutSum=filterOutSum[,c(2,1,3:ncol(filterOutSum))]
    }
    
  }
  
  a<-name[1]
  if (length(name)>1)
    for (i in 2:length(name)){a<-paste0(a,", ",name[i])}
  
  b<-filter_id[1]
  if (length(filter_id)>1)
    for (i in 2:length(filter_id)){b<-paste0(b,", ",filter_id[i])}
  
  #Separate allData to different tables
  initial_datasets<-list()
  for (i in 1:length(name)){
    initial_datasets[[name[i]]]<- allDataInitial %>% filter(allDataInitial$dataName==name[i])
  }
  
  cleaned_datasets<-list()
  if (length(allData)>0){
    for (i in 1:length(name)){
      cleaned_datasets[[name[i]]]<- allData %>% filter(allData$dataName==name[i])
    }
  }
  
  cleaned_out_datasets<-list()
  if (length(filterOutSum)>0){
    for (i in 1:length(name)){
      cleaned_out_datasets[[name[i]]]<- filterOutSum %>% filter(filterOutSum$dataName==name[i])
    }
  }
  
  confirm<-paste0("Datasets cleaned: ",a,". Cleaning Filters applied: ",b)
  
  # log time end and memory used 
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  cat(pryr::mem_used(), file=logFile, append=TRUE, sep = "\n")
  
  result=list("message"="","dim"=dim(allData), "workflow"=workflow, "workflow_datasets"=workflow_datasets, "allDataInitial"=allDataInitial, "allData"=allData, "filterOutSum"=filterOutSum, "initial_datasets"=initial_datasets, "cleaned_datasets"=cleaned_datasets, "cleaned_out_datasets"=cleaned_out_datasets, "confirm"=confirm)
  return(result)
  
}


######################################################################################################################################

imgtfilterLow <- function(rawDataSet,name, allData, cell_id=1, filter_id=c(1,2,3,4,5,6,7,8,9,10), filter_out_char1=" P", filter_out_char2="[:punct:]|X", filter_in_char="productive", filterStart="^*",filterEnd="*$", identityLow=95, identityHigh=100, VGene="", JGene="", DGene="", lengthLow=7, lengthHigh=15,  aminoacid="CASSPPDTGELFF", seq1=1,seq2=2) {
  #logfile
  a="Filter ids "
  for (i in 1:length(filter_id)){
    a=paste0(a,",",filter_id[i])
  }
  cat(paste0("imgtfilter","\t"), file=logFile, append=TRUE)
  cat(paste0(a,"\t"), file=logFile, append=TRUE)
  cat(paste0(nrow(allData),"\t"), file=logFile, append=TRUE)
  cat(paste0(ncol(rawDataSet[[names(rawDataSet)[1]]]),"\t"), file=logFile, append=TRUE)
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  
  cleaning_criteria=c("Functional V-Gene", "CDR3 with no Special Characters","Productive Sequence", "Productive Sequences")
  filtering_criteria=c("V-REGION identity %","Specific V Gene","Specific J Gene", "Specific D Gene","CDR3 length range","CDR3 length range")
  criteria=c(cleaning_criteria,filtering_criteria)
  
  #filter_column contains the columns that are used for each one of the 9 filters with ids=1:9
  filter_column=c(used_columns[["Summary"]][3],used_columns[["Summary"]][18],used_columns[["Summary"]][2],used_columns[["Summary"]][18],used_columns[["Summary"]][4],used_columns[["Summary"]][3], used_columns[["Summary"]][8], used_columns[["Summary"]][11], used_columns[["Summary"]][15], used_columns[["Summary"]][18], "Summary.V.REGION.identity....with.ins.del.events.")
  filterOut<-list()
  workflow<- matrix(0,length(filter_id),3)
  
  # Log start time and memory currently used
  start.time <- Sys.time()
  #mem_used()
  
  test_column=c(filter_column[1], filter_column[2], filter_column[5], used_columns[["Nt.sequences"]][1] )
  
  used_column=c("dataName")
  for (i in 1:length(filter_id)){
    used_column=c(used_column,filter_column[filter_id[i]])
  }
  
  #used_column=c(used_column,test_column)
  #return(names(allData))
  
  #Drop all the columns that will not be used
  #unusedColumns <- allData[,-match(used_column,names(allData))]
  #allData <- allData %>% select(used_column)
  
  # IMPORTANT: Datasets should start with T
  # dataSetIDs <- as.list(levels(unique(allData$dataName))) 
  
  allDataInitial<-allData
  
  workflow_datasets<-list()
  
  a=matrix(0,length(filter_id),3)
  for (j in 1:length(name)){
    workflow_datasets[[name[j]]]=a
  }
  
  # Apply the requested filters
  
  if (any(filter_id==5)){
    # datapoint$1_Summary.txt.V.REGION.identity..  -> Filter in value between 95 and 100
    i=which(filter_id==5)
    ins_del<-which(!is.na(allData[[filter_column[11]]]))
    
    if (length(ins_del)>0){
      allData[[filter_column[5]]][ins_del]<-allData[[filter_column[11]]][ins_del]
    }
    
    filterOut[[5]] <- allDataInitial[ which(allDataInitial[[filter_column[5]]] > identityHigh | allDataInitial[[filter_column[5]]] < identityLow),]
    allData <-  allData[ which(allData[[filter_column[5]]] <= identityHigh & allData[[filter_column[5]]] >= identityLow),]
    
    if (nrow(filterOut[[5]])>0) filterOut[[5]]=cbind(filterOut[[5]],FilterId=criteria[5])
    workflow[i,3]=nrow(allDataInitial)-nrow(filterOut[[filter_id[i]]])
    
    
    #write.table(filterOut5, file = "filterOut5.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
    
    workflow[i,1]=filter_id[i]-4
    workflow[i,2]=nrow(filterOut[[filter_id[i]]])
    
    for (j in 1:length(name)){
      if (nrow(filterOut[[5]])>0) filterOut_datasets<-filterOut[[5]] %>% filter(filterOut[[5]]$dataName==name[j])
      else filterOut_datasets<-filterOut[[5]]
      if (i==1){ prev=nrow(rawDataSet[[name[j]]])
      }
      else prev=workflow_datasets[[name[j]]][i-1,3]
      workflow_datasets[[name[j]]][i,3]=(prev)-nrow(filterOut_datasets) 
      workflow_datasets[[name[j]]][i,2]=nrow(filterOut_datasets)
      workflow_datasets[[name[j]]][i,1]=filter_id[i]-4
    }
  }
  
  if (any(filter_id==6)){
    # datapoint$1_Summary.txt.V-GENE.and.allele -> Filter in specific V Genes
    i=which(filter_id==6)
    filterOut[[6]] <- allDataInitial %>% filter(!str_detect(allDataInitial[[filter_column[6]]], gsub("[(]","[(]",gsub("[)]","[)]",gsub("[*]","[*]", VGene)))) )
    if (nrow(filterOut[[6]])>0) filterOut[[6]]=cbind(filterOut[[6]],FilterId=criteria[6])
    workflow[i,3]=nrow(allDataInitial)-nrow(filterOut[[filter_id[i]]])
    allData <- allData %>% filter(str_detect(allData[[filter_column[6]]], gsub("[(]","[(]",gsub("[)]","[)]",gsub("[*]","[*]", VGene)))) )
    
    #write.table(filterOut16, file = "filterOut6.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
    
    workflow[i,1]=filter_id[i]-4
    workflow[i,2]=nrow(filterOut[[filter_id[i]]])
    
    for (j in 1:length(name)){
      if (nrow(filterOut[[6]])>0) filterOut_datasets<-filterOut[[6]] %>% filter(filterOut[[6]]$dataName==name[j])
      else filterOut_datasets<-filterOut[[6]] 
      if (i==1) prev=nrow(rawDataSet[[name[j]]])
      else prev=workflow_datasets[[name[j]]][i-1,3]
      workflow_datasets[[name[j]]][i,3]=(prev)-nrow(filterOut_datasets) 
      workflow_datasets[[name[j]]][i,2]=nrow(filterOut_datasets)
      workflow_datasets[[name[j]]][i,1]=filter_id[i]-4
    }
  }
  
  if (any(filter_id==7)){
    # datapoint$1_Summary.txt.J-GENE.and.allele  -> Filter in specific J Genes
    i=which(filter_id==7)
    filterOut[[7]] <- allDataInitial %>% filter(!str_detect(allDataInitial[[filter_column[7]]], gsub("[(]","[(]",gsub("[)]","[)]",gsub("[*]","[*]", JGene)))) )
    if (nrow(filterOut[[7]])>0) filterOut[[7]]=cbind(filterOut[[7]],FilterId=criteria[7])
    workflow[i,3]=nrow(allDataInitial)-nrow(filterOut[[filter_id[i]]])
    allData <- allData %>% filter(str_detect(allData[[filter_column[7]]], gsub("[(]","[(]",gsub("[)]","[)]",gsub("[*]","[*]", JGene)))) )
    
    #write.table(filterOut7, file = "filterOut7.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
    
    workflow[i,1]=filter_id[i]-4
    workflow[i,2]=nrow(filterOut[[filter_id[i]]])
    
    for (j in 1:length(name)){
      if (nrow(filterOut[[7]])>0) filterOut_datasets<-filterOut[[7]] %>% filter(filterOut[[7]]$dataName==name[j])
      else filterOut_datasets<-filterOut[[7]]
      if (i==1) prev=nrow(rawDataSet[[name[j]]])
      else prev=workflow_datasets[[name[j]]][i-1,3]
      workflow_datasets[[name[j]]][i,3]=(prev)-nrow(filterOut_datasets) 
      workflow_datasets[[name[j]]][i,2]=nrow(filterOut_datasets)
      workflow_datasets[[name[j]]][i,1]=filter_id[i]-4
    }
  }
  
  if (any(filter_id==8)){
    # datapoint$1_Summary.txt.D-GENE.and.allele  -> Filter in specific D Genes
    i=which(filter_id==8)
    filterOut[[8]] <- allDataInitial %>% filter(!str_detect(allDataInitial[[filter_column[8]]], gsub("[(]","[(]",gsub("[)]","[)]",gsub("[*]","[*]", DGene)))) ) 
    if (nrow(filterOut[[8]])>0) filterOut[[8]]=cbind(filterOut[[8]],FilterId=criteria[8])
    workflow[i,3]=nrow(allDataInitial)-nrow(filterOut[[filter_id[i]]])
    allData <- allData %>% filter(str_detect(allData[[filter_column[8]]], gsub("[(]","[(]",gsub("[)]","[)]",gsub("[*]","[*]", DGene)))) )
    
    #write.table(filterOut8, file = "filterOut8.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
    
    workflow[i,1]=filter_id[i]-4
    workflow[i,2]=nrow(filterOut[[filter_id[i]]])
    
    for (j in 1:length(name)){
      if (nrow(filterOut[[8]])>0) filterOut_datasets<-filterOut[[8]] %>% filter(filterOut[[8]]$dataName==name[j])
      else  filterOut_datasets<-filterOut[[8]] 
      if (i==1) prev=nrow(rawDataSet[[name[j]]])
      else prev=workflow_datasets[[name[j]]][i-1,3]
      workflow_datasets[[name[j]]][i,3]=(prev)-nrow(filterOut_datasets) 
      workflow_datasets[[name[j]]][i,2]=nrow(filterOut_datasets)
      workflow_datasets[[name[j]]][i,1]=filter_id[i]-4
    }
  }
  
  if (any(filter_id==9)){
    # datapoint$CDR3-IMGT.length  -> Filter in value between 95 and 100
    i=which(filter_id==9)
    filterOut[[9]] <- allDataInitial[ which(as.numeric(allDataInitial[[filter_column[9]]]) > lengthHigh | as.numeric(allDataInitial[[filter_column[9]]]) < lengthLow),]
    if (nrow(filterOut[[9]])>0) filterOut[[9]]=cbind(filterOut[[9]],FilterId=criteria[9])
    allData <-  allData[ which(as.numeric(allData[[filter_column[9]]]) <= lengthHigh & as.numeric(allData[[filter_column[9]]]) >= lengthLow),]
    
    #return(lengthLow)
    #write.table(filterOut9, file = "filterOut9.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
    
    workflow[i,1]=filter_id[i]-4
    workflow[i,2]=nrow(filterOut[[filter_id[i]]])
    workflow[i,3]=nrow(allDataInitial)-nrow(filterOut[[filter_id[i]]])
    
    for (j in 1:length(name)){
      if (nrow(filterOut[[9]])>0) filterOut_datasets<-filterOut[[9]] %>% filter(filterOut[[9]]$dataName==name[j])
      else filterOut_datasets<-filterOut[[9]] 
      if (i==1) prev=nrow(rawDataSet[[name[j]]])
      else prev=workflow_datasets[[name[j]]][i-1,3]
      workflow_datasets[[name[j]]][i,3]=(prev)-nrow(filterOut_datasets) 
      workflow_datasets[[name[j]]][i,2]=nrow(filterOut_datasets)
      workflow_datasets[[name[j]]][i,1]=filter_id[i]-4
    }
  }
  
  if (any(filter_id==10)){
    # datapoint$1_Summary.txt.AA.JUNCTION  -> Filter in CDR3 containing specific amino-acid sequence
    i=which(filter_id==10)
    filterOut[[10]] <- allDataInitial %>% filter(!str_detect(allDataInitial[[filter_column[10]]], aminoacid))
    if (nrow(filterOut[[10]])>0) filterOut[[10]]=cbind(filterOut[[10]],FilterId=criteria[10])
    workflow[i,3]=nrow(allDataInitial)-nrow(filterOut[[filter_id[i]]])
    allData <- allData %>% filter(str_detect(allData[[filter_column[10]]], aminoacid))
    
    #write.table(filterOut10, file = "filterOut10.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
    
    workflow[i,1]=filter_id[i]-4
    workflow[i,2]=nrow(filterOut[[filter_id[i]]])
    
    for (j in 1:length(name)){
      if (nrow(filterOut[[10]])>0) filterOut_datasets<-filterOut[[10]] %>% filter(filterOut[[10]]$dataName==name[j])
      else  filterOut_datasets<-filterOut[[10]]
      if (i==1) prev=nrow(rawDataSet[[name[j]]])
      else prev=workflow_datasets[[name[j]]][i-1,3]
      workflow_datasets[[name[j]]][i,3]=(prev)-nrow(filterOut_datasets) 
      workflow_datasets[[name[j]]][i,2]=nrow(filterOut_datasets)
      workflow_datasets[[name[j]]][i,1]=filter_id[i]-4
    }
  }
  
  # Provide a 1_Summary.txt of the datasets
  # (allData %>% group_by(dataName) %>%  summarize(n()))[[2]][1]
  
  #write.table(allData, file = "allDataAfterFiltering.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
  
  
  #mem_used() # log memory used after filtering (no significant change should be evident)
  filterOutSum<-c()
  if (length(filter_id)>0){
    for (i in 1:length(filter_id)){
      if (i==1)
        filterOutSum<-filterOut[[filter_id[1]]]
      else
        filterOutSum<-rbind(filterOutSum,filterOut[[filter_id[i]]])
    }
    #Handle conflictions
    if (nrow(filterOutSum)>0){
      filterOutSum<-aggregate(filterOutSum[,c(1,3:ncol(filterOutSum))], list(filterOutSum[,2]), function(x) paste0(unique(x)))
      colnames(filterOutSum)[1]=used_columns[["Summary"]][1]
      filterOutSum=filterOutSum[,c(2,1,3:ncol(filterOutSum))]
    }
  }
  
  # Do the actual analysis - targeted tables
  
  a<-name[1]
  if (length(name)>1)
    for (i in 2:length(name)){a<-paste0(a,", ",name[i])}
  
  b<-filter_id[1]-4
  if (length(filter_id)>1)
    for (i in 2:length(filter_id)){b<-paste0(b,", ",(filter_id[i]-4))}
  
  #Delete (see comment) from genes
  a=which(str_detect(allData[[used_columns[["Summary"]][3]]]," or|,| [(]see"))
  if (length(a)>0){
    a2=strsplit(allData[[used_columns[["Summary"]][3]]][a]," or|,| [(]see") 
    allData[[used_columns[["Summary"]][3]]][a]=as.character(ldply(a2,function(s){t(data.frame(unlist(s)))})[,1])
  }
  
  a=which(str_detect(allData[[used_columns[["Summary"]][8]]]," or|,| [(]see"))
  if (length(a)>0){
    a2=strsplit(allData[[used_columns[["Summary"]][8]]][a]," or|,| [(]see") 
    allData[[used_columns[["Summary"]][8]]][a]=as.character(ldply(a2,function(s){t(data.frame(unlist(s)))})[,1])
  }
  
  if (!all(is.na(allData[[used_columns[["Summary"]][11]]]))){
    a=which(str_detect(allData[[used_columns[["Summary"]][11]]]," or|,| [(]see"))
    if (length(a)>0){
      a2=strsplit(allData[[used_columns[["Summary"]][11]]][a]," or|,| [(]see") 
      allData[[used_columns[["Summary"]][11]]][a]=as.character(ldply(a2,function(s){t(data.frame(unlist(s)))})[,1])
    }
  }
  
  #Separate allData to different tables
  initial_datasets<-list()
  for (i in 1:length(name)){
    initial_datasets[[name[i]]]<- allDataInitial %>% filter(allDataInitial$dataName==name[i])
  }
  
  filtered_datasets<-list()
  if (length(allData)>0){
    for (i in 1:length(name)){
      filtered_datasets[[name[i]]]<- allData %>% filter(allData$dataName==name[i])
    }
  }
  
  filtered_out_datasets<-list()
  if (length(filterOutSum)>0){
    for (i in 1:length(name)){
      filtered_out_datasets[[name[i]]]<- filterOutSum %>% filter(filterOutSum$dataName==name[i])
    }
  }
  
  confirm<-paste0("Datasets filtered: ",a,". Filters applied: ",b)
  
  # log time end and memory used 
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  cat(pryr::mem_used(), file=logFile, append=TRUE, sep = "\n")
  
  result=list("message"="","dim"=dim(allData),"workflow"=workflow,"workflow_datasets"=workflow_datasets, "allDataInitial"=allDataInitial, "allData"=allData, "filterOutSum"=filterOutSum,"initial_datasets"=initial_datasets, "filtered_datasets"=filtered_datasets, "filtered_out_datasets"=filtered_out_datasets, "confirm"=confirm)
  return(result)
  
}


######################################################################################################################################

clonotypes<- function(allData,allele,gene,junction,name){
  #logfile
  if (allele==F){
    g=str_replace(gene,".and.allele","")
  }else{
    g=gene
  }
  cat(paste0("clonotypes","\t"), file=logFile, append=TRUE)
  cat(paste0(paste(g,junction,sep = ","),"\t"), file=logFile, append=TRUE)
  cat(paste0(nrow(allData),"\t"), file=logFile, append=TRUE)
  cat(paste0(ncol(allData),"\t"), file=logFile, append=TRUE)
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  
  #clonotypes all Data
  allData_initial=allData
  view_specific_clonotype_allData<-list()
  convergent_evolution<-c()
  convergent_evolution_list_allData<-list()
  cluster_id<-c()
  freq_cluster_id<-c()
  
  if (length(gene)>0){
    if (allele==F){
      a2=strsplit(allData[[gene]],"[*]")  
      allData[[gene]]=as.character(ldply(a2,function(s){t(data.frame(unlist(s)))})[,1])
    }
    distinctVGenes_CDR3=allData %>% group_by(Genes=allData[[gene]], CDR3=allData[[junction]]) %>% summarise(n=n())
    distinctVGenes_CDR3=distinctVGenes_CDR3[order(-distinctVGenes_CDR3$n),]
    distinctVGenes_CDR3$clonotype <- do.call(paste, c(distinctVGenes_CDR3[c("Genes", "CDR3")], sep = " - ")) 
    for (i in 1:nrow(distinctVGenes_CDR3)){
      inputVGenes_CDR3=which((allData[[gene]]==distinctVGenes_CDR3$Genes[i]) & (allData[[junction]]==distinctVGenes_CDR3$CDR3[i]))  
      view_specific_clonotype_allData[[distinctVGenes_CDR3$clonotype[i]]]=allData_initial[inputVGenes_CDR3,]
      ce=view_specific_clonotype_allData[[distinctVGenes_CDR3$clonotype[i]]] %>% 
        group_by(view_specific_clonotype_allData[[distinctVGenes_CDR3$clonotype[i]]][[used_columns[["IMGT.gapped.nt.sequences"]][9]]]) %>% 
        summarise(convergent_evolution=n())
      convergent_evolution_list_allData[[as.character(i)]]=ce
      convergent_evolution=c(convergent_evolution,paste0("cluster ",i," : ",nrow(ce)))
      
      freq_cluster_id[inputVGenes_CDR3]=100*length(inputVGenes_CDR3)/nrow(allData_initial)
      cluster_id[inputVGenes_CDR3]=i
    }
  }else{
    distinctVGenes_CDR3=allData %>% group_by(clonotype=allData[[junction]]) %>% summarise(n=n())
    distinctVGenes_CDR3=distinctVGenes_CDR3[order(-distinctVGenes_CDR3$n),]
    for (i in 1:nrow(distinctVGenes_CDR3)){
      inputVGenes_CDR3=which(allData[[junction]]==distinctVGenes_CDR3$clonotype[i])
      view_specific_clonotype_allData[[distinctVGenes_CDR3$clonotype[i]]]=allData[inputVGenes_CDR3,]
      ce=view_specific_clonotype_allData[[distinctVGenes_CDR3$clonotype[i]]] %>% group_by(view_specific_clonotype_allData[[distinctVGenes_CDR3$clonotype[i]]][[used_columns[["IMGT.gapped.nt.sequences"]][9]]]) %>% summarise(convergent_evolution=n())
      convergent_evolution_list_allData[[as.character(i)]]=ce
      convergent_evolution=c(convergent_evolution,paste0("cluster ",i," : ",nrow(ce)))
      
      freq_cluster_id[inputVGenes_CDR3]=100*length(inputVGenes_CDR3)/nrow(allData_initial)
      cluster_id[inputVGenes_CDR3]=i
    }
  }
  
  clono_allData=distinctVGenes_CDR3[,c("clonotype","n")]
  clono_allData <- cbind(clono_allData,Freq=100*clono_allData$n/nrow(allData),convergent_evolution)
  colnames(clono_allData) <- c('clonotype','N','Freq','Convergent Evolution')
  
  if (save_tables_individually){
    clono_write=as.data.frame(distinctVGenes_CDR3[,c("Genes","CDR3")])
    clono_write=cbind(clono_write,clono_allData[,c('N','Freq','Convergent Evolution')])
    colnames(clono_write) <- c('Genes','CDR3','N','Freq','Convergent Evolution')
    write.table((clono_write),paste0(output_folder,"/","Clonotypes_All Data",".txt"),sep = "\t", row.names = FALSE, col.names = TRUE)
    
    
    write.table((cbind(allData_initial, "cluster_id"=cluster_id, "freq_cluster_id"=freq_cluster_id)),
                paste0(output_folder,"/","filterin_clono_All_Data",".txt"),sep = "\t", row.names = FALSE, col.names = TRUE)
  }
  
  ############################################# clonotypes datasets 
  clono_datasets<-list()
  view_specific_clonotype_datasets<-list()
  convergent_evolution_list_datasets<-list()
  convergent_evolution_list_datasets_only_num<-list()
  
  one_run<-function(j){
    data=allData %>% filter(allData$dataName==name[j])
    view_specific_clonotype_datasets[[name[j]]]<-list()
    convergent_evolution<-c()
    convergent_evolution_list_datasets[[name[j]]]<-list()
    convergent_evolution_list_datasets_only_num[[name[j]]]<-c()
    data_initial=allData_initial %>% filter(allData$dataName==name[j])
    cluster_id<-c()
    freq_cluster_id<-c()
    
    if (length(gene)>0){
      distinctVGenes_CDR3=data %>% group_by(Genes=data[[gene]], CDR3=data[[junction]]) %>% summarise(n=n())
      distinctVGenes_CDR3=distinctVGenes_CDR3[order(-distinctVGenes_CDR3$n),]
      distinctVGenes_CDR3$clonotype <- do.call(paste, c(distinctVGenes_CDR3[c("Genes", "CDR3")], sep = " - ")) 
      for (i in 1:nrow(distinctVGenes_CDR3)){
        inputVGenes_CDR3=which((data[[gene]]==distinctVGenes_CDR3$Genes[i]) & (data[[junction]]==distinctVGenes_CDR3$CDR3[i]))  
        view_specific_clonotype_datasets[[name[j]]][[distinctVGenes_CDR3$clonotype[i]]]=data_initial[inputVGenes_CDR3,]
        ce=view_specific_clonotype_datasets[[name[j]]][[distinctVGenes_CDR3$clonotype[i]]] %>% group_by(view_specific_clonotype_datasets[[name[j]]][[distinctVGenes_CDR3$clonotype[i]]][[used_columns[["IMGT.gapped.nt.sequences"]][9]]]) %>% summarise(convergent_evolution=n())
        convergent_evolution_list_datasets[[name[j]]][[as.character(i)]]=ce
        convergent_evolution_list_datasets_only_num[[name[j]]]=c( convergent_evolution_list_datasets_only_num[[name[j]]],nrow(ce))
        convergent_evolution=c(convergent_evolution,paste0("cluster ",i," : ",nrow(ce)))
        
        freq_cluster_id[inputVGenes_CDR3]=100*length(inputVGenes_CDR3)/nrow(data_initial)
        cluster_id[inputVGenes_CDR3]=i
      }
    }else{
      distinctVGenes_CDR3=data %>% group_by(clonotype=data[[junction]]) %>% summarise(n=n())
      distinctVGenes_CDR3=distinctVGenes_CDR3[order(-distinctVGenes_CDR3$n),]
      for (i in 1:nrow(distinctVGenes_CDR3)){
        inputVGenes_CDR3=which(data[[junction]]==distinctVGenes_CDR3$clonotype[i])
        view_specific_clonotype_datasets[[name[j]]][[distinctVGenes_CDR3$clonotype[i]]]=data_initial[inputVGenes_CDR3,]
        ce=view_specific_clonotype_datasets[[name[j]]][[distinctVGenes_CDR3$clonotype[i]]] %>% group_by(view_specific_clonotype_datasets[[name[j]]][[distinctVGenes_CDR3$clonotype[i]]][[used_columns[["IMGT.gapped.nt.sequences"]][9]]]) %>% summarise(convergent_evolution=n())
        convergent_evolution_list_datasets[[name[j]]][[as.character(i)]]=ce
        convergent_evolution_list_datasets_only_num[[name[j]]]=c( convergent_evolution_list_datasets_only_num[[name[j]]],nrow(ce))
        convergent_evolution=c(convergent_evolution,paste0("cluster ",i," : ",nrow(ce)))
        
        freq_cluster_id[inputVGenes_CDR3]=100*length(inputVGenes_CDR3)/nrow(data_initial)
        cluster_id[inputVGenes_CDR3]=i
      }
      
    }
    clono_datasets[[name[j]]]=distinctVGenes_CDR3[,c("clonotype","n")]
    clono_datasets[[name[j]]] <- clono_datasets[[name[j]]][order(-clono_datasets[[name[j]]]$n),]
    clono_datasets[[name[j]]] <- cbind(clono_datasets[[name[j]]],Freq=100*clono_datasets[[name[j]]]$n/nrow(data))
    clono_datasets[[name[j]]]=cbind(clono_datasets[[name[j]]],convergent_evolution)
    colnames(clono_datasets[[name[j]]]) <- c('clonotype','N','Freq','Convergent Evolution')
    
    if (save_tables_individually){
      clono_write= as.data.frame(distinctVGenes_CDR3[,c("Genes","CDR3")])
      clono_write= cbind(clono_write,clono_datasets[[name[j]]][,c('N','Freq','Convergent Evolution')])
      colnames(clono_write) <- c('Genes','CDR3','N','Freq','Convergent Evolution')
      write.table((clono_write),paste0(output_folder,"/","Clonotypes_",name[j],".txt"),sep = "\t", row.names = FALSE, col.names = TRUE)
    
      write.table((cbind(data_initial, "cluster_id"=cluster_id, "freq_cluster_id"=freq_cluster_id)),
                  paste0(output_folder,"/","filterin_clono_",name[j],".txt"),sep = "\t", row.names = FALSE, col.names = TRUE)
    }
    result=list()
    result[["clono_datasets"]]=clono_datasets[[name[j]]]
    result[["view_specific_clonotype_datasets"]]=view_specific_clonotype_datasets[[name[j]]]
    result[["convergent_evolution_list_datasets"]]=convergent_evolution_list_datasets[[name[j]]]
    result[["convergent_evolution_list_datasets_only_num"]]=convergent_evolution_list_datasets_only_num[[name[j]]]
    
    return(result)  
  } 
  
  if (Sys.info()[1] == "Windows"){
    #cl <- makeCluster(num_of_cores)
    #clono_datasets=clusterApply(cl=cl,1:length(name),one_run)
    a=lapply(1:length(name),one_run)
    for (i in 1:length(name)){
      view_specific_clonotype_datasets[[name[i]]]=a[[i]]$view_specific_clonotype_datasets
      clono_datasets[[name[i]]]=a[[i]]$clono_datasets
      convergent_evolution_list_datasets[[name[i]]]=a[[i]]$convergent_evolution_list_datasets
      convergent_evolution_list_datasets_only_num[[name[i]]]=a[[i]]$convergent_evolution_list_datasets_only_num
    }
  }else{
    a=mclapply(1:length(name),one_run,mc.cores = num_of_cores, mc.preschedule = TRUE)
    for (i in 1:length(name)){
      view_specific_clonotype_datasets[[name[i]]]=a[[i]]$view_specific_clonotype_datasets
      clono_datasets[[name[i]]]=a[[i]]$clono_datasets
      convergent_evolution_list_datasets[[name[i]]]=a[[i]]$convergent_evolution_list_datasets
      convergent_evolution_list_datasets_only_num[[name[i]]]=a[[i]]$convergent_evolution_list_datasets_only_num
    }
  }
  
  confirm<-paste0("Clonotypes run!")
  
  result=list("clono_allData"=clono_allData,"clono_datasets"=clono_datasets,
              "view_specific_clonotype_allData"=view_specific_clonotype_allData,
              "convergent_evolution_list_allData"=convergent_evolution_list_allData,
              "view_specific_clonotype_datasets"=view_specific_clonotype_datasets,
              "convergent_evolution_list_datasets"=convergent_evolution_list_datasets, "convergent_evolution_list_datasets_only_num"=convergent_evolution_list_datasets_only_num,
              "confirm"=confirm)
  
  # log time end and memory used 
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  cat(pryr::mem_used(), file=logFile, append=TRUE, sep = "\n")
  
  return(result)
}


######################################################################################################################################

highly_similar_clonotypes<- function(clono_allData,clono_datasets,num_of_mismatches,take_gene,cdr3_lengths,gene_clonotypes,clonotype_freq_thr_for_highly_sim,name){
  #logfile
  cat(paste0("highly_similar_clonotypes","\t"), file=logFile, append=TRUE)
  cat(paste0(paste("take_gene ",take_gene,"threshold",clonotype_freq_thr_for_highly_sim,sep = ","),"\t"), file=logFile, append=TRUE)
  cat(paste0(nrow(clono_allData),"\t"), file=logFile, append=TRUE)
  cat(paste0(ncol(clono_allData),"\t"), file=logFile, append=TRUE)
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)

  ####################################### All Data   ####################################### 
  clono_allData_only_cdr3=clono_allData
  if (str_detect(clono_allData$clonotype[1]," - ") && take_gene=="No"){
    a2=strsplit(clono_allData$clonotype," - ") 
    clono_allData_only_cdr3$clonotype=as.character(ldply(a2,function(s){t(data.frame(unlist(s)))})[,2])
    clono_allData_only_cdr3=as.data.table(clono_allData_only_cdr3[,1:3])[, lapply(.SD, sum), by = .(clonotype=clonotype)]
    clono_allData_only_cdr3=clono_allData_only_cdr3[order(-clono_allData_only_cdr3$N),]
  }
  if (str_detect(clono_allData$clonotype[1]," - ") && take_gene=="Yes"){
    a2=strsplit(clono_allData$clonotype," - ") 
    clono_allData_only_cdr3$clonotype=as.character(ldply(a2,function(s){t(data.frame(unlist(s)))})[,2])
    clono_allData_only_cdr3$gene=as.character(ldply(a2,function(s){t(data.frame(unlist(s)))})[,1])
  }
  #if the gene does matter than I do not have to exclude it from the clono_allData_only_cdr3 table
  #group by cdr3
  
  clono_allData_only_cdr3$cluster_id=as.numeric(row.names(clono_allData_only_cdr3))
  
  #for each cdr3 length
  highly_sim_view_specific_clonotypes<-list()
  for (i in 1:length(cdr3_lengths)){
    highly_sim_view_specific_clonotypes[[paste0("length ",cdr3_lengths[i])]]=list()
    
    clonotypes_of_this_length=clono_allData_only_cdr3 %>% filter(str_length(clono_allData_only_cdr3$clonotype)==(cdr3_lengths[i]+2))
    
    end_process_for_this_length=F
    
    while(end_process_for_this_length==F){
      major_clonotype=clonotypes_of_this_length$clonotype[1]
      
      if (nrow(clonotypes_of_this_length)>0){
        if (clonotypes_of_this_length$Freq[1]>clonotype_freq_thr_for_highly_sim){
          if (take_gene=="Yes"){
            clonotypes_of_this_length_gene=clonotypes_of_this_length %>% filter(clonotypes_of_this_length$gene==clonotypes_of_this_length$gene[1])
            dist_from_major=stringdist(clonotypes_of_this_length_gene$clonotype,major_clonotype)
            matched_clonotypes=clonotypes_of_this_length_gene %>% filter(dist_from_major<=num_of_mismatches[i])
            not_matched_clonotypes=clonotypes_of_this_length %>% filter(!(clonotypes_of_this_length$clonotype %in% matched_clonotypes$clonotype))
          }else{
            dist_from_major=stringdist(clonotypes_of_this_length$clonotype,major_clonotype)
            matched_clonotypes=clonotypes_of_this_length %>% filter(dist_from_major<=num_of_mismatches[i])
            not_matched_clonotypes=clonotypes_of_this_length %>% filter(dist_from_major>num_of_mismatches[i])
          }
          
          if (nrow(matched_clonotypes)>0){
            #save it
            if (take_gene=="No"){
              highly_sim_view_specific_clonotypes[[paste0("length ",cdr3_lengths[i])]][[clonotypes_of_this_length$clonotype[1]]]=matched_clonotypes #save the corresponding clonotype from clono_allData table not just the cdr3
              highly_sim_view_specific_clonotypes[[paste0("length ",cdr3_lengths[i])]][[clonotypes_of_this_length$clonotype[1]]]$prev_cluster=as.numeric(row.names(matched_clonotypes))
            }else{
              highly_sim_view_specific_clonotypes[[paste0("length ",cdr3_lengths[i])]][[clono_allData$clonotype[clonotypes_of_this_length$cluster_id[1]]]]=clono_allData[matched_clonotypes$cluster_id,] #save the corresponding clonotype from clono_allData table not just the cdr3
              highly_sim_view_specific_clonotypes[[paste0("length ",cdr3_lengths[i])]][[clono_allData$clonotype[clonotypes_of_this_length$cluster_id[1]]]]$prev_cluster=as.numeric(row.names(clono_allData[matched_clonotypes$cluster_id,]))
            }
            
          }
          if (nrow(not_matched_clonotypes)==0){
            end_process_for_this_length=T
          }else{
            clonotypes_of_this_length=not_matched_clonotypes
          }
        }else{
          #terminate the process for this length
          end_process_for_this_length=T
        }
        
      }else{
        #terminate the process for this length
        end_process_for_this_length=T
      }
    }
    
  }
  
  ##### Results to tables
  highly_sim_clonotypes<-list()
  highly_sim_clonotypes_allGroups<-list()
  for (i in 1:length(cdr3_lengths)){
    clonotype=names(highly_sim_view_specific_clonotypes[[paste0("length ",cdr3_lengths[i])]])
    if (!(is.null(clonotype))){
      N=c()
      Freq=c()
      prev_cluster=c()
      for (j in 1:length(highly_sim_view_specific_clonotypes[[paste0("length ",cdr3_lengths[i])]])){
        highly_sim_view_specific_clonotypes[[paste0("length ",cdr3_lengths[i])]][[clonotype[j]]]$HS_cluster_id=j
        N=c(N,sum(highly_sim_view_specific_clonotypes[[paste0("length ",cdr3_lengths[i])]][[clonotype[j]]]$N))
        Freq=c(Freq,sum(highly_sim_view_specific_clonotypes[[paste0("length ",cdr3_lengths[i])]][[clonotype[j]]]$Freq))
        prev_cluster_c<-""
        for (k in 1:nrow(highly_sim_view_specific_clonotypes[[paste0("length ",cdr3_lengths[i])]][[clonotype[j]]])){
          prev_cluster_c=paste(prev_cluster_c,highly_sim_view_specific_clonotypes[[paste0("length ",cdr3_lengths[i])]][[clonotype[j]]]$prev_cluster[k])
        }
        prev_cluster=c(prev_cluster,prev_cluster_c)
      }
      highly_sim_clonotypes[[paste0("length ",cdr3_lengths[i])]]=data.frame(clonotype,N,Freq,prev_cluster,stringsAsFactors = F) #I have this data frame for each length
      highly_sim_clonotypes[[paste0("length ",cdr3_lengths[i])]]$HS_cluster_id=as.numeric(row.names(highly_sim_clonotypes[[paste0("length ",cdr3_lengths[i])]]))
    }
    highly_sim_clonotypes_allGroups[[paste0("length ",cdr3_lengths[i])]]=do.call(rbind.data.frame, highly_sim_view_specific_clonotypes[[paste0("length ",cdr3_lengths[i])]])
    
    #extra clonotypes
    if (take_gene=="Yes"){
      clonotypes_of_this_length_id=which(str_length(clono_allData_only_cdr3$clonotype)==(cdr3_lengths[i]+2))
      extra_clono=clono_allData[clonotypes_of_this_length_id,] %>% filter(!(clono_allData[clonotypes_of_this_length_id,]$clonotype %in% highly_sim_clonotypes_allGroups[[paste0("length ",cdr3_lengths[i])]]$clonotype))
    }else{
      clonotypes_of_this_length=clono_allData_only_cdr3 %>% filter(str_length(clono_allData_only_cdr3$clonotype)==(cdr3_lengths[i]+2))
      extra_clono=clonotypes_of_this_length %>% filter(!(clonotypes_of_this_length$clonotype %in% highly_sim_clonotypes_allGroups[[paste0("length ",cdr3_lengths[i])]]$clonotype))
    }
    
    if (nrow(extra_clono)>0){
      extra_clono$prev_cluster=0
      for (k in 1:nrow(extra_clono)){
        temp=as.character(extra_clono[["Convergent Evolution"]][k])
        extra_clono$prev_cluster[k]=as.numeric(strsplit(strsplit(temp,"cluster ")[[1]][2]," :")[[1]][1])
      }
      #print(extra_clono)
      if (!(is.null(clonotype))){
        extra_clono$HS_cluster_id=(max(highly_sim_clonotypes[[paste0("length ",cdr3_lengths[i])]]$HS_cluster_id)+1):(max(highly_sim_clonotypes[[paste0("length ",cdr3_lengths[i])]]$HS_cluster_id)+nrow(extra_clono))
      }else{
        extra_clono$HS_cluster_id=1:nrow(extra_clono)
      }
      highly_sim_clonotypes_allGroups[[paste0("length ",cdr3_lengths[i])]]=rbind(highly_sim_clonotypes_allGroups[[paste0("length ",cdr3_lengths[i])]],extra_clono)
      highly_sim_clonotypes[[paste0("length ",cdr3_lengths[i])]]=rbind(highly_sim_clonotypes[[paste0("length ",cdr3_lengths[i])]],extra_clono[,c("clonotype","N","Freq","prev_cluster","HS_cluster_id")])
    }
    
    row.names(highly_sim_clonotypes_allGroups[[paste0("length ",cdr3_lengths[i])]])=1:nrow(highly_sim_clonotypes_allGroups[[paste0("length ",cdr3_lengths[i])]])
    
    if (save_tables_individually){
      filename=paste0(output_folder,"/","Highly sim Clonotypes_","All Data_length_",cdr3_lengths[i],".txt")
      write.table(highly_sim_clonotypes[[paste0("length ",cdr3_lengths[i])]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
      filename=paste0(output_folder,"/","Highly sim Clonotypes groups_","All Data_length_",cdr3_lengths[i],".txt")
      write.table(highly_sim_clonotypes_allGroups[[paste0("length ",cdr3_lengths[i])]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
    }
    
  }
  
  ####################################### Separate Datasets ####################################### 
  highly_sim_view_specific_clonotypes_datasets<-list()
  highly_sim_clonotypes_datasets<-list()
  highly_sim_clonotypes_allGroups_datasets<-list()
  
  one_run<-function(j){
    clono_allData_only_cdr3=clono_datasets[[name[j]]]
    if (str_detect(clono_datasets[[name[j]]]$clonotype[1]," - ") && take_gene=="No"){
      a2=strsplit(clono_datasets[[name[j]]]$clonotype," - ") 
      clono_allData_only_cdr3$clonotype=as.character(ldply(a2,function(s){t(data.frame(unlist(s)))})[,2])
      clono_allData_only_cdr3=as.data.table(clono_allData_only_cdr3[,1:3])[, lapply(.SD, sum), by = .(clonotype=clonotype)]
      clono_allData_only_cdr3=clono_allData_only_cdr3[order(-clono_allData_only_cdr3$N),]
    }
    if (str_detect(clono_datasets[[name[j]]]$clonotype[1]," - ") && take_gene=="Yes"){
      a2=strsplit(clono_datasets[[name[j]]]$clonotype," - ") 
      clono_allData_only_cdr3$clonotype=as.character(ldply(a2,function(s){t(data.frame(unlist(s)))})[,2])
      clono_allData_only_cdr3$gene=as.character(ldply(a2,function(s){t(data.frame(unlist(s)))})[,1])
    }
    
    clono_allData_only_cdr3$cluster_id=as.numeric(row.names(clono_allData_only_cdr3))
    
    #### analysis
    #for each cdr3 length
    highly_sim_view_specific_clonotypes_datasets[[name[j]]]<-list()
    for (i in 1:length(cdr3_lengths)){
      highly_sim_view_specific_clonotypes_datasets[[name[j]]][[paste0("length ",cdr3_lengths[i])]]=list()
      
      clonotypes_of_this_length=clono_allData_only_cdr3 %>% filter(str_length(clono_allData_only_cdr3$clonotype)==(cdr3_lengths[i]+2))
      
      end_process_for_this_length=F
      
      while(end_process_for_this_length==F){
        major_clonotype=clonotypes_of_this_length$clonotype[1]
        if (nrow(clonotypes_of_this_length)>0){
          if (clonotypes_of_this_length$Freq[1]>clonotype_freq_thr_for_highly_sim){
            if (take_gene=="Yes"){
              clonotypes_of_this_length_gene=clonotypes_of_this_length %>% filter(clonotypes_of_this_length$gene==clonotypes_of_this_length$gene[1])
              dist_from_major=stringdist(clonotypes_of_this_length_gene$clonotype,major_clonotype)
              matched_clonotypes=clonotypes_of_this_length_gene %>% filter(dist_from_major<=num_of_mismatches[i])
              not_matched_clonotypes=clonotypes_of_this_length %>% filter(!(clonotypes_of_this_length$clonotype %in% matched_clonotypes$clonotype))
            }else{
              dist_from_major=stringdist(clonotypes_of_this_length$clonotype,major_clonotype)
              matched_clonotypes=clonotypes_of_this_length %>% filter(dist_from_major<=num_of_mismatches[i])
              not_matched_clonotypes=clonotypes_of_this_length %>% filter(dist_from_major>num_of_mismatches[i])
            }
            
            if (nrow(matched_clonotypes)>0){
              #save it
              if (take_gene=="No"){
                highly_sim_view_specific_clonotypes_datasets[[name[j]]][[paste0("length ",cdr3_lengths[i])]][[clonotypes_of_this_length$clonotype[1]]]=matched_clonotypes #save the corresponding clonotype from clono_allData table not just the cdr3
                highly_sim_view_specific_clonotypes_datasets[[name[j]]][[paste0("length ",cdr3_lengths[i])]][[clonotypes_of_this_length$clonotype[1]]]$prev_cluster=row.names(matched_clonotypes) #save the corresponding clonotype from clono_allData table not just the cdr3
                
              }else{
                highly_sim_view_specific_clonotypes_datasets[[name[j]]][[paste0("length ",cdr3_lengths[i])]][[clono_datasets[[name[j]]]$clonotype[clonotypes_of_this_length$cluster_id[1]]]]=clono_datasets[[name[j]]][matched_clonotypes$cluster_id,] #save the corresponding clonotype from clono_allData table not just the cdr3
                highly_sim_view_specific_clonotypes_datasets[[name[j]]][[paste0("length ",cdr3_lengths[i])]][[clono_datasets[[name[j]]]$clonotype[clonotypes_of_this_length$cluster_id[1]]]]$prev_cluster=row.names(clono_datasets[[name[j]]][matched_clonotypes$cluster_id,]) #save the corresponding clonotype from clono_allData table not just the cdr3
              }
              
            }
            if (nrow(not_matched_clonotypes)==0){
              end_process_for_this_length=T
            }else{
              clonotypes_of_this_length=not_matched_clonotypes
            }
          }else{
            #terminate the process for this length
            end_process_for_this_length=T
          }
          
        }else{
          #terminate the process for this length
          end_process_for_this_length=T
        }
      }
      
    }
    
    ##### Results to tables
    highly_sim_clonotypes_datasets[[name[j]]]<-list()
    highly_sim_clonotypes_allGroups_datasets[[name[j]]]<-list()
    for (i in 1:length(cdr3_lengths)){
      clonotype=names(highly_sim_view_specific_clonotypes_datasets[[name[j]]][[paste0("length ",cdr3_lengths[i])]])
      if (!(is.null(clonotype))){
        N=c()
        Freq=c()
        prev_cluster=c()
        for (cl in 1:length(highly_sim_view_specific_clonotypes_datasets[[name[j]]][[paste0("length ",cdr3_lengths[i])]])){
          highly_sim_view_specific_clonotypes_datasets[[name[j]]][[paste0("length ",cdr3_lengths[i])]][[clonotype[cl]]]$HS_cluster_id=cl
          N=c(N,sum(highly_sim_view_specific_clonotypes_datasets[[name[j]]][[paste0("length ",cdr3_lengths[i])]][[clonotype[cl]]]$N))
          Freq=c(Freq,sum(highly_sim_view_specific_clonotypes_datasets[[name[j]]][[paste0("length ",cdr3_lengths[i])]][[clonotype[cl]]]$Freq))
          prev_cluster_c<-""
          for (k in 1:nrow(highly_sim_view_specific_clonotypes_datasets[[name[j]]][[paste0("length ",cdr3_lengths[i])]][[clonotype[cl]]])){
            prev_cluster_c=paste(prev_cluster_c,highly_sim_view_specific_clonotypes_datasets[[name[j]]][[paste0("length ",cdr3_lengths[i])]][[clonotype[cl]]]$prev_cluster[k])
          }
          prev_cluster=c(prev_cluster,prev_cluster_c)
        }
        highly_sim_clonotypes_datasets[[name[j]]][[paste0("length ",cdr3_lengths[i])]]=data.frame(clonotype,N,Freq,prev_cluster,stringsAsFactors = F) #I have this data frame for each length
        highly_sim_clonotypes_datasets[[name[j]]][[paste0("length ",cdr3_lengths[i])]]$HS_cluster_id=as.numeric(row.names(highly_sim_clonotypes_datasets[[name[j]]][[paste0("length ",cdr3_lengths[i])]]))
      }
      highly_sim_clonotypes_allGroups_datasets[[name[j]]][[paste0("length ",cdr3_lengths[i])]]=do.call(rbind.data.frame, highly_sim_view_specific_clonotypes_datasets[[name[j]]][[paste0("length ",cdr3_lengths[i])]])
      
      #extra clonotypes
      if (take_gene=="Yes"){
        clonotypes_of_this_length_id=which(str_length(clono_allData_only_cdr3$clonotype)==(cdr3_lengths[i]+2))
        extra_clono=clono_datasets[[name[j]]][clonotypes_of_this_length_id,] %>% filter(!(clono_datasets[[name[j]]][clonotypes_of_this_length_id,]$clonotype %in% highly_sim_clonotypes_allGroups_datasets[[name[j]]][[paste0("length ",cdr3_lengths[i])]]$clonotype))
      }else{
        clonotypes_of_this_length=clono_allData_only_cdr3 %>% filter(str_length(clono_allData_only_cdr3$clonotype)==(cdr3_lengths[i]+2))
        extra_clono=clonotypes_of_this_length %>% filter(!(clonotypes_of_this_length$clonotype %in% highly_sim_clonotypes_allGroups_datasets[[name[j]]][[paste0("length ",cdr3_lengths[i])]]$clonotype))
      }
      
      if (nrow(extra_clono)>0){
        extra_clono$prev_cluster=0
        for (k in 1:nrow(extra_clono)){
          temp=as.character(extra_clono[["Convergent Evolution"]][k])
          extra_clono$prev_cluster[k]=as.numeric(strsplit(strsplit(temp,"cluster ")[[1]][2]," :")[[1]][1])
        }
        if (!(is.null(clonotype))){
          extra_clono$HS_cluster_id=(max(highly_sim_clonotypes_datasets[[name[j]]][[paste0("length ",cdr3_lengths[i])]]$HS_cluster_id)+1):(max(highly_sim_clonotypes_datasets[[name[j]]][[paste0("length ",cdr3_lengths[i])]]$HS_cluster_id)+nrow(extra_clono))
        }else{
          extra_clono$HS_cluster_id=1:nrow(extra_clono)
        }
        highly_sim_clonotypes_allGroups_datasets[[name[j]]][[paste0("length ",cdr3_lengths[i])]]=rbind(highly_sim_clonotypes_allGroups_datasets[[name[j]]][[paste0("length ",cdr3_lengths[i])]],extra_clono)
        highly_sim_clonotypes_datasets[[name[j]]][[paste0("length ",cdr3_lengths[i])]]=rbind(highly_sim_clonotypes_datasets[[name[j]]][[paste0("length ",cdr3_lengths[i])]],extra_clono[,c("clonotype","N","Freq","prev_cluster","HS_cluster_id")])
        
        row.names(highly_sim_clonotypes_allGroups_datasets[[name[j]]][[paste0("length ",cdr3_lengths[i])]])=1:nrow(highly_sim_clonotypes_allGroups_datasets[[name[j]]][[paste0("length ",cdr3_lengths[i])]])
        
      }
      
      if (save_tables_individually){
        filename=paste0(output_folder,"/","Highly sim Clonotypes_",name[j],"_length_",cdr3_lengths[i],".txt")
        write.table(highly_sim_clonotypes_datasets[[name[j]]][[paste0("length ",cdr3_lengths[i])]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
        filename=paste0(output_folder,"/","Highly sim Clonotypes groups_",name[j],"_length_",cdr3_lengths[i],".txt")
        write.table(highly_sim_clonotypes_allGroups_datasets[[name[j]]][[paste0("length ",cdr3_lengths[i])]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
      }    
    }
    
    result=list()
    result[["highly_sim_clonotypes_allGroups_datasets"]]=highly_sim_clonotypes_allGroups_datasets[[name[j]]]
    result[["highly_sim_clonotypes_datasets"]]=highly_sim_clonotypes_datasets[[name[j]]]
    result[["highly_sim_view_specific_clonotypes_datasets"]]=highly_sim_view_specific_clonotypes_datasets[[name[j]]]
    
    return(result)  
  } 
  
  if (Sys.info()[1] == "Windows"){
    a=lapply(1:length(name),one_run)
    for (i in 1:length(name)){
      highly_sim_clonotypes_allGroups_datasets[[name[i]]]=a[[i]]$highly_sim_clonotypes_allGroups_datasets
      clono_datasets[[name[i]]]=a[[i]]$clono_datasets
      highly_sim_clonotypes_datasets[[name[i]]]=a[[i]]$highly_sim_clonotypes_datasets
      highly_sim_view_specific_clonotypes_datasets[[name[i]]]=a[[i]]$highly_sim_view_specific_clonotypes_datasets
    }
  }else{
    a=lapply(1:length(name),one_run)
    #a=mclapply(1:length(name),one_run,mc.cores = num_of_cores, mc.preschedule = TRUE)
    for (i in 1:length(name)){
      highly_sim_clonotypes_allGroups_datasets[[name[i]]]=a[[i]]$highly_sim_clonotypes_allGroups_datasets
      clono_datasets[[name[i]]]=a[[i]]$clono_datasets
      highly_sim_clonotypes_datasets[[name[i]]]=a[[i]]$highly_sim_clonotypes_datasets
      highly_sim_view_specific_clonotypes_datasets[[name[i]]]=a[[i]]$highly_sim_view_specific_clonotypes_datasets
    }
  }
  
  confirm<-paste0("Highly Similar Clonotypes run!")
  
  result=list("highly_sim_view_specific_clonotypes"=highly_sim_view_specific_clonotypes,"highly_sim_clonotypes"=highly_sim_clonotypes,
              "highly_sim_view_specific_clonotypes_datasets"=highly_sim_view_specific_clonotypes_datasets,"highly_sim_clonotypes_datasets"=highly_sim_clonotypes_datasets,
              "highly_sim_clonotypes_allGroups"=highly_sim_clonotypes_allGroups,"highly_sim_clonotypes_allGroups_datasets"=highly_sim_clonotypes_allGroups_datasets,
              "confirm"=confirm)
  
  # log time end and memory used 
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  cat(pryr::mem_used(), file=logFile, append=TRUE, sep = "\n")
  
  return(result)
  
  
}


######################################################################################################################################

public_clonotypes<- function(clono_allData,clono_datasets,take_gene,use_reads,public_clonotype_thr,name, highly){ 
  #logfile
  cat(paste0("public_clonotypes","\t"), file=logFile, append=TRUE)
  cat(paste0(paste("take_gene ",take_gene,"threshold",public_clonotype_thr,sep = ","),"\t"), file=logFile, append=TRUE)
  cat(paste0(nrow(clono_allData),"\t"), file=logFile, append=TRUE)
  cat(paste0(ncol(clono_allData),"\t"), file=logFile, append=TRUE)
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  
  if (str_detect(clono_allData$clonotype[1]," - ") && take_gene=="No"){
    a2=strsplit(clono_allData$clonotype," - ") 
    clono_allData$clonotype=as.character(ldply(a2,function(s){t(data.frame(unlist(s)))})[,2])
    clono_allData=as.data.table(clono_allData[,1:3])[, lapply(.SD, sum), by = .(clonotype=clonotype)]
    clono_allData=clono_allData[order(-clono_allData$N),]
  }
  
  initial_sum<-list()
  for (n in name){
    initial_sum[[n]]=sum(clono_datasets[[n]]$N)
  }
  
  if (use_reads){
    clono_allData <- clono_allData %>% filter(N>public_clonotype_thr)
  }else{
    clono_allData <- clono_allData[1:public_clonotype_thr,]
  }

  public_clono<-data.frame(clonotype=unique(clono_allData$clonotype),stringsAsFactors = F)
  
  #for each dataset 
  for (n in name){
    if (str_detect(clono_datasets[[n]]$clonotype[1]," - ") && take_gene=="No"){
      a2=strsplit(clono_datasets[[n]]$clonotype," - ") 
      clono_datasets[[n]]$clonotype=as.character(ldply(a2,function(s){t(data.frame(unlist(s)))})[,2])
      clono_datasets[[n]]=as.data.table(clono_datasets[[n]][,1:3])[, lapply(.SD, sum), by = .(clonotype=clonotype)]
      clono_datasets[[n]]=clono_datasets[[n]][order(-clono_datasets[[n]]$N),]
    }
    
    if (use_reads){
      clono_datasets[[n]] <- clono_datasets[[n]] %>% filter(N>public_clonotype_thr)
    }else{
      clono_datasets[[n]] <- clono_datasets[[n]][1:public_clonotype_thr,]
    }
    
    ids_dataset=which(clono_datasets[[n]]$clonotype %in% public_clono$clonotype) 
    ids=match(clono_datasets[[n]]$clonotype,public_clono$clonotype)[which(!(is.na(match(clono_datasets[[n]]$clonotype,public_clono$clonotype))))]
    public_clono[[paste0(n,"_Reads/Total")]] <- NA
    public_clono[[paste0(n,"_Freq")]] <- NA
    public_clono[[paste0(n,"_Reads/Total")]][ids] = paste0(clono_datasets[[n]]$N[ids_dataset],"/",initial_sum[[n]])
    public_clono[[paste0(n,"_Freq")]][ids] = clono_datasets[[n]]$Freq[ids_dataset]
  }
  
  public_clono$Num_of_patients<-NA
  
  #filter results
  public_clono$Num_of_patients <- (apply(public_clono, 1, function(x) sum(!(is.na(x))))-1)/2
  public_clono <- public_clono %>% filter(Num_of_patients>1)
  
  #replace NA with 0
  public_clono[is.na(public_clono)] <- 0
  
  if (save_tables_individually){
    if (highly){
      filename = paste0(output_folder,"/","public_highly_clonotypes",".txt")
      write.table(public_clono, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
    }else{
      filename = paste0(output_folder,"/","public_clonotypes",".txt")
      write.table(public_clono, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
    }
    
  }
  
  confirm<-paste0("Shared Clonotypes run!")
  
  result=list("public_clono"=public_clono,"confirm"=confirm)
  
  # log time end and memory used 
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  cat(pryr::mem_used(), file=logFile, append=TRUE, sep = "\n")
  
  return(result)
  
}


######################################################################################################################################

createLink <- function(val,on_click_js) {
  #'<a class="btn btn-primary">Info</a>'
  as.character(tags$a(href = "#", onclick = sprintf(on_click_js,val), val))
}


######################################################################################################################################

viewClonotypes <- function(allData,allele,gene,junction,val1,val2) {
  #logfile
  cat(paste0("viewClonotypes","\t"), file=logFile, append=TRUE)
  cat(paste0(nrow(allData),"\t"), file=logFile, append=TRUE)
  cat(paste0(ncol(allData),"\t"), file=logFile, append=TRUE)
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  
  temp=allData
  if (length(gene)>0){
    if (allele==F){
      for (i in 1:nrow(temp)){
        temp[[gene]][i]=strsplit(temp[[gene]][i],"[*]")[[1]][1]
      }
    }else{
      for (i in 1:nrow(temp)){
        temp[[gene]][i]=strsplit(temp[[gene]][i],"(see comment)")[[1]][1]
      }
    }
    inputVGenes_CDR3=which((temp[[gene]]==val1) & (temp[[junction]]==val2))  
  }else{
    inputVGenes_CDR3=which(temp[[junction]]==val1)
  }
  
  a=allData[inputVGenes_CDR3,]
  
  # log time end and memory used 
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  cat(pryr::mem_used(), file=logFile, append=TRUE, sep = "\n")
  
  return(a)
}


######################################################################################################################################

repertoires<- function(clono_allData,clono_datasets,allele,allele_clonotypes,gene,gene_clonotypes,name,view_specific_clonotype_allData,view_specific_clonotype_datasets,highly_sim){
  if (allele==F){
    g=str_replace(gene,".and.allele","")
  }else{
    g=gene
  }
  #logfile
  cat(paste0("repertoires","\t"), file=logFile, append=TRUE)
  cat(paste0(g,"\t"), file=logFile, append=TRUE)
  cat(paste0(nrow(clono_allData),"\t"), file=logFile, append=TRUE)
  cat(paste0(ncol(clono_allData),"\t"), file=logFile, append=TRUE)
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  
  #for (i in 1:nrow(clono_allData)){
  #my_table <- viewClonotypes(clono_allData,allele,gene,junction,clonotype[[1]][1],clonotype[[1]][2])
  #}
  
  for (i in 1:nrow(clono_allData)){
    clono_allData[i,1]=strsplit(as.character(clono_allData[i,1])," - ")[[1]][1]
  }
  
  for (j in 1:length(name)){
    for (i in 1:nrow(clono_datasets[[name[j]]])){
      clono_datasets[[name[j]]][i,1]=strsplit(as.character(clono_datasets[[name[j]]][i,1])," - ")[[1]][1]   
    }
  }
  
  
  if (gene==gene_clonotypes && allele==allele_clonotypes && !(is.null(gene_clonotypes))){
    ####################################### All Data
    Repertoires_allData=clono_allData %>% group_by(clono_allData[["clonotype"]]) %>% summarise(n=n())
    Repertoires_allData <- Repertoires_allData[order(-Repertoires_allData$n),] 
    Repertoires_allData=cbind(Repertoires_allData,Freq=100*Repertoires_allData$n/nrow(clono_allData))
    
    ####################################### Separate Datasets
    Repertoires_datasets<-list()
    one_run<-function(j){
      Repertoires_datasets[[name[j]]]=clono_datasets[[name[j]]] %>% group_by(clono_datasets[[name[j]]][["clonotype"]]) %>% summarise(n=n())
      Repertoires_datasets[[name[j]]] <- Repertoires_datasets[[name[j]]][order(-Repertoires_datasets[[name[j]]]$n),] 
      Repertoires_datasets[[name[j]]]=cbind(Repertoires_datasets[[name[j]]],Freq=100*Repertoires_datasets[[name[j]]]$n/nrow(clono_datasets[[name[j]]]))
      colnames(Repertoires_datasets[[name[j]]])=c("Gene","N","Freq")
      
      if (save_tables_individually){
        filename=paste0(output_folder,"/","Repertoires_",g,"_",name[j],".txt")
        write.table(Repertoires_datasets[[name[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
      }
      return(Repertoires_datasets[[name[j]]])  
    } 
    
    if (Sys.info()[1] == "Windows"){
      #cl <- makeCluster(num_of_cores)
      #Repertoires_datasets=clusterApply(cl=cl,1:length(name),one_run)
      Repertoires_datasets=lapply(1:length(name),one_run)
    }else{
      Repertoires_datasets=mclapply(1:length(name),one_run,mc.cores = num_of_cores, mc.preschedule = TRUE)
    }
    
    names(Repertoires_datasets)=name
  }else{
    #find the most frequent gene that exists in each specific clonotype
    ####################################### All Data
    freq_gene_name<-data.frame()
    for (i in names(view_specific_clonotype_allData)){
      a=view_specific_clonotype_allData[[i]]
      #if ((allele==F) & (allele!=allele_clonotypes)){
      if (allele==F){
        if (!all(!(str_detect(a[[gene]],"[*]")))){
          a2=strsplit(a[[gene]],"[*]") 
          a[[gene]]=as.character(ldply(a2,function(s){t(data.frame(unlist(s)))})[,1])
        }
      }
      freq_gene = a %>% group_by(a[[gene]]) %>% summarise(n=n())
      freq_gene <- freq_gene[order(-freq_gene$n),] 
      freq_gene_name[i,1]=freq_gene[1,1]
    }
    
    colnames(freq_gene_name)=c("Gene")
    freq_gene_name = freq_gene_name %>% group_by(freq_gene_name[["Gene"]]) %>% summarise(n=n())
    freq_gene_name <- freq_gene_name[order(-freq_gene_name$n),]
    freq_gene_name=cbind(freq_gene_name,Freq=100*freq_gene_name$n/nrow(clono_allData))
    colnames(freq_gene_name)=c("Gene","N","Freq")
    
    
    ####################################### Separate Datasets
    freq_gene_name_datasets<-list()
    
    one_run<-function(j){
      freq_gene_name_datasets[[name[[j]]]]=data.frame()
      for (i in names(view_specific_clonotype_datasets[[name[j]]])){
        a=view_specific_clonotype_datasets[[name[j]]][[i]]
        if (allele==F){
          if (!all(!(str_detect(a[[gene]],"[*]")))){
            a2=strsplit(a[[gene]],"[*]") 
            a[[gene]]=as.character(ldply(a2,function(s){t(data.frame(unlist(s)))})[,1])
          }
        }
        freq_gene = a %>% group_by(a[[gene]]) %>% summarise(n=n())
        freq_gene <- freq_gene[order(-freq_gene$n),] 
        freq_gene_name_datasets[[name[[j]]]][i,1]=freq_gene[1,1]
      }
      colnames(freq_gene_name_datasets[[name[[j]]]])=c("Gene")
      
      freq_gene_name_datasets[[name[[j]]]] = freq_gene_name_datasets[[name[[j]]]] %>% group_by(freq_gene_name_datasets[[name[[j]]]][["Gene"]]) %>% summarise(n=n())
      freq_gene_name_datasets[[name[j]]] <- freq_gene_name_datasets[[name[j]]][order(-freq_gene_name_datasets[[name[j]]]$n),] 
      freq_gene_name_datasets[[name[j]]]=cbind(freq_gene_name_datasets[[name[j]]],Freq=100*freq_gene_name_datasets[[name[j]]]$n/nrow(clono_datasets[[name[j]]]))
      
      colnames(freq_gene_name_datasets[[name[j]]])=c("Gene","N","Freq")
      
      if (save_tables_individually){
        filename=paste0(output_folder,"/","Repertoires_",g,"_",name[j],".txt")
        write.table(freq_gene_name_datasets[[name[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
      }
      return(freq_gene_name_datasets[[name[j]]])  
    } 
    
    if (Sys.info()[1] == "Windows"){
      #cl <- makeCluster(num_of_cores)
      #freq_gene_name_datasets=clusterApply(cl=cl,1:length(name),one_run)
      freq_gene_name_datasets=lapply(1:length(name),one_run)
    }else{
      freq_gene_name_datasets=mclapply(1:length(name),one_run,mc.cores = num_of_cores, mc.preschedule = TRUE)
    }
    
    names(freq_gene_name_datasets)=name
    
    Repertoires_allData=freq_gene_name
    Repertoires_datasets=freq_gene_name_datasets
  }
  
  
  colnames(Repertoires_allData)=c("Gene","N","Freq")
  
  if (save_tables_individually){
    filename=paste0(output_folder,"/","Repertoires_",g,"_","All Data",".txt")
    write.table(Repertoires_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
  }
  
  
  
  confirm<-paste0("Repertoires run!")
  
  result=list("Repertoires_allData"=Repertoires_allData,"Repertoires_datasets"=Repertoires_datasets,"confirm"=confirm)
  
  # log time end and memory used 
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  cat(pryr::mem_used(), file=logFile, append=TRUE, sep = "\n")
  
  return(result)
}


######################################################################################################################################

repertoires_highly_similar<- function(clono_allData,clono_datasets,allele,allele_clonotypes,gene,gene_clonotypes,name,view_specific_clonotype_allData,view_specific_clonotype_datasets,take_gene){
  #logfile
  if (allele==F){
    g=str_replace(gene,".and.allele","")
  }else{
    g=gene
  }
  #logfile
  cat(paste0("repertoires_highly_similar","\t"), file=logFile, append=TRUE)
  cat(paste0(g,"\t"), file=logFile, append=TRUE)
  cat(paste0(nrow(clono_allData),"\t"), file=logFile, append=TRUE)
  cat(paste0(ncol(clono_allData),"\t"), file=logFile, append=TRUE)
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  
  if (allele==F){
    g=str_replace(gene,".and.allele","")
  }else{
    g=gene
  }
  
  #for (i in 1:nrow(clono_allData)){
  #my_table <- viewClonotypes(clono_allData,allele,gene,junction,clonotype[[1]][1],clonotype[[1]][2])
  #}
  
  clono_allData_initial<-clono_allData
  clono_datasets_initial<-clono_datasets
  
  for (i in 1:nrow(clono_allData)){
    clono_allData[i,1]=strsplit(as.character(clono_allData[i,1])," - ")[[1]][1]
  }
  
  for (j in 1:length(name)){
    for (i in 1:nrow(clono_datasets[[name[j]]])){
      clono_datasets[[name[j]]][i,1]=strsplit(as.character(clono_datasets[[name[j]]][i,1])," - ")[[1]][1]   
    }
  }
  
  
  if (gene==gene_clonotypes && allele==allele_clonotypes && take_gene=="Yes" && length(gene_clonotypes)>0){
    ####################################### All Data
    Repertoires_allData=clono_allData %>% group_by(clono_allData[["clonotype"]]) %>% summarise(n=n())
    Repertoires_allData <- Repertoires_allData[order(-Repertoires_allData$n),] 
    Repertoires_allData=cbind(Repertoires_allData,Freq=100*Repertoires_allData$n/nrow(clono_allData))
    
    ####################################### Separate Datasets
    Repertoires_datasets<-list()
    one_run<-function(j){
      Repertoires_datasets[[name[j]]]=clono_datasets[[name[j]]] %>% group_by(clono_datasets[[name[j]]][["clonotype"]]) %>% summarise(n=n())
      Repertoires_datasets[[name[j]]] <- Repertoires_datasets[[name[j]]][order(-Repertoires_datasets[[name[j]]]$n),] 
      Repertoires_datasets[[name[j]]]=cbind(Repertoires_datasets[[name[j]]],Freq=100*Repertoires_datasets[[name[j]]]$n/nrow(clono_datasets[[name[j]]]))
      colnames(Repertoires_datasets[[name[j]]])=c("Gene","N","Freq")
      
      if (save_tables_individually){
        filename=paste0(output_folder,"/","Repertoires_HighlySim_",g,"_",name[j],".txt")
        write.table(Repertoires_datasets[[name[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
      }
      return(Repertoires_datasets[[name[j]]])  
    } 
    
    if (Sys.info()[1] == "Windows"){
      Repertoires_datasets=lapply(1:length(name),one_run)
    }else{
      Repertoires_datasets=mclapply(1:length(name),one_run,mc.cores = num_of_cores, mc.preschedule = TRUE)
    }
    
    names(Repertoires_datasets)=name
    
  }else{
    #find the most frequent gene that exists in each specific clonotype
    
    ####################################### All Data
    freq_gene_name<-data.frame()
    
    if (take_gene=="Yes"){
      for (i in names(view_specific_clonotype_allData)){
        if (i %in% clono_allData_initial$clonotype){
          a=view_specific_clonotype_allData[[i]]
          if (allele==F){
            if (!all(!(str_detect(a[[gene]],"[*]")))){
              a2=strsplit(a[[gene]],"[*]") 
              a[[gene]]=as.character(ldply(a2,function(s){t(data.frame(unlist(s)))})[,1])
            }
            
            #a2=strsplit(a[[gene]],"[*]") 
            #a[[gene]]=as.character(plyr::ldply(a2,function(s){t(data.frame(unlist(s)))})[,1])
          }
          freq_gene = a %>% group_by(a[[gene]]) %>% summarise(n=n())
          freq_gene <- freq_gene[order(-freq_gene$n),] 
          freq_gene_name[i,1]=freq_gene[1,1]
        }
      }
      
    }
    colnames(freq_gene_name)=c("Gene")
    freq_gene_name = freq_gene_name %>% group_by(freq_gene_name[["Gene"]]) %>% summarise(n=n())
    
    freq_gene_name <- freq_gene_name[order(-freq_gene_name$n),]
    freq_gene_name=cbind(freq_gene_name,Freq=100*freq_gene_name$n/nrow(clono_allData))
    colnames(freq_gene_name)=c("Gene","N","Freq")
    
    ####################################### Separate Datasets
    freq_gene_name_datasets<-list()
    one_run<-function(j){
      freq_gene_name_datasets[[name[[j]]]]=data.frame()
      if (take_gene=="Yes"){
        for (i in names(view_specific_clonotype_datasets[[name[j]]])){
          if (i %in% clono_datasets_initial[[name[j]]]$clonotype){
            a=view_specific_clonotype_datasets[[name[j]]][[i]]
            if (allele==F){
              if (!all(!(str_detect(a[[gene]],"[*]")))){
                a2=strsplit(a[[gene]],"[*]") 
                a[[gene]]=as.character(ldply(a2,function(s){t(data.frame(unlist(s)))})[,1])
              }
            }
            freq_gene = a %>% group_by(a[[gene]]) %>% summarise(n=n())
            freq_gene <- freq_gene[order(-freq_gene$n),] 
            freq_gene_name_datasets[[name[[j]]]][i,1]=freq_gene[1,1]
          }
        }
        
        colnames(freq_gene_name_datasets[[name[[j]]]])=c("Gene")
        freq_gene_name_datasets[[name[[j]]]] = freq_gene_name_datasets[[name[[j]]]] %>% group_by(freq_gene_name_datasets[[name[[j]]]][["Gene"]]) %>% summarise(n=n())
        freq_gene_name_datasets[[name[j]]] <- freq_gene_name_datasets[[name[j]]][order(-freq_gene_name_datasets[[name[j]]]$n),] 
        freq_gene_name_datasets[[name[j]]]=cbind(freq_gene_name_datasets[[name[j]]],Freq=100*freq_gene_name_datasets[[name[j]]]$n/nrow(clono_datasets[[name[j]]]))
        colnames(freq_gene_name_datasets[[name[j]]])=c("Gene","N","Freq")
        
        if (save_tables_individually){
          filename=paste0(output_folder,"/","Repertoires_HiglySim_",g,"_",name[j],".txt")
          write.table(freq_gene_name_datasets[[name[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
        }
      }
      
      return(freq_gene_name_datasets[[name[j]]])  
    } 
    
    if (Sys.info()[1] == "Windows"){
      #cl <- makeCluster(num_of_cores)
      freq_gene_name_datasets=lapply(1:length(name),one_run)
    }else{
      freq_gene_name_datasets=mclapply(1:length(name),one_run,mc.cores = num_of_cores, mc.preschedule = TRUE)
    }
    
    names(freq_gene_name_datasets)=name
    
    Repertoires_allData=freq_gene_name
    Repertoires_datasets=freq_gene_name_datasets
  }
  
  colnames(Repertoires_allData)=c("Gene","N","Freq")
  
  if (save_tables_individually){
    filename=paste0(output_folder,"/","Repertoires_HighlySim_",g,"_","All Data",".txt")
    write.table(Repertoires_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
  }
  
  
  confirm<-paste0("Repertoires run!")
  
  result=list("Repertoires_allData"=Repertoires_allData,"Repertoires_datasets"=Repertoires_datasets,"confirm"=confirm)
  
  # log time end and memory used 
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  cat(pryr::mem_used(), file=logFile, append=TRUE, sep = "\n")
  
  return(result)
}


######################################################################################################################################

repertoires_comparison<- function(Repertoires_allData,Repertoires_datasets,name,highly_sim,id){ #set name equal to the selected dataset
  #logfile
  if (!highly_sim){
    n="repertoires_comparison"
  }else{
    n="repertoires_comparison_higly_similar"
  }
  cat(paste0(n,"\t"), file=logFile, append=TRUE)
  cat(paste0(paste("Number of datasets: ",length(name),sep = ""),"\t"), file=logFile, append=TRUE)
  cat(paste0(nrow(Repertoires_allData),"\t"), file=logFile, append=TRUE)
  cat(paste0(ncol(Repertoires_allData),"\t"), file=logFile, append=TRUE)
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  
  unique_repertoires<-data.frame(Gene=Repertoires_allData$Gene,stringsAsFactors = F)
  
  #for each dataset 
  for (n in name){
    ids_dataset=which(Repertoires_datasets[[n]]$Gene %in% unique_repertoires$Gene) 
    ids=match(Repertoires_datasets[[n]]$Gene,unique_repertoires$Gene)[which(!(is.na(match(Repertoires_datasets[[n]]$Gene,unique_repertoires$Gene))))]
    unique_repertoires[[paste0(n,"_N/Total")]] <- NA
    unique_repertoires[[paste0(n,"_Freq")]] <- NA
    unique_repertoires[[paste0(n,"_N/Total")]][ids] = paste0(Repertoires_datasets[[n]]$N[ids_dataset],"/",sum(Repertoires_datasets[[n]]$N))
    unique_repertoires[[paste0(n,"_Freq")]][ids] = Repertoires_datasets[[n]]$Freq[ids_dataset]
  }
  
  #replace NA with 0
  unique_repertoires[is.na(unique_repertoires)] <- 0
  
  #mean freq
  unique_repertoires$Mean_Freq<-NA
  unique_repertoires$Mean_Freq <- apply(unique_repertoires[,seq(3,ncol(unique_repertoires),2)], 1, function(x) mean(x))
  
  if (save_tables_individually){
    if (highly_sim){
      filename=paste0(output_folder,"/","highlySim_repertoires_comparison_table_",id,".txt")
      write.table(unique_repertoires, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
    }else{
      filename=paste0(output_folder,"/","repertoires_comparison_table_",id,".txt")
      write.table(unique_repertoires, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
    }
  }
  
  confirm<-paste0("Repertoires Comparison run!")
  
  result=list("unique_repertoires"=unique_repertoires,"confirm"=confirm)
  
  # log time end and memory used 
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  cat(pryr::mem_used(), file=logFile, append=TRUE, sep = "\n")
  
  return(result)
}


######################################################################################################################################

Multiple_value_comparison<-function(clono_allData,clono_datasets,allele_clonotypes,gene_clonotypes,view_specific_clonotype_allData,view_specific_clonotype_datasets,val1,val2,name,identity_groups){
  #logfile
  cat(paste0("Multiple_value_comparison","\t"), file=logFile, append=TRUE)
  cat(paste0(paste(val1,val2,sep = ","),"\t"), file=logFile, append=TRUE)
  cat(paste0(nrow(clono_allData),"\t"), file=logFile, append=TRUE)
  cat(paste0(ncol(clono_allData),"\t"), file=logFile, append=TRUE)
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  
  val1_initial=val1
  val2_initial=val2
  
  val_initial=c(val1_initial,val2_initial)
  
  if (val1=="Molecular mass" || val1=="pI"){
    val1=paste0("Junction.",val1)
  }else{
    val1=paste0("Summary.",val1)
  }
  
  val1=gsub(" ",".",val1)
  val1=gsub("-",".",val1)
  val1=gsub("%",".",val1)
  
  if (val2=="Molecular mass" || val2=="pI"){
    val2=paste0("Junction.",val2)
  }else{
    val2=paste0("Summary.",val2)
  }
  
  val2=gsub(" ",".",val2)
  val2=gsub("-",".",val2)
  val2=gsub("%",".",val2)
  
  if (!str_detect(val1,"allele") && str_detect(val1,"GENE")){
    val1=paste0(val1,".and.allele")
  }
  
  if (!str_detect(val2,"allele") && str_detect(val2,"GENE")){
    val2=paste0(val2,".and.allele")
  }
  
  Multiple_value_comparison_datasets<-list()
  
  for (i in 1:nrow(clono_allData)){
    clono_allData[i,1]=strsplit(as.character(clono_allData[i,1])," - ")[[1]][1]
  }
  
  for (j in 1:length(name)){
    for (i in 1:nrow(clono_datasets[[name[j]]])){
      clono_datasets[[name[j]]][i,1]=strsplit(as.character(clono_datasets[[name[j]]][i,1])," - ")[[1]][1]   
    }
  }
  
  multi_allData<-c()
  
  val=c(val1,val2)
  
  multi_datasets<-list()
  
  for (vals in 1:2){
    if (str_detect(val[vals],"GENE")){
      gene=val[vals]
      
      if (gene==gene_clonotypes && (str_detect(val_initial[vals],"allele"))==allele_clonotypes && !(is.null(gene_clonotypes))){
        ####################################### All Data
        multi_allData=cbind(multi_allData,clono_allData[["clonotype"]])
        colnames(multi_allData)[ncol(multi_allData)]=gene
        
        ####################################### Seperate Datasets
        one_run<-function(j){
          multi_datasets[[name[j]]]=cbind(multi_datasets[[name[j]]],clono_datasets[[name[j]]][["clonotype"]])
          colnames(multi_datasets[[name[j]]])[ncol(multi_datasets[[name[j]]])]=gene
          
          return(multi_datasets[[name[j]]])  
        } 
        
        if (Sys.info()[1] == "Windows"){
          #cl <- makeCluster(num_of_cores)
          #Multiple_value_comparison_datasets=clusterApply(cl=cl,1:length(name),one_run)
          multi_datasets=lapply(1:length(name),one_run)
        }else{
          multi_datasets=mclapply(1:length(name),one_run,mc.cores = num_of_cores, mc.preschedule = TRUE)
        }
        
        names(multi_datasets)=name
      }else{
        #find the most frequent gene that exists in each specific clonotype
        ####################################### All Data
        freq_gene_name<-data.frame()
        for (i in names(view_specific_clonotype_allData)){
          a=view_specific_clonotype_allData[[i]]
          if ((str_detect(val_initial[vals],"allele")==F)){
            if (!all(!(str_detect(a[[gene]],"[*]")))){
              a2=strsplit(a[[gene]],"[*]") 
              a[[gene]]=as.character(plyr::ldply(a2,function(s){t(data.frame(unlist(s)))})[,1])
            }
          }
          freq_gene = a %>% group_by(a[[gene]]) %>% summarise(n=n())
          freq_gene <- freq_gene[order(-freq_gene$n),] 
          freq_gene_name[i,1]=freq_gene[1,1]
        }
        
        colnames(freq_gene_name)=gene
        
        multi_allData=cbind(multi_allData,freq_gene_name[[gene]])
        colnames(multi_allData)[ncol(multi_allData)]=gene 
        
        ####################################### Seperate Datasets
        one_run<-function(j){
          freq_gene_name<-data.frame()
          for (i in names(view_specific_clonotype_datasets[[name[j]]])){
            a=view_specific_clonotype_datasets[[name[j]]][[i]]
            if ((str_detect(val_initial[vals],"allele")==F)){
              if (!all(!(str_detect(a[[gene]],"[*]")))){
                a2=strsplit(a[[gene]],"[*]") 
                a[[gene]]=as.character(plyr::ldply(a2,function(s){t(data.frame(unlist(s)))})[,1])
              }
            }
            freq_gene = a %>% group_by(a[[gene]]) %>% summarise(n=n())
            freq_gene <- freq_gene[order(-freq_gene$n),] 
            freq_gene_name[i,1]=freq_gene[1,1]
          }
          
          colnames(freq_gene_name)=gene
          
          multi_datasets[[name[j]]]=cbind(multi_datasets[[name[j]]],freq_gene_name[[gene]])
          colnames(multi_datasets[[name[j]]])[ncol(multi_datasets[[name[j]]])]=gene 
          
          return(multi_datasets[[name[j]]])  
        } 
        
        if (Sys.info()[1] == "Windows"){
          #cl <- makeCluster(num_of_cores)
          #Multiple_value_comparison_datasets=clusterApply(cl=cl,1:length(name),one_run)
          multi_datasets=lapply(1:length(name),one_run)
        }else{
          multi_datasets=mclapply(1:length(name),one_run,mc.cores = num_of_cores, mc.preschedule = TRUE)
        }
        
        names(multi_datasets)=name
      }
    }else{
      a=c()
      for (i in names(view_specific_clonotype_allData)){
        a=c(a,median(as.numeric(view_specific_clonotype_allData[[i]][[val[vals]]]),na.rm = TRUE))
      }
      if ((!is.null(identity_groups)) && (val[vals]==used_columns[["Summary"]][4])){
        a=as.numeric(a)
        temp=a
        for (values in 1:nrow(identity_groups)){
          if (values==nrow(identity_groups))  index=which(a>=identity_groups[values,1] & a<=identity_groups[values,2])
          else index=which(a>=identity_groups[values,1] & a<identity_groups[values,2])
          temp[index]=identity_groups$label[values]
        }
        a=temp
      }
      
      multi_allData=cbind(multi_allData,a)
      colnames(multi_allData)[ncol(multi_allData)]=val[vals] 
      
      ####################################### Seperate Datasets
      one_run<-function(j){
        a=c()
        for (i in names(view_specific_clonotype_datasets[[name[j]]])){
          a=c(a,median(as.numeric(view_specific_clonotype_datasets[[name[j]]][[i]][[val[vals]]]),na.rm = TRUE))
        }
        if ((!is.null(identity_groups)) && (val[vals]==used_columns[["Summary"]][4])){
          a=as.numeric(a)
          temp=a
          for (values in 1:nrow(identity_groups)){
            if (values==nrow(identity_groups))  index=which(a>=identity_groups[values,1] & a<=identity_groups[values,2])
            else index=which(a>=identity_groups[values,1] & a<identity_groups[values,2])
            temp[index]=identity_groups$label[values]
          }
          a=temp
        }
        
        multi_datasets[[name[j]]]=cbind(multi_datasets[[name[j]]],a)
        colnames(multi_datasets[[name[j]]])[ncol(multi_datasets[[name[j]]])]=val[vals]
        return(multi_datasets[[name[j]]])  
      } 
      
      if (Sys.info()[1] == "Windows"){
        #cl <- makeCluster(num_of_cores)
        #Multiple_value_comparison_datasets=clusterApply(cl=cl,1:length(name),one_run)
        multi_datasets=lapply(1:length(name),one_run)
      }else{
        multi_datasets=mclapply(1:length(name),one_run,mc.cores = num_of_cores, mc.preschedule = TRUE)
      }
      
      names(multi_datasets)=name
    }
    
  }
  multi_allData=data.frame(multi_allData,stringsAsFactors = F)
  
  Multiple_value_comparison_allData<- multi_allData %>% group_by(multi_allData[[val1]],multi_allData[[val2]]) %>% summarise(n=n())
  Multiple_value_comparison_allData=Multiple_value_comparison_allData[order(-Multiple_value_comparison_allData$n),]
  Multiple_value_comparison_allData=cbind(Multiple_value_comparison_allData,Freq=100*Multiple_value_comparison_allData$n/nrow(multi_allData))
  colnames(Multiple_value_comparison_allData)=c(val1_initial,val2_initial,"N","Freq")
  
  ####################################### Seperate Datasets
  one_run<-function(j){
    multi_datasets[[name[j]]]=data.frame(multi_datasets[[name[j]]],stringsAsFactors = F)
    
    Multiple_value_comparison_datasets[[name[j]]]<- multi_datasets[[name[j]]] %>% group_by(multi_datasets[[name[j]]][[val1]],multi_datasets[[name[j]]][[val2]]) %>% summarise(n=n())
    Multiple_value_comparison_datasets[[name[j]]]=Multiple_value_comparison_datasets[[name[j]]][order(-Multiple_value_comparison_datasets[[name[j]]]$n),]
    Multiple_value_comparison_datasets[[name[j]]]=cbind(Multiple_value_comparison_datasets[[name[j]]],Freq=100*Multiple_value_comparison_datasets[[name[j]]]$n/nrow(multi_datasets[[name[j]]]))
    colnames(Multiple_value_comparison_datasets[[name[j]]])=c(val1_initial,val2_initial,"N","Freq")
    
    Multiple_value_comparison_datasets[[name[j]]]=data.frame(Multiple_value_comparison_datasets[[name[j]]],stringsAsFactors = F)
    
    if (save_tables_individually){
      filename=paste0(output_folder,"/","Multiple_value_comparison_",str_replace(val1_initial,"%",""),"_",str_replace(val2_initial,"%",""),"_",name[j],".txt")
      write.table(Multiple_value_comparison_datasets[[name[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
    }
    
    return(Multiple_value_comparison_datasets[[name[j]]])  
  } 
  
  if (Sys.info()[1] == "Windows"){
    #cl <- makeCluster(num_of_cores)
    #Multiple_value_comparison_datasets=clusterApply(cl=cl,1:length(name),one_run)
    Multiple_value_comparison_datasets=lapply(1:length(name),one_run)
  }else{
    Multiple_value_comparison_datasets=mclapply(1:length(name),one_run,mc.cores = num_of_cores, mc.preschedule = TRUE)
  }
  
  names(Multiple_value_comparison_datasets)=name
  
  if (save_tables_individually){
    filename=paste0(output_folder,"/","Multiple_value_comparison_",str_replace(val1_initial,"%",""),"_",str_replace(val2_initial,"%",""),"_","All Data",".txt")
    write.table(Multiple_value_comparison_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
  }
  
  confirm<-paste0("Multiple_value_comparison ",val1_initial," - ",val2_initial, " run!")
  
  result=list("Multiple_value_comparison_allData"=Multiple_value_comparison_allData,"Multiple_value_comparison_datasets"=Multiple_value_comparison_datasets,"confirm"=confirm)
  
  # log time end and memory used 
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  cat(pryr::mem_used(), file=logFile, append=TRUE, sep = "\n")
  
  return(result)
  
}


######################################################################################################################################

Multiple_value_comparison_highly_similar<-function(clono_allData,clono_datasets,allele_clonotypes,gene_clonotypes,view_specific_clonotype_allData,view_specific_clonotype_datasets,val1,val2,name,identity_groups){
  #logfile
  cat(paste0("Multiple_value_comparison_highly_similar","\t"), file=logFile, append=TRUE)
  cat(paste0(paste(val1,val2,sep = ","),"\t"), file=logFile, append=TRUE)
  cat(paste0(nrow(clono_allData),"\t"), file=logFile, append=TRUE)
  cat(paste0(ncol(clono_allData),"\t"), file=logFile, append=TRUE)
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  
  val1_initial=val1
  val2_initial=val2
  
  val_initial=c(val1_initial,val2_initial)
  
  if (val1=="Molecular mass" || val1=="pI"){
    val1=paste0("Junction.",val1)
  }else{
    val1=paste0("Summary.",val1)
  }
  
  val1=gsub(" ",".",val1)
  val1=gsub("-",".",val1)
  val1=gsub("%",".",val1)
  
  if (val2=="Molecular mass" || val2=="pI"){
    val2=paste0("Junction.",val2)
  }else{
    val2=paste0("Summary.",val2)
  }
  
  val2=gsub(" ",".",val2)
  val2=gsub("-",".",val2)
  val2=gsub("%",".",val2)
  
  if (!str_detect(val1,"allele") && str_detect(val1,"GENE")){
    val1=paste0(val1,".and.allele")
  }
  
  if (!str_detect(val2,"allele") && str_detect(val2,"GENE")){
    val2=paste0(val2,".and.allele")
  }
  
  Multiple_value_comparison_datasets<-list()
  
  for (i in 1:nrow(clono_allData)){
    clono_allData[i,1]=strsplit(as.character(clono_allData[i,1])," - ")[[1]][1]
  }
  
  for (j in 1:length(name)){
    for (i in 1:nrow(clono_datasets[[name[j]]])){
      clono_datasets[[name[j]]][i,1]=strsplit(as.character(clono_datasets[[name[j]]][i,1])," - ")[[1]][1]   
    }
  }
  
  multi_allData<-c()
  
  val=c(val1,val2)
  
  multi_datasets<-list()
  
  for (vals in 1:2){
    if (str_detect(val[vals],"GENE")){
      gene=val[vals]
      
      if (gene==gene_clonotypes && (str_detect(val_initial[vals],"allele"))==allele_clonotypes && !(is.null(gene_clonotypes))){
        ####################################### All Data
        multi_allData=cbind(multi_allData,clono_allData[["clonotype"]])
        colnames(multi_allData)[ncol(multi_allData)]=gene
        
        ####################################### Seperate Datasets
        one_run<-function(j){
          multi_datasets[[name[j]]]=cbind(multi_datasets[[name[j]]],clono_datasets[[name[j]]][["clonotype"]])
          colnames(multi_datasets[[name[j]]])[ncol(multi_datasets[[name[j]]])]=gene
          
          return(multi_datasets[[name[j]]])  
        } 
        
        if (Sys.info()[1] == "Windows"){
          #cl <- makeCluster(num_of_cores)
          #Multiple_value_comparison_datasets=clusterApply(cl=cl,1:length(name),one_run)
          multi_datasets=lapply(1:length(name),one_run)
        }else{
          multi_datasets=mclapply(1:length(name),one_run,mc.cores = num_of_cores, mc.preschedule = TRUE)
        }
        
        names(multi_datasets)=name
      }else{
        #find the most frequent gene that exists in each specific clonotype
        ####################################### All Data
        freq_gene_name<-data.frame()
        id=0
        for (i in 1:nrow(clono_allData)){
          prev_clono=as.numeric(strsplit(as.character(clono_allData$prev_cluster[i])," ")[[1]][2:length(strsplit(as.character(clono_allData$prev_cluster[i])," ")[[1]])])
          a=view_specific_clonotype_allData[[prev_clono[1]]]
          if(length(prev_clono)>1){
            for (cl in 2:length(prev_clono))
              a=rbind(a,view_specific_clonotype_allData[[prev_clono[cl]]])
          }
          if ((str_detect(val_initial[vals],"allele")==F)){
            if (!all(!(str_detect(a[[gene]],"[*]")))){
              a2=strsplit(a[[gene]],"[*]") 
              a[[gene]]=as.character(plyr::ldply(a2,function(s){t(data.frame(unlist(s)))})[,1])
            }
          }
          freq_gene = a %>% group_by(a[[gene]]) %>% summarise(n=n())
          freq_gene <- freq_gene[order(-freq_gene$n),] 
          freq_gene_name[i,1]=freq_gene[1,1]
        }
        colnames(freq_gene_name)=gene
        
        multi_allData=cbind(multi_allData,freq_gene_name[[gene]])
        colnames(multi_allData)[ncol(multi_allData)]=gene 
        
        ####################################### Seperate Datasets
        one_run<-function(j){
          freq_gene_name<-data.frame()
          for (i in 1:nrow(clono_datasets[[name[j]]])){
            prev_clono=as.numeric(strsplit(as.character(clono_datasets[[name[j]]]$prev_cluster[i])," ")[[1]][2:length(strsplit(as.character(clono_datasets[[name[j]]]$prev_cluster[i])," ")[[1]])])
            prev_clono=prev_clono[!is.na(prev_clono)]
            a=view_specific_clonotype_datasets[[name[j]]][[prev_clono[1]]]
            if(length(prev_clono)>1){
              for (cl in 2:length(prev_clono))
                a=rbind(a,view_specific_clonotype_datasets[[name[j]]][[prev_clono[cl]]])
            }
            if ((str_detect(val_initial[vals],"allele")==F)){
              if (!all(!(str_detect(a[[gene]],"[*]")))){
                a2=strsplit(a[[gene]],"[*]") 
                a[[gene]]=as.character(plyr::ldply(a2,function(s){t(data.frame(unlist(s)))})[,1])
              }
            }
            freq_gene = a %>% group_by(a[[gene]]) %>% summarise(n=n())
            freq_gene <- freq_gene[order(-freq_gene$n),] 
            freq_gene_name[i,1]=freq_gene[1,1]
          }
          colnames(freq_gene_name)=gene
          
          multi_datasets[[name[j]]]=cbind(multi_datasets[[name[j]]],freq_gene_name[[gene]])
          colnames(multi_datasets[[name[j]]])[ncol(multi_datasets[[name[j]]])]=gene 
          
          return(multi_datasets[[name[j]]])  
        } 
        
        if (Sys.info()[1] == "Windows"){
          #cl <- makeCluster(num_of_cores)
          #Multiple_value_comparison_datasets=clusterApply(cl=cl,1:length(name),one_run)
          multi_datasets=lapply(1:length(name),one_run)
        }else{
          multi_datasets=mclapply(1:length(name),one_run,mc.cores = num_of_cores, mc.preschedule = TRUE)
        }
        
        names(multi_datasets)=name
      }
    }else{
      a=c()
      for (i in 1:nrow(clono_allData)){
        prev_clono=as.numeric(strsplit(as.character(clono_allData$prev_cluster[i])," ")[[1]][2:length(strsplit(as.character(clono_allData$prev_cluster[i])," ")[[1]])])
        prev_clono=prev_clono[!is.na(prev_clono)]
        view=view_specific_clonotype_allData[[prev_clono[1]]]
        if(length(prev_clono)>1){
          for (cl in 2:length(prev_clono))
            view=rbind(view,view_specific_clonotype_allData[[prev_clono[cl]]])
        }
        a=c(a,median(as.numeric(view[[val[vals]]]),na.rm = TRUE))
      }
      
      if ((!is.null(identity_groups)) && (val[vals]==used_columns[["Summary"]][4])){
        a=as.numeric(a)
        temp=a
        for (values in 1:nrow(identity_groups)){
          if (values==nrow(identity_groups))  index=which(a>=identity_groups[values,1] & a<=identity_groups[values,2])
          else index=which(a>=identity_groups[values,1] & a<identity_groups[values,2])
          temp[index]=identity_groups$label[values]
        }
        a=temp
      }
      
      multi_allData=cbind(multi_allData,a)
      colnames(multi_allData)[ncol(multi_allData)]=val[vals] 
      
      ####################################### Seperate Datasets
      one_run<-function(j){
        a=c()
        id=0
        for (i in 1:nrow(clono_datasets[[name[j]]])){
          prev_clono=as.numeric(strsplit(as.character(clono_datasets[[name[j]]]$prev_cluster[i])," ")[[1]][2:length(strsplit(as.character(clono_datasets[[name[j]]]$prev_cluster[i])," ")[[1]])])
          prev_clono=prev_clono[!is.na(prev_clono)]
          view=view_specific_clonotype_datasets[[name[j]]][[prev_clono[1]]]
          if(length(prev_clono)>1){
            for (cl in 2:length(prev_clono))
              view=rbind(view,view_specific_clonotype_datasets[[name[j]]][[prev_clono[cl]]])
          }
          a=c(a,median(as.numeric(view[[val[vals]]]),na.rm = TRUE))
        }
        
        if ((!is.null(identity_groups)) && (val[vals]==used_columns[["Summary"]][4])){
          a=as.numeric(a)
          temp=a
          for (values in 1:nrow(identity_groups)){
            if (values==nrow(identity_groups))  index=which(a>=identity_groups[values,1] & a<=identity_groups[values,2])
            else index=which(a>=identity_groups[values,1] & a<identity_groups[values,2])
            temp[index]=identity_groups$label[values]
          }
          a=temp
        }
        
        multi_datasets[[name[j]]]=cbind(multi_datasets[[name[j]]],a)
        colnames(multi_datasets[[name[j]]])[ncol(multi_datasets[[name[j]]])]=val[vals]
        return(multi_datasets[[name[j]]])  
      } 
      
      if (Sys.info()[1] == "Windows"){
        #cl <- makeCluster(num_of_cores)
        #Multiple_value_comparison_datasets=clusterApply(cl=cl,1:length(name),one_run)
        multi_datasets=lapply(1:length(name),one_run)
      }else{
        multi_datasets=mclapply(1:length(name),one_run,mc.cores = num_of_cores, mc.preschedule = TRUE)
      }
      
      names(multi_datasets)=name
    }
    
  }
  multi_allData=data.frame(multi_allData,stringsAsFactors = F)
  
  Multiple_value_comparison_allData<- multi_allData %>% group_by(multi_allData[[val1]],multi_allData[[val2]]) %>% summarise(n=n())
  Multiple_value_comparison_allData=Multiple_value_comparison_allData[order(-Multiple_value_comparison_allData$n),]
  Multiple_value_comparison_allData=cbind(Multiple_value_comparison_allData,Freq=100*Multiple_value_comparison_allData$n/nrow(multi_allData))
  colnames(Multiple_value_comparison_allData)=c(val1_initial,val2_initial,"N","Freq")
  
  ####################################### Seperate Datasets
  one_run<-function(j){
    multi_datasets[[name[j]]]=data.frame(multi_datasets[[name[j]]],stringsAsFactors = F)
    
    Multiple_value_comparison_datasets[[name[j]]]<- multi_datasets[[name[j]]] %>% group_by(multi_datasets[[name[j]]][[val1]],multi_datasets[[name[j]]][[val2]]) %>% summarise(n=n())
    Multiple_value_comparison_datasets[[name[j]]]=Multiple_value_comparison_datasets[[name[j]]][order(-Multiple_value_comparison_datasets[[name[j]]]$n),]
    Multiple_value_comparison_datasets[[name[j]]]=cbind(Multiple_value_comparison_datasets[[name[j]]],Freq=100*Multiple_value_comparison_datasets[[name[j]]]$n/nrow(multi_datasets[[name[j]]]))
    colnames(Multiple_value_comparison_datasets[[name[j]]])=c(val1_initial,val2_initial,"N","Freq")
    
    Multiple_value_comparison_datasets[[name[j]]]=data.frame(Multiple_value_comparison_datasets[[name[j]]],stringsAsFactors = F)
    
    if (save_tables_individually){
      filename=paste0(output_folder,"/","Multiple_value_comparison_highly_similar",str_replace(val1_initial,"%",""),"_",str_replace(val2_initial,"%",""),"_",name[j],".txt")
      write.table(Multiple_value_comparison_datasets[[name[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
    }
    
    return(Multiple_value_comparison_datasets[[name[j]]])  
  } 
  
  if (Sys.info()[1] == "Windows"){
    #cl <- makeCluster(num_of_cores)
    #Multiple_value_comparison_datasets=clusterApply(cl=cl,1:length(name),one_run)
    Multiple_value_comparison_datasets=lapply(1:length(name),one_run)
  }else{
    Multiple_value_comparison_datasets=mclapply(1:length(name),one_run,mc.cores = num_of_cores, mc.preschedule = TRUE)
  }
  
  names(Multiple_value_comparison_datasets)=name
  
  if (save_tables_individually){
    filename=paste0(output_folder,"/","Multiple_value_comparison_highly_similar",str_replace(val1_initial,"%",""),"_",str_replace(val2_initial,"%",""),"_","All Data",".txt")
    write.table(Multiple_value_comparison_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
  }
  
  confirm<-paste0("Multiple_value_comparison ",val1_initial," - ",val2_initial, " run!")
  
  result=list("Multiple_value_comparison_allData"=Multiple_value_comparison_allData,"Multiple_value_comparison_datasets"=Multiple_value_comparison_datasets,"confirm"=confirm)
  
  # log time end and memory used 
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  cat(pryr::mem_used(), file=logFile, append=TRUE, sep = "\n")
  
  return(result)
  
}


######################################################################################################################################

createFrequencyTableCDR3<- function(region_name,input,name,regionLength,FtopN,topClonotypesAlldata,topClonotypesDatasets,gene,junction,allele){
  #logfile
  cat(paste0("createFrequencyTableCDR3","\t"), file=logFile, append=TRUE)
  cat(paste0(paste(region_name,paste0("Top N: ",FtopN),sep = ","),"\t"), file=logFile, append=TRUE)
  cat(paste0(nrow(input),"\t"), file=logFile, append=TRUE)
  cat(paste0(ncol(input),"\t"), file=logFile, append=TRUE)
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  
  input_initial=input
  
  if (FtopN){
    #################################combine all conent of the top N clonotypes
    input_topN<-c()
    for (i in 1:nrow(topClonotypesAlldata)){
      cl=strsplit(topClonotypesAlldata$clonotype[i]," - ")
      if (is.na(cl[[1]][1])){
        cl[[1]][1]=""
      }
      if (is.na(cl[[1]][2])){
        cl[[1]][2]=""
      }
      input_topN=rbind(input_topN,viewClonotypes(input,allele,gene,junction,cl[[1]][1],cl[[1]][2]))
    }
    input=input_topN
  }
  
  if (region_name=="CDR3") region_name="CDR3.IMGT"
  region_name=paste0("IMGT.gapped.AA.sequences.",region_name)
  
  ###################################### all data #######################################
  #select AA juction or CDR3
  region<-unique(input[[region_name]])
  
  region_with_specific_length=input %>% filter(str_length(input[[region_name]])==regionLength)
  region_with_specific_length=region_with_specific_length[[region_name]]
  
  region_split<-strsplit(region,"")
  region<- as.data.frame(region)
  
  ancestral<- as.data.frame(c("I","L","V","A","M","C","T","S","H","K","R","E","D","P","G","Y","F","W","Q","N"))
  colnames(ancestral)<-"x"
  
  new_list<-list()
  k=0
  for (i in 1:length(region_split)){
    if (length(region_split[[i]])==regionLength){
      k=k+1
      new_list[[k]]<-region_split[[i]]
    }
  }
  
  region_new<-as.data.frame(new_list)
  region_new=data.frame(t(region_new))
  row.names(region_new)<-NULL
  
  if (length(region_new)>0){
    
    #remove factors
    region_new=region_new %>% mutate_if(is.factor, as.character)
    ancestral=ancestral %>% mutate_if(is.factor, as.character)
    a=as.data.frame(region_new %>% group_by(x=region_new[,1]) %>% summarise(n=n()))
    table_count<-join(as.data.frame(ancestral), a, type="left", by = "x")
    
    for (i in 2:ncol(region_new)){
      a=as.data.frame(region_new %>% group_by(x=region_new[,i]) %>% summarise(n=n()))
      table_count<-join(table_count, a, type="left", by = "x")
      
    }
    
    table_count[is.na(table_count)]<-0
    
    row.names(table_count)<-table_count[,1]
    #table_count<-table_count[,2:ncol(table_count)]
    
    #Create frequency table
    table_freq=table_count
    table_freq[,2:ncol(table_count)]<-100*table_count[,2:ncol(table_count)]/k
    
    colnames(table_freq)=c('AA',1:(ncol(table_freq)-1))
    colnames(table_count)=c('AA',1:(ncol(table_count)-1))
    
    if (region_name==paste0("IMGT.gapped.AA.sequences.","CDR3.IMGT")){
      if ((ncol(table_count)-1)==13) a=105:117
      else if ((ncol(table_count)-1)==12) a=c(105:110,112:117)
      else if ((ncol(table_count)-1)==11) a=c(105:110,113:117)
      else if ((ncol(table_count)-1)==10) a=c(105:109,113:117)
      else if ((ncol(table_count)-1)==9) a=c(105:109,114:117)
      else if ((ncol(table_count)-1)==8) a=c(105:108,114:117)
      else if ((ncol(table_count)-1)==7) a=c(105:108,115:117)
      else if ((ncol(table_count)-1)==6) a=c(105:107,115:117)
      else if ((ncol(table_count)-1)==5) a=c(105:107,116:117)
      
      colnames(table_count) <- c('AA',a)
      colnames(table_freq) <- c('AA',a)
    }
    
  }else{
    table_freq<-c()
    table_count<-c()
    print("no data for this")
  }
  
  ###################################### Separate datasets #######################################
  table_count_datasets<-list()
  table_freq_datasets<-list()
  region_with_specific_length_dataset<-list()
  
  for (j in 1:length(name)){
    namej=name[j]
    input_dataset=input_initial %>% filter(input_initial$dataName==name[j])
    
    region_with_specific_length_dataset[[name[j]]]=input_dataset %>% filter(str_length(input_dataset[[region_name]])==regionLength)
    region_with_specific_length_dataset[[name[j]]]=region_with_specific_length_dataset[[name[j]]][[region_name]]
    
    if (FtopN){
      #################################combine all conent of the top N clonotypes
      #thhe function should take allele,gene,junction as input!!!!!!!!!!!!
      
      input_topN<-c()
      for (i in 1:nrow(topClonotypesDatasets[[name[j]]])){
        cl=strsplit(topClonotypesDatasets[[name[j]]]$clonotype[i]," - ")
        if (is.na(cl[[1]][1])){
          cl[[1]][1]=""
        }
        if (is.na(cl[[1]][2])){
          cl[[1]][2]=""
        }
        input_topN=rbind(input_topN,viewClonotypes(input_dataset,allele,gene,junction,cl[[1]][1],cl[[1]][2]))
      }
      input_dataset=input_topN
    }
    region<-unique(input_dataset[[region_name]])
    region_split<-strsplit(region,"")
    region<- as.data.frame(region)
    
    ancestral<- as.data.frame(c("I","L","V","A","M","C","T","S","H","K","R","E","D","P","G","Y","F","W","Q","N"))
    colnames(ancestral)<-"x"
    
    #take the region with length=region
    new_list<-list()
    k=0
    for (i in 1:length(region_split)){
      if (length(region_split[[i]])==regionLength){
        k=k+1
        new_list[[k]]<-region_split[[i]]
      }
    }
    
    name[j]=namej
    
    region_new<-as.data.frame(new_list)
    region_new=data.frame(t(region_new))
    row.names(region_new)<-NULL
    
    if (length(region_new)>0){
      #remove factors
      region_new=region_new %>% mutate_if(is.factor, as.character)
      ancestral=ancestral %>% mutate_if(is.factor, as.character)
      a=as.data.frame(region_new %>% group_by(x=region_new[,1]) %>% summarise(n=n()))
      table_count_datasets[[name[j]]]=join(as.data.frame(ancestral), a, type="left", by = "x")
      
      for (i in 2:ncol(region_new)){
        a=as.data.frame(region_new %>% group_by(x=region_new[,i]) %>% summarise(n=n()))
        table_count_datasets[[name[j]]]=join(table_count_datasets[[name[j]]], a, type="left", by = "x")
        
      }
      
      table_count_datasets[[name[j]]][is.na(table_count_datasets[[name[j]]])]<-0
      row.names(table_count_datasets[[name[j]]])<-table_count_datasets[[name[j]]][,1]
      #table_count_datasets[[name[j]]]<-table_count_datasets[[name[j]]][,2:ncol(table_count_datasets[[name[j]]])]
      
      #Create frequency table
      table_freq_datasets[[name[j]]]<-table_count_datasets[[name[j]]]
      table_freq_datasets[[name[j]]][,2:ncol(table_count_datasets[[name[j]]])]<-100*table_count_datasets[[name[j]]][,2:ncol(table_count_datasets[[name[j]]])]/k
      
      colnames(table_count_datasets[[name[j]]])=c('AA',1:(ncol(table_count_datasets[[name[j]]])-1))
      colnames(table_freq_datasets[[name[j]]])=c('AA',1:(ncol(table_freq_datasets[[name[j]]])-1))
      
      if (region_name==paste0("IMGT.gapped.AA.sequences.","CDR3.IMGT")){
        if ((ncol(table_count_datasets[[name[j]]])-1)==13) a=105:117
        else if ((ncol(table_count_datasets[[name[j]]])-1)==12) a=c(105:110,112:117)
        else if ((ncol(table_count_datasets[[name[j]]])-1)==11) a=c(105:110,113:117)
        else if ((ncol(table_count_datasets[[name[j]]])-1)==10) a=c(105:109,113:117)
        else if ((ncol(table_count_datasets[[name[j]]])-1)==9) a=c(105:109,114:117)
        else if ((ncol(table_count_datasets[[name[j]]])-1)==8) a=c(105:108,114:117)
        else if ((ncol(table_count_datasets[[name[j]]])-1)==7) a=c(105:108,115:117)
        else if ((ncol(table_count_datasets[[name[j]]])-1)==6) a=c(105:107,115:117)
        else if ((ncol(table_count_datasets[[name[j]]])-1)==5) a=c(105:107,116:117)
        
        colnames(table_count_datasets[[name[j]]]) <- c('AA',a)
        colnames(table_freq_datasets[[name[j]]]) <- c('AA',a)
      }
      
      
    }else{
      table_freq_datasets[[name[j]]]<-c()
      table_count_datasets[[name[j]]]<-c()
    }
    
  } 
  
  confirm<-paste0("Frequency tables run!")
  
  result=list("region_with_specific_length_dataset"=region_with_specific_length_dataset,"region_with_specific_length"=region_with_specific_length,"table_count"=table_count,"table_freq"=table_freq,"table_count_datasets"=table_count_datasets,"table_freq_datasets"=table_freq_datasets,"confirm"=confirm)
  
  # log time end and memory used 
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  cat(pryr::mem_used(), file=logFile, append=TRUE, sep = "\n")
  
  return(result)
  
}


######################################################################################################################################

createLogo<-function(table_count,table_count_datasets,name){
  #logfile
  cat(paste0("createLogo","\t"), file=logFile, append=TRUE)
  cat(paste0("logo plot","\t"), file=logFile, append=TRUE)
  cat(paste0(nrow(table_count),"\t"), file=logFile, append=TRUE)
  cat(paste0(ncol(table_count),"\t"), file=logFile, append=TRUE)
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  
  #Create color matrix
  mat<-matrix(nrow=20, ncol=2)
  mat[1,]<-c("I", "#0000FF")
  mat[2,]<-c("L","#0000FF")
  mat[3,]<-c("V","#0000FF")
  mat[4,]<-c("A","#0000FF")
  
  mat[5,]<-c("M","#C6E2FF")
  mat[6,]<-c("C","#C6E2FF")
  
  mat[7,]<-c("T","#54FF9F")
  mat[8,]<-c("S","#54FF9F")
  
  mat[9,]<-c("H","#FF0000")
  mat[10,]<-c("K","#FF0000")
  mat[11,]<-c("R","#FF0000")
  
  mat[12,]<-c("E","#FFD700")
  mat[13,]<-c("D","#FFD700")
  
  mat[14,]<-c("P","#FFD700")
  
  mat[15,]<-c("G","#00EE00")
  
  mat[16,]<-c("Y","#C1FFC1")
  
  mat[17,]<-c("F","#1E90FF")
  
  mat[18,]<-c("W","#BA55D3")
  
  mat[19,]<-c("Q","#ED9121")
  mat[20,]<-c("N","#ED9121")
  
  ##########################
  
  if (!is.null(table_count)){
    p=pcm2pfm(table_count)
    color_set<-c()
    for (i in 1:length(rownames(p))){
      color=which(mat[,1]==rownames(p)[i])
      color_set=c(color_set,mat[color,2])
    }
    
    names(color_set)=row.names(p)
    #plotMotifLogo(p,p=rep(1/length(rownames(p)),length(rownames(p))),ic.scale=FALSE, colset = color_set, ylab="probability")
    
    motif_all <- new("pcm", mat=as.matrix(p), name="")
    motif_all$color<-color_set
    
    motif_datasets<-list()
    for (j in 1:length(name)){
      if (!is.null(table_count_datasets[[name[j]]])){
        if ("AA" %in% colnames(table_count_datasets[[name[j]]])){
          table_count_datasets[[name[j]]]=table_count_datasets[[name[j]]][,2:ncol(table_count_datasets[[name[j]]])]
        }
        p=pcm2pfm(table_count_datasets[[name[j]]])
        color_set<-c()
        for (i in 1:length(rownames(p))){
          color=which(mat[,1]==rownames(p)[i])
          color_set=c(color_set,mat[color,2])
        }
        names(color_set)=row.names(p)
        #plotMotifLogo(p,p=rep(1/length(rownames(p)),length(rownames(p))),ic.scale=FALSE, colset = color_set, ylab="probability")
        
        
        motif <- new("pcm", mat=as.matrix(p), name="")
        motif$color<-color_set
        
        motif_datasets[[name[j]]]=motif
      }else{
        motif_datasets[[name[j]]]=c()
      }
      
    }
  }else{
    motif_all=c()
    motif_datasets=c()
  }
  
  
  confirm="Logo run!"
  
  result=list("motif_all"=motif_all,"motif_datasets"=motif_datasets,"confirm"=confirm)
  
  # log time end and memory used 
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  cat(pryr::mem_used(), file=logFile, append=TRUE, sep = "\n")
  
  return(result)
  
  #write to png file
  #dev.print(png, file=paste0("K",j,"_H",j," AA.png"),width=1000, height=550)
  
  
}


######################################################################################################################################

alignment<-function(input,region,germline,name,only_one_germline,use_genes_germline,Tcell,AAorNtAlignment,clono_allData,clono_datasets,view_specific_clonotype_allData,view_specific_clonotype_datasets,topNClono,FtopN,thrClono,Fthr,highly){
  #logfile
  cat(paste0("alignment","\t"), file=logFile, append=TRUE)
  cat(paste0(paste(region,AAorNtAlignment,"top ",topNClono,"clonotypes",sep=","),"\t"), file=logFile, append=TRUE)
  cat(paste0(nrow(input),"\t"), file=logFile, append=TRUE)
  cat(paste0(ncol(input),"\t"), file=logFile, append=TRUE)
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  
  if (AAorNtAlignment=="aa") file="IMGT.gapped.AA.sequences."
  else file="IMGT.gapped.nt.sequences."
  
  if (region=="CDR3") region="JUNCTION"
  region=paste0(file,region)
  
  print(region)
  
  max_length_region=max(str_length(input[[region]]))
  
  if (Tcell==F && only_one_germline==F){
    type=strsplit(strsplit(as.character(input[[used_columns[["Summary"]][3]]][1])," ")[[1]][2],"V")[[1]][1]
    if ((type=="IGK") | (type=="IGL")){
      germline_file=paste0("param/","Germline sequences alignments ","IGK","V ",AAorNtAlignment,".csv")
      Tgermlines=read.csv(germline_file,sep=";",stringsAsFactors = F,colClasses = c("character"))
      if (AAorNtAlignment=="aa"){
        Tgermlines[,113:117]="."
      }else{
        Tgermlines[,336:351]="."
      }
      germline_file=paste0("param/","Germline sequences alignments ","IGL","V ",AAorNtAlignment,".csv")
      te=read.csv(germline_file,sep=";",stringsAsFactors = F,colClasses = c("character"))
      colnames(Tgermlines)=colnames(te)
      Tgermlines=rbind(Tgermlines,te)
    }else{
      germline_file=paste0("param/","Germline sequences alignments ",type,"V ",AAorNtAlignment,".csv")
      Tgermlines=read.csv(germline_file,sep=";",stringsAsFactors = F,colClasses = c("character"))
    }
    
    Tgermlines=unique(Tgermlines)
    colnames(Tgermlines)=c("V1",1:(ncol(Tgermlines)-1))
    
    a2=strsplit(Tgermlines$V1," or|,| [(]see| OR") 
    Tgermlines$V1=as.character(ldply(a2,function(s){t(data.frame(unlist(s)))})[,1])
    
    #max_length_region=ncol(Tgermlines)-1
    if (max_length_region>(ncol(Tgermlines)-1)){
      extra_dots=matrix(".",nrow(Tgermlines),max_length_region-ncol(Tgermlines)) 
      Tgermlines=cbind(Tgermlines,extra_dots)
      colnames(Tgermlines)=c("V1",1:(max_length_region-1))
      Tgermlines[Tgermlines==""]="."
      Tgermlines[is.na(Tgermlines)]="."
    }
  }
  
  if (only_one_germline){
    #max_length_region=length(strsplit(germline,"")[[1]])
  }
  
  if (region %in% colnames(input)){
    
    ############### Clonotypes ##############
    cluster_id<-c()
    freq_cluster_id<-c()
    if (length(view_specific_clonotype_allData)==0){
      cluster_id[1:nrow(input)]=0
      freq_cluster_id[1:nrow(input)]=0
    }else{
      if (!highly){
        for (i in 1:length(view_specific_clonotype_allData)){
          #index=as.numeric(row.names(view_specific_clonotype_allData[[names(view_specific_clonotype_allData)[i]]]))
          index=which(input[[used_columns[["Summary"]][1]]] %in% view_specific_clonotype_allData[[names(view_specific_clonotype_allData)[i]]][[used_columns[["Summary"]][1]]])
          if (index[1]>0) {
            freq_cluster_id[index]=clono_allData$Freq[i]
            cluster_id[index]=i
          }
        }
      }else{
        for (i in 1:nrow(clono_allData)){
          prev_clono=as.numeric(strsplit(as.character(clono_allData$prev_cluster[i])," ")[[1]][2:length(strsplit(as.character(clono_allData$prev_cluster[i])," ")[[1]])])
          view=view_specific_clonotype_allData[[prev_clono[1]]]
          if(length(prev_clono)>1){
            for (cl in 2:length(prev_clono))
              view=rbind(view,view_specific_clonotype_allData[[prev_clono[cl]]])
          }
          index=which(input[[used_columns[["Summary"]][1]]] %in% view[[used_columns[["Summary"]][1]]])
          if (index[1]>0) {
            freq_cluster_id[index]=clono_allData$Freq[i]
            cluster_id[index]=i
          }
        }
      }
      
      
    }
    
    #########################################
    region_split<-strsplit(input[[region]],"")
    
    for (i in 1:length(region_split)){
      if (length(region_split[[i]])>max_length_region)
        region_split[[i]]<-region_split[[i]][1:max_length_region]
      if (length(region_split[[i]])<max_length_region)
        region_split[[i]][length(region_split[[i]]):max_length_region]<-"."
      
    }
    
    region_split<- as.data.frame(region_split)
    region_split<-t(region_split)
    row.names(region_split)<-NULL
    
    #region_alignment<-cbind(as.data.frame(cluster_id),Functionality=input[[used_columns[["Summary"]][2]]])
    region_alignment<-cbind(as.data.frame(cluster_id),as.data.frame(freq_cluster_id),Functionality="productive")
    
    region_alignment<-cbind(region_alignment
                            ,J.GENE.and.allele=input[[used_columns[["Summary"]][8]]]
                            ,D.GENE.and.allele=input[[used_columns[["Summary"]][11]]]
                            ,V.GENE.and.allele=input[[used_columns[["Summary"]][3]]],region_split,stringsAsFactors = F)
    
    region_alignment$cluster_id=as.character(cluster_id)
    region_alignment$freq_cluster_id=as.character(freq_cluster_id)
    print(nrow(region_alignment))  
    if (FtopN){
      region_alignment=region_alignment %>% filter(as.numeric(as.character(region_alignment$cluster_id))<=topNClono | region_alignment$cluster_id=="-")
    }
    
    if (Fthr){
      region_alignment=region_alignment %>% filter(as.numeric(as.character(freq_cluster_id))>=thrClono | region_alignment$cluster_id=="-")
    }
    
    #region_alignment=region_alignment %>% filter(Functionality=="productive")
    #region_alignment$Functionality="productive"
    print(nrow(region_alignment))
    if (only_one_germline){
      germline<-strsplit(germline,"")[[1]]
      germline<-data.frame(t(germline),stringsAsFactors = F)
      germline=c("-","-","germline","-","-","-",germline)
      germline=as.data.frame(germline,stringsAsFactors=F)
      colnames(germline)=colnames(region_alignment[,1:ncol(germline)])
      alignment_with_germline<-rbind(germline,region_alignment[,1:ncol(germline)])
      #for (i in 3:length(alignment_with_germline)){
      #  alignment_with_germline[,i] = factor(alignment_with_germline[,i], levels=c(levels(alignment_with_germline[,i]), "-"))
      #}
      
      a= t(apply(alignment_with_germline[2:nrow(alignment_with_germline),3:length(alignment_with_germline)],1, function(x){x==alignment_with_germline[1,3:length(alignment_with_germline)] & x!="."} )) #x: a row of input[count,XColumns]
      temp=replace(alignment_with_germline[2:nrow(alignment_with_germline),3:length(alignment_with_germline)],a==TRUE,"-")
      #add the first and the second columns
      temp2=cbind(alignment_with_germline[2:nrow(alignment_with_germline),1:2],temp)
      #add the last columns
      print((length(alignment_with_germline)+1):length(region_alignment))
      if ((length(alignment_with_germline)+1)<length(region_alignment))
        temp2=rbind(temp2,region_alignment[,(length(alignment_with_germline)+1):length(region_alignment)])
      print(temp2)
      #add the germline (first row)
      germline_new=germline
      print(germline_new)
      #germline_new[,(length(germline)+1):length(region_alignment)]="."
      #print(germline_new)
      colnames(germline_new)<-colnames(temp2)
      output=rbind(germline_new[1,],temp2)
      print(output)
      
    }else{
      
      if (use_genes_germline){
        Tgermlines=Tgermlines%>%filter(str_detect(Tgermlines$V1,"[*]01 F"))
        for (i in 1:nrow(Tgermlines)){
          Tgermlines$V1[i]=strsplit(Tgermlines$V1,"[*]")[[i]][1]
        }
        
        region_alignment=region_alignment 
        
        for (i in 1:nrow(region_alignment)){
          region_alignment$V.GENE.and.allele[i]=strsplit(region_alignment$V.GENE.and.allele[i],"[*]")[[1]][1]
        }
      }
      
      if ((ncol(region_alignment)-ncol(Tgermlines)-5)>0){
        a=matrix(".",ncol=ncol(region_alignment)-ncol(Tgermlines)-5, nrow = nrow(Tgermlines))
        germlines=cbind("-",0,"germline","-","-",Tgermlines,a)
        colnames(germlines)=colnames(region_alignment)
        alignment_with_germline=rbind(germlines,region_alignment)  
      }else{
        germlines=cbind("-",0,"germline","-","-",Tgermlines)
        germlines=germlines[,1:ncol(region_alignment)]
        colnames(germlines)=colnames(region_alignment)
        
        alignment_with_germline=rbind(germlines,region_alignment)
      }
      
      df3=alignment_with_germline
      
      print(nrow(alignment_with_germline))
      germline<-c()
      output<-c()  
      a<-c()
      XColumns=1:(ncol(region_alignment)-6)
      XColumns=as.character(XColumns)
      b=by(alignment_with_germline, alignment_with_germline$V.GENE.and.allele,
           function(y){
             germline<<-which(y[,"Functionality"]=="germline")
             germline<<-germline[1]
             productive<<-which(y[,"Functionality"]=="productive")
             if (length(germline)>0 && length(productive)>0){
               a<-t(apply(y[productive,XColumns],1, function(x){x==y[germline,XColumns] & x!="."} )) #x: a row of input[count,XColumns]
               temp=replace(y[productive,XColumns],a==TRUE,"-")
               temp2=cbind(y[productive,colnames(alignment_with_germline[,1:6])],temp)
               output<<-rbind(output,y[germline,c(colnames(alignment_with_germline[,1:6]),XColumns)],temp2)
             }
           }
      )
      
    }
    alignment_allData=output %>% select(-c(Functionality))
    
    ################################ for Separate datasets ###############################
    alignment_datasets<-list()
    
    one_run<-function(j){
      input_tmp=input %>% filter(input$dataName==name[j])
      ############### Clonotypes ##############
      cluster_id<-c()
      freq_cluster_id<-c()
      if (length(view_specific_clonotype_allData)==0){
        cluster_id[1:nrow(input_tmp)]=0
        freq_cluster_id[1:nrow(input)]=0
      }else{
        if (!highly){
          for (i in 1:length(view_specific_clonotype_datasets[[name[j]]])){
            index=which(input_tmp[[used_columns[["Summary"]][1]]] %in% view_specific_clonotype_datasets[[name[j]]][[names(view_specific_clonotype_datasets[[name[j]]])[i]]][[used_columns[["Summary"]][1]]])
            #index=as.numeric(row.names(view_specific_clonotype_datasets[[name[j]]][[names(view_specific_clonotype_datasets[[name[j]]])[i]]]))
            if (index[1]>0) {
              cluster_id[index]=i
              freq_cluster_id[index]=clono_datasets[[name[j]]]$Freq[i]
            }
          }
        }else{
          for (i in 1:nrow(clono_datasets[[name[j]]])){
            prev_clono=as.numeric(strsplit(as.character(clono_datasets[[name[j]]]$prev_cluster[i])," ")[[1]][2:length(strsplit(as.character(clono_datasets[[name[j]]]$prev_cluster[i])," ")[[1]])])
            prev_clono=prev_clono[!is.na(prev_clono)]
            view=view_specific_clonotype_datasets[[name[j]]][[prev_clono[1]]]
            if(length(prev_clono)>1){
              for (cl in 2:length(prev_clono))
                view=rbind(view,view_specific_clonotype_datasets[[name[j]]][[prev_clono[cl]]])
            }
            index=which(input_tmp[[used_columns[["Summary"]][1]]] %in% view_specific_clonotype_datasets[[name[j]]][[names(view_specific_clonotype_datasets[[name[j]]])[i]]][[used_columns[["Summary"]][1]]])
            #index=as.numeric(row.names(view_specific_clonotype_datasets[[name[j]]][[names(view_specific_clonotype_datasets[[name[j]]])[i]]]))
            if (index[1]>0) {
              cluster_id[index]=i
              freq_cluster_id[index]=clono_datasets[[name[j]]]$Freq[i]
            }
          }
        }
        
      }
      
      #########################################
      region_split<-strsplit(input_tmp[[region]],"")
      for (i in 1:length(region_split)){
        if (length(region_split[[i]])>max_length_region)
          region_split[[i]]<-region_split[[i]][1:max_length_region]
        if (length(region_split[[i]])<max_length_region)
          region_split[[i]][length(region_split[[i]]):max_length_region]<-"."
        
      }  
      
      region_split<- as.data.frame(region_split)
      region_split<-t(region_split)
      row.names(region_split)<-NULL
      
      region_alignment<-cbind(as.data.frame(cluster_id),as.data.frame(freq_cluster_id),Functionality="productive")
      
      region_alignment<-cbind(region_alignment
                              ,J.GENE.and.allele=input_tmp[[used_columns[["Summary"]][8]]]
                              ,D.GENE.and.allele=input_tmp[[used_columns[["Summary"]][11]]]
                              ,V.GENE.and.allele=input_tmp[[used_columns[["Summary"]][3]]],region_split,stringsAsFactors = F)
      
      region_alignment$cluster_id=as.character(cluster_id)
      region_alignment$freq_cluster_id=as.character(freq_cluster_id)
      
      #region_alignment=region_alignment %>% filter(Functionality=="productive")
      
      
      if (FtopN){
        region_alignment=region_alignment %>% filter(as.numeric(as.character(region_alignment$cluster_id))<=topNClono | region_alignment$cluster_id=="-")
      }
      
      if (Fthr){
        region_alignment=region_alignment %>% filter(as.numeric(as.character(freq_cluster_id))>=thrClono | region_alignment$cluster_id=="-")
      }
      
      if (only_one_germline){
        print(germline)
        alignment_with_germline<-rbind(germline,region_alignment[,1:length(germline)])
        #for (i in 3:length(alignment_with_germline)){
        #  alignment_with_germline[,i] = factor(alignment_with_germline[,i], levels=c(levels(alignment_with_germline[,i]), "-"))
        #}
        
        a= t(apply(alignment_with_germline[2:nrow(alignment_with_germline),3:length(alignment_with_germline)],1, function(x){x==alignment_with_germline[1,3:length(alignment_with_germline)] & x!="."} )) #x: a row of input[count,XColumns]
        temp=replace(alignment_with_germline[2:nrow(alignment_with_germline),3:length(alignment_with_germline)],a==TRUE,"-")
        #add the first and the second columns
        temp2=cbind(alignment_with_germline[2:nrow(alignment_with_germline),1:2],temp)
        #add the last columns
        if ((length(alignment_with_germline)+1)<length(region_alignment))
          temp2=rbind(temp2,region_alignment[,(length(alignment_with_germline)+1):length(region_alignment)])
        #add the germline (first row)
        germline_new=germline
        #germline_new[,(length(germline)+1):length(region_alignment)]="."
        colnames(germline_new)<-colnames(temp2)
        output=rbind(germline_new[1,],temp2)
        
      }else{
        
        if (use_genes_germline){
          for (i in 1:nrow(region_alignment)){
            region_alignment$V.GENE.and.allele[i]=strsplit(region_alignment$V.GENE.and.allele[i],"[*]")[[1]][1]
          }
          
        }
        
        a=matrix(".",ncol=ncol(region_alignment)-ncol(Tgermlines)-5, nrow = nrow(Tgermlines))
        germlines=cbind("-",0,"germline","-","-",Tgermlines,a)
        colnames(germlines)=colnames(region_alignment)
        
        alignment_with_germline=rbind(germlines,region_alignment)
        
        germline<-c()
        output<-c()  
        a<-c()
        XColumns=1:(ncol(region_alignment)-6)
        XColumns=as.character(XColumns)
        b=by(alignment_with_germline, alignment_with_germline$V.GENE.and.allele,
             function(y){
               germline<<-which(y[,"Functionality"]=="germline")
               germline<<-germline[1]
               productive<<-which(y[,"Functionality"]=="productive")
               if (length(germline)>0 && length(productive)>0){
                 a<-t(apply(y[productive,XColumns],1, function(x){x==y[germline,XColumns] & x!="."} )) #x: a row of input[count,XColumns]
                 temp=replace(y[productive,XColumns],a==TRUE,"-")
                 temp2=cbind(y[productive,colnames(alignment_with_germline[,1:6])],temp)
                 output<<-rbind(output,y[germline,c(colnames(alignment_with_germline[,1:6]),XColumns)],temp2)
               }
             }
        )
        
        
      }
      
      alignment_datasets[[name[j]]]=output %>% select(-c(Functionality))
      
      if (save_tables_individually){
        filename=paste0(output_folder,"/","Alignment_",AAorNtAlignment,"_",name[j],".txt")
        write.table(alignment_datasets[[name[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
      }
      
      return(alignment_datasets[[name[j]]])  
    } 
    
    if (Sys.info()[1] == "Windows"){
      alignment_datasets=lapply(1:length(name),one_run)
    }else{
      alignment_datasets=mclapply(1:length(name),one_run,mc.cores = num_of_cores, mc.preschedule = TRUE)
    }
    
    names(alignment_datasets)=name
    
    if (save_tables_individually){
      filename=paste0(output_folder,"/","Alignment_",AAorNtAlignment,"_","All Data",".txt")
      write.table(alignment_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
    }
    
    confirm="Alignment run!"
    
    result=list("alignment_allData"=alignment_allData,"alignment_datasets"=alignment_datasets,"confirm"=confirm)
    
    # log time end and memory used 
    cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
    cat(pryr::mem_used(), file=logFile, append=TRUE, sep = "\n")
    
    return(result)
    
  }else return(0)
  
}


######################################################################################################################################

mutations<-function(align,align_datasets,thr,AAorNtMutations,name,topNClono,FtopN,FclonoSeperately,cl,Fthr,thrClono,FthrSep=T,thrSep){
  #logfile
  cat(paste0("mutations","\t"), file=logFile, append=TRUE)
  cat(paste0(paste("region",AAorNtMutations,sep=","),"\t"), file=logFile, append=TRUE)
  cat(paste0(nrow(align),"\t"), file=logFile, append=TRUE)
  cat(paste0(ncol(align),"\t"), file=logFile, append=TRUE)
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  
  if (FtopN){
    align=filter(align, cluster_id %in% c("-",as.character(1:topNClono)))
  }
  
  if (Fthr){
    align=align %>% filter(align$freq_cluster_id>=thrClono | align$cluster_id=="-")
  }
  
  if (FclonoSeperately){
    cl_id=paste0("_cluster_id_",cl)
    if (FthrSep){
      align=filter(align, cluster_id %in% c("-",as.character(cl)))
    }else{
      align=filter(align, cluster_id %in% c("-",as.character(cl)))
    }
  }else{
    cl_id=""
  }
  
  align[] <- lapply( align, factor) # the "[]" keeps the dataframe structure
  colnames(align)[4:ncol(align)]=paste0("X",colnames(align)[4:ncol(align)])
  
  
  #Find IMGT-region from position
  region_names<-c("FR1-IMGT","CDR1-IMGT","FR2-IMGT","CDR2-IMGT","FR3-IMGT","CDR3-IMGT")
  if (AAorNtMutations=="aa"){
    index_1<-c(1,27,39,56,66,105)
    index_2<-c(26,38,55,65,104,114)  
    start_g=85
    IMGT_groups=list(Acidic=c("E","D"),Basic=c("K","R","H"),Aromatic=c("F","W","Y"),
                     Non_polar_aliphatic=c("G","A","I","V","L","P","M"),Polar_non_charged=c("S","T","C","N","Q"))
  }else{
    index_1<-c(1,79,115,166,196,313)
    index_2<-c(78,114,165,195,312,342)
    start_g=260
    IMGT_groups=list(transitions=c("a>g","g>a","c>t","t>c"),transversions=c("a>c","c>a","g>t","t>g","c>g","g>c","a>t","t>a"))
  }
  
  #for each position find the number of sequences that do not contain "-"
  count<-0
  mutation_change_allData<-list()
  level_counts<-c()
  output<-c()
  b=by(align, align$V.GENE.and.allele,
       function(y){
         germline<<-which(y$cluster_id=="-")
         germline<<-germline[1]
         productive<<-which(y$cluster_id!="-")
         if (length(productive)>0 & length(germline)){
           mc=c()
           germ_length=ncol(y)-length(which(y[germline,start_g:ncol(align)]=="."))
           pos=which((colSums(y[productive,5:germ_length] != "-")/nrow(y))>thr) #till germline length
           
           for (i in pos){
             level_counts<<-plyr::count(y[productive,],paste0("X",i))
             letter_=which(level_counts[,1]=="-")
             letter_dot=which(level_counts[,1]==".")
             if (length(letter_)>0 & length(letter_dot)>0) index=which(as.numeric(row.names(level_counts))!=letter_ & as.numeric(row.names(level_counts))!=letter_dot)
             else if (length(letter_)>0 & length(letter_dot)==0) index=which(as.numeric(row.names(level_counts))!=letter_)
             else if (length(letter_)==0 & length(letter_dot)>0) index=which(as.numeric(row.names(level_counts))!=letter_dot)
             else if (length(letter_)==0 & length(letter_dot)==0) index=as.numeric(row.names(level_counts))
             
             if (length(index)>0){
               max_freq_id=which(level_counts[index,2]==max(level_counts[index,2]))
               new_row=cbind(level_counts[index,][max_freq_id,],pos=i,germline=y[[paste0("X",i)]][germline])
               colnames(new_row)[1]="X"
               if (new_row[,2][1]/nrow(y)>thr){
                 mc=rbind(mc,new_row)
               }
             }
             
           }
           
           mc$region=c()
           mc$germ_physico=c()
           mc$new_physico=c()
           mc$Change_in_physicochemical_properties=c()
           if (length(mc)>0){
             for (r in 1:length(region_names)){
               mc[which(mc$pos>=index_1[r] & mc$pos<=index_2[r]),5]=region_names[r]
             }
             
             if (AAorNtMutations=="aa"){
               for (r in names(IMGT_groups)){
                 mc[which(mc$germline %in% IMGT_groups[[r]]),6]=r
                 mc[which(mc$X %in% IMGT_groups[[r]]),7]=r
               }
               mc$Change_in_physicochemical_properties=paste0(mc$V6,"->",mc$V7)
             }else{
               for (r in names(IMGT_groups)){
                 mc[which(paste0(mc$germline,">",mc$X) %in% IMGT_groups[[r]]),6]=r
               }
               mc$Change_in_physicochemical_properties=mc$V6
             }
             
             
             mc=cbind(Gene=as.character(y$V.GENE.and.allele[germline]),Change_in_position=paste0(mc$germline,mc$pos,">",mc$X),Region=mc$V5,Change_in_physicochemical_properties=mc$Change_in_physicochemical_properties,N=mc$freq,Freq=100*mc$freq/(nrow(y)-1))
             output<<-rbind(output,mc)
           }
         }
         
       }
  )
  
  mutation_change_allData=output
  
  ################################ for Separate datasets ###############################
  mutation_change_datasets<-list()
  
  one_run<-function(j){
    if (FtopN)
      align_datasets[[name[j]]]=filter(align_datasets[[name[j]]], cluster_id %in% c("-",as.character(1:topNClono)))
    
    if (FclonoSeperately){
      align_datasets[[name[j]]]=filter(align_datasets[[name[j]]], cluster_id %in% c("-",as.character(cl)))
    }
    align_datasets[[name[j]]][] <- lapply( align_datasets[[name[j]]], factor)
    colnames(align_datasets[[name[j]]])[4:ncol(align_datasets[[name[j]]])]=paste0("X",colnames(align_datasets[[name[j]]])[4:ncol(align_datasets[[name[j]]])])
    
    #for each position find the number of sequences that do not contain "-"
    count<-0
    output<-c()
    mutation_change_temp<-list()
    b=by(align_datasets[[name[j]]], align_datasets[[name[j]]]$V.GENE.and.allele,
         function(y){
           germline<<-which(y[,"cluster_id"]=="-")
           germline<<-germline[1]
           productive<<-which(y[,"cluster_id"]!="-")
           if (length(productive)>0 & length(germline)){
             mc=c()
             germ_length=ncol(y)-length(which(y[germline,start_g:ncol(align_datasets[[name[j]]])]=="."))
             pos=which((colSums(y[productive,5:germ_length] != "-")/nrow(y))>thr) #till germline length
             
             for (i in pos){
               level_counts=plyr::count(y[productive,],paste0("X",i))
               letter_=which(level_counts[,1]=="-")
               letter_dot=which(level_counts[,1]==".")
               if (length(letter_)>0 & length(letter_dot)>0) index=which(as.numeric(row.names(level_counts))!=letter_ & as.numeric(row.names(level_counts))!=letter_dot)
               else if (length(letter_)>0 & length(letter_dot)==0) index=which(as.numeric(row.names(level_counts))!=letter_)
               else if (length(letter_)==0 & length(letter_dot)>0) index=which(as.numeric(row.names(level_counts))!=letter_dot)
               else if (length(letter_)==0 & length(letter_dot)==0) index=as.numeric(row.names(level_counts))
               
               if (length(index)>0){
                 max_freq_id=which(level_counts[index,2]==max(level_counts[index,2]))
                 new_row=cbind(level_counts[index,][max_freq_id,],pos=i,germline=y[[paste0("X",i)]][germline])
                 colnames(new_row)[1]="X"
                 if (new_row[,2][1]/nrow(y)>thr){
                   mc=rbind(mc,new_row)
                 }
               }
               
             }
             mc$region=c()
             mc$germ_physico=c()
             mc$new_physico=c()
             mc$Change_in_physicochemical_properties=c()
             if (length(mc)>0){
               for (r in 1:length(region_names)){
                 mc[which(mc$pos>=index_1[r] & mc$pos<=index_2[r]),5]=region_names[r]
               }
               
               if (AAorNtMutations=="aa"){
                 for (r in names(IMGT_groups)){
                   mc[which(mc$germline %in% IMGT_groups[[r]]),6]=r
                   mc[which(mc$X %in% IMGT_groups[[r]]),7]=r
                 }
                 mc$Change_in_physicochemical_properties=paste0(mc$V6,"->",mc$V7)
               }else{
                 for (r in names(IMGT_groups)){
                   mc[which(paste0(mc$germline,">",mc$X) %in% IMGT_groups[[r]]),6]=r
                 }
                 mc$Change_in_physicochemical_properties=mc$V6
               }
               
               mc=cbind(Gene=as.character(y$V.GENE.and.allele[germline]),Change_in_position=paste0(mc$germline,mc$pos,">",mc$X),Region=mc$V5,Change_in_physicochemical_properties=mc$Change_in_physicochemical_properties,N=mc$freq,Freq=100*mc$freq/(nrow(y)-1))
               output<<-rbind(output,mc)
             }
           }
           
         }
    )
    
    mutation_change_datasets[[name[j]]]<-output
    if (save_tables_individually){
      filename=paste0(output_folder,"/","Mutations_thr",thr, "_",AAorNtMutations,"_",name[j],cl_id,".txt")
      write.table(mutation_change_datasets[[name[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
    }
    
    return(mutation_change_datasets[[name[j]]])  
  } 
  
  if (Sys.info()[1] == "Windows"){
    mutation_change_datasets=lapply(1:length(name),one_run)
  }else{
    mutation_change_datasets=mclapply(1:length(name),one_run,mc.cores = num_of_cores, mc.preschedule = TRUE)
  }
  
  names(mutation_change_datasets)=name
  
  if (save_tables_individually){
    filename=paste0(output_folder,"/","Mutations_thr",thr, "_",AAorNtMutations,"_","All Data",cl_id,".txt")
    write.table(mutation_change_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
  }
  
  confirm="Mutations run!"
  
  result=list("mutation_change_allData"=mutation_change_allData,"mutation_change_datasets"=mutation_change_datasets,"confirm"=confirm)
  
  # log time end and memory used 
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  cat(pryr::mem_used(), file=logFile, append=TRUE, sep = "\n")
  
  return(result)
  
}


######################################################################################################################################

#input: the alignment file
groupedAlignment<-function(alignment_allData,alignment_datasets,name,AAorNtAlignment){
  #logfile
  cat(paste0("groupedAlignment","\t"), file=logFile, append=TRUE)
  cat(paste0("grouping","\t"), file=logFile, append=TRUE)
  cat(paste0(nrow(alignment_allData),"\t"), file=logFile, append=TRUE)
  cat(paste0(ncol(alignment_allData),"\t"), file=logFile, append=TRUE)
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  
  input<- data.table(alignment_allData)
  
  XColumns=1:(ncol(alignment_allData)-6)
  XColumns=as.character(XColumns)
  XColumns=c("V.GENE.and.allele","cluster_id","freq_cluster_id",XColumns)
  
  distinct_changes=input[,list(Freq =.N), by=XColumns]
  grouped_alignment_allData=cbind(N=distinct_changes$Freq,distinct_changes[,1:(ncol(distinct_changes)-1)])
  
  grouped_alignment_datasets<-list()
  one_run<-function(j){
    input<- data.table(alignment_datasets[[name[j]]])
    distinct_changes=input[,list(Freq =.N), by=XColumns]
    grouped_alignment_datasets[[name[j]]]=cbind(N=distinct_changes$Freq,distinct_changes[,1:(ncol(distinct_changes)-1)])
    if (save_tables_individually){
      filename=paste0(output_folder,"/","Grouped Alignment_",AAorNtAlignment,"_","All Data",".txt")
      write.table(grouped_alignment_datasets[[name[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
    }
    if (save_tables_individually){
      filename=paste0(output_folder,"/","Grouped Alignment_",AAorNtAlignment,"_",name[j],".txt")
      write.table(grouped_alignment_datasets[[name[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
    }
    return(grouped_alignment_datasets[[name[j]]])  
  } 
  
  if (Sys.info()[1] == "Windows"){
    grouped_alignment_datasets=lapply(1:length(name),one_run)
  }else{
    grouped_alignment_datasets=mclapply(1:length(name),one_run,mc.cores = num_of_cores, mc.preschedule = TRUE)
  }
  
  names(grouped_alignment_datasets)=name
  
  if (save_tables_individually){
    filename=paste0(output_folder,"/","Grouped Alignment_",AAorNtAlignment,"_","All Data",".txt")
    write.table(grouped_alignment_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
  }
  
  confirm="Grouped Alignment run!"
  
  result=list("grouped_alignment_allData"=grouped_alignment_allData,"grouped_alignment_datasets"=grouped_alignment_datasets,"confirm"=confirm)
  
  # log time end and memory used 
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  cat(pryr::mem_used(), file=logFile, append=TRUE, sep = "\n")
  
  return(result)
}


######################################################################################################################################

find_cdr3_diff1P<-function(allData,max_length_cdr3,position,name){
  #logfile
  cat(paste0("find_cdr3_diff1P","\t"), file=logFile, append=TRUE)
  cat(paste0("max length ",max_length_cdr3,",","Position ",position,"\t"), file=logFile, append=TRUE)
  cat(paste0(nrow(allData),"\t"), file=logFile, append=TRUE)
  cat(paste0(ncol(allData),"\t"), file=logFile, append=TRUE)
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  
  ################## Clonotypes ################
  gene=used_columns[["Summary"]][3]
  junction=used_columns[["Summary"]][18]
  
  
  unique_genes=unique(allData[[gene]])
  cdr3_diff1P_allData=c()
  
  for (i in 1:length(unique_genes)){
    gene_name=unique_genes[i]
    allData_temp=allData %>% filter(allData[[gene]]==gene_name)
    
    distinctVGenes_CDR3=allData_temp %>% group_by(JUNCTION=allData_temp[[junction]]) %>% summarise(Freq=n())
    distinctVGenes_CDR3<-cbind(distinctVGenes_CDR3, cluster_id=row.names(distinctVGenes_CDR3))
    
    clono_datasets<-list()
    for (j in 1:length(name)){
      data=allData_temp %>% filter(allData_temp$dataName==name[j])
      clono_datasets[[name[j]]]=data %>% group_by(JUNCTION=data[[junction]]) %>% summarise(Freq=n())
      clono_datasets[[name[j]]]=cbind(clono_datasets[[name[j]]], cluster_id=row.names(clono_datasets[[name[j]]]))
    }
    
    ################## Find the CDR3 that have difference only at the positions 8 and 9 with P ###############
    sub_groups<-c()
    sub_groups2<-c()
    
    used_rows=c()
    cdr3_split<- strsplit(distinctVGenes_CDR3$JUNCTION,"")
    cdr3_split_new<-c()
    k=0
    for (j in 1:length(cdr3_split)){
      if (length(cdr3_split[[j]])==max_length_cdr3){
        k=k+1
        used_rows=c(used_rows,j)
        cdr3_split_new[[k]]=cdr3_split[[j]]
      }
      if (length(cdr3_split[[j]])==(max_length_cdr3-1)){
        #shift right from the position 8 till the end
        k=k+1
        cdr3_split[[j]][(position+1):max_length_cdr3]<-cdr3_split[[j]][position:(max_length_cdr3-1)]
        cdr3_split_new[[k]]=cdr3_split[[j]]
        used_rows=c(used_rows,j)
      }
      
      
      distinctVGenes_CDR3=distinctVGenes_CDR3[used_rows,]
      
    }
    
    cdr3_split_new<- as.data.frame(cdr3_split_new)
    
    cdr3_split_new<-as.data.frame(t(cdr3_split_new),stringsAsFactors=FALSE)
    row.names(cdr3_split_new)<-NULL
    
    if (nrow(cdr3_split_new)>0){
      #Find distinct cdr3_split_new
      if (nrow(distinct(cdr3_split_new))<nrow(cdr3_split_new)){
        cdr3_split_new=data.table(cdr3_split_new)
        VColumns=colnames(cdr3_split_new)
        temp=cdr3_split_new[,list(Freq_sub_cluster =.N), by=VColumns]
        
        v_cdr3_cluster_id<-c()
        for (j in 1:nrow(distinctVGenes_CDR3)){
          v_cdr3_cluster_id[j]=which((do.call(paste0,cdr3_split_new[j,])==do.call(paste0,temp[,1:max_length_cdr3])))
        }
        
        multiple_objects_id<-c()
        for (j in 1:length(v_cdr3_cluster_id)){
          if (length(which(v_cdr3_cluster_id==v_cdr3_cluster_id[j]))>1) multiple_objects_id<-c(multiple_objects_id,j)
        }
        
        
        #Combine to one data frame
        cdr3_split_cluster_id<-cbind.data.frame(cdr3_split_new,v_cdr3_cluster_id,stringsAsFactors=FALSE)
        result<-cbind(cluster_id=distinctVGenes_CDR3$cluster_id[multiple_objects_id],JUNCTION=distinctVGenes_CDR3$JUNCTION[multiple_objects_id],Freq=distinctVGenes_CDR3$Freq[multiple_objects_id])
        result=as.data.frame(result,stringsAsFactors=FALSE)
        a=distinctVGenes_CDR3[multiple_objects_id,]
        a=cbind(gene_name,a)
        result=a[order(a[,'JUNCTION']), ]
        
        cdr3_diff1P_allData=rbind(cdr3_diff1P_allData,result)
      }
    }
    
  }
  
  ################################### Separate Datasets ######################################
  cdr3_diff1P_datasets<-list()
  for (n in 1:length(name)){
    data_dataset=allData %>% filter(allData$dataName==name[n])
    unique_genes=unique(data_dataset[[gene]])
    cdr3_diff1P_datasets[[name[n]]]=c()
    
    for (i in 1:length(unique_genes)){
      gene_name=unique_genes[i]
      allData_temp=data_dataset %>% filter(data_dataset[[gene]]==gene_name)
      
      distinctVGenes_CDR3=allData_temp %>% group_by(JUNCTION=allData_temp[[junction]]) %>% summarise(Freq=n())
      distinctVGenes_CDR3<-cbind(distinctVGenes_CDR3, cluster_id=row.names(distinctVGenes_CDR3))
      
      clono_datasets<-list()
      for (j in 1:length(name)){
        data=allData_temp %>% filter(allData_temp$dataName==name[j])
        clono_datasets[[name[j]]]=data %>% group_by(JUNCTION=data[[junction]]) %>% summarise(Freq=n())
        clono_datasets[[name[j]]]=cbind(clono_datasets[[name[j]]], cluster_id=row.names(clono_datasets[[name[j]]]))
      }
      
      ################## Find the CDR3 that have difference only at the positions 8 and 9 with P ###############
      sub_groups<-c()
      sub_groups2<-c()
      
      used_rows=c()
      cdr3_split<- strsplit(distinctVGenes_CDR3$JUNCTION,"")
      cdr3_split_new<-c()
      k=0
      for (j in 1:length(cdr3_split)){
        if (length(cdr3_split[[j]])==max_length_cdr3){
          k=k+1
          used_rows=c(used_rows,j)
          cdr3_split_new[[k]]=cdr3_split[[j]]
        }
        if (length(cdr3_split[[j]])==(max_length_cdr3-1)){
          #shift right from the position 8 till the end
          k=k+1
          cdr3_split[[j]][(position+1):max_length_cdr3]<-cdr3_split[[j]][position:(max_length_cdr3-1)]
          cdr3_split_new[[k]]=cdr3_split[[j]]
          used_rows=c(used_rows,j)
        }
        
        
        distinctVGenes_CDR3=distinctVGenes_CDR3[used_rows,]
        
      }
      
      cdr3_split_new<- as.data.frame(cdr3_split_new)
      
      cdr3_split_new<-as.data.frame(t(cdr3_split_new),stringsAsFactors=FALSE)
      row.names(cdr3_split_new)<-NULL
      
      if (nrow(cdr3_split_new)>0){
        #Find distinct cdr3_split_new
        if (nrow(distinct(cdr3_split_new))<nrow(cdr3_split_new)){
          cdr3_split_new=data.table(cdr3_split_new)
          VColumns=colnames(cdr3_split_new)
          temp=cdr3_split_new[,list(Freq_sub_cluster =.N), by=VColumns]
          
          v_cdr3_cluster_id<-c()
          for (j in 1:nrow(distinctVGenes_CDR3)){
            v_cdr3_cluster_id[j]=which((do.call(paste0,cdr3_split_new[j,])==do.call(paste0,temp[,1:max_length_cdr3])))
          }
          
          multiple_objects_id<-c()
          for (j in 1:length(v_cdr3_cluster_id)){
            if (length(which(v_cdr3_cluster_id==v_cdr3_cluster_id[j]))>1) multiple_objects_id<-c(multiple_objects_id,j)
          }
          
          
          #Combine to one data frame
          cdr3_split_cluster_id<-cbind.data.frame(cdr3_split_new,v_cdr3_cluster_id,stringsAsFactors=FALSE)
          result<-cbind(cluster_id=distinctVGenes_CDR3$cluster_id[multiple_objects_id],JUNCTION=distinctVGenes_CDR3$JUNCTION[multiple_objects_id],Freq=distinctVGenes_CDR3$Freq[multiple_objects_id])
          result=as.data.frame(result,stringsAsFactors=FALSE)
          a=distinctVGenes_CDR3[multiple_objects_id,]
          a=cbind(gene_name,a)
          result=a[order(a[,'JUNCTION']), ]
          
          cdr3_diff1P_datasets[[name[n]]]=rbind(cdr3_diff1P_datasets[[name[n]]],result)
        }
      }
      
    }
  }
  
  confirm="CDR3 1 diff length run!"
  
  result=list("cdr3_diff1P_allData"=cdr3_diff1P_allData,"cdr3_diff1P_datasets"=cdr3_diff1P_datasets,"confirm"=confirm)
  
  # log time end and memory used 
  cat(paste0(Sys.time(),"\t"), file=logFile, append=TRUE)
  cat(pryr::mem_used(), file=logFile, append=TRUE, sep = "\n")
  
  return(result)
  
}


######################################################################################################################################

addRepertoryFct <- function(id,btn) {
  insertUI(
    selector = "#placeholderRepertories",
    ui = tags$div(
      selectInput(btn, "Select type:",c("V Gene", "V Gene and allele",
                                        "J Gene", "J Gene and allele",
                                        "D Gene", "D Gene and allele"), width="170px"),
      id=id
    )
  )
}

######################################################################################################################################

addMultipleValues <- function(id, btn, columns_for_Multiple_value_comparison,default_val1 = NULL,default_val2 = NULL) {
  insertUI(
    selector = '#placeholder',
    ## wrap element in a div with id for ease of removal
    ui = tags$div(
      #forloop for columns
      div(style="display:inline-block",selectInput(paste0("select_MultipleValues_column1_",btn), "Select 1st column:",columns_for_Multiple_value_comparison, selected=default_val1, width="170px")),
      div(style="display:inline-block",selectInput(paste0("select_MultipleValues_column2_",btn), "Select 2nd column:",columns_for_Multiple_value_comparison, selected=default_val2, width="170px")),
      id=id 
    )
  )
}

######################################################################################################################################
# Set up a button to have an animated loading indicator and a checkmark
# for better user experience
# Need to use with the corresponding `withBusyIndicator` server function
withBusyIndicatorUI <- function(button) {
  id <- button[['attribs']][['id']]
  div(
    `data-for-btn` = id,
    button,
    span(
      class = "btn-loading-container",
      hidden(
        img(src = "ajax-loader-bar.gif", class = "btn-loading-indicator"),
        icon("check", class = "btn-done-indicator")
      )
    ),
    hidden(
      div(class = "btn-err",
          div(icon("exclamation-circle"),
              tags$b("Error: "),
              span(class = "btn-err-msg")
          )
      )
    )
  )
}

######################################################################################################################################
# Call this function from the server with the button id that is clicked and the
# expression to run when the button is clicked
withBusyIndicatorServer <- function(buttonId, expr) {
  # UX stuff: show the "busy" message, hide the other messages, disable the button
  loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  shinyjs::disable(buttonId)
  shinyjs::show(selector = loadingEl)
  shinyjs::hide(selector = doneEl)
  shinyjs::hide(selector = errEl)
  on.exit({
    shinyjs::enable(buttonId)
    shinyjs::hide(selector = loadingEl)
  })
  
  # Try to run the code when the button is clicked and show an error message if
  # an error occurs or a success message if it completes
  tryCatch({
    value <- expr
    shinyjs::show(selector = doneEl)
    shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade",
                                       time = 0.5))
    value
  }, error = function(err){ errorFunc(err, buttonId) })
}

# When an error happens after a button click, show the error
errorFunc <- function(err, buttonId) {
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
  errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
  shinyjs::html(html = errMessage, selector = errElMsg)
  shinyjs::show(selector = errEl, anim = TRUE, animType = "fade")
}

appCSS <- "
.btn-loading-container {
margin-left: 10px;
font-size: 1.2em;
}
.btn-done-indicator {
color: green;
}
.btn-err {
margin-top: 10px;
color: red;
}
"

