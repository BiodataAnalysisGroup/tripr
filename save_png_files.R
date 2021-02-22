clonotypes_barplot_select_range <- F
clonotypes_barchart_threshold <- 0.1
clonotypes_barchart_down_threshold <- 0.1
clonotypes_barchart_up_threshold <- 1


folder_name=paste("Analysis",trunc(as.numeric(Sys.time())))
if(!file.exists(paste0(tmp_path,"/",folder_name))){ #check if the directory has been made yet, I use the time/date at which the action button was pressed to make it relatively unique
  dir.create(paste0(tmp_path,"/",folder_name))}#make the dir if not
in.path=paste0(tmp_path,"/",folder_name) #go into the dir, alternatively you could just set the path of the file each time

#check if the following have run

####### clonotype plots  #######
if (clono$confirm!=""){
  if (clonotypes_barplot_select_range){
    parameters=paste0("from cluster",clonotypes_barchart_down_threshold,"to cluster",clonotypes_barchart_up_threshold)
  }else{
    parameters=paste0("with_threshold ",clonotypes_barchart_threshold)
  }
  
  if (clonotypes_barplot_select_range==F){
    #Find the clonotypes that we want to draw for all the datasets
    cl<-c()
    a<-list()
    if (is.null(clonotypes_barchart_threshold)) thr=0 else thr=clonotypes_barchart_threshold
    a[["allData"]]=clono$clono_allData %>% filter(clono$clono_allData$Freq>thr)
    cl=c(cl,a[["allData"]]$clonotype)
    for (i in loaded_datasets){
      a[[i]]=clono$clono_datasets[[i]] %>% filter(clono$clono_datasets[[i]]$Freq>thr)
      cl<-c(cl,a[[i]]$clonotype)
    }
    
  }else{
    #Find the clonotypes that we want to draw for all the datasets
    range=input$clonotypes_barchart_down_threshold:input$clonotypes_barchart_up_threshold
    cl<-c()
    a<-list()
    a[["allData"]]=clono$clono_allData[range,]
    cl=c(cl,a[["allData"]]$clonotype)
    for (i in loaded_datasets){
      a[[i]]=clono$clono_datasets[[i]][range,]
      cl<-c(cl,a[[i]]$clonotype)
    }
  }
  
  #Unique clonotypes
  cl=unique(cl)
  cl<<-c(cl,"Other")
  
  #Create a freqeuncy matrix
  data=c("allData",loaded_datasets)
  freq_mat=matrix(0,length(cl),(length(loaded_datasets)+1))
  ki=0
  for (i in 1:length(cl)){
    for (j in 1:length(data)){
      if (i==length(cl)) freq_mat[i,j]=100-sum(freq_mat[1:(i-1),j])
      else{
        if (length(which(a[[data[j]]]$clonotype==cl[i]))>0){
          freq_mat[i,j]=a[[data[j]]]$Freq[which(a[[data[j]]]$clonotype==cl[i])]
        } 
      }
    }
    
  }
  
  colnames(freq_mat) <- data
  rownames(freq_mat) <- cl 
  freq_mat<<-freq_mat
  freq_mat<<-round(freq_mat,2)
  
  png(paste0(in.path,"/","clonotypes_bar_plot_",parameters,".png"),width=3000, height=1550)
  barplot(
    freq_mat,
    xlim=c(0, ncol(freq_mat) + 5),
    col=brewer.pal(nrow(freq_mat), "Paired"),
    legend.text=TRUE,
    args.legend=list(
      x=ncol(freq_mat) + 5,
      y=max(colSums(freq_mat)),
      bty = "n"
    )
  )
  #barplot(freq_mat, col=rainbow(nrow(freq_mat)),names.arg=c("All Data",loaded_datasets), width=2) 
  #legend("topright", fill=rainbow(nrow(freq_mat)), legend=cl,cex = 0.6)
  dev.off()
}

####### Highly Similar clonotype plots  #######
higly_sim_clonotypes_barplot_select_range <- F
higly_sim_clonotypes_barchart_up_threshold <- 1
higly_sim_clonotypes_barchart_up_threshold <- 0.1
higly_sim_clonotypes_barchart_threshold <- 0.1
if (highly_similar_clonotypes_results$confirm!=""){
  if (higly_sim_clonotypes_barplot_select_range){
    parameters=paste0("from cluster",higly_sim_clonotypes_barchart_down_threshold,"to cluster",higly_sim_clonotypes_barchart_up_threshold)
  }else{
    parameters=paste0("with_threshold ",higly_sim_clonotypes_barchart_threshold)
  }
  
  if (higly_sim_clonotypes_barplot_select_range==F){
    #Find the clonotypes that we want to draw for all the datasets
    cl<-c()
    a<-list()
    if (is.null(higly_sim_clonotypes_barchart_threshold)) thr=0 else thr=higly_sim_clonotypes_barchart_threshold
    a[["allData"]]=highly_sim %>% filter(highly_sim$Freq>thr)
    cl=c(cl,a[["allData"]]$clonotype)
    for (i in loaded_datasets){
      a[[i]]=highly_sim_datasets[[i]] %>% filter(highly_sim_datasets[[i]]$Freq>thr)
      cl<-c(cl,a[[i]]$clonotype)
    }
    
  }else{
    #Find the clonotypes that we want to draw for all the datasets
    range=higly_sim_clonotypes_barchart_down_threshold:higly_sim_clonotypes_barchart_up_threshold
    cl<-c()
    a<-list()
    a[["allData"]]=highly_sim[range,]
    cl=c(cl,a[["allData"]]$clonotype)
    for (i in loaded_datasets){
      a[[i]]=highly_sim_datasets[[i]][range,]
      cl<-c(cl,a[[i]]$clonotype)
    }
  }
  
  #Unique clonotypes
  cl=unique(cl)
  cl<<-c(cl,"Other")
  
  #Create a freqeuncy matrix
  data=c("allData",loaded_datasets)
  freq_mat=matrix(0,length(cl),(length(loaded_datasets)+1))
  ki=0
  for (i in 1:length(cl)){
    for (j in 1:length(data)){
      if (i==length(cl)) freq_mat[i,j]=100-sum(freq_mat[1:(i-1),j])
      else{
        if (length(which(a[[data[j]]]$clonotype==cl[i]))>0){
          freq_mat[i,j]=a[[data[j]]]$Freq[which(a[[data[j]]]$clonotype==cl[i])]
        } 
      }
    }
    
  }
  
  colnames(freq_mat) <- data
  rownames(freq_mat) <- cl 
  freq_mat<<-freq_mat
  freq_mat<<-round(freq_mat,2)
  
  png(paste0(in.path,"/","Highly_sim_clonotypes_bar_plot_",parameters,".png"),width=3000, height=1550)
  barplot(
    freq_mat,
    xlim=c(0, ncol(freq_mat) + 5),
    col=brewer.pal(nrow(freq_mat), "Paired"),
    legend.text=TRUE,
    args.legend=list(
      x=ncol(freq_mat) + 5,
      y=max(colSums(freq_mat)),
      bty = "n"
    )
  )
  #barplot(freq_mat, col=rainbow(nrow(freq_mat)),names.arg=c("All Data",loaded_datasets), width=2) 
  #legend("topright", fill=rainbow(nrow(freq_mat)), legend=cl,cex = 0.6)
  dev.off()
}

####### Repertoires #######
repertories_pies_threshold <- NULL
if (!(is.null(repertories_results))){####### reperoires plots
  if (repertories_results[[1]]$confirm!=""){
    if (is.null(repertories_pies_threshold)) thr=0 else thr=repertories_pies_threshold
    
    for (k in 1:length(insertedRepertoires)){
      for (j in 1:(length(loaded_datasets)+1)){
        if (j==(length(loaded_datasets)+1)){
          
          #Genes that have percentage<threshold are grouped into one cell
          data=repertories_results[[k]]$Repertoires_allData
          data_filterIn=data %>% filter(data$Freq>thr)
          data_filterOut=data %>% filter(data$Freq<=thr)
          data=data_filterIn
          data[(nrow(data)+1),]=c("Other genes",sum(data_filterOut$N),sum(data_filterOut$Freq))
          #plot
          f=paste0(in.path,"/","Repertoires_pies",input[[paste0("selectRepertoires_",insertedRepertoires[k])]],"_","All Data",".png")
          
          png(f,width=900, height=600)
          pie(as.numeric(data$N), labels = round(as.numeric(data$Freq),2), main = paste0(strsplit(strsplit(as.character(imgtfilter_results$allData[[used_columns[["Summary"]][3]]][1])," ")[[1]][2],"V")[[1]][1],input[[paste0("selectRepertoires_",insertedRepertoires[k])]]," ", input$RepertoiresDataset),col = rainbow(length(data$N)))
          legend("topright", data$Gene, cex = 0.8,
                 fill = rainbow(length(data$N)))
          dev.off()
          
        }else{
          #Genes that have percentage<threshold are grouped into one cell
          data=repertories_results[[k]]$Repertoires_datasets[[loaded_datasets[j]]]
          data_filterIn=data %>% filter(data$Freq>thr)
          data_filterOut=data %>% filter(data$Freq<=thr)
          data=data_filterIn
          data[(nrow(data)+1),]=c("Other genes",sum(data_filterOut$N),sum(data_filterOut$Freq))
          
          #plot
          f=paste0(in.path,"/","Repertoires_pies",input[[paste0("selectRepertoires_",insertedRepertoires[k])]],"_",loaded_datasets[j],".png")
          
          png(f,width=900, height=600)
          pie(as.numeric(data$N), labels = round(as.numeric(data$Freq),2), main = paste0(strsplit(strsplit(as.character(imgtfilter_results$allData[[used_columns[["Summary"]][3]]][1])," ")[[1]][2],"V")[[1]][1],input[[paste0("selectRepertoires_",insertedRepertoires[k])]]," ", input$RepertoiresDataset),col = rainbow(length(data$N)))
          legend("topright", data$Gene, cex = 0.8,
                 fill = rainbow(length(data$N)))
          dev.off()
        }
        
      }
      
    }}}


####### Highly Similar Repertoires #######
HighlySim_repertories_pies_threshold <- 0.1
if (!(is.null(HighlySim_repertories_results))){####### reperoires plots
  if (HighlySim_repertories_results[[1]]$confirm!=""){
    if (is.null(HighlySim_repertories_pies_threshold)) thr=0 else thr=HighlySim_repertories_pies_threshold
    
    for (k in 1:length(insertedRepertoires)){
      for (j in 1:(length(loaded_datasets)+1)){
        if (j==(length(loaded_datasets)+1)){
          
          #Genes that have percentage<threshold are grouped into one cell
          data=HighlySim_repertories_results[[k]]$Repertoires_allData
          data_filterIn=data %>% filter(data$Freq>thr)
          data_filterOut=data %>% filter(data$Freq<=thr)
          data=data_filterIn
          data[(nrow(data)+1),]=c("Other genes",sum(data_filterOut$N),sum(data_filterOut$Freq))
          #plot
          f=paste0(in.path,"/","Highly_Sim_Repertoires_pies",input[[paste0("selectRepertoires_",insertedRepertoires[k])]],"_","All Data",".png")
          
          png(f,width=900, height=600)
          pie(as.numeric(data$N), labels = round(as.numeric(data$Freq),2), main = paste0(strsplit(strsplit(as.character(imgtfilter_results$allData[[used_columns[["Summary"]][3]]][1])," ")[[1]][2],"V")[[1]][1],input[[paste0("selectRepertoires_",insertedRepertoires[k])]]," ", input$RepertoiresDataset),col = rainbow(length(data$N)))
          legend("topright", data$Gene, cex = 0.8,
                 fill = rainbow(length(data$N)))
          dev.off()
          
        }else{
          #Genes that have percentage<threshold are grouped into one cell
          data=HighlySim_repertories_results[[k]]$Repertoires_datasets[[loaded_datasets[j]]]
          data_filterIn=data %>% filter(data$Freq>thr)
          data_filterOut=data %>% filter(data$Freq<=thr)
          data=data_filterIn
          data[(nrow(data)+1),]=c("Other genes",sum(data_filterOut$N),sum(data_filterOut$Freq))
          
          #plot
          f=paste0(in.path,"/","Highly_Sim_Repertoires_pies",input[[paste0("selectRepertoires_",insertedRepertoires[k])]],"_",loaded_datasets[j],".png")
          
          png(f,width=900, height=600)
          pie(as.numeric(data$N), labels = round(as.numeric(data$Freq),2), main = paste0(strsplit(strsplit(as.character(imgtfilter_results$allData[[used_columns[["Summary"]][3]]][1])," ")[[1]][2],"V")[[1]][1],input[[paste0("selectRepertoires_",insertedRepertoires[k])]]," ", input$RepertoiresDataset),col = rainbow(length(data$N)))
          legend("topright", data$Gene, cex = 0.8,
                 fill = rainbow(length(data$N)))
          dev.off()
        }
        
      }
      
    }}}

#Mutational status ####### 
pipeline_mutational_status <- T
if (("1_Summary.txt" %in% files) & (pipeline_mutational_status)){
  for (j in 1:(length(loaded_datasets)+1)){
    if (j==(length(loaded_datasets)+1)){
      png(paste0(in.path,"/","Mutational status ","All Data",".png"),width=900, height=600)
      pie(as.numeric(mutational_status_table_allData$N), labels = round(mutational_status_table_allData$freq*100,2), main = paste0("Mutational Status ", "All Data"),col = rainbow(length(mutational_status_table_allData$N)))
      legend("topright", as.character(mutational_status_table_allData[[used_columns[["Summary"]][4]]]), cex = 0.8,
             fill = rainbow(length(mutational_status_table_allData$N)))
      dev.off()
      write.table(mutational_status_table_allData, paste0(in.path,"/","Mutational Status ", "All Data",".txt"), sep = "\t")
    }else{
      png(paste0(in.path,"/","Mutational status ",loaded_datasets[j],".png"),width=900, height=600)
      pie(as.numeric(mutational_status_table_datasets[[loaded_datasets[j]]]$N), labels = round(100*mutational_status_table_datasets[[loaded_datasets[j]]]$freq,2), main = paste0("Mutational Status ", loaded_datasets[j]),col = rainbow(length(mutational_status_table_datasets[[loaded_datasets[j]]]$N)))
      legend("topright", as.character(mutational_status_table_datasets[[loaded_datasets[j]]][[used_columns[["Summary"]][4]]]), cex = 0.8,
             fill = rainbow(length(mutational_status_table_datasets[[loaded_datasets[j]]]$N)))
      dev.off()
      write.table(mutational_status_table_datasets[[loaded_datasets[j]]], paste0(in.path,"/","Mutational Status ", loaded_datasets[j],".txt"), sep = "\t", row.names = F)
    }
  }
}


############################## Distributions #####################################
select_clono_or_highly_for_cdr3_distribution <- "initial_clonotypes"
cdr3_length_distribution_dataset <- list()
if (clono$confirm!=""){
  ############ CDR3 Distribution  ############
  if (pipeline_cdr3_distribution){
    var=used_columns[["Summary"]][15]
    if (pipeline_highly_similar_clonotypes){
      if (select_clono_or_highly_for_cdr3_distribution=="initial_clonotypes"){
        highly=F
      }else{
        highly=T
      }
    }else{
      highly=F
    }
    if (!highly){
      for (j in 1:(length(loaded_datasets)+1)){
        if (j==(length(loaded_datasets)+1)){
          d=c()
          for (i in names(clono$view_specific_clonotype_allData)){
            d=c(d,clono$view_specific_clonotype_allData[[i]][[var]][1])
          }
          d=as.data.frame(d,stringsAsFactors=F)
          colnames(d)=var
          d = d %>% group_by((d[[var]])) %>% summarise(n=n())
          d$Freq=100*d$n/nrow(clono$clono_allData)
          colnames(d)=c("CDR3Length","n","Freq")
          d$CDR3Length=as.numeric(d$CDR3Length)
          cdr3_length_distribution<<-d[order(d$CDR3Length),]
        }else{
          d=c()
          for (i in names(clono$view_specific_clonotype_datasets[[loaded_datasets[j]]])){
            d=c(d,clono$view_specific_clonotype_datasets[[loaded_datasets[j]]][[i]][[var]][1])
          }
          d=as.data.frame(d,stringsAsFactors=F)
          colnames(d)=var
          d = d %>% group_by((d[[var]])) %>% summarise(n=n())
          d$Freq=100*d$n/nrow(clono$clono_datasets[[loaded_datasets[j]]])
          colnames(d)=c("CDR3Length","n","Freq")
          d$CDR3Length=as.numeric(d$CDR3Length)
          cdr3_length_distribution_dataset[[loaded_datasets[j]]] <- d[order(d$CDR3Length),]
        }
      }
    }else{
      for (j in 1:(length(loaded_datasets)+1)){
        if (j==(length(loaded_datasets)+1)){
          d=c()
          for (i in 1:nrow(highly_sim)){
            prev_clono=as.numeric(strsplit(as.character(highly_sim$prev_cluster[i])," ")[[1]][2:length(strsplit(as.character(highly_sim$prev_cluster[i])," ")[[1]])])
            a=clono$view_specific_clonotype_allData[[prev_clono[1]]]
            if(length(prev_clono)>1){
              for (cl in 2:length(prev_clono))
                a=rbind(a,clono$view_specific_clonotype_allData[[prev_clono[cl]]])
            }
            d=c(d,a[[var]][1])
          }
          d=as.data.frame(d,stringsAsFactors=F)
          colnames(d)=var
          d = d %>% group_by((d[[var]])) %>% summarise(n=n())
          d$Freq=100*d$n/nrow(highly_sim)
          colnames(d)=c("CDR3Length","n","Freq")
          d$CDR3Length=as.numeric(d$CDR3Length)
          cdr3_length_distribution<<-d[order(d$CDR3Length),]
        }else{
          d=c()
          for (i in 1:nrow(highly_sim_datasets[[loaded_datasets[j]]])){
            prev_clono=as.numeric(strsplit(as.character(highly_sim_datasets[[loaded_datasets[j]]]$prev_cluster[i])," ")[[1]][2:length(strsplit(as.character(highly_sim_datasets[[loaded_datasets[j]]]$prev_cluster[i])," ")[[1]])])
            prev_clono=prev_clono[!is.na(prev_clono)]
            a=clono$view_specific_clonotype_datasets[[loaded_datasets[j]]][[prev_clono[1]]]
            if(length(prev_clono)>1){
              for (cl in 2:length(prev_clono))
                a=rbind(a,clono$view_specific_clonotype_datasets[[loaded_datasets[j]]][[prev_clono[cl]]])
            }
            d=c(d,a[[var]][1])
          }
          d=as.data.frame(d,stringsAsFactors=F)
          colnames(d)=var
          d = d %>% group_by((d[[var]])) %>% summarise(n=n())
          d$Freq=100*d$n/nrow(highly_sim_datasets[[loaded_datasets[j]]])
          colnames(d)=c("CDR3Length","n","Freq")
          d$CDR3Length=as.numeric(d$CDR3Length)
          cdr3_length_distribution_dataset[[loaded_datasets[j]]] <- d[order(d$CDR3Length),]
        }
      }
    }
    
  }
}

#CDR3 Length Distribution #######
pipeline_cdr3_distribution <- T
if (pipeline_cdr3_distribution){
  for (j in 1:(length(loaded_datasets)+1)){
    if (j==(length(loaded_datasets)+1)){
      png(paste0(in.path,"/","CDR3 Length Distribution ","All Data",".png"),width=900, height=600)
      d=cdr3_length_distribution
      plot(d$CDR3Length,d$n,main=paste0("CDR3 IMGT length ", "All Data"), xlab="length",ylab="") # plots the results
      lines(spline(d$CDR3Length,d$n))
      dev.off()
      write.table(cdr3_length_distribution, paste0(in.path,"/","CDR3 Length Distribution ", "All Data",".txt"), sep = "\t")
    }else{
      png(paste0(in.path,"/","CDR3 Length Distribution ",loaded_datasets[j],".png"),width=900, height=600)
      d=cdr3_length_distribution_dataset[[loaded_datasets[j]]]
      plot(d$CDR3Length,d$n,main=paste0("CDR3 IMGT length ", "All Data"), xlab="length",ylab="") # plots the results
      lines(spline(d$CDR3Length,d$n))
      dev.off()
      write.table(cdr3_length_distribution_dataset[[loaded_datasets[j]]], paste0(in.path,"/","CDR3 Length Distribution ", loaded_datasets[j],".txt"), sep = "\t", row.names = F)
    }
  }
}

select_clono_or_highly_for_pi_distribution <- "initial_clonotypes"
if (pipeline_pi_distribution){
  var="Junction.pI"
  max_length=length(as.numeric(imgtfilter_results$allData[[var]]))
  box_input<<-c()
  
  if (pipeline_highly_similar_clonotypes){
    if (select_clono_or_highly_for_pi_distribution=="initial_clonotypes"){
      highly=F
    }else{
      highly=T
    }
  }else{
    highly=F
  }
  
  if (!highly){
    for (j in 1:(length(loaded_datasets)+1)){
      if (j==(length(loaded_datasets)+1)){
        d=c()
        for (i in names(clono$view_specific_clonotype_allData)){
          d=c(d,as.numeric(clono$view_specific_clonotype_allData[[i]][[var]][1]))
        }
        box_input<<-cbind(box_input,d)
      }else{
        d=c()
        for (i in names(clono$view_specific_clonotype_datasets[[loaded_datasets[j]]])){
          d=c(d,as.numeric(clono$view_specific_clonotype_datasets[[loaded_datasets[j]]][[i]][[var]][1]))
        }
        box_input<<-cbind(box_input,d)
      }
    }
  }else{
    for (j in 1:(length(loaded_datasets)+1)){
      if (j==(length(loaded_datasets)+1)){
        d=c()
        for (i in 1:nrow(highly_sim)){
          prev_clono=as.numeric(strsplit(as.character(highly_sim$prev_cluster[i])," ")[[1]][2:length(strsplit(as.character(highly_sim$prev_cluster[i])," ")[[1]])])
          a=clono$view_specific_clonotype_allData[[prev_clono[1]]]
          if(length(prev_clono)>1){
            for (cl in 2:length(prev_clono))
              a=rbind(a,clono$view_specific_clonotype_allData[[prev_clono[cl]]])
          }
          d=c(d,as.numeric(a[[var]][1]))
        }
        box_input<<-cbind(box_input,d)
      }else{
        d=c()
        for (i in 1:nrow(highly_sim_datasets[[loaded_datasets[j]]])){
          prev_clono=as.numeric(strsplit(as.character(highly_sim_datasets[[loaded_datasets[j]]]$prev_cluster[i])," ")[[1]][2:length(strsplit(as.character(highly_sim_datasets[[loaded_datasets[j]]]$prev_cluster[i])," ")[[1]])])
          prev_clono=prev_clono[!is.na(prev_clono)]
          a=clono$view_specific_clonotype_datasets[[loaded_datasets[j]]][[prev_clono[1]]]
          if(length(prev_clono)>1){
            for (cl in 2:length(prev_clono))
              a=rbind(a,clono$view_specific_clonotype_datasets[[loaded_datasets[j]]][[prev_clono[cl]]])
          }
          d=c(d,as.numeric(a[[var]][1]))
        }
        box_input<<-cbind(box_input,d)
      }
    }
  }
  
  colnames(box_input)=c(loaded_datasets,"All Data")
  box_input<<-box_input
  
}

if ("6_Junction.txt" %in% files){
  var="Junction.pI"
  
  if (pipeline_highly_similar_clonotypes){
    if (select_clono_or_highly_for_pi_distribution=="initial_clonotypes"){
      highly=F
    }else{
      highly=T
    }
  }else{
    highly=F
  }
  pi_distribution_dataset <- list()
  if (!highly){
    for (j in 1:(length(loaded_datasets)+1)){
      if (j==(length(loaded_datasets)+1)){
        d=c()
        for (i in names(clono$view_specific_clonotype_allData)){
          d=c(d,clono$view_specific_clonotype_allData[[i]][[var]][1])
        }
        d=as.data.frame(d,stringsAsFactors=F)
        colnames(d)=var
        d = d %>% group_by((d[[var]])) %>% summarise(n=n())
        d$Freq=100*d$n/nrow(clono$clono_allData)
        colnames(d)=c("Pi","n","Freq")
        d$Pi=as.numeric(d$Pi)
        pi_distribution<<-d[order(d$Pi),]
      }else{
        d=c()
        for (i in names(clono$view_specific_clonotype_datasets[[loaded_datasets[j]]])){
          d=c(d,clono$view_specific_clonotype_datasets[[loaded_datasets[j]]][[i]][[var]][1])
        }
        d=as.data.frame(d,stringsAsFactors=F)
        colnames(d)=var
        d = d %>% group_by((d[[var]])) %>% summarise(n=n())
        d$Freq=100*d$n/nrow(clono$clono_datasets[[loaded_datasets[j]]])
        colnames(d)=c("Pi","n","Freq")
        d$Pi=as.numeric(d$Pi)
        pi_distribution_dataset[[loaded_datasets[j]]]<-d[order(d$Pi),]
      }
    }
  }else{
    for (j in 1:(length(loaded_datasets)+1)){
      if (j==(length(loaded_datasets)+1)){
        d=c()
        for (i in 1:nrow(highly_sim)){
          prev_clono=as.numeric(strsplit(as.character(highly_sim$prev_cluster[i])," ")[[1]][2:length(strsplit(as.character(highly_sim$prev_cluster[i])," ")[[1]])])
          a=clono$view_specific_clonotype_allData[[prev_clono[1]]]
          if(length(prev_clono)>1){
            for (cl in 2:length(prev_clono))
              a=rbind(a,clono$view_specific_clonotype_allData[[prev_clono[cl]]])
          }
          d=c(d,a[[var]][1])
        }
        d=as.data.frame(d,stringsAsFactors=F)
        colnames(d)=var
        d = d %>% group_by((d[[var]])) %>% summarise(n=n())
        d$Freq=100*d$n/nrow(highly_sim)
        colnames(d)=c("Pi","n","Freq")
        d$Pi=as.numeric(d$Pi)
        pi_distribution<<-d[order(d$Pi),]
      }else{
        d=c()
        for (i in 1:nrow(highly_sim_datasets[[loaded_datasets[j]]])){
          prev_clono=as.numeric(strsplit(as.character(highly_sim_datasets[[loaded_datasets[j]]]$prev_cluster[i])," ")[[1]][2:length(strsplit(as.character(highly_sim_datasets[[loaded_datasets[j]]]$prev_cluster[i])," ")[[1]])])
          prev_clono=prev_clono[!is.na(prev_clono)]
          a=clono$view_specific_clonotype_datasets[[loaded_datasets[j]]][[prev_clono[1]]]
          if(length(prev_clono)>1){
            for (cl in 2:length(prev_clono))
              a=rbind(a,clono$view_specific_clonotype_datasets[[loaded_datasets[j]]][[prev_clono[cl]]])
          }
          d=c(d,a[[var]][1])
        }
        d=as.data.frame(d,stringsAsFactors=F)
        colnames(d)=var
        d = d %>% group_by((d[[var]])) %>% summarise(n=n())
        d$Freq=100*d$n/nrow(highly_sim_datasets[[loaded_datasets[j]]])
        colnames(d)=c("Pi","n","Freq")
        d$Pi=as.numeric(d$Pi)
        pi_distribution_dataset[[loaded_datasets[j]]]<-d[order(d$Pi),]
      }
    }
  }
  
}

#Pi Distribution #######  
pipeline_pi_distribution <- T
if (pipeline_pi_distribution){
  png(paste0(in.path,"/","Pi Distribution ","All Data",".png"),width=900, height=600)
  boxplot(box_input, horizontal=F, main=" ")
  dev.off()
  for (j in 1:(length(loaded_datasets)+1)){
    if (j==(length(loaded_datasets)+1)){
      write.table(pi_distribution, paste0(in.path,"/","Pi Distribution ", "All Data",".txt"), sep = "\t")
    }else{
      write.table(pi_distribution_dataset[[loaded_datasets[j]]], paste0(in.path,"/","Pi Distribution ", loaded_datasets[j],".txt"), sep = "\t", row.names = F)
    }
  } 
}


####### logo plots ####### 
msgLogo <- F
if (msgLogo!="")
  if (input$regionFreqTable=='CDR3'){
    for (j in 1:(length(loaded_datasets)+1)){
      if (j==(length(loaded_datasets)+1)){
        png(paste0(in.path,"/","logo_","CDR3","_","All Data",".png"),width=1000, height=550)
        logo_plot<<-plot(motif_all,ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
      }else{
        png(paste0(in.path,"/","logo_","CDR3","_",loaded_datasets[j],".png"),width=1000, height=550)
        logo_plot<<-plot(motif_datasets[[loaded_datasets[j]]],ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
      }
      table_count=frequenciesTables_results$table_count[,2:ncol(frequenciesTables_results$table_count)]
      index1=1
      index2=ncol(table_count)
      
      if ((ncol(table_count)-1)==13) a=105:117
      else if ((ncol(table_count)-1)==12) a=c(105:110,112:117)
      else if ((ncol(table_count)-1)==11) a=c(105:110,113:117)
      else if ((ncol(table_count)-1)==10) a=c(105:109,113:117)
      else if ((ncol(table_count)-1)==9) a=c(105:109,114:117)
      else if ((ncol(table_count)-1)==8) a=c(105:108,114:117)
      else if ((ncol(table_count)-1)==7) a=c(105:108,115:117)
      else if ((ncol(table_count)-1)==6) a=c(105:107,115:117)
      else if ((ncol(table_count)-1)==5) a=c(105:107,116:117)
      colnames(table_count) <- a
      
      if (input$regionFreqTable=="CDR3"){
        axis(1,at=seq((1/(2*(ncol(table_count[,index1:index2])-1))),1-1/(2*(ncol(table_count[,index1:index2])-1)),by=(1-1/(ncol(table_count[,index1:index2])-1))/(ncol(table_count[,index1:index2])-1)),colnames(table_count)) #paste0(index1:index2,":",colnames(table_count[,index1:index2]))
      }else{
        axis(1,at=seq((1/(2*(ncol(table_count[,index1:index2])-1))),1-1/(2*(ncol(table_count[,index1:index2])-1)),by=(1-1/(ncol(table_count[,index1:index2])-1))/(ncol(table_count[,index1:index2])-1)),index1:index2) #paste0(index1:index2,":",colnames(table_count[,index1:index2]))
      }
      axis(2,at=seq(0,1,by=1/5))
      dev.off()
    }
    
    
  }else{
    for (j in 1:(length(loaded_datasets)+1)){
      if (j==(length(loaded_datasets)+1)){
        png(paste0(in.path,"/","logo_",input$regionFreqTable,"_","All Data",".png"),width=1500, height=550)
        logo_plot<<-plot(motif_all,ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
      }else{
        png(paste0(in.path,"/","logo_",input$regionFreqTable,"_",loaded_datasets[j],".png"),width=1000, height=550)
        logo_plot<<-plot(motif_datasets[[loaded_datasets[j]]],ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
      }
      table_count=frequenciesTables_results$table_count[,2:ncol(frequenciesTables_results$table_count)]
      index1=1
      index2=ncol(table_count)
      axis(1,at=seq((1/(2*(ncol(table_count[,index1:index2])-1))),1-1/(2*(ncol(table_count[,index1:index2])-1)),by=(1-1/(ncol(table_count[,index1:index2])-1))/(ncol(table_count[,index1:index2])-1)),index1:index2) #paste0(index1:index2,":",colnames(table_count[,index1:index2]))
      axis(2,at=seq(0,1,by=1/5))
      dev.off()
    }
    
    for (j in 1:(length(loaded_datasets)+1)){
      region_names<-c("FR1-IMGT","CDR1-IMGT","FR2-IMGT","CDR2-IMGT","FR3-IMGT","CDR3-IMGT")
      index_1<-c(1,27,39,56,66,105)
      index_2<-c(26,38,55,65,104,114)
      
      region_id=0
      for (regions in region_names){
        region_id=region_id+1
        r=region_id
        i1=index_1[r]
        i2=index_2[r]
        if (j==(length(loaded_datasets)+1)){
          png(paste0(in.path,"/","logo_",regions,"_","All Data",".png"),width=1000, height=550)
          logo_plot<<-plot(logo_per_region[[regions]]$motif_all,ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
          table_count=frequenciesTables_results$table_count[,2:ncol(frequenciesTables_results$table_count)]
        }else{
          png(paste0(in.path,"/","logo_",regions,"_",input$Dataset[j],".png"),width=1000, height=550)
          logo_plot<<-plot(logo_per_region[[regions]]$motif_datasets[[loaded_datasets[j]]],ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
          table_count=frequenciesTables_results$table_count_datasets[[loaded_datasets[j]]][,2:ncol(frequenciesTables_results$table_count_datasets[[loaded_datasets[j]]])]
        }
        
        
        axis(1,at=seq((1/(2*(ncol(table_count[,i1:i2])-1))),1-1/(2*(ncol(table_count[,i1:i2])-1)),by=(1-1/(ncol(table_count[,i1:i2])-1))/(ncol(table_count[,i1:i2])-1)),i1:i2) #paste0(i1:i2,":",colnames(table_count[,i1:i2])
        axis(2,at=seq(0,1,by=1/5))
        dev.off()
      }
    }
    
  }

if (FclonoLogoSeperately){
  for (cl in 1:length(cl_ids_logos)){
    for (j in 1:(length(loaded_datasets)+1)){
      if (j==(length(loaded_datasets)+1)){
        png(paste0(in.path,"/","logo_cl",cl_ids_logos[cl],"_",input$regionFreqTable,"_","All Data",".png"),width=1000, height=550)
        logo_plot<<-plot(logo_result_cl[[cl]]$motif_all,ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
      }else{
        png(paste0(in.path,"/","logo_cl",cl_ids_logos[cl],"_",input$regionFreqTable,"_",loaded_datasets[j],".png"),width=1000, height=550)
        logo_plot<<-plot(logo_result_cl[[cl]]$motif_datasets[[loaded_datasets[j]]],ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
      }
      
      table_count=frequenciesTables_results_cl[[cl]]$table_count[,2:ncol(frequenciesTables_results_cl[[cl]]$table_count)]
      index1=1
      index2=ncol(table_count)
      if (input$regionFreqTable=="CDR3"){
        axis(1,at=seq((1/(2*(ncol(table_count[,index1:index2])-1))),1-1/(2*(ncol(table_count[,index1:index2])-1)),by=(1-1/(ncol(table_count[,index1:index2])-1))/(ncol(table_count[,index1:index2])-1)),colnames(table_count)) #paste0(index1:index2,":",colnames(table_count[,index1:index2]))
      }else{
        axis(1,at=seq((1/(2*(ncol(table_count[,index1:index2])-1))),1-1/(2*(ncol(table_count[,index1:index2])-1)),by=(1-1/(ncol(table_count[,index1:index2])-1))/(ncol(table_count[,index1:index2])-1)),index1:index2) #paste0(index1:index2,":",colnames(table_count[,index1:index2]))
      }
      axis(2,at=seq(0,1,by=1/5))
      dev.off()
    }
    if (input$regionFreqTable!="CDR3"){
      for (j in 1:(length(loaded_datasets)+1)){
        region_names<-c("FR1-IMGT","CDR1-IMGT","FR2-IMGT","CDR2-IMGT","FR3-IMGT","CDR3-IMGT")
        index_1<-c(1,27,39,56,66,105)
        index_2<-c(26,38,55,65,104,114)
        
        region_id=0
        for (regions in region_names){
          region_id=region_id+1
          r=region_id
          i1=index_1[r]
          i2=index_2[r]
          if (j==(length(loaded_datasets)+1)){
            png(paste0(in.path,"/","logo_cl",cl_ids_logos[cl],"_",regions,"_","All Data",".png"),width=1000, height=550)
            logo_plot<<-plot(logo_per_region_cl[[cl]][[regions]]$motif_all,ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
            table_count=frequenciesTables_results$table_count[,2:ncol(frequenciesTables_results$table_count)]
          }else{
            png(paste0(in.path,"/","logo_cl",cl_ids_logos[cl],"_",regions,"_",input$Dataset[j],".png"),width=1000, height=550)
            logo_plot<<-plot(logo_per_region_cl[[cl]][[regions]]$motif_datasets[[loaded_datasets[j]]],ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
            table_count=frequenciesTables_results$table_count_datasets[[loaded_datasets[j]]][,2:ncol(frequenciesTables_results$table_count_datasets[[loaded_datasets[j]]])]
          }
          
          
          axis(1,at=seq((1/(2*(ncol(table_count[,i1:i2])-1))),1-1/(2*(ncol(table_count[,i1:i2])-1)),by=(1-1/(ncol(table_count[,i1:i2])-1))/(ncol(table_count[,i1:i2])-1)),i1:i2) #paste0(i1:i2,":",colnames(table_count[,i1:i2])
          axis(2,at=seq(0,1,by=1/5))
          dev.off()
        }
      }
    }
  }
}

####### nucleotides of top clonotypes ####### 
fileNames=loaded_datasets
nucleotides_per_clonotype_topN <- 20
topN=nucleotides_per_clonotype_topN
if (clono$confirm!=""){
  if ((input$nucleotides_per_clonotype==F) && is.null(fileNames)){
    fileNames=loaded_datasets
    topN=10
  }
  nucleotides=matrix(0,topN,length(fileNames))
  
  allData<-list()
  input_datasets=""
  for (i in 1:length(fileNames)){
    #clono$convergent_evolution_list_allData[1:input$nucleotides_per_clonotype_topN,]
    nucleotides[,i]=clono$convergent_evolution_list_datasets_only_num[[loaded_datasets[i]]][1:nucleotides_per_clonotype_topN]
    input_datasets=paste(input_datasets,fileNames[i])
  }
  #plot
  png(paste0(in.path,"/","hist3D-nucleotides of top ",topN, " clonotypes of datasets",input_datasets,".png"))
  hist3D(y = 1:length(fileNames), x = 1:topN, z = nucleotides, clab = "Num of Nucleotides",ylab="Samples",xlab="Clonotypes",
         zlab="Num of Nucleotides",ticktype="detailed",axes=TRUE, theta=50, phi=25, expand=0.75)
  dev.off()
  
  if (length(fileNames)>1){
    #plot
    png(paste0(in.path,"/","persp3D-nucleotides of top ",topN, " clonotypes of datasets",input_datasets,".png"))
    persp3D(y = 1:length(fileNames), x = 1:topN, z = nucleotides, clab = "Num of Nucleotides",ylab="Samples",xlab="Clonotypes",
            zlab="Num of Nucleotides",ticktype="detailed",axes=TRUE, theta=50, phi=25, expand=0.75)
    dev.off()
    
    #plot
    png(paste0(in.path,"/","image2D-nucleotides of top ",topN, " clonotypes of datasets",input_datasets,".png"))
    image2D(y = 1:length(fileNames), x = 1:topN, z = nucleotides, clab = "Num of Nucleotides",ylab="Samples",xlab="Clonotypes",
            colkey = list(dist = 0, shift = 0.15,
                          side = 4, length = 0.5, width = 0.5,
                          cex.clab = 1, col.clab = "black", line.clab = 1.4,
                          col.axis = "black", col.ticks = "black", cex.axis = 0.8))
    dev.off()
  }
  
  
}