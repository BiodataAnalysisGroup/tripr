source("libraries.R")
source("helpers.R")

run_ARGP <- function(datapath, filelist, cell, throughput, preselection, selection, identity_range, vgenes, dgenes, jgenes, cdr3_length_range, aminoacid,
                     pipeline, select_clonotype, highly_sim_params, shared_clonotypes_params, highly_shared_clonotypes_params, repertoires_params, identity_groups,
                     multiple_values_params, alignment_params, mutations_params){
  ##### Input data parameters ####
  name <- list.files(datapath) #dataset names eg c("B1","B2")
  allDatasets <- name
  loaded_datasets <- list.files(datapath) #dataset names eg c("B1","B2")
  files <- filelist #selected imgt files eg c("1_Summary.txt", "2_IMGT-gapped-nt-sequences.txt", "4_IMGT-gapped-AA-sequences.txt","6_Junction.txt" )
  
  if (cell=="Bcell"){
    cell_id <- 2
    Tcell <- F
  }else{
    cell_id <- 1
    Tcell <- T
  }
  
  throughput <- "High Throughput"
  
  testColumnNamesOut <- testColumnNames(name, files, datapath)
  rawDataSet <- testColumnNamesOut$rawDataSet
  name <- names(rawDataSet)
  allDatasets <- name
  loaded_datasets <- name
  
  #### Preselection ####
  preselection <- strsplit(preselection, ",")[[1]]
  option4 <- which(startsWith(preselection,"4"))
  filterStart=""
  filterEnd=""
  filter_id <- as.numeric(preselection)
  if (length(option4) > 0){
    filter_id <- preselection[-option4]
    filter_id <- c(as.numeric(filter_id),4)
    temp <- strsplit(preselection[option4], 4)[[1]][2]
    filterStart <- paste0("^", strsplit(temp, ":")[[1]][1])
    filterEnd <- paste0(strsplit(temp, ":")[[1]][2], "$")
  }

  if (throughput=="High Throughput"){
    imgtcleaning_results <- imgtcleaning(rawDataSet, name, allDatasets, files, cell_id, 
                                         filter_id, " P| ORF", "[*]|X|#|[.]", "productive", 
                                         filterStart,filterEnd, identityLow=95, identityHigh=100, VGene="", 
                                         JGene="", DGene="", lengthLow=7, lengthHigh=15,  aminoacid="CASSPPDTGELFF", seq1=1,seq2=2,Tcell)
  }else{
    imgtcleaning_results <- imgtcleaningLow(rawDataSet, name, allDatasets, files, cell_id, 
                                            filter_id, " P| ORF", "[*]|X|#|[.]", "productive", 
                                            filterStart,filterEnd, identityLow=95, identityHigh=100, VGene="", 
                                            JGene="", DGene="", lengthLow=7, lengthHigh=15,  aminoacid="CASSPPDTGELFF", seq1=1,seq2=2,Tcell)
  }
  
  
  #### Selection ####
  filter_id <- as.numeric(strsplit(selection, ",")[[1]])
  identityLow <- 0
  identityHigh <- 100
  lengthLow <- 100
  lengthHigh <- 0
  if (5 %in% filter_id){
    identityLow <- as.numeric(strsplit(identity_range, ":")[[1]][1])
    identityHigh <- as.numeric(strsplit(identity_range, ":")[[1]][2])
  }
  
  if (9 %in% filter_id){
    lengthLow <- as.numeric(strsplit(cdr3_length_range, ":")[[1]][1])
    lengthHigh <- as.numeric(strsplit(cdr3_length_range, ":")[[1]][2])
  }
  
  if (throughput=="High Throughput"){
    imgtfilter_results <- imgtfilter(imgtcleaning_results$cleaned_datasets,loaded_datasets, imgtcleaning_results$allData, 
                                     cell_id, filter_id, " P| ORF", "[*]|X|#|[.]", "productive", start_char, 
                                     end_char, identityLow, identityHigh, vgenes, jgenes, dgenes, lengthLow, lengthHigh, aminoacid, seq1,seq2)
  }else{
    imgtfilter_results <- imgtfilterLow(imgtcleaning_results$cleaned_datasets,loaded_datasets, imgtcleaning_results$allData, 
                                        cell_id, filter_id, " P| ORF", "[*]|X|#|[.]", "productive", start_char, 
                                        end_char, identityLow, identityHigh, vgenes, jgenes, dgenes, lengthLow, lengthHigh, aminoacid, seq1,seq2)
  }
  
  cdr3_lengths <- sort(unique(imgtfilter_results$allData[[used_columns[["Summary"]][15]]]))
  cdr3_lengths <- as.numeric(cdr3_lengths) #+2
  cdr3_lengths <- sort(cdr3_lengths)
  
  
  
  ########### Pipeline ###############
  pipeline <- as.numeric(strsplit(pipeline, ",")[[1]])
  
  pipeline_clonotypes <- F
  pipeline_highly_similar_clonotypes <- F
  pipeline_public_clonotypes <- F
  pipeline_highly_sim_public_clonotypes <- F
  pipeline_Repertoires <- F
  pipeline_HighlySim_Repertoires <- F
  pipeline_repertoires_comparison <- F
  pipeline_insert_identity_groups <- F
  pipeline_mutational_status <- F
  pipeline_cdr3_distribution <- F
  pipeline_pi_distribution <- F
  pipeline_Multiple_value_comparison <- F
  pipeline_CDR3Diff1 <- F
  pipeline_alignment <- F
  pipeline_mutations <- F
  pipeline_logo <- F
  
  if (1 %in% pipeline){
    pipeline_clonotypes <- T
  }
  
  if (2 %in% pipeline){
    pipeline_highly_similar_clonotypes <- T
  }
  
  if (3 %in% pipeline){
    pipeline_public_clonotypes <- T
  }
  
  if (4 %in% pipeline){
    pipeline_highly_sim_public_clonotypes <- T
  }
  
  if (5 %in% pipeline){
    pipeline_Repertoires <- T
  }
  
  if (6 %in% pipeline){
    pipeline_repertoires_comparison <- T
  }
  
  if (7 %in% pipeline){
    pipeline_HighlySim_Repertoires <- T
  }
  
  if (8 %in% pipeline){
    pipeline_insert_identity_groups <- T
  }
  
  if (9 %in% pipeline){
    pipeline_mutational_status <- T
  }
  
  if (10 %in% pipeline){
    pipeline_cdr3_distribution <- T
  }
  
  if (11 %in% pipeline){
    pipeline_pi_distribution <- T
  }
  
  if (12 %in% pipeline){
    pipeline_Multiple_value_comparison <- T
  }
  
  if (13 %in% pipeline){
    pipeline_CDR3Diff1 <- T
  }
  
  if (14 %in% pipeline){
    pipeline_alignment <- T
  }
  
  if (15 %in% pipeline){
    pipeline_mutations <- T
  }
  
  if (16 %in% pipeline){
    pipeline_logo <- T
  }
  
  
  
  ############ Clonotypes ###############
  if (pipeline_clonotypes){
    if (select_clonotype=="V Gene + CDR3 Amino Acids"){
      allele=F
      gene=used_columns[["Summary"]][3]
      junction=used_columns[["Summary"]][18]
    }else if (select_clonotype=="V Gene and Allele + CDR3 Amino Acids"){
      allele=T
      gene=used_columns[["Summary"]][3]
      junction=used_columns[["Summary"]][18]
    }else if (select_clonotype=="V Gene + CDR3 Nucleotide"){
      allele=F
      gene=used_columns[["Summary"]][3]
      junction=used_columns[["IMGT.gapped.nt.sequences"]][9]
    }else if (select_clonotype=="V Gene and Allele + CDR3 Nucleotide"){
      allele=T
      gene=used_columns[["Summary"]][3]
      junction=used_columns[["IMGT.gapped.nt.sequences"]][9]
    }else if (select_clonotype=="J Gene + CDR3 Amino Acids"){
      allele=F
      gene=used_columns[["Summary"]][8]
      junction=used_columns[["Summary"]][18]
    }else if (select_clonotype=="J Gene and Allele + CDR3 Amino Acids"){
      allele=T
      gene=used_columns[["Summary"]][3]
      junction=used_columns[["Summary"]][18]
    }else if (select_clonotype=="J Gene + CDR3 Nucleotide"){
      allele=F
      gene=used_columns[["Summary"]][8]
      junction=used_columns[["IMGT.gapped.nt.sequences"]][9]
    }else if (select_clonotype=="J Gene and Allele + CDR3 Nucleotide"){
      allele=T
      gene=used_columns[["Summary"]][8]
      junction=used_columns[["IMGT.gapped.nt.sequences"]][9]
    }else if (select_clonotype=="CDR3 Amino Acids"){
      allele=F
      junction=used_columns[["Summary"]][18]
      gene=c()
    }else{
      allele=F
      junction=used_columns[["IMGT.gapped.nt.sequences"]][9]
      gene=c()
    }
    
    gene_clonotypes <- gene
    junction_clonotypes <- junction
    allele_clonotypes <- allele
    
    clono <- clonotypes(imgtfilter_results$allData,allele,gene,junction,loaded_datasets)
    
  }
  
  ########### Highly similar clonotypes #############
  if (pipeline_highly_similar_clonotypes){
    highly_sim_params <- strsplit(highly_sim_params, ",")[[1]]
    missmatches_user <- strsplit(strsplit(highly_sim_params[1], " ")[[1]],"-", fixed = T)
    missmatches_user2 <- as.data.frame(matrix(0, nrow = length(missmatches_user), ncol = 2))
    for (i in 1:length(missmatches_user)){
      missmatches_user2[i,] <- as.numeric(missmatches_user[[i]])
    }
    clonotype_freq_thr_for_highly_sim <- highly_sim_params[2]
    take_gene_highly_similar <- highly_sim_params[3]
    
    select_highly_sim_num_of_missmatches <- 'select_highly_sim_num_of_missmatches_number'
    num_of_missmatches <- rep(1,length(cdr3_lengths))
    #if (select_highly_sim_num_of_missmatches != 'select_highly_sim_num_of_missmatches_number'){
      for (i in 1:length(cdr3_lengths)){
        if (cdr3_lengths[i] %in% missmatches_user2[,1]){
          id_length <- which(missmatches_user2[,1] == cdr3_lengths[i])
          num_of_missmatches[i] <- missmatches_user2[id_length,2]
        }
        #num_of_missmatches <- c(num_of_missmatches,round(cdr3_lengths[i]*num_of_missmatches[i]/100,0))
      }
    #}

    highly_similar_clonotypes_results <- highly_similar_clonotypes(clono$clono_allData,clono$clono_datasets,num_of_missmatches,
                                                                   take_gene_highly_similar,cdr3_lengths,gene_clonotypes,
                                                                   clonotype_freq_thr_for_highly_sim,loaded_datasets)
    
    highly_sim_datasets <- list()
    for (d in names(highly_similar_clonotypes_results$highly_sim_clonotypes_datasets)){
      temp<-do.call(rbind.data.frame, highly_similar_clonotypes_results$highly_sim_clonotypes_datasets[[d]])
      temp$clonotype<-as.character(temp$clonotype)
      row.names(temp)=NULL
      temp=temp[,c("clonotype", "N", "Freq", "prev_cluster")]
      temp=temp[order(-temp$N),]
      row.names(temp)=1:nrow(temp)
      highly_sim_datasets[[d]]<-temp
      temp$Gene=NA
      temp$CDR3=NA
      for (cl in 1:nrow(temp)){
        temp$Gene[cl]=strsplit(temp$clonotype[cl]," - ")[[1]][1]
        temp$CDR3[cl]=strsplit(temp$clonotype[cl]," - ")[[1]][2]
      }
      temp=temp[,c("Gene","CDR3","N","Freq","prev_cluster")]
      if (save_tables_individually){
        write.table(temp, paste0(output_folder,"/","highly_sim_all_clonotypes_",d,".txt"), sep = "\t", row.names = FALSE, col.names = TRUE)
      }
    }
    
    highly_sim <- do.call(rbind.data.frame, highly_similar_clonotypes_results$highly_sim_clonotypes)
    highly_sim$clonotype <- as.character(highly_sim$clonotype)
    row.names(highly_sim)=NULL
    highly_sim=highly_sim[,c("clonotype", "N", "Freq", "prev_cluster")]
    highly_sim=highly_sim[order(-highly_sim$N),]
    row.names(highly_sim)=1:nrow(highly_sim)
    highly_sim<<-highly_sim
    
    if (save_tables_individually){
      temp=highly_sim
      temp$Gene=NA
      temp$CDR3=NA
      for (cl in 1:nrow(temp)){
        temp$Gene[cl]=strsplit(temp$clonotype[cl]," - ")[[1]][1]
        temp$CDR3[cl]=strsplit(temp$clonotype[cl]," - ")[[1]][2]
      }
      temp=temp[,c("Gene","CDR3","N","Freq","prev_cluster")]
      write.table(temp, paste0(output_folder,"/","highly_sim_all_clonotypes_","All Data",".txt"),sep = "\t", row.names = FALSE, col.names = TRUE)
    }
  }
  
  ############# Shared clonotypes ##############
  if (pipeline_public_clonotypes){
    if (strsplit(shared_clonotypes_params, ",")[[1]][1] == "reads")
      select_topN_or_reads_thr_shared_clono <- "select_reads_thr_shared_clono"
    take_gene_public_clono <- strsplit(shared_clonotypes_params, ",")[[1]][3]
    
    if (select_topN_or_reads_thr_shared_clono=="select_reads_thr_shared_clono"){
      use_reads <- T
      threshlod <- strsplit(shared_clonotypes_params, ",")[[1]][2]
    }else{
      use_reads <- F
      threshlod <- strsplit(shared_clonotypes_params, ",")[[1]][2]
    }
    
    public_clonotypes_results <- public_clonotypes(clono$clono_allData,clono$clono_datasets,take_gene_public_clono,use_reads,threshlod,loaded_datasets,F)
    
  }
  
  ############# Highly similar Shared clonotypes ##############
  if (pipeline_highly_sim_public_clonotypes){
    if (strsplit(highly_shared_clonotypes_params, ",")[[1]][1] == "reads")
      select_topN_or_reads_thr_shared_clono <- "select_reads_thr_shared_clono"
    take_gene_highly_sim_public_clono <- strsplit(highly_shared_clonotypes_params, ",")[[1]][3]
    
    if (select_topN_or_reads_thr_shared_clono=="select_reads_thr_shared_clono"){
      use_reads <- T
      thr_highly_sim_public_clono <- strsplit(highly_shared_clonotypes_params, ",")[[1]][2]
    }else{
      use_reads <- F
      thr_highly_sim_public_clono <- strsplit(highly_shared_clonotypes_params, ",")[[1]][2]
    }
    highly_sim_public_clonotypes_results <- public_clonotypes(highly_sim,highly_sim_datasets,take_gene_highly_sim_public_clono,T,
                                                              thr_highly_sim_public_clono,loaded_datasets,T)
    
  }
 
  
  
  ########### Repertoires #############
  if (pipeline_Repertoires){
    repertories_results <- list()
    insertedRepertoires <- as.numeric(strsplit(repertoires_params, ",")[[1]])
    
    for (i in 1:length(insertedRepertoires)){
      if (insertedRepertoires[i]==1){
        allele <- F
        gene <- used_columns[["Summary"]][3]
      }else if (insertedRepertoires[i]==2){
        allele <- T
        gene <- used_columns[["Summary"]][3]
      }else if (insertedRepertoires[i]==3){
        allele <- F
        gene <- used_columns[["Summary"]][8]
      }else if (insertedRepertoires[i]==4){
        allele <- T
        gene <- used_columns[["Summary"]][8]
      }else if (insertedRepertoires[i]==5){
        allele <- F 
        gene <- used_columns[["Summary"]][11]
      }else{
        allele <- T
        gene <- used_columns[["Summary"]][11]
      }
      
      repertories_results[[i]] <- repertoires(clono$clono_allData,clono$clono_datasets,allele,allele_clonotypes,
                                              gene,gene_clonotypes,loaded_datasets,clono$view_specific_clonotype_allData,
                                              clono$view_specific_clonotype_datasets)
      
    }
  }
  
  
  
  
  ############ Highly similar repertoires ############
  if (pipeline_HighlySim_Repertoires){
    if (pipeline_HighlySim_Repertoires){
      take_gene_highly_similar <- "Yes"
      HighlySim_repertories_results <- list()
      for (i in 1:length(insertedRepertoires)){
        HighlySim_repertories_results[[i]] <- repertoires_highly_similar(highly_sim,highly_sim_datasets,allele,allele_clonotypes,gene,gene_clonotypes,loaded_datasets,clono$view_specific_clonotype_allData,clono$view_specific_clonotype_datasets,take_gene_highly_similar)
      }
    }
  }
  
  
  ########### Repertoire comparison #########
  if (pipeline_repertoires_comparison){
    if (pipeline_repertoires_comparison){
      repertoires_comparison_results <- list()
      highly_sim_repertoires_comparison_results <- list()
      pipeline_HighlySim_Repertoires <- T
      for (i in 1:length(insertedRepertoires)){
        repertoires_comparison_results[[i]] <- repertoires_comparison(repertories_results[[i]]$Repertoires_allData,repertories_results[[i]]$Repertoires_datasets,loaded_datasets,F,i)
        if (pipeline_HighlySim_Repertoires==T){
          highly_sim_repertoires_comparison_results[[i]] <- repertoires_comparison(HighlySim_repertories_results[[i]]$Repertoires_allData,HighlySim_repertories_results[[i]]$Repertoires_datasets,loaded_datasets,T,i)
        }
      }
    }
  }
  
  
  ########### Identity groups #############
  if (pipeline_insert_identity_groups){
    if (("1_Summary.txt" %in% files) & (pipeline_mutational_status)){
      Identity_low_group <- as.numeric(strsplit(strsplit(identity_groups, ":")[[1]][1],",")[[1]])
      Identity_high_group <- as.numeric(strsplit(strsplit(identity_groups, ":")[[1]][2], ",")[[1]])
      print(Identity_low_group)
      print(Identity_high_group)
      select_clono_or_highly_for_mutational_status <- "initial_clonotypes"
      label=paste(Identity_low_group,Identity_high_group,sep="-")
      identity_groups <- (data.frame(low=Identity_low_group,high=Identity_high_group, label=label,stringsAsFactors = F))
      print(identity_groups)
      if (pipeline_highly_similar_clonotypes){
        if (select_clono_or_highly_for_mutational_status=="initial_clonotypes"){
          highly=F
        }else{
          highly=T
        }
      }else{
        highly=F
      }
      
      if (!highly){
        #All Data
        if (throughput=="Low Throughput"){
          filteredData_id<<-imgtfilter_results$allData
          temp=filteredData_id[[used_columns[["Summary"]][4]]]
          if (!is.null(identity_groups))
            for (values in 1:nrow(identity_groups)){
              if (values==nrow(identity_groups))  index=which(filteredData_id[[used_columns[["Summary"]][4]]]>=identity_groups[values,1] & filteredData_id[[used_columns[["Summary"]][4]]]<=identity_groups[values,2])
              else index=which(filteredData_id[[used_columns[["Summary"]][4]]]>=identity_groups[values,1] & filteredData_id[[used_columns[["Summary"]][4]]]<identity_groups[values,2])
              temp[index]=identity_groups$label[values]
            }
          filteredData_id[[used_columns[["Summary"]][4]]]<<-temp
        }else{
          d=c()
          var=used_columns[["Summary"]][4]
          for (i in names(clono$view_specific_clonotype_allData)){
            d=c(d,median(clono$view_specific_clonotype_allData[[i]][[var]]))
          }
          d=as.data.frame(d,stringsAsFactors=F)
          colnames(d)=var
          
          filteredData_id<<-d
          temp=d
          if (!is.null(identity_groups))
            for (values in 1:nrow(identity_groups)){
              if (values==nrow(identity_groups))  index=which(d[[var]]>=identity_groups[values,1] & d[[var]]<=identity_groups[values,2])
              else index=which(d[[var]]>=identity_groups[values,1] & d[[var]]<identity_groups[values,2])
              temp[index,1]=identity_groups$label[values]
            }
          filteredData_id<<-temp
        }
        
        
        #Separate data
        mutational_status_table_datasets <- list()
        for (j in 1:(length(loaded_datasets)+1)){
          if (j==(length(loaded_datasets)+1)){
            mut=filteredData_id %>% group_by(Summary.V.REGION.identity..) %>% summarise(N=n())
            freq=mut$N/nrow(filteredData_id)
            mutational_status_table_allData<<-data.frame(mut,freq)
          }else{
            if (throughput=="Low Throughput"){
              data=imgtfilter_results$filtered_datasets[[loaded_datasets[j]]]
              temp=data[[used_columns[["Summary"]][4]]]
            }else{
              var=used_columns[["Summary"]][4]
              name=loaded_datasets
              d=c()
              for (i in names(clono$view_specific_clonotype_datasets[[name[j]]])){
                d=c(d,median(clono$view_specific_clonotype_datasets[[name[j]]][[i]][[var]]))
              }
              d=as.data.frame(d,stringsAsFactors=F)
              colnames(d)=var
              temp=d
              data=d
            }
            if (!is.null(identity_groups)){
              for (values in 1:nrow(identity_groups)){
                if (values==nrow(identity_groups))  index=which(data[[used_columns[["Summary"]][4]]]>=identity_groups[values,1] & data[[used_columns[["Summary"]][4]]]<=identity_groups[values,2])
                else index=which(data[[used_columns[["Summary"]][4]]]>=identity_groups[values,1] & data[[used_columns[["Summary"]][4]]]<identity_groups[values,2])
                temp[index,1]=identity_groups$label[values]
              }
              data=temp
            }
            mut=data %>% group_by(Summary.V.REGION.identity..) %>% summarise(N=n())
            freq=mut$N/nrow(data)
            mutational_status_table_datasets[[loaded_datasets[j]]] <- data.frame(mut,freq)
          }
        }
        
      }else{
        #All Data
        if (throughput=="Low Throughput"){
          filteredData_id<<-imgtfilter_results$allData
          temp=filteredData_id[[used_columns[["Summary"]][4]]]
          if (!is.null(identity_groups))
            for (values in 1:nrow(identity_groups)){
              if (values==nrow(identity_groups))  index=which(filteredData_id[[used_columns[["Summary"]][4]]]>=identity_groups[values,1] & filteredData_id[[used_columns[["Summary"]][4]]]<=identity_groups[values,2])
              else index=which(filteredData_id[[used_columns[["Summary"]][4]]]>=identity_groups[values,1] & filteredData_id[[used_columns[["Summary"]][4]]]<identity_groups[values,2])
              temp[index]=identity_groups$label[values]
            }
          filteredData_id[[used_columns[["Summary"]][4]]]<<-temp
        }else{
          d=c()
          var=used_columns[["Summary"]][4]
          for (i in 1:nrow(highly_sim)){
            prev_clono=as.numeric(strsplit(as.character(highly_sim$prev_cluster[i])," ")[[1]][2:length(strsplit(as.character(highly_sim$prev_cluster[i])," ")[[1]])])
            a=clono$view_specific_clonotype_allData[[prev_clono[1]]]
            if(length(prev_clono)>1){
              for (cl in 2:length(prev_clono))
                a=rbind(a,clono$view_specific_clonotype_allData[[prev_clono[cl]]])
            }
            d=c(d,median(a[[var]]))
          }
          d=as.data.frame(d,stringsAsFactors=F)
          colnames(d)=var
          
          filteredData_id<<-d
          temp=d
          if (!is.null(identity_groups))
            for (values in 1:nrow(identity_groups)){
              if (values==nrow(identity_groups))  index=which(d[[var]]>=identity_groups[values,1] & d[[var]]<=identity_groups[values,2])
              else index=which(d[[var]]>=identity_groups[values,1] & d[[var]]<identity_groups[values,2])
              temp[index,1]=identity_groups$label[values]
            }
          filteredData_id<<-temp
        }
        
        
        #Separate data
        mutational_status_table_datasets <- list()
        for (j in 1:(length(loaded_datasets)+1)){
          if (j==(length(loaded_datasets)+1)){
            mut=filteredData_id %>% group_by(Summary.V.REGION.identity..) %>% summarise(N=n())
            freq=mut$N/nrow(filteredData_id)
            mutational_status_table_allData<<-data.frame(mut,freq)
          }else{
            if (throughput=="Low Throughput"){
              data=imgtfilter_results$filtered_datasets[[loaded_datasets[j]]]
              temp=data[[used_columns[["Summary"]][4]]]
            }else{
              var=used_columns[["Summary"]][4]
              name=loaded_datasets
              d=c()
              for (i in 1:nrow(highly_sim_datasets[[name[j]]])){
                prev_clono=as.numeric(strsplit(as.character(highly_sim_datasets[[name[j]]]$prev_cluster[i])," ")[[1]][2:length(strsplit(as.character(highly_sim_datasets[[name[j]]]$prev_cluster[i])," ")[[1]])])
                prev_clono=prev_clono[!is.na(prev_clono)]
                a=clono$view_specific_clonotype_datasets[[name[j]]][[prev_clono[1]]]
                if(length(prev_clono)>1){
                  for (cl in 2:length(prev_clono))
                    a=rbind(a,clono$view_specific_clonotype_datasets[[name[j]]][[prev_clono[cl]]])
                }
                d=c(d,median(a[[var]]))
              }
              d=as.data.frame(d,stringsAsFactors=F)
              colnames(d)=var
              temp=d
              data=d
            }
            if (!is.null(identity_groups)){
              for (values in 1:nrow(identity_groups)){
                if (values==nrow(identity_groups))  index=which(data[[used_columns[["Summary"]][4]]]>=identity_groups[values,1] & data[[used_columns[["Summary"]][4]]]<=identity_groups[values,2])
                else index=which(data[[used_columns[["Summary"]][4]]]>=identity_groups[values,1] & data[[used_columns[["Summary"]][4]]]<identity_groups[values,2])
                temp[index,1]=identity_groups$label[values]
              }
              data=temp
            }
            mut=data %>% group_by(Summary.V.REGION.identity..) %>% summarise(N=n())
            freq=mut$N/nrow(data)
            mutational_status_table_datasets[[loaded_datasets[j]]] <- data.frame(mut,freq)
          }
        }
      } 
    }
  }
  
  ############ Multiple value comparison ##########
  if (pipeline_Multiple_value_comparison){
    values <- strsplit(multiple_values_params,",")[[1]]
    select_clono_or_highly_for_Multiple_value_comparison <- F
    Multiple_value_comparison_result <- list()
    Multiple_value_comparison_input_values <- matrix("", nrow = 2, ncol = length(values))
    options <- c("V GENE", 
                 "V GENE and allele",
                 "J GENE", 
                 "J GENE and allele",
                 "D GENE", 
                 "D GENE and allele",
                 "CDR3-IMGT length", 
                 "D-REGION reading frame",
                 "Molecular mass",
                 "pI",
                 "V-REGION identity %")
    for (i in 1:length(values)){
      Multiple_value_comparison_input_values[1,i] <- options[as.numeric(strsplit(values[i], ":")[[1]][1])]
      Multiple_value_comparison_input_values[2,i] <- options[as.numeric(strsplit(values[i], ":")[[1]][2])]
    }
    Multiple_value_comparison_input_values[1,] <- c("V GENE", "J GENE") 
    Multiple_value_comparison_input_values[2,] <- c("J GENE", "CDR3-IMGT length")
    for (i in 1:nrow(Multiple_value_comparison_input_values)){
      val1 <- Multiple_value_comparison_input_values[i,1]
      val2 <- Multiple_value_comparison_input_values[i,2]
      Multiple_value_comparison_input_values<<-rbind(Multiple_value_comparison_input_values,c(val1,val2))
      
      if (select_clono_or_highly_for_Multiple_value_comparison=="initial_clonotypes"){
        highly=F
      }else{
        highly=T
      }
      if (highly)
        Multiple_value_comparison_result[[i]] <- Multiple_value_comparison_highly_similar(highly_sim,highly_sim_datasets,allele_clonotypes,gene_clonotypes,clono$view_specific_clonotype_allData,clono$view_specific_clonotype_datasets,val1,val2,loaded_datasets,c())
      else
        Multiple_value_comparison_result[[i]] <- Multiple_value_comparison(clono$clono_allData,clono$clono_datasets,allele_clonotypes,gene_clonotypes,clono$view_specific_clonotype_allData,clono$view_specific_clonotype_datasets,val1,val2,loaded_datasets,c())
      
    }
  }
  
  ################## Create Frequency Tables ####################
  if (pipeline_logo){
    select_topN_clonotypes_for_freqTable <- "topN_clonotypes_for_alignment"
    if (select_topN_clonotypes_for_freqTable=="topN_clonotypes_for_alignment"){
      FtopN=T
      Fthr=F
    }else{
      FtopN=F
      Fthr=T
    }
    pipeline_highly_similar_clonotypes <- T
    topNFreqTable <- 20
    if (FtopN){
      if (pipeline_highly_similar_clonotypes==F){
        clono_allDataTopN=clono$clono_allData[1:topNFreqTable,] 
        if (is.null(clono$clono_allData)) return()
        clono_datasetsTopN<-list()
        for (i in 1:length(loaded_datasets)){
          clono_datasetsTopN[[loaded_datasets[i]]]=clono$clono_datasets[[loaded_datasets[i]]][1:topNFreqTable,]
        }
      }else{
        clono_allDataTopN=highly_sim[1:topNFreqTable,] 
        if (is.null(highly_sim)) return()
        clono_datasetsTopN<-list()
        for (i in 1:length(loaded_datasets)){
          clono_datasetsTopN[[loaded_datasets[i]]]=highly_sim_datasets[[loaded_datasets[i]]][1:topNFreqTable,]
        }
      }
    }
    
    thrClonoLogos <- 0.1
    if (Fthr){
      if (pipeline_highly_similar_clonotypes==F){
        clono_allDataTopN=clono$clono_allData %>% filter(Freq>thrClonoLogos)
        if (is.null(clono$clono_allData)) return()
        clono_datasetsTopN<-list()
        for (i in 1:length(loaded_datasets)){
          clono_datasetsTopN[[loaded_datasets[i]]]=clono$clono_datasets[[loaded_datasets[i]]] %>% filter(Freq>thrClonoLogos)
        }
      }else{
        clono_allDataTopN=highly_sim %>% filter(Freq>thrClonoLogos)
        if (is.null(highly_sim)) return()
        clono_datasetsTopN<-list()
        for (i in 1:length(loaded_datasets)){
          clono_datasetsTopN[[loaded_datasets[i]]]=highly_sim_datasets[[loaded_datasets[i]]] %>% filter(Freq>thrClonoLogos)
        }
      }
    }
    regionFreqTable <- "CDR3"
    regionLengthFreq <- 12
    FclonoLogoSeperately <- F
    frequenciesTables_results <- createFrequencyTableCDR3(regionFreqTable,imgtfilter_results$allData,loaded_datasets,regionLengthFreq,(FtopN || Fthr),clono_allDataTopN,clono_datasetsTopN,gene_clonotypes,junction_clonotypes,allele_clonotypes)
    if (FclonoLogoSeperately){
      if (pipeline_highly_similar_clonotypes==F){
        for (cl in 1:length(cl_ids_logos)){
          clono_datasets_cl<-list()
          for (i in 1:length(loaded_datasets)){
            clono_datasets_cl[[loaded_datasets[i]]]=clono$clono_datasets[[loaded_datasets[i]]][cl_ids_logos[cl],]
          }
          frequenciesTables_results_cl[[cl]]<<-createFrequencyTableCDR3(regionFreqTable,imgtfilter_results$allData,loaded_datasets,regionLengthFreq,FclonoLogoSeperately,clono$clono_allData[cl_ids_logos[cl],],clono_datasets_cl,gene_clonotypes,junction_clonotypes,allele_clonotypes)
        }
      }else{
        for (cl in 1:length(cl_ids_logos)){
          clono_datasets_cl<-list()
          for (i in 1:length(loaded_datasets)){
            clono_datasets_cl[[loaded_datasets[i]]]=highly_sim_datasets[[loaded_datasets[i]]][cl_ids_logos[cl],]
          }
          frequenciesTables_results_cl[[cl]]<<-createFrequencyTableCDR3(regionFreqTable,imgtfilter_results$allData,loaded_datasets,regionLengthFreq,FclonoLogoSeperately,highly_sim[cl_ids_logos[cl],],clono_datasets_cl,gene_clonotypes,junction_clonotypes,allele_clonotypes)
        }
      }
    }
  }
  
  
  ############### Alignment #############
  if (pipeline_alignment){
    AAorNtAlignment <- strsplit(alignment_params, ",")[[1]][2]
    regionAlignment <- strsplit(alignment_params, ",")[[1]][1]
    if (regionAlignment == "1"){
      regionAlignment <- "V.D.J.REGION"
    }else{
      regionAlignment <- "V.J.REGION"
    }
    if (strsplit(alignment_params, ",")[[1]][3] == "1"){
      use_genes_germline <- F
      only_one_germline <- F
    }else if (strsplit(alignment_params, ",")[[1]][3] == "2"){
      use_genes_germline <- T
      only_one_germline <- F
    }else{
      use_genes_germline <- T
      only_one_germline <- T
    }
    
    Germline <- c()
    thrClonoAlignment <- 0.1
    topNClonoAlignment <- 20
    Fthr <- F
    FtopN <- F
    
    if (str_detect(strsplit(alignment_params, ",")[[1]][1], ":")){
      temp <- strsplit(strsplit(alignment_params, ",")[[1]][1], ":")[[1]]
      if (temp[1] == "2"){
        Fthr <- F
        FtopN <- T
        thrClonoAlignment <- 0.1
        topNClonoAlignment <- as.numeric(temp[2])
      }else{
        Fthr <- T
        FtopN <- F
        thrClonoAlignment <- as.numeric(temp[2])
        topNClonoAlignment <- 20
      }
    }
    
    #if (length(highly_sim)==0){
    if (AAorNtAlignment=="both"){
      alignmentRegion_results <- alignment(imgtfilter_results$allData,regionAlignment,Germline,loaded_datasets,only_one_germline,
                                           use_genes_germline,Tcell==T,"aa",clono$clono_allData,clono$clono_datasets,
                                           clono$view_specific_clonotype_allData,clono$view_specific_clonotype_datasets,
                                           topNClonoAlignment,FtopN,thrClonoAlignment,Fthr,F)
      alignmentRegion_results_nt <- alignment(imgtfilter_results$allData,regionAlignment,Germline,loaded_datasets,only_one_germline,
                                              use_genes_germline,Tcell==T,"nt",clono$clono_allData,clono$clono_datasets,
                                              clono$view_specific_clonotype_allData,clono$view_specific_clonotype_datasets,
                                              topNClonoAlignment,FtopN,thrClonoAlignment,Fthr,F)
    }else{
      alignmentRegion_results <- alignment(imgtfilter_results$allData,regionAlignment,Germline,loaded_datasets,only_one_germline,
                                           use_genes_germline,Tcell==T,AAorNtAlignment,clono$clono_allData,clono$clono_datasets,
                                           clono$view_specific_clonotype_allData,clono$view_specific_clonotype_datasets,
                                           topNClonoAlignment,FtopN,thrClonoAlignment,Fthr,F)
    }
    #}else{
    #  if (AAorNtAlignment=="both"){
    #    alignmentRegion_results <- alignment(imgtfilter_results$allData,regionAlignment,Germline,loaded_datasets,only_one_germline,
    #                                         use_genes_germline,Tcell==T,"aa",highly_sim,highly_sim_datasets,clono$view_specific_clonotype_allData,
    #                                         clono$view_specific_clonotype_datasets,topNClonoAlignment,FtopN,thrClonoAlignment,Fthr,T)
    #    alignmentRegion_results_nt <- alignment(imgtfilter_results$allData,regionAlignment,Germline,loaded_datasets,only_one_germline,
    #                                            use_genes_germline,Tcell==T,"nt",highly_sim,highly_sim_datasets,clono$view_specific_clonotype_allData,
    #                                            clono$view_specific_clonotype_datasets,topNClonoAlignment,FtopN,thrClonoAlignment,Fthr,T)
    #  }else{
    #    alignmentRegion_results <- alignment(imgtfilter_results$allData,regionAlignment,Germline,loaded_datasets,only_one_germline,use_genes_germline,
    #                                         Tcell==T,AAorNtAlignment,highly_sim,highly_sim_datasets,clono$view_specific_clonotype_allData,
    #                                         clono$view_specific_clonotype_datasets,topNClonoAlignment,FtopN,thrClonoAlignment,Fthr,T)
    #  }
    #}
    
    ####### Grouped alignment ########
    if (AAorNtAlignment=="both") n="aa" else n="nt"
    grouped_alignment_results <- groupedAlignment(alignmentRegion_results$alignment_allData,alignmentRegion_results$alignment_datasets,loaded_datasets,n)
    if (AAorNtAlignment=="both"){
      grouped_alignment_results_nt <- groupedAlignment(alignmentRegion_results_nt$alignment_allData,alignmentRegion_results_nt$alignment_datasets,loaded_datasets,"nt")
    }
    
  }
  
  ############### Mutations #############
  if (pipeline_mutations){
    AAorNtMutations <- strsplit(mutations_params, ",")[[1]][1]
    ThrAAMutations <- strsplit(mutations_params, ",")[[1]][2]
    ThrNtMutations <- strsplit(mutations_params, ",")[[1]][3]
    topNClonoMutations <- 20
    thrClonoMutations <- 0.1
    if (str_detect(strsplit(mutations_params, ",")[[1]][1], ":")){
      temp <- strsplit(strsplit(mutations_params, ",")[[1]][1], ":")[[1]]
      if (temp[1] == "2"){
        thrClonoMutations <- 0.1
        topNClonoMutations <- as.numeric(temp[2])
      }else{
        thrClonoMutations <- as.numeric(temp[2])
        topNClonoAlignment <- 20
      }
    }
    
    if (AAorNtMutations=="both"){ 
      mutation_results <- mutations(grouped_alignment_results$grouped_alignment_allData,grouped_alignment_results$grouped_alignment_datasets,ThrAAMutations,"aa",loaded_datasets,topNClonoMutations, FtopN,F,0,Fthr,thrClonoMutations)
      mutation_results_nt<<-mutations(grouped_alignment_results_nt$grouped_alignment_allData,grouped_alignment_results_nt$grouped_alignment_datasets,ThrNtMutations,"nt",loaded_datasets, topNClonoMutations, FtopN,F,0,Fthr,thrClonoMutations)
    }else{
      if (AAorNtMutations=="aa"){
        thr=ThrAAMutations 
        align_all=grouped_alignment_results$grouped_alignment_allData
        align_datasets=grouped_alignment_results$grouped_alignment_datasets
      }else {#AAorNtMutations=="nt"
        thr=ThrNtMutations
        if (AAorNtAlignment=="nt"){
          align_all=grouped_alignment_results$grouped_alignment_allData
          align_datasets=grouped_alignment_results$grouped_alignment_datasets
        } 
        else if (AAorNtAlignment=="both"){
          align_all=grouped_alignment_results_nt$grouped_alignment_allData
          align_datasets=grouped_alignment_results_nt$grouped_alignment_datasets
        } 
      }
      mutation_results<<-mutations(align_all,align_datasets,thr,AAorNtMutations,loaded_datasets,topNClonoMutations, FtopN,F,0,Fthr,thrClonoMutations)
    }
    
    FclonoSeperately <- T
    cl_ids_mutations <- c(1,2,3)
    mutation_results_cl <- list()
    mutation_results_nt_cl <- list()
    
    if (FclonoSeperately){
      for (cl in 1:length(cl_ids_mutations)){
        if (AAorNtMutations=="both"){ 
          mutation_results_cl[[cl]] <- mutations(grouped_alignment_results$grouped_alignment_allData,grouped_alignment_results$grouped_alignment_datasets,ThrAAMutations,"aa",loaded_datasets,topNClonoMutations, FtopN,FclonoSeperately,cl_ids_mutations[cl],F)
          mutation_results_nt_cl[[cl]] <- mutations(grouped_alignment_results_nt$grouped_alignment_allData,grouped_alignment_results_nt$grouped_alignment_datasets,ThrNtMutations,"nt",loaded_datasets, topNClonoMutations, FtopN,FclonoSeperately,cl_ids_mutations[cl],F)
        }else{
          if (AAorNtMutations=="aa"){
            thr=ThrAAMutations 
            align_all=grouped_alignment_results$grouped_alignment_allData
            align_datasets=grouped_alignment_results$grouped_alignment_datasets
          }else {#AAorNtMutations=="nt"
            thr=ThrNtMutations
            if (AAorNtAlignment=="nt"){
              align_all=grouped_alignment_results$grouped_alignment_allData
              align_datasets=grouped_alignment_results$grouped_alignment_datasets
            } 
            else if (AAorNtAlignment=="both"){
              align_all=grouped_alignment_results_nt$grouped_alignment_allData
              align_datasets=grouped_alignment_results_nt$grouped_alignment_datasets
            } 
          }
          mutation_results_cl[[cl]] <- mutations(align_all,align_datasets,thr,AAorNtMutations,loaded_datasets,topNClonoMutations, FtopN,FclonoSeperately,cl_ids_mutations[cl],F)
        }
      }
    }
  }
  
  
  
  ############### save png files  ############### 
  #source("save_png_files.R")
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
  if (pipeline_clonotypes){
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
      range=clonotypes_barchart_down_threshold:clonotypes_barchart_up_threshold
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
  if (pipeline_highly_similar_clonotypes){
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
  if (pipeline_Repertoires){####### reperoires plots
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
            f=paste0(in.path,"/","Repertoires_pies",insertedRepertoires[k],"_","All Data",".png")
            
            png(f,width=900, height=600)
            pie(as.numeric(data$N), labels = round(as.numeric(data$Freq),2), main = paste0(strsplit(strsplit(as.character(imgtfilter_results$allData[[used_columns[["Summary"]][3]]][1])," ")[[1]][2],"V")[[1]][1],insertedRepertoires[k]),col = rainbow(length(data$N)))
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
            f=paste0(in.path,"/","Repertoires_pies",insertedRepertoires[k],"_",loaded_datasets[j],".png")
            
            png(f,width=900, height=600)
            pie(as.numeric(data$N), labels = round(as.numeric(data$Freq),2), main = paste0(strsplit(strsplit(as.character(imgtfilter_results$allData[[used_columns[["Summary"]][3]]][1])," ")[[1]][2],"V")[[1]][1],insertedRepertoires[k]),col = rainbow(length(data$N)))
            legend("topright", data$Gene, cex = 0.8,
                   fill = rainbow(length(data$N)))
            dev.off()
          }
          
        }
        
      }}}
  
  
  ####### Highly Similar Repertoires #######
  HighlySim_repertories_pies_threshold <- 0.1
  if (pipeline_HighlySim_Repertoires){####### reperoires plots
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
            f=paste0(in.path,"/","Highly_Sim_Repertoires_pies",insertedRepertoires[k],"_","All Data",".png")
            
            png(f,width=900, height=600)
            pie(as.numeric(data$N), labels = round(as.numeric(data$Freq),2), main = paste0(strsplit(strsplit(as.character(imgtfilter_results$allData[[used_columns[["Summary"]][3]]][1])," ")[[1]][2],"V")[[1]][1],insertedRepertoires[k]),col = rainbow(length(data$N)))
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
            f=paste0(in.path,"/","Highly_Sim_Repertoires_pies",insertedRepertoires[k],"_",loaded_datasets[j],".png")
            
            png(f,width=900, height=600)
            pie(as.numeric(data$N), labels = round(as.numeric(data$Freq),2), main = paste0(strsplit(strsplit(as.character(imgtfilter_results$allData[[used_columns[["Summary"]][3]]][1])," ")[[1]][2],"V")[[1]][1],insertedRepertoires[k]),col = rainbow(length(data$N)))
            legend("topright", data$Gene, cex = 0.8,
                   fill = rainbow(length(data$N)))
            dev.off()
          }
          
        }
        
      }}}
  
  ####### Mutational status ####### 
  regionFreqTable <- 'CDR3' 
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
  
  
  ##### Distributions #####################################
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
  msgLogo <- ""
  if (msgLogo!=""){
    if (regionFreqTable=='CDR3'){
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
        
        if (regionFreqTable=="CDR3"){
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
          png(paste0(in.path,"/","logo_",regionFreqTable,"_","All Data",".png"),width=1500, height=550)
          logo_plot<<-plot(motif_all,ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
        }else{
          png(paste0(in.path,"/","logo_",regionFreqTable,"_",loaded_datasets[j],".png"),width=1000, height=550)
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
            png(paste0(in.path,"/","logo_",regions,"_",Dataset[j],".png"),width=1000, height=550)
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
          png(paste0(in.path,"/","logo_cl",cl_ids_logos[cl],"_",regionFreqTable,"_","All Data",".png"),width=1000, height=550)
          logo_plot<<-plot(logo_result_cl[[cl]]$motif_all,ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
        }else{
          png(paste0(in.path,"/","logo_cl",cl_ids_logos[cl],"_",regionFreqTable,"_",loaded_datasets[j],".png"),width=1000, height=550)
          logo_plot<<-plot(logo_result_cl[[cl]]$motif_datasets[[loaded_datasets[j]]],ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
        }
        
        table_count=frequenciesTables_results_cl[[cl]]$table_count[,2:ncol(frequenciesTables_results_cl[[cl]]$table_count)]
        index1=1
        index2=ncol(table_count)
        if (regionFreqTable=="CDR3"){
          axis(1,at=seq((1/(2*(ncol(table_count[,index1:index2])-1))),1-1/(2*(ncol(table_count[,index1:index2])-1)),by=(1-1/(ncol(table_count[,index1:index2])-1))/(ncol(table_count[,index1:index2])-1)),colnames(table_count)) #paste0(index1:index2,":",colnames(table_count[,index1:index2]))
        }else{
          axis(1,at=seq((1/(2*(ncol(table_count[,index1:index2])-1))),1-1/(2*(ncol(table_count[,index1:index2])-1)),by=(1-1/(ncol(table_count[,index1:index2])-1))/(ncol(table_count[,index1:index2])-1)),index1:index2) #paste0(index1:index2,":",colnames(table_count[,index1:index2]))
        }
        axis(2,at=seq(0,1,by=1/5))
        dev.off()
      }
      if (regionFreqTable!="CDR3"){
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
              png(paste0(in.path,"/","logo_cl",cl_ids_logos[cl],"_",regions,"_",Dataset[j],".png"),width=1000, height=550)
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
  }}
  
  ####### nucleotides of top clonotypes ####### 
  fileNames=loaded_datasets
  nucleotides_per_clonotype_topN <- 20
  nucleotides_per_clonotype <- F
  topN=nucleotides_per_clonotype_topN
  if (clono$confirm!=""){
    if ((nucleotides_per_clonotype==F) && is.null(fileNames)){
      fileNames=loaded_datasets
      topN=10
    }
    nucleotides=matrix(0,topN,length(fileNames))
    
    allData<-list()
    input_datasets=""
    for (i in 1:length(fileNames)){
      #clono$convergent_evolution_list_allData[1:nucleotides_per_clonotype_topN,]
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
} 
