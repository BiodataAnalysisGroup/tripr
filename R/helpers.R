testColumnNames <- function(name, files, datapath) {
    # Set Working Directory on level back
    #save(name,files,datapath, file = "./1_testColumnNames.RData")
    d <- "Datasets loaded:"

    d <- paste(d, name, collapse = " ")

    # logFile<-e$logFile
    # cat(paste0("testColumnNames", "\t"), file = logFile, append = TRUE)
    # cat(paste0(d, "\t"), file = logFile, append = TRUE)
    # cat(paste0("read data", "\t"), file = logFile, append = TRUE)
    # cat(paste0("read data", "\t"), file = logFile, append = TRUE)
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
    
    # used columns
    if (use_only_useful_columns) {
        input <- as.data.frame(fread(system.file("extdata/param",
            "used_columns_only_useful.csv",
            package = "tripr",
            mustWork = TRUE
        ),
        sep = ";", stringsAsFactors = FALSE, header = FALSE
        ))
    } else {
        input <- as.data.frame(fread(system.file("extdata/param",
            "used_columns.csv",
            package = "tripr",
            mustWork = TRUE
        ),
        sep = ";", stringsAsFactors = FALSE, header = FALSE
        ))
    }

    used_columns <- list()
    all_used_columns <- c()

    for (i in seq(1, 29, 3)) {
        file_name <- paste0(input[i, 1], ".txt")
        input[i, 1] <- strsplit(input[i, 1], "_")[[1]][2]
        input[i, 1] <- make.names(input[i, 1])
        input[i + 1, ] <- make.names(input[i + 1, ])
        a <- input[i + 1, which(input[i + 1, ] != "X")]
        a <- paste0(input[i, 1], ".", a)
        used_columns[[input[i, 1]]] <- paste0(input[i, 1], ".", input[i + 1, ])

        if (file_name %in% files) {
            all_used_columns <- c(all_used_columns, a[which(!stringr::str_detect(a, ".NA."))])
        }
    }
   
    all_used_columns <- c("dataName", all_used_columns)
    ## Stored in 'e' environment ("global.R", L:4)
    e$all_used_columns <- all_used_columns
    e$used_columns <- used_columns
    
    
    # filter_column contains the columns that are used for each one of the 9 filters with ids=1:9
    filter_column <- c()
    if ("1_Summary.txt" %in% files) {
        filter_column <- c(
            used_columns[["Summary"]][3],
            used_columns[["Summary"]][18],
            used_columns[["Summary"]][2],
            used_columns[["Summary"]][18],
            used_columns[["Summary"]][4],
            used_columns[["Summary"]][3],
            used_columns[["Summary"]][8],
            used_columns[["Summary"]][11],
            used_columns[["Summary"]][15],
            used_columns[["Summary"]][18]
        )
    }
    
    # Log start time and memory currently used
    #start.time <- Sys.time()

    rawDataSet <- list()
    count_wrong <- 0
    worng_columns_id <- list()
    worng_columns_names <- list()
    wrong_dataset <- c()

    # Load datasets individually
    for (i in seq_len(length(name))) {
        firstSepData <- TRUE
        if (strsplit(name[i], "_")[[1]][1] != name[i] && suppressWarnings(as.numeric(strsplit(name[i], "_")[[1]][2])) > 1) firstSepData <- FALSE
        rawDataSet[[name[i]]] <- data.frame(dataName = strsplit(name[i], "_")[[1]][1])
        worng_columns_names_temp <- c()
        worng_columns_id_temp <- c()
        num_initial_col <- 0

        for (j in seq_len(length(files))) {
            b <- strsplit(files[j], "_")
            b2 <- gsub(".txt", "", b[[1]][2])
            b2 <- gsub("-", ".", b2)
            var.name <- b2
            
            read_input <- fread(paste0(datapath, "/", name[i], "/", files[j]), sep = "\t", stringsAsFactors = FALSE, fill = TRUE)
            #read_input <- as.data.frame(read_input) #aspa
            # temp <- data.frame(assign(var.name, read_input)) #aspa
            temp <- assign(var.name, read_input)

            num_initial_col <- num_initial_col + ncol(temp)

            colnames(temp) <- paste0(b2, ".", colnames(temp))
            if (paste0(b2, ".X") %in% colnames(temp)) {
                temp <- temp[, -match(paste0(b2, ".X"), names(temp))]
            }

            rawDataSet[[name[i]]] <- data.frame(rawDataSet[[name[i]]], temp)

            # Check the column names
            for (k in seq_len(length(used_columns[[b2]]))) {
                if ((used_columns[[b2]][k] %in% colnames(rawDataSet[[name[i]]])) || stringr::str_detect(used_columns[[b2]][k], ".NA") || stringr::str_detect(used_columns[[b2]][k], ".X")) {
                    # do nothing
                } else {
                    message <- "wrong column names"
                    tmp2 <- paste0(files[j], ": ", gsub("[.]", " ", gsub(b2, "", used_columns[[b2]][k])))
                    if (tmp2 %in% worng_columns_names_temp) {

                    } else {
                        worng_columns_names_temp <- c(worng_columns_names_temp, tmp2)
                        worng_columns_id_temp <- c(worng_columns_id_temp, which(all_used_columns %in% used_columns[[b2]][k]))
                    }
                }
            }
        }

        if (length(worng_columns_id_temp) > 0) {
            wrong_dataset <- c(wrong_dataset, name[i])
            count_wrong <- count_wrong + 1
            worng_columns_id[[count_wrong]] <- worng_columns_id_temp
            worng_columns_names[[count_wrong]] <- worng_columns_names_temp
        } else {
            # Drop all the columns that will not be used
            
            rawDataSet[[name[i]]] <- setDT(rawDataSet[[name[i]]])
            rawDataSet[[name[i]]] <- rawDataSet[[name[i]]][, all_used_columns,with=FALSE]
            rawDataSet[[name[i]]] <- setDF(rawDataSet[[name[i]]])
          
        }
    }
    for (i in seq_len(length(name))) {
        firstSepData <- TRUE

        if (strsplit(name[i], "_")[[1]][1] != name[i] && suppressWarnings(as.numeric(strsplit(name[i], "_")[[1]][2])) == 0) {
            newName <- strsplit(name[i], "_")[[1]][1]
            rawDataSet[[newName]] <- rawDataSet[[name[i]]]
            rawDataSet[[name[i]]] <- NULL
        }

        if (strsplit(name[i], "_")[[1]][1] != name[i] && suppressWarnings(as.numeric(strsplit(name[i], "_")[[1]][2])) > 0) {
            newName <- strsplit(name[i], "_")[[1]][1]
            rawDataSet[[newName]] <- rbind(rawDataSet[[newName]], rawDataSet[[name[i]]])
            rawDataSet[[name[i]]] <- NULL
        }

        num_of_datasets <- length(rawDataSet)
    }

    newDatasetNames <- names(rawDataSet)

    k <- c()
    # check if the column names are correct

    # log time end and memory used
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
    # cat(pryr::mem_used(), file = logFile, append = TRUE, sep = "\n")

    confirm <- "Data Loaded!"

    if (length(worng_columns_id) > 0) {
        return(list(
            "confirm" = confirm, #"logFile" = logFile,
            "message" = "wrong column names",
            "wrong_dataset" = wrong_dataset,
            "worng_columns_id" = worng_columns_id,
            "worng_columns_names" = worng_columns_names,
            "rawDataSet" = rawDataSet
        ))
    }
    
    # Correct the wrong column names
    return(list(
        "confirm" = confirm, #"logFile" = logFile,
        "newDatasetNames" = newDatasetNames,
        "message" = "",
        "wrong_dataset" = c(),
        "worng_columns_id" = c(),
        "worng_columns_names" = c(),
        "rawDataSet" = rawDataSet
    ))
}

######################################################################################################################################

correctColumnNames <- function(files, rawDataSet, allDatasets, wrong_dataset, new_columns = list(), worng_columns_id = list(), name) {
    nr <- 0
    for (i in names(rawDataSet)) {
        nr <- nrow(rawDataSet[[i]]) + nr
    }
    
    # logfile
    # logFile<-e$logFile
    # cat(paste0("correctColumnNames", "\t"), file = logFile, append = TRUE)
    # cat(paste0("wrong datasets", paste(wrong_dataset, sep = ","), "\t"), file = logFile, append = TRUE)
    # cat(paste0(nr, "\t"), file = logFile, append = TRUE)
    # cat(paste0(ncol(rawDataSet[[names(rawDataSet)[1]]]), "\t"), file = logFile, append = TRUE)
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)

    # filter_column contains the columns that are used for each one of the 9 filters with ids=1:9
    filter_column <- c(used_columns[["Summary"]][3], used_columns[["Summary"]][18], used_columns[["Summary"]][2], used_columns[["Summary"]][18], used_columns[["Summary"]][4], used_columns[["Summary"]][3], used_columns[["Summary"]][8], used_columns[["Summary"]][11], used_columns[["Summary"]][15], used_columns[["Summary"]][18])

    # used columns
    input <- fread(system.file("extdata/param", "used_columns.csv",  #as.data.frame
        package = "tripr", mustWork = TRUE
    ),
    sep = ";", stringsAsFactors = FALSE, header = FALSE
    )

    used_columns <- list()
    all_used_columns <- c()
    for (i in seq(1, 29, 3)) {
        file_name <- paste0(input[i, 1], ".txt")
        input[i, 1] <- strsplit(input[i, 1], "_")[[1]][2]
        input[i, 1] <- make.names(input[i, 1])
        input[i + 1, ] <- make.names(input[i + 1, ])
        a <- input[i + 1, which(input[i + 1, ] != "X")]
        a <- paste0(input[i, 1], ".", a)
        used_columns[[input[i, 1]]] <- a

        if (file_name %in% files) {
            all_used_columns <- c(all_used_columns, a)
        }
    }
    all_used_columns <- c("dataName", all_used_columns)

    # Log start time and memory currently used
    #start.time <- Sys.time()
    correct <- 0

    w <- 0
    for (i in seq_len(length(wrong_dataset))) {
        w <- w + length(worng_columns_id[[i]])
        # update columns
        # wrong column names

        for (j in seq_len(length(new_columns[[i]]))) {
            if (length(which(names(rawDataSet[[wrong_dataset[i]]]) == new_columns[[i]][j])) > 0) {
                correct <- correct + 1
            }
            names(rawDataSet[[wrong_dataset[i]]])[which(names(rawDataSet[[wrong_dataset[i]]]) == new_columns[[i]][j])] <- all_used_columns[worng_columns_id[[i]][j]]
        }
    }

    if (correct == w) {
        correct <- "yes"
        for (i in seq_len(length(name))) {
            rawDataSet[[name[i]]] <- setDT(rawDataSet[[name[i]]])
            rawDataSet[[name[i]]] <- rawDataSet[[name[i]]][, all_used_columns,with=FALSE]
            rawDataSet[[name[i]]] <- setDF(rawDataSet[[name[i]]])
        }
    } else {
        correct <- "no"
    }

    # log time end and memory used
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
    # cat(pryr::mem_used(), file = logFile, append = TRUE, sep = "\n")

    return(list("rawDataSet" = rawDataSet, "correct" = correct))
}

######################################################################################################################################

imgtcleaning <- function(rawDataSet, name, allDatasets, files, cell_id = 1, filter_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), filter_out_char1 = " P", filter_out_char2 = "[*]|X|#|[.]", filter_in_char = "productive", filterStart = "^*", filterEnd = "*$", identityLow = 95, identityHigh = 100, VGene = "", JGene = "", DGene = "", lengthLow = 7, lengthHigh = 15, aminoacid = "CASSPPDTGELFF", seq1 = 1, seq2 = 2, Tcell) {
    used_columns <- e$used_columns
    a <- "Filter ids "
    for (i in seq_len(length(filter_id))) {
      a <- paste0(a, ",", filter_id[i])
    }
    nr <- 0
    for (i in names(rawDataSet)) {
      nr <- nrow(rawDataSet[[i]]) + nr
    }
    # logfile
    # logFile<-e$logFile
    # cat(paste0("imgtcleaning", "\t"), file = logFile, append = TRUE)
    # cat(paste0(a, "\t"), file = logFile, append = TRUE)
    # cat(paste0(nr, "\t"), file = logFile, append = TRUE)
    # cat(paste0(ncol(rawDataSet[[names(rawDataSet)[1]]]), "\t"), file = logFile, append = TRUE)
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
    
    cleaning_criteria <- c(
      "Functional V-Gene",
      "CDR3 with no Special Characters",
      "Productive Sequence",
      "Productive Sequences"
    )
    
    # filter_column contains the columns that are used for each one of the 9 filters with ids=1:9
    filter_column <- c(
      used_columns[["Summary"]][3],
      used_columns[["Summary"]][18],
      used_columns[["Summary"]][2],
      used_columns[["Summary"]][18],
      used_columns[["Summary"]][4],
      used_columns[["Summary"]][3],
      used_columns[["Summary"]][8],
      used_columns[["Summary"]][11],
      used_columns[["Summary"]][15],
      used_columns[["Summary"]][18],
      "Summary.V.REGION.identity....with.ins.del.events."
    )
    
    filterOut <- list()
    workflow <- matrix(0, length(filter_id), 3)
    
    # Log start time and memory currently used
    #start.time <- Sys.time()
    
    # Combine raw name
    for (i in seq_len(length(name))) {
        if (i == 1) {
          allData <- rawDataSet[[name[i]]]
        } else {
          allData <- rbind(allData, rawDataSet[[name[i]]])
        }
    }
    
    test_column <- c(filter_column[1], filter_column[2], filter_column[5], used_columns[["Nt.sequences"]][1])
    
    # Drop all the columns that will not be used
    
    # IMPORTANT: Datasets should start with T
    # dataSetIDs <- as.list(levels(unique(allData$dataName)))
    
    allDataInitial <- allData
    workflow_datasets <- list()
    
    a <- matrix(0, length(filter_id), 3)
    for (j in seq_len(length(name))) {
        workflow_datasets[[name[j]]] <- a
    }
    
    # Take only the first gene (V, J D) (Separated with "or")
    a <- which(stringr::str_detect(allData[[filter_column[1]]], " or|,"))
    if (length(a) > 0) {
        a2 <- strsplit(allData[[filter_column[1]]][a], " or|,")
        allData[[filter_column[1]]][a] <- as.character(plyr::ldply(a2, function(s) {
          t(data.frame(unlist(s)))
        })[, 1])
    }
    
    a <- which(stringr::str_detect(allData[[filter_column[7]]], " or|,"))
    if (length(a) > 0) {
        a2 <- strsplit(allData[[filter_column[7]]][a], " or|,")
        allData[[filter_column[7]]][a] <- as.character(plyr::ldply(a2, function(s) {
          t(data.frame(unlist(s)))
        })[, 1])
    }
    
    if (!all(is.na(allData[[filter_column[8]]]))) {
      a <- which(stringr::str_detect(allData[[filter_column[8]]], " or|,"))
      if (length(a) > 0) {
          a2 <- strsplit(allData[[filter_column[8]]][a], " or|,")
          allData[[filter_column[8]]][a] <- as.character(plyr::ldply(a2, function(s) {
            t(data.frame(unlist(s)))
          })[, 1])
      }
    }
    
    # Remove (see comment) from AA.JUNCTION
    a <- which(stringr::str_detect(allData[[filter_column[2]]], " [(]see"))
    if (length(a) > 0) {
        a2 <- strsplit(allData[[filter_column[2]]][a], " [(]see")
        allData[[filter_column[2]]][a] <- as.character(plyr::ldply(a2, function(s) {
          t(data.frame(unlist(s)))
        })[, 1])
    }
    
    # Apply the requested filters
    if (any(filter_id == 1)) {
        i <- which(filter_id == 1)
        filterOut[[1]] <- allData %>% dplyr::filter(stringr::str_detect(allData[[filter_column[1]]], filter_out_char1))
        if (nrow(filterOut[[1]]) > 0) filterOut[[1]] <- cbind(filterOut[[1]], FilterId = cleaning_criteria[1])
        
        allData <- allData %>% dplyr::filter(!stringr::str_detect(allData[[filter_column[1]]], filter_out_char1))
        
        workflow[i, 1] <- filter_id[i]
        workflow[i, 2] <- nrow(filterOut[[1]])
        workflow[i, 3] <- nrow(allData)
        
        for (j in seq_len(length(name))) {
            if (nrow(filterOut[[1]]) > 0) {
              filterOut_datasets <- filterOut[[1]] %>% dplyr::filter(filterOut[[1]]$dataName == name[j])
            } else {
              filterOut_datasets <- filterOut[[1]]
            }
            
            if (i == 1) {
              prev <- nrow(rawDataSet[[name[j]]])
            } else {
              prev <- (workflow_datasets[[name[j]]][i - 1, 3])
            }
            
            workflow_datasets[[name[j]]][i, 3] <- prev - nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 2] <- nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 1] <- filter_id[i]
        }
    }
    
    if (any(filter_id == 2)) {
        i <- which(filter_id == 2)
        filterOut[[2]] <- allData %>% dplyr::filter(stringr::str_detect(allData[[filter_column[2]]], filter_out_char2))
        if (nrow(filterOut[[2]]) > 0) filterOut[[2]] <- cbind(filterOut[[2]], FilterId = cleaning_criteria[2])
        
        allDataInitial_tmp <- allData
        allData <- allData %>% dplyr::filter(!stringr::str_detect(allData[[filter_column[2]]], filter_out_char2))
        
        workflow[i, 1] <- filter_id[i]
        workflow[i, 2] <- nrow(filterOut[[filter_id[i]]])
        workflow[i, 3] <- nrow(allData)
        
        for (j in seq_len(length(name))) {
            if (nrow(filterOut[[2]]) > 0) {
              filterOut_datasets <- filterOut[[2]] %>% dplyr::filter(filterOut[[2]]$dataName == name[j])
            } else {
              filterOut_datasets <- filterOut[[2]]
            }
            if (i == 1) {
              prev <- nrow(rawDataSet[[name[j]]])
            } else {
              prev <- workflow_datasets[[name[j]]][i - 1, 3]
            }
            workflow_datasets[[name[j]]][i, 3] <- prev - nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 2] <- nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 1] <- filter_id[i]
        }                           
    }
    
    if (any(filter_id == 3)) {
        i <- which(filter_id == 3)
        
        if (Tcell) {
          filterOut[[3]] <- allData %>% dplyr::filter(!stringr::str_detect(allData[[filter_column[3]]], "^productive$"))
          allData <- allData %>% dplyr::filter(stringr::str_detect(allData[[filter_column[3]]], "^productive$"))
        } else {
          filterOut[[3]] <- allData %>% dplyr::filter(!stringr::str_detect(allData[[filter_column[3]]], "^productive"))
          
          allData <- allData %>% dplyr::filter(stringr::str_detect(allData[[filter_column[3]]], "^productive"))
          
          ins_del <- which(!is.na(allData[[filter_column[11]]]))
          
          if (length(ins_del) > 0) {
            # check if V-REGION deletions or V-REGION insertions contain (cause frameshift)
            delete <- allData[ins_del, ] %>% dplyr::filter(stringr::str_detect(allData[[used_columns[["Summary"]][21]]][ins_del], "[(]cause frameshift[)]"))
            delete2 <- allData[ins_del, ] %>% dplyr::filter(stringr::str_detect(allData[[used_columns[["Summary"]][22]]][ins_del], "[(]cause frameshift[)]"))
            delete_all <- unique(rbind(delete, delete2))
            
            filterOut[[3]] <- unique(rbind(filterOut[[3]], delete_all))
            allData <- allData %>% dplyr::filter(!(allData[[used_columns[["Summary"]][1]]] %in% filterOut[[3]][[used_columns[["Summary"]][1]]]))
          }
      }
      
      if (nrow(filterOut[[3]]) > 0) filterOut[[3]] <- cbind(filterOut[[3]], FilterId = cleaning_criteria[3])
      
      workflow[i, 3] <- nrow(allData)
      workflow[i, 1] <- filter_id[i]
      workflow[i, 2] <- nrow(filterOut[[3]])
      
      for (j in seq_len(length(name))) {
          if (nrow(filterOut[[3]]) > 0) {
            filterOut_datasets <- filterOut[[3]] %>% dplyr::filter(filterOut[[3]]$dataName == name[j])
          } else {
            filterOut_datasets <- filterOut[[3]]
          }
          if (i == 1) {
            prev <- nrow(rawDataSet[[name[j]]])
          } else {
            prev <- (workflow_datasets[[name[j]]][i - 1, 3])
          }
          workflow_datasets[[name[j]]][i, 3] <- (prev) - nrow(filterOut_datasets)
          workflow_datasets[[name[j]]][i, 2] <- nrow(filterOut_datasets)
          workflow_datasets[[name[j]]][i, 1] <- filter_id[i]
        }
    }
    
    if (any(filter_id == 4)) {
        i <- which(filter_id == 4)
        if (filterStart == "" && filterEnd == "") {
          filterOut[[4]] <- allData %>% dplyr::filter(!(stringr::str_detect(allData[[filter_column[4]]], filterStart)))
        } else if (filterEnd == "") {
          filterOut[[4]] <- allData %>% dplyr::filter(!(stringr::str_detect(allData[[filter_column[4]]], filterStart)))
          allData <- allData %>% dplyr::filter((stringr::str_detect(allData[[filter_column[4]]], filterStart)))
        } else if (filterStart == "") {
          filterOut[[4]] <- allData %>% dplyr::filter(!stringr::str_detect(allData[[filter_column[4]]], filterEnd))
          allData <- allData %>% dplyr::filter(stringr::str_detect(allData[[filter_column[4]]], filterEnd))
        } else {
          filterOut[[4]] <- allData %>% dplyr::filter(!(stringr::str_detect(allData[[filter_column[4]]], filterStart)))
          filterOut[[4]] <- rbind(filterOut[[4]], (allData %>% dplyr::filter(!(stringr::str_detect(allData[[filter_column[4]]], filterEnd)))))
          allData <- allData %>% dplyr::filter((stringr::str_detect(allData[[filter_column[4]]], filterStart)))
          allData <- allData %>% dplyr::filter(stringr::str_detect(allData[[filter_column[4]]], filterEnd))
        }
        if (nrow(filterOut[[4]]) > 0) filterOut[[4]] <- cbind(filterOut[[4]], FilterId = cleaning_criteria[4])
        
        
        workflow[i, 1] <- filter_id[i]
        workflow[i, 2] <- nrow(filterOut[[4]])
        workflow[i, 3] <- nrow(allData)
        
        for (j in seq_len(length(name))) {
            if (nrow(filterOut[[4]]) > 0) {
              filterOut_datasets <- filterOut[[4]] %>% dplyr::filter(filterOut[[4]]$dataName == name[j])
            } else {
              filterOut_datasets <- filterOut[[4]]
            }
            if (i == 1) {
              prev <- nrow(rawDataSet[[name[j]]])
            } else {
              prev <- workflow_datasets[[name[j]]][i - 1, 3]
            }
            workflow_datasets[[name[j]]][i, 3] <- nrow(allData %>% dplyr::filter(allData$dataName == name[j]))
            workflow_datasets[[name[j]]][i, 2] <- prev - nrow(allData %>% dplyr::filter(allData$dataName == name[j]))
            workflow_datasets[[name[j]]][i, 1] <- filter_id[i]
        }
    }
    
    filterOutSum <- c()
    if (length(filter_id) > 0) {
      for (i in seq_len(length(filter_id))) {
        if (i == 1) {
          filterOutSum <- filterOut[[filter_id[1]]]
        } else {
          filterOutSum <- rbind(filterOutSum, filterOut[[filter_id[i]]])
        }
      }
    }
    
    a <- name[1]
    if (length(name) > 1) {
      for (i in 2:length(name)) {
        a <- paste0(a, ", ", name[i])
      }
    }
    
    b <- filter_id[1]
    if (length(filter_id) > 1) {
      for (i in 2:length(filter_id)) {
        b <- paste0(b, ", ", filter_id[i])
      }
    }
    
    # Separate allData to different tables
    initial_datasets <- list()
    for (i in seq_len(length(name))) {
      initial_datasets[[name[i]]] <- allDataInitial %>% dplyr::filter(allDataInitial$dataName == name[i])
    }
    
    cleaned_datasets <- list()
    if (length(allData) > 0) {
      for (i in seq_len(length(name))) {
        cleaned_datasets[[name[i]]] <- allData %>% dplyr::filter(allData$dataName == name[i])
      }
    }
    
    cleaned_out_datasets <- list()
    if (length(filterOutSum) > 0) {
      for (i in seq_len(length(name))) {
        cleaned_out_datasets[[name[i]]] <- filterOutSum %>% dplyr::filter(filterOutSum$dataName == name[i])
      }
    }
    
    confirm <- paste0("Datasets cleaned: ", a, ". Cleaning Filters applied: ", b)
    
    # log time end and memory used
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
    # cat(pryr::mem_used(), file = logFile, append = TRUE, sep = "\n")
    
    
    result <- list(
      "message" = "",
      "dim" = dim(allData),
      "workflow" = workflow,
      "workflow_datasets" = workflow_datasets,
      "allDataInitial" = allDataInitial,
      "allData" = allData,
      "filterOutSum" = filterOutSum,
      "initial_datasets" = initial_datasets,
      "cleaned_datasets" = cleaned_datasets,
      "cleaned_out_datasets" = cleaned_out_datasets,
      "confirm" = confirm
    )
    
    
    return(result)
}

######################################################################################################################################

imgtfilter <- function(rawDataSet, name, allData, cell_id = 1, filter_id = c(5, 6, 7, 8, 9, 10), filter_out_char1 = " P", filter_out_char2 = "[:punct:]|X", filter_in_char = "productive", filterStart = "^*", filterEnd = "*$", identityLow = 95, identityHigh = 100, VGene = "", JGene = "", DGene = "", lengthLow = 7, lengthHigh = 15, aminoacid = "CASSPPDTGELFF", seq1 = 1, seq2 = 2) {
    # logfile
    # logFile<-e$logFile
    used_columns <- e$used_columns
    a <- "Filter ids "
    for (i in seq_len(length(filter_id))) {
        a <- paste0(a, ",", filter_id[i])
    }
    # cat(paste0("imgtfilter", "\t"), file = logFile, append = TRUE)
    # cat(paste0(a, "\t"), file = logFile, append = TRUE)
    # cat(paste0(nrow(allData), "\t"), file = logFile, append = TRUE)
    # cat(paste0(ncol(rawDataSet[[names(rawDataSet)[1]]]), "\t"), file = logFile, append = TRUE)
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)

    cleaning_criteria <- c("Functional V-Gene", "CDR3 with no Special Characters", "Productive Sequence", "Productive Sequences")
    filtering_criteria <- c("V-REGION identity %", "Specific V Gene", "Specific J Gene", "Specific D Gene", "CDR3 length range", "CDR3 length range")
    criteria <- c(cleaning_criteria, filtering_criteria)

    # filter_column contains the columns that are used for each one of the 9 filters with ids=1:9
    filter_column <- c(used_columns[["Summary"]][3], used_columns[["Summary"]][18], used_columns[["Summary"]][2], used_columns[["Summary"]][18], used_columns[["Summary"]][4], used_columns[["Summary"]][3], used_columns[["Summary"]][8], used_columns[["Summary"]][11], used_columns[["Summary"]][15], used_columns[["Summary"]][18], "Summary.V.REGION.identity....with.ins.del.events.")
    filterOut <- list()
    workflow <- matrix(0, length(filter_id), 3)

    # Log start time and memory currently used
    #start.time <- Sys.time()

    test_column <- c(filter_column[1], filter_column[2], filter_column[5], used_columns[["Nt.sequences"]][1])

    used_column <- c("dataName")
    for (i in seq_len(length(filter_id))) {
        used_column <- c(used_column, filter_column[filter_id[i]])
    }

    # IMPORTANT: Datasets should start with T
    # dataSetIDs <- as.list(levels(unique(allData$dataName)))

    allDataInitial <- allData

    workflow_datasets <- list()

    a <- matrix(0, length(filter_id), 3)
    for (j in seq_len(length(name))) {
        workflow_datasets[[name[j]]] <- a
    }

    # Apply the requested filters

    if (any(filter_id == 5)) {
        i <- which(filter_id == 5)
        ins_del <- which(!is.na(allData[[filter_column[11]]]))

        if (length(ins_del) > 0) {
            allData[[filter_column[5]]][ins_del] <- allData[[filter_column[11]]][ins_del]
        }

        filterOut[[5]] <- allData[which(allData[[filter_column[5]]] > identityHigh | allData[[filter_column[5]]] < identityLow), ]
        allData <- allData[which(allData[[filter_column[5]]] <= identityHigh & allData[[filter_column[5]]] >= identityLow), ]

        if (nrow(filterOut[[5]]) > 0) filterOut[[5]] <- cbind(filterOut[[5]], FilterId = criteria[5])
        workflow[i, 3] <- nrow(allData)


        workflow[i, 1] <- filter_id[i] - 4
        workflow[i, 2] <- nrow(filterOut[[filter_id[i]]])

        for (j in seq_len(length(name))) {
            if (nrow(filterOut[[5]]) > 0) {
                filterOut_datasets <- filterOut[[5]] %>% dplyr::filter(filterOut[[5]]$dataName == name[j])
            } else {
                filterOut_datasets <- filterOut[[5]]
            }
            if (i == 1) {
                prev <- nrow(rawDataSet[[name[j]]])
            } else {
                prev <- workflow_datasets[[name[j]]][i - 1, 3]
            }
            workflow_datasets[[name[j]]][i, 3] <- (prev) - nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 2] <- nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 1] <- filter_id[i] - 4
        }
    }

    if (any(filter_id == 6)) {
        i <- which(filter_id == 6)
        filterOut[[6]] <- allData %>% dplyr::filter(!stringr::str_detect(allData[[filter_column[6]]], gsub("[(]", "[(]", gsub("[)]", "[)]", gsub("[*]", "[*]", VGene)))))
        if (nrow(filterOut[[6]]) > 0) filterOut[[6]] <- cbind(filterOut[[6]], FilterId = criteria[6])
        workflow[i, 3] <- nrow(allData) - nrow(filterOut[[filter_id[i]]])
        allData <- allData %>% dplyr::filter(stringr::str_detect(allData[[filter_column[6]]], gsub("[(]", "[(]", gsub("[)]", "[)]", gsub("[*]", "[*]", VGene)))))

        workflow[i, 1] <- filter_id[i] - 4
        workflow[i, 2] <- nrow(filterOut[[filter_id[i]]])

        for (j in seq_len(length(name))) {
            if (nrow(filterOut[[6]]) > 0) {
                filterOut_datasets <- filterOut[[6]] %>% dplyr::filter(filterOut[[6]]$dataName == name[j])
            } else {
                filterOut_datasets <- filterOut[[6]]
            }
            if (i == 1) {
                prev <- nrow(rawDataSet[[name[j]]])
            } else {
                prev <- workflow_datasets[[name[j]]][i - 1, 3]
            }
            workflow_datasets[[name[j]]][i, 3] <- (prev) - nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 2] <- nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 1] <- filter_id[i] - 4
        }
    }

    if (any(filter_id == 7)) {
        i <- which(filter_id == 7)
        filterOut[[7]] <- allData %>% dplyr::filter(!stringr::str_detect(allData[[filter_column[7]]], gsub("[(]", "[(]", gsub("[)]", "[)]", gsub("[*]", "[*]", JGene)))))
        if (nrow(filterOut[[7]]) > 0) filterOut[[7]] <- cbind(filterOut[[7]], FilterId = criteria[7])
        workflow[i, 3] <- nrow(allData) - nrow(filterOut[[filter_id[i]]])
        allData <- allData %>% dplyr::filter(stringr::str_detect(allData[[filter_column[7]]], gsub("[(]", "[(]", gsub("[)]", "[)]", gsub("[*]", "[*]", JGene)))))

        workflow[i, 1] <- filter_id[i] - 4
        workflow[i, 2] <- nrow(filterOut[[filter_id[i]]])

        for (j in seq_len(length(name))) {
            if (nrow(filterOut[[7]]) > 0) {
                filterOut_datasets <- filterOut[[7]] %>% dplyr::filter(filterOut[[7]]$dataName == name[j])
            } else {
                filterOut_datasets <- filterOut[[7]]
            }
            if (i == 1) {
                prev <- nrow(rawDataSet[[name[j]]])
            } else {
                prev <- workflow_datasets[[name[j]]][i - 1, 3]
            }
            workflow_datasets[[name[j]]][i, 3] <- (prev) - nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 2] <- nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 1] <- filter_id[i] - 4
        }
    }

    if (any(filter_id == 8)) {
        i <- which(filter_id == 8)
        filterOut[[8]] <- allData %>% dplyr::filter(!stringr::str_detect(allData[[filter_column[8]]], gsub("[(]", "[(]", gsub("[)]", "[)]", gsub("[*]", "[*]", DGene)))))
        if (nrow(filterOut[[8]]) > 0) filterOut[[8]] <- cbind(filterOut[[8]], FilterId = criteria[8])
        workflow[i, 3] <- nrow(allData) - nrow(filterOut[[filter_id[i]]])
        allData <- allData %>% dplyr::filter(stringr::str_detect(allData[[filter_column[8]]], gsub("[(]", "[(]", gsub("[)]", "[)]", gsub("[*]", "[*]", DGene)))))

        workflow[i, 1] <- filter_id[i] - 4
        workflow[i, 2] <- nrow(filterOut[[filter_id[i]]])

        for (j in seq_len(length(name))) {
            if (nrow(filterOut[[8]]) > 0) {
                filterOut_datasets <- filterOut[[8]] %>% dplyr::filter(filterOut[[8]]$dataName == name[j])
            } else {
                filterOut_datasets <- filterOut[[8]]
            }
            if (i == 1) {
                prev <- nrow(rawDataSet[[name[j]]])
            } else {
                prev <- workflow_datasets[[name[j]]][i - 1, 3]
            }
            workflow_datasets[[name[j]]][i, 3] <- (prev) - nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 2] <- nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 1] <- filter_id[i] - 4
        }
    }

    if (any(filter_id == 9)) {
        i <- which(filter_id == 9)
        filterOut[[9]] <- allData[which(as.numeric(allData[[filter_column[9]]]) > lengthHigh | as.numeric(allData[[filter_column[9]]]) < lengthLow), ]
        if (nrow(filterOut[[9]]) > 0) filterOut[[9]] <- cbind(filterOut[[9]], FilterId = criteria[9])
        workflow[i, 3] <- nrow(allData) - nrow(filterOut[[filter_id[i]]])
        allData <- allData[which(as.numeric(allData[[filter_column[9]]]) <= lengthHigh & as.numeric(allData[[filter_column[9]]]) >= lengthLow), ]

        workflow[i, 1] <- filter_id[i] - 4
        workflow[i, 2] <- nrow(filterOut[[filter_id[i]]])

        for (j in seq_len(length(name))) {
            if (nrow(filterOut[[9]]) > 0) {
                filterOut_datasets <- filterOut[[9]] %>% dplyr::filter(filterOut[[9]]$dataName == name[j])
            } else {
                filterOut_datasets <- filterOut[[9]]
            }
            if (i == 1) {
                prev <- nrow(rawDataSet[[name[j]]])
            } else {
                prev <- workflow_datasets[[name[j]]][i - 1, 3]
            }
            workflow_datasets[[name[j]]][i, 3] <- (prev) - nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 2] <- nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 1] <- filter_id[i] - 4
        }
    }

    if (any(filter_id == 10)) {
        i <- which(filter_id == 10)
        filterOut[[10]] <- allData %>% dplyr::filter(!stringr::str_detect(allData[[filter_column[10]]], aminoacid))
        if (nrow(filterOut[[10]]) > 0) filterOut[[10]] <- cbind(filterOut[[10]], FilterId = criteria[10])
        workflow[i, 3] <- nrow(allData) - nrow(filterOut[[filter_id[i]]])
        allData <- allData %>% dplyr::filter(stringr::str_detect(allData[[filter_column[10]]], aminoacid))

        workflow[i, 1] <- filter_id[i] - 4
        workflow[i, 2] <- nrow(filterOut[[filter_id[i]]])

        for (j in seq_len(length(name))) {
            if (nrow(filterOut[[10]]) > 0) {
                filterOut_datasets <- filterOut[[10]] %>% dplyr::filter(filterOut[[10]]$dataName == name[j])
            } else {
                filterOut_datasets <- filterOut[[10]]
            }
            if (i == 1) {
                prev <- nrow(rawDataSet[[name[j]]])
            } else {
                prev <- workflow_datasets[[name[j]]][i - 1, 3]
            }
            workflow_datasets[[name[j]]][i, 3] <- (prev) - nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 2] <- nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 1] <- filter_id[i] - 4
        }
    }

    # Provide a 1_Summary.txt of the datasets

    filterOutSum <- c()
    if (length(filter_id) > 0) {
        for (i in seq_len(length(filter_id))) {
            if (i == 1) {
                filterOutSum <- filterOut[[filter_id[1]]]
            } else {
                filterOutSum <- rbind(filterOutSum, filterOut[[filter_id[i]]])
            }
        }
    }

    # Do the actual analysis - targeted tables

    a <- name[1]
    if (length(name) > 1) {
        for (i in 2:length(name)) {
            a <- paste0(a, ", ", name[i])
        }
    }

    b <- filter_id[1] - 4
    if (length(filter_id) > 1) {
        for (i in 2:length(filter_id)) {
            b <- paste0(b, ", ", (filter_id[i] - 4))
        }
    }

    # Delete (see comment) from genes
    a <- which(stringr::str_detect(allData[[used_columns[["Summary"]][3]]], " or|,| [(]see"))
    if (length(a) > 0) {
        a2 <- strsplit(allData[[used_columns[["Summary"]][3]]][a], " or|,| [(]see")
        allData[[used_columns[["Summary"]][3]]][a] <- as.character(plyr::ldply(a2, function(s) {
            t(data.frame(unlist(s)))
        })[, 1])
    }

    a <- which(stringr::str_detect(allData[[used_columns[["Summary"]][8]]], " or|,| [(]see"))
    if (length(a) > 0) {
        a2 <- strsplit(allData[[used_columns[["Summary"]][8]]][a], " or|,| [(]see")
        allData[[used_columns[["Summary"]][8]]][a] <- as.character(plyr::ldply(a2, function(s) {
            t(data.frame(unlist(s)))
        })[, 1])
    }

    if (!all(is.na(allData[[used_columns[["Summary"]][11]]]))) {
        a <- which(stringr::str_detect(allData[[used_columns[["Summary"]][11]]], " or|,| [(]see"))
        if (length(a) > 0) {
            a2 <- strsplit(allData[[used_columns[["Summary"]][11]]][a], " or|,| [(]see")
            allData[[used_columns[["Summary"]][11]]][a] <- as.character(plyr::ldply(a2, function(s) {
                t(data.frame(unlist(s)))
            })[, 1])
        }
    }


    # Separate allData to different tables
    initial_datasets <- list()
    for (i in seq_len(length(name))) {
        initial_datasets[[name[i]]] <- allDataInitial %>% dplyr::filter(allDataInitial$dataName == name[i])
    }

    filtered_datasets <- list()
    if (length(allData) > 0) {
        for (i in seq_len(length(name))) {
            filtered_datasets[[name[i]]] <- allData %>% dplyr::filter(allData$dataName == name[i])
            if (save_tables_individually_filter_in) {
                write.table((filtered_datasets[[name[i]]]), paste0(e$output_folder, "/", "filter_in_", name[i], ".txt"), sep = "\t", row.names = FALSE, col.names = TRUE)
            }
        }
    }

    filtered_out_datasets <- list()
    if (length(filterOutSum) > 0) {
        for (i in seq_len(length(name))) {
            filtered_out_datasets[[name[i]]] <- filterOutSum %>% dplyr::filter(filterOutSum$dataName == name[i])
        }
    }

    a <- ""
    for (i in seq_len(length(name))) {
        a <- paste(a, name[i])
    }

    if (save_tables_individually_filter_in) {
        write.table((allData), paste0(e$output_folder, "/", "filter_in_All_Data", ".txt"), sep = "\t", row.names = FALSE, col.names = TRUE)
    }

    confirm <- paste0("Datasets filtered: ", a, ". Filters applied: ", b)

    # log time end and memory used
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
    # cat(pryr::mem_used(), file = logFile, append = TRUE, sep = "\n")

    result <- list(
        "message" = "",
        "dim" = dim(allData),
        "workflow" = workflow,
        "workflow_datasets" = workflow_datasets,
        "allDataInitial" = allDataInitial,
        "allData" = allData,
        "filterOutSum" = filterOutSum,
        "initial_datasets" = initial_datasets,
        "filtered_datasets" = filtered_datasets,
        "filtered_out_datasets" = filtered_out_datasets,
        "confirm" = confirm
    )



    return(result)
}

######################################################################################################################################

imgtcleaningLow <- function(rawDataSet, name, allDatasets, files, cell_id = 1, filter_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), filter_out_char1 = " P", filter_out_char2 = "[*]|X|#|[.]", filter_in_char = "productive", filterStart = "^*", filterEnd = "*$", identityLow = 95, identityHigh = 100, VGene = "", JGene = "", DGene = "", lengthLow = 7, lengthHigh = 15, aminoacid = "CASSPPDTGELFF", seq1 = 1, seq2 = 2, Tcell) {
    used_columns <- e$used_columns
    a <- "Filter ids "
    for (i in seq_len(length(filter_id))) {
        a <- paste0(a, ",", filter_id[i])
    }
    nr <- 0
    for (i in names(rawDataSet)) {
        nr <- nrow(rawDataSet[[i]]) + nr
    }
    # logfile
    # logFile<-e$logFile
    # cat(paste0("imgtcleaning", "\t"), file = logFile, append = TRUE)
    # cat(paste0(a, "\t"), file = logFile, append = TRUE)
    # cat(paste0(nr, "\t"), file = logFile, append = TRUE)
    # cat(paste0(ncol(rawDataSet[[names(rawDataSet)[1]]]), "\t"), file = logFile, append = TRUE)
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)

    cleaning_criteria <- c("Functional V-Gene", "CDR3 with no Special Characters", "Productive Sequence", "Productive Sequences")

    # filter_column contains the columns that are used for each one of the 9 filters with ids=1:9
    filter_column <- c(used_columns[["Summary"]][3], used_columns[["Summary"]][18], used_columns[["Summary"]][2], used_columns[["Summary"]][18], used_columns[["Summary"]][4], used_columns[["Summary"]][3], used_columns[["Summary"]][8], used_columns[["Summary"]][11], used_columns[["Summary"]][15], used_columns[["Summary"]][18], "Summary.V.REGION.identity....with.ins.del.events.")
    filterOut <- list()
    filterIn <- list()
    workflow <- matrix(0, length(filter_id), 3)

    # Log start time and memory currently used
    #start.time <- Sys.time()

    # Combine raw name
    for (i in seq_len(length(name))) {
        if (i == 1) {
            allData <- rawDataSet[[name[i]]]
        } else {
            allData <- rbind(allData, rawDataSet[[name[i]]])
        }
    }

    test_column <- c(filter_column[1], filter_column[2], filter_column[5], used_columns[["Nt.sequences"]][1])

    used_column <- c("dataName")
    for (i in seq_len(length(filter_id))) {
        used_column <- c(used_column, filter_column[filter_id[i]])
    }

    # IMPORTANT: Datasets should start with T
    # dataSetIDs <- as.list(levels(unique(allData$dataName)))

    allDataInitial <- allData
    workflow_datasets <- list()

    a <- matrix(0, length(filter_id), 3)
    for (j in seq_len(length(name))) {
        workflow_datasets[[name[j]]] <- a
    }

    # Take only the first gene (V, J D) (Separated with "or")
    a <- which(stringr::str_detect(allData[[filter_column[1]]], " or|,"))
    if (length(a) > 0) {
        a2 <- strsplit(allData[[filter_column[1]]][a], " or|,")
        allData[[filter_column[1]]][a] <- as.character(plyr::ldply(a2, function(s) {
            t(data.frame(unlist(s)))
        })[, 1])
    }

    a <- which(stringr::str_detect(allData[[filter_column[7]]], " or|,"))
    if (length(a) > 0) {
        a2 <- strsplit(allData[[filter_column[7]]][a], " or|,")
        allData[[filter_column[7]]][a] <- as.character(plyr::ldply(a2, function(s) {
            t(data.frame(unlist(s)))
        })[, 1])
    }

    if (!all(is.na(allData[[filter_column[8]]]))) {
        a <- which(stringr::str_detect(allData[[filter_column[8]]], " or|,"))
        if (length(a) > 0) {
            a2 <- strsplit(allData[[filter_column[8]]][a], " or|,")
            allData[[filter_column[8]]][a] <- as.character(plyr::ldply(a2, function(s) {
                t(data.frame(unlist(s)))
            })[, 1])
        }
    }

    # Remove (see comment) from AA.JUNCTION
    a <- which(stringr::str_detect(allData[[filter_column[2]]], " [(]see"))
    if (length(a) > 0) {
        a2 <- strsplit(allData[[filter_column[2]]][a], " [(]see")
        allData[[filter_column[2]]][a] <- as.character(plyr::ldply(a2, function(s) {
            t(data.frame(unlist(s)))
        })[, 1])
    }

    # Apply the requested filters

    if (any(filter_id == 1)) {
        i <- which(filter_id == 1)
        filterOut[[1]] <- allDataInitial %>% dplyr::filter(stringr::str_detect(allDataInitial[[filter_column[1]]], filter_out_char1))
        if (nrow(filterOut[[1]]) > 0) filterOut[[1]] <- cbind(filterOut[[1]], FilterId = cleaning_criteria[1])
        workflow[i, 3] <- nrow(allDataInitial) - nrow(filterOut[[filter_id[i]]])

        allData <- allData %>% dplyr::filter(!stringr::str_detect(allData[[filter_column[1]]], filter_out_char1))

        workflow[i, 1] <- filter_id[i]
        workflow[i, 2] <- nrow(filterOut[[filter_id[i]]])

        for (j in seq_len(length(name))) {
            if (nrow(filterOut[[1]]) > 0) {
                filterOut_datasets <- filterOut[[1]] %>% dplyr::filter(filterOut[[1]]$dataName == name[j])
            } else {
                filterOut_datasets <- filterOut[[1]]
            }
            if (i == 1) {
                prev <- nrow(rawDataSet[[name[j]]])
            } else {
                prev <- (workflow_datasets[[name[j]]][i - 1, 3])
            }
            workflow_datasets[[name[j]]][i, 3] <- prev - nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 2] <- nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 1] <- filter_id[i]
        }
    }

    if (any(filter_id == 2)) {
        i <- which(filter_id == 2)
        filterOut[[2]] <- allDataInitial %>% dplyr::filter(stringr::str_detect(allDataInitial[[filter_column[2]]], filter_out_char2))
        if (nrow(filterOut[[2]]) > 0) filterOut[[2]] <- cbind(filterOut[[2]], FilterId = cleaning_criteria[2])
        workflow[i, 3] <- nrow(allDataInitial) - nrow(filterOut[[filter_id[i]]])
        allData <- allData %>% dplyr::filter(!stringr::str_detect(allData[[filter_column[2]]], filter_out_char2))

        workflow[i, 1] <- filter_id[i]
        workflow[i, 2] <- nrow(filterOut[[filter_id[i]]])

        for (j in seq_len(length(name))) {
            if (nrow(filterOut[[2]]) > 0) {
                filterOut_datasets <- filterOut[[2]] %>% dplyr::filter(filterOut[[2]]$dataName == name[j])
            } else {
                filterOut_datasets <- filterOut[[2]]
            }
            if (i == 1) {
                prev <- nrow(rawDataSet[[name[j]]])
            } else {
                prev <- workflow_datasets[[name[j]]][i - 1, 3]
            }
            workflow_datasets[[name[j]]][i, 3] <- prev - nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 2] <- nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 1] <- filter_id[i]
        }
    }

    if (any(filter_id == 3)) {
        i <- which(filter_id == 3)

        if (Tcell) {
            filterOut[[3]] <- allData %>% dplyr::filter(!stringr::str_detect(allData[[filter_column[3]]], "^productive$"))
            allData <- allData %>% dplyr::filter(stringr::str_detect(allData[[filter_column[3]]], "^productive$"))
        } else {
            filterOut[[3]] <- allData %>% dplyr::filter(!stringr::str_detect(allData[[filter_column[3]]], "^productive"))

            allData <- allData %>% dplyr::filter(stringr::str_detect(allData[[filter_column[3]]], "^productive"))

            ins_del <- which(!is.na(allData[[filter_column[11]]]))

            if (length(ins_del) > 0) {
                # check if V-REGION deletions or V-REGION insertions contain (cause frameshift)
                delete <- allData[ins_del, ] %>% dplyr::filter(stringr::str_detect(allData[[used_columns[["Summary"]][21]]][ins_del], "[(]cause frameshift[)]"))
                delete2 <- allData[ins_del, ] %>% dplyr::filter(stringr::str_detect(allData[[used_columns[["Summary"]][22]]][ins_del], "[(]cause frameshift[)]"))
                delete_all <- unique(rbind(delete, delete2))

                filterOut[[3]] <- unique(rbind(filterOut[[3]], delete_all))
                allData <- allData %>% dplyr::filter(!(allData[[used_columns[["Summary"]][1]]] %in% filterOut[[3]][[used_columns[["Summary"]][1]]]))
            }
        }

        if (nrow(filterOut[[3]]) > 0) filterOut[[3]] <- cbind(filterOut[[3]], FilterId = cleaning_criteria[3])

        workflow[i, 3] <- nrow(allDataInitial) - nrow(filterOut[[filter_id[i]]])
        workflow[i, 1] <- filter_id[i]
        workflow[i, 2] <- nrow(filterOut[[filter_id[i]]])

        for (j in seq_len(length(name))) {
            if (nrow(filterOut[[3]]) > 0) {
                filterOut_datasets <- filterOut[[3]] %>% dplyr::filter(filterOut[[3]]$dataName == name[j])
            } else {
                filterOut_datasets <- filterOut[[3]]
            }
            if (i == 1) {
                prev <- nrow(rawDataSet[[name[j]]])
            } else {
                prev <- (workflow_datasets[[name[j]]][i - 1, 3])
            }
            workflow_datasets[[name[j]]][i, 3] <- (prev) - nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 2] <- nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 1] <- filter_id[i]
        }
    }

    if (any(filter_id == 4)) {
        i <- which(filter_id == 4)
        if (filterStart == "" && filterEnd == "") {
            filterOut[[4]] <- allDataInitial %>% dplyr::filter(!(stringr::str_detect(allDataInitial[[filter_column[4]]], filterStart)))
            workflow[i, 3] <- nrow(allDataInitial) - nrow(filterOut[[filter_id[i]]])
        } else if (filterEnd == "") {
            filterOut[[4]] <- allDataInitial %>% dplyr::filter(!(stringr::str_detect(allDataInitial[[filter_column[4]]], filterStart)))
            workflow[i, 3] <- nrow(allDataInitial) - nrow(filterOut[[filter_id[i]]])
            allData <- allData %>% dplyr::filter((stringr::str_detect(allData[[filter_column[4]]], filterStart)))
        } else if (filterStart == "") {
            filterOut[[4]] <- allDataInitial %>% dplyr::filter(!stringr::str_detect(allDataInitial[[filter_column[4]]], filterEnd))
            workflow[i, 3] <- nrow(allDataInitial) - nrow(filterOut[[filter_id[i]]])
            allData <- allData %>% dplyr::filter(stringr::str_detect(allData[[filter_column[4]]], filterEnd))
        } else {
            filterOut[[4]] <- allDataInitial %>% dplyr::filter(!(stringr::str_detect(allDataInitial[[filter_column[4]]], filterStart)))
            filterOut[[4]] <- rbind(filterOut[[4]], (allDataInitial %>% dplyr::filter(!(stringr::str_detect(allDataInitial[[filter_column[4]]], filterEnd)))))
            workflow[i, 3] <- nrow(allDataInitial) - nrow(filterOut[[filter_id[i]]])
            allData <- allData %>% dplyr::filter((stringr::str_detect(allData[[filter_column[4]]], filterStart)))
            allData <- allData %>% dplyr::filter(stringr::str_detect(allData[[filter_column[4]]], filterEnd))
        }
        if (nrow(filterOut[[4]]) > 0) filterOut[[4]] <- cbind(filterOut[[4]], FilterId = cleaning_criteria[4])


        workflow[i, 1] <- filter_id[i]
        workflow[i, 2] <- nrow(filterOut[[filter_id[i]]])

        for (j in seq_len(length(name))) {
            if (nrow(filterOut[[4]]) > 0) {
                filterOut_datasets <- filterOut[[4]] %>% dplyr::filter(filterOut[[4]]$dataName == name[j])
            } else {
                filterOut_datasets <- filterOut[[4]]
            }
            if (i == 1) {
                prev <- nrow(rawDataSet[[name[j]]])
            } else {
                prev <- workflow_datasets[[name[j]]][i - 1, 3]
            }
            workflow_datasets[[name[j]]][i, 3] <- nrow(allData %>% dplyr::filter(allData$dataName == name[j]))
            workflow_datasets[[name[j]]][i, 2] <- prev - nrow(allData %>% dplyr::filter(allData$dataName == name[j]))
            workflow_datasets[[name[j]]][i, 1] <- filter_id[i]
        }
    }

    filterOutSum <- c()
    if (length(filter_id) > 0) {
        for (i in seq_len(length(filter_id))) {
            if (i == 1) {
                filterOutSum <- filterOut[[filter_id[1]]]
            } else {
                filterOutSum <- rbind(filterOutSum, filterOut[[filter_id[i]]])
            }
        }

        # Handle conflictions
        if (nrow(filterOutSum) > 0) {
            filterOutSum <- aggregate(filterOutSum[, c(1, 3:ncol(filterOutSum))], list(filterOutSum[, 2]), function(x) paste0(unique(x)))
            colnames(filterOutSum)[1] <- used_columns[["Summary"]][1]
            filterOutSum <- filterOutSum[, c(2, 1, 3:ncol(filterOutSum))]
        }
    }

    a <- name[1]
    if (length(name) > 1) {
        for (i in 2:length(name)) {
            a <- paste0(a, ", ", name[i])
        }
    }

    b <- filter_id[1]
    if (length(filter_id) > 1) {
        for (i in 2:length(filter_id)) {
            b <- paste0(b, ", ", filter_id[i])
        }
    }

    # Separate allData to different tables
    initial_datasets <- list()
    for (i in seq_len(length(name))) {
        initial_datasets[[name[i]]] <- allDataInitial %>% dplyr::filter(allDataInitial$dataName == name[i])
    }

    cleaned_datasets <- list()
    if (length(allData) > 0) {
        for (i in seq_len(length(name))) {
            cleaned_datasets[[name[i]]] <- allData %>% dplyr::filter(allData$dataName == name[i])
        }
    }

    cleaned_out_datasets <- list()
    if (length(filterOutSum) > 0) {
        for (i in seq_len(length(name))) {
            cleaned_out_datasets[[name[i]]] <- filterOutSum %>% dplyr::filter(filterOutSum$dataName == name[i])
        }
    }

    confirm <- paste0("Datasets cleaned: ", a, ". Cleaning Filters applied: ", b)

    # log time end and memory used
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
    # cat(pryr::mem_used(), file = logFile, append = TRUE, sep = "\n")

    result <- list("message" = "", "dim" = dim(allData), "workflow" = workflow, "workflow_datasets" = workflow_datasets, "allDataInitial" = allDataInitial, "allData" = allData, "filterOutSum" = filterOutSum, "initial_datasets" = initial_datasets, "cleaned_datasets" = cleaned_datasets, "cleaned_out_datasets" = cleaned_out_datasets, "confirm" = confirm)
    return(result)
}

######################################################################################################################################

imgtfilterLow <- function(rawDataSet, name, allData, cell_id = 1, filter_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), filter_out_char1 = " P", filter_out_char2 = "[:punct:]|X", filter_in_char = "productive", filterStart = "^*", filterEnd = "*$", identityLow = 95, identityHigh = 100, VGene = "", JGene = "", DGene = "", lengthLow = 7, lengthHigh = 15, aminoacid = "CASSPPDTGELFF", seq1 = 1, seq2 = 2) {
    used_columns <- e$used_columns
    
    
    # logfile
    # logFile<-e$logFile
    a <- "Filter ids "
    for (i in seq_len(length(filter_id))) {
        a <- paste0(a, ",", filter_id[i])
    }
    # cat(paste0("imgtfilter", "\t"), file = logFile, append = TRUE)
    # cat(paste0(a, "\t"), file = logFile, append = TRUE)
    # cat(paste0(nrow(allData), "\t"), file = logFile, append = TRUE)
    # cat(paste0(ncol(rawDataSet[[names(rawDataSet)[1]]]), "\t"), file = logFile, append = TRUE)
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)

    cleaning_criteria <- c("Functional V-Gene", "CDR3 with no Special Characters", "Productive Sequence", "Productive Sequences")
    filtering_criteria <- c("V-REGION identity %", "Specific V Gene", "Specific J Gene", "Specific D Gene", "CDR3 length range", "CDR3 length range")
    criteria <- c(cleaning_criteria, filtering_criteria)

    # filter_column contains the columns that are used for each one of the 9 filters with ids=1:9
    filter_column <- c(used_columns[["Summary"]][3], used_columns[["Summary"]][18], used_columns[["Summary"]][2], used_columns[["Summary"]][18], used_columns[["Summary"]][4], used_columns[["Summary"]][3], used_columns[["Summary"]][8], used_columns[["Summary"]][11], used_columns[["Summary"]][15], used_columns[["Summary"]][18], "Summary.V.REGION.identity....with.ins.del.events.")
    filterOut <- list()
    workflow <- matrix(0, length(filter_id), 3)

    # Log start time and memory currently used
    #start.time <- Sys.time()
    # mem_used()

    test_column <- c(filter_column[1], filter_column[2], filter_column[5], used_columns[["Nt.sequences"]][1])

    used_column <- c("dataName")
    for (i in seq_len(length(filter_id))) {
        used_column <- c(used_column, filter_column[filter_id[i]])
    }

    # IMPORTANT: Datasets should start with T
    # dataSetIDs <- as.list(levels(unique(allData$dataName)))
    
    allDataInitial <- allData

    workflow_datasets <- list()

    a <- matrix(0, length(filter_id), 3)
    for (j in seq_len(length(name))) {
        workflow_datasets[[name[j]]] <- a
    }

    # Apply the requested filters

    if (any(filter_id == 5)) {
        i <- which(filter_id == 5)
        ins_del <- which(!is.na(allData[[filter_column[11]]]))

        if (length(ins_del) > 0) {
            allData[[filter_column[5]]][ins_del] <- allData[[filter_column[11]]][ins_del]
        }

        filterOut[[5]] <- allDataInitial[which(allDataInitial[[filter_column[5]]] > identityHigh | allDataInitial[[filter_column[5]]] < identityLow), ]
        allData <- allData[which(allData[[filter_column[5]]] <= identityHigh & allData[[filter_column[5]]] >= identityLow), ]

        if (nrow(filterOut[[5]]) > 0) filterOut[[5]] <- cbind(filterOut[[5]], FilterId = criteria[5])
        workflow[i, 3] <- nrow(allDataInitial) - nrow(filterOut[[filter_id[i]]])

        workflow[i, 1] <- filter_id[i] - 4
        workflow[i, 2] <- nrow(filterOut[[filter_id[i]]])

        for (j in seq_len(length(name))) {
            if (nrow(filterOut[[5]]) > 0) {
                filterOut_datasets <- filterOut[[5]] %>% dplyr::filter(filterOut[[5]]$dataName == name[j])
            } else {
                filterOut_datasets <- filterOut[[5]]
            }
            if (i == 1) {
                prev <- nrow(rawDataSet[[name[j]]])
            } else {
                prev <- workflow_datasets[[name[j]]][i - 1, 3]
            }
            workflow_datasets[[name[j]]][i, 3] <- (prev) - nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 2] <- nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 1] <- filter_id[i] - 4
        }
    }

    if (any(filter_id == 6)) {
        i <- which(filter_id == 6)
        filterOut[[6]] <- allDataInitial %>% dplyr::filter(!stringr::str_detect(allDataInitial[[filter_column[6]]], gsub("[(]", "[(]", gsub("[)]", "[)]", gsub("[*]", "[*]", VGene)))))
        if (nrow(filterOut[[6]]) > 0) filterOut[[6]] <- cbind(filterOut[[6]], FilterId = criteria[6])
        workflow[i, 3] <- nrow(allDataInitial) - nrow(filterOut[[filter_id[i]]])
        allData <- allData %>% dplyr::filter(stringr::str_detect(allData[[filter_column[6]]], gsub("[(]", "[(]", gsub("[)]", "[)]", gsub("[*]", "[*]", VGene)))))

        workflow[i, 1] <- filter_id[i] - 4
        workflow[i, 2] <- nrow(filterOut[[filter_id[i]]])

        for (j in seq_len(length(name))) {
            if (nrow(filterOut[[6]]) > 0) {
                filterOut_datasets <- filterOut[[6]] %>% dplyr::filter(filterOut[[6]]$dataName == name[j])
            } else {
                filterOut_datasets <- filterOut[[6]]
            }
            if (i == 1) {
                prev <- nrow(rawDataSet[[name[j]]])
            } else {
                prev <- workflow_datasets[[name[j]]][i - 1, 3]
            }
            workflow_datasets[[name[j]]][i, 3] <- (prev) - nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 2] <- nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 1] <- filter_id[i] - 4
        }
    }

    if (any(filter_id == 7)) {
        i <- which(filter_id == 7)
        filterOut[[7]] <- allDataInitial %>% dplyr::filter(!stringr::str_detect(allDataInitial[[filter_column[7]]], gsub("[(]", "[(]", gsub("[)]", "[)]", gsub("[*]", "[*]", JGene)))))
        if (nrow(filterOut[[7]]) > 0) filterOut[[7]] <- cbind(filterOut[[7]], FilterId = criteria[7])
        workflow[i, 3] <- nrow(allDataInitial) - nrow(filterOut[[filter_id[i]]])
        allData <- allData %>% dplyr::filter(stringr::str_detect(allData[[filter_column[7]]], gsub("[(]", "[(]", gsub("[)]", "[)]", gsub("[*]", "[*]", JGene)))))

        workflow[i, 1] <- filter_id[i] - 4
        workflow[i, 2] <- nrow(filterOut[[filter_id[i]]])

        for (j in seq_len(length(name))) {
            if (nrow(filterOut[[7]]) > 0) {
                filterOut_datasets <- filterOut[[7]] %>% dplyr::filter(filterOut[[7]]$dataName == name[j])
            } else {
                filterOut_datasets <- filterOut[[7]]
            }
            if (i == 1) {
                prev <- nrow(rawDataSet[[name[j]]])
            } else {
                prev <- workflow_datasets[[name[j]]][i - 1, 3]
            }
            workflow_datasets[[name[j]]][i, 3] <- (prev) - nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 2] <- nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 1] <- filter_id[i] - 4
        }
    }

    if (any(filter_id == 8)) {
        i <- which(filter_id == 8)
        filterOut[[8]] <- allDataInitial %>% dplyr::filter(!stringr::str_detect(allDataInitial[[filter_column[8]]], gsub("[(]", "[(]", gsub("[)]", "[)]", gsub("[*]", "[*]", DGene)))))
        if (nrow(filterOut[[8]]) > 0) filterOut[[8]] <- cbind(filterOut[[8]], FilterId = criteria[8])
        workflow[i, 3] <- nrow(allDataInitial) - nrow(filterOut[[filter_id[i]]])
        allData <- allData %>% dplyr::filter(stringr::str_detect(allData[[filter_column[8]]], gsub("[(]", "[(]", gsub("[)]", "[)]", gsub("[*]", "[*]", DGene)))))

        workflow[i, 1] <- filter_id[i] - 4
        workflow[i, 2] <- nrow(filterOut[[filter_id[i]]])

        for (j in seq_len(length(name))) {
            if (nrow(filterOut[[8]]) > 0) {
                filterOut_datasets <- filterOut[[8]] %>% dplyr::filter(filterOut[[8]]$dataName == name[j])
            } else {
                filterOut_datasets <- filterOut[[8]]
            }
            if (i == 1) {
                prev <- nrow(rawDataSet[[name[j]]])
            } else {
                prev <- workflow_datasets[[name[j]]][i - 1, 3]
            }
            workflow_datasets[[name[j]]][i, 3] <- (prev) - nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 2] <- nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 1] <- filter_id[i] - 4
        }
    }

    if (any(filter_id == 9)) {
        i <- which(filter_id == 9)
        filterOut[[9]] <- allDataInitial[which(as.numeric(allDataInitial[[filter_column[9]]]) > lengthHigh | as.numeric(allDataInitial[[filter_column[9]]]) < lengthLow), ]
        if (nrow(filterOut[[9]]) > 0) filterOut[[9]] <- cbind(filterOut[[9]], FilterId = criteria[9])
        allData <- allData[which(as.numeric(allData[[filter_column[9]]]) <= lengthHigh & as.numeric(allData[[filter_column[9]]]) >= lengthLow), ]

        workflow[i, 1] <- filter_id[i] - 4
        workflow[i, 2] <- nrow(filterOut[[filter_id[i]]])
        workflow[i, 3] <- nrow(allDataInitial) - nrow(filterOut[[filter_id[i]]])

        for (j in seq_len(length(name))) {
            if (nrow(filterOut[[9]]) > 0) {
                filterOut_datasets <- filterOut[[9]] %>% dplyr::filter(filterOut[[9]]$dataName == name[j])
            } else {
                filterOut_datasets <- filterOut[[9]]
            }
            if (i == 1) {
                prev <- nrow(rawDataSet[[name[j]]])
            } else {
                prev <- workflow_datasets[[name[j]]][i - 1, 3]
            }
            workflow_datasets[[name[j]]][i, 3] <- (prev) - nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 2] <- nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 1] <- filter_id[i] - 4
        }
    }

    if (any(filter_id == 10)) {
        i <- which(filter_id == 10)
        filterOut[[10]] <- allDataInitial %>% dplyr::filter(!stringr::str_detect(allDataInitial[[filter_column[10]]], aminoacid))
        if (nrow(filterOut[[10]]) > 0) filterOut[[10]] <- cbind(filterOut[[10]], FilterId = criteria[10])
        workflow[i, 3] <- nrow(allDataInitial) - nrow(filterOut[[filter_id[i]]])
        allData <- allData %>% dplyr::filter(stringr::str_detect(allData[[filter_column[10]]], aminoacid))

        workflow[i, 1] <- filter_id[i] - 4
        workflow[i, 2] <- nrow(filterOut[[filter_id[i]]])

        for (j in seq_len(length(name))) {
            if (nrow(filterOut[[10]]) > 0) {
                filterOut_datasets <- filterOut[[10]] %>% dplyr::filter(filterOut[[10]]$dataName == name[j])
            } else {
                filterOut_datasets <- filterOut[[10]]
            }
            if (i == 1) {
                prev <- nrow(rawDataSet[[name[j]]])
            } else {
                prev <- workflow_datasets[[name[j]]][i - 1, 3]
            }
            workflow_datasets[[name[j]]][i, 3] <- (prev) - nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 2] <- nrow(filterOut_datasets)
            workflow_datasets[[name[j]]][i, 1] <- filter_id[i] - 4
        }
    }

    # Provide a 1_Summary.txt of the datasets

    # mem_used() # log memory used after filtering (no significant change should be evident)
    filterOutSum <- c()
    if (length(filter_id) > 0) {
        for (i in seq_len(length(filter_id))) {
            if (i == 1) {
                filterOutSum <- filterOut[[filter_id[1]]]
            } else {
                filterOutSum <- rbind(filterOutSum, filterOut[[filter_id[i]]])
            }
        }
        # Handle conflictions
        if (nrow(filterOutSum) > 0) {
            filterOutSum <- aggregate(filterOutSum[, c(1, 3:ncol(filterOutSum))], list(filterOutSum[, 2]), function(x) paste0(unique(x)))
            colnames(filterOutSum)[1] <- used_columns[["Summary"]][1]
            filterOutSum <- filterOutSum[, c(2, 1, 3:ncol(filterOutSum))]
        }
    }

    # Do the actual analysis - targeted tables

    a <- name[1]
    if (length(name) > 1) {
        for (i in 2:length(name)) {
            a <- paste0(a, ", ", name[i])
        }
    }

    b <- filter_id[1] - 4
    if (length(filter_id) > 1) {
        for (i in 2:length(filter_id)) {
            b <- paste0(b, ", ", (filter_id[i] - 4))
        }
    }

    # Delete (see comment) from genes
    a <- which(stringr::str_detect(allData[[used_columns[["Summary"]][3]]], " or|,| [(]see"))
    if (length(a) > 0) {
        a2 <- strsplit(allData[[used_columns[["Summary"]][3]]][a], " or|,| [(]see")
        allData[[used_columns[["Summary"]][3]]][a] <- as.character(plyr::ldply(a2, function(s) {
            t(data.frame(unlist(s)))
        })[, 1])
    }

    a <- which(stringr::str_detect(allData[[used_columns[["Summary"]][8]]], " or|,| [(]see"))
    if (length(a) > 0) {
        a2 <- strsplit(allData[[used_columns[["Summary"]][8]]][a], " or|,| [(]see")
        allData[[used_columns[["Summary"]][8]]][a] <- as.character(plyr::ldply(a2, function(s) {
            t(data.frame(unlist(s)))
        })[, 1])
    }

    if (!all(is.na(allData[[used_columns[["Summary"]][11]]]))) {
        a <- which(stringr::str_detect(allData[[used_columns[["Summary"]][11]]], " or|,| [(]see"))
        if (length(a) > 0) {
            a2 <- strsplit(allData[[used_columns[["Summary"]][11]]][a], " or|,| [(]see")
            allData[[used_columns[["Summary"]][11]]][a] <- as.character(plyr::ldply(a2, function(s) {
                t(data.frame(unlist(s)))
            })[, 1])
        }
    }

    # Separate allData to different tables
    initial_datasets <- list()
    for (i in seq_len(length(name))) {
        initial_datasets[[name[i]]] <- allDataInitial %>% dplyr::filter(allDataInitial$dataName == name[i])
    }

    filtered_datasets <- list()
    if (length(allData) > 0) {
        for (i in seq_len(length(name))) {
            filtered_datasets[[name[i]]] <- allData %>% dplyr::filter(allData$dataName == name[i])
        }
    }

    filtered_out_datasets <- list()
    if (length(filterOutSum) > 0) {
        for (i in seq_len(length(name))) {
            filtered_out_datasets[[name[i]]] <- filterOutSum %>% dplyr::filter(filterOutSum$dataName == name[i])
        }
    }

    confirm <- paste0("Datasets filtered: ", a, ". Filters applied: ", b)

    # log time end and memory used
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
    # cat(pryr::mem_used(), file = logFile, append = TRUE, sep = "\n")
    
    result <- list("message" = "", "dim" = dim(allData), "workflow" = workflow, "workflow_datasets" = workflow_datasets, "allDataInitial" = allDataInitial, "allData" = allData, "filterOutSum" = filterOutSum, "initial_datasets" = initial_datasets, "filtered_datasets" = filtered_datasets, "filtered_out_datasets" = filtered_out_datasets, "confirm" = confirm)
    return(result)
}

######################################################################################################################################

clonotypes <- function(
    allData, 
    allele, 
    gene, 
    junction,
    name, 
    run_diagnosis,
    identity_groups,
    run_sub_clono,
    N_clono_cutoff,
    Freq_clono_cutoff
) {
  
  used_columns <- e$used_columns
  # save(allData,
  #      allele,
  #      gene,
  #      junction,
  #      name,
  #      run_diagnosis,
  #      used_columns,
  #      file = './ONLY_CDR3Clonotypes.RData')
  # logfile
  # logFile<-e$logFile
  message("Clonotype execution started: ")
  
  g = ifelse(allele, gene, stringr::str_replace(gene, ".and.allele", ""))
  
  # cat(paste0("clonotypes", "\t"), file = logFile, append = TRUE)
  # cat(paste0(paste(g, junction, sep = ","), "\t"), file = logFile, append = TRUE)
  # cat(paste0(nrow(allData), "\t"), file = logFile, append = TRUE)
  # cat(paste0(ncol(allData), "\t"), file = logFile, append = TRUE)
  # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
 
  # clonotypes all Data
  allData                           <- setDT(allData)
  
  if (junction == "IMGT.gapped.nt.sequences.V.D.J.REGION") {
    allData$IMGT.gapped.nt.sequences.FR4.IMGT <- stringr::str_pad(allData$IMGT.gapped.nt.sequences.FR4.IMGT, width = max(nchar(allData$IMGT.gapped.nt.sequences.FR4.IMGT)), side = "right", pad = ".")
    allData$new.V.REGION <- paste(allData$IMGT.gapped.nt.sequences.FR1.IMGT, allData$IMGT.gapped.nt.sequences.CDR1.IMGT, allData$IMGT.gapped.nt.sequences.FR2.IMGT, allData$IMGT.gapped.nt.sequences.CDR2.IMGT, allData$IMGT.gapped.nt.sequences.FR3.IMGT, allData$IMGT.gapped.nt.sequences.FR4.IMGT, sep = "")
  }
  
  if (junction == "IMGT.gapped.nt.sequences.FR1_CDR3") {
    allData$IMGT.gapped.nt.sequences.FR1_CDR3 <- paste(allData$IMGT.gapped.nt.sequences.FR1.IMGT, allData$IMGT.gapped.nt.sequences.CDR1.IMGT, allData$IMGT.gapped.nt.sequences.FR2.IMGT, allData$IMGT.gapped.nt.sequences.CDR2.IMGT, allData$IMGT.gapped.nt.sequences.FR3.IMGT, allData$IMGT.gapped.nt.sequences.CDR3.IMGT, sep = "")
    allData$new.V.REGION <- paste(allData$IMGT.gapped.nt.sequences.FR1.IMGT, allData$IMGT.gapped.nt.sequences.CDR1.IMGT, allData$IMGT.gapped.nt.sequences.FR2.IMGT, allData$IMGT.gapped.nt.sequences.CDR2.IMGT, allData$IMGT.gapped.nt.sequences.FR3.IMGT, sep = "")
  }
  
  if (length(name) > 1) {
    
    if (length(gene) > 0) {
      
      message("Clonotype Analysis Step 2.a")
      
      if (allele == FALSE) {
        allData$gene <- stringr::str_split(allData[[gene]], "\\*", simplify = TRUE)[, 1]
      } else {
        allData$gene <- allData[[gene]]
      }
      
      #allData[, by = c(Genes = allData$gene), N:= .N] ##ASPA28/3  CDR3 = allData[[junction]])
      
      allData$clonotype <- paste0(allData$gene, " - ", allData[[junction]])
      allData[, by = clonotype, N := .N]
      allData            <- allData[order(-N, clonotype),]
      allData$Freq       <- 100 * allData$N / nrow(allData)
      allData$cluster_id <- data.table::rleid(allData$clonotype) # prefix = "cluster "
      
      allData[, by = clonotype, 
              N.uniq.seq := length(unique(.SD[[1]])), 
              .SDcols = used_columns[["IMGT.gapped.nt.sequences"]][9]]
      
      
      allData$cluster_info <- paste0(
        "cluster ", allData$cluster_id, " : ", allData$N.uniq.seq
      )
      
      
      convergent_evolution_list_allData <- allData[, by = c(
        "cluster_id", used_columns[["IMGT.gapped.nt.sequences"]][9]
      ),.N]
      
      colnames(convergent_evolution_list_allData)[3] <- "convergent_evolution"
      
      # hold all info for each clonotype in a list
      view_specific_clonotype_allData <- split(allData, allData$clonotype)
      
      
    } else {
      
      message("Clonotype Analysis Step 2.b")
      
      allData$clonotype <- allData[[junction]]
      
      allData[, by = clonotype, N := .N] 
      
      allData <- allData[order(-N, clonotype), ]
      allData$Freq <- 100 *(allData$N / nrow(allData))
      allData$cluster_id <- data.table::rleid(allData$clonotype) 
      
      allData[, by = clonotype, 
              N.uniq.seq := length(unique(.SD[[1]])), 
              .SDcols = used_columns[["IMGT.gapped.nt.sequences"]][9]]
      
      
      allData$cluster_info <- paste0(
        "cluster ", allData$cluster_id, " : ", allData$N.uniq.seq
      )
      
      convergent_evolution_list_allData <- allData[, by = c(
        "cluster_id", used_columns[["IMGT.gapped.nt.sequences"]][9]
      ), .N]
      
      colnames(convergent_evolution_list_allData)[3] <- "convergent_evolution"
      
      # hold all info for each clonotype in a list
      view_specific_clonotype_allData <- split(allData, allData$clonotype)
      
    }
    
    clono_allData <- unique(allData[,c("clonotype", "N", "Freq", "cluster_info", "Junction.pI")])
    clono_allData <- clono_allData[!duplicated(clono_allData[, c("clonotype", "N", "Freq", "cluster_info")])]
    colnames(clono_allData) <- c("clonotype", "N", "Freq", "Convergent Evolution", "pI")
    
    if (junction == "IMGT.gapped.nt.sequences.V.D.J.REGION" | junction == "IMGT.gapped.nt.sequences.FR1_CDR3") {
      clono_allData <- unique(allData[,c(
        "clonotype", "N", "Freq", "cluster_id", 
        "Summary.V.GENE.and.allele", "Summary.AA.JUNCTION", "Summary.CDR3.IMGT.length", "IMGT.gapped.nt.sequences.CDR3.IMGT", "new.V.REGION", "Summary.V.REGION.identity..", "Junction.pI"
      )])
      clono_allData <- clono_allData[!duplicated(clono_allData[, c("clonotype", "N", "Freq", "cluster_id")])]
      colnames(clono_allData)[4:11] <- c("Clonotype ID", "V Gene and allele", "AA Junction", "AA Junction length", "nt Junction", "new.V.REGION", "V.REGION.identity", "pI")
    }
    
    
    if (save_tables_individually) {
      if (junction == "IMGT.gapped.nt.sequences.V.D.J.REGION" | junction == "IMGT.gapped.nt.sequences.FR1_CDR3") {
        colnames(clono_allData) 
        clono_allData <- clono_allData[, .(`Clonotype ID`, `V Gene and allele`, `AA Junction`, `AA Junction length`, N, Freq, V.REGION.identity, pI)]
        clono_allData <- clono_allData[, `V Gene and allele` := stringr::str_replace(`V Gene and allele`, "^Homsap ", "")]
        clono_write <- clono_allData
      } else {
        ## Changed cbind to merge
        if (length(gene) > 0) {
          gene_cdr3 <- unique(allData[,c('gene',"Summary.AA.JUNCTION")])   #gene ##CHANGE
          clono_write <- cbind(gene_cdr3,clono_allData[, c("N", "Freq", "Convergent Evolution", "pI")])
          colnames(clono_write)[1:2] <- c('Genes',"CDR3")
        } else{
          clono_write <- clono_allData
        }
      }
      
      
      if (Freq_clono_cutoff != 0) {
        clono_allData <- clono_allData %>% filter(Freq > Freq_clono_cutoff)
        clono_allData$Freq  <- 100 * clono_allData$N / sum(clono_allData$N)
        
        clono_write <- clono_write %>% filter(Freq > Freq_clono_cutoff)
        clono_write$Freq  <- 100 * clono_write$N / sum(clono_write$N)
      }
      
      if (N_clono_cutoff != 0) {
        clono_allData <- clono_allData %>% filter(N > N_clono_cutoff)
        clono_allData$Freq  <- 100 * clono_allData$N / sum(clono_allData$N)
        
        clono_write <- clono_write %>% filter(N > N_clono_cutoff)
        clono_write$Freq  <- 100 * clono_write$N / sum(clono_write$N)
      }
      
      
      fwrite(clono_write,
             file = paste0(e$output_folder, "/", "Clonotypes_All_Data", ".txt"),
             row.names = FALSE,
             col.names = TRUE,
             quote = FALSE,
             sep = "\t"
      )
      
      fwrite(allData,
             file = paste0(e$output_folder, "/", "filterin_clono_All_Data", ".txt"),
             sep = "\t",
             row.names = FALSE,
             col.names = TRUE,
             quote = FALSE
      )
      
      
      
    }
  }
  
  # clonotypes datasets
  
  clono_datasets                              <- list()
  clono_datasets_sub                          <- list()
  filterin_clono_datasets                     <- list()
  view_specific_clonotype_datasets            <- list()
  convergent_evolution_list_datasets          <- list()
  convergent_evolution_list_datasets_only_num <- list()
  diagnosis                                   <- list()
  group.freq.seq                              <- list()
  vregion_status                              <- list()
  div_clono_datasets                          <- list()
  
  message("Clonotype Analysis Step 3")

  one_run <- function(j) {
    message("Clonotype Analysis Step 5")
    
    
    data <- allData[allData$dataName == name[j]]
    
    view_specific_clonotype_datasets[[name[j]]] <- list()
    convergent_evolution_list_datasets[[name[j]]] <- list()
    convergent_evolution_list_datasets_only_num[[name[j]]] <- c()
    group.freq.seq[[name[j]]] <- data
    
    if (length(gene) > 0) {
      
      if (allele == FALSE) {
        data$gene <- stringr::str_split(data[[gene]], "\\*", simplify = TRUE)[, 1]
      } else {
        data$gene <- data[[gene]]
      }
      
      group.freq.seq[[name[j]]]$clonotype <- paste(group.freq.seq[[name[j]]][[gene]],
                                                   group.freq.seq[[name[j]]][[junction]],
                                                   sep = " - "
      )
      
      group.freq.seq[[name[j]]] <- data.table::as.data.table(group.freq.seq[[name[j]]])
      
      data$clonotype <- paste0(data$gene," - ",data[[junction]])
      data[, by = clonotype, N:=.N]
      data <- data[order(-N, clonotype),]
      data$Freq       <- 100 * data$N / nrow(data)
      data$cluster_id <- data.table::rleid(data$clonotype) 
      
      data[, by = clonotype, 
           N.uniq.seq := length(unique(.SD[[1]])), 
           .SDcols = used_columns[["IMGT.gapped.nt.sequences"]][9]]
      
      
      data$cluster_info <- paste0(
        "cluster ", data$cluster_id, " : ", data$N.uniq.seq
      )
      
      convergent_evolution_list_datasets[[name[j]]] <- data[, by = c(
        "cluster_id", used_columns[["IMGT.gapped.nt.sequences"]][9]
      ), .N]
      
      colnames(convergent_evolution_list_datasets[[name[j]]]) = c(
        "cluster_id", 
        used_columns[["IMGT.gapped.nt.sequences"]][9], 
        "convergent_evolution"
      )
      convergent_evolution_list_datasets_only_num[[name[j]]] <- 
        c(convergent_evolution_list_datasets_only_num[[name[j]]], 
          convergent_evolution_list_datasets[[name[j]]]$convergent_evolution)
      
      view_specific_clonotype_datasets[[name[j]]] <- split(data, data$clonotype)
      
    } else {
      
      data$clonotype <- data[[junction]]
      data[, by = clonotype, N := .N] 
      data <- data[order(-N, clonotype), ]
      data$Freq = 100 *(data$N / nrow(data))
      data$cluster_id = data.table::rleid(data$clonotype) 
      
      data[, by = clonotype, 
           N.uniq.seq := length(unique(.SD[[1]])), 
           .SDcols = used_columns[["IMGT.gapped.nt.sequences"]][9]]
      
      
      data$cluster_info = paste0(
        "cluster ", data$cluster_id, " : ", data$N.uniq.seq
      )
      
      convergent_evolution_list_datasets[[name[j]]] <- data[, by = c(
        "cluster_id", used_columns[["IMGT.gapped.nt.sequences"]][9]
      ), .N]
      
      colnames(convergent_evolution_list_datasets[[name[j]]]) = c(
        "cluster_id", 
        used_columns[["IMGT.gapped.nt.sequences"]][9], 
        "convergent_evolution"
      )
      convergent_evolution_list_datasets_only_num[[name[j]]] <- 
        c(convergent_evolution_list_datasets_only_num[[name[j]]], 
          convergent_evolution_list_datasets[[name[j]]]$convergent_evolution)
      
      # hold all info for each clonotype in a list
      view_specific_clonotype_datasets[[name[j]]] = split(data, data$clonotype)
    }
    
    clono_datasets[[name[j]]] <- unique(data[, c("clonotype", "N", "Freq", "cluster_info", "Junction.pI")])
    clono_datasets[[name[j]]] <- clono_datasets[[name[j]]][!duplicated(clono_datasets[[name[j]]][, c("clonotype", "N", "Freq", "cluster_info")])]
    colnames(clono_datasets[[name[j]]])[4] <- "Convergent Evolution"
    colnames(clono_datasets[[name[j]]])[5] <- "pI"
    
    div_clono_datasets[[name[j]]] <- unique(data[, c("clonotype", "N", "Freq", "cluster_info", "Summary.AA.JUNCTION")])
    div_clono_datasets[[name[j]]] <- div_clono_datasets[[name[j]]][, c("N", "Freq", "Summary.AA.JUNCTION")]
    colnames(div_clono_datasets[[name[j]]]) <- c("Clones", "Proportion", "CDR3.aa")
    
    if (junction == "IMGT.gapped.nt.sequences.V.D.J.REGION" | junction == "IMGT.gapped.nt.sequences.FR1_CDR3") {
      
      clono_datasets[[name[j]]] <- unique(data[,
                                               c("clonotype", "N", "Freq", 
                                                 "cluster_id", 
                                                 "Summary.V.GENE.and.allele", 
                                                 "Summary.AA.JUNCTION", "Summary.CDR3.IMGT.length", "IMGT.gapped.nt.sequences.CDR3.IMGT", "new.V.REGION", "Summary.V.REGION.identity..", "Junction.pI")])
      clono_datasets[[name[j]]] <- clono_datasets[[name[j]]][!duplicated(clono_datasets[[name[j]]][, c("clonotype", "N", "Freq", "cluster_id")])]
      colnames(clono_datasets[[name[j]]])[4:11] <- c("Clonotype ID", "V Gene and allele", "AA Junction", "AA Junction length", "nt Junction", "new.V.REGION", "V.REGION.identity", "pI")
      
      div_clono_datasets[[name[j]]] <- clono_datasets[[name[j]]]
      div_clono_datasets[[name[j]]] <- div_clono_datasets[[name[j]]][, c("N", "Freq", "AA Junction")]
      colnames(div_clono_datasets[[name[j]]]) <- c("Clones", "Proportion", "CDR3.aa")
    }
    
    if (run_diagnosis) {
      index = colnames(group.freq.seq[[name[j]]]) |> 
        stringr::str_detect("V.GENE.and.allele|V.REGION.identity|AA.JUNCTION") |>
        which()
      
      group.freq.seq[[name[j]]] = group.freq.seq[[name[j]]][, index, with = FALSE]
      
      index = colnames(group.freq.seq[[name[j]]]) |> 
        stringr::str_detect("with.ins.del.events") |>
        which()
      
      group.freq.seq[[name[j]]] = group.freq.seq[[name[j]]][, -index, with = FALSE]
      index = colnames(group.freq.seq[[name[j]]]) |> sort()
      group.freq.seq[[name[j]]] = group.freq.seq[[name[j]]][, index, with = FALSE]
      colnames(group.freq.seq[[name[j]]]) = c("CDR3", "Gene", "V-Region identity")
      
      if (allele == FALSE) {
        group.freq.seq[[name[j]]]$Gene = group.freq.seq[[name[j]]]$Gene |>
          stringr::str_split("\\*|\\,") |>
          lapply(function(x) return(x[1])) |>
          unlist()
      } else {
        group.freq.seq[[name[j]]]$Gene <- group.freq.seq[[name[j]]]$Gene
      }
      
      group.freq.seq[[name[j]]]$clonotype = paste0(group.freq.seq[[name[j]]]$Gene, " - ", group.freq.seq[[name[j]]]$CDR3)
      group.freq.seq[[name[j]]]$Gene      = NULL
      group.freq.seq[[name[j]]]$CDR3      = NULL
      group.freq.seq[[name[j]]] = group.freq.seq[[name[j]]][, by = .(clonotype, `V-Region identity`), .(`No. of sequences` = .N)]
      group.freq.seq[[name[j]]] = merge(clono_datasets[[name[j]]], group.freq.seq[[name[j]]], by = "clonotype", all = TRUE)
      group.freq.seq[[name[j]]] = group.freq.seq[[name[j]]][order(-N, clonotype, -`No. of sequences`)]
      
      vregion_status[[name[j]]] = lapply(group.freq.seq[[name[j]]]$`V-Region identity`, function(z) z > identity_groups$low & z <= identity_groups$high)
      vregion_status[[name[j]]] = as.data.frame(vregion_status[[name[j]]])
      vregion_status[[name[j]]] = t(vregion_status[[name[j]]])
      vregion_status[[name[j]]] = as.data.frame(vregion_status[[name[j]]])
      colnames(vregion_status[[name[j]]]) = identity_groups$status
      
      for (i in 1:nrow(vregion_status[[name[j]]])) {
        for (z in 1:ncol(vregion_status[[name[j]]])) {
          
          if (vregion_status[[name[j]]][i, z] == TRUE) {
            
            vregion_status[[name[j]]][i, z] = colnames(vregion_status[[name[j]]])[z]
            
          } else {
            vregion_status[[name[j]]][i, z] = ""
          }
        }
      }
      
      vregion_status[[name[j]]] = tidyr::unite(vregion_status[[name[j]]], "V-Region status", sep = "")
      group.freq.seq[[name[j]]]$"V-Region status" = vregion_status[[name[j]]]
      
    }
    
    if (run_sub_clono == TRUE) {
      # Functions
      count_mismatches <- function(reference_string, strings_to_compare) {
        mismatches <- numeric(length(strings_to_compare))
        if (nchar(reference_string) == 0) {
          stop("Reference string cannot be empty")
        }
        for (i in seq_along(strings_to_compare)) {
          string <- strings_to_compare[i]
          if (nchar(reference_string) != nchar(string)) {
            stop("Strings must be of equal length")
          }
          count <- 0
          for (y in 1:nchar(reference_string)) {
            if (substr(reference_string, y, y) != substr(string, y, y)) {
              count <- count + 1
            }
          }
          mismatches[i] <- count
        }
        return(mismatches)
      }
      apply_count_mismatches <- function(data) {
        if (nrow(data)>1) {
          mismatches <- c("-", count_mismatches(data[1, "new.V.REGION"], data[2:nrow(data), "new.V.REGION"]))
        } else {
          mismatches <- "Not applicable"
        }
        return(mismatches)
      }
      add_sub_cluster_id <- function(df, sub_cluster_id) {
        df$"Sub-clonotypes cluster ID" <- "" 
        df$"Sub-clonotypes cluster ID"[1] <- sub_cluster_id
        return(df)
      }
      add_empty_row <- function(df) {
        empty_row <- as.data.frame(lapply(df, function(x) NA))
        names(empty_row) <- names(df)
        rbind(empty_row, df)
      }
      sum_Ν <- function(df) {
        total_sum <- sum(as.numeric(df$N), na.rm = TRUE)
        df[1, "N"] <- total_sum
        return(df)
      }
      sum_Freq <- function(df) {
        total_sum <- sum(as.numeric(df$Freq), na.rm = TRUE)
        df[1, "Freq"] <- total_sum
        return(df)
      }
      aa_junction_fun <- function(df) {
        levels <- levels(as.factor(df$`AA Junction`))
        df[1, "AA Junction"] <- paste(levels)
        return(df)
      }
      aa_junction_length_fun <- function(df) {
        levels <- levels(as.factor(df$`AA Junction length`))
        df[1, "AA Junction length"] <- paste(levels)
        return(df)
      }
      V.gene_fun <- function(df) {
        levels <- levels(as.factor(df$V.gene))
        df[1, "V.gene"] <- paste(levels)
        return(df)
      }
      # Function to modify data frames with exactly 2 rows
      modify_dfs <- function(df_list) {
        for (i in seq_along(df_list)) {
          df <- df_list[[i]]
          if (nrow(df) == 2) {
            col_names <- names(df)
            df_list[[i]] <- data.frame(tail(df, 1))
            names(df_list[[i]]) <- col_names
            df_list[[i]][, ncol(df)] <- head(df[, ncol(df)], 1)
          }
        }
        return(df_list)
      }
      
      clono_datasets_sub[[name[j]]] <- setDF(clono_datasets[[name[j]]])
      
      if (Freq_clono_cutoff != 0) {
        clono_datasets_sub[[name[j]]] <- clono_datasets_sub[[name[j]]] %>% filter(Freq > Freq_clono_cutoff)
        clono_datasets_sub[[name[j]]]$Freq  <- 100 * clono_datasets_sub[[name[j]]]$N / sum(clono_datasets_sub[[name[j]]]$N)
      }
      
      if (N_clono_cutoff != 0) {
        clono_datasets_sub[[name[j]]] <- clono_datasets_sub[[name[j]]] %>% filter(N > N_clono_cutoff)
        clono_datasets_sub[[name[j]]]$Freq  <- 100 * clono_datasets_sub[[name[j]]]$N / sum(clono_datasets_sub[[name[j]]]$N)
      }
      
      clono_datasets_sub[[name[j]]]$V.gene <- (as.data.frame(t(as.data.frame(strsplit(clono_datasets_sub[[name[j]]]$`V Gene and allele`, "\\*")))))$V1
      clono_datasets_sub[[name[j]]]$sub_filter <- paste(clono_datasets_sub[[name[j]]]$V.gene, clono_datasets_sub[[name[j]]]$`nt Junction`)
      clono_datasets_sub[[name[j]]] <- split(clono_datasets_sub[[name[j]]], clono_datasets_sub[[name[j]]]$sub_filter)
      num_rows <- sapply(clono_datasets_sub[[name[j]]], function(x) nrow(x))
      clono_datasets_sub[[name[j]]] <- clono_datasets_sub[[name[j]]][order(-num_rows)]
      clono_datasets_sub[[name[j]]] <- lapply(clono_datasets_sub[[name[j]]], function(x) {
        x$mismatches <- apply_count_mismatches(x)
        return(x)
      })
      
      clono_datasets_sub[[name[j]]] <- lapply(clono_datasets_sub[[name[j]]], add_empty_row)
      clono_datasets_sub[[name[j]]] <- lapply(clono_datasets_sub[[name[j]]], sum_Ν)
      clono_datasets_sub[[name[j]]] <- lapply(clono_datasets_sub[[name[j]]], sum_Freq)
      clono_datasets_sub[[name[j]]] <- lapply(clono_datasets_sub[[name[j]]], aa_junction_fun)
      clono_datasets_sub[[name[j]]] <- lapply(clono_datasets_sub[[name[j]]], aa_junction_length_fun)
      clono_datasets_sub[[name[j]]] <- lapply(clono_datasets_sub[[name[j]]], V.gene_fun)
      clono_datasets_sub[[name[j]]] <- clono_datasets_sub[[name[j]]][order(-(sapply(clono_datasets_sub[[name[j]]], function(df) df$Freq[1])))]
      names(clono_datasets_sub[[name[j]]]) <- paste0(1:length(clono_datasets_sub[[name[j]]]))
      for (i in seq_along(clono_datasets_sub[[name[j]]])) {
        clono_datasets_sub[[name[j]]][[i]] <- add_sub_cluster_id(clono_datasets_sub[[name[j]]][[i]], names(clono_datasets_sub[[name[j]]])[i])
      }
      
      clono_datasets_sub[[name[j]]] <- modify_dfs(clono_datasets_sub[[name[j]]])
      
      clono_datasets_sub[[name[j]]] <- rbind(clono_datasets_sub[[name[j]]])
      clono_datasets_sub[[name[j]]] <- do.call(rbind, clono_datasets_sub[[name[j]]])
      clono_datasets_sub[[name[j]]]$V.gene <- substring(clono_datasets_sub[[name[j]]]$V.gene, 8)
      clono_datasets_sub[[name[j]]] <- clono_datasets_sub[[name[j]]][, c("Sub-clonotypes cluster ID", "V.gene", "AA Junction", "AA Junction length", "N", "Freq", "V.REGION.identity", "mismatches", "Clonotype ID")]
    }
    
    if (save_tables_individually) {
      if (junction == "IMGT.gapped.nt.sequences.V.D.J.REGION" | junction == "IMGT.gapped.nt.sequences.FR1_CDR3") {
        clono_datasets[[name[j]]] <- data.table(clono_datasets[[name[j]]])
        clono_datasets[[name[j]]] <- clono_datasets[[name[j]]][, .(`Clonotype ID`, `V Gene and allele`, `AA Junction`, `AA Junction length`, N, Freq, V.REGION.identity, pI)]
        clono_datasets[[name[j]]] <- clono_datasets[[name[j]]][, `V Gene and allele` := stringr::str_replace(`V Gene and allele`, "^Homsap ", "")]
        clono_write <- clono_datasets[[name[j]]]
      } else {
        if (length(gene) > 0){
          gene_cdr3 <- unique(data[,c('gene',"Summary.AA.JUNCTION")]) #gene
          clono_write <- cbind(gene_cdr3,clono_datasets[[name[j]]][, c("N", "Freq", "Convergent Evolution", "pI")])
          colnames(clono_write)[1:2] <- c("Genes","CDR3")
        } else{
          clono_write <- clono_datasets[[name[j]]]
        }
      }
      
      if (Freq_clono_cutoff != 0) {
        clono_datasets[[name[j]]] <- clono_datasets[[name[j]]] %>% filter(Freq > Freq_clono_cutoff)
        clono_datasets[[name[j]]]$Freq  <- 100 * clono_datasets[[name[j]]]$N / sum(clono_datasets[[name[j]]]$N)
        
        clono_write <- clono_write %>% filter(Freq > Freq_clono_cutoff)
        clono_write$Freq  <- 100 * clono_write$N / sum(clono_write$N)
      }
      
      if (N_clono_cutoff != 0) {
        clono_datasets[[name[j]]] <- clono_datasets[[name[j]]] %>% filter(N > N_clono_cutoff)
        clono_datasets[[name[j]]]$Freq  <- 100 * clono_datasets[[name[j]]]$N / sum(clono_datasets[[name[j]]]$N)
        
        clono_write <- clono_write %>% filter(N > N_clono_cutoff)
        clono_write$Freq  <- 100 * clono_write$N / sum(clono_write$N)
      }
      
      # Printing clonotypes file
      
      fwrite(clono_write,
             paste0(e$output_folder, "/", "Clonotypes_", name[j], ".txt"),
             sep = "\t",
             row.names = FALSE,
             col.names = TRUE,
             quote = FALSE)
      
      # Printing whole integrated clonotypes file
      
      fwrite(data,
             paste0(e$output_folder, "/", "filterin_clono_", name[j], ".txt"),
             sep = "\t",
             row.names = FALSE,
             col.names = TRUE,
             quote = FALSE
      )
      
      
      # Printing SHM file
      
      # Printing diagnosis file
      
      if (run_diagnosis) {
        
        fwrite(group.freq.seq[[name[j]]],
               paste0(e$output_folder, "/", "vregion_status_", name[j], ".txt"),
               sep = "\t",
               row.names = FALSE,
               col.names = TRUE,
               quote = FALSE
        )
      }
      
      if (run_sub_clono) {
        
        fwrite(clono_datasets_sub[[name[j]]],
               paste0(e$output_folder, "/", "Sub_clonotypes_", name[j], ".txt"),
               sep = "\t",
               row.names = FALSE,
               col.names = TRUE,
               quote = FALSE
        )
      }
      
    }
    
    result <- list()
    
    result[["clono_datasets"]] <- clono_datasets[[name[j]]]
    result[["filterin_highly_clono"]] <- data
    result[["view_specific_clonotype_datasets"]] <- view_specific_clonotype_datasets[[name[j]]]
    result[["convergent_evolution_list_datasets"]] <- convergent_evolution_list_datasets[[name[j]]]
    result[["convergent_evolution_list_datasets_only_num"]] <- convergent_evolution_list_datasets_only_num[[name[j]]]
    result[["Diagnosis"]] <- group.freq.seq[[name[j]]]
    result[["sub_clono_datasets"]] <- clono_datasets_sub[[name[j]]]
    result[["div_clono_datasets"]] <- div_clono_datasets[[name[j]]]
    
    return(result)
  }
  
  if (Sys.info()[1] == "Windows") {
    message("Clonotype Analysis Step 4.a")
    
    a <- lapply(seq_len(length(name)), one_run)
    
    for (i in seq_len(length(name))) {
      view_specific_clonotype_datasets[[name[i]]]            <- a[[i]]$view_specific_clonotype_datasets
      clono_datasets[[name[i]]]                              <- a[[i]]$clono_datasets
      convergent_evolution_list_datasets[[name[i]]]          <- a[[i]]$convergent_evolution_list_datasets
      convergent_evolution_list_datasets_only_num[[name[i]]] <- a[[i]]$convergent_evolution_list_datasets_only_num
      diagnosis[[name[i]]]                                   <- a[[i]]$Diagnosis
      clono_datasets_sub[[name[i]]]                          <- a[[i]]$clono_datasets_sub
    }
  } else {
    message("Clonotype Analysis Step 4.b")
    
    a <- lapply(seq_len(length(name)), one_run)
    ## for debugging use lapply
    
    for (i in seq_len(length(name))) {
      view_specific_clonotype_datasets[[name[i]]]            <- a[[i]]$view_specific_clonotype_datasets
      clono_datasets[[name[i]]]                              <- a[[i]]$clono_datasets
      convergent_evolution_list_datasets[[name[i]]]          <- a[[i]]$convergent_evolution_list_datasets
      convergent_evolution_list_datasets_only_num[[name[i]]] <- a[[i]]$convergent_evolution_list_datasets_only_num
      diagnosis[[name[i]]]                                   <- a[[i]]$Diagnosis
      clono_datasets_sub[[name[i]]]                          <- a[[i]]$sub_clono_datasets
      div_clono_datasets[[name[i]]]                          <- a[[i]]$div_clono_datasets
    }
    
  }
  
  if (length(name) == 1) {
    clono_allData <- a[[1]]$clono_datasets
    convergent_evolution_list_allData <- convergent_evolution_list_datasets
    view_specific_clonotype_allData <- a[[1]]$view_specific_clonotype_datasets
    clono_allData_freq <- a[[1]]$filterin_highly_clono
    
    if (save_tables_individually) {
      
      if (junction == "IMGT.gapped.nt.sequences.V.D.J.REGION" | junction == "IMGT.gapped.nt.sequences.FR1_CDR3") {
        clono_allData <- clono_allData[, .(`Clonotype ID`, `V Gene and allele`, `AA Junction`, `AA Junction length`, N, Freq, V.REGION.identity, pI)]
        clono_allData <- clono_allData[, `V Gene and allele` := stringr::str_replace(`V Gene and allele`, "^Homsap ", "")]
        clono_write <- clono_allData
      } else {
        if (length(gene) > 0) {
        clono_write <- stringr::str_split(clono_allData$clonotype, " - ", simplify = TRUE)
        clono_write <- data.table::as.data.table(clono_write)
        clono_write <- cbind(clono_write, clono_allData[, c("N", "Freq", "Convergent Evolution", "pI")])
        colnames(clono_write) <- c("Genes", "CDR3", "N", "Freq", "Convergent Evolution", "pI") #Genes
        } else {
          clono_write <- stringr::str_split(clono_allData$clonotype, " - ", simplify = TRUE)
          clono_write <- data.table::as.data.table(clono_write)
          clono_write <- cbind(clono_write, clono_allData[, c("N", "Freq", "Convergent Evolution", "pI")])
          colnames(clono_write) <- c("clonotype", "N", "Freq", "Convergent Evolution", "pI") #Genes
      }
        } 
      
      if (Freq_clono_cutoff != 0) {
        clono_write <- clono_write %>% filter(Freq > Freq_clono_cutoff)
        clono_write$Freq  <- 100 * clono_write$N / sum(clono_write$N)
      }
      
      if (N_clono_cutoff != 0) {
        clono_write <- clono_write %>% filter(N > N_clono_cutoff)
        clono_write$Freq  <- 100 * clono_write$N / sum(clono_write$N)
      }
      
      fwrite(clono_write, paste0(e$output_folder, "/", "Clonotypes_All Data", ".txt"),
             sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE
      )
      
      fwrite(clono_allData_freq, paste0(e$output_folder, "/", "filterin_clono_All_Data", ".txt"),
             sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE
      )
    }
  }
  
  confirm <- paste0("Clonotypes run!")
  
  clono_allData <- setDF(clono_allData)
  clono_datasets <- lapply(clono_datasets, setDF)
  allData <- setDF(allData)
  view_specific_clonotype_allData <- lapply(view_specific_clonotype_allData, setDF)
  convergent_evolution_list_allData <- setDF(convergent_evolution_list_allData)
  view_specific_clonotype_datasets <- lapply(view_specific_clonotype_datasets, setDF)
  convergent_evolution_list_datasets <- lapply(convergent_evolution_list_datasets,setDF)
  diagnosis <- lapply(diagnosis, setDF)
  clono_datasets_sub <- lapply(clono_datasets_sub, setDF)
  div_clono_datasets <- lapply(div_clono_datasets, setDF)
  
  result <- list(
    "clono_allData" = clono_allData,
    "clono_datasets" = clono_datasets, 
    "filterin_highly_clono" = allData, 
    "view_specific_clonotype_allData" = view_specific_clonotype_allData, 
    "convergent_evolution_list_allData" = convergent_evolution_list_allData, 
    "view_specific_clonotype_datasets" = view_specific_clonotype_datasets, 
    "convergent_evolution_list_datasets" = convergent_evolution_list_datasets, 
    "convergent_evolution_list_datasets_only_num" = convergent_evolution_list_datasets_only_num,
    "diagnosis" = diagnosis,
    "sub_clono_datasets" = clono_datasets_sub,
    "div_clono_datasets" = div_clono_datasets,
    "confirm" = confirm
  )
  
  # log time end and memory used
  # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
  # cat(pryr::mem_used(), file = logFile, append = TRUE, sep = "\n")
  
  return(result)
}

######################################################################################################################################
meta_clonotypes <- function(clono_datasets, num_of_missmatches, cdr3_lengths, name) {

  clono_datasets_meta <- list()
  mismatches_per_length_df <- data.frame("AA Junction length" = cdr3_lengths, allowed_mismatches = num_of_missmatches)
  colnames(mismatches_per_length_df)[1] <- "AA Junction length"
  
  # Functions
  count_mismatches <- function(reference_string, strings_to_compare) {
    mismatches <- numeric(length(strings_to_compare))
    if (nchar(reference_string) == 0) {
      stop("Reference string cannot be empty")
    }
    for (i in seq_along(strings_to_compare)) {
      string <- strings_to_compare[i]
      if (nchar(reference_string) != nchar(string)) {
        stop("Strings must be of equal length")
      }
      count <- 0
      for (y in 1:nchar(reference_string)) {
        if (substr(reference_string, y, y) != substr(string, y, y)) {
          count <- count + 1
        }
      }
      mismatches[i] <- count
    }
    return(mismatches)
  }
  apply_count_mismatches <- function(data) {
    if (nrow(data)>1) {
      mismatches <- c(NA, count_mismatches(data[1, "AA Junction"], data[2:nrow(data), "AA Junction"]))
    } else {
      mismatches <- NA
    }
    return(mismatches)
  }
  add_meta_cluster_column <- function(df, df_name) {
    df$Meta_cluster_ID <- ifelse(is.na(df$mismatches) | df$mismatches <= df$allowed_mismatches,
                                 paste0("meta_cluster_", df_name),
                                 "drop")
    return(df)
  }
  add_empty_row <- function(df) {
    empty_row <- as.data.frame(lapply(df, function(x) NA))
    names(empty_row) <- names(df)
    rbind(empty_row, df)
  }
  sum_Ν <- function(df) {
    total_sum <- sum(as.numeric(df$N), na.rm = TRUE)
    df[1, "N"] <- total_sum
    return(df)
  }
  sum_Freq <- function(df) {
    total_sum <- sum(as.numeric(df$Freq), na.rm = TRUE)
    df[1, "Freq"] <- total_sum
    return(df)
  }
  add_meta_cluster_id <- function(df, meta_cluster_id) {
    df$"Meta-clonotypes cluster ID" <- "" 
    df$"Meta-clonotypes cluster ID"[1] <- meta_cluster_id
    return(df)
  }
  add_mismatches_column <- function(df) {
    reference_string <- df$'AA Junction'[1]
    strings_to_compare <- df$'AA Junction'[-(1)]
    mismatches <- sapply(strings_to_compare, function(x) count_mismatches(reference_string, x))
    df$mismatches <- unlist(c(NA, mismatches))
    return(df)
  }
  aa_junction_fun <- function(df) {
    df[1, "AA Junction"] <- df[2, "AA Junction"]
    return(df)
  }
  aa_junction_length_fun <- function(df) {
    levels <- levels(as.factor(df$`AA Junction length`))
    df[1, "AA Junction length"] <- paste(levels)
    return(df)
  }
  V.gene_fun <- function(df) {
    levels <- levels(as.factor(df$V.gene))
    df[1, "V.gene"] <- paste(levels)
    return(df)
  }
  # Function to modify data frames with exactly 2 rows
  modify_dfs <- function(df_list) {
    for (i in seq_along(df_list)) {
      df <- df_list[[i]]
      if (nrow(df) == 2) {
        col_names <- names(df)
        df_list[[i]] <- data.frame(tail(df, 1))
        names(df_list[[i]]) <- col_names
        df_list[[i]][, ncol(df)] <- head(df[, ncol(df)], 1)
      }
    }
    return(df_list)
  }

  for (j in 1:length(name)) {

    clono_datasets_meta[[name[j]]] <- data.table::setDT(clono_datasets[[name[j]]])
    clono_datasets_meta[[name[j]]]$`AA Junction length` <- as.numeric(clono_datasets_meta[[name[j]]]$`AA Junction length`)
    clono_datasets_meta[[name[j]]] <- left_join(clono_datasets_meta[[name[j]]], mismatches_per_length_df, by = "AA Junction length")
    clono_datasets_meta[[name[j]]]$V.gene <- (as.data.frame(t(as.data.frame(strsplit(clono_datasets_meta[[name[j]]]$`V Gene and allele`, "\\*")))))$V1
    clono_datasets_meta[[name[j]]]$meta_filter <- paste(clono_datasets_meta[[name[j]]]$V.gene, clono_datasets_meta[[name[j]]]$`AA Junction length`)
    clono_datasets_meta[[name[j]]] <- split(clono_datasets_meta[[name[j]]], clono_datasets_meta[[name[j]]]$meta_filter)
    clono_datasets_meta[[name[j]]] <- lapply(clono_datasets_meta[[name[j]]], as.data.frame)
    clono_datasets_meta[[name[j]]] <- lapply(clono_datasets_meta[[name[j]]], function(x) {
      x$mismatches <- apply_count_mismatches(x)
      return(x)
    })
    clono_datasets_meta[[name[j]]] <- clono_datasets_meta[[name[j]]][order(-(sapply(clono_datasets_meta[[name[j]]], function(df) df$Freq[1])))]
    names(clono_datasets_meta[[name[j]]]) <- paste0(1:length(clono_datasets_meta[[name[j]]]))
    for (i in seq_along(clono_datasets_meta[[name[j]]])) {
      df_name <- names(clono_datasets_meta[[name[j]]])[i]
      clono_datasets_meta[[name[j]]][[i]] <- add_meta_cluster_column(clono_datasets_meta[[name[j]]][[i]], df_name)
    }
    clono_datasets_meta[[name[j]]] <- rbind(clono_datasets_meta[[name[j]]])
    clono_datasets_meta[[name[j]]] <- do.call(rbind, clono_datasets_meta[[name[j]]])
    clono_datasets_meta[[name[j]]]$new_meta_filter <- paste(clono_datasets_meta[[name[j]]]$meta_filter, clono_datasets_meta[[name[j]]]$Meta_cluster_ID)
    clono_datasets_meta[[name[j]]] <- split(clono_datasets_meta[[name[j]]], clono_datasets_meta[[name[j]]]$new_meta_filter)
    drop_dataframes <- clono_datasets_meta[[name[j]]][grep("drop", names(clono_datasets_meta[[name[j]]]))]
    drop_dataframes <- lapply(drop_dataframes, add_mismatches_column)
    names_drop_dataframes <- names(drop_dataframes)
    drop_dataframes_list <- list()
    for (df in drop_dataframes) {
      filtered_rows <- df[(df$mismatches > df$allowed_mismatches) & !(is.na(df$mismatches)), ]
      if (nrow(filtered_rows) > 0) {
        for (i in 1:nrow(filtered_rows)) {
          drop_dataframes_list <- c(drop_dataframes_list, list(filtered_rows[i, , drop = FALSE]))
        }
      }
      other_rows <- df[!((df$mismatches > df$allowed_mismatches) & !(is.na(df$mismatches))), ]
      drop_dataframes_list <- c(list(other_rows), drop_dataframes_list)
    }
    for (i in seq_along(drop_dataframes_list)) {
      df <- drop_dataframes_list[[i]]
      if (nrow(df) == 1) {
        df$mismatches <- NA
        drop_dataframes_list[[i]] <- df
      }
    }
    clono_datasets_meta[[name[j]]] <- clono_datasets_meta[[name[j]]][!names(clono_datasets_meta[[name[j]]]) %in% names_drop_dataframes]
    clono_datasets_meta[[name[j]]] <- c(clono_datasets_meta[[name[j]]], drop_dataframes_list)
    clono_datasets_meta[[name[j]]] <- lapply(clono_datasets_meta[[name[j]]], add_empty_row)
    clono_datasets_meta[[name[j]]] <- lapply(clono_datasets_meta[[name[j]]], sum_Ν)
    clono_datasets_meta[[name[j]]] <- lapply(clono_datasets_meta[[name[j]]], sum_Freq)
    clono_datasets_meta[[name[j]]] <- clono_datasets_meta[[name[j]]][order(-(sapply(clono_datasets_meta[[name[j]]], function(df) df$Freq[1])))]
    clono_datasets_meta[[name[j]]] <- lapply(clono_datasets_meta[[name[j]]], aa_junction_fun)
    clono_datasets_meta[[name[j]]] <- lapply(clono_datasets_meta[[name[j]]], aa_junction_length_fun)
    clono_datasets_meta[[name[j]]] <- lapply(clono_datasets_meta[[name[j]]], V.gene_fun)
    names(clono_datasets_meta[[name[j]]]) <- paste0(1:length(clono_datasets_meta[[name[j]]]))
    for (i in seq_along(clono_datasets_meta[[name[j]]])) {
      clono_datasets_meta[[name[j]]][[i]] <- add_meta_cluster_id(clono_datasets_meta[[name[j]]][[i]], names(clono_datasets_meta[[name[j]]])[i])
    }
    clono_datasets_meta[[name[j]]] <- modify_dfs(clono_datasets_meta[[name[j]]])
    clono_datasets_meta[[name[j]]] <- rbind(clono_datasets_meta[[name[j]]])
    clono_datasets_meta[[name[j]]] <- do.call(rbind, clono_datasets_meta[[name[j]]])
    clono_datasets_meta[[name[j]]] <- clono_datasets_meta[[name[j]]][, c("Meta-clonotypes cluster ID", "V.gene", "AA Junction", "AA Junction length", "N", "Freq", "V.REGION.identity", "mismatches", "Clonotype ID")]
    
    if (save_tables_individually) {
    fwrite(clono_datasets_meta[[name[j]]],
             paste0(e$output_folder, "/", "Meta_clonotypes_", name[j], ".txt"),
             sep = "\t",
             row.names = FALSE,
             col.names = TRUE,
             quote = FALSE
      )
    }
  }
  
  result <- list()
  result <- list(
    "meta_clonotypes" = clono_datasets_meta
  )
  
  return(result)
  
}

######################################################################################################################################
diversity_indeces <- function(div_clono_datasets, name, indeces) {
  
  diversity_ls <- list()

  transform_div_clono_datasets <- function(x) {
    x <- t(x)           # Transpose the data frame
    x <- x[1, , drop = FALSE]  # Keep only the first row
    x <- as.numeric(x)  # Convert to numeric vector
    return(x)
  }
  
  div_clono_datasets <- lapply(div_clono_datasets, transform_div_clono_datasets)
  
  for (j in 1:length(name)) {
    
    diversity_ls[[name[j]]] <- as.data.frame(name[j])
    colnames(diversity_ls[[name[j]]]) <- "Sample"
    
    if ("Richness" %in% indeces) {
      diversity_ls[[name[j]]]$"Richness" <- specnumber(div_clono_datasets[[name[j]]])
    }
    if ("Shannon index" %in% indeces) {
      diversity_ls[[name[j]]]$"Shannon index" <- diversity(div_clono_datasets[[name[j]]], index = "shannon")
    }
    if ("InvSimpson index" %in% indeces) {
      diversity_ls[[name[j]]]$"InvSimpson index" <- diversity(div_clono_datasets[[name[j]]], index = "invsimpson")
    }
    
  }
  
  diversity_ls <- bind_rows(diversity_ls)
  
  if (save_tables_individually) {
  fwrite(diversity_ls, paste0(e$output_folder, "/", "diversity_indeces", ".txt"),
         sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE
   )
  }
  
  result <- list()
  result <- list(
    "diversity_ls" = diversity_ls
  )
  
}

######################################################################################################################################

highly_similar_clonotypes <- function(clono_allData, clono_datasets, num_of_mismatches, take_gene, cdr3_lengths, gene_clonotypes, clonotype_freq_thr_for_highly_sim, name) {
    # logfile
    clono_allData <- as.data.frame(clono_allData)
    # logFile<-e$logFile
    # cat(paste0("highly_similar_clonotypes", "\t"), file = logFile, append = TRUE)
    # cat(paste0(paste("take_gene ", take_gene, "threshold", clonotype_freq_thr_for_highly_sim, sep = ","), "\t"), file = logFile, append = TRUE)
    # cat(paste0(nrow(clono_allData), "\t"), file = logFile, append = TRUE)
    # cat(paste0(ncol(clono_allData), "\t"), file = logFile, append = TRUE)
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
    
    ####################################### All Data   #######################################
    clono_allData_only_cdr3 <- clono_allData
    
    if (stringr::str_detect(clono_allData$clonotype[1], " - ") && take_gene == "No") {
        a2 <- strsplit(clono_allData$clonotype, " - ")
        clono_allData_only_cdr3$clonotype <- as.character(plyr::ldply(a2, function(s) {
            t(data.frame(unlist(s)))})[, 2])
        clono_allData_only_cdr3 <- data.table::as.data.table(clono_allData_only_cdr3[, seq_len(3), with=FALSE])[, lapply(.SD, sum), by = .(clonotype = clonotype)]
        clono_allData_only_cdr3 <- clono_allData_only_cdr3[order(-clono_allData_only_cdr3$N), ]
    }

    if (stringr::str_detect(clono_allData$clonotype[1], " - ") && take_gene == "Yes") {
        a2 <- strsplit(clono_allData$clonotype, " - ")
        clono_allData_only_cdr3$clonotype <- as.character(plyr::ldply(a2, function(s) {
            t(data.frame(unlist(s)))})[, 2])
        clono_allData_only_cdr3$gene <- as.character(plyr::ldply(a2, function(s) {
            t(data.frame(unlist(s)))})[, 1])
    }

    # if the gene does matter than I do not have to exclude it from the clono_allData_only_cdr3 table
    # group by cdr3

    clono_allData_only_cdr3$cluster_id <- as.numeric(row.names(clono_allData_only_cdr3))

    # for each cdr3 length
    highly_sim_view_specific_clonotypes <- list()
    for (i in seq_len(length(cdr3_lengths))) {
        highly_sim_view_specific_clonotypes[[paste0("length ", cdr3_lengths[i])]] <- list()

        clonotypes_of_this_length <- clono_allData_only_cdr3 %>% dplyr::filter(str_length(clono_allData_only_cdr3$clonotype) == (cdr3_lengths[i] + 2))

        end_process_for_this_length <- FALSE

        while (end_process_for_this_length == FALSE) {
            major_clonotype <- clonotypes_of_this_length$clonotype[1]

            if (nrow(clonotypes_of_this_length) > 0) {
                if (clonotypes_of_this_length$Freq[1] > clonotype_freq_thr_for_highly_sim) {
                    if (take_gene == "Yes") {
                        clonotypes_of_this_length_gene <- clonotypes_of_this_length %>% dplyr::filter(clonotypes_of_this_length$gene == clonotypes_of_this_length$gene[1])
                        dist_from_major <- stringdist::stringdist(clonotypes_of_this_length_gene$clonotype, major_clonotype)
                        matched_clonotypes <- clonotypes_of_this_length_gene %>% dplyr::filter(dist_from_major <= num_of_mismatches[i])
                        not_matched_clonotypes <- clonotypes_of_this_length %>% dplyr::filter(!(clonotypes_of_this_length$clonotype %in% matched_clonotypes$clonotype))
                    } else {
                        dist_from_major <- stringdist::stringdist(clonotypes_of_this_length$clonotype, major_clonotype)
                        matched_clonotypes <- clonotypes_of_this_length %>% dplyr::filter(dist_from_major <= num_of_mismatches[i])
                        not_matched_clonotypes <- clonotypes_of_this_length %>% dplyr::filter(dist_from_major > num_of_mismatches[i])
                    }

                    if (nrow(matched_clonotypes) > 0) {
                        # save it
                        if (take_gene == "No") {
                            highly_sim_view_specific_clonotypes[[paste0("length ", cdr3_lengths[i])]][[clonotypes_of_this_length$clonotype[1]]] <- matched_clonotypes # save the corresponding clonotype from clono_allData table not just the cdr3
                            highly_sim_view_specific_clonotypes[[paste0("length ", cdr3_lengths[i])]][[clonotypes_of_this_length$clonotype[1]]]$prev_cluster <- as.numeric(row.names(matched_clonotypes))
                        } else {
                            highly_sim_view_specific_clonotypes[[paste0("length ", cdr3_lengths[i])]][[clono_allData$clonotype[clonotypes_of_this_length$cluster_id[1]]]] <- clono_allData[matched_clonotypes$cluster_id, ] # save the corresponding clonotype from clono_allData table not just the cdr3
                            highly_sim_view_specific_clonotypes[[paste0("length ", cdr3_lengths[i])]][[clono_allData$clonotype[clonotypes_of_this_length$cluster_id[1]]]]$prev_cluster <- as.numeric(row.names(clono_allData[matched_clonotypes$cluster_id, ]))
                        }
                    }
                    if (nrow(not_matched_clonotypes) == 0) {
                        end_process_for_this_length <- TRUE
                    } else {
                        clonotypes_of_this_length <- not_matched_clonotypes
                    }
                } else {
                    # terminate the process for this length
                    end_process_for_this_length <- TRUE
                }
            } else {
                # terminate the process for this length
                end_process_for_this_length <- TRUE
            }
        }
    }

    ##### Results to tables
    highly_sim_clonotypes <- list()
    highly_sim_clonotypes_allGroups <- list()
    for (i in seq_len(length(cdr3_lengths))) {
        clonotype <- names(highly_sim_view_specific_clonotypes[[paste0("length ", cdr3_lengths[i])]])
        if (!(is.null(clonotype))) {
            N <- c()
            Freq <- c()
            prev_cluster <- c()
            for (j in seq_len(length(highly_sim_view_specific_clonotypes[[paste0("length ", cdr3_lengths[i])]]))) {
                highly_sim_view_specific_clonotypes[[paste0("length ", cdr3_lengths[i])]][[clonotype[j]]]$HS_cluster_id <- j
                N <- c(N, sum(highly_sim_view_specific_clonotypes[[paste0("length ", cdr3_lengths[i])]][[clonotype[j]]]$N))
                Freq <- c(Freq, sum(highly_sim_view_specific_clonotypes[[paste0("length ", cdr3_lengths[i])]][[clonotype[j]]]$Freq))
                prev_cluster_c <- ""
                for (k in seq_len(nrow(highly_sim_view_specific_clonotypes[[paste0("length ", cdr3_lengths[i])]][[clonotype[j]]]))) {
                    prev_cluster_c <- paste(prev_cluster_c, highly_sim_view_specific_clonotypes[[paste0("length ", cdr3_lengths[i])]][[clonotype[j]]]$prev_cluster[k])
                }
                prev_cluster <- c(prev_cluster, prev_cluster_c)
            }
            highly_sim_clonotypes[[paste0("length ", cdr3_lengths[i])]] <- data.frame(clonotype, N, Freq, prev_cluster, stringsAsFactors = FALSE) # I have this data frame for each length
            highly_sim_clonotypes[[paste0("length ", cdr3_lengths[i])]]$HS_cluster_id <- as.numeric(row.names(highly_sim_clonotypes[[paste0("length ", cdr3_lengths[i])]]))
        }
        highly_sim_clonotypes_allGroups[[paste0("length ", cdr3_lengths[i])]] <- do.call(rbind.data.frame, highly_sim_view_specific_clonotypes[[paste0("length ", cdr3_lengths[i])]])

        # extra clonotypes
        if (take_gene == "Yes") {
            clonotypes_of_this_length_id <- which(str_length(clono_allData_only_cdr3$clonotype) == (cdr3_lengths[i] + 2))
            extra_clono <- clono_allData[clonotypes_of_this_length_id, ] %>% dplyr::filter(!(clono_allData[clonotypes_of_this_length_id, ]$clonotype %in% highly_sim_clonotypes_allGroups[[paste0("length ", cdr3_lengths[i])]]$clonotype))
        } else {
            clonotypes_of_this_length <- clono_allData_only_cdr3 %>% dplyr::filter(str_length(clono_allData_only_cdr3$clonotype) == (cdr3_lengths[i] + 2))
            extra_clono <- clonotypes_of_this_length %>% dplyr::filter(!(clonotypes_of_this_length$clonotype %in% highly_sim_clonotypes_allGroups[[paste0("length ", cdr3_lengths[i])]]$clonotype))
        }

        if (nrow(extra_clono) > 0) {
            extra_clono$prev_cluster <- 0
            for (k in seq_len(nrow(extra_clono))) {
                temp <- as.character(extra_clono[["Convergent Evolution"]][k])
                extra_clono$prev_cluster[k] <- as.numeric(strsplit(strsplit(temp, "cluster ")[[1]][2], " :")[[1]][1])
            }
            if (!(is.null(clonotype))) {
                extra_clono$HS_cluster_id <- (max(highly_sim_clonotypes[[paste0("length ", cdr3_lengths[i])]]$HS_cluster_id) + 1):(max(highly_sim_clonotypes[[paste0("length ", cdr3_lengths[i])]]$HS_cluster_id) + nrow(extra_clono))
            } else {
                extra_clono$HS_cluster_id <- seq_len(nrow(extra_clono))
            }
            highly_sim_clonotypes_allGroups[[paste0("length ", cdr3_lengths[i])]] <- rbind(highly_sim_clonotypes_allGroups[[paste0("length ", cdr3_lengths[i])]], extra_clono)
            highly_sim_clonotypes[[paste0("length ", cdr3_lengths[i])]] <- rbind(highly_sim_clonotypes[[paste0("length ", cdr3_lengths[i])]], extra_clono[, c("clonotype", "N", "Freq", "prev_cluster", "HS_cluster_id")])
        }

        row.names(highly_sim_clonotypes_allGroups[[paste0("length ", cdr3_lengths[i])]]) <- seq_len(nrow(highly_sim_clonotypes_allGroups[[paste0("length ", cdr3_lengths[i])]]))

        if (save_tables_individually) {
            filename <- paste0(e$output_folder, "/", "Highly_sim_Clonotypes_", "All_Data_length_", cdr3_lengths[i], ".txt")
            write.table(highly_sim_clonotypes[[paste0("length ", cdr3_lengths[i])]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
            filename <- paste0(e$output_folder, "/", "Highly_sim_Clonotypes_groups_", "All_Data_length_", cdr3_lengths[i], ".txt")
            write.table(highly_sim_clonotypes_allGroups[[paste0("length ", cdr3_lengths[i])]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
        }
    }

    ####################################### Separate Datasets #######################################
    highly_sim_view_specific_clonotypes_datasets <- list()
    highly_sim_clonotypes_datasets <- list()
    highly_sim_clonotypes_allGroups_datasets <- list()

    one_run <- function(j) {
        clono_allData_only_cdr3 <- clono_datasets[[name[j]]]
        if (stringr::str_detect(clono_datasets[[name[j]]]$clonotype[1], " - ") && take_gene == "No") {
            a2 <- strsplit(clono_datasets[[name[j]]]$clonotype, " - ")
            clono_allData_only_cdr3$clonotype <- as.character(plyr::ldply(a2, function(s) {
                t(data.frame(unlist(s)))
            })[, 2])
            clono_allData_only_cdr3 <- data.table::as.data.table(clono_allData_only_cdr3[, seq_len(3), with=FALSE])[, lapply(.SD, sum), by = .(clonotype = clonotype)]
            clono_allData_only_cdr3 <- clono_allData_only_cdr3[order(-clono_allData_only_cdr3$N), ]
        }
        if (stringr::str_detect(clono_datasets[[name[j]]]$clonotype[1], " - ") && take_gene == "Yes") {
            a2 <- strsplit(clono_datasets[[name[j]]]$clonotype, " - ")
            clono_allData_only_cdr3$clonotype <- as.character(plyr::ldply(a2, function(s) {
                t(data.frame(unlist(s)))
            })[, 2])
            clono_allData_only_cdr3$gene <- as.character(plyr::ldply(a2, function(s) {
                t(data.frame(unlist(s)))
            })[, 1])
        }

        clono_allData_only_cdr3$cluster_id <- as.numeric(row.names(clono_allData_only_cdr3))

        #### analysis
        # for each cdr3 length
        highly_sim_view_specific_clonotypes_datasets[[name[j]]] <- list()
        for (i in seq_len(length(cdr3_lengths))) {
            highly_sim_view_specific_clonotypes_datasets[[name[j]]][[paste0("length ", cdr3_lengths[i])]] <- list()

            clonotypes_of_this_length <- clono_allData_only_cdr3 %>% dplyr::filter(str_length(clono_allData_only_cdr3$clonotype) == (cdr3_lengths[i] + 2))

            end_process_for_this_length <- FALSE

            while (end_process_for_this_length == FALSE) {
                major_clonotype <- clonotypes_of_this_length$clonotype[1]
                if (nrow(clonotypes_of_this_length) > 0) {
                    if (clonotypes_of_this_length$Freq[1] > clonotype_freq_thr_for_highly_sim) {
                        if (take_gene == "Yes") {
                            clonotypes_of_this_length_gene <- clonotypes_of_this_length %>% dplyr::filter(clonotypes_of_this_length$gene == clonotypes_of_this_length$gene[1])
                            dist_from_major <- stringdist::stringdist(clonotypes_of_this_length_gene$clonotype, major_clonotype)
                            matched_clonotypes <- clonotypes_of_this_length_gene %>% dplyr::filter(dist_from_major <= num_of_mismatches[i])
                            not_matched_clonotypes <- clonotypes_of_this_length %>% dplyr::filter(!(clonotypes_of_this_length$clonotype %in% matched_clonotypes$clonotype))
                        } else {
                            dist_from_major <- stringdist::stringdist(clonotypes_of_this_length$clonotype, major_clonotype)
                            matched_clonotypes <- clonotypes_of_this_length %>% dplyr::filter(dist_from_major <= num_of_mismatches[i])
                            not_matched_clonotypes <- clonotypes_of_this_length %>% dplyr::filter(dist_from_major > num_of_mismatches[i])
                        }

                        if (nrow(matched_clonotypes) > 0) {
                            # save it
                            if (take_gene == "No") {
                                highly_sim_view_specific_clonotypes_datasets[[name[j]]][[paste0("length ", cdr3_lengths[i])]][[clonotypes_of_this_length$clonotype[1]]] <- matched_clonotypes # save the corresponding clonotype from clono_allData table not just the cdr3
                                highly_sim_view_specific_clonotypes_datasets[[name[j]]][[paste0("length ", cdr3_lengths[i])]][[clonotypes_of_this_length$clonotype[1]]]$prev_cluster <- row.names(matched_clonotypes) # save the corresponding clonotype from clono_allData table not just the cdr3
                            } else {
                                highly_sim_view_specific_clonotypes_datasets[[name[j]]][[paste0("length ", cdr3_lengths[i])]][[clono_datasets[[name[j]]]$clonotype[clonotypes_of_this_length$cluster_id[1]]]] <- clono_datasets[[name[j]]][matched_clonotypes$cluster_id, ] # save the corresponding clonotype from clono_allData table not just the cdr3
                                highly_sim_view_specific_clonotypes_datasets[[name[j]]][[paste0("length ", cdr3_lengths[i])]][[clono_datasets[[name[j]]]$clonotype[clonotypes_of_this_length$cluster_id[1]]]]$prev_cluster <- row.names(clono_datasets[[name[j]]][matched_clonotypes$cluster_id, ]) # save the corresponding clonotype from clono_allData table not just the cdr3
                            }
                        }
                        if (nrow(not_matched_clonotypes) == 0) {
                            end_process_for_this_length <- TRUE
                        } else {
                            clonotypes_of_this_length <- not_matched_clonotypes
                        }
                    } else {
                        # terminate the process for this length
                        end_process_for_this_length <- TRUE
                    }
                } else {
                    # terminate the process for this length
                    end_process_for_this_length <- TRUE
                }
            }
        }

        ##### Results to tables
        highly_sim_clonotypes_datasets[[name[j]]] <- list()
        highly_sim_clonotypes_allGroups_datasets[[name[j]]] <- list()
        for (i in seq_len(length(cdr3_lengths))) {
            clonotype <- names(highly_sim_view_specific_clonotypes_datasets[[name[j]]][[paste0("length ", cdr3_lengths[i])]])
            if (!(is.null(clonotype))) {
                N <- c()
                Freq <- c()
                prev_cluster <- c()
                for (cl in seq_len(length(highly_sim_view_specific_clonotypes_datasets[[name[j]]][[paste0("length ", cdr3_lengths[i])]]))) {
                    highly_sim_view_specific_clonotypes_datasets[[name[j]]][[paste0("length ", cdr3_lengths[i])]][[clonotype[cl]]]$HS_cluster_id <- cl
                    N <- c(N, sum(highly_sim_view_specific_clonotypes_datasets[[name[j]]][[paste0("length ", cdr3_lengths[i])]][[clonotype[cl]]]$N))
                    Freq <- c(Freq, sum(highly_sim_view_specific_clonotypes_datasets[[name[j]]][[paste0("length ", cdr3_lengths[i])]][[clonotype[cl]]]$Freq))
                    prev_cluster_c <- ""
                    for (k in seq_len(nrow(highly_sim_view_specific_clonotypes_datasets[[name[j]]][[paste0("length ", cdr3_lengths[i])]][[clonotype[cl]]]))) {
                        prev_cluster_c <- paste(prev_cluster_c, highly_sim_view_specific_clonotypes_datasets[[name[j]]][[paste0("length ", cdr3_lengths[i])]][[clonotype[cl]]]$prev_cluster[k])
                    }
                    prev_cluster <- c(prev_cluster, prev_cluster_c)
                }
                highly_sim_clonotypes_datasets[[name[j]]][[paste0("length ", cdr3_lengths[i])]] <- data.frame(clonotype, N, Freq, prev_cluster, stringsAsFactors = FALSE) # I have this data frame for each length
                highly_sim_clonotypes_datasets[[name[j]]][[paste0("length ", cdr3_lengths[i])]]$HS_cluster_id <- as.numeric(row.names(highly_sim_clonotypes_datasets[[name[j]]][[paste0("length ", cdr3_lengths[i])]]))
            }
            highly_sim_clonotypes_allGroups_datasets[[name[j]]][[paste0("length ", cdr3_lengths[i])]] <- do.call(rbind.data.frame, highly_sim_view_specific_clonotypes_datasets[[name[j]]][[paste0("length ", cdr3_lengths[i])]])

            # extra clonotypes
            if (take_gene == "Yes") {
                clonotypes_of_this_length_id <- which(str_length(clono_allData_only_cdr3$clonotype) == (cdr3_lengths[i] + 2))
                extra_clono <- clono_datasets[[name[j]]][clonotypes_of_this_length_id, ] %>% dplyr::filter(!(clono_datasets[[name[j]]][clonotypes_of_this_length_id, ]$clonotype %in% highly_sim_clonotypes_allGroups_datasets[[name[j]]][[paste0("length ", cdr3_lengths[i])]]$clonotype))
            } else {
                clonotypes_of_this_length <- clono_allData_only_cdr3 %>% dplyr::filter(str_length(clono_allData_only_cdr3$clonotype) == (cdr3_lengths[i] + 2))
                extra_clono <- clonotypes_of_this_length %>% dplyr::filter(!(clonotypes_of_this_length$clonotype %in% highly_sim_clonotypes_allGroups_datasets[[name[j]]][[paste0("length ", cdr3_lengths[i])]]$clonotype))
            }

            if (nrow(extra_clono) > 0) {
                extra_clono$prev_cluster <- 0
                for (k in seq_len(nrow(extra_clono))) {
                    temp <- as.character(extra_clono[["Convergent Evolution"]][k])
                    extra_clono$prev_cluster[k] <- as.numeric(strsplit(strsplit(temp, "cluster ")[[1]][2], " :")[[1]][1])
                }
                if (!(is.null(clonotype))) {
                    extra_clono$HS_cluster_id <- (max(highly_sim_clonotypes_datasets[[name[j]]][[paste0("length ", cdr3_lengths[i])]]$HS_cluster_id) + 1):(max(highly_sim_clonotypes_datasets[[name[j]]][[paste0("length ", cdr3_lengths[i])]]$HS_cluster_id) + nrow(extra_clono))
                } else {
                    extra_clono$HS_cluster_id <- seq_len(nrow(extra_clono))
                }
                highly_sim_clonotypes_allGroups_datasets[[name[j]]][[paste0("length ", cdr3_lengths[i])]] <- rbind(highly_sim_clonotypes_allGroups_datasets[[name[j]]][[paste0("length ", cdr3_lengths[i])]], extra_clono)
                highly_sim_clonotypes_datasets[[name[j]]][[paste0("length ", cdr3_lengths[i])]] <- rbind(highly_sim_clonotypes_datasets[[name[j]]][[paste0("length ", cdr3_lengths[i])]], extra_clono[, c("clonotype", "N", "Freq", "prev_cluster", "HS_cluster_id")])

                row.names(highly_sim_clonotypes_allGroups_datasets[[name[j]]][[paste0("length ", cdr3_lengths[i])]]) <- seq_len(nrow(highly_sim_clonotypes_allGroups_datasets[[name[j]]][[paste0("length ", cdr3_lengths[i])]]))
            }

            if (save_tables_individually) {
                filename <- paste0(e$output_folder, "/", "Highly_sim_Clonotypes_", name[j], "_length_", cdr3_lengths[i], ".txt")
                write.table(highly_sim_clonotypes_datasets[[name[j]]][[paste0("length ", cdr3_lengths[i])]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
                filename <- paste0(e$output_folder, "/", "Highly_sim_Clonotypes_groups_", name[j], "_length_", cdr3_lengths[i], ".txt")
                write.table(highly_sim_clonotypes_allGroups_datasets[[name[j]]][[paste0("length ", cdr3_lengths[i])]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
            }
        }

        result <- list()
        result[["highly_sim_clonotypes_allGroups_datasets"]] <- highly_sim_clonotypes_allGroups_datasets[[name[j]]]
        result[["highly_sim_clonotypes_datasets"]] <- highly_sim_clonotypes_datasets[[name[j]]]
        result[["highly_sim_view_specific_clonotypes_datasets"]] <- highly_sim_view_specific_clonotypes_datasets[[name[j]]]

        return(result)
    }

    if (Sys.info()[1] == "Windows") {
        a <- lapply(seq_len(length(name)), one_run)
        for (i in seq_len(length(name))) {
            highly_sim_clonotypes_allGroups_datasets[[name[i]]] <- a[[i]]$highly_sim_clonotypes_allGroups_datasets
            clono_datasets[[name[i]]] <- a[[i]]$clono_datasets
            highly_sim_clonotypes_datasets[[name[i]]] <- a[[i]]$highly_sim_clonotypes_datasets
            highly_sim_view_specific_clonotypes_datasets[[name[i]]] <- a[[i]]$highly_sim_view_specific_clonotypes_datasets
        }
    } else {
        a <- lapply(seq_len(length(name)), one_run)
        #a <- parallel::mclapply(seq_len(length(name)), one_run, mc.cores = num_of_cores, mc.preschedule = TRUE)
        for (i in seq_len(length(name))) {
            highly_sim_clonotypes_allGroups_datasets[[name[i]]] <- a[[i]]$highly_sim_clonotypes_allGroups_datasets
            clono_datasets[[name[i]]] <- a[[i]]$clono_datasets
            highly_sim_clonotypes_datasets[[name[i]]] <- a[[i]]$highly_sim_clonotypes_datasets
            highly_sim_view_specific_clonotypes_datasets[[name[i]]] <- a[[i]]$highly_sim_view_specific_clonotypes_datasets
        }
    }

    confirm <- paste0("Highly Similar Clonotypes run!")

    result <- list(
        "highly_sim_view_specific_clonotypes" = highly_sim_view_specific_clonotypes,
        "highly_sim_clonotypes" = highly_sim_clonotypes,
        "highly_sim_view_specific_clonotypes_datasets" = highly_sim_view_specific_clonotypes_datasets,
        "highly_sim_clonotypes_datasets" = highly_sim_clonotypes_datasets,
        "highly_sim_clonotypes_allGroups" = highly_sim_clonotypes_allGroups,
        "highly_sim_clonotypes_allGroups_datasets" = highly_sim_clonotypes_allGroups_datasets,
        "confirm" = confirm
    )

    # log time end and memory used
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
    # cat(pryr::mem_used(), file = logFile, append = TRUE, sep = "\n")
    
    return(result)
}

######################################################################################################################################

public_clonotypes <- function(clono_allData, clono_datasets, take_gene, use_reads, public_clonotype_thr, name, highly) {
  # logfile
  clono_allData <- as.data.frame(clono_allData)
  # logFile<-e$logFile
  # cat(paste0("public_clonotypes", "\t"), file = logFile, append = TRUE)
  # cat(paste0(paste("take_gene ", take_gene, "threshold", public_clonotype_thr, sep = ","), "\t"), file = logFile, append = TRUE)
  # cat(paste0(nrow(clono_allData), "\t"), file = logFile, append = TRUE)
  # cat(paste0(ncol(clono_allData), "\t"), file = logFile, append = TRUE)
  # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
  
  if (stringr::str_detect(clono_allData$clonotype[1], " - ") && take_gene == "No") {
    a2 <- strsplit(clono_allData$clonotype, " - ")
    clono_allData$clonotype <- as.character(plyr::ldply(a2, function(s) {
      t(data.frame(unlist(s)))
    })[, 2])
    clono_allData <- data.table::as.data.table(clono_allData[, seq_len(3)])[, lapply(.SD, sum), by = .(clonotype = clonotype)]
    clono_allData <- clono_allData[order(-clono_allData$N), ]
  }
  
  initial_sum <- list()
  for (n in name) {
    initial_sum[[n]] <- sum(clono_datasets[[n]]$N)
  }
  
  if (use_reads) {
    clono_allData <- clono_allData %>% dplyr::filter(N > public_clonotype_thr)
  } else {
    clono_allData <- clono_allData[seq_len(public_clonotype_thr), ]
  }
  
  public_clono <- data.frame(clonotype = unique(clono_allData$clonotype), stringsAsFactors = FALSE)
  
  # for each dataset
  for (n in name) {
    if (stringr::str_detect(clono_datasets[[n]]$clonotype[1], " - ") && take_gene == "No") {
      a2 <- strsplit(clono_datasets[[n]]$clonotype, " - ")
      clono_datasets[[n]]$clonotype <- as.character(plyr::ldply(a2, function(s) {
        t(data.frame(unlist(s)))
      })[, 2])
      clono_datasets[[n]] <- data.table::as.data.table(clono_datasets[[n]][, seq_len(3)])[, lapply(.SD, sum), by = .(clonotype = clonotype)]
      clono_datasets[[n]] <- clono_datasets[[n]][order(-clono_datasets[[n]]$N), ]
    }
    
    if (use_reads) {
      clono_datasets[[n]] <- clono_datasets[[n]] %>% dplyr::filter(N > public_clonotype_thr)
    } else {
      clono_datasets[[n]] <- clono_datasets[[n]][seq_len(public_clonotype_thr), ]
    }
    
    ids_dataset <- which(clono_datasets[[n]]$clonotype %in% public_clono$clonotype)
    ids <- match(clono_datasets[[n]]$clonotype, public_clono$clonotype)[which(!(is.na(match(clono_datasets[[n]]$clonotype, public_clono$clonotype))))]
    public_clono[[paste0(n, "_Reads/Total")]] <- NA
    public_clono[[paste0(n, "_Freq")]] <- NA
    public_clono[[paste0(n, "_Reads/Total")]][ids] <- paste0(clono_datasets[[n]]$N[ids_dataset], "/", initial_sum[[n]])
    public_clono[[paste0(n, "_Freq")]][ids] <- clono_datasets[[n]]$Freq[ids_dataset]
  }
  
  public_clono$Num_of_patients <- NA
  
  # filter results
  public_clono$Num_of_patients <- (apply(public_clono, 1, function(x) sum(!(is.na(x)))) - 1) / 2
  public_clono <- public_clono %>% dplyr::filter(Num_of_patients > 1)
  
  # replace NA with 0
  public_clono[is.na(public_clono)] <- 0
  
  if (save_tables_individually) {
    if (highly) {
      filename <- paste0(e$output_folder, "/", "public_highly_clonotypes", ".txt")
      write.table(public_clono, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
    } else {
      filename <- paste0(e$output_folder, "/", "public_clonotypes", ".txt")
      write.table(public_clono, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
    }
  }
  
  confirm <- paste0("Shared Clonotypes run!")
  
  result <- list("public_clono" = public_clono, "confirm" = confirm)
  
  # log time end and memory used
  # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
  # cat(pryr::mem_used(), file = logFile, append = TRUE, sep = "\n")
  
  return(result)
}

######################################################################################################################################

createLink <- function(val, on_click_js) {
    as.character(tags$a(href = "#", onclick = sprintf(on_click_js, val), val))
}

######################################################################################################################################

viewClonotypes <- function(allData, allele, gene, junction, val1, val2) {
    # logfile
    # logFile<-e$logFile
    # cat(paste0("viewClonotypes", "\t"), file = logFile, append = TRUE)
    # cat(paste0(nrow(allData), "\t"), file = logFile, append = TRUE)
    # cat(paste0(ncol(allData), "\t"), file = logFile, append = TRUE)
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)

    temp <- allData
    if (length(gene) > 0) {
        if (allele == FALSE) {
            for (i in seq_len(nrow(temp))) {
                temp[[gene]][i] <- strsplit(temp[[gene]][i], "[*]")[[1]][1]
            }
        } else {
            for (i in seq_len(nrow(temp))) {
                temp[[gene]][i] <- strsplit(temp[[gene]][i], "(see comment)")[[1]][1]
            }
        }
        inputVGenes_CDR3 <- which((temp[[gene]] == val1) & (temp[[junction]] == val2))
    } else {
        inputVGenes_CDR3 <- which(temp[[junction]] == val1)
    }

    a <- allData[inputVGenes_CDR3, ]

    # log time end and memory used
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
    # cat(pryr::mem_used(), file = logFile, append = TRUE, sep = "\n")

    return(a)
}

######################################################################################################################################

repertoires <- function(clono_allData, clono_datasets, allele, allele_clonotypes, gene, gene_clonotypes, name, view_specific_clonotype_allData, view_specific_clonotype_datasets ) {
    #save(clono_allData, clono_datasets, allele, allele_clonotypes, gene, gene_clonotypes, name, view_specific_clonotype_allData, view_specific_clonotype_datasets, file = './4_repertoire.RData')
    if (allele == FALSE) {
        g <- stringr::str_replace(gene, ".and.allele", "")
    } else {
        g <- gene
    }
      
    # logfile
    # logFile<-e$logFile
    # cat(paste0("repertoires", "\t"), file = logFile, append = TRUE)
    # cat(paste0(g, "\t"), file = logFile, append = TRUE)
    # cat(paste0(nrow(clono_allData), "\t"), file = logFile, append = TRUE)
    # cat(paste0(ncol(clono_allData), "\t"), file = logFile, append = TRUE)
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)

    for (i in seq_len(nrow(clono_allData))) {
        clono_allData[i, 1] <- strsplit(as.character(clono_allData[i, 1]), " - ")[[1]][1]
    }

    for (j in seq_len(length(name))) {
        for (i in seq_len(nrow(clono_datasets[[name[j]]]))) {
            clono_datasets[[name[j]]][i, 1] <- strsplit(as.character(clono_datasets[[name[j]]][i, 1]), " - ")[[1]][1]
        }
    }

    if (gene == gene_clonotypes && allele == allele_clonotypes && !(is.null(gene_clonotypes))) {
        ####################################### All Data
        clono_allData       <- setDT(clono_allData)
        Repertoires_allData <- clono_allData[ , by = clonotype , .N ]
        # Repertoires_allData <- clono_allData %>%
        #     dplyr::group_by(clono_allData[["clonotype"]]) %>%
        #     dplyr::summarise(n = n())
        Repertoires_allData <- Repertoires_allData[order(-Repertoires_allData$N), ]
        Repertoires_allData <- cbind(Repertoires_allData, Freq = 100 * Repertoires_allData$N / nrow(clono_allData))

        ####################################### Separate Datasets
        Repertoires_datasets <- list()

        one_run <- function(j) {
            clono_datasets <- lapply(clono_datasets, setDT)
            Repertoires_datasets[[name[j]]] <- clono_datasets[[name[j]]][ , by = clonotype, .N]
            # Repertoires_datasets[[name[j]]] <- clono_datasets[[name[j]]] %>%
            #     dplyr::group_by(clono_datasets[[name[j]]][["clonotype"]]) %>%
            #     dplyr::summarise(n = n())
            Repertoires_datasets[[name[j]]] <- Repertoires_datasets[[name[j]]][order(-Repertoires_datasets[[name[j]]]$N), ]
            Repertoires_datasets[[name[j]]] <- cbind(Repertoires_datasets[[name[j]]], Freq = 100 * Repertoires_datasets[[name[j]]]$N / nrow(clono_datasets[[name[j]]]))
            colnames(Repertoires_datasets[[name[j]]]) <- c("Gene", "N", "Freq")

            if (save_tables_individually) {
                filename <- paste0(e$output_folder, "/", "Repertoires_", g, "_", name[j], ".txt")
                fwrite(Repertoires_datasets[[name[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
            }
            return(Repertoires_datasets[[name[j]]])
        }

        if (Sys.info()[1] == "Windows") {
            Repertoires_datasets <- lapply(seq_len(length(name)), one_run)
        } else {
            Repertoires_datasets <- lapply(seq_len(length(name)), one_run)
            #Repertoires_datasets <- parallel::mclapply(seq_len(length(name)), one_run, mc.cores = num_of_cores, mc.preschedule = TRUE)
        }

        names(Repertoires_datasets) <- name
    } else {
        # find the most frequent gene that exists in each specific clonotype
        ####################################### All Data
        freq_gene_name <- data.frame()
        for (i in names(view_specific_clonotype_allData)) {
            a <- view_specific_clonotype_allData[[i]]
            # if ((allele==F) & (allele!=allele_clonotypes)){
            if (allele == FALSE) {
                if (!all(!(stringr::str_detect(a[[gene]], "[*]")))) {
                    a2 <- strsplit(a[[gene]], "[*]")
                    a[[gene]] <- as.character(plyr::ldply(a2, function(s) {
                        t(data.frame(unlist(s)))
                    })[, 1])
                }
            }
            ### CHANGE
            a$gene <- a[[gene]]
            a <- setDT(a)
            freq_gene <- a[, by = gene, .N]
            
            # freq_gene <- a %>%
            #     dplyr::group_by(a[[gene]]) %>%
            #     dplyr::summarise(n = n())
            # freq_gene <- freq_gene[order(-freq_gene$n), ]
            freq_gene <- freq_gene[order(-freq_gene$N), ]
            freq_gene_name[i, 1] <- freq_gene[1, 1]
        }

        colnames(freq_gene_name) <- c("Gene")
        freq_gene_name <- setDT(freq_gene_name)
        freq_gene_name <- freq_gene_name[, .N, by = Gene]
        
        # freq_gene_name <- freq_gene_name %>%
        #     dplyr::group_by(freq_gene_name[["Gene"]]) %>%
        #     dplyr::summarise(n = n())
        freq_gene_name <- freq_gene_name[order(-freq_gene_name$N), ]
        freq_gene_name <- cbind(freq_gene_name, Freq = 100 * freq_gene_name$N / nrow(clono_allData))
        colnames(freq_gene_name) <- c("Gene", "N", "Freq")

        
        ####################################### Separate Datasets
        freq_gene_name_datasets <- list()

        one_run <- function(j) {
            freq_gene_name_datasets[[name[[j]]]] <- data.frame()
            for (i in names(view_specific_clonotype_datasets[[name[j]]])) {
                a <- view_specific_clonotype_datasets[[name[j]]][[i]]
                if (allele == FALSE) {
                    if (!all(!(stringr::str_detect(a[[gene]], "[*]")))) {
                        a2 <- strsplit(a[[gene]], "[*]")
                        a[[gene]] <- as.character(plyr::ldply(a2, function(s) {
                            t(data.frame(unlist(s)))
                        })[, 1])
                    }
                }
                a$gene <- a[[gene]]
                a <- setDT(a)
                freq_gene <- a[, by = gene, .N]
                # freq_gene <- a %>%
                #     dplyr::group_by(a[[gene]]) %>%
                #     dplyr::summarise(n = n())
                freq_gene <- freq_gene[order(-freq_gene$N), ]
                freq_gene_name_datasets[[name[[j]]]][i, 1] <- freq_gene[1, 1]
            }
            colnames(freq_gene_name_datasets[[name[[j]]]]) <- c("Gene")
            setDT(freq_gene_name_datasets[[name[[j]]]])
            freq_gene_name_datasets[[name[[j]]]] <- freq_gene_name_datasets[[name[[j]]]][, by = Gene, .N]
            # freq_gene_name_datasets[[name[[j]]]] <- freq_gene_name_datasets[[name[[j]]]] %>%
            #     dplyr::group_by(freq_gene_name_datasets[[name[[j]]]][["Gene"]]) %>%
            #     dplyr::summarise(n = n())
            freq_gene_name_datasets[[name[j]]] <- freq_gene_name_datasets[[name[j]]][order(-freq_gene_name_datasets[[name[j]]]$N), ]
            freq_gene_name_datasets[[name[j]]] <- cbind(freq_gene_name_datasets[[name[j]]], Freq = 100 * freq_gene_name_datasets[[name[j]]]$N / nrow(clono_datasets[[name[j]]]))

            colnames(freq_gene_name_datasets[[name[j]]]) <- c("Gene", "N", "Freq")

            if (save_tables_individually) {
                filename <- paste0(e$output_folder, "/", "Repertoires_", g, "_", name[j], ".txt")
                fwrite(freq_gene_name_datasets[[name[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
            }
            return(freq_gene_name_datasets[[name[j]]])
        }

        if (Sys.info()[1] == "Windows") {
            freq_gene_name_datasets <- lapply(seq_len(length(name)), one_run)
        } else {
            freq_gene_name_datasets <- lapply(seq_len(length(name)), one_run)
            #freq_gene_name_datasets <- parallel::mclapply(seq_len(length(name)), one_run, mc.cores = num_of_cores, mc.preschedule = TRUE)
        }

        names(freq_gene_name_datasets) <- name
        Repertoires_allData <- freq_gene_name
        Repertoires_datasets <- freq_gene_name_datasets
    }


    colnames(Repertoires_allData) <- c("Gene", "N", "Freq")

    if (save_tables_individually) {
        filename <- paste0(e$output_folder, "/", "Repertoires_", g, "_", "All_Data", ".txt")
        fwrite(Repertoires_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
    }

    confirm <- paste0("Repertoires run!")

    result <- list("Repertoires_allData" = Repertoires_allData, "Repertoires_datasets" = Repertoires_datasets, "confirm" = confirm)

    # log time end and memory used
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
    # cat(pryr::mem_used(), file = logFile, append = TRUE, sep = "\n")

    return(result)

}

######################################################################################################################################

repertoires_highly_similar <- function(clono_allData, clono_datasets, allele, allele_clonotypes, gene, gene_clonotypes, name, view_specific_clonotype_allData, view_specific_clonotype_datasets, take_gene) {
    # logfile
    #save(clono_allData, clono_datasets, allele, allele_clonotypes, gene, gene_clonotypes, name, view_specific_clonotype_allData, view_specific_clonotype_datasets, take_gene, file = './HIGHLY_SIM_REPERTOIRE.RData')
    if (allele == FALSE) {
        g <- stringr::str_replace(gene, ".and.allele", "")
    } else {
        g <- gene
    }

    # logfile
    # logFile<-e$logFile
    # cat(paste0("repertoires_highly_similar", "\t"), file = logFile, append = TRUE)
    # cat(paste0(g, "\t"), file = logFile, append = TRUE)
    # cat(paste0(nrow(clono_allData), "\t"), file = logFile, append = TRUE)
    # cat(paste0(ncol(clono_allData), "\t"), file = logFile, append = TRUE)
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)

    if (allele == FALSE) {
        g <- stringr::str_replace(gene, ".and.allele", "")
    } else {
        g <- gene
    }

    clono_allData_initial <- clono_allData
    clono_datasets_initial <- clono_datasets

    for (i in seq_len(nrow(clono_allData))) {
        clono_allData[i, 1] <- strsplit(as.character(clono_allData[i, 1]), " - ")[[1]][1]
    }

    for (j in seq_len(length(name))) {
        for (i in seq_len(nrow(clono_datasets[[name[j]]]))) {
            clono_datasets[[name[j]]][i, 1] <- strsplit(as.character(clono_datasets[[name[j]]][i, 1]), " - ")[[1]][1]
        }
    }


    if (gene == gene_clonotypes && allele == allele_clonotypes && take_gene == "Yes" && length(gene_clonotypes) > 0) {
        ####################################### All Data
        Repertoires_allData <- clono_allData %>%
            dplyr::group_by(clono_allData[["clonotype"]]) %>%
            dplyr::summarise(n = n())
        Repertoires_allData <- Repertoires_allData[order(-Repertoires_allData$n), ]
        Repertoires_allData <- cbind(Repertoires_allData, Freq = 100 * Repertoires_allData$n / nrow(clono_allData))

        ####################################### Separate Datasets
        Repertoires_datasets <- list()
        one_run <- function(j) {
            Repertoires_datasets[[name[j]]] <- clono_datasets[[name[j]]] %>%
                dplyr::group_by(clono_datasets[[name[j]]][["clonotype"]]) %>%
                dplyr::summarise(n = n())
            Repertoires_datasets[[name[j]]] <- Repertoires_datasets[[name[j]]][order(-Repertoires_datasets[[name[j]]]$n), ]
            Repertoires_datasets[[name[j]]] <- cbind(Repertoires_datasets[[name[j]]], Freq = 100 * Repertoires_datasets[[name[j]]]$n / nrow(clono_datasets[[name[j]]]))
            colnames(Repertoires_datasets[[name[j]]]) <- c("Gene", "N", "Freq")

            if (save_tables_individually) {
                filename <- paste0(e$output_folder, "/", "Repertoires_HighlySim_", g, "_", name[j], ".txt")
                write.table(Repertoires_datasets[[name[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
            }
            return(Repertoires_datasets[[name[j]]])
        }

        if (Sys.info()[1] == "Windows") {
            Repertoires_datasets <- lapply(seq_len(length(name)), one_run)
        } else {
            Repertoires_datasets <- lapply(seq_len(length(name)), one_run)
            #Repertoires_datasets <- parallel::mclapply(seq_len(length(name)), one_run, mc.cores = num_of_cores, mc.preschedule = TRUE)
        }

        names(Repertoires_datasets) <- name
    } else {
        # find the most frequent gene that exists in each specific clonotype

        ####################################### All Data
        freq_gene_name <- data.frame()

        if (take_gene == "Yes") {
            for (i in names(view_specific_clonotype_allData)) {
                if (i %in% clono_allData_initial$clonotype) {
                    a <- view_specific_clonotype_allData[[i]]
                    a <- setDT(a)
                    if (allele == FALSE) {
                        if (!all(!(stringr::str_detect(a[[gene]], "[*]")))) {
                            a2 <- strsplit(a[[gene]], "[*]")
                            a[[gene]] <- as.character(plyr::ldply(a2, function(s) {
                                t(data.frame(unlist(s)))
                            })[, 1])
                        }
                    }
                    ##change
                    freq_gene <- a[,by =a[[gene]], n =.N]
                    
                    # freq_gene <- a %>%
                    #     dplyr::group_by(a[[gene]]) %>%
                    #     dplyr::summarise(n = n())
                    freq_gene <- freq_gene[order(-freq_gene$N), ]
                    freq_gene_name[i, 1] <- freq_gene[1, 1]
                }
            }
        }
        colnames(freq_gene_name) <- c("Gene")
        freq_gene_name <- freq_gene_name %>%
            dplyr::group_by(freq_gene_name[["Gene"]]) %>%
            dplyr::summarise(n = n())

        freq_gene_name <- freq_gene_name[order(-freq_gene_name$n), ]
        freq_gene_name <- cbind(freq_gene_name, Freq = 100 * freq_gene_name$n / nrow(clono_allData))
        colnames(freq_gene_name) <- c("Gene", "N", "Freq")

        ####################################### Separate Datasets
        freq_gene_name_datasets <- list()
        one_run <- function(j) {
            freq_gene_name_datasets[[name[[j]]]] <- data.frame()
            if (take_gene == "Yes") {
                for (i in names(view_specific_clonotype_datasets[[name[j]]])) {
                    if (i %in% clono_datasets_initial[[name[j]]]$clonotype) {
                        a <- view_specific_clonotype_datasets[[name[j]]][[i]]
                        if (allele == FALSE) {
                            if (!all(!(stringr::str_detect(a[[gene]], "[*]")))) {
                                a2 <- strsplit(a[[gene]], "[*]")
                                a[[gene]] <- as.character(plyr::ldply(a2, function(s) {
                                    t(data.frame(unlist(s)))
                                })[, 1])
                            }
                        }
                        
                        freq_gene <- a[,by =a[[gene]], n =.N]
                        # freq_gene <- a %>%
                        #     dplyr::group_by(a[[gene]]) %>%
                        #     dplyr::summarise(n = n())
                        freq_gene <- freq_gene[order(-freq_gene$N), ]
                        freq_gene_name_datasets[[name[[j]]]][i, 1] <- freq_gene[1, 1]
                    }
                }

                colnames(freq_gene_name_datasets[[name[[j]]]]) <- c("Gene")
                freq_gene_name_datasets[[name[[j]]]] <- freq_gene_name_datasets[[name[[j]]]] %>%
                    dplyr::group_by(freq_gene_name_datasets[[name[[j]]]][["Gene"]]) %>%
                    dplyr::summarise(n = n())
                freq_gene_name_datasets[[name[j]]] <- freq_gene_name_datasets[[name[j]]][order(-freq_gene_name_datasets[[name[j]]]$n), ]
                freq_gene_name_datasets[[name[j]]] <- cbind(freq_gene_name_datasets[[name[j]]], Freq = 100 * freq_gene_name_datasets[[name[j]]]$n / nrow(clono_datasets[[name[j]]]))
                colnames(freq_gene_name_datasets[[name[j]]]) <- c("Gene", "N", "Freq")

                if (save_tables_individually) {
                    filename <- paste0(e$output_folder, "/", "Repertoires_HiglySim_", g, "_", name[j], ".txt")
                    fwrite(freq_gene_name_datasets[[name[j]]], file = filename, sep = "\t", row.names = FALSE, col.names = TRUE)
                }
            }

            return(freq_gene_name_datasets[[name[j]]])
        }

        if (Sys.info()[1] == "Windows") {
            freq_gene_name_datasets <- lapply(seq_len(length(name)), one_run)
        } else {
            freq_gene_name_datasets <- lapply(seq_len(length(name)), one_run)
            #freq_gene_name_datasets <- parallel::mclapply(seq_len(length(name)), one_run, mc.cores = num_of_cores, mc.preschedule = TRUE)
        }

        names(freq_gene_name_datasets) <- name

        Repertoires_allData <- freq_gene_name
        Repertoires_datasets <- freq_gene_name_datasets
    }

    colnames(Repertoires_allData) <- c("Gene", "N", "Freq")

    if (save_tables_individually) {
        filename <- paste0(e$output_folder, "/", "Repertoires_HighlySim_", g, "_", "All_Data", ".txt")
        fwrite(Repertoires_allData, file = filename, sep = "\t", row.names = FALSE, col.names = TRUE)
    }


    confirm <- paste0("Repertoires run!")

    result <- list(
        "Repertoires_allData" = Repertoires_allData,
        "Repertoires_datasets" = Repertoires_datasets,
        "confirm" = confirm
    )

    # log time end and memory used
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
    # cat(pryr::mem_used(), file = logFile, append = TRUE, sep = "\n")

    return(result)
}

######################################################################################################################################

repertoires_comparison <- function(Repertoires_allData, Repertoires_datasets, name, highly_sim, id) { # set name equal to the selected dataset
    # logfile
    # logFile<-e$logFile
    if (!highly_sim) {
        n <- "repertoires_comparison"
    } else {
        n <- "repertoires_comparison_higly_similar"
    }
    # cat(paste0(n, "\t"), file = logFile, append = TRUE)
    # cat(paste0(paste("Number of datasets: ", length(name), sep = ""), "\t"), file = logFile, append = TRUE)
    # cat(paste0(nrow(Repertoires_allData), "\t"), file = logFile, append = TRUE)
    # cat(paste0(ncol(Repertoires_allData), "\t"), file = logFile, append = TRUE)
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)

    unique_repertoires <- data.frame(Gene = Repertoires_allData$Gene, stringsAsFactors = FALSE)

    # for each dataset
    for (n in name) {
        ids_dataset <- which(Repertoires_datasets[[n]]$Gene %in% unique_repertoires$Gene)
        ids <- match(Repertoires_datasets[[n]]$Gene, unique_repertoires$Gene)[which(!(is.na(match(Repertoires_datasets[[n]]$Gene, unique_repertoires$Gene))))]
        unique_repertoires[[paste0(n, "_N/Total")]] <- NA
        unique_repertoires[[paste0(n, "_Freq")]] <- NA
        unique_repertoires[[paste0(n, "_N/Total")]][ids] <- paste0(Repertoires_datasets[[n]]$N[ids_dataset], "/", sum(Repertoires_datasets[[n]]$N))
        unique_repertoires[[paste0(n, "_Freq")]][ids] <- Repertoires_datasets[[n]]$Freq[ids_dataset]
    }

    # replace NA with 0
    unique_repertoires[is.na(unique_repertoires)] <- 0

    # mean freq
    unique_repertoires$Mean_Freq <- NA
    unique_repertoires$Mean_Freq <- apply(unique_repertoires[, seq(3, ncol(unique_repertoires), 2)], 1, function(x) mean(x))

    if (save_tables_individually) {
        if (highly_sim) {
            filename <- paste0(e$output_folder, "/", "highlySim_repertoires_comparison_table_", id, ".txt")
            write.table(unique_repertoires, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
        } else {
            filename <- paste0(e$output_folder, "/", "repertoires_comparison_table_", id, ".txt")
            write.table(unique_repertoires, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
        }
    }

    confirm <- paste0("Repertoires Comparison run!")

    result <- list("unique_repertoires" = unique_repertoires, "confirm" = confirm)

    # log time end and memory used
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
    # cat(pryr::mem_used(), file = logFile, append = TRUE, sep = "\n")

    return(result)
}

######################################################################################################################################

Multiple_value_comparison <- function(clono_allData, clono_datasets, allele_clonotypes, gene_clonotypes, view_specific_clonotype_allData, view_specific_clonotype_datasets, val1, val2, name, identity_groups) {
    used_columns <- e$used_columns
    # logfile
    # logFile<-e$logFile
    # cat(paste0("Multiple_value_comparison", "\t"), file = logFile, append = TRUE)
    # cat(paste0(paste(val1, val2, sep = ","), "\t"), file = logFile, append = TRUE)
    # cat(paste0(nrow(clono_allData), "\t"), file = logFile, append = TRUE)
    # cat(paste0(ncol(clono_allData), "\t"), file = logFile, append = TRUE)
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
    #save(clono_allData, clono_datasets, allele_clonotypes, gene_clonotypes, view_specific_clonotype_allData, view_specific_clonotype_datasets, val1, val2, name, identity_groups, file='./Multi_comparisonBcell.RData')
    val1_initial <- val1
    val2_initial <- val2
    
    val_initial <- c(val1_initial, val2_initial)

    if (val1 == "Molecular mass" || val1 == "pI") {
        val1 <- paste0("Junction.", val1)
    } else {
        val1 <- paste0("Summary.", val1)
    }

    val1 <- gsub(" ", ".", val1)
    val1 <- gsub("-", ".", val1)
    val1 <- gsub("%", ".", val1)

    if (val2 == "Molecular mass" || val2 == "pI") {
        val2 <- paste0("Junction.", val2)
    } else {
        val2 <- paste0("Summary.", val2)
    }

    val2 <- gsub(" ", ".", val2)
    val2 <- gsub("-", ".", val2)
    val2 <- gsub("%", ".", val2)

    if (!stringr::str_detect(val1, "allele") && stringr::str_detect(val1, "GENE")) {
        val1 <- paste0(val1, ".and.allele")
    }

    if (!stringr::str_detect(val2, "allele") && stringr::str_detect(val2, "GENE")) {
        val2 <- paste0(val2, ".and.allele")
    }

    Multiple_value_comparison_datasets <- list()

    for (i in seq_len(nrow(clono_allData))) {
        clono_allData[i, 1] <- strsplit(as.character(clono_allData[i, 1]), " - ")[[1]][1]
    }

    for (j in seq_len(length(name))) {
        for (i in seq_len(nrow(clono_datasets[[name[j]]]))) {
            clono_datasets[[name[j]]][i, 1] <- strsplit(as.character(clono_datasets[[name[j]]][i, 1]), " - ")[[1]][1]
        }
    }

    multi_allData <- c()

    val <- c(val1, val2)

    multi_datasets <- list()

    for (vals in seq_len(2)) {
        if (stringr::str_detect(val[vals], "GENE")) {
            gene <- val[vals]

            if (gene == gene_clonotypes && (stringr::str_detect(val_initial[vals], "allele")) == allele_clonotypes && !(is.null(gene_clonotypes))) {
                ####################################### All Data
                multi_allData <- cbind(multi_allData, clono_allData[["clonotype"]])
                colnames(multi_allData)[ncol(multi_allData)] <- gene

                ####################################### Seperate Datasets
                one_run <- function(j) {
                    multi_datasets[[name[j]]] <- cbind(multi_datasets[[name[j]]], clono_datasets[[name[j]]][["clonotype"]])
                    colnames(multi_datasets[[name[j]]])[ncol(multi_datasets[[name[j]]])] <- gene

                    return(multi_datasets[[name[j]]])
                }

                if (Sys.info()[1] == "Windows") {
                    multi_datasets <- lapply(seq_len(length(name)), one_run)
                } else {
                    multi_datasets <- lapply(seq_len(length(name)), one_run)
                    #multi_datasets <- parallel::mclapply(seq_len(length(name)), one_run, mc.cores = num_of_cores, mc.preschedule = TRUE)
                }

                names(multi_datasets) <- name
            } else {
                # find the most frequent gene that exists in each specific clonotype
                ####################################### All Data
                freq_gene_name <- data.frame()
                for (i in names(view_specific_clonotype_allData)) {
                    
                    a <- view_specific_clonotype_allData[[i]]
                    if ((stringr::str_detect(val_initial[vals], "allele") == FALSE)) {
                        if (!all(!(stringr::str_detect(a[[gene]], "[*]")))) {
                            a2 <- strsplit(a[[gene]], "[*]")
                            a[[gene]] <- as.character(plyr::ldply(a2, function(s) {
                                t(data.frame(unlist(s)))
                            })[, 1])
                        }
                    }
                    a$gene <- a[[gene]]
                    a <- setDT(a)
                    freq_gene <- a[, by = gene , .N]
                    # freq_gene <- a %>%
                    #     dplyr::group_by(a[[gene]]) %>%
                    #     dplyr::summarise(n = n())
                    freq_gene <- freq_gene[order(-freq_gene$N), ]
                    freq_gene_name[i, 1] <- freq_gene[1, 1]
                }

                colnames(freq_gene_name) <- gene

                multi_allData <- cbind(multi_allData, freq_gene_name[[gene]])
                colnames(multi_allData)[ncol(multi_allData)] <- gene
                
                ####################################### Seperate Datasets
                one_run <- function(j) {
                    freq_gene_name <- data.frame()
                    for (i in names(view_specific_clonotype_datasets[[name[j]]])) {
                        a <- view_specific_clonotype_datasets[[name[j]]][[i]]
                        if ((stringr::str_detect(val_initial[vals], "allele") == FALSE)) {
                            if (!all(!(stringr::str_detect(a[[gene]], "[*]")))) {
                                a2 <- strsplit(a[[gene]], "[*]")
                                a[[gene]] <- as.character(plyr::ldply(a2, function(s) {
                                    t(data.frame(unlist(s)))
                                })[, 1])
                            }
                        }
                        a$gene <- a[[gene]]
                        a <- setDT(a)
                        freq_gene <- a[, by = gene,.N]
                        # freq_gene <- a %>%
                        #     dplyr::group_by(a[[gene]]) %>%
                        #     dplyr::summarise(n = n())
                        freq_gene <- freq_gene[order(-freq_gene$N), ]
                        freq_gene_name[i, 1] <- freq_gene[1, 1]
                    }

                    colnames(freq_gene_name) <- gene

                    multi_datasets[[name[j]]] <- cbind(multi_datasets[[name[j]]], freq_gene_name[[gene]])
                    colnames(multi_datasets[[name[j]]])[ncol(multi_datasets[[name[j]]])] <- gene

                    return(multi_datasets[[name[j]]])
                }

                if (Sys.info()[1] == "Windows") {
                    multi_datasets <- lapply(seq_len(length(name)), one_run)
                } else {
                    multi_datasets <- lapply(seq_len(length(name)), one_run)
                    #multi_datasets <- parallel::mclapply(seq_len(length(name)), one_run, mc.cores = num_of_cores, mc.preschedule = TRUE)
                }

                names(multi_datasets) <- name
            }
        } else {
            a <- c()
            for (i in names(view_specific_clonotype_allData)) {
                a <- c(a, median(as.numeric(view_specific_clonotype_allData[[i]][[val[vals]]]), na.rm = TRUE))
            }
            if ((!is.null(identity_groups)) && (val[vals] == used_columns[["Summary"]][4])) {
                a <- as.numeric(a)
                temp <- a
                for (values in seq_len(nrow(identity_groups))) {
                    if (values == nrow(identity_groups)) {
                        index <- which(a >= identity_groups[values, 1] & a <= identity_groups[values, 2])
                    } else {
                        index <- which(a >= identity_groups[values, 1] & a < identity_groups[values, 2])
                    }
                    temp[index] <- identity_groups$label[values]
                }
                a <- temp
            }

            multi_allData <- cbind(multi_allData, a)
            colnames(multi_allData)[ncol(multi_allData)] <- val[vals]

            ####################################### Seperate Datasets
            one_run <- function(j) {
                a <- c()
                for (i in names(view_specific_clonotype_datasets[[name[j]]])) {
                    a <- c(a, median(as.numeric(view_specific_clonotype_datasets[[name[j]]][[i]][[val[vals]]]), na.rm = TRUE))
                }
                if ((!is.null(identity_groups)) && (val[vals] == used_columns[["Summary"]][4])) {
                    a <- as.numeric(a)
                    temp <- a
                    for (values in seq_len(nrow(identity_groups))) {
                        if (values == nrow(identity_groups)) {
                            index <- which(a >= identity_groups[values, 1] & a <= identity_groups[values, 2])
                        } else {
                            index <- which(a >= identity_groups[values, 1] & a < identity_groups[values, 2])
                        }
                        temp[index] <- identity_groups$label[values]
                    }
                    a <- temp
                }

                multi_datasets[[name[j]]] <- cbind(multi_datasets[[name[j]]], a)
                colnames(multi_datasets[[name[j]]])[ncol(multi_datasets[[name[j]]])] <- val[vals]
                return(multi_datasets[[name[j]]])
            }

            if (Sys.info()[1] == "Windows") {
                multi_datasets <- lapply(seq_len(length(name)), one_run)
            } else {
                multi_datasets <- lapply(seq_len(length(name)), one_run)
                #multi_datasets <- parallel::mclapply(seq_len(length(name)), one_run, mc.cores = num_of_cores, mc.preschedule = TRUE)
            }

            names(multi_datasets) <- name
        }
    }
    #multi_allData <- data.frame(multi_allData, stringsAsFactors = FALSE)
    multi_allData <- data.table(multi_allData)
    Multiple_value_comparison_allData <- multi_allData[, by = .(multi_allData[[val1]], multi_allData[[val2]]),  .N]
      
    # Multiple_value_comparison_allData <- multi_allData %>%
    #     dplyr::group_by(multi_allData[[val1]], multi_allData[[val2]]) %>%
    #     dplyr::summarise(n = n())
    Multiple_value_comparison_allData <- Multiple_value_comparison_allData[order(-Multiple_value_comparison_allData$N), ]
    Multiple_value_comparison_allData <- cbind(Multiple_value_comparison_allData, Freq = 100 * Multiple_value_comparison_allData$N / nrow(multi_allData))
    colnames(Multiple_value_comparison_allData) <- c(val1_initial, val2_initial, "N", "Freq")

    ####################################### Seperate Datasets
    one_run <- function(j) {
        multi_datasets[[name[j]]] <- data.table(multi_datasets[[name[j]]], stringsAsFactors = FALSE)

        Multiple_value_comparison_datasets[[name[j]]] <- multi_datasets[[name[j]]][, by = .(multi_datasets[[name[j]]][[val1]], multi_datasets[[name[j]]][[val2]]), .N]
        # Multiple_value_comparison_datasets[[name[j]]] <- multi_datasets[[name[j]]] %>%
        #     dplyr::group_by(multi_datasets[[name[j]]][[val1]], multi_datasets[[name[j]]][[val2]]) %>%
        #     dplyr::summarise(n = n())
        Multiple_value_comparison_datasets[[name[j]]] <- Multiple_value_comparison_datasets[[name[j]]][order(-Multiple_value_comparison_datasets[[name[j]]]$N), ]
        Multiple_value_comparison_datasets[[name[j]]] <- cbind(Multiple_value_comparison_datasets[[name[j]]], Freq = 100 * Multiple_value_comparison_datasets[[name[j]]]$N / nrow(multi_datasets[[name[j]]]))
        colnames(Multiple_value_comparison_datasets[[name[j]]]) <- c(val1_initial, val2_initial, "N", "Freq")

        #Multiple_value_comparison_datasets[[name[j]]] <- data.frame(Multiple_value_comparison_datasets[[name[j]]], stringsAsFactors = FALSE)

        if (save_tables_individually) {
            filename <- paste0(e$output_folder, "/", "Multiple_value_comparison_", stringr::str_replace(val1_initial, "%", ""), "_", stringr::str_replace(val2_initial, "%", ""), "_", name[j], ".txt")
            fwrite(Multiple_value_comparison_datasets[[name[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
        }

        return(Multiple_value_comparison_datasets[[name[j]]])
    }

    if (Sys.info()[1] == "Windows") {
        Multiple_value_comparison_datasets <- lapply(seq_len(length(name)), one_run)
    } else {
        Multiple_value_comparison_datasets <- lapply(seq_len(length(name)), one_run)
        #Multiple_value_comparison_datasets <- parallel::mclapply(seq_len(length(name)), one_run, mc.cores = num_of_cores, mc.preschedule = TRUE)
    }

    names(Multiple_value_comparison_datasets) <- name

    if (save_tables_individually) {
        filename <- paste0(e$output_folder, "/", "Multiple_value_comparison_", stringr::str_replace(val1_initial, "%", ""), "_", stringr::str_replace(val2_initial, "%", ""), "_", "All_Data", ".txt")
        fwrite(Multiple_value_comparison_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
    }

    confirm <- paste0("Multiple_value_comparison ", val1_initial, " - ", val2_initial, " run!")

    result <- list("Multiple_value_comparison_allData" = Multiple_value_comparison_allData, "Multiple_value_comparison_datasets" = Multiple_value_comparison_datasets, "confirm" = confirm)

    # log time end and memory used
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
    # cat(pryr::mem_used(), file = logFile, append = TRUE, sep = "\n")

    return(result)
}

######################################################################################################################################

Multiple_value_comparison_highly_similar <- function(clono_allData, clono_datasets, allele_clonotypes, gene_clonotypes, view_specific_clonotype_allData, view_specific_clonotype_datasets, val1, val2, name, identity_groups) {
    # logfile
  
    # logFile<-e$logFile
    # cat(paste0("Multiple_value_comparison_highly_similar", "\t"), file = logFile, append = TRUE)
    # cat(paste0(paste(val1, val2, sep = ","), "\t"), file = logFile, append = TRUE)
    # cat(paste0(nrow(clono_allData), "\t"), file = logFile, append = TRUE)
    # cat(paste0(ncol(clono_allData), "\t"), file = logFile, append = TRUE)
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)

    val1_initial <- val1
    val2_initial <- val2

    val_initial <- c(val1_initial, val2_initial)

    if (val1 == "Molecular mass" || val1 == "pI") {
        val1 <- paste0("Junction.", val1)
    } else {
        val1 <- paste0("Summary.", val1)
    }

    val1 <- gsub(" ", ".", val1)
    val1 <- gsub("-", ".", val1)
    val1 <- gsub("%", ".", val1)

    if (val2 == "Molecular mass" || val2 == "pI") {
        val2 <- paste0("Junction.", val2)
    } else {
        val2 <- paste0("Summary.", val2)
    }

    val2 <- gsub(" ", ".", val2)
    val2 <- gsub("-", ".", val2)
    val2 <- gsub("%", ".", val2)

    if (!stringr::str_detect(val1, "allele") && stringr::str_detect(val1, "GENE")) {
        val1 <- paste0(val1, ".and.allele")
    }

    if (!stringr::str_detect(val2, "allele") && stringr::str_detect(val2, "GENE")) {
        val2 <- paste0(val2, ".and.allele")
    }

    Multiple_value_comparison_datasets <- list()

    for (i in seq_len(nrow(clono_allData))) {
        clono_allData[i, 1] <- strsplit(as.character(clono_allData[i, 1]), " - ")[[1]][1]
    }

    for (j in seq_len(length(name))) {
        for (i in seq_len(nrow(clono_datasets[[name[j]]]))) {
            clono_datasets[[name[j]]][i, 1] <- strsplit(as.character(clono_datasets[[name[j]]][i, 1]), " - ")[[1]][1]
        }
    }

    multi_allData <- c()

    val <- c(val1, val2)

    multi_datasets <- list()

    for (vals in seq_len(2)) {
        if (stringr::str_detect(val[vals], "GENE")) {
            gene <- val[vals]

            if (gene == gene_clonotypes && (stringr::str_detect(val_initial[vals], "allele")) == allele_clonotypes && !(is.null(gene_clonotypes))) {
                ####################################### All Data
                multi_allData <- cbind(multi_allData, clono_allData[["clonotype"]])
                colnames(multi_allData)[ncol(multi_allData)] <- gene
                multi_allData <- data.frame(multi_allData)
                ####################################### Seperate Datasets
                one_run <- function(j) {
                    multi_datasets[[name[j]]] <- cbind(multi_datasets[[name[j]]], clono_datasets[[name[j]]][["clonotype"]])
                    colnames(multi_datasets[[name[j]]])[ncol(multi_datasets[[name[j]]])] <- gene

                    return(multi_datasets[[name[j]]])
                }

                if (Sys.info()[1] == "Windows") {
                    multi_datasets <- lapply(seq_len(length(name)), one_run)
                } else {
                    multi_datasets <- lapply(seq_len(length(name)), one_run)
                    #multi_datasets <- parallel::mclapply(seq_len(length(name)), one_run, mc.cores = num_of_cores, mc.preschedule = TRUE)
                }

                names(multi_datasets) <- name
            } else {
                # find the most frequent gene that exists in each specific clonotype
                ####################################### All Data
                freq_gene_name <- data.frame()
                id <- 0
                for (i in seq_len(nrow(clono_allData))) {
                    prev_clono <- as.numeric(strsplit(as.character(clono_allData$prev_cluster[i]), " ")[[1]][2:length(strsplit(as.character(clono_allData$prev_cluster[i]), " ")[[1]])])
                    a <- view_specific_clonotype_allData[[prev_clono[1]]]
                    if (length(prev_clono) > 1) {
                        for (cl in 2:length(prev_clono)) {
                            a <- rbind(a, view_specific_clonotype_allData[[prev_clono[cl]]])
                        }
                    }
                    if ((stringr::str_detect(val_initial[vals], "allele") == FALSE)) {
                        if (!all(!(stringr::str_detect(a[[gene]], "[*]")))) {
                            a2 <- strsplit(a[[gene]], "[*]")
                            a[[gene]] <- as.character(plyr::ldply(a2, function(s) {
                                t(data.frame(unlist(s)))
                            })[, 1])
                        }
                    }
                    freq_gene <- a[ ,by = gene, .N]
                    # freq_gene <- a %>%
                    #     dplyr::group_by(a[[gene]]) %>%
                    #     dplyr::summarise(n = n())
                    freq_gene <- freq_gene[order(-freq_gene$N), ]
                    freq_gene_name[i, 1] <- freq_gene[1, 1]
                }
                colnames(freq_gene_name) <- gene

                multi_allData <- cbind(multi_allData, freq_gene_name[[gene]])
                colnames(multi_allData)[ncol(multi_allData)] <- gene

                ####################################### Seperate Datasets
                one_run <- function(j) {
                    freq_gene_name <- data.frame()
                    for (i in seq_len(nrow(clono_datasets[[name[j]]]))) {
                        prev_clono <- as.numeric(strsplit(as.character(clono_datasets[[name[j]]]$prev_cluster[i]), " ")[[1]][2:length(strsplit(as.character(clono_datasets[[name[j]]]$prev_cluster[i]), " ")[[1]])])
                        prev_clono <- prev_clono[!is.na(prev_clono)]
                        a <- view_specific_clonotype_datasets[[name[j]]][[prev_clono[1]]]
                        if (length(prev_clono) > 1) {
                            for (cl in 2:length(prev_clono)) {
                                a <- rbind(a, view_specific_clonotype_datasets[[name[j]]][[prev_clono[cl]]])
                            }
                        }
                        if ((stringr::str_detect(val_initial[vals], "allele") == FALSE)) {
                            if (!all(!(stringr::str_detect(a[[gene]], "[*]")))) {
                                a2 <- strsplit(a[[gene]], "[*]")
                                a[[gene]] <- as.character(plyr::ldply(a2, function(s) {
                                    t(data.frame(unlist(s)))
                                })[, 1])
                            }
                        }
                        
                        freq_gene <- a[ ,by = gene ,.N]
                        # freq_gene <- a %>%
                        #     dplyr::group_by(a[[gene]]) %>%
                        #     dplyr::summarise(n = n())
                        freq_gene <- freq_gene[order(-freq_gene$N), ]
                        freq_gene_name[i, 1] <- freq_gene[1, 1]
                    }
                    colnames(freq_gene_name) <- gene

                    multi_datasets[[name[j]]] <- cbind(multi_datasets[[name[j]]], freq_gene_name[[gene]])
                    colnames(multi_datasets[[name[j]]])[ncol(multi_datasets[[name[j]]])] <- gene

                    return(multi_datasets[[name[j]]])
                }

                if (Sys.info()[1] == "Windows") {
                    multi_datasets <- lapply(seq_len(length(name)), one_run)
                } else {
                    multi_datasets <- lapply(seq_len(length(name)), one_run)
                    #multi_datasets <- parallel::mclapply(seq_len(length(name)), one_run, mc.cores = num_of_cores, mc.preschedule = TRUE)
                }

                names(multi_datasets) <- name
            }
        } else {
            a <- c()
            for (i in seq_len(nrow(clono_allData))) {
                prev_clono <- as.numeric(strsplit(as.character(clono_allData$prev_cluster[i]), " ")[[1]][2:length(strsplit(as.character(clono_allData$prev_cluster[i]), " ")[[1]])])
                prev_clono <- prev_clono[!is.na(prev_clono)]
                view <- view_specific_clonotype_allData[[prev_clono[1]]]
                if (length(prev_clono) > 1) {
                    for (cl in 2:length(prev_clono)) {
                        view <- rbind(view, view_specific_clonotype_allData[[prev_clono[cl]]])
                    }
                }
                a <- c(a, median(as.numeric(view[[val[vals]]]), na.rm = TRUE))
            }

            if ((!is.null(identity_groups)) && (val[vals] == used_columns[["Summary"]][4])) {
                a <- as.numeric(a)
                temp <- a
                for (values in seq_len(nrow(identity_groups))) {
                    if (values == nrow(identity_groups)) {
                        index <- which(a >= identity_groups[values, 1] & a <= identity_groups[values, 2])
                    } else {
                        index <- which(a >= identity_groups[values, 1] & a < identity_groups[values, 2])
                    }
                    temp[index] <- identity_groups$label[values]
                }
                a <- temp
            }

            multi_allData <- cbind(multi_allData, a)
            colnames(multi_allData)[ncol(multi_allData)] <- val[vals]

            ####################################### Seperate Datasets
            one_run <- function(j) {
                a <- c()
                id <- 0
                for (i in seq_len(nrow(clono_datasets[[name[j]]]))) {
                    prev_clono <- as.numeric(strsplit(as.character(clono_datasets[[name[j]]]$prev_cluster[i]), " ")[[1]][2:length(strsplit(as.character(clono_datasets[[name[j]]]$prev_cluster[i]), " ")[[1]])])
                    prev_clono <- prev_clono[!is.na(prev_clono)]
                    view <- view_specific_clonotype_datasets[[name[j]]][[prev_clono[1]]]
                    if (length(prev_clono) > 1) {
                        for (cl in 2:length(prev_clono)) {
                            view <- rbind(view, view_specific_clonotype_datasets[[name[j]]][[prev_clono[cl]]])
                        }
                    }
                    a <- c(a, median(as.numeric(view[[val[vals]]]), na.rm = TRUE))
                }

                if ((!is.null(identity_groups)) && (val[vals] == used_columns[["Summary"]][4])) {
                    a <- as.numeric(a)
                    temp <- a
                    for (values in seq_len(nrow(identity_groups))) {
                        if (values == nrow(identity_groups)) {
                            index <- which(a >= identity_groups[values, 1] & a <= identity_groups[values, 2])
                        } else {
                            index <- which(a >= identity_groups[values, 1] & a < identity_groups[values, 2])
                        }
                        temp[index] <- identity_groups$label[values]
                    }
                    a <- temp
                }

                multi_datasets[[name[j]]] <- cbind(multi_datasets[[name[j]]], a)
                colnames(multi_datasets[[name[j]]])[ncol(multi_datasets[[name[j]]])] <- val[vals]
                return(multi_datasets[[name[j]]])
            }

            if (Sys.info()[1] == "Windows") {
                multi_datasets <- lapply(seq_len(length(name)), one_run)
            } else {
                multi_datasets <- lapply(seq_len(length(name)), one_run)
                #multi_datasets <- parallel::mclapply(seq_len(length(name)), one_run, mc.cores = num_of_cores, mc.preschedule = TRUE)
            }

            names(multi_datasets) <- name
        }
    }
    multi_allData <- setDT(data.frame(multi_allData, stringsAsFactors = FALSE))

    Multiple_value_comparison_allData <- multi_allData[ , by = .(multi_allData[[val1]], multi_allData[[val2]]) , .N]
    # Multiple_value_comparison_allData <- multi_allData %>%
    #     dplyr::group_by(multi_allData[[val1]], multi_allData[[val2]]) %>%
    #     dplyr::summarise(n = n())
    Multiple_value_comparison_allData <- Multiple_value_comparison_allData[order(-Multiple_value_comparison_allData$N), ]
    Multiple_value_comparison_allData <- cbind(Multiple_value_comparison_allData, Freq = 100 * Multiple_value_comparison_allData$N / nrow(multi_allData))
    colnames(Multiple_value_comparison_allData) <- c(val1_initial, val2_initial, "N", "Freq")

    ####################################### Seperate Datasets
    one_run <- function(j) {
        multi_datasets[[name[j]]] <- setDT(data.frame(multi_datasets[[name[j]]], stringsAsFactors = FALSE))

        Multiple_value_comparison_datasets[[name[j]]] <- multi_datasets[[name[j]]][ , by = .(multi_datasets[[name[j]]][[val1]], multi_datasets[[name[j]]][[val2]]), .N]
        # Multiple_value_comparison_datasets[[name[j]]] <- multi_datasets[[name[j]]] %>%
        #     dplyr::group_by(multi_datasets[[name[j]]][[val1]], multi_datasets[[name[j]]][[val2]]) %>%
        #     dplyr::summarise(n = n())
        Multiple_value_comparison_datasets[[name[j]]] <- Multiple_value_comparison_datasets[[name[j]]][order(-Multiple_value_comparison_datasets[[name[j]]]$N), ]
        Multiple_value_comparison_datasets[[name[j]]] <- cbind(Multiple_value_comparison_datasets[[name[j]]], Freq = 100 * Multiple_value_comparison_datasets[[name[j]]]$N / nrow(multi_datasets[[name[j]]]))
        colnames(Multiple_value_comparison_datasets[[name[j]]]) <- c(val1_initial, val2_initial, "N", "Freq")

        Multiple_value_comparison_datasets[[name[j]]] <- data.frame(Multiple_value_comparison_datasets[[name[j]]], stringsAsFactors = FALSE)

        if (save_tables_individually) {
            filename <- paste0(e$output_folder, "/", "Multiple_value_comparison_highly_similar", stringr::str_replace(val1_initial, "%", ""), "_", stringr::str_replace(val2_initial, "%", ""), "_", name[j], ".txt")
            fwrite(Multiple_value_comparison_datasets[[name[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
        }

        return(Multiple_value_comparison_datasets[[name[j]]])
    }

    if (Sys.info()[1] == "Windows") {
        Multiple_value_comparison_datasets <- lapply(seq_len(length(name)), one_run)
    } else {
        Multiple_value_comparison_datasets <- lapply(seq_len(length(name)), one_run)
        #Multiple_value_comparison_datasets <- parallel::mclapply(seq_len(length(name)), one_run, mc.cores = num_of_cores, mc.preschedule = TRUE)
    }

    names(Multiple_value_comparison_datasets) <- name

    if (save_tables_individually) {
        filename <- paste0(e$output_folder, "/", "Multiple_value_comparison_highly_similar", stringr::str_replace(val1_initial, "%", ""), "_", stringr::str_replace(val2_initial, "%", ""), "_", "All_Data", ".txt")
        fwrite(Multiple_value_comparison_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
    }

    confirm <- paste0("Multiple_value_comparison ", val1_initial, " - ", val2_initial, " run!")

    result <- list("Multiple_value_comparison_allData" = Multiple_value_comparison_allData, "Multiple_value_comparison_datasets" = Multiple_value_comparison_datasets, "confirm" = confirm)

    # log time end and memory used
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
    # cat(pryr::mem_used(), file = logFile, append = TRUE, sep = "\n")

    return(result)
}

######################################################################################################################################

createFrequencyTableCDR3 <- function(region_name, input, name, regionLength, FtopN, topClonotypesAlldata, topClonotypesDatasets, gene, junction, allele) {
    # logfile
    # logFile<-e$logFile
    # cat(paste0("createFrequencyTableCDR3", "\t"), file = logFile, append = TRUE)
    # cat(paste0(paste(region_name, paste0("Top N: ", FtopN), sep = ","), "\t"), file = logFile, append = TRUE)
    # cat(paste0(nrow(input), "\t"), file = logFile, append = TRUE)
    # cat(paste0(ncol(input), "\t"), file = logFile, append = TRUE)
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
    #save(region_name, input, name, regionLength, FtopN, topClonotypesAlldata, topClonotypesDatasets, gene, junction, allele, file = './3_createFrequency.RData')
    input_initial <- input

    if (FtopN) {
        ################################# combine all conent of the top N clonotypes
        input_topN <- c()
        for (i in seq_len(nrow(topClonotypesAlldata))) {
            cl <- strsplit(topClonotypesAlldata$clonotype[i], " - ")
            if (is.na(cl[[1]][1])) {
                cl[[1]][1] <- ""
            }
            if (is.na(cl[[1]][2])) {
                cl[[1]][2] <- ""
            }
            input_topN <- rbind(input_topN, viewClonotypes(input, allele, gene, junction, cl[[1]][1], cl[[1]][2]))
        }
        input <- input_topN
    }

    if (region_name == "CDR3") region_name <- "CDR3.IMGT"
    region_name <- paste0("IMGT.gapped.AA.sequences.", region_name)

    ###################################### all data #######################################
    # select AA juction or CDR3
    region <- unique(input[[region_name]])

    region_with_specific_length <- input %>% dplyr::filter(str_length(input[[region_name]]) == regionLength)
    region_with_specific_length <- region_with_specific_length[[region_name]]

    region_split <- strsplit(region, "")
    region <- as.data.frame(region)

    ancestral <- as.data.frame(c("I", "L", "V", "A", "M", "C", "T", "S", "H", "K", "R", "E", "D", "P", "G", "Y", "F", "W", "Q", "N"))
    colnames(ancestral) <- "x"

    new_list <- list()
    k <- 0
    for (i in seq_len(length(region_split))) {
        if (length(region_split[[i]]) == regionLength) {
            k <- k + 1
            new_list[[k]] <- region_split[[i]]
        }
    }

    region_new <- as.data.frame(new_list)
    region_new <- data.frame(t(region_new))
    row.names(region_new) <- NULL

    if (length(region_new) > 0) {

        # remove factors
        region_new <- region_new %>% mutate_if(is.factor, as.character)
        ancestral <- ancestral %>% mutate_if(is.factor, as.character)
        a <- as.data.frame(region_new %>% dplyr::group_by(x = region_new[, 1]) %>% dplyr::summarise(n = n()))
        table_count <- join(as.data.frame(ancestral), a, type = "left", by = "x")

        for (i in 2:ncol(region_new)) {
            a <- as.data.frame(region_new %>% dplyr::group_by(x = region_new[, i]) %>% dplyr::summarise(n = n()))
            table_count <- join(table_count, a, type = "left", by = "x")
        }

        table_count[is.na(table_count)] <- 0

        row.names(table_count) <- table_count[, 1]

        # Create frequency table
        table_freq <- table_count
        table_freq[, 2:ncol(table_count)] <- 100 * table_count[, 2:ncol(table_count)] / k

        colnames(table_freq) <- c("AA", seq_len((ncol(table_freq) - 1)))
        colnames(table_count) <- c("AA", seq_len((ncol(table_count) - 1)))

        if (region_name == paste0("IMGT.gapped.AA.sequences.", "CDR3.IMGT")) {
            if ((ncol(table_count) - 1) == 13) {
                a <- 105:117
            } else if ((ncol(table_count) - 1) == 12) {
                a <- c(105:110, 112:117)
            } else if ((ncol(table_count) - 1) == 11) {
                a <- c(105:110, 113:117)
            } else if ((ncol(table_count) - 1) == 10) {
                a <- c(105:109, 113:117)
            } else if ((ncol(table_count) - 1) == 9) {
                a <- c(105:109, 114:117)
            } else if ((ncol(table_count) - 1) == 8) {
                a <- c(105:108, 114:117)
            } else if ((ncol(table_count) - 1) == 7) {
                a <- c(105:108, 115:117)
            } else if ((ncol(table_count) - 1) == 6) {
                a <- c(105:107, 115:117)
            } else if ((ncol(table_count) - 1) == 5) a <- c(105:107, 116:117)

            colnames(table_count) <- c("AA", a)
            colnames(table_freq) <- c("AA", a)
        }
    } else {
        table_freq <- c()
        table_count <- c()
        warning("No data for this")
    }

    ###################################### Separate datasets #######################################
    table_count_datasets <- list()
    table_freq_datasets <- list()
    region_with_specific_length_dataset <- list()

    for (j in seq_len(length(name))) {
        namej <- name[j]
        input_dataset <- input_initial %>% dplyr::filter(input_initial$dataName == name[j])

        region_with_specific_length_dataset[[name[j]]] <- input_dataset %>% dplyr::filter(str_length(input_dataset[[region_name]]) == regionLength)
        region_with_specific_length_dataset[[name[j]]] <- region_with_specific_length_dataset[[name[j]]][[region_name]]

        if (FtopN) {
            ################################# combine all conent of the top N clonotypes
            # thhe function should take allele,gene,junction as input!!!!!!!!!!!!

            input_topN <- c()
            for (i in seq_len(nrow(topClonotypesDatasets[[name[j]]]))) {
                cl <- strsplit(topClonotypesDatasets[[name[j]]]$clonotype[i], " - ")
                if (is.na(cl[[1]][1])) {
                    cl[[1]][1] <- ""
                }
                if (is.na(cl[[1]][2])) {
                    cl[[1]][2] <- ""
                }
                input_topN <- rbind(input_topN, viewClonotypes(input_dataset, allele, gene, junction, cl[[1]][1], cl[[1]][2]))
            }
            input_dataset <- input_topN
        }
        region <- unique(input_dataset[[region_name]])
        region_split <- strsplit(region, "")
        region <- as.data.frame(region)

        ancestral <- as.data.frame(c("I", "L", "V", "A", "M", "C", "T", "S", "H", "K", "R", "E", "D", "P", "G", "Y", "F", "W", "Q", "N"))
        colnames(ancestral) <- "x"

        # take the region with length=region
        new_list <- list()
        k <- 0
        for (i in seq_len(length(region_split))) {
            if (length(region_split[[i]]) == regionLength) {
                k <- k + 1
                new_list[[k]] <- region_split[[i]]
            }
        }

        name[j] <- namej

        region_new <- as.data.frame(new_list)
        region_new <- data.frame(t(region_new))
        row.names(region_new) <- NULL

        if (length(region_new) > 0) {
            # remove factors
            region_new <- region_new %>% mutate_if(is.factor, as.character)
            ancestral <- ancestral %>% mutate_if(is.factor, as.character)
            a <- as.data.frame(region_new %>% dplyr::group_by(x = region_new[, 1]) %>% dplyr::summarise(n = n()))
            table_count_datasets[[name[j]]] <- join(as.data.frame(ancestral), a, type = "left", by = "x")

            for (i in 2:ncol(region_new)) {
                a <- as.data.frame(region_new %>% dplyr::group_by(x = region_new[, i]) %>% dplyr::summarise(n = n()))
                table_count_datasets[[name[j]]] <- join(table_count_datasets[[name[j]]], a, type = "left", by = "x")
            }

            table_count_datasets[[name[j]]][is.na(table_count_datasets[[name[j]]])] <- 0
            row.names(table_count_datasets[[name[j]]]) <- table_count_datasets[[name[j]]][, 1]

            # Create frequency table
            table_freq_datasets[[name[j]]] <- table_count_datasets[[name[j]]]
            table_freq_datasets[[name[j]]][, 2:ncol(table_count_datasets[[name[j]]])] <- 100 * table_count_datasets[[name[j]]][, 2:ncol(table_count_datasets[[name[j]]])] / k

            colnames(table_count_datasets[[name[j]]]) <- c("AA", seq_len((ncol(table_count_datasets[[name[j]]]) - 1)))
            colnames(table_freq_datasets[[name[j]]]) <- c("AA", seq_len((ncol(table_freq_datasets[[name[j]]]) - 1)))

            if (region_name == paste0("IMGT.gapped.AA.sequences.", "CDR3.IMGT")) {
                if ((ncol(table_count_datasets[[name[j]]]) - 1) == 13) {
                    a <- 105:117
                } else if ((ncol(table_count_datasets[[name[j]]]) - 1) == 12) {
                    a <- c(105:110, 112:117)
                } else if ((ncol(table_count_datasets[[name[j]]]) - 1) == 11) {
                    a <- c(105:110, 113:117)
                } else if ((ncol(table_count_datasets[[name[j]]]) - 1) == 10) {
                    a <- c(105:109, 113:117)
                } else if ((ncol(table_count_datasets[[name[j]]]) - 1) == 9) {
                    a <- c(105:109, 114:117)
                } else if ((ncol(table_count_datasets[[name[j]]]) - 1) == 8) {
                    a <- c(105:108, 114:117)
                } else if ((ncol(table_count_datasets[[name[j]]]) - 1) == 7) {
                    a <- c(105:108, 115:117)
                } else if ((ncol(table_count_datasets[[name[j]]]) - 1) == 6) {
                    a <- c(105:107, 115:117)
                } else if ((ncol(table_count_datasets[[name[j]]]) - 1) == 5) a <- c(105:107, 116:117)

                colnames(table_count_datasets[[name[j]]]) <- c("AA", a)
                colnames(table_freq_datasets[[name[j]]]) <- c("AA", a)
            }
        } else {
            table_freq_datasets[[name[j]]] <- c()
            table_count_datasets[[name[j]]] <- c()
        }
    }

    confirm <- paste0("Frequency tables run!")

    result <- list("region_with_specific_length_dataset" = region_with_specific_length_dataset, "region_with_specific_length" = region_with_specific_length, "table_count" = table_count, "table_freq" = table_freq, "table_count_datasets" = table_count_datasets, "table_freq_datasets" = table_freq_datasets, "confirm" = confirm)

    # log time end and memory used
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
    # cat(pryr::mem_used(), file = logFile, append = TRUE, sep = "\n")

    return(result)
}

######################################################################################################################################

createLogo <- function(table_count, table_count_datasets, name) {
    # logfile
    # logFile<-e$logFile
    # cat(paste0("createLogo", "\t"), file = logFile, append = TRUE)
    # cat(paste0("logo plot", "\t"), file = logFile, append = TRUE)
    # cat(paste0(nrow(table_count), "\t"), file = logFile, append = TRUE)
    # cat(paste0(ncol(table_count), "\t"), file = logFile, append = TRUE)
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
    #save(table_count,table_count_datasets, file = './logo_input.RData')
    # Create color matrix
    mat <- matrix(nrow = 20, ncol = 2)
    mat[1, ] <- c("I", "#0000FF")
    mat[2, ] <- c("L", "#0000FF")
    mat[3, ] <- c("V", "#0000FF")
    mat[4, ] <- c("A", "#0000FF")

    mat[5, ] <- c("M", "#C6E2FF")
    mat[6, ] <- c("C", "#C6E2FF")

    mat[7, ] <- c("T", "#54FF9F")
    mat[8, ] <- c("S", "#54FF9F")

    mat[9, ] <- c("H", "#FF0000")
    mat[10, ] <- c("K", "#FF0000")
    mat[11, ] <- c("R", "#FF0000")

    mat[12, ] <- c("E", "#FFD700")
    mat[13, ] <- c("D", "#FFD700")

    mat[14, ] <- c("P", "#FFD700")

    mat[15, ] <- c("G", "#00EE00")

    mat[16, ] <- c("Y", "#C1FFC1")

    mat[17, ] <- c("F", "#1E90FF")

    mat[18, ] <- c("W", "#BA55D3")

    mat[19, ] <- c("Q", "#ED9121")
    mat[20, ] <- c("N", "#ED9121")

    ##########################

    if (!is.null(table_count)) {
        p <- motifStack::pcm2pfm(table_count)
        color_set <- c()
        for (i in seq_len(length(rownames(p)))) {
            color <- which(mat[, 1] == rownames(p)[i])
            color_set <- c(color_set, mat[color, 2])
        }

        names(color_set) <- row.names(p)

        motif_all <- new("pcm", mat = as.matrix(p), name = "")
        motif_all$color <- color_set

        motif_datasets <- list()
        for (j in seq_len(length(name))) {
            if (!is.null(table_count_datasets[[name[j]]])) {
                if ("AA" %in% colnames(table_count_datasets[[name[j]]])) {
                    table_count_datasets[[name[j]]] <- table_count_datasets[[name[j]]][, 2:ncol(table_count_datasets[[name[j]]])]
                }
                p <- motifStack::pcm2pfm(table_count_datasets[[name[j]]])
                color_set <- c()
                for (i in seq_len(length(rownames(p)))) {
                    color <- which(mat[, 1] == rownames(p)[i])
                    color_set <- c(color_set, mat[color, 2])
                }
                names(color_set) <- row.names(p)


                motif <- new("pcm", mat = as.matrix(p), name = "")
                motif$color <- color_set

                motif_datasets[[name[j]]] <- motif
            } else {
                motif_datasets[[name[j]]] <- c()
            }
        }
    } else {
        motif_all <- c()
        motif_datasets <- c()
    }


    confirm <- "Logo run!"

    result <- list("motif_all" = motif_all, "motif_datasets" = motif_datasets, "confirm" = confirm)

    # log time end and memory used
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
    # cat(pryr::mem_used(), file = logFile, append = TRUE, sep = "\n")

    return(result)
}

######################################################################################################################################

alignment <- function(clonotype_type, input, region, germline, name, only_one_germline, use_genes_germline, Tcell, AAorNtAlignment, clono_allData, clono_datasets, view_specific_clonotype_allData, view_specific_clonotype_datasets, topNClono, FtopN, thrClono, Fthr, highly) {
  used_columns <- e$used_columns
 
  # logfile
  # logFile<-e$logFile
  # 
  # cat(paste0("alignment", "\t"), file = logFile, append = TRUE)
  # cat(paste0(paste(region, AAorNtAlignment, "top", topNClono, "clonotypes", sep = ","), "\t"), file = logFile, append = TRUE)
  # cat(paste0(nrow(input), "\t"), file = logFile, append = TRUE)
  # cat(paste0(ncol(input), "\t"), file = logFile, append = TRUE)
  # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
  
  if (clonotype_type == "Sequence") {
    colnames(clono_allData)[1] <- "clonotype"
    clono_datasets <- lapply(clono_datasets, function(df) {
      colnames(df)[1] <- "clonotype"
      return(df)
    })
  
    names(view_specific_clonotype_allData) <- lapply(view_specific_clonotype_allData, function(df) {
      levels(as.factor(df$cluster_id))  
    })
    
    # Iterate over each inner list within the main list
    view_specific_clonotype_datasets <- lapply(view_specific_clonotype_datasets, function(inner_list) {
      
      # Apply the function to each dataframe within the inner list
      names(inner_list) <- sapply(inner_list, function(df) {
        levels(as.factor(df$cluster_id))[1]  # Extract the first level of the cluster_id column
      })
      
      return(inner_list)  # Return the modified inner list
    })
    
     }
  
  
  
  if (AAorNtAlignment == "aa") {
    file <- "IMGT.gapped.AA.sequences."
  } else {
    file <- "IMGT.gapped.nt.sequences."
  }
  
  if (region == "CDR3") region <- "JUNCTION"
  region <- paste0(file, region)
  
  max_length_region <- max(str_length(input[[region]]))
  
  if (Tcell == FALSE && only_one_germline == FALSE) {
    type <- strsplit(strsplit(as.character(input[[used_columns[["Summary"]][3]]][1]), " ")[[1]][2], "V")[[1]][1]
    
    if ((type == "IGK") | (type == "IGL")) {
      germline_file <- paste0("Germline_sequences_alignments_", "IGK", "V_", AAorNtAlignment, ".csv")
      Tgermlines <- as.data.frame(fread(system.file("extdata/param", germline_file,
                                                    package = "tripr", mustWork = TRUE
      ),
      sep = ";", stringsAsFactors = FALSE,
      colClasses = c("character")
      ))
      
      if (AAorNtAlignment == "aa") {
        Tgermlines[, 113:117] <- "."
      } else {
        Tgermlines[, 336:351] <- "."
      }
      
      germline_file <- paste0("Germline_sequences_alignments_", "IGL", "V_", AAorNtAlignment, ".csv")
      te <- as.data.frame(fread(system.file("extdata/param", germline_file,
                                            package = "tripr", mustWork = TRUE
      ),
      sep = ";", stringsAsFactors = FALSE,
      colClasses = c("character")
      ))
      
      
      
      colnames(Tgermlines) <- colnames(te)
      Tgermlines <- rbind(Tgermlines, te)
    } else {
      germline_file <- paste0("Germline_sequences_alignments_", type, "V_", AAorNtAlignment, ".csv")
      Tgermlines <- as.data.frame(fread(system.file("extdata/param", germline_file,
                                                    package = "tripr", mustWork = TRUE
      ),
      sep = ";", stringsAsFactors = FALSE,
      colClasses = c("character")
      ))
    }
    
    Tgermlines <- unique(Tgermlines)
    colnames(Tgermlines) <- c("V1", seq_len((ncol(Tgermlines) - 1)))
    
    a2 <- strsplit(Tgermlines$V1, " or|,| [(]see| OR")
    Tgermlines$V1 <- as.character(plyr::ldply(a2, function(s) {
      t(data.frame(unlist(s)))
    })[, 1])
    
    if (max_length_region > (ncol(Tgermlines) - 1)) {
      extra_dots <- matrix(".", nrow(Tgermlines), max_length_region - ncol(Tgermlines))
      Tgermlines <- cbind(Tgermlines, extra_dots)
      colnames(Tgermlines) <- c("V1", seq_len((max_length_region - 1)))
      Tgermlines[Tgermlines == ""] <- "."
      Tgermlines[is.na(Tgermlines)] <- "."
    }
  }
  
  if (region %in% colnames(input)) {
    
    ############### Clonotypes ##############
    cluster_id <- c()
    freq_cluster_id <- c()
    if (length(view_specific_clonotype_allData) == 0) {
      cluster_id[seq_len(nrow(input))] <- 0
      freq_cluster_id[seq_len(nrow(input))] <- 0
    } else {
      if (!highly) {
        pattern <- clono_allData$clonotype
        view_specific_clonotype_allData <- view_specific_clonotype_allData[pattern]
        for (i in seq_len(length(view_specific_clonotype_allData))) {
          index <- which(input[[used_columns[["Summary"]][1]]] %in% view_specific_clonotype_allData[[names(view_specific_clonotype_allData)[i]]][[used_columns[["Summary"]][1]]])
          if (index[1] > 0) {
            freq_cluster_id[index] <- clono_allData$Freq[i]
            cluster_id[index] <- i
          }
        }
      } else {
        cluster_id_vector <- lapply(view_specific_clonotype_allData, function(x){
          x <- levels(as.factor(x$cluster_id))
          return(x)
        })
        cluster_id_vector <- unlist(cluster_id_vector)
        names(view_specific_clonotype_allData) <- cluster_id_vector
        prev_clono <- c()
        for (i in seq_len(nrow(clono_allData))) {
          prev_clono[[i]] <- as.numeric(strsplit(as.character(clono_allData$prev_cluster[i]), " ")[[1]][2:length(strsplit(as.character(clono_allData$prev_cluster[i]), " ")[[1]])])
        }
        names(prev_clono) <- clono_allData$clonotype
        prev_clono <- data.frame(ID = rep(names(prev_clono), sapply(prev_clono, length)),
                                 Obs = unlist(prev_clono))
        prev_clono <- na.omit(prev_clono)
        prev_clono <- prev_clono[match(names(view_specific_clonotype_allData), prev_clono$Obs),]
        names(view_specific_clonotype_allData) <- prev_clono$ID
        view_specific_clonotype_allData <- names(view_specific_clonotype_allData) %>% 
          unique %>% 
          purrr::map(~ view_specific_clonotype_allData[names(view_specific_clonotype_allData)==.x] %>% 
                       bind_rows(.id = "id"))
        clono_allData_clonotypes <- unlist(lapply(view_specific_clonotype_allData, function(x) levels(as.factor(x$id))))
        names(view_specific_clonotype_allData) <- clono_allData_clonotypes
        pattern <- clono_allData$clonotype
        view_specific_clonotype_allData <- view_specific_clonotype_allData[pattern]
        for (i in seq_len(length(view_specific_clonotype_allData))) {
          index <- which(input[[used_columns[["Summary"]][1]]] %in% view_specific_clonotype_allData[[names(view_specific_clonotype_allData)[i]]][[used_columns[["Summary"]][1]]])
          if (index[1] > 0) {
            freq_cluster_id[index] <- clono_allData$Freq[i]
            cluster_id[index] <- i
          }
        }
      }
    }
    
    #########################################
    region_split <- strsplit(input[[region]], "")
    
    
    for (i in seq_len(length(region_split))) {
      if (length(region_split[[i]]) > max_length_region) {
        region_split[[i]] <- region_split[[i]][seq_len(max_length_region)]
      }
      if (length(region_split[[i]]) < max_length_region) {
        region_split[[i]][length(region_split[[i]]):max_length_region] <- "."
      }
    }
    
    region_split <- as.data.frame(region_split)
    region_split <- t(region_split)
    row.names(region_split) <- NULL
    
    region_alignment <- cbind(as.data.frame(cluster_id),
                              as.data.frame(freq_cluster_id),
                              Functionality = "productive"
    )
    
    
    region_alignment <- cbind(region_alignment,
                              J.GENE.and.allele = input[[used_columns[["Summary"]][8]]],
                              D.GENE.and.allele = input[[used_columns[["Summary"]][11]]],
                              V.GENE.and.allele = input[[used_columns[["Summary"]][3]]],
                              region_split, stringsAsFactors = FALSE
    )
    
    region_alignment$cluster_id <- as.character(cluster_id)
    region_alignment$freq_cluster_id <- as.character(freq_cluster_id)
    
    if (FtopN) {
      region_alignment <- region_alignment %>% dplyr::filter(as.numeric(as.character(region_alignment$cluster_id)) <= topNClono | region_alignment$cluster_id == "-")
    }
    
    if (Fthr) {
      region_alignment <- region_alignment %>% dplyr::filter(as.numeric(as.character(freq_cluster_id)) >= thrClono | region_alignment$cluster_id == "-")
    }
    
    if (only_one_germline) {
      germline <- strsplit(germline, "")[[1]]
      germline <- data.frame(t(germline), stringsAsFactors = FALSE)
      germline <- c("-", "-", "germline", "-", "-", "-", germline)
      germline <- as.data.frame(germline, stringsAsFactors = FALSE)
      colnames(germline) <- colnames(region_alignment[, seq_len(ncol(germline)), with=FALSE])
      alignment_with_germline <- rbind(germline, region_alignment[, seq_len(ncol(germline)), with=FALSE])
      
      a <- t(apply(alignment_with_germline[2:nrow(alignment_with_germline), 3:length(alignment_with_germline)], 1, function(x) {
        x == alignment_with_germline[1, 3:length(alignment_with_germline)] & x != "."
      })) # x: a row of input[count,XColumns]
      temp <- replace(alignment_with_germline[2:nrow(alignment_with_germline), 3:length(alignment_with_germline)], a == TRUE, "-")
      # add the first and the second columns
      temp2 <- cbind(alignment_with_germline[2:nrow(alignment_with_germline), seq_len(2), with=FALSE], temp)
      # add the last columns
      if ((length(alignment_with_germline) + 1) < length(region_alignment)) {
        temp2 <- rbind(temp2, region_alignment[, (length(alignment_with_germline) + 1):length(region_alignment)])
      }
      # add the germline (first row)
      germline_new <- germline
      colnames(germline_new) <- colnames(temp2)
      output <- rbind(germline_new[1, ], temp2)
    } else {
      if (use_genes_germline) {
        Tgermlines <- Tgermlines %>% dplyr::filter(stringr::str_detect(Tgermlines$V1, "[*]01 F"))
        for (i in seq_len(nrow(Tgermlines))) {
          Tgermlines$V1[i] <- strsplit(Tgermlines$V1, "[*]")[[i]][1]
        }
        
        region_alignment <- region_alignment
        
        for (i in seq_len(nrow(region_alignment))) {
          region_alignment$V.GENE.and.allele[i] <- strsplit(region_alignment$V.GENE.and.allele[i], "[*]")[[1]][1]
        }
      }
      
      if ((ncol(region_alignment) - ncol(Tgermlines) - 5) > 0) {
        a <- matrix(".", ncol = ncol(region_alignment) - ncol(Tgermlines) - 5, nrow = nrow(Tgermlines))
        germlines <- cbind("-", 0, "germline", "-", "-", Tgermlines, a)
        colnames(germlines) <- colnames(region_alignment)
        alignment_with_germline <- rbind(germlines, region_alignment)
      } else {
        germlines <- cbind("-", 0, "germline", "-", "-", Tgermlines)
        germlines <- germlines[, seq_len(ncol(region_alignment)), with=FALSE]
        colnames(germlines) <- colnames(region_alignment)
        
        alignment_with_germline <- rbind(germlines, region_alignment)
      }
      
      df3 <- alignment_with_germline
      
      germline <- c()
      output <- c()
      a <- c()
      XColumns <- seq_len((ncol(region_alignment) - 6))
      XColumns <- as.character(XColumns)
      
      alignment_with_germline <- data.table::as.data.table(alignment_with_germline)
      
      for (germ in unique(alignment_with_germline$V.GENE.and.allele)) {
        y <- alignment_with_germline[which(alignment_with_germline$V.GENE.and.allele == germ), ]
        
        germline <- which(y[["Functionality"]] == "germline")
        
        productive <- which(y[["Functionality"]] == "productive")
        
        
        
        if (length(germline) > 0 && length(productive) > 0) {
          germline <- germline[1]
          
          t <- as.matrix(y[germline, ..XColumns])
          t <- rep(t, length(productive))
          t <- matrix(data = t, nrow = length(productive), byrow = TRUE)
          
          a <- y[productive, ..XColumns] == t & y[productive, ..XColumns] != "."
          
          temp <- replace(y[productive, ..XColumns], a == TRUE, "-")
          
          temp.names <- colnames(alignment_with_germline[, seq_len(6), with=FALSE])
          
          temp2 <- cbind(y[productive, ..temp.names], temp)
          
          temp.names <- c(temp.names, XColumns)
          
          output <- rbind(output, y[germline, ..temp.names], temp2)
        }
      }
    }
    
    alignment_allData <- output %>% select(-c(Functionality))
    
    ################################ for Separate datasets ###############################
    alignment_datasets <- list()
    
    one_run <- function(j) {
      input_tmp <- input %>% dplyr::filter(input$dataName == name[j])
      ############### Clonotypes ##############
      cluster_id <- c()
      freq_cluster_id <- c()
      
      if (length(view_specific_clonotype_allData) == 0) {
        cluster_id[seq_len(nrow(input_tmp))] <- 0
        freq_cluster_id[seq_len(nrow(input))] <- 0
      } else {
        if (!highly) {
          patterns <- list()
          for (i in seq_len(length(clono_datasets))) {
            patterns[[i]] <- clono_datasets[[i]]$clonotype
          }
          names(patterns) <- name
          view_specific_clonotype_datasets[[name[j]]] <- view_specific_clonotype_datasets[[name[j]]][patterns[[name[j]]]]
          for (i in seq_len(length(view_specific_clonotype_datasets[[name[j]]]))) {
            index <- which(input_tmp[[used_columns[["Summary"]][1]]] %in% view_specific_clonotype_datasets[[name[j]]][[names(view_specific_clonotype_datasets[[name[j]]])[i]]][[used_columns[["Summary"]][1]]])
            if (index[1] > 0) {
              cluster_id[index] <- i
              freq_cluster_id[index] <- clono_datasets[[name[j]]]$Freq[i]
            }
          }
        } else {
          cluster_id_vector <- list()
          for (i in seq_len(length(view_specific_clonotype_datasets))) {
            cluster_id_vector[[i]] <- lapply(view_specific_clonotype_datasets[[i]], function(x){
              x <- levels(as.factor(x$cluster_id))
              return(x)
            })
          }
          for (i in seq_len(length(cluster_id_vector))) {
            cluster_id_vector[[i]] <- unlist(cluster_id_vector[[i]])
          }
          for (i in seq_len(length(view_specific_clonotype_datasets))) {
            names(view_specific_clonotype_datasets[[i]]) <- cluster_id_vector[[i]]
          }
          prev_clono_function <- function(clono_df) {
            prev_clono <- list()
            for (i in seq_len(nrow(clono_df))) {
              x <- as.numeric(strsplit(as.character(clono_df$prev_cluster[i]), " ")[[1]][2:length(strsplit(as.character(clono_df$prev_cluster[i]), " ")[[1]])])
              prev_clono[[i]] <- x
            }
            return(prev_clono)
          }
          prev_clono <- list()
          prev_clono <- lapply(clono_datasets , prev_clono_function)
          names(prev_clono[[name[j]]]) <- clono_datasets[[name[j]]]$clonotype
          prev_clono[[name[j]]] <- data.frame(ID = rep(names(prev_clono[[name[j]]]), sapply(prev_clono[[name[j]]], length)),
                                              Obs = unlist(prev_clono[[name[j]]]))
          prev_clono[[name[j]]] <- na.omit(prev_clono[[name[j]]])
          prev_clono[[name[j]]] <- prev_clono[[name[j]]][match(names(view_specific_clonotype_datasets[[name[j]]]), prev_clono[[name[j]]]$Obs),]
          names(view_specific_clonotype_datasets[[name[j]]]) <- prev_clono[[name[j]]]$ID
          setDF_function <- function(x_list) {
            a <- list()
            for (i in seq_len(length(x_list))) {
              x <- setDF(x_list[[i]])
              a[[i]] <- x
              names(a[[i]]) <- names(x_list[[i]])
            }
            return(a)
          }
          view <- list()
          view <- lapply(view_specific_clonotype_datasets, setDF_function)
          names(view[[name[j]]]) <- names(view_specific_clonotype_datasets[[name[j]]])
          view[[name[j]]] <- names(view[[name[j]]]) %>% 
            unique %>% 
            purrr::map(~ view[[name[j]]][names(view[[name[j]]])==.x] %>% 
                         bind_rows(.id = "id"))
          clono_datasets_clonotypes <- list()
          clono_datasets_clonotypes[[name[j]]] <- unlist(lapply(view[[name[j]]], function(x) levels(as.factor(x$id))))
          names(view[[name[j]]]) <- clono_datasets_clonotypes[[name[j]]]
          patterns <- list()
          for (i in seq_len(length(clono_datasets))) {
            patterns[[i]] <- clono_datasets[[i]]$clonotype
          }
          names(patterns) <- name
          view[[name[j]]] <- view[[name[j]]][patterns[[name[j]]]]
          for (i in seq_len(length(view[[name[j]]]))) {
            index <- which(input_tmp[[used_columns[["Summary"]][1]]] %in% view[[name[j]]][[names(view[[name[j]]])[i]]][[used_columns[["Summary"]][1]]])
            if (index[1] > 0) {
              cluster_id[index] <- i
              freq_cluster_id[index] <- clono_datasets[[name[j]]]$Freq[i]
            }
          }
        }
      }
      
      #########################################
      region_split <- strsplit(input_tmp[[region]], "")
      for (i in seq_len(length(region_split))) {
        if (length(region_split[[i]]) > max_length_region) {
          region_split[[i]] <- region_split[[i]][seq_len(max_length_region)]
        }
        
        if (length(region_split[[i]]) < max_length_region) {
          region_split[[i]][length(region_split[[i]]):max_length_region] <- "."
        }
      }
      
      region_split <- as.data.frame(region_split)
      region_split <- t(region_split)
      row.names(region_split) <- NULL
      
      region_alignment <- cbind(
        as.data.frame(cluster_id),
        as.data.frame(freq_cluster_id),
        Functionality = "productive"
      )
      
      region_alignment <- cbind(region_alignment,
                                J.GENE.and.allele = input_tmp[[used_columns[["Summary"]][8]]],
                                D.GENE.and.allele = input_tmp[[used_columns[["Summary"]][11]]],
                                V.GENE.and.allele = input_tmp[[used_columns[["Summary"]][3]]],
                                region_split, stringsAsFactors = FALSE
      )
      
      region_alignment$cluster_id <- as.character(cluster_id)
      region_alignment$freq_cluster_id <- as.character(freq_cluster_id)
      
      if (FtopN) {
        region_alignment <- region_alignment %>%
          dplyr::filter(as.numeric(as.character(region_alignment$cluster_id)) <= topNClono | region_alignment$cluster_id == "-")
      }
      
      if (Fthr) {
        region_alignment <- region_alignment %>%
          dplyr::filter(as.numeric(as.character(freq_cluster_id)) >= thrClono | region_alignment$cluster_id == "-")
      }
      
      if (only_one_germline) {
        alignment_with_germline <- rbind(germline, region_alignment[, seq_len(length(germline)), with=FALSE])
        
        a <- t(apply(alignment_with_germline[2:nrow(alignment_with_germline), 3:length(alignment_with_germline)], 1, function(x) {
          x == alignment_with_germline[1, 3:length(alignment_with_germline)] & x != "."
        })) # x: a row of input[count,XColumns]
        temp <- replace(alignment_with_germline[2:nrow(alignment_with_germline), 3:length(alignment_with_germline)], a == TRUE, "-")
        # add the first and the second columns
        temp2 <- cbind(alignment_with_germline[2:nrow(alignment_with_germline), seq_len(2), with=FALSE], temp)
        # add the last columns
        if ((length(alignment_with_germline) + 1) < length(region_alignment)) {
          temp2 <- rbind(temp2, region_alignment[, (length(alignment_with_germline) + 1):length(region_alignment)])
        }
        # add the germline (first row)
        germline_new <- germline
        colnames(germline_new) <- colnames(temp2)
        output <- rbind(germline_new[1, ], temp2)
      } else {
        if (use_genes_germline) {
          for (i in seq_len(nrow(region_alignment))) {
            region_alignment$V.GENE.and.allele[i] <- strsplit(region_alignment$V.GENE.and.allele[i], "[*]")[[1]][1]
          }
        }
        
        a <- matrix(".", ncol = ncol(region_alignment) - ncol(Tgermlines) - 5, nrow = nrow(Tgermlines))
        germlines <- cbind("-", 0, "germline", "-", "-", Tgermlines, a)
        colnames(germlines) <- colnames(region_alignment)
        
        alignment_with_germline <- rbind(germlines, region_alignment)
        
        germline <- c()
        output <- c()
        a <- c()
        XColumns <- seq_len((ncol(region_alignment) - 6))
        XColumns <- as.character(XColumns)
        
        alignment_with_germline <- data.table::as.data.table(alignment_with_germline)
        
        for (germ in unique(alignment_with_germline$V.GENE.and.allele)) {
          y <- alignment_with_germline[which(alignment_with_germline$V.GENE.and.allele == germ), ]
          
          germline <- which(y[["Functionality"]] == "germline")
          productive <- which(y[["Functionality"]] == "productive")
          
          if (length(germline) > 0 && length(productive) > 0) {
            germline <- germline[1]
            
            t <- as.matrix(y[germline, ..XColumns])
            t <- rep(t, length(productive))
            t <- matrix(data = t, nrow = length(productive), byrow = TRUE)
            
            a <- y[productive, ..XColumns] == t & y[productive, ..XColumns] != "."
            
            temp <- replace(y[productive, ..XColumns], a == TRUE, "-")
            
            temp.names <- colnames(alignment_with_germline[, seq_len(6), with=FALSE])
            
            temp2 <- cbind(y[productive, ..temp.names], temp)
            
            temp.names <- c(temp.names, XColumns)
            
            output <- rbind(output, y[germline, ..temp.names], temp2)
          }
        }
      }
      
      
      alignment_datasets[[name[j]]] <- output %>% select(-c(Functionality))
      
      if (save_tables_individually) {
        filename <- paste0(e$output_folder, "/", "Alignment_", AAorNtAlignment, "_", name[j], ".txt")
        write.table(alignment_datasets[[name[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
      }
      
      return(alignment_datasets[[name[j]]])
    }
    
    if (Sys.info()[1] == "Windows") {
      if (length(name) == 1) {
        alignment_datasets[[name]] <- alignment_allData
      } else {
        alignment_datasets <- lapply(seq_len(length(name)), one_run)
      }
    } else {
      if (length(name) == 1) {
        alignment_datasets[[name]] <- alignment_allData
      } else {
        alignment_datasets <- lapply(seq_len(length(name)), one_run)
        #alignment_datasets <- parallel::mclapply(seq_len(length(name)), one_run, mc.cores = num_of_cores, mc.preschedule = TRUE)
      }
    }
    
    names(alignment_datasets) <- name
    
    if (save_tables_individually) {
      filename <- paste0(e$output_folder, "/", "Alignment_", AAorNtAlignment, "_", "All_Data", ".txt")
      write.table(alignment_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
    }
    
    confirm <- "Alignment run!"
    
    result <- list(
      "alignment_allData" = alignment_allData,
      "alignment_datasets" = alignment_datasets,
      "confirm" = confirm
    )
    
    # log time end and memory used
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
    # cat(pryr::mem_used(), file = logFile, append = TRUE, sep = "\n")
    
    return(result)
  } else {
    return(0)
  }
}

######################################################################################################################################

mutations <- function(align, align_datasets, thr, AAorNtMutations, name, topNClono, FtopN, FclonoSeperately, cl, Fthr, thrClono, FthrSep = TRUE, thrSep) {
    # logfile
    # logFile<-e$logFile
    # cat(paste0("mutations", "\t"), file = logFile, append = TRUE)
    # cat(paste0(paste("region", AAorNtMutations, sep = ","), "\t"), file = logFile, append = TRUE)
    # cat(paste0(nrow(align), "\t"), file = logFile, append = TRUE)
    # cat(paste0(ncol(align), "\t"), file = logFile, append = TRUE)
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)

    if (FtopN) {
        align <- dplyr::filter(align, cluster_id %in% c("-", as.character(seq_len(topNClono))))
    }

    if (Fthr) {
        align <- align %>% dplyr::filter(align$freq_cluster_id >= thrClono | align$cluster_id == "-")
    }

    if (FclonoSeperately) {
        cl_id <- paste0("_cluster_id_", cl)
        if (FthrSep) {
            align <- dplyr::filter(align, cluster_id %in% c("-", as.character(cl)))
        } else {
            align <- dplyr::filter(align, cluster_id %in% c("-", as.character(cl)))
        }
    } else {
        cl_id <- ""
    }

    align[] <- lapply(align, factor) # the "[]" keeps the dataframe structure
    colnames(align)[4:ncol(align)] <- paste0("X", colnames(align)[4:ncol(align)])


    # Find IMGT-region from position
    region_names <- c("FR1-IMGT", "CDR1-IMGT", "FR2-IMGT", "CDR2-IMGT", "FR3-IMGT", "CDR3-IMGT")
    if (AAorNtMutations == "aa") {
        index_1 <- c(1, 27, 39, 56, 66, 105)
        index_2 <- c(26, 38, 55, 65, 104, 114)
        start_g <- 85
        IMGT_groups <- list(
            Acidic = c("E", "D"), Basic = c("K", "R", "H"), Aromatic = c("F", "W", "Y"),
            Non_polar_aliphatic = c("G", "A", "I", "V", "L", "P", "M"), Polar_non_charged = c("S", "T", "C", "N", "Q")
        )
    } else {
        index_1 <- c(1, 79, 115, 166, 196, 313)
        index_2 <- c(78, 114, 165, 195, 312, 342)
        start_g <- 260
        IMGT_groups <- list(transitions = c("a>g", "g>a", "c>t", "t>c"), transversions = c("a>c", "c>a", "g>t", "t>g", "c>g", "g>c", "a>t", "t>a"))
    }

    # for each position find the number of sequences that do not contain "-"
    count <- 0
    mutation_change_allData <- list()
    level_counts <- c()
    output <- c()
     
    b <- by(
        align, align$V.GENE.and.allele,
        function(y) {
            # germline <<- which(y$cluster_id == "-")
            germline <- which(y$cluster_id == "-")
            # germline <<- germline[1]
            germline <- germline[1]
            # productive <<- which(y$cluster_id != "-")
            productive <- which(y$cluster_id != "-")
            if (length(productive) > 0 & length(germline)) {
                mc <- c()
                germ_length <- ncol(y) - length(which(y[germline, start_g:ncol(align)] == "."))
                pos <- which((colSums(y[productive, 5:germ_length] != "-") / nrow(y)) > thr) # till germline length

                for (i in pos) {
                    level_counts <<- plyr::count(y[productive, ], paste0("X", i))
                    letter_ <- which(level_counts[, 1] == "-")
                    letter_dot <- which(level_counts[, 1] == ".")
                    if (length(letter_) > 0 & length(letter_dot) > 0) {
                        index <- which(as.numeric(row.names(level_counts)) != letter_ & as.numeric(row.names(level_counts)) != letter_dot)
                    } else if (length(letter_) > 0 & length(letter_dot) == 0) {
                        index <- which(as.numeric(row.names(level_counts)) != letter_)
                    } else if (length(letter_) == 0 & length(letter_dot) > 0) {
                        index <- which(as.numeric(row.names(level_counts)) != letter_dot)
                    } else if (length(letter_) == 0 & length(letter_dot) == 0) index <- as.numeric(row.names(level_counts))

                    if (length(index) > 0) {
                        max_freq_id <- which(level_counts[index, 2] == max(level_counts[index, 2]))
                        new_row <- cbind(level_counts[index, ][max_freq_id, ], pos = i, germline = y[[paste0("X", i)]][germline])
                        colnames(new_row)[1] <- "X"
                        if (new_row[, 2][1] / nrow(y) > thr) {
                            mc <- rbind(mc, new_row)
                        }
                    }
                }

                mc$region <- c()
                mc$germ_physico <- c()
                mc$new_physico <- c()
                mc$Change_in_physicochemical_properties <- c()
                if (length(mc) > 0) {
                    for (r in seq_len(length(region_names))) {
                        mc[which(mc$pos >= index_1[r] & mc$pos <= index_2[r]), 5] <- region_names[r]
                    }

                    if (AAorNtMutations == "aa") {
                        for (r in names(IMGT_groups)) {
                            mc[which(mc$germline %in% IMGT_groups[[r]]), 6] <- r
                            mc[which(mc$X %in% IMGT_groups[[r]]), 7] <- r
                        }
                        mc$Change_in_physicochemical_properties <- paste0(mc$V6, "->", mc$V7)
                    } else {
                        for (r in names(IMGT_groups)) {
                            mc[which(paste0(mc$germline, ">", mc$X) %in% IMGT_groups[[r]]), 6] <- r
                        }
                        mc$Change_in_physicochemical_properties <- mc$V6
                    }


                    mc <- cbind(Gene = as.character(y$V.GENE.and.allele[germline]), Change_in_position = paste0(mc$germline, mc$pos, ">", mc$X), Region = mc$V5, Change_in_physicochemical_properties = mc$Change_in_physicochemical_properties, N = mc$freq, Freq = 100 * mc$freq / (nrow(y) - 1))
                    output <<- rbind(output, mc)
                }
            }
        }
    )

    mutation_change_allData <- output

    ################################ for Separate datasets ###############################
    mutation_change_datasets <- list()

    one_run <- function(j) {
        if (FtopN) {
            align_datasets[[name[j]]] <- dplyr::filter(align_datasets[[name[j]]], cluster_id %in% c("-", as.character(seq_len(topNClono))))
        }

        if (FclonoSeperately) {
            align_datasets[[name[j]]] <- dplyr::filter(align_datasets[[name[j]]], cluster_id %in% c("-", as.character(cl)))
        }
        align_datasets[[name[j]]][] <- lapply(align_datasets[[name[j]]], factor)
        colnames(align_datasets[[name[j]]])[4:ncol(align_datasets[[name[j]]])] <- paste0("X", colnames(align_datasets[[name[j]]])[4:ncol(align_datasets[[name[j]]])])

        # for each position find the number of sequences that do not contain "-"
        count <- 0
        output <- c()
        mutation_change_temp <- list()
        
        b <- by(
            align_datasets[[name[j]]], align_datasets[[name[j]]]$V.GENE.and.allele,
            function(y) {
                # germline <<- which(y[, "cluster_id"] == "-")
                germline <- which(y[, "cluster_id"] == "-")
                # germline <<- germline[1]
                germline <- germline[1]
                # productive <<- which(y[, "cluster_id"] != "-")
                productive <- which(y[, "cluster_id"] != "-")
                if (length(productive) > 0 & length(germline)) {
                    mc <- c()
                    germ_length <- ncol(y) - length(which(y[germline, start_g:ncol(align_datasets[[name[j]]])] == "."))
                    pos <- which((colSums(y[productive, 5:germ_length] != "-") / nrow(y)) > thr) # till germline length

                    for (i in pos) {
                        level_counts <- plyr::count(y[productive, ], paste0("X", i))
                        letter_ <- which(level_counts[, 1] == "-")
                        letter_dot <- which(level_counts[, 1] == ".")
                        if (length(letter_) > 0 & length(letter_dot) > 0) {
                            index <- which(as.numeric(row.names(level_counts)) != letter_ & as.numeric(row.names(level_counts)) != letter_dot)
                        } else if (length(letter_) > 0 & length(letter_dot) == 0) {
                            index <- which(as.numeric(row.names(level_counts)) != letter_)
                        } else if (length(letter_) == 0 & length(letter_dot) > 0) {
                            index <- which(as.numeric(row.names(level_counts)) != letter_dot)
                        } else if (length(letter_) == 0 & length(letter_dot) == 0) index <- as.numeric(row.names(level_counts))

                        if (length(index) > 0) {
                            max_freq_id <- which(level_counts[index, 2] == max(level_counts[index, 2]))
                            new_row <- cbind(level_counts[index, ][max_freq_id, ], pos = i, germline = y[[paste0("X", i)]][germline])
                            colnames(new_row)[1] <- "X"
                            if (new_row[, 2][1] / nrow(y) > thr) {
                                mc <- rbind(mc, new_row)
                            }
                        }
                    }
                    mc$region <- c()
                    mc$germ_physico <- c()
                    mc$new_physico <- c()
                    mc$Change_in_physicochemical_properties <- c()
                    if (length(mc) > 0) {
                        for (r in seq_len(length(region_names))) {
                            mc[which(mc$pos >= index_1[r] & mc$pos <= index_2[r]), 5] <- region_names[r]
                        }

                        if (AAorNtMutations == "aa") {
                            for (r in names(IMGT_groups)) {
                                mc[which(mc$germline %in% IMGT_groups[[r]]), 6] <- r
                                mc[which(mc$X %in% IMGT_groups[[r]]), 7] <- r
                            }
                            mc$Change_in_physicochemical_properties <- paste0(mc$V6, "->", mc$V7)
                        } else {
                            for (r in names(IMGT_groups)) {
                                mc[which(paste0(mc$germline, ">", mc$X) %in% IMGT_groups[[r]]), 6] <- r
                            }
                            mc$Change_in_physicochemical_properties <- mc$V6
                        }

                        mc <- cbind(Gene = as.character(y$V.GENE.and.allele[germline]), Change_in_position = paste0(mc$germline, mc$pos, ">", mc$X), Region = mc$V5, Change_in_physicochemical_properties = mc$Change_in_physicochemical_properties, N = mc$freq, Freq = 100 * mc$freq / (nrow(y) - 1))
                        output <<- rbind(output, mc)
                    }
                }
            }
        )

        mutation_change_datasets[[name[j]]] <- output
        if (save_tables_individually) {
            filename <- paste0(e$output_folder, "/", "Mutations_thr", thr, "_", AAorNtMutations, "_", name[j], cl_id, ".txt")
            write.table(mutation_change_datasets[[name[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
        }

        return(mutation_change_datasets[[name[j]]])
    }

    if (Sys.info()[1] == "Windows") {
        mutation_change_datasets <- lapply(seq_len(length(name)), one_run)
    } else {
        mutation_change_datasets <- lapply(seq_len(length(name)), one_run)
        #mutation_change_datasets <- parallel::mclapply(seq_len(length(name)), one_run, mc.cores = num_of_cores, mc.preschedule = TRUE)
    }

    names(mutation_change_datasets) <- name

    if (save_tables_individually) {
        filename <- paste0(e$output_folder, "/", "Mutations_thr", thr, "_", AAorNtMutations, "_", "All_Data", cl_id, ".txt")
        write.table(mutation_change_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
    }

    confirm <- "Mutations run!"

    result <- list("mutation_change_allData" = mutation_change_allData, "mutation_change_datasets" = mutation_change_datasets, "confirm" = confirm)

    # log time end and memory used
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
    # cat(pryr::mem_used(), file = logFile, append = TRUE, sep = "\n")

    return(result)
}

######################################################################################################################################

# input: the alignment file
groupedAlignment <- function(alignment_allData, alignment_datasets, name, AAorNtAlignment) {
    # logfile
    # logFile<-e$logFile
    # cat(paste0("groupedAlignment", "\t"), file = logFile, append = TRUE)
    # cat(paste0("grouping", "\t"), file = logFile, append = TRUE)
    # cat(paste0(nrow(alignment_allData), "\t"), file = logFile, append = TRUE)
    # cat(paste0(ncol(alignment_allData), "\t"), file = logFile, append = TRUE)
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)

    input <- data.table(alignment_allData)

    XColumns <- seq_len((ncol(alignment_allData) - 6))
    XColumns <- as.character(XColumns)
    XColumns <- c("V.GENE.and.allele", "cluster_id", "freq_cluster_id", XColumns)

    distinct_changes <- input[, list(Freq = .N), by = XColumns]
    grouped_alignment_allData <- cbind(N = distinct_changes$Freq, distinct_changes[, seq_len((ncol(distinct_changes) - 1)), with=FALSE])

    grouped_alignment_datasets <- list()
    one_run <- function(j) {
        input <- data.table(alignment_datasets[[name[j]]])
        distinct_changes <- input[, list(Freq = .N), by = XColumns]
        grouped_alignment_datasets[[name[j]]] <- cbind(N = distinct_changes$Freq, distinct_changes[, seq_len((ncol(distinct_changes) - 1)), with=FALSE])
        if (save_tables_individually) {
            filename <- paste0(e$output_folder, "/", "Grouped Alignment_", AAorNtAlignment, "_", "All_Data", ".txt")
            write.table(grouped_alignment_datasets[[name[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
        }
        if (save_tables_individually) {
            filename <- paste0(e$output_folder, "/", "Grouped Alignment_", AAorNtAlignment, "_", name[j], ".txt")
            write.table(grouped_alignment_datasets[[name[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
        }
        return(grouped_alignment_datasets[[name[j]]])
    }

    if (Sys.info()[1] == "Windows") {
        grouped_alignment_datasets <- lapply(seq_len(length(name)), one_run)
    } else {
        grouped_alignment_datasets <- lapply(seq_len(length(name)), one_run)
        #grouped_alignment_datasets <- parallel::mclapply(seq_len(length(name)), one_run, mc.cores = num_of_cores, mc.preschedule = TRUE)
    }

    names(grouped_alignment_datasets) <- name

    if (save_tables_individually) {
        filename <- paste0(e$output_folder, "/", "Grouped Alignment_", AAorNtAlignment, "_", "All_Data", ".txt")
        write.table(grouped_alignment_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
    }

    confirm <- "Grouped Alignment run!"

    result <- list("grouped_alignment_allData" = grouped_alignment_allData, "grouped_alignment_datasets" = grouped_alignment_datasets, "confirm" = confirm)

    # log time end and memory used
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
    # cat(pryr::mem_used(), file = logFile, append = TRUE, sep = "\n")

    return(result)
}

######################################################################################################################################

find_cdr3_diff1P <- function(allData, max_length_cdr3, position, name) {
    # logfile
    # logFile<-e$logFile
    # cat(paste0("find_cdr3_diff1P", "\t"), file = logFile, append = TRUE)
    # cat(paste0("max length ", max_length_cdr3, ",", "Position ", position, "\t"), file = logFile, append = TRUE)
    # cat(paste0(nrow(allData), "\t"), file = logFile, append = TRUE)
    # cat(paste0(ncol(allData), "\t"), file = logFile, append = TRUE)
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
    
    ################## Clonotypes ################
    used_columns <- e$used_columns
    gene <- used_columns[["Summary"]][3]
    junction <- used_columns[["Summary"]][18]


    unique_genes <- unique(allData[[gene]])
    cdr3_diff1P_allData <- c()

    for (i in seq_len(length(unique_genes))) {
        gene_name <- unique_genes[i]
        #allData_temp <- allData %>% dplyr::filter(allData[[gene]] == gene_name)
        allData_temp <- allData[allData[[gene]] == gene_name]
        distinctVGenes_CDR3 <- allData_temp[, by = .(JUNCTION = allData_temp[[junction]]), .(Freq = .N)]
        # OLDdistinctVGenes_CDR3 <- allData_temp %>%
        #     dplyr::group_by(JUNCTION = allData_temp[[junction]]) %>%
        #     dplyr::summarise(Freq = n())
        distinctVGenes_CDR3 <- cbind(distinctVGenes_CDR3, cluster_id = row.names(distinctVGenes_CDR3))

        clono_datasets <- list()
        for (j in seq_len(length(name))) {
            data <- allData_temp[allData_temp$dataName == name[j]]
            #data <- allData_temp %>% dplyr::filter(allData_temp$dataName == name[j])
            clono_datasets[[name[j]]] <- data[, by = .(JUNCTION = data[[junction]]), .(Freq = .N)]
            # clono_datasets[[name[j]]] <- data %>%
            #     dplyr::group_by(JUNCTION = data[[junction]]) %>%
            #     dplyr::summarise(Freq = n())
            clono_datasets[[name[j]]] <- cbind(clono_datasets[[name[j]]], cluster_id = row.names(clono_datasets[[name[j]]]))
        }

        ################## Find the CDR3 that have difference only at the positions 8 and 9 with P ###############
        sub_groups <- c()
        sub_groups2 <- c()

        used_rows <- c()
        cdr3_split <- strsplit(distinctVGenes_CDR3$JUNCTION, "")
        cdr3_split_new <- c()
        k <- 0
        for (j in seq_len(length(cdr3_split))) {
            if (length(cdr3_split[[j]]) == max_length_cdr3) {
                k <- k + 1
                used_rows <- c(used_rows, j)
                cdr3_split_new[[k]] <- cdr3_split[[j]]
            }
            if (length(cdr3_split[[j]]) == (max_length_cdr3 - 1)) {
                # shift right from the position 8 till the end
                k <- k + 1
                used_rows <- c(used_rows, j)
                cdr3_split[[j]][(position + 1):max_length_cdr3] <- cdr3_split[[j]][position:(max_length_cdr3 - 1)]
                cdr3_split_new[[k]] <- cdr3_split[[j]]
                
            }


            #distinctVGenes_CDR3 <- distinctVGenes_CDR3[c(used_rows), ] #aspa
        }
        
        distinctVGenes_CDR3 <- distinctVGenes_CDR3[c(used_rows), ]
        cdr3_split_new <- as.data.frame(cdr3_split_new)
        cdr3_split_new <- as.data.frame(t(cdr3_split_new), stringsAsFactors = FALSE)
        row.names(cdr3_split_new) <- NULL

        if (nrow(cdr3_split_new) > 0) {
            # Find distinct cdr3_split_new
            if (nrow(distinct(cdr3_split_new)) < nrow(cdr3_split_new)) { 
                cdr3_split_new <- setDT(cdr3_split_new)
                VColumns <- colnames(cdr3_split_new)
                temp <- cdr3_split_new[, list(Freq_sub_cluster = .N), by = VColumns]

                v_cdr3_cluster_id <- c()
                for (j in seq_len(nrow(distinctVGenes_CDR3))) {
                    v_cdr3_cluster_id[j] <- which((do.call(paste0, cdr3_split_new[j, ]) == do.call(paste0, temp[, seq_len(max_length_cdr3), with=FALSE])))
                }

                multiple_objects_id <- c()
                for (j in seq_len(length(v_cdr3_cluster_id))) {
                    if (length(which(v_cdr3_cluster_id == v_cdr3_cluster_id[j])) > 1) multiple_objects_id <- c(multiple_objects_id, j)
                }


                # Combine to one data frame
                cdr3_split_cluster_id <- cbind.data.frame(cdr3_split_new, v_cdr3_cluster_id, stringsAsFactors = FALSE)
                result <- cbind(cluster_id = distinctVGenes_CDR3$cluster_id[multiple_objects_id], JUNCTION = distinctVGenes_CDR3$JUNCTION[multiple_objects_id], Freq = distinctVGenes_CDR3$Freq[multiple_objects_id])
                result <- as.data.frame(result, stringsAsFactors = FALSE)
                a <- distinctVGenes_CDR3[multiple_objects_id, ]
                a <- cbind(gene_name, a)
                result <- a[order(a[, "JUNCTION"]), ]

                cdr3_diff1P_allData <- rbind(cdr3_diff1P_allData, result)
            }
        }
    }

    ################################### Separate Datasets ######################################
    cdr3_diff1P_datasets <- list()
    for (n in seq_len(length(name))) {
        data_dataset <- allData[allData$dataName == name[n]]
        #data_dataset <- allData %>% dplyr::filter(allData$dataName == name[n])
        unique_genes <- unique(data_dataset[[gene]])
        cdr3_diff1P_datasets[[name[n]]] <- c()

        for (i in seq_len(length(unique_genes))) {
            gene_name <- unique_genes[i]
            allData_temp <- data_dataset[data_dataset[[gene]] == gene_name]
            #allData_temp <- data_dataset %>% dplyr::filter(data_dataset[[gene]] == gene_name)
            distinctVGenes_CDR3 <- allData_temp[, by = .(JUNCTION = allData_temp[[junction]]),.(Freq = .N)]
            # distinctVGenes_CDR3 <- allData_temp %>%
            #     dplyr::group_by(JUNCTION = allData_temp[[junction]]) %>%
            #     dplyr::summarise(Freq = n())
            distinctVGenes_CDR3 <- cbind(distinctVGenes_CDR3, cluster_id = row.names(distinctVGenes_CDR3))

            clono_datasets <- list()
            for (j in seq_len(length(name))) {
                data <- allData_temp[allData_temp$dataName == name[j]]
                #data <- allData_temp %>% dplyr::filter(allData_temp$dataName == name[j])
                clono_datasets[[name[j]]] <- data[, by = .(JUNCTION = data[[junction]]), .(Freq = .N)]
                # clono_datasets[[name[j]]] <- data %>%
                #     dplyr::group_by(JUNCTION = data[[junction]]) %>%
                #     dplyr::summarise(Freq = n())
                clono_datasets[[name[j]]] <- cbind(clono_datasets[[name[j]]], cluster_id = row.names(clono_datasets[[name[j]]]))
            }

            ################## Find the CDR3 that have difference only at the positions 8 and 9 with P ###############
            sub_groups <- c()
            sub_groups2 <- c()

            used_rows <- c()
            cdr3_split <- strsplit(distinctVGenes_CDR3$JUNCTION, "")
            cdr3_split_new <- c()
            k <- 0
            for (j in seq_len(length(cdr3_split))) {
                if (length(cdr3_split[[j]]) == max_length_cdr3) {
                    k <- k + 1
                    used_rows <- c(used_rows, j)
                    cdr3_split_new[[k]] <- cdr3_split[[j]]
                }
                if (length(cdr3_split[[j]]) == (max_length_cdr3 - 1)) {
                    # shift right from the position 8 till the end
                    k <- k + 1
                    cdr3_split[[j]][(position + 1):max_length_cdr3] <- cdr3_split[[j]][position:(max_length_cdr3 - 1)]
                    cdr3_split_new[[k]] <- cdr3_split[[j]]
                    used_rows <- c(used_rows, j)
                }


                # distinctVGenes_CDR3 <- distinctVGenes_CDR3[used_rows, ]
            }
            
            distinctVGenes_CDR3 <- distinctVGenes_CDR3[c(used_rows), ]
            cdr3_split_new <- data.frame(cdr3_split_new)

            cdr3_split_new <- as.data.frame(t(cdr3_split_new), stringsAsFactors = FALSE)
            row.names(cdr3_split_new) <- NULL

            if (nrow(cdr3_split_new) > 0) {
                # Find distinct cdr3_split_new
                if (nrow(distinct(cdr3_split_new)) < nrow(cdr3_split_new)) { 
                    cdr3_split_new <- setDT(cdr3_split_new)
                    VColumns <- colnames(cdr3_split_new)
                    temp <- cdr3_split_new[, list(Freq_sub_cluster = .N), by = VColumns]

                    v_cdr3_cluster_id <- c()
                    for (j in seq_len(nrow(distinctVGenes_CDR3))) {
                        v_cdr3_cluster_id[j] <- which((do.call(paste0, cdr3_split_new[j, ]) == do.call(paste0, temp[, seq_len(max_length_cdr3), with=FALSE])))
                    }

                    multiple_objects_id <- c()
                    for (j in seq_len(length(v_cdr3_cluster_id))) {
                        if (length(which(v_cdr3_cluster_id == v_cdr3_cluster_id[j])) > 1) multiple_objects_id <- c(multiple_objects_id, j)
                    }


                    # Combine to one data frame
                    cdr3_split_cluster_id <- cbind.data.frame(cdr3_split_new, v_cdr3_cluster_id, stringsAsFactors = FALSE)
                    result <- cbind(cluster_id = distinctVGenes_CDR3$cluster_id[multiple_objects_id], JUNCTION = distinctVGenes_CDR3$JUNCTION[multiple_objects_id], Freq = distinctVGenes_CDR3$Freq[multiple_objects_id])
                    result <- as.data.frame(result, stringsAsFactors = FALSE)
                    a <- distinctVGenes_CDR3[multiple_objects_id, ]
                    a <- cbind(gene_name, a)
                    result <- a[order(a[, "JUNCTION"]), ]

                    cdr3_diff1P_datasets[[name[n]]] <- rbind(cdr3_diff1P_datasets[[name[n]]], result)
                }
            }
        }
    }

    confirm <- "CDR3 1 diff length run!"

    result <- list("cdr3_diff1P_allData" = cdr3_diff1P_allData, "cdr3_diff1P_datasets" = cdr3_diff1P_datasets, "confirm" = confirm)

    # log time end and memory used
    # cat(paste0(Sys.time(), "\t"), file = logFile, append = TRUE)
    # cat(pryr::mem_used(), file = logFile, append = TRUE, sep = "\n")

    return(result)
}

######################################################################################################################################

addRepertoryFct <- function(id, btn) {
    insertUI(
        selector = "#placeholderRepertories",
        ui = tags$div(
            selectInput(btn, "Select type:", c(
                "V Gene", "V Gene and allele",
                "J Gene", "J Gene and allele",
                "D Gene", "D Gene and allele"
            ), width = "170px"),
            id = id
        )
    )
}

######################################################################################################################################

addMultipleValues <- function(id, btn, columns_for_Multiple_value_comparison, default_val1 = NULL, default_val2 = NULL) {
    insertUI(
        selector = "#placeholder",
        ## wrap element in a div with id for ease of removal
        ui = tags$div(
            # forloop for columns
            div(style = "display:inline-block", selectInput(paste0("select_MultipleValues_column1_", btn), "Select 1st column:", columns_for_Multiple_value_comparison, selected = default_val1, width = "170px")),
            div(style = "display:inline-block", selectInput(paste0("select_MultipleValues_column2_", btn), "Select 2nd column:", columns_for_Multiple_value_comparison, selected = default_val2, width = "170px")),
            id = id
        )
    )
}

######################################################################################################################################
# Set up a button to have an animated loading indicator and a checkmark
# for better user experience
# Need to use with the corresponding `withBusyIndicator` server function
withBusyIndicatorUI <- function(button) {
    id <- button[["attribs"]][["id"]]
    div(
        `data-for-btn` = id,
        button,
        span(
            class = "btn-loading-container",
            hidden(
                img(src = "www/ajax-loader-bar.gif", class = "btn-loading-indicator"),
                icon("check", class = "btn-done-indicator")
            )
        ),
        hidden(div(
            class = "btn-err",
            div(
                icon("exclamation-circle"),
                tags$b("Error: "),
                span(class = "btn-err-msg")
            )
        ))
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

    tryCatch(
        {
            value <- expr
            shinyjs::show(selector = doneEl)
            shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade", time = 0.5))
            value
        },
        error = function(err) {
            errorFunc(err, buttonId)
        }
    )
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
