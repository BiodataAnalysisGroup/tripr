test_that("dataframe after filter is not empty", {
    
    ## All the process before imgtfilter() from run_TRIP_without_ui.R
    ## Needed args: name, files, datapath, throughput, preselection
    ##     selection.
    datapath <- fs::path_package("extdata", "dataset", package="tripr")
    filelist <- c("1_Summary.txt", "2_IMGT-gapped-nt-sequences.txt", 
        "4_IMGT-gapped-AA-sequences.txt","6_Junction.txt")
    cell = "Tcell"
    throughput = "High Throughput"
    preselection = "1,2,3,4C:W"
    selection="5"
    identity_range = "85:100"

    name <- list.files(datapath) # dataset names eg c("B1","B2")
    allDatasets <- name
    loaded_datasets <- list.files(datapath) # dataset names eg c("B1","B2")
    # selected imgt files eg c("1_Summary.txt", 
    # "2_IMGT-gapped-nt-sequences.txt", "4_IMGT-gapped-AA-sequences.txt",
    # "6_Junction.txt" )
    files <- filelist 
    if (cell == "Bcell") {
        cell_id <- 2
        Tcell <- FALSE
    } else {
        cell_id <- 1
        Tcell <- TRUE
    }

    throughput <- "High Throughput"

    testColumnNamesOut <- testColumnNames(name, files, datapath)
    rawDataSet <- testColumnNamesOut$rawDataSet
    name <- names(rawDataSet)
    allDatasets <- name
    loaded_datasets <- name

    #### Preselection ####
    preselection <- strsplit(preselection, ",")[[1]]
    option4 <- which(startsWith(preselection, "4"))
    filterStart <- ""
    filterEnd <- ""

    ## Solves 'NAs introduced by coercion' Warning
    if (length(option4) > 0) {
        ## filter_id == 4 present
        filter_id <- preselection[-option4]
        filter_id <- c(suppressWarnings(as.numeric(filter_id)), 4)
        temp <- strsplit(preselection[option4], 4)[[1]][2]
        filterStart <- paste0("^", strsplit(temp, ":")[[1]][1])
        filterEnd <- paste0(strsplit(temp, ":")[[1]][2], "$")
    } else {
        ## No filter_id == 4 present
        filter_id <- suppressWarnings(as.numeric(preselection))
    }

    if (throughput == "High Throughput") {
        imgtcleaning_results <- imgtcleaning(rawDataSet, name, allDatasets, 
            files, cell_id,
            filter_id, " P| ORF", "[*]|X|#|[.]", "productive",
            filterStart, filterEnd,
            identityLow = 95, identityHigh = 100, VGene = "",
            JGene = "", DGene = "", lengthLow = 7, lengthHigh = 15, 
            aminoacid = "CASSPPDTGELFF", seq1 = 1, seq2 = 2, Tcell
        )
    } else {
        imgtcleaning_results <- imgtcleaningLow(rawDataSet, name, 
            allDatasets, files, cell_id,
            filter_id, " P| ORF", "[*]|X|#|[.]", "productive",
            filterStart, filterEnd,
            identityLow = 95, identityHigh = 100, VGene = "",
            JGene = "", DGene = "", lengthLow = 7, lengthHigh = 15, 
            aminoacid = "CASSPPDTGELFF", seq1 = 1, seq2 = 2, Tcell
        )
    }

    ## SELECTION ########################

    filter_id <- as.numeric(strsplit(selection, ",")[[1]])
    identityLow <- 0
    identityHigh <- 100
    lengthLow <- 100
    lengthHigh <- 0
    if (5 %in% filter_id) {
        identityLow <- as.numeric(strsplit(identity_range, ":")[[1]][1])
        identityHigh <- as.numeric(strsplit(identity_range, ":")[[1]][2])
    }

    if (9 %in% filter_id) {
        lengthLow <- as.numeric(strsplit(cdr3_length_range, ":")[[1]][1])
        lengthHigh <- as.numeric(strsplit(cdr3_length_range, ":")[[1]][2])
    }

    if (throughput == "High Throughput") {
        imgtfilter_results <- imgtfilter(
            imgtcleaning_results$cleaned_datasets, loaded_datasets, 
            imgtcleaning_results$allData,
            cell_id, filter_id, " P| ORF", "[*]|X|#|[.]", "productive", 
            start_char,
            end_char, identityLow, identityHigh, vgenes, jgenes, dgenes, 
            lengthLow, lengthHigh, aminoacid, seq1, seq2
        )
    } else {
        imgtfilter_results <- imgtfilterLow(
            imgtcleaning_results$cleaned_datasets, loaded_datasets, 
            imgtcleaning_results$allData,
            cell_id, filter_id, " P| ORF", "[*]|X|#|[.]", "productive", 
            start_char,
            end_char, identityLow, identityHigh, vgenes, jgenes, dgenes, 
            lengthLow, lengthHigh, aminoacid, seq1, seq2
        )
    }


    ## data.frame shouldn't be empty
    expect_false(plyr::empty(imgtfilter_results$allData))
})
