#app <- shinyApp(ui = ui, server = server)
#runApp(app, host ="0.0.0.0", port = 8084, launch.browser = FALSE)
#shiny::runApp(port=3168)

#install.packages(c("shinyFiles", "shinyjs", "shinyBS", "DT","plyr","dplyr","pryr","data.table","stringr","tidyr","xtable","plot3D","gridExtra","stringdist","plotly"))
#source("http://bioconductor.org/biocLite.R")
#biocLite("Biostrings")
#source("https://bioconductor.org/biocLite.R") 
#biocLite("motifStack")

#library(motifStack)     
library(shinyFiles)
library("shinyBS")
library("DT")
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

#Sys.setenv(R_GSCMD=file.path("C:", "Program Files", "gs","gs9.21", "bin", "gswin32c.exe"))

source("helpers.R")
includeHTML("on_reload.html")

options(shiny.maxRequestSize=3000*1024^2)

on_click_js = "
Shiny.onInputChange('mydata', '%s');
$('#modalViewSpecificClonotype').modal('show')
"
on_click_js_convergent_evolution= "
Shiny.onInputChange('conv_evo', '%s');
$('#modalViewConvergentEvolution').modal('show')
"
##JS Code for enabling and diabling tabs
jscode <- "shinyjs.disabletab =function(name){
$('ul li:has(a[data-value= ' + name + '])').addClass('disabled');
$('.nav li.disabled a').prop('disabled',true)
}
 
shinyjs.enabletab =function(name){
$('.nav li.disabled a').prop('disabled',false)
$('ul li:has(a[data-value= ' + name + '])').removeClass('disabled');
} " 

shinyServer( 
  function(session, input, output) { 
    
    ######################################## initialize global variables  ########################################
    print("session started")
    loaded_datasets <- c()
    newDatasetNames <- NULL   
    file_size <- 0
    cleaning_criteria <- c("Functional V-Gene", "CDR3 with no Special Characters","Productive Sequence", "Productive Sequences")
    filtering_criteria <- c("V-REGION identity %","Specific V Gene","Specific J Gene", "Specific D Gene","CDR3 length range","CDR3 length range")
    
    filtering_workflow <- c()
    cleaning_workflow <- c()
    
    identity_groups <- c()
    specificClonotypes <- c()
    filterStartEnd <- NULL
    start_char <- NULL
    end_char <- NULL
    newDataset <- FALSE
    cleaning_confirm <- ""
    correctColumns <- "no"
    rawDataSet <- list()
    worng_columns_id <- list()
    new_columns <- list()
    gene_clonotypes <- ""
    junction_clonotypes <- ""
    allele_clonotypes <- ""
    
    cleaning_parameters <- c()
    filtering_parameters <- c()
    pipeline_parameters <- c()
    
    filteredData_id <- c()
    
    Multiple_value_comparison_input_values <- c()
    
    mutational_status_table_allData <- c() #delete from global 
    mutational_status_table_datasets <- list() #delete from global 
    
    cl_ids_mutations <- c()
    FclonoSeperately <- F
    
    cl_ids_logos <- c()
    FclonoLogoSeperately <- F
    
    countTables_per_region_datasets <- list()
    
    highly_sim_datasets <- list()  
    highly_sim <- c()
    
    #function results
    imgtfilter_results <- c()
    imgtcleaning_results <- c() #save only the workflow table into a different variable and delete it from global
    insertedMultiple_value_comparison  <-  c()
    insertedRepertoires <- c()
    clono <- c()
    public_clonotypes_results <- list() #delete from global 
    highly_sim_public_clonotypes_results <- list() #delete from global 
    repertories_results <- c()
    repertoires_comparison_results <- list() #delete from global 
    HighlySim_repertories_results <- list() 
    highly_sim_repertoires_comparison_results <- list() #delete from global 
    columns_for_Multiple_value_comparison <- c()
    Multiple_value_comparison_result <- list() #delete from global 
    alignmentRegion_results <- list() #delete from global 
    alignmentRegion_results_nt <- list() #delete from global 
    grouped_alignment_results <- list()
    grouped_alignment_results_nt <- list()
    mutation_results <- list() #delete from global 
    mutation_results_nt <- list() #delete from global 
    mutation_results_cl <- list() #delete from global 
    mutation_results_nt_cl <- list() #delete from global 
    frequenciesTables_results <- list() #delete from global 
    frequenciesTables_results_cl <- list() #delete from global 
    logo_result <- list() #delete from global 
    logo_per_region <- list() #delete from global 
    logo_result_cl <- list() #delete from global 
    logo_per_region_cl <- list() #delete from global 
    CDR3Diff1_results <- list()
    highly_similar_clonotypes_results <- list()
    
    just_restored_session <- F
    just_restored_session_cleaning <- F
    just_restored_session_clonotypes <- F
    just_restored_session_Repertoires <- F
    just_restored_session_HighlySim_Repertoires <- F
    just_restored_session_repertoires_comparison <- F
    just_restored_session_Multiple_value_comparison <- F
    just_restored_session_alignment <- F
    just_restored_session_freqTables <- F
    just_restored_session_logo <- F
    just_restored_session_CDR3Diff1 <- F
    just_restored_session_highly_similar_clonotypes <- F
    just_restored_session_public_clonotypes <- F
    just_restored_session_highly_sim_public_clonotypes <- F
    just_restored_session_mutations <- F
    
    msgLoadData <- ""
    msgCleaning <- ""
    msgFiltering <- ""
    msgClonotypes <- ""
    msgHighlySim <- ""
    msgPublicClono <- ""
    msgPublicClono <- ""
    msgRepertoires <- c()
    msgHighlySim_Repertoires <- c()
    #msgRepertoires[1] <- "" 
    msgRepertoiresComp <- ""
    msgMultiple_value_comparison <- c()
    msgAlignment <- ""
    msgGroupedAlignment <- ""
    msgFreqTables <- ""
    msgLogo <- ""
    msgCDR3Diff1 <- ""
    msgMutation <- ""
    
    logo_plot <- NULL
    freq_mat <- c()
    pie_repertory <- list()
    cd <- NULL
    
    region_names <- c("FR1-IMGT","CDR1-IMGT","FR2-IMGT","CDR2-IMGT","FR3-IMGT","CDR3-IMGT")
    index_1 <- c(1,27,39,56,66,105)
    index_2 <- c(26,38,55,65,104,114)
    
    cdr3_lengths <- c()
    
    #Distributions
    box_input <- c()  #delete from global 
    cdr3_length_distribution_dataset <- list()  #delete from global 
    cdr3_length_distribution <- c()  #delete from global 
    pi_distribution <- c()  #delete from global 
    pi_distribution_dataset <- list()   #delete from global 
    
    
    ############################################# Select Datasets ######################################### #############################################
    
    # dir
    shinyDirChoose(input, 'dir', roots = c(home = '.'), filetypes = c('', 'txt'))
    
    dir <- reactive(input$dir)
    output$dir <- renderPrint(dir())
    
    # path
    path <- reactive({
      home <- normalizePath(".")
      file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
    })
    
    # files
    output$files <- renderPrint(list.files(path()))
    
    #Load Data
    output$uiLoadData <- renderUI({
      if (is.null(input$inputFiles) | is.null(input$Dataset)) return()
      # Wrap the button in the function `withBusyIndicatorUI()`
      withBusyIndicatorUI(
      actionButton("LoadData", "Load Data", 
                   style="color: #fff; background-color: #5F021F; border-color: #fff")
      )
    })

    dataInputColumns <- reactive({
      if (is.null(input$inputFiles) | is.null(input$Dataset) | is.null(input$LoadData)) return()
      if (input$LoadData==FALSE){return()}
      withBusyIndicatorServer("LoadData", {
        #testColumnNames(list.files(path()), input$inputFiles, path()) #load all datasets that are located at the path
        testColumnNames(input$Dataset, input$inputFiles, path()) #load only selected datasets
      })
    })
    
    output$num_of_datasets<-reactive({
      if ((is.null(input$inputFiles) | is.null(input$Dataset) | is.null(input$LoadData)) & (input$select_load_or_compute_clonotypes!="compute_clonotypes")) return()
      if (input$select_load_or_compute_clonotypes=="compute_clonotypes"){
        if (is.null(input$inputFiles) | is.null(input$Dataset) | is.null(input$LoadData)) return()
        if (input$LoadData==FALSE){return()}
      }
      if (input$select_load_or_compute_clonotypes=="compute_clonotypes")
        length(unique(t(data.frame(strsplit(input$Dataset,"_")))[,1]))
      else
        length(loaded_datasets)
    })
    outputOptions(output, 'num_of_datasets', suspendWhenHidden = FALSE)
    
    output$confirmLoadData <- renderUI({
      if (is.null(dataInputColumns())) return()
      msgLoadData<<-dataInputColumns()$confirm
      h5(msgLoadData, style = "color: #CD0000;")
    })
    
    output$uiInputFiles <- renderUI({
      if (is.null(dir())){return()}
      a=list.files(paste0(path(),"/",list.files(path())[1]))
      wellPanel(
        tags$div(class = "multicol", checkboxGroupInput(inputId = "inputFiles", label = "Select Files", inline=FALSE, choices = a, selected=c("1_Summary.txt","2_IMGT-gapped-nt-sequences.txt","4_IMGT-gapped-AA-sequences.txt","6_Junction.txt")))
      )
    })
    
    output$uiDatasets <- renderUI({
      if (is.null(dir())) return()
      checkboxGroupInput(inputId = "Dataset", label = "Select Datasets", inline=TRUE, choices = list.files(path()))
    })
    
    observeEvent(input$LoadData,{
      if (is.null(input$LoadData)) return()
      if (input$LoadData==FALSE){return()}
      loaded_datasets<<-unique(t(data.frame(strsplit(input$Dataset,"_")))[,1])
    })
    
    observeEvent(input$Dataset,{
      if (is.null(input$Dataset)) return()
      file_size<<-0
      for (i in 1:length(input$Dataset)){
        for (j in 1:length(input$inputFiles)){
          file_size<<-file_size+file.size(paste0(path(),"/",input$Dataset[i],"/",input$inputFiles[j]))
        }
      }
      print(paste0(file_size/1000000,"MB to be loaded"))
      
    })
    
    observeEvent(input$select_load_or_compute_clonotypes,{
      if (input$select_load_or_compute_clonotypes == 'load_clonotypes'){
        #used columns
        load("rData files/used_columns.rData")
        used_columns<<-used_columns
        load("rData files/cdr3_lengths.rData")
        cdr3_lengths<<-cdr3_lengths
      }
    })
    
    
    ########################################### Wrong column names  ####################################### #############################################
    
    # Return the UI for a modal dialog with data selection input. If 'failed' is
    # TRUE, then display a message that the previous value was invalid.
    dataModal <- function(failed = FALSE) {
      data <- dataInputColumns()
      modalDialog(
        lapply(1:length(data$worng_columns_id), function(i) {
          fluidPage(
            #width = 9,
            h4(paste0(" Wrong column names for Dataset ",data$wrong_dataset[i]," : ", toString(data$worng_columns_names[[i]]),". Complete the new column names.")),
            #sidebarPanel(
            # width = 19,
            textInput(paste0("column_name",i), "Column names:"),
            helpText("Separate the different column names with comma e.g. V-GENE and allele,AA JUNCTION")
            #actionButton("Execute2", "Execute")  
            #)
          )
        }),
        if (failed)
          div(tags$b("Invalid name of data object", style = "color: red;")),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ok", "OK")
        )
      )
    }
    
    output$uiColumns <- renderUI({
      if (is.null(input$inputFiles) | is.null(dir()) | is.null(input$Dataset) |is.null(input$LoadData)) return()
      if (input$LoadData==F) return()
      data <- dataInputColumns()
      if (data$message!="wrong column names"){return()}
      if (length(new_columns)>0){if (correctColumns=="yes"){return()}}
      showModal(dataModal())
      
    })
    
    # When OK button is pressed, attempt to load the data set. If successful,
    # remove the modal. If not show another modal, but this time with a failure
    # message.
    observeEvent(input$ok, {
      data <- dataInputColumns()
      # Take the input values for the new column names.
      for (i in 1:length(data$worng_columns_id)){
        col<-input[[paste0("column_name",i)]]
        if (length(col)==0) return()
        if (col=="") return()
        new_columns_names=strsplit(as.character(col), ",")[[1]] 
        new_columns_files=strsplit(toString(data$worng_columns_names[[i]]),":")[[1]]
        
        new_columns_temp<-c()
        for (j in 1:length(new_columns_names)){
          b=strsplit(new_columns_files[j],"_")
          b2<-gsub(".txt","",b[[1]][2])
          b2<-gsub("-",".",b2)
          tmp=paste(b2, new_columns_names[j])
          tmp2=gsub(" ", ".",tmp)
          tmp2=gsub("-", ".",tmp2)
          new_columns_temp<-c(new_columns_temp,tmp2)
        }
        new_columns[[i]]<<-new_columns_temp
      }
      
      correctInputColumns <- reactive({
        #print(dataInputColumns())
        correctColumnNames(input$inputFiles,data$rawDataSet, list.files(path()), data$wrong_dataset, new_columns, data$worng_columns_id,loaded_datasets)
      })
      
      if (correctInputColumns()$correct=="yes"){
        correctColumns<<-"yes"
        rawDataSet<<-correctInputColumns()$rawDataSet
        
        removeModal()
      } else {
        correctColumns<<-"no"
        showModal(dataModal(failed = TRUE))
      }
    })
    
    
    ################################################# Load a Previous Session  ############################ #################################################
    
    vals <- reactiveValues(sum = 0)
    vals <- reactiveValues(excludedPoints=c(1,2,3))
    
    # makeReactiveBinding("excludedPoints")
    
    # Save extra values in state$values when we bookmark
    onBookmark(function(state) {
      #load saved objects
      #load("Multiple_value_comparison_result.rdata")
      
      state$values$dir <- dir
      state$values$path <-path()
      state$values$newDatasetNames <- newDatasetNames
      state$values$imgtfilter_results<-imgtfilter_results
      state$values$imgtcleaning_results<-imgtcleaning_results
      state$values$repertories_results<-repertories_results
      state$values$insertedRepertoires<-insertedRepertoires
      state$values$HighlySim_repertories_results<-HighlySim_repertories_results
      state$values$repertoires_comparison_results<-repertoires_comparison_results
      state$values$highly_sim_repertoires_comparison_results<-highly_sim_repertoires_comparison_results
      state$values$clono<-clono
      state$values$highly_similar_clonotypes_results<-highly_similar_clonotypes_results
      state$values$public_clonotypes_results<-public_clonotypes_results
      state$values$cdr3_lengths<-cdr3_lengths
      #state$values$Multiple_value_comparison_result<-Multiple_value_comparison_result
      state$values$insertedMultiple_value_comparison<-insertedMultiple_value_comparison
      state$values$Multiple_value_comparison_input_values<-Multiple_value_comparison_input_values
      state$values$columns_for_Multiple_value_comparison<-columns_for_Multiple_value_comparison
      state$values$frequenciesTables_results<-frequenciesTables_results
      state$values$logo_result<-logo_result
      state$values$alignmentRegion_results<-alignmentRegion_results
      state$values$alignmentRegion_results_nt<-alignmentRegion_results_nt
      state$values$mutation_results <-mutation_results
      state$values$mutation_results_nt <-mutation_results_nt
      state$values$mutation_results_cl <-mutation_results_cl
      state$values$mutation_results_nt_cl <-mutation_results_nt_cl
      state$values$grouped_alignment_results<-grouped_alignment_results
      state$values$grouped_alignment_results_nt<-grouped_alignment_results_nt
      state$values$dataInputColumns<-dataInputColumns()
      
      if (input$Continue!=F)
        state$values$newDataset<-F
      else
        state$values$newDataset<-newDataset

    })
    
    # Read values from state$values when we restore
    onRestore(function(state) {
      updateTabsetPanel(session, "navbar", "home")
      
      dir <<- state$values$dir

      newDatasetNames<<- state$values$newDatasetNames
      dataInputColumnsTemp<<-state$values$dataInputColumns
      imgtfilter_results<<-state$values$imgtfilter_results
      imgtcleaning_results<<-state$values$imgtcleaning_results
      newDataset<<-state$values$newDataset
      repertories_results<<-state$values$repertories_results
      insertedRepertoires<<-state$values$insertedRepertoires
      HighlySim_repertories_results<<-state$values$HighlySim_repertories_results
      repertoires_comparison_results<<-state$values$repertoires_comparison_results
      highly_sim_repertoires_comparison_results<<-state$values$highly_sim_repertoires_comparison_results
      
      clono<<-state$values$clono
      highly_similar_clonotypes_results<<-state$values$highly_similar_clonotypes_results
      public_clonotypes_results<<-state$values$public_clonotypes_results
      cdr3_lengths<<-state$values$cdr3_lengths
      insertedMultiple_value_comparison<<-state$values$insertedMultiple_value_comparison

      Multiple_value_comparison_input_values<<-state$values$Multiple_value_comparison_input_values
      columns_for_Multiple_value_comparison<<-state$values$columns_for_Multiple_value_comparison
      frequenciesTables_results<<-state$values$frequenciesTables_results
      logo_result<<-state$values$logo_result
      alignmentRegion_results<<-state$values$alignmentRegion_results
      alignmentRegion_results_nt<<-state$values$alignmentRegion_results_nt
      grouped_alignment_results<<-state$values$grouped_alignment_results
      grouped_alignment_results_nt<<-state$values$grouped_alignment_results_nt
      
      mutation_results<<-state$values$mutation_results
      mutation_results_nt<<-state$values$mutation_results_nt
      mutation_results_cl<<-state$values$mutation_results_cl
      mutation_results_nt_cl<<-state$values$mutation_results_nt_cl
       
      just_restored_session<<-T
      just_restored_session_cleaning<<-T
      just_restored_session_clonotypes<<-T
      just_restored_session_public_clonotypes<<-T
      just_restored_session_Repertoires<<-T
      just_restored_session_HighlySim_Repertoires<<-T
      just_restored_session_repertoires_comparison<<-T
      just_restored_session_Multiple_value_comparison<<-T
      just_restored_session_alignment<<-T
      just_restored_session_mutations<<-T
      just_restored_session_freqTables<<-T
      just_restored_session_logo<<-T
      just_restored_session_alignment<<-T
      just_restored_session_highly_similar_clonotypes<<-T
      
      if (input$Continue!=F)
        newDataset<-F
      else
        newDataset<-newDataset
      
      dataInputColumns <- reactive({
        dataInputColumnsTemp
      })
      
      #Execute Filtering if cleaning has alreary been applied
      output$uiExecute <- renderUI({
        #if (input$Continue==FALSE | newDataset==TRUE){return()}
        actionButton("Execute", "Execute", 
                     style="color: #fff; background-color: #5F021F; border-color: #fff")
        withBusyIndicatorUI(
          actionButton("Execute", "Execute", 
                     style="color: #fff; background-color: #5F021F; border-color: #fff")
        )
      })
      
      #Execute Button for pipeline if filtering has alreary been applied
      output$uiExecute_pipeline <- renderUI({
        #if (length(imgtfilter_results)==0){return()}
        actionButton("Execute_pipeline", "Execute Pipeline", 
                     style="color: #fff; background-color: #5F021F; border-color: #fff")
        withBusyIndicatorUI(
          actionButton(
            "Execute_pipeline", "Execute Pipeline", 
            style="color: #fff; background-color: #5F021F; border-color: #fff"
          )
        )
        
      })
      
      newDataset<-F
      
      #insert repertoires on pipeline tab
      if (length(insertedRepertoires)>0)
        for (i in 1:length(insertedRepertoires)){
          btn <- paste0('selectRepertoires_',insertedRepertoires[i])
          id <- insertedRepertoires[i]
          addRepertoryFct(id, btn)
        }
      
      #insert Multiple_value_comparison on pipeline tab
      if (length(insertedMultiple_value_comparison)>0)
        for (i in 1:length(insertedMultiple_value_comparison)){
          btn <- strsplit(insertedMultiple_value_comparison[i],"_")[[1]][2]
          id <- paste0('MultipleValues_', btn)
          addMultipleValues(id, btn, columns_for_Multiple_value_comparison,Multiple_value_comparison_input_values[i,1],Multiple_value_comparison_input_values[i,2])
        }
      
    })
    
    observeEvent(input$restorePreviousSession, {
      #show the previoys sessions by their dates to the UI
      output$uiPreviousSessions <- renderUI({
        if (is.null(list.files('shiny_bookmarks'))) return()
        #save the date that each bookmarked folder was created
        dates_of_files=lapply(list.files('shiny_bookmarks'),function(x) file.mtime(paste0("shiny_bookmarks/",x)))
        dates_as_char<-c()
        for (j in 1:length(dates_of_files)){
          dates_as_char=c(dates_as_char,as.character(dates_of_files[[j]]))
        }
        dates_as_char_ordered<-dates_as_char[order(dates_as_char)]
        
        wellPanel(
          tags$div(class = "multicol", radioButtons(inputId = "prevSession", label = "Select Session", inline=TRUE, choices = dates_as_char_ordered))
        )
      })
      
      output$uiLoadPreviousSessions <- renderUI({
        if (is.null(input$prevSession)) return()
        all_session_ids=list.files('shiny_bookmarks')
        dates_of_files=lapply(list.files('shiny_bookmarks'),function(x) file.mtime(paste0("shiny_bookmarks/",x)))
        
        #find the session id selected by the user
        for (j in 1:length(dates_of_files)){
          if (strsplit(as.character(dates_of_files[[j]])," [+]")[[1]][1]==input$prevSession){
            session_id=all_session_ids[j] 
            break
          }
        }
        
        
        
        #Update the link according to the new link of the session
        link=paste0("http://127.0.0.1:3168/?_state_id_=",session_id) 
        
        #actionButton("load", "Load",onclick="window.open('http://127.0.0.1:3168/?_state_id_=b09f6c6f6e51b35f', '_blank')")
        
        helpText(a("Click Here to load session", href=link, target="_blank"))
        a(h4("Load Session", class = "btn btn-default action-button" , 
             style = "fontweight:600"), target = "_blank",
          href = link)
        
        
        
      })
      
    })
    
    ####################################### Enable/Disable Tabs ###########################################
    observeEvent(input$pipeline_alignment,{
      if(input$pipeline_alignment){#If true enable, else disable
        js$enabletab("Alignment")
      }else{
        js$disabletab("Alignment")
      }
    })
    
    observeEvent(input$pipeline_mutations,{
      if(input$pipeline_mutations){#If true enable, else disable
        js$enabletab("Mutation_tab")
      }else{
        js$disabletab("Mutation_tab")
      }
    })
    
    observeEvent(input$pipeline_clonotypes,{
      if(input$pipeline_clonotypes){#If true enable, else disable
        js$enabletab("Clonotypes")
      }else{
        js$disabletab("Clonotypes")
      }
    })
    
    observeEvent(input$pipeline_highly_similar_clonotypes,{
      if(input$pipeline_highly_similar_clonotypes){#If true enable, else disable
        js$enabletab("highly_similar_clonotypes")
      }else{
        js$disabletab("highly_similar_clonotypes")
      }
    })
    
    observeEvent(input$pipeline_Repertoires,{
      if(input$pipeline_Repertoires){#If true enable, else disable
        js$enabletab("Repertoires")
      }else{
        js$disabletab("Repertoires")
      }
    })
    
    observeEvent(input$pipeline_Multiple_value_comparison,{
      if(input$pipeline_Multiple_value_comparison){#If true enable, else disable
        js$enabletab("Multiple_value_comparisonTab")
      }else{
        js$disabletab("Multiple_value_comparisonTab")
      }
    })
    
    observeEvent(input$pipeline_CDR3Diff1,{
      if(input$pipeline_CDR3Diff1){#If true enable, else disable
        js$enabletab("CDR3_with_1_length_difference_tab")
      }else{
        js$disabletab("CDR3_with_1_length_difference_tab")
      }
    })
    
    observeEvent(input$pipeline_logo,{
      if(input$pipeline_logo){#If true enable, else disable
        js$enabletab("logo_tab")
      }else{
        js$disabletab("logo_tab")
      }
    })
    
    ############################# In each tab select the dataset you want to see  ######################### ###############################
    output$uiSelectDatasetCleaning <- renderUI({
      if ((is.null(input$Dataset)) & (length(loaded_datasets)==0)){return()}
      selectInput("cleaningDataset", "Select Dataset:",c("All Data",loaded_datasets), width="170px")
    })
    
    output$uiSelectDatasetFiltering <- renderUI({
      if ((is.null(input$Dataset)) & (length(loaded_datasets)==0)){return()}
      selectInput("filteringDataset", "Select Dataset:",c("All Data",loaded_datasets), width="170px")
    })
    
    
    output$uiSelectDatasetClonotypes <- renderUI({
      if ((is.null(input$Dataset)) & (length(loaded_datasets)==0)){return()}
      selectInput("clonotypesDataset", "Select Dataset:",c("All Data",loaded_datasets), width="170px")
    })
    
    output$uiSelectDatasethighly_similar_clonotypes <- renderUI({
      if ((is.null(input$Dataset)) & (length(loaded_datasets)==0)){return()}
      selectInput("highlySimClonotypesDataset", "Select Dataset:",c("All Data",loaded_datasets), width="170px")
    })
    
    output$uiSelectDatasetRepertoires <- renderUI({
      if ((is.null(input$Dataset)) & (length(loaded_datasets)==0)){return()}
      selectInput("RepertoiresDataset", "Select Dataset:",c("All Data",loaded_datasets), width="170px")
    })
    
    output$uiSelectDatasetMultiple_value_comparison <- renderUI({
      if ((is.null(input$Dataset)) & (length(loaded_datasets)==0)){return()}
      selectInput("Multiple_value_comparisonDataset", "Select Dataset:",c("All Data",loaded_datasets), width="170px")
    })
    
    output$uiSelectDatasetAlignment <- renderUI({
      if ((is.null(input$Dataset)) & (length(loaded_datasets)==0)){return()}
      selectInput("alignmentDataset", "Select Dataset:",c("All Data",loaded_datasets), width="170px")
    })
    
    output$uiSelectDatasetMutation <- renderUI({
      if ((is.null(input$Dataset)) & (length(loaded_datasets)==0)){return()}
      selectInput("mutationDataset", "Select Dataset:",c("All Data",loaded_datasets), width="170px")
    })
    
    output$uiSelectDatasetFreqTable <- renderUI({
      if ((is.null(input$Dataset)) & (length(loaded_datasets)==0)){return()}
      selectInput("freqTableDataset", "Select Dataset:",c("All Data",loaded_datasets), width="170px")
    })
    
    output$uiSelectDatasetLogo <- renderUI({
      if ((is.null(input$Dataset)) & (length(loaded_datasets)==0)){return()}
      selectInput("LogoDataset", "Select Dataset:",c("All Data",loaded_datasets), width="170px")
    })
    
    output$uiSelectDatasetCDR3Diff1 <- renderUI({
      if ((is.null(input$Dataset)) & (length(loaded_datasets)==0)){return()}
      selectInput("CDR3Diff1Dataset", "Select Dataset:",c("All Data",loaded_datasets), width="170px")
    })
    
    output$uiSelectDatasetVisualisation <- renderUI({
      if ((is.null(input$Dataset)) & (length(loaded_datasets)==0)){return()}
      selectInput("VisualisationDataset", "Select Dataset:",c("All Data",loaded_datasets), width="170px")
    })
    
    ###################################### Cleaning and Filtering inputs  ################################# ######################################
    
    output$uiStart <- renderUI({
      if (input$start_end==FALSE){return()}
      textInput("start_char", "Start with:",value = "C", width="110px")
    })
    
    output$uiStart_comment <- renderUI({
      if (input$start_end==FALSE){return()}
      helpText("Separate the different letters with | e.g. C|D")
    })
    
    output$uiEnd <- renderUI({
      if (input$start_end==FALSE){return()}
      textInput("end_char", "End with:",value = "F", width="110px")
    })
    
    output$uiEnd_comment <- renderUI({
      if (input$start_end==FALSE){return()}
      helpText("Separate the different letters with | e.g. F|D")
    })
    
    output$uiIdentityLow <- renderUI({
      if (input$identity==FALSE){return()}
      numericInput("identityLow", "Identity Low %:", 95,  min = 0, max = 100, width="110px")
    })
    
    output$uiIdentityHigh <- renderUI({
      if (input$identity==FALSE){return()}
      numericInput("identityHigh", "Identity High %:", 100, min = 0, max = 100, width="110px")
    })
    
    output$uiVGene <- renderUI({
      if (input$VGene==FALSE){return()}
      textInput("VGene_name", "V-Gene names")
    })
    
    output$uiVGene_comment <- renderUI({
      if (input$VGene==FALSE){return()}
      helpText("Separate the different V-Gene names with | e.g. TRBV11-2|TRBV29-1*03 (F)")
    })
    
    output$uiJGene <- renderUI({
      if (input$JGene==FALSE){return()}
      textInput("JGene_name", "J-Gene names")
    })
    
    output$uiJGene_comment <- renderUI({
      if (input$JGene==FALSE){return()}
      helpText("Separate the different J-Gene names with | e.g. TRBJ2-6|TRBJ2-2")
    })
    
    output$uiDGene <- renderUI({
      if (input$DGene==FALSE){return()}
      textInput("DGene_name", "D-Gene names")
    })
    
    output$uiDGene_comment <- renderUI({
      if (input$DGene==FALSE){return()}
      helpText("Separate the different D-Gene names with | e.g. TRBD2|TRBD1")
    })
    
    output$uilengthLow <- renderUI({
      if (input$length==FALSE){return()}
      numericInput("lengthLow", "Length Low Limit:", 7,  min = 0, max = 20, width="140px")
    })
    
    output$uilengthHigh <- renderUI({
      if (input$length==FALSE){return()}
      numericInput("lengthHigh", "Length Upper Limit:", 13, min = 0, max = 20, width="140px")
    })
    
    output$uiAminoacid <- renderUI({
      if (input$aminoacid==FALSE){return()}
      textInput("aminoacid_name", "Amino-acid:")
    })
    
    #Execute Filtering if cleaning has alreary been applied
    output$uiExecute <- renderUI({
      if (input$Continue==FALSE | is.null(input$inputFiles) | is.null(dir()) | is.null(input$Dataset)){return()}
      
      withBusyIndicatorUI(
        actionButton("Execute", "Execute", 
                   style="color: #fff; background-color: #5F021F; border-color: #fff")
      )
    })
    
    ################## Execute Button for pipeline if filtering has alreary been applied ###################
    output$uiExecute_pipeline <- renderUI({
      if ((input$Continue==FALSE | is.null(input$inputFiles) | is.null(dir()) | is.null(input$Dataset) | newDataset==TRUE) & (input$select_load_or_compute_clonotypes=="compute_clonotypes")){return()}
      if (input$select_load_or_compute_clonotypes=="compute_clonotypes")
        if ((input$Execute==FALSE)){return()}
      #actionButton("Execute_pipeline", "Execute Pipeline", 
      #             style="color: #fff; background-color: #fff; border-color: #fff")
      withBusyIndicatorUI(
        actionButton(
          "Execute_pipeline", "Execute Pipeline", 
          style="color: #fff; background-color: #5F021F; border-color: #fff"
        )
      )
    })
    
    ################################################## Cleaning  ########################################## ##################################################
    #newDataset=T when the dataset state is changed. When this happens the execute button has to disapear and cleaning must be applyed
    observeEvent(input$Dataset, {
      newDataset<<-TRUE
      
    })
    
    observeEvent(input$Continue, {
      
      # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
      withBusyIndicatorServer("Continue", {
        
      newDataset<<-FALSE
      loaded_datasets<<-unique(t(data.frame(strsplit(input$Dataset,"_")))[,1])
      if ((is.null(input$inputFiles) | is.null(dir()) | is.null(input$Dataset)) && just_restored_session_cleaning==F) {
        validate(
          #"Please select a data set!"
        )
        showModal(modalDialog(
          title = "Error Message",
          "Please select a data set",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
      
      if (just_restored_session_cleaning==F){
        #check the selected input 
        if(input$cell=="T cell"){
          cell_id=1
        }else{
          cell_id=2
        }
        
        filter_id <-c()
        cleaning_parameters<<-c()
        if (input$Functional){
          filter_id <- c(filter_id, 1)
          cleaning_parameters<<-c(cleaning_parameters,"Functional V-Gene")
        }
        
        if (input$Characters){
          filter_id <- c(filter_id, 2)
          cleaning_parameters<<-c(cleaning_parameters,"CDR3 with no Special Characters")
        }
        
        if (input$Productive){
          filter_id <- c(filter_id, 3)
          cleaning_parameters<<-c(cleaning_parameters,"Productive Sequences")
        }
        
        if (input$start_end){
          filter_id <- c(filter_id, 4)
          cleaning_parameters<<-c(cleaning_parameters,paste0("CDR3 start with ",input$start_char," and end with ",input$end_char))
          if ((input$start_char=="")) start_char=""
          else{
            k=strsplit(as.character(input$start_char),"|")[[1]]
            start_char=""
            for (j in 1:length(k)){
              if (j%%2==1){
                start_char=paste0(start_char,paste0("^",k[j]))
              }else{
                start_char=paste0(start_char,k[j])
              }
            }
          }
        }
        
        if (input$start_end){
          #filter_id <- c(filter_id, 4)
          if ((input$end_char=="")) end_char=""
          else{
            k=strsplit(as.character(input$end_char),"|")[[1]]
            end_char=""
            for (j in 1:length(k)){
              if (j%%2==1){
                end_char=paste0(end_char,paste0(k[j],"$"))
              }else{
                end_char=paste0(end_char,k[j])
              }
            }
          }
        }
        
        
        if (length(new_columns)==0){
          rawDataSet<<-dataInputColumns()$rawDataSet
        }
        
        if (input$cell=="T cell"){
          Tcell <- T
        }else{
          Tcell <- F
        }
        
        if (input$throughput=="High Throughput"){
          imgtcleaning_results<<-imgtcleaning(rawDataSet, loaded_datasets, list.files(path()), input$inputFiles, cell_id, filter_id, " P| ORF", "[*]|X|#|[.]", "productive", start_char, end_char, as.numeric(input$identityLow), as.numeric(input$identityHigh), input$VGene_name, input$JGene_name, input$DGene_name, as.numeric(input$lengthLow), as.numeric(input$lengthHigh), input$aminoacid_name, seq1,seq2,Tcell)
        }else{
          imgtcleaning_results<<-imgtcleaningLow(rawDataSet, loaded_datasets, list.files(path()), input$inputFiles, cell_id, filter_id, " P| ORF", "[*]|X|#|[.]", "productive", start_char, end_char, as.numeric(input$identityLow), as.numeric(input$identityHigh), input$VGene_name, input$JGene_name, input$DGene_name, as.numeric(input$lengthLow), as.numeric(input$lengthHigh), input$aminoacid_name, seq1,seq2,Tcell)
        }
        
      }
      
      just_restored_session_cleaning<<-F
      
      cleaning_workflow<<-imgtcleaning_results$workflow
      
      
      ################################################
      cleaning_confirm<<-imgtcleaning_results$confirm
      
      msg<<-imgtcleaning_results$message
      
      
      ################# Cleaning Results to tables #################
      
      output$tableCleaning<-renderTable({
        if(is.null(input$cleaningDataset)) return()
        if (input$cleaningDataset=="All Data") tab <- imgtcleaning_results$workflow
        else tab <- imgtcleaning_results$workflow_datasets[[input$cleaningDataset]]
        colnames(tab) <- c('Filter id','Filter out', 'Filter in')
        return(tab)
      },digits = 0)
      
      output$tableTitleCleaning <- renderUI({
        h3("Cleaning results")
      })
      
      output$allDataInitialTableCleaning<-renderDataTable({
        if(is.null(input$cleaningDataset)) return()
        if (input$cleaningDataset=="All Data"){
          tab <- imgtcleaning_results$allDataInitial
          return(tab) 
        }
        else{
          tab <- imgtcleaning_results$initial_datasets[[input$cleaningDataset]]
          return(tab)
        }
        
      },options = list(scrollX = TRUE))
      
      output$downloadallDataInitialTableCleaning <- downloadHandler(
        filename = function(){paste0("InitialTableCleaning_",input$cleaningDataset,".txt")},
        content = function(file) {
          if (input$cleaningDataset=="All Data") write.table(imgtcleaning_results$allDataInitial, file, sep = "\t", row.names = FALSE, col.names = TRUE)
          else write.table(imgtcleaning_results$initial_datasets[[input$cleaningDataset]], file, sep = "\t", row.names = FALSE, col.names = TRUE)
        }) 
      
      output$allDataInitialTableTitleCleaning <- renderUI({
        h3("All Data table")
      })
      
      output$filterInTableCleaning<-renderDataTable({
        if(is.null(input$cleaningDataset)) return()
        if (input$cleaningDataset=="All Data"){
          tab <- imgtcleaning_results$allData
          return(tab) 
        }
        else{
          tab <- imgtcleaning_results$cleaned_datasets[[input$cleaningDataset]]
          return(tab)
        }
      },options = list(scrollX = TRUE))
      
      output$downloadfilterInTableCleaning <- downloadHandler(
        filename = function(){paste0("filterInTableCleaning_",input$cleaningDataset,".txt")},
        content = function(file) {
          if (input$cleaningDataset=="All Data") write.table(imgtcleaning_results$allData, file, sep = "\t", row.names = FALSE, col.names = TRUE)
          else write.table(imgtcleaning_results$cleaned_datasets[[input$cleaningDataset]], file, sep = "\t", row.names = FALSE, col.names = TRUE)
        }) 
      
      output$filterInTableTitleCleaning <- renderUI({
        h3("Clean table")
      })
      
      output$filterOutTableCleaning<-renderDataTable({
        if(is.null(input$cleaningDataset)) return()
        if (input$cleaningDataset=="All Data"){
          tab <- imgtcleaning_results$filterOutSum
          return(tab) 
        }
        else{
          tab <- imgtcleaning_results$cleaned_out_datasets[[input$cleaningDataset]]
          return(tab)
        }
      },options = list(scrollX = TRUE))
      
      output$filterOutTableTitleCleaning <- renderUI({
        h3("Clean out table")
      })
      
      output$downloadfilterOutTableCleaning <- downloadHandler(
        filename = function(){paste0("filterOutTableCleaning_",input$cleaningDataset,".txt")},
        content = function(file) {
          if (input$cleaningDataset=="All Data") write.table(imgtcleaning_results$filterOutSum, file, sep = "\t", row.names = FALSE, col.names = TRUE)
          else write.table(imgtcleaning_results$cleaned_out_datasets[[input$cleaningDataset]], file, sep = "\t", row.names = FALSE, col.names = TRUE)
        }) 
      
      output$confirmCleaning <- renderUI({
        h5(imgtcleaning_results$confirm, style = "color: #CD0000;")
      })
      return()

      })
      
    })
    
    
    ################################################## Filtering ##########################################
    observeEvent(input$Execute, {
      
      withBusyIndicatorServer("Execute", {
      #check the selected input 
      if(input$cell=="T cell"){
        cell_id=1
      }else{
        cell_id=2
      }
      
      #release memory
      rawDataSet<<-list()
      worng_columns_id <- list()
      new_columns <- list()
        
      filter_id <-c()
      filtering_parameters<<-c()
      
      if (input$identity){
        filter_id <- c(filter_id, 5)
        filtering_parameters<<-c(filtering_parameters,paste0("V-REGION identity from ",input$identityLow,"% to ",input$identityHigh,"%"))
      }
      
      if (input$VGene){
        filter_id <- c(filter_id, 6)
        filtering_parameters<<-c(filtering_parameters,paste0("V Gene: ",input$VGene_name))
      }
      
      if (input$JGene){
        filter_id <- c(filter_id, 7)
        filtering_parameters<<-c(filtering_parameters,paste0("J Gene: ",input$JGene_name))
      }
      
      if (input$DGene){
        filter_id <- c(filter_id, 8)
        filtering_parameters<<-c(filtering_parameters,paste0("D Gene: ",input$DGene_name))
      }
      
      if (input$length){
        filter_id <- c(filter_id, 9)
        filtering_parameters<<-c(filtering_parameters,paste0("CDR3 length from ",input$lengthLow," to ",input$lengthHigh))
      }   
      
      if (input$aminoacid){
        filter_id <- c(filter_id, 10)
        filtering_parameters<<-c(filtering_parameters,paste0("CDR3 containing the amino-acid sequence: ",input$aminoacid_name))
      }
      
      if (just_restored_session==F || length(imgtfilter_results)==0){
        if (input$throughput=="High Throughput")
          imgtfilter_results<<-imgtfilter(imgtcleaning_results$cleaned_datasets,loaded_datasets, imgtcleaning_results$allData, cell_id, filter_id, " P| ORF", "[*]|X|#|[.]", "productive", start_char, end_char, as.numeric(input$identityLow), as.numeric(input$identityHigh), input$VGene_name, input$JGene_name, input$DGene_name, as.numeric(input$lengthLow), as.numeric(input$lengthHigh), input$aminoacid_name, seq1,seq2)
        else
          imgtfilter_results<<-imgtfilterLow(imgtcleaning_results$cleaned_datasets,loaded_datasets, imgtcleaning_results$allData, cell_id, filter_id, " P| ORF", "[*]|X|#|[.]", "productive", start_char, end_char, as.numeric(input$identityLow), as.numeric(input$identityHigh), input$VGene_name, input$JGene_name, input$DGene_name, as.numeric(input$lengthLow), as.numeric(input$lengthHigh), input$aminoacid_name, seq1,seq2)
        
      }
      
      just_restored_session<<-F
      
      filtering_workflow<<-imgtfilter_results$workflow
      
      output$confirmCleaningFiltering <- renderUI({
        h5(cleaning_confirm, style = "color: #CD0000;")
      })
      
      output$confirmFiltering <- renderUI({
        h5(imgtfilter_results$confirm, style = "color: #CD0000;")
      })
      
      msg<<-imgtfilter_results$message
      
      
      ################ Filtering Results to tables ###############
      output$table<-renderTable({
        if(is.null(input$filteringDataset)) return()
        if (input$filteringDataset=="All Data") tab <- imgtfilter_results$workflow
        else tab <- imgtfilter_results$workflow_datasets[[input$filteringDataset]]
        colnames(tab) <- c('Filter id','Filter out', 'Filter in')
        return(tab)
        #imgtfilter_results$workflow
      },digits = 0)
      
      output$tableTitle <- renderUI({
        h3("Filtering results")
      })
      
      output$allDataInitialTable<-renderDataTable({
        if(is.null(input$filteringDataset)) return()
        if (input$filteringDataset=="All Data"){
          tab <- imgtfilter_results$allDataInitial
          return(tab) 
        }
        else{
          tab <- imgtfilter_results$imgtcleaning_results$initial_datasets[[input$filteringDataset]]
          return(tab)
        }
      },options = list(scrollX = TRUE))
      
      output$downloadAllDataInitialTable <- downloadHandler(
        filename = function(){paste0("DataInitial",input$filteringDataset,".txt")},
        content = function(file) {
          if (input$filteringDataset=="All Data") write.table(imgtfilter_results$allDataInitial, file, sep = "\t", row.names = FALSE, col.names = TRUE)
          else write.table(imgtcleaning_results$initial_datasets[[input$filteringDataset]], file, sep = "\t", row.names = FALSE, col.names = TRUE)
        }) 
      
      output$allDataInitialTableTitle <- renderUI({
        h3("All Data table")
      })
      
      output$filterInTable<-renderDataTable({
        if(is.null(input$filteringDataset)) return()
        if (input$filteringDataset=="All Data"){
          tab <- imgtfilter_results$allData
          return(tab) 
        }
        else{
          tab <- imgtfilter_results$filtered_datasets[[input$filteringDataset]]
          return(tab)
        }
      },options = list(scrollX = TRUE))
      
      output$downloadfilterInTable <- downloadHandler(
        filename = function(){paste0("filterInTable",input$filteringDataset,".txt")},
        content = function(file) {
          if (input$filteringDataset=="All Data") write.table(imgtfilter_results$allData, file, sep = "\t", row.names = FALSE, col.names = TRUE)
          else write.table(imgtfilter_results$filtered_datasets[[input$filteringDataset]], file, sep = "\t", row.names = FALSE, col.names = TRUE)
        }) 
      
      output$filterInTableTitle <- renderUI({
        h3("Filter in table")
      })
      
      output$filterOutTable<-renderDataTable({
        if(is.null(input$filteringDataset)) return()
        if (input$filteringDataset=="All Data"){
          tab <- imgtfilter_results$filterOutSum
          return(tab) 
        }
        else{
          tab <- imgtfilter_results$filtered_out_datasets[[input$filteringDataset]]
          return(tab)
        }
      },options = list(scrollX = TRUE))
      
      output$downloadfilterOutTable <- downloadHandler(
        filename = function(){paste0("filterOutTable",input$filteringDataset,".txt")},
        content = function(file) {
          if (input$filteringDataset=="All Data") write.table(imgtfilter_results$filterOutSum, file, sep = "\t", row.names = FALSE, col.names = TRUE)
          else write.table(imgtfilter_results$filtered_out_datasets[[input$filteringDataset]], file, sep = "\t", row.names = FALSE, col.names = TRUE)
        }) 
      
      output$filterOutTableTitle <- renderUI({
        h3("Filter out table")
      })
      
      
      })
      
    })
      
    ########################################### Pipeline  ############################################### 
    observeEvent(input$Execute_pipeline, {
      msgClonotypes<<-""
      if (length(insertedRepertoires)>0)
        for (i in 1:length(insertedRepertoires)) msgRepertoires[i]<<-""
        if (length(msgMultiple_value_comparison)>0)
          for (i in 1:length(insertedMultiple_value_comparison)) msgMultiple_value_comparison[i]<<-""
          msgAlignment<<-""
          msgGroupedAlignment<<-""
          msgFreqTables<<-""
          msgLogo<<-""
          msgCDR3Diff1<<-""
          
          output$confirmClonotype <- renderUI({
            h5(msgClonotypes)
          })
          
          output$confirmhighlySimClonotypes <- renderUI({
            h5("")
          })
          
          output$confirmPublicClonotypes <- renderUI({
            h5("")
          })
          
          output$confirmRepertoiresUi <- renderUI({
            h5("")
          })
          
          output$confirmRepertoiresComparison <- renderUI({
            h5("")
          })
          
          output$confirmMultiple_value_comparison <- renderUI({
            h5("")
          })
          
          output$confirmAlignment <- renderUI({
            h5(msgAlignment)
          })
          
          output$confirmGroupedAlignment<- renderUI({
            h5(msgGroupedAlignment)
          })
          
          output$confirmMutations <- renderUI({
            h5("")
          })
          
          output$confirmFrequenciesTables <- renderUI({
            h5(msgFreqTables)
          })
          
          output$confirmLogo <- renderUI({
            h5(msgLogo)
          })
          
          output$confirmCDR3Diff1 <- renderUI({
            h5(msgCDR3Diff1)
          })
          
    })
    
    ########################################### Error Msg  ##############################################
    observeEvent(input$Execute_pipeline, {
      if (input$pipeline_mutations){
        if (input$pipeline_alignment==F) {
          validate(
            #"Please ckeck Alignment first!"
          )
          showModal(modalDialog(
            title = "Error Message Mutations",
            "Please ckeck Alignment first!",
            easyClose = TRUE,
            footer = NULL
          ))
          return()
        }
        
        if (input$pipeline_alignment==T & input$AAorNtMutations=="both" & input$AAorNtAlignment !="both") {
          validate(
            #"Please ckeck Alignment both first!"
          )
          showModal(modalDialog(
            title = "Error Message Mutations",
            "Please ckeck Alignment both first!",
            easyClose = TRUE,
            footer = NULL
          ))
          return()
        }
        
        if (input$pipeline_alignment==T & input$AAorNtMutations=="aa" & input$AAorNtAlignment =="nt") {
          validate(
            #"Please ckeck Alignment both first!"
          )
          showModal(modalDialog(
            title = "Error Message Mutations",
            "Please ckeck Alignment aa first!",
            easyClose = TRUE,
            footer = NULL
          ))
          return()
        }
        
        if (input$pipeline_alignment==T & input$AAorNtMutations=="nt" & input$AAorNtAlignment =="aa") {
          validate(
            #"Please ckeck Alignment nt first!"
          )
          showModal(modalDialog(
            title = "Error Message Mutations",
            "Please ckeck Alignment nt first!",
            easyClose = TRUE,
            footer = NULL
          ))
          return()
        }
      }
    })
    
    ########################################### Clonotypes ##############################################  
    observeEvent(input$Execute_pipeline, {
      
      if (input$pipeline_clonotypes==F) return()
      
      # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
      withBusyIndicatorServer("Execute_pipeline", {
        
        if (input$select_load_or_compute_clonotypes == 'load_clonotypes'){
          load("rData files/clono.rdata")
          clono<<-clono
          loaded_datasets<<-names(clono$clono_datasets)
          #used columns
          load("rData files/used_columns.rData")
          used_columns<<-used_columns
        }else{
          cdr3_lengths<<-sort(unique(imgtfilter_results$allData[[used_columns[["Summary"]][15]]]))
          cdr3_lengths<<-as.numeric(cdr3_lengths) #+2
          cdr3_lengths<<-sort(cdr3_lengths)
          #save(cdr3_lengths,file=paste0(output_folder,"/cdr3_lengths.rData"))
        }
        
        if (input$select_clonotype=="V Gene + CDR3 Amino Acids"){
          allele=F
          gene=used_columns[["Summary"]][3]
          junction=used_columns[["Summary"]][18]
        }else if (input$select_clonotype=="V Gene and Allele + CDR3 Amino Acids"){
          allele=T
          gene=used_columns[["Summary"]][3]
          junction=used_columns[["Summary"]][18]
        }else if (input$select_clonotype=="V Gene + CDR3 Nucleotide"){
          allele=F
          gene=used_columns[["Summary"]][3]
          junction=used_columns[["IMGT.gapped.nt.sequences"]][9]
        }else if (input$select_clonotype=="V Gene and Allele + CDR3 Nucleotide"){
          allele=T
          gene=used_columns[["Summary"]][3]
          junction=used_columns[["IMGT.gapped.nt.sequences"]][9]
        }else if (input$select_clonotype=="J Gene + CDR3 Amino Acids"){
          allele=F
          gene=used_columns[["Summary"]][8]
          junction=used_columns[["Summary"]][18]
        }else if (input$select_clonotype=="J Gene and Allele + CDR3 Amino Acids"){
          allele=T
          gene=used_columns[["Summary"]][3]
          junction=used_columns[["Summary"]][18]
        }else if (input$select_clonotype=="J Gene + CDR3 Nucleotide"){
          allele=F
          gene=used_columns[["Summary"]][8]
          junction=used_columns[["IMGT.gapped.nt.sequences"]][9]
        }else if (input$select_clonotype=="J Gene and Allele + CDR3 Nucleotide"){
          allele=T
          gene=used_columns[["Summary"]][8]
          junction=used_columns[["IMGT.gapped.nt.sequences"]][9]
        }else if (input$select_clonotype=="CDR3 Amino Acids"){
          allele=F
          junction=used_columns[["Summary"]][18]
          gene=c()
        }else{
          allele=F
          junction=used_columns[["IMGT.gapped.nt.sequences"]][9]
          gene=c()
        }
        
        gene_clonotypes<<-gene
        junction_clonotypes<<-junction
        allele_clonotypes<<-allele
        
        if ((just_restored_session_clonotypes==F) & (input$select_load_or_compute_clonotypes != 'load_clonotypes'))
          clono<<-clonotypes(imgtfilter_results$allData,allele,gene,junction,loaded_datasets)
        
        just_restored_session_clonotypes<<-F
        #convergent_evolution_list_datasets<<-clono$convergent_evolution_list_datasets
        
        msgClonotypes<<-clono$confirm
        
        output$clonoTable <- renderDataTable({
          if(is.null(input$clonotypesDataset)) return()
          if (input$clonotypesDataset=="All Data"){
            my_table <- clono$clono_allData
          }
          else{
            my_table <- clono$clono_datasets[[input$clonotypesDataset]]
          }
          colnames(my_table) <- c(paste0("Clonotype (",input$select_clonotype,")"),'N','Freq','Convergent Evolution')
          my_table[[paste0("Clonotype (",input$select_clonotype,")")]] <- sapply(my_table[[paste0("Clonotype (",input$select_clonotype,")")]], function(x) {as.character(tags$a(href = "#", onclick = sprintf(on_click_js,x), x))})
          my_table[['Convergent Evolution']] <- sapply(my_table[['Convergent Evolution']], function(x) {as.character(tags$a(href = "#", onclick = sprintf(on_click_js_convergent_evolution,x), x))})
          return(my_table)
          
        }, escape = FALSE,options = list(
          autoWidth = FALSE,
          columnDefs = list(list(width = '40%', targets = 1)) )
        )
        
        output$downloadAllClonotypes <- downloadHandler(
          filename = function(){paste0("Clonotypes_",input$select_clonotype,"_",input$clonotypesDataset,".txt")},
          content = function(file) {
            if (input$clonotypesDataset=="All Data"){
              clono$clono_allData$CDR3=clono$clono_allData[,1]
              clono$clono_allData=clono$clono_allData[,c(1,5,2:4)]
              for (i in 1:nrow(clono$clono_allData)){
                clono$clono_allData[i,2]=strsplit(as.character(clono$clono_allData[i,1])," - ")[[1]][2]
                clono$clono_allData[i,1]=strsplit(as.character(clono$clono_allData[i,1])," - ")[[1]][1]
              }
            }else{
              clono$clono_datasets[[input$clonotypesDataset]]$CDR3=clono$clono_datasets[[input$clonotypesDataset]][,1]
              clono$clono_datasets[[input$clonotypesDataset]]=clono$clono_datasets[[input$clonotypesDataset]][,c(1,5,2:4)]
              for (i in 1:nrow(clono$clono_datasets[[input$clonotypesDataset]])){
                clono$clono_datasets[[input$clonotypesDataset]][i,2]=strsplit(as.character(clono$clono_datasets[[input$clonotypesDataset]][i,1])," - ")[[1]][2] 
                clono$clono_datasets[[input$clonotypesDataset]][i,1]=strsplit(as.character(clono$clono_datasets[[input$clonotypesDataset]][i,1])," - ")[[1]][1]   
              }
            }
            
            if (input$clonotypesDataset=="All Data") write.table(clono$clono_allData, file, sep = "\t", row.names = FALSE, col.names = TRUE)
            else write.table(clono$clono_datasets[[input$clonotypesDataset]], file, sep = "\t", row.names = FALSE, col.names = TRUE)
          }) 
        
        output$viewSpecificClonotype<- DT::renderDataTable({
          if (input$clonotypesDataset=="All Data"){
            my_table<-clono$view_specific_clonotype_allData[[input$mydata]]
          }
          else{
            my_table<-clono$view_specific_clonotype_datasets[[input$clonotypesDataset]][[input$mydata]]
          }
          #my_table <- viewClonotypes(data,allele,gene,junction,clonotype[[1]][1],clonotype[[1]][2])
          specificClonotypes<<-my_table
          return(my_table)
        }, escape = FALSE,options = list(scrollX = TRUE)
        )
        
        output$viewSpecificConvergentEvolution<- DT::renderDataTable({
          cluster=strsplit(input$conv_evo," ")[[1]][2]
          if (input$clonotypesDataset=="All Data"){
            my_table<-clono$convergent_evolution_list_allData[[cluster]]
          }
          else{
            my_table<-clono$convergent_evolution_list_datasets[[input$clonotypesDataset]][[cluster]]
          }
          colnames(my_table)<-c('IMGT.gapped.nt.sequences.CDR3.IMGT','N: Convergent Evolution')
          SpecificConvergentEvolution<<-my_table
          return(my_table)
        }, escape = FALSE,options = list(scrollX = TRUE)
        )
        
        output$downloadElementsOfClonotype <- downloadHandler(
          filename = function(){
            paste0("ElementsOfClonotype ",input$mydata,".txt")
          } ,
          content = function(file) {
            write.table(specificClonotypes, file, sep = "\t", row.names = FALSE, col.names = TRUE)
          })
        
        output$downloadConvergentEvolution <- downloadHandler(
          filename = function(){
            paste0("ConvergentEvolution ",input$conv_evo,".txt")
          } ,
          content = function(file) {
            write.table(SpecificConvergentEvolution, file, sep = "\t", row.names = FALSE, col.names = TRUE)
          })
        
        ########### create frequency matrix for bar plots
        
        ########### 
        
        output$clonotypes_bar_plot <- renderPlot({
          if(is.null(input$VisualisationDataset)) return()
          
          if (input$clonotypes_barplot_select_range==F){
            #Find the clonotypes that we want to draw for all the datasets
            cl<-c()
            a<-list()
            if (is.null(input$clonotypes_barchart_threshold)) thr=0 else thr=input$clonotypes_barchart_threshold
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
          #freq_mat<<-round(freq_mat,2)
          
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
        }) 
        
        
        output$confirmClonotype <- renderUI({
          h5(msgClonotypes, style = "color: #CD0000;")
        })
        
        #save(clono, file=paste0(output_folder,"/clono.rdata"))
        
        
      }) #with busy indicator 
      
    })
    
    ################################### Highly Similar Clonotypes #####################################  
    observeEvent(input$pipeline_highly_similar_clonotypes, {
      if ((input$select_load_or_compute_clonotypes != 'load_clonotypes') & (just_restored_session_highly_similar_clonotypes==F)){
        cdr3_lengths<<-sort(unique(imgtfilter_results$allData[[used_columns[["Summary"]][15]]]))
        cdr3_lengths<<-as.numeric(cdr3_lengths) #+2
        cdr3_lengths<<-sort(cdr3_lengths)
      }else{
        load("rData files/cdr3_lengths.rData")
        cdr3_lengths<<-cdr3_lengths
      }

      lapply(1:length(cdr3_lengths), function(i) {
        output[[paste0("num_of_mismatches_length", i)]] <- renderUI({
          numericInput(paste0("num_of_missmatches_cdr3_length_",i),paste0("CDR3 Length ",cdr3_lengths[i]), 1,  min = 1, max = 100, width="140px")
        })
      })
      
      lapply(1:length(cdr3_lengths), function(i) {
        output[[paste0("num_of_mismatches_length_thr", i)]] <- renderUI({
          numericInput(paste0("num_of_mismatches_thr_cdr3_length_",i),paste0("CDR3 Length ",cdr3_lengths[i]), 20,  min = 1, max = 100, width="140px")
        })
      })
      
      output$select_highly_similar_clonotypes_parameters<- renderUI({
        #mainPanel(
        fluidRow(
          h4("Select number of missmatches"),
          radioButtons("select_highly_sim_num_of_missmatches", "Use:",
                       c("Number" = "select_highly_sim_num_of_missmatches_number",
                         "Threshold %" = "select_highly_sim_num_of_missmatches_thr")),
          
          conditionalPanel(
            condition = "input.select_highly_sim_num_of_missmatches == 'select_highly_sim_num_of_missmatches_number'",
            lapply(1:length(cdr3_lengths), function(i) { 
              column(2,uiOutput(paste0("num_of_mismatches_length", i)))
            })
          ),
          
          conditionalPanel(
            condition = "input.select_highly_sim_num_of_missmatches == 'select_highly_sim_num_of_missmatches_thr'",
            lapply(1:length(cdr3_lengths), function(i) { 
              column(2,uiOutput(paste0("num_of_mismatches_length_thr", i)))
            })
          ),
          
          
          column(3,h4("Select Clonotype Frequency Threshold")),
          numericInput("clonotype_freq_thr_for_highly_sim", "Threshold% : range [0,100]:", 1,  min = 0, max = 100, width="140px"),
          h4("Take Gene into account"),
          selectInput("take_gene_highly_similar", "Select type:",c("Yes", "No"), width="320")
        )
        
        #)  
      })
      
      output$select_length_to_show_higlySimClono_ui<-renderUI({
        selectInput("select_length_to_show_highlySimClono", "Select length:", cdr3_lengths, width="170px")
      })
    })
    
    observeEvent(input$Execute_pipeline, {
      
      if (input$pipeline_highly_similar_clonotypes==F) return()
      
      if (input$pipeline_clonotypes==F) {
        validate(
          #"Please ckeck Clonotypes first!"
        )
        showModal(modalDialog(
          title = "Error Message Repertoires",
          "Please ckeck Clonotypes first!",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
      
      # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
      withBusyIndicatorServer("Execute_pipeline", {
        num_of_missmatches<-c()
        if (input$select_highly_sim_num_of_missmatches == 'select_highly_sim_num_of_missmatches_number'){
          for (i in 1:length(cdr3_lengths)){
            num_of_missmatches=c(num_of_missmatches,input[[paste0("num_of_missmatches_cdr3_length_",i)]])
          }
        }else{
          for (i in 1:length(cdr3_lengths)){
            num_of_missmatches=c(num_of_missmatches,round(cdr3_lengths[i]*input[[paste0("num_of_mismatches_thr_cdr3_length_",i)]]/100,0))
          }
        }
        
        if (just_restored_session_highly_similar_clonotypes==F)
          highly_similar_clonotypes_results<<-highly_similar_clonotypes(clono$clono_allData,clono$clono_datasets,num_of_missmatches,input$take_gene_highly_similar,cdr3_lengths,gene_clonotypes,input$clonotype_freq_thr_for_highly_sim,loaded_datasets)
        
        just_restored_session_highly_similar_clonotypes<<-F
        
        msgHighlySim<<-highly_similar_clonotypes_results$confirm
        #highly_sim_view_specific_clonotypes,highly_sim_clonotypes
        
        #highly_sim_datasets<-list()
        for (d in names(highly_similar_clonotypes_results$highly_sim_clonotypes_datasets)){
          temp<-do.call(rbind.data.frame, highly_similar_clonotypes_results$highly_sim_clonotypes_datasets[[d]])
          temp$clonotype<-as.character(temp$clonotype)
          row.names(temp)=NULL
          temp=temp[,c("clonotype", "N", "Freq", "prev_cluster")]
          temp=temp[order(-temp$N),]
          row.names(temp)=1:nrow(temp)
          highly_sim_datasets[[d]]<<-temp
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
        
        highly_sim<<-do.call(rbind.data.frame, highly_similar_clonotypes_results$highly_sim_clonotypes)
        highly_sim$clonotype<<-as.character(highly_sim$clonotype)
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
        
        output$highlySimAllClonoTable <- renderDataTable({
          if(is.null(input$highlySimClonotypesDataset)) return()
          if (input$highlySimClonotypesDataset=="All Data"){
            my_table <- highly_sim
          }
          else{
            my_table <- highly_sim_datasets[[input$highlySimClonotypesDataset]]
          }
          return(my_table)
          
        }, escape = FALSE,options = list(
          autoWidth = FALSE,
          columnDefs = list(list(width = '40%', targets = 1)) )
        )
        
        output$downloadHighlySimAllClonoTable <- downloadHandler(
          filename = function(){paste0("highly_sim_all_clonotypes_",input$highlySimClonotypesDataset,".txt")},
          content = function(file) {
            if (input$highlySimClonotypesDataset=="All Data") write.table(highly_sim, file,sep = "\t", row.names = FALSE, col.names = TRUE)
            else write.table(highly_sim_datasets[[input$highlySimClonotypesDataset]], file, sep = "\t", row.names = FALSE, col.names = TRUE)
          }) 
        
        output$highlySimClonoTable <- renderDataTable({
          if(is.null(input$highlySimClonotypesDataset)) return()
          if (input$highlySimClonotypesDataset=="All Data"){
            my_table <- highly_similar_clonotypes_results$highly_sim_clonotypes[[paste0("length ",input$select_length_to_show_highlySimClono)]]
          }
          else{
            my_table <- highly_similar_clonotypes_results$highly_sim_clonotypes_datasets[[input$highlySimClonotypesDataset]][[paste0("length ",input$select_length_to_show_highlySimClono)]]
          }
          return(my_table)
          
        }, escape = FALSE,options = list(
          autoWidth = FALSE,
          columnDefs = list(list(width = '40%', targets = 1)) )
        )
        
        output$downloadAllhighlySimClonotypes <- downloadHandler(
          filename = function(){paste0("highly_sim_clonotypes",input$highlySimClonotypesDataset,"_",paste0("length ",input$select_length_to_show_highlySimClono),".txt")},
          content = function(file) {
            if (input$highlySimClonotypesDataset=="All Data") write.table(highly_similar_clonotypes_results$highly_sim_clonotypes[[paste0("length ",input$select_length_to_show_highlySimClono)]], file, sep = "\t", row.names = FALSE, col.names = TRUE)
            else write.table(highly_similar_clonotypes_results$highly_sim_clonotypes_datasets[[input$highlySimClonotypesDataset]][[paste0("length ",input$select_length_to_show_highlySimClono)]], file, sep = "\t", row.names = FALSE, col.names = TRUE)
          }) 
        
        output$highlySimClono_allGroups_Table <- renderDataTable({
          if(is.null(input$highlySimClonotypesDataset)) return()
          if (input$highlySimClonotypesDataset=="All Data"){
            my_table <- highly_similar_clonotypes_results$highly_sim_clonotypes_allGroups[[paste0("length ",input$select_length_to_show_highlySimClono)]]
          }
          else{
            my_table <- highly_similar_clonotypes_results$highly_sim_clonotypes_allGroups_datasets[[input$highlySimClonotypesDataset]][[paste0("length ",input$select_length_to_show_highlySimClono)]]
          }
          return(my_table)
          
        }, escape = FALSE,options = list(
          autoWidth = FALSE,
          columnDefs = list(list(width = '40%', targets = 1)) )
        )
        
        output$downloadAllhighlySimClonotypes_allGroups <- downloadHandler(
          filename = function(){paste0("highly_sim_clonotypes_allGroups",input$highlySimClonotypesDataset,"_",paste0("length ",input$select_length_to_show_highlySimClono),".txt")},
          content = function(file) {
            if (input$highlySimClonotypesDataset=="All Data") write.table(highly_similar_clonotypes_results$highly_sim_clonotypes_allGroups[[paste0("length ",input$select_length_to_show_highlySimClono)]], file, sep = "\t", row.names = FALSE, col.names = TRUE)
            else write.table(highly_similar_clonotypes_results$highly_sim_clonotypes_allGroups_datasets[[input$highlySimClonotypesDataset]][[paste0("length ",input$select_length_to_show_highlySimClono)]], file, sep = "\t", row.names = FALSE, col.names = TRUE)
          }) 
        
        ########### create frequency matrix for bar plots
        
        ########### 
        ############################################# Change Parameters !!!!!!!!!!!!!!!!!!!!!!!
        output$higly_sim_clonotypes_bar_plot <- renderPlot({
          if(is.null(input$VisualisationDataset)) return()
          
          if (input$higly_sim_clonotypes_barplot_select_range==F){
            #Find the clonotypes that we want to draw for all the datasets
            cl<-c()
            a<-list()
            if (is.null(input$higly_sim_clonotypes_barchart_threshold)) thr=0 else thr=input$higly_sim_clonotypes_barchart_threshold
            a[["allData"]]=highly_sim %>% filter(highly_sim$Freq>thr)
            cl=c(cl,a[["allData"]]$clonotype)
            for (i in loaded_datasets){
              a[[i]]=highly_sim_datasets[[i]] %>% filter(highly_sim_datasets[[i]]$Freq>thr)
              cl<-c(cl,a[[i]]$clonotype)
            }
            
          }else{
            #Find the clonotypes that we want to draw for all the datasets
            range=input$higly_sim_clonotypes_barchart_down_threshold:input$higly_sim_clonotypes_barchart_up_threshold
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
          #freq_mat<<-round(freq_mat,2)
          
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
          
        }) 
        
        output$confirmhighlySimClonotypes <- renderUI({
          h5(msgHighlySim, style = "color: #CD0000;")
        })
        
      })
    })
    
    
    ################################### Shared Clonotypes #####################################  
    observeEvent(input$Execute_pipeline, {
      
      if (input$pipeline_public_clonotypes==F) return()
      
      if (input$pipeline_clonotypes==F) {
        validate(
          #"Please ckeck Clonotypes first!"
        )
        showModal(modalDialog(
          title = "Error Message Repertoires",
          "Please ckeck Clonotypes first!",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
      
      # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
      withBusyIndicatorServer("Execute_pipeline", {
        if (input$select_topN_or_reads_thr_shared_clono=="select_reads_thr_shared_clono"){
          use_reads <- T
          threshlod <- input$thr_public_clono_reads
        }else{
          use_reads <- F
          threshlod <- input$thr_public_clono_topN
        }
        
        if (just_restored_session_public_clonotypes==F)
          public_clonotypes_results<<-public_clonotypes(clono$clono_allData,clono$clono_datasets,input$take_gene_public_clono,use_reads,threshlod,loaded_datasets,F)
        
        just_restored_session_public_clonotypes<<-F
        #highly_sim_view_specific_clonotypes,highly_sim_clonotypes
        
        msgPublicClono<<-public_clonotypes_results$confirm
        
        output$public_clonotypes_Table <- renderDataTable({
          if(is.null(public_clonotypes_results)) return()
          my_table<-public_clonotypes_results$public_clono
          return(my_table)
        },options = list(scrollX = TRUE))
        
        output$downloadPublic_clonotypes <- downloadHandler(
          filename = function(){paste0("public_clonotypes",".txt")},
          content = function(file) {
            write.table(public_clonotypes_results$public_clono, file, sep = "\t", row.names = FALSE, col.names = TRUE)
          }) 
        
        output$confirmPublicClonotypes <- renderUI({
          h5(msgPublicClono, style = "color: #CD0000;")
        })
        
      })
    })
    
    ############################ Highly Similar Public Clonotypes ############################  
    observeEvent(input$Execute_pipeline, {
      
      if (input$pipeline_highly_sim_public_clonotypes==F) return()
      
      if (input$pipeline_highly_similar_clonotypes==F) {
        validate(
          #"Please ckeck Clonotypes first!"
        )
        showModal(modalDialog(
          title = "Error Message Repertoires",
          "Please ckeck Clonotypes first!",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
      
      # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
      withBusyIndicatorServer("Execute_pipeline", {
        if (just_restored_session_public_clonotypes==F)
          highly_sim_public_clonotypes_results<<-public_clonotypes(highly_sim,highly_sim_datasets,input$take_gene_highly_sim_public_clono,T,input$thr_highly_sim_public_clono,loaded_datasets,T)
        
        just_restored_session_highly_sim_public_clonotypes<<-F
        #highly_sim_view_specific_clonotypes,highly_sim_clonotypes
        
        msgPublicClono<<-highly_sim_public_clonotypes_results$confirm
        
        output$highly_sim_public_clonotypes_Table <- renderDataTable({
          if(is.null(highly_sim_public_clonotypes_results)) return()
          my_table<-highly_sim_public_clonotypes_results$public_clono
          return(my_table)
        },options = list(scrollX = TRUE))
        
        output$download_highly_sim_Public_clonotypes <- downloadHandler(
          filename = function(){paste0("public_clonotypes",".txt")},
          content = function(file) {
            write.table(highly_sim_public_clonotypes_results$public_clono, file, sep = "\t", row.names = FALSE, col.names = TRUE)
          }) 
        
        output$confirmHighlySimPublicClonotypes <- renderUI({
          h5(msgPublicClono, style = "color: #CD0000;")
        })
        
      })
    })
    ########################################### Repertoires ###########################################   
    #Add new element
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
    
    observeEvent(input$addRepertory, {
      btn <- paste0('selectRepertoires_',input$addRepertory)
      id <- input$addRepertory
      if (id %in% insertedRepertoires) return()
      
      insertedRepertoires<<-c(insertedRepertoires,id)
      addRepertoryFct(id, btn)
    }, ignoreInit = TRUE)
    
    #remove insertedRepertoires
    observeEvent(input$removeRepertory, {
      removeUI(
        ## pass in appropriate div id
        selector = paste0('#', insertedRepertoires[length(insertedRepertoires)])
      )
      insertedRepertoires <<- insertedRepertoires[-length(insertedRepertoires)]
    })
    #################################################################################################### 
    observeEvent(input$Execute_pipeline, {
      if (input$pipeline_Repertoires==F) return()
      
      # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
      withBusyIndicatorServer("Execute_pipeline", {
        
        if (input$pipeline_clonotypes==F) {
          validate(
            #"Please ckeck Clonotypes first!"
          )
          showModal(modalDialog(
            title = "Error Message Repertoires",
            "Please ckeck Clonotypes first!",
            easyClose = TRUE,
            footer = NULL
          ))
          return()
        }
        
        for (i in 1:length(insertedRepertoires)){
          if (input[[paste0("selectRepertoires_",insertedRepertoires[i])]]=="V Gene"){
            allele=F
            gene=used_columns[["Summary"]][3]
          }else if (input[[paste0("selectRepertoires_",insertedRepertoires[i])]]=="V Gene and allele"){
            allele=T
            gene=used_columns[["Summary"]][3]
          }else if (input[[paste0("selectRepertoires_",insertedRepertoires[i])]]=="J Gene"){
            allele=F
            gene=used_columns[["Summary"]][8]
          }else if (input[[paste0("selectRepertoires_",insertedRepertoires[i])]]=="J Gene and allele"){
            allele=T
            gene=used_columns[["Summary"]][8]
          }else if (input[[paste0("selectRepertoires_",insertedRepertoires[i])]]=="D Gene"){
            allele=F 
            gene=used_columns[["Summary"]][11]
          }else{
            allele=T
            gene=used_columns[["Summary"]][11]
          }
          
          if (just_restored_session_Repertoires==F){
            repertories_results[[i]]<<-repertoires(clono$clono_allData,clono$clono_datasets,allele,allele_clonotypes,gene,gene_clonotypes,loaded_datasets,clono$view_specific_clonotype_allData,clono$view_specific_clonotype_datasets)
          }
          
          msgRepertoires[i]<<-repertories_results[[i]]$confirm
          
        }
        
        just_restored_session_Repertoires<<-F
        
        
        output$RepertoiresResultUi <- renderUI({
          lapply(1:length(insertedRepertoires), function(i) {
            mainPanel(
              br(),
              dataTableOutput(paste0("Repertory_tables", i)),
              downloadButton(paste0("downloadRepertory_table",i), "Download"),
              br(),
              br()
            )
          })
        })
        
        output$RepertoiresPiesUi <- renderUI({
          lapply(1:length(insertedRepertoires), function(i) {
            wellPanel(
              br(),
              numericInput("repertories_pies_threshold", "Select %threshold for pies:", 1,  min = 0, max = 100, width="140px"),
              plotlyOutput(paste0("repertories_pies", i)),
              br()
            )
          })
        })
        
        lapply(1:length(insertedRepertoires), function(i) {
          output[[paste0("Repertory_tables", i)]] <- renderDataTable({
            if (input$RepertoiresDataset=="All Data"){
              my_table <- repertories_results[[i]]$Repertoires_allData
            }
            else{
              my_table <- repertories_results[[i]]$Repertoires_datasets[[input$RepertoiresDataset]]
            }
            colnames(my_table) <- c('Gene','N','Freq')
            return(my_table)
          },options = list(scrollX = TRUE))
          
          output[[paste0("downloadRepertory_table", i)]] <- downloadHandler(
            filename = function(){ paste0("Repertoires_",input[[paste0("selectRepertoires_",insertedRepertoires[i])]],"_",input$RepertoiresDataset,".txt")},
            content = function(file) {
              if (input$RepertoiresDataset=="All Data") write.table(repertories_results[[i]]$Repertoires_allData, file, sep = "\t", row.names = FALSE, col.names = TRUE)
              else write.table(repertories_results[[i]]$Repertoires_datasets[[input$RepertoiresDataset]], file, sep = "\t", row.names = FALSE, col.names = TRUE)
              
            })
        })
        
        output$confirmRepertoiresUi <- renderUI({
          lapply(1:length(insertedRepertoires), function(i) {
            uiOutput(paste0("confirmRepertoires", i))
          })
        })
        
        lapply(1:length(insertedRepertoires), function(i) {
          output[[paste0("confirmRepertoires", i)]] <- renderUI({
            h5(msgRepertoires[i], style = "color: #CD0000;")
          })
        })
        
        ######################################## Pie plots ###############################################
        lapply(1:length(insertedRepertoires), function(i) {
          output[[paste0("repertories_pies", i)]] <- renderPlotly({
            if(is.null(input$VisualisationDataset)) return()
            if (input$VisualisationDataset=="All Data"){
              
              #Genes that have percentage<threshold are grouped into one cell
              data=repertories_results[[i]]$Repertoires_allData
              data_filterIn=data %>% filter(data$Freq>input$repertories_pies_threshold)
              data_filterOut=data %>% filter(data$Freq<=input$repertories_pies_threshold)
              data=data_filterIn
              data[(nrow(data)+1),]=c("Other genes",sum(data_filterOut$N),sum(data_filterOut$Freq))
              
              #pie(as.numeric(data$N), labels =round(as.numeric(data$Freq),2), main = paste0(strsplit(strsplit(as.character(imgtfilter_results$allData[[used_columns[["Summary"]][3]]][1])," ")[[1]][2],"V")[[1]][1],input[[paste0("selectRepertoires_",insertedRepertoires[i])]]," ", input$VisualisationDataset),col = rainbow(length(data$N)))
              #legend("topright", data$Gene, cex = 0.8,
              #fill = rainbow(length(data$N)))
              
              p <- plot_ly(data, labels = ~data$Gene, values = ~round(as.numeric(data$Freq),2), type = 'pie') %>%
                layout(title = paste0(strsplit(strsplit(as.character(imgtfilter_results$allData[[used_columns[["Summary"]][3]]][1])," ")[[1]][2],"V")[[1]][1],input[[paste0("selectRepertoires_",insertedRepertoires[i])]]," ", input$VisualisationDataset),
                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
              #p
              return(p)
            }else{
              #Genes that have percentage<threshold are grouped into one cell
              data=repertories_results[[i]]$Repertoires_datasets[[input$VisualisationDataset]]
              data_filterIn=data %>% filter(data$Freq>input$repertories_pies_threshold)
              data_filterOut=data %>% filter(data$Freq<=input$repertories_pies_threshold)
              data=data_filterIn
              data[(nrow(data)+1),]=c("Other genes",sum(data_filterOut$N),sum(data_filterOut$Freq))
              
              #plot
              #pie(as.numeric(data$N), labels = round(as.numeric(data$Freq),2), main = paste0(strsplit(strsplit(as.character(imgtfilter_results$allData[[used_columns[["Summary"]][3]]][1])," ")[[1]][2],"V")[[1]][1],input[[paste0("selectRepertoires_",insertedRepertoires[i])]]," ", input$VisualisationDataset),col = rainbow(length(data$N)))
              #legend("topright", data$Gene, cex = 0.8,
              #       fill = rainbow(length(data$N)))
              
              p <- plot_ly(data, labels = ~data$Gene, values = ~round(as.numeric(data$Freq),2), type = 'pie') %>%
                layout(title = paste0(strsplit(strsplit(as.character(imgtfilter_results$allData[[used_columns[["Summary"]][3]]][1])," ")[[1]][2],"V")[[1]][1],input[[paste0("selectRepertoires_",insertedRepertoires[i])]]," ", input$VisualisationDataset),
                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
              #p
              return(p)
            }
            
          })
          
          
          output[[paste0("downloadRepertory_pie", i)]] <- downloadHandler(
            filename = function(){ paste0("Repertoires_pies",input[[paste0("selectRepertoires_",insertedRepertoires[i])]],"_",input$RepertoiresDataset,".png")},
            content = function(file) {
              #plotly_IMAGE(pie_repertory[[i]][[input$RepertoiresDataset]], format = "png", out_file = file)
              #png(file)
              
            })
        })
        
      })
      return()
    })
    
    
    #################################### Repertoires Highly Similar ####################################   
    observeEvent(input$Execute_pipeline, {
      if (input$pipeline_HighlySim_Repertoires==F) return()
      
      # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
      withBusyIndicatorServer("Execute_pipeline", {
        
        if (input$pipeline_highly_similar_clonotypes==F) {
          validate(
            #"Please ckeck Clonotypes first!"
          )
          showModal(modalDialog(
            title = "Error Message Repertoires",
            "Please ckeck Highly Similar Clonotypes first!",
            easyClose = TRUE,
            footer = NULL
          ))
          return()
        }
        
        for (i in 1:length(insertedRepertoires)){
          if (input[[paste0("selectRepertoires_",insertedRepertoires[i])]]=="V Gene"){
            allele=F
            gene=used_columns[["Summary"]][3]
          }else if (input[[paste0("selectRepertoires_",insertedRepertoires[i])]]=="V Gene and allele"){
            allele=T
            gene=used_columns[["Summary"]][3]
          }else if (input[[paste0("selectRepertoires_",insertedRepertoires[i])]]=="J Gene"){
            allele=F
            gene=used_columns[["Summary"]][8]
          }else if (input[[paste0("selectRepertoires_",insertedRepertoires[i])]]=="J Gene and allele"){
            allele=T
            gene=used_columns[["Summary"]][8]
          }else if (input[[paste0("selectRepertoires_",insertedRepertoires[i])]]=="D Gene"){
            allele=F 
            gene=used_columns[["Summary"]][11]
          }else{
            allele=T
            gene=used_columns[["Summary"]][11]
          }
          
          if (just_restored_session_HighlySim_Repertoires==F){
            HighlySim_repertories_results[[i]]<<-repertoires_highly_similar(highly_sim,highly_sim_datasets,allele,allele_clonotypes,gene,gene_clonotypes,loaded_datasets,clono$view_specific_clonotype_allData,clono$view_specific_clonotype_datasets,input$take_gene_highly_similar)
          }
          
          
          msgHighlySim_Repertoires[i]<<-HighlySim_repertories_results[[i]]$confirm
          
        }
        
        just_restored_session_HighlySim_Repertoires<<-F
        
        output$HighlySim_RepertoiresResultUi <- renderUI({
          lapply(1:length(insertedRepertoires), function(i) {
            mainPanel(
              br(),
              dataTableOutput(paste0("HighlySim_Repertory_tables", i)),
              downloadButton(paste0("downloadHighlySim_Repertory_table",i), "Download"),
              br(),
              br()
            )
          })
        })
        
        output$HighlySim_RepertoiresPiesUi <- renderUI({
          lapply(1:length(insertedRepertoires), function(i) {
            wellPanel(
              br(),
              numericInput("HighlySim_repertories_pies_threshold", "Select %threshold for pies:", 1,  min = 0, max = 100, width="140px"),
              plotlyOutput(paste0("HighlySim_repertories_pies", i)),
              br()
            ) 
          })
        })
        
        lapply(1:length(insertedRepertoires), function(i) {
          output[[paste0("HighlySim_Repertory_tables", i)]] <- renderDataTable({
            if (input$RepertoiresDataset=="All Data"){
              my_table <- HighlySim_repertories_results[[i]]$Repertoires_allData
            }
            else{
              my_table <- HighlySim_repertories_results[[i]]$Repertoires_datasets[[input$RepertoiresDataset]]
            }
            colnames(my_table) <- c('Gene','N','Freq')
            return(my_table)
          },options = list(scrollX = TRUE))
          
          output[[paste0("downloadHighlySim_Repertory_table", i)]] <- downloadHandler(
            filename = function(){ paste0("HighlySim_Repertoires_",input[[paste0("selectRepertoires_",insertedRepertoires[i])]],"_",input$RepertoiresDataset,".txt")},
            content = function(file) {
              if (input$RepertoiresDataset=="All Data") write.table(HighlySim_repertories_results[[i]]$Repertoires_allData, file, sep = "\t", row.names = FALSE, col.names = TRUE)
              else write.table(HighlySim_repertories_results[[i]]$Repertoires_datasets[[input$RepertoiresDataset]], file, sep = "\t", row.names = FALSE, col.names = TRUE)
              
            })
        })
        
        output$confirmHighlySim_RepertoiresUi <- renderUI({
          lapply(1:length(insertedRepertoires), function(i) {
            uiOutput(paste0("HighlySim_confirmRepertoires", i))
          })
        })
        
        lapply(1:length(insertedRepertoires), function(i) {
          output[[paste0("HighlySim_confirmRepertoires", i)]] <- renderUI({
            h5(msgHighlySim_Repertoires[i], style = "color: #CD0000;")
          })
        })
        
        
        ######################################## Pie plots ###############################################
        lapply(1:length(insertedRepertoires), function(i) {
          output[[paste0("HighlySim_repertories_pies", i)]] <- renderPlotly({
            if(is.null(input$VisualisationDataset)) return()
            if (input$VisualisationDataset=="All Data"){
              
              #Genes that have percentage<threshold are grouped into one cell
              data=HighlySim_repertories_results[[i]]$Repertoires_allData
              data_filterIn=data %>% filter(data$Freq>input$HighlySim_repertories_pies_threshold)
              data_filterOut=data %>% filter(data$Freq<=input$HighlySim_repertories_pies_threshold)
              data=data_filterIn
              data[(nrow(data)+1),]=c("Other genes",sum(data_filterOut$N),sum(data_filterOut$Freq))
              
              #pie(as.numeric(data$N), labels =round(as.numeric(data$Freq),2), main = paste0(strsplit(strsplit(as.character(imgtfilter_results$allData[[used_columns[["Summary"]][3]]][1])," ")[[1]][2],"V")[[1]][1],input[[paste0("selectRepertoires_",insertedRepertoires[i])]]," ", input$VisualisationDataset),col = rainbow(length(data$N)))
              #legend("topright", data$Gene, cex = 0.8,
              #fill = rainbow(length(data$N)))
              
              p <- plot_ly(data, labels = ~data$Gene, values = ~round(as.numeric(data$Freq),2), type = 'pie') %>%
                layout(title = paste0(strsplit(strsplit(as.character(imgtfilter_results$allData[[used_columns[["Summary"]][3]]][1])," ")[[1]][2],"V")[[1]][1],input[[paste0("selectRepertoires_",insertedRepertoires[i])]]," ", input$VisualisationDataset),
                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
              #p
              return(p)
            }else{
              #Genes that have percentage<threshold are grouped into one cell
              data=HighlySim_repertories_results[[i]]$Repertoires_datasets[[input$VisualisationDataset]]
              data_filterIn=data %>% filter(data$Freq>input$HighlySim_repertories_pies_threshold)
              data_filterOut=data %>% filter(data$Freq<=input$HighlySim_repertories_pies_threshold)
              data=data_filterIn
              data[(nrow(data)+1),]=c("Other genes",sum(data_filterOut$N),sum(data_filterOut$Freq))
              
              #plot
              #pie(as.numeric(data$N), labels = round(as.numeric(data$Freq),2), main = paste0(strsplit(strsplit(as.character(imgtfilter_results$allData[[used_columns[["Summary"]][3]]][1])," ")[[1]][2],"V")[[1]][1],input[[paste0("selectRepertoires_",insertedRepertoires[i])]]," ", input$VisualisationDataset),col = rainbow(length(data$N)))
              #legend("topright", data$Gene, cex = 0.8,
              #       fill = rainbow(length(data$N)))
              
              p <- plot_ly(data, labels = ~data$Gene, values = ~round(as.numeric(data$Freq),2), type = 'pie') %>%
                layout(title = paste0(strsplit(strsplit(as.character(imgtfilter_results$allData[[used_columns[["Summary"]][3]]][1])," ")[[1]][2],"V")[[1]][1],input[[paste0("selectRepertoires_",insertedRepertoires[i])]]," ", input$VisualisationDataset),
                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
              #p
              return(p)
            }
            
          })
          
          
          output[[paste0("downloadHighlySim_Repertory_pie", i)]] <- downloadHandler(
            filename = function(){ paste0("HighlySim_Repertoires_pies",input[[paste0("selectRepertoires_",insertedRepertoires[i])]],"_",input$RepertoiresDataset,".png")},
            content = function(file) {
              #plotly_IMAGE(pie_repertory[[i]][[input$RepertoiresDataset]], format = "png", out_file = file)
              #png(file)
              
            })
        })
        
      })
      return()
    })
    
    
    ######################################## Repertoires Comparison ###############################################
    observeEvent(input$Execute_pipeline, {
      
      if (input$pipeline_repertoires_comparison==F) return() 
      
      # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
      withBusyIndicatorServer("Execute_pipeline", {
        if (input$pipeline_Repertoires==F) { 
          validate(
            #"Please ckeck Clonotypes and Repertoires first!"
          )
          showModal(modalDialog(
            title = "Error Message Repertoires",
            "Please ckeck Clonotypes and Repertoires first!",
            easyClose = TRUE,
            footer = NULL
          ))
          return()
        }
        
        for (i in 1:length(insertedRepertoires)){
          if (just_restored_session_repertoires_comparison==F){
            repertoires_comparison_results[[i]]<<-repertoires_comparison(repertories_results[[i]]$Repertoires_allData,repertories_results[[i]]$Repertoires_datasets,loaded_datasets,F,i)
            if (input$pipeline_HighlySim_Repertoires==T){
              highly_sim_repertoires_comparison_results[[i]]<<-repertoires_comparison(HighlySim_repertories_results[[i]]$Repertoires_allData,HighlySim_repertories_results[[i]]$Repertoires_datasets,loaded_datasets,T,i)
            }
          }
          
        }
        
        just_restored_session_repertoires_comparison<<-F
        
        if (input$pipeline_HighlySim_Repertoires==T){
          msgRepertoiresComp<<-highly_sim_repertoires_comparison_results[[length(insertedRepertoires)]]$confirm
        }else{
          msgRepertoiresComp<<-repertoires_comparison_results[[length(insertedRepertoires)]]$confirm
        }
        
        output$Repertoires_comparisonResultUi <- renderUI({
          lapply(1:length(insertedRepertoires), function(i) {
            mainPanel(
              br(),
              dataTableOutput(paste0("repertoires_comparison_tables", i)),
              downloadButton(paste0("downloadrepertoires_comparison_table",i), "Download"),
              br(),
              br()
            )
          })
        })
        
        lapply(1:length(insertedRepertoires), function(i) {
          output[[paste0("repertoires_comparison_tables", i)]] <- renderDataTable({
            my_table<-repertoires_comparison_results[[i]]$unique_repertoires
            return(my_table)
          },options = list(scrollX = TRUE))
          
          output[[paste0("downloadrepertoires_comparison_table", i)]] <- downloadHandler(
            filename = function(){ paste0("repertoires_comparison_table",".txt")},
            content = function(file) {
              write.table(repertoires_comparison_results[[i]]$unique_repertoires, file, sep = "\t", row.names = FALSE, col.names = TRUE)
              
            })
        })
        
        ####################### Highly Similar
        output$Highly_sim_Repertoires_comparisonResultUi <- renderUI({
          lapply(1:length(insertedRepertoires), function(i) {
            mainPanel(
              br(),
              dataTableOutput(paste0("Highly_sim_repertoires_comparison_tables", i)),
              downloadButton(paste0("downloadHighly_sim_repertoires_comparison_table",i), "Download"),
              br(),
              br()
            )
          })
        })
        
        lapply(1:length(insertedRepertoires), function(i) {
          output[[paste0("Highly_sim_repertoires_comparison_tables", i)]] <- renderDataTable({
            my_table<-highly_sim_repertoires_comparison_results[[i]]$unique_repertoires
            return(my_table)
          },options = list(scrollX = TRUE))
          
          output[[paste0("downloadHighly_sim_repertoires_comparison_table", i)]] <- downloadHandler(
            filename = function(){ paste0("Highly_sim_repertoires_comparison_table",".txt")},
            content = function(file) {
              write.table(highly_sim_repertoires_comparison_results[[i]]$unique_repertoires, file, sep = "\t", row.names = FALSE, col.names = TRUE)
              
            })
        })
        
        output$confirmRepertoiresComparison<- renderUI({
          h5(msgRepertoiresComp, style = "color: #CD0000;")
        })
        
      })
    })
    
    ########################################### Insert Identity Groups #################################
    observeEvent(input$pipeline_insert_identity_groups, {
      lapply(1:input$N_identity_groups, function(i) {
        output[[paste0("idenity_group_ui_", i)]] <- renderUI({
          fluidRow(
            column(3,
                   numericInput(paste0("Identity_low_group",i),"Low Limit:", 90,  min = 0, max = 100, width="140px")),
            column(3,
                   numericInput(paste0("Identity_high_group",i),"High Limit:", 95,  min = 0, max = 100, width="140px"))
          )
          #mainPanel(
          #div(style="display:inline-block",numericInput(paste0("Identity_low_group",i),"Identity Low:", 90,  min = 0, max = 100, width="140px")),
          #div(style="display:inline-block",numericInput(paste0("Identity_high_group",i),"Identity High:", 95,  min = 0, max = 100, width="140px"))
          #)
        })
      })
      output$insert_identity_groups_ui<- renderUI({
        lapply(1:input$N_identity_groups, function(i) {
          uiOutput(paste0("idenity_group_ui_", i))
        })
      })
      #Make it reactive!!
      
      low=c()
      high=c()
      for (i in 1:input$N_identity_groups){
        low=c(low,input[[paste0("Identity_low_group",i)]])
        high=c(high,input[[paste0("Identity_high_group",i)]])
      }
      label=paste(low,high,sep="-")
      
      identity_groups<<-(data.frame(low=low,high=high,label=label,stringsAsFactors = F))
      
    })
    
    ########################################### Multiple value comparison ##################################################
    #insert Multiple_value_comparison
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
    
    observeEvent(input$insertBtnMultiple_value_comparison, {
      if (!input$pipeline_Multiple_value_comparison) return()
      if ((is.null(input$inputFiles) | is.null(loaded_datasets)) & input$select_load_or_compute_clonotypes != 'load_clonotypes') return()
      btn <- input$insertBtnMultiple_value_comparison
      id <- paste0('MultipleValues_', btn)
      if (id %in% insertedMultiple_value_comparison) return()
      
      columns_for_Multiple_value_comparison<<-c()
      
      if (input$select_load_or_compute_clonotypes != 'load_clonotypes'){
        if ("1_Summary.txt" %in% input$inputFiles){
          columns_for_Multiple_value_comparison<<-c(columns_for_Multiple_value_comparison,"V GENE", "V GENE and allele",
                                                    "J GENE", "J GENE and allele",
                                                    "D GENE", "D GENE and allele",
                                                    "CDR3-IMGT length", "D-REGION reading frame")
        }
        
        if ("6_Junction.txt" %in% input$inputFiles){
          columns_for_Multiple_value_comparison<<-c(columns_for_Multiple_value_comparison, "Molecular mass","pI")
        }
      }else{
        columns_for_Multiple_value_comparison<<-c(columns_for_Multiple_value_comparison,"V GENE", "V GENE and allele",
                                                  "J GENE", "J GENE and allele",
                                                  "D GENE", "D GENE and allele",
                                                  "CDR3-IMGT length", "D-REGION reading frame", "Molecular mass","pI")
      }
      
      
      if (input$cell=="B cell"){
        columns_for_Multiple_value_comparison<<-c(columns_for_Multiple_value_comparison, "V-REGION identity %")
      }
      
      if (just_restored_session_Multiple_value_comparison==F){
        addMultipleValues(id, btn, columns_for_Multiple_value_comparison)
        insertedMultiple_value_comparison <<- c(insertedMultiple_value_comparison,id)
      }

    })
    
    #remove Multiple_value_comparison
    observeEvent(input$removeBtnMultiple_value_comparison, {
      if ((is.null(input$inputFiles) | is.null(loaded_datasets)) & input$select_load_or_compute_clonotypes != 'load_clonotypes') return()
      removeUI(
        ## pass in appropriate div id
        selector = paste0('#', insertedMultiple_value_comparison[length(insertedMultiple_value_comparison)])
      )
      insertedMultiple_value_comparison <<- insertedMultiple_value_comparison[-length(insertedMultiple_value_comparison)]
    })
    
    #################################################################################################### 
    Multiple_value_comparison_input_values<<-c()
    
    observeEvent(input$Execute_pipeline, {
      if (input$pipeline_Multiple_value_comparison==F) return()
      
      # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
      withBusyIndicatorServer("Execute_pipeline", {
        
        #Check if any combination has been selected
        if (length(insertedMultiple_value_comparison)==0) return()
        
        if (input$pipeline_clonotypes==F) {
          validate(
            #"Please ckeck Clonotypes first!"
          )
          showModal(modalDialog(
            title = "Error Message Multiple",
            "Please ckeck Clonotypes first!",
            easyClose = TRUE,
            footer = NULL
          ))
          return()
        }
        
        #display inputs on pipeline tab
        
        #identity groups
        low=c()
        high=c()
        for (i in 1:input$N_identity_groups){
          low=c(low,input[[paste0("Identity_low_group",i)]])
          high=c(high,input[[paste0("Identity_high_group",i)]])
        }
        label=paste(low,high,sep="-")
        identity_groups<<-(data.frame(low=low,high=high,label=label,stringsAsFactors = F))
        
        #Multiple_value_comparison_result<-list()
        
        for (i in 1:length(insertedMultiple_value_comparison)){
          
          if (just_restored_session_Multiple_value_comparison){
            val1=Multiple_value_comparison_input_values[i,1]
            val2=Multiple_value_comparison_input_values[i,2]
            load("rData files/Multiple_value_comparison_result.rData")
          }else{
            val1=input[[paste0("select_MultipleValues_column1_",strsplit(insertedMultiple_value_comparison[i],"_")[[1]][2])]]
            val2=input[[paste0("select_MultipleValues_column2_",strsplit(insertedMultiple_value_comparison[i],"_")[[1]][2])]]
            Multiple_value_comparison_input_values<<-rbind(Multiple_value_comparison_input_values,c(val1,val2))
            if (input$pipeline_highly_similar_clonotypes){
              if (input$select_clono_or_highly_for_Multiple_value_comparison=="initial_clonotypes"){
                highly=F
              }else{
                highly=T
              }
            }else{
              highly=F
            }
            if (highly)
              Multiple_value_comparison_result[[i]]<<-Multiple_value_comparison_highly_similar(highly_sim,highly_sim_datasets,allele_clonotypes,gene_clonotypes,clono$view_specific_clonotype_allData,clono$view_specific_clonotype_datasets,val1,val2,loaded_datasets,identity_groups)
            else
              Multiple_value_comparison_result[[i]]<<-Multiple_value_comparison(clono$clono_allData,clono$clono_datasets,allele_clonotypes,gene_clonotypes,clono$view_specific_clonotype_allData,clono$view_specific_clonotype_datasets,val1,val2,loaded_datasets,identity_groups)
          }
          
          msgMultiple_value_comparison[i]<<-Multiple_value_comparison_result[[i]]$confirm
          
        }
        
        just_restored_session_Multiple_value_comparison<<-F
        
        #Multiple_value_comparison tab
        output$uiMultiple_value_comparisonTables <- renderUI({
          lapply(1:length(insertedMultiple_value_comparison), function(i) {
            mainPanel(
              br(),
              dataTableOutput(paste0("Multiple_value_comparison_tables", i)),
              downloadButton(paste0("downloadMultiple_value_comparison",i), "Download")
              #br(),
              #br(),
              #plotOutput(paste0("Multiple_value_comparison_plot",i))
            )
          })
        })
        
        output$uiMultiple_value_plots <- renderUI({
          lapply(1:length(insertedMultiple_value_comparison), function(i) {
            wellPanel(
              br(),
              plotOutput(paste0("Multiple_value_comparison_plot",i))
            )
          })
        })
        
        lapply(1:length(insertedMultiple_value_comparison), function(i) {
          output[[paste0("Multiple_value_comparison_tables", i)]] <- renderDataTable({
            if (input$Multiple_value_comparisonDataset=="All Data"){
              my_table <- Multiple_value_comparison_result[[i]]$Multiple_value_comparison_allData
            }
            else{
              my_table <- Multiple_value_comparison_result[[i]]$Multiple_value_comparison_datasets[[input$Multiple_value_comparisonDataset]]
            }
            
            val1=input[[paste0("select_MultipleValues_column1_",strsplit(insertedMultiple_value_comparison[i],"_")[[1]][2])]]
            val2=input[[paste0("select_MultipleValues_column2_",strsplit(insertedMultiple_value_comparison[i],"_")[[1]][2])]]
            
            colnames(my_table)<-c(val1,val2,"N","Freq")
            return(my_table)
          },options = list(scrollX = TRUE))
          
          output[[paste0("downloadMultiple_value_comparison", i)]] <- downloadHandler(
            filename = function(){
              val1=input[[paste0("select_MultipleValues_column1_",strsplit(insertedMultiple_value_comparison[i],"_")[[1]][2])]]
              val2=input[[paste0("select_MultipleValues_column2_",strsplit(insertedMultiple_value_comparison[i],"_")[[1]][2])]]
              paste0("Multiple_value_comparison_",val1,"+",val2,"_",input$Multiple_value_comparisonDataset,".txt")},
            content = function(file) {
              if (input$Multiple_value_comparisonDataset=="All Data") write.table(Multiple_value_comparison_result[[i]]$Multiple_value_comparison_allData, file, sep = "\t", row.names = FALSE, col.names = TRUE)
              else write.table(Multiple_value_comparison_result[[i]]$Multiple_value_comparison_datasets[[input$Multiple_value_comparisonDataset]], file, sep = "\t", row.names = FALSE, col.names = TRUE)
            })
          
          output[[paste0("Multiple_value_comparison_plot", i)]] <- renderPlot({
            if(is.null(input$VisualisationDataset)) return()
            val1=input[[paste0("select_MultipleValues_column1_",strsplit(insertedMultiple_value_comparison[i],"_")[[1]][2])]]
            val2=input[[paste0("select_MultipleValues_column2_",strsplit(insertedMultiple_value_comparison[i],"_")[[1]][2])]]
            #Multiple_value_comparison_output_allData=as.data.frame(Multiple_value_comparison_output_allData)
            colnames(Multiple_value_comparison_result[[i]]$Multiple_value_comparison_allData)=c(val1,val2,"N","Freq")
            if ((val1 %in% c("CDR3-IMGT length", "V-REGION identity %","Molecular mass","pI")) && (val2 %in% c("CDR3-IMGT length", "V-REGION identity %","Molecular mass","pI"))){
              if (input$VisualisationDataset=="All Data"){
                plot(as.numeric(Multiple_value_comparison_result[[i]]$Multiple_value_comparison_allData[[val1]]),as.numeric(Multiple_value_comparison_result[[i]]$Multiple_value_comparison_allData[[val2]]),xlab=val1,ylab=val2) 
              }else{
                colnames(Multiple_value_comparison_result[[i]]$Multiple_value_comparison_datasets[[input$VisualisationDataset]])=c(val1,val2,"N","Freq")
                plot(as.numeric(Multiple_value_comparison_result[[i]]$Multiple_value_comparison_datasets[[input$VisualisationDataset]][[val1]]),as.numeric(Multiple_value_comparison_result[[i]]$Multiple_value_comparison_datasets[[input$VisualisationDataset]][[val2]]),xlab=val1,ylab=val2) 
              }
            }
            
          })
          
        })
        
        output$confirmMultiple_value_comparison <- renderUI({
          lapply(1:length(insertedMultiple_value_comparison), function(i) {
            uiOutput(paste0("confirmMultiple_value_comparison", i))
          })
        })
        
        lapply(1:length(insertedMultiple_value_comparison), function(i) {
          output[[paste0("confirmMultiple_value_comparison", i)]] <- renderUI({
            h5(msgMultiple_value_comparison[i], style = "color: #CD0000;")
          })
        })
        
        if (save_lists_for_bookmark)
          save(Multiple_value_comparison_result, file=paste0(output_folder,"/Multiple_value_comparison_result.rdata"))
        
      })
      
      
      return()
      
    })
    
    ########################################### Freq Tables ############################################    
    
    ########################################### Logo plots #############################################
    observeEvent(input$Execute_pipeline_2nd_part, {
      if (input$pipeline_logo==F) return()
      #if (msgFreqTables=="") return()
      
      # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
      withBusyIndicatorServer("Execute_pipeline_2nd_part", {
        
        if (input$select_topN_clonotypes_for_freqTable!="all_clonotypes"){
          if (input$select_topN_clonotypes_for_freqTable=="topN_clonotypes_for_alignment"){
            FtopN=T
            Fthr=F
          } 
          else {
            FtopN=F
            Fthr=T
          }
          
          if (input$pipeline_clonotypes==F) {
            validate(
              #"Please ckeck Clonotypes first!"
            )
            showModal(modalDialog(
              title = "Error Message Repertoires",
              "Please ckeck Clonotypes first!",
              easyClose = TRUE,
              footer = NULL
            ))
            return()
          }
        }else {
          FtopN=F
          Fthr=F
        }
        
        if (FtopN){
          if (input$pipeline_highly_similar_clonotypes==F){
            clono_allDataTopN=clono$clono_allData[1:input$topNFreqTable,] 
            if (is.null(clono$clono_allData)) return()
            clono_datasetsTopN<-list()
            for (i in 1:length(loaded_datasets)){
              clono_datasetsTopN[[loaded_datasets[i]]]=clono$clono_datasets[[loaded_datasets[i]]][1:input$topNFreqTable,]
            }
          }else{
            clono_allDataTopN=highly_sim[1:input$topNFreqTable,] 
            if (is.null(highly_sim)) return()
            clono_datasetsTopN<-list()
            for (i in 1:length(loaded_datasets)){
              clono_datasetsTopN[[loaded_datasets[i]]]=highly_sim_datasets[[loaded_datasets[i]]][1:input$topNFreqTable,]
            }
          }
          
          
        }
        
        if (Fthr){
          if (input$pipeline_highly_similar_clonotypes==F){
            clono_allDataTopN=clono$clono_allData %>% filter(Freq>input$thrClonoLogos)
            if (is.null(clono$clono_allData)) return()
            clono_datasetsTopN<-list()
            for (i in 1:length(loaded_datasets)){
              clono_datasetsTopN[[loaded_datasets[i]]]=clono$clono_datasets[[loaded_datasets[i]]] %>% filter(Freq>input$thrClonoLogos)
            }
          }else{
            clono_allDataTopN=highly_sim %>% filter(Freq>input$thrClonoLogos)
            if (is.null(highly_sim)) return()
            clono_datasetsTopN<-list()
            for (i in 1:length(loaded_datasets)){
              clono_datasetsTopN[[loaded_datasets[i]]]=highly_sim_datasets[[loaded_datasets[i]]] %>% filter(Freq>input$thrClonoLogos)
            }
          }
          
          
        }
        
        if (input$select_clonotypes_for_logo){
          FclonoLogoSeperately<<-T
          num_of_clusters = length(strsplit(input$clonotypes_for_logo,",")[[1]])
          cl_ids_logos<<-as.numeric(strsplit(input$clonotypes_for_logo,",")[[1]])
        }
        
        if (just_restored_session_freqTables==F){
          frequenciesTables_results<<-createFrequencyTableCDR3(input$regionFreqTable,imgtfilter_results$allData,loaded_datasets,input$regionLengthFreq,(FtopN || Fthr),clono_allDataTopN,clono_datasetsTopN,gene_clonotypes,junction_clonotypes,allele_clonotypes)
          if (FclonoLogoSeperately){
            if (input$pipeline_highly_similar_clonotypes==F){
              for (cl in 1:length(cl_ids_logos)){
                clono_datasets_cl<-list()
                for (i in 1:length(loaded_datasets)){
                  clono_datasets_cl[[loaded_datasets[i]]]=clono$clono_datasets[[loaded_datasets[i]]][cl_ids_logos[cl],]
                }
                frequenciesTables_results_cl[[cl]]<<-createFrequencyTableCDR3(input$regionFreqTable,imgtfilter_results$allData,loaded_datasets,input$regionLengthFreq,FclonoLogoSeperately,clono$clono_allData[cl_ids_logos[cl],],clono_datasets_cl,gene_clonotypes,junction_clonotypes,allele_clonotypes)
              }
            }else{
              for (cl in 1:length(cl_ids_logos)){
                clono_datasets_cl<-list()
                for (i in 1:length(loaded_datasets)){
                  clono_datasets_cl[[loaded_datasets[i]]]=highly_sim_datasets[[loaded_datasets[i]]][cl_ids_logos[cl],]
                }
                frequenciesTables_results_cl[[cl]]<<-createFrequencyTableCDR3(input$regionFreqTable,imgtfilter_results$allData,loaded_datasets,input$regionLengthFreq,FclonoLogoSeperately,highly_sim[cl_ids_logos[cl],],clono_datasets_cl,gene_clonotypes,junction_clonotypes,allele_clonotypes)
              }
            }
            
            
          }
        }
        
        just_restored_session_freqTables<<-F
        
        freqTables_datasets<<-frequenciesTables_results$table_freq_datasets
        
        msgFreqTables<<-frequenciesTables_results$confirm
        
        if (length(frequenciesTables_results$table_count)==0) return()
        
        output$countCDR3Table <- renderDataTable({
          if(is.null(input$freqTableDataset)) return()
          if (input$freqTableDataset=="All Data"){
            my_table <- frequenciesTables_results$table_count
            #my_table[,2:ncol(my_table)]=my_table
            #my_table[,1]=row.names(my_table)
            row.names(my_table)=c()
            colnames(my_table)=1:(ncol(frequenciesTables_results$table_count)-1)
          }
          else{
            my_table <- frequenciesTables_results$table_count_datasets[[input$freqTableDataset]]
            #my_table[,2:ncol(my_table)]=my_table
            #my_table[,1]=row.names(my_table)
            row.names(my_table)=c()
            colnames(my_table)=1:(ncol(frequenciesTables_results$table_count)-1)
          }
          
          if (input$regionFreqTable=="CDR3"){
            if ((ncol(my_table)-1)==13) a=105:117
            else if ((ncol(my_table)-1)==12) a=c(105:110,112:117)
            else if ((ncol(my_table)-1)==11) a=c(105:110,113:117)
            else if ((ncol(my_table)-1)==10) a=c(105:109,113:117)
            else if ((ncol(my_table)-1)==9) a=c(105:109,114:117)
            else if ((ncol(my_table)-1)==8) a=c(105:108,114:117)
            else if ((ncol(my_table)-1)==7) a=c(105:108,115:117)
            else if ((ncol(my_table)-1)==6) a=c(105:107,115:117)
            else if ((ncol(my_table)-1)==5) a=c(105:107,116:117)
            
            colnames(my_table) <- c('AA',a)
          }
          
          colnames(my_table)[1]<-'AA'
          
          return(my_table)
          
        }, options = list(pageLength =20,scrollX = TRUE))
        
        output$downloadcountCDR3Table <- downloadHandler(
          filename = function(){paste0("countCDR3Table_",input$freqTableDataset,".txt")},
          content = function(file) {
            if (input$clonotypesDataset=="All Data") write.table(frequenciesTables_results$table_count, file, sep = "\t", row.names = FALSE, col.names = TRUE)
            else write.table(frequenciesTables_results$table_count_datasets[[input$freqTableDataset]], file, sep = "\t", row.names = FALSE, col.names = TRUE)
          })
        
        output$frequencyCDR3Table <- renderDataTable({
          if(is.null(input$freqTableDataset)) return()
          if (input$freqTableDataset=="All Data"){
            my_table <- frequenciesTables_results$table_freq
            #my_table[,2:ncol(my_table)]=my_table
            #my_table[,1]=row.names(my_table)
            row.names(my_table)=c()
            colnames(my_table)=1:(ncol(frequenciesTables_results$table_count)-1)
          }
          else{
            my_table <- frequenciesTables_results$table_freq_datasets[[input$freqTableDataset]]
            #my_table[,2:ncol(my_table)]=my_table
            #my_table[,1]=row.names(my_table)
            row.names(my_table)=c()
            colnames(my_table)=1:(ncol(frequenciesTables_results$table_count)-1)
          }
          
          if (input$regionFreqTable=="CDR3"){
            if ((ncol(my_table)-1)==13) a=105:117
            else if ((ncol(my_table)-1)==12) a=c(105:110,112:117)
            else if ((ncol(my_table)-1)==11) a=c(105:110,113:117)
            else if ((ncol(my_table)-1)==10) a=c(105:109,113:117)
            else if ((ncol(my_table)-1)==9) a=c(105:109,114:117)
            else if ((ncol(my_table)-1)==8) a=c(105:108,114:117)
            else if ((ncol(my_table)-1)==7) a=c(105:108,115:117)
            else if ((ncol(my_table)-1)==6) a=c(105:107,115:117)
            else if ((ncol(my_table)-1)==5) a=c(105:107,116:117)
            
            colnames(my_table) <- c('AA',a)
          }
          
          return(my_table)
          
        }, options = list(pageLength =20,scrollX = TRUE))
        
        output$downloadfrequencyCDR3Table <- downloadHandler(
          filename = function(){paste0("countCDR3Table_",input$freqTableDataset,".txt")},
          content = function(file) {
            if (input$clonotypesDataset=="All Data") write.table(frequenciesTables_results$table_freq, file, sep = "\t", row.names = FALSE, col.names = TRUE)
            else write.table(frequenciesTables_results$table_freq_datasets[[input$freqTableDataset]], file, sep = "\t", row.names = FALSE, col.names = TRUE)
          })
        
        output$confirmFrequenciesTables<- renderUI({
          h5(msgFreqTables, style = "color: #CD0000;")
        })
        
        if (length(frequenciesTables_results$table_count)==0) return()
        
        if (input$regionFreqTable=="CDR3"){
          if ((ncol(frequenciesTables_results$table_count)-1)==13) a=105:117
          else if ((ncol(frequenciesTables_results$table_count)-1)==12) a=c(105:110,112:117)
          else if ((ncol(frequenciesTables_results$table_count)-1)==11) a=c(105:110,113:117)
          else if ((ncol(frequenciesTables_results$table_count)-1)==10) a=c(105:109,113:117)
          else if ((ncol(frequenciesTables_results$table_count)-1)==9) a=c(105:109,114:117)
          else if ((ncol(frequenciesTables_results$table_count)-1)==8) a=c(105:108,114:117)
          else if ((ncol(frequenciesTables_results$table_count)-1)==7) a=c(105:108,115:117)
          else if ((ncol(frequenciesTables_results$table_count)-1)==6) a=c(105:107,115:117)
          else if ((ncol(frequenciesTables_results$table_count)-1)==5) a=c(105:107,116:117)
          
          colnames(frequenciesTables_results$table_count) <- c('AA',a)
          
        }
        
        for (i in 1:length(loaded_datasets)){
          if (!is.null(frequenciesTables_results$table_count_datasets[[loaded_datasets[i]]])){
            if (input$regionFreqTable=="CDR3"){
              colnames(frequenciesTables_results$table_count_datasets[[loaded_datasets[i]]]) <- c('AA',a)
            }
            frequenciesTables_results$table_count_datasets[[loaded_datasets[i]]]=frequenciesTables_results$table_count_datasets[[loaded_datasets[i]]][,2:ncol(frequenciesTables_results$table_count_datasets[[loaded_datasets[i]]])]
            
          }
        }
        
        region_id=0
        
        ################### Separately #######################
        if (FclonoLogoSeperately){
          output$uiCountCDR3Table_cl <- renderUI({
            lapply(1:length(cl_ids_logos), function(i) {
              mainPanel(
                br(),
                br(),
                dataTableOutput(paste0("CountCDR3Table_cl", i)),
                br(),
                br(),
                downloadButton(paste0("downloadCountCDR3Table_cl",i), "Download"),
                br(),
                br(),
                dataTableOutput(paste0("FreqCDR3Table_cl", i)),
                br(),
                br(),
                downloadButton(paste0("downloadFreqCDR3Table_cl",i), "Download")
              )
            })
          })
          
          lapply(1:length(cl_ids_logos), function(i) {
            output[[paste0("CountCDR3Table_cl", i)]] <- renderDataTable({
              if(is.null(input$freqTableDataset)) return()
              if (input$freqTableDataset=="All Data"){
                my_table <- frequenciesTables_results_cl[[i]]$table_count
              }
              else{
                my_table <- frequenciesTables_results_cl[[i]]$table_count_datasets[[input$freqTableDataset]]
              }
              
              return(my_table)
            },options = list(scrollX = TRUE))
            
            output[[paste0("downloadCountCDR3Table_cl", i)]] <- downloadHandler(
              filename = function(){paste0("CountCDR3Table_cl",cl_ids_logos[i],input$freqTableDataset,".txt")},
              content = function(file) {
                if (input$freqTableDataset=="All Data") write.table(frequenciesTables_results_cl[[i]]$table_count, file, sep = "\t", row.names = FALSE, col.names = TRUE)
                else write.table(frequenciesTables_results_cl[[i]]$table_count_datasets[[input$freqTableDataset]], file, sep = "\t", row.names = FALSE, col.names = TRUE)
              })
            
            output[[paste0("FreqCDR3Table_cl", i)]] <- renderDataTable({
              if(is.null(input$freqTableDataset)) return()
              if (input$freqTableDataset=="All Data"){
                my_table <- frequenciesTables_results_cl[[i]]$table_freq
                
              }
              else{
                my_table <- frequenciesTables_results_cl[[i]]$table_freq_datasets[[input$freqTableDataset]]
                
              }
              
              return(my_table)
              
            },options = list(scrollX = TRUE))
            
            output[[paste0("downloadFreqCDR3Table_cl", i)]] <- downloadHandler(
              filename = function(){paste0("FreqCDR3Table_cl",cl_ids_logos[i],input$freqTableDataset,".txt")},
              content = function(file) {
                if (input$freqTableDataset=="All Data") write.table(frequenciesTables_results_cl[[i]]$table_freq, file, sep = "\t", row.names = FALSE, col.names = TRUE)
                else write.table(frequenciesTables_results_cl[[i]]$table_freq_datasets[[input$freqTableDataset]], file, sep = "\t", row.names = FALSE, col.names = TRUE)
              })
            
          })
        }
        
        #################################### Logo plots ##############################
        if (just_restored_session_logo==F){
          if (input$regionFreqTable=="CDR3"){
            logo_result<<-createLogo(frequenciesTables_results$table_count[,2:ncol(frequenciesTables_results$table_count)],frequenciesTables_results$table_count_datasets,loaded_datasets)
          }else{
            logo_result<<-createLogo(frequenciesTables_results$table_count[,2:ncol(frequenciesTables_results$table_count)],frequenciesTables_results$table_count_datasets,loaded_datasets)
            for (regions in region_names){
              region_id=region_id+1
              for (i in 1:length(loaded_datasets)){
                countTables_per_region_datasets[[loaded_datasets[i]]]=frequenciesTables_results$table_count_datasets[[loaded_datasets[i]]][,(index_1[region_id]):(index_2[region_id])]
              }
              logo_per_region[[regions]]<<-createLogo(frequenciesTables_results$table_count[,(index_1[region_id]+1):(index_2[region_id]+1)],countTables_per_region_datasets,loaded_datasets)
            }
          }
          
          if (FclonoLogoSeperately){
            for (cl in 1:length(cl_ids_logos)){
              if (input$regionFreqTable=="CDR3"){
                logo_result_cl[[cl]]<<-createLogo(frequenciesTables_results_cl[[cl]]$table_count[,2:ncol(frequenciesTables_results_cl[[cl]]$table_count)],frequenciesTables_results_cl[[cl]]$table_count_datasets,loaded_datasets)
              }else{
                logo_per_region_cl[[cl]]<<-list()
                logo_result_cl[[cl]]<<-createLogo(frequenciesTables_results_cl[[cl]]$table_count[,2:ncol(frequenciesTables_results_cl[[cl]]$table_count)],frequenciesTables_results_cl[[cl]]$table_count_datasets,loaded_datasets)
                region_id=0
                for (regions in region_names){
                  region_id=region_id+1
                  for (i in 1:length(loaded_datasets)){
                    countTables_per_region_datasets[[loaded_datasets[i]]]=frequenciesTables_results_cl[[cl]]$table_count_datasets[[loaded_datasets[i]]][,(index_1[region_id]):(index_2[region_id])]
                  }
                  logo_per_region_cl[[cl]][[regions]]<<-createLogo(frequenciesTables_results_cl[[cl]]$table_count[,(index_1[region_id]+1):(index_2[region_id]+1)],countTables_per_region_datasets,loaded_datasets)
                }
              }
            }
          }
        }
        
        just_restored_session_logo=F
        motif_datasets<<-logo_result$motif_datasets
        motif_all<<-logo_result$motif_all
        msgLogo<<-logo_result$confirm
        
        output$logo<-renderPlot({
          if(is.null(input$LogoDataset)) return()
          
          if (input$select_region_logo=="All V region" || input$regionFreqTable=='CDR3'){
            if (input$LogoDataset=="All Data"){
              logo_plot<<-plot(motif_all,ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
            }else{
              logo_plot<<-plot(motif_datasets[[input$LogoDataset]],ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
            }
            table_count=frequenciesTables_results$table_count[,2:ncol(frequenciesTables_results$table_count)]
            index1=1
            index2=ncol(table_count)
            if (input$regionFreqTable=="CDR3"){
              axis(1,at=seq((1/(2*(ncol(table_count[,index1:index2])-1))),1-1/(2*(ncol(table_count[,index1:index2])-1)),by=(1-1/(ncol(table_count[,index1:index2])-1))/(ncol(table_count[,index1:index2])-1)),colnames(table_count)) #paste0(index1:index2,":",colnames(table_count[,index1:index2]))
              
            }else{
              axis(1,at=seq((1/(2*(ncol(table_count[,index1:index2])-1))),1-1/(2*(ncol(table_count[,index1:index2])-1)),by=(1-1/(ncol(table_count[,index1:index2])-1))/(ncol(table_count[,index1:index2])-1)),index1:index2) #paste0(index1:index2,":",colnames(table_count[,index1:index2]))
            }
            axis(2,at=seq(0,1,by=1/5))
            
          }else{
            if (input$LogoDataset=="All Data"){
              logo_plot<<-plot(logo_per_region[[input$select_region_logo]]$motif_all,ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
            }else{
              logo_plot<<-plot(logo_per_region[[input$select_region_logo]]$motif_datasets[[input$LogoDataset]],ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
            }
            table_count=frequenciesTables_results$table_count[,2:ncol(frequenciesTables_results$table_count)]
            
            region_names<-c("FR1-IMGT","CDR1-IMGT","FR2-IMGT","CDR2-IMGT","FR3-IMGT","CDR3-IMGT")
            index_1<-c(1,27,39,56,66,105)
            index_2<-c(26,38,55,65,104,114)
            
            region_id=0
            for (regions in region_names){
              region_id=region_id+1
              if (regions==input$select_region_logo){
                r=region_id
                break
              }
            }
            
            i1=index_1[r]
            i2=index_2[r]
            
            axis(1,at=seq((1/(2*(ncol(table_count[,i1:i2])-1))),1-1/(2*(ncol(table_count[,i1:i2])-1)),by=(1-1/(ncol(table_count[,i1:i2])-1))/(ncol(table_count[,i1:i2])-1)),i1:i2) #paste0(i1:i2,":",colnames(table_count[,i1:i2])
            axis(2,at=seq(0,1,by=1/5))
          }
          
        })
        
        output$logo_visualisation<-renderPlot({
          if(is.null(input$VisualisationDataset)) return()
          
          if (input$select_region_logo=="All V region" || input$select_region_logo=='CDR3'){
            if (input$VisualisationDataset=="All Data"){
              # Create custom colour scheme
              #cs1 = make_col_scheme(chars=c("F","W","A","I","L","V","M","C","P","G","Y","T","S","H","K","R","E","D","Q","N"),
              #cols=c("#1E90FF", "#BA55D3", "#0000FF", "#0000FF", "#0000FF", "#0000FF", "#C6E2FF", "#C6E2FF", "#FFD700", "#00EE00", "#C1FFC1", "#54FF9F", "#54FF9F", "#FF0000", "#FF0000", "#FF0000", "#FFD700", "#FFD700", "#ED9121", "#ED9121"))
              #ggseqlogo(frequenciesTables_results$region_with_specific_length, method = "prob", col_scheme=cs1)
              #write.table(frequenciesTables_results$region_with_specific_length,"fre.txt",sep="\t")
              logo_plot<<-plot(motif_all,ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
            }else{
              # Create custom colour scheme
              #cs1 = make_col_scheme(chars=c("F","W","A","I","L","V","M","C","P","G","Y","T","S","H","K","R","E","D","Q","N"),
              #cols=c("#1E90FF", "#BA55D3", "#0000FF", "#0000FF", "#0000FF", "#0000FF", "#C6E2FF", "#C6E2FF", "#FFD700", "#00EE00", "#C1FFC1", "#54FF9F", "#54FF9F", "#FF0000", "#FF0000", "#FF0000", "#FFD700", "#FFD700", "#ED9121", "#ED9121"))
              #ggseqlogo(frequenciesTables_results$region_with_specific_length_dataset[[input$VisualisationDataset]], method = "prob", col_scheme=cs1)
              logo_plot<<-plot(motif_datasets[[input$VisualisationDataset]],ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
            }
            table_count=frequenciesTables_results$table_count[,2:ncol(frequenciesTables_results$table_count)]
            index1=1
            index2=ncol(table_count)
            if (input$regionFreqTable=="CDR3"){
              axis(1,at=seq((1/(2*(ncol(table_count[,index1:index2])-1))),1-1/(2*(ncol(table_count[,index1:index2])-1)),by=(1-1/(ncol(table_count[,index1:index2])-1))/(ncol(table_count[,index1:index2])-1)),colnames(table_count)) #paste0(index1:index2,":",colnames(table_count[,index1:index2]))
              
            }else{
              axis(1,at=seq((1/(2*(ncol(table_count[,index1:index2])-1))),1-1/(2*(ncol(table_count[,index1:index2])-1)),by=(1-1/(ncol(table_count[,index1:index2])-1))/(ncol(table_count[,index1:index2])-1)),index1:index2) #paste0(index1:index2,":",colnames(table_count[,index1:index2]))
            }
            axis(2,at=seq(0,1,by=1/5))
            
          }else{
            if (input$VisualisationDataset=="All Data"){
              logo_plot<<-plot(logo_per_region[[input$select_region_logo]]$motif_all,ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
            }else{
              logo_plot<<-plot(logo_per_region[[input$select_region_logo]]$motif_datasets[[input$VisualisationDataset]],ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
            }
            table_count=frequenciesTables_results$table_count[,2:ncol(frequenciesTables_results$table_count)]
            
            region_names<-c("FR1-IMGT","CDR1-IMGT","FR2-IMGT","CDR2-IMGT","FR3-IMGT","CDR3-IMGT")
            index_1<-c(1,27,39,56,66,105)
            index_2<-c(26,38,55,65,104,114)
            
            region_id=0
            for (regions in region_names){
              region_id=region_id+1
              if (regions==input$select_region_logo){
                r=region_id
                break
              }
            }
            
            i1=index_1[r]
            i2=index_2[r]
            
            axis(1,at=seq((1/(2*(ncol(table_count[,i1:i2])-1))),1-1/(2*(ncol(table_count[,i1:i2])-1)),by=(1-1/(ncol(table_count[,i1:i2])-1))/(ncol(table_count[,i1:i2])-1)),i1:i2) #paste0(i1:i2,":",colnames(table_count[,i1:i2])
            axis(2,at=seq(0,1,by=1/5))
          }
          
        })
        
        plotLogo=function(){
          
        }
        
        output$downloadLogo <- downloadHandler(
          filename = function(){paste0("logo_",input$select_region_logo,"_",input$LogoDataset,".png")},
          content = function(file) {
            png(file,width=1000, height=550)
            #dev.print(png, file,width=1000, height=550)
            #multiplot(logo_plot)
            if (input$select_region_logo=="All V region" || input$regionFreqTable=='CDR3'){
              if (input$LogoDataset=="All Data"){
                logo_plot<<-plot(motif_all,ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
              }else{
                logo_plot<<-plot(motif_datasets[[input$LogoDataset]],ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
              }
              table_count=frequenciesTables_results$table_count[,2:ncol(frequenciesTables_results$table_count)]
              index1=1
              index2=ncol(table_count)
              if (input$regionFreqTable=="CDR3"){
                axis(1,at=seq((1/(2*(ncol(table_count[,index1:index2])-1))),1-1/(2*(ncol(table_count[,index1:index2])-1)),by=(1-1/(ncol(table_count[,index1:index2])-1))/(ncol(table_count[,index1:index2])-1)),colnames(table_count)) #paste0(index1:index2,":",colnames(table_count[,index1:index2]))
                
              }else{
                axis(1,at=seq((1/(2*(ncol(table_count[,index1:index2])-1))),1-1/(2*(ncol(table_count[,index1:index2])-1)),by=(1-1/(ncol(table_count[,index1:index2])-1))/(ncol(table_count[,index1:index2])-1)),index1:index2) #paste0(index1:index2,":",colnames(table_count[,index1:index2]))
              }
              axis(2,at=seq(0,1,by=1/5))
              
            }else{
              if (input$LogoDataset=="All Data"){
                logo_plot<<-plot(logo_per_region[[input$select_region_logo]]$motif_all,ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
              }else{
                logo_plot<<-plot(logo_per_region[[input$select_region_logo]]$motif_datasets[[input$LogoDataset]],ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
              }
              table_count=frequenciesTables_results$table_count[,2:ncol(frequenciesTables_results$table_count)]
              
              region_names<-c("FR1-IMGT","CDR1-IMGT","FR2-IMGT","CDR2-IMGT","FR3-IMGT","CDR3-IMGT")
              index_1<-c(1,27,39,56,66,105)
              index_2<-c(26,38,55,65,104,114)
              
              region_id=0
              for (regions in region_names){
                region_id=region_id+1
                if (regions==input$select_region_logo){
                  r=region_id
                  break
                }
              }
              
              i1=index_1[r]
              i2=index_2[r]
              
              axis(1,at=seq((1/(2*(ncol(table_count[,i1:i2])-1))),1-1/(2*(ncol(table_count[,i1:i2])-1)),by=(1-1/(ncol(table_count[,i1:i2])-1))/(ncol(table_count[,i1:i2])-1)),i1:i2) #paste0(i1:i2,":",colnames(table_count[,i1:i2])
              axis(2,at=seq(0,1,by=1/5))
            }
            
            dev.off()
          }) 
        
        output$confirmLogo<- renderUI({
          h5(msgLogo, style = "color: #CD0000;")
        })
        
        ################### Separately #######################
        if (FclonoLogoSeperately){
          output$uiLogos_cl <- renderUI({
            lapply(1:length(cl_ids_logos), function(i) {
              mainPanel(
                br(),
                br(),
                plotOutput(paste0("logo_cl", i)),
                br(),
                br(),
                downloadButton(paste0("downloadLogo_cl",i), "Download"),
                br(),
                br()#,
                #dataTableOutput(paste0("FreqCDR3Table_cl", i)),
                #br(),
                #br(),
                #downloadButton(paste0("downloadFreqCDR3Table_cl",i), "Download")
              )
            })
          })
          
          lapply(1:length(cl_ids_logos), function(i) {
            output[[paste0("logo_cl", i)]] <- renderPlot({
              
              if(is.null(input$LogoDataset)) return()
              
              if (input$select_region_logo=="All V region" || input$regionFreqTable=='CDR3'){
                if (input$LogoDataset=="All Data"){
                  logo_plot<<-plot(logo_result_cl[[i]]$motif_all,ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
                }else{
                  logo_plot<<-plot(logo_result_cl[[i]]$motif_datasets[[input$LogoDataset]],ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
                }
                table_count=frequenciesTables_results_cl[[i]]$table_count[,2:ncol(frequenciesTables_results_cl[[i]]$table_count)]
                index1=1
                index2=ncol(table_count)
                if (input$regionFreqTable=="CDR3"){
                  axis(1,at=seq((1/(2*(ncol(table_count[,index1:index2])-1))),1-1/(2*(ncol(table_count[,index1:index2])-1)),by=(1-1/(ncol(table_count[,index1:index2])-1))/(ncol(table_count[,index1:index2])-1)),colnames(table_count)) #paste0(index1:index2,":",colnames(table_count[,index1:index2]))
                  
                }else{
                  axis(1,at=seq((1/(2*(ncol(table_count[,index1:index2])-1))),1-1/(2*(ncol(table_count[,index1:index2])-1)),by=(1-1/(ncol(table_count[,index1:index2])-1))/(ncol(table_count[,index1:index2])-1)),index1:index2) #paste0(index1:index2,":",colnames(table_count[,index1:index2]))
                }
                axis(2,at=seq(0,1,by=1/5))
                
              }else{
                if (input$LogoDataset=="All Data"){
                  logo_plot<<-plot(logo_per_region_cl[[i]][[input$select_region_logo]]$motif_all,ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
                }else{
                  logo_plot<<-plot(logo_per_region_cl[[i]][[input$select_region_logo]]$motif_datasets[[input$LogoDataset]],ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
                }
                table_count=frequenciesTables_results_cl[[i]]$table_count[,2:ncol(frequenciesTables_results_cl[[i]]$table_count)]
                
                region_names<-c("FR1-IMGT","CDR1-IMGT","FR2-IMGT","CDR2-IMGT","FR3-IMGT","CDR3-IMGT")
                index_1<-c(1,27,39,56,66,105)
                index_2<-c(26,38,55,65,104,114)
                
                region_id=0
                for (regions in region_names){
                  region_id=region_id+1
                  if (regions==input$select_region_logo){
                    r=region_id
                    break
                  }
                }
                
                i1=index_1[r]
                i2=index_2[r]
                
                axis(1,at=seq((1/(2*(ncol(table_count[,i1:i2])-1))),1-1/(2*(ncol(table_count[,i1:i2])-1)),by=(1-1/(ncol(table_count[,i1:i2])-1))/(ncol(table_count[,i1:i2])-1)),i1:i2) #paste0(i1:i2,":",colnames(table_count[,i1:i2])
                axis(2,at=seq(0,1,by=1/5))
              }
              
            })
            
            output[[paste0("downloadLogo_cl", i)]] <- downloadHandler(
              filename = function(){paste0("Logo_cl",cl_ids_logos[i],input$freqTableDataset,".png")},
              content = function(file) {
                png(file,width=1000, height=550)
                if (input$select_region_logo=="All V region" || input$regionFreqTable=='CDR3'){
                  if (input$LogoDataset=="All Data"){
                    logo_plot<<-plot(logo_result_cl[[i]]$motif_all,ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
                  }else{
                    logo_plot<<-plot(logo_result_cl[[i]]$motif_datasets[[input$LogoDataset]],ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
                  }
                  table_count=frequenciesTables_results_cl[[i]]$table_count[,2:ncol(frequenciesTables_results_cl[[i]]$table_count)]
                  index1=1
                  index2=ncol(table_count)
                  if (input$regionFreqTable=="CDR3"){
                    axis(1,at=seq((1/(2*(ncol(table_count[,index1:index2])-1))),1-1/(2*(ncol(table_count[,index1:index2])-1)),by=(1-1/(ncol(table_count[,index1:index2])-1))/(ncol(table_count[,index1:index2])-1)),colnames(table_count)) #paste0(index1:index2,":",colnames(table_count[,index1:index2]))
                    
                  }else{
                    axis(1,at=seq((1/(2*(ncol(table_count[,index1:index2])-1))),1-1/(2*(ncol(table_count[,index1:index2])-1)),by=(1-1/(ncol(table_count[,index1:index2])-1))/(ncol(table_count[,index1:index2])-1)),index1:index2) #paste0(index1:index2,":",colnames(table_count[,index1:index2]))
                  }
                  axis(2,at=seq(0,1,by=1/5))
                  
                }else{
                  if (input$LogoDataset=="All Data"){
                    logo_plot<<-plot(logo_per_region_cl[[i]][[input$select_region_logo]]$motif_all,ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
                  }else{
                    logo_plot<<-plot(logo_per_region_cl[[i]][[input$select_region_logo]]$motif_datasets[[input$LogoDataset]],ic.scale=FALSE, ylab="probability",xaxis=F,yaxis=F)
                  }
                  table_count=frequenciesTables_results_cl[[i]]$table_count[,2:ncol(frequenciesTables_results_cl[[i]]$table_count)]
                  
                  region_names<-c("FR1-IMGT","CDR1-IMGT","FR2-IMGT","CDR2-IMGT","FR3-IMGT","CDR3-IMGT")
                  index_1<-c(1,27,39,56,66,105)
                  index_2<-c(26,38,55,65,104,114)
                  
                  region_id=0
                  for (regions in region_names){
                    region_id=region_id+1
                    if (regions==input$select_region_logo){
                      r=region_id
                      break
                    }
                  }
                  
                  i1=index_1[r]
                  i2=index_2[r]
                  
                  axis(1,at=seq((1/(2*(ncol(table_count[,i1:i2])-1))),1-1/(2*(ncol(table_count[,i1:i2])-1)),by=(1-1/(ncol(table_count[,i1:i2])-1))/(ncol(table_count[,i1:i2])-1)),i1:i2) #paste0(i1:i2,":",colnames(table_count[,i1:i2])
                  axis(2,at=seq(0,1,by=1/5))
                }
                dev.off()
              })
            
            
            
          })
        }
        
      })
    })
    
    
    ########################################### Alignment ##############################################    
    observeEvent(input$Execute_pipeline_2nd_part, {
      if (input$pipeline_alignment==F) return()
      
      # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
      withBusyIndicatorServer("Execute_pipeline_2nd_part", {
        
        if (input$useGermline=="Insert Germline" && input$Germline=="") return()
        
        if (input$useGermline=="Insert Germline") only_one_germline=T else only_one_germline=F
        
        if (input$useGermline=="Use Gene's germline") use_genes_germline=T else use_genes_germline=F
        
        if (input$select_topN_clonotypes_for_alignment!="all_clonotypes"){
          if (input$select_topN_clonotypes_for_alignment=="topN_clonotypes_for_alignment"){
            FtopN=T
            Fthr=F
          } 
          else {
            FtopN=F
            Fthr=T
          }
          
          if (input$pipeline_clonotypes==F) {
            validate(
              #"Please ckeck Clonotypes first!"
            )
            showModal(modalDialog(
              title = "Error Message Repertoires",
              "Please ckeck Clonotypes first!",
              easyClose = TRUE,
              footer = NULL
            ))
            return()
          }
        }else {
          FtopN=F
          Fthr=F
        }
        
        if (just_restored_session_alignment==F){ 
          #if (input$regionAlignment=="V.D.J.REGION" || input$regionAlignment=="V.J.REGION"){
          if (length(highly_sim)==0){
            if (input$AAorNtAlignment=="both"){
              alignmentRegion_results<<-alignment(imgtfilter_results$allData,input$regionAlignment,input$Germline,loaded_datasets,only_one_germline,use_genes_germline,input$cell=="T cell","aa",clono$clono_allData,clono$clono_datasets,clono$view_specific_clonotype_allData,clono$view_specific_clonotype_datasets,input$topNClonoAlignment,FtopN,input$thrClonoAlignment,Fthr,F)
              alignmentRegion_results_nt<<-alignment(imgtfilter_results$allData,input$regionAlignment,input$Germline,loaded_datasets,only_one_germline,use_genes_germline,input$cell=="T cell","nt",clono$clono_allData,clono$clono_datasets,clono$view_specific_clonotype_allData,clono$view_specific_clonotype_datasets,input$topNClonoAlignment,FtopN,input$thrClonoAlignment,Fthr,F)
            }else{
              alignmentRegion_results<<-alignment(imgtfilter_results$allData,input$regionAlignment,input$Germline,loaded_datasets,only_one_germline,use_genes_germline,input$cell=="T cell",input$AAorNtAlignment,clono$clono_allData,clono$clono_datasets,clono$view_specific_clonotype_allData,clono$view_specific_clonotype_datasets,input$topNClonoAlignment,FtopN,input$thrClonoAlignment,Fthr,F)
            }
          }else{
            if (input$AAorNtAlignment=="both"){
              alignmentRegion_results<<-alignment(imgtfilter_results$allData,input$regionAlignment,input$Germline,loaded_datasets,only_one_germline,use_genes_germline,input$cell=="T cell","aa",highly_sim,highly_sim_datasets,clono$view_specific_clonotype_allData,clono$view_specific_clonotype_datasets,input$topNClonoAlignment,FtopN,input$thrClonoAlignment,Fthr,T)
              alignmentRegion_results_nt<<-alignment(imgtfilter_results$allData,input$regionAlignment,input$Germline,loaded_datasets,only_one_germline,use_genes_germline,input$cell=="T cell","nt",highly_sim,highly_sim_datasets,clono$view_specific_clonotype_allData,clono$view_specific_clonotype_datasets,input$topNClonoAlignment,FtopN,input$thrClonoAlignment,Fthr.T)
            }else{
              alignmentRegion_results<<-alignment(imgtfilter_results$allData,input$regionAlignment,input$Germline,loaded_datasets,only_one_germline,use_genes_germline,input$cell=="T cell",input$AAorNtAlignment,highly_sim,highly_sim_datasets,clono$view_specific_clonotype_allData,clono$view_specific_clonotype_datasets,input$topNClonoAlignment,FtopN,input$thrClonoAlignment,Fthr,T)
            }
          }
        }
        
        just_restored_session_alignment<<-F
        if (input$AAorNtAlignment=="both"){
          msgAlignment<<-alignmentRegion_results_nt$confirm
        }else{
          msgAlignment<<-alignmentRegion_results$confirm
        }
        
        output$regionAlignmentTable <- renderDataTable({
          if(is.null(input$alignmentDataset)) return()
          if (input$alignmentDataset=="All Data"){
            my_table <- alignmentRegion_results$alignment_allData
          }
          else{
            my_table <- alignmentRegion_results$alignment_datasets[[input$alignmentDataset]]
          }
          
          return(my_table)
          
        }, options = list(pageLength =10,scrollX = TRUE))
        
        output$downloadregionAlignmentTable <- downloadHandler(
          filename = function(){paste0("Alignment_",input$select_alignment, "_",(if (input$AAorNtAlignment!="both") input$AAorNtAlignment else "aa"),"_",input$alignmentDataset,".txt")},
          content = function(file) {
            if (input$alignmentDataset=="All Data") write.table(alignmentRegion_results$alignment_allData, file, sep = "\t", row.names = FALSE, col.names = TRUE)
            else write.table(alignmentRegion_results$alignment_datasets[[input$alignmentDataset]], file, sep = "\t", row.names = FALSE, col.names = TRUE)
          })
        
        output$regionAlignmentTableNt <- renderDataTable({
          if(is.null(input$alignmentDataset)) return()
          if (input$AAorNtAlignment!="both") return()
          if (input$alignmentDataset=="All Data"){
            my_table <- alignmentRegion_results_nt$alignment_allData
          }
          else{
            my_table <- alignmentRegion_results_nt$alignment_datasets[[input$alignmentDataset]]
          }
          
          return(my_table)
          
        }, options = list(pageLength =10,scrollX = TRUE))
        
        output$downloadregionAlignmentTableNt <- downloadHandler(
          filename = function(){paste0("Alignment_",input$select_alignment,"_nt_",input$alignmentDataset,".txt")},
          content = function(file) {
            if (input$alignmentDataset=="All Data") write.table(alignmentRegion_results_nt$alignment_allData, file, sep = "\t", row.names = FALSE, col.names = TRUE)
            else write.table(alignmentRegion_results_nt$alignment_datasets[[input$alignmentDataset]], file, sep = "\t", row.names = FALSE, col.names = TRUE)
          })
        
        output$confirmAlignment<- renderUI({
          h5(msgAlignment, style = "color: #CD0000;")
        })
        
        
        ######################################### Grouped Alignment #########################################
        
        if (input$regionAlignment!="CDR3"){
          #groupedAlignmentRegion<-groupedAlignment(alignmentRegion)
        }
        
        if (just_restored_session_alignment==F){ 
          if (input$AAorNtAlignment=="both") n="aa" else n="nt"
          grouped_alignment_results <<- groupedAlignment(alignmentRegion_results$alignment_allData,alignmentRegion_results$alignment_datasets,loaded_datasets,n)
          if (input$AAorNtAlignment=="both"){
            grouped_alignment_results_nt <<- groupedAlignment(alignmentRegion_results_nt$alignment_allData,alignmentRegion_results_nt$alignment_datasets,loaded_datasets,"nt")
          }
          
        }
        
        just_restored_session_alignment<<-F
        msgGroupedAlignment<<-grouped_alignment_results$confirm
        if (input$AAorNtAlignment=="both"){
          msgGroupedAlignment<<-grouped_alignment_results_nt$confirm
        }
        
        output$groupedAlignmentTable <- renderDataTable({
          if(is.null(input$alignmentDataset)) return()
          if (input$alignmentDataset=="All Data"){
            my_table <- grouped_alignment_results$grouped_alignment_allData
          }
          else{
            my_table <- grouped_alignment_results$grouped_alignment_datasets[[input$alignmentDataset]]
          }
          return(my_table)
          
        },options = list(scrollX = TRUE))
        
        output$downloadGroupedAlignmentTable <- downloadHandler(
          filename = function(){paste0("GroupedAlignment_",(if (input$AAorNtAlignment!="both") input$AAorNtAlignment else "aa"),"_",input$alignmentDataset,".txt")},
          content = function(file) {
            if (input$alignmentDataset=="All Data") write.table(grouped_alignment_results$grouped_alignment_allData, file, sep = "\t", row.names = FALSE, col.names = TRUE)
            else write.table(grouped_alignment_results$grouped_alignment_datasets[[input$alignmentDataset]], file, sep = "\t", row.names = FALSE, col.names = TRUE)
          }) 
        
        output$groupedAlignmentTableNt <- renderDataTable({
          if(is.null(input$alignmentDataset)) return()
          if (input$AAorNtAlignment!="both") return()
          if (input$alignmentDataset=="All Data"){
            my_table <- grouped_alignment_results_nt$grouped_alignment_results$grouped_alignment_allData
          }
          else{
            my_table <- grouped_alignment_results_nt$grouped_alignment_datasets[[input$alignmentDataset]]
          }
          return(my_table)
          
        },options = list(scrollX = TRUE))
        
        output$downloadGroupedAlignmentTableNt <- downloadHandler(
          filename = function(){paste0("GroupedAlignment_",input$select_alignment,"_nt_",input$alignmentDataset,".txt")},
          content = function(file) {
            if (input$alignmentDataset=="All Data") write.table(grouped_alignment_results_nt$grouped_alignment_allData, file, sep = "\t", row.names = FALSE, col.names = TRUE)
            else write.table(grouped_alignment_results_nt$grouped_alignment_datasets[[input$alignmentDataset]], file, sep = "\t", row.names = FALSE, col.names = TRUE)
          }) 
        
        output$confirmGroupedAlignment<- renderUI({
          h5(msgGroupedAlignment, style = "color: #CD0000;")
        })
        
      })
    })
    
    ########################################### Mutations ##############################################    
    observeEvent(input$Execute_pipeline_2nd_part, { 
      if (input$pipeline_mutations==F) return()
      
      if (input$pipeline_alignment==F) {
        validate(
          #"Please ckeck Alignment first!"
        )
        showModal(modalDialog(
          title = "Error Message Mutations",
          "Please ckeck Alignment first!",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
      
      if (input$pipeline_alignment==T & input$AAorNtMutations=="both" & input$AAorNtAlignment !="both") {
        validate(
          #"Please ckeck Alignment both first!"
        )
        showModal(modalDialog(
          title = "Error Message Mutations",
          "Please ckeck Alignment both first!",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
      
      if (input$pipeline_alignment==T & input$AAorNtMutations=="aa" & input$AAorNtAlignment =="nt") {
        validate(
          #"Please ckeck Alignment both first!"
        )
        showModal(modalDialog(
          title = "Error Message Mutations",
          "Please ckeck Alignment aa first!",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
      
      if (input$pipeline_alignment==T & input$AAorNtMutations=="nt" & input$AAorNtAlignment =="aa") {
        validate(
          #"Please ckeck Alignment nt first!"
        )
        showModal(modalDialog(
          title = "Error Message Mutations",
          "Please ckeck Alignment nt first!",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
      
      # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
      withBusyIndicatorServer("Execute_pipeline_2nd_part", {
        
        if (input$select_topN_clonotypes_for_mutations!="all_clonotypes"){
          if (input$select_topN_clonotypes_for_mutations=="topN_clonotypes_for_mutations"){
            FtopN=T
            Fthr=F
          } 
          else {
            FtopN=F
            Fthr=T
          }
          
          if (input$pipeline_clonotypes==F) {
            validate(
              #"Please ckeck Clonotypes first!"
            )
            showModal(modalDialog(
              title = "Error Message Repertoires",
              "Please ckeck Clonotypes first!",
              easyClose = TRUE,
              footer = NULL
            ))
            return()
          }
        }else{ 
          FtopN=F
          Fthr=F
        }
        
        if (input$select_clonotypes_for_mutations){
          FclonoSeperately<<-T
          if (input$pipeline_clonotypes==F) {
            validate(
              #"Please ckeck Clonotypes first!"
            )
            showModal(modalDialog(
              title = "Error Message Repertoires",
              "Please ckeck Clonotypes first!",
              easyClose = TRUE,
              footer = NULL
            ))
            return()
          }
        }else FclonoSeperately<<-F
        
        if (input$select_clonotypes_for_mutations){
          num_of_clusters = length(strsplit(input$clonotypes_for_mutations,",")[[1]])
          cl_ids_mutations<<-as.numeric(strsplit(input$clonotypes_for_mutations,",")[[1]])
        }
        
        
        if (just_restored_session_mutations==F){ 
          if (input$AAorNtMutations=="both"){ 
            mutation_results<<-mutations(grouped_alignment_results$grouped_alignment_allData,grouped_alignment_results$grouped_alignment_datasets,input$ThrAAMutations,"aa",loaded_datasets,input$topNClonoMutations, FtopN,F,0,Fthr,input$thrClonoMutations)
            mutation_results_nt<<-mutations(grouped_alignment_results_nt$grouped_alignment_allData,grouped_alignment_results_nt$grouped_alignment_datasets,input$ThrNtMutations,"nt",loaded_datasets, input$topNClonoMutations, FtopN,F,0,Fthr,input$thrClonoMutations)
          }else{
            if (input$AAorNtMutations=="aa"){
              thr=input$ThrAAMutations 
              align_all=grouped_alignment_results$grouped_alignment_allData
              align_datasets=grouped_alignment_results$grouped_alignment_datasets
            }else {#input$AAorNtMutations=="nt"
              thr=input$ThrNtMutations
              if (input$AAorNtAlignment=="nt"){
                align_all=grouped_alignment_results$grouped_alignment_allData
                align_datasets=grouped_alignment_results$grouped_alignment_datasets
              } 
              else if (input$AAorNtAlignment=="both"){
                align_all=grouped_alignment_results_nt$grouped_alignment_allData
                align_datasets=grouped_alignment_results_nt$grouped_alignment_datasets
              } 
            }
            mutation_results<<-mutations(align_all,align_datasets,thr,input$AAorNtMutations,loaded_datasets,input$topNClonoMutations, FtopN,F,0,Fthr,input$thrClonoMutations)
          }
          
          if (FclonoSeperately){
            for (cl in 1:length(cl_ids_mutations)){
              if (input$AAorNtMutations=="both"){ 
                mutation_results_cl[[cl]]<<-mutations(grouped_alignment_results$grouped_alignment_allData,grouped_alignment_results$grouped_alignment_datasets,input$ThrAAMutations,"aa",loaded_datasets,input$topNClonoMutations, FtopN,FclonoSeperately,cl_ids_mutations[cl],F)
                mutation_results_nt_cl[[cl]]<<-mutations(grouped_alignment_results_nt$grouped_alignment_allData,grouped_alignment_results_nt$grouped_alignment_datasets,input$ThrNtMutations,"nt",loaded_datasets, input$topNClonoMutations, FtopN,FclonoSeperately,cl_ids_mutations[cl],F)
              }else{
                if (input$AAorNtMutations=="aa"){
                  thr=input$ThrAAMutations 
                  align_all=grouped_alignment_results$grouped_alignment_allData
                  align_datasets=grouped_alignment_results$grouped_alignment_datasets
                }else {#input$AAorNtMutations=="nt"
                  thr=input$ThrNtMutations
                  if (input$AAorNtAlignment=="nt"){
                    align_all=grouped_alignment_results$grouped_alignment_allData
                    align_datasets=grouped_alignment_results$grouped_alignment_datasets
                  } 
                  else if (input$AAorNtAlignment=="both"){
                    align_all=grouped_alignment_results_nt$grouped_alignment_allData
                    align_datasets=grouped_alignment_results_nt$grouped_alignment_datasets
                  } 
                }
                mutation_results_cl[[cl]]<<-mutations(align_all,align_datasets,thr,input$AAorNtMutations,loaded_datasets,input$topNClonoMutations, FtopN,FclonoSeperately,cl_ids_mutations[cl],F)
              }
            }
            
          }
          
        }
        
        just_restored_session_mutations<<-F
        if (input$AAorNtMutations=="both"){
          msgMutation<<-mutation_results_nt$confirm
        }else{
          msgMutation<<-mutation_results$confirm
        }
        
        #output$uiSelectGeneMutation <- renderUI({
        #selectInput("select_gene_mutation", "Select gene of germline:",names(mutation_results$mutation_change_allData), width="200")
        #})
        
        output$MutationTable <- renderDataTable({
          if(is.null(input$mutationDataset)) return()
          if (input$mutationDataset=="All Data"){
            my_table <- mutation_results$mutation_change_allData#[[input$select_gene_mutation]]
          }
          else{
            my_table <- mutation_results$mutation_change_datasets[[input$mutationDataset]]#[[input$select_gene_mutation]]
          }
          
          return(my_table)
          
        }, options = list(pageLength =10,scrollX = TRUE))
        
        output$downloadMutationTable <- downloadHandler(
          filename = function(){paste0("Mutations_",input$ThrAAMutations, "_",(if (input$AAorNtMutations!="both") input$AAorNtMutations else "aa"),"_",input$mutationDataset,".txt")},
          content = function(file) {
            if (input$mutationDataset=="All Data") write.table(mutation_results$mutation_change_allData, file, sep = "\t", row.names = FALSE, col.names = TRUE)
            else write.table(mutation_results$mutation_change_datasets[[input$mutationDataset]], file, sep = "\t", row.names = FALSE, col.names = TRUE)
          })
        
        output$MutationTableNt <- renderDataTable({
          if(is.null(input$mutationDataset)) return()
          if (input$AAorNtMutations!="both") return()
          if (input$mutationDataset=="All Data"){
            my_table <- mutation_results_nt$mutation_change_allData#[[input$select_gene_mutation]]
          }
          else{
            my_table <- mutation_results_nt$mutation_change_datasets[[input$mutationDataset]]#[[input$select_gene_mutation]]
          }
          
          return(my_table)
          
        }, options = list(pageLength =10,scrollX = TRUE))
        
        output$downloadMutationTableNt <- downloadHandler(
          filename = function(){paste0("Mutations_thr",input$ThrAAMutations,"_nt_",input$mutationDataset,".txt")},
          content = function(file) {
            if (input$mutationDataset=="All Data") write.table(mutation_results_nt$mutation_change_allData, file, sep = "\t", row.names = FALSE, col.names = TRUE)
            else write.table(mutation_results_nt$mutation_change_datasets[[input$mutationDataset]], file, sep = "\t", row.names = FALSE, col.names = TRUE)
          })
        
        if (FclonoSeperately){
          output$uiMutationTables_cl <- renderUI({
            lapply(1:length(cl_ids_mutations), function(i) {
              mainPanel(
                br(),
                dataTableOutput(paste0("MutationTables_cl", i)),
                downloadButton(paste0("downloadMutationTables_cl",i), "Download"),
                br(),
                conditionalPanel(
                  condition = "input.AAorNtMutations == 'both'",
                  dataTableOutput(paste0("MutationTables_nt_cl", i)),
                  downloadButton(paste0("downloadMutationTables_nt_cl",i), "Download")
                )
              )
            })
          })
          
          lapply(1:length(cl_ids_mutations), function(i) {
            output[[paste0("MutationTables_cl", i)]] <- renderDataTable({
              if(is.null(input$mutationDataset)) return()
              if (input$mutationDataset=="All Data"){
                my_table <- mutation_results_cl[[i]]$mutation_change_allData
              }
              else{
                my_table <- mutation_results_cl[[i]]$mutation_change_datasets[[input$mutationDataset]]
              }
              
              return(my_table)
            },options = list(scrollX = TRUE))
            
            output[[paste0("downloadMutationTables_cl", i)]] <- downloadHandler(
              filename = function(){paste0("Mutations_cl",cl_ids_mutations[i],"_thr",input$ThrAAMutations, "_",(if (input$AAorNtMutations!="both") input$AAorNtMutations else "aa"),"_",input$mutationDataset,".txt")},
              content = function(file) {
                if (input$mutationDataset=="All Data") write.table(mutation_results_cl[[i]]$mutation_change_allData, file, sep = "\t", row.names = FALSE, col.names = TRUE)
                else write.table(mutation_results_cl[[i]]$mutation_change_datasets[[input$mutationDataset]], file, sep = "\t", row.names = FALSE, col.names = TRUE)
              })
            
            output[[paste0("MutationTables_nt_cl", i)]] <- renderDataTable({
              if(is.null(input$mutationDataset)) return()
              if (input$AAorNtMutations!="both") return()
              if (input$mutationDataset=="All Data"){
                my_table <- mutation_results_nt_cl[[i]]$mutation_change_allData
              }
              else{
                my_table <- mutation_results_nt_cl[[i]]$mutation_change_datasets[[input$mutationDataset]]
              }
              
              return(my_table)
              
            },options = list(scrollX = TRUE))
            
            output[[paste0("downloadMutationTables_nt_cl", i)]] <- downloadHandler(
              filename = function(){paste0("Mutations_cl",cl_ids_mutations[i],"_thr",input$ThrAAMutations,"_nt_",input$mutationDataset,".txt")},
              content = function(file) {
                if (input$mutationDataset=="All Data") write.table(mutation_results_nt_cl[[i]]$mutation_change_allData, file, sep = "\t", row.names = FALSE, col.names = TRUE)
                else write.table(mutation_results_nt_cl[[i]]$mutation_change_datasets[[input$mutationDataset]], file, sep = "\t", row.names = FALSE, col.names = TRUE)
              })
            
          })
        }
        
        output$confirmMutations<- renderUI({
          h5(msgMutation, style = "color: #CD0000;")
        })
      })
    })
    
    ########################################### Length distance ########################################
    output$uiSelectGene1Diff <- renderUI({
      if (input$pipeline_CDR3Diff1==F || length(imgtfilter_results$allData)==0) return()
      selectInput("selectGeneCDR3Diff1", "Select gene:",unique(imgtfilter_results$allData[[used_columns[["Summary"]][3]]]) , width="270px")
    })
    
    ########################################### CDR3 1 length diff #####################################
    observeEvent(input$Execute_pipeline_2nd_part, {
      if (input$pipeline_CDR3Diff1==F) return()
      
      # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
      withBusyIndicatorServer("Execute_pipeline_2nd_part", {
        
        if (just_restored_session_CDR3Diff1==F) 
          CDR3Diff1_results<<-find_cdr3_diff1P(imgtfilter_results$allData,input$cdr3MaxLength1Diff,input$cdr3Position1Diff,loaded_datasets)
        
        just_restored_session_CDR3Diff1<<-F
        msgCDR3Diff1<<-CDR3Diff1_results$confirm
        
        output$CDR3Diff1Table <- renderDataTable({
          if(is.null(input$CDR3Diff1Dataset)) return()
          if (input$CDR3Diff1Dataset=="All Data"){
            my_table <- CDR3Diff1_results$cdr3_diff1P_allData
          }
          else{
            my_table <- CDR3Diff1_results$cdr3_diff1P_datasets[[input$CDR3Diff1Dataset]]
          }

          return(my_table)
          
        }, options = list(pageLength =20,scrollX = TRUE))
        
        output$downloadCDR3Diff1Table <- downloadHandler(
          filename = function(){paste0("CDR3Diff1_",input$CDR3Diff1Dataset,".txt")},
          content = function(file) {
            if (input$clonotypesDataset=="All Data") write.table(CDR3Diff1_results$cdr3_diff1P_allData, file, sep = "\t", row.names = FALSE, col.names = TRUE)
            else write.table(CDR3Diff1_results$cdr3_diff1P_datasets[[input$CDR3Diff1Dataset]], file, sep = "\t", row.names = FALSE, col.names = TRUE)
          })
        
        output$confirmCDR3Diff1<- renderUI({
          h5(msgCDR3Diff1, style = "color: #CD0000;")
        })
        
      })
    })
    
    ########################################### Overview  ############################################## ###########################################       
    output$overview_data <- renderTable({
      if(!input$Execute) return()
      my_table=as.data.frame(c(paste0("Dataset uploaded: ",loaded_datasets),paste0("Selected files: ",input$inputFiles),paste0("Cell: ",input$cell)))
      colnames(my_table)="Input Data"
      return(my_table)
    })
    
    output$overview_cleaning_parameters <- renderTable({
      if(!input$Execute) return()
      #cleaning_workflow[,1]=cleaning_criteria[cleaning_workflow[,1]]
      cleaning_workflow[,1]=cleaning_parameters
      my_table=as.data.frame(cleaning_workflow)
      #my_table=as.data.frame(cleaning_parameters)
      colnames(my_table)=c("Preselection parameters","Filter Out", "Filter In")
      return(my_table)
    })
    
    output$overview_filtering_parameters <- renderTable({
      if(!input$Execute) return()
      filtering_workflow[,1]=filtering_parameters
      my_table=as.data.frame(filtering_workflow)
      colnames(my_table)=c("Selection parameters","Filter Out", "Filter In")
      return(my_table)
    })
    
    output$overview_clonotypes <- renderTable({
      if(!input$Execute) return()
      if (msgClonotypes=="") return()
      c1=c("Clonotypes selected","Number of clonotypes found","The most frequent clonotype")
      c2=c(input$select_clonotype,nrow(clono$clono_allData),paste0(clono$clono_allData$clonotype[1]," with ",clono$clono_allData$N[1]," sequences (",clono$clono_allData$Freq[1],"%)"))
      my_table=data.frame(Clonotypes=c1,Result=c2)
      return(my_table)
    })
    
    output$overview_highly_sim_clono <- renderTable({
      if(!input$Execute) return()
      if (msgHighlySim=="") return()
      my_table=c(paste0("Clonotype frequency threshold for highly similar: ",input$clonotype_freq_thr_for_highly_sim),paste0("Take gene highly similar: ",input$take_gene_highly_similar))
      for (i in 1:length(cdr3_lengths)){
        my_table=c(my_table,paste0("length ",cdr3_lengths[i],": ",input[[paste0("num_of_missmatches_cdr3_length_",i)]]))
      }
      my_table=data.frame(Highly_Similar_Clonotypes=my_table)
      return(my_table)
    })
    
    output$overview_identity_groups <- renderTable({
      if(!input$Execute) return()
      if (!input$pipeline_insert_identity_groups) return()
      my_table=c()

      low=c()
      high=c()
      for (i in 1:input$N_identity_groups){
        low=c(low,input[[paste0("Identity_low_group",i)]])
        high=c(high,input[[paste0("Identity_high_group",i)]])
      }
      label=paste(low,high,sep="-")
      identity_groups<<-(data.frame(low=low,high=high, label=label,stringsAsFactors = F))
      for (i in 1:input$N_identity_groups){
        my_table=c(my_table,paste0("low: ",identity_groups$low[i],", high: ",identity_groups$high[i]))
      }
      my_table=data.frame(identity_groups=my_table)
      colnames(my_table)="Identity Groups"
      return(my_table)
    })
    
    output$overview_public_clono <- renderTable({
      if(!input$Execute) return()
      if (msgPublicClono=="") return()
      my_table=c(paste0("Take Gene into account: ",input$take_gene_public_clono),paste0("Threshold for Clonotype reads:",input$thr_public_clono))
      my_table=data.frame(Public_Clonotypes_Parameters=my_table)
      return(my_table)
    })
    
    output$overview_repertoires <- renderTable({
      if(!input$Execute) return()
      if (is.null(msgRepertoires)) return()
      if (msgRepertoires[1]!="") return()
      my_table=c()
      for (i in 1:length(insertedRepertoires)){
        my_table=c(my_table,input[[paste0("selectRepertoires_",insertedRepertoires[i])]])
      }
      my_table=data.frame(Repertoires_Parameters=my_table)
      return(my_table)
    })
    
    output$overview_multiple_value_comparison <- renderTable({
      if(!input$Execute) return()
      if (length(msgMultiple_value_comparison)==0) return()
      my_table=c()
      for (i in 1:length(insertedMultiple_value_comparison)){
        val1=input[[paste0("select_MultipleValues_column1_",strsplit(insertedMultiple_value_comparison[i],"_")[[1]][2])]]
        val2=input[[paste0("select_MultipleValues_column2_",strsplit(insertedMultiple_value_comparison[i],"_")[[1]][2])]]
        my_table=c(my_table,paste0(val1," + ",val2))
      }
      my_table=data.frame(Multiple_value_comparison_Parameters=my_table)
      return(my_table)
    })
    
    output$overview_cdr3_1length_diff <- renderTable({
      if(!input$Execute) return()
      if (msgCDR3Diff1=="") return()
      my_table=c(paste0("max CDR3 length: ",input$cdr3MaxLength1Diff),paste0("CDR3 position with difference: ",input$cdr3Position1Diff))
      my_table=data.frame(CDR3Diff1_Parameters=my_table)
      return(my_table)
    })
    
    output$overview_alignment <- renderTable({
      if(!input$Execute) return()
      if (msgAlignment=="") return()
      my_table=c(paste0("Region for Alignment: ",input$regionAlignment),paste0("Max length of region: ",input$MaxLengthRegion))
      if (input$useGermline=="Insert Germline") my_table=c(my_table,paste0("Germline: ",input$Germline))
      else my_table=c(my_table,paste0("Germline: ",input$useGermline))
      my_table=data.frame(Alignment_Parameters=my_table)
      return(my_table)
    })
    
    output$overview_freq_table <- renderTable({
      if(!input$Execute) return()
      if (msgFreqTables=="") return()
      my_table=c(paste0("Region for Frequency Table: ",input$regionFreqTable),paste0("Region length: ",input$regionLengthFreq))
      if (input$select_topN_clonotypes_for_freqTable == T) my_table=c(my_table,paste0("Use top ",input$topNFreqTable," clonotypes"))
      else my_table=c(my_table,paste0("Germline: ",input$useGermline))
      my_table=data.frame(Logo_Parameters=my_table)
      return(my_table)
    })
    
    output$downloadOverview <- downloadHandler(
      filename = function(){paste0("overview",".pdf")},
      content = function(file) {
        pdf(file)
        
        my_table=as.data.frame(c(paste0("Dataset uploaded: ",loaded_datasets),paste0("Selected files: ",input$inputFiles),paste0("Cell: ",input$cell)))
        colnames(my_table)="Input Data"
        if (nrow(my_table)>10){
          k=1
          for (i in 1:(nrow(my_table)/10+1)){
            if (i==(nrow(my_table)/10+1)) my_table_part=my_table[k:(nrow(my_table)),]
            else my_table_part=my_table[k:(k+9),]
            k=k+10
            plot(NA, xlim=c(0,5), ylim=c(0,5), bty='n', xaxt='n', yaxt='n', xlab='', ylab='')
            text(1,4,"Input", pos=4)
            grid.table(my_table_part)
            if(k>=nrow(my_table)) break()
          }
        }else grid.table(my_table)
        
        
        for (j in 1:(length(loaded_datasets)+1)){
          if (j==(length(loaded_datasets)+1)){
            plot(NA, xlim=c(0,5), ylim=c(0,5), bty='n', xaxt='n', yaxt='n', xlab='', ylab='')
            text(1,4,"Cleaning Parameters (All Data)", pos=4)
            cleaning_workflow[,1]=cleaning_parameters
            my_table=as.data.frame(cleaning_workflow)
            if (nrow(my_table)>0){
              colnames(my_table)=c("Preselection parameters","Filter Out", "Filter In")
              grid.table(my_table)
            }
          }else{
            plot(NA, xlim=c(0,5), ylim=c(0,5), bty='n', xaxt='n', yaxt='n', xlab='', ylab='')
            text(1,4,paste0("Cleaning Parameters (",loaded_datasets[j],")"), pos=4)
            imgtcleaning_results$workflow_datasets[[loaded_datasets[j]]][,1]=cleaning_parameters
            my_table=as.data.frame(imgtcleaning_results$workflow_datasets[[loaded_datasets[j]]])
            if (nrow(my_table)>0){
              colnames(my_table)=c("Preselection parameters","Filter Out", "Filter In")
              grid.table(my_table)  
            }
            
          }
        }
        
        
        for (j in 1:(length(loaded_datasets)+1)){
          if (j==(length(loaded_datasets)+1)){
            plot(NA, xlim=c(0,5), ylim=c(0,5), bty='n', xaxt='n', yaxt='n', xlab='', ylab='')
            text(1,4,"Filtering Parameters (All Data)", pos=4)
            filtering_workflow[,1]=filtering_parameters
            my_table=as.data.frame(filtering_workflow)
            if (nrow(my_table)>0){
              colnames(my_table)=c("Selection parameters","Filter Out", "Filter In")
              grid.table(my_table) 
            }
          }else{
            plot(NA, xlim=c(0,5), ylim=c(0,5), bty='n', xaxt='n', yaxt='n', xlab='', ylab='')
            text(1,4,paste0("Filtering Parameters (",loaded_datasets[j],")"), pos=4)
            imgtfilter_results$workflow_datasets[[loaded_datasets[j]]][,1]=filtering_parameters
            my_table=as.data.frame(imgtfilter_results$workflow_datasets[[loaded_datasets[j]]])
            if (nrow(my_table)>0){
              colnames(my_table)=c("Selection parameters","Filter Out", "Filter In")
              grid.table(my_table)
            }
          }
        }
        
        
        if (msgClonotypes!="")
          for (j in 1:(length(loaded_datasets)+1)){
            if (j==(length(loaded_datasets)+1)){
              plot(NA, xlim=c(0,5), ylim=c(0,5), bty='n', xaxt='n', yaxt='n', xlab='', ylab='')
              text(1,4,"Clonotypes (All Data)", pos=4)
              c1=c("Clonotypes selected","Number of clonotypes found","The most frequent clonotype","Frequency of no 1 clonotype")
              c2=c(input$select_clonotype,nrow(clono$clono_allData),clono$clono_allData$clonotype[1],paste0(clono$clono_allData$N[1]," sequences (",clono$clono_allData$Freq[1],"%)"))
              my_table=data.frame(Clonotypes=c1,Result=c2)
              grid.table(my_table)
            }else{
              plot(NA, xlim=c(0,5), ylim=c(0,5), bty='n', xaxt='n', yaxt='n', xlab='', ylab='')
              text(1,4,paste0("Clonotypes (",loaded_datasets[j],")"), pos=4)
              c1=c("Clonotypes selected","Number of clonotypes found","The most frequent clonotype","Frequency of no 1 clonotype")
              c2=c(input$select_clonotype,nrow(clono$clono_datasets[[loaded_datasets[j]]]),clono$clono_datasets[[loaded_datasets[j]]]$clonotype[1],paste0(clono$clono_datasets[[loaded_datasets[j]]]$N[1]," sequences (",clono$clono_datasets[[loaded_datasets[j]]]$Freq[1],"%)"))
              my_table=data.frame(Clonotypes=c1,Result=c2)
              grid.table(my_table)
            }
          }
        
        if (msgHighlySim!=""){
          my_table=c(paste0("Clonotype frequency threshold for highly similar: ",input$clonotype_freq_thr_for_highly_sim),paste0("Take gene highly similar: ",input$take_gene_highly_similar))
          for (i in 1:length(cdr3_lengths)){
            my_table=c(my_table,paste0("length ",cdr3_lengths[i],": ",input[[paste0("num_of_missmatches_cdr3_length_",i)]]))
          }
          my_table=data.frame(Highly_Similar_Clonotypes=my_table)
          if (nrow(my_table)>10){
            k=1
            for (i in 1:(nrow(my_table)/10+1)){
              if (i==(nrow(my_table)/10+1)) my_table_part=my_table[k:(nrow(my_table)),]
              else my_table_part=my_table[k:(k+9),]
              my_table_part=data.frame(my_table_part)
              colnames(my_table_part)="Highly Similar Clonotypes Parameters"
              k=k+10
              plot(NA, xlim=c(0,5), ylim=c(0,5), bty='n', xaxt='n', yaxt='n', xlab='', ylab='')
              grid.table(my_table_part)
              if(k>=nrow(my_table)) break()
            }
          }else {
            plot(NA, xlim=c(0,5), ylim=c(0,5), bty='n', xaxt='n', yaxt='n', xlab='', ylab='')
            grid.table(my_table)
          }
        }
        
        if (msgPublicClono!=""){
          plot(NA, xlim=c(0,5), ylim=c(0,5), bty='n', xaxt='n', yaxt='n', xlab='', ylab='')
          text(1,4,paste0("Public Clonotypes Parameters"), pos=4)
          my_table=c(paste0("Take Gene into account: ",input$take_gene_public_clono),paste0("Threshold for Clonotype reads:",input$thr_public_clono))
          my_table=data.frame(Public_Clonotypes_Parameters=my_table)
          grid.table(my_table)
        }
        
        if (!(is.null(msgRepertoires))){
          if (msgRepertoires[1]!=""){
            plot(NA, xlim=c(0,5), ylim=c(0,5), bty='n', xaxt='n', yaxt='n', xlab='', ylab='')
            text(1,4,paste0("Repertoires Parameters"), pos=4)
            my_table=c()
            for (i in 1:length(insertedRepertoires)){
              my_table=c(my_table,input[[paste0("selectRepertoires_",insertedRepertoires[i])]])
            }
            my_table=data.frame(Repertoires_Parameters=my_table)
            grid.table(my_table)
          }
        }
        
        if (input$pipeline_insert_identity_groups){
          plot(NA, xlim=c(0,5), ylim=c(0,5), bty='n', xaxt='n', yaxt='n', xlab='', ylab='')
          text(1,4,paste0("Identity Groups"), pos=4)
          my_table=c()
          low=c()
          high=c()
          for (i in 1:input$N_identity_groups){
            low=c(low,input[[paste0("Identity_low_group",i)]])
            high=c(high,input[[paste0("Identity_high_group",i)]])
          }
          label=paste(low,high,sep="-")
          identity_groups<<-(data.frame(low=low,high=high, label=label,stringsAsFactors = F))
          for (i in 1:input$N_identity_groups){
            my_table=c(my_table,paste0("low: ",identity_groups$low[i],", high: ",identity_groups$high[i]))
          }
          my_table=data.frame(identity_groups=my_table)
          grid.table(my_table)
        }
        
        
        if (length(msgMultiple_value_comparison)>0){
          plot(NA, xlim=c(0,5), ylim=c(0,5), bty='n', xaxt='n', yaxt='n', xlab='', ylab='')
          text(1,4,paste0("Multiple value comparison Parameters"), pos=4)
          my_table=c()
          for (i in 1:length(insertedMultiple_value_comparison)){
            val1=input[[paste0("select_MultipleValues_column1_",strsplit(insertedMultiple_value_comparison[i],"_")[[1]][2])]]
            val2=input[[paste0("select_MultipleValues_column2_",strsplit(insertedMultiple_value_comparison[i],"_")[[1]][2])]]
            my_table=c(my_table,paste0(val1," + ",val2))
          }
          my_table=data.frame(Multiple_value_comparison_Parameters=my_table)
          grid.table(my_table)
        }
        
        if (msgCDR3Diff1!=""){
          plot(NA, xlim=c(0,5), ylim=c(0,5), bty='n', xaxt='n', yaxt='n', xlab='', ylab='')
          text(1,4,paste0("CDR3 Diff 1 Parameters"), pos=4)
          my_table=c(paste0("max CDR3 length: ",input$cdr3MaxLength1Diff),paste0("CDR3 position with difference: ",input$cdr3Position1Diff))
          my_table=data.frame(CDR3Diff1_Parameters=my_table)
          grid.table(my_table)
        }
        
        if (msgAlignment!=""){
          plot(NA, xlim=c(0,5), ylim=c(0,5), bty='n', xaxt='n', yaxt='n', xlab='', ylab='')
          text(1,4,paste0("Alignment Parameters"), pos=4)
          my_table=c(paste0("Region for Alignment: ",input$regionAlignment),paste0("AA or Nt Alignment: ",input$AAorNtAlignment))
          if (input$useGermline=="Insert Germline") my_table=c(my_table,paste0("Germline: ",input$Germline))
          else my_table=c(my_table,paste0("Germline: ",input$useGermline))
          my_table=data.frame(Alignment_Parameters=my_table)
          grid.table(my_table)
        }
        
        if (msgFreqTables!=""){
          plot(NA, xlim=c(0,5), ylim=c(0,5), bty='n', xaxt='n', yaxt='n', xlab='', ylab='')
          text(1,4,paste0("Frequency Table Parameters"), pos=4)
          my_table=c(paste0("Region for Frequency Table: ",input$regionFreqTable),paste0("Region length: ",input$regionLengthFreq))
          if (input$select_topN_clonotypes_for_freqTable == T) my_table=c(my_table,paste0("Use top ",input$topNFreqTable," clonotypes"))
          else my_table=c(my_table,paste0("Germline: ",input$useGermline))
          my_table=data.frame(Freq_Table_Parameters=my_table)
          grid.table(my_table)
        }
        
        
        dev.off()
      }) 
    
    ########################################### Download all png files into a .tar ################################# 
    output$downloadAllPlots <- downloadHandler(
      filename <- function() {
        paste("Analysis Plots ",Sys.time(), '.tar', sep='')}, #name the .tar file
      content <- function(file) {
        folder_name=paste("Analysis",trunc(as.numeric(Sys.time())))
        if(!file.exists(paste0(tmp_path,"/",folder_name))){ #check if the directory has been made yet, I use the time/date at which the action button was pressed to make it relatively unique
          dir.create(paste0(tmp_path,"/",folder_name))}#make the dir if not
        in.path=paste0(tmp_path,"/",folder_name) #go into the dir, alternatively you could just set the path of the file each time
        
        #check if the following have run
        
        ####### clonotype plots  #######
        if (msgClonotypes!=""){
          if (input$clonotypes_barplot_select_range){
            parameters=paste0("from cluster",input$clonotypes_barchart_down_threshold,"to cluster",input$clonotypes_barchart_up_threshold)
          }else{
            parameters=paste0("with_threshold ",input$clonotypes_barchart_threshold)
          }
          
          if (input$clonotypes_barplot_select_range==F){
            #Find the clonotypes that we want to draw for all the datasets
            cl<-c()
            a<-list()
            if (is.null(input$clonotypes_barchart_threshold)) thr=0 else thr=input$clonotypes_barchart_threshold
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
        if (msgHighlySim!=""){
          if (input$higly_sim_clonotypes_barplot_select_range){
            parameters=paste0("from cluster",input$higly_sim_clonotypes_barchart_down_threshold,"to cluster",input$higly_sim_clonotypes_barchart_up_threshold)
          }else{
            parameters=paste0("with_threshold ",input$higly_sim_clonotypes_barchart_threshold)
          }
          
          if (input$higly_sim_clonotypes_barplot_select_range==F){
            #Find the clonotypes that we want to draw for all the datasets
            cl<-c()
            a<-list()
            if (is.null(input$higly_sim_clonotypes_barchart_threshold)) thr=0 else thr=input$higly_sim_clonotypes_barchart_threshold
            a[["allData"]]=highly_sim %>% filter(highly_sim$Freq>thr)
            cl=c(cl,a[["allData"]]$clonotype)
            for (i in loaded_datasets){
              a[[i]]=highly_sim_datasets[[i]] %>% filter(highly_sim_datasets[[i]]$Freq>thr)
              cl<-c(cl,a[[i]]$clonotype)
            }
            
          }else{
            #Find the clonotypes that we want to draw for all the datasets
            range=input$higly_sim_clonotypes_barchart_down_threshold:input$higly_sim_clonotypes_barchart_up_threshold
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
        if (!(is.null(msgRepertoires)))
        {####### reperoires plots
          if (msgRepertoires[1]!=""){
            if (is.null(input$repertories_pies_threshold)) thr=0 else thr=input$repertories_pies_threshold
            
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
        if (!(is.null(msgRepertoires)))
        {####### reperoires plots
          if (msgRepertoires[1]!=""){
            if (is.null(input$HighlySim_repertories_pies_threshold)) thr=0 else thr=input$HighlySim_repertories_pies_threshold
            
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
        if (("1_Summary.txt" %in% input$inputFiles) & (input$pipeline_mutational_status)){
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
        
        #CDR3 Length Distribution #######
        if (input$pipeline_cdr3_distribution){
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
        
        #Pi Distribution #######  
        if (input$pipeline_pi_distribution){
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
        fileNames=input$nucleotides_per_clonotype_Datasets
        topN=input$nucleotides_per_clonotype_topN
        if (msgClonotypes!=""){
          if ((input$nucleotides_per_clonotype==F) && is.null(fileNames)){
            fileNames=loaded_datasets
            topN=10
          }
          nucleotides=matrix(0,topN,length(fileNames))
          
          allData<-list()
          input_datasets=""
          for (i in 1:length(fileNames)){
            #clono$convergent_evolution_list_allData[1:input$nucleotides_per_clonotype_topN,]
            nucleotides[,i]=clono$convergent_evolution_list_datasets_only_num[[loaded_datasets[i]]][1:input$nucleotides_per_clonotype_topN]
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
        
        
        ####### tar it #######
        tar(file,in.path)
      })
    
    ########################################### Visualisation  ######################################### ###########################################    
    observeEvent(input$Execute_pipeline, {
      output$nucleotides_per_clonotype_ui <- renderUI({
        checkboxGroupInput(inputId = "nucleotides_per_clonotype_Datasets", label = "Select Datasets", inline=TRUE, choices = loaded_datasets, selected = loaded_datasets)
      })
      
      ####################### Create Identity Groups - Mutational status #############################
      if (("1_Summary.txt" %in% input$inputFiles) & (input$pipeline_mutational_status)){
        low=c()
        high=c()
        for (i in 1:input$N_identity_groups){
          low=c(low,input[[paste0("Identity_low_group",i)]])
          high=c(high,input[[paste0("Identity_high_group",i)]])
        }
        label=paste(low,high,sep="-")
        identity_groups<<-(data.frame(low=low,high=high, label=label,stringsAsFactors = F))
        
        if (input$pipeline_highly_similar_clonotypes){
          if (input$select_clono_or_highly_for_mutational_status=="initial_clonotypes"){
            highly=F
          }else{
            highly=T
          }
        }else{
          highly=F
        }
        
        if (!highly){
          #All Data
          if (input$throughput=="Low Throughput"){
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
          #mutational_status_table_datasets<<-list()
          for (j in 1:(length(loaded_datasets)+1)){
            if (j==(length(loaded_datasets)+1)){
              mut=filteredData_id %>% group_by(Summary.V.REGION.identity..) %>% summarise(N=n())
              freq=mut$N/nrow(filteredData_id)
              mutational_status_table_allData<<-data.frame(mut,freq)
            }else{
              if (input$throughput=="Low Throughput"){
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
              mutational_status_table_datasets[[loaded_datasets[j]]]<<-data.frame(mut,freq)
            }
          }
          
        }else{
          #All Data
          if (input$throughput=="Low Throughput"){
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
          #mutational_status_table_datasets<<-list()
          for (j in 1:(length(loaded_datasets)+1)){
            if (j==(length(loaded_datasets)+1)){
              mut=filteredData_id %>% group_by(Summary.V.REGION.identity..) %>% summarise(N=n())
              freq=mut$N/nrow(filteredData_id)
              mutational_status_table_allData<<-data.frame(mut,freq)
            }else{
              if (input$throughput=="Low Throughput"){
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
              mutational_status_table_datasets[[loaded_datasets[j]]]<<-data.frame(mut,freq)
            }
          }
          
        } 
        
        #Mutational status plots
        output$mutational_status_plot <- renderPlot({
          if(is.null(input$VisualisationDataset)) return()
          if (("1_Summary.txt" %in% input$inputFiles) & (input$pipeline_mutational_status)){
            if (input$VisualisationDataset=="All Data"){
              pie(as.numeric(mutational_status_table_allData$N), labels = round(100*mutational_status_table_allData$freq,2), main = paste0("Mutational Status ", input$VisualisationDataset),col = rainbow(length(mutational_status_table_allData$N)))
              legend("topright", as.character(mutational_status_table_allData[[used_columns[["Summary"]][4]]]), cex = 0.8,
                     fill = rainbow(length(mutational_status_table_allData$N)))
            }else{
              pie(as.numeric(mutational_status_table_datasets[[input$VisualisationDataset]]$N), labels = round(100*mutational_status_table_datasets[[input$VisualisationDataset]]$freq,2), main = paste0("Mutational Status ", input$VisualisationDataset),col = rainbow(length(mutational_status_table_datasets[[input$VisualisationDataset]]$N)))
              legend("topright", as.character(mutational_status_table_datasets[[input$VisualisationDataset]][[used_columns[["Summary"]][4]]]), cex = 0.8,
                     fill = rainbow(length(mutational_status_table_datasets[[input$VisualisationDataset]]$N)))
            }
          } 
          
        })
        
        #Mutational status table
        output$mutational_status_table <- renderTable({
          if(is.null(input$VisualisationDataset)) return()
          if (("1_Summary.txt" %in% input$inputFiles) & (input$pipeline_mutational_status)){
            if (input$VisualisationDataset=="All Data"){
              my_table=mutational_status_table_allData
              
            }else{
              my_table=mutational_status_table_datasets[[input$VisualisationDataset]]
            }
          }
        })
        
      }
      ###################################################################
      output$nucleotides_per_clonotype_hist3D <- renderPlot({
        if(is.null(input$VisualisationDataset)) return()
        fileNames=input$nucleotides_per_clonotype_Datasets
        if (is.null(fileNames)) return()
        nucleotides=matrix(0,input$nucleotides_per_clonotype_topN,length(fileNames))
        
        allData<-list()
        for (i in 1:length(fileNames)){
          #clono$convergent_evolution_list_allData[1:input$nucleotides_per_clonotype_topN,]
          nucleotides[,i]=clono$convergent_evolution_list_datasets_only_num[[fileNames[i]]][1:input$nucleotides_per_clonotype_topN]
        }
        #plot
        hist3D(y = 1:length(fileNames), x = 1:input$nucleotides_per_clonotype_topN, z = nucleotides, clab = "Num of Nucleotides",ylab="Samples",xlab="Clonotypes",
               zlab="Num of Nucleotides",ticktype="detailed",axes=TRUE, theta=50, phi=25, expand=0.75)
      })
      
      output$nucleotides_per_clonotype_persp3D <- renderPlot({
        if(is.null(input$VisualisationDataset)) return()
        fileNames=input$nucleotides_per_clonotype_Datasets
        if (is.null(fileNames)) return()
        if (length(fileNames)==0) return()
        nucleotides=matrix(0,input$nucleotides_per_clonotype_topN,length(fileNames))
        
        allData<-list()
        for (i in 1:length(fileNames)){
          #clono$convergent_evolution_list_allData[1:input$nucleotides_per_clonotype_topN,]
          nucleotides[,i]=clono$convergent_evolution_list_datasets_only_num[[fileNames[i]]][1:input$nucleotides_per_clonotype_topN]
        }
        #plot
        persp3D(y = 1:length(fileNames), x = 1:input$nucleotides_per_clonotype_topN, z = nucleotides, clab = "Num of Nucleotides",ylab="Samples",xlab="Clonotypes",
                zlab="Num of Nucleotides",ticktype="detailed",axes=TRUE, theta=50, phi=25, expand=0.75)
      })
      
      output$nucleotides_per_clonotype_image2D <- renderPlot({
        if(is.null(input$VisualisationDataset)) return()
        fileNames=input$nucleotides_per_clonotype_Datasets
        if (is.null(fileNames)) return()
        if (length(fileNames)==0) return()
        nucleotides=matrix(0,input$nucleotides_per_clonotype_topN,length(fileNames))
        
        allData<-list()
        for (i in 1:length(fileNames)){
          #clono$convergent_evolution_list_allData[1:input$nucleotides_per_clonotype_topN,]
          nucleotides[,i]=clono$convergent_evolution_list_datasets_only_num[[fileNames[i]]][1:input$nucleotides_per_clonotype_topN]
        }
        #plot
        image2D(y = 1:length(fileNames), x = 1:input$nucleotides_per_clonotype_topN, z = nucleotides, clab = "Num of Nucleotides",ylab="Samples",xlab="Clonotypes",
                colkey = list(dist = 0, shift = 0.15,
                              side = 4, length = 0.5, width = 0.5,
                              cex.clab = 1, col.clab = "black", line.clab = 1.4,
                              col.axis = "black", col.ticks = "black", cex.axis = 0.8))
      })
      
      output$nucleotides_per_clonotype_surface <- renderPlotly({
        if(is.null(input$VisualisationDataset)) return()
        fileNames=input$nucleotides_per_clonotype_Datasets
        if (is.null(fileNames)) return()
        nucleotides=matrix(0,input$nucleotides_per_clonotype_topN,length(fileNames))
        
        allData<-list()
        for (i in 1:length(fileNames)){
          #clono$convergent_evolution_list_allData[1:input$nucleotides_per_clonotype_topN,]
          nucleotides[,i]=clono$convergent_evolution_list_datasets_only_num[[fileNames[i]]][1:input$nucleotides_per_clonotype_topN]
        }
        #plot
        ax <- list(
          gridwidth = 2,
          linewidth = 6 
        )
        p <- plot_ly(y = (fileNames), x = 1:input$nucleotides_per_clonotype_topN, z = t(nucleotides)) %>% add_surface()%>%
          layout(
            title = "Nucleotides per Clonotype",
            scene = list(
              xaxis = list(title = "Clonotypes"),
              yaxis = list(title = "Samples"),
              zaxis = list(title = "Num of Nucleotides")
            ),
            xaxis = ax, yaxis = ax
          )
        return(p)
      })
      ############################## Distributions #####################################
      if (msgClonotypes!=""){
        ############ CDR3 Distribution  ############
        if (input$pipeline_cdr3_distribution){
          var=used_columns[["Summary"]][15]
          if (input$pipeline_highly_similar_clonotypes){
            if (input$select_clono_or_highly_for_cdr3_distribution=="initial_clonotypes"){
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
                cdr3_length_distribution_dataset[[loaded_datasets[j]]]<<-d[order(d$CDR3Length),]
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
                cdr3_length_distribution_dataset[[loaded_datasets[j]]]<<-d[order(d$CDR3Length),]
              }
            }
          }
          
        }
        
        #length_distribution plot
        output$length_distribution <- renderPlot({
          if(is.null(input$VisualisationDataset)) return()
          if ("1_Summary.txt" %in% input$inputFiles){
            var=used_columns[["Summary"]][15]
            if (input$VisualisationDataset=="All Data"){
              d=cdr3_length_distribution
              plot(d$CDR3Length,d$n,main=paste0("CDR3 IMGT length ", "All Data"), xlab="length",ylab="") # plots the results
              lines(spline(d$CDR3Length,d$n))
            }else{
              d=cdr3_length_distribution_dataset[[input$VisualisationDataset]]
              plot(d$CDR3Length,d$n,main=paste0("CDR3 IMGT length ", "All Data"), xlab="length",ylab="") # plots the results
              lines(spline(d$CDR3Length,d$n))
            }
            
          }
          
        })
        
        #length_distribution table
        output$length_distribution_table <- renderDataTable({
          if(is.null(input$VisualisationDataset)) return()
          if ("1_Summary.txt" %in% input$inputFiles){
            if (input$VisualisationDataset=="All Data"){
              my_table=cdr3_length_distribution
              
            }else{
              my_table=cdr3_length_distribution_dataset[[input$VisualisationDataset]]
            }
            return(my_table)
          }
        })
        
        ############ Pi distribution ############
        if (input$pipeline_pi_distribution){
          var="Junction.pI"
          max_length=length(as.numeric(imgtfilter_results$allData[[var]]))
          box_input<<-c()
          
          if (input$pipeline_highly_similar_clonotypes){
            if (input$select_clono_or_highly_for_pi_distribution=="initial_clonotypes"){
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
        
        if ("6_Junction.txt" %in% input$inputFiles){
          var="Junction.pI"
          
          if (input$pipeline_highly_similar_clonotypes){
            if (input$select_clono_or_highly_for_pi_distribution=="initial_clonotypes"){
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
                pi_distribution_dataset[[loaded_datasets[j]]]<<-d[order(d$Pi),]
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
                pi_distribution_dataset[[loaded_datasets[j]]]<<-d[order(d$Pi),]
              }
            }
          }
          
        }
        
        output$pI_distribution <- renderPlot({
          if(is.null(input$VisualisationDataset)) return()
          if ("6_Junction.txt" %in% input$inputFiles){
            boxplot(box_input, horizontal=F, main=" ")
          } 
          
        })
        
        #pI distribution table
        output$pI_distribution_table <- renderDataTable({
          if(is.null(input$VisualisationDataset)) return()
          if ("6_Junction.txt" %in% input$inputFiles){
            if (input$VisualisationDataset=="All Data"){
              my_table=pi_distribution
              
            }else{
              my_table=pi_distribution_dataset[[input$VisualisationDataset]]
            }
            return(my_table)
          }
        })
      }
      
      
      
    })
    
    
    ########################################### Download all tables into a .tar ################################# 
    output$downloadAllTables <- downloadHandler(
      filename <- function() {
        paste("Analysis Tables ",Sys.time(), '.tar', sep='')
      }, #name the .tar file
      content <- function(file) {
        folder_name=paste("AnalysisTables",trunc(as.numeric(Sys.time())))
        if(!file.exists(paste0(tmp_path,"/",folder_name))){ 
          dir.create(paste0(tmp_path,"/",folder_name))
        }
        in.path=paste0(tmp_path,"/",folder_name)
        
        ################################### Initial-Clean-Filter Tables ######################################
        #if (cleaning_confirm!="")
        #for (j in 1:(length(loaded_datasets)+1)){
        #if (j==(length(loaded_datasets)+1)){
        #filename=paste0(in.path,"/","Initial_table_","All Data",".txt")
        #write.table(imgtcleaning_results$allDataInitial, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
        #filename=paste0(in.path,"/","Clean_table_","All Data",".txt")
        #write.table(imgtcleaning_results$allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
        #filename=paste0(in.path,"/","Clean_table_out_","All Data",".txt")
        #write.table(imgtcleaning_results$filterOutSum, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
        #}else{
        #filename=paste0(in.path,"/","Initial_table_",loaded_datasets[j],".txt")
        #write.table(imgtcleaning_results$initial_datasets[[loaded_datasets[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
        #filename=paste0(in.path,"/","Clean_table_",loaded_datasets[j],".txt")
        #write.table(imgtcleaning_results$cleaned_datasets[[loaded_datasets[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
        #filename=paste0(in.path,"/","Clean_table_out_",loaded_datasets[j],".txt")
        #write.table(imgtcleaning_results$cleaned_out_datasets[[loaded_datasets[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
        
        #}
        #}
        
        #if (msg!="")
        #for (j in 1:(length(loaded_datasets)+1)){
        #if (j==(length(loaded_datasets)+1)){
        #filename=paste0(in.path,"/","Filtered_table_","All Data",".txt")
        #write.table(imgtfilter_results$allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
        #filename=paste0(in.path,"/","Filtered_out_table_out_","All Data",".txt")
        #write.table(imgtfilter_results$filterOutSum, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
        #}else{
        #filename=paste0(in.path,"/","Filtered_table_",loaded_datasets[j],".txt")
        #write.table(imgtfilter_results$filtered_datasets[[loaded_datasets[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
        #filename=paste0(in.path,"/","Filtered_out_table_out_",loaded_datasets[j],".txt")
        #write.table(imgtfilter_results$filtered_out_datasets[[loaded_datasets[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
        
        #}
        #}
        
        ########################################### Clonotypes ###############################################  
        if (msgClonotypes!="")
          for (j in 1:(length(loaded_datasets)+1)){
            if (j==(length(loaded_datasets)+1)){
              clono$clono_allData$CDR3=clono$clono_allData[,1]
              clono$clono_allData=clono$clono_allData[,c(1,5,2:4)]
              for (i in 1:nrow(clono$clono_allData)){
                clono$clono_allData[i,2]=strsplit(as.character(clono$clono_allData[i,1])," - ")[[1]][2]
                clono$clono_allData[i,1]=strsplit(as.character(clono$clono_allData[i,1])," - ")[[1]][1]
              }
              filename=paste0(in.path,"/","Clonotypes_",input$select_clonotype,"_","All Data",".txt")
              write.table(clono$clono_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
            }else{
              name=loaded_datasets[j]
              clono$clono_datasets[[name]]$CDR3=clono$clono_datasets[[name]][,1]
              clono$clono_datasets[[name]]=clono$clono_datasets[[name]][,c(1,5,2:4)]
              for (i in 1:nrow(clono$clono_datasets[[name]])){
                clono$clono_datasets[[name]][i,2]=strsplit(as.character(clono$clono_datasets[[name]][i,1])," - ")[[1]][2] 
                clono$clono_datasets[[name]][i,1]=strsplit(as.character(clono$clono_datasets[[name]][i,1])," - ")[[1]][1]   
              }
              filename=paste0(in.path,"/","Clonotypes_",input$select_clonotype,"_",loaded_datasets[j],".txt")
              write.table(clono$clono_datasets[[loaded_datasets[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
            }
          }
        
        ################################### Highly Similar Clonotypes ########################################
        if (msgHighlySim!="")
          for (j in 1:(length(loaded_datasets)+1)){
            for (l in 1:length(cdr3_lengths)){
              if (j==(length(loaded_datasets)+1)){
                filename=paste0(in.path,"/","Highly sim Clonotypes_","All Data_length_",cdr3_lengths[l],".txt")
                write.table(highly_similar_clonotypes_results$highly_sim_clonotypes[[paste0("length ",cdr3_lengths[l])]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
                filename=paste0(in.path,"/","Highly sim Clonotypes groups_","All Data_length_",cdr3_lengths[l],".txt")
                write.table(highly_similar_clonotypes_results$highly_sim_clonotypes_allGroups[[paste0("length ",cdr3_lengths[l])]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
              }else{
                filename=paste0(in.path,"/","Highly sim Clonotypes_",loaded_datasets[j],"_length_",cdr3_lengths[l],".txt")
                write.table(highly_similar_clonotypes_results$highly_sim_clonotypes_datasets[[loaded_datasets[j]]][[paste0("length ",cdr3_lengths[l])]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
                filename=paste0(in.path,"/","Highly sim Clonotypes groups_",loaded_datasets[j],"_length_",cdr3_lengths[l],".txt")
                write.table(highly_similar_clonotypes_results$highly_sim_clonotypes_allGroups_datasets[[loaded_datasets[j]]][[paste0("length ",cdr3_lengths[l])]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
              }
            }
          }
        
        if (msgHighlySim!="")
          for (j in 1:(length(loaded_datasets)+1)){
            if (j==(length(loaded_datasets)+1)){
              filename=paste0(in.path,"/","highly_sim_all_clonotypes_",input$select_clonotype,"_","All Data",".txt")
              write.table(highly_sim, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
            }else{
              filename=paste0(in.path,"/","highly_sim_all_clonotypes_",input$select_clonotype,"_",loaded_datasets[j],".txt")
              write.table(highly_sim_datasets[[loaded_datasets[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
            }
          }
        
        ################################### Public Clonotypes  ##############################################
        if (msgPublicClono!=""){
          filename = paste0(in.path,"/","public_clonotypes",".txt")
          write.table(public_clonotypes_results$public_clono, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
        }
        
        ########################### Highly Similar Public Clonotypes  ########################################
        if (msgPublicClono!=""){
          filename = paste0(in.path,"/","highly_sim_public_clonotypes",".txt")
          write.table(highly_sim_public_clonotypes_results$public_clono, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
        }
        
        ########################################### Repertoires  ############################################
        if (!(is.null(msgRepertoires))){
          if (msgRepertoires[1]!=""){
            for (i in 1:length(insertedRepertoires)){
              for (j in 1:(length(loaded_datasets)+1)){
                if (j==(length(loaded_datasets)+1)){
                  filename=paste0(in.path,"/","Repertoires_",input[[paste0("selectRepertoires_",insertedRepertoires[i])]],"_","All Data",".txt")
                  write.table(repertories_results[[i]]$Repertoires_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
                }else{
                  filename=paste0(in.path,"/","Repertoires_",input[[paste0("selectRepertoires_",insertedRepertoires[i])]],"_",loaded_datasets[j],".txt")
                  write.table(repertories_results[[i]]$Repertoires_datasets[[loaded_datasets[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
                }
              }
            }
          }
        }
        
        ######################################## Highly Sim Repertoires #####################################
        if (!(is.null(msgHighlySim_Repertoires))){
          if (msgHighlySim_Repertoires[1]!=""){
            for (i in 1:length(insertedRepertoires)){
              for (j in 1:(length(loaded_datasets)+1)){
                if (j==(length(loaded_datasets)+1)){
                  filename=paste0(in.path,"/","HighlySim_Repertoires_",input[[paste0("selectRepertoires_",insertedRepertoires[i])]],"_","All Data",".txt")
                  write.table(HighlySim_repertories_results[[i]]$Repertoires_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
                }else{
                  filename=paste0(in.path,"/","HighlySim_Repertoires_",input[[paste0("selectRepertoires_",insertedRepertoires[i])]],"_",loaded_datasets[j],".txt")
                  write.table(HighlySim_repertories_results[[i]]$Repertoires_datasets[[loaded_datasets[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
                }
              }
            }
          }
        }
        
        ######################################## Repertoires Comparison #####################################
        if (msgRepertoiresComp!=""){
          for (i in 1:length(insertedRepertoires)){
            filename=paste0(in.path,"/","repertoires_comparison_table_",input[[paste0("selectRepertoires_",insertedRepertoires[i])]],".txt")
            write.table(repertoires_comparison_results[[i]]$unique_repertoires, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
          }
        }
        
        #################################### Highly Sim Repertoires Comparison ##############################
        if (msgRepertoiresComp!="" && msgHighlySim!=""){
          for (i in 1:length(insertedRepertoires)){
            filename=paste0(in.path,"/","highlySim_repertoires_comparison_table_",input[[paste0("selectRepertoires_",insertedRepertoires[i])]],".txt")
            write.table(highly_sim_repertoires_comparison_results[[i]]$unique_repertoires, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
          }
        }
        
        ########################################### Multiple value comparison  ##############################
        if (length(msgMultiple_value_comparison)>0){
          #load("rData files/Multiple_value_comparison_result.rdata")
          for (i in 1:length(insertedMultiple_value_comparison)){
            for (j in 1:(length(loaded_datasets)+1)){
              val1=input[[paste0("select_MultipleValues_column1_",strsplit(insertedMultiple_value_comparison[i],"_")[[1]][2])]]
              val2=input[[paste0("select_MultipleValues_column2_",strsplit(insertedMultiple_value_comparison[i],"_")[[1]][2])]]
              colnames(Multiple_value_comparison_result[[i]]$Multiple_value_comparison_allData)=c(val1,val2,"N")
              if (j==(length(loaded_datasets)+1)){
                filename=paste0(in.path,"/","Multiple_value_comparison_",str_replace(val1,"%",""),"_",str_replace(val2,"%",""),"_","All Data",".txt")
                write.table(Multiple_value_comparison_result[[i]]$Multiple_value_comparison_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
              }else{
                colnames(Multiple_value_comparison_result[[i]]$Multiple_value_comparison_datasets[[loaded_datasets[j]]])=c(val1,val2,"N")
                filename=paste0(in.path,"/","Multiple_value_comparison_",str_replace(val1,"%",""),"_",str_replace(val2,"%",""),"_",unique(t(data.frame(strsplit(input$Dataset,"_"))[,1]))[j],".txt")
                write.table(Multiple_value_comparison_result[[i]]$Multiple_value_comparison_datasets[[loaded_datasets[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
              }
            }
          }
        }
        ########################################### Freq Tables #############################################
        if (msgFreqTables!="")
          for (j in 1:(length(loaded_datasets)+1)){
            if (j==(length(loaded_datasets)+1)){
              filename=paste0(in.path,"/","Count_table_for_logo_","All Data",".txt")
              write.table(frequenciesTables_results$table_count, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
              filename=paste0(in.path,"/","Freq_table_for_logo_","All Data",".txt")
              write.table(frequenciesTables_results$table_freq, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
            }else{
              filename=paste0(in.path,"/","Count_table_for_logo_",loaded_datasets[j],".txt")
              write.table(frequenciesTables_results$table_count_datasets[[loaded_datasets[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
              filename=paste0(in.path,"/","Freq_table_for_logo_",loaded_datasets[j],".txt")
              write.table(frequenciesTables_results$table_freq_datasets[[loaded_datasets[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
            }
          }
        
        ########################################### Alignment ###############################################
        if (msgAlignment!="")
          for (j in 1:(length(loaded_datasets)+1)){
            if (j==(length(loaded_datasets)+1)){
              if (input$AAorNtAlignment=="both"){
                filename=paste0(in.path,"/","Alignment_",input$select_alignment, "_","aa","_","All Data",".txt")
                write.table(alignmentRegion_results$alignment_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
                filename=paste0(in.path,"/","Alignment_",input$select_alignment, "_","nt","_","All Data",".txt")
                write.table(alignmentRegion_results_nt$alignment_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
                #grouped
                filename=paste0(in.path,"/","Grouped Alignment_",input$select_alignment, "_","aa","_","All Data",".txt")
                write.table(grouped_alignment_results$grouped_alignment_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
                filename=paste0(in.path,"/","Grouped Alignment_",input$select_alignment, "_","nt","_","All Data",".txt")
                write.table(grouped_alignment_results_nt$grouped_alignment_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
              }else{
                filename=paste0(in.path,"/","Alignment_",input$select_alignment, "_",input$AAorNtAlignment,"_","All Data",".txt")
                write.table(alignmentRegion_results$alignment_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
                #grouped
                filename=paste0(in.path,"/","Grouped Alignment_",input$select_alignment, "_",input$AAorNtAlignment,"_","All Data",".txt")
                write.table(grouped_alignment_results$grouped_alignment_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
              } 
              
            }else{
              if (input$AAorNtAlignment=="both"){
                filename=paste0(in.path,"/","Alignment_",input$select_alignment, "_","aa","_",loaded_datasets[j],".txt")
                write.table(alignmentRegion_results$alignment_datasets[[loaded_datasets[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
                filename=paste0(in.path,"/","Alignment_",input$select_alignment,"_","nt","_",loaded_datasets[j],".txt")
                write.table(alignmentRegion_results_nt$alignment_datasets[[loaded_datasets[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
                #grouped
                filename=paste0(in.path,"/","Grouped Alignment_",input$select_alignment, "_","aa","_",loaded_datasets[j],".txt")
                write.table(grouped_alignment_results$grouped_alignment_datasets[[loaded_datasets[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
                filename=paste0(in.path,"/","Grouped Alignment_",input$select_alignment,"_","nt","_",loaded_datasets[j],".txt")
                write.table(grouped_alignment_results_nt$grouped_alignment_datasets[[loaded_datasets[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
              }else{
                filename=paste0(in.path,"/","Alignment_",input$select_alignment,"_",input$AAorNtAlignment,"_",loaded_datasets[j],".txt")
                write.table(alignmentRegion_results$alignment_datasets[[loaded_datasets[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
                #grouped
                filename=paste0(in.path,"/","Grouped Alignment_",input$select_alignment,"_",input$AAorNtAlignment,"_",loaded_datasets[j],".txt")
                write.table(grouped_alignment_results$grouped_alignment_datasets[[loaded_datasets[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
              }
              
            }
          }
        
        ########################################### Mutations ###############################################
        if (msgMutation!=""){
          
          for (j in 1:(length(loaded_datasets)+1)){
            #for (s in names(mutation_results$mutation_change_allData)){
            if (j==(length(loaded_datasets)+1)){
              if (input$AAorNtMutations=="both"){
                filename=paste0(in.path,"/","Mutations_thr",input$ThrAAMutations, "_","aa","_","All Data",".txt")
                write.table(mutation_results$mutation_change_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
                filename=paste0(in.path,"/","Mutations_thr",input$ThrNtMutations, "_","nt","_","All Data",".txt")
                write.table(mutation_results_nt$mutation_change_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
              }else{
                if (input$AAorNtMutations=="aa") thr=input$ThrAAMutations else thr=input$ThrNtMutations
                filename=paste0(in.path,"/","Mutations_thr",thr,"_","All Data",".txt")
                write.table(mutation_results$mutation_change_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
              } 
              
            }else{
              if (input$AAorNtMutations=="both"){
                filename=paste0(in.path,"/","Mutations_thr",input$ThrAAMutations, "_","aa","_",loaded_datasets[j],".txt")
                write.table(mutation_results$mutation_change_datasets[[loaded_datasets[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
                filename=paste0(in.path,"/","Mutations_thr",input$ThrNtMutations,"_","nt","_",loaded_datasets[j],".txt")
                write.table(mutation_results_nt$mutation_change_datasets[[loaded_datasets[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
              }else{
                if (input$AAorNtMutations=="aa") thr=input$ThrAAMutations else thr=input$ThrNtMutations
                filename=paste0(in.path,"/","Mutations_thr",thr,"_",loaded_datasets[j],".txt")
                write.table(mutation_results$mutation_change_datasets[[loaded_datasets[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
              }
              
            }
            #}
            
          }
          
          if (FclonoSeperately){
            for (cl in 1:length(cl_ids_mutations)){
              for (j in 1:(length(loaded_datasets)+1)){
                #for (l in names(mutation_allData)){
                if (j==(length(loaded_datasets)+1)){
                  if (input$AAorNtMutations=="both"){
                    filename=paste0(in.path,"/","Mutations_cl",cl,"_thr",input$ThrAAMutations, "_","aa","_","All Data",".txt")
                    write.table(mutation_results_cl$mutation_change_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
                    filename=paste0(in.path,"/","Mutations_cl",cl,"_thr",input$ThrNtMutations, "_","nt","_","All Data",".txt")
                    write.table(mutation_results_nt_cl$mutation_change_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
                  }else{
                    if (input$AAorNtMutations=="aa") thr=input$ThrAAMutations else thr=input$ThrNtMutations
                    filename=paste0(in.path,"/","Mutations_cl",cl,"_thr",thr, "_","All Data",".txt")
                    write.table(mutation_results_cl$mutation_change_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
                  } 
                  
                }else{
                  if (input$AAorNtMutations=="both"){
                    filename=paste0(in.path,"/","Mutations_cl",cl,"_thr",input$ThrAAMutations, "_","aa","_",loaded_datasets[j],".txt")
                    write.table(mutation_results_cl$mutation_change_datasets[[loaded_datasets[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
                    filename=paste0(in.path,"/","Mutations_cl",cl,"_thr",input$ThrNtMutations,"_","nt","_",loaded_datasets[j],".txt")
                    write.table(mutation_results_nt_cl$mutation_change_datasets[[loaded_datasets[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
                  }else{
                    if (input$AAorNtMutations=="aa") thr=input$ThrAAMutations else thr=input$ThrNtMutations
                    filename=paste0(in.path,"/","Mutations_cl",cl,"_thr",thr,"_",loaded_datasets[j],".txt")
                    write.table(mutation_results_cl$mutation_change_datasets[[loaded_datasets[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
                  }
                  
                }
                #}
                
              }
            }
          }
          
          
        }
        
        ########################################### CDR3 1 length diff ######################################
        if (msgCDR3Diff1!="")
          for (j in 1:(length(loaded_datasets)+1)){
            if (j==(length(loaded_datasets)+1)){
              filename=paste0(in.path,"/","CDR3Diff1_","All Data",".txt")
              write.table(CDR3Diff1_results$cdr3_diff1P_allData, filename, sep = "\t", row.names = FALSE, col.names = TRUE)
            }else{
              filename=paste0(in.path,"/","CDR3Diff1_",loaded_datasets[j],".txt")
              write.table(CDR3Diff1_results$cdr3_diff1P_datasets[[loaded_datasets[j]]], filename, sep = "\t", row.names = FALSE, col.names = TRUE)
            }
          }
        
        
        ####### tar it
        tar(file,in.path)
        #tar(file,in.path, tar='internal')
        #suppressWarnings(tar(file,in.path, tar='internal'))
      })
    
    
    
    
    
    # }}) #login
    
  }
)

