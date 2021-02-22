library(shiny)
library(shinyFiles)
library(shinyjs)
library("shinyBS") 
library("DT") 
library(xtable)

#jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

source("helpers.R")

function(request) {   
  # Add custom CSS & Javascript;
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
  
  appError <- "
  .shiny-output-error { visibility: hidden; }
  .shiny-output-error:before {
  visibility: visible;
  content: 'An error occurred. Please contact the admin.'; }
  }
  "

  ##JS Code for enabling and diabling
  jscode <- "shinyjs.disabletab =function(name){
  $('ul li:has(a[data-value= ' + name + '])').addClass('disabled');
  $('.nav li.disabled a').prop('disabled',true)
  }

  shinyjs.enabletab =function(name){
  $('.nav li.disabled a').prop('disabled',false)
  $('ul li:has(a[data-value= ' + name + '])').removeClass('disabled');
  } " 
  
  navbarPage(
    #shinyjs::useShinyjs(),
    "ARGP Tool",
    id = "navbar",
    position = "fixed-top", 
    tags$style(type="text/css", ""),
    inverse=TRUE,
    
    header = tagList(
      useShinyjs()
      #extendShinyjs("www/app-shinyjs.js", functions = c("updateHistory"))
    ), 
    
    tabPanel("Home", value = "home", 
             tags$style(type="text/css", "body {padding-top: 70px;}"), #padding for navigation bar
             includeHTML("on_reload.html"),
             tags$head(tags$style(
               HTML('
                    #sidebar {
                      background-color: #ffffff;
                    }

                    body, label, input, button, select { 
                    font-family: "Arial";
                    }')
              )),
             
             tags$head(
               tags$style(HTML("
                               .multicol {
                               -height: 150px;
                               -webkit-column-count: 2; /* Chrome, Safari, Opera */ 
                               -moz-column-count: 2;    /* Firefox */ 
                               column-count: 2; 
                               -moz-column-fill: auto;
                               -column-fill: auto;
                               }
                               "))
               ),
             
             mainPanel( 
               width=9,
               h3("Import Data"),
               br()
             ),
             
             mainPanel(
               width = 9,   
               #fileInput("file1", "IMGT output", multiple = TRUE),
               textOutput("sourced"),
               
               #textInput("file_name", "File names"),
               #helpText("Separate the different file names with comma e.g. T3304,T3396"),
               
               br(),
               
               h4("Select the directory where the folders of the patients' data are located or load previous session"),
               shinyDirButton("dir", "Choose directory", "Upload"),
               
               br(),
               br(),
               h4("Or"),
               
               br(),
               actionButton("restorePreviousSession", "Restore Previous Session"),
               
               
               #br(),
               #br(),
               #actionButton("sessionRestored", "Restored Previous Session"),
               
               br(),
               br(),
               uiOutput('uiPreviousSessions'),
               uiOutput('uiLoadPreviousSessions'),
               
               br(),
               br(),
               uiOutput("uiInputFiles"),
               
               br(),
               uiOutput("uiDatasets"),
               
               br(),
               useShinyjs(),
               tags$style(appCSS),
               uiOutput("uiLoadData"),
               br(),
               uiOutput("confirmLoadData"),
               
               br(),
               br(),
               
               selectInput("cell", "T cell or B cell option:",c("T cell", "B cell"), width="170px"),
               
               br(),
               
               textOutput("testColumns"),
               
               br(),
               
               selectInput("throughput", "High or Low Throughput option:",c("High Throughput", "Low Throughput"), width="170px")
               
             ),
             
             mainPanel( 
               width = 9,
               uiOutput("uiMessage"),
               br()
             ),
             uiOutput("uiColumns"),
             
             textOutput("col_name"),
             
             
             br(),
             
             mainPanel( 
               width = 9,
               h3("Preselection"),
               br()
             ),
             
             sidebarPanel(
               width = 9,
               shinyjs::useShinyjs(),
               
               checkboxInput("Functional", c("Only take into account Functional V-Gene",
                                             list(tags$style(type = "text/css", "#q_Functional {vertical-align: top;}"),
                                                  bsButton("q_Functional", label = "", icon = icon("question"), size = "extra-small"))),
                             width="500px"),
               
               bsPopover(id = "q_Functional", title = "Functional V-Gene",
                         content = paste0("Only the sequences utilizing a functional V gene are going to be included in the analysis. Sequences that use V genes with the indication of pseudogenes (P) or open reading frame (ORF) are excluded from the analysis."
                         ),
                         placement = "right", 
                         trigger = "focus", 
                         options = list(container = "body")
               ),
               
               checkboxInput("Characters", c("Only take into account CDR3 with no Special Characters (X,*,#,.)",
                                             list(tags$style(type = "text/css", "#q_Characters {vertical-align: top;}"),
                                                  bsButton("q_Characters", label = "", icon = icon("question"), size = "extra-small"))),
                             width="500px"),
               
               bsPopover(id = "q_Characters", title = "Special Characters",
                         content = paste0("Sequences with characters other than the 20 amino acids are excluded from the analysis."
                         ),
                         placement = "right", 
                         trigger = "focus", 
                         options = list(container = "body")
               ),
               
               checkboxInput("Productive", c("Only take into account Productive Sequences",
                                             list(tags$style(type = "text/css", "#q_Productive {vertical-align: top;}"),
                                                  bsButton("q_Productive", label = "", icon = icon("question"), size = "extra-small"))),
                             width="500px"),
               
               bsPopover(id = "q_Productive", title = "Productive",
                         content = paste0("Only productive sequences and sequences with indel that do not cause frameshift are included in the analysis."
                         ),
                         placement = "right", 
                         trigger = "focus", 
                         options = list(container = "body")
               ),
               
               checkboxInput("start_end", c("Only take into account CDR3 with valid start/end landmarks",
                                            list(tags$style(type = "text/css", "#q_start_end {vertical-align: top;}"),
                                                 bsButton("q_start_end", label = "", icon = icon("question"), size = "extra-small"))),
                             width="500px"),
               
               bsPopover(id = "q_start_end", title = "CDR3 with valid start/end landmarks",
                         content = paste0("Start/end landmarks can be customized by the user. More than one valid landmark can be used. The different letters should be separated with | e.g. F|D. Sequences with landmarks other than the given are excluded from the analysis."
                         ),
                         placement = "right", 
                         trigger = "focus", 
                         options = list(container = "body")
               ),
               
               uiOutput("uiStart"),
               uiOutput("uiStart_comment"),
               uiOutput("uiEnd"),
               uiOutput("uiEnd_comment"),
               
               br(),
               
               useShinyjs(),
               tags$style(appCSS),
               # Wrap the button in the function `withBusyIndicatorUI()`
               withBusyIndicatorUI(
                 actionButton("Continue", "Apply",#icon("paper-plane"), 
                              style="color: #fff; background-color: #5F021F; border-color: #fff; hover: #B2577A")
               ),
               uiOutput("confirmCleaning")
               
             ), 
             
             mainPanel( 
               width = 9,
               h3("Selection"),
               br()
             ),
             
             sidebarPanel(
               width = 9,
               shinyjs::useShinyjs(),
               
               checkboxInput("identity", c("V-REGION identity %",
                                           list(tags$style(type = "text/css", "#q_identity {vertical-align: top;}"),
                                                bsButton("q_identity", label = "", icon = icon("question"), size = "extra-small"))),
                             width="500px"),
               
               bsPopover(id = "q_identity", title = "V-REGION identity %",
                         content = paste0("Sequences with identity percentage to germline that do not fall in the range set by the user at this filter are excluded from the analysis."
                         ),
                         placement = "right", 
                         trigger = "focus", 
                         options = list(container = "body")
               ),
               
               uiOutput("uiIdentityLow"),
               uiOutput("uiIdentityHigh"),
               
               checkboxInput("VGene", c("Select Specific V Gene",
                                        list(tags$style(type = "text/css", "#q_VGene {vertical-align: top;}"),
                                             bsButton("q_VGene", label = "", icon = icon("question"), size = "extra-small"))),
                             width="500px"),
               
               bsPopover(id = "q_VGene", title = "Specific V Gene",
                         content = paste0("The user can decide if the sequences to be kept in the analysis should use one or more particular V genes. The different genes should be separated with | e.g. TRBV11-2|TRBV29-1*03 (F)."
                         ),
                         placement = "right", 
                         trigger = "focus", 
                         options = list(container = "body")
               ),
               
               uiOutput("uiVGene"),
               uiOutput("uiVGene_comment"),
               
               checkboxInput("JGene", c("Select Specific J Gene",
                                        list(tags$style(type = "text/css", "#q_JGene {vertical-align: top;}"),
                                             bsButton("q_JGene", label = "", icon = icon("question"), size = "extra-small"))),
                             width="500px"),
               
               bsPopover(id = "q_JGene", title = "Specific J Gene",
                         content = paste0("??he user can decide if the sequences to be kept in the analysis should use one or more particular J genes. The different genes should be separated with | TRBJ2-6|TRBJ2-2."
                         ),
                         placement = "right", 
                         trigger = "focus", 
                         options = list(container = "body")
               ),
               
               uiOutput("uiJGene"),
               uiOutput("uiJGene_comment"),
               
               checkboxInput("DGene", c("Select Specific D Gene",
                                        list(tags$style(type = "text/css", "#q_DGene {vertical-align: top;}"),
                                             bsButton("q_DGene", label = "", icon = icon("question"), size = "extra-small"))),
                             width="500px"),
               
               bsPopover(id = "q_DGene", title = "Specific J Gene",
                         content = paste0("??he user can decide if the sequences to be kept in the analysis should use one or more particular D genes. The different genes should be separated with | e.g. TRBD2|TRBD1."
                         ),
                         placement = "right", 
                         trigger = "focus", 
                         options = list(container = "body")
               ),
               
               uiOutput("uiDGene"),
               uiOutput("uiDGene_comment"),
               
               checkboxInput("length", c("Select CDR3 length range",
                                         list(tags$style(type = "text/css", "#q_length {vertical-align: top;}"),
                                              bsButton("q_length", label = "", icon = icon("question"), size = "extra-small"))),
                             width="500px"),
               
               bsPopover(id = "q_length", title = "Length range",
                         content = paste0("Sequences with specific CDR3 lengths are included in the analysis."
                         ),
                         placement = "right", 
                         trigger = "focus", 
                         options = list(container = "body")
               ),
               
               uiOutput("uilengthLow"),
               uiOutput("uilengthHigh"),
               
               checkboxInput("aminoacid", c("Only select CDR3 containing specific amino-acid sequence",
                                            list(tags$style(type = "text/css", "#q_aminoacid {vertical-align: top;}"),
                                                 bsButton("q_aminoacid", label = "", icon = icon("question"), size = "extra-small"))),
                             width="500px"),
               
               bsPopover(id = "q_aminoacid", title = "CDR3 containing specific amino-acid sequence",
                         content = paste0("Sequences without the specific amino acid sequence defined by the user are excluded from the analysis."
                         ),
                         placement = "right", 
                         trigger = "focus", 
                         options = list(container = "body")
               ),
               
               uiOutput("uiAminoacid"),
               
               br(),
               
               useShinyjs(),
               tags$style(appCSS),
               uiOutput("uiExecute"),
               
               uiOutput("confirmCleaningFiltering"),
               
               uiOutput("confirmFiltering")
               
             )
    ),
    
    tabPanel("Preselection", value = "cleaningResults",
             mainPanel( 
               br(),
               br(),
               br(),
               br(),
               
               uiOutput('uiSelectDatasetCleaning'),
               br(),
               
               actionButton("summaryCleaning", "Summary", 
                            style="color: #fff; background-color: #5F021F; border-color: #fff"),
               
               conditionalPanel(
                 condition = "input.summaryCleaning % 2 == 1",
                 br(),
                 br(),
                 uiOutput('tableTitleCleaning'),
                 tableOutput('tableCleaning')
               ),
               br(),
               br(),
               
               actionButton("allDataCleaning", "All Data table", 
                            style="color: #fff; background-color: #5F021F; border-color: #fff"),
               
               conditionalPanel(
                 condition = "input.allDataCleaning % 2 == 1",
                 br(),
                 br(),
                 uiOutput('allDataInitialTableTitleCleaning'),
                 dataTableOutput('allDataInitialTableCleaning'),
                 downloadButton("downloadallDataInitialTableCleaning", "Download")
               ),
               br(),
               br(),
               
               actionButton("cleanTable", "Clean table", 
                            style="color: #fff; background-color: #5F021F; border-color: #fff"),
               
               conditionalPanel(
                 condition = "input.cleanTable % 2 == 1",
                 br(),
                 br(),
                 uiOutput('filterInTableTitleCleaning'),
                 dataTableOutput('filterInTableCleaning'),
                 downloadButton("downloadfilterInTableCleaning", "Download")
               ),
               br(),
               br(),
               
               actionButton("cleanOut", "Clean out table", 
                            style="color: #fff; background-color: #5F021F; border-color: #fff"),
               
               conditionalPanel(
                 condition = "input.cleanOut % 2 == 1",
                 br(),
                 br(),
                 uiOutput('filterOutTableTitleCleaning'),
                 dataTableOutput('filterOutTableCleaning'),
                 downloadButton("downloadfilterOutTableCleaning", "Download")
               ),
               br()
               
             )
    ),
    
    tabPanel("Selection", value = "filteringResults",
             mainPanel( 
               br(),
               br(),
               br(),
               br(),
               
               uiOutput('uiSelectDatasetFiltering'),
               br(),
               
               actionButton("summary", "Summary", 
                            style="color: #fff; background-color: #5F021F; border-color: #fff"),
               
               conditionalPanel(
                 condition = "input.summary % 2 == 1",
                 br(),
                 br(),
                 uiOutput('tableTitle'),
                 tableOutput('table')
               ),
               br(),
               br(),
               
               actionButton("allData", "All Data table", 
                            style="color: #fff; background-color: #5F021F; border-color: #fff"),
               
               conditionalPanel(
                 condition = "input.allData % 2 == 1",
                 br(),
                 br(),
                 uiOutput('allDataInitialTableTitle'),
                 dataTableOutput('allDataInitialTable'),
                 downloadButton("downloadAllDataInitialTable", "Download")
               ),
               br(),
               br(),
               
               actionButton("filterIn", "Filter in table", 
                            style="color: #fff; background-color: #5F021F; border-color: #fff"),
               
               conditionalPanel(
                 condition = "input.filterIn % 2 == 1",
                 br(),
                 br(),
                 uiOutput('filterInTableTitle'),
                 dataTableOutput('filterInTable'),
                 downloadButton("downloadfilterInTable", "Download")
               ),
               br(),
               br(),
               
               actionButton("filterOut", "Filter out table", 
                            style="color: #fff; background-color: #5F021F; border-color: #fff"),
               
               conditionalPanel(
                 condition = "input.filterOut % 2 == 1",
                 br(),
                 br(),
                 uiOutput('filterOutTableTitle'),
                 dataTableOutput('filterOutTable'),
                 downloadButton("downloadfilterOutTable", "Download")
               ),
               br()
               
             )
    ),
    
    tabPanel("Pipeline", value = "pipeline",
             mainPanel( 
               br(),
               br(),
               br(),
               br(),
               
               tags$style(type="text/css", appError),
               
               ###################################################################
               checkboxInput("pipeline_clonotypes", c("Clonotypes Computation",
                                                      list(tags$style(type = "text/css", "#q_clonotypes {vertical-align: top;}"),
                                                           bsButton("q_clonotypes", label = "", icon = icon("question"), size = "extra-small"))),
                             width="500px"),
               
               bsPopover(id = "q_clonotypes", title = "Clonotypes",
                         content = paste0("The clonotype frequencies of the samples are computed. There are 10 different clonotype definitions from which the user might choose. Convergent evolution of each clonotype is also computed, when possible. The results are presented at the Clonotypes tab as a table, where the clonotype, the counts, the frequency and the convergent evolution (when feasible) are given. Each clonotypes is also a link, leading to another table specific for the particular clonotype. This second table is consisted of the sequences assigned to that clonotypes with all the information about them."
                         ),
                         placement = "right", 
                         trigger = "focus", 
                         options = list(container = "body")
               ),
               
               conditionalPanel(
                 condition = "input.pipeline_clonotypes % 2 == 1",
                 #br(),
                 radioButtons("select_load_or_compute_clonotypes", "Select:",
                              c("Compute clonotypes" = "compute_clonotypes",
                                "Load clonotypes" = "load_clonotypes")),
                 
                 #conditionalPanel(
                 #condition = "input.select_load_or_compute_clonotypes == 'compute_clonotypes'",
                 selectInput("select_clonotype", "Select type:",c("V Gene + CDR3 Amino Acids","V Gene and Allele + CDR3 Amino Acids", 
                                                                  "V Gene + CDR3 Nucleotide","V Gene and Allele + CDR3 Nucleotide",
                                                                  "J Gene + CDR3 Amino Acids", "J Gene + CDR3 Nucleotide",
                                                                  "J Gene and Allele + CDR3 Amino Acids", "J Gene and Allele + CDR3 Nucleotide",
                                                                  "CDR3 Amino Acids", "CDR3 Nucleotide"), width="320")
                 #)
                 
               ),
               
               uiOutput("confirmClonotype"),
               
               ###################################################################
               checkboxInput("pipeline_highly_similar_clonotypes", c("Highly Similar Clonotypes computation",
                                                                     list(tags$style(type = "text/css", "#q_highly_similar_clonotypes {vertical-align: top;}"),
                                                                          bsButton("q_highly_similar_clonotypes", label = "", icon = icon("question"), size = "extra-small"))),
                             width="500px"),
               
               bsPopover(id = "q_highly_similar_clonotypes", title = "Highly Similar Clonotypes",
                         content = paste0("The clonotype frequencies of the highly similar clonotypes."
                         ),
                         placement = "right", 
                         trigger = "focus", 
                         options = list(container = "body")
               ),
               
               conditionalPanel(
                 condition = "input.pipeline_highly_similar_clonotypes % 2 == 1",
                 #br(),
                 uiOutput("select_highly_similar_clonotypes_parameters"),
                 uiOutput("confirmhighlySimClonotypes")
               ),
               
               
               ###################################################################
               conditionalPanel(
                 condition = "output.num_of_datasets>1 | input.select_load_or_compute_clonotypes == 'load_clonotypes'",
                 checkboxInput("pipeline_public_clonotypes", c("Shared Clonotypes Computation",
                                                               list(tags$style(type = "text/css", "#q_public_clonotypes {vertical-align: top;}"),
                                                                    bsButton("q_public_clonotypes", label = "", icon = icon("question"), size = "extra-small"))),
                               width="500px"),
                 
                 bsPopover(id = "q_public_clonotypes", title = "Shared  Clonotypes Computation",
                           content = paste0("The clonotypes that appear in more than one patients and their frequencies."
                           ),
                           placement = "right", 
                           trigger = "focus", 
                           options = list(container = "body")
                 ),
                 
                 conditionalPanel(
                   condition = "input.pipeline_public_clonotypes % 2 == 1",
                   #br(),
                   selectInput("take_gene_public_clono", "Take Gene into account:",c("No","Yes"), width="320"),
                   
                   radioButtons("select_topN_or_reads_thr_shared_clono", "Use:",
                                c("Select reads" = "select_reads_thr_shared_clono",
                                  "Select clonotype rank" = "select_topN_thr_shared_clono")),
                   
                   conditionalPanel(
                     condition = "input.select_topN_or_reads_thr_shared_clono == 'select_reads_thr_shared_clono'",
                     numericInput("thr_public_clono_reads", "Threshold for Clonotype reads:", 1,  min = 0, max = 100000000, width="140px")
                   ),
                   
                   conditionalPanel(
                     condition = "input.select_topN_or_reads_thr_shared_clono == 'select_topN_thr_shared_clono'",
                     numericInput("thr_public_clono_topN", "Threshold using clonotype rank:", 100,  min = 0, max = 1000000000000, width="140px")
                   ),
                   
                   uiOutput("confirmPublicClonotypes")
                 )
               ),
               
               
               ###################################################################
               conditionalPanel(
                 condition = "output.num_of_datasets>1 | input.select_load_or_compute_clonotypes == 'load_clonotypes'",
                 checkboxInput("pipeline_highly_sim_public_clonotypes", c("Highly Similar Shared Clonotypes Computation",
                                                                          list(tags$style(type = "text/css", "#q_highly_sim_public_clonotypes {vertical-align: top;}"),
                                                                               bsButton("q_highly_sim_public_clonotypes", label = "", icon = icon("question"), size = "extra-small"))),
                               width="500px"),
                 
                 bsPopover(id = "q_highly_sim_public_clonotypes", title = "Highly Similar Shared Clonotypes Computation",
                           content = paste0("The clonotypes that appear in more than one patients and their frequencies."
                           ),
                           placement = "right", 
                           trigger = "focus", 
                           options = list(container = "body")
                 ),
                 
                 conditionalPanel(
                   condition = "input.pipeline_highly_sim_public_clonotypes % 2 == 1",
                   #br(),
                   selectInput("take_gene_highly_sim_public_clono", "Take Gene into account:",c("No","Yes"), width="320"),
                   numericInput("thr_highly_sim_public_clono", "Threshold for Clonotype reads:", 1,  min = 0, max = 100000000, width="140px"),
                   uiOutput("confirmHighlySimPublicClonotypes")
                 )
               ),
               
               ###################################################################            
               checkboxInput("pipeline_Repertoires",  c("Repertoires Extraction",
                                                        list(tags$style(type = "text/css", "#q_Repertoires {vertical-align: top;}"),
                                                             bsButton("q_Repertoires", label = "", icon = icon("question"), size = "extra-small"))),
                             width="500px"),
               
               bsPopover(id = "q_Repertoires", title = "Repertoires",
                         content = paste0("The number of clonotypes using each V, J or D gene/allele is computed over the total number of clonotypes based on the definition given in the clonotype computation step. More than one repertoires can be computed at the same time. The results are presented at the Repertoires tab as tables. Each table includes the gene/allele, the counts and the frequency."
                         ),
                         placement = "right", 
                         trigger = "focus", 
                         options = list(container = "body")
               ),
               
               conditionalPanel(
                 condition = "input.pipeline_Repertoires % 2 == 1",
                 #br(),
                 actionButton("addRepertory", "Insert"),
                 actionButton("removeRepertory", "Remove"),
                 br(),
                 br(),
                 tags$div(id = "placeholderRepertories"),
                 #br(),
                 checkboxInput("pipeline_HighlySim_Repertoires",  c("Highly Similar Repertoires Extraction",
                                                                    list(tags$style(type = "text/css", "#q_Repertoires {vertical-align: top;}"),
                                                                         bsButton("q_HighlySim_Repertoires", label = "", icon = icon("question"), size = "extra-small"))),
                               width="500px"),
                 
                 bsPopover(id = "q_HighlySim_Repertoires", title = "HighlySim_Repertoires",
                           content = paste0("The number of clonotypes using each V, J or D gene is computed over the total number of the highly similar clonotypes."
                           ),
                           placement = "right", 
                           trigger = "focus", 
                           options = list(container = "body")
                 )
               ),
               
               uiOutput("confirmRepertoiresUi"),
               
               
               
               ###################################################################
               conditionalPanel(
                 condition = "output.num_of_datasets>1 | input.select_load_or_compute_clonotypes == 'load_clonotypes'",
                 checkboxInput("pipeline_repertoires_comparison", c("Repertoires Comparison",
                                                                    list(tags$style(type = "text/css", "#q_repertoires_comparison {vertical-align: top;}"),
                                                                         bsButton("q_repertoires_comparison", label = "", icon = icon("question"), size = "extra-small"))),
                               width="500px"),
                 
                 bsPopover(id = "q_repertoires_comparison", title = "Repertoires Comparison",
                           content = paste0("A summary table with genes as rows and the datasets as columns."
                           ),
                           placement = "right", 
                           trigger = "focus", 
                           options = list(container = "body")
                 ),
                 
                 uiOutput('confirmRepertoiresComparison')
               ),
               
               
               ###################################################################
               conditionalPanel(
                 condition = "input.cell == 'B cell'",
                 checkboxInput("pipeline_insert_identity_groups", c("Insert Identity groups",
                                                                    list(tags$style(type = "text/css", "#q_insert_identity_groups {vertical-align: top;}"),
                                                                         bsButton("q_insert_identity_groups", label = "", icon = icon("question"), size = "extra-small"))),
                               width="500px"),
                 
                 bsPopover(id = "q_insert_identity_groups", title = "Insert Identity groups",
                           content = paste0("sequences can be grouped in different categories based on the V-region identity %. The user can in this tool determine the number of different identity groups and the limits of every group. (high limit: <, low limit: >=)"
                           ),
                           placement = "right", 
                           trigger = "focus", 
                           options = list(container = "body")
                 ),
                 
                 conditionalPanel(
                   condition = "input.pipeline_insert_identity_groups % 2 == 1",
                   numericInput("N_identity_groups", "Number of identity groups:", 5,  min = 0, max = 150, width="140px"),
                   uiOutput("insert_identity_groups_ui")
                 ),
                 
                 uiOutput("confirmInsert_identity_groups")
               ),
               
               ###################################################################
               conditionalPanel(
                 condition = "input.cell == 'B cell'",
                 checkboxInput("pipeline_mutational_status",  c("Somatic hypermutation status",
                                                                list(tags$style(type = "text/css", "#q_Mutational_status {vertical-align: top;}"),
                                                                     bsButton("q_Mutational_status", label = "", icon = icon("question"), size = "extra-small"))),
                               width="500px"),
                 
                 bsPopover(id = "q_Mutational_status", title = "Somatic hypermutation status",
                           content = paste0("Compute the percentages of rows that belong to each identity group. The results are presented at Visualization tab."
                           ),
                           placement = "right", 
                           trigger = "focus", 
                           options = list(container = "body")
                 ),
                 
                 conditionalPanel(
                   condition = "input.pipeline_mutational_status % 2 == 1 & input.pipeline_highly_similar_clonotypes % 2 == 1",
                   radioButtons("select_clono_or_highly_for_mutational_status", "Use:",
                                c("Initial clonotypes" = "initial_clonotypes",
                                  "Highly similar clonotypes" = "highly_similar_clonotypes"))
                 ),
                 
                 uiOutput("confirmMutational_status")
               ),
               
               ###################################################################
               #conditionalPanel(
               #condition = "'1_Summary.txt' %in% input.inputFiles",
               checkboxInput("pipeline_cdr3_distribution",  c("CDR3 Distribution",
                                                              list(tags$style(type = "text/css", "#q_cdr3_distribution {vertical-align: top;}"),
                                                                   bsButton("q_cdr3_distribution", label = "", icon = icon("question"), size = "extra-small"))),
                             width="500px"),
               
               bsPopover(id = "q_cdr3_distribution", title = "CDR3 Distribution",
                         content = paste0("Compute the CDR3 Distribution over the initial or the highly similar clonotypes. The results are presented at Visualization tab."
                         ),
                         placement = "right", 
                         trigger = "focus", 
                         options = list(container = "body")
               ),
               
               conditionalPanel(
                 condition = "input.pipeline_cdr3_distribution % 2 == 1 & input.pipeline_highly_similar_clonotypes % 2 == 1",
                 radioButtons("select_clono_or_highly_for_cdr3_distribution", "Use:",
                              c("Initial clonotypes" = "initial_clonotypes",
                                "Highly similar clonotypes" = "highly_similar_clonotypes"))
               ),
               #),
               
               ###################################################################
               #conditionalPanel(
               #condition = "'6_Junction.txt' %in% input.inputFiles",
               checkboxInput("pipeline_pi_distribution",  c("Pi Distribution",
                                                            list(tags$style(type = "text/css", "#q_pi_distribution {vertical-align: top;}"),
                                                                 bsButton("q_pi_distribution", label = "", icon = icon("question"), size = "extra-small"))),
                             width="500px"),
               
               bsPopover(id = "q_pi_distribution", title = "Pi Distribution",
                         content = paste0("Compute the Pi Distribution over the initial or the highly similar clonotypes. The results are presented at Visualization tab."
                         ),
                         placement = "right", 
                         trigger = "focus", 
                         options = list(container = "body")
               ),
               
               conditionalPanel(
                 condition = "input.pipeline_pi_distribution % 2 == 1 & input.pipeline_highly_similar_clonotypes % 2 == 1",
                 radioButtons("select_clono_or_highly_for_pi_distribution", "Use:",
                              c("Initial clonotypes" = "initial_clonotypes",
                                "Highly similar clonotypes" = "highly_similar_clonotypes"))
               ),
               #),
               
               ###################################################################
               checkboxInput("pipeline_Multiple_value_comparison",  c("Multiple value comparison",
                                                                      list(tags$style(type = "text/css", "#q_Multiple_value_comparison {vertical-align: top;}"),
                                                                           bsButton("q_Multiple_value_comparison", label = "", icon = icon("question"), size = "extra-small"))),
                             width="500px"),
               
               bsPopover(id = "q_Multiple_value_comparison", title = "Multiple value comparison",
                         content = paste0("The number of the combination of the two selected variables is computed. Many different combinations can be selected by the user to be created. The possible combinations depend on the selected input files from the Home tab. The results are presented at the Multiple value comparison tab in tables. Each table gives the values that are found to be associated and how many times."
                         ),
                         placement = "right", 
                         trigger = "focus", 
                         options = list(container = "body")
               ),
               
               conditionalPanel(
                 condition = "input.pipeline_Multiple_value_comparison % 2 == 1 & input.pipeline_highly_similar_clonotypes % 2 == 1",
                 radioButtons("select_clono_or_highly_for_Multiple_value_comparison", "Use:",
                              c("Initial clonotypes" = "initial_clonotypes",
                                "Highly similar clonotypes" = "highly_similar_clonotypes")),
                 br()
               ),
               
               conditionalPanel(
                 condition = "input.pipeline_Multiple_value_comparison % 2 == 1 ",
                 actionButton('insertBtnMultiple_value_comparison', 'Insert'), 
                 actionButton('removeBtnMultiple_value_comparison', 'Remove'), 
                 br(),
                 br(),
                 tags$div(id = 'placeholder')
               ),
               
               uiOutput("confirmMultiple_value_comparison"),
               
               useShinyjs(),
               tags$style(appCSS),
               
               ###################################################################
               uiOutput("uiExecute_pipeline"),
               br(),
               br(),
               
               ###################################################################
               checkboxInput("pipeline_CDR3Diff1",  c("CDR3 with 1 length difference",
                                                      list(tags$style(type = "text/css", "#q_CDR3_1length_diff {vertical-align: top;}"),
                                                           bsButton("q_CDR3_1length_diff", label = "", icon = icon("question"), size = "extra-small"))),
                             width="500px"),
               
               bsPopover(id = "q_CDR3_1length_diff", title = "CDR3 with 1 length difference",
                         content = paste0("Groups of similar CDR3 sequences are presented. In this case, two CDR3 sequences are called similar when their lengths differ by 1 and if we add an amino acid at a specific position of the sorter sequence, we will get the same sequence."
                         ),
                         placement = "right", 
                         trigger = "focus", 
                         options = list(container = "body")
               ),
               
               conditionalPanel(
                 condition = "input.pipeline_CDR3Diff1 % 2 == 1",
                 
                 #uiOutput("uiSelectGene1Diff"),
                 
                 numericInput("cdr3MaxLength1Diff", "Select max CDR3 length:", 12,  min = 0, max = 150, width="140px"),
                 
                 numericInput("cdr3Position1Diff", "Select CDR3 position with difference:", 8,  min = 0, max = 150, width="140px")
               ),
               
               uiOutput("confirmCDR3Diff1"),
               
               ###################################################################
               conditionalPanel(
                 condition = "input.cell == 'B cell'",
                 checkboxInput("pipeline_alignment", c("Alignment",
                                                       list(tags$style(type = "text/css", "#q_Alignment {vertical-align: top;}"),
                                                            bsButton("q_Alignment", label = "", icon = icon("question"), size = "extra-small"))),
                               width="500px"),
                 
                 bsPopover(id = "q_Alignment", title = "Alignment",
                           content = paste0("An alignment table is computed for the selected region (VDJ REGION, VJ REGION). A grouped alignment table is computed as well. The table is created by grouping the alignment table by the corresponding region that is used. The selected region can be aligned at nucleotide level, at amino acid level or both. For the alignment the reference sequences used by default are according to IMGT (http://www.imgt.org/vquest/refseqh.html). The reference sequences used can be at allele level or at gene level. At gene level, the allele *01 of every gene is considered as reference. The user can also insert their own reference sequence."
                           ),
                           placement = "right", 
                           trigger = "focus", 
                           options = list(container = "body")
                 ),
                 conditionalPanel(
                   condition = "input.pipeline_alignment % 2 == 1",
                   selectInput("regionAlignment", "Region for Alignment:",c("V.D.J.REGION","V.J.REGION"), width="170px"),
                   
                   selectInput("AAorNtAlignment", "AA or Nt:",c("aa","nt","both"), width="170px"),
                   
                   #numericInput("MaxLengthRegion", "Max length of region:", 127,  min = 0, max = 150, width="140px"),
                   
                   selectInput("useGermline", "Germline:",c("Use Allele's germline","Use Gene's germline", "Insert Germline"), width="180px"),
                   
                   conditionalPanel(
                     condition = "input.useGermline  == 'Insert Germline'",
                     textInput("Germline", "Insert Germline")
                   ),
                   
                   radioButtons("select_topN_clonotypes_for_alignment", "Use:",
                                c("All clonotypes" = "all_clonotypes",
                                  "Select top N clonotypes" = "topN_clonotypes_for_alignment",
                                  "Select threshold for clonotypes" = "thr_clonotypes_for_alignment")),
                   
                   conditionalPanel(
                     condition = "input.select_topN_clonotypes_for_alignment == 'topN_clonotypes_for_alignment'",
                     numericInput("topNClonoAlignment", "Select N", 20,  min = 1, max = 10000000000000, width="140px")
                   ),
                   
                   conditionalPanel(
                     condition = "input.select_topN_clonotypes_for_alignment == 'thr_clonotypes_for_alignment'",
                     numericInput("thrClonoAlignment", "Select Threshold %. Range[0,100]", 0.1,  min = 1, max = 100, width="140px")
                   )
                   
                   #checkboxInput("select_clonotypes_for_alignment", "Select clonotypes separately", width="500px"),
                   
                   #conditionalPanel(
                   #condition = "input.select_clonotypes_for_alignment  % 2 == 1",
                   #textInput("clonotypes_for_alignment", "cluster ids"),
                   #helpText("Separate the different cluster ids with comma e.g. 1,2,4")
                   #),
                   #br()
                   
                 ),
                 
                 uiOutput("confirmAlignment"),
                 uiOutput("confirmGroupedAlignment")
                 
               ),
               
               ###################################################################
               conditionalPanel(
                 condition = "input.cell == 'B cell'",
                 checkboxInput("pipeline_mutations", c("Somatic hypermutations",
                                                       list(tags$style(type = "text/css", "#q_Mutations {vertical-align: top;}"),
                                                            bsButton("q_Mutations", label = "", icon = icon("question"), size = "extra-small"))),
                               width="500px"),
                 
                 bsPopover(id = "q_Mutations", title = "Somatic hypermutations",
                           content = paste0("A table with the mutations based on Alignment table"
                           ),
                           placement = "right", 
                           trigger = "focus", 
                           options = list(container = "body")
                 ),
                 conditionalPanel(
                   condition = "input.pipeline_mutations % 2 == 1",
                   selectInput("AAorNtMutations", "AA or Nt:",c("aa","nt","both"), width="170px"),
                   
                   #numericInput("MaxLengthRegion", "Max length of region:", 127,  min = 0, max = 150, width="140px"),
                   conditionalPanel(
                     condition = "input.AAorNtMutations  == 'aa' | input.AAorNtMutations  == 'both'",
                     numericInput("ThrAAMutations", "Select threshold for AA: range [0,1]", 0.5,  min = 0, max = 1, width="140px")
                   ),
                   conditionalPanel(
                     condition = "input.AAorNtMutations  == 'nt' | input.AAorNtMutations  == 'both'",
                     numericInput("ThrNtMutations", "Select threshold for Nt range [0,1]:", 0.5,  min = 0, max = 1, width="140px")
                   ),
                   
                   radioButtons("select_topN_clonotypes_for_mutations", "Use:",
                                c("All clonotypes" = "all_clonotypes",
                                  "Select top N clonotypes" = "topN_clonotypes_for_mutations",
                                  "Select threshold for clonotypes" = "thr_clonotypes_for_mutations")),
                   
                   conditionalPanel(
                     condition = "input.select_topN_clonotypes_for_mutations == 'topN_clonotypes_for_mutations'",
                     numericInput("topNClonoMutations", "Select N", 20,  min = 1, max = 10000000000000, width="140px")
                   ),
                   
                   conditionalPanel(
                     condition = "input.select_topN_clonotypes_for_mutations == 'thr_clonotypes_for_mutations'",
                     numericInput("thrClonoMutations", "Select Threshold %. Range[0,100]", 0.1,  min = 1, max = 100, width="140px")
                   ),
                   
                   checkboxInput("select_clonotypes_for_mutations", "Select clonotypes separately", width="500px"),
                   
                   conditionalPanel(
                     condition = "input.select_clonotypes_for_mutations  % 2 == 1",
                     textInput("clonotypes_for_mutations", "cluster ids"),
                     helpText("Separate the different cluster ids with comma e.g. 1,2,4")
                   ),
                   br()
                 ),
                 
                 uiOutput("confirmMutations")#,
                 #uiOutput("confirmGroupedAlignment")
                 
               ),
               
               
               ###################################################################
               checkboxInput("pipeline_logo", c("Logo",
                                                list(tags$style(type = "text/css", "#q_Logo {vertical-align: top;}"),
                                                     bsButton("q_Logo", label = "", icon = icon("question"), size = "extra-small"))),
                             width="500px"),
               
               bsPopover(id = "q_Logo", title = "Logo",
                         content = paste0("At this tool a frequency table is created for the selected region (CDR3, VDJ REGION, VJ REGION) of a specific length. The frequency table is computed by counting the appearance of the 20 different amino acids at each position of the sequence. The user has the option to select if he wants the total frequency table to be created or the table of the top N clusters according to the clonotype frequencies. A logo is created using the above frequency table. The color code of the amino acids is created based on IMGT. (http://www.imgt.org/IMGTeducation/Aide-memoire/_UK/aminoacids/IMGTclasses.html)"
                         ),
                         placement = "right", 
                         trigger = "focus", 
                         options = list(container = "body")
               ),
               
               conditionalPanel(
                 condition = "input.pipeline_logo % 2 == 1",
                 #br(),
                 tags$div(id = 'placeholder'),
                 
                 selectInput("regionFreqTable", "Region for Frequency Table:",c( "CDR3","V.D.J.REGION","V.J.REGION"), width="170px"),
                 
                 numericInput("regionLengthFreq", "Select region length:", 12,  min = 0, max = 150, width="140px"),
                 
                 radioButtons("select_topN_clonotypes_for_freqTable", "Use:",
                              c("All clonotypes" = "all_clonotypes",
                                "Select top N clonotypes" = "topN_clonotypes_for_logos",
                                "Select threshold for clonotypes" = "thr_clonotypes_for_logos")),
                 
                 conditionalPanel(
                   condition = "input.select_topN_clonotypes_for_freqTable == 'topN_clonotypes_for_logos'",
                   numericInput("topNClonoLogos", "Select N", 20,  min = 1, max = 10000000000000, width="140px")
                 ),
                 
                 conditionalPanel(
                   condition = "input.select_topN_clonotypes_for_freqTable == 'thr_clonotypes_for_logos'",
                   numericInput("thrClonoLogos", "Select Threshold %. Range[0,100]", 0.1,  min = 1, max = 100, width="140px")
                 ),
                 
                 checkboxInput("select_clonotypes_for_logo", "Select clonotypes separately", width="500px"),
                 
                 conditionalPanel(
                   condition = "input.select_clonotypes_for_logo  % 2 == 1",
                   textInput("clonotypes_for_logo", "cluster ids"),
                   helpText("Separate the different cluster ids with comma e.g. 1,2,4")
                 ),
                 br()
                 
               ), 
               
               uiOutput("confirmFrequenciesTables"),
               
               conditionalPanel(
                 condition = "input.pipeline_logo % 2 == 1",
                 tags$div(id = 'placeholder')
               ),
               
               uiOutput("confirmLogo"),
               
               useShinyjs(),
               tags$style(appCSS),
               # Wrap the button in the function `withBusyIndicatorUI()`
               conditionalPanel(
                 condition = "input.Execute_pipeline % 2 == 1",
                 withBusyIndicatorUI(
                   actionButton("Execute_pipeline_2nd_part", "Execute pipeline 2nd part",#icon("paper-plane"), 
                                style="color: #fff; background-color: #5F021F; border-color: #fff; hover: #B2577A")
                 )
               )
               
             )
    ),
    
    tabPanel("Clonotypes", value = "Clonotypes",
             mainPanel(
               br(),
               br(),
               br(),
               br(),
               
               uiOutput('uiSelectDatasetClonotypes'),
               br(),
               
               actionButton("clonotypes", "View Clonotypes", 
                            style="color: #fff; background-color: #5F021F; border-color: #fff"),
               
               conditionalPanel(
                 condition = "input.clonotypes % 2 == 1",
                 br(),
                 br(),
                 #uiOutput('clonoTitle'),
                 dataTableOutput('clonoTable'),
                 
                 #The pop-up window
                 bsModal("modalViewSpecificClonotype", "Elements of the clonotype", "", size = "large",DT::dataTableOutput('viewSpecificClonotype'),
                         downloadButton('downloadElementsOfClonotype', 'Download')),
                 #The pop-up window
                 bsModal("modalViewConvergentEvolution", "Elements of convergent evolution", "", size = "large",DT::dataTableOutput('viewSpecificConvergentEvolution'),
                         downloadButton('downloadConvergentEvolution', 'Download')),
                 downloadButton("downloadAllClonotypes", "Download")
                 
               ),
               br(),
               
               br(),
               
               conditionalPanel(
                 condition = "input.pipeline_public_clonotypes == 1",
                 br(),
                 br(),
                 actionButton("public_clonotypesBtn", "View Public Clonotypes", 
                              style="color: #fff; background-color: #5F021F; border-color: #fff"),
                 
                 conditionalPanel(
                   condition = "input.public_clonotypesBtn % 2 == 1",
                   br(),
                   br(),
                   dataTableOutput('public_clonotypes_Table'),
                   br(),
                   downloadButton('downloadPublic_clonotypes')
                 )
               )
             )    
    ),
    
    tabPanel("Highly Similar Clonotypes", value = "highly_similar_clonotypes",
             mainPanel(
               br(),
               br(),
               br(),
               br(),
               
               uiOutput('uiSelectDatasethighly_similar_clonotypes'),
               br(),
               
               
               actionButton("highly_similar_all_clonotypes_btn", "View highly similar clonotypes", 
                            style="color: #fff; background-color: #5F021F; border-color: #fff"),
               
               conditionalPanel(
                 condition = "input.highly_similar_all_clonotypes_btn % 2 == 1",
                 br(),
                 br(),
                 #uiOutput('clonoTitle'),
                 dataTableOutput('highlySimAllClonoTable'),
                 br(),
                 downloadButton("downloadHighlySimAllClonoTable", "Download"),
                 br(),
                 br()
               ),
               br(),
               br(),
               
               uiOutput('select_length_to_show_higlySimClono_ui'),
               br(),
               br(),
               
               actionButton("highly_similar_clonotypes_btn", "View highly similar clonotypes per length", 
                            style="color: #fff; background-color: #5F021F; border-color: #fff"),
               
               conditionalPanel(
                 condition = "input.highly_similar_clonotypes_btn % 2 == 1",
                 br(),
                 br(),
                 #uiOutput('clonoTitle'),
                 dataTableOutput('highlySimClonoTable'),
                 br(),
                 downloadButton("downloadAllhighlySimClonotypes", "Download"),
                 br(),
                 br(),
                 
                 dataTableOutput('highlySimClono_allGroups_Table'),
                 br(),
                 downloadButton('downloadAllhighlySimClonotypes_allGroups')
                 
                 #plotOutput("clonotypes_bar_plot")
                 
               ),
               br(),
               br(),
               br(),
               
               conditionalPanel(
                 condition = "input.pipeline_highly_sim_public_clonotypes == 1",
                 br(),
                 br(),
                 actionButton("highly_sim_public_clonotypesBtn", "View Highly Similar Public Clonotypes", 
                              style="color: #fff; background-color: #5F021F; border-color: #fff"),
                 
                 conditionalPanel(
                   condition = "input.highly_sim_public_clonotypesBtn % 2 == 1",
                   br(),
                   br(),
                   dataTableOutput('highly_sim_public_clonotypes_Table'),
                   br(),
                   downloadButton('download_highly_sim_Public_clonotypes')
                 )
               )
             )    
    ),
    
    tabPanel("Repertoires", value = "Repertoires",
             mainPanel(
               br(),
               br(),
               br(),
               br(),
               
               uiOutput('uiSelectDatasetRepertoires'),
               br(),
               
               actionButton("RepertoiresBtn", "View Repertoires", 
                            style="color: #fff; background-color: #5F021F; border-color: #fff"),
               
               conditionalPanel(
                 condition = "input.RepertoiresBtn % 2 == 1",
                 br(),
                 br(),
                 uiOutput('RepertoiresResultUi')
               ),
               br(),
               br()
             ),
             mainPanel(
               conditionalPanel(
                 condition = "input.pipeline_HighlySim_Repertoires == 1",
                 actionButton("HighlySim_RepertoiresBtn", "View Highly Similar Repertoires", 
                              style="color: #fff; background-color: #5F021F; border-color: #fff"),
                 
                 conditionalPanel(
                   condition = "input.HighlySim_RepertoiresBtn % 2 == 1",
                   br(),
                   br(),
                   uiOutput('HighlySim_RepertoiresResultUi')
                 ),
                 br(),
                 br()
               )
             ),
             mainPanel(
               conditionalPanel(
                 condition = "input.pipeline_repertoires_comparison == 1",
                 br(),
                 br(),
                 actionButton("repertoires_comparisonBtn", "View Repertoires comparison", 
                              style="color: #fff; background-color: #5F021F; border-color: #fff"),
                 
                 conditionalPanel(
                   condition = "input.repertoires_comparisonBtn % 2 == 1",
                   br(),
                   br(),
                   uiOutput('Repertoires_comparisonResultUi')
                 )
               )
             ),
             
             br(),
             br(),
             br(),
             mainPanel(
               br(),
               actionButton("Highly_sim_repertoires_comparisonBtn", "View Highly Similar Repertoires comparison", 
                            style="color: #fff; background-color: #5F021F; border-color: #fff"),
               
               conditionalPanel(
                 condition = "input.Highly_sim_repertoires_comparisonBtn % 2 == 1",
                 br(),
                 br(),
                 uiOutput('Highly_sim_Repertoires_comparisonResultUi')
               )
             ),
             
             br(),
             br()
             
    ),
    
    tabPanel("Multiple value comparison", value = "Multiple_value_comparisonTab",
             mainPanel(
               br(),
               br(),
               br(),
               br(),
               
               uiOutput('uiSelectDatasetMultiple_value_comparison'),
               br(),
               actionButton("Multiple_value_comparisonBtn", "View Multiple value comparison", 
                            style="color: #fff; background-color: #5F021F; border-color: #fff"),
               
               conditionalPanel(
                 condition = "input.Multiple_value_comparisonBtn % 2 == 1",
                 br(),
                 br(),
                 uiOutput('uiMultiple_value_comparisonTables')
                 #downloadButton("downloadMultiple_value_comparison", "Download")
                 
               ),
               br()
             )
    ),
    
    tabPanel("CDR3 1 Length Diff", value = "CDR3_with_1_length_difference_tab",
             mainPanel(
               br(),
               br(),
               br(),
               br(),
               
               uiOutput('uiSelectDatasetCDR3Diff1'),
               br(),
               
               actionButton("CDR3Diff1Button", "View CDR3Diff1", 
                            style="color: #fff; background-color: #5F021F; border-color: #fff"),
               
               conditionalPanel(
                 condition = "input.CDR3Diff1Button % 2 == 1",
                 br(),
                 br(),
                 dataTableOutput('CDR3Diff1Table'),
                 downloadButton("downloadCDR3Diff1Table", "Download")
                 
               ),
               br()
             )
    ),
    
    tabPanel("Alignment", value = "Alignment",
             mainPanel(
               br(),
               br(),
               br(),
               br(),
               
               uiOutput('uiSelectDatasetAlignment'),
               br(),
               
               actionButton("alignmentButton", "View Alignment", 
                            style="color: #fff; background-color: #5F021F; border-color: #fff"),
               
               conditionalPanel(
                 condition = "input.alignmentButton % 2 == 1",
                 br(),
                 br(),
                 dataTableOutput('regionAlignmentTable'),
                 downloadButton("downloadregionAlignmentTable", "Download")
               ),
               br(),
               
               br(),
               actionButton("groupedAlignmentButton", "View Grouped Alignment", 
                            style="color: #fff; background-color: #5F021F; border-color: #fff"),
               
               conditionalPanel(
                 condition = "input.groupedAlignmentButton % 2 == 1 ",
                 br(),
                 br(),
                 dataTableOutput('groupedAlignmentTable'),
                 downloadButton("downloadGroupedAlignmentTable", "Download")
               ),
               br(),
               br(),
               br(),
               
               conditionalPanel(
                 condition = "input.AAorNtAlignment == 'both'",
                 actionButton("alignmentButtonNt", "View Alignment nt", 
                              style="color: #fff; background-color: #5F021F; border-color: #fff"),
                 
                 conditionalPanel(
                   condition = "input.alignmentButtonNt % 2 == 1",
                   br(),
                   br(),
                   dataTableOutput('regionAlignmentTableNt'),
                   downloadButton("downloadregionAlignmentTableNt", "Download")
                 ),
                 br(),
                 
                 br(),
                 actionButton("groupedAlignmentButtonNt", "View Grouped Alignment nt", 
                              style="color: #fff; background-color: #5F021F; border-color: #fff"),
                 
                 conditionalPanel(
                   condition = "input.groupedAlignmentButtonNt % 2 == 1 ",
                   br(),
                   br(),
                   dataTableOutput('groupedAlignmentTableNt'),
                   downloadButton("downloadGroupedAlignmentTableNt", "Download")
                 ),
                 br()
               )
               
             )
    ),
    
    tabPanel("Somatic hypermutations", value = "Mutation_tab",
             mainPanel(
               br(),
               br(),
               br(),
               br(),
               
               uiOutput('uiSelectDatasetMutation'),
               br(),
               
               uiOutput('uiSelectGeneMutation'),
               br(),
               
               actionButton("MutationButton", "View Mutation Table", 
                            style="color: #fff; background-color: #5F021F; border-color: #fff"),
               
               conditionalPanel(
                 condition = "input.MutationButton % 2 == 1",
                 br(),
                 br(),
                 dataTableOutput('MutationTable'),
                 downloadButton("downloadMutationTable", "Download")
               ),
               br(),
               br(),
               br(),
               
               conditionalPanel(
                 condition = "input.AAorNtMutations == 'both'",
                 actionButton("MutationButtonNt", "View Mutation nt", 
                              style="color: #fff; background-color: #5F021F; border-color: #fff"),
                 
                 conditionalPanel(
                   condition = "input.MutationButtonNt % 2 == 1",
                   br(),
                   br(),
                   dataTableOutput('MutationTableNt'),
                   downloadButton("downloadMutationTableNt", "Download")
                 ),
                 br()
               ),
               
               br(),
               
               conditionalPanel(
                 condition = "input.select_clonotypes_for_mutations % 2 == 1",
                 actionButton("ViewMutationsPerCluster", "View Mutations per cluster", 
                              style="color: #fff; background-color: #5F021F; border-color: #fff"),
                 
                 conditionalPanel(
                   condition = "input.ViewMutationsPerCluster % 2 == 1",
                   br(),
                   br(),
                   uiOutput("uiMutationTables_cl")
                 ),
                 br()
               )
               
               
             )
    ),
    
    tabPanel("Logo", value = "logo_tab",
             mainPanel(
               br(),
               br(),
               br(),
               br(),
               
               uiOutput('uiSelectDatasetFreqTable'),
               br(),
               
               actionButton("countTable", "View Count Table", 
                            style="color: #fff; background-color: #5F021F; border-color: #fff"),
               
               conditionalPanel(
                 condition = "input.countTable % 2 == 1",
                 br(),
                 br(),
                 #uiOutput('countCDR3TableTitle'),
                 dataTableOutput('countCDR3Table'),
                 downloadButton("downloadcountCDR3Table", "Download")
               ),
               br(),
               br(),
               
               actionButton("freqTable", "View Frequency Table", 
                            style="color: #fff; background-color: #5F021F; border-color: #fff"),
               
               conditionalPanel(
                 condition = "input.freqTable % 2 == 1",
                 br(),
                 br(),
                 #uiOutput('frequencyCDR3TableTitle'),
                 dataTableOutput('frequencyCDR3Table'),
                 downloadButton("downloadfrequencyCDR3Table", "Download")
               ),
               br(),
               br(),
               
               conditionalPanel(
                 condition = "input.select_clonotypes_for_logo % 2 == 1",
                 actionButton("countFreqTableSep", "View Tables for selected clusters separately", 
                              style="color: #fff; background-color: #5F021F; border-color: #fff"),
                 
                 conditionalPanel(
                   condition = "input.countFreqTableSep % 2 == 1",
                   br(),
                   br(),
                   uiOutput("uiCountCDR3Table_cl")
                 ),
                 br(),
                 br()
               )
               
             ),
             
             mainPanel(
               br(),
               br(),
               br(),
               br(),
               
               uiOutput('uiSelectDatasetLogo'),
               br(),
               
               actionButton("logoButton", "View Logo", 
                            style="color: #fff; background-color: #5F021F; border-color: #fff"),
               
               conditionalPanel(
                 condition = "input.logoButton % 2 == 1",
                 conditionalPanel(
                   condition = "input.regionFreqTable != 'CDR3'",
                   br(),
                   selectInput("select_region_logo", "Select region:",c("All V region","FR1-IMGT","CDR1-IMGT","FR2-IMGT","CDR2-IMGT","FR3-IMGT","CDR3-IMGT"), width="200")
                 ),
                 br(),
                 br(),
                 plotOutput('logo'),
                 downloadButton("downloadLogo", "Download")
                 
               ),
               br(),
               
               conditionalPanel(
                 condition = "input.select_clonotypes_for_logo % 2 == 1",
                 actionButton("LogosSep", "View Logos for selected clusters separately", 
                              style="color: #fff; background-color: #5F021F; border-color: #fff"),
                 
                 conditionalPanel(
                   condition = "input.LogosSep % 2 == 1",
                   br(),
                   br(),
                   uiOutput("uiLogos_cl")
                 ),
                 br(),
                 br()
               )
               
             ) 
    ),
    
    
    tabPanel("Visualisation", value = "Visualisation",
             mainPanel(
               br(),
               br(),
               br(),
               br(),
               
               ###################################################################
               uiOutput('uiSelectDatasetVisualisation'),
               br(),
               
               ###################################################################
               conditionalPanel(
                 condition = "input.pipeline_clonotypes == 1",
                 #plotOutput("clonotypes_bar_plot_visualisation"),
                 actionButton("view_clonotype_bar", "View Clonotype bar plots", 
                              style="color: #fff; background-color: #5F021F; border-color: #fff"),
                 
                 br(),
                 
                 conditionalPanel(
                   condition = "input.view_clonotype_bar % 2 == 1",
                   br(),
                   br(),  
                   
                   numericInput("clonotypes_barchart_threshold", "Select %threshold for bar chart:", 0.5,  min = 0, max = 100, width="140px"),
                   h4("Or"),
                   
                   checkboxInput("clonotypes_barplot_select_range", "Select range of clusters to show", width="500px"),
                   
                   conditionalPanel(
                     condition = "input.clonotypes_barplot_select_range % 2 == 1",
                     numericInput("clonotypes_barchart_down_threshold", "From cluster:", 1,  min = 0, max = 100, width="140px"),
                     numericInput("clonotypes_barchart_up_threshold", "To cluster:", 1,  min = 0, max = 10000000, width="140px")
                   ),
                   
                   br(),
                   plotOutput("clonotypes_bar_plot")
                 ),
                 
                 br()
               ),
               
               
               
               ###################################################################
               conditionalPanel(
                 condition = "input.pipeline_highly_similar_clonotypes == 1",
                 #plotOutput("clonotypes_bar_plot_visualisation"),
                 actionButton("view_higly_sim_clonotype_bar", "View Highly Similar Clonotype bar plots", 
                              style="color: #fff; background-color: #5F021F; border-color: #fff"),
                 
                 br(),
                 
                 conditionalPanel(
                   condition = "input.view_higly_sim_clonotype_bar % 2 == 1",
                   br(),
                   br(),  
                   
                   numericInput("higly_sim_clonotypes_barchart_threshold", "Select %threshold for bar chart:", 0.5,  min = 0, max = 100, width="140px"),
                   h4("Or"),
                   
                   checkboxInput("higly_sim_clonotypes_barplot_select_range", "Select range of clusters to show", width="500px"),
                   
                   conditionalPanel(
                     condition = "input.higly_sim_clonotypes_barplot_select_range % 2 == 1",
                     numericInput("higly_sim_clonotypes_barchart_down_threshold", "From cluster:", 1,  min = 0, max = 100, width="140px"),
                     numericInput("higly_sim_clonotypes_barchart_up_threshold", "To cluster:", 1,  min = 0, max = 10000000, width="140px")
                   ),
                   
                   br(),
                   plotOutput("higly_sim_clonotypes_bar_plot")
                 ),
                 
                 br()
               ),
               
               ###################################################################
               conditionalPanel(
                 condition = "input.pipeline_Repertoires == 1",
                 actionButton("view_repertoire_pies", "View Repertoire pies", 
                              style="color: #fff; background-color: #5F021F; border-color: #fff"),
                 
                 br(),
                 
                 conditionalPanel(
                   condition = "input.view_repertoire_pies % 2 == 1",
                   br(),
                   uiOutput("RepertoiresPiesUi")
                 ),
                 
                 br()
               ),
               
               ###################################################################
               conditionalPanel(
                 condition = "input.pipeline_Repertoires == 1 & input.pipeline_HighlySim_Repertoires == 1",
                 actionButton("view_highly_sim_repertoire_pies", "View Highly Similar Repertoire pies", 
                              style="color: #fff; background-color: #5F021F; border-color: #fff"),
                 
                 br(),
                 
                 conditionalPanel(
                   condition = "input.view_highly_sim_repertoire_pies % 2 == 1",
                   br(),
                   uiOutput("HighlySim_RepertoiresPiesUi")
                 ),
                 
                 br()
               ),
               
               ###################################################################
               #conditionalPanel(
               #condition = "input.pipeline_Multiple_value_comparison == 1",
               #actionButton("view_multiple_value_plots", "View Multiple value plots", 
               #style="color: #fff; background-color: #5F021F; border-color: #fff"),
               
               #br(),
               
               #conditionalPanel(
               #condition = "input.view_multiple_value_plots % 2 == 1",
               #br(),
               #uiOutput("uiMultiple_value_plots")
               #),
               
               #br()
               #),
               
               ###################################################################
               conditionalPanel(
                 condition = "input.pipeline_mutational_status == 1",
                 actionButton("view_mutational_status", "View Somatic hypermutation status plots", 
                              style="color: #fff; background-color: #5F021F; border-color: #fff"),
                 
                 conditionalPanel(
                   condition = "input.view_mutational_status % 2 == 1",
                   br(),
                   plotOutput("mutational_status_plot"),
                   br(),
                   actionButton("view_mutational_status_table", "View Somatic hypermutation status Table", 
                                style="color: #fff; background-color: #5F021F; border-color: #fff"),
                   
                   conditionalPanel(
                     condition = "input.view_mutational_status_table % 2 == 1",
                     br(),
                     tableOutput("mutational_status_table")  
                   )
                 ),
                 
                 br(),
                 br()
               ),
               
               ###################################################################
               conditionalPanel(
                 condition = "input.pipeline_logo == 1",
                 actionButton("view_logo_plots", "View Logo plots", 
                              style="color: #fff; background-color: #5F021F; border-color: #fff"),
                 
                 br(),
                 
                 conditionalPanel(
                   condition = "input.view_logo_plots % 2 == 1",
                   plotOutput('logo_visualisation')
                 ),
                 
                 br()
               ),
               ###################################################################
               conditionalPanel(
                 condition = "input.pipeline_clonotypes == 1 && output.num_of_datasets>1",
                 #condition= "unique(t(data.frame(strsplit(input$Dataset,'_')))[,1])>1",
                 actionButton("nucleotides_per_clonotype", "View nucleotides per clonotype", 
                              style="color: #fff; background-color: #5F021F; border-color: #fff"),
                 
                 br(),
                 #unique(t(data.frame(strsplit(input$Dataset,"_")))[,1])
                 
                 conditionalPanel(
                   condition = "input.nucleotides_per_clonotype % 2 == 1",
                   br(),
                   selectInput("select_plot_type_nucleotides_per_clonotype", "Select plot type:",c("hist3D","persp3D","image2D","surface"), width="200"),
                   numericInput("nucleotides_per_clonotype_topN", "Select top N Clonotypes:", 10,  min = 1, max = 1000, width="140px"),
                   br(),
                   uiOutput("nucleotides_per_clonotype_ui"),
                   br(),
                   conditionalPanel(
                     condition = "input.select_plot_type_nucleotides_per_clonotype == 'hist3D'",
                     plotOutput("nucleotides_per_clonotype_hist3D"),
                     br()
                   ),
                   
                   conditionalPanel(
                     condition = "input.select_plot_type_nucleotides_per_clonotype == 'persp3D'",
                     plotOutput("nucleotides_per_clonotype_persp3D"),
                     br()
                   ),
                   
                   conditionalPanel(
                     condition = "input.select_plot_type_nucleotides_per_clonotype == 'image2D'",
                     plotOutput("nucleotides_per_clonotype_image2D"),
                     br()
                   ),
                   
                   conditionalPanel(
                     condition = "input.select_plot_type_nucleotides_per_clonotype == 'surface'",
                     plotly::plotlyOutput("nucleotides_per_clonotype_surface"),
                     br()
                   )
                   
                   #checkboxGroupInput(inputId = "nucleotides_per_clonotype_Datasets", label = "Select Datasets", inline=TRUE, choices = input$Dataset)
                 ),
                 
                 br()
               ),
               ###################################################################
               conditionalPanel(
                 condition = "input.pipeline_cdr3_distribution == 1",
                 actionButton("view_length_distribution", "View CDR3 length Distribution", 
                              style="color: #fff; background-color: #5F021F; border-color: #fff"),
                 
                 br(),
                 
                 conditionalPanel(
                   condition = "input.view_length_distribution % 2 == 1",
                   br(),
                   plotOutput("length_distribution"),
                   br(),
                   
                   actionButton("view_length_distribution_table", "View CDR3 length Distribution Table", 
                                style="color: #fff; background-color: #5F021F; border-color: #fff"),
                   
                   br(),
                   
                   conditionalPanel(
                     condition = "input.view_length_distribution_table % 2 == 1",
                     br(),
                     dataTableOutput("length_distribution_table")
                   )
                   
                 )
               ),
               
               
               br(),
               
               ###################################################################
               conditionalPanel(
                 condition = "input.pipeline_pi_distribution == 1",
                 actionButton("view_pI_distribution", "View pI Distribution", 
                              style="color: #fff; background-color: #5F021F; border-color: #fff"),
                 
                 br(),
                 
                 conditionalPanel( 
                   condition = "input.view_pI_distribution % 2 == 1",
                   br(),
                   plotOutput("pI_distribution"),
                   br(),
                   
                   actionButton("view_pI_distribution_table", "View Pi Distribution Table", 
                                style="color: #fff; background-color: #5F021F; border-color: #fff"),
                   
                   br(),
                   
                   conditionalPanel(
                     condition = "input.view_pI_distribution_table % 2 == 1",
                     br(),
                     dataTableOutput("pI_distribution_table")
                   )
                   
                 ),
                 
                 br(),
                 br(),
                 
                 downloadButton("downloadAllPlots", "Download zip")
               )
             )
    ),
    
    tabPanel("Overview", value = "Overview",
             mainPanel(
               br(),    
               br(),
               br(),
               br(),
               
               tableOutput("overview_data"),
               tableOutput("overview_cleaning_parameters"), 
               tableOutput("overview_filtering_parameters"),
               tableOutput("overview_clonotypes"),
               tableOutput("overview_highly_sim_clono"),
               tableOutput("overview_public_clono"),
               tableOutput("overview_identity_groups"),
               tableOutput("overview_repertoires"),
               tableOutput("overview_multiple_value_comparison"),  
               tableOutput("overview_cdr3_1length_diff"),  
               tableOutput("overview_alignment"),  
               tableOutput("overview_freq_table"),  
               
               br(),
               br(),
               
               downloadButton("downloadOverview", "Download Overview"),
               br(),
               br(),
               downloadButton("downloadAllTables", "Download Tables zip"),
               br(),
               br(),
               bookmarkButton()
             )
    ),
    #To use js code in the app
    useShinyjs(),
    extendShinyjs(text = jscode,functions=c("enabletab","disabletab"))

  )
}