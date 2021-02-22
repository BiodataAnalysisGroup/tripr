#!/usr/bin/env Rscript

# Wrapper for the run_TRIP_without_ui script. It lives as an R shell script in 
# order to be seamlessly integrated to the general variant calling pipeline 
# which uses only system interactions.
# To run the script using the command line using the default input arguments: Rscript --vanilla make_options.R
# e.g. Rscript --vanilla make_options.R --pipeline 1
# Help: Rscript --vanilla make_options.R --help 

# Author: Maria Kotouza

suppressPackageStartupMessages(library("optparse"))

option_list <- list(
  make_option(
    opt_str=c("-a","--datapath"),
    action="store",
    default="Datasets/nikolas/Tcell_sample/",
    help=paste0("The directory where the folders of the patients' data are located.")
  ),
  
  make_option(
    opt_str=c("-b","--filelist"),
    action="store",
    default="1_Summary.txt,2_IMGT-gapped-nt-sequences.txt,4_IMGT-gapped-AA-sequences.txt,6_Junction.txt",
    help=paste0(
      "The files of the IMGT output that will be used through the analysis.\n",
      "Use comma to seperate the list of files (default %default)"
    )
  ),
  
  make_option(
    opt_str=c("-c","--cell"),
    action="store",
    default="Tcell",
    help="Supported cells: One of 'Bcell' (default) or 'Tcell'."
  ),
  
  make_option(
    opt_str=c("-d","--throughput"),
    action="store",
    default="High Throughput",
    help="Supported cells: One of 'High Throughput' (default) or 'Low Throughput'."
  ),
  
  make_option(
    opt_str=c("-e","--preselection"),
    action="store",
    default="1,4C:F",
    help=paste0(
      "Preselection options:\n",
      "1. Only take into account Functional V-Gene\n",
      "2. Only take into account CDR3 with no Special Characters (X,*,#,.) \n",
      "3. Only take into account Productive Sequences\n",
      "4. Only take into account CDR3 with valid start/end landmarks.\n",
      "For Preselection option 4, select start/end landmarks.\n",
      "Use the vertical line '|' to add more than one start or end landmarks\n",
      "Use comma to seperate the list of options, use semicolon ':' to seperate start and end landmarks (default %default) \n"
    )
  ),
  
  make_option(
    opt_str=c("-f","--selection"),
    action="store",
    default="5",
    help=paste0(
      "Selection options:\n",
      "5. V-REGION identity % \n",
      "6. Select Specific V Gene \n",
      "7. Select Specific J Gene \n",
      "8. Select Specific D Gene \n",
      "9. Select CDR3 length range \n",
      "10. Only select CDR3 containing specific amino-acid sequence.\n",
      "Use comma to seperate the list of options (default %default)"
    )
  ),
  
  make_option(
    opt_str=c("-g","--identity_range"),
    action="store",
    default="95:100",
    help=paste0(
      "V-REGION identity %Low and %High\n",
      "Use semicolon ':' to seperate identity low and high (default %default) \n"
    )
  ),
  
  make_option(
    opt_str=c("-i","--vgenes"),
    action="store",
    default="",
    help=paste0(
      "Filter in specific V Genes\n",
      "Separate the different V-Gene names with '|' e.g. TRBV11-2|TRBV29-1*03 (F)\n"
    )
  ),
  
  make_option(
    opt_str=c("-j","--dgenes"),
    action="store",
    default="",
    help=paste0(
      "Filter in specific D Genes\n",
      "Separate the different D-Gene names with | e.g. TRBD2|TRBD1\n"
    )
  ),
  
  make_option(
    opt_str=c("-k","--jgenes"),
    action="store",
    default="",
    help=paste0(
      "Filter in specific J Genes\n",
      "Separate the different J-Gene names with | e.g. TRBJ2-6|TRBJ2-2\n"
    )
  ),
  
  make_option(
    opt_str=c("-l","--cdr3_length_range"),
    action="store",
    default="",
    help=paste0(
      "Filter in rows with CDR3 lengths within a range\n",
      "Use semicolon ':' to seperate identity low and high (default %default)\n"
    )
  ),
  
  make_option(
    opt_str=c("-m","--aminoacid"),
    action="store",
    default="",
    help=paste0(
      "Filter in rows with CDR3 containing specific amino-acid sequence\n"
    )
  ),
  
  make_option(
    opt_str=c("-n","--pipeline"),
    action="store",
    default="1,12",
    help=paste0(
      "Pipeline options:\n",
      "1. Clonotypes Computation \n",
      "2. Highly Similar Clonotypes computation \n",
      "3. Shared Clonotypes Computation \n",
      "4. Highly Similar Shared Clonotypes Computation \n",
      "5. Repertoires Extraction \n",
      "6. Repertoires Comparison \n",
      "7. Highly Similar Repertoires Extraction \n",
      "8. Insert Identity groups \n",
      "9. Somatic hypermutation status\n", 
      "10. CDR3 Distribution \n",
      "11. Pi Distribution \n",
      "12. Multiple value comparison\n",
      "13. CDR3 with 1 length difference\n",
      "14. Alignment \n",
      "15. Somatic hypermutations\n", 
      "16. Logo\n",
      "17. SHM normal\n",
      "18. SHM High similarity\n",
      "19. Diagnosis\n",
      "Use comma to seperate the list of options (default %default)"
    )
  ),
  
  make_option(
    opt_str=c("-o","--select_clonotype"),
    action="store",
    default="V Gene + CDR3 Amino Acids",
    help=paste0(
      "Compute clonotypes. Select one the following options:\n",
      "V Gene + CDR3 Amino Acids\n",
      "V Gene and Allele + CDR3 Amino Acids\n",
      "V Gene + CDR3 Nucleotide\n",
      "V Gene and Allele + CDR3 Nucleotide\n",
      "J Gene + CDR3 Amino Acids\n",
      "J Gene and Allele + CDR3 Amino Acids\n",
      "J Gene + CDR3 Nucleotide\n",
      "J Gene and Allele + CDR3 Nucleotide\n",
      "CDR3 Amino Acids\n",
      "CDR3 Nucleotide\n",
      "Sequence\n",
      "default %default"
    )
  ),
  
  make_option(
    opt_str=c("-p","--highly_sim_params"),
    action="store",
    default="1-1 2-1 3-1 4-1 5-1 6-1 7-1 8-1 9-1 10-1 11-1 12-1 13-1 14-1 15-2 16-2 17-2 18-2 19-2 20-2 21-2 23-2 24-2 25-2 26-2 27-2 28-2 29-3 30-3 31-3 32-3 33-3 34-3 35-3 36-3 37-3 38-3 39-3 40-3 41-3 42-3 43-3 44-3 45-3 46-3 47-3 48-3 49-3 50-3,1,Yes",
    help=paste0(
      "Select number of missmatches, the theshold of the clonotype frequency and whether you want to take gene into account\n",
      "Use dashes '-' to show the length of the cdr3 sequences and the number of allowed missmatches and spaces ' ' to separate. For the cdr3 lengths that the number of missmatches are not specified the default value that will be used is 1 \n",
      "Use comma to separate the three options \n",
      "default %default"
    )
  ),
  
  make_option(
    opt_str=c("-q","--shared_clonotypes_params"),
    action="store",
    default="reads,1,Yes",
    help=paste0(
      "Shared clonotypes computation\n",
      "Select 'reads' of 'theshold' for clonotypes, the number of reads or the threshold percentage accordingly, and whether you want to take gene into account\n",
      "Use comma to seperate the 3 options \n",
      "default %default"
    )
  ),
  
  make_option(
    opt_str=c("-r","--highly_shared_clonotypes_params"),
    action="store",
    default="reads,1,Yes",
    help=paste0(
      "Highly Similar Shared Clonotypes Computation \n",
      "Select 'reads' of 'theshold' for clonotypes, the number of reads or the threshold percentage accordingly, and whether you want to take gene into account\n",
      "Use comma to seperate the 3  options \n",
      "default %default"
    )
  ),
  
  make_option(
    opt_str=c("-s","--repertoires_params"),
    action="store",
    default="1,4,6",
    help=paste0(
      "Repertoires Extraction  \n",
      "Options:\n",
      "1. V Gene\n", 
      "2. V Gene and allele\n",
      "3. J Gene\n", 
      "4. J Gene and allele\n",
      "5. D Gene\n", 
      "6. D Gene and allele\n",
      "Use comma to seperate the selected options \n",
      "default %default"
    )
  ),
  
  make_option(
    opt_str=c("-t","--identity_groups"),
    action="store",
    default="85:97,97:99,99:100,100:100",
    help=paste0(
      "Insert identity groups  \n",
      "Insert low and high values as follows\n",
      "low_values:high_values\n", 
      "Seperate low_values and high_values using comma.\n",
      "default %default"
    )
  ),
  
  make_option(
    opt_str=c("-u","--multiple_values_params"),
    action="store",
    default="2:7,2:3,2:5,2:11",
    help=paste0(
      "Multiple value comparison  \n",
      "Options\n",
      "1. V GENE \n", 
      "2. V GENE and allele \n",
      "3. J GENE \n", 
      "4. J GENE and allele \n",
      "5. D GENE \n", 
      "6. D GENE and allele \n",
      "7. CDR3-IMGT length \n", 
      "8. D-REGION reading frame \n",
      "9. Molecular mass \n",
      "10. pI \n",
      "11. V-REGION identity % \n",
      "Use semicolon to indicate cobinations of 2 values, use comma to seperate the selected options \n",
      "default %default"
    )
  ),
  
  make_option(
    opt_str=c("-v","--alignment_params"),
    action="store",
    default="1,both,1,2:20",
    help=paste0(
      "Alignment parameters:\n",
      "Region for Alignment: 1. V.D.J.REGION or 2. V.J.REGION\n",
      "AA or Nt: Select aa or nt or both\n",
      "Germline: 1. Use Allele's germline or 2. Use Gene's germline\n",
      "Use: 1. All clonotypes or 2. Select top N clonotypes or 3. Select threshold for clonotypes\n",
      "Use comma to seperate the 4 parameters.\n",
      "If you select option 2 or 3 at the 4rth parameter you have to set the N or the threshold as well using semicolon\n",
      "default %default"
    )
  ),
  
  make_option(
    opt_str=c("-w","--mutations_params"),
    action="store",
    default="both,0.5,0.5,2:20",
    help=paste0(
      "Somatic hypermutations parameters:\n",
      "AA or Nt: Select aa or nt or both\n",
      "Set threshold for AA\n",
      "Set threshold for Nt\n",
      "Use: 1. All clonotypes or 2. Select top N clonotypes or 3. Select threshold for clonotypes\n",
      "Use comma to seperate the 3 parameters.\n",
      "If you select option 2 or 3 at the 3rd parameter you have to set the N or the threshold as well using semicolon\n",
      "default %default"
    )
  )
);


opt <- parse_args(OptionParser(option_list=option_list));

filelist <- strsplit(opt$filelist, ",")[[1]]

source("run_TRIP_without_ui.R")

run_TRIP(datapath = opt$datapath, 
         filelist = filelist, 
         cell = opt$cell, 
         throughput = opt$throughput, 
         preselection = opt$preselection, 
         selection = opt$selection, 
         identity_range = opt$identity_range, 
         vgenes = opt$vgenes, 
         aminoacid = opt$aminoacid,
         dgenes = opt$dgenes, 
         jgenes = opt$jgenes, 
         cdr3_length_range = opt$cdr3_length_range, 
         aminoacid = opt$aminoacid,
         pipeline = opt$pipeline, 
         select_clonotype = opt$select_clonotype, 
         highly_sim_params = opt$highly_sim_params, 
         shared_clonotypes_params = opt$shared_clonotypes_params, 
         highly_shared_clonotypes_params = opt$highly_shared_clonotypes_params, 
         repertoires_params = opt$repertoires_params, 
         identity_groups = opt$identity_groups,
         multiple_values_params = opt$multiple_values_params, 
         alignment_params = opt$alignment_params, 
         mutations_params = opt$mutations_params)

# Print help
# print_help( OptionParser(option_list=option_list))

