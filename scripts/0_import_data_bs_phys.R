###############################################################################
#############              Barn swallow parental care             #############
#############               and offspring physiology              #############
#############                                                     #############
#############                   0. Data Import                    #############
#############                                                     #############
#############                  By: Zach Laubach                   #############
#############                created: 11 May 2022                 #############
#############             last updated: 26 May 2023               #############
###############################################################################


  ### PURPOSE: Load barn swallow parental care and offspring physiology 
              # data
  
  
  # Code Blocks
    # 1: Configure work space
    # 2: Import data 
    # 3: Export data files
  



###############################################################################
##############             1.  Configure work space              ##############
###############################################################################

  ### 1.1 Global options
    ## a) clear global environment
      rm(list = ls())

    ## b) prevent R from automatically reading charater strings as factors
      options(stringsAsFactors = FALSE)

      
  ### 1.2 Install and load CRAN packages   
    ## a) Data Manipulation and Descriptive Stats Packages
      # load tidyverse packages
        library ('tidyverse')
 
      # load here packages
        library ('here')

        
  ### 1.3 Get Version and Session Info
    R.Version()
    sessionInfo()
    
    # Developed in:   
    # R version 4.0.2 (2020-06-22)
    # Platform: x86_64-apple-darwin17.0 (64-bit)
    # Running under: macOS  10.16
    
  
  ### 1.4 Set working directory 
    setwd(here())
  
  
      
###############################################################################
##############                  2. Import data                   ##############
###############################################################################    
      
  ### 2.1 Import sample data files
    ## a) Import nestling data.
      nestling <- read_csv(here('data/nestling_data.csv'))
    
    ## b) Import parental care trial data.
      parent_care_trial <- read_csv(here('data/parental_care_data.csv'))
      
    ## c) Import parental care obs data.
      parent_care_obs <- read_csv(here('data/ab_pro_summary_data.csv'))
      
    ## d) Import nestling and parental care summary data.
      nest_parent_summry <- read_csv(
        here('data/nestling_parent_care_summary.csv'))
      
  

###############################################################################
##############                3. Export data files               ##############
###############################################################################
      
  ### 3.1 Export data to an RData file     
    ## a) Save and export raw data tables 
      # Files are saved in the 'data' folder in the working directory as an
      # RData file.
      save(file = here('data/1_raw_bs_phys_data.RData'), 
           list = c('nestling', 'parent_care_trial',
                    'parent_care_obs', 'nest_parent_summry'))
    