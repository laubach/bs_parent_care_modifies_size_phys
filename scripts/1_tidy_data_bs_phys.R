###############################################################################
#############              Barn swallow parental care             #############
#############               and offspring physiology              #############
#############                                                     #############
#############                    1. Tidy Data                     #############
#############                                                     #############
#############                  By: Zach Laubach                   #############
#############                created: 11 May 2022                 #############
#############              last updated: 01 Feb 2024              #############
###############################################################################


### PURPOSE: Tidy and join data in preparation for downstream analyses of
            # data
  
  
  # Code Blocks
    # 1: Configure work space
    # 2: Load RData
    # 3: Tidy data tables
    # 4: Export data files
  


###############################################################################
##############             1.  Configure work space              ##############
###############################################################################

  ### 1.1 Global options
    ## a) clear global environment
      rm(list = ls())

    ## b) prevent R from automatically reading charater strins as factors
      options(stringsAsFactors = FALSE)
      
    ## c) Setup packrat for reproducibility
      #library('packrat')
      #packrat::init('.') #initiate packrat in the current working directory
  

  ### 1.2 Install and load CRAN packages
    ## a) Data tidying and manipulation packages
     
      # load tidyverse package
        library('tidyverse')
      
      # load lubridate package
        library('lubridate')
      
      # load here package
        library('here')
      
   
  ### 1.3 Get Version and Session Info
    R.Version()
    sessionInfo()
    
    # Developed in:   
    # R version 4.2.3 (2023-03-15)
    # Platform: x86_64-apple-darwin17.0 (64-bit)
    # Running under: macOS 14.2.1
    
  
  ### 1.4 Set working directory 
    setwd(here())
    
    
  ### 1.5 Source functions
    ## a) Source scripts path
      source_path <- paste("~/WD/Git/source_code/")
    
    ## b) all_char_to_lower function
      source(file = paste0(source_path, "all_char_to_lower.R"))
    
    ## c) format_var_names function
      source(file = paste0(source_path, "format_var_names.R"))


      
###############################################################################
##############                    2. Load RData                  ##############
###############################################################################  
  
  ### 2.1 Load RData
    ## a) Load barn swallow raw data 
      load(here('data/1_raw_bs_phys_data.RData'))
     
      
      
###############################################################################
##############              3. Tidy individual tables            ##############
###############################################################################
  
  ### 3.1 Tidy nestling data
    ## a) Format data to all lower case
      nestling <- AllCharactersToLower(nestling)
      
    ## b) Format variable names to standard R format (sep = .)
      nestling <- FormatVarNames(nestling)
      
    ## c) Rename variables
      nestling <- nestling %>%
        rename(base.gluc = x3min.glucose,
               base.gluc.time = x3min.glucose.time,
               stress.gluc = x15min.glucose,
               stress.gluc.time = x15min.glucose.time)
      
    ## d) Format dates
      nestling$hatch.date <- as.Date(nestling$hatch.date, '%m/%d/%y')
      
      nestling$sample.date <- as.Date(nestling$sample.date, '%m/%d/%y')
      
    ## e) Concatenate sample.date and extract.time and format for lubridate
      nestling$sample.date.time <- paste(nestling$sample.date, 
                                          nestling$extract.time)
      
      nestling$sample.date.time <- gsub(' ','-', nestling$sample.date.time)
      
      nestling$sample.date.time <- gsub(':','-', nestling$sample.date.time)
      
    ## f) Convert sample.date.time to date time class
      nestling$sample.date.time <- ymd_hms(nestling$sample.date.time)
    
    ## g) Format glucose times 
      nestling$base.gluc.time <-substr(nestling$base.gluc.time, 1, 5)
      nestling$base.gluc.time <-ifelse(nestling$base.gluc.time>0, 
                                       paste0('00:',  nestling$base.gluc.time),
                                       '')
      
      nestling$stress.gluc.time <-substr(nestling$stress.gluc.time, 1, 5)
      nestling$stress.gluc.time <-ifelse(nestling$stress.gluc.time>0, 
                                       paste0('00:', nestling$stress.gluc.time),
                                       '')
      
    ## h) Convert blood times to seconds
      nestling$base.gluc.s <- 
        period_to_seconds(hms(nestling$base.gluc.time))
      
      nestling$stress.gluc.s <- 
        period_to_seconds(hms(nestling$stress.gluc.time))
      
    ## i) Format nest ID
      nestling$nest <- gsub('\\<.0\\>','', nestling$nest)
      
      nestling$nest.id <- as.factor(paste(nestling$site, 
                                       nestling$nest))
      
      nestling <- nestling %>%
        select(-c (nest))
      
    ## j) Format site
      nestling$site <- as.factor(nestling$site)
      
    ## k) Create a factor to identify developmental stage at sampling
      nestling <- nestling %>%
        group_by(nest.id) %>%
        mutate(sample.state =
                 case_when(nestling.age <= 6
                           ~ c('early'),
                           nestling.age > 6 & nestling.age <= 10
                           ~ c('mid'),
                           nestling.age > 10 
                           ~ c('late'))) %>%
        ungroup()
  
      #Re-code *nominal* factor (with ordered levels)
      # Set levels (odering) of state variable 
      nestling <- transform(nestling, 
                             sample.state = factor(sample.state,
                                            levels = c("early", "mid", 
                                                       "late")))
      levels(nestling$sample.state)
 
    ## l) Classify nestling size smallest vs other by nest by develop. state
      nestling <- nestling  %>%
        group_by(nest.id, sample.state) %>%
        mutate(size.order = as.factor(case_when(rt.wing.length == 
                                                  min(rt.wing.length, 
                                                      na.rm = T)
                                                ~ 'min',
                                                rt.wing.length != 
                                                  min(rt.wing.length, 
                                                      na.rm = T)
                                                ~ 'other'))) %>%
        ungroup()
      
    ## m) Classify nestling size above and below average size by nest by
      # develop. state
      avg_size_nest <- nestling  %>%
        group_by(nest.id, sample.state) %>%
        summarise(avg.size.by.nest = round(mean(rt.wing.length, 
                                              na.rm = T),2)) %>%
        ungroup()
      
      nestling <- nestling  %>%
        left_join(avg_size_nest, by = c('nest.id' = 'nest.id', 
                                        'sample.state' = 'sample.state'), 
                  copy = F)
      
      nestling <- nestling  %>%
        group_by(nest.id, sample.state) %>%
        mutate(size.by.avg = as.factor(case_when(rt.wing.length < 
                                                   avg.size.by.nest
                                                ~ 'sm',
                                                 rt.wing.length >= 
                                                  avg.size.by.nest
                                                ~ 'lrg'))) %>%
        ungroup()
      
      # Set levels (odering) of state variable 
      nestling <- transform(nestling, 
                            size.by.avg = factor(size.by.avg,
                                                 levels = c('sm', 'lrg')))
      levels(nestling$size.by.avg)
     
    ## n) Calculate mass to wing length ratio 
      nestling <- nestling %>%  
        mutate(mass.wing.index = (mass.pre.obs/rt.wing.length)) 
      
    ## o) Make a binary variable for some or no mites
      nestling <- nestling %>% 
        mutate(mite.bin = ifelse(nestling$nos.mites >= 1, 'yes', 'no'))
        
    ## p) Calculate difference second and first glucose (mg/dl)
      nestling <- nestling %>% 
        mutate(gluc.diff = (stress.gluc 
                            - base.gluc)) 
      
    ## q) Create a variable to indicate positive or negative differences
      # between stress and baseline glucose
      nestling$diff.dir = ifelse(nestling$gluc.diff <= 0, 
                                      'negative', 'positive')

    ## r) Reorder variables
      nestling_col <- colnames(nestling)
      
      nestling <- nestling[, c("female.band", "male.band","nestling.band",
                               "hatch.order", "site", "brood", "nest.id",
                               "hatch.date", "sample.date", "sample.date.time",
                               "nestling.age", "nestling.number", 
                               "sample.state", "extract.time", "rt.wing.length", 
                               "mass.pre.obs", "size.order", "avg.size.by.nest",      
                               "size.by.avg", "mass.wing.index", 
                               "base.gluc", "base.gluc.time", "base.gluc.s",
                               "stress.gluc","stress.gluc.time", 
                               "stress.gluc.s", "gluc.diff", "diff.dir", 
                               "blood.amount.lysis", "lysis.sample", 
                               "blood.amount.rna", "rna.sample", "feathers", 
                               "nos.mites", "mite.bin", "mites.tp", 
                               "survive.at.sampling", "post.obs.extract.time", 
                               "mass.post.obs", "notes" )] 
      
      
#***NOTE: Remove Hayes 7 and Schapps 131.2 because not used in main analysis
  #* Hayes 7 no mid and late nestling measurements (depredation)
  #* Schaaps 131.2 second brood
      nestling <- nestling  %>%
        filter(nest.id != 'hayes 7') %>%
        filter(nest.id != 'schaaps 131.2')

      
  ### 3.2 Tidy parent_care_trial data
    ## a) Format data to all lower case
      parent_care_trial <- AllCharactersToLower(parent_care_trial)
      
    ## b) Format variable names to standard R format (sep = .)
      parent_care_trial <- FormatVarNames(parent_care_trial)
      
    ## c) Format dates
      parent_care_trial$obs.date <- as.Date(parent_care_trial$obs.date, 
                                            '%m/%d/%y')
      
    ## d) Format nest ID
      parent_care_trial$nest <- gsub('\\<.0\\>','', parent_care_trial$nest)
      
      parent_care_trial$nest.id <- as.factor(paste(parent_care_trial$site, 
                                             parent_care_trial$nest))
      
      parent_care_trial <- parent_care_trial %>%
        select(-c (nest))
      
    ## e) Rename notes column
      parent_care_trial <- parent_care_trial %>%
        rename(notes.obs = notes)
      
      
    ## f) Create a factor to identify developmental stage at obs
      parent_care_trial <- parent_care_trial %>%
        group_by(nest.id) %>%
        mutate(obs.state =
                 case_when(nestling.age <= 6
                           ~ c('early'),
                           nestling.age > 6 & nestling.age <= 10
                           ~ c('mid'),
                           nestling.age > 10 
                           ~ c('late'))) %>%
        ungroup()
      
      #Re-code *nominal* factor (with ordered levels)
      # Set levels (odering) of state variable 
      parent_care_trial <- transform(parent_care_trial, 
                                     obs.state = factor(obs.state,
                                                  levels = c("early", "mid", 
                                                             "late")))
      levels(parent_care_trial$obs.state)
      
   
  ### 3.3 Tidy parent_care_obs data
    ## a) Format data to all lower case
      parent_care_obs <- AllCharactersToLower(parent_care_obs)
      
    ## b) Format variable names to standard R format (sep = .)
      parent_care_obs <- FormatVarNames(parent_care_obs)
      
    ## c) Format dates
      parent_care_obs$obs.date <- as.Date(parent_care_obs$obs.date, 
                                            '%m/%d/%y')
      
    ## d) Format nest ID
      parent_care_obs$nest <- gsub('\\<.0\\>','', parent_care_obs$nest)
     
      parent_care_obs$nest.id <- as.factor(paste(parent_care_obs$site, 
                                                   parent_care_obs$nest))
      
#***NOTE: Divide by total observation time to account for slight differences in 
#*** in effort and re scale seconds to minutes
    ## e) Proportion of time for duration and rate for event behaviors 
      parent_care_obs <- parent_care_obs %>%
        mutate(total.visits.rate = 
                 total.visits / (obs.duration/60)) %>%
      mutate(total.feeding.visits.rate = 
               total.feeding.visits / (obs.duration/60)) %>%
        mutate(total.an.duration.prop = 
                 total.an.duration / obs.duration) %>%
        mutate(total.brooding.duration.prop = 
                 total.brooding.duration / obs.duration)
        
    ## f) Left join parent_care_obs to parent_care_trial 
      parent_care <- parent_care_trial  %>%
        left_join(select(parent_care_obs, -c(...1, female.band, nest, site, 
                                         nestling.age)),
                  by = c('nest.id' = 'nest.id', 
                         'obs.date' = 'obs.date'), 
                  copy = F)
      
#***NOTE: Remove Hayes 7 and Schapps 131.2 because not used in main analysis
  #* Hayes 7 no mid and late nestling measurements (depredation)
  #* Schaaps 131.2 second brood
      parent_care <- parent_care  %>%
        filter(nest.id != 'hayes 7') %>%
        filter(nest.id != 'schaaps 131.2')
      
    ## h) Create tertiles of parental care behaviors within each developmental
      # state
      parent_care <- parent_care  %>%
        group_by(obs.state)  %>%
        # mutate(tert.tot.visit = ntile(total.visits.rate, 3)) %>%
        mutate(tert.tot.feed = ntile(total.feeding.visits.rate, 3)) %>%
        # mutate(tert.tot.time = ntile(total.an.duration.prop, 3)) %>%
        mutate(tert.tot.brood = ntile(total.brooding.duration.prop, 3)) %>%
        ungroup()
      
      
  ### 3.4 Join and tidy nestling and parental care data
    ## a) Left join parent_care to nestling 
      nestling_parent_care <- nestling  %>%
        left_join(select(parent_care, -c(female.band, male.band, site, 
                                        nestling.age, nestling.number, 
                                        obs.state)),
                  by = c('nest.id' = 'nest.id', 
                                        'sample.date' = 'obs.date'), 
                  copy = F)
 
      
  ### 3.5 Reformat data wide to long
    ## a) Transform nestling_parent_care from wide to long format
        nestling_parent_care_l_1 <- nestling_parent_care %>%
          select(-c(base.gluc.time, stress.gluc.time, base.gluc.s, 
                    stress.gluc.s)) %>%
          pivot_longer(cols = c(base.gluc, stress.gluc),
                       names_to = 'glucose.sample',
                       values_to = 'glucose')

        nestling_parent_care_l_2 <- nestling_parent_care %>%
          select(nestling.band, base.gluc.time, stress.gluc.time) %>%
          pivot_longer(cols = c(base.gluc.time, stress.gluc.time),
                        names_to = 'sample.order',
                        values_to = 'sample.time')

        nestling_parent_care_l_3 <- nestling_parent_care %>%
          select(nestling.band, base.gluc.s, stress.gluc.s) %>%
          pivot_longer(cols = c(base.gluc.s, stress.gluc.s),
                       names_to = 'sample.order.s',
                       values_to = 'sample.time.s')
          
    ## b) Select only variables that are needed
        nestling_parent_care_l_2 <- nestling_parent_care_l_2 %>%
          select(nestling.band, sample.time)
        
        nestling_parent_care_l_3 <- nestling_parent_care_l_3 %>%
          select(nestling.band, sample.time.s)

        nestling_parent_care_l <- cbind(nestling_parent_care_l_1, 
                                        nestling_parent_care_l_2,
                                        nestling_parent_care_l_3)
        
    ## c) Remove duplicated nestling.band column after checking for consistency
        nestling_parent_care_l <- nestling_parent_care_l[!duplicated
                                    (as.list(nestling_parent_care_l))]
        
        
        
###############################################################################
##############              4. Export data files                ##############
###############################################################################
      
  ### 4.1 Export data to an RData file 
      # Files are saved in the 'data' folder in the working directory as an
      # RData file.
    
    ## a) Save and export raw data tables for script 2 and 5
      save(file = here('data/2_5_tidy_bs_phys_data.RData'), 
           list = c('nestling_parent_care', 'nestling_parent_care_l',
                    'parent_care'))
      
  


   