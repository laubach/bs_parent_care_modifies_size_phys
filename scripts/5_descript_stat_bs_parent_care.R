###############################################################################
#############              Barn swallow parental care             #############
#############               and offspring physiology              #############
#############                                                     #############
#############                5. Descriptive Stats:                #############
#############               Parental care behaviors               #############
#############                  By: Zach Laubach                   #############
#############                created: 3 June 2022                 #############
#############              last updated: 01 Feb 2024              #############
###############################################################################


### PURPOSE: Calculate descriptive statistics and visualize parental care data.
  
  
  # Code Blocks
    # 1: Configure work space
    # 2: Load RData
    # 3: Univariate descriptive stats
    # 4: Additional data tidying
    # 5: Determinants of parental care
    # 6: Summarize parental care data
    # 7: Combine parent care and nestling data 
    # 8: Bivariate descriptive stats
    # 9: Export data files
    


###############################################################################
##############             1.  Configure work space              ##############
###############################################################################

  ### 1.1 Global options
    ## a) clear global environment
      rm(list = ls())

    ## b) prevent R from automatically reading character strings as factors
      options(stringsAsFactors = FALSE)
  

  ### 1.2 Install and load CRAN packages   
    ## a) Data Manipulation and Descriptive Stats Packages
      # load tidyverse packages
        library ('tidyverse')

      # load here packages
        library ('here')
      
      # load package Hmisc
        library('Hmisc')
     
    ## b) Graph Plotting and Visualization Packages
      # load ggplot2 packages
      library ('ggplot2')
      
      library('hrbrthemes')
  
      library('viridis')

      # load gridExtra packages
      library ('gridExtra')
      
    ## c) Modelling Packages
      # load nlme packages
      library ('nlme')
      
      #load lme4
      library('lme4')
      
      # load performance
      library('performance')
      
      # load dharma
      library('DHARMa')
      
      # load bbmle
      library('bbmle')
      
        
  ### 1.3 Get Version and Session Info
    R.Version()
    sessionInfo()
    
    # Developed in:   
    # R version 4.0.2 (2020-06-22)
    # Platform: x86_64-apple-darwin17.0 (64-bit)
    # Running under: macOS Catalina 10.16
    
  
  ### 1.4 Set working directory 
    setwd(here())
  

      
###############################################################################
##############                    2. Load RData                  ##############
###############################################################################  
  
  ### 2.1 Load RData
    ## a) Load RData tidy barn swallow data (parental care data)
    load(here('data/2_5_tidy_bs_phys_data.RData'))
    load(here('data/tidy_parent_nestl_weather_data_8-23_with_pci.RData'))
    
      
      
###############################################################################
##############          3. Univariate descriptive stats          ##############
###############################################################################
    
  ### 3.1 Histograms of parental care behaviors
    ## a) Histogram total visitation rates by developmental state
      tot_visit_hist <- parent_care %>%
        ggplot(aes(x = total.visits, fill = obs.state)) + 
        geom_histogram(color='gray50', alpha=0.6, position = 'identity', 
                       binwidth = 5) +
        # #geom_point(aes(y=response), position = position_jitter(w = 0.3, h = 0)) +
        scale_fill_manual(values=c('brown2', 'palegreen4', 'steelblue4'), 
                          name = 'Developmental state',
                          labels = c('early', 'mid', 'late')) +
        #theme_ipsum() +
        labs(title = 'Histogram of total vistation rate (counts/hour)
             by developmental state',
             x ='Total vistation rate (counts/~hour)', 
             y ='Frequency') +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        theme(axis.ticks = element_blank()) + # remove axis ticks
        theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
        # customize legend
        theme(legend.title=element_text(size=10),
              legend.text=element_text(size=8),
              legend.position = c(0.8, 0.8))
      
    ## b) Print plot 
      print(tot_visit_hist)
      
    ## c) Save plot
      ggsave('tot_visit_hist.pdf', plot = tot_visit_hist, 
             device = NULL, 
             path = here('output/'), scale = 1, width = 12, 
             height = 6, 
             units = c('in'), dpi = 300, limitsize = TRUE)     
      
    ## d) Histogram total feeding rates by developmental state   
      tot_feed_hist <- parent_care %>%
        ggplot(aes(x = total.feeding.visits, fill = obs.state)) + 
        geom_histogram(color='gray50', alpha=0.6, position = 'identity', 
                       binwidth = 3) +
        # #geom_point(aes(y=response), position = position_jitter(w = 0.3, h = 0)) +
        scale_fill_manual(values=c('brown2', 'palegreen4', 'steelblue4'), 
                          name = 'Developmental state',
                          labels = c('early', 'mid', 'late')) +
        #theme_ipsum() +
        labs(title = 'Histogram of total feeding rate (counts/hour)
             by developmental state',
             x ='Total feeding rate (counts/~hour)', 
             y ='Frequency') +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        theme(axis.ticks = element_blank()) + # remove axis ticks
        theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
        # customize legend
        theme(legend.title=element_text(size=10),
              legend.text=element_text(size=8),
              legend.position = c(0.8, 0.8))
      
    ## e) Print plot 
      print(tot_feed_hist)
      
    ## f) Save plot
      ggsave('tot_feed_hist.pdf', plot = tot_feed_hist, 
             device = NULL, 
             path = here('output/'), scale = 1, width = 12, 
             height = 6, 
             units = c('in'), dpi = 300, limitsize = TRUE)  
      
    ## g) Histogram of total duration of time at the nest by developmental state   
      tot_an_dur_hist <- parent_care %>%
        ggplot(aes(x = total.an.duration, fill = obs.state)) + 
        geom_histogram(color='gray50', alpha=0.6, position = 'identity', 
                       binwidth = 300) +
        # #geom_point(aes(y=response), position = position_jitter(w = 0.3, h = 0)) +
        scale_fill_manual(values=c('brown2', 'palegreen4', 'steelblue4'), 
                          name = 'Developmental state',
                          labels = c('early', 'mid', 'late')) +
        #theme_ipsum() +
        labs(title = 'Histogram of total time duration at the nest (s)
             by developmental state',
             x ='Total time at nest (s)', 
             y ='Frequency') +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        theme(axis.ticks = element_blank()) + # remove axis ticks
        theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
        # customize legend
        theme(legend.title=element_text(size=10),
              legend.text=element_text(size=8),
              legend.position = c(0.8, 0.8))
      
    ## h) Print plot 
      print(tot_an_dur_hist)
      
    ## i) Save plot
      ggsave('tot_an_dur_hist.pdf', plot = tot_an_dur_hist, 
             device = NULL, 
             path = here('output/'), scale = 1, width = 12, 
             height = 6, 
             units = c('in'), dpi = 300, limitsize = TRUE)
      
    ## j) Histogram of total duration of time brooding by developmental state   
      tot_brood_dur_hist <- parent_care %>%
        ggplot(aes(x = total.brooding.duration, fill = obs.state)) + 
        geom_histogram(color='gray50', alpha=0.6, position = 'identity', 
                       binwidth = 300) +
        # #geom_point(aes(y=response), position = position_jitter(w = 0.3, h = 0)) +
        scale_fill_manual(values=c('brown2', 'palegreen4', 'steelblue4'), 
                          name = 'Developmental state',
                          labels = c('early', 'mid', 'late')) +
        #theme_ipsum() +
        labs(title = 'Histogram of total time duration brooding (s)
             by developmental state',
             x ='Total time brooding (s)', 
             y ='Frequency') +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        theme(axis.ticks = element_blank()) + # remove axis ticks
        theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
        # customize legend
        theme(legend.title=element_text(size=10),
              legend.text=element_text(size=8),
              legend.position = c(0.8, 0.8))
      
    ## k) Print plot 
      print(tot_brood_dur_hist)
      
    ## l) Save plot
      ggsave('tot_brood_dur_hist.pdf', plot = tot_brood_dur_hist, 
             device = NULL, 
             path = here('output/'), scale = 1, width = 12, 
             height = 6, 
             units = c('in'), dpi = 300, limitsize = TRUE)
      
    
      
  ### 3.7 Univariate descriptive stats parental care behaviors           
    ## a) Descriptive stats parental care behaviors
      univar_parent_care_behav <- parent_care %>%
        group_by(obs.state) %>%
        summarise(
        #            n.tot.visits = sum(!is.na(total.visits)),
        #            avg.tot.visits = round (mean(total.visits, 
        #                                      na.rm = T),2),
        #            stdev.tot.visits = round (sd(total.visits, 
        #                                      na.rm = T), 2),
        #            med.tot.visits = round(median(total.visits,
        #                                       na.rm = T), 2),
        #            min.tot.visits = round(min(total.visits,
        #                                    na.rm = T), 2),
        #            max.tot.visits = round(max(total.visits,
        #                                    na.rm = T), 2),
                   n.tot.feed = sum(!is.na(total.feeding.visits.rate)),
                   avg.tot.feed = round (mean(total.feeding.visits.rate, 
                                                na.rm = T),3),
                   stdev.tot.feed = round (sd(total.feeding.visits.rate, 
                                                na.rm = T), 3),
                   med.tot.feed = round(median(60*total.feeding.visits.rate,
                                                 na.rm = T), 3),
                   min.tot.feed = round(min(60*total.feeding.visits.rate,
                                              na.rm = T), 3),
                   max.tot.feed = round(max(60*total.feeding.visits.rate,
                                              na.rm = T), 3),
                   # n.tot.an.dur = sum(!is.na(total.an.duration)),
                   # avg.tot.an.dur = round (mean(total.an.duration, 
                   #                            na.rm = T),2),
                   # stdev.tot.an.dur = round (sd(total.an.duration, 
                   #                            na.rm = T), 2),
                   # med.tot.an.dur = round(median(total.an.duration,
                   #                             na.rm = T), 2),
                   # min.tot.an.dur = round(min(total.an.duration,
                   #                          na.rm = T), 2),
                   # max.tot.an.dur = round(max(total.an.duration,
                   #                          na.rm = T), 2),
                   n.tot.brood.dur = sum(!is.na(total.brooding.duration.prop)),
                   avg.tot.brood.dur = round (mean(total.brooding.duration.prop, 
                                                na.rm = T),2),
                   stdev.tot.brood.dur = round (sd(total.brooding.duration.prop, 
                                                na.rm = T), 2),
                   med.tot.brood.dur = round(median(total.brooding.duration.prop,
                                                 na.rm = T), 2),
                   min.tot.brood.dur = round(min(total.brooding.duration.prop,
                                              na.rm = T), 2),
                   max.tot.brood.dur = round(max(total.brooding.duration.prop,
                                              na.rm = T), 2))
      


    ## b) save the data frame of summary stats as a pdf into output file
      pdf(here('output/univar_parent_care_behav.pdf'), height = 2, width = 17)
      grid.table(univar_parent_care_behav)
      dev.off()
      
    ## c) Descriptive stats parental care behaviors
      univar_parent_care_trial <- parent_care %>%
        group_by(obs.state) %>%
        summarise (n.trial.temp = sum(!is.na(trial.temp)),
                   avg.trial.temp = round (mean(trial.temp, 
                                                na.rm = T),2),
                   stdev.trial.temp = round (sd(trial.temp, 
                                                na.rm = T), 2),
                   med.trial.temp = round(median(trial.temp,
                                                 na.rm = T), 2),
                   min.trial.temp = round(min(trial.temp,
                                              na.rm = T), 2),
                   max.trial.temp = round(max(trial.temp,
                                              na.rm = T), 2),
                   n.trial.humid = sum(!is.na(trial.humidity)),
                   avg.trial.humid = round (mean(trial.humidity, 
                                              na.rm = T),2),
                   stdev.trial.humid = round (sd(trial.humidity, 
                                              na.rm = T), 2),
                   med.trial.humid = round(median(trial.humidity,
                                               na.rm = T), 2),
                   min.trial.humid = round(min(trial.humidity,
                                            na.rm = T), 2),
                   max.trial.humid = round(max(trial.humidity,
                                            na.rm = T), 2),
                   n.wind.sp = sum(!is.na(wind.speed)),
                   avg.wind.sp = round (mean(wind.speed, 
                                                na.rm = T),2),
                   stdev.wind.sp = round (sd(wind.speed, 
                                                na.rm = T), 2),
                   med.wind.sp = round(median(wind.speed,
                                                 na.rm = T), 2),
                   min.wind.sp = round(min(wind.speed,
                                              na.rm = T), 2),
                   max.wind.sp = round(max(wind.speed,
                                              na.rm = T), 2))
 
    ## d) save the data frame of summary stats as a pdf into output file
      pdf(here('output/univar_parent_care_trial.pdf'), height = 2, width = 27)
      grid.table(univar_parent_care_trial)
      dev.off()
                                              
      
      
###############################################################################
##############             4. Additional data tidying            ##############
###############################################################################
  
  ### 4.1 Additional data tidying based on data exploration
    # ## a) Extract early development total brooding duration for each nest
    #   early_nest_brood <- nestling_parent_care %>%
    #     select(nest.id, sample.state, total.brooding.duration, 
    #            total.brooding.duration.prop, tert.tot.brood) %>%
    #     filter(sample.state == 'early') %>%
    #     distinct(nest.id, .keep_all = T) %>%
    #     #mutate(row = row_number()) %>% # used to create unique identifier
    #     pivot_wider(names_from = sample.state,
    #                 values_from = c(total.brooding.duration,
    #                                 total.brooding.duration.prop,
    #                                 tert.tot.brood))
    #     #select(-row)
    #   
    # # b) Extract mid development total brooding duration for each nest
    #   mid_nest_brood <- nestling_parent_care %>%
    #     select(nest.id, sample.state, total.brooding.duration, 
    #            total.brooding.duration.prop, tert.tot.brood) %>%
    #     filter(sample.state == 'mid') %>%
    #     distinct(nest.id, .keep_all = T) %>%
    #     pivot_wider(names_from = sample.state,
    #                 values_from = c(total.brooding.duration,
    #                                 total.brooding.duration.prop,
    #                                 tert.tot.brood))
    #     
    # ## c) Left join early and mid brooding
    #   brood <- early_nest_brood %>%
    #     left_join(mid_nest_brood, by = c('nest.id' = 'nest.id'),
    #               copy = F)
    # 
    # ## d) Rename variables
    #   brood <- brood %>%
    #     rename(brood.dur.early = total.brooding.duration_early,
    #            prop.brood.dur.early = total.brooding.duration.prop_early,
    #            tert.brood.early = tert.tot.brood_early,
    #            brood.dur.mid = total.brooding.duration_mid,
    #            prop.brood.dur.mid = total.brooding.duration.prop_mid,
    #            tert.brood.mid = tert.tot.brood_mid)
    # 
    # ## e) Extract mid development total feeding visits for each nest
    #   mid_nest_feed <- nestling_parent_care %>%
    #     select(nest.id, sample.state, total.feeding.visits, 
    #            total.feeding.visits.rate, tert.tot.feed) %>%
    #     filter(sample.state == 'mid') %>%
    #     distinct(nest.id, .keep_all = T) %>%
    #     pivot_wider(names_from = sample.state,
    #                 values_from = c(total.feeding.visits,
    #                                 total.feeding.visits.rate, tert.tot.feed))
    #   
    # # f) Extract late development feeding visits for each nest
    #   late_nest_feed <- nestling_parent_care %>%
    #     select(nest.id, sample.state, total.feeding.visits, 
    #            total.feeding.visits.rate, tert.tot.feed) %>%
    #     filter(sample.state == 'late') %>%
    #     distinct(nest.id, .keep_all = T) %>%
    #     pivot_wider(names_from = sample.state,
    #                 values_from = c(total.feeding.visits,
    #                                 total.feeding.visits.rate, tert.tot.feed))
    #   
    # ## g) Left join early and mid brooding
    #   feed <- late_nest_feed %>%
    #     left_join(mid_nest_feed, by = c('nest.id' = 'nest.id'),
    #               copy = F)
    #   
    # ## h) Rename variables
    #   feed <- feed %>%
    #     rename(feed.vis.mid = total.feeding.visits_mid,
    #            feed.vis.rate.mid = total.feeding.visits.rate_mid,
    #            tert.feed.mid = tert.tot.feed_mid,
    #            feed.vis.late = total.feeding.visits_late,
    #            feed.vis.rate.late = total.feeding.visits.rate_late,
    #            tert.feed.late = tert.tot.feed_late)
    #   
    # ## i) Left join feed to brood data frame
    #   brood_feed <- brood %>%
    #     left_join(feed, by = c('nest.id' = 'nest.id'),
    #               copy = F)
      
    ## j) Get observation start time in usable format
      parent_care$obs.start.time.split <- 
        parent_care$obs.start.time
      
      parent_care <- separate(parent_care, col = obs.start.time.split,
                                       into = c("hour", "minute"), sep = ":")
      
      parent_care <- parent_care %>% 
        mutate(obs.start.24hr = 
                 as.numeric(hour) + 
                 as.numeric(minute)/60)
      
      parent_care$obs.start.24hr <- 
        as.numeric(parent_care$obs.start.24hr)
      
    ## k) Make site a factor
      parent_care$site <- as.factor(parent_care$site)
      
    ## l) Make nestID a factor
      parent_care$nest.id <- 
        as.factor(parent_care$nest.id)
      
    ## m) Create a variable that is the time from initial disturbance (i.e.,
      # time of nestling extraction) until observation start time
      nestling_parent_care$disturb.min <- with(nestling_parent_care, 
                                               difftime(obs.start.time, 
                                                        extract.time, 
                                                        units = 'mins'))
      
    ## n) Subset data to assess if care is influenced by nestling removal
      disturb_data <- nestling_parent_care %>%
        group_by(nest.id, sample.date) %>%
        arrange(disturb.min) %>%
        dplyr::slice_head() %>%
        select(nest.id, sample.date, disturb.min) %>%
        ungroup()
      
    ## o) Left join the disturb.min to parent_care data 
      parent_care <- parent_care %>%
        left_join(disturb_data, by = c('nest.id' = 'nest.id', 
                                       'obs.date' = 'sample.date'), 
                  copy = F)
   
    ## p) Format variable names
      source_path <- paste("~/WD/Git/source_code/")
      source(file = paste0(source_path, "format_var_names.R"))
      
      prim_merged <- FormatVarNames(prim_merged)
      
 #***NOTE: Remove Hayes 7 and Schaaps 131.2 because not used in main analysis
     #* Hayes 7 no mid and late nestling measurements (depredation)
    #* Schaaps 131.2 second brood
      prim_merged <- prim_merged  %>%
        filter(nest.id != 'hayes 7') %>%
        filter(nest.id != 'schaaps 131') 
    
    ## q) Left join the disturb.min to parent_care data 
      parent_care <- prim_merged %>%
        select(c(nest.id, obs.date, obs.med.temp)) %>%
        left_join(parent_care, by = c('nest.id' = 'nest.id', 
                                       'obs.date' = 'obs.date'), 
                  copy = F)  
      
    ## p) Clean up environment
      rm(disturb_data, nestl_merged, noaa, prim_merged, govee_daily
         #, early_nest_brood, mid_nest_brood, mid_nest_feed, 
         #late_nest_feed, brood, feed
         )
      
    

###############################################################################
##############         5. Determinants of parental care          ##############
###############################################################################   
      
  ### 5.1 Assess potential determinants of parental care      
    ## a) Double check variable class and behavior in models
      class(parent_care$nest.id)
      # checked to make sure difftime class functions as expected in model
      # it does
      class(parent_care$disturb.min)
      # parent_care$disturb.min <- as.numeric(parent_care$disturb.min)
    
    ## b) Association between nest age and feeding rate
      feed.by.age.lm <- lme(total.feeding.visits.rate ~ nestling.age, 
                                random = ~1|nest.id,
                                data = subset(parent_care,
                                            # obs.state != 'early' &         
                                        !is.na(x = nestling.age) &
                                        !is.na(x = total.feeding.visits.rate)))  
      
      summary(feed.by.age.lm)    # model summary 
      intervals(feed.by.age.lm)    # 95% CIs
      #plot(feed.by.age.lm)       # check residuals
      
    ## c) Association between brood size and feeding rate
      feed.by.brood.sz.lm <- lme(total.feeding.visits.rate ~ nestling.number, 
                            random = ~1|nest.id,
                            data = subset(parent_care,
                                      # obs.state != 'early' &         
                                        !is.na(x = nestling.number) &
                                        !is.na(x = total.feeding.visits.rate)))  
      
      summary(feed.by.brood.sz.lm)    # model summary 
      intervals(feed.by.brood.sz.lm)    # 95% CIs
      #plot(feed.by.brood.sz.lm)       # check residuals
      
    ## d) Association between median temp and feeding rate
      feed.by.med.temp.lm <- lme(total.feeding.visits.rate ~ obs.med.temp, 
                                 random = ~1|nest.id,
                                 data = subset(parent_care,
                                      # obs.state != 'early' &         
                                        !is.na(x = obs.med.temp) &
                                        !is.na(x = total.feeding.visits.rate)))  
      
      summary(feed.by.med.temp.lm)    # model summary 
      intervals(feed.by.med.temp.lm)    # 95% CIs
      #plot(feed.by.med.temp.lm)       # check residuals
      
    ## e) Association between disturbance time and feeding rate: if removing
      # nestlings from nests impacts parental behavior, then predict that
      # a positive association (longer time since disturbance = more feeding)
      feed.by.disturb.lm <- lme(total.feeding.visits.rate ~ disturb.min, 
                                random = ~1|nest.id,
                                data = subset(parent_care,
                                          # obs.state != 'early' &         
                                            !is.na(x = disturb.min) &
                                            !is.na(x = total.feeding.visits.rate))) 
      
      summary(feed.by.disturb.lm)    # model summary 
      intervals(feed.by.disturb.lm)    # 95% CIs
      #plot(feed.by.disturb.lm)       # check residuals
      

      
###############################################################################
##############          6. Summarize parental care data          ##############
###############################################################################  
      
  ### 6.1  Calculate BLUPs from parental care data set (parent_care)
      
      # NOTE: Use when there are repeated measuresments for a variable
      # that is to be used as an explanatory variable in another analysis.
      # Can control for other variables that bias estimates of explanatory
      # variable
      
      # NOTE: BLUPs are conditional modes from a generalized linear model
      # (according to Doug Bates). 
      # BLUP = fixef(intrcpt) + ranef
    
    ## a) Feeding BLUPs models      
      # Model using lmer 
      feeding_blups_lmm <- lmer(total.feeding.visits ~ scale(nestling.age) + 
                                  scale(nestling.number) + scale(obs.med.temp) + 
                                  scale(disturb.min) +
                                  offset(obs.duration/3600) +
                                  (1|nest.id),
                                data = subset(parent_care,
                                              is.na(total.feeding.visits) == F &
                                                is.na(nestling.age) == F &
                                                is.na(nestling.number) == F &
                                                is.na(obs.med.temp) == F)
      )
      
      plot(feeding_blups_lmm)
      # Residuals look a bit cone shaped
      
    ## b) Poisson
      feeding_blups_glmm_poss <- glmer(total.feeding.visits ~ scale(nestling.age) + 
                                         scale(nestling.number) + scale(obs.med.temp) + 
                                         scale(disturb.min) +
                                         offset(obs.duration/3600) +
                                         (1|nest.id),
                                       data = subset(parent_care,
                                                     is.na(total.feeding.visits) == F &
                                                       is.na(nestling.age) == F &
                                                       is.na(nestling.number) == F &
                                                       is.na(obs.med.temp) == F),
                                       family = poisson()
      )
      
      simulation_output <- simulateResiduals(fittedModel = feeding_blups_glmm_poss, 
                                             plot = T)
      # Not terrible but some issues
      
    ## c) Negative binomial
      feeding_blups_glmm_nb <- glmer.nb(total.feeding.visits ~ scale(nestling.age) + 
                                          scale(nestling.number) + scale(obs.med.temp) + 
                                          scale(disturb.min) +
                                          offset(obs.duration/3600) +
                                          (1|nest.id),
                                        data = subset(parent_care,
                                                      is.na(total.feeding.visits) == F &
                                                        is.na(nestling.age) == F &
                                                        is.na(nestling.number) == F &
                                                        is.na(obs.med.temp) == F))
      
      simulation_output.nb <- simulateResiduals(fittedModel = feeding_blups_glmm_nb, 
                                             plot = T)
      # Looks pretty good
      summary(feeding_blups_glmm_nb)
      confint(feeding_blups_glmm_nb)
      
    ## d) Compare model fit with AICtab (from bbmle)
      AICtab(feeding_blups_lmm, feeding_blups_glmm_poss, feeding_blups_glmm_nb)
      
      
  ### 6.2 Extract feeding BLUPs, individual variation in feeding visits
      # from the best fitting model (above)
      
    ## a) Generate best fit model summary
      ranef(feeding_blups_glmm_nb) # random effect
      fixef(feeding_blups_glmm_nb) # fixed effect
      coef(feeding_blups_glmm_nb) # fixed effect
      
     
    ## b) extract BLUPs from mixed model object
      feeding_blups <- as.data.frame(ranef(feeding_blups_glmm_nb)) # extract ranef as
      # a dataframe, BLUPs = rand effects + intercept (from poiss/neg. binom)
      
    ## c) Rename variables in blups table
      feeding_blups <- feeding_blups %>%
        rename('nest.id' = 'grp') %>%
        rename('feeding.ranef' = 'condval') %>%
        rename('feeding.ranef.sd' = 'condsd') %>%
        select(c('nest.id', 'feeding.ranef', 'feeding.ranef.sd'))
      
    ## d) extract fixed effect (intercept) from poisson/neg. binomial model
      feeding.intrcpt <- (fixef(feeding_blups_glmm_nb)[[1]])[[1]] # fixed effect
      #* INTERPETATION ****#  
      # Interpet as expected log count of behavior when controlling for /
      # holding constant the effects of covariates (or if exponentiated
      # the intercept is the incident rate of the expected count
      # of behavior) as a proporiton of time overlap (the offset)
      
    ## f) Create a new variable that is ranef plus both poisson/neg. binomial
      # model intercept. This provides estimates of individual level
      # variation in proportion of time spent of obs vs feeding visits
      feeding_blups <-  feeding_blups  %>%
        mutate(feed.blup = feeding.intrcpt + feeding.ranef) %>%
        mutate(feed.blup.exp = exp(feed.blup))
      #* INTERPETATION ****#  
      # Interpret as the conditional mode or the log counts / incident rate
      # of expected counts as a proportion of the overlap time (offset) for
      # each individual...while holding constant effect of other covariates
      

  # ### 6.3 Calculate BLUPs from parental care data set (parent_care)
  #   ## a) Brooding BLUPs
  #     # Model using lmer 
  #     brooding_blups_lmm <- lmer((total.brooding.duration/60) ~ scale(nestling.age) + 
  #                                  scale(nestling.number) + scale(obs.med.temp) + 
  #                                  scale(disturb.min) +
  #                                  offset(obs.duration/3600) +
  #                                  (1|nest.id),
  #                                data = subset(parent_care,
  #                                         #obs.state != 'early' &
  #                                         is.na(total.brooding.duration) == F &
  #                                         is.na(nestling.age) == F &
  #                                         is.na(nestling.number) == F &
  #                                         is.na(obs.med.temp) == F),
  #     )
  #   
  #     plot(brooding_blups_lmm)
  #     # Doesn't look great
  #     summary(brooding_blups_lmm)
  #     
  #     
  # ### 6.4 Extract brooding BLUPs, individual variation in brooding effort
  #   ## a) Generate model summary
  #     ranef(brooding_blups_lmm) # random effect
  #     fixef(brooding_blups_lmm) # fixed effect
  #     coef(brooding_blups_lmm) # fixed effect
  #     
  #   ## b) Eextract BLUPs from mixed model object
  #     brooding_blups <- as.data.frame(ranef(brooding_blups_lmm)) # extract ranef as
  #     # a dataframe, BLUPs = rand effects + intercept (from poiss/neg. binom)
  #     
  #   ## c) Rename variables in blups table
  #     brooding_blups <- brooding_blups %>%
  #       rename('nest.id' = 'grp') %>%
  #       rename('brooding.ranef' = 'condval') %>%
  #       rename('brooding.ranef.sd' = 'condsd') %>%
  #       select(c('nest.id', 'brooding.ranef', 'brooding.ranef.sd'))
  #     
  #   ## d) extract fixed effect (intercept) from poisson/neg. binomial model
  #     brooding.intrcpt <- (fixef(brooding_blups_lmm)[[1]])[[1]] # fixed effect
  #     
  #   ## f) Create a new variable that is ranef plus model intercept.
  #     brooding_blups <-  brooding_blups  %>%
  #       mutate(brood.blup = brooding.intrcpt + brooding.ranef)

      
  ### 6.5 Tidy BLUPs data 
    ## a) join BLUPs
      care_blups <- feeding_blups 
        # %>%
        # left_join(brooding_blups, by = c('nest.id' = 'nest.id'),
        #           copy = F)
      
    ## b) Create parental care categorical variable based on feeding BLUPs
      care_blups <- care_blups %>%
        mutate(feed.indx =  as.integer(Hmisc::cut2(feed.blup.exp, g=2)))
      
    # ## c) Create parental care categorical variable based on feeding BLUPs
    #   care_blups <- care_blups %>%
    #     mutate(brood.indx =  as.integer(Hmisc::cut2(brood.blup, g=2)))
      
    ## d) Re-label the brood.indx factor levels
      care_blups <- care_blups %>%
        mutate(feed.indx =
                        case_when(feed.indx == 1
                           ~ c('low'),
                           feed.indx == 2
                           ~ c('avg')
                           # ,
                           # feed.indx == 3
                           # ~ c('hi')
                           ))
      
    ## e) Re-code *nominal* factor (with ordered levels)
      # Set levels (ordering) of feed.indx variable 
      care_blups <- transform(care_blups, 
                              feed.indx = factor(feed.indx,
                                                   levels = c("low", "avg"
                                                              # , "hi"
                                                 )))

    # ## f) Re-label the brood.indx factor levels
    #   care_blups <- care_blups %>%
    #     mutate(brood.indx = case_when(brood.indx == 1
    #                        ~ c('low'),
    #                        brood.indx == 2
    #                        ~ c('avg')
    #                        # ,
    #                        # brood.indx == 3
    #                        # ~ c('hi')
    #                        ))
    # 
    # ## g) Re-code *nominal* factor (with ordered levels)
    #   # Set levels (ordering) of brood.indx variable
    #   care_blups <- transform(care_blups,
    #                           brood.indx = factor(brood.indx,
    #                                                 levels = c("low", "avg"
    #                                                            # , "hi"
    #                                               )))

    # ## h) Combine feeding and brooding BLUPs
    #   care_blups <- care_blups %>%
    #     mutate(care.indx = feed.blup.exp + brood.blup) %>% 
    #     mutate(care.indx =  as.integer(Hmisc::cut2(care.indx, g=2)))
    #   
    # ## i) Re-label the care.indx.3 factor levels
    #   care_blups <- care_blups %>%
    #     mutate(care.indx = case_when(care.indx == 1
    #                        ~ c('low'),
    #                        care.indx == 2
    #                        ~ c('avg')
    #                        # ,
    #                        # care.indx == 3
    #                        # ~ c('hi')
    #                        ))
    #   
    # ## j) Re-code *nominal* factor (with ordered levels)
    #   # Set levels (ordering) of brood.indx variable 
    #   care_blups <- transform(care_blups, 
    #                           care.indx = factor(care.indx,
    #                                                 levels = c("low", "avg"
    #                                                            # , "hi"
    #                                              )))
    #   
      
  # ### 6.6 Summarize parental care data
  #   #NOTE: Create index based on 3 behaviors (early brooding, mid feeding,
  #     # and late feeding) 
  #     
  #   ## a) Create parental care behavior sum adding tertile values for total 
  #       # early brooding duration and total mid and late feeding visits
  #     brood_feed <- brood_feed %>%
  #       rowwise() %>%
  #       mutate(care.sum.3 = sum(tert.brood.early, tert.feed.mid, 
  #                                tert.feed.late, na.rm = T))  %>%
  #       mutate(care.sum.3 = na_if(care.sum.3, 0)) %>%
  #       ungroup()
  #       
  #   ## b) Create parental care behavior possible total to account for 
  #     # missing data
  #     brood_feed <- brood_feed %>%
  #       mutate(care.tot.3 = case_when(
  #               !is.na(tert.brood.early) &
  #               !is.na(tert.feed.mid) &
  #               !is.na(tert.feed.late)
  #                 ~ 9,
  #               is.na(tert.brood.early) &
  #               !is.na(tert.feed.mid) &
  #               !is.na(tert.feed.late)
  #               ~ 6,
  #               !is.na(tert.brood.early) &
  #               is.na(tert.feed.mid) &
  #               !is.na(tert.feed.late)
  #               ~ 6,
  #               !is.na(tert.brood.early) &
  #               !is.na(tert.feed.mid) &
  #               is.na(tert.feed.late)
  #               ~ 6,
  #               is.na(tert.brood.early) &
  #               is.na(tert.feed.mid) &
  #               !is.na(tert.feed.late)
  #               ~ 3,
  #               is.na(tert.brood.early) &
  #               !is.na(tert.feed.mid) &
  #               is.na(tert.feed.late)
  #               ~ 3,
  #               !is.na(tert.brood.early) &
  #               is.na(tert.feed.mid) &
  #               is.na(tert.feed.late)
  #               ~ 3)) 
  #     
  #   ## c) Create continuous score of parental care behaviors 
  #       # sum of behavior tertile/total possible tertile sum
  #     brood_feed <- brood_feed %>%
  #       mutate(care.indx.cont.3 = care.sum.3/care.tot.3) %>%
  #       mutate(care.indx.3 =  as.integer(Hmisc::cut2(care.indx.cont.3, g=3)))
  #     
  #           # Hmisc alternative using cut and quantile   
  #           #    (cut(care.indx.cont.3, 
  #           #         quantile(care.indx.cont.3, probs=0:3/3,
  #           #                  na.rm = T), 
  #           #         include.lowest = T)))
  #     # don't use ntile, which allows the same value to occur in mult. quantile         
  #     #as.factor(ntile(care.indx.cont.3, 3))) 
  # 
  #   ## d) Re-label the care.indx.3 factor levels
  #     brood_feed <- brood_feed %>%
  #       mutate(care.indx.3 =
  #                case_when(care.indx.3 == 1
  #                          ~ c('low'),
  #                          care.indx.3 == 2
  #                          ~ c('avg'),
  #                          care.indx.3 == 3
  #                          ~ c('hi')))
  # 
  #   ## e) Re-code *nominal* factor (with ordered levels)
  #    # Set levels (ordering) of care.indx.3 variable 
  #     brood_feed <- transform(brood_feed, 
  #                             care.indx.3 = factor(care.indx.3,
  #                                                levels = c("low", "avg", 
  #                                                           "hi")))
  #     
  #     class(brood_feed$care.indx.3)
    #   
    # ## f) Select columns 
    #   brood_feed <- brood_feed %>%
    #     select(c(nest.id, care.indx.3))
    #     
    # ## f) Left join brood_feed to care_blups 
    #   brood_feed <- care_blups %>%
    #     left_join(brood_feed, by = c('nest.id' = 'nest.id'),
    #               copy = F)
    #   
      
      
###############################################################################
##############     7. Combine parent care and nestling data      ##############
###############################################################################   
      

  ### 7.1 Format and tidy data to join nest level parental care measures with
    # late developmental stage size glucose measures  
      
    ## a) Subset the data to include only the late (~day 12) measurements
      late_nestling_parent_care <- nestling_parent_care %>%
        filter(sample.state == 'late') %>%
        select(-c(total.visits:tert.tot.brood))
      
    ## c) Left join brood to late_nestling_parent_care data frame
      late_nestling_parent_care <- late_nestling_parent_care %>%
        left_join(care_blups, by = c('nest.id' = 'nest.id'),
                  copy = F)
      
        
                                                                 
  ### 7.2 Create growth variable and update late_nestling_parent_care df   
    ## a) Calculate difference between day 8 minus day 12 size based the 
      # rt. wing length
      diff_size <- nestling_parent_care %>%
        select(nestling.band, sample.state, rt.wing.length) %>%
        filter(sample.state != 'early') %>%
        filter(!is.na(rt.wing.length)) %>%
        pivot_wider(id_cols = nestling.band, names_from = sample.state, 
                    values_from = rt.wing.length) %>% 
        mutate(rt.wing.diff = (late - mid))
      
    ## b) Rename variables
      diff_size <- diff_size %>%
        rename(mid.rt.wing.length = mid,
               late.rt.wing.length = late)
      
    ## c) Left join diff_size to late_nestling_parent_care
      late_nestling_parent_care <- late_nestling_parent_care %>%
        left_join(diff_size, by = c('nestling.band' = 'nestling.band'), 
                  copy = F)
      
    ## d) Pivot glucose long to wide 
      base_gluc <- nestling_parent_care %>%
        select(nestling.band, sample.state, base.gluc) %>%
        filter(sample.state != 'early') %>%
        filter(!is.na((nestling.band))) %>%
        pivot_wider(id_cols = nestling.band, names_from = sample.state, 
                    values_from = base.gluc)
        
    ## e) Rename variables
      base_gluc <- base_gluc %>%
        rename(mid.base.gluc = mid,
               late.base.gluc = late)
      
    ## f) Pivot glucose long to wide 
      stress_gluc <- nestling_parent_care %>%
        select(nestling.band, sample.state, stress.gluc) %>%
        filter(sample.state != 'early') %>%
        filter(!is.na((nestling.band))) %>%
        pivot_wider(id_cols = nestling.band, names_from = sample.state, 
                    values_from = stress.gluc)
      
    ## g) Rename variables
      stress_gluc <- stress_gluc %>%
        rename(mid.stress.gluc = mid,
               late.stress.gluc = late)
      
    ## h) Left join base_gluc to late_nestling_parent_care
      late_nestling_parent_care <- late_nestling_parent_care %>%
        left_join(base_gluc, by = c('nestling.band' = 'nestling.band'), 
                  copy = F)
      
      late_nestling_parent_care <- late_nestling_parent_care %>%
        left_join(stress_gluc, by = c('nestling.band' = 'nestling.band'), 
                  copy = F)
      
      
###############################################################################
##############         8. Bivariate descriptive stats            ##############
###############################################################################  
      
  ### 8.1 Bivariate descriptive stats 
      
    ## a) Blood glucose by feeding BLUP indx 
      bivar_gluc_feed_indx <- late_nestling_parent_care %>%
        group_by(feed.indx) %>%
        summarise (n.mid.base.gluc = sum(!is.na(mid.base.gluc)),
                   avg.mid.base.gluc = round (mean(mid.base.gluc, 
                                               na.rm = T),2),
                   stdev.mid.base.gluc = round (sd(mid.base.gluc, 
                                               na.rm = T), 2),
                   med.mid.base.gluc = round(median(mid.base.gluc,
                                                na.rm = T), 2),
                   min.mid.base.gluc = round(min(mid.base.gluc,
                                             na.rm = T), 2),
                   max.mid.base.gluc = round(max(mid.base.gluc,
                                             na.rm = T), 2),
                   n.mid.stress.gluc = sum(!is.na(mid.stress.gluc)),
                   avg.mid.stress.gluc = round (mean(mid.stress.gluc, 
                                               na.rm = T),2),
                   stdev.mid.stress.gluc = round (sd(mid.stress.gluc, 
                                               na.rm = T), 2),
                   med.mid.stress.gluc = round(median(mid.stress.gluc,
                                                na.rm = T), 2),
                   min.mid.stress.gluc = round(min(mid.stress.gluc,
                                             na.rm = T), 2),
                   max.mid.stress.gluc = round(max(mid.stress.gluc,
                                             na.rm = T), 2),
                   
                   n.late.base.gluc = sum(!is.na(late.base.gluc)),
                   avg.late.base.gluc = round (mean(late.base.gluc, 
                                                   na.rm = T),2),
                   stdev.late.base.gluc = round (sd(late.base.gluc, 
                                                   na.rm = T), 2),
                   med.late.base.gluc = round(median(late.base.gluc,
                                                    na.rm = T), 2),
                   min.late.base.gluc = round(min(late.base.gluc,
                                                 na.rm = T), 2),
                   max.late.base.gluc = round(max(late.base.gluc,
                                                 na.rm = T), 2),
                   n.late.stress.gluc = sum(!is.na(late.stress.gluc)),
                   avg.late.stress.gluc = round (mean(late.stress.gluc, 
                                                     na.rm = T),2),
                   stdev.late.stress.gluc = round (sd(late.stress.gluc, 
                                                     na.rm = T), 2),
                   med.late.stress.gluc = round(median(late.stress.gluc,
                                                      na.rm = T), 2),
                   min.late.stress.gluc = round(min(late.stress.gluc,
                                                   na.rm = T), 2),
                   max.late.stress.gluc = round(max(late.stress.gluc,
                                                   na.rm = T), 2))
      
      
    ## b) Size/growth by feeding BLUP indx 
      bivar_size_feed_indx <- late_nestling_parent_care %>%
        group_by(feed.indx) %>%
        summarise (n.mid.size = sum(!is.na(mid.rt.wing.length)),
                   avg.mid.size = round (mean(mid.rt.wing.length, 
                                              na.rm = T),2),
                   stdev.mid.size = round (sd(mid.rt.wing.length, 
                                              na.rm = T), 2),
                   med.mid.size = round(median(mid.rt.wing.length,
                                               na.rm = T), 2),
                   min.mid.size = round(min(mid.rt.wing.length,
                                            na.rm = T), 2),
                   max.mid.size = round(max(mid.rt.wing.length,
                                            na.rm = T), 2),
                   n.late.size = sum(!is.na(late.rt.wing.length)),
                   avg.late.size = round (mean(late.rt.wing.length, 
                                               na.rm = T),2),
                   stdev.late.size = round (sd(late.rt.wing.length, 
                                               na.rm = T), 2),
                   med.late.size = round(median(late.rt.wing.length,
                                                na.rm = T), 2),
                   min.late.size = round(min(late.rt.wing.length,
                                             na.rm = T), 2),
                   max.late.size = round(max(late.rt.wing.length,
                                             na.rm = T), 2),
                   n.grow = sum(!is.na(rt.wing.diff)),
                   avg.grow = round (mean(rt.wing.diff, 
                                               na.rm = T),2),
                   stdev.grow = round (sd(rt.wing.diff, 
                                               na.rm = T), 2),
                   med.grow = round(median(rt.wing.diff,
                                                na.rm = T), 2),
                   min.grow = round(min(rt.wing.diff,
                                             na.rm = T), 2),
                   max.grow = round(max(rt.wing.diff,
                                             na.rm = T), 2))
      
    # ## c) Blood glucose brooding BLUP indx 
    #   bivar_gluc_brood_indx <- late_nestling_parent_care %>%
    #     group_by(brood.indx) %>%
    #     summarise (n.base.gluc = sum(!is.na(base.gluc)),
    #                avg.base.gluc = round (mean(base.gluc, 
    #                                            na.rm = T),2),
    #                stdev.base.gluc = round (sd(base.gluc, 
    #                                            na.rm = T), 2),
    #                med.base.gluc = round(median(base.gluc,
    #                                             na.rm = T), 2),
    #                min.base.gluc = round(min(base.gluc,
    #                                          na.rm = T), 2),
    #                max.base.gluc = round(max(base.gluc,
    #                                          na.rm = T), 2),
    #                n.gluc.diff = sum(!is.na(gluc.diff)),
    #                avg.gluc.diff = round (mean(gluc.diff, 
    #                                            na.rm = T),2),
    #                stdev.gluc.diff = round (sd(gluc.diff, 
    #                                            na.rm = T), 2),
    #                med.gluc.diff = round(median(gluc.diff,
    #                                             na.rm = T), 2),
    #                min.gluc.diff = round(min(gluc.diff,
    #                                          na.rm = T), 2),
    #                max.gluc.diff = round(max(gluc.diff,
    #                                          na.rm = T), 2))
    #   
    # ## c) Blood glucose by feeding + brooding BLUP indx, 'care_indx'
    #   bivar_care_indx <- late_nestling_parent_care %>%
    #     group_by(care.indx) %>%
    #     summarise (n.base.gluc = sum(!is.na(base.gluc)),
    #                avg.base.gluc = round (mean(base.gluc, 
    #                                            na.rm = T),2),
    #                stdev.base.gluc = round (sd(base.gluc, 
    #                                            na.rm = T), 2),
    #                med.base.gluc = round(median(base.gluc,
    #                                             na.rm = T), 2),
    #                min.base.gluc = round(min(base.gluc,
    #                                          na.rm = T), 2),
    #                max.base.gluc = round(max(base.gluc,
    #                                          na.rm = T), 2),
    #                n.gluc.diff = sum(!is.na(gluc.diff)),
    #                avg.gluc.diff = round (mean(gluc.diff, 
    #                                            na.rm = T),2),
    #                stdev.gluc.diff = round (sd(gluc.diff, 
    #                                            na.rm = T), 2),
    #                med.gluc.diff = round(median(gluc.diff,
    #                                             na.rm = T), 2),
    #                min.gluc.diff = round(min(gluc.diff,
    #                                          na.rm = T), 2),
    #                max.gluc.diff = round(max(gluc.diff,
    #                                          na.rm = T), 2))
    #   
    #   
    # ## d)  Blood glucose by feeding + brooding indx 
    #   # (early brood + mid feed + late feed), 'care_indx.3'
    #   bivar_gluc_care_indx3 <- late_nestling_parent_care %>%
    #     group_by(care.indx.3) %>%
    #     summarise (n.base.gluc = sum(!is.na(base.gluc)),
    #                avg.base.gluc = round (mean(base.gluc, 
    #                                       na.rm = T),2),
    #                stdev.base.gluc = round (sd(base.gluc, 
    #                                       na.rm = T), 2),
    #                med.base.gluc = round(median(base.gluc,
    #                                        na.rm = T), 2),
    #                min.base.gluc = round(min(base.gluc,
    #                                     na.rm = T), 2),
    #                max.base.gluc = round(max(base.gluc,
    #                                     na.rm = T), 2),
    #                n.gluc.diff = sum(!is.na(gluc.diff)),
    #                avg.gluc.diff = round (mean(gluc.diff, 
    #                                           na.rm = T),2),
    #                stdev.gluc.diff = round (sd(gluc.diff, 
    #                                           na.rm = T), 2),
    #                med.gluc.diff = round(median(gluc.diff,
    #                                            na.rm = T), 2),
    #                min.gluc.diff = round(min(gluc.diff,
    #                                         na.rm = T), 2),
    #                max.gluc.diff = round(max(gluc.diff,
    #                                         na.rm = T), 2))
      
      
      ## a) Histogram total feeding rates by developmental state 
      # relative nestling size boxplot
      mid_gluc_size_box <- nestling_parent_care %>%
        filter(sample.state == 'mid') %>%
        ggplot(aes(x = rt.wing.length, y = gluc.diff, fill = size.order)) + 
        geom_boxplot() + 
        scale_fill_viridis(discrete = TRUE, alpha=0.6) +
        geom_jitter(color="black", size=2, alpha=0.9) +
        # scale_color_manual(values=c('#69b3a2', '#404080'), 
        #                    name = 'smallest vs other nestlings',
        #                    labels = c('small', 'other')) +
        labs(title = 'Mid-development difference in blood glucose (stressed - baseline) by 
             mid-development relative size',
             y ='Diff. glucose (mg/dl)') +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        theme(axis.ticks = element_blank()) + # remove axis ticks
        theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
        # customize legend
        theme(legend.title=element_text(size=10),
              legend.text=element_text(size=8),
              legend.position = c(0.91, 0.94))
      
      ## b) Print plot 
      print(mid_gluc_size_box)
      
      ## c) Save plot
      ggsave('mid_gluc_size_box.pdf', plot = mid_gluc_size_box, 
             device = NULL, 
             path = here('output/'), scale = 1, width = 8, 
             height = 6, 
             units = c('in'), dpi = 300, limitsize = TRUE) 
      
    #   


###############################################################################
##############               9. Export data files                ##############
###############################################################################
      
  ### 9.1 Export data to an RData file     
    ## a) Save and export raw data tables 
      # Files are saved in the 'data' folder in the working directory as an
      # RData file.
      save(file = here('data/6_bs_phys_sz_care_data.RData'), 
           list = c('late_nestling_parent_care'))
      
    
               