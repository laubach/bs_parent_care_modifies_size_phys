###############################################################################
#############              Barn swallow parental care             #############
#############               and offspring physiology              #############
#############                                                     #############
#############                5. Descriptive Stats:                #############
#############               Parental care behaviors               #############
#############                  By: Zach Laubach                   #############
#############                created: 3 June 2022                 #############
#############              last updated: 20 Sept 2024              #############
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
      
    
      
  ### 3.7 Univariate descriptive stats parental care behaviors           
    ## a) Descriptive stats parental care behaviors
      univar_parent_care_behav <- parent_care %>%
        group_by(obs.state) %>%
        summarise(
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
      
    ## a) Get observation start time in usable format
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
      
    ## b) Make site a factor
      parent_care$site <- as.factor(parent_care$site)
      
    ## c) Make nestID a factor
      parent_care$nest.id <- 
        as.factor(parent_care$nest.id)
      
    ## d) Create a variable that is the time from initial disturbance (i.e.,
      # time of nestling extraction) until observation start time
      nestling_parent_care$disturb.min <- with(nestling_parent_care, 
                                               difftime(obs.start.time, 
                                                        extract.time, 
                                                        units = 'mins'))
      
    ## e) Subset data to assess if care is influenced by nestling removal
      disturb_data <- nestling_parent_care %>%
        group_by(nest.id, sample.date) %>%
        arrange(disturb.min) %>%
        dplyr::slice_head() %>%
        select(nest.id, sample.date, disturb.min) %>%
        ungroup()
      
    ## f) Left join the disturb.min to parent_care data 
      parent_care <- parent_care %>%
        left_join(disturb_data, by = c('nest.id' = 'nest.id', 
                                       'obs.date' = 'sample.date'), 
                  copy = F)
   
    ## g) Format variable names
      source_path <- paste("~/WD/Git/source_code/")
      source(file = paste0(source_path, "format_var_names.R"))
      
      prim_merged <- FormatVarNames(prim_merged)
      
 #***NOTE: Remove Hayes 7 and Schaaps 131.2 because not used in main analysis
     #* Hayes 7 no mid and late nestling measurements (depredation)
    #* Schaaps 131.2 second brood
      prim_merged <- prim_merged  %>%
        filter(nest.id != 'hayes 7') %>%
        filter(nest.id != 'schaaps 131') 
    
    ## h) Left join the disturb.min to parent_care data 
      parent_care <- prim_merged %>%
        select(c(nest.id, obs.date, obs.med.temp)) %>%
        left_join(parent_care, by = c('nest.id' = 'nest.id', 
                                       'obs.date' = 'obs.date'), 
                  copy = F)  
      
    ## i) Clean up environment
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
      
    ## e) # Marginal and conditional R-squared
      r2_nakagawa(feeding_blups_glmm_nb)
      #, by_group=T)
      
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
      

  ### 6.3 Tidy BLUPs data 
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
      
      
      
  ### 8.2 Visual assessment of raw parental care data vs categories based on BLUPs 
    ## a) summarize the raw parental care data
      avg_feed_vs_feed_indx <-parent_care %>%
        group_by(nest.id) %>%
        summarise (n.feed = sum(!is.na(total.feeding.visits)),
                   avg.feed = round (mean(total.feeding.visits, 
                                              na.rm = T),2)) %>%
        filter(avg.feed != 'NaN')
      
    ## b) join avg parental care data to care_blups
      care_blups <- care_blups %>%
        left_join(avg_feed_vs_feed_indx, by = c('nest.id' = 'nest.id'), 
                  copy = F)
      
    ## c) Histogram avg feeding counts by BLUP based categories
      # relative nestling size boxplot
      avg_feed_by_parent_care <- care_blups %>%
        ggplot(aes(x = feed.indx, y = avg.feed, fill = feed.indx)) + 
        geom_boxplot() + 
        scale_x_discrete(labels=c('low' = 'low', 'avg' = 'high')) +
        geom_point(aes(x = feed.indx, y = avg.feed, fill = feed.indx),
                   color = 'black', shape = 21, # black outline around points
                   size = 3, position = position_jitterdodge(0.5,
                                                             dodge.width = .1)) +
        scale_fill_manual(name = 'parental care', labels = c('low', 'high'), 
                          values=c('burlywood4', 'purple4')) +
       
        labs(title = 'The average count of feeding visits for by level of parental care 
as determined by the feeding BLUPs') +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        # bold and size title and axes labels
        theme(text = element_text(size=20, face = 'bold')) +
        #theme(legend.position = 'none') +
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
        # add major axes
        theme(axis.line = element_line(colour = 'black',
                                       size = 0.5, linetype = 'solid')) +
        
        # change axes font style, color, size, angle, margin, and legend
        theme(axis.text.x = element_text(face='bold', color='black', 
                                         size=20, angle=0,
                                         margin = margin(t = 10, r = 0, 
                                                         b = 10, l = 0)),
              axis.text.y = element_text(face='bold', color='black', 
                                         size=20, angle=0, 
                                         margin = margin(t = 0, r = 10, 
                                                         b = 0, l = 0)),
              legend.title = element_text(size = 16),
              legend.text = element_text(size=14),
              legend.position = 'none',
              legend.key = element_blank()) +
        xlab(expression(bold('parental care levels (based on BLUPs)'))) +
        ylab(expression(bold('average count of feeding visits'))) 
      
    ## d) Print plot 
      print(avg_feed_by_parent_care)
      
    ## e) Save plot
      ggsave('avg_feed_by_parent_care.pdf', plot = avg_feed_by_parent_care, 
             device = NULL, 
             path = here('output/'), scale = 1, width = 8, 
             height = 6, 
             units = c('in'), dpi = 300, limitsize = TRUE) 
      



###############################################################################
##############               9. Export data files                ##############
###############################################################################
      
  ### 9.1 Export data to an RData file     
    ## a) Save and export raw data tables 
      # Files are saved in the 'data' folder in the working directory as an
      # RData file.
      save(file = here('data/6_bs_phys_sz_care_data.RData'), 
           list = c('late_nestling_parent_care'))
      
    
               