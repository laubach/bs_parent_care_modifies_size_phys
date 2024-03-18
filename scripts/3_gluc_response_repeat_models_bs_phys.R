###############################################################################
#############              Barn swallow parental care             #############
#############               and offspring physiology              #############
#############                                                     #############
#############               3. Glucose response and               #############
#############                 repeatability models                #############
#############                                                     #############
#############                  By: Zach Laubach                   #############
#############                created: 23 May 2022                 #############
#############              last updated: 7 Feb 2024               #############
###############################################################################


### PURPOSE: Models of glucose response to a standardized stressor and glucose
            # repeatability across development.
  
  
  # Code Blocks
    # 1: Configure work space
    # 2: Load RData
    # 3: Glucose response models
    # 4: Repeatability models
    # 5: Graph model results



###############################################################################
##############             1.  Configure work space              ##############
###############################################################################

  ### 1.1 Global options
    ## a) clear global environment
      rm(list = ls())

    ## b) prevent R from automatically reading charater strins as factors
      options(stringsAsFactors = FALSE)
  

  ### 1.2 Install and load CRAN packages   
    ## a) Data Manipulation and Descriptive Stats Packages
      # load tidyverse packages
        library('tidyverse')

      # load here packages
        library('here')
     
    ## b) Graph Plotting and Visualization Packages
      # load ggplot2 packages
      library ('ggplot2')
      
      library('hrbrthemes')
  
      library('viridis')

      # load gridExtra packages
      library('gridExtra')
      
    ## c) Modeling Packages
      # load lme4 packages
      library ('lme4')
      
      # load broom packages
      library('broom')
      library('broom.mixed')
      
      # load dharma
      library('DHARMa')
      
      # load irrNA (for calculating ICC)
      library('irrNA')
      
      # load irrICC (for calculating ICC)
      library('irrICC')
      
      # load emmeans packages
      library ('emmeans')
      
      # load pbkrtest and lmertest (emmeans dependency)
      library('pbkrtest')
      library('lmerTest')
      
      # prevent lmerTest from masking the lme4 function for lmer
      lmer <- lme4::lmer
        
  ### 1.3 Get Version and Session Info
    R.Version()
    sessionInfo()
    
    # Developed in:   
    # R version 4.2.3 (2023-03-15)
    # Platform: x86_64-apple-darwin17.0 (64-bit)
    # Running under: macOS 14.2.1
    
  
  ### 1.4 Set working directory 
    setwd(here())
  

      
###############################################################################
##############                    2. Load RData                  ##############
###############################################################################  
  
  ### 2.1 Load RData
    ## a) Load RData (diagnostics and barn swallow data base)
      load(here('data/3_bs_phys_data.RData'))
      
      
      
###############################################################################
##############            3. Glucose response models             ##############
###############################################################################
    
  ### 3.1 Subset data for glucose response sensitivity analysis 
    ## a) Subset to include samples in which blood glucose is sample in under
      # 4 min, since Cort does not sig. rise in birds until after 3 mins; 
      # Cf. Wingfield JC, Smith JP, and Farner DS. Endocrine responses
      # of white-crowned sparrows to environmental stress.
      # Condor 84: 399–409, 1982.
      # but also note stress induced hyperglycemia is not evident until 15 mins
      # in birds as described in Remage-Healey & Romero 2000 and 2001
      nestling_parent_care_sens <- nestling_parent_care %>%
        filter(base.gluc.s < 240)
    
    ## b) Transform nestling_parent_care from wide to long format
      nestling_parent_care_sens_l_1 <- nestling_parent_care_sens %>%
        select(-c(base.gluc.time, stress.gluc.time, base.gluc.s, 
                  stress.gluc.s)) %>%
        pivot_longer(cols = c(base.gluc, stress.gluc),
                     names_to = 'glucose.sample',
                     values_to = 'glucose')
    
      nestling_parent_care_sens_l_2 <- nestling_parent_care_sens %>%
        select(nestling.band, base.gluc.time, stress.gluc.time) %>%
        pivot_longer(cols = c(base.gluc.time, stress.gluc.time),
                     names_to = 'sample.order',
                     values_to = 'sample.time')
      
      nestling_parent_care_sens_l_3 <- nestling_parent_care_sens %>%
        select(nestling.band, base.gluc.s, stress.gluc.s) %>%
        pivot_longer(cols = c(base.gluc.s, stress.gluc.s),
                     names_to = 'sample.order.s',
                     values_to = 'sample.time.s')
      
      ## c) Select only variables that are needed
      nestling_parent_care_sens_l_2 <-nestling_parent_care_sens_l_2 %>%
        select(nestling.band, sample.time)
      
      nestling_parent_care_sens_l_3 <- nestling_parent_care_sens_l_3 %>%
        select(nestling.band, sample.time.s)
      
      nestling_parent_care_sens_l <- cbind(nestling_parent_care_sens_l_1, 
                                           nestling_parent_care_sens_l_2,
                                           nestling_parent_care_sens_l_3)
      
    ## d) Remove duplicated nestling.band column after checking for consistency
      nestling_parent_care_sens_l <- nestling_parent_care_sens_l[!duplicated
                  (as.list(nestling_parent_care_sens_l))]
      
    ## e) Remove temporary tables
      rm(nestling_parent_care_sens_l_1, nestling_parent_care_sens_l_2,
           nestling_parent_care_sens_l_3)
      
      nestling_parent_care_l$glucose.sample <- as.factor(nestling_parent_care_l$glucose.sample)
      levels(nestling_parent_care_l$glucose.sample)    
      
      
  ### 3.2 Mid-development stress response models
    ## a) Mid-development unadjusted baseline to stress state glucose levels 
      mid.base.stress.gluc.lmm <- lmer(glucose ~ glucose.sample +
                                       # nestling ID nested in nest
                                  (1|nest.id) + (1|nestling.band:nest.id), 
                                  data = subset(nestling_parent_care_l,
                                        sample.state == 'mid' &
                                        !is.na(x = glucose) &
                                        !is.na(x = glucose.sample) &
                                        !is.na(x = nestling.band)))
      
    ## b) Parameter estimates
      summary(mid.base.stress.gluc.lmm)  # model parameter estimates
      confint(mid.base.stress.gluc.lmm)  # 95% CIs
      #plot(mid.base.stress.gluc.lmm) # view fitted vs residuals
      
    ## c) Mid-development adjusted baseline to stress state glucose levels 
      mid.base.stress.gluc.lmm.adj <- lmer(glucose ~ glucose.sample +
                                             scale(nestling.age) + 
                                             scale(sample.time.s) +
                                            # sensitivity 1 - nos nestlings 
                                            # scale(nestling.number) + 
                                      # nestling ID nested in nest
                                      (1|nest.id) + (1|nestling.band:nest.id), 
                                       data = subset(nestling_parent_care_l,
                                    # sensitivity analysis - 4min max baseline
                                    # data = subset(nestling_parent_care_sens_l,
                                            sample.state == 'mid' &
                                            !is.na(x = glucose) &
                                            !is.na(x = glucose.sample) &
                                            !is.na(x = nestling.band)))
      
    ## d) Parameter estimates
      summary(mid.base.stress.gluc.lmm.adj)  # model parameter estimates
      confint(mid.base.stress.gluc.lmm.adj)  # 95% CIs
      #plot(mid.base.stress.gluc.lmm.adj) # view fitted vs residuals
    
    ## e) Calculate marginal means
      mid.base.stress.gluc.mmean.adj <- emmeans(mid.base.stress.gluc.lmm.adj,
                                                'glucose.sample')
      summary(mid.base.stress.gluc.mmean.adj)
      

  ### 3.2 Late development stress response models
    ## a) Late development unadjusted baseline to stress state glucose levels 
      late.base.stress.gluc.lmm <- lmer(glucose ~ glucose.sample +
                                         # nestling ID nested in nest
                                    (1|nest.id) + (1|nestling.band:nest.id), 
                                    data = subset(nestling_parent_care_l,
                                          sample.state == 'late' &
                                          !is.na(x = glucose) &
                                          !is.na(x = glucose.sample) &
                                          !is.na(x = nestling.band)))
      
    ## b) Parameter estimates
      summary(late.base.stress.gluc.lmm)  # model parameter estimates
      confint(late.base.stress.gluc.lmm)  # 95% CIs
      #plot(late.base.stress.gluc.lmm) # view fitted vs residuals
      
    ## c) Late development adjusted baseline to stress state glucose levels 
      late.base.stress.gluc.lmm.adj <- lmer(glucose ~ glucose.sample + 
                                            scale(nestling.age) +
                                            scale(sample.time.s) +
                                          # sensitivity 1 - nos nestlings 
                                          # scale(nestling.number) + 
                                      # nestling ID nested in nest
                                      (1|nest.id) + (1|nestling.band:nest.id), 
                                       data = subset(nestling_parent_care_l,
                                    # sensitivity analysis - 4min max baseline
                                    # data = subset(nestling_parent_care_sens_l,
                                             sample.state == 'late' &
                                             !is.na(x = glucose) &
                                             !is.na(x = glucose.sample) &
                                             !is.na(x = nestling.band)))
      
      
    ## d) Parameter estimates
      summary(late.base.stress.gluc.lmm.adj)  # model parameter estimates
      confint(late.base.stress.gluc.lmm.adj)  # 95% CIs
      #plot(late.base.stress.gluc.lmm.adj) # view fitted vs residuals
      
    ## e) Calculate marginal means
      late.base.stress.gluc.mmean.adj <- emmeans(late.base.stress.gluc.lmm.adj,
                                                'glucose.sample')
      summary(late.base.stress.gluc.mmean.adj)
      
      
      
###############################################################################
##############              4. Repeatability models              ##############
###############################################################################
            
  ### 4.1 ICC for baseline glucose across develop. stages
    ## a) ICC for baseline blood glucose
      base.gluc.icc.lmm <- lmer(glucose ~ 1 +
                                       (1|nestling.band), 
                                     data = subset(nestling_parent_care_l,
                                          sample.state != 'early' &
                                          glucose.sample == 'base.gluc' &
                                          !is.na(x = nestling.band)))
      
    ## b) Baseline model summary
      summary(base.gluc.icc.lmm)  # model parameter estimates
      
    ## c) Calculate baseline ICC using variance from random effects
      # = between subject / (between + within subject)
      base.gluc.icc <- (199.9 / (199.9 + 627.2))
      
      sigma_id <- attr(VarCorr(base.gluc.icc.lmm)[[1]],"stddev") 
      sigma_error <- attr(VarCorr(base.gluc.icc.lmm),"sc")
      ICC1_base_lmm <- as.double(sigma_id^2/(sigma_error^2+sigma_id^2))
    
    ## d) Create a data frame for calculating baseline ICC using irrNA package
      icc.base.data <-nestling_parent_care %>%
        filter(sample.state != 'early') %>%
        filter(!is.na(nestling.band)) %>%
        select(c(nestling.band, sample.state, base.gluc)) %>%
        pivot_wider(id_cols = nestling.band, names_from = sample.state, 
                    values_from = base.gluc) 
      
    ## e) Calculate baseline ICC using irrICC package 
      icc1a.fn(icc.base.data)
      ci.ICC1a(icc.base.data,conflev=0.95)
      
    # ## f) Calculate baseline ICC using irrNA package to double check value
    #   base.gluc.icc.irr <- iccNA(icc.base.data[, c('mid', 'late')],
    #                              rho0 = 0.3, conf = 0.95, detail = F)
    #   # model = 'oneway',
    #   # type = 'consistency',
    #   # unit = c('single'),
    #   # r0 = 0,
    #   # conf.level = 0.95)
    #   print(base.gluc.icc.irr)
    
      
  ### 4.2 ICC for difference in blood glucose (stress - baseline) 
      # across develop. stages      
    ## a) ICC for difference in blood glucose (stress - baseline)
      diff.gluc.icc.lmm <- lmer(gluc.diff ~ 1 +
                           (1|nestling.band), 
                           data = subset(nestling_parent_care_l,
                                  sample.state != 'early' &
                                  glucose.sample == 'base.gluc' &
                                  !is.na(x = nestling.band)))
      
    ## b) Parameter estimates
      summary(diff.gluc.icc.lmm)  # model parameter estimates
      
    ## c) Calculate ICC using variance from random effects
      # = between subject / (between + within subject)
      diff.gluc.icc <- (0.0 / (0.0 + 967.1))
      
      sigma_id <- attr(VarCorr(diff.gluc.icc.lmm)[[1]],"stddev") 
      sigma_error <- attr(VarCorr(diff.gluc.icc.lmm),"sc")
      ICC1_diff_lmm <- as.double(sigma_id^2/(sigma_error^2+sigma_id^2))
      
    ## d) Create a data frame for calculating ICC using irrNA package
      icc.diff.data <-nestling_parent_care %>%
        filter(sample.state != 'early') %>%
        filter(!is.na(nestling.band)) %>%
        select(c(nestling.band, sample.state, gluc.diff)) %>%
        pivot_wider(id_cols = nestling.band, names_from = sample.state, 
                    values_from = gluc.diff) 
      
    ## e) Calculate baseline ICC using irrICC package 
      icc1a.fn(icc.diff.data)
      ci.ICC1a(icc.diff.data,conflev=0.95)

  ### 4.3 ICC sensitivity analysis
    ## a) Subset to include samples in which blood glucose is sample in under
      # 4 min, since Cort does not sig. rise in birds until after 3 mins; 
      # Cf. Wingfield JC, Smith JP, and Farner DS. Endocrine responses
      # of white-crowned sparrows to environmental stress.
      # Condor 84: 399–409, 1982.
      # but also note stress induced hyperglycemia is not evident until 15 mins
      # in birds as described in Remage-Healey & Romero 2000 and 2001
      nestling_parent_care_sens <- nestling_parent_care %>%
        filter(base.gluc.s < 240)
      
    ## b) Create a data frame for calculating baseline ICC using irrNA package
      icc.base.data.sens <-nestling_parent_care_sens %>%
        filter(sample.state != 'early') %>%
        filter(!is.na(nestling.band)) %>%
        filter(base.gluc.s < 240) %>%
        select(c(nestling.band, sample.state, base.gluc)) %>%
        pivot_wider(id_cols = nestling.band, names_from = sample.state, 
                    values_from = base.gluc) 
      
    ## c) Calculate baseline ICC using irrICC package 
      icc1a.fn(icc.base.data.sens)
      ci.ICC1a(icc.base.data.sens,conflev=0.95)

    ## d) Create a data frame for calculating diff ICC using irrNA package
      icc.diff.data.sens <-nestling_parent_care_sens %>%
        filter(sample.state != 'early') %>%
        filter(!is.na(nestling.band)) %>%
        filter(base.gluc.s < 240) %>%
        select(c(nestling.band, sample.state, gluc.diff)) %>%
        pivot_wider(id_cols = nestling.band, names_from = sample.state, 
                    values_from = gluc.diff) 
      
    ## e) Calculate baseline ICC using irrICC package 
      icc1a.fn(icc.diff.data.sens)
      ci.ICC1a(icc.diff.data.sens,conflev=0.95)
      
      
###############################################################################
##############              5. Graph model results               ##############
###############################################################################
      

  ### 5.1 Graph of regression estimates for glucose response at mid and late
      # development
      
    ## a) Use broom to extract estimates and tidy the data frame
      mid.dev.gluc.respon <- tidy(mid.base.stress.gluc.mmean.adj, 
                                  conf.int = T) 
      
    ## b) Label the estimates in data frame
      mid.dev.gluc.respon$model <- c("Mid-development")
      
    ## c) Use broom to extract estimates and tidy the data frame
      late.dev.gluc.respon <- tidy(late.base.stress.gluc.mmean.adj, 
                                  conf.int = T) 
      
    ## d) Label the estimates in data frame
      late.dev.gluc.respon$model <- c("Late development")
      
     
    ## e) Combine regression estimates into a tidy table
      gluc.respon.by.dev <- bind_rows(mid.dev.gluc.respon, 
                                    late.dev.gluc.respon)
      
    
    ## f) Rename variable in gluc.respon.by.dev 
      gluc.respon.by.dev <- gluc.respon.by.dev %>%
        mutate(glucose.sample =
                 case_when(glucose.sample == 'base.gluc'
                           ~ c('baseline'),
                           glucose.sample == 'stress.gluc'
                           ~ c('stressed')))
      
    ## g) Re-code *nominal* factor (with ordered levels)  
      # Set levels (ordering) of 'glucose.sample' variable 
        gluc.respon.by.dev <- 
        transform(gluc.respon.by.dev, 
                  glucose.sample = factor(glucose.sample,
                                 levels = c('baseline', 
                                            'stressed')))  
        
    ## h) Re-code *nominal* factor (with ordered levels)  
        # Set levels (ordering) of 'model' variable 
        gluc.respon.by.dev <- transform(gluc.respon.by.dev, 
                                             model = factor(model,
                                                levels = c("Mid-development", 
                                                "Late development")))

      
    ## h) Graph results of baseline vs stressed state glucose at mid and late
        # development
        gluc.respon.by.dev.plot <- 
        ggplot(gluc.respon.by.dev, aes(x = glucose.sample, y = estimate, 
                                       color = model, group = model)) +
        # geom_hline(yintercept = 1, color = 'red',
        #            linetype = 2) + # line at null behind coefs
        geom_point(size = 6, 
                   position=position_dodge(width = -0.5)) +
        geom_errorbar(aes(ymin=(estimate - std.error), 
                          ymax=(estimate + std.error)), width=.1,
                      position=position_dodge(-0.5)) +
        geom_line(position=position_dodge(-0.5)) +
        scale_color_manual(values=c('palegreen4', 'steelblue4')) +
        # scale_fill_manual(values=c('Mid-development' = 'navy', 
        #                             'Late Development' = 'red')) +
        #coord_flip() + # flip x and y axes
        labs(title = 'Nestling blood glucose at baseline and in response to 
        handling stress at mid- and late development') +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        theme(plot.subtitle = element_text(hjust = 0.5, size = 14)) + 
        # bold and size title and axes labels
        theme(text = element_text(size=20, face = 'bold')) +
        #theme(legend.position = 'none') +
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
        # change x-axis scale / tick interval
        scale_y_continuous(breaks = round(seq
                                      (min(gluc.respon.by.dev$estimate - 10), 
                                      max(gluc.respon.by.dev$estimate + 10), 
                                              by = 10),0)) +
        # add major axes
        #theme(axis.line = element_line(colour = 'lightgrey', 
        #                               size = 1, linetype = 'solid')) + 
        # change axes font style, color, size, angle, margin, and legend
        theme(axis.text.x = element_text(face='bold', color='black', 
                                         size=20, angle=0,
                                         margin = margin(t = 10, r = 0, 
                                                         b = 10, l = 0)),
              axis.text.y = element_text(face='bold', color='black', 
                                         size=20, angle=0, 
                                         margin = margin(t = 0, r = 0, 
                                                         b = 0, l = 10)),
              legend.title=element_blank(),
              legend.text=element_text(size=14),
              legend.position = c(0.4, 0.94),
              legend.key = element_blank()) +
        xlab(expression(italic("(Sample collection time)"))) +
        ylab(expression
             (atop(bold("Marginal mean and SE blood glucose (mg/dl)")))) 

      
      print(gluc.respon.by.dev.plot)
      
    ## f) Save Plot
      # use ggsave to save the linearization plot
      ggsave('gluc.respon.by.dev.plot.pdf', plot = gluc.respon.by.dev.plot, 
             device = NULL,
             path = paste0(here(),'/output'), 
             scale = 1, width = 12.5,
             height = 9,
             units = c('in'), dpi = 300, limitsize = TRUE)
      
      
      
      