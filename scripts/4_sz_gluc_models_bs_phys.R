###############################################################################
#############              Barn swallow parental care             #############
#############               and offspring physiology              #############
#############                                                     #############
#############               4. Glucose by size models             #############
#############                                                     #############
#############                  By: Zach Laubach                   #############
#############                created: 24 May 2022                 #############
#############             last updated: 13 Nov 2024               #############
###############################################################################


### PURPOSE: Models of early-life size and growth as determinants of blood
            # glucose levels.
  
  
  # Code Blocks
    # 1: Configure work space
    # 2: Load RData
    # 3: Mid-development glucose by size models
    # 4: Late dev. glucose by mid-dev. size models
    # 5: Late dev. glucose by late dev. size models
    


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
      # library ('ggplot2')
      # 
      # library('hrbrthemes')
      # 
      # library('viridis')
      # 
      # load gridExtra packages
      # library('gridExtra')
      
    ## c) Modeling Packages
      # load lme4 packages
      library ('lme4')
      
      # load performance
      library('performance')
      
      # load broom packages
      library('broom')
      library('broom.mixed')
      
      # load dharma
      library('DHARMa')

        
  ### 1.3 Get Version and Session Info
    R.Version()
    sessionInfo()
    
    # Developed in:   
      # R version 4.4.2 (2024-10-31)
      # Platform: x86_64-apple-darwin20
      # Running under: macOS Sequoia 15.1
    
  
  ### 1.4 Set working directory 
    setwd(here())
  

      
###############################################################################
##############                    2. Load RData                  ##############
###############################################################################  
  
  ### 2.1 Load RData
    ## a) Load RData (diognotistics and barn swallow data base)
      load(here('data/4_bs_phys_data.RData'))
      
      
      
###############################################################################
##############     3. Mid-development glucose by size models     ##############
###############################################################################
    
  ### 3.1 Mid-development baseline glucose by wing length models
    ## a) Unadjusted mid-development baseline glucose 
      # by mid-dev. right wing length
      mid.base.gluc.wing.lmm <- lmer(base.gluc ~ scale(rt.wing.length) +    
                                (1|nest.id), 
                                data = subset(nestling_parent_care,
                                       sample.state == 'mid' &
                                       !is.na(x = base.gluc) &
                                       !is.na(x = rt.wing.length) &
                                       !is.na(x = nestling.band)))

      summary(mid.base.gluc.wing.lmm)    # model summary 
      confint(mid.base.gluc.wing.lmm)    # 95% CIs
      #plot(mid.base.gluc.wing.lmm)       # check residuals
      
      # Marginal and conditional R-squared
      r2_nakagawa(mid.base.gluc.wing.lmm)
      #, by_group=T)
      
    ## b) Adjusted mid-development baseline glucose 
      # by mid-dev. right wing length
      mid.base.gluc.wing.lmm.adj <- lmer(base.gluc ~ scale(rt.wing.length) +
                                      scale(base.gluc.s) + 
                                      scale(nestling.age) +
                                    # sensitivity 1 - nos nestlings 
                                    # scale(nestling.number) + 
                                     (1|nest.id), 
                                     data = subset(nestling_parent_care,
                                        sample.state == 'mid' &
                                        !is.na(x = base.gluc) &
                                        !is.na(x = rt.wing.length) &
                                        !is.na(x = nestling.band)
                                    # sensitivity analysis - 4min max baseline
                                      # & base.gluc.s < 240 
                                        ))

      summary(mid.base.gluc.wing.lmm.adj)    # model summary 
      confint(mid.base.gluc.wing.lmm.adj)    # 95% CIs
      #plot(mid.base.gluc.wing.lmm.adj)       # check residuals
      
      # Marginal and conditional R-squared
      r2_nakagawa(mid.base.gluc.wing.lmm.adj)
      #, by_group=T)
      
      
  ### 3.2 Mid-development glucose difference (stressed - baseline) 
      # by right wing length models
    ## a) Unadjusted mid-development glucose difference  
      # by mid-dev. right wing length
      mid.diff.gluc.wing.lmm <- lmer(gluc.diff ~ scale(rt.wing.length) +
                                     (1|nest.id), 
                                     data = subset(nestling_parent_care,
                                            sample.state == 'mid' &
                                            !is.na(x = gluc.diff) &
                                            !is.na(x = rt.wing.length) &
                                            !is.na(x = nestling.band)))

      summary(mid.diff.gluc.wing.lmm)    # model summary 
      confint(mid.diff.gluc.wing.lmm)    # 95% CIs
      #plot(mid.diff.gluc.wing.lmm)       # check residuals
      
      # Marginal and conditional R-squared
      r2_nakagawa(mid.diff.gluc.wing.lmm)
      #, by_group=T)
      
    ## b) Adjusted mid-development glucose difference 
      # by mid-dev. right wing length
      mid.diff.gluc.wing.lmm.adj <- lmer(gluc.diff ~ scale(rt.wing.length) +
                                          scale(base.gluc.s) + 
                                          scale(nestling.age) +
                                        # sensitivity 1 - nos nestlings 
                                        # scale(nestling.number) +
                                         (1|nest.id), 
                                         data = subset(nestling_parent_care,
                                                sample.state == 'mid' &
                                                !is.na(x = gluc.diff) &
                                                !is.na(x = rt.wing.length) &
                                                !is.na(x = nestling.band)
                                    # sensitivity analysis - 4min max baseline
                                              # & base.gluc.s < 240 
                                         ))

      summary(mid.diff.gluc.wing.lmm.adj)    # model summary 
      confint(mid.diff.gluc.wing.lmm.adj)    # 95% CIs
      #plot(mid.diff.gluc.wing.lmm.adj)       # check residuals
      
      # Marginal and conditional R-squared
      r2_nakagawa(mid.diff.gluc.wing.lmm.adj)
      #, by_group=T)

      
  ### 3.5 Mid-development baseline glucose by relative nestling size 
      # NOTE: nestling size above and below average size by nest by
      # develop. state
    ## a) Unadjusted mid-development baseline glucose 
      # by mid-dev. relative nestling size
      mid.base.gluc.size.lmm <- lmer(base.gluc ~ size.by.avg +
                                    (1|nest.id), 
                                    data = subset(nestling_parent_care,
                                          sample.state == 'mid' &
                                          !is.na(x = base.gluc) &
                                          !is.na(x = size.by.avg) &
                                          !is.na(x = nestling.band)))

      summary(mid.base.gluc.size.lmm)    # model summary 
      confint(mid.base.gluc.size.lmm)    # 95% CIs
      #plot(mid.base.gluc.size.lmm)       # check residuals
      
      # Marginal and conditional R-squared
      r2_nakagawa(mid.base.gluc.size.lmm)
      #, by_group=T)
      
    ## b) Adjusted mid-development baseline glucose 
      # by mid-dev. relative nestling size
      mid.base.gluc.size.lmm.adj <- lmer(base.gluc ~ size.by.avg +
                                          scale(base.gluc.s) + 
                                          scale(nestling.age) +
                                        # sensitivity 1 - nos nestlings 
                                        # scale(nestling.number) +
                                        (1|nest.id), 
                                        data = subset(nestling_parent_care,
                                              sample.state == 'mid' &
                                              !is.na(x = base.gluc) &
                                              !is.na(x = size.by.avg) &
                                              !is.na(x = nestling.band)
                                    # sensitivity analysis - 4min max baseline
                                          #  & base.gluc.s < 240
                                        ))

      summary(mid.base.gluc.size.lmm.adj)    # model summary 
      confint(mid.base.gluc.size.lmm.adj)    # 95% CIs
      #plot(mid.base.gluc.size.lmm.adj)       # check residuals
      
      # Marginal and conditional R-squared
      r2_nakagawa(mid.base.gluc.size.lmm.adj)
      #, by_group=T)
      
      
  ### 3.6 Mid-development glucose difference (stressed - baseline) 
      # by relative nestling size
      # NOTE: nestling size above and below average size by nest by
      # develop. state
    ## a) Unadjusted mid-development glucose difference 
      # by mid-dev. relative nestling size
      mid.diff.gluc.size.lmm <- lmer(gluc.diff ~ size.by.avg +
                                  (1|nest.id), 
                                  data = subset(nestling_parent_care,
                                        sample.state == 'mid' &
                                        !is.na(x = gluc.diff) &
                                        !is.na(x = size.by.avg) &
                                        !is.na(x = nestling.band)))

      summary(mid.diff.gluc.size.lmm)    # model summary 
      confint(mid.diff.gluc.size.lmm)    # 95% CIs
      #plot(mid.diff.gluc.size.lmm)       # check residuals
      
      # Marginal and conditional R-squared
      r2_nakagawa(mid.diff.gluc.size.lmm)
      #, by_group=T)
      
    ## b) Adjusted mid-development glucose difference 
      # by mid-dev. relative nestling size
      mid.diff.gluc.size.lmm.adj <- lmer(gluc.diff ~ size.by.avg +
                                          scale(base.gluc.s) + 
                                          scale(nestling.age) +
                                        # sensitivity 1 - nos nestlings 
                                        # scale(nestling.number) +
                                      (1|nest.id), 
                                      data = subset(nestling_parent_care,
                                            sample.state == 'mid' &
                                            !is.na(x = gluc.diff) &
                                            !is.na(x = size.by.avg) &
                                            !is.na(x = nestling.band)
                                    # sensitivity analysis - 4min max baseline
                                           #& base.gluc.s < 240 
                                            ))

      summary(mid.diff.gluc.size.lmm.adj)    # model summary 
      confint(mid.diff.gluc.size.lmm.adj)    # 95% CIs
      #plot(mid.diff.gluc.size.lmm.adj)       # check residuals
      
      # Marginal and conditional R-squared
      r2_nakagawa(mid.diff.gluc.size.lmm.adj)
      #, by_group=T)
     
      
      
###############################################################################
##############   4. Late dev. glucose by mid-dev. size models    ##############
###############################################################################       
      
  ### 4.1 Late development baseline glucose by mid-dev. wing length models
    ## a) Unadjusted late development baseline glucose 
      # by mid-dev. right wing length
      late.base.gluc.mid.wing.lmm <- lmer(base.gluc ~ 
                                            scale(mid.rt.wing.length) +
                                    (1|nest.id), 
                                    data = subset(late_nestling_size,
                                          !is.na(x = base.gluc) &
                                          !is.na(x = mid.rt.wing.length) &
                                          !is.na(x = nestling.band)))

      summary(late.base.gluc.mid.wing.lmm)    # model summary 
      confint(late.base.gluc.mid.wing.lmm)    # 95% CIs
      #plot(late.base.gluc.mid.wing.lmm)       # check residuals
      
      # Marginal and conditional R-squared
      r2_nakagawa(late.base.gluc.mid.wing.lmm)
      #, by_group=T)
      
    ## b) Adjusted late development baseline glucose 
      # by mid-dev. right wing length
      late.base.gluc.mid.wing.lmm.adj <- lmer(base.gluc ~ 
                                              scale(mid.rt.wing.length) +
                                              scale(base.gluc.s) + 
                                              scale(nestling.age) +
                                            # sensitivity 1 - nos nestlings 
                                             #scale(nestling.number) +
                                        (1|nest.id), 
                                        data = subset(late_nestling_size,
                                              !is.na(x = base.gluc) &
                                              !is.na(x = mid.rt.wing.length) &
                                              !is.na(x = nestling.band)
                                    # sensitivity analysis - 4min max baseline
                                             #& base.gluc.s < 240 
                                              ))

      summary(late.base.gluc.mid.wing.lmm.adj)    # model summary 
      confint(late.base.gluc.mid.wing.lmm.adj)    # 95% CIs
      #plot(late.base.gluc.mid.wing.lmm.adj)       # check residuals
      
      # Marginal and conditional R-squared
      r2_nakagawa(late.base.gluc.mid.wing.lmm.adj)
      #, by_group=T)
      
      
  ### 4.2 Late development glucose difference (stressed - baseline) 
      # by mid-dev. right wing length models
    ## a) Unadjusted late development glucose difference  
      # by mid-dev. right wing length
      late.diff.gluc.mid.wing.lmm <- lmer(gluc.diff ~ 
                                            scale(mid.rt.wing.length) +
                                    (1|nest.id), 
                                    data = subset(late_nestling_size,
                                          !is.na(x = gluc.diff) &
                                          !is.na(x = mid.rt.wing.length) &
                                          !is.na(x = nestling.band)))

      summary(late.diff.gluc.mid.wing.lmm)    # model summary 
      confint(late.diff.gluc.mid.wing.lmm)    # 95% CIs
     # plot(late.diff.gluc.mid.wing.lmm)       # check residuals
      
      # Marginal and conditional R-squared
      r2_nakagawa(late.diff.gluc.mid.wing.lmm)
      #, by_group=T)
      
    ## b) Adjusted late development glucose difference 
      # by mid-dev. right wing length
      late.diff.gluc.mid.wing.lmm.adj <- lmer(gluc.diff ~
                                              scale(mid.rt.wing.length) +
                                              scale(base.gluc.s) + 
                                              scale(nestling.age) +
                                            # sensitivity 1 - nos nestlings 
                                            # scale(nestling.number) +
                                      (1|nest.id), 
                                      data = subset(late_nestling_size,
                                            !is.na(x = gluc.diff) &
                                            !is.na(x = mid.rt.wing.length) &
                                            !is.na(x = nestling.band)
                                    # sensitivity analysis - 4min max baseline
                                           #& base.gluc.s < 240 
                                            ))

      summary(late.diff.gluc.mid.wing.lmm.adj)    # model summary 
      confint(late.diff.gluc.mid.wing.lmm.adj)    # 95% CIs
      #plot(late.diff.gluc.mid.wing.lmm.adj)       # check residuals
      
      # Marginal and conditional R-squared
      r2_nakagawa(late.diff.gluc.mid.wing.lmm.adj)
      #, by_group=T)
      
      
  ### 4.3 Late development baseline glucose by mid-dev. relative nestling size
      # NOTE: nestling size above and below average size by nest by
      # develop. state
      
    ## a) Unadjusted late development baseline glucose 
      # by mid-dev. relative nestling size
      late.base.gluc.mid.size.lmm <- lmer(base.gluc ~ mid.size.by.avg +
                                  (1|nest.id), 
                                  data = subset(late_nestling_size,
                                        !is.na(x = base.gluc) &
                                        !is.na(x = mid.size.by.avg) &
                                        !is.na(x = nestling.band)))

      summary(late.base.gluc.mid.size.lmm)    # model summary 
      confint(late.base.gluc.mid.size.lmm)    # 95% CIs
      #plot(late.base.gluc.mid.size.lmm)       # check residuals
      
      # Marginal and conditional R-squared
      r2_nakagawa(late.base.gluc.mid.size.lmm)
      #, by_group=T)
      
    ## b) Adjusted late development baseline glucose 
      # by mid-dev. relative nestling size
      late.base.gluc.mid.size.lmm.adj <- lmer(base.gluc ~ mid.size.by.avg +
                                                scale(base.gluc.s) + 
                                                scale(nestling.age) +
                                              # sensitivity 1 - nos nestlings 
                                              # scale(nestling.number) +
                                      (1|nest.id), 
                                      data = subset(late_nestling_size,
                                            !is.na(x = base.gluc) &
                                            !is.na(x = mid.size.by.avg) &
                                            !is.na(x = nestling.band)
                                    # sensitivity analysis - 4min max baseline
                                         #& base.gluc.s < 240 
                                            ))

      summary(late.base.gluc.mid.size.lmm.adj)    # model summary 
      confint(late.base.gluc.mid.size.lmm.adj)    # 95% CIs
      #plot(late.base.gluc.mid.size.lmm.adj)       # check residuals
      
      # Marginal and conditional R-squared
      r2_nakagawa(late.base.gluc.mid.size.lmm.adj)
      #, by_group=T)
      
      
  ### 4.4 Late development glucose difference (stressed - baseline) 
      # by mid-dev. relative nestling size
      # NOTE: nestling size above and below average size by nest by
      # develop. state
      
    ## a) Unadjusted late development glucose difference 
      # by mid-dev. relative nestling size
      late.diff.gluc.mid.size.lmm <- lmer(gluc.diff ~ mid.size.by.avg +
                                  (1|nest.id), 
                                  data = subset(late_nestling_size,
                                        !is.na(x = gluc.diff) &
                                        !is.na(x = mid.size.by.avg) &
                                        !is.na(x = nestling.band)))

      summary(late.diff.gluc.mid.size.lmm)    # model summary 
      confint(late.diff.gluc.mid.size.lmm)    # 95% CIs
      #plot(late.diff.gluc.mid.size.lmm)       # check residuals
      
      # Marginal and conditional R-squared
      r2_nakagawa(late.diff.gluc.mid.size.lmm)
      #, by_group=T)
      
    ## b) Adjusted late development glucose difference 
      # by mid-dev. relative nestling size
      late.diff.gluc.mid.size.lmm.adj <- lmer(gluc.diff ~ mid.size.by.avg +
                                                scale(base.gluc.s) + 
                                                scale(nestling.age) +
                                              # sensitivity 1 - nos nestlings 
                                              # scale(nestling.number) +
                                    (1|nest.id), 
                                    data = subset(late_nestling_size,
                                          !is.na(x = gluc.diff) &
                                          !is.na(x = mid.size.by.avg) &
                                          !is.na(x = nestling.band)
                                    # sensitivity analysis - 4min max baseline
                                        # & base.gluc.s < 240 
                                          ))

      summary(late.diff.gluc.mid.size.lmm.adj)    # model summary 
      confint(late.diff.gluc.mid.size.lmm.adj)    # 95% CIs
     # plot(late.diff.gluc.mid.size.lmm.adj)       # check residuals
      
      # Marginal and conditional R-squared
      r2_nakagawa(late.diff.gluc.mid.size.lmm.adj)
      #, by_group=T)
      
      
      
###############################################################################
##############   5. Late dev. glucose by late dev. size models   ##############
###############################################################################
    
  ### 5.1 Late development baseline glucose by late dev. wing length models
    ## a) Unadjusted late development baseline glucose 
      # by late dev. right wing length
      late.base.gluc.late.wing.lmm <- lmer(base.gluc ~ 
                                             scale(late.rt.wing.length) +
                                      (1|nest.id), 
                                      data = subset(late_nestling_size,
                                             !is.na(x = base.gluc) &
                                             !is.na(x = late.rt.wing.length) &
                                             !is.na(x = nestling.band)))

      summary(late.base.gluc.late.wing.lmm)    # model summary 
      confint(late.base.gluc.late.wing.lmm)    # 95% CIs
      #plot(late.base.gluc.late.wing.lmm)       # check residuals
      
      # Marginal and conditional R-squared
      r2_nakagawa(late.base.gluc.late.wing.lmm)
      #, by_group=T)
      
      
    ## b) Adjusted late development baseline glucose 
      # by late dev. right wing length
      late.base.gluc.late.wing.lmm.adj <- lmer(base.gluc ~ 
                                                scale(late.rt.wing.length) +
                                                scale(base.gluc.s) + 
                                                scale(nestling.age) +
                                              # sensitivity 1 - nos nestlings 
                                              # scale(nestling.number) +
                                        (1|nest.id), 
                                        data = subset(late_nestling_size,
                                              !is.na(x = base.gluc) &
                                              !is.na(x = late.rt.wing.length) &
                                              !is.na(x = nestling.band)
                                    # sensitivity analysis - 4min max baseline
                                            # & base.gluc.s < 240 
                                              ))

      summary(late.base.gluc.late.wing.lmm.adj)    # model summary 
      confint(late.base.gluc.late.wing.lmm.adj)    # 95% CIs
      #plot(late.base.gluc.late.wing.lmm.adj)       # check residuals
      
      # Marginal and conditional R-squared
      r2_nakagawa(late.base.gluc.late.wing.lmm.adj)
      #, by_group=T)
    
        
  ### 5.2 Late development glucose difference (stressed - baseline) 
      # by late dev. right wing length models
    ## a) Unadjusted late development glucose difference  
      # by late dev. right wing length
      late.diff.gluc.late.wing.lmm <- lmer(gluc.diff ~ 
                                             scale(late.rt.wing.length) +
                                      (1|nest.id), 
                                      data = subset(late_nestling_size,
                                            !is.na(x = gluc.diff) &
                                            !is.na(x = late.rt.wing.length) &
                                            !is.na(x = nestling.band)))

      summary(late.diff.gluc.late.wing.lmm)    # model summary 
      confint(late.diff.gluc.late.wing.lmm)    # 95% CIs
      #plot(late.diff.gluc.late.wing.lmm)       # check residuals
      
      # Marginal and conditional R-squared
      r2_nakagawa(late.diff.gluc.late.wing.lmm)
      #, by_group=T)
      
    ## b) Adjusted late development glucose difference 
      # by late dev. right wing length
      late.diff.gluc.late.wing.lmm.adj <- lmer(gluc.diff ~ 
                                                scale(late.rt.wing.length) +
                                                scale(base.gluc.s) + 
                                                scale(nestling.age) +
                                              # sensitivity 1 - nos nestlings 
                                              # scale(nestling.number) +
                                        (1|nest.id), 
                                        data = subset(late_nestling_size,
                                              !is.na(x = gluc.diff) &
                                              !is.na(x = late.rt.wing.length) &
                                              !is.na(x = nestling.band)
                                    # sensitivity analysis - 4min max baseline
                                             #& base.gluc.s < 240
                                              ))

      summary(late.diff.gluc.late.wing.lmm.adj)    # model summary 
      confint(late.diff.gluc.late.wing.lmm.adj)    # 95% CIs
      #plot(late.diff.gluc.late.wing.lmm.adj)       # check residuals
      
      # Marginal and conditional R-squared
      r2_nakagawa(late.diff.gluc.late.wing.lmm.adj)
      #, by_group=T)
      
      
  ### 5.3 Late development baseline glucose by late dev. relative nestling size
      # NOTE: nestling size above and below average size by nest by
      # develop. state
      
    ## a) Unadjusted late development baseline glucose 
      # by late dev. relative nestling size
      late.base.gluc.late.size.lmm <- lmer(base.gluc ~ late.size.by.avg +
                                    (1|nest.id), 
                                    data = subset(late_nestling_size,
                                           !is.na(x = base.gluc) &
                                           !is.na(x = late.size.by.avg) &
                                           !is.na(x = nestling.band)))
      
      summary(late.base.gluc.late.size.lmm)    # model summary 
      confint(late.base.gluc.late.size.lmm)    # 95% CIs
      #plot(late.base.gluc.late.size.lmm)       # check residuals
      
      # Marginal and conditional R-squared
      r2_nakagawa(late.base.gluc.late.size.lmm)
      #, by_group=T)
      
    ## b) Adjusted late development baseline glucose 
      # by late dev. relative nestling size
      late.base.gluc.late.size.lmm.adj <- lmer(base.gluc ~ late.size.by.avg +
                                                scale(base.gluc.s) + 
                                                scale(nestling.age) +
                                              # sensitivity 1 - nos nestlings 
                                              # scale(nestling.number) +
                                        (1|nest.id), 
                                        data = subset(late_nestling_size,
                                              !is.na(x = base.gluc) &
                                              !is.na(x = late.size.by.avg) &
                                              !is.na(x = nestling.band)
                                    # sensitivity analysis - 4min max baseline
                                             #& base.gluc.s < 240
                                               ))
      
      summary(late.base.gluc.late.size.lmm.adj)    # model summary 
      confint(late.base.gluc.late.size.lmm.adj)    # 95% CIs
      #plot(late.base.gluc.late.size.lmm.adj)       # check residuals
      
      # Marginal and conditional R-squared
      r2_nakagawa(late.base.gluc.late.size.lmm.adj)
      #, by_group=T)
      
      
  ### 5.4 Late development glucose difference (stressed - baseline) 
      # by late dev. relative nestling size
      # NOTE: nestling size above and below average size by nest by
      # develop. state
      
    ## a) Unadjusted late development glucose difference 
      # by late dev. relative nestling size
      late.diff.gluc.late.size.lmm <- lmer(gluc.diff ~ late.size.by.avg +
                                      (1|nest.id), 
                                      data = subset(late_nestling_size,
                                             !is.na(x = gluc.diff) &
                                             !is.na(x = late.size.by.avg) &
                                             !is.na(x = nestling.band)))
      
      summary(late.diff.gluc.late.size.lmm)    # model summary 
      confint(late.diff.gluc.late.size.lmm)    # 95% CIs
      #plot(late.diff.gluc.late.size.lmm)       # check residuals
      
      # Marginal and conditional R-squared
      r2_nakagawa(late.diff.gluc.late.size.lmm)
      #, by_group=T)
      
    ## b) Adjusted late development glucose difference 
      # by late dev. relative nestling size
      late.diff.gluc.late.size.lmm.adj <- lmer(gluc.diff ~ late.size.by.avg +
                                                scale(base.gluc.s) + 
                                                scale(nestling.age) +
                                              # sensitivity 1 - nos nestlings 
                                              # scale(nestling.number) +
                                        (1|nest.id), 
                                        data = subset(late_nestling_size,
                                              !is.na(x = gluc.diff) &
                                              !is.na(x = late.size.by.avg) &
                                              !is.na(x = nestling.band)
                                    # sensitivity analysis - 4min max baseline
                                             #& base.gluc.s < 240
                                              ))
      
      summary(late.diff.gluc.late.size.lmm.adj)    # model summary 
      confint(late.diff.gluc.late.size.lmm.adj)    # 95% CIs
      #plot(late.diff.gluc.late.size.lmm.adj)       # check residuals
      
      # Marginal and conditional R-squared
      r2_nakagawa(late.diff.gluc.late.size.lmm.adj)
      #, by_group=T)
      

  ### 5.5 Late development baseline glucose by diff (late - mid-development) 
      # nestling size
    ## a) Unadjusted late development baseline glucose 
      # by diff (late - mid-development) nestling size
      late.base.gluc.diff.size.lmm <- lmer(base.gluc ~ 
                                             scale(rt.wing.diff) +
                                      (1|nest.id), 
                                      data = subset(late_nestling_size,
                                             !is.na(x = base.gluc) &
                                             !is.na(x = rt.wing.diff) &
                                             !is.na(x = nestling.band)))
    
      
      summary(late.base.gluc.diff.size.lmm)    # model summary 
      confint(late.base.gluc.diff.size.lmm)    # 95% CIs
      #plot(late.base.gluc.diff.size.lmm)       # check residuals
      
      # Marginal and conditional R-squared
      r2_nakagawa(late.base.gluc.diff.size.lmm)
      #, by_group=T)
      
    ## b) Adjusted late development baseline glucose 
      # by diff (late - mid-development) nestling size
      late.base.gluc.diff.size.lmm.adj <- lmer(base.gluc ~ 
                                                scale(rt.wing.diff) +
                                                scale(base.gluc.s) + 
                                                scale(nestling.age) +
                                              # sensitivity 1 - nos nestlings 
                                              # scale(nestling.number) +
                                        (1|nest.id), 
                                        data = subset(late_nestling_size,
                                              !is.na(x = base.gluc) &
                                              !is.na(x = rt.wing.diff) &
                                              !is.na(x = nestling.band)
                                    # sensitivity analysis - 4min max baseline
                                             #& base.gluc.s < 240
                                               ))
      
      summary(late.base.gluc.diff.size.lmm.adj)    # model summary 
      confint(late.base.gluc.diff.size.lmm.adj)    # 95% CIs
      #plot(late.base.gluc.diff.size.lmm.adj)       # check residuals
      
      # Marginal and conditional R-squared
      r2_nakagawa(late.base.gluc.diff.size.lmm.adj)
      #, by_group=T)
      
      
  ### 5.6 Late development glucose difference (stressed - baseline) 
      # by diff (late - mid-development) nestling size
    ## a) Unadjusted late development glucose difference 
      # by diff (late - mid-development) nestling size
      late.diff.gluc.diff.size.lmm <- lmer(gluc.diff ~ 
                                             scale(rt.wing.diff) +
                                      (1|nest.id), 
                                      data = subset(late_nestling_size,
                                            !is.na(x = gluc.diff) &
                                            !is.na(x = rt.wing.diff) &
                                            !is.na(x = nestling.band)))
      
      
      summary(late.diff.gluc.diff.size.lmm)    # model summary 
      confint(late.diff.gluc.diff.size.lmm)    # 95% CIs
      #plot(late.diff.gluc.diff.size.lmm)       # check residuals
      
      # Marginal and conditional R-squared
      r2_nakagawa(late.diff.gluc.diff.size.lmm)
      #, by_group=T)
      
    ## b) Adjusted late development glucose difference 
      # by diff (late - mid-development) nestling size
      late.diff.gluc.diff.size.lmm.adj <- lmer(gluc.diff ~ 
                                                scale(rt.wing.diff) +
                                                scale(base.gluc.s) + 
                                                scale(nestling.age) +
                                              # sensitivity 1 - nos nestlings 
                                              # scale(nestling.number) +
                                        (1|nest.id), 
                                        data = subset(late_nestling_size,
                                              !is.na(x = gluc.diff) &
                                              !is.na(x = rt.wing.diff) &
                                              !is.na(x = nestling.band)
                                    # sensitivity analysis - 4min max baseline
                                            # & base.gluc.s < 240
                                               ))
      
      summary(late.diff.gluc.diff.size.lmm.adj)    # model summary 
      confint(late.diff.gluc.diff.size.lmm.adj)    # 95% CIs
      #plot(late.diff.gluc.diff.size.lmm.adj)       # check residuals
      
      # Marginal and conditional R-squared
      r2_nakagawa(late.diff.gluc.diff.size.lmm.adj)
      #, by_group=T)
      
      
  

      

      
    
               