###############################################################################
#############              Barn swallow parental care             #############
#############               and offspring physiology              #############
#############                                                     #############
#############              6. Effect modification by              #############
#############               parental care behaviors               #############
#############                  By: Zach Laubach                   #############
#############                created: 10 June 2022                #############
#############              last updated: 06 Feb 2024              #############
###############################################################################


### PURPOSE: Check for and model effect modification on the relationship 
           # between size/growth and glucose by parental care 
  
  
  # Code Blocks
    # 1: Configure work space
    # 2: Load RData
    # 3: Effect modification models
    # 4: Stratified models
    # 5: Graph results

    

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
      # detach('package:lmerTest',
      #        unload = TRUE) # make sure lmerTest is not attached - influences CIs
      
      # load nlme packages
      #library ('nlme')
      
      # load broom packages
      #library('broom')
      library('broom.mixed')
      
      # load dharma
      library('DHARMa')
      
        
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
    load(here('data/6_bs_phys_sz_care_data.RData'))
    
      
      
###############################################################################
##############           3. Effect modification models           ##############
###############################################################################

  # ### 3.1 Late dev. baseline glucose by wing length by parental care (feed)
  #     # interaction models
  #     # NOTE: ⍺ for intx. term  = 0.20
  #   
  #   ## a) Late dev. baseline glucose by late dev. 
  #     # right wing length x feed.indx
  #     base.gluc.wing.x.care.lmm <- lme(base.gluc ~ scale(rt.wing.length) * 
  #                                        feed.indx,
  #                                        random = ~1|nest.id, 
  #                                  data = subset(late_nestling_parent_care,
  #                                                 !is.na(x = base.gluc) &
  #                                                 !is.na(x = feed.indx) &
  #                                                 !is.na(x = rt.wing.length) &
  #                                                 !is.na(x = nestling.band)))
  # 
  #     summary(base.gluc.wing.x.care.lmm)    # model summary 
  #     intervals(base.gluc.wing.x.care.lmm)    # 95% CIs
  #     plot(base.gluc.wing.x.care.lmm)       # check residuals
  #     
  #   ## b) Late dev. difference in glucose by late dev. 
  #     # right wing length x feed.indx
  #     gluc.diff.wing.x.care.lmm <- lme(gluc.diff ~ scale(rt.wing.length) * 
  #                                        feed.indx,
  #                                      random = ~1|nest.id, 
  #                                      data = subset(late_nestling_parent_care,
  #                                                 !is.na(x = gluc.diff) &
  #                                                 !is.na(x = feed.indx) &
  #                                                 !is.na(x = rt.wing.length) &
  #                                                 !is.na(x = nestling.band)))
  #     
  #     summary(gluc.diff.wing.x.care.lmm)    # model summary 
  #     intervals(gluc.diff.wing.x.care.lmm)    # 95% CIs
  #     plot(gluc.diff.wing.x.care.lmm)       # check residuals
  #     
  #     
  # ### 3.2 Late dev. baseline glucose by growth (late - mid wing length)  
  #     # by parental care index interaction models
  #     # NOTE: ⍺ for intx. term  = 0.20
  #   
  #   ## a) Late dev. baseline glucose by by growth (late - mid wing length) 
  #     #  x feed.indx
  #     base.gluc.wing.diff.x.care.lmm <- lme(base.gluc ~ scale(rt.wing.diff) 
  #                                           * feed.indx,
  #                                      random = ~1|nest.id, 
  #                                      data = subset(late_nestling_parent_care,
  #                                                 !is.na(x = base.gluc) &
  #                                                 !is.na(x = feed.indx) &
  #                                                 !is.na(x = rt.wing.diff) &
  #                                                 !is.na(x = nestling.band)))
  #   
  #     summary(base.gluc.wing.diff.x.care.lmm)    # model summary 
  #     intervals(base.gluc.wing.diff.x.care.lmm)    # 95% CIs
  #     plot(base.gluc.wing.diff.x.care.lmm)       # check residuals
  #     
  #   ## b) Late dev. difference in glucose by late dev. 
  #     # right wing length x feed.indx
  #     gluc.diff.wing.diff.x.care.lmm <- lme(gluc.diff ~ scale(rt.wing.diff) 
  #                                           * feed.indx,
  #                                      random = ~1|nest.id, 
  #                                      data = subset(late_nestling_parent_care,
  #                                                 !is.na(x = gluc.diff) &
  #                                                 !is.na(x = feed.indx) &
  #                                                 !is.na(x = rt.wing.diff) &
  #                                                 !is.na(x = nestling.band)))
  #   
  #     summary(gluc.diff.wing.diff.x.care.lmm)    # model summary 
  #     intervals(gluc.diff.wing.diff.x.care.lmm)    # 95% CIs
  #     plot(gluc.diff.wing.diff.x.care.lmm)       # check residuals
      

      
###############################################################################
##############                4. Stratified models               ##############
###############################################################################
      
  ### 4.1 Late dev. baseline glucose by wing length, stratified by parental 
      # care models
      
    ## a) Low parental care: late dev. baseline glucose by late dev. 
      # right wing length 
      base.gluc.wing.low.care.lmm <- lmer(base.gluc ~ 
                                        scale(rt.wing.length) +
                                        scale(base.gluc.s) + 
                                        scale(nestling.age) +
                                      (1|nest.id), 
                                      data = subset(late_nestling_parent_care,
                                            feed.indx == 'low' &
                                          # sensitivity 3 - index 4 
                                          # care.indx.4 == 'low' &
                                            !is.na(x = base.gluc) &
                                      # indx.3 and indx.4 have equal samp size
                                            !is.na(x = rt.wing.length) &
                                            !is.na(x = nestling.band)))
      
      summary(base.gluc.wing.low.care.lmm)    # model summary 
      confint(base.gluc.wing.low.care.lmm, level = 0.95, method = 'profile')
      plot(base.gluc.wing.low.care.lmm)       # check residuals
      # check boot strap CI
      set.seed(2019) 
      confint(base.gluc.wing.low.care.lmm, level = 0.95 , method = 'boot', 
              boot.type = 'perc')
      
      
      
    ## b) Avg. parental care: late dev. baseline glucose by late dev. 
      # right wing length 
      base.gluc.wing.avg.care.lmm <- lmer(base.gluc ~ 
                                        scale(rt.wing.length) +
                                        scale(base.gluc.s) + 
                                        scale(nestling.age) +
                                      (1|nest.id), 
                                      data = subset(late_nestling_parent_care,
                                                    feed.indx == 'avg' &
                                          # sensitivity 3 - index 4 
                                          # care.indx.4 == 'avg' &
                                            !is.na(x = base.gluc) &
                                       # indx.3 and indx.4 have equal samp size
                                            !is.na(x = rt.wing.length) &
                                            !is.na(x = nestling.band)))
      
      summary(base.gluc.wing.avg.care.lmm)    # model summary 
      confint(base.gluc.wing.avg.care.lmm, level = 0.95, method = 'profile')
      #plot(base.gluc.wing.avg.care.lmm)       # check residuals
      
    # ## c) High parental care: late dev. baseline glucose by late dev. 
    #   # right wing length 
    #   base.gluc.wing.hi.care.lmm <- lmer(base.gluc ~ 
    #                                     scale(rt.wing.length) +
    #                                     scale(base.gluc.s) + 
    #                                     scale(nestling.age) +
    #                                   (1|nest.id), 
    #                                   data = subset(late_nestling_parent_care,
    #                                                 feed.indx == 'hi' &
    #                                       # sensitivity 3 - index 4 
    #                                       # care.indx.4 == 'hi' &
    #                                         !is.na(x = base.gluc) &
    #                                   # indx.3 and indx.4 have equal samp size
    #                                         !is.na(x = rt.wing.length) &
    #                                         !is.na(x = nestling.band)))
    #   
    #   summary(base.gluc.wing.hi.care.lmm)    # model summary 
    #   confint(base.gluc.wing.hi.care.lmm)
    #   plot(base.gluc.wing.hi.care.lmm)       # check residuals
      
  
      
  ### 4.2 Late dev. baseline glucose by growth, stratified by parental 
      # care models
    ## a) Low parental care: late dev. baseline glucose by growth
      base.gluc.grow.low.care.lmm <- lmer(base.gluc ~ 
                                        scale(rt.wing.diff) +
                                        scale(base.gluc.s) + 
                                        scale(nestling.age) +
                                      (1|nest.id), 
                                      data = subset(late_nestling_parent_care,
                                                    feed.indx == 'low' &
                                          # sensitivity 3 - index 4 
                                          # care.indx.4 == 'low' &
                                            !is.na(x = base.gluc) &
                                      # indx.3 and indx.4 have equal samp size
                                            !is.na(x = rt.wing.diff) &
                                            !is.na(x = nestling.band)))
      
      summary(base.gluc.grow.low.care.lmm)    # model summary 
      confint(base.gluc.grow.low.care.lmm, level = 0.95, method = 'profile')
      #plot(base.gluc.grow.low.care.lmm)       # check residuals
      
    ## b) Avg. parental care: late dev. baseline glucose by growth
      base.gluc.grow.avg.care.lmm <- lmer(base.gluc ~ 
                                        scale(rt.wing.diff) +
                                        scale(base.gluc.s) + 
                                        scale(nestling.age) +
                                      (1|nest.id), 
                                      data = subset(late_nestling_parent_care,
                                                    feed.indx == 'avg' &
                                          # sensitivity 3 - index 4 
                                          # care.indx.4 == 'avg' &
                                            !is.na(x = base.gluc) &
                                       # indx.3 and indx.4 have equal samp size
                                            !is.na(x = rt.wing.diff) &
                                            !is.na(x = nestling.band)))
      
      summary(base.gluc.grow.avg.care.lmm)    # model summary 
      confint(base.gluc.grow.avg.care.lmm, level = 0.95, method = 'profile')
      #plot(base.gluc.grow.avg.care.lmm)       # check residuals
      
    # ## c) High parental care: late dev. baseline glucose by growth
    #   base.gluc.grow.hi.care.lmm <- lmer(base.gluc ~ 
    #                                     scale(rt.wing.diff) +
    #                                     scale(base.gluc.s) + 
    #                                     scale(nestling.age) +
    #                                   (1|nest.id), 
    #                                   data = subset(late_nestling_parent_care,
    #                                                 feed.indx == 'hi' &
    #                                       # sensitivity 3 - index 4 
    #                                       # care.indx.4 == 'hi' &
    #                                         !is.na(x = base.gluc) &
    #                                    # indx.3 and indx.4 have equal samp size
    #                                         !is.na(x = rt.wing.diff) &
    #                                         !is.na(x = nestling.band)))
    #   
    #   summary(base.gluc.grow.hi.care.lmm)    # model summary 
    #   confint(base.gluc.grow.hi.care.lmm)
    #   plot(base.gluc.grow.hi.care.lmm)       # check residuals
      
      
  ### 4.3 Late dev. baseline glucose (stress - baseline) by rt.wing.length, 
      # stratified by parental care models
      
    ## a) Low parental care: late dev. baseline glucose by rt.wing.length
      gluc.diff.wing.low.care.lmm <- lmer(gluc.diff ~ 
                                        scale(rt.wing.length) +
                                        scale(base.gluc.s) + 
                                        scale(nestling.age) +
                                      (1|nest.id), 
                                      data = subset(late_nestling_parent_care,
                                                    feed.indx == 'low' &
                                          # sensitivity 3 - index 4 
                                          # care.indx.4 == 'low' &
                                            !is.na(x = gluc.diff) &
                                          # indx.3 and indx.4 have equal samp size
                                            !is.na(x = rt.wing.length) &
                                            !is.na(x = nestling.band)))
      
      summary(gluc.diff.wing.low.care.lmm)    # model summary 
      confint(gluc.diff.wing.low.care.lmm, level = 0.95, method = 'profile')
      #plot(gluc.diff.wing.low.care.lmm)       # check residuals
      
    ## b) Avg. parental care: late dev. baseline glucose by rt.wing.length
      gluc.diff.wing.avg.care.lmm <- lmer(gluc.diff ~ 
                                        scale(rt.wing.length) +
                                        scale(base.gluc.s) + 
                                        scale(nestling.age) +
                                      (1|nest.id), 
                                      data = subset(late_nestling_parent_care,
                                                    feed.indx == 'avg' &
                                          # sensitivity 3 - index 4 
                                          # care.indx.4 == 'avg' &
                                            !is.na(x = gluc.diff) &
                                      # indx.3 and indx.4 have equal samp size
                                            !is.na(x = rt.wing.length) &
                                            !is.na(x = nestling.band)))
      
      summary(gluc.diff.wing.avg.care.lmm)    # model summary 
      confint(gluc.diff.wing.avg.care.lmm, level = 0.95, method = 'profile')
      #plot(gluc.diff.wing.avg.care.lmm)       # check residuals
      
    # ## c) High parental care: late dev. baseline glucose by growth
    #   gluc.diff.wing.hi.care.lmm <- lmer(gluc.diff ~ 
    #                                     scale(rt.wing.length) +
    #                                     scale(base.gluc.s) + 
    #                                     scale(nestling.age) +
    #                                   (1|nest.id), 
    #                                   data = subset(late_nestling_parent_care,
    #                                                 feed.indx == 'hi' &
    #                                       # sensitivity 3 - index 4 
    #                                       # care.indx.4 == 'hi' &
    #                                         !is.na(x = gluc.diff) &
    #                                   # indx.3 and indx.4 have equal samp size
    #                                          !is.na(x = rt.wing.length) &
    #                                          !is.na(x = nestling.band)))
    #   
    #   summary(gluc.diff.wing.hi.care.lmm)    # model summary 
    #   confint(gluc.diff.wing.hi.care.lmm)
    #   plot(gluc.diff.wing.hi.care.lmm)       # check residuals
      
      
      
  ### 4.4 Late dev. glucose response (stress - baseline) by growth, 
      # stratified by parental care models
      
    ## a) Low parental care: late dev. glucose response by growth
      gluc.diff.grow.low.care.lmm <- lmer(gluc.diff ~ 
                                            scale(rt.wing.diff) +
                                            scale(base.gluc.s) + 
                                            scale(nestling.age) +
                                            (1|nest.id), 
                                      data = subset(late_nestling_parent_care,
                                              feed.indx == 'low' &
                                              # sensitivity 3 - index 4 
                                              # care.indx.4 == 'low' &
                                              !is.na(x = gluc.diff) &
                                    # indx.3 and indx.4 have equal samp size
                                              !is.na(x = rt.wing.diff) &
                                              !is.na(x = nestling.band)))
      
      summary(gluc.diff.grow.low.care.lmm)    # model summary 
      confint(gluc.diff.grow.low.care.lmm, level = 0.95, method = 'profile')
      #plot(gluc.diff.grow.low.care.lmm)       # check residuals
      
    ## b) Avg. parental care: late dev. glucose response by growth
      gluc.diff.grow.avg.care.lmm <- lmer(gluc.diff ~ 
                                            scale(rt.wing.diff) +
                                            scale(base.gluc.s) + 
                                            scale(nestling.age) +
                                            (1|nest.id), 
                                      data = subset(late_nestling_parent_care,
                                                      feed.indx == 'avg' &
                                              # sensitivity 3 - index 4 
                                              # care.indx.4 == 'avg' &
                                              !is.na(x = gluc.diff) &
                                      # indx.3 and indx.4 have equal samp size
                                               !is.na(x = rt.wing.diff) &
                                               !is.na(x = nestling.band)))
      
      summary(gluc.diff.grow.avg.care.lmm)    # model summary 
      confint(gluc.diff.grow.avg.care.lmm, level = 0.95, method = 'profile')
      #plot(gluc.diff.grow.avg.care.lmm)       # check residuals
      
    # ## c) High parental care: late dev. glucose response by growth
    #   gluc.diff.grow.hi.care.lmm <- lmer(gluc.diff ~ 
    #                                        scale(rt.wing.diff) +
    #                                        scale(base.gluc.s) + 
    #                                        scale(nestling.age) +
    #                                        (1|nest.id), 
    #                                   data = subset(late_nestling_parent_care,
    #                                         feed.indx == 'hi' &
    #                                         # sensitivity 3 - index 4 
    #                                         # care.indx.4 == 'hi' &
    #                                         !is.na(x = gluc.diff) &
    #                                   # indx.3 and indx.4 have equal samp size
    #                                         !is.na(x = rt.wing.diff) &
    #                                         !is.na(x = nestling.band)))
    #   
    #   summary(gluc.diff.grow.hi.care.lmm)    # model summary 
    #   confint(gluc.diff.grow.hi.care.lmm)
    #   plot(gluc.diff.grow.hi.care.lmm)       # check residuals
      
      
      
###############################################################################
##############                 5. Graph results                  ##############
###############################################################################      
    
  ### 5.1 Parental care stratified graph of regression estimates for late 
      # development baseline glucose  by late development wing length
    ## a) Low parental care model: extract estimates and tidy the data frame 
  
      base.gluc.wing.low.care.est <- tidy(base.gluc.wing.low.care.lmm, 
                                  conf.int = T, 
                                  conf.method = 'profile', 
                                  conf.level = 0.95) %>%
        filter(term == 'scale(rt.wing.length)')
      
    ## b) Low parental care model: Label the estimates in data frame
      base.gluc.wing.low.care.est$model <- c("Low parental care")
      
    ## c) Avg parental care model: extract estimates and tidy the data frame 
      base.gluc.wing.avg.care.est <- tidy(base.gluc.wing.avg.care.lmm, 
                                          conf.int = T, 
                                          conf.method = 'profile', 
                                          conf.level = 0.95) %>%
        filter(term == 'scale(rt.wing.length)')
      
    ## d) Avg parental care model: Label the estimates in data frame
      base.gluc.wing.avg.care.est$model <- c("Avg. parental care")
      
    # ## e) High parental care model: extract estimates and tidy the data frame 
    #   base.gluc.wing.hi.care.est <- tidy(base.gluc.wing.hi.care.lmm, 
    #                                       conf.int = T, 
    #                                       conf.method = 'profile', 
    #                                       conf.level = 0.95) %>%
    #     filter(term == 'scale(rt.wing.length)')
    #   
    # ## f) High parental care model: Label the estimates in data frame
    #   base.gluc.wing.hi.care.est$model <- c("High parental care")  
    #   
    ## g) Combine regression estimates into a tidy table
      base.gluc.by.wing.by.care <- bind_rows(base.gluc.wing.low.care.est,
                                      base.gluc.wing.avg.care.est
                                      #, base.gluc.wing.hi.care.est
                                      )
      
    ## h) Re-code *nominal* factor (with ordered levels)  
      # Set levels (odering) of 'model' variable 
      base.gluc.by.wing.by.care <- 
        transform(base.gluc.by.wing.by.care, 
                  model = factor(model,
                                          levels = c('Low parental care',
                                                     'Avg. parental care'
                                                     #,'High parental care'
                                                     )))  
      
    ## i) Graph results of baseline vs stressed state glucose at mid and late
      # development
      base.gluc.by.wing.by.care.plot <- 
        ggplot(base.gluc.by.wing.by.care, aes(x = model, y = estimate, 
                                       color = model)) +
        geom_hline(yintercept = 0, color = 'red',
                   linetype = 2) + # line at null behind coefs
        geom_point(size = 6) +
        geom_errorbar(aes(ymin=(conf.low), 
                          ymax=(conf.high)), width=.1) +
        scale_color_manual(values=c('steelblue4', 'steelblue1'
                                   # , 'firebrick4'
                                    )) +
        #coord_flip() + # flip x and y axes
        labs(title = 'The association beween late development size and late development baseline blood 
glucose (mg/dl) from models stratified by amount of parental care') +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        theme(plot.subtitle = element_text(hjust = 0.5, size = 14)) + 
        # bold and size title and axes labels
        theme(text = element_text(size=20, face = 'bold')) +
        theme(legend.position = 'none') +
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
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
              legend.position = 'none', #c(0.91, 0.94),
              legend.key = element_blank()) +
        xlab(expression(italic("Models stratified by parental care"))) +
        ylab(expression
             (atop(bold("Beta estimate and 95% CI"),
    paste(italic("Baseline glucose (mg/dl) per 1 SD wing length"))))) 
      
      
    print(base.gluc.by.wing.by.care.plot)
      
    ## j) Save Plot
      # use ggsave to save the plot
      ggsave('base.gluc.by.wing.by.care.plot.pdf', 
             plot = base.gluc.by.wing.by.care.plot, 
             device = NULL,
             path = paste0(here(),'/output'), 
             scale = 1, width = 12.5,
             height = 9,
             units = c('in'), dpi = 300, limitsize = TRUE)
      
      
  ### 5.2 Parental care stratified graph of regression estimates for late 
      # development baseline glucose by growth (late - mid wing length)
    ## a) Low parental care model: extract estimates and tidy the data frame 
      base.gluc.grow.low.care.est <- tidy(base.gluc.grow.low.care.lmm, 
                                          conf.int = T, 
                                          conf.method = 'profile', 
                                          conf.level = 0.95) %>%
        filter(term == 'scale(rt.wing.diff)')
      
    ## b) Low parental care model: Label the estimates in data frame
      base.gluc.grow.low.care.est$model <- c("Low parental care")
      
      
    ## c) Avg parental care model: extract estimates and tidy the data frame 
      base.gluc.grow.avg.care.est <- tidy(base.gluc.grow.avg.care.lmm, 
                                          conf.int = T, 
                                          conf.method = 'profile', 
                                          conf.level = 0.95) %>%
        filter(term == 'scale(rt.wing.diff)')
      
    ## d) Avg parental care model: Label the estimates in data frame
      base.gluc.grow.avg.care.est$model <- c("Avg. parental care")
      
    # ## e) High parental care model: extract estimates and tidy the data frame 
    #   base.gluc.grow.hi.care.est <- tidy(base.gluc.grow.hi.care.lmm, 
    #                                      conf.int = T, 
    #                                      conf.method = 'profile', 
    #                                      conf.level = 0.95) %>%
    #     filter(term == 'scale(rt.wing.diff)')
    #   
    # ## f) High parental care model: Label the estimates in data frame
    #   base.gluc.grow.hi.care.est$model <- c("High parental care")  
      
      
    ## g) Combine regression estimates into a tidy table
      base.gluc.by.grow.by.care <- bind_rows(base.gluc.grow.low.care.est,
                                             base.gluc.grow.avg.care.est
                                            # , base.gluc.grow.hi.care.est
                                             )
      
    ## h) Re-code *nominal* factor (with ordered levels)  
      # Set levels (odering) of 'model' variable 
      base.gluc.by.grow.by.care <- 
        transform(base.gluc.by.grow.by.care, 
                  model = factor(model,
                                 levels = c('Low parental care',
                                            'Avg. parental care'
                                            #, 'High parental care'
                                            )))  
      
    ## i) Graph results of baseline vs stressed state glucose at mid and late
      # development
      base.gluc.by.grow.by.care.plot <- 
        ggplot(base.gluc.by.grow.by.care, aes(x = model, y = estimate, 
                                              color = model)) +
        geom_hline(yintercept = 0, color = 'red',
                   linetype = 2) + # line at null behind coefs
        geom_point(size = 6) +
        geom_errorbar(aes(ymin=(conf.low), 
                          ymax=(conf.high)), width=.1) +
        scale_color_manual(values=c('steelblue4', 'steelblue1'
                                    #, 'firebrick4'
                                    )) +
        #coord_flip() + # flip x and y axes
        labs(title = 'The association between growth and late development baseline blood 
glucose (mg/dl) from models stratified by amount of parental care') +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        theme(plot.subtitle = element_text(hjust = 0.5, size = 14)) + 
        # bold and size title and axes labels
        theme(text = element_text(size=20, face = 'bold')) +
        theme(legend.position = 'none') +
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
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
              legend.position = 'none', #c(0.91, 0.94),
              legend.key = element_blank()) +
        xlab(expression(italic("Models stratified by parental care"))) +
        ylab(expression
             (atop(bold("Beta estimate and 95% CI"),
                   paste(italic("Baseline glucose (mg/dl) per 1 SD growth"))))) 
      
      
      print(base.gluc.by.grow.by.care.plot)
      
    ## j) Save Plot
      # use ggsave to save the plot
      ggsave('base.gluc.by.grow.by.care.plot.pdf', 
             plot = base.gluc.by.grow.by.care.plot, 
             device = NULL,
             path = paste0(here(),'/output'), 
             scale = 1, width = 12.5,
             height = 9,
             units = c('in'), dpi = 300, limitsize = TRUE)
      
      
  ### 5.3 Parental care stratified graph of regression estimates for late 
      # development glucose response by late development wing length
    ## a) Low parental care model: extract estimates and tidy the data frame 
      gluc.diff.wing.low.care.est <- tidy(gluc.diff.wing.low.care.lmm, 
                                          conf.int = T, 
                                          conf.method = 'profile', 
                                          conf.level = 0.95) %>%
        filter(term == 'scale(rt.wing.length)')
      
    ## b) Low parental care model: Label the estimates in data frame
      gluc.diff.wing.low.care.est$model <- c("Low parental care")
      
      
    ## c) Avg parental care model: extract estimates and tidy the data frame 
      gluc.diff.wing.avg.care.est <- tidy(gluc.diff.wing.avg.care.lmm, 
                                          conf.int = T, 
                                          conf.method = 'profile', 
                                          conf.level = 0.95) %>%
        filter(term == 'scale(rt.wing.length)')
      
    ## d) Avg parental care model: Label the estimates in data frame
      gluc.diff.wing.avg.care.est$model <- c("Avg. parental care")
      
      # ## e) High parental care model: extract estimates and tidy the data frame 
      #   gluc.diff.wing.hi.care.est <- tidy(gluc.diff.wing.hi.care.lmm, 
      #                                       conf.int = T, 
      #                                       conf.method = 'profile', 
      #                                       conf.level = 0.95) %>%
      #     filter(term == 'scale(rt.wing.length)')
      #   
      # ## f) High parental care model: Label the estimates in data frame
      #   gluc.diff.wing.hi.care.est$model <- c("High parental care")  
      #   
    ## g) Combine regression estimates into a tidy table
      gluc.diff.by.wing.by.care <- bind_rows(gluc.diff.wing.low.care.est,
                                             gluc.diff.wing.avg.care.est
                                             #, gluc.diff.wing.hi.care.est
      )
      
    ## h) Re-code *nominal* factor (with ordered levels)  
      # Set levels (odering) of 'model' variable 
      gluc.diff.by.wing.by.care <- 
        transform(gluc.diff.by.wing.by.care, 
                  model = factor(model,
                                 levels = c('Low parental care',
                                            'Avg. parental care'
                                            #,'High parental care'
                                 )))  
      
    ## i) Graph results of baseline vs stressed state glucose at mid and late
      # development
      gluc.diff.by.wing.by.care.plot <- 
        ggplot(gluc.diff.by.wing.by.care, aes(x = model, y = estimate, 
                                              color = model)) +
        geom_hline(yintercept = 0, color = 'red',
                   linetype = 2) + # line at null behind coefs
        geom_point(size = 6) +
        geom_errorbar(aes(ymin=(conf.low), 
                          ymax=(conf.high)), width=.1) +
        scale_color_manual(values=c('steelblue4', 'steelblue1'
                                    #, 'firebrick4'
        )) +
        #coord_flip() + # flip x and y axes
        labs(title = 'The association between late development size and late development glucose response 
(stress - baseline) from models stratified by amount of parental care') +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        theme(plot.subtitle = element_text(hjust = 0.5, size = 14)) + 
        # bold and size title and axes labels
        theme(text = element_text(size=20, face = 'bold')) +
        theme(legend.position = 'none') +
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
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
              legend.position = 'none', #c(0.91, 0.94),
              legend.key = element_blank()) +
        xlab(expression(italic("Models stratified by parental care"))) +
        ylab(expression
             (atop(bold("Beta estimate and 95% CI"),
                   paste(italic("Glucose response (mg/dl) per 1 SD wing length"))))) 
      
      
      print(gluc.diff.by.wing.by.care.plot)
      
      ## j) Save Plot
      # use ggsave to save the plot
      ggsave('gluc.diff.by.wing.by.care.plot.pdf', 
             plot = gluc.diff.by.wing.by.care.plot, 
             device = NULL,
             path = paste0(here(),'/output'), 
             scale = 1, width = 12.5,
             height = 9,
             units = c('in'), dpi = 300, limitsize = TRUE)      
      
      
  ### 5.4 Parental care stratified graph of regression estimates for late 
      # development glucose response by growth (late - mid wing length)
    ## a) Low parental care model: extract estimates and tidy the data frame 
      gluc.diff.grow.low.care.est <- tidy(gluc.diff.grow.low.care.lmm, 
                                          conf.int = T, 
                                          conf.method = 'profile', 
                                          conf.level = 0.95) %>%
        filter(term == 'scale(rt.wing.diff)')
      
    ## b) Low parental care model: Label the estimates in data frame
      gluc.diff.grow.low.care.est$model <- c("Low parental care")
      
      
    ## c) Avg parental care model: extract estimates and tidy the data frame 
      gluc.diff.grow.avg.care.est <- tidy(gluc.diff.grow.avg.care.lmm, 
                                          conf.int = T, 
                                          conf.method = 'profile', 
                                          conf.level = 0.95) %>%
        filter(term == 'scale(rt.wing.diff)')
      
    ## d) Avg parental care model: Label the estimates in data frame
      gluc.diff.grow.avg.care.est$model <- c("Avg. parental care")
      
    # ## e) High parental care model: extract estimates and tidy the data frame 
    #   gluc.diff.grow.hi.care.est <- tidy(gluc.diff.grow.hi.care.lmm, 

    #                                      conf.int = T, 
    #                                      conf.method = 'profile', 
    #                                      conf.level = 0.95) %>%
    #     filter(term == 'scale(rt.wing.diff)')
    #   
    # ## f) High parental care model: Label the estimates in data frame
    #   gluc.diff.grow.hi.care.est$model <- c("High parental care")  
      
      
    ## g) Combine regression estimates into a tidy table
      gluc.diff.by.grow.by.care <- bind_rows(gluc.diff.grow.low.care.est,
                                             gluc.diff.grow.avg.care.est
                                             #, gluc.diff.grow.hi.care.est
                                             )
      
    ## h) Re-code *nominal* factor (with ordered levels)  
      # Set levels (odering) of 'model' variable 
      gluc.diff.by.grow.by.care <- 
        transform(gluc.diff.by.grow.by.care, 
                  model = factor(model,
                                 levels = c('Low parental care',
                                            'Avg. parental care'
                                            #, 'High parental care'
                                            )))  
      
    ## i) Graph results of baseline vs stressed state glucose at mid and late
      # development
      gluc.diff.by.grow.by.care.plot <- 
        ggplot(gluc.diff.by.grow.by.care, aes(x = model, y = estimate, 
                                              color = model)) +
        geom_hline(yintercept = 0, color = 'red',
                   linetype = 2) + # line at null behind coefs
        geom_point(size = 6) +
        geom_errorbar(aes(ymin=(conf.low), 
                          ymax=(conf.high)), width=.1) +
        scale_color_manual(values=c('steelblue4', 'steelblue1'
                                    #, 'firebrick4'
        )) +
        #coord_flip() + # flip x and y axes
        labs(title = 'The association between growth on late development glucose response 
(stress - baseline) from models stratified by amount of parental care') +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        theme(plot.subtitle = element_text(hjust = 0.5, size = 14)) + 
        # bold and size title and axes labels
        theme(text = element_text(size=20, face = 'bold')) +
        theme(legend.position = 'none') +
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
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
              legend.position = 'none', #c(0.91, 0.94),
              legend.key = element_blank()) +
        xlab(expression(italic("Models stratified by parental care"))) +
        ylab(expression
             (atop(bold("Beta estimate and 95% CI"),
                   paste(italic("Glucose response (mg/dl) per 1 SD growth"))))) 
      
      
      print(gluc.diff.by.grow.by.care.plot)
      
    ## j) Save Plot
      # use ggsave to save the plot
      ggsave('gluc.diff.by.grow.by.care.plot.pdf', 
             plot = gluc.diff.by.grow.by.care.plot, 
             device = NULL,
             path = paste0(here(),'/output'), 
             scale = 1, width = 12.5,
             height = 9,
             units = c('in'), dpi = 300, limitsize = TRUE)

  


      
  
      
    
               