###############################################################################
#############              Barn swallow parental care             #############
#############               and offspring physiology              #############
#############                                                     #############
#############              6. Effect modification by              #############
#############               parental care behaviors               #############
#############                  By: Zach Laubach                   #############
#############                created: 10 June 2022                #############
#############              last updated: 13 Nov 2024              #############
###############################################################################



### PURPOSE: Check for and model effect modification on the relationship 
           # between size/growth and glucose by parental care 
  
  
  # Code Blocks
    # 1: Configure work space
    # 2: Load RData
    # 3: Interaction models
    # 4: Stratified models
    # 5: Additional graphs of results

    

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
      
    # load ggeffects packages
      library('ggeffects')
      
      # library('hrbrthemes')
      # 
      # library('viridis')
      # 
      # # load gridExtra packages
      # library('gridExtra')
      
    ## c) Modeling Packages
      # load lme4 packages
      library ('lme4')
      # detach('package:lmerTest',
      #        unload = TRUE) # make sure lmerTest is not attached - influences CIs

      # load performance
      library('performance')
      
      # load nlme packages
      #library ('nlme')
      
      # load broom packages
      #library('broom')
      library('broom.mixed')
      
      # load dharma
      library('DHARMa')
      
      library('lmerTest')
      
      # prevent lmerTest from masking the lme4 function for lmer
      lmer <- lme4::lmer
      
        
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
    ## a) Load RData tidy barn swallow data (parental care data)
      load(here('data/6_bs_phys_sz_care_data.RData'))
    
      
      
###############################################################################
##############                3.Interaction models               ##############
###############################################################################

    
  ### 3.1 Late dev. baseline glucose by wing length by parental care (feed)
      # interaction models
    ## Generates output for Table S12
    ## a) Late dev. baseline glucose by late dev.
      # right wing length x feed.indx
      base.gluc.wing.x.care.lmm <- lmerTest::lmer(base.gluc ~ scale(rt.wing.length) *
                                        feed.indx +
                                        scale(base.gluc.s) +
                                        scale(nestling.age) +
                                        (1|nest.id),
                                        data = subset(late_nestling_parent_care,
                                                  !is.na(x = feed.indx) &
                                                # sensitivity 3 - index 4
                                                # care.indx.4 == 'low' &
                                                  !is.na(x = base.gluc) &
                                              # indx.3 and indx.4 have equal
                                              # samp size
                                                  !is.na(x = rt.wing.length) &
                                                  !is.na(x = nestling.band)))

    ## b) Model summary
      summary(base.gluc.wing.x.care.lmm)    # model summary
      confint(base.gluc.wing.x.care.lmm, level = 0.95,
              method = 'profile')  # 95% CIs
      #plot(base.gluc.wing.x.care.lmm)       # check residuals
      
    ## c) Marginal and conditional R-squared
      r2_nakagawa(base.gluc.wing.x.care.lmm)
      #, by_group=T)


  ### 3.2 Late dev. baseline glucose by growth by parental care (feed)
      # interaction models

    ## a) Late dev. baseline glucose by late dev.
      # growth x feed.indx
      base.gluc.grow.x.care.lmm <- lmerTest::lmer(base.gluc ~ scale(rt.wing.diff) *
                                          feed.indx +
                                          scale(base.gluc.s) +
                                          scale(nestling.age) +
                                          (1|nest.id),
                                      data = subset(late_nestling_parent_care,
                                              !is.na(x = feed.indx) &
                                            # sensitivity 3 - index 4
                                            # care.indx.4 == 'low' &
                                              !is.na(x = base.gluc) &
                                            # indx.3 and indx.4 have equal
                                            # samp size
                                              !is.na(x = rt.wing.diff) &
                                              !is.na(x = nestling.band)))

    ## b) Model summary
      summary(base.gluc.grow.x.care.lmm)    # model summary
      confint(base.gluc.grow.x.care.lmm, level = 0.95,
              method = 'profile')  # 95% CIs
      #plot(base.gluc.grow.x.care.lmm)       # check residuals
      
    ## c) Marginal and conditional R-squared
      r2_nakagawa(base.gluc.grow.x.care.lmm)
      #, by_group=T)


  ### 3.3 Late dev. glucose response (stress - baseline) by wing length by
      # parental care (feed) interaction models

    ## a) Late dev. glucose response by late dev.
          # right wing length x feed.indx
      gluc.diff.wing.x.care.lmm <- lmerTest::lmer(gluc.diff ~ scale(rt.wing.length) *
                                          feed.indx +
                                          scale(base.gluc.s) +
                                          scale(nestling.age) +
                                          (1|nest.id),
                                        data = subset(late_nestling_parent_care,
                                              !is.na(x = feed.indx) &
                                            # sensitivity 3 - index 4
                                            # care.indx.4 == 'low' &
                                              !is.na(x = gluc.diff) &
                                            # indx.3 and indx.4 have equal
                                            # samp size
                                              !is.na(x = rt.wing.length) &
                                              !is.na(x = nestling.band)))

    ## b) Model summary
      summary(gluc.diff.wing.x.care.lmm)    # model summary
      confint(gluc.diff.wing.x.care.lmm, level = 0.95,
              method = 'profile')  # 95% CIs
      #plot(gluc.diff.wing.x.care.lmm)       # check residuals
      
    ## c) Marginal and conditional R-squared
      r2_nakagawa(gluc.diff.wing.x.care.lmm)
      #, by_group=T)

  
  ### 3.4 Late dev. glucose response (stress - baseline) by right wing growth by
      # parental care (feed) interaction models

    ## a) Late dev. glucose response by late dev.
      # right wing growth x feed.indx
      gluc.diff.grow.x.care.lmm <- lmerTest::lmer(gluc.diff ~ scale(rt.wing.diff) *
                                          feed.indx +
                                          scale(base.gluc.s) +
                                          scale(nestling.age) +
                                          (1|nest.id),
                                      data = subset(late_nestling_parent_care,
                                             !is.na(x = feed.indx) &
                                           # sensitivity 3 - index 4
                                           # care.indx.4 == 'low' &
                                             !is.na(x = gluc.diff) &
                                           # indx.3 and indx.4 have equal
                                           # samp size
                                             !is.na(x = rt.wing.diff) &
                                             !is.na(x = nestling.band)))

    ## b) Model summary
      summary(gluc.diff.grow.x.care.lmm)    # model summary
      confint(gluc.diff.grow.x.care.lmm, level = 0.95,
              method = 'profile')  # 95% CIs
      #plot(gluc.diff.grow.x.care.lmm)       # check residuals
      
    ## c) Marginal and conditional R-squared
      r2_nakagawa(gluc.diff.grow.x.care.lmm)
      #, by_group=T)
      

      

###############################################################################
##############                4. Stratified models               ##############
###############################################################################
      
  ### 4.1 Late dev. baseline glucose by wing length, stratified by parental 
      # care models  **** STRATIFIED MODELS
    ## Plot used to create Figure 4 and generate output for Table S12
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
    
    ## b) model summary
      summary(base.gluc.wing.low.care.lmm)    # model summary 
      confint(base.gluc.wing.low.care.lmm, level = 0.95, method = 'profile')
      #plot(base.gluc.wing.low.care.lmm)       # check residuals
      # check boot strap CI
      # set.seed(2019) 
      # confint(base.gluc.wing.low.care.lmm, level = 0.95 , method = 'boot', 
      #         boot.type = 'perc')
      
    ## c) Marginal and conditional R-squared
      r2_nakagawa(base.gluc.wing.low.care.lmm)
      #, by_group=T)
      
    ## c) Use ggeffects to extract predicted values 
      base.gluc.wing.low.care.pred <- predict_response(base.gluc.wing.low.care.lmm,
                                                       terms = c('rt.wing.length'))

    ## d) Avg. parental care: late dev. baseline glucose by late dev. 
      # right wing length 
      # Note: 'avg' renamed 'high' during review
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
      
    ## e) model summary  
      summary(base.gluc.wing.avg.care.lmm)    # model summary 
      confint(base.gluc.wing.avg.care.lmm, level = 0.95, method = 'profile')
      #plot(base.gluc.wing.avg.care.lmm)       # check residuals
      
      ## Marginal and conditional R-squared
      r2_nakagawa(base.gluc.wing.avg.care.lmm)
      #, by_group=T)
      
    ## f) Use ggeffects to extract predicted values 
      base.gluc.wing.avg.care.pred <- predict_response(base.gluc.wing.avg.care.lmm,
                                                      terms = c('rt.wing.length'))
      
    ## g) A graph of the predicted values and raw data for the relationship
      # between right wing length and baseline glucose in low vs
      # average parental care nestlings *** STRATIFIED MODELS ***
      # Note: 'avg' renamed 'high' during review
      
      base.gluc.by.wing.by.care.strat.pred.plot <- 
        # raw data plot
        ggplot(data = subset(late_nestling_parent_care,
                             !is.na(x = feed.indx) &
                               !is.na(x = base.gluc) &
                               !is.na(x = rt.wing.length) &
                               !is.na(x = nestling.band))) +
        #xlim(10,35) +
        geom_point(aes(x = rt.wing.length, y = base.gluc, fill = feed.indx),
                   color = 'black', shape = 21, # black outline around points
                   size = 4) +
        scale_fill_manual(name = 'parental care', labels = c('low', 'high'), 
                          values=c('steelblue4', 'steelblue1')) +
        # predicted values plotted line for low parental care model
        geom_line(data = base.gluc.wing.low.care.pred, 
                  aes(x = x, y = predicted), linewidth = 2, 
                  color =  'steelblue4', show.legend = F) +
        # predicted values plotted line for average parental care model
        # Note: 'avg' renamed 'high' during review
        geom_line(data = base.gluc.wing.avg.care.pred, 
                  aes(x = x, y = predicted), linewidth = 2, 
                  color =  'steelblue1',show.legend = F) +
        # Titles, axes, and legends
 #        labs(title = 'Predicted values and raw data for the relationship between
 # right wing length and baseline glucose levels (stratified models)') +
        theme(plot.title = element_text(hjust = 0.5, size = 14)) + # center title
        # bold and size title and axes labels
        theme(text = element_text(size=24, face = 'bold')) +
        #theme(legend.position = 'none') +
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
        # add major axes
        theme(axis.line = element_line(colour = 'black',
                                       size = 0.5, linetype = 'solid')) +
        
        # change axes font style, color, size, angle, margin, and legend
        theme(axis.text.x = element_text(face='bold', color='black', 
                                         size=24, angle=0,
                                         margin = margin(t = 10, r = 0, 
                                                         b = 10, l = 0)),
              axis.text.y = element_text(face='bold', color='black', 
                                         size=24, angle=0, 
                                         margin = margin(t = 0, r = 10, 
                                                         b = 0, l = 0)),
              legend.title = element_text(size = 20),
              legend.text = element_text(size=20),
              legend.position = c(0.19, 0.9),
              legend.key = element_blank()) +
        xlab(expression(bold('right wing length (mm)'))) +
        ylab(expression(bold('baseline glucose (mg/dL)'))) 
      
    ## h) View plot
      print(base.gluc.by.wing.by.care.strat.pred.plot)
      
    ## i) Save Plot
      # use ggsave to save the plot
      ggsave('base.gluc.by.wing.by.care.strat.pred.plot.pdf', 
             plot = base.gluc.by.wing.by.care.strat.pred.plot, 
             device = NULL,
             path = paste0(here(),'/output'), 
             scale = 1, width = 6.5,
             height = 7,
             units = c('in'), dpi = 300, limitsize = TRUE)   
      
      
  ### 4.2 Late dev. baseline glucose by growth, stratified by parental 
      # care models. **** STRATIFIED MODELS
    ## Plot used to create Figure 4 and generate output for Table S12  
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
      
    ## b) model summary
      summary(base.gluc.grow.low.care.lmm)    # model summary 
      confint(base.gluc.grow.low.care.lmm, level = 0.95, method = 'profile')
      #plot(base.gluc.grow.low.care.lmm)       # check residuals
      
      ## Marginal and conditional R-squared
      r2_nakagawa(base.gluc.grow.low.care.lmm)
      #, by_group=T)
      
    ## c) Use ggeffects to extract predicted values 
      base.gluc.grow.low.care.pred <- predict_response(base.gluc.grow.low.care.lmm,
                                                   terms = c('rt.wing.diff'))
      
    ## d) Avg. parental care: late dev. baseline glucose by growth
      # Note: 'avg' renamed 'high' during review
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
      
    ## e) model summary
      summary(base.gluc.grow.avg.care.lmm)    # model summary 
      confint(base.gluc.grow.avg.care.lmm, level = 0.95, method = 'profile')
      #plot(base.gluc.grow.avg.care.lmm)       # check residuals
      
      ## Marginal and conditional R-squared
      r2_nakagawa(base.gluc.grow.avg.care.lmm)
      #, by_group=T)
     
    ## f) Use ggeffects to extract predicted values  
      base.gluc.grow.avg.care.pred <- predict_response(base.gluc.grow.avg.care.lmm,
                                                        terms = c('rt.wing.diff'))  
      
    ## g) A graph of the predicted values and raw data for the relationship
      # between growth and baseline glucose in low vs
      # average parental care nestlings *** STRATIFIED MODELS ***
      # Note: 'avg' renamed 'high' during review
      base.gluc.by.grow.by.care.strat.pred.plot <- 
        # raw data plot
        ggplot(data = subset(late_nestling_parent_care,
                             !is.na(x = feed.indx) &
                               !is.na(x = base.gluc) &
                               !is.na(x = rt.wing.diff) &
                               !is.na(x = nestling.band))) +
        xlim(10,35) +
        geom_point(aes(x = rt.wing.diff, y = base.gluc, fill = feed.indx),
                   color = 'black', shape = 21, # black outline around points
                   size = 4) +
        scale_fill_manual(name = 'parental care', labels = c('low', 'high'), 
                          values=c('steelblue4', 'steelblue1')) +
        # predicted values plotted line for low parental care model
        geom_line(data = base.gluc.grow.low.care.pred, 
                  aes(x = x, y = predicted), linewidth = 2, 
                  color =  'steelblue4', show.legend = F) +
        # predicted values plotted line for average parental care model
        # Note: 'avg' renamed 'high' during review
        geom_line(data = base.gluc.grow.avg.care.pred, 
                  aes(x = x, y = predicted), linewidth = 2, 
                  color =  'steelblue1',show.legend = F) +
        # Titles, axes, and legends
 #        labs(title = 'Predicted values and raw data for the relationship between
 # right wing growth and baseline glucose levels (stratified models)') +
        theme(plot.title = element_text(hjust = 0.5, size = 14)) + # center title
        # bold and size title and axes labels
        theme(text = element_text(size=24, face = 'bold')) +
        #theme(legend.position = 'none') +
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
        # add major axes
        theme(axis.line = element_line(colour = 'black',
                                       size = 0.5, linetype = 'solid')) +
        
        # change axes font style, color, size, angle, margin, and legend
        theme(axis.text.x = element_text(face='bold', color='black', 
                                         size=24, angle=0,
                                         margin = margin(t = 10, r = 0, 
                                                         b = 10, l = 0)),
              axis.text.y = element_text(face='bold', color='black', 
                                         size=24, angle=0, 
                                         margin = margin(t = 0, r = 10, 
                                                         b = 0, l = 0)),
              legend.title = element_text(size = 20),
              legend.text = element_text(size=20),
              legend.position = c(0.19, 0.9),
              legend.key = element_blank()) +
        xlab(expression(bold('right wing growth (mm)'))) +
        ylab(expression(bold('baseline glucose (mg/dL)'))) 
      
    ## h) View plot
      print(base.gluc.by.grow.by.care.strat.pred.plot)
      
    ## i) Save Plot
      # use ggsave to save the plot
      ggsave('base.gluc.by.grow.by.care.strat.pred.plot.pdf', 
             plot = base.gluc.by.grow.by.care.strat.pred.plot, 
             device = NULL,
             path = paste0(here(),'/output'), 
             scale = 1, width = 6.5,
             height = 7,
             units = c('in'), dpi = 300, limitsize = TRUE)           
      
      
  ### 4.3 Late dev. glucose response (stress - baseline) by rt.wing.length, 
      # stratified by parental care models  **** STRATIFIED MODELS
    ## Plot used to create Figure 4 and generate output for Table S12 
    ## a) Low parental care: late dev. glucose response by rt.wing.length
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
      
    ## b) model summary  
      summary(gluc.diff.wing.low.care.lmm)    # model summary 
      confint(gluc.diff.wing.low.care.lmm, level = 0.95, method = 'profile')
      #plot(gluc.diff.wing.low.care.lmm)       # check residuals
      
      ## Marginal and conditional R-squared
      r2_nakagawa(gluc.diff.wing.low.care.lmm, tolerance = 1e-20)
      #, by_group=T)
      
    ## c) Use ggeffects to extract predicted values 
      gluc.diff.wing.low.care.pred <- predict_response(gluc.diff.wing.low.care.lmm,
                                                       terms = c('rt.wing.length'))
      
    ## d) Avg. parental care: late dev. glucose response by rt.wing.length
      # Note: 'avg' renamed 'high' during review
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
    ## e) model summary    
      summary(gluc.diff.wing.avg.care.lmm)    # model summary 
      confint(gluc.diff.wing.avg.care.lmm, level = 0.95, method = 'profile')
      #plot(gluc.diff.wing.avg.care.lmm)       # check residuals
      
      ## Marginal and conditional R-squared
      r2_nakagawa(gluc.diff.wing.avg.care.lmm)
      #, by_group=T)
      
    ## f) Use ggeffects to extract predicted values 
      gluc.diff.wing.avg.care.pred <- predict_response(gluc.diff.wing.avg.care.lmm,
                                                       terms = c('rt.wing.length'))
      
    ## g) A graph of the predicted values and raw data for the relationship
      # right wing length and glucose response in low vs
      # average parental care nestlings *** STRATIFIED MODELS ***
      # Note: 'avg' renamed 'high' during review
      gluc.diff.by.wing.by.care.strat.pred.plot <- 
        # raw data plot
        ggplot(data = subset(late_nestling_parent_care,
                             !is.na(x = feed.indx) &
                               !is.na(x = gluc.diff) &
                               !is.na(x = rt.wing.length) &
                               !is.na(x = nestling.band))) +
        #xlim(10,35) +
        geom_point(aes(x = rt.wing.length, y = gluc.diff, fill = feed.indx),
                   color = 'black', shape = 21, # black outline around points
                   size = 4) +
        scale_fill_manual(name = 'parental care', labels = c('low', 'high'), 
                          values=c('steelblue4', 'steelblue1')) +
        # predicted values plotted line for low parental care model
        geom_line(data = gluc.diff.wing.low.care.pred, 
                  aes(x = x, y = predicted), linewidth = 2, 
                  color =  'steelblue4', show.legend = F) +
        # predicted values plotted line for average parental care model
        # Note: 'avg' renamed 'high' during review
        geom_line(data = gluc.diff.wing.avg.care.pred, 
                  aes(x = x, y = predicted), linewidth = 2, 
                  color =  'steelblue1',show.legend = F) +
        # Titles, axes, and legends
 #        labs(title = 'Predicted values and raw data for the relationship between
 # right wing length and glucose response (stratified models)') +
        theme(plot.title = element_text(hjust = 0.5, size = 14)) + # center title
        # bold and size title and axes labels
        theme(text = element_text(size=24, face = 'bold')) +
        #theme(legend.position = 'none') +
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
        # add major axes
        theme(axis.line = element_line(colour = 'black',
                                       size = 0.5, linetype = 'solid')) +
        
        # change axes font style, color, size, angle, margin, and legend
        theme(axis.text.x = element_text(face='bold', color='black', 
                                         size=24, angle=0,
                                         margin = margin(t = 10, r = 0, 
                                                         b = 10, l = 0)),
              axis.text.y = element_text(face='bold', color='black', 
                                         size=24, angle=0, 
                                         margin = margin(t = 0, r = 10, 
                                                         b = 0, l = 0)),
              legend.title = element_text(size = 20),
              legend.text = element_text(size=20),
              legend.position = c(0.19, 0.9),
              legend.key = element_blank()) +
        xlab(expression(bold('right wing length (mm)'))) +
        ylab(expression(bold('glucose response (mg/dL)'))) 
      
    ## h) View plot
      print(gluc.diff.by.wing.by.care.strat.pred.plot)
      
    ## i) Save Plot
      # use ggsave to save the plot
      ggsave('gluc.diff.by.wing.by.care.strat.pred.plot.pdf', 
             plot = gluc.diff.by.wing.by.care.strat.pred.plot, 
             device = NULL,
             path = paste0(here(),'/output'), 
             scale = 1, width = 6.5,
             height = 7,
             units = c('in'), dpi = 300, limitsize = TRUE)           
      
      
  ### 4.4 Late dev. glucose response (stress - baseline) by growth, 
      # stratified by parental care models. **** STRATIFIED MODELS
    ## Plot used to create Figure 4 and generate output for Table S12
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
    ## b) model summary
      summary(gluc.diff.grow.low.care.lmm)    # model summary 
      confint(gluc.diff.grow.low.care.lmm, level = 0.95, method = 'profile')
      #plot(gluc.diff.grow.low.care.lmm)       # check residuals
      
      ## Marginal and conditional R-squared
      r2_nakagawa(gluc.diff.grow.low.care.lmm)
      #, by_group=T)
      
    ## c) Use ggeffects to extract predicted values 
      gluc.diff.grow.low.care.pred <- predict_response(gluc.diff.grow.low.care.lmm,
                                                       terms = c('rt.wing.diff'))
      
    ## d) Avg. parental care: late dev. glucose response by growth
      # Note: 'avg' renamed 'high' during review
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
    ## e) model summary 
      summary(gluc.diff.grow.avg.care.lmm)    # model summary 
      confint(gluc.diff.grow.avg.care.lmm, level = 0.95, method = 'profile')
      #plot(gluc.diff.grow.avg.care.lmm)       # check residuals
      
      ## Marginal and conditional R-squared
      r2_nakagawa(gluc.diff.grow.avg.care.lmm)
      #, by_group=T)
      
    ## f) Use ggeffects to extract predicted values 
      gluc.diff.grow.avg.care.pred <- predict_response(gluc.diff.grow.avg.care.lmm,
                                                       terms = c('rt.wing.diff'))
                                                       
    ## g) A graph of the predicted values and raw data for the relationship
      # between growth and glucose response in low vs
      # average parental care nestlings *** STRATIFIED MODELS ***
      # Note: 'avg' renamed 'high' during review
      gluc.diff.by.grow.by.care.strat.pred.plot <- 
        # raw data plot
        ggplot(data = subset(late_nestling_parent_care,
                             !is.na(x = feed.indx) &
                               !is.na(x = gluc.diff) &
                               !is.na(x = rt.wing.diff) &
                               !is.na(x = nestling.band))) +
        xlim(10,35) +
        geom_point(aes(x = rt.wing.diff, y = gluc.diff, fill = feed.indx),
                   color = 'black', shape = 21, # black outline around points
                   size = 4) +
        scale_fill_manual(name = 'parental care', labels = c('low', 'high'), 
                          values=c('steelblue4', 'steelblue1')) +
        # predicted values plotted line for low parental care model
        geom_line(data = gluc.diff.grow.low.care.pred, 
                  aes(x = x, y = predicted), linewidth = 2, 
                  color =  'steelblue4', show.legend = F) +
        # predicted values plotted line for average parental care model
        # Note: 'avg' renamed 'high' during review
        geom_line(data = gluc.diff.grow.avg.care.pred, 
                  aes(x = x, y = predicted), linewidth = 2, 
                  color =  'steelblue1',show.legend = F) +
        # Titles, axes, and legends
 #        labs(title = 'Predicted values and raw data for the relationship between
 # right wing growth and glucose response (stratified models)') +
        theme(plot.title = element_text(hjust = 0.5, size = 14)) + # center title
        # bold and size title and axes labels
        theme(text = element_text(size=24, face = 'bold')) +
        #theme(legend.position = 'none') +
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
        # add major axes
        theme(axis.line = element_line(colour = 'black',
                                       size = 0.5, linetype = 'solid')) +
        # change axes font style, color, size, angle, margin, and legend
        theme(axis.text.x = element_text(face='bold', color='black', 
                                         size=24, angle=0,
                                         margin = margin(t = 10, r = 0, 
                                                         b = 10, l = 0)),
              axis.text.y = element_text(face='bold', color='black', 
                                         size=24, angle=0, 
                                         margin = margin(t = 0, r = 10, 
                                                         b = 0, l = 0)),
              legend.title = element_text(size = 20),
              legend.text = element_text(size=20),
              legend.position = c(0.19, 0.9),
              legend.key = element_blank()) +
        xlab(expression(bold('right wing growth (mm)'))) +
        ylab(expression(bold('glucose response (mg/dL)'))) 
      
    ## h) View plot
      print(gluc.diff.by.grow.by.care.strat.pred.plot)
      
    ## i) Save Plot
      # use ggsave to save the plot
      ggsave('gluc.diff.by.grow.by.care.strat.pred.plot.pdf', 
             plot = gluc.diff.by.grow.by.care.strat.pred.plot, 
             device = NULL,
             path = paste0(here(),'/output'), 
             scale = 1, width = 6.5,
             height = 7,
             units = c('in'), dpi = 300, limitsize = TRUE)      
      
      
      
      
###############################################################################
##############          5. Additional graphs of results          ##############
###############################################################################      

      
  ### 5.1 Parental care stratified graph of regression estimates for late 
      # development baseline glucose  by late development wing length
    ## Plot used to create Figure 4
    ## a) Low parental care model: extract estimates and tidy the data frame 
  
      base.gluc.wing.low.care.est <- tidy(base.gluc.wing.low.care.lmm, 
                                  conf.int = T, 
                                  conf.method = 'profile', 
                                  conf.level = 0.95) %>%
        filter(term == 'scale(rt.wing.length)')
      
    ## b) Low parental care model: Label the estimates in data frame
      base.gluc.wing.low.care.est$model <- c("low care")
      
    ## c) Avg parental care model: extract estimates and tidy the data frame 
      # Note: 'avg' renamed 'high' during review
      base.gluc.wing.avg.care.est <- tidy(base.gluc.wing.avg.care.lmm, 
                                          conf.int = T, 
                                          conf.method = 'profile', 
                                          conf.level = 0.95) %>%
        filter(term == 'scale(rt.wing.length)')
      
    ## d) Avg parental care model: Label the estimates in data frame
      # Note: 'avg' renamed 'high' during review
      base.gluc.wing.avg.care.est$model <- c("high care")
      
   
    ## e) Combine regression estimates into a tidy table
      base.gluc.by.wing.by.care <- bind_rows(base.gluc.wing.low.care.est,
                                      base.gluc.wing.avg.care.est
                                      #, base.gluc.wing.hi.care.est
                                      )
      
    ## f) Re-code *nominal* factor (with ordered levels)  
      # Set levels (odering) of 'model' variable 
      base.gluc.by.wing.by.care <- 
        transform(base.gluc.by.wing.by.care, 
                  model = factor(model,
                                          levels = c('low care',
                                                     'high care'
                                                     #,'High parental care'
                                                     )))  
      
    ## g) Graph results of baseline vs stressed state glucose at mid and late
      # development
      base.gluc.by.wing.by.care.plot <- 
        ggplot(base.gluc.by.wing.by.care, aes(x = model, y = estimate, 
                                       color = model)) +
        geom_hline(yintercept = 0, color = 'red',
                   linetype = 2, size = 2) + # line at null behind coefs
        geom_point(size = 8) +
        geom_errorbar(aes(ymin=(conf.low), 
                          ymax=(conf.high)), width = .1, size = 2) +
        scale_color_manual(values=c('steelblue4', 'steelblue1'
                                   # , 'firebrick4'
                                    )) +
        #coord_flip() + # flip x and y axes
#         labs(title = 'The association beween late development size and late development baseline 
# blood glucose (mg/dL) from models stratified by amount of parental care') +
        theme(plot.title = element_text(hjust = 0.5, size = 14)) + # center title
        theme(plot.subtitle = element_text(hjust = 0.5, size = 14)) + 
        # bold and size title and axes labels
        theme(text = element_text(size=24, face = 'bold')) +
        theme(legend.position = 'none') +
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
        # add major axes
        theme(axis.line = element_line(colour = 'black',
                                       size = 0.5, linetype = 'solid')) +
        # change axes font style, color, size, angle, margin, and legend
        theme(axis.text.x = element_text(face='bold', color='black', 
                                         size=24, angle=0,
                                         margin = margin(t = 10, r = 0, 
                                                         b = 10, l = 0)),
              axis.text.y = element_text(face='bold', color='black', 
                                         size=24, angle=0, 
                                         margin = margin(t = 0, r = 0, 
                                                         b = 0, l = 10)),
              legend.title=element_blank(),
              legend.text=element_text(size=14),
              legend.position = 'none', #c(0.91, 0.94),
              legend.key = element_blank()) +
        xlab(expression(italic("models stratified by parental care"))) +
        ylab(expression
             (atop(bold("beta estimate and 95% CI"),
    paste(italic("baseline glucose (mg/dL) / 1 SD wing length"))))) 
      
      
    print(base.gluc.by.wing.by.care.plot)
      
    ## h) Save Plot
      # use ggsave to save the plot
      ggsave('base.gluc.by.wing.by.care.plot.pdf', 
             plot = base.gluc.by.wing.by.care.plot, 
             device = NULL,
             path = paste0(here(),'/output'), 
             scale = 1, width = 8.5,
             height = 8,
             units = c('in'), dpi = 300, limitsize = TRUE)
      
      
  ### 5.2 Parental care stratified graph of regression estimates for late 
      # development baseline glucose by growth (late - mid wing length)
    ## Plot used to create Figure 4
    ## a) Low parental care model: extract estimates and tidy the data frame 
      base.gluc.grow.low.care.est <- tidy(base.gluc.grow.low.care.lmm, 
                                          conf.int = T, 
                                          conf.method = 'profile', 
                                          conf.level = 0.95) %>%
        filter(term == 'scale(rt.wing.diff)')
      
    ## b) Low parental care model: Label the estimates in data frame
      base.gluc.grow.low.care.est$model <- c("low care")
      
      
    ## c) Avg parental care model: extract estimates and tidy the data frame
      # Note: 'avg' renamed 'high' during review
      base.gluc.grow.avg.care.est <- tidy(base.gluc.grow.avg.care.lmm, 
                                          conf.int = T, 
                                          conf.method = 'profile', 
                                          conf.level = 0.95) %>%
        filter(term == 'scale(rt.wing.diff)')
      
    ## d) Avg parental care model: Label the estimates in data frame
      base.gluc.grow.avg.care.est$model <- c("high care")
      
    ## e) Combine regression estimates into a tidy table
      base.gluc.by.grow.by.care <- bind_rows(base.gluc.grow.low.care.est,
                                             base.gluc.grow.avg.care.est
                                            # , base.gluc.grow.hi.care.est
                                             )
      
    ## f) Re-code *nominal* factor (with ordered levels)  
      # Set levels (odering) of 'model' variable 
      base.gluc.by.grow.by.care <- 
        transform(base.gluc.by.grow.by.care, 
                  model = factor(model,
                                 levels = c('low care',
                                            'high care'
                                            #, 'High parental care'
                                            )))  
      
    ## g) Graph results of baseline vs stressed state glucose at mid and late
      # development
      base.gluc.by.grow.by.care.plot <- 
        ggplot(base.gluc.by.grow.by.care, aes(x = model, y = estimate, 
                                              color = model)) +
        geom_hline(yintercept = 0, color = 'red',
                   linetype = 2, size = 2) + # line at null behind coefs
        geom_point(size = 8) +
        geom_errorbar(aes(ymin=(conf.low), 
                          ymax=(conf.high)), width = .1, size = 2) +
        scale_color_manual(values=c('steelblue4', 'steelblue1'
                                    #, 'firebrick4'
                                    )) +
        #coord_flip() + # flip x and y axes
#         labs(title = 'The association between growth and late development baseline  
# blood glucose (mg/dL) from models stratified by amount of parental care') +
        theme(plot.title = element_text(hjust = 0.5, size = 14)) + # center title
        theme(plot.subtitle = element_text(hjust = 0.5, size = 14)) + 
        # bold and size title and axes labels
        theme(text = element_text(size=24, face = 'bold')) +
        theme(legend.position = 'none') +
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
        # add major axes
        theme(axis.line = element_line(colour = 'black',
                                       size = 0.5, linetype = 'solid')) +
        # change axes font style, color, size, angle, margin, and legend
        theme(axis.text.x = element_text(face='bold', color='black', 
                                         size=24, angle=0,
                                         margin = margin(t = 10, r = 0, 
                                                         b = 10, l = 0)),
              axis.text.y = element_text(face='bold', color='black', 
                                         size=24, angle=0, 
                                         margin = margin(t = 0, r = 0, 
                                                         b = 0, l = 10)),
              legend.title=element_blank(),
              legend.text=element_text(size=14),
              legend.position = 'none', #c(0.91, 0.94),
              legend.key = element_blank()) +
        xlab(expression(italic("models stratified by parental care"))) +
        ylab(expression
             (atop(bold("beta estimate and 95% CI"),
                   paste(italic("baseline glucose (mg/dL) / 1 SD growth"))))) 
      
      
      print(base.gluc.by.grow.by.care.plot)
      
    ## h) Save Plot
      # use ggsave to save the plot
      ggsave('base.gluc.by.grow.by.care.plot.pdf', 
             plot = base.gluc.by.grow.by.care.plot, 
             device = NULL,
             path = paste0(here(),'/output'), 
             scale = 1, width = 8.5,
             height = 8,
             units = c('in'), dpi = 300, limitsize = TRUE)
      
      
  ### 5.3 Parental care stratified graph of regression estimates for late 
      # development glucose response by late development wing length
    ## Plot used to create Figure 4
    ## a) Low parental care model: extract estimates and tidy the data frame 
      gluc.diff.wing.low.care.est <- tidy(gluc.diff.wing.low.care.lmm, 
                                          conf.int = T, 
                                          conf.method = 'profile', 
                                          conf.level = 0.95) %>%
        filter(term == 'scale(rt.wing.length)')
      
    ## b) Low parental care model: Label the estimates in data frame
      gluc.diff.wing.low.care.est$model <- c("low care")
      
      
    ## c) Avg parental care model: extract estimates and tidy the data frame
      # Note: 'avg' renamed 'high' during review
      gluc.diff.wing.avg.care.est <- tidy(gluc.diff.wing.avg.care.lmm, 
                                          conf.int = T, 
                                          conf.method = 'profile', 
                                          conf.level = 0.95) %>%
        filter(term == 'scale(rt.wing.length)')
      
    ## d) Avg parental care model: Label the estimates in data frame
      gluc.diff.wing.avg.care.est$model <- c("high care")
      
    ## e) Combine regression estimates into a tidy table
      gluc.diff.by.wing.by.care <- bind_rows(gluc.diff.wing.low.care.est,
                                             gluc.diff.wing.avg.care.est
                                             #, gluc.diff.wing.hi.care.est
      )
      
    ## f) Re-code *nominal* factor (with ordered levels)  
      # Set levels (odering) of 'model' variable 
      gluc.diff.by.wing.by.care <- 
        transform(gluc.diff.by.wing.by.care, 
                  model = factor(model,
                                 levels = c('low care',
                                            'high care'
                                            #,'High parental care'
                                 )))  
      
    ## g) Graph results of baseline vs stressed state glucose at mid and late
      # development
      gluc.diff.by.wing.by.care.plot <- 
        ggplot(gluc.diff.by.wing.by.care, aes(x = model, y = estimate, 
                                              color = model)) +
        geom_hline(yintercept = 0, color = 'red',
                   linetype = 2, size = 2) + # line at null behind coefs
        geom_point(size = 8) +
        geom_errorbar(aes(ymin=(conf.low), 
                          ymax=(conf.high)), width = .1, size = 2) +
        scale_color_manual(values=c('steelblue4', 'steelblue1'
                                    #, 'firebrick4'
        )) +
        #coord_flip() + # flip x and y axes
#         labs(title = 'The association between late development size and late development glucose 
# response (stress - baseline) from models stratified by amount of parental care') +
        theme(plot.title = element_text(hjust = 0.5, size = 14)) + # center title
        theme(plot.subtitle = element_text(hjust = 0.5, size = 14)) + 
        # bold and size title and axes labels
        theme(text = element_text(size=24, face = 'bold')) +
        theme(legend.position = 'none') +
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
        # add major axes
        theme(axis.line = element_line(colour = 'black',
                                       size = 0.5, linetype = 'solid')) +
        # change axes font style, color, size, angle, margin, and legend
        theme(axis.text.x = element_text(face='bold', color='black', 
                                         size=24, angle=0,
                                         margin = margin(t = 10, r = 0, 
                                                         b = 10, l = 0)),
              axis.text.y = element_text(face='bold', color='black', 
                                         size=24, angle=0, 
                                         margin = margin(t = 0, r = 0, 
                                                         b = 0, l = 10)),
              legend.title=element_blank(),
              legend.text=element_text(size=14),
              legend.position = 'none', #c(0.91, 0.94),
              legend.key = element_blank()) +
        xlab(expression(italic("models stratified by parental care"))) +
        ylab(expression
             (atop(bold("beta estimate and 95% CI"),
                   paste(italic("glucose response (mg/dL) / 1 SD wing length"))))) 
      
      
      print(gluc.diff.by.wing.by.care.plot)
      
    ## h) Save Plot
      # use ggsave to save the plot
      ggsave('gluc.diff.by.wing.by.care.plot.pdf', 
             plot = gluc.diff.by.wing.by.care.plot, 
             device = NULL,
             path = paste0(here(),'/output'), 
             scale = 1, width = 8.5,
             height = 8,
             units = c('in'), dpi = 300, limitsize = TRUE)      
      
      
  ### 5.4 Parental care stratified graph of regression estimates for late 
      # development glucose response by growth (late - mid wing length)
    ## Plot used to create Figure 4
    ## a) Low parental care model: extract estimates and tidy the data frame 
      gluc.diff.grow.low.care.est <- tidy(gluc.diff.grow.low.care.lmm, 
                                          conf.int = T, 
                                          conf.method = 'profile', 
                                          conf.level = 0.95) %>%
        filter(term == 'scale(rt.wing.diff)')
      
    ## b) Low parental care model: Label the estimates in data frame
      gluc.diff.grow.low.care.est$model <- c("low care")
      
      
    ## c) Avg parental care model: extract estimates and tidy the data frame
      # Note: 'avg' renamed 'high' during review
      gluc.diff.grow.avg.care.est <- tidy(gluc.diff.grow.avg.care.lmm, 
                                          conf.int = T, 
                                          conf.method = 'profile', 
                                          conf.level = 0.95) %>%
        filter(term == 'scale(rt.wing.diff)')
      
    ## d) Avg parental care model: Label the estimates in data frame
      gluc.diff.grow.avg.care.est$model <- c("high care")
      
    ## e) Combine regression estimates into a tidy table
      gluc.diff.by.grow.by.care <- bind_rows(gluc.diff.grow.low.care.est,
                                             gluc.diff.grow.avg.care.est
                                             #, gluc.diff.grow.hi.care.est
                                             )
      
    ## f) Re-code *nominal* factor (with ordered levels)  
      # Set levels (odering) of 'model' variable 
      gluc.diff.by.grow.by.care <- 
        transform(gluc.diff.by.grow.by.care, 
                  model = factor(model,
                                 levels = c('low care',
                                            'high care'
                                            #, 'High parental care'
                                            )))  
      
    ## g) Graph results of baseline vs stressed state glucose at mid and late
      # development
      gluc.diff.by.grow.by.care.plot <- 
        ggplot(gluc.diff.by.grow.by.care, aes(x = model, y = estimate, 
                                              color = model)) +
        geom_hline(yintercept = 0, color = 'red',
                   linetype = 2, size = 2) + # line at null behind coefs
        geom_point(size = 8) +
        geom_errorbar(aes(ymin=(conf.low), 
                          ymax=(conf.high)), width = .1, size = 2) +
        scale_color_manual(values=c('steelblue4', 'steelblue1'
                                    #, 'firebrick4'
        )) +
        #coord_flip() + # flip x and y axes
#         labs(title = 'The association between growth on late development glucose 
# (stress - baseline) from models stratified by amount of parental care') +
        theme(plot.title = element_text(hjust = 0.5, size = 14)) + # center title
        theme(plot.subtitle = element_text(hjust = 0.5, size = 14)) + 
        # bold and size title and axes labels
        theme(text = element_text(size=24, face = 'bold')) +
        theme(legend.position = 'none') +
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
        # add major axes
        theme(axis.line = element_line(colour = 'black',
                                       size = 0.5, linetype = 'solid')) +
        # change axes font style, color, size, angle, margin, and legend
        theme(axis.text.x = element_text(face='bold', color='black', 
                                         size=24, angle=0,
                                         margin = margin(t = 10, r = 0, 
                                                         b = 10, l = 0)),
              axis.text.y = element_text(face='bold', color='black', 
                                         size=24, angle=0, 
                                         margin = margin(t = 0, r = 0, 
                                                         b = 0, l = 10)),
              legend.title=element_blank(),
              legend.text=element_text(size=14),
              legend.position = 'none', #c(0.91, 0.94),
              legend.key = element_blank()) +
        xlab(expression(italic("models stratified by parental care"))) +
        ylab(expression
             (atop(bold("beta estimate and 95% CI"),
                   paste(italic("glucose response (mg/dL) / 1 SD growth"))))) 
      
      
      print(gluc.diff.by.grow.by.care.plot)
      
    ## h) Save Plot
      # use ggsave to save the plot
      ggsave('gluc.diff.by.grow.by.care.plot.pdf', 
             plot = gluc.diff.by.grow.by.care.plot, 
             device = NULL,
             path = paste0(here(),'/output'), 
             scale = 1, width = 8.5,
             height = 8,
             units = c('in'), dpi = 300, limitsize = TRUE)

  

