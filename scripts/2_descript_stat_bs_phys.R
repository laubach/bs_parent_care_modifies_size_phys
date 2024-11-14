###############################################################################
#############              Barn swallow parental care             #############
#############               and offspring physiology              #############
#############                                                     #############
#############                2. Descriptive Stats:                #############
#############                 Size and physiology                 #############
#############                                                     #############
#############                  By: Zach Laubach                   #############
#############                created: 12 May 2022                 #############
#############              last updated: 11 Nov 2024              #############
###############################################################################


  ### PURPOSE: Calculate descriptive statistics and visualize size and 
             # glucose data.
  
  
  # Code Blocks
    # 1: Configure work space
    # 2: Load RData
    # 3: Additional data tidying
    # 4: Univariate descriptive stats
    # 5: Bivariate descriptive stats
    # 6: Covariate models
    # 7: Export data files
    


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
        library ('tidyverse')

      # load here packages
        library ('here')
     
    ## b) Graph Plotting and Visualization Packages
      # load ggplot2 packages
        library ('ggplot2')
      
        library('hrbrthemes')
  
        library('viridis')

      # load gridExtra packages
        library ('gridExtra')
      
    ## b) Modelling Packages
      # load nlme packages
        library ('nlme')
      
        
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
    ## a) Load tidy barn swallow RData 
      load(here('data/2_5_tidy_bs_phys_data.RData'))
      
      

###############################################################################
##############             3. Additional data tidying            ##############
###############################################################################
    
  ### 3.1 Additional data tidying based on data exploration
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
    
    ## f) Subset the data to include only the late (~day 12) measurements
      late_nestling_size <- nestling_parent_care %>%
        filter(sample.state == 'late') %>%
        select(-c(mass.wing.index, rt.wing.length))
    
    ## g) Left join diff measurements to late_nestling_size data frame
      late_nestling_size <- late_nestling_size %>%
        left_join(diff_size, by = c('nestling.band' = 'nestling.band'), 
                  copy = F)
    
    ## h) Subset the data to include mid-dev. size.by.avg
      # NOTE: nestling size above and below average size by nest by
      # develop. state
      mid_size_by_avg <- nestling_parent_care %>%
        filter(sample.state == 'mid') %>%
        select(c(nestling.band, size.by.avg)) %>%
        rename(mid.size.by.avg = size.by.avg)
      
    ## i) Left join mid.size.by.avg measurements to late_nestling_size data 
      late_nestling_size <- late_nestling_size %>%
        left_join(mid_size_by_avg, by = c('nestling.band' = 'nestling.band'), 
                  copy = F)
      
    ## j) Rename size.by.avg variable as late.size.by.avg 
      late_nestling_size <- late_nestling_size %>%
        rename(late.size.by.avg = size.by.avg)
      
      
    
###############################################################################
##############          4. Univariate descriptive stats          ##############
###############################################################################
    
  ### 4.1 Histogram of glucose levels
    ## a) Histograms of baseline and stressed glucose measures at 
      # mid-development
      mid_gluc_hist <- nestling_parent_care_l %>%
        filter(sample.state == 'mid') %>%
        ggplot(aes(x = glucose, fill = glucose.sample)) + 
        geom_histogram(color='gray50', alpha=0.6, position = 'identity', 
                       binwidth = 12) +
        # #geom_point(aes(y=response), position = position_jitter(w = 0.3, h = 0)) +
        scale_fill_manual(values=c('blue1', 'coral4'), 
                          name = 'sample order',
                          labels = c('baseline', 'stressed')) +
        #theme_ipsum() +
        labs(title = 'Histogram of serially sampled glucose at mid-development',
             x ='Glucose (mg/dl)', 
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
      print(mid_gluc_hist)
      
    ## c) Save plot
      ggsave('mid_gluc_hist.pdf', plot = mid_gluc_hist, 
             device = NULL, 
             path = here('output/'), scale = 1, width = 12, 
             height = 6, 
             units = c('in'), dpi = 300, limitsize = TRUE) 
      
      
    ## d) Histograms of baseline and stressed glucose measures at
        # late development
      late_gluc_hist <- nestling_parent_care_l %>%
        filter(sample.state == 'late') %>%
        ggplot(aes(x = glucose, fill = glucose.sample)) + 
        geom_histogram(color='gray50', alpha=0.6, position = 'identity', 
                       binwidth = 12) +
        # #geom_point(aes(y=response), position = position_jitter(w = 0.3, h = 0)) +
        scale_fill_manual(values=c('dodgerblue4', 'red4'), 
                          name = 'sample order',
                          labels = c('baseline', 'stressed')) +
        #theme_ipsum() +
        labs(title = 'Histogram of serially sampled glucose at late development',
             x ='Glucose (mg/dl)', 
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
      print(late_gluc_hist)
      
    ## f) Save plot
      ggsave('late_gluc_hist.pdf', plot = late_gluc_hist, 
             device = NULL, 
             path = here('output/'), scale = 1, width = 12, 
             height = 6, 
             units = c('in'), dpi = 300, limitsize = TRUE) 
    
 
  ### 4.2 Histogram of difference glucose levels
    ## a) Histograms stressed minus baseline glucose measures
      gluc_diff_hist <- nestling_parent_care %>%
        ggplot(aes(x = gluc.diff, fill = sample.state)) + 
        geom_histogram(color='gray50', alpha=0.6, position = 'identity', 
                       binwidth = 12) +
        # #geom_point(aes(y=response), position = position_jitter(w = 0.3, h = 0)) +
        scale_fill_manual(values=c('palegreen4', 'steelblue4'), 
                          name = 'Developmental state',
                          labels = c('Mid-development', 'Late development')) +
        #theme_ipsum() +
        
        labs(title = 'Histogram of difference glucose measures (stress - baseline)
             by developmental state') +
        theme(plot.title = element_text(hjust = 0.5, size = 14)) + # center title
        theme(plot.subtitle = element_text(hjust = 0.5, size = 14)) + 
        # bold and size title and axes labels
        theme(text = element_text(size=28, face = 'bold')) +
        theme(legend.position = 'none') +
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
        # add major axes
        #theme(axis.line = element_line(colour = 'lightgrey', 
        #                               size = 1, linetype = 'solid')) + 
        # change axes font style, color, size, angle, margin, and legend
        theme(axis.text.x = element_text(face='bold', color='black', 
                                         size=28, angle=0,
                                         margin = margin(t = 10, r = 0, 
                                                         b = 10, l = 0)),
              axis.text.y = element_text(face='bold', color='black', 
                                         size=28, angle=0, 
                                         margin = margin(t = 0, r = 0, 
                                                         b = 0, l = 0)),
              legend.title=element_blank(),
              legend.text=element_text(size=14),
              #legend.position = 'none', #c(0.91, 0.94),
              legend.key = element_blank()) +
        xlab(expression(italic("Glucose response(mg/dl)"))) +
        ylab(expression
             (atop(bold("Frequency"))))
      
    ## b) Print plot 
      print(gluc_diff_hist)
      
    ## c) Save plot
      ggsave('gluc_diff_hist.pdf', plot = gluc_diff_hist, 
             device = NULL, 
             path = here('output/'), scale = 1, width = 12, 
             height = 6, 
             units = c('in'), dpi = 300, limitsize = TRUE)   
     
      
  ### 4.3 Scatter plot of repeated measures glucose measures by develop. state
    ## a) Scatter plot of baseline and stressed glucose measures at mid develop. 
      mid_gluc_samp_scatter <- nestling_parent_care_l %>%
        filter(sample.state == 'mid') %>%
        ggplot(aes(x = sample.time.s, y = glucose, 
                   color = glucose.sample, group = nestling.band)) + 
        geom_line(aes(color = diff.dir)) + geom_point() + 
        scale_color_manual(values=c('gray75','red', 'black', 'gray25'), 
                          name = 'direction and sample order',
                          labels = c('baseline', 'negative', 
                                     'positive', 'stressed')) +
        labs(title = 'Scatter plot of serially sampled glucose',
             x ='sampling time (s)', 
             y ='glucose (mg/dl)') +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        # bold and size title and axes labels
        theme(text = element_text(size=22, face = 'bold')) +
        theme(axis.ticks = element_blank()) + # remove axis ticks
        theme(axis.text.x = element_text(size=22, angle = 60, 
                                         vjust = 1, hjust=1)) +
        # add major axes
        theme(axis.line = element_line(colour = 'black',
                                       size = 0.5, linetype = 'solid')) +
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
        # change axes font style, color, size, angle, margin, and legend
        theme(axis.text.x = element_text(face='bold', color='black', 
                                         size=22, angle=0,
                                         margin = margin(t = 10, r = 0, 
                                                         b = 10, l = 0)),
              axis.text.y = element_text(face='bold', color='black', 
                                         size=22, angle=0, 
                                         margin = margin(t = 0, r = 0, 
                                                         b = 0, l = 0)),
              legend.title=element_blank(), #element_text(size=10)
              legend.text=element_text(size=10),
              legend.position = 'none', #c(0.91, 0.94),
              legend.key = element_blank()) 
      
    ## c) Print plot 
      print(mid_gluc_samp_scatter)
    
    ## d) Save plot
      ggsave('mid_gluc_samp_scatter.pdf', plot = mid_gluc_samp_scatter, 
             device = NULL, 
             path = here('output/'), scale = 1, width = 8.5, 
             height = 8, 
             units = c('in'), dpi = 300, limitsize = TRUE) 
      
    ## e) Scatterplot of baseline and stressed glucose measures at late develop. 
      late_gluc_samp_scatter <- nestling_parent_care_l %>%
        filter(sample.state == 'late') %>%
        ggplot(aes(x = sample.time.s, y = glucose, 
                   color = glucose.sample, group = nestling.band)) + 
        geom_line(aes(color = diff.dir)) + geom_point() + 
        scale_color_manual(values=c('gray75','red', 'black', 'gray25'), 
                           name = 'direction and sample order',
                           labels = c('baseline', 'negative', 
                                      'positive', 'stressed')) +
        labs(title = 'Scatter plot of serially sampled glucose',
             x ='sampling time (s)', 
             y ='glucose (mg/dl)') +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        # bold and size title and axes labels
        theme(text = element_text(size=22, face = 'bold')) +
        theme(axis.ticks = element_blank()) + # remove axis ticks
        theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
        # add major axes
        theme(axis.line = element_line(colour = 'black',
                                       size = 0.5, linetype = 'solid')) +
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
        # change axes font style, color, size, angle, margin, and legend
        theme(axis.text.x = element_text(face='bold', color='black', 
                                         size=22, angle=0,
                                         margin = margin(t = 10, r = 0, 
                                                         b = 10, l = 0)),
              axis.text.y = element_text(face='bold', color='black', 
                                         size=22, angle=0, 
                                         margin = margin(t = 0, r = 0, 
                                                         b = 0, l = 0)),
              legend.title=element_blank(), #element_text(size=10)
              legend.text=element_text(size=10),
              legend.position = 'none', #c(0.91, 0.94),
              legend.key = element_blank()) 
      
    ## f) Print plot 
      print(late_gluc_samp_scatter)
      
    ## g) Save plot
      ggsave('late_gluc_samp_scatter.pdf', plot = late_gluc_samp_scatter, 
             device = NULL, 
             path = here('output/'), scale = 1, width = 8.5, 
             height = 8, 
             units = c('in'), dpi = 300, limitsize = TRUE) 
      
     
  ### 4.4 Univariate descriptive stats 
    ## a) Descriptive stats blood glucose
      univar_glucose <- nestling_parent_care_l %>%
        group_by(sample.state, glucose.sample) %>%
        summarise (n.gluc = sum(!is.na(glucose)),
                   avg.gluc = round (mean(glucose, 
                                            na.rm = T),2),
                   stdev.gluc = round (sd(glucose, 
                                            na.rm = T), 2),
                   med.gluc = round(median(glucose,
                                             na.rm = T), 2),
                   min.gluc = round(min(glucose,
                                          na.rm = T), 2),
                   max.gluc = round(max(glucose,
                                          na.rm = T), 2),
                   avg.time.s = round (mean(sample.time.s, 
                                          na.rm = T),2),
                   stdev.time.s = round (sd(sample.time.s, 
                                          na.rm = T), 2),
                   med.time.s = round(median(sample.time.s,
                                           na.rm = T), 2),
                   min.time.s = round(min(sample.time.s,
                                        na.rm = T), 2),
                   max.time.s = round(max(sample.time.s,
                                        na.rm = T), 2))
      
    ## b) save the data frame of summary stats as a pdf into output file
      pdf(here('output/univar_glucose.pdf'), height = 3, width = 14)
      grid.table(univar_glucose)
      dev.off()
      
    ## c) Descriptive stats morphology
      univar_morph_mites <- nestling_parent_care %>%
        group_by(sample.state) %>%
        summarise (n.rt.wing = sum(!is.na(rt.wing.length)),
                   avg.rt.wing = round (mean(rt.wing.length, 
                                            na.rm = T),2),
                   stdev.rt.wing = round (sd(rt.wing.length, 
                                            na.rm = T), 2),
                   med.rt.wing = round(median(rt.wing.length,
                                             na.rm = T), 2),
                   min.rt.wing = round(min(rt.wing.length,
                                          na.rm = T), 2),
                   max.rt.wing = round(max(rt.wing.length,
                                          na.rm = T), 2),
                   n.mass = sum(!is.na(mass.pre.obs)),
                   avg.mass = round (mean(mass.pre.obs, 
                                             na.rm = T),2),
                   stdev.mass = round (sd(mass.pre.obs, 
                                             na.rm = T), 2),
                   med.mass = round(median(mass.pre.obs,
                                              na.rm = T), 2),
                   min.mass = round(min(mass.pre.obs,
                                           na.rm = T), 2),
                   max.mass = round(max(mass.pre.obs,
                                           na.rm = T), 2),
                   n.mass.wing = sum(!is.na(mass.wing.index)),
                   avg.mass.wing = round (mean(mass.wing.index, 
                                             na.rm = T),2),
                   stdev.mass.wing = round (sd(mass.wing.index, 
                                             na.rm = T), 2),
                   med.mass.wing = round(median(mass.wing.index,
                                              na.rm = T), 2),
                   min.mass.wing = round(min(mass.wing.index,
                                           na.rm = T), 2),
                   max.mass.wing = round(max(mass.wing.index,
                                           na.rm = T), 2),
                   n.mites = sum(!is.na(nos.mites)),
                   avg.mites = round (mean(nos.mites, 
                                          na.rm = T),2),
                   stdev.mites = round (sd(nos.mites, 
                                          na.rm = T), 2),
                   med.mites = round(median(nos.mites,
                                           na.rm = T), 2),
                   min.mites = round(min(nos.mites,
                                        na.rm = T), 2),
                   max.mites = round(max(nos.mites,
                                        na.rm = T), 2)) %>%
          ungroup()
      
    ## d) save the data frame of summary stats as a pdf into output file
      pdf(here('output/univar_morph_mites.pdf'), height = 2, width = 27)
      grid.table(univar_morph_mites)
      dev.off()
      
    ## e) Descriptive stats blood growth
      univar_growth <- late_nestling_size %>%
        summarise (n.rt.wing.diff = sum(!is.na(rt.wing.diff)),
                   avg.rt.wing.diff = round (mean(rt.wing.diff, 
                                          na.rm = T),2),
                   stdev.rt.wing.diff = round (sd(rt.wing.diff, 
                                          na.rm = T), 2),
                   med.rt.wing.diff = round(median(rt.wing.diff,
                                           na.rm = T), 2),
                   min.rt.wing.diff = round(min(rt.wing.diff,
                                        na.rm = T), 2),
                   max.rt.wing.diff = round(max(rt.wing.diff,
                                        na.rm = T), 2))
      
      ## b) save the data frame of summary stats as a pdf into output file
      pdf(here('output/univar_growth.pdf'), height = 3, width = 14)
      grid.table(univar_growth)
      dev.off()


      
###############################################################################
##############           5. Bivariate descriptive stats          ##############
###############################################################################
      
  ### 5.1 Bivariate descriptive stats 
  #   ## a) Blood glucose by min versus other size descriptive stats
  #     bivar_gluc_size <- nestling_parent_care %>%
  #       group_by(sample.state, size.order) %>%
  #       summarise (n.1st.gluc = sum(!is.na(base.gluc)),
  #                  avg.1st.gluc = round (mean(base.gluc, 
  #                                         na.rm = T),2),
  #                  stdev.1st.gluc = round (sd(base.gluc, 
  #                                         na.rm = T), 2),
  #                  med.1st.gluc = round(median(base.gluc,
  #                                          na.rm = T), 2),
  #                  min.1st.gluc = round(min(base.gluc,
  #                                       na.rm = T), 2),
  #                  max.1st.gluc = round(max(base.gluc,
  #                                       na.rm = T), 2),
  #                  n.gluc.diff = sum(!is.na(gluc.diff)),
  #                  avg.gluc.diff = round (mean(gluc.diff, 
  #                                             na.rm = T),2),
  #                  stdev.gluc.diff = round (sd(gluc.diff, 
  #                                             na.rm = T), 2),
  #                  med.gluc.diff = round(median(gluc.diff,
  #                                              na.rm = T), 2),
  #                  min.gluc.diff = round(min(gluc.diff,
  #                                           na.rm = T), 2),
  #                  max.gluc.diff = round(max(gluc.diff,
  #                                           na.rm = T), 2))
  #     
  #   ## b) Blood glucose by size above or below avg descriptive stats
  #     bivar_gluc_size_avg <- nestling_parent_care %>%
  #       group_by(sample.state, size.by.avg) %>%
  #       summarise (n.1st.gluc = sum(!is.na(base.gluc)),
  #                  avg.1st.gluc = round (mean(base.gluc, 
  #                                             na.rm = T),2),
  #                  stdev.1st.gluc = round (sd(base.gluc, 
  #                                             na.rm = T), 2),
  #                  med.1st.gluc = round(median(base.gluc,
  #                                              na.rm = T), 2),
  #                  min.1st.gluc = round(min(base.gluc,
  #                                           na.rm = T), 2),
  #                  max.1st.gluc = round(max(base.gluc,
  #                                           na.rm = T), 2),
  #                  n.gluc.diff = sum(!is.na(gluc.diff)),
  #                  avg.gluc.diff = round (mean(gluc.diff, 
  #                                              na.rm = T),2),
  #                  stdev.gluc.diff = round (sd(gluc.diff, 
  #                                              na.rm = T), 2),
  #                  med.gluc.diff = round(median(gluc.diff,
  #                                               na.rm = T), 2),
  #                  min.gluc.diff = round(min(gluc.diff,
  #                                            na.rm = T), 2),
  #                  max.gluc.diff = round(max(gluc.diff,
  #                                            na.rm = T), 2))
  #     
  # ## c) Blood glucose by mites descriptive stats   
  #     bivar_gluc_mite <- nestling_parent_care %>%
  #       group_by(sample.state, mite.bin) %>%
  #       summarise (n.1st.gluc = sum(!is.na(base.gluc)),
  #                  avg.1st.gluc = round (mean(base.gluc, 
  #                                             na.rm = T),2),
  #                  stdev.1st.gluc = round (sd(base.gluc, 
  #                                             na.rm = T), 2),
  #                  med.1st.gluc = round(median(base.gluc,
  #                                              na.rm = T), 2),
  #                  min.1st.gluc = round(min(base.gluc,
  #                                           na.rm = T), 2),
  #                  max.1st.gluc = round(max(base.gluc,
  #                                           na.rm = T), 2),
  #                  n.gluc.diff = sum(!is.na(gluc.diff)),
  #                  avg.gluc.diff = round (mean(gluc.diff, 
  #                                              na.rm = T),2),
  #                  stdev.gluc.diff = round (sd(gluc.diff, 
  #                                              na.rm = T), 2),
  #                  med.gluc.diff = round(median(gluc.diff,
  #                                               na.rm = T), 2),
  #                  min.gluc.diff = round(min(gluc.diff,
  #                                            na.rm = T), 2),
  #                  max.gluc.diff = round(max(gluc.diff,
  #                                            na.rm = T), 2))
  #     
  #     
  # ### 5.2 Bivariate graphs for mid-development (~day 8) blood glucose diffs.
  #   ## a) Mid-development difference blood glucose by mid-development 
  #     # relative nestling size boxplot
  #     mid_gluc_size_box <- nestling_parent_care %>%
  #       filter(sample.state == 'mid') %>%
  #       ggplot(aes(x = rt.wing.length, y = gluc.diff, fill = size.order)) + 
  #       geom_boxplot() + 
  #       scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  #       geom_jitter(color="black", size=2, alpha=0.9) +
  #       # scale_color_manual(values=c('#69b3a2', '#404080'), 
  #       #                    name = 'smallest vs other nestlings',
  #       #                    labels = c('small', 'other')) +
  #       labs(title = 'Mid-development difference in blood glucose (stressed - baseline) by 
  #            mid-development relative size',
  #            y ='Diff. glucose (mg/dl)') +
  #       theme(plot.title = element_text(hjust = 0.5)) + # center title
  #       theme(axis.ticks = element_blank()) + # remove axis ticks
  #       theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
  #       # remove background color
  #       theme(panel.background = element_rect(fill = 'white')) +
  #       # customize legend
  #       theme(legend.title=element_text(size=10),
  #             legend.text=element_text(size=8),
  #             legend.position = c(0.91, 0.94))
  #     
  #   ## b) Print plot 
  #     print(mid_gluc_size_box)
  #     
  #   ## c) Save plot
  #     ggsave('mid_gluc_size_box.pdf', plot = mid_gluc_size_box, 
  #            device = NULL, 
  #            path = here('output/'), scale = 1, width = 8, 
  #            height = 6, 
  #            units = c('in'), dpi = 300, limitsize = TRUE) 
    #   
    # ## d) Mid-development difference blood glucose by mid-development 
    #   # mites (y/n) boxplot
    #   mid_gluc_mite_box <- nestling_parent_care %>%
    #     filter(sample.state == 'mid') %>%
    #     ggplot(aes(x = mite.bin, y = gluc.diff, fill = mite.bin)) + 
    #     geom_boxplot() + 
    #     #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    #     #geom_jitter(color="black", size=2, alpha=0.9) +
    #     scale_color_manual(values=c('purple', 'dark green')) +
    #     labs(title = 'Mid-development difference in blood glucose (stressed - baseline) by 
    #          mid-development mites (y/n)',
    #          y ='Diff. glucose (mg/dl)') +
    #     theme(plot.title = element_text(hjust = 0.5)) + # center title
    #     theme(axis.ticks = element_blank()) + # remove axis ticks
    #     theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    #     # remove background color
    #     theme(panel.background = element_rect(fill = 'white')) +
    #     # customize legend
    #     theme(legend.title=element_text(size=10),
    #           legend.text=element_text(size=8),
    #           legend.position = c(0.91, 0.94))
    #   
    # ## e) Print plot 
    #   print(mid_gluc_mite_box)
    #   
    # ## f) Save plot
    #   ggsave('mid_gluc_mite_box.pdf', plot = mid_gluc_mite_box, 
    #          device = NULL, 
    #          path = here('output/'), scale = 1, width = 8, 
    #          height = 6, 
    #          units = c('in'), dpi = 300, limitsize = TRUE)   
    #   
    # ## g) Mid-development difference blood glucose by mid-development nest age
    #   mid_gluc_age_scatter <- nestling_parent_care %>%
    #     filter(sample.state == 'mid') %>%
    #     ggplot(aes(x = nestling.age, y = gluc.diff)) + 
    #     geom_point() + 
    #     labs(title = 'Mid-development difference in blood glucose (stressed - baseline) by 
    #          mid-development nest age',
    #          y ='Diff. glucose (mg/dl)') +
    #     theme(plot.title = element_text(hjust = 0.5)) + # center title
    #     theme(axis.ticks = element_blank()) + # remove axis ticks
    #     theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    #     # remove background color
    #     theme(panel.background = element_rect(fill = 'white')) +
    #     # customize legend
    #     theme(legend.title=element_text(size=10),
    #           legend.text=element_text(size=8),
    #           legend.position = c(0.91, 0.94))
    #   
    # ## h) Print plot 
    #   print(mid_gluc_age_scatter)
    #   
    # ## i) Save plot
    #   ggsave('mid_gluc_age_scatter.pdf', plot = mid_gluc_age_scatter, 
    #          device = NULL, 
    #          path = here('output/'), scale = 1, width = 8, 
    #          height = 6, 
    #          units = c('in'), dpi = 300, limitsize = TRUE)
    
    # ## j) Mid-development difference blood glucose by mid-development 
    #   # first blood sample  time (seconds)
    #   mid_gluc_base_time_scatter <- nestling_parent_care %>%
    #     filter(sample.state == 'mid') %>%
    #     ggplot(aes(x = base.gluc.s, y = gluc.diff)) + 
    #     geom_point() +
    #     geom_smooth(method = "loess", size = 1.5) +
    #     labs(title = 'Mid-development diff blood glucose (stress - baseline) by 
    #          mid-development baseline blood sample time (s)',
    #          y ='Diff. glucose (mg/dl)') +
    #     theme(plot.title = element_text(hjust = 0.5)) + # center title
    #     # bold and size title and axes labels
    #     theme(text = element_text(size=20, face = 'bold')) +
    #     theme(axis.ticks = element_blank()) + # remove axis ticks
    #     theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    #     # remove background color
    #     theme(panel.background = element_rect(fill = 'white')) +
    #     # change axes font style, color, size, angle, margin, and legend
    #     theme(axis.text.x = element_text(face='bold', color='black', 
    #                                      size=20, angle=0,
    #                                      margin = margin(t = 10, r = 0, 
    #                                                      b = 10, l = 0)),
    #           axis.text.y = element_text(face='bold', color='black', 
    #                                      size=20, angle=0, 
    #                                      margin = margin(t = 0, r = 0, 
    #                                                      b = 0, l = 0)),
    #           legend.title=element_blank(), #element_text(size=10)
    #           legend.text=element_text(size=10),
    #           legend.position = 'none', #c(0.91, 0.94),
    #           legend.key = element_blank()) 
    #     
    #   
    # ## k) Print plot 
    #   print(mid_gluc_base_time_scatter)
    #   
    # ## l) Save plot
    #   ggsave('mid_gluc_base_time_scatter.pdf', 
    #          plot = mid_gluc_base_time_scatter, 
    #          device = NULL, 
    #          path = here('output/'), scale = 1, width = 8, 
    #          height = 6, 
    #          units = c('in'), dpi = 300, limitsize = TRUE)
    #   
    # ## m) Mid-development difference in blood glucose by mid-development 
    #   # temperature (C) at around sunrise (observation trial start time)
    #   mid_gluc_temp_scatter <- nestling_parent_care %>%
    #     filter(sample.state == 'mid') %>%
    #     ggplot(aes(x = trial.temp, y = gluc.diff)) + 
    #     geom_point() +
    #     geom_smooth(method = "loess", size = 1.5) +
    #     labs(title = 'Mid-development difference in blood glucose (stress - baseline) by 
    #          mid-development trial temperature (C)',
    #          y ='Diff. glucose (mg/dl)') +
    #     theme(plot.title = element_text(hjust = 0.5)) + # center title
    #     theme(axis.ticks = element_blank()) + # remove axis ticks
    #     theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    #     # remove background color
    #     theme(panel.background = element_rect(fill = 'white')) +
    #     # customize legend
    #     theme(legend.title=element_text(size=10),
    #           legend.text=element_text(size=8),
    #           legend.position = c(0.91, 0.94))
    #   
    # ## n) Print plot 
    #   print(mid_gluc_temp_scatter)
    #   
    # ## o) Save plot
    #   ggsave('mid_gluc_temp_scatter.pdf', 
    #          plot = mid_gluc_temp_scatter, 
    #          device = NULL, 
    #          path = here('output/'), scale = 1, width = 8, 
    #          height = 6, 
    #          units = c('in'), dpi = 300, limitsize = TRUE)
    #   
    # ## p) Mid-development difference in blood glucose by mid-development 
    #   # number of nestlings in the nest
    #   mid_gluc_nos_nest_scatter <- nestling_parent_care %>%
    #     filter(sample.state == 'mid') %>%
    #     ggplot(aes(x = nestling.number, y = gluc.diff)) + 
    #     geom_point() +
    #     geom_smooth(method = "loess", size = 1.5) +
    #     labs(title = 'Mid-development difference in blood glucose (stress - baseline) by 
    #          mid-development number of nestlings',
    #          y ='Diff. glucose (mg/dl)') +
    #     theme(plot.title = element_text(hjust = 0.5)) + # center title
    #     theme(axis.ticks = element_blank()) + # remove axis ticks
    #     theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    #     # remove background color
    #     theme(panel.background = element_rect(fill = 'white')) +
    #     # customize legend
    #     theme(legend.title=element_text(size=10),
    #           legend.text=element_text(size=8),
    #           legend.position = c(0.91, 0.94))
    #   
    # ## q) Print plot 
    #   print(mid_gluc_nos_nest_scatter)
    #   
    # ## r) Save plot
    #   ggsave('mid_gluc_nos_nest_scatter.pdf', 
    #          plot = mid_gluc_nos_nest_scatter, 
    #          device = NULL, 
    #          path = here('output/'), scale = 1, width = 8, 
    #          height = 6, 
    #          units = c('in'), dpi = 300, limitsize = TRUE)
    #   
    #   
  # ### 5.3 Bivariate graphs for mid-development (~day 8) blood glucose baseline.  
  #   ## a) Mid-development baseline blood glucose by mid-development 
  #     # relative nestling size boxplot
  #     mid_base_gluc_size_box <- nestling_parent_care %>%
  #       filter(sample.state == 'mid') %>%
  #       ggplot(aes(x = rt.wing.length, y = base.gluc, fill = size.order)) + 
  #       geom_boxplot() + 
  #       scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  #       geom_jitter(color="black", size=2, alpha=0.9) +
  #       # scale_color_manual(values=c('#69b3a2', '#404080'), 
  #       #                    name = 'smallest vs other nestlings',
  #       #                    labels = c('small', 'other')) +
  #       labs(title = 'Mid-development baseline blood glucose by mid-development relative size',
  #            y ='Baseline glucose (mg/dl)') +
  #       theme(plot.title = element_text(hjust = 0.5)) + # center title
  #       theme(axis.ticks = element_blank()) + # remove axis ticks
  #       theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
  #       # remove background color
  #       theme(panel.background = element_rect(fill = 'white')) +
  #       # customize legend
  #       theme(legend.title=element_text(size=10),
  #             legend.text=element_text(size=8),
  #             legend.position = c(0.91, 0.94))
  #     
  #   ## b) Print plot 
  #     print(mid_base_gluc_size_box)
  #     
  #   ## c) Save plot
  #     ggsave('mid_base_gluc_size_box.pdf', plot = mid_base_gluc_size_box, 
  #            device = NULL, 
  #            path = here('output/'), scale = 1, width = 8, 
  #            height = 6, 
  #            units = c('in'), dpi = 300, limitsize = TRUE) 
  #     
    # ## d) Mid-development baseline blood glucose by mid-development
    #   # mites (y/n) boxplot
    #   mid_base_gluc_mite_box <- nestling_parent_care %>%
    #     filter(sample.state == 'mid') %>%
    #     ggplot(aes(x = mite.bin, y = base.gluc, fill = mite.bin)) + 
    #     geom_boxplot() + 
    #     #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    #     #geom_jitter(color="black", size=2, alpha=0.9) +
    #     scale_color_manual(values=c('purple', 'dark green')) +
    #     labs(title = 'Mid-development baseline blood glucose by 
    #          mid-development mites (y/n)',
    #          y ='Baseline glucose (mg/dl)') +
    #     theme(plot.title = element_text(hjust = 0.5)) + # center title
    #     theme(axis.ticks = element_blank()) + # remove axis ticks
    #     theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    #     # remove background color
    #     theme(panel.background = element_rect(fill = 'white')) +
    #     # customize legend
    #     theme(legend.title=element_text(size=10),
    #           legend.text=element_text(size=8),
    #           legend.position = c(0.91, 0.94))
    #   
    # ## e) Print plot 
    #   print(mid_base_gluc_mite_box)
    #   
    # ## f) Save plot
    #   ggsave('mid_base_gluc_mite_box.pdf', plot = mid_base_gluc_mite_box, 
    #          device = NULL, 
    #          path = here('output/'), scale = 1, width = 8, 
    #          height = 6, 
    #          units = c('in'), dpi = 300, limitsize = TRUE)   
    #   
    # ## g) Mid-development baseline blood glucose by mid-development nest age
    #   mid_base_gluc_age_scatter <- nestling_parent_care %>%
    #     filter(sample.state == 'mid') %>%
    #     ggplot(aes(x = nestling.age, y = base.gluc)) + 
    #     geom_point() + 
    #     labs(title = 'Mid-development baseline blood glucose by 
    #          mid-development nest age',
    #          y ='Baseline glucose (mg/dl)') +
    #     theme(plot.title = element_text(hjust = 0.5)) + # center title
    #     theme(axis.ticks = element_blank()) + # remove axis ticks
    #     theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    #     # remove background color
    #     theme(panel.background = element_rect(fill = 'white')) +
    #     # customize legend
    #     theme(legend.title=element_text(size=10),
    #           legend.text=element_text(size=8),
    #           legend.position = c(0.91, 0.94))
    #   
    # ## h) Print plot 
    #   print(mid_base_gluc_age_scatter)
    #   
    # ## i) Save plot
    #   ggsave('mid_base_gluc_age_scatter.pdf', plot = mid_base_gluc_age_scatter, 
    #          device = NULL, 
    #          path = here('output/'), scale = 1, width = 8, 
    #          height = 6, 
    #          units = c('in'), dpi = 300, limitsize = TRUE)
    #   
    # ## j) Mid-development baseline blood glucose by mid-development baseline
    #   # blood sample time (seconds)
    #   mid_base_gluc_blood_scatter <- nestling_parent_care %>%
    #     filter(sample.state == 'mid') %>%
    #     ggplot(aes(x = base.gluc.s, y = base.gluc)) + 
    #     geom_point() +
    #     geom_smooth(method = "loess", size = 1.5) +
    #     labs(title = 'Mid-development baseline blood glucose by 
    #          mid-development baseline blood sample time (s)',
    #          y ='Baseline glucose (mg/dl)') +
    #     theme(plot.title = element_text(hjust = 0.5)) + # center title
    #     theme(axis.ticks = element_blank()) + # remove axis ticks
    #     theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    #     # remove background color
    #     theme(panel.background = element_rect(fill = 'white')) +
    #     # customize legend
    #     theme(legend.title=element_text(size=10),
    #           legend.text=element_text(size=8),
    #           legend.position = c(0.91, 0.94))
    #   
    # ## k) Print plot 
    #   print(mid_base_gluc_blood_scatter)
    #   
    # ## l) Save plot
    #   ggsave('mid_base_gluc_blood_scatter.pdf', 
    #          plot = mid_base_gluc_blood_scatter, 
    #          device = NULL, 
    #          path = here('output/'), scale = 1, width = 8, 
    #          height = 6, 
    #          units = c('in'), dpi = 300, limitsize = TRUE) 
    #   
    # ## m) Mid-development baseline glucose by mid-development 
    #   # temperature (C) at around sunrise (observation trial start time)
    #   mid_base_gluc_temp_scatter <- nestling_parent_care %>%
    #     filter(sample.state == 'mid') %>%
    #     ggplot(aes(x = trial.temp, y = base.gluc)) + 
    #     geom_point() +
    #     geom_smooth(method = "loess", size = 1.5) +
    #     labs(title = 'Mid-development baseline blood glucose by 
    #          mid-development trial temperature (C)',
    #          y ='Baseline glucose (mg/dl)') +
    #     theme(plot.title = element_text(hjust = 0.5)) + # center title
    #     theme(axis.ticks = element_blank()) + # remove axis ticks
    #     theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    #     # remove background color
    #     theme(panel.background = element_rect(fill = 'white')) +
    #     # customize legend
    #     theme(legend.title=element_text(size=10),
    #           legend.text=element_text(size=8),
    #           legend.position = c(0.91, 0.94))
    #   
    # ## n) Print plot 
    #   print(mid_base_gluc_temp_scatter)
    #   
    # ## o) Save plot
    #   ggsave('mid_base_gluc_temp_scatter.pdf', 
    #          plot = mid_base_gluc_temp_scatter, 
    #          device = NULL, 
    #          path = here('output/'), scale = 1, width = 8, 
    #          height = 6, 
    #          units = c('in'), dpi = 300, limitsize = TRUE)
    #   
    # ## p) Mid-development baseline blood glucose by mid-development 
    #   # number of nestlings in the nest
    #   mid_base_gluc_nos_nest_scatter <- nestling_parent_care %>%
    #     filter(sample.state == 'mid') %>%
    #     ggplot(aes(x = nestling.number, y = base.gluc)) + 
    #     geom_point() +
    #     geom_smooth(method = "loess", size = 1.5) +
    #     labs(title = 'Mid-development baseline blood glucose by 
    #          mid-development number of nestlings',
    #          y ='Diff. glucose (mg/dl)') +
    #     theme(plot.title = element_text(hjust = 0.5)) + # center title
    #     theme(axis.ticks = element_blank()) + # remove axis ticks
    #     theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    #     # remove background color
    #     theme(panel.background = element_rect(fill = 'white')) +
    #     # customize legend
    #     theme(legend.title=element_text(size=10),
    #           legend.text=element_text(size=8),
    #           legend.position = c(0.91, 0.94))
    #   
    # ## q) Print plot 
    #   print(mid_base_gluc_nos_nest_scatter)
    #   
    # ## r) Save plot
    #   ggsave('mid_base_gluc_nos_nest_scatter.pdf', 
    #          plot = mid_base_gluc_nos_nest_scatter, 
    #          device = NULL, 
    #          path = here('output/'), scale = 1, width = 8, 
    #          height = 6, 
    #          units = c('in'), dpi = 300, limitsize = TRUE)
    #   
    #   
  # ### 5.4 Bivariate graphs for late development (~day 12) blood glucose diffs.
  #   ## a) Late development difference blood glucose by late development
  #     # relative nestling size boxplot
  #     late_gluc_late_size_box <- nestling_parent_care %>%
  #       filter(sample.state == 'late') %>%
  #       ggplot(aes(x = rt.wing.length, y = gluc.diff, fill = size.order)) +
  #       geom_boxplot() +
  #       scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  #       geom_jitter(color="black", size=2, alpha=0.9) +
  #       # scale_color_manual(values=c('#69b3a2', '#404080'),
  #       #                    name = 'smallest vs other nestlings',
  #       #                    labels = c('small', 'other')) +
  #       labs(title = 'Late development difference in blood glucose (stressed - baseline) by
  #            late development relative size',
  #            y ='Diff. glucose (mg/dl)') +
  #       theme(plot.title = element_text(hjust = 0.5)) + # center title
  #       theme(axis.ticks = element_blank()) + # remove axis ticks
  #       theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
  #       # remove background color
  #       theme(panel.background = element_rect(fill = 'white')) +
  #       # customize legend
  #       theme(legend.title=element_text(size=10),
  #             legend.text=element_text(size=8),
  #             legend.position = c(0.91, 0.94))
  # 
  #   ## b) Print plot
  #     print(late_gluc_late_size_box)
  # 
  #   ## c) Save plot
  #     ggsave('late_gluc_late_size_box.pdf', plot = late_gluc_late_size_box,
  #            device = NULL,
  #            path = here('output/'), scale = 1, width = 8,
  #            height = 6,
  #            units = c('in'), dpi = 300, limitsize = TRUE)
    #   
    # ## d) Late development difference blood glucose by late development 
    #   # first blood sample time (seconds)
    #   late_gluc_late_base_blood_scatter <- nestling_parent_care %>%
    #     filter(sample.state == 'late') %>%
    #     ggplot(aes(x = base.gluc.s, y = gluc.diff)) + 
    #     geom_point() +
    #     geom_smooth(method = "loess", size = 1.5) +
    #     labs(title = 'Late development difference in blood glucose (stressed - baseline) by 
    #          late development baseline blood sample time (s)',
    #          y ='Diff. glucose (mg/dl)') +
    #     theme(plot.title = element_text(hjust = 0.5)) + # center title
    #     theme(axis.ticks = element_blank()) + # remove axis ticks
    #     theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    #     # remove background color
    #     theme(panel.background = element_rect(fill = 'white')) +
    #     # customize legend
    #     theme(legend.title=element_text(size=10),
    #           legend.text=element_text(size=8),
    #           legend.position = c(0.91, 0.94))
    #   
    # ## e) Print plot 
    #   print(late_gluc_late_base_blood_scatter)
    #   
    # ## f) Save plot
    #   ggsave('late_gluc_late_base_blood_scatter.pdf', 
    #          plot = late_gluc_late_base_blood_scatter, 
    #          device = NULL, 
    #          path = here('output/'), scale = 1, width = 8, 
    #          height = 6, 
    #          units = c('in'), dpi = 300, limitsize = TRUE)
    #   
    # ## g) Late development difference blood glucose by late development 
    #   # mites (y/n) boxplot
    #   late_gluc_late_mite_box <- nestling_parent_care %>%
    #     filter(sample.state == 'late') %>%
    #     ggplot(aes(x = mite.bin, y = gluc.diff, fill = mite.bin)) + 
    #     geom_boxplot() + 
    #     #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    #     #geom_jitter(color="black", size=2, alpha=0.9) +
    #     scale_color_manual(values=c('purple', 'dark green')) +
    #     labs(title = 'Late development difference in blood glucose (stressed - baseline) by 
    #          late development mite (y/n)',
    #          y ='Diff. glucose (mg/dl)') +
    #     theme(plot.title = element_text(hjust = 0.5)) + # center title
    #     theme(axis.ticks = element_blank()) + # remove axis ticks
    #     theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    #     # remove background color
    #     theme(panel.background = element_rect(fill = 'white')) +
    #     # customize legend
    #     theme(legend.title=element_text(size=10),
    #           legend.text=element_text(size=8),
    #           legend.position = c(0.91, 0.94))
    #   
    # ## h) Print plot 
    #   print(late_gluc_late_mite_box)
    #   
    # ## i) Save plot
    #   ggsave('late_gluc_late_mite_box.pdf', plot = late_gluc_late_mite_box, 
    #          device = NULL, 
    #          path = here('output/'), scale = 1, width = 8, 
    #          height = 6, 
    #          units = c('in'), dpi = 300, limitsize = TRUE)   
    #   
    # ## j) Late development difference blood glucose by late development
    #   # right wing length
    #   late_gluc_late_wing_scatter <- late_nestling_size %>%
    #     ggplot(aes(x = late.rt.wing.length, y = gluc.diff)) + 
    #     geom_point() +
    #     geom_smooth(method = "loess", size = 1.5) +
    #     labs(title = 'Late development difference in blood glucose (stressed - baseline) by 
    #          late development right wing length ',
    #          y ='Diff. glucose (mg/dl)') +
    #     theme(plot.title = element_text(hjust = 0.5)) + # center title
    #     theme(axis.ticks = element_blank()) + # remove axis ticks
    #     theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    #     # remove background color
    #     theme(panel.background = element_rect(fill = 'white')) +
    #     # customize legend
    #     theme(legend.title=element_text(size=10),
    #           legend.text=element_text(size=8),
    #           legend.position = c(0.91, 0.94))
    #   
    # ## k) Print plot 
    #   print(late_gluc_late_wing_scatter)
    #   
    # ## l) Save plot
    #   ggsave('late_gluc_late_wing_scatter.pdf', 
    #          plot = late_gluc_late_wing_scatter, 
    #          device = NULL, 
    #          path = here('output/'), scale = 1, width = 8, 
    #          height = 6, 
    #          units = c('in'), dpi = 300, limitsize = TRUE) 
    #   
    # ## m) Late development difference blood glucose by mid-development 
    #   # right wing length
    #   late_gluc_mid_wing_scatter <- late_nestling_size %>%
    #     ggplot(aes(x = mid.rt.wing.length, y = gluc.diff)) + 
    #     geom_point() +
    #     geom_smooth(method = "loess", size = 1.5) +
    #     labs(title = 'Late development difference in blood glucose (stressed - baseline) by 
    #          mid-development right wing length',
    #          y ='Diff. glucose (mg/dl)') +
    #     theme(plot.title = element_text(hjust = 0.5)) + # center title
    #     theme(axis.ticks = element_blank()) + # remove axis ticks
    #     theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    #     # remove background color
    #     theme(panel.background = element_rect(fill = 'white')) +
    #     # customize legend
    #     theme(legend.title=element_text(size=10),
    #           legend.text=element_text(size=8),
    #           legend.position = c(0.91, 0.94))
    #   
    # ## n) Print plot 
    #   print(late_gluc_mid_wing_scatter)
    #   
    # ## o) Save plot
    #   ggsave('late_gluc_mid_wing_scatter.pdf', 
    #          plot = late_gluc_mid_wing_scatter, 
    #          device = NULL, 
    #          path = here('output/'), scale = 1, width = 8, 
    #          height = 6, 
    #          units = c('in'), dpi = 300, limitsize = TRUE)
    #   
    # ## p) Late development difference blood glucose by difference (late - mid)  
    #   # right wing length
    #   late_gluc_wing_diff_scatter <- late_nestling_size %>%
    #     ggplot(aes(x = rt.wing.diff, y = gluc.diff)) + 
    #     geom_point() +
    #     geom_smooth(method = "loess", size = 1.5) +
    #     labs(title = 'Late development difference in blood glucose (stressed - baseline) by 
    #          difference (late - mid) in right wing length',
    #          y ='Diff. glucose (mg/dl)') +
    #     theme(plot.title = element_text(hjust = 0.5)) + # center title
    #     theme(axis.ticks = element_blank()) + # remove axis ticks
    #     theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    #     # remove background color
    #     theme(panel.background = element_rect(fill = 'white')) +
    #     # customize legend
    #     theme(legend.title=element_text(size=10),
    #           legend.text=element_text(size=8),
    #           legend.position = c(0.91, 0.94))
    #   
    # ## q) Print plot 
    #   print(late_gluc_wing_diff_scatter)
    #   
    # ## r) Save plot
    #   ggsave('late_gluc_wing_diff_scatter.pdf', 
    #          plot = late_gluc_wing_diff_scatter, 
    #          device = NULL, 
    #          path = here('output/'), scale = 1, width = 8, 
    #          height = 6, 
    #          units = c('in'), dpi = 300, limitsize = TRUE) 
    #   
    # ## s) Late development difference blood glucose by late 
    #   # development nest age
    #   late_gluc_late_age_scatter <- nestling_parent_care %>%
    #     filter(sample.state == 'late') %>%
    #     ggplot(aes(x = nestling.age, y = gluc.diff)) + 
    #     geom_point() + 
    #     labs(title = 'Late development difference in blood glucose (stressed - baseline) by 
    #          late development nest age',
    #          y ='Diff. glucose (mg/dl)') +
    #     theme(plot.title = element_text(hjust = 0.5)) + # center title
    #     theme(axis.ticks = element_blank()) + # remove axis ticks
    #     theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    #     # remove background color
    #     theme(panel.background = element_rect(fill = 'white')) +
    #     # customize legend
    #     theme(legend.title=element_text(size=10),
    #           legend.text=element_text(size=8),
    #           legend.position = c(0.91, 0.94))
    #   
    # ## t) Print plot 
    #   print(late_gluc_late_age_scatter)
    #   
    # ## u) Save plot
    #   ggsave('late_gluc_late_age_scatter.pdf', 
    #          plot = late_gluc_late_age_scatter, 
    #          device = NULL, 
    #          path = here('output/'), scale = 1, width = 8, 
    #          height = 6, 
    #          units = c('in'), dpi = 300, limitsize = TRUE)
    #   
    # ## v) Late development difference in glucose (stress - baseline) by 
    #   # late development temperature (C) at around sunrise (obs start time)
    #   late_gluc_late_temp_scatter <-  nestling_parent_care %>%
    #     filter(sample.state == 'late') %>%
    #     ggplot(aes(x = trial.temp, y = gluc.diff)) + 
    #     geom_point() +
    #     geom_smooth(method = "loess", size = 1.5) +
    #     labs(title = 'Late development difference in blood glucose (stress - baseline) by 
    #          late development trial temperature (C)',
    #          y ='Diff. glucose (mg/dl)') +
    #     theme(plot.title = element_text(hjust = 0.5)) + # center title
    #     theme(axis.ticks = element_blank()) + # remove axis ticks
    #     theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    #     # remove background color
    #     theme(panel.background = element_rect(fill = 'white')) +
    #     # customize legend
    #     theme(legend.title=element_text(size=10),
    #           legend.text=element_text(size=8),
    #           legend.position = c(0.91, 0.94))
    #   
    # ## w) Print plot 
    #   print(late_gluc_late_temp_scatter)
    #   
    # ## x) Save plot
    #   ggsave('late_gluc_late_temp_scatter.pdf', 
    #          plot = late_gluc_late_temp_scatter, 
    #          device = NULL, 
    #          path = here('output/'), scale = 1, width = 8, 
    #          height = 6, 
    #          units = c('in'), dpi = 300, limitsize = TRUE)
    #   
    # ## y) Late development difference in blood glucose by late development 
    #   # number of nestlings in the nest
    #   late_gluc_nos_nest_scatter <- nestling_parent_care %>%
    #     filter(sample.state == 'late') %>%
    #     ggplot(aes(x = nestling.number, y = gluc.diff)) + 
    #     geom_point() +
    #     geom_smooth(method = "loess", size = 1.5) +
    #     labs(title = 'Late development difference in blood glucose (stress-baseline) by
    #          late development number of nestlings',
    #          y ='Diff. glucose (mg/dl)') +
    #     theme(plot.title = element_text(hjust = 0.5)) + # center title
    #     theme(axis.ticks = element_blank()) + # remove axis ticks
    #     theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    #     # remove background color
    #     theme(panel.background = element_rect(fill = 'white')) +
    #     # customize legend
    #     theme(legend.title=element_text(size=10),
    #           legend.text=element_text(size=8),
    #           legend.position = c(0.91, 0.94))
    #   
    # ## z) Print plot 
    #   print(late_gluc_nos_nest_scatter)
    #   
    # ## a) Save plot
    #   ggsave('late_gluc_nos_nest_scatter.pdf', 
    #          plot = late_gluc_nos_nest_scatter, 
    #          device = NULL, 
    #          path = here('output/'), scale = 1, width = 8, 
    #          height = 6, 
    #          units = c('in'), dpi = 300, limitsize = TRUE)
    #   
    #   
  # ### 5.5 Bivariate graphs for late development (~day 12) blood glucose baseline
  #   ## a) Late development baseline blood glucose by late development
  #     # relative nestling size boxplot
  #     late_base_gluc_late_indx_box <- nestling_parent_care %>%
  #       filter(sample.state == 'late') %>%
  #       ggplot(aes(x = rt.wing.length, y = base.gluc, fill = size.order)) +
  #       geom_boxplot() +
  #       scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  #       geom_jitter(color="black", size=2, alpha=0.9) +
  #       # scale_color_manual(values=c('#69b3a2', '#404080'),
  #       #                    name = 'smallest vs other nestlings',
  #       #                    labels = c('small', 'other')) +
  #       labs(title = 'Late development baseline glucose by
  #            late development relative size',
  #            y ='Baseline glucose (mg/dl)') +
  #       theme(plot.title = element_text(hjust = 0.5)) + # center title
  #       theme(axis.ticks = element_blank()) + # remove axis ticks
  #       theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
  #       # remove background color
  #       theme(panel.background = element_rect(fill = 'white')) +
  #       # customize legend
  #       theme(legend.title=element_text(size=10),
  #             legend.text=element_text(size=8),
  #             legend.position = c(0.91, 0.94))
  # 
  #   ## b) Print plot
  #     print(late_base_gluc_late_indx_box)
  # 
  #   ## c) Save plot
  #     ggsave('llate_base_gluc_late_indx_box.pdf',
  #            plot = late_base_gluc_late_indx_box,
  #            device = NULL,
  #            path = here('output/'), scale = 1, width = 8,
  #            height = 6,
  #            units = c('in'), dpi = 300, limitsize = TRUE)
  #     
    # ## d) Late development baseline blood glucose by late development 
    #   # first blood sample time (seconds)
    #   late_base_gluc_late_base_time_scatter <- nestling_parent_care %>%
    #     filter(sample.state == 'late') %>%
    #     ggplot(aes(x = base.gluc.s, y = base.gluc)) + 
    #     geom_point() +
    #     geom_smooth(method = "loess", size = 1.5) +
    #     labs(title = 'Late development baseline blood glucose by baseline blood sample time (s)',
    #          y ='Baseline glucose (mg/dl)') +
    #     theme(plot.title = element_text(hjust = 0.5)) + # center title
    #     theme(axis.ticks = element_blank()) + # remove axis ticks
    #     theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    #     # remove background color
    #     theme(panel.background = element_rect(fill = 'white')) +
    #     # customize legend
    #     theme(legend.title=element_text(size=10),
    #           legend.text=element_text(size=8),
    #           legend.position = c(0.91, 0.94))
    #   
    # ## e) Print plot 
    #   print(late_base_gluc_late_base_time_scatter)
    #   
    # ## f) Save plot
    #   ggsave('late_base_gluc_late_base_time_scatter.pdf', 
    #          plot = late_base_gluc_late_base_time_scatter, 
    #          device = NULL, 
    #          path = here('output/'), scale = 1, width = 8, 
    #          height = 6, 
    #          units = c('in'), dpi = 300, limitsize = TRUE)
    #   
    # ## g) Late development baseline blood glucose by late development 
    #   # mites (y/n) boxplot
    #   late_base_gluc_late_mite_box <- nestling_parent_care %>%
    #     filter(sample.state == 'late') %>%
    #     ggplot(aes(x = mite.bin, y = base.gluc, fill = mite.bin)) + 
    #     geom_boxplot() + 
    #     #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    #     #geom_jitter(color="black", size=2, alpha=0.9) +
    #     scale_color_manual(values=c('purple', 'dark green')) +
    #     labs(title = 'Late development baseline blood glucose by late development mite (y/n)',
    #          y ='Baseline glucose (mg/dl)') +
    #     theme(plot.title = element_text(hjust = 0.5)) + # center title
    #     theme(axis.ticks = element_blank()) + # remove axis ticks
    #     theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    #     # remove background color
    #     theme(panel.background = element_rect(fill = 'white')) +
    #     # customize legend
    #     theme(legend.title=element_text(size=10),
    #           legend.text=element_text(size=8),
    #           legend.position = c(0.91, 0.94))
    #   
    # ## h) Print plot 
    #   print(late_base_gluc_late_mite_box)
    #   
    # ## i) Save plot
    #   ggsave('late_base_gluc_late_mite_box.pdf', plot = late_base_gluc_late_mite_box, 
    #          device = NULL, 
    #          path = here('output/'), scale = 1, width = 8, 
    #          height = 6, 
    #          units = c('in'), dpi = 300, limitsize = TRUE)   
    #   
    # ## j) Late development baseline blood glucose by late development
    #   # right wing length
    #   late_base_gluc_late_wing_scatter <- late_nestling_size %>%
    #     ggplot(aes(x = late.rt.wing.length, y = base.gluc)) + 
    #     geom_point() +
    #     geom_smooth(method = "loess", size = 1.5) +
    #     labs(title = 'Late development baseline blood glucose by 
    #          late development right wing length',
    #          y ='Baseline glucose (mg/dl)') +
    #     theme(plot.title = element_text(hjust = 0.5)) + # center title
    #     theme(axis.ticks = element_blank()) + # remove axis ticks
    #     theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    #     # remove background color
    #     theme(panel.background = element_rect(fill = 'white')) +
    #     # customize legend
    #     theme(legend.title=element_text(size=10),
    #           legend.text=element_text(size=8),
    #           legend.position = c(0.91, 0.94))
    #   
    # ## k) Print plot 
    #   print(late_base_gluc_late_wing_scatter)
    #   
    # ## l) Save plot
    #   ggsave('late_base_gluc_late_wing_scatter.pdf', 
    #          plot = late_base_gluc_late_wing_scatter, 
    #          device = NULL, 
    #          path = here('output/'), scale = 1, width = 8, 
    #          height = 6, 
    #          units = c('in'), dpi = 300, limitsize = TRUE) 
    #   
    # ## m) Late development baseline blood glucose by mid-development 
    #   # right wing length
    #   late_base_gluc_mid_wing_scatter <- late_nestling_size %>%
    #     ggplot(aes(x = mid.rt.wing.length, y = base.gluc)) + 
    #     geom_point() +
    #     geom_smooth(method = "loess", size = 1.5) +
    #     labs(title = 'Late development baseline blood glucose by 
    #          mid-development right wing length',
    #          y ='Baseline glucose (mg/dl)') +
    #     theme(plot.title = element_text(hjust = 0.5)) + # center title
    #     theme(axis.ticks = element_blank()) + # remove axis ticks
    #     theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    #     # remove background color
    #     theme(panel.background = element_rect(fill = 'white')) +
    #     # customize legend
    #     theme(legend.title=element_text(size=10),
    #           legend.text=element_text(size=8),
    #           legend.position = c(0.91, 0.94))
    #   
    # ## n) Print plot 
    #   print(late_base_gluc_mid_wing_scatter)
    #   
    # ## o) Save plot
    #   ggsave('late_base_gluc_mid_wing_scatter.pdf', 
    #          plot = late_base_gluc_mid_wing_scatter, 
    #          device = NULL, 
    #          path = here('output/'), scale = 1, width = 8, 
    #          height = 6, 
    #          units = c('in'), dpi = 300, limitsize = TRUE)
    #   
    # ## p) Late development baseline blood glucose by difference (late - mid)  
    #   # right wing length
    #   late_base_gluc_wing_diff_scatter <- late_nestling_size %>%
    #     ggplot(aes(x = rt.wing.diff, y = base.gluc)) + 
    #     geom_point() +
    #     geom_smooth(method = "loess", size = 1.5) +
    #     labs(title = 'Late development baseline blood glucose by 
    #          difference (late - mid) right wing lenght',
    #          y ='Baseline glucose (mg/dl)') +
    #     theme(plot.title = element_text(hjust = 0.5)) + # center title
    #     theme(axis.ticks = element_blank()) + # remove axis ticks
    #     theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    #     # remove background color
    #     theme(panel.background = element_rect(fill = 'white')) +
    #     # customize legend
    #     theme(legend.title=element_text(size=10),
    #           legend.text=element_text(size=8),
    #           legend.position = c(0.91, 0.94))
    #   
    # ## q) Print plot 
    #   print(late_base_gluc_wing_diff_scatter)
    #   
    # ## r) Save plot
    #   ggsave('late_base_gluc_wing_diff_scatter.pdf', 
    #          plot = late_base_gluc_wing_diff_scatter, 
    #          device = NULL, 
    #          path = here('output/'), scale = 1, width = 8, 
    #          height = 6, 
    #          units = c('in'), dpi = 300, limitsize = TRUE) 
    #   
    # ## s) Late development baseline blood glucose by late development nest age
    #   late_base_gluc_late_age_scatter <- nestling_parent_care %>%
    #     filter(sample.state == 'late') %>%
    #     ggplot(aes(x = nestling.age, y = base.gluc)) + 
    #     geom_point() + 
    #     labs(title = 'Late development baseline blood glucose by 
    #          late development nest age',
    #          y ='Baseline glucose (mg/dl)') +
    #     theme(plot.title = element_text(hjust = 0.5)) + # center title
    #     theme(axis.ticks = element_blank()) + # remove axis ticks
    #     theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    #     # remove background color
    #     theme(panel.background = element_rect(fill = 'white')) +
    #     # customize legend
    #     theme(legend.title=element_text(size=10),
    #           legend.text=element_text(size=8),
    #           legend.position = c(0.91, 0.94))
    #   
    # ## t) Print plot 
    #   print(late_base_gluc_late_age_scatter)
    #   
    # ## u) Save plot
    #   ggsave('late_base_gluc_late_age_scatter.pdf', 
    #          plot = late_base_gluc_late_age_scatter, 
    #          device = NULL, 
    #          path = here('output/'), scale = 1, width = 8, 
    #          height = 6, 
    #          units = c('in'), dpi = 300, limitsize = TRUE)
    #   
    # ## v) Late development baseline glucose by mid-development 
    #   # temperature (C) at around sunrise (observation trial start time)
    #   late_base_gluc_late_temp_scatter <-  nestling_parent_care %>%
    #     filter(sample.state == 'late') %>%
    #     ggplot(aes(x = trial.temp, y = base.gluc)) + 
    #     geom_point() +
    #     geom_smooth(method = "loess", size = 1.5) +
    #     labs(title = 'Late development baseline blood glucose by 
    #          late development trial temperature (C)',
    #          y ='Baseline glucose (mg/dl)') +
    #     theme(plot.title = element_text(hjust = 0.5)) + # center title
    #     theme(axis.ticks = element_blank()) + # remove axis ticks
    #     theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    #     # remove background color
    #     theme(panel.background = element_rect(fill = 'white')) +
    #     # customize legend
    #     theme(legend.title=element_text(size=10),
    #           legend.text=element_text(size=8),
    #           legend.position = c(0.91, 0.94))
    #   
    # ## w) Print plot 
    #   print(late_base_gluc_late_temp_scatter)
    #   
    # ## x) Save plot
    #   ggsave('late_base_gluc_late_temp_scatter.pdf', 
    #          plot = late_base_gluc_late_temp_scatter, 
    #          device = NULL, 
    #          path = here('output/'), scale = 1, width = 8, 
    #          height = 6, 
    #          units = c('in'), dpi = 300, limitsize = TRUE)
    #   
    # ## y) Late development baseline glucose by late development 
    #   # number of nestlings in the nest
    #   late_base_gluc_nos_nest_scatter <- nestling_parent_care %>%
    #     filter(sample.state == 'late') %>%
    #     ggplot(aes(x = nestling.number, y = base.gluc)) + 
    #     geom_point() +
    #     geom_smooth(method = "loess", size = 1.5) +
    #     labs(title = 'Late development baseline glucoces by
    #          late development number of nestlings',
    #          y ='Diff. glucose (mg/dl)') +
    #     theme(plot.title = element_text(hjust = 0.5)) + # center title
    #     theme(axis.ticks = element_blank()) + # remove axis ticks
    #     theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    #     # remove background color
    #     theme(panel.background = element_rect(fill = 'white')) +
    #     # customize legend
    #     theme(legend.title=element_text(size=10),
    #           legend.text=element_text(size=8),
    #           legend.position = c(0.91, 0.94))
    #   
    # ## z) Print plot 
    #   print(late_base_gluc_nos_nest_scatter)
    #   
    # ## aa) Save plot
    #   ggsave('late_base_gluc_nos_nest_scatter.pdf', 
    #          plot = late_base_gluc_nos_nest_scatter, 
    #          device = NULL, 
    #          path = here('output/'), scale = 1, width = 8, 
    #          height = 6, 
    #          units = c('in'), dpi = 300, limitsize = TRUE)
    #   
      
  
###############################################################################
##############                6. Covariate models                ##############
###############################################################################      
            
  ### 6.1 Simple associations mid-development (covariates with glucose)
      
    # Note: Covariates may include potential confounders or precision variables
      
    ## a) Mid-development difference in blood glucose by number of nestlings
      # in the nest
      mid.gluc.nest.nos.lm <- lme(gluc.diff ~ nestling.number, 
                                 random = ~1|nest.id,
                                 data = subset(nestling_parent_care, 
                                               sample.state == 'mid'&
                                                 !is.na(x = gluc.diff) &
                                                 !is.na(x = nestling.number)))  
      
      summary(mid.gluc.nest.nos.lm)    # model summary 
      intervals(mid.gluc.nest.nos.lm)    # 95% CIs
      #plot(mid.gluc.nest.nos.lm)       # check residuals  
      
    ## b) Mid-development baseline blood glucose by number of nestlings
      # in the nest
      mid.base.gluc.nest.nos.lm <- lme(base.gluc ~ nestling.number,
                                      random = ~1|nest.id,
                                      data = subset(nestling_parent_care, 
                                                sample.state == 'mid'&
                                                  !is.na(x = base.gluc) &
                                                  !is.na(x = nestling.number))) 
      
      summary(mid.base.gluc.nest.nos.lm)    # model summary 
      intervals(mid.base.gluc.nest.nos.lm)    # 95% CIs
      #plot(mid.base.gluc.nest.nos.lm)       # check residuals  
      
    ## c) Mid-development difference in blood glucose by mid-dev. mites
      mid.gluc.mite.lm <- lme(gluc.diff ~ mite.bin, 
                               random = ~1|nest.id,
                               data = subset(nestling_parent_care, 
                                             sample.state == 'mid'&
                                               !is.na(x = gluc.diff)))
      
      summary(mid.gluc.mite.lm)    # model summary 
      intervals(mid.gluc.mite.lm)    # 95% CIs
      #plot(mid.gluc.mite.lm)       # check residuals  
      
    ## d) Mid-development baseline blood glucose by mid-dev. mites
      mid.base.gluc.mite.lm <- lme(base.gluc ~ mite.bin,
                                    random = ~1|nest.id,
                                    data = subset(nestling_parent_care, 
                                                  sample.state == 'mid'&
                                                    !is.na(x = base.gluc))) 
      
      summary(mid.base.gluc.mite.lm)    # model summary 
      intervals(mid.base.gluc.mite.lm)    # 95% CIs
      #plot(mid.base.gluc.mite.lm)       # check residuals  
      
    ## e) Mid-development difference in blood glucose by mid-dev. nest age
      mid.gluc.age.lm <- lme(gluc.diff ~ nestling.age, 
                              random = ~1|nest.id,
                              data = subset(nestling_parent_care, 
                                            sample.state == 'mid'&
                                              !is.na(x = gluc.diff))) 
      
      
      summary(mid.gluc.age.lm)    # model summary 
      intervals(mid.gluc.age.lm, which = 'fixed')    # 95% CIs
      #plot(mid.gluc.age.lm)       # check residuals  
      
    ## f) Mid-development baseline blood glucose by mid-dev. nest age
      mid.base.gluc.age.lm <- lme(base.gluc ~ nestling.age,
                                   random = ~1|nest.id,
                                   data = subset(nestling_parent_care, 
                                                 sample.state == 'mid'&
                                                   !is.na(x = base.gluc))) 
      
      
      summary(mid.base.gluc.age.lm)    # model summary 
      intervals(mid.base.gluc.age.lm)    # 95% CIs
      #plot(mid.base.gluc.age.lm)       # check residuals  
      
    ## g) Mid-development difference in blood glucose by baseline sampling time
      mid.gluc.base.s.lm <- lme(gluc.diff ~ base.gluc.s, 
                                random = ~1|nest.id,
                                data = subset(nestling_parent_care, 
                                              sample.state == 'mid'&
                                                !is.na(x = gluc.diff)))  
      
      summary(mid.gluc.base.s.lm)    # model summary 
      intervals(mid.gluc.base.s.lm)    # 95% CIs
      #plot(mid.gluc.base.s.lm)       # check residuals  
      
    ## h) Mid-development baseline blood glucose by baseline sampling time
      mid.base.gluc.base.s.lm <- lme(base.gluc ~ base.gluc.s,
                                     random = ~1|nest.id,
                                     data = subset(nestling_parent_care, 
                                                   sample.state == 'mid'&
                                                     !is.na(x = base.gluc))) 
      
      summary(mid.base.gluc.base.s.lm)    # model summary 
      intervals(mid.base.gluc.base.s.lm)    # 95% CIs
      #plot(mid.base.gluc.base.s.lm)       # check residuals    
 
    ## i) Mid-development difference in blood glucose by mid-dev. trial temp.
      mid.gluc.temp.lm <- lme(gluc.diff ~ trial.temp, 
                               random = ~1|nest.id,
                               data = subset(nestling_parent_care, 
                                             sample.state == 'mid'&
                                               !is.na(x = gluc.diff)&
                                               !is.na(x = trial.temp)))
      
      
      summary(mid.gluc.temp.lm)    # model summary 
      intervals(mid.gluc.temp.lm)    # 95% CIs
      #plot(mid.gluc.temp.lm)       # check residuals  
      
    ## j) Mid-development baseline blood glucose by mid-dev. trial temp.
      mid.base.gluc.temp.lm <- lme(base.gluc ~ trial.temp,
                                    random = ~1|nest.id,
                                    data = subset(nestling_parent_care, 
                                                  sample.state == 'mid'&
                                                    !is.na(x = base.gluc) &
                                                    !is.na(x = trial.temp)))  
      
      summary(mid.base.gluc.temp.lm)    # model summary 
      intervals(mid.base.gluc.temp.lm)    # 95% CIs
      #plot(mid.base.gluc.temp.lm)       # check residuals  
   
  
  ### 6.2 Simple associations late development (covariates with glucose)
      
    # Note: Covariates may include potential confounders or precision variables
      
    ## a) Late development difference in blood glucose by baseline sampling time
      late.gluc.nest.nos.lm <- lme(gluc.diff ~ nestling.number, 
                                random = ~1|nest.id,
                                data = subset(nestling_parent_care, 
                                              sample.state == 'late'&
                                                !is.na(x = gluc.diff) &
                                                !is.na(x = nestling.number)))  
      
      summary(late.gluc.nest.nos.lm)    # model summary 
      intervals(late.gluc.nest.nos.lm)    # 95% CIs
      #plot(late.gluc.nest.nos.lm)       # check residuals  
      
    ## b) Late development baseline blood glucose by baseline sampling time
      late.base.gluc.nest.nos.lm <- lme(base.gluc ~ nestling.number,
                                     random = ~1|nest.id,
                                     data = subset(nestling_parent_care, 
                                                sample.state == 'late'&
                                                  !is.na(x = base.gluc)&
                                                  !is.na(x = nestling.number))) 
      
      summary(late.base.gluc.nest.nos.lm)    # model summary 
      intervals(late.base.gluc.nest.nos.lm)    # 95% CIs
      #plot(late.base.gluc.nest.nos.lm)       # check residuals  
      
    ## c) Late development difference in blood glucose by late dev. mites
      late.gluc.mite.lm <- lme(gluc.diff ~ mite.bin, 
                              random = ~1|nest.id,
                              data = subset(nestling_parent_care, 
                                            sample.state == 'late'&
                                              !is.na(x = gluc.diff)))
      
      summary(late.gluc.mite.lm)    # model summary 
      intervals(late.gluc.mite.lm)    # 95% CIs
      #plot(late.gluc.mite.lm)       # check residuals  
      
    ## d) Late development baseline blood glucose by late dev. mites
      late.base.gluc.mite.lm <- lme(base.gluc ~ mite.bin,
                                    random = ~1|nest.id,
                                  data = subset(nestling_parent_care, 
                                                sample.state == 'late'&
                                                  !is.na(x = base.gluc))) 
      
      summary(late.base.gluc.mite.lm)    # model summary 
      intervals(late.base.gluc.mite.lm)    # 95% CIs
      #plot(late.base.gluc.mite.lm)       # check residuals  
      
    ## e) Late development difference in blood glucose by late dev. nest age
      late.gluc.age.lm <- lme(gluc.diff ~ nestling.age, 
                             random = ~1|nest.id,
                             data = subset(nestling_parent_care, 
                                           sample.state == 'late'&
                                             !is.na(x = gluc.diff))) 
      
      
      summary(late.gluc.age.lm)    # model summary 
      intervals(late.gluc.age.lm)    # 95% CIs
      #plot(late.gluc.age.lm)       # check residuals  
      
    ## f) Late development baseline blood glucose by late dev. nest age
      late.base.gluc.age.lm <- lme(base.gluc ~ nestling.age,
                                  random = ~1|nest.id,
                                  data = subset(nestling_parent_care, 
                                                sample.state == 'late'&
                                                  !is.na(x = base.gluc))) 
      
      
      summary(late.base.gluc.age.lm)    # model summary 
      intervals(late.base.gluc.age.lm)    # 95% CIs
      #plot(late.base.gluc.age.lm)       # check residuals  
      
      
    ## g) Late development difference in blood glucose by baseline sampling time
      late.gluc.base.s.lm <- lme(gluc.diff ~ base.gluc.s, 
                                 random = ~1|nest.id,
                                 data = subset(nestling_parent_care, 
                                               sample.state == 'late'&
                                                 !is.na(x = gluc.diff)))  
      
      summary(late.gluc.base.s.lm)    # model summary 
      intervals(late.gluc.base.s.lm)    # 95% CIs
      #plot(late.gluc.base.s.lm)       # check residuals  
      
    ## h) Late development baseline blood glucose by baseline sampling time
      late.base.gluc.base.s.lm <- lme(base.gluc ~ base.gluc.s,
                                      random = ~1|nest.id,
                                      data = subset(nestling_parent_care, 
                                                    sample.state == 'late'&
                                                      !is.na(x = base.gluc))) 
      
      summary(late.base.gluc.base.s.lm)    # model summary 
      intervals(late.base.gluc.base.s.lm)    # 95% CIs
      #plot(late.base.gluc.base.s.lm)       # check residuals  
      
    ## i) Late development difference in blood glucose by late dev. trial temp.
      late.gluc.temp.lm <- lme(gluc.diff ~ trial.temp, 
                              random = ~1|nest.id,
                              data = subset(nestling_parent_care, 
                                            sample.state == 'late'&
                                              !is.na(x = gluc.diff) &
                                              !is.na(x=trial.temp))) 
      
      
      summary(late.gluc.temp.lm)    # model summary 
      intervals(late.gluc.temp.lm)    # 95% CIs
      #plot(late.gluc.temp.lm)       # check residuals  
      
    ## j) Late development baseline blood glucose by late dev. trial temp.
      late.base.gluc.temp.lm <- lme(base.gluc ~ trial.temp,
                                   random = ~1|nest.id,
                                   data = subset(nestling_parent_care, 
                                                 sample.state == 'late'&
                                                   !is.na(x = base.gluc) &
                                                   !is.na(x = trial.temp)))  
      
      summary(late.base.gluc.temp.lm)    # model summary 
      intervals(late.base.gluc.temp.lm)    # 95% CIs
      #plot(late.base.gluc.temp.lm)       # check residuals
      
      
  ### 6.3 Simple associations mid-development (covariates with exposure)
      
    # Note: Covariates may include potential confounders or precision variables
      
    ## a) Mid-development right wing length by mid-dev. number of nestlings 
      # in the nest
      mid.wing.nos.lm <- lme(rt.wing.length ~ nestling.number,
                                 random = ~1|nest.id,
                                 data = subset(nestling_parent_care,
                                               sample.state == 'mid' &
                                                 !is.na(x = rt.wing.length) &
                                                 !is.na(x = nestling.number)))

      summary(mid.wing.nos.lm)    # model summary
      intervals(mid.wing.nos.lm)    # 95% CIs
      #plot(mid.wing.nos.lm)       # check residuals
      
    ## b) Mid-development right wing length by mid-dev. mites
      mid.wing.mite.lm <- lme(rt.wing.length ~ mite.bin, 
                                random = ~1|nest.id,
                                data = subset(nestling_parent_care, 
                                              sample.state == 'mid' &
                                                !is.na(x = rt.wing.length) &
                                                !is.na(x = mite.bin)))  
      
      summary(mid.wing.mite.lm)    # model summary 
      intervals(mid.wing.mite.lm)    # 95% CIs
      #plot(mid.wing.mite.lm)       # check residuals  
      
    ## c) Mid-development right wing length by mid-dev nest age
      mid.wing.age.lm <- lme(rt.wing.length ~ nestling.age, 
                              random = ~1|nest.id,
                              data = subset(nestling_parent_care, 
                                            sample.state == 'mid' &
                                              !is.na(x = rt.wing.length) &
                                              !is.na(x = nestling.age)))  
      
      summary(mid.wing.age.lm)    # model summary 
      intervals(mid.wing.age.lm)    # 95% CIs
      #plot(mid.wing.age.lm)       # check residuals  
      
      
  ### 6.4 Simple associations late development (covariates with exposure)
      
    # Note: Covariates may include potential confounders or precision variables
      
    ## a) Late development right wing length by number of nestlings 
      # in the nest
      late.wing.nos.lm <- lme(late.rt.wing.length ~ nestling.number,
                                random = ~1|nest.id,
                                data = subset(late_nestling_size,
                                                !is.na(x = late.rt.wing.length) &
                                                !is.na(x = nestling.number)))

      summary(late.wing.nos.lm)    # model summary
      intervals(late.wing.nos.lm)    # 95% CIs
      #plot(late.wing.nos.lm)       # check residuals

    ## b) Late vs mid development right wing length by number of nestlings 
      # in the nest
      diff.wing.nos.lm <- lme(rt.wing.diff ~ nestling.number,
                                 random = ~1|nest.id,
                                 data = subset(late_nestling_size,
                                               !is.na(x = rt.wing.diff) &
                                                 !is.na(x = nestling.number)))

      summary(diff.wing.nos.lm)    # model summary
      intervals(diff.wing.nos.lm)    # 95% CIs
      #plot(diff.wing.nos.lm)       # check residuals
      
    ## c) Late development right wing length by late dev. mites
      late.wing.mite.lm <- lme(late.rt.wing.length ~ mite.bin,
                                 random = ~1|nest.id,
                                 data = subset(late_nestling_size,
                                               !is.na(x = late.rt.wing.length) &
                                                 !is.na(x = mite.bin)))

      summary(late.wing.mite.lm)    # model summary
      intervals(late.wing.mite.lm)    # 95% CIs
      #plot(late.wing.mite.lm)       # check residuals
      
    ## d) Late vs mid development right wing length by late dev. mites
      diff.wing.mite.lm <- lme(rt.wing.diff ~ mite.bin, 
                                 random = ~1|nest.id,
                                 data = subset(late_nestling_size,
                                               !is.na(x = rt.wing.diff) &
                                                 !is.na(x = mite.bin)))  
      
      summary(diff.wing.mite.lm)    # model summary 
      intervals(diff.wing.mite.lm)    # 95% CIs
      #plot(diff.wing.mite.lm)       # check residuals  

    ## e) Late development right wing length by late dev. nest age
      late.wing.age.lm <- lme(late.rt.wing.length ~ nestling.age, 
                               random = ~1|nest.id,
                               data = subset(late_nestling_size,
                                             !is.na(x = late.rt.wing.length) &
                                               !is.na(x = nestling.age)))  
      
      summary(late.wing.age.lm)    # model summary 
      intervals(late.wing.age.lm)    # 95% CIs
      #plot(late.wing.age.lm)       # check residuals  
      
    ## f) Late vs mid development right wing length by late dev. nest age
      diff.wing.age.lm <- lme(rt.wing.diff ~ nestling.age, 
                               random = ~1|nest.id,
                               data = subset(late_nestling_size,
                                             !is.na(x = rt.wing.diff) &
                                               !is.na(x = nestling.age)))  
      
      summary(diff.wing.age.lm)    # model summary 
      intervals(diff.wing.age.lm)    # 95% CIs
      #plot(diff.wing.age.lm)       # check residuals  

    

###############################################################################
##############              7. Export data files                ##############
###############################################################################
      
  ### 7.1 Export data to an RData file     
    ## a) Save and export raw data tables 
      # Files are saved in the 'data' folder in the working directory as an
      # RData file.
      save(file = here('data/3_bs_phys_data.RData'), 
           list = c('nestling_parent_care', 'nestling_parent_care_l'))
    
      
      save(file = here('data/4_bs_phys_data.RData'), 
           list = c('nestling_parent_care','late_nestling_size'))
           