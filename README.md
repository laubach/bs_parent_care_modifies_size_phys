# bs_parent_care_modifies_size_phys

# Manuscript Citation
Laubach, Z. M., Madden, S. A., Pardue, A., and Safran, R. J. 2025. The American Naturalist. Does parental care modify the association of early-life size and growth with physiology?


----

### Author names, affiliations and contact information
Zachary M. Laubach, 1
Email: zachary.laubach@colorado.edu

Sage A. Madden, 2
Aleea Pardue, 1
Rebecca J. Safran, 1

1 Department of Ecology and Evolutionary Biology, University of Colorado, Boulder, CO, USA
2 Department of Evolution and Ecology, University of California, Davis, CA, USA


----

### Paper summary:
Size and growth early in life are associated with physiological development and all these traits influence fitness. Life history theory predicts that the relationship between traits reflect constraints involving allocation and acquisition of resources. Using longitudinal data from 113 wild nestling barn swallows (_Hirundo rustica erythrogaster_), we first characterized developmental changes in glucose metabolism, a physiological trait involved in energy mobilization and response to stress. Next, we tested hypotheses from life history theory about allocation and acquisition of resources based on associations of nestling size and growth with glucose physiology and assessed whether these relationships are modified by parental care. Larger nestlings had higher baseline blood glucose and larger magnitude of change in glucose in response to a stressor than smaller nestlings. Further, the relationship in which greater growth was associated with stronger stress response glucose levels was most pronounced among birds in nests that received the lowest amount of parental care. These results suggest that physiological constraints may contribute to the early-life disadvantage of slow growth, especially in the context of lower parental care. While these findings are inconsistent with a trade-off involving differential allocation of resources between life history traits, they align with the differential acquisition hypothesis.

![Data collection](/cover_image.png "cover image")


----

### Author contributions
Code and data collection by: Zachary M. Laubach

Data collection also by: Sage A. Madden and Aleea Pardue


----

### File organization and description

#### Purpose: This repository contains the scripts, data, and output necessary to reproduce the analysis for this paper. Scripts are modular and numbered in the order they are to be run. An overview of the repository organization and file descriptions are provided below.

#### There are 3 subdirectories in this repository.

* _data -_ this subdirectory contains the raw data (.csv) and the processed data (.RData) for all analyses. The .RData follow the modular scripts such that the processed data saved at the end of one script is loaded in the sequential script, thus allowing the user to enter the analysis pipeline at an stage without having to re-run all previous steps.

* _scripts -_ this subdirectory contains the sequentially numbered R scripts for load and cleaning the data and for all downstream analyses. Given that the scripts are modular, they can be run alone. For complete reproducibility run the scripts in sequential order starting with script 1.

* _output_ - this subdirectory contains summary tables and figures generated as part of the analysis.

#### Data description
* _nestling_data.csv -_ This file has 354 rows and 29 columns that contain the raw data measured for each nestling barn swallow. Repeated measures for each individual nestling are stored in rows in long format (i.e., there are multiple rows for each individual). The columns include four data types, including 11 characater variables, 13 double numeric variables, 1 logical variable and 4 time variables.
	-	female_band = col_character(), the mother's ID
	-	male_band = col_character(), the father's ID
	-	nestling_band = col_character(), the nestling ID
	-	hatch_order = col_double(), numbering of hatching order
	-	nest = col_double(), the nest ID
	-	site = col_character(), the name of the breeding site location
	-	brood = col_double(), the brood attempt number (either first or second brood)
	-	hatch_date = col_character(), date when the first nestling hatched
	-	sample_date = col_character(), date when a samples were collected from nestlings
	-	nestling_age = col_double(), estimated nestling age (days) on date when samples were collected
	-	nestling_number = col_double(), total number of nestlings in a nest on the date when samples were collected
	-	extract_time = col_time(format = ""), 24 hr time when first nestling was extracted from a nest for sample collection before the behavioral observations
	-	nos_mites = col_double(), estimated number of mites counted on nestling
	-	mites_tp = col_logical(), an unused variable, number of mites counted on paper put in a nest
	-	rt_wing_length = col_double(), the length in mm of the nestlings right wing
	-	mass_pre_obs = col_double(), the nestlings mass before behavioral observations
	-	mass_post_obs = col_double(), the nestlings mass before behavioral observations
	-	post_obs_extract_time = col_time(format = ""), 24 hr time when first nestling was extracted from a nest for sample collection after the behavioral observations
	-	`3min_glucose` = col_double(), the first/baseline blood glucose measurement (mg/dl)
	-	`3min_glucose_time` = col_time(format = ""), the time (minutes:seconds) between when the first nestling was extracted and the first/baseline blood glucose reading
	-	`15min_glucose` = col_double(), the second/baseline blood glucose measurement (mg/dl)
	-	`15min_glucose_time` = col_time(format = ""), the time (minutes:seconds) between when the first nestling was extracted and the first/baseline blood glucose reading
	-	blood_amount_lysis = col_double(), an estimated volume of blood (uL) collected in lysis buffer
	-	blood_amount_rna = col_double(), an estimated volume of blood (uL) collected in RNAlater buffer
	-	lysis_sample = col_character(), a binary indicating if blood collection in lysis buffer was successful
	-	rna_sample = col_character(), a binary indicating if blood collection in lysis buffer was successful
	-	feathers = col_character(), a binary indicating if feather samples were collected
	-	survive_at_sampling = col_character(), a binary indicating if the individual nestling was alive on the sample collection date
	-	notes = col_character(), unstructured notes about the data

* _parent_care_trial.csv -_ This file has 92 rows and 20 columns that contain the raw data measured for each nest. Repeated measures for each nest are stored in rows in long format (i.e., there are multiple rows for each nest; up to three observation trials). The columns include three data types, including 8 character variables, 10 double numeric variables, and 2 time variables.
	-	female_band = col_character(), the mother's ID
	-	male_band = col_character(), the father's ID
	-	nest = col_double(), the nest ID
	-	site = col_character(), the name of the breeding site location
	-	obs_date = col_character(), date when each behavior observation and sample collection occurred
  -	observer = col_character(), the initials of the observer
  -	obs_method = col_character(), whether the behavior observation happened in person or was scored from a video recording
  -	nestling_number = col_double(), total number of nestlings in a nest on the date when samples were collected
  -	nestling_age = col_double(), estimated nestling age (days) on date when samples were collected (based on the date when the first nestling hatched)
  -	blind_camera_distance = col_double(), distance (m) between the observer's blind or the camera from the nest
  -	obs_start_time = col_time(format = ""), 24 hr time when behavioral observation trial began
  -	obs_end_time = col_time(format = ""), 24 hr time when behavioral observation trial ended
  -	trial_temp = col_double(), the temperature (deg.C) recorded from the Govee by the observer at the approximate time of the behavior observation
  -	min_temp = col_double(), the 24 hour min temperature (deg.C) recorded from the Govee by the observer at the approximate time of the behavior observation
  -	max_temp = col_double(), the 24 hour max temperature (deg.C) recorded from the Govee by the observer at the approximate time of the behavior observation
  -	trial_humidity = col_double(), the percent humidity recorded from the Govee by the observer at the approximate time of the behavior observation
  -	wind_speed = col_double(), the wind speed recorded from local weather stations by the observer at the approximate time of the behavior observation
  -	wind_gust = col_double(), the wind speed gusts recorded from local weather stations by the observer at the approximate time of the behavior observation
  -	cloud_cover = col_character(), a categorical description of the amount of cloud cover recorded by the observer at the approximate time of the behavior observation
  -	notes = col_character(), unstructured notes about the data

* _parent_care_obs.csv -_ This file has 92 rows and 18 columns that contain the raw data measured for each nest. Repeated measures for each nest are stored in rows in long format (i.e., there are multiple rows for each nest; up to three observation trials). The columns include two data types, including 3 character variables and 15 double numeric variables.
	-	female_band = col_character(), the mother's ID
	-	nest = col_double(), the nest ID
	-	site = col_character(), the name of the breeding site location
	-	obs_date = col_character(), date when each behavior observation and sample collection occurred
	-	nestling_age = col_double(), estimated nestling age (days) on date when samples were collected (based on the date when the first nestling hatched)
	-	total_visits = col_double(), a count of the sum of both female and male visits to the nest
 	-	female_visits = col_double(), a count of female visits to the nest
 	-	male_visits = col_double(), a count of male visits to the nest
 	-	total_feeding_visits = col_double(), a count of the sum of both female and male feeding visits to the nest
 	-	female_feeding_visits = col_double(), a count of female feeding visits to the nest
 	-	male_feeding_visits = col_double(), a count of male feeding visits to the nest
 	-	total_an_duration = col_double(), a duration (s) of time either or both the female and male were at the nest
 	-	female_an_duration = col_double(), a duration (s) of time the female was at the nest
 	-	male_an_duration = col_double(), a duration (s) of time the male was at the nest
 	-	total_brooding_duration = col_double(), a duration (s) of time either the female or male were at the nest and brooding
 	-	female_brooding_duration = col_double(), a duration (s) of time the female was at the nest and brooding
 	-	male_brooding_duration = col_double(), a duration (s) of time the male was at the nest and brooding
 	-	obs_duration = col_double(), a duration (s) of time during which observation data were collected (i.e., total trial length of time)


* _nest_parent_summary.csv -_ This file has 33 rows and 26 columns that contain summary raw data measured for each nest. Data for each nest are stored in rows. The columns include two data types, including22 character variables and 4 double numeric variables.
	-	site = col_character(), the name of the breeding site location
	-	`Nest #` = col_double(), a numeric ID for each nest
  -	`Govee date` = col_character(), date when Govee thermometers were deployed near each nest
  -	`Govee initials` = col_character(), initials of the person who deployed the Govee
  -	CI = col_character(), date when the clutch was initiated at each nest
  -	`No. eggs` = col_double(), number of eggs laid
  -	`No. hatchling` = col_double(), number of nestlings that hatched
  -	`No. survive day 12` = col_double(), number of nestlings that survived to ~ day 12
  -	EHD = col_character(), estimated hatch date based on clutch initiation
  -	HD = col_character(), actual hatch date when first nestling hatches
  -	`Day 3` = col_character(), date when early-development parental care observation occurred
  -	`Day 3 Observer` = col_character(), initials of the parental care observer for early-development
  -	`Day 8` = col_character(), date when mid-development parental care observation occurred
  -	`Day 8 Observer` = col_character(), initials of the parental care observer for mid-development
  -	`Day 12` = col_character(), date when late-development parental care observation occurred
  -	`Day 12 Observer` = col_character(), initials of the parental care observer for late-development
  -	`All data sheets` = col_character(), a binary for if all nestling and parental care data sheets were accounted for
  -	`Data entered` = col_character(), initials of who entered data
  -	Complete_obs = col_character(), a binary for if parental care data was collected at early, mid and late development, i.e., complete data set for each nest
  -	Notes = col_character(), unstructured notes about the data
  -	nestling_measures = col_character(), a binary for if nestling measures were collected for each nest
  -	ab_pro_3 = col_character(), a binary for if the early-development parental care data were summarized in animal behavior pro app.
  -	ab_pro_8 = col_character(), a binary for if the mid-development parental care data were summarized in animal behavior pro app.
  -	ab_pro_12 = col_character(), a binary for if the late-development parental care data were summarized in animal behavior pro app.
  -	parent_care_data_sheet = col_character(), a binary for if all parental care data sheets were accounted for
  -	govee_data = col_character() a binary for if Govee temperature data were collected for each nest

* _parent_care_trial.csv -_ This file has 92 rows and 4 columns that contain the raw data measured for each nest. Repeated measures for each nest are stored in rows in long format (i.e., there are multiple rows for each nest; up to three observation trials). The columns include three data types, including 2 character variables, 1 double numeric variable, and 1 time variable.
	-	female_band = col_character(), the mother's ID
	-	nest.id = col_character(), the nest ID
	-	obs.date = col_date(format = ""), date when each behavior observation and sample collection occurred
  -	obs.med.temp = col_double(), the median temperature from the Govee thermometer during the parental care observation period.


#### Script description

* _0_import_data_bs_phys.R -_ Load barn swallow parental care and offspring physiology data. The 0_import_data script simply loads the raw data to be used in the analysis. If data are pulled from a remote location, skip this step and proceed to script 1.

* _1_tidy_data_bs_phys.R -_ Tidy and join data in preparation for downstream analyses of data.

* _2_descript_stat_bs_phys.R -_ Calculate descriptive statistics and visualize glucose data.

* _3_gluc_response_repeat_models_bs_phys.R -_ Models of glucose response to a standardized stressor and glucose repeatability across development.

* _4_sz_gluc_models_bs_phys.R -_ Models of early-life size and growth as determinants of blood glucose levels.

* _5_descript_stat_bs_parent_care.R -_ Calculate descriptive statistics and visualize parental care data.

* _6_sz_care_gluc_bs_models.R -_ Check for and model effect modification on the relationship between size/growth and glucose by parental care.


#### Output description

#### Plots used to create Figure 4
* _gluc.diff.by.grow.by.care.plot.pdf -_ The association between growth on late development glucose (stress − baseline) from models stratified by amount of parental care

* _gluc.diff.by.wing.by.care.plot.pdf -_ The association between late development size and late development glucose response (stress − baseline) from models stratified by amount of parental care

* _base.gluc.by.grow.by.care.plot.pdf -_ The association between growth and late development baseline blood glucose (mg/dl) from models stratified by amount of parental care

* _base.gluc.by.wing.by.care.plot.pdf -_ The association beween late development size and late development baseline blood glucose (mg/dl) from models stratified by amount of parental care

* _gluc.diff.by.grow.by.care.strat.pred.plot.pdf -_ Predicted values and raw data for the relationship between right wing growth and glucose response (stratified models)

*	_gluc.diff.by.wing.by.care.strat.pred.plot.pdf -_ Predicted values and raw data for the relationship between right wing length and glucose response (stratified models)

*	_base.gluc.by.grow.by.care.strat.pred.plot.pdf -_ Predicted values and raw data for the relationship between right wing growth and baseline glucose levels (stratified models

*	_base.gluc.by.wing.by.care.strat.pred.plot.pdf -_ Predicted values and raw data for the relationship between right wing length and baseline glucose levels (stratified models)

#### Plots used to create Figure A1
* _avg_feed_by_parent_care.pdf -_ Average count of feeding visits for by level of parental as determined by the feeding BLUPs

#### A data exploration table
* _univar_parent_care_trial.pdf -_ Descriptive statistics for weather conditions during the behavioral observation trials

#### Data exploration tables used to create Table A3
* _univar_parent_care_behav.pdf -_ Descriptive statistics parental behaviors during the behavioral observation trials

* _univar_morph_mites.pdf -_ Descriptive statistics of nestling size/growth by developmental stage

* _univar_glucose.pdf -_ Descriptive statistics of nestling glucose levels by developmental stage

#### A data exploration figure
* _tot_feed_hist.pdf -_ Histogram of total feeding rate (counts/hour) by developmental state

#### A data exploration figure
* _tot_visit_hist.pdf -_ Histogram of total visitation rate (counts/hour) by developmental state

#### Plots used to create Figure 3
* _gluc.respon.by.dev.plot.pdf -_ Nestling blood glucose at baseline and in response to
handling stress at mid− and late development

* _mid_gluc_samp_scatter.pdf -_ Scatter plot of serially sampled glucose (paired baseline and stressed induced samples) at mid development

* _late_gluc_samp_scatter.pdf -_ Scatter plot of serially sampled glucose (paired baseline and stressed induced samples) at late development

#### A data exploration table
* _univar_growth.pdf -_ Descriptive statistics of nestling size/growth

#### A data exploration figure
* _gluc_diff_hist.pdf -_ Histogram of difference glucose measures (stress − baseline) by developmental state

#### A data exploration figure
* _mid_gluc_hist.pdf -_ Histogram of serially sampled glucose at mid−development

#### A data exploration figure
* _late_gluc_hist.pdf -_ Histogram of serially sampled glucose at late development


----

### Software and version information

#### This analysis was performed in R
*	R version 4.4.2 (2024-10-31)
*	Platform: x86_64-apple-darwin20
*	Running under: macOS Sequoia 15.1

#### R package versions
attached base packages:
[1] stats4    stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] lmerTest_3.1-3      pbkrtest_0.5.3      emmeans_1.10.5      irrICC_1.0         
 [5] irrNA_0.2.3         broom.mixed_0.2.9.6 broom_1.0.7         gridExtra_2.3      
 [9] viridis_0.6.5       viridisLite_0.4.2   hrbrthemes_0.8.7    bbmle_1.0.25.1     
[13] DHARMa_0.4.7        performance_0.12.4  lme4_1.1-35.5       Matrix_1.7-1       
[17] nlme_3.1-166        Hmisc_5.2-1         here_1.0.1          lubridate_1.9.3    
[21] forcats_1.0.0       stringr_1.5.1       dplyr_1.1.4         purrr_1.0.2        
[25] readr_2.1.5         tidyr_1.3.1         tibble_3.2.1        ggplot2_3.5.1      
[29] tidyverse_2.0.0    
