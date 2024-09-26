# bs_parent_care_modifies_size_phys
Laubach, Z. M., Madden, S. A., Pardue, A., and Safran, R. J. (in revision) The American Naturalist. Parental care modifies the role of early-life size and growth in shaping future physiology.


#### By: Zachary M. Laubach
#### Updated: 26 May 2023

![Data collection](/cover_image.png "cover image")

#### Purpose: This repository contains the scripts, data, and output necessary to reproduce the analysis for this paper. Scripts are modular and numbered in the order they are to be run. An overview of the repository organization and file descriptions are provided below.

This analysis was performed in R
	* R version 4.2.3 (2023-03-15)
	* Platform: x86_64-apple-darwin17.0 (64-bit)
	* Running under: macOS 14.2.1
	
---- 

### File organization and description

#### There are 4 subdirectores in this repository. 

* _data -_ this subdirectory contains the raw data (.csv) and the processed data (.RData) for all analyses. The .RData follow the modular scripts such that the processed data saved at the end of one script is loaded in the sequential script, thus allowing the user to enter the analysis pipeline at an stage without having to re-run all previous steps. 

* _scripts -_ this subdirectory contains the sequentially numbered R scripts for load and cleaning the data and for all downstream analyses. Given that the scripts are modular, they can be run alone. For complete reproducibility run the scripts in sequential order starting with script 1. 

* _output_ - this subdirectory contains summary tables and figures generated as part of the analysis.

* _packrat -_ this subdirectory is created by the R package [packrat](https://rstudio.github.io/packrat/), which is a dependency management system for R. In this subdirectory are the specific versions of the R packages used in this analysis. 

#### Script description

* _0_import_data_bs_phys.R -_ Load barn swallow parental care and offspring physiology data. The 0_import_data script simply loads the raw data to be used in the analysis. If data are pulled from a remote location, skip this step and proceed to script 1.

* _1_tidy_data_bs_phys.R -_ Tidy and join data in preparation for downstream analyses of data.

* _2_descript_stat_bs_phys.R -_ Calculate descriptive statistics and visualize glucose data.

* _3_gluc_response_repeat_models_bs_phys.R -_ Models of glucose response to a standardized stressor and glucose repeatability across development.

* _4_sz_gluc_models_bs_phys.R -_ Models of early-life size and growth as determinants of blood glucose levels.

* _5_descript_stat_bs_parent_care.R -_ Calculate descriptive statistics and visualize parental care data.

* _6_sz_care_gluc_bs_models.R -_ Check for and model effect modification on the relationship between size/growth and glucose by parental care. 
