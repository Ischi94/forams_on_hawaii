
# forams_on_hawaii

<!-- badges: start -->
[![Generic badge](https://img.shields.io/badge/Status-In_Review-orange.svg)](https://shields.io/)
<!-- badges: end -->

This is the repository for the manuscript **Benthic foraminifera as bioindicators for assessing reef condition in Kāneʻohe Bay, Oʻahu, Hawai'i**.  
  
  
# Structure  
  
All code and data to reproduce our findings can be found in this repository. Both raw and processed data can be found in the **data** folder. Figures for both the main manuscript and supplemental data can be found in the **figures** folder and are created solely using R (`ggplot2`) and can hence be completely reproduced using the accompanying code. The current version of the manuscript and the supplemental data, both blinded for review, can be found in the **manuscript** folder. All Bayesian models with meta-data are saved as *rds* objects in the **model** folder. Supplemental plots, also produced by using R (`flextable`) and knitted to a word-file can be found in the **tables** folder. All R code to produce our results, figures, and tables can be found in the **R** folder. The code is commented and uses relative directories by means of the `here` package. It is therefore possible (and recommended) to simply copy or clone the whole repository including the *rproject* file to a local directory. It is then possible to run all scripts without setting the directory to absolute paths, i.e. it is not needed to manually change anything to make the code run on any device.  
  
# R code  
  
The **R** folder contains all code to reproduce our analysis within the R programming language:  
- bayesian_model.R  
- nmds.R  
- robustness_test.R  
- suppl_tables.R  
  
It is recommended to start with the bayesian_model.R file that fits various Bayesian regression models, checks the model fit and produces the main figures in the manuscript. The nmds.R file conducts an assemblage analysis by means of dimension reduction. The robustness_test.R fits a Bayesian model on an unbiased subset of the data. Any supplemental tables are produced within R with the suppl_tables.R file.  
  


