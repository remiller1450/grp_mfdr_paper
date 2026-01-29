####################################################################################################
#
#                                                                                     
#   Filename    :	             grp_mfdr_reproduce.R										  
#   Required input files  :    grp_fdr_sim_funs.R                                                        
#   Output files :             Fig_1.png, Fig_2.png, Fig_3.png, Fig_4.png, Table_1.png
#
#   Required R packages :  devtools, grpreg, ncvreg, Matrix, covTest, selectiveInference, ggplot2,
#                          Rccp, gridExtra, locfdr, grid, reshape2, hdi, knockoff
#
#
####################################################################################################

library(devtools)
#install_github("remiller1450/grpreg")  ## Must use this modified version of grpreg
library(grpreg)
library(Matrix)
library(splines)
library(survival)
library(knockoff)
library(selectiveInference)
library(gridExtra)
library(tidyr)

## Sets the working directory to current location of this folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Source functions used in the simulations for the construction of figures
source("grp_fdr_sim_funs.R")

##################################################
### 
###   Fig 1 and 2 (Expected False Discovery Curves)
###
##################################################

## Create and Export Fig 1
png("Fig_1.png", h=3.3, w=8, units = 'in', res = 300)
  make_plot1(reps = 100)
dev.off()

## Create and Export Fig 2
png("Fig_2.png", h=3.3, w=8, units = 'in', res = 300)
   make_plot2(reps = 100)
dev.off()

##################################################
### 
###     Fig 3 (Accuracy vs. Sample Size)
###
##################################################


## Create and Export Fig 3
png("Fig_3.png", h=3.3, w=7, units = 'in', res = 300)
   make_plot3(reps = 500)
dev.off()


##################################################
### 
###     Fig 4 (True Positives vs. Signal Strength)
###
##################################################

png("Fig_4.png", h=3.5, w=8, units = 'in', res = 300)
   make_plot4(reps = 50)
dev.off()

##################################################
###
###     Table 1 (Comparison vs. Cross-validation)
###
##################################################


png("Table_1.png", h=15, w=15, units = 'in', res = 300)
   make_table1(reps = 100)
dev.off()

   
### NOTE: CASE STUDY RESULTS ARE RECREATED VIA THE FILE grp_mfdr_case_study.R
