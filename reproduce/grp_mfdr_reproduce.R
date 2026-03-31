####################################################################################################
#
#                                                                                     
#   Filename    :	             grp_mfdr_reproduce.R										  
#   Required input files  :    grp_fdr_sim_funs.R                                                        
#   Output files :             Fig_1.png, Fig_2.png, Fig_3.png, Fig_4.png, Table_1.png
#
#   Required R packages :  devtools, grpreg, ncvreg, Matrix, covTest, selectiveInference, ggplot2,
#                          Rccp, gridExtra, locfdr, grid, reshape2, hdi, knockoff, DescTools
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
library(DescTools)
library(ggplot2)

## Sets the working directory to current location of this folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Source functions used in the simulations for the construction of figures
source("grp_fdr_sim_funs.R")

##################################################
### 
###   Fig 1 (false discovery curves)
###
##################################################


fig1 = create_fig1()
ggsave("Fig1.png", fig1, width = 7, height = 7, units = "in", dpi = 300)

##################################################
### 
###   Fig 2 (mFDR accuracy by sample size)
###
##################################################

fig2 = create_fig2(reps = 100)
ggsave("Fig2.png", fig2, width = 7, height = 3.5, units = "in", dpi = 300)


##################################################
### 
###     Fig 3 (Accuracy vs. Sample Size)
###
##################################################


fig3 = create_fig3(reps = 200)
ggsave("Fig3.png", fig3, width = 7, height = 3.5, units = "in", dpi = 300)


##################################################
### 
###     Fig 4 (True Positives vs. Signal Strength)
###
##################################################

fig4 = make_plot4(reps = 50)
ggsave("Fig4.png", fig4, width = 7, height = 3.5, units = "in", dpi = 300)


##################################################
###
###     Table 1 (Comparison vs. Cross-validation)
###
##################################################


png("Table_1.png", h=15, w=15, units = 'in', res = 300)
   make_table1(reps = 100)
dev.off()

   
### NOTE: CASE STUDY RESULTS ARE RECREATED VIA THE FILE grp_mfdr_case_study.R
