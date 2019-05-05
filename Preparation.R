# Preparation of the analysis
# Initialize the factors of the design
# samp=sample size, es=effect size, corr=correlation/covariance, ex=(in)exchangeability condition
samp <- c(10, 20, 50, 100)
es <- c(0, 0.2, 0.5, 0.8)
corr <- c(0, .1, .3, .5)
ex <- c('exmet','inex1','inex2')
# Create the simulation design matrix (full factorial)
Design <- expand.grid(samp = samp, es = es, corr = corr, ex = ex)
# Remove irrelevant design rows (within condition 'inex1')
Design <- Design[-c(81:128),]
# Install and load necessary packages
install.packages('perm')
install.packages('clusterGeneration')
install.packages('MASS')
install.packages('dplyr')
library(perm)
library(clusterGeneration)
library(MASS)
library(dplyr)
# Source the necessary files
source("MyDataGeneration.R")
source("Method_new.R")
source("Method_old.R")
source("MyEvaluationPC.R")