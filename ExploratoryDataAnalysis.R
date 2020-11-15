#Hospital Readmission - Initial exploration of the data
#Author - Rafia Bushra
#Date Created - 11/15/20

library(tidyverse)
library(ggplot2)
library(dplyr)
library(mice)
library(VIM)


#Reading in train data
train <- read.csv("../hm7-Train.csv")

glimpse(train)

#Initial ideas/observation based on glimpse
#--------------------------------
#Separate numeric and factor columns
#Turn binary variables to 0/1 perhaps (?) - Gender, diabetesMed
#Change the age range to a numeric column where we only keep the upper limit of the range (maybe)
#Make histograms to see distribution - race, gender, admission type,discharge, admission source, payer_code
#Make Histograms to see skew - time, #labs, #proc, #medication, #outpatient, #emergency, #inpatient, #diagnoses
#Questionable attributes - Admission Source, medical specialty, #proc, max_glu_serum, A1Cresult, Medication Columns (metformin:metformin.piog)
#Lots of missing - payer_code, max_glu_serum, A1Cresult, medical specialty
#Change Nones to NAs - max_glu_serum, A1Cresult


#Complete Case Stats
sum(complete.cases(train)) #Total number of complete cases
(sum(complete.cases(train))/nrow(train))*100  #Percentage of complete cases



