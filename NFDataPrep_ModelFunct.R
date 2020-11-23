#Reading in train data
train <- read.csv("hm7-Train.csv",na.strings=c("","NA","<NA>"))
test  <- read.csv("hm7-Test.csv",na.strings= c("","NA","<NA>"))

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

#Changing other values to NAs
train <- train %>% replace_with_na(replace = list(max_glu_serum="None", A1Cresult="None"))
test <- test %>% replace_with_na(replace = list(max_glu_serum="None", A1Cresult="None"))
# Changing values that equal NA to "Unknown" for race
train <- train %>% mutate_at(vars(race),~replace(.,is.na(.),"Unknown"))
test <- test %>% mutate_at(vars(race),~replace(.,is.na(.),"Unknown"))
#Convert character vars to factors
train[sapply(train, is.character)] <- lapply(train[sapply(train, is.character)], 
                                             as.factor)
test[sapply(test, is.character)] <- lapply(test[sapply(test, is.character)], 
                                           as.factor)

#Convert some int columns with nominal values to factors
train <- train %>% mutate_at(vars(admission_type,discharge_disposition,
                                  admission_source),as.factor)
test <- test %>% mutate_at(vars(admission_type,discharge_disposition,
                                admission_source),as.factor)
#Convert patientID to char val
train <- train %>% mutate_at(vars(ends_with("ID")), as.character)
test <- test %>% mutate_at(vars(ends_with("ID")), as.character)

#Observation
#------------
#Droping a few columns with high MV percentage 
#shoots up the complete cases percentage from 28% to 98% 
train <- select(train, -c(payer_code, medical_specialty, max_glu_serum, A1Cresult))
test <- select(test, -c(payer_code, medical_specialty, max_glu_serum, A1Cresult))

#Drop NA values
train <-drop_na(train)

#### MODEL TIME!!! ####
#Fitting logistic regression model (Best = 0.6408396)
fit <- glm(data=train, readmitted ~ race+gender+age+admission_type+
             discharge_disposition+admission_source+time_in_hospital+
             num_lab_procedures+num_procedures+num_medications+number_outpatient+
             number_emergency+number_inpatient+number_diagnoses+metformin+repaglinide+
             nateglinide+glimepiride+glipizide+glyburide+pioglitazone+rosiglitazone+
             acarbose+miglitol+insulin+diabetesMed, family="binomial")