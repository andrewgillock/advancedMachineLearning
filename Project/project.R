#getwd()
setwd('/Users/apgillock/Dropbox/MIS 382N Advanced Machine Learning/Project/dataset_diabetes')
data <- read.csv('diabetic_data.csv')

head(data)
View(data)
dim(data) # 101766 x 50

library(tidyverse)

##############################################################################################

# deal with missing values
sum(is.na(data))
sum(is.null(data))

data %>% group_by(race) %>% count() # 2273 missing race
data %>% group_by(weight) %>% count() # 98569 missing weight
data %>% group_by(payer_code) %>% count() # 40256 missing payer_code
data %>% group_by(medical_specialty) %>% count() # 49949 missing medical_specialty

# get rid of '?'
data <- data %>% 
  select(-weight, -payer_code, -medical_specialty) # de-select irrelevant columns
  
data %>% 
  filter(diag_1 != '?', diag_2 != '?', diag_3 != '?') %>% # remove unknown instances of diagnoses
  filter_all(any_vars(. %in% '?')) # check for ?s

data %>% 
  filter(diag_1 != '?', diag_2 != '?', diag_3 != '?') %>% 
  filter_all(any_vars(. %in% '?')) %>% 
  filter(race != '?') # remove unknown instances of race

data <- data %>% 
  filter(diag_1 != '?', diag_2 != '?', diag_3 != '?') %>% 
  filter(race != '?')

data %>% filter_all(any_vars(. %in% '?')) %>% count() # ensure no more missing values

dim(data) # after removing missing values, we are left with 98053 rows, 47 columns

##############################################################################################
library(psych)
library(Hmisc)
# how much numeric data do we have?
data %>% select_if(is.numeric) %>% dim() # 98053 x 13
num_data <- data %>% 
  select_if(is.numeric) %>% 
  select(
    -encounter_id, -patient_nbr, 
    -admission_type_id, -discharge_disposition_id,
    -admission_source_id) 

hist.data.frame(num_data) # create histograms

describe(num_data) # get summary statistics

##############################################################################################

# select records relating to medication
names(data)
meds <- names(data)[22:38]

data %>% select(any_of(meds)) %>% count()

##############################################################################################

# let's start by considering a binary response variable
data %>% group_by(readmitted) %>% count()

data2 <- data %>% mutate(
  readmitted = case_when(
    readmitted == '>30' ~ 1,
    readmitted == '<30' ~ 1,
    readmitted == 'NO' ~ 0))

data2 %>% group_by(readmitted) %>% count()

# now we encode medication variables
data2 <- data2 %>% 
  mutate_at(
  vars(any_of(meds)),
  funs(case_when(
    . == 'Steady' ~ 1,
    . == 'Down' ~ 1,
    . == 'Up' ~ 1,
    . == 'No' ~ 0)
  )
)

head(data2)
meds
data2 %>% select(meds)

##############################################################################################

# lets not consider drug interactions for now
drug_combos <- names(drug_class_data)[23:27]
meds %>% length()
drug_classes %>% length()
# try grouping medications
drug_class_data <- data2 %>% mutate(
  'Biguanide' = case_when(
    metformin == 1 ~ 1,
    TRUE ~ 0),
  'Meglitinide' = case_when(
    repaglinide == 1 ~ 1,
    nateglinide == 1 ~ 1,
    TRUE ~ 0),
  'Sulfonylurea' = case_when(
    chlorpropamide == 1 ~ 1,
    glimepiride == 1 ~ 1,
    acetohexamide == 1 ~ 1,
    glipizide == 1 ~ 1,
    glyburide == 1 ~ 1,
    tolbutamide == 1 ~ 1,
    tolazamide == 1 ~ 1,
    TRUE ~ 0),
  'Thiazolidinedione' = case_when(
    pioglitazone == 1 ~ 1,
    rosiglitazone == 1 ~ 1,
    troglitazone == 1 ~ 1,
    TRUE ~ 0),
  'Alpha-glucosidase inhibitor' = case_when(
    acarbose == 1 ~ 1,
    miglitol == 1 ~ 1,
    TRUE ~ 0),
  'Loop Diuretic' = case_when(
    examide == 1 ~ 1,
    TRUE ~ 0),
  'Dipeptidyl peptidase-4 inhibitor' = case_when(
    citoglipton == 1 ~ 1,
    TRUE ~ 0)
  ) %>% select(-meds, -drug_combos)

# even after placing in drug classes, we have a lot of observations taking no medication AND no insulin
drug_class_data %>% filter(
  Biguanide != 1, 
  Meglitinide != 1,
  Sulfonylurea != 1,
  Thiazolidinedione != 1,
  `Alpha-glucosidase inhibitor` != 1,
  `Loop Diuretic` != 1,
  `Dipeptidyl peptidase-4 inhibitor` != 1
  ) %>% group_by(insulin) %>% count() # 22898 'No'

# should be equivalent to this
drug_class_data %>% group_by(diabetesMed) %>% count() # 22702 'No'

# create list of drug class names
drug_classes <- drug_class_data %>% 
  select(Biguanide, Meglitinide, Sulfonylurea, 
         Thiazolidinedione, `Alpha-glucosidase inhibitor`, 
         `Loop Diuretic`, `Dipeptidyl peptidase-4 inhibitor`) %>% names()

drug_class_data %>% group_by(diabetesMed) %>% count()

drug_class_data %>% select(drug_classes, diabetesMed) %>% filter(
  Biguanide != 1, 
  Meglitinide != 1,
  Sulfonylurea != 1,
  Thiazolidinedione != 1,
  `Alpha-glucosidase inhibitor` != 1,
  `Loop Diuretic` != 1,
  `Dipeptidyl peptidase-4 inhibitor` != 1
) %>% filter(diabetesMed == 'Yes') %>% count()

# no patient marked as not taking diabetes medication is on any of the drugs in drug_classes... or insulin
drug_class_data %>% filter(diabetesMed == 'No') %>% 
  select(drug_classes, insulin) %>% 
  group_by(`Dipeptidyl peptidase-4 inhibitor`) %>% count()


##############################################################################################
drug_class_data %>% group_by(admission_type_id) %>% count()
drug_class_data %>% group_by()
##############################################################################################

split = sort(sample(nrow(drug_class_data), nrow(drug_class_data)*0.7))
train <- drug_class_data[split,]
test <- drug_class_data[-split,]

train %>% group_by(readmitted) %>% count()
test %>% group_by(readmitted) %>% count()
