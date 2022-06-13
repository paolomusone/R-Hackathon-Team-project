################################################################################
"
Program    : HACKATHON PharmaCo Business Case
Team       : Team10
Start Date : 28th Jan 2022
Member     : Karley Webster, Mithrajaa Govindaraj, Paolo Musone
           : Tayla-Lee Chick, Takahiro Yamada                    
"
################################################################################

# Loading required libraries

library(pdftools) 
library(tidytuesdayR)
library(dplyr)
library(tidytext)
library(tidyr)
library(stringr)
library(scales)
library(ggplot2)
library(igraph)
library(ggraph)
library(widyr)
library(tidyverse)
library(readxl)
library(wordcloud)
library(scales)
library(NLP)
library(tm)
library(ggpubr)
library(broom)
library(readr)
library(ggcorrplot)
library(ROCR)
library(caret)
library(rpart)
library(rpart.plot)

################################################################################

# Overview of dataset

################################################################################

# Extracting dataset

db_data <- read_csv("/Users/karleywebster/Documents/Education/Hackathon/DataSets/dataset_diabetes/diabetic_data.csv", 
                    col_types = cols(age = col_character(), 
                                     weight = col_number(), admission_type_id = col_number(), 
                                     discharge_disposition_id = col_number(), 
                                     admission_source_id = col_number(), 
                                     time_in_hospital = col_number(), 
                                     diag_1 = col_number(), diag_2 = col_number(), 
                                     nateglinide = col_character()))

glimpse(db_data) #gives you a quick view of the variables in the object

summary(db_data) #statistical summary of each variable if it is numeric

str(db_data) #shows the data object structure 

sapply(db_data, function(x) sum(is.na(x)))

################################################################################

# Massaging the Data

################################################################################

# weight, payer_code
db_data$diag_1 <- as.numeric(db_data$diag_1) # highest mean can be Random Test
db_data$diag_2 <-as.numeric(db_data$diag_2)  # middle slight high can can be Glucose Tolerance Test
db_data$diag_3 <-as.numeric(db_data$diag_3)  # middle slight high can can be Fasting Blood Sugar Test

##Beginning to clean the data 

new_db_data <- db_data

#cleaning the blanks 
new_db_data_weight <- db_data[-which(is.na(db_data$weight)),]

# lowercasing all characters
tolower(new_db_data_weight)

##changing Male and Female to binary

tolower(new_db_data_weight$gender)
summary(new_db_data_weight$gender)
new_db_data_weight$new_gender <- ifelse(new_db_data_weight$gender == "male", 1, 0)

is.numeric(new_db_data_weight$new_gender)

#identifying outliers 
boxplot(new_db_data_weight$weight,
        ylab = "weight")
boxplot(new_db_data_weight$admission_type_id,
        ylab = "Admission ID")
boxplot(new_db_data_weight$time_in_hospital, 
        ylab= "Time in Hospital")
boxplot(new_db_data_weight$num_lab_procedures,
        ylab = "Lab Procedures")
boxplot(new_db_data_weight$num_medications,
        ylab = "Number of Medications")
boxplot(new_db_data_weight$number_outpatient,
        ylab = "Number of times outpatient")
boxplot(new_db_data_weight$number_emergency,
        ylab = "Number of Emergencies")
boxplot(new_db_data_weight$number_inpatient,
        ylab = "Number of times Inpatient")
boxplot(new_db_data_weight$number_diagnoses,
        ylab = "Number of Diagnoses")

##Removing outliers
new_db_data_weight_no_outliers <- new_db_data_weight

remove_outliers <- function(x, na.rm = TRUE) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

new_db_data_weight_no_outliers$weight <- remove_outliers(new_db_data_weight_no_outliers$weight)
new_db_data_weight_no_outliers$admission_type_id <- remove_outliers(new_db_data_weight_no_outliers$admission_type_id)
new_db_data_weight_no_outliers$time_in_hospital <- remove_outliers(new_db_data_weight_no_outliers$time_in_hospital)
new_db_data_weight_no_outliers$num_lab_procedures <- remove_outliers(new_db_data_weight_no_outliers$num_lab_procedures)
new_db_data_weight_no_outliers$num_medications <- remove_outliers(new_db_data_weight_no_outliers$num_medications)
new_db_data_weight_no_outliers$number_outpatient <- remove_outliers(new_db_data_weight_no_outliers$number_outpatient)
new_db_data_weight_no_outliers$number_emergency <- remove_outliers(new_db_data_weight_no_outliers$number_emergency)
new_db_data_weight_no_outliers$number_inpatient <- remove_outliers(new_db_data_weight_no_outliers$number_inpatient)
new_db_data_weight_no_outliers$number_diagnoses <- remove_outliers(new_db_data_weight_no_outliers$number_diagnoses)


#splitting the data for male and female
new_db_data_weight_gender <- split(new_db_data_weight,f=new_db_data_weight$gender)
new_db_data_weight_gender_female <- data.frame(new_db_data_weight_gender$Female)
new_db_data_weight_gender_male <- data.frame(new_db_data_weight_gender$Male)

#splitting the data based on A1Cresult
new_db_data_weight_A1C <- split(new_db_data_weight,f=new_db_data_weight$A1Cresult)
new_db_data_weight_A1C_highest <- data.frame(new_db_data_weight_A1C$`>8`)
new_db_data_weight_A1C_high <- data.frame(new_db_data_weight_A1C$`>7`)
new_db_data_weight_A1C_normal <- data.frame(new_db_data_weight_A1C$Norm)
new_db_data_weight_A1C_na <- data.frame(new_db_data_weight_A1C$None)


##splitting gender for highest and normal A1C

new_db_data_weight_A1C_highest_gender <- split(new_db_data_weight_A1C_highest,f=new_db_data_weight_A1C_highest$gender)
new_db_data_weight_A1C_highest_gender_female <- data.frame(new_db_data_weight_A1C_highest_gender$Female)
new_db_data_weight_A1C_highest_gender_male <- data.frame(new_db_data_weight_A1C_highest_gender$Male)

new_db_data_weight_A1C_normal_gender <- split(new_db_data_weight_A1C_normal,f=new_db_data_weight_A1C_normal$gender)
new_db_data_weight_A1C_normal_gender_female <- data.frame(new_db_data_weight_A1C_normal_gender$Female)
new_db_data_weight_A1C_normal_gender_male <- data.frame(new_db_data_weight_A1C_normal_gender$Male)


##Splitting race for highest and normal A1C

new_db_data_weight_A1C_highest_race<- split(new_db_data_weight_A1C_highest,f=new_db_data_weight_A1C_highest$race)
new_db_data_weight_A1C_highest_race_caucasian <- data.frame(new_db_data_weight_A1C_highest_race$Caucasian)
new_db_data_weight_A1C_highest_race_africanamerican <- data.frame(new_db_data_weight_A1C_highest_race$africanamerican)
new_db_data_weight_A1C_normal_race <- split(new_db_data_weight_A1C_normal,f=new_db_data_weight_A1C_normal$race)
new_db_data_weight_A1C_normal_race_africanamerican <- data.frame(new_db_data_weight_A1C_normal_race$africanamerican)
new_db_data_weight_A1C_normal_race_caucasian <- data.frame(new_db_data_weight_A1C_normal_race$Caucasian)



################################################################################
# Understanding buyer persona 

db_data$persona <-paste(db_data$race,db_data$gender,db_data$age)
persona_df <- db_data %>% 
  group_by(race, gender, age) %>% 
  summarise(num_words = n()) %>% 
  arrange(desc(num_words))

################################################################################

## Effective Medicine analysis 

################################################################################

# A. Cleaning data
db_data1 <- db_data[-c(1,# encounter_id 
                       2,# patient_nbr
                       6,# weight
                       7,# admission_type_id
                       8,# discharge_disposition_id
                       9,# admission_source_id
                       11,# payer_code
                       13,# num_lab_procedures
                       40, #examide
                       41 #citoglipton
) ]

# Removing missing values
sum(is.na(db_data1))

new_data_med <- na.omit(db_data1)

# Replacing ? with NA - eliminating this will reduce the data size hence replacing with NA

col_list <- c(colnames(new_data_med))
new_data_med[col_list] <- lapply(new_data_med[col_list], gsub, pattern = "[?]", replacement = NA)

# Removing brackets in age column
new_data_med <- new_data_med %>%
  mutate(age = substr(age, 2, 6))

write.csv(new_data_med,"C:/Users/gmith/OneDrive/Desktop/Mithu/MSBA/Hackathon/Medicine.csv", row.names = FALSE)


# B. Massaging data

# Assumptions
## For analysis of medicine effectiveness - the individual patient details are not required.
## The above excluded columns are not considered for this analysis (weight dataset is explored separately)
## Column examide and citoglipton though medicines used for diabetes since all 
# values in the field are No are excluded for the analysis 
## Other columns are considered relevant irrespective of empty / null / NA values hence retained.
## 379 rows with NAs are removed
## New_data_med will be the clean data set that we work with further

summary(new_data_med)

is.character(new_data_med$metformin)
is.character(new_data_med$repaglinide)

# We see that medicine fields are in string format
# For the purpose our analysis we will assign value of 2 for Up since up means 
# the medicine has been increased for the patient
# We will assign value of -1 for Down since that implies decrease in medicine provided
# Steady will be assigned a value of 1 and No will be assigned a value of 0


A1C_corr <- new_data_med

nm1 <- c("metformin", "repaglinide", "nateglinide",	"chlorpropamide",	"glimepiride",	
         "acetohexamide",	"glipizide",	"glyburide",	"tolbutamide",	"pioglitazone",
         "rosiglitazone",	"acarbose",	"miglitol",	"troglitazone",	"tolazamide"
         ,	"insulin",	"glyburide-metformin",	"glipizide-metformin",	
         "glimepiride-pioglitazone", "metformin-rosiglitazone",	"metformin-pioglitazone")

A1C_corr[nm1] <- lapply(A1C_corr[nm1], gsub, pattern = "No", replacement = 0)
A1C_corr[nm1] <- lapply(A1C_corr[nm1], gsub, pattern = "Up", replacement = 2)
A1C_corr[nm1] <- lapply(A1C_corr[nm1], gsub, pattern = "Down", replacement = -1)
A1C_corr[nm1] <- lapply(A1C_corr[nm1], gsub, pattern = "Steady", replacement = 1)
A1C_corr[nm1] <- sapply(A1C_corr[nm1],as.numeric)

write.csv(A1C_corr,"C:/Users/gmith/OneDrive/Desktop/Mithu/MSBA/Hackathon/Medicine_massaged.csv", row.names = FALSE)

################################################################################
# Correlation

corr <- round(cor(A1C_corr[,17:37]), 1)

ggcorrplot(corr)

# Medicine interactions and success
#	The following medicines have positive interactions:
#o	Metformin and Glipzide - Existing Medicine
#o	Metformin and Glyburide - Existing Medicine
#o	Metformin and Rosiglitazone - Existing Medicine
#	The following medicines have negative interactions
#o	Glipzide and Glyburide
#o	Glipzide and Glimepiride
#o	Glyburide and Glimepiride
#o	Pioglitazone and Rosiglitazone

################################################################################
# Multiple Linear Regression using  A1C result

mean(A1C_corr$metformin)
median(A1C_corr$metformin)

A1C_results <- db_data %>% 
  filter(A1Cresult != 'None')

A1C_results[nm1] <- lapply(A1C_results[nm1], gsub, pattern = "No", replacement = 0)
A1C_results[nm1] <- lapply(A1C_results[nm1], gsub, pattern = "Up", replacement = 2)
A1C_results[nm1] <- lapply(A1C_results[nm1], gsub, pattern = "Down", replacement = -1)
A1C_results[nm1] <- lapply(A1C_results[nm1], gsub, pattern = "Steady", replacement = 1)
A1C_results[nm1] <- sapply(A1C_results[nm1],as.numeric)

A1C_results$dummy <- A1C_results$A1Cresult

A1C_results$dummy <- gsub(">7", "1", A1C_results$dummy)
A1C_results$dummy <- gsub(">8", "1", A1C_results$dummy)
A1C_results$dummy <- gsub("Norm", "1", A1C_results$dummy)
A1C_results$dummy <- gsub("None", "0", A1C_results$dummy)

A1C_results$dummy <- as.numeric(A1C_results$dummy)

A1C_reg_met <- lm(dummy ~ metformin, A1C_results)
A1C_reg_rep <- lm(dummy ~ repaglinide, A1C_results)
A1C_reg_nat <- lm(dummy ~ nateglinide, A1C_results)
A1C_reg_chl <- lm(dummy ~ chlorpropamide, A1C_results)
A1C_reg_glim <- lm(dummy ~ glimepiride, A1C_results)
A1C_reg_glip <- lm(dummy ~ glipizide, A1C_results)
A1C_reg_gly <- lm(dummy ~ glyburide, A1C_results)
A1C_reg_tol <- lm(dummy ~ tolbutamide, A1C_results)
A1C_reg_pio <- lm(dummy ~ pioglitazone, A1C_results)
A1C_reg_ros <- lm(dummy ~ rosiglitazone, A1C_results)
A1C_reg_acar <- lm(dummy ~ acarbose, A1C_results)
A1C_reg_mig <- lm(dummy ~ miglitol, A1C_results)
A1C_reg_ins <- lm(dummy ~ insulin, A1C_results)
A1C_reg_glymet <- lm(dummy ~ `glyburide-metformin`, A1C_results)
A1C_reg_glipmet <- lm(dummy ~ `glipizide-metformin`, A1C_results)
A1C_reg_rosmet <- lm(dummy ~ `metformin-rosiglitazone`, A1C_results)

reg_met <- tidy(A1C_reg_met)
reg_rep <- tidy(A1C_reg_rep)
reg_nat <- tidy(A1C_reg_nat)
reg_chl <- tidy(A1C_reg_chl)
reg_glim <- tidy(A1C_reg_glim)
reg_glip <- tidy(A1C_reg_glip)
reg_gly <- tidy(A1C_reg_gly)
reg_tol <- tidy(A1C_reg_tol)
reg_pio <- tidy(A1C_reg_pio)
reg_ros <- tidy(A1C_reg_ros)
reg_acar <- tidy(A1C_reg_acar)
reg_mig <- tidy(A1C_reg_mig)
reg_ins <- tidy(A1C_reg_ins)
reg_glymet <- tidy(A1C_reg_glymet)
reg_glipmet <- tidy(A1C_reg_glipmet)
reg_rosmet <- tidy(A1C_reg_rosmet)

regression_table <- bind_rows(reg_met, reg_rep, reg_nat, reg_chl, reg_glim, reg_glip, reg_glipmet,
                              reg_gly, reg_glymet, reg_ins, reg_mig, reg_pio, reg_ros, reg_rosmet, reg_tol, 
) %>% filter(term != "(Intercept)")

regression_table

# Metformin is effective on A1C - maintains steady
# Glyburide is effective on A1C - maintains steady

################################################################################

Med_up <- db_data1[apply(db_data1, 1, function(x) any (x == "Up")), ]

Med_up_rga <- Med_up %>% 
  mutate(age = substr(age, 2, 6)) %>% 
  filter(race != 'NA') %>% 
  filter(race != '?')

table(Med_up_rga$race)
table(Med_up_rga$age) # ignoring age since the distribution seems normal
table(Med_up_rga$gender)

ggplot(Med_up_rga, aes(x = age, fill = race)) + 
  geom_bar()

# We see that Caucasian data is highly represented followed by African Americans
# The data is also right skewed meaning 50 and above are in high risk category for getting diabetes
# 50+ is the age group with medicines are the increase scale - "UP" - they are potential target 
# Caucasian could be in high risk due to data bias. 

################################################################################

##BMI Calculations -- look at BMI File

################################################################################
# test<- new_data_med
# test$weight[test$weight=="?"]<- NA
# A1C_analysis<- na.omit(test)
# 
# # Creating the new variable "height"
# 
# A1C_analysis$height<- c()
# 
# #For loop to fill the "height" column
# 
# for (i in 1:nrow(A1C_analysis)){
#   if (A1C_analysis$age[i] == "[0-10)" & A1C_analysis$gender[i]=="Male"){
#     A1C_analysis$height[i] <- 138.4
#   }else if (A1C_analysis$age[i] == "[10-20)" & A1C_analysis$gender[i]=="Male"){
#     A1C_analysis$height[i] <- 177
#   }else if (A1C_analysis$age[i] == "[20-30)" & A1C_analysis$gender[i]=="Male"){
#     A1C_analysis$height[i] <- 176.1
#   }else if (A1C_analysis$age[i] == "[30-40)" & A1C_analysis$gender[i]=="Male"){
#     A1C_analysis$height[i] <- 176.1
#   }else if (A1C_analysis$age[i] == "[40-50)" & A1C_analysis$gender[i]=="Male"){
#     A1C_analysis$height[i] <- 175.8
#   }else if (A1C_analysis$age[i] == "[50-60)" & A1C_analysis$gender[i]=="Male"){
#     A1C_analysis$height[i] <- 175.8
#   }else if (A1C_analysis$age[i] == "[60-70)" & A1C_analysis$gender[i]=="Male"){
#     A1C_analysis$height[i] <- 173.4
#   }else if (A1C_analysis$age[i] == "[70-80)" & A1C_analysis$gender[i]=="Male"){
#     A1C_analysis$height[i] <- 173.4
#   }else if (A1C_analysis$age[i] == "[80-90)" & A1C_analysis$gender[i]=="Male"){
#     A1C_analysis$height[i] <- 173.4
#   }else if (A1C_analysis$age[i] == "[90-100)" & A1C_analysis$gender[i]=="Male"){
#     A1C_analysis$height[i] <- 173.4
#   }else if (A1C_analysis$age[i] == "[0-10)" & A1C_analysis$gender[i]=="Female"){
#     A1C_analysis$height[i] <- 138.4
#   }else if (A1C_analysis$age[i] == "[10-20)" & A1C_analysis$gender[i]=="Female"){
#     A1C_analysis$height[i] <- 162.3 
#   }else if (A1C_analysis$age[i] == "[20-30)" & A1C_analysis$gender[i]=="Female"){
#     A1C_analysis$height[i] <- 162.7
#   }else if (A1C_analysis$age[i] == "[30-40)" & A1C_analysis$gender[i]=="Female"){
#     A1C_analysis$height[i] <- 162.7
#   }else if (A1C_analysis$age[i] == "[40-50)" & A1C_analysis$gender[i]=="Female"){
#     A1C_analysis$height[i] <- 162.1
#   }else if (A1C_analysis$age[i] == "[50-60)" & A1C_analysis$gender[i]=="Female"){
#     A1C_analysis$height[i] <- 162.1
#   }else if (A1C_analysis$age[i] == "[60-70)" & A1C_analysis$gender[i]=="Female"){
#     A1C_analysis$height[i] <- 159.3
#   }else if (A1C_analysis$age[i] == "[70-80)" & A1C_analysis$gender[i]=="Female"){
#     A1C_analysis$height[i] <- 159.3
#   }else if (A1C_analysis$age[i] == "[80-90)" & A1C_analysis$gender[i]=="Female"){
#     A1C_analysis$height[i] <- 159.3
#   }else if (A1C_analysis$age[i] == "[90-100)" & A1C_analysis$gender[i]=="Female"){
#     A1C_analysis$height[i] <- 159.3
#   }else{
#     A1C_analysis$height[i] <- 0}
# }#closing the loop
# 
# # Creating a new variable "weight_Kg"
# 
# A1C_analysis$weight_Kg<- c()
# 
# # Filling the "weight_Kg" column
# 
# for (i in 1:nrow(A1C_analysis)){
#   if (A1C_analysis$weight[i] == "[0-25)"){
#     A1C_analysis$weight_Kg[i] <- 25
#   }else if (A1C_analysis$weight[i] == "[25-50)" ){
#     A1C_analysis$weight_Kg[i] <- 50
#   }else if (A1C_analysis$weight[i] == "[50-75)" ){
#     A1C_analysis$weight_Kg[i] <- 75
#   }else if (A1C_analysis$weight[i] == "[75-100)" ){
#     A1C_analysis$weight_Kg[i] <- 100
#   }else if (A1C_analysis$weight[i] == "[100-125)" ){
#     A1C_analysis$weight_Kg[i] <- 125
#   }else if (A1C_analysis$weight[i] == "[125-150)" ){
#     A1C_analysis$weight_Kg[i] <- 150
#   }else if (A1C_analysis$weight[i] == "[150-175)" ){
#     A1C_analysis$weight_Kg[i] <- 175
#   }else if (A1C_analysis$weight[i] == "[175-200)" ){
#     A1C_analysis$weight_Kg[i] <- 200
#   }else if (A1C_analysis$weight[i] == ">200" ){
#     A1C_analysis$weight_Kg[i] <- 225
#   }else{
#     A1C_analysis$weight_Kg[i] <- NA }
# }#closing the loop
# 
# # Creating a new variable "BMI"
# 
# A1C_analysis <- A1C_results %>% 
#   filter(A1Cresult != 'None') %>%
#   filter(gender != 'None')
# 
# A1C_analysis$A1C_numeric <- c()
# ##numeric value for A1C
# for (i in 1:nrow(A1C_analysis)){
#   if (A1C_analysis$A1Cresult[i] == ">7"){
#     A1C_analysis$A1C_numeric[i]<- 1
#   }else if (A1C_analysis$A1Cresult[i] == ">8" ){
#     A1C_analysis$A1C_numeric[i] <- 1
#   }else if (A1C_analysis$A1Cresult[i] == "Norm" ){
#     A1C_analysis$A1C_numeric[i] <- 0 
#   }else{
#     A1C_analysis$A1Cresult[i] <- NA }  
# }#closing the loop  
# 
# 
# A1C_analysis$BMI<- (A1C_analysis$weight_Kg)/(A1C_analysis$height * A1C_analysis$height/10000) 
# 

################################################################################

##BMI statistical test -- look at BMI dataset
# 
# ################################################################################
# 
# #Linear regression
# 
# linear<-glm(A1C_numeric~BMI,data=A1C_analysis)
# 
# summary(linear)
# 
# #correlation
# 
# cor.test(A1C_analysis$A1C_numeric,A1C_analysis$BMI)
# 
# # Creating the plot
# plot(A1C_analysis$A1C_numeric, A1C_analysis$BMI, pch = 19, col = "lightblue")
# 
# # Regression line
# abline(lm(A1C_analysis$BMI ~ A1C_analysis$A1C_numeric), col = "red", lwd = 3)
# 
# # Pearson correlation
# text(paste("Correlation:", round(cor(test1$A1C_numeric, test1$BMI), 2)), x = 25, y = 95)


################################################################################

##A1C Analysis

################################################################################


A1C_analysis <- A1C_results %>% 
  filter(A1Cresult != 'None') %>%
  filter(gender != 'None')

normal <- function(var1){
  normalized <-(var1-min(var1))/(max(var1)-min(var1))
  return(normalized)  
}#closing the normal UDF

#Numeric value for male/female
for (i in 1:nrow(A1C_analysis)){
  if (A1C_analysis$gender[i] == "Female"){
    A1C_analysis$gender_numeric[i]<- 1
  }else if (A1C_analysis$gender[i] == "Male" ){
    A1C_analysis$gender_numeric[i] <- 0
  }else{
    A1C_analysis$A1Cresult[i] <- NA }  
}#closing the loop  

##Normalizing the data
A1C_analysis$A1C_norm <- normal(var1=A1C_analysis$A1C_numeric)
A1C_analysis$admission_type_id_norm <- normal(var1=A1C_analysis$admission_type_id)
A1C_analysis$discharge_disposition_id_norm <- normal(var1=A1C_analysis$discharge_disposition_id)
A1C_analysis$weight_norm <- normal(var1=A1C_analysis$weight)
A1C_analysis$admission_source_id_norm <- normal(var1=A1C_analysis$admission_source_id)
A1C_analysis$time_in_hospital_norm <- normal(var1=A1C_analysis$time_in_hospital)
A1C_analysis$num_lab_procedures_norm <- normal(var1=A1C_analysis$num_lab_procedures)
A1C_analysis$num_medications_norm <- normal(var1=A1C_analysis$num_medications)
A1C_analysis$number_outpatient_norm <- normal(var1=A1C_analysis$number_outpatient)
A1C_analysis$number_emergency_norm <- normal(var1=A1C_analysis$number_emergency)
A1C_analysis$number_inpatient_norm <- normal(var1=A1C_analysis$number_inpatient)
A1C_analysis$diag_1_norm <- normal(var1=A1C_analysis$diag_1_norm)
A1C_analysis$diag_2_norm <- normal(var1=A1C_analysis$diag_2_norm)
A1C_analysis$diag_3_norm <- normal(var1=A1C_analysis$diag_3_norm)
A1C_analysis$insulin_norm <- normal(var1=A1C_analysis$insulin)
A1C_analysis$gender_norm <- normal(var1=A1C_analysis$gender_numeric)

##Train the model
train_index <- sample(1:nrow(A1C_analysis), size=0.8*nrow(A1C_analysis))

A1C_train <- A1C_analysis[train_index,]

#creating a test environment from subtracting the train environment 
A1C_test <- A1C_analysis[-train_index,]

#glm to predict binary
A1C_logit_total <- glm(A1C_numeric~time_in_hospital_norm +num_lab_procedures_norm+
                         num_medications_norm+number_outpatient_norm+number_emergency_norm
                       +number_inpatient_norm+insulin_norm+ gender_norm, data=A1C_train, family="binomial")

##Most important predictors include: 
A1C_logit <- glm(A1C_numeric~time_in_hospital_norm +num_lab_procedures_norm+
                   number_inpatient_norm+insulin_norm
                 + gender_norm, data=A1C_train, family="binomial")


summary(A1C_logit_total)

A1C_prediction_training <- predict(A1C_logit, A1C_train, type ="response")
confusionMatrix(data= as.factor(as.numeric(A1C_prediction_training > 0.5)),
                reference= as.factor(as.numeric(A1C_train$A1C_numeric)))

A1C_pred_logit <- prediction(A1C_prediction_training, A1C_train$A1C_numeric)

A1C_perf_logit <- performance(A1C_pred_logit, "tpr", "fpr")

plot(A1C_perf_logit)

##Gini Tree 
A1C_gini_tree <- rpart(A1C_numeric~time_in_hospital +num_lab_procedures+
                         number_inpatient+insulin
                       + gender + race + age, data=A1C_train, method = "class",
                       cp= 0.00036)

rpart.plot(A1C_gini_tree, extra=1, type=0)

sum(A1C_analysis$A1C_numeric)

#Gini Tree- Gender and Age
##Gini Tree 

A1C_gini_tree_age_gender <- rpart(A1C_numeric~gender+age+race, data=A1C_train, method = "class",
                                  cp= 0.000005)

rpart.plot(A1C_gini_tree_age_gender, extra=1, type=0)


nrow(A1C_analysis)
##prediction 

A1C_predict_test <- predict(A1C_gini_tree, A1C_test, type="prob")
A1C_predict_train <- predict(A1C_gini_tree, A1C_train, type="prob")

A1C_tree_prediction <- prediction(A1C_predict_train[,2], A1C_train$A1C_numeric)

A1C_tree_performance <- performance(A1C_tree_prediction, "tpr", "fpr")

plot(A1C_tree_performance, col="black")
plot(A1C_perf_logit, col="purple", add=TRUE)

write.csv(A1C_analysis,"/Users/karleywebster/Documents/Education/Hackathon/DataSets/dataset_diabetes/A1C.csv", row.names = FALSE)

################################################################################

##Graphs

################################################################################

#scatter plot comparing weight and number of medications
ggscatter(new_db_data_weight, x = "weight", y = "num_medications", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Weight", ylab = "Number of Medications")

#comparing reasons for coming into hospital with number of medications
ggscatter(new_db_data_weight, x = "admission_source_id", y = "num_medications", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "reason for coming into hospital", ylab = "Number of Medications")

#comparing number of lab procedures with number of medications -- is there a slight bias??
ggscatter(new_db_data_weight, x = "num_procedures", y = "num_medications", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "number of medical procedures", ylab = "Number of Medications")

#gg plot- weight vs number of diagnoses compares with lab procedures
ggplot(data=new_db_data_weight, aes(x=weight, y=number_diagnoses, color=num_lab_procedures)) + geom_jitter()

##Comparing results for those with highest A1C
ggplot(data=new_db_data_weight_A1C_highest, aes(x=num_medications, y=time_in_hospital)) + geom_jitter()

##Highest A1C- scatter plot-- number of medications compared to time in hospital
ggscatter(new_db_data_weight_A1C_highest, x = "num_medications", y = "time_in_hospital", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "number of medications", ylab = "Time in hospital")

##Normal A1C- scatter plot-- number of medications compared to time in hospital
ggscatter(new_db_data_weight_A1C_normal, x = "num_medications", y = "time_in_hospital", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "number of medications", ylab = "Time in hospital")

##Normal A1C- scatter plot- Gender split number of medications compared to time in hospital

##female
ggscatter(new_db_data_weight_A1C_normal_gender_female, x = "num_medications", y = "time_in_hospital", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "number of medications", ylab = "Time in hospital")

##male
ggscatter(new_db_data_weight_A1C_normal_gender_male, x = "num_medications", y = "time_in_hospital", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "number of medications", ylab = "Time in hospital")

##High A1C- scatter plot- Race split number of medications compared to time in hospital
#African American -- NO DATA POINTS
ggscatter(new_db_data_weight_A1C_normal_race_africanamerican, x = "num_medications", y = "time_in_hospital", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "number of medications", ylab = "Time in hospital")
#Caucasian
ggscatter(new_db_data_weight_A1C_normal_race_caucasian, x = "num_medications", y = "time_in_hospital", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "number of medications", ylab = "Time in hospital")



