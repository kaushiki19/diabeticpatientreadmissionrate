###APPLIED MULTIVARIATE DATA ANALYSIS
###FINAL PROJECT SPRING 2021 - PART 1
###DATA CLEANING AND PREPROCESSING
###READMISSION PREDICTION OF DIABETES PATIENTS
###KAUSHIKI CHAUHAN

#####################################LIBRARY#############################################
library(GGally)
library(ggplot2)
library(corrplot)
library(tidyverse)
library(plyr)

#####################################DATASET IMPORT######################################

filename <- "C://Workspace//Rowan Class//Applied Multivariate Data Analysis//Final Project//Part1//dataset_diabetes//diabetic_data.csv"
df <- read.csv(filename, header = TRUE)
head(df)
names(df)
class(df)
dim(df)

##########################DATA CLEANING AND PREPROCESSING####################################

is.null(df)
sum(is.na(df))
str(df)
summary(df)

##Replace "?" with NA and then remove NA
# df[df == "?"] <- NA
# df <- na.omit(df)
# sum(is.na(df))
# dim(df)

#install.packages("tidyverse")
#library(tidyverse)
#glimpse(df2) 


prop.table(table(df$weight))

###New Preprocessed and cleaned dataframe 
###Removing unnecessary columns with more cardinality. Also, weight(96%) missing
###encounter_id is a patient id
###medical_speciality has 49949 = "?" and also high cardinality
###payer_code has 40256 = "?"
###examide and citoglipton have constant NO value
df2 <- select(df, -encounter_id, -patient_nbr, -weight, -payer_code, -diag_1, -diag_2, -diag_3,
              -medical_specialty, -examide, -citoglipton)
dim(df2)

#Race column - setting race = "?" to other
table(df$race)
#df2$race <- replace(df2$race, df2$race == "?", "Other")
df2 <- df2[df2$race != "?",]
table(df2$race)
str(df2)
sum(is.na(df2$race))
dim(df2)

#Gender column 
table(df$gender)
df2$gender <- ifelse(df2$gender == "Male", 1, 2)
table(df2$gender)

#Age column 
table(df$age)

df2$age <- as.numeric(df2$age)
table(df2$age)

#Removing columns having only NO values: acetohexamide, troglitazone, glipizide.metformin, 
#glimepiride.pioglitazone, metformin.rosiglitazone, metformin.pioglitazone
df2 <- select(df2, -acetohexamide, -tolbutamide, -troglitazone, 
              -glipizide.metformin, -glimepiride.pioglitazone, 
              -metformin.rosiglitazone, -metformin.pioglitazone)
dim(df2)


#max_glu_serum column: None and Norm = 0, >200 = 2, >300 = 3
table(df$max_glu_serum)
df2$max_glu_serum <- ifelse(df2$max_glu_serum == ">300", 3, 
                            ifelse(df2$max_glu_serum == ">200", 2, 0))
table(df2$max_glu_serum)

#A1Cresult column: None and Norm = 0, >7 = 2, >8 = 3
table(df$A1Cresult)
df2$A1Cresult <- ifelse(df2$A1Cresult == ">8", 3, 
                        ifelse(df2$A1Cresult == ">7", 2, 0))

table(df2$A1Cresult)

#Converting categorical column values No,Steady,Up,Down to numerical values
colnames <- c("metformin", "repaglinide",	"nateglinide", "chlorpropamide", "glimepiride",
              "glipizide", "glyburide", "pioglitazone", "rosiglitazone", "acarbose", 
              "miglitol", "tolazamide",	"insulin",	"glyburide.metformin")

for (col in colnames) {
  df2[,col] <- ifelse((df2[,col] == "No" | df2[,col] == "Steady"), 0,
                    ifelse(df2[,col] == "Down", 1, 2))
}

summary(df2[,colnames])

#Change column
table(df$change)

df2$change <- ifelse(df2$change == "Ch", 1, 0)
table(df2$change)

#DiabetesMed column
table(df$diabetesMed)

df2$diabetesMed <- ifelse(df2$diabetesMed == "Yes", 1, 0)
table(df2$diabetesMed)

#Readmitted column
##Representation of Readmitted within 30 days
##1 represents readmission within 30 days. 0 represents No readmission or readmission after 30 days
table(df$readmitted)
prop.table(table(df$readmitted))

df2$readmitted <- ifelse(df2['readmitted'] == "<30", 1, 0)
table(df2$readmitted)
prop.table(table(df2$readmitted))

#race column to numeric
table(df$race)
unique_a <- unique(df2$race)
length(unique_a)
df2$race <- as.numeric(factor(df2$race), levels=sort(unique_a))
table(df2$race)

summary(df2)

########################EXPLORATORY DATA ANALYSIS AND PLOTS###########################

###################Univariate Analysis On All Variables####################
par (mar=c (5,5,5,5) , # space around graph
     cex.main=2 , # title font Size
     cex.lab=1.5 , # axes labels Font
     cex.axis= 0.8 # Font Size for axes Numbers
)

par(mfrow = c(2,2))
#Gender distribution
gender <- prop.table(table(df2$gender))*100
barplot(gender, col = c("blue","orange"),
        ylim = c(0,60),
        main = "Gender Distribution",
        xlab = "Gender",
        ylab = "Proportion",
        las=2,
        names.arg = c("Male","Female"))

#Age Distribution
age <- prop.table(table(df$age))*100
barplot(age, col = c("blue"),
        #xlim = c(0,100),
        las = 2,
        ylim = c(0,30),
        main = "Age Distribution",
        xlab = "Age",
        ylab = "Proportion")

race <- prop.table(table(df$race)) * 100
barplot(race, col = c(2,3,4,5,6,7),
        #xlim = c(0,100),
        las = 1,
        ylim = c(0,80),
        main = "Race Distribution",
        xlab = "Race",
        ylab = "Proportion")

#Demographics Data Analysis of Patient based on Readmission


#race,gender,age distribution on readmission
par (mar=c(5,7,5,5) , # space around graph
     cex.main=1 , # title font Size
     cex.lab=1 , # axes labels Font
     cex.axis= 1 # Font Size for axes Numbers
)

par(mfrow=c(2,2))

readmission <- prop.table(table(df2$readmitted)) * 100
readmission <- readmission[c("1","0")]
barplot(readmission,
        col = c(3,4),
        ylim = c(0,100),
        main = "Readmission Within 30 Days",
        xlab = "Readmission Within 30 Days",
        ylab = "Proportion of Patients",
        names.arg = c("Yes","No"),
        args.legend = list(x = "topleft"),
        legend.text = c("Yes","No"))

unique(df$race)
race <- table(df2$readmitted,df2$race)
#race <- race[,2:6]
barplot(race, main = "Race distribution by Readmission",
        col = c("blue","green"),
        #xlab = "Race Readmission", ylab = "Number of Patients",
        axis.lty = 1,
        ylim = c(0, 80000),
        args.legend = list(x = "topleft"),
        names.arg = c("AfricanAmerican","Asian","Caucasian","Hispanic","Other"),
        legend.text = c("No","Yes"),
        las = 2,
        beside = FALSE)

gender <- table(df2$readmitted,df2$gender)
barplot(gender, main = "Gender distribution by Readmission",
        col = c("blue","green"), 
        names.arg = c("Male","Female"),
        #xlab = "Gender Readmission", ylab = "Number of Patients",
        axis.lty = 1,
        ylim = c(0, 90000),
        args.legend = list(x = "topleft"),
        legend.text = c("No","Yes"),
        las = 2,
        beside = FALSE)

age <- table(df2$readmitted,df2$age)
barplot(age, main = "Age distribution by Readmission",
        col = c("blue","green"), 
        names.arg = c("[0-10)","[10-20)","[20-30)","[30-40)","[40-50)",
                      "[50-60)","[60-70)","[70-80)","[80-90)","[90-100)"),
        #xlab = "Age Readmission", ylab = "Number of Patients",
        axis.lty = 1,
        ylim = c(0, 30000),
        args.legend = list(x = "topleft"),
        legend.text = c("No","Yes"),
        las =2,
        beside = FALSE)

graphics.off()

#Medication in Patient - Anything > 200 is diabetic patient
par(mfrow=c(2,2))
glucose <- table(df$max_glu_serum)
barplot(glucose,
        main = "Insulin Level",
        col = c(2,3,4,5), 
        xlab = "Glucose Indicator", ylab = "Number of Patients",
        axis.lty = 2,
        ylim = c(0, 100000))

#Blood Sugar level in blood - having >6.5 is diabetic
hbA1C <- table(df$A1Cresult)
barplot(hbA1C,
        main = "Blood Sugar Level in Blood",
        col = c(2,3,4,5), 
        xlab = "Blood Sugar Level", ylab = "Number of Patients",
        axis.lty = 2,
        ylim = c(0, 100000))

#Change in Medication
med <- table(df$change)
barplot(med,
        main = "Change in Medication",
        col = c(3,2), 
        xlab = "Medication Change", ylab = "Number of Patients",
        axis.lty = 2,
        names.arg = c("Yes","No"),
        ylim = c(0, 60000))

#Diabetes Medication
diab <- table(df$diabetesMed)
Newdiab <- diab[c(2,1)]
barplot(Newdiab,
        main = "Diabetes Medication of Patients",
        col = c(3,2), 
        xlab = "Diabetes Medication", ylab = "Number of Patients",
        axis.lty = 2,
        ylim = c(0, 80000))

graphics.off()

par (mar=c(5,5,5,5) , # space around graph
     cex.main=2 , # title font Size
     cex.lab=1.5 , # axes labels Font
     cex.axis= 1 # Font Size for axes Numbers
)
time_hos <- table(df2$readmitted, df2$time_in_hospital)
barplot(time_hos,
        main = "Patients Time in Hospital",
        col = c(3,4), 
        xlab = "Time in Hospital", ylab = "Number of Patients",
        axis.lty = 2,
        legend.text = c("No","Yes"),
        beside = TRUE,
        ylim = c(0, 20000))

admission_type <- table(df2$readmitted, df2$admission_type_id)
barplot(admission_type,
        main = "Admission Type in Hospiatls",
        col = c(3,4), 
        xlab = "Admission Type", ylab = "Number of Patients",
        axis.lty = 2,
        legend.text = c("No","Yes"),
        beside = TRUE,
        ylim = c(0, 50000))

admission_source_id <- table(df2$readmitted, df2$admission_source_id)
barplot(admission_source_id,
        main = "Admission Source in Hospitals",
        col = c(3,4), 
        xlab = "Admission Source", ylab = "Number of Patients",
        axis.lty = 2,
        legend.text = c("No","Yes"),
        beside = TRUE,
        ylim = c(0, 50000))

#columns number_outpatient, number_emergency, number_inpatient combined to total_patient
# df2$total_patient <- df2$number_outpatient + df2$number_emergency + df2$number_inpatient
# 
# dim(df2)
# df2 <- select(df2, -number_outpatient, -number_emergency, -number_inpatient)
# dim(df2)
# str(df2)


#######################WRITING CLEANED DATA TO CSV FILE########################
cleanfile <- "C://Workspace//Rowan Class//Applied Multivariate Data Analysis//Final Project//Part1//dataset_diabetes//cleaned_diabetic_data.csv"
write.csv(df2, file = cleanfile, row.names = FALSE)









