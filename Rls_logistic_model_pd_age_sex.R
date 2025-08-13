# --------------------------------------------------
# NOTE: This script requires private datasets that are not included due to confidentiality. 
# Replace the paths with your own files structured similarly to the expected data frames.
# --------------------------------------------------

#Import libraries
library("readxl")
library("tidyr")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("readxl")
library("boot")
library("table1")
library("Hmisc")
library("sjmisc")
library("gt")

#This table shows the results of a logistic regression model that analyzed the likelihood of having Restless Leg Syndrome (RLS) based on:
#      Parkinson's status (Parkinson’s Disease or Prodromal vs. Healthy Controls)
#      Age at enrollment
#      Sex



#Import data
ParticipantStatus <- read_csv("data/ParticipantStatus.csv")
PDDiagHistory <- read.csv("data/PDDiagHistory.csv")
Demographics <- read_csv("data/Demographics.csv")
Demo <- read_csv("data/Demographics.csv")
MedConditions <- read.csv("data/MedConditions.csv")
MedConditions1 <- read.csv("data/MedConditions.csv")
MedConditions1 = MedConditions1[!duplicated(MedConditions1$PATNO),]
Ran <- merge(MedConditions1,ParticipantStatus[,c("PATNO","COHORT_DEFINITION")],by="PATNO",all.x=T)

#Combine ParticipantStatus and DiagHistory
cohToTest <- ParticipantStatus
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all=T)

### Sleep Problems ###
cohToTest <- ParticipantStatus
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all=T)

#Import sleep problems data
Sleep <- read_csv("data/MedConditions.csv")
Sleep <- Sleep[grepl("(sleep\\b|sleeping|somnia\\b|dream\\b|nightmare\\b|REM\\b)",Sleep$MHTERM,ignore.case = T),]
Sleep = Sleep[!duplicated(Sleep$PATNO),]

#Mutate restless leg syndrome data
RLS <- MedConditions[grepl("(Restless Leg Syndrome\\b|RLS\\b)",MedConditions$MHTERM,ignore.case = T),]
tmp <- RLS
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"
refff <- merge(ParticipantStatus,tmp,by="PATNO",all.y=T)

#Merge data
cohToTest1 <- merge(cohToTest,tmp,by="PATNO",all=T)
cohToTest2 <- merge(cohToTest1,Demographics,by="PATNO",all=T)
cohToTest2 <- merge(cohToTest2,MedConditions,by="PATNO",all.y=T)

Tes1 <- cohToTest2
Tes1[c("MHTERM.x")][is.na(Tes1[c("MHTERM.x")])] <- FALSE

Tes1 <- Tes1 %>%
  mutate(MHTER=case_when(
    MHTERM.x == FALSE ~ 0
  ))
Tes1[c("MHTER")][is.na(Tes1[c("MHTER")])] <- 1

MHTERMM <- Tes1$MHTER
Tes1 <- Tes1[order(-MHTERMM),]
Tes1 <- Tes1[!is.na(Tes1$COHORT_DEFINITION),]
Tes1 = Tes1[!duplicated(Tes1$PATNO),]

#Keep SWEDD as Healthy Controls
Tes1 <- Tes1 %>%
  mutate(COHORT_DEFINITION=case_when(
    COHORT_DEFINITION == "SWEDD" ~ "Healthy Control",
    COHORT_DEFINITION == "Parkinson's Disease" ~ "Parkinson's Disease",
    COHORT_DEFINITION == "Prodromal" ~ "Prodromal",
    COHORT_DEFINITION == "Healthy Control" ~ "Healthy Control"
  ))

#Parkinson's and Prodromal patients are TRUE and Healthy Control and SWEDD patients are FALSE
Tes2 <- Tes1 %>%
  mutate(ID=case_when(
    COHORT_DEFINITION == "Parkinson's Disease" ~ TRUE,
    COHORT_DEFINITION == "Prodromal" ~ TRUE,
    COHORT_DEFINITION == "Healthy Control" ~ FALSE,
    COHORT_DEFINITION == "SWEDD" ~ FALSE
  ))

#Keep certain columns
Tes2 <- select(Tes2, c(PATNO, SEX, COHORT_DEFINITION, ENROLL_AGE, HISPLAT, RAASIAN, RABLACK, RAHAWOPI, RAINDALS, RAWHITE, RANOS, RAUNKNOWN, ID, MHTER))

#Rename the column values with the correct race, sex, age, and hearing loss status
Tes2 <- Tes2 %>%
  mutate(Race=case_when(
    HISPLAT == 1 ~ "Hispanic/Latino",
    RAASIAN == 1 ~ "Asian",
    RABLACK == 1 ~ "Black/African American",
    RAHAWOPI == 1 ~ "Hawaiian/Other Pacific Islander",
    RAINDALS == 1 ~ "American Indian/Alaska Native",
    RAWHITE == 1 ~ "White",
    RANOS == 1 ~ "Unknown",
    RAUNKNOWN == 1 ~ "Unknown"
  ))
Tes2 <- Tes2 %>%
  mutate(Sex=case_when(
    SEX == 1 ~ "Male",
    SEX == 0 ~ "Female",
  ))
Tes2 <- Tes2 %>%
  mutate(Age=case_when(
    ENROLL_AGE <18 ~ "<18",
    ENROLL_AGE >=18 & ENROLL_AGE <24 ~ "18-24",
    ENROLL_AGE >=24 & ENROLL_AGE <36 ~ "24-36",
    ENROLL_AGE >=36 & ENROLL_AGE <42 ~ "36-42",
    ENROLL_AGE >=42 & ENROLL_AGE <56 ~ "42-56",
    ENROLL_AGE >=56 & ENROLL_AGE <66 ~ "56-66",
    ENROLL_AGE >=66 & ENROLL_AGE <76 ~ "66-76",
    ENROLL_AGE >=76 & ENROLL_AGE <86 ~ "76-86",
    ENROLL_AGE >=86 ~ ">86"
  ))
Tes2 <- Tes2 %>%
  mutate(rls=case_when(
    MHTER == 1 ~ 1,
    MHTER == 0 ~ 0
  ))

#Keep certain columns 
Tes2 <- select(Tes2, c(PATNO, Sex, COHORT_DEFINITION, ENROLL_AGE, Age, rls, Race, ID, MHTER, SEX))
colnames(Tes2)[which(names(Tes2) == "COHORT_DEFINITION")] <- "Parkinsons"
label(Tes2$ID) <- "Parkinson's or Healthy?"

#Order columns in a certain order for the table
Tes2 <- Tes2 %>%
  mutate(Race = factor(Race, levels=c("White", "Black/African American", "Hispanic/Latino", "Asian", "American Indian/Alaska Native", "Hawaiian/Other Pacific Islander", "Unknown")))
Tes2 <- Tes2 %>%
  mutate(Sex = factor(Sex, levels=c("Male", "Female")))
Tes2 <- Tes2 %>%
  mutate(Age = factor(Age, levels=c("24-36", "36-42", "42-56", "56-66", "66-76", "76-86", ">86")))
Tes2 <- Tes2 %>%
  mutate(Parkinsons = factor(Parkinsons, levels=c("Parkinson's Disease", "Prodromal", "Healthy Control", "SWEDD")))

table1(~ Race + Sex + Age + Parkinsons | rls, data=Tes2,
       title = "Demographics",
)

#Mutate values of the Parkinson's status
Tes2 <- Tes2 %>%
  mutate(ID=case_when(
    Parkinsons == "Parkinson's Disease" ~ 2,
    Parkinsons == "Prodromal" ~ 1,
    Parkinsons == "Healthy Control" ~ 0,
    Parkinsons == "SWEDD" ~ 0
  ))
Tes2 <- Tes2 %>%
  mutate(Parkinsons1=case_when(
    Parkinsons == "Parkinson's Disease" ~ "Parkinson's Disease",
    Parkinsons == "Prodromal" ~ "Prodromal",
    Parkinsons == "Healthy Control" ~ "Healthy Control"
  ))

Tes2$Parkinsons1 <- as.factor(Tes2$Parkinsons1)
Tes2 <- Tes2 %>%
  mutate(Parkinsons1 = factor(Parkinsons1, levels=c("Healthy Control", "Parkinson's Disease", "Prodromal")))

#Create Proportion Table
mod <- glm(rls ~ Parkinsons1 + ENROLL_AGE + SEX,
           data = Tes2,
           family = "binomial")
tab <- summary(mod)$coef

data <- data.frame(variable = rownames(tab),
                   oddsratio = round(exp(tab[,1]), 3),
                   ci_low = round(exp(tab[,1] - 1.96 * tab[,2]), 3),
                   ci_high = round(exp(tab[,1] + 1.96 * tab[,2]), 3),
                   pval = scales::pvalue(tab[,4]),
                   row.names = NULL)[-1,]
data1 <- data[-c(2,3), ]

label(data$ci_low) <- "Low Confidence Interval"
label(data$ci_high) <- "High Confidence Interval"
label(data$pval) <- "P-value"
label(data$oddsratio) <- "Odds Ratio"
label(data$variable) <- "Variable"
data <- data %>%
  mutate(variable=case_when(
    variable == "Parkinsons1Parkinson's Disease" ~ "Parkinson's",
    variable == "Parkinsons1Prodromal" ~ "Prodromal",
    variable == "ENROLL_AGE" ~ "Age",
    variable == "SEX" ~ "Sex",
  ))
label(data$variable) <- "Variables"

gt_tbl <- gt(data)
gt_tbl <-
  gt_tbl |>
  tab_header(
    title = "Summary Table for Restless Leg Syndrome",
  )
gt_tbl
