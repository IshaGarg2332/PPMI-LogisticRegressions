# --------------------------------------------------
# NOTE: This script requires private datasets that are not included due to confidentiality. 
# Replace the paths with your own files structured similarly to the expected data frames.
# --------------------------------------------------

#Import libraries
library("dplyr")
library("tidyverse")
library("ggplot2")
library("survival")


#Whether people who had PTSD are more likely to be in the Parkinson’s-related groups (Parkinson’s or Prodromal) than in the non-Parkinson’s groups (Healthy Control or SWEDD)
#The coefficient for trauma tells you the direction of the association


#Load in the csvs
ParticipantStatus <- read_csv("data/ParticipantStatus.csv")
MedConditions <- read_csv("data/MedConditions.csv")
PDDiagHistory <- read.csv("data/PDDiagHistory.csv")
Demo <- read_csv("data/Demographics.csv")

#### PTSD ####
#Merge ParticipantStatus and PDDiagHistory
cohToTest1 <- ParticipantStatus
cohToTest1 <- cohToTest1[!is.na(cohToTest1$ENROLL_AGE),]
cohToTest1$EnrollDate <- as.Date(paste0("01/",cohToTest1$ENROLL_DATE),format="%d/%m/%Y")
cohToTest1 <- merge(cohToTest1,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

#Read data and filter for PTSD
PTSD <- read_csv("data/MedConditions.csv")
PTSD <- PTSD[grepl("(Post traumatic stress disorder\\b|PTSD\\b)",PTSD$MHTERM,ignore.case = T),]
PTSD <- PTSD[!is.na(PTSD$MHDIAGDT),]
PTSD$MHTERM <- tolower(PTSD$MHTERM)
PTSD$MHTERM <- str_squish(PTSD$MHTERM)

#Merge PTSD to rest of data
cohToTest1 <- merge(cohToTest1,PTSD,by="PATNO",all=T)

#Calculate date differences between PTSD and Parkinson's diagnosis dates
cohToTest1$HLDate <- as.Date(paste0("01/",cohToTest1$MHDIAGDT),format="%d/%m/%Y")
cohToTest1$PDdate <- as.Date(paste0("01/",cohToTest1$PDDXDT),format="%d/%m/%Y")
cohToTest1$hlDiff <- ifelse(is.na(cohToTest1$PDdate),cohToTest1$EnrollDate - cohToTest1$HLDate,cohToTest1$PDdate - cohToTest1$HLDate)
cohToTest1$trauma <- ifelse(!is.na(cohToTest1$MHTERM),ifelse(cohToTest1$hlDiff > (-365 * 1) ,T,F),NA)

#Add grouping variables
cohToTest1$AgeCut <- cut(cohToTest1$ENROLL_AGE,breaks=c(0,60,80,120))
cohToTest1$HLBfCut <- cut(as.numeric(cohToTest1$hlDiff),breaks=c(-99999,-365,0,365,99999),na.rm=T)
cohToTest1$ID <- ifelse(grepl("Park|prodro",cohToTest1$COHORT_DEFINITION,ignore.case=T),T,F)
cohToTest1 <- merge(cohToTest1,Demo,by="PATNO",all.x=T)

#Build PTSD dataset
PTSD <- select(cohToTest1, c(HLDate, PDdate, hlDiff, SEX, ENROLL_AGE, trauma, COHORT_DEFINITION, PATNO))

#Remove NA values from dataframe
PTSD <- PTSD[!is.na(PTSD$trauma),]
PTSD <- select(PTSD, c(trauma, PATNO, COHORT_DEFINITION))

#Assign binary outcome
PTSD <- PTSD %>%
  mutate(ID=case_when(
    COHORT_DEFINITION == "Parkinson's Disease" ~ 1,
    COHORT_DEFINITION == "Prodromal" ~ 1,
    COHORT_DEFINITION == "Healthy Control" ~ 0,
    COHORT_DEFINITION == "SWEDD" ~ 0
  ))

#Run logistic regression
PTSD <- glm(ID ~ as.numeric(trauma),
            data = PTSD, 
            family = "binomial")
summary(PTSD)
