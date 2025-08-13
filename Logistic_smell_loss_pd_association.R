# --------------------------------------------------
# NOTE: This script requires private datasets that are not included due to confidentiality. 
# Replace the paths with your own files structured similarly to the expected data frames.
# --------------------------------------------------

#Import libraries
library("dplyr")
library("tidyverse")
library("ggplot2")
library("survival")


#People with a diagnosed smell disorder were significantly more likely to have Parkinson’s or be in the prodromal group.
#The logistic regression showed a positive association between smell loss and PD status.


#Import datasets
ParticipantStatus <- read_csv("Library/Mobile Documents/com~apple~CloudDocs/PPMI - Gregory Scott's files/Data/Subject_Characteristics/Participant_Status_13Jun2024.csv")
MedConditions <- read_csv("Library/Mobile Documents/com~apple~CloudDocs/PPMI - Gregory Scott's files/Data/Medical_History/Medical_Conditions_Log_13Jun2024.csv")
PDDiagHistory <- read.csv("Library/Mobile Documents/com~apple~CloudDocs/PPMI - Gregory Scott's files/Data/Medical_History/PD_Diagnosis_History_13Jun2024.csv")
Demo <- read_csv("Library/Mobile Documents/com~apple~CloudDocs/PPMI - Gregory Scott's files/Data/Subject_Characteristics/Demographics_13Jun2024.csv")

#### Loss of Smell ####
#Mutate and merge datasets
cohToTest1 <- ParticipantStatus
cohToTest1 <- cohToTest1[!is.na(cohToTest1$ENROLL_AGE),]
cohToTest1$EnrollDate <- as.Date(paste0("01/",cohToTest1$ENROLL_DATE),format="%d/%m/%Y")
cohToTest1 <- merge(cohToTest1,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

#Filter smell data
Smell <- read_csv("Library/Mobile Documents/com~apple~CloudDocs/PPMI - Gregory Scott's files/Data/Medical_History/Medical_Conditions_Log_13Jun2024.csv")
Smell <- Smell[grepl("(smell\\b)",Smell$MHTERM,ignore.case = T),]
Smell <- Smell[!is.na(Smell$MHDIAGDT),]
Smell$MHTERM <- tolower(Smell$MHTERM)
Smell$MHTERM <- str_squish(Smell$MHTERM)

#Keep only the ones where they were diagnosed with a smell disorder at least a year prior to diagnosis of Parkinson's disease
cohToTest1 <- merge(cohToTest1,Smell,by="PATNO",all.x=T)
cohToTest1$HLDate <- as.Date(paste0("01/",cohToTest1$MHDIAGDT),format="%d/%m/%Y")
cohToTest1$PDdate <- as.Date(paste0("01/",cohToTest1$PDDXDT),format="%d/%m/%Y")
cohToTest1$hlDiff <- ifelse(is.na(cohToTest1$PDdate),cohToTest1$EnrollDate - cohToTest1$HLDate,cohToTest1$PDdate - cohToTest1$HLDate)
cohToTest1$smell <- ifelse(!is.na(cohToTest1$MHTERM),ifelse(cohToTest1$hlDiff > (-365 * 1) ,T,F),NA)

#Further mutate smell data
cohToTest1$AgeCut <- cut(cohToTest1$ENROLL_AGE,breaks=c(0,60,80,120))
cohToTest1$HLBfCut <- cut(as.numeric(cohToTest1$hlDiff),breaks=c(-99999,-365,0,365,99999),na.rm=T)
cohToTest1$ID <- ifelse(grepl("Park|prodro",cohToTest1$COHORT_DEFINITION,ignore.case=T),T,F)
cohToTest1 <- merge(cohToTest1,Demo,by="PATNO",all.x=T)
Smell <- select(cohToTest1, c(HLDate, PDdate, hlDiff, SEX, ENROLL_AGE, smell, COHORT_DEFINITION, PATNO))
Smell <- Smell[!is.na(Smell$smell),] 

#Select certain columns
Smell1 <- select(Smell, c(smell, PATNO, COHORT_DEFINITION))

#Mutate Parkinson's status where Parkinson's disease is 1 and heathy controls are 0
Smell1 <- Smell1 %>%
  mutate(ID=case_when(
    COHORT_DEFINITION == "Parkinson's Disease" ~ 1,
    COHORT_DEFINITION == "Prodromal" ~ 1,
    COHORT_DEFINITION == "Healthy Control" ~ 0,
    COHORT_DEFINITION == "SWEDD" ~ 0
  ))

#Get summary (p-value, odds ratio, and confidence interval) for intersection of smell disorders and Parkinson's disease
Smell <- glm(ID ~ as.numeric(smell),
             data = Smell1, 
             family = "binomial")
summary(Smell)
