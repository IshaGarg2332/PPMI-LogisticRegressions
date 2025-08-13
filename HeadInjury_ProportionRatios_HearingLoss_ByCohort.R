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
library("survival")


#Tests association between clinician-documented hearing loss and case status, adjusting for age and sex
#Tests association between self-reported hearing loss and case status, adjusting for age and sex
#Tests whether hearing loss before age 13 is associated with case status, adjusting for sex
#Tests joint and interactive effects of objective and self-reported hearing loss on case status, adjusting for age and sex



#Import datasets
Participant <- read_csv("Library/Mobile Documents/com~apple~CloudDocs/PPMI - Gregory Scott's files/Data/Subject_Characteristics/Participant_Status_13Jun2024.csv")
FOUND_RFQ_Head_Injury_13Jun2024 <- read_csv("Library/Mobile Documents/com~apple~CloudDocs/PPMI - Gregory Scott's files/Data/FOllow_Up_persons_w_Neurologic_Disease/FOUND_RFQ_Head_Injury_13Jun2024.csv")
Medical_Conditions_Log_13Jun2024 <- read_csv("Library/Mobile Documents/com~apple~CloudDocs/PPMI - Gregory Scott's files/Data/Medical_History/Medical_Conditions_Log_13Jun2024.csv")
Demographics <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/PPMI - Gregory Scott's files/Data/Subject_Characteristics/Demographics_13Jun2024.csv")

#Rename datasest names
HeadInjury <- FOUND_RFQ_Head_Injury_13Jun2024
tmp <- HeadInjury

#Rename column
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"

#Merge data
tmp2 <- merge(Participant,tmp,by="PATNO",all.x=T)

#Form hearing loss data
hloss <- Medical_Conditions_Log_13Jun2024[grepl("(hear\\b|hearing)",Medical_Conditions_Log_13Jun2024$MHTERM,ignore.case = T),]
hloss <- hloss[!is.na(hloss$MHDIAGDT),]
hloss$MHTERM <- tolower(hloss$MHTERM)
hloss$MHTERM <- str_squish(hloss$MHTERM)
hloss <- hloss[!grepl("(neuroma|tumor)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[!grepl("(implant|eustachian|calcification)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[hloss$MHTERM != "hearing",]
#hloss <- hloss[!grepl("(decreas)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[!grepl("(asymm|left|right|unilat| l | r )",hloss$MHTERM,ignore.case=T),]

#Merge the two together
cohToTest <- merge(tmp2,hloss,by="PATNO",all.x=T)

#Keep only the ones where they were diagnosed with hearing loss at least a year prior to diagnosis of Parkinson's disease
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest$HLDate <- as.Date(paste0("01/",cohToTest$MHDIAGDT),format="%d/%m/%Y")
cohToTest$PDdate <- as.Date(paste0("01/",cohToTest$PDDXDT),format="%d/%m/%Y")

cohToTest$hlDiff <- ifelse(is.na(cohToTest$PDdate),cohToTest$EnrollDate - cohToTest$HLDate,cohToTest$PDdate - cohToTest$HLDate)
cohToTest$hlBefore <- ifelse(!is.na(cohToTest$MHTERM),ifelse(cohToTest$hlDiff > (-365 * 1) ,T,F),NA)

#Remove the types of hearing loss
cohToTest$bilat <- ifelse(grepl("(bilateral|b\\/l|both ears)",cohToTest$MHTERM,ignore.case=T),T,F)
cohToTest$unilat <- ifelse(grepl("(asymm|left|right|unilat)",cohToTest$MHTERM,ignore.case=T),T,F)
cohToTest$loss <- ifelse(grepl("(loss)",cohToTest$MHTERM,ignore.case=T),T,F)
cohToTest$impair <- ifelse(grepl("(impair|defici)",cohToTest$MHTERM,ignore.case=T),T,F)
cohToTest$decr <- ifelse(grepl("(decreas|reduc|hardness)",cohToTest$MHTERM,ignore.case=T),T,F)
cohToTest$anyHL <- ifelse(cohToTest$bilat | cohToTest$unilat | cohToTest$loss | cohToTest$impair | cohToTest$decr,T,F)
cohToTest$aid <- ifelse(grepl("(aid|device)",cohToTest$MHTERM,ignore.case=T),T,F)
cohToTest$sensori <- ifelse(grepl("(sensory|sensori)",cohToTest$MHTERM,ignore.case=T),T,F)
cohToTest$slight <- ifelse(grepl("(slight|mild|partial)",cohToTest$MHTERM,ignore.case=T),T,F)
cohToTest$moder <- ifelse(grepl("(moderate|chronic)",cohToTest$MHTERM,ignore.case=T),T,F)
cohToTest$tinnitus <- ifelse(grepl("(tinitis|tinnitus)",cohToTest$MHTERM,ignore.case=T),T,F)

# Flag hearing loss based on hlBefore
cohToTest$hl <- ifelse(is.na(cohToTest$hlBefore), FALSE, cohToTest$hlBefore)

# Clean up HIQ1 values (9999 = missing)
cohToTest$hiq1 <- ifelse(is.na(cohToTest$hiq1), NA, ifelse(cohToTest$hiq1 == 9999, NA, cohToTest$hiq1))

# Create binary variable for reported hearing loss
cohToTest$hiqYes <- ifelse(cohToTest$hiq1 == 1, TRUE, FALSE)

# Flag PD or prodromal participants
cohToTest$ID <- ifelse(grepl("Park", cohToTest$COHORT_DEFINITION, ignore.case=TRUE), TRUE, FALSE)

#Merge datasets
cohToTest <- merge(cohToTest,Demographics,by="PATNO",all.x=T)

# Flag participants with hearing issues before age 13
cohToTest$testHiqAge <- ifelse(cohToTest$hiqa1_age <= 13,T,F)



# Hearing loss (binary) and covariates
res <- clogit(ID ~ hlBefore + ENROLL_AGE + SEX, cohToTest)
summary(res)

# Self-reported hearing loss (HIQ) and covariates
res <- clogit(ID ~ hiqYes + ENROLL_AGE + SEX, cohToTest)
summary(res)

# Early-onset hearing issues (before age 13) and sex
res <- clogit(ID ~ testHiqAge + SEX, cohToTest)
summary(res)

# Combined model: HIQ, objective HL, interaction, and covariates
res <- clogit(ID ~ hiqYes + hlBefore + hiqYes * hlBefore + ENROLL_AGE + SEX, cohToTest)
summary(res)
