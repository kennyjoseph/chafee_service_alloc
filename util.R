library(data.table)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(fixest)
library(scales)
library(tidytext)
library(ggridges)
library(mgcv)
library(plyr); library(dplyr)

theme_set(theme_bw(20))

service_mapping = c("SpecEdSv"= "Special Education",
                    "ILNAsv"= "Independent Living Needs Assessment",
                    "AcSuppSv"= "Academic Support", 
                    "PSEdSuppSv"= "Post-Secondary Educational Support",
                    "CareerSv"= "Career Preparation",
                    "EmplyTrSv"= "Employment Programs Or Vocational", 
                    "BudgetSv"= "Budget And Financial Management",
                    "HousEdSv"= "Housing Education And Home", 
                    "HlthEdSv"= "Health Education And Risk Prevention", 
                    "FamSuppSv"= "Family Support And Healthy Marriage Education",
                    "MentorSv"= "Mentoring", 
                    "SILsv"= "Supervised Independent Living",
                    "RmBrdFASv"= "Room And Board Financial Assistance", 
                    "EducFinaSv"= "Education Financial Assistance", 
                    "OthrFinaSv"= "Other Financial Assistance")
service_cols <- names(service_mapping)



casegoals <- c("1"="Reunification",
               "2"="Live w/ Relatives",
               "3"="Adoption",
               "4"="Long Term Foster Care",
               "5"="Emancipation",
               "6"="Guardianship",
               "7"="Not Yet Established",
               "99"="Unknown/Null")

placement_setting <- c("1"="Pre-adoptive home",
                       "2"="Foster family home,relative",
                       "3"="Foster family home, non-relative",
                       "4"="Group home",
                       "5"="Institution",
                       "6"="Supervised independent living",
                       "7"="Runaway",
                       "8"="Trial home visit",
                       "99"="Unknown/Null")

factor_names <- c("shap_prev_AllServices_Binary" =	"Previous Year Services",
                  "shap_prev_YoungerServices_Binary" =	"Previous Year Younger Services",
                  "shap_log_age_at_end_int" =	"Log(Age at end of FY)",
                  "shap_St_PA" =	"State: PA",
                  "shap_St_KY" =	"State: KY",
                  "shap_St_TX"= "State: TX",
                  "shap_St_IL"= "State: IL",
                  "shap_St_GA"= "State: GA",
                  "shap_CurPlSet_6.0"= "Current Placement Setting: SIL",
                  "shap_St_IA"= "State: IA",
                  "shap_age_at_end_int"= "Age at end of FY",
                  "shap_St_TN"= "State: TN",
                  "shap_Sex"= "Sex: Female",
                  "shap_RU13"= "RU13 (Higher is More Rural)",
                  "shap_log_latest_removal_discharge_date_int"= "Log(Latest Removal Discharge Date)",
                  "shap_latest_removal_date_int"= "Latest Removal Date",
                  "shap_log_latest_removal_date_int"= "Log(Latest Removal Date)",
                  "shap_log_latest_setting_date_int"= "Log(Latest Setting Date)",
                  "shap_CurPlSet_7.0"= "Current Placement Setting: Runaway",
                  "shap_St_OH"= "State: OH",
                  "shap_age_17"= "Age 17",
                  "shap_St_FL"= "State: FL",
                  "shap_St_AZ"= "State: AZ",
                  "shap_St_WV"= "State: WV",
                  "shap_CaseGoal_4.0"= "Case Goal: Long Term Foster Care")

get_date <- function(date_col){
  anchor_date_num = 15248
  anchor_date = ymd("2001/9/30")
  date_col_converted <- anchor_date + days(date_col - anchor_date_num)
  date_col_converted[date_col == 0 ] <- NA
  return(date_col_converted)  
}

get_data <- function(filename){
  d <- fread(filename)
  
  d[, first_rem_date := get_date(Rem1Dt)]
  d[, latest_rem_date := get_date(LatRemDt)]
  d[, latest_setting_date := get_date(CurSetDt)]
  
  d[, latest_discharge_date := get_date(DoDFCDt)]
  d[, last_rem_discharge_date := get_date(DLstFCDt)]
  d[, periodic_review_date := get_date(PedRevDt)]
  
  return(d)
}


