# include

# source("0-library.R")

# run query:
#   * Patients - By Medication
#       - Clinical Event: dexamethasone, predniSONE, prednisoLONE
#       - Person Location- Facility (Curr): Memorial Hermann Children's Hospital
#       - Admit date: User-Defined

library(tidyverse)
library(lubridate)
library(edwr)

dir_raw <- "data/raw"
dir.save <- "data_save"

# take data from edw query for patients receiving either dexamethasone or
# prednisone/prednisolone at CMHH and filter to desired age range and admission
# time-frame
raw_patients <- read_data(dir_raw, "patients") %>%
    as.patients() %>%
    filter(age >= 4,
           age <= 17,
           discharge.datetime <= mdy_hms("09/30/2016 23:59:59"))

edw_pie <- concat_encounters(raw_patients$pie.id, 900)

# take the list of eligible patients and screen for diagnosis codes of acute
# asthma exacerbation

# run the following queries:
#   * Diagnosis Codes (ICD-9/10-CM) - All

ref_icd9 <- c("493.92", "493.02", "493.12", "493.22", "493.01", "493.11",
              "493.21", "493.91")

ref_icd10 <- c("J45.901", "J45.21", "J45.31", "J45.41", "J45.51", "J45.22",
               "J45.32", "J45.42", "J45.52", "J45.902")

eligible_pie <- read_data(dir_raw, "diagnosis") %>%
    as.diagnosis() %>%
    filter((diag.code %in% ref_icd9 & code.source == "ICD-9-CM") |
               (diag.code %in% ref_icd10 & code.source == "ICD-10-CM")) %>%
    distinct(pie.id)

edw_eligible <- concat_encounters(eligible_pie$pie.id)

write_rds(eligible_pie, "data/final/eligible.Rds")

# run the following queries:
#   * Demographics
#   * Labs - Pregnancy
#   * Location History
#   * Medications - Inpatient Intermittent - Prompt
#       - Clinical Event: racepinephrine
#   * Medications - Inpatient Intermittent with Frequency - Prompt
#       - Clinical Event: dexamethasone, predniSONE, prednisoLONE

raw_demographics <- read_data(dir_raw, "demographics") %>%
    as.demographics()

edw_person <- concat_encounters(raw_demographics$person.id)

# run the following queries:
#   * Encounters - by Person ID
