# include

library(tidyverse)
library(lubridate)
library(edwr)

dir_raw <- "data/raw/mbo"

# step 1 -----------------------------------------------
# run MBO query:
#   * Patients - By Medication (Generic)
#       - Medication (Generic): dexamethasone, methylprednisOLONE, predniSONE,
#       prednisoLONE
#       - Facility (Curr): HC Children's
#       - Date Only - Admit

# filter data
raw_patients <- read_data(dir_raw, "patients", FALSE) %>%
    as.patients() %>%
    filter(age >= 4, age <= 17,
           visit.type != "Outpatient", visit.type != "Emergency",
           discharge.datetime <= mdy_hms("09/30/2016 23:59:59", tz = "US/Central"))

mbo_encounter <- concat_encounters(raw_patients$millennium.id)

# step 2 -----------------------------------------------
# screen for diagnosis codes of acute asthma exacerbation

# run the following queries:
#   * Diagnosis Codes (ICD-9/10-CM)

ref_icd9 <- c("493.92", "493.02", "493.12", "493.22", "493.01", "493.11",
              "493.21", "493.91")

ref_icd10 <- c("J45.901", "J45.21", "J45.31", "J45.41", "J45.51", "J45.22",
               "J45.32", "J45.42", "J45.52", "J45.902")

eligible_pts <- read_data(dir_raw, "diagnosis", FALSE) %>%
    as.diagnosis() %>%
    filter((diag.code %in% ref_icd9 & code.source %in% c("ICD-9-CM", "ICD9")) |
               (diag.code %in% ref_icd10 & code.source %in% c("ICD-10-CM", "ICD10-CM"))) %>%
    distinct(millennium.id)

mbo_eligible <- concat_encounters(eligible_pts$millennium.id)

write_rds(eligible_pts, "data/final/eligible.Rds", compress = "gz")

# step 3 -----------------------------------------------
# get main data files

# run the following MBO queries:
#   * Clinical Events - Prompt
#       - RC Acc Muscle Pre Asthma Assess; RC Air Exch Pre Asthma Assess; RC Pre
#       Asthma Assess Total; RC Resp Rate Pre Asthma Assess; RC Rm Air O2 Sat
#       Pre Asthma Assess; RC Wheezes Pre Asthma Assess; Asthma Treatment
#       Recommended; RC Asthma Treatment Recommended; RC Air Exch Post Asthma
#       Assess; RC Resp Rate Post Asthma Assess; RC Rm Air O2 Sat Post Asthma
#       Assess; RC Acc Muscle Post Asthma Assess; RC Post Asthma Assess Total;
#       RC Wheezes Post Asthma Assess
#   * Demographics
#   * Labs - Pregnancy
#   * Location History
#   * Measures
#   * Medications - Inpatient - Prompt
#       - Medication (Generic): albuterol, racepinephrine, dexamethasone,
#       methylPREDNISolone, predniSONE, prednisoLONE

# run the following EDW queries:
#   * Output - Emesis
#       - Millennium Encounter ID: values from mbo_eligible
#       - PowerInsight Encounter Id: 1
#   * Identifiers
#       - Millennium Encounter ID: values from mbo_eligible
#       - PowerInsight Encounter Id: 1
#       - Formatted Financial Nbr: 1
#       - Person ID: 1

# step 4 -----------------------------------------------
# make list of Person ID and PIE

raw_person <- read_data(dir_raw, "identifiers") %>%
    as.id() %>%
    distinct(person.id)

edw_person <- concat_encounters(raw_person$person.id)

# run the following EDW queries:
#   * Encounters - by Person ID
#       - Person ID: value from edw_person

# step 5 -----------------------------------------------
# get order actions to use for continuous albuterol discontinue time

raw_albuterol <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt() %>%
    filter(!is.na(event.tag),
           med == "albuterol") %>%
    distinct(order.id)

mbo_order <- concat_encounters(raw_albuterol$order.id)

# run the MBO query:
#   * Orders - Actions - by Order Id
