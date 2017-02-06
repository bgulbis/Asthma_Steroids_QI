# include

library(tidyverse)
library(lubridate)
library(edwr)

dir_raw <- "data/raw"

# step 1 -----------------------------------------------
# run MBO query:
#   * Patients - By Medication (Generic)
#       - Medication (Generic): dexamethasone, methylprednisOLONE, predniSONE, prednisoLONE
#       - Facility (Curr): HC Children's
#       - Date Only - Admit

# filter data
raw_patients <- read_data(dir_raw, "patients") %>%
    as.patients(FALSE, tzone = "UTC") %>%
    filter(age >= 4, age <= 17,
           visit.type != "Outpatient", visit.type != "Emergency",
           discharge.datetime <= mdy_hms("09/30/2016 23:59:59", tz = "US/Central"))

# edw_pie <- concat_encounters(raw_patients$pie.id)
mbo_pie <- concat_encounters(raw_patients$millennium.id)

# step 2 -----------------------------------------------
# screen for diagnosis codes of acute asthma exacerbation

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

write_rds(eligible_pie, "data/final/eligible.Rds", compress = "gz")

# step 3 -----------------------------------------------
# run the following queries:
#   * Clinical Events - Prompt
#       - RC Acc Muscle Pre Asthma Assess; RC Air Exch Pre Asthma Assess; RC Pre Asthma Assess Total; RC Resp Rate Pre Asthma Assess; RC Rm Air O2 Sat Pre Asthma Assess; RC Wheezes Pre Asthma Assess; Asthma Treatment Recommended; RC Asthma Treatment Recommended; RC Air Exch Post Asthma Assess; RC Resp Rate Post Asthma Assess; RC Rm Air O2 Sat Post Asthma Assess; RC Acc Muscle Post Asthma Assess; RC Post Asthma Assess Total; RC Wheezes Post Asthma Assess
#   * Demographics
#   * Identifiers - by PowerInsight Encounter Id
#   * Labs - Pregnancy
#   * Location History
#   * Measures (Height and Weight)
#   * Medications - Inpatient Continuous - All
#   * Medications - Inpatient Continuous - Prompt
#       - Clinical Event: albuterol
#   * Medications - Inpatient Intermittent - Prompt
#       - Clinical Event: racepinephrine
#   * Medications - Inpatient Intermittent with Frequency - Prompt
#       - Clinical Event: dexamethasone, predniSONE, prednisoLONE
#   * Vomiting Output

raw_demographics <- read_data(dir_raw, "demographics") %>%
    as.demographics()

edw_person <- concat_encounters(raw_demographics$person.id)

# step 4 -----------------------------------------------
# run the following queries:
#   * Encounters - by Person ID

raw_meds_sched <- read_data(dir_raw, "meds_sched") %>%
    as.meds_sched()

ref <- tibble(name = "albuterol", type = "med", group = "cont")

raw_albuterol <- read_data(dir_raw, "meds_cont_asthma") %>%
    as.meds_cont() %>%
    tidy_data(ref, raw_meds_sched) %>%
    distinct(order.id)

edw_order <- concat_encounters(raw_albuterol$order.id)

# step 5 -----------------------------------------------
# run the following queries:
#   * Orders - from Clinical Event Id - Prompt
