# include

library(tidyverse)
library(lubridate)
library(edwr)

dir_raw <- "data/raw/megan"

# step 1 -----------------------------------------------
# run MBO query:
#   * Patients - By Medication (Generic) - Administration Date
#       - Medication (Generic): albuterol
#       - Facility (Curr): HC Children's
#       - Date Only - Admit

# filter data
raw_patients <- read_data(dir_raw, "patients", FALSE) %>%
    as.patients() %>%
    filter(age <= 17,
           visit.type != "Outpatient",
           visit.type != "Emergency",
           discharge.datetime <= mdy("12/31/2017", tz = "US/Central"))

mbo_pie <- concat_encounters(raw_patients$millennium.id)

# step 2 -----------------------------------------------
# limit to patients receiving albuterol between desired date ranges

# run MBO query
#   * Medications - Inpatient - All

raw_meds <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt()

meds_albuterol <- raw_meds %>%
    filter(med == "albuterol",
           (med.datetime >= mdy("9/22/2016", tz = "US/Central") &
                med.datetime <= mdy("10/19/2016", tz = "US/Central")) |
               (med.datetime >= mdy("9/22/2017", tz = "US/Central") &
                    med.datetime <= mdy("10/19/2017", tz = "US/Central")))

mbo_alb <- concat_encounters(unique(meds_albuterol$millennium.id))

# step 3 -----------------------------------------------
# run the following MBO queries:
#   * Demographics
#   * Diagnosis Codes (ICD-9/10-CM) - All
#   * Measures
#   * Vitals - BP

# find ashtma patients; ICD 10 = J45.*

diag_asthma <- read_data(dir_raw, "diagnosis", FALSE) %>%
    as.diagnosis() %>%
    filter(diag.type == "FINAL",
           str_detect(diag.code, "J45"))

pts_asthma <- raw_patients %>%
    semi_join(diag_asthma, by = "millennium.id") %>%
    mutate(group = year(discharge.datetime)) %>%
    select(millennium.id, group)

raw_demographics <- read_data(dir_raw, "demographics", FALSE) %>%
    as.demographics()

raw_measures <- read_data(dir_raw, "measures", FALSE) %>%
    as.events(order_var = FALSE)

pts_all <- raw_demographics %>%
    left_join(pts_asthma, by = "millennium.id")

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
