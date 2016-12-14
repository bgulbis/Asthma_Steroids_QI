# include

# library

library(BGTools)
library(dplyr)
library(tibble)
library(stringr)
library(tidyr)
library(lubridate)
library(readr)
library(purrr)

dir.patients <- "data_patients"
dir.save <- "data_save"

gzip_files(dir.patients)

# data for Shannan's QI project
# codes included: "493.92", "493.02", "493.12", "493.22", "493.01", "493.11", "493.21", "493.91"

dir.shannan <- "data_shannan"

raw.patients <- read_edw_data(dir.shannan, "patients") %>%
    filter(age < 18,
           discharge.datetime <= mdy_hms("3/21/2015 23:59:59"),
           visit.type != "Inpatient")

concat_encounters(raw.patients$pie.id, 900)

raw.identifiers <- read_edw_data(dir.shannan, "identifiers", "id")

raw.visits <- read_edw_data(dir.shannan, "visits") %>%
    mutate(length.stay = difftime(discharge.datetime, admit.datetime, units = "days"))

codes <- c("493.92", "493.02", "493.12", "493.22", "493.01", "493.11", "493.21", "493.91")

tmp.descript <- icd_description(codes)

raw.icd9 <- read_edw_data(dir.shannan, "icd9") %>%
    inner_join(tmp.descript, by = c("diag.code" = "icd.code")) %>%
    distinct(pie.id, diag.code) %>%
    arrange(desc(diag.seq)) %>%
    group_by(pie.id) %>%
    summarize(diag.code = first(diag.code),
              diag.description = first(icd.description))

data <- inner_join(raw.patients, raw.identifiers, by = "pie.id") %>%
    inner_join(raw.visits[c("pie.id", "length.stay")], by = "pie.id") %>%
    inner_join(raw.icd9, by = "pie.id") %>%
    select(fin, everything(), -discharge.datetime, -person.id, -pie.id)

write_csv(data, paste(dir.shannan, "asthma_patients.csv", sep = "/"))
