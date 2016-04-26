# include

source("0-library.R")

# take data from edw query for patients receiving either dexamethasone or
# prednisone/prednisolone at CMHH and filter to desired age range and admission
# time-frame
raw.patients <- read_edw_data(dir.patients, "patients") %>%
    filter(age >= 4,
           age <= 17,
           discharge.datetime <= mdy_hms("12/31/2015 23:59:59"))

concat_encounters(raw.patients$pie.id, 900)

# take the list of eligible patients and screen for diagnosis codes of acute
# asthma exacerbation

# icd9: 493.92 (may want to consider: 493.02, 493.12, 493.22)
# icd10: J45.901 (may want to consider: J45.21, J45.31, J45.41, J45.51)

# code <- "493.92"
code <- c("493.92", "493.02", "493.12", "493.22", "493.01", "493.11", "493.21",
          "493.91")
ref.icd9 <- data_frame(disease.state = "asthma", type = "ICD", code = code)

# code <- "J45.901"
code <- c("J45.901", "J45.21", "J45.31", "J45.41", "J45.51", "J45.22", "J45.32",
          "J45.42", "J45.52", "J45.902")
ref.icd10 <- data_frame(disease.state = "asthma", type = "ICD", code = code)

raw.icd9 <- read_edw_data(dir.patients, "icd9")
raw.icd10 <- read_edw_data(dir.patients, "icd10")

tidy.icd9 <- tidy_data(raw.icd9, "icd9", ref.data = ref.icd9)
tidy.icd10 <- tidy_data(raw.icd10, "icd10", ref.data = ref.icd10)

eligible <- bind_rows(tidy.icd9, tidy.icd10)$pie.id

save_rds(dir.save, "^eligible")

concat_encounters(eligible, 900)
