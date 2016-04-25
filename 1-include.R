# include

source("0-library.R")

# take data from edw query for patients receiving either dexamethasone or
# prednisone/prednisolone at CMHH and filter to desired age range and admission
# time-frame
raw.patients <- read_edw_data(dir.patients, "patients") %>%
    filter(age >= 4,
           age <= 17,
           discharge.datetime <= mdy_hms("12/31/2015 23:59:59"))

edw.patients <- concat_encounters(raw.patients$pie.id, 900)
print(edw.patients)

# take the list of eligible patients and screen for diagnosis codes of acute
# asthma exacerbation

# icd9: 493.92 (may want to consider: 493.02, 493.12, 493.22)
# icd10: J45.901 (may want to consider: J45.21, J45.31, J45.41, J45.51)

raw.icd9 <- read_edw_data(dir.patients, "icd9")
raw.icd10 <- read_edw_data(dir.patients, "icd10")
