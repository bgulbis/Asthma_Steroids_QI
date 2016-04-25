# exclude

source("0-library.R")

tmp <- get_rds(dir.save)

# get list of unique person id's to use for identifying re-encounters
raw.persons <- read_edw_data(dir.patients, "demographics") %>%
    select(person.id) %>%
    distinct

concat_encounters(raw.persons$person.id)

# exclude any pregnant patients

preg.icd9 <- read_edw_data(dir.patients, "icd9")
preg.icd10 <- read_edw_data(dir.patients, "icd10")
preg.labs <- read_edw_data(dir.patients, "labs_preg", "labs")

excl.preg <- check_pregnant(preg.labs, preg.icd9, preg.icd10)

patients$exclude_pregnant = excl.preg$pie.id
