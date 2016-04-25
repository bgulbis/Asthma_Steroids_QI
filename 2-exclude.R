# exclude

source("0-library.R")

tmp <- get_rds(dir.save)

# get list of unique person id's to use for identifying re-encounters
raw.persons <- read_edw_data(dir.patients, "demographics") %>%
    select(person.id) %>%
    distinct

concat_encounters(raw.persons$person.id)
