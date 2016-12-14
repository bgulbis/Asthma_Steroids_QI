# analyze

library(tidyverse)

tmp <- dirr::get_rds("data/tidy")

analyze_patients <- select(data_demographics, pie.id, age, sex, length.stay) %>%
    inner_join(data_groups, by = "pie.id") %>%
    inner_join(data_measures, by = "pie.id") %>%
    inner_join(data_primary_diagnosis[c("pie.id", "diag.code", "icd.description")],
               by = "pie.id") %>%
    inner_join(data_readmit, by = "pie.id") %>%
    inner_join(data_picu, by = "pie.id") %>%
    inner_join(data_emesis, by = "pie.id")

analyze_scores <- inner_join(data_groups, data_asthma, by = "pie.id")

analyze_steroids <- select(data_steroids, -first.datetime, -last.datetime) %>%
    inner_join(data_groups, by = "pie.id")

analyze_meds_adjunct <- inner_join(data_groups, data_meds_adjunct, by = "pie.id")

analyze_identifiers <- select(data_identifiers, -person.id)

to.save <- ls(pattern = "^analyze")
walk(to.save, ~ write_csv(get(.x), paste0("data/final/", .x, ".csv")))

dirr::save_rds("data/final", "^analyze")
