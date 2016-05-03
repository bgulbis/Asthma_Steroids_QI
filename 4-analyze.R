# analyze

source("0-library.R")

tmp <- get_rds(dir.save)

analyze.patients <- select(data.demographics, pie.id, age, sex, length.stay) %>%
    inner_join(data.groups, by = "pie.id") %>%
    inner_join(data.measures, by = "pie.id") %>%
    inner_join(data.primary.diagnosis[c("pie.id", "diag.code", "icd.description")],
               by = "pie.id") %>%
    inner_join(data.readmit, by = "pie.id") %>%
    inner_join(data.picu, by = "pie.id") %>%
    inner_join(data.emesis, by = "pie.id")

analyze.scores <- inner_join(data.groups, data.asthma, by = "pie.id")

analyze.steroids <- select(data.steroids, -first.datetime, -last.datetime) %>%
    inner_join(data.groups, by = "pie.id")

analyze.meds.adjunct <- inner_join(data.groups, data.meds.adjunct, by = "pie.id")

analyze.identifiers <- select(data.identifiers, -person.id)

to.save <- ls(pattern = "^analyze")
walk(to.save, ~ write_csv(get(.x), paste0(dir.save, "/", .x, ".csv")))

save_rds(dir.save, "^analyze")
