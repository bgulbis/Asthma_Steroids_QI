# tidy

source("0-library.R")

tmp <- get_rds(dir.save)

include <- data_frame(pie.id = patients$include)

# demographics ----

data.demographics <- read_edw_data(dir.patients, "demographics") %>%
    semi_join(include, by = "pie.id")

# primary diagnosis ----

tmp.icd9 <- read_edw_data(dir.patients, "icd9") %>%
    semi_join(include, by = "pie.id") %>%
    filter(diag.type == "Final",
           diag.seq == 1)

tmp <- icd_description(tmp.icd9$diag.code)

tmp.icd9 <- inner_join(tmp.icd9, tmp, by = c("diag.code" = "icd.code"))

tmp.icd10 <- read_edw_data(dir.patients, "icd10") %>%
    semi_join(include, by = "pie.id") %>%
    filter(diag.type == "Final",
           diag.seq == 1)

tmp <- icd_description(tmp.icd10$diag.code, icd10 = TRUE)

tmp.icd10 <- inner_join(tmp.icd10, tmp, by = c("diag.code" = "icd.code"))

data.primary.diagnosis <- bind_rows(tmp.icd9, tmp.icd10)

# measures ----

tmp.measures <- read_edw_data(dir.patients, "measures") %>%
    semi_join(include, by = "pie.id")

tmp.height <- filter(tmp.measures, measure == "Height",
                     measure.units == "cm") %>%
    group_by(pie.id) %>%
    arrange(measure.datetime) %>%
    summarize(height = first(measure.result))

data.measures <- filter(tmp.measures, measure == "Weight",
                        measure.units == "kg") %>%
    group_by(pie.id) %>%
    arrange(measure.datetime) %>%
    summarize(weight = first(measure.result)) %>%
    left_join(tmp.height, by = "pie.id")

# asthma assessment ----

data.asthma <- read_edw_data(dir.patients, "scores", "events") %>%
    mutate(post = ifelse(str_detect(event, "post"), TRUE, FALSE),
           event = str_replace_all(event, "  ", " "),
           event = str_replace_all(event, "(rc|pre|post| asthma assess|asthma treatment )", ""),
           event = str_trim(event, "both"),
           event = str_replace_all(event, " ", "_")) %>%
    filter(event.result != "") %>%
    group_by(pie.id, event.datetime, post, event) %>%
    summarize(event.result = first(event.result)) %>%
    group_by(pie.id, event.datetime, post) %>%
    spread(event, event.result)
