library(tidyverse)
library(lubridate)
library(edwr)

dir_raw <- "data/raw/megan"
tzone <- "US/Central"

dirr::gzip_files(dir_raw)

# step 1 -----------------------------------------------
# run MBO query:
#   * Patients - By Medication (Generic) - Administration Date
#       - Medication (Generic): albuterol
#       - Facility (Curr): HC Children's
#       - Date Only - Admit

# filter data
raw_patients <- read_data(dir_raw, "patients", FALSE) %>%
    as.patients() %>%
    filter(
        age <= 17,
        visit.type != "Outpatient",
        visit.type != "Emergency",
        discharge.datetime <= mdy("12/31/2017", tz = tzone)
    )

mbo_pie <- concat_encounters(raw_patients$millennium.id)

# step 2 -----------------------------------------------
# limit to patients receiving albuterol between desired date ranges

# run MBO query
#   * Medications - Inpatient - All

raw_meds <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt()

meds_albuterol <- raw_meds %>%
    filter(med == "albuterol",
           (
               med.datetime >= mdy("9/22/2016", tz = tzone) &
                   med.datetime <= mdy("10/19/2016", tz = tzone)
           ) |
               (
                   med.datetime >= mdy("9/22/2017", tz = tzone) &
                       med.datetime <= mdy("10/19/2017", tz = tzone)
               )
    ) %>%
    mutate(
        orig.order.id = if_else(
            order.parent.id == 0,
            order.id,
            order.parent.id
        )
    )

mbo_alb <- concat_encounters(meds_albuterol$millennium.id)

mbo_order <- concat_encounters(meds_albuterol$orig.order.id, 1000)

# step 3 -----------------------------------------------
# run the following MBO queries using mbo_alb
#   * Demographics
#   * Diagnosis Codes (ICD-9/10-CM) - All
#   * Identifiers - by Millennium Encounter Id
#   * Location History
#   * Measures
#   * Vitals - BP

# run the following MBO query using mbo_order
#   * Orders Meds - Details - by Order id
#   * Orders Meds - Details - by Order id, Volume

# find ashtma patients; ICD 10 = J45.*

diag_asthma <- read_data(dir_raw, "diagnosis", FALSE) %>%
    as.diagnosis() %>%
    filter(
        diag.type == "FINAL",
        str_detect(diag.code, "J45")
    )

pts_asthma <- raw_patients %>%
    semi_join(diag_asthma, by = "millennium.id") %>%
    mutate(asthma = TRUE) %>%
    select(millennium.id, asthma)
    # mutate(group = year(discharge.datetime)) %>%
    # select(millennium.id, group)

raw_demographics <- read_data(dir_raw, "demographics", FALSE) %>%
    as.demographics(extras = list("age.days" = "Age- Days (At Admit)")) %>%
    mutate_at("age.days", as.numeric)

raw_measures <- read_data(dir_raw, "measures", FALSE) %>%
    as.events(order_var = FALSE)

raw_vitals <- read_data(dir_raw, "vitals", FALSE) %>%
    as.vitals()

sbp <- raw_vitals %>%
    filter(str_detect(vital, "systolic"))

pts_all <- raw_demographics %>%
    semi_join(pts_asthma, by = "millennium.id")

# orders -----------------------------------------------

raw_orders <- read_data(dir_raw, "orders-details_", FALSE) %>%
    as.order_detail()

raw_orders_vol <- read_data(dir_raw, "orders-details-vol", FALSE) %>%
    rename(
        `Order Strength Dose` = `Order Volume Dose`,
        `Order Strength Dose Unit` = `Order Volume Dose Unit`
    ) %>%
    as.order_detail()
    # anti_join(raw_orders, c("millennium.id", "order.id"))

orders <- bind_rows(raw_orders, raw_orders_vol) %>%
    arrange(
        millennium.id,
        order.id,
        order.datetime
    ) %>%
    filter(ingredient.unit %in% c("mg", "puff")) %>%
    select(
        millennium.id,
        order.id,
        order.datetime,
        freq,
        prn
    ) %>%
    distinct()

# albuterol --------------------------------------------
meds <- meds_albuterol %>%
    left_join(
        orders,
        by = c("millennium.id", "orig.order.id" = "order.id")
    ) %>%
    semi_join(pts_asthma, by = "millennium.id")

meds_albuterol_cont <- meds %>%
    filter(!is.na(event.tag) | med.dose.units == "microgram")
    # filter(is.na(order.datetime))
    # filter(!is.na(event.tag) | route == "IV")

meds_albuterol_cont_run <- meds_albuterol %>%
    filter(!is.na(event.tag) | med.dose.units == "microgram") %>%
    mutate_at("med.rate", funs(na_if(., 0L))) %>%
    mutate_at("med.rate", funs(coalesce(., med.dose))) %>%
    mutate_at("med.rate.units", funs(coalesce(., med.dose.units))) %>%
    calc_runtime()

meds_albuterol_cont_dosing <- summarize_data(meds_albuterol_cont_run)

mbo_cont <- concat_encounters(meds_albuterol_cont$orig.order.id, 1000)

raw_orders_cont <- read_data(dir_raw, "orders-details-cont", FALSE) %>%
    rename(
        `Order Strength Dose` = `Order Volume Dose`,
        `Order Strength Dose Unit` = `Order Volume Dose Unit`
    ) %>%
    mutate(
        Frequency = NA_character_,
        `PRN Indicator` = NA_character_
    ) %>%
    as.order_detail(
        extras = list(clinical.display = "Complete Clinical Display Line")
    )

# run MBO query
#   * Orders Meds - Details - by Order Id, Cont

meds_albuterol_mdi_run <- meds_albuterol %>%
    filter(med.dose.units == "puff") %>%
    calc_runtime(cont = FALSE) %>%
    mutate(
        orig.order.id = if_else(
            order.parent.id == 0,
            order.id,
            order.parent.id
        )
    ) %>%
    left_join(
        orders,
        by = c("millennium.id", "orig.order.id" = "order.id")
    )

meds_albuterol_mdi_dosing <- meds_albuterol_mdi_run %>%
    summarize_data(cont = FALSE)

meds_albuterol_neb_run <- meds_albuterol %>%
    filter(
        is.na(event.tag),
        med.dose.units != "microgram",
        med.dose.units != "puff"
    ) %>%
    calc_runtime(cont = FALSE) %>%
    mutate(
        orig.order.id = if_else(
            order.parent.id == 0,
            order.id,
            order.parent.id
        )
    ) %>%
    left_join(
        orders,
        by = c("millennium.id", "orig.order.id" = "order.id")
    )

meds_albuterol_neb_dosing <- meds_albuterol_neb_run %>%
    summarize_data(cont = FALSE)

# other ---------------------------------------------

data_meds_steroids_run <- raw_meds %>%
    semi_join(pts_asthma, by = "millennium.id") %>%
    filter(
        med %in% c(
            "prednisone",
            "prednisolone",
            "methylprednisolone",
            "dexamethasone"
        )
    ) %>%
    calc_runtime(cont = FALSE)

data_meds_steroids_dosing <- data_meds_steroids_run %>%
    summarize_data(cont = FALSE)

meds_mag <- raw_meds %>%
    semi_join(pts_asthma, by = "millennium.id") %>%
    filter(
        str_detect(med, "magnesium"),
        route %in% c("IV", "IVPB")
    )

meds_mag_cont <- meds_mag %>%
    filter(!is.na(event.tag))

if (nrow(meds_mag_cont) > 0) {
    meds_mag_cont_run <- meds_mag_cont %>%
        mutate_at("med.rate", funs(na_if(., 0L))) %>%
        mutate_at("med.rate", funs(coalesce(., med.dose))) %>%
        mutate_at("med.rate.units", funs(coalesce(., med.dose.units))) %>%
        calc_runtime()

    meds_mag_cont_dosing <- summarize_data(meds_mag_cont_run)
}

meds_mag_run <- meds_mag %>%
    filter(is.na(event.tag)) %>%
    calc_runtime(cont = FALSE)

meds_mag_dosing <- meds_mag_run %>%
    summarize_data(cont = FALSE)

bp_mag <- meds_mag %>%
    left_join(sbp, by = "millennium.id") %>%
    filter(
        difftime(med.datetime, vital.datetime, units = "hours") > 0,
        difftime(med.datetime, vital.datetime, units = "hours") <= 1
    )

# allergy meds -----------------------------------------

allergy <- raw_meds %>%
    semi_join(pts_all, by = "millennium.id") %>%
    filter(
        med %in% c(
            "montelukast",
            "cetirizine",
            "diphenhydramine",
            "loratidine",
            "levocetirizine",
            "fluticasone",
            "mometasone"
        )
    )

# measures ---------------------------------------------

measures <- raw_measures %>%
    filter(!is.na(event.result.units)) %>%
    arrange(millennium.id, event.datetime) %>%
    group_by(millennium.id, event, event.result.units) %>%
    mutate_at("event.result", as.numeric) %>%
    summarize_at("event.result", first) %>%
    ungroup() %>%
    select(-event.result.units) %>%
    spread(event, event.result)

# locations --------------------------------------------

locations <- read_data(dir_raw, "locations", FALSE) %>%
    as.locations() %>%
    tidy_data() %>%
    semi_join(pts_all, by = "millennium.id")

# vent -------------------------------------------------

vent <- read_data(dir_raw, "vent", FALSE) %>%
    as.events() %>%
    select(millennium.id:event.result, event.location) %>%
    arrange(millennium.id, event.datetime) %>%
    semi_join(pts_all, by = "millennium.id") %>%
    distinct(
        millennium.id,
        event,
        event.result,
        .keep_all = TRUE
    )

# data sets --------------------------------------------

id <- read_data(dir_raw, "identifiers", FALSE) %>%
    as.id()

group <- meds_albuterol %>%
    # mutate(year = floor_date(med.datetime, "year")) %>%
    mutate(year = year(med.datetime)) %>%
    distinct(millennium.id, year)

data_demographics <- pts_all %>%
    semi_join(meds_albuterol, by = "millennium.id") %>%
    left_join(id, by = "millennium.id") %>%
    left_join(group, by = "millennium.id") %>%
    left_join(pts_asthma, by = "millennium.id") %>%
    mutate_at("asthma", funs(coalesce(., FALSE))) %>%
    left_join(measures, by = "millennium.id") %>%
    select(-disposition, -visit.type, -facility)

write.csv(data_demographics, "data/external/demographics.csv", row.names = FALSE)
write.csv(meds_albuterol_cont_run, "data/external/albuterol_cont_all.csv", row.names = FALSE)
write.csv(meds_albuterol_cont_dosing, "data/external/albuterol_cont_summary.csv", row.names = FALSE)
write.csv(meds_albuterol_mdi_run, "data/external/albuterol_mdi_all.csv", row.names = FALSE)
write.csv(meds_albuterol_mdi_dosing, "data/external/albuterol_mdi_summary.csv", row.names = FALSE)
write.csv(meds_albuterol_neb_run, "data/external/albuterol_neb_all.csv", row.names = FALSE)
write.csv(meds_albuterol_neb_dosing, "data/external/albuterol_neb_summary.csv", row.names = FALSE)
write.csv(data_meds_steroids_run, "data/external/steroids_all.csv", row.names = FALSE)
write.csv(data_meds_steroids_dosing, "data/external/steroids_summary.csv", row.names = FALSE)

if (exists("meds_mag_cont_run")) write.csv(meds_mag_cont_run, "data/external/magnesium_cont_all.csv", row.names = FALSE)
if (exists("meds_mag_cont_dosing")) write.csv(meds_mag_cont_dosing, "data/external/magnesium_cont_summary.csv", row.names = FALSE)

write.csv(meds_mag_run, "data/external/magnesium_all.csv", row.names = FALSE)
write.csv(meds_mag_dosing, "data/external/magnesium_summary.csv", row.names = FALSE)
write.csv(bp_mag, "data/external/bp_magnesium.csv", row.names = FALSE)

write.csv(locations, "data/external/locations.csv", row.names = FALSE)
write.csv(vent, "data/external/vent.csv", row.names = FALSE)
write.csv(allergy, "data/external/allergy_meds.csv", row.names = FALSE)

# explore ----------------------------------------------


# median duration between starting continuous and first intermittent dose

order_rate <- raw_orders_cont %>%
    mutate(
        rate.text = str_extract(clinical.display, "Delivers(.*)h(ou)?r")
    ) %>%
    mutate_at("rate.text", str_extract, pattern = "[0-9]{1,2}mg") %>%
    mutate_at("rate.text", str_extract, pattern = "[0-9]{1,2}") %>%
    mutate_at("rate.text", as.numeric) %>%
    mutate_at(
        "rate.text",
        funs(
            coalesce(
                .,
                case_when(
                    str_detect(clinical.display, "233\\.4.*83.*30") ~ 10,
                    str_detect(clinical.display, "216\\.6.*167.*30") ~ 20
                )
            )
        )
    ) %>%
    filter(!is.na(rate.text)) %>%
    distinct(millennium.id, order.id, rate.text)

med_rate <- meds_albuterol_cont %>%
    left_join(
        order_rate,
        by = c("millennium.id", "orig.order.id" = "order.id")
    ) %>%
    filter(!is.na(rate.text))

# by each rate / year, mean/median duration at that rate

data_med_cont_durations <- med_rate %>%
    arrange(
        millennium.id,
        med.datetime
    ) %>%
    left_join(
        data_demographics[c("millennium.id", "year")],
        by = "millennium.id"
    ) %>%
    # mutate_at("med.dose", round, digits = 0) %>%
    group_by(millennium.id, year) %>%
    mutate(same.rate = rate.text != lag(rate.text)) %>%
    mutate_at("same.rate", funs(coalesce(., TRUE))) %>%
    mutate(rate.count = cumsum(same.rate)) %>%
    group_by(millennium.id, year, rate.text, rate.count) %>%
    summarize_at("med.datetime", first) %>%
    group_by(millennium.id, year) %>%
    arrange(
        millennium.id,
        rate.count
    ) %>%
    mutate(
        duration = difftime(
            lead(med.datetime),
            med.datetime,
            units = "hours"
        )
    ) %>%
    mutate_at("duration", funs(coalesce(., 8.7)))

# median duration between starting continuous and first intermittent dose

cont_start <- data_med_cont_durations %>%
    group_by(millennium.id, year) %>%
    summarize_at("med.datetime", min) %>%
    rename(cont.start = med.datetime)

intermit <- meds_albuterol %>%
    inner_join(
        data_demographics[c("millennium.id", "year")],
        by = "millennium.id"
    ) %>%
    left_join(
        orders,
        by = c("millennium.id", "orig.order.id" = "order.id")
    ) %>%
    filter(
        is.na(event.tag),
        # str_detect(freq, "Q4H"),
        prn == "Scheduled"
    ) %>%
    group_by(millennium.id) %>%
    arrange(millennium.id, med.datetime)

data_med_intermit_start <- intermit %>%
    inner_join(cont_start, by = c("millennium.id", "year")) %>%
    filter(
        str_detect(freq, "Q4H"),
        med.datetime >= cont.start
    ) %>%
    group_by(millennium.id, year, cont.start) %>%
    summarize_at("med.datetime", min) %>%
    ungroup() %>%
    mutate(
        time.intermit = difftime(
            med.datetime,
            cont.start,
            units = "hours"
        )
    )

data_med_intermit_start_any <- intermit %>%
    inner_join(cont_start, by = c("millennium.id", "year")) %>%
    filter(
        # str_detect(freq, "Q4H"),
        med.datetime >= cont.start
    ) %>%
    group_by(millennium.id, year, cont.start) %>%
    summarize_at("med.datetime", min) %>%
    ungroup() %>%
    mutate(
        time.intermit = difftime(
            med.datetime,
            cont.start,
            units = "hours"
        )
    )
# if on 10mg/hr were they also on 10 puffs q4h, duration (sub for 20mg/hr)

cont_10 <- data_med_cont_durations %>%
    group_by(millennium.id, year) %>%
    filter(rate.text == 10) %>%
    rename(rate.start = med.datetime)

data_med_cont_intermit <- intermit %>%
    inner_join(cont_10, by = c("millennium.id", "year")) %>%
    mutate_at("duration", as.integer) %>%
    filter(med.dose == 10,
           med.datetime >= rate.start,
           med.datetime <= rate.start + hours(duration))

# duration of puffs / nebs at each frequency

data_med_intermit_duration <- meds_albuterol %>%
    inner_join(
        data_demographics[c("millennium.id", "year")],
        by = "millennium.id"
    ) %>%
    left_join(
        orders,
        by = c("millennium.id", "orig.order.id" = "order.id")
    ) %>%
    filter(
        is.na(event.tag),
        prn == "Scheduled",
        freq != "ONCE",
        freq != "ONCALL"
    ) %>%
    mutate_at("med.dose", round, digits = 1) %>%
    mutate(
        alt.freq = case_when(
            str_detect(freq, "Q15Min") ~ 0.25,
            str_detect(freq, "Q1H") ~ 1,
            str_detect(freq, "Q2H") ~ 2,
            str_detect(freq, "Q3H") ~ 3,
            str_detect(freq, "Q4H") ~ 4,
            str_detect(freq, "Q6H|QID") ~ 6,
            str_detect(freq, "Q8H|TID") ~ 8,
            str_detect(freq, "Q12H|BID") ~ 12,
            str_detect(freq, "Q24H|Daily") ~ 24
        )
    ) %>%
    arrange(millennium.id, med.datetime) %>%
    group_by(millennium.id, year) %>%
    mutate(same.dose = med.dose != lag(med.dose) |
               med.dose.units != lag(med.dose.units) |
               alt.freq != lag(alt.freq)) %>%
    mutate_at("same.dose", funs(coalesce(., TRUE))) %>%
    mutate(dose.count = cumsum(same.dose)) %>%
    group_by(
        millennium.id,
        year,
        dose.count,
        med.dose,
        med.dose.units,
        alt.freq
    ) %>%
    summarize_at("med.datetime", funs(first, last)) %>%
    group_by(millennium.id, dose.count) %>%
    mutate(duration = difftime(last, first, units = "hours")) %>%
    mutate_at("duration", as.numeric) %>%
    mutate_at(
        "duration",
        funs(
            if_else(
                duration == 0,
                1,
                .
            )
        )
    )

cont_to_q4 <- med_rate %>%
    ungroup() %>%
    mutate(med.rate = rate.text) %>%
    left_join(
        data_med_intermit_start %>%
            rename(q4.start = med.datetime),
        by = "millennium.id"
    ) %>%
    filter(
        med.datetime >= cont.start,
        med.datetime <= q4.start
    ) %>%
    calc_runtime() %>%
    mutate_at(
        c("duration", "run.time"),
        na_if,
        y = 0
    ) %>%
    mutate_at(
        c("duration", "run.time"),
        funs(coalesce(., 8.7))
    ) %>%
    summarize_data()

intermit_to_q4 <- meds_albuterol %>%
    filter(is.na(event.tag)) %>%
    # semi_join(data_demographics, by = "millennium.id") %>%
    semi_join(med_rate, by = "millennium.id") %>%
    left_join(
        data_med_intermit_start %>%
            rename(q4.start = med.datetime),
        by = "millennium.id"
    ) %>%
    filter(
        med.datetime >= cont.start,
        med.datetime <= q4.start
    ) %>%
    mutate(
        cum.dose = if_else(
            med.dose.units == "puff",
            med.dose * 0.09,
            med.dose
        )
    ) %>%
    group_by(millennium.id) %>%
    summarize_at("cum.dose", sum, na.rm = TRUE)

data_albut_total <- cont_to_q4 %>%
    select(millennium.id, cum.dose) %>%
    bind_rows(intermit_to_q4) %>%
    group_by(millennium.id) %>%
    summarize_at("cum.dose", sum, na.rm = TRUE) %>%
    left_join(
        data_demographics[c("millennium.id", "year")],
        by = "millennium.id"
    )

dirr::save_rds("data/tidy/megan", "data_")

# df <- data_med_cont_durations %>%
#     ungroup() %>%
#     add_count(year, rate.text) %>%
#     group_by(year, rate.text, n) %>%
#     summarize_at(
#         "duration",
#         funs(
#             mean,
#             sd,
#             median,
#             q25 = quantile(., 0.25),
#             q75 = quantile(., 0.75)
#         ),
#         na.rm = TRUE
#     )
#
# df2 <- data_med_intermit_duration %>%
#     ungroup() %>%
#     add_count(year, med.dose, med.dose.units, alt.freq) %>%
#     group_by(year, med.dose, med.dose.units, alt.freq, n) %>%
#     summarize_at(
#         "duration",
#         funs(
#             mean,
#             sd,
#             median,
#             q25 = quantile(., 0.25),
#             q75 = quantile(., 0.75)
#         ),
#         na.rm = TRUE
#     )
