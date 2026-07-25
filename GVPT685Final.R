## Loading libraries
library(dplyr)
library(lubridate)
library(tidyr)
library(janitor)

## Loading data
ElgbVtr <- read.csv("~/Downloads/VoterList.ElgbVtr.62941.010224102656.csv", stringsAsFactors = FALSE, na.strings = c("", "NA")) |>
  clean_names()
VtHst <- read.csv("~/Downloads/VoterList.VtHst.62941.010224102656.csv", stringsAsFactors = FALSE, na.strings = c("", "NA")) |>
  clean_names()

## Parsing dates
ElgbVtr <- ElgbVtr |>
  mutate(
    dob      = as.Date(birth_date,        format = "%m/%d/%Y"),
    reg_date = as.Date(registration_date, format = "%m/%d/%Y"),
    yob      = year(dob)
  )

VtHst <- VtHst |>
  mutate(
    edate = as.Date(election_date, format = "%m/%d/%Y"),
    yr    = year(edate),
    mo    = month(edate)
  )

## Filtering to November generals
vh_gen <- VtHst |> filter(mo == 11, yr %% 2 == 0)

## Building per-voter turnout flags
turnout_flags <- vh_gen |>
  distinct(voter_id, yr) |>
  mutate(voted = 1L) |>
  pivot_wider(names_from = yr, values_from = voted, names_prefix = "voted_", values_fill = 0L)

ElgbVtr <- ElgbVtr |>
  left_join(turnout_flags, by = "voter_id") |>
  mutate(across(starts_with("voted_"), ~replace_na(.x, 0L)))

for (y in c(2010, 2012, 2014, 2016, 2018, 2020, 2022)) {
  col <- paste0("voted_", y)
  if (!col %in% names(ElgbVtr)) ElgbVtr[[col]] <- 0L
}

## Building the DV
ElgbVtr <- ElgbVtr |>
  mutate(dv_pres1620 = pmax(voted_2016, voted_2020))

## Building party variables
ElgbVtr <- ElgbVtr |>
  mutate(
    party_clean = trimws(party),
    party_dem  = as.integer(party_clean == "Democrat"),
    party_rep  = as.integer(party_clean == "Republican"),
    party_npa  = as.integer(party_clean == "Non-Partisan"),
    party_iap  = as.integer(party_clean == "Independent American Party"),
    party_nlp  = as.integer(party_clean == "Natural Law Party"),
    party_lib  = as.integer(party_clean == "Libertarian Party"),
    party_grn  = as.integer(party_clean == "Green Party"),
    party_oth  = as.integer(party_clean == "Other (All Others)"),
    party_ord  = case_when(
      party_dem == 1 ~ 1L,
      party_rep == 1 ~ 3L,
      TRUE           ~ 2L
    )
  )

## Building phone flag
ElgbVtr <- ElgbVtr |>
  mutate(phone_flag = as.integer(!is.na(phone) & nchar(trimws(phone)) > 0))

## Building county dummies
ElgbVtr <- ElgbVtr |>
  mutate(county_clean = ifelse(is.na(residential_county) | residential_county == "",
                               "UNKNOWN",
                               toupper(trimws(residential_county))))

for (c in sort(unique(ElgbVtr$county_clean))) {
  vname <- paste0("cty_", tolower(gsub("[^A-Z0-9]", "", c)))
  ElgbVtr[[vname]] <- as.integer(ElgbVtr$county_clean == c)
}

## Building congressional district dummies
ElgbVtr <- ElgbVtr |>
  mutate(cd_clean = ifelse(is.na(congressional_district) | congressional_district == "",
                           "UNKNOWN",
                           as.character(congressional_district)))

for (d in sort(unique(ElgbVtr$cd_clean))) {
  vname <- paste0("cd_", tolower(gsub("[^A-Z0-9]", "", d)))
  ElgbVtr[[vname]] <- as.integer(ElgbVtr$cd_clean == d)
}

## Building new voter, active flag, tenure
ElgbVtr <- ElgbVtr |>
  mutate(
    new_voter        = as.integer(reg_date > as.Date("2020-11-03")),
    years_registered = as.numeric(difftime(as.Date("2024-01-01"), reg_date, units = "days")) / 365.25,
    active_flag      = as.integer(toupper(trimws(county_status)) == "ACTIVE")
  )

## Building age variables and buckets
ElgbVtr <- ElgbVtr |>
  mutate(
    age_2020 = 2020 - yob,
    age_2022 = 2022 - yob,
    age_2024 = 2024 - yob,
    age_bucket = case_when(
      age_2024 < 25  ~ "18_24",
      age_2024 < 35  ~ "25_34",
      age_2024 < 50  ~ "35_49",
      age_2024 < 65  ~ "50_64",
      age_2024 >= 65 ~ "65plus",
      TRUE           ~ NA_character_
    )
  )

for (b in c("18_24","25_34","35_49","50_64","65plus")) {
  ElgbVtr[[paste0("age_", b)]] <- as.integer(ElgbVtr$age_bucket == b)
}

## Building of_index from midterms
ElgbVtr <- ElgbVtr |>
  mutate(of_index = voted_2010 + voted_2014 + voted_2018 + voted_2022)

for (k in 0:4) {
  ElgbVtr[[paste0("of_", k)]] <- as.integer(ElgbVtr$of_index == k)
}

## Running descriptives — turnout
for (y in c(2012, 2014, 2016, 2018, 2020, 2022)) {
  print(table(vh_gen$vote_code[vh_gen$yr == y], useNA = "ifany"))
  print(table(ElgbVtr[[paste0("voted_", y)]], useNA = "ifany"))
}

## Running descriptives — party
print(table(ElgbVtr$party, useNA = "ifany"))
for (p in c("party_dem","party_rep","party_npa","party_iap",
            "party_nlp","party_lib","party_grn","party_oth")) {
  print(table(ElgbVtr[[p]], useNA = "ifany"))
}
print(table(ElgbVtr$party_ord, useNA = "ifany"))

## Running descriptives — phone, counties, CDs
print(sum(!is.na(ElgbVtr$phone) & nchar(trimws(ElgbVtr$phone)) > 0))
print(table(ElgbVtr$phone_flag, useNA = "ifany"))
print(table(ElgbVtr$residential_county, useNA = "ifany"))
for (cd in grep("^cty_", names(ElgbVtr), value = TRUE)) {
  print(table(ElgbVtr[[cd]], useNA = "ifany"))
}
print(table(ElgbVtr$congressional_district, useNA = "ifany"))
for (d in grep("^cd_(cd|unknown)", names(ElgbVtr), value = TRUE)) {
  print(table(ElgbVtr[[d]], useNA = "ifany"))
}

## Running descriptives — new voter, active, age, of_index
print(summary(ElgbVtr$years_registered))
print(table(ElgbVtr$new_voter, useNA = "ifany"))
print(table(ElgbVtr$county_status, useNA = "ifany"))
print(table(ElgbVtr$active_flag, useNA = "ifany"))
print(mean(ElgbVtr$age_2020, na.rm = TRUE))
print(mean(ElgbVtr$age_2022, na.rm = TRUE))
print(mean(ElgbVtr$age_2024, na.rm = TRUE))
print(table(ElgbVtr$of_index, useNA = "ifany"))
for (k in 0:4) print(table(ElgbVtr[[paste0("of_", k)]], useNA = "ifany"))
print(table(ElgbVtr$age_bucket, useNA = "ifany"))
for (b in c("18_24","25_34","35_49","50_64","65plus")) {
  print(table(ElgbVtr[[paste0("age_", b)]], useNA = "ifany"))
}

## Running bivariate regressions
biv <- function(iv_name, data = ElgbVtr) {
  f <- as.formula(paste("dv_pres1620 ~", iv_name))
  m <- glm(f, data = data, family = binomial(link = "logit"))
  s <- summary(m)$coefficients
  data.frame(
    variable = iv_name,
    coef     = round(s[2, "Estimate"], 4),
    p_value  = signif(s[2, "Pr(>|z|)"], 4),
    stringsAsFactors = FALSE
  )
}

all_ivs <- c(
  "voted_2012","voted_2014","voted_2018","voted_2022",
  "party_dem","party_rep","party_npa","party_iap","party_nlp",
  "party_lib","party_grn","party_oth","party_ord",
  "phone_flag",
  grep("^cty_", names(ElgbVtr), value = TRUE),
  grep("^cd_(cd|unknown)", names(ElgbVtr), value = TRUE),
  "new_voter","active_flag","age_2020",
  "age_18_24","age_25_34","age_35_49","age_50_64","age_65plus",
  "of_index","of_0","of_1","of_2","of_3","of_4"
)

results <- do.call(rbind, lapply(all_ivs, function(v) {
  tryCatch(biv(v),
           error = function(e) data.frame(variable = v, coef = NA, p_value = NA))
}))

print(results, row.names = FALSE)
write.csv(results, "bivariate_results.csv", row.names = FALSE)

## Fitting the final model
mod <- glm(
  dv_pres1620 ~ of_1 + of_2 + of_3 + of_4 +
    age_25_34 + age_35_49 + age_50_64 + age_65plus +
    party_dem + party_rep +
    active_flag + new_voter + phone_flag +
    cty_clark + cty_washoe,
  data = ElgbVtr,
  family = binomial(link = "logit")
)

summary(mod)

## Generating predicted probabilities
ElgbVtr$pred_2024 <- predict(mod, newdata = ElgbVtr, type = "response")

print(sum(ElgbVtr$pred_2024, na.rm = TRUE))
print(mean(ElgbVtr$pred_2024, na.rm = TRUE))
print(sum(ElgbVtr$pred_2024 > 0.5, na.rm = TRUE))

## Building TO buckets
ElgbVtr$to_bucket <- as.integer(cut(
  ElgbVtr$pred_2024,
  breaks = quantile(ElgbVtr$pred_2024, probs = seq(0, 1, 0.1), na.rm = TRUE),
  include.lowest = TRUE,
  labels = FALSE
))

for (k in 1:10) {
  ElgbVtr[[paste0("to_bucket_", k)]] <- as.integer(ElgbVtr$to_bucket == k)
}
for (k in 1:10) {
  col <- paste0("to_bucket_", k)
  ElgbVtr[[col]][is.na(ElgbVtr[[col]])] <- 0L
}

print(table(ElgbVtr$to_bucket, useNA = "ifany"))

ElgbVtr |>
  filter(!is.na(to_bucket)) |>
  group_by(to_bucket) |>
  summarise(n = n(),
            mean_pred = round(mean(pred_2024), 3),
            min_pred  = round(min(pred_2024),  3),
            max_pred  = round(max(pred_2024),  3))