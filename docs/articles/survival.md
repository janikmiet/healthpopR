# Survival Analysis from 50 years forward

``` r
# this must be 
# - not using data
# - simplest method to get results
# - outline of what we want to achieve
```

## Data

For this analysis,

### Luku: Koko populaation keski-ikä 2003 vuoden lopussa (n=14200)

``` r
# library(lubridate)
# library(dplyr)
admin_censor <- as.Date("2023-12-31") ## TODO mistä
d1 <- dpop %>% 
  mutate(AGE_2023 = time_length(interval(DATE_BIRTH, as.Date("2023-12-31")), "years"),
         # DIED_2023 = ifelse(DATE_DEATH < as.Date("2023-12-31"), 1, 0)
         DIED_2023 = ifelse(DATE_DEATH > as.Date("2023-12-31") | is.na(DATE_DEATH), 0, 1)
  ) %>% 
  filter(DIED_2023 == 0) %>% ## TODO eli tiputetaanko pois jos kuollut 2023?
  select(ID, AGE_2023, DIED_2023, DATE_DEATH)
d2 <- d1 %>% 
  group_by(AGE_2023) %>% 
  count()
mean_age <- sum(d2$AGE_2023 * d2$n) / sum(d2$n)
print("Mean Age")
mean_age   # mean age
print("Population alive")
sum(d2$n)  # population alive
```

### Kuvio1: Dementian ja lonkkamurtumat kumulatiivinen insidenssi 50-vuotta eteenpäin. Johansen-Aalen estimaatti, kuolema vastaava riski

``` r
## PVM data
df <- df_dates_per_id(exposure_diagnoses = params$data_exp_diagnoses, 
                      response_diagnoses = params$data_resp_diagnoses, 
                      dpop = params$data_classified_pop)
```

``` r
library(dplyr)
library(tibble)
library(cmprsk)
library(ggplot2)

## Dementia vs Mortality after 50y
df_exp <- df_dates_ftime(df, end = DATE_EXPOSURE)
ci_exp  <- cuminc(df_exp$ftime,  df_exp$status,  cencode = 0)
names(ci_exp)  <- c("Dementia", "Death")
ci_exp_df  <- extract_ci_all(ci_exp, "Dementia")
ggplot(ci_exp_df, aes(x = time, y = est, color = curve, group=curve, fill = curve)) +
  geom_step(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Years since age 50",
    y = "Cumulative incidence",
    title = "Dementia vs Death",
    color = "Event",
    fill  = "Event"
  ) +
  theme_minimal(base_size = 14)
```

``` r
## Hip fracture vs Mortality after 50y
df_resp <- df_dates_ftime(df, end = DATE_RESPONSE) 
ci_resp <- cuminc(df_resp$ftime, df_resp$status, cencode = 0)
names(ci_resp) <- c("Hip Fracture", "Death")
ci_resp_df <- extract_ci_all(ci_resp, "Hip Fracture")
ggplot(ci_resp_df, aes(x = time, y = est, color = curve, group=curve, fill = curve)) +
  geom_step(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Years since age 50",
    y = "Cumulative incidence",
    title = "Hip Fracture vs Death",
    color = "Event",
    fill  = "Event"
  ) +
  theme_minimal(base_size = 14)
```

### Kuvio2: Dementia diagnoosista eteenpäin lonkkamurtumat kum.insidenssi

``` r
library(dplyr)
library(lubridate)
library(cmprsk)    # cuminc
library(ggplot2)
library(mstate)    # optional: alternate AJ tools & plotting

# example conversion and cleaning pipeline
df2 <- df %>%
  # convert to Date if necessary; adapt formats if not ISO
  mutate(across(c(DATE_BIRTH, DATE_DEATH, DATE_MIGRATION, DATE_EXPOSURE, DATE_RESPONSE, DATE_50),~ as.Date(.x))) %>%
  
  # compute start = later of DATE_50 and DATE_EXPOSURE
  mutate(start_date = pmax(DATE_50, DATE_EXPOSURE, na.rm = TRUE)) %>%
  
  # define administrative censoring date
  mutate(admin_censor = as.Date("2023-12-31")) %>%
  
  # compute earliest observed end date among possible endpoints and censoring
  mutate(raw_end = pmin(DATE_RESPONSE, DATE_DEATH, DATE_MIGRATION, admin_censor, na.rm = TRUE)) %>%
  
  # if raw_end is earlier than start_date, set end_date = start_date (zero follow-up) and treat as censored
  mutate(end_date = if_else(raw_end <= start_date, start_date, raw_end)) %>%
  
  # define event indicator:
  # 1 = response (DATE_RESPONSE occurred after start), 2 = death (DATE_DEATH occurred after start), 0 = censored
  mutate(fstatus = case_when(
    !is.na(DATE_RESPONSE) & (DATE_RESPONSE > start_date) & (DATE_RESPONSE <= raw_end) ~ 1L,
    !is.na(DATE_DEATH) & (DATE_DEATH > start_date) & (DATE_DEATH <= raw_end) ~ 2L,
    TRUE ~ 0L
  )) %>%
  
  # follow-up time in years (use days/365.25 to preserve fractional years)
  mutate(ftime = as.numeric(difftime(end_date, start_date, units = "days")) / 365.25) %>%
  
  # remove people with missing start_date (no exposure or DATE_50) or negative follow-up
  filter(!is.na(start_date)) %>%
  filter(ftime >= 0)

# quick check
table(df2$fstatus, useNA = "ifany")
summary(df2$ftime)

# 3. Aalen-Johanse estimater
# ensure numeric vectors
ftime <- df2$ftime
fstatus <- df2$fstatus

# overall CIFs (no grouping)
ci <- cuminc(ftime = ftime, fstatus = fstatus, cencode = 0)
names(ci)
names(ci) <- c("Response", "Death")

# base plotting (built-in)
plot(ci, xlab = "Years since start", ylab = "Cumulative incidence", lty = 1, 
     main = "Aalen–Johansen cumulative incidence (cmprsk::cuminc)")
legend("topleft", legend = c("Response (cause 1)","Death (cause 2)"),
       col = 1:2, lty = 1, bty = "n")



## 4. ggplot
# convert cuminc object to tidy data.frame for ggplot
cuminc_to_df <- function(ci_obj) {
  # names(ci_obj) often includes "1" and "2" for causes, possibly with group suffixes
  nam <- names(ci_obj)
  df_list <- lapply(nam, function(nm) {
    comp <- ci_obj[[nm]]
    # cuminc elements typically have $time and $est
    data.frame(cause = nm,
               time = comp$time,
               est = comp$est,
               stringsAsFactors = FALSE)
  })
  dplyr::bind_rows(df_list)
}

ci_df <- cuminc_to_df(ci)

# clean cause names (optional): map "1" -> "response", "2" -> "death"
ci_df <- ci_df %>%
  mutate(cause = case_when(
    grepl("^1", cause) ~ "Response",
    grepl("^2", cause) ~ "Death",
    TRUE ~ cause
  ))

# ggplot step plot
## Hip Fracture vs Mortality after Dementia diagnose (Cumulative Incidence)
ggplot(ci_df, aes(x = time, y = est, color = cause, group = cause)) +
  geom_step() +
  labs(x = "Years since start", y = "Cumulative incidence", title = "Aalen–Johansen CIF (cmprsk)") +
  theme_minimal()
```

### Kuvio3: lonkkamurtuman jälkeen dementia kum.insidenssi

``` r



# example conversion and cleaning pipeline
df2 <- df %>%
  # convert to Date if necessary; adapt formats if not ISO
  mutate(across(c(DATE_BIRTH, DATE_DEATH, DATE_MIGRATION, DATE_EXPOSURE, DATE_RESPONSE, DATE_50),~ as.Date(.x))) %>%
  
  # compute start = later of DATE_50 and DATE_RESPONSE
  mutate(start_date = pmax(DATE_50, DATE_RESPONSE, na.rm = TRUE)) %>%
  
  # define administrative censoring date
  mutate(admin_censor = as.Date("2023-12-31")) %>%
  
  # compute earliest observed end date among possible endpoints and censoring
  mutate(raw_end = pmin(DATE_EXPOSURE, DATE_DEATH, DATE_MIGRATION, admin_censor, na.rm = TRUE)) %>%
  
  # if raw_end is earlier than start_date, set end_date = start_date (zero follow-up) and treat as censored
  mutate(end_date = if_else(raw_end <= start_date, start_date, raw_end)) %>%
  
  # define event indicator:
  # 1 = response (DATE_RESPONSE occurred after start), 2 = death (DATE_DEATH occurred after start), 0 = censored
  mutate(fstatus = case_when(
    !is.na(DATE_EXPOSURE) & (DATE_EXPOSURE > start_date) & (DATE_EXPOSURE <= raw_end) ~ 1L,
    !is.na(DATE_DEATH) & (DATE_DEATH > start_date) & (DATE_DEATH <= raw_end) ~ 2L,
    TRUE ~ 0L
  )) %>%
  
  # follow-up time in years (use days/365.25 to preserve fractional years)
  mutate(ftime = as.numeric(difftime(end_date, start_date, units = "days")) / 365.25) %>%
  
  # remove people with missing start_date (no exposure or DATE_50) or negative follow-up
  filter(!is.na(start_date)) %>%
  filter(ftime >= 0)

# quick check
table(df2$fstatus, useNA = "ifany")
summary(df2$ftime)

# 3. Aalen-Johanse estimater
# ensure numeric vectors
ftime <- df2$ftime
fstatus <- df2$fstatus

# overall CIFs (no grouping)
ci <- cuminc(ftime = ftime, fstatus = fstatus, cencode = 0)
names(ci)
names(ci) <- c("Response", "Death")

# base plotting (built-in)
plot(ci, xlab = "Years since start", ylab = "Cumulative incidence", lty = 1, 
     main = "Aalen–Johansen cumulative incidence (cmprsk::cuminc)")
legend("topleft", legend = c("EXPOSURE (cause 1)","Death (cause 2)"),
       col = 1:2, lty = 1, bty = "n")



## 4. ggplot
# convert cuminc object to tidy data.frame for ggplot
cuminc_to_df <- function(ci_obj) {
  # names(ci_obj) often includes "1" and "2" for causes, possibly with group suffixes
  nam <- names(ci_obj)
  df_list <- lapply(nam, function(nm) {
    comp <- ci_obj[[nm]]
    # cuminc elements typically have $time and $est
    data.frame(cause = nm,
               time = comp$time,
               est = comp$est,
               stringsAsFactors = FALSE)
  })
  dplyr::bind_rows(df_list)
}

ci_df <- cuminc_to_df(ci)

# clean cause names (optional): map "1" -> "response", "2" -> "death"
ci_df <- ci_df %>%
  mutate(cause = case_when(
    grepl("^1", cause) ~ "Dementia",
    grepl("^2", cause) ~ "Death",
    TRUE ~ cause
  ))

# ggplot step plot
ggplot(ci_df, aes(x = time, y = est, color = cause, group = cause)) +
  geom_step() +
  labs(x = "Years since start", y = "Cumulative incidence", title = "Dementia Aalen–Johansen CIF (cmprsk)") +
  theme_minimal()
```
