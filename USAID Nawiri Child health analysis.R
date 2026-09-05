# Name: Diramu Halake
# Project: USAID Nawiri Child Health & Nutrition Analysis

# STEP 1: LOAD LIBRARIES
library(ggplot2)
library(sandwich)
library(lmtest)
library(dplyr)

# STEP 2: IMPORT THE DATASET FROM GITHUB CSV
github_url <- "https://raw.githubusercontent.com/Diramu-Halake/USAID-Nawiri-Child-Health-Analysis/main/USAID_Nawiri_child_health_Sample%20data.csv"
df <- read.csv(github_url)

# STEP 3: DATA CLEANING & WHO OUTLIER FILTERING
df_clean <- df %>%
  filter(whz_score >= -6 & whz_score <= 6) %>%
  arrange(child_id, round)

# STEP 4: PANEL LAG & RELAPSE TRACKING ACROSS 12 ROUNDS
df_clean <- df_clean %>%
  group_by(child_id) %>%
  mutate(
    wasted_lag = lag(wasted, order_by = round)
  ) %>%
  ungroup()

df_clean$wasted_lag[is.na(df_clean$wasted_lag)] <- 0

# Flagging relapsed cases (recovered previously, slipped back into wasting)
df_clean$relapsed_case <- ifelse(df_clean$wasted_lag == 0 & df_clean$wasted == 1, 1, 0)

# STEP 5: PANEL LOGISTIC REGRESSION MODEL
logit_model <- glm(
  wasted ~ wasted_lag + season + milk_access + diarrhea_past + age_months,
  data = df_clean,
  family = binomial(link = "logit")
)

summary(logit_model)

# STEP 6: HOUSEHOLD CLUSTER-ROBUST STANDARD ERRORS
robust_se <- vcovCL(logit_model, cluster = df_clean$hh_id)
coeftest(logit_model, vcov = robust_se)

# STEP 7: SEASONAL VIZ OF RELAPSED WASTING
ggplot(df_clean, aes(x = season, fill = factor(relapsed_case))) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(
    title = "Proportion of Relapsed Wasting Episodes by Season",
    subtitle = "USAID Nawiri Longitudinal Panel (Isiolo & Marsabit)",
    x = "Survey Season",
    y = "Proportion of Children",
    fill = "Relapsed Case\n(0 = Stable/New, 1 = Relapsed)"
  )