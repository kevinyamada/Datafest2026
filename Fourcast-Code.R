options(scipen = 999)

library(DBI)
library(duckdb)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(tidyr)
library(forcats)
library(scales)
library(ranger)
library(pROC)

base_dir <- "C:/Users/kevin/Downloads/Students/Students/data/data_files"
out_dir <- file.path(base_dir, "type2_diabetes_gap_outputs")

con <- dbConnect(duckdb::duckdb(), dbdir = file.path(out_dir, "type2_diabetes_gap.duckdb"))

dbExecute(con, "
CREATE OR REPLACE TABLE diabetes_analysis_base_observed AS
SELECT
  *,
  DATE_DIFF('day', FirstDate, DATE '2025-12-31') AS ObservationDays
FROM diabetes_analysis_base
WHERE FirstDate IS NOT NULL
")

analysis_data <- dbGetQuery(con, "
SELECT
  LongGap180,
  FirstEncounterType,
  FirstVisitTypeDescription,
  FirstDepartmentType,
  FirstDepartmentSpecialty,
  PatientBirthYearBin,
  SexAssignedAtBirth,
  OmbRace,
  OmbEthnicity,
  MyChartStatus,
  SmokingStatus,
  HasSdohScreening,
  SdohDomainsAnswered,
  AnySocialRisk,
  TransportationRisk,
  FoodRisk,
  HousingRisk,
  FinancialRisk,
  StressRisk,
  DepressionRisk,
  SocialConnectionRisk,
  UtilitiesRisk,
  SocialRiskCount
FROM diabetes_analysis_base_observed
WHERE LongGap180 IS NOT NULL
  AND ObservationDays >= 365
")

set.seed(123)

analysis_data <- analysis_data |>
  mutate(
    LongGap180 = factor(LongGap180, levels = c(0, 1), labels = c("NoLongGap", "LongGap")),
    FirstEncounterType = fct_lump_n(factor(FirstEncounterType), n = 20),
    FirstVisitTypeDescription = fct_lump_n(factor(FirstVisitTypeDescription), n = 30),
    FirstDepartmentType = fct_lump_n(factor(FirstDepartmentType), n = 10),
    FirstDepartmentSpecialty = fct_lump_n(factor(FirstDepartmentSpecialty), n = 30),
    PatientBirthYearBin = fct_lump_n(factor(PatientBirthYearBin), n = 25),
    SexAssignedAtBirth = fct_lump_n(factor(SexAssignedAtBirth), n = 10),
    OmbRace = fct_lump_n(factor(OmbRace), n = 15),
    OmbEthnicity = fct_lump_n(factor(OmbEthnicity), n = 10),
    MyChartStatus = fct_lump_n(factor(MyChartStatus), n = 15),
    SmokingStatus = fct_lump_n(factor(SmokingStatus), n = 15),
    HasSdohScreening = factor(HasSdohScreening),
    AnySocialRisk = factor(AnySocialRisk),
    TransportationRisk = factor(TransportationRisk),
    FoodRisk = factor(FoodRisk),
    HousingRisk = factor(HousingRisk),
    FinancialRisk = factor(FinancialRisk),
    StressRisk = factor(StressRisk),
    DepressionRisk = factor(DepressionRisk),
    SocialConnectionRisk = factor(SocialConnectionRisk),
    UtilitiesRisk = factor(UtilitiesRisk)
  ) |>
  na.omit()

keep_cols <- names(analysis_data)[sapply(analysis_data, function(x) {
  if (is.factor(x)) nlevels(droplevels(x)) >= 2 else length(unique(x[!is.na(x)])) >= 2
})]

analysis_data <- analysis_data[, keep_cols]

if (nrow(analysis_data) > 200000) {
  analysis_data_model <- analysis_data |> slice_sample(n = 200000)
} else {
  analysis_data_model <- analysis_data
}

train_id <- sample(seq_len(nrow(analysis_data_model)), size = floor(0.75 * nrow(analysis_data_model)))

train_data <- analysis_data_model[train_id, ]
test_data <- analysis_data_model[-train_id, ]

train_data <- train_data |> mutate(across(where(is.factor), droplevels))
test_data <- test_data |> mutate(across(where(is.factor), droplevels))

rf_fit_clean <- ranger(
  LongGap180 ~ .,
  data = train_data,
  probability = TRUE,
  importance = "permutation",
  num.trees = 500,
  mtry = floor(sqrt(ncol(train_data) - 1)),
  min.node.size = 25,
  seed = 123
)

rf_pred_clean <- predict(rf_fit_clean, data = test_data)$predictions[, "LongGap"]

rf_auc_clean <- as.numeric(auc(response = test_data$LongGap180, predictor = rf_pred_clean, levels = c("NoLongGap", "LongGap")))

rf_pred_class_clean <- ifelse(rf_pred_clean >= 0.5, "LongGap", "NoLongGap")

confusion_clean <- table(
  actual = test_data$LongGap180,
  predicted = factor(rf_pred_class_clean, levels = c("NoLongGap", "LongGap"))
)

rf_metrics_clean <- tibble(
  auc = rf_auc_clean,
  accuracy = mean(rf_pred_class_clean == as.character(test_data$LongGap180)),
  test_rows = nrow(test_data),
  train_rows = nrow(train_data),
  observation_filter = "ObservationDays >= 365",
  removed_first_encounter_year = TRUE
)

write_csv(rf_metrics_clean, file.path(out_dir, "28_clean_rf_metrics_no_year_observed365.csv"))
write_csv(as.data.frame(confusion_clean), file.path(out_dir, "29_clean_rf_confusion_matrix_no_year_observed365.csv"))
print(rf_metrics_clean)
print(confusion_clean)

importance_table_clean <- tibble(
  variable = names(rf_fit_clean$variable.importance),
  importance = as.numeric(rf_fit_clean$variable.importance)
) |>
  arrange(desc(importance))

write_csv(importance_table_clean, file.path(out_dir, "30_clean_rf_variable_importance_no_year_observed365.csv"))
print(importance_table_clean)

test_scored_clean <- test_data |>
  mutate(
    predicted_probability = rf_pred_clean,
    risk_group = case_when(
      predicted_probability < quantile(predicted_probability, 1 / 3, na.rm = TRUE) ~ "Low",
      predicted_probability < quantile(predicted_probability, 2 / 3, na.rm = TRUE) ~ "Medium",
      TRUE ~ "High"
    )
  )

risk_table_clean <- test_scored_clean |>
  group_by(risk_group) |>
  summarise(
    journeys = n(),
    actual_long_gap_180_rate = mean(LongGap180 == "LongGap"),
    average_predicted_probability = mean(predicted_probability),
    .groups = "drop"
  ) |>
  arrange(match(risk_group, c("Low", "Medium", "High")))

write_csv(risk_table_clean, file.path(out_dir, "31_clean_risk_group_performance_no_year_observed365.csv"))
print(risk_table_clean)

observed_summary <- dbGetQuery(con, "
SELECT
  COUNT(*) AS diabetes_journeys_observed365,
  COUNT(DISTINCT PatientDurableKey) AS diabetes_patients_observed365,
  AVG(LongGap90) AS long_gap_90_rate,
  AVG(LongGap180) AS long_gap_180_rate,
  AVG(LongGap365) AS long_gap_365_rate,
  MEDIAN(MaxGapDays) AS median_max_gap_days,
  AVG(MaxGapDays) AS mean_max_gap_days,
  MEDIAN(NumEncounters) AS median_encounters,
  MEDIAN(NumVisitDays) AS median_visit_days,
  AVG(HasSdohScreening) AS sdoh_screened_rate,
  AVG(AnySocialRisk) AS any_social_risk_rate,
  AVG(SocialRiskCount) AS mean_social_risk_count
FROM diabetes_analysis_base_observed
WHERE ObservationDays >= 365
")

write_csv(observed_summary, file.path(out_dir, "32_observed365_main_summary.csv"))
print(observed_summary)

by_first_type_clean <- dbGetQuery(con, "
SELECT
  FirstEncounterType,
  COUNT(*) AS journeys,
  COUNT(DISTINCT PatientDurableKey) AS patients,
  AVG(LongGap180) AS long_gap_180_rate,
  MEDIAN(MaxGapDays) AS median_max_gap_days
FROM diabetes_analysis_base_observed
WHERE ObservationDays >= 365
GROUP BY FirstEncounterType
HAVING COUNT(*) >= 50
ORDER BY long_gap_180_rate DESC
")

write_csv(by_first_type_clean, file.path(out_dir, "33_observed365_gap_by_first_encounter_type.csv"))
print(by_first_type_clean)

by_department_specialty_clean <- dbGetQuery(con, "
SELECT
  FirstDepartmentSpecialty,
  COUNT(*) AS journeys,
  COUNT(DISTINCT PatientDurableKey) AS patients,
  AVG(LongGap180) AS long_gap_180_rate,
  MEDIAN(MaxGapDays) AS median_max_gap_days
FROM diabetes_analysis_base_observed
WHERE ObservationDays >= 365
GROUP BY FirstDepartmentSpecialty
HAVING COUNT(*) >= 50
ORDER BY long_gap_180_rate DESC
")

write_csv(by_department_specialty_clean, file.path(out_dir, "34_observed365_gap_by_department_specialty.csv"))
print(by_department_specialty_clean)

p1 <- importance_table_clean |>
  head(15) |>
  mutate(variable = fct_reorder(variable, importance)) |>
  ggplot(aes(x = variable, y = importance)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Predictors of 180-day follow-up gaps after removing time-window effects",
    x = "Variable",
    y = "Permutation importance"
  ) +
  theme_minimal()

ggsave(file.path(out_dir, "plot_08_clean_variable_importance_no_year_observed365.png"), p1, width = 8, height = 6, dpi = 300)

p2 <- risk_table_clean |>
  mutate(risk_group = factor(risk_group, levels = c("Low", "Medium", "High"))) |>
  ggplot(aes(x = risk_group, y = actual_long_gap_180_rate)) +
  geom_col() +
  geom_text(aes(label = paste0(round(actual_long_gap_180_rate * 100, 1), "%")), vjust = -0.4, size = 5) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  labs(
    title = "Clean model risk tiers for Type 2 diabetes follow-up gaps",
    subtitle = "Filtered to journeys with at least 365 days of observation; FirstEncounterYear removed",
    x = "Predicted risk group",
    y = "Actual gap > 180 days rate"
  ) +
  theme_minimal()

ggsave(file.path(out_dir, "plot_09_clean_risk_group_gap_rates_no_year_observed365.png"), p2, width = 7, height = 5, dpi = 300)

p3 <- by_first_type_clean |>
  mutate(FirstEncounterType = str_trunc(FirstEncounterType, 45)) |>
  mutate(FirstEncounterType = fct_reorder(FirstEncounterType, long_gap_180_rate)) |>
  ggplot(aes(x = FirstEncounterType, y = long_gap_180_rate)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  labs(
    title = "Observed 365+ days: follow-up gaps by first encounter type",
    x = "First encounter type",
    y = "Journeys with gap > 180 days"
  ) +
  theme_minimal()

ggsave(file.path(out_dir, "plot_10_observed365_first_encounter_gap_rates.png"), p3, width = 8, height = 6, dpi = 300)

dbDisconnect(con, shutdown = TRUE)

options(scipen = 999)

library(DBI)
library(duckdb)
library(dplyr)
library(ggplot2)
library(readr)
library(forcats)
library(scales)
library(stringr)

base_dir <- "C:/Users/kevin/Downloads/Students/Students/data/data_files"
out_dir <- file.path(base_dir, "type2_diabetes_gap_outputs")

con <- dbConnect(duckdb::duckdb(), dbdir = file.path(out_dir, "type2_diabetes_gap.duckdb"), read_only = TRUE)

theme_sv <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 11),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(size = 11),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.position = "none"
    )
}

pct_lab <- function(x) paste0(round(x * 100, 1), "%")

dist_data <- dbGetQuery(con, "
SELECT
  MaxGapDays
FROM diabetes_analysis_base_observed
WHERE ObservationDays >= 365
  AND MaxGapDays IS NOT NULL
  AND MaxGapDays BETWEEN 0 AND 1460
")

dist_stats <- dbGetQuery(con, "
SELECT
  MEDIAN(MaxGapDays) AS median_gap,
  AVG(MaxGapDays) AS mean_gap
FROM diabetes_analysis_base_observed
WHERE ObservationDays >= 365
  AND MaxGapDays IS NOT NULL
  AND MaxGapDays BETWEEN 0 AND 1460
")

median_gap <- dist_stats$median_gap[1]
mean_gap <- dist_stats$mean_gap[1]

p_dist <- ggplot(dist_data, aes(x = MaxGapDays)) +
  geom_histogram(bins = 45, fill = "#2A9D8F", color = "white", alpha = 0.95) +
  geom_vline(xintercept = median_gap, linewidth = 1.2, color = "#E76F51") +
  annotate("text", x = median_gap, y = Inf, label = paste("Median:", round(median_gap), "days"), vjust = 1.5, hjust = -0.05, color = "#E76F51", size = 4.5, fontface = "bold") +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Distribution of maximum follow up gaps",
    subtitle = "Type 2 diabetes journeys with at least 365 days of observation",
    x = "Maximum gap in days",
    y = "Number of journeys"
  ) +
  theme_sv()

ggsave(file.path(out_dir, "plot_16_distribution_of_max_gap_better.png"), p_dist, width = 9, height = 6, dpi = 300)

risk_data <- read_csv(file.path(out_dir, "31_clean_risk_group_performance_no_year_observed365.csv"), show_col_types = FALSE) |>
  mutate(
    risk_group = factor(risk_group, levels = c("Low", "Medium", "High")),
    fill_col = c("#6CC24A", "#F4B400", "#D1495B")
  )

p_risk <- ggplot(risk_data, aes(x = risk_group, y = actual_long_gap_180_rate, fill = risk_group)) +
  geom_col(width = 0.7, alpha = 0.95) +
  geom_text(aes(label = pct_lab(actual_long_gap_180_rate)), vjust = -0.5, size = 5.2, fontface = "bold") +
  scale_fill_manual(values = c("Low" = "#6CC24A", "Medium" = "#F4B400", "High" = "#D1495B")) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  labs(
    title = "Risk tiers separate patients by follow up risk",
    subtitle = "Actual share of journeys with gaps greater than 180 days",
    x = "Risk tier",
    y = "Gap rate"
  ) +
  theme_sv()

ggsave(file.path(out_dir, "plot_17_risk_tiers_better.png"), p_risk, width = 8, height = 5.5, dpi = 300)

predictor_data <- read_csv(file.path(out_dir, "30_clean_rf_variable_importance_no_year_observed365.csv"), show_col_types = FALSE) |>
  slice_max(order_by = importance, n = 10) |>
  mutate(
    variable = str_replace_all(variable, "_", " "),
    variable = str_replace_all(variable, "(?<=.)([A-Z])", " \\1"),
    variable = str_trim(variable),
    variable = str_to_title(variable),
    variable = fct_reorder(variable, importance),
    rank = row_number()
  )

p_predictors <- ggplot(predictor_data, aes(x = variable, y = importance, fill = importance)) +
  geom_col(width = 0.75) +
  coord_flip() +
  scale_fill_gradient(low = "#8ECAE6", high = "#1D3557") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Top predictors of delayed follow up",
    subtitle = "Random forest variable importance",
    x = NULL,
    y = "Importance"
  ) +
  theme_sv()

ggsave(file.path(out_dir, "plot_18_predictors_better.png"), p_predictors, width = 9, height = 6.5, dpi = 300)

first_type_data <- dbGetQuery(con, "
SELECT
  FirstEncounterType,
  COUNT(*) AS journeys,
  AVG(LongGap180) AS long_gap_180_rate
FROM diabetes_analysis_base_observed
WHERE ObservationDays >= 365
GROUP BY FirstEncounterType
HAVING COUNT(*) >= 50
") |>
  mutate(
    FirstEncounterType = str_trim(FirstEncounterType),
    FirstEncounterType = fct_reorder(FirstEncounterType, long_gap_180_rate)
  )

p_first_type <- ggplot(first_type_data, aes(x = FirstEncounterType, y = long_gap_180_rate, fill = long_gap_180_rate)) +
  geom_col(width = 0.75) +
  coord_flip() +
  geom_text(aes(label = pct_lab(long_gap_180_rate)), hjust = -0.1, size = 4.2, fontface = "bold") +
  scale_fill_gradient(low = "#A8DADC", high = "#457B9D") +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  labs(
    title = "Follow up gaps by first encounter type",
    subtitle = "Type 2 diabetes journeys with at least 365 days of observation",
    x = "First encounter type",
    y = "Gap rate"
  ) +
  theme_sv()

ggsave(file.path(out_dir, "plot_19_first_encounter_type_better.png"), p_first_type, width = 9, height = 6.5, dpi = 300)
base_dir <- "C:/Users/kevin/Downloads/Students/Students/data/data_files"
out_dir <- file.path(base_dir, "type2_diabetes_gap_outputs")

con <- dbConnect(duckdb::duckdb(), dbdir = file.path(out_dir, "type2_diabetes_gap.duckdb"), read_only = TRUE)

mychart_data <- dbGetQuery(con, "
SELECT
  CASE
    WHEN MyChartStatus IN ('Activated', 'Pending Activation', 'Inactivated') THEN 'Activated'
    WHEN MyChartStatus = 'Patient Declined' THEN 'Patient declined'
    ELSE NULL
  END AS MyChartGroup,
  COUNT(*) AS journeys,
  AVG(LongGap180) AS long_gap_180_rate
FROM diabetes_analysis_base_observed
WHERE ObservationDays >= 365
GROUP BY MyChartGroup
HAVING MyChartGroup IS NOT NULL
")

mychart_data <- mychart_data |>
  mutate(
    MyChartGroup = factor(MyChartGroup, levels = c("Activated", "Patient declined"))
  )

write_csv(mychart_data, file.path(out_dir, "40_observed365_gap_by_mychart_combined.csv"))

p_mychart <- ggplot(mychart_data, aes(x = MyChartGroup, y = long_gap_180_rate, fill = MyChartGroup)) +
  geom_col(width = 0.65, alpha = 0.95) +
  geom_text(aes(label = paste0(round(long_gap_180_rate * 100, 1), "%")), vjust = -0.5, size = 5.2, fontface = "bold") +
  scale_fill_manual(values = c(
    "Activated" = "#add8e6",
    "Patient declined" = "#FFADAD"
  )) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  labs(
    title = "Type 2 diabetes follow up gaps by MyChart status",
    x = "MyChart status",
    y = "Journeys with gap > 180 days"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  )

ggsave(file.path(out_dir, "plot_22_mychart_combined_gap_rate.png"), p_mychart, width = 8, height = 5.5, dpi = 300)

social_data <- dbGetQuery(con, "
SELECT
  AnySocialRisk,
  COUNT(*) AS journeys,
  AVG(LongGap180) AS long_gap_180_rate
FROM diabetes_analysis_base_observed
WHERE ObservationDays >= 365
GROUP BY AnySocialRisk
") |>
  mutate(
    AnySocialRisk = factor(AnySocialRisk, levels = c(0, 1), labels = c("No social risk", "Any social risk"))
  )

p_social <- ggplot(social_data, aes(x = AnySocialRisk, y = long_gap_180_rate, fill = AnySocialRisk)) +
  geom_col(width = 0.65, alpha = 0.95) +
  geom_text(aes(label = pct_lab(long_gap_180_rate)), vjust = -0.5, size = 5.2, fontface = "bold") +
  scale_fill_manual(values = c("No social risk" = "#4CC9F0", "Any social risk" = "#F72585")) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  labs(
    title = "Follow up gaps by social risk",
    subtitle = "Recorded social risk compared with no recorded social risk",
    x = NULL,
    y = "Gap rate"
  ) +
  theme_sv()

ggsave(file.path(out_dir, "plot_21_social_risk_better.png"), p_social, width = 7.5, height = 5.5, dpi = 300)

dbDisconnect(con, shutdown = TRUE)



options(scipen = 999)

library(DBI)
library(duckdb)
library(dplyr)
library(ggplot2)
library(readr)
library(forcats)
library(scales)
library(stringr)
library(tidyr)

base_dir <- "C:/Users/kevin/Downloads/Students/Students/data/data_files"
out_dir <- file.path(base_dir, "type2_diabetes_gap_outputs")

con <- dbConnect(duckdb::duckdb(), dbdir = file.path(out_dir, "type2_diabetes_gap.duckdb"), read_only = TRUE)

theme_win <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18, color = "#1B1B1B"),
      plot.subtitle = element_text(size = 11, color = "#4D4D4D"),
      axis.title = element_text(face = "bold", color = "#1B1B1B"),
      axis.text = element_text(color = "#333333"),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.position = "none"
    )
}

pct_lab <- function(x) paste0(round(x * 100, 1), "%")

dist_data <- dbGetQuery(con, "
SELECT
  MaxGapDays,
  CASE WHEN MaxGapDays > 180 THEN 'Gap over 180 days' ELSE 'Gap 180 days or less' END AS GapGroup
FROM diabetes_analysis_base_observed
WHERE ObservationDays >= 365
  AND MaxGapDays IS NOT NULL
  AND MaxGapDays BETWEEN 0 AND 1460
")

dist_stats <- dbGetQuery(con, "
SELECT
  MEDIAN(MaxGapDays) AS median_gap,
  AVG(LongGap180) AS long_gap_180_rate
FROM diabetes_analysis_base_observed
WHERE ObservationDays >= 365
  AND MaxGapDays IS NOT NULL
")

p_dist <- ggplot(dist_data, aes(x = MaxGapDays, fill = GapGroup)) +
  geom_histogram(bins = 55, color = "white", alpha = 0.95) +
  geom_vline(xintercept = 180, color = "#D62828", linewidth = 1.3) +
  geom_vline(xintercept = dist_stats$median_gap[1], color = "#003049", linewidth = 1.1, linetype = "dotted") +
  annotate("label", x = 180, y = Inf, label = "180 day threshold", vjust = 1.4, hjust = -0.03, fill = "white", color = "#D62828", label.size = 0) +
  annotate("label", x = dist_stats$median_gap[1], y = Inf, label = paste0("Median max gap: ", round(dist_stats$median_gap[1]), " days"), vjust = 3.2, hjust = -0.03, fill = "white", color = "#003049", label.size = 0) +
  scale_fill_manual(values = c("Gap 180 days or less" = "#8ECAE6", "Gap over 180 days" = "#FFB703")) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Most Type 2 diabetes journeys have long follow up gaps",
    subtitle = paste0(pct_lab(dist_stats$long_gap_180_rate[1]), " of observed journeys had at least one gap over 180 days"),
    x = "Maximum gap in days",
    y = "Number of journeys"
  ) +
  theme_win() +
  theme(legend.position = "top")

ggsave(file.path(out_dir, "final_01_distribution_max_gap.png"), p_dist, width = 10, height = 6, dpi = 300)

risk_data <- read_csv(file.path(out_dir, "31_clean_risk_group_performance_no_year_observed365.csv"), show_col_types = FALSE) |>
  mutate(
    risk_group = factor(risk_group, levels = c("Low", "Medium", "High")),
    label = pct_lab(actual_long_gap_180_rate)
  )

p_risk <- ggplot(risk_data, aes(x = risk_group, y = actual_long_gap_180_rate, fill = risk_group)) +
  geom_col(width = 0.62, alpha = 0.95) +
  geom_point(aes(y = actual_long_gap_180_rate), size = 11, color = "white") +
  geom_text(aes(label = label), color = "#1B1B1B", fontface = "bold", size = 5) +
  scale_fill_manual(values = c("Low" = "#2A9D8F", "Medium" = "#F4A261", "High" = "#E63946")) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  labs(
    title = "The model creates practical risk tiers",
    subtitle = "Actual 180 day gap rate rises sharply from low to high risk",
    x = "Predicted risk tier",
    y = "Actual gap rate"
  ) +
  theme_win()

ggsave(file.path(out_dir, "final_02_risk_tiers.png"), p_risk, width = 8.5, height = 5.8, dpi = 300)

predictor_data <- read_csv(file.path(out_dir, "30_clean_rf_variable_importance_no_year_observed365.csv"), show_col_types = FALSE) |>
  slice_max(order_by = importance, n = 10) |>
  mutate(
    variable = str_replace_all(variable, "_", " "),
    variable = str_replace_all(variable, "(?<=.)([A-Z])", " \\1"),
    variable = str_squish(str_to_title(variable)),
    variable = recode(
      variable,
      "Sdoh Domains Answered" = "SDOH Domains Answered",
      "Has Sdoh Screening" = "Has SDOH Screening"
    ),
    variable = fct_reorder(variable, importance)
  )

p_predictors <- ggplot(predictor_data, aes(x = importance, y = variable)) +
  geom_segment(aes(x = 0, xend = importance, y = variable, yend = variable), color = "#A8DADC", linewidth = 2.2, lineend = "round") +
  geom_point(aes(size = importance, color = importance), alpha = 0.95) +
  scale_color_gradient(low = "#457B9D", high = "#E63946") +
  scale_size(range = c(4, 10)) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "What information drives the risk score?",
    subtitle = "Top model predictors after filtering to fair observation time",
    x = "Variable importance",
    y = NULL
  ) +
  theme_win()

ggsave(file.path(out_dir, "final_03_predictors_lollipop.png"), p_predictors, width = 10, height = 6.5, dpi = 300)

first_type_data <- dbGetQuery(con, "
SELECT
  FirstEncounterType,
  COUNT(*) AS journeys,
  AVG(LongGap180) AS long_gap_180_rate
FROM diabetes_analysis_base_observed
WHERE ObservationDays >= 365
GROUP BY FirstEncounterType
HAVING COUNT(*) >= 50
ORDER BY long_gap_180_rate DESC
") |>
  mutate(
    FirstEncounterType = str_trim(FirstEncounterType),
    FirstEncounterType = fct_reorder(FirstEncounterType, long_gap_180_rate),
    label = pct_lab(long_gap_180_rate)
  )

p_first_type <- ggplot(first_type_data, aes(x = long_gap_180_rate, y = FirstEncounterType)) +
  geom_vline(xintercept = 0.748, color = "#999999", linetype = "dashed", linewidth = 1) +
  geom_point(aes(size = journeys, color = long_gap_180_rate), alpha = 0.95) +
  geom_text(aes(label = label), hjust = -0.35, size = 4, fontface = "bold", color = "#222222") +
  scale_color_gradient(low = "#2A9D8F", high = "#D62828") +
  scale_size(range = c(5, 14), labels = comma) +
  scale_x_continuous(labels = percent_format(), limits = c(0, 1)) +
  labs(
    title = "Entry point into care is linked to later follow up gaps",
    subtitle = "Dot size shows number of journeys. Dashed line shows overall 180 day gap rate",
    x = "Gap rate",
    y = "First encounter type"
  ) +
  theme_win()

ggsave(file.path(out_dir, "final_04_first_encounter_type_bubble.png"), p_first_type, width = 10, height = 6.5, dpi = 300)

mychart_data <- dbGetQuery(con, "
SELECT
  CASE
    WHEN MyChartStatus IN ('Activated', 'Pending Activation', 'Inactivated') THEN 'Activated, pending, or inactivated'
    WHEN MyChartStatus = 'Patient Declined' THEN 'Patient declined'
    ELSE NULL
  END AS MyChartGroup,
  COUNT(*) AS journeys,
  AVG(LongGap180) AS long_gap_180_rate
FROM diabetes_analysis_base_observed
WHERE ObservationDays >= 365
GROUP BY MyChartGroup
HAVING MyChartGroup IS NOT NULL
") |>
  mutate(
    MyChartGroup = factor(MyChartGroup, levels = c("Patient declined", "Activated, pending, or inactivated")),
    label = pct_lab(long_gap_180_rate)
  )

p_mychart <- ggplot(mychart_data, aes(x = MyChartGroup, y = long_gap_180_rate, fill = MyChartGroup)) +
  geom_col(width = 0.5, alpha = 0.96) +
  geom_text(aes(label = label), vjust = -0.45, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Patient declined" = "#8338EC", "Activated, pending, or inactivated" = "#3A86FF")) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  labs(
    title = "Follow up gaps by MyChart status",
    subtitle = "MyChart status is descriptive, not causal",
    x = NULL,
    y = "Gap rate"
  ) +
  theme_win() +
  theme(axis.text.x = element_text(size = 12))

ggsave(file.path(out_dir, "final_05_mychart_grouped.png"), p_mychart, width = 8.5, height = 5.5, dpi = 300)

social_data <- dbGetQuery(con, "
SELECT
  AnySocialRisk,
  COUNT(*) AS journeys,
  AVG(LongGap180) AS long_gap_180_rate
FROM diabetes_analysis_base_observed
WHERE ObservationDays >= 365
GROUP BY AnySocialRisk
") |>
  mutate(
    SocialRiskGroup = factor(AnySocialRisk, levels = c(0, 1), labels = c("No recorded social risk", "Any recorded social risk")),
    label = pct_lab(long_gap_180_rate)
  )

p_social <- ggplot(social_data, aes(x = SocialRiskGroup, y = long_gap_180_rate, fill = SocialRiskGroup)) +
  geom_col(width = 0.55, alpha = 0.96) +
  geom_text(aes(label = label), vjust = -0.45, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("No recorded social risk" = "#4CC9F0", "Any recorded social risk" = "#F72585")) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  labs(
    title = "Recorded social risk alone does not explain follow up gaps",
    subtitle = "This is a raw comparison and does not hold other factors constant",
    x = NULL,
    y = "Gap rate"
  ) +
  theme_win() +
  theme(axis.text.x = element_text(size = 12))

ggsave(file.path(out_dir, "final_06_social_risk_grouped.png"), p_social, width = 8.5, height = 5.5, dpi = 300)

dbDisconnect(con, shutdown = TRUE)

options(scipen = 999)

library(DBI)
library(duckdb)
library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(grid)

base_dir <- "C:/Users/kevin/Downloads/Students/Students/data/data_files"
out_dir <- file.path(base_dir, "type2_diabetes_gap_outputs")

con <- dbConnect(duckdb::duckdb(), dbdir = file.path(out_dir, "type2_diabetes_gap.duckdb"), read_only = TRUE)

social_data <- dbGetQuery(con, "
SELECT
  AnySocialRisk,
  COUNT(*) AS journeys,
  AVG(LongGap180) AS long_gap_180_rate
FROM diabetes_analysis_base_observed
WHERE ObservationDays >= 365
GROUP BY AnySocialRisk
") |>
  mutate(
    SocialRiskGroup = factor(
      AnySocialRisk,
      levels = c(0, 1),
      labels = c('No social risk', 'Any social risk')
    )
  ) |>
  arrange(SocialRiskGroup)

write_csv(social_data, file.path(out_dir, "40_social_risk_line_clean_style.csv"))

diff_value <- social_data$long_gap_180_rate[2] - social_data$long_gap_180_rate[1]

p_social_line <- ggplot(social_data, aes(x = SocialRiskGroup, y = long_gap_180_rate, group = 1)) +
  geom_line(color = "#2A9D8F", linewidth = 1.4) +
  geom_point(color = "#2A9D8F", size = 4) +
  geom_text(
    aes(label = paste0(round(long_gap_180_rate * 100, 1), "%")),
    vjust = -1.1,
    size = 4.5,
    fontface = "bold",
    color = "black"
  ) +
  annotate(
    "curve",
    x = 1.23,
    xend = 1.77,
    y = 0.728,
    yend = 0.748,
    curvature = 0.45,
    arrow = arrow(length = unit(0.18, "cm")),
    color = "gray35",
    linewidth = 0.9
  ) +
  annotate(
    "text",
    x = 1.5,
    y = 0.57,
    label = paste0("+", round(diff_value * 100, 1), "%"),
    size = 5,
    fontface = "bold",
    color = "black"
  ) +
  annotate(
    "text",
    x = 1.5,
    y = 0.50,
    label = "Higher observed gap rate\namong patients with recorded social risk",
    size = 3.8,
    color = "black",
    lineheight = 0.95
  ) +
  scale_y_continuous(
    labels = percent_format(),
    limits = c(0, 1.05),
    breaks = seq(0, 1, 0.25)
  ) +
  labs(
    title = "Follow up gaps by social risk",
    subtitle = "Recorded social risk compared with no recorded social risk",
    x = "Social risk",
    y = "Journeys with gap > 180 days"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none"
  )

ggsave(file.path(out_dir, "plot_22_social_risk_line_white_clean.png"), p_social_line, width = 10, height = 6, dpi = 300)

dbDisconnect(con, shutdown = TRUE)
