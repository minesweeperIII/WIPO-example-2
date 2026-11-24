# prepare_data.R — Run locally once to generate data/app_plots.rds
rm(list = ls())
library(tidyverse)
library(haven)

## 1. Load and clean data (all raw files inside data/)
### ISIC classification
ISIC_Rev_4 <- read_csv("data/ISIC_Rev_4_english_structure.csv")

ISIC_2digit <- ISIC_Rev_4 %>%
  mutate(Sector = ifelse(grepl("^[A-Za-z]$", Code), Description, NA_character_)) %>%
  fill(Sector, .direction = "down") %>%
  filter(nchar(Code) == 2) %>%
  add_row(Code = "00", Description = "Unemployment", Sector = "Unemployment")

creative_ISIC <- c(
  "7310", "7320", "7110", "5911", "5912", "5913", "5914", "6010", "6020", "6391",
  "7420", "4761", "5811", "5813", "5819", "9000", "9102", "9103", "7410", "1391",
  "1393", "1629", "2310", "2393", "3211", "3212", "1820", "5920", "8542", "2619",
  "5820", "6201", "6202", "7210", "7220", "7740"
)

recode_type_of_empl <- function(x) {
  case_when(
    x == 1 ~ "Owner",
    x == 2 ~ "Self-employed",
    x == 3 ~ "Family worker",
    x == 4 ~ "Cooperative member",
    x == 5 ~ "Employee",
    TRUE   ~ NA_character_
  )
}

### LFS 2016
lfs_2016 <- read_dta("data/data2016/lfs_2016_final_gui.dta", encoding = "UTF-8") %>%
  mutate(
    year = 2016,
    rural = ifelse(ttnt == 2, 1, 0),
    female = ifelse(c3 == 1, 0, 1),
    edu_cat = as.factor(ifelse(c12 %in% 1:9, c12, NA)),
    empl_stat = ifelse(hdkt == 1, "E", ifelse(hdkt == 2, "U", "Inactive")),
    college = ifelse(c12 %in% 7:9, 1, 0),
    ISIC = ifelse(empl_stat == "U", "00", ifelse(nchar(c23) == 4, as.character(c23), paste0("0", c23))),
    ISIC_unemployed = ifelse(c58 == 2, NA_character_, ifelse(nchar(c62) == 4, as.character(c62), paste0("0", c62))),
    industry = ifelse(empl_stat == "U", "00", substr(ISIC, 1, 2)),
    creative = ifelse(empl_stat == "U", 0, ifelse(ISIC %in% creative_ISIC, 1, 0)),
    permanent_contract = ifelse(c29 == 1 & empl_stat == "E", 1, 0),
    social_insurance = NA_real_,
    type_of_empl = recode_type_of_empl(c28),
    wage = c40,
    weekly_hours = c42,
    second_job = ifelse(c43 == 1, 1, 0),
    weight = weigh_final
  ) %>%
  filter(hdkt %in% c(1, 2) & c5 %in% 15:65 & !is.na(ISIC)) %>%
  select(
    year, rural, female, edu_cat, college, empl_stat, ISIC, ISIC_unemployed,
    industry, creative, permanent_contract, social_insurance, type_of_empl,
    wage, weekly_hours, second_job, weight
  )

### LFS 2017
lfs_2017 <- read_dta("data/data2017/LFS_2017.dta") %>%
  mutate(
    year = 2017,
    rural = ifelse(TTNT == 2, 1, 0),
    female = ifelse(C3 == 1, 0, 1),
    empl_stat = ifelse(HDKT == 1, "E", ifelse(HDKT == 2, "U", "Inactive")),
    edu_cat = as.factor(ifelse(C14 %in% 1:9, C14, NA)),
    college = ifelse(C14 %in% 7:9, 1, 0),
    ISIC = ifelse(empl_stat == "U", "00", ifelse(nchar(C25) == 4, as.character(C25), paste0("0", C25))),
    ISIC_unemployed = ifelse(C58 == 2, NA_character_, ifelse(nchar(C62) == 4, as.character(C62), paste0("0", C62))),
    industry = ifelse(empl_stat == "U", "00", substr(ISIC, 1, 2)),
    creative = ifelse(empl_stat == "U", 0, ifelse(ISIC %in% creative_ISIC, 1, 0)),
    wage = C39,
    weekly_hours = C41,
    permanent_contract = ifelse(C31 == 1 & empl_stat == "E", 1, 0),
    social_insurance = ifelse(C34 == 1 & empl_stat == "E", 1, 0),
    type_of_empl = recode_type_of_empl(C30),
    second_job = ifelse(!is.na(C40B) | !is.na(C41B), 1, 0),
    weight = weigh_final
  ) %>%
  filter(HDKT %in% c(1, 2) & !is.na(ISIC) & C5 %in% 15:65) %>%
  select(
    year, rural, female, edu_cat, college, empl_stat, ISIC, ISIC_unemployed,
    industry, creative, permanent_contract, social_insurance, type_of_empl,
    wage, weekly_hours, second_job, weight
  )

### LFS 2018
lfs_2018 <- read_dta("data/data2018/data 2018.dta", encoding = "UTF-8") %>%
  mutate(
    year = 2018,
    rural = ifelse(TTNT == 2, 1, 0),
    female = ifelse(C3 == 1, 0, 1),
    empl_stat = ifelse(HDKT == 1, "E", ifelse(HDKT == 2, "U", "Inactive")),
    edu_cat = as.factor(ifelse(C17 %in% 1:9, C17, NA)),
    college = ifelse(C17 %in% 7:9, 1, 0),
    ISIC = ifelse(empl_stat == "U", "00", ifelse(nchar(C30C) == 4, as.character(C30C), paste0("0", C30C))),
    ISIC_unemployed = ifelse(C63 == 2, NA_character_, ifelse(nchar(C67C) == 4, as.character(C67C), paste0("0", C67C))),
    industry = ifelse(empl_stat == "U", "00", substr(ISIC, 1, 2)),
    creative = ifelse(empl_stat == "U", 0, ifelse(ISIC %in% creative_ISIC, 1, 0)),
    wage = C44,
    weekly_hours = C46,
    permanent_contract = ifelse(C36 == 1 & empl_stat == "E", 1, 0),
    social_insurance = ifelse(C39 == 1 & empl_stat == "E", 1, 0),
    type_of_empl = recode_type_of_empl(C35),
    second_job = ifelse(!is.na(C45B) | !is.na(C46B), 1, 0),
    weight = WEIGH_FI
  ) %>%
  filter(HDKT %in% c(1, 2) & !is.na(ISIC) & C5 %in% 15:65) %>%
  select(
    year, rural, female, edu_cat, college, empl_stat, ISIC, ISIC_unemployed,
    industry, creative, permanent_contract, social_insurance, type_of_empl,
    wage, weekly_hours, second_job, weight
  )

### Merge data
finaldf <- bind_rows(lfs_2016, lfs_2017, lfs_2018)

## 2. Summaries
sector_empl_summary <- finaldf %>%
  left_join(ISIC_2digit, by = c("industry" = "Code")) %>%
  filter(!is.na(Sector)) %>%
  group_by(year, Sector) %>%
  summarise(
    sector_empl = sum(weight, na.rm = TRUE),
    creative_empl = sum(weight[creative == 1], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year, Sector)

type_of_empl_summary <- finaldf %>%
  filter(!is.na(type_of_empl)) %>%
  group_by(year, type_of_empl) %>%
  summarise(
    non_creative_empl = sum(weight[creative == 0], na.rm = TRUE),
    creative_empl = sum(weight[creative == 1], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(year) %>%
  mutate(
    non_creative_share = (non_creative_empl / sum(non_creative_empl, na.rm = TRUE)) * 100,
    creative_share = (creative_empl / sum(creative_empl, na.rm = TRUE)) * 100
  ) %>%
  ungroup() %>%
  group_by(type_of_empl) %>%
  summarise(
    non_creative_share = mean(non_creative_share, na.rm = TRUE),
    creative_share = mean(creative_share, na.rm = TRUE),
    .groups = "drop"
  )

working_condition_summary <- finaldf %>%
  filter(!is.na(creative)) %>%
  mutate(group = ifelse(creative == 1, "Creative", "Non-Creative")) %>%
  group_by(group) %>%
  summarise(
    permanent_contract = weighted.mean(permanent_contract, w = weight, na.rm = TRUE) * 100,
    social_insurance   = weighted.mean(social_insurance, w = weight, na.rm = TRUE) * 100,
    female_share       = weighted.mean(female, w = weight, na.rm = TRUE) * 100,
    average_wage       = weighted.mean(wage, w = weight, na.rm = TRUE),
    weekly_hours       = weighted.mean(weekly_hours, w = weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -group,
    names_to = "metric",
    values_to = "raw_value"
  ) %>%
  group_by(metric) %>%
  mutate(
    indexed_value = (raw_value / raw_value[group == "Non-Creative"]) * 100,
    bar_label = case_when(
      metric %in% c("permanent_contract", "social_insurance", "female_share") ~ sprintf("%.1f%%", raw_value),
      metric == "average_wage" ~ scales::comma(round(raw_value, 0)),
      TRUE ~ sprintf("%.1f", raw_value)
    )
  ) %>%
  ungroup() %>%
  filter(is.finite(indexed_value))

## 3. Custom Theme
theme_dark_blue <- theme_minimal(base_family = "sans") +
  theme(
    plot.background = element_rect(fill = "#0A192F", color = NA),
    panel.background = element_rect(fill = "#0A192F", color = NA),
    text = element_text(color = "white"),
    plot.title = element_text(color = "white", face = "bold", size = 14),
    plot.caption = element_text(color = "#B3CDE3", size = 9.5, hjust = 0),
    axis.title = element_text(color = "white", face = "bold"),
    axis.text = element_text(color = "white"),
    axis.line = element_line(color = "white", linewidth = 1.2),
    axis.ticks = element_line(color = "white", linewidth = 1.2),
    panel.grid = element_blank(),
    legend.background = element_rect(fill = "#0A192F", color = NA),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white", face = "bold"),
    strip.background = element_rect(fill = "#112240", color = NA),
    strip.text = element_text(color = "white", face = "bold")
  )

## 4. Build All 4 Plots
### Plot 1: annual_share_plot
annual_share_df <- sector_empl_summary %>%
  group_by(year) %>%
  summarise(
    creative_share = (sum(creative_empl, na.rm = TRUE) / sum(sector_empl, na.rm = TRUE)) * 100,
    .groups = "drop"
  )

annual_share_plot <- ggplot(annual_share_df, aes(x = factor(year), y = creative_share)) +
  geom_col(fill = "#56B4E9", color = NA, width = 0.6) +
  geom_text(
    aes(label = sprintf("%.1f%%", creative_share)),
    vjust = -0.6,
    size = 4,
    fontface = "bold",
    color = "white"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Creative Industry Share of Total Annual Employment",
    x = "Year",
    y = "Creative Employment Share (%)"
  ) +
  theme_dark_blue

### Plot 2: creative_sector_share_plot
creative_sector_share_df <- sector_empl_summary %>%
  group_by(year) %>%
  mutate(
    creative_sector_share = (creative_empl / sum(creative_empl, na.rm = TRUE)) * 100
  ) %>%
  ungroup() %>%
  group_by(Sector) %>%
  filter(creative_sector_share > 0) %>%
  arrange(year) %>%
  ungroup()

creative_sector_smooth_df <- creative_sector_share_df %>%
  group_by(Sector) %>%
  reframe(
    as.data.frame(spline(year, creative_sector_share, n = 100, method = "natural"))
  ) %>%
  rename(year = x, creative_sector_share = y) %>%
  mutate(creative_sector_share = pmax(0, creative_sector_share))

creative_sector_share_plot <- ggplot() +
  geom_line(
    data = creative_sector_smooth_df,
    aes(x = year, y = creative_sector_share, color = Sector, group = Sector),
    linewidth = 1.5
  ) +
  geom_point(
    data = creative_sector_share_df,
    aes(x = year, y = creative_sector_share, color = Sector),
    size = 3
  ) +
  scale_x_continuous(breaks = unique(creative_sector_share_df$year)) +
  labs(
    title = "Sector Share of Creative Industry Employment Over Time",
    x = "Year",
    y = "Share of Creative Industry Employment (%)",
    color = "Sector"
  ) +
  theme_dark_blue

### Plot 3: type_empl_plot
pie_empl_df <- type_of_empl_summary %>%
  pivot_longer(
    cols = c(non_creative_share, creative_share),
    names_to = "category",
    values_to = "avg_share"
  ) %>%
  mutate(
    category = factor(
      category,
      levels = c("non_creative_share", "creative_share"),
      labels = c("Non-Creative Industry Employment", "Creative Industry Employment")
    )
  )

small_shares_df <- pie_empl_df %>%
  filter(avg_share < 0.5)

small_share_note <- if (nrow(small_shares_df) > 0) {
  paste0(
    "Note: Categories with < 0.5% share are omitted\n",
    paste(paste(small_shares_df$category, ":", sprintf("%.3f%%", small_shares_df$avg_share)), collapse = "\n"),
    "."
  )
} else {
  NULL
}

type_empl_plot <- ggplot(pie_empl_df, aes(x = 1, y = avg_share, fill = type_of_empl)) +
  geom_col(width = 1, color = NA) +
  coord_polar(theta = "y", start = 0) +
  geom_text(
    aes(
      x = 1.30,
      label = ifelse(avg_share >= 0.5, sprintf("%.1f%%", avg_share), "")
    ),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 3.2
  ) +
  facet_wrap(~ category, ncol = 2) +
  labs(
    title = "Average Employment Composition (2016-2018): Non-Creative vs. Creative Industries",
    caption = small_share_note,
    x = NULL,
    y = NULL,
    fill = "Type of Employment"
  ) +
  theme_dark_blue +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  )

### Plot 4: working_condition_plot (WebR-safe: group aesthetic + no \n in x labels + precomputed bar_label)
working_condition_plot <- ggplot(
  working_condition_summary,
  aes(x = metric, y = indexed_value, fill = group, group = group)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(label = bar_label),
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    color = "white",
    fontface = "bold",
    size = 3.5
  ) +
  scale_x_discrete(
    labels = c(
      "average_wage"       = "Average Wage \n(thousands VND)",
      "female_share"       = "Female Share",
      "permanent_contract" = "Permanent \nContract",
      "social_insurance"   = "Social \nInsurance",
      "weekly_hours"       = "Weekly Hours"
    )
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  scale_fill_manual(
    values = c(
      "Creative" = "#56B4E9",
      "Non-Creative" = "#B3CDE3"
    )
  ) +
  labs(
    title = "Creative vs. Non-Creative Industry Employment: Working Conditions",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_dark_blue +
  theme(
    legend.position = "top",
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )

## 5. Save all 4 plots to data/app_plots.rds
plots_list <- list(
  annual_share_plot          = annual_share_plot,
  creative_sector_share_plot = creative_sector_share_plot,
  type_empl_plot             = type_empl_plot,
  working_condition_plot     = working_condition_plot
)

saveRDS(plots_list, file = "data/app_plots.rds")