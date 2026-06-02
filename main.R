
##############################################
### PARSE PDFS AND PUT THEM INTO A COLUMN ####
##############################################

source("code/function_read_parse_clean_pdf_2.R")

library(purrr)
library(readr)

files <- list.files("data/", pattern = "\\.pdf$", full.names = TRUE)

results_df <- map_dfr(files, function(f) {
  df <- read_pdf(f)
  tibble(
    file = basename(f),
    text = paste(df$text, collapse = " ")
  )
})

##############################################
### EXTRACT CLIMATE RELATED CONTENT ##########
##############################################

source("code/function_extract_climate_content.R")

results_df <- results_df |>
  rowwise() |>
  mutate(result = list(extract_climate_context(text, source_name = file))) |>
  ungroup()

###############################################
### ANALYSIS OF OUTPUT ########################
###############################################

# Word count

results_df <- results_df |>
  mutate(word_count = str_count(text, "\\S+"))

word_count_files <- results_df |>
                        select(file, word_count) |>
                        arrange(desc(word_count))

# Various graphs

library(tidyverse)

# --- 1. TAG YEAR AND COMPANY FROM FILENAME ----------------------------------

results_flat <- results_df |>
  unnest(result)

results_tagged <- results_flat |>
  mutate(
    year    = case_when(
      str_detect(file, "2021") ~ "2021",
      str_detect(file, "2025") ~ "2025"
    ),
    company = file |>
      str_remove("_?(2021|2025).*$") |>
      str_remove("\\.pdf$") |>
      str_remove("^ID_?\\d+_?") |>
      str_to_title()
  ) |>
  filter(!is.na(year))

# --- 2. CATEGORY COUNTS BY YEAR AND COMPANY ---------------------------------

category_counts <- results_tagged |>
  count(company, year, category)

# Heatmap: category frequency by company and year
ggplot(category_counts, aes(x = year, y = category, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), size = 3) +
  facet_wrap(~company) +
  scale_fill_gradient(low = "#eaf4fb", high = "#1a6fa8") +
  labs(
    title = "Climate Category Frequency by Company and Year",
    x = "Year", y = "Category", fill = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- 3. YEAR-ON-YEAR CHANGE PER CATEGORY AND COMPANY -----------------------

category_diff <- category_counts |>
  pivot_wider(names_from = year, values_from = n, values_fill = 0) |>
  mutate(diff = `2025` - `2021`)

# Diverging bar chart: change from 2021 to 2025
ggplot(category_diff, aes(x = diff, y = category, fill = diff > 0)) +
  geom_col(show.legend = FALSE) +
  geom_vline(xintercept = 0, linewidth = 0.4) +
  facet_wrap(~company) +
  scale_fill_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c")) +
  labs(
    title    = "Change in Climate Category Mentions: 2021 → 2025",
    subtitle = "Green = increase, Red = decrease",
    x = "Change in count", y = "Category"
  ) +
  theme_minimal()

# --- 4. TOP KEYWORDS BY YEAR AND COMPANY ------------------------------------

keyword_counts <- results_tagged |>
  mutate(keyword_matched = str_to_lower(keyword_matched)) |>
  count(company, year, keyword_matched, sort = TRUE) |>
  group_by(company, year) |>
  slice_max(n, n = 10) |>
  ungroup()

# Faceted bar chart: top keywords per company, coloured by year
ggplot(keyword_counts,
       aes(x = n, y = reorder(keyword_matched, n), fill = year)) +
  geom_col(position = "dodge") +
  facet_wrap(~company, scales = "free_y") +
  scale_fill_manual(values = c("2021" = "#f39c12", "2025" = "#2980b9")) +
  labs(
    title = "Top 10 Climate Keywords by Company and Year",
    x = "Count", y = "Keyword", fill = "Year"
  ) +
  theme_minimal()

# --- 5. SHARE OF EACH CATEGORY BY COMPANY (STACKED BAR) --------------------

category_share <- results_tagged |>
  count(company, year, category) |>
  group_by(company, year) |>
  mutate(share = n / sum(n))

ggplot(category_share, aes(x = year, y = share, fill = category)) +
  geom_col() +
  facet_wrap(~company) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Year", y = "Share", fill = "Category"
  ) +
  theme_minimal()

# --- 6. CROSS-COMPANY COMPARISON FOR ONE CATEGORY --------------------------
# Edit `focus_category` to whichever category you want to zoom into

focus_category <- "net_zero"

results_tagged |>
  filter(category == focus_category) |>
  count(company, year) |>
  ggplot(aes(x = company, y = n, fill = year)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("2021" = "#f39c12", "2025" = "#2980b9")) +
  labs(
    x = "Company", y = "Count", fill = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


