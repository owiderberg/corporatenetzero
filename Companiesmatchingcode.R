
#######################################################run from beginning
library(dplyr)
library(stringr)
library(readr)
library(readxl)

marketcap <- read_csv("Datacompanies/companiesmarketcap.com - Companies ranked.csv")
snapshot <- read_excel("Datacompanies/current_snapshot_2026-02-23_06-38-18.xlsx")


marketcap <- marketcap |>
  mutate(
    Name_clean = Name |>
      str_to_lower() |>
      str_remove_all("\\s*\\(.*?\\)") |>
      str_trim())

marketcap <- marketcap |>
  mutate(Rank = as.numeric(Rank)) |>
  filter(Rank <= 500)

snapshot <- snapshot |>
  mutate(
    entity_clean = `Entity type and location` |>
      str_to_lower() |>
      str_remove_all("\\s*\\(.*?\\)") |>
      str_trim())

snapshot_companies <- snapshot |>
  filter(grepl("^COM", `...2`))
write.csv(snapshot_companies, "snapshot_companies.csv", row.names = FALSE)

snapshotcom <- snapshot_companies |>
  mutate(
    entity_clean = `Entity type and location` |>
      str_to_lower() |>
      str_remove_all("\\s*\\(.*?\\)") |>
      str_trim())

snapshotcom <- snapshot_companies |>
  mutate(
    entity_clean = `Entity type and location` |>
      str_to_lower() |>
      str_remove_all("\\s*\\(.*?\\)") |>
      str_trim()
  )

match_results <- marketcap |>
  rowwise() |>
  mutate(
    snapshot_match = first(snapshotcom$`Entity type and location`[
      snapshotcom$entity_clean == Name_clean |
        str_detect(snapshotcom$entity_clean, paste0("\\b", Name_clean, "\\b")) |
        str_detect(Name_clean, paste0("\\b", snapshotcom$entity_clean, "\\b"))
    ])) |>
  ungroup()

match_results |>
  summarise(
    matched = sum(!is.na(snapshot_match)),
    unmatched = sum(is.na(snapshot_match))
  )
match_results |>
  filter(is.na(snapshot_match)) |>
  arrange(Rank) |>
  select(Rank, Name)|> 
  print(n = 40)

match_results |>
  filter(!is.na(snapshot_match)) |>
  arrange(Rank) |>
  select(Rank, Name, snapshot_match) |>
  print(n = 40)
######################
closest_matchescom <- match_results |>
  filter(is.na(snapshot_match)) |>
  rowwise() |>
  mutate(
    min_distance = min(adist(Name_clean, snapshotcom$entity_clean)),
    closest_snapshot = snapshotcom$`Entity type and location`[
      which.min(adist(Name_clean, snapshotcom$entity_clean))
    ]
  ) |>
  ungroup()
closest_matchescom |>
  arrange(Rank) |>
  select(Rank, Name, closest_snapshot) |>
  print(n = 90)

############end


###############Check if specific company (in this case heineken) is matched
match_results |>
  filter(str_detect(Name, "Heineken")) |>
  select(Rank, Name, snapshot_match)

marketcap |>
  filter(str_detect(str_to_lower(Name), "heineken")) |>
  select(Name)
