# Environmental Econ Aside

library(jsonlite)
library(tidyverse)
library(here)
library(rio)

clean_energy_data <- function() {
  # Open text of all json data
  seds_url <- "https://api.eia.gov/bulk/SEDS.zip"
  options(timeout = 1000)
  download.file(seds_url, here("data", "seds.zip"))
  unzip(zipfile = here("data"), exdir = here("data"))
  file.remove(here("data", "seds.zip"))
  
  print("Finished downloading and unzipping raw data")
  
  con <- file(here("data", "SEDS.txt"))
  all_json <- readLines(con)
  close(con)
  
  # Removes categories
  all_json <- all_json[grepl("series_id", all_json)]
  all_json <- all_json[grepl("iso3166", all_json)]
  
  # Creates a data frame out of each json line
  data_list <- vector(mode = "list", length = length(all_json))
  for (i in seq_along(all_json)) {
    series_json <- all_json[i]
    series_list <- fromJSON(series_json)
    
    series_data <- as_tibble(series_list[["data"]])
    
    series_data <- series_data %>% 
      mutate(series_id = str_sub(series_list[["series_id"]], 6, 10),
             location = series_list[["iso3166"]]) %>% 
      rename(year = V1, value = V2) %>% 
      select(year, value, location, series_id)
    
    data_list[[i]] <- series_data
  }
  
  d <- do.call(rbind, data_list)
  if (!dir.exists(here("data_clean"))) dir.create(here("data_clean"))
  export(d, here("data_clean", "data_clean.csv"))
  return(d)
  
}

clean_population_data <- function() {
  d <- import(here("data", "population.csv"), header = TRUE)
  
  d <- d %>%
    pivot_longer(!state, names_to = "year", values_to = "population") %>% 
    mutate(year = as.numeric(year),
           population = gsub(",", "", population),
           population = as.numeric(population)) %>% 
    group_by(state) %>% 
    mutate(diff = (lead(population) - population) / 10)
  
  d_new <- tibble(
    decade = rep(c(10*rep(0:5, each = 10)+1960, 2020), 51),
    year = rep(1960:2020, 51),
    state = rep(unique(d$state), each = length(1960:2020))
  ) %>% 
    mutate(year_diff = year - decade)
  
  dd <- left_join(d_new, d, by = c("year", "state")) %>% 
    group_by(state, decade) %>% 
    mutate(max_diff = max(diff, na.rm=TRUE)) %>% 
    mutate(max_pop = max(population, na.rm=TRUE)) %>% 
    mutate(pop2 = max_pop + (year_diff * max_diff)) %>% 
    ungroup() %>% 
    mutate(population = ifelse(is.na(population), pop2, population)) %>% 
    select(state, year, population)
  
  if (!dir.exists(here("data_clean"))) dir.create(here("data_clean"))
  export(dd, here("data_clean", "population_clean.csv"))
}

clean_co2_data <- function() {
  url <- "https://www.eia.gov/environment/emissions/state/excel/table2.xlsx"
  download.file(url, here("data", "table2.xlsx"))
  d <- import(here("data", "table2.xlsx"), range = "A5:T56")
  i <- import(here("data", "iso.csv"))
  
  d <- left_join(d, i, by = c("State" = "state_name"))
  d <- d %>% 
    select(-State) %>% 
    pivot_longer(!state, names_to = "year", values_to = "emissions")
  
  export(d, here("data_clean", "emissions.csv"))
}