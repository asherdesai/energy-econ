# Environmental Econ Aside

library(jsonlite)
library(tidyverse)

clean_energy_data <- function() {
  # Open text of all json data
  con <- file("../data/SEDS.txt")
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
  file.remove("../data_clean/data_clean.csv")
  write_csv(d, "../data_clean/data_clean.csv")
  
}

clean_population_data <- function() {
  d <- read_csv("../data/population.csv")
  d <- d %>%
    pivot_longer(!state, names_to = "year", values_to = "population") %>% 
    group_by(state) %>% 
    mutate(diff = (lead(population) - population) / 10) %>% 
    mutate(year = as.numeric(year))
  
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
  
  write_csv(dd, "../data_clean/population_clean.csv")
  return (dd)
}
