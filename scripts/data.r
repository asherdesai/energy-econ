# Data

library(tidyverse)
library(here)

get_data <- function() {
  d <- read_csv("../data_clean/data_clean.csv")
  return (d)
}

get_population <- function() {
  d <- read_csv(here("data_clean", "population_clean.csv"))
  return (d)
}

get_emissions <- function() {
  d <- read_csv(here("data_clean", "emissions.csv"))
  return(d)
}