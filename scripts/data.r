# Data

library(tidyverse)

get_data <- function() {
  d <- read_csv("../data_clean/data_clean.csv")
  return (d)
}

get_population <- function() {
  d <- read_csv("../data_clean/population_clean.csv")
  return (d)
}