# Script misc

library(tidyverse)

d <- get_data()
p <- get_population()
p <- filter(p, year != 2020)
e <- get_emissions()

dd <- d %>% 
  filter(
    series_id == "NGEID" |
    series_id == "ESTCP" |
    series_id == "CLEIP" |
    series_id == "NGEIP" |
    series_id == "PAEIP"
  ) %>% 
  select(year, value, location, series_id) %>% 
  pivot_wider(names_from = series_id, values_from = value) #%>% 
  # mutate(total_co2 = (430 * PAEIP) + (1810 * CLEIP) + (54.8 * NGEIP))

dd <- full_join(p, df, by = c("state" = "location", "year"))

dd <- left_join(e, dd, by = c("state", "year"))

data <- filter(dd, year == 2018)

g <- ggplot(data) +
  geom_point(aes(x = NGEID, y = emissions))