# Script misc

library(tidyverse)

if (!exists("d")) {
  d <- get_data()
}

if (!exists("p")) {
  p <- get_population()
  p <- filter(p, year != 2020)
}

df <- d %>% 
  filter(
    series_id == "NGEID" |
    series_id == "ESTCP" |
    series_id == "CLEIP" |
    series_id == "NGEIP" |
    series_id == "PAEIP"
  ) %>% 
  select(year, value, location, series_id) %>% 
  pivot_wider(names_from = series_id, values_from = value) %>% 
  mutate(total_co2 = (430 * PAEIP) + (1810 * CLEIP) + (54.8 * NGEIP))

dd <- full_join(p, df, by = c("state" = "location", "year"))

data <- dd %>% 
  filter(year %in% c(2005, 2019)) %>% 
  group_by(state) %>% 
  summarize(price_delta = (NGEID-lag(NGEID))/lag(NGEID),
            total_co2 = total_co2 / population) %>% 
  drop_na()

# g <- ggplot(data = dd) +
#   geom_point(mapping = aes(x = ))