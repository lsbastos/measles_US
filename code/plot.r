library(tidyverse)


files <- list.files("data/", pattern="*.csv", full.names=TRUE)

measles <- lapply( files, 
               vroom::vroom) |> bind_rows(.id = "a") |>
  group_by(a) |> 
  mutate(
    dt_week_release = max(week_start)
  ) |> ungroup() |> select(-a)



# Maximum delay size (Dmax) 
Dmax <- diff(range(measles$dt_week_release)) |> as.numeric() / 7

# Minimum date to be considered information before is useless for calculate the delay
DT_trunc <- min(measles$dt_week_release) - Dmax * 7
DT_last_B <- max(measles$dt_week_release) 




measles |> 
  filter(week_start >= DT_trunc) |> 
  ggplot(aes(x = week_start, y = cases, group = dt_week_release, color = as.character(dt_week_release))) + 
  geom_line() + 
  scale_x_date(date_breaks = "week", date_labels = "%U") +
  # scale_color_manual(values = 1:length(files), labels = paste("Week",7 + 1:length(files))) + 
  labs(
    title = "Measles cases in the US",
    x = "Week",
    y = "Cases",
    color = "Data release date",
    caption = "Source: https://www.cdc.gov/measles/data-research/", 
  ) + 
  theme_bw( base_size = 16) + 
  theme(legend.position = "inside", legend.position.inside = c(0.2, 0.75))


measles |> 
  filter(dt_week_release == DT_last_B) |> 
  ggplot(aes(x = week_start, y = cases, )) + 
  geom_line() + 
  scale_x_date(date_breaks = "10 weeks", date_labels = "%U/%y") +
  # scale_color_manual(values = 1:length(files), labels = paste("Week",7 + 1:length(files))) + 
  labs(
    title = "Measles cases in the US",
    x = "Week/Year",
    y = "Cases",
    caption = "Source: https://www.cdc.gov/measles/data-research/", 
  ) + 
  theme_bw( base_size = 16)  
# theme(legend.position = "inside", legend.position.inside = c(0.2, 0.75))



