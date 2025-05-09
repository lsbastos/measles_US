library(tidyverse)
library(nowcaster) # https://covid19br.github.io/nowcaster/


# Getting file names 
files <- list.files("data", pattern="*.csv", full.names=TRUE)

measles <- lapply( files,
                   vroom::vroom) |> 
  bind_rows(.id = "a") |>
  group_by(a) |> 
  mutate(
    dt_week_release = max(week_start)
  ) |> ungroup() |> select(-a)

# Maximum delay size (Dmax) 
# Dmax <- diff(range(measles$dt_week_release)) |> as.numeric() / 7
Dmax <- 5


# Minimum date to be considered. Information before is useless for calculate the delay
DT_trunc <- min(measles$dt_week_release) - Dmax * 7
# Start of the week of the last dataset
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


time.tbl <- tibble(  dt_time = seq(DT_trunc, max(measles$week_start), 7)) |> 
  rowid_to_column(var = "time")

# Time of the first dataset
b = time.tbl$time [ time.tbl$dt_time == min(measles$dt_week_release) ] 

# Time of the last dataset
B = max(time.tbl$time) 

measles.aux <- measles |> 
  filter(week_start >= DT_trunc) |>
  mutate(
    delay = as.numeric(dt_week_release - week_start) / 7
  ) |> left_join(time.tbl, by = c("week_start" = "dt_time")) |> 
  mutate(TD = time + delay) |> 
  select(week_start, time, delay, Nt_td = cases, TD)

measles.aux2 <- measles |> 
  filter(week_start >= DT_trunc) |>
  mutate(
    delay = as.numeric(dt_week_release - week_start) / 7
  ) |> left_join(time.tbl, by = c("week_start" = "dt_time")) |>
  # To make the dataset match TD = time + delay + 1 and TD.prev = time + delay (TD = TD.prev+1) 
  mutate(TD = time + delay + 1)  |> 
  select(week_start, Nt_tdm1 = cases, TD)


measles.delay <- measles.aux |> left_join(measles.aux2) |> 
  mutate(Y = Nt_td - Nt_tdm1) |> 
  select(week_start, time, delay, Y) |> 
  filter(delay != 0) |> 
  bind_rows(
    measles |> 
      mutate(
        delay = as.numeric(dt_week_release - week_start) / 7
      ) |> left_join(time.tbl, by = c("week_start" = "dt_time")) |> 
      filter(delay==0) |> 
      select(week_start, time, delay, Y=cases)
  ) |> arrange(time, delay) |> 
  filter(time + delay > b, delay <= Dmax) 

# Filling NAs for prediction
measles.NA <- tibble(
  time = rep((b+1):B,Dmax), 
  delay = rep(1:Dmax,each=length((b+1):B)), 
  Y = NA) |> 
  filter(time+delay > B) |> 
  left_join(time.tbl, by = "time") |> 
  rename( "week_start" = "dt_time") 


# measles.delay |> group_by(week_start,time) |> summarise(Y = sum(Y))

out <- nowcasting_no_age(dataset = measles.delay |> 
                           mutate(Y = ifelse(Y<0,0,Y)) |> 
                           bind_rows(measles.NA) |> 
                           rename("Time"="time", "dt_event"="week_start"))


measles.now <- nowcasting.summary(out, age = F)

  

# measles |> 
#   filter(dt_week_release == DT_last_B, year(week_start) == 2025) |> 
measles.out <- measles |> 
  filter(dt_week_release == DT_last_B, year(week_start) == 2025) |> 
  left_join(measles.now$total, by = c("week_start"="dt_event"))

## Saving
# measles.out |> write_csv(file = paste0("output/week",B+1,".csv"))

g <- measles.out |> 
  ggplot(aes(x = week_start, y = cases, )) + 
  geom_ribbon(#data = measles.now$total, 
              mapping = aes(x = week_start, y = Median, 
                            ymin = LI, ymax = LS,
                            fill = "95% CI"), alpha = 0.25) + 
  geom_ribbon(#data = measles.now$total, 
              mapping = aes(x = week_start, y = Median, 
                            ymin = LIb, ymax = LSb,
                            fill = "50% CI"), alpha = 0.5) + 
  geom_line(aes(colour = "Reported")) +
  geom_line(#data = measles.now$total, 
            mapping = aes(x = week_start, y = Median, colour = "Estimated (Nowcasting)")) +
  scale_x_date(date_breaks = "week", date_labels = "%U") +
  scale_color_manual(values = c( "red", "black")) +
  scale_fill_manual(values = c("red", "red")) + 
  labs(
    title = "Measles cases in the US, 2025",
    x = "Week",
    y = "Cases",
    caption = "Source: https://www.cdc.gov/measles/data-research/", 
    fill = "",
    colour = "Measles cases"
  ) + 
  theme_bw( base_size = 16)  +
  theme(legend.position = "inside", legend.position.inside = c(0.2, 0.70))

g

# ggsave(plot = g, device = "png", filename = paste0("output/nowcast",B+1,".png"))

measles |> 
  filter(dt_week_release == DT_last_B) |> 
  ggplot(aes(x = week_start, y = cases, )) + 
  geom_ribbon(data = measles.now$total, 
              mapping = aes(x = dt_event, y = Median, 
                            ymin = LI, ymax = LS,
                            fill = "95% CI"), alpha = 0.25) + 
  geom_ribbon(data = measles.now$total, 
              mapping = aes(x = dt_event, y = Median, 
                            ymin = LIb, ymax = LSb,
                            fill = "50% CI"), alpha = 0.5) + 
  geom_line(aes(colour = "Reported")) +
  geom_line(data = measles.now$total, mapping = aes(x = dt_event, y = Median, colour = "Estimated (Nowcasting)")) +
  # scale_x_date(date_breaks = "week", date_labels = "%U") +
  scale_x_date(date_breaks = "10 weeks", date_labels = "%U/%y") +
  scale_color_manual(values = c( "red", "black")) +
  scale_fill_manual(values = c("red", "red")) + 
  labs(
    title = "Measles cases in the US",
    # subtitle = "@lsbastos",
    x = "Week/year",
    y = "Cases",
    caption = "Source: https://www.cdc.gov/measles/data-research/", 
    fill = "",
    colour = "Measles cases"
  ) + 
  theme_bw( base_size = 16)  +
  theme(legend.position = "inside", legend.position.inside = c(0.2, 0.70))

# Total number of cases

# View(out)

total.observed <- measles |> 
  filter(dt_week_release == DT_last_B, 
         epiyear(week_start) == 2025) |> 
  summarise(
    cases = sum(cases))

total.runoff <- measles |> 
  filter(dt_week_release == DT_last_B, 
         epiyear(week_start) == 2025,
         week_start < min(out$dt_event)) |> 
  summarise(
    cases = sum(cases))


out |> group_by(sample) |> summarise(total.runoff = sum(Y) + total.runoff$cases[1]) |> 
  summarise(
    Mean = mean(total.runoff),
    Q.50 = quantile(total.runoff,prob=0.5),
    LI.50 = quantile(total.runoff,prob=0.25),
    LS.50 = quantile(total.runoff,prob=0.75),
    LI.95 = quantile(total.runoff,prob=0.025),
    LS.95 = quantile(total.runoff,prob=0.975)
  )


out |> 
  group_by(sample) |> 
  summarise(cases = sum(Y) + total.runoff$cases[1]) |> 
  ggplot() + 
  geom_density(mapping = aes(x = cases)) +
  geom_vline(xintercept = total.observed$cases[1], linetype = "dashed" ) + 
  theme_bw()
