library(tidyverse)

week12 <- read_csv("output/week12.csv")
week13 <- read_csv("output/week13.csv")
week14 <- read_csv("output/week14.csv")
week15 <- read_csv("output/week15.csv")
week16 <- read_csv("output/week16.csv")
week17 <- read_csv("output/week17.csv")
week18 <- read_csv("output/week18.csv")

week <- list(week12,week13, week14, week15, week16, week17, week18)
K = length(week)
gg <- list()

g0 <- week[[K]] |> 
  ggplot(aes(x = week_start, y = cases))


for(k in 1:K){
  gg[[k]] <- g0 +  
    geom_ribbon(data = week[[k]], 
                mapping = aes(x = week_start, y = Median, 
                              ymin = LI, ymax = LS,
                              fill = "95% CI"), alpha = 0.25) + 
    geom_ribbon(data = week[[k]], 
                mapping = aes(x = week_start, y = Median, 
                              ymin = LIb, ymax = LSb,
                              fill = "50% CI"), alpha = 0.5) + 
    geom_line(aes(colour = "Reported (up-to-date)")) +
    geom_line(data = week[[k]], aes(colour = "Reported (at the time)"), linetype = "dashed") +
    geom_line(data = week[[k]], mapping = aes(x = week_start, y = Median, colour = "Estimated (Nowcasting)")) + 
    scale_x_date(date_breaks = "week", date_labels = "%U") +
    scale_color_manual(values = c( "red", "black", "black")) +
    scale_fill_manual(values = c("red", "red")) + 
    labs(
      title = "Measles cases in the US, 2025",
      x = "Week",
      y = "Cases",
      caption = "Source: https://www.cdc.gov/measles/data-research/", 
      fill = "",
      colour = "Measles cases"
    ) + 
    ylim(0,352) +
    theme_bw( base_size = 16)  +
    theme(legend.position = "inside", legend.position.inside = c(0.2, 0.70))
  
}

gg


# Gerando um gif no braco
pdf(file = "output/test.pdf", width=9.9, height=5.9)
gg
dev.off()


# convert -verbose -delay 50 -loop 0 -density 300 test.pdf file.gif
