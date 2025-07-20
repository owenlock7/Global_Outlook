### Load relevant packages
library(tidyverse)
library(magrittr)
library(ggthemes)

### Load Data
FM <- read.csv("IMF_Fiscal_Monitor.csv")

### Transform data to be just the indicators and the time series
FM %<>% 
  dplyr:: select("COUNTRY", "GFS_STO", "TRANSFORMATION", contains("X"))

### Transform the data into longer format
FM %<>%
  tidyr:: pivot_longer(cols = -c(COUNTRY, GFS_STO, TRANSFORMATION), 
                       names_to = "Date", 
                       values_to = "Indicator_Value")

### Format the data to remove the 'X' which was a legacy of the dates as column names formatting
FM %<>% 
  dplyr:: mutate(Date = gsub("X", "", Date)) %>%
  dplyr:: mutate(Indicator_Value = as.numeric(Indicator_Value),
                 Date = as.numeric(Date))

### Plot historical net lending and borrowing for the UK - against some peers

# unique(FM$COUNTRY)
# unique(FM$GFS_STO)

Chart1 <- FM %>%
  dplyr:: filter(COUNTRY %in% c("United Kingdom",
                                "United States",
                                "Germany")) %>%
  dplyr:: filter(GFS_STO == "Net lending (+) / net borrowing (-)") %>%
  dplyr:: filter(Date <= 2025) %>%
  ggplot2:: ggplot(aes(x = Date, y = Indicator_Value)) +
  ggplot2:: geom_line(aes(colour = COUNTRY), linewidth = 1.05) +
  ggthemes::theme_economist() +
  ggplot2:: theme(plot.title = element_text(hjust = 0.5),
                  legend.position = "top",
                  legend.direction = "horizontal") +
  ggplot2:: labs(title = "Government Deficit") +
  ggplot2:: xlab("Year") +
  ggplot2:: ylab("Per Cent of GDP") 
  # ggplot2:: geom_vline(xintercept = 2025, linetype = "dashed", colour = "black")
Chart1

Chart.function <- function(Indicator, Title, y_axis = "Per Cent of GDP"){
  FM %>%
    dplyr:: filter(COUNTRY %in% c("United Kingdom",
                                  "United States",
                                  "Germany")) %>%
    dplyr:: filter(GFS_STO == Indicator) %>%
    dplyr:: filter(Date <= 2025) %>%
    ggplot2:: ggplot(aes(x = Date, y = Indicator_Value)) +
    ggplot2:: geom_line(aes(colour = COUNTRY), linewidth = 1.05) +
    ggthemes::theme_economist() +
    ggplot2:: theme(plot.title = element_text(hjust = 0.5),
                    legend.position = "top",
                    legend.direction = "horizontal") +
    ggplot2:: labs(title = Title) +
    ggplot2:: xlab("Year") +
    ggplot2:: ylab(y_axis) 
}

Chart.function(Indicator = "Cyclically adjusted primary balance",
               Title = "Cyclically Adjusted Primary Government Deficit")



unique(FM$COUNTRY)
unique(FM$GFS_STO)





