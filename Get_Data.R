# Get the data from the FredMD macroeconomic database

# The following packages need to be implemented
# install.packages("stats")
# install.packages("readr")
# install.packages("pracma")

# install.packages("devtools", type = "win.binary")
# library(devtools)
# install_github("cykbennie/fbi")
library(fbi)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(TTR)

# Load the data from the FREDMD database
start_date = "1960-01-01"
data = fredmd("https://files.stlouisfed.org/files/htdocs/fred-md/monthly/current.csv")
data_raw = fredmd("https://files.stlouisfed.org/files/htdocs/fred-md/monthly/current.csv",
                  transform = FALSE)
# Description of all transformations
varlist = fredmd_description
vars = intersect(colnames(data),varlist$fred)

data = data %>% as.tibble()%>%
  select(all_of(c("date",vars)))
varlist = varlist%>%filter(fred%in%vars)
prices_varlist = varlist%>%filter(group=="Prices",tcode==6)

data = data%>% as_tibble()%>%
  select( -all_of(prices_varlist$fred) )

prices = data_raw %>% as_tibble() %>%
  select(all_of(prices_varlist$fred))%>%
  mutate_all(.funs = function(x)100*c(NA,x%>%log()%>%diff()))

data = cbind(data%>%as.data.frame(),prices%>%as.data.frame())

data = data %>%
  filter(date>=start_date)%>%
  select_if(~ !any(is.na(.)))

# CPI : first difference in logarithm. Inflation at time t
CPI = data$CPIAUCSL
date = as.Date(data$date)

# Create new dataframe for dates and (transformed) CPI combined
data_CPI = data.frame(
  day = date,
  value = CPI
)

data_CPI_raw = data.frame(
  day = as.Date(data_raw$date),
  value = data_raw$CPIAUCSL
)

# Create time series plot for CPI 
# Time plot for the first differences logarithm: inflation in %
p = ggplot(data_CPI, aes(x=day, y=value)) +
  geom_line() + 
  xlab("") + ylab("Inflation %")

p + scale_x_date(date_breaks = "5 years", date_labels = "%Y")

# Time series plot of CPI before any transformations
# Accumulated inflation therefore an upward trend 
q = ggplot(data_CPI_raw, aes(x=day, y=value)) +
  geom_line() + 
  xlab("") + ylab("Accumulated inflation")

q + scale_x_date(date_breaks = "5 years", date_labels = "%Y")
