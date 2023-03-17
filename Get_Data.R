# Get the data from the FredMD macroeconomic database 

# The following packages need to be implemented
# install.packages("stats")
# install.packages("readr")
# install.packages("pracma")

# install.packages("devtools", type = "win.binary")
# library(devtools)
# install_github("cykbennie/fbi")
#install.packages("stargazer")
#library(stargazer)
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

# CPI : first difference in logarithm. Inflation at month t
CPI = data$CPIAUCSL
date = as.Date(data$date)

# Make the yield data
slope_yield = data_raw$GS10 - data_raw$TB3MS
slope_yield_st = diff(slope_yield)
slope_yield_st = slope_yield_st[12:768]

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

p + scale_x_date(date_breaks = "3 years", date_labels = "%Y")

# Time series plot of CPI before any transformations
# Accumulated inflation therefore an upward trend 
q = ggplot(data_CPI_raw, aes(x=day, y=value)) +
  geom_line() + 
  xlab("") + ylab("Accumulated inflation")

q + scale_x_date(date_breaks = "3 years", date_labels = "%Y")

# Stable and unstable split
# standard deviation out of sample period
sd(CPI[481:757])
# standard deviation for first split
sd(CPI[481:612])
# standard deviation for second split
sd(CPI[613:720])
# standard deviation for third split
sd(CPI[721:757])

save(data,file="data.Rda")

dataset = data.frame(date, data$CPIAUCSL, data$RPI, data$INDPRO, data$UNRATE, data$HOUST,
                data$DPCERA3M086SBEA, data$M2SL, data$TB3MS, data$`S&P 500`, slope_yield_st)
save(dataset, file="dataset.Rda")
#stargazer(dataset[721:757,], type = "latex", title="Descriptive statistics", digits=1, out="table1.txt",  flip=TRUE)

plot(tail(data[,"CPIAUCSL"],180),type = "l")

# Data from https://fredhelp.stlouisfed.org/fred/data/understanding-the-data/recession-bars/
recessions.df = read.table(textConnection(
  "Peak, Trough
1857-06-01, 1858-12-01
1860-10-01, 1861-06-01
1865-04-01, 1867-12-01
1869-06-01, 1870-12-01
1873-10-01, 1879-03-01
1882-03-01, 1885-05-01
1887-03-01, 1888-04-01
1890-07-01, 1891-05-01
1893-01-01, 1894-06-01
1895-12-01, 1897-06-01
1899-06-01, 1900-12-01
1902-09-01, 1904-08-01
1907-05-01, 1908-06-01
1910-01-01, 1912-01-01
1913-01-01, 1914-12-01
1918-08-01, 1919-03-01
1920-01-01, 1921-07-01
1923-05-01, 1924-07-01
1926-10-01, 1927-11-01
1929-08-01, 1933-03-01
1937-05-01, 1938-06-01
1945-02-01, 1945-10-01
1948-11-01, 1949-10-01
1953-07-01, 1954-05-01
1957-08-01, 1958-04-01
1960-04-01, 1961-02-01
1969-12-01, 1970-11-01
1973-11-01, 1975-03-01
1980-01-01, 1980-07-01
1981-07-01, 1982-11-01
1990-07-01, 1991-03-01
2001-03-01, 2001-11-01
2007-12-01, 2009-06-01
2020-02-01, 2020-04-01"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)

recessions.trim = subset(recessions.df, Peak >= min(data_CPI$day) )

g = ggplot(data_CPI) + geom_line(aes(x=day, y=value))+ scale_x_date(limits = as.Date(c("1960-01-01","2023-01-01")), date_breaks = "6 years", date_labels = "%Y") + theme_bw()
g = g + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='blue', alpha=0.2)


# Out-of-sample start and split in the out-of-sample
h = g + geom_vline(xintercept = as.Date("1999-12-01"), color = "red")+ geom_vline(xintercept = as.Date("2010-12-01"), color = "orange") + geom_vline(xintercept = as.Date("2019-12-01"), color = "orange")+ labs(x = "Date", 
         y = "Inflation %")
ggsave("inflationplot.pdf", plot = h)
