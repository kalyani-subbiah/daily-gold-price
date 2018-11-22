
# Libraries
library(lubridate)
library(jsonlite)
library(plyr)
library(RJSONIO)
library(ggplot2)
library(plotly)
library(gridExtra)
options(stringsAsFactors = FALSE)

## Gold Prices Data - load as json and convert to dataframe

#Get JSON from API

raw_gold <- fromJSON("https://www.quandl.com/api/v3/datasets/WGC/GOLD_DAILY_USD.json?api_key=8rzMrxYvNJCxwA2cX2UU")
data <-  raw_gold[['dataset']]
data <-  data[['data']]
Value <- sapply(data, function(x) x[[2]])
Date <- sapply(data, function(x) x[[1]])
gold <-  data.frame(Date, Value)

## Yield Curve Data
raw_yield <- fromJSON("https://www.quandl.com/api/v3/datasets/USTREASURY/YIELD.json?api_key=8rzMrxYvNJCxwA2cX2UU")
data <-  raw_yield[['dataset']]
data <-  data[['data']]

Date <- sapply(data, function(x) x[[1]][[1]])
Date[sapply(Date, is.null)] <- NA
Date = unlist(Date)

onemo <- sapply(data, function(x) x[[2]])
onemo[sapply(onemo, is.null)] <- NA
onemo = unlist(onemo)

threemo <- sapply(data, function(x) x[[4]])
threemo[sapply(threemo, is.null)] <- NA
threemo = unlist(threemo)

sixmo <- sapply(data, function(x) x[[5]])
sixmo[sapply(sixmo, is.null)] <- NA
sixmo = unlist(sixmo)

oneyr <- sapply(data, function(x) x[[6]])
oneyr[sapply(oneyr, is.null)] <- NA
oneyr = unlist(oneyr)

twoyr <- sapply(data, function(x) x[[7]])
twoyr[sapply(twoyr, is.null)] <- NA
twoyr = unlist(twoyr)

threeyr <- sapply(data, function(x) x[[8]])
threeyr[sapply(threeyr, is.null)] <- NA
threeyr = unlist(threeyr)

fiveyr <- sapply(data, function(x) x[[9]])
fiveyr[sapply(fiveyr, is.null)] <- NA
fiveyr = unlist(fiveyr)

sevenyr <- sapply(data, function(x) x[[10]])
sevenyr[sapply(sevenyr, is.null)] <- NA
sevenyr = unlist(sevenyr)

tenyr <- sapply(data, function(x) x[[11]])
tenyr[sapply(tenyr, is.null)] <- NA
tenyr = unlist(tenyr)

twentyyr <- sapply(data, function(x) x[[12]])
twentyyr[sapply(twentyyr, is.null)] <- NA
twentyyr = unlist(twentyyr)

thirtyyr <- sapply(data, function(x) x[[13]])
thirtyyr[sapply(thirtyyr, is.null)] <- NA
thirtyyr = unlist(thirtyyr)

yield = data.frame(Date,onemo,threemo,sixmo,oneyr,twoyr,threeyr,fiveyr,sevenyr,
                   tenyr,twentyyr,thirtyyr)


# Gold date processing
gold$date <- as.Date(gold$Date, "%Y-%m-%d") 
gold = subset(gold, date >= "1979-01-01")
p <-  plot_ly(gold, x = ~date, y = ~Value, type = 'scatter', mode = 'lines') %>% 
  layout( xaxis = list(range = c('2013-06-04', '2013-12-04')))
p

# Yield Date processing
yield$date <- as.Date(yield$Date, "%Y-%m-%d") 
p <- ggplot(yield, aes(date, threemo)) +geom_line()
p <- plot_ly(yield, x = ~date, y = ~threemo, type = 'scatter', mode = 'lines') %>%
  layout( xaxis = list(range = c('2013-06-04', '2013-12-04')))
p

# Daily change in price
value = gold$Value

one_diff <- function(values){
  x <- c(1,2)
  for (i in 1:length(values)) {
    v = value[i] - value[i-1]
    x <- append(x,v)
    i = i+1
  }
  return(x)
}
value_lag1 = one_diff(value)
value_lag1 = value_lag1[-(0:2)]
gold_fd = gold[-c(0:1), ]
gold_fd['value_lag1'] = value_lag1


### Weekly differences

value = gold$Value

week_diff <- function(values){
  x <- c(1,2)
  for (i in 7:length(values)) {
    v = value[i] - value[i-7]
    x <- append(x,v)
    i = i+1
  }
  return(x)
}
value_lag7 = week_diff(value)
head(value_lag7)
value_lag7 = value_lag7[-(0:2)]
length(value_lag7)
gold_wd = gold[-c(0:7), ]
gold_wd['value_lag7'] = value_lag7

### Monthly Differences

value = gold$Value

month_diff <- function(values){
  x <- c(1,2)
  for (i in 30:length(values)) {
    v = value[i] - value[i-30]
    x <- append(x,v)
    i = i+1
  }
  return(x)
}
value_lagm = month_diff(value)
value_lagm = value_lagm[-(0:2)]
gold_md = gold[-c(0:30), ]
gold_md['value_lagm'] = value_lagm


##Gold_yield dataframe
df_lag1 = data.frame(gold_fd$date, gold_fd$value_lag1)
colnames(df_lag1) = c('date', 'value_lag1')
df_lag7 = data.frame(gold_wd$date, gold_wd$value_lag7)
colnames(df_lag7) = c('date', 'value_lag7')

df_lagm = data.frame(gold_md$date, gold_md$value_lagm)
colnames(df_lagm) = c('date', 'value_lagm')

gold_yield = merge(gold, df_lag1, by = 'date')
gold_yield = merge(gold_yield, df_lag7, by = 'date')
gold_yield = merge(gold_yield, df_lagm, by = 'date')
gold_yield = merge(gold_yield, yield, by = 'date')

head(gold_yield)
