

rm(list=ls())
library(xlsx)     # needed to read from Excel file
library(stringr)  # needed to do some padding in string operations
library(rvest)      # to read html tables
library(XML)        # for using readHTMLTable
library(reshape2)   # for using melt()
library(dplyr)

rawdf = readRDS(file="./Aggregated_10Yr_Financials.Rds")

scode_list = unique(rawdf$StockCode)

# singledf = rawdf[rawdf$StockCode %in% scode_list[c(463,464)],]
singledf = rawdf[,c('StockName','StockCode','YearTag','Fin Year','Revenue','Div')]

calc_cagr <- function(df, n) {
  df <- df %>%
    arrange(StockCode, desc(YearTag)) %>%
    group_by(StockCode) %>%
    mutate(Rev_yoy = round(((Revenue-lag(Revenue, 1))/lag(Revenue, 1))*100, digits=1)
           , Div_yoy = round(((Div-lag(Div, 1))/lag(Div, 1))*100, digits=1)
           , Rev_cagr = round((((Revenue/lag(Revenue, n))^(1/n))-1)*100, digits=1)
           , Div_cagr = round((((Div/lag(Div, n))^(1/n))-1)*100, digits=1)
           )

  return(df)
}

outputdf = calc_cagr(singledf,3)

## Part 2: Getting unique counters that are dividend growers
unique_divgrower = outputdf %>%
  arrange(StockCode, desc(YearTag)) %>%
  group_by(StockCode) %>%
  filter(YearTag %in% c(2) & Div_cagr>0) %>%
  .[[2]]

test = outputdf %>%
  filter(YearTag %in% c(1) & StockCode %in% unique_divgrower) %>%
  mutate(Div_pos_yoy = Div_yoy>=0
         ,Div_pos_cagr = Div_cagr>=0)

table(test[,c('Div_pos_yoy','Div_pos_cagr')]) %>% prop.table() %>% round(digits=2)

## Code reservoir ==================================================================================

# unique_divgrower = outputdf %>%
#   arrange(StockCode, desc(YearTag)) %>%
#   group_by(StockCode) %>%
#   filter(YearTag %in% c(1,2)) %>%
#   mutate(cagr_stat = prod(cagr>=0)) %>%
#   filter(cagr_stat>=1) %>%
#   .[[2]] %>% unique()
