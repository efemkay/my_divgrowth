## This script is meant to automate updating watch list for stock investment. The output would be
## a table of positive 3-year CAGR for EPS & DPS for counter with potential to give good returns.
##
## Step 1: update the counter watch list in google spreadsheets
## Step 2: update the counter list in s_code_personal.xlsx
## Step 3: run this script
##
rm(list=ls())

stime = Sys.time()
library(xlsx)     # needed to read from Excel file
library(stringr)  # needed to do some padding in string operations

scode_list = as.matrix(read.xlsx(file="./s_code_personal.xlsx", sheetIndex=1, startRow=2)[,3])
scode_list = unique(scode_list[which(scode_list!="FALSE")])
scode_list = str_pad(scode_list, width=4, side="left", pad="0")
etime_codelist = Sys.time()

## Call the scraping function and scrape from the i3 website. Output automatically
## saved to "./Stock Data/"
source("a_scrape_i3klse_annual.R")
scrape_i3klse_annual(scode_list)
etime_scrape = Sys.time()

## Call the compile 10 year flat function and save output to df_data data frame
source("a_compile_10yr_annual_function.R")
df_data = Compile_10yr_Annual(scode_list)
etime_compile = Sys.time()

## Customizing columns to fit my objective, In this case, only to look at 3-year CAGR
## Manually defining the columns to ensure proper order
mycolumns = c("aascode",
              "aasname",
              "Div Payout  aT4Q",
              "Adj EPS aT4Q",
              "Adj EPS n-0",
              "Adj EPS n-1",
              "Adj EPS n-2",
              "Adj EPS n-3",
              "ROE aT4Q",
              "Adj DPS aT4Q",
              "Adj DPS n-0",
              "Adj DPS n-1",
              "Adj DPS n-2",
              "Adj DPS n-3")
## Sort the rows by the stock name. Clear the random row numbers.
## Save the final output to csv file
df = df_data[order(df_data$aasname),mycolumns]
rownames(df) = NULL
write.csv(df, file='./Mytrade3yrDivPlay.csv')
etime_saveout = Sys.time()

## printout the running time
print(round(etime_codelist - stime, 2))
print(round(etime_scrape - etime_codelist, 2))
print(round(etime_compile - etime_scrape, 2))
print(round(etime_saveout - etime_compile, 2))
print(round(etime_saveout - stime, 2))
