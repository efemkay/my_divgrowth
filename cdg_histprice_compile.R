

rm(list=ls())
# library(xlsx)     # needed to read from Excel file
# library(stringr)  # needed to do some padding in string operations
# library(utils)    # to unzip files
# library(reshape2) # for using melt()
library(dplyr)    # for pipe %>%



pricefile_list = list.files(path = "./Price Data - hist _old", pattern = "history_.*.csv", full.names = TRUE)
# pricefile_list = pricefile_list[1:5]

agg_df = read.csv(file=pricefile_list[1])
agg_df = NULL

# data_loc="./Stock Data/"
for(pricefile in pricefile_list){
  # webpage = read_html(paste0(data_loc, scode, '.html'))
  agg_df_tmp = read.csv(file=pricefile)
  agg_df = rbind(agg_df, agg_df_tmp)
}

agg_df$X.date. = agg_df$X.date. %>% strptime("%Y%m%d") %>% as.Date()


### For plotting ##############################################
library(plotly)
library(highcharter)
library(xts)

data <- agg_df %>% filter(X.name.=="GASMSIA")
p <- plot_ly(data, x = ~X.date., y = ~X.close., type = 'scatter', mode = 'lines')
p

highchart(type = "stock") %>% 
  hc_add_series(data = xts(x=data$X.close., order.by=data$X.date.)
                , type = "line")
