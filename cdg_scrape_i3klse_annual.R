##
## Web-scraping for i3 KLSE
## Author: Faiz MK
##

library(rvest)    # to read html tables
library(XML)      # to read xml
library(stringr)


scrape_i3klse_annual = function(scode_listx) {
  
  ## Download from klse.i3investor.com website via these sample url
  ## http://klse.i3investor.com/servlets/stk/fin/7052.jsp?type=last10fy
  ## Do it for every stock quote code in the csv file
  for (scode in scode_listx) {
    surl = paste('http://klse.i3investor.com/servlets/stk/fin/',scode,'.jsp?type=last10fy',
                 sep="")
    sdest = paste('./Stock Data/',scode,'.html',sep="")
    download.file(url=surl, destfile=sdest, method='auto')
    
    Sys.sleep(1)  # use delay to avoid hogging website's bandwidth
  }
}

## Read list of stock quote code (from a csv file) to download
scode_list = as.matrix(read.xlsx(file="./s_code_shariah_all.xlsx", sheetIndex=1, startRow=2)[,3])
scode_list = unique(scode_list[which(scode_list!="FALSE")])
scode_list = str_pad(scode_list, width=4, side="left", pad="0")

## Run the scraping
scrape_i3klse_annual(scode_listx = scode_list)
