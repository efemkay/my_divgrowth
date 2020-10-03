## Part of investing strategy "Continued Dividend Growth" or CDG
## - Purpose -
## 1. Extract 10 year financial table from pre-saved html of klse.i3.com website
## 2. Compile into 1 big dataset
## Note: Whole code Will take approximately 2-3 minutes.
## Note: Scraping the i3 website is done in separate R code

rm(list=ls())

stime = Sys.time()
library(xlsx)     # needed to read from Excel file
library(stringr)  # needed to do some padding in string operations
library(rvest)      # to read html tables
library(XML)        # for using readHTMLTable
library(reshape2)   # for using melt()

scode_list = as.matrix(read.xlsx(file="./s_code_shariah_all.xlsx", sheetIndex=1, startRow=2)[,3])
# scode_list = as.matrix(read.xlsx(file="./s_code_personal.xlsx", sheetIndex=1, startRow=2)[,3])
scode_list = unique(scode_list[which(scode_list!="FALSE")])
scode_list = str_pad(scode_list, width=4, side="left", pad="0")
etime_codelist = Sys.time()

Extract_Fin_Annual = function(scode, data_loc="./Stock Data/"){
  
  webpage = read_html(paste0(data_loc, scode, '.html'))
  sname = webpage %>% html_node(css="title") %>% html_text() %>% strsplit(" ") %>% unlist() %>% .[1]
  fintable = try(webpage %>% html_nodes(xpath='//*[@id="content"]/table[2]') %>% html_table() %>% .[[1]], silent=TRUE)

  if(!(is.null(dim(fintable)))){
    colnames(fintable) = paste0("V", seq(from=1,to=ncol(fintable)))
    
    ## Cleanup and remove irrelevant rows
    vremove = c("YoY %", "Horiz. %")
    ix = which(fintable$V1 %in% vremove)
    fintable = fintable[-ix,]
    fintable[1,1] = "Fin Year"
    # fill up empty row in V1
    ix = which(fintable$V1=="")
    ix2 = ix-1
    fintable$V1[ix] = fintable$V1[ix2]
    
    
    
    ## Standardized row/category naming. By default it is the original row names
    ## except for Prices and Adjusted NOSH rows 
    # create new var name LCat for better item row name, repeat for all rows
    fintable = cbind(LCat="", fintable)
    fintable$LCat = fintable$LCat %>% as.character()
    ix = which(fintable$V2=="AQR"&fintable$V1!="Date")
    fintable$LCat[ix] = as.character(fintable$V1[ix])
    for (i in 1:length(fintable$LCat)) {
      if (fintable$LCat[i]=="") {
        fintable$LCat[i] = fintable$LCat[i-1]
      }
    }
    
    ## Renaming & abbreviating Financial Items for Prices & Adjusted NOSH
    ## (a) create new vector to store special abbreviation (i.e Prices & Adjusted NOSH)
    cat = fintable$LCat   # create new vector
    cat = NA              # reset it to empty
    ## (b) abbreviate only for Price Multiplier & save to cat
    ix = grep("Price", fintable$LCat, value=FALSE)
    cat[ix] = abbreviate(fintable$LCat[ix], minlength = 3, use.classes = TRUE,
                         dot = FALSE, strict = FALSE, 
                         method = c("left.kept", "both.sides"), named = FALSE)
    ## (c) abbreviate (1st 3 letter) for Adjustment on NOSH & save to cat
    ix = grep("Adj", fintable$LCat, value=FALSE)
    x = fintable$LCat
    cat[ix] = substr(x[ix], 1, 3)
    ## (d) replace the Prices & Adjusted rows with abbreviated one's to V1
    ix = which(is.na(cat))
    fintable$V1 = as.character(fintable$V1)
    fintable$V1[-ix] = paste(cat[-ix], fintable$V1[-ix], sep=" ")
    
    ## Further cleanup. Remove LCat column & AQR & CAGR, and header rows
    fintable = subset(fintable, select=-LCat)
    fintable = fintable[, -grep("AQR|CAGR", fintable[1,])]
    ## Remove header rows (1/3)
    ix = which(fintable$V3=="T4Q"&!(fintable$V1=="Fin Year")&!(fintable$V1==grep("Date",fintable$V1,value=TRUE)))
    fintable = fintable[-ix,]
    ## Remove header rows (2/3)
    ix = which(fintable$V3==fintable$V1)
    fintable = fintable[-ix,]
    ## Remove header rows (3/3)
    ix = which(fintable$V3=="")
    fintable = fintable[-ix,]
    ## Remove header rows (4/4)
    ix = grep("Price.*Date", fintable$V1)
    fintable = fintable[-ix,]
    
    
    
    ## Rename the column & reset the row names
    for (i in 1:ncol(fintable)) {
      if (i==1) {names(fintable)[1] = "FinRatio"}
      if (i==2) {names(fintable)[2] = 0}
      if (i>=3) {names(fintable)[i] = i-2}
    }
    rownames(fintable) = c()
    
    ## convert % to numeric base 100
    fintable[] = (gsub("%", "", sapply(fintable[,], function(x) as.character(x))))
    fintable[] = (gsub(",", "", sapply(fintable[,], function(x) as.character(x))))
    fintable[] = lapply(fintable, as.character)
    
    ## Transpose to flat-table for easy rbind later on
    fintable_t = fintable %>% t() %>% as.data.frame()
    colnames(fintable_t) = fintable$FinRatio
    fintable_t = fintable_t[-1,]    # remove header row
    fintable_t = cbind(StockName=sname
                       , StockCode=scode
                       , YearTag=rownames(fintable_t)
                       , fintable_t
    )
    rownames(fintable_t) = c()
    fintable_t[] = lapply(fintable_t, as.character)
    
    ## Convert relevant columns to numeric
    ix = grep("Stock|Date|Fin Year", names(fintable_t))
    fintable_t[-ix] = lapply(fintable_t[-ix], as.numeric) %>% as.data.frame() %>% suppressWarnings()

  } else fintable_t=NULL
  return(fintable_t)
}

agg_df = Extract_Fin_Annual(scode=7090)
agg_df = NULL

# data_loc="./Stock Data/"
for(scode in scode_list){
  # webpage = read_html(paste0(data_loc, scode, '.html'))
  agg_df_tmp = Extract_Fin_Annual(scode=scode)
  agg_df = rbind(agg_df, agg_df_tmp)
}

etime_compile = Sys.time()

## save it to RDS
saveRDS(agg_df, file="./Aggregated_10Yr_Financials.Rds")

## printout the running time
print(round(etime_codelist - stime, 2))
print(round(etime_compile - etime_codelist, 2))
