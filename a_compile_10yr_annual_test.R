##
## Compile all stock data into 1 big table
## Author: Faiz MK
##

# define the lag for which to run

library(rvest)      # to read html tables
library(XML)        # for using readHTMLTable
library(reshape2)   # for using melt()

# find which web table has the financial data table
# scode_list = list.files("./Stock Data/", pattern="html",
#                         all.files=FALSE, full.names=FALSE)
scode_list = sub(".html", "", scode_list)
# scode_list = scode_list[1:5]
for (scode in scode_list) {
  breakout=FALSE
  weblist = read_html(paste('./Stock Data/',scode,'.html',sep=""))
  sname = (html_node(weblist, "title") %>% html_text() %>% strsplit(" ") %>% unlist())[1]
  rm(list='weblist')
  webtable = readHTMLTable(doc=paste('./Stock Data/',scode,'.html',sep=""))
  
  for (i in 1:length(webtable)) {
    if (!is.null(webtable[[i]])) {
      if (nrow(webtable[[i]])>100) {
        fintable = webtable[[i]]
        break
      } else if (i==length(webtable)) breakout=TRUE
    }
  }
  if (breakout==TRUE) { next }   # to break from main loop as scode no datatable
  
  vremove = c("YoY %", "Horiz. %")
  ix = which(fintable$V1 %in% vremove)
  fintable = fintable[-ix,]
  
  # fill up empty row in V1
  ix = which(fintable$V1=="")
  ix2 = ix-1
  fintable[ix,1] = fintable[ix2,1]
  
  # remove rows with NA
  ix = which(is.na(fintable$V4))
  fintable = fintable[-ix,]
  
  ## Cleaning up table formatting for standardized naming
  ##
  # create new var name LCat for better item row name
  # repeat for all rows
  fintable$LCat = ""
  ix = which(fintable$V2=="AQR"&fintable$V1!="Date")
  fintable$LCat[ix] = as.character(fintable$V1[ix])
  for (i in 1:length(fintable$LCat)) {
    if (fintable$LCat[i]=="") {
      fintable$LCat[i] = fintable$LCat[i-1]
    }
  }
  
  cat = fintable$LCat   # create new vector
  cat = NA              # reset it to empty
  # abbreviate only for Price Multiplier
  ix = grep("Price", fintable$LCat, value=FALSE)
  cat[ix] = abbreviate(fintable$LCat[ix], minlength = 3, use.classes = TRUE,
                       dot = FALSE, strict = FALSE, 
                       method = c("left.kept", "both.sides"), named = FALSE)
  # 1st 3 letter for Adjustment on NOSH
  ix = grep("Adj", fintable$LCat, value=FALSE)
  x = fintable$LCat
  cat[ix] = substr(x[ix], 1, 3)
  
  # append the abbrev cat to V1
  ix = which(is.na(cat))
  fintable$V1 = as.character(fintable$V1)
  fintable$V1[-ix] = paste(cat[-ix], fintable$V1[-ix], sep=" ")
  
  # remove LCat column & AQR & CAGR
  fintable = subset(fintable, select=-LCat)
  # remove redundant header rows
  ix = which(fintable$V2=="AQR"&!(fintable$V1=="Last 10 FY Result")&!(grepl("(?<=D )Date", fintable$V1, perl=TRUE)))
  fintable = fintable[-ix,]
  fintable = fintable[, -grep("AQR|CAGR", as.matrix(fintable[1,]))]
  
  
  # subset only Adj EPS, Adj DPS, ROE, PMoFQED Price, PMoFQED P/EPS, PMoFQED DY, and Div Payout %
  vselect = c("Adj EPS", "Adj DPS", "ROE",
              "NP", "Div", "NOSH",
              "PMoFQED Date", "PMoFQED P/EPS", "PMoFQED DY",
              "PMoAD Date", "PMoAD P/EPS", "PMoAD DY",
              "Div Payout %")
  ix = c(which(fintable$V1 %in% vselect))
  fintable = fintable[ix,]
  
  ## Rename the column
  for (i in 1:ncol(fintable)) {
    if (i==1) {names(fintable)[1]="FinRatio"}
    if (i==2) {names(fintable)[2]="T4Q"}
    if (i>=3) {names(fintable)[i] = paste("n-",i-3,sep="")}
  }
  ## Reset the row names
  rownames(fintable) = c()
  
  ## convert % to numeric base 100
  fintable = as.data.frame((sub("%", "", sapply(fintable[,], function(x) as.character(x)))))
  # fintable[,-1] <- suppressWarnings(lapply(fintable[,-1], function(x) as.numeric(as.character(x))))
  
  x = fintable
  x = melt(fintable, id.vars="FinRatio")
  x = x[order(x$FinRatio),]
  x$FinRatio = paste(x$FinRatio, x$variable, sep=" ")
  x = subset(x, select=-variable)
  x = rbind(c(FinRatio="scode", value=scode), c(FinRatio="sname", value=sname), x)
  # fintable = as.data.frame(t(x[,2]))
  row.names(x) = c()
  names(x) = c("FinRatio", scode)
  fintable = x
  # names(fintable) = x$FinRatio
  
  ## compile each counter financials into a master table containing all counters
  ## merge(sort=FALSE) is used to retain columns arrangement (i.e. T4Q, n-0, n-1,...)
  df_sum_sub = fintable
  df_sum_sub[,1] = gsub(",", "", as.character(df_sum_sub[,1]))
  if (!exists('df_sum')) {
    df_sum = df_sum_sub
  } else {
    df_sum = merge(df_sum, df_sum_sub, by="FinRatio", all=TRUE, sort=FALSE)
    # df_sum = rbind(df_sum, df_sum_sub)
  }
  
} # the end of for loop for every scode

## Convert untransposed df_sum columns (except $FinRatio & exclude sname) as numeric
## Transpose df_sum by retaining all columns but 1st ($FinRatio) which is used as colnamees
df_sum[-2,-(1)] <- suppressWarnings(lapply(df_sum[-2,-(1)], function(x) as.numeric(as.character(x))))
y = df_sum$FinRatio
df_sum_t = as.data.frame(t(df_sum[,-1]))
names(df_sum) = y

ix = c(1:2, grep("Date", names(x)))
df_sum[,-ix] = lapply(df_sum[,-ix], FUN=function(x) as.numeric(as.character(x)))
ix = grep("Date", names(df_sum))
df_sum[,ix] = lapply(df_sum[,ix], FUN=function(x) as.Date(x, "%d/%m/%y"))

# save to csv
write.csv(df_sum, file='./Compiled10yrflat.csv')
