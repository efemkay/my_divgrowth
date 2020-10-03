
rm(list=ls())
rawdf = readRDS(file="./Aggregated_10Yr_Financials.Rds")

rawdf$`PMoAD Date` = rawdf$`PMoAD Date` %>% as.Date("%d/%m/%y")
rawdf$`PMoFQED Date` =  rawdf$`PMoFQED Date` %>% as.Date("%d/%m/%y")

rawdf = rawdf[,c(-15)]

df_tmp = merge(rawdf, agg_df[,c('X.name.', 'X.date.', 'X.close.')]
               , by.x=c('StockName', 'PMoAD Date'), by.y=c('X.name.', 'X.date.')
               , all.x=FALSE, all.y=FALSE, sort=FALSE)
df_tmp = merge(df_tmp, agg_df[,c('X.name.', 'X.date.', 'X.close.')]
               , by.x=c('StockName', 'PMoFQED Date'), by.y=c('X.name.', 'X.date.')
               , all.x=FALSE, all.y=FALSE, sort=FALSE, suffixes = c("AD","QED"))

names(df_tmp)


ix1 = df_tmp$StockName %>% unique
ix2 = agg_df$X.name. %>% unique

ixna = which(!(ix2 %in% ix1))

ix2[ixna]

# KGB     OPCOM   OPENSYS SCICOM  VITROX 