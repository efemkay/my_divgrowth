# my_divgrowth
Tracker for dividend growth investing in Malaysia stock market

## Overview of Files
1. R code files with prefix "cdg_" are currently used to do some backtesting on expected returns investing in div stock with growing DPS & EPS. Explained in detailed under section **Div Growth Backtesting**
2. R code files with prefix "a_" are currently used to build snapshot table to track stocks i may invest. Explained further under section **Div Growth Tracker**
3. /Stock Data are samples of financial data extract (scraped) from i3 website (URL: https://klse.i3investor.com/servlets/stk/fin/0166.jsp?type=last10fy)

### Div Growth Backtesting
1. `cdg_scrape_i3klse_annual.R` is used to scrape from i3 website and save the financial data page as standalone html file
2. `cdg_compile_10yr_annual.R` is used to compile all of the downloaded financial data html files (above) into a single big table for further analysis. It will produce .RDS file to store the big table

3. `cdg_histprice_unzip.R` is used to extract price file downloaded from ShareInvestor website (the downloading is a separate code, not shared here)
4. `cdg_histprice_compile.R` is used to compile the multiple price file extracted (above) into single big table
5. `cdg_merge_fin_price.R` is an attempt to merge the 1st part (financial data) and 2nd part (price data)

### Div Growth Tracker

## Kanban Board
https://www.notion.so/Dividend-Investing-Dash-c38d66a3522b41b29d6d8c7fcd354dad
