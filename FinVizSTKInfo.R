require("data.table");require("pbapply");require("rvest");require("xml2");require("stringr")

getFinVizSTKInfo= function(ticker){
  Sys.sleep(5)
  URL <- paste0("https://finviz.com/quote.ashx?t=",ticker)
  dat <- read_html(URL)
  dat <- dat %>% html_nodes(xpath ="/html/body/div[4]/div/table[2]") %>% html_table()
  df1 <- rbind((cbind(dat[[1]]$X1,dat[[1]]$X2) %>% as.data.frame()),
               (cbind(dat[[1]]$X3,dat[[1]]$X4) %>% as.data.frame()),
               (cbind(dat[[1]]$X5,dat[[1]]$X6) %>% as.data.frame()),
               (cbind(dat[[1]]$X7,dat[[1]]$X8) %>% as.data.frame()),
               (cbind(dat[[1]]$X9,dat[[1]]$X10) %>% as.data.frame()),
               (cbind(dat[[1]]$X11,dat[[1]]$X12) %>% as.data.frame()))
  colnames(df1) <- c("Metric","Value")
  df2 <- as.data.frame(rbind(cbind("Ticker",ticker),cbind("Date",paste(Sys.Date()))))
  colnames(df2) <- c("Metric","Value")
  df1 <- rbind(df2,df1)
  
  df0 <- as.data.frame(t(df1$Value))
  colnames(df0) <- c(df1$Metric)
  df0
}

tmp <- getFinVizSTKInfo("MRNA")
tickers <- c("TSLA","AAPL","GOOGL")

INFO <- pblapply(as.list(tickers),function(x){
  tmp <-try(getFinVizSTKInfo(ticker=x), silent = TRUE)
  if(!inherits(tmp,"try-error"))
    tmp
})
formatKMB <- function(x){
  tmp <- gsub('K', 'e3', x)
  tmp <- gsub('M', 'e6', tmp)
  tmp <- gsub('B', 'e9', tmp)
  as.numeric(tmp)
}
df <- rbindlist(INFO,use.names = TRUE, fill = TRUE)

# convert to numeric columns
df$`Market Cap`   <- formatKMB(df$`Market Cap`) %>% suppressWarnings()
df$Income         <- formatKMB(df$Income) %>% suppressWarnings()
df$Sales          <- formatKMB(df$Sales ) %>% suppressWarnings()
df$`Book/sh`      <- as.numeric(df$`Book/sh`) %>% suppressWarnings()
df$`Cash/sh`      <- as.numeric(df$`Cash/sh`) %>% suppressWarnings()
df$Dividend       <- as.numeric(df$Dividend) %>% suppressWarnings()
df$`Dividend %`   <- as.numeric(gsub("\\%","",df$`Dividend %`))/100 
df$Employees      <- as.numeric(df$Employees) %>% suppressWarnings()
df$Recom          <- as.numeric(df$Recom) %>% suppressWarnings()
df$`P/E`          <- as.numeric(df$`P/E`) %>% suppressWarnings()
df$`Forward P/E`  <- as.numeric(df$`Forward P/E`) %>% suppressWarnings()
df$PEG            <- as.numeric(df$PEG) %>% suppressWarnings()
df$`P/S`          <- as.numeric(df$`P/S`) %>% suppressWarnings()
df$`P/B`          <- as.numeric(df$`P/B`) %>% suppressWarnings()
df$`P/C`          <- as.numeric(df$`P/C`) %>% suppressWarnings()
df$`P/FCF`        <- as.numeric(df$`P/FCF`) %>% suppressWarnings()
df$`Quick Ratio`  <- as.numeric(df$`Quick Ratio`) %>% suppressWarnings()
df$`Current Ratio`<- as.numeric(df$`Current Ratio`) %>% suppressWarnings()
df$`Debt/Eq`      <- as.numeric(df$`Debt/Eq`) %>% suppressWarnings()
df$`LT Debt/Eq`   <- as.numeric(df$`LT Debt/Eq`) %>% suppressWarnings()
df$SMA20          <- as.numeric(gsub("\\%","",df$SMA20))/100 %>% suppressWarnings()
df$`EPS (ttm)`    <- as.numeric(df$`EPS (ttm)`) %>% suppressWarnings()
df$`EPS next Y`   <- as.numeric(df$`EPS next Y`) %>% suppressWarnings()
df$`EPS next Q`   <- as.numeric(df$`EPS next Q`) %>% suppressWarnings()
df$`EPS this Y`   <- as.numeric(gsub("\\%","",df$`EPS this Y`))/100 %>% suppressWarnings()
colnames(df)[31]  <- "EPS this Y %"
df$`EPS this Y %` <- as.numeric(gsub("\\%","",df$`EPS this Y %`))/100 %>% suppressWarnings()
df$`EPS next 5Y`  <- as.numeric(gsub("\\%","",df$`EPS next 5Y`))/100 %>% suppressWarnings()
df$`EPS past 5Y`  <-as.numeric(gsub("\\%","",df$`EPS past 5Y`))/100 %>% suppressWarnings()
df$`Sales past 5Y`<- as.numeric(gsub("\\%","",df$`Sales past 5Y`))/100 %>% suppressWarnings()
df$`Sales Q/Q`    <- as.numeric(gsub("\\%","",df$`Sales Q/Q`))/100 %>% suppressWarnings()
df$`EPS Q/Q`      <- as.numeric(gsub("\\%","",df$`EPS Q/Q`))/100 %>% suppressWarnings()
df$Earnings       <- gsub("AMC",paste0(format(Sys.Date(), "%Y") ," 13:00"),df$Earnings)
df$Earnings       <- gsub("BMO",paste0(format(Sys.Date(), "%Y") ," 04:00"),df$Earnings)
df$Earnings       <- as.POSIXct(df$Earnings, format="%b %d %Y %H:%M")
df$SMA50          <- as.numeric(gsub("\\%","",df$SMA50))/100 %>% suppressWarnings()
df$`Insider Own`  <- as.numeric(gsub("\\%","",df$`Insider Own`))/100 %>% suppressWarnings()
df$`Insider Trans`<- as.numeric(gsub("\\%","",df$`Insider Trans`))/100 %>% suppressWarnings()
df$`Inst Own`     <- as.numeric(gsub("\\%","",df$`Inst Own`))/100 %>% suppressWarnings()
df$`Inst Trans`   <- as.numeric(gsub("\\%","",df$`Inst Trans`))/100 %>% suppressWarnings()
df$ROA            <- as.numeric(gsub("\\%","",df$ROA))/100 %>% suppressWarnings()
df$ROE            <- as.numeric(gsub("\\%","",df$ROE))/100 %>% suppressWarnings()
df$ROI            <- as.numeric(gsub("\\%","",df$ROI))/100 %>% suppressWarnings()
df$`Gross Margin` <- as.numeric(gsub("\\%","",df$`Gross Margin`))/100 %>% suppressWarnings()
df$`Oper. Margin` <- as.numeric(gsub("\\%","",df$`Oper. Margin`))/100 %>% suppressWarnings()
df$`Profit Margin`<- as.numeric(gsub("\\%","",df$`Profit Margin`))/100 %>% suppressWarnings()
df$Payout         <- as.numeric(gsub("\\%","",df$Payout))/100 %>% suppressWarnings()
df$SMA200         <- as.numeric(gsub("\\%","",df$SMA200))/100 %>% suppressWarnings()
df$`Shs Outstand` <- formatKMB(df$`Shs Outstand`) %>% suppressWarnings()
df$`Shs Float`    <- formatKMB(df$`Shs Float`) %>% suppressWarnings()
df$`Short Float`  <- as.numeric(gsub("\\%","",df$`Short Float`))/100 %>% suppressWarnings()
df$`Short Ratio`  <- as.numeric(df$`Short Ratio`) %>% suppressWarnings()
df$`Target Price` <- as.numeric(df$`Target Price`) %>% suppressWarnings()
df$`52W High $`   <- as.numeric(do.call(rbind,str_split(df$`52W Range`,"\\-"))[,2]) %>% suppressWarnings()
df$`52W Low $`    <- as.numeric(do.call(rbind,str_split(df$`52W Range`,"\\-"))[,1]) %>% suppressWarnings()
df$`52W High`     <- as.numeric(gsub("\\%","",df$`52W High`))/100 %>% suppressWarnings()
df$`52W Low`      <- as.numeric(gsub("\\%","",df$`52W Low`))/100 %>% suppressWarnings()
df$`RSI (14)`     <- as.numeric(df$`RSI (14)`) %>% suppressWarnings()
df$`Rel Volume`   <- as.numeric(df$`Rel Volume`) %>% suppressWarnings()
df$`Avg Volume`   <- formatKMB(df$`Avg Volume`) %>% suppressWarnings()
df$Volume         <- as.numeric(gsub("\\,","",df$Volume))
df$`Perf Week`    <- as.numeric(gsub("\\%","",df$`Perf Week` ))/100 %>% suppressWarnings()
df$`Perf Month`   <- as.numeric(gsub("\\%","",df$`Perf Month`))/100 %>% suppressWarnings()
df$`Perf Quarter` <- as.numeric(gsub("\\%","",df$`Perf Quarter`))/100 %>% suppressWarnings()
df$`Perf Half Y`  <- as.numeric(gsub("\\%","",df$`Perf Half Y`))/100 %>% suppressWarnings()
df$`Perf Year`    <- as.numeric(gsub("\\%","",df$`Perf Year`))/100 %>% suppressWarnings()
df$`Perf YTD`     <- as.numeric(gsub("\\%","",df$`Perf YTD`))/100 %>% suppressWarnings()
df$Beta           <- as.numeric(df$Beta) %>% suppressWarnings()
df$ATR            <- as.numeric(df$ATR) %>% suppressWarnings()
df$`Volatility W`   <- as.numeric(gsub("\\%"," ",do.call(rbind,str_split(df$Volatility," "))[,1]))/100 %>% suppressWarnings()
df$`Volatility M`   <- as.numeric(gsub("\\%"," ",do.call(rbind,str_split(df$Volatility," "))[,2]))/100 %>% suppressWarnings()
df$`Prev Close` <- as.numeric(df$`Prev Close`) %>% suppressWarnings()
df$Price <- as.numeric(df$Price) %>% suppressWarnings()
df$Change <- as.numeric(gsub("\\%","",df$Change))/100 %>% suppressWarnings()


#saveRDS(df, "Stats.rds")
write.table(df,paste0("Stats.csv"), sep=",")

