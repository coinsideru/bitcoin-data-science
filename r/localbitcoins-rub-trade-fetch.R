library("rjson")
library("dplyr")
library("ggplot2")

fetch_lbtc_rub <- function() {
  rdf <- data.frame(tid=integer(), date=as.Date(character()), amount=double(), price=double())
  
  url <- 'https://localbitcoins.com/bitcoincharts/RUB/trades.json'
  tid = 0
  
  repeat{
    url <- paste0('https://localbitcoins.com/bitcoincharts/RUB/trades.json?since=', tid)
    print(paste("Fetching:", url))
  
    json_data <- fromJSON(file=url)
    if (length(json_data) == 0) {
      break
    }
    
    m <- lapply(
      json_data,
      function(x) c(x['tid'], x['date'], x['amount'], x['price'])
    )
    
    m1 <- do.call(rbind, m)
    df <- as.data.frame(m1)
    
    df$tid <- unlist(df$tid)
    df$date <- unlist(df$date)
    df$amount <- unlist(df$amount)
    df$price <- unlist(df$price)
    
    df$date <- as.POSIXct(df$date, origin="1970-01-01")
  
    rdf <- rbind(rdf, df)

    last <- tail(df, n=1)
    
    tid <- last$tid
    print(paste("Latest date:", last$date))
    Sys.sleep(1)
  }
  
  return(rdf)
}

df <- fetch_lbtc_rub()

df_day <- df %>% mutate(total=as.numeric(price) * as.numeric(amount), day=as.Date(format(as.Date(date), "%Y-%m-%d"))) %>% group_by(day) %>% summarise(total_rub=sum(total))
df_month <- df %>% mutate(total=as.numeric(price) * as.numeric(amount), day=as.Date(format(as.Date(date), "%Y-%m-01"))) %>% group_by(day) %>% summarise(total_rub=sum(total))

df_month %>% ggplot(aes(x=day, y=total_rub)) + geom_point(color="#880011") + stat_smooth(method="auto")