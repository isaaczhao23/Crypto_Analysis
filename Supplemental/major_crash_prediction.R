setwd("~/Desktop/Crypto_Analysis-master/Supplemental")
source("custom_functions.R")

# Creates dates for uptrend/downtrend
df<-data.frame(ath.date=as.Date(c("2011-06-08","2013-11-30","2017-12-16"), origin="1970-01-01"),
	atl.date = as.Date(c("2011-12-18","2015-10-13","2018-11-18"), origin="1970-01-01"))


# btc.df has the price for every date
bitcoin_history_early = read_excel("btc_early_history.xlsx") %>% dplyr::rename(price = close) %>% dplyr::select(date,price) %>% mutate(date = as.Date(date))
bitcoin_history = crypto_history("Bitcoin")
btc.df1 = bitcoin_history %>% dplyr::rename(price = close) %>% dplyr::select(date,price) 
btc.df = rbind(bitcoin_history_early,btc.df1)

# Assigns prices for dates of uptrend/downtrend
btc.df2 = btc.df %>% dplyr::rename(ath.date = date) %>% dplyr::rename(ath = price)
btc.df3 = btc.df %>% dplyr::rename(atl.date = date) %>% dplyr::rename(atl = price)
df = left_join(df,btc.df2,by="ath.date") %>% left_join(btc.df3,by="atl.date")


df$days.ath.atl = as.numeric(df$atl.date - df$ath.date)
df$reg.ath.atl = regression(start.date = df$ath.date,end.date = df$atl.date,btc.df)

model = lm(days.ath.atl~reg.ath.atl - 1,df[-3,])
summary(model)

predict(model,df[3,])

df$colors = c("old","old","now")
ggplot(df,aes(reg.ath.atl,days.ath.atl))+
	geom_point(aes(color=colors),size=5)
	


