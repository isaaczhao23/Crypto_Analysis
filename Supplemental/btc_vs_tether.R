setwd("~/Desktop/Crypto_Analysis-master/Supplemental")
source("custom_functions.R")




bitcoin = generate_coin.list(comparison_date = "2017-01-01", end_date = "2018-08-18") %>% filter(name=="Bitcoin")
tether = generate_coin.list(comparison_date = "2017-01-01", end_date = "2018-08-18")%>% filter(name=="Tether")

# coin.list666 = data.frame(date = seq(from = as.Date("2018-01-01"), to = as.Date("2018-12-31"), by=1), price = 0, name="Bitcoin")
# coin.list66 = generate_coin.list(comparison_date = "2018-01-01", end_date = "2018-03-09")
# coin.list6 = full_join(coin.list666,coin.list66, by=c("name","date"))  %>% filter(name=="Bitcoin")


# plot all btc years
plot(bitcoin$date, log(log(log(bitcoin$price))) , ylab="",col="blue", type="l" )
par(new=TRUE)
plot(tether$date, tether$price , ylab="",col="green", type="l" )

