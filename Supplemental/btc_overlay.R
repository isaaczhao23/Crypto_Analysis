setwd("~/Desktop/Crypto_Analysis-master")
source("custom_functions.R")






coin.list111 = data.frame(date = seq(from = as.Date("2013-01-01"), to = as.Date("2013-12-31"), by=1), price = 0, name="Bitcoin")
coin.list11 = generate_coin.list(comparison_date = "2013-01-01", end_date = "2013-12-31")
coin.list1 = full_join(coin.list111,coin.list11, by=c("name","date"))  %>% filter(name=="Bitcoin")

coin.list2 = generate_coin.list(comparison_date = "2014-01-01", end_date = "2014-12-31") %>% filter(name=="Bitcoin")
coin.list3 = generate_coin.list(comparison_date = "2015-01-01", end_date = "2015-12-31")%>% filter(name=="Bitcoin")
coin.list4 = generate_coin.list(comparison_date = "2016-01-01", end_date = "2016-12-31")%>% filter(name=="Bitcoin")
coin.list5 = generate_coin.list(comparison_date = "2017-01-01", end_date = "2017-12-31")%>% filter(name=="Bitcoin")

coin.list666 = data.frame(date = seq(from = as.Date("2018-01-01"), to = as.Date("2018-12-31"), by=1), price = 0, name="Bitcoin")
coin.list66 = generate_coin.list(comparison_date = "2018-01-01", end_date = "2018-03-09")
coin.list6 = full_join(coin.list666,coin.list66, by=c("name","date"))  %>% filter(name=="Bitcoin")


# plot all btc years
plot(coin.list1$date, log(coin.list1$price.y) , ylab="",col="red", type="l" , xlim= c(as.Date("2013-01-01"),  as.Date("2013-12-31")))
par(new=TRUE)
plot(coin.list2$date, log(coin.list2$price) , ylab="", col="orange", type="l")
par(new=TRUE)
plot(coin.list3$date, log(coin.list3$price) , ylab="", col="yellow", type="l")
par(new=TRUE)
plot(coin.list4$date, log(coin.list4$price) , ylab="", col="green", type="l")
par(new=TRUE)
plot(coin.list5$date, log(coin.list5$price) , ylab="", col="blue", type="l")
par(new=TRUE)
plot(coin.list6$date, log(coin.list6$price.y) , ylab="", col="mediumpurple", type="l")

# plot btc 2015 vs btc 2018
plot(coin.list3$date, log(coin.list3$price) , ylab="", col="yellow", type="l")
par(new=TRUE)
plot(coin.list6$date, log(coin.list6$price.y) , ylab="", col="mediumpurple", type="l")



####################################


coin.list111 = data.frame(date = seq(from = as.Date("2017-01-01"), to = as.Date("2017-12-31"), by=1), price = 0, name="Bitcoin")
coin.list11 = generate_coin.list(comparison_date = "2017-01-01", end_date = "2017-12-31")
coin.list1 = full_join(coin.list111,coin.list11, by=c("name","date"))  %>% filter(name=="Nano")


coin.list666 = data.frame(date = seq(from = as.Date("2018-01-01"), to = as.Date("2018-12-31"), by=1), price = 0, name="Bitcoin")
coin.list66 = generate_coin.list(comparison_date = "2018-01-01", end_date = "2018-02-27")
coin.list6 = full_join(coin.list666,coin.list66, by=c("name","date"))  %>% filter(name=="Nano")


plot(coin.list5$date, log(coin.list5$price) , ylab="", col="blue", type="l")
par(new=TRUE)
plot(coin.list6$date, log(coin.list6$price.y) , ylab="", col="mediumpurple", type="l")

