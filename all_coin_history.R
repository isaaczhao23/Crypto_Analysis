setwd("~/Desktop/Crypto_Analysis")

library(crypto)
library(dplyr)

setwd("~/Crypto_Analysis")

#Retreieve most up to date data
#coin.list = getCoins()

# Only gets first 20 ranked coins
#coin.list = coin.list %>% 
    #select(symbol,name,date,ranknow,close) %>% 
    #filter(ranknow %in% c(1:20))

#save(coin.list, file = "coin_list.Rda")
load("coin_list.Rda")

# Only look at since Jan 1, 2016 for comparison purposes
coin.list = dplyr::filter(coin.list, !grepl('2013', date))
coin.list = dplyr::filter(coin.list, !grepl('2014', date))
coin.list = dplyr::filter(coin.list, !grepl('2015', date))


#feb_3_2018 = 
#feb_3_2017 = 

# Seperates coins by coin rank/name
coin = list()

for(i in 1:20){
    coin[[i]] = filter(coin.list, ranknow == i)
}

