check_packages = function(names){
    for(name in names){
        if (!(name %in% installed.packages()))
            install.packages(name, repos="http://cran.us.r-project.org",quiet=TRUE,dependencies=TRUE) #if package not installed, install the package
        library(name, character.only=TRUE,warn.conflicts=FALSE,quietly=TRUE)
    }
}

check_packages(c("ggpubr","ggthemes","RColorBrewer","MASS","plyr","RcmdrMisc", "xts","tidyverse", "tidyr","scales", "crypto", "directlabels", "shiny","ggpubr", "ggrepel","readxl","gridExtra","sjPlot","lubridate","dplyr","ggplot2"))
select <- dplyr::select
rename <- dplyr::rename
mutate <- dplyr::mutate
summarize <- dplyr::summarize
arrange <- dplyr::arrange

###############################################################################################
###############################################################################################

percentage = function(x){
    
        output =  paste(as.character(comma(round(x*100,0))), "%", sep=""  )
    return(output) 
}


###############################################################################################
###############################################################################################

model_fit_stats <- function(linear.model) {
  r.sqr <- summary(linear.model)$r.squared
  adj.r.sqr <- summary(linear.model)$adj.r.squared
  pre.r.sqr <- pred_r_squared(linear.model)
  PRESS <- PRESS(linear.model)
  return.df <- data.frame(r.squared = r.sqr, adj.r.squared = adj.r.sqr, pred.r.squared = pre.r.sqr, press = PRESS)
  return(return.df)
}

pred_r_squared <- function(linear.model) {
  #' Use anova() to get the sum of squares for the linear model
  lm.anova <- anova(linear.model)
  #' Calculate the total sum of squares
  tss <- sum(lm.anova$'Sum Sq')
  # Calculate the predictive R^2
  pred.r.squared <- 1-PRESS(linear.model)/(tss)
  
  return(pred.r.squared)
}


PRESS <- function(linear.model) {
  #' calculate the predictive residuals
  pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
  #' calculate the PRESS
  PRESS <- sum(pr^2)
  
  return(PRESS)
}

# use ldply(list(fit1, fit2), model_fit_stats) to compare predictive accuracy
###############################################################################################
###############################################################################################


generate_coin.list <- function(comparison_date, end_date, topranks = 30){
    
    # Turns dates into 'yyyymmdd' format to use with getCoins()
    convert_date_df = data.frame(a = comparison_date, b = end_date) %>%
        separate(a,c("year1","month1","day1"),"-") %>%
        separate(b,c("year2","month2","day2"),"-") %>%
        mutate(date1=paste0(year1,month1,day1)) %>%
        mutate(date2=paste0(year2,month2,day2))
    comparison_date1 = convert_date_df$date1
    end_date1 = convert_date_df$date2
    
    if (comparison_date < end_date){
            coin.list = crypto_history(limit= topranks, start_date = comparison_date1, end_date = end_date1)  %>%  dplyr::select(name,date,close) %>% dplyr::rename(price = close)
    } else {
            coin.list = crypto_history(limit= topranks, start_date = end_date1, end_date = comparison_date1) %>%   dplyr::select(name,date,close) %>% dplyr::rename(price = close)
    }
}

###############################################################################################
###############################################################################################
# Used for creating btc prediction dataframe. 
# Needs btc.df (list of all dates with all btc values) to run this function

regression = function(start.date,end.date,btc.df) {
n = length(start.date)
output = rep(NA,n)

for (i in 1:n){
	if (is.na(start.date[i]) || is.na(end.date[i])){
	output[i] = NA
	} else {
   price.df = btc.df %>% filter(date <= end.date[i], date >= start.date[i] ) %>% dplyr::select(price) 	
          days = seq(from=1,to=nrow(price.df),by=1)
          reg.df = cbind(price.df,days)
          output[i] = lm(price~days,reg.df)$coefficients[[2]] / reg.df$price[1]  *100
	   }
}
return(output)
}




bad.months.function = function(date) {
	output = rep(NA,length(date))
	for (i in 1:length(date)){
	month1 = as.numeric(month(date[i]))
	if ( (month1 >=1 && month1 < 3) || month1 >= 11   ){
	output[i] = 1
	} else{
   output[i] = 0
	}
	}
return(as.factor(output))
}


###############################################################################################
###############################################################################################

comparison_chart <- function(comparison_date, end_date = Sys.Date(), month_or_day = "day", interval = 1, type = "marketcap", topranks=50, view_chart=TRUE){
    
    # Creates data
    coin.list = generate_coin.list(comparison_date, end_date, topranks=50) 
    
    
    if (comparison_date < end_date){
    date_seq = seq(from = as.Date(comparison_date), to = as.Date(end_date), by=paste("+", as.character(interval), " ", month_or_day, sep=""))
    } else {
    date_seq = seq(from = as.Date(comparison_date), to = as.Date(end_date), by=paste("-", as.character(interval), " ", month_or_day, sep=""))
    }
    
    dates = list()
    for (i in 1:length(date_seq)){
        char_date <-  paste("price", as.character(date_seq[i]), sep = "_")
        dates[[i]] = filter(coin.list, date ==  as.character(date_seq[i])) %>%       
            dplyr::rename(!!char_date:=price) %>% 
            dplyr::select(-date)
    }
    
    price_history_df = join_all(dates, by =c("name"), type = 'full')
    
    for (i in 1:length(date_seq)){
        char_date <- as.character(date_seq[i])
        starting_date <-  paste("price", as.character(date_seq[1]), sep = "_")
        ending_date <-  paste("price", as.character(date_seq[i]), sep = "_")
        if (comparison_date < end_date){ 
            price_history_df[[char_date]] = (price_history_df[[ending_date]] - price_history_df[[starting_date]]) / price_history_df[[starting_date]]
        } else{
            price_history_df[[char_date]] = (price_history_df[[starting_date]] - price_history_df[[ending_date]]) / price_history_df[[ending_date]]
        }
    }
    
    price_history_df = price_history_df %>% dplyr::select(-contains("price")) %>% dplyr::select(-2)     # Removes prices and today's date
    
    
    #### Creates dataframe for sorting by marketcap with no percent
    nopercent_history_df = price_history_df
    for(i in 2: (ncol(price_history_df))){
        nopercent_history_df[,i] = price_history_df[,i]
    }
 
    
    date_seq_name = format(date_seq[2:ncol(price_history_df)], format="%B %d, %Y")
    #### Creates dataframe for sorting by marketcap #####
    mktcap_history_df = price_history_df
    for(i in 2: (ncol(price_history_df))){
        mktcap_history_df[,i] = percentage(price_history_df[,i])
        mktcap_history_df[,i][grep("NA%", mktcap_history_df[,i])] <- ""
    }
    colnames(mktcap_history_df) = c("Name",date_seq_name)
    
    #### Creates dataframe for sorting by coin name ######
    name_history_df = arrange(mktcap_history_df, Name)
    colnames(name_history_df) = c("Name",date_seq_name)
    
    
    #### Creates dataframe sorting each column by highest percent gain #####
    sort_df = list()
    for ( i in 1:ncol(price_history_df)){
        sort_df[[i]] = price_history_df[order(price_history_df[,i], decreasing=TRUE), c(1,i)] 
    }
    
    sort_df[[1]] = NULL
    
    percent_history_df = matrix(0,nrow(price_history_df), ncol(price_history_df)) %>% as.data.frame() 
    colnames(percent_history_df) = c("Rank",date_seq_name)
    
    percent_history_df[,1] = paste( "#", as.character(c(1:nrow(percent_history_df))), sep="")
    
    for(i in 2: (ncol(price_history_df))){
        percent_history_df[,i] = paste( percentage(sort_df[[i-1]][,2]), " (", sort_df[[i-1]][,1], ")", sep="")
        percent_history_df[,i][grep("NA%", percent_history_df[,i])] <- ""
    }
    
    # Pick which dataset to use for output
    if (type == "percent"){
        output = percent_history_df
    } else if (type =="name"){
        output = name_history_df
    } else if (type =="nopercent") {
        output = nopercent_history_df
    } else {
        output = mktcap_history_df
    }
    
    
    if (view_chart == TRUE){
    View(output)
title = paste("Percent Change of Top",topranks, "Cryptocurrency Prices Compared to the Prices of", format(comparison_date, format="%B %d, %Y"))
sjPlot::tab_df(output, title, alternate.rows=TRUE) 
    }else {
	    return(output)
    }

}

###############################################################################################
###############################################################################################


comparison_graph = function(comparison_date, end_date = Sys.Date()-1, coins=1:5, unit="USD",line_type=NULL) {
    load("all_coins.R")
	
	if (comparison_date > as.Date("2018-10-01") || end_date > as.Date("2018-10-01")){
	if (is.character(coins)){
		all_coins_new = crypto_history(coin = coins, limit = NULL, start_date = "20180930") %>% 
			select(name,date,close) %>% rename(price=close)
	}else if (is.numeric(coins)){
		all_coins_new = crypto_history(limit = max(coins), start_date = "20180930") %>% 
			select(name,date,close) %>% rename(price=close)
	} else {stop("INVALID COINS INPUT FORMAT")}
	
	all_coins = all_coins %>% bind_rows(all_coins_new)
	}
	
	if (unit=="USD"){
 		if (is.character(coins)) {
			data1 = all_coins %>% filter(name %in% coins) %>% filter(date >= comparison_date & date <= end_date )
			min = data1 %>% group_by(name) %>% slice(1) %>% rename(start_price = price)
			data2 = data1 %>% full_join(min,by=c("name")) %>% rename(date = date.x) %>% select(-date.y) %>% 
				mutate(percentage = (price-start_price)/start_price)
		
    	}else if (is.numeric(coins)){
    		ranks = crypto_list(coin = unique(all_coins$name), start_date = Sys.Date()-1, Sys.Date()-1) %>% select(name,rank)
    		data1 = all_coins %>% filter(name %in% ranks$name[coins]) %>% filter(date >= comparison_date & date <= end_date )
			min = data1 %>% group_by(name) %>% slice(1) %>% rename(start_price = price)
			data2 = data1 %>% full_join(min,by=c("name")) %>% rename(date = date.x) %>% select(-date.y) %>% 
				mutate(percentage = (price-start_price)/start_price)
    	}
	}else if (unit=="BTC"){
    	
		btc_prices = all_coins %>% filter(name=="Bitcoin") %>% 
		  filter(date >= comparison_date & date <= end_date ) %>% rename(btc_price = price) %>% select(-name)
		
		if (is.character(coins)) {
			data1 = all_coins %>% filter(name=="Bitcoin") %>% filter(date >= comparison_date & date <= end_date )
			min = data1 %>% group_by(name) %>% slice(1) %>% rename(start_price = price)
			data2 = data1 %>% full_join(min,by=c("name")) %>% rename(date = date.x) %>% select(-date.y) %>% 
				mutate(percentage = (price-start_price)/start_price)
		
    	}else if (is.numeric(coins)){
    		ranks = crypto_list(coin = unique(all_coins$name), start_date = Sys.Date()-1, Sys.Date()-1) %>% select(name,rank)
    		data1 = all_coins %>% filter(name=="Bitcoin") %>% filter(date >= comparison_date & date <= end_date )
			min = data1 %>% group_by(name) %>% slice(1) %>% rename(start_price = price)
			data2 = data1 %>% full_join(min,by=c("name")) %>% rename(date = date.x) %>% select(-date.y) %>% 
				mutate(percentage = (price-start_price)/start_price)
    	}
	}
		

    plot = ggplot(data2,aes(x=date,y=percentage, color=name)) +
    	theme_light()+
    	ggtitle(paste("% Change","in",unit))+
        scale_y_continuous("", breaks = pretty(data2$percentage, n = 20),labels=percent) + 
    	scale_x_date("",limits=c(min(data2$date),
    		max(data2$date + as.numeric((max(data2$date)-min(data2$date)))/20)), 
    		breaks= pretty(data2$date,n=20), labels=date_format("%B %d, %Y"))+
    	theme(legend.position="bottom",
    		axis.ticks.length=unit(0.25,"cm"),
    		axis.text.x = element_text(angle = 30, hjust = 1,face="bold",size=10),
    		axis.text.y=element_text(face="bold")) +
        geom_dl(aes(label = name), method = "last.points", cex = 0.3,alpha=0.7)
    
    if (is.null(line_type) || (line_type!="smooth" & line_type!="very smooth")){
    	plot +  geom_line(size=1,alpha=0.5) 
    }else if (line_type=="smooth"){
    	plot + geom_point(alpha=0.1) + stat_smooth(geom="line",method="loess",se=FALSE,span=0.2,size=1,alpha=0.5)
    }
    else if (line_type=="very smooth"){
    	plot + stat_smooth(geom="line",method="loess",se=FALSE,span=0.99,size=1,alpha=0.5)
    }
}

###############################################################################################
###############################################################################################


predict_downtrend = function(ooath.date, ooatl.date,oath.date, oatl.date, ath.date, atl.date=Sys.Date()-1, ci.level = 0.95){
load("bitcoin_history_all.RDA")  #btc.df
load("models/d.fit1.Rda")
load("models/d.fit2.Rda")
load("models/d.fit3.Rda")
load("models/d.fit4.Rda")
load("models/d.fit5.Rda")
load("models/d.fit6.Rda")
load("models/d.fit7.Rda")
load("models/d.fit8.Rda")
	
ooath.date = as.Date(ooath.date)
ooatl.date = as.Date(ooatl.date)
oath.date = as.Date(oath.date)
oatl.date = as.Date(oatl.date)
ath.date = as.Date(ath.date)
atl.date = as.Date(atl.date)

btc.df.new = getCoins(coin="Bitcoin",start_date='20180812') %>% dplyr::rename(price = close) %>% dplyr::select(date,price)
btc.df = btc.df %>% filter(date >= as.Date("2017-12-01")) %>% bind_rows(btc.df.new)
pred.df2 = data.frame(ooath.date,ooatl.date,oath.date,oatl.date,ath.date,atl.date)
a1 = btc.df %>% dplyr::rename(ooath.date = date) %>% dplyr::rename(ooath = price)
a2 = btc.df %>% dplyr::rename(ooatl.date = date) %>% dplyr::rename(ooatl = price)
a3 = btc.df %>% dplyr::rename(oath.date = date) %>% dplyr::rename(oath = price)
a4 = btc.df %>% dplyr::rename(oatl.date = date) %>% dplyr::rename(oatl = price)
a5 = btc.df %>% dplyr::rename(ath.date = date) %>% dplyr::rename(ath = price)
a6 = btc.df %>% dplyr::rename(atl.date = date) %>% dplyr::rename(atl = price)
pred.df = pred.df2 %>%
	left_join(a1,by="ooath.date") %>% 
	left_join(a2,by="ooatl.date") %>% 
	left_join(a3,by="oath.date") %>% 
	left_join(a4,by="oatl.date") %>% 
	left_join(a5,by="ath.date") %>%
	left_join(a6,by="atl.date")

# Create percent change of uptrend/downtrend
pred.df$pct.ooath.ooatl = (pred.df$ooath - pred.df$ooatl)/pred.df$ooath * 100
pred.df$pct.ooatl.oath = (pred.df$oath - pred.df$ooatl)/pred.df$ooatl * 100
pred.df$pct.oath.oatl = (pred.df$oath - pred.df$oatl)/pred.df$oath * 100
pred.df$pct.oatl.ath = (pred.df$ath - pred.df$oatl)/pred.df$oatl * 100
pred.df$pct.ooath.oath = (pred.df$oath - pred.df$ooath)/pred.df$ooath * 100
pred.df$pct.oath.ath = (pred.df$ath - pred.df$oath)/pred.df$oath * 100
pred.df$pct.ooatl.oatl = (pred.df$oatl - pred.df$ooatl)/pred.df$ooatl * 100

# Create duration of days
pred.df$days.ooath.ooatl = as.numeric(pred.df$ooatl.date - pred.df$ooath.date)
pred.df$days.ooatl.oath = as.numeric(pred.df$oath.date - pred.df$ooatl.date)
pred.df$days.oath.oatl = as.numeric(pred.df$oatl.date - pred.df$oath.date)
pred.df$days.oatl.ath = as.numeric(pred.df$ath.date - pred.df$oatl.date)
pred.df$days.ooath.oath = as.numeric(pred.df$oath.date - pred.df$ooath.date)
pred.df$days.oath.ath = as.numeric(pred.df$ath.date - pred.df$oath.date)
pred.df$days.ooatl.oatl = as.numeric(pred.df$oatl.date - pred.df$ooatl.date)

# Create percent per day
pred.df$pct.day.ooath.ooatl = pred.df$pct.ooath.ooatl/pred.df$days.ooath.ooatl
pred.df$pct.day.ooatl.oath = pred.df$pct.ooatl.oath/pred.df$days.ooatl.oath
pred.df$pct.day.oath.oatl = pred.df$pct.oath.oatl/pred.df$days.oath.oatl
pred.df$pct.day.oatl.ath = pred.df$pct.oatl.ath/pred.df$days.oatl.ath
pred.df$pct.day.ooath.oath = pred.df$pct.ooath.oath/pred.df$days.ooath.oath
pred.df$pct.day.oath.ath = pred.df$pct.oath.ath/pred.df$days.oath.ath
pred.df$pct.day.ooatl.oatl = pred.df$pct.ooatl.oatl/pred.df$days.ooatl.oatl

# Create regression
pred.df$reg.ooath.ooatl = abs(regression(start.date = pred.df$ooath.date,end.date = pred.df$ooatl.date, btc.df))
pred.df$reg.ooatl.oath = abs(regression(start.date = pred.df$ooatl.date,end.date = pred.df$oath.date,btc.df))
pred.df$reg.oath.oatl = abs(regression(start.date = pred.df$oath.date,end.date = pred.df$oatl.date,btc.df))
pred.df$reg.oatl.ath = abs(regression(start.date = pred.df$oatl.date,end.date = pred.df$ath.date,btc.df))
pred.df$reg.ooath.oath =  regression(start.date = pred.df$ooath.date,end.date = pred.df$oath.date,btc.df)
pred.df$reg.oath.ath = regression(start.date = pred.df$oath.date,end.date = pred.df$ath.date,btc.df)
pred.df$reg.ooatl.oatl = regression(start.date = pred.df$ooatl.date,end.date = pred.df$oatl.date,btc.df)
pred.df$reg.ath.atl = abs(regression(start.date = pred.df$ath.date,end.date = pred.df$atl.date,btc.df))

log.pred.df = log(abs(pred.df[,c(13:ncol(pred.df))]))
names(log.pred.df) <- paste0("log.",names(log.pred.df))
pred.df = cbind(pred.df,log.pred.df)

pred.df$bad.months = bad.months.function(pred.df$ath.date)

# Creates factor if ath is less than oath
pred.df$ath.bull = as.factor(ifelse(pred.df$ath < pred.df$oath, 0,1))



ci = as.data.frame(matrix(nrow = 8, ncol = 3))
colnames(ci) = c("fit","low","high")
ci[1,] = exp(predict(d.fit1, pred.df, se.fit=T, type='response',interval = "conf",level=ci.level)$fit)
ci[2,] = exp(predict(d.fit2, pred.df, se.fit=T, type='response',interval = "conf",level=ci.level)$fit)
ci[3,]= exp(predict(d.fit3, pred.df, se.fit=T, type='response',interval = "conf",level=ci.level)$fit)
ci[4,] = exp(predict(d.fit4, pred.df, se.fit=T, type='response',interval = "conf",level=ci.level)$fit)
ci[5,] = exp(predict(d.fit5, pred.df, se.fit=T, type='response',interval = "conf",level=ci.level)$fit)
ci[6,] = exp(predict(d.fit6, pred.df, se.fit=T, type='response',interval = "conf",level=ci.level)$fit)
ci[7,] = exp(predict(d.fit7, pred.df, se.fit=T, type='response',interval = "conf",level=ci.level)$fit)
ci[8,] = exp(predict(d.fit8, pred.df, se.fit=T, type='response',interval = "conf",level=ci.level)$fit)

# ci = ci %>% mutate(date.fit = format(ath.date+fit,"%B %d") , 
# 	date.low = format(ath.date+low,"%B %d"), 
# 	date.high = format(ath.date+high,"%B %d")) 

# output = cbind(ci%>%
# 		select(date.fit,date.low,date.high),
# 	sapply(d.stats.model %>% 
# 		mutate(adj.r.squared = adj.r.squared*100, pred.r.squared = pred.r.squared*100),round)) %>%
# 	rename('Prediction' = date.fit ) %>%
# 	mutate('95% Confidence Interval' = paste0("(",date.low,", ",date.high,")")) %>%
# 	rename('Adjusted R2' = adj.r.squared, 'Predicted R2' = pred.r.squared, 'Average Error' = mean_error, 'Median Error' = median_error) %>%
# 	select(-date.low,-date.high) %>%
# 	select(1,7,2,3,4,5,6)


    ggplot(ci)+
	geom_rect(aes(xmin=low, xmax=high,ymin=0,ymax=0.5),alpha=0.15,fill="blue",color=NA)+
	theme_classic()+
	ylab("")+
	ylim(0,1)+
	theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
	scale_x_continuous("Date that downtrend is expected to end (darkest region = highest probability)",breaks=round(seq(min(ci$low), max(ci$high),length.out=20)) ,
		labels = format(round(seq(min(ci$low), max(ci$high),length.out=20)) + ath.date, "%B %d"))+
        theme(axis.text.x = element_text(angle = 60, hjust = 1,size=12.5,face="bold"))

    #table1 = ggtexttable(output, rows = NULL, theme = ttheme("mBlue"))
    
 #ggarrange(plot1, table1, ncol = 1, nrow = 2,heights = c(0.5, 0.5))


}


###############################################################################################
###############################################################################################

predict_uptrend = function(ooath.date, ooatl.date,oath.date, oatl.date, ath.date, atl.date, nath.date=Sys.Date()-1, ci.level = 0.95){
load("bitcoin_history_all.RDA")  #btc.df
load("models/u.fit1.Rda")
load("models/u.fit2.Rda")
load("models/u.fit3.Rda")
load("models/u.fit4.Rda")
load("models/u.fit5.Rda")
load("models/u.fit6.Rda")
load("models/u.fit7.Rda")
load("models/u.fit8.Rda")
load("models/u.fit9.Rda")
#load("models/u.fit10.Rda")
	
ooath.date = as.Date(ooath.date)
ooatl.date = as.Date(ooatl.date)
oath.date = as.Date(oath.date)
oatl.date = as.Date(oatl.date)
ath.date = as.Date(ath.date)
atl.date = as.Date(atl.date)
nath.date=as.Date(nath.date)

btc.df.new = crypto_history(coin="Bitcoin",start_date='20180812') %>% dplyr::rename(price = close) %>% dplyr::select(date,price)
btc.df = btc.df %>% filter(date >= as.Date("2017-12-01")) %>% bind_rows(btc.df.new)
pred.df2 = data.frame(ooath.date,ooatl.date,oath.date,oatl.date,ath.date,atl.date,nath.date)
a1 = btc.df %>% dplyr::rename(ooath.date = date) %>% dplyr::rename(ooath = price)
a2 = btc.df %>% dplyr::rename(ooatl.date = date) %>% dplyr::rename(ooatl = price)
a3 = btc.df %>% dplyr::rename(oath.date = date) %>% dplyr::rename(oath = price)
a4 = btc.df %>% dplyr::rename(oatl.date = date) %>% dplyr::rename(oatl = price)
a5 = btc.df %>% dplyr::rename(ath.date = date) %>% dplyr::rename(ath = price)
a6 = btc.df %>% dplyr::rename(atl.date = date) %>% dplyr::rename(atl = price)
a7 = btc.df %>%dplyr::rename(nath.date = date) %>% dplyr::rename(nath = price)
pred.df = pred.df2 %>%
	left_join(a1,by="ooath.date") %>% 
	left_join(a2,by="ooatl.date") %>% 
	left_join(a3,by="oath.date") %>% 
	left_join(a4,by="oatl.date") %>% 
	left_join(a5,by="ath.date") %>%
	left_join(a6,by="atl.date") %>%
	left_join(a7,by="nath.date") %>%
	filter(row_number()==1)


# Create percent change of uptrend/downtrend
pred.df$pct.ooath.ooatl = (pred.df$ooath - pred.df$ooatl)/pred.df$ooath * 100
pred.df$pct.ooatl.oath = (pred.df$oath - pred.df$ooatl)/pred.df$ooatl * 100
pred.df$pct.oath.oatl = (pred.df$oath - pred.df$oatl)/pred.df$oath * 100
pred.df$pct.oatl.ath = (pred.df$ath - pred.df$oatl)/pred.df$oatl * 100
pred.df$pct.ooath.oath = (pred.df$oath - pred.df$ooath)/pred.df$ooath * 100
pred.df$pct.oath.ath = (pred.df$ath - pred.df$oath)/pred.df$oath * 100
pred.df$pct.ooatl.oatl = (pred.df$oatl - pred.df$ooatl)/pred.df$ooatl * 100
pred.df$pct.ath.atl = (pred.df$ath - pred.df$atl)/pred.df$ath * 100
pred.df$pct.ath.nath = (pred.df$nath - pred.df$ath)/pred.df$ath * 100
pred.df$pct.oatl.atl = (pred.df$atl - pred.df$oatl)/pred.df$oatl * 100

# Create duration of days
pred.df$days.ooath.ooatl = as.numeric(pred.df$ooatl.date - pred.df$ooath.date)
pred.df$days.ooatl.oath = as.numeric(pred.df$oath.date - pred.df$ooatl.date)
pred.df$days.oath.oatl = as.numeric(pred.df$oatl.date - pred.df$oath.date)
pred.df$days.oatl.ath = as.numeric(pred.df$ath.date - pred.df$oatl.date)
pred.df$days.ooath.oath = as.numeric(pred.df$oath.date - pred.df$ooath.date)
pred.df$days.oath.ath = as.numeric(pred.df$ath.date - pred.df$oath.date)
pred.df$days.ooatl.oatl = as.numeric(pred.df$oatl.date - pred.df$ooatl.date)
pred.df$days.ath.atl = as.numeric(pred.df$atl.date - pred.df$ath.date)
pred.df$days.oatl.atl = as.numeric(pred.df$atl.date - pred.df$oatl.date)

# Create percent per day
pred.df$pct.day.ooath.ooatl = pred.df$pct.ooath.ooatl/pred.df$days.ooath.ooatl
pred.df$pct.day.ooatl.oath = pred.df$pct.ooatl.oath/pred.df$days.ooatl.oath
pred.df$pct.day.oath.oatl = pred.df$pct.oath.oatl/pred.df$days.oath.oatl
pred.df$pct.day.oatl.ath = pred.df$pct.oatl.ath/pred.df$days.oatl.ath
pred.df$pct.day.ooath.oath = pred.df$pct.ooath.oath/pred.df$days.ooath.oath
pred.df$pct.day.oath.ath = pred.df$pct.oath.ath/pred.df$days.oath.ath
pred.df$pct.day.ooatl.oatl = pred.df$pct.ooatl.oatl/pred.df$days.ooatl.oatl
pred.df$pct.day.ath.atl = pred.df$pct.ath.atl/pred.df$days.ath.atl
pred.df$pct.day.oatl.atl = pred.df$pct.oatl.atl/pred.df$days.oatl.atl

# Create regression
pred.df$reg.ooath.ooatl = abs(regression(start.date = pred.df$ooath.date,end.date = pred.df$ooatl.date, btc.df))
pred.df$reg.ooatl.oath = abs(regression(start.date = pred.df$ooatl.date,end.date = pred.df$oath.date,btc.df))
pred.df$reg.oath.oatl = abs(regression(start.date = pred.df$oath.date,end.date = pred.df$oatl.date,btc.df))
pred.df$reg.oatl.ath = abs(regression(start.date = pred.df$oatl.date,end.date = pred.df$ath.date,btc.df))
pred.df$reg.ooath.oath =  regression(start.date = pred.df$ooath.date,end.date = pred.df$oath.date,btc.df)
pred.df$reg.oath.ath = regression(start.date = pred.df$oath.date,end.date = pred.df$ath.date,btc.df)
pred.df$reg.ooatl.oatl = regression(start.date = pred.df$ooatl.date,end.date = pred.df$oatl.date,btc.df)
pred.df$reg.ath.atl = abs(regression(start.date = pred.df$ath.date,end.date = pred.df$atl.date,btc.df))
pred.df$reg.oatl.atl = regression(start.date = pred.df$oatl.date,end.date = pred.df$atl.date,btc.df)
pred.df$reg.atl.nath =  abs(regression(start.date = pred.df$atl.date,end.date = pred.df$nath.date,btc.df))

log.pred.df = log(abs(pred.df[,c(15:ncol(pred.df))]))
names(log.pred.df) <- paste0("log.",names(log.pred.df))
pred.df = cbind(pred.df,log.pred.df)

pred.df$bad.months = bad.months.function(pred.df$ath.date)

# Creates factor if ath is less than oath
pred.df$ath.bull = as.factor(ifelse(pred.df$ath < pred.df$oath, 0,1))
pred.df$atl.bull = as.factor(ifelse(pred.df$atl < pred.df$oatl, 0,1))


ci = as.data.frame(matrix(nrow = 8, ncol = 3))
colnames(ci) = c("fit","low","high")
ci[1,] = predict(u.fit1, pred.df, se.fit=T, type='response',interval = "conf",level=ci.level)$fit
ci[2,] = predict(u.fit2, pred.df, se.fit=T, type='response',interval = "conf",level=ci.level)$fit
ci[3,]= predict(u.fit3, pred.df, se.fit=T, type='response',interval = "conf",level=ci.level)$fit
ci[4,] = predict(u.fit4, pred.df, se.fit=T, type='response',interval = "conf",level=ci.level)$fit
ci[5,] = predict(u.fit5, pred.df, se.fit=T, type='response',interval = "conf",level=ci.level)$fit
ci[6,] = exp(predict(u.fit6, pred.df, se.fit=T, type='response',interval = "conf",level=ci.level)$fit)
ci[7,] = exp(predict(u.fit7, pred.df, se.fit=T, type='response',interval = "conf",level=ci.level)$fit)
ci[8,] = exp(predict(u.fit8, pred.df, se.fit=T, type='response',interval = "conf",level=ci.level)$fit)
#ci[9,] = predict(u.fit9, pred.df, se.fit=T, type='response',interval = "conf",level=0.95)$fit
#ci[10,] = predict(u.fit10, pred.df, se.fit=T, type='response',interval = "conf",level=0.95)$fit


    ggplot(ci)+
	geom_rect(aes(xmin=low, xmax=high,ymin=0,ymax=0.5),alpha=0.15,fill="red",color=NA)+
	theme_classic()+
	ylab("")+
	ylim(0,1)+
	theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
	scale_x_continuous("Date that uptrend is expected to end (darkest region = highest probability)",breaks=round(seq(min(ci$low), max(ci$high),length.out=20)) ,
		labels = format(round(seq(min(ci$low), max(ci$high),length.out=20)) + atl.date, "%B %d"))+
        theme(axis.text.x = element_text(angle = 60, hjust = 1,size=12.5,face="bold"))
}




