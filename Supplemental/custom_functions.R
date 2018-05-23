source("check_packages.R")
check_packages(c("MASS","plyr","RcmdrMisc", "tidyverse", "tidyr","scales", "crypto", "ggthemes", "directlabels", "shiny","ggpubr", "ggrepel","readxl","gridExtra","sjPlot","lubridate"))

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
            coin.list = getCoins(limit= topranks, start_date = comparison_date1, end_date = end_date1)  %>%  dplyr::select(name,date,close) %>% dplyr::rename(price = close)
    } else {
            coin.list = getCoins(limit= topranks, start_date = end_date1, end_date = comparison_date1) %>%   dplyr::select(name,date,close) %>% dplyr::rename(price = close)
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


comparison_graph = function(comparison_date = "2018-02-06", end_date = Sys.Date(), filter_name=NULL, filter_rank = NULL) {
    
    if (comparison_date <= end_date){
        comparison_date1 = comparison_date
        end_date1 = end_date
    } else {
        comparison_date1 = end_date
        end_date1 = comparison_date
    }
    output = comparison_chart(comparison_date1, end_date1, month_or_day = "day", interval=1, type="nopercent", view_chart=FALSE)
    
    c = as.character(as.Date(end_date)+1)
    
    output1 = cbind(rank = c(1:nrow(output)), output) %>% 
        cbind(a= rep(0,nrow(output))) %>% dplyr::rename(!!comparison_date:=a) %>% 
        cbind(b= rep(NA,nrow(output))) %>% dplyr::rename(!!as.character(as.Date(end_date)+1):=b) %>% 
        gather(date, change, -name, -rank) %>% mutate(date = as.Date(date)) %>% arrange(date,rank)
    
    if (!is.null(filter_name) && is.null(filter_rank)) {
    plotdf1 = output1 %>% filter(name %in% filter_name)
    } else if (!is.null(filter_rank) && is.null(filter_name)){
        plotdf1 = output1 %>% filter(rank %in% filter_rank)
    } else {
        plotdf1 = output1 %>% filter(rank %in% c(1:5))
    }
        
    
    
    ggplot(plotdf1,aes(x=date,y=change, color=name)) +
        theme_economist()+
        scale_y_continuous("Percent Change", labels=percent) + 
    	geom_line() +
        ggtitle(paste("Percent Change of Cryptocurrency Prices Compared to the Prices of", format(comparison_date, format="%B %d, %Y"))) +
        #theme(legend.position="none") +
        geom_dl(aes(label = name), method = "last.points", cex = 1)
}

###############################################################################################
###############################################################################################

predict_correction = function(ooath.date, ooatl.date,oath.date, oatl.date, ath.date, atl.date = Sys.Date()-1){
	
ooath.date = "2018-01-06"
ooatl.date = "2018-02-05"
oath.date = "2018-03-04"
oatl.date = "2018-04-06"
ath.date = "2018-05-05"
atl.date = Sys.Date()
	
	
ooath.date = as.Date(ooath.date)
ooatl.date = as.Date(ooatl.date)
oath.date = as.Date(oath.date)
oatl.date = as.Date(oatl.date)
ath.date = as.Date(ath.date)
atl.date = as.Date(atl.date)

load("downtrend_model2.Rda")
btc.df = getCoins(coin="Bitcoin",start_date='20180101') %>% dplyr::rename(price = close) %>% dplyr::select(date,price)

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
pred.df$reg.ath.atl = abs(regression(start.date = pred.df$ath.date,end.date = pred.df$atl.date,btc.df))
pred.df$reg.ooath.oath =  regression(start.date = pred.df$ooath.date,end.date = pred.df$oath.date,btc.df)
pred.df$reg.oath.ath = regression(start.date = pred.df$oath.date,end.date = pred.df$ath.date,btc.df)
pred.df$reg.ooatl.oatl = regression(start.date = pred.df$ooatl.date,end.date = pred.df$oatl.date,btc.df)

pred.df$bad.months = bad.months.function(pred.df$ath.date)


# Creates factor if ath is less than oath
pred.df$ath.bull = as.factor(ifelse(pred.df$ath < pred.df$oath, 0,1))


fit1_conf_ci = exp(predict(downtrend_model2, pred.df, se.fit=T, type='response',interval = "conf",level=0.68)$fit)
fit1_conf_ci_low = as.Date(ath.date) + fit1_conf_ci[2] 
fit1_conf_ci_mean = as.Date(ath.date) + fit1_conf_ci[1]
fit1_conf_ci_high = as.Date(ath.date) + fit1_conf_ci[3]

print(paste("Downtrend will end somewhere between", format(fit1_conf_ci_low,"%B %d, %Y"), 
	"and", format(fit1_conf_ci_high,"%B %d, %Y"), "with an expected date of", format(fit1_conf_ci_mean,"%B %d, %Y"),
	", which is",round(fit1_conf_ci[1]),"days total since the beginning of the downtrend."))
	
print(paste0("This downtrend drops about $", round(pred.df$ath*pred.df$reg.ath.atl/100), " per day."))
}