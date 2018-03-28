source("check_packages.R")
check_packages(c("plyr","tidyverse", "scales", "crypto", "ggthemes", "directlabels", "shiny","ggpubr", "ggrepel","readxl","gridExtra","MASS","sjPlot"))

###############################################################################################
###############################################################################################

percentage = function(x){
    
        output =  paste(as.character(comma(round(x*100,0))), "%", sep=""  )
    return(output) 
}


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


