source("check_packages.R")
check_packages(c("dplyr","plyr", "scales", "sjPlot"))
load("coin_list.Rda")


###############################################################################################
###############################################################################################

percentage = function(x){
    
        output =  paste(as.character(comma(round(x*100,0))), "%", sep=""  )
    return(output) 
}


###############################################################################################
###############################################################################################



###############################################################################################
###############################################################################################


generate_comparison <- function(comparison_date, end_date, month_or_day = "day", interval = 1, type = "marketcap"){
    
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
            select(-date, -ranknow, -symbol)
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
    
    price_history_df = price_history_df %>% select(-contains("price")) %>% select(-2)     # Removes prices and today's date
    
    
    date_seq_name = format(date_seq[2:ncol(price_history_df)], format="%B %d, %Y")
    #### Creates dataframe for sorting by marketcap with no percent
    nopercent_history_df = price_history_df
    for(i in 2: (ncol(price_history_df))){
        nopercent_history_df[,i] = price_history_df[,i]
        nopercent_history_df[,i][grep("NA%", nopercent_history_df[,i])] <- ""
    }
    colnames(nopercent_history_df) = c("Name",date_seq_name)
    
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
    
    return(output)
}

###############################################################################################
###############################################################################################



