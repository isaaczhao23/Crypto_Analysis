df = coin.list %>% filter(name=="Bitcoin")


start_date ="2017-01-01"
end_date = "2018-02-18"
interval = 1

date_seq = seq(from = as.Date(start_date), to = as.Date(end_date), 
               by=paste("+", as.character(interval), " ", "day", sep=""))
datalist = list()
index =1

for (i in 1:length(date_seq)){
  for (j in 1:length(date_seq)){
    
    print( paste(index,"/",length(date_seq)^2))
    fromPrice <- df%>%filter(date==as.character(date_seq[j]))%>%.$price
    toPrice <- df%>%filter(date==as.character(date_seq[i]))%>%.$price
    
    # ... make some data
    dat <- data.frame(index=index,
                      fromDate=date_seq[j],
                      toDate=date_seq[i],
                      fromPrice=fromPrice[1],
                      toPrice=toPrice[1]
    )%>%
      mutate(
        change=fromPrice-toPrice,
        perc_change =((fromPrice-toPrice)/fromPrice)*100,
        num_days=as.Date(toDate)-as.Date(fromDate)
      )
    datalist[[index]] <- dat # add it to your list
    index =index+1
    
  }
}
df.change <- dplyr::bind_rows(datalist)%>%
  filter(num_days>=0)%>%
  filter(perc_change<=-30)%>%
  arrange(fromDate)
head(df.change)


