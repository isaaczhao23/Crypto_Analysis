# How to use this program:
 1) This program uses R code. You must have R and RStudio installed (free download).
 2) Download the zip file from this Github page. Unzip and put the folder in your Desktop. Make sure the folder is named "Crypto_Analysis-master".
 3) Open a.Rmd file depending on your objective. Click the green arrow in the top right of each code chunk to run the codes.
 4) You may have to change the directory format if you are on a PC instead of a mac for line 4: setwd(.....)

# Current things this analysis can do:
 ### 1) Compare percent change of multiple coins on the same graph/table. (Comparison_Graph.Rmd)
 comparison_graph(comparison_date="2018-01-01", coins = c("Bitcoin","Litecoin","XRP"))
![screen shot 2018-10-01 at 1 25 13 am](https://user-images.githubusercontent.com/30127730/46271105-fa0f6500-c518-11e8-873f-0f943b5f8738.png)

comparison_graph(comparison_date="2017-10-05", coins = 2:6 , line_type="smooth")
![screen shot 2018-10-01 at 1 25 43 am](https://user-images.githubusercontent.com/30127730/46271107-fb409200-c518-11e8-9f30-93dee470c792.png)


 ### 2) Predict the date when the current downtrend or uptrend for Bitcoin will end to buy and sell at the right time. (Predictions.Rmd)
 ![screen shot 2018-09-01 at 2 58 23 pm](https://user-images.githubusercontent.com/30127730/44949062-b7fae280-adf7-11e8-9841-51bc1ccab678.png)
 
 ![screen shot 2018-09-01 at 1 11 55 pm](https://user-images.githubusercontent.com/30127730/44948277-d73e4380-ade8-11e8-9699-71269a56438a.png)
