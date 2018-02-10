library(ggpubr)
library(ggplot2)
library(ggthemes)
library(sjPlot)

# Input data from chart
x = c(4,13,35,14,410,88,162,8,53,4,24)
y = c(30,41,39,33,85,76,94,34,49,72,94)
z = c(8,40,55,48,1181,211,631,12,66,86,40)

# Create a dataframe from the data
df = data.frame(days_declining = x, percent_decline = y, days_recovering = z)
# Create column that calculates rate of decline as a new variable
df2 = mutate(df, decline.per.day = percent_decline/days_declining)
# Creates a log of outcome variable of interest
df2$log_days_declining = log(df2$days_declining)

# Creates a linear scatter plot (not a very good fit)
ggscatter(df2, x = "decline.per.day", y= "log_days_declining", add = c("reg.line"), conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson")
# See statistical analysis of linear model
fit = lm(log_days_declining ~ decline.per.day, data = df2)
summary(fit)
sjt.lm(fit,show.fstat=TRUE)


# Creates lowess curve (much better fit)
ggplot(df2, aes(x=decline.per.day,y=log_days_declining)) +
    geom_point() +
    geom_smooth(method="loess",span=1) +
    geom_vline( xintercept = 56/49, color="red") +
    scale_y_continuous(breaks=seq(0,6,by=0.5)) +
    scale_x_continuous(breaks=seq(0,25,by=1)) +
    ggtitle("Predicting how many days BTC downtrend takes based on average rate of decline") +
    theme_economist() + scale_colour_economist() 
    
