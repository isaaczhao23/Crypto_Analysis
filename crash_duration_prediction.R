library(ggpubr)
library(ggplot2)
library(ggthemes)
library(sjPlot)

# Input data from chart
days_declining = c(4,13,35,14,410,88,162,8,53,4,24)
percent_decline = c(30,41,39,33,85,76,94,34,49,72,94)
days_recovering = c(8,40,55,48,1181,211,631,12,66,86,40)

# Create a dataframe from the data.
# days_declining = total days since start of crash
crash_history_df = data.frame(days_declining, percent_decline, days_recovering) %>%
# Create column that calculates rate of decline as a new variable
mutate(decline_per_day = percent_decline/days_declining) %>% arrange(decline_per_day)
# Creates a log of outcome variable of interest
crash_history_df$log_days_declining = log(crash_history_df$days_declining)

# Creates a linear scatter plot (not a very good fit)
ggscatter(crash_history_df, x = "decline_per_day", y= "log_days_declining", add = c("reg.line"), conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson")

# See statistical analysis of linear model
fit = lm(log_days_declining ~ decline_per_day, data = crash_history_df)
sjt.lm(fit,show.fstat=TRUE)


# Creates lowess curve function (much better fit)
# current_decline_per_day is the total % decrease since all time high divided by the total number of days between all time high and today
    
current_decline_per_day = 56/49
    
ggplot(crash_history_df, aes(x=decline_per_day, y=log_days_declining)) +
    geom_point(size=3) +
    geom_smooth(method="loess", span = 1, fill="darkorange1", color = "orange3", alpha = 0.45) +
        geom_smooth(method="loess", span = 3, fill="palegreen2", color = "green", alpha = 0.45) +
    geom_vline( xintercept = 56/49, color="red") +
    scale_y_continuous(breaks=seq(0,6,by=0.25)) +
    scale_x_continuous(breaks=seq(0,25,by=0.25), limits = c(max(0, current_decline_per_day - 3.5), round(current_decline_per_day + 3.5))) +
    ggtitle("End of Crash Prediction w/ Various Loess Spans") +
    theme_economist() + scale_colour_economist() +
    xlab("Average % Decline Per Day from All Time High") +
    ylab("Log of Total # of Days Crash Lasted") +
    geom_label(x = round(current_decline_per_day + 2), y = max(crash_history_df$log_days_declining), label = "Orange Model = More Conservative")
    

