path = getwd()
setwd(paste0(path,'\\Github\\airbnb-data\\'))
library(data.table)
#may_listings = as.data.table(read.csv('listings.csv'))
may_reviews = as.data.table(read.csv('reviews.csv'))
#may_calendar = as.data.table(read.csv('calendar.csv'))

may_reviews$date = lubridate::ymd(may_reviews$date)

april_reviews = as.data.table(read.csv('april-reviews.csv'))
april_reviews$date = lubridate::ymd(april_reviews$date)

#may_calendar[listing_id == 10564262,]

library(tidyverse)

#tail(may_reviews %>% group_by(date) %>% summarise(counts = n()),10)

april_reviews_grouped = april_reviews %>% group_by(date) %>% summarise(counts = n())
may_reviews_grouped = may_reviews %>% group_by(date) %>% summarise(counts = n())


library(ggplot2)
p = ggplot(data = may_reviews_grouped, aes(x = date, y = counts))+
  geom_line()
print(p)

library(plotly)
ggplotly(p)

forecasted_dates = data.table(date = seq(tail(april_reviews_grouped$date,1),
                       tail(april_reviews_grouped$date,1) + lubridate::days(30),
                       by = 'days'))

arima_model = arima(april_reviews_grouped$counts, order = c(1,0,0))
AIC(arima_model)

library(forecast)
auto_arima_model = auto.arima(april_reviews_grouped$counts)
AIC(auto_arima_model)
forecasted_dates$counts = predict(auto_arima_model, 31)[1]

april_reviews_grouped = data.table(april_reviews_grouped)
may_reviews_grouped = data.table(may_reviews_grouped)
forecasted_dates = data.table(forecasted_dates)
forecasted_dates$counts = as.numeric(forecasted_dates$counts)


p = ggplot()+
  geom_line(data = may_reviews_grouped, aes(x = date, y = counts), color = "black")+
  geom_line(data = forecasted_dates, aes(x = date, y = counts), color = "red")
ggplotly(p)

#lets make the data stationary and do log transformation
april_reviews_grouped$log_counts = log(april_reviews_grouped$counts)
may_reviews_grouped$log_counts = log(may_reviews_grouped$counts)
auto_arima_model_log = auto.arima(april_reviews_grouped$log_counts)
forecasted_dates$counts_log = as.numeric(predict(auto_arima_model_log, 31)[[1]])

q = ggplot()+
  geom_line(data = may_reviews_grouped, aes(x = date, y = log_counts), color = "black")+
  geom_line(data = forecasted_dates, aes(x = date, y = counts_log), color = "red")
ggplotly(q)


# lets diff the data
april_reviews_grouped$diff_counts = diff(april_reviews_grouped$counts, lag = 1)
may_reviews_grouped$diff_counts = diff(may_reviews_grouped$counts, lag = 1)
auto_arima_model_diff = auto.arima(april_reviews_grouped$diff_counts)
forecasted_dates$counts_diff = as.numeric(predict(auto_arima_model_diff, 31)[[1]])

r = ggplot()+
  geom_line(data = may_reviews_grouped, aes(x = date, y = diff_counts), color = "black")+
  geom_line(data = forecasted_dates, aes(x = date, y = counts_diff), color = "red")
ggplotly(r)


# lets log-diff the data
april_reviews_grouped$log_diff_counts = diff(april_reviews_grouped$log_counts, lag = 1)
may_reviews_grouped$log_diff_counts = diff(may_reviews_grouped$log_counts, lag = 1)
auto_arima_model_log_diff = auto.arima(april_reviews_grouped$log_diff_counts)
forecasted_dates$counts_log_diff = as.numeric(predict(auto_arima_model_log_diff, 31)[[1]])

s = ggplot()+
  geom_line(data = may_reviews_grouped, aes(x = date, y = log_diff_counts), color = "black")+
  geom_line(data = forecasted_dates, aes(x = date, y = counts_log_diff), color = "red")
ggplotly(s)

# Lets try arima
arima_model_log_diff = arima(april_reviews_grouped$log_diff_counts, c(1,1,1), seasonal =list(order =  c(3,2,1), period = 7))
forecasted_dates$counts_log_diff = as.numeric(predict(arima_model_log_diff, 31)[[1]])

t = ggplot()+
  geom_line(data = may_reviews_grouped, aes(x = date, y = log_diff_counts), color = "black")+
  geom_line(data = forecasted_dates, aes(x = date, y = counts_log_diff), color = "red")
ggplotly(t)

arima_model_log_diff %>% forecast(h=12) %>% autoplot()


#lets try sarima from astsa package
library(astsa)
sarima_0_1_1_log_diff = sarima(april_reviews_grouped$log_diff_counts, 0, 1,1)
forecasted_dates$counts_log_diff = as.numeric(predict(sarima_0_1_1_log_diff, 31)[[1]])
sarima_prediction_plot = sarima.for(diff(april_reviews_grouped$counts),31,2,0,5, S = 7)
print(sarima_prediction_plot)
forecasted_dates$sarima = sarima_prediction_plot[[1]]
u = ggplot()+
  geom_line(data = may_reviews_grouped, aes(x = date, y = diff_counts), color = "black")+
  geom_line(data = forecasted_dates, aes(x = date, y = sarima), color = "red")
ggplotly(u)

# autoarima again
auto_arima_model_log_diff = auto.arima(april_reviews_grouped$log_diff_counts)
forecasted_dates$counts_log_diff_auto_arima = as.numeric(predict(auto_arima_model_log_diff, 31)[[1]])

v = ggplot()+
  geom_line(data = may_reviews_grouped, aes(x = date, y = log_diff_counts), color = "black")+
  geom_line(data = forecasted_dates, aes(x = date, y = counts_log_diff), color = "red")
ggplotly(v)

# lets try gamlss package
library(gamlss)
april_reviews_grouped$date_numeric = as.numeric(april_reviews_grouped$date)
m0 = gamlss::gamlss(date_numeric~pb(counts), data = april_reviews_grouped, family = NO)
m2<-gamlss(date_numeric~cs(counts,df=10), data=april_reviews_grouped, family=NO)
m3<-gamlss(date_numeric~pb(counts), sigma.formula = ~pb(counts),data=april_reviews_grouped, family=NO)
m4<-gamlss(date_numeric~pb(log_diff_counts), sigma.formula = ~pb(counts),nu.formula = ~pb(counts),data=april_reviews_grouped, family=BCCG)
