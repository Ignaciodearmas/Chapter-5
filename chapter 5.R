library(fpp2)
library(ggplot2)
library(forecast)
## 5.1 A. 
daily20 <- head(elecdaily,20)
plot(elecdaily)
reg_Demand <- tslm(Demand ~ Temperature, data = daily20)
reg_Demand

daily20 %>%
  as.data.frame() %>%
  ggplot(aes(x=Temperature, y=Demand)) +
  ylab("Demand") +
  xlab("Temperature") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

## there appears to be a positve relationship between the demand for electricity
## and the temperature. Meaning that as the temperature rises the demand for
## electricity also rises, this may be due to the use of air conditioning or refuge from the heat. 

## B. 
checkresiduals(reg_Demand)

## the residuals seem to be uncorrelated which is a positve sign. However, there seems to be some outliers in the right hand side of the bell curve meaning that there were a couple of extremely high temperature days.

## C.
New_Forecast <- forecast(reg_Demand, newdata = data.frame(Temperature = c(15,35)))
New_Forecast

## I believe that this forecast is faily accurate, as they electricity demanded is correlates with previous points in the regression the forecasted electricity demand for 15 is 140 and for 35 is 275.

## D. 
New_data <- data.frame(Temperature = 25)
Prediction1 = predict(reg_Demand, newdata = New_data, interval = "prediction")

## we can say with 95% accuray that the demand of electricity at 25 degrees will be between 160 and 255 

##E. 
elecdaily %>%
  as.data.frame() %>%
  ggplot(aes(x=Temperature, y=Demand)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
## viewing the full dataset tells us that the our study with 20 observations was skewed and not entirely accurate. 

## 5.2 A.
plot(mens400)
## the plot tells us that the winnig times have decrease greatly over time, we can see that the observations start at 1900 and end around 2020. there is also some missing data between around the 1920s and the 1940s.

## B.
mens400
men_time <- time(mens400)
tsmens <- tslm(mens400 ~ men_time, data = mens400)
autoplot(mens400) + geom_abline(slope = tsmens$coefficients[2],
                                intercept = tsmens$coefficients[1])
tsmens$coefficients[2]
## the average rate yearly rate of decrease in time is 0.064 seconds.

## C.
cbind(Time = men_time,
      Residuals= tsmens$residuals) %>%
  as.data.frame() %>%
  ggplot(aes(x=Time, y=Residuals)) + geom_point() 

## D.
lmMens <- lm(mens400 ~ men_time, data = mens400, na.action = na.exclude)
New_year <- data.frame(men_time = 2020)
Prediction2 = predict(lmMens, newdata = New_year, interval = "prediction")

## I am 95% confident that for the 2020 Olympics the winner of the race will finish it between 39 and 42 seconds. The assumptions that I made where the missing values do not affect the data and that the residuals were normaly distributed.

## 5.3
easter(ausbeer)

## I can see a quarterly divided data from 1956 to the second quarter of 2010.

## 5.5 A.
plot(fancy)

## the plot seems to behave seasonlly and shows an exponential growth over the years especifically around 1993 - 1994.

## B.

## Logarithms are necessary when the data is heavily skewed in an attempt to normalize the data and produce more accurate results.

## C. 


## 5.6 A.

## 5.7 A.

plot(huron)

## in the plot we can observe some seassonal movements with very sharp increases and decreases.

## B. 

K <- 8 
ts_huron <- tslm(huron ~ trend)
fc_huron <- forecast(ts_huron, h=K)

