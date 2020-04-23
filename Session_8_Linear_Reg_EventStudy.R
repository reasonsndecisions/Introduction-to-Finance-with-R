## -------------------------------------------------------------------------------------------------
library(tidyverse)


## -------------------------------------------------------------------------------------------------
volks <- read_csv("volks.csv")


## -------------------------------------------------------------------------------------------------
dax <- read_csv("GDAXI.csv")


## -------------------------------------------------------------------------------------------------
volks <- left_join(volks, dax, by = "date")


## -------------------------------------------------------------------------------------------------
volksfig <- ggplot(volks) + 
  geom_line(aes(date, VOW.DE), color = "steelblue") 

daxfig <- ggplot(volks) + 
  geom_line(aes(date, GDAXI.Adjusted), color = "thistle4") 

library(gridExtra)

grid.arrange(volksfig, daxfig)


## -------------------------------------------------------------------------------------------------
# compute return

volks <- volks %>%
  mutate(volksRt = log(VOW.DE/(lag(VOW.DE))),
         daxRt = log(GDAXI.Adjusted/(lag(GDAXI.Adjusted)))) %>%
  drop_na() %>%
  select(date, volksRt, daxRt)



## -------------------------------------------------------------------------------------------------
# Create estimation window 

est_window <- volks %>%
  filter(date <= "2015-08-06" & date >= "2014-08-04")



## -------------------------------------------------------------------------------------------------
# Scatterplot

volksscat <- ggplot(volks) +
  geom_point(aes(daxRt, volksRt), color = "steelblue", alpha = 0.5) + 
  xlab("Market returns") + 
  ylab("Volkswagen returns") + 
  geom_abline(intercept = -0.000478, slope = 1.08528, color = "blue") +
  geom_abline(intercept = -0.000562, slope = 0.918528, color = "violet") +
  geom_abline(intercept = -0.000502, slope = 1.518528, color = "lavenderblush4") 

volksscat


## -------------------------------------------------------------------------------------------------
# Estimating beta

market_model <- lm(volksRt ~ daxRt, data = est_window)



## -------------------------------------------------------------------------------------------------
summary(market_model)


## -------------------------------------------------------------------------------------------------
# Create Estimation and Event Window 

event_window <-  volks %>%
  filter(date >= "2015-08-13" & date <= "2015-09-24")


## -------------------------------------------------------------------------------------------------
# Calculate Abnormal Returns and Cumulative Abnormal Returns duing the event window  

ar <- event_window %>%
  mutate(ar = -0.0005 + 1.085*daxRt,
         t = ar/0.009799,
         car = cumsum(ar))



## -------------------------------------------------------------------------------------------------
# Plot Abnormal and Cumulative Abnormal Returns

ggplot(ar, aes(date, ar)) + geom_line() +
  geom_line(aes(date, car), color = "steelblue") 



## -------------------------------------------------------------------------------------------------
# Calculate test statistics for Cumulative Abnormal Returns  

ar$car[ar$date == "2015-09-24"]/((31*var(ar$ar))^(0.5)) 




## -------------------------------------------------------------------------------------------------
apple <- read_csv("apple.csv")

nasdaq <- read_csv("nasdaq.csv")

apple <- left_join(apple, nasdaq, by = "date")



## -------------------------------------------------------------------------------------------------
applefig <- ggplot(apple) + 
  geom_line(aes(date, AAPL), color = "steelblue") 

nasdaqfig <- ggplot(apple) + 
  geom_line(aes(date, NDX.Adjusted), color = "thistle4") 

library(gridExtra)

grid.arrange(applefig, nasdaqfig)


## -------------------------------------------------------------------------------------------------
# compute return

apple <- apple %>%
  mutate(appleRt = log(AAPL/(lag(AAPL))),
         nasdaqRt = log(NDX.Adjusted/(lag(NDX.Adjusted)))) %>%
  drop_na() %>%
  select(date, appleRt, nasdaqRt)



## -------------------------------------------------------------------------------------------------
# Create Estimation and Event window 

event_window <-  apple %>%
  filter(date >= "2019-10-16" & date <= "2019-11-27")

est_window <- apple %>%
  filter(date <= "2019-10-15" & date >= "2018-10-13")



## -------------------------------------------------------------------------------------------------
# Estimating beta

market_model <- lm(appleRt ~ nasdaqRt, data = est_window)

summary(market_model)


## -------------------------------------------------------------------------------------------------

# Calculate Abnormal Returns and Cumulative Abnormal Returns duing the event window  

ar <- event_window %>%
  mutate(ar = -0.0002 + 1.2156*nasdaqRt,
         t = ar/0.01098,
         car = cumsum(ar))

# Calculate test statistics for Cumulative Abnormal Returns  

ar$car[ar$date == "2019-11-27"]/((31*var(ar$ar))^(0.5)) 



## -------------------------------------------------------------------------------------------------
# Plot Abnormal and Cumulative Abnormal Returns

ggplot(ar, aes(date, ar)) + geom_line() +
  geom_line(aes(date, car), color = "steelblue") 


