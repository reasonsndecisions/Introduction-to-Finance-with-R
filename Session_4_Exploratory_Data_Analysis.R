## ----message=FALSE, warning=FALSE-------------------------------------------------------------------------
# Load the package `tidyverse` that includes ggplot2
library(tidyverse)
library(gridExtra)


## ---------------------------------------------------------------------------------------------------------

# Create a column Rt, which is net return from -0.5 to 0.5.

# We have 500 observations of net returns

df1 <- tibble(Rt = seq(from = -0.5, to = 0.5, length.out = 500))

# Use mutate() to create logRt = log(1+Rt)

df1 <- df1 %>% 
  mutate(logRt  = log(1+Rt))




## ---------------------------------------------------------------------------------------------------------
head(df1, 5)


## ---------------------------------------------------------------------------------------------------------

fig1 <- ggplot(data = df1) +
  geom_point(aes(x = Rt, y = Rt), size = 0.1, color = "red")


  



## ---------------------------------------------------------------------------------------------------------
fig1


## ---------------------------------------------------------------------------------------------------------

fig1 <- fig1 +
  geom_point(aes(x = Rt, y = logRt), size = 0.1, color = "steelblue") +
  geom_vline(xintercept = 0, color = "thistle3")



  
  


## ---------------------------------------------------------------------------------------------------------
fig1


## ---------------------------------------------------------------------------------------------------------

fig1 <- fig1 +
  labs(title = "Log Returns Approximate Net Returns Around 0", 
       y = "Net Return and Log Return", 
       x = "Net Return")
  

fig1


## ---------------------------------------------------------------------------------------------------------

fig1 <- fig1 +
  labs(title = "Log Returns Approximate Net Returns Around 0", 
       y = "Net Return and Log Return", 
       x = "Net Return")
  

fig1


## ---------------------------------------------------------------------------------------------------------

# Your code - Carefully look at the code below

# You will need to remove # and modify "???"

# Hints below

# Create a tibble 

# df2 <- tibble(W = seq(???, ???, length.out = 500))

# Create new columns, for Jack's and Jill's utility function 


# df2 <- df2 %>% 
#     mutate(Jack = W^(1 - ???)/(1 - ???), 
#     Jill = W^(1 - ???)/(1 - ???))

# Create ggplot object

# ggplot(df2) + 
#  geom_point(aes(x = W, y = ???), color = "red", size = 0.1) + 
#  geom_point(aes(x = W, y = ???), color = "steelblue", size = 0.1) +
#  labs(x = "Wealth", 
#       y = "Utility Function for Jack and Jill")



## ----echo=FALSE, include=FALSE----------------------------------------------------------------------------

df2 <- tibble(W = seq(0, 20, length.out = 500))

# Create new columns, for Jack's and Jill's utility function 


df2 <- df2 %>% 
  mutate(Jack = W^(1 - 0.5)/(1 - 0.5),
         Jill = W^(1 - 0.7)/(1 - 0.7))

# Create ggplot object

ggplot(df2) + 
  geom_line(aes(x = W, y = Jack), color = "red", size = 0.5) + 
  geom_line(aes(x = W, y = Jill), color = "steelblue", size = 0.5) + 
  labs(x = "Wealth", 
       y = "Utility Function for Jack and Jill")
  



## ---------------------------------------------------------------------------------------------------------
rm(list = ls())


## ---------------------------------------------------------------------------------------------------------
financialData <- read_csv("financialData.csv")


## ---------------------------------------------------------------------------------------------------------
ggplot(data = financialData) + 
  geom_bar(aes(Industry, fill = Industry))


## ---------------------------------------------------------------------------------------------------------
financialData %>% 
  group_by(Industry) %>% 
  summarise(Number = n()) %>%   # n() is to count
  arrange(Number)    # arrange() is used to order the data


## ----warning=FALSE, message=FALSE-------------------------------------------------------------------------

library(moments)

df_summary <- financialData %>%
  summarise(mean_Sales = mean(Sales), 
            med_Sales = median(Sales),
            trim_mean = mean(Sales, trim = 0.05),
            sd_Sales = sd(Sales), 
            skew_Sales = skewness(Sales),
            kurt_Sales = kurtosis(Sales))



## ---------------------------------------------------------------------------------------------------------
df_summary


## ---------------------------------------------------------------------------------------------------------
quantile(financialData$Sales, c(0.25, 0.5, 0.75))


## ---------------------------------------------------------------------------------------------------------
# Your code


## ----echo=FALSE, include=FALSE----------------------------------------------------------------------------

df_summary <- financialData %>%
  summarise(mean_DebtEquity = mean(DebtEquity), 
            med_DebtEquity = median(DebtEquity),
            trim_mean = mean(DebtEquity, trim = 0.05),
            sd_DebtEquity = sd(DebtEquity), 
            skew_DebtEquity = skewness(DebtEquity),
            kurt_DebtEquity = kurtosis(DebtEquity))
df_summary


## ---------------------------------------------------------------------------------------------------------

# Your code



## ----echo=FALSE, include=FALSE----------------------------------------------------------------------------

quantile(financialData$DebtEquity, c(0.05,0.25,0.50, 0.75, 0.975))



## ---------------------------------------------------------------------------------------------------------
df_summary <- financialData %>%
  summarise_at(c("Sales", "DebtEquity", "CAPX"), 
               list(mean, median,sd), na.rm = TRUE)

df_summary[,1:4]
df_summary[,5:9]


## ---------------------------------------------------------------------------------------------------------

ggplot(financialData) +
  geom_histogram(aes(Sales), bins = 30, 
                 color = "white", 
                 fill = "steelblue")



## ---------------------------------------------------------------------------------------------------------
# Keep observations where Sales < 9000 million.
financialData <- financialData %>%
  filter(Sales <= 8000)



## ---------------------------------------------------------------------------------------------------------
ggplot(financialData) +
  geom_histogram(aes(Sales), bins = 30, 
                 color = "white", 
                 fill = "steelblue")


## ---------------------------------------------------------------------------------------------------------

# Your code



## ----echo=FALSE, include=FALSE----------------------------------------------------------------------------

ggplot(financialData) +
  geom_histogram(aes(CAPX), bins = 30, 
                 color = "white", 
                 fill = "steelblue")





## ----echo=FALSE, include=FALSE----------------------------------------------------------------------------

ggplot(financialData) +
  geom_histogram(aes(DebtEquity), bins = 30, 
                 color = "white", 
                 fill = "steelblue")



## ----boxSales, fig.cap="Boxplot for Sales"----------------------------------------------------------------
box_sales <- ggplot(financialData) +
  geom_boxplot(aes(Sales), fill = "thistle") +
  coord_flip()
box_sales


## ---------------------------------------------------------------------------------------------------------
# Your code


## ----echo=FALSE, include=FALSE----------------------------------------------------------------------------
box_capx <- ggplot(financialData) +
  geom_boxplot(aes(CAPX), fill = "pink") +
  coord_flip()


box_de <- ggplot(financialData) +
  geom_boxplot(aes(DebtEquity), fill = "steelblue") +
  coord_flip()




## ----warning=FALSE, message=FALSE-------------------------------------------------------------------------
library(gridExtra)


## ----boxfinancial, fig.width=6, fig.cap="Boxplots for Financial Data"-------------------------------------
grid.arrange(box_sales, box_capx, box_de, nrow = 1)


## ---------------------------------------------------------------------------------------------------------

rm(list = ls()) 



## ----message=FALSE, warning=FALSE-------------------------------------------------------------------------

df_stocks <- read_csv("assets.csv")



## ---------------------------------------------------------------------------------------------------------

# Note how we use the filter() to obtain the relevant time period. 

fig_APPL <- ggplot( data = filter(df_stocks, date >= "2019-01-01")) +
  geom_line(aes(x = date, y = AAPL), color = "purple") +
  labs(x = "Time",
       y = "Apple Share Price ($)")
  
fig_APPL


## ---------------------------------------------------------------------------------------------------------

fig_AMZN <- ggplot( data = filter(df_stocks, date >= "2019-01-01")) +
  geom_line(aes(x = date, y = AMZN), color = "plum4") +
  labs(x = "Time",
       y = "Amazon Share Price ($)")

fig_AMZN



## ---------------------------------------------------------------------------------------------------------
# Your code

# There are many colours avaiable. 

# You may choose from the following colours:

# "steelblue","palegreen4", "thistle3", bisque4", "red", "green", "slategray".

 


## ----echo=FALSE, include=FALSE----------------------------------------------------------------------------

fig_NFLX <- ggplot( data = filter(df_stocks, date >= "2019-01-01")) +
  geom_line(aes(x = date, y = NFLX), color = "steelblue") +
  labs(x = "Time",
       y = "Netflix Share Price ($)")

fig_NFLX



## ----echo=FALSE, include=FALSE----------------------------------------------------------------------------

fig_MSFT <- ggplot( data = filter(df_stocks, date >= "2019-01-01")) +
  geom_line(aes(x = date, y = MSFT), color = "slategray") +
  labs(x = "Time",
       y = "Microsoft Share Price ($)")

fig_MSFT



## ----fig.width=6, fig.height=4.5, fig.cap = "Prices of Four Stocks"---------------------------------------
grid.arrange(fig_AMZN,fig_APPL,fig_MSFT,fig_NFLX, nrow = 2)


## ---------------------------------------------------------------------------------------------------------
simple_df <- tibble(date = c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04"),
                    Asset1 = c(3,4,3,5),
                    Asset2 = c(7,5,6,7))

simple_df


## ---------------------------------------------------------------------------------------------------------
simple_df <- simple_df %>%
  gather(Stocks, Price, -date)

simple_df


## ---------------------------------------------------------------------------------------------------------
simple_df <- simple_df %>%
  group_by(Stocks) %>%
  mutate(Return = log(Price) - log(lag(Price)))


## ---------------------------------------------------------------------------------------------------------
simple_df <- simple_df %>% 
  drop_na()

simple_df


## ---------------------------------------------------------------------------------------------------------
# Your code 
# Hint: Incomplete code provided below. 
# Replace ??? with appropriate code 


# df_returns <- ??? %>%
#  gather(???, ???, -date) %>%
#  group_by(Stocks) %>%
#  mutate(Return = log(Price)-log(lag(Price))) %>%
#  select(date, ???, ???) %>% 
#  drop_na()



## ----message=FALSE, warning=FALSE, echo=FALSE, include=FALSE----------------------------------------------
df_returns <- df_stocks %>%
  gather(Stocks, Price, -date) %>%
  group_by(Stocks) %>%
  mutate(Return = log(Price)-log(lag(Price))) %>%
  select(date, Stocks, Return) %>% 
  drop_na()


## ---------------------------------------------------------------------------------------------------------

# Apple
summary(filter(df_returns, Stocks == "AAPL"))

# Amazon
summary(filter(df_returns, Stocks == "AMZN"))

# Netflix
summary(filter(df_returns, Stocks == "NFLX"))

# Microsoft
summary(filter(df_returns, Stocks == "MSFT"))



## ----retfig, echo=FALSE,fig.height= 5, fig.width=6,  fig.cap = "Returns for Four Assets"------------------


ggplot(df_returns, aes(x = date, y = Return, 
                       group = Stocks,
                       color = Stocks)) + 
  geom_line() +
  facet_wrap(~Stocks, ncol = 2) +
  theme(legend.position = "none")



## ----amazondensity, fig.height= 5, fig.width=6,  fig.cap = "Density Plot for Returns"---------------------


ggplot(df_returns) + 
  geom_density(aes(x = Return, 
                       group = Stocks, 
                   color = Stocks) ) +
  facet_wrap(~Stocks, ncol = 2) +
  theme(legend.position = "none")



## ---------------------------------------------------------------------------------------------------------
df_returns <- df_returns %>%
  spread(Stocks, Return) %>%
  drop_na()


## ----scat1, fig.height= 3.5, fig.width=4.5,  fig.cap = "Scatterplot for Apple and Amazon"-----------------

ggplot(df_returns) +
  geom_point(aes(x = AAPL, y = AMZN), 
             color = "steelblue", alpha = 0.4) +
  labs(title = "Scatterplot for Apple & Amazon", 
       x = "Return on Apple Stock",
       y = "Return on Amazon Stock")
  



## ---------------------------------------------------------------------------------------------------------
# Your code


## ----echo=FALSE, include=FALSE----------------------------------------------------------------------------

ggplot(df_returns) +
  geom_point(aes(x = NFLX, y = MSFT), 
             color = "thistle4", alpha = 0.4) +
  labs(title = "Scatterplot for Apple & Amazon", 
       x = "Return on Netflix",
       y = "Return on Microsoft")


