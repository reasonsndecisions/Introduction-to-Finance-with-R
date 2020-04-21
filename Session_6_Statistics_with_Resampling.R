## ----message=FALSE, warning=FALSE-------------------------------------------------------------------------
# Load packages needed for this session

library(tidyverse) 
library(gridExtra) # To display multiple plots
library(moments) # To compute skewness



## ----message=FALSE, warning=FALSE-------------------------------------------------------------------------

directorsTrades <- read_csv("directorsPurchases.csv")



## ----echo=FALSE, include=FALSE----------------------------------------------------------------------------
head(directorsTrades)


## ----table1, message=FALSE, warning=FALSE, echo=FALSE-----------------------------------------------------

library(kableExtra)
kable(directorsTrades[1:10,], 
      format="latex", 
      caption = "Directors' Net Purchases (in £)",
      booktabs = TRUE) %>%
  kable_styling(latex_options = "striped")



## ---------------------------------------------------------------------------------------------------------

# Your code

# Hint: You will need three functions to compute 
# relevant mean differences in the code below

# directorsTrades %>% 
#  ???(Sector) %>% ???(???(Purchase))


## ----echo=FALSE, include=FALSE----------------------------------------------------------------------------
directorsTrades %>% 
 group_by(Sector) %>% 
  summarise(mean(Purchase), 
            n())


## ----histpurch, fig.width=5,fig.height=4.5, fig.cap="Histograms for Directors' Net Purchases in Two Sectors"----

hist_retail <- ggplot(data = filter(directorsTrades, Sector == "Retail")) +
  geom_histogram(aes(Purchase), color = "white", fill = "thistle3")

hist_fin <- ggplot(data = filter(directorsTrades,Sector == "Finance")) +
  geom_histogram(aes(Purchase), color = "white", fill = "steelblue")


grid.arrange(hist_retail,hist_fin)

# Remove histograms to clear R environment 

rm(hist_retail,hist_fin)


## ---------------------------------------------------------------------------------------------------------
# Your code


## ----echo=FALSE, include=FALSE----------------------------------------------------------------------------

directorsTrades %>% 
  group_by(Sector) %>%
  summarise(Averages = mean(Purchase))




## ---------------------------------------------------------------------------------------------------------

population_diff <-  (40151.25 - 40125.94)


## ---------------------------------------------------------------------------------------------------------
# Create dataframe for finance and retail

Retail_population <- directorsTrades %>%
  filter(Sector == "Retail")
Finance_population <- directorsTrades %>%
  filter(Sector == "Finance")



## ---------------------------------------------------------------------------------------------------------
# Retail sector

set.seed(1)

sample_Retail <- sample_n(Retail_population, size = 500, replace = TRUE)



## ---------------------------------------------------------------------------------------------------------
# Finance sector

set.seed(1)

sample_Finance <- sample_n(Finance_population, size = 400, replace = TRUE)



## ----histretail1, fig.cap="Histgrams for Net Purchases"---------------------------------------------------

hist_retail1 <- ggplot(sample_Retail) + 
  geom_histogram(aes(Purchase), color = "white", fill = "steelblue")

hist_finance1 <- ggplot(sample_Finance) + 
  geom_histogram(aes(Purchase), color = "white", fill = "thistle3")

grid.arrange(hist_retail1,hist_finance1)

# Remove histograms
rm(hist_finance1,hist_retail1)


## ---------------------------------------------------------------------------------------------------------

sample_Retail %>%
  summarise(Count = n(),
            mean_retail = mean(Purchase))



## ---------------------------------------------------------------------------------------------------------

sample_Finance %>%
  summarise(Count = n(),
            mean_retail = mean(Purchase))





## ---------------------------------------------------------------------------------------------------------
# set.seed(2)
# Your code 



## ----echo=FALSE, include=FALSE----------------------------------------------------------------------------
set.seed(2)


sample_Retail2 <- sample_n(Retail_population, size = 500, replace = TRUE)


# Finance sector

set.seed(2)

sample_Finance2 <- sample_n(Finance_population, size = 400, replace = TRUE)

sample_Retail2 %>%
  summarise(Count = n(),
            mean_retail = mean(Purchase))

sample_Finance2 %>%
  summarise(Count = n(),
            mean_finance = mean(Purchase))

rm(sample_Finance2,sample_Finance,sample_Retail,sample_Retail2)



## ---------------------------------------------------------------------------------------------------------
set.seed(123)
rnorm(5, mean = 0, sd = 1)



## ---------------------------------------------------------------------------------------------------------
set.seed(123)

df1 <- replicate(n = 5, expr = rnorm(5, mean = 0, sd = 1))

head(df1)
# Convert the matrix df1 into a dataframe

df1 <- as.data.frame(df1)




## ---------------------------------------------------------------------------------------------------------
df1 <- data.frame(x = colMeans(df1))

df1


## ----histfig, echo=FALSE, fig.cap="\\label{fig:hist_smeans1} A Histogram of Sample Means"-----------------

ggplot(df1, aes(x)) + geom_histogram(color = "white", fill = "steelblue")

rm(df1)


## ---------------------------------------------------------------------------------------------------------
# set.seed(1234)
# Your code


## ----echo=FALSE, include=FALSE----------------------------------------------------------------------------
set.seed(1234)
rnorm(10, mean = 0, sd = 2)


## ---------------------------------------------------------------------------------------------------------
# set.seed(1234)
# Your code


## ----echo=FALSE, include=FALSE----------------------------------------------------------------------------
set.seed(1234)
df2 <- as.data.frame(replicate(n = 200, expr = rnorm(10, mean = 0, sd = 2)))


## ---------------------------------------------------------------------------------------------------------
# Your code 


## ----echo=FALSE, include=FALSE----------------------------------------------------------------------------
df2 <- data.frame(x = colMeans(df2))



## ---------------------------------------------------------------------------------------------------------
# Your code


## ----echo=FALSE, include=FALSE----------------------------------------------------------------------------
ggplot(df2, aes(x)) + geom_histogram(color = "white", fill = "thistle3")
rm(df2)


## ---------------------------------------------------------------------------------------------------------
# 10,000 samples of size 500 each for the retail sector

set.seed(123)

sample500_retail <- bind_rows(
  replicate(n = 10000, expr = sample_n(Retail_population, size = 500,replace = TRUE),
            simplify=FALSE), 
  .id="Sample_Number") 



## ---------------------------------------------------------------------------------------------------------
# 10,000 samples of size 400 each for the finance sector

set.seed(123)

sample400_finance <- bind_rows(
  replicate(n = 10000, expr = sample_n(Finance_population, size = 400,replace = TRUE),
            simplify=FALSE), 
  .id="Sample_Number") 



## ---------------------------------------------------------------------------------------------------------
sample500_retail <- sample500_retail %>%
  group_by(Sample_Number) %>%
  summarise(sampleMean_R = mean(Purchase)) %>%
  ungroup()

sample400_finance <- sample400_finance %>%
  group_by(Sample_Number) %>%
  summarise(sampleMean_F = mean(Purchase)) %>%
  ungroup()



## ---------------------------------------------------------------------------------------------------------
sample1 <- left_join(sample500_retail, sample400_finance, by = "Sample_Number")



## ---------------------------------------------------------------------------------------------------------
sample1 <- sample1 %>% 
  mutate(sample_diff = sampleMean_F - sampleMean_R)


## ----truesamp, fig.cap="Sampling Distibution based on the Population"-------------------------------------

ggplot(sample1, aes(sample_diff)) +
  geom_histogram(color= "white", fill = "steelblue", bins = 50) +
   geom_vline(xintercept = population_diff, color = "red")



## ----qqplotsam, fig.cap="QQ-Plot for Sampling Distribution"-----------------------------------------------

ggplot(sample1, aes(sample = sample_diff)) + 
  stat_qq(color = "steelblue") + 
  stat_qq_line(color = "red") +
  labs(x = "Theoretical Normal Quantiles", y = "Sample")



## ---------------------------------------------------------------------------------------------------------
mean(sample1$sample_diff)


## ---------------------------------------------------------------------------------------------------------
sample_sd <- sd(sample1$sample_diff) 


## ---------------------------------------------------------------------------------------------------------
rm(sample1, sample400_finance,sample500_retail)


## ---------------------------------------------------------------------------------------------------------
# Your code


## ---------------------------------------------------------------------------------------------------------
# Retail sector

set.seed(12345)

sample_Retail <- sample_n(Retail_population, size = 500, replace = TRUE)


## ---------------------------------------------------------------------------------------------------------
# Finance sector

set.seed(12345)

sample_Finance <- sample_n(Finance_population, size = 400, replace = TRUE)


## ---------------------------------------------------------------------------------------------------------
mean_diff <- mean(sample_Finance$Purchase) - mean(sample_Retail$Purchase)


## ---------------------------------------------------------------------------------------------------------
# Resamples : Retail
set.seed(12357)

Resample_Retail <- bind_rows(
  replicate(n = 10000, expr = sample_n(sample_Retail, size = 500,replace = TRUE),
            simplify=FALSE), 
  .id="Sample_Number") 


## ---------------------------------------------------------------------------------------------------------
# Resamples : Finance
set.seed(12357)

Resample_Finance <- bind_rows(
  replicate(n = 10000, expr = sample_n(sample_Finance, size = 400,replace = TRUE),
            simplify=FALSE), 
  .id="Sample_Number") 


## ---------------------------------------------------------------------------------------------------------

Resample_Retail <- Resample_Retail %>%
  group_by(Sample_Number) %>%
  summarise(Retail_Mean = mean(Purchase)) %>%
  ungroup()


Resample_Finance <- Resample_Finance %>%
  group_by(Sample_Number) %>%
  summarise(Finance_Mean = mean(Purchase)) %>%
  ungroup()



## ---------------------------------------------------------------------------------------------------------
# Join the two dataframe

Resample_All <- left_join(Resample_Retail, Resample_Finance, by = "Sample_Number")


# Compute the difference in means

Resample_All <- Resample_All %>%
  mutate(mean_difference = Finance_Mean - Retail_Mean) %>%
  select(mean_difference)




## ----bootstrapdist, fig.width=5.5, fig.height=4, fig.cap="Histogram of Bootstrap Distribution"------------

ggplot(Resample_All) + 
  geom_histogram(aes(mean_difference),
                 color = "white", 
                 fill = "steelblue", 
                 bins = 30) +
  labs(x = "Mean Differences") +
  geom_vline(xintercept = mean_diff, color = "red")




## ---------------------------------------------------------------------------------------------------------
mean(Resample_All$mean_difference)


## ---------------------------------------------------------------------------------------------------------
mean(Resample_All$mean_difference) - mean_diff


## ---------------------------------------------------------------------------------------------------------
boot_sd <- sd(Resample_All$mean_difference)


## ---------------------------------------------------------------------------------------------------------

skewness(Resample_All$mean_difference)



## ---------------------------------------------------------------------------------------------------------
quantile(Resample_All$mean_difference, c(0.025, 0.975))


## ---------------------------------------------------------------------------------------------------------
-509.791 - 2*boot_sd
-509.791 + 2*boot_sd

