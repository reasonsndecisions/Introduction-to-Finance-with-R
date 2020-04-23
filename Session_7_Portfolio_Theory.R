## ----warning=FALSE, include=FALSE--------------------------
library(tidyverse)
library(gridExtra)


## ----logreturn, fig.cap="Normal Distribution and Log Return"----

df <- tibble(x = seq(-15,30, length.out = 500)) %>%
  mutate(y1 = dnorm(x, mean = 5,sd = 5),
         y2 = dnorm(x, mean = 5,sd = 2))



ggplot(df) + 
  geom_path(aes(x,y1, color = "Portfolio Y")) + 
  geom_path(aes(x, y2, color = "Portfolio X")) +
  labs(x = "Log Return Y", y = "f(y)") +
  theme(axis.title=element_text(size=8),
        legend.title=element_blank())
  



rm(df)


## ----ftsenorm, fig.cap= "FTSE 100 Returns and Normal Distribution"----
# Open Index data

indexRt <- read_csv(file = "indices.csv")

# Compute Returns and select FTSE 1nd EuroNext

indexRt <- indexRt %>%
  mutate(FTSE = log(FTSE) - log(lag(FTSE))) %>%
  select(date, FTSE) %>%
  drop_na()

c(mean(indexRt$FTSE),sd(indexRt$FTSE))

fig1 <- ggplot(indexRt) +
  geom_density(aes(FTSE), color = "steelblue") +
  stat_function(fun = dnorm,  
                args = list(mean = mean(indexRt$FTSE), 
                            sd = sd(indexRt$FTSE)),
                color = "red") +
  labs(title = "Panel 1")
  

fig2 <- ggplot(indexRt) +
  geom_density(aes(FTSE), color = "steelblue") +
  stat_function(fun = dnorm,  
                args = list(mean = median(indexRt$FTSE), 
                            sd = mad(indexRt$FTSE, center = median(indexRt$FTSE))),
                color = "red") +
  labs(title = "Panel 2")


grid.arrange(fig1, fig2)
rm(indexRt)


## ----------------------------------------------------------

df2 <- tibble(time = seq(1,100, length.out = 100))




## ----------------------------------------------------------

# bind_rows() from dplyr is used to combine rows of different dataframes

set.seed(1234)
random_walks <- bind_rows(
  replicate(20,
            df2 %>% mutate(y = cumsum(rnorm(100, 0,1)),
                           sd1p = sqrt(time),
                           sd1n = -sqrt(time)),
            simplify=FALSE), .id="SampNum") 



## ----randomwalk, fig.height=4, fig.cap="A Normal Random Walk"----
ggplot(random_walks) + 
  geom_line(aes(x = time,y= y, group = SampNum, 
           color = SampNum), size = 0.3) +
  theme(legend.position="none") +
  geom_line(aes(x = time, y = sd1p), color = "red", size = 1) +
  geom_line(aes(x = time, y = sd1n) , color = "red", size = 1)
              
rm(df2,random_walks)


## ----------------------------------------------------------
# Your code here 


## ----echo=FALSE, include=FALSE-----------------------------
## A Solution
set.seed(1234)
df3 <- tibble(time = seq(1,100, length.out = 100))

random_walks1 <- bind_rows(
  replicate(20,
            df3 %>% mutate(y = cumsum(rnorm(100, 0,2)),
                           sd1p = 2*sqrt(time),
                           sd1n = -2*sqrt(time)),
            simplify=FALSE), .id="SampNum") 

ggplot(random_walks1) + 
  geom_line(aes(x = time,y= y, group = SampNum, 
           color = SampNum), size = 0.3) +
  theme(legend.position="none") +
  geom_line(aes(x = time, y = sd1p), color = "red", size = 0.8) +
  geom_line(aes(x = time, y = sd1n) , color = "red", size = 0.8)

rm(df3,random_walks1)


## ----------------------------------------------------------
# Save expected returns and standard deviations 

R1 <- 2 # Expected return for Asset 1
R2 <- 3 # Expected return for Asset 2

S1 <- 16 # Variance return for Asset 1
S2 <- 25 # Variance return for Asset 2


## ----------------------------------------------------------

# Two possible correlations

Corr1 <- 0.25  
Corr2 <- -0.25

cov1 <- Corr1*sqrt(S1)*sqrt(S2)
cov2 <- Corr2*sqrt(S1)*sqrt(S2)

# To keep things simple, w is in [0,1]

two_assets <- tibble(w = seq(0,1, length.out = 500)) %>%
  mutate(Rp1 = R1*w + R2*(1-w),
         Sp1 = sqrt(w^2*S1+ (1-w)^2*S2 + 2*w*(1-w)*cov1),
         Rp2 = R1*w + R2*(1-w),
         Sp2 = sqrt(w^2*S1+ (1-w)^2*S2 + 2*w*(1-w)*cov2))




## ----meanvarportfolio, fig.cap="Risk-Return Trade-off and Correlation"----

fig_25 <- ggplot(two_assets) + 
  geom_point(aes(Sp1, Rp1), color = "steelblue") +
  geom_point(aes(Sp2, Rp2), color = "purple") +
  labs(x = "Portfolio Standard Deviation", 
       y = "Portfolio Expected Return") +
  geom_point(aes(x = 3.572588, y = 2.737475), size = 3, color = "red") + 
  geom_point(aes(x = 4.078664, y = 2.737475), size = 3, color = "red") +
  annotate("text", x = 3.5, y = 2.8, label = "X")+
  annotate("text", x = 4.2, y = 2.7, label = "Y")

fig_25



## ----------------------------------------------------------
rm(list = ls())


## ----------------------------------------------------------

# Your code. 



## ----echo=FALSE, include=FALSE-----------------------------

R1 <- 2 # Expected return for Asset 1
R2 <- 3 # Expected return for Asset 2

S1 <- 16 # Variance return for Asset 1
S2 <- 25 # Variance return for Asset 2

# Two possible correlations

Corr1a <- 1 
Corr2a <- -1

cov1a <- Corr1a*sqrt(S1)*sqrt(S2)
cov2a <- Corr2a*sqrt(S1)*sqrt(S2)

two_assets_2 <- tibble(w = seq(0,1, length.out = 500)) %>%
  mutate(Rp1 = R1*w + R2*(1-w),
         Sp1 = sqrt(w^2*S1+ (1-w)^2*S2 + 2*w*(1-w)*cov1a),
         Rp2 = R1*w + R2*(1-w),
         Sp2 = sqrt(w^2*S1+ (1-w)^2*S2 + 2*w*(1-w)*cov2a))


ggplot(two_assets_2) + 
  geom_point(aes(Sp1, Rp1), color = "steelblue") +
  geom_point(aes(Sp2, Rp2), color = "thistle4") +
  labs(x = "Portfolio Standard Deviation", 
       y = "Portfolio Expected Return")  
  




## ----------------------------------------------------------
rm(list = ls())


## ----------------------------------------------------------

## Our first function - mutiply a number by 2
myFunc1 <- function(x){
  x*2
}

myFunc1(2)

#myFunc1("s")



## ----------------------------------------------------------
## Our second function - mutiply a number by 2 after checking

myFunc2 <- function(x){
  if (is.character(x)){
    print("Cannot multiply character with a number")
  } else {
    x*2 
  }
}

myFunc2(2)
#myFunc2("s")



## ----------------------------------------------------------
rm(list = ls())


## ----------------------------------------------------------

# Write a function 
opportunity.set <- function(x){
# Specify expected returns and variances
R1 <- 2 # Expected return for Asset 1
R2 <- 3 # Expected return for Asset 2

S1 <- 16 # Variance return for Asset 1
S2 <- 25 # Variance return for Asset 2
# Dataframe containing Return and Risk
two_assets <- tibble(w = seq(0,1, length.out = 500)) %>%
  mutate(Rp = R1*w + R2*(1-w),
         Sp = sqrt(w^2*S1+ (1-w)^2*S2 + 2*w*(1-w)*x*sqrt(S1)*sqrt(S2)))
}




## ----------------------------------------------------------
df <- opportunity.set(1)

rm(df)


## ----------------------------------------------------------
# Create a list of correlations

correlations <- list(-1, -0.75, -0.5,-0.25,0, 0.25,0.5, 0.75, 1)

# Apply your function to the list of correlations

portfolios <- map_df(correlations, opportunity.set) 



## ----------------------------------------------------------

portfolios <- portfolios %>%
  mutate(Correlation = as.factor(rep(c(-1, -0.75, -0.5,-0.25,0, 0.25,0.5, 0.75, 1), 
                                     each = 500)))


## ----portfront, fig.width=6, fig.cap="Portfolio Returns with Different Correlations"----

ggplot(portfolios, aes(Sp, Rp, color = Correlation)) +
  geom_point(alpha = 0.60, size = 0.8) +
  labs(x = "Portfolio Standard Deviation", 
       y = "Portfolio Expected Return")

rm(portfolios, correlations, opportunity.set)


## ----------------------------------------------------------

R1 <- 2 # Expected return for Asset 1
R2 <- 3 # Expected return for Asset 2

S1 <- 16 # Variance return for Asset 1
S2 <- 25 # Variance return for Asset 2


# Correlaton 0.25
Corr1 <- 0.25
cov1 <- Corr1*sqrt(S1)*sqrt(S2)

w1min <- (S2 - cov1)/(S1 + S2 - 2*cov1)

two_assets <- tibble(w = seq(0,1, length.out = 500)) %>%
  mutate(Rp1 = R1*w + R2*(1-w),
         Sp1 = sqrt(w^2*S1+ (1-w)^2*S2 + 2*w*(1-w)*cov1))

# Expected return for the minimum variance portfolio 
Rpmin <- R1*w1min + R2*(1-w1min)

# Standard deviation for the minimum variance portfolio 
Sdmin <-  sqrt(w1min^2*S1+ (1-w1min)^2*S2 + 2*w1min*(1-w1min)*cov1)


## ----minvarfig, fig.cap="Portfolio Frontier and the Minimum-Variance Portfolio"----

ggplot(two_assets) + 
  geom_point(aes(Sp1, Rp1), color = "steelblue") +
  labs(x = "Portfolio Standard Deviation", 
       y = "Portfolio Expected Return") +
  geom_point(aes(x = Sdmin, y = Rpmin), size = 3, color = "red") +
  annotate("text", x = Sdmin+0.3, y = Rpmin, 
           label = "Minimum-Variance Portfolio", size = 4) +
  geom_point(aes(x = 3.73468, y = 2.60), size = 3, color = "green") +
  geom_point(aes(x = 3.73468, y = 2.11), size = 3, color = "purple") +
  annotate("text", x = 3.73468, y = 2.65, 
           label = "A", size = 4) +
  annotate("text", x = 3.73468, y = 2.15, 
           label = "B", size = 4) 
  


## ----------------------------------------------------------


port.var <- function(w) w^2*S1+ (1-w)^2*S2 + 2*w*(1-w)*cov1

optimize(port.var, c(0, 1))


## ----------------------------------------------------------
rm(list = ls())


## ----------------------------------------------------------
# Write a function

set.seed(1234)

# We can extend our function and include means, 
# varainces and covariances as additional arguments.

portfolio.frontier <- function(x){
  R1 <- 5
  R2 <- 6
  R3 <- 8
  S1 <- 36
  S2 <- 64
  S3 <- 81
  C12 <- 20
  C13 <- -20
  C23 <- 0
# Dataframe containing Return and Risk
two_assets <- tibble(w1 = runif(100,min = 0,max = 0.9)) %>%
  mutate(Rp = R1*w1 + R2*w2 + R3*(1-w1-w2),
         Sp = sqrt(w1^2*S1+ w2^2*S2 +(1-w1-w2)^2*S3+
                     2*w1*w2*C12 + 2*w1*(1-w1-w2)*C13 + 2*w2*(1-w1-w2)*C23))
}

# A list of correlations
set.seed(1234)
w2 <- runif(50,min = 0,max = 0.5)

# Apply your function to the list of correlations
port_returns <- map_df(w2, portfolio.frontier)  
  


## ----threeast,fig.width=6, fig.height=5, fig.cap="Portfolios for Three Assets"----
ggplot(port_returns, aes(Sp, Rp)) +
  geom_point(alpha = 0.15, size = 0.5, color = "steelblue") +
  labs(x = "Portfolio Standard Deviation", 
       y = "Portfolio Expected Return") +
  geom_point(aes(x = 6, y = 5, color = "Asset 1"), size = 2.5)+
  geom_point(aes(x = 8, y = 6, color = "Asset 2"), size = 2.5)+
  geom_point(aes(x = 9, y = 8, color = "Asset 3"), size = 2.5) +
  theme(axis.title=element_text(size=8),
        legend.title=element_blank())
  




## ----------------------------------------------------------

R1 <- 2 # Expected return for Asset 1
R2 <- 3 # Expected return for Asset 2

S1 <- 16 # Variance return for Asset 1
S2 <- 25 # Variance return for Asset 2

Corr1 <- 0.25

cov1 <- Corr1*sqrt(S1)*sqrt(S2)


Rf <- 1

vcoc1 <- matrix(data = c(S1,cov1,cov1,S2), nrow = 2, byrow = TRUE)

Evec <- matrix(data = c(R1, R2), nrow = 2)

vcoc1

Evec-Rf
z <- solve(vcoc1) %*% (Evec-Rf)
z

w1t <- z[1]/sum(z)
w2t <- z[2]/sum(z)

# Expected return for the tangency portfolio 
Rpt <- R1*w1t + R2*(1-w1t)
Rpt
# Standard deviation for the tangency portfolio 
Sdt <-  sqrt(w1t^2*S1+ (1-w1t)^2*S2 + 2*w1t*(1-w1t)*cov1)
Sdt
slope <- (Rpt-Rf)/Sdt
slope

#

two_assets <- tibble(w = seq(-0.5,1.2, length.out = 500)) %>%
  mutate(Rp = R1*w + R2*(1-w),
         Sp = sqrt(w^2*S1+ (1-w)^2*S2 + 2*w*(1-w)*cov1))

ggplot(two_assets) +
  geom_point(aes(Sp, Rp), size = 0.09, color = "steelblue") +
  labs(x = "Portfolio Standard Deviation", 
       y = "Portfolio Expected Return") + 
  geom_point(aes(x=0, y = 1), color = "red") +
  geom_abline(intercept = 1, slope = slope, color = "red") + 
  xlim(0, 7) +
  ylim(0.5,4)
  


