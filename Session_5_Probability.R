## ---------------------------------------------------------------------------------------------------------
# The rep() function generates repeated observations 
# For example, below we use it to generate 6 observations of 1/6.

sample(c(1,2,3,4,5,6), size = 1, prob = c(rep(1/6,6)))

# Rerun the above code a few times to see what happens.


# Try changing the prob argument above. For example, 

sample(c(1,2,3,4,5,6), size = 1, prob = c(0.1,0.3,0.2,0,0,0.4))

# Now 6 is the most likely outcome

# Rerun the above code a few times to see what happens.



## ----echo=FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
library(tidyverse)
library(gridExtra)

set.seed(1234)

df_dice1 <- data.frame(Rolls = seq(from=10, to=40, by = 5))

compute_prob <- function(x){
  roll <- sample(1:6,x, replace = TRUE)
  prob6 <- sum(roll=="6")/length(roll)
  return(prob6)
}

df_dice1$Probability <- apply(df_dice1, MARGIN = 1, FUN = compute_prob)

dice1 <- ggplot(df_dice1, aes(x=Rolls, y = Probability)) + 
  geom_point(color="red") + 
  geom_line(color="steelblue") + 
  geom_hline(yintercept=1/6, linetype="dashed", color = "black") +
  labs(x = "Number of Rolls of a Dice", 
       y = "Relative Frequency of 6") +
  theme(axis.title=element_text(size=8))

df_dice2 <- data.frame(Rolls = seq(from=10, to=1000, by = 20))


df_dice2$Probability <- apply(df_dice2, MARGIN = 1, FUN = compute_prob)

dice2 <- ggplot(df_dice2, aes(x=Rolls, y = Probability)) + 
  geom_point(color="red") + 
  geom_line(color="steelblue") + 
  geom_hline(yintercept=1/6, linetype="dashed", color = "black") +
  labs(x = "Number of Rolls of a Dice", 
       y = "Relative Frequency of 6") +
  theme(axis.title=element_text(size=8))



## ----dicefig, echo=FALSE,  fig.cap = "Relative Frequency for a Dice Roll"---------------------------------



grid.arrange(dice1, dice2)




## ----echo=FALSE-------------------------------------------------------------------------------------------
rm(compute_prob,df_dice2, df_dice1, dice2,dice1)


## ---------------------------------------------------------------------------------------------------------
x <- c(0,1,2)
p <- c(1/4,1/2,1/4)

mean_x <- sum(x * p)

mean_x

var_x <- sum((x-mean_x)^2*p)

var_x



## ---------------------------------------------------------------------------------------------------------
df <- tibble(probs = rep(1/6, 6),
             R_A = c(-0.4,-0.2,-0.2,0.2,0.2,0.4),
             R_B = c(1,0.5, 0,-0.1667, -0.1667,-0.5 ))

R_A <- ggplot(df ) + 
  geom_histogram(aes(R_A), fill = "steelblue")

R_B <- ggplot(df ) + 
  geom_histogram(aes(R_B), fill = "thistle3")




## ----covfig, fig.cap="Distribution of Returns for Two Assets"---------------------------------------------
grid.arrange(R_A,R_B)


## ---------------------------------------------------------------------------------------------------------

# Mean 0, sigma 1

rnorm(n = 5, mean = 0, sd = 1)



## ---------------------------------------------------------------------------------------------------------

pnorm(q = 0, mean = 0,sd = 1)



## ---------------------------------------------------------------------------------------------------------
qnorm(p = 0.25, mean = 1, sd = 1)

# Composite function
pnorm(qnorm(p = 0.25, mean = 1, sd = 1))


## ---------------------------------------------------------------------------------------------------------

df <- tibble(epsilon = seq(-5,5, length.out = 500)) %>%
  mutate(f = dnorm(epsilon, mean = 0,sd = 1)) 



n1 <- ggplot(df, aes(epsilon,f)) + 
  geom_path(color="steelblue") + 
  xlab("Standard Normal Random Variable"~""*epsilon*"") +
  ylab(expression("f"~"("*epsilon*")")) +
  theme(axis.title=element_text(size=8))




## ----normal1fig, echo=FALSE,  fig.cap = "Standard Normal Distribution"------------------------------------
n1

rm(n1, df)


## ---------------------------------------------------------------------------------------------------------
# Your code
# Mean 5, sigma 10

rnorm(n = 5, mean = 5, sd = 1)



## ---------------------------------------------------------------------------------------------------------
# Your code 


## ---------------------------------------------------------------------------------------------------------
# Your code



## ----echo=FALSE, include=FALSE----------------------------------------------------------------------------
pnorm(1) - pnorm(-1)
pnorm(2) - pnorm(-2)
pnorm(3) - pnorm(-3)



## ---------------------------------------------------------------------------------------------------------
# Your code


## ---------------------------------------------------------------------------------------------------------
# Your code


## ----echo=FALSE, include=FALSE----------------------------------------------------------------------------
pnorm(q = 6, mean = 5, sd = 1) - pnorm(4, mean = 5, sd = 1) 


## ----standnormal, fig.width=6, fig.height=4.8, fig.cap="Important Intervals for Standard Normal Variable"----

# In situations where we repeat code, it is better to create a function.
# Writing functions is an important skills for all data analysts. 

df <- tibble(epsilon = seq(-5,5, length.out = 500)) %>%
  mutate(f = dnorm(epsilon, mean = 0,sd = 1)) 


n1 <- ggplot(df, aes(epsilon,f)) + 
  geom_path(color="steelblue") + 
  xlab("Standard Normal Random Variable"~""*epsilon*"") +
  ylab(expression("f"~"("*epsilon*")")) +
  theme(axis.title=element_text(size=8)) +
  geom_segment(aes(x = -1, y = 0, xend = -1, yend = dnorm(-1)), color = "red") +
  geom_segment(aes(x = 1, y = 0, xend = 1, yend = dnorm(1)), color = "red") +
  geom_area(stat = "function", fun = dnorm, fill = "lavenderblush", xlim = c(-1, 1)) +
  annotate("text", x = -1, y = -0.02, label = "-1", size = 2) +
  annotate("text", x = 1, y = -0.02, label = "1", size = 2)

n2 <- ggplot(df, aes(epsilon,f)) + 
  geom_path(color="steelblue") + 
  xlab("Standard Normal Random Variable"~""*epsilon*"") +
  ylab(expression("f"~"("*epsilon*")")) +
  theme(axis.title=element_text(size=8)) +
  geom_segment(aes(x = -2, y = 0, xend = -2, yend = dnorm(-2)), color = "red") +
  geom_segment(aes(x = 2, y = 0, xend = 2, yend = dnorm(2)), color = "red") +
  geom_area(stat = "function", fun = dnorm, fill = "thistle", xlim = c(-2, 2)) +
  annotate("text", x = -2, y = -0.02, label = "-2", size = 2) +
  annotate("text", x = 2, y = -0.02, label = "2", size = 2)


n3 <- ggplot(df, aes(epsilon,f)) + 
  geom_path(color="steelblue") + 
  xlab("Standard Normal Random Variable"~""*epsilon*"") +
  ylab(expression("f"~"("*epsilon*")")) +
  theme(axis.title=element_text(size=8)) +
  geom_segment(aes(x = -3, y = 0, xend = -3, yend = dnorm(-3)), color = "red") +
  geom_segment(aes(x = 3, y = 0, xend = 3, yend = dnorm(3)), color = "red") +
  geom_area(stat = "function", fun = dnorm, fill = "antiquewhite", xlim = c(-3, 3)) +
  annotate("text", x = -3, y = -0.02, label = "-3", size = 2) +
  annotate("text", x = 3, y = -0.02, label = "3", size = 2)


grid.arrange(n1,n2,n3)



## ---------------------------------------------------------------------------------------------------------
rm(list = ls())

