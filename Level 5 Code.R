setwd("C:/Users/fayew/Documents/_Year 4 Semester 2/5092M Mixed Models with Medical Applications/Practicals")
load(file = "killers2.Rdata")
makesample(8646)
save(CWsample, file = "CWsample.RData")
attach(CWsample)

model0 <- lm(AgeFirstKill ~ 1, data = CWsample)
summary(model0)

CWsample$States <- as.numeric(CWsample$State)
model1 <- lm(States ~ 1, data = CWsample)
summary(model1)

sigmasq <- 9.62**2
sigmasq_u <- 3.09**2
sigmasq_e <- sigmasq - sigmasq_u
sqrt(sigmasq_e)

n <- power.t.test(n = NULL, delta = 2.5, sd = 9.621, power = 0.8)$n

ICC <- sigmasq_u/(sigmasq)
DE <- 1 - ICC

#FIGURE 1

library(dplyr)
library(knitr)
install.packages("kableExtra")
library(kableExtra)
state_tbl <- CWsample %>% group_by(State) %>% 
  summarise(AFK = (round(mean(AgeFirstKill), digits=2)),
            .groups = 'drop')
kable(t(state_tbl), caption = "Table of average age of first kill of killer's in each state") %>%
  kable_styling(full_width = F) %>%
  row_spec(1, bold = TRUE)

#FIGURE 2

calc_p <- function(delta,sd,power)
{
  (sqrt(2*sd**2)*(-qnorm(1 - 0.05/2, mean = 0, sd = 1) - qnorm(power/1000, mean = 0, sd = 1))/delta)**2
}

X <- calc_p(delta=2.5, sd=9.62, power=600:999)
power=600:999/1000
par(mfrow = c(1, 2))
plot(X, power, xlab="Number of participants required", ylab="Value of power", main="Scatter plot of required sample size compared to \n specified power", pch=16, col="blue")

calc_a <- function(delta,sd,alpha)
{
  (sqrt(2*sd**2)*(-qnorm(1 - alpha/1000, mean = 0, sd = 1) - qnorm(0.8, mean = 0, sd = 1))/delta)**2
}

Y <- calc_a(delta=2.5, sd=9.62, alpha=1:100)
alpha=1:100/1000
plot(Y, alpha, xlab="Number of participants required", ylab="Value of type I error rate", main="Scatter plot of required sample size compared to \n specified type I error rate", pch=16, col="red")

#FIGURE 3

par(mfrow = c(1, 1))
calc_sd <- function(delta,sd,alpha)
{
  (sqrt(2*sd**2)*(-qnorm(1 - alpha, mean = 0, sd = 1) - qnorm(0.8, mean = 0, sd = 1))/delta)**2
}

W <- calc_sd(delta=2.5, sd=1:150/10, alpha=0.025)
sd=1:150/10
plot(W, sd, xlab="Number of participants required", ylab="Value of standard deviation", main="Scatter plot of required sample size compared to standard deviation", pch=16, col="purple")
