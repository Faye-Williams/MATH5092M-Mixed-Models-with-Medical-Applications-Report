setwd("C:/Users/fayew/Documents/_Year 4 Semester 2/5092M Mixed Models with Medical Applications/Practicals")
load(file = "killers2.Rdata")
makesample(8646)
save(CWsample, file = "CWsample.RData")
table(CWsample$State)
attach(CWsample)

#Defining new variables
CWsample$Male <- CWsample$Sex == "Male"
CWsample$White <- CWsample$Race == "White"
CWsample$Hetero <- CWsample$SexualPreference == "Heterosexual"

CWsample$States <- as.numeric(CWsample$State)
colours <- c("black", "blue", "red3", "magenta",  "goldenrod4", "slateblue", "deepskyblue3", "green", "navy", "darkgreen", "purple", "orange")
States_list <- unique(paste(CWsample$State))
States_full <- c("Alabama", "Arkansas", "Arizona", "California", "Conneticut", "Washington DC", "Georgia", "Kansas", "Maryland", "Missouri", "Minnesota", "North Carolina")

#FIGURE 1
par(mfrow = c(1,2)) 
hist(CWsample$AgeLastKill, 
     xlab = "Age of killer at time of last murder (years)", 
     main = "Histogram plot of age of killer at time of last murder", 
     col = "#EBF0FF")
qqnorm(CWsample$AgeLastKill, main="QQ plot of age of killer at time of last murder")

#LINEAR MODEL
model0 <- lm(AgeLastKill ~ 1, data = CWsample)
summary(model0)
betahat <- model0$coefficients

#LINEAR REGRESSION MODEL
modelA <- lm(AgeLastKill ~ AgeFirstKill, data = CWsample)
summary(modelA)

#FIGURE 2
par(mfrow = c(1, 2))    
plot(CWsample$AgeLastKill ~ CWsample$AgeFirstKill, 
     main = "Scatter plot of age of killer at time of last \n murder vs. age at time of first murder",  
     ylab = "Age Last Kill (years)", 
     xlab = "Age First Kill (years)")
abline(modelA, lwd = 2, col = "purple")
qqnorm(modelA$resid, main="Normal QQ plot of age of killer at time of last \n murder vs. age at time of first murder")
qqline(modelA$resid, col="orange", lwd=2)

#FIGURE 3
library(knitr)
install.packages("kableExtra")
library(kableExtra)
State_tab <- table(t(CWsample$State))
kbl(t(State_tab), align="l",  caption = "Frequency table of Killer's home state") %>%
  kable_styling( full_width = F)

#FIGURE 4
par(mfrow=c(1,1))
par(mar = c(5, 4, 4, 9))
plot(CWsample$States ~ CWsample$AgeLastKill, 
     xlab = "Age Last Kill (years)", 
     ylab = " ", main = " ", pch = 16,   
     yaxt = "n", ylim = c(0, 12),
     col = colours[CWsample$States])

axis(4, at = 1:12, labels = States_full, las = 2)
abline(v = betahat, lty = 2)
axis(3, at = betahat, labels = expression(paste(widehat(beta)[0], " = 43.83")))

#RANDOM INTERCEPT MODEL
library("lme4")
modelB <- lmer(AgeLastKill ~ AgeFirstKill + (1|State), data = CWsample, REML=FALSE)
summary(modelB)
randomeffects <- ranef(modelB)
u <- randomeffects$State$"(Intercept)"
beta0 <- modelB@beta[1]
beta1 <- modelB@beta[2]

#FIGURE 5
par(mfrow=c(1,1))
par(mar = c(5,5,5,13))
plot(CWsample$AgeLastKill ~ CWsample$AgeFirstKill, 
     ylab = "Age last kill (years)", 
     xlab = "Age first kill (years)", 
     main = "Scatter plot of killer's age at first kill compared to their age at last kill, as categorised by their home state", 
     pch = 16,
     cex = 1.2,
     col = colours[CWsample$States])
legend(80,95, cex = 1, bty = "n", ncol=1, legend = States_full, x.intersp=0.5, seg.len=1.5, col = colours, lty=2, lwd=2, xpd = TRUE)
abline(modelA, lwd = 2)
for(j in 1:12){
  abline(a = beta0 + u[j], b = beta1, col = colours[j], lty=2, lwd=2) 
}

#LIKELIHOOD RATIO TEST
A_log = logLik(modelA) 
B_log = logLik(modelB)
chi = 2*(B_log - A_log)
p = 1-pchisq(chi, df=1)

#Equivalently
anova(modelB,modelA)

#FIGURE 6
par(mfrow = c(1,3))
boxplot(CWsample$AgeLastKill ~ CWsample$Male, cex.main=1.5, cex.lab=1.4, col=c("red", "green"), main = "Box plots of 'AgeLastKill' for male vs. \n female killers", xlab="Male" , ylab="Age of killer at last kill (years)")
boxplot(CWsample$AgeLastKill ~ CWsample$White, cex.main=1.5, cex.lab=1.4, col=c("blue", "pink"), main = "Box plots of 'AgeLastKill' for white vs. \n non-white killers", xlab="White", ylab="Age of killer at last kill (years)")
boxplot(CWsample$AgeLastKill ~ CWsample$Hetero, cex.main=1.5, cex.lab=1.4, col=c("purple", "orange"), main = "Box plots of 'AgeLastKill' for heterosexual vs. \n non-heterosexual killers", xlab="Heterosexual", ylab="Age of killer at last kill (years)")

#POTENTIAL COVARIATES
modelAm <- lm(AgeLastKill ~ Male, data = CWsample)
summary(modelAm)
modelAw <- lm(AgeLastKill ~ White, data = CWsample)
summary(modelAw)
modelAh <- lm(AgeLastKill ~ Hetero, data = CWsample)
summary(modelAh)

#RANDOM INTERCEPT MODEL WITH AN ADDITIONAL COVARIATE
modelC <- lmer(AgeLastKill ~ AgeFirstKill + White + (1 | State), data = CWsample, REML = FALSE)
summary(modelC)

#FIGURE 7
beta0 <- modelC@beta[1]
beta1 <- modelC@beta[2]
beta2 <- modelC@beta[3]
randomeffects <- ranef(modelC)
u <- randomeffects$State$"(Intercept)"

#Posterior variances:
str(attr(randomeffects$State, "postVar"))
v <- attr(randomeffects$State, "postVar")[1, 1, ]

group <- rownames(randomeffects$State)
# Create data frame:
level2 <- data.frame(group, u, v)
# Name the columns:
colnames(level2) <- c("Group", "Residual", "PostVar")
# Order by size of the residuals:
level2 <- level2[order(level2$Residual), ]
# Include a column containing the ranks:
level2 <- cbind(level2, 1:nrow(level2))
# Name the new column:
colnames(level2)[4] <- "Rank"
level2$Lower <- level2$Residual - qnorm(0.975)*sqrt(level2$PostVar)
level2$Upper <- level2$Residual + qnorm(0.975)*sqrt(level2$PostVar)

plot(level2$Rank, level2$Residual, 
     xlab = "Ranking", ylab = "Residual", 
     pch = 20, main = "Caterpillar plot", 
     ylim = c(min(level2$Lower), 1.5*max(level2$Upper)))
# Plot the credible intervals:
segments(level2$Rank, level2$Lower, level2$Rank, level2$Upper, lwd = 2)
abline(h = 0, lty=1, col="red", lwd=2)
# Add group names to the plot:
groupname = paste(level2$Group)
text(x = 1:12 , y = level2$Upper, labels = groupname, srt = 90,  adj = -0.5, cex = 1)

#FIGURE 8
par(mfrow = c(1, 1))
par(mfrow = c(1, 2)) 
qqnorm(resid(modelC), main="Normal QQ plot of level 1 residuals in model C")
qqline(resid(modelC), col=c("#33CC00"), lwd=2)
qqnorm(level2$Residual, main="Normal QQ plot of level 2 residuals in model C")
qqline(level2$Residual, col="#FF3399", lwd=2)

#CONFIDENCE INTERVALS ON RANDOM INTERCEPT MODELS
modelD <- lmer(AgeLastKill ~ White + Hetero + (1 | State), data = CWsample, REML = FALSE)
summary(modelD)

sigmahat_u0 <- 4.224
sigmahat_e0 <- 10.775
betahat <- matrix(modelD@beta, nrow = 3, ncol = 1) 

#WHAT ABOUT beta_1=beta_2=0? 5%
n <- nrow(CWsample)
intercept <- rep(1, n)
covariates <- CWsample[, c("White", "Hetero")]
X <- cbind(intercept, covariates)
X <- as.matrix(X)
I <- diag(n)
G <- matrix(NA, nrow = n, ncol = n)
for(h in 1:n){
  for(k in 1:n){
    
    G[h, k] <-  as.numeric(CWsample$State[h] == CWsample$State[k]) 
  }  
}
Sigmahat <- (sigmahat_e0^2)*I + (sigmahat_u0^2)*G 

q <- 2
d <- sqrt(qchisq(1 - 0.05, df = q))
C <- matrix(data = 0, nrow = 2, ncol = 3)
C[1, 2] <- 1
C[2, 3] <- 1
C
k = matrix(c(0,0), ncol = 1, nrow = 2)
k
V <- C %*% solve(t(X) %*% solve(Sigmahat) %*% X) %*% t(C)
D =  sqrt( t(C%*%betahat - k) %*% solve(V) %*% (C%*%betahat - k) )
D > d
D
d

#H_0 : beta_1+beta_2=0, 10% significance
C <- matrix(c(0,1,1), nrow = 1, ncol = 3)
r <- qnorm(1 - 0.1/2)
lower <- C %*% betahat - r * sqrt(C %*% solve(t(X) %*% solve(Sigmahat) %*% X) %*% t(C))
upper <- C %*% betahat + r * sqrt(C %*% solve(t(X) %*% solve(Sigmahat) %*% X) %*% t(C))
c(lower, upper)

#5% significance
C <- matrix(c(0,1,1), nrow = 1, ncol = 3)
r <- qnorm(1 - 0.05/2) 
lower <- C %*% betahat - r * sqrt(C %*% solve(t(X) %*% solve(Sigmahat) %*% X) %*% t(C))
upper <- C %*% betahat + r * sqrt(C %*% solve(t(X) %*% solve(Sigmahat) %*% X) %*% t(C))
c(lower, upper)

#H_0 : b_1-b_2=0, 10% significance level
r <- qnorm(1 - 0.1/2)
C <- matrix(c(0,1,-1), nrow = 1, ncol = 3)
l <- C %*% betahat - r * sqrt(C %*% solve(t(X) %*% solve(Sigmahat) %*% X) %*% t(C))
u <- C %*% betahat + r * sqrt(C %*% solve(t(X) %*% solve(Sigmahat) %*% X) %*% t(C))
c(l, u)

#5% significance level
r <- qnorm(1 - 0.05/2)
C <- matrix(c(0,1,-1), nrow = 1, ncol = 3)
l <- C %*% betahat - r * sqrt(C %*% solve(t(X) %*% solve(Sigmahat) %*% X) %*% t(C))
u <- C %*% betahat + r * sqrt(C %*% solve(t(X) %*% solve(Sigmahat) %*% X) %*% t(C))
c(l, u)


#RANDOM SLOPE MODEL
modelE <- lmer(AgeLastKill ~ AgeFirstKill + White + (1 + AgeFirstKill| State), data = CWsample, REML = FALSE)
#WARNING/ERROR MESSAGE
summary(modelE)

E_log = logLik(modelE) 
B_log = logLik(modelB)
chi = 2*(E_log - B_log)
p1 = 1 - pchisq(chi, 1)
p2 = 1 - pchisq(chi, 2)
p = (p1 + p2)/2

#FIGURE 9
beta0 <- modelE@beta[1]
beta1 <- modelE@beta[2]
beta2 <- modelE@beta[3]
randomeffects <- ranef(modelE)
u0 <- randomeffects$State$"(Intercept)"
u1 <- randomeffects$State$"AgeFirstKill"

plot(CWsample$AgeLastKill ~ CWsample$AgeFirstKill, 
     ylab = "Age last kill (years)", 
     xlab = "Age first kill (years)", 
     main = "Scatter plot of killer's age at first kill compared to their age at last kill, as categorised by their home state", 
     pch = 16,
     cex = 1.2,
     col = colours[CWsample$States])

legend(75,100, cex = 1, bty = "n", ncol=1, legend = States_full, x.intersp=0.2, seg.len=1.5, col = colours, lty=1, xpd = TRUE)
legend(75,20, bty="n", cex=1, legend = c("White", "Non-white"), x.intersp=0.2, seg.len=1.5, lty=c(1:2), xpd=TRUE)
for(j in 1:12){ 
  abline(a = beta0 + beta2 + u0[j], b = beta1 + u1[j], col = colours[j], lty = 2)
}
for(j in 1:12){ 
  abline(a = beta0 + u0[j], b = beta1 + u1[j], col = colours[j]) 
}

#FIGURE 10
randomeffects$State
str(attr(randomeffects$State, "postVar"))
attr(randomeffects$State, "postVar")
varu0 <- attr(randomeffects$State, "postVar")[1,1 , ]
varu1 <- attr(randomeffects$State, "postVar")[2,2 , ]

group <- rownames(randomeffects$State)
level2 <- data.frame(group, u0, u1, varu0, varu1)
colnames(level2) <- c("Group", "Intercept Residuals", "'AgeFirstKill' Residuals", "IntPostVar", "AFKPostVar")
level2


par(mfrow = c(1, 1))
par(mfrow = c(1, 2))
for(i in 1:2){ 
  
  # Order by the rankings of the effects in column i+1:
  
  level2 <- level2[order(level2[ , 1 + i]), ]
  
  # Calculate the credible intervals for the
  # effects in column i+1:
  
  Lower <- level2[ , i + 1] - qnorm(0.975)*sqrt(level2[ , i + 3])
  Upper <- level2[ , i + 1] + qnorm(0.975)*sqrt(level2[ , i + 3]) 
  
  # Plot the effects against the rankings:
  
  plot(1:12, level2[ , i + 1], pch = 20,  
       xlab = "Rank", ylab = "Residual", 
       main = names(level2)[i + 1],
       ylim = c(min(Lower), 1.5*max(Upper)))
  
  # Plot the credible intervals:
  
  segments(1:12, Lower, 1:12, Upper)
  
  abline(h = 0, col = "purple", lwd=2)
  
  groupname = paste(level2$Group)
  
  text(x = 1:12 , y = Upper, labels = groupname, srt = 90,  adj = 0, cex = 1)
  
}

#FIGURE 11
par(mfrow = c(1, 1))
qqnorm(u1, main="Normal Q-Q Plot for level 2 residuals in model E")
qqline(u1, col=c("red"), lwd=2)


#ERRORS
modelF <- lmer(AgeLastKill ~ AgeFirstKill + White + (1 + AgeFirstKill + White| State), data = CWsample, REML = FALSE)
summary(modelF)

CWsample$AgeFirstKilldecades <- CWsample$AgeFirstKill/10
modelG <- lmer(AgeLastKill ~ AgeFirstKill + White + (1 + AgeFirstKilldecades + White | State), data = CWsample, REML = FALSE)
summary(modelG)
