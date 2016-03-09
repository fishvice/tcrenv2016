library(ggplot2)
library(dplyr)
cod.ca <- read.csv("C:/Users/atlantic/Documents/example-ca.csv")

ggplot(cod.ca,aes(length,ungutted)) + geom_point()

ggplot(cod.ca,aes(log(length),log(ungutted))) + geom_point()

wlFun <- function(dat,a,b){
  
  ## plot the prediction and the data together
  p <- ggplot(dat,aes(log(length),log(ungutted))) +
    geom_point() +
    geom_abline(intercept=log(a),slope=b)+
    ylim(c(0,5)) + xlim(c(0,5))
  
  ## calculate the distance
  ss <- sum((log(dat$ungutted) - log(a) -
               b*log(dat$length))^2)

  return(list(p=p,ss=ss))
}

wlFun(cod.ca,0.005,3.2)

## fit a model
fit <- lm(log(ungutted)~log(length),cod.ca)
summary(fit)
coefficients(fit)

fit.2 <- lm(log(ungutted)~log(length)*sex+maturity,cod.ca)
summary(fit.2)
coefficients(fit.2)

# parameter selection
AIC(fit.2)
step(fit.2)

fit.3 <- lm(log(ungutted)~.+log(length)*sex,cod.ca)
step(fit.3)


# estimate a maturity ogive
library(broom)
cod.ca <- 
  mutate(cod.ca,
         mat.1 = ifelse(maturity==1,0,1)) %>% 
  filter(!is.na(mat.1))

fit.mat <- 
  glm(mat.1~length,family = 'binomial',data=cod.ca)
# gives the coefficents and p.values
tidy(fit.mat)
# this here gives us the model variables and predictions (.fitted)
output <- augment(fit.mat,type.predict = 'response')
# maturity ogive
ggplot(output,aes(length,.fitted)) + geom_line() + 
  geom_jitter(data=cod.ca,aes(length,mat.1),alpha=0.05,height = 0.1) +
  geom_vline(xintercept = 64.70074)

# Let's find L50
output %>% 
  filter(.fitted <= 0.50) %>% 
  summarise(L50=max(length))
## or more robust way
library(MASS)
dose.p(fit.mat, cf = 1:2, p = 0.5)

## estimate a vonB 
vonB.par <-
  nls(length~Linf*(1-exp(-K*(age-t0))),
      data=cod.ca, start=list(Linf=110, K=0.1, t0=-1))

confint(vonB.par) # confidence intervals
tidy(vonB.par)    # parameter estimates and p.values
vonB.pred <- augment(vonB.par) # prediction from the model
## plot
ggplot(vonB.pred,aes(age,length)) + 
  geom_point() + # or geom_jitter() + 
  geom_line(aes(y=.fitted))
# residual vs. age
ggplot(vonB.pred,aes(age,.resid)) + geom_point()
