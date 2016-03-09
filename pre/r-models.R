library(dplyr)
library(ggplot2)
library(broom)

minke <- read.csv('http://www.hafro.is/~bthe/minke.csv',stringsAsFactors = FALSE)




wlFun <- function(dat,a,b){
  return(list(p=ggplot(dat,aes(log(length),log(weight))) +
                geom_point() +
                geom_abline(intercept=log(a),slope=b)+
                ylim(c(0,10)) + xlim(c(0,10)),
              ss=sum((log(dat$weight) - log(a) -
                        b*log(dat$length))^2)))
}


minke %>% filter(!is.na(weight)) %>% wlFun(exp(-11.5),3)



cod.isl <- 
  all.kv %>% 
  filter(tegund == 1, !is.na(oslaegt)) %>% 
  sample_n(1e3) %>% 
  select(sample.id = synis.id, length=lengd,
         age = aldur, sex = kyn, maturity = kynthroski, 
         ungutted = oslaegt, gutted = slaegt, liver=lifur, gonad = kynfaeri) %>% 
  mutate(sex = factor(sex,labels=c('Males','Females')),
         maturity = ordered(maturity))


fit.cod <- lm(log(ungutted)~log(length)*sex*maturity,cod.isl) %>% 
  step()



ca.dat <-  get_datras(record = 'CA') 

cod.ca <- 
  filter(ca.dat,speccode=='126436') %>%
  mutate(length = ifelse(lngtcode=='.',0.1*lngtclass,lngtclass),
         weight = indwgt*1e-3,
         maturity = ifelse(maturity>10,maturity-60,maturity),
         maturity = factor(maturity,labels = c('Juvenile','Maturing','Spawning','Spent','Resting','Abnormal')))




lm(log(weight)~log(length)*maturity*sex,cod.ca) %>% tidy() #drop1(test='Chisq')  #augment() %>% tbl_df()

glm(weight~log(length)*maturity*sex,data=cod.ca,family=gaussian(log))%>% summary() #drop1(test='Chisq')  #augment() %>% tbl_df()



nls(length~Linf*(1-exp(-K*(age-t0))),
    data=cod.ca, start=list(Linf=110, K=0.1, t0=-1)) %>% 
  tidy()


