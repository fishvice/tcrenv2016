
###################################

## One can create a subset of the data using the \texttt{filter} command:
## load dplyr
library(dplyr)
## create a dataset with only females
minke.females <-
  filter(minke,sex=='Female')


## or non-pregnant
minke.nonpregnant <-
  filter(minke,sex=='Female', maturity != 'pregnant')

## select id, age, length and sex
minke.redux <-
  select(minke,whale.id,age,length,sex)

## select all column except stomach volume and weight
minke.noStom <-
  select(minke,-contains('stomach'))

## select id and all column between length and sex
minke.min <-
  select(minke,c(whale.id,length:sex))

## arrange by length (ascending)
minke.lasc <-
  arrange(minke,length)

## arrange by length (descending)
minke.ldesc <-
  arrange(minke,desc(length))

## arrange by age and then length (ascending)
minke.alasc <-
  arrange(minke,age,length)

## add new column old style
minke$approx.weight <- 3.85*1e-06*minke$length^3.163

## add new column using mutate
minke <-
  mutate(minke,
         approx.weight = 3.85*1e-06*length^3.163)

## not really useful until you add more than one
minke <-
  mutate(minke,
         approx.weight = 3.85*1e-06*length^3.163,
         adj.weight = ifelse(is.na(weight),approx.weight,
                             weight))
## create summaries
summarise(minke,num.obs = n())

## you can have as many as you like
summarise(minke,
          num.obs = n(),
          num.fem = sum(sex == 'Female'),
          num.large = sum(length > 650))

## But not particularly interesting until one can group the data
minke.NS <- group_by(minke,area)
summarise(minke.NS,
          num.obs = n(),
          num.fem = sum(sex == 'Female'),
          num.large = sum(length > 650))


## this is the same as splitting things up
minke.N <- filter(minke,area=='North')
summarise(minke.N,
          num.obs = n(),
          num.fem = sum(sex == 'Female'),
          num.large = sum(length > 650))


minke.S <- filter(minke,area=='South')
summarise(minke.S,
          num.obs = n(),
          num.fem = sum(sex == 'Female'),
          num.large = sum(length > 650))


## reshape data
## read in data in a wide format and convert to long format
catatage <-
  read.csv('http://data.hafro.is/assmt/2015/cod/catage.csv')

head(catatage)

catlong <-
  gather(catatage,age,number,-Year)

head(catlong)

## long to wide
minke.y.a <- group_by(minke,year,area)
num.minke.y.a <- summarise(minke.y.a,num=n())

spread(num.minke.y.a,year,num)


## separate
minke.by.day <-
  separate(minke,date.caught,c('yr','m','d'),sep='-')

## combine
minke.m.s <-
  unite(minke,mat.sex,c(sex,maturity),sep='-')


## chaining operations together
summarise(
  group_by(
    filter(minke,!is.na(weight)),
    sex),
  num.whale=n(),
  m.weight = mean(weight)
)


## using the %>% operator
minke %>%
  filter(!is.na(weight)) %>%
  group_by(sex) %>%
  summarise(num.whale = n(),
            m.weight = mean(weight))

## lets do more
minke %>%
  filter(area == 'North') %>%
  group_by(maturity) %>%
  summarise(n=n(),
            num.na = sum(is.na(age)),
            m.age = mean(age,na.rm=TRUE),
            sd.age = sd(age,na.rm=TRUE)) %>%
  mutate(prop = n/sum(n)) %>%
  arrange(prop)

## play with ungroup
minke %>%
  group_by(maturity) %>%
  mutate(lnorm.in = length/mean(length)) %>%
  ungroup() %>%
  mutate(lnorm.out = length/mean(length)) %>%
  select(whale.id,maturity,lnorm.in,lnorm.out)


## class excercise

## num caught by year

minke %>%
  group_by(year) %>%
  summarise(num.caught = n())

## num which are females

minke %>%
  group_by(year) %>%
  summarise(num.caught = n(),
            num.fem = sum(sex=='Female'),
            prop.fem = mean(sex=='Female')) %>%
  mutate(prop2 = num.fem/num.caught)

## mean age and sd by maturity

minke %>%
  group_by(maturity) %>%
  summarise(ml = mean(length),
            sdl = sd(length),
            ma = mean(age,na.rm=TRUE),
            sda = sd(age,na.rm=TRUE))


##

minke %>%
  group_by(year,area) %>%
  summarise(num = n()) %>%
  spread(year,num)


