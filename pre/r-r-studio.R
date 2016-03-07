## simple calculations
1 + 1

1 + 2 + 4

2*4

2^3

log(10)

exp(1)

1/7
## assignment

x <- 1.2
1.3 -> x1
y <- 'A'
z <- TRUE
w <- factor(c('A','B','A'))

## vector with numbers
x<-c(1,2,3)
(x<-c(1,2,3))

## vector with letters
(y<-c("a","b","c"))

## vector with logicals
(z1<-c(TRUE,TRUE,FALSE))
(z2<-c(T,T,F)) # same

# vector type
class(x)
class(y)
class(z1)
## for matricies
mode(x)


######################################
# few useful commands

# make an vector
(x1<-seq(1,10))
# the same
(x1<-1:10)
(x2<-seq(0,1,length=10))

rep(1,10)
rep(c(1,2,3),2)
rep(c(1,2,3),each=2)

# length of a vector
length(x1)

(x3<-c(6,3,8,9,4))
# sort the elements of the vector
sort(x3)
# find the rank of each element
rank(x3)
# gives an index for reordering
order(x3)

# join two vectors
c(x1,x2)
cbind(x1,x2)
rbind(x1,x2)

x <- rnorm(100)

mean(x)
median(x)
quantile(x)

######################################
# vector indices
x4<-c(4,7,5,3,4,8,9)
# first element of x4
x4[1]
x3[5]
# first and fourth
x4[c(1,4)]
# use logicals
x4[x4>=7]
x4[x4 != 3]
# indices that satify a condition
which(x4>=7)
which(x4!=3)   # != not equal to
which(x4==3)   # == equal to

# get everything except the first element
x4[-1]
# get everything except the first element and second element
x4[-c(1,2)]

######################################
# WARNING -- repetions

(y1<-c(1,2,3,4))
(y2<-c(1,2,3))
(y3<-c(1,2))

y1+y2
y1+y3   #!
y1 + 1

y1*2
y1*y2
y1*y3  #!

######################################
# matricies

(X<-matrix(1:10,ncol=2,byrow=T))
(Y<-matrix(1:10,ncol=2,byrow=F))
# single element
X[1,2]
# get the second column
X[,2]
# get the third row
X[3,]
# matrix size
dim(X)
# we can name them
rownames(Y)<-c("ry1","ry2","ry3","ry4","ry5")
colnames(Y)<-c("dy1","dy2")
Y
# and call them by name
Y["ry2","dy2"]

# transpose
t(X)
# elementwise multiplication
X*Y
# normal matrix multiplication
X%*%t(Y)

(Z<-matrix(1:4,ncol=2))
# determinant
det(Z)
# inverse
solve(Z)

## Dataframes
## lets look at the minke dataset
minke <- read.csv2('minke.csv')

########################################
# useful stuff

# how does this dataset look like
head(minke,n=10)
names(minke)
dim(minke)
str(minke)

# use $ to get a column in the data.frame
minke$length[3]
minke[1,3]
minke[3,1]

## plots

plot(minke$age,minke$length)
hist(minke$length)

## list all variables in the environment
ls()

# remove x from the environment
rm(x)
ls()

## remove everything
rm(list=ls())


## if - else - then
x <- 10
## check if x is greater than 0
if(x>0){
  print('x is positive')
}

x <- 10

## check if x > 10
if(x>10){
  print('x is greater than 10')
} else if(x>=0){
  print('x is less than 10 but greater than or equal to 0')
} else {
  print('x is negative')
}

## ifelse
x <- c(1,2,3,NA,4,5)
## find all missing entries and replace them
x <- ifelse(is.na(x),0,x)

## loops
## find the number of entries in the data
n <- nrow(minke)

## calculate the mean length old fashion way
ML <- 0
for(i in 1:n){
  ML <- ML + dat$length[i]
}
ML <- ML/n


## functions
add1 <- function(x){
  x <- x + 1
  return(x)
}

add1(10)
x
