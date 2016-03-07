library(dplyr)
library(tidyr)

## one can simply enter data directly to the console
weight <- c(1,5,3,2,6)
length <- c(10,17,14,12,18)
plot(length,weight)

## or be more clever
dat <- data.frame(id=numeric(0), species=character(0),
                  length=numeric(0),age=numeric(0),
                  lat = numeric(0),lon=numeric(0))
dat <- edit(dat)

## read in the minke data properly
minke <-
  read.table(file = 'data/minke.csv', ## path to the file
             header = TRUE,       ## are column names
             ## at the top
             dec = '.',           ## decimal sign
             sep = ',',           ## column separator symbol
             skip = 0,            ## num lines at top to skip
             stringsAsFactors = FALSE,
             comment.char = '#')  ## indicating comments

## or you can use
minke <- read.csv('data/minke.csv')

## more low level:
minke.lines <- readLines('data/minke.csv')

## read the header in using scan
header <- scan(text = minke.lines[4],
               what = 'character',sep=',')

minke <- read.table(text = minke.lines[-c(1:4)],
                    sep = ',',dec='.')
## use the header read with scan to name the
## columns
names(minke) <- header

## simple sanity checks
head(minke)
tail(minke)
dim(minke)
names(minke)
summary(minke)
str(minke)

## one can write the data to file
write.table(minke,
            file = 'minke-class.csv', ## file name
            col.names = TRUE,    ## write header
            row.names = FALSE,   ## write row names
            quote = FALSE,       ## characters qouted?
            sep = ',',
            dec = '.')
## or simply
write.csv(minke,file='minke.csv')

## location of files
minke <-
  read.csv('http://www.hafro.is/~bthe/minke.csv')

catatage <-
  read.csv('http://data.hafro.is/assmt/2015/cod/catage.csv')

## excel files
library(readxl)

minke.xls <- read_excel("minke.xlsx")

## write excel files
library(openxlsx)

write.xlsx(minke,file='minke.xlsx')

fish <- read.csv2('lec1/fish.csv')

write.xlsx(list("minke"=minke,'fish'=fish),file='minkeandfish.xlsx')

## list sheets
excel_sheets('minkeandfish.xlsx')

## read in only the fish data
fish.xls <- read_excel('minkeandfish.xlsx',sheet = 'fish')

## database connectivity
library(dplyr)

## setup a connection to a database
## by creating it
db <- src_sqlite('minke.db',create = TRUE)

## dump data into it
tmp <- copy_to(db,minke,'minke_tbl')

## query the database
minke.tbl <- tbl(db,'minke_tbl')

## run sql on the database
num.minke <- tbl(db,sql('select count(1) from minke_tbl'))


## describe the table (analogous to str in R)
tbl_vars(minke.tbl)
glimpse(minke.tbl)

## list all tables in a data base
db_list_tables(db$con)

## for more db stuff refer to
?db_list_tables
