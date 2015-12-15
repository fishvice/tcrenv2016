# TCRENV 2016




This space will host the nitty gritty part of the upcoming ICES training course in R.

* Information on registration can be found on the [ICES webpages](http://ices.dk/news-and-events/Training/Pages/R-environment.aspx)
* The introctuction to the course was provided as [“ICES news”](http://ices.dk/news-and-events/news-archive/news/Pages/Learning-the-language-of-R.aspx)

## Dates and venues
___

* 2016-02-29 09:00 Monday - 2016-03-05 16:00 Friday
* ICES HQ, Copenhagen, Denmark

## A tentative schedule
___

Tentative here means that the schedule will be subject to changes, but to only a minor extent the context. 

### Day 1 - Monday

* Introduction:
    - Rstudio and R projects
    - knitr, markdown, document writing (html, pdf,  docx)
    - reproducible analysis using R
* Getting data into R and out of R:
    - from ones own computer (text files, excel, ...)
    - from the web
    - from API's (ICES Webservices)
    - from databases (Ram's online Postgres database)
* The grammar of data and graphics:
    - Introduction to ggplot2
    - Introduction to dplyr

### Day 2 - Tuesday

* The grammar of data and graphics - continued:
    - Exploratory data analysis and visualization
* Working with characters and dates
* The base R equivalence

### Day 3 - Wednesday

* GIS in R
    - using ggplot2 and ggmap
    - using leaflet
* Applied project(s) - From "messy" data to a final report using reproducible approach based on case examples
    - candidate: abundance and biomass indices starting with ICES DATRAS data

### Day 4 - Thursday

* Applied project - continued

### Day 5 - Friday
* The fundamentals of functions and package writing
    - Fundamental of functions and documentation
    - Directory structure and a minimal example
    - Version control (git) and social coding (www.github.com)

> Daily routine: Each day will be split up into group discussion of the topics/assignments covered the previous day, introduction lectures of the day's topics followed by practical assignments. Emphasis will be put on cooperative work and code sharing (including difficulties/stumbling blocks) among participants.

## Some simple examples of what we will cover

### ICES stocks - mortality trends


```r
# devtools::install_github("einarhjorleifsson\wices")
library(wices)
library(dplyr)
library(tidyr)
library(reshape2)
library(RColorBrewer)
library(ggplot2)
library(ggmap)
```

Get list of stock names available in year 2015 and create a loop to get all the stock summary data from ices.dk via the [ICES standard graph webservice API](http://standardgraphs.ices.dk/standardgraphswebservices.asmx):

```r
stocks <- wices::get_list_stock(year = 2015) %>% 
  filter(Status == "Published",
         !FishStockName %in% c("lin-icel","tur-nsea"),
         SpeciesName != "Mallotus villosus")

rby <- NULL
for (i in c(1:nrow(stocks))) {
  x <- wices::get_summary_table(stocks$FishStockName[i], year = 2015)
  rby <- bind_rows(rby,x)
}
```

Some minor "cleaning":

```r
rby <- 
  rby %>% 
  filter(Year <= 2015) %>% 
  mutate(F = ifelse(Year == 2015, NA, F)) %>% 
  left_join(stocks)
```





A "glimpse" of what the table looks like:

```r
rby
```

```
## Source: local data frame [2,981 x 22]
## 
##     Year recruitment high_recruitment low_recruitment low_SSB   SSB
##    (int)       (dbl)            (dbl)           (dbl)   (dbl) (dbl)
## 1   1980           0               NA              NA    1.20  1.20
## 2   1981           0               NA              NA    1.18  1.18
## 3   1982           0               NA              NA    1.15  1.15
## 4   1983           0               NA              NA    1.11  1.11
## 5   1984           0               NA              NA    1.08  1.08
## 6   1985           0               NA              NA    1.07  1.07
## 7   1986           0               NA              NA    1.06  1.07
## 8   1987           0               NA              NA    1.02  1.02
## 9   1988           0               NA              NA    0.90  0.92
## 10  1989           0               NA              NA    0.78  0.81
## ..   ...         ...              ...             ...     ...   ...
## Variables not shown: high_SSB (dbl), catches (dbl), landings (dbl),
##   discards (dbl), low_F (dbl), F (dbl), high_F (dbl), fishstock (chr),
##   AssessmentYear (int), key (int), .id (chr), FishStockName (chr),
##   StockDescription (chr), Status (chr), SpeciesName (chr), EcoRegion (chr).
```

A boxplot of mortality trends with individual values superimposed:

```r
rby %>% 
  ggplot(aes(factor(Year), F)) +
  theme_bw() +
  geom_boxplot(fill = "blue", alpha = 0.4) +
  geom_jitter(col = "red", size = 2, alpha = 0.3) +
  coord_cartesian(ylim = c(0,1)) +
  scale_x_discrete(breaks = seq(1950, 2010, by = 10)) +
  labs(x = NULL, y = "Fishing mortality")
```

![](README_files/figure-html/ices_fmort-1.png) 

Trends by "major" species:

```r
rby %>% 
  filter(SpeciesName %in% c("Clupea harengus",
                            "Gadus morhua",
                            "Melanogrammus aeglefinus",
                            "Pleuronectes platessa",
                            "Pollachius virens",
                            "Solea solea")) %>% 
  ggplot(aes(Year, F)) +
  theme_bw() +
  geom_hline(yintercept = 0.2, col = "yellow") +
  geom_line(aes(group = FishStockName), col = "red", alpha = 0.3) +
  geom_point(alpha = 0.2, col = "red") +
  stat_smooth() +
  facet_wrap(~ SpeciesName, scale = "free_y") +
  coord_cartesian(ylim = c(0,1.5)) +
  labs(x = NULL, y = NULL, title = "Fishing mortality trends in different species")
```

![](README_files/figure-html/ices_fmort_mspecies-1.png) 

### Survey indices at age "brought to live"


```r
PAIRED <- rep(brewer.pal(12,"Paired"),100)
survey <- readr::read_csv("http://data.hafro.is/assmt/2015/cod/smb.csv") %>% 
  gather(age, index, -Year) %>% 
  mutate(age = as.integer(as.character(age)),
         yc = Year - age) %>% 
  filter(age %in% 2:10)
```

A "glimpse" of the table:

```r
survey
```

```
## Source: local data frame [279 x 4]
## 
##     Year   age  index    yc
##    (int) (int)  (dbl) (int)
## 1   1985     2 110.48  1983
## 2   1986     2  60.58  1984
## 3   1987     2  28.29  1985
## 4   1988     2   7.06  1986
## 5   1989     2  16.40  1987
## 6   1990     2  11.79  1988
## 7   1991     2  16.02  1989
## 8   1992     2  16.91  1990
## 9   1993     2   4.77  1991
## 10  1994     2  14.96  1992
## ..   ...   ...    ...   ...
```

Making it colourful as well as graphical:

```r
n <- length(unique(survey$yc))
ggplot(survey,aes(Year,index,fill=factor(yc))) +
  theme_bw() +
  geom_bar(stat="identity") + 
  scale_fill_manual(values=PAIRED[1:n])  + 
  theme(legend.position = "none") +
  labs(X = NULL, y = NULL, title = "iCod: standardized spring survey indices by age. Colour indicate cohorts") +
  facet_grid(age ~ ., scale="free_y") +
  scale_y_continuous(breaks = c(3)) +
  scale_x_continuous(breaks = seq(1985, 2015, by = 5))
```

![](README_files/figure-html/crayola-1.png) 

### Shaking hands with DATRAS


```r
st <- wices::get_hh_data(survey = "NS-IBTS",
                         year = 2015,
                         quarter = 1) %>% 
  select(shootlat, shootlong, haullat, haullong)
```

```
## xmlns: URI ices.dk.local/DATRAS is not absolute
```

```r
st$id <- 1:nrow(st)
p <- get_map(location = c(mean(st$shootlong), mean(st$shootlat)),
             zoom = 6,
             maptype = "satellite")
ggmap(p) +
  geom_segment(data = st, aes(x = shootlong,
                              xend = haullong,
                              y = shootlat,
                              yend = haullat),
               color = "white") +
  labs(x = NULL, y = NULL)
```

![](README_files/figure-html/nsibts-1.png) 

### More examples may be added ...

## Context, objective and level
___


### Context

The R language is becoming the _Lingua franca_ both in data science in general as well as within the ICES community. Recent advancements within R have resulted in that R can no longer be considered as a specific statistical programming language but as a general scientific working environment. This broader environment has resulted in the R has become a natural component of reproducible data analysis and document writing.

Various R packages (e.g. FLR, DATRAS, MSY, SURBAR, VMStools) have often been the backbone of ICES training course and/or workshops. These packages as well as courses are geared towards solving specific pending tasks that tend to come with requirements that the participants are reasonable proficient in basic R and that the input data are correctly formatted and available. Any of these requirements have been seen to pose problems.

The course is aimed at covering the fundamental/generic basis of the grammar of data and graphics as well reproducible document writing where R is used as the sole working medium. Recent developments in the R community that are of interest to fisheries science will also be described.

### Objective

The objective of the course is to provide participants with a solid foundation in efficient use of the R environment using various typical and familiar fisheries data sets (landings data, catch data, survey data and tagging data) as case examples. Emphasis will be put on data munging and literate programming starting with "raw" data (individual stations, individual fish measurements) and culminating with deliverance of publishable output produced from a single coded document file.

By the end of the course, the participants:

  * Will be able to import data from multitude of sources computer (i.e. own text files, excel, access, sql databases) and via the web.
  * Will be able to clean, manipulate, explore, summarize and graph data. This includes being able to:
      - Apply best practices in data preparation
      - Present results graphically, highlighting significant results
      - Merge, slice and dice various datasets
  * Will be able to apply the principle of reproducible analysis and report writing from A through Z which are then deliverable through any of the current three common deliverable formats: .html, .pdf and .docx.
  * Will be able to produce own functions and understand the principles of creating R packages as well participate in social coding (through www.github.com).

### Level

The course is targeted at fisheries scientist with already have some basic experience in R but are yet not proficient enough to write fluently code for data manipulation, exploration and writing own functions. We believe that some part of the course would also be beneficial to those that are currently productively using R in fisheries science but may along the way have skipped the basics or are unaware of recent advancements in the R environment.


## Lecturers
___


  * Bjarki Þór Elvarsson, Marine Research Institute, Iceland
      - Bjarki is a statistician in the Fisheries Advisory Section of the Marine Research Institute, Reykjavík Iceland. He recently finished his phd in statistical methods related to stock assessment models. He has been a member of the Icelandic delegation to the scientific council of International whaling commission since 2010 and participated in various ICES working group meetings since 2013. R has been in his main working environment since 2004. 
  * Einar Hjörleifsson, Marine Research Institute, Iceland
      - Einar is a fisheries scientist in Fisheries Advisory Section of the Marine Research Institute, Reykjavík Iceland. He has been involved in various ICES works since 1996 that spans the whole spectrum from ACFM/ACOM membership up to working group participation. In the early 2000's he taught stock assessment at ICES with Dankert Skagen for three consequtive years. He has also been involved in the United Nation University Fisheries Training Program teaching stock assessment. R has been his primary working environment since 2009.
      
