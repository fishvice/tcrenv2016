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

Read table directly from the net:

```r
survey <- readr::read_csv("http://data.hafro.is/assmt/2015/cod/smb.csv")
```

A "glimpse" of the table:

```r
survey
```

```
## Source: local data frame [31 x 11]
## 
##     Year     1      2      3      4     5     6     7     8     9    10
##    (int) (dbl)  (dbl)  (dbl)  (dbl) (dbl) (dbl) (dbl) (dbl) (dbl) (dbl)
## 1   1985 16.54 110.48  35.41  48.25 64.59 22.95 15.26  5.04  3.39  1.84
## 2   1986 15.07  60.58  95.95  22.46 21.51 27.44  7.17  2.80  0.93  0.82
## 3   1987  3.65  28.29 104.44  82.67 21.41 12.76 12.94  2.79  0.98  0.42
## 4   1988  3.45   7.06  72.51 103.56 69.54  8.39  6.41  7.23  0.67  0.28
## 5   1989  4.04  16.40  22.06  79.90 74.16 39.11  4.85  1.71  1.42  0.27
## 6   1990  5.56  11.79  26.10  14.18 27.91 35.22 16.74  1.75  0.58  0.48
## 7   1991  3.95  16.02  18.20  30.24 15.49 18.94 22.45  4.91  0.94  0.31
## 8   1992  0.71  16.91  33.60  18.95 16.66  6.87  6.35  5.78  1.49  0.23
## 9   1993  3.57   4.77  30.87  36.79 13.53 10.61  2.42  2.03  1.40  0.41
## 10  1994 14.40  14.96   9.04  26.91 22.43  6.09  3.96  0.80  0.53  0.52
## ..   ...   ...    ...    ...    ...   ...   ...   ...   ...   ...   ...
```


Making it graphical and "informical":

```r
survey <-
  survey %>% 
  gather(age, index, -Year) %>% 
  mutate(age = as.integer(as.character(age)),
         yc = Year - age) %>% 
  filter(age %in% 2:10)

PAIRED <- rep(brewer.pal(12,"Paired"),100)
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

_The graph above contain a lot of detail stories. The bottom line, that explains the increase in the abundance indices of older fish with time, despite no increase in recruitment abuncance, is management action in reducing fishing mortality over this time period through TAC control_

### Shaking hands with [DATRAS](http://ices.dk/marine-data/data-portals/Pages/DATRAS.aspx)

Get station information ("hh" data) for "NS-IBTS", first quarter in 2015 via the ICES DATRAS webservice API and plot each tow trajectories:

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
```

Take a peek at the data:

```r
head(st)
```

```
##   shootlat shootlong haullat haullong id
## 1   57.536    -1.281  57.515   -1.306  1
## 2   57.806    -0.893  57.776   -0.902  2
## 3   58.046    -0.983  58.019   -0.961  3
## 4   58.209    -3.011  58.187   -3.047  4
## 5   58.087    -2.919  58.065   -2.952  5
## 6   57.864    -3.055  57.863   -3.109  6
```

Plot the stuff:

```r
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
      
