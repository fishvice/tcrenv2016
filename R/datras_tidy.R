tidy_station <- function(x) {
  # unique station id
  x <-
    x %>% 
    dplyr::mutate(id = paste(year, quarter, country, ship, gear, stno, haulno,sep="-"))
  
  # proper date/time format
  x <- 
    x %>% 
    dplyr::mutate(timeshot = paste0(substr(timeshot,1,2),":",substr(timeshot,3,4)),
           date = lubridate::ymd_hm(paste(year,month,day,timeshot)),
           datehaul = date + 60 * hauldur)
  
}

tidy_length <- function(x, st) {
  d <-
    x %>% 
    dplyr::mutate(id = paste(year, quarter, country, ship, gear, stno, haulno,sep="-")) %>% 
    dplyr::select(-(recordtype:year))
  
  # standardize/recode length
  d <- 
    d %>% 
    dplyr::mutate(lngtclass = as.numeric(lngtclass),
                  lngtclass = ifelse(lngtcode %in% c(".","0"),
                              0.1 * lngtclass,
                              lngtclass)) %>% 
    dplyr::select(-lngtcode)
  
  # reconstruct the original count-variable in the length table
  #  has been standaridized to 1 hour if datatype field in
  #  the station table is labelled C (R is no standardization 
  #  has been done)
  
  # and then do the raising
  d <- 
    d %>% 
    dplyr::left_join(st %>% dplyr::select(id, datatype, hauldur)) %>% 
    dplyr::mutate(r = ifelse(datatype == "C", hauldur/60, subfactor),
           n = r * hlnoatlngt) %>% 
    dplyr::select(-r, -datatype, -hauldur)                  # remove the temporary columns
  
  
  # get latin names and return only needed columns
  #d <- 
  #  d %>% 
  #  dplyr::left_join(species) %>% 
  #  dplyr::select(id, latin, sex, length = lngtclass, n)

}


