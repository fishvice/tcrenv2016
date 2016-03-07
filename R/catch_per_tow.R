catch_per_tow <- function(Year, Species) {
  st <-
    Station %>% 
    filter(year %in% Year) %>% 
    select(id, year, towlength, lon, lat) 
  d <-
    Length %>% 
    filter(id %in% st$id,             # this ensures we only work with SMB length 
           species %in% Species) %>% 
    select(id, length, n) %>% 
    left_join(Subsampling %>%
                filter(species %in% Species) %>%
                mutate(r = n.total/n.measured,
                       r = ifelse(is.na(r), 1, r)) %>% 
                select(id, r), by =  c("id")) %>% 
    mutate(n = n * r,
           wt = (n * 0.01 * length^3)/1000) %>%
    group_by(id) %>%
    summarize(n = sum(n),
              wt = sum(wt)) %>% 
    right_join(st, by = c("id")) %>%
    mutate(n = (ifelse(is.na(n), 0, n)/1e3)/towlength * 4,
           wt = (ifelse(is.na(wt), 0, wt)/1e3)/towlength * 4)
  
  return(d)
}