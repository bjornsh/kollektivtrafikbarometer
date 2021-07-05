
##################################################################################
### Script description
##################################################################################

# Get distance estimates for Kollektivtrafikbarometer RVU joureys based on 
# Google Directions API




##################################################################################
### Set up
##################################################################################

### clean start
rm(list = ls())
gc()


### load libraries etc
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, writexl, gmapsdistance)


# avoid scientific notation
options(scipen=999)


# function to get specific date (monday = 2) 
nextweekday <- function(date, wday) {
  date <- as.Date(date)
  diff <- wday - wday(date)
  if( diff < 0 )
    diff <- diff + 7
  return(date + diff)
}

`%notin%` <- Negate(`%in%`)

### Read keys, define paths 
api_fil = read_file("Z:/api")
google_api_direction <- gsub('^.*google_direction: \\s*|\\s*\r.*$', "", api_fil)

folder_output = paste0(getwd(), "/data/output/")



##################################################################################
### Define variables etc
##################################################################################

set.api.key(google_api_direction)

# List all months to be included
datum = c("202105") # c("YYYYMM", "YYYYMM")

# get date for next monday (departure date)
next_monday = as.character(nextweekday(Sys.Date(), 2))

# define departure time
dep_time = "07:30:00"




##################################################################################
### Load and prepare Kollbar data
##################################################################################


rvu_restid_exist = read.csv2(paste0(folder_output, "rvu_restider.csv"))
pers = read.csv2(paste0(folder_output, "person.csv"))
rvu = read.csv2(paste0(folder_output, "rvu.csv"))


# identify journeys from specified period
include = pers %>% 
  filter(ar.manad %in% datum) %>% 
  dplyr::select(respondentid) %>% 
  pull()


# include journeys within specified period and 
# exclude journeys with existing API data
if (file.exists(paste0(folder_output, "rvu_restider.csv"))){
  rvu_restid_exist = read.csv2(paste0(folder_output, "rvu_restider.csv"))
  
  exclude = rvu_restid_exist %>% 
    # create unique journey identifier
    mutate(concat = paste(respondentid, resa_nr)) %>% 
    dplyr::select(concat) %>% 
    pull()
  
  # remove data outside specified time period & 
  # for which API data already exists
  rvu = rvu %>% 
    mutate(concat = paste(respondentid, resa_nr)) %>% 
    filter(respondentid %in% include) %>% 
    filter(concat %notin% exclude)
} else {
  rvu = rvu %>% 
    mutate(concat = paste(respondentid, resa_nr)) %>% 
    filter(respondentid %in% include)
}





### prepare variable needed for API call
rvu = rvu %>% 
  mutate(start_koord_wgs84 = paste(b3_lat, b3_lng, sep = ","),
         stop_koord_wgs84 = paste(b9_lat, b9_lng, sep = ","))


### prepare subsets
google_df = rvu %>% 
  filter(!is.na(b3_lat) & !is.na(b9_lat)) %>% 
  dplyr::select(respondentid, 
                resa_nr, 
                fardmedel.kat, 
                start_koord_wgs84, 
                stop_koord_wgs84)


bil_koll = filter(google_df, fardmedel.kat == "Bil" |
                         str_detect(fardmedel.kat, "Kollektiv*")) 

walk = filter(google_df, fardmedel.kat == "Gång") 

cykel = filter(google_df, fardmedel.kat == "Cykel") 



##################################################################################
### ONLY FOR TESTING
##################################################################################

# create subset
# bil_koll = bil_koll[1:50,]
# walk = walk[1:50,]
# cykel = cykel[1:50,]


##################################################################################
### API körning
##################################################################################

### bil
google_bil = list()

for(i in 1:nrow(bil_koll)){
google_bil[[i]] = gmapsdistance(bil_koll$start_koord_wgs84[i], 
                                bil_koll$stop_koord_wgs84[i],
                                dep_date = next_monday,
                                dep_time = dep_time,
                                combinations="pairwise",
                                mode="driving",
                                shape = "long")
}

### kollektivtrafik
google_koll = list()

for(i in 1:nrow(bil_koll)){
  google_koll[[i]] = gmapsdistance(bil_koll$start_koord_wgs84[i], 
                                   bil_koll$stop_koord_wgs84[i],
                                   dep_date = next_monday,
                                   dep_time = dep_time,
                                   combinations="pairwise",
                                   mode="transit",
                                   shape = "long")
}


### cykel
google_cykel = list()

for(i in 1:nrow(cykel)){
  google_cykel[[i]] = gmapsdistance(cykel$start_koord_wgs84[i], 
                                   cykel$stop_koord_wgs84[i],
                                   dep_date = next_monday,
                                   dep_time = dep_time,
                                   combinations="pairwise",
                                   mode="bicycling",
                                   shape = "long")
}


### gång
google_walk = list()

for(i in 1:nrow(walk)){
  google_walk[[i]] = gmapsdistance(walk$start_koord_wgs84[i], 
                                    walk$stop_koord_wgs84[i],
                                    dep_date = next_monday,
                                    dep_time = dep_time,
                                    combinations="pairwise",
                                    mode="walking",
                                    shape = "long")
}





##################################################################################
### Extract data from JSON and turn into df
##################################################################################

# Bil
google_bil_resultat = data.frame()

for(i in 1:length(google_koll)){
  google_bil_resultat[i,1] = google_bil[[i]]$Time
  google_bil_resultat[i,2] = google_bil[[i]]$Distance
}

colnames(google_bil_resultat) = c("bil_tid", "bil_distance")


# Kollektivtrafik
google_koll_resultat = data.frame()

for(i in 1:length(google_koll)){
  google_koll_resultat[i,1] = google_koll[[i]]$Time
  google_koll_resultat[i,2] = google_koll[[i]]$Distance
}

colnames(google_koll_resultat) = c("koll_tid", "koll_distance")


# Cykel
google_cykel_resultat = data.frame()

for(i in 1:length(google_cykel)){
  google_cykel_resultat[i,1] = google_cykel[[i]]$Time
  google_cykel_resultat[i,2] = google_cykel[[i]]$Distance
}

colnames(google_cykel_resultat) = c("cykel_tid", "cykel_distance")


# Walk
google_walk_resultat = data.frame()

for(i in 1:length(google_walk)){
  google_walk_resultat[i,1] = google_walk[[i]]$Time
  google_walk_resultat[i,2] = google_walk[[i]]$Distance
}

colnames(google_walk_resultat) = c("walk_tid", "walk_distance")


### Merge
bil_koll1 = bind_cols(bil_koll, google_bil_resultat, google_koll_resultat) 

cykel1 = bind_cols(cykel, google_cykel_resultat)

walk1 = bind_cols(walk, google_walk_resultat)


rvu_restider = bind_rows(bil_koll1, cykel1, walk1) %>%
  mutate(restidskvot_koll_bil = koll_tid / bil_tid,
         api_run_datum = Sys.Date(),
         api_rese_datum = paste(next_monday, dep_time))


write.csv2(rvu_restider, paste0(folder_output, "rvu_restider.csv"), row.names = FALSE)


##################################################################################
### Summary of results
##################################################################################

# Andel av bil-koll resor med restidskvot <= 1.5
nrow(filter(rvu_restider, !is.na(restidskvot_koll_bil) & restidskvot_koll_bil <= 1.5)) /
  nrow(filter(rvu_restider, !is.na(restidskvot_koll_bil) & restidskvot_koll_bil > 1.5))


# Medel restidskvot där färdmedel = koll
rvu_restider %>% filter(!is.na(restidskvot_koll_bil) & 
         str_detect(fardmedel.kat, "Kollektiv*")) %>% 
  summarise(medel = mean(restidskvot_koll_bil))


# Medel restidskvot där färdmedel = bil
rvu_restider %>% filter(!is.na(restidskvot_koll_bil) & 
                 fardmedel.kat == "Bil") %>% 
  summarise(medel = mean(restidskvot_koll_bil))





