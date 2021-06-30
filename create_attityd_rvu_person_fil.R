##################################################################################
### Script description
##################################################################################

# Convert Kollektivtrafikbarometer (Kollbar) data to a more user friendly format.

# Original monthly data files contains about 500 rows and 300 columns where the number
# of columns can change on a monthly basis. Sporadic changes in column names, eg upper 
# instead of lower case and "." instead of "_" are taken care of. 

# RVU data comes in a wide format where each variable of each journey gets a new 
# column requiring conversion to long format.

# Journey coordinates are associated with admin boundaries (kommun, DeSO etc) and SCB
# grid for subsequent analysis. Shapefiles used are stored on Github. 

# New data (observations and variables) are added to existing data shortening processing
# times compared to reading in all monthly files.

# All input files need to be stored as .xlsx files due to encoding problems with .csv
# Input files need to be stored in: ~/data/input



##################################################################################
### Clean start
##################################################################################

rm(list = ls())
gc()


##################################################################################
### Libraries etc
##################################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sf)



# avoid scientific notation
options(scipen=999)


##################################################################################
### Create directories (if not already exist)
##################################################################################

dir.create(file.path(getwd(), "data"))
dir.create(file.path(paste0(getwd(), "/data"), "shapefile"))
dir.create(file.path(paste0(getwd(), "/data"), "input"))
dir.create(file.path(paste0(getwd(), "/data"), "output"))

project_wd = getwd()

##################################################################################
### Functions
##################################################################################

`%notin%` <- Negate(`%in%`)


# download and unzip shapefiles from Github
get_shapefile <- function(path){
    file_name = str_extract(url_shp, "[^/]+(?=\\.zip$)")
    file_name_full = paste0(file_name, ".zip")
    
    if(!file.exists(paste0(folder_shapefile, "/", file_name_full))){
      file_name = str_extract(url_shp, "[^/]+(?=\\.zip$)")
      file_name_full = paste0(file_name, ".zip")
      download.file(url_shp, destfile = paste0(folder_shapefile, "/", file_name_full))
      fname = unzip(paste0(folder_shapefile, "/", file_name_full), list=TRUE)$Name[1:5]
      unzip(paste0(folder_shapefile, "/", file_name_full), 
          exdir=folder_shapefile, 
          overwrite=TRUE)
    }
    file_name <<- file_name
    }
  
  

##################################################################################
### Read keys, define paths 
##################################################################################

api_fil <- read_file("Z:/api")

folder_input = paste0(getwd(), "/data/input/")
folder_output = paste0(getwd(), "/data/output/")
folder_shapefile = paste0(getwd(), "/data/shapefile/")
folder_github = "https://github.com/bjornsh/gis_data/raw/main/"





##################################################################################
### Ladda geodata
##################################################################################

### kommun
url_shp = paste0(folder_github, "lantmateriet/kommun/ak_riks.zip")
get_shapefile(url_shp)
kommun = st_read(paste0(folder_shapefile, "/", file_name, ".shp"),
                 options = "ENCODING=WINDOWS-1252")


### tatort
url_shp = paste0(folder_github, "lantmateriet/tatort/mb_riks.zip")
get_shapefile(url_shp)
tatort = st_read(paste0(folder_shapefile, "/", file_name, ".shp"),
                 options = "ENCODING=WINDOWS-1252")


### DeSO
url_shp = paste0(folder_github, "scb/deso/Deso_inomUppsalaLan.zip")
get_shapefile(url_shp)
deso = st_read(paste0(folder_shapefile, "/", file_name, ".shp"),
                 options = "ENCODING=WINDOWS-1252")


### 500m ruta
url_shp = paste0(folder_github, "scb/ruta/Rutor500_03_region.zip")
get_shapefile(url_shp)
ruta500 = st_read(paste0(folder_shapefile, "/", file_name, ".shp"),
               options = "ENCODING=WINDOWS-1252")%>%
  st_set_crs(3006)

### 1000m ruta
url_shp = paste0(folder_github, "scb/ruta/Rutor1000_03_region.zip")
get_shapefile(url_shp)
ruta1000 = st_read(paste0(folder_shapefile, "/", file_name, ".shp"),
                  options = "ENCODING=WINDOWS-1252")



##################################################################################
### Ladda kollbar
##################################################################################

### Identifiera Kollbar filen som ska läggas till
## Unique kollbar data files that already exist in the "database"
# Respondent ID does not always indicate year-month
# hence datum column needs to be used 

pers = read.csv2(paste0(folder_output, "person.csv"))

# vector with year-month combinations already present in db
exist = unique(pers$ar.manad)

# create file names for already existing data
exist = paste0("kollbar_", exist, ".xlsx")

### Read names of all files in folder
filenames <- list.files(path = folder_input, pattern = "*xlsx")

# exclude 2017 and 2018 files 
# these are not monthly files and hence do not match file name pattern
filenames = filenames[filenames != "kollbar_2017.xlsx" & 
                        filenames != "kollbar_2018.xlsx"]


# names of files for dates that are not already included
filenames = filenames[filenames %notin% exist]


### read all files
setwd(folder_input)

files <- list()

for(i in filenames){
  files[[i]] <-  readxl::read_excel(i)
  }


### change col names to lower
for(i in 1:length(files)){
  names(files[[i]]) <- tolower(names(files[[i]]))
}


### replace Swedish characters and # from column names
for(i in 1:length(files)){
  names(files[[i]]) <- gsub("å", "a", (names(files[[i]])))
  names(files[[i]]) <- gsub("ä", "a", (names(files[[i]])))
  names(files[[i]]) <- gsub("ö", "o", (names(files[[i]])))
  names(files[[i]]) <- gsub("\\.", "_", (names(files[[i]])))
  names(files[[i]]) <- gsub("#", "_", (names(files[[i]])))
}


### replace wrong column name
for(i in 1:length(files)){
  names(files[[i]]) <- gsub("b7r1_8", "b7r1_7", (names(files[[i]]))) # kollbar_201801: b7r1_8 finns inte i enkät
}

### put all dfs into a single list where new columns are added
### ie wrong column names are included as new columns
dat = files %>%
  bind_rows()

### convert all string to lower case
dat = mutate_all(dat, .funs=tolower)

table(dat$u_kommun)


##################################################################################
### skapa variabler
##################################################################################

lanets_kommuner = c("enköping", "heby", "håbo", "knivsta", "tierp", "uppsala", "älvkarleby", "östhammar")

dat = dat %>% 
  mutate(ar = as.character(as.numeric(ar) + 2009), # bind_rows is class sensitive
         manad.text = month.abb[manad],
         manad = ifelse(nchar(manad) == 1, paste0("0", manad), manad),
         ar.manad = as.character(paste0(ar, manad)),  # bind_rows is class sensitive
         alder = as.character(as.numeric(h1) + 12), # bind_rows is class sensitive
         kon = ifelse(u_konkod == "1", "Man",
                      ifelse(u_konkod=="2", "Kvinna",
                             ifelse(u_konkod=="3", "Okänt", "XXX"))),
         responsedate.weekday = weekdays(as.Date(responsedate,'%m/%d/%Y')), #### travel day
         traveldate = as.Date(responsedate,'%m/%d/%Y')-1,
         traveldate.weekday = weekdays(as.Date(responsedate,'%m/%d/%Y')-1)) %>% 
  filter(u_kommun %in% lanets_kommuner) # a couple of people from other kommuner slipped in

# kolla antal intervjuar per månad
table(dat$ar, dat$manad)

with(dat, table(ar, manad, u_kommun))


##################################################################################
### skapa attityd filen
##################################################################################

attityd = dat %>% 
  dplyr::select(respondentid,
                starts_with("c"), # förutsättningar
                starts_with("d"), # ombord
                starts_with("e"), # service och info
                starts_with("g"), # regionpaket
                nki, nps,
                starts_with("ul")) # tillägsfrågor



# load existing data
attityd_exist = read.csv2(paste0(folder_output, "attityd.csv"), 
                          # bind_rows is class sensitive, avoid problems by turning everything to character
                          colClasses = "character") 

# append existing with new data
attityd_final = attityd_exist %>% 
  bind_rows(., attityd)
  



##################################################################################
### skapa person filen
##################################################################################

# create file with relevant columns
pers = dat %>% 
  dplyr::select(respondentid, ar, ar.manad, alder, kon, 
                u_kommun, u_kommunkod, u_lan, u_lanskod, u_postnummer, 
                u_postort, u_stad, u_stadskod, 
                responsedate.weekday,
                b1, 
                h1:h6, 
                weight, 
                b3_r1_lng, b3_r1_lat)


# create sf with start coordinates of first journey, ie assumed bostad
pers_sf =  pers %>%
  dplyr::select(respondentid, b3_r1_lng, b3_r1_lat) %>% 
  filter(!is.na(b3_r1_lng), !is.na(b3_r1_lat)) %>% # remove rows with missing data
  mutate_at(vars(b3_r1_lng, b3_r1_lat), as.numeric) %>%   # coordinates must be numeric
  st_as_sf(
    coords = c("b3_r1_lng", "b3_r1_lat"),
    agr = "constant",
    crs = 4326,        # assign WGS84 as CRS
    stringsAsFactors = FALSE,
    remove = TRUE
  ) %>% 
  st_transform(3006) # convert to SWEREF99


# intersect coordinates with admin boundaries
bostad_meta <- pers_sf %>% 
  st_join(., kommun) %>% 
  st_join(., tatort) %>% 
  st_join(., deso) %>% 
  st_join(., ruta500) %>% 
  st_join(., ruta1000)


# rename columns
bostad_meta = bostad_meta %>% 
  as.data.frame() %>% 
  dplyr::select(respondentid,
                kommun_namn = "KOMMUNNAMN", 
                kommun_kod = KOMMUNKOD, 
                lan_namn = LANSNAMN, 
                lan_kod = LANSKOD.x,
                tatort = KATEGORI.y,
                tatort_namn = NAMN1,
                tatort_kod = TATNR,
                deso = Deso,
                rutid_500 = Rut_id.x,
                rutid_1000 = Rut_id.y) %>%
  # add prefix "bostad_"
  rename_at(vars(!matches("respondentid")), ~ paste0("bostad_", .))


# join bostad metadata to pers file
pers = pers %>% 
  left_join(., bostad_meta, by = "respondentid") %>% 
  mutate(across(everything(), as.character))


##########################################################
##### run only if database already exists #######

# load existing data
pers_exist = read.csv2(paste0(folder_output, "person.csv"), 
                       # bind_rows is class sensitive, avoid problems by turning everything to character
                       colClasses = "character") 


# append new to existing data
pers_final = pers_exist %>% 
  bind_rows(., pers)

##########################################################




#################################################################################
### skapa df med RVU data
##################################################################################

### turn RVU data from wide to long
meta = dat %>% dplyr::select(respondentid,  
                             contains("u_"), 
                             contains("Bostad"),
                             traveldate, traveldate.weekday,
                             ar, ar.manad, manad.text, kon, alder,weight)


resa1= dat %>% 
  dplyr::select(respondentid, avstand_1, contains("r1")) %>% 
  dplyr::select(respondentid, avstand_1, matches("lat|lng|b2|b5|b6|b7|b8|b11")) %>%
  dplyr::select(-contains("open")) %>%
  mutate(resa_nr = "resa1")


kolnamn = names(resa1) %>% 
  gsub("avstand_1", "avstand", .) %>% 
  str_remove(., "_r1") %>% 
  str_remove(., "r1")


resa2= dat %>% 
  dplyr::select(respondentid, avstand_2, contains("r2")) %>% 
  dplyr::select(respondentid, avstand_2, matches("lat|lng|b2|b5|b6|b7|b8|b11")) %>%
  dplyr::select(-contains("open")) %>%
  mutate(resa_nr = "resa2")

resa3= dat %>% 
  dplyr::select(respondentid, avstand_3, contains("r3")) %>% 
  dplyr::select(respondentid, avstand_3, matches("lat|lng|b2|b5|b6|b7|b8|b11")) %>%
  dplyr::select(-contains("open")) %>%
  mutate(resa_nr = "resa3")

resa4= dat %>% 
  dplyr::select(respondentid, avstand_4, contains("r4")) %>% 
  dplyr::select(respondentid, avstand_4, matches("lat|lng|b2|b5|b6|b7|b8|b11")) %>%
  dplyr::select(-contains("open")) %>%
  mutate(resa_nr = "resa4")

resa5 = dat %>% 
  dplyr::select(respondentid, avstand_5, contains("r5")) %>% 
  dplyr::select(respondentid, avstand_5, matches("lat|lng|b2|b5|b6|b7|b8|b11")) %>%
  dplyr::select(-contains("open")) %>%
  mutate(resa_nr = "resa5")

colnames(resa1) = paste(kolnamn)
colnames(resa2) = paste(kolnamn)
colnames(resa3) = paste(kolnamn)
colnames(resa4) = paste(kolnamn)
colnames(resa5) = paste(kolnamn)

rvu = bind_rows(resa1, resa2, resa3, resa4, resa5)


### skapa geometadata för alla resor

### start koordinater
start_koordinat = rvu %>% 
  filter(!is.na(b3_lng), !is.na(b3_lat)) %>%
  distinct(b3_lng, b3_lat) %>% 
  rename(lng = b3_lng,
         lat = b3_lat)

start_koordinat_sf =  start_koordinat %>% 
  mutate_at(vars(lng, lat), as.numeric) %>%   # coordinates must be numeric
  st_as_sf(
    coords = c("lng", "lat"),
    agr = "constant",
    crs = 4326,        # assign WGS84 as CRS
    stringsAsFactors = FALSE,
    remove = TRUE
  ) %>% 
  st_transform(3006)


start_points_join <- start_koordinat_sf %>% 
  st_join(., kommun) %>% 
  st_join(., tatort) %>% 
  st_join(., deso) %>% 
  st_join(., ruta500) %>% 
  st_join(., ruta1000)

start_meta = start_points_join %>% 
  as.data.frame() %>% 
  dplyr::select(kommun_namn = "KOMMUNNAMN", 
                kommun_kod = KOMMUNKOD, 
                lan_namn = LANSNAMN, 
                lan_kod = LANSKOD.x,
                tatort = KATEGORI.y,
                tatort_namn = NAMN1,
                tatort_kod = TATNR,
                deso = Deso,
                rutid_500 = Rut_id.x,
                rutid_1000 = Rut_id.y) %>% 
  bind_cols(., start_koordinat)

colnames(start_meta) <- paste("start", colnames(start_meta), sep = "_")


### stop koordinater
stop_koordinat = rvu %>% 
  filter(!is.na(b9_lng), !is.na(b9_lat)) %>%
  distinct(b9_lng, b9_lat) %>% 
  rename(lng = b9_lng,
         lat = b9_lat)

stop_koordinat_sf =  stop_koordinat %>% 
  mutate_at(vars(lng, lat), as.numeric) %>%   # coordinates must be numeric
  st_as_sf(
    coords = c("lng", "lat"),
    agr = "constant",
    crs = 4326,        # assign WGS84 as CRS
    stringsAsFactors = FALSE,
    remove = TRUE
  ) %>% 
  st_transform(3006)


stop_points_join <- stop_koordinat_sf %>% 
  st_join(., kommun) %>% 
  st_join(., tatort) %>% 
  st_join(., deso) %>% 
  st_join(., ruta500) %>% 
  st_join(., ruta1000)

stop_meta = stop_points_join %>% 
  as.data.frame() %>% 
  dplyr::select(kommun_namn = "KOMMUNNAMN", 
                kommun_kod = KOMMUNKOD, 
                lan_namn = LANSNAMN, 
                lan_kod = LANSKOD.x,
                tatort = KATEGORI.y,
                tatort_namn = NAMN1,
                tatort_kod = TATNR,
                deso = Deso,
                rutid_500 = Rut_id.x,
                rutid_1000 = Rut_id.y) %>% 
  bind_cols(., stop_koordinat)

colnames(stop_meta) <- paste("stop", colnames(stop_meta), sep = "_")


### join start and stop koordinat metadata with original RVU data
rvu = rvu %>% 
  left_join(., start_meta, by = c("b3_lng" = "start_lng", "b3_lat" = "start_lat")) %>% 
  left_join(., stop_meta, by = c("b9_lng" = "stop_lng", "b9_lat" = "stop_lat"))



### add fardmedel kolumn

# if person states >1 färdmedel in Q b5, then follows Q b6 (huvudsakliga färdmedel)
# if b6 is not NA the person has already decided on a huvud färdmedel
# if b6 is NA, the question was not asked because the person stated only one färdmedel in Q b5

rvu = rvu %>% 
  mutate(fardmedel = ifelse(!is.na(b6), b6,
                            ifelse(is.na(b6) & b5_1 == "1", "1",
                                 ifelse(is.na(b6) & b5_2 == "1", "2",
                                        ifelse(is.na(b6) & b5_3 == "1", "3",
                                               ifelse(is.na(b6) & b5_4 == "1", "4",
                                                      ifelse(is.na(b6) & b5_5 == "1", "5",
                                                             ifelse(is.na(b6) & b5_6 == "1", "6",
                                                                    ifelse(is.na(b6) & b5_7 == "1", "7","XXXX")))))))))


rvu = rvu %>% 
  # if koll is main and the type is train than this is always classed as a train journey
  mutate(fardmedel.kat = ifelse(fardmedel == "1" & 
                                  b7_3 == "1", # tåg 
                                "Kollektiv_tåg", NA)) %>%
  mutate(fardmedel.kat = ifelse(is.na(fardmedel.kat) & 
                                           fardmedel == "1" &
                                           (b7_1 == "1" | # buss
                                              b7_2 == "1"), # Flexlinje/Flextrafik/Närtrafik/Servicelinje 
                                         "Kollektiv_buss", fardmedel.kat)) %>%
  mutate(fardmedel.kat = ifelse(is.na(fardmedel.kat) & 
                                  fardmedel == "1" &
                                  (b7_4 == "4" | # Tunnelbana
                                     b7_5 == "5" | # Spårvagn
                                     b7_6 == "6" | # Båt/färja
                                     b7_7 == "7"), # Taxi (även färdtjänst och skolskjuts)
                                "Kollektiv_annat", fardmedel.kat)) %>% 
  mutate(fardmedel.kat = ifelse(is.na(fardmedel.kat) & 
                                  (fardmedel == "2" | # Bil/Moped/MC – som förare
                                     fardmedel == "3" | # Bil/Moped/MC – som passagerare
                                     fardmedel == "6"), # Taxi (även färdtjänst och skolskjuts)
                                   "Bil", fardmedel.kat)) %>% 
  mutate(fardmedel.kat = ifelse(is.na(fardmedel.kat) & 
                                  fardmedel == "4", "Cykel", fardmedel.kat)) %>%
  mutate(fardmedel.kat = ifelse(is.na(fardmedel.kat) &
                                  fardmedel == "5", "Gång", fardmedel.kat)) %>% 
  mutate(fardmedel.kat = ifelse(is.na(fardmedel.kat) &
                                  fardmedel==7, "Annat", fardmedel.kat))


### create columns: 
# 1) number of färdmedel used
# 2) are start and stop coordinates the same

rvu = rvu %>% 
  # number of färdmedel used
  mutate_at(vars(starts_with("b5_")), as.numeric) %>% 
  mutate(fardmedelantal = b5_1 + b5_2 + b5_3 + b5_4 + b5_5 + b5_6 + b5_7) %>% 
  # are start and stop coordinates the same?
  mutate(samma_koordinat = case_when(!is.na(b3_lat) & !is.na(b9_lat) &
                                       b3_lat == b9_lat &
                                       b3_lng == b9_lng ~ "ja",
                                     !is.na(b3_lat) & !is.na(b9_lat) &
                                       (b3_lat != b9_lat &
                                       b3_lng != b9_lng) ~ "nej")) %>% 
  # all variables as.character to facilitate append
  mutate(across(everything(), as.character))
  

#### data cleaning
# all journeys get assigned a start coordinate, 
# conversion from wide to long introduced rows for journeys that did not take place 
# remove these rows
# use column "b1" in file "person" for tex %respondents that travelled yesterday 
rvu1 = filter(rvu, !is.na(b3_lat))



##########################################################
##### run only if database already exists #######

### load existing data
rvu_exist = read.csv2(paste0(folder_output, "rvu.csv"), 
                       # bind_rows is class sensitive, avoid problems by turning everything to character
                       colClasses = "character") 


### append existing with new data
rvu_final = rvu_exist %>% 
  bind_rows(., rvu1)

##########################################################




#################################################################################
### write to disk
##################################################################################

write.table(pers_final, 
            file = paste0(folder_output, "person.csv"), 
            sep = ";", 
            row.names=FALSE)

write.csv2(rvu_final, 
           paste0(folder_output, "rvu.csv"), 
           row.names=FALSE)

write.table(attityd_final, 
            file = paste0(folder_output, "attityd.csv"), 
            sep = ";", 
            row.names=FALSE)



#################################################################################
### reset working directory
##################################################################################

setwd(project_wd)

