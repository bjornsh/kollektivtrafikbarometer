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
pacman::p_load(tidyverse, sf, mapview)



# avoid scientific notation
options(scipen=999)


##################################################################################
### Define paths and create data directories (if not already exist)
##################################################################################

project_wd = getwd()

# Read keys 
api_fil = read_file("Z:/api")
kollbar_data = gsub('^.*kollbar_data: \\s*|\\s*\r.*$', "", api_fil)

# create local directories
dir.create(file.path(paste0(kollbar_data, "shapefile")))
dir.create(file.path(paste0(kollbar_data, "output")))

# define local paths
folder_shapefile = paste0(kollbar_data, "shapefile")
folder_input = paste0(kollbar_data, "input/")
folder_output = paste0(kollbar_data, "output/")

# define path to Github folder containing geodata
folder_github = "https://github.com/bjornsh/gis_data/raw/main/" 


##################################################################################
### Functions
##################################################################################

`%notin%` <- Negate(`%in%`)


# download and unzip shapefiles from Github unless done previously
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
### Download geodata from Github, save locally and load SF
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
url_shp = paste0(folder_github, "scb/deso/deso_2018_v2.zip")
get_shapefile(url_shp)
deso = st_read(paste0(folder_shapefile, "/", "DeSO_2018_v2.gpkg"))

deso = filter(deso, lan == "03") # extract data for Uppsala län


### RegSO
# url_shp = paste0(folder_github, "scb/regso-2018_v1-1.zip")
# get_shapefile(url_shp)
regso = st_read(paste0(folder_shapefile,  "regso-2018_v1-1/RegSO_2018_v1.gpkg"))

url_shp = paste0(folder_github, "scb/regso-2018_v1-1.zip")
get_shapefile(url_shp)
regso = st_read(paste0(folder_shapefile, "/regso-2018_v1-1/RegSO_2018_v1.gpkg"))

regso = filter(regso, substr(kommun,1,2) == "03") # extract data for Uppsala län

mapview(regso)


### SCB 500m ruta
url_shp = paste0(folder_github, "scb/ruta/Rutor500_03_region.zip")
get_shapefile(url_shp)
ruta500 = st_read(paste0(folder_shapefile, "/", file_name, ".shp"),
               options = "ENCODING=WINDOWS-1252")%>%
  st_set_crs(3006) # set coordinate system as SWEREF 99TM

### SCB 1000m ruta
url_shp = paste0(folder_github, "scb/ruta/Rutor1000_03_region.zip")
get_shapefile(url_shp)
ruta1000 = st_read(paste0(folder_shapefile, "/", file_name, ".shp"),
                  options = "ENCODING=WINDOWS-1252")



##################################################################################
### Load Kollbar data
##################################################################################


### Identifiera Kollbar filen som ska läggas till
## Unique kollbar data files that already exist in the "database"
# Respondent ID does not always indicate year-month
# hence datum column needs to be used 

if (file.exists(paste0(folder_output, "person.csv"))){
  pers = read.csv2(paste0(folder_output, "person.csv"))
  
  # vector with year-month combinations already present in db
  exist = unique(pers$ar.manad)
  
  # create file names for already existing data
  exist = paste0("kollbar_", exist, ".xlsx")
  
  ### Read names of all files in folder
  filenames <- list.files(path = folder_input, pattern = "*xlsx")
  
  # exclude 2017 and 2018 files, these are not monthly files and hence do not match file name pattern
  # exclude files that already exist in database 
  filenames = filenames[filenames != "kollbar_2017.xlsx" & 
                          filenames != "kollbar_2018.xlsx" &
                          filenames %notin% exist]
} else {
  filenames = list.files(path = folder_input, pattern = "*xlsx")
}

# check that selection is correct
print(filenames)



### load all input files
setwd(folder_input)

# load input files into list
files <- list()

for(i in filenames){
  files[[i]] <-  readxl::read_excel(i, col_types = "text")
  }


### modify input data
# change col names to lower
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

# view number of interviews per kommun
table(dat$u_kommun)


##################################################################################
### skapa variabler
##################################################################################

# define kommuner within länet
lanets_kommuner = c("enköping", "heby", "håbo", "knivsta", "tierp", "uppsala", "älvkarleby", "östhammar")


# create variables and filter "lanets_kommuner"
dat = dat %>% 
  mutate(ar = as.character(as.numeric(ar) + 2009), # bind_rows is class sensitive
         manad.text = month.abb[as.numeric(manad)],
         manad = ifelse(nchar(manad) == 1, paste0("0", manad), manad),
         ar.manad = as.character(paste0(ar, manad)),  # bind_rows is class sensitive
         alder = as.character(as.numeric(h1) + 12), # bind_rows is class sensitive
         kon = ifelse(u_konkod == "1", "Man",
                      ifelse(u_konkod=="2", "Kvinna",
                             ifelse(u_konkod=="3", "Okänt", "XXX"))),
         responsedate.weekday = weekdays(as.Date(responsedate,'%m/%d/%Y')), #### travel day
         traveldate = as.Date(responsedate,'%m/%d/%Y')-1,
         traveldate.weekday = weekdays(as.Date(responsedate,'%m/%d/%Y')-1)) %>% 
  filter(u_kommun %in% lanets_kommuner) # remove data for interviewees folkbokförd outside länet


# view number of interviews per month
table(dat$ar, dat$manad)

# with(dat, table(ar, manad, u_kommun))


##################################################################################
### skapa attityd filen
##################################################################################

### select variables
attityd = dat %>% 
  dplyr::select(respondentid,
                starts_with("a"), # kännedom och resvanor
                starts_with("c"), # förutsättningar
                starts_with("d"), # ombord
                starts_with("e"), # service och info
                starts_with("g"), # regionpaket
                nki, nps,
                starts_with("ul")) # tillägsfrågor



### load existing data and merge with new data
if (file.exists(paste0(folder_output, "attityd.csv"))){
  attityd_exist = read.csv2(paste0(folder_output, "attityd.csv"), 
                            # bind_rows is class sensitive, avoid problems by turning everything to character
                            colClasses = "character") 
  attityd_final = attityd_exist %>%
    bind_rows(., attityd)
} else {
  attityd_final = attityd
}



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
                b3_r1, # "1" om första resan började hemifrån. "99" = annan adress
                b3_r1_lng, b3_r1_lat,
                contains("hemadress")) %>%  # extra beställning
  # ta bostadskoordinater (extra beställning) om dem finns, annars ta startkoordinater av första resa
  mutate(bostad_lng = ifelse(!is.na(lng_hemadress) &
                               lng_hemadress != "0" &
                               !grepl("sakna",tolower(lng_hemadress)), lng_hemadress, 
                             ifelse((is.na(lng_hemadress) |
                                       lng_hemadress == "0" |
                                       grepl("sakna",tolower(lng_hemadress))) &
                                      !is.na(b3_r1_lng) & 
                                      b3_r1 == "1", b3_r1_lng, NA)),
         bostad_lat = ifelse(!is.na(lat_hemadress) & 
                               lat_hemadress != "0" &
                               !grepl("sakna",tolower(lat_hemadress)), lat_hemadress, 
                             ifelse((is.na(lat_hemadress) |
                                       lat_hemadress == "0" |
                                       grepl("sakna",tolower(lat_hemadress))) &
                                      !is.na(b3_r1_lat) & 
                                      b3_r1 == "1", b3_r1_lat, NA)))



# Skapa SF objekt 
pers_sf = pers %>%
  # select variables used in geodata manipulation
  dplyr::select(respondentid, bostad_lng, bostad_lat) %>% 
  filter(!is.na(bostad_lng), !is.na(bostad_lat)) %>% # remove rows with missing data
  mutate(bostad_lng = str_replace(bostad_lng, ",", "."), # replace decimal "," with "."
         bostad_lat = str_replace(bostad_lat, ",", ".")) %>% 
  mutate_at(vars(bostad_lng, bostad_lat), as.numeric) %>%   # coordinates must be numeric
  # skapa SF object
  st_as_sf(
    coords = c("bostad_lng", "bostad_lat"),
    agr = "constant",
    crs = 4326,        # assign WGS84 as CRS
    stringsAsFactors = FALSE,
    remove = TRUE
  ) %>% 
  st_transform(3006) # convert to SWEREF99 for intersect with shapefiles

# View bostadskoordinater
mapview(pers_sf)


# intersect coordinates with admin boundaries
bostad_meta_sf <- pers_sf %>% 
  st_join(., kommun) %>% 
  st_join(., tatort) %>% 
  st_join(., deso) %>% 
  st_join(., regso) %>%
  st_join(., ruta500) %>% 
  st_join(., ruta1000)


# rename columns
bostad_meta = bostad_meta_sf %>% 
  as.data.frame() %>% 
  dplyr::select(respondentid,
                kommun_namn = "KOMMUNNAMN", 
                kommun_kod = KOMMUNKOD, 
                lan_namn = LANSNAMN, 
                lan_kod = LANSKOD.x,
                tatort = KATEGORI.y,
                tatort_namn = NAMN1,
                tatort_kod = TATNR,
                deso_kod = deso,
                regso_kod = regsokod,
                regso_namn = regso,
                rutid_500 = Rut_id.x,
                rutid_1000 = Rut_id.y) %>%
  # Create short RegSO name, ie everything before first "-"
  mutate(regso_namn_kort = str_split(regso_namn, "-", simplify = TRUE)[,1]) %>% 
  # add prefix "bostad_"
  rename_at(vars(!matches("respondentid")), ~ paste0("bostad_", .))


######################
# Kvalitetscheck #

# kolla: antal svarspersoner med bostadskoordinater utanför länet
nrow(filter(bostad_meta, is.na(bostad_deso_kod)))

# kolla: andel svarspersoner med bostadskoordinater utanför länet
paste0(round(100 * (nrow(filter(bostad_meta, is.na(bostad_deso_kod))) / 
  nrow(bostad_meta)), 1), "%")

######################


# join bostad metadata to pers file
pers = pers %>% 
  left_join(., bostad_meta, by = "respondentid") %>% 
  mutate(across(everything(), as.character))



# load existing data if it exists
if (file.exists(paste0(folder_output, "person.csv"))){
  pers_exist = read.csv2(paste0(folder_output, "person.csv"), 
                            # bind_rows is class sensitive, avoid problems by turning everything to character
                            colClasses = "character") 
  # append new to existing data
  pers_final = pers_exist %>% 
    bind_rows(., pers)
} else {
  pers_final = pers
}






#################################################################################
### skapa df med RVU data
##################################################################################

### turn RVU data from wide to long
### data structure for journey 1 differs from journey 2,3,4,5, 
### ie lat columns comes before long, all other columns the other way around
### as other inconsistencies may exist, colname needs to be created from each input df 

meta = dat %>% dplyr::select(respondentid,  
                             contains("u_"), 
                             traveldate, traveldate.weekday,
                             ar, ar.manad, manad.text, kon, alder,weight)


resa1 = dat %>% # lat kommer innan lng, alla andra resor kommer lat efter lng
  dplyr::select(respondentid, avstand_1, contains("r1")) %>% 
  dplyr::select(respondentid, avstand_1, matches("b3_r1|lat|lng|b2|b5|b6|b7|b8|b11")) %>% 
  dplyr::select(-contains("open")) %>%
  mutate(resa_nr = "resa1")

kolnamn_resa1 = names(resa1) %>% 
  gsub("avstand_1", "avstand", .) %>% 
  str_remove(., "_r1") %>% 
  str_remove(., "r1")



resa2 = dat %>% 
  dplyr::select(respondentid, avstand_2, contains("r2")) %>% 
  dplyr::select(respondentid, avstand_2, matches("b3_r2|lat|lng|b2|b5|b6|b7|b8|b11")) %>%
  dplyr::select(-contains("open")) %>%
  mutate(resa_nr = "resa2")

kolnamn_resa2 = names(resa2) %>% 
  gsub("avstand_2", "avstand", .) %>% 
  str_remove(., "_r2") %>% 
  str_remove(., "r2")



resa3= dat %>% 
  dplyr::select(respondentid, avstand_3, contains("r3")) %>% 
  dplyr::select(respondentid, avstand_3, matches("b3_r3|lat|lng|b2|b5|b6|b7|b8|b11")) %>%
  dplyr::select(-contains("open")) %>%
  mutate(resa_nr = "resa3")

kolnamn_resa3 = names(resa3) %>% 
  gsub("avstand_3", "avstand", .) %>% 
  str_remove(., "_r3") %>% 
  str_remove(., "r3")



resa4= dat %>% 
  dplyr::select(respondentid, avstand_4, contains("r4")) %>% 
  dplyr::select(respondentid, avstand_4, matches("b3_r4|lat|lng|b2|b5|b6|b7|b8|b11")) %>%
  dplyr::select(-contains("open")) %>%
  mutate(resa_nr = "resa4")

kolnamn_resa4 = names(resa4) %>% 
  gsub("avstand_4", "avstand", .) %>% 
  str_remove(., "_r4") %>% 
  str_remove(., "r4")



resa5 = dat %>% 
  dplyr::select(respondentid, avstand_5, contains("r5")) %>% 
  dplyr::select(respondentid, avstand_5, matches("b3_r5|lat|lng|b2|b5|b6|b7|b8|b11")) %>%
  dplyr::select(-contains("open")) %>%
  mutate(resa_nr = "resa5")

kolnamn_resa5 = names(resa5) %>% 
  gsub("avstand_5", "avstand", .) %>% 
  str_remove(., "_r5") %>% 
  str_remove(., "r5")



colnames(resa1) = paste(kolnamn_resa1)
colnames(resa2) = paste(kolnamn_resa2)
colnames(resa3) = paste(kolnamn_resa3)
colnames(resa4) = paste(kolnamn_resa4)
colnames(resa5) = paste(kolnamn_resa5)

rvu = bind_rows(resa1, resa2, resa3, resa4, resa5)


### skapa geometadata för alla resor

### start koordinater
start_koordinat = rvu %>% 
  filter(!is.na(b3_lng), !is.na(b3_lat)) %>%
  distinct(b3_lng, b3_lat) %>% 
  rename(lng = b3_lng,
         lat = b3_lat)

start_koordinat_sf =  start_koordinat %>%
  mutate(lng = str_replace(lng, ",", "."), # replace decimal "," with "."
         lat = str_replace(lat, ",", ".")) %>% 
  mutate_at(vars(lng, lat), as.numeric) %>%   # coordinates must be numeric
  st_as_sf(
    coords = c("lng", "lat"),
    agr = "constant",
    crs = 4326,        # assign WGS84 as CRS
    stringsAsFactors = FALSE,
    remove = TRUE
  ) %>% 
  st_transform(3006) # convert to SWEREF99 TM


start_points_join <- start_koordinat_sf %>% 
  st_join(., kommun) %>% 
  st_join(., tatort) %>% 
  st_join(., deso) %>% 
  st_join(., regso) %>%
  st_join(., ruta500) %>% 
  st_join(., ruta1000)

start_meta = start_points_join %>% 
  as.data.frame() %>% 
  dplyr::select(kommun_namn = KOMMUNNAMN, 
                kommun_kod = KOMMUNKOD, 
                lan_namn = LANSNAMN, 
                lan_kod = LANSKOD.x,
                tatort = KATEGORI.y,
                tatort_namn = NAMN1,
                tatort_kod = TATNR,
                deso_kod = deso,
                regso_kod = regsokod,
                regso_namn = regso,
                rutid_500 = Rut_id.x,
                rutid_1000 = Rut_id.y) %>% 
  # Create short RegSO name, ie everything before first "-"
  mutate(regso_namn_kort = str_split(regso_namn, "-", simplify = TRUE)[,1]) %>% 
  bind_cols(., start_koordinat)

colnames(start_meta) <- paste("start", colnames(start_meta), sep = "_")


### stop koordinater
stop_koordinat = rvu %>% 
  filter(!is.na(b9_lng), !is.na(b9_lat)) %>%
  distinct(b9_lng, b9_lat) %>% 
  rename(lng = b9_lng,
         lat = b9_lat)

stop_koordinat_sf =  stop_koordinat %>% 
  mutate(lng = str_replace(lng, ",", "."), # replace decimal "," with "."
         lat = str_replace(lat, ",", ".")) %>% 
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
  st_join(., regso) %>%
  st_join(., ruta500) %>% 
  st_join(., ruta1000)

stop_meta = stop_points_join %>% 
  as.data.frame() %>% 
  dplyr::select(kommun_namn = KOMMUNNAMN, 
                kommun_kod = KOMMUNKOD, 
                lan_namn = LANSNAMN, 
                lan_kod = LANSKOD.x,
                tatort = KATEGORI.y,
                tatort_namn = NAMN1,
                tatort_kod = TATNR,
                deso_kod = deso,
                regso_kod = regsokod,
                regso_namn = regso,
                rutid_500 = Rut_id.x,
                rutid_1000 = Rut_id.y) %>% 
  # Create short RegSO name, ie everything before first "-"
  mutate(regso_namn_kort = str_split(regso_namn, "-", simplify = TRUE)[,1]) %>% 
  bind_cols(., stop_koordinat)

colnames(stop_meta) <- paste("stop", colnames(stop_meta), sep = "_")


### join start and stop koordinat metadata with original RVU data
rvu = rvu %>% 
  left_join(., start_meta, by = c("b3_lng" = "start_lng", "b3_lat" = "start_lat")) %>% 
  left_join(., stop_meta, by = c("b9_lng" = "stop_lng", "b9_lat" = "stop_lat"))



### add column for fardmedel and combination journeys

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
                                  fardmedel==7, "Annat", fardmedel.kat)) %>% 
  # kombinationsresor med koll och bil eller cykel
  mutate_at(vars(starts_with("b5_")),  as.numeric) %>% 
  mutate(antal_fardmedel = rowSums(select(., starts_with("b5_")), na.rm = TRUE)) %>%
  mutate(koll_kombi = ifelse(b5_1 == "1" & b5_4 == "1", "koll_cykel",
                             ifelse(b5_1 == "1" & 
                                      (b5_2 == "1" | b5_3 == "1"), "koll_bil", NA))) %>% 
  # are start and stop coordinates the same
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



# load existing data if it exists
if (file.exists(paste0(folder_output, "rvu.csv"))){
  rvu_exist = read.csv2(paste0(folder_output, "rvu.csv"), 
                        # bind_rows is class sensitive, avoid problems by turning everything to character
                        colClasses = "character")
  # append new to existing data
  rvu_final = rvu_exist %>% 
    bind_rows(., rvu1)
} else {
  rvu_final = rvu1
}



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




