#---------------------------------------------------------------------------------------------------
# Syfte
#---------------------------------------------------------------------------------------------------

## Hantera Kollektivtrafikbarometer data som laddas ner som måndsfiler (500 rader med 300+ kolumner per månad)
## Skapa en fil för all attityddata, en fil för RVU data och en fil med persondata
## lägg till geodata som kategorisera start- och stopkoordinater




#---------------------------------------------------------------------------------------------------
# Städa
#---------------------------------------------------------------------------------------------------
rm(list = ls())
gc()



#---------------------------------------------------------------------------------------------------
# Libraries
#---------------------------------------------------------------------------------------------------
library(reshape2)
library(dplyr)
library(readxl)
library(rgdal)
library(raster)
library(readxl)

`%notin%` <- Negate(`%in%`)




#---------------------------------------------------------------------------------------------------
# Hämta data
#---------------------------------------------------------------------------------------------------

## set wd (data folder)
setwd("Z:/a_data/kollbar/data_ul/xlsx") 
filenames <- list.files(path=".",pattern="*xlsx")


## load files in list
files <- list()
for(i in filenames){
  files[[i]] <-  read_xlsx(i, col_names = TRUE)} #  read.csv2(i, header = TRUE, fileEncoding="UTF-8-BOM") ### ,, , stringsAsFactors=FALSE



#---------------------------------------------------------------------------------------------------
# Hantera data
#---------------------------------------------------------------------------------------------------

## change col names to lower
for(i in 1:length(files)){
  names(files[[i]]) <- tolower(names(files[[i]]))
}

## replace Swedish characters and # from column names
for(i in 1:length(files)){
  names(files[[i]]) <- gsub("å", "a", (names(files[[i]])))
  names(files[[i]]) <- gsub("ä", "a", (names(files[[i]])))
  names(files[[i]]) <- gsub("ö", "o", (names(files[[i]])))
  names(files[[i]]) <- gsub("#", "_", (names(files[[i]])))
}

## replace wrong column name
for(i in 1:length(files)){
  names(files[[i]]) <- gsub("b7r1_8", "b7r1_7", (names(files[[i]]))) # kollbar_201801: b7r1_8 finns inte i enkät
}

## extract useful columns
## column ordering is not consistent across files, requiring loading of fixed column names 
files = lapply(files, "[", c("a1", "a2", "a4", "a6", "a7", "a7a", "a7b",
                             "a5a_open", "a5a_1", "a5a_2", "a5a_3", "a5a_4", "a5a_5", "a5a_6", "a5a_7", "a5a_8", "a5a_9", 
                             "a5b_open", "a5b_1", "a5b_2", "a5b_3", "a5b_4", "a5b_5", "a5b_6", "a5b_7", "a5b_8", "a5b_9", "a5b_10", "a5b_11", "a5b_12",
                             "avstand_1", "avstand_2", "avstand_3", "avstand_4", "avstand_5", 
                             "avstand_1_kod", "avstand_2_kod", "avstand_3_kod", "avstand_4_kod", "avstand_5_kod", 
                             "b1", "b10_r1", "b10_r2", "b10_r3", "b10_r4", "b10_r5", "b11a_r1", 
                             "b11a_r2", "b11a_r3", "b11a_r4", "b11a_r5", "b11b_r1", "b11b_r2", "b11b_r3", "b11b_r4", "b11b_r5", 
                             "b12_r1", "b12_r2", "b12_r3", "b12_r4", "b12_r5", "b13_r1", "b13_r2", "b13_r3", "b13_r4", "b13_r5", 
                             "b2_r1", "b2_r2", "b2_r3", "b2_r4", "b2_r5", "b3_r1", "b3_r2", "b3_r3", "b3_r4", "b3_r5", "b3_r1_adress", 
                             "b3_r1_formatted_address", "b3_r1_lat", "b3_r1_lng", "b3_r1_type_administrative_area_level_1", 
                             "b3_r1_type_country", "b3_r1_type_locality", "b3_r1_type_postal_code", "b3_r1_type_postal_town", 
                             "b3_r1_type_sublocality", "b3_r2_lat", "b3_r2_lng", "b3_r3_lat", "b3_r3_lng", "b3_r4_lat", "b3_r4_lng", 
                             "b3_r5_lat", "b3_r5_lng", "b4_r1", "b4_r2", "b4_r3", "b4_r4", "b4_r5", "b5r1_1", "b5r1_2", "b5r1_3", 
                             "b5r1_4", "b5r1_5", "b5r1_6", "b5r1_7", "b5r2_1", "b5r2_2", "b5r2_3", "b5r2_4", "b5r2_5", "b5r2_6", 
                             "b5r2_7", "b5r3_1", "b5r3_2", "b5r3_3", "b5r3_4", "b5r3_5", "b5r3_6", "b5r3_7", "b5r4_1", "b5r4_2", 
                             "b5r4_3", "b5r4_4", "b5r4_5", "b5r4_6", "b5r4_7", "b5r5_1", "b5r5_2", "b5r5_3", "b5r5_4", "b5r5_5", 
                             "b5r5_6", "b5r5_7", "b6_r1", "b6_r2", "b6_r3", "b6_r4", "b6_r5",
                             "b7r1_1", "b7r1_2", "b7r1_3", "b7r1_4", "b7r1_5", "b7r1_6", "b7r1_7", 
                             "b7r2_1", "b7r2_2", "b7r2_3", "b7r2_4", "b7r2_5", "b7r2_6", "b7r2_7", 
                             "b7r3_1", "b7r3_2", "b7r3_3", "b7r3_4", "b7r3_5", "b7r3_6", "b7r3_7", 
                             "b7r4_1", "b7r4_2", "b7r4_3", "b7r4_4", "b7r4_5", "b7r4_6", "b7r4_7", 
                             "b7r5_1", "b7r5_2", "b7r5_3", "b7r5_4", "b7r5_5", "b7r5_6", "b7r5_7",
                             "b7ar1_1", "b7ar1_2", "b7ar1_3", "b7ar1_4", 
                             "b7ar2_1", "b7ar2_2", "b7ar2_3", "b7ar2_4", 
                             "b7ar3_1", "b7ar3_2", "b7ar3_3", "b7ar3_4", 
                             "b7ar4_1", "b7ar4_2", "b7ar4_3", "b7ar4_4", 
                             "b7ar5_1", "b7ar5_2", "b7ar5_3", "b7ar5_4", 
                             "b7br1_1", "b7br1_2", "b7br1_3", "b7br1_4", 
                             "b7br2_1", "b7br2_2", "b7br2_3", "b7br2_4", 
                             "b7br3_1", "b7br3_2", "b7br3_3", "b7br3_4", 
                             "b7br4_1", "b7br4_2", "b7br4_3", "b7br4_4", 
                             "b7br5_1", "b7br5_2", "b7br5_3", "b7br5_4", 
                             "b8a_r1", "b8a_r2", "b8a_r3", "b8a_r4", "b8a_r5", "b8a_r1_open", 
                             "b8a_r2_open", "b8a_r3_open", "b8a_r4_open", "b8a_r5_open", "b8b_r1", 
                             "b8b_r2", "b8b_r3", "b8b_r4", "b8b_r5", "b9_r1", "b9_r2", "b9_r3", "b9_r4", "b9_r5", "b9_r1_adress", 
                             "b9_r1_formatted_address", "b9_r1_lat", "b9_r1_lng", "b9_r1_type_administrative_area_level_1", "b9_r1_type_country", 
                             "b9_r1_type_locality", "b9_r1_type_postal_code", "b9_r1_type_postal_town", "b9_r1_type_sublocality", "b9_r2_adress", 
                             "b9_r2_formatted_address", "b9_r2_lat", "b9_r2_lng", "b9_r2_type_administrative_area_level_1", "b9_r2_type_country", 
                             "b9_r2_type_locality", "b9_r2_type_postal_code", "b9_r2_type_postal_town", "b9_r2_type_sublocality", "b9_r3_adress", 
                             "b9_r3_formatted_address", "b9_r3_lat", "b9_r3_lng", "b9_r3_type_administrative_area_level_1", "b9_r3_type_country", 
                             "b9_r3_type_locality", "b9_r3_type_postal_code", "b9_r3_type_postal_town", "b9_r3_type_sublocality", "b9_r4_adress", 
                             "b9_r4_formatted_address", "b9_r4_lat", "b9_r4_lng", "b9_r4_type_administrative_area_level_1", "b9_r4_type_country", 
                             "b9_r4_type_locality", "b9_r4_type_postal_code", "b9_r4_type_postal_town", "b9_r4_type_sublocality", "b9_r5_adress", 
                             "b9_r5_formatted_address", "b9_r5_lat", "b9_r5_lng", "b9_r5_type_administrative_area_level_1", "b9_r5_type_country", 
                             "b9_r5_type_locality", "b9_r5_type_postal_code", "b9_r5_type_postal_town", "b9_r5_type_sublocality", 
                             "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "d1", "d2", "d3", "d4", 
                             "e1", "e2", "e3", "e4", "e5", "e6", "e6a", "g1", "g2", "g3", "g1_open", "g4_1", "g4_2", "g4_3", "g4_4", "g4_5", 
                             "g4_6", "g4_7", "g4_8", "g4_9", "g4_10", "g4_11", "h1", "h2", "h3", "h4", "h5", "h6", "h7", "manad", "nki", "nps", 
                             "respondentid", "responsedate", "totalt_avstand", "totalt_avstand_kod", "u_bolag", "u_bolagskod", "u_jordbruksverketsdef", 
                             "u_jordbruksverketsdefkod", "u_kommun", "u_kommunkod", "u_kon", "u_konkod", "u_lan", "u_lanskod", "u_postnummer", 
                             "u_postort", "u_stad", "u_stadskod", "u_trafikomrade", "u_trafikomradeskod", "u_trafiktyp", "u_trafiktypskod", "weight", 
                             "ar"))



## bind all files into single df
fin <- do.call(rbind, files)

## create useful variables
fin = fin %>% mutate(ar = ifelse(ar==8, "2017", ## survey year
                           ifelse(ar==9, "2018",
                                  ifelse(ar==10, "2019",
                                         ifelse(ar==11, "2020",
                                                ifelse(ar==12, "2021",
                                                       ifelse(ar==13, "2022", "XXX")))))),
               kon = ifelse(u_konkod=="1", "Man", ## gender of respondent
                            ifelse(u_konkod=="2", "Kvinna",
                                   ifelse(u_konkod=="3", "Okänt", "XXX"))),
               alder = h1+12, ## age of respondent
               ar.manad = paste(ar, manad, sep="/"), # year/month numeric
               manad.text = month.abb[manad], # month abbreviated
               responsedate.weekday = weekdays(as.Date(responsedate,'%m/%d/%Y')), ## response day
               traveldate = as.Date(responsedate,'%m/%d/%Y')-1, ## travel day
               traveldate.weekday = weekdays(as.Date(responsedate,'%m/%d/%Y')-1),
               u_postort = tolower(u_postort),
               u_kommun = tolower(u_kommun),
               u_jordbruksverketsdef = tolower(u_jordbruksverketsdef),
               u_trafiktyp= tolower(u_trafiktyp))
               
                                                

## check: number of interviews per month
table(fin$ar,fin$manad)




## create geometadata for first journey (antagligen bostad)

# ladda shapefiler
kommun=readOGR("Z:/a_data/GIS/polygon/lantmateriet/ak_riks.shp", stringsAsFactors = FALSE, verbose = FALSE)
tatort=readOGR("Z:/a_data/GIS/polygon/lantmateriet/mb_riks.shp", stringsAsFactors = FALSE, verbose = FALSE) # Kommun och Län data ingår!
#deso=readOGR("Z:/a_data/GIS/deso/Deso_inomUppsalaLan.shp", stringsAsFactors = FALSE, verbose = FALSE)
deso=readOGR("Z:/a_data/GIS/polygon/SCB_Deso/Deso_inomUppsalaLan.shp", stringsAsFactors = FALSE, verbose = FALSE)
ruta500 = readOGR("Z:/a_data/GIS/aa_BasFilerUppsalaLan/Rutor500_03_region.shp", stringsAsFactors = FALSE, verbose = FALSE)
ruta1000 = readOGR("Z:/a_data/GIS/aa_BasFilerUppsalaLan/Rutor1000_03_region.shp", stringsAsFactors = FALSE, verbose = FALSE)
#lan_uppsala = readOGR("Z:/a_data/GIS/Lan_SCB/lan_uppsala.shp", stringsAsFactors = FALSE, verbose = FALSE)


# definera projection string for SWEREF99
SWEREF99TM = "+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
# WGS84 = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

UnikKoordinat = fin %>% 
  dplyr::select(b3_r1_lng, b3_r1_lat) %>% 
  filter(!is.na(b3_r1_lng), !is.na(b3_r1_lat)) %>%
  distinct(b3_r1_lng, b3_r1_lat)

UnikKoordinatKopia = UnikKoordinat

coordinates(UnikKoordinatKopia) = ~b3_r1_lng+b3_r1_lat 

proj4string(UnikKoordinatKopia) = CRS("+init=epsg:4326") # define original CRS (WGS84)

# transformera koordinater från WGS84 till SWEREF
UnikKoordinatKopia_sweref <- spTransform(UnikKoordinatKopia, CRS(SWEREF99TM))
UnikKoordinatKopia_sweref <- as_data_frame(UnikKoordinatKopia_sweref)

# merge hpl coordinates with shapefiles 
start_kommun <- data.frame(coordinates(UnikKoordinatKopia_sweref),
                         extract(kommun, UnikKoordinatKopia_sweref))
start_tatort <- data.frame(coordinates(UnikKoordinatKopia_sweref),
                         extract(tatort, UnikKoordinatKopia_sweref))
start_deso <- data.frame(coordinates(UnikKoordinatKopia_sweref),
                       extract(deso, UnikKoordinatKopia_sweref))
start_ruta500 <- data.frame(coordinates(UnikKoordinatKopia_sweref),
                       extract(ruta500, UnikKoordinatKopia_sweref))
start_ruta1000 <- data.frame(coordinates(UnikKoordinatKopia_sweref),
                            extract(ruta1000, UnikKoordinatKopia_sweref))

# append files: alla hpl med all meta data
start_meta = cbind(start_kommun[,c("b3_r1_lng", "b3_r1_lat", "KOMMUNNAMN", "LANSNAMN")], 
                 start_tatort[,c("KATEGORI", "NAMN1")], 
                 start_deso[,c("Deso")], 
                 start_ruta500[,c("Rut_id")],
                 start_ruta1000[,c("Rut_id")],
                 UnikKoordinat)

colnames(start_meta) = c("b3_lng_sweref", "b3_lat_sweref", "BostadKommunNamn", 
                         "BostadLanNamn", "BostadTatortKat", "BostadTatortNamn", 
                         "BostadDesoID", "BostadRut500ID", "BostadRut1000ID", 
                         "Bostad_Wgs84lng", "Bostad_Wgs84lat")

fin$ConcatStart = paste(fin$b3_r1_lng, fin$b3_r1_lat)

start_meta$concat = paste(start_meta$Bostad_Wgs84lng, start_meta$Bostad_Wgs84lat)
start_meta = start_meta %>% dplyr::select(-b3_lng_sweref, -b3_lat_sweref)

fin = fin %>% 
  left_join(., start_meta, by = c("ConcatStart" = "concat"))






#---------------------------------------------------------------------------------------------------
# Skapa fil med data för intervjupersonen
#---------------------------------------------------------------------------------------------------
persondata  = fin %>% 
  dplyr::select(respondentid, ar, ar.manad, alder, kon, contains("Bostad"), 
                u_kommun, u_kommunkod, u_lan, u_lanskod, u_postnummer, 
                u_postort, u_stad, u_stadskod, h1:h6, weight) %>%
  as.data.frame()


# write to disk
write.table(persondata, file = "data_ul/persondata.csv", sep = ";", row.names=FALSE)





#---------------------------------------------------------------------------------------------------
# Skapa fil med attityddata
#---------------------------------------------------------------------------------------------------
tabort = names(persondata)
tabort = tabort[tabort!="respondentid"]

attityd = fin %>% 
  dplyr::select(-tabort) %>%
  dplyr::select(-starts_with("b2"), -starts_with("b3"), -starts_with("b4"), -starts_with("b5"), -starts_with("b6"), -starts_with("b7"),
                -starts_with("b8"), -starts_with("b9"), -starts_with("b10"), -starts_with("b11"), -starts_with("b12"), -starts_with("b13"),
                -starts_with("avstand"), -starts_with("u_"), -starts_with("totalt_"), -ConcatStart)

# write to disk
write.table(attityd, file = "Z:/a_data/kollbar/data_ul/kolbart_2017-nu.csv", sep = ";", row.names=FALSE)





#---------------------------------------------------------------------------------------------------
# Skapa fil med RVU data
#---------------------------------------------------------------------------------------------------

meta = fin %>% dplyr::select(respondentid, responsedate, 
                             contains("u_"), 
                             contains("Bostad"),
                             traveldate, traveldate.weekday,
                             ar, ar.manad, manad.text, kon, alder,weight)


resa1= fin %>% 
  dplyr::select(respondentid, avstand_1, contains("r1")) %>% 
  dplyr::select(respondentid, avstand_1, matches("lat|lng|b2|b5|b6|b7|b8|b11")) %>%
  dplyr::select(-contains("open")) %>%
  mutate(resa.no = "resa1")

resa2= fin %>% 
  dplyr::select(respondentid, avstand_2, contains("r2")) %>% 
  dplyr::select(respondentid, avstand_2, matches("lat|lng|b2|b5|b6|b7|b8|b11")) %>%
  dplyr::select(-contains("open")) %>%
  mutate(resa.no = "resa2")

resa3= fin %>% 
  dplyr::select(respondentid, avstand_3, contains("r3")) %>% 
  dplyr::select(respondentid, avstand_3, matches("lat|lng|b2|b5|b6|b7|b8|b11")) %>%
  dplyr::select(-contains("open")) %>%
  mutate(resa.no = "resa3")

resa4= fin %>% 
  dplyr::select(respondentid, avstand_4, contains("r4")) %>% 
  dplyr::select(respondentid, avstand_4, matches("lat|lng|b2|b5|b6|b7|b8|b11")) %>%
  dplyr::select(-contains("open")) %>%
  mutate(resa.no = "resa4")

resa5= fin %>% 
  dplyr::select(respondentid, avstand_5, contains("r5")) %>% 
  dplyr::select(respondentid, avstand_5, matches("lat|lng|b2|b5|b6|b7|b8|b11")) %>%
  dplyr::select(-contains("open")) %>%
  mutate(resa.no = "resa5")

kolnamn = c("respondentid", "avstand", "b11a", "b11b", "b2", "b3_lat", "b3_lng", "b5.1", "b5.2", "b5.3", 
            "b5.4", "b5.5", "b5.6", "b5.7", "b6", "b7a.1", "b7a.2", "b7a.3", "b7a.4", "b7b.1", 
            "b7b.2", "b7b.3", "b7b.4", "b7.1", "b7.2", "b7.3", "b7.4", "b7.5", "b7.6", "b7.7", 
            "b8a", "b8b", "b9_lat", "b9_lng",
            "resa.no")

colnames(resa1) = paste(kolnamn)
colnames(resa2) = paste(kolnamn)
colnames(resa3) = paste(kolnamn)
colnames(resa4) = paste(kolnamn)
colnames(resa5) = paste(kolnamn)

# setdiff(resa1, resa5)

rvu= bind_rows(resa1, resa2, resa3, resa4, resa5)

rvu = rvu %>% dplyr::select("resa.no", "respondentid", "avstand", "b2", "b3_lat", "b3_lng", "b5.1", "b5.2", "b5.3", 
"b5.4", "b5.5", "b5.6", "b5.7", "b6", "b7a.1", "b7a.2", "b7a.3", "b7a.4", "b7b.1", 
"b7b.2", "b7b.3", "b7b.4", "b7.1", "b7.2", "b7.3", "b7.4", "b7.5", "b7.6", "b7.7", 
"b8a", "b8b", "b9_lat", "b9_lng", "b11a", "b11b")

# rvu = rvu %>% left_join(., meta, by = "respondentid")


### skapa geometadata för alla resor ###

UnikKoordinatStart = rvu %>% 
  dplyr::select(b3_lng, b3_lat) %>% 
  filter(!is.na(b3_lng), !is.na(b3_lat)) %>%
  distinct(b3_lng, b3_lat)

UnikKoordinatStop = rvu %>% 
  dplyr::select(b9_lng, b9_lat) %>% 
  filter(!is.na(b9_lng), !is.na(b9_lat)) %>%
  distinct(b9_lng, b9_lat)

UnikKoordinatStartKopia = UnikKoordinatStart
UnikKoordinatStopKopia = UnikKoordinatStop

coordinates(UnikKoordinatStartKopia) = ~b3_lng+b3_lat 
coordinates(UnikKoordinatStopKopia) = ~b9_lng+b9_lat 

proj4string(UnikKoordinatStartKopia) = CRS("+init=epsg:4326") # define original CRS (WGS84)
proj4string(UnikKoordinatStopKopia) = CRS("+init=epsg:4326")


# transformera koordinater från WGS84 till SWEREF
UnikKoordinatStartKopia_sweref <- spTransform(UnikKoordinatStartKopia, CRS(SWEREF99TM))
UnikKoordinatStartKopia_sweref <- as_data_frame(UnikKoordinatStartKopia_sweref)

UnikKoordinatStopKopia_sweref <- spTransform(UnikKoordinatStopKopia, CRS(SWEREF99TM))
UnikKoordinatStopKopia_sweref <- as_data_frame(UnikKoordinatStopKopia_sweref)



# merge hpl coordinates with shapefiles 
start_kommun <- data.frame(coordinates(UnikKoordinatStartKopia_sweref),
                           extract(kommun, UnikKoordinatStartKopia_sweref))
start_tatort <- data.frame(coordinates(UnikKoordinatStartKopia_sweref),
                           extract(tatort, UnikKoordinatStartKopia_sweref))
start_deso <- data.frame(coordinates(UnikKoordinatStartKopia_sweref),
                         extract(deso, UnikKoordinatStartKopia_sweref))
start_ruta500 <- data.frame(coordinates(UnikKoordinatStartKopia_sweref),
                            extract(ruta500, UnikKoordinatStartKopia_sweref))
start_ruta1000 <- data.frame(coordinates(UnikKoordinatStartKopia_sweref),
                             extract(ruta1000, UnikKoordinatStartKopia_sweref))


stop_kommun <- data.frame(coordinates(UnikKoordinatStopKopia_sweref),
                          extract(kommun, UnikKoordinatStopKopia_sweref))
stop_tatort <- data.frame(coordinates(UnikKoordinatStopKopia_sweref),
                          extract(tatort, UnikKoordinatStopKopia_sweref))
stop_deso <- data.frame(coordinates(UnikKoordinatStopKopia_sweref),
                        extract(deso, UnikKoordinatStopKopia_sweref))
stop_ruta500 <- data.frame(coordinates(UnikKoordinatStopKopia_sweref),
                           extract(ruta500, UnikKoordinatStopKopia_sweref))
stop_ruta1000 <- data.frame(coordinates(UnikKoordinatStopKopia_sweref),
                            extract(ruta1000, UnikKoordinatStopKopia_sweref))


# append files: alla hpl med all meta data
start_meta = cbind(start_kommun[,c("b3_lng", "b3_lat", "KOMMUNNAMN", "LANSNAMN")], 
                   start_tatort[,c("KATEGORI", "NAMN1")], 
                   start_deso[,c("Deso")], 
                   start_ruta500[,c("Rut_id")],
                   start_ruta1000[,c("Rut_id")],
                   UnikKoordinatStart)

stop_meta = cbind(stop_kommun[,c("b9_lng", "b9_lat", "KOMMUNNAMN", "LANSNAMN")], 
                  stop_tatort[,c("KATEGORI", "NAMN1")], 
                  stop_deso[,c("Deso")], 
                  stop_ruta500[,c("Rut_id")],
                  stop_ruta1000[,c("Rut_id")],
                  UnikKoordinatStop)


colnames(start_meta) = c("b3_lng_sweref", "b3_lat_sweref", "StartKommunNamn", 
                         "StartLanNamn", "StartTatortKat", "StartTatortNamn", 
                         "StartDesoID", "StartRut500ID", "StartRut1000ID", 
                         "Start_Wgs84lng", "Start_Wgs84lat")

colnames(stop_meta) = c("b9_lng_sweref", "b9_lat_sweref", "StopKommunNamn", 
                        "StopLanNamn", "StopTatortKat", "StopTatortNamn", 
                        "StopDesoID", "StopRut500ID", "StopRut1000ID", 
                        "Stop_Wgs84lng", "Stop_Wgs84lat")

rvu$ConcatStart = paste(rvu$b3_lng, rvu$b3_lat)
rvu$ConcatStop = paste(rvu$b9_lng, rvu$b9_lat)

start_meta$concat = paste(start_meta$Start_Wgs84lng, start_meta$Start_Wgs84lat)
start_meta = start_meta %>% dplyr::select(-b3_lng_sweref, -b3_lat_sweref)

stop_meta$concat = paste(stop_meta$Stop_Wgs84lng, stop_meta$Stop_Wgs84lat)
stop_meta = stop_meta %>% dplyr::select(-b9_lng_sweref, -b9_lat_sweref)

rvu = rvu %>% 
  left_join(., start_meta, by = c("ConcatStart" = "concat")) %>%
  left_join(., stop_meta, by = c("ConcatStop" = "concat"))


# add fardmedel kolumn
rvu$fardmedel = ifelse(!is.na(rvu$b6), rvu$b6, 
                          ifelse(is.na(rvu$b6) & rvu$b5.1 == 1, "1",
                                 ifelse(is.na(rvu$b6) & rvu$b5.2 == 1, "2",
                                        ifelse(is.na(rvu$b6) & rvu$b5.3 == 1, "3",
                                               ifelse(is.na(rvu$b6) & rvu$b5.4 == 1, "4",
                                                      ifelse(is.na(rvu$b6) & rvu$b5.5 == 1, "5",
                                                             ifelse(is.na(rvu$b6) & rvu$b5.6 == 1, "6",
                                                                    ifelse(is.na(rvu$b6) & rvu$b5.7 == 1, "7","XXXX"))))))))


rvu$fardmedel.kat = with(rvu, ifelse(fardmedel==1, "Kollektiv",
                                     ifelse(fardmedel==2 | fardmedel==3 | fardmedel==6, "Bil",
                                            ifelse(fardmedel==4, "Cykel",
                                                   ifelse(fardmedel==5, "Gang",
                                                          ifelse(fardmedel==7, "Annat", "XXXX"))))))

rvu = rvu %>% 
  mutate(fardmedelantal = rvu$b5.1+rvu$b5.2+rvu$b5.3+rvu$b5.4+rvu$b5.5+rvu$b5.6+rvu$b5.7)
  

## data cleaning

## ta bort rader utan resinformation
rvu1= rvu %>% filter(!is.na(b3_lat)) 


# ta bort rader med samma start och stop koordinater
rvu2 = rvu1 %>% 
  filter(rvu1$b3_lat!=rvu1$b9_lat) %>% 
  as.data.frame()

nrow(rvu) # alla rader
nrow(rvu1) # bara rader med resinformation, dvs med koordinater
nrow(rvu2) # bara rader där start lat och stop lat är inte samma 


#Trafikverket: Närmare 80 % av personerna i åldern 16-84 genomför åtminstone en resa under en vanlig vardag
# https://trafikverket.ineko.se/Files/sv-SE/11581/RelatedFiles/2012_237_resvaneundersokning_i_sydostra_sverige.pdf
length(unique(rvu$respondentid)) # 8569 (2017/01 - 2018/05)
length(unique(rvu1$respondentid)) # 5377 (63% av alla unika IDs) (2017/01 - 2018/05)
length(unique(rvu2$respondentid)) # 4070 (48%) (2017/01 - 2018/05)

# write to file
write.csv2(rvu, "Z:/a_data/kollbar/data_ul/rvu_alla_rader.csv", row.names=FALSE)
write.csv2(rvu1, "Z:/a_data/kollbar/data_ul/rvu_utan_na_rader.csv", row.names=FALSE)
write.csv2(rvu2, "Z:/a_data/kollbar/data_ul/rvu_utan_na_rader_och_samma_start_stop.csv", row.names=FALSE)



# a = rvu1 %>% left_join(., persondata, by = "respondentid") %>% group_by(ar.manad) %>% summarise(n = n())
# b = rvu2 %>% left_join(., persondata, by = "respondentid") %>% group_by(ar.manad) %>% summarise(n = n())
# c = a %>%
#   left_join(., b, by = "ar.manad") %>%
#   rename(Utan_NA = n.x, Utan_NA_SammaStartStop = n.y) %>%
#   mutate(AndelSammaStartStop = (Utan_NA - Utan_NA_SammaStartStop) / Utan_NA) %>%
#   print(., n=100)

