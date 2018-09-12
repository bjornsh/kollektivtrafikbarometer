library(reshape2)
library(dplyr)
library(readxl)

##Read files names
filenames <- list.files(path="02_input_data/xlsx",pattern="*xlsx")

#### put all dfs into a single list
setwd("C:\xxxx\xxx") # set to folder where data is stored

files <- list()
for(i in filenames){
  files[[i]] <- read_xlsx(i, col_names = TRUE) } 

# change col names to lower
for(i in 1:length(files)){
  names(files[[i]]) <- tolower(names(files[[i]]))
}

# replace Swedish characters and # in column names
for(i in 1:length(files)){
  names(files[[i]]) <- gsub("å", "a", (names(files[[i]])))
  names(files[[i]]) <- gsub("ä", "a", (names(files[[i]])))
  names(files[[i]]) <- gsub("ö", "o", (names(files[[i]])))
  names(files[[i]]) <- gsub("#", "_", (names(files[[i]])))
}


# number of columns can vary between months, hence extract a set of useful columns
files = lapply(files, "[", c("a1", "a2", "a4", "a6", "a7", "a5a_open", "a5a_1", "a5a_2", "a5a_3", "a5a_4", "a5a_5", 
                             "a5a_6", "a5a_7", "a5a_8", "a5a_9", "a5b_open", "a5b_1", "a5b_2", "a5b_3", "a5b_4", "a5b_5", 
                             "a5b_6", "a5b_7", "a5b_8", "a5b_9", "a5b_10", "a5b_11", "a5b_12", "a7a", "a7b", "avstand_1", 
                             "avstand_2", "avstand_3", "avstand_4", "avstand_5", "avstand_1_kod", "avstand_2_kod", "avstand_3_kod", 
                             "avstand_4_kod", "avstand_5_kod", "b1", "b10_r1", "b10_r2", "b10_r3", "b10_r4", "b10_r5", "b11a_r1", 
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
                             "b5r5_6", "b5r5_7", "b6_r1", "b6_r2", "b6_r3", "b6_r4", "b6_r5", "b7ar1_1", "b7ar1_2", "b7ar1_3", "b7ar1_4", 
                             "b7ar2_1", "b7ar2_2", "b7ar2_3", "b7ar2_4", "b7ar3_1", "b7ar3_2", "b7ar3_3", "b7ar3_4", "b7ar4_1", "b7ar4_2", 
                             "b7ar4_3", "b7ar4_4", "b7ar5_1", "b7ar5_2", "b7ar5_3", "b7ar5_4", "b7br1_1", "b7br1_2", "b7br1_3", "b7br1_4", 
                             "b7br2_1", "b7br2_2", "b7br2_3", "b7br2_4", "b7br3_1", "b7br3_2", "b7br3_3", "b7br3_4", "b7br4_1", "b7br4_2", 
                             "b7br4_3", "b7br4_4", "b7br5_1", "b7br5_2", "b7br5_3", "b7br5_4", "b7r1_1", "b7r1_2", "b7r1_3", "b7r1_4", 
                             "b7r1_5", "b7r1_6", "b7r2_1", "b7r2_2", "b7r2_3", "b7r2_4", "b7r2_5", "b7r2_6", "b7r2_7", "b7r3_1", 
                             "b7r3_2", "b7r3_3", "b7r3_4", "b7r3_5", "b7r3_6", "b7r3_7", "b7r4_1", "b7r4_2", "b7r4_3", "b7r4_4", "b7r4_5", 
                             "b7r4_6", "b7r4_7", "b7r5_1", "b7r5_2", "b7r5_3", "b7r5_4", "b7r5_5", "b7r5_6", "b7r5_7", "b8a_r1", "b8a_r2", 
                             "b8a_r3", "b8a_r4", "b8a_r5", "b8a_r1_open", "b8a_r2_open", "b8a_r3_open", "b8a_r4_open", "b8a_r5_open", "b8b_r1", 
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


# bind all files into single df
fin <- do.call(rbind, files)

# create some useful variables
fin$ar = with(fin, ifelse(ar==8, "2017", ifelse(ar==9, "2018", "XXX")))
fin$alder = fin$h1+12

# check number of interviews per month
table(fin$ar,fin$manad)


# fix variables
fin$kon= ifelse(fin$u_konkod=="1", "Man",
                ifelse(fin$u_konkod=="2", "Kvinna",
                       ifelse(fin$u_konkod=="3", "Okänt", "XXX")))

fin$u_postort= tolower(fin$u_postort)
fin$u_kommun= tolower(fin$u_kommun)
fin$u_jordbruksverketsdef= tolower(fin$u_jordbruksverketsdef)
fin$u_trafiktyp= tolower(fin$u_trafiktyp)

fin$ar.manad=paste(fin$ar,fin$manad,sep="/") # year/month numeric
fin$manad.text = month.abb[fin$manad] # month abbreviated

# which day and date did last journey happen
fin$responsedate.weekday = weekdays(as.Date(fin$responsedate,'%m/%d/%Y'))
fin$traveldate = as.Date(fin$responsedate,'%m/%d/%Y')-1
fin$traveldate.weekday = weekdays(as.Date(fin$responsedate,'%m/%d/%Y')-1)


##### write to disk
write.table(fin, file = "location/file_name.csv", sep = ";", row.names=FALSE)
