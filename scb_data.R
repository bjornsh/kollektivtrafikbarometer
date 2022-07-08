##################################################################################
### Syfte
##################################################################################

# Donwload open SCB data at DeSo level with pxweb library 
# and join to Kollbar person file



##################################################################################
### Clean start
##################################################################################

rm(list = ls())
gc()


##################################################################################
### Libraries etc
##################################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sf, mapview, pxweb)



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

### DeSO
url_shp = paste0(folder_github, "deso_2018_v2.zip")
get_shapefile(url_shp)
deso = st_read(paste0(folder_shapefile, "/", "DeSO_2018_v2.gpkg"))

deso = filter(deso, lan == "03") # extract data for Uppsala län

deso_include = deso$deso





##################################################################################
### Download öppna SCB data
##################################################################################

# 1) create query
# 2) download data
# 3) convert to df
# 4) fix data


year = "2021"


# use to identify query
pxweb_interactive()


####### Befolkning per DeSo
pxweb_query_list <- 
  list("Region"=c("*"),
       "Kon"=c("1+2"),
       "ContentsCode"=c("000005FC"),
       "Tid"=c("*"))

px_data <- 
  pxweb_get(url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101Y/FolkmDesoKonN",
                   query = pxweb_query_list)
       
pop <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text") %>% 
  filter(region %in% deso_include,
         år == year) %>% 
  select(deso = 1, pop = 4)


pop <- pop %>%
  left_join(.,
            # calculate area per deso
            deso %>%
              cbind(., area = as.numeric(st_area(.))) %>% 
              as.data.frame() %>% 
              # original is in m2, divide with 1000 to create nicer numbers
              mutate(area = area / 1000) %>% 
              select(deso, area), 
            by = "deso") %>% 
  mutate(pop_density = area / pop)


tab_pop_density = select(pop, deso, pop_density)






####### utbildningsnivå per deso
pxweb_query_list <- list("Region"=c("*"),
                         "UtbildningsNiva"=c("*"),
                         "ContentsCode"=c("000005MO"),
                         "Tid"=c("*"))


px_data <- pxweb_get(url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/UF/UF0506/UF0506YDeso/UtbSUNBefDesoRegso",
                     query = pxweb_query_list)

# Convert to data.frame 
education <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")       

tab_edu = education %>% 
  filter(region %in% deso_include,
         år == year,
         utbildningsnivå != "uppgift om utbildningsnivå saknas") %>% 
  group_by(region) %>% 
  mutate(andel = Befolkning / sum(Befolkning)) %>% 
  filter(utbildningsnivå == "eftergymnasial utbildning, 3 år eller mer") %>% 
  select(deso = 1, andel_eftergymnasial_utbildning = 5)
  
  
  

####### Personbilar i trafik
pxweb_query_list <- 
  list("Region"=c("*"),
       "Bestand"=c("ITRAF"),
       "ContentsCode"=c("000005Z2"),
       "Tid"=c("*"))

px_data <- pxweb_get(url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/TK/TK1001/TK1001Z/PersBilarDeso",
                     query = pxweb_query_list)

tab_bil <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text") %>% 
  filter(region %in% deso_include,
         år == year) %>% 
  select(deso = 1, personbilar = 4) %>% 
  left_join(., pop[,c("deso", "pop")], by = "deso") %>% 
  mutate(bil_per_pop = round(personbilar / pop, 3)) %>% 
  select(-pop)
  




###### Bostad Upplatelseform
pxweb_query_list <- 
  list("Region"=c("*"),
       "Upplatelseform"=c("1","2","3","ÖVRIGT"),
       "ContentsCode"=c("000004F3"),
       "Tid"=c("2015","2016","2017","2018","2019","2020","2021"))

# Download data 
px_data <- pxweb_get(url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/BO/BO0104/BO0104X/BO0104T10",
                     query = pxweb_query_list)

# Convert to data.frame 
tab_bostad <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text") %>% 
  filter(region %in% deso_include,
         år == year) %>% 
  rename(typ = 2, antal = 4) %>% 
  group_by(region) %>% 
  mutate(andel = antal / sum(antal)) %>% 
  filter(typ != "uppgift saknas") %>% 
  select(-3, -4) %>% 
  pivot_wider(names_from = typ, values_from = andel) %>% 
  rename(deso = 1)




##### Inkomst
pxweb_query_list <- 
  list("Region"=c("*"),
       "Inkomstkomponenter"=c("10"),
       "Kon"=c("1+2"),
       "ContentsCode"=c("000005FW"),
       "Tid"=c("*"))

px_data <- pxweb_get(url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/HE/HE0110/HE0110I/Tab2InkDesoN",
                     query = pxweb_query_list)

tab_income <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text") %>% 
  filter(region %in% deso_include,
         år == "2020") %>% 
  select(deso = 1, medel_employment_income = 5)





##################################################################################
### Create final output
##################################################################################

# create list with all output files
temp = mget(ls(patter = "tab"))

# join all output files
final = plyr::join_all(temp, by = "deso", type = "left", match = "all") %>% 
  janitor::clean_names(.)


write.csv2(final, paste0(folder_output, "scb_deso_data.csv"), 
           row.names = FALSE)




### to add data directly to person file run

# # load Kollbar person data
# pers <- read_delim(paste0(folder_output, "person.csv"), ";", escape_double = FALSE, 
#                    locale = locale(encoding = "ISO-8859-1"), 
#                    trim_ws = TRUE)
# 
# # join SCB data to Kollbar person file
# pers = left_join(pers, final, by = c("bostad_deso_kod" = "deso"))
# 
# write.csv2(pers, paste0(folder_output, "person.csv"), 
#            row.names = FALSE)





