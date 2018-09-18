
##### Append journeys into same columns

library(dplyr,lib.loc="C:/R/Rpackages")


dat=read.csv("02_input_data/kolbart_2017-nu.csv", header=T, sep = ";")

# select all columns linked to first, second, third etc journey and paste into separate df
resa1= dat %>% select(respondentid, responsedate, avstand_1, contains("r1"), traveldate, traveldate.weekday) %>% 
  select(respondentid, responsedate, avstand_1, matches("lat|lng|b2|b5|b6|b7|b8|b11"), traveldate, traveldate.weekday) %>% 
  select(-contains("open")) %>%
  mutate(resa.no = "resa1")

resa2= dat %>% select(respondentid, responsedate, avstand_2, contains("r2"), traveldate, traveldate.weekday) %>% 
  select(respondentid, responsedate, avstand_2, matches("lat|lng|b2|b5|b6|b7|b8|b11"), traveldate, traveldate.weekday) %>% 
  select(-contains("open")) %>%
  mutate(resa.no = "resa2")

resa3= dat %>% select(respondentid, responsedate, avstand_3, contains("r3"), traveldate, traveldate.weekday) %>% 
  select(respondentid, responsedate, avstand_3, matches("lat|lng|b2|b5|b6|b7|b8|b11"), traveldate, traveldate.weekday) %>% 
  select(-contains("open")) %>%
  mutate(resa.no = "resa3")

resa4= dat %>% select(respondentid, responsedate, avstand_4, contains("r4"), traveldate, traveldate.weekday) %>% 
  select(respondentid, responsedate, avstand_4, matches("lat|lng|b2|b5|b6|b7|b8|b11"), traveldate, traveldate.weekday) %>% 
  select(-contains("open")) %>%
  mutate(resa.no = "resa4")

resa5= dat %>% select(respondentid, responsedate, avstand_5, contains("r5"), traveldate, traveldate.weekday) %>% 
  select(respondentid, responsedate, avstand_5, matches("lat|lng|b2|b5|b6|b7|b8|b11"), traveldate, traveldate.weekday) %>% 
  select(-contains("open")) %>%
  mutate(resa.no = "resa5")

# standardise column names
kolnamn = c("respondentid", "responsedate", "avstand", "b11a", "b11b", "b2", "b3_lat", "b3_lng", "b5.1", "b5.2", "b5.3", 
            "b5.4", "b5.5", "b5.6", "b5.7", "b6", "b7a.1", "b7a.2", "b7a.3", "b7a.4", "b7b.1", 
            "b7b.2", "b7b.3", "b7b.4", "b7.1", "b7.2", "b7.3", "b7.4", "b7.5", "b7.6", "b7.7", 
            "b8a", "b8b", "b9_lat", "b9_lng", "traveldate", "traveldate.weekday", "resa.no")

colnames(resa1) = paste(kolnamn)
colnames(resa2) = paste(kolnamn)
colnames(resa3) = paste(kolnamn)
colnames(resa4) = paste(kolnamn)
colnames(resa5) = paste(kolnamn)


# append all dataframes into on df
rvu= bind_rows(resa1,resa2, resa3, resa4, resa5)

# nrow(rvu)
# rvu %>% select(b2, b3_lat) %>% filter(is.na(b2) & is.na(b3_lat)) %>% nrow()
# rvu %>% select(b2, b3_lat) %>% filter(!is.na(b2) & !is.na(b3_lat)) %>% nrow()
# rvu %>% select(b2, b3_lat) %>% filter(!is.na(b2) & is.na(b3_lat)) %>% nrow()
# rvu %>% select(b2, b3_lat) %>% filter(is.na(b2) & !is.na(b3_lat)) %>% nrow()
# 

# data cleaning 
rvu1= rvu %>% filter(!is.na(b3_lat)) # remove rows without travel information

rvu2 = rvu1 %>% filter(rvu1$b3_lat!=rvu1$b9_lat) %>% as.data.frame() # remove rows where start and stop coordinates are the same

#Number of rows in resulting dataframes
nrow(rvu) # original data
nrow(rvu1) # data containing coordinates
nrow(rvu2) # data containing coordinates but not same start and stop coordinates

