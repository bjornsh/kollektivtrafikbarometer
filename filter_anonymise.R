##################################################################################
### Script description
##################################################################################

# Create anonymised Kollektivtrafikbarometer data files excluding GDPR sensitive variables 
# for eg sharing outside the organisation. Script works with data files with the
# data and folder structure created by script
# https://github.com/bjornsh/kollektivtrafikbarometer/blob/master/create_attityd_rvu_person_fil.R
# https://github.com/bjornsh/kollektivtrafikbarometer/blob/master/google_distance.R



##################################################################################
### Set up
##################################################################################

### clean start
rm(list = ls())
gc()


### Libraries
library(dplyr)



### Read keys, define paths 
api_fil = read_file("Z:/api")
kollbar_data = gsub('^.*kollbar_data: \\s*|\\s*\r.*$', "", api_fil)

folder_input = paste0(kollbar_data, "input/")
folder_output = paste0(kollbar_data, "output/")

dir.create(file.path(paste0(folder_output, "sample")))



##################################################################################
### Filter
##################################################################################

### define sample 
# spcific kommun: kommun = c("håbo", "enköping", "knivsta")
# alla kommuner: kommun = c()

kommun = c("håbo") # c("håbo", "enköping", "knivsta")
tid = c("2021")



##################################################################################
### Read data
##################################################################################

pers = read.csv2(paste0(folder_output, "person.csv"))
attityd = read.csv2(paste0(folder_output, "attityd.csv"))
rvu = read.csv2(paste0(folder_output, "rvu.csv"))
restid = read.csv2(paste0(folder_output, "rvu_restider.csv"))



##################################################################################
### Create files
##################################################################################

### respondent data

if (length(kommun) > 0 & length(tid) > 0) {
  pers = pers %>% 
    # remove interviewees from outside target kommun and time period
    filter(u_kommun %in% kommun & ar %in% tid) %>% 
    # remove GDPR sensitive data
    dplyr::select(-bostad_rutid_500, 
                  -bostad_rutid_1000, 
                  -b3_r1_lng, 
                  -b3_r1_lat,
                  -u_postnummer, 
                  -u_postort,
                  -h6,
                  -contains("hemadress"))
} else {
  pers = pers %>% 
  # remove GDPR sensitive data
  dplyr::select(-contains("rutid"),
                -contains("lat"),
                -contains("lng"),
                -u_postnummer, 
                -u_postort,
                -h6,
                -contains("hemadress"))
}


# Create vector with unique personal IDs
inklud = pers %>% dplyr::select(respondentid) %>% pull()



### Attityd data
attityd = attityd %>% 
  filter(respondentid %in% inklud) %>% 
  # remove variables containing open questions as these may contain personal data
  dplyr::select(-contains("open"), 
                -contains("avstand"))



### RVU data
rvu = rvu %>% 
  filter(respondentid %in% inklud) %>% 
  # remove GDPR sensitive data
  dplyr::select(-avstand,
                -contains("rutid"),
                -contains("lat"),
                -contains("lng"))



### Google travel time and distance data
restid = restid %>% 
  filter(respondentid %in% inklud) %>% 
  # remove GDPR sensitive data
  dplyr::select(-contains("koord"))




##################################################################################
### Save files
##################################################################################

write.csv2(pers, paste0(folder_output, "sample/person_", kommun, tid,  ".csv"), row.names = FALSE)
write.csv2(attityd, paste0(folder_output, "sample/attityd_", kommun, tid,  ".csv"), row.names = FALSE)
write.csv2(rvu, paste0(folder_output, "sample/rvu_", kommun, tid,  ".csv"), row.names = FALSE)
write.csv2(restid, paste0(folder_output, "sample/restid_", kommun, tid,  ".csv"), row.names = FALSE)




