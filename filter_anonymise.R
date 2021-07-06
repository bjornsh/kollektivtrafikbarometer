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


### Create directory 
dir.create(file.path(paste0(getwd(), "/data/output/sample")))



# define folder path
path = paste0(getwd(), "/data/output/")
path_sample = paste0(getwd(), "/data/output/sample/")



##################################################################################
### Filter
##################################################################################

### define sample 
# spcific kommun: kommun = c("håbo", "enköping", "knivsta")
# alla kommuner: kommun = c()

kommun = c("håbo") # c("håbo", "enköping", "knivsta")




##################################################################################
### Read data
##################################################################################

pers = read.csv2(paste0(path, "person.csv"))
attityd = read.csv2(paste0(path, "attityd.csv"))
rvu = read.csv2(paste0(path, "rvu.csv"))
restid = read.csv2(paste0(path, "rvu_restider.csv"))



##################################################################################
### Create files
##################################################################################

### respondent data

if (length(kommun) > 0) {
  pers = pers %>% 
    # remove rows where person is not folkbokförd in target kommun
    filter(u_kommun %in% kommun) %>% 
    # remove GDPR sensitive data
    dplyr::select(-bostad_rutid_500, 
                  -bostad_rutid_1000, 
                  -b3_r1_lng, 
                  -b3_r1_lat,
                  -u_postnummer, 
                  -u_postort,
                  -h6)
} else {
  pers = pers %>% 
  # remove GDPR sensitive data
  dplyr::select(-contains("rutid"),
                -contains("lat"),
                -contains("lng"),
                -u_postnummer, 
                -u_postort,
                -h6)
}


# Create vector with unique personal IDs
inklud = pers %>% dplyr::select(respondentid) %>% pull()



### Attityd data
attityd = attityd %>% 
  filter(respondentid %in% inklud) %>% 
  # remove variables containing open questions as these may contain personal data
  dplyr::select(-contains("open"))



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

write.csv2(pers, paste0(path_sample, "person_sample_", Sys.Date(), ".csv"), row.names = FALSE)
write.csv2(attityd, paste0(path_sample, "attityd_sample_", Sys.Date(), ".csv"), row.names = FALSE)
write.csv2(rvu, paste0(path_sample, "rvu_sample_", Sys.Date(), ".csv"), row.names = FALSE)
write.csv2(restid, paste0(path_sample, "rvu_restid_sample_", Sys.Date(), ".csv"), row.names = FALSE)




