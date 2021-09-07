##################################################################################
### Script description
##################################################################################

# In case wrong data was delivered or the data management process has changed, 
# eg new variables, there is a need to remove data consistently across 
# output files prior to a rerun of the process


##################################################################################
### Set up
##################################################################################

### clean start
rm(list = ls())
gc()


### Libraries
library(dplyr)


##################################################################################
### Define time period to be removed
##################################################################################

time_out = c("202108") # tex c("202108", "202101")




##################################################################################
### Define folder path, function
##################################################################################

# Read keys 
api_fil = read_file("Z:/api")
kollbar_data = gsub('^.*kollbar_data: \\s*|\\s*\r.*$', "", api_fil)


# define local paths
folder_output = paste0(kollbar_data, "output/")


# function
`%notin%` <- Negate(`%in%`)



##################################################################################
### Load data and remove rows
##################################################################################

pers = read.csv2(paste0(folder_output, "person.csv"))
attityd = read.csv2(paste0(folder_output, "attityd.csv"))
rvu = read.csv2(paste0(folder_output, "rvu.csv"))


# Identify respondent ID to be remove
exclude = pers %>%
  filter(ar.manad %in% time_out) %>% 
  dplyr::select(respondentid) %>% 
  pull()


# Remove data
pers_final = pers %>% 
  filter(respondentid %notin% exclude)

attityd_final = attityd %>% 
  filter(respondentid %notin% exclude)

rvu_final = rvu %>% 
  filter(respondentid %notin% exclude)



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
