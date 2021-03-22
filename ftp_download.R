library(RCurl)
library(readr)
library(dplyr)
library(writexl)

rm(list = ls())



setwd("..")

api <- "Z:/" # location of file with keys
output_csv <- paste0(getwd(),"/data_ul/csv/") # path to folder for csv file
output_xlsx <- paste0(getwd(),"/data_ul/xlsx/")# path to folder for xlsx file


##### DOWNLOAD CSV FILE FROM SFTP SERVER
protocol <- "sftp"
rkm <- "UL"


## Read FTP server keys
api_fil <- read_file(paste0(api, "api"))
server <- gsub('^.*kollbar_server: \\s*|\\s*\r.*$', "", api_fil)
userpwd <- gsub('^.*kollbar_userpwd: \\s*|\\s*\r.*$', "", api_fil)



## get filenames
url0 <- paste0(protocol, "://", server) # for file names only
filenames <- getURL(url = url0, userpwd=userpwd, dirlistonly = TRUE)
destnames <- filenames <-  strsplit(filenames, "\r*\n")[[1]]


## Download Data

## create filename for last month
tsfrFilename <- paste0("Databas Kollektivtrafikbarometern ",
                  paste0(format(Sys.Date(), "%Y"), paste0("0", as.integer(format(Sys.Date(), "%m")) - 1)), " ",
                  rkm)

ouptFilename <- paste0(output_csv, tsfrFilename, ".csv")

url <- paste0(protocol, "://", server, "/", tsfrFilename)
data <- getURL(url = url, userpwd=userpwd)

## Create File and save as csv
fconn <- file(ouptFilename)
write(data, fconn)
close(fconn)

### Write as Excel
dat = read.csv2(paste0(output_csv, tsfrFilename, ".csv"), encoding="UTF-8")

dat = rename(dat, "A1" = 1) # csv import changes name of first column. Change back to correct name

write_xlsx(dat, paste0(output_xlsx, tsfrFilename, ".xlsx"))








