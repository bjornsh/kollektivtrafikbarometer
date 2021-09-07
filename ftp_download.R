##################################################################################
### Script description
##################################################################################

# Download Kollektivtrafikbarometer .csv file from password protected FTP server 
# and storing data as .xlsx file while avoiding encoding issues




##################################################################################
### Set up
##################################################################################

### clean start
rm(list = ls())
gc()


### load libraries etc
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, RCurl, writexl)


# avoid scientific notation
options(scipen=999)


### Create directories (if not already exist) or define specific path to indata/output
dir.create(file.path(getwd(), "data"))
dir.create(file.path(paste0(getwd(), "/data"), "input"))


# Read keys, define paths 
api_fil = read_file("Z:/api")
server = gsub('^.*kollbar_server: \\s*|\\s*\r.*$', "", api_fil)
userpwd = gsub('^.*kollbar_userpwd: \\s*|\\s*\r.*$', "", api_fil)
kollbar_data = gsub('^.*kollbar_data: \\s*|\\s*\r.*$', "", api_fil)

folder_input = paste0(kollbar_data, "input/")



##################################################################################
### Download .csv file from server
##################################################################################

### Define variables
datum = "202107" # Download data for which month? (Format "YYYYMM")
rkm = "UL" # Which RKM?
protocol = "sftp"


### Name of file to be downloaded
file_download = paste0("Databas Kollektivtrafikbarometern ", datum, " ", rkm)


### get names of files on server
url0 = paste0(protocol, "://", server) # for file names only
filenames = getURL(url = url0, userpwd=userpwd, dirlistonly = TRUE)
destnames = filenames <-  strsplit(filenames, "\r*\n")[[1]]


### Download Data
url <- paste0(protocol, "://", server, "/", paste0(file_download, ".csv"))
data <- getURL(url = url, userpwd=userpwd)


# create local df
# read.csv, read_delim etc failed to handle encoding
df = data.table::fread(data, 
                  sep = ";", 
                  encoding = 'UTF-8')


# Check if encoding is correct
names(df)[1] # correct first column name?
table(df$u_kommun) # correct Swedish spelling?


### Write as Excel
write_xlsx(df, paste0(folder_input, "kollbar_", datum, ".xlsx"))

