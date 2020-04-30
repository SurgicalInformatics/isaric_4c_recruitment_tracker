# API REDCap patient data pull excluding records where removal has been approved
#!/usr/bin/env Rscript
library(RCurl)
library(tidyverse)
# 
# The API call can randomly fail
# Let's try at least 5 times before we give up
tries = 0
# by default, NA gets the class "logical"
data = NA
while (tries == 0 | (tries < 5 & class(data) == "try-error")){
  data = try(postForm(
    uri='https://ncov.medsci.ox.ac.uk/api/',
    token=Sys.getenv("ccp_token"),
    content='record',
    format='csv',
    type='flat',
    rawOrLabel='raw',
    rawOrLabelHeaders='raw',
    exportCheckboxLabel='false',
    exportSurveyFields='false',
    exportDataAccessGroups='true',
    returnFormat='json'
  ))
  tries = tries + 1
  # let's wait a second letting the API cool off
  Sys.sleep(1)
}
data = read_csv(data, na = "", guess_max = 20000)
# load('redcap_pulled.RData')
# data = ccp_pulled_data

source("CCPUKSARI_R_2020-03-04_1532.r")

ccp_data = data

#Topline
source("05_dag_locator.R")

ccp_data = ccp_data %>% 
  ff_relabel_df(data)

#remove
rm(data)
