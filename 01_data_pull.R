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
    'events[0]'='day_1_hospital_adm_arm_1',
    'events[1]'='dischargedeath_arm_1',
    'events[2]'='day_1_hospitalicu_arm_2',
    'events[3]'='dischargedeath_arm_2',
    'events[4]'='day_1_arm_3',
    'events[5]'='dischargedeath_arm_3',
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
rm(data)

#Topline
source("05_dag_locator.R")

#clear memory
gc()

#remove
rm(tries)
