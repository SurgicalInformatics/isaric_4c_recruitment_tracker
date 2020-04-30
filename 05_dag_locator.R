# API REDCap patient data pull excluding records where removal has been approved
#!/usr/bin/env Rscript
library(RCurl)
library(tidyverse)
library(ggmap)
library(sf)
library(sp)
library(geogrid)
library(stringr)

#make dag_id
ccp_data = ccp_data %>% 
  mutate(dag_id = gsub("\\-.*","", subjid)) %>% 
  mutate(dag_id = str_replace_all(dag_id, 'O', '0')) %>% 
  mutate(dag_id = ifelse(dag_id == 'RGT02', 'RTG02', dag_id)) %>% 
  mutate(dag_id = ifelse(dag_id == 'RLB14', 'RBL14', dag_id)) %>% 
  select(subjid, dag_id, everything())

#New bit
area_ethnicity_estimates = read_csv('imd_lookups/ccp_ethnicity_out_30-April-2020.csv') %>% distinct(dag_id_e, .keep_all = T)

#add it to ccp_data
ccp_data = ccp_data %>% left_join(area_ethnicity_estimates, by = c('dag_id' = 'dag_id_e'))

# The API call can randomly fail
# Let's try at least 5 times before we give up
tries = 0
# by default, NA gets the class "logical"
data_labels = NA
while (tries == 0 | (tries < 5 & inherits(data, "try-error"))){
  data_labels = try(postForm(
    uri='https://ncov.medsci.ox.ac.uk/api/',
    token=Sys.getenv("ccp_token"),
    content='record',
    format='csv',
    type='flat',
    'fields[0]'='subjid',
    rawOrLabel='label',
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
data_labels = read_csv(data_labels, na = "", guess_max = 20000)

dag_labels = data_labels %>% 
  filter(!is.na(redcap_data_access_group)) %>% 
  select(subjid, redcap_data_access_group) %>% 
  mutate(dag_id = gsub("\\-.*","", subjid)) %>% 
  mutate(dag_id = str_replace_all(dag_id, 'O', '0')) %>% 
  distinct(dag_id, .keep_all = T) %>% 
  select(dag_id, everything(), -subjid) %>% 
  rename(redcap_data_access_group_labelled = redcap_data_access_group) 

ccp_data = ccp_data %>% 
  left_join(dag_labels, by = 'dag_id')

master = readRDS('geoJSONs/master.rds')

# #sort lanarkshire
# dag_lookup = dag_lookup %>% #sensitivity maximising search
#   mutate(dag_label = str_replace(dag_label, 'Lanarkshire NHSFT', 'Monklands Hospital, Lanarkshire')) %>% 
#   mutate(dag_label = str_replace(dag_label, 'NHS Foundation Trust', '')) %>% 
#   mutate(dag_label = str_replace(dag_label, 'Hospitals', 'Hospital')) %>% 
#   mutate(dag_label = str_replace(dag_label, '&', 'and')) %>% 
#   mutate(dag_label = ifelse(grepl('R1F01', dag_id, ignore.case = T), paste(dag_label, 'Paddington'), dag_label)) %>% 
#   mutate(dag_label = ifelse(grepl('hospital', dag_label, ignore.case = T), dag_label, paste(dag_label, 'Hospital'))) %>% 
#   mutate(dag_label = ifelse(grepl('kent & canterbury', dag_label, ignore.case = T), paste('Kent and Canterbury Hospital, Ethelbert Rd, Canterbury CT1 3NG'), 
#                             dag_label)) %>% 
#   mutate(dag_label = paste(dag_label, 'United Kingdom'))
# 
#                 
# #Now search them
# #API/ other variables
# api_key = 'AIzaSyADK-Xycf5uqnyvYDPP58wSQdnLJZCzrr8' #TD API key linked to my credit card so don't use lots!
# register_google(api_key)
# # 
# #required geoJSONs
# # source('geoJSONs/format_geoJSONs.R')

source('06_imd_mapping.R')

# #search
# result_lat_long = geocode(dag_lookup$dag_label, output = "latlona", source = "google") 
# 
# dag_lookup = cbind(dag_lookup, result_lat_long) %>% 
#   filter(dag_label != 'Export only' & !is.na(address))

# #Now find out which ones in Scotland
# coords = ccp_data[ , c("lon", "lat")]   # coordinates - sorry for sq brackets
# data_sp   = ccp_data[ , c('dag_id')]          # data
# crs    = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") # proj4string of coords
# 
# # make the spatial points data frame object
# spdf_lookup = SpatialPointsDataFrame(coords = coords,
#                                      data = data_sp, 
#                                      proj4string = crs)
# 
# lookup_of_centres = sp::over(spdf_lookup, master, fn = NULL) #do the join
# 
# lookup_of_centres = cbind(dag_lookup, lookup_of_centres)
# save(lookup_of_centres, file = "dag_lookup_coords_imd.rda")

rm(area_ethnicity_estimates)