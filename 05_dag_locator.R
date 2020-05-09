#make dag_id
ccp_data = ccp_data %>% 
  mutate(dag_id = gsub("\\-.*","", subjid)) %>% 
  mutate(dag_id = str_replace_all(dag_id, 'O', '0')) %>% 
  mutate(dag_id = ifelse(dag_id == 'RGT02', 'RTG02', dag_id)) %>% 
  mutate(dag_id = ifelse(dag_id == 'RLB14', 'RBL14', dag_id)) %>% 
  select(subjid, dag_id, everything())

#New bit
area_ethnicity_estimates = area_ethnicity_estimates = read_csv('https://raw.githubusercontent.com/SurgicalInformatics/population_profiles_health_authorities_uk/master/data_out_ccp_lookup_with_population_level_estimate/ccp_ethnicity_out.csv') %>% distinct(dag_id_e, .keep_all = TRUE) 

#add it to ccp_data
ccp_data = ccp_data %>% left_join(area_ethnicity_estimates, by = c('dag_id' = 'dag_id_e')) 

rm(area_ethnicity_estimates)
