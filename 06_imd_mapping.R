#Build IMD lookups
library(tidyverse)
library(janitor)

#IMDs
england_imd_19 = read_csv('imd_lookups/imd_lookup_england_2019.csv')
wales_imd_19 = read_csv('imd_lookups/imd_lookup_wales_2019.csv')
scotland_imd_20 = read_csv('imd_lookups/imd_lookup_scotland_2020.csv')
ni_imd_17 = read_csv('imd_lookups/nimdm_2017.csv')

#Data zones/ LSOAs
scotland_datazones = read_csv('imd_lookups/scottish_data_areas.csv')
england_lsoas = read_csv('imd_lookups/england_lsoa_to_ccg.csv') %>% clean_names()
ni_sa_to_lgd = readxl::read_xls('imd_lookups/ni_sa_to_lgd.xls')
ni_soa_to_hsct = read_csv('imd_lookups/soa_to_hsct_ni.csv') %>% select(SOA, HSCT)
wales_hb_names = read_csv('imd_lookups/wales_hb_names.csv')

#To get wales (no direct linkages across lsoa and HB)
#This is derived from all postcodes of the UK https://geoportal.statistics.gov.uk/datasets/national-statistics-postcode-lookup-february-2020
#Needs unzipped into imd_lookups folder
#wales_lsoa_lookup = read_csv('imd_lookups/Data/NSPL_FEB_2020_UK.csv') %>%  filter(startsWith(lsoa11, 'W')) %>% select(lsoa11, ccg, hlthau)
# write_csv(wales_lsoa_lookup, 'imd_lookups/wales_lsoa_to_hb.csv')
wales_lsoa_lookup = read_csv('imd_lookups/wales_lsoa_to_hb.csv')

wales_imd = wales_imd_19 %>% 
  left_join(wales_lsoa_lookup, by = c('lsoa11cd' = 'lsoa11')) %>% 
  group_by(hlthau) %>% 
  mutate(wa_average_imd = median(wimd_2019)) %>% 
  ungroup() %>% 
  left_join(wales_hb_names, by = c('hlthau' = 'LHB19CD')) %>% 
  distinct(hlthau, .keep_all = T) %>% 
  select(hlthau, LHB19NM, wa_average_imd) %>% 
  arrange(desc(wa_average_imd)) %>% 
  mutate(wales_average_imd_rank = 1:n()) %>% 
  select(-wa_average_imd) 

#make ni lookup
ni_imd = ni_soa_to_hsct %>% 
  left_join(ni_sa_to_lgd, by = c('SOA' = 'SOA2001')) %>% 
  left_join(ni_imd_17, by = c('LGD2014' = 'LGD2014code')) %>% 
  group_by(HSCT) %>% 
  mutate(avg_income_per_hsct = median(Income_perc)) %>%  #done on income alone!!!
  distinct(HSCT, .keep_all = T) %>% 
  select(HSCT, avg_income_per_hsct) %>% 
  filter(!is.na(HSCT)) %>% 
  ungroup() %>% 
  arrange(desc(avg_income_per_hsct)) %>% 
  mutate(ni_average_imd_rank = 1:n()) %>% 
  select(-avg_income_per_hsct) %>% 
  mutate(HSCT = ifelse(HSCT == 'Western HSCT', 'WHSCT', HSCT),
         HSCT = ifelse(HSCT == 'Southern HSCT', 'SHSCT', HSCT),
         HSCT = ifelse(HSCT == 'South Eastern HSCT', 'SEHSCT', HSCT),
         HSCT = ifelse(HSCT == 'Belfast HSCT', 'BHSCT', HSCT),
         HSCT = ifelse(HSCT == 'Northern HSCT', 'NHSCT', HSCT))# This is v. generalised and not enough data granularity

#Build big lookups
scotland_imd_20 %>% 
  rename(DataZone = Data_Zone) %>% 
  left_join(scotland_datazones, by = 'DataZone') -> scotland_imd_lookup

england_imd_19 %>% 
  left_join(england_lsoas, by = c('ccg19cd' = 'ccg18cd')) -> england_ccg_imd_lookup

#group_by and select most useful things

scotland_imd_lookup %>% 
  select(HB, DataZone, SIMD2020_Rank) %>% 
  group_by(HB) %>% 
  mutate(average_imd_rank = median(SIMD2020_Rank)) %>% 
  select(-SIMD2020_Rank, -DataZone) %>% 
  distinct(HB, .keep_all = T) %>% 
  ungroup() %>% 
  arrange(-desc(average_imd_rank)) %>% 
  mutate(scotland_average_imd_rank = 1:n()) %>% 
  select(-average_imd_rank) -> scotland_hb_imd

england_ccg_imd_lookup %>% 
  select(ccg19cd, RAvgRank) %>% 
  rename(average_imd_rank = RAvgRank) %>% 
  distinct(ccg19cd, .keep_all = T)  -> england_ccg_imd

#now map these to redcap dataset

ccp_data = ccp_data %>% 
  left_join(scotland_hb_imd, by = c('ccg' = 'HB')) %>% 
  left_join(england_ccg_imd,  by = c('ccg' = 'ccg19cd')) %>% 
  left_join(wales_imd,  by = c('ccg' = 'hlthau')) %>% 
  mutate(average_imd_rank = ifelse(country == 'Scotland', scotland_average_imd_rank, average_imd_rank),
         average_imd_rank = ifelse(country == 'Wales', wales_average_imd_rank, average_imd_rank))

#update hb mappings
unmatched_hb_ccg_in_master = read_csv('imd_lookups/unmatched_ccg_hb.csv') %>% 
  left_join(england_ccg_imd, by = c('new_code' = 'ccg19cd')) %>% 
  left_join(scotland_hb_imd, by = c('new_code' = 'HB')) %>% 
  left_join(wales_imd, by = c('new_code' = 'hlthau')) %>% 
  mutate(miss_average_imd_rank = average_imd_rank) %>% 
  mutate(miss_average_imd_rank = ifelse(!is.na(scotland_average_imd_rank), scotland_average_imd_rank, miss_average_imd_rank),
         miss_average_imd_rank = ifelse(!is.na(wales_average_imd_rank), wales_average_imd_rank, miss_average_imd_rank)) %>% 
  select(code, miss_average_imd_rank)

# #Load master from mapping file
# master = sp::merge(master, england_ccg_imd, by.x = 'code', by.y = 'ccg19cd')
# master = sp::merge(master, scotland_hb_imd, by.x = 'code', by.y = 'HB')
# master = sp::merge(master, ni_imd, by.x = 'code', by.y = 'HSCT')
# master = sp::merge(master, wales_imd, by.x = 'code', by.y = 'hlthau')
# master = sp::merge(master, unmatched_hb_ccg_in_master, by.x = 'code', by.y = 'code')
# 
# #now merge IMDs to 1 col
# master$average_imd_rank = ifelse(master$country == 'Scotland', master$scotland_average_imd_rank, master$average_imd_rank)
# master$average_imd_rank = ifelse(master$country == 'Northern Ireland', master$ni_average_imd_rank, master$average_imd_rank)
# master$average_imd_rank = ifelse(master$country == 'Wales', master$wales_average_imd_rank, master$average_imd_rank)
# master$average_imd_rank = ifelse(!is.na(master$miss_average_imd_rank), master$miss_average_imd_rank, master$average_imd_rank)
# 
# # master$scotland_average_imd_rank = NULL
# # master$ni_average_imd_rank = NULL
# # master$wa_average_imd_rank = NULL

rm(unmatched_hb_ccg_in_master, england_imd_19, wales_imd_19, scotland_imd_20, ni_imd_17,
   scotland_datazones, england_lsoas, ni_soa_to_hsct, ni_sa_to_lgd, wales_hb_names, wales_lsoa_lookup,
   wales_imd, ni_imd, scotland_imd_lookup, scotland_hb_imd, england_ccg_imd_lookup, england_ccg_imd)