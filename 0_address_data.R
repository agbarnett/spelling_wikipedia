# 0_address_data.R  
# get the address information which is used later in find authors' countries
# get country codes, emails, and US states. Used for affiliation data; copied from my baseline_tables repository
# February 2025
library(dplyr)
library(countrycode) # for list of countries for affiliation
library(rvest) # for reading html
library(stringr)

# key URLs
url_country_codes = "https://en.wikipedia.org/wiki/Country_code_top-level_domain"
url_capitals = 'https://en.wikipedia.org/wiki/List_of_national_capitals'
url_states = 'https://en.wikipedia.org/wiki/List_of_U.S._state_and_territory_abbreviations'

# 1) get list of all countries
countries = c(codelist$country.name.en, 'UK', 'USA', 'Korea','Republic of Korea') # add a few common abbreviations and alternatives

# 2) get email extensions from Wikipedia
page = read_html(url_country_codes)
table = page %>% 
  html_elements("table") %>% # changed from xml_nodes
  .[2] %>% # 
  html_table(header = TRUE, fill = TRUE) 
emails = table[[1]] %>%
  select("Name[3]","Entity") %>%
  rename('email' = 'Name[3]',
         'country' = 'Entity') %>%
  mutate(email = paste('\\', email, sep=''), # to make '.'
         country = str_remove_all(country, pattern = '\\(.*?\\)' ), # remove everything in brackets
         country = str_squish(country)) 

# 3) get US states 
page = read_html(url_states)
table = page %>% 
  html_elements("table") %>%
  .[2] %>% # second table 
  html_table(header = FALSE, fill = TRUE) # ignore header
states = table[[1]] %>%
  filter(X2=='State') %>%
  select(X1, X7) %>%
  rename('state' = 'X1',
         'postal' = 'X7')

# 4) get capital cities worldwide
page = read_html(url_capitals)
table = page %>% 
  html_elements("table") %>%
  .[2] %>% # 
  html_table(header = TRUE, fill = TRUE) #
capitals = table[[1]] %>%
  janitor::clean_names() %>%
  rename('country' = 'country_territory') %>%
  select(-notes)

# save as a list
address_info = list()
address_info$countries = countries
address_info$emails = emails
address_info$states = states
address_info$capitals = capitals
save(address_info, file = 'data/0_address.RData')

