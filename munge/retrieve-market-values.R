# Pull Zillow Home Market Values

# libraries ---------------------------------------------------------------

library(tidyverse)
library(xml2)
library(httr)
library(rvest)

source(here::here("libs", "get_zillow_data.R"))
for(i in 52:82){
# current data ------------------------------------------------------------

dat_1 <- readr::read_rds(here::here("data", "forsyth_property_taxes.rds"))

single_family_zones <- c(
  "RS40",
  "RS30",
  "RS20",
  "RS15",
  "RS12",
  "RS9",
  "RS7"
)

dat_2 <- dat_1 %>% 
  filter(!is.na(PROPERTYADDRESS)) %>% 
  filter(!is.na(REID)) %>% 
  mutate(REID = str_trim(REID, "both"),
         PRZONING = str_trim(PRZONING, "both")) %>% 
  filter(PRZONING %in% single_family_zones)

existing_records <- list.files("data", pattern = "addresses.csv", full.names = T)

already_read <- map_dfr(existing_records, read_csv, col_type = cols(REID = col_character())) %>% 
  mutate(street = toupper(street))

# seed setting ------------------------------------------------------------

set.seed(336)


# format data for call ----------------------------------------------------

dat_2 = dat_2 %>% 
  anti_join(already_read, by = c("REID"))

# Format for Zillow API Requirements
make_payload <- sample_n(dat_2, 200) %>% 
  mutate(address = paste(PROPERTYADDRESS),
         city = paste("WINSTON-SALEM, NC")) %>% 
  mutate(address = str_trim(str_replace(address,"[[:space:]]+", " ")),"both") %>% 
  select(REID, address, city)


safe_zillow <- safely(get_zillow_data)


# retrieve data -----------------------------------------------------------


test <- map2(make_payload$address, 
             make_payload$city, 
             safe_zillow, zillow_api_keys[[i]])

# Pull out those with non-errors
a<- transpose(test)[["result"]]
names(a) <- pull(make_payload, REID)
# Remove the null list positions and bind rows
non_error_returns <- plyr::compact(a) %>% 
  bind_rows(.id = "REID")

# Write to disk
stamp <- gsub(":|\\.", "",hms::as_hms(Sys.time()))
file_name <- paste0("data/",Sys.Date(),stamp,"_returned_addresses.csv")

write_csv(non_error_returns, path = file_name)
}

out_csvs = fs::dir_info(here::here("data"), type = "file", glob = "*.csv") %>% 
  filter(size == 0)

map(pull(out_csvs, path), fs::file_delete)
