# Pull Zillow Home Market Values

# libraries ---------------------------------------------------------------

library(tidyverse)


# import data -------------------------------------------------------------
# <http://www.ci.winston-salem.nc.us/portals/0/pdf/Planning/forms-reports/zoning_classif_062405.pdf>
single_family_zones <- c(
  "RS40",
  "RS30",
  "RS20",
  "RS15",
  "RS12",
  "RS9",
  "RS7"
)

combined <- purrr::map_dfr(fs::dir_ls(path = here::here("data"), regexp = ".+raw_tax.+"), readr::read_rds)
dat_1 <- combined %>% 
  distinct()


dat_2 <- dat_1 %>% 
  filter(!is.na(PROPERTYADDRESS)) %>% 
  filter(!is.na(REID)) %>% 
  mutate(REID = str_trim(REID, "both"),
         PRZONING = str_trim(PRZONING, "both")) %>% 
  filter(PRZONING %in% single_family_zones)

existing_records <- list.files("data", pattern = "addresses.csv", full.names = T)

already_read <- map_dfr(existing_records, read_csv, col_type = cols(REID = col_character())) %>% 
  mutate(street = toupper(street))


# compare effective tax rates ---------------------------------------------

combined_housing <- dat_2 %>% 
  select(REID, PROPERTYADDRESS, TOTALVALUE, ) %>% 
  inner_join(already_read, by = "REID") %>% 
  distinct()

overall_lost_tax <- combined_housing %>% 
  mutate(TOTALVALUE = as.numeric(TOTALVALUE)) %>% 
  mutate(effective_tax_rate = 0.013*TOTALVALUE/zestimate) %>% 
  mutate(market_value_diff = zestimate - TOTALVALUE) %>% 
  mutate(lost_tax_revenue = market_value_diff * .013) %>% 
  group_by(name) %>% 
  summarise(avg_effective_tax_rate = mean(effective_tax_rate, na.rm = T),
            avg_diff = mean(market_value_diff, na.rm = T),
            avg_loss = mean(lost_tax_revenue, na.rm = T),
            count = n(),
            total_lost = sum(lost_tax_revenue, na.rm = T),
            avg_zest = mean(zestimate, na.rm = T),
            avg_value = mean(TOTALVALUE, na.rm = T)) %>% 
  arrange(-avg_diff) %>% 
  mutate(rat = avg_zest/avg_value)

overall_lost_tax %>% 
  summarise(sum(total_lost), sum(count))

            