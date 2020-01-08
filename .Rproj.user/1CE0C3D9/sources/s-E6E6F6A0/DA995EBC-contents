# Purpose: Import Building Permit Data and Making Tidy

library(tidyverse)
library(readxl)


# import sections ---------------------------------------------------------

#Total 
raw_1 <- read_excel(here::here("data-raw", "CBSA_COMBINEDANNUAL_2018.xlsx"), range = "A4:S383") %>% 
  mutate(type = "Total") %>% 
  janitor::clean_names()

raw_1 <- raw_1 %>% 
  fill(hud_region)

cbsa_info <- select(raw_1, cbsa, cbsa_name, hud_region)

# Single Family
raw_sf <- read_excel(here::here("data-raw", "CBSA_COMBINEDANNUAL_2018.xlsx"), range = "U4:AJ383")%>% 
  mutate_all(str_replace, ",", "") %>% 
  mutate_all(as.numeric) %>% 
  mutate(type = "Single Family") %>% 
  bind_cols(cbsa_info)

# Multi Family
raw_mf <- read_excel(here::here("data-raw", "CBSA_COMBINEDANNUAL_2018.xlsx"), range = "AL4:BA383")%>% 
  mutate_all(str_replace, ",", "") %>% 
  mutate_all(as.numeric) %>% 
  mutate(type = "Multi-Family")%>% 
  bind_cols(cbsa_info)

# 5+ Family
raw_5p <- read_excel(here::here("data-raw", "CBSA_COMBINEDANNUAL_2018.xlsx"), range = "BC4:BR383") %>% 
  mutate_all(str_replace, ",", "") %>% 
  mutate_all(as.numeric) %>% 
  mutate(type = "5+")%>% 
  bind_cols(cbsa_info)

pivot_permits <- function(df){
  df %>% 
    pivot_longer(-c(type, cbsa, cbsa_name, hud_region), names_to = "year", values_to = "n_permits")
}

combined_out <- map_dfr(list(raw_sf, raw_mf, raw_5p), pivot_permits)

combined_out %>% 
  filter(grepl(pattern = "Winston", x = cbsa_name)) %>% 
  ggplot(aes(year, n_permits, group = type, color = type))+
  geom_line()


# write_outputs -----------------------------------------------------------

write_rds(combined_out, here::here("data", "building_permit_data.rds"))