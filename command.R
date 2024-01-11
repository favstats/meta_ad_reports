library(tidyverse)





full_cntry_list <- readRDS("cntry_list.rds") %>% 
  rename(iso2c = iso2,
         country = cntry) 

out <- full_cntry_list$iso2c %>% 
  map(~{
    .x %>% 
      paste0(c("-yesterday", "-last_7_days", "-last_30_days", 
               "-last_90_days", "-lifelong"))
  }) %>% 
  unlist() %>% 
  .[str_detect(., "lifelong")] %>% 
  # .[100:120] %>% 
  map_dfr_progress(~{
    the_assets <- httr::GET(paste0("https://github.com/favstats/meta_ad_reports/releases/expanded_assets/", .x))
    
    the_assets %>% httr::content() %>% 
      html_elements(".Box-row") %>% 
      html_text()  %>%
      tibble(raw = .)   %>%
      # Split the raw column into separate lines
      mutate(raw = strsplit(as.character(raw), "\n")) %>%
      # Extract the relevant lines for filename, file size, and timestamp
      transmute(
        filename = sapply(raw, function(x) trimws(x[3])),
        file_size = sapply(raw, function(x) trimws(x[6])),
        timestamp = sapply(raw, function(x) trimws(x[7]))
      ) %>% 
      filter(filename != "Source code") %>% 
      mutate(release = .x) %>% 
      mutate_all(as.character)
  }) %>% 
  rename(tag = release,
         file_name = filename) %>% 
  arrange(desc(tag)) %>% 
  separate(tag, into = c("country", "timeframe"), remove = F, sep = "-") %>% 
  filter(str_detect(file_name, "rds")) %>% 
  mutate(day  = str_remove(file_name, "\\.rds|\\.zip") %>% lubridate::ymd()) %>% 
  arrange(desc(day)) %>% 
  group_by(country) %>% 
  slice(1) %>% 
  ungroup()


get_reports <- function(cntry, tframe = "lifelong", date = "latest") {
  
  thetemp <- tempfile()
  
  cntry %>% 
    paste0("-", tframe) %>% 
    paste0("https://github.com/favstats/meta_ad_reports/releases/download/", .,"/", date,".rds") %>% 
    .[1] %>% 
    download.file(destfile = thetemp, quiet = T)
  
  thedat <- read_rds(thetemp)
  
  file.remove(thetemp)
  
  thedat
  
}

get_reports2 <- possibly(get_reports, otherwise = NULL, quiet = T)

all_reports <- full_cntry_list$iso2c %>% 
  map_dfr_progress(~get_reports2(.x, date = out %>% filter(country == .x) %>% pull(day)))

# get_reports("DE")

df_final <- all_reports %>%
  # sample_n(100) %>% 
       pivot_longer(cols = starts_with("amount_spent"), names_to = "currency", values_to = "amount_spent") %>%
       mutate(currency = str_to_upper(str_replace(currency, "amount_spent_", ""))) %>%
       filter(!is.na(amount_spent)) %>%
      select(-currency, amount_spent, everything());beepr::beep()

saveRDS(df_final, "df_final.rds")

df_final <- readRDS("df_final.rds")

library(priceR)
# Retrieve AUD to EUR exchange rates
ae <- historical_exchange_rates("AUD", to = "EUR",
                                start_date = "2013-01-01", end_date = "2023-06-30")


spend_dict <- df_final %>% 
  distinct(date, currency) %>%
  filter(currency != "USD") %>% 
  split(1:nrow(.)) %>% 
  map_dfr_progress(~{
    historical_exchange_rates(.x$currency, to = "USD",
                              start_date = .x$date, end_date = .x$date) %>% 
      set_names(c("date", "rate")) %>% 
      mutate(currency = .x$currency) 
  })

saveRDS(spend_dict, "data/spend_dict.rds")

spend_dict %>% View()

final_spending <- df_final %>% 
  left_join(spend_dict) %>%
  mutate(rate = ifelse(currency == "USD", 1, rate)) %>% 
  mutate(amount_spent_usd = parse_number(amount_spent)*rate) %>% 
  arrange(desc(amount_spent_usd)) %>% 
  group_by(page_id, cntry) %>% 
  arrange(disclaimer) %>% 
  mutate(amount_spent_usd = sum(amount_spent_usd)) %>% 
  ungroup() %>% 
  distinct(page_id, cntry, .keep_all = T) 


chinese <- final_spending %>% 
  filter(str_detect(page_name, "Xinhua|CGTN|People's Daily|Global Times|China Daily")|str_detect(disclaimer, "Xinhua|CGTN|People's Daily|Global Times|China Daily")) %>% 
  group_by(cntry) %>% 
  summarize(amount_spent_usd = sum(amount_spent_usd)) %>% 
  arrange(desc(amount_spent_usd)) 

chinese


library(rnaturalearth)



options(scipen = 999)

world <- ne_countries(scale = "small", returnclass = "sf")

# region_bbound <- geojsonsf::geojson_sf("https://services6.arcgis.com/uWtJiVzcBsV6C7NV/arcgis/rest/services/M49_regions_final/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=geojson")

ggworld <- world %>% 
  filter(!(name_en %in% c("Antarctica", "Greenland"))) %>% 
  # count(region, sort = T) 
  mutate(cntry = countrycode::countrycode(name_en, "country.name", "iso2c")) %>% 
  left_join(  chinese %>% distinct(cntry,.keep_all = T))
# filter(target == "custom_audience")) 

# saveRDS(ggworld, file = "overleaf/n_targeting_per_cntry.rds")

the_world <- ggworld %>% 
  # filter(cntry != "US") %>%
  ggplot() + 
  geom_sf(aes(fill = amount_spent_usd), color = "white") +
  theme_void() +
  theme(legend.position = "top") +
  labs(fill = "Spending by Chinese State Media:")  +
  colorspace::scale_fill_binned_sequential(palette = "Viridis", na.value = "lightgrey", 
                                           # breaks = my_breaks, labels = my_breaks2, 
                                           # breaks = c(0,1,2,3,seq(4,20,4)),
                                           # breaks = seq(0,1000,20),
                                           breaks = c(2500,10000,30000),
                                           # breaks = c(0,1,2,3),
                                           guide="legend") +
  # geom_sf(data = region_bbound, color = "white", fill =NA, size = 10) +
  # ggthemes::scale_fill_colorblind(na.value = "lightgrey") + # or direction=1
  # scale_fill_manual(values = c("lightgrey", "blue")) + 
  guides(fill=guide_colourbar(nrow=1, byrow=TRUE, barwidth = 10.5, barheight = 0.4, title.vjust=1.2))
# coord_sf()
the_world










# total spending:
  
  
  ggworld <- world %>% 
  filter(!(name_en %in% c("Antarctica", "Greenland"))) %>% 
  # count(region, sort = T) 
  mutate(cntry = countrycode::countrycode(name_en, "country.name", "iso2c")) %>% 
  left_join(  final_spending %>% 
                # filter(str_detect(page_name, "Xinhua|CGTN|People's Daily|Global Times|China Daily")|str_detect(disclaimer, "Xinhua|CGTN|People's Daily|Global Times|China Daily")) %>% 
                group_by(cntry) %>% 
                summarize(amount_spent_usd = sum(amount_spent_usd)) %>% 
                arrange(desc(amount_spent_usd))   %>% distinct(cntry,.keep_all = T))
# filter(target == "custom_audience")) 

# saveRDS(ggworld, file = "overleaf/n_targeting_per_cntry.rds")
library(tidyverse)
  # my_breaks <- plyr::round_any(exp(seq(log(0.1), log(4000), length = 7)), 10)
the_world <- ggworld %>% 
  # filter(cntry != "US") %>%
  ggplot() + 
  geom_sf(aes(fill = amount_spent_usd/1000000), color = "white") +
  theme_void() +
  theme(legend.position = "top") +
  labs(fill = "$ Spending on Meta Ads about Social Issues, Elections or Politics (log scale):")  +
  scale_fill_viridis_c(na.value = "lightgrey", trans = "log", direction = -1,
                       breaks = c(0.001,0.01, 0.1, 1, 10, 100, 1000, 5000),
                       labels = c("1k", "10k", "100k", "1m", "10m", "100m", "1b", "5b")) +
  # colorspace::scale_fill_binned_sequential(palette = "Viridis", na.value = "lightgrey", 
  #                                          # breaks = my_breaks, labels = my_breaks2, 
  #                                          # breaks = c(0,1,2,3,seq(4,20,4)),
  #                                          # breaks = seq(0,1000,20),
  #                                          breaks = c(1,2,5, 300),
  #                                          # breaks = c(0,1,2,3),
  #                                          guide="legend") +
  # geom_sf(data = region_bbound, color = "white", fill =NA, size = 10) +
  # ggthemes::scale_fill_colorblind(na.value = "lightgrey") + # or direction=1
  # scale_fill_manual(values = c("lightgrey", "blue")) + 
  guides(fill=guide_colourbar(nrow=1, byrow=TRUE, barwidth = 11.5, barheight = 0.4, title.vjust=1.2)) +
  labs(caption = "Total Spending between 2019 and 2024. Source: Meta Ad Library Report.") +
  theme(plot.caption = element_text(hjust = 0.5))
# coord_sf()
the_world

ggsave("img/totalspend.png", dpi = 900, bg = "white", width = 8, height = 4)


