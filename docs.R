library(playwrightr)
# library(tidyverse)

source("utils.R")

options(python_init = TRUE)

# cntry_str <- "NL"
time_preset <- commandArgs(trailingOnly = TRUE)
# time_preset <- "last_30_days"

# install.packages("pacman")
pacman::p_load(
  reticulate,
  vroom,
  progress,
  janitor,
  fs,
  tidyr,
  # appendornot,
  countrycode,
  dplyr,
  stringr,
  lubridate,
  purrr,
  glue,
  rvest,
  cli,
  digest,
  readr
)


# options(googledrive_quiet = TRUE)
# 
# drive_auth(path = Sys.getenv("GOOGLE_APPLICATION_KEY"))

# conda_install(packages = "fcntl", pip = T)
if(Sys.info()[["sysname"]]=="Windows"){
  
  pw_init(use_xvfb = F)
} else{
  
  conda_install(packages = "xvfbwrapper", pip = T)
  
  print("installed xvfbwrapper")
  conda_install(packages = "playwright", pip = T)
  print("installed playwright")
  
  pw_init(use_xvfb = T)
  system("playwright install")
}


browser_df <- browser_launch(
  headless = F,
  browser = "firefox",
  user_agent = NULL,
  user_data_dir = "out"
)

dir.create("lifelong")


print("headlesss")
# Create a new page

# page_df <- new_page(browser_df)
page_df <- browser_df %>%
  glimpse


country_codes <- c("AD", "AL", "AM", "AR", "AT", 
                   "AU", "BA", "BE", "BG", "BR", 
                   "CA", "CH", "CL", "CO", "CY", 
                   "CZ", "DE", "DK", "EC", "EE", 
                   "ES", "FI", "FR", "GB", "GR", 
                   "GT", "HR", "HU", "IE", "IN", 
                   "IS", "IT", "LI", "LT", "LU", 
                   "LV", "MD", "ME", "MK", "MT",
                   "MX", "NL", "NO", "NZ", "PL", 
                   "PT", "RO", "RS", "SE", "SI", 
                   "SK", "SM", "TR", "UA", "US", 
                   "VE", "ZA")


fullcntries <- c("AD", "AE", "AG", "AI", "AL", "AM", "AO", "AR", "AT", "AU", "AZ", "BA",
                 "BB", "BD", "BE", "BF", "BG", "BH", "BI", "BJ", "BM", "BN", "BO", "BR",
                 "BS", "BT", "BW", "BY", "BZ", "CA", "CD", "CF", "CG", "CH", "CI", "CL",
                 "CM", "CO", "CR", "CV", "CY", "CZ", "DE", "DJ", "DK", "DM", "DO", "DZ",
                 "EC", "EE", "EG", "ER", "ES", "ET", "FI", "FJ", "FK", "FM", "FR", "GA",
                 "GB", "GD", "GE", "GG", "GH", "GI", "GM", "GN", "GQ", "GR", "GT", "GW",
                 "GY", "HN", "HR", "HT", "HU", "ID", "IE", "IL", "IM", "IN", "IQ", "IS",
                 "IT", "JE", "JM", "JO", "JP", "KE", "KG", "KH", "KI", "KM", "KN", "KW",
                 "KY", "KZ", "LA", "LB", "LC", "LI", "LK", "LR", "LS", "LT", "LU", "LV",
                 "LY", "MA", "MC", "MD", "ME", "MG", "MH", "MK", "ML", "MM", "MN", "MR",
                 "MS", "MT", "MU", "MV", "MW", "MX", "MY", "MZ", "NA", "NE", "NG", "NI",
                 "NL", "NO", "NP", "NR", "NZ", "OM", "PA", "PE", "PG", "PH", "PK", "PL",
                 "PS", "PT", "PW", "PY", "QA", "RO", "RS", "RW", "SA", "SB", "SC", "SE",
                 "SG", "SH", "SI", "SK", "SL", "SM", "SN", "SO", "SR", "SS", "ST", "SV",
                 "SZ", "TC", "TD", "TG", "TH", "TJ", "TM", "TN", "TO", "TR", "TT", "TV",
                 "TW", "TZ", "UA", "UG", "US", "UY", "UZ", "VC", "VE", "VG", "VI", "VN",
                 "VU", "WF", "WS", "YE", "YT", "ZA", "ZM", "ZW")

cntry_list3 <- fullcntries %>%
  tibble(iso2 = .) %>% 
  mutate(cntry = countrycode::countrycode(iso2, origin = "iso2c", destination = "country.name"))


set_pages <- function(iso) {
  
  try({
    
    # iso <- "ps"
    urls <- paste0("https://github.com/favstats/wtm_", iso, "/settings/pages")
    
    page_df %>%
      goto(urls)
    
    print(urls)
    
    none_dat <- page_df %>% 
      # playwrightr::get_by
      playwrightr::get_by_text("None") 
    
    if(nrow(none_dat)==2){
      return(NULL)
    }
    
    page_df %>% 
      # playwrightr::get_by
      playwrightr::get_by_text("None") %>%
      slice(1) %>% 
      playwrightr::click()
    
    print("None")
    
    page_df %>% 
      # playwrightr::get_by
      playwrightr::get_by_text("main") %>%
      slice(1) %>% 
      playwrightr::click()
    
    print("main")
    
    page_df %>% 
      # playwrightr::get_by
      playwrightr::get_by_text("/ (root)") %>%
      slice(1) %>% 
      playwrightr::click()
    
    print("/ (root)")
    
    
    page_df %>% 
      # playwrightr::get_by
      playwrightr::get_by_text("/docs") %>%
      slice(1) %>% 
      playwrightr::click()
    
    print("/docs")
    
    page_df %>% 
      # playwrightr::get_by
      playwrightr::get_by_text("Save") %>%
      slice(4) %>% 
      playwrightr::click()

    
    print("Save")
  })
 
  
}

structure(list(cntry = c("ae", "ag", "ai", "ao", "az", "ba", 
                         "bb", "bd", "bf", "bh", "bi", "bm", "bn", "bo", "bs", "bt", "bw", 
                         "by", "bz", "ca", "cf", "cg", "cm", "co", "cr", "cv", "cz", "dj", 
                         "do", "dz", "ee", "er", "et", "fk", "fm", "ga", "gd", "ge", "gg", 
                         "gi", "gm", "gn", "gq", "gw", "gy", "hn", "ht", "id", "im", "is", 
                         "je", "jm", "jo", "kg", "kh", "ki", "km", "kn", "kr", "kz", "la", 
                         "lb", "lr", "ls", "lu", "ly", "mc", "mg", "mh", "mk", "mn", "mr", 
                         "ms", "mu", "mv", "mz", "na", "ne", "ng", "np", "nr", "pa", "pe", 
                         "pk", "ps", "pw", "qa", "rw", "sa", "sb", "sc", "sg", "sh", "si", 
                         "sl", "sm", "sn", "so", "sr", "ss", "st", "sv", "tc", "td", "tg", 
                         "th", "tj", "tl", "tm", "tn", "to", "tt", "tw", "tz", "ua", "ug", 
                         "uy", "uz", "vc", "ve", "vg", "vn", "vu", "ws", "xk", "ye", "yt", 
                         "zm", "zw"), res = c("nopage", "nopage", "nopage", "nopage", 
                                              "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", 
                                              "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", 
                                              "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", 
                                              "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", 
                                              "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", 
                                              "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", 
                                              "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", 
                                              "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", 
                                              "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", 
                                              "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", 
                                              "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", 
                                              "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", 
                                              "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", 
                                              "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", 
                                              "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", 
                                              "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", 
                                              "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", "nopage", 
                                              "nopage", "nopage", "nopage", "nopage", "nopage", "nopage")), class = c("tbl_df", 
                                                                                                                      "tbl", "data.frame"), row.names = c(NA, -129L)) %>%
  pull(cntry) %>% 
  map_dfr_progress(set_pages)
