# Using pacman to manage packages
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(readr, glue, lubridate, janitor, purrr, dplyr, stringr, tidyr)


walk_progress <- function(.x, .f, ...) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(
    total = length(.x), 
    format = " (:spin) [:bar] :percent | :current / :total | eta: :eta",
    # format = " downloading [:bar] :percent eta: :eta",
    force = TRUE)
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::walk(.x, f, ...)
}

map_progress <- function(.x, .f, ...) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(
    total = length(.x), 
    format = " (:spin) [:bar] :percent | :current / :total | eta: :eta",
    # format = " downloading [:bar] :percent eta: :eta",
    force = TRUE)
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::map(.x, f, ...)
}

map_dfr_progress <- function(.x, .f, ...) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(
    total = length(.x), 
    format = " (:spin) [:bar] :percent | :current / :total | eta: :eta",
    # format = " downloading [:bar] :percent eta: :eta",
    force = TRUE)
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::map_dfr(.x, f, ...)
}


source("utils.R")

tstamp <- Sys.time()


cntries <- c("AD", "AE", "AG", "AI", "AL", "AM", "AO", "AR", "AT", "AU", "AZ", "BA", 
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
"VU", "WF", "WS", "YE", "YT", "ZA", "ZM", "ZW") %>% unique



retrieve_dats <- function(cntry) {
  
  more_data <- readr::read_rds(glue::glue("https://github.com/r-user-group-stuttgart/meta_reports730/raw/main/last_7_days/{cntry}.rds"))  %>%
    mutate(date_produced = lubridate::ymd(date)) %>%
    drop_na(date_produced) %>% 
    janitor::clean_names() %>% 
    mutate(page_id = as.character(page_id)) 
  
  
  # more_data
  more_data <- names(more_data) %>% 
    str_replace("amount_spent_.*", "spend") %>% 
    set_names(more_data, .) %>% 
    mutate(spend = readr::parse_number(spend))
  
  return(more_data)
}

retrieve_dats <- possibly(retrieve_dats, otherwise = NULL)

more_data <- cntries %>% 
  map_dfr_progress(retrieve_dats)

old_dat <- dir("targeting/7", full.names = T, recursive = T) %>% 
  map_dfr_progress(readRDS)


scraper <- function(.x, time = tf) {
  
  # print(paste0(.x$page_name,": ", round(which(internal_page_ids$page_id == .x$page_id)/nrow(internal_page_ids)*100, 2)))
  
  fin <- get_targeting(.x$page_id, timeframe = glue::glue("LAST_{time}_DAYS")) %>%
    mutate(tstamp = tstamp)
  
  if(nrow(fin)>1){
    dsss <- as.character(fin$ds[1])
    the_folder <<- glue::glue("targeting/{time}/{dsss}")
    if(!dir.exists(the_folder)) {
      #
      print(the_folder)
      dir.create(the_folder, recursive = T)
    }
    
    # path <- paste0(the_folder,.x$page_id, ".rds")
    # if(file.exists(path)){
    #   ol <- read_rds(path)
    #
    #   saveRDS(fin %>% bind_rows(ol), file = path)
    # } else {
    
    # saveRDS(fin, file = path)
    # }
  } else if (nrow(fin)==1){
    break
  }else {
    fin <- tibble(internal_id = .x$page_id, no_data = T) %>%
      mutate(tstamp = tstamp)
  }
  
  # print(nrow(fin))
  # })
  return(fin)
  
}

scraper <- possibly(scraper, otherwise = NULL, quiet = F)

  enddat <- more_data %>% 
    filter(date_produced %in% c(max(date_produced), max(date_produced)-1)) %>% 
    arrange(desc(spend)) %>%
    distinct(page_id, .keep_all = T) %>% 
    # slice(1:3) %>%
    anti_join(old_dat %>% select(ds, cntry, page_id = internal_id)) %>% 
    # arrange(page_id) %>%
    # slice(1:150) %>% 
    # filter(!(page_id %in% latest_elex$page_id)) %>% 
    split(1:nrow(.)) 
  
    # map_dfr_progress(scraper, tf = "7") 
  
  # Initialize progress bar
  total <- length(enddat)
  # pb <- txtProgressBar(min = 0, max = total, style = 3)
  
  # Initialize dataframe
  df <- tibble()
  
  options(scipen = 999)
  
  counter <- 1
  for (jj in enddat) {
    df <- bind_rows(df, scraper(jj, time = "7")) %>% 
      mutate(cntry = jj$cntry[1],
             date_produced = jj$date_produced[1])
    
    # Update progress bar
    # setTxtProgressBar(pb, counter)
    if(counter %% 10 == 0){
      message(paste0(counter , "/", total, ": ", round(counter/total, 3)*100))
    }   
    
    counter <- counter + 1
  }
  
  # Close progress bar
  # close(pb)
  
  # saveRDS()

df %>% 
  group_by(cntry) %>% 
  group_split() %>% 
  walk(~{
    
    try({
      oldd <- readRDS(glue::glue("{the_folder}/{.x$cntry[1]}.rds"))
    })
    
    if(!exists("oldd")) oldd <- tibble()
    
    fin <- .x %>% bind_rows(oldd) %>% distinct()
    
    saveRDS(fin, glue::glue("{the_folder}/{.x$cntry[1]}.rds"))
  })
  
  

# saveRDS(enddat, "data/targeting_dat.rds")
  
  
  
  
  
  
