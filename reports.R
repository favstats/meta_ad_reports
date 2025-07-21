unlink("node_modules", recursive = T, force = T)
unlink("out", recursive = T, force = T)

# library(tidyverse)
options(timeout=300)

source("utils.R")

options(python_init = TRUE)

# cntry_str <- "NL"
time_preset <- commandArgs(trailingOnly = TRUE)
# time_preset <- "last_90_days"

if(length(time_preset)==0){
  time_preset <- "last_30_days"
}

if(!("pacman" %in% tibble::as_tibble(installed.packages())$Package)){
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
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
  readr,
  piggyback
)


if(!("playwrightr" %in% tibble::as_tibble(installed.packages())$Package)){
  remotes::install_github("benjaminguinaudeau/playwrightr")
}

library(playwrightr)

# releases <- piggyback::pb_releases()

# debugonce(pb_releases)
# Call the function to perform the operation
full_repos <- read_rds("https://github.com/favstats/meta_ad_reports2/raw/refs/heads/main/data/full_repos.rds")




# options(googledrive_quiet = TRUE)
# 
# drive_auth(path = Sys.getenv("GOOGLE_APPLICATION_KEY"))

# conda_install(packages = "fcntl", pip = T)
if(Sys.info()[["sysname"]]=="Darwin"){
  
  pw_init(use_xvfb = F)
  # time_preset <- "last_90_days"
  
} else{
  # Load reticulate
  library(reticulate)
  
  # Manually set the Python path (works better in GitHub Actions)
  use_python("/usr/share/miniconda/envs/r-reticulate/bin/python", required = TRUE)
  
  # Install xvfbwrapper and playwright inside the correct environment
  py_install("xvfbwrapper")
  print("installed xvfbwrapper")
  
  py_install("playwright")
  print("installed playwright")
  
  # Explicitly check if the module exists before proceeding
  py_run_string("import xvfbwrapper")
  
  # Initialize Playwright with xvfb
  pw_init(use_xvfb = TRUE)
  
  # Install Playwright browsers
  system("playwright install --force")
}


browser_df <- browser_launch(
  headless = F,
  browser = "firefox",
  user_agent = NULL,
  user_data_dir = "out"
)



full_cntry_list <- readRDS("cntry_list.rds") %>% 
  rename(iso2c = iso2,
         country = cntry) 


cntries <- full_cntry_list$iso2c

out <- cntries %>% 
  map(~{
    .x %>% 
      paste0(c("-yesterday", "-last_7_days", "-last_30_days", 
               "-last_90_days", "-lifelong"))
  }) %>% 
  unlist() %>% 
  keep(~str_detect(.x, time_preset)) %>% 
  # .[100:120] %>% 
  map_dfr_progress(~{
    the_assets <- httr::GET(paste0("https://github.com/favstats/meta_ad_reports2/releases/expanded_assets/", .x))
    
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
  })


print("headlesss")
# Create a new page

# page_df <- new_page(browser_df)
page_df <- browser_df %>%
  glimpse


# pw_restart <- function() {
#   reticulate::py_run_string("p.stop()")
#   pw_init(use_xvfb = xxxx)
#   reticulate::py_run_string("p = sync_playwright().start()")
# }


print("sooo22")

on <- function(page_df, event, lambda_string) {
  playwrightr:::py_run(glue('{page_df$page_id}.on("{event}", {lambda_string})'))
  return(page_df)
}
off <- function(page_df, event, lambda_string) {
  playwrightr:::py_run(glue(
    '{page_df$page_id}.remove_listener("{event}", {lambda_string})'
  ))
  return(page_df)
}

print("soooxx")
execute_script <- function (page_df, script) {
  playwrightr:::py_run(glue("d = {{page_df$page_id}}.evaluate('{{script}}')"))
}

page_df %>%
  goto("https://www.facebook.com/ads/library/report")
print("visit website")
Sys.sleep(2)

# page_df %>% screenshot("/data/res/facebook_add_reports/test.png")

try({
  page_df %>%
    get_by_test_id("cookie-policy-manage-dialog-accept-button") %>%
    slice(1) %>%
    click() %>%
    screenshot("/data/res/facebook_add_reports/test.png")
})


# Write post-data string to disk into tmp
tmp_post_data_string <-
  paste0(digest::digest("YOOYOOo"), ".txt")#  tempfile(fileext = ".txt")
# page_df %>% on("request", glue::glue('lambda request: print(request.url)'))
# page_df %>% on("request", glue::glue('lambda request: print(request.post_data) if (request.method == "POST" and "report/v2/download" in request.url) else None'))
# page_df %>% on("request", glue::glue('lambda request: print(request.post_data) if (request.method == "POST" and "graphql" in request.url) else None'))
page_df %>% on(
  "request",
  glue::glue(
    'lambda request: open("{tmp_post_data_string}", "w").write(request.post_data) if (request.method == "POST" and "report/v2/download" in request.url) else None'
  )
)
page_df %>% on(
  "request",
  glue::glue(
    'lambda request: open("{tmp_post_data_string}", "w").write(request.post_data) if (request.method == "POST" and "graphql" in request.url) else None'
  )
)
print("some other stuff")
# Print Console
# tmp_download_link <- tempfile()
tmp_download_link <-
  paste0(digest::digest("sdff"), ".txt")#  tempfile(fileext = ".txt")

page_df %>% on("console",
               "lambda msg: open('{tmp_download_link}', 'w').write(msg.text)")

# First click to obtain the request post-body-data
page_df %>%
  get_by_text("Download report") %>%
  slice(2) %>%
  click()

# Print download path
tmp_download_path <-
  paste0(digest::digest("sdsdfsdfdff"), ".txt")#
page_df %>% on(
  "download",
  glue::glue(
    'lambda download: open("{tmp_download_path}", "w").write(download.path())'
  )
)
print("some other stuff 2")

data_string <- readLines(tmp_post_data_string, warn = F) %>%
  str_squish() %>%
  glimpse

# full_cntry_list$iso2c
# countries <- tibble::tibble(country = c("NL", "DE", "CA", "FR", "US"))
countries <-
  tibble::tibble(country = countrycode::codelist$iso2c) %>%
  filter(!is.na(country)) %>%
  glimpse
# countries <- fs::file_info(dir("/data", recursive = T, full.names = T)) %>%
#   filter(size > 1) %>%
#   pull(path) %>%
#   fs::path_dir() %>%
#   fs::path_file() %>%
#   unique
# readr::write_rds(countries, "data/countries.rds")
#
# countries <- tibble::tibble(country = readr::read_rds("data/countries.rds")) %>%
#   filter(!is.na(country)) %>%
#   glimpse

daysies <-
  tibble::tibble(day = lubridate::as_date(seq.int(
    lubridate::dmy("01-07-2019"), lubridate::today(), by = 1
  ))) %>%
  # days <- tibble::tibble(day = lubridate::as_date(seq.int(lubridate::dmy("15-07-2023"), lubridate::today(), by = 1))) #%>%
  head(-2)
print("afterdaises")


dt <- expand_grid(countries, daysies) %>%
  glimpse


# try({
#   all_reports_old <- readRDS(paste0("logs/all_reports_", time_preset, ".rds"))
# })
# 
# if(!exists("all_reports_old")){
#   all_reports_old <- c()
# }

# dir("report/ES", full.names = T, recursive = T) %>% sort
dir.create("extracted")
dir.create("report")
print("creation")

library(tidyverse)
# readRDS("reports/US/last_30_days.rds") %>% count(date)

# thosearethere <- dir("reports") %>% 
#   map_dfr_progress(~{
#     
#     if(file.exists(paste0("reports/", .x,"/", time_preset, ".rds"))){
#       return(read_rds(paste0("reports/", .x,"/", time_preset, ".rds")))
#     }
#     
#     }) 
# 
# if(nrow(thosearethere)!=0){
#   thosearethere <- thosearethere %>% 
#     distinct(cntry, date) %>% 
#     rename(day = date) %>% 
#     mutate(day = lubridate::ymd(day))
# } else {
#   thosearethere <- thosearethere %>% mutate(cntry = NA, day = lubridate::ymd("2020-01-01"))
# }



thosearethere <- out %>% 
  rename(tag = release,
         file_name = filename) %>% 
  arrange(desc(tag)) %>% 
  separate(tag, into = c("country", "timeframe"), remove = F, sep = "-") %>% 
  mutate(day  = str_remove(file_name, "\\.rds|\\.zip")) %>% 
  distinct(country, day, timeframe) %>% 
  # filter(str_detect(timeframe, time_preset)) %>% 
  mutate(day = lubridate::ymd(day))

if(nrow(thosearethere)==0){
  thosearethere <- tibble(timeframe = "", day = lubridate::ymd("2020-01-01"), country = "")
} else {
  # print(glimpse(thosearethere))
}
# thosearethere %>% write_rds("test.rds", compress = "xz")
# thosearethere %>% saveRDS("test2.rds")

download_it <- function(download_url, file_name) {
  download.file(download_url,
                file_name,
                quiet = T,
                mode = "wb")   
}

download_it_now <- safely(download_it, quiet = F)

if(file.exists("blacklist.csv")) {
  blacklist <- read_csv("blacklist.csv") %>% mutate(day = lubridate::ymd(day)) %>% filter(day <= lubridate::ymd("2023-12-31"))
} else {
  blacklist <- tibble(country = "", day = lubridate::ymd("2020-01-01"))
}

rawlings <- dt %>%
  # arrange(day, country != "RU") %>%
  filter(country %in% cntries) %>%
  arrange(desc(day), country) %>%
  anti_join(thosearethere) %>%
  anti_join(blacklist)

thoseneedtobehere <- rawlings %>%
  # filter(day >= (lubridate::today() - lubridate::days(7))) %>% 
  filter(day >= (lubridate::ymd("2024-01-01"))) 

nicetohave <- rawlings %>% 
  # filter(day >= (lubridate::today() - lubridate::days(7))) %>% 
  filter(day >= (lubridate::ymd("2022-01-01"))) %>%
  arrange(desc(day), country) %>% 
  sample_n(1000)



thoseneedtobehere %>%
bind_rows(nicetohave) %>%
  filter(day > (lubridate::ymd("2025-07-05"))) %>%
  # filter(country == "BA") %>% 
  # filter(day>=lubridate::ymd("2024-01-01")) %>% 
# tibble(country = "BA", 
#        day = seq.Date(from = lubridate::ymd("2024-01-07"), 
#                              to = lubridate::ymd("2024-01-07"), by = "1 day")) %>%
  # filter(day <= (lubridate::ymd("2024-01-01"))) %>% 
  # slice(1:5000) %>%
  # slice(1:2) %>%
  sample_n(10, replace = T) %>%
  distinct(country, day, .keep_all = T) %>% 
  split(1:nrow(.)) %>% #bashR::simule_map(1)
  walk_progress( ~ {
    
    
    # browser()
    file_name <-
      glue::glue("report/{.x$country}/{as.character(.x$day)}-{time_preset}.zip")
    # if (file_name %in% all_reports_old)
    #   return()
    
    cli::cli_alert_info(glue::glue("{.x$country} - {.x$day}"))
    
    path_dir <- fs::path_dir(file_name)
    if (!fs::dir_exists(path_dir))
      fs::dir_create(path_dir)
    
    # print(time_preset)
    
    if(length(time_preset)==0){
      
      print("ATTENTION FOR SOME REASON NO TIMEPRESET")
      
      # time_preset <- "last_7_days"
      time_preset <- "last_30_days"
      # time_preset <- "yesterday"
      # time_preset <- "lifelong"
      
    }
    
    
    js_code <-
      paste0(
        'fetch("https://www.facebook.com/api/graphql/',
        '", {"headers": {"accept": "*/*", "content-type": "application/x-www-form-urlencoded"}, "body": "',
        paste0(data_string, "&variables=%7B%22country%22%3A%22", .x$country ,"%22%2C%22reportDS%22%3A%22", as.character(.x$day) ,"%22%2C%22timePreset%22%3A%22", time_preset,"%22%7D"),
        '", "method": "POST", "mode": "cors", "credentials": "include" }).then(resp => resp.text()).then(data => console.log(data));'
      )
    
    
    
    page_df %>% execute_script(js_code)
    Sys.sleep(.1)
    
    download_url <- readLines(tmp_download_link, warn = F) %>%
      str_extract("\"https.*?\"") %>%
      str_remove_all("(^\")|(\"$)") %>%
      str_remove_all("\\\\") %>%
      glimpse
    if (is.na(download_url)) {
      if (!(.x$day %in% lubridate::as_date((lubridate::today() - lubridate::days(10)):lubridate::today()
      ))) {
        write(list(), file_name)
      }       
      
      # print("ho")
      # print(.x)
      # debugonce(save_csv)
      save_csv(.x, path = "blacklist.csv")
      # print("ho")
      
    } else if (str_detect(download_url, "facebook.com/help/contact/")) {
      cli::cli_alert_danger("Blocked")
      Sys.sleep(10)
      return("Blocked")
    } else {
      # try({
      res <- download_it_now(download_url, file_name)       
      # })
      if(!is.null(res$error)) return("Waitlisted")
      
    }
    
    
    
    Sys.sleep(runif(1, 0, .3))
  })





# latest_available_date <- dir("extracted") %>% 
#   keep(~str_detect(.x, cntry_str)) %>% 
#   sort(decreasing = T) %>% 
#   str_split("_") %>% unlist %>% .[2]
# 
# print("whats the latest available date")
# 
# 
# if(length(latest_available_date)==0){
#   print("its actually zero why")
#   
#   latest_available_date <- as.character(lubridate::today()-lubridate::days(4))
# }


print("NL DOWNLOADED")
# dir.create("reports")

tat_path <- thosearethere %>% 
  mutate(path = paste0("report/", country, "/", day, "-", timeframe, ".zip")) %>% 
  drop_na(day)

report_paths <- dir(paste0("report"), full.names = T, recursive = T) %>%
  sort(decreasing = T) %>%
  setdiff(tat_path$path) %>%
  sort()
# keep(~str_detect(.x, "2024-01-01")) %>% 
# keep(~str_detect(.x, "last_7_days"))
# .[200:202]

latest_dat <- tat_path %>%
  group_by(country) %>% 
  arrange(desc(day)) %>% 
  slice(1) %>% 
  ungroup()

# session("https://github.com/favstats/meta_ad_reports/releases/tag/ZW-lifelong") %>% 
#   html_elements(".mb-3") %>% 
#   html_text() %>% str_squish()
#   # html_children() %>% 
#   str_detect("2024-01-01")
# 
#   https://github.com/favstats/meta_ad_reports/releases/download/ZW-lifelong/2024-01-01.rds

progress_bar <- function(current, total, bar_width = 50) {
  # Calculate the percentage completed
  percent_done <- round(current / total * 100)
  
  # Number of filled positions in the bar
  filled_positions <- round(bar_width * percent_done / 100)
  
  # Create the progress bar string
  bar <- paste0("[", 
                strrep("=", filled_positions), 
                ">", 
                strrep(" ", bar_width - filled_positions), 
                "] ", 
                percent_done, "%")
  
  # Print the progress bar and use carriage return to stay on the same line
  cat("\r", bar, sep = "")
  flush.console()
}


# full_repos$tag %>% unique %>% 
#   walk(~{pb_release_delete(tag= .x)})

# report_path <- report_paths[3]
# report_path <- report_paths[str_detect(report_paths, "OM")][1]

# releases <- pb_releases()
release_names <- full_repos$tag %>% unique

# report_path <- report_paths[1]

for (report_path in report_paths) {
  # print(report_path)
  progress_bar(which(report_path==report_paths, report_paths), total = length(report_paths))
  
  unzip(report_path, exdir = "extracted")
  
  rawww <-  str_split(report_path, "/") %>% unlist 
  
  cntry_str <- rawww[2]
  
  tframe <- str_remove(str_split(rawww, "-") %>% unlist() %>% .[length(.)], ".zip")
  the_date <- str_remove_all(rawww[3], paste0(".zip|-", tframe))   
  
  cntry_name <- full_cntry_list %>% 
    filter(iso2c == cntry_str) %>% 
    pull(country)
  
  extracted_path <- dir("extracted", full.names = T, recursive = F) %>% 
    keep(~ str_detect(.x, "advert")) %>%
    keep(~ str_detect(.x, cntry_str) & str_detect(.x, as.character(lubridate::ymd(the_date)-1)) & str_detect(.x, tframe)) 
  
  if(length(extracted_path)==0){
    print("no data")
    next
  }
  
  # tframe <- str_extract(extracted_path, "yesterday|last_7_days|last_30_days|last_90_days|lifelong")
  
  thedata <- vroom::vroom(extracted_path, show_col_types = F) %>%
    janitor::clean_names() %>%
    mutate(date = str_extract(extracted_path, "\\d{4}-\\d{2}-\\d{2}")) %>%
    mutate_all(as.character) %>%
    mutate(path = extracted_path) %>%
    mutate(tf = tframe) %>%
    mutate(cntry = cntry_str)
  
  if (any(c("name_disclaimer_amount") %in% names(thedata))) {
    print("##within1")
    print(thedata)
    thedata <- thedata %>%
      filter(is.na(name_disclaimer_amount))  %>%
      janitor::remove_empty()
    print("##within2")
    # print(thedata)
  }
  
  # print("helloo")
  
  if(nrow(thedata)==0){
    print("no data for some reason")
    next
  }
  
  thedata %>%
    readr::write_rds(paste0(the_date, ".rds"), compress = "xz")
  
  the_tag <- paste0(cntry_str, "-", tframe)
  
  # cntry_name
  
  if(!(the_tag %in% release_names)){
    pb_release_create_fr(repo = "favstats/meta_ad_reports2", 
                         tag = the_tag,
                         body = paste0("This release includes ", cntry_name ," '", tframe ,"' Meta ad spending reports."), 
                         releases = full_repos)    # Sys.sleep(5)
  }
  
  file.copy(report_path, paste0(the_date, ".zip"), overwrite = T)
  
  try({
    # print(paste0(the_date, ".rds"))
    # print(the_tag)
    # debugonce(pb_upload_file_fr)
    # debugonce(pb_upload_file_fr)
    pb_upload_file_fr(paste0(the_date, ".rds"), repo = "favstats/meta_ad_reports2", tag = the_tag, releases = full_repos)
    pb_upload_file_fr(paste0(the_date, ".zip"), repo = "favstats/meta_ad_reports2", tag = the_tag, releases = full_repos)
    
    lat_dat <- latest_dat %>% 
      filter(country == cntry_str)
    
    if(nrow(lat_dat) == 0){
      check_it <- T
    } else {
      check_it <- (lubridate::ymd(the_date) >= lat_dat$day) 
    }
    
    # if(length(check_it)!=0){
    #   if(check_it){
    #     file.rename(paste0(the_date, ".rds"), "latest.rds")
    #     # debugonce(pb_upload_file_fr)
    #     pb_upload_file_fr("latest.rds", repo = "favstats/meta_ad_reports", tag = the_tag, releases = full_repos)     
    #     
    #     file.remove("latest.rds")
    #   }
    # }
    
    
    
    
    file.remove(paste0(the_date, ".rds"))
    file.remove(paste0(the_date, ".zip"))   
  })
  
  
  
  gc()
  
}
# # .[1:7] %>% 
# walk_progress( ~ {
# 
#   
# })

# unzip("report/TN/2023-11-28.zip", exdir = "extracted", overwrite = T)

# unzip(dir(paste0("report/",cntry_str), full.names = T, recursive = T), exdir = "extracted")

print("NL UNZIPPED")



unlink("node_modules", recursive = T, force = T)
unlink("out", recursive = T, force = T)

print("################6")

dir() %>%
  keep( ~ str_detect(.x, ".txt")) %>%
  discard( ~ str_detect(.x, "n_advertisers.txt")) %>%
  walk(file.remove)

# all_reports_old <- readRDS("logs/all_reports.rds")

print("################9")

# all_reports <- dir("report", full.names = T, recursive = T)

print("################10")

# all_reports <- all_reports_old %>% 
#   c(all_reports) %>% 
#   unique()
# print("################11")
# 
# saveRDS(all_reports, file = paste0("logs/all_reports_", time_preset, ".rds"))

print("################12")

unlink("report", recursive = T, force = T)
unlink("extracted", recursive = T, force = T)
