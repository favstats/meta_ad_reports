library(playwrightr)
# library(tidyverse)
options(timeout=300)

source("utils.R")

options(python_init = TRUE)

# cntry_str <- "NL"
time_preset <- commandArgs(trailingOnly = TRUE)
# time_preset <- "last_7_days"

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
  readr,
  piggyback
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




full_cntry_list <- readRDS("cntry_list.rds") %>% 
  rename(iso2c = iso2,
         country = cntry) 


cntries <- full_cntry_list$iso2c

# cntries <- "US"

# retrieve_dats <- function(cntry) {
#   
#   more_data <- readr::read_rds(glue::glue("https://github.com/favstats/meta_reports2/raw/main/lifelong/{cntry}.rds"))  
#   
#   return(more_data)
# }
# 
# #retrieve_dats <- possibly(retrieve_dats, otherwise = NULL)
# 
# #more_data <- cntries %>% 
# #  map_dfr_progress(retrieve_dats)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# old_dat <- dir("daily", full.names = F) %>% 
#   keep(~str_detect(.x, "rds")) %>%
#   str_remove_all("\\.rds") %>%
#   unique()


print("headlesss")
# Create a new page

# page_df <- new_page(browser_df)
page_df <- browser_df %>%
  glimpse


print("sooo")
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

full_repos <- pb_list() %>% as_tibble()

thosearethere <- full_repos %>% 
  arrange(desc(tag)) %>% 
  separate(tag, into = c("country", "timeframe"), remove = F, sep = "-") %>% 
  mutate(day  = str_remove(file_name, "\\.rds|\\.zip")) %>% 
  distinct(country, day, timeframe) %>% 
  filter(str_detect(timeframe, time_preset)) %>% 
  mutate(day = lubridate::ymd(day))

if(nrow(thosearethere)==0){
  thosearethere <- tibble(timeframe = "", day = lubridate::ymd("2020-01-01"))
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

dt %>%
  # arrange(day, country != "RU") %>%
  filter(country %in% cntries) %>%
  arrange(desc(day), country) %>%
  anti_join(thosearethere) %>% 
  # filter(day >= (lubridate::today() - lubridate::days(7))) %>% 
  filter(day >= (lubridate::ymd("2022-10-01"))) %>% 
  # slice(496:500) %>%
  sample_n(100) %>%
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
    } else if (str_detect(download_url, "facebook.com/help/contact/")) {
      cli::cli_alert_danger("Blocked")
      Sys.sleep(10)
      return("Blocked")
    } else {
      # try({
        res <- download_it_now(download_url, file_name)       
      # })
      if(is.null(res$error)) return("Waitlisted")
        
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
dir.create("reports")

tat_path <- thosearethere %>% 
  mutate(path = paste0("report/", country, "/", day, "-", timeframe, ".zip"))

report_paths <- dir(paste0("report"), full.names = T, recursive = T) %>%
  sort(decreasing = T) %>%
  setdiff(tat_path$path) %>% 
  keep(~str_detect(.x, "2024-01-01"))
  # .[200:202]





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

# report_path <- report_paths[1]


for (report_path in report_paths) {
  
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
    print(thedata)
  }
  
  # print("helloo")
  
  thedata %>%
    readr::write_rds(paste0(the_date, ".rds"), compress = "xz")
  
  forsur <- thedata %>% 
    drop_na(date, tf)
  
  the_tag <- paste0(cntry_str, "-", tframe)
  
  # cntry_name
  
  if(!(the_tag %in% pb_releases()$release_name)){
    pb_release_create(repo = "favstats/meta_ad_reports", tag = the_tag, body = paste0(paste0("This release includes ", cntry_name ," '", tframe ,"' Meta ad spending reports.")))
    Sys.sleep(5)
  }
  
  file.copy(report_path, paste0(the_date, ".zip"), overwrite = T)
  
  pb_upload(paste0(the_date, ".rds"), repo = "favstats/meta_ad_reports", tag = the_tag, overwrite = T)
  pb_upload(paste0(the_date, ".zip"), repo = "favstats/meta_ad_reports", tag = the_tag, overwrite = T)
  
  file.remove(paste0(the_date, ".rds"))
  file.remove(paste0(the_date, ".zip"))
  
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



# step1 <- dir("extracted", full.names = T, recursive = F) 
# print(head(step1))
# tobeextracted <- step1 %>% keep(~ str_detect(.x, "advert")) 
# print(head(tobeextracted))
# 
# the_dat <- tobeextracted %>%
#   .[20:22] %>%
#   walk_progress(~ {
#     cntry_str <- str_split(.x, "_") %>% unlist %>% .[3]
#     tframe <- str_extract(.x, "yesterday|last_7_days|last_30_days|last_90_days|lifelong")
#     
#     
#     
#     thedata <- vroom::vroom(.x, show_col_types = F) %>%
#       janitor::clean_names() %>%
#       mutate(date = str_extract(.x, "\\d{4}-\\d{2}-\\d{2}")) %>%
#       mutate_all(as.character) %>%
#       mutate(path = .x) %>%
#       mutate(tf = tframe) %>%
#       mutate(cntry = cntry_str)
#     
#     if (any(c("name_disclaimer_amount") %in% names(thedata))) {
#       print("##within1")
#       print(thedata)
#       thedata <- thedata %>%
#         filter(is.na(name_disclaimer_amount))  %>%
#         janitor::remove_empty()
#       print("##within2")
#       print(thedata)
#     } else {
#       # print("##after1")
#       # print(thedata)
#       thedata <- thedata
#       #   print("##after2")ive = T)
#     }
#     # cntry_str
#     # print(saver)
#     
#     # readr::write_rds(thedata, paste0(saver, tframe, ".rds"))
#     
#     # saver <- paste0("reports/", cntry_str, "/", tframe, "/")
#     # if(!dir.exists(saver)){
#     #   dir.create(saver, recursive = T)
#     # }
#     
#     # try({
#     #   thedata <- thedata %>%
#     #     bind_rows(readr::read_rds(paste0(saver, tframe, ".rds"))) %>%
#     #     distinct()
#     # })
#     print("helloo")
# 
#     thedata %>%
#       readr::write_rds(paste0(cntry_str, ".rds"), compress = "xz")
#     
#     forsur <- thedata %>% 
#       drop_na(date, tf)
#     
#     the_tag <- paste0(forsur$date[1], ".", str_extract(forsur$tf[1], "yesterday|7|30|90|lifelong"))
#     
#     if(!(the_tag %in% pb_releases()$release_name)){
#       pb_release_create(repo = "favstats/meta_ad_reports", tag = the_tag)
#       Sys.sleep(5)
#     }
#     
#     pb_upload(paste0(cntry_str, ".rds"), repo = "favstats/meta_ad_reports", tag = the_tag, overwrite = T)
#     
#     file.remove(paste0(cntry_str, ".rds"))
#     
#     # # 
#     # # return(thedata)
#     # 
#     # lf <- lifelong %>% mutate(date = lubridate::ymd(date)) %>% arrange(date) %>%  distinct(page_id, page_name, disclaimer, amount_spent_usd, number_of_ads_in_library, cntry, .keep_all = T)
# 
#  
#     
#   })
#       # print(thedata)
    # }

# pb_current()


# dir("reports", full.names = T, recursive = T) %>% 
#   keep(~str_detect(.x, "last_30")) %>% 
#   # .[20] %>% 
#   walk_progress(~{
#     
#     cntry_str <- str_split(.x, "/") %>% unlist %>% .[2]
#     # print(.x)
#     # print(cntry_str)
#     # print(paste0("reports/", cntry_str, "/"))
#     file.copy(.x, to = paste0(cntry_str, ".rds"))
#     
#     fin <- readRDS(paste0(cntry_str, ".rds"))
#     
#     the_tag <- paste0(fin$date[1], ".", str_extract(fin$tf[1], "7|30|yesterday|lifelong|90"))
#     
#     pb_release_create(repo = "favstats/meta_ad_reports", tag = the_tag)
#     pb_upload(paste0(cntry_str, ".rds"), repo = "favstats/meta_ad_reports", tag = the_tag, overwrite = T)
#     
#     file.remove(paste0(cntry_str, ".rds"))
#   })


# pb_download()

# readr::write_rds(lifelong %>%  filter(date == min(date)), 
#                  str_replace(saver, paste0(tframe, "/"), paste0(tframe, ".rds")), 
#                  compress = "xz")

# lf <- lifelong %>% mutate(date = lubridate::ymd(date)) %>% arrange(date) %>%  distinct(page_id, page_name, disclaimer, amount_spent_usd, number_of_ads_in_library, cntry, .keep_all = T)
# 
# readr::write_rds(lf %>%  filter(date == min(date)), "test.rds", compress = "xz")


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

