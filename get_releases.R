unlink("node_modules", recursive = T, force = T)
unlink("out", recursive = T, force = T)

# library(playwrightr)
# library(tidyverse)
options(timeout=300)

source("utils.R")


# cntry_str <- "NL"
# time_preset <- commandArgs(trailingOnly = TRUE)
# time_preset <- "lifelong"

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











out2 <- cntries %>% 
  map(~{
    .x %>% 
    paste0(c("-yesterday", "-last_7_days", "-last_30_days", 
             "-last_90_days", "-lifelong"))
  }) %>% 
  unlist() %>% 
  .[str_detect(., "-last_7_days")] %>% 
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
  })
  



# fin_page <- read_html("https://github.com/favstats/meta_ad_reports/releases") %>% 
#   html_elements(".pagination") %>% 
#   html_children() %>% 
#   html_text() %>% 
#   as.numeric() %>% 
#   na.omit() %>% 
#   max()
# 
# retrieve_releases <- function(pageee) {
#   
#   urrrl <- paste0("https://github.com/favstats/meta_ad_reports/releases?page=", pageee)
#   
#   the_tags  <- read_html(urrrl) %>% 
#     html_elements(".Link--primary") %>% 
#     html_text()
#   
#   the_tags %>% 
#     map_dfr(~{
#       the_assets <- httr::GET(paste0("https://github.com/favstats/meta_ad_reports/releases/expanded_assets/", .x))
#       
#       the_assets %>% httr::content() %>% 
#         html_elements(".Box-row") %>% 
#         html_text()  %>%
#         tibble(raw = .)   %>%
#         # Split the raw column into separate lines
#         mutate(raw = strsplit(as.character(raw), "\n")) %>%
#         # Extract the relevant lines for filename, file size, and timestamp
#         transmute(
#           filename = sapply(raw, function(x) trimws(x[3])),
#           file_size = sapply(raw, function(x) trimws(x[6])),
#           timestamp = sapply(raw, function(x) trimws(x[7]))
#         ) %>% 
#         filter(filename != "Source code") %>% 
#         mutate(release = .x)
#     })
#   
#   
#   
# }
# 
# out <- 1:fin_page %>% map_dfr_progress(retrieve_releases)
# 
# 
# saveRDS(out, "out.rds")





# retrieve_releases <- function(pageee) {
#   
#   page_df %>% goto(paste0("https://github.com/favstats/meta_ad_reports/releases?page=", pageee))
#   
#   # Sys.sleep(5)
#   
#   # print("yo")
#   
#   # assets_dat <- page_df %>% get_by_text("Assets")
#   # assets_dat <- page_df %>% get_by_selector("details")
#   
#   the_content <- page_df %>% get_content() 
#   # print("sd")
#   
#   fin <- the_content  %>% 
#     html_elements(".Link--primary") %>% 
#     html_attr("href") %>% 
#     paste0("https://github.com/", .) %>% 
#     walk(~{
#       
#       # print("asd")
#       page_df %>% goto(.x)
#       # print("sd")
#       
#       the_content <<- page_df %>% get_content() 
#       the_boxes_raw <- the_content %>% 
#         html_elements(".Box") 
#       
#       release_name <- the_boxes_raw %>% 
#         html_elements(".Link--primary") %>% 
#         html_text() %>% 
#         str_squish()
#       # print("2323")
#       counter <<- 1 
#       fr <- the_boxes_raw %>% 
#         html_elements(".Box-footer") %>% 
#         map_dfr(~{
#           
#           # print("hello")
#           fin <- .x %>% 
#             html_elements(".Box-row") %>% 
#             html_text() %>% 
#             tibble(raw = .)   %>%
#             # Split the raw column into separate lines
#             mutate(raw = strsplit(as.character(raw), "\n")) %>%
#             # Extract the relevant lines for filename, file size, and timestamp
#             transmute(
#               filename = sapply(raw, function(x) trimws(x[3])),
#               file_size = sapply(raw, function(x) trimws(x[6])),
#               timestamp = sapply(raw, function(x) trimws(x[7]))
#             ) %>% 
#             filter(filename != "Source code") %>% 
#             mutate(release = release_name[counter]) %>% 
#             mutate_all(as.character)
#           # print("hello2")
#           counter <<- 1 + counter
#           
#           return(fin)
#           
#         })
#       
#       
#       
#       return(fr)
#     })
#   
#   # assets_dat <- page_df %>% get_by_selector(".Link--primary Link")
#   # 
#   # 
#   # assets_dat %>%
#   #   slice(-1) %>%
#   #   split(1:nrow(.)) %>% walk(~{Sys.sleep(0.5);click(.x)})
#   # 
#   # print("yooo")
#   
#   # Sys.sleep(5)
#   
#   return(fin)
# }








# if(!("playwrightr" %in% tibble::as_tibble(installed.packages())$Package)){
#   remotes::install_github("benjaminguinaudeau/playwrightr")
# }
# 
# 
# if(Sys.info()[["sysname"]]=="Windows"){
#   
#   pw_init(use_xvfb = F)
# } else{
#   
#   conda_install(packages = "xvfbwrapper", pip = T)
#   
#   print("installed xvfbwrapper")
#   conda_install(packages = "playwright", pip = T)
#   print("installed playwright")
#   
#   pw_init(use_xvfb = T)
#   system("playwright install")
# }
# 
# 
# browser_df <- browser_launch(
#   headless = F,
#   browser = "firefox",
#   user_agent = NULL,
#   user_data_dir = "out"
# )
# 
# 
# 
# 
# print("headlesss")
# # Create a new page
# 
# # page_df <- new_page(browser_df)
# page_df <- browser_df %>%
#   glimpse
# 
# page_df %>% goto("https://github.com/favstats/meta_ad_reports/releases")
# 
# the_content <- page_df %>% get_content() 
# 
# fin_page <- the_content %>% 
#   html_elements(".pagination") %>% 
#   html_children() %>% 
#   html_text() %>% 
#   as.numeric() %>% 
#   na.omit() %>% 
#   max()
