library(tidyverse)
library(piggyback)
Sys.setenv("piggyback_cache_duration"=3600)

source("utils.R")

# release_names <- pb_releases()$release_name

# releases <- pb_releases()

full_cntry_list <- readRDS("cntry_list.rds") %>% 
  rename(iso2c = iso2,
         country = cntry)   
# storage30/AI/2024-01-01.rds

# full_repos <- pb_list() %>% as_tibble()

full_repos <- piggyback:::pb_info("favstats/meta_ad_reports") %>% as_tibble()
release_names <- full_repos$tag %>% unique


thosearethere <- full_repos %>% 
  arrange(desc(tag)) %>% 
  separate(tag, into = c("country", "timeframe"), remove = F, sep = "-") #%>% 
  # mutate(day  = str_remove(file_name, "\\.rds|\\.zip")) %>% 
  # distinct(country, day, timeframe) #%>% 
  # filter(str_detect(timeframe, time_preset)) %>% 
  # mutate(day = lubridate::ymd(day))

tat_path <- thosearethere %>% 
  mutate(path = paste0("storage", readr::parse_number(timeframe),"/", country, "/", file_name))

report_paths <- dir("storage30", full.names = T, recursive = T) %>%
  c(dir("storage7", full.names = T, recursive = T)) %>%
  c(dir("storage90", full.names = T, recursive = T)) %>%
  sort(decreasing = T) %>%
  setdiff(tat_path$path)

report_paths %>% 
  walk_progress(~{
    
    if(str_detect(.x, "30")) tframe <- "last_30_days"
    if(str_detect(.x, "7")) tframe <- "last_7_days"
    if(str_detect(.x, "90")) tframe <- "last_90_days"
    
    cntry_str <- str_split(.x, "/") %>% unlist %>% .[2]
    
    the_tag <- paste0(cntry_str, "-", tframe)
    
    cntry_name <- full_cntry_list %>% 
      filter(iso2c == cntry_str) %>% 
      pull(country)
    
    if(!(the_tag %in% release_names)){
      pb_release_create_fr(repo = "favstats/meta_ad_reports", 
                           tag = the_tag,
                           body = paste0("This release includes ", cntry_name ," '", tframe ,"' Meta ad spending reports."), 
                           releases = full_repos)
      
      # Sys.sleep(5)
    } else {
      
      pb_upload_file_fr(.x, 
                        repo = "favstats/meta_ad_reports",
                        tag = the_tag, releases = full_repos)
      
    }
    
    # pb_upload_file_fr(paste0(cntry_str, ".rds"), repo = "favstats/meta_ad_reports", tag = the_tag, overwrite = T)
    

    
  })


# piggyback:::pb_upload_file()
# piggyback:::pb_upload_file(c("storage7/EE/2024-01-01.rds"), 
#                            repo = "favstats/meta_ad_reports", 
#                            tag = "EE-last_7_days", overwrite = T)
# 
# 
# pb_upload_file_fr <- function (file, repo, tag, .token = gh::gh_token(), releases, dir = NULL) {
#   # Construct the file path
#   file_path <- do.call(file.path, compact(list(dir, file)))
#   
#   # Check if the file exists
#   if (!file.exists(file_path)) {
#     stop("File does not exist: ", file_path)
#   }
#   
#   # Obtain release information
#   # releases <- pb_releases(repo = repo, .token = .token)
#   upload_url <- releases$upload_url[releases$tag_name == tag]
#   print(upload_url)
#   # Set up the request
#   rsd <<- httr::POST(
#     # "POST",
#     url = sub("\\{.+$", "", upload_url),
#     query = list(name = basename(file_path)),
#     httr::add_headers(Authorization = paste("token", .token)),
#     body = httr::upload_file(file_path)
#   )
#   
#   if(!is.null(httr::content(rsd)$errors[[1]]$code)){
#     return(NULL)
#   }
#   
#   # Handle response
#   httr::warn_for_status(rsd)
#   invisible(rsd)
# }
# 
# df <- piggyback:::pb_info(repo = "favstats/meta_ad_reports", tag = "EE-last_7_days", .token = gh::gh_token())
# 
# httr::headers(rsd)
# httr::content(rsd)
# 
# dir("storage30", full.names = T, recursive = T) %>%
#   c(dir("storage7", full.names = T, recursive = T)) %>%
#   c(dir("storage90", full.names = T, recursive = T))
# 
# pb_upload_file_fr("storage7/DO/latest.rds"     , 
#                   repo = "favstats/meta_ad_reports",
#                   tag = "DO-last_7_days", releases = releases)
# 
# pb_release_create_fr <- function (repo, tag, commit = NULL, name = tag, 
#           body = "Data release", draft = FALSE, prerelease = FALSE, releases,
#           .token = gh::gh_token()) {
#   if(is.null(releases)){
#     releases <- pb_releases(repo = repo, .token = .token, verbose = FALSE)
#     
#   }
#   if (nrow(releases) > 0 && tag %in% releases$tag_name) {
#     cli::cli_warn("Failed to create release: {.val {tag}} already exists!")
#     return(invisible(releases[tag %in% releases$tag_name, 
#     ]))
#   }
#   r <- piggyback:::parse_repo(repo)
#   payload <- compact(list(tag_name = tag, target_commitish = commit, 
#                           name = name, body = body, draft = draft, prerelease = prerelease))
#   resp <- httr::RETRY(verb = "POST", url = glue::glue("https://api.github.com/repos/{r[[1]]}/{r[[2]]}/releases"), 
#                       httr::add_headers(Authorization = paste("token", .token)), 
#                       body = jsonlite::toJSON(payload, auto_unbox = TRUE), 
#                       terminate_on = c(400, 401, 403, 404, 422))
#   if (httr::http_error(resp)) {
#     cli::cli_warn(c(`!` = "Failed to create release: HTTP error {.val {httr::status_code(resp)}}.", 
#                     "See returned error messages for more details"))
#     return(httr::content(resp))
#   }
#   piggyback:::.pb_cache_clear()
#   release <- httr::content(resp)
#   cli::cli_alert_success("Created new release {.val {name}}.")
#   return(invisible(release))
# }
