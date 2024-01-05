
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

calc_targeting <- function(only_tags) {
  
  total_sppppeen <- only_tags %>%
    distinct(internal_id, .keep_all = T)  %>%
    # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
    mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
    select(internal_id, total_spend) %>%
    arrange(desc(total_spend)) %>%
    summarize(total_spend = sum(total_spend))
  
  howmuchisinterest <- only_tags %>%
    filter(type == "detailed") %>%
    group_by(internal_id) %>%
    filter(total_spend_pct == max(total_spend_pct)) %>%
    slice(1) %>%
    ungroup() %>%
    # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
    mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
    mutate(spend_per = total_spend * total_spend_pct) %>%
    select(internal_id, spend_per) %>%
    arrange(desc(spend_per)) %>%
    summarize(spend_per = sum(spend_per)) %>%
    mutate(target = "interest")
  
  howmuchislocation <- only_tags %>%
    filter(type == "location") %>%
    group_by(internal_id, location_type) %>%
    filter(total_spend_pct == max(total_spend_pct)) %>%
    slice(1) %>%
    ungroup() %>%
    # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
    mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
    mutate(spend_per = total_spend * total_spend_pct) %>%
    select(internal_id, spend_per, location_type) %>%
    arrange(desc(spend_per)) %>%
    group_by(location_type) %>%
    summarize(spend_per = sum(spend_per)) %>%
    rename(target = location_type)
  
  howmuchisage <- only_tags %>%
    filter(type == "age") %>%
    filter(total_spend_pct != 0) %>%
    group_by(internal_id) %>%
    mutate(n_ages = n()) %>% #count(n_ages, sort = T)
    ungroup() %>%
    filter(n_ages <= 47) %>%
    group_by(internal_id) %>%
    filter(total_spend_pct == max(total_spend_pct)) %>%
    slice(1) %>%
    ungroup() %>%
    # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
    mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
    mutate(spend_per = total_spend * total_spend_pct) %>%
    select(internal_id, spend_per) %>%
    summarize(spend_per = sum(spend_per))  %>%
    mutate(target = "age")
  
  howmuchisgender <- only_tags %>%
    filter(type == "gender") %>%
    filter(total_spend_pct != 0) %>%
    filter(value != "All") %>%
    group_by(internal_id) %>%
    filter(total_spend_pct == max(total_spend_pct)) %>%
    slice(1) %>%
    ungroup() %>%
    # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
    mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
    mutate(spend_per = total_spend * total_spend_pct) %>%
    select(internal_id, spend_per) %>%
    summarize(spend_per = sum(spend_per))  %>%
    mutate(target = "gender")
  
  howmuchcustom <- only_tags %>%
    filter(type == "custom_audience") %>%
    filter(total_spend_pct != 0) %>%
    # filter(value != "All") %>%
    group_by(internal_id) %>%
    filter(total_spend_pct == max(total_spend_pct)) %>%
    slice(1) %>%
    ungroup() %>%
    # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
    mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
    mutate(spend_per = total_spend * total_spend_pct) %>%
    select(internal_id, spend_per) %>%
    summarize(spend_per = sum(spend_per))  %>%
    mutate(target = "custom_audience")
  
  
  howmuchlookalike <- only_tags %>%
    filter(type == "lookalike_audience") %>%
    filter(total_spend_pct != 0) %>%
    # filter(value != "All") %>%
    group_by(internal_id) %>%
    filter(total_spend_pct == max(total_spend_pct)) %>%
    slice(1) %>%
    ungroup() %>%
    # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
    mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
    mutate(spend_per = total_spend * total_spend_pct) %>%
    select(internal_id, spend_per) %>%
    summarize(spend_per = sum(spend_per))  %>%
    mutate(target = "lookalike_audience")
  
  howmuchlanguage <- only_tags %>%
    filter(type == "language") %>%
    filter(total_spend_pct != 0) %>%
    drop_na(value) %>%
    # filter(value != "All") %>%
    group_by(internal_id) %>%
    filter(total_spend_pct == max(total_spend_pct)) %>%
    slice(1) %>%
    ungroup() %>%
    # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
    mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
    mutate(spend_per = total_spend * total_spend_pct) %>%
    select(internal_id, spend_per) %>%
    summarize(spend_per = sum(spend_per))  %>%
    mutate(target = "language")
  
  targeting_on_each <- howmuchisinterest %>%
    bind_rows(howmuchislocation) %>%
    bind_rows(howmuchisage) %>%
    bind_rows(howmuchisgender) %>%
    bind_rows(howmuchcustom) %>%
    bind_rows(howmuchlookalike) %>%
    bind_rows(howmuchlanguage) %>%
    mutate(total = total_sppppeen$total_spend) %>%
    mutate(perc = spend_per/total*100) %>%
    arrange(desc(perc))
  
  return(targeting_on_each)
}

relationshipstuff <- "Widowed|Recently moved|Away|[r|R]elationship|Parents|Partner|Separated|Divorced|Single|Complicated|Married|Engaged|Newlywed|Civil Union|Unspecified|Newly engaged"


add_ribbons <- function(x, adv, col) {
  x %>%
    tab_options(table.width = pct(100)) %>%
    tab_style(
      style = cell_borders(
        sides = c("left"),
        color = col,
        weight = px(18.5),
        style = "solid"
      ),
      locations = cells_body(
        columns = `Number of Advertisers`,
        rows = adv
      ))
}




get_targeting <- function(id, timeframe = "LAST_30_DAYS") {
  
  url <- "https://www.facebook.com/api/graphql/"
  
  heads_up <- httr::add_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:93.0) Gecko/20100101 Firefox/93.0",
                                Accept = "*/*",
                                `Accept-Language` = 'en-US,en;q=0.5',
                                `X-FB-Friendly-Name` = "AdLibraryPageAudienceTabQuery",
                                `X-FB-LSD`= "AVrNiQCSUnA",
                                `Alt-Used`= "www.facebook.com",
                                `Sec-Fetch-Dest`= "empty",
                                `Sec-Fetch-Mode`= "cors",
                                `Sec-Fetch-Site`= "same-origin",
                                # `Accept-Encoding` = "gzip, deflate, br",
                                `Content-Type` = "application/x-www-form-urlencoded",
                                Connection = "keep-alive"
  )
  
  
  if(timeframe == "LAST_30_DAYS"){
    
    # audienceTimeframe <- "%7B%22audienceTimeframe%22%3A%22LAST_30_DAYS%22%2C%22"
    da_body <- glue::glue("av=0&__user=0&__a=1&__dyn=7xeUmxa3-Q8zo5ObwKBWobVo9E4a2i5U4e1FxebzEdF8ixy7EiwvoWdwJwCwAwgU2lxS6Ehwem0nCqbwgE3awbG78b87C1xwEwgolzUO0n2US2G3i1ywa-2l0Fwwwi831wnFokwyx2cw8WfK6E5i3e4U3mxOu2S2W2K7o725U4q0HUkyE9E11EbodEGdw46wbLwiU8U6C2-&__csr=&__req=m&__hs=19237.BP%3ADEFAULT.2.0.0.0.0&dpr=1&__ccg=EXCELLENT&__rev=1006139712&__s=ll61s1%3Axn89ey%3Admpplc&__hsi=7138774996758193009&__comet_req=0&lsd=AVrNiQCSYrc&jazoest=2981&__spin_r=1006139712&__spin_b=trunk&__spin_t=1662125577&__jssesw=1&fb_api_caller_class=RelayModern&fb_api_req_friendly_name=AdLibraryPageAudienceTabQuery&variables=%7B%22audienceTimeframe%22%3A%22LAST_30_DAYS%22%2C%22viewAllPageID%22%3A%22{id}%22%7D&server_timestamps=true&doc_id=4756112137823411") %>% as.character()
    
  } else if (timeframe == "LAST_7_DAYS"){
    
    # audienceTimeframe <- "%7B%22"
    da_body <- glue::glue("av=0&__user=0&__a=1&__dyn=7xeUmxa3-Q8zo5ObwKBWobVo9E4a2i5U4e1FxebzEdF8aUuxa1ZzES2S2q2i13w9m7oqx60Vo1upEK12wcG0KEswIwuo662y11xmfz81sbzoaEd86a0HU9k2C2218wc61uBxi2a48O0zE-Uqwl8cUjwdq79UbobEaUtws8nwhE2LxiawCw46wJwSyES0gq0K-1bwzwqobU&__csr=&__req=f&__hs=19245.BP%3ADEFAULT.2.0.0.0.0&dpr=1&__ccg=EXCELLENT&__rev=1006179750&__s=njkc5w%3A6o847a%3A9gcoa8&__hsi=7141736891942848978&__comet_req=0&lsd=AVrbeuAiHJg&jazoest=21000&__spin_r=1006179750&__spin_b=trunk&__spin_t=1662815197&__jssesw=1&fb_api_caller_class=RelayModern&fb_api_req_friendly_name=AdLibraryPageAudienceTabQuery&variables=%7B%22audienceTimeframe%22%3A%22LAST_7_DAYS%22%2C%22viewAllPageID%22%3A%22{id}%22%7D&server_timestamps=true&doc_id=4756112137823411") %>% as.character()
    
  } else if (timeframe == "LAST_90_DAYS"){
    
    da_body <- glue::glue("av=0&__user=0&__a=1&__dyn=7xeUmxa3-Q8zo5ObwKBWobVo9E4a2i5U4e1FxebzEdF8aUuxa1ZzES2S2q2i13w9m7oqx60Vo1upEK12wcG0KEswIwuo662y11xmfz81sbzoaEd86a0HU9k2C2218wc61uBxi2a48O3u1mzXxG1kwPxe3C0D8sDwJwKwHxS1Mxu16wa-58G2q0gq2S3qazo11E2XU4K2e1FwLw8O2i&__csr=&__req=h&__hs=19301.BP%3ADEFAULT.2.0.0.0.0&dpr=1&__ccg=EXCELLENT&__rev=1006553893&__s=20shv5%3A62a2bj%3A6goj90&__hsi=7162612241770415577&__comet_req=0&lsd=AVohzhTn68E&jazoest=2965&__spin_r=1006553893&__spin_b=trunk&__spin_t=1667675618&__jssesw=1&fb_api_caller_class=RelayModern&fb_api_req_friendly_name=AdLibraryPageAudienceTabQuery&variables=%7B%22audienceTimeframe%22%3A%22LAST_90_DAYS%22%2C%22viewAllPageID%22%3A%22{id}%22%7D&server_timestamps=true&doc_id=4756112137823411") %>% as.character()
    
    url <- "https://www.facebook.com/api/graphql/"
    
    heads_up <- httr::add_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:93.0) Gecko/20100101 Firefox/93.0",
                                  Accept = "*/*",
                                  `Accept-Language` = 'en-US,en;q=0.5',
                                  `X-FB-Friendly-Name` = "AdLibraryPageAudienceTabQuery",
                                  `X-FB-LSD`= "AVrNiQCSUnA",
                                  `Alt-Used`= "www.facebook.com",
                                  `Sec-Fetch-Dest`= "empty",
                                  `Sec-Fetch-Mode`= "cors",
                                  `Sec-Fetch-Site`= "same-origin",
                                  # `Accept-Encoding` = "gzip, deflate, br",
                                  `Content-Type` = "application/x-www-form-urlencoded",
                                  Connection = "keep-alive"
    )
    
  }
  
  
  
  
  
  posted = httr::POST(url, heads_up, body = da_body)
  
  contentwise <- httr::content(posted)
  
  rate_limit <- str_detect(as.character(contentwise), "Rate limit exceeded")
  if(rate_limit){
    warning(as.character(contentwise))
    print("BLOCKED!")
    return(tibble(blocked=T))
  }
  
  
  
  out_raw <- contentwise %>%
    rvest::html_nodes("body") %>%
    rvest::html_nodes("p") %>%
    as.character() %>% str_remove_all("</p>|<p>") %>%
    jsonlite::fromJSON()  %>%
    purrr::pluck("data") %>%
    purrr::pluck("page") %>%
    purrr::pluck("ad_library_page_targeting_insight")
  
  
  summary_dat <- out_raw %>%
    purrr::pluck("ad_library_page_targeting_summary") %>%
    dplyr::bind_rows()
  
  if(nrow(summary_dat) > 1){
    
    summary_dat <- summary_dat %>%
      dplyr::slice(which(summary_dat$detailed_spend$currency == summary_dat$main_currency)) %>%
      dplyr::select(-detailed_spend)
    
  }
  
  targeting_details_raw <- out_raw[!(names(out_raw) %in% c("ad_library_page_targeting_summary", "ad_library_page_has_siep_ads"))]
  
  # names(targeting_details_raw)
  
  res <- targeting_details_raw %>%
    purrr::discard(purrr::is_empty) %>%
    purrr::imap_dfr(~{.x %>% dplyr::mutate(type = .y %>% stringr::str_remove("ad_library_page_targeting_"))}) %>%
    dplyr::bind_cols(summary_dat) %>%
    dplyr::mutate(internal_id = id)
  
  return(res)
  
}

get_targeting <- suppressWarnings(get_targeting)

append_date_suffix <- function(dates){
  dayy <- lubridate::day(dates)
  suff <- case_when(dayy %in% c(11,12,13) ~ "th",
                    dayy %% 10 == 1 ~ 'st',
                    dayy %% 10 == 2 ~ 'nd',
                    dayy %% 10 == 3 ~'rd',
                    TRUE ~ "th")
  paste0(dayy, suff)
}

create_date <- function(x) {
  the_date <- format(x, "%b %d")
  the_date <- ifelse(str_detect(the_date, " 0"),
                     str_remove(the_date, "0"),
                     the_date)
  str_replace(the_date, 
              as.character(lubridate::day(x)), 
              append_date_suffix(x))
}


scale_fill_parties <- function(...){
  ggplot2:::manual_scale(
    'fill', 
    values = setNames(color_dat$colors, color_dat$party), 
    ...
  )
}
scale_color_parties <- function(...){
  ggplot2:::manual_scale(
    'color', 
    values = setNames(color_dat$colors, color_dat$party), 
    ...
  )
}


pb_upload_file_fr <- function (file, repo, tag, .token = gh::gh_token(), releases, dir = NULL) {
  # Construct the file path
  file_path <- do.call(file.path, compact(list(dir, file)))
  
  # Check if the file exists
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  # full_repos$file_name
  # Obtain release information
  # releases <- pb_releases(repo = repo, .token = .token)
  upload_url <- releases$upload_url[releases$tag == tag][1]
  # print(upload_url)
  # Set up the request
  rsd <- httr::POST(
    # "POST",
    url = sub("\\{.+$", "", upload_url),
    query = list(name = basename(file_path)),
    httr::add_headers(Authorization = paste("token", .token)),
    body = httr::upload_file(file_path)
  )

  iso_to_emoji_unicode = function(iso_codes)
  {
    # check input
    if (!any(
      nchar(iso_codes) == 2 |
      is.na(iso_codes)))
    {
      stop('iso_to_emoji: ISO codes must be two (2) letters long.')
    }
    if (!any(
      str_detect(iso_codes, pattern = '[a-zA-Z][a-zA-Z]') |
      is.na(iso_codes)))
    {
      stop('iso_to_emoji: ISO codes must be letters only.')
    }
    
    # substitute unicode regional indicator symbols for the original characters
    return(str_replace_all(str_to_lower(iso_codes), c('a' = '🇦', 'b' = '🇧', 'c' = '🇨',
                                                      'd' = '🇩', 'e' = '🇪', 'f' = '🇫', 'g' = '🇬', 'h' = '🇭', 'i' = '🇮',
                                                      'j' = '🇯', 'k' = '🇰', 'l' = '🇱', 'm' = '🇲', 'n' = '🇳', 'o' = '🇴',
                                                      'p' = '🇵', 'q' = '🇶', 'r' = '🇷', 's' = '🇸', 't' = '🇹', 'u' = '🇺',
                                                      'v' = '🇻', 'w' = '🇼', 'x' = '🇽', 'y' = '🇾', 'z' = '🇿')))
  }
  
  if(!is.null(httr::content(rsd)$errors[[1]]$code)){
    # tag <- "EE-last_7_days"
    
    df <- releases[releases$tag == tag,]
    
    if(basename(file_path)=="latest.rds"){
      
      # df <- piggyback:::pb_info(repo = repo, tag = tag, .token = .token)
      i <- which(df$file_name == "latest.rds")
      # if (length(i) > 0) {
      # if (use_timestamps) {
      #   local_timestamp <- fs::file_info(file_path)$modification_time
      #   no_update <- local_timestamp <= df[i, "timestamp"]
      #   if (no_update) {
      #     cli::cli_warn("Matching or more recent version of {.file {file_path}} found on GH, not uploading.")
      #     return(invisible(NULL))
      #   }
      # }
      # if (overwrite) {
      gh::gh("DELETE /repos/:owner/:repo/releases/assets/:id", 
             owner = df$owner[[1]], repo = df$repo[[1]], 
             id = df$id[i], .token = .token)
      # }
      # else {
      #   cli::cli_warn("Skipping upload of {.file {df$file_name[i]}} as file exists on GitHub and {.code overwrite = FALSE}")
      #   return(invisible(NULL))
      # }
      # }
      rsd <- httr::POST(
        # "POST",
        url = sub("\\{.+$", "", upload_url),
        query = list(name = basename(file_path)),
        httr::add_headers(Authorization = paste("token", .token)),
        body = httr::upload_file(file_path)
      )
      
    } 
      
    httr::warn_for_status(rsd)
    # invisible(rsd)
    
    return(NULL)
   
  
  }
  
  # Handle response
  httr::warn_for_status(rsd)
  invisible(rsd)
}



pb_release_create_fr <- function (repo, tag, commit = NULL, name = tag, 
                                  body = "Data release", draft = FALSE, prerelease = FALSE, releases,
                                  .token = gh::gh_token()) {
  if(is.null(releases)){
    releases <- pb_releases(repo = repo, .token = .token, verbose = FALSE)
  }
  
  if (nrow(releases) > 0 && tag %in% releases$tag) {
    cli::cli_warn("Failed to create release: {.val {tag}} already exists!")
    return(invisible(releases[tag %in% releases$tag, 
    ]))
  }
  r <- piggyback:::parse_repo(repo)
  payload <- compact(list(tag_name = tag, target_commitish = commit, 
                          name = name, body = body, draft = draft, prerelease = prerelease))
  resp <- httr::RETRY(verb = "POST", url = glue::glue("https://api.github.com/repos/{r[[1]]}/{r[[2]]}/releases"), 
                      httr::add_headers(Authorization = paste("token", .token)), 
                      body = jsonlite::toJSON(payload, auto_unbox = TRUE), 
                      terminate_on = c(400, 401, 403, 404, 422))
  if (httr::http_error(resp)) {
    cli::cli_warn(c(`!` = "Failed to create release: HTTP error {.val {httr::status_code(resp)}}.", 
                    "See returned error messages for more details"))
    return(httr::content(resp))
  }
  piggyback:::.pb_cache_clear()
  release <- httr::content(resp)
  cli::cli_alert_success("Created new release {.val {name}}.")
  return(invisible(release))
}

# Define a function to perform the operation
get_full_release <- function() {
  tryCatch({
    # Your original operation
    full_repos <- piggyback:::pb_info("favstats/meta_ad_reports") %>% as_tibble()
    
    return(full_repos)  # return the result
  }, error = function(e) {
    # Print the error message
    print(paste("Error occurred: ", e$message))
    
    # Wait for an hour (3600 seconds)
    print("Waiting for 1 hour before retrying...")
    Sys.sleep(3600)
    
    # Retry the operation
    return(get_full_release())
  })
}


iso_to_emoji_unicode <- function(iso_codes){
  # check input
  if (!any(
    nchar(iso_codes) == 2 |
    is.na(iso_codes)))
  {
    stop('iso_to_emoji: ISO codes must be two (2) letters long.')
  }
  if (!any(
    str_detect(iso_codes, pattern = '[a-zA-Z][a-zA-Z]') |
    is.na(iso_codes)))
  {
    stop('iso_to_emoji: ISO codes must be letters only.')
  }
  
  # substitute unicode regional indicator symbols for the original characters
  return(str_replace_all(str_to_lower(iso_codes), c('a' = '\ud83c\udde6', 'b' = '\ud83c\udde7', 'c' = '\ud83c\udde8',
                                                    'd' = '\ud83c\udde9', 'e' = '\ud83c\uddea', 'f' = '\ud83c\uddeb', 'g' = '\ud83c\uddec', 'h' = '\ud83c\udded', 'i' = '\ud83c\uddee',
                                                    'j' = '\ud83c\uddef', 'k' = '\ud83c\uddf0', 'l' = '\ud83c\uddf1', 'm' = '\ud83c\uddf2', 'n' = '\ud83c\uddf3', 'o' = '\ud83c\uddf4',
                                                    'p' = '\ud83c\uddf5', 'q' = '\ud83c\uddf6', 'r' = '\ud83c\uddf7', 's' = '\ud83c\uddf8', 't' = '\ud83c\uddf9', 'u' = '\ud83c\uddfa',
                                                    'v' = '\ud83c\uddfb', 'w' = '\ud83c\uddfc', 'x' = '\ud83c\uddfd', 'y' = '\ud83c\uddfe', 'z' = '\ud83c\uddff')))
}


source("https://raw.githubusercontent.com/favstats/appendornot/master/R/save.R")
