#New Code to query FantasyPros Data


fp_auth <- function() { 
  
  fp_auth <- list(
    user_agent="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
    x_api_key="zjxN52G3lP4fORpHRftGI2mTU8cTwxVNvkjByM3j",
    authorization="Basic WC1BcGktS2V5OnpqeE41MkczbFA0Zk9ScEhSZnRHSTJtVFU4Y1R3eFZOdmtqQnlNM2o="
  )

}

fp_build_url <- function(base = "https://api.fantasypros.com/v2/json/nfl/", year, type, scoring, pos, week){

  fp_url <- paste0(base,year,"/consensus-rankings?type=",type,
               "&scoring=",scoring,"&position=",pos,"&week=",week,"&experts=available")
}

fp_get_ranking_data <- function(year, type, scoring, pos, week) {
  fp_auth = fp_auth()
    if (type == "both")  {
    fp_url <- fp_build_url(year = year, type = "draft", scoring = scoring, pos = pos, week = week)
    data_ecr <- httr::GET(fp_url,httr::add_headers("user-agent" = fp_auth$user_agent,
                                                   "x-api-key" =  fp_auth$x_api_key,
                                                   "Authorization" =  fp_auth$authorization)) %>% 
      httr::content("parsed", "application/json") %>% .[["players"]] %>% 
      sapply(c) %>% t() %>% data.frame() %>% janitor::clean_names() %>% as_tibble() %>% 
      dplyr::select(player = player_name, id = player_id, team = player_team_id, position = player_position_id,
      ecr_tier = tier,  ecr_pos_rank = pos_rank, ecr_rank = rank_ecr, ecr_min = rank_min, 
      ecr_max = rank_max, ecr_rank_avg = rank_ave, ecr_rank_std = rank_std) %>% mutate(as.numeric(ecr_rank_avg), as.numeric(.$ecr_rank_std)) %>%
      lapply(unlist) %>% 
      as.data.frame()

    data_adp <- httr::GET(fp_url,httr::add_headers("user-agent" = fp_auth$user_agent,
                                                   "x-api-key" =  fp_auth$x_api_key,
                                                   "Authorization" =  fp_auth$authorization)) %>% 
      httr::content("parsed", "application/json") %>% .[["players"]] %>% 
      sapply(c) %>% t() %>% data.frame() %>% janitor::clean_names() %>% as_tibble() %>% 
      dplyr::select(id = player_id,
      adp_tier = tier,  adp_pos_rank = pos_rank, adp_rank = rank_ecr, adp_min = rank_min, 
      adp_max = rank_max, adp_rank_avg = rank_ave, adp_rank_std = rank_std) %>%
      lapply(unlist) %>% 
      as.data.frame()

    fp_draft_rankings <- dplyr::inner_join(data_ecr, data_adp, by = "id")

  } else if (type == "draft") {
      fp_url <- fp_build_url(year = year, type = "draft", scoring = scoring, pos = pos, week = week)
      fp_draft_rankings <- httr::GET(fp_url,httr::add_headers("user-agent" = fp_auth$user_agent,
                                                              "x-api-key" =  fp_auth$x_api_key,
                                                              "Authorization" =  fp_auth$authorization)) %>% 
        httr::content("parsed", "application/json") %>% .[["players"]] %>% 
        sapply(c) %>% t() %>% data.frame() %>% janitor::clean_names() %>% as_tibble() %>% 
        dplyr::select(player = player_name, id = player_id, team = player_team_id, position = player_position_id,
        ecr_tier = tier,  ecr_pos_rank = pos_rank, ecr_rank = rank_ecr, ecr_min = rank_min, 
        ecr_ecr_max = rank_max, ecr_rank_avg = rank_ave, ecr_rank_std = rank_std) %>% mutate(as.numeric(ecr_rank_avg), as.numeric(.$ecr_rank_std)) %>% 
        lapply(unlist) %>% 
        as.data.frame()

  } else if (type == "adp") {
      fp_url <- fp_build_url(year = year, type = "adp", scoring = scoring, pos = pos, week = week)
      fp_draft_rankings <- httr::GET(fp_url,httr::add_headers("user-agent" = fp_auth$user_agent,
                                                              "x-api-key" =  fp_auth$x_api_key,
                                                              "Authorization" =  fp_auth$authorization)) %>% 
        httr::content("parsed", "application/json") %>% .[["players"]] %>% 
        sapply(c) %>% t() %>% data.frame() %>% janitor::clean_names() %>% as_tibble() %>% 
        dplyr::select(player = player_name, id = player_id, team = player_team_id, position = player_position_id,
        adp_tier = tier,  adp_pos_rank = pos_rank, adp_rank = rank_ecr, adp_min = rank_min, 
        adp_max = rank_max, adp_rank_avg = rank_ave, adp_rank_std = rank_std) %>%
        lapply(unlist) %>% 
        as.data.frame()
  }
}
