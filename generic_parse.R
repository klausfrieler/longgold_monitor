library(tidyverse)

adaptive_tests <- c("JAJ", "EDT", "MPT", "MDT", "BAT", "PIT", "HPT", "MIQ", "RAT", "HPT")
dummy <- list()

is_debug_id <- function(p_id){
  substr(p_id, 1, 1) == "!" | substr(p_id, 1, 1) == "#" | substr(p_id, 1, 6) == "000000"
}

make_adaptive_test_dummy <- function(test_id){
  dummy <- tibble(ability  = NA, ability_sem  = NA, num_items = NA)
  names(dummy) <- sprintf("%s.%s", test_id, names(dummy))
  dummy
}

make_quest_dummy <- function(test_id){
  subscales <- psyquest::get_subscales(test_id) %>% 
    str_split(";") %>% 
    unlist() %>% 
    unique() %>% 
    tolower() %>% 
    str_replace_all(" ", "_") %>% 
    sprintf("%s.%s", test_id, .)
  
  if(length(subscales) == 0){
    stop(sprintf("Invalid test_id '%s' in make_quest_dummy", test_id))
  }
  
  matrix(rep(NA, length(subscales)), nrow = 1) %>% 
    as_tibble(.name_repair = "minimal") %>% 
    set_names(subscales) %>% mutate_if(is.logical, as.numeric)
  
}

filter_for_test <- function(results, test_id){
  map(results %>% as.list(), function(x){
    #browser()
    data <- x$data$value[[test_id]]
    if(is.null(data)){
      return(NULL)
    }
    tibble(p_id = x$participant_id, data = as.data.frame(data))    
  }) %>% discard(is.null)
}

dummy <- append(map(adaptive_tests, make_adaptive_test_dummy), map(psyquest::get_tests(), make_quest_dummy))
names(dummy) <- c(adaptive_tests, psyquest::get_tests())

post_process <- function(entry, test_id){
  if(test_id == "DEG"){
    if("DEG.handedness" %in% names(entry)){
      entry$DEG.handedness_writing <- c("right", "left", "both")[[entry$DEG.handedness[[2]]]] 
      entry$DEG.handedness <- c("right", "left", "both")[[entry$DEG.handedness[[1]]]] 
    }
    if("DEG.age" %in% names(entry)){
      entry$DEG.age <- entry$DEG.age/12
    }
    if("DEG.gender" %in% names(entry)){
      entry$DEG.gender = factor(entry$DEG.gender, 
                                levels = 1:4, 
                                labels = c("female", "male", "other", "rather not say")) 
    }
  }  
  entry
}

parse_generic_entry <- function(q_entry, label){
  if(label %in% c("session", "results")){
    return(NULL)
  }
  #messagef("Parsing entry of length %d for label '%s'", length(q_entry), label)
  dummy_entry <- dummy[[label]]
  #browser()
  if(is.null(dummy_entry)){
    browser()
  }
  if(is.null(q_entry)){
    return(dummy_entry)
  }
  names <- names(q_entry)
  if(length(names) == 0){
    return(dummy_entry)
  }
  sum_data <- names[!stringr::str_detect(names, "q[0-9]+")]
  sum_data <- sum_data[!stringr::str_detect(sum_data, "i[0-9]+")]
  ret <- q_entry[sum_data]
  names(ret) <- sprintf("%s.%s", label, names(ret) %>% stringr::str_to_lower() %>% stringr::str_replace_all(" ", "_"))
  ret <- post_process(ret, label)
  ret %>% tibble::as_tibble()
}

parse_battery_results <- function(data){
  map_dfr(data %>% as.list(), function(x){
    browser()
    if(!("participant_id" %in% names(x))){
      return(NULL)
    }
    if(!("data" %in% names(x))){
      return(NULL)
    }
    if(!("value" %in% names(x$data))){
      return(NULL)
    }
    meta <- tibble(p_id = x$participant_id, 
                   finished = x$finished, 
                   time_started = x$time_created, 
                   time_last_modified = x$time_last_modified)
    
    meta %>% bind_cols(map_dfc(names(x$data$value), ~{parse_generic_entry(x$data$value[[.x]], .x)})) 
  })  
}

parse_single_participant_results <- function(data){
  #browser()
  
  if(!("session" %in% names(data))){
    return(NULL)
  }
  meta <- tibble(p_id = data$session$p_id, 
                 finished = data$session$complete, 
                 time_started = data$session$time_started, 
                 time_last_modified = data$session$current_time, 
                 num_tests = length(setdiff(names(data), "session")))
  meta %>% bind_cols(map_dfc(setdiff(names(data), "session"), ~{parse_generic_entry(data[[.x]], .x)})) 
}