library(tidyverse)
source("generic_parse.R")

messagef <- function(...) message(sprintf(...))
printf <- function(...) print(sprintf(...))

num_predictors <- c("GMS.general", "HPT.ability",  "age")
cat_predictors <- c("gender")


get_parameters <- function(data, input, keep_pseudo_na = T, var_data){
  vars <- c("x" = input$bv_variable1, "y" = input$bv_variable2)
  var_info1 <- var_data %>% filter(variable == input$bv_variable1)
  var_info2 <- var_data %>% filter(variable == input$bv_variable2)
  sub_type <- sprintf("%s-%s", substr(var_info1$type, 1, 3), substr(var_info2$type, 1, 3))  
  list(vars = vars, sub_type = sub_type)
}


split_multi_entry <- function(entry){
  if(length(entry) == 1){
    ret <- str_replace_all(entry, "',", "@") %>% 
      str_replace_all("'", "") %>% 
      str_split("@") %>% 
      unlist()        
  }    
  else{
    ret <- NULL
  }
  ret
}

unify_data_vec <- function(x){
  #browser()
  ret <- unique(x[!is.na(x)])  
  if(length(ret) == 0){
    ret <- NA
  }
  if(length(ret) > 1){
    messagef("Found confliciting data in row: [%s]", paste(ret, collapse = ", "))
    ret <- ret[1]
  }
  ret
}

join_two_part_data_rows <- function(rows){
  #browser()
  if(nrow(rows) < 2){
    return(rows)
  }
  
  if(nrow(rows) > 2){
    return(rows %>% join_restart_rows() %>% join_two_part_data())
  }
  if(n_distinct(rows$p_id) != 1){
    messagef("Row wiht more than one ID given: [%s]", paste(rows$p_id, collapse = ", "))
    rows <- rows[rows$p_id == rows$p_id[1],]
  }
  #messagef("Joining rows for %s", rows$p_id[1])
  test_vars <- names(rows)[names(rows) %>% str_detect("[.]")]
  base_data <- 
  tibble(p_id = rows$p_id[1], 
         finished = rows$finished[1] & rows$finished[2], 
         time_started = min(rows$time_started),
         time_last_modified = min(rows$time_last_modified),
         num_tests = sum(rows$num_tests)
  ) 
  #browser()
  base_data %>% bind_cols(
    rows %>% select(all_of(test_vars)) %>% mutate_all(unify_data_vec)
  ) 
}

join_two_part_data <- function(data){
  #browser()
  ids <- data %>% count(p_id) %>% filter(n > 1) %>% pull(p_id)
  ret <- data %>% filter(!(p_id %in% ids))
  map_dfr(ids, function(i){
    data %>% filter(p_id == i) %>% join_two_part_data_rows()
  }) %>% 
    bind_rows(ret)
  
}

join_restart_rows <- function(data){
  if(is.null(data[["p_id"]])){
    return(data)
  }
  ids <- data %>% count(p_id) %>% filter(n > 1) %>% pull(p_id)
  ret <- data %>% filter(!(p_id %in% ids))
  fixed_rows <- 
    map_dfr(ids, function(i){
      tmp <- data %>% filter(p_id == i)
      finished <- which(tmp$finished == TRUE)
      if(length(finished) == 0){
        tmp  <- tmp[nrow(tmp),]   
      }
      else{
        tmp <- tmp[max(finished), ]  
      }
      tmp
    })
  ret %>% bind_rows(fixed_rows) 
}

read_adaptive_raw_data <- function(result_dir = "data/from_server", test_ids){
  if(missing(test_ids)){
    stop("[read_adaptive_raw_data] At least one test id must be provided for ")
  }
  messagef("Setting up adaptive raw data from [%s]", paste(result_dir, collapse = ", "))
  
  results <- purrr::map(list.files(result_dir, pattern = "*.rds", full.names = T), ~{readRDS(.x) %>% as.list()})
  #browser()
  purrr::map_dfr(results, function(x){
    parse_single_participant_results(x, raw_data = T, test_ids = test_ids)
  })
}

read_data <- function(result_dir = "data/from_server"){
  #browser()
  messagef("Setting up data from [%s]", paste(result_dir, collapse = ", "))
  
  results <- purrr::map(list.files(result_dir, pattern = "*.rds", full.names = T), ~{readRDS(.x) %>% as.list()})
  purrr::map_dfr(results, function(x){
    parse_single_participant_results(x)
  })
}

format_difftime <- function(dtime, places = 2){
  hms::as_hms(round(dtime, places)) %>% as.character()
}

apply_session_filter <- function(data, id_filter, complete_filter){
  data <- data %>% apply_id_filter(id_filter) 
  data <- data %>% apply_complete_filter(complete_filter)
  data
}

apply_complete_filter <- function(data, complete_filter){
  if(complete_filter == "All"){
    return(data)
  }
  if(complete_filter == "Finished"){
    return(data %>% filter(is_finished))
  }
  if(complete_filter == "Unfinished"){
    return(data %>% filter(!is_finished))
  }
  data
}  

apply_id_filter <- function(data, id_filter, complete_filter){
  if(id_filter == "All IDs"){
    return(data)
  }
  
  if(id_filter == "LongGold IDs without Debug IDs"){
    return(data %>% filter(is_longgold_id & !is_debug_id))
  }
  if(id_filter == "LongGold IDs"){
    return(data %>% filter(is_longgold_id))
  }
  data %>% filter(!is_longgold_id)
}

read_sessions <- function(session_dir = "../../test_batteries/output/sessions/"){
  dirs <- map(session_dir, ~{list.files(.x, full.names = T)}) %>% unlist() %>% unique()
  #browser()
  map_dfr(dirs, function(x){
    #browser()
    data_f <- readRDS(file.path(x, "data.RDS"))
    time_stamp <- readRDS(file.path(x, "timestamp.RDS"))
    tibble(p_id = data_f$passive$p_id,
           session_name = x, 
           is_debug_id = is_debug_id(data_f$passive$p_id),
           is_longgold_id = (nchar(p_id) %in% c(10, 11, 14, 15)) | substr(p_id, 2, 7) == "000000",
           time_started = data_f$passive$time_started,
           time_last_modified = time_stamp,
           test_run_time = format_difftime(time_last_modified - time_started, 2),
           #closed = data_f$passive$closed,
           num_restarts = data_f$passive$num_restarts,
           language = data_f$passive$language,
           num_tests = data_f$passive$results %>% as.list() %>% length(),
           tests_finished = paste( data_f$passive$results %>% names(), collapse = ";"))
  }) %>% mutate(current_time = lubridate::now()) %>% arrange(desc(time_started))
  
} 

fix_bad_tpt_data <- function(data){
  data <- data %>% 
    select(
      -contains("abs_env"), 
      -contains("raw_env"), 
      -contains("env_order"), 
      -contains("bin_env"), 
      -contains("abs_flux"), 
      -contains("raw_flux"), 
      -contains("bin_flux"), 
      -contains("flux_order"), 
      -contains("abs_cent"), 
      -contains("raw_cent"), 
      -contains("bin_cent"), 
      -contains("cent_order"), 
    ) 
  TPT.env_score <- rowSums(data %>% select(contains("env_score")), na.rm = T)
  TPT.flux_score <- rowSums(data %>% select(contains("flux_score")), na.rm = T)
  TPT.cent_score <- rowSums(data %>% select(contains("cent_score")), na.rm = T)
  TPT.general_score <- rowSums(data %>% select(contains("general_score")), na.rm = T)

  data <- data %>% 
    select(-contains("env_score")) %>% 
    select(-contains("cent_score")) %>% 
    select(-contains("flux_score")) %>% 
    select(-contains("general_score"))
    
  data$TPT.env_score <- TPT.env_score
  data$TPT.flux_score <- TPT.flux_score
  data$TPT.cent_score <- TPT.cent_score
  data$TPT.general_score <- TPT.general_score
  data  
}

set_school_from_p_id <- function(data, school_defs){
  if(!("p_id") %in% names(data)){
    return(data)
  }
  school <- substr(p_id, 3, 4)  
  country <- substr(p_id, 1, 2)   
  valid <- nchar(str_extract(country, "[0-9]+")) != 2 
}

setup_workspace <- function(results = "data/from_server", filter_debug = T){
  print("setup workspace called")
  browser()
  
  school_def <- 
    readxl::read_excel("data/school_def.xlsx", sheet = "school_info") %>% 
    select(school = acronym, school_id, country) %>% 
    mutate(country_code = c("UK" = "00", "DE" = "01", "IT" = "02", "LV" = "03")[country], 
           combined = sprintf("%s%s", country_code, school_id))
  
  school_map <- school_def$school
  names(school_map) <- school_def$combined
  
  master <- read_data(results) 
  master <- master %>% set_names(str_replace(names(master), "MSA_results", "MSA")) 
  
  if(filter_debug)master <- master %>% filter(!is_debug_id(p_id))
  master <- master %>% join_two_part_data()
  master <- master %>% select(-ends_with("num_items"))
  
  if(any(str_detect(names(master), "raw_flux"))) {
    master <- fix_bad_tpt_data(master)
  }
  if(!("school" %in% names(master))){
    master <- master %>% mutate(school = school_map[substr(p_id, 1, 4)])
    #master[is.na(master$school),]$school <- substr(master[is.na(master$school),]$p_id, 1, 4)
  }
  messagef("Filter for debug ids is '%s'", filter_debug)
  assign("master", master, globalenv())
}

get_correlations <- function(data, var_x, var_y, method = "pearson"){
  f <- as.formula(sprintf("~ %s + %s", var_x, var_y))
  ct <- cor.test(f, data = data, method = method)
  return(ct %>% broom::tidy())     
}

get_pc_graph <- function(master, 
                         alpha = .05,   
                         charge = -120,
                         linkDistance = 100,
                         fontSize = 16,
                         opacityNoHover = .75){
  #require(Rgraphviz)
  
  red <- master
  cor_data <- cor(red, use = "pairwise.complete.obs")
  #psych::pairs.panels(red)
  suffStat <- list(C = cor_data, n = nrow(red))
  pc.fit <- pcalg::pc(suffStat, indepTest = pcalg::gaussCItest, p = ncol(red), alpha = alpha)
  labels <- names(red)
  names(labels) <- pc.fit@graph@nodes
  nAttrs <- list()
  nAttrs$label <- labels
  attrs <- list(node = list(shape = "ellipse", fixedsize = FALSE))
  
  adjm <- wgtMatrix(getGraph(pc.fit), transpose = FALSE)
  #ig_network <- graph_from_adjacency_matrix(adjm, mode = "directed", weighted = T)
  ig_network <- graph_from_graphnel(getGraph(pc.fit), name = TRUE, weight = TRUE, unlist.attrs = TRUE)
  d3_network <- igraph_to_networkD3(ig_network)
  d3_network$nodes$group <- 1
  d3_network$nodes$name <- labels
  for(i in 1:nrow(d3_network$links)){
    d3_network$links[i,]$value <-  50*abs(cor_data[d3_network$links[i,]$source + 1,  d3_network$links[i,]$target + 1])
  }
  sn <- forceNetwork(
    Links = d3_network$links, 
    Nodes = d3_network$nodes, 
    Source = 'source', 
    Target = 'target', 
    Value = "value",
    NodeID = 'name', 
    Group = "group", 
    fontSize = fontSize,
    opacityNoHover = opacityNoHover,
    bounded = F,
    zoom = T,
    charge = charge,
    linkDistance = linkDistance,
    arrows = TRUE,
    #Nodesize = "size",
    colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")
  )
  #q <- pcalg::iplotPC(pc.fit, labels = labels)
  #plot(pc.fit@graph, main = "", nodeAttrs = nAttrs, attrs = attrs)
  sn
}
beta_plot <- function(lm_model, order_by_size = F){
  if("lm" %in% class(lm_model)){
    lm_model <- lm_model %>% 
      broom::tidy()
  }
  lm_model <- lm_model %>% 
    filter(term != "(Intercept)") %>% 
    mutate(y_min = estimate  - 1.96*std.error, y_max = estimate  + 1.96*std.error, 
           sig = y_min > 0 | y_max < 0)
  
  if(order_by_size) 
    lm_model <- lm_model %>% mutate(term = factor(term) %>% fct_reorder(estimate, mean))
  if("N" %in%  names(lm_model)){
    q <- lm_model %>% 
      mutate(N_fact = factor(N)) %>% 
      ggplot(aes(x = term, y = estimate, colour = sig, group = N_fact)) 
    q <- q + geom_point(shape = 2, size = 2, position = position_dodge(width = 1)) 
    q <- q + geom_linerange(aes(ymin = y_min, ymax = y_max, colour = sig, group = N_fact), position = position_dodge(width = 1))
    q <- q + geom_text(aes(y = 2, x = 10 * (N - min(N))/max(N) + 2, label = sprintf("N = %s", N)), 
                       size = 3, colour ="black")                            
    q <- q + ylim(-1, 1)
  }
  else{
    q <- lm_model %>% ggplot(aes(x = term, y = estimate )) 
    q <- q + geom_point(shape = 2, size = 2, color = def_colour1) 
    q <- q + geom_linerange(aes(ymin = y_min, ymax = y_max))
  }
  q <- q + coord_flip()  
  q <- q + geom_hline(yintercept = 0, linetype = "dashed")
  q <- q + theme(legend.position = "none")
  q
}

get_model <- function(data, dv = "MDT.ability", predictors = num_predictors, output_format = "raw", ...){
  output_format <- match.arg(output_format, c("raw","summary", "glance", "tidy", "sj", "jtools_tab", "jtools_plot"))
  predictors <- setdiff(predictors, dv)
  data <- data %>% select(all_of(c(dv, predictors))) %>%  mutate_if(is.numeric, scale)
  f <- sprintf("%s ~ .", dv) %>% as.formula()                      
  lm_tab <- lm(f, data = data)
  if(output_format == "summary"){
    lm_tab <- lm_tab %>% 
      summary()
  }
  if(output_format == "tidy"){
    lm_tab <- lm_tab %>% 
      broom::tidy()
  }
  if(output_format == "glance"){
    lm_tab <- lm_tab %>% 
      broom::glance()
  }
  if(output_format == "sj"){
    lm_tab <- lm_tab %>% 
      sjPlot::tab_model(...)
  }
  if(output_format == "jtools_tab"){
    lm_tab <- lm_tab %>% 
      jtools::summ(...)
  }
  if(output_format == "jtools_plot"){
    lm_tab <- lm_tab %>% 
      jtools::plot_summs(scale = T,  ...)
  }
  lm_tab
}
