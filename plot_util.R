def_colour1 <- "#1f77b4" 
def_colour2 <- "#ff7f0e"
def_colour3 <- "green"
uv_alpha <- .7
bv_alpha <- .7
long_alpha <- .7
default_text_size <- 16

library(corrr)
#library(ppcor)
select <- dplyr::select
default_grid_color <- "gray64"
correlation_background_color <- "gray72"
get_display_name <-function(var_name){
  var_name
}

get_default_theme <- function(x_rotate = 0, keep_legend = F){
  t <- theme_minimal()
  t <- t + theme(strip.text = element_text(size = round(default_text_size*.75), hjust = 0))
  t <- t + theme(text = element_text(size = default_text_size))
  t <- t + theme(axis.title.x = element_text(size = default_text_size, vjust = -.5))
  t <- t + theme(plot.title = element_text(hjust=0))
  t <- t + theme(panel.spacing.x = unit(0.5, "cm"))
  t <- t + theme(panel.spacing.y = unit(0.5, "cm"))
  if (x_rotate != 0){
    t <- t + theme(axis.text.x = element_text(size=round(default_text_size *.85), angle=x_rotate, hjust=1))
  }
  else{
    t <- t + theme(axis.text.x = element_text(size=round(default_text_size *.85)))
    
  }
  
  return(t)
  t <- t + theme(panel.border=element_blank())
  t <- t + theme(panel.grid.major	= element_line(colour=default_grid_color, size=.3))
  t <- t + theme(panel.grid.minor	= element_blank())
  t <- t + theme(plot.title = element_text(hjust=0))
  t <- t + theme(panel.spacing.x = unit(0.5, "cm"))
  t <- t + theme(panel.spacing.y = unit(0.5, "cm"))
  #t <- t + theme(legend.title=element_text(size=default_text_size))
  t <- t + theme(legend.title = element_blank())
  #t <- t + theme(legend.title.align=1)
  t <- t + theme(legend.text = element_text(size=round(default_text_size*.75)))
  if(!keep_legend){
    t <- t + theme(legend.position = "none")
  }
  t <- t + theme(legend.key.size=unit(0.5, "cm"))
  t <- t + theme(legend.key.width=unit(.1, "cm"))
  t
}

is_valid <- function(x){
  if("character" %in% class(x)){
    return(length(x) > 0 && nchar(x) > 0 && sum(is.na(x)) == 0)
  }
  invalid <- is.null(x) || length(x) == 0 || sum(is.na(x)) > 0 || sum(nchar(x)) == 0
  !invalid
}

is_valid_char <- function(x){
  is_valid(x) && is.character(x)
}

get_optimal_ncol <- function(data, group_var){
  ncol <- 1
  if("group_var" %in% names(data)){
      ncol <- ifelse(n_distinct(data$group_var) > 2, 2, 1)
      gv_levels <- levels(factor(data$group_var))
      if (gv_levels == c("Low", "Mid", "High")){
        ncol <- 1
      }
  }
  ncol
}

add_group_var <- function(data, group_var, second = F, remove_na = F){
  #printf("Checking %s", group_var)
  if(is.null(data) || is.null(group_var) || is.null(second) || is.null(remove_na) || is.na(group_var)){
    #browser()
  }
  
  if(is_valid_char(group_var)){
    #messagef("Adding group_var for %s", group_var)
    if(second){
      data$group_var2 <- data[[group_var]]
      if(remove_na){
        data <- data %>% filter(!is.na(group_var))
      }
    }
    else{
      data$group_var <- data[[group_var]]
      if(remove_na){
        data <- data %>% filter(!is.na(group_var))
      }
    }
  } else{
    #browser()
    messagef("Invalid group_var: '%s'", group_var)
    
  }
  data
}

add_group_vars <- function(data, group_var, group_var2, remove_na = F, remove_na2 = T){
  data <- add_group_var(data, group_var, remove_na = remove_na)
  data <- add_group_var(data, group_var2, second = T, remove_na = remove_na2)
  data
}

add_facet_wrap <- function(plot_obj, data, scale = "free"){
  if("group_var" %in% names(data)){
    #message("Adding facet_wrap")
    ncol <- get_optimal_ncol(data)
    if(nrow(count(data, group_var)) == 0){
      return(plot_obj)
    }
    plot_obj <- plot_obj + facet_wrap(~group_var, ncol = ncol , scale =  scale)
  }
  plot_obj
  
}

add_facet_grid <- function(plot_obj, data, scale = "free"){
  if(sum(c("group_var", "group_var2") %in% names(data)) ==  2){
    #message("Adding facet_grid")
    if(nrow(count(data, group_var)) == 0 || nrow(count(data, group_var2)) == 0){
      return(plot_obj)
    }
    plot_obj <- plot_obj + facet_grid(group_var ~ group_var2, scale =  scale)
  }
  plot_obj
  
}

add_facet <-function(plot_obj, data, group_var, group_var2 = NULL, scale = "free"){
  if(is_valid_char(group_var2)){
    if(is_valid_char(group_var)){
      plot_obj <- add_facet_grid(plot_obj, data, scale)
    }
  }
  else{
    if(is_valid_char(group_var)){
      plot_obj <- add_facet_wrap(plot_obj, data, scale)
    }
  }
  plot_obj
}

univariate_plot_categorial <- function(data, var, group_var = NULL, 
                                       group_var2 = NULL, scale = "fixed", remove_na = F, remove_na2 = F,
                                       coord_flip = FALSE){
  data <- data %>% mutate_at(vars(var), funs(factor))
  data <- add_group_vars(data, group_var, group_var2, remove_na = remove_na, remove_na2 = remove_na2)
  
  q <- ggplot(data, aes_string(x = var, y = "..count..")) 
  q <- q + geom_bar(fill = def_colour1, alpha = uv_alpha, colour = "black")
  q <- add_facet(q, data, group_var, group_var2, scale = scale)
  q <- q + labs(x = get_display_name(var))  
  x_text <- paste0(levels(data[[var]]), collapse = "")
  len_x_text <- nchar(x_text)

  if(len_x_text > 100){
    q <- q + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  }
  if(coord_flip){
    q <- q + coord_flip()
  }
  q
}

univariate_plot_numeric <- function(data, var, 
                                    group_var = NULL, group_var2 = NULL, scale = "fixed", 
                                    remove_na = F, remove_na2 = F){
  if(!is_valid_char(group_var)){
    group_var <- "--"
  }
  data <- add_group_vars(data, group_var, group_var2, remove_na = remove_na, remove_na2 = remove_na2)
  
  q <- ggplot(data, aes_string(x = var, y = "..count..")) 
  q <- q + geom_histogram(fill = def_colour1 , alpha = uv_alpha, colour = "black")
  q <- add_facet(q, data, group_var, group_var2, scale)
  q <- q + labs(x = get_display_name(var))
  if(!is.null(group_var) && group_var != "--"){
    sum_data <- data %>% 
      group_by(group_var) %>% 
      summarise(m = mean(!!sym(var), na.rm = T)) %>% ungroup()
  }
  else {
    sum_data <- data %>% summarise(m = mean(!!sym(var), na.rm = T)) %>% ungroup()
    
  }
  q <- q + geom_vline(data = sum_data, 
                      aes(xintercept = m), 
                      size = 1.2, linetype = "solid", colour = "indianred")
  q
}


bivariate_plot_categorial_numeric <- function(data, var_x, var_y, 
                                              group_var = NULL, group_var2 = NULL, scale = "fixed", 
                                              remove_na = F, remove_na2 = F){
  #browser()
  data <- data[!is.na(data[, var_x]),]
  data <- data %>% mutate_at(vars(var_x), funs(factor))
  data <- add_group_vars(data, group_var, group_var2, remove_na = remove_na, remove_na2 = remove_na2)
  
  q <- ggplot(data, aes_string(x = var_x, y = var_y)) 
  q <- q + geom_boxplot(fill = def_colour1)
  q <- q + geom_point(alpha = bv_alpha)
  q <- add_facet(q, data, group_var, group_var2, scale)
  q <- q + labs(x = get_display_name(var_x), y = get_display_name(var_y))  
  
  q
}

bivariate_plot_numeric_numeric <- function(data, var_x, var_y, add_regression = T, 
                                           group_var = NULL, group_var2 = NULL, scale = "fixed", 
                                           remove_na = F, remove_na2 = F){
  #if(!is.null(group_var2)){
  #  browser()
  #}
  #browser()
  data <- add_group_vars(data, group_var, group_var2, remove_na = remove_na, remove_na2 = remove_na2)
  q <- ggplot(data, aes_string(x = var_x, y = var_y)) 
  q <- q + geom_point(alpha = bv_alpha, colour = def_colour1)
  if(add_regression){
    q <- q + geom_smooth(method = "lm", colour = def_colour2)
    q <- q + geom_smooth(method = "gam", formula= y ~ s(x, bs="tp"), colour = def_colour3)
  }
  q <- add_facet(q, data, group_var, group_var2, scale)
  q <- q + labs(x = get_display_name(var_x), y = get_display_name(var_y))  
  q
}


bivariate_plot_auto <- function(data, input, var_data, group_var2 = NULL, remove_na = F, remove_na2 = F){
  #browser()
  vars <- get_parameters(data, input, keep_pseudo_na = T, var_data = var_data)
  var_x <- vars$vars["x"]
  var_y <- vars$vars["y"]
  group_var <-   vars$vars["grouping"]
  scale <- vars$scale
  if(vars$sub_type == "cat-cat"){
    p <- bivariate_plot_categorial_categorial(data, var_x, var_y)
  }
  if(vars$sub_type == "cat-num"){
    #messagef("BVplot Cat - num")
    p <- bivariate_plot_categorial_numeric(data, var_x, var_y, 
                                           group_var = group_var, group_var2 = group_var2, scale = scale,
                                           remove_na = remove_na, remove_na2 = remove_na2)
  }
  if(vars$sub_type == "num-cat"){
    #messagef("BVplot Num - CAT")
    p <- bivariate_plot_categorial_numeric(data, var_y, var_x, 
                                           group_var = group_var, group_var2 = group_var2, scale = scale, 
                                           remove_na = remove_na, remove_na2 = remove_na2)
  }
  if(vars$sub_type == "num-num"){
    #messagef("BVplot Num - num")
    p <- bivariate_plot_numeric_numeric(data, var_x, var_y, 
                                        group_var = group_var, group_var2 = group_var2, scale = scale, 
                                        remove_na = remove_na, remove_na2 = remove_na2)
  }
  
  p
}

bivariate_plot_categorial_categorial <- function(data, var_x, var_y, na_rm_x = T, na_rm_y = T){
  var_x <- sym(var_x)
  var_y <- sym(var_y)
  if(na_rm_x){
    data <- data %>% filter(!is.na(!!var_x))
  }
  if(na_rm_y){
    data <- data %>% filter(!is.na(!!var_y))
  }
  sum_data <- data %>% count(!!var_x, !!var_y)
  sum_data  <- sum_data %>% group_by(!!var_y) %>% mutate(rel_freq = n/sum(n), facet_var = !!var_y) %>%  ungroup()

  q <- ggplot(sum_data, aes_(x = var_x, y = sym("rel_freq"))) 
  q <- q + geom_col(fill = def_colour1)
  q <- q + facet_wrap(~facet_var)
  q <- q + labs(x = get_display_name(as.character(var_x)), y = get_display_name(as.character(var_y)))  
  q
}

filter_non_numeric <- function(data, vars){
  if(is.null(vars) || length(vars) == 0){
    return(character(0))
  }
  ret <- map_chr(vars, function(v){
    if(is.numeric(data[[v]])){
      v
    }
    else{
      ""
    }
  })  
  ret[nchar(ret) > 0]
}

plot_cor_network <- function(data, cor_vars, 
                             grouping_var = NULL, 
                             min_cor = .1, 
                             partial = F, 
                             output_type = c("matrix", "network", "text", "raw"), 
                             legend  = T,
                             text_size = 12,
                             aggregate = T){
  cor_vars <- intersect(names(data), cor_vars)
  cor_vars <- filter_non_numeric(data, cor_vars)
  if(length(cor_vars) == 0){
    messagef("plot_cor_network: No vars left")
    return(NULL)
  }
  output_type <- match.arg(output_type)
  
  data <- data %>% dplyr::select(p_id, cor_vars)
  if(aggregate){
    data <- data %>% 
      group_by(p_id) %>% 
      summarise_at(cor_vars, mean) %>% 
      ungroup()
  }
  data <- data %>% dplyr::select(-p_id)
  if(partial){
    require(ppcor)
    tmp <- data %>% as.matrix() %>% na.omit()
    if(nrow(tmp) < 2){
      messagef("plot_cor_network: No data left")
      return(NULL)
    }
    res_cor <- pcor(tmp)$estimate %>% as.data.frame() %>% rownames_to_column() %>% as_tibble()
    map(1:nrow(res_cor), function(i) res_cor[i, i+1] <<- NA)
  }
  else{
    res_cor <- corrr::correlate(data)
  }
  #browser()
  if(output_type == "network"){
    q <- tryCatch({
      res_cor %>% network_plot(min_cor = min_cor,  colours = c("indianred4", "white", "skyblue3"), legend = legend)
      }, 
      error = function(e){
        message("Error during network_plot")
        return(NULL)
      })
    if(!is.null(q)){
      q <- q + theme(text = element_text(size = text_size),
                     panel.background = element_rect(fill = correlation_background_color))

    } 
    q
  }
  else if(output_type == "matrix"){
    q <- tryCatch({
      res_cor %>% rplot(print_cor = T,  colours = c("indianred4", "white", "skyblue3"), legend = legend) 
      },
      error = function(e){
        message("Error during rplot")
        return(NULL)
      })
    if(!is.null(q)){
      q <- q + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
      q <- q + theme(text = element_text(size = text_size), panel.background = element_rect(fill = correlation_background_color)) 
    }
    q
  }
  else if(output_type == "text"){
    #browser()
    res_cor %>% fashion() %>% as_tibble() %>% rename(Variable = rowname)
  }
  else if(output_type == "raw"){
    #browser()
    res_cor 
  }
}

plot_cor_networks_with_grouping <- function(data, 
                                            cor_vars, 
                                            grouping_var = NULL, 
                                            min_cor = .1, 
                                            partial = F, 
                                            output_type = c("matrix", "network", "text", "raw"),
                                            min_group_size = 30,
                                            aggregate = T,
                                            ncol = NULL){
  #browser()
  cor_vars <- filter_non_numeric(data, cor_vars)
  if(length(cor_vars) == 0){
    return(NULL)
  }
  if(is.null(grouping_var) || grouping_var == "--" || !(grouping_var %in% names(data))){
    return(plot_cor_network(data, 
                            cor_vars, 
                            min_cor = min_cor, 
                            partial = partial, 
                            output_type = output_type,
                            aggregate = aggregate,
                            legend = T))    
  }
  output_type <- match.arg(output_type)
  #browser()
  data <- data %>% filter(!is.na(!!sym(grouping_var)))
  if(aggregate){
    #browser()
    data <- data %>% group_by(!!sym(grouping_var), p_id) %>% summarise_at(cor_vars, mean, na.rm = T) %>% ungroup()
  }
  group_counts <- data %>% count(!!sym(grouping_var)) 
  if(!is.null(min_group_size)){
    #browser()
    groups <- group_counts %>% 
      filter(n >= min_group_size) %>% 
      filter(!is.na(!!sym(grouping_var))) %>% 
      pull(!!sym(grouping_var)) %>% 
      unique()
  }
  else {
    groups <- unique(data %>% pull(grouping_var)) 
  }
  #browser()
  if(length(groups) == 0){
    return(NULL)
  }
  plots <- map(groups, function(g){
    group_n <- group_counts %>% filter(!!sym(grouping_var) == g) %>% pull(n) 
    title <- sprintf("%s (N = %s)", g, group_n[1])
    q <- plot_cor_network(data %>% filter(!!sym(grouping_var) == g), 
                     cor_vars, 
                     min_cor = min_cor, 
                     partial = partial, 
                     legend = F,
                     output_type = output_type) 
    if(is.null(q)){
      return(q)
    }
    if(output_type %in% c("network", "matrix")){
      q <- q + labs(title = title) + 
        theme(text = element_text(size = 12),  
              panel.border = element_rect(colour = "gray64", fill = NA, size = .25),
              panel.spacing = unit(5, "pt"),
              panel.background = element_rect(fill = correlation_background_color),
              plot.margin = margin(5, 5, 5, 5))
    }
    else{
      q <- q %>% mutate(Group = g) %>% select(Group, everything())
    }
    q
  })
  if(output_type %in% c("network", "matrix")){
    cowplot::plot_grid(plotlist = plots, ncol = ncol)
  }
  else if(output_type == "text"){
    suppressWarnings(bind_rows(plots))
  }
  else{
    suppressWarnings(plots)
  }
}
plot_timeline <- function(data, var_t, var_y, add_regression = F, scale = "fixed", min_time_points = 2, current_page = 1, ncol = 3, nrow = 3){
  if(!(var_t %in% c("age", "age.months", "test_year", "age_group", "year_group"))){
    stop(sprintf("Invalid time variable: %s", var_t))
  }
  var_t <- sym(var_t)
  var_y <- sym(var_y)
  data <- data %>%  filter(!is.na(!!var_t))
  data <- filter_by_measurements(data, as.character(var_y), min_time_points)
  if(as.character(var_t) == "age.months"){
    data <- data %>% mutate(age.months = floor(as.numeric(age.months))/12 + (as.numeric(age.months) %% 12)/10)
  }
  if(nrow(data) == 0) {
    messagef("Not enough measurements for %s", as.character(var_y))
      return(NULL)
  }
  q <- ggplot(data, aes(x = !!var_t, y = !!var_y)) 
  q <- q + geom_point(alpha = long_alpha, colour = def_colour1)
  if(add_regression){
    q <- q + geom_smooth(method = "lm", colour = def_colour2)
  }
  else{
    q <- q + geom_line(colour = def_colour2, aes(group = p_id))
  }
  if(as.character(var_t) == "age"){
    q <- q + scale_x_continuous(breaks = seq(8, 18, 2))
  }
  else if(as.character(var_t) == "age.months"){
    q <- q + scale_x_continuous(breaks = seq(8, 18, 2))
  }
  n_pages <- as.integer(ceiling(n_distinct(data$p_id)/(ncol * nrow)))
  current_page <- min(n_pages, current_page)
  q <- q + ggforce::facet_wrap_paginate(~p_id, ncol = ncol, nrow = nrow, page = current_page)
  q <- q + labs(x = get_display_name(as.character(var_t)), 
                y = get_display_name(as.character(var_y)),
                subtitle = sprintf("Page #%d/%d", current_page, n_pages))  
  q <- q + theme(legend.position = "none")
  q
}

get_num_timeline_pages <- function(data, var_t, var_y,  min_time_points = 2, ncol = 3, nrow = 3){
  var_t <- sym(var_t)
  data <- data %>%  filter(!is.na(!!var_t))
  data <- filter_by_measurements(data, var_y, min_time_points)
  if(nrow(data) == 0) {
    messagef("Not enough measurements for %s", var_y)
    return(0)
  }
  #browser()
  pages <- floor(n_distinct(data$p_id)/(ncol * nrow))
  return(pages)
}

plot_timeline_features <- function(data, var_name, var_time,  min_measurements = 3, scale = F){
  tmp <- get_timeline_features(data, var_name, var_time, min_measurements, scale = T)  
  if(is.null(tmp))
    return(NULL)
  names(tmp) <- c("p_id", "Total Change (z-score)", "Mean Change (z-score)", "Mean Abs. Difference (z)", "Correlation coefficient")
  
  tmp <- tmp %>% 
    gather(key = key, value = value, -p_id) 

  q <- tmp %>% ggplot(aes(x = value, y = ..count..)) 
  q <- q + geom_histogram(fill=def_colour1, color="black") 
  q <- q + facet_wrap(~key, scale = "free") 
  q <- q + labs(x = "Change Features") 
  q <- q + get_default_theme()
  q
}
get_lm_coefs_for_timeline <- function(data, var_y, var_t, var_grouping){
  lm_f <- as.formula(sprintf("%s ~ %s", var_y, var_t))
  if(is.factor(data %>%  pull(var_grouping))){
    groups <- levels(data %>%  pull(var_grouping))
  }
  else{
    groups <- unique(data %>% filter(!is.na(!!sym(var_grouping))) %>% pull(var_grouping))
  }
  if(!is.numeric(data %>% pull(var_y))){
    return(
      tibble(group = groups, y0 = NA, beta = NA)
      
    )  
  }
  
  map_dfr(groups, function(g){
    #browser()
    min_t <- min(data %>% pull(var_t), na.rm = T) 
    data <- as.data.frame(data)
    data[, var_t] <- data[, var_t] - min_t
    coefs <- lm(lm_f, data  = data %>% filter(!!sym(var_grouping) == g)) %>% coef()
    tibble(group = g, y0 = coefs[1], beta = coefs[2])
  })
}

plot_combined_timelines <- function(data, 
                                    var_y, 
                                    var_t, 
                                    var_grouping = "--", 
                                    min_time_points = 2, 
                                    min_n = NULL, 
                                    scale = F, 
                                    common_t = F){
  if(!(var_t %in% c("age", "age.months", "test_year", "age_group", "year_group"))){
    messagef("Invalid time variable: %s", var_t)
    return(NULL)
  }
  if(!is.numeric(data %>% pull(var_y))){
    messagef("Invalid dependent variable: %s", var_y)
    return(NULL)
  }
  
  #browser()
  var_t <- sym(var_t)
  var_y <- sym(var_y)
  data <- data %>%  filter(!is.na(!!var_t))
  data <- filter_by_measurements(data, as.character(var_y), min_time_points)
  has_grouping <- !is.null(var_grouping) && (var_grouping != "--")
  #printf("has_grouping: %s, var_grouoing = %s",has_grouping, var_grouping)
  
  if(has_grouping){
    if(var_t == var_grouping){
      return()
    }
    data <- data %>% filter(!is.na(!!sym(var_grouping)))
  }
  if(common_t){
    if(has_grouping){
      tmp <- data %>% 
        group_by(!!sym(var_grouping)) 
    }
    else{
      tmp <- data
    }
    tmp <- tmp %>% 
      summarise(min_t = min(!!var_t), max_t = max(!!var_t)) %>% 
      ungroup() %>% 
      summarise(min_t = max(min_t), max_t = min(max_t))
    messagef("Min %s, max %s", tmp$min_t[1], tmp$max_t[1])
    data <- data %>% filter(!!var_t >= tmp$min_t[1], !!var_t <= tmp$max_t[1])    
  }
  if(!is.null(min_n) && min_n > 0){
    #browser()
    if(has_grouping){
      bad_combination <- data %>% 
        count(!!var_t, !!sym(var_grouping)) %>% 
        filter(n < min_n ) %>% 
        mutate(bad_comb = sprintf("%s-%s", !!var_t, !!sym(var_grouping))) %>% 
        pull(bad_comb)
      
      data <- data %>% 
        mutate(bad_comb = sprintf("%s-%s", !!var_t, !!sym(var_grouping))) %>% 
        filter(!(bad_comb %in% bad_combination))    
      data$bad_id <- NULL
    }
    else{
      bad_timepoints <- data %>% count(!!var_t) %>% filter(n < min_n ) %>% pull(!!var_t)
      data <- data %>% filter(!(!!var_t %in% bad_timepoints))    
      
    }
  }
  if(nrow(data) == 0) {
    messagef("Not enough measurements for %s", as.character(var_y))
    return(NULL)
  }
  if(as.character(var_t) == "age.months"){
    data <- data %>% mutate(age.months = floor(as.numeric(age.months))/12 + (as.numeric(age.months) %% 12)/10)
  }
  if(as.character(var_t) == "age_group"){
    age_group_labels <- sort(unique(data$age_group))
    data <- data %>% mutate(age_group = as.integer(factor(age_group)))
  }
  
  if(has_grouping){
    mean_data <- data %>% group_by(!!sym(var_grouping), !!var_t) %>% summarise(m = mean(!!var_y, na.rm = T)) 
    #browser()
    coefs <- get_lm_coefs_for_timeline(data, as.character(var_y), as.character(var_t), var_grouping) %>% 
      mutate(label = sprintf("%s (t = %.2f, b = %.2f)", group, y0, beta))
    labels <- coefs %>% pull(label)
    names(labels) <- coefs %>% pull(group)
    sort_levels <- coefs %>% pull(group) 
    data <- as.data.frame(data)
    data[, var_grouping] = factor(data[, var_grouping], labels = labels, levels = sort_levels)
    data <- as_tibble(data)
    mean_data <- as.data.frame(mean_data)
    mean_data[, var_grouping] = factor(mean_data[, var_grouping], labels = labels, levels = sort_levels)
    mean_data <- as_tibble(mean_data)
  }
  else{
    mean_data <- data %>% group_by(!!var_t) %>% summarise(m = mean(!!var_y, na.rm = T)) 
  }
  #browser()
  
  q <- ggplot(data, aes(x = !!var_t, y = !!var_y)) 
  #q <- q + geom_line(colour = def_colour1, aes(group = p_id))
  q <- q + geom_path(aes(group = p_id),alpha = .2)
  #q <- q + geom_point(aes(colour = p_id))
  q <- q + geom_smooth(method = "lm", colour = "indianred3")
  q <- q + geom_line(data = mean_data, aes(x = !!var_t, y = m), size = 2)
  
  if(as.character(var_t) == "age"){
    q <- q + scale_x_continuous(breaks = seq(8, 18, 2))
  }
  else if(as.character(var_t) == "age.months"){
    q <- q + scale_x_continuous(breaks = seq(8, 18, 2))
  }
  
  if(has_grouping){
    q <- q + facet_wrap(as.formula(paste("~", var_grouping)), 
                        scale = "fixed")
    q <- q + theme(strip.text.x = element_text(size = 12))
  }
  
  if(var_t == "age_group"){
    q <- q + scale_x_continuous(breaks = 1:length(age_group_labels), 
                                labels = age_group_labels)
    q <- q + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  q <- q + labs(x = get_display_name(as.character(var_t)), y = get_display_name(as.character(var_y)))
  q <- q + theme(legend.position = "none")
  q
}