#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinythemes)
library(DT)
library(sjPlot)
# library(pcalg)
# library(networkD3)
# library(igraph)

source("analysis.R")
source("plot_util.R")
on_server <- grepl("shiny-server", getwd())

if(on_server){
    #result_dir <- "../de_wave_7_2021/output/results"
  filter_debug <- T
  result_dir  <- readRDS("result_dir.rds")
  session_dir <- str_replace(result_dir, "results", "sessions")
} else{
  result_dir <- "data/from_server"
  filter_debug <- T
  #result_dir <- sprintf("data/it_wave/from_server/part%d", 1:2)
  session_dir <- "../../test_batteries/output/sessions/"
  #result_dir <- "data/it_wave/from_server"
}

setup_workspace(result_dir, filter_debug)

var_choices <- setdiff(names(master), c("p_id",
                                       "time_started", 
                                       "time_last_modified", 
                                       "pilot", 
                                       "complete", 
                                       "num_restarts", 
                                       "DEG.first_language",
                                       "DEG.second_language",
                                       "DEG.gender", 
                                       "DEG.age", 
                                       "RAT.num_items"))
school_choices <- c("--", unique(master$school) %>% na.omit())

var_types <- c("categorial", "numeric")[1 + map_lgl(var_choices, ~{(master[[.x]] %>% class())[1] == "numeric"})]
var_data <- tibble(variable = var_choices, type = var_types)

theme_set(get_default_theme())
format_result_dir <- function(fname = result_dir){
  fname %>%
    str_replace_all("output/results", "") %>% 
    str_replace_all("[.]+", "") %>% 
    str_replace_all("^[/]+", "")%>% 
    str_replace_all("[/]+$", "") %>%  
    paste(collapse = ", ")
}

get_intro_text <- function(){
  div(h3("Welcome to the LongGold Battery Monitor App"), 
         p("This app allows you visualize and inspect the data from", shiny::tags$br(), 
           shiny::tags$b(sprintf("[%s]", format_result_dir(result_dir)))),
      p("Have fun!"),
      style = "width:50%;text-align:justify")
}

impressum <- function(){
    p(
        "LongGold  Battery  Monitor  v0.1", 
        shiny::tags$br(), 
        shiny::tags$br(), 
        "Author: Klaus Frieler", 
        shiny::tags$br(), 
        shiny::a(href = "https://www.aesthetics.mpg.de/en.html", 
                 "Max Planck Institute for Empirical Aesthetics, Frankfurt/M, Germany", 
                 target = "_blank"),
        shiny::tags$br(),
        shiny::tags$br(), 
        "Powered by",
        shiny::tags$br(),
        shiny::a(href = "http://www.music-psychology.de/",
                 "Deutsche Gesellschaft für Musikspsychologie", target = "_blank"),
        shiny::tags$br(), 
        shiny::tags$br(),
        shiny::a(href = "https://github.com/klausfrieler/longgold_monitor", "On Github", target = "_blank"), 
        style = "font-size: 10pt; display: block"
    )
    
}

input_width <- 300


ui_new <-   
    shiny::shinyUI(
        navbarPage(
            title = "LongGold Battery Monitor", 
            theme = shinytheme("spacelab"),
            id = "tabs",
            tabPanel(
                "Home",
                sidebarLayout(
                    sidebarPanel(
                        impressum(),
                        selectizeInput("ov_filter_school", "School:", school_choices, multiple = F), 
                        downloadButton("download_all_data_csv", "Download Data"),
                        checkboxInput("dec", label = "Use German Format", value = 0),
                        downloadButton("download_HPT", "Download HPT Data"),
                        width = 2
                    ),
                    
                    # Main panel for displaying outputs ----
                    mainPanel(
                        htmlOutput("introduction"),
                        h4("Summary"),
                        tableOutput("overall_stats")
                    )
                    
                )
            ),
            tabPanel(
                "Univariate",
                sidebarLayout(
                    sidebarPanel(
                        selectizeInput("uv_variable", "Variable:", var_choices, multiple = F), 
                        selectizeInput("uv_filter_school", "School:", school_choices, multiple = F), 
                        impressum(),
                        width = 2
                    ),
                    
                    # Main panel for displaying outputs ----
                    mainPanel(
                        plotOutput("univariate_plot", width = "800px")
                        )
                    
                )
            ),            
            tabPanel(
                "Bivariate",
                sidebarLayout(
                    sidebarPanel(
                        selectizeInput("bv_variable1", "Variable X:", var_choices, selected = "RAT.ability", multiple = F), 
                        selectizeInput("bv_variable2", "Variable y:", var_choices, selected = "MDT.ability", multiple = F), 
                        selectizeInput("bv_filter_school", "School:", school_choices, multiple = F), 
                        actionButton("switch_axes", 
                                     label = "Switch axes", style = "margin-bottom: 10px"),
                        impressum(),
                        width = 2
                    ),
                    
                    # Main panel for displaying outputs ----
                    mainPanel(
                        plotOutput("bivariate_plot", width = "800px"),
                        tableOutput("corr_tab")
                    )
                    
                )
            ),            
            tabPanel(
                "Data",
                sidebarLayout(
                    sidebarPanel(
                        impressum(),
                        width = 2
                    ),
                    
                    # Main panel for displaying outputs ----
                    mainPanel(
                        DT::DTOutput("raw_data")
                    )
                    
                )
            ),
            tabPanel(
              "Sessions",
              sidebarLayout(
                sidebarPanel(
                  selectizeInput("id_filter", "ID Filter", c("All IDs", 
                                                                     "LongGold IDs without Debug IDs", 
                                                                     "LongGold IDs", 
                                                                     "Other IDs"), multiple = F), 
                  selectizeInput("complete_filter", "State Filter", c("All", 
                                                                     "Finished", 
                                                                     "Unfinished"), multiple = F), 
                  
                  impressum(),
                  width = 2
                ),
                # Main panel for displaying outputs ----
                mainPanel(
                  DT::DTOutput("session_data")
                )
              )
            )
            
            ))

# Define server logic required to draw a plot
reread_data <- function(x){
  setup_workspace(result_dir, filter_debug)
}

check_data_func <- map(1:length(result_dir), function(i){
    reactiveFileReader(1000, NULL, result_dir[i], reread_data)
  })

check_data <- function(){
  for(i in 1:length(check_data_func)){
    do.call(check_data_func[[i]], list())
  }  
}

apply_school_filter <- function(data, filter_school){
  if(is.null(filter_school) || is.na(filter_school) || filter_school == "--"){
    return(data)
  }
  messagef("Filtering for %s", filter_school)
  data %>% filter(school == filter_school)
}

server <- function(input, output, session) {
  message("*** STARTING APP***")
  #check_data <- reactiveFileReader(1000, session, result_dir[1], reread_data, result_dir[1])
  shiny::observeEvent(input$switch_axes, {
    x <- input$bv_variable1
    y <- input$bv_variable2
    updateSelectizeInput(session, inputId = "bv_variable1", selected = y)
    updateSelectizeInput(session, inputId = "bv_variable2", selected = x)
       
   })
  shiny::observeEvent(input$ov_filter_school, {
    updateSelectizeInput(session, inputId = "uv_filter_school", selected = input$ov_filter_school)
    updateSelectizeInput(session, inputId = "bv_filter_school", selected = input$ov_filter_school)
    
  })

  shiny::observeEvent(input$uv_filter_school, {
    updateSelectizeInput(session, inputId = "ov_filter_school", selected = input$uv_filter_school)
    updateSelectizeInput(session, inputId = "bv_filter_school", selected = input$uv_filter_school)
    
  })
  shiny::observeEvent(input$bv_filter_school, {
    updateSelectizeInput(session, inputId = "ov_filter_school", selected = input$bv_filter_school)
    updateSelectizeInput(session, inputId = "uv_filter_school", selected = input$bv_filter_school)
    
  })
  
  output$introduction <- renderUI({
    get_intro_text()
  })
  output$overall_stats <- renderTable({
    check_data()
    #browser()
    p_id_stats <- master %>% 
      apply_school_filter(filter_school = input$ov_filter_school) %>% 
      distinct(p_id, DEG.gender, DEG.age, GMS.general, finished) %>% 
      summarise(n_female = sum(DEG.gender == "female", na.rm = T), 
                n_male = sum(DEG.gender == "male", na.rm = T), 
                n_other = sum(DEG.gender == "other", na.rm = T), 
                n_not_say = sum(DEG.gender == "not_say", na.rm = T), 
                mean_age = mean(DEG.age, na.rm = T), 
                mean_GMS = mean(GMS.general, na.rm = T), 
                n_unique = n(),
                n_complete = sum(finished, na.rm = T),
                .groups = "drop")
    p_id_stats %>% 
      select(n_unique, n_complete, starts_with("n"), mean_age, mean_GMS, everything()) %>% 
      set_names("Total N", "Completed", "Females", "Males", "Other", "Rather not say",  "Mean Age", "Mean GMS General") 
    })
   
    output$raw_data <- renderDataTable({
      check_data()
      master %>% 
        select(-p_id, -GMS.instrument ) %>% 
        mutate_if(is.numeric, round, 2) %>% 
        select(time_started, time_last_modified, DEG.first_language, finished, DEG.age, DEG.gender, everything())
      }, options = list(lengthMenu = list(c(25, 50,  -1), c("25", "50",  "All"))))
    
    output$session_data <- renderDataTable({
      check_data()
      finished_ids <- master %>% filter(finished) %>% pull(p_id)
      unfinished_ids <- master %>% filter(!finished) %>% pull(p_id)
      session_data <- read_sessions(session_dir) %>%  
        mutate(is_finished = p_id %in% finished_ids) %>%
        select(-session_name) %>% 
        select(p_id, is_longgold_id, is_debug_id, is_finished, everything())
      #browser()
      apply_session_filter(session_data, input$id_filter, input$complete_filter)
    }, options = list(lengthMenu = list(c(25, 50,  -1), c("25", "50",  "All"))))
    
    
    output$univariate_plot <- renderPlot({
      check_data()
      var_info <- var_data %>% filter(variable == input$uv_variable)
      if(var_info$type == "numeric"){
        q <- univariate_plot_numeric(master%>% apply_school_filter(input$uv_filter_school), 
                                     input$uv_variable, remove_na = T)
        } 
      else if (var_info$type == "categorial"){
        data <- master %>% apply_school_filter(input$uv_filter_school)
        coord_flip <- n_distinct(data[[input$uv_variable]]) > 3
        q <- univariate_plot_categorial(data, input$uv_variable,  remove_na = T, coord_flip = coord_flip)
      }
      else {
        return()
        }
      q
    })

    output$bivariate_plot <- renderPlot({
      check_data()
       #browser()
      if(input$bv_variable1 == input$bv_variable2){
        return()
        }
      bivariate_plot_auto(master %>% apply_school_filter(input$bv_filter_school), input, var_data, remove_na = T)   
      })
   
   #  output$pc_plot <- renderForceNetwork({
   #    check_data()
   #    if(length(input$pc_variable) < 2){
   #      return()
   #    }
   #    get_pc_graph(master %>% select(input$pc_variable), 
   #                 alpha = as.numeric(input$alpha),
   #                 charge = as.numeric(input$charge),
   #                 linkDistance = as.numeric(input$link_distance),
   #                 fontSize = as.numeric(input$font_size),
   #                 opacityNoHover = as.numeric(input$opacity))
   # })
   
  output$corr_tab <- renderTable({
    check_data()
    vars <- get_parameters(data, input, var_data = var_data)
    if(vars$sub_type == "num-num" && input$bv_variable1 != input$bv_variable2) {
      get_correlations(master %>% apply_school_filter(input$bv_filter_school), 
                       input$bv_variable1, 
                       input$bv_variable2)   
      }
    },  caption = "Correlations", caption.placement = getOption("xtable.caption.placement", "top"))
   
  output$model_tab <- renderUI({
    check_data()
    #browser()
    if(nrow(master) < 5 ){
      return()
    }
    lm_tab <- lm(scale(MDT.ability) ~ 
                   scale(DEG.age),# + 
                   #scale(RAT.ability) + 
                   #scale(GMS.general), 
                 data = master ) %>% 
      sjPlot::tab_model()
    shiny::div(shiny::h4("Main Model"), shiny::HTML(lm_tab$knitr))
  })
  output$model_tab_stats <- renderTable({
    check_data()
    lm(MDT.ability ~ DEG.age + RAT.ability + GMS.general, data = master) %>% 
      broom::glance()
   },  caption = "Main Model Performance", caption.placement = getOption("xtable.caption.placement", "top"))
  
  output$num_model_tab <- renderUI({
    check_data()
    lm_tab <- get_model(master, predictors = input$pc_variable, output_format = "sj")
    shiny::div(shiny::h4("Causal Network Model Regression"), shiny::HTML(lm_tab$knitr))
   })
  output$download_all_data_csv <- downloadHandler(
    filename = "de_wave_7_2021_data.csv",
    content = function(file) {
      dec <- ifelse(input$dec, ",", ".") 
      write.table(master, file, row.names = FALSE, dec = dec, sep = ";", quote = T)
    }
  )
  output$download_HPT <- downloadHandler(
    filename = "de_wave_7_2021_HPT_data.rds",
    content = function(file) {
      raw_data <- read_adaptive_raw_data(result_dir, "HPT")
      messagef("Read %d rows of HPT data", nrow(raw_data))
      saveRDS(file = file, object = raw_data)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui_new, server = server)

