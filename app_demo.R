# Load Packages
library(shiny)
library(tidyverse)

# Load Data
pitch_values <- read_rds("pitch_values")
batter_data <- read_rds("batter_info")
pitcher_data <- read_rds("pitcher_info")

# Create New Variables
pitcher_data <- pitcher_data %>%
  droplevels() 

batter_data <- batter_data %>%
  droplevels() 

pitch_values <- pitch_values %>%
  droplevels() %>%
  ungroup() %>%
  mutate(balls = substr(count, 1, 1), strikes = substr(count, 3, 3)) %>%
  left_join(pitcher_data) %>%
  left_join(batter_data) %>%
  mutate(pitcher_ev = round(-mean_ev, 3), shouldSwing = (mean_ev_swing > mean_ev_take),
         p_wrongtake = (1 - prob_swing) * shouldSwing, p_wrongswing = prob_swing * (1 - shouldSwing),
         p_wrongdec = p_wrongtake + p_wrongswing)

# UI ----------------------------------------------------------------
ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
    
    # Inputs --------------------------------------------------------    
    sidebarPanel(
      
      selectInput("pitcher", 
                  label = "Pitcher",
                  choices = unique(pitcher_data$pitcher_name),
                  selected = "Corey Kluber"),
      
      selectInput("batter", 
                  label = "Batter",
                  choices = unique(batter_data$batter_name),
                  selected = "Edwin Encarnacion"),
      
      
      radioButtons("balls",
                   label = "Balls",
                   choices = c(0, 1, 2, 3),
                   inline = TRUE,
                   selected = 0),
      
      radioButtons("strikes",
                   label = "Strikes",
                   choices = c(0, 1, 2),
                   inline = TRUE,
                   selected = 0),
      checkboxGroupInput("pitch_type","Select pitch types:", choices = unique(pitch_values$pitch_type), 
                         selected = "FF")),
    
    # Output --------------------------------------------------------    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Pitcher expected value",
                           uiOutput("runvals"),
                           plotOutput("ev_plot")
                  ),
                  tabPanel("Optimal batter decisions",
                           plotOutput("should_swing")
                  ),
                  tabPanel("Plate Discipline Tendencies",
                           uiOutput("prob_bad"),
                           uiOutput("dec_type"),
                           plotOutput("pd_tend")
                  )
      )
      

    )
    
  )
)


# SERVER ------------------------------------------------------------
server = function(input, output) {
  
  count_filtered <- reactive({
    req(input$balls)
    req(input$strikes)
    pitch_values %>%
      filter(balls == input$balls, strikes == input$strikes) %>%
      mutate(left = case_when(
        hand == "L" ~ -1.03,
        T ~ -1.
      ), right = case_when(
        hand =="L" ~ 1,
        T ~ 1.03))
  })
  
  filtered_data <- reactive({
    req(input$pitcher)
    req(input$batter)
    count_filtered() %>%
      filter(batter_name == input$batter, pitcher_name == input$pitcher) %>%
      mutate(left = case_when(
        hand == "L" ~ -1.03,
        T ~ -1.
      ), right = case_when(
        hand =="L" ~ 1,
        T ~ 1.03))
  })
  
  sz_coords <- reactive({
    df <- expand.grid(c(filtered_data()$sz_bot[1], filtered_data()$sz_top[1]),
                             c(filtered_data()$left[1],filtered_data()$right[1]))
    df <- rbind(df, df)
    colnames(df) <- c("plate_z", "plate_x")
    df$group <- c(1,1,2,2,3,4,3,4)
    df
  })
  
  output$runvals <- renderUI({
    extreme_val <- max(abs(count_filtered()$pitcher_ev))
    min_v <- -extreme_val
    max_v <- extreme_val
    sliderInput("run_limits",
                label = "Pitch expected value (in runs) between:",
                min = min_v, max = max_v, value = c(min_v, max_v), step=.001)
    
  })
  
  plot_data_ptype <- reactive({
    req(input$pitch_type)
    filtered_data() %>%
      filter(pitch_type %in% input$pitch_type)
  })
  
  plot_data1 <- reactive({
    req(input$run_limits)
    pd1 <- plot_data_ptype() %>%
    filter((pitcher_ev <= input$run_limits[2]) & pitcher_ev >= input$run_limits[1])
    suppressWarnings(validate(
      need(((input$run_limits[1] <= max(pd1$pitcher_ev)) | (input$run_limits[2] >= min(pd1$pitcher_ev))) & (length(pd1$pitcher_ev > 0)), 
           "In this context, there are no areas of the strike zone for which this pitcher has such an extreme expected value.")
    ))
    pd1
  })
  
  output$prob_bad <- renderUI({
    min_v <- 0
    max_v <- 1
    sliderInput("prob_limits",
                label = "Probability of incorrect decision between:",
                min = min_v, max = max_v, value = c(min_v, max_v), step=.01)
    
  })
  
  output$dec_type <- renderUI({
    checkboxGroupInput("dec_type", "Type of mistake:", choices = c("Aggressive", "Passive"), 
                       selected = c("Aggressive", "Passive"))
  })
  
  plot_data3 <- reactive({
    req(input$dec_type)
    req(input$prob_limits)
    pd3 <- plot_data_ptype() %>%
      mutate(y = case_when(
        ("Aggressive" %in% input$dec_type) & ("Passive" %in% input$dec_type) ~ p_wrongdec,
        ("Aggressive" %in% input$dec_type) ~ p_wrongswing,
        ("Passive" %in% input$dec_type) ~ p_wrongtake,
        T ~ -1
      )) %>%
      filter((y <= input$prob_limits[2]) & (y >= input$prob_limits[1]) & (y > 0))
    suppressWarnings(validate(
      need(((input$prob_limits[1] <= max(pd3$y)) | (input$prob_limits[2] >= min(pd3$y))) & (length(pd3$y > 0)), 
           "In this context, there are no areas of the strike zone for which this batter has such an extreme error rate of this type.")
    ))
    pd3
  })
  
  output$runvals <- renderUI({
    extreme_val <- max(abs(count_filtered()$pitcher_ev))
    min_v <- -extreme_val
    max_v <- extreme_val
    sliderInput("run_limits",
                label = "Pitch expected value (in runs) between:",
                min = min_v, max = max_v, value = c(min_v, max_v), step=.001)
    
  })
  
  output$ev_plot <- renderPlot({
    ggplot(plot_data1(), aes(x = plate_x, y = plate_z)) +
      geom_raster(aes(fill = pitcher_ev)) +
      geom_line(data = sz_coords(), mapping = aes(x = plate_x, y = plate_z,
                                                group = group),
      colour = "black") +
      scale_fill_gradient2(low = "blue", high = "red",
                           midpoint = (min(filtered_data()$pitcher_ev) + max(filtered_data()$pitcher_ev))/2,
                           name = "Pitcher EV") +
      xlim(c(-1.5,1.5)) +
      ylim(c(0,4)) +
      facet_wrap(~ pitch_type, ncol = 2) +
      xlab("Horizontal Location") +
      ylab("Height Above Ground (ft)") +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=12, face = "bold"),
            plot.title=element_text(size=14, face = "bold", hjust = 0.5))
  })
  
  output$should_swing <- renderPlot({
    ggplot(plot_data_ptype(), aes(x = plate_x, y = plate_z)) +
      geom_raster(aes(fill = shouldSwing), alpha = 0.8) +
      geom_line(data = sz_coords(), mapping = aes(x = plate_x, y = plate_z,
                                                  group = group),
                colour = "black") +
      scale_fill_manual(name = "Should Swing?", values = c("blue", "red"), labels = c("No", "Yes")) +
      xlim(c(-1.5,1.5)) +
      ylim(c(0,4)) +
      facet_wrap(~ pitch_type, ncol = 2) +
      xlab("Horizontal Location") +
      ylab("Height Above Ground (ft)") +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=12, face = "bold"),
            plot.title=element_text(size=14, face = "bold", hjust = 0.5))
  })
  
  output$pd_tend <- renderPlot({
    ggplot(plot_data3(), aes(x = plate_x, y = plate_z)) +
      geom_raster(aes(fill = y)) +
      geom_line(data = sz_coords(), mapping = aes(x = plate_x, y = plate_z,
                                                  group = group),
                colour = "black") +
      scale_fill_gradient2(low = "blue", high = "red",
                           midpoint = 0.5,
                           name = "Pr(Bad Decision)") +
      xlim(c(-1.5,1.5)) +
      ylim(c(0,4)) +
      facet_wrap(~ pitch_type, ncol = 2) +
      xlab("Horizontal Location") +
      ylab("Height Above Ground (ft)") +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=12, face = "bold"),
            plot.title=element_text(size=14, face = "bold", hjust = 0.5))
  })
  
  
}

# Run app -----------------------------------------------------------
shinyApp(ui = ui, server = server)
