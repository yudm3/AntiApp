# Load libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr)
library(shinythemes)
library(shinycssloaders)
library(plotly)
library(DT)
library(RColorBrewer)

# Read and prepare data
data <- read_csv("screen_time.csv")
data$Date <- as.Date(data$Date, format="%Y/%m/%d")
data$TotalAppMinutes <- data$AppHours * 60 + data$AppMinutes

# Use the data directly
full_data <- data

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Anti App"),
  dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Courier+Prime&display=swap"),
      tags$style(HTML("
        .sidebar-menu li a {
          font-size: 18px;
        }
        .main-sidebar {
          width: 250px;
        }
        /* Set global font to Courier Prime */
        body {
          font-family: 'Courier Prime', monospace;
        }
      "))
    ),
    sidebarMenu(id = "tabs",
                menuItem("About App", tabName = "about", icon = icon("info-circle")),
                menuItem("Home", tabName = "home", icon = icon("home")),
                menuItem("Set a Goal", tabName = "set_goal", icon = icon("bullseye")),
                menuItem("Results", tabName = "results", icon = icon("chart-line")),
                menuItem("References", tabName = "references", icon = icon("book"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          margin-left: 250px;
        }
        .main-header .logo {
          width: 250px;
        }
        /* Increase font size for input labels */
        .shiny-input-container > label {
          font-size: 16px;
          font-family: 'Courier Prime', monospace;
        }
        /* Increase font size for input values */
        .selectize-input, .input-daterange input, .shiny-date-range-input input {
          font-size: 16px;
          font-family: 'Courier Prime', monospace;
        }
        /* Increase font size for date input fields */
        .date-range input {
          font-size: 16px !important;
          font-family: 'Courier Prime', monospace;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "about",
              h2("Welcome to Anti App", style = "font-family: 'Courier Prime';"),
              p("Anti App is designed to help you monitor and reduce your screen time by setting goals and tracking your progress.", style = "font-family: 'Courier Prime'; font-size: 16px;"),
              h3("How It Works", style = "font-family: 'Courier Prime';"),
              tags$ol(
                tags$li("Navigate to the Home page to view your current screen time statistics.", style = "font-family: 'Courier Prime'; font-size: 16px;"),
                tags$li("Review your Daily Screen Time and App Usage Breakdown.", style = "font-family: 'Courier Prime'; font-size: 16px;"),
                tags$li("Check your Top 3 Most Used Apps and consider setting goals for them.", style = "font-family: 'Courier Prime'; font-size: 16px;"),
                tags$li("Go to the 'Set a Goal' page to create personalized screen time reduction plans.", style = "font-family: 'Courier Prime'; font-size: 16px;"),
                tags$li("Follow your reduction plan and track your progress over time.", style = "font-family: 'Courier Prime'; font-size: 16px;")
              ),
              p("We hope Anti App helps you achieve a healthier balance with your screen time!", style = "font-family: 'Courier Prime'; font-size: 16px;")
      ),
      tabItem(tabName = "home",
              h2("Anti App - Home Page", style = "font-family: 'Courier Prime';"),
              fluidRow(
                box(width = 12,
                    column(6, selectInput("user", "Select User:", choices = unique(full_data$UserID))),
                    column(6, dateRangeInput("dateRange", "Select Date Range:",
                                             start = min(full_data$Date),
                                             end = max(full_data$Date),
                                             width = '100%',
                                             # Use tags to increase font size
                                             label = tags$span("Select Date Range:", style = "font-size:16px; font-family: 'Courier Prime';"),
                                             startview = "month"))
                )
              ),
              fluidRow(
                box(title = "Daily Screen Time", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("dailyScreenTimePlot", height = 400) %>% withSpinner())
              ),
              fluidRow(
                box(title = "App Usage Breakdown", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("appUsagePlot", height = 500) %>% withSpinner())
              ),
              fluidRow(
                box(title = "Top 3 Apps", status = "primary", solidHeader = TRUE, width = 12,
                    uiOutput("topAppsUI"))
              )
      ),
      tabItem(tabName = "set_goal",
              h2("Anti App - Set a Goal", style = "font-family: 'Courier Prime';"),
              uiOutput("goalSettingUI")
      ),
      tabItem(tabName = "results",
              h2("Anti App - Results", style = "font-family: 'Courier Prime';"),
              uiOutput("reductionMessage"),
              uiOutput("reductionPlanTabs")
      ),
      tabItem(tabName = "references",
              h2("References", style = "font-family: 'Courier Prime';"),
              p("This project is open-source and available on GitHub:", style = "font-family: 'Courier Prime'; font-size: 16px;"),
              tags$a(href = "https://github.com/yudm3/AntiApp", "GitHub Repository", target = "_blank", style = "font-family: 'Courier Prime'; font-size: 16px;"),
              h3("Contact Information", style = "font-family: 'Courier Prime';"),
              p("Name: YUGAY DMITRIY", style = "font-family: 'Courier Prime'; font-size: 16px;"),
              p("Email: ", tags$a(href = "mailto:yudm3hw@gmail.com", "yudm3hw@gmail.com", style = "font-family: 'Courier Prime'; font-size: 16px;"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive data for the selected user and date range
  user_data <- reactive({
    req(input$user, input$dateRange)
    full_data %>%
      filter(UserID == input$user,
             Date >= input$dateRange[1],
             Date <= input$dateRange[2])
  })
  
  # Reactive list of apps used by the user, ordered by total usage
  user_apps <- reactive({
    user_data() %>%
      group_by(AppName) %>%
      summarize(TotalMinutes = sum(TotalAppMinutes)) %>%
      arrange(desc(TotalMinutes)) %>%
      pull(AppName)
  })
  
  # Total daily screen time with formatted usage time
  daily_screen_time <- reactive({
    user_data() %>%
      group_by(Date) %>%
      summarize(
        TotalHours = sum(TotalAppMinutes) / 60,
        UsageTimeText = paste0(
          floor(TotalHours), "h ",
          round((TotalHours - floor(TotalHours)) * 60), "m"
        )
      )
  })
  
  # App usage breakdown
  app_usage <- reactive({
    user_data() %>%
      group_by(AppName) %>%
      summarize(TotalMinutes = sum(TotalAppMinutes)) %>%
      mutate(
        TotalHours = TotalMinutes / 60,
        UsageTimeText = paste0(
          floor(TotalHours), "h ",
          round((TotalHours - floor(TotalHours)) * 60), "m"
        )
      )
  })
  
  # Plot daily screen time
  output$dailyScreenTimePlot <- renderPlotly({
    p <- ggplot(daily_screen_time(), aes(x = Date, y = TotalHours, text = UsageTimeText, group = 1)) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(color = "steelblue", size = 2) +
      labs(title = "Daily Screen Time", x = "Date", y = "Total Hours") +
      scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold", family = "Courier Prime"),
        axis.title = element_text(size = 16, family = "Courier Prime"),
        axis.text = element_text(size = 14, family = "Courier Prime"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    ggplotly(p, tooltip = c("x", "text"))
  })
  
  # App usage breakdown (pie chart)
  output$appUsagePlot <- renderPlotly({
    app_usage_data <- app_usage() %>%
      mutate(Percentage = TotalMinutes / sum(TotalMinutes) * 100)
    
    plot_ly(app_usage_data, labels = ~AppName, values = ~TotalMinutes, type = 'pie',
            textinfo = 'label+percent',
            hoverinfo = 'label+text',
            text = ~paste0("Usage Time: ", UsageTimeText),
            insidetextfont = list(color = '#FFFFFF', size = 14, family = "Courier Prime"),
            marker = list(
              colors = colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(nrow(app_usage_data)),
              line = list(color = '#FFFFFF', width = 1)
            )) %>%
      layout(title = list(
        text = paste('App Usage Breakdown (', format(input$dateRange[1], "%b %d"), ' - ', format(input$dateRange[2], "%b %d"), ')', sep=''),
        font = list(family = "Courier Prime", size = 20)
        ),
        showlegend = TRUE,
        legend = list(font = list(size = 18, family = "Courier Prime")),
        margin = list(l = 50, r = 50, b = 50, t = 80))
  })
  
  # Top 3 Most Used Apps
  output$topAppsUI <- renderUI({
    top_apps <- app_usage() %>%
      arrange(desc(TotalMinutes)) %>%
      head(3) %>%
      mutate(TotalHours = round(TotalMinutes / 60, 2))
    
    app_list <- lapply(1:nrow(top_apps), function(i) {
      app <- top_apps$AppName[i]
      hours <- top_apps$TotalHours[i]
      tags$li(tags$span(style = "font-family: 'Courier Prime'; font-size: 18px;", paste0(app, ": ", hours, " hours")))
    })
    
    action_button <- actionButton("goToSetGoal", "Set a Goal", icon = icon("bullseye"), 
                                  style = "font-family: 'Courier Prime'; font-size: 18px; width: 200px; height: 50px; background-color: #007BFF; color: #FFFFFF;")
    
    tagList(
      tags$h3("Top 3 Most Used Apps:", style = "font-family: 'Courier Prime';"),
      tags$ul(app_list),
      tags$br(),
      action_button
    )
  })
  
  # Navigate to the Set a Goal page
  observeEvent(input$goToSetGoal, {
    updateTabItems(session, "tabs", "set_goal")
  })
  
  # Goal Setting UI
  output$goalSettingUI <- renderUI({
    tagList(
      h3("Select Apps to Set Goals", style = "font-family: 'Courier Prime';"),
      selectInput("goalApps", "Apps:", choices = user_apps(), selected = NULL, multiple = TRUE),
      h3("Set Daily Usage Limits (Minutes)", style = "font-family: 'Courier Prime';"),
      uiOutput("appLimitsUI"),
      dateInput("startDate", "Select Start Date:", value = Sys.Date(), format = "yyyy-mm-dd", startview = "month"),
      actionButton("applyGoal", "Apply Goal", style = "font-family: 'Courier Prime'; font-size: 18px; width: 200px; height: 50px; background-color: #28a745; color: #FFFFFF;")
    )
  })
  
  # Generate input fields for each selected app
  output$appLimitsUI <- renderUI({
    req(input$goalApps)
    lapply(input$goalApps, function(app) {
      input_id <- paste0("limit_", gsub("[^[:alnum:]_]", "_", app))
      numericInput(inputId = input_id, label = paste("Limit for", app), value = 60)
    })
  })
  
  # Handle Goal Application
  observeEvent(input$applyGoal, {
    req(input$goalApps, input$startDate)
    # Gather limits
    limits <- sapply(input$goalApps, function(app) {
      input_id <- paste0("limit_", gsub("[^[:alnum:]_]", "_", app))
      input[[input_id]]
    })
    names(limits) <- input$goalApps
    
    days <- 14  # Assuming a 14-day plan
    start_date <- input$startDate
    
    # Create a reduction plan
    reduction_plan <- do.call(rbind, lapply(names(limits), function(app) {
      current_avg <- app_usage() %>%
        filter(AppName == app) %>%
        pull(TotalMinutes) / length(unique(user_data()$Date))
      goal_minutes <- limits[app]
      
      if (current_avg > goal_minutes) {
        seq_values <- seq(from = current_avg, to = goal_minutes, length.out = days)
      } else {
        seq_values <- rep(goal_minutes, days)
      }
      
      data.frame(
        AppName = app,
        Date = seq.Date(from = start_date, by = "day", length.out = days),
        TargetMinutes = round(seq_values, 1)
      )
    }))
    
    # Render the plan as a table
    output$reductionPlanTable <- renderDataTable({
      plan_table <- reduction_plan %>%
        select(Date, AppName, TargetMinutes) %>%
        arrange(AppName, Date)
      
      datatable(plan_table, options = list(pageLength = 10), rownames = FALSE)
    })
    
    # Render the message
    output$reductionMessage <- renderUI({
      messages <- lapply(names(limits), function(app) {
        current_avg <- app_usage() %>%
          filter(AppName == app) %>%
          pull(TotalMinutes) / length(unique(user_data()$Date))
        goal_minutes <- limits[app]
        if (current_avg > goal_minutes) {
          total_reduction <- current_avg - goal_minutes
          daily_reduction <- total_reduction / days
          tags$p(style = "font-family: 'Courier Prime'; font-size: 16px;",
                 paste0("To achieve your goal for ", app, ", you need to reduce your usage by ",
                        round(daily_reduction, 1), " minutes per day over ", days, " days."))
        } else {
          tags$p(style = "font-family: 'Courier Prime'; font-size: 16px;",
                 paste0("Your current usage of ", app, " is already below your goal."))
        }
      })
      do.call(tagList, messages)
    })
    
    # Generate tabs for each app
    output$reductionPlanTabs <- renderUI({
      req(reduction_plan)
      apps <- unique(reduction_plan$AppName)
      tabpanels <- lapply(apps, function(app) {
        # Define app_id within the lapply function
        app_id <- gsub("[^[:alnum:]_]", "_", app)
        tabPanel(
          title = app,
          tags$div(
            style = "margin-top: 20px;",
            dataTableOutput(outputId = paste0("reductionPlanTable_", app_id))
          ),
          tags$br(),
          plotOutput(outputId = paste0("reductionPlanPlot_", app_id), height = 400)
        )
      })
      do.call(tabsetPanel, c(list(id = "appTabs"), tabpanels))
    })
    
    # Render tables and plots for each app
    observe({
      req(reduction_plan)
      apps <- unique(reduction_plan$AppName)
      for (app in apps) {
        local({
          app_name <- app
          app_id <- gsub("[^[:alnum:]_]", "_", app_name)
          app_data <- reduction_plan[reduction_plan$AppName == app_name, ]
          
          output[[paste0("reductionPlanTable_", app_id)]] <- renderDataTable({
            datatable(app_data[, c("Date", "AppName", "TargetMinutes")], options = list(pageLength = 10), rownames = FALSE)
          })
          
          output[[paste0("reductionPlanPlot_", app_id)]] <- renderPlot({
            ggplot(app_data, aes(x = factor(Date), y = TargetMinutes)) +
              geom_bar(stat = "identity", fill = "steelblue") +
              geom_text(aes(label = paste0(floor(TargetMinutes / 60), "h ", round(TargetMinutes %% 60), "m")), vjust = -0.5) +
              labs(title = paste("Reduction Plan for", app_name), x = "Date", y = "Usage Time (Minutes)") +
              theme_minimal() +
              theme(
                plot.title = element_text(size = 20, face = "bold", family = "Courier Prime"),
                axis.title = element_text(size = 16, family = "Courier Prime"),
                axis.text = element_text(size = 14, family = "Courier Prime"),
                axis.text.x = element_text(angle = 45, hjust = 1)
              )
          })
        })
      }
    })
    
    # Navigate to the Results page
    updateTabItems(session, "tabs", "results")
    
    # Show user agreement modal
    showModal(modalDialog(
      title = "User Agreement",
      tags$p("By using this app, you agree to voluntarily participate in the screen time reduction plan. The app may provide recommendations or block certain apps and websites to help you achieve your goals. You understand and acknowledge that you are responsible for your usage habits."),
      easyClose = FALSE,
      footer = tagList(
        actionButton("agreeAgreement", "I Agree", class = "btn btn-primary"),
        actionButton("declineAgreement", "I Do Not Agree", class = "btn btn-secondary")
      )
    ))
  })
  
  # Handle User Agreement Response
  observeEvent(input$declineAgreement, {
    removeModal()
    showNotification("You need to agree to the user agreement to proceed.", type = "error")
  })
  
  observeEvent(input$agreeAgreement, {
    removeModal()
    # Navigate to the Results page
    updateTabItems(session, "tabs", "results")
    showNotification("Your reduction plan is set. Good luck!", type = "message")
  })
}



shinyApp(ui = ui, server = server)
