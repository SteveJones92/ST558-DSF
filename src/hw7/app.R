library(shiny)
library(shinyalert)
library(tidyverse)

source("helpers.R")

hhl_vals_select <- c("All"="all", "English Only"="english", "Spanish"="spanish", "Other"="other")
fs_vals_select <- c("All"="all", "Yes"="yes", "No"="no")
schl_vals_select <- c("All"="all", "High School not Completed"="no_hs", "High School or GED"="hs", "College Degree"="college")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Correlation Exploration"),
  sidebarLayout(
    sidebarPanel(
      h2("Select Variables to Find Correlation:"),
      selectInput(
        "corr_x",
        "x Variable",
        choices = numeric_vars,
        selected = numeric_vars[1]
      ),
      selectInput(
          "corr_y",
          "y Variable",
          choices = numeric_vars,
          selected = numeric_vars[2]
      ),
      h2("Choose a subset of the data"),
      radioButtons(
        "hhl_corr",
        "Household Language",
        choices = hhl_vals_select,
        selected = hhl_vals_select[1]
      ),
      radioButtons(
          "fs_corr",
          "SNAP Recipient",
          choices = fs_vals_select,
          selected = fs_vals_select[1]
      ),
      radioButtons(
          "schl_corr",
          "Educational attainment",
          choices = schl_vals_select,
          selected = schl_vals_select[1]
      ),
      h2("Select a Sample Size"),
      sliderInput(
          "corr_n",
          "",
          min = 20,
          max = 500,
          value = 20
      ),
      actionButton("corr_sample","Get a Sample!")
    ),
    mainPanel(
      plotOutput("scatter_plot"),
      conditionalPanel(
        "input.corr_sample",
        h2("Guess the correlation!"),
        column(6,numericInput(
          "corr_guess", "", value = 0, min = -1, max = 1
        )),
        column(6, actionButton("corr_submit", "Check Your Guess!"))
      )
    ),
  )
)

my_sample <- readRDS("my_sample_temp.rds")

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    
    #################################################3
    ##Correlation tab
    sample_corr <- reactiveValues(corr_data = NULL, corr_truth = NULL)

    # update input boxes so they can't choose the same variable
    observeEvent(c(input$corr_x, input$corr_y), {
      corr_x <- input$corr_x
      corr_y <- input$corr_y
      choices <- numeric_vars
      if (corr_x == corr_y){
        choices <- choices[-which(choices == corr_x)]
        updateSelectizeInput(session,
                             "corr_y",
                             choices = choices)#we'll cover this kind of thing shortly!
      }
    })

    observeEvent(input$corr_sample, {
      if(input$hhl_corr == "all"){
        hhl_sub <- HHLvals
      } else if(input$hhl_corr == "english"){
        hhl_sub <- HHLvals["1"]
      } else if(input$hhl_corr == "spanish"){
        hhl_sub <- HHLvals["2"]
      } else {
        hhl_sub <- HHLvals[c("0", "3", "4", "5")]
      }

      if(input$fs_corr == "all"){
        fs_sub <- FSvals
      } else if(input$fs_corr == "yes"){
        fs_sub <- FSvals["1"]
      } else {
        fs_sub <- FSvals["2"]
      }

      if(input$schl_corr == "all"){
        schl_sub <- SCHLvals
      } else if(input$schl_corr == "no_hs"){
        schl_sub <- SCHLvals[as.character(0:15)]
      } else if(input$schl_corr == "hs"){
        schl_sub <- SCHLvals[as.character(16:19)]
      } else {
        schl_sub <- SCHLvals[as.character(20:24)]
      }

      corr_vars <- c(input$corr_x, input$corr_y)

      subsetted_data <- my_sample |>
        filter(#cat vars first
          HHLfac %in% hhl_sub,
          FSfac %in% fs_sub,
          SCHLfac %in% schl_sub
        ) %>% #make sure numeric variables are in appropriate range, must use %>% here for {} to work
        {if("WKHP" %in% corr_vars) filter(., WKHP > 0) else .} %>%
        {if("VALP" %in% corr_vars) filter(., !is.na(VALP)) else .} %>%
        {if("TAXAMT" %in% corr_vars) filter(., !is.na(TAXAMT)) else .} %>%
        {if("GRPIP" %in% corr_vars) filter(., GRPIP > 0) else .} %>%
        {if("GASP" %in% corr_vars) filter(., GASP > 0) else .} %>%
        {if("ELEP" %in% corr_vars) filter(., ELEP > 0) else .} %>%
        {if("WATP" %in% corr_vars) filter(., WATP > 0) else .} %>%
        {if("PINCP" %in% corr_vars) filter(., AGEP > 18) else .} %>%
        {if("JWMNP" %in% corr_vars) filter(., !is.na(JWMNP)) else .}

      index <- sample(1:nrow(subsetted_data),
                      size = input$corr_n,
                      replace = TRUE,
                      prob = subsetted_data$PWGTP/sum(subsetted_data$PWGTP))
      sample_corr$corr_data <- subsetted_data[index,]
      sample_corr$corr_truth <- cor(sample_corr$corr_data |> select(corr_vars))[1,2]
    })

    output$scatter_plot <- renderPlot({
      validate(
        need(!is.null(sample_corr$corr_data), "Please select your variables, subset, and click the 'Get a Sample!' button.")
      ) #this is a useful function to add as a placeholder until data is generated!
      ggplot(sample_corr$corr_data, aes_string(x = isolate(input$corr_x), y = isolate(input$corr_y))) +
        geom_point()
    })


    #Use this code for the correlation guessing game!
    observeEvent(input$corr_submit, {
      close <- abs(input$corr_guess - sample_corr$corr_truth) <= .05
      if(close){
        shinyalert(title = "Nicely done!",
                   paste0("The sample correlation is ",
                          round(sample_corr$corr_truth, 4),
                          "."),
                   type = "success"
        )
      } else {
        if(input$corr_guess > sample_corr$corr_truth){
          shinyalert(title = "Try again!",
                     "Try guessing a lower value.")
        } else {
          shinyalert(title = "Try again!",
                     "Try guessing a higher value.")
        }
      }
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
