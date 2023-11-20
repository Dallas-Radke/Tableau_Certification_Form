library(tidyverse)
library(lubridate)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(digest)
library(bslib)


# which fields get saved
fieldsAll <- c("requestor_name",
               "publisher_name",
               "dashboard_name",
               "live_extracts",
               "db_load_time",
               "wb_optimizer_score",
               "db_glossary",
               "naming_convention",
               "header_convention",
               "business_justification_desc")

# which fields are mandatory
fieldsMandatory <- c("requestor_name",
                     "publisher_name",
                     "dashboard_name",
                     "live_extracts",
                     "db_load_time",
                     "wb_optimizer_score",
                     "db_glossary",
                     "naming_convention",
                     "header_convention")


# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}


# get current Epoch time
epochTime <- function() {
  Sys.time()
}


# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in a window filename)
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}


# save the results to a file
saveData <- function(data) {
  fileName <- sprintf("%s_%s.csv",
                      humanTime(),
                      digest::digest(data))
  
  write.csv(x = data, file = file.path(responsesDir, fileName),
            row.names = FALSE, quote = TRUE)
}


# directory where responses are stored
responsesDir <- file.path("/Users/drad/Documents/Data_Projects/Tableau_Form/Responses/")


# pull in employee list
employees_df <- read_csv("/Users/drad/Documents/Data_Projects/Tableau_Form/Employee_List.csv") %>% 
  as_tibble() %>% 
  janitor::clean_names()


# CSS to use in the app
appCSS <- ".mandatory_star {color: red}"


shinyApp(
  ui = fixedPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    titlePanel(title = div(img(src = "tableau_logo.png", height = 45, width = 45),
                           "Tableau Dashboard Certification",
                           br(), br())
    ),
    
    theme = bs_theme(
      version = 4,
      bootswatch = "superhero"
    ),
    
    div(
      id = "form",
      
      fluidRow(
        column(3,
               selectizeInput("requestor_name",
                              labelMandatory("Requestor"),
                              employees_df$full_name,
                              options = list(openOnFocus = F))
        ),
        column(3,
               textInput("publisher_name",
                         labelMandatory("Publisher Name"), "")
        ),
        column(3,
               textInput("dashboard_name",
                         labelMandatory("Dashboard Name"), "")
        )
      ),
      
      fluidRow(
        column(5,
               selectInput("live_extracts",
                           labelMandatory("Is the dashboard using extracts for all data sources?"),
                           c("Yes", "No", "No (w/ Justification)")),
               selectInput("wb_optimizer_score",
                           labelMandatory("Is the workbook optimizer score 100% dispositioned?"),
                           c("Yes", "No")),
               selectInput("header_convention",
                           labelMandatory("Is there a proper header w/ required elements (company logo/title/date)?"),
                           c("Yes", "No"))
               ),
        column(5,
               selectInput("db_load_time",
                           labelMandatory("Is the dashboard load time 10 seconds or below?"),
                           c("Yes", "No", "No (w/ Justification)")),
               selectInput("naming_convention",
                           labelMandatory("Are there proper naming conventions (e.g. title, tab names)?"),
                           c("Yes", "No")),
               selectInput("db_glossary",
                           labelMandatory("Does the dashboard include a glossary for definitions?"),
                           c("Yes", "No")),
        )
      ),
      
      textAreaInput("business_justification_desc",
                    "Business Justification",
                    width = '60%',
                    rows = 7),
      
      actionButton("submit",
                   "Submit",
                   class = "btn-primary"),
      
      shinyjs::hidden(
        span(id = "submit_msg", "Submitting..."),
        div(id = "error",
            div(br(), tags$b("Error: "), span(id = "error_msg"))
        )
      )
    ),
    
    
    shinyjs::hidden(
      div(
        id = "thankyou_msg",
        h3("Thanks, your response was submitted successfully!"),
        actionLink("submit_another", "Submit another response")
      )
    )
    
  ),
  
  
  server = function(input, output, session) {
    
    # Enable the submit button when all mandatory fields are filled out
    observe({
      mandatoryFiilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFiilled <- all(mandatoryFiilled)
      
      shinyjs::toggleState(id = "submit", condition = mandatoryFiilled)
    })
    
    # Gather all the form inputs & add timestamp
    formData <- reactive({
      data <- sapply(fieldsAll, function(x) input[[x]])
      data <- c(data, timestamp = format(lubridate::now(), "%Y%m%d-%H%M%OS"))
      data <- t(data)
      data
    })
    
    # action to take when submit button is pressed
    observeEvent(input$submit, {
      
      # user experience stuff
      shinyjs::disable("submit")
      shinyjs::show("submit_msg")
      shinyjs::hide("error")
      
      # save the data (show error message in case of error)
      tryCatch({
        saveData(formData())
        shinyjs::reset("form")
        shinyjs::hide("form")
        shinyjs::show("thankyou_msg")
      },
      error = function(err) {
        shinjs::html("error_msg", err$message)
        shinyjs::show(id = "error", anim = TRUE, animType = "fade")
      },
      finally = {
        shinyjs::enable("submit")
        shinyjs::hide("submit_msg")
      })
    })
    
    # submit another response
    observeEvent(input$submit_another, {
      shinyjs::show("form")
      shinyjs::hide("thankyou_msg")
    })
    
  }
)



