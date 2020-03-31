#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("select_entries.R")
source("drop_field.R")
source("title_tolower.R")

fields <- c("abstract", "address", "annote", "author", "booktitle",
            "chapter", "crossref", "edition", "editor", "file", "howpublished",
            "institution", "journal", "key", "month", "note", "number",
            "organization", "pages", "publisher", "school", "series",
            "title", "type", "volume", "url")


# Define UI for application that draws a histogram
ui <- navbarPage(
    theme = shinythemes::shinytheme("sandstone"),
    # Application title
    # titlePanel("LazyBib: Work through the BibTex file for you"),
    "LazyBib: Work through the BibTex file for you",

    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Upload .bib file", accept = ".bib"),
            selectInput("drop", "Input the fields that you want to delete", choices =fields, multiple = TRUE),
            checkboxInput("title2lower",
                          "Do you want to convert the upper cases in titles to lower cases, except the initial of the first word and abbreviation?",
                          value = TRUE),
            actionButton("execute", "Execute")
        ),
        mainPanel(
            downloadButton("download"),
            verbatimTextOutput("test")
        )
    )



)

# Define server logic required to draw a histogram
server <- function(input, output) {
    file <- reactive({
        req(input$file)
        ext <- tools::file_ext(input$file$name)
        if(ext != "bib") validate("Invalid file; Please upload a .bib file")
        xfun::read_utf8(input$file$datapath)
        # select_entry(file, input$drop)
    })

    dothejob <- eventReactive(input$execute, {
        entries <- select_entry(file())
        entries <- drop_field(entries, input$drop)
        if(input$title2lower)
            entries <- title_tolower(entries)
        return(end_operation(entries))
    })


    output$test <- renderPrint({
        cat(paste(dothejob(), collapse = "\r"))
    })

    output$download <- downloadHandler(
        filename = function(){
            paste0("LazyBib_",input$file$name)
        },
        content = function(file){
            write(dothejob(), file)
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)
