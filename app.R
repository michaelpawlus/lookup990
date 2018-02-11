library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents")
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    library(tidyverse)
    library(xml2)
    library(jsonlite)
    library(stringr)
    
    index <- fromJSON("https://s3.amazonaws.com/irs-form-990/index_2017.json")[[1]]
    
    index2 <- index
    
    index2$TaxPeriod <- as.integer(index2$TaxPeriod)
    
    index2 <- index2 %>% arrange(desc(TaxPeriod)) %>% distinct(EIN, .keep_all = TRUE)
    
    req(input$file1)
    
    eins <- read_csv(input$file1$datapath)
    
    eins <- map_df(eins, ~str_pad(.x, 9, pad = "0"))
    
    found_ids <- eins %>%
      left_join(index2)
    
    urls <- found_ids$URL
    
    get_name_and_grants <- function(url){
      xf <- read_xml( x=url, options=NULL )
      xml_ns_strip( xf )
      name <- xf %>% xml_find_all("//Return/ReturnHeader/Filer/BusinessName/BusinessNameLine1Txt") %>% xml_text()
      fy_gifts <- xf %>% xml_find_all(".//CYContributionsGrantsAmt") %>% xml_integer()
      tibble(org_name = name, 
             grants = fy_gifts)
      
    }
    
    found_list <- urls %>%
      map(safely(get_name_and_grants)) %>%
      transpose()
    
    is_ok <- found_list$error %>% 
      map_lgl(is_null)
    
    res <- found_list$result[is_ok] %>%
      keep(~ length(.x$org_name) > 0) %>%
      {
        tibble(
          found_name = map_chr(., "org_name"),
          contributions = map_int(., "grants")
        )
      }
    
    df <- res
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
