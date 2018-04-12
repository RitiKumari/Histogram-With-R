library(shiny)

ui <- fluidPage(
  pageWithSidebar(
    
    headerPanel("My First Shiny App"),
    
    sidebarPanel(
      selectInput("Distribution", "Select distribution Type", 
                  choices = c("Normal", "Exponential")),
      sliderInput("SampleSize", "Please select the sample size",
                  min= 100, max = 5000, step = 100, value = 1000),
      conditionalPanel(condition = "input.Distribution == 'Normal'",
                       textInput("Mean","Please enter the mean", 10),
                       textInput("Sd","Please enter the standard deviation",3)),
      conditionalPanel(condition = "input.Distribution == 'Exponential'",
                       textInput("Lambda","Please enter the lambda", 1))
    ),
    
    mainPanel(
      plotOutput("myPlot")
      
    )
  )
  
)

server <- function(input, output){
  output$myPlot <- renderPlot({
    disType <- input$Distribution
    size <- input$SampleSize
    
    if(disType == "Normal"){
      randvec <- rnorm(size, mean= as.numeric(input$Mean), sd=as.numeric(input$Sd))
      
    }
    else{
      randvec <-rexp(size, rate = 1/as.numeric(input$Lambda))
    }
    
    hist(randvec, col="blue")
    
  })
}

shinyApp(ui=ui , server = server)