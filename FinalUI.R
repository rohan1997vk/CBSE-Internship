library(ggplot2)
library(shiny)
library(shinydashboard)
library(dplyr)
my_data <- read.csv("Test_5.csv")
my_data <- subset(my_data, select=c(-1,-2))
my_data <- as.data.frame(sapply(my_data, as.numeric))
my_data[is.na(my_data)] <- ""
c_new <- read.csv("colors1.csv")
c_new <- as.data.frame(c_new)


ui <- fluidPage(    
  titlePanel("Marks Distribution"),
  sidebarLayout(      
    sidebarPanel(
      selectInput("scode", "Subject Code:",choices=colnames(my_data)),
      selectInput("clr", "Color:",choices=c_new),
      selectInput("bsize", "Bin Size:",choices=seq(10,100,by=10))
    ),
    mainPanel(plotOutput("MarksPlot"))
  )
)
server <- function(input, output) 

  {output$MarksPlot <- renderPlot({
  ggplot(my_data, aes(x=as.numeric(my_data[,input$scode]))) + 
    geom_area(stat = 'bin', binwidth =as.numeric(input$bsize), fill =input$clr) + 
    geom_smooth(aes(y=as.numeric(my_data[,input$scode])), span = 0.08) +
               ggtitle("Marks Distribution ") + xlab("Marks") + ylab("Students")
})}            
shinyApp(ui,server)