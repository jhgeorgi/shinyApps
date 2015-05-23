#install.packages("shiny")
library(shiny)

stateList <- data.frame(read.csv("statelist.csv",header = FALSE))
stateList$V1<-as.character(stateList$V1)

shinyUI(fluidPage(
     headerPanel('Craft Artist Employment, 2004-2014'),
     sidebarPanel(
          h5("By default, the National Data for all employment is shown."),
          h5("Select a State to View below the National Data"),
          selectInput('names','States (not all states represented)',stateList$V1),
          h5("Selecting a variable, will update both the National and State Data"),
          selectInput('var1','Variable', c('Total Employent', 'Self Employed','Employed (not Self)', 'Mean Hourly Wage', 'Mean Annual Income'),selected = 'Total Employment'),
          br(),
          h5("Select number of year to forecast on choosen data"),
          numericInput("obs", "Years:", 5, min = 1, max = 10)
          
     ),
     
     mainPanel(
          
          plotOutput('nationalPlot'),
          textOutput('NF'),
          plotOutput('statePlot'),
          textOutput('SF')

     )
))
