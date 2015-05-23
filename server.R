#library(shiny)
require(data.table)
require(ggplot2)

nationalData <- read.csv("national_craftartists.csv")
stateData <- read.csv("state_craftartists.csv",na.strings = "NA")

shinyServer(function(input, output) {
     var1 <- reactive(input$var1)
     
     output$nationalPlot <- renderPlot({
          NdataDT <- data.table(nationalData)
          y_name <- switch(var1(),
                      "Total Employent" = "Total Employent",
                      "Self Employed" = "Self Employed" ,
                      "Employed (not Self)" = "Employed (not Self)",
                      "Mean Hourly Wage" = "Mean Hourly Wage", 
                      "Mean Annual Income" = "Mean Annual Income")
          y_var <- switch(var1(),
                     "Total Employent" = NdataDT$ALL_EMP,
                     "Self Employed" = NdataDT$SELF_EMP ,
                     "Employed (not Self)" = NdataDT$TOT_EMP,
                     "Mean Hourly Wage" = NdataDT$H_MEAN, 
                     "Mean Annual Income" = NdataDT$A_MEAN)
          qplot(NdataDT$year, y_var,colour=I("red"),size = I(4),main = "National Data",xlab="Year", ylab = y_name)
     })
          
     output$NF <- renderText({
          NdataDT <- data.table(nationalData)
          y_name <- switch(var1(),
                           "Total Employent" = "Total Employent",
                           "Self Employed" = "Self Employed" ,
                           "Employed (not Self)" = "Employed (not Self)",
                           "Mean Hourly Wage" = "Mean Hourly Wage", 
                           "Mean Annual Income" = "Mean Annual Income")
          y_var <- switch(var1(),
                          "Total Employent" = NdataDT$ALL_EMP,
                          "Self Employed" = NdataDT$SELF_EMP ,
                          "Employed (not Self)" = NdataDT$TOT_EMP,
                          "Mean Hourly Wage" = NdataDT$H_MEAN, 
                          "Mean Annual Income" = NdataDT$A_MEAN)
          fit1 <- glm(y_var ~ NdataDT$year,na.action = na.omit)
          futureYear <- data.table(year = c(2015:(2015+input$obs)))
          fit1_pred <- predict(fit1,futureYear,type="response")
          (paste("The predicted value for", y_name, "in", input$obs, "years is", round(fit1_pred[input$obs+1],2)))
          
     })
         
     output$statePlot <- renderPlot({
          SdataDT <- data.table(subset(stateData,STATE==input$names))
          y_name1 <- switch(var1(),
                      "Total Employent" = "Total Employent",
                      "Self Employed" = "Self Employed" ,
                      "Employed (not Self)" = "Employed (not Self)",
                      "Mean Hourly Wage" = "Mean Hourly Wage", 
                      "Mean Annual Income" = "Mean Annual Income")
          y_var1 <- switch(var1(),
                     "Total Employent" = SdataDT$ALL_EMP,
                     "Self Employed" = SdataDT$SELF_EMP ,
                     "Employed (not Self)" = SdataDT$TOT_EMP,
                     "Mean Hourly Wage" = SdataDT$H_MEAN, 
                     "Mean Annual Income" = SdataDT$A_MEAN)
          qplot(SdataDT$year, y_var1,colour=I("blue"),size = I(4),main = paste("Employment Data for",input$names),xlab="Year", ylab = y_name1)
     })
     
     output$SF <- renderText({
          SdataDT <- data.table(subset(stateData,STATE==input$names))
          y_name <- switch(var1(),
                           "Total Employent" = "Total Employent",
                           "Self Employed" = "Self Employed" ,
                           "Employed (not Self)" = "Employed (not Self)",
                           "Mean Hourly Wage" = "Mean Hourly Wage", 
                           "Mean Annual Income" = "Mean Annual Income")
          y_var <- switch(var1(),
                          "Total Employent" = SdataDT$ALL_EMP,
                          "Self Employed" = SdataDT$SELF_EMP ,
                          "Employed (not Self)" = SdataDT$TOT_EMP,
                          "Mean Hourly Wage" = SdataDT$H_MEAN, 
                          "Mean Annual Income" = SdataDT$A_MEAN)
          fit1 <- glm(y_var ~ SdataDT$year,na.action = na.omit)
          futureYear <- data.table(year = c(2015:(2015+input$obs)))
          fit1_pred <- predict(fit1,futureYear,type="response")
          (paste("The predicted value for", y_name, "for",input$names, "in", input$obs, "years is", round(fit1_pred[input$obs+1],2),"(Note that if there are too few data points, forecast is NA.)"))
          
     })
     
})

