source("stats.R")

library(shiny)
library(shinydashboard)

server <- function(input, output) {
  output$peelProportion <- renderValueBox({
    valueBox(refAvg, "Percent of Banana Weight That Is Peel", color = "green", icon = icon("thumbs-up"))
  })
  
  output$vendor <- renderValueBox({
    valueBox("Yes", "Vendor Makes a Difference?", color = "red", icon = icon("thumbs-down"))
  })
  
  output$ripeness <- renderValueBox({
    valueBox("No", "Ripeness Makes a Difference?", color = "green", icon = icon("thumbs-up"))
  })
  
  output$season <- renderValueBox({
    valueBox("No", "Season Makes a Difference?", color = "green", icon = icon("thumbs-up"))
  })
  
  output$refBell <- renderPlot(refPlot())
  
  output$refPrint <- renderPrint(refCI)
  
  output$oneTBell <- renderPlot(oneTPlot())
  
  output$oneTPrint <- renderPrint(oneSampT)
  
  output$twoTBell <- renderPlot(twoTPlot())
  
  output$twoTPrint <- renderPrint(twoSampT)
  
  output$fBell <- renderPlot(twoTPlot())
  
  output$fPrint <- renderPrint(fTest)
  
  output$pairedBell <- renderPlot(pairedPlot())
  
  output$pairedPrint <- renderPrint(pairedT)
  
  output$anovaPlot <- renderPlot(anovaBoxplot())
  
  output$anovaPrint <- renderPrint(aovTest)
  
  output$anova2Plot <- renderPlot(anova2Boxplot())
  
  output$anova2Print <- renderPrint(aov2Test)
  
  output$regPlot <- renderPlot(linPlot())
  
  output$regPrint <- renderPrint(linMod)
  
}