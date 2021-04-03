source("stats.R")

library(shiny)
library(shinydashboard)

ui <- dashboardPage (
  dashboardHeader(title = "Banana Peel Stats"), 
  dashboardSidebar(
      sidebarMenu(
          menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
          menuItem("Details", tabName = "details", icon = icon("chart-line")),
          menuItem("GitHub Repository", icon = icon("file-code-o"), href = "https://github.com/msyphus/bananas")
          )
      ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type= "text/css", href = "style.css")
    ),
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("peelProportion"),
                valueBoxOutput("vendor")
              ),
              fluidRow(
                valueBoxOutput("ripeness"),
                valueBoxOutput("season")
              )
      ),
      tabItem(tabName = "details", 
              fluidRow(
                box(h1("Reference Sample:"), class = "titleBox", width = 3),
                box(h4(refPlotText), br(), h4(refCIText), width = 3),
                box(plotOutput("refBell"), width = 3),
                verbatimTextOutput("refPrint")
              ),
              fluidRow(
                box(h1("One Sample T Test:"), class = "titleBox", width = 3),
                box(h4(oneTPlotText), br(), h4(oneTText), width = 3),
                box(plotOutput("oneTBell"), width = 3),
                verbatimTextOutput("oneTPrint")
              ),
              fluidRow(
                box(h1("Two Sample T Test:"), class = "titleBox", width = 3),
                box(h4(twoTPlotText), br(), h4(twoTText), width = 3),
                box(plotOutput("twoTBell"), width = 3),
                verbatimTextOutput("twoTPrint")
              ),
              fluidRow(
                box(h1("F Test:"), class = "titleBox", width = 3),
                box(h4(fPlotText), br(), h4(fText), width = 3),
                box(plotOutput("fBell"), width = 3),
                verbatimTextOutput("fPrint")
              ),
              fluidRow(
                box(h1("Paired T Test:"), class = "titleBox", width = 3),
                box(h4(pairedPlotText), br(), h4(pairedText), width = 3),
                box(plotOutput("pairedBell"), width = 3),
                verbatimTextOutput("pairedPrint")
              ),
              fluidRow(
                box(h1("One-Way ANOVA Test:"), class = "titleBox", width = 3),
                box(h4(anovaPlotText), br(), h4(anovaText), width = 3),
                box(plotOutput("anovaPlot"), width = 3),
                verbatimTextOutput("anovaPrint")
              ),
              fluidRow(
                box(h1("Two-Way ANOVA Test:"), class = "titleBox", width = 3),
                box(h4(anova2PlotText), br(), h4(anova2Text), width = 3),
                box(plotOutput("anova2Plot"), width = 3),
                verbatimTextOutput("anova2Print")
              ),
              fluidRow(
                box(h1("Regression Test:"), class = "titleBox", width = 3),
                box(h4(regText), br(), h4(regText2), br(), h4(regText3), width = 3),
                box(plotOutput("regPlot"), width = 3),
                verbatimTextOutput("regPrint")
              )
      )
    )
  )
        
)