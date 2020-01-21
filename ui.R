library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape2)

sidebar <- dashboardSidebar(
  sidebarMenu(id="tabs",
              menuItem("At a Glance", tabName="general", icon=icon("map"), selected=TRUE),
              menuItem("Search with Criteria", tabName="advanced", icon=icon("search")),
              menuItem("About PSES", tabName="about",icon=icon("question"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName="general",
      fluidPage(
        titlePanel("Health Canada PSES Results"),
        fluidRow(
          id="selector-p1",
          style="margin:20px 30px 20px 30px",
          selectInput(inputId="year-p1", label="Year:",
                      c("2019"="2019", "2017"="2017", "2014"="2014",
                        "2011"="2011", "2008"="2008")),
          actionButton(inputId="expand-p1", label="Expand All"),
          actionButton(inputId="collapse-p1", label="Collapse All")
        ),
        uiOutput(outputId="graphs-p1")
      )
    ),
    tabItem(
      tabName="advanced",
      fluidPage(
        titlePanel("Health Canada PSES Results"),
        fluidRow(
          id="selector-p2",
          style="margin:20px 30px 20px 30px;",
          selectInput(inputId="year-p2", label="Year:",
                      c("2019"="2019", "2017"="2017", "2014"="2014",
                        "2011"="2011", "2008"="2008")),
          selectInput(inputId="theme-p2", label="Theme:",
                      c("All"="all")),
          selectInput(inputId="extra-p2", label="Stratify by:",
                      c("None"="none")),
          actionButton(inputId="expand-p2", label="Expand All"),
          actionButton(inputId="collapse-p2", label="Collapse All")
        ),
        uiOutput(outputId="graphs-p2")
      )
    ),
    tabItem(
      tabName="about",
      fluidPage(
        titlePanel("About PSES"),
        p("The Public Service Employee Survey (PSES) is a survey conducted 
          by the Treasury Board of Canada. Its objective is to measure the
          opinions of federal public servants on their engagement, leadership,
          workforce, workplace, workplace well-being and compensation."),
        p("Click",
          tags$a(href="https://www.canada.ca/en/treasury-board-secretariat/services/innovation/public-service-employee-survey.html",
                 style="",
                 "here"),
          "to learn more."
        )
      )
    )
  )
)

ui <- dashboardPage(
  dashboardHeader(title="PSES Results"),
  sidebar,
  body
)
