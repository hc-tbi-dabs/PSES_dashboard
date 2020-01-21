library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape2)

# csv files (non-webscraping) -------------------------------------------------

qtext <- read.csv("Data/2019_PSES_Supporting_Documentation_Document_de_reference_du_SAFF_2019.csv",
                  header=TRUE)
colnames(qtext)[1] <- "Qnum"
data1 <- read.csv("Data/2019_PSES_SAFF_ subset-1_Sous-ensemble-1.csv", header=TRUE)
data1 <- data1[data1$LEVEL1ID==0 | data1$LEVEL1ID==6,]

# -----------------------------------------------------------------------------

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
          id="selectorp1",
          style="margin:20px 20px 50px 30px",
          selectInput(inputId="yearp1", label="Year:",
                      c("2019"="2019", "2017"="2017", "2014"="2014",
                        "2011"="2011", "2008"="2008")),
          actionButton(inputId="expandp1", label="Expand All"),
          actionButton(inputId="collapsep1", label="Collapse All")
        ),
        fluidRow(
          uiOutput(outputId="graphsp1")
        )
      )
    ),
    tabItem(
      tabName="advanced",
      fluidPage(
        titlePanel("Health Canada PSES Results"),
        fluidRow(
          id="selectorp2",
          style="margin:20px 30px 20px 30px;",
          selectInput(inputId="yearp2", label="Year:",
                      c("2019"="2019", "2017"="2017", "2014"="2014",
                        "2011"="2011", "2008"="2008")),
          selectInput(inputId="themep2", label="Theme:",
                      c("All"="all")),
          selectInput(inputId="extrap2", label="Stratify by:",
                      c("None"="none")),
          actionButton(inputId="expandp2", label="Expand All"),
          actionButton(inputId="collapsep2", label="Collapse All")
        ),
        fluidRow(
          uiOutput(outputId="graphsp2")
        )
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
