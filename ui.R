library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
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

jscode <- "
shinyjs.expand = function(boxid) {
if ($('#' + boxid).closest('.box').hasClass('collapsed-box')) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click(); }
}

shinyjs.collapse = function(boxid) {
if ($('#' + boxid).closest('.box').hasClass('collapsed-box') == false) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click(); }
}

shinyjs.toTop = function() {
window.scrollTo(0, 0);
}

shinyjs.changeHTMLonRender = function(id, newText) {
if ($('#' + id).closest('.box').hasClass('collapsed-box') == false) {
$('#' + id).html(newText); }
}
"

header <- dashboardHeader(
  title = textOutput(outputId="title")
)

sidebar <- dashboardSidebar(
  sidebarMenu(id="tabs",
              menuItem(textOutput(outputId="menu1", inline=TRUE),
                       tabName="general", icon=icon("map"), selected=TRUE),
              menuItem(textOutput(outputId="menu2", inline=TRUE),
                       tabName="advanced", icon=icon("search")),
              menuItem(textOutput(outputId="menu3", inline=TRUE),
                       tabName="about",icon=icon("question"))
  ),
  selectInput(inputId="language", label=textOutput(outputId="displng"),
                c("English"="en", "Français"="fr")),
  absolutePanel(
    bottom=10, left=50, style="opacity:0.8;", fixed=TRUE,
    draggable = FALSE,
    actionButton(inputId="top", label=textOutput(outputId="toptxt"))
  )
)

body <- dashboardBody(
  useShinyjs(),
  extendShinyjs(text=jscode),
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
          uiOutput(outputId="graphsp1") %>% withSpinner(color="#777777")
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
  header,
  sidebar,
  body
)
