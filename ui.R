library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape2)

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

shinyjs.showEng = function() {
$('body').find('.sidebar-menu').find('[data-value=generalen]').show();
$('body').find('.sidebar-menu').find('[data-value=advanceden]').show();
$('body').find('.sidebar-menu').find('[data-value=abouten]').show();
$('body').find('.sidebar-menu').find('[data-value=generalfr]').hide();
$('body').find('.sidebar-menu').find('[data-value=advancedfr]').hide();
$('body').find('.sidebar-menu').find('[data-value=aboutfr]').hide();
$('body').find('.sidebar-menu').find('[data-value=generalen]').click();
}

shinyjs.showFr = function() {
$('body').find('.sidebar-menu').find('[data-value=generalen]').hide();
$('body').find('.sidebar-menu').find('[data-value=advanceden]').hide();
$('body').find('.sidebar-menu').find('[data-value=abouten]').hide();
$('body').find('.sidebar-menu').find('[data-value=generalfr]').show();
$('body').find('.sidebar-menu').find('[data-value=advancedfr]').show();
$('body').find('.sidebar-menu').find('[data-value=aboutfr]').show();
$('body').find('.sidebar-menu').find('[data-value=generalfr]').click();
}

shinyjs.showMainContent = function() {
$('body').find('#loadingscrn').hide();
$('body').find('#mainscrn').show();
}
"

appCSS <- "
#loadingscrn {
position: absolute;
background: #3c8dbc;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #ffffff;
}

#selecteng, #selectfr {
margin: 50px 5px 60px 5px;
color: #444444;
font-size: 20px;
}
"

header <- dashboardHeader(
  title = textOutput(outputId="title")
)

sidebar <- dashboardSidebar(
  tags$head(
    tags$style(
      HTML("#language+ div>.selectize-dropdown{
            background: #1e272c;
            color: #ffffff;
            margin-left: 20px}

            #language+ div>.selectize-input{
            background: #1e272c;
            color: #ffffff;
            border-color: #1e272c;
            width: 160px;
            margin: 10px 0px 0px 20px;}

            #language+ div>.selectize-input:after{
            right: 10px;
            border: 6px solid transparent;
            border-color: #fff transparent transparent transparent;
            }")
    )
  ),
  sidebarMenu(id="tabs",
              menuItem("Full Results by Year",
                       tabName="generalen", icon=icon("map"), selected=TRUE),
              menuItem("Search with Criteria",
                       tabName="advanceden", icon=icon("search")),
              menuItem("About PSES",
                       tabName="abouten",icon=icon("question")),
              menuItem("Résultats complets par année",
                       tabName="generalfr", icon=icon("map")),
              menuItem("Rechercher avec critères",
                       tabName="advancedfr", icon=icon("search")),
              menuItem("À propos du SAFF",
                       tabName="aboutfr", icon=icon("question"))
  ),
  # selectInput(inputId="language", label=textOutput(outputId="displng"),
  #             c("English"="en", "Français"="fr")),
  uiOutput(outputId="langselector"),
  absolutePanel(
    bottom=10, left=50, style="opacity:0.8;", fixed=TRUE,
    draggable = FALSE,
    actionButton(inputId="top", label=textOutput(outputId="toptxt"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName="generalen",
      fluidPage(
        titlePanel("Health Canada PSES Results"),
        fluidRow(
          id="selectorp1",
          style="margin:20px 20px 50px 30px;",
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
      tabName="advanceden",
      fluidPage(
        titlePanel("Health Canada PSES Results"),
        fluidRow(
          id="selectorp2",
          style="margin:20px 20px 50px 30px;",
          selectInput(inputId="yearp2", label="Year:",
                      c("2019"="2019", "2017"="2017", "2014"="2014",
                        "2011"="2011", "2008"="2008")),
          selectInput(inputId="themep2", label="Theme:",
                      c("All"="all", "Employee engagement"="1", "Leadership"="2",
                        "Workforce"="3","Workplace"="4","Workplace well-being"="5",
                        "Compensation"="6")),
          selectInput(inputId="extrap2", label="Stratify by:",
                      c("None"="none")),
          actionButton(inputId="expandp2", label="Expand All"),
          actionButton(inputId="collapsep2", label="Collapse All")
        ),
        fluidRow(
          uiOutput(outputId="graphsp2") %>% withSpinner(color="#777777")
        )
      )
    ),
    tabItem(
      tabName="abouten",
      fluidPage(
        titlePanel("About PSES"),
        p("The Public Service Employee Survey (PSES) is a survey conducted 
          by the Treasury Board of Canada. Its objective is to measure the
          opinions of federal public servants on their engagement, leadership,
          workforce, workplace, workplace well-being and compensation."),
        p("Click",
          tags$a(href="https://www.canada.ca/en/treasury-board-secretariat/services/innovation/public-service-employee-survey.html",
                 style="",
                 target="_blank",
                 "here"),
          "to learn more."
        )
      )
    ),
    tabItem(
      tabName="generalfr",
      fluidPage(
        titlePanel("Résultats du SAFF pour Santé Canada"),
        fluidRow(
          id="selectorp4",
          style="margin:20px 20px 50px 30px;",
          selectInput(inputId="yearp4", label="Année:",
                      c("2019"="2019", "2017"="2017", "2014"="2014",
                        "2011"="2011", "2008"="2008")),
          actionButton(inputId="expandp4", label="Développer tout"),
          actionButton(inputId="collapsep4", label="Réduire tout")
        ),
        fluidRow(
          uiOutput(outputId="graphsp4") %>% withSpinner(color="#777777")
        )
      )
    ),
    tabItem(
      tabName="advancedfr",
      fluidPage(
        titlePanel("Résultats du SAFF pour Santé Canada"),
        fluidRow(
          id="selectorp5",
          style="margin:20px 20px 50px 30px;",
          selectInput(inputId="yearp5", label="Année:",
                      c("2019"="2019", "2017"="2017", "2014"="2014",
                        "2011"="2011", "2008"="2008")),
          selectInput(inputId="themep5", label="Thème:",
                      c("Tout"="all", "Mobilisation des employés"="1",
                        "Leadership"="2","Effectif"="3","Milieu de travail"="4",
                        "Bien-être en milieu de travail"="5", "Rénumération"="6")),
          selectInput(inputId="extrap5", label="Stratifier par:",
                      c("Aucun"="none")),
          actionButton(inputId="expandp5", label="Développer tout"),
          actionButton(inputId="collapsep5", label="Réduire tout")
        ),
        fluidRow(
          uiOutput(outputId="graphsp5") %>% withSpinner(color="#777777")
        )
      )
    ),
    tabItem(
      tabName="aboutfr",
      fluidPage(
        titlePanel("À propos du SAFF"),
        p("Le Sondage auprès des fonctionnaires fédéraux (SAFF) est un sondage
          mené par le Conseil du Trésor du Canada. Son objectif est de mesurer 
          les opinions des fonctionnaires fédéraux concernant leur mobilisation,
          le leadership, l’effectif, le milieu de travail, le bien-être en
          milieu de travail et la rémunération."),
        p("Cliquez",
          tags$a(href="https://www.canada.ca/fr/secretariat-conseil-tresor/services/innovation/sondage-fonctionnaires-federaux.html",
                  style="",
                  target="_blank",
                  "ici"),
          "pour en apprendre plus."
        )
      )
    )
  )
)

ui <- tagList(
  useShinyjs(),
  extendShinyjs(text=jscode),
  inlineCSS(appCSS),
  div(id="mainscrn",
      style="display:none;",
      dashboardPage(
        title = "PSES Results/Résultats du SAFF",
        header,
        sidebar,
        body)
  ),
  div(id="loadingscrn",
      fluidPage(
        br(style="line-height:100px;"),
        h1("PSES Results / Résultats du SAFF"),
        img(id="spinner", src="spinner.gif"),
        div(id="continuebtns", style="height:151px; display:none;",
          actionButton(inputId="selecteng", label="Continue in English"),
          actionButton(inputId="selectfr", label="Continuer en français")
        ),
        h4("Welcome! / Bienvenue!")
      )
  )
)
