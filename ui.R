
## ~~~~~~~~~~~~ UI ~~~~~~~~~~~~~ ##

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)

## ~~~~ Data files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

themes <- read.csv(
  "data/lookups/PSES_SAFF-Themes_Thèmes.csv", header=TRUE, encoding="UTF-8",
  col.names=c("THEME_ID","THEME_EN","THEME_FR"), stringsAsFactors=FALSE)

## ~~~~ CSS formatting ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

#language+ div>.selectize-dropdown {
  background: #1e272c;
  color: #ffffff;
  margin-left: 20px
}

#language+ div>.selectize-input {
  background: #1e272c;
  color: #ffffff;
  border-color: #1e272c;
  width: 160px;
  margin: 10px 0px 0px 20px;
}

#language+ div>.selectize-input:after {
  right: 10px;
  border: 6px solid transparent;
  border-color: #fff transparent transparent transparent;
}"

## ~~~~ Shinyjs functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

jscode <- "
shinyjs.toTop = function() {
  window.scrollTo(0, 0);
}

shinyjs.showEng = function() {
  $('body').find('.sidebar-menu').find('[data-value=compare_en]').show();
  $('body').find('.sidebar-menu').find('[data-value=full_en]').show();
  $('body').find('.sidebar-menu').find('[data-value=about_en]').show();
  $('body').find('.sidebar-menu').find('[data-value=compare_fr]').hide();
  $('body').find('.sidebar-menu').find('[data-value=full_fr]').hide();
  $('body').find('.sidebar-menu').find('[data-value=about_fr]').hide();
  $('body').find('.sidebar-menu').find('[data-value=compare_en]').click();
}

shinyjs.showFr = function() {
  $('body').find('.sidebar-menu').find('[data-value=compare_en]').hide();
  $('body').find('.sidebar-menu').find('[data-value=full_en]').hide();
  $('body').find('.sidebar-menu').find('[data-value=about_en]').hide();
  $('body').find('.sidebar-menu').find('[data-value=compare_fr]').show();
  $('body').find('.sidebar-menu').find('[data-value=full_fr]').show();
  $('body').find('.sidebar-menu').find('[data-value=about_fr]').show();
  $('body').find('.sidebar-menu').find('[data-value=compare_fr]').click();
}

shinyjs.showMainContent = function() {
  $('body').find('#loadingscrn').hide();
  $('body').find('#mainscrn').show();
}"

## ~~~~ Dashboard components (header, sidebar, body) ~~~~~~~~~~~~~~~~~~~~~~~~~~

header <- dashboardHeader(title = textOutput(outputId="title"))

sidebar <- dashboardSidebar(
  sidebarMenu(
    id="tabs",
    menuItem("Compare Data", tabName="compare_en", icon=icon("project-diagram"), selected=TRUE),
    menuItem("Full Results by Year", tabName="full_en", icon=icon("map")),
    menuItem("About PSES", tabName="about_en",icon=icon("question")),
    menuItem("Comparer les données", tabName="compare_fr", icon=icon("project-diagram")),
    menuItem("Résultats complets par année", tabName="full_fr", icon=icon("map")),
    menuItem("À propos du SAFF", tabName="about_fr", icon=icon("question"))
  ),
  uiOutput(outputId="langselector"),
  absolutePanel(
    bottom=10, left=50, style="opacity:0.8;", fixed=TRUE, draggable = FALSE,
    actionButton(inputId="top", label=textOutput(outputId="toptxt"))
  ))

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName="compare_en",
      fluidPage(
        titlePanel("Compare Data for ROEB"),
        fluidRow(
        ))),
    tabItem(
      tabName="full_en",
      fluidPage(
        titlePanel("Full Results by Year"),
        fluidRow(
          id="selectorp2",
          style="margin:20px 30px 30px 30px;",
          div(
            style="display:inline-block;",
            selectInput(inputId="yearp2",label="Year:",c("2019"="2019", "2018"="2018", "2017"="2017"),width="80px")),
          div(
            style="display:inline-block; padding-left:5px;",
            selectInput(inputId="themep2",label="Theme:",themes$THEME_EN,width="550px"))
        ),
        fluidRow(
          uiOutput(outputId="graphsp2") %>% withSpinner(color="#777777")
        ))),
    tabItem(
      tabName="about_en",
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
        ))),
    tabItem(
      tabName="compare_fr",
      fluidPage(
        titlePanel("Comparer les données pour la DGORAL"),
        fluidRow(
        ))),
    tabItem(
      tabName="full_fr",
      fluidPage(
        titlePanel("Résultats complets par année"),
        fluidRow(
          id="selectorp5",
          style="margin:20px 30px 30px 30px;",
          div(
            style="display:inline-block;",
            selectInput(inputId="yearp5",label="Année:",c("2019"="2019", "2018"="2018", "2017"="2017"),width="80px")),
          div(
            style="display:inline-block; padding-left:5px;",
            selectInput(inputId="themep5",label="Thème:",themes$THEME_FR,width="550px"))
        ),
        fluidRow(
          uiOutput(outputId="graphsp5") %>% withSpinner(color="#777777")
        ))),
    tabItem(
      tabName="about_fr",
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
        )))))

ui <- tagList(
  useShinyjs(),
  extendShinyjs(text=jscode),
  inlineCSS(appCSS),
  div(id="mainscrn", style="display:none;", dashboardPage(title="PSES Results/Résultats du SAFF", header, sidebar, body)),
  div(
    id="loadingscrn",
    fluidPage(
      br(style="line-height:100px;"),
      h1("PSES Results / Résultats du SAFF"),
      img(id="spinner", src="spinner.gif"),
      div(id="continuebtns", style="height:151px; display:none;",
          actionButton(inputId="selecteng", label="Continue in English"),
          actionButton(inputId="selectfr", label="Continuer en français")
      ),
      h4("Welcome! / Bienvenue!")
    )))
