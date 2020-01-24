library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(shinybusy)
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape2)
library(rmarkdown)

# csv files (non-webscraping) -------------------------------------------------

qtext <- read.csv("Data/2019_PSES_Supporting_Documentation_Document_de_reference_du_SAFF_2019.csv",
                  header=TRUE)
colnames(qtext)[1] <- "Qnum"
data1 <- read.csv("Data/2019_PSES_SAFF_ subset-1_Sous-ensemble-1.csv", header=TRUE)
data1 <- data1[data1$LEVEL1ID==0 | data1$LEVEL1ID==6,]

# -----------------------------------------------------------------------------

qIDs <- c(paste0("0",as.character(1:9)),
          as.character(10:17),paste0("18",letters[1:8]),as.character(19:31),
          as.character(33:45),paste0("46",letters[1:8]),as.character(47:58),
          paste0("59",letters[1:7]),paste0("60",letters[1:13]),
          paste0("61",letters[1:9]),paste0("62",letters[1:16]),
          as.character(63:66),paste0("67",letters[1:7]),
          paste0("68",letters[1:14]),paste0("69",letters[1:9]),
          paste0("70",letters[1:16]),as.character(71:73),
          paste0("74",letters[1:20]),as.character(75:87))
qIDs <- paste0("Q",qIDs)
N <- length(qIDs)
toDisplay <- rep(0,N)

ans.sets.en <- list(c("Strongly agree","Somewhat agree","Neither agree nor disagree",
                    "Somewhat disagree","Strongly disagree","Don't know",
                    "Not applicable"),
                  c("Always/almost always","Often","Sometimes","Rarely",
                    "Never/almost never","Don't know","Not applicable"),
                  c("Not at all","To a small extent","To a moderate extent",
                    "To a large extent","To a very large extent","Don't know",
                    "Not applicable"),
                  c("Yes","No","Not sure"),
                  c("To retire","To pursue another position within my department or agency",
                    "To pursue a position in another department or agency",
                    "To pursue a position outside the federal public service",
                    "End of my term, casual or student employment","Other"),
                  c("Yes","No"),
                  c("Very low","Low","Moderate","High","Very high","Don't know",
                    "Not applicable"))
ans.sets.fr <- list(c("Fortement d'accord","Plutôt d'accord","Ni d'accord ni en désaccord",
                    "Plutôt en désaccord","Fortement en désaccord","Ne sais pas",
                    "Ne s'applique pas"),
                  c("Toujours/ Presque toujours","Souvent","Parfois","Rarement",
                    "Jamais/ Presque jamais","Ne sais pas","Ne s'applique pas"),
                  c("Aucunement","Dans une faible mesure","Modérément",
                    "Dans une grande mesure","Dans une très grande mesure",
                    "Ne sais pas","Ne s'applique pas"),
                  c("Oui","Non","Pas certain(e)"),
                  c("Prendre ma retraite",
                    "Occuper un autre poste au sein de mon ministère ou organisme",
                    "Occuper un poste dans un autre ministère ou organisme",
                    "Occuper un poste à l'extérieur de la fonction publique fédérale",
                    "Fin de mon emploi pour une durée déterminée, occasionnel ou étudiant",
                    "Autre"),
                  c("Oui","Non"),
                  c("Très faible","Faible","Modéré","Élevé","Très élevé",
                    "Ne sais pas","Ne s'applique pas"))
ans.type <- c(rep(1,16),rep(2,9),rep(1,18),2,rep(1,7),rep(3,8),rep(1,7),4,5,1,1,
              rep(6,47),1,1,rep(6,48),1,1,rep(3,20),7,2,1,1,6,6,1,3,6,6,1,1,3)
knitr::include_graphics(path = "figures/modal_spinner.png")

# -----------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Translations --------------------------------------------------------------
  
  output$title <- renderText({
    switch(input$language, "en"="PSES Results", "fr"="Résultats du SAFF")
  })
  output$menu1 <- renderText({
    switch(input$language, "en"="Full Results by Year",
           "fr"="Résultats complets par année")
  })
  output$menu2 <- renderText({
    switch(input$language, "en"="Search with Criteria",
           "fr"="Rechercher avec critères")
  })
  output$menu3 <- renderText({
    switch(input$language, "en"="About PSES",
           "fr"="À propos du SAFF")
  })
  output$displng <- renderText({
    switch(input$language, "en"="Display language:",
           "fr"="Langue d'affichage:")
  })
  output$toptxt <- renderText({
    switch(input$language, "en"="Back to top",
           "fr"="Haut de page")
  })
  output$titlep1 <- renderText({
    switch(input$language, "en"="Health Canada PSES Results",
           "fr"="Résultats du SAFF pour Santé Canada")
  })
  output$titlep2 <- renderText({
    switch(input$language, "en"="Health Canada PSES Results",
           "fr"="Résultats du SAFF pour Santé Canada")
  })
  output$titlep3 <- renderText({
    switch(input$language, "en"="About PSES",
           "fr"="À propos du SAFF")
  })
  output$yrtxtp1 <- renderText({
    switch(input$language, "en"="Year:","fr"="Année:")
  })
  output$exptxtp1 <- renderText({
    switch(input$language, "en"="Expand All",
           "fr"="Développer tout")
  })
  output$collpstxtp1 <- renderText({
    switch(input$language, "en"="Collapse All",
           "fr"="Réduire tout")
  })
  output$exptxtp2 <- renderText({
    switch(input$language, "en"="Expand All",
           "fr"="Développer tout")
  })
  output$collpstxtp2 <- renderText({
    switch(input$language, "en"="Collapse All",
           "fr"="Réduire tout")
  })
  output$yrtxtp2 <- renderText({
    switch(input$language, "en"="Year:","fr"="Année:")
  })
  output$thmtxtp2 <- renderText({
    switch(input$language, "en"="Theme:","fr"="Thème:")
  })
  output$strttxtp2 <- renderText({
    switch(input$language, "en"="Stratify by:","fr"="Stratifier par:")
  })
  output$themeselector <- renderUI({
    tagList(
      if(input$language == "en") {
        selectInput(inputId="themep2", label=textOutput(outputId="thmtxtp2"),
                    c("All"="all"))
      } else {
        selectInput(inputId="themep2", label=textOutput(outputId="thmtxtp2"),
                    c("Tout"="all"))
      }
    )
  })
  output$stratselector <- renderUI({
    tagList(
      if(input$language == "en") {
        selectInput(inputId="extrap2", label=textOutput(outputId="strttxtp2"),
                    c("None"="none"))
      } else {
        selectInput(inputId="extrap2", label=textOutput(outputId="strttxtp2"),
                    c("Aucun"="none"))
      }
    )
  })
  output$firsttxtp3 <- renderText({
    switch(input$language, 
           "en"="The Public Service Employee Survey (PSES) is a survey conducted 
                  by the Treasury Board of Canada. Its objective is to measure the
                  opinions of federal public servants on their engagement, leadership,
                  workforce, workplace, workplace well-being and compensation.",
           "fr"="Le Sondage auprès des fonctionnaires fédéraux (SAFF) est un sondage
                  mené par le Conseil du Trésor du Canada. Son objectif est de mesurer 
                  les opinions des fonctionnaires fédéraux concernant leur mobilisation,
                  le leadership, l’effectif, le milieu de travail, le bien-être en
                  milieu de travail et la rémunération.")
  })
  output$secondtxtp3 <- renderUI({
    tagList(
      if(input$language == "en") {
        p("Click",
          tags$a(href="https://www.canada.ca/en/treasury-board-secretariat/services/innovation/public-service-employee-survey.html",
                 style="",
                 target="_blank",
                 "here"),
          "to learn more."
        )
      } else {
        p("Cliquez",
          tags$a(href="https://www.canada.ca/fr/secretariat-conseil-tresor/services/innovation/sondage-fonctionnaires-federaux.html",
                 style="",
                 target="_blank",
                 "ici"),
          "pour en apprendre plus."
        )
      }
    )
  })
  
  # ---------------------------------------------------------------------------
  
  observeEvent(input$expandp1,{
    for(i in 1:N) {
      js$expand(paste0("b",i))
    }
  })
  observeEvent(input$collapsep1,{
    for(i in 1:N) {
      js$collapse(paste0("b",i))
    }
  })
  observeEvent(input$top,{
    js$toTop()
  })
  output$graphsp1 <- renderUI({
    data1.f <- data1[data1$SURVEYR==input$yearp1,]
    for(i in 1:N) {
      res <- data1.f[data1.f$QUESTION==qIDs[i],]
      if(nrow(res) == 0) {
        toDisplay[i] = 0
      } else {
        toDisplay[i] = 1
      }
    }
    qs <- which(toDisplay==1)
    lapply(qs, function(q) {
      if (input$language=="en") {
        qtitle <- qtext[qtext$Qnum==qIDs[q],"English"]
      } else {
        qtitle <- qtext[qtext$Qnum==qIDs[q],"Français"]
      }
      if (q < 10) { # this condition is only here for testing purposes
      
      res <- data1.f[data1.f$QUESTION==qIDs[q],]
      v <- c()
      n <- length(ans.sets.en[[ans.type[q]]])
      for(s in paste0("ANSWER",1:n)) {
        v <- c(v,res[,s])
      }
      
      if (input$language == "en") {
        df <- structure(v,
                        .Dim=c(nrow(res),n),
                        .Dimnames=list(c("Public Service","Health Canada"),
                                       ans.sets.en[[ans.type[q]]]))
      } else {
        df <- structure(v,
                        .Dim=c(nrow(res),n),
                        .Dimnames=list(c("Fonction publique","Santé Canada"),
                                       ans.sets.fr[[ans.type[q]]]))
      }
      
      df.m <- melt(df)
      df.m <- rename(df.m, Unit = Var1, Responses = Var2, Proportion = value)
      
      box(id=paste0("b",q),title=qtitle,status="primary",solidHeader=TRUE,
          width=12,collapsible=TRUE,collapsed=TRUE,
          render_delayed({
            txt <- switch(input$language,
                          "en"="(Percentages may not add to 100 due to rounding)",
                          "fr"="(Les pourcentages peuvent ne pas totaliser 100
                                en raison d'erreurs dans les arrondissements)")
            p(txt)
          }),
          renderPlot(height=200, {
            ggplot(df.m, aes(x=Unit, y=Proportion, fill=Responses)) +
              geom_bar(stat="identity", position=position_stack(reverse = TRUE)) +
              labs(x=switch(input$language,"en"="Department","fr"="Département"),
                   y=switch(input$language, "en"="Proportion responded (%)",
                            "fr"="Pourcentage répondu (%)"),
                   fill=switch(input$language, "en"="Responses",
                               "fr"="Réponses")) +
              geom_text(size=3, position=position_stack(vjust=0.5, reverse=TRUE),
                        aes(label=Proportion)) +
              coord_flip()
          }),
          renderTable(rownames=TRUE, align="c", width="100%", {
            dtb <- data.frame(res[2,"ANSCOUNT"], res[1,"ANSCOUNT"],
                              row.names=switch(input$language,
                                               "en"="Number of Responses",
                                               "fr"="Nombre de réponses"))
            names(dtb) <- c(switch(input$language,
                                   "en"="Health Canada","fr"="Santé Canada"),
                            switch(input$language,
                                   "en"="Public Service","fr"="Fonction publique"))
            return(dtb)
          }))
      }
    })
  })
}
