library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
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

# -----------------------------------------------------------------------------

server <- function(input, output, session) {
  
  rv <- reactiveValues(default.lang = NA)
  
  # Translations --------------------------------------------------------------
  
  output$title <- renderText({
    req(input$language)
    switch(input$language, "en"="PSES Results", "fr"="Résultats du SAFF")
  })
  output$displng <- renderText({
    req(input$language)
    switch(input$language, "en"="Display language:",
           "fr"="Langue d'affichage:")
  })
  output$toptxt <- renderText({
    req(input$language)
    switch(input$language, "en"="Back to top",
           "fr"="Haut de page")
  })
  output$langselector <- renderUI({
    req(rv$default.lang)
    selectInput(inputId="language", label=textOutput(outputId="displng"),
                c("English"="en", "Français"="fr"), selected=rv$default.lang)
  })
  
  # Individual observers ------------------------------------------------------
  
  observeEvent(input$selecteng,{
    rv$default.lang <- "en"
    js$showMainContent()
    js$showEng()
  })
  observeEvent(input$selectfr,{
    rv$default.lang <- "fr"
    js$showMainContent()
    js$showFr()
  })
  observeEvent(input$expandp1,{
    for(i in 1:N) { js$expand(paste0("b",i)) }
  })
  observeEvent(input$expandp1,{
    for(i in 1:N) { js$expand(paste0("b",i)) }
  })
  observeEvent(input$collapsep1,{
    for(i in 1:N) { js$collapse(paste0("b",i)) }
  })
  observeEvent(input$expandp2,{
    for(i in N+(1:N)) { js$expand(paste0("b",i)) }
  })
  observeEvent(input$collapsep2,{
    for(i in N+(1:N)) { js$collapse(paste0("b",i)) }
  })
  observeEvent(input$expandp4,{
    for(i in (2*N)+(1:N)) { js$expand(paste0("b",i)) }
  })
  observeEvent(input$collapsep4,{
    for(i in (2*N)+(1:N)) { js$collapse(paste0("b",i)) }
  })
  observeEvent(input$expandp5,{
    for(i in (3*N)+(1:N)) { js$expand(paste0("b",i)) }
  })
  observeEvent(input$collapsep5,{
    for(i in (3*N)+(1:N)) { js$collapse(paste0("b",i)) }
  })
  observeEvent(input$top,{
    js$toTop()
  })
  observeEvent(input$language, {
    if(input$language=="en") {
      js$showEng()
    } else {
      js$showFr()
    }
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
      qtitle <- qtext[qtext$Qnum==qIDs[q],"English"]
      if (q < 10) { # this condition is only here for testing purposes
      
      res <- data1.f[data1.f$QUESTION==qIDs[q],]
      v <- c()
      n <- length(ans.sets.en[[ans.type[q]]])
      for(s in paste0("ANSWER",1:n)) {
        v <- c(v,res[,s])
      }
      
      df <- structure(v,
                      .Dim=c(nrow(res),n),
                      .Dimnames=list(c("Public Service","Health Canada"),
                                     ans.sets.en[[ans.type[q]]]))
      
      df.m <- melt(df)
      df.m <- rename(df.m, Unit = Var1, Responses = Var2, Proportion = value)
      
      box(id=paste0("b",q),title=qtitle,status="primary",solidHeader=TRUE,
          width=12,collapsible=TRUE,collapsed=TRUE,
          render_delayed({
            p("(Percentages may not add to 100 due to rounding)")
          }),
          renderPlot(height=200, {
            ggplot(df.m, aes(x=Unit, y=Proportion, fill=Responses)) +
              geom_bar(stat="identity", position=position_stack(reverse = TRUE)) +
              labs(x="", y="Proportion responded (%)",
                   fill="Responses") +
              geom_text(size=3, position=position_stack(vjust=0.5, reverse=TRUE),
                        aes(label=Proportion)) +
              coord_flip() +
              theme_minimal()
          }),
          renderTable(rownames=TRUE, align="c", width="100%", {
            dtb <- data.frame(res[2,"ANSCOUNT"], res[1,"ANSCOUNT"],
                              row.names="Number of responses")
            names(dtb) <- c("Health Canada","Public Service")
            return(dtb)
          }))
      }
    })
  })
  output$graphsp2 <- renderUI({
    req(input$themep2)
    data1.f <- data1[data1$SURVEYR==input$yearp2,]
    for(i in 1:N) {
      if (input$themep2=="all") {
        res <- data1.f[data1.f$QUESTION==qIDs[i],]
      } else {
        res <- data1.f[data1.f$QUESTION==qIDs[i] & data1.f$INDICATORID==input$themep2,]
      }
      if(nrow(res) == 0) {
        toDisplay[i] = 0
      } else {
        toDisplay[i] = 1
      }
    }
    qs <- which(toDisplay==1)
    lapply(qs, function(q) {
      qtitle <- qtext[qtext$Qnum==qIDs[q],"English"]
      
      if (q < 20) { # this condition is only here for testing purposes
        
        if (input$themep2=="all") {
          res <- data1.f[data1.f$QUESTION==qIDs[q],]
        } else {
          res <- data1.f[data1.f$QUESTION==qIDs[q] & data1.f$INDICATORID==input$themep2,]
        }
        v <- c()
        n <- length(ans.sets.en[[ans.type[q]]])
        for(s in paste0("ANSWER",1:n)) {
          v <- c(v,res[,s])
        }
        
        df <- structure(v,
                        .Dim=c(nrow(res),n),
                        .Dimnames=list(c("Public Service","Health Canada"),
                                       ans.sets.en[[ans.type[q]]]))
        
        df.m <- melt(df)
        df.m <- rename(df.m, Unit = Var1, Responses = Var2, Proportion = value)
        
        box(id=paste0("b",q+N),title=qtitle,status="primary",solidHeader=TRUE,
            width=12,collapsible=TRUE,collapsed=TRUE,
            render_delayed({
              p("(Percentages may not add to 100 due to rounding)")
            }),
            renderPlot(height=200, {
              ggplot(df.m, aes(x=Unit, y=Proportion, fill=Responses)) +
                geom_bar(stat="identity", position=position_stack(reverse = TRUE)) +
                labs(x="", y="Proportion responded (%)",
                     fill="Responses") +
                geom_text(size=3, position=position_stack(vjust=0.5, reverse=TRUE),
                          aes(label=Proportion)) +
                coord_flip() +
                theme_minimal()
            }),
            renderTable(rownames=TRUE, align="c", width="100%", {
              dtb <- data.frame(res[2,"ANSCOUNT"], res[1,"ANSCOUNT"],
                                row.names="Number of responses")
              names(dtb) <- c("Health Canada","Public Service")
              return(dtb)
            }))
      }
    })
  })
  output$graphsp4 <- renderUI({
    data1.f <- data1[data1$SURVEYR==input$yearp4,]
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
      qtitle <- qtext[qtext$Qnum==qIDs[q],"Français"]
      
      if (q < 10) { # this condition is only here for testing purposes
        
        res <- data1.f[data1.f$QUESTION==qIDs[q],]
        v <- c()
        n <- length(ans.sets.en[[ans.type[q]]])
        for(s in paste0("ANSWER",1:n)) {
          v <- c(v,res[,s])
        }
        
        df <- structure(v,
                        .Dim=c(nrow(res),n),
                        .Dimnames=list(c("Fonction publique","Santé Canada"),
                                       ans.sets.fr[[ans.type[q]]]))
        
        df.m <- melt(df)
        df.m <- rename(df.m, Unit = Var1, Responses = Var2, Proportion = value)
        
        box(id=paste0("b",q+2*N),title=qtitle,status="primary",solidHeader=TRUE,
            width=12,collapsible=TRUE,collapsed=TRUE,
            render_delayed({
              p("(Les pourcentages peuvent ne pas totaliser 100 en raison d'erreurs
                dans les arrondissements)")
            }),
            renderPlot(height=200, {
              ggplot(df.m, aes(x=Unit, y=Proportion, fill=Responses)) +
                geom_bar(stat="identity", position=position_stack(reverse = TRUE)) +
                labs(x="Département", y="Pourcentage répondu (%)",
                     fill="Réponses") +
                geom_text(size=3, position=position_stack(vjust=0.5, reverse=TRUE),
                          aes(label=Proportion)) +
                coord_flip()
            }),
            renderTable(rownames=TRUE, align="c", width="100%", {
              dtb <- data.frame(res[2,"ANSCOUNT"], res[1,"ANSCOUNT"],
                                row.names="Nombre de réponses")
              names(dtb) <- c("Santé Canada","Fonction publique")
              return(dtb)
            }))
      }
    })
  })
  output$graphsp5 <- renderUI({
    req(input$themep5)
    data1.f <- data1[data1$SURVEYR==input$yearp5,]
    for(i in 1:N) {
      if (input$themep5=="all") {
        res <- data1.f[data1.f$QUESTION==qIDs[i],]
      } else {
        res <- data1.f[data1.f$QUESTION==qIDs[i] & data1.f$INDICATORID==input$themep5,]
      }
      if(nrow(res) == 0) {
        toDisplay[i] = 0
      } else {
        toDisplay[i] = 1
      }
    }
    qs <- which(toDisplay==1)
    lapply(qs, function(q) {
      qtitle <- qtext[qtext$Qnum==qIDs[q],"Français"]
      
      if (q < 20) { # this condition is only here for testing purposes
        
        if (input$themep5=="all") {
          res <- data1.f[data1.f$QUESTION==qIDs[q],]
        } else {
          res <- data1.f[data1.f$QUESTION==qIDs[q] & data1.f$INDICATORID==input$themep5,]
        }
        v <- c()
        n <- length(ans.sets.en[[ans.type[q]]])
        for(s in paste0("ANSWER",1:n)) {
          v <- c(v,res[,s])
        }
        
        df <- structure(v,
                        .Dim=c(nrow(res),n),
                        .Dimnames=list(c("Fonction publique","Santé Canada"),
                                       ans.sets.fr[[ans.type[q]]]))
        
        df.m <- melt(df)
        df.m <- rename(df.m, Unit = Var1, Responses = Var2, Proportion = value)
        
        box(id=paste0("b",q+3*N),title=qtitle,status="primary",solidHeader=TRUE,
            width=12,collapsible=TRUE,collapsed=TRUE,
            render_delayed({
              p("(Les pourcentages peuvent ne pas totaliser 100 en raison d'erreurs
                dans les arrondissements)")
            }),
            renderPlot(height=200, {
              ggplot(df.m, aes(x=Unit, y=Proportion, fill=Responses)) +
                geom_bar(stat="identity", position=position_stack(reverse = TRUE)) +
                labs(x="Département", y="Pourcentage répondu (%)",
                     fill="Réponses") +
                geom_text(size=3, position=position_stack(vjust=0.5, reverse=TRUE),
                          aes(label=Proportion)) +
                coord_flip()
            }),
            renderTable(rownames=TRUE, align="c", width="100%", {
              dtb <- data.frame(res[2,"ANSCOUNT"], res[1,"ANSCOUNT"],
                                row.names="Nombre de réponses")
              names(dtb) <- c("Santé Canada","Fonction publique")
              return(dtb)
            }))
      }
    })
  })
  
  outputOptions(output, "graphsp1", suspendWhenHidden=FALSE)
  outputOptions(output, "graphsp2", suspendWhenHidden=FALSE)
  outputOptions(output, "graphsp4", suspendWhenHidden=FALSE)
  outputOptions(output, "graphsp5", suspendWhenHidden=FALSE)
  outputOptions(output, "title", suspendWhenHidden=FALSE)
  outputOptions(output, "displng", suspendWhenHidden=FALSE)
  outputOptions(output, "toptxt", suspendWhenHidden=FALSE)
  outputOptions(output, "langselector", suspendWhenHidden=FALSE)
  
  runjs("$(document).on('shiny:value', function(event) {
    $('#spinner').hide();
    $('#continuebtns').show();
    })")
}
