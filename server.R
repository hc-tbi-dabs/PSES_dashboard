
## ~~~~~~~~~~~~ SERVER ~~~~~~~~~~~~~ ##

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape2)
library(rmarkdown)
options(warn=-1)

## ~~~~ Data Files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

questions <- read.csv("data/lookups/PSES_SAFF-Questions.csv", header=TRUE, encoding="UTF-8", col.names=c("ID","ANS_TYPE","IS_REV","EN","FR","IND_EN","IND_FR","IND_ID","SUBIND_EN","SUBIND_FR","SUBIND_ID"), stringsAsFactors=FALSE)
themes <- read.csv("data/lookups/PSES_SAFF-Themes_Thèmes.csv", header=TRUE, encoding="UTF-8", col.names=c("THEME_ID","THEME_EN","THEME_FR"), stringsAsFactors=FALSE)
answers <- read.csv("data/lookups/PSES_SAFF-Answers_Réponses.csv", header=TRUE, encoding="UTF-8", col.names=c(paste0("TYPE",1:7,"_EN"), paste0("TYPE",1:7,"_FR")), stringsAsFactors=FALSE)
data <- read.csv("data/2019_2018_2017-PSES_SAFF-ROEB_DGORAL-Full_data_Données_complètes.csv", header=TRUE, encoding="UTF-8", stringsAsFactors=FALSE)
data <- data[,2:ncol(data)]

## ~~~~ Global data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

QIDS <- unique(questions$ID)
N <- length(QIDS)

toDisplay <- rep(0,N)

# change the variable names here
recolourBars <- function(q, p, lang) {
  atype <- questions[questions$ID==q,"ANS_TYPE"][1]
  is.reversed <- questions[questions$ID==q,"IS_REV"][1]
  if(lang=="en"){ ans.set <- answers[,paste0("TYPE",atype,"_EN")] }
  else{ ans.set <- answers[,paste0("TYPE",atype,"_FR")] }
  ans.set <- ans.set[ans.set!=""]
  
  if(atype %in% c(1,3,7)){ p <- p+scale_fill_manual(breaks=ans.set,values=c("steelblue3","lightskyblue","azure1","peachpuff","lightsalmon","grey92","grey80")) }
  else if(atype==2 & is.reversed){ p <- p+scale_fill_manual(breaks=ans.set,values=c("lightsalmon","peachpuff","azure1","lightskyblue","steelblue3","grey92","grey80")) }
    else if(atype==2){ p <- p+scale_fill_manual(breaks=ans.set,values=c("steelblue3","lightskyblue","azure1","peachpuff","lightsalmon","grey92","grey80")) }
  else if(atype==4){ p <- p+scale_fill_manual(breaks=ans.set,values=c("lightsalmon","steelblue3","gray90")) }
  else if(atype==6 & is.reversed){ p <- p+scale_fill_manual(breaks=ans.set,values=c("lightsalmon","steelblue3")) }
    else if(atype==6){ p <- p+scale_fill_manual(breaks=ans.set,values=c("steelblue3","lightsalmon")) }
  else if(atype==5){ p <- p+scale_fill_manual(breaks=ans.set,values=c("steelblue3","lightskyblue","lightblue1","aquamarine","darkseagreen1","lightgreen","palegreen3")) }
  
  return(p)
}

## ~~~~ Main server function ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

server <- function(input, output, session) {
  rv <- reactiveValues(default.lang = NA)
  
  # ---- Translations ---------------------------------------------------------
  
  output$title <- renderText({ req(input$language); switch(input$language,"en"="PSES Results","fr"="Résultats du SAFF") })
  output$displng <- renderText({ req(input$language); switch(input$language,"en"="Display language:","fr"="Langue d'affichage:") })
  output$toptxt <- renderText({ req(input$language); switch(input$language,"en"="Back to top","fr"="Haut de page") })
  output$langselector <- renderUI({
    req(rv$default.lang)
    selectInput(inputId="language", label=textOutput(outputId="displng"), c("English"="en","Français"="fr"), selected=rv$default.lang) })
  
  # ---- Individual observers -------------------------------------------------
  
  observeEvent(input$selecteng,{ rv$default.lang <- "en"; js$showMainContent(); js$showEng() })
  observeEvent(input$selectfr,{ rv$default.lang <- "fr"; js$showMainContent();  js$showFr() })
  observeEvent(input$top,{ js$toTop() })
  observeEvent(input$language, {
    if(input$language=="en") { js$showEng() }
    else { js$showFr() } })
  
  # ---- Plot outputs ---------------------------------------------------------
  
  output$graphsp2 <- renderUI({
    req(input$yearp2)
    req(input$themep2)
    data.f <- data[data$SURVEYR==input$yearp2 & data$THEME_EN==input$themep2,]
    
    qs <- as.character(unique(data.f$QID))
    lapply(qs, function(q) {
      qtitle <- as.character(questions[questions$ID==q,"EN"][1])
      res <- data.f[data.f$QID==q,] # maybe add criterion !is.na(data$ANSWER1)
      ans.set <- answers[,paste0("TYPE",questions[questions$ID==q,"ANS_TYPE"][1],"_EN")]
      ans.set <- ans.set[ans.set!=""]
      
      v <- c()
      n <- length(ans.set)
      for(s in paste0("ANSWER",1:n)) { v <- c(v,res[,s]) }
      df <- structure(v, .Dim=c(nrow(res),n), .Dimnames=list(as.character(res$ORGANIZATION_EN), ans.set))
      df.m <- melt(df)
      df.m <- rename(df.m, Unit=Var1, Responses=Var2, Proportion=value)
      
      box(id=paste0("b",which(QIDS==q)), title=qtitle, status="primary", solidHeader=TRUE, width=12, collapsible=TRUE, collapsed=TRUE,
          render_delayed({ p("(Percentages may not add to 100 due to rounding)") }), # keep?
          renderPlot(height=400, {
            p <- ggplot(df.m, aes(x=Unit, y=Proportion, fill=Responses)) +
              geom_bar(stat="identity", position=position_stack(reverse=TRUE)) +
              labs(x="", y="Proportion responded (%)", fill="Responses") +
              geom_text(size=3, position=position_stack(vjust=0.5, reverse=TRUE), aes(label=Proportion)) +
              coord_flip() +
              theme_minimal()
            return(recolourBars(q,p,"en"))
          }),
          renderTable(rownames=TRUE, align="c", width="100%", {
            dtb <- data.frame(as.list(rev(res$COUNT)), row.names="Number of responses")
            names(dtb) <- rev(as.character(res$ORGANIZATION_EN))
            return(dtb)
          }))
    })})
  
  output$graphsp5 <- renderUI({
    req(input$yearp5)
    req(input$themep5)
    data.f <- data[data$SURVEYR==input$yearp5 & data$THEME_FR==input$themep5,]
    
    qs <- as.character(unique(data.f$QID))
    lapply(qs, function(q) {
      qtitle <- as.character(questions[questions$ID==q,"FR"][1])
      res <- data.f[data.f$QID==q,] # maybe add criterion !is.na(data$ANSWER1)
      ans.set <- answers[,paste0("TYPE",questions[questions$ID==q,"ANS_TYPE"][1],"_FR")]
      ans.set <- ans.set[ans.set!=""]
      
      v <- c()
      n <- length(ans.set)
      for(s in paste0("ANSWER",1:n)) { v <- c(v,res[,s]) }
      df <- structure(v, .Dim=c(nrow(res),n), .Dimnames=list(as.character(res$ORGANIZATION_FR), ans.set))
      df.m <- melt(df)
      df.m <- rename(df.m, Unit=Var1, Responses=Var2, Proportion=value)
      
      box(id=paste0("b",which(QIDS==q)+N), title=qtitle, status="primary", solidHeader=TRUE, width=12, collapsible=TRUE, collapsed=TRUE,
          render_delayed({ p("(Les pourcentages peuvent ne pas totaliser 100 en raison d'erreurs dans les arrondissements)") }), # keep?
          renderPlot(height=400, {
            p <- ggplot(df.m, aes(x=Unit, y=Proportion, fill=Responses)) +
              geom_bar(stat="identity", position=position_stack(reverse=TRUE)) +
              labs(x="", y="Pourcentage répondu (%)", fill="Réponses") +
              geom_text(size=3, position=position_stack(vjust=0.5, reverse=TRUE), aes(label=Proportion)) +
              coord_flip() +
              theme_minimal()
            return(recolourBars(q,p,"fr"))
          }),
          renderTable(rownames=TRUE, align="c", width="100%", {
            dtb <- data.frame(as.list(rev(res$COUNT)), row.names="Nombre de réponses")
            names(dtb) <- rev(as.character(res$ORGANIZATION_FR))
            return(dtb)
          }))
    })
  })
  
  outputOptions(output,"graphsp2",suspendWhenHidden=FALSE)
  outputOptions(output,"graphsp5",suspendWhenHidden=FALSE)
  outputOptions(output,"title",suspendWhenHidden=FALSE)
  outputOptions(output,"displng",suspendWhenHidden=FALSE)
  outputOptions(output,"toptxt",suspendWhenHidden=FALSE)
  outputOptions(output,"langselector",suspendWhenHidden=FALSE)
  
  runjs("$(document).on('shiny:value', function(event){ $('#spinner').hide(); $('#continuebtns').show(); })")
}
