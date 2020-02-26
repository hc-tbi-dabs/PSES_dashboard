
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

## ~~~~ Data Files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

questions <-
  read.csv("data/lookups/PSES_SAFF-Questions.csv",header=TRUE,encoding="UTF-8",
           col.names=c("ID","ANS_TYPE","IS_REV","EN","FR","IND_EN","IND_FR",
                       "IND_ID","SUBIND_EN","SUBIND_FR","SUBIND_ID"),
           stringsAsFactors=FALSE)
themes <- 
  read.csv("data/lookups/PSES_SAFF-Themes_Thèmes.csv",header=TRUE,
           encoding="UTF-8",col.names=c("THEME_ID","THEME_EN","THEME_FR"),
           stringsAsFactors=FALSE)
answers <-
  read.csv("data/lookups/PSES_SAFF-Answers_Réponses.csv",header=TRUE,
           encoding="UTF-8",col.names=c(paste0("TYPE",1:7,"_EN"),
                                        paste0("TYPE",1:7,"_FR")),
           stringsAsFactors=FALSE)
data <-
  read.csv(
    "data/2019_2018_2017-PSES_SAFF-ROEB_DGORAL-Full_data_Données_complètes.csv",
    header=TRUE,encoding="UTF-8",stringsAsFactors=FALSE)
data <- data[,2:ncol(data)]

## ~~~~ Global data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

QIDS <- unique(questions$ID)
N <- length(QIDS)
DIRECTORATES_EN <-
  c("Planning and Operations (POD)"="POD",
    "Medical Devices and Clinical Compliance (MDCCD)"="MDCCD",
    "Health Products Compliance (HPCD)"="HPCD",
    "Laboratories (LABS)"="LABS",
    "Policy and Regulatory Strategies / Assistant Deputy Minister's Office (PRSD/ADMO)"=
      "PRSD and ADMO",
    "Controlled Substances and Environmental Health (EHPD)"="EHPD",
    "Consumer Product Safety, Tobacco, and Pesticides (CPCSD)"="CPCSD",
    "Cannabis (CD)"="CD")
DIRECTORATES_FR <-
  c("Planification et opérations (DPO)"="DPO",
    "Conformité des matériels médicaux et en milieux cliniques (DCMMMC)"=
      "DCMMMC",
    "Conformité des produits de santé (DCPS)"="DCPS",
    "Laboratoires (LABS)"="LABS",
    "Politiques et stratégies réglementaires / Bureau du sous-ministre adjoint (DPSR et BSMA)"=
      "DPSR et BSMA",
    "Substances contrôlées et santé environnementale (DSCSE)"="DSCSE",
    "Sécurité des produits de consommation, tabac et pesticides (DSPCTP)"=
      "DSPCTP",
    "Cannabis (DC)"="DC")
HTML_COLOURS <-
  c("steelblue4"="#37648b",
    "steelblue3"="#4f94cd",
    "lightskyblue"="#8bcef8",
    "lightskyblue1"="#b0e1ff",
    "lightsalmon"="#ffa07a",
    "palegreen3"="#7cce7c",
    "darkseagreen2"="#b4eeb4",
    "snow"="#fefaf9",
    "pink"="#ffc0cb",
    "lightcoral"="#f0807f",
    "white"="#ffffff",
    "firebrick"="#b22222",
    "palegreen4"="#548b54",
    "grey80"="#cccccc",
    "grey70"="#b3b3b3",
    "grey60"="#999999",
    "seagreen"="#2e8a57",
    "aliceblue"="#eff8ff",
    "slategray3"="#a0b6cd",
    "null"="#ebebeb")

recolourBars <- function(q, p, lang) {
  atype <- questions[questions$ID==q,"ANS_TYPE"][1]
  is.reversed <- questions[questions$ID==q,"IS_REV"][1]
  if(lang=="en") {
    ans.set <- answers[,paste0("TYPE",atype,"_EN")] }
  else {
    ans.set <- answers[,paste0("TYPE",atype,"_FR")] }
  ans.set <- ans.set[ans.set!=""]
  
  if(atype %in% c(1,3,7)) {
    p <- p+scale_fill_manual(
      breaks=ans.set,values=c("steelblue3","lightskyblue","azure1","peachpuff",
                              "lightsalmon","grey92","grey80")) }
  else if(atype==2 & is.reversed) {
    p <- p+scale_fill_manual(
      breaks=ans.set,values=c("lightsalmon","peachpuff","azure1","lightskyblue",
                              "steelblue3","grey92","grey80")) }
  else if(atype==2) {
    p <- p+scale_fill_manual(
      breaks=ans.set,values=c("steelblue3","lightskyblue","azure1","peachpuff",
                              "lightsalmon","grey92","grey80")) }
  else if(atype==4) {
    p <- p+scale_fill_manual(
      breaks=ans.set,values=c("lightsalmon","steelblue3","gray90")) }
  else if(atype==6 & is.reversed) {
    p <- p+scale_fill_manual(
      breaks=ans.set,values=c("lightsalmon","steelblue3")) }
  else if(atype==6) {
    p <- p+scale_fill_manual(
      breaks=ans.set,values=c("steelblue3","lightsalmon")) }
  else if(atype==5) {
    p <- p+scale_fill_manual(
      breaks=ans.set,values=c("steelblue3","lightskyblue","lightblue1",
                              "aquamarine","darkseagreen1","lightgreen",
                              "palegreen3")) }
  return(p)
}

## ~~~~ Main server function ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

server <- function(input, output, session) {
  rv <- reactiveValues(default.lang = NA)
  
  # ---- Translations ----------------------------------------------------------
  
  output$title <- renderText({
    req(input$language)
    switch(input$language,"en"="PSES Results","fr"="Résultats du SAFF")})
  output$displng <- renderText({
    req(input$language)
    switch(input$language,"en"="Display language:","fr"="Langue d'affichage:")})
  output$toptxt <- renderText({
    req(input$language)
    switch(input$language,"en"="Back to top","fr"="Haut de page")})
  output$langselector <- renderUI({
    req(rv$default.lang)
    selectInput(inputId="language",label=textOutput(outputId="displng"),
                c("English"="en","Français"="fr"),selected=rv$default.lang)})
  
  # ---- Dynamic content -------------------------------------------------------
  
  output$dir_outputp1 <- renderUI({
    if(input$againstp1 %in% c(2019,2018)) {
      selectInput(inputId="directoratep1",label=NULL,multiple=TRUE,
                  width="600px",DIRECTORATES_EN,
                  selected=unname(DIRECTORATES_EN)) }
    else {
      selectInput(inputId="directoratep1",label=NULL,multiple=TRUE,
                  width="600px",DIRECTORATES_EN[1:(length(DIRECTORATES_EN)-1)],
                  selected=DIRECTORATES_EN[1:(length(DIRECTORATES_EN)-1)]) }
  })
  output$chng1_outputp1 <- renderUI({
    if(input$againstp1==2019) {
      selectInput(inputId="change1p1",label=NULL,c("ROEB"="ROEB"),width="90px")}
    else if(input$againstp1==2018) {
      selectInput(inputId="change1p1",label=NULL,
                  c("ROEB"="ROEB","2018"="2018"),width="90px") }
    else {
      selectInput(inputId="change1p1",label=NULL,
                  c("ROEB"="ROEB","2017"="2017"),width="90px") }
  })
  output$chng3_outputp1 <- renderUI({
    nms <- c("Any directorate",
             names(DIRECTORATES_EN[DIRECTORATES_EN %in% input$directoratep1]))
    vals <- c("any",input$directoratep1)
    names(vals) <- nms
    if(is.null(input$directoratep1)) {
      disabled(selectInput(inputId="change3p1",label=NULL,vals)) }
    else {
      selectInput(inputId="change3p1",label=NULL,vals) }
  })
  output$dir_outputp4 <- renderUI({
    if(input$againstp4 %in% c(2019,2018)) {
      selectInput(inputId="directoratep4",label=NULL,multiple=TRUE,
                  width="629px",DIRECTORATES_FR,
                  selected=unname(DIRECTORATES_FR)) }
    else {
      selectInput(inputId="directoratep4",label=NULL,multiple=TRUE,
                  width="629px",DIRECTORATES_FR[1:(length(DIRECTORATES_FR)-1)],
                  selected=DIRECTORATES_FR[1:(length(DIRECTORATES_FR)-1)]) }})
  output$chng1_outputp4 <- renderUI({
    if(input$againstp4==2019) {
      selectInput(inputId="change1p4",label=NULL,c("DGORAL"="DGORAL"),
                  width="90px")}
    else if(input$againstp4==2018) {
      selectInput(inputId="change1p4",label=NULL,
                  c("DGORAL"="DGORAL","2018"="2018"),width="90px") }
    else {
      selectInput(inputId="change1p4",label=NULL,
                  c("DGORAL"="DGORAL","2017"="2017"),width="90px") }
  })
  output$chng3_outputp4 <- renderUI({
    nms <- c("N'importe quelle direction",
             names(DIRECTORATES_FR[DIRECTORATES_FR %in% input$directoratep4]))
    vals <- c("any",input$directoratep4)
    names(vals) <- nms
    if(is.null(input$directoratep4)) {
      disabled(selectInput(inputId="change3p4",label=NULL,vals)) }
    else {
      selectInput(inputId="change3p4",label=NULL,vals) }
  })
  output$ques_outputp1 <- renderUI({
    if(is.null(input$directoratep1) | is.null(input$change1p1)
       | is.null(input$change3p1)) {
      disabled(selectInput(inputId="questionp1",label=NULL,c(),width="600px")) }
    else{
      data.f <- data[data$SURVEYR %in% c(2019,input$againstp1)
                     & data$ORGANIZATION_EN %in% c("ROEB",input$directoratep1)
                     & data$THEME_EN==input$themep1,]
      allqs <- as.character(unique(data.f$QUESTION_EN))
      qs <- c()
      for(q in allqs) {
        if(!(q %in% data.f[data.f$SURVEYR==2019,"QUESTION_EN"])
           | !(q %in% data.f[data.f$SURVEYR==input$againstp1,"QUESTION_EN"])) {
          next
        }
        if(input$change1p1=="ROEB" & input$change3p1!="any") {
          res <- data.f[data.f$SURVEYR==2019
                        & data.f$ORGANIZATION_EN %in% c("ROEB",input$change3p1)
                        & data.f$QUESTION_EN==q,]
          if(is.na(res[res$ORGANIZATION_EN=="ROEB","POSITIVE"])
             | is.na(res[res$ORGANIZATION_EN==input$change3p1,"POSITIVE"])
             | abs(res[res$ORGANIZATION_EN=="ROEB","POSITIVE"]-
                   res[res$ORGANIZATION_EN==input$change3p1,"POSITIVE"])
             < input$change2p1) {
            next }}
        else if(input$change1p1=="ROEB") {
          res <- data.f[data.f$SURVEYR==2019 & data.f$QUESTION_EN==q,]
          greaterThanMin <- FALSE
          for(dir in input$directoratep1) {
            if(!is.na(res[res$ORGANIZATION_EN=="ROEB","POSITIVE"])
               & !is.na(res[res$ORGANIZATION_EN==dir,"POSITIVE"])
               & abs(res[res$ORGANIZATION_EN=="ROEB","POSITIVE"]-
                     res[res$ORGANIZATION_EN==dir,"POSITIVE"])
               >= input$change2p1) {
              greaterThanMin <- TRUE
              break }}
          if(!greaterThanMin) {
            next }}
        else if(input$change3p1!="any") {
          res <- data.f[data.f$ORGANIZATION_EN==input$change3p1
                        & data.f$QUESTION_EN==q,]
          if(is.na(res[res$SURVEYR==2019,"POSITIVE"])
             | is.na(res[res$SURVEYR==input$againstp1,"POSITIVE"])
             | abs(res[res$SURVEYR==2019,"POSITIVE"]
                   -res[res$SURVEYR==input$againstp1,"POSITIVE"])
             < input$change2p1) {
            next }}
        else {
          res <- data.f[data.f$ORGANIZATION_EN %in% input$directoratep1
                        & data.f$QUESTION_EN==q,]
          greaterThanMin <- FALSE
          for(dir in input$directoratep1) {
            if(!is.na(res[res$SURVEYR==2019 & res$ORGANIZATION_EN==dir,
                          "POSITIVE"])
               & !is.na(res[res$SURVEYR==input$againstp1
                            & res$ORGANIZATION_EN==dir,"POSITIVE"])
               & abs(res[res$SURVEYR==2019 & res$ORGANIZATION_EN==dir,
                         "POSITIVE"] -
                     res[res$SURVEYR==input$againstp1
                         & res$ORGANIZATION_EN==dir,"POSITIVE"])
               >= input$change2p1) {
              greaterThanMin <- TRUE
              break }}
          if(!greaterThanMin) {
            next }}
        qs <- c(qs,q) }
      
      if(input$change1p1=="ROEB" & input$change3p1!="any") {
        res <- data.f[data.f$SURVEYR==2019
                      & data.f$ORGANIZATION_EN %in% c("ROEB",input$change3p1)
                      & data.f$QUESTION_EN %in% qs,]
        avg1 <- mean(res[res$ORGANIZATION_EN=="ROEB","POSITIVE"],na.rm=TRUE)
        avg2 <- mean(res[res$ORGANIZATION_EN==input$change3p1,"POSITIVE"],
                     na.rm=TRUE)
        if(!is.nan(avg1) & !is.nan(avg2)
           & round(abs(avg1-avg2),0) >= input$change2p1) {
          qs <- c("All questions (averaged)",qs) }}
      else if(input$change1p1=="ROEB"){
        res <- data.f[data.f$SURVEYR==2019 & data.f$QUESTION_EN %in% qs,]
        avg1 <- mean(res[res$ORGANIZATION_EN=="ROEB","POSITIVE"],na.rm=TRUE)
        for(dir in input$directoratep1) {
          avg2 <- mean(res[res$ORGANIZATION_EN==dir,"POSITIVE"],na.rm=TRUE)
          if(!is.nan(avg1) & !is.nan(avg2)
             & round(abs(avg1-avg2),0) >= input$change2p1) {
            qs <- c("All questions (averaged)",qs)
            break }}}
      else if(input$change3p1!="any") {
        res <- data.f[data.f$ORGANIZATION_EN==input$change3p1
                      & data.f$QUESTION_EN %in% qs,]
        avg1 <- mean(res[res$SURVEYR==2019,"POSITIVE"],na.rm=TRUE)
        avg2 <- mean(res[res$SURVEYR==input$againstp1,"POSITIVE"],na.rm=TRUE)
        if(!is.nan(avg1) & !is.nan(avg2)
           & round(abs(avg1-avg2),0) >= input$change2p1) {
          qs <- c("All questions (averaged)",qs) }}
      else {
        res <- data.f[data.f$ORGANIZATION_EN %in% input$directoratep1
                      & data.f$QUESTION_EN %in% qs,]
        for(dir in input$directoratep1) {
          avg1 <- mean(
            res[res$SURVEYR==2019 & res$ORGANIZATION_EN==dir,"POSITIVE"],
            na.rm=TRUE)
          avg2 <- mean(
            res[res$SURVEYR==input$againstp1 & res$ORGANIZATION_EN==dir,
                "POSITIVE"],
            na.rm=TRUE)
          if(!is.nan(avg1) & !is.nan(avg2)
             & round(abs(avg1-avg2),0) >= input$change2p1) {
            qs <- c("All questions (averaged)",qs)
            break }}}
      selectInput(inputId="questionp1",label=NULL,qs,width="600px") }
  })
  output$ques_outputp4 <- renderUI({
    if(is.null(input$directoratep4) | is.null(input$change1p4)
       | is.null(input$change3p4)) {
      disabled(selectInput(inputId="questionp4",label=NULL,c(),width="629px")) }
    else{
      data.f <- data[data$SURVEYR %in% c(2019,input$againstp4)
                     & data$ORGANIZATION_FR %in% c("DGORAL",input$directoratep4)
                     & data$THEME_FR==input$themep4,]
      allqs <- as.character(unique(data.f$QUESTION_FR))
      qs <- c()
      for(q in allqs) {
        if(!(q %in% data.f[data.f$SURVEYR==2019,"QUESTION_FR"])
           | !(q %in% data.f[data.f$SURVEYR==input$againstp4,"QUESTION_FR"])) {
          next
        }
        if(input$change1p4=="DGORAL" & input$change3p4!="any") {
          res <- data.f[data.f$SURVEYR==2019
                        & data.f$ORGANIZATION_FR %in% c("DGORAL",
                                                        input$change3p4)
                        & data.f$QUESTION_FR==q,]
          if(is.na(res[res$ORGANIZATION_FR=="DGORAL","POSITIVE"])
             | is.na(res[res$ORGANIZATION_FR==input$change3p4,"POSITIVE"])
             | abs(res[res$ORGANIZATION_FR=="DGORAL","POSITIVE"]-
                   res[res$ORGANIZATION_FR==input$change3p4,"POSITIVE"])
             < input$change2p4) {
            next }}
        else if(input$change1p4=="DGORAL") {
          res <- data.f[data.f$SURVEYR==2019 & data.f$QUESTION_FR==q,]
          greaterThanMin <- FALSE
          for(dir in input$directoratep4) {
            if(!is.na(res[res$ORGANIZATION_FR=="DGORAL","POSITIVE"])
               & !is.na(res[res$ORGANIZATION_FR==dir,"POSITIVE"])
               & abs(res[res$ORGANIZATION_FR=="DGORAL","POSITIVE"]-
                     res[res$ORGANIZATION_FR==dir,"POSITIVE"])
               >= input$change2p4) {
              greaterThanMin <- TRUE
              break }}
          if(!greaterThanMin) {
            next }}
        else if(input$change3p4!="any") {
          res <- data.f[data.f$ORGANIZATION_FR==input$change3p4
                        & data.f$QUESTION_FR==q,]
          if(is.na(res[res$SURVEYR==2019,"POSITIVE"])
             | is.na(res[res$SURVEYR==input$againstp4,"POSITIVE"])
             | abs(res[res$SURVEYR==2019,"POSITIVE"]
                   -res[res$SURVEYR==input$againstp4,"POSITIVE"])
             < input$change2p4) {
            next }}
        else {
          res <- data.f[data.f$ORGANIZATION_FR %in% input$directoratep4
                        & data.f$QUESTION_FR==q,]
          greaterThanMin <- FALSE
          for(dir in input$directoratep4) {
            if(!is.na(res[res$SURVEYR==2019 & res$ORGANIZATION_FR==dir,
                          "POSITIVE"])
               & !is.na(res[res$SURVEYR==input$againstp4
                            & res$ORGANIZATION_FR==dir,"POSITIVE"])
               & abs(res[res$SURVEYR==2019 & res$ORGANIZATION_FR==dir,
                         "POSITIVE"] -
                     res[res$SURVEYR==input$againstp4
                         & res$ORGANIZATION_FR==dir,"POSITIVE"])
               >= input$change2p4) {
              greaterThanMin <- TRUE
              break }}
          if(!greaterThanMin) {
            next }}
        qs <- c(qs,q) }
      
      if(input$change1p4=="DGORAL" & input$change3p4!="any") {
        res <- data.f[data.f$SURVEYR==2019
                      & data.f$ORGANIZATION_FR %in% c("DGORAL",input$change3p4)
                      & data.f$QUESTION_FR %in% qs,]
        avg1 <- mean(res[res$ORGANIZATION_FR=="DGORAL","POSITIVE"],na.rm=TRUE)
        avg2 <- mean(res[res$ORGANIZATION_FR==input$change3p4,"POSITIVE"],
                     na.rm=TRUE)
        if(!is.nan(avg1) & !is.nan(avg2)
           & round(abs(avg1-avg2),0) >= input$change2p4) {
          qs <- c("All Toutes questions (moyennées)",qs) }}
      else if(input$change1p4=="DGORAL"){
        res <- data.f[data.f$SURVEYR==2019 & data.f$QUESTION_FR %in% qs,]
        avg1 <- mean(res[res$ORGANIZATION_FR=="DGORAL","POSITIVE"],na.rm=TRUE)
        for(dir in input$directoratep4) {
          avg2 <- mean(res[res$ORGANIZATION_FR==dir,"POSITIVE"],na.rm=TRUE)
          if(!is.nan(avg1) & !is.nan(avg2)
             & round(abs(avg1-avg2),0) >= input$change2p4) {
            qs <- c("Toutes questions (moyennées)",qs)
            break }}}
      else if(input$change3p4!="any") {
        res <- data.f[data.f$ORGANIZATION_FR==input$change3p4
                      & data.f$QUESTION_FR %in% qs,]
        avg1 <- mean(res[res$SURVEYR==2019,"POSITIVE"],na.rm=TRUE)
        avg2 <- mean(res[res$SURVEYR==input$againstp4,"POSITIVE"],na.rm=TRUE)
        if(!is.nan(avg1) & !is.nan(avg2)
           & round(abs(avg1-avg2),0) >= input$change2p4) {
          qs <- c("Toutes questions (moyennées)",qs) }}
      else {
        res <- data.f[data.f$ORGANIZATION_FR %in% input$directoratep4
                      & data.f$QUESTION_FR %in% qs,]
        for(dir in input$directoratep4) {
          avg1 <- mean(
            res[res$SURVEYR==2019 & res$ORGANIZATION_FR==dir,"POSITIVE"],
            na.rm=TRUE)
          avg2 <- mean(
            res[res$SURVEYR==input$againstp4 & res$ORGANIZATION_FR==dir,
                "POSITIVE"],
            na.rm=TRUE)
          if(!is.nan(avg1) & !is.nan(avg2)
             & round(abs(avg1-avg2),0) >= input$change2p4) {
            qs <- c("Toutes questions (moyennées)",qs)
            break }}}
      selectInput(inputId="questionp4",label=NULL,qs,width="629px") }
  })
  
  # ---- Individual observers --------------------------------------------------
  
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
  observeEvent(input$top,{ js$toTop() })
  observeEvent(input$language,{
    if(input$language=="en") { js$showEng() }
    else { js$showFr() }
  })
  observeEvent(input$directoratep1,ignoreNULL=FALSE,{
    if(is.null(input$directoratep1)) {
      disable("change1p1")
      disable("change2p1")}
    else{
      enable("change1p1")
      enable("change2p1") }
  })
  observeEvent(input$directoratep4,ignoreNULL=FALSE,{
    if(is.null(input$directoratep4)) {
      disable("change1p4")
      disable("change2p4")}
    else{
      enable("change1p4")
      enable("change2p4") }
  })
  observeEvent(input$retrievep1,{
    req(input$questionp1)
    output$resultsp1 <- renderUI({
      isolate(
        if(input$questionp1=="All questions (averaged)") {
          dirs <- c("ROEB",input$directoratep1)
          avgPos <- avgNeg <- avgNeut <- rep(NA,length(dirs))
          avgPosOld <- avgNeutOld <- avgNegOld <- rep(NA,length(dirs))
          data.f <- data[data$SURVEYR %in% c(2019,input$againstp1)
                         & data$ORGANIZATION_EN %in% dirs
                         & data$THEME_EN==input$themep1,]
          allqs <- as.character(unique(data.f$QUESTION_EN))
          qs <- c()
          for(q in allqs) {
            if(q %in% data.f[data.f$SURVEYR==2019,"QUESTION_EN"]
               & q %in% data.f[data.f$SURVEYR==input$againstp1,"QUESTION_EN"]) {
              qs <- c(qs,q) }}
          
          for(dir in dirs) {
            pos <- which(dirs==dir)
            res <- data.f[data.f$SURVEYR==2019
                          & data.f$ORGANIZATION_EN==dir
                          & data.f$QUESTION_EN %in% qs,c("POSITIVE","NEGATIVE")]
            if(is.nan(mean(res$POSITIVE,na.rm=TRUE))) {
              avgPos[pos] <- NA }
            else {
              avgPos[pos] <- round(mean(res$POSITIVE,na.rm=TRUE),0) }
            if(is.nan(mean(res$NEGATIVE,na.rm=TRUE))) {
              avgNeg[pos] <- NA }
            else {
              avgNeg[pos] <- round(mean(res$NEGATIVE,na.rm=TRUE),0) }
            avgNeut[pos] <- 100-avgPos[pos]-avgNeg[pos]
            res <- data.f[data.f$SURVEYR==input$againstp1
                          & data.f$ORGANIZATION_EN==dir
                          & data.f$QUESTION_EN %in% qs,c("POSITIVE","NEGATIVE")]
            if(is.nan(mean(res$POSITIVE,na.rm=TRUE))) {
              avgPosOld[pos] <- NA }
            else {
              avgPosOld[pos] <- round(mean(res$POSITIVE,na.rm=TRUE),0) }
            if(is.nan(mean(res$NEGATIVE,na.rm=TRUE))) {
              avgNegOld[pos] <- NA }
            else {
              avgNegOld[pos] <- round(mean(res$NEGATIVE,na.rm=TRUE),0) }
            avgNeutOld[pos] <- 100-avgPosOld[pos]-avgNegOld[pos]
          }
          if(input$againstp1==2019) {
            d <- data.frame(
              SURVEYR=c(rep(2019,length(dirs))),
              ORGANIZATION_EN=dirs,
              POSITIVE=avgPos,
              NEUTRAL=avgNeut,
              NEGATIVE=avgNeg) }
          else {
            d <- data.frame(
              SURVEYR=c(rep(2019,length(dirs)),
                        rep(input$againstp1,length(dirs))),
              ORGANIZATION_EN=rep(dirs,2),
              POSITIVE=c(avgPos,avgPosOld),
              NEUTRAL=c(avgNeut,avgNeutOld),
              NEGATIVE=c(avgNeg,avgNegOld))
          }
        } else {
          d <- data[data$SURVEYR %in% c(2019,input$againstp1)
                    & data$ORGANIZATION_EN %in% c("ROEB",input$directoratep1)
                    & data$QUESTION_EN==input$questionp1,
                    c("SURVEYR","ORGANIZATION_EN","POSITIVE","NEUTRAL",
                      "NEGATIVE")]
        })
      isolate(d$ORGANIZATION_EN <-
                factor(d$ORGANIZATION_EN,levels=c("ROEB",input$directoratep1)))
      d$SURVEYR <- as.character(d$SURVEYR)
      d.m <- melt(d,c("ORGANIZATION_EN","SURVEYR"))
      
      htmlstr <- "<div style='display:inline-block;
                              width:45%;
                              margin:3px 0 27px 2%;'>
                    <table style='width:100%;'>"
      rowTemplate <-
        "<tr>
           <td>
             <div class='well' style='background-color:%s; width:100%%;
               height:112px;'>
               <div style='color:%s; font-size:10pt;'>
                 Positive in 2019
               </div>
               <div style='text-align:right;'>
                 <strong style='color:%s; font-size:25pt;'>
                   %s%%
                 </strong>
               </div>
             </div></td></tr>"
      htmlstr2 <- "<div style='display:inline-block;
                               width:51%;
                               margin:3px 2% 27px 0;'>
                     <table style='width:100%;'>"
      rowTemplate2 <-
        "<tr>
           <td class='rightcol'>
             <div class='well' style='padding:0 9%% 0 9%%; background-color:%s;
               width:100%%; height:56px;'>
               <div style='color:%s; font-size:10pt; width:50%%;
                 display:inline-block; padding-top:17px; vertical-align:top;'>
                 <span>vs %s</span>
               </div>
               <div style='text-align:right; display:inline-block; width:47%%;
                 padding-top:8px;'>
                 <strong style='color:%s; font-size:20pt;display:inline-block;'>
                   %s
                 </strong>
               </div>
             </div></td></tr>"
      htmlstr3 <- "<div style='display:inline-block;
                               width:45%;
                               margin:3px 0 27px 2%;'>
                     <table style='width:100%;'>"
      rowTemplate3 <-
        "<tr>
           <td>
             <div class='well' style='background-color:%s; width:100%%;
               height:85px; padding-top:10px;'>
               <div style='color:%s; font-size:10pt;'>
                 Positive in 2019
               </div>
               <div style='text-align:right;'>
                 <strong style='color:%s; font-size:25pt;'>
                   %s%%
                 </strong>
               </div>
             </div></td></tr>"
      htmlstr4 <- "<div style='display:inline-block;
                               width:51%;
                               margin:3px 2% 27px 0;'>
                     <table style='width:100%;'>"
      rowTemplate4 <-
        "<tr>
           <td class='rightcol'>
             <div class='well' style='padding:0 9%% 0 9%%; background-color:%s;
               width:100%%; height:85px;'>
               <div style='color:%s; font-size:10pt; width:50%%;
                 display:inline-block; padding-top:32px; vertical-align:top;'>
                 <span>vs ROEB</span>
               </div>
               <div style='text-align:right; display:inline-block; width:47%%;
                 padding-top:23px;'>
                 <strong style='color:%s; font-size:20pt;display:inline-block;'>
                   %s
                 </strong>
               </div>
             </div></td></tr>"
      
      isolate(
        for(dir in c("ROEB",input$directoratep1)) {
          positive <- d[d$SURVEYR=="2019" & d$ORGANIZATION_EN==dir,"POSITIVE"]
          change1 <- NA
          change2 <- d[d$SURVEYR=="2019" & d$ORGANIZATION_EN==dir,"POSITIVE"]-
            d[d$SURVEYR==input$againstp1 & d$ORGANIZATION_EN==dir,"POSITIVE"]
          if(dir != "ROEB") {
            change1 <- d[d$SURVEYR=="2019" & d$ORGANIZATION_EN==dir,"POSITIVE"]-
              d[d$SURVEYR=="2019" & d$ORGANIZATION_EN=="ROEB","POSITIVE"] }
          if(is.na(positive)) {
            htmlstr <-
              paste0(htmlstr,sprintf(rowTemplate,HTML_COLOURS["null"],
                                     HTML_COLOURS["grey80"],
                                     HTML_COLOURS["grey80"],"--"))
            htmlstr3 <-
              paste0(htmlstr3,sprintf(rowTemplate3,HTML_COLOURS["null"],
                                      HTML_COLOURS["grey80"],
                                      HTML_COLOURS["grey80"],"--")) }
          else {
            if(positive >= 80) {
              htmlstr <-
                paste0(htmlstr,sprintf(rowTemplate,HTML_COLOURS["steelblue4"],
                                       HTML_COLOURS["white"],
                                       HTML_COLOURS["white"],positive))
              htmlstr3 <-
                paste0(htmlstr3,sprintf(rowTemplate3,HTML_COLOURS["steelblue4"],
                                        HTML_COLOURS["white"],
                                        HTML_COLOURS["white"],positive)) }
            else if(positive >= 70 & positive < 80) {
              htmlstr <-
                paste0(htmlstr,sprintf(rowTemplate,HTML_COLOURS["steelblue3"],
                                       HTML_COLOURS["white"],
                                       HTML_COLOURS["white"],positive))
              htmlstr3 <-
                paste0(htmlstr3,sprintf(rowTemplate3,HTML_COLOURS["steelblue3"],
                                        HTML_COLOURS["white"],
                                        HTML_COLOURS["white"],positive)) }
            else if(positive >= 60 & positive < 70) {
              htmlstr <-
                paste0(htmlstr,sprintf(rowTemplate,HTML_COLOURS["lightskyblue"],
                                       HTML_COLOURS["white"],
                                       HTML_COLOURS["white"],positive))
              htmlstr3 <-
                paste0(htmlstr3,sprintf(rowTemplate3,
                                        HTML_COLOURS["lightskyblue"],
                                        HTML_COLOURS["white"],
                                        HTML_COLOURS["white"],positive)) }
            else if(positive >= 50 & positive < 60) {
              htmlstr <-
                paste0(htmlstr,
                       sprintf(rowTemplate,HTML_COLOURS["lightskyblue1"],
                               HTML_COLOURS["white"],HTML_COLOURS["white"],
                               positive))
              htmlstr3 <-
                paste0(htmlstr3,
                       sprintf(rowTemplate3,HTML_COLOURS["lightskyblue1"],
                               HTML_COLOURS["white"],HTML_COLOURS["white"],
                               positive)) }
            else {
              htmlstr <-
                paste0(htmlstr,sprintf(rowTemplate,HTML_COLOURS["aliceblue"],
                                       HTML_COLOURS["slategray3"],
                                       HTML_COLOURS["slategray3"],positive))
              htmlstr3 <-
                paste0(htmlstr3,sprintf(rowTemplate3,HTML_COLOURS["aliceblue"],
                                        HTML_COLOURS["slategray3"],
                                        HTML_COLOURS["slategray3"],positive)) }}
          if(is.na(change1)) {
            htmlstr2 <-
              paste0(htmlstr2,
                     sprintf(rowTemplate2,HTML_COLOURS["null"],
                             HTML_COLOURS["grey80"],"ROEB",
                             HTML_COLOURS["grey80"],"--"))
            htmlstr4 <-
              paste0(htmlstr4,
                     sprintf(rowTemplate4,HTML_COLOURS["null"],
                             HTML_COLOURS["grey80"],HTML_COLOURS["grey80"],
                             "--"))}
          else {
            if(change1 >= 10) {
              htmlstr2 <-
                paste0(htmlstr2,
                       sprintf(rowTemplate2,HTML_COLOURS["palegreen3"],
                               HTML_COLOURS["seagreen"],"ROEB",
                               HTML_COLOURS["seagreen"],paste0("+",change1)))
              htmlstr4 <-
                paste0(htmlstr4,
                       sprintf(rowTemplate4,HTML_COLOURS["palegreen3"],
                               HTML_COLOURS["seagreen"],
                               HTML_COLOURS["seagreen"],paste0("+",change1))) }
            else if(change1 >= 5) {
              htmlstr2 <-
                paste0(htmlstr2,
                       sprintf(rowTemplate2,HTML_COLOURS["darkseagreen2"],
                               HTML_COLOURS["seagreen"],"ROEB",
                               HTML_COLOURS["seagreen"],paste0("+",change1)))
              htmlstr4 <-
                paste0(htmlstr4,
                       sprintf(rowTemplate4,HTML_COLOURS["darkseagreen2"],
                               HTML_COLOURS["seagreen"],
                               HTML_COLOURS["seagreen"],paste0("+",change1))) }
            else if(change1 > 0) {
              htmlstr2 <-
                paste0(htmlstr2,
                       sprintf(rowTemplate2,HTML_COLOURS["snow"],
                               HTML_COLOURS["grey60"],"ROEB",
                               HTML_COLOURS["grey60"],paste0("+",change1)))
              htmlstr4 <-
                paste0(htmlstr4,
                       sprintf(rowTemplate4,HTML_COLOURS["snow"],
                               HTML_COLOURS["grey60"],HTML_COLOURS["grey60"],
                               paste0("+",change1))) }
            else if(change1 > -5) {
              htmlstr2 <-
                paste0(htmlstr2,
                       sprintf(rowTemplate2,HTML_COLOURS["snow"],
                               HTML_COLOURS["grey60"],"ROEB",
                               HTML_COLOURS["grey60"],change1))
              htmlstr4 <-
                paste0(htmlstr4,
                       sprintf(rowTemplate4,HTML_COLOURS["snow"],
                               HTML_COLOURS["grey60"],HTML_COLOURS["grey60"],
                               change1)) }
            else if(change1 > -10) {
              htmlstr2 <-
                paste0(htmlstr2,
                       sprintf(rowTemplate2,HTML_COLOURS["pink"],
                               HTML_COLOURS["firebrick"],"ROEB",
                               HTML_COLOURS["firebrick"],change1))
              htmlstr4 <-
                paste0(htmlstr4,
                       sprintf(rowTemplate4,HTML_COLOURS["pink"],
                               HTML_COLOURS["firebrick"],
                               HTML_COLOURS["firebrick"],change1)) }
            else {
              htmlstr2 <-
                paste0(htmlstr2,
                       sprintf(rowTemplate2,HTML_COLOURS["lightcoral"],
                               HTML_COLOURS["firebrick"],"ROEB",
                               HTML_COLOURS["firebrick"],change1))
              htmlstr4 <-
                paste0(htmlstr4,
                       sprintf(rowTemplate4,HTML_COLOURS["lightcoral"],
                               HTML_COLOURS["firebrick"],
                               HTML_COLOURS["firebrick"],change1)) }}
          if(is.na(change2)) {
            htmlstr2 <-
              paste0(htmlstr2,
                     sprintf(rowTemplate2,HTML_COLOURS["null"],
                             HTML_COLOURS["grey80"],input$againstp1,
                             HTML_COLOURS["grey80"],"--"))}
          else {
            if(change2 >= 10) {
              htmlstr2 <-
                paste0(htmlstr2,
                       sprintf(rowTemplate2,HTML_COLOURS["palegreen3"],
                               HTML_COLOURS["seagreen"],input$againstp1,
                               HTML_COLOURS["seagreen"],paste0("+",change2)))}
            else if(change2 >= 5) {
              htmlstr2 <-
                paste0(htmlstr2,
                       sprintf(rowTemplate2,HTML_COLOURS["darkseagreen2"],
                               HTML_COLOURS["seagreen"],input$againstp1,
                               HTML_COLOURS["seagreen"],paste0("+",change2)))}
            else if(change2 > 0) {
              htmlstr2 <-
                paste0(htmlstr2,
                       sprintf(rowTemplate2,HTML_COLOURS["snow"],
                               HTML_COLOURS["grey60"],input$againstp1,
                               HTML_COLOURS["grey60"],paste0("+",change2))) }
            else if(change2 > -5) {
              htmlstr2 <-
                paste0(htmlstr2,
                       sprintf(rowTemplate2,HTML_COLOURS["snow"],
                               HTML_COLOURS["grey60"],input$againstp1,
                               HTML_COLOURS["grey60"],change2)) }
            else if(change2 > -10) {
              htmlstr2 <-
                paste0(htmlstr2,
                       sprintf(rowTemplate2,HTML_COLOURS["pink"],
                               HTML_COLOURS["firebrick"],input$againstp1,
                               HTML_COLOURS["firebrick"],change2)) }
            else {
              htmlstr2 <-
                paste0(htmlstr2,
                       sprintf(rowTemplate2,HTML_COLOURS["lightcoral"],
                               HTML_COLOURS["firebrick"],input$againstp1,
                               HTML_COLOURS["firebrick"],change2)) }}
        })
      htmlstr <- paste0(htmlstr,"</table></div>")
      htmlstr2 <- paste0(htmlstr2,"</table></div>")
      htmlstr3 <- paste0(htmlstr3,"</table></div>")
      htmlstr4 <- paste0(htmlstr4,"</table></div>")
      p <- ggplot(d.m,aes(x=SURVEYR,y=value,fill=variable)) +
        geom_bar(stat="identity",
                 position=position_stack(reverse=TRUE)) +
        coord_flip() +
        facet_grid(rows=vars(ORGANIZATION_EN)) +
        theme_light() +
        labs(x=NULL, y="%", fill="") +
        scale_fill_manual(
          breaks=c("POSITIVE","NEUTRAL","NEGATIVE"),
          values=c("steelblue4","azure1","lightsalmon")) +
        geom_text(size=3,
                  position=position_stack(vjust=0.5,reverse=TRUE),
                  aes(label=value)) +
        theme(legend.position="top")
      
      tagList(
        isolate(h3(input$themep1)),
        isolate(h5(input$questionp1)),
        br(),
        isolate(
          if(input$againstp1==2019) {
            box(
              status=NULL,solidHeader=TRUE,width=7,
              renderPlot(height=160+85*length(input$directoratep1),{
                suppressWarnings({ print(p) })
              }))}
          else {
            box(
              status=NULL,solidHeader=TRUE,width=7,
              renderPlot(height=195+111*length(input$directoratep1),{
                suppressWarnings({ print(p) })
              }))}
        ),
        isolate(
          if(input$againstp1==2019) {
            box(
              status=NULL,solidHeader=TRUE,title="Trends",width=5,
              HTML(paste0(htmlstr3,htmlstr4))) }
          else {
            box(
              status=NULL,solidHeader=TRUE,title="Trends",width=5,
              HTML(paste0(htmlstr,htmlstr2))) }
        )
      )
    })
  })
  observeEvent(input$retrievep4,{
    req(input$questionp4)
    output$resultsp4 <- renderUI({
      isolate(
        if(input$questionp4=="Toutes questions (moyennées)") {
          dirs <- c("DGORAL",input$directoratep4)
          avgPos <- avgNeg <- avgNeut <- rep(NA,length(dirs))
          avgPosOld <- avgNeutOld <- avgNegOld <- rep(NA,length(dirs))
          data.f <- data[data$SURVEYR %in% c(2019,input$againstp4)
                         & data$ORGANIZATION_FR %in% dirs
                         & data$THEME_FR==input$themep4,]
          allqs <- as.character(unique(data.f$QUESTION_FR))
          qs <- c()
          for(q in allqs) {
            if(q %in% data.f[data.f$SURVEYR==2019,"QUESTION_FR"]
               & q %in% data.f[data.f$SURVEYR==input$againstp4,"QUESTION_FR"]) {
              qs <- c(qs,q) }}
          
          for(dir in dirs) {
            pos <- which(dirs==dir)
            res <- data.f[data.f$SURVEYR==2019
                          & data.f$ORGANIZATION_FR==dir
                          & data.f$QUESTION_FR %in% qs,c("POSITIVE","NEGATIVE")]
            if(is.nan(mean(res$POSITIVE,na.rm=TRUE))) {
              avgPos[pos] <- NA }
            else {
              avgPos[pos] <- round(mean(res$POSITIVE,na.rm=TRUE),0) }
            if(is.nan(mean(res$NEGATIVE,na.rm=TRUE))) {
              avgNeg[pos] <- NA }
            else {
              avgNeg[pos] <- round(mean(res$NEGATIVE,na.rm=TRUE),0) }
            avgNeut[pos] <- 100-avgPos[pos]-avgNeg[pos]
            res <- data.f[data.f$SURVEYR==input$againstp4
                          & data.f$ORGANIZATION_FR==dir
                          & data.f$QUESTION_FR %in% qs,c("POSITIVE","NEGATIVE")]
            if(is.nan(mean(res$POSITIVE,na.rm=TRUE))) {
              avgPosOld[pos] <- NA }
            else {
              avgPosOld[pos] <- round(mean(res$POSITIVE,na.rm=TRUE),0) }
            if(is.nan(mean(res$NEGATIVE,na.rm=TRUE))) {
              avgNegOld[pos] <- NA }
            else {
              avgNegOld[pos] <- round(mean(res$NEGATIVE,na.rm=TRUE),0) }
            avgNeutOld[pos] <- 100-avgPosOld[pos]-avgNegOld[pos]
          }
          if(input$againstp4==2019) {
            d <- data.frame(
              SURVEYR=c(rep(2019,length(dirs))),
              ORGANIZATION_FR=dirs,
              POSITIVE=avgPos,
              NEUTRAL=avgNeut,
              NEGATIVE=avgNeg) }
          else {
            d <- data.frame(
              SURVEYR=c(rep(2019,length(dirs)),
                        rep(input$againstp4,length(dirs))),
              ORGANIZATION_FR=rep(dirs,2),
              POSITIVE=c(avgPos,avgPosOld),
              NEUTRAL=c(avgNeut,avgNeutOld),
              NEGATIVE=c(avgNeg,avgNegOld))
          }
        } else {
          d <- data[data$SURVEYR %in% c(2019,input$againstp4)
                    & data$ORGANIZATION_FR %in% c("DGORAL",input$directoratep4)
                    & data$QUESTION_FR==input$questionp4,
                    c("SURVEYR","ORGANIZATION_FR","POSITIVE","NEUTRAL",
                      "NEGATIVE")]
        })
      isolate(d$ORGANIZATION_FR <-
                factor(d$ORGANIZATION_FR,levels=c("DGORAL",input$directoratep4))
              )
      d$SURVEYR <- as.character(d$SURVEYR)
      d.m <- melt(d,c("ORGANIZATION_FR","SURVEYR"))
      
      htmlstr <- "<div style='display:inline-block;
                              width:45%;
                              margin:3px 0 27px 2%;'>
                    <table style='width:100%;'>"
      rowTemplate <-
        "<tr>
           <td>
             <div class='well' style='background-color:%s; width:100%%;
               height:112px; padding-left:18px; padding-right:18px;'>
               <div style='color:%s; font-size:10pt;'>
                 Positives en 2019
               </div>
               <div style='text-align:right;'>
                 <strong style='color:%s; font-size:25pt;'>
                   %s%%
                 </strong>
               </div>
             </div></td></tr>"
      htmlstr2 <- "<div style='display:inline-block;
                               width:51%;
                               margin:3px 2% 27px 0;'>
                     <table style='width:100%;'>"
      rowTemplate2 <-
        "<tr>
           <td class='rightcol'>
             <div class='well' style='padding:0 9%% 0 9%%; background-color:%s;
               width:100%%; height:56px;'>
               <div style='color:%s; font-size:10pt; width:60%%;
                 display:inline-block; padding-top:17px; vertical-align:top;'>
                 <span>c. %s</span>
               </div>
               <div style='text-align:right; display:inline-block; width:37%%;
                 padding-top:8px;'>
                 <strong style='color:%s; font-size:20pt;display:inline-block;'>
                   %s
                 </strong>
               </div>
             </div></td></tr>"
      htmlstr3 <- "<div style='display:inline-block;
                               width:45%;
                               margin:3px 0 27px 2%;'>
                     <table style='width:100%;'>"
      rowTemplate3 <-
        "<tr>
           <td>
             <div class='well' style='background-color:%s; width:100%%;
               height:85px; padding-top:10px; padding-left:18px;
               padding-right:18px;'>
               <div style='color:%s; font-size:10pt;'>
                 Positives en 2019
               </div>
               <div style='text-align:right;'>
                 <strong style='color:%s; font-size:25pt;'>
                   %s%%
                 </strong>
               </div>
             </div></td></tr>"
      htmlstr4 <- "<div style='display:inline-block;
                               width:51%;
                               margin:3px 2% 27px 0;'>
                     <table style='width:100%;'>"
      rowTemplate4 <-
        "<tr>
           <td class='rightcol'>
             <div class='well' style='padding:0 9%% 0 9%%; background-color:%s;
               width:100%%; height:85px;'>
               <div style='color:%s; font-size:10pt; width:60%%;
                 display:inline-block; padding-top:32px; vertical-align:top;'>
                 <span>c. la DGORAL</span>
               </div>
               <div style='text-align:right; display:inline-block; width:37%%;
                 padding-top:23px;'>
                 <strong style='color:%s; font-size:20pt;display:inline-block;'>
                   %s
                 </strong>
               </div>
             </div></td></tr>"
      
      isolate(
        for(dir in c("DGORAL",input$directoratep4)) {
          positive <- d[d$SURVEYR=="2019" & d$ORGANIZATION_FR==dir,"POSITIVE"]
          change1 <- NA
          change2 <- d[d$SURVEYR=="2019" & d$ORGANIZATION_FR==dir,"POSITIVE"]-
            d[d$SURVEYR==input$againstp4 & d$ORGANIZATION_FR==dir,"POSITIVE"]
          if(dir != "DGORAL") {
            change1 <- d[d$SURVEYR=="2019" & d$ORGANIZATION_FR==dir,"POSITIVE"]-
              d[d$SURVEYR=="2019" & d$ORGANIZATION_FR=="DGORAL","POSITIVE"] }
          if(is.na(positive)){
            htmlstr <-
              paste0(htmlstr,sprintf(rowTemplate,HTML_COLOURS["null"],
                                     HTML_COLOURS["grey80"],
                                     HTML_COLOURS["grey80"],"--"))
            htmlstr3 <-
              paste0(htmlstr3,sprintf(rowTemplate3,HTML_COLOURS["null"],
                                      HTML_COLOURS["grey80"],
                                      HTML_COLOURS["grey80"],"--")) }
          else {
            if(positive >= 80) {
              htmlstr <-
                paste0(htmlstr,sprintf(rowTemplate,HTML_COLOURS["steelblue4"],
                                       HTML_COLOURS["white"],
                                       HTML_COLOURS["white"],positive))
              htmlstr3 <-
                paste0(htmlstr3,sprintf(rowTemplate3,HTML_COLOURS["steelblue4"],
                                        HTML_COLOURS["white"],
                                        HTML_COLOURS["white"],positive)) }
            else if(positive >= 70 & positive < 80) {
              htmlstr <-
                paste0(htmlstr,sprintf(rowTemplate,HTML_COLOURS["steelblue3"],
                                       HTML_COLOURS["white"],
                                       HTML_COLOURS["white"],positive))
              htmlstr3 <-
                paste0(htmlstr3,sprintf(rowTemplate3,HTML_COLOURS["steelblue3"],
                                        HTML_COLOURS["white"],
                                        HTML_COLOURS["white"],positive)) }
            else if(positive >= 60 & positive < 70) {
              htmlstr <-
                paste0(htmlstr,sprintf(rowTemplate,HTML_COLOURS["lightskyblue"],
                                       HTML_COLOURS["white"],
                                       HTML_COLOURS["white"],positive))
              htmlstr3 <-
                paste0(htmlstr3,
                       sprintf(rowTemplate3,HTML_COLOURS["lightskyblue"],
                               HTML_COLOURS["white"],HTML_COLOURS["white"],
                               positive)) }
            else if(positive >= 50 & positive < 60) {
              htmlstr <-
                paste0(htmlstr,
                       sprintf(rowTemplate,HTML_COLOURS["lightskyblue1"],
                               HTML_COLOURS["white"],HTML_COLOURS["white"],
                               positive))
              htmlstr3 <-
                paste0(htmlstr3,
                       sprintf(rowTemplate3,HTML_COLOURS["lightskyblue1"],
                               HTML_COLOURS["white"],HTML_COLOURS["white"],
                               positive)) }
            else {
              htmlstr <-
                paste0(htmlstr,sprintf(rowTemplate,HTML_COLOURS["aliceblue"],
                                       HTML_COLOURS["slategray3"],
                                       HTML_COLOURS["slategray3"],positive))
              htmlstr3 <-
                paste0(htmlstr3,sprintf(rowTemplate3,HTML_COLOURS["aliceblue"],
                               HTML_COLOURS["slategray3"],
                               HTML_COLOURS["slategray3"],positive)) }}
          if(is.na(change1)) {
            htmlstr2 <-
              paste0(htmlstr2,
                     sprintf(rowTemplate2,HTML_COLOURS["null"],
                             HTML_COLOURS["grey80"],"la DGORAL",
                             HTML_COLOURS["grey80"],"--"))
            htmlstr4 <-
              paste0(htmlstr4,
                     sprintf(rowTemplate4,HTML_COLOURS["null"],
                             HTML_COLOURS["grey80"],
                             HTML_COLOURS["grey80"],"--")) }
          else {
            if(change1 >= 10) {
              htmlstr2 <-
                paste0(htmlstr2,
                       sprintf(rowTemplate2,HTML_COLOURS["palegreen3"],
                               HTML_COLOURS["seagreen"],"la DGORAL",
                               HTML_COLOURS["seagreen"],paste0("+",change1)))
              htmlstr4 <-
                paste0(htmlstr4,
                       sprintf(rowTemplate4,HTML_COLOURS["palegreen3"],
                               HTML_COLOURS["seagreen"],
                               HTML_COLOURS["seagreen"],paste0("+",change1))) }
            else if(change1 >= 5) {
              htmlstr2 <-
                paste0(htmlstr2,
                       sprintf(rowTemplate2,HTML_COLOURS["darkseagreen2"],
                               HTML_COLOURS["seagreen"],"la DGORAL",
                               HTML_COLOURS["seagreen"],paste0("+",change1)))
              htmlstr4 <-
                paste0(htmlstr4,
                       sprintf(rowTemplate4,HTML_COLOURS["darkseagreen2"],
                               HTML_COLOURS["seagreen"],
                               HTML_COLOURS["seagreen"],paste0("+",change1))) }
            else if(change1 > 0) {
              htmlstr2 <-
                paste0(htmlstr2,
                       sprintf(rowTemplate2,HTML_COLOURS["snow"],
                               HTML_COLOURS["grey60"],"la DGORAL",
                               HTML_COLOURS["grey60"],paste0("+",change1)))
              htmlstr4 <-
                paste0(htmlstr4,
                       sprintf(rowTemplate4,HTML_COLOURS["snow"],
                               HTML_COLOURS["grey60"],
                               HTML_COLOURS["grey60"],paste0("+",change1))) }
            else if(change1 > -5) {
              htmlstr2 <-
                paste0(htmlstr2,
                       sprintf(rowTemplate2,HTML_COLOURS["snow"],
                               HTML_COLOURS["grey60"],"la DGORAL",
                               HTML_COLOURS["grey60"],change1))
              htmlstr4 <-
                paste0(htmlstr4,
                       sprintf(rowTemplate4,HTML_COLOURS["snow"],
                               HTML_COLOURS["grey60"],
                               HTML_COLOURS["grey60"],change1)) }
            else if(change1 > -10) {
              htmlstr2 <-
                paste0(htmlstr2,
                       sprintf(rowTemplate2,HTML_COLOURS["pink"],
                               HTML_COLOURS["firebrick"],"la DGORAL",
                               HTML_COLOURS["firebrick"],change1))
              htmlstr4 <-
                paste0(htmlstr4,
                       sprintf(rowTemplate4,HTML_COLOURS["pink"],
                               HTML_COLOURS["firebrick"],
                               HTML_COLOURS["firebrick"],change1)) }
            else {
              htmlstr2 <-
                paste0(htmlstr2,
                       sprintf(rowTemplate2,HTML_COLOURS["lightcoral"],
                               HTML_COLOURS["firebrick"],"la DGORAL",
                               HTML_COLOURS["firebrick"],change1))
              htmlstr4 <-
                paste0(htmlstr4,
                       sprintf(rowTemplate4,HTML_COLOURS["lightcoral"],
                               HTML_COLOURS["firebrick"],
                               HTML_COLOURS["firebrick"],change1)) }}
          if(is.na(change2)) {
            htmlstr2 <-
              paste0(htmlstr2,
                     sprintf(rowTemplate2,HTML_COLOURS["null"],
                             HTML_COLOURS["grey80"],input$againstp4,
                             HTML_COLOURS["grey80"],"--"))}
          else {
            if(change2 >= 10) {
              htmlstr2 <-
                paste0(htmlstr2,
                       sprintf(rowTemplate2,HTML_COLOURS["palegreen3"],
                               HTML_COLOURS["seagreen"],input$againstp4,
                               HTML_COLOURS["seagreen"],paste0("+",change2)))}
            else if(change2 >= 5) {
              htmlstr2 <-
                paste0(htmlstr2,
                       sprintf(rowTemplate2,HTML_COLOURS["darkseagreen2"],
                               HTML_COLOURS["seagreen"],input$againstp4,
                               HTML_COLOURS["seagreen"],paste0("+",change2)))}
            else if(change2 > 0) {
              htmlstr2 <-
                paste0(htmlstr2,
                       sprintf(rowTemplate2,HTML_COLOURS["snow"],
                               HTML_COLOURS["grey60"],input$againstp4,
                               HTML_COLOURS["grey60"],paste0("+",change2))) }
            else if(change2 > -5) {
              htmlstr2 <-
                paste0(htmlstr2,
                       sprintf(rowTemplate2,HTML_COLOURS["snow"],
                               HTML_COLOURS["grey60"],input$againstp4,
                               HTML_COLOURS["grey60"],change2)) }
            else if(change2 > -10) {
              htmlstr2 <-
                paste0(htmlstr2,
                       sprintf(rowTemplate2,HTML_COLOURS["pink"],
                               HTML_COLOURS["firebrick"],input$againstp4,
                               HTML_COLOURS["firebrick"],change2)) }
            else {
              htmlstr2 <-
                paste0(htmlstr2,
                       sprintf(rowTemplate2,HTML_COLOURS["lightcoral"],
                               HTML_COLOURS["firebrick"],input$againstp4,
                               HTML_COLOURS["firebrick"],change2)) }}
        })
      htmlstr <- paste0(htmlstr,"</table></div>")
      htmlstr2 <- paste0(htmlstr2,"</table></div>")
      htmlstr3 <- paste0(htmlstr3,"</table></div>")
      htmlstr4 <- paste0(htmlstr4,"</table></div>")
      p <- ggplot(d.m, aes(x=SURVEYR,y=value,fill=variable)) +
        geom_bar(stat="identity",
                 position=position_stack(reverse=TRUE)) +
        coord_flip() +
        facet_grid(rows=vars(ORGANIZATION_FR)) +
        theme_light() +
        labs(x=NULL, y="%", fill="") +
        scale_fill_manual(
          breaks=c("POSITIVE","NEUTRAL","NEGATIVE"),
          labels=c("POSITIVES","NEUTRES","NÉGATIVES"),
          values=c("steelblue4","azure1","lightsalmon")) +
        geom_text(size=3,
                  position=position_stack(vjust=0.5,reverse=TRUE),
                  aes(label=value)) +
        theme(legend.position="top")
      
      tagList(
        isolate(h3(input$themep4)),
        isolate(h5(input$questionp4)),
        br(),
        isolate(
          if(input$againstp4==2019) {
            box(
              status=NULL,solidHeader=TRUE,width=7,
              renderPlot(height=160+85*length(input$directoratep4),{
                suppressWarnings({ print(p) })
              }))}
          else {
            box(
              status=NULL,solidHeader=TRUE,width=7,
              renderPlot(height=195+111*length(input$directoratep4),{
                suppressWarnings({ print(p) })
              }))}
        ),
        isolate(
          if(input$againstp4==2019) {
            box(
              status=NULL,solidHeader=TRUE,title="Les tendances",width=5,
              HTML(paste0(htmlstr3,htmlstr4))) }
          else {
            box(
              status=NULL,solidHeader=TRUE,title="Les tendances",width=5,
              HTML(paste0(htmlstr,htmlstr2))) }
        )
      )
    })
  })
  
  # ---- Plot outputs ----------------------------------------------------------
  
  output$graphsp2 <- renderUI({
    req(input$yearp2,input$themep2)
    data.f <- data[data$SURVEYR==input$yearp2 & data$THEME_EN==input$themep2,]
    
    qs <- as.character(unique(data.f$QID))
    lapply(qs, function(q) {
      qtitle <- as.character(questions[questions$ID==q,"EN"][1])
      res <- data.f[data.f$QID==q,]
      ans.set <-
        answers[,paste0("TYPE",questions[questions$ID==q,"ANS_TYPE"][1],"_EN")]
      ans.set <- ans.set[ans.set!=""]
      
      v <- c()
      n <- length(ans.set)
      for(s in paste0("ANSWER",1:n)) {
        v <- c(v,res[,s]) }
      df <- structure(v,
                      .Dim=c(nrow(res),n),
                      .Dimnames=list(as.character(res$ORGANIZATION_EN),ans.set))
      df.m <- melt(df)
      df.m <- rename(df.m,Unit=Var1,Responses=Var2,Proportion=value)
      
      box(id=paste0("b",which(QIDS==q)),title=qtitle,status="primary",
          solidHeader=TRUE,width=12,collapsible=TRUE,collapsed=TRUE,
          render_delayed({
            p("(Percentages may not add to 100 due to rounding)")
          }),
          renderPlot(height=400,{
            p <- ggplot(df.m, aes(x=Unit, y=Proportion, fill=Responses)) +
              geom_bar(stat="identity", position=position_stack(reverse=TRUE)) +
              labs(x="", y="Proportion responded (%)", fill="Responses") +
              geom_text(size=3, position=position_stack(vjust=0.5,reverse=TRUE),
                        aes(label=Proportion)) +
              coord_flip() +
              theme_minimal()
            return(recolourBars(q,p,"en"))
          }),
          renderTable(rownames=TRUE, align="c", width="100%", {
            dtb <- data.frame(as.list(rev(res$COUNT)),
                              row.names="Number of responses")
            names(dtb) <- rev(as.character(res$ORGANIZATION_EN))
            return(dtb)
          }))
    })
  })
  output$graphsp5 <- renderUI({
    req(input$yearp5)
    req(input$themep5)
    data.f <- data[data$SURVEYR==input$yearp5 & data$THEME_FR==input$themep5,]
    
    qs <- as.character(unique(data.f$QID))
    lapply(qs, function(q) {
      qtitle <- as.character(questions[questions$ID==q,"FR"][1])
      res <- data.f[data.f$QID==q,]
      ans.set <-
        answers[,paste0("TYPE",questions[questions$ID==q,"ANS_TYPE"][1],"_FR")]
      ans.set <- ans.set[ans.set!=""]
      
      v <- c()
      n <- length(ans.set)
      for(s in paste0("ANSWER",1:n)) {
        v <- c(v,res[,s]) }
      df <- structure(v,
                      .Dim=c(nrow(res),n),
                      .Dimnames=list(as.character(res$ORGANIZATION_FR),ans.set))
      df.m <- melt(df)
      df.m <- rename(df.m,Unit=Var1,Responses=Var2,Proportion=value)
      
      box(id=paste0("b",which(QIDS==q)+N),title=qtitle,status="primary",
          solidHeader=TRUE,width=12,collapsible=TRUE,collapsed=TRUE,
          render_delayed({
            p("(Les pourcentages peuvent ne pas totaliser 100 en raison
              d'erreurs dans les arrondissements)")
          }),
          renderPlot(height=400,{
            p <- ggplot(df.m, aes(x=Unit, y=Proportion, fill=Responses)) +
              geom_bar(stat="identity", position=position_stack(reverse=TRUE)) +
              labs(x="", y="Pourcentage répondu (%)", fill="Réponses") +
              geom_text(size=3, position=position_stack(vjust=0.5,reverse=TRUE),
                        aes(label=Proportion)) +
              coord_flip() +
              theme_minimal()
            return(recolourBars(q,p,"fr"))
          }),
          renderTable(rownames=TRUE, align="c", width="100%", {
            dtb <- data.frame(as.list(rev(res$COUNT)),
                              row.names="Nombre de réponses")
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
  
  runjs("$(document).on('shiny:value', function(event){
         $('#spinner').hide();
         $('#continuebtns').show();})")
}
