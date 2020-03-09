
## ~~~~~~~~~~~~ SERVER ~~~~~~~~~~~~~ ##

 # Created by: Sijia Wang
 # Team: Data Analytics and Business Solutions (DABS)
 # Version: 1.1
 # Last modified: 2020-03-05
 # Description: Server for 2019 PSES/SAFF dashboard for ROEB and its
 #   directorates.

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape2)
library(rmarkdown)

# Vector of question IDs of the format QXX or QXXx, where X is a digit from
#   0-9 and x is a letter from a-z (e.g. Q07 or Q59a)
QIDS <- unique(questions$ID)
N <- length(QIDS)

# Takes in a stacked bar ggplot 'p' and re-scales the colours in the bars based
#   on the expected responses for question 'q' ('lang' is one of "en" or "fr")
recolourBars <- function(q, p, lang) {
  
  # Variable 'atype':
  # Denotes the expected type of answer for question 'q'
  #
  #   1 = Strongly agree, Somewhat agree, Neither agree nor disagree, Somewhat
  #       disagree, Strongly disagree, Don't know, Not applicable
  #   2 = Always/Almost always, Often, Sometimes, Rarely, Never/Almost never,
  #       Don't know, Not applicable
  #   3 = Not at all, To a small extent, To a moderate extent, To a large
  #       extent, To a very large extent, Don't know, Not applicable
  #   4 = Yes, No, Not sure
  #   5 = To retire, To pursue another position within my department or agency,
  #       To pursue a position in another department or agency, To pursue a 
  #       position outside the federal public service, End of my term, casual 
  #       or student employment, Other
  #   6 = Yes, No
  #   7 = Very low, Low, Moderate, High, Very high, Don't know, Not applicable
  
  # Variable 'is.reversed':
  # Denotes whether or not the responses are sorted from most positive to least
  #   positive or least positive to most positive
  #
  #   0 = Not reversed (most positive first)
  #   1 = Reversed (least positive first)
  
  atype <- questions[questions$ID==q,"ANS_TYPE"][1]
  is.reversed <- questions[questions$ID==q,"IS_REV"][1]
  
  # Retrieves the appropriate set of answers for the given question
  if(lang=="en") {
    ans.set <- answers[,paste0("TYPE",atype,"_EN")] }
  else {
    ans.set <- answers[,paste0("TYPE",atype,"_FR")] }
  ans.set <- ans.set[ans.set!=""]
  
  # Re-colours bars based on 'atype' and 'is.reversed'
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

# Takes in a statistic 'val' and a category 'cat' and returns the colour of the
#   text box based on the value of the statistic ('cat' is one of "pos" or 
#   "change")
recolourBox <- function(val, cat) {
  if(cat=="pos") {
    if(is.na(val)) { return(HTML_COLOURS["null"]) }
    if(val>=80) { return(HTML_COLOURS["steelblue4"]) }
    if(val>=70) { return(HTML_COLOURS["steelblue3"]) }
    if(val>=60) { return(HTML_COLOURS["lightskyblue"]) }
    if(val>=50) { return(HTML_COLOURS["lightskyblue1"]) }
    return(HTML_COLOURS["aliceblue"]) }
  else {
    if(is.na(val)) { return(HTML_COLOURS["null"]) }
    if(val>=10) { return(HTML_COLOURS["palegreen3"]) }
    if(val>=5) { return(HTML_COLOURS["darkseagreen2"]) }
    if(val>-5) { return(HTML_COLOURS["snow"]) }
    if(val>-10) { return(HTML_COLOURS["pink"]) }
    return(HTML_COLOURS["lightcoral"]) }
}

# Takes in a statistic 'val' and a category 'cat' and returns the colour of the
#   text based on the value of the statistic ('cat' is one of "pos" or "change")
recolourText <- function(val, cat) {
  if(cat=="pos") {
    if(is.na(val)) { return(HTML_COLOURS["grey80"]) }
    if(val>=50) { return(HTML_COLOURS["white"]) }
    return(HTML_COLOURS["slategray3"]) }
  else {
    if(is.na(val)) { return(HTML_COLOURS["grey80"]) }
    if(val>=5) { return(HTML_COLOURS["palegreen4"]) }
    if(val>-5) { return(HTML_COLOURS["grey60"]) }
    return(HTML_COLOURS["firebrick"]) }
}

# Takes in a statistic 'val' and a category 'cat' and formats 'val' as an
#   appropriate text string based on the value of the statistic ('cat' is one of
#   "pos" or "change")
formatChangeText <- function(val, cat) {
  if(cat=="pos") {
    if(is.na(val)) { return("--") }
    return(as.character(val)) }
  else {
    if(is.na(val)) { return("--") }
    if(val>0) { return(paste0("+",val)) }
    return(as.character(val)) }
}

## ~~~~ Main server function ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

server <- function(input, output, session) {
  # Custom reactive value to track the language setting ("en" or "fr") when
  #   accessing the main display for the first time
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
    # Directorate selector (EN): if year is 2017, remove the Cannabis
    #   directorate
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
    
    # Filter (EN): select which data to use as reference for filtering questions
    #   to display
    #
    #   comparing against 2019 -> filter against ROEB
    #   comparing against 2018 -> filter against ROEB, 2018
    #   comparing against 2017 -> filter against ROEB, 2017
    
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
    # Filter (EN): select which directorate to be compared to reference
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
    # Directorate selector (FR): if year is 2017, remove the Cannabis
    #   directorate
    if(input$againstp4 %in% c(2019,2018)) {
      selectInput(inputId="directoratep4",label=NULL,multiple=TRUE,
                  width="629px",DIRECTORATES_FR,
                  selected=unname(DIRECTORATES_FR)) }
    else {
      selectInput(inputId="directoratep4",label=NULL,multiple=TRUE,
                  width="629px",DIRECTORATES_FR[1:(length(DIRECTORATES_FR)-1)],
                  selected=DIRECTORATES_FR[1:(length(DIRECTORATES_FR)-1)]) }})
  output$chng1_outputp4 <- renderUI({
    
    # Filter (FR): select which data to use as reference for filtering questions
    #   to display
    #
    #   comparing against 2019 -> filter against ROEB
    #   comparing against 2018 -> filter against ROEB, 2018
    #   comparing against 2017 -> filter against ROEB, 2017
    
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
    # Filter (FR): select which directorate to be compared to reference
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
    
    # Disables question selector if other, required selectors are NULL
    if(is.null(input$directoratep1) | is.null(input$change1p1)
       | is.null(input$change3p1)) {
      disabled(selectInput(inputId="questionp1",label=NULL,c(),width="600px")) }
    else {
      
      # Retrieves subset of data for the given years, directorates, and theme
      data.f <- data[data$SURVEYR %in% c(2019,input$againstp1)
                     & data$ORGANIZATION_EN %in% c("ROEB",input$directoratep1)
                     & data$THEME_EN==input$themep1,]
      # All questions found in the relevant data above
      allqs <- as.character(unique(data.f$QUESTION_EN))
      
      # Variable 'qs':
      # Accumulator vector of all the questions to be displayed to the user
      #   after applying all filtering criteria
      qs <- c()
      
      for(q in allqs) {
        # Question must be common to both years of data
        if(!(q %in% data.f[data.f$SURVEYR==2019,"QUESTION_EN"])
           | !(q %in% data.f[data.f$SURVEYR==input$againstp1,"QUESTION_EN"])) {
          next
        }
        
        # If filtering against ROEB by a particular directorate, the rate of
        #   positive responses for the question for that directorate and the
        #   rate of positive responses for the question for ROEB must differ by
        #   at least the user-specified amount in any direction
        
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
        
        # If filtering against ROEB by any directorate, the rate of positive
        #   responses for the question for any directorate and the rate of
        #   positive responses for the question for ROEB must differ by at
        #   least the user-specified amount in any direction
        
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
        
        # If filtering against 2018/2017 by a particular directorate, the rate
        #   of positive responses for the question for that directorate and the
        #   rate of positive responses for the question for 2018/2017 must
        #   differ by at least the user-specified amount in any direction
        
        else if(input$change3p1!="any") {
          res <- data.f[data.f$ORGANIZATION_EN==input$change3p1
                        & data.f$QUESTION_EN==q,]
          if(is.na(res[res$SURVEYR==2019,"POSITIVE"])
             | is.na(res[res$SURVEYR==input$againstp1,"POSITIVE"])
             | abs(res[res$SURVEYR==2019,"POSITIVE"]
                   -res[res$SURVEYR==input$againstp1,"POSITIVE"])
             < input$change2p1) {
            next }}
        
        # If filtering against 2018/2017 by any directorate, the rate of
        #   positive responses for the question for any directorate and the
        #   rate of positive responses for the question for 2018/2017 must
        #   differ by at least the user-specified amount in any direction
        
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
        
        # Question passes criteria and is added to 'qs'
        qs <- c(qs,q)
      }
      
      # Adds an option for "All questions (averaged)" across the entire theme
      #   selected if the AVERAGED values satisfy the appropriate filtering
      #   criteria (as described above)
      
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
      
      # Final output
      selectInput(inputId="questionp1",label=NULL,qs,width="600px") }
  })
  output$ques_outputp4 <- renderUI({
    
    # Disables question selector if other, required selectors are NULL
    if(is.null(input$directoratep4) | is.null(input$change1p4)
       | is.null(input$change3p4)) {
      disabled(selectInput(inputId="questionp4",label=NULL,c(),width="629px")) }
    else {
      # Retrieves subset of data for the given years, directorates, and theme
      data.f <- data[data$SURVEYR %in% c(2019,input$againstp4)
                     & data$ORGANIZATION_FR %in% c("DGORAL",input$directoratep4)
                     & data$THEME_FR==input$themep4,]
      # All questions found in the relevant data above
      allqs <- as.character(unique(data.f$QUESTION_FR))
      
      # Variable 'qs':
      # Accumulator vector of all the questions to be displayed to the user
      #   after applying all filtering criteria
      qs <- c()
      
      for(q in allqs) {
        # Question must be common to both years of data
        if(!(q %in% data.f[data.f$SURVEYR==2019,"QUESTION_FR"])
           | !(q %in% data.f[data.f$SURVEYR==input$againstp4,"QUESTION_FR"])) {
          next
        }
        
        # If filtering against ROEB by a particular directorate, the rate of
        #   positive responses for the question for that directorate and the
        #   rate of positive responses for the question for ROEB must differ by
        #   at least the user-specified amount in any direction
        
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
        
        # If filtering against ROEB by any directorate, the rate of positive
        #   responses for the question for any directorate and the rate of
        #   positive responses for the question for ROEB must differ by at
        #   least the user-specified amount in any direction
        
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
        
        # If filtering against 2018/2017 by a particular directorate, the rate
        #   of positive responses for the question for that directorate and the
        #   rate of positive responses for the question for 2018/2017 must
        #   differ by at least the user-specified amount in any direction
        
        else if(input$change3p4!="any") {
          res <- data.f[data.f$ORGANIZATION_FR==input$change3p4
                        & data.f$QUESTION_FR==q,]
          if(is.na(res[res$SURVEYR==2019,"POSITIVE"])
             | is.na(res[res$SURVEYR==input$againstp4,"POSITIVE"])
             | abs(res[res$SURVEYR==2019,"POSITIVE"]
                   -res[res$SURVEYR==input$againstp4,"POSITIVE"])
             < input$change2p4) {
            next }}
        
        # If filtering against 2018/2017 by any directorate, the rate of
        #   positive responses for the question for any directorate and the
        #   rate of positive responses for the question for 2018/2017 must
        #   differ by at least the user-specified amount in any direction
        
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
        
        # Question passes criteria and is added to 'qs'
        qs <- c(qs,q)
      }
      
      # Adds an option for "Toutes questions (moyennées)" across the entire
      #   theme selected if the AVERAGED values satisfy the appropriate
      #   filtering criteria (as described above)
      
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
      
      # Final output
      selectInput(inputId="questionp4",label=NULL,qs,width="629px") }
  })
  
  # ---- Individual observers --------------------------------------------------
  
  observeEvent(input$selecteng,{
    # Shows main content in English if user selects "Continue in English"
    rv$default.lang <- "en"
    js$showMainContent()
    js$showEng()
  })
  observeEvent(input$selectfr,{
    # Shows main content in French if user selects "Continuer en français"
    rv$default.lang <- "fr"
    js$showMainContent()
    js$showFr()
  })
  observeEvent(input$top,{ js$toTop() })
  observeEvent(input$language,{
    # Changes language
    if(input$language=="en") { js$showEng() }
    else { js$showFr() }
  })
  observeEvent(input$directoratep1,ignoreNULL=FALSE,{
    # While no directorates are selected, disable filtering selectors
    if(is.null(input$directoratep1)) {
      disable("change1p1")
      disable("change2p1")}
    else{
      enable("change1p1")
      enable("change2p1") }
  })
  observeEvent(input$directoratep4,ignoreNULL=FALSE,{
    # While no directorates are selected, disable filtering selectors
    if(is.null(input$directoratep4)) {
      disable("change1p4")
      disable("change2p4")}
    else{
      enable("change1p4")
      enable("change2p4") }
  })
  observeEvent(input$retrievep1,{
    # Displays visual output when user clicks on button to retrieve data
    
    req(input$questionp1)
    output$resultsp1 <- renderUI({
      isolate(
        
        # Variable 'd':
        # A data frame to be generated of all the data that will be displayed.
        #   Has the following columns:
        #
        #   SURVEYR (TYPE=character) - the year to which the observation belongs
        #   ORGANIZATION_EN (TYPE=factor) - e.g. "ROEB","LABS"
        #   POSITIVE (TYPE=integer) - proportion of positive responses for the
        #     selected question
        #   NEUTRAL (TYPE=integer) - proportion of neither-positive-nor-negative
        #     responses for the selected question
        #   NEGATIVE (TYPE=integer) - proportion of negative responses for the
        #     selected question
        
        # If question selected is all questions averaged:
        if(input$questionp1=="All questions (averaged)") {
          
          # ROEB and all directorates
          dirs <- c("ROEB",input$directoratep1)
          # Stores averaged positive, negative, and neutral responses in 2019
          avgPos <- avgNeg <- avgNeut <- rep(NA,length(dirs))
          # Stores averaged positive, negative, and neutral responses in
          #   previous year of comparison
          avgPosOld <- avgNeutOld <- avgNegOld <- rep(NA,length(dirs))
          # Retrieves subset of data for the given years, directorates, and
          #   theme
          data.f <- data[data$SURVEYR %in% c(2019,input$againstp1)
                         & data$ORGANIZATION_EN %in% dirs
                         & data$THEME_EN==input$themep1,]
          # All questions found in the relevant data above
          allqs <- as.character(unique(data.f$QUESTION_EN))
          
          # Variable 'qs':
          # Accumulator vector of all the questions to be averaged over after
          #   factoring in all filtering criteria
          qs <- c()
          
          for(q in allqs) {
            # Question must be common to both years of data
            if(q %in% data.f[data.f$SURVEYR==2019,"QUESTION_EN"]
               & q %in% data.f[data.f$SURVEYR==input$againstp1,"QUESTION_EN"]) {
              qs <- c(qs,q) }}
          
          # For ROEB and each of its directorates, calculates the average
          #   positive, neutral, and negative responses for both years and
          #   stores them in order in the appropriate vectors
          
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
          
          # If displaying only data for 2019, creates a data frame 'd' with
          #   observations from 2019 only
          if(input$againstp1==2019) {
            d <- data.frame(
              SURVEYR=c(rep(2019,length(dirs))),
              ORGANIZATION_EN=dirs,
              POSITIVE=avgPos,
              NEUTRAL=avgNeut,
              NEGATIVE=avgNeg) }
          
          # Otherwise, creates a data frame 'd' with observations from 2019 and
          #   the previous year of comparison
          else {
            d <- data.frame(
              SURVEYR=c(rep(2019,length(dirs)),
                        rep(input$againstp1,length(dirs))),
              ORGANIZATION_EN=rep(dirs,2),
              POSITIVE=c(avgPos,avgPosOld),
              NEUTRAL=c(avgNeut,avgNeutOld),
              NEGATIVE=c(avgNeg,avgNegOld))
          }}
        
        # If question selected is not all questions averaged, queries directly
        #   from complete dataset to generate 'd'
        else {
          d <- data[data$SURVEYR %in% c(2019,input$againstp1)
                    & data$ORGANIZATION_EN %in% c("ROEB",input$directoratep1)
                    & data$QUESTION_EN==input$questionp1,
                    c("SURVEYR","ORGANIZATION_EN","POSITIVE","NEUTRAL",
                      "NEGATIVE")]
        })
      
      # Converts organizations to factors to ensure consistent ordering when
      #   plotted
      isolate(d$ORGANIZATION_EN <-
                factor(d$ORGANIZATION_EN,levels=c("ROEB",input$directoratep1)))
      # Converts years to strings to make them categorical rather than numerical
      d$SURVEYR <- as.character(d$SURVEYR)
      # Converts 'd' to an easily-plottable format for stacked bar charts
      d.m <- melt(d,c("ORGANIZATION_EN","SURVEYR"))
      
      # HTML strings:
      # Store HTML code as text to be rendered in the output.
      #
      #   'htmlstr' - first column of output in the right-hand panel to be
      #               stored as a one-columned table
      #             - user is comparing data from 2019 against data from 2018
      #               or 2017
      #   'htmlstr2' - second column of output in the right-hand panel to be
      #                stored as a one-columned table
      #              - user is comparing data from 2019 against data from 2018
      #                or 2017
      #   'htmlstr3' - first column of output in the right-hand panel to be
      #                stored as a one-columned table
      #              - user is not comparing data from 2019 against any other
      #                year
      #   'htmlstr4' - second column of output in the right-hand panel to be
      #                stored as a one-columned table
      #              - user is not comparing data from 2019 against any other
      #                year
      
      # HTML templates:
      # Store HTML code templates for each row as text, to be used with
      #   `sprintf` to populate each row
      #
      #   'rowTemplate' - generates one row in the first column of output in
      #                   the right-hand panel
      #                 - user is comparing data from 2019 against data from
      #                   2018 or 2017
      #   'rowTemplate2' - generates one row in the second column of output in
      #                    the right-hand panel
      #                  - user is comparing data from 2019 against data from
      #                    2018 or 2017
      #   'rowTemplate3' - generates one row in the first column of output in
      #                    the right-hand panel
      #                  - user is not comparing data from 2019 against any
      #                    other year
      #   'rowTemplate4' - generates one row in the second column of output in
      #                    the right-hand panel
      #                  - user is not comparing data from 2019 against any
      #                    other year
      
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
          
          # Variable 'positive':
          # Proportion of positive responses for the directorate in 2019
          
          # Variable 'change1':
          # Change in proportion of positive responses for the directorate in
          #   2019 vs ROEB
          
          # Variable 'change2':
          # Change in proportion of positive responses for the directorate in
          #   2019 vs itself in the previous year of comparison
          
          positive <- d[d$SURVEYR=="2019" & d$ORGANIZATION_EN==dir,"POSITIVE"]
          change1 <- NA
          change2 <- d[d$SURVEYR=="2019" & d$ORGANIZATION_EN==dir,"POSITIVE"]-
            d[d$SURVEYR==input$againstp1 & d$ORGANIZATION_EN==dir,"POSITIVE"]
          if(dir != "ROEB") {
            change1 <- d[d$SURVEYR=="2019" & d$ORGANIZATION_EN==dir,"POSITIVE"]-
              d[d$SURVEYR=="2019" & d$ORGANIZATION_EN=="ROEB","POSITIVE"] }
          
          # Populates HTML for first column with proportions of positive
          #   responses; darker blue backgrounds for higher proportions
          #   (categories: <50%, 50-59%, 60-69%, 70-79%, 80-100%)
          
          htmlstr <-
            paste0(htmlstr,
                   sprintf(rowTemplate,recolourBox(positive,"pos"),
                           recolourText(positive,"pos"),
                           recolourText(positive,"pos"),
                           formatChangeText(positive,"pos")))
          htmlstr3 <-
            paste0(htmlstr3,
                   sprintf(rowTemplate3,recolourBox(positive,"pos"),
                           recolourText(positive,"pos"),
                           recolourText(positive,"pos"),
                           formatChangeText(positive,"pos")))
          
          # Adds change data vs ROEB to HTML for second column; darker green
          #   for larger positive changes and darker red for larger negative
          #   changes (categories: -10% or more extreme, -9 to -5%, -4 to 4%,
          #   +5 to +9%, +10% or more extreme)
          
          htmlstr2 <-
            paste0(htmlstr2,
                   sprintf(rowTemplate2,recolourBox(change1,"change"),
                           recolourText(change1,"change"),"ROEB",
                           recolourText(change1,"change"),
                           formatChangeText(change1,"change")))
          
          htmlstr4 <-
            paste0(htmlstr4,
                   sprintf(rowTemplate4,recolourBox(change1,"change"),
                           recolourText(change1,"change"),
                           recolourText(change1,"change"),
                           formatChangeText(change1,"change")))
          
          # Adds change data vs 2018/2017 to HTML for second column; darker
          #   green for larger positive changes and darker red for larger
          #   negative changes (categories: -10% or more extreme, -9 to -5%, -4
          #   to 4%, +5 to +9%, +10% or more extreme)
          
          htmlstr2 <-
            paste0(htmlstr2,
                   sprintf(rowTemplate2,recolourBox(change2,"change"),
                           recolourText(change2,"change"),input$againstp1,
                           recolourText(change2,"change"),
                           formatChangeText(change2,"change")))
        })
      
      # Closes all HTML tags
      htmlstr <- paste0(htmlstr,"</table></div>")
      htmlstr2 <- paste0(htmlstr2,"</table></div>")
      htmlstr3 <- paste0(htmlstr3,"</table></div>")
      htmlstr4 <- paste0(htmlstr4,"</table></div>")
      
      # Stacked bar chart using ggplot with facets for each directorate
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
        isolate(h3(input$themep1)), # theme heading
        isolate(h5(input$questionp1)), # question sub-heading
        br(),
        isolate(
          
          # Renders ggplot in left-hand panel; height dimension depends on
          #   the number of directorates included and whether or not the data
          #   from 2019 is being compared to data from a previous year
          
          # `suppressWarnings({ print(p) })` displays the plot but does not
          #   allow any warnings to print to the Console (in the case of missing
          #   data, which are omitted automatically but which generate warning
          #   messages)
          
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
          
          # Renders the appropriate HTML in the right-hand panel
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
    # Displays visual output when user clicks on button to retrieve data
    
    req(input$questionp4)
    output$resultsp4 <- renderUI({
      isolate(
        
        # Variable 'd':
        # A data frame to be generated of all the data that will be displayed.
        #   Has the following columns:
        #
        #   SURVEYR (TYPE=character) - the year to which the observation belongs
        #   ORGANIZATION_EN (TYPE=factor) - e.g. "ROEB","LABS"
        #   POSITIVE (TYPE=integer) - proportion of positive responses for the
        #     selected question
        #   NEUTRAL (TYPE=integer) - proportion of neither-positive-nor-negative
        #     responses for the selected question
        #   NEGATIVE (TYPE=integer) - proportion of negative responses for the
        #     selected question
        
        # If question selected is all questions averaged:
        if(input$questionp4=="Toutes questions (moyennées)") {
          
          # ROEB and all directorates
          dirs <- c("DGORAL",input$directoratep4)
          # Stores averaged positive, negative, and neutral responses in 2019
          avgPos <- avgNeg <- avgNeut <- rep(NA,length(dirs))
          # Stores averaged positive, negative, and neutral responses in
          #   previous year of comparison
          avgPosOld <- avgNeutOld <- avgNegOld <- rep(NA,length(dirs))
          # Retrieves subset of data for the given years, directorates, and
          #   theme
          data.f <- data[data$SURVEYR %in% c(2019,input$againstp4)
                         & data$ORGANIZATION_FR %in% dirs
                         & data$THEME_FR==input$themep4,]
          # All questions found in the relevant data above
          allqs <- as.character(unique(data.f$QUESTION_FR))
          
          # Variable 'qs':
          # Accumulator vector of all the questions to be averaged over after
          #   factoring in all filtering criteria
          qs <- c()
          
          for(q in allqs) {
            # Question must be common to both years of data
            if(q %in% data.f[data.f$SURVEYR==2019,"QUESTION_FR"]
               & q %in% data.f[data.f$SURVEYR==input$againstp4,"QUESTION_FR"]) {
              qs <- c(qs,q) }}
          
          # For ROEB and each of its directorates, calculates the average
          #   positive, neutral, and negative responses for both years and
          #   stores them in order in the appropriate vectors
          
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
          
          # If displaying only data for 2019, creates a data frame 'd' with
          #   observations from 2019 only
          if(input$againstp4==2019) {
            d <- data.frame(
              SURVEYR=c(rep(2019,length(dirs))),
              ORGANIZATION_FR=dirs,
              POSITIVE=avgPos,
              NEUTRAL=avgNeut,
              NEGATIVE=avgNeg) }
          
          # Otherwise, creates a data frame 'd' with observations from 2019 and
          #   the previous year of comparison
          else {
            d <- data.frame(
              SURVEYR=c(rep(2019,length(dirs)),
                        rep(input$againstp4,length(dirs))),
              ORGANIZATION_FR=rep(dirs,2),
              POSITIVE=c(avgPos,avgPosOld),
              NEUTRAL=c(avgNeut,avgNeutOld),
              NEGATIVE=c(avgNeg,avgNegOld))
          }}
        
        # If question selected is not all questions averaged, queries directly
        #   from complete dataset to generate 'd'
        else {
          d <- data[data$SURVEYR %in% c(2019,input$againstp4)
                    & data$ORGANIZATION_FR %in% c("DGORAL",input$directoratep4)
                    & data$QUESTION_FR==input$questionp4,
                    c("SURVEYR","ORGANIZATION_FR","POSITIVE","NEUTRAL",
                      "NEGATIVE")]
        })
      
      # Converts organizations to factors to ensure consistent ordering when
      #   plotted
      isolate(d$ORGANIZATION_FR <-
                factor(d$ORGANIZATION_FR,levels=c("DGORAL",input$directoratep4))
              )
      # Converts years to strings to make them categorical rather than numerical
      d$SURVEYR <- as.character(d$SURVEYR)
      # Converts 'd' to an easily-plottable format for stacked bar charts
      d.m <- melt(d,c("ORGANIZATION_FR","SURVEYR"))
      
      # HTML strings:
      # Store HTML code as text to be rendered in the output.
      #
      #   'htmlstr' - first column of output in the right-hand panel to be
      #               stored as a one-columned table
      #             - user is comparing data from 2019 against data from 2018
      #               or 2017
      #   'htmlstr2' - second column of output in the right-hand panel to be
      #                stored as a one-columned table
      #              - user is comparing data from 2019 against data from 2018
      #                or 2017
      #   'htmlstr3' - first column of output in the right-hand panel to be
      #                stored as a one-columned table
      #              - user is not comparing data from 2019 against any other
      #                year
      #   'htmlstr4' - second column of output in the right-hand panel to be
      #                stored as a one-columned table
      #              - user is not comparing data from 2019 against any other
      #                year
      
      # HTML templates:
      # Store HTML code templates for each row as text, to be used with
      #   `sprintf` to populate each row
      #
      #   'rowTemplate' - generates one row in the first column of output in
      #                   the right-hand panel
      #                 - user is comparing data from 2019 against data from
      #                   2018 or 2017
      #   'rowTemplate2' - generates one row in the second column of output in
      #                    the right-hand panel
      #                  - user is comparing data from 2019 against data from
      #                    2018 or 2017
      #   'rowTemplate3' - generates one row in the first column of output in
      #                    the right-hand panel
      #                  - user is not comparing data from 2019 against any
      #                    other year
      #   'rowTemplate4' - generates one row in the second column of output in
      #                    the right-hand panel
      #                  - user is not comparing data from 2019 against any
      #                    other year
      
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
          
          # Variable 'positive':
          # Proportion of positive responses for the directorate in 2019
          
          # Variable 'change1':
          # Change in proportion of positive responses for the directorate in
          #   2019 vs ROEB
          
          # Variable 'change2':
          # Change in proportion of positive responses for the directorate in
          #   2019 vs itself in the previous year of comparison
          
          positive <- d[d$SURVEYR=="2019" & d$ORGANIZATION_FR==dir,"POSITIVE"]
          change1 <- NA
          change2 <- d[d$SURVEYR=="2019" & d$ORGANIZATION_FR==dir,"POSITIVE"]-
            d[d$SURVEYR==input$againstp4 & d$ORGANIZATION_FR==dir,"POSITIVE"]
          if(dir != "DGORAL") {
            change1 <- d[d$SURVEYR=="2019" & d$ORGANIZATION_FR==dir,"POSITIVE"]-
              d[d$SURVEYR=="2019" & d$ORGANIZATION_FR=="DGORAL","POSITIVE"] }
          
          # Populates HTML for first column with proportions of positive
          #   responses; darker blue backgrounds for higher proportions
          #   (categories: <50%, 50-59%, 60-69%, 70-79%, 80-100%)
          
          htmlstr <-
            paste0(htmlstr,
                   sprintf(rowTemplate,recolourBox(positive,"pos"),
                           recolourText(positive,"pos"),
                           recolourText(positive,"pos"),
                           formatChangeText(positive,"pos")))
          htmlstr3 <-
            paste0(htmlstr3,
                   sprintf(rowTemplate3,recolourBox(positive,"pos"),
                           recolourText(positive,"pos"),
                           recolourText(positive,"pos"),
                           formatChangeText(positive,"pos")))
          
          # Adds change data vs ROEB to HTML for second column; darker green
          #   for larger positive changes and darker red for larger negative
          #   changes (categories: -10% or more extreme, -9 to -5%, -4 to 4%,
          #   +5 to +9%, +10% or more extreme)
          
          htmlstr2 <-
            paste0(htmlstr2,
                   sprintf(rowTemplate2,recolourBox(change1,"change"),
                           recolourText(change1,"change"),"la DGORAL",
                           recolourText(change1,"change"),
                           formatChangeText(change1,"change")))
          
          htmlstr4 <-
            paste0(htmlstr4,
                   sprintf(rowTemplate4,recolourBox(change1,"change"),
                           recolourText(change1,"change"),
                           recolourText(change1,"change"),
                           formatChangeText(change1,"change")))
          
          # Adds change data vs 2018/2017 to HTML for second column; darker
          #   green for larger positive changes and darker red for larger
          #   negative changes (categories: -10% or more extreme, -9 to -5%, -4
          #   to 4%, +5 to +9%, +10% or more extreme)
          
          htmlstr2 <-
            paste0(htmlstr2,
                   sprintf(rowTemplate2,recolourBox(change2,"change"),
                           recolourText(change2,"change"),input$againstp4,
                           recolourText(change2,"change"),
                           formatChangeText(change2,"change")))
        })
      
      # Closes all HTML tags
      htmlstr <- paste0(htmlstr,"</table></div>")
      htmlstr2 <- paste0(htmlstr2,"</table></div>")
      htmlstr3 <- paste0(htmlstr3,"</table></div>")
      htmlstr4 <- paste0(htmlstr4,"</table></div>")
      
      # Stacked bar chart using ggplot with facets for each directorate
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
        isolate(h3(input$themep4)), # theme heading
        isolate(h5(input$questionp4)), # question sub-heading
        br(),
        isolate(
          
          # Renders ggplot in left-hand panel; height dimension depends on
          #   the number of directorates included and whether or not the data
          #   from 2019 is being compared to data from a previous year
          
          # `suppressWarnings({ print(p) })` displays the plot but does not
          #   allow any warnings to print to the Console (in the case of missing
          #   data, which are omitted automatically but which generate warning
          #   messages)
          
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
          
          # Renders the appropriate HTML in the right-hand panel
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
  observeEvent(input$retrievepA,{
    # Displays visual output when user clicks on button to retrieve data
    
    df <- data[data$ORGANIZATION_EN==input$directoratepA,]
    dirs <- c("Regulatory Operations and Enforcement Branch (ROEB)"="ROEB",
              DIRECTORATES_EN)
    
    # Variable 'htmlstr':
    # Stores HTML text used to generate the table display
    
    htmlstr <-
      "<table style='margin:10px 10px 20px 10px; width:99%'>
         <tr class='heading'>
           <td style='width:5%;'></td>
           <td style='width:50%;'></td>
           <td style='width:11%; text-align:center;'>
             <strong>Positive in 2019</strong>
           </td>
           <td style='width:11%; text-align:center;'>
             <strong>Versus ROEB in 2019</strong>
           </td>
           <td style='width:11%; text-align:center;'>
             <strong>Average change from 2018*</strong>
           </td>
           <td style='width:11%; text-align:center;'>
             <strong>Average change from 2017*</strong>
           </td>
         </tr>"
    
    # Variable 'rowTemplate':
    # HTML format to generate one row of the table
    
    rowTemplate <-
      "<tr class='body'>
         <td style='width:5%%;'><button>+</button></td>
         <td style='width:50%%;'><strong>%s</strong></td>
         <td style='width:11%%;'>
           <div class='well' style='padding:1px 15%% 0 15%%;background-color:%s;
             height:40px; text-align:right;'>
             <strong style='color:%s; font-size:18pt;'>
               %s%%
             </strong>
           </div>
         </td>
         <td style='width:11%%;'>
           <div class='well' style='padding:1px 15%% 0 15%%;background-color:%s;
             height:40px; text-align:right;'>
             <strong style='color:%s; font-size:18pt;'>
               %s
             </strong>
           </div>
         </td>
         <td style='width:11%%;'>
           <div class='well' style='padding:1px 15%% 0 15%%;background-color:%s;
             height:40px; text-align:right;'>
             <strong style='color:%s; font-size:18pt;'>
               %s
             </strong>
           </div>
         </td>
         <td style='width:11%%;'>
           <div class='well' style='padding:1px 15%% 0 15%%;background-color:%s;
             height:40px; text-align:right;'>
             <strong style='color:%s; font-size:18pt;'>
               %s
             </strong>
           </div>
         </td>
       </tr>"
    
    # Populates the rows of the table
    for(thm in themes$THEME_EN) {
      
      # 'pos' takes average positive over the theme in 2019
      d <- df[df$SURVEYR==2019 & df$THEME_EN==thm,]
      if(is.nan(mean(d$POSITIVE,na.rm=TRUE))) { pos <- NA }
      else { pos <- round(mean(d$POSITIVE,na.rm=TRUE),0) }
      
      # 'vs_roeb' takes change in average positive in directorate from average
      #   positive in ROEB for 2019
      isolate(
        if(input$directoratepA=="ROEB") { vs_roeb <- NA }
        else {
          d <- data[data$SURVEYR==2019 & data$THEME_EN==thm
                    & data$ORGANIZATION_EN=="ROEB",]
          vs_roeb <- pos - round(mean(d$POSITIVE,na.rm=TRUE),0)
          if(is.nan(vs_roeb)) { vs_roeb <- NA } })
      
      # 'vs_2018' takes change in average positive in directorate in 2019 from
      #   average positive in same directorate in 2018 (questions must be
      #   common to both years in order to count)
      
      data.f <- data[data$SURVEYR %in% c(2019,2018)
                     & data$ORGANIZATION_EN==isolate(input$directoratepA)
                     & data$THEME_EN==thm,]
      allqs <- as.character(unique(data.f$QUESTION_EN))
      qs <- c()
      
      for(q in allqs) {
        # Question must be common to both years of data
        if(q %in% data.f[data.f$SURVEYR==2019,"QUESTION_EN"]
           & q %in% data.f[data.f$SURVEYR==2018,"QUESTION_EN"]) {
          qs <- c(qs,q) }}
      
      d1 <- data.f[data.f$SURVEYR==2019 & data.f$QUESTION_EN %in% qs,]
      d2 <- data.f[data.f$SURVEYR==2018 & data.f$QUESTION_EN %in% qs,]
      vs_2018 <- round(mean(d1$POSITIVE,na.rm=TRUE),0) -
        round(mean(d2$POSITIVE,na.rm=TRUE),0)
      if(is.nan(vs_2018)) { vs_2018 <- NA }
      
      # 'vs_2017' takes change in average positive in directorate in 2019 from
      #   average positive in same directorate in 2017 (questions must be
      #   common to both years in order to count)
      
      data.f <- data[data$SURVEYR %in% c(2019,2017)
                     & data$ORGANIZATION_EN==isolate(input$directoratepA)
                     & data$THEME_EN==thm,]
      allqs <- as.character(unique(data.f$QUESTION_EN))
      qs <- c()
      
      for(q in allqs) {
        # Question must be common to both years of data
        if(q %in% data.f[data.f$SURVEYR==2019,"QUESTION_EN"]
           & q %in% data.f[data.f$SURVEYR==2017,"QUESTION_EN"]) {
          qs <- c(qs,q) }}
      
      d1 <- data.f[data.f$SURVEYR==2019 & data.f$QUESTION_EN %in% qs,]
      d2 <- data.f[data.f$SURVEYR==2017 & data.f$QUESTION_EN %in% qs,]
      vs_2017 <- round(mean(d1$POSITIVE,na.rm=TRUE),0) -
        round(mean(d2$POSITIVE,na.rm=TRUE),0)
      if(is.nan(vs_2017)) { vs_2017 <- NA }
      
      # Generates HTML text for next row and adds it to the table
      newstr <- 
        sprintf(rowTemplate,thm,recolourBox(pos,"pos"),recolourText(pos,"pos"),
                formatChangeText(pos,"pos"),recolourBox(vs_roeb,"change"),
                recolourText(vs_roeb,"change"),
                formatChangeText(vs_roeb,"change"),
                recolourBox(vs_2018,"change"),recolourText(vs_2018,"change"),
                formatChangeText(vs_2018,"change"),
                recolourBox(vs_2017,"change"),recolourText(vs_2017,"change"),
                formatChangeText(vs_2017,"change"))
      htmlstr <- paste0(htmlstr,newstr)
    }
    htmlstr <- paste0(htmlstr,"</table>")
    
    # Output results
    output$resultspA <- renderUI({
      isolate(
        tagList(
          h3(names(dirs)[dirs==input$directoratepA]),
          h5("Average changes by theme"),
          br(),
          box(status=NULL,solidHeader=TRUE,width=12,HTML(htmlstr)),
          br(),
          p("*Only questions common to both years were included in
            calculations.")
        )
      )})
  })
  observeEvent(input$retrievepB,{
    # Displays visual output when user clicks on button to retrieve data
    
    df <- data[data$ORGANIZATION_FR==input$directoratepB,]
    dirs <- c("Direction générale des opérations réglementaires et de l’application de la loi (DGORAL)"=
                "DGORAL",
              DIRECTORATES_FR)
    
    # Variable 'htmlstr':
    # Stores HTML text used to generate the table display
    
    htmlstr <-
      "<table style='margin:10px 10px 20px 10px; width:99%'>
         <tr class='heading'>
           <td style='width:5%;'></td>
           <td style='width:50%;'></td>
           <td style='width:11%; text-align:center;'>
             <strong>Positives in 2019</strong>
           </td>
           <td style='width:11%; text-align:center;'>
             <strong>Variation par rapport à la DGORAL en 2019</strong>
           </td>
           <td style='width:11%; text-align:center;'>
             <strong>Variation moyenne par rapport à 2018*</strong>
           </td>
           <td style='width:11%; text-align:center;'>
             <strong>Variation moyenne par rapport à 2017*</strong>
           </td>
         </tr>"
    
    # Variable 'rowTemplate':
    # HTML format to generate one row of the table
    
    rowTemplate <-
      "<tr class='body'>
         <td style='width:5%%;'><button>+</button></td>
         <td style='width:50%%;'><strong>%s</strong></td>
         <td style='width:11%%;'>
           <div class='well' style='padding:1px 15%% 0 15%%;background-color:%s;
             height:40px; text-align:right;'>
             <strong style='color:%s; font-size:18pt;'>
               %s%%
             </strong>
           </div>
         </td>
         <td style='width:11%%;'>
           <div class='well' style='padding:1px 15%% 0 15%%;background-color:%s;
             height:40px; text-align:right;'>
             <strong style='color:%s; font-size:18pt;'>
               %s
             </strong>
           </div>
         </td>
         <td style='width:11%%;'>
           <div class='well' style='padding:1px 15%% 0 15%%;background-color:%s;
             height:40px; text-align:right;'>
             <strong style='color:%s; font-size:18pt;'>
               %s
             </strong>
           </div>
         </td>
         <td style='width:11%%;'>
           <div class='well' style='padding:1px 15%% 0 15%%;background-color:%s;
             height:40px; text-align:right;'>
             <strong style='color:%s; font-size:18pt;'>
               %s
             </strong>
           </div>
         </td>
       </tr>"
    
    # Populates the rows of the table
    for(thm in themes$THEME_FR) {
      
      # 'pos' takes average positive over the theme in 2019
      d <- df[df$SURVEYR==2019 & df$THEME_FR==thm,]
      if(is.nan(mean(d$POSITIVE,na.rm=TRUE))) { pos <- NA }
      else { pos <- round(mean(d$POSITIVE,na.rm=TRUE),0) }
      
      # 'vs_roeb' takes change in average positive in directorate from average
      #   positive in ROEB for 2019
      isolate(
        if(input$directoratepB=="DGORAL") { vs_roeb <- NA }
        else {
          d <- data[data$SURVEYR==2019 & data$THEME_FR==thm
                    & data$ORGANIZATION_FR=="DGORAL",]
          vs_roeb <- pos - round(mean(d$POSITIVE,na.rm=TRUE),0)
          if(is.nan(vs_roeb)) { vs_roeb <- NA } })
      
      # 'vs_2018' takes change in average positive in directorate in 2019 from
      #   average positive in same directorate in 2018 (questions must be
      #   common to both years in order to count)
      
      data.f <- data[data$SURVEYR %in% c(2019,2018)
                     & data$ORGANIZATION_FR==isolate(input$directoratepB)
                     & data$THEME_FR==thm,]
      allqs <- as.character(unique(data.f$QUESTION_FR))
      qs <- c()
      
      for(q in allqs) {
        # Question must be common to both years of data
        if(q %in% data.f[data.f$SURVEYR==2019,"QUESTION_FR"]
           & q %in% data.f[data.f$SURVEYR==2018,"QUESTION_FR"]) {
          qs <- c(qs,q) }}
      
      d1 <- data.f[data.f$SURVEYR==2019 & data.f$QUESTION_FR %in% qs,]
      d2 <- data.f[data.f$SURVEYR==2018 & data.f$QUESTION_FR %in% qs,]
      vs_2018 <- round(mean(d1$POSITIVE,na.rm=TRUE),0) -
        round(mean(d2$POSITIVE,na.rm=TRUE),0)
      if(is.nan(vs_2018)) { vs_2018 <- NA }
      
      # 'vs_2017' takes change in average positive in directorate in 2019 from
      #   average positive in same directorate in 2017 (questions must be
      #   common to both years in order to count)
      
      data.f <- data[data$SURVEYR %in% c(2019,2017)
                     & data$ORGANIZATION_FR==isolate(input$directoratepB)
                     & data$THEME_FR==thm,]
      allqs <- as.character(unique(data.f$QUESTION_FR))
      qs <- c()
      
      for(q in allqs) {
        # Question must be common to both years of data
        if(q %in% data.f[data.f$SURVEYR==2019,"QUESTION_FR"]
           & q %in% data.f[data.f$SURVEYR==2017,"QUESTION_FR"]) {
          qs <- c(qs,q) }}
      
      d1 <- data.f[data.f$SURVEYR==2019 & data.f$QUESTION_FR %in% qs,]
      d2 <- data.f[data.f$SURVEYR==2017 & data.f$QUESTION_FR %in% qs,]
      vs_2017 <- round(mean(d1$POSITIVE,na.rm=TRUE),0) -
        round(mean(d2$POSITIVE,na.rm=TRUE),0)
      if(is.nan(vs_2017)) { vs_2017 <- NA }
      
      # Generates HTML text for next row and adds it to the table
      newstr <- 
        sprintf(rowTemplate,thm,recolourBox(pos,"pos"),recolourText(pos,"pos"),
                formatChangeText(pos,"pos"),recolourBox(vs_roeb,"change"),
                recolourText(vs_roeb,"change"),
                formatChangeText(vs_roeb,"change"),
                recolourBox(vs_2018,"change"),recolourText(vs_2018,"change"),
                formatChangeText(vs_2018,"change"),
                recolourBox(vs_2017,"change"),recolourText(vs_2017,"change"),
                formatChangeText(vs_2017,"change"))
      htmlstr <- paste0(htmlstr,newstr)
    }
    htmlstr <- paste0(htmlstr,"</table>")
    
    # Output results
    output$resultspB <- renderUI({
      isolate(
        tagList(
          h3(names(dirs)[dirs==input$directoratepB]),
          h5("Variations moyennes par thème"),
          br(),
          box(status=NULL,solidHeader=TRUE,width=12,HTML(htmlstr)),
          br(),
          p("*Les calculs sont basés sur les questions communes entre les deux
            années seulement.")
        )
      )})
  })
  
  # ---- Plot outputs ----------------------------------------------------------
  
  output$graphsp2 <- renderUI({
    req(input$yearp2,input$themep2)
    # Retrieves subset of data for the given year and theme
    data.f <- data[data$SURVEYR==input$yearp2 & data$THEME_EN==input$themep2,]
    
    # Retrieves all questions contained in 'data.f' by ID
    qs <- as.character(unique(data.f$QID))
    lapply(qs, function(q) {
      
      # For each ID, retrieves question text, answer set, and all data
      #   pertaining to the question ('res')
      qtitle <- as.character(questions[questions$ID==q,"EN"][1])
      res <- data.f[data.f$QID==q,]
      ans.set <-
        answers[,paste0("TYPE",questions[questions$ID==q,"ANS_TYPE"][1],"_EN")]
      ans.set <- ans.set[ans.set!=""]
      
      # Shapes data in 'res' into a data structure where rows correspond to
      #   organizations and columns correspond to each answer in the answer set
      v <- c()
      n <- length(ans.set)
      for(s in paste0("ANSWER",1:n)) {
        v <- c(v,res[,s]) }
      df <- structure(v,
                      .Dim=c(nrow(res),n),
                      .Dimnames=list(as.character(res$ORGANIZATION_EN),ans.set))
      # Reshapes the data again to be easily-plottable as a stacked bar chart
      #   in ggplot
      df.m <- melt(df)
      df.m <- rename(df.m,Unit=Var1,Responses=Var2,Proportion=value)
      
      box(title=qtitle,status="primary",solidHeader=TRUE,width=12,
          collapsible=TRUE,collapsed=TRUE,
          
          # `render_delayed` prevents the text from appearing before the plot
          render_delayed({
            p("(Percentages may not add to 100 due to rounding)")
          }),
          
          # ggplot output with suppressed warnings (in the case of missing data,
          #   which are automatically omitted but which generate warning
          #   messages)
          renderPlot(height=400,{
            p <- ggplot(df.m, aes(x=Unit, y=Proportion, fill=Responses)) +
              geom_bar(stat="identity", position=position_stack(reverse=TRUE)) +
              labs(x="", y="Proportion responded (%)", fill="Responses") +
              geom_text(size=3, position=position_stack(vjust=0.5,reverse=TRUE),
                        aes(label=Proportion)) +
              coord_flip() +
              theme_minimal()
            suppressWarnings({ print(recolourBars(q,p,"en")) })
          }),
          
          # table of the number of responses by organization
          renderTable(rownames=TRUE, align="c", width="100%", {
            dtb <- data.frame(as.list(rev(res$COUNT)),
                              row.names="Number of responses")
            names(dtb) <- rev(as.character(res$ORGANIZATION_EN))
            return(dtb)
          }))
    })
  })
  output$graphsp5 <- renderUI({
    req(input$yearp5,input$themep5)
    # Retrieves subset of data for the given year and theme
    data.f <- data[data$SURVEYR==input$yearp5 & data$THEME_FR==input$themep5,]
    
    # Retrieves all questions contained in 'data.f' by ID
    qs <- as.character(unique(data.f$QID))
    lapply(qs, function(q) {
      
      # For each ID, retrieves question text, answer set, and all data
      #   pertaining to the question ('res')
      qtitle <- as.character(questions[questions$ID==q,"FR"][1])
      res <- data.f[data.f$QID==q,]
      ans.set <-
        answers[,paste0("TYPE",questions[questions$ID==q,"ANS_TYPE"][1],"_FR")]
      ans.set <- ans.set[ans.set!=""]
      
      # Shapes data in 'res' into a data structure where rows correspond to
      #   organizations and columns correspond to each answer in the answer set
      v <- c()
      n <- length(ans.set)
      for(s in paste0("ANSWER",1:n)) {
        v <- c(v,res[,s]) }
      df <- structure(v,
                      .Dim=c(nrow(res),n),
                      .Dimnames=list(as.character(res$ORGANIZATION_FR),ans.set))
      # Reshapes the data again to be easily-plottable as a stacked bar chart
      #   in ggplot
      df.m <- melt(df)
      df.m <- rename(df.m,Unit=Var1,Responses=Var2,Proportion=value)
      
      box(id=paste0("b",which(QIDS==q)+N),title=qtitle,status="primary",
          solidHeader=TRUE,width=12,collapsible=TRUE,collapsed=TRUE,
          
          # `render_delayed` prevents the text from appearing before the plot
          render_delayed({
            p("(Les pourcentages peuvent ne pas totaliser 100 en raison
              d'erreurs dans les arrondissements)")
          }),
          
          # ggplot output with suppressed warnings (in the case of missing data,
          #   which are automatically omitted but which generate warning
          #   messages)
          renderPlot(height=400,{
            p <- ggplot(df.m, aes(x=Unit, y=Proportion, fill=Responses)) +
              geom_bar(stat="identity", position=position_stack(reverse=TRUE)) +
              labs(x="", y="Pourcentage répondu (%)", fill="Réponses") +
              geom_text(size=3, position=position_stack(vjust=0.5,reverse=TRUE),
                        aes(label=Proportion)) +
              coord_flip() +
              theme_minimal()
            suppressWarnings({ print(recolourBars(q,p,"fr")) })
          }),
          
          # table of the number of responses by organization
          renderTable(rownames=TRUE, align="c", width="100%", {
            dtb <- data.frame(as.list(rev(res$COUNT)),
                              row.names="Nombre de réponses")
            names(dtb) <- rev(as.character(res$ORGANIZATION_FR))
            return(dtb)
          }))
    })
  })
  
  # `suspendWhenHiddle=FALSE` allows dynamic content to render even when it is
  #   not being displayed
  outputOptions(output,"graphsp2",suspendWhenHidden=FALSE)
  outputOptions(output,"graphsp5",suspendWhenHidden=FALSE)
  outputOptions(output,"title",suspendWhenHidden=FALSE)
  outputOptions(output,"displng",suspendWhenHidden=FALSE)
  outputOptions(output,"toptxt",suspendWhenHidden=FALSE)
  outputOptions(output,"langselector",suspendWhenHidden=FALSE)
  
  # JavaScript function to allow users to proceed from the loading screen when
  #   main content loads
  runjs("$(document).on('shiny:value', function(event){
         $('#spinner').hide();
         $('#continuebtns').show();})")
}
