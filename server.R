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

ans.sets <- list(c("Strongly agree","Somewhat agree","Neither agree nor disagree",
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
ans.type <- c(rep(1,16),rep(2,9),rep(1,18),2,rep(1,7),rep(3,8),rep(1,7),4,5,1,1,
              rep(6,47),1,1,rep(6,48),1,1,rep(3,20),7,2,1,1,6,6,1,3,6,6,1,1,3)

# -----------------------------------------------------------------------------

server <- function(input, output) {
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
      #if (q < 10) { # this condition is only here for testing purposes to load 
      
      res <- data1.f[data1.f$QUESTION==qIDs[q],]
      v <- c()
      n <- length(ans.sets[[ans.type[q]]])
      for(s in paste0("ANSWER",1:n)) {
        v <- c(v,res[,s])
      }
      df <- structure(v,
                      .Dim=c(nrow(res),n),
                      .Dimnames=list(c("Public Service","Health Canada"),
                                     ans.sets[[ans.type[q]]]))
      df.m <- melt(df)
      df.m <- rename(df.m, Unit = Var1, Responses = Var2, Proportion = value)
      
      box(id=paste0("b",q),title=qtitle,status="primary",solidHeader=TRUE,
          width=12,collapsible=TRUE,collapsed=TRUE,
          "(Percentages may not add to 100 due to rounding)",
          renderPlot(height=200, {
            ggplot(df.m, aes(x=Unit, y=Proportion, fill=Responses)) +
              geom_bar(stat="identity", position=position_stack(reverse = TRUE)) +
              labs(x="Department", y="Proportion responded (%)") +
              geom_text(size=3, position=position_stack(vjust=0.5, reverse=TRUE),
                        aes(label=Proportion)) +
              coord_flip()
          }))
      #}
    })
  })
}
