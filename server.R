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
      if (q < 10) { # this condition is only here for testing purposes to load faster
      box(id=paste0("b",q),title=qtitle,status="primary",solidHeader=TRUE,
          width=12,collapsible=TRUE,collapsed=TRUE,
          renderPlot({ggplot()}))
      }
    })
  })
}
