
## ~~~~~~~~~~~~ WEBSCRAPER ~~~~~~~~~~~~~ ##

# Run this program to update the local CSV from the Web.

# Years: 2019, 2018, 2017
# Units: Public Service, Health Canada, ROEB, all directorates of ROEB
# Notes: Allow approx. 3 minutes for the data to be compiled

library(rvest)
library(xml2)
library(stringr)

## ~~~~ Directorate IDs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

LEVEL3IDs_2019 <- c("POD_DPO"="328", "MDCCD_DCMMMC"="329", "HPCD_DCPS"="330", "LABS_LABS"="331", "PRSD_ADMO_DPSR_BSMA"="332", "EHPD_DSCSE"="333", "CPCSD_DSPCTP"="334", "CD_DC"="335")
LEVEL3IDs_2018 <- c("POD_DPO"="336", "MDCCD_DCMMMC"="335", "HPCD_DCPS"="333", "LABS_LABS"="334", "PRSD_ADMO_DPSR_BSMA"="330", "EHPD_DSCSE"="332", "CPCSD_DSPCTP"="331", "CD_DC"="337")
LEVEL3IDs_2017 <- c("POD_DPO"="107", "MDCCD_DCMMMC"="104", "HPCD_DCPS"="098", "LABS_LABS"="101", "PRSD_ADMO_DPSR_BSMA"="090", "EHPD_DSCSE"="095", "CPCSD_DSPCTP"="091")

URL_2019 <- "https://www.tbs-sct.gc.ca/pses-saff/2019/results-resultats/bd-pm/06/210/%s/org-eng.aspx"
URL_2018 <- "https://www.tbs-sct.gc.ca/pses-saff/2018/results-resultats/bd-pm/06/209/%s/org-eng.aspx"
URL_2017 <- "https://www.tbs-sct.gc.ca/pses-saff/2017-2/results-resultats/bd-pm/06/089/%s/org-eng.aspx"

## ~~~~ Main data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data <- data.frame(
  SURVEYR = character(),
  QID = integer(),
  QUESTION_EN = character(),
  QUESTION_FR = character(),
  THEME_EN = character(),
  THEME_FR = character(),
  ORGANIZATION_EN = character(),
  ORGANIZATION_FR = character(),
  ANSWER1 = integer(),
  ANSWER2 = integer(),
  ANSWER3 = integer(),
  ANSWER4 = integer(),
  ANSWER5 = integer(),
  ANSWER6 = integer(),
  ANSWER7 = integer(),
  POSITIVE = integer(),
  NEUTRAL = integer(),
  NEGATIVE = integer(),
  COUNT = integer(),
  stringsAsFactors=FALSE
)

## ~~~~ Text and translations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

units <- read.csv("data/lookups/PSES_SAFF-Units_Unités.csv", header=TRUE, encoding="UTF-8", col.names=c("FULL_NAME","UNIT_EN","UNIT_FR"), stringsAsFactors=FALSE)
questions <- read.csv("data/lookups/PSES_SAFF-Questions.csv", header=TRUE, encoding="UTF-8", col.names=c("ID","ANS_TYPE","IS_REV","EN","FR","IND_EN","IND_FR","IND_ID","SUBIND_EN","SUBIND_FR","SUBIND_ID"), stringsAsFactors=FALSE)
themes <- read.csv("data/lookups/PSES_SAFF-Themes_Thèmes.csv", header=TRUE, encoding="UTF-8", col.names=c("THEME_ID","THEME_EN","THEME_FR"), stringsAsFactors=FALSE)
answers <- read.csv("data/lookups/PSES_SAFF-Answers_Réponses.csv", header=TRUE, encoding="UTF-8", col.names=c(paste0("TYPE",1:7,"_EN"), paste0("TYPE",1:7,"_FR")), stringsAsFactors=FALSE)

## ~~~~ Table-processing functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Takes in a raw table scraped from the PSES page and formats each table to have the desired number of rows
padTable <- function(tbl, dirId, year) {
  d <- tbl[0,]
  d[1,1] <- as.character(year)
  
  if(year==2019 & dirId==LEVEL3IDs_2019["POD_DPO"]) {
    if(nrow(tbl[tbl$Organization=="Public Service",])==0) { d[1,2] <- "Public Service"; tbl <- rbind(d,tbl) }
    if(nrow(tbl[tbl$Organization=="Health Canada",])==0) { d[1,2] <- "Health Canada"; tbl <- rbind(tbl[1,],d,tbl[-1,]) }
    if(nrow(tbl[tbl$Organization=="Regulatory Operations and Enforcement Branch",])==0) { d[1,2] <- "Regulatory Operations and Enforcement Branch"; tbl <- rbind(tbl[1:2,],d,tbl[-(1:2),]) }
    if(nrow(tbl[tbl$Organization=="Planning and Operations Directorate",])==0) { d[1,2] <- "Planning and Operations Directorate"; tbl <- rbind(tbl[1:3,],d) }
  } else if(year==2019) {
    tbl <- tbl[tbl$Organization!="Public Service" & tbl$Organization!="Health Canada" & tbl$Organization!="Regulatory Operations and Enforcement Branch",]
    if(dirId==LEVEL3IDs_2019["MDCCD_DCMMMC"] & nrow(tbl[tbl$Organization=="Medical Devices and Clinical Compliance Directorate",])==0) { d[1,2] <- "Medical Devices and Clinical Compliance Directorate"; tbl <- rbind(d,tbl) }
    if(dirId==LEVEL3IDs_2019["HPCD_DCPS"] & nrow(tbl[tbl$Organization=="Health Products Compliance Directorate",])==0) { d[1,2] <- "Health Products Compliance Directorate"; tbl <- rbind(d,tbl) }
    if(dirId==LEVEL3IDs_2019["LABS_LABS"] & nrow(tbl[tbl$Organization=="Laboratories Directorate",])==0) { d[1,2] <- "Laboratories Directorate"; tbl <- rbind(d,tbl) }
    if(dirId==LEVEL3IDs_2019["PRSD_ADMO_DPSR_BSMA"] & nrow(tbl[tbl$Organization=="Policy and Regulatory Strategies Directorate and Community of Federal Regulators / Assistant Deputy Minister's Office",])==0) { d[1,2] <- "Policy and Regulatory Strategies Directorate and Community of Federal Regulators / Assistant Deputy Minister's Office"; tbl <- rbind(d,tbl) }
    if(dirId==LEVEL3IDs_2019["EHPD_DSCSE"] & nrow(tbl[tbl$Organization=="Controlled Substances and Environmental Health Directorate",])==0) { d[1,2] <- "Controlled Substances and Environmental Health Directorate"; tbl <- rbind(d,tbl) }
    if(dirId==LEVEL3IDs_2019["CPCSD_DSPCTP"] & nrow(tbl[tbl$Organization=="Consumer Product Safety, Tobacco, and Pesticides Directorate",])==0) { d[1,2] <- "Consumer Product Safety, Tobacco, and Pesticides Directorate"; tbl <- rbind(d,tbl) }
    if(dirId==LEVEL3IDs_2019["CD_DC"] & nrow(tbl[tbl$Organization=="Cannabis Directorate",])==0) { d[1,2] <- "Cannabis Directorate"; tbl <- rbind(d,tbl) }
  }
  
  if(year==2018 & dirId==LEVEL3IDs_2018["POD_DPO"]) {
    if(nrow(tbl[tbl$Organization=="Public Service",])==0) { d[1,2] <- "Public Service"; tbl <- rbind(d,tbl) }
    if(nrow(tbl[tbl$Organization=="Health Canada",])==0) { d[1,2] <- "Health Canada"; tbl <- rbind(tbl[1,],d,tbl[-1,]) }
    if(nrow(tbl[tbl$Organization=="Regulatory Operations & Regions Branch",])==0) { d[1,2] <- "Regulatory Operations and Enforcement Branch"; tbl <- rbind(tbl[1:2,],d,tbl[-(1:2),]) }
    if(nrow(tbl[tbl$Organization=="Planning and Operations",])==0) { d[1,2] <- "Planning and Operations Directorate"; tbl <- rbind(tbl[1:3,],d) }
  } else if(year==2018) {
    tbl <- tbl[tbl$Organization!="Public Service" & tbl$Organization!="Health Canada" & tbl$Organization!="Regulatory Operations & Regions Branch",]
    if(dirId==LEVEL3IDs_2018["MDCCD_DCMMMC"] & nrow(tbl)==0) { d[1,2] <- "Medical Devices and Clinical Compliance Directorate"; tbl <- rbind(d,tbl) }
    else if(dirId==LEVEL3IDs_2018["MDCCD_DCMMMC"]) { tbl[1,2] <- "Medical Devices and Clinical Compliance Directorate" }
    if(dirId==LEVEL3IDs_2018["HPCD_DCPS"] & nrow(tbl[tbl$Organization=="Health Products Compliance",])==0){ d[1,2] <- "Health Products Compliance Directorate"; tbl <- rbind(d,tbl) }
    if(dirId==LEVEL3IDs_2018["LABS_LABS"] & nrow(tbl[tbl$Organization=="Laboratories",])==0) { d[1,2] <- "Laboratories Directorate"; tbl <- rbind(d,tbl) }
    if(dirId==LEVEL3IDs_2018["PRSD_ADMO_DPSR_BSMA"] & nrow(tbl)==0) { d[1,2] <- "Policy and Regulatory Strategies Directorate and Community of Federal Regulators / Assistant Deputy Minister's Office"; tbl <- rbind(d,tbl) }
    else if(dirId==LEVEL3IDs_2018["PRSD_ADMO_DPSR_BSMA"]) { tbl[1,2] <- "Policy and Regulatory Strategies Directorate and Community of Federal Regulators / Assistant Deputy Minister's Office" }
    if(dirId==LEVEL3IDs_2018["EHPD_DSCSE"] & nrow(tbl)==0) { d[1,2] <- "Controlled Substances and Environmental Health Directorate"; tbl <- rbind(d,tbl) }
    else if(dirId==LEVEL3IDs_2018["EHPD_DSCSE"]) { tbl[1,2] <- "Controlled Substances and Environmental Health Directorate" }
    if(dirId==LEVEL3IDs_2018["CPCSD_DSPCTP"] & nrow(tbl[tbl$Organization=="Consumer Product Safety, Tobacco and Pesticides",])==0) { d[1,2] <- "Consumer Product Safety, Tobacco, and Pesticides Directorate"; tbl <- rbind(d,tbl) }
    if(dirId==LEVEL3IDs_2018["CD_DC"] & nrow(tbl[tbl$Organization=="Cannabis",])==0) { d[1,2] <- "Cannabis Directorate"; tbl <- rbind(d,tbl) }
  }
  
  if(year==2017 & dirId==LEVEL3IDs_2017["POD_DPO"]) {
    if(nrow(tbl[tbl$Organization=="Public Service",])==0) { d[1,2] <- "Public Service"; tbl <- rbind(d,tbl) }
    if(nrow(tbl[tbl$Organization=="Health Canada",])==0) { d[1,2] <- "Health Canada"; tbl <- rbind(tbl[1,],d,tbl[-1,]) }
    if(nrow(tbl[tbl$Organization=="Regulatory Operations & Regions Branch",])==0) { d[1,2] <- "Regulatory Operations and Enforcement Branch"; tbl <- rbind(tbl[1:2,],d,tbl[-(1:2),]) }
    if(nrow(tbl[tbl$Organization=="Planning and Operations",])==0) { d[1,2] <- "Planning and Operations Directorate"; tbl <- rbind(tbl[1:3,],d) }
  } else if(year==2017) {
    tbl <- tbl[tbl$Organization!="Public Service" & tbl$Organization!="Health Canada" & tbl$Organization!="Regulatory Operations & Regions Branch",]
    if(dirId==LEVEL3IDs_2017["MDCCD_DCMMMC"] & nrow(tbl)==0) { d[1,2] <- "Medical Devices and Clinical Compliance Directorate"; tbl <- rbind(d,tbl) }
    else if(dirId==LEVEL3IDs_2017["MDCCD_DCMMMC"]) { tbl[1,2] <- "Medical Devices and Clinical Compliance Directorate" }
    if(dirId==LEVEL3IDs_2017["HPCD_DCPS"] & nrow(tbl[tbl$Organization=="Health Products Compliance",])==0){ d[1,2] <- "Health Products Compliance Directorate"; tbl <- rbind(d,tbl) }
    if(dirId==LEVEL3IDs_2017["LABS_LABS"] & nrow(tbl[tbl$Organization=="Laboratories",])==0) { d[1,2] <- "Laboratories Directorate"; tbl <- rbind(d,tbl) }
    if(dirId==LEVEL3IDs_2017["PRSD_ADMO_DPSR_BSMA"] & nrow(tbl)==0) { d[1,2] <- "Policy and Regulatory Strategies Directorate and Community of Federal Regulators / Assistant Deputy Minister's Office"; tbl <- rbind(d,tbl) }
    else if(dirId==LEVEL3IDs_2017["PRSD_ADMO_DPSR_BSMA"]) { tbl[1,2] <- "Policy and Regulatory Strategies Directorate and Community of Federal Regulators / Assistant Deputy Minister's Office" }
    if(dirId==LEVEL3IDs_2017["EHPD_DSCSE"] & nrow(tbl)==0) { d[1,2] <- "Controlled Substances and Environmental Health Directorate"; tbl <- rbind(d,tbl) }
    else if(dirId==LEVEL3IDs_2017["EHPD_DSCSE"]) { tbl[1,2] <- "Controlled Substances and Environmental Health Directorate" }
    if(dirId==LEVEL3IDs_2017["CPCSD_DSPCTP"] & nrow(tbl[tbl$Organization=="Consumer Product Safety, Tobacco and Pesticides",])==0) { d[1,2] <- "Consumer Product Safety, Tobacco, and Pesticides Directorate"; tbl <- rbind(d,tbl) }
  }
  
  return(tbl)
}

# Takes in a processed table and replaces organization names with appropriate acronyms
tableUseAcronyms <- function(tbl) { for(i in 1:nrow(tbl)) { tbl$Organization[i] <- as.character(units[units$FULL_NAME == tbl$Organization[i],"UNIT_EN"][1]) }; return(tbl) }

# Adds data to the main dataframe
addToData <- function(tbl, ques, thm) {
  qid <- as.character(questions[questions$EN==ques,"ID"][1])
  if(is.na(qid)) { return(data) }
  
  d <- data[0,]
  d[1:nrow(tbl),"SURVEYR"] <- as.character(tbl$`Survey year`)
  d[,c("THEME_EN","THEME_FR","COUNT","QID","ORGANIZATION_EN")] <- data.frame(as.character(themes[themes$THEME_ID==thm,"THEME_EN"][1]), as.character(themes[themes$THEME_ID==thm,"THEME_FR"][1]), tbl$`Total responses`, qid, tbl$Organization)
  for(i in 1:nrow(tbl)) { d[i,"ORGANIZATION_FR"] <- as.character(units[units$UNIT_EN==tbl$Organization[i],"UNIT_FR"][1]) }
  d[,c("QUESTION_EN","QUESTION_FR")] <- data.frame(as.character(questions[questions$ID==qid,"EN"][1]), as.character(questions[questions$EN==ques,"FR"][1]))
  if("Positive answers(%)" %in% names(tbl)) {
    d[,c("POSITIVE","NEGATIVE", "NEUTRAL")] <- data.frame(tbl$`Positive answers(%)`, tbl$`Negative answers(%)`, 100-tbl$`Positive answers(%)`-tbl$`Negative answers(%)`)
    for(i in 1:7) { d[,paste0("ANSWER",i)] <- tbl[,i+2] } }
  else { for(i in 1:(ncol(tbl)-3)) { d[,paste0("ANSWER",i)] <- tbl[,i+2] } }
  
  rbind(data,d)
}

## ~~~~ Data read-ins ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Reads 2019 data
for(id3 in LEVEL3IDs_2019) {
  url <- read_html(sprintf(URL_2019,id3))
  sections <- url %>% html_node("#divResults") %>% html_nodes("section")
  for(s in sections) {
    children <- s %>% html_children()
    for(ch in children) {
      if(ch %>% html_name() == "div") {
        q <- ch %>% html_node("h3") %>% html_text()
        for(i in 1:nchar(q)) { if(substr(q, start=i, stop=i) == ".") { q <- str_trim(substr(q, start=i+1, stop=nchar(q))); break } }
        dtbl <- ch %>% html_node("table") %>% html_table()
        dtbl <- tableUseAcronyms(padTable(dtbl, id3, 2019))
        thm <- questions[questions$EN == q, "SUBIND_ID"][1]
        data <- addToData(dtbl, q, thm)
      }}}}

# Reads 2018 data
for(id3 in LEVEL3IDs_2018) {
  url <- read_html(sprintf(URL_2018,id3))
  sections <- url %>% html_node("#divResults") %>% html_nodes("section")
  for(s in sections) {
    children <- s %>% html_children()
    for(ch in children) {
      if(ch %>% html_name() == "div") {
        q <- ch %>% html_node("h3") %>% html_text()
        for(i in 1:nchar(q)) { if(substr(q, start=i, stop=i) == ".") { q <- str_trim(substr(q, start=i+1, stop=nchar(q))); break } }
        dtbl <- ch %>% html_node("table") %>% html_table()
        dtbl <- tableUseAcronyms(padTable(dtbl, id3, 2018))
        thm <- questions[questions$EN == q, "SUBIND_ID"][1]
        data <- addToData(dtbl, q, thm)
      }}}}

# Reads 2017 data
for(id3 in LEVEL3IDs_2017) {
  url <- read_html(sprintf(URL_2017,id3))
  sections <- url %>% html_node("#divResults") %>% html_nodes("section")
  for(s in sections) {
    children <- s %>% html_children()
    for(ch in children) {
      if(ch %>% html_name() == "div") {
        q <- ch %>% html_node("h3") %>% html_text()
        for(i in 1:nchar(q)) { if(substr(q, start=i, stop=i) == ".") { q <- str_trim(substr(q, start=i+1, stop=nchar(q))); break } }
        dtbl <- ch %>% html_node("table") %>% html_table()
        dtbl <- tableUseAcronyms(padTable(dtbl, id3, 2017))
        thm <- questions[questions$EN == q, "SUBIND_ID"][1]
        data <- addToData(dtbl, q, thm)
      }}}}

## ~~~~ File output ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

write.csv(data,"data/2019_2018_2017-PSES_SAFF-ROEB_DGORAL-Full_data_Données_complètes.csv",fileEncoding="UTF-8")
