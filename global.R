
## ~~~~~~~~~~ GLOBAL DATA ~~~~~~~~~~~ ##

# Created by: Sijia Wang
# Team: Data Analytics and Business Solutions (DABS)
# Version: 1.1
# Last modified: 2020-03-10
# Description: Global data for 2019 PSES/SAFF dashboard for ROEB and its
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

## ~~~~ Data Files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

questions <-
  read.csv("data/lookups/PSES_SAFF-Questions.csv",header=TRUE,encoding="UTF-8",
           col.names=c("ID","ANS_TYPE","IS_REV","EN","FR","IND_EN","IND_FR",
                       "IND_ID","SUBIND_EN","SUBIND_FR","SUBIND_ID"),
           stringsAsFactors=FALSE)
themes <- 
  read.csv("data/lookups/PSES_SAFF-Themes.csv",header=TRUE,
           encoding="UTF-8",col.names=c("THEME_ID","THEME_EN","THEME_FR"),
           stringsAsFactors=FALSE)
answers <-
  read.csv("data/lookups/PSES_SAFF-Answers_Reponses.csv",header=TRUE,
           encoding="UTF-8",col.names=c(paste0("TYPE",1:7,"_EN"),
                                        paste0("TYPE",1:7,"_FR")),
           stringsAsFactors=FALSE)
data <-
  read.csv(
    "data/2019_2018_2017-PSES_SAFF-ROEB_DGORAL-Full_data_Donnees_completes.csv",
    header=TRUE,encoding="UTF-8",stringsAsFactors=FALSE)
data <- data[,2:ncol(data)]

## ~~~~ Global variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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