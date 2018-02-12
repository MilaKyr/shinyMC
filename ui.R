library(shiny)
library(ggplot2)
library(plotly)
library(reshape2)
library(stringi)
library(RColorBrewer)


menu <- read.csv("menu.csv",stringsAsFactors = F,
                 encoding="UTF-8")
Sys.setlocale("LC_ALL", "ES_ES.UTF-8")

#menu
menuR <- melt(menu)
nok <- grep("Daily.Value.",menuR[,4])
menuRmcall <- menuR[-nok,]
menuRmcall$Category <- factor(menuRmcall$Category)
menuRmcall$variable = stringi::stri_trans_general(menuRmcall$variable, "latin-ascii")
menuRmcall$variable = gsub("[.]"," ",menuRmcall$variable)
menuRmcall$variable <- gsub("Saturated.Fat","Saturated Fat (gr)",menuRmcall$variable)
menuRmcall$variable <- gsub("Trans.Fat","Trans Fat (gr)",menuRmcall$variable)
menuRmcall$variable <- gsub("Cholesterol","Cholesterol (mg)",menuRmcall$variable)
menuRmcall$variable <- gsub("Dietary Fiber","Dietary Fiber (gr)",menuRmcall$variable)
menuRmcall$variable <- gsub("Sodium","Sodium (mg)",menuRmcall$variable)
menuRmcall$variable <- gsub("Sugars","Sugars (gr)",menuRmcall$variable)
menuRmcall$variable <- gsub("Total Fat","Total Fat (gr)",menuRmcall$variable)
menuRmcall$variable <- gsub("Protein","Protein (gr)",menuRmcall$variable)
menuRmcall$variable <- gsub("Carbohydrates","Carbohydrates (gr)",menuRmcall$variable)
menuRmcall$variable <- factor(menuRmcall$variable)
medium <- grep("Small|Large", menuRmcall[menuRmcall$Category=="Coffee & Tea","Item"])
menuRmcall[menuRmcall$Category=="Coffee & Tea","Item"][medium] <- NA
menuRmc <- menuRmcall[is.na(menuRmcall$Item)==F,]

#rest
rest <- read.csv("/Users/mila/Documents/foolingAround/app/McMap3.csv",
                 stringsAsFactors = F,
                 encoding="UTF-8")
loglat <- read.csv2("/Users/mila/Documents/foolingAround/app/latlog.csv",
                    stringsAsFactors = F)
colnames(loglat)<- c("Lat","Log","Country")
rest <- rest[1:116,]
restR <- merge(rest[c(1,3)],loglat,by="Country")
restR$Country <- factor(restR$Country)
colnames(restR)[2] <- c("value")


shinyUI(fluidPage(
  tabsetPanel(
    tabPanel(title = "Nutristions",
             titlePanel(title=div(div(HTML("<center>McDonald's Nutritions</center>")),
                                  img(src="food2.png", style="display: block; margin-left: auto; margin-right: auto;", width="250", height="53")
             )),
             sidebarLayout(
               sidebarPanel(selectInput(inputId = "category", 
                                        label = "Choose a category", 
                                        choices = as.list(levels(menuRmc$Category)),
                                        selected = NULL, 
                                        multiple = FALSE, 
                                        selectize = TRUE),
                            radioButtons(inputId = "variable",
                                         label="Choose nutrition",
                                         choices = as.list(levels(menuRmc$variable)),
                                         inline = F),
                            div(style = "marginRight: 50px; width: 200px; ")
               ),
               
               mainPanel(plotlyOutput(outputId = "plot", height = "600px")))),
    tabPanel(title = "Restaurants",
             titlePanel(title=div(div(HTML("<center>McDonald's Restaurants</center>")),
                                  img(src="food2.png", style="display: block; margin-left: auto; margin-right: auto;", width="250", height="53")
             )),
             plotlyOutput(outputId = "plot2", height = "600px")))
))
