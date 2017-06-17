library(shiny)
library(ggplot2)
library(plotly)
library(reshape2)
library(stringi)
library(RColorBrewer)


menu <- read.csv("/Users/mila/Documents/foolingAround/app/menu.csv",stringsAsFactors = F,
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

shinyServer(function(input,output){
  output$plot <- renderPlotly({
    print(  
      ggplotly(      
        ggplot(menuRmc[menuRmc$variable==input$variable & menuRmc$Category==input$category & menuRmc$value > 0,], aes(Item,value))+
          geom_bar(stat = "identity",position = position_dodge(),fill="#2171B5",colour="white")+
          coord_flip()+theme_minimal()+
          theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
                axis.text.y=element_text(family = "Georgia",debug = T))) %>% 
        layout(xaxis = list(side="top"))
    )
  })
  
  output$plot2 <- renderPlotly({
    g <- list(
      showframe = F,
      showland = T,
      landcolor = toRGB("#F5F5F5")
    )
    
    plot_geo(restR, locationmode = 'country names') %>%
      add_trace(
        z = ~value,  color = ~value, colors = "Blues",
        text = ~Country, locations = ~Country
      ) %>%
      colorbar(title = 'Number of restaurants') %>%
      layout(geo = g
      )
    
    
  })
}
)

