library(shiny)
library(shinythemes)
library(plyr)
library(dplyr)
library(ggplot2)
library(DT)

library(maptools)
library(rgdal)
library(tidyr)


##Prep Data ####
death <- read.csv("death101.csv")
# data <- death
counties <- readOGR(dsn="Counties", layer="Counties")
counties.df <- fortify(counties)
counties@data$id <- rownames(counties@data)
counties.df <- join(counties.df, counties@data, by="id")
vectorCounty <- as.vector(death$County)
vectorCoshort <- substr(vectorCounty, 1, nchar(vectorCounty)-7)
newdeath <- cbind(vectorCoshort,death)
names <- c("Name","County","Date","Deaths")
colnames(newdeath) <- names
wideDeath <- spread(newdeath, Date, Deaths)
namesWideDeath <- c("Name","County","y1999","y2000","y2001","y2002","y2003","y2004","y2005","y2006","y2007","y2008","y2009","y2010","y2011","y2012")
colnames(wideDeath) <- namesWideDeath

##Server Function ####
shinyServer(function(input, output){
    


# 
#     output$Plot <- renderPlot({
# ## Anderson - Brazoria ####
#         if(input$county == 1) { 
#             newDeath <- filter(death, grepl("Anderson", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 2) {
#             newDeath <- filter(death, grepl("Andrews", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 3){
#             newDeath <- filter(death, grepl("Angelina", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 4) {
#             newDeath <- filter(death, grepl("Aransas", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 5) {
#             newDeath <- filter(death, grepl("Archer", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 6) {
#             newDeath <- filter(death, grepl("Armstrong", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 7) {
#             newDeath <- filter(death, grepl("Atascosa", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 8) {
#             newDeath <- filter(death, grepl("Austin", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 9) {
#             newDeath <- filter(death, grepl("Bailey", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 10) {
#             newDeath <- filter(death, grepl("Bandera", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 11) {
#             newDeath <- filter(death, grepl("Bastrop", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 12) {
#             newDeath <- filter(death, grepl("Baylor", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 13) {
#             newDeath <- filter(death, grepl("Bee", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 14) {
#             newDeath <- filter(death, grepl("Bell", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#         } else if(input$county == 15) {
#             newDeath <- filter(death, grepl("Bexar", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#         } else if(input$county == 16) {
#             newDeath <- filter(death, grepl("Blanco", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#         } 
#         else if(input$county == 17) {
#             newDeath <- filter(death, grepl("Borden", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#         } else if(input$county == 18) {
#             newDeath <- filter(death, grepl("Bosque", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#         } else if(input$county == 19) {
#             newDeath <- filter(death, grepl("Bowie", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#         } 
#         else if(input$county == 20) {
#             newDeath <- filter(death, grepl("Brazoria", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
# ## Brazos - Cochran ####    
#         } else if(input$county == 21) {
#             newDeath <- filter(death, grepl("Brazos", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 22) {
#             newDeath <- filter(death, grepl("Brewster", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } 
#         else if(input$county == 23) {
#             newDeath <- filter(death, grepl("Briscoe", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))    
#         
#         } else if(input$county == 24) {
#             newDeath <- filter(death, grepl("Brooks", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 25) {
#             newDeath <- filter(death, grepl("Brown", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#         } 
#         else if(input$county == 26) {
#             newDeath <- filter(death, grepl("Burleson", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 27) {
#             newDeath <- filter(death, grepl("Burnet", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 28) {
#             newDeath <- filter(death, grepl("Caldwell", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } 
#         else if(input$county == 29) {
#             newDeath <- filter(death, grepl("Calhoun", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 30) {
#             newDeath <- filter(death, grepl("Callahan", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 31) {
#             newDeath <- filter(death, grepl("Cameron", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#         } 
#         else if(input$county == 32) {
#             newDeath <- filter(death, grepl("Camp", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 33) {
#             newDeath <- filter(death, grepl("Carson", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 34) {
#             newDeath <- filter(death, grepl("Cass", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } 
#         else if(input$county == 35) {
#             newDeath <- filter(death, grepl("Castro", County))    
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 36) {
#             newDeath <- filter(death, grepl("Chambers", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 37) {
#             newDeath <- filter(death, grepl("Cherokee", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } 
#         else if(input$county == 38) {
#             newDeath <- filter(death, grepl("Childress", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 39) {
#             newDeath <- filter(death, grepl("Clay", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 40) {
#             newDeath <- filter(death, grepl("Cochran", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#         } 
# ##Coke - Delta####  
#         else if(input$county == 41) {
#             newDeath <- filter(death, grepl("Coke", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 42) {
#             newDeath <- filter(death, grepl("Coleman", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 43) {
#             newDeath <- filter(death, grepl("Collin", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } 
#         else if(input$county == 44) {
#             newDeath <- filter(death, grepl("Collingsworth", County))    
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 45) {
#             newDeath <- filter(death, grepl("Colorado", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 46) {
#             newDeath <- filter(death, grepl("Comal", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } 
#         else if(input$county == 47) {
#             newDeath <- filter(death, grepl("Comanche", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 48) {
#             newDeath <- filter(death, grepl("Concho", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 49) {
#             newDeath <- filter(death, grepl("Cooke", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } 
#         else if(input$county == 50) {
#             newDeath <- filter(death, grepl("Coryell", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 51) {
#             newDeath <- filter(death, grepl("Cottle", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#         } 
#         else if(input$county == 52) {
#             newDeath <- filter(death, grepl("Crane", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 53) {
#             newDeath <- filter(death, grepl("Crockett", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 54) {
#             newDeath <- filter(death, grepl("Crosby", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#         } 
#         else if(input$county == 55) {
#             newDeath <- filter(death, grepl("Culberson", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 56) {
#             newDeath <- filter(death, grepl("Dallam", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 57) {
#             newDeath <- filter(death, grepl("Dallas", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } 
#         else if(input$county == 58) {
#             newDeath <- filter(death, grepl("Dawson", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 59) {
#             newDeath <- filter(death, grepl("Deaf Smith", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 60) {
#             newDeath <- filter(death, grepl("Delta", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } 
# ##Denton- Franklin  ####
#         else if(input$county == 61) {
#             newDeath <- filter(death, grepl("Denton", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 62) {
#             newDeath <- filter(death, grepl("DeWitt", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 63) {
#             newDeath <- filter(death, grepl("Dickens", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 64) {
#             newDeath <- filter(death, grepl("Dimmit", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 65) {
#             newDeath <- filter(death, grepl("Donley", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 66) {
#             newDeath <- filter(death, grepl("Duval", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 67) {
#             newDeath <- filter(death, grepl("Eastland", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 68) {
#             newDeath <- filter(death, grepl("Ector", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 69) {
#             newDeath <- filter(death, grepl("Edwards", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 70) {
#             newDeath <- filter(death, grepl("Ellis", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 71) {
#             newDeath <- filter(death, grepl("El Paso", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 72) {
#             newDeath <- filter(death, grepl("Erath", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 73) {
#             newDeath <- filter(death, grepl("Falls", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 74) {
#             newDeath <- filter(death, grepl("Fannin", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 75) {
#             newDeath <- filter(death, grepl("Fayette", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 76) {
#             newDeath <- filter(death, grepl("Fisher", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 77) {
#             newDeath <- filter(death, grepl("Floyd", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 78) {
#             newDeath <- filter(death, grepl("Foard", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 79) {
#             newDeath <- filter(death, grepl("Fort Bend", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 80) {
#             newDeath <- filter(death, grepl("Franklin", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
# ##Freestone- Hardin  ####            
#         } else if(input$county == 81) {
#             newDeath <- filter(death, grepl("Freestone", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 82) {
#             newDeath <- filter(death, grepl("Frio", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 83) {
#             newDeath <- filter(death, grepl("Gaines", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 84) {
#             newDeath <- filter(death, grepl("Galveston", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 85) {
#             newDeath <- filter(death, grepl("Garza", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 86) {
#             newDeath <- filter(death, grepl("Gillespie", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 87) {
#             newDeath <- filter(death, grepl("Glasscock", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 88) {
#             newDeath <- filter(death, grepl("Goliad", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 89) {
#             newDeath <- filter(death, grepl("Gonzales", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 90) {
#             newDeath <- filter(death, grepl("Gray", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 91) {
#             newDeath <- filter(death, grepl("Grayson", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 92) {
#             newDeath <- filter(death, grepl("Gregg", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 93) {
#             newDeath <- filter(death, grepl("Grimes", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 94) {
#             newDeath <- filter(death, grepl("Guadalupe", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 95) {
#             newDeath <- filter(death, grepl("Hale", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 96) {
#             newDeath <- filter(death, grepl("Hall", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 97) {
#             newDeath <- filter(death, grepl("Hamilton", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 98) {
#             newDeath <- filter(death, grepl("Hansford", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 99) {
#             newDeath <- filter(death, grepl("Hardeman", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 100) {
#             newDeath <- filter(death, grepl("Hardin", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
# ##Harris- Jackson  ####            
#         } else if(input$county == 101) {
#             newDeath <- filter(death, grepl("Harris", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 102) {
#             newDeath <- filter(death, grepl("Harrison", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 103) {
#             newDeath <- filter(death, grepl("Hartley", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 104) {
#             newDeath <- filter(death, grepl("Haskell", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 105) {
#             newDeath <- filter(death, grepl("Hays", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 106) {
#             newDeath <- filter(death, grepl("Hemphill", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 107) {
#             newDeath <- filter(death, grepl("Henderson", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 108) {
#             newDeath <- filter(death, grepl("Hidalgo", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 109) {
#             newDeath <- filter(death, grepl("Hill", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 110) {
#             newDeath <- filter(death, grepl("Hockley", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 111) {
#             newDeath <- filter(death, grepl("Hood", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 112) {
#             newDeath <- filter(death, grepl("Hopkins", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 113) {
#             newDeath <- filter(death, grepl("Houston", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 114) {
#             newDeath <- filter(death, grepl("Howard", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 115) {
#             newDeath <- filter(death, grepl("Hudspeth", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 116) {
#             newDeath <- filter(death, grepl("Hunt", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 117) {
#             newDeath <- filter(death, grepl("Hutchinson", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 118) {
#             newDeath <- filter(death, grepl("Irion", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 119) {
#             newDeath <- filter(death, grepl("Jack", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 120) {
#             newDeath <- filter(death, grepl("Jackson", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
# ##Jasper-Lamar   ####            
#         } else if(input$county == 121) {
#             newDeath <- filter(death, grepl("Jasper", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 122) {
#             newDeath <- filter(death, grepl("Jeff Davis", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 123) {
#             newDeath <- filter(death, grepl("Jefferson", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 124) {
#             newDeath <- filter(death, grepl("Jim Hogg", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 125) {
#             newDeath <- filter(death, grepl("Jim Wells", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 126) {
#             newDeath <- filter(death, grepl("Johnson", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         } else if(input$county == 127) {
#             newDeath <- filter(death, grepl("Jones", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 128) {
#             newDeath <- filter(death, grepl("Karnes", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 129) {
#             newDeath <- filter(death, grepl("Kaufman", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 130) {
#             newDeath <- filter(death, grepl("Kendall", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 131) {
#             newDeath <- filter(death, grepl("Kenedy", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 132) {
#             newDeath <- filter(death, grepl("Kent", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 133) {
#             newDeath <- filter(death, grepl("Kerr", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 134) {
#             newDeath <- filter(death, grepl("Kimble", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 135) {
#             newDeath <- filter(death, grepl("King", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 136) {
#             newDeath <- filter(death, grepl("Kinney", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 137) {
#             newDeath <- filter(death, grepl("Kleberg", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 138) {
#             newDeath <- filter(death, grepl("Knox", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 139) {
#             newDeath <- filter(death, grepl("La Salle", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 140) {
#             newDeath <- filter(death, grepl("Lamar", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
# ##Lamb- McCulloh  ####            
#         }else if(input$county == 141) {
#             newDeath <- filter(death, grepl("Lamb", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 142) {
#             newDeath <- filter(death, grepl("Lampasas", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 143) {
#             newDeath <- filter(death, grepl("Lavaca", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 144) {
#             newDeath <- filter(death, grepl("Lee", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 145) {
#             newDeath <- filter(death, grepl("Leon", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 146) {
#             newDeath <- filter(death, grepl("Liberty", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 147) {
#             newDeath <- filter(death, grepl("Limestone", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 148) {
#             newDeath <- filter(death, grepl("Lipscomb", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 149) {
#             newDeath <- filter(death, grepl("Live Oak", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 150) {
#             newDeath <- filter(death, grepl("Llano", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 151) {
#             newDeath <- filter(death, grepl("Loving", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 152) {
#             newDeath <- filter(death, grepl("Lubbock", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 153) {
#             newDeath <- filter(death, grepl("Lynn", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 154) {
#             newDeath <- filter(death, grepl("Madison", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 155) {
#             newDeath <- filter(death, grepl("Marion", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 156) {
#             newDeath <- filter(death, grepl("Martin", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 157) {
#             newDeath <- filter(death, grepl("Mason", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 158) {
#             newDeath <- filter(death, grepl("Matagorda", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 159) {
#             newDeath <- filter(death, grepl("Maverick", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 160) {
#             newDeath <- filter(death, grepl("McCulloch", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
# ##McLennan-Oldham   ####            
#         }else if(input$county == 161) {
#             newDeath <- filter(death, grepl("McLennan", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 162) {
#             newDeath <- filter(death, grepl("McMullen", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 163) {
#             newDeath <- filter(death, grepl("Medina", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 164) {
#             newDeath <- filter(death, grepl("Menard", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 165) {
#             newDeath <- filter(death, grepl("Midland", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 166) {
#             newDeath <- filter(death, grepl("Milam", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 167) {
#             newDeath <- filter(death, grepl("Mills", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 168) {
#             newDeath <- filter(death, grepl("Mitchell", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 169) {
#             newDeath <- filter(death, grepl("Montague", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 170) {
#             newDeath <- filter(death, grepl("Montgomery", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 171) {
#             newDeath <- filter(death, grepl("Moore", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 172) {
#             newDeath <- filter(death, grepl("Morris", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 173) {
#             newDeath <- filter(death, grepl("Motley", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 174) {
#             newDeath <- filter(death, grepl("Nacogdoches", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 175) {
#             newDeath <- filter(death, grepl("Navarro", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 176) {
#             newDeath <- filter(death, grepl("Newton", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 177) {
#             newDeath <- filter(death, grepl("Nolan", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 178) {
#             newDeath <- filter(death, grepl("Nueces", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 179) {
#             newDeath <- filter(death, grepl("Ochiltree", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 180) {
#             newDeath <- filter(death, grepl("Oldham", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
# ##Orange-Runnels ####            
#         }else if(input$county == 181) {
#             newDeath <- filter(death, grepl("Orange", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 182) {
#             newDeath <- filter(death, grepl("Palo Pinto", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 183) {
#             newDeath <- filter(death, grepl("Panola", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 184) {
#             newDeath <- filter(death, grepl("Parker", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 185) {
#             newDeath <- filter(death, grepl("Parmer", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 186) {
#             newDeath <- filter(death, grepl("Pecos", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 187) {
#             newDeath <- filter(death, grepl("Polk", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 188) {
#             newDeath <- filter(death, grepl("Potter", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 189) {
#             newDeath <- filter(death, grepl("Presidio", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 190) {
#             newDeath <- filter(death, grepl("Rains", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 191) {
#             newDeath <- filter(death, grepl("Randall", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 192) {
#             newDeath <- filter(death, grepl("Reagan", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 193) {
#             newDeath <- filter(death, grepl("Real", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 194) {
#             newDeath <- filter(death, grepl("Red River", County))    
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 195) {
#             newDeath <- filter(death, grepl("Reeves", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 196) {
#             newDeath <- filter(death, grepl("Refugio", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 197) {
#             newDeath <- filter(death, grepl("Roberts", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 198) {
#             newDeath <- filter(death, grepl("Robertson", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 199) {
#             newDeath <- filter(death, grepl("Rockwall", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 200) {
#             newDeath <- filter(death, grepl("Runnels", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
# ##Rusk -Tarrant ####            
#         }else if(input$county == 201) {
#             newDeath <- filter(death, grepl("Rusk", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 202) {
#             newDeath <- filter(death, grepl("Sabine", County))    
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 203) {
#             newDeath <- filter(death, grepl("San Augustine", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 204) {
#             newDeath <- filter(death, grepl("San Jacinto", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 205) {
#             newDeath <- filter(death, grepl("San Patricio", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 206) {
#             newDeath <- filter(death, grepl("San Saba", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 207) {
#             newDeath <- filter(death, grepl("Schleicher", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 208) {
#             newDeath <- filter(death, grepl("Scurry", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 209) {
#             newDeath <- filter(death, grepl("Shackelford", County))            
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 210) {
#             newDeath <- filter(death, grepl("Shelby", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 211) {
#             newDeath <- filter(death, grepl("Sherman", County))            
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 212) {
#             newDeath <- filter(death, grepl("Smith", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 213) {
#             newDeath <- filter(death, grepl("Somervell", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 214) {
#             newDeath <- filter(death, grepl("Starr", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 215) {
#             newDeath <- filter(death, grepl("Stephens", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 216) {
#             newDeath <- filter(death, grepl("Sterling", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 217) {
#             newDeath <- filter(death, grepl("Stonewall", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 218) {
#             newDeath <- filter(death, grepl("Sutton", County))        
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 219) {
#             newDeath <- filter(death, grepl("Swisher", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 220) {
#             newDeath <- filter(death, grepl("Tarrant", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
# ##Taylor- Webb ####            
#         }else if(input$county == 221) {
#             newDeath <- filter(death, grepl("Taylor", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 222) {
#             newDeath <- filter(death, grepl("Terrell", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 223) {
#             newDeath <- filter(death, grepl("Terry", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 224) {
#             newDeath <- filter(death, grepl("Throckmorton", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 225) {
#             newDeath <- filter(death, grepl("Titus", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 226) {
#             newDeath <- filter(death, grepl("Tom Green", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 227) {
#             newDeath <- filter(death, grepl("Travis", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 228) {
#             newDeath <- filter(death, grepl("Trinity", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 229) {
#             newDeath <- filter(death, grepl("Tyler", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 230) {
#             newDeath <- filter(death, grepl("Upshur", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 231) {
#             newDeath <- filter(death, grepl("Upton", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 232) {
#             newDeath <- filter(death, grepl("Uvalde", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 233) {
#             newDeath <- filter(death, grepl("Val Verde", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 234) {
#             newDeath <- filter(death, grepl("Van Zandt", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 235) {
#             newDeath <- filter(death, grepl("Victoria", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 236) {
#             newDeath <- filter(death, grepl("Walker", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 237) {
#             newDeath <- filter(death, grepl("Waller", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 238) {
#             newDeath <- filter(death, grepl("Ward", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 239) {
#             newDeath <- filter(death, grepl("Washington", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 240) {
#             newDeath <- filter(death, grepl("Webb", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
# ##Wharton-  #####            
#         }else if(input$county == 241) {
#             newDeath <- filter(death, grepl("Wharton", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 242) {
#             newDeath <- filter(death, grepl("Wheeler", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 243) {
#             newDeath <- filter(death, grepl("Wichita", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 244) {
#             newDeath <- filter(death, grepl("Wilbarger", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 245) {
#             newDeath <- filter(death, grepl("Willacy", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 246) {
#             newDeath <- filter(death, grepl("Williamson", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 247) {
#             newDeath <- filter(death, grepl("Wilson", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 248) {
#             newDeath <- filter(death, grepl("Winkler", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 249) {
#             newDeath <- filter(death, grepl("Wise", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 250) {
#             newDeath <- filter(death, grepl("Wood", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 251) {
#             newDeath <- filter(death, grepl("Yoakum", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 252) {
#             newDeath <- filter(death, grepl("Young", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 253) {
#             newDeath <- filter(death, grepl("Zapata", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }else if(input$county == 254) {
#             newDeath <- filter(death, grepl("Zavala", County))
#             ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
#                 geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
#                 scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
#             
#         }
#         else {
#             print    
#         }
#     })
#  
#     

# Data Table --------------------------------------------------------------
output$data= renderDataTable({
    datatable(death)
})

# Map ----------------------------------------------------------------------
output$Map <- renderPlot({

##Prep Flu Data to join to Map #####
anderson <- counties.df[counties.df$Name=="Anderson",]
andersonFlu <- wideDeath[1,3:16]
anderson <- cbind(anderson, andersonFlu)

andrews <- counties.df[counties.df$Name=="Andrews",]
andrewsFlu <- wideDeath[2,3:16]
andrews <- cbind(andrews,andrewsFlu)

angelina <- counties.df[counties.df$Name=="Angelina",]
angelinaFlu <- wideDeath[3,3:16]
angelina <- cbind(angelina,angelinaFlu)

aransas <- counties.df[counties.df$id==3,]
aransasFlu <- wideDeath[4,3:16]
aransas <- cbind(aransas, aransasFlu)

archer <- counties.df[counties.df$id==4,]
archerFlu <- wideDeath[5,3:16]
archer <- cbind(archer, archerFlu)

armstrong <- counties.df[counties.df$Name=="Armstrong",]
armstrongFlu <- wideDeath[6,3:16]
armstrong <- cbind(armstrong, armstrongFlu)

atascosa <- counties.df[counties.df$id==6,]
atascosaFlu <- wideDeath[7,3:16]
atascosa <- cbind(atascosa,atascosaFlu)

austin <- counties.df[counties.df$Name=="Austin",]
austinFlu <- wideDeath[8,3:16]
austin <- cbind(austin,austinFlu)

bailey <- counties.df[counties.df$id==8,]
baileyFlu <- wideDeath[9,3:16]
bailey <- cbind(bailey,baileyFlu)

bandera <- counties.df[counties.df$Name=="Bandera",]
banderaFlu <- wideDeath[10,3:16]
bandera <- cbind(bandera,banderaFlu)

bastrop <- counties.df[counties.df$Name=="Bastrop",]
bastropFlu <- wideDeath[11,3:16]
bastrop <- cbind(bastrop,bastropFlu)

baylor <- counties.df[counties.df$Name=="Baylor",]
baylorFlu <- wideDeath[12,3:16]
baylor <- cbind(baylor,baylorFlu)

bee <- counties.df[counties.df$Name=="Bee",]
beeFlu <- wideDeath[13,3:16]
bee <- cbind(bee, beeFlu)

bell <- counties.df[counties.df$Name=="Bell",]
bellFlu <- wideDeath[14,3:16]
bell <- cbind(bell,bellFlu)

bexar <- counties.df[counties.df$Name=="Bexar",]
bexarFlu <- wideDeath[15,3:16]
bexar <- cbind(bexar,bexarFlu)

blanco <- counties.df[counties.df$Name=="Blanco",]
blancoFlu <- wideDeath[16,3:16]
blanco <- cbind(blanco,blancoFlu)

borden <- counties.df[counties.df$Name=="Borden",]
bordenFlu <- wideDeath[17,3:16]
borden <- cbind(borden,bordenFlu)

bosque <- counties.df[counties.df$Name=="Bosque",]
bosqueFlu <- wideDeath[18,3:16]
bosque <- cbind(bosque,bosqueFlu)

bowie <- counties.df[counties.df$Name=="Bowie",]
bowieFlu <- wideDeath[19,3:16]
bowie <- cbind(bowie, bowieFlu)

brazoria <- counties.df[counties.df$Name=="Brazoria",]
brazoriaFlu <- wideDeath[20,3:16]
brazoria <- cbind(brazoria, brazoriaFlu)

brazos <- counties.df[counties.df$Name=="Brazos",]
brazosFlu <- wideDeath[21,3:16]
brazos <- cbind(brazos, brazosFlu)

brewster <- counties.df[counties.df$Name=="Brewster",]
brewsterFlu <- wideDeath[22,3:16]
brewster <- cbind(brewster,brewsterFlu)

briscoe <- counties.df[counties.df$Name=="Briscoe",]
briscoeFlu <- wideDeath[23,3:16]
briscoe <- cbind(briscoe, briscoeFlu)

brooks <- counties.df[counties.df$Name=="Brooks",]
brooksFlu <- wideDeath[24,3:16]
brooks <- cbind(brooks,brooksFlu)

brown <- counties.df[counties.df$Name=="Brown",]
brownFlu <- wideDeath[25,3:16]
brown <- cbind(brown,brownFlu)

burleson <- counties.df[counties.df$Name=="Burleson",]
burlesonFlu <- wideDeath[26,3:16]
burleson <- cbind(burleson,burlesonFlu)

burnet <- counties.df[counties.df$Name=="Burnet",]
burnetFlu <- wideDeath[27,3:16]
burnet <- cbind(burnet,burnetFlu)

caldwell <- counties.df[counties.df$Name=="Caldwell",]
caldwellFlu <- wideDeath[28,3:16]
caldwell <- cbind(caldwell,caldwellFlu)

calhoun <- counties.df[counties.df$Name=="Calhoun",]
calhounFlu <- wideDeath[29,3:16]
calhoun <- cbind(calhoun, calhounFlu)

callahan <- counties.df[counties.df$Name=="Callahan",]
callahanFlu <- wideDeath[30,3:16]
callahan <- cbind(callahan,callahanFlu)

cameron <- counties.df[counties.df$Name=="Cameron",]
cameronFlu <- wideDeath[31,3:16]
cameron <- cbind(cameron,cameronFlu)

camp <- counties.df[counties.df$Name=="Camp",]
campFlu <- wideDeath[32,3:16]
camp <- cbind(camp,campFlu)

carson <- counties.df[counties.df$Name=="Carson",]
carsonFlu <- wideDeath[33,3:16]
carson <- cbind(carson,carsonFlu)

cass <- counties.df[counties.df$Name=="Cass",]
cassFlu <- wideDeath[34,3:16]
cass <- cbind(cass,cassFlu)

castro <- counties.df[counties.df$Name=="Castro",]
castroFlu <- wideDeath[35,3:16]
castro <- cbind(castro,castroFlu)

chambers <- counties.df[counties.df$Name=="Chambers",]
chambersFlu <- wideDeath[36,3:16]
chambers <- cbind(chambers,chambersFlu)

cherokee <- counties.df[counties.df$Name=="Cherokee",]
cherokeeFlu <- wideDeath[37,3:16]
cherokee <- cbind(cherokee,cherokeeFlu)

childress <- counties.df[counties.df$Name=="Childress",]
childressFlu <- wideDeath[38,3:16]
childress <- cbind(childress,childressFlu)

clay <- counties.df[counties.df$Name=="Clay",]
clayFlu <- wideDeath[39,3:16]
clay <- cbind(clay,clayFlu)

cochran <- counties.df[counties.df$Name=="Cochran",]
cochranFlu <- wideDeath[40,3:16]
cochran <- cbind(cochran,cochranFlu)

coke <- counties.df[counties.df$Name=="Coke",]
cokeFlu <- wideDeath[41,3:16]
coke <- cbind(coke,cokeFlu)

coleman <- counties.df[counties.df$Name=="Coleman",]
colemanFlu <- wideDeath[42,3:16]
coleman <- cbind(coleman,colemanFlu)

collin <- counties.df[counties.df$Name=="Collin",]
collinFlu <- wideDeath[43,3:16]
collin <- cbind(collin,collinFlu)

collingsworth <- counties.df[counties.df$Name=="Collingsworth",]
collingsworthFlu <- wideDeath[44,3:16]
collingsworth <- cbind(collingsworth,collingsworthFlu)

colorado <- counties.df[counties.df$Name=="Colorado",]
coloradoFlu <- wideDeath[45,3:16]
colorado <- cbind(colorado,coloradoFlu)

comal <- counties.df[counties.df$Name=="Comal",]
comalFlu <- wideDeath[46,3:16]
comal <- cbind(comal,comalFlu)

comanche <- counties.df[counties.df$Name=="Comanche",]
comancheFlu <- wideDeath[47,3:16]
comanche <- cbind(comanche,comancheFlu)

concho <- counties.df[counties.df$Name=="Concho",]
conchoFlu <- wideDeath[48,3:16]
concho <- cbind(concho,conchoFlu)

cooke <- counties.df[counties.df$Name=="Cooke",]
cookeFlu <- wideDeath[49,3:16]
cooke <- cbind(cooke,cookeFlu)

coryell <- counties.df[counties.df$Name=="Coryell",]
coryellFlu <- wideDeath[50,3:16]
coryell <- cbind(coryell,coryellFlu)

cottle <- counties.df[counties.df$Name=="Cottle",]
cottleFlu <- wideDeath[51,3:16]
cottle <- cbind(cottle,cottleFlu)

crane <- counties.df[counties.df$Name=="Crane",]
craneFlu <- wideDeath[52,3:16]
crane <- cbind(crane,craneFlu)

crockett <- counties.df[counties.df$Name=="Crockett",]
crockettFlu <- wideDeath[53,3:16]
crockett <- cbind(crockett,crockettFlu)

crosby <- counties.df[counties.df$Name=="Crosby",]
crosbyFlu <- wideDeath[54,3:16]
crosby <- cbind(crosby,crosbyFlu)

culberson <- counties.df[counties.df$Name=="Culberson",]
culbersonFlu <- wideDeath[55,3:16]
culberson <- cbind(culberson,culbersonFlu)

dallam <- counties.df[counties.df$Name=="Dallam",]
dallamFlu <- wideDeath[56,3:16]
dallam <- cbind(dallam,dallamFlu)

dallas <- counties.df[counties.df$Name=="Dallas",]
dallasFlu <- wideDeath[57,3:16]
dallas <- cbind(dallas,dallasFlu)

dawson <- counties.df[counties.df$Name=="Dawson",]
dawsonFlu <- wideDeath[58,3:16]
dawson <- cbind(dawson,dawsonFlu)

dewitt <- counties.df[counties.df$id==58,]
dewittFlu <- wideDeath[59,3:16]
dewitt <- cbind(dewitt,dewittFlu)

deafsmith <- counties.df[counties.df$Name=="Deaf Smith",]
deafsmithFlu <- wideDeath[60,3:16]
deafsmith <- cbind(deafsmith,deafsmithFlu)

delta <- counties.df[counties.df$Name=="Delta",]
deltaFlu <- wideDeath[61,3:16]
delta <- cbind(delta,deltaFlu)

denton <- counties.df[counties.df$Name=="Denton",]
dentonFlu <- wideDeath[62,3:16]
denton <- cbind(denton,dentonFlu)

dickens <- counties.df[counties.df$Name=="Dickens",]
dickensFlu <- wideDeath[63,3:16]
dickens <- cbind(dickens,dickensFlu)

dimmit <- counties.df[counties.df$Name=="Dimmit",]
dimmitFlu <- wideDeath[64,3:16]
dimmit <- cbind(dimmit,dimmitFlu)

donley <- counties.df[counties.df$Name=="Donley",]
donleyFlu <- wideDeath[65,3:16]
donley <- cbind(donley,donleyFlu)

duval <- counties.df[counties.df$Name=="Duval",]
duvalFlu <- wideDeath[66,3:16]
duval <- cbind(duval,duvalFlu)

eastland <- counties.df[counties.df$Name=="Eastland",]
eastlandFlu <- wideDeath[67,3:16]
eastland <- cbind(eastland,eastlandFlu)

ector <- counties.df[counties.df$Name=="Ector",]
ectorFlu <- wideDeath[68,3:16]
ector <- cbind(ector,ectorFlu)

edwards <- counties.df[counties.df$Name=="Edwards",]
edwardsFlu <- wideDeath[69,3:16]
edwards <- cbind(edwards,edwardsFlu)

elpaso <- counties.df[counties.df$Name=="El Paso",]
elpasoFlu <- wideDeath[70,3:16]
elpaso <- cbind(elpaso,elpasoFlu)

ellis <- counties.df[counties.df$Name=="Ellis",]
ellisFlu <- wideDeath[71,3:16]
ellis <- cbind(ellis,ellisFlu)

erath <- counties.df[counties.df$Name=="Erath",]
erathFlu <- wideDeath[72,3:16]
erath <- cbind(erath,erathFlu)

falls <- counties.df[counties.df$Name=="Falls",]
fallsFlu <- wideDeath[73,3:16]
falls <- cbind(falls,fallsFlu)

fannin <- counties.df[counties.df$Name=="Fannin",]
fanninFlu <- wideDeath[74,3:16]
fannin <- cbind(fannin,fanninFlu)

fayette <- counties.df[counties.df$Name=="Fayette",]
fayetteFlu <- wideDeath[75,3:16]
fayette <- cbind(fayette, fayetteFlu)

fisher <- counties.df[counties.df$Name=="Fisher",]
fisherFlu <- wideDeath[76,3:16]
fisher <- cbind(fisher,fisherFlu)

floyd <- counties.df[counties.df$Name=="Floyd",]
floydFlu <- wideDeath[77,3:16]
floyd <- cbind(floyd,floydFlu)

foard <- counties.df[counties.df$Name=="Foard",]
foardFlu <- wideDeath[78,3:16]
foard <- cbind(foard,foardFlu)

fortbend <- counties.df[counties.df$Name=="Fort Bend",]
fortbendFlu <- wideDeath[79,3:16]
fortbend <- cbind(fortbend,fortbendFlu)

franklin <- counties.df[counties.df$Name=="Franklin",]
franklinFlu <- wideDeath[80,3:16]
franklin <- cbind(franklin,franklinFlu)

freestone <- counties.df[counties.df$Name=="Freestone",]
freestoneFlu <- wideDeath[81,3:16]
freestone <- cbind(freestone,freestoneFlu)

frio <- counties.df[counties.df$Name=="Frio",]
frioFlu <- wideDeath[82,3:16]
frio <- cbind(frio,frioFlu)

gaines <- counties.df[counties.df$Name=="Gaines",]
gainesFlu <- wideDeath[83,3:16]
gaines <- cbind(gaines,gainesFlu)

galveston <- counties.df[counties.df$Name=="Galveston",]
galvestonFlu <- wideDeath[84,3:16]
galveston <- cbind(galveston,galvestonFlu)

garza <- counties.df[counties.df$Name=="Garza",]
garzaFlu <- wideDeath[85,3:16]
garza <- cbind(garza,garzaFlu)

gillespie <- counties.df[counties.df$Name=="Gillespie",]
gillespieFlu <- wideDeath[86,3:16]
gillespie <- cbind(gillespie,gillespieFlu)

glasscock <- counties.df[counties.df$Name=="Glasscock",]
glasscockFlu <- wideDeath[87,3:16]
glasscock <- cbind(glasscock,glasscockFlu)

goliad <- counties.df[counties.df$Name=="Goliad",]
goliadFlu <- wideDeath[88,3:16]
goliad <- cbind(goliad,goliadFlu)

gonzales <- counties.df[counties.df$Name=="Gonzales",]
gonzalesFlu <- wideDeath[89,3:16]
gonzales <- cbind(gonzales,gonzalesFlu)

gray <- counties.df[counties.df$Name=="Gray",]
grayFlu <- wideDeath[90,3:16]
gray <- cbind(gray,grayFlu)

grayson <- counties.df[counties.df$Name=="Grayson",]
graysonFlu <- wideDeath[91,3:16]
grayson <- cbind(grayson,graysonFlu)

gregg <- counties.df[counties.df$Name=="Gregg",]
greggFlu <- wideDeath[92,3:16]
gregg <- cbind(gregg,greggFlu)

grimes <- counties.df[counties.df$Name=="Grimes",]
grimesFlu <- wideDeath[93,3:16]
grimes <- cbind(grimes,grimesFlu)

guadalupe <- counties.df[counties.df$Name=="Guadalupe",]
guadalupeFlu <- wideDeath[94,3:16]
guadalupe <- cbind(guadalupe,guadalupeFlu)

hale <- counties.df[counties.df$Name=="Hale",]
haleFlu <- wideDeath[95,3:16]
hale <- cbind(hale,haleFlu)

hall <- counties.df[counties.df$Name=="Hall",]
hallFlu <- wideDeath[96,3:16]
hall <- cbind(hall,hallFlu)

hamilton <- counties.df[counties.df$Name=="Hamilton",]
hamiltonFlu <- wideDeath[97,3:16]
hamilton <- cbind(hamilton,hamiltonFlu)

hansford <- counties.df[counties.df$Name=="Hansford",]
hansfordFlu <- wideDeath[98,3:16]
hansford <- cbind(hansford,hansfordFlu)

hardeman <- counties.df[counties.df$Name=="Hardeman",]
hardemanFlu <- wideDeath[99,3:16]
hardeman <- cbind(hardeman,hardemanFlu)

hardin <- counties.df[counties.df$Name=="Hardin",]
hardinFlu <- wideDeath[100,3:16]
hardin <- cbind(hardin,hardinFlu)

harris <- counties.df[counties.df$Name=="Harris",]
harrisFlu <- wideDeath[101,3:16]
harris <- cbind(harris,harrisFlu)

harrison <- counties.df[counties.df$Name=="Harrison",]
harrisonFlu <- wideDeath[102,3:16]
harrison <- cbind(harrison,harrisonFlu)

hartley <- counties.df[counties.df$Name=="Hartley",]
hartleyFlu <- wideDeath[103,3:16]
hartley <- cbind(hartley,hartleyFlu)

haskell <- counties.df[counties.df$Name=="Haskell",]
haskellFlu <- wideDeath[104,3:16]
haskell <- cbind(haskell,haskellFlu)

hays <- counties.df[counties.df$Name=="Hays",]
haysFlu <- wideDeath[105,3:16]
hays <- cbind(hays,haysFlu)

hemphill <- counties.df[counties.df$Name=="Hemphill",]
hemphillFlu <- wideDeath[106,3:16]
hemphill <- cbind(hemphill,hemphillFlu)

henderson <- counties.df[counties.df$Name=="Henderson",]
hendersonFlu <- wideDeath[107,3:16]
henderson <- cbind(henderson,hendersonFlu)

hidalgo <- counties.df[counties.df$Name=="Hidalgo",]
hidalgoFlu <- wideDeath[108,3:16]
hidalgo <- cbind(hidalgo,hidalgoFlu)

hill <- counties.df[counties.df$Name=="Hill",]
hillFlu <- wideDeath[109,3:16]
hill <- cbind(hill,hillFlu)

hockley <- counties.df[counties.df$Name=="Hockley",]
hockleyFlu <- wideDeath[110,3:16]
hockley <- cbind(hockley,hockleyFlu)

hood <- counties.df[counties.df$Name=="Hood",]
hoodFlu <- wideDeath[111,3:16]
hood <- cbind(hood,hoodFlu)

hopkins <- counties.df[counties.df$Name=="Hopkins",]
hopkinsFlu <- wideDeath[112,3:16]
hopkins <- cbind(hopkins,hopkinsFlu)

houston <- counties.df[counties.df$Name=="Houston",]
houstonFlu <- wideDeath[113,3:16]
houston <- cbind(houston,houstonFlu)

howard <- counties.df[counties.df$Name=="Howard",]
howardFlu <- wideDeath[114,3:16]
howard <- cbind(howard,howardFlu)

hudspeth <- counties.df[counties.df$Name=="Hudspeth",]
hudspethFlu <- wideDeath[115,3:16]
hudspeth <- cbind(hudspeth,hudspethFlu)

hunt <- counties.df[counties.df$Name=="Hunt",]
huntFlu <- wideDeath[116,3:16]
hunt <- cbind(hunt,huntFlu)

hutchinson <- counties.df[counties.df$Name=="Hutchinson",]
hutchinsonFlu <- wideDeath[117,3:16]
hutchinson <- cbind(hutchinson,hutchinsonFlu)

irion <- counties.df[counties.df$Name=="Irion",]
irionFlu <- wideDeath[118,3:16]
irion <- cbind(irion,irionFlu)

jack <- counties.df[counties.df$Name=="Jack",]
jackFlu <- wideDeath[119,3:16]
jack <- cbind(jack,jackFlu)

jackson <- counties.df[counties.df$Name=="Jackson",]
jacksonFlu <- wideDeath[120,3:16]
jackson <- cbind(jackson,jacksonFlu)

jasper <- counties.df[counties.df$Name=="Jasper",]
jasperFlu <- wideDeath[121,3:16]
jasper <- cbind(jasper,jasperFlu)

jeffdavis <- counties.df[counties.df$Name=="Jeff Davis",]
jeffdavisFlu <- wideDeath[122,3:16]
jeffdavis <- cbind(jeffdavis,jeffdavisFlu)

jefferson <- counties.df[counties.df$Name=="Jefferson",]
jeffersonFlu <- wideDeath[123,3:16]
jefferson <- cbind(jefferson,jeffersonFlu)

jimhogg <- counties.df[counties.df$Name=="Jim Hogg",]
jimhoggFlu <- wideDeath[124,3:16]
jimhogg <- cbind(jimhogg,jimhoggFlu)

jimwells <- counties.df[counties.df$Name=="Jim Wells",]
jimwellsFlu <- wideDeath[125,3:16]
jimwells <- cbind(jimwells,jimwellsFlu)

johnson <- counties.df[counties.df$Name=="Johnson",]
johnsonFlu <- wideDeath[126,3:16]
johnson <- cbind(johnson,johnsonFlu)

jones <- counties.df[counties.df$Name=="Jones",]
jonesFlu <- wideDeath[127,3:16]
jones <- cbind(jones,jonesFlu)

karnes <- counties.df[counties.df$Name=="Karnes",]
karnesFlu <- wideDeath[128,3:16]
karnes <- cbind(karnes,karnesFlu)

kaufman <- counties.df[counties.df$Name=="Kaufman",]
kaufmanFlu <- wideDeath[129,3:16]
kaufman <- cbind(kaufman,kaufmanFlu)

kendall <- counties.df[counties.df$Name=="Kendall",]
kendallFlu <- wideDeath[130,3:16]
kendall <- cbind(kendall,kendallFlu)

kenedy <- counties.df[counties.df$Name=="Kenedy",]
kenedyFlu <- wideDeath[131,3:16]
kenedy <- cbind(kenedy,kenedyFlu)

kent <- counties.df[counties.df$Name=="Kent",]
kentFlu <- wideDeath[132,3:16]
kent <- cbind(kent,kentFlu)

kerr <- counties.df[counties.df$Name=="Kerr",]
kerrFlu <- wideDeath[133,3:16]
kerr <- cbind(kerr,kerrFlu)

kimble <- counties.df[counties.df$Name=="Kimble",]
kimbleFlu <- wideDeath[134,3:16]
kimble <- cbind(kimble,kimbleFlu)

king <- counties.df[counties.df$Name=="King",]
kingFlu <- wideDeath[135,3:16]
king <- cbind(king,kingFlu)

kinney <- counties.df[counties.df$Name=="Kinney",]
kinneyFlu <- wideDeath[136,3:16]
kinney <- cbind(kinney,kinneyFlu)

kleberg <- counties.df[counties.df$Name=="Kleberg",]
klebergFlu <- wideDeath[137,3:16]
kleberg <- cbind(kleberg,klebergFlu)

knox <- counties.df[counties.df$Name=="Knox",]
knoxFlu <- wideDeath[138,3:16]
knox <- cbind(knox,knoxFlu)

lasalle <- counties.df[counties.df$Name=="La Salle",]
lasalleFlu <- wideDeath[139,3:16]
lasalle <- cbind(lasalle,lasalleFlu)

lamar <- counties.df[counties.df$Name=="Lamar",]
lamarFlu <- wideDeath[140,3:16]
lamar <- cbind(lamar,lamarFlu)

lamb <- counties.df[counties.df$Name=="Lamb",]
lambFlu <- wideDeath[141,3:16]
lamb <- cbind(lamb,lambFlu)

lampasas <- counties.df[counties.df$Name=="Lampasas",]
lampasasFlu <- wideDeath[142,3:16]
lampasas <- cbind(lampasas,lampasasFlu)

lavaca <- counties.df[counties.df$Name=="Lavaca",]
lavacaFlu <- wideDeath[143,3:16]
lavaca <- cbind(lavaca,lavacaFlu)

lee <- counties.df[counties.df$Name=="Lee",]
leeFlu <- wideDeath[144,3:16]
lee <- cbind(lee,leeFlu)

leon <- counties.df[counties.df$Name=="Leon",]
leonFlu <- wideDeath[145,3:16]
leon <- cbind(leon,leonFlu)

liberty <- counties.df[counties.df$Name=="Liberty",]
libertyFlu <- wideDeath[146,3:16]
liberty <- cbind(liberty,libertyFlu)

limestone <- counties.df[counties.df$Name=="Limestone",]
limestoneFlu <- wideDeath[147,3:16]
limestone <- cbind(limestone,limestoneFlu)

lipscomb <- counties.df[counties.df$Name=="Lipscomb",]
lipscombFlu <- wideDeath[148,3:16]
lipscomb <- cbind(lipscomb,lipscombFlu)

liveoak <- counties.df[counties.df$Name=="Live Oak",]
liveoakFlu <- wideDeath[149,3:16]
liveoak <- cbind(liveoak,liveoakFlu)

llano <- counties.df[counties.df$Name=="Llano",]
llanoFlu <- wideDeath[150,3:16]
llano <- cbind(llano,llanoFlu)

loving <- counties.df[counties.df$Name=="Loving",]
lovingFlu <- wideDeath[151,3:16]
loving <- cbind(loving,lovingFlu)

lubbock <- counties.df[counties.df$Name=="Lubbock",]
lubbockFlu <- wideDeath[152,3:16]
lubbock <- cbind(lubbock, lubbockFlu)

lynn <- counties.df[counties.df$Name=="Lynn",]
lynnFlu <- wideDeath[153,3:16]
lynn <- cbind(lynn,lynnFlu)

madison <- counties.df[counties.df$Name=="Madison",]
madisonFlu <- wideDeath[154,3:16]
madison <- cbind(madison,madisonFlu)

marion <- counties.df[counties.df$Name=="Marion",]
marionFlu <- wideDeath[155,3:16]
marion <- cbind(marion,marionFlu)

martin <- counties.df[counties.df$Name=="Martin",]
martinFlu <- wideDeath[156,3:16]
martin <- cbind(martin,martinFlu)

mason <- counties.df[counties.df$Name=="Mason",]
masonFlu <- wideDeath[157,3:16]
mason <- cbind(mason,masonFlu)

matagorda <- counties.df[counties.df$Name=="Matagorda",]
matagordaFlu <- wideDeath[158,3:16]
matagorda <- cbind(matagorda,matagordaFlu)

maverick <- counties.df[counties.df$Name=="Maverick",]
maverickFlu <- wideDeath[159,3:16]
maverick <- cbind(maverick,maverickFlu)

mcculloch <- counties.df[counties.df$Name=="McCulloch",]
mccullochFlu <- wideDeath[160,3:16]
mcculloch <- cbind(mcculloch,mccullochFlu)

mclennan <- counties.df[counties.df$Name=="McLennan",]
mclennanFlu <- wideDeath[161,3:16]
mclennan <- cbind(mclennan,mclennanFlu)

mcmullen <- counties.df[counties.df$Name=="McMullen",]
mcmullenFlu <- wideDeath[162,3:16]
mcmullen <- cbind(mcmullen,mcmullenFlu)

medina <- counties.df[counties.df$Name=="Medina",]
medinaFlu <- wideDeath[163,3:16]
medina <- cbind(medina,medinaFlu)

menard <- counties.df[counties.df$Name=="Menard",]
menardFlu <- wideDeath[164,3:16]
menard <- cbind(menard,menardFlu)

midland <- counties.df[counties.df$Name=="Midland",]
midlandFlu <- wideDeath[165,3:16]
midland <- cbind(midland,midlandFlu)

milam <- counties.df[counties.df$Name=="Milam",]
milamFlu <- wideDeath[166,3:16]
milam <- cbind(milam,milamFlu)

mills <- counties.df[counties.df$Name=="Mills",]
millsFlu <- wideDeath[167,3:16]
mills <- cbind(mills,millsFlu)

mitchell <- counties.df[counties.df$Name=="Mitchell",]
mitchellFlu <- wideDeath[168,3:16]
mitchell <- cbind(mitchell,mitchellFlu)

montague <- counties.df[counties.df$Name=="Montague",]
montagueFlu <- wideDeath[169,3:16]
montague <- cbind(montague,montagueFlu)

montgomery <- counties.df[counties.df$Name=="Montgomery",]
montgomeryFlu <- wideDeath[170,3:16]
montgomery <- cbind(montgomery,montgomeryFlu)

moore <- counties.df[counties.df$Name=="Moore",]
mooreFlu <- wideDeath[171,3:16]
moore <- cbind(moore,mooreFlu)

morris <- counties.df[counties.df$Name=="Morris",]
morrisFlu <- wideDeath[172,3:16]
morris <- cbind(morris,morrisFlu)

motley <- counties.df[counties.df$Name=="Motley",]
motleyFlu <- wideDeath[173,3:16]
motley <- cbind(motley,motleyFlu)

nacogdoches <- counties.df[counties.df$Name=="Nacogdoches",]
nacogdochesFlu <- wideDeath[174,3:16]
nacogdoches <- cbind(nacogdoches,nacogdochesFlu)

navarro <- counties.df[counties.df$Name=="Navarro",]
navarroFlu <- wideDeath[175,3:16]
navarro <- cbind(navarro,navarroFlu)

newton <- counties.df[counties.df$Name=="Newton",]
newtonFlu <- wideDeath[176,3:16]
newton <- cbind(newton,newtonFlu)

nolan <- counties.df[counties.df$Name=="Nolan",]
nolanFlu <- wideDeath[177,3:16]
nolan <- cbind(nolan,nolanFlu)

nueces <- counties.df[counties.df$Name=="Nueces",]
nuecesFlu <- wideDeath[178,3:16]
nueces <- cbind(nueces,nuecesFlu)

ochiltree <- counties.df[counties.df$Name=="Ochiltree",]
ochiltreeFlu <- wideDeath[179,3:16]
ochiltree <- cbind(ochiltree,ochiltreeFlu)

oldham <- counties.df[counties.df$id==179,]
oldhamFlu <- wideDeath[180,3:16]
oldham <- cbind(oldham,oldhamFlu)

orange <- counties.df[counties.df$Name=="Orange",]
orangeFlu <- wideDeath[181,3:16]
orange <- cbind(orange,orangeFlu)

palopinto <- counties.df[counties.df$Name=="Palo Pinto",]
palopintoFlu <- wideDeath[182,3:16]
palopinto <- cbind(palopinto,palopintoFlu
)
panola <- counties.df[counties.df$Name=="Panola",]
panolaFlu <- wideDeath[183,3:16]
panola <- cbind(panola,panolaFlu)

parker <- counties.df[counties.df$Name=="Parker",]
parkerFlu <- wideDeath[184,3:16]
parker <- cbind(parker,parkerFlu)

parmer <- counties.df[counties.df$Name=="Parmer",]
parmerFlu <- wideDeath[185,3:16]
parmer <- cbind(parmer,parmerFlu)

pecos <- counties.df[counties.df$Name=="Pecos",]
pecosFlu <- wideDeath[186,3:16]
pecos <- cbind(pecos,pecosFlu)

polk <- counties.df[counties.df$Name=="Polk",]
polkFlu <- wideDeath[187,3:16]
polk <- cbind(polk,polkFlu)

potter <- counties.df[counties.df$Name=="Potter",]
potterFlu <- wideDeath[188,3:16]
potter <- cbind(potter,potterFlu)

presidio <- counties.df[counties.df$Name=="Presidio",]
presidioFlu <- wideDeath[189,3:16]
presidio <- cbind(presidio,presidioFlu)

rains <- counties.df[counties.df$Name=="Rains",]
rainsFlu <- wideDeath[190,3:16]
rains <- cbind(rains,rainsFlu)

randall <- counties.df[counties.df$Name=="Randall",]
randallFlu <- wideDeath[191,3:16]
randall <- cbind(randall,randallFlu)

reagan <- counties.df[counties.df$Name=="Reagan",]
reaganFlu <- wideDeath[192,3:16]
reagan <- cbind(reagan,reaganFlu)

real <- counties.df[counties.df$Name=="Real",]
realFlu <- wideDeath[193,3:16]
real <- cbind(real,realFlu)

redriver <- counties.df[counties.df$Name=="Red River",]
redriverFlu <- wideDeath[194,3:16]
redriver <- cbind(redriver,redriverFlu)

reeves <- counties.df[counties.df$Name=="Reeves",]
reevesFlu <- wideDeath[195,3:16]
reeves <- cbind(reeves,reevesFlu)

refugio <- counties.df[counties.df$Name=="Refugio",]
refugioFlu <- wideDeath[196,3:16]
refugio <- cbind(refugio,refugioFlu)

roberts <- counties.df[counties.df$Name=="Roberts",]
robertsFlu <- wideDeath[197,3:16]
roberts <- cbind(roberts,robertsFlu)

robertson <- counties.df[counties.df$Name=="Robertson",]
robertsonFlu <- wideDeath[198,3:16]
robertson <- cbind(robertson,robertsonFlu)

rockwall <- counties.df[counties.df$Name=="Rockwall",]
rockwallFlu <- wideDeath[199,3:16]
rockwall <- cbind(rockwall,rockwallFlu)

runnels <- counties.df[counties.df$Name=="Runnels",]
runnelsFlu <- wideDeath[200,3:16]
runnels <- cbind(runnels,runnelsFlu)

rusk <- counties.df[counties.df$Name=="Rusk",]
ruskFlu <- wideDeath[201,3:16]
rusk <- cbind(rusk,ruskFlu)

sabine <- counties.df[counties.df$Name=="Sabine",]
sabineFlu <- wideDeath[202,3:16]
sabine <- cbind(sabine,sabineFlu)

sanaugustine <- counties.df[counties.df$Name=="San Augustine",]
sanaugustineFlu <- wideDeath[203,3:16]
sanaugustine <- cbind(sanaugustine,sanaugustineFlu)

sanjacinto <- counties.df[counties.df$Name=="San Jacinto",]
sanjacintoFlu <- wideDeath[204,3:16]
sanjacinto <- cbind(sanjacinto,sanjacintoFlu)

sanpatricio <- counties.df[counties.df$Name=="San Patricio",]
sanpatricioFlu <- wideDeath[205,3:16]
sanpatricio <- cbind(sanpatricio,sanpatricioFlu)

sansaba <- counties.df[counties.df$Name=="San Saba",]
sansabaFlu <- wideDeath[206,3:16]
sansaba <- cbind(sansaba,sansabaFlu)

schleicher <- counties.df[counties.df$Name=="Schleicher",]
schleicherFlu <- wideDeath[207,3:16]
schleicher <- cbind(schleicher,schleicherFlu)

scurry <- counties.df[counties.df$Name=="Scurry",]
scurryFlu <- wideDeath[208,3:16]
scurry <- cbind(scurry,scurryFlu)

schackelford <- counties.df[counties.df$id==208,]
schackelfordFlu <- wideDeath[209,3:16]
schackelford <- cbind(schackelford,schackelfordFlu)

shelby <- counties.df[counties.df$Name=="Shelby",]
shelbyFlu <- wideDeath[210,3:16]
shelby <- cbind(shelby,shelbyFlu)

sherman <- counties.df[counties.df$Name=="Sherman",]
shermanFlu <- wideDeath[211,3:16]
sherman <- cbind(sherman,shermanFlu)

smith <- counties.df[counties.df$Name=="Smith",]
smithFlu <- wideDeath[212,3:16]
smith <- cbind(smith,smithFlu)

somervell <- counties.df[counties.df$Name=="Somervell",]
somervellFlu <- wideDeath[213,3:16]
somervell <- cbind(somervell,somervellFlu)

starr <- counties.df[counties.df$Name=="Starr",]
starrFlu <- wideDeath[214,3:16]
starr <- cbind(starr,starrFlu)

stephens <- counties.df[counties.df$Name=="Stephens",]
stephensFlu <- wideDeath[215,3:16]
stephens <- cbind(stephens,stephensFlu)

sterling <- counties.df[counties.df$Name=="Sterling",]
sterlingFlu <- wideDeath[216,3:16]
sterling <- cbind(sterling,sterlingFlu)

stonewall <- counties.df[counties.df$Name=="Stonewall",]
stonewallFlu <- wideDeath[217,3:16]
stonewall <- cbind(stonewall,stonewallFlu)

sutton <- counties.df[counties.df$Name=="Sutton",]
suttonFlu <- wideDeath[218,3:16]
sutton <- cbind(sutton,suttonFlu)

swisher <- counties.df[counties.df$Name=="Swisher",]
swisherFlu <- wideDeath[219,3:16]
swisher <- cbind(swisher,swisherFlu)

tarrant <- counties.df[counties.df$Name=="Tarrant",]
tarrantFlu <- wideDeath[220,3:16]
tarrant <- cbind(tarrant,tarrantFlu)

taylor <- counties.df[counties.df$Name=="Taylor",]
taylorFlu <- wideDeath[221,3:16]
taylor <- cbind(taylor,taylorFlu)

terrell <- counties.df[counties.df$Name=="Terrell",]
terrellFlu <- wideDeath[222,3:16]
terrell <- cbind(terrell,terrellFlu)

terry <- counties.df[counties.df$Name=="Terry",]
terryFlu <- wideDeath[223,3:16]
terry <- cbind(terry,terryFlu)

throckmorton <- counties.df[counties.df$Name=="Throckmorton",]
throckmortonFlu <- wideDeath[224,3:16]
throckmorton <- cbind(throckmorton,throckmortonFlu)

titus <- counties.df[counties.df$Name=="Titus",]
titusFlu <- wideDeath[225,3:16]
titus <- cbind(titus,titusFlu)

tomgreen <- counties.df[counties.df$Name=="Tom Green",]
tomgreenFlu <- wideDeath[226,3:16]
tomgreen <- cbind(tomgreen,tomgreenFlu)

travis <- counties.df[counties.df$Name=="Travis",]
travisFlu <- wideDeath[227,3:16]
travis <- cbind(travis,travisFlu)

trinity <- counties.df[counties.df$Name=="Trinity",]
trinityFlu <- wideDeath[228,3:16]
trinity <- cbind(trinity,trinityFlu)

tyler <- counties.df[counties.df$Name=="Tyler",]
tylerFlu <- wideDeath[229,3:16]
tyler <- cbind(tyler,tylerFlu)

upshur <- counties.df[counties.df$Name=="Upshur",]
upshurFlu <- wideDeath[230,3:16]
upshur <- cbind(upshur,upshurFlu)

upton <- counties.df[counties.df$Name=="Upton",]
uptonFlu <- wideDeath[231,3:16]
upton <- cbind(upton,uptonFlu)

uvalde <- counties.df[counties.df$Name=="Uvalde",]
uvaldeFlu <- wideDeath[232,3:16]
uvalde <- cbind(uvalde,uvaldeFlu)

valverde <- counties.df[counties.df$Name=="Val Verde",]
valverdeFlu <- wideDeath[233,3:16]
valverde <- cbind(valverde,valverdeFlu)

vanzandt <- counties.df[counties.df$Name=="Van Zandt",]
vanzandtFlu <- wideDeath[234,3:16]
vanzandt <- cbind(vanzandt,vanzandtFlu)

victoria <- counties.df[counties.df$Name=="Victoria",]
victoriaFlu <- wideDeath[235,3:16]
victoria <- cbind(victoria,victoriaFlu)

walker <- counties.df[counties.df$Name=="Walker",]
walkerFlu <- wideDeath[236,3:16]
walker <- cbind(walker,walkerFlu)

waller <- counties.df[counties.df$Name=="Waller",]
wallerFlu <- wideDeath[237,3:16]
waller <- cbind(waller,wallerFlu)

ward <- counties.df[counties.df$Name=="Ward",]
wardFlu <- wideDeath[238,3:16]
ward <- cbind(ward,wardFlu)

washington <- counties.df[counties.df$Name=="Washington",]
washingtonFlu <- wideDeath[239,3:16]
washington <- cbind(washington,washingtonFlu)

webb <- counties.df[counties.df$Name=="Webb",]
webbFlu <- wideDeath[240,3:16]
webb <- cbind(webb,webbFlu)

wharton <- counties.df[counties.df$Name=="Wharton",]
whartonFlu <- wideDeath[241,3:16]
wharton <- cbind(wharton,whartonFlu)

wheeler <- counties.df[counties.df$id==241,]
wheelerFlu <- wideDeath[242,3:16]
wheeler <- cbind(wheeler,wheelerFlu)

wichita <- counties.df[counties.df$id==242,]
wichitaFlu <- wideDeath[243,3:16]
wichita <- cbind(wichita,wichitaFlu)

wilbarger <- counties.df[counties.df$Name=="Wilbarger",]
wilbargerFlu <- wideDeath[244,3:16]
wilbarger <- cbind(wilbarger,wilbargerFlu)

willacy <- counties.df[counties.df$Name=="Willacy",]
willacyFlu <- wideDeath[245,3:16]
willacy <- cbind(willacy,willacyFlu)

williamson <- counties.df[counties.df$Name=="Williamson",]
williamsonFlu <- wideDeath[246,3:16]
williamson <- cbind(williamson,williamsonFlu)

wilson <- counties.df[counties.df$Name=="Wilson",]
wilsonFlu <- wideDeath[247,3:16]
wilson <- cbind(wilson,wilsonFlu)

winkler <- counties.df[counties.df$Name=="Winkler",]
winklerFlu <- wideDeath[248,3:16]
winkler <- cbind(winkler,winklerFlu)

wise <- counties.df[counties.df$Name=="Wise",]
wiseFlu <- wideDeath[249,3:16]
wise <- cbind(wise,wiseFlu)

wood <- counties.df[counties.df$Name=="Wood",]
woodFlu <- wideDeath[250,3:16]
wood <- cbind(wood,woodFlu)

yoakum <- counties.df[counties.df$Name=="Yoakum",]
yoakumFlu <- wideDeath[251,3:16]
yoakum <- cbind(yoakum,yoakumFlu)

young <- counties.df[counties.df$Name=="Young",]
youngFlu <- wideDeath[252,3:16]
young <- cbind(young,youngFlu)

zapata <- counties.df[counties.df$Name=="Zapata",]
zapataFlu <- wideDeath[253,3:16]
zapata <- cbind(zapata,zapataFlu)

zavala <- counties.df[counties.df$Name=="Zavala",]
zavalaFlu <- wideDeath[254,3:16]
zavala <- cbind(zavala,zavalaFlu)

##Make County List ####
coList <- list(anderson,andrews,angelina,aransas,archer,armstrong,atascosa,austin,bailey,
               bandera,bastrop,baylor,bee,bell,bexar,blanco,borden,bosque,bowie,brazoria,
               brazos,brewster,briscoe,brooks,brown,burleson,burnet,caldwell,calhoun,
               callahan,cameron,camp,carson,cass,castro,chambers,cherokee,childress,clay,
               cochran,coke,coleman,collin,collingsworth,colorado,comal,comanche,concho,
               cooke,coryell,cottle,crane,crockett,crosby,culberson,dallam,dallas,dawson,
               dewitt,deafsmith,delta,denton,dickens,dimmit,donley,duval,eastland,ector,
               edwards,elpaso,ellis,erath,falls,fannin,fayette,fisher,floyd,foard,
               fortbend,franklin,freestone,frio,gaines,galveston,garza,gillespie,
               glasscock,goliad,gonzales,gray,grayson,gregg,grimes,guadalupe,hale,hall,
               hamilton,hansford,hardeman,hardin,harris,harrison,hartley,haskell,hays,
               hemphill,henderson,hidalgo,hill,hockley,hood,hopkins,houston,howard,
               hudspeth,hunt,hutchinson,irion,jack,jackson,jasper,jeffdavis,jefferson,
               jimhogg,jimwells,johnson,jones,karnes,kaufman,kendall,kenedy,kent,kerr,  
               kimble,king,kinney,kleberg,knox,lasalle,lamar,lamb,lampasas,lavaca,
               lee,leon,liberty,limestone,lipscomb,liveoak,llano,loving,lubbock,lynn,
               madison,marion,martin,mason,matagorda,maverick,mcculloch,mclennan,mcmullen,
               medina,menard,midland,milam,mills,mitchell,montague,montgomery,moore,morris,
               motley,nacogdoches,navarro,newton,nolan,nueces,ochiltree,oldham,orange,
               palopinto,panola,parker, parmer,pecos,polk,potter,presidio,rains,randall,
               reagan,real,redriver,reeves,refugio,roberts,robertson,rockwall,runnels,
               rusk,sabine,sanaugustine,sanjacinto,sanpatricio,sansaba,schleicher,scurry,
               schackelford,shelby,sherman,smith,somervell,starr,stephens,sterling,
               stonewall,sutton,swisher,tarrant,taylor,terrell,terry,throckmorton,titus,
               tomgreen,travis,trinity,tyler,upshur,upton,uvalde,valverde,vanzandt,
               victoria,walker,waller,ward,washington,webb,wharton,wheeler,wichita,
               wilbarger,willacy,williamson,wilson,winkler,wise,wood,yoakum,young,zapata,zavala)
##Create One List ####
counties <- rbind.fill(coList)

##Map if Statement ####
if(input$year == 1) { 
    ggplot(counties, aes(long, lat, group = group, fill = y1999)) + geom_polygon() +
        coord_equal()
    
} else if(input$year == 2) {
    ggplot(counties, aes(long, lat, group = group, fill = y2000)) + geom_polygon() +
        coord_equal()
    
} else if(input$year == 3){
    ggplot(counties, aes(long, lat, group = group, fill = y2001)) + geom_polygon() + 
        coord_equal()
    
} else if(input$year == 4){
    ggplot(counties, aes(long, lat, group = group, fill = y2002)) + geom_polygon() +
        coord_equal()
    
} else if(input$year == 5){
    ggplot(counties, aes(long, lat, group = group, fill = y2003)) + geom_polygon() + 
        coord_equal()
    
} else if(input$year == 6){
    ggplot(counties, aes(long, lat, group = group, fill = y2004)) + geom_polygon() + 
        coord_equal()
    
} else if(input$year == 7){
    ggplot(counties, aes(long, lat, group = group, fill = y2005)) + geom_polygon() + 
        coord_equal()
    
} else if(input$year){
    ggplot(counties, aes(long, lat, group = group, fill = y2006)) + geom_polygion() + 
        coord_equal()
    
} else if(input$year == 8){
    ggplot(counties, aes(long, lat, group = group, fill = y2007)) + geom_polygon() +
        coord_equal()
    
} else if(input$year == 9){
    ggplot(counties, aes(long, lat, group = group, fill = y2008)) + geom_polygon() +
        coord_equal()
    
} else if(input$year == 10){
    ggplot(counties, aes(long, lat, group = group, fill = y2009)) + geom_polygon() + 
        coord_equal()
    
} else if(input$year ==11){
    ggplot(counties, aes(long, lat, group = group, fill = y2010)) + geom_polygon() +
        coord_equal()
    
} else if(input$year == 12){
    ggplot(counties, aes(long, lat, group = group, fill = y2011)) + geom_polygon() + 
        coord_equal()
    
} else if(input$year == 13){
    ggplot(counties, aes(long, lat, group = group, fill = y2012)) + geom_polygon() + 
        coord_equal()
    
} else {
    print
}


})
})

    