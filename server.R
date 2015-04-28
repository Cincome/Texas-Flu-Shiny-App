library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(DT)


death <- read.csv("death101.csv")
data <- death
shinyServer(function(input, output){
    



    output$Plot <- renderPlot({
## Anderson - Brazoria ####
        if(input$county == 1) { 
            newDeath <- filter(death, grepl("Anderson", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 2) {
            newDeath <- filter(death, grepl("Andrews", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 3){
            newDeath <- filter(death, grepl("Angelina", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 4) {
            newDeath <- filter(death, grepl("Aransas", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 5) {
            newDeath <- filter(death, grepl("Archer", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 6) {
            newDeath <- filter(death, grepl("Armstrong", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 7) {
            newDeath <- filter(death, grepl("Atascosa", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 8) {
            newDeath <- filter(death, grepl("Austin", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 9) {
            newDeath <- filter(death, grepl("Bailey", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 10) {
            newDeath <- filter(death, grepl("Bandera", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 11) {
            newDeath <- filter(death, grepl("Bastrop", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 12) {
            newDeath <- filter(death, grepl("Baylor", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 13) {
            newDeath <- filter(death, grepl("Bee", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 14) {
            newDeath <- filter(death, grepl("Bell", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
        } else if(input$county == 15) {
            newDeath <- filter(death, grepl("Bexar", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
        } else if(input$county == 16) {
            newDeath <- filter(death, grepl("Blanco", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
        } 
        else if(input$county == 17) {
            newDeath <- filter(death, grepl("Borden", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
        } else if(input$county == 18) {
            newDeath <- filter(death, grepl("Bosque", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
        } else if(input$county == 19) {
            newDeath <- filter(death, grepl("Bowie", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
        } 
        else if(input$county == 20) {
            newDeath <- filter(death, grepl("Brazoria", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
## Brazos - Cochran ####    
        } else if(input$county == 21) {
            newDeath <- filter(death, grepl("Brazos", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 22) {
            newDeath <- filter(death, grepl("Brewster", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } 
        else if(input$county == 23) {
            newDeath <- filter(death, grepl("Briscoe", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))    
        
        } else if(input$county == 24) {
            newDeath <- filter(death, grepl("Brooks", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 25) {
            newDeath <- filter(death, grepl("Brown", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
        } 
        else if(input$county == 26) {
            newDeath <- filter(death, grepl("Burleson", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 27) {
            newDeath <- filter(death, grepl("Burnet", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 28) {
            newDeath <- filter(death, grepl("Caldwell", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } 
        else if(input$county == 29) {
            newDeath <- filter(death, grepl("Calhoun", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 30) {
            newDeath <- filter(death, grepl("Callahan", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 31) {
            newDeath <- filter(death, grepl("Cameron", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
        } 
        else if(input$county == 32) {
            newDeath <- filter(death, grepl("Camp", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 33) {
            newDeath <- filter(death, grepl("Carson", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 34) {
            newDeath <- filter(death, grepl("Cass", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } 
        else if(input$county == 35) {
            newDeath <- filter(death, grepl("Castro", County))    
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 36) {
            newDeath <- filter(death, grepl("Chambers", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 37) {
            newDeath <- filter(death, grepl("Cherokee", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } 
        else if(input$county == 38) {
            newDeath <- filter(death, grepl("Childress", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 39) {
            newDeath <- filter(death, grepl("Clay", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 40) {
            newDeath <- filter(death, grepl("Cochran", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
        } 
##Coke - Delta####  
        else if(input$county == 41) {
            newDeath <- filter(death, grepl("Coke", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 42) {
            newDeath <- filter(death, grepl("Coleman", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 43) {
            newDeath <- filter(death, grepl("Collin", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } 
        else if(input$county == 44) {
            newDeath <- filter(death, grepl("Collingsworth", County))    
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 45) {
            newDeath <- filter(death, grepl("Colorado", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 46) {
            newDeath <- filter(death, grepl("Comal", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } 
        else if(input$county == 47) {
            newDeath <- filter(death, grepl("Comanche", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 48) {
            newDeath <- filter(death, grepl("Concho", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 49) {
            newDeath <- filter(death, grepl("Cooke", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } 
        else if(input$county == 50) {
            newDeath <- filter(death, grepl("Coryell", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 51) {
            newDeath <- filter(death, grepl("Cottle", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
        } 
        else if(input$county == 52) {
            newDeath <- filter(death, grepl("Crane", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 53) {
            newDeath <- filter(death, grepl("Crockett", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 54) {
            newDeath <- filter(death, grepl("Crosby", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
        } 
        else if(input$county == 55) {
            newDeath <- filter(death, grepl("Culberson", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 56) {
            newDeath <- filter(death, grepl("Dallam", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 57) {
            newDeath <- filter(death, grepl("Dallas", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } 
        else if(input$county == 58) {
            newDeath <- filter(death, grepl("Dawson", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 59) {
            newDeath <- filter(death, grepl("Deaf Smith", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 60) {
            newDeath <- filter(death, grepl("Delta", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } 
##Denton- Franklin  ####
        else if(input$county == 61) {
            newDeath <- filter(death, grepl("Denton", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 62) {
            newDeath <- filter(death, grepl("DeWitt", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 63) {
            newDeath <- filter(death, grepl("Dickens", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 64) {
            newDeath <- filter(death, grepl("Dimmit", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 65) {
            newDeath <- filter(death, grepl("Donley", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 66) {
            newDeath <- filter(death, grepl("Duval", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 67) {
            newDeath <- filter(death, grepl("Eastland", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 68) {
            newDeath <- filter(death, grepl("Ector", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 69) {
            newDeath <- filter(death, grepl("Edwards", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 70) {
            newDeath <- filter(death, grepl("Ellis", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 71) {
            newDeath <- filter(death, grepl("El Paso", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 72) {
            newDeath <- filter(death, grepl("Erath", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 73) {
            newDeath <- filter(death, grepl("Falls", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 74) {
            newDeath <- filter(death, grepl("Fannin", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 75) {
            newDeath <- filter(death, grepl("Fayette", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 76) {
            newDeath <- filter(death, grepl("Fisher", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 77) {
            newDeath <- filter(death, grepl("Floyd", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 78) {
            newDeath <- filter(death, grepl("Foard", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 79) {
            newDeath <- filter(death, grepl("Fort Bend", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 80) {
            newDeath <- filter(death, grepl("Franklin", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
##Freestone- Hardin  ####            
        } else if(input$county == 81) {
            newDeath <- filter(death, grepl("Freestone", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 82) {
            newDeath <- filter(death, grepl("Frio", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 83) {
            newDeath <- filter(death, grepl("Gaines", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 84) {
            newDeath <- filter(death, grepl("Galveston", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 85) {
            newDeath <- filter(death, grepl("Garza", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 86) {
            newDeath <- filter(death, grepl("Gillespie", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 87) {
            newDeath <- filter(death, grepl("Glasscock", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 88) {
            newDeath <- filter(death, grepl("Goliad", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 89) {
            newDeath <- filter(death, grepl("Gonzales", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 90) {
            newDeath <- filter(death, grepl("Gray", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 91) {
            newDeath <- filter(death, grepl("Grayson", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 92) {
            newDeath <- filter(death, grepl("Gregg", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 93) {
            newDeath <- filter(death, grepl("Grimes", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 94) {
            newDeath <- filter(death, grepl("Guadalupe", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 95) {
            newDeath <- filter(death, grepl("Hale", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 96) {
            newDeath <- filter(death, grepl("Hall", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 97) {
            newDeath <- filter(death, grepl("Hamilton", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 98) {
            newDeath <- filter(death, grepl("Hansford", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 99) {
            newDeath <- filter(death, grepl("Hardeman", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 100) {
            newDeath <- filter(death, grepl("Hardin", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
##Harris- Jackson  ####            
        } else if(input$county == 101) {
            newDeath <- filter(death, grepl("Harris", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 102) {
            newDeath <- filter(death, grepl("Harrison", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 103) {
            newDeath <- filter(death, grepl("Hartley", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 104) {
            newDeath <- filter(death, grepl("Haskell", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 105) {
            newDeath <- filter(death, grepl("Hays", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 106) {
            newDeath <- filter(death, grepl("Hemphill", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 107) {
            newDeath <- filter(death, grepl("Henderson", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 108) {
            newDeath <- filter(death, grepl("Hidalgo", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 109) {
            newDeath <- filter(death, grepl("Hill", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 110) {
            newDeath <- filter(death, grepl("Hockley", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 111) {
            newDeath <- filter(death, grepl("Hood", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 112) {
            newDeath <- filter(death, grepl("Hopkins", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 113) {
            newDeath <- filter(death, grepl("Houston", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 114) {
            newDeath <- filter(death, grepl("Howard", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 115) {
            newDeath <- filter(death, grepl("Hudspeth", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 116) {
            newDeath <- filter(death, grepl("Hunt", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 117) {
            newDeath <- filter(death, grepl("Hutchinson", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 118) {
            newDeath <- filter(death, grepl("Irion", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 119) {
            newDeath <- filter(death, grepl("Jack", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 120) {
            newDeath <- filter(death, grepl("Jackson", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
##Jasper-Lamar   ####            
        } else if(input$county == 121) {
            newDeath <- filter(death, grepl("Jasper", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 122) {
            newDeath <- filter(death, grepl("Jeff Davis", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 123) {
            newDeath <- filter(death, grepl("Jefferson", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 124) {
            newDeath <- filter(death, grepl("Jim Hogg", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 125) {
            newDeath <- filter(death, grepl("Jim Wells", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 126) {
            newDeath <- filter(death, grepl("Johnson", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        } else if(input$county == 127) {
            newDeath <- filter(death, grepl("Jones", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 128) {
            newDeath <- filter(death, grepl("Karnes", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 129) {
            newDeath <- filter(death, grepl("Kaufman", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 130) {
            newDeath <- filter(death, grepl("Kendall", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 131) {
            newDeath <- filter(death, grepl("Kenedy", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 132) {
            newDeath <- filter(death, grepl("Kent", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 133) {
            newDeath <- filter(death, grepl("Kerr", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 134) {
            newDeath <- filter(death, grepl("Kimble", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 135) {
            newDeath <- filter(death, grepl("King", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 136) {
            newDeath <- filter(death, grepl("Kinney", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 137) {
            newDeath <- filter(death, grepl("Kleberg", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 138) {
            newDeath <- filter(death, grepl("Knox", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 139) {
            newDeath <- filter(death, grepl("La Salle", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 140) {
            newDeath <- filter(death, grepl("Lamar", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
##Lamb- McCulloh  ####            
        }else if(input$county == 141) {
            newDeath <- filter(death, grepl("Lamb", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 142) {
            newDeath <- filter(death, grepl("Lampasas", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 143) {
            newDeath <- filter(death, grepl("Lavaca", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 144) {
            newDeath <- filter(death, grepl("Lee", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 145) {
            newDeath <- filter(death, grepl("Leon", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 146) {
            newDeath <- filter(death, grepl("Liberty", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 147) {
            newDeath <- filter(death, grepl("Limestone", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 148) {
            newDeath <- filter(death, grepl("Lipscomb", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 149) {
            newDeath <- filter(death, grepl("Live Oak", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 150) {
            newDeath <- filter(death, grepl("Llano", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 151) {
            newDeath <- filter(death, grepl("Loving", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 152) {
            newDeath <- filter(death, grepl("Lubbock", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 153) {
            newDeath <- filter(death, grepl("Lynn", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 154) {
            newDeath <- filter(death, grepl("Madison", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 155) {
            newDeath <- filter(death, grepl("Marion", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 156) {
            newDeath <- filter(death, grepl("Martin", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 157) {
            newDeath <- filter(death, grepl("Mason", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 158) {
            newDeath <- filter(death, grepl("Matagorda", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 159) {
            newDeath <- filter(death, grepl("Maverick", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 160) {
            newDeath <- filter(death, grepl("McCulloch", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
##McLennan-Oldham   ####            
        }else if(input$county == 161) {
            newDeath <- filter(death, grepl("McLennan", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 162) {
            newDeath <- filter(death, grepl("McMullen", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 163) {
            newDeath <- filter(death, grepl("Medina", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 164) {
            newDeath <- filter(death, grepl("Menard", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 165) {
            newDeath <- filter(death, grepl("Midland", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 166) {
            newDeath <- filter(death, grepl("Milam", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 167) {
            newDeath <- filter(death, grepl("Mills", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 168) {
            newDeath <- filter(death, grepl("Mitchell", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 169) {
            newDeath <- filter(death, grepl("Montague", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 170) {
            newDeath <- filter(death, grepl("Montgomery", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 171) {
            newDeath <- filter(death, grepl("Moore", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 172) {
            newDeath <- filter(death, grepl("Morris", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 173) {
            newDeath <- filter(death, grepl("Motley", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 174) {
            newDeath <- filter(death, grepl("Nacogdoches", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 175) {
            newDeath <- filter(death, grepl("Navarro", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 176) {
            newDeath <- filter(death, grepl("Newton", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 177) {
            newDeath <- filter(death, grepl("Nolan", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 178) {
            newDeath <- filter(death, grepl("Nueces", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 179) {
            newDeath <- filter(death, grepl("Ochiltree", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 180) {
            newDeath <- filter(death, grepl("Oldham", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
##Orange-Runnels ####            
        }else if(input$county == 181) {
            newDeath <- filter(death, grepl("Orange", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 182) {
            newDeath <- filter(death, grepl("Palo Pinto", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 183) {
            newDeath <- filter(death, grepl("Panola", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 184) {
            newDeath <- filter(death, grepl("Parker", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 185) {
            newDeath <- filter(death, grepl("Parmer", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 186) {
            newDeath <- filter(death, grepl("Pecos", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 187) {
            newDeath <- filter(death, grepl("Polk", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 188) {
            newDeath <- filter(death, grepl("Potter", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 189) {
            newDeath <- filter(death, grepl("Presidio", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 190) {
            newDeath <- filter(death, grepl("Rains", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 191) {
            newDeath <- filter(death, grepl("Randall", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 192) {
            newDeath <- filter(death, grepl("Reagan", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 193) {
            newDeath <- filter(death, grepl("Real", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 194) {
            newDeath <- filter(death, grepl("Red River", County))    
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 195) {
            newDeath <- filter(death, grepl("Reeves", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 196) {
            newDeath <- filter(death, grepl("Refugio", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 197) {
            newDeath <- filter(death, grepl("Roberts", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 198) {
            newDeath <- filter(death, grepl("Robertson", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 199) {
            newDeath <- filter(death, grepl("Rockwall", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 200) {
            newDeath <- filter(death, grepl("Runnels", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
##Rusk -Tarrant ####            
        }else if(input$county == 201) {
            newDeath <- filter(death, grepl("Rusk", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 202) {
            newDeath <- filter(death, grepl("Sabine", County))    
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 203) {
            newDeath <- filter(death, grepl("San Augustine", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 204) {
            newDeath <- filter(death, grepl("San Jacinto", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 205) {
            newDeath <- filter(death, grepl("San Patricio", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 206) {
            newDeath <- filter(death, grepl("San Saba", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 207) {
            newDeath <- filter(death, grepl("Schleicher", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 208) {
            newDeath <- filter(death, grepl("Scurry", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 209) {
            newDeath <- filter(death, grepl("Shackelford", County))            
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 210) {
            newDeath <- filter(death, grepl("Shelby", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 211) {
            newDeath <- filter(death, grepl("Sherman", County))            
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 212) {
            newDeath <- filter(death, grepl("Smith", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 213) {
            newDeath <- filter(death, grepl("Somervell", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 214) {
            newDeath <- filter(death, grepl("Starr", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 215) {
            newDeath <- filter(death, grepl("Stephens", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 216) {
            newDeath <- filter(death, grepl("Sterling", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 217) {
            newDeath <- filter(death, grepl("Stonewall", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 218) {
            newDeath <- filter(death, grepl("Sutton", County))        
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 219) {
            newDeath <- filter(death, grepl("Swisher", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 220) {
            newDeath <- filter(death, grepl("Tarrant", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
##Taylor- Webb ####            
        }else if(input$county == 221) {
            newDeath <- filter(death, grepl("Taylor", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 222) {
            newDeath <- filter(death, grepl("Terrell", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 223) {
            newDeath <- filter(death, grepl("Terry", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 224) {
            newDeath <- filter(death, grepl("Throckmorton", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 225) {
            newDeath <- filter(death, grepl("Titus", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 226) {
            newDeath <- filter(death, grepl("Tom Green", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 227) {
            newDeath <- filter(death, grepl("Travis", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 228) {
            newDeath <- filter(death, grepl("Trinity", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 229) {
            newDeath <- filter(death, grepl("Tyler", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 230) {
            newDeath <- filter(death, grepl("Upshur", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 231) {
            newDeath <- filter(death, grepl("Upton", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 232) {
            newDeath <- filter(death, grepl("Uvalde", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 233) {
            newDeath <- filter(death, grepl("Val Verde", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 234) {
            newDeath <- filter(death, grepl("Van Zandt", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 235) {
            newDeath <- filter(death, grepl("Victoria", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 236) {
            newDeath <- filter(death, grepl("Walker", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 237) {
            newDeath <- filter(death, grepl("Waller", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 238) {
            newDeath <- filter(death, grepl("Ward", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 239) {
            newDeath <- filter(death, grepl("Washington", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 240) {
            newDeath <- filter(death, grepl("Webb", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
##Wharton-  #####            
        }else if(input$county == 241) {
            newDeath <- filter(death, grepl("Wharton", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 242) {
            newDeath <- filter(death, grepl("Wheeler", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 243) {
            newDeath <- filter(death, grepl("Wichita", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 244) {
            newDeath <- filter(death, grepl("Wilbarger", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 245) {
            newDeath <- filter(death, grepl("Willacy", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 246) {
            newDeath <- filter(death, grepl("Williamson", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 247) {
            newDeath <- filter(death, grepl("Wilson", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 248) {
            newDeath <- filter(death, grepl("Winkler", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 249) {
            newDeath <- filter(death, grepl("Wise", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 250) {
            newDeath <- filter(death, grepl("Wood", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 251) {
            newDeath <- filter(death, grepl("Yoakum", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 252) {
            newDeath <- filter(death, grepl("Young", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 253) {
            newDeath <- filter(death, grepl("Zapata", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }else if(input$county == 254) {
            newDeath <- filter(death, grepl("Zavala", County))
            ggplot(newDeath, aes(x=Date,y=Deaths,fill=Deaths)) + 
                geom_bar(stat="identity") + xlab('Year')  + ylab('Influenza Related Deaths') + 
                scale_x_continuous(breaks=c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012))
            
        }
        else {
            print    
        }
    })
 
    

# Data Table --------------------------------------------------------------
output$data= renderDataTable({
    datatable(death)
})

})

    