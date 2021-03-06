library(shiny)
library(shinythemes)
library(plyr)
library(dplyr)
library(ggplot2)
library(DT)
library(BH)

library(maptools)
library(rgdal)
library(tidyr)



shinyUI(fluidPage(theme=shinytheme('cerulean'),
    headerPanel("Influenza Related Deaths in Texas Counties (1999-2012)"),
    sidebarLayout(
#         sidebarPanel(h3("Select County"), position="left",
#                      selectInput("county", label = h5("Select a County"), 
#                                  choices = list("Anderson County"=1,"Andrews County"=2,"Angelina County"=3,"Aransas County"=4,"Archer County"=5,
#                                                 "Armstrong County"=6,"Atascosa County"=7,"Austin County"=8,"Bailey County"=9,"Bandera County"=10,
#                                                 "Bastrop County"=11,"Baylor County"=12,"Bee County"=13,"Bell County"=14,"Bexar County"=15,"Blanco County"=16,
#                                                 "Borden County"=17,"Bosque County"=18,"Bowie County"=19,"Brazoria County"=20,"Brazos County"=21,"Brewster County"=22,
#                                                 "Briscoe County"=23,"Brooks County"=24,"Brown County"=25,"Burleson County"=26,"Burnet County"=27,"Caldwell County"=28,
#                                                 "Calhoun County"=29,"Callahan County"=30,"Cameron County"=31,"Camp County"=32,"Carson County"=33,"Cass County"=34,
#                                                 "Castro County"=35,"Chambers County"=36,"Cherokee County"=37,"Childress County"=38,"Clay County"=39,"Cochran County"=40,
#                                                 "Coke County"=41,"Coleman County"=42,"Collin County"=43,"Collingsworth County"=44,"Colorado County"=45,"Comal County"=46,
#                                                 "Comanche County"=47,"Concho County"=48,"Cooke County"=49,"Coryell County"=50,"Cottle County"=51,"Crane County"=52,"Crockett County"=53,
#                                                 "Crosby County"=54,"Culberson County"=55,"Dallam County"=56,"Dallas County"=57,"Dawson County"=58,"Deaf Smith County"=59,"Delta County"=60,
#                                                 "Denton County"=61,"DeWitt County"=62,"Dickens County"=63,"Dimmit County"=64,"Donley County"=65,"Duval County"=66,"Eastland County"=67,
#                                                 "Ector County"=68,"Edwards County"=69,"El Paso County"=70,"Ellis County"=71,"Erath County"=72,"Falls County"=73,"Fannin County"=74,
#                                                 "Fayette County"=75,"Fisher County"=76,"Floyd County"=77,"Foard County"=78,"Fort Bend County"=79,"Franklin County"=80,"Freestone County"=81,
#                                                 "Frio County"=82,"Gaines County"=83,"Galveston County"=84,"Garza County"=85,"Gillespie County"=86,"Glasscock County"=87,"Goliad County"=88,
#                                                 "Gonzales County"=89,"Gray County"=90,"Grayson County"=91,"Gregg County"=92,"Grimes County"=93,"Guadalupe County"=94,"Hale County"=95,
#                                                 "Hall County"=96,"Hamilton County"=97,"Hansford County"=98,"Hardeman County"=99,"Hardin County"=100,"Harris County"=101,"Harrison County"=102,
#                                                 "Hartley County"=103,"Haskell County"=104,"Hays County"=105,"Hemphill County"=106,"Henderson County"=107,"Hidalgo County"=108,"Hill County"=109,
#                                                 "Hockley County"=110,"Hood County"=111,"Hopkins County"=112,"Houston County"=113,"Howard County"=114,"Hudspeth County"=115,"Hunt County"=116,
#                                                 "Hutchinson County"=117,"Irion County"=118,"Jack County"=119,"Jackson County"=120,"Jasper County"=121,"Jeff Davis County"=122,"Jefferson County"=123,
#                                                 "Jim Hogg County"=124,"Jim Wells County"=125,"Johnson County"=126,"Jones County"=127,"Karnes County"=128,"Kaufman County"=129,"Kendall County"=130,
#                                                 "Kenedy County"=131,"Kent County"=132,"Kerr County"=133,"Kimble County"=134,"King County"=135,"Kinney County"=136,"Kleberg County"=137,"Knox County"=138,
#                                                 "La Salle County"=139,"Lamar County"=140,"Lamb County"=141,"Lampasas County"=142,"Lavaca County"=143,"Lee County"=144,"Leon County"=145,"Liberty County"=146,
#                                                 "Limestone County"=147,"Lipscomb County"=148,"Live Oak County"=149,"Llano County"=150,"Loving County"=151,"Lubbock County"=152,"Lynn County"=153,
#                                                 "Madison County"=154,"Marion County"=155,"Martin County"=156,"Mason County"=157,"Matagorda County"=158,"Maverick County"=159,"McCulloch County"=160,
#                                                 "McLennan County"=161,"McMullen County"=162,"Medina County"=163,"Menard County"=164,"Midland County"=165,"Milam County"=166,"Mills County"=167,
#                                                 "Mitchell County"=168,"Montague County"=169,"Montgomery County"=170,"Moore County"=171,"Morris County"=172,"Motley County"=173,"Nacogdoches County"=174,
#                                                 "Navarro County"=175,"Newton County"=176,"Nolan County"=177,"Nueces County"=178,"Ochiltree County"=179,"Oldham County"=180,"Orange County"=181,"Palo Pinto County"=182,
#                                                 "Panola County"=183,"Parker County"=184,"Parmer County"=185,"Pecos County"=186,"Polk County"=187,"Potter County"=188,"Presidio County"=189,"Rains County"=190,
#                                                 "Randall County"=191,"Reagan County"=192,"Real County"=193,"Red River County"=194,"Reeves County"=195,"Refugio County"=196,"Roberts County"=197,
#                                                 "Robertson County"=198,"Rockwall County"=199,"Runnels County"=200,"Rusk County"=201,"Sabine County"=202,"San Augustine County"=203,"San Jacinto County"=204,
#                                                 "San Patricio County"=205,"San Saba County"=206,"Schleicher County"=207,"Scurry County"=208,"Shackelford County"=209,"Shelby County"=210,"Sherman County"=211,
#                                                 "Smith County"=212,"Somervell County"=213,"Starr County"=214,"Stephens County"=215,"Sterling County"=216,"Stonewall County"=217,"Sutton County"=218,"Swisher County"=219,
#                                                 "Tarrant County"=220,"Taylor County"=221,"Terrell County"=222,"Terry County"=223,"Throckmorton County"=224,"Titus County"=225,"Tom Green County"=226,"Travis County"=227,
#                                                 "Trinity County"=228,"Tyler County"=229,"Upshur County"=230,"Upton County"=231,"Uvalde County"=232,"Val Verde County"=233,"Van Zandt County"=234,"Victoria County"=235,
#                                                 "Walker County"=236,"Waller County"=237,"Ward County"=238,"Washington County"=239,"Webb County"=240,"Wharton County"=241,"Wheeler County"=242,"Wichita County"=243,
#                                                 "Wilbarger County"=244,"Willacy County"=245,"Williamson County"=246,"Wilson County"=247,"Winkler County"=248,"Wise County"=249,"Wood County"=250,"Yoakum County"=251,
#                                                 "Young County"=252,"Zapata County"=253,"Zavala County"=254), selected = 1)),
        
        sidebarPanel(h3("Select Year"), position="left",
                     selectInput("year", label = h5("Select a Year"), 
                                 choices = list("1999"=1,"2000"=2,"2001"=3,"2002"=4,"2003"=5,
                                                "2004"=6,"2005"=7,"2006"=8,"2007"=9,"2008"=10,
                                                "2009"=11,"2010"=12,"2011"=13, "2012"=14), selected = 1)),
        
    
    mainPanel(
    tabsetPanel(type="tabs",
#                 tabPanel("Plots",
#                          plotOutput("Plot"), 
#                          position="right"),
                tabPanel("About",
                         helpText("this is filler text")),
                tabPanel("Data Set",
                         dataTableOutput("data")),
                tabPanel("Maps",
                         plotOutput("Map"),
                         position="right")
    ))
)
))