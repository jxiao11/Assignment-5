#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)



mydata <- read.csv("All_year3.csv")
hourlydata <-read.csv("All_year_hourly.csv")
dataset1 <- read.csv("dataset1.csv")
dataset2 <- read.csv("dataset2.csv")
dataset3 <- read.csv("dataset3.csv")
dataset4 <- read.csv("dataset4.csv")
dataset1 <- select(dataset1,-X)
dataset2 <- select(dataset2,-X)
dataset3 <- select(dataset3,-X)
dataset4 <- select(dataset4,-X)
datafiles <- list(dataset1, dataset2,dataset3,dataset4)

hourlydata <- select(hourlydata, -X)
hourlydata$Date <- as.Date(hourlydata$Date)

mydata <- select(mydata,-X)
mydata$Date <- as.Date(mydata$Date)
#mydata <- read.csv("All_year10.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Assignment5"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Buoy 46035 Analysis",tabName = "dashboard"),
      menuItem("Veg toxicity Analysis",tabName = "Udochi")
    )
  ),
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("plot1",height = 250)),
                box("Choose a time range",dateRangeInput("date","Date", start = "1985-09-14", end ="2017-12-31")),
                box(HTML('<p><img src="http://www.ndbc.noaa.gov/images/stations/3mfoam_mini.jpg"/></p>',"BuoyStation 46035")),
                box(plotOutput("plot3",height = 250)),
                box("T test: Mean Air Temperature change over the past 30 years",br(),
                    "data:  Airtmp1986 and Airtmp2017",br(),
                    "t = -3.9948, df = 725, p-value = 7.137e-05",br(),
                    "alternative hypothesis: true difference in means is not equal to 0",br(),
                    "95 percent confidence interval:",br(),
                    "-1.7177222 -0.5856986",br(),
                    "sample estimates:",br(),
                    "mean of Airtmp1985 mean of Airtmp2017",br(), 
                    "3.470879  4.622590"),
                box("T test: Mean Sea Temperature change over the past 30 years",br(),
                    "data:  Sea_tmp1986 and Sea_tmp2017",br(),
                    "t = -4.2139, df = 647.56, p-value = 2.866e-05",br(),
                    "alternative hypothesis: true difference in means is not equal to 0",br(),
                    "95 percent confidence interval:",br(),
                    "-1.2091689 -0.4404648",br(),
                    "sample estimates:",br(),
                    "mean of Seatmp1986 mean of y",br(),
                    "5.308516  6.133333" ),
                box("The effect of global warming",br(),"*More frequent and severe wealther",
                    br(),"*Higher sea level. By estimation, Our sea level will be one to four feet higher by 2010",
                    br(),"*More acidic Oceans,it poses a serious threat to underwater life",
                    br(),"*Higher death rates,especially children,the elderly,low-income communites...")
                
                
                
              )
      ),
      tabItem(tabName = "Udochi",
              fluidRow(
                box(plotOutput("plot2",height=250)),
                box(selectInput("dataset","Choose Dataset",choice = c("2016"="1","2014"="2","2010"="3","2006"="4"))),
                box(HTML('<p><img src="http://www.dole.com/~/media/Mastheads/Fresh_Vegetables.jpg" width="300" height ="200"/></p>')),
                box("Conclusion:",br(),"Based on our exploratory analysis and visualization of the data we found that during the years of 2006, 2010, 2014, and 2016 for majority of the restricted use chemicals being applied to broccoli and cauliflower, the amount applied was higher than the level at which it becomes toxic. In our plots we have illustrated which chemicals were applied in safe amounts versus those that were unsafe. Clearly, this is not good for those of us who consume these vegetables regularly.")
              )
              
      )
    )
  )
)





server <- function(input, output) {
  
  
  output$plot1 <- renderPlot({
    min <- as.character(input$date[1])
    max <- as.character(input$date[2])
    sub_data <- filter(mydata, as.Date(mydata$Date) >= min & as.Date(mydata$Date) <= max)
    ggplot(data = mydata, aes(Date, Celsius, color = Temperature),na.rm = T) +geom_line()+ ggtitle("Southern Bering Sea Air and Water Temperature Daily over past 30 years")
    
    
  })
  output$plot3 <- renderPlot({
    
    
    ggplot(data = hourlydata, aes(Date, Temperature, color = Type),na.rm = T) +geom_line()+ ggtitle("Southern Bering Sea Air and Water Temperature hourly over past 30 years")
    
    
    
  })
  
  output$plot2 <-renderPlot({
    temp <- datafiles[[as.numeric(input$dataset)]]
    ggplot(temp,aes(ValueVeg,Name,color =safe1))+
      geom_point()+
      facet_wrap(~Commodity)
  })
}

shinyApp(ui, server)

