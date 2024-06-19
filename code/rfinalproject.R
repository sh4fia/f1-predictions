# Read Csv --------------------------
constructor_standing <- read.csv('https://raw.githubusercontent.com/Rohan-patel23/Data_Analytics/main/Data/constructor_standings.csv')
driver_details <- read.csv('https://raw.githubusercontent.com/Rohan-patel23/Data_Analytics/main/Data/driver_details.csv')
driver_standings <- read.csv('https://raw.githubusercontent.com/Rohan-patel23/Data_Analytics/main/Data/driver_standings.csv')
fastest_laps <- read.csv('https://raw.githubusercontent.com/Rohan-patel23/Data_Analytics/main/Data/fastest_laps.csv')
fastestlaps_detailed <- read.csv('https://raw.githubusercontent.com/Rohan-patel23/Data_Analytics/main/Data/fastestlaps_detailed.csv')
pitstops <- read.csv('https://raw.githubusercontent.com/Rohan-patel23/Data_Analytics/main/Data/pitstops.csv')
practices <- read.csv('https://raw.githubusercontent.com/Rohan-patel23/Data_Analytics/main/Data/practices.csv')
qualifyings <- read.csv('https://raw.githubusercontent.com/Rohan-patel23/Data_Analytics/main/Data/qualifyings.csv')
race_details <- read.csv('https://raw.githubusercontent.com/Rohan-patel23/Data_Analytics/main/Data/race_details.csv')
race_summaries <- read.csv('https://raw.githubusercontent.com/Rohan-patel23/Data_Analytics/main/Data/race_summaries.csv')
starting_grids <- read.csv('https://raw.githubusercontent.com/Rohan-patel23/Data_Analytics/main/Data/starting_grids.csv')
team_details <- read.csv('https://raw.githubusercontent.com/Rohan-patel23/Data_Analytics/main/Data/team_details.csv')


# Data Transformations ----------------------
library(tidyverse) # --> already Installed library

# Since This package is not present in our libraries we had to install it externally
# install.packages("devtools") --> Commands to install the package
# devtools::install_github("tidyverse/lubridate")
library(lubridate) 


# 1. Converting Driver date values to date columns
head(driver_details['Date'],10)
head(dmy(driver_details$Date),10) # --> as we can see that date conversion is working now appending it to original dataset 
driver_details$Date <- dmy(driver_details$Date)
head(driver_details['Date'],10)

head(team_details['Date'],10)
head(dmy(team_details$Date),10) # --> as we can see that date conversion is working now appending it to original dataset 
team_details$Date <- dmy(team_details$Date)
head(team_details['Date'],10)

# 2 Extracting 1999 and 2000 data for analysis
# Team Data
c_1999 <- subset(constructor_standing, Year == 1999)
c_2000 <- subset(constructor_standing, Year == 2000)
c_2001 <- subset(constructor_standing, Year == 2001)
head(c_1999,10) # c--> Printing the data to check if the correct data is extracted
head(c_2000,10)


# Exploratory Data Analysis


# Question 1 


# The Below function helps in rotating the labels to 45˚ for clear visualization of labels
rotate_x <- function(data, column_to_plot, labels_vec, rot_angle,b,title) {
  plt <- barplot(data[[column_to_plot]], col='steelblue', xaxt="n",ylab=b,main=title)
  text(plt, par("usr")[3], labels = labels_vec, srt = rot_angle, adj = c(1.1,1.1), xpd = TRUE, cex=0.75) 
} # Ref = https://stackoverflow.com/questions/10286473/rotating-x-axis-labels-in-r-for-barplot

# 1 Plotting Bar chart of team & Drivers who scored most points in the season 1999 and 2000
rotate_x(c_1999, 'PTS', c_1999$Team, 45,"Points Scored","F1 Teams V/S Points 1999") # --> Team Bar Graph
rotate_x(c_2000, 'PTS', c_2000$Team, 45,"Points Scored","F1 Teams V/S Points 2000") 
rotate_x(c_2001, 'PTS', c_2001$Team, 45,"Points Scored","F1 Teams V/S Points 2001") 

library(dplyr)

group_data <- subset(race_details[,c("Car","PTS","Year")], Year > 1998 & Year < 2002)
summ <- group_data %>% group_by(Car,Year) %>% summarise(Avg_pts_scored_per_race = mean(PTS), .group='drop')
rotate_x(summ,'Avg_pts_scored_per_race',summ$Car,45,'Avg Points Scored Per Race','F1 Teams V/s Avg Points Scored Per Race')

# Analysis
#  Ferrari won the constructors championship for three consective years that is because there average points score
# per race is much higher than others 

# Question 2

# Plotting Drives V/s Total Wins Secured
driver_wins = subset(race_details[c("Driver","Pos")],Pos==1)
driver_wins$Pos = as.numeric(driver_wins$Pos)
driver_wins = driver_wins %>% group_by(Driver) %>% summarise(Wins_Secured = sum(Pos), .group='drop')
driver_wins = driver_wins[order(-driver_wins$Wins_Secured),]
driver_wins = head(driver_wins,10)
rotate_x(driver_wins,'Wins_Secured',driver_wins$Driver,45,'Total Wins Secured','Drives V/s Total Wins Secured')

qual = subset(qualifyings,Pos == 1 | Pos == 2)
qual$Pos = 1
qual = qual %>% group_by(Driver) %>% summarise(Front_Grid_Secured = sum(Pos), .group='drop')
qual = qual[order(-qual$Front_Grid_Secured),]
qual = head(qual,10)
rotate_x(qual,'Front_Grid_Secured',qual$Driver,45,'Front Grid Secured','Drives V/s Front Grid Secured')




# Training the model

#install.packages("rpart.plot")
final_dataset = read.csv('https://raw.githubusercontent.com/sh4fia/f1-predictions/main/code/Train_data.csv')
library(rpart)
library(rpart.plot)
fit <- rpart(final_dataset$Champion~., data = final_dataset[c('Average_points_Scored','No_of_times_secured_front_grid','Total_no_of_fastest_Laps')], method = 'class')
rpart.plot(fit, extra = 106)

# Shiny dashboard
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "F1 prediction dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Points", tabName = "points", icon = icon("th")),
      menuItem("Avg points scored", tabName = "avgpoints", icon = icon("th")),
      menuItem("Drives", tabName = "drives", icon = icon("th")),
      menuItem("Model prediction", tabName = "prediction", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "points",
    fluidRow(
      box(plotOutput("plot1"), width = 100),
      box(plotOutput("plot2"), width = 100),
      box(plotOutput("plot3"), width = 100)
    )
    ),
    tabItem(tabName = "avgpoints",
            fluidRow(
              box(plotOutput("plot4"), width = 100)
            )
            ),
    tabItem(tabName = "drives",
            fluidRow(
              box(plotOutput("plot5"), width = 100),
              box(plotOutput("plot6"), width = 100)
            )),
    tabItem(tabName = "prediction",
            fluidRow(
              box(plotOutput("plot7"), width = 100)
            ))
  )
  )
)

server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    rotate_x(c_1999, 'PTS', c_1999$Team, 45,"Points Scored","F1 Teams V/S Points 1999")
  })
  
  output$plot2 <- renderPlot({
    rotate_x(c_2000, 'PTS', c_2000$Team, 45,"Points Scored","F1 Teams V/S Points 2000") 
  })
  
  output$plot3 <- renderPlot({
    rotate_x(c_2001, 'PTS', c_2001$Team, 45,"Points Scored","F1 Teams V/S Points 2001") 
  })
  
  output$plot4 <- renderPlot({
    rotate_x(summ,'Avg_pts_scored_per_race',summ$Car,45,'Avg Points Scored Per Race','F1 Teams V/s Avg Points Scored Per Race')
  })
  
  output$plot5 <- renderPlot({
    rotate_x(driver_wins,'Wins_Secured',driver_wins$Driver,45,'Total Wins Secured','Drives V/s Total Wins Secured')
  })
  
  output$plot6 <- renderPlot({
    rotate_x(qual,'Front_Grid_Secured',qual$Driver,45,'Front Grid Secured','Drives V/s Front Grid Secured')
  })
  
  output$plot7 <- renderPlot({
    fit <- rpart(final_dataset$Champion~., data = final_dataset[c('Average_points_Scored','No_of_times_secured_front_grid','Total_no_of_fastest_Laps')], method = 'class')
    rpart.plot(fit, extra = 106)
  })
  
  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })
}

shinyApp(ui, server)
