#Read in data from URL
library(RCurl)
library(shiny)
library(lubridate)
library(shinydashboard)
library(ggplot2)
library(leaflet)

utility <- read.csv("https://www.evl.uic.edu/aej/424/litterati%20challenge-65.csv",
                    header = TRUE)

#Convert the timestamps of the littering data into "year, month, day, hour, minute, and seconds
newDates <- ymd_hms(utility$litterTimestamp)

#Add new column in the data
utility$newDate <- newDates
#Convert to CST
utility$newDate <- force_tz(time = utility$newDate, tzone = "America/Chicago")

#Convert tags from Factor to character
newTags <- as.character(utility$tags)
utility$newTags <- newTags

#Change blank values with "untagged"
utility$newTags[utility$newTags == ""] <- "untagged"

library(shiny)
ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Project 1"),
  dashboardSidebar(),
  dashboardBody())
server <- function(input, output){}
shinyApp(ui = ui, server = server)