#Read in data from URL
library(RCurl)
library(shiny)
library(lubridate)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(rsconnect)
library(shinyalert)
library(stringr)

utility <- read.csv(file = 'litterati challenge-65.csv')

#Filter out the data (no map tags from anywhere not in America)
utility <- utility[utility$lat > 41,]
#Filter out rows where map point is not in Chicagoland Area
utility <- utility[utility$lon < -71,]

#Remove ones in the actual city
utility <- utility[utility$lon < -87.80,]

#Remove two from Riverside
utility <- utility[utility$lat >  41.83,]

#Remove four from Maywood
utility <- utility[utility$lon >  -87.83,]

#Convert the timestamps of the littering data into "year, month, day, hour, minute, and seconds
newDates <- ymd_hms(utility$litterTimestamp)

#Add new column in the data
utility$newDate <- newDates
#Convert to CST
attr(utility$newDate, "tzone") <- "America/Chicago"
#Convert tags from Factor to character
newTags <- as.character(utility$tags)
utility$newTags <- newTags
utility$newTags <- strsplit(utility$newTags, ",")



newTags <- as.character(utility$tags)
#Change blank values with "untagged"
utility$newTags[utility$newTags == ""] <- "untagged"
unlistTags <- unlist(utility$newTags)
Alltags <- as.data.frame(table(unlistTags))
topTenTags <- Alltags[order(-Alltags$Freq),]
topTenTags <- head(topTenTags, n = 10)

table(topTenTags1)

#Calculate how many total tags there are in the utility table
itemsofLitter <- length(utility$newTags)

#Plotting by hour
hoursOfTheDay <- format(utility$newDate, "%H", tz = "America/Chicago" )
utility$hoursOfTheDay <- hoursOfTheDay

#Plotting by day of the week 
daysOfTheWeek <- weekdays(utility$newDate)
utility$daysOfTheWeek <- daysOfTheWeek

# Change all the usernames to characters 
# change the ones with "literati" in front of it to be just the number
utility$username <- as.character(utility$username)
utility$username[utility$username == "litterati-115453"] <- "115453"
utility$username[utility$username == "litterati-117766"] <- "117766"
utility$username[utility$username == "litterati-119389"] <- "119389"
utility$username[utility$username == "litterati-126822"] <- "126822"
utility$username[utility$username == "litterati-127490"] <- "127490"
utility$username[utility$username == "litterati-57379"] <- "57379"
utility$username[utility$username == "litterati-64263"] <- "64263"
utility$username[utility$username == "litterati-73940"] <- "73940"

# Generating a list of usernames and then sort them into decreasing order
extractNames <- subset(utility, select = "username")
extractedNames <- as.data.frame(sort(table(extractNames), decreasing = TRUE))

#Create a table of the Top 10 names and their frequency
topTen <- head(extractedNames, n = 10)
#Changing column names
names(topTen) <- c("userName", "LitterPicked")
topTen$userName <- as.character(topTen$userName)


#Plotting by day
individualDays <- date(utility$newDate)
utility$individualDays <- individualDays

dateTable <- subset(utility, select = "individualDays")
dateTable <- as.data.frame(dateTable)
dateTable <- table(dateTable)

utility$dayOfTheWeek <- daysOfTheWeek

dayTable <- subset(utility, select = "dayOfTheWeek")
dayTable <- as.data.frame(dayTable)
dayTable <- table(dayTable)

utility$hourOfTheDay <- hoursOfTheDay

hourTable <- subset(utility, select = "hourOfTheDay")
hourTable <- as.data.frame(hourTable)
hourTable <- table(hourTable)


ui <- dashboardPage(
  
  # Create dashboard header + Sidebar menu + Body of dashboard
  
  dashboardHeader(title = "CS 424 Project 1"),
  
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
  sidebarMenu(actionLink("AboutID", "About"),
  selectInput("userNameInput", "Select a username from the Top Ten", c("Default", topTen$userName)),  
  selectInput("TagInput", "Select a tag from the Top Ten", choices = topTenTags$unlistTags, selected = NULL, multiple = FALSE), width = 6),
  useShinyalert()),
  
  
  dashboardBody(
fluidRow(
    column(2, 
                    
    fluidRow(box(title = "Top 10 pickers by username", width = 14, tableOutput("usernames"))), 
    fluidRow(box(title = "Top Ten tags", width = 12, tableOutput("tagTable"))), 
    fluidRow(box(title = "Per Hour", width = 12, tableOutput("dateTable")))),
    
   column(4, 
                    fluidRow(box(title = "Litter per day", width = 12, plotOutput("days")),
                    fluidRow(box(title = "Top 10 tags of litter", width = 12, plotOutput("tagNames", height = 200))), 
                    fluidRow(box(title = "Per day of the week", width = 12, tableOutput("dayTable"))),
                    fluidRow(infoBox("Total amount of Litter", value = length(utility$newTags), width = 12, fill = TRUE)) 
                    )),
    
    column(6, 
    fluidRow(box(title = "Litter picked up by hour", width = 12, plotOutput("hours", height = 200))), 
    fluidRow(box(title = "Map of Chicago", width = 12, leafletOutput("MapofChicago", height = 200)),
    fluidRow(box(title = "Litter picked up by Weekday", width = 12, plotOutput("week")))
    ))
  )))
server <- function(input, output){
  
  # Use reactive function to grab subset of data to use when user selects a username or tag to look at
  
   userData <- reactive({subset(utility, utility$username == input$userNameInput | utility$newTags == input$TagInput)})

    
  observeEvent(input$AboutID, {
    shinyalert("Who created this: Vijay Vemu\n
               Libraries used: shiny, shinydashboard, shinyalert, RCurl, lubridate, ggplot2, stringr, leaflet, rsconnect\n
               Where the data is from: Literrati litter data")
  })
  
  output$About <- renderMenu(menuItem("About", title = "About"))
  
  output$MapofChicago <- renderLeaflet({ 
    leaflet(data = userData(), width = 100) %>% addTiles() %>%
                                         addMarkers(clusterOptions = markerClusterOptions(),~lon, ~lat)})

  output$caption <- renderText(itemsofLitter)
  
  output$dateTable <- renderTable({
    userName <- userData()
   table(userName$hourOfTheDay)}
    )
    
    
  output$dayTable <- renderTable({    userData <- userData()
                                    table(userData$dayOfTheWeek)})
  output$tagTable <-renderTable(topTenTags)
  
  output$week<- renderPlot({
    userName <- userData()
   ggplot(userName, aes(x = userName$daysOfTheWeek, y = userName$litterId)) + geom_bar(stat = "identity", fill = "navy") + labs(x = "Day of the Week", y = "Amount of Litter")})

  output$tagNames <- renderPlot(
    {
      ggplot(data = topTenTags, aes(x = topTenTags$unlistTags, y = topTenTags$Freq)) + geom_bar(stat = "identity", fill= "orange") + labs(x = "Tag", y = "Amount of Litter")})
  
  output$usernames <- renderTable(topTen)
  
  output$days <- renderPlot({
    userName <- userData()
    ggplot(data = userName, aes(x = userName$individualDays, y = userName$litterId)) + geom_bar(stat = "identity") + labs(x = "Day", y = "Amount of Litter")})
  
  output$hours <- renderPlot(
    {
      userName <- userData() 
      ggplot(userName, aes(x = userName$hoursOfTheDay, y = userName$litterId)) + geom_bar(stat = "identity", fill = "purple") + labs(x = "Hour of the Day", y = "Amount of Litter")})
  }
shinyApp(ui = ui, server = server)
#rsconnect::setAccountInfo(name='vijayvemucs424', token='2E7AE4174C3BBDECBA6FFFB6B7D1111D', secret='0Sg8PHFt0IOXy6M2WmsHUmgkeHVqQOPQIAcPYpGO')
#rsconnect::deployApp('/Users/vijayvemu/Documents/College Work/Senior Year/CS 424/Project1')