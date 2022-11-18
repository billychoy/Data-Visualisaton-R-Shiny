
options(scipen = 999)
library('tidyverse')
library('stringr')
library('shinydashboard')
library('plotly')
library('shiny')
library('shinyjs')
library('DT')
library('dplyr')
library('markdown')
library('tidyr')
library('ggplot2')
library('hrbrthemes')
library('leaflet')
library(dashboardthemes)
library(lubridate)
library(kableExtra)
#Import dataset - ignore empty columns (ORI, url and source)
# split the department name into city and state for map
crime <- read_csv("Gun_V2.csv") 
crime$`Incident Date`<-as.Date(with(crime, paste(Year,Month,Day,sep="-")), "%Y-%m-%d")

#crime<-crime[,3:17]
colnames(crime)
# Basics

ui <- dashboardPage( 
                     #Application title
                     dashboardHeader(title = "Gun Crime 2018 - 2021 Dashboard",titleWidth = 400),
                     
                     # dashboard sidebar functions will be inserted here
                     dashboardSidebar(
                       sidebarMenu(
                         menuItem("Intro",tabName = "intro",icon = icon("dashboard")),
                         menuItem("Main",tabName = "main",icon = icon("dashboard")),
                         menuItem("Map",tabName = "map",icon = icon("map"))
                       ),
                       sliderInput("yearInput",
                                   label = "Year Range",
                                   min =  2018,
                                   max = 2021,
                                   step = 1,
                                   value = c(2018,2021),
                                   sep = ""),
                       sliderInput("monthInput",
                                   label = "Month Range",
                                   min =  1,
                                   max = 12,
                                   step = 1,
                                   value = c('Jan','Dec'),
                                   sep = ""),
                       uiOutput("typeSelectOutput"),
                       radioButtons("parameter",
                                    label = "Select Crime:",
                                    choices = c("Mass_Murder",
                                                "Mass_Shooting",
                                                "Other",
                                                "Defensive",
                                                "Murder_Suicide",
                                                "Unintentional",
                                                "Teens",
                                                "Officers_Involved",
                                                "Children",
                                                "Subject",
                                                "Home_Invasion", "Intro plot"),
                                    selected = "Intro plot")
                     ),
                     
                     # Dash board Area
                     dashboardBody( shinyDashboardThemes(
                       theme = "blue_gradient"
                     ),
                       tabItems(
                         tabItem(tabName = "intro",
                                 fluidRow(includeMarkdown("include1.md"),br(),
                                          plotOutput("totalplot"),br(),
                                          infoBox("Max crime:",subtitle = "In a state", "Texas:4530",icon = icon("bullhorn"),fill = T , color = "aqua", width = 2),
                                          infoBox("Max crime:",subtitle = "In a city", "Chicago: 2298",icon = icon("bullhorn"),fill = T , color = "light-blue", width = 2),
                                          infoBox("Mass Murder:" ,subtitle = "In a state","Texas: 100",icon = icon("bullhorn"),fill = T , color = "light-blue", width = 2),
                                          infoBox("Suicide:",subtitle = "In a state", "Texas: 293",icon = icon("bullhorn"),fill = T , color = "blue", width = 2),
                                          infoBox("Mass Shooting:", "California: 400",subtitle = "In a state",icon = icon("bullhorn"),fill = T , color = "blue", width = 2),
                                          infoBox("Children:", "Texas: 134",subtitle = "In a state",icon = icon("bullhorn"),fill = T , color = "navy", width = 2),
                                          infoBox("Teenagers:", "Illinois: 576",subtitle = "In a state",icon = icon("bullhorn"),fill = T , color = "light-blue", width = 2),
                                          infoBox("Officers:", "Texas: 29",subtitle = "In a state",icon = icon("bullhorn"),fill = T , color = "light-blue", width = 2),
                                          infoBox("Total Crimes:", "36600",subtitle = "In all states",icon = icon("bullhorn"),fill = T , color = "blue", width = 2),
                                          infoBox("Total Incidents:", "49441",subtitle = "In all states",icon = icon("bullhorn"),fill = T , color = "blue", width = 2),
                                          infoBox("Killed:", "24053",subtitle = "In all states",icon = icon("bullhorn"),fill = T , color = "navy", width = 2),
                                          infoBox("Safest:", "Wyoming: 44",subtitle = "City",icon = icon("bullhorn"),fill = T , color = "navy", width = 2)
                                          )),
                         
                         tabItem(tabName = "main",
                                 fluidRow(includeMarkdown("include2.md"),br(),
                                   splitLayout(style = "border: 1px solid silver:",  
                                               #  plotOutput("thePlot")
                                               #,
                                               plotOutput("thePlot2")
                                   )),
                                 br(),
                                 DT::dataTableOutput("crimeTable")
                         ),
                         tabItem(tabName = "map",
                                 plotlyOutput("crimeMap"),
                                 verbatimTextOutput("click")
                         )
                       )
                     )
)

### Set up server

server <- function(input, output) {
  set.seed(100)
  histdata <- rnorm(500)
  
  #########  Dashboard Page -- Start --        #################  
  # List box - city selection
  #Intro matterial
 #library(dplyr)
  ##crime %>% 
    #select(State,Total_Incidents,CityOrCounty) %>% 
    #group_by(CityOrCounty) %>% 
    #group_by(State) %>% 
  #   summarise(x=sum(Total_Incidents)) %>% 
  #   arrange(desc(x)) %>%
  #   top_n(5)
  # sum(crime$Total_Incidents)
  # crime %>% 
  #   select(State,Killed,CityOrCounty, Dataset) %>% 
  #   #filter(Dataset == "Officers_Involved") %>% 
  #   #group_by(CityOrCounty) %>% 
  #   #group_by(State) %>% 
  #   summarise(x=sum(Killed)) %>% 
  #   arrange(desc(x)) %>%
  #   top_n(5)
  # sum(crime$Killed)
  #output$Dangerous<- renderInfoBox({
    #infoBox("Count of",value=4530,icon(icon="list"),color = "black")})
  

  #######################
  output$typeSelectOutput <- renderUI({
    selectInput("typeInput","Select state from list:",
                sort(unique(crime$State)),
                multiple = TRUE,selected='California', selectize = FALSE)
  })
  
  # Create data_React dataset
  data_reactive <- reactive({
    data_React <- crime %>% 
 #     filter(between(date, input$date[1], input$date[2])) %>%  
      filter(Year >= input$yearInput[1],Year <= input$yearInput[2]) %>% 
      filter(Month >= input$monthInput[1],Month <= input$monthInput[2])  %>% 
      filter(State %in% input$typeInput)
    if(nrow(data_React)==0){
      return(NULL)
    }
    data_React 
  })
  ##########################
  output$totalplot <- renderPlot({
    crime %>% 
      #     filter(between(date, input$date[1], input$date[2])) %>%  
      filter(Year >= input$yearInput[1],Year <= input$yearInput[2]) %>% 
      filter(Month >= input$monthInput[1],Month <= input$monthInput[2])  %>% 
      filter(State %in% input$typeInput) %>%
      group_by(State, Year, Month)%>%
      summarise(year.month = sort(unique(str_c(Year, '.', Month))),
                total_killed = sum(Killed,na.rm = T),
                total_injured = sum(Injured,na.rm = T),
                total_incidents = sum(Total_Incidents,na.rm = T),date2=`Incident Date`) %>%
      ggplot(mapping=aes(x=date2,y=total_incidents, color=State)) +
      geom_smooth(se=FALSE) +
      geom_point(alpha=0)+theme_modern_rc()
    
      })
  ###########################
  # Barplot on State and Year
  output$thePlot2 <- renderPlot({
    crime %>% 
      #     filter(between(date, input$date[1], input$date[2])) %>%  
      filter(Year >= input$yearInput[1],Year <= input$yearInput[2]) %>% 
      filter(Month >= input$monthInput[1],Month <= input$monthInput[2])  %>% 
      filter(State %in% input$typeInput) %>%
      filter(Dataset == input$parameter) %>% 
      group_by(State, Year, Month)%>%
      summarise(year.month = sort(unique(str_c(Year, '.', Month))),
                total_killed = sum(Killed,na.rm = T),
                total_injured = sum(Injured,na.rm = T),
                total_incidents = sum(Total_Incidents,na.rm = T),date2=`Incident Date`) %>%
      ggplot() +
      geom_col(mapping=aes(x=year.month,y=total_incidents, fill=State))+theme_modern_rc()
      #ggplot(aes(x=date2,y=total_incidents, fill=State), ylim(0, 50)) + 
      #geom_col(position="stack", stat="identity")+ theme_modern_rc()+scale_x_discrete(guide = guide_axis(angle = 30))
    
#      ggplot(mapping=aes(x=year.month, y=total_incidents,
#                         ylim=c(1,50), olor=State))
#    + 
#     ggtitle(title = paste("Displaying ",  input$parameter, " in ", input$typeInput,
#                            "between", input$yearInput[1] ,".", input$monthInput[1],
#                            "and", input$yearInput[2] ,".", input$monthInput[2]
#                            ))
   # + 

    }
    )
  
  # Outoput dataset result
  # Create dynamic dataset by city and input year 1 and 2
  outputList <- reactive({
    data_reactive() %>% 
      filter(State %in% input$typeInput)%>%  
      filter(Year >= input$yearInput[1],Year <= input$yearInput[2]) %>% 
      filter(Month >= input$monthInput[1],Month <= input$monthInput[2]) %>% 
      group_by(State) %>% 
      filter(Dataset == input$parameter) %>% 
      summarise(Crime = input$parameter,
                Killed = sum(Killed,na.rm = T),
                Injured = sum(Injured,na.rm = T),
                Total = sum(Total_Incidents,na.rm = T),
                )
  })
  
  output$crimeTable <- DT::renderDataTable(
    datatable(outputList(),
    options = list(pageLength = 8), rownames = FALSE
  ) )
                   
  
  
  
  #########  Dashboard Page --  End  --        #################  bebin
 
  ##########bebin
  # Plots the map of US
  output$crimeMap <- renderPlotly({
    plot_data = crime %>% 
      filter(Year == input$yearInput[1])%>%
      group_by(State_Code, State) %>% 
      summarise(total_killed = sum(Killed,na.rm = T),
                total_injured = sum(Injured,na.rm = T),
                total_incidents = sum(Total_Incidents,na.rm = T))
    
    plot_data$click  = with(plot_data,
                            paste(State, "(", State_Code, ")",'<br>',
                                  "Total Killed:",total_killed,'<br>',
                                  "Total Injured:",total_injured,'<br>',
                                  "Total Incidents:",total_incidents,'<br>')
                            )
    
    #link - https://plotly-r.com/maps.html
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      lakecolor = toRGB('white')
    )
    
    fig <- plot_ly()
    fig <- fig %>% add_trace(
      z = plot_data$total_incidents, 
      locations = plot_data$State_Code,
      text = plot_data$click,
      type = 'choropleth',
      colorscale="Viridis",
      locationmode = 'USA-states')
    fig <- fig %>% colorbar(title = "Crime Rate")%>%
      layout(geo = g,
             title = paste0("Map displaying Crime Rate in US in the Year: ",
                            input$yearInput[1]))
  })
}

shinyApp(ui, server)
#?shinyDashboardThemes
