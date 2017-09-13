d <- read.csv("Timesdata-tophundrd.csv")
x <- read.csv("Timesdata-USAOnlyLatLong.csv")

library(shiny);
library(shinydashboard);
library(devtools);
library(ggvis);
library(ggplot2);
library(dplyr);
library(randomcoloR);
library(ggmap);
library(shinydashboard);
library(scatterplot3d);
library(corrgram);
library(googleCharts);

xlim <- list(
  min = 0,
  max = 100
)
ylim <- list(
  min = 10,
  max = 120
)





ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "World University Rankings", titleWidth = 500), 
  dashboardSidebar(width = 500,
    sidebarMenu(
      menuItem("ViewData", tabName = "Data", icon = icon("th")),
      menuItem("Correlation Analysis", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Google Charts", tabName = "gplot", icon = icon("th")),
      menuItem("USA Top Colleges in Map", tabName = "topUsa", icon = icon("th")),
      menuItem("k-means Clusters", tabName = "kcluster", icon = icon("th")),
    
      mainPanel(
        img(src='escudos.jpg', align = "left", width = "485px", height = "400px")
      )
    )
  ),
  dashboardBody(
    tabItems(
      #Dataset
      tabItem(tabName = "Data",
              fluidPage(
                tags$head(tags$style(HTML('
               .main-header .logo {
                                          font-family: "Georgia", Times, "Times New Roman", serif;
                                          font-weight: bold;
                                          font-size: 24px;
                                          }
                                          '))),
                headerPanel("TimesData of universities"),
                dataTableOutput('tbl'))
      ),
      
      # First tab content
      tabItem(tabName = "dashboard",
              fluidPage(
                pageWithSidebar(
                  headerPanel("Correlation Analysis and 3D Plot"),
                  sidebarPanel(
                    selectInput(inputId ="Model",
                                label = "Select Appropriate Model",
                                choices= c("scatterplot3d","corrgram with correlation values","corrgram with Ellipse","corrgram with Shade","corrgram with Pie Chart"),
                                selected = "corrgram with Ellipse"),
                    
                    
                    conditionalPanel( condition = "input.Model == TRUE ",
                                      selectInput(inputId = "yearc",
                                                  label = "Select appropriate year",
                                                  choices = c(2011,2012,2013, 2014,2015,2016),selected = 2011)
                    )),
                  mainPanel(
                    plotOutput(outputId = "WorldRankvsScore_plot")
                  ))
              )),
      
      #Gplot
      tabItem(tabName = "gplot",
              fluidPage(
                googleChartsInit(),
                
                tags$link(
                  href=paste0("http://fonts.googleapis.com/css?",
                              "family=Source+Sans+Pro:300,600,300italic"),
                  rel="stylesheet", type="text/css"),
                tags$style(type="text/css",
                           "body {font-family: 'Source Sans Pro'}"
                ),
                
                h2("Google Charts"),
                
                googleBubbleChart("chart",
                                  width="100%", height = "600px",
                                  options = list(
                                    fontName = "Source Sans Pro",
                                    fontSize = 16,
                                    hAxis = list(
                                      title = " international_students ",
                                      viewWindow = xlim
                                    ),
                                    vAxis = list(
                                      title = "income",
                                      viewWindow = ylim
                                    ),
                                    chartArea = list(
                                      top = 50, left = 75,
                                      height = "75%", width = "75%"
                                    ),
                                    explorer = list(),
                                    bubble = list(
                                      opacity = 0.4, stroke = "none",
                                      textStyle = list(
                                        color = "none"
                                      )
                                    ),
                                    titleTextStyle = list(
                                      fontSize = 16
                                    ),
                                    tooltip = list(
                                      textStyle = list(
                                        fontSize = 13
                                      )
                                    )
                                  )
                ),
                fluidRow(
                  shiny::column( 10, offset = 5,
                                 box(
                                   title = "Select Year",
                                   sliderInput(inputId = "yearm", label = "Year",
                                               min = 2011, max = 2016,
                                               value = 2014, animate = TRUE)
                                 )
                  )
                )
              )),
      
      
      # Google Map
      tabItem(tabName = "topUsa",
              fluidPage(
                # Application title
                pageWithSidebar(
                  headerPanel("USA University Data"),
                  sidebarPanel(  
                    
                    # Sidebar with a slider input for number of bins
                    
                    selectInput(inputId = "mapType",label = h3("Type of Map"),choices = c("terrain","hybrid","toner","watercolor"), selected = "hybrid"),
                    # sliderInput(inputId = "year", label = "Year",
                    #      min = 2011, max = 2016,
                    #     value = 2014),
                    
                    
                    selectInput(inputId = "yearw",label = h3("Year"),choices = c(2011,2012,2013,2014,2015,2016), selected = 2011)),
                  # Show a plot of the generated distribution
                  mainPanel(
                    plotOutput(outputId = "main_plot")
                  )
                ))),      
      
      #kcluster
      tabItem(tabName = "kcluster",
              fluidPage(
                pageWithSidebar(
                  headerPanel('k-means clustering'),
                  sidebarPanel(
                    selectInput('xcol', 'X Variable', choices = c("world_rank", "teaching", "research", "citations", "income",
                                                                  "student_staff_ratio", "total_score")),
                    selectInput('ycol', 'Y Variable', choices = c("world_rank", "teaching", "research", "citations", "income",
                                                                  "student_staff_ratio", "total_score")),
                    selected=names(d)[[2]]),
                  numericInput('clusters', 'Cluster count', 3,
                               min = 1, max = 9)
                ),
                mainPanel(
                  plotOutput('plot1')
                )
              ))
      
    )
  )
)