d <- read.csv("Timesdata-tophundrd.csv")
x <- read.csv("Timesdata-USAOnlyLatLong.csv")
library(shiny)
library(ggplot2)
server <- function(input, output) {  
  #Dataset
  output$tbl <- renderDataTable(
    d, options = list(lengthChange = FALSE)
  )
  
  #Gplot
  d$country <- as.factor(d$country)
  d$international_students <- as.numeric(sub("%","",d$international_students))
  defaultColors <- c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477",
                     "#66cc33", "#cc3366", "#4c33cc", "#33b3cc", "#cc9933", "#7094db", "#142851",
                     "#2d5bb7", "#99b2e5", "#0a1428", "#2077d0", "#ff0000", "#00ffff", "#ffd700",
                     "#ff6666", "#660066", "#cc0000", "#a0db8e", "#ffff66", "#c39797", "#ff4040","#cccccc", "#003366", "#f08080")
  series <- structure(
    lapply(defaultColors, function(color) { list(color=color) }
    )
  )
  
  yearData <- reactive({
    df <- subset(d, year == input$yearm, select = c(num_students, international_students, income, country, total_score))
  })
  
  output$chart <- reactive({
    list(
      data = googleDataTable(yearData()),
      options = list(
        title = sprintf(
          " international_students vs income, %s",
          input$yearm),
        series = series
      )
    )
  })
  
  #kcluster
  selectedData <- reactive({
    d[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  
  #Corrgram
  output$WorldRankvsScore_plot <- renderPlot({
    if(input$Model == "scatterplot3d"){
      d1 <- subset(d, year == input$yearc, select = c(world_rank, international_students, total_score))
      scatterplot3d(d1$world_rank,d1$international_students,d1$total_score,xlab = "University World Rank",ylab = "Number Of Stufdents", 
                    zlab = "Total Score", pch = 19,color="violet",type="h",lty.hplot = 3)
    }
    else if (input$Model == "corrgram with Pie Chart"){
      
      d1 <- subset(d, year == input$yearc,select = c(world_rank, teaching, research, citations, income,
                                                     num_students, student_staff_ratio, international_students, total_score))
      #corrgram(Main = "Correlogram of University World Rankings",upper.panel=panel.conf,lower.panel=panel.pie)
      
      corrgram(d1,order = TRUE, lower.panel = panel.pie, upper.panel = panel.pts, main="Correlogram of University world Rankings")
    }
    else if (input$Model == "corrgram with correlation values"){
      
      d1 <- subset(d, year == input$yearc,select = c(world_rank, teaching, research, citations, income,
                                                     num_students, student_staff_ratio, international_students, total_score))
      
      corrgram(d1,order = TRUE, lower.panel = panel.pie, upper.panel = panel.conf, main="Correlogram of University world Rankings")
      
      #corrgram(d, lower.panel = panel.pie, upper.panel = panel.pts, main="Correlogram of University world Rankings")
    }
    else if (input$Model == "corrgram with Ellipse"){
      
      d1 <- subset(d, year == input$yearc,select = c(world_rank, teaching, research, citations, income,
                                                     num_students, student_staff_ratio, international_students, total_score))
      
      corrgram(d1,order = TRUE, lower.panel = panel.pie, upper.panel = panel.ellipse, main="Correlogram of University world Rankings")
      
      #corrgram(d, lower.panel = panel.pie, upper.panel = panel.pts, main="Correlogram of University world Rankings")
    }
    else if (input$Model == "corrgram with Shade"){
      
      d1 <- subset(d, year == input$yearc,select = c(world_rank, teaching, research, citations, income,
                                                     num_students, student_staff_ratio, international_students, total_score))
      
      corrgram(d1,order = TRUE, lower.panel = panel.pie, upper.panel = panel.shade, main="Correlogram of University world Rankings")
      
      #corrgram(d, lower.panel = panel.pie, upper.panel = panel.pts, main="Correlogram of University world Rankings")
    }
    
  })
  
  
  
  #My Google Map
  output$main_plot <- renderPlot({
    #Timeline
    
    jinga <- subset(x,year == input$yearw)
    # creating a sample data.frame with your lat/lon points
    lon <- c(jinga$Latitude)
    lat <- c(jinga$Longitude)
    df <- as.data.frame(cbind(lon,lat))
    
    # getting the map
    mapJinga <- get_map(location = c(lon = mean(df$lon), lat = mean(df$lat)),zoom = 3,
                        maptype = input$mapType , scale = 2)
    
    # plotting the map with some points on it
    p <-  ggmap(mapJinga) +
      geom_point(data = df, aes(x = lon, y = lat, fill = "pink", alpha = 0.8),size = 5, shape = 21) +
      guides(fill=FALSE, alpha=FALSE, size=FALSE)
    p
    
  })
  
}
