library(shiny)
library(leaflet)

ui <- fluidPage(
  titlePanel("Super Complex Machine Learning AI Birthday Decision Maker 9000"),
  
  sidebarLayout(
    sidebarPanel(
      #help text
      helpText("Input your values and hit Go to make a decision!"),
  # Input: 
  sliderInput("travel", "How far do you feel like travelling?:",
              min = 1, max = 3,
              value = 1, step=1),
  
  # Input:
  sliderInput("energy", "How much energy can you afford to spend?:",
              min = 1, max = 3,
              value = 1, step = 1),
  
  # Input: 
  sliderInput("adventure", "How adventerous are you feeling?:",
              min = 1, max = 3,
              value = 1, step=1),
  
  # Input:
  radioButtons("weather", "Do you mind the weather? (i.e.,  do you mind getting wet?):",
               choices=c("No, there is never bad weather, only bad clothing", 'Yes, I mind, need to look seggsi for the afternoon')),
  
  # Input: 
  radioButtons("finish", "Where do you want to end up when we finish?:",
               choices=c("Campus", "Hampstead Heath", "I don't care")),
  
  
  #action button
  actionButton("go", "Go!", class =
                 "btn-success")
    ),
    
    mainPanel(
      textOutput("optiontitle"),
      textOutput("text1"),
      textOutput("text2"),
      textOutput("text3"),
      textOutput("text4"),
      textOutput("text5"),
      leafletOutput("mymap")
    )
  )
)


server <- function(input, output){
  observeEvent(input$go,{
    travel <- input$travel
    energy <- input$energy
    adventure <- input$adventure
    weather <- input$weather
    finish <- input$finish
    
    ## wet weather option ##
    if(weather =="Yes, I mind, need to look seggsi for the afternoon"){
      optiontitle <- "Option 3: Wet Weather Option"
      mytext1 <- "1. Meet at 10:30 at Spitalfields City Farm"
      mytext2 <- "2. Brick Lane + Brick Lane Bookshop"
      mytext3 <- "3. Barbican (because ik you like it a lot xD)"
      mytext4 <- "4. Pies @ Pieminster Leather Ln"
      mytext5 <- "5. The Post Building Rooftop"

      point1_lat_demo <- 51.522101183769436
      point1_long_demo <- -0.06744764091891219
      point2_lat_demo <- 51.521433781693666
      point2_long_demo <- -0.07111211519160646
      point3_lat_demo <-51.52010311831101
      point3_long_demo <- -0.09318355937196572
      point4_lat_demo <- 51.52181787576912
      point4_long_demo <- -0.11030531809904102
      point5_lat_demo <- 51.516838731212154
      point5_long_demo <- -0.12460845945644308
      
      df_demo <- data.frame(long=c(point1_long_demo, point2_long_demo,
                                   point3_long_demo, point4_long_demo, point5_long_demo), lat =
                              c(point1_lat_demo, point2_lat_demo,
                                point3_lat_demo, point4_lat_demo, point5_lat_demo), name = c("1. Start Here: Spitalfields City Farm",
                                                           "2. Brick Lane + Brick Lane Bookshop",
                                                           "3. Barbican",
                                                           "4. Pieminster Leather Lane",
                                                           "5. The Post Building Rooftop"))
      
      
      output$optiontitle <- renderText({
        toString(optiontitle)
      })
      output$text1 <- renderText({
        toString(mytext1)
      })
      output$text2 <- renderText({
        toString(mytext2)
      })
      output$text3 <- renderText({
        toString(mytext3)
      })
      output$text4 <- renderText({
        toString(mytext4)
      })
      output$text5 <- renderText({
        toString(mytext5)
      })
      
      output$mymap <- renderLeaflet({
        leaflet() %>% addTiles() %>%
          addMarkers(lng=as.numeric(df_demo$long),
                     lat=as.numeric(df_demo$lat), label=df_demo$name,
                     labelOptions = labelOptions(noHide = F)) %>%
          addPolylines(data = df_demo, lng = ~long, lat = ~lat)
      })
    
    } 
    ## cycling ##
    else if(adventure==3 && finish =="Campus" && weather =="No, there is never bad weather, only bad clothing"){
      optiontitle <- "Option 2: Face Your Fears grrrrr >:("
      mytext1 <- "1. Meet at Santander bikes outside Archaeology at 09:30"
      mytext2 <- "2a. Cycling Scenic Route: UCL> Temple > Embankment/Westminster > cross at Westminster bridge > south bank > millennium bridge > st Paul's cathedral > UCL"
      mytext3 <- "2b. Cycling Beginner Friendly Route:  UCL > regents park > primrose hill > Camden > UCL."
      mytext4 <- "3. Food - depends on where we end up"
      
      point1_lat_demo <-51.525017580067924
      point1_long_demo <- -0.13168847790982494
      
      df_demo <- data.frame(long=c(point1_long_demo), lat =
                              c(point1_lat_demo), name = c("1. Start Here: UCL Archaeology"))
      
      
      output$optiontitle <- renderText({
        toString(optiontitle)
      })
      output$text1 <- renderText({
        toString(mytext1)
      })
      output$text2 <- renderText({
        toString(mytext2)
      })
      output$text3 <- renderText({
        toString(mytext3)
      })
      output$text4 <- renderText({
        toString(mytext4)
      })
      
      output$mymap <- renderLeaflet({
        leaflet() %>% addTiles() %>%
          addMarkers(lng=as.numeric(df_demo$long),
                     lat=as.numeric(df_demo$lat), label=df_demo$name,
                     labelOptions = labelOptions(noHide = F)) %>%
          addPolylines(data = df_demo, lng = ~long, lat = ~lat)
      })
    
    ## Hampstead Heath End Point
    } else if(finish=="Hampstead Heath"){
      optiontitle <- "Option 1: Something Comforting :)"
      mytext1 <- "1. Meet at 09:30 at Parliament Hill Lido"
      mytext2 <- "2. Swimming!!"
      mytext3 <- "3. Food at. Polly's"
      mytext4 <- "4. Kenwood house "
      
      point1_lat_demo <-51.55637232098641
      point1_long_demo <- -0.15110095937060003
      point2_lat_demo <- 51.55573608135147
      point2_long_demo <- -0.16669033053456359
      point3_lat_demo <-51.57153490435442
      point3_long_demo <- -0.16742747790985205

      
      df_demo <- data.frame(long=c(point1_long_demo, point2_long_demo,
                                   point3_long_demo), lat =
                              c(point1_lat_demo, point2_lat_demo,
                                point3_lat_demo), name = c("1. Start Here: Parliament Hill Lido",
                                                                            "2. Food @ Polly's Cafe",
                                                           "3. Walk Through Hampstead Heath and Kenwood House"))
      
      
      output$optiontitle <- renderText({
        toString(optiontitle)
      })
      output$text1 <- renderText({
        toString(mytext1)
      })
      output$text2 <- renderText({
        toString(mytext2)
      })
      output$text3 <- renderText({
        toString(mytext3)
      })
      output$text4 <- renderText({
        toString(mytext4)
      })
      
      output$mymap <- renderLeaflet({
        leaflet() %>% addTiles() %>%
          addMarkers(lng=as.numeric(df_demo$long),
                     lat=as.numeric(df_demo$lat), label=df_demo$name,
                     labelOptions = labelOptions(noHide = F)) %>%
          addPolylines(data = df_demo, lng = ~long, lat = ~lat)
      })
    
      ## Hampstead Heath ##
    } else if (adventure ==3 && energy>=2 && finish!="Hampstead Heath"){
      optiontitle <- "Option 4: Try Something New!"
      mytext1 <- "1. Meet at 09:30 at London Fields Lido"
      mytext2 <- "2. Swimming!!"
      mytext3 <- "3. Food at. Finch Cafe"
      mytext4 <- "4. Long walk back to campus along the canal (1hr ish)"
      
      point1_lat_demo <-51.54242180517086
      point1_long_demo <- -0.06150461704325338
      point2_lat_demo <- 51.53989156690294
      point2_long_demo <- -0.05754207286302285
      point3_lat_demo <-51.53608270390507
      point3_long_demo <- -0.0654282811430612
      point4_lat_demo <- 51.5246602532804
      point4_long_demo <- -0.13400527822771696
      
      
      df_demo <- data.frame(long=c(point1_long_demo, point2_long_demo,
                                   point3_long_demo, point4_long_demo), lat =
                              c(point1_lat_demo, point2_lat_demo,
                                point3_lat_demo, point4_lat_demo), name = c("1. Start Here: London Fields Lido",
                                                           "2. Food @ Finch Cafe",
                                                           "3. Long Walk back to campus via Canals", 
                                                           "4. Finish at Campus"))
      
      
      output$optiontitle <- renderText({
        toString(optiontitle)
      })
      output$text1 <- renderText({
        toString(mytext1)
      })
      output$text2 <- renderText({
        toString(mytext2)
      })
      output$text3 <- renderText({
        toString(mytext3)
      })
      output$text4 <- renderText({
        toString(mytext4)
      })
      
      output$mymap <- renderLeaflet({
        leaflet() %>% addTiles() %>%
          addMarkers(lng=as.numeric(df_demo$long),
                     lat=as.numeric(df_demo$lat), label=df_demo$name,
                     labelOptions = labelOptions(noHide = F)) %>%
          addPolylines(data = df_demo, lng = ~long, lat = ~lat)
      })
      
      ## London Fields ##
    } else if(finish=="I don't care" && adventure >=2){
      optiontitle <- "Option 4: Try Something New!"
      mytext1 <- "1. Meet at 09:30 at London Fields Lido"
      mytext2 <- "2. Swimming!!"
      mytext3 <- "3. Food at. Finch Cafe"
      mytext4 <- "4. Long walk back to campus along the canal (1hr ish)"
      
      point1_lat_demo <-51.54242180517086
      point1_long_demo <- -0.06150461704325338
      point2_lat_demo <- 51.53989156690294
      point2_long_demo <- -0.05754207286302285
      point3_lat_demo <-51.53608270390507
      point3_long_demo <- -0.0654282811430612
      point4_lat_demo <- 51.5246602532804
      point4_long_demo <- -0.13400527822771696
      
      
      df_demo <- data.frame(long=c(point1_long_demo, point2_long_demo,
                                   point3_long_demo, point4_long_demo), lat =
                              c(point1_lat_demo, point2_lat_demo,
                                point3_lat_demo, point4_lat_demo), name = c("1. Start Here: London Fields Lido",
                                                                            "2. Food @ Finch Cafe",
                                                                            "3. Long Walk back to campus via Canals", 
                                                                            "4. Finish at Campus"))
      
      
      output$optiontitle <- renderText({
        toString(optiontitle)
      })
      output$text1 <- renderText({
        toString(mytext1)
      })
      output$text2 <- renderText({
        toString(mytext2)
      })
      output$text3 <- renderText({
        toString(mytext3)
      })
      output$text4 <- renderText({
        toString(mytext4)
      })
      
      output$mymap <- renderLeaflet({
        leaflet() %>% addTiles() %>%
          addMarkers(lng=as.numeric(df_demo$long),
                     lat=as.numeric(df_demo$lat), label=df_demo$name,
                     labelOptions = labelOptions(noHide = F)) %>%
          addPolylines(data = df_demo, lng = ~long, lat = ~lat)
      })
    } else if(finish=="I don't care" && travel==3 && energy==3 && adventure==3){
      optiontitle <- "Option 2: Face Your Fears grrrrr >:("
      mytext1 <- "1. Meet at Santander bikes outside Archaeology at 09:30"
      mytext2 <- "2a. Cycling Scenic Route: UCL> Temple > Embankment/Westminster > cross at Westminster bridge > south bank > millennium bridge > st Paul's cathedral > UCL"
      mytext3 <- "2b. Cycling Beginner Friendly Route:  UCL > regents park > primrose hill > Camden > UCL."
      mytext4 <- "3. Food - depends on where we end up"
      
      point1_lat_demo <-51.525017580067924
      point1_long_demo <- -0.13168847790982494
      
      df_demo <- data.frame(long=c(point1_long_demo), lat =
                              c(point1_lat_demo), name = c("1. Start Here: UCL Archaeology"))
      
      
      output$optiontitle <- renderText({
        toString(optiontitle)
      })
      output$text1 <- renderText({
        toString(mytext1)
      })
      output$text2 <- renderText({
        toString(mytext2)
      })
      output$text3 <- renderText({
        toString(mytext3)
      })
      output$text4 <- renderText({
        toString(mytext4)
      })
      
      output$mymap <- renderLeaflet({
        leaflet() %>% addTiles() %>%
          addMarkers(lng=as.numeric(df_demo$long),
                     lat=as.numeric(df_demo$lat), label=df_demo$name,
                     labelOptions = labelOptions(noHide = F)) %>%
          addPolylines(data = df_demo, lng = ~long, lat = ~lat)
      })
    } else{
      optiontitle <- "Option 1: Something Comforting :)"
      mytext1 <- "1. Meet at 09:30 at Parliament Hill Lido"
      mytext2 <- "2. Swimming!!"
      mytext3 <- "3. Food at. Polly's"
      mytext4 <- "4. Kenwood house "
      
      point1_lat_demo <-51.55637232098641
      point1_long_demo <- -0.15110095937060003
      point2_lat_demo <- 51.55573608135147
      point2_long_demo <- -0.16669033053456359
      point3_lat_demo <-51.57153490435442
      point3_long_demo <- -0.16742747790985205
      
      
      df_demo <- data.frame(long=c(point1_long_demo, point2_long_demo,
                                   point3_long_demo), lat =
                              c(point1_lat_demo, point2_lat_demo,
                                point3_lat_demo), name = c("1. Start Here: Parliament Hill Lido",
                                                           "2. Food @ Polly's Cafe",
                                                           "3. Walk Through Hampstead Heath and Kenwood House"))
      
      
      output$optiontitle <- renderText({
        toString(optiontitle)
      })
      output$text1 <- renderText({
        toString(mytext1)
      })
      output$text2 <- renderText({
        toString(mytext2)
      })
      output$text3 <- renderText({
        toString(mytext3)
      })
      output$text4 <- renderText({
        toString(mytext4)
      })
      
      output$mymap <- renderLeaflet({
        leaflet() %>% addTiles() %>%
          addMarkers(lng=as.numeric(df_demo$long),
                     lat=as.numeric(df_demo$lat), label=df_demo$name,
                     labelOptions = labelOptions(noHide = F)) %>%
          addPolylines(data = df_demo, lng = ~long, lat = ~lat)
      })
    }
    
  })
}

shinyApp(ui, server)