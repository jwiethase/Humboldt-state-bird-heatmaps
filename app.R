# 
# 
rm(list = ls(all=TRUE))  
library(shiny)
library(dplyr)
library(ggplot2)
library(gstat)
library(sp)
library(lubridate)
library(DT)
library(shinycssloaders)
library(raster)
library(RStoolbox)
load("banding_data_CJ.RData")

Species <- c("Wrentit (Chamaea fasciata)", "Swainson's Thrush (Catharus ustulatus)", 
             "Hermit Thrush (Catharus guttatus)", "Song Sparrow (Melospiza melodia)", 
             "Dark-eyed Junco (Junco hyemalis oreganus)", "Spotted Towhee (Pipilo maculatus)", 
             "Fox Sparrow (Passerella iliaca megarhyncha)", "Wilson's Warbler (Cardellina pusilla)", 
             "Macgillivray's Warbler (Geothlypis tolmiei)", "Orange-crowned Warbler (Vermivora celata)", 
             "Nashville Warbler (Leiothlypis ruficapilla)", "Yellow-rumped Warbler (Setophaga coronata auduboni)", 
             "Yellow Warbler (Setophaga petechia)", "Yellow-breasted Chat (Icteria virens)", 
             "Mountain Chickadee (Poecile gambeli)", "Chestnut-backed Chickadee (Poecile rufescens)", 
             "Willow Flycatcher (Empidonax traillii)", "Western Tanager (Piranga ludoviciana)", 
             "Black-headed Grosbeak (Pheucticus melanocephalus)", 
             "Purple Finch (Haemorhous purpureus)")

Species <- c(sort(Species, decreasing = FALSE))


# Define UI for application that draws a histogram
ui <- fluidPage(theme = "bootstrap.css",
      ## custom CSS for 3 column layout (used below for mechanics filter options)
      tags$head(
            tags$style(HTML("
                            
                            .multicol {
                            
                            -webkit-column-count: 2; /* Chrome, Safari, Opera */
                            
                            -moz-column-count: 2; /* Firefox */
                            
                            column-count: 2;
                            
                            }
                            
                            "))
            
            ),
   # Application title
   titlePanel("Heatmap of body condition"),
   
   # Sidebar with a slider input for number of bins 
   sidebarPanel(
            selectInput(inputId = "species",
                        label = "Species:",
                        choices = Species),
            selectInput(inputId = "age",
                        label = "Life stage:",
                        choices =  c("All Combined", "Local", "Hatching Year", "After Hatching Year")),
            selectInput(inputId = "sex",
                        label = "Sex:",
                        choices = c( "All Combined", "Male", "Female")),
            selectInput(inputId = "breeding",
                        label = "Breeding:",
                        choices = c( "All Combined", "Yes", "No")),
            selectInput(inputId = "molting",
                        label = "Molting:",
                        choices = c( "All Combined", "Yes", "No")),
            selectInput(inputId = "point",
                        label = "Choose point display option", 
                        choices = c("Points --> Individuals", "Points --> Individuals/Net hours")),
            wellPanel(tags$div(class = "multicol",
                               checkboxGroupInput(inputId = "months",
                                                  label = "Choose Months", 
                                                  choiceNames = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                                                                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                                                  choiceValues = 1:12,
                                                  selected = 1:12))),
            actionButton("go", "Render the plot!", icon = icon("angle-double-right"), width = "auto")
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(tags$head(tags$style(type="text/css", "
             #loadmessage {
                                     position: fixed;
                                     top: 0px;
                                     left: 0px;
                                     width: 100%;
                                     padding: 5px 0px 5px 0px;
                                     text-align: center;
                                     font-weight: bold;
                                     font-size: 100%;
                                     color: #000000;
                                     background-color: #D3D3D3;
                                     z-index: 105;
                                     }
                                     ")),
                conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                 tags$div("Loading...",id="loadmessage")),
            splitLayout(cellWidths = c("50%", "50%"),verbatimTextOutput("nIndividuals"),
                        verbatimTextOutput("info")),
            tabsetPanel(type = "tabs",
                        tabPanel("Heat Map",  plotOutput("heatPlot", width = "100%", click = "plot_click")),
                        tabPanel("Data", withSpinner(DT::dataTableOutput('table'))),
                        tabPanel("Banding Stations", withSpinner(DT::dataTableOutput('station'))))
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
      output$info <- renderText({
            xy_str <- function(e) {
                  if(is.null(e)) return("NULL\n")
                  paste0("lon=", round(e$x, 3), " lat=", round(e$y, 3), "\n")
            }
            xy_range_str <- function(e) {
                  if(is.null(e)) return("NULL\n")
                  paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
                         " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
            }
            
            paste0(
                  "click: ", xy_str(input$plot_click)
            )
      })
      output$table <- DT::renderDataTable({
            Species <- gsub("[[:space:]]\\(.*$", "", input$species)
            new_df <- data_merged %>% dplyr::filter(grepl(Species, CommonName, ignore.case = TRUE) ==
                                                          TRUE)
            if(input$age != "All Combined"){
                  new_df <- new_df %>% dplyr::filter(Age == input$age)}
            if(input$sex != "All Combined"){
                  new_df <- new_df %>% dplyr::filter(grepl(input$sex, Sex, ignore.case = TRUE) ==
                                                           TRUE)}
            if(input$breeding != "All Combined"){
                  new_df <- new_df %>% dplyr::filter(breeding == input$breeding)}
            if(input$molting != "All Combined"){
                  new_df <- new_df %>% dplyr::filter(molt == input$molting)}
            new_df$date <- dmy(paste(new_df$DayCollected, new_df$MonthCollected, new_df$YearCollected, sep="-"))
            new_df$date <- as.character(new_df$date)
            new_df$bodyCondition <- new_df$Weight/new_df$WingMeasurementLength
            new_df$bodyCondition <- round(new_df$bodyCondition, digits = 3)
            tableOut <- new_df %>% dplyr::select(CommonName, bodyCondition, breeding, molt, LifeStage, Sex, 
                                          WingMeasurementLength, Weight, date) %>% 
                  dplyr::rename(WingLength = WingMeasurementLength)
            
            tableOut 
            
      }, options = list(scrollX = TRUE))
      
      output$station <- DT::renderDataTable({
            Species <- gsub("[[:space:]]\\(.*$", "", input$species)
            new_df <- data_merged %>% dplyr::filter(grepl(Species, CommonName, ignore.case = TRUE) ==
                                                          TRUE)
            if(input$age != "All Combined"){
                  new_df <- new_df %>% dplyr::filter(Age == input$age)}
            if(input$sex != "All Combined"){
                  new_df <- new_df %>% dplyr::filter(grepl(input$sex, Sex, ignore.case = TRUE) ==
                                                           TRUE)}
            if(input$breeding != "All Combined"){
                  new_df <- new_df %>% dplyr::filter(breeding == input$breeding)}
            if(input$molting != "All Combined"){
                  new_df <- new_df %>% dplyr::filter(molt == input$molting)}
            new_df$date <- dmy(paste(new_df$DayCollected, new_df$MonthCollected, new_df$YearCollected, sep="-"))
            new_df$date <- as.character(new_df$date)
            new_df$bodyCondition <- new_df$Weight/new_df$WingMeasurementLength
            new_df$bodyCondition <- round(new_df$bodyCondition, digits = 3)
            tableOut <- new_df %>% dplyr::select(DecimalLongitude, 
                                          DecimalLatitude, RouteIdentifier, Locality)
            tableOut <- unique(tableOut)
            tableOut$DecimalLongitude <- round(tableOut$DecimalLongitude, digits = 3)
            tableOut$DecimalLatitude <- round(tableOut$DecimalLatitude, digits = 3)
            tableOut 
      }, options = list(scrollX = TRUE))
      
      output$nIndividuals <- renderText({
            Species <- gsub("[[:space:]]\\(.*$", "", input$species)
            
            new_df <- data_merged %>% dplyr::filter(grepl(Species, CommonName, ignore.case = TRUE) ==
                                                          TRUE)
            if(input$age != "All Combined"){
                  new_df <- new_df %>% dplyr::filter(Age == input$age)}
            if(input$sex != "All Combined"){
                  new_df <- new_df %>% dplyr::filter(grepl(input$sex, Sex, ignore.case = TRUE) ==
                                                           TRUE)}
            if(input$breeding != "All Combined"){
                  new_df <- new_df %>% dplyr::filter(breeding == input$breeding)}
            if(input$molting != "All Combined"){
                  new_df <- new_df %>% dplyr::filter(molt == input$molting)}
            
            new_df <- new_df %>% dplyr::filter(MonthCollected %in% input$months == TRUE)
            validate(
                  need(length(rownames(new_df)) > 20, paste("Not enough data (n=", length(rownames(new_df)), 
                                                            ", min. of 20 required)"))
            )
            
            paste("Number of individuals:", length(rownames(new_df)))
            
      })
   observeEvent(input$go, {
   output$heatPlot <- renderPlot({
         input$go
         isolate({
         Species <- gsub("[[:space:]]\\(.*$", "", input$species)
        
         new_df <- data_merged %>% dplyr::filter(grepl(Species, CommonName, ignore.case = TRUE) ==
                                                       TRUE)
         if(input$age != "All Combined"){
               new_df <- new_df %>% dplyr::filter(Age == input$age)}
         if(input$sex != "All Combined"){
               new_df <- new_df %>% dplyr::filter(grepl(input$sex, Sex, ignore.case = TRUE) ==
                                                        TRUE)}
         if(input$breeding != "All Combined"){
               new_df <- new_df %>% dplyr::filter(breeding == input$breeding)}
         if(input$molting != "All Combined"){
               new_df <- new_df %>% dplyr::filter(molt == input$molting)}
        
         new_df <- new_df %>% dplyr::filter(MonthCollected %in% input$months == TRUE)
         new_df$bodyCondition <- new_df$Weight/new_df$WingMeasurementLength
         
         # Make data frame for mapping
         new_df <- new_df %>% dplyr::rename(lon = "DecimalLongitude", 
                                            lat = "DecimalLatitude")
         coords <- cbind(new_df$lon, new_df$lat)
         sp = sp::SpatialPoints(coords)
         spdf = sp::SpatialPointsDataFrame(sp, new_df)
         sp::proj4string(spdf) <- CRS("+init=epsg:4326")
         
         # Create an empty grid where n is the total number of cells
         # Define the grid extent:
         
         x.range <- as.numeric(c(min(new_df$lon - 1), max(new_df$lon + 
                                                                1)))  # min/max longitude of the interpolation area
         y.range <- as.numeric(c(min(new_df$lat - 1), max(new_df$lat + 
                                                                1)))  # min/max latitude of the interpolation area
         
         extent <- data.frame(lon = c(min(new_df$lon - 0.5), max(new_df$lon + 
                                                                       0.5)), lat = c(min(new_df$lat - 0.5), max(new_df$lat + 
                                                                                                                       0.5)))
         # expand points to grid
         grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2],
                                    by = round((log(length(rownames(new_df))))^2 * 0.0007, digits = 3)),
                            y = seq(from = y.range[1], 
                                    to = y.range[2], 
                                    by = round((log(length(rownames(new_df))))^2 * 0.0007, digits = 3)))
            
         sp::coordinates(grd) <- ~x + y
         sp::gridded(grd) <- TRUE
         
         # Add P's projection information to the empty grid
         sp::proj4string(grd) <- sp::proj4string(spdf)
         
         # Interpolate the grid cells using a power value of 2
         # (idp=2.0)
         
         P.idw <- gstat::idw(new_df$bodyCondition ~ 1, locations = spdf, 
                             newdata = grd, idp = 2)
        
         # Convert to raster object then clip to Oregon/California
         r <- raster::raster(P.idw)
         temp <- as.data.frame(P.idw)
         clipped <- as.data.frame(raster::mask(r, oregonCalif))
         clip_df <- merge(temp, clipped, by = "var1.pred")
         
         # Calculate Individuals/Net hours
         new_df$NetHours1 <- as.numeric(as.character(new_df$NetHours1))
         new_df <- new_df %>% dplyr::group_by(RouteIdentifier) %>% 
               dplyr::mutate(count = length(CommonName), countNetHours = length(CommonName)/sum(NetHours1, 
                                                                                                na.rm = TRUE))
         text_df <- new_df %>% dplyr::ungroup() %>% dplyr::select(lon, 
                                                                  lat, count, countNetHours, RouteIdentifier) %>% unique()
         
         # Check if we arrived at a data set that is large enough for interpolation
         validate(
               need(length(rownames(new_df)) > 20, paste("Not enough data (n=", length(rownames(new_df)), 
                                                        ", min. of 20 required)"))
         )
  
         
         clip_df$cut <- raster::cut(clip_df$var1.pred, breaks = round(log(length(rownames(new_df))) * 2))
         clip_df <- clip_df %>% dplyr::rename(lon = "x", lat = "y")
         
         plot <- ggplot2::ggplot(data = text_df, aes(x = lon, y = lat)) + 
               geom_tile(data = clip_df, aes(x = lon, y = lat, fill = cut)) + 
               geom_polygon(data = oregonCalif, aes(long, lat, group = group), 
                            color = "black", fill = "transparent", lwd = 0.3) + 
               geom_polygon(data = ocean, aes(long, lat, group = group), 
                            color = "black", fill = "lightseagreen", lwd = 0.3, 
                            alpha = 0.5) 
         if (input$point == "Points --> Individuals")
         {
               plot <- plot + geom_point(aes(cex = count), pch = 21, 
                                         alpha = 0.3, fill = "black", col = "black")
         }
         
         if (input$point == "Points --> Individuals/Net hours")
         {
               plot <- plot + geom_point(aes(cex = countNetHours), pch = 21, 
                                         alpha = 0.3, fill = "black", col = "black")
         }
         
         plot <- plot + coord_fixed(ratio = 1, xlim = extent$lon, 
                                    ylim = extent$lat) + 
               theme_bw() + 
               guides(fill = guide_legend(title = "Body condition"),
                      cex = guide_legend(title = input$point, nrow=2,byrow=TRUE)) +
               xlab("lon") + 
               ylab("lat") + 
               theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(), 
                     legend.background = element_rect(fill = "transparent", size = 0.5, linetype = "solid", colour = "grey"), 
                     plot.title = element_text(size = 10), 
                     legend.text = element_text(size = 8), 
                     legend.key.size = unit(0.32, "cm"), 
                     legend.title = element_text(size = 8), 
                     legend.position = "bottom") + 
                     scale_fill_manual(values = c(rev(heat.colors(round(log(length(rownames(new_df))) * 
                                                                        2))))) + 
               scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0,0))
         plot
         })
   }, width = 650, height = 650)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

