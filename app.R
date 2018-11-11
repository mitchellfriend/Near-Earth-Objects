#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(dplyr)
library(shiny)
library(plotly)
library(readr)
library(scatterplot3d)
library(plotly)

grpby <- fread("NEO2_grpby.csv")
orbits <- NEO2_grpbyplot$orbits
size <- NEO2_grpbyplot$size
velocity <- NEO2_grpbyplot$velocity
df <- fread("NEO2.csv")

ui <- fluidPage(
  #Header
  titlePanel(title = h2("Near Earth objects", align="center")),
  
  sidebarLayout(
    
    sidebarPanel(
      
      radioButtons(inputId = "y",
                   label = "Heatmap: Size Estimate of NEO (m)",
                   choices = c("Low" = "LowSizeEstimateMeters"  ,
                               "Median" = "MedianSizeEstimateMeterMeters",
                               "High" = "HighSizeEstimateMeters" ),
                   selected = "MedianSizeEstimateMeterMeters")
      
    
    
  ),
  
  
  #Main Panel
  mainPanel(
    tabsetPanel(type = "tab",
                tabPanel("Information"),
                tabPanel("Heatmap", plotOutput("heatmap", width = "700px", height = "600px", inline = FALSE)),
                tabPanel("Plot"),
                tabPanel("3D model", plotlyOutput("plot", width = "800px", height = "700px", inline = FALSE))
                )
      )
    )
  )
server <- function(input, output, session) {
  
  output$heatmap <-  renderPlot({
    
    
    ggplot(df, aes_string(x="Nominal_Distance_mi", y=input$y) ) +
      ylim(1000,3000)+
      stat_density_2d(geom = "raster", aes(fill = stat(density)), contour = FALSE)+
      xlab("Distance From Center of Earth (mi.)")+
      ylab("Diameter (m)")+
      scale_fill_gradient2(low="black",
                           high="navyblue",
                           mid="lightblue",
                           midpoint=0.00000000005)+
      scale_x_continuous(labels=scales::comma)+
      theme(
        legend.position='right',
        axis.text =element_text(size=18),
        axis.title = element_text(size=24)
      )
  })
  output$plot <- renderPlotly({
    plot_ly(grpby, x = orbits, y = size, z = velocity, size = size,
            marker = list(color = size, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE) ) %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = '# of flybys (X-axis'),
                          yaxis = list(title = 'size (Y-axis'),
                          zaxis = list(title = 'velocity(Z-axis')),
             annotations = list(
               x = 1.13,
               y = 1.05,
               text = 'Object Diameter (mi.)',
               xref = 'paper',
               yref = 'paper',
               showarrow = FALSE
             ))
  })
  
  
  
  
  

  
  
  
}

shinyApp(ui, server)


