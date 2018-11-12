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
library(data.table)

grpby <- fread("NEO2_grpby.csv")



NEO_dropped_outliers_shiny <- fread("NEO2_no_outliers.csv")
NEO_dropped_outliers_shiny$NominalDistanceFlags <- ifelse(NEO_dropped_outliers_shiny$Nominal_Distance_mi <= 4000, 2, ifelse(NEO_dropped_outliers_shiny$Nominal_Distance_mi > 225623 , 0, 1))
NEO_dropped_outliers_shiny$MinimumDistanceFlags <- ifelse(NEO_dropped_outliers_shiny$Minimal_Distance_mi <= 4000, 2, ifelse(NEO_dropped_outliers_shiny$Minimal_Distance_mi > 225623 , 0, 1))



NEO2_grpby <- NEO_dropped_outliers_shiny%>%
  group_by(Object)%>%
  summarise(velocity = mean(`V relative (mi/s)`),
            orbits=mean(Orbits),
            size=max(High_Size_Estimate_Meters),
            min_dist=min(Minimal_Distance_mi))%>%
  filter(velocity<30, orbits<80)

NEO2_grpbyplot<- NEO2_grpby%>%
  filter(size<10000,orbits<50)


orbits <- NEO2_grpbyplot$orbits
size <- NEO2_grpbyplot$size
velocity <- NEO2_grpbyplot$velocity



ui <- fluidPage(
  #Header
  titlePanel(title = h2("Near Earth objects", align="center")),
  
  br(),
  
  sidebarLayout(
    
    sidebarPanel(
      
      radioButtons(inputId = "size",
                   label = "Heatmap: Size Estimate of NEO (m)",
                   choices = c("Low" = "Low_Size_Estimate_Meters"  ,
                               "Median" = "Median_Size_Estimate_Meters",
                               "High" = "High_Size_Estimate_Meters" ),
                   selected = "Median_Size_Estimate_Meters"),
      selectInput(inputId = "xaxis",
                  label = "Scatterplot X-axis",
                  choices =  c("Nominal Distance (mi)" = "Nominal_Distance_mi",
                               "Minimal Distance (mi)" = "Minimal_Distance_mi",
                               "Relative Velocity to Earth (mi./s)" = "`V relative (mi/s)`",
                               "H (magnitude)" = "`H (mag)`",
                               "Low Size Estimates (m)" = "Low_Size_Estimate_Meters",
                               "Median Size Estimate (m)" = "Median_Size_Estimate_Meter",
                               "High Size Estimate (m)"= "High_Size_Estimate_Meters"),
                  selected = "V relative (mi/s)"),

      selectInput(inputId = "yaxis",
                  label = "Scatterplot Y-axis",
                  choices =  c("Nominal Distance (mi)" = "Nominal_Distance_mi",
                               "Minimal Distance (mi)" = "Minimal_Distance_mi",
                               "Relative Velocity to Earth (mi./s)" = "`V relative (mi/s)`",
                               "H (magnitude)" = "`H (mag)`",
                               "Low Size Estimates (m)" = "Low_Size_Estimate_Meters",
                               "Median Size Estimate (m)" = "Median_Size_Estimate_Meters",
                               "High Size Estimate (m)"= "High_Size_Estimate_Meters"),
                  selected = "Nominal_Distance_mi")
                

      

    
    
  ),
  
  
  #Main Panel
  mainPanel(
    tabsetPanel(type = "tab",
                tabPanel("Information", plotOutput("scatterplot", width = "1000px", height = "700px", inline = FALSE)),
                tabPanel("Animation", plotlyOutput("animation", width = "1000px", height = "700px", inline = FALSE)),
                tabPanel("Heatmap", plotOutput("heatmap", width = "700px", height = "600px", inline = FALSE)),
                tabPanel("3D Graph", plotlyOutput("plot", width = "800px", height = "700px", inline = FALSE))
                )
      )
    )
  )
server <- function(input, output, session) {

  
  output$heatmap <-  renderPlot({
    

    
    ggplot(NEO_dropped_outliers_shiny, aes_string(x="Nominal_Distance_mi", y=input$size) ) +
      ylim(1000,3000)+
      stat_density_2d(geom = "raster", aes(fill = stat(density)), contour = FALSE)+
      xlab("Distance From Center of Earth (mi.)")+
      ylab("Size in (m)")+
      scale_fill_gradient2(low="black",
                           high="navyblue",
                           mid="lightblue",
                           midpoint=0.00000000005)+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black"))+
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
               x = 1.15,
               y = 1.02,
               text = 'Diameter (m)',
               xref = 'paper',
               yref = 'paper',
               showarrow = FALSE
             ))
  })
  
  output$animation <- renderPlotly({
    
    
    plot_ly(
      x = NEO_dropped_outliers_shiny$Nominal_Distance_mi,
      y = NEO_dropped_outliers_shiny$`V relative (mi/s)`,
      size = NEO_dropped_outliers_shiny$MedianSizeEstimateMeter,
      frame= NEO_dropped_outliers_shiny$YEAR,
      type = 'scatter',
      text = NEO_dropped_outliers_shiny$Object,
      hoverinfo = "text+x+y",
      mode = 'markers',
      color = NEO_dropped_outliers_shiny$NominalDistanceFlags,
      colors = "YlOrRd"
    )%>%
      layout(
        xaxis = list(
          type = "log")
        
      )%>%
      animation_opts(1250, redraw = FALSE)%>%
      hide_colorbar()
    
  })
  
  output$scatterplot <- renderPlot({
    ggplot(data =  NEO_dropped_outliers_shiny, aes_string(x=input$xaxis, y=input$yaxis ))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))+
      geom_point()
    
  
    
      
            
      
      

 })
  
  

  
  
  
}

shinyApp(ui, server)


