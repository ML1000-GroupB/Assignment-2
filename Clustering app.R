library(shiny)
library(cluster)
library(ggplot2)
library(shinydashboard)
library(shinythemes)
library(readr)
#library(fpc)
#library(factoextra)
#############################################################################
###################################### UI ###################################

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File", accept = ".csv"),
      checkboxInput("header", "Header", TRUE)
    ),
    mainPanel(
      plotOutput("plot1")
    )
  )
)

##############################################################################
################################### SERVER ###################################

server <- function(input, output) {
      

output$plot1 <- renderPlot({
  

  input_data = reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    input_data = read.csv(inFile$datapath, stringsAsFactors = TRUE )
    
    input_data=as.data.frame(input_data)
  }) 
  
  
  pam_fit <- reactive({
    
                   gower_dist = daisy(input_data(), metric = "gower", type = list(logratio = 3))
                  
                  gower_mat = as.matrix(gower_dist)
                  
                  pam_fit = pam(gower_mat, k=3, diss=TRUE)
              
                  
  })

  
  
  par(mar = c(2,2,2,2))
  

  ggplot(aes(x = Sales, y = Profit), 
             data = input_data() )+ 
    geom_point( aes(color = factor(pam_fit()$clustering) ) ) + 
    scale_color_manual(name = "Clusters",values = c("blue","red","green"), 
                       labels=c("Cluster 1","Cluster 2","Cluster 3"))
  


})


}



# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)


