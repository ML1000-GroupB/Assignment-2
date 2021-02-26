library(shiny)
library(cluster)
library(ggplot2)
library(shinydashboard)
library(shinythemes)
library(readr)
library(fpc)
library(factoextra)
library(FactoMineR)
#############################################################################
###################################### UI ###################################

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Please upload a CSV File that you want to analyze:", accept = ".csv"),
      helpText("Default max file size is 5MB."),
      checkboxInput(inputId =  "header", label="Header", TRUE),
      radioButtons (inputId = "stringsAsFactors", label = "Do you want strings to be treated as Factors?",
                    choices = c("Yes" = "TRUE", "No" = "FALSE")),
      ),
    
    mainPanel(
      tabsetPanel(
      tabPanel("Plots for your data",
        plotOutput("plot1")
       ),
      tabPanel("Plots on PCA",
      plotOutput("plot2")
      )
    )
  )
)
)

##############################################################################
################################### SERVER ###################################

server <- function(input, output) {
  
  input_data = reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile)) {
      return()
      } 
        
        input_data = read.csv(inFile$datapath, 
                              stringsAsFactors = as.logical(input$stringsAsFactors), 
                              header = input$header)
        
        input_data=as.data.frame(input_data)
        input_data
      
    })
  
  
output$plot1 <- renderPlot({

  if (is.null(input_data())) {return()} else {
    
    withProgress(
      message = 'Clustering in progress. Please wait ...', {
        
        gower_dist = daisy(input_data()[,c(5,13,15,16,18:21)], metric = "gower", type = list(logratio = 3))
        
        gower_mat = as.matrix(gower_dist)
        
        pam_fit = pam(gower_mat, k=3, diss=TRUE)
        
        par(mar = c(2,2,2,2))
        
        ggplot(data = input_data(), aes(x = Sales, y = Profit))+ 
          geom_point( aes(color = factor(pam_fit$clustering) ) ) + 
          scale_color_manual(name = "Clusters",values = c("blue","red","green"), 
                             labels=c("Cluster 1","Cluster 2","Cluster 3"))
        
      }
    )
  }

})

output$plot2 <- renderPlot({
  
  
  if (is.null(input_data())) {return()} else {
    
    withProgress(
      message = 'FAMD in progress. Please wait ...', {

  famd_fca=FAMD(input_data()[,c(5,13,15,16,18:21)])
#  plot(input_data()[,18],xlim = c(0,1000),ylim = c(0,1000))
  ind=get_famd_ind(famd_fca)$coord
  ind=as.data.frame(ind)
  ind$cluster=as.factor(pam_fit$clustering)
  
  ggscatter(
    ind, x = "Dim.1", y = "Dim.2",
    color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",    #adding the concentration ellipses
    size = 1.5,  legend = "right", ggtheme = theme_classic()
    
  ) 
  
  
      }
)
  }
})

}



# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)


