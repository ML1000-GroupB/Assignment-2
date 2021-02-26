library(shiny)
library(cluster)
library(ggplot2)
library(shinydashboard)
library(shinythemes)
library(readr)
library(fpc)
library(factoextra)
library(FactoMineR)
library(ggpubr)
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
      
      checkboxGroupInput(inputId =  "columns", label="Which columns do you want to use for clustering?", 
                         choices = c("1"=1, "2"=2,"3"=3,"4"=4,"5"=5,"6"=6,
                                     "7"=7,"8"=8,"9"=9,"10"=10,"11"=11,
                                     "12"=12,"13"=13,"14"=14,"15"=15,"16"=16,
                                     "17"=17,"18"=18,"19"=19,"20"=20,"21"=21,
                                     "22"=22,"23"=23,"24"=24,"25"=25),selected = c(5,13,15,16,18:21)),
      helpText("This app is suitable for dataset with up to 25 columns."),
      
      numericInput(inputId =  "xaxis", label="Which column do you want to plot on the x-axis for clustering?",
                   value=18),
      
      numericInput(inputId = "yaxis", label = "Which column do you want to plot on the y-axis for clustering?",
                   value=21)
      
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
  
  
  pam_fit <- reactive({
    
    if (is.null(input_data())) {return()} else {
      
      withProgress(
        message = 'Clustering in progress. Please wait ...', {
          
          gower_dist = daisy(input_data()[,as.numeric(input$columns)], metric = "gower", type = list(logratio = 3))
          
          gower_mat = as.matrix(gower_dist)
          
          pam_fit = pam(gower_mat, k=3, diss=TRUE)
          
        }
      )
    }
    
  })
  
  
  
output$plot1 <- renderPlot({

  if (is.null(input_data())) {return()} else {
    
    withProgress(
      message = 'Clustering in progress. Please wait ...', {
        
        par(mar = c(2,2,2,2))
        
        X=input$xaxis
        
        Y=input$yaxis
        
        ggplot(data = input_data(), aes(x = input_data()[,X], y = input_data()[,Y]))+ 
          geom_point( aes(color = factor(pam_fit()$clustering) ) ) + 
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

  famd_fca=FAMD(input_data()[,as.numeric(input$columns)])
#  plot(input_data()[,18],xlim = c(0,1000),ylim = c(0,1000))
  ind=get_famd_ind(famd_fca)$coord
  ind=as.data.frame(ind)
  ind$cluster=as.factor(pam_fit()$clustering)
  
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


