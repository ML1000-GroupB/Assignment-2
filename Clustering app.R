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
library(readr)
library(thematic)
library(bslib)
#############################################################################
###################################### UI ###################################

ui <- fluidPage(
  
  theme = bs_theme(bg="#00783c", fg="#f0f0f0", primary="#EA80FC",
                   secondary = "#f4c0ff", 
                   base_font = font_google("Fira Sans", local = TRUE)),
 titlePanel("Let's cluster!"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Please upload a CSV File that you want to analyze:", accept = ".csv"),
      helpText("Default max file size is 5MB."),
      br(),
      numericInput(inputId =  "k", label="How many clusters do you want?",
                   value=3),
      br(),
      checkboxGroupInput(inputId ="columns", label="Below are all the columns of your data, 
                         please select the ones you want to use for clustering:", 
                         choices = c("1"=1,"2"=2)),
      
      numericInput(inputId =  "xaxis", label="Which column in your data do you want to plot on the x-axis to look at clusters
                   (2D plot by clusters)?",
                   value=7),
      
      numericInput(inputId = "yaxis", label = "Which column in your data do you want to plot on the y-axis to look at clusters 
                   (2D plot by clusters)?",
                   value=10),
      width = 4
      
      ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Introduction",
                 h5("This app is an application of clustering analysis for mixed data, which contains both numeric and categorical variables.
                    Clustering analysis is one of the unsupervised learning algorithms. 
                    It finds similarities between data according to the characteristics 
                    found in the data and grouping similar data objects into clusters. 
                    Intuitively, it can be shown as the picture below."),
                 br(),
                 img(src = "https://raw.githubusercontent.com/ML1000-GroupB/Assignment-2/main/images.png", height = 140, width = 350, align="right",position="top"),
                
                 h5(em("How does this app work:")),
                 h6(),
                 
                 tags$div(
                   "1. The default plots in the 2 tabs are based on the US superstore data.",
                   tags$a(href="https://www.kaggle.com/juhi1994/superstore", 
                          em("The raw data can be found here."))
                 ),
                 
                 h6(),
                 tags$div("2. We chose 9 variables from the raw data, and added a transformed variable, which indicates the number of days it took to ship since the order was made. 
                    Since there were over 9000 records in the raw data, for the sake of shorter computational time for display purpose, we randomly selected
                    2000 records from the raw data to perform clustering and display the results in the 2 tabs.",
                          a(href="https://github.com/ML1000-GroupB/Assignment-2/blob/main/US_Superstore_data_forclustering.csv",
                            em("The traing data used to demonstrate clustering can be found here."))
                   
                 ),
                 h6( ),

                 h6("3. The current clustering method is xx, 
                    the default number of cluster is 3, if you are not satisfied, please customize it on the left side panel"
                    )
                 ),
        
        
      tabPanel("2D Plot by clusters",
        plotOutput("plot1"),
        h5("The above xxx")
       ),
      
      tabPanel("Visualization of FAMD (Factor Analysis of Mixed Data)",
      plotOutput("plot2")
      )
    )
  )
)
)

##############################################################################
################################### SERVER ###################################

server <- function(input, output, session) {
  
  dsnames <- c()
  
  input_data_upload = reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile)) {
              input_data_upload <- read.csv("https://raw.githubusercontent.com/ML1000-GroupB/Assignment-2/main/US_Superstore_data_forclustering.csv",
                                            stringsAsFactors = T)
              input_data_upload=as.data.frame(input_data_upload)
              
    } else {
      
      input_data_upload = read.csv(inFile$datapath, 
                                   stringsAsFactors = T, 
                                   header = T)
      
      input_data_upload=as.data.frame(input_data_upload)
      input_data_upload
      
      }
    })
  
  observe({
#    req(input_data_upload())
    dsnames <- names(input_data_upload())
    cb_options <- list()
    cb_options[ dsnames] <- dsnames
    updateCheckboxGroupInput(session, "columns",
                             label = "Below are all the columns in your data, 
                         please select the ones you want to use for clustering:",
                             choices = cb_options,
                             selected = cb_options)
  })
  
  
  pam_fit_upload <- reactive({
    
    if (is.null(input_data_upload())) {return()} else {
      
      withProgress(
        message = 'Clustering in progress. Please wait ...', {
          
          
 set.seed(123)         
          gower_dist = daisy(input_data_upload()[,input$columns], metric = "gower", type = list(logratio = 3))
          
          gower_mat = as.matrix(gower_dist)
          
          pam_fit_upload = pam(gower_mat, k=input$k, diss=TRUE)
          
        }
      )
    }
    
  })


output$plot1 <- renderPlot({
  
  if (is.null(input_data_upload())) {return()} else {
    
    withProgress(
      message = 'Clustering in progress. Please wait ...', {
        
        par(mar = c(3,3,3,3))
        
        X=input_data_upload()[,input$xaxis]
        
        Y=input_data_upload()[,input$yaxis]
        
        X_name=colnames(input_data_upload())[input$xaxis]
        
        Y_name=colnames(input_data_upload()[input$yaxis])
        
        legendnames=paste(rep("Cluster",input$k),1:(input$k))
        
        if (class(X)=="factor" & class(Y)!="factor") {
          
          if (max(Y)>1000) {
            
            ggplot(data = input_data_upload(), aes(x = X, y = Y))+ 
              geom_point( aes(color = factor(pam_fit_upload()$clustering) ) ) + 
              scale_color_manual(name = "Clusters", values = 1:(input$k),
                                 labels= legendnames) +
              labs(x=X_name,y=Y_name) +
              ylim(-max(Y)/5,max(Y)/5)
            
            
          } else {
            ggplot(data = input_data_upload(), aes(x = X, y = Y))+ 
              geom_point( aes(color = factor(pam_fit_upload()$clustering) ) ) + 
              scale_color_manual(name = "Clusters", values = 1:(input$k),
                                 labels= legendnames) +
              labs(x=X_name,y=Y_name) 
          }
          
             } else {
          
          if (class(X)!="factor"  & class(Y)!="factor") {
            
            ggplot(data = input_data_upload(), aes(x = X, y = Y))+ 
              geom_point( aes(color = factor(pam_fit_upload()$clustering) ) ) + 
              scale_color_manual(name = "Clusters", values = 1:(input$k),
                                 labels= legendnames) +
              labs(x=X_name,y=Y_name) +
              xlim(0,ifelse(max(X)>1000,max(X)/5,max(X))) +
              ylim(ifelse(max(Y)>1000 & min(Y)<0,-max(Y)/10,
                          ifelse(max(Y)>1000 & min(Y)>=0, min(Y),min(Y))),
                   ifelse(max(Y)>1000,max(Y)/10,max(Y))
                   )
            
          } else { 
            
            if (class(X)!="factor" & class(Y)=="factor" ) {
              
              ggplot(data = input_data_upload(), aes(x = X, y = Y))+ 
                geom_point( aes(color = factor(pam_fit_upload()$clustering) ) ) + 
                scale_color_manual(name = "Clusters", values = 1:(input$k),
                                   labels= legendnames) +
                labs(x=X_name,y=Y_name) +
                xlim(min(X),
                     ifelse(max(X)>1000,max(X)/10,max(X)))
              
            } else {
              
              ggplot(data = input_data_upload(), aes(x = X, y = Y))+ 
                geom_point( aes(color = factor(pam_fit_upload()$clustering) ) ) + 
                scale_color_manual(name = "Clusters", values = 1:(input$k),
                                   labels= legendnames) +
                labs(x=X_name,y=Y_name) 
                
            }
            

          }
        }
      }
    )
  }
  
})


output$plot2 <- renderPlot({
  
  
  if (is.null(input_data_upload())) {return()} else {
    
    withProgress(
      message = 'FAMD in progress. Please wait ...', {
        
set.seed(123)
        
  famd_fca=FAMD(input_data_upload()[,input$columns], graph = F)

    ind=get_famd_ind(famd_fca)$coord
  ind=as.data.frame(ind)
  ind$cluster=as.factor(pam_fit_upload()$clustering)
  
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
shinyApp(ui = ui, server = server )


