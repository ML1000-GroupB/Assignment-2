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
 titlePanel("Let's cluster for mixed data!"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Please upload a CSV File that you want to analyze:", accept = ".csv"),
      helpText("Default max file size is 30MB."),
      br(),
      numericInput(inputId =  "k", label="How many clusters do you want?",
                   value=3,min=0,max=10),
      br(),
      checkboxGroupInput(inputId ="columns", label="Below are all the columns of your data, 
                         please select the ones you want to use for clustering:", 
                         choices = c("1"=1,"2"=2)),
      
      numericInput(inputId =  "xaxis", label="Which column in your data do you want to plot on the x-axis to look at clusters
                   (2D plot by clusters)?",
                   value=7, min=0),
      
      numericInput(inputId = "yaxis", label = "Which column in your data do you want to plot on the y-axis to look at clusters 
                   (2D plot by clusters)?",
                   value=10,min=0),
      width = 4
      
      ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Introduction",
                 h5("This app is an application of clustering analysis for mixed data, which contains both numeric and categorical variables.
                    Clustering analysis is one of the unsupervised learning algorithms. 
                    It finds similarities between data according to the characteristics 
                    found in the data and groups similar data objects into clusters. 
                    Intuitively, it can be shown as the picture below."),
                 br(),
                 img(src = "https://raw.githubusercontent.com/ML1000-GroupB/Assignment-2/main/images.png", height = 140, width = 350, align="right",position="top"),
                
                 h5(),
                 
                 h5("How does this app work:"),
                 h5(),
                 
                 h6(em("1. What is the background of the default data in this app?")),
                 
                 tags$div(
                   "The default plots in the 3rd and 4th tabs are based on the US superstore data.",
                   tags$a(href="https://www.kaggle.com/juhi1994/superstore", 
                          em("The raw data can be found by clicking here."))
                 ),
                 
                 h6(),
                 h6(em("2. What data was used for the default demonstrative plots in the 3rd and 4th tab?")),
                 tags$div(
                 "We chose 9 variables from the original data, and added a transformed variable, which indicates the number of days it took to ship since the order was made. 
                    Since there were over 9000 records in the raw data and the computational time of the PAM and FAMD function could be long, for display purpose, we randomly selected
                    2000 records from the raw data to perform clustering and display the results in the last 2 tabs.",
                          a(href="https://github.com/ML1000-GroupB/Assignment-2/blob/main/US_Superstore_data_forclustering.csv",
                            em("The traing data used to demonstrate clustering can be found by clicking here."))
                   
                 ),
                 h6( ),

                 h6(em("3. What clustering method is used in this app?")),
                 h6("Since original the data contains both numeric and categorical variables, we use Gower's distance to measure the similarities between two records, 
                 and then use PAM (Partitioning Around Medoids) algorithm to identify clusters. 
                    The default number of clusters used in this app is 3, if it does not suit your business case, please customize it on the left side panel."
                    ),
                 h6(),
                 
                 h6(em("4. How to read the plot in the 3rd tab?")),
                 h6("The '2D plot by clusters' tab shows the scatter plot of any two variables of your choice from your data. Each record/data point is colored by its cluster.
                     For a good clustering partition, different colors in the scatter plot should be as separated as possible, and the points of the same color should cluster closely."),
                 h6(),
                 
                 h6(em("5. How to read the plot in the 4th tab?")),
                 h6("The 'Visualization of FAMD (Factor Analysis of Mixed Data)' tab demonstrates the clusters on the first two principal components of the data, 
                    which is done by FAMD. FAMD is a principal component method dedicated to analyze a data set containing both numeric and categorical variables. 
                    Plots from a good clustering should have little overlapping from the concentration ellipse around each cluster. If you obtain a plot with a lot overlapping areas, 
                    please consider clean or transform your data, or change the number of clusters."), 
                h6("Please note that if your data contains only numeric or categorical variables, 
                    the clustering will still work and you can still get a 2D plot on your origianl variables. However, since FAMD is intended for mixed data only, 
                    you will not be able to get a clustering plot on the FAMD reduced dimensions in the 4th tab if your data has only numeric or categorical variables. "),
                 br()
                 
                 ),
       tabPanel("How to use this app?",
                h6("  1. Upload your own data from the 'Browse' button on the top left corner."),
                h6("  2. Select the number of clusters/groups you want for your data."),
                h6("  3. Select the variables that you want to perform clustering on from the left panel."),
                h6("  4. Input the column indices of the variables that you want to take a look at the 2D plot in the bottom left boxs."),
                h6("  5. Go to '2D plot by clusters' to inspect the clusters."),
                h6("  6. Go to the 'Visualization of FAMD' tab to inspect the clusters based on the first two principal components."),
                h5(strong("Please note whenever you change your selections on the left side panel, the results and plots will be updated accordingly and automatically on the tabs! 
                           ")),
                h6(em("Please note this clustering app may not work for large dataset due to memory limit.")),
                h5()
       ),
         
        
      tabPanel("2D Plot by clusters",
        plotOutput("plot1"),
        br(),
        tags$div("The above sample scatter plot of the Sales and Profit variables, from the ",
                 a(href="https://github.com/ML1000-GroupB/Assignment-2/blob/main/US_Superstore_data_forclustering.csv",
                   em("randomly sampled 2000 records from the US Superstore data")), "shows the profit can either go up or go down as the sales increases. 
                   Moreover, the three colors (i.e., three clusters) overlapped each other, indicating the three clusters do not show a clear 
                  relationship between Profit and Sales. In other words, the clusters may not be very meaningful. This could be due to only 20% of all records were used to identify clusters, 
                 or some other new variables or potential transformations might be needed to figure out a clearer pattern."
        )
       ),
      
      tabPanel("Visualization of FAMD (Factor Analysis of Mixed Data)",
      plotOutput("plot2"),
      br(),
      tags$div("The above sample plot of the Sales and Profit variables, from the ",
               a(href="https://github.com/ML1000-GroupB/Assignment-2/blob/main/US_Superstore_data_forclustering.csv",
                 em("randomly sampled 2000 records from the US Superstore data")), "confirms what was shown from the 2D plot in the first tab - the clusters are overlapping,
               indicating the records in the sample data might not be well partitioned. If you obtain a similar plot, please consider other possible methods to improve your clusters."
      )
      )
    )
  )
)
)

##############################################################################
################################### SERVER ###################################

server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 30*1024^2)
  
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
#      input_data_upload=input_data_upload[complete.cases(input_data_upload),]
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


