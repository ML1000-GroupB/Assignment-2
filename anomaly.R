#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggthemes)
library(ggiraph)
library(dplyr)
library(kableExtra)
library(ggplot2)
library(wesanderson)
library(formattable)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    
    titlePanel("Online Sales Anomaly Detection"),
    titlePanel(title=h3("Superstore Executive Summary - Profitability",align="center")),
    tableOutput("table"),
    hr(),
    
    titlePanel(title=h3("Local and Global Outliers", align="center")),

    
    
    fluidRow(column(4,
                    h4("Local Outlier Factor using Nearest Neighbour Method"),
                    girafeOutput("lofPlot")),
             column(4,
                    h4("Outlier Detection with outForest using Random Forest Method"),
                    girafeOutput("outPlot")),
             column(4,
                    h4("Density Based Spatial Clustering(DBSCAN) using Clustering "),
                    girafeOutput("dbsPlot"))
              ),
        hr(),
    
 
    
    fluidRow(
        
        
        column(3,
               h4("Select Items"),
               sliderInput('year', 'Order Year',
                           min=2014, max=2017, value=2015,
                           step=1, round=0)
        ),
        column(4, offset = 1,
               selectInput('axis1', 'X', c("profit_amt","sales_amt"),selected ="sales_amt"),
               selectInput('axis2', 'Y', c("profit_amt","sales_amt"),selected ="profit_amt")
        ),
        column(4,
               selectInput('region', 'U.S Region', c("All","Central","East","South","West"))
              
        )
    ),
    hr(),
    tags$div(em("Note:U.S Superstore dataset is sourced from"),tags$a(href="https://www.kaggle.com/juhi1994/superstore",em("Kaggle")) ,em(" The dataset have online orders for  Superstores in U.S. from 2014-2017. Tableau community is the owner of the dataset. The dataset has 9994 records and 21 attributes.", align="left"))
    
    
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    
    datsummary<-readRDS("datsummary.rds")
    
    
    datsummary %>%
        kbl() %>%
        kable_styling(bootstrap_options = "striped") %>% 
        row_spec(0, bold = T, color = "white", background = "#D7261E")
    
    superstore_unq<-readRDS("superstore_unq.rds")
    
    dat <- reactive({
        
        if (input$region == "All") return (superstore_unq<-superstore_unq %>% 
                                               filter(orderyear==input$year))
        
        else
        superstore_unq<-superstore_unq %>% 
            filter(orderyear==input$year) %>% 
            filter(region==input$region)
        
        
        superstore_unq
        
        
        
    })
    
    

    output$lofPlot <- renderGirafe({
        
        
        
        plot1<-ggplot(dat(),aes_string(x=input$axis1,y=input$axis2))+
            geom_point_interactive(alpha=.5,aes(shape = outlier_lofstatus, colour = outlier_lofstatus,tooltip=orderid,data_id=orderid),size=3)+theme_hc()+scale_color_manual(values=wes_palette(n=3, name="Darjeeling1"))
           
        
        css <- 'fill-opacity:.3;cursor:pointer;r:5pt;'
        ggiraph(code = print(plot1), 
                hover_css = css, 
                zoom_max = 2)
        
        
        
    })
    
    
    
    output$outPlot <- renderGirafe({
        
        
        
        plot2<-ggplot(dat(),aes_string(x=input$axis1,y=input$axis2))+
            geom_point_interactive(alpha=.5,aes(shape = outlier_outstatus, colour = outlier_outstatus,tooltip=orderid,data_id=orderid),size=3)+scale_color_manual(values=wes_palette(n=3, name="Darjeeling1"))+theme_hc()
        
        css <- 'fill-opacity:.3;cursor:pointer;r:5pt;'
        ggiraph(code = print(plot2), 
                hover_css = css, 
                zoom_max = 2)
        
        
        
    })
    
    
    
    output$dbsPlot <- renderGirafe({
        
        
        
        plot3<-ggplot(dat(),aes_string(x=input$axis1,y=input$axis2))+
            geom_point_interactive(alpha=.5,aes(shape = outlier_dbcstatus, colour = outlier_dbcstatus,tooltip=orderid,data_id=orderid),size=3)+theme_hc()+scale_color_manual(values=wes_palette(n=3, name="Darjeeling1"))
        
        css <- 'fill-opacity:.3;cursor:pointer;r:5pt;'
        ggiraph(code = print(plot3), 
                hover_css = css, 
                zoom_max = 2)
        
        
        
    })
    output$table<-function(){
        
       datsummary %>%
            knitr::kable("html") %>%
            kable_styling(bootstrap_options = "striped") %>% 
            row_spec(0, bold = T, color = "white", background = "#D7261E") 
        

        
        
    }
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
