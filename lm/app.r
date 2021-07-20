#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Linear Model"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(

            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            # Input: Select number of rows to display ----
            actionButton("lmPlot", "Linear regression"),
            downloadButton('downloadPlot', 'Download Plot')
            #downloadButton('downloadPlot', 'Download Plot')
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("lmPlot"),
           h4("Rsquared"),
           textOutput("Rsquared"),
           h4("Slope"),
           textOutput("Slope"),
           h4("Intercept"),
           textOutput("Intercept"),
           plotOutput('plot')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #     print(bins)
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
    # 

    
    output$distPlot <- renderPlot({
        plot(dataInput()$x,dataInput()$y)
    
    })
    LinearModel <- eventReactive(input$lmPlot, {
        y <- dataInput()$y
        x <- dataInput()$x
        lmPlot <- lm(y ~ x)
    })
    output$lmPlot <- renderPlot({
        #plot(dataInput()$x,dataInput()$y)
        #abline(LinearModel(), col = "red", lwd = 2)
        lmgraph()
    })
    
    lmgraph <- function(){
        plot(dataInput()$x,dataInput()$y)
        abline(LinearModel(), col = "red", lwd = 2)
    }
    output$summary<- renderText({
 
        
        paste("Adj R2=", signif(summary(LinearModel())$adj.r.squared,5), "\n",
              "Intercept = ", signif(LinearModel()$coef[[1]],5), "\n", 
              "Slope = ", signif(LinearModel()$coef[[2]], 5))
   
        
        
    })
    output$Rsquared<- renderText({
        paste("Adj R2=", signif(summary(LinearModel())$adj.r.squared,5)
              )
        
    })
    output$Slope<- renderText({
        paste("Slope = ", signif(LinearModel()$coef[[2]], 5)
        )
        
    })

    output$Intercept<- renderText({
        paste("Intercept = ", signif(LinearModel()$coef[[1]],5)
        )
        
    })    

    output$downloadPlot <- downloadHandler(
        filename = function() { paste(input$file1$datapath, '.png', sep='') },
        content = function(file) {
            device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
            png(file)
            lmgraph()
            dev.off()
        }
    )
    
   
        
}

# Run the application 
shinyApp(ui = ui, server = server)
