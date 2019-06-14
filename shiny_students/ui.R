library(shiny)
library("shinythemes")

# Define UI for miles per gallon application

                                                                            ### STUDENTS

shinyUI(
  fillPage( theme=shinytheme("lumen"),
  
    title="Tag Analisys",
    plotOutput("plot", height = "75%",
               hover = hoverOpts(id = "plot_hover"),
               click = "plot_click"
               ),

    
    fluidRow(style="margin-bottom: 50px;",height="25%",
    
    ## Informuje wybór zestawu danych
    column(2,offset=3,
           div(wellPanel(radioButtons("data",choices=c("Gaming",
                                                       "Data Science",
                                                       "Music"), label = h3("Data set")))),style="font-size: 20px;"),
    
    # Etap analizy
    
    column(2,
           div(wellPanel(radioButtons("stage",choices=c("First look",
                                                        "Adjusted",
                                                        "Final"), label = h3("Stage")))),style="font-size: 20px;"),
    ## Numer tagu do wyświetlenia
    
    column(2,
           div(wellPanel(radioButtons("method",choices=c("Pearson",
                                                         "Spearman",
                                                         "Kendall"), label = h3("Method")))),style="font-size: 20px;")
    )
        )


)
