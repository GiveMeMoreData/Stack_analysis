library(shiny)
library("shinythemes")

                                                          ##TAG ANALYSIS

shinyUI(
  fillPage( theme=shinytheme("lumen"),
  
    title="Tag Analisys",
    plotOutput("plot", height = "75%",
               hover = hoverOpts(id = "plot_hover"),
               click = "plot_click"
               ),

    
    fluidRow(style="margin-bottom: 50px;",height="25%",
    
    ## Informuje wybór zestawu danych
    column(3,offset=3,
           div(wellPanel(radioButtons("data",choices=c("Gaming"="gaming",
                             "Data Science"="datascience",
                             "Music"="music"), label = h3("Data set")))),style="font-size: 20px;"),
    
    ## Numer tagu do wyświetlenia
    
    column(3,
           div(
             wellPanel(numericInput("ID","Number of tag (by the most popular)",1),
    
    ## Lub ewentualnie nazwa tagu (eksperymentalnie)
    textInput("textID","Tag's name",placeholder = "Insert name here"))
    ,style="font-size: 20px;")
    ),
    column(width = 2, div(
             verbatimTextOutput("hover_info"),
             verbatimTextOutput("click_info"),style="margin-top: 10%;")
    )
    )


))
