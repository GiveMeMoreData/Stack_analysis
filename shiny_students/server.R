library(shiny)
options(stringsAsFactors = FALSE)
library('dplyr')
library('data.table')
library("stringi")
library("ggplot2")
# tu dane nie zależące od imputu

wd <-"C:\\Users\\Bartek\\Desktop\\pd3\\"



                                                             ### STUDENTS

## Loading data

#Gaming




shinyServer(function(input, output) {
    
  
    set <- reactive({
      switch (input$data,
              "Gaming"="gaming\\",
              "Data Science"="datascience\\",
              "Music"="music\\"
      )
    })
    
    plik <- reactive({
      switch (input$stage,
              "First look"="Resoults_Cor.csv",
              "cos"="BestResoults_Cor.csv",
              "Final"="BestResoults_2Cor.csv"
      )
    })
  
    dane <- reactive({
      as.data.frame(read.csv(paste0(wd,set(),plik())))
    })
    
    
    
    
    
    
    output$plot <- renderPlot(
      
           ggplot()+
             geom_histogram(aes(dane()[,input$method]),fill="#54aee5",color="black",size=1)+
             stat_bin(bins=20)+
             ylab("Count")+
             xlab(paste0(input$method,"'s rank correlation coefficient"))+
             labs(title=paste(input$data,input$stage,input$method,sep = " | "))+
             
             
             theme(plot.title = element_text(hjust=0.48,size=30,face = "bold"),
                   axis.title=element_text(hjust=0.48,size=25),
                   
                   panel.background = element_rect(fill = "#FFFFFF"),
                   panel.grid.major = element_line(size = 0.5, linetype = 'dashed',
                                                   colour = "grey"), 
                   panel.grid.minor = element_line(size = 0.25, linetype = 'dashed',
                                                   colour = "grey"),
                   axis.line = element_line(colour = "grey"),
                   axis.text = element_text(size=18),
                   plot.margin = margin(2,2,0.5,2,"cm")
             )
    )
    
    
    
    
    
})

