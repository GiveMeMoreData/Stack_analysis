library(shiny)
options(stringsAsFactors = FALSE)
library('dplyr')
library('data.table')
library("stringi")
library("ggplot2")

wd <-"C:\\Users\\Bartek\\Desktop\\pd3\\"

                                                                  ##TAG ANALYSIS

shinyServer(function(input, output) {
    
    path <- reactive({switch (input$data,
                            "gaming"="gaming\\",
                            "datascience"="datascience\\",
                            "music"="music\\")})
    
    full_path <- reactive({paste0(wd,path())})
    
    index <- reactive(
      if(!stri_cmp_eq(input$textID,"")){
        which(
          Titles()%in%paste(unlist(strsplit(input$textID," ",fixed=TRUE)),collapse="."))}
      else{
          input$ID
        })
    
    checkPoints <- reactive(read.csv(paste0(full_path(),"checkPoints.csv"))%>%
                              lapply(FUN=function(y){as.Date(y)}))
    PostsPerMonth <- reactive(read.csv(paste0(full_path(),"PostsPerMonth.csv"))[[index()]])
    Titles <- reactive(colnames(read.csv(paste0(full_path(),"PostsPerMonth.csv"))))
    
    Date <- reactive(checkPoints()$x)
    
    output$plot <- renderPlot(
           ggplot(data=NULL,mapping=aes(x=Date(),y=PostsPerMonth()))+
             geom_line(colour="#54aee5",size=2)+
             xlab("Date")+
             ylab("Posts Per Month")+
                        
             ggtitle(paste(unlist(strsplit(Titles()[index()],".",fixed=TRUE)),collapse =" ")) +
             
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
    
    ## Hover
    output$hover_info <- renderPrint({
      cat("Hover info\n")
      cat("Date: ")
      cat(as.character(as.Date(as.numeric(input$plot_hover$x),origin="1970-01-01")))
      cat("\nNr of posts: ")
      cat(floor(as.numeric(input$plot_hover$y)))
    })
    
    ## Click
    output$click_info <- renderPrint({
      cat("Click info\n")
      cat("Date: ")
      cat(as.character(as.Date(as.numeric(input$plot_click$x),origin="1970-01-01")))
      cat("\nNr of posts: ")
      cat( floor(as.numeric(input$plot_click$y)))
    })
    
})



