library(shiny)
library(poppr)
df<-read.table("reduced_database.txt.csv", header=TRUE, sep="\t")
df.m<-as.matrix(df)
newrow<-c()

shinyServer(function(input, output) {
 
  treeInput <- reactive({
    switch(input$tree,
           "upgma" = "upgma",
           "nj" = "nj"
           )
  })

output$distPlotTree <- renderPlot({
  newrow <<- c("query","query",input$mst1,input$mst2,input$mst3,input$mst4,input$mst5,input$mst6,input$mst7,input$mst8,input$mst9)
  df.m <- rbind(df.m,newrow,deparse.level=0)
  df.m <- as.data.frame(df.m)
  gen <- df2genind(df.m[, -c(1,2)], ploid=2, sep="/", pop=df.m[, 2], ind.names=df.m[, 1])
  if (input$boot > 1000){
    return("No Plots Allowed.")
  }
  else{
  a<-bruvo.boot(gen, sample=input$boot, tree=input$tree)
  plot(a)
  tiplabels(gen$pop,  adj=c(-6, 0.5), frame="none")
  }
})
  
output$downloadData <- downloadHandler(
  filename = function() { paste(input$tree, '.tre', sep='') },
  content = function(file) {
    write.tree(a, file)
  })

output$downloadPdf <- downloadHandler(
  filename = function() { paste(input$tree, '.pdf', sep='') },
  content = function(file) {
    pdf(file,width=6,height=4,paper='letter')
    plot(a, show.node.label=TRUE)
    if (input$tree =="upgma")
      axisPhylo(3)
    dev.off()
  })

output$MinSpanTree <- renderPlot({
  newrow <<- c("query","query",input$mst1,input$mst2,input$mst3,input$mst4,input$mst5,input$mst6,input$mst7,input$mst8,input$mst9)
  df.m <- rbind(df.m,newrow,deparse.level=0)
  df.m <- as.data.frame(df.m)
  gen <- df2genind(df.m[, -c(1,2)], ploid=2, sep="/", pop=df.m[, 2], ind.names=df.m[, 1])
  bruvo.msn(gen)
})

})
