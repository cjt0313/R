#set up enviroment
library(tidyverse)
library(ggplot2)
library(clinfun)
library(shiny)
library(plotly)
library(shinyBS)
#build up the ui section
ui = fluidPage(
  #title
  titlePanel("Simon's Two-Stage Design"),
  fluidRow(
    column(3,
           #checkbox
           checkboxGroupInput("method", "Type of design",
                              choices = list("Minimax" = "Minimax", "Optimal" = "Optimal"),
                              selected = "Minimax")),
    column(3,
           #alpha
           numericInput("alpha",
                        h3("Nominal type I error rate:"), 
                        value = 0.05)),
    column(3,
           #beta
           numericInput("beta",
                        h3("Nominal type II error rate:"), 
                        value = 0.2))
  ),
  fluidRow(
    column(3,
           #range of p0
           sliderInput("p0", 
                       "Unacceptable response rate:",
                       min = 0.01, max = 0.50,
                       value = c(0.01,0.06))),
    column(3,
           #range of p1
           sliderInput("p1",
                       "Desirable response rate:",
                       min = 0.05,
                       max = 1,
                       value = c(0.4,0.5))),
    column(3,
           #stepsize of the p0 sequence
           sliderInput("stepp0", 
                       "p0_step_size:",
                       min = 0.01, max = 0.1,
                       value = 0.03)),
    column(3,
           #stepsize of the p1 sequence
           sliderInput("stepp1", 
                       "p1_step_size:",
                       min = 0.01, max = 0.1,
                       value = 0.05))
  ),
  #downloadbutton
  downloadButton("downloadData", "Download"),
  #plots
  plotlyOutput("plot"),
  #table
  tableOutput("df")
  
)

#build up the server
server = function(input,output){
  select_nmax_simon = function(p0,p1,alpha,beta)
    {for (i in seq(100,800,100)) {
    tryCatch(return(ph2simon(p0,p1,alpha,beta,i)),
             warning = function(w){},
             error = function(e){})
    }}
  p2_compute = function(p0,p1,alpha,beta,method){
    p2_df = select_nmax_simon(p0,p1,alpha,beta)$out 
    p2_df = data.frame(p2_df)
    if (method == "Minimax"){
      p2_stats = p2_df[1,]
    } else {
      p2_stats = subset(p2_df,p2_df$EN.p0.==min(p2_df$EN.p0.))
    }
    p2_stats$p0 = p0
    p2_stats$p1 = p1
    p2_stats$obs.p = round(p2_stats$r/p2_stats$n,3)
    p2_stats$obs.p1 = round(p2_stats$r1/p2_stats$n1,3)
    ci = binom.test(p2_stats$r, p2_stats$n, p=p0, alternative="two.sided", conf.level=0.95)
    p2_stats$ci95low = round(ci$conf.int[1], 3)
    p2_stats$ci95high = round(ci$conf.int[2], 3)
    p2_stats$alpha = alpha
    p2_stats$power = 1-beta
    p2_stats = subset(p2_stats,select = -c(EN.p0.,PET.p0.))
    return(p2_stats)
  }
  p2_df = function(seq1,seq2,alpha,beta,method){
    p2df = data.frame()
    for (i in 1:length(seq1)) {
      for (j in 1:length(seq2)) {
        df_i_j = p2_compute(seq1[i],seq2[j],alpha,beta,method)
        p2df = rbind(p2df,df_i_j)}}
    p2df$type = method
    return(p2df)
  }
  
  #table
  output$df = renderTable({
    if(length(input$method) ==1){    if (input$method == "Minimax"){
      p2_df(seq(input$p0[1],input$p0[2],input$stepp0),seq(input$p1[1],input$p1[2],input$stepp1),input$alpha/2,input$beta,
            "Minimax")
      }
    else if(input$method == "Optimal"){
      p2_df(seq(input$p0[1],input$p0[2],input$stepp0),seq(input$p1[1],input$p1[2],input$stepp1),input$alpha/2,input$beta,
            "Optimal")}}
    else if (length(input$method) ==2)
      {rbind(p2_df(seq(input$p0[1],input$p0[2],input$stepp0),seq(input$p1[1],input$p1[2],input$stepp1),input$alpha/2,input$beta,
                     "Minimal"),p2_df(seq(input$p0[1],input$p0[2],input$stepp0),seq(input$p1[1],input$p1[2],input$stepp1),input$alpha/2,input$beta,
                                      "Optimal"))}
    })
  #line plots
  output$plot = renderPlotly({
    if (length(input$method) ==1){
      if (input$method == "Minimax"){
      p2df = p2_df(seq(input$p0[1],input$p0[2],input$stepp0),seq(input$p1[1],input$p1[2],input$stepp1),input$alpha/2,input$beta,
                   "Minimax")
      p2df$p0 = as.factor(p2df$p0)
      ggplotly(ggplot(data = p2df,aes (x = p1,y=n,z = p0,r = r,r1 =r1,n1 =n1,obsp = obs.p,obsp1 = obs.p1))
               +geom_line(aes(group = p0,color = p0))+geom_point(),tooltip = "all")}
    else{
      p2df = p2_df(seq(input$p0[1],input$p0[2],input$stepp0),seq(input$p1[1],input$p1[2],input$stepp1),input$alpha/2,input$beta,
                   "Optimal")
      p2df$p0 = as.factor(p2df$p0)
      ggplotly(ggplot(data = p2df,aes (x = p1,y=n,z = p0,r = r,r1 =r1,n1 =n1,obsp = obs.p,obsp1 = obs.p1))
               +geom_line(aes(group = p0,color = p0),linetype="dashed")+geom_point(),tooltip = "all")}
    }
    else {
      p2df = rbind(p2_df(seq(input$p0[1],input$p0[2],input$stepp0),seq(input$p1[1],input$p1[2],input$stepp1),input$alpha/2,input$beta,
                         "Minimax"),p2_df(seq(input$p0[1],input$p0[2],input$stepp0),seq(input$p1[1],input$p1[2],input$stepp1),input$alpha/2,input$beta,
                                          "Optimal"))
      p2df$p0 = as.factor(p2df$p0)
      p2df$type = as.factor(p2df$type)
      ggplotly(ggplot(data = p2df,aes (x = p1,y=n,z = p0,r = r,r1 =r1,n1 =n1,obsp = obs.p,obsp1 = obs.p1,group =interaction(p0,type) ))
               +geom_line(aes(group = interaction(p0,type),color = p0,linetype=type))
                          +geom_point())
    }
    
  })
  #save the table as a .csv
  data = reactive(if(length(input$method) ==1){if (input$md == "Minimax"){
      dat = p2_df(seq(input$p0[1],input$p0[2],input$stepp0),seq(input$p1[1],input$p1[2],input$stepp1),input$alpha/2,input$beta,
            "Minimax")
    }
      else if(input$method == "Optimal"){
       dat= p2_df(seq(input$p0[1],input$p0[2],input$stepp0),seq(input$p1[1],input$p1[2],input$stepp1),input$alpha/2,input$beta,
              "Optimal")}}
    else if (length(input$method) ==2)
    {dat = rbind(p2_df(seq(input$p0[1],input$p0[2],input$stepp0),seq(input$p1[1],input$p1[2],input$stepp1),input$alpha/2,input$beta,
                 "Minimal"),p2_df(seq(input$p0[1],input$p0[2],input$stepp0),seq(input$p1[1],input$p1[2],input$stepp1),input$alpha/2,input$beta,
                                  "Optimal"))})
   
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data(), file)
    }
  )  }

#run
shinyApp(ui=ui, server=server)




