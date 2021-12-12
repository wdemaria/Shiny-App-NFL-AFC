### NFL AFC East Shiny project
### December 2021

library(rsconnect)
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(visreg)
library(caret)
library(plsdepot)
library(jtools)
library(kableExtra)

#36,43,+RPO_yds.play+Play_Action_yds.att
### App code
alldata <- read.csv("NFL.csv")
data_2021 <- alldata %>%
  filter(Year=="2021")
mydata = alldata[5:48,]

ui <- fluidPage(theme = shinytheme("cosmo"),
                titlePanel("NFL AFC East Stats: My Shiny App"),
                sidebarLayout(
                  sidebarPanel(
                    
                    helpText("Select your preferred metric."),
                    selectInput("iv", "Select Metric:", c("", "Yards/Play", 
                              "Fumbles", "Interceptions", "Penalties","Yards/Pass",
                              "Yards/Rush","RPO Yards/Play","Play Action Yards/Attempt")),
                    
                    helpText("Check to see 2021 Predictions based on all metrics
                             except RPO and Play Action data (not enough data available"),
                    checkboxInput("pred","Predictions"),
                    verbatimTextOutput("pred"),
                    
                    tags$br(),
                    h4("Data Sources (external sites):"),
                    uiOutput("AFC_stats")
                    
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Plots", plotOutput("plot"),plotOutput("forecast"),verbatimTextOutput("model")), 
                      tabPanel("Effect",plotOutput("effect")),
                      tabPanel("Summary", verbatimTextOutput("summary"))  
                    )
                  )
                )
)


#Function for Visualizing Results#
server <- function(input, output) {

  #Renders scatter plots based on selected metric 
  output$plot <- renderPlot({
  if (input$iv == "Yards/Play") {
    data_yp <- mydata %>%
      select(Tm,Y.P,W.L)
      trainrows_yp = sample(1:48, 24)
      traindata_yp = data_yp[trainrows_yp,]
      testdata_yp = data_yp[-trainrows_yp,]
        
      mod_y = lm(W.L ~ Y.P, data = data_yp)
      
      pats_yp <- data_yp %>%
        filter(Tm=="New England Patriots")
      bills_yp <- data_yp %>%
        filter(Tm=="Buffalo Bills")
      jets_yp <- data_yp %>%
        filter(Tm=="New York Jets")
      fins_yp <- data_yp %>%
        filter(Tm=="Miami Dolphins")
      teams = c("New England Patriots","Buffalo Bills","New York Jets","Miami Dolphins")
      
      plot_y <- ggplot(data_yp) + 
        geom_point(data=pats_yp, aes(x = Y.P, y = W.L,size=6,colour="blue",show.legend=TRUE)) +
        geom_point(data=bills_yp, aes(x = Y.P, y = W.L,size=6,colour="red",show.legend=TRUE)) +
        geom_point(data=jets_yp, aes(x = Y.P, y = W.L,size=6,colour="green",show.legend=TRUE)) +
        geom_point(data=fins_yp,aes(x = Y.P, y = W.L,size=6,colour="pink",show.legend=TRUE)) +
        geom_abline(intercept = mod_y[1]$coefficients[1], slope = mod_y[1]$coefficients[2], color = "red" ) +
        xlim(0,8) + ylim(0,1) + xlab("Yards/Play") + ylab("% Games Won") +
        scale_colour_manual(name = 'Legend',
                            values=c("blue"="blue","red"="red","green"="green","pink"="pink"),
                            guide = 'legend',
                            labels = teams)
      plot_y
      } else
        if (input$iv == "Fumbles") {
          data_fl <- mydata %>%
            select(Tm,FL,W.L)
          trainrows_fl = sample(1:48, 24)
          traindata_fl = data_fl[trainrows_fl,]
          testdata_fl = data_fl[-trainrows_fl,]
          
          mod_fl= lm(W.L ~ FL, data = data_fl)
          
          pats_fl <- data_fl %>%
            filter(Tm=="New England Patriots")
          bills_fl <- data_fl %>%
            filter(Tm=="Buffalo Bills")
          jets_fl <- data_fl %>%
            filter(Tm=="New York Jets")
          fins_fl <- data_fl %>%
            filter(Tm=="Miami Dolphins")
          teams = c("New England Patriots","Buffalo Bills","New York Jets","Miami Dolphins")
          
          plot_fl <- ggplot(data_fl) + 
            geom_point(data=pats_fl, aes(x = FL, y = W.L,size=6,colour="blue",show.legend=TRUE)) +
            geom_point(data=bills_fl, aes(x = FL, y = W.L,size=6,colour="red",show.legend=TRUE)) +
            geom_point(data=jets_fl, aes(x = FL, y = W.L,size=6,colour="green",show.legend=TRUE)) +
            geom_point(data=fins_fl,aes(x = FL, y = W.L,size=6,colour="pink",show.legend=TRUE)) +
            geom_abline(intercept = mod_fl[1]$coefficients[1], slope = mod_fl[1]$coefficients[2], color = "red" ) +
            xlim(0,15) + ylim(0,1) + xlab("Fumbles") + ylab("% Games Won") +
            scale_colour_manual(name = 'Legend',
                                values=c("blue"="blue","red"="red","green"="green","pink"="pink"),
                                guide = 'legend',
                                labels = teams)
          plot_fl
        } else
          if (input$iv == "Interceptions") {
            data_int <- mydata %>%
              select(Tm,Int_pass,W.L)
            trainrows_int = sample(1:48, 24)
            traindata_int = data_int[trainrows_int,]
            testdata_int = data_int[-trainrows_int,]
            
            mod_int = lm(W.L ~ Int_pass, data = data_int)
            
            pats_int <- data_int %>%
              filter(Tm=="New England Patriots")
            bills_int <- data_int %>%
              filter(Tm=="Buffalo Bills")
            jets_int <- data_int %>%
              filter(Tm=="New York Jets")
            fins_int <- data_int %>%
              filter(Tm=="Miami Dolphins")
            teams = c("New England Patriots","Buffalo Bills","New York Jets","Miami Dolphins")
            
            plot_int <- ggplot(data_int) + 
              geom_point(data=pats_int, aes(x = Int_pass, y = W.L,size=6,colour="blue",show.legend=TRUE)) +
              geom_point(data=bills_int, aes(x = Int_pass, y = W.L,size=6,colour="red",show.legend=TRUE)) +
              geom_point(data=jets_int, aes(x = Int_pass, y = W.L,size=6,colour="green",show.legend=TRUE)) +
              geom_point(data=fins_int,aes(x = Int_pass, y = W.L,size=6,colour="pink",show.legend=TRUE)) +
              geom_abline(intercept = mod_int[1]$coefficients[1], slope = mod_int[1]$coefficients[2], color = "red" ) +
              xlim(0,30) + ylim(0,1) + xlab("Interceptions") + ylab("% Games Won") +
              scale_colour_manual(name = 'Legend',
                                  values=c("blue"="blue","red"="red","green"="green","pink"="pink"),
                                  guide = 'legend',
                                  labels = teams)
            plot_int
          } else
            if (input$iv == "Penalties") {
              data_pen <- mydata %>%
                select(Tm,Pen,W.L)
              trainrows_pen = sample(1:48, 24)
              traindata_pen = data_pen[trainrows_pen,]
              testdata_pen = data_pen[-trainrows_pen,]
              
              mod_pen = lm(W.L ~ Pen, data = data_pen)
              
              pats_pen <- data_pen %>%
                filter(Tm=="New England Patriots")
              bills_pen <- data_pen %>%
                filter(Tm=="Buffalo Bills")
              jets_pen <- data_pen %>%
                filter(Tm=="New York Jets")
              fins_pen <- data_pen %>%
                filter(Tm=="Miami Dolphins")
              teams = c("New England Patriots","Buffalo Bills","New York Jets","Miami Dolphins")
              
              plot_pen <- ggplot(data_pen) + 
                geom_point(data=pats_pen, aes(x = Pen, y = W.L,size=6,colour="blue",show.legend=TRUE)) +
                geom_point(data=bills_pen, aes(x = Pen, y = W.L,size=6,colour="red",show.legend=TRUE)) +
                geom_point(data=jets_pen, aes(x = Pen, y = W.L,size=6,colour="green",show.legend=TRUE)) +
                geom_point(data=fins_pen,aes(x = Pen, y = W.L,size=6,colour="pink",show.legend=TRUE)) +
                geom_abline(intercept = mod_pen[1]$coefficients[1], slope = mod_pen[1]$coefficients[2], color = "red" ) +
                xlim(0,150) + ylim(0,1) +xlab("Penalties") + ylab("% Games Won") +
                scale_colour_manual(name = 'Legend',
                                    values=c("blue"="blue","red"="red","green"="green","pink"="pink"),
                                    guide = 'legend',
                                    labels = teams)
              plot_pen
            } else
              if (input$iv == "Yards/Pass") {
                data_ypass <- mydata %>%
                  select(Tm,NY.A_pass,W.L)
                trainrows_ypass = sample(1:48, 24)
                traindata_ypass = data_ypass[trainrows_ypass,]
                testdata_ypass = data_ypass[-trainrows_ypass,]
                
                mod_ypass = lm(W.L ~ NY.A_pass, data = data_ypass)
                
                pats_ypass <- data_ypass %>%
                  filter(Tm=="New England Patriots")
                bills_ypass <- data_ypass %>%
                  filter(Tm=="Buffalo Bills")
                jets_ypass <- data_ypass %>%
                  filter(Tm=="New York Jets")
                fins_ypass <- data_ypass %>%
                  filter(Tm=="Miami Dolphins")
                teams = c("New England Patriots","Buffalo Bills","New York Jets","Miami Dolphins")
                
                plot_ypass <- ggplot(data_ypass) + 
                  geom_point(data=pats_ypass, aes(x = NY.A_pass, y = W.L,size=6,colour="blue",show.legend=TRUE)) +
                  geom_point(data=bills_ypass, aes(x = NY.A_pass, y = W.L,size=6,colour="red",show.legend=TRUE)) +
                  geom_point(data=jets_ypass, aes(x = NY.A_pass, y = W.L,size=6,colour="green",show.legend=TRUE)) +
                  geom_point(data=fins_ypass,aes(x = NY.A_pass, y = W.L,size=6,colour="pink",show.legend=TRUE)) +
                  geom_abline(intercept = mod_ypass[1]$coefficients[1], slope = mod_ypass[1]$coefficients[2], color = "red" ) +
                  xlim(0,10) + ylim(0,1) +xlab("Yards/Pass") + ylab("% Games Won") +
                  scale_colour_manual(name = 'Legend',
                                      values=c("blue"="blue","red"="red","green"="green","pink"="pink"),
                                      guide = 'legend',
                                      labels = teams)
                plot_ypass
              } else
                if (input$iv == "Yards/Rush") {
                  data_yrush <- mydata %>%
                    select(Tm,Y.A_rush,W.L)
                  trainrows_yrush = sample(1:48, 24)
                  traindata_yrush = data_yrush[trainrows_yrush,]
                  testdata_yrush = data_yrush[-trainrows_yrush,]
                  
                  mod_yrush = lm(W.L ~ Y.A_rush, data = data_yrush)
                  
                  pats_yrush <- data_yrush %>%
                    filter(Tm=="New England Patriots")
                  bills_yrush <- data_yrush %>%
                    filter(Tm=="Buffalo Bills")
                  jets_yrush <- data_yrush %>%
                    filter(Tm=="New York Jets")
                  fins_yrush <- data_yrush %>%
                    filter(Tm=="Miami Dolphins")
                  teams = c("New England Patriots","Buffalo Bills","New York Jets","Miami Dolphins")
                  
                  plot_yrush <- ggplot(data_yrush) + 
                    geom_point(data=pats_yrush, aes(x = Y.A_rush, y = W.L,size=6,colour="blue",show.legend=TRUE)) +
                    geom_point(data=bills_yrush, aes(x = Y.A_rush, y = W.L,size=6,colour="red",show.legend=TRUE)) +
                    geom_point(data=jets_yrush, aes(x = Y.A_rush, y = W.L,size=6,colour="green",show.legend=TRUE)) +
                    geom_point(data=fins_yrush,aes(x = Y.A_rush, y = W.L,size=6,colour="pink",show.legend=TRUE)) +
                    geom_abline(intercept = mod_yrush[1]$coefficients[1], slope = mod_yrush[1]$coefficients[2], color = "red" ) +
                    xlim(0,10) + ylim(0,1) +xlab("Yards/Rush") + ylab("% Games Won") +
                    scale_colour_manual(name = 'Legend',
                                        values=c("blue"="blue","red"="red","green"="green","pink"="pink"),
                                        guide = 'legend',
                                        labels = teams)
                  plot_yrush
                } else
                  if (input$iv == "RPO Yards/Play") {
                    data_rpo <- mydata %>%
                      select(Tm,RPO_yds.play,W.L)
                    trainrows_rpo = sample(1:48, 24)
                    traindata_rpo = data_rpo[trainrows_rpo,]
                    testdata_rpo = data_rpo[-trainrows_rpo,]
                    
                    mod_rpo = lm(W.L ~ RPO_yds.play, data = data_rpo)
                    
                    pats_rpo <- data_rpo %>%
                      filter(Tm=="New England Patriots")
                    bills_rpo <- data_rpo %>%
                      filter(Tm=="Buffalo Bills")
                    jets_rpo <- data_rpo %>%
                      filter(Tm=="New York Jets")
                    fins_rpo <- data_rpo %>%
                      filter(Tm=="Miami Dolphins")
                    teams = c("New England Patriots","Buffalo Bills","New York Jets","Miami Dolphins")
                    
                    plot_rpo <- ggplot(data_rpo) + 
                      geom_point(data=pats_rpo, aes(x = RPO_yds.play, y = W.L,size=6,colour="blue",show.legend=TRUE)) +
                      geom_point(data=bills_rpo, aes(x = RPO_yds.play, y = W.L,size=6,colour="red",show.legend=TRUE)) +
                      geom_point(data=jets_rpo, aes(x = RPO_yds.play, y = W.L,size=6,colour="green",show.legend=TRUE)) +
                      geom_point(data=fins_rpo,aes(x = RPO_yds.play, y = W.L,size=6,colour="pink",show.legend=TRUE)) +
                      geom_abline(intercept = mod_rpo[1]$coefficients[1], slope = mod_rpo[1]$coefficients[2], color = "red" ) +
                      xlim(0,10) + ylim(0,1) +xlab("RPO Yards/Play") + ylab("% Games Won") +
                      scale_colour_manual(name = 'Legend',
                                          values=c("blue"="blue","red"="red","green"="green","pink"="pink"),
                                          guide = 'legend',
                                          labels = teams)
                    plot_rpo
                  } else
                    if (input$iv == "Play Action Yards/Attempt") {
                      data_pa <- mydata %>%
                        select(Tm,Play_Action_yds.att,W.L)
                      trainrows_pa = sample(1:48, 24)
                      traindata_pa = data_pa[trainrows_pa,]
                      testdata_pa = data_pa[-trainrows_pa,]
                      
                      mod_pa = lm(W.L ~ Play_Action_yds.att, data = data_pa)
                      
                      pats_pa <- data_pa %>%
                        filter(Tm=="New England Patriots")
                      bills_pa <- data_pa %>%
                        filter(Tm=="Buffalo Bills")
                      jets_pa <- data_pa %>%
                        filter(Tm=="New York Jets")
                      fins_pa <- data_pa %>%
                        filter(Tm=="Miami Dolphins")
                      teams = c("New England Patriots","Buffalo Bills","New York Jets","Miami Dolphins")
                      
                      plot_pa <- ggplot(data_pa) + 
                        geom_point(data=pats_pa, aes(x = Play_Action_yds.att, y = W.L,size=6,colour="blue",show.legend=TRUE)) +
                        geom_point(data=bills_pa, aes(x = Play_Action_yds.att, y = W.L,size=6,colour="red",show.legend=TRUE)) +
                        geom_point(data=jets_pa, aes(x = Play_Action_yds.att, y = W.L,size=6,colour="green",show.legend=TRUE)) +
                        geom_point(data=fins_pa,aes(x = Play_Action_yds.att, y = W.L,size=6,colour="pink",show.legend=TRUE)) +
                        geom_abline(intercept = mod_pa[1]$coefficients[1], slope = mod_pa[1]$coefficients[2], color = "red" ) +
                        xlim(0,10) + ylim(0,1) +xlab("Play Action Yards/Attempt") + ylab("% Games Won") +
                        scale_colour_manual(name = 'Legend',
                                            values=c("blue"="blue","red"="red","green"="green","pink"="pink"),
                                            guide = 'legend',
                                            labels = teams)
                      plot_pa
                    }
  })
  output$forecast <- renderPlot({
    if (input$pred) {
      mod = lm(W.L ~ Y.P+FL+Int_pass+Pen+NY.A_pass+Y.A_rush, data = mydata[,c(6,13,15,21,22,27,29)])
      preds=data.frame(predict(mod,data_2021[,c(13,15,21,22,27,29)],level=0.95,interval="confidence"))
      plot <- ggplot(preds) +
        geom_point(data=data_2021,aes(x=W.L,y=preds$fit,size=6,color=factor(Tm))) +
        labs(x="Current % Games Won",y="Predicted % Games Won",color="Team") +xlim(0,1)+ylim(0,1)+
        ggtitle("Current % Games Won vs. Predicted % Games Won in 2021")
      plot
    }
  })
  output$model <- renderPrint({
    if (input$pred) {
    mod = lm(W.L ~ Y.P+FL+Int_pass+Pen+NY.A_pass+Y.A_rush, data = mydata[,c(6,13,15,21,22,27,29)])
    preds=data.frame(predict(mod,data_2021[,c(13,15,21,22,27,29)],level=0.95,interval="confidence"))
    summ=summ(mod)
    summ
    }
  })
  #Renders effect plot based on selected metric
  output$effect <- renderPlot({
    if (input$iv == "Yards/Play") {
      data_yp <- mydata %>%
        select(Tm,Y.P,W.L)
      trainrows_yp = sample(1:48, 24)
      traindata_yp = data_yp[trainrows_yp,]
      testdata_yp = data_yp[-trainrows_yp,]
      
      mod_y = lm(W.L ~ Y.P, data = data_yp)
      effect_plot(mod_y,pred=Y.P,interval=TRUE,plot.points=TRUE)
    } else
      if(input$iv == "Fumbles") {
        data_fl <- mydata %>%
          select(Tm,FL,W.L)
      trainrows_fl = sample(1:48, 24)
      traindata_fl = data_fl[trainrows_fl,]
      testdata_fl = data_fl[-trainrows_fl,]
      
      mod_fl = lm(W.L ~ FL, data = data_fl)
      effect_plot(mod_fl,pred=FL,interval=TRUE,plot.points=TRUE)
      } else
        if(input$iv == "Interceptions") {
          data_int <- mydata %>%
            select(Tm,Int_pass,W.L)
          trainrows_int = sample(1:48, 24)
          traindata_int = data_int[trainrows_int,]
          testdata_int = data_int[-trainrows_int,]
          
          mod_int = lm(W.L ~ Int_pass, data = data_int)
          effect_plot(mod_int,pred=Int_pass,interval=TRUE,plot.points=TRUE)
        } else
          if(input$iv == "Penalties") {
            data_pen <- mydata %>%
              select(Tm,Pen,W.L)
            trainrows_pen= sample(1:48, 24)
            traindata_pen = data_pen[trainrows_pen,]
            testdata_pen= data_pen[-trainrows_pen,]
            
            mod_pen = lm(W.L ~ Pen, data = data_pen)
            effect_plot(mod_pen,pred=Pen,interval=TRUE,plot.points=TRUE)
          } else
            if(input$iv == "Yards/Pass") {
              data_ypass <- mydata %>%
                select(Tm,NY.A_pass,W.L)
              trainrows_ypass = sample(1:48, 24)
              traindata_ypass = data_ypass[trainrows_ypass,]
              testdata_ypass = data_ypass[-trainrows_ypass,]
              
              mod_ypass = lm(W.L ~ NY.A_pass, data = data_ypass)
              effect_plot(mod_ypass,pred=NY.A_pass,interval=TRUE,plot.points=TRUE)
            } else
            if(input$iv == "Yards/Rush") {
              data_yrush <- mydata %>%
                select(Tm,Y.A_rush,W.L)
              trainrows_yrush= sample(1:48, 24)
              traindata_yrush= data_yrush[trainrows_yrush,]
              testdata_yrush= data_yrush[-trainrows_yrush,]
            
              mod_yrush = lm(W.L ~ Y.A_rush, data = data_yrush)
              effect_plot(mod_yrush,pred=Y.A_rush,interval=TRUE,plot.points=TRUE)
          } else
            if (input$iv == "RPO Yards/Play") {
              data_rpo <- mydata %>%
                select(Tm,RPO_yds.play,W.L)
              trainrows_rpo = sample(1:48, 24)
              traindata_rpo = data_rpo[trainrows_rpo,]
              testdata_rpo = data_rpo[-trainrows_rpo,]
    
              mod_rpo = lm(W.L ~ RPO_yds.play, data = data_rpo)
              effect_plot(mod_rpo,RPO_yds.play,interval=TRUE,plot.points=TRUE)
          } else 
            if (input$iv == "Play Action Yards/Attempt") {
              data_pa <- mydata %>%
                select(Tm,Play_Action_yds.att,W.L)
              trainrows_pa = sample(1:48, 24)
              traindata_pa = data_pa[trainrows_pa,]
              testdata_pa = data_pa[-trainrows_pa,]
              
              mod_pa = lm(W.L ~ Play_Action_yds.att, data = data_pa)
              effect_plot(mod_pa,Play_Action_yds.att,interval=TRUE,plot.points=TRUE)
            }
  })
  #Renders table summary based on selected metric
  output$summary <- renderPrint({
    if (input$iv == "Yards/Play") {
      data_yp <- mydata %>%
        select(Tm,Y.P,W.L)
      trainrows_yp = sample(1:48, 24)
      traindata_yp = data_yp[trainrows_yp,]
      testdata_yp = data_yp[-trainrows_yp,]
      
      mod_y = lm(W.L ~ Y.P, data = data_yp)
      s_y=summ(mod_y)
      s_y
    } else
      if(input$iv == "Fumbles") {
        data_fl <- mydata %>%
          select(Tm,FL,W.L)
        trainrows_fl = sample(1:48, 24)
        traindata_fl = data_fl[trainrows_fl,]
        testdata_fl = data_fl[-trainrows_fl,]
        
        mod_fl = lm(W.L ~ FL, data = data_fl)
        s_fl=summ(mod_fl)
        s_fl
      } else
        if(input$iv == "Interceptions") {
          data_int <- mydata %>%
            select(Tm,Int_pass,W.L)
          trainrows_int = sample(1:48, 24)
          traindata_int = data_int[trainrows_int,]
          testdata_int = data_int[-trainrows_int,]
          
          mod_int = lm(W.L ~ Int_pass, data = data_int)
          s_int=summ(mod_int)
          s_int
        } else
          if(input$iv == "Penalties") {
            data_pen <- mydata %>%
              select(Tm,Pen,W.L)
            trainrows_pen= sample(1:48, 24)
            traindata_pen = data_pen[trainrows_pen,]
            testdata_pen= data_pen[-trainrows_pen,]
            
            mod_pen = lm(W.L ~ Pen, data = data_pen)
            s_pen=summ(mod_pen)
            s_pen
          } else
            if(input$iv == "Yards/Pass") {
              data_ypass <- mydata %>%
                select(Tm,NY.A_pass,W.L)
              trainrows_ypass = sample(1:48, 24)
              traindata_ypass = data_ypass[trainrows_ypass,]
              testdata_ypass = data_ypass[-trainrows_ypass,]
              
              mod_ypass = lm(W.L ~ NY.A_pass, data = data_ypass)
              s_ypass=summ(mod_ypass)
              s_ypass
            } else
              if(input$iv == "Yards/Rush") {
                data_yrush <- mydata %>%
                  select(Tm,Y.A_rush,W.L)
                trainrows_yrush= sample(1:48, 24)
                traindata_yrush= data_yrush[trainrows_yrush,]
                testdata_yrush= data_yrush[-trainrows_yrush,]
                
                mod_yrush = lm(W.L ~ Y.A_rush, data = data_yrush)
                s_yrush=summ(mod_yrush)
                s_yrush
              } else
                if (input$iv == "RPO Yards/Play") {
                  data_rpo <- mydata %>%
                    select(Tm,RPO_yds.play,W.L)
                  trainrows_rpo = sample(1:48, 24)
                  traindata_rpo = data_rpo[trainrows_rpo,]
                  testdata_rpo = data_rpo[-trainrows_rpo,]
                  
                  mod_rpo = lm(W.L ~ RPO_yds.play, data = data_rpo)
                  s_rpo=summ(mod_rpo)
                  s_rpo
                } else 
                  if (input$iv == "Play Action Yards/Attempt") {
                    data_pa <- mydata %>%
                      select(Tm,Play_Action_yds.att,W.L)
                    trainrows_pa = sample(1:48, 24)
                    traindata_pa = data_pa[trainrows_pa,]
                    testdata_pa = data_pa[-trainrows_pa,]
                    
                    mod_pa = lm(W.L ~ Play_Action_yds.att, data = data_pa)
                    s_pa=summ(mod_pa)
                    s_pa
                  }
  })
  #Link to data source#
  url1 <- a("Link", href="https://www.pro-football-reference.com/", target = "_blank")
  output$AFC_stats <- renderUI({
    tagList("AFC East Team Stats", url1)
  })
}
#Executes app opening
shinyApp(ui = ui, server = server)

