## app.R ##
if(!require("shiny")){
  install.packages("shiny")
  library('shiny')
}


if(!require(shinydashboard)){
  install.packages("shinydashboard")
  library('shinydashboard')
}

if(!require(plotly)){
  install.packages("plotly")
  library('plotly')
}


if(!require(tidyverse)){
  install.packages("tidyverse")
  library('tidyverse')
}


if(!require(survival)) {
  install.packages("survival")
  require(survival)
}

if(!require(survminer)) {
  install.packages("survminer")
  require(survminer)
}

#####################################################################
###Finale Datensatz laden
#####################################################################
load("Finaler_Datensatz_08.RData")


#####################################################################
###globale Variablen
#####################################################################

isInit<-FALSE
minDay<-min(Datensatz_Final$Produktionsdatum)
maxDay<-max(Datensatz_Final$Produktionsdatum)


#####################################################################
###globale Funktion
#####################################################################
#use a helper ggplot function written by Edwin Thoen to plot pretty survival distributions in R.

#code source:
#           https://plot.ly/python/v3/ipython-notebooks/survival-analysis-r-vs-python/#using-r

ggsurv <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                   cens.col = 'red', lty.est = 1, lty.ci = 2,
                   cens.shape = 3, back.white = F, xlab = 'Time',
                   ylab = 'Survival', main = ''){
  
  library(ggplot2)
  strata <- ifelse(is.null(s$strata) ==T, 1, length(s$strata))
  stopifnot(length(surv.col) == 1 | length(surv.col) == strata)
  stopifnot(length(lty.est) == 1 | length(lty.est) == strata)
  
  ggsurv.s <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                       cens.col = 'red', lty.est = 1, lty.ci = 2,
                       cens.shape = 3, back.white = F, xlab = 'Time',
                       ylab = 'Survival', main = ''){
    
    dat <- data.frame(time = c(0, s$time),
                      surv = c(1, s$surv),
                      up = c(1, s$upper),
                      low = c(1, s$lower),
                      cens = c(0, s$n.censor))
    dat.cens <- subset(dat, cens != 0)
    
    col <- ifelse(surv.col == 'gg.def', 'black', surv.col)
    
    pl <- ggplot(dat, aes(x = time, y = surv)) +
      xlab(xlab) + ylab(ylab) + ggtitle(main) +
      geom_step(col = col, lty = lty.est)
    
    pl <- if(CI == T | CI == 'def') {
      pl + geom_step(aes(y = up), color = col, lty = lty.ci) +
        geom_step(aes(y = low), color = col, lty = lty.ci)
    } else (pl)
    
    pl <- if(plot.cens == T & length(dat.cens) > 0){
      pl + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape,
                      col = cens.col)
    } else if (plot.cens == T & length(dat.cens) == 0){
      stop ('There are no censored observations')
    } else(pl)
    
    pl <- if(back.white == T) {pl + theme_bw()
    } else (pl)
    pl
  }
  
  ggsurv.m <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                       cens.col = 'red', lty.est = 1, lty.ci = 2,
                       cens.shape = 3, back.white = F, xlab = 'Time',
                       ylab = 'Survival', main = '') {
    n <- s$strata
    
    groups <- factor(unlist(strsplit(names
                                     (s$strata), '='))[seq(2, 2*strata, by = 2)])
    gr.name <-  unlist(strsplit(names(s$strata), '='))[1]
    gr.df <- vector('list', strata)
    ind <- vector('list', strata)
    n.ind <- c(0,n); n.ind <- cumsum(n.ind)
    for(i in 1:strata) ind[[i]] <- (n.ind[i]+1):n.ind[i+1]
    
    for(i in 1:strata){
      gr.df[[i]] <- data.frame(
        time = c(0, s$time[ ind[[i]] ]),
        surv = c(1, s$surv[ ind[[i]] ]),
        up = c(1, s$upper[ ind[[i]] ]),
        low = c(1, s$lower[ ind[[i]] ]),
        cens = c(0, s$n.censor[ ind[[i]] ]),
        group = rep(groups[i], n[i] + 1))
    }
    
    dat <- do.call(rbind, gr.df)
    dat.cens <- subset(dat, cens != 0)
    
    pl <- ggplot(dat, aes(x = time, y = surv, group = group)) +
      xlab(xlab) + ylab(ylab) + ggtitle(main) +
      geom_step(aes(col = group, lty = group))
    
    col <- if(length(surv.col == 1)){
      scale_colour_manual(name = gr.name, values = rep(surv.col, strata))
    } else{
      scale_colour_manual(name = gr.name, values = surv.col)
    }
    
    pl <- if(surv.col[1] != 'gg.def'){
      pl + col
    } else {pl + scale_colour_discrete(name = gr.name)}
    
    line <- if(length(lty.est) == 1){
      scale_linetype_manual(name = gr.name, values = rep(lty.est, strata))
    } else {scale_linetype_manual(name = gr.name, values = lty.est)}
    
    pl <- pl + line
    
    pl <- if(CI == T) {
      if(length(surv.col) > 1 && length(lty.est) > 1){
        stop('Either surv.col or lty.est should be of length 1 in order
             to plot 95% CI with multiple strata')
      }else if((length(surv.col) > 1 | surv.col == 'gg.def')[1]){
        pl + geom_step(aes(y = up, color = group), lty = lty.ci) +
          geom_step(aes(y = low, color = group), lty = lty.ci)
      } else{pl +  geom_step(aes(y = up, lty = group), col = surv.col) +
          geom_step(aes(y = low,lty = group), col = surv.col)}
    } else {pl}
    
    
    pl <- if(plot.cens == T & length(dat.cens) > 0){
      pl + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape,
                      col = cens.col)
    } else if (plot.cens == T & length(dat.cens) == 0){
      stop ('There are no censored observations')
    } else(pl)
    
    pl <- if(back.white == T) {pl + theme_bw()
    } else (pl)
    pl
  }
  pl <- if(strata == 1) {ggsurv.s(s, CI , plot.cens, surv.col ,
                                  cens.col, lty.est, lty.ci,
                                  cens.shape, back.white, xlab,
                                  ylab, main)
  } else {ggsurv.m(s, CI, plot.cens, surv.col ,
                   cens.col, lty.est, lty.ci,
                   cens.shape, back.white, xlab,
                   ylab, main)}
  pl
}

#####################################################################
###header
#####################################################################
               
header<-dashboardHeader( 
  title = tags$img(src="logo.png", height = '100%', width = '200px',
                   style = 'position: absolute; opacity: 1;'),
  titleWidth = 400

)



#####################################################################
###sidebar
#####################################################################
sidebar<-dashboardSidebar(
  # zeitinterval f眉r Produktionsdatum eingeben
  dateRangeInput("Days",
                 label = h3("Produktionsdatum "),
                 min = minDay,
                 max = maxDay,
                 start = "2008-12-02", 
                 end = "2015-02-03"),
  
  # Copy the chunk below to make a group of checkboxes
  checkboxGroupInput("Fahrzeug_Typ:", label = h3("Fahrzeug Typ"), 
                     choices = list("Typ 11" = 11,
                                    "Typ 12" = 12,
                                    "Typ 21" = 21,
                                    "Typ 22" = 22),
                     selected = 11)
  
  
  # actionButton("login", "Log in"),
  # textInput('userid','User id:',value=' definitely not Florian')
  #fluidRow(column(10,h3("Fahrzeug Typ"), verbatimTextOutput("value")))
)


#####################################################################
###body
#####################################################################
body<-dashboardBody(  

  mainPanel(
    tabsetPanel(
      
      id = "id_tabset",
      
      tabPanel(
        title = "Fehlerhäuufigkeit",
        id = "tab1",
        plotly::plotlyOutput("Balkendiagramm")
      ),
      
      tabPanel(
        title = "Zeitinterval",
        id = "tab2",
        plotly::plotlyOutput("Boxplot")
      ),
      tabPanel(
        title = "KaplanMeier",
        id = "tab3",
        plotly::plotlyOutput("KaplanMeier")
      ),
      tabPanel(
        title = "Datensatz",
        id = "tab4",
        DT::DTOutput("data_to_show")
      )
      
    )
    
  )

)

#####################################################################
###ui
#####################################################################
ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body,
  skin = "red"
  

)
#####################################################################
###server
#####################################################################




server <- function(input, output) {

    #get data:
    #         Datensatz anhand Kombination von Eingaben abwandeln
    get_data_interested <- reactive({
      
      data <- Datensatz_Final%>%
        dplyr::filter(Produktionsdatum>=input$Days[1],
                      Produktionsdatum<=input$Days[2],)%>%
        dplyr::filter(Fahrzeug_Typ %in% input$Fahrzeug_Typ)
      
      return(data)

      
    })


    
    # observeEvent(input$Interval_Produktion, {
    #   showModal(modalDialog(
    #     title = "Somewhat important message",
    #     paste0(minDay,"  ",maxDay),
    #     easyClose = TRUE,
    #     footer = NULL
    #   ))
    # 
    # 
    # })
    
    #render Balkendiagramm
    output$Balkendiagramm<- plotly::renderPlotly({
      Data_Balken<-get_data_interested()%>%
        dplyr::select(Fehlerhaft,Fahrzeug_Typ,Motor_Typ)%>%
        dplyr::group_by(Fahrzeug_Typ,Motor_Typ)%>%
        dplyr::summarise(n=n(),f_rate=sum(Fehlerhaft==TRUE)/n)%>%
        tidyr::spread(Motor_Typ,f_rate)
        
      plot_ly(Data_Balken, x = ~Fahrzeug_Typ, y = ~Benzinmotor, type = 'bar', name = 'Benzinmotor') %>%
        add_trace(y = ~Dieselmotor, name = 'Dieselmotor') %>%
        layout(yaxis = list(title = 'Fehlerh盲ufigkeit'), barmode = 'group')

      
    })
    
    
    
    #render Boxplot
    output$Boxplot <- plotly::renderPlotly({
      Data_Boxplot<-get_data_interested()%>%
        dplyr::filter(Fehlerhaft==TRUE)%>%
        dplyr::select(Lebensdauer,Fahrzeug_Typ,Motor_Typ)
        
      plot_ly(Data_Boxplot, x = ~Fahrzeug_Typ, y = ~Lebensdauer, color = ~Motor_Typ, type = "box")%>%
        layout(boxmode = "group")
    })

    #render KaplanMeier
    output$KaplanMeier <- plotly::renderPlotly({
      # Estimate the survivor function from this dataset via kaplan-meier.

      #ggplot
      p <- ggsurv(survfit(Surv(Lebensdauer, Fehlerhaft) ~ Motor_Typ, data = get_data_interested()),
                  xlab = 'Zeit (in Tag)',
                  ylab = 'Überlebenswahrscheinlichkeit',
                  main='Kaplan-Meier estimate with 95% confidence bounds') + theme_bw()
      #convert ggplot to plotly
      pl<-ggplotly(p)
      pl

    })    
    
    #render kompletten Datensatz
    output$data_to_show <- DT::renderDT({
      Datensatz_Final
      
    })
    
    
}


#####################################################################
###App laufen
#####################################################################
shinyApp(ui, server)