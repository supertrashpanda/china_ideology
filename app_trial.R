library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(DT)
library(tidyverse)
library(plotly)
library(shinyWidgets)
library(tidymodels)
library(anytime)

df<-read.csv("https://raw.githubusercontent.com/supertrashpanda/china_ideology/refs/heads/master/data.csv")

df$birth<-as.numeric(df$Q261)
df$Q106<-ifelse(df$Q106>0,df$Q106,NA)
df$Q107<-ifelse(df$Q107>0,df$Q107,NA)
df$Q108<-ifelse(df$Q108>0,df$Q108,NA)
df$mkt<-df$Q106-1+10-df$Q107+df$Q108-1
df$Q235<-ifelse(df$Q235>0,df$Q235,NA)
df$Q236<-ifelse(df$Q236>0,df$Q236,NA)
df$Q238<-ifelse(df$Q238>0,df$Q238,NA)
df$demo<-df$Q235-1+4-df$Q236+4-df$Q238
df$demo<-df$demo*27/9
df$`authoritarian-democratic`<-df$demo
df$`state-market`<-df$mkt
df$sex<-ifelse(df$female==1,'female','male')
df$province<-sub(".*? ", "", df$province)
df$education<-ifelse(df$Q275R==1,"low",NA)
df$education<-ifelse(df$Q275R==2,"middle",df$education)
df$education<-ifelse(df$Q275R==3,"higher",df$education)
df<-df[,c('province','education','sex','birth','age',
          'authoritarian-democratic','state-market')]

contain_yes <- function(list1,list2) {
  result<-FALSE
  for(i in list2){
    if(i %in% list1){result=TRUE}
  }
  return(result)
}

ui <- navbarPage("China's Ideological Spectrum",
                 tabPanel("page1",fluidPage(
                   fluidRow(
                     tags$h3("For respondents that were born within the following time span, \n
                             their two-dimentional ideological distributions are: ",
                             style="width: 50%; font-family:Arial;font-weight:bold;text-align: center;")),
                   chooseSliderSkin("Flat"),
                   sliderInput("time",
                               label = "",
                               min = 1948,
                               max = 2000,
                               ticks=FALSE,
                               width='50%',
                               value=c(1948,2000),
                               sep=""),
                   tags$script(HTML("
        $(document).ready(function() {setTimeout(function() {
          supElement = document.getElementById('time').parentElement;
          $(supElement).find('span.irs-max, span.irs-min').remove();}, 10);})
      ")), br(),br(),
                   dashboardBody(
                     box(width = 6,
                         height = "50em",
                         plotlyOutput(outputId = "plot")
                     ))))
                 )
                 ################################End of Panel 3##################################



server<-function(input, output, session) {
  
  
  cohort= reactive({
    return(df%>%filter((birth>=input$time[1])&(birth<=input$time[2])))
  })
  
  ###########################End of Reactive Functions#############################
output$plot=renderPlotly({
    ggplotly(ggplot(cohort(),aes(group=1,label2=province,label3=sex,label1=education))+
               geom_jitter(aes(`state-market`,`authoritarian-democratic`),alpha=0.5,
                           width = 0.5, height = 0.5)+
               geom_smooth(aes(`state-market`,`authoritarian-democratic`),method='lm')+
               ylim(0,27)+
               xlim(0,27)+
               expand_limits(x = 0, y = 0)+
               labs(x="Pro-State vs. Pro-Market Economic Ideology",
                    y="Authoritarian vs. Democratic Political Ideology")+
               coord_obs_pred()+
               theme_light(),height = 400, width = 400)
  })
  

  
#output for panel 3
}

shinyApp(ui = ui, server = server)

