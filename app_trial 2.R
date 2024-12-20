library(reshape2)
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
library(magrittr)
library(hchinamap)

df<-read.csv("https://raw.githubusercontent.com/supertrashpanda/china_ideology/refs/heads/master/data.csv")

df$birth<-as.numeric(df$Q261)
df$Q106<-ifelse(df$Q106>0,df$Q106,NA)
df$Q107<-ifelse(df$Q107>0,df$Q107,NA)
df$Q108<-ifelse(df$Q108>0,df$Q108,NA)
df$mkt<-df$Q106-1+10-df$Q107+df$Q108-1
df$Q235<-ifelse(df$Q235>0,df$Q235,NA)
df$Q236<-ifelse(df$Q236>0,df$Q236,NA)
df$Q238<-ifelse(df$Q238>0,df$Q238,NA)
df$class<-ifelse(df$Q287<=0,NA,df$Q287)

df<-df%>%mutate(class = case_when(
  class ==1 ~ 'Upper',
  class == 2 ~ 'Upper Middle',
  class==3 ~ 'Middle',
  class==4 ~ 'Lower Middle',
  class==5 ~ 'Lower'))
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
          'authoritarian-democratic','state-market',"N_REGION_WVS",'class')]

pro<-read.csv("https://raw.githubusercontent.com/supertrashpanda/china_ideology/refs/heads/master/provinces.csv",header=FALSE)
pro$code<-substr(pro$V1, 1, 6)


pro$province<-colsplit(pro$V1," ",c("no","province"))[,2]
pro<-pro[,-1]
colnames(pro)<-c('c_province','code','province')


dir <- tempdir()
download.file('https://czxb.github.io/br/chinadf.rda', file.path(dir, 'chinadf.rda'))
load(file.path(dir, 'chinadf.rda'), verbose = TRUE)







ui <- navbarPage("China's Ideological Spectrum",
                 tabPanel("Generational Ideology",fluidPage(
                   fluidRow(
                     tags$h5("For respondents that were born within the time span below, \n
                             The scatter plot illustrate their two-dimensional ideological distribution ",
                             style="width: 50%; font-family:Arial;font-weight:bold;text-align: left;")),
                   chooseSliderSkin("Flat"),
                   sliderInput("time",
                               label = "",
                               min = 1948,
                               max = 2000,
                               ticks=FALSE,
                               width='50%',
                               value=c(1948,1968),
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
                     )))),
######################################end of panel 1####################################3
                 tabPanel("Ideology Map",sidebarPanel(
                   h3("Ideology Map"),
                   br(),
                   h5("Apply the filters below",style="font-weight:bold"),
                   #tags$head(tags$style(HTML('#dim+ div>.selectize-input{width: 300px !important}'))),
                   selectizeInput('dim', tags$span("1. Select the Ideological Dimension to Examine:",style="font-weight:bold; padding:0px"), 
                                  choices = c('Economic','Political'),selected = TRUE,  multiple = FALSE),
                   chooseSliderSkin("Flat"),
                   sliderInput("yearrange",
                               label = tags$span("2. People Born within the Time Span:",style="font-weight:bold; padding:0px"),
                               min = 1948,
                               max = 2000,
                               ticks=FALSE,
                               value=c(1948,1968),
                               sep=""),
                   tags$script(HTML("
        $(document).ready(function() {setTimeout(function() {
          supElement = document.getElementById('time').parentElement;
          $(supElement).find('span.irs-max, span.irs-min').remove();}, 10);})
      ")),
         selectizeInput('sex', tags$span("3. Sex:",style="font-weight:bold; padding:0px"), 
        choices = c('All','Female','Male'),selected = TRUE,multiple = FALSE),
        selectizeInput('class', tags$span("3. Self-identified Social Class:",style="font-weight:bold; padding:0px"), 
                       choices = c('All','Upper',
                                   'Upper Middle','Middle',
                                   'Lower Middle','Lower'),selected = TRUE,multiple = FALSE)
                 
                 ),
                 mainPanel(hchinamapOutput(outputId = "map", height = 500)
                           ))
################################End of Panel 2##################################       
                 )




server<-function(input, output, session) {
  
  
  cohort= reactive({
    return(df%>%filter((birth>=input$time[1])&(birth<=input$time[2])))
  })
  
  subdf=reactive({
    if(input$class!='All'){
    sub<-df[df$class==input$class,]
    }
    else{sub<-df}
    
    if(input$sex=='Female'){
      sub<-sub%>%filter((birth>=input$yearrange[1])&(birth<=input$yearrange[2]),sex=='female')
    }
    else if(input$sex=='Male'){
      sub<-sub%>%filter((birth>=input$yearrange[1])&(birth<=input$yearrange[2]),sex=='male')
    }
    else{sub<-sub%>%filter((birth>=input$yearrange[1])&(birth<=input$yearrange[2]))}

    pros<-sub%>%group_by(N_REGION_WVS)%>% summarise(avg_eco=mean(`state-market`,na.rm=T),
                                               avg_poli=mean(`authoritarian-democratic`,na.rm=T))%>%
      mutate(N_REGION_WVS=as.character(N_REGION_WVS))%>%
      inner_join(pro,by=c("N_REGION_WVS"="code"))
    china <- chinadf %>% 
      dplyr::filter(region == "China")%>%
      inner_join(pros,by=c('name'='c_province'))%>%
      mutate(province=sub(".*? ", "", province))
    return(china)
  })
  
dimension<-reactive({
    input$dim
  })
  
#############################End of Reactive Functions#############################


output$map=renderHchinamap({ 
  
  if(dimension()[[1]]=='Economic'){
  hchinamap(name=subdf()$name,value = subdf()$avg_eco,titleSize="18px",itermName="state-market economic dimension",
            width = "100%", height = "400px",maxColor='#57b9ff',min=8,legendTitle="Pro-State vs. Pro-Market",
            minColor='#FF0000',title = "Province-Level State-Market Economic Attitude Index", region = "China",
            theme = "sunset")
  }
  else{
    hchinamap(name=subdf()$name,value = subdf()$avg_poli,titleSize="18px",itermName="authoritarian-democratic political dimension",
              width = "100%", height = "400px",maxColor='#57b9ff',min=8,legendTitle="Authortarian vs. Democratic",
              minColor='#FF0000',title = "Province-Level Authortarian-Democratic Political Attitude Index", region = "China",
              theme = "sunset")   
  }

  })


output$plot=renderPlotly({
    ggplotly(ggplot(cohort(),aes(group=1,label2=province,label3=sex,label1=education))+
               geom_abline(slope=1, intercept=0,alpha=0.5,size=0.5)+
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

