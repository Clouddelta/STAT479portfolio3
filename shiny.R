library("ggplot2")
library('tidyverse')
library('shiny')
library('tsibble')
library('DT')
Sys.setlocale("LC_TIME", "English")  

equal_breaks <- function(n = 3, s = 0.5,m=100, ...){
  function(x){
    # rescaling
    d <- s * diff(range(x)) / (1+2*s)
    (seq(min(x)+d, max(x)-d, length=n)%/%m)*m
  }
}
plotcountries=function(data){
  p1=data%>%
    ggplot( aes(Date_reported,New_cases,color=Country_code)) +
    geom_line()+
    facet_grid(rows=vars(Country_code),
               cols=vars(Quater),scales='free')+
    labs(x='Date')+
    scale_x_date(date_labels='%b',date_breaks='3 month',expand=c(0.02,0))+
    scale_y_continuous(breaks=equal_breaks(n=5, s=0.05),labels = comma)+
    theme(plot.title = element_text(color="red", size=14,hjust=0.5, face="bold.italic"),
          axis.title.x = element_text(color="blue", size=14, face="bold"),
          axis.title.y = element_text(color="#993333", size=14, face="bold"),
          legend.position = "none",
          strip.background = element_blank(), 
          strip.placement = "outside",
          strip.text=element_text(size=8),
          strip.text.y=element_text(angle=0),
          panel.grid.major.x = element_blank())
  p2=data%>%
    ggplot( aes(Date_reported,New_deaths,color=Country_code)) +
    geom_line()+
    facet_grid(rows=vars(Country_code),
               cols=vars(Quater),scales='free')+
    labs(x='Date')+
    scale_x_date(date_labels='%b',date_breaks='3 month',expand=c(0.02,0))+
    scale_y_continuous(breaks=equal_breaks(n=5, s=0.05,m=10),labels = comma)+
    theme(plot.title = element_text(color="red", size=14,hjust=0.5, face="bold.italic"),
          axis.title.x = element_text(color="blue", size=14, face="bold"),
          axis.title.y = element_text(color="#993333", size=14, face="bold"),
          legend.position = "none",
          strip.background = element_blank(), 
          strip.placement = "outside",
          strip.text=element_text(size=8),
          strip.text.y=element_text(angle=0),
          panel.grid.major.x = element_blank())
  patch1=p1+p2+plot_annotation(title = 'Covid_19 around countries')&
    theme(plot.title = element_text(size=14,color='red',hjust=0.5,face="bold.italic"))
  patch1
}
plotseasons=function(data){
  pl=data%>%summarise(d1=dayofy%%90,Quater,New_cases)%>%
    group_by(d1,Quater) %>%
    summarise(n = sum(New_cases)) %>%
    mutate(percentage = n / sum(n))%>%
    ggplot(aes(x=d1,y=percentage,fill=reorder(Quater,percentage)))+
    geom_area(alpha=0.6 , size=0.5, colour="white")+
    scale_fill_viridis(discrete = T,name = "Quarter") +labs(title='Covid New Cases in Seasons',
                                                            subtitle='measured in percentage')+
    xlab('Day')+ylab('Cases')+
    theme_ipsum() + 
    theme(plot.title=element_text(color='navy',size=14,hjust=0.5,face='bold.italic'),
          plot.subtitle = element_text(color='navy',size=8,hjust=0.75,face='bold.italic'),
          axis.title.x = element_text(color="blue", size=14,hjust=0.5, face="bold"),
          axis.title.y = element_text(color="#993333", size=14,hjust=0.5, face="bold"))
  ggplotly(pl)
}
reset_selection <- function(x, brush) {
  brushedPoints(x, brush, allRows = TRUE)$selected_
}


world <- map_data("world")
covid_data=read_csv(url("https://covid19.who.int/WHO-COVID-19-global-data.csv"))
covid_data11=covid_data
covid_data11[which(covid_data11$Country=='United States of America'),3]='USA'
covid_data11[which(covid_data11$Country=='The United Kingdom'),3]='UK'
covid_data11[which(covid_data11$Country=='Russian Federation'),3]='Russia'
coviddate=covid_data11$Date_reported

covid_data1=covid_data%>%
  mutate(Quater=as.factor(yearquarter(Date_reported)))
covid_data2=covid_data1%>%filter(Quater!='2022 Q1')
covid_data3=covid_data2%>%
  mutate(quart=as.factor(quarter(Date_reported)),
         m_in_q=ifelse(month(Date_reported)%%3==0,3,month(Date_reported)%%3),
         dayofy=yday(Date_reported)) 

country=covid_data2%>%
  filter(Date_reported=='2021-12-31')%>%
  arrange(-Cumulative_cases)%>%pull(Country)
coviddate0=covid_data2$Date_reported

ui <- navbarPage('Comparison',
                 tabPanel('ss',
                          fluidPage(
                   titlePanel('Covid new cases around the world'),
                   h2("Note: Grey means no data", style = "height: 50px; width: 100%; font-size: 20px;"),
                   dateRangeInput('dateRange',
                                  label = 'Date',
                                  start = '2021-11-15', end = '2021-12-15'
                   ),
                   plotOutput('map'),
                   DTOutput('table')           
                 )),
                 tabPanel('Among countries',
                          fluidPage(
                            selectInput('cty1','Country(sorted by cummulated cases)',choices=country,
                                        selected=c('Canada','The United Kingdom','United States of America'),
                                        multiple = TRUE),
                            dateRangeInput('dateRange0',
                                           label = 'Date',
                                           start = min(coviddate), end = max(coviddate)
                            ),
                            DTOutput('table3'),
                            plotOutput('p1'),
                            DTOutput("table1")
                          )
                 ),
                 tabPanel('Among seasons(one country)',
                          fluidPage(
                            selectInput('cty2','Country(sorted by cummulated cases)',choices=country,
                                        selected='Canada'),
                            plotlyOutput('p2'),
                            DTOutput("table2")
                          )
                 )
)



server <- function(input, output, session) {
  world1=reactive({left_join(world,(covid_data11%>%
                              filter(Date_reported>=input$dateRange[1]&
                                       Date_reported<=input$dateRange[2])%>%
                              group_by(Country)%>%
                              summarise(total_new_cases=sum(New_cases))),
                     by=c('region'='Country'))
  })
  output$table=renderDT(world1())
  output$map=renderPlot(ggplot(world1())+
                            geom_map(data=world,map=world,
                                     aes(long, lat, map_id = region),
                                     color = "white",
                                     fill = "lightgray",
                                     size = 0.1)+
                            geom_map(map = world, 
                                     aes(map_id = region, fill = total_new_cases), 
                                     size=0.25)+
                            scale_fill_gradient(
                              low="#fff7bc", high="#cc4c02", name="New cases counts")+ 
                            expand_limits(x = world$long, y = world$lat)+ 
                            labs(x="", y="")+
                            theme(panel.grid=element_blank(), panel.border=element_blank())+ 
                            theme(axis.ticks=element_blank(), axis.text=element_blank())+
                            theme(legend.position="top"))
  
  selectedcovid1=reactive({covid_data2%>%filter(Country%in%input$cty1&
                                                  Date_reported>=input$dateRange0[1]&
                                                  Date_reported<=input$dateRange0[2])})
  selectedcovid2=reactive({covid_data3%>%filter(Country==input$cty2)})
  output$table1=renderDT(selectedcovid1())
  output$test=renderTable(input$dateRange[1])
  output$p1=renderPlot(plotcountries(selectedcovid1()))
  output$table2=renderDT(selectedcovid2())
  output$p2=renderPlotly(plotseasons(selectedcovid2()))
  output$table3=renderDT(selectedcovid1()%>%
                           group_by(Country)%>%
                           summarise(casemean=mean(New_cases),
                                     deathmean=mean(New_deaths))%>%
                           arrange(casemean))
  
  
}
shinyApp(ui, server)
