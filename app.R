#Author of the idea lauri.punga@eestipank.ee; Estonian Fiscal Council, Bank of Estonia

#Technical realisation for this code: Andres Võrk, University of Tartu, andres.vork@ut.ee, member of the Estonian Fiscal
#Updated 15 September 2018 - loads data in R format

#libraries required
library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(mFilter)
library(zoo)

#Directory for local drive
#setwd("C:/Users/avork/Documents/Eelarvenoukogu/EstonianHeatmap")

#See file "datapreparation..." for data preparation

#Read actual real GDP growth, used to compare with the heatmap composite indicator
#This is taken from Statistics Estonia database
realskp <- readRDS(file= "data/realskp")

#Old version from Excel
#Read data of indicators
# df2 <- read_xlsx("data/data_eng.xlsx")
#Date, turn string into date
# df2 <- df2 %>% 
#   mutate(kpv= as.Date(paste0(substr(time,1,4), "-", as.character(as.numeric(substr(time,8,8))*3-2), "-1"))) %>% 
#   select(-time) %>% select(kpv, everything())

#Automatically downloaded data
load("data/data2018-09-15")


#Start of the ui function
ui <- fluidPage(
  titlePanel("Heatmap of the Estonian economy"),
  sidebarLayout(
    sidebarPanel(width=2,
                 wellPanel(
                   dateInput("begdate","Change start date for standardisation", 
                             min = min(df$kpv), 
                             max = max(df$kpv), 
                             value = max(as.Date("2005-01-01"),min(df$kpv)), 
                             format = "yyyy-mm", startview = "year", language = "en")
                 ),
                 wellPanel(
                   h5("Change weights of the component"),
                   sliderInput(inputId = "weight_coreinflation",label = "Core inflation",value = 0.1, min=0, max=1, step=0.1),
                   sliderInput(inputId = "weight_averagewage",label = "Average wage",value = 0.1, min=0, max=1, step=0.1),
                   sliderInput(inputId = "weight_unemploymentrate",label = "Unemployment rate",value = 0.1, min=0, max=1, step=0.1),
                   sliderInput(inputId = "weight_employmentrate",label = "Employment rate",value = 0.1, min=0, max=1, step=0.1),
                   sliderInput(inputId = "weight_vacancies",label = "Vacancies",value = 0.1, min=0, max=1, step=0.1),
                   sliderInput(inputId = "weight_capacityutilisation",label = "Capacity utilisation",value = 0.1, min=0, max=1, step=0.1),
                   sliderInput(inputId = "weight_demandinconstruction",label = "Demand in construction",value = 0.1, min=0, max=1, step=0.1),
                   sliderInput(inputId = "weight_demandinindustry",label = "Demand in industry",value = 0.1, min=0, max=1, step=0.1),
                   sliderInput(inputId = "weight_demandinservices",label = "Demand in services",value = 0.1, min=0, max=1, step=0.1),
                   sliderInput(inputId = "weight_economicsentiment",label = "Economic sentiment",value = 0.1, min=0, max=1, step=0.1),
                   
                   actionButton("button_weights", "Restore default weights",  
                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                   ),
                 br(),
                 "contact: Andres Võrk, andres.vork@ut.ee"
                 
                 #end of sidebarPanel
    ), 
    
    mainPanel(width = 10,
              tabsetPanel(
                tabPanel("Heatmap", 
                         plotOutput("heatmap", height = 700, width = 1000) #,
                         # br(),
                         # "Composite indicator and GDP gap with HP filter",
                         # plotOutput("comparison", height = 700, width = 1000)
                         ),
                tabPanel("Standardised components", plotOutput("stdatagraph", height = 700, width = 1000)),
                tabPanel("Source data", 
                         plotOutput("sourcedategraph", height = 700, width = 1000)) 
                
                ,
                tabPanel("Composite indicator vs GDP gap with HP",
                         fluidRow(
                           inputPanel(
                             selectInput(inputId = "gdpserieschoice", label = "Choose GDP series", c("Seasonally adjusted" = "Silutud",
                                                                                                     "Seasonally unadjusted" = "Silumata"), selected = "Silutud"),
                             numericInput(inputId = "hpsmoothing",label = "HP smoothing parameter",
                                          value = 1600, min=6, max=129600))
                         ),
                         plotOutput("comparison", height = 500, width = 800),
                         br(),
                         h3("GDP time series and cyclical component"),
                         plotOutput("hpfiltergraph", height = 500, width = 800)
                )
              ) #end of tabsetPanel
    ) #end of mainPanel
    
    #end of sidebarLayout
  )
) # end of UI

#Server function
server <- function(input, output, session) {
  #all variable names except the first (which is date)
  tunnused <- c(colnames(df[,-1]))
  
  #select observations that start later than begdate, which is chosen in UI part
  #alguskuupäevaga aegridade piiramine  
  dfalgus <- reactive({
    df %>%  filter(kpv>=input$begdate)
  })
  
  #weights
  #kaalud
  #update weights to their default values if it is required
  #uuenda vaikimisi valik kaalude kohta
  observeEvent(input$button_weights,  {
    updateSliderInput(session, inputId = "weight_coreinflation",label = "Core inflation",value = 0.1, min=0, max=1, step=0.05)
    updateSliderInput(session, inputId = "weight_averagewage",label = "Average wage",value = 0.1, min=0, max=1, step=0.05)
    updateSliderInput(session, inputId = "weight_unemploymentrate",label = "Unemployment rate",value = 0.1, min=0, max=1, step=0.05)
    updateSliderInput(session, inputId = "weight_employmentrate",label = "Employment rate",value = 0.1, min=0, max=1, step=0.05)
    updateSliderInput(session, inputId = "weight_vacancies",label = "Vacancies",value = 0.1, min=0, max=1, step=0.05)
    updateSliderInput(session, inputId = "weight_capacityutilisation",label = "Capacity utilisation",value = 0.1, min=0, max=1, step=0.05)
    updateSliderInput(session, inputId = "weight_demandinconstruction",label = "Demand in construction",value = 0.1, min=0, max=1, step=0.05)
    updateSliderInput(session, inputId = "weight_demandinindustry",label = "Demand in industry",value = 0.1, min=0, max=1, step=0.05)
    updateSliderInput(session, inputId = "weight_demandinservices",label = "Demand in services",value = 0.1, min=0, max=1, step=0.05)
    updateSliderInput(session, inputId = "weight_economicsentiment",label = "Economic sentiment",value = 0.1, min=0, max=1, step=0.05)
  } 
  )
  
  #initial data, these are influenced only by the selection of observations
  #algandmed, neid mõjutab vaid aegrea alguse valik 
  
  #English
  componentnamesforsourcegraph <- c(coreinflation =  'Core inflation (annualised)',
                      averagewage =  'Average wage (annual growth rate)',
                      unemploymentrate =  'Unemployment rate',
                      employmentrate =  'Employment rate',
                      vacancies =  'Vacancies',
                      capacityutilisation =  'Capacity utilisation',
                      demandinconstruction =  'Demand limiting in construction',
                      demandinindustry =  'Demand limiting in industry',
                      demandinservices =  'Demand limiting in services',
                      economicsentiment =  'Economic sentiment')
  
  #graph
  output$sourcedategraph <- renderPlot({
    dfalgus() %>% select(kpv, tunnused) %>% 
      gather(key="indicator", value = "value", - kpv)  %>% 
      ggplot(aes(x=kpv, y=value, color=indicator)) +
      geom_line() +
      facet_wrap(~indicator, ncol=3, scales = "free", labeller = as_labeller(componentnamesforsourcegraph)) +
      guides(color=FALSE) + labs(y="", x="", caption = "Data: Eurostat, Statistics Estonia \nIdea: Estonian Fiscal Council") +
      theme(text=element_text(size=14))
  })
  
  #standardisation of indicators and weighting together
  #both weights and the beginning of time series influence the results
  #indikaatorite standardiseerimine ja kokku kaalumine, seda mõjutab nii algus, kui kaalud
  dfst <- reactive({

    #requires that weights are present, actually required if weights are input using inputNumeric function, which was the older version
    #nõua, et oleksid olemas kaalud, enne ära arvuta
    
    # req(input$weight_coreinflation , input$weight_averagewage, input$weight_unemploymentrate , input$weight_employmentrate ,
    #     input$weight_vacancies , input$weight_capacityutilisation, input$weight_demandinconstruction , input$weight_demandinindustry ,
    #     input$weight_demandinservices, input$weight_economicsentiment)

    #variables that have opposite sign    
    #muutujad, mille teistpidi pöörame
    oppvars <- c("unemploymentrate", "demandinconstruction", "demandinindustry", "demandinservices")
    
    #standardisation
    #standardiseerimine
    temp <-  dfalgus() %>% select(kpv, tunnused) %>%
      mutate_at(tunnused, funs((.-mean(., na.rm=TRUE))/sd(., na.rm=TRUE))) %>% 
      
      #turn signs
      #osadel pöörame märgi ümber
      mutate_at(vars(one_of(oppvars)),funs(.*(-1))) %>% as.data.frame()
    
    #weighted average
    #Kaalutud keskmise leidmine
    temp <- temp %>%  mutate(Composite = coreinflation*input$weight_coreinflation + 
                               averagewage * input$weight_averagewage+ 
                               unemploymentrate*input$weight_unemploymentrate + 
                               employmentrate*input$weight_employmentrate+
                               vacancies*input$weight_vacancies + 
                               capacityutilisation*input$weight_capacityutilisation+ 
                               demandinconstruction*input$weight_demandinconstruction+
                               demandinindustry*input$weight_demandinindustry +
                               demandinservices*input$weight_demandinservices+
                               economicsentiment*input$weight_economicsentiment) %>% 
      
      #if the sum of the weights is not 1, then rescale all weights
      #skaleerime, et kaalude efektiivne summa oleks taas üks, kui kasutaja pole seda teinud
      mutate(Composite = Composite / (input$weight_coreinflation + input$weight_averagewage+ input$weight_unemploymentrate + input$weight_employmentrate +
                                              input$weight_vacancies + input$weight_capacityutilisation+ input$weight_demandinconstruction + input$weight_demandinindustry +
                                              input$weight_demandinservices+ input$weight_economicsentiment))
    #long form for graphs
    #pikale kujule
    dfst <- temp %>%  gather(key="indicator", value = "value", - kpv)
    dfst
  })   
  
  #graph of standardised variables
  output$stdatagraph <- renderPlot({
    ggplot(data=dfst()[dfst()$indicator!="Composite",], aes(x=kpv, y=value, color=indicator)) +
      geom_line() +
      geom_line(data=dfst()[dfst()$indicator=="Composite",], aes(x=kpv, y=value), size=1.5) +
      labs(y="", x="", caption = "Data: Eurostat, Statistics Estonia \nIdea: Estonian Fiscal Council \n\nFor variables 'Unemployment rate', 'Demand in construction', 'Demand in industry', 'Demand in services' \nsign has been changed", color="") +
      theme(text=element_text(size=14)) +
    scale_color_hue(
      labels = c('Composite indicator', 'Core inflation', 'Average wage','Unemployment rate', 'Employment rate', 'Vacancies',
                 'Capacity utilisation', 'Demand in construction', 'Demand in industry', 'Demand in services', 'Economic sentiment'),
      breaks = c('Composite', 'coreinflation', 'averagewage', 'unemploymentrate', 'employmentrate', 'vacancies',
                 'capacityutilisation', 'demandinconstruction', 'demandinindustry', 'demandinservices', 'economicsentiment')
    )

  })
  
  
  output$heatmap <- renderPlot({
    ggplot(data=dfst(), aes(x=kpv, y=indicator)) +
      geom_tile(aes(fill=value)) +
      scale_fill_gradient2(low = "blue",  high = "orange", midpoint = 0,
                           breaks = c(-3.5,-2,-1,0,1,2,3)) +
      #one can change the breaks to have different colors
      
#      scale_y_discrete(limits = c("Composite", tunnused), expand=c(0,0)) +
      
      labs(y="", x="", caption = "Data: Eurostat, Statistics Estonia  \nIdea: Estonian Fiscal Council", fill="Heat") +
      theme(text=element_text(size=14)) +
      
      scale_x_date(expand=c(0,0), date_breaks="year", date_labels = "%Y") +
      scale_y_discrete(
        labels = c('Composite indicator', 'Core inflation', 'Average wage','Unemployment rate', 'Employment rate', 'Vacancies',
                   'Capacity utilisation', 'Demand in construction', 'Demand in industry', 'Demand in services', 'Economic sentiment'),
        breaks = c('Composite', 'coreinflation', 'averagewage', 'unemploymentrate', 'employmentrate', 'vacancies',
                   'capacityutilisation', 'demandinconstruction', 'demandinindustry', 'demandinservices', 'economicsentiment'),
        limits = c(tunnused, "Composite"), #et saaks koondindikaatori üles
        expand=c(0,0)
      )
    
    
  })

  #GDP time series
  tsrealskp <- reactive({
    tempdf <- realskp %>%  filter(kohandatud==input$gdpserieschoice) 
    ts(tempdf$obsValue, start = c(tempdf$aasta[1],tempdf$kvartal[1]), frequency =4)
    })
  
  #calculation of GDP gap using HP filter
  #SKP lõhe arvutus
  #hptrend
  ontrend <- FALSE
  
  hp.realskpdf <- reactive({
    req(input$hpsmoothing)
    
    #tee aegrea objekt HP jaoks
    
    hp.realskp <-  hpfilter(tsrealskp(),freq=input$hpsmoothing,type="lambda",drift=ontrend)
    
    #HP back to time series, standardised
    #HP, tagasi aegreaks
    hp.realskpdf <- data.frame(HP_tsykkel = (hp.realskp$cycle-mean(hp.realskp$cycle, na.rm=TRUE))/sd(hp.realskp$cycle, na.rm=TRUE)  , kpv=as.Date(time(tsrealskp())))
    hp.realskpdf
  })
  
  #Graph of components of GDP growth rate
  #alumine joonis
  output$hpfiltergraph <- renderPlot({
      req(input$hpsmoothing)
      plot.mFilter(hpfilter(tsrealskp(),freq=input$hpsmoothing,type="lambda",drift=ontrend), ask=FALSE)  
  })
  
  #Graph of comparison of GDP gap and composite index
  #koos joonis
  output$comparison <- renderPlot({
      #Composite andmed
      koond <- dfst() %>%  filter(indicator=="Composite") %>%
        rename(Composite=value) %>%  select(kpv, Composite)
      
      #put together
      #kokku panek
      dfvordlus <- merge(hp.realskpdf(), koond, by = "kpv", all = TRUE) %>% 
        gather(key="indicator", value = "value", - kpv) %>%  filter(kpv>=input$begdate)
      #joonis
      ggplot(dfvordlus, aes(y=value, x=kpv, color=indicator)) +
        geom_line() + 
        scale_x_date(date_breaks = "1 year", date_labels= "%m-%Y") +
        #facet_wrap(~indicator, scales = "free_y", ncol=1) + guides(color=FALSE) + 
        labs(y="", x="",  color= "", caption = "GDP data source: Statistics Estonia, tabel RAA0012, real GDP.
              HP filter is always calculated using time series since 1Q 1995, then cyclical component is standardised") +
        scale_color_hue(
          labels = c('Composite indicator', 'GDP gap'),
          breaks = c('Composite', 'HP_tsykkel')
        )
    }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)