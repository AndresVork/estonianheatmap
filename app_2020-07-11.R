#Technical realisation for this code: Andres Võrk, University of Tartu, andres.vork@ut.ee, member of the Estonian Fiscal
#Author of the idea lauri.punga@eestipank.ee; Estonian Fiscal Council, Bank of Estonia
#Updated 15 September 2018 - loads data in R format
#Updated 27 December 2018 - added plotly graphs with ggplotly command
#Updated 5 April 2018 - automated update of data

#11 July 2020 - 
#- vakantside tabel muutus 2018 ja edasi, muutsin koodi
#-oma arvutis paneks all_of(tunnused), serveris aga see ei töötanud millegi pärast, vist vana tidyselect

#libraries required
library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(tidyselect) #Lisatud 11 juuli 2020
library(ggplot2)
library(mFilter)
library(zoo)
library(plotly)

library(eurostat)
library(lubridate)
require(rsdmx)

#Directory for local drive
#setwd("C:/Users/avork/Documents/Eelarvenoukogu/EstonianHeatmap")

#See file "datapreparation..." for separate data preparation file

#Automatically downloaded data
#load("data/data2018-09-15")
#load("data/data2018-12-26")
#load("data/data2019-03-26")
load("data/data.RData")

#laen teksti
moisted <- read_excel("moisted.xlsx", sheet = "Sheet1")


#load(file = paste0("C:/Users/avork/Documents/Eelarvenoukogu/EstonianHeatmap/data/data", as.character(Sys.Date())))
#source("https://raw.githubusercontent.com/AndresVork/rsdmx_esa/master/rsdmx_esa_fn.R")

#Start of the ui function
ui <- fluidPage(
  titlePanel("Heatmap of the Estonian economy"),
  tags$head(tags$style(".shiny-notification {position: fixed; top: 60% ;left: 50%}")),
  
  sidebarLayout(
    sidebarPanel(width=2,
                 wellPanel(
                   dateInput("begdate","Change start date for standardisation", 
                             min = min(df$kpv), 
                             max = max(df$kpv), 
                             value = max(as.Date("2005-01-01"),min(df$kpv)), 
                             format = "yyyy-mm", startview = "year", language = "en"),
                   "Changes the start of the heatmap and recalculates colors"
                 ),

                 br(),
                 wellPanel(
                   #br(),
                   actionButton("button_uuenda", "Update source data",  
                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                   br(),
                   br(),
                   "Updates data automatically from Eurostat and Statistic Estonia database"
                 ),
                 #"Uuendab automaatselt andmed Eurostati ja ESA andmebaasist ning Töötukassa ja Eesti Konjunktuuriinstituudi kodulehelt.", 
                 br(),
                 "contact: Andres Võrk, andres.vork@ut.ee, University of Tartu",
                 br(),
                 "Idea: Estonian Fiscal Council (Eelarvenõukogu)"

               #   br(),
               # wellPanel(
               #     h5("Change weights of the component"),
               #     sliderInput(inputId = "weight_coreinflation",label = "Core inflation",value = 0.1, min=0, max=1, step=0.1),
               #     sliderInput(inputId = "weight_averagewage",label = "Average wage",value = 0.1, min=0, max=1, step=0.1),
               #     sliderInput(inputId = "weight_unemploymentrate",label = "Unemployment rate",value = 0.1, min=0, max=1, step=0.1),
               #     sliderInput(inputId = "weight_employmentrate",label = "Employment rate",value = 0.1, min=0, max=1, step=0.1),
               #     sliderInput(inputId = "weight_vacancies",label = "Vacancies",value = 0.1, min=0, max=1, step=0.1),
               #     sliderInput(inputId = "weight_capacityutilisation",label = "Capacity utilisation",value = 0.1, min=0, max=1, step=0.1),
               #     sliderInput(inputId = "weight_demandinconstruction",label = "Demand in construction",value = 0.1, min=0, max=1, step=0.1),
               #     sliderInput(inputId = "weight_demandinindustry",label = "Demand in industry",value = 0.1, min=0, max=1, step=0.1),
               #     sliderInput(inputId = "weight_demandinservices",label = "Demand in services",value = 0.1, min=0, max=1, step=0.1),
               #     sliderInput(inputId = "weight_economicsentiment",label = "Economic sentiment",value = 0.1, min=0, max=1, step=0.1),
               #     
               #     actionButton("button_weights", "Restore default weights",  
               #                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
               #     ),

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
                #tabPanel("Standardised components", 
                #         plotOutput("stdatagraph", height = 700, width = 1000)) ,
                #         #plotlyOutput("stdatagraphplotly", height = 700, width = 1000)),
                

                tabPanel("Composite indicator vs GDP gap with HP",
                         fluidRow(
                           inputPanel(
                             #selectInput(inputId = "gdpserieschoice", label = "Choose GDP series", c("Seasonally adjusted" = "Silutud",
                             #                                                                        "Seasonally unadjusted" = "Silumata"), selected = "Silutud"),
                             numericInput(inputId = "hpsmoothing",label = "Choose HP smoothing parameter",
                                          value = 1600, min=6, max=129600))
                         ),
                         plotOutput("comparison", height = 700, width = 1000),
                         br(),
                         br(),
                         br(),
                         
                         h3("GDP time series and cyclical component"),
                         plotOutput("hpfiltergraph", height = 500, width = 800)
                ),
                
                tabPanel("Source data", 
                         plotOutput("sourcedategraph", height = 900, width = 1000), 
                         h3("Mõisted ja allikad"),
                         br(),
                         column(tableOutput("moisted"), width = 8))
                
                #plotlyOutput("sourcedategraphplotly", height = 1000, width = 1000)) 
                
              ) #end of tabsetPanel
    ) #end of mainPanel
    
    #end of sidebarLayout
  )
) # end of UI

#Server function
server <- function(input, output, session) {
  
  observeEvent(input$button_uuenda,  {
    
    #if (as.Date(file.info("data.RData")$mtime)== as.Date(Sys.Date())) {
    #  showNotification("Viimane seis on täna juba laetud", type="message")
    #} else{
      
    # #kui kataloogis olev faili uuendamise kuupäev ei ole võrdne süsteemi kuupäevaga, siis lae uuesti andmed
    #showNotification("Laen andmebaasidest uuemad andmed, läheb veidi aega", type="message")
    
    showNotification("Updating data from Eurostat and Statistics Estonia, it takes some time", type="message")
    
    #1. Läbi rääkida Lauriga, kas tasub kasutada Eurostati andmeid võrreldes KI-ga
    #2. Paneks SKP samasse tabelisse ja võtaks vaid sesoonselt silutud
    #3. Üldisem - teha enda ESA tabeli funktsioon paindlikumaks
    #4. Vakantside puhul - Eesti andmete puhul tehtud vanas ja uues andmefailis silumised ja kokku pandud. Võimalik enne kokku panna ja siis silida.
    #5. Tööpuuduse ja hõive määrad saaks ka Eurostatist tuua
    #6. Eurostati vakantsides lüngad Eesti jaoks
    
    #Real GDP
    #GDP dowload from Statistics Estonia server
      tf2 <- tempfile(tmpdir = tdir <- tempdir())
      download.file("http://andmebaas.stat.ee/restsdmx/sdmx.ashx/GetData/RAA0012/I+II+III+IV.1+2.2/all?startTime=1995", tf2)
      realskp <- readSDMX(tf2, isURL = FALSE) %>% 
        as.data.frame() %>%  # filter(DIM4==2,DIM2 !="1") %>%
        mutate(kvartal = as.numeric(as.roman(DIM2)), 
                kpv=as.Date(paste0(obsTime, "-", as.character(kvartal*3-2), "-01")),
                aasta=as.numeric(obsTime)) %>%
        mutate(kohandatud = ifelse(DIM3==1, "Silumata", "Silutud")) %>%
        arrange(aasta, kvartal) %>%
        dplyr::select(aasta, kvartal, kohandatud, obsValue, kpv)
    
    #ggplot(realskp, aes(x=as.Date(kpv), y=obsValue, group=kohandatud, color=kohandatud)) +
    #  geom_line()
    #Save GDP data  to data subfolder if necessary
    saveRDS(realskp, file= "data/realskp")
    realskp <- readRDS(file= "data/realskp")
    
    
    #Komponendid
    #1. 'Core inflation (annualised)'
    #Alusinflatsioon	Ühtlustatud tarbijahinnaindeks, v.a. energia, toit, alkohol ja tubakas
    #muutus võrreldes eelmise aasta sama kvartaliga, kvartal=kolme kuu keskmine	Eurostat
    # In Euro Area, the core inflation rate is calculated using the weighted average of the Harmonised Index of Consumer Price (HICP) aggregates, 
    #excluding energy, food, alcohol & tobacco that face volatile price movements. 
    
    #TOT_X_NRG_FOOD	Overall index excluding energy, food, alcohol and tobacco
    df_temp<- get_eurostat("prc_hicp_midx", filters = list(geo = "EE", coicop="TOT_X_NRG_FOOD", unit="I15")) %>%
      filter(time>=as.Date("2003-01-01")) %>%  dplyr::select(time, values) %>% 
      dplyr::rename(coreinflation=values)
    
    inflatsioon <- df_temp %>%   mutate(quarter= quarter(time), year=year(time)) %>% 
      group_by(year, quarter) %>% 
      summarise(coreinflation=mean(coreinflation), 
                rank=which.min(time),
                kpv = time[rank]) %>% ungroup() %>% 
      mutate(coreinflation=(coreinflation-dplyr::lag(coreinflation,4))/dplyr::lag(coreinflation,4)*100) %>% 
      dplyr::filter(kpv>=as.Date("2004-01-01"), !is.na(coreinflation)) %>%   #drop observations with missing values
      dplyr::select(kpv, coreinflation) %>% as.data.frame()
    
    #plot(inflatsioon, type="o")
    
    #2. Keskmine palk	Keskmine brutokuupalk, tegevusalad kokku	
    #muutus võrreldes eelmise aasta sama kvartaliga	- statistikaamet
    #"PA001: Keskmine brutopalk, tööjõukulu, töötatud tunnid ja töötajate arv tegevusalarühma järgi (kvartalid)"

    tfpalk <- tempfile(tmpdir = tdir <- tempdir())
    download.file("http://andmebaas.stat.ee/restsdmx/sdmx.ashx/GetData/PA001/D11_EMPL.TOTAL.Q/all?startTime=2002-Q1", tfpalk)
    keskminepalk <- readSDMX(tfpalk, isURL = FALSE) %>%  as.data.frame() %>% 
        mutate(kpv=as.Date(paste0(substr(obsTime,1,4), "-", as.character(as.numeric(substr(obsTime,7,7))*3-2), "-1")))  %>% 
      #kasvumäär võrreldes eelmise aasta sama perioodiga
      arrange(kpv) %>% 
      mutate(averagewage=(obsValue-dplyr::lag(obsValue,4))/dplyr::lag(obsValue,4)*100) %>% 
      dplyr::select(kpv, averagewage) %>%  
      filter(kpv>=as.Date("2004-01-01")) %>% 
      as.data.frame()
    #plot(keskminepalk, type="l")

    #3. Töötuse ja hõive määr	Töötuse määr vanuses 15-74	tase	statistikaamet
    #kasutame enda funktsiooni
    #df_temp <- rsdmx_esa("TT461")
    tfTT461 <- tempfile(tmpdir = tdir <- tempdir())
    download.file("http://andmebaas.stat.ee/restsdmx/sdmx.ashx/GetData/TT461/I+II+III+IV.7+8/all?startTime=1997", tfTT461)
    dfTT461 <- readSDMX(tfTT461, isURL = FALSE) %>%  as.data.frame() %>% 
      mutate(kvartal = as.numeric(as.roman(DIM2)), 
             kpv=as.Date(paste0(obsTime, "-", as.character(kvartal*3-2), "-01")),
             aasta=as.numeric(obsTime))
    
    tootusemaar <- dfTT461 %>%
      dplyr:: filter(kpv>=as.Date("2004-01-01"), DIM3==8)  %>% 
      dplyr::rename(unemploymentrate=obsValue) %>% 
      dplyr::select(kpv, unemploymentrate) %>%  arrange(kpv)
    #plot(tootusemaar, type="l")
    
    #4. Hõive määr	Tööhõive määr vanuses 15-74	tase	statistikaamet, samast tabelist
    hoivemaar <- dfTT461 %>%
      dplyr:: filter(kpv>=as.Date("2004-01-01"), DIM3==7)  %>% 
      dplyr::rename(employmentrate=obsValue) %>% 
      dplyr::select(kpv, employmentrate) %>%  arrange(kpv)
    #plot(hoivemaar, type="l")
    
    #5. Vabad ametikohad	Vabade ametikohtade arv, tegevusalad kokku	tase	statistikaamet
    #need andmed on olemas alates 2008 andmebaasis. Vanad andmed peab võtma teisest tabelist
    tfPAV011 <- tempfile(tmpdir = tdir <- tempdir())
    #Uuendatud 11.07.2020. ESA tabelis muutus midagi
    
    download.file("http://andmebaas.stat.ee/restsdmx/sdmx.ashx/GetData/PAV011/VAC._T.Q/all?startTime=2008-Q1", tfPAV011)
    vabadametikohadal2008 <- readSDMX(tfPAV011, isURL = FALSE) %>%  as.data.frame() %>% 
      mutate(kpv=as.Date(as.yearqtr(obsTime, format = "%Y-Q%q"))) %>% 
      dplyr::rename(vacancies=obsValue) %>% 
      dplyr::select(kpv, vacancies) %>%  arrange(kpv)
    
    #Vana
    #download.file("http://andmebaas.stat.ee/restsdmx/sdmx.ashx/GetData/PAV011/I+II+III+IV.20.1/all?startTime=2008", tfPAV011)
    #vabadametikohadal2008 <- readSDMX(tfPAV011, isURL = FALSE) %>%  as.data.frame() %>% 
    #  mutate(kvartal = as.numeric(as.roman(DIM2)), 
    #  kpv=as.Date(paste0(as.character(obsTime), "-", as.character(kvartal*3-2), "-1"))) %>% 
    #  dplyr::rename(vacancies=obsValue) %>% 
    #  dplyr::select(kpv, vacancies) %>%  arrange(kpv)
   #plot(vabadametikohadal2008, type="l") 

   #Vanad andmed - seda ei peaks käima enam pärimas, kui on maha salvestatud
   #tfPAV11 <- tempfile(tmpdir = tdir <- tempdir())
   #download.file("http://andmebaas.stat.ee/restsdmx/sdmx.ashx/GetData/PAV11/I+II+III+IV.580400000.23123/all?startTime=2005", tfPAV11)
   #vabadametikohadkuni2007 <- readSDMX(tfPAV11, isURL = FALSE) %>%  as.data.frame()  %>% 
   #  mutate(kvartal = as.numeric(as.roman(DIM2)), 
   #         kpv=as.Date(paste0(as.character(obsTime), "-", as.character(kvartal*3-2), "-1"))) %>% 
   #    dplyr::rename(vacancies=obsValue) %>% 
   #    dplyr::select(kpv, vacancies) %>%  arrange(kpv)
   #saveRDS(vabadametikohadkuni2007, file= "data/vabadametikohadkuni2007")

   vabadametikohadkuni2007 <- readRDS(file= "data/vabadametikohadkuni2007")
   vabadametikohad<- rbind(vabadametikohadkuni2007, vabadametikohadal2008) %>%  
      arrange(kpv)
  #rm(vabadametikohadkuni2007, vabadametikohadal2008)
  #plot(vabadametikohad, type="l")
    
    # ###########
    # #Vabad ametikohad Eurostatist, kui soovida laiendada teiste riikide kohta
    # df_temp<- get_eurostat("jvs_q_nace2", filters = list(geo = "EE", indic_em=c("JOBVAC"),
    #                                                      s_adj = "NSA", nace_r2="A-S", sizeclas="TOTAL"))
    # vabadametikohadal2008eurostat <- df_temp %>% 
    #   filter(time>=as.Date("2008-01-01")) %>%
    #   dplyr::rename(kpv=time) %>%  dplyr::select(kpv, values) %>% arrange(kpv) %>%  as.data.frame()
    # #2009 1. kvartal ja 2015 1. kvartal oli lünk! 
    # 
    # df_temp<- get_eurostat("jvs_q_nace1", filters = list(geo = "EE", indic_em=c("JOBVAC"),
    #                                                      s_adj = "NSA", nace_r1="TOTAL", sizeclas="TOTAL"))
    # vabadametikohadkuni2007eurostat <- df_temp %>% 
    #   dplyr::rename(kpv=time) %>%
    #   filter(kpv>=as.Date("2004-01-01") & kpv<as.Date("2008-01-01"))  %>% 
    #   dplyr::select(kpv, values) %>% arrange(kpv) %>%  as.data.frame()
    # 
    # vabadametikohadeurostat<- rbind(vabadametikohadkuni2007eurostat, vabadametikohadal2008eurostat) %>%  
    #   arrange(kpv) %>% 
    #   mutate(values=(values+ 
    #                    dplyr::lag(values,1)+ 
    #                    dplyr::lag(values,2)+ 
    #                    dplyr::lag(values,3))/4) %>% 
    #   dplyr::rename(vacancies=values)
    # rm(vabadametikohadkuni2007eurostat, vabadametikohadal2008eurostat)
    # 
    
    #6. Rakendatuse määr	Tootmisvõimsuse rakendatuse määr töötlevas tööstuses,  
    #sesoonselt silutud	tase (!)	Eurostat
    #BS-ICU-PC	Current level of capacity utilization (%)
    df_temp<- get_eurostat("ei_bsin_q_r2", filters = list(geo = "EE", indic=c("BS-ICU-PC", "BS-FLP2-PC"),
                                                          s_adj = "SA"))
    
    rakendatusemaar <- df_temp %>% filter(indic=="BS-ICU-PC", time>=as.Date("2004-01-01")) %>%
      dplyr::rename(kpv=time, capacityutilisation=values) %>%  dplyr::select(kpv, capacityutilisation) %>% arrange(kpv)
    
    #plot(rakendatusemaar, type="l")
    
    #7. Nõudlus tööstuses	Tööstusbaromeeter: ebapiisav nõudlus kui kõige olulisem toodangu kasvu piirav tegur	tase, nelja kvartali (TODO! ??) keskmine	Konjunktuuriinstituut
    # Eurostatis ei ole kuist andmestikku, kus oleks piirav tegur antud
    #Kasutan otse kvartalit, kuid see ei tule täpselt sama, mis on Lauril
    df_temp<- get_eurostat("ei_bsin_q_r2", filters = list(geo = "EE", indic=c("BS-FLP2-PC"),
                                                          s_adj = "NSA"))
    noudlustoostuses <- df_temp %>% filter(indic=="BS-FLP2-PC", time>=as.Date("2003-01-01")) %>%
      dplyr::rename(kpv=time) %>% dplyr::select(kpv, values) %>% arrange(kpv) %>% 
      mutate(values=(values+ 
                       dplyr::lag(values,1)+ 
                       dplyr::lag(values,2)+ 
                       dplyr::lag(values,3))/4) %>% 
      dplyr::rename(demandinindustry=values)   %>%   
      filter(kpv>=as.Date("2004-01-01")) 
    
    
    #8. Nõudlus ehituses	Ehitusbaromeeter: ebapiisav nõudlus kui kõige olulisem ehitustegevust piirav tegur	tase, 
    # viimase? nelja kvartali keskmine, kvartal=kolme kuu keskmine	Konjunktuuriinstituut
    #? Lauril peaks olema lihtsalt viimase 12 kuu keskmine tingimusel, et andmed on olemas kvartali viimasel kuul
    # Tundub, et see ei ole sesoonselt kohandatud
    
    #Eurostatist kuised andmed. Kvartaalseid ei ole ehituse kohta valmis kujul.
    #BS-FLBA2-PC	Factors limiting building activity - Insufficient demand
    df_temp <- get_eurostat("ei_bsbu_m_r2", filters = list(geo = "EE", indic=c("BS-FLBA2-PC"),
                                                           s_adj = "NSA"))
    noudlusehituses <- df_temp %>% filter(time>=as.Date("2003-01-01")) %>%
      #kvartali tunnus, nagu ülal ja leiame keskmise
      mutate(quarter= quarter(time), year=year(time)) %>% 
      group_by(year, quarter) %>% 
      summarise(values=mean(values), 
                rank=which.min(time),
                kpv = time[rank]) %>% ungroup() %>% arrange(kpv) %>% 
      #viimase nelja kvartali keskmine, teen käsitsi
      mutate(values=(values+
                       dplyr::lag(values,1)+ 
                       dplyr::lag(values,2)+ 
                       dplyr::lag(values,3))/4) %>% 
      filter(kpv>=as.Date("2004-01-01")) %>% 
      dplyr::select(kpv, values) %>% 
      dplyr::rename(demandinconstruction=values) %>%  
      as.data.frame()
    
    #Väike erinevus võrreldes Lauri KI andmetega. Kunagi täpsustada
    
    #9. Nõudlus teeninduses	Teenindusbaromeeter: 
    #ebapiisav nõudlus kui kõige olulisem äritegevust piirav tegur	tase, 
    #nelja kvartali keskmine	Konjunktuuriinstituut
    
    #Endale: Eurostati kuises andmestikus on juba mingi libisev keskmine muutus
    #ei_bsse_m_r2 - tunnus 	BS-SARM	Evolution of demand over the past 3 months
    
    #Eurostati Kvartaalses andmestikus on tunnus olemas
    #BS-FLB2-PC	Main factors limiting the business - insufficient demand
    
    df_temp<- get_eurostat("ei_bsse_q_r2", filters = list(geo = "EE", indic=c("BS-FLB2-PC"),
                                                          s_adj = "NSA"))
    #nagu ülal
    noudlusteeninduses <- df_temp %>% filter(indic=="BS-FLB2-PC", time>=as.Date("2003-01-01")) %>%
      dplyr::rename(kpv=time) %>%  dplyr::select(kpv, values) %>% arrange(kpv) %>% 
      mutate(values=(values+ 
                       dplyr::lag(values,1)+ 
                       dplyr::lag(values,2)+ 
                       dplyr::lag(values,3))/4) %>% 
      dplyr::rename(demandinservices=values)   %>%   
      filter(kpv>=as.Date("2004-01-01"))
    
    #paneme esimes puuduva väärtuse samaks, mis teises kvartalis, Lauril näis sama
    noudlusteeninduses$demandinservices[noudlusteeninduses$kpv==as.Date("2004-01-01")] <-
      noudlusteeninduses$demandinservices[noudlusteeninduses$kpv==as.Date("2004-04-01")] 
    
    
    #10. Majandususaldusindeks	Ettevõtete (4 sektorit) ja 
    #tarbijate kindlustunde indikaator, sesoonselt silutud	tase, 
    #kvartal=kolme kuu keskmine	Konjunktuuriinstituut
    
    # Sentiment indicators - monthly data (ei_bssi_m_r2) 
    #BS-ESI-I	Economic sentiment indicator
    #See on juba sesoonselt kohandatud!
    df_temp<- get_eurostat("ei_bssi_m_r2", filters = list(geo = "EE", indic=c("BS-ESI-I")))
    
    majandususaldusindeks <- df_temp %>% filter(time>=as.Date("2003-01-01")) %>%
      #kvartali tunnus, nagu ülal ja leiame keskmise
      mutate(quarter= quarter(time), year=year(time)) %>% 
      group_by(year, quarter) %>% 
      summarise(values=mean(values), 
                rank=which.min(time),
                kpv = time[rank]) %>% ungroup() %>% arrange(kpv) %>% 
      #viimase nelja kvartali keskmine, teen käsitsi
      mutate(values=(values+
                       dplyr::lag(values,1)+ 
                       dplyr::lag(values,2)+ 
                       dplyr::lag(values,3))/4) %>% 
      filter(kpv>=as.Date("2004-01-01")) %>% 
      dplyr::select(kpv, values) %>% 
      dplyr::rename(economicsentiment=values) %>%  
      as.data.frame()
    
    #Paneme kõik kokku. Rbind ei tööta, sest ridade arv võib olla erinev. Kuiste andmete juures tegin alguses long-formaadi!
    #Peaks ikkagi mergema
    # df <- cbind(hoivemaar, inflatsioon, keskminepalk,majandususaldusindeks,
    #                   noudlusehituses, noudlusteeninduses, noudlustoostuses, 
    #                   rakendatusemaar, tootusemaar, vabadametikohad)  %>% as.data.frame() 

    #käsuga Reduce
    #Reduce() takes a function f of two arguments and a list or vector x which is to be ‘reduced’ using f.
    #The function is first called on the first two components of x, then with the result of that as the first argument and the third component of x as the second argument, then again with the result of the second step as first argument and the fourth component of x as the second argument etc.
    #The process is continued until all elements of x have been processed.

    tabelid <- list(hoivemaar, inflatsioon, keskminepalk,majandususaldusindeks,
                    noudlusehituses, noudlusteeninduses, noudlustoostuses,
                    rakendatusemaar, tootusemaar, vabadametikohad)
    df <- Reduce(function(x,y) merge(x,y,by="kpv",all=TRUE) ,
                 tabelid) %>%  as.data.frame()

    #Arhiveerimine faili, kus ka kuupäeva nimi juures
    save(df, realskp, 
         file = paste0("data/data.RData", as.character(Sys.Date())))

    save(df, realskp, 
         file = "data/data.RData")
    
    #katsetada
    #showNotification("Andmed uuendatud, soovitan värskendada browseri vaadet", type="message")
    showNotification("Data updated, recommend refreshing brwoser", type="message")
    
    })
  
    
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
  # observeEvent(input$button_weights,  {
  #   updateSliderInput(session, inputId = "weight_coreinflation",label = "Core inflation",value = 0.1, min=0, max=1, step=0.05)
  #   updateSliderInput(session, inputId = "weight_averagewage",label = "Average wage",value = 0.1, min=0, max=1, step=0.05)
  #   updateSliderInput(session, inputId = "weight_unemploymentrate",label = "Unemployment rate",value = 0.1, min=0, max=1, step=0.05)
  #   updateSliderInput(session, inputId = "weight_employmentrate",label = "Employment rate",value = 0.1, min=0, max=1, step=0.05)
  #   updateSliderInput(session, inputId = "weight_vacancies",label = "Vacancies",value = 0.1, min=0, max=1, step=0.05)
  #   updateSliderInput(session, inputId = "weight_capacityutilisation",label = "Capacity utilisation",value = 0.1, min=0, max=1, step=0.05)
  #   updateSliderInput(session, inputId = "weight_demandinconstruction",label = "Demand in construction",value = 0.1, min=0, max=1, step=0.05)
  #   updateSliderInput(session, inputId = "weight_demandinindustry",label = "Demand in industry",value = 0.1, min=0, max=1, step=0.05)
  #   updateSliderInput(session, inputId = "weight_demandinservices",label = "Demand in services",value = 0.1, min=0, max=1, step=0.05)
  #   updateSliderInput(session, inputId = "weight_economicsentiment",label = "Economic sentiment",value = 0.1, min=0, max=1, step=0.05)
  # } 
  # )
  
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
    dfalgus() %>% dplyr::select(kpv, tunnused) %>% #dplyr::select(kpv, all_of(tunnused)) %>%
      gather(key="indicator", value = "value", - kpv)  %>%
      ggplot(aes(x=kpv, y=value, color=indicator)) +
      geom_line() +
      facet_wrap(~indicator, ncol=2, scales = "free", labeller = as_labeller(componentnamesforsourcegraph)) +
      guides(color=FALSE) + labs(y="", x="", caption = "Data: Eurostat, Statistics Estonia \nIdea: Estonian Fiscal Council") +
      theme_bw()+
      theme(text=element_text(size=14))
  })
  
  # output$sourcedategraphplotly <- renderPlotly({
  #   p <- dfalgus() %>% select(Date=kpv, tunnused) %>% 
  #     gather(key="indicator", value = "value", - Date)  %>% 
  #     mutate(Value = round(value,2)) %>% 
  #     ggplot(aes(x=Date, y=Value, color=indicator, group=indicator, 
  #                text = paste('Date: ', as.Date(Date),
  #                             '<br>Value: ', Value))) +
  #     geom_line() +
  #     facet_wrap(~indicator, ncol=2, scales = "free", labeller = as_labeller(componentnamesforsourcegraph)) +
  #     guides(color=FALSE) + labs(y="", x="", caption = "Data: Eurostat, Statistics Estonia \nIdea: Estonian Fiscal Council") +
  #     theme(text=element_text(size=14), legend.position='none',
  #           #strip.text = element_text(size=10, lineheight=0),
  #           strip.background = element_blank())
  #   
  #   ggplotly(p, tooltip=c("text"))
  # })
  
  
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
    #TODO! järgmises kahes peaks funs käsuga midagi tegema, hoiatab, et depreciated
    temp <-  dfalgus() %>% dplyr::select(kpv, tunnused) %>% #dplyr::select(kpv, all_of(tunnused)) %>%
      mutate_at(tunnused, funs((.-mean(., na.rm=TRUE))/sd(., na.rm=TRUE))) %>% 
      #mutate_at(all_of(tunnused), funs((.-mean(., na.rm=TRUE))/sd(., na.rm=TRUE))) %>% 
      #turn signs
      #osadel pöörame märgi ümber
      mutate_at(vars(one_of(oppvars)),funs(.*(-1))) %>% as.data.frame()
    
    #weighted average
    #Kaalutud keskmise leidmine
    # temp <- temp %>%  mutate(Composite = coreinflation*input$weight_coreinflation + 
    #                            averagewage * input$weight_averagewage+ 
    #                            unemploymentrate*input$weight_unemploymentrate + 
    #                            employmentrate*input$weight_employmentrate+
    #                            vacancies*input$weight_vacancies + 
    #                            capacityutilisation*input$weight_capacityutilisation+ 
    #                            demandinconstruction*input$weight_demandinconstruction+
    #                            demandinindustry*input$weight_demandinindustry +
    #                            demandinservices*input$weight_demandinservices+
    #                            economicsentiment*input$weight_economicsentiment) %>% 
    #   
    #   #if the sum of the weights is not 1, then rescale all weights
    #   #skaleerime, et kaalude efektiivne summa oleks taas üks, kui kasutaja pole seda teinud
    #   mutate(Composite = Composite / (input$weight_coreinflation + input$weight_averagewage+ input$weight_unemploymentrate + input$weight_employmentrate +
    #                                     input$weight_vacancies + input$weight_capacityutilisation+ input$weight_demandinconstruction + input$weight_demandinindustry +
    #                                     input$weight_demandinservices+ input$weight_economicsentiment))
    
    weight_coreinflation = 0.1
    weight_averagewage = 0.1 
    weight_unemploymentrate = 0.1
    weight_employmentrate = 0.1
    weight_vacancies = 0.1
    weight_capacityutilisation = 0.1
    weight_demandinconstruction = 0.1
    weight_demandinindustry = 0.1
    weight_demandinservices = 0.1
    weight_economicsentiment = 0.1
      
    
    temp <- temp %>%  mutate(Composite = coreinflation*weight_coreinflation + 
                               averagewage * weight_averagewage+ 
                               unemploymentrate*weight_unemploymentrate + 
                               employmentrate*weight_employmentrate+
                               vacancies*weight_vacancies + 
                               capacityutilisation*weight_capacityutilisation+ 
                               demandinconstruction*weight_demandinconstruction+
                               demandinindustry*weight_demandinindustry +
                               demandinservices*weight_demandinservices+
                               economicsentiment*weight_economicsentiment) %>% 
      
      #if the sum of the weights is not 1, then rescale all weights
      #skaleerime, et kaalude efektiivne summa oleks taas üks, kui kasutaja pole seda teinud
      mutate(Composite = Composite / (weight_coreinflation + weight_averagewage+ weight_unemploymentrate + weight_employmentrate +
                                              weight_vacancies + weight_capacityutilisation+ weight_demandinconstruction + weight_demandinindustry +
                                              weight_demandinservices+ weight_economicsentiment))
    #long form for graphs
    #pikale kujule
    dfst <- temp %>%  gather(key="indicator", value = "value", - kpv)
    
    dfst <- dfst %>%  
      mutate(indicatorlbl=case_when(
        indicator=="Composite" ~ 'Composite indicator',
        indicator=="coreinflation" ~ 'Core inflation',
        indicator=="averagewage" ~ 'Average wage',
        indicator=="unemploymentrate" ~ 'Unemployment rate',
        indicator=="employmentrate" ~ 'Employment rate',
        indicator=="vacancies" ~ 'Vacancies',
        indicator=="capacityutilisation" ~ 'Capacity utilisation',
        indicator=="demandinconstruction" ~ 'Demand in construction',
        indicator=="demandinindustry" ~ 'Demand in industry',
        indicator=="demandinservices" ~ 'Demand in services',
        indicator=="economicsentiment" ~ 'Economic sentiment'))
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
  
  # output$stdatagraphplotly <- renderPlotly({
  #   p <- ggplot(data=dfst()[dfst()$indicator!="Composite",], aes(x=kpv, y=value, color=indicatorlbl)) +
  #     geom_line() +
  #     geom_line(data=dfst()[dfst()$indicator=="Composite",], aes(x=kpv, y=value), size=1.5) +
  #     labs(y="", x="", caption = "Data: Eurostat, Statistics Estonia \nIdea: Estonian Fiscal Council \n\nFor variables 'Unemployment rate', 'Demand in construction', 'Demand in industry', 'Demand in services' \nsign has been changed", color="") +
  #     theme(text=element_text(size=14))
  #   ggplotly(p)
  # })

    
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
    #tempdf <- realskp %>%  filter(kohandatud==input$gdpserieschoice) 
    tempdf <- realskp %>%  filter(kohandatud=="Silutud") 
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
    #hp.realskpdf <- data.frame(HP_tsykkel = (hp.realskp$cycle-mean(hp.realskp$cycle, na.rm=TRUE))/sd(hp.realskp$cycle, na.rm=TRUE)  , kpv=as.Date(time(tsrealskp())))
    #hp.realskpdf
    
    #hp.realskpdf <- data.frame(HP_tsykkel = (hp.realskp$cycle-mean(hp.realskp$cycle, na.rm=TRUE))/sd(hp.realskp$cycle, na.rm=TRUE)  , kpv=as.Date(time(tsrealskp())))
    #hp.realskpdf
    hp.realskpdf <- data.frame(kpv=as.Date(time(tsrealskp())),
                               indicator = "Outputgap",
                               value = as.numeric(hp.realskp$cycle/hp.realskp$trend*100))   #to check as numeric
    return(hp.realskpdf)
})
  
  #Graph of components of GDP growth rate
  #alumine joonis
  output$hpfiltergraph <- renderPlot({
      req(input$hpsmoothing)
      plot.mFilter(hpfilter(tsrealskp(),freq=input$hpsmoothing,type="lambda",drift=ontrend), ask=FALSE)  
  })
  
  #Graph of comparison of GDP gap and composite index
  #koos joonis
  # output$comparison <- renderPlot({
  #     #Composite andmed
  #     koond <- dfst() %>%  filter(indicator=="Composite") %>%
  #       dplyr::rename(Composite=value) %>%  dplyr::select(kpv, Composite)
  #     
  #     #put together
  #     #kokku panek
  #     dfvordlus <- merge(hp.realskpdf(), koond, by = "kpv", all = TRUE) %>% 
  #       gather(key="indicator", value = "value", - kpv) %>%  filter(kpv>=input$begdate)
  #     #joonis
  #     ggplot(dfvordlus, aes(y=value, x=kpv, color=indicator)) +
  #       geom_line() + 
  #       scale_x_date(date_breaks = "1 year", date_labels= "%m-%Y") +
  #       #facet_wrap(~indicator, scales = "free_y", ncol=1) + guides(color=FALSE) + 
  #       labs(y="", x="",  color= "", caption = "GDP data source: Statistics Estonia, tabel RAA0012, real GDP.
  #             HP filter is always calculated using time series since 1Q 1995, then cyclical component is standardised") +
  #       scale_color_hue(
  #         labels = c('Composite indicator', 'Output gap'),
  #         breaks = c('Composite', 'HP_outputgap')
  #       )
  #   }
  # )
  
  componentnamesforgdpgraph <- c(Outputgap = 'Output gap from HP filter',
                                    Composite = 'Composite indicator')
  
  output$comparison <- renderPlot({
    #Composite andmed
    koond <- dfst() %>%  filter(indicator=="Composite") %>%
      dplyr::select(kpv, indicator, value)
    
    #put together
    #kokku panek
    #vana
    #dfvordlus <- merge(hp.realskpdf(), koond, by = "kpv", all = TRUE) %>% 
    #  gather(key="indicator", value = "value", - kpv) %>%  filter(kpv>=input$begdate)

    dfvordlus <- rbind(hp.realskpdf(), koond) %>% filter(kpv>=input$begdate) 
        
    #joonis
    ggplot(dfvordlus, aes(y=value, x=kpv, color=indicator)) +
      geom_line() + 
      guides(color=FALSE) +
      scale_x_date(date_breaks = "1 year", date_labels= "%m-%Y") +
      facet_wrap(~indicator, scales = "free_y", ncol=1, labeller = as_labeller(componentnamesforgdpgraph)) + 
      theme_bw()+
      theme(text=element_text(size=14)) +
      #facet_wrap(~indicator, scales = "free_y", ncol=1) + guides(color=FALSE) + 
      labs(y="", x="", title = "Composite indicator and GDP output gap", color= "", caption = "GDP data source: Statistics Estonia, tabel RAA0012, real GDP.
           HP filter is always calculated using time series since 1Q 1995") #, then cyclical component is standardised
      #scale_color_hue(
      #  labels = c('Output gap', 'Composite indicator'),
      #  breaks = c( 'Outputgap', 'Composite')
      #)
  }
  )
  
  ## Tabel tekstiga
  output$moisted <- renderTable(moisted, striped = TRUE, na = 'missing')
  
  #output$ajutine <- DT::renderDataTable({
  #    DT::datatable(data=TK_data)
  #})
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)