#Data preparation for the Estonian heatmap
#Andmete ettevalmistus majanduse termomeetriks
#Idee autor, Lauri Punga, Eesti Pank, Eelarvenõukogu
#R kood, Andres Võrk, TÜ, Eelarvenõukogu

setwd("C:/Users/avork/Documents/Eelarvenoukogu/EstonianHeatmap")

#TODO!
#1. Läbi rääkida Lauriga, kas tasub kasutada Eurostati andmeid võrreldes KI-ga
#2. Paneks SKP samasse tabelisse ja võtaks vaid sesoonselt silutud
#3. Üldisem - teha enda ESA tabeli funktsioon paindlikumaks
#4. Vakantside puhul - Eesti andmete puhul tehtud vanas ja uues andmefailis silumised ja kokku pandud. Võimalik enne kokku panna ja siis silida.
#5. Tööpuuduse ja hõive määrad saaks ka Eurostatist tuua
#6. Eurostati vakantsides lüngad Eesti jaoks

library(dplyr)
library(lubridate)

# National GDP
# SKP
# Andrese kirjutatud funktsioonid ESA andmebaasist automaatseks laadimiseks
# Code to download data from Statistics Estonia database
# alati ei tööta, sest ESAl tabelite süsteem muutmas
source("https://raw.githubusercontent.com/AndresVork/rsdmx_esa/master/rsdmx_esa_fn.R")

#GDP dowload from Statistics Estonia server
skp <- rsdmx_esa("RAA0012")
#salvestan selle RDS objektina, võib teha ka muud
realskp <- skp %>% filter(DIM4==2,DIM2 !="1") %>%
  mutate(kvartal = as.numeric(substr(DIM2label.en, 1, 1)),
         kpv=as.Date(paste0(obsTime, "-", as.character(as.numeric(substr(DIM2label.en, 1, 1))*3-2), "-01")),
         aasta=as.numeric(obsTime)) %>%
  mutate(kohandatud = ifelse(DIM3==1, "Silumata", "Silutud")) %>%
  arrange(aasta, kvartal) %>%
  dplyr::select(aasta, kvartal, kohandatud, obsValue, kpv)

#Soovi korral salvesta ja/või lae sisse
#Save GDP data  to data subfolder
#saveRDS(realskp, file= "data/realskp")


#Komponendid
#1. 'Core inflation (annualised)'
#Alusinflatsioon	Ühtlustatud tarbijahinnaindeks, v.a. energia, toit, alkohol ja tubakas
#muutus võrreldes eelmise aasta sama kvartaliga, kvartal=kolme kuu keskmine	Eurostat
# In Euro Area, the core inflation rate is calculated using the weighted average of the Harmonised Index of Consumer Price (HICP) aggregates, 
#excluding energy, food, alcohol & tobacco that face volatile price movements. 

#TOT_X_NRG_FOOD	Overall index excluding energy, food, alcohol and tobacco
library(eurostat)
df_temp<- get_eurostat("prc_hicp_midx", filters = list(geo = "EE", coicop="TOT_X_NRG_FOOD", unit="I15")) %>%
  filter(time>=as.Date("2003-01-01")) %>%  select(time, values) %>% 
  rename(coreinflation=values)
  
inflatsioon <- df_temp %>%   mutate(quarter= quarter(time), year=year(time)) %>% 
  group_by(year, quarter) %>% 
  summarise(coreinflation=mean(coreinflation), 
            rank=which.min(time),
            kpv = time[rank]) %>% ungroup() %>% 
  mutate(coreinflation=(coreinflation-dplyr::lag(coreinflation,4))/dplyr::lag(coreinflation,4)*100) %>% 
  filter(kpv>=as.Date("2004-01-01")) %>% 
  select(kpv, coreinflation) %>% as.data.frame()
plot(inflatsioon, type="o")

#2. Keskmine palk	Keskmine brutokuupalk, tegevusalad kokku	
#muutus võrreldes eelmise aasta sama kvartaliga	- statistikaamet
#"PA001: Keskmine brutopalk, tööjõukulu, töötatud tunnid ja töötajate arv tegevusalarühma järgi (kvartalid)"
#suur tabel kuna filtreid ei ole peal

#Sellel tabelil on ESA muutnud struktuuri! Ei kasuta enam nimetusi DIM1, DIM2 jne
mytable<- "PA001"
tf <- tempfile(tmpdir = tdir <- tempdir())
download.file(paste0("http://andmebaas.stat.ee/restsdmx/sdmx.ashx/GetData/", mytable, "/all"), tf)
#Aeglane, kuna töötab suure tabeliga alguses
df_temp <- readSDMX(tf, isURL = FALSE) %>% as.data.frame()

keskminepalk <- df_temp %>%  filter(INDICATOR=="D11_EMPL", NACE_R2=="TOTAL") %>% 
  mutate(kpv=as.Date(paste0(substr(obsTime,1,4), "-", as.character(as.numeric(substr(obsTime,7,7))*3-2), "-1"))) %>% 
  #kasvumäär
  arrange(kpv) %>% 
  mutate(averagewage=(obsValue-dplyr::lag(obsValue,4))/dplyr::lag(obsValue,4)*100) %>% 
    select(kpv, averagewage) %>%  
  filter(kpv>=as.Date("2004-01-01")) %>% 
  as.data.frame()

#3. Töötuse määr	Töötuse määr vanuses 15-74	tase	statistikaamet
#kasutame enda funktsiooni
df_temp <- rsdmx_esa("TT461")
tootusemaar <- df_temp %>%  filter(DIM3==8) %>% 
  mutate(kvartal = as.numeric(substr(DIM2label.en,1,1))) %>% 
  mutate(kpv=as.Date(paste0(as.character(obsTime), "-", as.character(kvartal*3-2), "-1"))) %>% 
  rename(unemploymentrate=obsValue) %>% 
  filter(kpv>=as.Date("2004-01-01")) %>% 
  select(kpv, unemploymentrate) %>%  arrange(kpv)
  
#4. Hõive määr	Tööhõive määr vanuses 15-74	tase	statistikaamet, samast tabelist
hoivemaar <- df_temp %>%  filter(DIM3==7) %>% 
  mutate(kvartal = as.numeric(substr(DIM2label.en,1,1))) %>% 
  mutate(kpv=as.Date(paste0(as.character(obsTime), "-", as.character(kvartal*3-2), "-1"))) %>% 
  rename(employmentrate=obsValue) %>% 
  filter(kpv>=as.Date("2004-01-01")) %>% 
  select(kpv, employmentrate) %>%  arrange(kpv)

#5. Vabad ametikohad	Vabade ametikohtade arv, tegevusalad kokku	tase	statistikaamet
#need andmed on olemas alates 2008 andmebaasis. Vanad andmed peab võtma teisest tabelist
df_temp <- rsdmx_esa("PAV011")
vabadametikohadal2008 <- df_temp %>%  filter(DIM3==20, DIM4==1) %>% 
  mutate(kvartal = as.numeric(substr(DIM2label.en,1,1))) %>% 
  mutate(kpv=as.Date(paste0(as.character(obsTime), "-", as.character(kvartal*3-2), "-1"))) %>% 
  rename(vacancies=obsValue) %>% 
  select(kpv, vacancies) %>%  arrange(kpv)

#Vanad andmed - seda ei peaks käima enam pärimas, kui on maha salvestatud
df_temp <- rsdmx_esa("PAV11")
vabadametikohadkuni2007 <- df_temp %>%  filter(DIM3=="580400000", DIM4=="23123") %>%
  mutate(kvartal = as.numeric(substr(DIM2label.en,1,1))) %>%
  mutate(kpv=as.Date(paste0(as.character(obsTime), "-", as.character(kvartal*3-2), "-1"))) %>%
  rename(vacancies=obsValue) %>% filter(kpv<as.Date("2008-01-01")) %>%
  select(kpv, vacancies) %>%  arrange(kpv)

saveRDS(vabadametikohadkuni2007, file= "data/vabadametikohadkuni2007")
vabadametikohadkuni2007 <- readRDS(file= "data/vabadametikohadkuni2007")

vabadametikohad<- rbind(vabadametikohadkuni2007, vabadametikohadal2008) %>%  
  arrange(kpv)
rm(vabadametikohadkuni2007, vabadametikohadal2008)


###########
#Vabad ametikohad Eurostatist, kui soovida laiendada teiste riikide kohta
df_temp<- get_eurostat("jvs_q_nace2", filters = list(geo = "EE", indic_em=c("JOBVAC"),
                                                      s_adj = "NSA", nace_r2="A-S", sizeclas="TOTAL"))
vabadametikohadal2008eurostat <- df_temp %>% 
  filter(time>=as.Date("2008-01-01")) %>%
  rename(kpv=time) %>%  select(kpv, values) %>% arrange(kpv) %>%  as.data.frame()
#2009 1. kvartal ja 2015 1. kvartal oli lünk! 

df_temp<- get_eurostat("jvs_q_nace1", filters = list(geo = "EE", indic_em=c("JOBVAC"),
                                                     s_adj = "NSA", nace_r1="TOTAL", sizeclas="TOTAL"))
vabadametikohadkuni2007eurostat <- df_temp %>% 
  rename(kpv=time) %>%
  filter(kpv>=as.Date("2004-01-01") & kpv<as.Date("2008-01-01"))  %>% 
  select(kpv, values) %>% arrange(kpv) %>%  as.data.frame()

vabadametikohadeurostat<- rbind(vabadametikohadkuni2007eurostat, vabadametikohadal2008eurostat) %>%  
  arrange(kpv) %>% 
  mutate(values=(values+ 
                   dplyr::lag(values,1)+ 
                   dplyr::lag(values,2)+ 
                   dplyr::lag(values,3))/4) %>% 
  rename(vacancies=values)
rm(vabadametikohadkuni2007eurostat, vabadametikohadal2008eurostat)


#6. Rakendatuse määr	Tootmisvõimsuse rakendatuse määr töötlevas tööstuses,  
#sesoonselt silutud	tase (!)	Eurostat
#BS-ICU-PC	Current level of capacity utilization (%)
df_temp<- get_eurostat("ei_bsin_q_r2", filters = list(geo = "EE", indic=c("BS-ICU-PC", "BS-FLP2-PC"),
                                                           s_adj = "SA"))

rakendatusemaar <- df_temp %>% filter(indic=="BS-ICU-PC", time>=as.Date("2004-01-01")) %>%
  rename(kpv=time, capacityutilisation=values) %>%  select(kpv, capacityutilisation) %>% arrange(kpv)

#7. Nõudlus tööstuses	Tööstusbaromeeter: ebapiisav nõudlus kui kõige olulisem toodangu kasvu piirav tegur	tase, nelja kvartali keskmine	Konjunktuuriinstituut
# Eurostatis ei ole kuist andmestikku, kus oleks piirav tegur antud
#Kasutan otse kvartalit, kuid see ei tule täpselt sama, mis on Lauril
df_temp<- get_eurostat("ei_bsin_q_r2", filters = list(geo = "EE", indic=c("BS-FLP2-PC"),
                                                      s_adj = "NSA"))
noudlustoostuses <- df_temp %>% filter(indic=="BS-FLP2-PC", time>=as.Date("2003-01-01")) %>%
  rename(kpv=time) %>%  select(kpv, values) %>% arrange(kpv) %>% 
  mutate(values=(values+ 
                   dplyr::lag(values,1)+ 
                   dplyr::lag(values,2)+ 
                   dplyr::lag(values,3))/4) %>% 
  rename(demandinindustry=values)   %>%   
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
  select(kpv, values) %>% 
  rename(demandinconstruction=values) %>%  
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
  rename(kpv=time) %>%  select(kpv, values) %>% arrange(kpv) %>% 
  mutate(values=(values+ 
                   dplyr::lag(values,1)+ 
                   dplyr::lag(values,2)+ 
                   dplyr::lag(values,3))/4) %>% 
  rename(demandinservices=values)   %>%   
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
  select(kpv, values) %>% 
  rename(economicsentiment=values) %>%  
  as.data.frame()

#Paneme kõik kokku käsuga Reduce

#Reduce() takes a function f of two arguments and a list or vector x which is to be ‘reduced’ using f. 
#The function is first called on the first two components of x, then with the result of that as the first argument and the third component of x as the second argument, then again with the result of the second step as first argument and the fourth component of x as the second argument etc. 
#The process is continued until all elements of x have been processed. 

tabelid <- list(hoivemaar, inflatsioon, keskminepalk,majandususaldusindeks,
                     noudlusehituses, noudlusteeninduses, noudlustoostuses, 
                     rakendatusemaar, tootusemaar, vabadametikohad) 
df <- Reduce(function(x,y) merge(x,y,by="kpv",all=TRUE) ,
       tabelid) %>%  as.data.frame()

#Arhiveerimine faili, kus ka kuupäeva nimi juures
save(df, tabelid, realskp, file = paste0("C:/Users/avork/Documents/Eelarvenoukogu/EstonianHeatmap/data/data", as.character(Sys.Date())))

