library(rvest)
library(xts) 
library(xml2) 
library(plotly)
library(tibble)
library(dplyr)
tbl<-tibble(ort=NA,
            art=NA,
            fotos=NA,
            strasse=NA,
            plz=NA,
            miete_zzgl=NA,
            miete_inkl=NA,
            nebenkosten=NA,
            heizkosten=NA,
            kaution=NA,
            garage=NA,
            rentincludingheating =NA,
            heatcostincludedStatus =NA,
            zimmer=NA,
            flaeche=NA,
            zustand=NA,
            baujahr=NA,
            denkmalschutz=NA,
            verfuegbar=NA,
            energieklasse=NA,
            energieverbrauch=NA,
            energieverbrauchsausweis=NA,
            heizungsart=NA,
            befeuerungsart=NA,
            energiestandard=NA,
            ausstattung=NA,
            objekt=NA,
            lage=NA,
            sonstiges=NA,
            ansprechpartner=NA,
            anbieter=NA,
            firma=NA,
            anbieter_str=NA,
            anbieter_plz=NA,
            dokumente=NA,
            id=NA,
            stadt=NA)
#tbl<-tbl[-1,]
city<-c("143262","121673","87372",
         "131403","88038","109447",
         "152652","142265",

        "109489","100207","120829",
         "141582","100051","120801",
         "112355","102053","107196",

         "105043","123323","116172")

stadt<-c("Stuttgart","München","Berlin",
         "Potsdam","Bremen","Hamburg",
         "Wiesbaden","Schwerin",
         "Hannover","Düsseldorf","Mainz",
         "Saarbrücken","Dresden","Magdeburg",
         "Kiel","Erfurt","Garmisch-Partenkirchen",
         "Frankfurt","Nürnberg","Leipzig")

for(m in 1:20){
  page<-paste0("https://www.immonet.de/immobiliensuche/sel.do?suchart=2&fromarea=1.0&torooms=9999.0&city=",city[m],"&marketingtype=2&pageoffset=1&radius=0&parentcat=1&listsize=26&sortby=0&objecttype=1&fromrooms=1.0&fromprice=1.0&page=1")
  page<-read_html(page)
  n<-page %>%
    html_elements("#totalCount") %>%
    html_text2() %>%
    strsplit(split = " ") %>%
    .[[1]] %>%
    .[1] %>%
    as.numeric()/26
  n<-trunc(n)
  CITY=stadt[m]
  for(j in 1:n){
    
    page<-paste0("https://www.immonet.de/immobiliensuche/sel.do?suchart=2&fromarea=1.0&torooms=9999.0&city=",city[m],"&marketingtype=2&pageoffset=1&radius=0&parentcat=1&listsize=26&sortby=0&objecttype=1&fromrooms=1.0&fromprice=1.0&page=",j)
    page<-read_html(page)
    
    # Ort
    orte<-page %>%
      html_elements(".box-25.ellipsis .text-100") %>%
      html_text2() %>%
      strsplit("•") %>%
      sapply("[[",2) %>%
      gsub(pattern = paste("",CITY,""),replacement = "")
    #art
    arten<-page %>%
      html_elements(".box-25.ellipsis .text-100") %>%
      html_text2() %>%
      strsplit(" •") %>%
      sapply("[[",1) 
    # ID`s`
    ids<-page %>%
      html_elements(".flex-grow-1.overflow-hidden.box-25 a")%>%
      html_attr("href")
    pages2<-paste0("https://immonet.de",ids)
   
    
    for( i in 1:length(pages2)){
      pages<-tryCatch(read_html(pages2[i]), error = function(e) e, finally = NA)
      
      if("message" %in% names(pages)) next
      
      pages<-xml_child(pages, 2)
      
      ort<-orte[i]
      art<-arten[i]
      # Fotos
      safe<-pages %>% 
        html_elements("#fotorama div") %>%
        length()
      fotos<-ifelse(length(safe)==0,NA,safe)
      rm(safe)
      # Straße
      safe<-pages %>% 
        html_elements(".text-100.pull-left") %>%
        html_text2()
      
      strasse<-NA
      plz<-NA
      safe<-ifelse(length(safe)==0,"XXX",safe)
      if(length(safe)!=0 & safe != "XXX"){
        
        safe<-pages %>% 
          html_elements(".text-100.pull-left") %>%
          html_text2() %>%
          strsplit("\n") %>%
          .[[1]] %>%
          .[1] %>%
          gsub(pattern = paste("",CITY),replacement = "")
      }
      
      if(substr(safe,1,1)==1 ){
        #Straße
        strasse<-NA
        # PLZ
        safe<-pages %>% 
          html_elements(".text-100.pull-left") %>%
          html_text2() %>%
          strsplit("\n") %>%
          .[[1]] %>%
          .[1] %>%
          gsub(pattern = paste("",CITY),replacement = "")
        plz<-ifelse(length(safe)==0,NA,safe)
        
      }else{
        if(safe != "XXX"){
          straße<-ifelse(is.na(safe),NA,safe)
          # Straße
          safe<-pages %>% 
            html_elements(".text-100.pull-left") %>%
            html_text2() %>%
            strsplit("\n") %>%
            .[[1]] %>%
            .[1] %>%
            gsub(pattern = paste("",CITY),replacement = "")
          strasse<-ifelse(length(safe)==0,NA,safe)
          
          # PLZ
          safe<-pages %>% 
            html_elements(".text-100.pull-left") %>%
            html_text2() %>%
            strsplit("\n") %>%
            .[[1]] %>%
            .[2] %>%
            gsub(pattern = paste(" ",CITY),replacement = "")  
          plz<-ifelse(length(safe)==0,NA,safe)
          
        }
      }
      
      #Miete zzgl 
      miete_zzgl<-NA
      rm(safe)
      safe<-pages %>% 
        html_elements("#priceid_2") %>%
        html_text2() %>%
        gsub(pattern = " €",replacement = "") %>%
        as.numeric()
      miete_zzgl<-ifelse(length(safe)==0,NA,safe)
      
      #Miete inkl  
      miete_inkl<-NA
      rm(safe)
      safe<-pages %>% 
        html_elements("#priceid_4") %>%
        html_text2() %>%
        gsub(pattern = " €",replacement = "") %>%
        as.numeric()
      miete_inkl<-ifelse(length(safe)==0,NA,safe)
      # Nebenkosten
      rm(safe)
      nebenkosten<-NA
      safe<-pages %>% 
        html_elements("#priceid_20") %>%
        html_text2() %>%
        gsub(pattern = " €",replacement = "")
      nebenkosten<-ifelse(length(safe)==0,NA,safe)
      # Heizkosten
      rm(safe)
      heizkosten<-NA
      safe<-pages %>% 
        html_elements("#priceid_5") %>%
        html_text2() %>%
        gsub(pattern = " €",replacement = "")
      heizkosten<-ifelse(length(safe)==0,NA,safe)
      # Kaution
      rm(safe)
      kaution<-NA
      safe<-pages %>% 
        html_elements("#priceid_19") %>%
        html_text2() %>%
        gsub(pattern = " €",replacement = "")
      kaution<-ifelse(length(safe)==0,NA,safe)
      # Garage
      rm(safe)
      garage<-NA
      safe<-pages %>% 
        html_elements("#priceid_12") %>%
        html_text2() %>%
        gsub(pattern = " €",replacement = "")
      garage<-ifelse(length(safe)==0,NA,safe)
      
      #  HK in Nebenkosten enthalten
      rm(safe)
      inhk<-NA
      safe<-pages %>% 
        html_elements("#rentincludingheating") %>%
        html_text2() 
      rentincludingheating<-ifelse(length(safe)==0,NA,safe)
      
      # Courtage 
      rm(safe)
      heatcostincludedStatus<-NA
      safe<-pages %>% 
        html_elements("#heatcostincludedStatus ") %>%
        html_text2()
      heatcostincludedStatus <-ifelse(length(safe)==0,NA,safe)
      
      # Zimmer
      rm(safe)
      safe<-pages %>% 
        html_elements("#equipmentid_1") %>%
        html_text2() %>%
        as.numeric()
      zimmer<-ifelse(length(safe)==0,NA,safe)
      
      # Fläche
      rm(safe)
      safe<-pages %>% 
        html_elements("#areaid_1") %>%
        html_text2() %>%
        gsub(pattern = " m²",replacement = "") %>%
        as.numeric()
      flaeche<-ifelse(length(safe)==0,NA,safe)
      
      # Zustand
      rm(safe)
      safe<-pages %>% 
        html_elements("#objectstatecategoryValue") %>%
        html_text2() 
      zustand<-ifelse(length(safe)==0,NA,safe)
      
      # Baujahr
      rm(safe)
      safe<-pages %>% 
        html_elements("#yearbuild") %>%
        html_text2()
      baujahr<-ifelse(length(safe)==0,NA,safe)
      
      # Denkmalschutz
      rm(safe)
      safe<-pages %>% 
        html_elements("#monumentProtectionName") %>%
        html_text2()
      denkmalschutz<-ifelse(length(safe)==0,NA,safe)
      
      # Verfügbar ab
      rm(safe)
      safe<-pages %>% 
        html_elements("#deliveryValue") %>%
        html_text2()
      verfuegbar<-ifelse(length(safe)==0,NA,safe)
      
      # Energieklasse
      rm(safe)
      safe<-pages %>% 
        html_elements("#efficiencyValue") %>%
        html_text2()
      energieklasse<-ifelse(length(safe)==0,NA,safe)
      
      # Energieverbrauch
      rm(safe)
      safe<-pages %>% 
        html_elements("#energyValue") %>%
        html_text2()
      energieverbrauch<-ifelse(length(safe)==0,NA,safe)
      
      # Energieverbrauchsausweis
      rm(safe)
      safe<-pages %>% 
        html_elements("#electricityConsumptionValue") %>%
        html_text2()
      energieverbrauchsausweis<-ifelse(length(safe)==0,NA,safe)
      
      # Heizungsart
      rm(safe)
      safe<-pages %>% 
        html_elements("#heatTypeValue") %>%
        html_text2()
      heizungsart<-ifelse(length(safe)==0,NA,safe)
      
      # Befeuerungsart
      rm(safe)
      safe<-pages %>% 
        html_elements("#heaterSupplierValue") %>%
        html_text2()
      befeuerungsart<-ifelse(length(safe)==0,NA,safe)
      
      # Energiestandard
      rm(safe)
      safe<-pages %>% 
        html_elements("#energiestandardValue") %>%
        html_text2()
      energiestandard<-ifelse(length(safe)==0,NA,safe)
      
      # Ausstattung Anzahl Wörter
      rm(safe)
      safe<-pages %>% 
        html_elements("#panelFeatures") %>%
        html_text2() 
      if(length(safe)!=0){
        safe<-pages %>% 
          html_elements("#panelFeatures") %>%
          html_text2() %>%
          gsub(pattern = "\n",replacement = "") %>%
          strsplit(" ") %>%
          .[[1]] %>%
          length()
      }
      ausstattung<-ifelse(length(safe)==0,NA,safe)
      
      # Objektbeschreibung Anzahl Wörter
      rm(safe)
      safe<-pages %>% 
        html_elements("#objectDescription") %>%
        html_text2() 
      if(length(safe)!=0){
        safe<-pages %>% 
          html_elements("#objectDescription") %>%
          html_text2() %>%
          gsub(pattern = "\n",replacement = "") %>%
          strsplit(" ") %>%
          .[[1]] %>%
          length()
      }
      objekt<-ifelse(length(safe)==0,NA,safe)
      
      # Lage Anzahl Wörter
      rm(safe)
      safe<-pages %>% 
        html_elements("#locationDescription") %>%
        html_text2() 
      if(length(safe)!=0){
        safe<-pages %>% 
          html_elements("#locationDescription") %>%
          html_text2() %>%
          gsub(pattern = "\n",replacement = "") %>%
          strsplit(" ") %>%
          .[[1]] %>%
          length()
      }
      lage<-ifelse(length(safe)==0,NA,safe)
      
      # Sonstiges Anzahl Wörter
      rm(safe)
      safe<-pages %>% 
        html_elements("#otherDescription") %>%
        html_text2() %>%
        gsub(pattern = "\n",replacement = "")
      if (length(safe)!=0) {
        safe<-pages %>% 
          html_elements("#otherDescription") %>%
          html_text2() %>%
          gsub(pattern = "\n",replacement = "")%>%
          strsplit(" ") %>%
          .[[1]] %>%
          length()
      }
      sonstiges<-ifelse(length(safe)==0,NA,safe)
      
      # Ansprechpartner
      rm(safe)
      safe<-pages %>% 
        html_elements("#bdContactName") %>%
        html_text2() 
      ansprechpartner<-ifelse(length(safe)==0,NA,safe)
      
      # Anbieter
      rm(safe)
      safe<-pages %>% 
        html_elements("#bdBrokerName") %>%
        html_text2() 
      anbieter<-ifelse(length(safe)==0,NA,safe)
      
      # Firma
      rm(safe)
      safe<-pages %>% 
        html_elements("#bdBrokerFirmname") %>%
        html_text2() 
      firma<-ifelse(length(safe)==0,NA,safe)
      
      # Anbieter Str
      rm(safe)
      safe<-pages %>% 
        html_elements("#bdBrokerStreet") %>%
        html_text2() 
      anbieter_str<-ifelse(length(safe)==0,NA,safe)
      
      # Anbieter plz
      rm(safe)
      safe<-pages %>% 
        html_elements("#bdBrokerZipCity") %>%
        html_text2() %>%
        gsub(pattern = " .*",replacement = "")
      anbieter_plz<-ifelse(length(safe)==0,NA,safe)
      
      # Dokumente
      rm(safe)
      safe<-pages %>% 
        html_nodes("#panelDocuments") %>%
        html_text2() 
      if(length(safe)!=0){
        safe<-pages %>% 
          html_nodes("#panelDocuments") %>%
          html_text2() %>%
          strsplit("\n\n") %>%
          .[[1]]%>%
          length()
      }
      dokumente<-ifelse(length(safe)==0,NA,safe)
      
      id<-ids[i]
      tbl<-tbl %>%
        add_row(
          tibble(ort,
                 art,
                 fotos,
                 strasse,
                 plz,
                 miete_zzgl,
                 miete_inkl,
                 nebenkosten,
                 heizkosten,
                 kaution,
                 garage,
                 rentincludingheating,
                 heatcostincludedStatus ,
                 zimmer,
                 flaeche,
                 zustand,
                 baujahr,
                 denkmalschutz,
                 verfuegbar,
                 energieklasse,
                 energieverbrauch,
                 energieverbrauchsausweis,
                 heizungsart,
                 befeuerungsart,
                 energiestandard,
                 ausstattung,
                 objekt,
                 lage,
                 sonstiges,
                 ansprechpartner,
                 anbieter,
                 firma,
                 anbieter_str,
                 anbieter_plz,
                 dokumente,
                 id,
                 stadt=CITY)
        )
      gc(reset = T)
    }
  
    
  }
  print(Sys.time())
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
##      Fertig auslesen
##
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tbl<-tbl[-1,]
err<- tbl$plz=="Auf Karte anzeigen" & is.na(tbl$plz)==F
#str definieren!
tbl$strasse[err & is.na(tbl$strasse)]<-"ERR"
tbl$plz[err]<-str
tbl$strasse[err]<-NA

getwd()
setwd("C:/Users/steph/Documents/Studium 2021/3. Semester/seminar")
#write.csv(tbl,"immonet.txt")

#save(tbl,file="20230107miete.rda")

# Januar 2h 20 min Laufzeit 7.500 Daten
