load("~/Studium 2021/3. Semester/seminar/20221223miete.rda")

`%ni%` <- Negate(`%in%`)

tbl<-tbl %>%
  rowwise() %>%
  mutate(heizkosten = as.numeric(heizkosten),
         nebenkosten=as.numeric(nebenkosten),
         garage=as.numeric(garage),
         plz=ifelse(plz=="Auf Karte anzeigen",strasse,plz),
         strasse=ifelse(plz==strasse,NA,strasse)) %>% 
  tibble()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Fall miete zuzüglich > miete inkl
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tbl %>%
  select(miete_inkl,miete_zzgl,heizkosten,garage,nebenkosten,id) %>%
  filter(miete_inkl < miete_zzgl)

# Datensatz 1,2,3 und 5 offensichtlich vertauscht 
# Datensatz 4 nicht eindeutig identifizierbar (kommen raus)
tmp<-tbl %>%
  filter(miete_inkl < miete_zzgl) %>%
  slice(1,2,3,5) %>%
  mutate(heizkosten = as.numeric(heizkosten),
         nebenkosten=as.numeric(nebenkosten),
         garage=as.numeric(garage),
         a=miete_inkl,
         miete_inkl = miete_zzgl,
         miete_zzgl = a)  %>%
  select(-a)
tbl[which(tbl$id %in% tmp$id),]<-tmp

# Werfe fehlerhaften Datensatz raus
tmp<-tbl %>% filter(miete_inkl < miete_zzgl)
tbl<-tbl %>% filter(id %ni% tmp$id )

# Werfe Daten raus die weder Kalt- noch Warmmiete haben
tbl %>% 
  filter(is.na(tbl$miete_inkl)!=T | is.na(tbl$miete_zzgl)!=T) %>%
  nrow()- nrow(tbl)

tbl<-tbl %>% 
  filter(is.na(tbl$miete_inkl)!=T | is.na(tbl$miete_zzgl)!=T)

sum(tbl$miete_inkl==tbl$miete_zzgl,na.rm = T)

tbl$kaltmiete<-NA
tbl$warmmiete<-NA
tbl$pauschalmiete<-NA
tbl$diff<-NA
tbl$a<-NA
tbl$b<-NA
tbl$c<-NA
tbl$d<-NA
tbl$e<-NA
tbl$f<-NA
tbl$g<-NA
tbl$h<-NA
tbl$i<-NA
tbl$j<-NA
tbl$k<-NA
tbl$l<-NA
tbl$m<-NA
tbl$n<-NA
# Warmmiete = Kaltmiete + NK + Heizkosten (Ohne Garage)
# Kaltmiete = Miete ohne weitere Kosten
tbl$heatcostincludedStatus[is.na(tbl$heatcostincludedStatus)]<-"Nein"
tbl$rentincludingheating[is.na(tbl$rentincludingheating)]<-"Nein"
# tbl %>%
#   filter(rentincludingheating=="Ja" & !is.na(heizkosten)) %>%
#   select(miete_inkl,miete_zzgl,nebenkosten,heizkosten,garage, kaltmiete, warmmiete,id) %>%
#   mutate(warmmiete=miete_zzgl+nebenkosten+heizkosten,
#          bool= warmmiete-miete_inkl)  %>%
#   arrange(desc(bool))
# 
# 
# tbl %>%
#   filter(heatcostincludedStatus=="Ja" & !is.na(heizkosten) ) %>%
#   select(id,miete_inkl,miete_zzgl,nebenkosten,heizkosten,garage, kaltmiete, warmmiete) %>%
#   mutate(warmmiete=miete_zzgl+nebenkosten,
#          bool= warmmiete-miete_inkl)  %>%
#   arrange((bool))
# 
# tbl %>%
#   filter(heatcostincludedStatus=="Nein" & rentincludingheating=="Nein" ) %>%
#   select(id,miete_inkl,miete_zzgl,nebenkosten,heizkosten,garage, kaltmiete, warmmiete) %>%
#   mutate(warmmiete=miete_zzgl+nebenkosten,
#          bool= warmmiete-miete_inkl)  %>%
#   arrange((bool))
# 
# tbl %>%
#   filter(is.na(d) & !is.na(garage) & is.na(a) & is.na(b) & is.na(c)) %>%
#   select(id,miete_inkl,miete_zzgl,nebenkosten,heizkosten,garage, kaltmiete, warmmiete) %>%
#   mutate(
#     bool= miete_inkl-miete_zzgl-nebenkosten-heizkosten)  %>%
#   arrange((bool))
# 
# tbl %>%
#   filter(is.na(miete_inkl) & rentincludingheating =="Nein" & !is.na(heizkosten)  ) %>%
#   select(id,miete_inkl,miete_zzgl,nebenkosten,heizkosten,garage, kaltmiete, warmmiete) %>%
#   mutate(
#     bool= miete_inkl-miete_zzgl-nebenkosten-heizkosten)  %>%
#   arrange((bool))
# 
# 
# tbl %>%
#   filter(!is.na(miete_zzgl) & is.na(kaltmiete) & abs(diff)>50 ) %>%
#   select(id,miete_inkl,miete_zzgl,diff,nebenkosten,heizkosten,garage, kaltmiete, warmmiete) %>%
#   mutate(
#     d1= miete_inkl-miete_zzgl-nebenkosten-heizkosten,
#     bool= d1==diff)  %>%
#   arrange((bool)) 
# 
# tbl %>%
#   filter(is.na(warmmiete) ) %>%
#   select(id,miete_inkl,miete_zzgl,diff,nebenkosten,heizkosten,garage, kaltmiete, warmmiete) %>%
#   mutate(
#     d1= miete_inkl-miete_zzgl-nebenkosten-heizkosten,
#     bool= d1==diff)  %>%
#   arrange(desc(diff))
# sum(is.na(tbl$kaltmiete))
# sum(is.na(tbl$warmmiete))


for (i in 1:nrow(tbl)) {
  if(!is.na(tbl$miete_inkl[i])){
    tbl$diff[i]<-tbl$miete_inkl[i]-sum(tbl$miete_zzgl[i],tbl$nebenkosten[i],tbl$heizkosten[i],na.rm = T)
  }
  # Überprüfe  heatcostincludedStatus 
  
  if(tbl$heatcostincludedStatus[i]=="Ja" & !is.na(tbl$miete_inkl[i])){
    # richtige Daten
    if(tbl$miete_inkl[i]==sum(tbl$miete_zzgl[i],tbl$nebenkosten[i],na.rm = T)){
      tbl$kaltmiete[i]<- tbl$miete_zzgl[i]
      tbl$warmmiete[i]<- tbl$miete_inkl[i]
      tbl$a[i]<-T
    }else{
      # Wenn falsch, aber die Summe hier korrekt, dann war wahrscheinlich
      # 'in Warmmiete enthalten' gemeint
      if(tbl$miete_inkl[i]==sum(tbl$miete_zzgl[i],tbl$nebenkosten[i],tbl$heizkosten[i],na.rm = T)){
        tbl$kaltmiete[i]<- tbl$miete_zzgl[i]
        tbl$warmmiete[i]<- tbl$miete_inkl[i]
        tbl$a[i]<-T
      }
    }
    
  }
  
  # Überprüge Heizkosten in Warmmiete
  if(tbl$rentincludingheating[i]=="Ja" & !is.na(tbl$miete_inkl[i]) & is.na(tbl$a[i])){
    # richtige Daten
    if(tbl$miete_inkl[i]==sum(tbl$miete_zzgl[i],tbl$nebenkosten[i],tbl$heizkosten[i],na.rm = T)){
      tbl$kaltmiete[i]<- tbl$miete_zzgl[i]
      tbl$warmmiete[i]<- tbl$miete_inkl[i]
      tbl$b[i]<-T
    }else{
      # Wenn falsch, aber die Summe ohne HK korrekt, dann war wahrscheinlich
      # 'in Nebenkosten enthalten' 
      if(tbl$miete_inkl[i]==sum(tbl$miete_zzgl[i],tbl$nebenkosten[i],na.rm = T)){
        tbl$kaltmiete[i]<- tbl$miete_zzgl[i]
        tbl$warmmiete[i]<- tbl$miete_inkl[i]
        tbl$b[i]<-T
      }
    }
    
  }
  
  # Überprüge Heizkosten in Warmmiete
  if(tbl$rentincludingheating[i]=="Nein" & tbl$heatcostincludedStatus[i]=="Nein" & !is.na(tbl$miete_inkl[i])){
    # richtige Daten
    if(tbl$miete_inkl[i]==sum(tbl$miete_zzgl[i],tbl$nebenkosten[i],tbl$heizkosten[i],na.rm = T)){
      tbl$kaltmiete[i]<- tbl$miete_zzgl[i]
      tbl$warmmiete[i]<- tbl$miete_inkl[i]
      tbl$c[i]<-T
    } 
  }
  if(!is.na(tbl$garage[i]) & !is.na(tbl$miete_inkl[i])){
    if(tbl$miete_inkl[i]==sum(tbl$miete_zzgl[i],tbl$nebenkosten[i],tbl$heizkosten[i],garage[i],na.rm = T)){
      tbl$kaltmiete[i]<- tbl$miete_zzgl[i]
      tbl$warmmiete[i]<- tbl$miete_inkl[i]
      tbl$d[i]<-T
    }
  }
  
  if(is.na(tbl$miete_inkl[i]) & tbl$heatcostincludedStatus[i] =="Ja"){
    tbl$kaltmiete[i]<- tbl$miete_zzgl[i]
    tbl$warmmiete[i]<- sum(tbl$miete_zzgl[i],tbl$nebenkosten[i],garage[i],na.rm = T)
    tbl$e[i]<-T
  }
  if(is.na(tbl$miete_inkl[i]) & tbl$heatcostincludedStatus[i] =="Nein"){
    tbl$kaltmiete[i]<- tbl$miete_zzgl[i]
    tbl$warmmiete[i]<- sum(tbl$miete_zzgl[i],tbl$nebenkosten[i],tbl$heizkosten[i],garage[i],na.rm = T)
    tbl$e[i]<-T
  }
  if(is.na(tbl$kaltmiete[i]) & !is.na(tbl$miete_zzgl[i]) & !is.na(tbl$heizkosten[i])){
    
    # Annahme "HK in NK nicht angegeben"
    if(abs(tbl$diff[i]) == tbl$heizkosten[i] ){
      tbl$kaltmiete[i]<- tbl$miete_zzgl[i]
      tbl$warmmiete[i]<- tbl$miete_inkl[i]
      tbl$f[i]<-T
    } 
  }
  # Differenz wird einfach akzeptiert
  if(is.na(tbl$kaltmiete[i]) & !is.na(tbl$miete_zzgl[i])){
    tbl$kaltmiete[i]<- tbl$miete_zzgl[i]
    tbl$warmmiete[i]<- tbl$miete_inkl[i]
    tbl$g[i]<-T
  }
  
}

# Es bleiben Datensätze übrige bei denen die inkl Daten fehlen und keine Informationen ersichtlich sind
# Diese werden entfernt
tbl<-tbl %>% filter(!is.na(tbl$kaltmiete))
tbl$pauschalmiete<-tbl$kaltmiete==tbl$warmmiete

sum(is.na(tbl$kaltmiete) )
sum(is.na(tbl$warmmiete))
sum(is.na(tbl$miete_inkl))
sum(!is.na(tbl$b))
sum(tbl$g,na.rm = T)
tbl$f<-NA
tbl %>% 
  filter(d==T) %>%
  select(miete_inkl,miete_zzgl,nebenkosten,heizkosten,garage, kaltmiete, warmmiete,id,diff)  %>%
  View()

tbl$energieverbrauch<-gsub(tbl$energieverbrauch,pattern = "kWh.*",replacement = "") %>%
  as.numeric()
library(tibble)
library(dplyr)
tbl[is.na(tbl$zustand),"zustand"]<-"keine Angabe"
tbl[is.na(tbl$denkmalschutz),"denkmalschutz"]<-"FALSE"
tbl[tbl$denkmalschutz!="FALSE","denkmalschutz"]<-"TRUE"
tbl[is.na(tbl$dokumente),"dokumente"]<-0
tbl[is.na(tbl$heizungsart),"heizungsart"]<-"keine Angabe"
tbl[is.na(tbl$energieklasse),"energieklasse"]<-"keine Angabe"
tbl[is.na(tbl$energieverbrauchsausweis),
    "energieverbrauchsausweis"]<-"keine Angabe"

tbl2<-tbl %>%
  mutate(ort = as.factor(ort),
         art= as.factor(art),
         kaution = as.numeric(kaution),
         zustand=as.factor(zustand),
         baujahr=as.Date(baujahr %>% as.character(),"%Y"),
         denkmalschutz=as.factor(denkmalschutz),
         energieklasse=as.factor(energieklasse),
         energieverbrauchsausweis=as.factor(energieverbrauchsausweis),
         heizungsart=as.factor(heizungsart),
         befeuerungsart=as.factor(befeuerungsart),
         energiestandard=as.factor(energiestandard),
         firma=as.factor(firma),
         stadt=as.factor(stadt))%>%
  select(stadt, ort, strasse, plz, art, flaeche,zimmer,zustand,baujahr,denkmalschutz,energieklasse,energieverbrauch, #12
         energieverbrauchsausweis,energiestandard,heizungsart,befeuerungsart,fotos,dokumente,verfuegbar, #19
         firma,anbieter,anbieter_str,anbieter_plz,ansprechpartner,ausstattung,objekt,lage,sonstiges,id,kaltmiete,warmmiete,pauschalmiete)

tbl2$qm_preis<-tbl2$warmmiete/tbl2$flaeche

## Es wurden Daten ausgelesen, die außerhalb der Städte befinden, das liegt daran, dass auf der letzten Seite
# bei immonet Immobilien aus der Umgebung angezeigt werden. 
# Diese werden hiermit entfernt
t<-substr(tbl2$art,5,6)
t<-as.numeric(t)
tbl2<-tbl2[is.na(t),]
tbl2$art <- tbl2$art %>% as.character() %>% as.factor()
save(tbl2,file="20221223miete2.rda")
load("~/Studium 2021/3. Semester/seminar/20221223miete2.rda")