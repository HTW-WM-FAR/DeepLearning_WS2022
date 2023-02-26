#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
# Es ist etwas viel Code geworden, aber ich musste mir alle Daten genau 
# angucken. Im Nachhinein betrachtet hätte man sich Funktionen bauen 
# sollen. Viel Code war häufig einfach kopiert und durch andere Variablen 
# ersetzt.
# 
# In R: Öffne Outliner für Inhaltverzeichnis
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



library(fastDummies)
library(forecast)
library(neuralnet)
library(dplyr)
library(tibble)
library(lubridate)
getwd()
#load("D20221201.rda")
#load("~/immobiliensales/R/data.rda")
load("~/Studium 2021/3. Semester/seminar/20221223miete2.rda")
tbl<-tbl2 %>% filter(id != "/angebot/48714946") 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
#                   Daten umwandeln ####
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

summary(tbl)
str(tbl)
colnames(tbl)
tbl$energieverbrauch<-gsub(
  tbl$energieverbrauch,pattern = "kWh.*",replacement = "") %>%
  as.numeric()

tbl<-tbl %>%
  mutate(baujahr=as.Date(baujahr)) %>%
  select(warmmiete,kaltmiete,flaeche,zimmer,stadt,ort,art,dokumente,fotos,
         zustand,baujahr,denkmalschutz,energieklasse,energieverbrauch,
         energieverbrauchsausweis,energiestandard,
         heizungsart,befeuerungsart,pauschalmiete,id)
summary(tbl)
sapply(tbl, is.factor) %>% sum
sapply(tbl, is.numeric) %>% sum
sapply(tbl, is.character) %>% sum
sapply(tbl, is.Date) %>% sum

tbl2<-tbl
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
#                   Korrelation und Missing Values ####
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(naniar)
library(corrplot)
vis_miss(tbl)
Num.cols <- sapply(
  tbl, 
  is.numeric
  )
Cor.data<-tbl %>%
  .[,Num.cols] %>%
  cor(use = "pairwise")
colnames(Cor.data)<-colnames(Cor.data) %>% 
  toupper()
rownames(Cor.data)<-rownames(Cor.data) %>% 
  toupper()

vis_miss(tbl[,Num.cols])

corrplot.mixed(Cor.data, tl.col="black", tl.pos = "lt",upper.col =F)
corrplot(Cor.data,method = "number",type="upper")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#                    Betrachtung der Features ####
#                            Start:
##                   Warm- und Kaltmiete ####
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(plotly)
plot_ly(tbl,x=~warmmiete,type="histogram") %>%
  layout(
    title="Histogram der Preise",
    xaxis=list(title="Preis"),
    yaxis=list(title="Anzahl")
  )
# Erster Wert der direkt entfernt wird
tbl<-tbl %>% 
  filter(warmmiete<100000)

plot_ly(
  tbl,
  x=~warmmiete,
  type="histogram") %>%
  layout(
    title="Histogram der Preise",
    xaxis=list(title="Preis"),
    yaxis=list(title="Anzahl")
  )

plot_ly(
  tbl,
  x=~kaltmiete,
  type="histogram") %>%
  layout(
    title="Histogram der Preise",
    xaxis=list(title="Preis"),
    yaxis=list(title="Anzahl")
  )

plot_ly(
  tbl,
  x=~log(warmmiete),
  type="histogram") %>%
  layout(
    title="Histogram der Preise",
    xaxis=list(title="Preis"),
    yaxis=list(title="Anzahl")
  )

plot_ly(
  tbl,
  x=~log(kaltmiete),
  type="histogram") %>%
  layout(
    title="Histogram der Preise",
    xaxis=list(title="Preis"),
    yaxis=list(title="Anzahl")
  )

plot_ly(
  tbl,
  x=~log(kaltmiete),
  y=~log(warmmiete),type="scatter",mode="markers") %>%
  layout(
    title="Histogram der Preise",
    xaxis=list(title="Preis"),
    yaxis=list(title="Anzahl")
  )
qqplot(
  log(tbl$kaltmiete),
  log(tbl$warmmiete))

# logarithmisch sieht sinnvoller aus
cor(tbl$warmmiete,tbl$kaltmiete)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
##                   Fläche betrachten ####
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Da die Korrelation zwischen Warm- und Kaltmiete 99 % beträgt wird im folgenden
# nur noch die Warmmiete betrachtet
fit_lm1<-lm("warmmiete~flaeche",tbl)
flaeche<-tibble(
  flaeche=
    seq(
      min(tbl$flaeche,na.rm = T), 
      max(tbl$flaeche,na.rm = T), 
      length.out = length(tbl$flaeche)
      )
  )
pred<-predict(fit_lm1,flaeche)

fig<-plot_ly(
  tbl,
  x=~flaeche,
  y=~warmmiete,
  type = "scatter",
  showlegend=F,
  name = "warmmiete/Fläche",
  mode="markers") %>%
  layout(
    xaxis=list(title="Fläche"),
    yaxis=list(title="Warmmiete")
  )
fig %>% add_trace(
  x=flaeche$flaeche,
  y=pred,
  mode="lines",
  alpha=1,
  name="lm") 

# log warmmiete
fit_lm1<-lm("log(warmmiete)~flaeche",tbl)
flaeche<-tibble(
  flaeche=seq(
    min(tbl$flaeche,na.rm = T),
    max(tbl$flaeche,na.rm = T), 
    length.out = length(tbl$flaeche)))
pred<-predict(fit_lm1,flaeche)

fig<-plot_ly(
  tbl,
  x=~flaeche,
  y=~log(warmmiete),
  type = "scatter",
  name = "warmmiete/Fläche",
  mode="markers")
fig %>% add_trace(
  x=flaeche$flaeche,
  y=pred,
  mode="lines",
  alpha=1,
  name="lm") 

# log warmmiete & fläche
fit_lm1<-lm("log(warmmiete)~log(flaeche)",tbl)
flaeche<-tibble(
  flaeche=seq(
    min((tbl$flaeche),na.rm = T), 
    max((tbl$flaeche),na.rm = T), 
    length.out = length(tbl$flaeche)))
pred<-predict(fit_lm1,(flaeche))

fig<-plot_ly(
  tbl,
  x=~log(flaeche),
  y=~log(warmmiete),    
  showlegend=F,
  type = "scatter",
  name = "warmmiete/Fläche", 
  text=~id,
  mode="markers") %>%
  layout(
    xaxis=list(title="log(Fläche)"),
    yaxis=list(title="log(Warmmiete)")
  )
fig %>% add_trace(
  x=log(flaeche$flaeche),
  y=pred,mode="lines",
  alpha=1,
  name="lm")

# log warmmiete & sqrt fläche
fit_lm1<-lm("log(warmmiete)~sqrt(flaeche)",tbl)
flaeche<-tibble(
  flaeche=seq(
    min((tbl$flaeche),na.rm = T), 
    max((tbl$flaeche),na.rm = T), 
    length.out = length(tbl$flaeche)))
pred<-predict(fit_lm1,(flaeche))

fig<-plot_ly(
  tbl,
  x=~sqrt(flaeche),
  y=~log(warmmiete),    
  showlegend=F,
  type = "scatter",
  name = "warmmiete/Fläche", 
  text=~id,
  mode="markers") %>%
  layout(
    xaxis=list(title="log(Fläche)"),
    yaxis=list(title="log(Warmmiete)")
  )
fig %>% add_trace(
  x=sqrt(flaeche$flaeche),
  y=pred,mode="lines",
  alpha=1,
  name="lm")

cor(tbl$warmmiete,tbl$flaeche)
cor(log(tbl$warmmiete),tbl$flaeche)
cor(log(tbl$warmmiete),log(tbl$flaeche))

cor(log(tbl$warmmiete),1/(tbl$flaeche))
cor(log(tbl$warmmiete),log(tbl$flaeche+min(tbl$flaeche)))
cor(log(tbl$warmmiete),sqrt(tbl$flaeche))

cor(tbl$kaltmiete,tbl$flaeche)
cor(log(tbl$kaltmiete),tbl$flaeche)
cor(log(tbl$kaltmiete),log(tbl$flaeche))

cor(log(tbl$kaltmiete),1/(tbl$flaeche))
cor(log(tbl$kaltmiete),log(tbl$flaeche+min(tbl$flaeche)))
cor(log(tbl$kaltmiete),sqrt(tbl$flaeche))

# logarithmisch sinnvoller?

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
##                   Zimmer ####
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fit_lm1<-lm("warmmiete~zimmer",tbl)
zimmer<-tibble(
  zimmer=seq(
    min(tbl$zimmer,na.rm = T), 
    max(tbl$zimmer,na.rm = T), 
    length.out = length(tbl$zimmer)))
pred<-predict(fit_lm1,zimmer)
fig<-plot_ly(
  tbl,
  x=~zimmer,
  y=~warmmiete,type = "scatter",
  name = "warmmiete/Zimmer",
  mode="markers")
fig %>% add_trace(
  x=zimmer$zimmer,
  y=pred,mode="lines",
  alpha=1,
  name="lm") 

# log warmmiete
fit_lm1<-lm("log(warmmiete)~zimmer",tbl)
zimmer<-tibble(
  zimmer=seq(
    min(tbl$zimmer,na.rm = T), 
    max(tbl$zimmer,na.rm = T), 
    length.out = length(tbl$zimmer)))
pred<-predict(fit_lm1,zimmer)
fig<-plot_ly(
  tbl,
  x=~zimmer,
  y=~log(warmmiete),
  type = "scatter",
  name = "warmmiete/Zimmer",
  mode="markers")
fig %>% add_trace(
  x=zimmer$zimmer,
  y=pred,
  mode="lines",
  alpha=1,
  name="lm") 


# log warmmiete & Zimmer
fit_lm1<-lm("log(warmmiete)~log(zimmer)",tbl)
zimmer<-tibble(
  zimmer=seq(
    min(tbl$zimmer,na.rm = T), 
    max(tbl$zimmer,na.rm = T), 
    length.out = length(tbl$zimmer)))
pred<-predict(fit_lm1,zimmer)
fig<-plot_ly(
  tbl,
  x=~log(zimmer),
  y=~log(warmmiete),
  type = "scatter",
  name = "warmmiete/Zimmer",
  text=~id,
  mode="markers")
fig %>% add_trace(
  x=log(zimmer$zimmer),
  y=pred,
  mode="lines",
  alpha=1,
  name="lm") 

# log warmmiete & Zimmer
fit_lm1<-lm("log(warmmiete)~sqrt(zimmer)",tbl)
zimmer<-tibble(
  zimmer=seq(
    min(tbl$zimmer,na.rm = T), 
    max(tbl$zimmer,na.rm = T), 
    length.out = length(tbl$zimmer)))
pred<-predict(fit_lm1,zimmer)
fig<-plot_ly(
  tbl,
  x=~sqrt(zimmer),
  y=~log(warmmiete),
  type = "scatter",
  name = "warmmiete/Zimmer",
  text=~id,
  mode="markers")
fig %>% add_trace(
  x=sqrt(zimmer$zimmer),
  y=pred,
  mode="lines",
  alpha=1,
  name="lm") 

table(tbl$zimmer)

cor(tbl$warmmiete,tbl$zimmer)
cor(log(tbl$warmmiete),tbl$zimmer)
cor(log(tbl$warmmiete),log(tbl$zimmer))
cor(log(tbl$warmmiete),sqrt(tbl$zimmer))

# Mit den log Zimmern, sieht die Verteilung gar nicht so schlecht aus
# Allerdings überschätzt die LM die warmmietee je mehr Zimmer
# Die größeren Daten sind bereits als Ausreißer identifiziert
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
##                   Stadt ####
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_ly(
  tbl,
  x=~reorder(stadt,warmmiete,median),
  y = ~warmmiete, 
  type = "box",
  jitter=0,
  transforms=list(
    list(
      type="groupby",
      groups=~stadt
    )
  )
) %>%
  layout(
    title=paste("Boxplot warmmiete/Stadt"),
    yaxis = list(
      title = '',
      categoryorder = "array",
      categoryarray = levels(~stadt)),
    xaxis = list(title = ''),
    showlegend=F
  )

# Preise über 4 Mio / 6 Mio raus?
plot_ly(
  tbl,
  x=~reorder(stadt,log(warmmiete),median),
  y = ~log(warmmiete), 
  type = "box",
  jitter=0,
  text=~id,
  transforms=list(
    list(
      type="groupby",
      groups=~stadt
    )
  )
) %>%
  layout(
    title=paste("Boxplot warmmiete/Stadt"),
    yaxis = list(
      title = '',
      categoryorder = "array",
      categoryarray = levels(~stadt)),
    xaxis = list(title = ''),
    showlegend=F
  )

tbl %>%
  group_by(stadt) %>%
  summarise(anzahl=length(stadt)) %>%
  arrange(anzahl)

# Nur 8 Datensätze für Garmisch Patenkirchen ist sehr schwach

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
##                   Art ####
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_ly(
  tbl,
  x=~reorder(art,warmmiete,median),
  y = ~warmmiete, 
  type = "box",
  jitter=0,
  transforms=list(
    list(
      type="groupby",
      groups=~art
    )
  )
) %>%
  layout(
    title=paste("Boxplot warmmiete/art"),
    yaxis = list(
      title = '',
      categoryorder = "array",
      categoryarray = levels(~art)),
    xaxis = list(title = ''),
    showlegend=F
  )


plot_ly(
  tbl,
  x=~reorder(art,log(warmmiete),median),
  y = ~log(warmmiete), 
  type = "box",
  jitter=0,
  transforms=list(
    list(
      type="groupby",
      groups=~art
    )
  )
) %>%
  layout(
    title=paste("Boxplot warmmiete/Stadt"),
    yaxis = list(
      title = '',
      categoryorder = "array",
      categoryarray = levels(~art)),
    xaxis = list(title = ''),
    showlegend=F
  )

# Ausreißer eher nach oben als nach unten
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
##                   Energieverbrauch ####
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fit_lm1<-lm("warmmiete~energieverbrauch",tbl)
energieverbrauch<-tibble(
  energieverbrauch=
    seq(
      min(tbl$energieverbrauch,na.rm = T), 
      max(tbl$energieverbrauch,na.rm = T), 
      length.out = length(tbl$energieverbrauch)))
pred<-predict(fit_lm1,energieverbrauch)

fig<-plot_ly(
  tbl ,
  x=~energieverbrauch,
  y=~warmmiete,
  type = "scatter",
  name = "warmmiete/energieverbrauch",
  text=~id,
  mode="markers");fig
fig %>% add_trace(
  x=energieverbrauch$energieverbrauch,
  y=pred,
  mode="lines",
  alpha=1,
  name="lm") 

# Merkwürdiger Wert für energieverbrauch>50k

fit_lm1<-lm("warmmiete~energieverbrauch",tbl%>%filter(energieverbrauch<50000))
energieverbrauch<-tibble(
  energieverbrauch=seq(
    min(tbl$energieverbrauch,na.rm = T),
    max(tbl$energieverbrauch[tbl$energieverbrauch<50000],na.rm = T), 
    length.out = length(tbl$energieverbrauch)))
pred<-predict(fit_lm1,energieverbrauch)

fig<-plot_ly(
  tbl %>%
    filter(energieverbrauch<50000),
  x=~energieverbrauch,
  y=~warmmiete,
  type = "scatter",
  name = "warmmiete/energieverbrauch",
  mode="markers");fig
fig %>% add_trace(
  x=energieverbrauch$energieverbrauch,
  y=pred,
  mode="lines",
  alpha=1,
  name="lm") 

# Sehr schlecht

fit_lm1<-lm(
  "log(warmmiete)~energieverbrauch",
  tbl %>%
    filter(energieverbrauch<50000))
energieverbrauch<-tibble(
  energieverbrauch=
    seq(
      min(tbl$energieverbrauch,na.rm = T), 
      max(tbl$energieverbrauch[tbl$energieverbrauch<50000],na.rm = T), 
      length.out = length(tbl$energieverbrauch)))
pred<-predict(fit_lm1,energieverbrauch)

fig<-plot_ly(
  tbl %>%
    filter(energieverbrauch<50000),x=~energieverbrauch,
  y=~log(warmmiete),
  type = "scatter",
  name = "warmmiete/energieverbrauch",
  mode="markers");fig
fig %>% add_trace(
  x=energieverbrauch$energieverbrauch,
  y=pred,
  mode="lines",
  alpha=1,
  name="lm") 

# immer noch schlecht, Log warmmiete & energieverbrauch? 

fit_lm1<-lm(
  "log(warmmiete)~log(energieverbrauch)",
  tbl %>% filter(energieverbrauch<50000))
energieverbrauch<-tibble(
  energieverbrauch=seq(
    min(tbl$energieverbrauch,na.rm = T), 
    max(tbl$energieverbrauch[tbl$energieverbrauch<50000],na.rm = T), 
    length.out = sum(!is.na(tbl$energieverbrauch))))
energieverbrauch<-energieverbrauch[1:11380,]
pred<-predict(fit_lm1,energieverbrauch)

fig<-plot_ly(
  tbl %>%
    filter(energieverbrauch<5000),
  x=~log(energieverbrauch),
  y=~log(warmmiete),
  type = "scatter",
  name = "warmmiete/energieverbrauch",
  mode="markers");fig
fig %>% add_trace(
  x=log(energieverbrauch$energieverbrauch),
  y=pred,
  mode="lines",
  alpha=1,
  name="lm") 

cor(tbl$warmmiete,tbl$energieverbrauch,use = "pairwise" )
cor(
  log(tbl$warmmiete[tbl$energieverbrauch<50000]),
  log(tbl$energieverbrauch[tbl$energieverbrauch<50000]),
  use = "pairwise" )

tbl %>% 
  filter(energieverbrauch>50000) %>% 
  select(id,energieverbrauch,warmmiete,kaltmiete,flaeche,zimmer)
tbl %>%
  filter(energieverbrauch<5) %>% 
  select(id,energieverbrauch,warmmiete,kaltmiete,flaeche,zimmer)

# Ich setze den Energieverbrauch > 10k und <5 auf NA, 
#da der Rest der Daten in Ordnung aussieht
tbl$energieverbrauch[which(tbl$energieverbrauch>10000)]<-NA
tbl$energieverbrauch[which(tbl$energieverbrauch<5)]<-NA


# Umstrukturierung in Kategoriale Variable
par(mar=c(3,6,3,6))
wss <- NA

kmeans_energieverbrauch<-tbl[,c("warmmiete","energieverbrauch")]
kmeans_energieverbrauch<-na.omit(kmeans_energieverbrauch)
kmeans_energieverbrauch$warmmiete<-log(kmeans_energieverbrauch$warmmiete)
kmeans_energieverbrauch$energieverbrauch<-log(
  kmeans_energieverbrauch$energieverbrauch)

for (i in 1:20) wss[i] <- 
  sum(
    kmeans(
      kmeans_energieverbrauch$energieverbrauch,
      iter.max = 100,
      centers = i,
      nstart=20)$within
    )/
  kmeans(
    kmeans_energieverbrauch$energieverbrauch,
    iter.max = 100,
    centers = i,
    nstart=20)$totss
plot(
  1:20, 
  wss, 
  type="b", 
  xlab="Anzahl der Cluster",
  ylab="Summe der quadratischen Abweichungen\n innerhalb der Gruppen",
  bty="n",
  main = "Optimale Anzahl der Cluster")
points(
  5,
  sum(
    kmeans(
      kmeans_energieverbrauch$energieverbrauch,
      5,
      nstart=20)$within)/
         kmeans(
           kmeans_energieverbrauch$energieverbrauch,
           5,
           nstart=20)$totss,
  col="blue",
  pch=19)
set.seed(123)
k<-kmeans(kmeans_energieverbrauch$energieverbrauch,centers = 5,nstart=40)
kmeans_energieverbrauch$k<-k$cluster
summary(kmeans_energieverbrauch$energieverbrauch[k$cluster==1])[c(1,6)] %>% 
  exp()
summary(kmeans_energieverbrauch$energieverbrauch[k$cluster==2])[c(1,6)] %>% 
  exp()
summary(kmeans_energieverbrauch$energieverbrauch[k$cluster==3])[c(1,6)] %>% 
  exp()
summary(kmeans_energieverbrauch$energieverbrauch[k$cluster==4])[c(1,6)] %>% 
  exp()
summary(kmeans_energieverbrauch$energieverbrauch[k$cluster==5])[c(1,6)] %>% 
  exp()

tbl$energieverbrauch_mod<-NA
tbl$energieverbrauch_mod[
  which(tbl$energieverbrauch  <=30)  ]<-"kleiner 30 kWh/(m²*a)"
tbl$energieverbrauch_mod[
  which(
    tbl$energieverbrauch >30 & 
      tbl$energieverbrauch  <=65)  ]<-"30 - 65 kWh/(m²*a)"
tbl$energieverbrauch_mod[
  which(
    tbl$energieverbrauch >65 & 
      tbl$energieverbrauch  <=100)  ]<-"65 - 100 kWh/(m²*a)"
tbl$energieverbrauch_mod[
  which(
    tbl$energieverbrauch >100 & 
      tbl$energieverbrauch  <=150)  ]<-"100 - 150 kWh/(m²*a)"
tbl$energieverbrauch_mod[
  which(tbl$energieverbrauch >150)]<-"größer 5800 kWh/(m²*a)"
tbl$energieverbrauch_mod[is.na(tbl$energieverbrauch)]<-"Keine Angabe"
tbl$energieverbrauch_mod<-as.factor(tbl$energieverbrauch_mod)

tmp<-tbl %>% 
  filter(!is.na(energieverbrauch_mod))
plot_ly(
  tmp,
  x=~reorder(energieverbrauch_mod,log(warmmiete),median),
  y = ~log(warmmiete), 
  type = "box",
  jitter=0,
  transforms=list(
      list(
        type="groupby",
        groups=~(energieverbrauch_mod)
      )
    )
) %>%
  layout(
    title=paste("Boxplot warmmiete/energieverbrauch_mod"),
    xaxis = list(title = ''),
    showlegend=F
  )
plot_ly(
  tbl,
  x=~log(energieverbrauch),
  y=~log(warmmiete),
  type="scatter",
  mode="markers",
  color=~energieverbrauch_mod)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
##                   Dokumente ####
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fit_lm1<-lm("warmmiete~dokumente",tbl)
dokumente<-tibble(
  dokumente=seq(
    min(tbl$dokumente,na.rm = T),
    max(tbl$dokumente,na.rm = T), 
    length.out = length(tbl$dokumente)))
pred<-predict(fit_lm1,dokumente)
fig<-plot_ly(
  tbl,
  x=~dokumente,
  y=~warmmiete,
  type = "scatter",
  name = "warmmiete/Dokumente",
  mode="markers")
fig %>% add_trace(
  x=dokumente$dokumente,
  y=pred,
  mode="lines",
  alpha=1,
  name="lm") 

# log warmmiete
fit_lm1<-lm("log(warmmiete)~dokumente",tbl)
dokumente<-tibble(
  dokumente=seq(
    min(tbl$dokumente,na.rm = T), 
    max(tbl$dokumente,na.rm = T), 
    length.out = length(tbl$dokumente)))
pred<-predict(fit_lm1,dokumente)
fig<-plot_ly(
  tbl,
  x=~dokumente,
  y=~log(warmmiete),
  type = "scatter",
  name = "warmmiete/dokumente",
  mode="markers")
fig %>% add_trace(
  x=dokumente$dokumente,
  y=pred,mode="lines",
  alpha=1,
  name="lm") 


# log warmmiete & dokumente
fit_lm1<-lm("log(warmmiete)~log(dokumente+1)",tbl)
dokumente<-tibble(
  dokumente=seq(
    min(tbl$dokumente,na.rm = T), 
    max(tbl$dokumente,na.rm = T), 
    length.out = length(tbl$dokumente)))
pred<-predict(fit_lm1,dokumente)
fig<-plot_ly(
  tbl,
  x=~log(dokumente),
  y=~log(warmmiete),
  type = "scatter",
  name = "warmmiete/dokumente",
  text=~id,
  mode="markers")
fig %>% add_trace(
  x=log(dokumente$dokumente+1),
  y=pred,
  mode="lines",
  alpha=1,
  name="lm") 

table(tbl$dokumente)

cor(tbl$warmmiete,tbl$dokumente,use = "pairwise" )
cor(
  log(tbl$warmmiete),
  tbl$dokumente,
  use = "pairwise" )
cor(
  log(tbl$warmmiete),
  log(tbl$dokumente+1),
  use = "pairwise" )

# Macht kaum einen Unterschied - der Einfluss sollte eh relativ gering sein
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
##                   Fotos ####
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fit_lm1<-lm("warmmiete~fotos",tbl)
fotos<-tibble(
  fotos=seq(
    min(tbl$fotos,na.rm = T), 
    max(tbl$fotos,na.rm = T), 
    length.out = length(tbl$fotos)))
pred<-predict(
  fit_lm1,
  fotos)
fig<-plot_ly(
  tbl,
  x=~fotos,
  y=~warmmiete,
  type = "scatter",
  name = "warmmiete/fotos",
  mode="markers")
fig %>% add_trace(
  x=fotos$fotos,
  y=pred,
  mode="lines",
  alpha=1,
  name="lm") 

# log warmmiete
fit_lm1<-lm(
  "log(warmmiete)~fotos",
  tbl)
fotos<-tibble(
  fotos=seq(
    min(tbl$fotos,na.rm = T), 
    max(tbl$fotos,na.rm = T), 
    length.out = length(tbl$fotos)))
pred<-predict(fit_lm1,fotos)
fig<-plot_ly(
  tbl,
  x=~fotos,
  y=~log(warmmiete),
  type = "scatter",
  name = "warmmiete/fotos",
  mode="markers")
fig %>% add_trace(
  x=fotos$fotos,
  y=pred,
  mode="lines",
  alpha=1,
  name="lm") 


# log warmmiete & fotos
fit_lm1<-lm("log(warmmiete)~log(fotos+1)",tbl)
fotos<-tibble(
  fotos=seq(
    min(tbl$fotos,na.rm = T)+1, 
    max(tbl$fotos,na.rm = T), 
    length.out = length(tbl$fotos)))
pred<-predict(fit_lm1,fotos)
fig<-plot_ly(
  tbl,
  x=~log(fotos),
  y=~log(warmmiete),
  type = "scatter",
  name = "warmmiete/fotos",
  text=~id,
  mode="markers")
fig %>% add_trace(
  x=log(fotos$fotos+1),
  y=pred,mode="lines",
  alpha=1,
  name="lm") 

table(tbl$fotos)

cor(tbl$warmmiete,tbl$fotos,use = "pairwise" )
cor(
  log(tbl$warmmiete),
  tbl$fotos,
  use = "pairwise" )
cor(
  log(tbl$warmmiete),
  log(tbl$fotos+1),
  use = "pairwise" )

# Fotos müssen nicht logarithmiert werden

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
##                   Zustand ####
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
levels(tbl$zustand)
tmp<-tbl%>% filter(!is.na(zustand))
plot_ly(
  tmp,
  x=~reorder(zustand,warmmiete,median),
  y = ~warmmiete, 
  type = "box",
  jitter=0,
  transforms=list(
    list(
      type="groupby",
      groups=~zustand
    )
  )
) %>%
  layout(
    title=paste("Boxplot warmmiete/zustand"),
    xaxis = list(title = ''),
    showlegend=F
  )

# warmmietee über 4 Mio / 6 Mio raus?
plot_ly(
  tmp,
  x=~reorder(zustand,warmmiete,median),
  y = ~log(warmmiete), 
  type = "box",
  jitter=0,
  text=~id,
  transforms=list(
    list(
      type="groupby",
      groups=~zustand
    )
  )
) %>%
  layout(
    title=paste("Boxplot warmmiete/Stadt"),
    xaxis = list(title = ''),
    showlegend=F
  )

table(tbl$zustand)
sum(is.na(tbl$zustand))
tbl %>% 
  filter( zustand=="Entkernt") %>% 
  select(id,warmmiete,flaeche,zimmer,stadt)

tbl$ausreißer[which(tbl$zustand=="Baufällig")]<-T
tbl$ausreißer[which(tbl$zustand=="Entkernt")]<-T

tbl$ausreißer<-F
tbl$ausreißer[tbl$id=="/angebot/49285555"]<-T
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
##                   Baujahr ####
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fit_lm1<-lm("warmmiete~baujahr",tbl)
baujahr<-tibble(
  baujahr=seq(
    min(tbl$baujahr,na.rm = T), 
    max(tbl$baujahr,na.rm = T), 
    length.out = length(tbl$baujahr)))
pred<-predict(fit_lm1,baujahr)
fig<-plot_ly(
  tbl,
  x=~baujahr,
  y=~warmmiete,type = "scatter",
  name = "warmmiete/baujahr",
  mode="markers")
fig %>% add_trace(
  x=baujahr$baujahr,
  y=pred,
  mode="lines",
  alpha=1,
  name="lm") 

# log warmmiete
fit_lm1<-lm("log(warmmiete)~baujahr",tbl)
baujahr<-tibble(
  baujahr=seq(
    min(tbl$baujahr,na.rm = T), 
    max(tbl$baujahr,na.rm = T), 
    length.out = length(tbl$baujahr)))
pred<-predict(fit_lm1,baujahr)
fig<-plot_ly(
  tbl,
  x=~baujahr,
  y=~log(warmmiete),
  showlegend=F,
  type = "scatter",
  name = "warmmiete/baujahr",
  mode="markers")
fig %>% add_trace(
  x=baujahr$baujahr,
  y=pred,mode="lines",
  alpha=1,
  name="lm") 



#schwierig
tbl %>% 
  filter(baujahr < as.Date("1700-01-01")) %>% 
  select(id)

# evtl umstrukturieren: sehr alt/alt/neuer/ neu ->KMeans
par(mar=c(3,6,3,6))
wss <- NA

kmeans_baujahr<-tbl[,c("warmmiete","baujahr")]
kmeans_baujahr<-na.omit(kmeans_baujahr)
kmeans_baujahr$baujahr<-kmeans_baujahr$baujahr %>% 
  year()
kmeans_baujahr<-kmeans_baujahr %>% 
  filter(baujahr>=1700)

for (i in 1:20) wss[i] <- sum(
  kmeans(kmeans_baujahr$baujahr,iter.max = 100,centers = i,nstart=20)$within)/
  kmeans(kmeans_baujahr$baujahr,iter.max = 100,centers = i,nstart=20)$totss
plot(
  1:20, 
  wss, 
  type="b", 
  xlab="Anzahl der Cluster",
  ylab="Summe der quadratischen Abweichungen\n innerhalb der Gruppen",
  bty="n",
  main = "Optimale Anzahl der Cluster")
points(
  3,
  sum(
    kmeans(kmeans_baujahr$baujahr,3,nstart=20)$within)/
    kmeans(kmeans_baujahr$baujahr,3,nstart=20)$totss,
  col="blue",
  pch=19)
set.seed(123)
k<-kmeans(
  kmeans_baujahr$baujahr,
  iter.max = 100,
  centers = 3,
  nstart=20)
kmeans_baujahr$k<-k$cluster
summary(kmeans_baujahr$baujahr[k$cluster==1])[c(1,6)]
summary(kmeans_baujahr$baujahr[k$cluster==2])[c(1,6)]
summary(kmeans_baujahr$baujahr[k$cluster==3])[c(1,6)]

tbl$baujahr_mod<-NA
tbl$baujahr_mod[
  which(
    tbl$baujahr %>% 
      year() >1734 & 
      tbl$baujahr %>% 
      year() <=1937) ]<-"vor 1937"
tbl$baujahr_mod[
  which(
    tbl$baujahr %>% 
      year() >1937 & 
          tbl$baujahr %>% year() <=1990)  ]<-"1938 - 1990"
tbl$baujahr_mod[
  which(
    tbl$baujahr %>% 
      year() >1990)]<-"nach 1991"
tbl$baujahr_mod[which(is.na(tbl$baujahr_mod))]<-"Keine Angabe"
tbl$baujahr_mod<-as.factor(tbl$baujahr_mod)

tmp<- tbl%>% 
  filter(!is.na(baujahr_mod))
plot_ly(
  tmp,
  x=~reorder(baujahr_mod,log(warmmiete),median),
  y = ~log(warmmiete), 
  type = "box",
  jitter=0,
  transforms=list(
    list(
      type="groupby",
      groups=~(baujahr_mod)
    )
  )
) %>%
  layout(
    title=paste(""),
    xaxis = list(title = ''),
    showlegend=F
  )

plot_ly(
  tbl %>% 
    filter(baujahr_mod!="Keine Angabe"),
  x=~baujahr,
  y=~log(warmmiete),
  type="scatter",
  mode="markers",
  color=~baujahr_mod,
  colors = c("red","blue"))%>%
  layout(
    title="",
    xaxis=list(title="Baujahr"),
    yaxis=list(title="log(warmmiete)")
  )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
##                   Pauschalmiete ####
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
table(tbl$pauschalmiete)
tbl$pauschalmiete<-as.factor(tbl$pauschalmiete)
table(tbl$pauschalmiete)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
##                   Denkmalschutz ####
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
table(tbl$denkmalschutz)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
##                   Energieklasse ####
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

levels(tbl$energieklasse)
tmp<-tbl %>% 
  filter(!is.na(energieklasse))
plot_ly(
  tmp,
  x=~reorder(energieklasse,warmmiete,median),
  y = ~warmmiete, 
  type = "box",
  jitter=0,
  transforms=list(
    list(
      type="groupby",
      groups=~(energieklasse)
    )
  )
) %>%
  layout(
    title=paste("Boxplot Preis/energieklasse"),
    xaxis = list(title = ''),
    showlegend=F
  )

plot_ly(
  tmp,
  x=~reorder(energieklasse,log(warmmiete),mean),
  y = ~log(warmmiete), 
  type = "box",
  jitter=0,
  text=~id,
  transforms=list(
    list(
      type="groupby",
      groups=~(energieklasse)
    )
  )
) %>%
  layout(
    title=paste("Boxplot warmmiete/energieklasse"),
    xaxis = list(title = ''),
    showlegend=F
  )

## Evtl könnte man drei Kategorien bilden: A-B ; C -E ; F -H 
# Allerdings sieht G etwas merkwürdig in der Mitte aus

table(tbl$energieklasse)
# A-B ~ 1200 Daten
# C-D ~ 1100 Daten
# E-H ~ 600 Daten

tbl$energieklasse_mod<-NA
tbl$energieklasse_mod[
  which(
    tbl$energieklasse=="Klasse A" | 
      tbl$energieklasse=="Klasse A+" | 
      tbl$energieklasse=="Klasse B")]<-"A-B"
tbl$energieklasse_mod[
  which(
    tbl$energieklasse=="Klasse C" | 
      tbl$energieklasse=="Klasse D")]<-"C-D"
tbl$energieklasse_mod[
  which(
    tbl$energieklasse=="Klasse E" | 
      tbl$energieklasse=="Klasse F" | 
      tbl$energieklasse=="Klasse G" |
      tbl$energieklasse=="Klasse H")]<-"E-H"
tbl$energieklasse_mod[which(tbl$energieklasse=="keine Angabe")]<-"Keine Angabe"
tbl$energieklasse_mod<-as.factor(tbl$energieklasse_mod)
tmp<-tbl%>% 
  filter(!is.na(energieklasse_mod))
plot_ly(
  tmp %>% 
    filter(ausreißer==F),
  x=~reorder(energieklasse_mod,log(warmmiete),mean),
  y = ~log(warmmiete), 
  type = "box",
  jitter=0,
  text=~id,
  transforms=list(
    list(
      type="groupby",
      groups=~(energieklasse_mod)
    )
  )
) %>%
  layout(
    title=paste("Boxplot Preis/energieklasse_mod"),
    xaxis = list(title = ''),
    showlegend=F
  )
levels(tbl$energieklasse_mod)

# Im Optimalfall sinkt der Medianpreis je schlechter die Klasse :/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
##                   Energieverbrauchsausweis ####
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

levels(tbl$energieverbrauchsausweis)
tmp<-tbl %>% 
  filter(!is.na(energieverbrauchsausweis))
plot_ly(
  tmp,
  x=~reorder(energieverbrauchsausweis,warmmiete,median),
  y = ~warmmiete, 
  type = "box",
  jitter=0,
  transforms=list(
    list(
      type="groupby",
      groups=~(energieverbrauchsausweis)
    )
  )
) %>%
  layout(
    title=paste("Boxplot warmmiete/energieverbrauchsausweis"),
    xaxis = list(title = ''),
    showlegend=F
  )

plot_ly(
  tmp,
  x=~reorder(energieverbrauchsausweis,log(warmmiete),mean),
  y = ~log(warmmiete), 
  type = "box",
  jitter=0,
  text=~id,
  transforms=list(
    list(
      type="groupby",
      groups=~(energieverbrauchsausweis)
    )
  )
) %>%
  layout(
    title=paste("Boxplot warmmiete/energieverbrauchsausweis"),
    xaxis = list(title = ''),
    showlegend=F
  )


table(tbl$energieverbrauchsausweis)

fig<-plot_ly(
  tbl,
  x=~log(energieverbrauch),
  y=~log(warmmiete),
  type = "scatter",
  name = ~energieverbrauchsausweis,
  text=~id,
  color=~energieverbrauchsausweis,
  colors=c("blue", "red"),
  mode="markers" );fig


fig<-plot_ly(
  tbl,
  x=~log(energieverbrauch),
  y=~log(warmmiete),
  type = "scatter",
  name = ~energieverbrauchsausweis,
  text=~id,
  mode="markers",
  symbol = ~energieverbrauchsausweis, 
  symbols = c("107",'circle','diamond','triangle-down-open','cross-dot'));fig


# Wenn 'nicht nötig', dann kein Energieverbrauch Angabe
levels(tbl$energieverbrauchsausweis)
tbl %>% 
  filter(energieverbrauchsausweis=="nicht nötig") %>%
  select(energieverbrauch) %>%
  filter(!is.na(energieverbrauch))
table(tbl$energieverbrauchsausweis)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
##                   Energiestandard ####
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tbl$energiestandard<-as.factor(tbl$energiestandard)
levels(tbl$energiestandard)
tmp<-tbl %>% 
  filter(!is.na(energiestandard))
plot_ly(
  tmp,
  x=~reorder(energiestandard,warmmiete,median),
  y = ~warmmiete, 
  type = "box",
  jitter=0,
  transforms=list(
    list(
      type="groupby",
      groups=~(energiestandard)
    )
  )
) %>%
  layout(
    title=paste("Boxplot Preis/energiestandard"),
    xaxis = list(title = ''),
    showlegend=F
  )

plot_ly(
  tmp,
  x=~reorder(energiestandard,log(warmmiete),mean),
  y = ~log(warmmiete), 
  type = "box",
  jitter=0,
  text=~id,
  transforms=list(
    list(
      type="groupby",
      groups=~(energiestandard)
    )
  )
) %>%
  layout(
    title=paste("Boxplot Preis/energiestandard"),
    xaxis = list(title = ''),
    showlegend=F
  )
table(tbl$energiestandard)
table(tbl$energiestandard) %>% 
  sum() /nrow(tbl) * 100 # nur 4 % der Daten vorhanden

tbl$energiestandard<-NULL
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
##                   Heizungsart ####
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

levels(tbl$heizungsart)
tmp<-tbl%>% 
  filter(!is.na(heizungsart))
plot_ly(
  tmp,
  x=~reorder(heizungsart,warmmiete,median),
  y = ~warmmiete, 
  type = "box",
  jitter=0,
  transforms=list(
    list(
      type="groupby",
      groups=~(heizungsart)
    )
  )
) %>%
  layout(
    title=paste("Boxplot warmmiete/heizungsart"),
    xaxis = list(title = ''),
    showlegend=F
  )

plot_ly(
  tmp,
  x=~reorder(heizungsart,log(warmmiete),mean),
  y = ~log(warmmiete), 
  type = "box",
  jitter=0,
  text=~id,
  transforms=list(
    list(
      type="groupby",
      groups=~(heizungsart)
    )
  )
) %>%
  layout(
    title=paste("Boxplot warmmiete/heizungsart"),
    xaxis = list(title = ''),
    showlegend=F
  )
table(tbl$heizungsart)


fig<-plot_ly(
  tbl,
  x=~log(energieverbrauch),
  y=~log(warmmiete),
  type = "scatter",
  name = ~heizungsart,text=~id,mode="markers",
  symbol = ~heizungsart, 
  symbols = c("107",'circle','diamond','triangle-down-open','cross-dot'));fig

#Die Zentralheizungen liegen weit außerhalb
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
##                   Befeuerungsart ####
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

levels(tbl$befeuerungsart)
tmp<-tbl %>% 
  filter(!is.na(befeuerungsart))
tmp<-tmp[
  tmp$befeuerungsart %in% 
    names(
      sort(
        table(tmp$befeuerungsart),decreasing = T)[1:10]),]
plot_ly(
  tmp,
  x=~reorder(befeuerungsart,warmmiete,median),
  y = ~warmmiete, 
  type = "box",
  jitter=0,
  transforms=list(
    list(
      type="groupby",
      groups=~(befeuerungsart)
    )
  )
) %>%
  layout(
    title=paste("Boxplot warmmiete/befeuerungsart"),
    xaxis = list(title = ''),
    showlegend=F
  )

plot_ly(
  tmp,
  x=~reorder(befeuerungsart,log(warmmiete),mean),
  y = ~log(warmmiete),
  type = "box",
  jitter=0,
  text=~id,
  transforms=list(
    list(
      type="groupby",
      groups=~(befeuerungsart)
    )
  )
) %>%
  layout(
    title=paste("Boxplot warmmiete/befeuerungsart"),
    xaxis = list(title = ''),
    showlegend=F
  )
sort(table(tbl$befeuerungsart),decreasing = T)
sort(table(tbl$befeuerungsart),decreasing = T)[c(1:6,8:10,15)] 
# Das entspricht der gleichen Auswahl wie bei den Immobilien Kaufpreisen

# Aufteilen in  Gas / Fernwärme / Öl / Luft-Wasserwärme / Gas /
#fernwärme Strom /Pellets / 
# Luft /wasserwärme Strom 
namen<-names(
  sort(table(tbl$befeuerungsart),decreasing = T)[c(1:6,8:10,15)])
`%ni%` <- Negate(`%in%`)
tbl$befeuerungsart<-as.character(tbl$befeuerungsart)
tbl$befeuerungsart[
  which(tbl$befeuerungsart %ni% namen & !is.na(tbl$befeuerungsart))]<-"Sonstige"
tbl$befeuerungsart[is.na(tbl$befeuerungsart)]<-"Keine Angabe"
tbl$befeuerungsart <-as.factor(tbl$befeuerungsart)
sort(table(tbl$befeuerungsart))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
#                   Mahalonobis Distanz ####
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tbl_mah<- tbl %>% 
  #  filter(ausreißer==F)%>%
  mutate(
    warmmiete = log(warmmiete),
    kaltmiete = log(kaltmiete),
    flaeche= log(flaeche),
    zimmer=log(zimmer))
tbl_mah <- tbl_mah %>% 
  select(warmmiete,kaltmiete,flaeche,zimmer)
tbl_mah$maha<-mahalanobis(tbl_mah ,colMeans(tbl_mah), cov(tbl_mah))
tbl_mah$pval<-pchisq(tbl_mah$maha,df=3,lower.tail = F)
tbl_mah$a<-(pchisq(tbl_mah$maha,df=3,lower.tail = F)<0.0001)
tbl$mah<-tbl_mah$a
sum(tbl$mah)
sum(tbl$ausreißer)
sum(tbl$ausreißer | tbl$mah) # insgesamt gibt es damit 52 Ausreißer
sum(tbl$ausreißer & tbl$mah) # 10 der 46 Ausreißer wurden bereits gefunden

plot_ly(
  tbl_mah,
  x=~kaltmiete,
  y=~warmmiete,
  type = "scatter",
  mode="markers",
  color=~a,
  colors=c("blue","red"))

plot_ly(
  tbl_mah,
  x=~flaeche,
  y=~warmmiete,
  type = "scatter",
  mode="markers",
  color=~a,colors=c("blue","red"))

plot_ly(
  tbl_mah,
  x=~flaeche,
  y=~kaltmiete,
  type = "scatter",
  mode="markers",
  color=~a,
  colors=c("blue","red"))

plot_ly(
  tbl_mah,
  x=~zimmer,
  y=~warmmiete,
  type = "scatter",
  mode="markers",
  color=~a,colors=c("blue","red"))

plot_ly(
  tbl_mah,
  x=~zimmer,
  y=~flaeche,
  type = "scatter",
  mode="markers",
  color=~a,colors=c("blue","red"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
#                   Abschließender Korrelationsvergleich ####
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dev.off()
par(mfrow=c(1,3))
Num.cols <- sapply(
  tbl2 %>% 
    select(warmmiete,kaltmiete,flaeche,zimmer,dokumente,fotos,energieverbrauch), 
  is.numeric)
Cor.data<-tbl2 %>%
  filter(warmmiete<20000) %>%
  select(
    warmmiete,kaltmiete,flaeche,zimmer,dokumente,fotos,energieverbrauch) %>%
  cor(use = "pairwise")
colnames(Cor.data)<-toupper(colnames(Cor.data))
rownames(Cor.data)<-toupper(rownames(Cor.data))
corrplot.mixed(
  Cor.data, 
  tl.col="black", 
  tl.pos = "lt",
  lower.col = "blue")
corrplot(
  Cor.data,
  method = "number",
  type="upper")

# mit log daten 
Num.cols <- sapply(tbl,is.numeric)
Cor.data<-tbl%>%
 # filter(ausreißer==F & mah==F) %>%
  mutate(warmmiete = log(warmmiete),
         kaltmiete= log(kaltmiete),
         flaeche= sqrt(flaeche),
         zimmer=sqrt(zimmer),
         energieverbrauch=log(energieverbrauch)
         ) %>%
  .[,Num.cols] %>%
  cor(use = "pairwise")
colnames(Cor.data)<-toupper(colnames(Cor.data))
rownames(Cor.data)<-toupper(rownames(Cor.data))
corrplot.mixed(
  Cor.data, 
  tl.col="black", 
  tl.pos = "lt",
  lower.col = "blue")
mtext("            log Daten ohne Ausreißer",side=3,line = -20)

corrplot(Cor.data,method = "number",type="upper")


# mit log daten ohne Ausreißer
Num.cols <- sapply(tbl,is.numeric)
Cor.data<-tbl%>%
  filter(ausreißer==F & mah==F) %>%
  mutate(warmmiete = log(warmmiete),
         kaltmiete= log(kaltmiete),
         flaeche= sqrt(flaeche),
         zimmer=sqrt(zimmer),
         energieverbrauch=log(energieverbrauch)
  ) %>%
  .[,Num.cols] %>%
  cor(use = "pairwise")
colnames(Cor.data)<-toupper(colnames(Cor.data))
rownames(Cor.data)<-toupper(rownames(Cor.data))
corrplot.mixed(
  Cor.data, 
  tl.col="black", 
  tl.pos = "lt",
  lower.col = "blue")
mtext("            log Daten ohne Ausreißer",side=3,line = -20)
corrplot(Cor.data,method = "number",type="upper")

# Entscheidung: Transformieren: ja - Mahalonobis Ausreißer raus: Nein
sum(tbl$ausreißer)
tbl_f<-tbl %>%
  filter(ausreißer==F) %>%
  mutate(baujahr = baujahr_mod,
         energieverbrauch = energieverbrauch_mod,
         energieklasse=energieklasse_mod,
         warmmiete = log(warmmiete),
         kaltmiete=log(kaltmiete),
         flaeche=sqrt(flaeche),
         zimmer=sqrt(zimmer),
         ) %>%
  select(-mah,-energieverbrauch_mod,-baujahr_mod,-ausreißer,-ort)

getwd()
tbl<-tbl_f %>% select(-mah)
write.csv2(tbl_f,file = "tbl_f.csv")

setwd("C:/Users/steph/Documents/Studium 2021/3. Semester/Seminar/neuer Start/R")
save(tbl_f,file="tbl.rda")
