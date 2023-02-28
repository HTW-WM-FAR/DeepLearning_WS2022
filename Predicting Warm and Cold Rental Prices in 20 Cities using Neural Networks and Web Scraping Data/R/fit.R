library(fastDummies)
library(forecast)
library(neuralnet)
library(dplyr)
library(tibble)
library(lubridate)
getwd()

setwd("C:/Users/steph/Documents/Studium 2021/3. Semester/statistical learning/neuer Start/R")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
#                   Vorbereitungen zur Modellierung ####
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


load("tbl.rda")
tbl_f<-tbl_f %>% select(-energieklasse_mod)
tbl<-tbl_f %>% select(-mah)

str(tbl)
sapply(tbl, is.numeric) %>% sum
sapply(tbl, is.factor) %>% sum 


# DIe Tabelle wird mit dem Skript gefüllt
acc_table<-tibble(
  "ME"=1,
  "RMSE"=1,
  "MAE"=1,
  "MPE"=1,
  "MAPE"=1,
  "Name"="as",
  "Zeit"=1,
  "Warmmiete"=T,
  "train_test"="a",
  "seed"=NA)
acc_table<-acc_table[-1,]
normalisieren<-function(x)  (x-min(x,na.rm = T))/
  (max(x,na.rm = T)-min(x,na.rm = T))
tblNum<-tbl %>%
  dummy_cols(
    select_columns = c(
      "stadt","art","energieverbrauch","zustand",
      "baujahr","energieklasse",
      "energieverbrauchsausweis","heizungsart",
      "befeuerungsart","denkmalschutz","pauschalmiete"
    ),
    remove_first_dummy = F) %>%
  select(
    -stadt,-art,-energieverbrauch,-zustand,-baujahr,-energieklasse,
         -energieverbrauchsausweis,-heizungsart,-befeuerungsart,
    -denkmalschutz,-pauschalmiete)

dim(tblNum)
colnames(tblNum)<-colnames(tblNum) %>%
  gsub(pattern = " ",replacement = "") %>%
  gsub(pattern = "\\,",replacement = "\\.") %>%
  gsub(pattern = "/",replacement = "\\.") %>%
  gsub(pattern = "-",replacement = "") %>%
  gsub(pattern = ">",replacement = "größer") %>%
  gsub(pattern = "²",replacement = "2") %>%
  gsub(pattern = "\\*",replacement = "mal.") %>%
  gsub(pattern = "\\*",replacement = "mal.") %>%
  gsub(pattern = "\\(",replacement = ".") %>%
  gsub(pattern = "\\)",replacement = ".") %>%
  gsub(pattern = "\\+",replacement = "plus") 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
#                   Aufteilen Datensatz train/test ####
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

set.seed(123)
`%ni%` <- Negate(`%in%`)
#idx<-sample(c(T,F),nrow(tblNum),prob = c(.8,.2),T)
idx<-sample(nrow(tblNum),nrow(tblNum)*.8,F)
tbltrain<-tblNum[idx,] 
tbltest<-tblNum[which((1:nrow(tblNum)) %ni% idx),]

#lmtrain<-tbl_f[idx==T,] 
#lmtest<-tbl_f[idx,]
lmtrain<-tbl_f[idx,] 
lmtest<-tbl_f[which((1:nrow(tblNum)) %ni% idx),]

################# Testdaten skalieren mit den Daten der Trainingsdaten
lmtrain_safe<-tbltrain # safe die original Werte 
tbltest<-tbltest %>%
  mutate(
    warmmiete=
      (warmmiete-min(tbltrain$warmmiete))/
      (max(tbltrain$warmmiete)-min(tbltrain$warmmiete)),
    kaltmiete=
      (kaltmiete-min(tbltrain$kaltmiete))/
      (max(tbltrain$kaltmiete)-min(tbltrain$kaltmiete)),
    flaeche=
      (flaeche-min(tbltrain$flaeche))/
      (max(tbltrain$flaeche)-min(tbltrain$flaeche)),
    zimmer=
      (zimmer-min(tbltrain$zimmer))/
      (max(tbltrain$zimmer)-min(tbltrain$zimmer)),
    fotos=
      (fotos-min(tbltrain$fotos))/
      (max(tbltrain$fotos)-min(tbltrain$fotos)),
    dokumente=
      (dokumente-min(tbltrain$dokumente))/
      (max(tbltrain$dokumente)-min(tbltrain$dokumente))
  )


tbltrain<-tbltrain %>% 
  mutate(
    warmmiete=normalisieren(warmmiete),
    kaltmiete=normalisieren(kaltmiete),
    flaeche=normalisieren(flaeche),
    zimmer=normalisieren(zimmer),
    fotos=normalisieren(fotos),
    dokumente=normalisieren(dokumente)
  )

################# Testdaten skalieren mit den Daten der Trainingsdaten
lmtest_safe<-lmtest # safe die original Werte 

lmtest<-lmtest %>%
  mutate(
    warmmiete=
      (warmmiete-min(lmtrain$warmmiete))/
      (max(lmtrain$warmmiete)-min(lmtrain$warmmiete)),
    kaltmiete=
      (kaltmiete-min(lmtrain$kaltmiete))/
      (max(lmtrain$kaltmiete)-min(lmtrain$kaltmiete)),
    flaeche=
      (flaeche-min(lmtrain$flaeche))/
      (max(lmtrain$flaeche)-min(lmtrain$flaeche)),
    zimmer=
      (zimmer-min(lmtrain$zimmer))/
      (max(lmtrain$zimmer)-min(lmtrain$zimmer)),
    fotos=
      (fotos-min(lmtrain$fotos))/
      (max(lmtrain$fotos)-min(lmtrain$fotos)),
    dokumente=
      (dokumente-min(lmtrain$dokumente))/
      (max(lmtrain$dokumente)-min(lmtrain$dokumente))
  )

lmtrain<-lmtrain %>% 
  mutate(
    warmmiete=normalisieren(warmmiete),
    kaltmiete=normalisieren(kaltmiete),
    flaeche=normalisieren(flaeche),
    zimmer=normalisieren(zimmer),
    fotos=normalisieren(fotos),
    dokumente=normalisieren(dokumente)
  )
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
#                  LM MOdell ####
#
# Info: 2 bzw. 4 Durchgänge.
# 1. Durchgang: mit Pauschalmiete, Befeuerungsart, Dokumente 
# 1.1 das gleiche nochmal für die Kaltmiete
# 2. Durchgang ohne Pauschalmiete, Befeuerungsart, Dokumente 
# 2.1 das gleiche nochmal mit der Kaltmiete
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(forecast)
## Namen & Formel lm ####
names_list<-paste(
  colnames(lmtrain %>% 
             select(-warmmiete,-kaltmiete,-id)),
  collapse = "+")
f_lm<-formula(paste("warmmiete~",names_list,collapse = ""))

## fit lm ####

start<-Sys.time()
fitlm<-lm(formula = f_lm,data = lmtrain)
ende<-Sys.time()
summary(fitlm)
anova(fitlm)

## ACC Train Daten ####
acc_table_lm<-accuracy(
  exp(predict(fitlm)*
        (max(lmtrain_safe$warmmiete)-
           min(lmtrain_safe$warmmiete))+
        min(lmtrain_safe$warmmiete)),
  exp(lmtrain_safe$warmmiete)) %>% 
  as.tibble()
acc_table_lm<-acc_table_lm %>%
  add_column(
    "Name"="Lm",
    "Zeit"=as.numeric(ende-start),
    "Warmmiete"=T,
    "train_test"="train",
    "seed"=NA
  )
acc_table<-acc_table %>% 
  add_row(acc_table_lm)

## ACC Test Daten ####

acc_table_lm<-accuracy(
  exp(
    predict(fitlm,newdata = lmtest)*
      (max(lmtrain_safe$warmmiete)-
         min(lmtrain_safe$warmmiete))+
      min(lmtrain_safe$warmmiete)),
  exp(lmtest_safe$warmmiete)) %>% 
  as.tibble()
acc_table_lm<-acc_table_lm %>%
  add_column(
    "Name"="Lm",
    "Zeit"=as.numeric(ende-start),
    "Warmmiete"=T,
    "train_test"="test",
    "seed"=NA
  )
acc_table<-acc_table %>% 
  add_row(acc_table_lm)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
#                  XG Modell ####
#
# Müsste theoretisch auch 2x durchgeführt werden. Einmal für die Warmmiete,  
# einmal für die Kaltmiete
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(xgboost)
library(Ckmeans.1d.dp)

## XG Train/Test ####
xgtrain<-xgb.DMatrix(as.matrix(tbltrain %>% select(-id,-warmmiete,-kaltmiete)),label=tbltrain$warmmiete)
xgtest<-xgb.DMatrix(as.matrix(tbltest %>% select(-id,-warmmiete,-kaltmiete)))

## Parameterwahl ####
param<-list(
  objective = "reg:linear",
  booster = "gbtree",
  eta=0.05, # soll das overfitting verhindern default 0.3
  gamma=0, # 0 - oo - je größer desto konservativer 
  max_depth=3, #default=6  - grßere Bäume -> Overfitting + Speicerprobleme
  min_child_weight=1, #default=1
  subsample=1
)
dev.off()
## CV für XG ####
start<-Sys.time()
set.seed(123)
xgbcv <- xgb.cv( 
  params = param, 
  data = xgtrain, 
  nrounds = 2000, 
  nfold = 5, # 5 gleichgroße Stichproben
  showsd = T, 
  stratified = T, 
  print_every_n = 40,
  early_stopping_rounds = 10, # wenn keine weitere Verbesserung -> Abbruch
  maximize = F, 
  base_score = .5,
  prediction = T)
xgb_mod <- xgb.train(
  data = xgtrain, 
  params=param, 
  nrounds = xgbcv$best_iteration,
  base_score =.5)
ende<-Sys.time()


acc_table_xg<-accuracy(
  exp(
    predict(xgb_mod,xgtrain)*
      (max(lmtrain_safe$warmmiete)-
         min(lmtrain_safe$warmmiete))+
      min(lmtrain_safe$warmmiete)),
  exp(lmtrain_safe$warmmiete)) %>% 
  as.tibble()
acc_table_xg<-acc_table_xg %>%
  add_column(
    "Name"=paste("xg",xgbcv$best_iteration),
    "Zeit"=as.numeric(ende-start),
    "Warmmiete"=F,
    "train_test"="train",
    "seed"=123
  )
acc_table<-acc_table %>% 
  add_row(acc_table_xg)

## ACC Test Daten ####

acc_table_xg<-accuracy(
  exp(predict(xgb_mod,xgtest)*
        (max(lmtrain_safe$warmmiete)-
           min(lmtrain_safe$warmmiete))+
        min(lmtrain_safe$warmmiete)),
  exp(lmtest_safe$warmmiete)) %>% 
  as.tibble()
acc_table_xg<-acc_table_xg %>%
  add_column(
    "Name"=paste("xg",xgbcv$best_iteration),
    "Zeit"=as.numeric(ende-start),
    "Warmmiete"=F,
    "train_test"="train",
    "seed"=123
  )
acc_table<-acc_table %>% 
  add_row(acc_table_xg)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
#                  NN Modell ####
#
# Um die Acc-Daten für die Kaltmiete zu bekommen muss 
# predict(fit_nn,tbltrain)[,1] auf predict(fit_nn,tbltrain)[,2]
# geändert werden
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(neuralnet)

## Namen Und Formel nn ####
names_list<-paste(
  colnames(
    tbltrain %>%
      select(-warmmiete,-kaltmiete,-id)),
  collapse = "+")
f_nn<-formula(
  paste("warmmiete+kaltmiete~",names_list,collapse = ""))

fit_nn<-list()
i<-1
while (length(fit_nn)!=14) {
  set.seed(i)
  start<-Sys.time()
  fit_nn<-neuralnet(
    f_nn,
    data=tbltrain %>% 
      select(-id),
    hidden=c(5,3),
    algorithm = "rprop+",
    linear.output = T,
    act.fct = "logistic")
  ende<-Sys.time()
  i<-i+1
  print(ende-start)
}
#fit_nn_200_1<-fit_nn
#fit_nn_200_2<-fit_nn
#fit_nn_420_3<-fit_nn
#fit_nn_321_3<-fit_nn
#fit_nn_632_1<-fit_nn
#fit_nn_631_1<-fit_nn

## ACC Train Daten ####
acc_table_nn<-accuracy(
  exp(predict(fit_nn,tbltrain)[,1]*
        (max(lmtrain_safe$warmmiete)-
           min(lmtrain_safe$warmmiete))+
        min(lmtrain_safe$warmmiete)),
  exp(lmtrain_safe$warmmiete)) %>% 
  as.tibble()
acc_table_nn<-acc_table_nn %>%
  add_column(
    "Name"="nn c(1)",
    "Zeit"=as.numeric(ende-start),
    "Warmmiete"=T,
    "train_test"="train",
    "seed"=2
  )
acc_table<-acc_table %>% 
  add_row(acc_table_nn)

## ACC Test Daten ####

acc_table_nn<-accuracy(
  exp(
    predict(fit_nn_420_3,tbltest)[,1]*
      (max(lmtrain_safe$warmmiete)-
         min(lmtrain_safe$warmmiete))+
      min(lmtrain_safe$warmmiete)),
  exp(lmtest_safe$warmmiete)) %>% 
  as.tibble()
acc_table_nn<-acc_table_nn %>%
  add_column(
    "Name"="nn c(1)",
    "Zeit"=as.numeric(ende-start),
    "Warmmiete"=T,
    "train_test"="test",
    "seed"=2
  )
acc_table<-acc_table %>% 
  add_row(acc_table_nn)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
#                  NN Aggregation ####
#
# Bootstrap Aggregation - bisher unentschlossen, welches das beste Modell ist
#
# Erstelle 210 NN und entscheide dann:
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


fit_nn<-list()
start<-NA
ende<-NA
laufzeit<-NA
for(i in 1:210){
  start[i]<-Sys.time()
  set.seed(i)
  idx<-sample(nrow(tbltrain),nrow(tbltrain),T)
  bootstrain<-tbltrain[idx,]
  set.seed(1)
  
  if(i<=50){
    fit_nn[[i]]<-neuralnet(
      f_nn,
      data=bootstrain %>% 
        select(-id),
      hidden=c(1),
      algorithm = "rprop+",
      linear.output = T,
      act.fct = "logistic")
    if(length(fit_nn[[i]])!=14) fit_nn[[i]]<-NULL
  }
  if(i>50 & i <=100){
    fit_nn[[i]]<-neuralnet(
      f_nn,
      data=bootstrain %>% 
        select(-id),
      hidden=c(2),
      algorithm = "rprop+",
      linear.output = T,
      act.fct = "logistic")
    if(length(fit_nn[[i]])!=14) fit_nn[[i]]<-NULL
  }
  if(i>100 & i <=150){
    fit_nn[[i]]<-neuralnet(
      f_nn,
      data=bootstrain %>% 
        select(-id),
      hidden=c(3),
      algorithm = "rprop+",
      linear.output = T,
      act.fct = "logistic")
    if(length(fit_nn[[i]])!=14) fit_nn[[i]]<-NULL
  }
  if(i>150 & i <=170){
    fit_nn[[i]]<-neuralnet(
      f_nn,
      data=bootstrain %>% 
        select(-id),
      hidden=c(4,1),
      algorithm = "rprop+",
      linear.output = T,
      act.fct = "logistic")
    if(length(fit_nn[[i]])!=14) fit_nn[[i]]<-NULL
  }
  if(i>170 & i <=190){
    fit_nn[[i]]<-neuralnet(
      f_nn,
      data=bootstrain %>% 
        select(-id),
      hidden=c(4,2),
      algorithm = "rprop+",
      linear.output = T,
      act.fct = "logistic")
    if(length(fit_nn[[i]])!=14) fit_nn[[i]]<-NULL
  }
  if(i>190 & i <=210){
    fit_nn[[i]]<-neuralnet(
      f_nn,
      data=bootstrain %>% 
        select(-id),
      hidden=c(4,3),
      algorithm = "rprop+",
      linear.output = T,
      act.fct = "logistic")
    if(length(fit_nn[[i]])!=14) fit_nn[[i]]<-NULL
  }
  ende[i]<-Sys.time()
  laufzeit[i]<-ende[i]-start[i]
  print(paste("Durchgang",i,"Laufzeit:",laufzeit[i]))
}

# Alle 210 Modelle sind hier gespeichert:
#setwd("C:/Users/steph/Documents/Studium 2021/3. Semester/statistical learning/neuer Start/R")
#load(file = "fit_nn_1_50x.rda")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
#                  Acc_table 2 MOdell ####
#
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

acc_table2<-tibble(
  "ME"=1,
  "RMSE"=1,
  "MAE"=1,
  "MPE"=1,
  "MAPE"=1,
  "Name"="as",
  "Mean Zeit"=1,
  "Warmmiete"=T,
  "train_test"="a",
  "Durchläufe"=NA,
  "Konvergiert"=NA)
acc_table2<-acc_table2[-1,]
m<-matrix(NA,nrow(tbltrain),50)
j<-0
for(i in 1:50){
  j<-j+1
  if(is.null(fit_nn[[i]])) next
  m[,j]<-exp(
    predict(fit_nn[[i]],tbltrain)[,1]*
      (max(lmtrain_safe$warmmiete)-
         min(lmtrain_safe$warmmiete))+
      min(lmtrain_safe$warmmiete))
}
pred1<-rowMeans(m,na.rm = T)
a<-accuracy(pred1,exp(lmtrain_safe$kaltmiete)) %>% 
  as.tibble()
a<-a %>% add_column(
  "Name"="c(3)",
  "Mean Zeit"= round(mean(laufzeit[101:150])/60,1),
  "Warmmiete"=T,
  "train_test"="Train",
  "Durchläufe"=50,
  "Konvergiert"=50-sum(is.na(m[1,]))
)
# acc_table2<-acc_table2 %>% add_row(a);acc_table2
# write.csv2(acc_table2,file = "acc_table2.txt",row.names = F)
# write.csv2(acc_table,file = "acc_table.txt",row.names = F)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
#                 fast Finale Vorhersage ####
#
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Trainingsdaten Warmmiete:
m<-matrix(NA,nrow(tbltrain),60)
j<-0
for(i in 151:210){
  j<-j+1
  if(is.null(fit_nn[[i]])) next
  m[,j]<-exp(
    predict(fit_nn[[i]],tbltrain)[,1]*
      (max(lmtrain_safe$warmmiete)-
         min(lmtrain_safe$warmmiete))+
      min(lmtrain_safe$warmmiete))
}
pred1<-rowMeans(m,na.rm = T)
accuracy(pred1,exp(lmtrain_safe$warmmiete))

# Testdaten Warmmiete
m<-matrix(NA,nrow(tbltest),60)
j<-0
for(i in 151:210){
  j<-j+1
  if(is.null(fit_nn[[i]])) next
  m[,j]<-exp(predict(fit_nn[[i]],tbltest)[,1]*
               (max(lmtrain_safe$warmmiete)-
                  min(lmtrain_safe$warmmiete))+
               min(lmtrain_safe$warmmiete))
}
pred2<-rowMeans(m,na.rm = T)
accuracy(pred2,exp(lmtest_safe$warmmiete))

# Trainingsdaten Kaltmiete
m<-matrix(NA,nrow(tbltrain),60)
j<-0
for(i in 151:210){
  j<-j+1
  if(is.null(fit_nn[[i]])) next
  m[,j]<-exp(predict(fit_nn[[i]],tbltrain)[,2]*
               (max(lmtrain_safe$kaltmiete)-
                  min(lmtrain_safe$kaltmiete))+
               min(lmtrain_safe$kaltmiete))
}
pred3<-rowMeans(m,na.rm = T)
accuracy(pred3,exp(lmtrain_safe$kaltmiete))

# Testdaten Kaltmiete
m<-matrix(NA,nrow(tbltest),60)
j<-0
for(i in 151:210){
  j<-j+1
  if(is.null(fit_nn[[i]])) next
  m[,j]<-exp(predict(fit_nn[[i]],tbltest)[,2]*
               (max(lmtrain_safe$kaltmiete)-
                  min(lmtrain_safe$kaltmiete))+
               min(lmtrain_safe$kaltmiete))
}
pred4<-rowMeans(m,na.rm = T)
accuracy(pred4,exp(lmtest_safe$kaltmiete))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
#                  Finale Vorhersageintervall ####
#
# Da die komplexeren Modelle besser waren, werde ich nur die Modelle 
# Modelle c(4,1), c(4,2) & c(4,3) verwenden
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

final_fit<-list()
for(i in 1:60){
  final_fit[[i]]<-fit_nn[[150+i]]  
}

m<-matrix(NA,nrow(tbltest),60)
for(i in 1:60){
  if(is.null(final_fit[[i]])) next
  m[,i]<-predict(final_fit[[i]],tbltest)[,1]
}

m<-m[,-which(is.na(m[1,]))]
pred<-rowMeans(m,na.rm = T)

rowSd <-sqrt(rowSums((m-pred)^2)/38)
up<-(pred+rowSd*1.5) *
  (max(lmtrain_safe$warmmiete)-
     min(lmtrain_safe$warmmiete))+
  min(lmtrain_safe$warmmiete)
down<-(pred-rowSd*1.5) *
  (max(lmtrain_safe$warmmiete)-
     min(lmtrain_safe$warmmiete))+
  min(lmtrain_safe$warmmiete)
up<-exp(up)
down<-exp(down)

# "Verschönere" die Daten durch ganzzahliges runden
down[down<1000]<-trunc(down[down<1000]/10)*10
down[down<10000 & down>1000]<-trunc(down[down<10000 & down>1000]/100)*100
down[down>10000]<-trunc(down[ down>10000]/1000)*1000
up[up<1000]<-trunc(up[up<1000]/10)*10
up[up<10000 & up>1000]<-trunc(up[up<10000 & up>1000]/100)*100
up[up>10000]<-trunc(up[ up>10000]/1000)*1000
warmmiete_real<-exp(lmtest_safe$warmmiete)

# Füge Daten zusammen

pred<-exp(
  pred*
    (max(lmtrain_safe$warmmiete)-
       min(lmtrain_safe$warmmiete))+
    min(lmtrain_safe$warmmiete)) %>% 
  trunc()
df<-data.frame(warmmiete_real,down,up,pred)
df$treffer<-(warmmiete_real>down & warmmiete_real < up)
sum(df$treffer)
sum(df$treffer)/nrow(tbltest)  # Treffer
sum(warmmiete_real>up ) # Unterschätzt
sum(warmmiete_real>up )/nrow(tbltest) # Unterschätzt
sum(warmmiete_real<down ) # Überschätzt
sum(warmmiete_real<down )/nrow(tbltest) # Überschätzt

# 5 größten Intervalle (absolut)
df %>% 
  mutate(diff_abs=up-down) %>%
  arrange(desc(diff_abs)) %>%
  dplyr::slice(1:5)

# 5 kleinsten Intervalle (absolut)
df %>% 
  mutate(diff_abs=up-down) %>%
  arrange(diff_abs) %>%
  dplyr::slice(1:5)

# 5 größten prozentuale Intervalle
df %>% 
  mutate(diff_proz=up/down) %>%
  arrange(desc(diff_proz)) %>%
  dplyr::slice(1:5)

# 5 kleinsten prozentuale Intervalle
df %>% 
  mutate(diff_proz=up/down) %>%
  arrange(diff_proz) %>%
  dplyr::slice(1:5)


df[up/down==max(up/down),]
df[up/down==min(up/down,1:3),]
min(df$preis,top=3)

df %>% 
  mutate(preis=preis/1000,
         up = up/1000,
         down=down/1000) %>%
  dplyr::slice(sample(nrow(df),10,F))
accuracy(
  exp(
    rowMeans(m)*
      (max(lmtrain_safe$preis)-
         min(lmtrain_safe$preis))+
      min(lmtrain_safe$preis)),
  exp(lmtrain_safe$preis))
acc_table

dev.off()
par(mfrow=c(1,3))
plot(
  tbltrain$preis,
  pred,
  bty="n",
  pch=20,
  xlab="Preis",
  ylab="Vorhersage",
  col="blue1")
abline(
  a=0,
  b=1,
  col="red")
qqplot(
  tbltrain$preis,
  pred,bty="n",
  pch=20,
  xlab="Quantile Preis",
  ylab="Quantile Vorhersage",
  col="blue3")
plot(tbltrain$preis-pred,bty="n",pch=20,ylab="Residuen",col="blue4")
abline(h=0,col="red")


#save(tbltrain,tbltest,lmtrain_safe,lmtest_safe,xgb_mod,df,final_fit,file = "fits_für_präsi.rda")
