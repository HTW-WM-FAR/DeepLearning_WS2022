
# EN 

There are four R files.

- immonet web scraping_mieten.R
  - Start: read data
    - The result is the file  20221223miete.rda 

- scrapping_korrektur.R
  - The data was incorrect (see seminar paper).
  - Therefore it was tried to correct the data scrapping_correction.R.
    - The result is the file 20221223miete2.rda 

- AusreißerCheck.R
  - Here the data was analyzed and converted (Numeric -> Factor for example).
    - The result is the file tbl.rda this is used for the modeling

- fit.R
  - This is where the actual modeling takes place. 
  - Here are several files created, which I saved to avoid having to recalculate outputs not to have to calculate again, or to make results reproducible
    - <fit_nn_1_50x.rda> all 210 neural networks - The file is too big for upload (250 MB), if you are interested we will have to find another way.
    - <acc_table.txt> The accuracy data for the all models (without bootstrap aggregation).
    - <acc_table2.txt> The Accuracy data for the all Bootstrap Aggregation models 



# DE

Es gibt vier R Dateien.
  
- immonet web scraping_mieten
  - Start: Daten auslesen immonet web scraping_mieten.R
    - Das Ergebnis ist die Datei  20221223miete.rda  

- scrapping_korrektur.R
  - Die Daten waren Fehlerhaft (siehe Seminararbeit). Daher wurde versucht die Daten zu korrigieren scrapping_korrektur.R
    - Das Ergebnis ist die Datei 20221223miete2.rda 

-  AusreißerCheck.R
  - Hier wurden die Daten analysiert und umgewandelt (Numeric -> Factor z.B.)
    - Das Ergebnis ist die Datei tbl.rda diese wird für die Modellierung verwendet

-  fit.R
  - Hier findet die eigentliche Modellierung statt. 
  - Hier sind mehrere Dateien entstanden, die ich abgespeichert habe, um Outputs nicht erneut berechnen zu müssen, oder um Ergebnisse reproduzierbar zu machen
    - fit_nn_1_50x.rda alle 210 neuronale Netze - Die Datei ist zu groß für den Upload (250 MB), bei Interesse müssen wir einen anderen Weg finden.
    - acc_table.txt Die Accuracy Daten für die alle Modelle (ohne Bootstrap Aggregation)
    - acc_table2.txt Die Accuracy Daten für die alle Bootstrap Aggregation Modelle 
