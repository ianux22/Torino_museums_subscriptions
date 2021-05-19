#### Librerie ####
library(WriteXLS)
require(tidyverse)
library(magrittr)
library(RColorBrewer)
library(lubridate)
library(rgdal)
library(plyr)
library(sp)
library(dplyr) # load dplyr
library(tmap)
library(ggplot2)   # for general plotting
library(ggmap)    # for fortifying shapefileslibrary(maptools)
library(raster)
require(maptools)
library(rgeos)
library(tidyverse)
library(lubridate)
library(data.table)
library(arules)
library(arulesViz)
library("devtools")
install_github("mhahsler/arulesViz")
library(caTools)
library(data.table)
library(psych)
library(mvtnorm)
library(caret)
library(PRROC)
library(ggplot2)
library(caTools)
library(pROC)
library(readxl)
library(MASS)
library(e1071)
library(utils)
library(randomForest)
require(caTools)
library(e1071)

#### lavoro sui dataset ####

#Carico Dataset

info.clienti <- read.csv("C:/Users/LEONARDO/Desktop/Falco/exam/Info Clienti.csv", row.names=1)
library(readxl)
info.uso <- read_excel("C:/Users/LEONARDO/Desktop/Falco/exam/Info.uso.xlsx")
churn.data <- read.csv("C:/Users/LEONARDO/Desktop/Falco/exam/Churn.csv")

#rimuovo la variabile professione

info.clienti <- info.clienti[,-11]

#rinomino altre variabili


#prime statistiche descrittive

summary(info.clienti)

#guardo a quali prezzi sono state comprate le tessere

info.uso.1<-info.uso[,c(4,8)]
info.uso.1$importo <- as.numeric(as.character(info.uso.1$importo))
info.uso.agg<-aggregate(info.uso.1,list(CodCliente=info.uso.1$CodCliente),FUN=sum)#list è argomento per variabile su cui aggregare
colnames(info.uso.agg)[3]<-'somma_cod_Cli'
n_visite<-info.uso.agg$somma_cod/info.uso.agg$CodCliente
info.uso.agg<-cbind(info.uso.agg,n_visite)
info.uso.agg<-info.uso.agg[,-3]
colnames(info.uso.agg)[2]<-'spesa_tot'
colnames(info.uso.agg)[1]<-'codcliente'
info.visite <- info.uso.agg
rm(info.uso.agg)
rm(info.uso.1)

totale<-merge(info.visite,info.clienti,by='codcliente')


totale$data_nascita <- as.numeric(as.character(totale$data_nascita))
eta_abbonati <- rep(2013, nrow(totale))-totale$data_nascita
totale.2 <- cbind(totale, eta_abbonati)
totale.3 <- merge(totale.2, churn.data,by="codcliente")
totale <- totale.3
rm(totale.2)
rm(totale.3)

summary(totale$eta_abbonati)

totale <- subset(totale, eta_abbonati>= 5)

sapply(totale, class)

totale$importo <- as.numeric(as.character(totale$importo))
totale$si2014 <- as.factor(totale$si2014)
sapply(totale, class)


#### Matrice di Adiacenza #####
info.uso.2 <- cbind(info.uso, rep(1, length(info.uso$importo)))
visite <- info.uso.2[,c(8,5)]
View(visite)
colnames(info.uso.2) [9]<- "N"
visite_cliente <- aggregate(info.uso.2$N,list(CodCliente = info.uso.2$CodCliente,
                                              museo = info.uso.2$museo),sum)

visite_cliente_date <- aggregate(info.uso.2$N,list(CodCliente = info.uso.2$CodCliente,
                                                   museo = info.uso.2$museo, data= info.uso.2$datai),sum)


head(visite_cliente)
musei <- unique(info.uso.2$museo)
musei <- as.data.frame(musei)
nodes <- musei %>% rowid_to_column("id")
musei_visite <- aggregate(info.uso.2$N, list(museo=info.uso.2$museo), sum)
musei_visite_28 <- subset(musei_visite, x>=3800)
musei_top_28 <- unique(musei_visite_28$museo)
musei_top_28 <- as.data.frame(musei_top_28)
visite_28 <- subset(visite, museo %in% musei_top_28$musei_top_28)

library(data.table)

transazioni <- dcast(data = visite_cliente, CodCliente ~ museo, value.var = "x")
transazioni[is.na(transazioni)] <- 0

visite_cliente_01 <- visite_cliente
visite_cliente_01$x[visite_cliente_01$x >= 2] <- 1

transazioni_01 <- dcast(data = visite_cliente, CodCliente ~ museo, value.var = "x")
transazioni_01[is.na(transazioni_01)] <- 0


##### Variabile "mesi"#######
library(lubridate)

sapply(visite_cliente_date, class)
visite_cliente_date$Month <- format(as.Date(visite_cliente_date$data), "%m")

visite_cliente_date$January <- 0
visite_cliente_date$February <- 0
visite_cliente_date$March <- 0
visite_cliente_date$April <- 0
visite_cliente_date$May <- 0
visite_cliente_date$June <- 0
visite_cliente_date$July <- 0
visite_cliente_date$August <- 0
visite_cliente_date$September <- 0
visite_cliente_date$October <- 0
visite_cliente_date$November <- 0
visite_cliente_date$Dicember <- 0

visite_cliente_date$January[visite_cliente_date$Month == "01"] <- 1
visite_cliente_date$February[visite_cliente_date$Month == "02"] <- 1
visite_cliente_date$March[visite_cliente_date$Month == "03"] <- 1
visite_cliente_date$April[visite_cliente_date$Month == "04"] <- 1
visite_cliente_date$May[visite_cliente_date$Month == "05"] <- 1
visite_cliente_date$June[visite_cliente_date$Month == "06"] <- 1
visite_cliente_date$July[visite_cliente_date$Month == "07"] <- 1
visite_cliente_date$August[visite_cliente_date$Month == "08"] <- 1
visite_cliente_date$September[visite_cliente_date$Month == "09"] <- 1
visite_cliente_date$October[visite_cliente_date$Month == "10"] <- 1
visite_cliente_date$November[visite_cliente_date$Month == "11"] <- 1
visite_cliente_date$Dicember[visite_cliente_date$Month == "12"] <- 1

head(visite_cliente_date)
sapply(visite_cliente_date, class)

Gennaio <- subset(visite_cliente_date, January == 1)
Gennaio$Month <- NULL
Gennaio$February <- NULL
Gennaio$March <- NULL
Gennaio$April <- NULL
Gennaio$May <- NULL
Gennaio$June <- NULL
Gennaio$July <- NULL
Gennaio$August <- NULL
Gennaio$September <- NULL
Gennaio$October <- NULL
Gennaio$November <- NULL
Gennaio$Dicember <- NULL

Gennaio <- aggregate(Gennaio$January, list("ID" = Gennaio$CodCliente), sum)

Gennaio$January <- Gennaio$x
Gennaio$x <- NULL

head(Gennaio)

Febbraio <- subset(visite_cliente_date, February == 1)
Febbraio$Month <- NULL
Febbraio$January <- NULL
Febbraio$March <- NULL
Febbraio$April <- NULL
Febbraio$May <- NULL
Febbraio$June <- NULL
Febbraio$July <- NULL
Febbraio$August <- NULL
Febbraio$September <- NULL
Febbraio$October <- NULL
Febbraio$November <- NULL
Febbraio$Dicember <- NULL

Febbraio <- aggregate(Febbraio$February, list("ID" = Febbraio$CodCliente), sum)

Febbraio$February <- Febbraio$x
Febbraio$x <- NULL

Marzo <- subset(visite_cliente_date, March == 1)
Marzo$Month <- NULL
Marzo$January <- NULL
Marzo$February <- NULL
Marzo$April <- NULL
Marzo$May <- NULL
Marzo$June <- NULL
Marzo$July <- NULL
Marzo$August <- NULL
Marzo$September <- NULL
Marzo$October <- NULL
Marzo$November <- NULL
Marzo$Dicember <- NULL

Marzo <- aggregate(Marzo$March, list("ID" = Marzo$CodCliente), sum)

Marzo$March <- Marzo$x
Marzo$x <- NULL

Aprile <- subset(visite_cliente_date, April == 1)
Aprile$Month <- NULL
Aprile$January <- NULL
Aprile$February <- NULL
Aprile$March <- NULL
Aprile$May <- NULL
Aprile$June <- NULL
Aprile$July <- NULL
Aprile$August <- NULL
Aprile$September <- NULL
Aprile$October <- NULL
Aprile$November <- NULL
Aprile$Dicember <- NULL

Aprile <- aggregate(Aprile$April, list("ID" = Aprile$CodCliente), sum)

Aprile$April <- Aprile$x
Aprile$x <- NULL


Maggio <- subset(visite_cliente_date, May == 1)
Maggio$Month <- NULL
Maggio$January <- NULL
Maggio$February <- NULL
Maggio$March <- NULL
Maggio$April <- NULL
Maggio$June <- NULL
Maggio$July <- NULL
Maggio$August <- NULL
Maggio$September <- NULL
Maggio$October <- NULL
Maggio$November <- NULL
Maggio$Dicember <- NULL

Maggio <- aggregate(Maggio$May, list("ID" = Maggio$CodCliente), sum)

Maggio$May <- Maggio$x
Maggio$x <- NULL


Giugno <- subset(visite_cliente_date, June == 1)
Giugno$Month <- NULL
Giugno$January <- NULL
Giugno$February <- NULL
Giugno$March <- NULL
Giugno$April <- NULL
Giugno$May <- NULL
Giugno$July <- NULL
Giugno$August <- NULL
Giugno$September <- NULL
Giugno$October <- NULL
Giugno$November <- NULL
Giugno$Dicember <- NULL

Giugno <- aggregate(Giugno$June, list("ID" = Giugno$CodCliente), sum)

Giugno$June <- Giugno$x
Giugno$x <- NULL

Luglio <- subset(visite_cliente_date, July == 1)
Luglio$Month <- NULL
Luglio$January <- NULL
Luglio$February <- NULL
Luglio$March <- NULL
Luglio$April <- NULL
Luglio$May <- NULL
Luglio$June <- NULL
Luglio$August <- NULL
Luglio$September <- NULL
Luglio$October <- NULL
Luglio$November <- NULL
Luglio$Dicember <- NULL

Luglio <- aggregate(Luglio$July, list("ID" = Luglio$CodCliente), sum)

Luglio$July <- Luglio$x
Luglio$x <- NULL

Agosto <- subset(visite_cliente_date, August == 1)
Agosto$Month <- NULL
Agosto$January <- NULL
Agosto$February <- NULL
Agosto$March <- NULL
Agosto$April <- NULL
Agosto$May <- NULL
Agosto$June <- NULL
Agosto$July <- NULL
Agosto$September <- NULL
Agosto$October <- NULL
Agosto$November <- NULL
Agosto$Dicember <- NULL

Agosto <- aggregate(Agosto$August, list("ID" = Agosto$CodCliente), sum)

Agosto$August <- Agosto$x
Agosto$x <- NULL

Settembre <- subset(visite_cliente_date, September == 1)
Settembre$Month <- NULL
Settembre$January <- NULL
Settembre$February <- NULL
Settembre$March <- NULL
Settembre$April <- NULL
Settembre$May <- NULL
Settembre$June <- NULL
Settembre$July <- NULL
Settembre$August <- NULL
Settembre$October <- NULL
Settembre$November <- NULL
Settembre$Dicember <- NULL

Settembre <- aggregate(Settembre$September, list("ID" = Settembre$CodCliente), sum)

Settembre$September <- Settembre$x
Settembre$x <- NULL

Ottobre <- subset(visite_cliente_date, October == 1)
Ottobre$Month <- NULL
Ottobre$January <- NULL
Ottobre$February <- NULL
Ottobre$March <- NULL
Ottobre$April <- NULL
Ottobre$May <- NULL
Ottobre$June <- NULL
Ottobre$July <- NULL
Ottobre$August <- NULL
Ottobre$September <- NULL
Ottobre$November <- NULL
Ottobre$Dicember <- NULL

Ottobre <- aggregate(Ottobre$October, list("ID" = Ottobre$CodCliente), sum)

Ottobre$October <- Ottobre$x
Ottobre$x <- NULL



Novembre <- subset(visite_cliente_date, November == 1)
Novembre$Month <- NULL
Novembre$January <- NULL
Novembre$February <- NULL
Novembre$March <- NULL
Novembre$April <- NULL
Novembre$May <- NULL
Novembre$June <- NULL
Novembre$July <- NULL
Novembre$August <- NULL
Novembre$September <- NULL
Novembre$October <- NULL
Novembre$Dicember <- NULL

Novembre <- aggregate(Novembre$November, list("ID" = Novembre$CodCliente), sum)

Novembre$November <- Novembre$x
Novembre$x <- NULL

Dicembre <- subset(visite_cliente_date, Dicember == 1)
Dicembre$Month <- NULL
Dicembre$January <- NULL
Dicembre$February <- NULL
Dicembre$March <- NULL
Dicembre$April <- NULL
Dicembre$May <- NULL
Dicembre$June <- NULL
Dicembre$July <- NULL
Dicembre$August <- NULL
Dicembre$September <- NULL
Dicembre$October <- NULL
Dicembre$November <- NULL

Dicembre <- aggregate(Dicembre$Dicember, list("ID" = Dicembre$CodCliente), sum)

Dicembre$Dicember <- Dicembre$x
Dicembre$x <- NULL


Visite_mesi <- merge(Gennaio, Febbraio, by="ID", all=TRUE )
Visite_mesi <- merge(Visite_mesi, Marzo, by="ID", all=TRUE )
Visite_mesi <- merge(Visite_mesi, Aprile, by="ID", all=TRUE )
Visite_mesi <- merge(Visite_mesi, Maggio, by="ID", all=TRUE )
Visite_mesi <- merge(Visite_mesi, Giugno, by="ID", all=TRUE )
Visite_mesi <- merge(Visite_mesi, Luglio, by="ID", all=TRUE)
Visite_mesi <- merge(Visite_mesi, Agosto, by="ID", all=TRUE)
Visite_mesi <- merge(Visite_mesi, Settembre, by="ID", all=TRUE)
Visite_mesi <- merge(Visite_mesi, Ottobre, by="ID", all=TRUE)
Visite_mesi <- merge(Visite_mesi, Novembre, by="ID", all=TRUE)
Visite_mesi <- merge(Visite_mesi, Dicembre, by="ID", all=TRUE)



Visite_mesi[is.na(Visite_mesi)] <- 0

head(Visite_mesi)

Visite_mesi$January[Visite_mesi$January >= 2] <- 1
Visite_mesi$February[Visite_mesi$February >= 2] <- 1
Visite_mesi$March[Visite_mesi$March >= 2] <- 1
Visite_mesi$April[Visite_mesi$April >= 2] <- 1
Visite_mesi$May[Visite_mesi$May >= 2] <- 1
Visite_mesi$June[Visite_mesi$June >= 2] <- 1
Visite_mesi$July[Visite_mesi$July >= 2] <- 1
Visite_mesi$August[Visite_mesi$August >= 2] <- 1
Visite_mesi$September[Visite_mesi$September >= 2] <- 1
Visite_mesi$October[Visite_mesi$October >= 2] <- 1
Visite_mesi$November[Visite_mesi$November >= 2] <- 1
Visite_mesi$Dicember[Visite_mesi$Dicember >= 2] <- 1

Visite_mesi$sum_month <- rowSums( Visite_mesi[,2:13] )

head(Visite_mesi)

colnames(Visite_mesi)
colnames(Visite_mesi) = c("codcliente", "January", "February", 
                          "March", "April", "May", "June", "July",
                          "August", "September", "October",
                          "November", "Dicember", "mesi_tot")

colnames(Visite_mesi)

Visite_mesi$January <- NULL
Visite_mesi$February <- NULL
Visite_mesi$March <- NULL
Visite_mesi$April <- NULL
Visite_mesi$May <- NULL
Visite_mesi$June <- NULL
Visite_mesi$July <- NULL
Visite_mesi$August <- NULL
Visite_mesi$September <- NULL
Visite_mesi$October <- NULL
Visite_mesi$November <- NULL
Visite_mesi$Dicember <- NULL


totale <- merge(totale, Visite_mesi, by="codcliente", all.x=TRUE)


sapply(totale, class)

rm(Gennaio)
rm(Febbraio)
rm(Marzo)
rm(Aprile)
rm(Maggio)
rm(Giugno)
rm(Luglio)
rm(Agosto)
rm(Settembre)
rm(Ottobre)
rm(Novembre)
rm(Dicembre)

totale$mesi_tot <- as.integer(totale$mesi_tot)
sapply(totale, class)

##### Variabile Residenza #####

totale["Residenza"] <- totale$comune

totale$Residenza <- as.numeric(as.character(totale$Residenza))

library(readxl)
CAP_ITALIANI_VERO <- read_excel("C:/Users/LEONARDO/Desktop/Falco/CAP ITALIANI VERO.xlsx")


CAP_TO_PROV = subset(CAP_ITALIANI_VERO, Regione=="Piemonte" & Provincia=="Torino")
CAP_PIEM_NO_TO = subset(CAP_ITALIANI_VERO, Regione=="Piemonte" & Provincia!="Torino")
totale$Residenza[totale$cap %in% CAP_TO_PROV$CAP ] <- 2
totale$Residenza[totale$cap %in% CAP_PIEM_NO_TO$CAP ] <- 1
totale$Residenza[totale$comune=="TORINO" ] <- 3
totale$Residenza[is.na(totale$Residenza)] <- 0

totale$Residenza <- as.factor(totale$Residenza)

##### Densità età X sesso #####

eta_sex <- ggplot(na.omit(totale), aes(x = eta_abbonati, fill=sesso)) + geom_density(alpha=0.4) + ggtitle("Customer's age") +
              xlab("Age") 
eta_sex + scale_fill_manual( values = c("black","white"))

eta_sex <- ggplot(na.omit(totale), aes(x = eta_abbonati, fill=sesso)) + geom_density(alpha=0.4) + ggtitle("Customer's age") +
  xlab("Age") 
eta_sex + scale_fill_manual( values = c("black","white"), name="Sex",
                             breaks=c("1", "0"),
                             labels=c("Female", "Male")) 



##### Densità età X Churn #####

eta <- ggplot(totale, aes(x = eta_abbonati, fill=si2014)) + geom_density(alpha=0.4) + ggtitle("Churn by age") + xlab("Age")
eta + scale_fill_manual( values = c("red","blue"), name = "Renewal",
                         breaks=c("1", "0"),
                         labels=c("No", "Yes")) 


##### Sesso X Churn #####

sesso_churn <- ggplot(data.frame(totale), aes(x=sesso, fill=si2014)) +
  geom_bar(position="dodge") + ggtitle("Sex and Churn") + xlab("Sex") 

sesso_churn + scale_fill_manual( name = "Renewal",
                                 breaks=c("0", "1"),
                                 labels=c("No", "Yes")) 

##### Residenze X Churn ####

residenze <- ggplot(data.frame(totale), aes(x=Residenza, fill=si2014)) +
  geom_bar()

residenze

sapply(totale, class)




##### Spesa_tot X si2014####

spesa_churn <- ggplot(totale, aes(x = spesa_tot, fill=si2014)) + geom_density(alpha=0.25)
spesa_churn + scale_fill_manual( values = c("black","green")) + xlim(c(0, 250))







##### N_visite X si2014 #####

n_visite <- ggplot(totale, aes(x = n_visite, fill=si2014)) + geom_density(alpha=0.4)
n_visite + scale_fill_manual( values = c("orange","white")) + xlim(c(0,80))





##### Mesi TOT X si2014 ####

mesi_tot <- ggplot(totale, aes(x = mesi_tot, fill=si2014)) + geom_histogram(binwidth = 2)
mesi_tot


#### Diffusione Tessere Musei con Shapefiles ######
#library(sp)
#library(rgdal)


shp <- readOGR(dsn = "C:/Users/LEONARDO/Desktop/Falco/exam", layer="cap_NO")
head(shp@data)
names(shp@data)
summary(shp@data)


#metto in cap tutte le persone che vivono in un cap
library(plyr)
cap = table(totale$cap)
cap
cap = as.matrix(cap)
cap[NA,]<-0
cap = as.data.frame(cap)
cap_vettore=rownames(cap)
cap = cbind(cap, cap_vettore)

names(cap)[1] = 'Freq'
names(cap)[2] = 'cap'

table(cap$Freq)
table(cap$cap)


cap$cap <- as.numeric(as.character(cap$cap))
sapply(cap, class)

#controllo quali cap sono presenti e quali assenti
shp$IT_CAP %in% cap$cap
shp$IT_CAP[!shp$IT_CAP %in% cap$cap]


#library(dplyr) # load dplyr
shp@data <- left_join(shp@data, cap, by = c('IT_CAP' = 'cap'))
names(shp@data)

head(shp@data)
summary(shp@data)

#GGPLOT2

library(ggplot2)   # for general plotting
#library(ggmap)    # for fortifying shapefiles

shp_df <- fortify(shp)
#con Fortify c'è una perdita di informazioni, ma le righe rimangono 
#uguali, quindi basta creare una colonna id che avanza progressivamente
#e usarla come riferimento per il left
shp$id = row.names(shp)
shp_df = left_join(shp_df, shp@data, by="id")
head(shp_df)
head(shp@data)

# Now the shapefile can be plotted as either a geom_path or a geom_polygon.
# Paths handle clipping better. Polygons can be filled.
# You need the aesthetics long, lat, and group.
mappa_tessere = ggplot(shp_df, aes(long, lat, group=group, fill=Freq)) +
  geom_polygon(data=shp_df, aes(x=shp_df$long, y=shp_df$lat), colour='black') + #serve per mettere i bordi
  coord_equal() +
  labs(x = "", y="",
       fill='Number of  \nsubscriptions') + 
  ggtitle("Subscription spread in north Italy") +
  scale_fill_gradient(low='Red', high='yellow') +
  theme_minimal()
mappa_tessere

############## ZOOM : Diffusione tessere in Piemonte  ####

#library(readxl)
comune_cap_pr_regione <- read_excel("C:/Users/LEONARDO/Desktop/Falco/CAP ITALIANI VERO.xlsx")

Cap_Piemonte <- subset(comune_cap_pr_regione, Regione=="Piemonte")
shp.2 <- shp
Cap_Piemonte$CAP <- as.numeric(as.character(Cap_Piemonte$CAP))
sapply(Cap_Piemonte, class)
summary(Cap_Piemonte)
shp.2@data <- subset(shp.2@data, IT_CAP %in% Cap_Piemonte$CAP)
summary(shp.2@data)
summary(Cap_Piemonte$CAP)
head(shp.2@data)


#GGPLOT2

library(ggplot2)   # for general plotting
library(ggmap)    # for fortifying shapefiles

shp.2=subset(shp,IT_CAP%in%Cap_Piemonte$CAP)

shp.2_df <- fortify(shp.2)
#con Fortify c'è una perdita di informazioni, ma le righe rimangono 
#uguali, quindi basta creare una colonna id che avanza progressivamente
#e usarla come riferimento per il left
shp.2$id = row.names(shp.2)
shp.2_df = left_join(shp.2_df, shp.2@data, by="id")
head(shp.2_df)
head(shp.2@data)

# Now the shapefile can be plotted as either a geom_path or a geom_polygon.
# Paths handle clipping better. Polygons can be filled.
# You need the aesthetics long, lat, and group.
mappa_tessere_piemonte = ggplot(shp.2_df, aes(long, lat, group=group, fill=Freq)) +
  geom_polygon(data=shp.2_df, aes(x=shp.2_df$long, y=shp.2_df$lat), colour='black') + #serve per mettere i bordi
  coord_equal() +
  labs(x = "", y="",
       fill='Number of  \nSubscriptions') + 
  ggtitle("Subscriptions in Piemonte") +
  scale_fill_gradient(low='Red', high='yellow') +
  theme_minimal()
mappa_tessere_piemonte



#### Shapefile : Visite per CAP #####
sapply(totale, class)
totale$cap <- as.numeric(as.character(totale$cap))
total_cap <- totale[,c(3, 14)]
#aggrego facendo somma visite tenendo conto del cap
vis_total <- aggregate(total_cap$n_visite, list(total_cap = total_cap$cap), sum)
vis_total
## 
shp@data <- left_join(shp@data, vis_total, by = c('IT_CAP' = 'total_cap'))
shp_df <- fortify(shp)
#con Fortify c'è una perdita di informazioni, ma le righe rimangono 
#uguali, quindi basta creare una colonna id che avanza progressivamente
#e usarla come riferimento per il left
shp$id = row.names(shp)
shp_df = left_join(shp_df, shp@data, by="id")

mappa_visite = ggplot(shp_df, aes(long, lat, group=group, fill=Freq)) +
  geom_polygon(data=shp_df, aes(x=shp_df$long, y=shp_df$lat), colour='black') + #serve per mettere i bordi
  coord_equal() +
  labs(x = "", y="",
       fill='Number of visits') + 
  ggtitle("Visits per city") +
  scale_fill_gradient(low='Green', high='Black') +
  theme_minimal()
mappa_visite

#### Shapefile : Rinnovi per CAP####

shp.copia <- readOGR(dsn = "C:/Users/LEONARDO/Desktop/Falco/exam", layer="cap_NO")

churn_cap <- totale[,c(14, 18)]
churn_cap$cap <- as.numeric(as.character(churn_cap$cap))
churn_cap$si2014 <- as.numeric(as.character(churn_cap$si2014))
sapply(churn_cap, class)
churn_cap_agg <- aggregate(churn_cap$si2014, list(cap = churn_cap$cap), sum)
colnames(churn_cap_agg)[2]="N_churn"
head(churn_cap_agg)
summary(churn_cap_agg)

shp.copia@data <- left_join(shp.copia@data, churn_cap_agg, by = c('IT_CAP' = 'cap'))
shp_cap_churn <- fortify(shp.copia)
#con Fortify c'è una perdita di informazioni, ma le righe rimangono 
#uguali, quindi basta creare una colonna id che avanza progressivamente
#e usarla come riferimento per il left
shp.copia$id = row.names(shp.copia)
shp_cap_churn = left_join(shp_cap_churn, shp.copia@data, by="id")
head(shp.copia@data)

mappa_cap_churn = ggplot(shp_cap_churn, aes(long, lat, group=group, fill=N_churn)) +
  geom_polygon(data=shp_cap_churn, aes(x=shp_cap_churn$long, y=shp_cap_churn$lat), colour='black') + #serve per mettere i bordi
  coord_equal() +
  labs(x = "", y="",
       fill='Number of Renewal') + 
  ggtitle("Renewal per city") +
  scale_fill_gradient(low='black', high='yellow') +
  theme_minimal()
mappa_cap_churn

############## ZOOM : Rinnovi per CAP in Piemonte ####

shp.copia <- readOGR(dsn = "C:/Users/LEONARDO/Desktop/Falco/exam", layer="cap_NO")

head(shp.copia)
shp.copia@data <- left_join(shp.copia@data, churn_cap_agg, by = c('IT_CAP' = 'cap'))
head(shp.copia@data)
shp.Piem = subset(shp.copia,IT_CAP%in%Cap_Piemonte$CAP)
shp.Piem_df <- fortify(shp.Piem)

#con Fortify c'è una perdita di informazioni, ma le righe rimangono 
#uguali, quindi basta creare una colonna id che avanza progressivamente
#e usarla come riferimento per il left
shp.Piem$id = row.names(shp.Piem)
shp.Piem_df = left_join(shp.Piem_df, shp.Piem@data, by="id")

head(shp.Piem_df)
head(shp.Piem@data)
mappa_cap_churn_piemonte = ggplot(shp.Piem_df, aes(long, lat, group=group, fill=N_churn)) +
  geom_polygon(data=shp.Piem_df, aes(x=shp.Piem_df$long, y=shp.Piem_df$lat), colour='black') + #serve per mettere i bordi
  coord_equal() +
  labs(x = "", y="",
       fill='Number of Renewals') + 
  ggtitle("Renewals in Piemonte") +
  scale_fill_gradient(low='black', high='yellow') +
  theme_minimal()
mappa_cap_churn_piemonte


#### Market Basket Analysys ####

library(arules)
library(arulesViz)
library(datasets)
library("devtools")
install_github("mhahsler/arulesViz", force = TRUE)
library(arulesViz)

############## Trasformiamo il dataset per usare arule####

visite_cliente_date$X1<-NULL
visite_cliente_date$x<-NULL

#read transactions
str(visite_cliente_date)

head(visite_cliente_date)

df_sorted <- visite_cliente_date[order(visite_cliente_date$CodCliente),]

#convert member number to numeric
df_sorted$CodCliente <- as.numeric(df_sorted$CodCliente)

#convert item description to categorical format
df_sorted$museo <- as.factor(df_sorted$museo)

# Have a look at the different types of items
table(df_sorted$museo)


#convert dataframe to transaction format using ddply; 
#group all the items that were bought together; by the same customer on the same date
#library(plyr)
df_itemList <- ddply(visite_cliente_date, c("CodCliente","data"), function(df1)paste(df1$museo,collapse = ","))

#remove member number and date
df_itemList$CodCliente <- NULL
df_itemList$data <- NULL

colnames(df_itemList) <- c("itemList")

#write to csv format
write.csv(df_itemList,"ItemList.csv", quote = FALSE, row.names = TRUE)
head(df_itemList)

#load package required
#library(arules)

#convert csv file to basket format
txn = read.transactions(file="ItemList.csv", rm.duplicates= FALSE, format="basket",sep=",",cols=1);
summary(txn)
#remove quotes from transactions
txn@itemInfo$labels <- gsub("\"","",txn@itemInfo$labels)


# Now we can apply the apriori algorithm to the object "txn"
############## Musei più e meno gettonati - ARULE##########
dev.off()

itemFrequencyPlot(txn,
                  type="relative",
                  topN=10, # can be changed to the number of interest
                  horiz=TRUE,
                  col='steelblue3',
                  xlab='',
                  main='Item frequency, relative')

barplot(sort(table(unlist(LIST(txn))))[1:10],
        horiz=TRUE,
        las=1,
        col='steelblue3',
        xlab='',
        main='Frequency, relative')
############## Ricerca Regola #######
lifts <- crossTable(txn, measure='lift',sort=T)
lifts

itemsets <- apriori(txn,
                    parameter = list(support=.0001,
                                     minlen=2,
                                     target='frequent' # to mine for itemsets
                    ))

summary(itemsets)
inspect(sort(itemsets, by='support', decreasing = T)[1:10])

quality(itemsets)$lift <- interestMeasure(itemsets, measure='lift', txn)
inspect(sort(itemsets, by ='lift', decreasing = T)[1:10])

#LENGTH = 2
itemsets.2 <- apriori(txn,
                    parameter = list(support=.0001,
                                     minlen=2,
                                     maxlen=2,
                                     target='frequent' # to mine for itemsets
                    ))
quality(itemsets.2)$lift <- interestMeasure(itemsets.2, measure='lift', txn)
inspect(sort(itemsets.2, by ='lift', decreasing = T)[1:10])

###Looking for the RULE

rules <- apriori(txn,
                 parameter = list(support=.00001,
                                  confidence=.7,
                                  minlen=2,
                                  target='rules' # to mine for rules
                 ))

summary(rules)

inspect(sort(rules, by='lift', decreasing = T))
inspect(sort(rules, by='support', decreasing = T))

library(arulesViz)
plot(rules,method="graph",interactive=TRUE,shading=NA)


############## Cambiando rhs

item<-attr(txn,which="itemInfo")
dim(item)
names(item)
item$labels


#Regole con RHS i 10  museI più visitati in questione

inspect(sort(subset(rules,
                    subset=rhs() %in% 'MOSTRA ELLIOTT ERWITT' & confidence > .7),
             by = 'confidence',
             decreasing = T))

inspect(sort(subset(rules,
                    subset=rhs %in% 'MUSEO ACCORSI-OMETTO' & confidence > .7),
             by = 'confidence',
             decreasing = T))
inspect(sort(subset(rules,
                    subset=rhs %in% 'MUSEO NAZIONALE AUTOMOBILE' & confidence > .7),
             by = 'confidence',
             decreasing = T))
inspect(sort(subset(rules,
                    subset=rhs %in% 'MOSTRA ROBERT CAPA - RETROSPETTIVA' & confidence > .7),
             by = 'confidence',
             decreasing = T))

inspect(sort(subset(rules,
                    subset=rhs %in% 'PALAZZO REALE' & confidence > .7),
             by = 'confidence',
             decreasing = T))

inspect(sort(subset(rules,
                    subset=rhs %in% 'MUSEO EGIZIO' & confidence > .7),
             by = 'confidence',
             decreasing = T))

inspect(sort(subset(rules,
                    subset=rhs %in% 'MUSEO NAZIONALE DEL CINEMA' & confidence > .7),
             by = 'confidence',
             decreasing = T))

inspect(sort(subset(rules,
                    subset=rhs %in% 'GAM - GALLERIA CIVICA ARTE MODERNA E C' & confidence > .7),
             by = 'confidence',
             decreasing = T))

inspect(sort(subset(rules,
                    subset=rhs %in% 'MUSEO CIV. ARTE ANTICA PALAZZO MADAMA' & confidence > .7),
             by = 'confidence',
             decreasing = T))

inspect(sort(subset(rules,
                    subset=rhs %in% 'REGGIA DI VENARIA REALE' & confidence > .7),
             by = 'confidence',
             decreasing = T))

#Plot di rule con rhs i 10 musei top
plot(subset(rules, subset=rhs %in% "MOSTRA ELLIOTT ERWITT" & confidence > .7),method="graph",interactive=TRUE,shading=NA)
#NO MUSEO ACCORSI-OMETTO
#NO MUSEO NAZIONALE AUTOMOBILE
plot(subset(rules, subset=rhs %in% "PALAZZO REALE" & confidence > .99),method="graph",interactive=T,shading=NA)
#troppe
plot(subset(rules, subset=rhs %in% "MUSEO EGIZIO" & confidence > .999),method="graph",interactive=T,shading=NA)

#NO MOSTRA ROBERT CAPA - RETROSPETTIVA
# NO MUSEO NAZIONALE DEL CINEMA
#NO GAM - GALLERIA CIVICA ARTE MODERNA E C
#NO MUSEO CIV. ARTE ANTICA PALAZZO MADAMA
#NO REGGIA DI VENARIA REALE


#Regole con LHS i 10  museI più visitati in questione

inspect(sort(subset(rules,
                    subset=lhs %in% 'MOSTRA ELLIOTT ERWITT' & confidence > .7),
             by = 'confidence',
             decreasing = T)[1:10])

inspect(sort(subset(rules,
                    subset=lhs %in% 'MUSEO ACCORSI-OMETTO' & confidence > .7),
             by = 'confidence',
             decreasing = T)[1:10])
inspect(sort(subset(rules,
                    subset=lhs %in% 'MUSEO NAZIONALE AUTOMOBILE' & confidence > .7),
             by = 'confidence',
             decreasing = T)[1:10])
inspect(sort(subset(rules,
                    subset=lhs %in% 'MOSTRA ROBERT CAPA - RETROSPETTIVA' & confidence > .7),
             by = 'confidence',
             decreasing = T)[1:10])

inspect(sort(subset(rules,
                    subset=lhs %in% 'PALAZZO REALE' & confidence > .7),
             by = 'confidence',
             decreasing = T)[1:10])

inspect(sort(subset(rules,
                    subset=lhs %in% 'MUSEO EGIZIO' & confidence > .7),
             by = 'confidence',
             decreasing = T)[1:10])

inspect(sort(subset(rules,
                    subset=lhs %in% 'MUSEO NAZIONALE DEL CINEMA' & confidence > .7),
             by = 'confidence',
             decreasing = T)[1:10])

inspect(sort(subset(rules,
                    subset=lhs %in% 'GAM - GALLERIA CIVICA ARTE MODERNA E C' & confidence > .7),
             by = 'confidence',
             decreasing = T)[1:10])

inspect(sort(subset(rules,
                    subset=lhs %in% 'MUSEO CIV. ARTE ANTICA PALAZZO MADAMA' & confidence > .7),
             by = 'confidence',
             decreasing = T)[1:10])

inspect(sort(subset(rules,
                    subset=lhs %in% 'REGGIA DI VENARIA REALE' & confidence > .7),
             by = 'confidence',
             decreasing = T)[1:10])

#Plot di rule con LHS i 10 musei top
plot(subset(rules, subset=lhs %in% "MOSTRA ELLIOTT ERWITT" & lift > 25),method="graph",interactive=TRUE,shading=NA)
plot(subset(rules, subset=lhs %in% "MUSEO ACCORSI-OMETTO" & lift > 25),method="graph",interactive=TRUE,shading=NA)
plot(subset(rules, subset=lhs %in% "MUSEO NAZIONALE AUTOMOBILE"& lift > 25),method="graph",interactive=TRUE,shading=NA)
plot(subset(rules, subset=lhs %in% "PALAZZO REALE"& lift > 25),method="graph",interactive=T,shading=NA)
plot(subset(rules, subset=lhs %in% "MUSEO EGIZIO"& lift > 25),method="graph",interactive=T,shading=NA)
plot(subset(rules, subset=lhs %in% "MOSTRA ROBERT CAPA - RETROSPETTIVA"& lift > 25),method="graph",engine = 'interactive',shading=NA)
plot(subset(rules, subset=lhs %in% "MUSEO NAZIONALE DEL CINEMA"& lift > 25),method="graph",interactive=T,shading=NA)
plot(subset(rules, subset=lhs %in% "GAM - GALLERIA CIVICA ARTE MODERNA E C"& lift > 25),method="graph",interactive=T,shading=NA)
#MANCA MUSEO CIV. ARTE ANTICA PALAZZO MADAMA IN LHS
plot(subset(rules, subset=lhs %in% "REGGIA DI VENARIA REALE"),method="graph",interactive=T,shading=NA)


#altri plot di regole
plot(subset(rules, subset=rhs %in% "MUSEO DEL GIOCATTOLO"),method="graph",interactive=TRUE,shading=NA)
plot(subset(rules, subset=rhs %in% "FONDAZIONE PISTOLETTO"),method="graph",interactive=TRUE,shading=NA)
plot(subset(rules, subset=rhs %in% "PALAZZO REALE"),method="graph",interactive=TRUE,shading=NA)
plot(subset(rules, subset=rhs %in% "MUSEO EGIZIO"),method="graph",interactive=TRUE,shading=NA)
plot(subset(rules, subset=rhs %in% "MUSEO DELLA CERAMICA"),method="graph",interactive=T, shading=NA)
plot(subset(rules, subset=rhs %in% "MUSEO DELLA SCUOLA"),method="graph",interactive=TRUE,shading=NA)
plot(subset(rules, subset=rhs %in% "MUSEO CIVICO S. ANASTASIO"),method="graph",interactive=TRUE,shading=NA)
plot(subset(rules, subset=rhs %in% "MUSEO CIVICO DI ARCHEOLOGIA STORIA ARTE"),method="graph",interactive=TRUE,shading=NA)
plot(subset(rules, subset=rhs %in% "MUSEO DELLA FRUTTA"),method="graph",interactive=TRUE,shading=NA)




#### Preparazione LOGIT ####

library(caTools)

levels(totale$sesso)
library(plyr)
totale$sesso <- revalue(totale$sesso, c("M"="0", "F"="1"))
totale$mesi_tot <- as.integer(totale$mesi_tot)
logit.data <- subset(totale[,c(2,3,5,11,16, 18, 22, 23)])

#Mettendo interazione con eta-residenza, Residenza non è più significativa

##### SOM - Internet ####

require(tidyverse)
library(tidyverse)
require(magrittr)
library(magrittr)
library(RColorBrewer)

n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

data = logit.data[,-6]


map_dimension = 20

n_iterations = 1000

recalculate_map = T

recalculate_no_clusters = T


# prepare data

install.packages("magrittr")
library(magrittr)

numerics = summarise_all( logit.data[,-6], is.numeric ) %>%  as.logical()

factors = names(logit.data[,-6])%>%
  .[!numerics]

numerics = names(logit.data[,-6])%>%
  .[numerics]



data_list = list()
distances = vector()

for (fac in factors){
  
  data_list[[fac]] = kohonen::classvec2classmat( logit.data[,-6][[fac]] )
  
  distances = c(distances, 'tanimoto')
  
}

data_list[['numerics']] = scale(data[,numerics])
distances = c( distances, 'euclidean')

str(data_list)

names(data_list)

distances

# create a grid onto which the som will be mapped
# we might want to change map dimension in future versions

som_grid = kohonen::somgrid(xdim = map_dimension
                            , ydim=map_dimension
                            , topo="hexagonal")



if(recalculate_map == F & file.exists('som.Rdata') == T){
  
  load('som.Rdata')
  
} else{
  
  m = kohonen::supersom( data_list
                         , grid=som_grid
                         , rlen= n_iterations
                         , alpha = 0.05
                         , whatmap = c(factors, 'numerics')
                         , dist.fcts = distances
                         #, user.weights = weight_layers
                         #, maxNA.fraction = .5
  )
  
  save(m, file = 'som.Rdata')
  
}

plot(m, type="changes")

plot(m, type="counts")
plot(m, type="dist.neighbours")
plot(m, type="codes")
plot(m, type="quality")

# fuse all layers into one dataframe
codes = tibble( layers = names(m$codes)
                ,codes = m$codes ) %>%
  mutate( codes = purrr::map(codes, as_tibble) ) %>%
  spread( key = layers, value = codes) %>%
  apply(1, bind_cols) %>%
  .[[1]] %>%
  as_tibble()

# generate distance matrix for codes
dist_m = dist(codes) %>%
  as.matrix()

# generate seperate distance matrix for map location

dist_on_map = kohonen::unit.distances(som_grid)


#exponentiate euclidean distance by distance on map
dist_adj = dist_m ^ dist_on_map


#best n of cluster

factoextra::fviz_nbclust(dist_adj
                         , factoextra::hcut
                         , method = "wss"
                         , hc_method = 'ward.D2'
                         , k.max = 15) 

#silhouette method
factoextra::fviz_nbclust(dist_adj
                         , factoextra::hcut
                         , method = "wss"
                         , hc_method = 'ward.D2'
                         , k.max = 15)

#gap statistic 
#set.seed(123)
#gap_stat = cluster::clusGap(dist_adj
#                           , FUN = factoextra::hcut
#                            , K.max = 15
#                            , B = 50
#                            , hc_method = "ward.D2") 

#factoextra::fviz_gap_stat(gap_stat)


###
# Viewing WCSS for kmeans


dist_adj =  dist_m ^ dist_on_map

clust_adj = hclust(as.dist(dist_adj), 'ward.D2')


som_cluster_adj = cutree(clust_adj,3)
plot(m, type="codes", main = "Clusters", bgcol = col_vector[som_cluster_adj], pchs = NA)


link = tibble( map_loc = names(som_cluster_adj) %>% as.integer()
               ,cluster = som_cluster_adj)

pred = tibble( map_loc = m$unit.classif) %>%
  left_join(link)


logit.data_pred = logit.data[,-6] %>%
  bind_cols(pred)

# get vector with cluster value for each original data sample
cluster_assignment <- som_cluster_adj[m$unit.classif]
# for each of analysis, add the assignment as a column in the original data:
logit.data$cluster <- cluster_assignment

logit.data$cluster <- as.factor(logit.data$cluster)


##### Modello LOGIT #####

sapply(logit.data,class)

#Divido in train e Test

require(caTools)
library(e1071)

logit.data.na <- na.omit(logit.data)

split2=sample.split(logit.data.na$importo,SplitRatio=1/10)
train.2=subset(logit.data.na,split2==TRUE)
test.2=subset(logit.data.na,split2==FALSE)

split3=sample.split(logit.data.na$importo,SplitRatio=1/7)
train.3=subset(logit.data.na,split3==TRUE)
test.3=subset(logit.data.na,split3==FALSE)

split4=sample.split(logit.data.na$importo,SplitRatio=45/100)
train.4=subset(logit.data.na,split4==TRUE)
test.4=subset(logit.data.na,split4==FALSE)

split5=sample.split(logit.data.na$importo,SplitRatio=70/100)
train.5=subset(logit.data.na,split5==TRUE)
test.5=subset(logit.data.na,split5==FALSE)

split6=sample.split(logit.data.na$importo,SplitRatio=25/100)
train.6=subset(logit.data.na,split6==TRUE)
test.6=subset(logit.data.na,split6==FALSE)

mylogit <- glm(si2014 ~ ., data = train.4, family = "binomial")
summary(mylogit)
exp(coef(mylogit))


predict.logit <- predict(mylogit, train.4)
table(train.4$si2014, predict.logit > 0.5)

(3851+18699)/ (3851+4805+3961+18699) #0.72 circa

#Il nostro modelo sul dataset di train ci prende il delle volte

prediction.logit.test<- predict(mylogit, test.4)
prediction.logit.test
table(test.4$si2014, prediction.logit.test > 0.5)

(4657+22840) / (4657+5956+4821+22840) #quasi 0.72

#ROCR Curve
library(ROCR)
ROCRpred.1 <- prediction(prediction.logit.test, test.4$si2014)
ROCRperf.1 <- performance(ROCRpred.1, 'tpr','fpr')
plot(ROCRperf.1, colorize = TRUE, text.adj = c(test.4-0.2,1.7))

#The area under curve (AUC), referred to as index of accuracy(A)
#or concordance index, is a perfect performance metric for ROC curve.
#Higher the area under curve, better the prediction power of the model. 
#Below is a sample ROC curve. The ROC of a perfect predictive model has 
#TP equals 1 and FP equals 0. This curve will touch the top left corner 
#of the graph.

exp(coef(mylogit))

b0 <- mylogit$coef[1] # intercept
spesa_tot <- mylogit$coef[2]
n_visite <- mylogit$coef[3]
importo <- -mylogit$coef[4]
sex <- mylogit$coef[5]
eta_Abbonati.1 <- mylogit$coef[6]
mesi_logit <- mylogit$coef[7]
Residenza1 <- mylogit$coef[8]
Residenza2 <- mylogit$coef[9]
Residenza3 <- mylogit$coef[10]
cluster2 <- mylogit$coef[11]
cluster3 <- mylogit$coef[12]

n_visite_mean <- mean(logit.data$n_visite)
n_visite_mean
importo_mean <- mean(logit.data$importo)
importo_mean
spesa_mean <- mean(logit.data$spesa_tot)


eta_range <- seq.int(from=0, to=100, by=1)
eta_range

eta_range<-as.numeric(as.character(eta_range))

a_logits <- (b0 + 
  n_visite*n_visite_mean +
    +spesa_tot*spesa_mean+
  importo*importo_mean + 
  eta_Abbonati.1*eta_range +
  Residenza1*0 + 
  Residenza2*0 +
  Residenza3*0 + 
    sex*0 +
    mesi_logit*3+
    cluster2*1 )


a_logits

b_logits <- b0 + 
    n_visite*n_visite_mean + 
  spesa_tot*spesa_mean+
  
    importo*importo_mean + 
    eta_Abbonati.1*eta_range + 
    Residenza1*1 + 
    Residenza2*0 + 
    Residenza3*0 + 
    sex*0 + 
  mesi_logit*3+
  cluster2*1 

c_logits <- b0 + 
      n_visite*n_visite_mean + 
      importo*importo_mean + 
      eta_Abbonati.1*eta_range + 
  spesa_tot*spesa_mean+
  
      Residenza1*0 + 
      Residenza2*1 + 
      Residenza3*0 + 
      sex*0 +
  mesi_logit*3+
  cluster2*1 

d_logits <- b0 + 
      n_visite*n_visite_mean + 
      importo*importo_mean + 
  spesa_tot*spesa_mean+
  
      eta_Abbonati.1*eta_range + 
      Residenza1*0 + 
      Residenza2*0 + 
      Residenza3*1 + 
      sex*0 +    
  mesi_logit*3+
  cluster2*1 


d_logits
   
# Compute the probibilities (this is what will actually get plotted):
a_probs <- exp(a_logits)/(1 + exp(a_logits))
b_probs <- exp(b_logits)/(1 + exp(b_logits))
c_probs <- exp(c_logits)/(1 + exp(c_logits))
d_probs <- exp(d_logits)/(1 + exp(d_logits))


###CON GGPLOT2
library(ggplot2); 
library(tidyr)
# first you have to get the information into a long dataframe, which is what ggplot likes :)
plot.data <- data.frame(Other_Region=a_probs, Piemonte=b_probs, Prov_TO=c_probs, Torino=d_probs, X1=eta_range)
plot.data <- gather(plot.data, key=group, value=prob, c(Other_Region, Piemonte, Prov_TO, Torino))

ggplot(plot.data, aes(x=X1, y=prob, color=group)) + # asking it to set the color by the variable "group" is what makes it draw three different lines
  geom_line(lwd=2) + 
  labs(x="Age", y="P(Renewal)", title="Renewal Probability")



### library(stargazer)
### stargazer(mylogit)



##### SVM ####

library(e1071)
##NB IMPORTANTE

svm_tune.5 <- tune(svm, si2014 ~ ., data = train.6,
                   ranges = list(gamma = seq(0.1,0.8,0.1), cost = (2:6)),
                   .parallel=TRUE)

print(svm_tune.5)

#####gamma = 0.6

svm_model_after_tune <- svm(si2014 ~ ., data=train.4, kernel="radial", cost=3, gamma=0.6)
summary(svm_model_after_tune)

pred_train <- predict(svm_model_after_tune, train.4)
table(pred_train, train.4$si2014)

(2642+21194) / (2462+1471+6189+21194) #0.7611445

pred_test <- predict(svm_model_after_tune, test.4)
table(pred_test, test.4$si2014)

(2828+25686) / (2828+1975+7785+25686) #0.7472436



#ROCR Curve
library(ROCR)

svm_model_after_tune_prob <- svm(si2014 ~ ., data=train.4, kernel="radial", cost=3, gamma=0.6, probability=TRUE)
prob_test_SVM <- predict(svm_model_after_tune_prob, test.4, type='prob', probability=TRUE)
prob_test_SVM
head(attr(prob_test_SVM, "probabilities"))
prob_SVM <- attr(prob_test_SVM, "probabilities")
prob_SVM


svm_pred_mat <- as.data.frame.matrix(prob_SVM)
svm_pred_prob_1=svm_pred_mat[,1]
svm_prob_roc<-prediction(svm_pred_prob_1,test.4[,6])
svm_ROC <- performance(svm_prob_roc, "tpr","fpr")
plot(svm_ROC,col='blue',lwd=1)

 
##### Random Forest ####

library(randomForest)

gc()
bestmtry <- tuneRF(train.3[,-6], train.3$si2014, stepFactor=1.5, improve=1e-5, ntree=500)
Model_RF <- randomForest(si2014 ~ ., data = train.3, ntree = 500, mtry = 2)

Model_RF
sapply(train.3, class)
# Predicting on train set
pred_RF <- predict(Model_RF, train.3, type = "class")
pred_RF
train.3$si2014 <- as.numeric(as.character(train.3$si2014))
train.3$si2014 <- as.factor(train.3$si2014)
# Checking classification accuracy
table(pred_RF, train.3$si2014)  

(1872+7029) / (1872+158+884+7029)  #0.89

# Checking classification accuracy
pred_RF_test <- predict(Model_RF, test.3, type = "class")
table(pred_RF_test, test.3$si2014)  


(5071+39166) / (5071+3968+11442+39166) #0.74

#Rilancio con più osservazioni


bestmtry <- tuneRF(train.4[,-6], train.4$si2014, stepFactor=1.5, improve=1e-5, ntree=500)
Model_RF <- randomForest(si2014 ~ ., data = train.4, ntree = 500, mtry = 2)
Model_RF
# Predicting on train set
pred_RF_train <- predict(Model_RF, train.4, type = "class")
# Checking classification accuracy
table(pred_RF_train, train.4$si2014)  

(4521+21884) / (4521+776+4135+21884)  #0.84

# Checking classification accuracy
pred_RF_test <- predict(Model_RF, test.4, type = "class")
table(pred_RF_test, test.4$si2014)  


(3081+25509) / (3081+2152+7532+25509) #0.74

#ROCR Curve
library(ROCR)

prob_RF_test <- predict(Model_RF, test.4, type = "prob")
prob_RF_test

rf_pred_matrix=as.data.frame.matrix(prob_RF_test)
rf_pred_prob_1=rf_pred_matrix[,2]
rf_prob_roc<-prediction(rf_pred_prob_1,test.4[,6])
rf_ROC <- performance(rf_prob_roc, "tpr","fpr")
plot(rf_ROC,col='green',main='ROC curve',lwd=1,xlab='False Positive', ylab='True Positive')


###### Curve ROC a confronto #####

plot(rf_ROC,col='black',main='ROC curves',lwd=1,xlab='False Positive', ylab='True Positive')
plot(svm_ROC,col='blue',lwd=1,add=T)
plot(ROCRperf.1,col='orange',lwd=1,add=T)
legend('right',legend=c('RandomForest','SVM','logit'),col=c('black','blue','orange'),lty=1,lwd=2,cex=0.6)

##### Ricavi attesi ####

#Creo variabile di costo

test.4$costo <- ((test.4$spesa_tot/2)+1)

#Random Forest

# Checking classification accuracy

prob_RF_test <- predict(Model_RF, test.4, type = "prob")
prob_RF_test

exp_value_RF <- (prob_RF_test[,2] * (test.4$importo-test.4$costo)) - (prob_RF_test[,1] * (test.4$costo))
exp_value_RF <- sort(exp_value_RF, decreasing=TRUE)
cum_exp_value_RF <- cumsum(exp_value_RF)


plot(cum_exp_value_RF, x=c(1:length(cum_exp_value_RF)))

#SVM
svm_model_after_tune_prob <- svm(si2014 ~ ., data=train.4, kernel="radial", cost=3, gamma=0.6, probability=TRUE)


prob_test_SVM <- predict(svm_model_after_tune_prob, test.4, probability=TRUE)
prob_test_SVM

head(attr(prob_test_SVM, "probabilities"))
prob_SVM <- attr(prob_test_SVM, "probabilities")
prob_SVM

exp_value_SVM <- (prob_SVM[,1] * (test.4$importo-test.4$costo)) - (prob_SVM[,2] * (test.4$costo))
exp_value_SVM <- sort(exp_value_SVM, decreasing=TRUE)
cum_exp_value_SVM <- cumsum(exp_value_SVM)

plot(cum_exp_value_SVM, x=c(1:length(cum_exp_value_SVM)))


#Logit

prediction.logit.test

odds <- exp(prediction.logit.test)
prob_logit <- odds / (1 + odds)
prob_logit

val_atteso_logit <- prob_logit * (test.4$importo-test.4$costo) + (1-prob_logit) * (-test.4$costo)
val_atteso_logit <- sort(val_atteso_logit, decreasing=TRUE)
cum_Val_atteso_logit <- cumsum(val_atteso_logit)

plot(cum_Val_atteso_logit, x=c(1:length(cum_Val_atteso_logit)))

cum_lo <- as.data.frame(cum_Val_atteso_logit)
cum_lo$ID <- 1:length(cum_lo$cum_Val_atteso_logit)

cum_logit <- ggplot(cum_lo, aes(x = cum_Val_atteso_logit, y = ID)) + geom_density(alpha=0.4)
cum_logit



#### Disegno tutte le curve

num<-c(1:length(exp_value_SVM))
cum_data<-data.frame(num, cum_exp_value_RF, cum_Val_atteso_logit, cum_exp_value_SVM)


library(reshape2)
library(ggplot2)

exp_profit_data <- melt(cum_data, id="num")

Profit_curves <-ggplot(data=exp_profit_data, aes(x=num, y=value,colour=variable))+geom_line()+
  scale_color_manual(values=c("black", "red", "green"),
                     name="Models", breaks=c("cum_exp_value_RF","cum_Val_atteso_logit", "cum_exp_value_SVM"),
                     labels=c("Random Forest","Logit",'SVM'))+
  theme_minimal()+
  labs(title="Profit Curves", x="Number of Consumers", y=" ")

Profit_curves  + geom_vline(xintercept = 5000)

riga_5000 <- subset(cum_data, num==5000)
riga_5000