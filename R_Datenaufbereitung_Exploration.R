

#### Verbindung zu MS-SQL Server herstellen


library(odbc)
library(RODBC)
con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "LAPTOP-J5J6KNQV\MSSQLSERVER01",
                 Database = "SEC_Companies",
                 UID = "LAPTOP-J5J6KNQV\Startklar"
                 )

con <- dbConnect(odbc::odbc(), "mydbalias")




#
# Open connection to SQL Server using a Windows ODBC DSN
# local instance of SQL Server - zuvor festgelegt
# verwendet zuvor festgelegte Configurations

# versucht die Verbindung aufzubauen
db_conn <- odbcConnect("LocalDSN", rows_at_time = 1)

# Prüfe ob Verbindung aufbauen kann
if(db_conn == -1) {
  quit("no", 1)
}


# Import der Nicht-Betrugsunternehmen
sql <- "select * from dbo.Input_SEC_Companies_NONFRAUD;"
non_fraud_data <- sqlQuery(db_conn, sql, stringsAsFactors = FALSE)

# Import der Betrugsunternehmen
sql <- "select * from dbo.Input_SEC_Companies_3;"
fraud_data <- sqlQuery(db_conn, sql, stringsAsFactors = FALSE)

    # Close the DB connection
odbcClose(db_conn)
    
    
    #
    # Examine my R connected to SQL Server awesomeness!
    #
View(fraud_data)
class(fraud_data) #data.frame
colnames(fraud_data)


# 
dim(fraud_data)# 28 145
dim(non_fraud_data) # 59 145

fraud_data$Company

head()

########################################################################

# Packages:
library(MASS)
library(tidyverse)
library(sf)
library(tmap)
library(dplyr)

library(gridExtra)
library(raster)
library(rasterVis)
library(rcartocolor)
library(ggplot2)
library(e1071)
library(corrplot)
library(tidyverse)
library(caret)


#all_data<-rbind(fraud_data, non_fraud_data)
colnames(non_fraud_data)
colnames(fraud_data)

# die letzten 2 Variablenamen der beiden Datensätze sind überflüssig
# werden daher entfernt

fraud_data_1<-fraud_data[,c(1:143)]
non_fraud_data_1<-non_fraud_data[,c(1:143)]
dim(non_fraud_data_1)

# Zusammenfügen der zwei Datensätze (Betrug und Nicht-Betrug)
all_data_1<-rbind(fraud_data_1, non_fraud_data_1)


# FRAUD_1_NONFRAUD_0 - in character umwandeln, um 
all_data_1$FRAUD_1_NONFRAUD_0_STR<-as.character(all_data_1$FRAUD_1_NONFRAUD_0)


# Variablen (Spalten) entfernen
all_data_2 <- all_data_1[, -which(names(all_data_1) %in% c("Company", "Stichtag", "Stichtag_Vorjahr",
                                                           "ANM", "FRAUD_1_NONFRAUD_0", "FRAUD_TYPE"))]


all_data_cor<-all_data_1[, -which(names(all_data_1) %in% c("Company", "Stichtag", "Stichtag_Vorjahr",
                                                           "ANM", "FRAUD_1_NONFRAUD_0_STR", "FRAUD_TYPE"))]

# entfernen von Int Variablen, die nur angeben
# ob operativer CF positiv oder negativ war
all_data_3 <- all_data_2[, -which(names(all_data_2) %in% c("POS_NEG_O_CF_J",
                                                           "POS_NEG_O_CF_VJ",
                                                           "POS_NEG_O_CF_4"))]

# alle unnötigen Variablen von Wiley-Kennzahlen aus Gesamtdatensatz entfernen:
all_data_4<-subset(all_data_2, select=-c(CR_VJ, CR_3, CR_4, QR_J, QR_VJ, 
                                         QR_3, QR_4, QR_CHANGE, QR_CHANGE_34,
                                         INV_J, INV_VJ, INV_3, INV_4, 
                                         AV_REC_J, AV_REC_VJ, AV_REC_3, AV_REC_4,
                                         T_LIAB_J, T_LIAB_VJ, T_LIAB_3, T_LIAB_4,
                                         AV_ASSET_J, AV_ASSET_VJ, AV_ASSET_3, 
                                         AV_ASSET_4,
                                         CR_J, INV_TR_J, INV_TR_VJ, REC_TR_3, REC_TR_4,
                                         DER_J, DER_VJ, DER_3, DER_4, PMR_J, PMR_VJ,
                                         PMR_3, PMR_4, ASSET_TR_J, ASSET_TR_VJ, ASSET_TR_3, 
                                         ASSET_TR_4, REC_TR_J, REC_TR_VJ,
                                         INV_TR_3, INV_TR_4))


dim(all_data_4) # 87 91


# aber neue Variablen daraus erzeugbar
# J_change_var1 = J_var1_J/J_var1_VJ etc....
# Umwandlung in Veränderungsvariablen

all_data_4$J_change_var1<-all_data_4$J_var1_J/all_data_4$J_var1_VJ
all_data_4$J_change_var2<-all_data_4$J_var2_J/all_data_4$J_var2_VJ
all_data_4$J_change_var3<-all_data_4$J_var3_J/all_data_4$J_var3_VJ

all_data_4$J_change_var1_34<-all_data_4$J_var1_3/all_data_4$J_var1_4
all_data_4$J_change_var2_34<-all_data_4$J_var2_3/all_data_4$J_var2_4
all_data_4$J_change_var3_34<-all_data_4$J_var3_3/all_data_4$J_var3_4

all_data_4$MJ_change_var2<-all_data_4$MJ_var2_J/all_data_4$MJ_var2_VJ
all_data_4$MJ_change_var2_34<-all_data_4$MJ_var2_3/all_data_4$MJ_var2_4

all_data_5 <- all_data_4
colnames(all_data_5)

# alle Einzelvariablen die nur ein Jahr betreffen aus Datensatz
# herausgelöscht 
# Verhältniskennzahlen, die nur ein Jahr betreffen können nicht relevant sein

all_data_5<-subset(all_data_4, select=-c(J_var1_J, J_var1_VJ, J_var1_3, J_var1_4,
                                         J_var2_J, J_var2_VJ, J_var2_3, J_var2_4,
                                         J_var3_J, J_var3_VJ, J_var3_3, J_var3_4,
                                         MJ_var2_J, MJ_var2_VJ, MJ_var2_3, MJ_var2_4))


dim(all_data_5) # 87 83

# Verhältniskennzahlen z-Score
all_data_5$zscore_1_change<-all_data_5$ZSCORE_1_J/all_data_5$ZSCORE_1_VJ
all_data_5$zscore_2_change<-all_data_5$ZSCORE_2_J/all_data_5$ZSCORE_2_VJ
all_data_5$zscore_3_change<-all_data_5$ZSCORE_3_J/all_data_5$ZSCORE_3_VJ
all_data_5$zscore_4_change<-all_data_5$ZSCORE_4_J/all_data_5$ZSCORE_4_VJ
all_data_5$zscore_5_change<-all_data_5$ZSCORE_5_J/all_data_5$ZSCORE_5_VJ

colnames(all_data_5)

all_data_5<-subset(all_data_5, select=-c(ZSCORE_1_J, ZSCORE_1_VJ, ZSCORE_2_J, ZSCORE_2_VJ,
                                         ZSCORE_3_J, ZSCORE_3_VJ, ZSCORE_4_J, ZSCORE_4_VJ,
                                         ZSCORE_5_J, ZSCORE_5_VJ))

dim(all_data_5) # 87 79
# 87 Zeilen, 79 Spalten

# NAs entfernen
all_data_5[is.na(all_data_5)] <- 0

# neue Variablen - Verhältnisse von Jones und Modified Jones
# all_data_5 



colnames(all_data_5)


### Corrplot 

corr_df<-data.frame(cor(all_data_cor))
corr_df$FRAUD_1_NONFRAUD_0







##############################################################################
################################################################################
# Datenanalyse von Beneish-M-Score:
##############################################################################

# Loop für jede Kategorie erzeugen, um Histogramme der Daten zu erhalten
# damit ich die Verteilung der Daten in Histogramm
# je nach FRAUD=1 oder Nicht Fraud = 0 sehen kann 

# Beneish-M-Score
colnames(all_data_1)

beneish_m<-all_data_1[,c(1,7, 2:6,8:14)]
length(colnames(beneish_m)) # 14

# Loop, der Histogramm für jede Beneish-M-Variable erzeugt:
colNames<-names(beneish_m)[3:14]
for(i in colNames){
  plt <- ggplot(beneish_m, aes_string(x=i, fill="FRAUD_1_NONFRAUD_0_STR")) +
    geom_histogram() + stat_bin(bins=50) 
  plt<- plt + labs(fill = "Betrug: 0 \nkein Betrug: 1")
  print(plt)
  
  ggsave(plt, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}


# select all columns except one
# mtcars[, -which(names(mtcars) == "carb")]

colnames(beneish_m)
# Veränderung Zielvariable 

colnames(beneish_m)
a<-beneish_m[2:14]

# Logistische Regression bei Verwendung beneish-M-Variablen zur Identifkation
# Fraud
a$FRAUD_1_NONFRAUD_0_STR <- as.factor(a$FRAUD_1_NONFRAUD_0_STR)
model <- glm( FRAUD_1_NONFRAUD_0_STR ~., data = a, family = binomial)
summary(model)

# Pr(>|z) --> keine Koeffizienten signifikant
# AIC: 118.91
anova(model, test="Chisq")

# SGI_1      1   6.6598        77     95.973 0.009861 **
model <- glm( FRAUD_1_NONFRAUD_0_STR ~SGI_1, data = a, family = binomial)
summary(model)
anova(model, test="Chisq")
# keine einzige Variable ist signifikant 

beneish_m[,c("M_Score")]


# Untersuchen der Beneish-Kennzahlen und einteilen
# gem. Beneish-Tabelle
#        NONFRAUD    FRAUD
# DSRI:     1.031      1.465
# GMI       1.014     1.193
# AQI:      1.039     1.254
# SGI:      1.134     1.607
# TATA:     0.018     0.031

beneish_m_kz<-beneish_m
colnames(beneish_m_kz)

# gem. Erkenntnisse von Beneish
# Wenn KZ > als lt. Forschung sind - dann FRAUD
# wenn z.B. größer 1.465 bei DSRI - dann ist es FRAUD lt. Beneish
# Abgleich mit realen Werten wie gut Kennzahlen funktionieren


# Wenn Variable > als Grenzwert (siehe oben) ist
# dann Fraud sonst kein Fraud

beneish_m_kz$DSRI_res <- ifelse(beneish_m_kz$DSRI>1.465, 1, 0)
beneish_m_kz$DSRI_res1 <- ifelse(beneish_m_kz$DSRI_1>1.465, 1, 0)

beneish_m_kz$DSRI_res == beneish_m_kz$FRAUD_1_NONFRAUD_0

table(fraud = beneish_m_kz$FRAUD_1_NONFRAUD_0, Calc_Ergebnis = beneish_m_kz$DSRI_res1)

table(beneish_m_kz$FRAUD_1_NONFRAUD_0 , beneish_m_kz$DSRI_res1)
table(beneish_m_kz$FRAUD_1_NONFRAUD_0 , beneish_m_kz$DSRI_res)

beneish_m_kz$GMI_res <- ifelse(beneish_m_kz$GMI>1.193, 1, 0)
beneish_m_kz$GMI_res1 <- ifelse(beneish_m_kz$GMI_1>1.193, 1, 0)

table(beneish_m_kz$FRAUD_1_NONFRAUD_0 , beneish_m_kz$GMI_res)
table(beneish_m_kz$FRAUD_1_NONFRAUD_0 , beneish_m_kz$GMI_res1)

beneish_m_kz$AQI_res <- ifelse(beneish_m_kz$AQI>1.254, 1, 0)
beneish_m_kz$AQI_res1 <- ifelse(beneish_m_kz$AQI_1>1.254, 1, 0)

beneish_m_kz$SGI_res <- ifelse(beneish_m_kz$SGI>1.607, 1, 0)
beneish_m_kz$SGI_res1 <- ifelse(beneish_m_kz$SGI_1>1.607, 1, 0)

beneish_m_kz$TATA_res <- ifelse(beneish_m_kz$TATA>0.031, 1, 0)
beneish_m_kz$TATA_res1 <- ifelse(beneish_m_kz$TATA_1>0.031, 1, 0)


##########################################################################



# Analyse des Beneish-M-Scores
# Eingabe verschiedener Grenzwerte - wenn Werte > Grenzwert = dann Betrug


beneish_m_kz <- beneish_m
colnames(beneish_m)
# DSRI GMI "AQI, SGI, TATA

Grenzwert<- 2.94

jahr_x <- ifelse(beneish_m_kz$Prob>=Grenzwert, 1, 0)
jahr_x1 <- ifelse(beneish_m_kz$Prob_1>=Grenzwert, 1, 0)

table_X<-table(fraud = beneish_m_kz$FRAUD_1_NONFRAUD_0, 
               Calc_Ergebnis = jahr_x)

table_X1<-table(fraud = beneish_m_kz$FRAUD_1_NONFRAUD_0, 
               Calc_Ergebnis = jahr_x1)

mean((beneish_m_kz$FRAUD_1_NONFRAUD_0) == jahr_x)

# Sensitivität = Prozentuelle Anteil der richtig klassifizierten
# Betrugsunternehmen
table_X[4]/(table_X[2] + table_X[4])*100

# Spezifität: Prozentuelle Anteil der richtig nicht betrügerisch klassifizierten
# Unternehmen 
table_X[1]/(table_X[3]+table_X[1])*100


table_X1[4]/(table_X1[2] + table_X1[4])*100
table_X1[1]/(table_X1[3]+table_X1[1])*100




# Erzeugen von Tabellen - Vergleich Tatsächliche Werte vs. Trefferquote

#beneish_m_kz$DSRI_res <- ifelse(beneish_m_kz$DSRI>1.465, 1, 0)
#beneish_m_kz$DSRI_res1 <- ifelse(beneish_m_kz$DSRI_1>1.465, 1, 0)

table_1<-table(fraud = beneish_m_kz$FRAUD_1_NONFRAUD_0, 
               Calc_Ergebnis = beneish_m_kz$DSRI_res1)

#mean(beneish_m_kz$DSRI_res == (beneish_m_kz$FRAUD_1_NONFRAUD_0))
#(beneish_m_kz$DSRI_res[28:87])

table_1<-table(fraud = beneish_m_kz$FRAUD_1_NONFRAUD_0, 
               Calc_Ergebnis = beneish_m_kz$DSRI_res1)

# Sensitivität = Prozentuelle Anteil der richtig klassifizierten
# Betrugsunternehmen
table_1[4]/(table_1[2] + table_1[4])

# Spezifität: Prozentuelle Anteil der richtig nicht betrügerisch klassifizierten
# Unternehmen 
table_1[1]/(table_1[3]+table_1[1])


table(beneish_m_kz$FRAUD_1_NONFRAUD_0 , beneish_m_kz$DSRI_res1)
table(beneish_m_kz$FRAUD_1_NONFRAUD_0 , beneish_m_kz$DSRI_res)

beneish_m_kz$GMI_res <- ifelse(beneish_m_kz$GMI>1.193, 1, 0)
beneish_m_kz$GMI_res1 <- ifelse(beneish_m_kz$GMI_1>1.193, 1, 0)

table(beneish_m_kz$FRAUD_1_NONFRAUD_0 , beneish_m_kz$GMI_res)
table(beneish_m_kz$FRAUD_1_NONFRAUD_0 , beneish_m_kz$GMI_res1)

beneish_m_kz$AQI_res <- ifelse(beneish_m_kz$AQI>1.254, 1, 0)
beneish_m_kz$AQI_res1 <- ifelse(beneish_m_kz$AQI_1>1.254, 1, 0)

beneish_m_kz$SGI_res <- ifelse(beneish_m_kz$SGI>1.607, 1, 0)
beneish_m_kz$SGI_res1 <- ifelse(beneish_m_kz$SGI_1>1.607, 1, 0)

beneish_m_kz$TATA_res <- ifelse(beneish_m_kz$TATA>0.031, 1, 0)
beneish_m_kz$TATA_res1 <- ifelse(beneish_m_kz$TATA_1>0.031, 1, 0)




# Auswertung der Beneish-M Kennzahlen auf Treffergenauigkeit
# auf Wahrscheinlichkeiten
# Grenzwerte: 6,85%, 3,76% und 2,94 % gem. Originalstudie:

beneish_m_kz$Prob

Grenzwert <- 2.94

# Mittelwerte: 14, 15 16 17
# median: 3 3.5 4
Grenzwert <- 2.5


probo <- ifelse(beneish_m_kz$Prob>Grenzwert, 1, 0)
table_1<-table(fraud = beneish_m_kz$FRAUD_1_NONFRAUD_0, 
               Calc_Ergebnis = probo)

# Sensitivität = Prozentuelle Anteil der richtig klassifizierten
# Betrugsunternehmen
table_1[4]/(table_1[2] + table_1[4])

# Spezifität: Prozentuelle Anteil der richtig nicht betrügerisch klassifizierten
# Unternehmen 
table_1[3]/(table_1[3]+table_1[1])

t_n<-table_1[1]
f_n<-table_1[3]
t_p<-table_1[4]
f_p<-table_1[2]

precision<-t_p / (t_p + f_p)
recalls<- t_p / (t_p + f_n)




###############################
# Evaluierung der Daten anhand von M-Score
# Einteilung in :
# 

beneish_m$FRAUD_1_NONFRAUD_0<-as.factor(beneish_m$FRAUD_1_NONFRAUD_0)
abs(beneish_m$M_Score)
abs(beneish_m$M_Score_1)

pnorm(beneish_m[,c("M_Score")])
pnorm(abs(beneish_m[,c("M_Score")]))

#################################################################################


# Ermittlung der M-Score Wahrscheinlichkeit:

library(dplyr)
# Transformation M-Score in Wahrscheinlichkeit:
# # Berechnung des Integrals um die kumulative Standardnormalverteilung zu berechnen, mit mü: 0 und Stdabw: 1
beneish_m$M_Score
beneish_m[,c("M_Score")]

probability_mscore<-pnorm(beneish_m[,c("M_Score")])
beneish_m$Prob<-round(probability_mscore*100, 10)
probability_mscore_1<-pnorm(beneish_m[,c("M_Score_1")])
beneish_m$Prob_1<-round(probability_mscore_1*100, 10)

probability_mean<-beneish_m %>% group_by(FRAUD_1_NONFRAUD_0) %>% summarise(
  Mittelwert = mean(Prob), Median = median(Prob), Mittelwert_1 = mean(Prob_1), Median_1= median(Prob_1)
)

colnames(beneish_m)

colnames(beneish_m)
beneish_m[,c(2,15)] # M-Score Jahr X + X-1
beneish_m[,c(2,16)] # M-Score Jahr X-1 + X-2



# Mittelwerte der Wahrscheinlichkeiten für betrügerische-(1) und 
# nicht betrügerische Unternehmen (0)
probability_mean<-beneish_m %>% group_by(FRAUD_1_NONFRAUD_0_STR) %>% summarise(
  Mittelwert = mean(Prob), Mittelwert_1 = mean(Prob_1)
)

# Visualisierung der Daten
library("ggpubr")
ggboxplot(beneish_m, x = "FRAUD_1_NONFRAUD_0_STR", y = "Prob", 
          color = "FRAUD_1_NONFRAUD_0_STR", palette = c("#00AFBB", "#E7B800"),
          ylab = "Probability for Fraud", xlab = "FRAUD vs. NON FRAUD")

ggboxplot(beneish_m, x = "FRAUD_1_NONFRAUD_0_STR", y = "Prob_1", 
          color = "FRAUD_1_NONFRAUD_0_STR", palette = c("#00AFBB", "#E7B800"),
          ylab = "Probability for Fraud", xlab = "FRAUD vs. NON FRAUD")

# https://bjoernwalther.com/mann-whitney-u-test-wilcoxon-test-in-r-rechnen/
# Wilcox-Test/Mann-Whitney U-Test ist derselbe
wilcox.test(Prob ~ FRAUD_1_NONFRAUD_0_STR, data=beneish_m, 
            exact=FALSE)

# p-value = 0.5671 - nicht signifikant
# keine statistische Abweichung zwischen den Rängen der WK feststellbar


wilcox.test(Prob_1 ~ FRAUD_1_NONFRAUD_0_STR, data=beneish_m, 
            exact=FALSE)


# p-value = 0.3024 - nicht signifikant
# keine statistische Abweichung zwischen den Rängen der WK feststellbar


plt <- ggplot(beneish_m, aes_string(x="Prob", fill="FRAUD_1_NONFRAUD_0_STR")) +
  geom_histogram() + stat_bin(bins=50) 

plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")

plt <- ggplot(beneish_m, aes_string(x="Prob_1", fill="FRAUD_1_NONFRAUD_0_STR")) +
  geom_histogram() + stat_bin(bins=50) 

plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")


# histogram für Wahrscheinlichkeiten erstellen
##############################


######## Mittelwerte und Median Ergebnisse von Beneish-Kennzahlen
# shapiro Test, T-Test und Wilcoxon-Test


# DSRI, GMI, AQI, SGI, TATA
m_DSRI<-beneish_m %>% group_by(FRAUD_1_NONFRAUD_0_STR) %>%
  summarise(Mittelwert = mean(DSRI), Mittelwert_1 = mean(DSRI_1),
            Median = median(DSRI), Median_1 = median(DSRI_1))

beneish_m %>% group_by(FRAUD_1_NONFRAUD_0_STR) %>%
  summarise(count=n(), Mittelwert = mean(DSRI), Mittelwert_1 = mean(DSRI_1),
            sd= sd(DSRI), sd_1= sd(DSRI_1))

with(beneish_m, shapiro.test(DSRI[FRAUD_1_NONFRAUD_0_STR=="0"]))
with(beneish_m, shapiro.test(DSRI[FRAUD_1_NONFRAUD_0_STR=="1"]))


t.test(DSRI ~ FRAUD_1_NONFRAUD_0_STR, data=beneish_m)
t.test(DSRI_1 ~ FRAUD_1_NONFRAUD_0_STR, data=beneish_m)

# p-Wert größer als 0.05 --> Unterschied zwischen Mittelwert 
# beider Gruppen ist nicht signifikant

# wichtig - Daten sind zumeist nicht NV
# daher besser Wilcoxon Test verwenden für signifikanz

# Frage: Unterscheidet sich der Mittelwert zwischen
# Fraud=1 und Nicht-FRAUD=0 signfikant?

# http://www.sthda.com/english/wiki/unpaired-two-samples-wilcoxon-test-in-r
wilcox.test(DSRI ~ FRAUD_1_NONFRAUD_0_STR, data=beneish_m, 
            exact=FALSE)
# kein signifikantes Ergebnis

wilcox.test(DSRI_1 ~ FRAUD_1_NONFRAUD_0_STR, data=beneish_m, 
            exact=FALSE)
# kein signifikantes Ergebnis

# p-Value: wenn p-Wert größer 0.05 --> kein signifikanter Unterschied
# p-Value < kleiner als 0.05 --> signifikanter Unterschied zwischen beiden Gruppen

############################


m_GMI<-beneish_m %>% group_by(FRAUD_1_NONFRAUD_0_STR) %>%
  summarise(Mittelwert = mean(GMI), Mittelwert_1 = mean(GMI_1),
            Median = median(GMI), Median_1 = median(GMI_1))

wilcox.test(GMI ~ FRAUD_1_NONFRAUD_0_STR, data=beneish_m, 
            exact=FALSE)
# kein signifikantes Ergebnis

wilcox.test(GMI_1 ~ FRAUD_1_NONFRAUD_0_STR, data=beneish_m, 
            exact=FALSE)
# kein signifikantes Ergebnis

###############################


m_AQI<-beneish_m %>% group_by(FRAUD_1_NONFRAUD_0_STR) %>%
  summarise(Mittelwert = mean(AQI), Mittelwert_1 = mean(AQI_1),
            Median = median(AQI), Median_1 = median(AQI_1))

wilcox.test(AQI ~ FRAUD_1_NONFRAUD_0_STR, data=beneish_m, 
            exact=FALSE)
# kein signifikantes Ergebnis

wilcox.test(AQI_1 ~ FRAUD_1_NONFRAUD_0_STR, data=beneish_m, 
            exact=FALSE)
# kein signifikantes Ergebnis


####################################

m_SGI<-beneish_m %>% group_by(FRAUD_1_NONFRAUD_0_STR) %>%
  summarise(Mittelwert = mean(SGI), Mittelwert_1 = mean(SGI_1),
            Median = median(SGI), Median_1 = median(SGI_1))


wilcox.test(SGI ~ FRAUD_1_NONFRAUD_0_STR, data=beneish_m, 
            exact=FALSE)
# kein signifikantes Ergebnis


wilcox.test(SGI_1 ~ FRAUD_1_NONFRAUD_0_STR, data=beneish_m, 
            exact=FALSE)
# bissi signifikantes Ergebnis





######################################
m_TATA<-beneish_m %>% group_by(FRAUD_1_NONFRAUD_0_STR) %>%
  summarise(Mittelwert = mean(TATA), Mittelwert_1 = mean(TATA_1),
            Median = median(TATA), Median_1 = median(TATA_1))


wilcox.test(TATA ~ FRAUD_1_NONFRAUD_0_STR, data=beneish_m, 
            exact=FALSE)
# kein signifikantes Ergebnis

wilcox.test(TATA_1 ~ FRAUD_1_NONFRAUD_0_STR, data=beneish_m, 
            exact=FALSE)
# kein signifikantes Ergebnis





























##############################################################################
################################################################################
# Datenanalyse von Kennzahlen
##############################################################################

# Wiley-Kennzahlen: ab CR_J: current_ratio
wiley_kz<-all_data_1[,c(1,144, 15:81)]
length(colnames(wiley_kz)) # 69


# Erzeugen von Histogrammen für jede Kennzahl
colNames<-names(wiley_kz)[3:69]
for(i in colNames){
  plt <- ggplot(beneish_m, aes_string(x=i, fill="FRAUD_1_NONFRAUD_0_STR")) +
    geom_histogram() + stat_bin(bins=50) 
  plt<- plt + labs(fill = "Betrug: 0 \nkein Betrug: 1")
  print(plt)
  
  ggsave(plt, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}


wiley_kz_var<-wiley_kz[,c(2,4:69)]
wiley_kz_var$FRAUD_1_NONFRAUD_0_STR<-as.factor(wiley_kz_var$FRAUD_1_NONFRAUD_0_STR)
is.factor(wiley_kz_var$FRAUD_1_NONFRAUD_0_STR)


# logistische Regression: wiley_kz
model <- glm(FRAUD_1_NONFRAUD_0_STR ~., data = wiley_kz_var, family = binomial)
summary(model)
summary(model)$coef
glm()



################################################
# Trennung der Kennzahlen in eigene Datensätze
# jeder Sub-Datensatz enthält nur ähnliche zusammengehörige Kennzahlen:


###############################################################################

########################################################################
# current ratios
########################################################################

colnames(wiley_sig)
colnames(all_data_1)

# Aufsplitten Wiley-Kennzahlen:
# current_ratios 
wiley_cr<-all_data_1[,c(1,144, 16:21)]
wiley_cr_1<-wiley_cr[,c(2:8)]
length(colnames(wiley_cr)) # 8
summary(wiley_cr_1)
wiley_cr_1$FRAUD_1_NONFRAUD_0_STR<-as.factor(wiley_cr_1$FRAUD_1_NONFRAUD_0_STR)



colnames(wiley_cr)
a<-wiley_cr[2:8]
class(a$FRAUD_1_NONFRAUD_0_STR)
a$FRAUD_1_NONFRAUD_0_STR <- as.factor(a$FRAUD_1_NONFRAUD_0_STR)
model <- glm( FRAUD_1_NONFRAUD_0_STR ~., data = a, family = binomial)
summary(model)
anova(model, test="Chisq")

# Summary bzw. Anova Ergebnis in Txt-File speichern
capture.output(summary(model), file = "wiley_cr.txt")
capture.output((anova(model, test="Chisq")), file = "anova_wiley_cr.txt")

# auch keine einzige Variable signifikant 

# Name der Variable
datafile<-wiley_cr

colNames<-names(datafile)[3:8] # Spalte die ausgewertet werden
for(i in colNames){
  plt <- ggplot(datafile, aes_string(x=i, fill="FRAUD_1_NONFRAUD_0_STR")) +
    geom_histogram() + stat_bin(bins=50) 
  plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
  #print(plt)
  
  ggsave(plt, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}



########################################################################
# Quick Ratio 
########################################################################

wiley_qr<-all_data_1[,c(1,144, 22:27)]
length(colnames(wiley_qr)) # 8

colnames(a)
a<-wiley_qr[2:8]
class(a$FRAUD_1_NONFRAUD_0_STR)
a$FRAUD_1_NONFRAUD_0_STR <- as.factor(a$FRAUD_1_NONFRAUD_0_STR)
model <- glm( FRAUD_1_NONFRAUD_0_STR ~., data = a, family = binomial)
summary(model)
anova(model, test="Chisq")

# Summary bzw. Anova Ergebnis in Txt-File speichern
capture.output(summary(model), file = "wiley_qr.txt")
capture.output((anova(model, test="Chisq")), file = "anova_wiley_qr.txt")


# Name der Variable
datafile<-wiley_cr

colNames<-names(datafile)[3:8] # Spalte die ausgewertet werden
for(i in colNames){
  plt <- ggplot(datafile, aes_string(x=i, fill="FRAUD_1_NONFRAUD_0_STR")) +
    geom_histogram() + stat_bin(bins=50) 
  plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
  #print(plt)
  
  ggsave(plt, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}



########################################################################
# Inventory Change and Ratio 
########################################################################

wiley_ir<-all_data_1[,c(1,144, 28:39)]
length(colnames(wiley_ir)) # 8


colnames(a)
a<-wiley_ir[2:8]
class(a$FRAUD_1_NONFRAUD_0_STR)
a$FRAUD_1_NONFRAUD_0_STR <- as.factor(a$FRAUD_1_NONFRAUD_0_STR)
model <- glm( FRAUD_1_NONFRAUD_0_STR ~., data = a, family = binomial)
summary(model)
anova(model, test="Chisq")

# Summary bzw. Anova Ergebnis in Txt-File speichern
capture.output(summary(model), file = "wiley_inv.txt")
capture.output((anova(model, test="Chisq")), file = "anova_wiley_inv.txt")



# Name der Variable
datafile<-wiley_ir

colNames<-names(datafile)[3:8] # Spalte die ausgewertet werden
for(i in colNames){
  plt <- ggplot(datafile, aes_string(x=i, fill="FRAUD_1_NONFRAUD_0_STR")) +
    geom_histogram() + stat_bin(bins=50) 
  plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
  #print(plt)
  
  ggsave(plt, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}

########################################################################
# average Receivables Change + receivable turnover-Kennzahl
########################################################################

wiley_rec<-all_data_1[,c(1,144, 40:51)]
length(colnames(wiley_rec)) # 14


colnames(a)
a<-wiley_rec[2:14]
class(a$FRAUD_1_NONFRAUD_0_STR)
a$FRAUD_1_NONFRAUD_0_STR <- as.factor(a$FRAUD_1_NONFRAUD_0_STR)
model <- glm( FRAUD_1_NONFRAUD_0_STR ~., data = a, family = binomial)
summary(model)
anova(model, test="Chisq")

# Summary bzw. Anova Ergebnis in Txt-File speichern
capture.output(summary(model), file = "wiley_rec.txt")
capture.output((anova(model, test="Chisq")), file = "anova_wiley_rec.txt")






# Name der Variable
datafile<-wiley_rec

colNames<-names(datafile)[3:14] # Spalte die ausgewertet werden
for(i in colNames){
  plt <- ggplot(datafile, aes_string(x=i, fill="FRAUD_1_NONFRAUD_0_STR")) +
    geom_histogram() + stat_bin(bins=50) 
  plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
  #print(plt)
  
  ggsave(plt, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}

# keine Auffälligkeiten oder signifikanten Unterschiede erkennbar


########################################################################
# Liabilities Change
########################################################################
# --> enthält absolute Werte (keine Verhältniswerte)
# sind für die weiteren Auswertungen nicht relevant

wiley_lia<-all_data_1[,c(1,144, 52:57)]
length(colnames(wiley_lia)) # 8

# Name der Variable
datafile<-wiley_lia

colNames<-names(datafile)[3:8] # Spalte die ausgewertet werden
for(i in colNames){
  plt <- ggplot(datafile, aes_string(x=i, fill="FRAUD_1_NONFRAUD_0_STR")) +
    geom_histogram() + stat_bin(bins=50) 
  plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
  #print(plt)
  
  ggsave(plt, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}

# keine Auffälligkeiten oder signifikanten Unterschiede erkennbar


a<-wiley_lia[2:8]
colnames(a)
class(a$FRAUD_1_NONFRAUD_0_STR)
a$FRAUD_1_NONFRAUD_0_STR <- as.factor(a$FRAUD_1_NONFRAUD_0_STR)
model <- glm( FRAUD_1_NONFRAUD_0_STR ~., data = a, family = binomial)
summary(model)
anova(model, test="Chisq")

# Summary bzw. Anova Ergebnis in Txt-File speichern
capture.output(summary(model), file = "wiley_lia.txt")
capture.output((anova(model, test="Chisq")), file = "anova_wiley_lia.txt")


########################################################################
# debt equity ratios: DER_....
########################################################################


wiley_der<-all_data_1[,c(1,144, 58:63)]
length(colnames(wiley_der)) # 8


a<-wiley_der[2:8]
colnames(a)
class(a$FRAUD_1_NONFRAUD_0_STR)
a$FRAUD_1_NONFRAUD_0_STR <- as.factor(a$FRAUD_1_NONFRAUD_0_STR)
model <- glm( FRAUD_1_NONFRAUD_0_STR ~., data = a, family = binomial)
summary(model)
anova(model, test="Chisq")

# Summary bzw. Anova Ergebnis in Txt-File speichern
capture.output(summary(model), file = "wiley_der.txt")
capture.output((anova(model, test="Chisq")), file = "anova_wiley_der.txt")


# Name der Variable
datafile<-wiley_der

colNames<-names(datafile)[3:8] # Spalte die ausgewertet werden
for(i in colNames){
  plt <- ggplot(datafile, aes_string(x=i, fill="FRAUD_1_NONFRAUD_0_STR")) +
    geom_histogram() + stat_bin(bins=50) 
  plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
  #print(plt)
  
  ggsave(plt, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}

# ein bissi erkennbar: DER_3, DER_J, DER_VJ
# 




########################################################################
# profit margin ratio
########################################################################


wiley_pmr<-all_data_1[,c(1,144, 64:69)]
length(colnames(wiley_pmr)) # 8

a<-wiley_pmr[2:8]
colnames(a)
class(a$FRAUD_1_NONFRAUD_0_STR)
a$FRAUD_1_NONFRAUD_0_STR <- as.factor(a$FRAUD_1_NONFRAUD_0_STR)
model <- glm( FRAUD_1_NONFRAUD_0_STR ~., data = a, family = binomial)
summary(model)
anova(model, test="Chisq")

# Summary bzw. Anova Ergebnis in Txt-File speichern
capture.output(summary(model), file = "wiley_pmr.txt")
capture.output((anova(model, test="Chisq")), file = "anova_wiley_pmr.txt")


# Name der Variable
datafile<-wiley_pmr

colNames<-names(datafile)[3:8] # Spalte die ausgewertet werden
for(i in colNames){
  plt <- ggplot(datafile, aes_string(x=i, fill="FRAUD_1_NONFRAUD_0_STR")) +
    geom_histogram() + stat_bin(bins=50) 
  plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
  #print(plt)
  
  ggsave(plt, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}

# Abweichungen bei PMR_4 - bissi
# PMR_CHANGE
# vielleicht PMR_VJ

########################################################################
# average Asset, Asset Turnover Rate
########################################################################


wiley_asset<-all_data_1[,c(1,144, 70:81)]
length(colnames(wiley_asset)) # 14


a<-wiley_asset[2:14]
colnames(a)
class(a$FRAUD_1_NONFRAUD_0_STR)
a$FRAUD_1_NONFRAUD_0_STR <- as.factor(a$FRAUD_1_NONFRAUD_0_STR)
model <- glm( FRAUD_1_NONFRAUD_0_STR ~., data = a, family = binomial)
summary(model)
anova(model, test="Chisq")

# Summary bzw. Anova Ergebnis in Txt-File speichern
capture.output(summary(model), file = "wiley_asset.txt")
capture.output((anova(model, test="Chisq")), file = "anova_wiley_asset.txt")


# Name der Variable
datafile<-wiley_asset

colNames<-names(datafile)[3:14] # Spalte die ausgewertet werden
for(i in colNames){
  plt <- ggplot(datafile, aes_string(x=i, fill="FRAUD_1_NONFRAUD_0_STR")) +
    geom_histogram() + stat_bin(bins=50) 
  plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
  #print(plt)
  
  ggsave(plt, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}

# Auffälligkeiten:
# ASSET_TR_CHANGE
# ASSET_TR_CHANGE_34

########################################################################


########################################################################
# F-Score Auswertung
########################################################################

colnames(all_data_1[,c(82:91)])
# F Score
f_score<-all_data_1[,c(1,7, 82:91)]
f_score<-all_data_1[,c(1,144, 82:91)]

length(colnames(f_score)) # 12

colnames(f_score)

a<-f_score[2:12]
colnames(a)
class(a$FRAUD_1_NONFRAUD_0_STR)
a$FRAUD_1_NONFRAUD_0_STR <- as.factor(a$FRAUD_1_NONFRAUD_0_STR)
model <- glm( FRAUD_1_NONFRAUD_0_STR ~., data = a, family = binomial)
summary(model)
anova(model, test="Chisq")




# Name der Variable
datafile<-f_score

colNames<-names(datafile)[3:12] # Spalte die ausgewertet werden
for(i in colNames){
  plt <- ggplot(datafile, aes_string(x=i, fill="FRAUD_1_NONFRAUD_0_STR")) +
    geom_histogram() + stat_bin(bins=50) 
  plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
  #print(plt)
  
  ggsave(plt, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}

# ein wenig auffällig: CHANGE_INV_J und CHANGE_INV_VJ
# CHANGE_REC_AV_J und CHANGE_REC_AV_VJ
# RSST_ACC_J, RSST_ACC_VJ

########################################################################
# Performance Variablen von F-Score:
########################################################################


performance<-all_data_1[,c(1,144, 92:99)]
length(colnames(performance)) # 10


a<-performance[2:10]
colnames(a)
class(a$FRAUD_1_NONFRAUD_0_STR)
a$FRAUD_1_NONFRAUD_0_STR <- as.factor(a$FRAUD_1_NONFRAUD_0_STR)
model <- glm( FRAUD_1_NONFRAUD_0_STR ~., data = a, family = binomial)
summary(model)
anova(model, test="Chisq")

# Summary bzw. Anova Ergebnis in Txt-File speichern
capture.output(summary(model), file = "wiley_perf.txt")
capture.output((anova(model, test="Chisq")), file = "anova_wiley_perf.txt")






# Name der Variable
datafile<-performance

colNames<-names(datafile)[3:10] # Spalte die ausgewertet werden
for(i in colNames){
  plt <- ggplot(datafile, aes_string(x=i, fill="FRAUD_1_NONFRAUD_0_STR")) +
    geom_histogram() + stat_bin(bins=50) 
  plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
  #print(plt)
  
  ggsave(plt, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}

# CHANGE_CASH_MARGIN_VJ - Betrug tendenziell geringer
# Change_FREE_CF - Betrug eher geringer 0 
# 


########################################################################
# operativer Cashflow positiv / negativ
########################################################################



pos_neg_CF<-all_data_1[,c("FRAUD_1_NONFRAUD_0", "POS_NEG_O_CF_J", 
                          "POS_NEG_O_CF_VJ", "POS_NEG_O_CF_4")]

pos_neg_CF$POS_NEG_O_CF_Y2<-pos_neg_CF$POS_NEG_O_CF_J + pos_neg_CF$POS_NEG_O_CF_VJ



dim(all_data_1)
colnames(all_data_1)

# Positiver negativer Cashflow 0 oder 1 für Jahr, Vorjahr und VorVorjahr
#pos_neg_CF<-all_data_1[,c(1,144, 112:114)]
length(colnames(pos_neg_CF)) # 5


a<-pos_neg_CF[2:5]
colnames(a)
class(a$FRAUD_1_NONFRAUD_0_STR)
a$FRAUD_1_NONFRAUD_0_STR <- as.factor(a$FRAUD_1_NONFRAUD_0_STR)
model <- glm( FRAUD_1_NONFRAUD_0_STR ~., data = a, family = binomial)
summary(model)
anova(model, test="Chisq")

# Summary bzw. Anova Ergebnis in Txt-File speichern
capture.output(summary(model), file = "wiley_pos_neg_ocf.txt")
capture.output((anova(model, test="Chisq")), file = "anova_wiley_pos_neg_ocf.txt")


# Name der Variable
datafile<-pos_neg_CF

colNames<-names(datafile)[3:5] # Spalte die ausgewertet werden
for(i in colNames){
  plt <- ggplot(datafile, aes_string(x=i, fill="FRAUD_1_NONFRAUD_0_STR")) +
    geom_histogram() + stat_bin(bins=50) 
  plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
  #print(plt)
  
  ggsave(plt, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}


# Analyse Log. Regression: pos-neg-cf-Analys

pos_neg_CF$FRAUD_1_NONFRAUD_0 <-as.factor(pos_neg_CF$FRAUD_1_NONFRAUD_0)

summary(model)


# keine log. Analyse - sondern nur Abgleich
# ob Fraud = 0 bei Jahr X bedeutet bei CashFlow pos-neg
colnames(pos_neg_CF)

pos_neg_CF[,c("FRAUD_1_NONFRAUD_0", "POS_NEG_O_CF_J")]

pos_neg_CF$POS_NEG_O_CF_J + pos_neg_CF$POS_NEG_O_CF_VJ + pos_neg_CF$POS_NEG_O_CF_4

table(pos_neg_CF$FRAUD_1_NONFRAUD_0, pos_neg_CF$POS_NEG_O_CF_J)/28


training.samples <- pos_neg_CF$FRAUD_1_NONFRAUD_0 %>% 
  createDataPartition(p = 0.7, list = FALSE)

# Zuweisung der Indexwerte zu Train und Test
train.data  <- pos_neg_CF[training.samples, ]
test.data <- pos_neg_CF[-training.samples, ]

classifier = randomForest(x = training_set[-3],
                          y = training_set$Purchased,
                          ntree = 500, random_state = 0)


y_pred = predict(classifier, newdata = test_set[-3])

########################################################################
# Altman-Z-Score
########################################################################

z_score<-all_data_1[,c(1,144, 100:111)]
length(colnames(z_score)) # 14


a<-z_score[2:14]
colnames(a)
class(a$FRAUD_1_NONFRAUD_0_STR)
a$FRAUD_1_NONFRAUD_0_STR <- as.factor(a$FRAUD_1_NONFRAUD_0_STR)
model <- glm( FRAUD_1_NONFRAUD_0_STR ~., data = a, family = binomial)
summary(model)
anova(model, test="Chisq")

# Summary bzw. Anova Ergebnis in Txt-File speichern
capture.output(summary(model), file = "wiley_zscore.txt")
capture.output((anova(model, test="Chisq")), file = "anova_wiley_zscore.txt")


# Correlation Plot erzeugen, um Korrelationen unter den Werten untereinander
# zu erkennen
a<-z_score[2:14]
a$FRAUD_1_NONFRAUD_0_STR <- as.numeric(a$FRAUD_1_NONFRAUD_0_STR)
cor(a)
par(mfrow=c(1,1))
corrplot(cor(a), method="number",number.cex=0.75)



# Name der Variable
datafile<-z_score

colNames<-names(datafile)[3:14] # Spalte die ausgewertet werden
for(i in colNames){
  plt <- ggplot(datafile, aes_string(x=i, fill="FRAUD_1_NONFRAUD_0_STR")) +
    geom_histogram() + stat_bin(bins=50) 
  plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
  #print(plt)
  
  ggsave(plt, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}

#######################################################################

# Analyse Z-Score Ergebnisse: 

# Feststellung, welche Unternehmen laut Z-Score insolvenzgefährdet sind:

colnames(all_data_5)

f_nf<-all_data_5$FRAUD

TF_J_181<-all_data_5$ZSCORE_RESULT_J <1.81
TF_VJ_181<-all_data_5$ZSCORE_RESULT_VJ <1.81

TF_J_267<-all_data_5$ZSCORE_RESULT_J > 2.67
TF_VJ_267<-all_data_5$ZSCORE_RESULT_VJ > 2.67

# Alle Ergebnisse in ein DF laden
df_z_results <-data.frame(f_nf, TF_J_181, TF_VJ_181, TF_J_267, TF_VJ_267)
colnames(df_z_results)

# Aufsplitten in FRAUD = 1 und non FRAUD 0 DATAFRAME
df_z_results_0<-df_z_results[which(df_z_results$f_nf == 0),]
df_z_results_1<-df_z_results[which(df_z_results$f_nf == 1),]

df_z_results %>% group_by(f_nf) %>% count()

# Auswertung Prozentuelle Anteil
# an TRUE-Ergebnissen bei NON-FRAUD Unternehmen

df_z_results_0  %>% group_by(f_nf) %>% summarise(TF_J_181 = sum(TF_J_181)/59, 
                                                 TF_VJ_181 = sum(TF_VJ_181)/59,
                                                 TF_J_267 = sum(TF_J_267/59),
                                                 TF_VJ_267 = sum(TF_VJ_267)/59)
# Auswertung Prozentuelle Anteil
# an TRUE-Ergebnissen bei FRAUD Unternehmen

df_z_results_1  %>% group_by(f_nf) %>% summarise(TF_J_181 = sum(TF_J_181)/28, 
                                                 TF_VJ_181 = sum(TF_VJ_181)/28,
                                                 TF_J_267 = sum(TF_J_267/28),
                                                 TF_VJ_267 = sum(TF_VJ_267)/28)


# Chi-Quadrat-Test der Ergebnisse auf Signifikanz

chisq.test(df_z_results$f_nf, df_z_results$TF_J_181, correct=FALSE)
fisher.test(df_z_results$f_nf, df_z_results$TF_J_181)

chisq.test(df_z_results$f_nf, df_z_results$TF_VJ_181, correct=FALSE)
fisher.test(df_z_results$f_nf, df_z_results$TF_VJ_181)

chisq.test(df_z_results$f_nf, df_z_results$TF_J_267, correct=FALSE)
fisher.test(df_z_results$f_nf, df_z_results$TF_J_267)

chisq.test(df_z_results$f_nf, df_z_results$TF_VJ_267, correct=FALSE)
fisher.test(df_z_results$f_nf, df_z_results$TF_VJ_267)


(all_data_5$ZSCORE_RESULT_J) <1.81 %>% count()


########################################################################
# Jones und Modified-Jones Modell:
########################################################################



########################################################################
# Accruals Balance Sheet Methode:
########################################################################

acc_bc<-all_data_1[,c(1,144, 134:137)]
length(colnames(acc_bc)) # 6

a<-acc_bc[2:6]
colnames(a)
class(a$FRAUD_1_NONFRAUD_0_STR)
a$FRAUD_1_NONFRAUD_0_STR <- as.factor(a$FRAUD_1_NONFRAUD_0_STR)
model <- glm( FRAUD_1_NONFRAUD_0_STR ~., data = a, family = binomial)
summary(model)
anova(model, test="Chisq")

# Summary bzw. Anova Ergebnis in Txt-File speichern
capture.output(summary(model), file = "acc_bc.txt")
capture.output((anova(model, test="Chisq")), file = "anova_acc_bc.txt")


# Name der Variable
datafile<-acc_bc

colNames<-names(datafile)[3:6] # Spalte die ausgewertet werden
for(i in colNames){
  plt <- ggplot(datafile, aes_string(x=i, fill="FRAUD_1_NONFRAUD_0_STR")) +
    geom_histogram() + stat_bin(bins=50) 
  plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
  #print(plt)
  
  ggsave(plt, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}

# alle Variablen - ein wenig


########################################################################
########################################################################
# Accruals Cash Flow Methode:
########################################################################



acc_cf<-all_data_1[,c(1,144, 138:141)]
length(colnames(acc_cf)) # 6

a<-acc_cf[2:6]
colnames(a)
class(a$FRAUD_1_NONFRAUD_0_STR)
a$FRAUD_1_NONFRAUD_0_STR <- as.factor(a$FRAUD_1_NONFRAUD_0_STR)
model <- glm( FRAUD_1_NONFRAUD_0_STR ~., data = a, family = binomial)
summary(model)
anova(model, test="Chisq")

# Summary bzw. Anova Ergebnis in Txt-File speichern
capture.output(summary(model), file = "acc_cf.txt")
capture.output((anova(model, test="Chisq")), file = "anova_acc_cf.txt")


# Name der Variable
datafile<-acc_cf

colNames<-names(datafile)[3:6] # Spalte die ausgewertet werden
for(i in colNames){
  plt <- ggplot(datafile, aes_string(x=i, fill="FRAUD_1_NONFRAUD_0_STR")) +
    geom_histogram() + stat_bin(bins=50) 
  plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
  #print(plt)
  
  ggsave(plt, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}


########################################################################
# Jones-Modell Variablen:
########################################################################

# Variablen für Total_Accruals berechnen- Jones-Modell
# Berechnung für ACC_CF oder ACC_BC
# mit Prediction für Jones-Modell

dim(all_data_1)
colnames(j_acc_var)
dim(j_acc_var)
j_acc_var<-all_data_1[,c(1,7, 118:129,142)]
j_acc_var$FRAUD_1_NONFRAUD_0 <- as.factor(j_acc_var$FRAUD_1_NONFRAUD_0)


colnames(all_data_1[,c(118:129)])

#j_acc_var<-all_data_1[,c(1,144, 118:129,142)]
length(colnames(j_acc_var)) # 15



a<-j_acc_var[2:15]
colnames(a)
class(a$FRAUD_1_NONFRAUD_0)

a$FRAUD_1_NONFRAUD_0 <- as.factor(a$FRAUD_1_NONFRAUD_0_STR)
model <- glm( FRAUD_1_NONFRAUD_0 ~., data = a, family = binomial)
summary(model)
anova(model, test="Chisq")

# Summary bzw. Anova Ergebnis in Txt-File speichern
capture.output(summary(model), file = "jones.txt")
capture.output((anova(model, test="Chisq")), file = "anova_jones.txt")


# backward, forward, both: für j_acc_var_new
model <- glm( FRAUD_1_NONFRAUD_0 ~., data = a, family = binomial) %>%
  stepAIC(trace = FALSE, direction="forward")

summary(model)

model <- glm(FRAUD_1_NONFRAUD_0_STR ~., data = wiley_ratios_only, family = binomial) %>%
  stepAIC(trace = FALSE, direction="both")

summary(model)



# Name der Variable
datafile<-j_acc_var

colNames<-names(datafile)[3:15] # Spalte die ausgewertet werden
for(i in colNames){
  plt <- ggplot(datafile, aes_string(x=i, fill="FRAUD_1_NONFRAUD_0_STR")) +
    geom_histogram() + stat_bin(bins=50) 
  plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
  #print(plt)
  
  ggsave(plt, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}

# bissi auffällig:
# J_var1_3, J, VJ, 4 - je höher der Wert desto eher Fraud
# J_var2: 3 und VJ
# J_var3_: keine auffälligen Werte
# y_pred_Jones: gewisse Abweichung - Werte viel größer/kleiner 0 - ja
# 

########################################################################
# Modified-Jones-Modell:
########################################################################

# Variablen für Total_Accruals berechnen - Modified Jones Modell
# Berechnung für ACC_CF oder ACC_BC
# mit Prediction für Modified-Jones-Modell

mj_acc_var<-all_data_1[,c(1,7,130:133,143)]
length(colnames(mj_acc_var)) # 7

a<-mj_acc_var[2:7]
colnames(a)
class(a$FRAUD_1_NONFRAUD_0_STR)
a$FRAUD_1_NONFRAUD_0_STR <- as.factor(a$FRAUD_1_NONFRAUD_0_STR)
model <- glm( FRAUD_1_NONFRAUD_0_STR ~., data = a, family = binomial)
summary(model)
anova(model, test="Chisq")

# Summary bzw. Anova Ergebnis in Txt-File speichern
capture.output(summary(model), file = "modified_jones.txt")
capture.output((anova(model, test="Chisq")), file = "anova_modified_jones.txt")




# Name der Variable
datafile<-mj_acc_var

colNames<-names(datafile)[3:7] # Spalte die ausgewertet werden
for(i in colNames){
  plt <- ggplot(datafile, aes_string(x=i, fill="FRAUD_1_NONFRAUD_0_STR")) +
    geom_histogram() + stat_bin(bins=50) 
  plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
  #print(plt)
  
  ggsave(plt, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}

# Auffällige Abweichungen:
# MJ_var2_3: Werte über 0 eher FRAUD 
# y_pred_MJ: auch hier Werte größer/kleiner 0 - helfen















########################################################################
# Gesamtauswertung aller Variablen:
################################################

# Test auf Signifikanz für alle Variablen des Datensatzes
# mittels Mann-Whitney-U bzw. Wilcoxon-Test

all_data_2[,c(1)]
all_data_2$DSRI
dim(all_data_2) # 87 138


p_wilcox_results<-c()
for (i in 1:(dim(all_data_2)[2]-1)) {
  # Wilcox-Test mit allen Variablen des Datensatzes
  # try: - Errorhandling - damit bei Fehler der Loop nicht unterbrochen wird
  # durch [[3]] erhalte nur den p-value ohne String
  pvalue<-try(wilcox.test(all_data_2[,c(i)] ~ all_data_2$FRAUD_1_NONFRAUD_0_STR, exact=FALSE)[[3]], 
      silent=TRUE)
  p_wilcox_results<-c(p_wilcox_results, pvalue)
  
  
}


# Wilcox-Test nur von Wiley-Kennzahlen
#######################
p_wilcox_results<-c()
for (i in 1:(dim(all_data_2)[2]-1)) {
  # Wilcox-Test mit allen Variablen des Datensatzes
  # try: - Errorhandling - damit bei Fehler der Loop nicht unterbrochen wird
  # durch [[3]] erhalte nur den p-value ohne String
  pvalue<-try(wilcox.test(all_data_2[,c(i)] ~ all_data_2$FRAUD_1_NONFRAUD_0_STR, exact=FALSE)[[3]], 
              silent=TRUE)
  p_wilcox_results<-c(p_wilcox_results, pvalue)
  
  
}





#####################





length(p_wilcox_results) #137 Variablen
# loop hat funktioniert 
# Jetzt analysieren, welche Variablen signifikant waren


Col_names<-colnames(all_data_2)[1:137]

df_wilcox<-data.frame(Col_names, p_wilcox_results)

with(df_wilcox, df_wilcox$p_wilcox_results<0.05)
# es gibt Werte die diese Bedingung erfüllen - zumindestens etwas

df_wilcox[df_wilcox$p_wilcox_results <0.05,]

df_wilcox$p_wilcox_results



###################################################################
#################################################################
# Durchsicht auf Korrelationen unter den Variablen:
#####################################################################


library(MASS)

# Fehlermeldung - algorithm did not converge
# Grund hohe Korrelation unter den Kennzahlen
wiley_cor<-cor(wiley_kz_var[,c(2:67)])



# https://towardsdatascience.com/how-to-create-a-correlation-matrix-with-too-many-variables-309cc0c0a57
corr_simple <- function(data=wiley_kz_var[,c(2:67)],sig=0.8){
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  #run a correlation and drop the insignificant ones
  corr <- cor(df_cor)
  #prepare to drop duplicates and correlations of 1     
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  #drop perfect correlations
  corr[corr == 1] <- NA 
  #turn into a 3-column table
  corr <- as.data.frame(as.table(corr))
  #remove the NA values from above 
  corr <- na.omit(corr) 
  #select significant values  
  corr <- subset(corr, abs(Freq) > sig) 
  #sort by highest correlation
  corr <- corr[order(-abs(corr$Freq)),] 
  #print table
  print(corr)
  #turn corr back into matrix in order to plot with corrplot
  mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  
  #plot correlations visually
  corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ")
}
a<-corr_simple()
a
























############################################################################################
# Datenauswertung von Kennzahlen:
###############################################################################################


# Hinauswerfen von Kennzahlen in wiley_kz die hohe Korrelationen haben

wiley_kz_var

wiley_kz_var<-subset(wiley_kz_var, select=-c(CR_VJ, CR_3, CR_4, QR_J, QR_VJ, 
                                             QR_3, QR_4, QR_CHANGE, QR_CHANGE_34,
                                             INV_J, INV_VJ, INV_3, INV_4, 
                                             AV_REC_J, AV_REC_VJ, AV_REC_3, AV_REC_4,
                                             T_LIAB_J, T_LIAB_VJ, T_LIAB_3, T_LIAB_4,
                                             AV_ASSET_J, AV_ASSET_VJ, AV_ASSET_3, 
                                             AV_ASSET_4))

colnames(wiley_kz_var)
dim(wiley_kz_var) #42 Variablen
is.factor(wiley_kz_var$FRAUD_1_NONFRAUD_0_STR)

# es macht eigentlich überhaupt keinen Sinn nur die Verhältniskennzahlen innerhalb
# eines Jahres in die Auswertung einzubringen
# daher nur Verhältniskennzahlen, die die Veränderung von Jahr X zu X-1 bzw. X-1 zu X-2 veranschaulichen
# 

wiley_ratios_only<-wiley_kz_var[,c("FRAUD_1_NONFRAUD_0_STR", "CR_CHANGE", 
                                   "CR_CHANGE_34", "INV_CHANGE", "INV_CHANGE_34",
                                   "INV_TR_CHANGE", "INV_TR_CHANGE_34", 
                                   "AV_REC_CHANGE", "AV_REC_CHANGE_34",
                                   "REC_TR_CHANGE", "REC_TR_CHANGE_34", 
                                   "T_LIAB_CHANGE", "T_LIAB_CHANGE_34",
                                   "DER_CHANGE", "DER_CHANGE_34", 
                                   "PMR_CHANGE", "PMR_CHANGE_34",
                                   "AV_ASSET_CHANGE", "AV_ASSET_CHANGE_34",
                                   "ASSET_TR_CHANGE", "ASSET_TR_CHANGE_34")]

# wiley_ratios_only
dim(wiley_ratios_only) # 19 Kennzahlen/Variablen mit FRAUD_1

# stepAIC - Backwards-Selection by Default
# Fit the model
model <- glm(FRAUD_1_NONFRAUD_0_STR ~., data = wiley_ratios_only, family = binomial) %>%
  stepAIC(trace = FALSE)

summary(model)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)   
#(Intercept)       2.5058     2.2549   1.111  0.26645   
#CR_CHANGE        -1.4446     1.0525  -1.373  0.16989   
#CR_CHANGE_34      0.7836     0.5200   1.507  0.13178   
#AV_REC_CHANGE     3.3847     1.3065   2.591  0.00958 **
#  REC_TR_CHANGE     2.3910     1.5067   1.587  0.11253   
#T_LIAB_CHANGE    -2.4779     1.0279  -2.411  0.01593 * 
#  PMR_CHANGE       -0.2499     0.1201  -2.081  0.03745 * 
#  PMR_CHANGE_34     0.1979     0.1713   1.155  0.24798   
#ASSET_TR_CHANGE  -5.8971     2.6873  -2.194  0.02820 * 

# demnach: AV_REC_CHANGE, T_LIAB_CHANGE, PMR_CHANGE, ASSET_TR_CHANGE


#####################################


# Forward-Selection
# Fit the model
model <- glm(FRAUD_1_NONFRAUD_0_STR ~., data = wiley_ratios_only, family = binomial) %>%
  stepAIC(trace = FALSE, direction="forward")

summary(model)
# relevante Variablen:
# T_LIAB_CHANGE       -3.7245     1.6712  -2.229   0.0258 *
# PMR_CHANGE          -0.2776     0.1336  -2.078   0.0377 *
# ASSET_TR_CHANGE     -9.8085     4.6685  -2.101   0.0356 *


##########################


### both direction + Kombination von forward und backward selection
# direction="both"
model <- glm(FRAUD_1_NONFRAUD_0_STR ~., data = wiley_ratios_only, family = binomial) %>%
  stepAIC(trace = FALSE, direction="both")

summary(model)
# AV_REC_CHANGE     3.3847     1.3065   2.591  0.00958 **
# T_LIAB_CHANGE    -2.4779     1.0279  -2.411  0.01593 * 
# PMR_CHANGE       -0.2499     0.1201  -2.081  0.03745 *
# ASSET_TR_CHANGE  -5.8971     2.6873  -2.194  0.02820 *



wiley_sig<-wiley_ratios_only[,c("FRAUD_1_NONFRAUD_0_STR", "AV_REC_CHANGE", 
                                "T_LIAB_CHANGE", "PMR_CHANGE", "ASSET_TR_CHANGE")]

wiley_sig<-all_data_1[,c("FRAUD_1_NONFRAUD_0", "AV_REC_CHANGE", 
                                "T_LIAB_CHANGE", "PMR_CHANGE", "ASSET_TR_CHANGE")]

wiley_sig$FRAUD_1_NONFRAUD_0<-as.factor(wiley_sig$FRAUD_1_NONFRAUD_0)


model <- glm(FRAUD_1_NONFRAUD_0 ~., data = wiley_sig, family = binomial)
summary(model)

#Estimate Std. Error z value Pr(>|z|)  
#(Intercept)       1.7705     1.8734   0.945   0.3446  
#AV_REC_CHANGE     1.3838     0.6845   2.022   0.0432 *
#  T_LIAB_CHANGE    -0.8389     0.5657  -1.483   0.1381  
#PMR_CHANGE       -0.2316     0.1133  -2.044   0.0409 *
#  ASSET_TR_CHANGE  -2.9103     1.7821  -1.633   0.1025 

model <- glm(FRAUD_1_NONFRAUD_0 ~  PMR_CHANGE, 
             data = wiley_sig, family = binomial)
summary(model)

library(caret)
library(InformationValue)

# logistische Regression von Kennzahlen 258
train.data$PMR_CHANGE
set.seed(123)
sens_results<-c()
spec_results<-c()
results_test<-c()
for (i in 1:1000) {
  
  training.samples <- wiley_sig$FRAUD_1_NONFRAUD_0 %>% 
    createDataPartition(p = 0.7, list = FALSE)
  # Zuweisung der Indexwerte zu Train und Test
  train.data  <- wiley_sig[training.samples, ]
  test.data <- wiley_sig[-training.samples, ]
  
  # Fit the model
  #model <- glm( FRAUD_1_NONFRAUD_0 ~., 
   #             data = train.data, family = binomial)
  
  model <- glm( FRAUD_1_NONFRAUD_0 ~PMR_CHANGE,
                data = train.data, family = binomial)
  
  #predicted_train <- predict(model, train.data, type="response")
  predicted_test <- predict(model, test.data, type="response")
  
  #train_optimal<-optimalCutoff(training_set$FRAUD, predicted)
  #test_optimal<-optimalCutoff(test.data$FRAUD, predicted_test)
  
  # Optimale Trennung laut Programm:
  predicted_result<-ifelse(predicted_test > 0.325, 1, 0)
  
  #predicted_result<-ifelse(predicted_test > 0.2, 1, 0)
  
  result<-mean(predicted_result == test.data$FRAUD)
  results_test<-c(results_test, result)
  
  #results_train<-c(results_train, train_optimal)
  # results_test <- c(results_test, test_optimal)
  
  # Ermittlung des Spezifität und Sensitivität
  actual_values<-test.data$FRAUD
  conf_matrix<-table(predicted_result,actual_values)
  sens<-conf_matrix[4]/(conf_matrix[3]+conf_matrix[4])
  spec<-conf_matrix[1]/(conf_matrix[1]+conf_matrix[2])
  
  # Einzelergebnisse in die Vektoren einfügen
  sens_results<-c(sens_results, sens)
  spec_results<-c(spec_results, spec)
  
  
}
mean(results_test)
sens_results[is.na(sens_results)]<-0
spec_results[is.na(spec_results)]<-0

mean(sens_results)
mean(spec_results)

summary(model)

###########################################

wiley_sig<-wiley_ratios_only[,c("FRAUD_1_NONFRAUD_0_STR", "AV_REC_CHANGE", "PMR_CHANGE")]
model <- glm(FRAUD_1_NONFRAUD_0_STR ~., data = wiley_sig, family = binomial)
summary(model)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)  
#(Intercept)    -1.4755     0.6382  -2.312   0.0208 *
#  AV_REC_CHANGE   0.7051     0.4928   1.431   0.1525  
#PMR_CHANGE     -0.2100     0.1034  -2.032   0.0422 *

###############################################



wiley_sig<-wiley_ratios_only[,c("FRAUD_1_NONFRAUD_0_STR", "PMR_CHANGE")]
model <- glm(FRAUD_1_NONFRAUD_0_STR ~., data = wiley_sig, family = binomial)
summary(model)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)  
#(Intercept)  -0.6172     0.2442  -2.527   0.0115 *
#  PMR_CHANGE   -0.1993     0.1022  -1.950   0.0512 .



#############
# plotten der vier signifikanten Ergebnisse von Wiley-Kennzahlen
#


plot_df <- ggplot(wiley_ratios_only, aes(x = AV_REC_CHANGE, y = PMR_CHANGE, colour = FRAUD_1_NONFRAUD_0_STR)) + geom_point()
plot_df 

plot_df <- ggplot(wiley_ratios_only, aes(x = AV_REC_CHANGE, y = T_LIAB_CHANGE, colour = FRAUD_1_NONFRAUD_0_STR)) + geom_point()
plot_df 

plot_df <- plot_df + geom_bar(position = method_df, stat = "identity") + coord_flip()
plot_df


library(plot3D)
x<-wiley_ratios_only$AV_REC_CHANGE
y<-wiley_ratios_only$T_LIAB_CHANGE
z<-wiley_ratios_only$PMR_CHANGE
color_type<-as.numeric(wiley_ratios_only$FRAUD_1_NONFRAUD_0_STR)

# , col = NULL, add = FALSE
scatter3D(x, y, z, colvar = color_type)

library(plotly)
fig_neu <- plot_ly(wiley_ratios_only, x = ~AV_REC_CHANGE, y = ~T_LIAB_CHANGE, z = ~PMR_CHANGE,
                   color = ~FRAUD_1_NONFRAUD_0_STR)
fig_neu
fig_neu <- plot_ly(wiley_ratios_only, x = ~AV_REC_CHANGE, y = ~PMR_CHANGE, z = ~ASSET_TR_CHANGE,
                   color = ~FRAUD_1_NONFRAUD_0_STR)

fig_neu
fig_neu <- plot_ly(wiley_ratios_only, x = ~T_LIAB_CHANGE, y = ~PMR_CHANGE, z = ~ASSET_TR_CHANGE,
                   color = ~FRAUD_1_NONFRAUD_0_STR)
fig_neu
fig_neu <- plot_ly(wiley_ratios_only, x = ~AV_REC_CHANGE, y = ~T_LIAB_CHANGE, z = ~ASSET_TR_CHANGE,
                   color = ~FRAUD_1_NONFRAUD_0_STR)
fig_neu

########################################################

#########################################################################################
all_data_1$FRAUD_1_NONFRAUD_0

colnames(all_data_2)

# # alle unnötigen Variablen von Wiley aus Gesamtdatensatz entfernen:
# all_data_4<-subset(all_data_2, select=-c(CR_VJ, CR_3, CR_4, QR_J, QR_VJ, 
#                                              QR_3, QR_4, QR_CHANGE, QR_CHANGE_34,
#                                              INV_J, INV_VJ, INV_3, INV_4, 
#                                              AV_REC_J, AV_REC_VJ, AV_REC_3, AV_REC_4,
#                                              T_LIAB_J, T_LIAB_VJ, T_LIAB_3, T_LIAB_4,
#                                              AV_ASSET_J, AV_ASSET_VJ, AV_ASSET_3, 
#                                              AV_ASSET_4,
#                                          CR_J, INV_TR_J, INV_TR_VJ, REC_TR_3, REC_TR_4,
#                                          DER_J, DER_VJ, DER_3, DER_4, PMR_J, PMR_VJ,
#                                          PMR_3, PMR_4, ASSET_TR_J, ASSET_TR_VJ, ASSET_TR_3, 
#                                          ASSET_TR_4, REC_TR_J, REC_TR_VJ,
#                                          INV_TR_3, INV_TR_4))
# 
# 
# dim(all_data_4) # 87 91
# 
# ########################
# # Durchsicht zeigt, dass sich immernoch Idiotenvariablen im Datensatz befinden
# # J_var1_J --> 1 / Assets_Jahr - komplett sinnlos 
# 
# 
# 
# # aber neue Variablen daraus erzeugbar
# # J_change_var1 = J_var1_J/J_var1_VJ etc....
# all_data_4$J_change_var1<-all_data_4$J_var1_J/all_data_4$J_var1_VJ
# all_data_4$J_change_var2<-all_data_4$J_var2_J/all_data_4$J_var2_VJ
# all_data_4$J_change_var3<-all_data_4$J_var3_J/all_data_4$J_var3_VJ
# 
# all_data_4$J_change_var1_34<-all_data_4$J_var1_3/all_data_4$J_var1_4
# all_data_4$J_change_var2_34<-all_data_4$J_var2_3/all_data_4$J_var2_4
# all_data_4$J_change_var3_34<-all_data_4$J_var3_3/all_data_4$J_var3_4
# 
# all_data_4$MJ_change_var2<-all_data_4$MJ_var2_J/all_data_4$MJ_var2_VJ
# all_data_4$MJ_change_var2_34<-all_data_4$MJ_var2_3/all_data_4$MJ_var2_4
# 
# # neue Variablen - Verhältnisse von Jones und Modified Jones
# # all_data_5 
# 
# 
# all_data_5 <- all_data_4
# colnames(all_data_5)
# 
# # alle Einzelvariablen die nur ein Jahr betreffen aus Datensatz
# # herausgelöscht 
# # Verhältniskennzahlen, die nur ein Jahr betreffen können nicht relevant sein
# 
# all_data_5<-subset(all_data_4, select=-c(J_var1_J, J_var1_VJ, J_var1_3, J_var1_4,
#                                          J_var2_J, J_var2_VJ, J_var2_3, J_var2_4,
#                                          J_var3_J, J_var3_VJ, J_var3_3, J_var3_4,
#                                          MJ_var2_J, MJ_var2_VJ, MJ_var2_3, MJ_var2_4))
# 
# 
# dim(all_data_5) # 87 83
# 
# # Verhältniskennzahlen z-Score
# all_data_5$zscore_1_change<-all_data_5$ZSCORE_1_J/all_data_5$ZSCORE_1_VJ
# all_data_5$zscore_2_change<-all_data_5$ZSCORE_2_J/all_data_5$ZSCORE_2_VJ
# all_data_5$zscore_3_change<-all_data_5$ZSCORE_3_J/all_data_5$ZSCORE_3_VJ
# all_data_5$zscore_4_change<-all_data_5$ZSCORE_4_J/all_data_5$ZSCORE_4_VJ
# all_data_5$zscore_5_change<-all_data_5$ZSCORE_5_J/all_data_5$ZSCORE_5_VJ
# 
# colnames(all_data_5)
# 
# all_data_5<-subset(all_data_5, select=-c(ZSCORE_1_J, ZSCORE_1_VJ, ZSCORE_2_J, ZSCORE_2_VJ,
#                                          ZSCORE_3_J, ZSCORE_3_VJ, ZSCORE_4_J, ZSCORE_4_VJ,
#                                          ZSCORE_5_J, ZSCORE_5_VJ))
# 



# colnames(all_data_5)
# dim(all_data_5) # 87 78
# # Nach Reduktion und Veränderung hinzugefügt
# # daher nun weiterhin 87 Zeilen, 78 Spalten = Features
# 
# colnames(all_data_4)
# all_data_5$FRAUD<-as.factor(all_data_1$FRAUD_1_NONFRAUD_0)
# dim(all_data_5) # 87 79
# # 87 Zeilen, 79 Spalten
# 
# # NAs entfernen
# all_data_5[is.na(all_data_5)] <- 0





#################################################################################
# Auswertung Positiver negativer Cashflow:
#################################################################################


### positiver Cashflow /negativer Cashflow
# mittels Chi-Square prüfen
# prüfen ob zwei Kategorische Daten eine Abhängigkeit zueinander haben


library(FSelector)

all_data_v1$POS_NEG_O_CF_J
all_data_v1$POS_NEG_O_CF_VJ
all_data_v1$POS_NEG_O_CF_4

pos_neg_CF<-all_data_1[,c(7, 112:114)]
class(pos_neg_CF$FRAUD_1_NONFRAUD_0)

table(pos_neg_CF$FRAUD_1_NONFRAUD_0 , pos_neg_CF$POS_NEG_O_CF_J )
#    0  1
# 0  9 50
# 1 11 17

# chi-square-Tabelle:
# https://datatab.de/tutorial/tabelle-chi-quadrat
# Ergebnis ist korrekt

chisq.test(pos_neg_CF$FRAUD_1_NONFRAUD_0 , pos_neg_CF$POS_NEG_O_CF_J, correct = FALSE)

# X-squared = 6.1942, df = 1, p-value = 0.01282
# da p-Value unter 0.05 - kann von gewisser Abhängigkeit von beiden Variablen
# sprechen

chisq.test(pos_neg_CF$FRAUD_1_NONFRAUD_0 , pos_neg_CF$POS_NEG_O_CF_VJ, correct = FALSE)

# X-squared = 0.013738, df = 1, p-value = 0.9067
# kein sign. Ergebnis - 

chisq.test(pos_neg_CF$FRAUD_1_NONFRAUD_0 , pos_neg_CF$POS_NEG_O_CF_4, correct = FALSE)

# X-squared = 1.9544, df = 1, p-value = 0.1621
# kein sign. Ergebnis - 

pos_neg_CF$FRAUD_1_NONFRAUD_0 <- as.factor(pos_neg_CF$FRAUD_1_NONFRAUD_0)

# Rowsum der 3 Jahre Jahr + Vorjahr + Vor-Vorjahr
pos_neg_CF$POS_NEG_3Y<-rowSums(pos_neg_CF[,c(2:4)])
chisq.test(pos_neg_CF$FRAUD_1_NONFRAUD_0 , as.factor(pos_neg_CF$POS_NEG_3Y), correct = FALSE)
# X-squared = 5.0243, df = 3, p-value = 0.17

# Rowsum von 2 Jahren: Jahr + Vorjahr
pos_neg_CF$POS_NEG_2Y<-rowSums(pos_neg_CF[,c(2:3)])
chisq.test(pos_neg_CF$FRAUD_1_NONFRAUD_0 , as.factor(pos_neg_CF$POS_NEG_2Y), correct = FALSE)
# X-squared = 5.1496, df = 2, p-value = 0.07617

# Rowsum von 2 Jahren: Vorjahr + Vor-Vorjahr
pos_neg_CF$POS_NEG_2BY<-rowSums(pos_neg_CF[,c(3:4)])
chisq.test(pos_neg_CF$FRAUD_1_NONFRAUD_0 , as.factor(pos_neg_CF$POS_NEG_2BY), correct = FALSE)
# X-squared = 1.0251, df = 2, p-value = 0.599

colnames(pos_neg_CF)

?chi.squared

weight = chi.squared(pos_neg_CF$FRAUD_1_NONFRAUD_0~pos_neg_CF$POS_NEG_O_CF_J)
print(weight)
weight = chi.squared(pos_neg_CF$FRAUD_1_NONFRAUD_0~pos_neg_CF$POS_NEG_O_CF_VJ)
print(weight)
weight = chi.squared(pos_neg_CF$FRAUD_1_NONFRAUD_0~pos_neg_CF$POS_NEG_O_CF_4)
print(weight)

cutoff.k(weight, 2)

## keinerlei Treffer - Daten sind komplett sinnlos

## Versuch mittels Random-Forest
weight = random.forest.importance(FRAUD_1_NONFRAUD_0~., pos_neg_CF)
print(weight)

#attr_importance
#POS_NEG_O_CF_J        22.117869
#POS_NEG_O_CF_VJ       12.165556
#POS_NEG_O_CF_4         3.559801
#POS_NEG_3Y            12.203380
#POS_NEG_2Y            16.582149
#POS_NEG_2BY            7.735265

?random.forest.importance




###############################################################################
############################################################################
# Analyse Jones und Modified Jones Modell
#################################################################################
#################################################################################

dim(all_data_5)
colnames(all_data_5)


all_data_5$ACCR_CF_J
all_data_5$ACCR_BS_J

bs_v_pred_j_abs<-abs(all_data_5$ACCR_BS_J) - abs(all_data_5$y_pred_Jones)
bs_v_pred_j<-(all_data_5$ACCR_BS_J) - (all_data_5$y_pred_Jones)
v_fraud<-all_data_5$FRAUD

df_5<-data.frame(v_fraud,bs_v_pred_j, bs_v_pred_j_abs)
df_5 %>% group_by(v_fraud) %>%
  summarise(Mittelwert = mean(bs_v_pred_j), Median = median(bs_v_pred_j),
            M_abs = mean(bs_v_pred_j_abs), Median_abs = median(bs_v_pred_j_abs))


colnames(all_data_5)
j_acc_var <- all_data_5[,c("FRAUD", "ACCR_BS_J", "ACCR_BS_VJ",
                           "ACCR_BS_3", "ACCR_BS_4", 
                           "ACCR_CF_J", "ACCR_CF_VJ", "ACCR_CF_3",
                           "ACCR_CF_4", "y_pred_Jones", 
                           "J_change_var1", "J_change_var2",
                           "J_change_var3", "J_change_var1_34", 
                           "J_change_var2_34", "J_change_var3_34")]

colnames(j_acc_var)
j_acc_var$change_ACCR_CF<-j_acc_var$ACCR_CF_J/j_acc_var$ACCR_CF_VJ
j_acc_var$change_ACCR_CF_34<-j_acc_var$ACCR_CF_3/j_acc_var$ACCR_CF_4

colnames(j_acc_var)
dim(j_acc_var) # 87 18

# Lösung: alle infinite Ergebnisse in NA umwandeln
# NA in 0 umwandeln
is.na(j_acc_var)<-sapply(j_acc_var, is.infinite)
j_acc_var[is.na(j_acc_var)]<-0

# Abweichungen von Predictited zu IST
j_acc_var$ABW_BS<-j_acc_var$ACCR_BS_J- j_acc_var$y_pred_Jones
j_acc_var$ABW_CF<-j_acc_var$ACCR_CF_J - j_acc_var$y_pred_Jones

j_acc_var$ABW_BS_res_abs<-abs(j_acc_var$ACCR_BS_J- j_acc_var$y_pred_Jones)
j_acc_var$ABW_CF_res_abs<-abs(j_acc_var$ACCR_CF_J - j_acc_var$y_pred_Jones)

j_acc_var$ABW_BS_ABS <- abs(j_acc_var$ACCR_BS_J) - abs(j_acc_var$y_pred_Jones)
j_acc_var$ABW_CF_ABS <- abs(j_acc_var$ACCR_CF_J) - abs(j_acc_var$y_pred_Jones)

j_acc_var %>% group_by(FRAUD) %>%
  summarise(Count = n(),  
            Median = median(change_ACCR_CF),
            Mean = mean(change_ACCR_CF), 
            STD = sd(change_ACCR_CF),
            Max = max(change_ACCR_CF),
            Min = min(change_ACCR_CF))


j_acc_var %>% group_by(FRAUD) %>%
  summarise(Count = n(),  
            Median = median(change_ACCR_CF_34),
            Mean = mean(change_ACCR_CF_34), 
            STD = sd(change_ACCR_CF_34),
            Max = max(change_ACCR_CF_34),
            Min = min(change_ACCR_CF_34))



j_acc_var %>% group_by(FRAUD) %>%
  summarise(Count = n(),  
            Median = median(y_pred_Jones),
            Mean = mean(y_pred_Jones), 
            STD = sd(y_pred_Jones),
            Max = max(y_pred_Jones),
            Min = min(y_pred_Jones))

j_acc_var %>% group_by(FRAUD) %>%
  summarise(Count = n(),  
            Median = median(ABW_BS_res_abs),
            Mean = mean(ABW_BS_res_abs), 
            STD = sd(ABW_BS_res_abs),
            Max = max(ABW_BS_res_abs),
            Min = min(ABW_BS_res_abs))


j_acc_var %>% group_by(FRAUD) %>%
  summarise(Count = n(),  
            Median = median(ABW_CF_res_abs),
            Mean = mean(ABW_CF_res_abs), 
            STD = sd(ABW_CF_res_abs),
            Max = max(ABW_CF_res_abs),
            Min = min(ABW_CF_res_abs))


################################################

# Auswertung auf Relevanz der Variablen
library(caret)

model <- glm( FRAUD ~., data = j_acc_var, family = binomial) %>%
  stepAIC(trace = FALSE, direction="both")
summary(model)
summary(model)$coef

# ACCR_BS_VJ       -27.64947   12.85608  -2.151   0.0315 *
# ACCR_BS_3         26.23355   12.59397   2.083   0.0372 *
# ABW_CF_res_abs     7.19325    3.59631   2.000   0.0455 *

# y_pred_Jones       1.25736    1.50138   0.837   0.4023 
# ACCR_BS_VJ       -21.93967   12.69039  -1.729   0.0838 .
# J_change_var1_34  -4.45795    2.07280  -2.151   0.0315 *

model <- glm( FRAUD ~ACCR_BS_J + ACCR_BS_VJ +
                ACCR_BS_3 + J_change_var1_34 +
                ABW_CF_res_abs, data = j_acc_var, family = binomial) %>%
  stepAIC(trace = FALSE, direction="both")
summary(model)


model <- glm( FRAUD ~ ABW_CF_res_abs, data = j_acc_var, family = binomial)

summary(model)


# Analyse der Kennzahlen: ABW_CF_res_abs und ABW_BS_res_abw
# auf Trefferquote für Fraud, vollidiot2

j_acc_var$ABW_CF_res_abs

library("ggpubr")
ggboxplot(j_acc_var, x = "FRAUD", y = "ABW_CF_res_abs", 
          color = "FRAUD", palette = c("#00AFBB", "#E7B800"),
          ylab = "Abs. Abweichung", xlab = "FRAUD vs. NON FRAUD")

# entfernen von Ausreißern

data <- j_acc_var
dim(data)
## [1] 87 24

quartiles <- quantile(data$ABW_CF_res_abs, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data$ABW_CF_res_abs)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(data, data$ABW_CF_res_abs > Lower & data$ABW_CF_res_abs < Upper)

dim(data_no_outlier)

ggboxplot(data_no_outlier, x = "FRAUD", y = "ABW_CF_res_abs", 
          color = "FRAUD", palette = c("#00AFBB", "#E7B800"),
          ylab = "Abs. Abweichung", xlab = "FRAUD vs. NON FRAUD")

quantile(with(data_no_outlier, ABW_CF_res_abs[FRAUD == 1]))
quantile(with(data_no_outlier, ABW_CF_res_abs[FRAUD == 0]))
quantile(data_no_outlier$ABW_BS_res_abs)
head(j_acc_var, 5)


#################################################################################
# Analyse Modified-Jones-Modell:
#################################################################################
#################################################################################

mj_acc_var <- all_data_5[,c("FRAUD", "ACCR_BS_J", "ACCR_BS_VJ",
                           "ACCR_BS_3", "ACCR_BS_4", 
                           "ACCR_CF_J", "ACCR_CF_VJ", "ACCR_CF_3",
                           "ACCR_CF_4", "y_pred_MJ", 
                           "J_change_var1", "MJ_change_var2",
                           "J_change_var3", "J_change_var1_34", 
                           "MJ_change_var2_34", "J_change_var3_34")]

mj_acc_var$y_pred_MJ <- all_data_1$y_pred_MJ

# Lösung: alle infinite Ergebnisse in NA umwandeln
# NA in 0 umwandeln
is.na(mj_acc_var)<-sapply(mj_acc_var, is.infinite)
mj_acc_var[is.na(mj_acc_var)]<-0

# Abweichung von IST (ACCR_BS oder _CF) zu 

mj_acc_var$ABW_BS<-mj_acc_var$ACCR_BS_J- mj_acc_var$y_pred_MJ
mj_acc_var$ABW_CF<-mj_acc_var$ACCR_CF_J - mj_acc_var$y_pred_MJ

mj_acc_var$ABW_BS_res_abs<-abs(mj_acc_var$ACCR_BS_J- mj_acc_var$y_pred_MJ)
mj_acc_var$ABW_CF_res_abs<-abs(mj_acc_var$ACCR_CF_J - mj_acc_var$y_pred_MJ)

mj_acc_var$ABW_BS_ABS <- abs(mj_acc_var$ACCR_BS_J) - abs(mj_acc_var$y_pred_MJ)
mj_acc_var$ABW_CF_ABS <- abs(mj_acc_var$ACCR_CF_J) - abs(mj_acc_var$y_pred_MJ)


# WICHTIG - für Masterarbeit wurden folgende Abweichungskennzahlen verwendet
# ABW_BS_res_abs bzw. ABW_CF_res_abs

library("ggpubr")
ggboxplot(mj_acc_var, x = "FRAUD", y = "ABW_CF_res_abs", 
          color = "FRAUD", palette = c("#00AFBB", "#E7B800"),
          ylab = "Abs. Abweichung", xlab = "FRAUD vs. NON FRAUD")

# entfernen von Ausreißern

data <- mj_acc_var
dim(data)
## [1] 87 25

quartiles <- quantile(data$ABW_CF_res_abs, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data$ABW_CF_res_abs)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(data, data$ABW_CF_res_abs > Lower & data$ABW_CF_res_abs < Upper)

dim(data_no_outlier)

ggboxplot(data_no_outlier, x = "FRAUD", y = "ABW_CF_res_abs", 
          color = "FRAUD", palette = c("#00AFBB", "#E7B800"),
          ylab = "Abs. Abweichung", xlab = "FRAUD vs. NON FRAUD")

quantile(with(data_no_outlier, ABW_CF_res_abs[FRAUD == 1]))
quantile(with(data_no_outlier, ABW_CF_res_abs[FRAUD == 0]))
quantile(data_no_outlier$ABW_BS_res_abs)
head(j_acc_var, 5)


#######################################################################
######################################################################

# Analyse der Abweichungen und versuch zur Trennung


a<-round(j_acc_var$ABW_CF_res_abs, 4)*100
a[1:28]
a[28:87]


jones_cf_abw<-ifelse(a > 15, 1, 0)
table(j_acc_var$FRAUD, jones_cf_abw)

# 10% Trennung
jones_cf_abw<-ifelse(a > 6, 1, 0)
table(j_acc_var$FRAUD, jones_cf_abw)


jones_cf_abw<-ifelse(a > 2, 1, 0)
table(j_acc_var$FRAUD, jones_cf_abw)


model <- glm( FRAUD ~., data = j_acc_var, family = binomial) %>%
  stepAIC(trace = FALSE, direction="both")

summary(model)

a<-round(mj_acc_var$ABW_CF_res_abs, 4)*100
a[1:28]
a[28:87]

mjones_cf_abw<-ifelse(a > 3, 1, 0)
table(mj_acc_var$FRAUD, mjones_cf_abw)

model <- glm( FRAUD ~., data = mj_acc_var, family = binomial) %>%
  stepAIC(trace = FALSE, direction="forward")

summary(model)



###########################################################
# Deskriptive Statistik
# jede Variable Count_, Median_, Mean_, Max_, Min_


# Auswertung ACCR_BS_J
mj_acc_var %>% group_by(FRAUD) %>%
  summarise(Count = n(),  
            Median = median(ACCR_BS_J),
            Mean = mean(ACCR_BS_J), 
            STD = sd(ACCR_BS_J),
            Max = max(ACCR_BS_J),
            Min = min(ACCR_BS_J))

# Auswertung ACCR_CF_J
mj_acc_var %>% group_by(FRAUD) %>%
  summarise(Count = n(),  
            Median = median(ACCR_CF_J),
            Mean = mean(ACCR_CF_J),
            STD = sd(ACCR_CF_J),
            Max = max(ACCR_CF_J),
            Min = min(ACCR_CF_J))

# Auswertung y_pred_MJ
mj_acc_var %>% group_by(FRAUD) %>%
  summarise(Count = n(),  
            Median = median(y_pred_MJ),
            Mean = mean(y_pred_MJ), 
            STD = sd(y_pred_MJ),
            Max = max(y_pred_MJ),
            Min = min(y_pred_MJ))

# Auswertung ABW_BS_res_abs
mj_acc_var %>% group_by(FRAUD) %>%
  summarise(Count = n(),  
            Median = median(ABW_BS_res_abs),
            Mean = mean(ABW_BS_res_abs), 
            STD = sd(ABW_BS_res_abs),
            Max = max(ABW_BS_res_abs),
            Min = min(ABW_BS_res_abs))


# Auswertung ABW_CF_res_abs
mj_acc_var %>% group_by(FRAUD) %>%
  summarise(Count = n(),  
            Median = median(ABW_CF_res_abs),
            Mean = mean(ABW_CF_res_abs), 
            STD = sd(ABW_CF_res_abs),
            Max = max(ABW_CF_res_abs),
            Min = min(ABW_CF_res_abs))




mj_acc_var$MJ_var_J <- all_data_1$MJ_var2_J
mj_acc_var$MJ_var_VJ <- all_data_1$MJ_var2_VJ
mj_acc_var$MJ_var_3 <- all_data_1$MJ_var2_3
mj_acc_var$MJ_var_4 <- all_data_1$MJ_var2_4

mj_acc_var$MJ_change_var2 <- mj_acc_var$MJ_var_J/mj_acc_var$MJ_var_VJ
mj_acc_var$MJ_change_var2_34 <- mj_acc_var$MJ_var_3/mj_acc_var$MJ_var_4


mj_acc_var[,c("FRAUD", "MJ_change_var2", "MJ_change_var2_34", 
              "ABW_CF_res_abs", "ABW_BS_res_abs")]

mj_acc_var$ACCR_BS_J[1:28]
mj_acc_var$ACCR_BS_J[28:87]


# Versuch Klassifikation mit ACCR_BS
accr_bs_test<-ifelse(mj_acc_var$ACCR_BS_J > 0.15, 1, 0)
table(j_acc_var$FRAUD, accr_bs_test)


quantile(with(j_acc_var, ACCR_BS_J[FRAUD == 1]))
quantile(with(j_acc_var, ACCR_BS_J[FRAUD == 0]))

model <- glm( FRAUD ~., data = mj_acc_var, family = binomial)
summary(model)
summary(model)$coef

model <- glm( FRAUD ~., data = j_acc_var, family = binomial)
summary(model)
summary(model)$coef

# ABW_CF_res_abs      6.83932    3.85141   1.776   0.0758 .
# MJ_change_var2     -0.16371    0.07788  -2.102   0.0355 *
# ACCR_BS_VJ        -27.04275   13.32577  -2.029   0.0424 *
# ACCR_BS_3          25.97593   13.15660   1.974   0.0483 *

mj_acc_var$ABW_CF_res_abs
mj_acc_var$ACCR_BS_VJ

model <- glm( FRAUD ~ABW_CF_res_abs+ MJ_change_var2, data = mj_acc_var, family = binomial)
summary(model)

model <- glm( FRAUD ~ABW_CF_res_abs, data = j_acc_var, family = binomial)
summary(model)





plot(x=mj_acc_var$ABW_CF_res_abs, y=mj_acc_var$MJ_change_var2, col=mj_acc_var$FRAUD)
legend('topright', legend = levels(mj_acc_var$FRAUD), col = 1:3, cex = 0.8, pch = 1)


plot(x=j_acc_var$ABW_CF_res_abs, y=j_acc_var$J_change_var2, col=j_acc_var$FRAUD)
legend('topright', legend = levels(j_acc_var$FRAUD), col = 1:3, cex = 0.8, pch = 1)



j_acc_var$change_ACCR_CF
plot(x=j_acc_var$J_change_var2, y=j_acc_var$change_ACCR_CF, col=j_acc_var$FRAUD)
legend('topright', legend = levels(j_acc_var$FRAUD), col = 1:3, cex = 0.8, pch = 1)

plot(x=j_acc_var$ABW_CF_res_abs, y=mj_acc_var$MJ_change_var2, col=mj_acc_var$FRAUD)
legend('topright', legend = levels(mj_acc_var$FRAUD), col = 1:3, cex = 0.8, pch = 1)



plot(x=j_acc_var$ABW_CF_res_abs, y=mj_acc_var$MJ_change_var2, col=mj_acc_var$FRAUD)
legend('topright', legend = levels(mj_acc_var$FRAUD), col = 1:3, cex = 0.8, pch = 1)

plot(x=j_acc_var$ABW_CF_res_abs, y=mj_acc_var$MJ_change_var2, col=mj_acc_var$FRAUD)
legend('topright', legend = levels(mj_acc_var$FRAUD), col = 1:3, cex = 0.8, pch = 1)


plot(x=mj_acc_var$ABW_CF_res_abs, y=mj_acc_var$MJ_change_var2, col=mj_acc_var$FRAUD)
legend('topright', legend = levels(mj_acc_var$FRAUD), col = 1:3, cex = 0.8, pch = 1)

hist(mj_acc_var$MJ_change_var2, col=mj_acc_var$FRAUD)

plt <- ggplot(mj_acc_var, aes_string(x="MJ_change_var2", fill="FRAUD")) +
  geom_histogram() + stat_bin(bins=50) 
plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
plt

plt <- ggplot(j_acc_var, aes_string(x="J_change_var2", fill="FRAUD")) +
  geom_histogram() + stat_bin(bins=50) 
plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
plt






#########################################################################

# Signifikanztest der Abweichungsvariablen 

# 1. Schritt Prüfung auf Normalverteilung:
par(mfrow=c(2,2))
hist(j_acc_var$ABW_BS_res_ab, col='steelblue', main='Original')
hist(j_acc_var$ABW_CF_res_ab, col='steelblue', main='Original')
hist(mj_acc_var$ABW_BS_res_ab, col='steelblue', main='Original')
hist(mj_acc_var$ABW_CF_res_ab, col='steelblue', main='Original')

# Transformation mit log
par(mfrow=c(2,2))
hist(log(j_acc_var$ABW_BS_res_ab), col='steelblue', main='Original')
hist(log(j_acc_var$ABW_CF_res_ab), col='steelblue', main='Original')
hist(log(mj_acc_var$ABW_BS_res_ab), col='steelblue', main='Original')
hist(log(mj_acc_var$ABW_CF_res_ab), col='steelblue', main='Original')

# Transformation mit cube-root
par(mfrow=c(2,2))
hist((j_acc_var$ABW_BS_res_ab)^(1/3), col='steelblue', main='Original')
hist((j_acc_var$ABW_CF_res_ab)^(1/3), col='steelblue', main='Original')
hist((mj_acc_var$ABW_BS_res_ab)^(1/3), col='steelblue', main='Original')
hist((mj_acc_var$ABW_CF_res_ab)^(1/3), col='steelblue', main='Original')


# QQ-Plot mit ohne Transformation
qqnorm((j_acc_var$ABW_BS_res_ab), main='Normal')
qqline((j_acc_var$ABW_BS_res_ab))
qqnorm((j_acc_var$ABW_CF_res_ab), main='Normal')
qqline((j_acc_var$ABW_CF_res_ab))
qqnorm((mj_acc_var$ABW_BS_res_ab), main='Normal')
qqline((mj_acc_var$ABW_BS_res_ab))
qqnorm((mj_acc_var$ABW_CF_res_ab), main='Normal')
qqline((mj_acc_var$ABW_CF_res_ab))

# QQ-Plot mit Log-Transformation
qqnorm(log(j_acc_var$ABW_BS_res_ab), main='Normal')
qqline(log(j_acc_var$ABW_BS_res_ab))
qqnorm(log(j_acc_var$ABW_CF_res_ab), main='Normal')
qqline(log(j_acc_var$ABW_CF_res_ab))
qqnorm(log(mj_acc_var$ABW_BS_res_ab), main='Normal')
qqline(log(mj_acc_var$ABW_BS_res_ab))
qqnorm(log(mj_acc_var$ABW_CF_res_ab), main='Normal')
qqline(log(mj_acc_var$ABW_CF_res_ab))



shapiro.test(j_acc_var$ABW_BS_res_abs) #  p-value = 1.153e-11
shapiro.test(j_acc_var$ABW_CF_res_abs) #  p-value < 2.2e-16
shapiro.test(mj_acc_var$ABW_BS_res_abs) #  p-value = 5.94e-12
shapiro.test(mj_acc_var$ABW_CF_res_abs) #   p-value < 2.2e-16


# Shapiro-Test mit Log-Transformation
shapiro.test(log(j_acc_var$ABW_BS_res_abs)) #  p-value = 0.2396
shapiro.test(log(j_acc_var$ABW_CF_res_abs)) #  p-value = 0.03009
shapiro.test(log(mj_acc_var$ABW_BS_res_abs)) #  p-value = 0.05452
shapiro.test(log(mj_acc_var$ABW_CF_res_abs)) #   p-value = 0.2308


# Kolmogorov-Test

ks.test(log(j_acc_var$ABW_BS_res_abs), 'pnorm') #  p-value = 0.2396
ks.test(log(j_acc_var$ABW_CF_res_abs), 'pnorm') #  p-value = 0.03009
ks.test(log(mj_acc_var$ABW_BS_res_abs), 'pnorm') #  p-value = 0.05452
ks.test(log(mj_acc_var$ABW_CF_res_abs), 'pnorm') #   p-value = 0.2308

with(j_acc_var, shapiro.test(log(ABW_BS_res_abs)[FRAUD == 1])) # p-value = 0.4829
with(j_acc_var, shapiro.test(log(ABW_BS_res_abs)[FRAUD == 0])) # p-value = 0.6226
with(j_acc_var, shapiro.test(log(ABW_CF_res_abs)[FRAUD == 1])) # p-value = 0.2699
with(j_acc_var, shapiro.test(log(ABW_CF_res_abs)[FRAUD == 0])) # p-value = 0.06737

with(mj_acc_var, shapiro.test(log(ABW_BS_res_abs)[FRAUD == 1])) # p-value = 0.09051
with(mj_acc_var, shapiro.test(log(ABW_BS_res_abs)[FRAUD == 0])) # p-value = 0.5019
with(mj_acc_var, shapiro.test(log(ABW_CF_res_abs)[FRAUD == 1])) # p-value = 0.5057
with(mj_acc_var, shapiro.test(log(ABW_CF_res_abs)[FRAUD == 0])) # p-value = 0.6091


with(j_acc_var, ABW_BS_res_abs[FRAUD == 1])

# QQPLOT für Jones ABweichungen

qqnorm(log(with(j_acc_var, ABW_BS_res_abs[FRAUD == 0])), main='Normal')
qqline(log(with(j_acc_var, ABW_BS_res_abs[FRAUD == 0])))

qqnorm(log(with(j_acc_var, ABW_BS_res_abs[FRAUD == 1])), main='Normal')
qqline(log(with(j_acc_var, ABW_BS_res_abs[FRAUD == 1])))

qqnorm(log(with(j_acc_var, ABW_CF_res_abs[FRAUD == 0])), main='Normal')
qqline(log(with(j_acc_var, ABW_CF_res_abs[FRAUD == 0])))

qqnorm(log(with(j_acc_var, ABW_CF_res_abs[FRAUD == 1])), main='Normal')
qqline(log(with(j_acc_var, ABW_CF_res_abs[FRAUD == 1])))


# QQPLOT für ModifiedJones ABweichungen
qqnorm(log(with(mj_acc_var, ABW_BS_res_abs[FRAUD == 0])), main='Normal')
qqline(log(with(mj_acc_var, ABW_BS_res_abs[FRAUD == 0])))

qqnorm(log(with(mj_acc_var, ABW_BS_res_abs[FRAUD == 1])), main='Normal')
qqline(log(with(mj_acc_var, ABW_BS_res_abs[FRAUD == 1])))

qqnorm(log(with(mj_acc_var, ABW_CF_res_abs[FRAUD == 0])), main='Normal')
qqline(log(with(mj_acc_var, ABW_CF_res_abs[FRAUD == 0])))

qqnorm(log(with(mj_acc_var, ABW_CF_res_abs[FRAUD == 1])), main='Normal')
qqline(log(with(mj_acc_var, ABW_CF_res_abs[FRAUD == 1])))



# P-Wert > 0.05 --> Wert größer als 0,05 Alpha
# dh, keine signifikante Differenz zwischen der Varianz der zwei Datensätze
#  daher kann der T-Test angewendet werden

var.test(log(ABW_BS_res_abs) ~ FRAUD, data=j_acc_var) # p-value = 0.5066
var.test(log(ABW_CF_res_abs) ~ FRAUD, data=j_acc_var) # p-value 0.6911
var.test(log(ABW_BS_res_abs) ~ FRAUD, data=mj_acc_var) # p-value = 0.6856
var.test(log(ABW_CF_res_abs) ~ FRAUD, data=mj_acc_var) # p-value 0.3517

#######

# log(with(mj_acc_var, ABW_CF_res_abs[FRAUD == 0])

# Test auf Signifikanz mittels T-Test
# Daten sind NV und beide Gruppen 1 und 0 sind unabhängig von einander


# Modified-Jones-Modell
####################
x<- log(with(mj_acc_var, ABW_CF_res_abs[FRAUD == 0]))
y<- log(with(mj_acc_var, ABW_CF_res_abs[FRAUD == 1]))        

t.test(x,y, var.equal = TRUE)
# t = -2.6311, df = 85, p-value = 0.0101
# Ergebnis ist signifikant von einander unterschiedlich
# daher ist der Mittelwert der Abweichung bei 
# betrügerischen Unternehmen anders als bei nicht betrüg. Unternehmen


x<- log(with(mj_acc_var, ABW_BS_res_abs[FRAUD == 0]))
y<- log(with(mj_acc_var, ABW_BS_res_abs[FRAUD == 1]))        

t.test(x,y, var.equal = TRUE)
# p-value = 0.192 - nicht signifikant
#######################################

# Jones Modell T-TEST

x<- log(with(j_acc_var, ABW_CF_res_abs[FRAUD == 0]))
y<- log(with(j_acc_var, ABW_CF_res_abs[FRAUD == 1]))        

t.test(x,y, var.equal = TRUE) # p-value = 0.003593


x<- log(with(j_acc_var, ABW_BS_res_abs[FRAUD == 0]))
y<- log(with(j_acc_var, ABW_BS_res_abs[FRAUD == 1]))        

t.test(x,y, var.equal = TRUE)
# p-value = 0.1312 - nicht signifikant



# Sicherheitshalber auch mittels Kolmogorov-Smirnov-Test

#ks.test(j_acc_var$ABW_BS_res_abs, 'pnorm')
#ks.test(j_acc_var$ABW_CF_res_abs, 'pnorm')
#ks.test(mj_acc_var$ABW_BS_res_abs, 'pnorm')
#ks.test(mj_acc_var$ABW_CF_res_abs, 'pnorm')
#####################################################################
# Wenn p>0.05 wäre --> Verteilung der Daten ist nicht signifikant anders
# als eine Normalverteilung --> demnach Normalverteilung gegeben

# Wenn p< 0.05. --> dann keine Normalverteilung gegeben

#####################################################################

# Prüfen auf Fehldarstellung 

# wie gut funktioniert das (Modified-) Jones Modell

hist(j_acc_var$ABW_CF)
hist(log(j_acc_var$ABW_CF))

j_bigger0<-j_acc_var %>% filter(ABW_CF >0)

plt <- ggplot(j_bigger0, aes_string((x="ABW_CF"), fill="FRAUD")) +
  geom_histogram() + stat_bin(bins=50) 
plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
plt

j_acc_var$LOG_ABW_CF

plt <- ggplot(j_acc_var, aes_string(log(x="LOG_ABW_CF"), fill="FRAUD")) +
  geom_histogram() + stat_bin(bins=50) 
plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
plt


j_acc_var$ABW_CF_res_abs_rounded<- round(j_acc_var$ABW_CF_res_abs*100, 2)


quantile(with(j_acc_var, ABW_CF_res_abs_rounded[FRAUD == 1]))
quantile(with(j_acc_var, ABW_CF_res_abs_rounded[FRAUD == 0]))

quantile(j_acc_var$ABW_CF_res_abs_rounded)

library("ggpubr")
ggboxplot(j_acc_var, x = "FRAUD", y = "ABW_CF_res_abs_rounded", 
          color = "FRAUD", palette = c("#00AFBB", "#E7B800"),
          ylab = "Abweichung in %", xlab = "FRAUD = 1 vs. NON FRAUD = 0")



j_acc_var$ABW_CF_res_abs_rounded

# Ein-Dimensionale Graphik erzeugen

j_acc_var$ABW_CF_res_abs_rounded<- round(j_acc_var$ABW_CF_res_abs*100, 2)
par(mfrow=c(1,1))
stripchart(ABW_CF_res_abs_rounded ~ FRAUD, data=j_acc_var,
           pch=22, frame=FALSE)

j_100<-j_acc_var %>% filter(ABW_CF_res_abs_rounded < 100)

j_100 %>% group_by(FRAUD) %>% summarise(COUNT = n())

#FRAUD COUNT
#<fct> <int>
#  1 0        58
#  2 1        25

# Werte über 50% resultieren bei Fraud 1 bei 3 der 28 Werte
# Werte über 50% resultieren bei NonFraud 0 bei 1 der 59 Werte
# somit viele Ausreißer entfernt, die tendenziell
# eher bei FRAUD Unternehmen vorzufinden sind

# nochmals 1-D- Plotten mittels Stripchart
par(mfrow=c(1,1))
stripchart((ABW_CF_res_abs_rounded) ~ FRAUD, data=j_100,
           pch=22, frame=FALSE)

par(mfrow=c(1,1))
stripchart((ABW_CF) ~ FRAUD, data=j_100,
           pch=22, frame=FALSE)



# selbe nochmals für modified jones machen
mj_acc_var$ABW_CF_res_abs_rounded <- round(mj_acc_var$ABW_CF_res_abs*100, 2)
quantile(with(mj_acc_var, ABW_CF_res_abs_rounded[FRAUD == 1]))
quantile(with(mj_acc_var, ABW_CF_res_abs_rounded[FRAUD == 0]))

mj_100<-mj_acc_var %>% filter(ABW_CF_res_abs_rounded < 100)

mj_var<-mj_acc_var %>% filter(ABW_CF_res_abs_rounded < 20)

par(mfrow=c(1,1))
stripchart(ABW_CF_res_abs_rounded ~ FRAUD, data=mj_acc_var,
           pch=22, frame=FALSE)

par(mfrow=c(1,1))
stripchart(ABW_CF_res_abs_rounded ~ FRAUD, data=mj_100,
           pch=22, frame=FALSE)

par(mfrow=c(1,1))
stripchart(ABW_CF_res_abs_rounded ~ FRAUD, data=mj_var,
           pch=22, frame=FALSE)


j_acc_var$ABW_CF_res_abs_rounded

j_acc_var$ABW_CF


ggplot(j_acc_var, aes(x=ABW_CF_res_abs_rounded, group=FRAUD, color=FRAUD)) +
  geom_histogram() +
  labs(title="Histogramm der Variablen") +
  stat_bin(bins=30)

colnames(mj_acc_var)

par(mfrow=c(2,2))
# Jones- Modell
# Verwendung absolute Werte
plt <- ggplot(j_acc_var, aes_string(x="ABW_CF_res_abs_rounded", fill="FRAUD")) +
  geom_histogram() + stat_bin(bins=50) 
plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
plt

j_100<-j_acc_var %>% filter(ABW_CF_res_abs_rounded < 100)

plt <- ggplot(j_100, aes_string(x="ABW_CF_res_abs_rounded", fill="FRAUD")) +
  geom_histogram() + stat_bin(bins=50) 
plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
plt

j_acc_var$ABW_CF

# Verwendung Abweichungen mit Vorzeichen



plt <- ggplot(j_acc_var, aes_string(x="ABW_CF", fill="FRAUD")) +
  geom_histogram() + stat_bin(bins=50) 
plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
plt

j_acc_var_v22<-j_acc_var %>% filter(ABW_CF <2 & ABW_CF > -2)

plt <- ggplot(j_acc_var_v22, aes_string(x="ABW_CF", fill="FRAUD")) +
  geom_histogram() + stat_bin(bins=50) 
plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
plt



# Modified-Jones Modell
# Verwendung Absolute Werte
plt <- ggplot(mj_acc_var, aes_string(x="ABW_CF_res_abs_rounded", fill="FRAUD")) +
  geom_histogram() + stat_bin(bins=50) 
plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
plt

mj_100<-mj_acc_var %>% filter(ABW_CF_res_abs_rounded < 100)

plt <- ggplot(mj_100, aes_string(x="ABW_CF_res_abs_rounded", fill="FRAUD")) +
  geom_histogram() + stat_bin(bins=50) 
plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
plt


j_acc_var$LOG_ABW_CF<- log(j_acc_var$ABW_CF_res_abs_rounded)

plt <- ggplot(j_acc_var, aes_string(x="LOG_ABW_CF", fill="FRAUD")) +
  geom_histogram() + stat_bin(bins=50) 
plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
plt


mj_acc_var$LOG_ABW_CF<- log(mj_acc_var$ABW_CF_res_abs_rounded)

plt <- ggplot(mj_acc_var, aes_string(x="LOG_ABW_CF", fill="FRAUD")) +
  geom_histogram() + stat_bin(bins=50) 
plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
plt

j_100

plt <- ggplot(j_100, aes_string(x="ABW_CF_res_abs_rounded", fill="FRAUD")) +
  geom_histogram() + stat_bin(bins=50) 
plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
plt




#######################################################
# Analyse des F-Scores 
#####################################################################################
#########################################################################################
 
dim(all_data_1)
colnames(f_score)


f_score<-all_data_5[,c("FRAUD", "WC_ACC_J", "WC_ACC_34", 
                       "RSST_ACC_J", "RSST_ACC_VJ",
                       "CHANGE_REC_AV_J","CHANGE_REC_AV_VJ","CHANGE_INV_J","CHANGE_INV_VJ",
                       "SOFT_ASSET_J","SOFT_ASSET_VJ","CHANGE_CASH_SALES","CHANGE_CASH_MARGIN_J",
                       "CHANGE_CASH_MARGIN_VJ","CHANGE_RET_ASSET_J","CHANGE_RET_ASSET_VJ","CHANGE_FREE_CF",
                       "DEF_TAX_EXP_J","DEF_TAX_EXP_VJ")]


length(colnames(f_score)) # 19


corrplot(cor(f_score[sapply(f_score,is.numeric)]), method="number")


head(f_score, 5)

model <- glm( FRAUD ~., data = f_score, family = binomial)
summary(model)

model1 <- randomForest(FRAUD ~ ABW_CF_res_abs, data = j_acc_var, importance = TRUE)
model1

random.forest.importance(FRAUD ~. ,data=f_score)
# CHANGE_FREE_CF              8.7875984
# DEF_TAX_EXP_J               6.0378340
# CHANGE_REC_AV_J             5.2707791
# CHANGE_REC_AV_VJ            5.5889550
# CHANGE_INV_J                4.7660226

model <- glm( FRAUD ~CHANGE_FREE_CF + DEF_TAX_EXP_J + CHANGE_REC_AV_J +
                CHANGE_INV_J, data = f_score, family = binomial)
summary(model)

model <- glm( FRAUD ~CHANGE_FREE_CF  +
                CHANGE_INV_J, data = f_score, family = binomial)
summary(model)

colnames(f_score)


#################################################################################
################################################################################

#################################################################################
################################################################################

#### Analyse des F-Scores:

# Import der Actual Issuance Variable, um F-Score zu berechnen

##################################################################################

library("readxl")
issuance_data <- read_excel("issuance.xlsx", sheet="Actual Issuance")
head(issuance_data,3)

head(f_score, 3)

# Actual Issuance Spalte von Stichtag auf den Datensatz F-Score hinzufügen

f_score$issue_X<-issuance_data$Issuance_Stichtag
f_score$issue_X1<-issuance_data$Issuance_Vorjahr
f_score$issue_X2<-issuance_data$`Issuance_Vorjahr-Vorjahr`

v<-replace(issuance_data$Issuance_Stichtag,issuance_data$Issuance_Stichtag<0,0)
real_issuance<-v

# REAL ISSUANCE - es geht darum -1 in 0 umzuwandeln
# da ich selbst -1 erfasst habe, wenn das Unternehmen Repurchases gemacht hat
# mit somit ist REAL => nur 0 und 1
# issuance hat -1, 0, 1
# -1 --> Repurchase, 0 = nix, 1 = issuance stock
v<-replace(f_score$issue_X,f_score$issue_X<0,0)
f_score$issue_X_real<-v

colnames(f_score)
# Berechnung des F-Scores - mittels Regression laut F-Score-Studie:


# average income france 19 century

f_score$CHANGE_RET_ASSET_J <-all_data_1$CHANGE_RET_ASSET_J
f_score$CHANGE_CASH_SALES <- all_data_1$CHANGE_CASH_SALES
colnames(f_score)

v<- -7.893+0.790 * (f_score$RSST_ACC_J)+ 2.518 * 
  (f_score$CHANGE_REC_AV_J)+ 1.191 * (f_score$CHANGE_INV_J)+ 1.979 * 
  (f_score$SOFT_ASSET_J)+ 0.171 * 
  (f_score$CHANGE_CASH_SALES)+ -0.932 * (f_score$CHANGE_RET_ASSET_J) + 1.029 * (f_score$issue_X_real)
v
# Umwandlung der Ergebnisse in Wahrscheinlichkeiten mittels Sigmoid-Funktion

v_sigm<-exp(v) / (1+ exp(v))

# Laut F-Score Studie Wahrscheinlichkeit für FRAUD bei: = 0.0037
# somit Berechnung in Werte größer kleiner 1 sind
# also 1 = WK gleich hoch wie in der Gesamtpopulation
# WK > 1 --> höhere WK als in Gesamtpopulation
# WK < 1 --> WK geringer als in der Gesamtpopulation

v_fscore<- v_sigm/0.0037

v_fscore[1:28] # Anzahl > 1 = 16
16/28 # Trefferquote: 0.5714286

v_fscore[29:87] # 19
(58-19)/58 # 0.6724138

f_score[f_score$issue_X <0]

# f_score$issue_X_real

f_score$F_Score_Result_before_prob<-v_sigm
f_score$F_Score_Result<-v_fscore

colnames(f_score)

f_score %>% group_by(FRAUD) %>%
  summarise(Count = n(),  
            Median = median(F_Score_Result),
            Mean = mean(F_Score_Result), 
            STD = sd(F_Score_Result),
            Max = max(F_Score_Result),
            Min = min(F_Score_Result))


# F-Score

# Prüfung auf Normalverteilung

# 1. Schritt Prüfung auf Normalverteilung:
par(mfrow=c(2,2))
f_score$F_Score_Result

plt <- ggplot(f_score, aes_string(x="F_Score_Result", fill="FRAUD")) +
  geom_histogram() + stat_bin(bins=50) 
plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
plt

# F-Score Ergebnis logarithmiert 
f_score$F_Score_Result_log<-log(f_score$F_Score_Result)

# Histogramm mit logarithmierten F-Score - gruppiert nach FRAUD
plt <- ggplot(f_score, aes_string(x="F_Score_Result_log", fill="FRAUD")) +
  geom_histogram() + stat_bin(bins=50) 
plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
plt


# Transformation mit log
# par(mfrow=c(2,2))


# QQ-Plot mit ohne Transformation
qqnorm((f_score$F_Score_Result), main='Normal')
qqline((f_score$F_Score_Result))

# wird besser - aber immer noch keine NV
qqnorm(log(f_score$F_Score_Result), main='Normal')
qqline(log(f_score$F_Score_Result))

# nicht gut
qqnorm((f_score$F_Score_Result)^(1/3), main='Normal')
qqline((f_score$F_Score_Result)^(1/3))


shapiro.test(f_score$F_Score_Result) #   p-value < 2.2e-16
shapiro.test(log(f_score$F_Score_Result)) #  p-value = 2.427e-08

with(f_score, shapiro.test(log(F_Score_Result)[FRAUD == 1])) # p-value = 0.0006384

with(f_score, shapiro.test(log(F_Score_Result)[FRAUD == 0])) # p-value = 1.047e-06


wilcox.test(F_Score_Result ~ FRAUD, data=f_score, 
            exact=FALSE)

with(f_score, quantile((F_Score_Result)[FRAUD == 0]))
with(f_score, quantile((F_Score_Result)[FRAUD == 1]))

#quantile(f_score$F_Score_Result)

f_score
# True positive, negative... Table erzeugen

fraud_1<-replace(f_score$F_Score_Result,f_score$F_Score_Result>=1,1)
fraud_1<-replace(fraud_1,fraud_1<1,0)
fraud_1<-factor(fraud_1)
length(fraud_1)

f_score$boolean_result<-fraud_1

table(f_score$FRAUD, f_score$boolean_result)



fraud_2<-replace(f_score$F_Score_Result,f_score$F_Score_Result<1.4,0)
fraud_2<-replace(fraud_2,fraud_2>1.4,1)
fraud_2<-factor(fraud_2)

f_score$boolean_result_14<-fraud_2

table(f_score$FRAUD, f_score$boolean_result_14)

#### # Auswertung der Variablen issue X , X1 und X2 für drei Jahre

f_score %>% group_by(FRAUD) %>% 
  summarise(Count=n(),
            Mean = mean(issue_X), 
            STDABW = sd(issue_X))


f_score %>% group_by(FRAUD) %>% 
  summarise(Count=n(),
            Mean = mean(issue_X1), 
            STDABW = sd(issue_X1))


f_score %>% group_by(FRAUD) %>% 
  summarise(Count=n(),
            Mean = mean(issue_X2), 
            STDABW = sd(issue_X2))

# ermittelte Summenwert für die Jahre X, X-1 und X-2 
f_score$issue_sum<- f_score$issue_X + f_score$issue_X1 + f_score$issue_X2


f_score %>% group_by(FRAUD) %>% 
  summarise(Count=n(),
            Mean = mean(issue_sum), 
            STDABW = sd(issue_sum))


# Auswertung mittels Häufigkeitstabelle, wie häufig gewisse Klassifikationen
# je Gruppe = FRAUD, kein FRAUD vorkommen

library(sur)

percent.table(f_score$FRAUD, f_score$issue_X)

table(f_score$FRAUD, f_score$issue_X)/28*100
table(f_score$FRAUD, f_score$issue_X)/59*100

table(f_score$FRAUD, f_score$issue_X1)/28*100
table(f_score$FRAUD, f_score$issue_X1)/59*100

table(f_score$FRAUD, f_score$issue_X2)/28*100
table(f_score$FRAUD, f_score$issue_X2)/59*100

table(f_score$FRAUD, f_score$issue_X1)

table(f_score$FRAUD, f_score$issue_sum)


# Auswertung auf Signifikanz der drei Kategorien -1, 1, 0 bei 0 und 1 kein Betrug/Betrug

# Compute the analysis of variance
res.aov <- aov(issue_X ~ FRAUD, data = f_score)
# Summary of the analysis
summary(res.aov)

res.aov <- aov(issue_X1 ~ FRAUD, data = f_score)
# Summary of the analysis
summary(res.aov)

res.aov <- aov(issue_X2 ~ FRAUD, data = f_score)
# Summary of the analysis
summary(res.aov)



#############################################
#############################################
#############################################




library(caret)
# 431
dim(f_score)

colnames(f_score_lim1)

colnames(f_score)

# Limitierung des F-Score-Datensatzes auf jene Variablen, die nicht
# hochkorrelierend untereinander sind
f_score_lim2<-f_score[,c("FRAUD", "WC_ACC_J", "RSST_ACC_J" , 
           "RSST_ACC_VJ","CHANGE_REC_AV_J","CHANGE_INV_J" ,
           "SOFT_ASSET_J","SOFT_ASSET_VJ", "CHANGE_CASH_SALES",
           "CHANGE_CASH_MARGIN_J", "CHANGE_RET_ASSET_J", "CHANGE_FREE_CF",
           "DEF_TAX_EXP_J","DEF_TAX_EXP_VJ", 
            "F_Score_Result", "issue_sum", "boolean_result")]


corrplot(cor(f_score_lim2[sapply(f_score_lim2,is.numeric)]), method="number")


model2 <- glm(FRAUD ~., data = f_score_lim2, family = binomial) %>%
  stepAIC(trace = FALSE, direction="forward")

summary(model2)

# CHANGE_INV_J          42.73796   17.52207   2.439   0.0147 *
# CHANGE_FREE_CF       -12.86343    5.51254  -2.333   0.0196 *
# issue_sum              0.62643    0.26099   2.400   0.0164 *


model2 <- glm(FRAUD ~., data = f_score_lim2, family = binomial) %>%
  stepAIC(trace = FALSE, direction="backward")

summary(model2)
#(Intercept)     -1.0751     0.2995  -3.590 0.000331 ***
#  CHANGE_INV_J    31.5778    13.5551   2.330 0.019828 *  
#  CHANGE_FREE_CF -12.0718     4.5568  -2.649 0.008068 ** 
#  issue_sum        0.5606     0.1649   3.400 0.000674 ***
# AIC: 86.925

model2 <- glm(FRAUD ~., data = f_score_lim2, family = binomial) %>%
  stepAIC(trace = FALSE, direction="both")

summary(model2)

###########################################################################
##########################################################################
#########################################################################

# Backward, Forward Elimination, bei Veränderung der Ausgangsvariablen

# alle korrelierenden Werte entfernt - nun max. 0,55 Korrelation
f_score_lim3<-f_score[,c("FRAUD", "RSST_ACC_J" , 
                         "RSST_ACC_VJ","CHANGE_REC_AV_J","CHANGE_INV_J" ,
                         "SOFT_ASSET_J", "CHANGE_CASH_SALES",
                          "CHANGE_FREE_CF",
                         "issue_sum")]

corrplot(cor(f_score_lim3[sapply(f_score_lim3,is.numeric)]), method="number")


model2 <- glm(FRAUD ~., data = f_score_lim3, family = binomial) %>%
  stepAIC(trace = FALSE, direction="forward")

summary(model2)

# CHANGE_FREE_CF    -12.0910     4.9401  -2.448 0.014384 *  
# issue_sum           0.5889     0.1755   3.354 0.000795 ***
# CHANGE_INV_J       29.7973    13.3922   2.225 0.026083 *

model2 <- glm(FRAUD ~., data = f_score_lim3, family = binomial) %>%
  stepAIC(trace = FALSE, direction="backward")

summary(model2)

#(Intercept)     -1.0751     0.2995  -3.590 0.000331 ***
#  CHANGE_INV_J    31.5778    13.5551   2.330 0.019828 *  
#  CHANGE_FREE_CF -12.0718     4.5568  -2.649 0.008068 ** 
#  issue_sum        0.5606     0.1649   3.400 0.000674 ***


# Ausgehend davon - Anwendung logistische Regression
# von F-Score

model2 <- glm(FRAUD ~CHANGE_INV_J + CHANGE_FREE_CF + issue_sum, data = f_score_lim3, family = binomial)

summary(model2)



############################################################
#########################################################

############################################################
#########################################################

# Auswertung der Datenqualität Extensions

#######################################################################

library("readxl")
specific_tags <- read_excel("issuance.xlsx", sheet="entity_specific_tags")
head(specific_tags,3)

summary(specific_tags)

# Specific Tags in numerische Daten umwandeln

specific_tags$entity_specific_tag<-as.numeric(specific_tags$entity_specific_tag)
specific_tags$entity_specific_tag_VJ<-as.numeric(specific_tags$entity_specific_tag_VJ)

colnames(specific_tags)

summary(specific_tags)


class(non_fraud_data$EXT_J)

# Abgleich non_fraud_data und specific_tags (Fraud-Data)

quantile(non_fraud_data$EXT_J)
quantile(non_fraud_data$EXT_VJ)

quantile(specific_tags$entity_specific_tag)
quantile(specific_tags$entity_specific_tag_VJ)

# beide Datensätze zusammensetzen
class(non_fraud_data$FRAUD_1_NONFRAUD_0)

tag_non_fraud<-non_fraud_data[,c("EXT_J", "EXT_VJ","FRAUD_1_NONFRAUD_0" )]
tag_fraud<-specific_tags[,c("entity_specific_tag", "entity_specific_tag_VJ")]
# Einfügen Spalte - dass 28 x Fraud Unternehmen ist
x<-c(rep(1), rep(1,27))
length(x)
tag_fraud$FRAUD<-x
dim(tag_fraud)

colnames(tag_fraud)

colnames(tag_fraud) <- c("EXT_J", "EXT_VJ", "FRAUD")
colnames(tag_non_fraud) <- c("EXT_J", "EXT_VJ", "FRAUD")

tag_combined<-rbind(tag_fraud, tag_non_fraud)

summary(tag_combined)

tag_combined %>% group_by(FRAUD) %>% 
  summarise(Count=n(),
            Mean = mean(EXT_J), 
            STDABW = sd(EXT_J))

tag_combined %>% group_by(FRAUD) %>% 
  summarise(Count=n(),
            Mean = mean(EXT_VJ), 
            STDABW = sd(EXT_VJ))

# Entity Tag Auswertung Extensions

# Prüfung auf Normalverteilung

# 1. Schritt Prüfung auf Normalverteilung:
# par(mfrow=c(2,2))
f_score$F_Score_Result

tag_combined$FRAUD<-as.factor(tag_combined$FRAUD)

plt <- ggplot(tag_combined, aes_string(x="EXT_J", fill="FRAUD")) +
  geom_histogram() + stat_bin(bins=50) 
plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
plt


plt <- ggplot(tag_combined, aes_string(x="EXT_VJ", fill="FRAUD")) +
  geom_histogram() + stat_bin(bins=50) 
plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
plt

#############

# Extension Ergebnis logarithmiert 
tag_combined$EXT_J_log<-log(tag_combined$EXT_J)
tag_combined$EXT_VJ_log<-log(tag_combined$EXT_VJ)

# Histogramm mit logarithmierten Extension - gruppiert nach FRAUD
plt <- ggplot(tag_combined, aes_string(x="EXT_J_log", fill="FRAUD")) +
  geom_histogram() + stat_bin(bins=50) 
plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
plt

plt <- ggplot(tag_combined, aes_string(x="EXT_VJ_log", fill="FRAUD")) +
  geom_histogram() + stat_bin(bins=50) 
plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
plt

# kein Verbesserung - Daten werden nicht NV


tag_combined$EXT_J_sqrt<-sqrt(tag_combined$EXT_J)
tag_combined$EXT_VJ_sqrt<-sqrt(tag_combined$EXT_VJ)

# Histogramm mit logarithmierten Extension - gruppiert nach FRAUD
plt <- ggplot(tag_combined, aes_string(x="EXT_J_sqrt", fill="FRAUD")) +
  geom_histogram() + stat_bin(bins=50) 
plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
plt

plt <- ggplot(tag_combined, aes_string(x="EXT_VJ_sqrt", fill="FRAUD")) +
  geom_histogram() + stat_bin(bins=50) 
plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
plt

# keine NV - zu viel Spread


# 3. Versuch mit Kubischer Funktion

tag_combined$EXT_J_cube<-(tag_combined$EXT_J)^(1/3)
tag_combined$EXT_VJ_cube<-sqrt(tag_combined$EXT_VJ)^(1/3)

# Histogramm mit logarithmierten Extension - gruppiert nach FRAUD
plt <- ggplot(tag_combined, aes_string(x="EXT_J_cube", fill="FRAUD")) +
  geom_histogram() + stat_bin(bins=50) 
plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
plt

plt <- ggplot(tag_combined, aes_string(x="EXT_VJ_cube", fill="FRAUD")) +
  geom_histogram() + stat_bin(bins=50) 
plt<- plt + labs(fill = "Betrug: 1 \nkein Betrug: 0")
plt

# ausgehend von drei Versuchen mit
# sqrt, log und Wurzel aus 3 bzw. hoch 1/3 - keine Verbesserung
# es liegt wohl keine NV vor 



############################


# Transformation mit log
par(mfrow=c(2,2))


# QQ-Plot ohne Transformation
qqnorm((tag_combined$EXT_J), main='QQ-PLOT ohne Transformation von Jahr X')
qqline((tag_combined$EXT_J))

# wird besser - aber immer noch keine NV
qqnorm((tag_combined$EXT_VJ), main='QQ-PLOT ohne Transformation von Jahr X-1')
qqline((tag_combined$EXT_VJ))

# QQ-Plot mit Transformation log
qqnorm(log(tag_combined$EXT_J), main='QQ-PLOT mit Log-Transformation von Jahr X')
qqline(log(tag_combined$EXT_J))

# wird 
qqnorm(log(tag_combined$EXT_VJ), main='QQ-PLOT mit Log-Transformation von Jahr X-1')
qqline(log(tag_combined$EXT_VJ))



# nicht gut
qqnorm((tag_combined$EXT_VJ)^(1/3), main='QQ-PLOT mit Log-Transformation von Jahr X-1')
qqline((tag_combined$EXT_VJ)^(1/3))

# Prüfung auf NV
# je kleiner der p-wert desto kleiner die WK, dass eine solche Verteilung 
# bei einer NV vorkommt - je höher desto eher sind Daten NV 

shapiro.test(tag_combined$EXT_J) # p-value = 4.665e-07
shapiro.test(tag_combined$EXT_VJ) #   p-value = 4.127e-05
shapiro.test(log(tag_combined$EXT_J)) #  p-value = 7.481e-12
shapiro.test(log(tag_combined$EXT_VJ)) # p-value = 8.01e-12

# Das Ergebnis zeigt - alle Werte mit p-Wert unter 0.05 - keine NV anzunehmen

####################################################

# Prüfung auf Signifikanz der Ergebnisse je Gruppe für die Extension
# mittels Wilcoxon- Test bzw. Mann-Whitney-U-Test - ist dasselbe

# Visualisierung der Daten
library("ggpubr")
ggboxplot(tag_combined, x = "FRAUD", y = "EXT_J", 
          color = "FRAUD", palette = c("#00AFBB", "#E7B800"),
          ylab = "Anteil an Extensions", xlab = "Anteil Extensions im Jahr X")

ggboxplot(tag_combined, x = "FRAUD", y = "EXT_VJ", 
          color = "FRAUD", palette = c("#00AFBB", "#E7B800"),
          ylab = "Anteil an Extensions", xlab = "Anteil Extensions im Jahr X-1")

# http://www.sthda.com/english/wiki/unpaired-two-samples-wilcoxon-test-in-r
# Wilcox-Test/Mann-Whitney U-Test ist derselbe

# https://bjoernwalther.com/mann-whitney-u-test-wilcoxon-test-in-r-rechnen/
# exact= wenn weniger als 40-50 Stichproben, dann TRUE
# sonst FALSE
# Sinn: 

wilcox.test(EXT_J ~ FRAUD, data=tag_combined, 
            exact=FALSE) # p-value = 0.002676

wilcox.test(EXT_J ~ FRAUD, data=tag_combined, 
            exact=FALSE, alternative="less") # p-value = 0.001338

wilcox.test(EXT_VJ ~ FRAUD, data=tag_combined, 
            exact=FALSE) # p-value = 0.001643

wilcox.test(EXT_VJ ~ FRAUD, data=tag_combined, 
            exact=FALSE, alternative="less")


# Evaluierung der Effektstärke 

# Jahr
# Berechnung r = | z-Wert /Wurzel(N) |
z <- qnorm(0.002676/2) # Ergebnis durch 2
r <- abs(z/sqrt(87)) # Anzahl Stichproben
r
# 0.3219228

# Vorjahr
# Berechnung r = | z-Wert /Wurzel(N) |
z <- qnorm(0.001643/2) # Ergebnis durch 2
#z <- qnorm(0.001643)
r <- abs(z/sqrt(87)) # Anzahl Stichproben
r
# 0.3375185

#Laut Cohen: Statistical Power Analysis for the Behavioral Sciences (1988), S. 79-81 bzw. Cohen (1992), S. 157 sind die Effektgrenzen:
  
#  ab 0,1 (schwach)
#ab 0,3 (mittel)
#ab 0,5 (stark).

# mittlere Effektstärke 


# Analyse + Versuch Prognose anhand von Extensions

tag_combined$EXT_J
tag_combined$FRAUD
tag_combined[,c("FRAUD", "EXT_J")]



########################
# Analyse mittels log. Regression: 


tag_combined$EXT_J


set.seed(123)
sens_results<-c()
spec_results<-c()
results_test<-c()
for (i in 1:1000) {
  
  training.samples <- tag_combined$FRAUD %>% 
    createDataPartition(p = 0.7, list = FALSE)
  
  # Zuweisung der Indexwerte zu Train und Test
  train.data  <- tag_combined[training.samples, ]
  test.data <- tag_combined[-training.samples, ]
  
  # Fit the model
  model <- glm( FRAUD ~EXT_J, 
                data = train.data, family = binomial)
  
  #predicted_train <- predict(model, train.data, type="response")
  predicted_test <- predict(model, test.data, type="response")
  
  #train_optimal<-optimalCutoff(training_set$FRAUD, predicted)
  #test_optimal<-optimalCutoff(test.data$FRAUD, predicted_test)
  
  
  predicted_result<-ifelse(predicted_test > 0.2, 1, 0)
  
  result<-mean(predicted_result == test.data$FRAUD)
  results_test<-c(results_test, result)
  
  #results_train<-c(results_train, train_optimal)
  # results_test <- c(results_test, test_optimal)
  
  # Ermittlung des Spezifität und Sensitivität
  actual_values<-test.data$FRAUD
  conf_matrix<-table(predicted_result,actual_values)
  sens<-conf_matrix[4]/(conf_matrix[3]+conf_matrix[4])
  spec<-conf_matrix[1]/(conf_matrix[1]+conf_matrix[2])
  
  # Einzelergebnisse in die Vektoren einfügen
  sens_results<-c(sens_results, sens)
  spec_results<-c(spec_results, spec)
  
  
}
mean(results_test)
sens_results[is.na(sens_results)]<-0
spec_results[is.na(spec_results)]<-0

mean(sens_results)
mean(spec_results)


#############


# Alternative Auswertung:

mean(tag_combined$EXT_J[1:28]) # 0.2134267
median(tag_combined$EXT_J[1:28]) # 0.2057867

mean(tag_combined$EXT_J[29:87]) # 0.1301285
median(tag_combined$EXT_J[29:87]) # 0.1701149

mean(tag_combined$EXT_J)


# Werte über einer gewissen Grenze - 1 und darunter 0 


# versuch 1
own_pred1<-ifelse(tag_combined$EXT_J > 0.15, 1, 0)
tag_combined$FRAUD

mean(own_pred1 == tag_combined$FRAUD) # 0.5402299

# Versuch 2
own_pred1<-ifelse(tag_combined$EXT_J > 0.1, 1, 0)
tag_combined$FRAUD

mean(own_pred1 == tag_combined$FRAUD) # 0.5172414


own_pred1<-ifelse(tag_combined$EXT_J > 0.18, 1, 0)
tag_combined$FRAUD

mean(own_pred1 == tag_combined$FRAUD) # 0,5862
own_pred1[1:28]

own_pred1<-ifelse(tag_combined$EXT_J > 0.2, 1, 0)
tag_combined$FRAUD

mean(own_pred1 == tag_combined$FRAUD)
own_pred1[1:28]

own_pred1<-ifelse(tag_combined$EXT_J > 0.21, 1, 0)
tag_combined$FRAUD

mean(own_pred1 == tag_combined$FRAUD)

own_pred1[1:28]


