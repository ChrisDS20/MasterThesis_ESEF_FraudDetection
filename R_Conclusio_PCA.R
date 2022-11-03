

##### Conclusio - PCA Grafiken:
library(ggfortify)
library(plotly)
#############################################################


# PCA von dim(final_data_relevant_1_v2)

dim(final_data_relevant_1_v2)

str(final_data_relevant_1_v2)


final_data_relevant_1_v2.pca <- prcomp(final_data_relevant_1_v2[,c(2:27)],
                                       center = TRUE,
                                       scale. = TRUE)


final_data_relevant_1_v2.pca$x

#summary(final_data_relevant_1_v2.pca)


#final_data_relevant_1_v2$FRAUD_1_NONFRAUD_0


f_v2.pca.plot <- autoplot(final_data_relevant_1_v2.pca,
                          data = final_data_relevant_1_v2,
                          colour = 'FRAUD_1_NONFRAUD_0')

f_v2.pca.plot




library(plotly)

pca_f_v2<-final_data_relevant_1_v2

summary(pca_f_v2)

pca_f_v2$FRAUD_1_NONFRAUD_0 <- as.numeric(as.character(pca_f_v2$FRAUD_1_NONFRAUD_0))
pca_f_v2[is.na(pca_f_v2)] <- 0

class(pca_f_v2$FRAUD_1_NONFRAUD_0)

pca_f_v2$FRAUD_1_NONFRAUD_0[which(pca_f_v2$FRAUD_1_NONFRAUD_0 == 0)] <- 'Nicht-Betrug'
pca_f_v2$FRAUD_1_NONFRAUD_0[which(pca_f_v2$FRAUD_1_NONFRAUD_0 == 1)] <- 'Betrug'
pca_f_v2$FRAUD_1_NONFRAUD_0 <- as.factor(pca_f_v2$FRAUD_1_NONFRAUD_0)


summary(final_data_relevant_1_v2.pca)
class(final_data_relevant_1_v2.pca)

class(final_data_relevant_1_v2.pca$x)

pca_df<- as.data.frame(final_data_relevant_1_v2.pca$x)

class(pca_df)
dim(pca_df)
pca_df$FRAUD_1_NONFRAUD_0 <- pca_f_v2$FRAUD_1_NONFRAUD_0

fig <- plot_ly(pca_df, x = ~PC1, y = ~PC2, z = ~PC3, color = ~FRAUD_1_NONFRAUD_0, colors = c('#BF382A', '#0C4B8E'))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'PC1'),
                                   yaxis = list(title = 'PC2'),
                                   zaxis = list(title = 'PC3')))

fig


plot(pca_df$PC1, pca_df$PC2 )
colors <- c("red", # Orange
            "green" # Light green
) 

plot(pca_df$PC1, pca_df$PC2, pch=10, 
     xlim=c(-4, 3), ylim=c(-5, 5), xlab="PC1: 23,7%", ylab="PC2: 13,2%",
     col = colors[factor(pca_df$FRAUD_1_NONFRAUD_0)], main="Datensatz mit nicht signifikanten Variablen")


plot(pca_df$PC1, pca_df$PC2, pch=10, 
     xlim=c(-22, 2), ylim=c(-5, 5), xlab="PC1: 23,7%", ylab="PC2: 13,2%",
     col = colors[factor(pca_df$FRAUD_1_NONFRAUD_0)], main="Datensatz mit nicht signifikanten Variablen")


# Legend
legend("topleft",
       legend = c("Betrug", "Kein Betrug"),
       pch = 19,
       col = colors)




par(new=TRUE)
plot(a,b, pch=2, col="red", xlim=c(-3, 5), ylim=c(-4,5), xlab="x-Variable", ylab="y-Variable")
par(new=TRUE)
rect(0, 1, 3,5)





##########################################################

# PCA von dim(final_data_relevant_1_v4)

dim(final_data_relevant_1_v4)

str(final_data_relevant_1_v4)


final_data_relevant_1_v4.pca <- prcomp(final_data_relevant_1_v4[,c(2:13)],
                                       center = TRUE,
                                       scale. = TRUE)


final_data_relevant_1_v2.pca$x

#summary(final_data_relevant_1_v2.pca)


#final_data_relevant_1_v2$FRAUD_1_NONFRAUD_0

library(ggfortify)
f_v4.pca.plot <- autoplot(final_data_relevant_1_v4.pca,
                          data = final_data_relevant_1_v4,
                          colour = 'FRAUD_1_NONFRAUD_0')

f_v4.pca.plot






pca_f_v4<-final_data_relevant_1_v4

summary(pca_f_v4)

pca_f_v4$FRAUD_1_NONFRAUD_0 <- as.numeric(as.character(pca_f_v4$FRAUD_1_NONFRAUD_0))
pca_f_v4[is.na(pca_f_v4)] <- 0

class(pca_f_v4$FRAUD_1_NONFRAUD_0)

pca_f_v4$FRAUD_1_NONFRAUD_0[which(pca_f_v4$FRAUD_1_NONFRAUD_0 == 0)] <- 'Nicht-Betrug'
pca_f_v4$FRAUD_1_NONFRAUD_0[which(pca_f_v4$FRAUD_1_NONFRAUD_0 == 1)] <- 'Betrug'
pca_f_v4$FRAUD_1_NONFRAUD_0 <- as.factor(pca_f_v4$FRAUD_1_NONFRAUD_0)


summary(final_data_relevant_1_v2.pca)
class(final_data_relevant_1_v2.pca)

class(final_data_relevant_1_v2.pca$x)

pca_df<- as.data.frame(final_data_relevant_1_v4.pca$x)

class(pca_df)
dim(pca_df)
pca_df$FRAUD_1_NONFRAUD_0 <- pca_f_v4$FRAUD_1_NONFRAUD_0

fig <- plot_ly(pca_df, x = ~PC1, y = ~PC2, z = ~PC3, color = ~FRAUD_1_NONFRAUD_0, colors = c('#BF382A', '#0C4B8E'))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'PC1'),
                                   yaxis = list(title = 'PC2'),
                                   zaxis = list(title = 'PC3')))

fig


plot(pca_df$PC1, pca_df$PC2 )
colors <- c("red", # Orange
            "green" # Light green
) 

plot(pca_df$PC1, pca_df$PC2, pch=10, 
     xlim=c(-4, 2), ylim=c(-11, 4), xlab="PC1: 20,06%", ylab="PC2: 15,24%",
     col = colors[factor(pca_df$FRAUD_1_NONFRAUD_0)], main="Datensatz mit signifikanten Variablen")


plot(pca_df$PC1, pca_df$PC2, pch=10, 
     xlim=c(-4,2), ylim=c(-5, 5), xlab="PC1: 20,06%", ylab="PC2: 15,24%",
     col = colors[factor(pca_df$FRAUD_1_NONFRAUD_0)], main="Datensatz mit nicht signifikanten Variablen")


# Legend
legend("topleft",
       legend = c("Betrug", "Kein Betrug"),
       pch = 19,
       col = colors)




par(new=TRUE)
plot(a,b, pch=2, col="red", xlim=c(-3, 5), ylim=c(-4,5), xlab="x-Variable", ylab="y-Variable")
par(new=TRUE)
rect(0, 1, 3,5)