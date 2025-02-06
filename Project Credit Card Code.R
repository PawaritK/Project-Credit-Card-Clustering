library(readxl)
CC_GENERAL <- read_csv("C:/Users/User/Downloads/CC_GENERAL.csv")

CC = data.frame(CC_GENERAL)

CC_scale = scale(CC[,-1], center = TRUE, scale = TRUE)

library(factoextra)

fviz_nbclust(CC_scale, FUN = hcut, method = "wss")
fviz_nbclust(CC_scale, FUN = hcut, method = "silhouette")
fviz_nbclust(CC_scale, FUN = hcut, method = "gap_stat")


k7 = kmeans(CC_scale, centers = 7)
fviz_cluster(k7, data = CC_scale)

T2 = kmeans(CC_scale, centers = 2)
fviz_cluster(T2, data = CC_scale)

T9 = kmeans(CC_scale, centers = 9)
fviz_cluster(T9, data = CC_scale)



k7 = kmeans(CC_scale, centers = 7)
fviz_cluster(k7, data = CC_scale)

CC_clus <- data.frame(CC_scale, cluster_7 = k7$cluster)
round(k7$centers, 7)
round(T9$centers, 9)

###################

CC2 = data.frame(CC[,-1])

CC2_dist <- dist(CC2, method = "euclidean")

library(vegan)
CC2_dist <- vegdist(CC2, method = "chisq")

library(stats)
Ch1 = hclust(CC2_dist, method = "single" )
plot(Ch1, cex = 0.6, hang = -1)
rect.hclust(Ch1, k = 7, border = 2:5)
sub_grp_3 <- cutree(Ch1, k = 7)

Ch2 = hclust(CC2_dist, method = "complete" )
plot(Ch2, cex = 0.6, hang = -1)
rect.hclust(Ch2, k = 7, border = 2:5)
sub_grp_4 <- cutree(Ch2, k = 7)

Ch2_cluss <- cbind(CC2, c2 = sub_grp_4)
summary(aov(cbind(BALANCE,BALANCE_FREQUENCY,PURCHASES,ONEOFF_PURCHASES,INSTALLMENTS_PURCHASES,CASH_ADVANCE,
                  PURCHASES_FREQUENCY,ONEOFF_PURCHASES_FREQUENCY,PURCHASES_INSTALLMENTS_FREQUENCY,CASH_ADVANCE_FREQUENCY,CASH_ADVANCE_TRX
                  ,PURCHASES_TRX,CREDIT_LIMIT,PAYMENTS,MINIMUM_PAYMENTS,PRC_FULL_PAYMENT,TENURE) ~ c2, data = Ch2_cluss))

frameCh2_cluss = data.frame(Ch2_cluss)
Ch2d <- describe(cbind(BALANCE,BALANCE_FREQUENCY,PURCHASES,ONEOFF_PURCHASES,INSTALLMENTS_PURCHASES,CASH_ADVANCE,
                       PURCHASES_FREQUENCY,ONEOFF_PURCHASES_FREQUENCY,PURCHASES_INSTALLMENTS_FREQUENCY,CASH_ADVANCE_FREQUENCY,CASH_ADVANCE_TRX
                       ,PURCHASES_TRX,CREDIT_LIMIT,PAYMENTS,MINIMUM_PAYMENTS,PRC_FULL_PAYMENT,TENURE) ~ c2, data = Ch2_cluss)


Ch3 = hclust(CC2_dist, method = "average" )
plot(Ch3, cex = 0.6, hang = -1)
rect.hclust(Ch3, k = 7, border = 2:5)
sub_grp_5 <- cutree(Ch3, k = 7)

Ch4 = hclust(CC2_dist, method = "ward.D" )
plot(Ch4, cex = 0.6, hang = -1)
rect.hclust(Ch4, k = 7, border = 2:5)
sub_grp_6 <- cutree(Ch4, k = 7)

Ch5 = hclust(CC2_dist, method = "ward.D2" )
plot(Ch5, cex = 0.6, hang = -1)
rect.hclust(Ch5, k = 7, border = 2:5)
sub_grp_6 <- cutree(Ch5, k = 7)


#######################

Ch4 = hclust(CC2_dist, method = "ward.D" )
plot(Ch4, cex = 0.6, hang = -1)
rect.hclust(Ch4, k = 7, border = 2:5)
sub_grp_7 <- cutree(Ch4, k = 7)

CC_cluss <- cbind(CC2, cluster__7 = sub_grp_7)
summary(aov(cbind(BALANCE,BALANCE_FREQUENCY,PURCHASES,ONEOFF_PURCHASES,INSTALLMENTS_PURCHASES,CASH_ADVANCE,
                  PURCHASES_FREQUENCY,ONEOFF_PURCHASES_FREQUENCY,PURCHASES_INSTALLMENTS_FREQUENCY,CASH_ADVANCE_FREQUENCY,CASH_ADVANCE_TRX
                  ,PURCHASES_TRX,CREDIT_LIMIT,PAYMENTS,MINIMUM_PAYMENTS,PRC_FULL_PAYMENT,TENURE) ~ cluster__7, data = CC_cluss))

library(psych)
frameCC2_cluss = data.frame(CC_cluss)
Cds <- describe(cbind(BALANCE,BALANCE_FREQUENCY,PURCHASES,ONEOFF_PURCHASES,INSTALLMENTS_PURCHASES,CASH_ADVANCE,
                      PURCHASES_FREQUENCY,ONEOFF_PURCHASES_FREQUENCY,PURCHASES_INSTALLMENTS_FREQUENCY,CASH_ADVANCE_FREQUENCY,CASH_ADVANCE_TRX
                      ,PURCHASES_TRX,CREDIT_LIMIT,PAYMENTS,MINIMUM_PAYMENTS,PRC_FULL_PAYMENT,TENURE) ~ cluster__7, data = CC_cluss)

meanCC <- rbind(Cds$'1'[,7], Cds$'2'[,7], Cds$'3'[,7], Cds$'4'[,7], Cds$'5'[,7],Cds$'6'[,7],Cds$'7'[,7])

colnames(meanCC) <- c("BALANCE","BALANCE_FREQUENCY","PURCHASES","ONEOFF_PURCHASES","INSTALLMENTS_PURCHASES","CASH_ADVANCE",
                    "PURCHASES_FREQUENCY","ONEOFF_PURCHASES_FREQUENCY","PURCHASES_INSTALLMENTS_FREQUENCY","CASH_ADVANCE_FREQUENCY","CASH_ADVANCE_TRX"
                    ,"PURCHASES_TRX","CREDIT_LIMIT","PAYMENTS","MINIMUM_PAYMENTS","PRC_FULL_PAYMENT","TENURE")

rownames(meanCC) <- c("cluster 1", "cluster 2", "cluster 3", "cluster 4", "cluster 5", "cluster 6", "cluster 7")


############################

Ch1 = hclust(CC2_dist, method = "single" )
plot(Ch1, cex = 0.6, hang = -1)
rect.hclust(Ch1, k = 7, border = 2:5)
sub_grp_3 <- cutree(Ch1, k = 7)

Ch1_cluss <- cbind(CC2, c1 = sub_grp_3)
summary(aov(cbind(BALANCE,BALANCE_FREQUENCY,PURCHASES,ONEOFF_PURCHASES,INSTALLMENTS_PURCHASES,CASH_ADVANCE,
                  PURCHASES_FREQUENCY,ONEOFF_PURCHASES_FREQUENCY,PURCHASES_INSTALLMENTS_FREQUENCY,CASH_ADVANCE_FREQUENCY,CASH_ADVANCE_TRX
                  ,PURCHASES_TRX,CREDIT_LIMIT,PAYMENTS,MINIMUM_PAYMENTS,PRC_FULL_PAYMENT,TENURE) ~ c1, data = Ch1_cluss))

frameCh1_cluss = data.frame(Ch1_cluss)
Ch1d <- describe(cbind(BALANCE,BALANCE_FREQUENCY,PURCHASES,ONEOFF_PURCHASES,INSTALLMENTS_PURCHASES,CASH_ADVANCE,
                      PURCHASES_FREQUENCY,ONEOFF_PURCHASES_FREQUENCY,PURCHASES_INSTALLMENTS_FREQUENCY,CASH_ADVANCE_FREQUENCY,CASH_ADVANCE_TRX
                      ,PURCHASES_TRX,CREDIT_LIMIT,PAYMENTS,MINIMUM_PAYMENTS,PRC_FULL_PAYMENT,TENURE) ~ c1, data = Ch1_cluss)


#################################

Ch5 = hclust(CC2_dist, method = "ward.D2" )
plot(Ch5, cex = 0.6, hang = -1)
rect.hclust(Ch5, k = 7, border = 2:5)
sub_grp_6 <- cutree(Ch5, k = 7)

Ch5_cluss <- cbind(CC2, c5 = sub_grp_6)
summary(aov(cbind(BALANCE,BALANCE_FREQUENCY,PURCHASES,ONEOFF_PURCHASES,INSTALLMENTS_PURCHASES,CASH_ADVANCE,
                  PURCHASES_FREQUENCY,ONEOFF_PURCHASES_FREQUENCY,PURCHASES_INSTALLMENTS_FREQUENCY,CASH_ADVANCE_FREQUENCY,CASH_ADVANCE_TRX
                  ,PURCHASES_TRX,CREDIT_LIMIT,PAYMENTS,MINIMUM_PAYMENTS,PRC_FULL_PAYMENT,TENURE) ~ c5, data = Ch5_cluss))

frameCh5_cluss = data.frame(Ch5_cluss)
Ch1d <- describe(cbind(BALANCE,BALANCE_FREQUENCY,PURCHASES,ONEOFF_PURCHASES,INSTALLMENTS_PURCHASES,CASH_ADVANCE,
                       PURCHASES_FREQUENCY,ONEOFF_PURCHASES_FREQUENCY,PURCHASES_INSTALLMENTS_FREQUENCY,CASH_ADVANCE_FREQUENCY,CASH_ADVANCE_TRX
                       ,PURCHASES_TRX,CREDIT_LIMIT,PAYMENTS,MINIMUM_PAYMENTS,PRC_FULL_PAYMENT,TENURE) ~ c5, data = Ch5_cluss)

#######################################
Ch1 = hclust(CC2_dist, method = "single" )
plot(Ch1, cex = 0.6, hang = -1)
rect.hclust(Ch1, k = 7, border = 2:5)
sub_grp_3 <- cutree(Ch1, k = 7)

Ch1_cluss <- cbind(CC2, c1 = sub_grp_3)
summary(aov(cbind(BALANCE,BALANCE_FREQUENCY,PURCHASES,ONEOFF_PURCHASES,INSTALLMENTS_PURCHASES,CASH_ADVANCE,
                  PURCHASES_FREQUENCY,ONEOFF_PURCHASES_FREQUENCY,PURCHASES_INSTALLMENTS_FREQUENCY,CASH_ADVANCE_FREQUENCY,CASH_ADVANCE_TRX
                  ,PURCHASES_TRX,CREDIT_LIMIT,PAYMENTS,MINIMUM_PAYMENTS,PRC_FULL_PAYMENT,TENURE) ~ c1, data = Ch1_cluss))

frameCh1_cluss = data.frame(Ch1_cluss)
Ch1d <- describe(cbind(BALANCE,BALANCE_FREQUENCY,PURCHASES,ONEOFF_PURCHASES,INSTALLMENTS_PURCHASES,CASH_ADVANCE,
                      PURCHASES_FREQUENCY,ONEOFF_PURCHASES_FREQUENCY,PURCHASES_INSTALLMENTS_FREQUENCY,CASH_ADVANCE_FREQUENCY,CASH_ADVANCE_TRX
                      ,PURCHASES_TRX,CREDIT_LIMIT,PAYMENTS,MINIMUM_PAYMENTS,PRC_FULL_PAYMENT,TENURE) ~ c1, data = Ch1_cluss)


###########################

Ch4 = hclust(CC2_dist, method = "ward.D" )
plot(Ch4, cex = 0.6, hang = -1)
rect.hclust(Ch4, k = 7, border = 2:5)
sub_grp_7 <- cutree(Ch4, k = 7)

CC_cluss <- cbind(CC2, cluster__7 = sub_grp_7)
summary(aov(cbind(BALANCE,BALANCE_FREQUENCY,PURCHASES,ONEOFF_PURCHASES,INSTALLMENTS_PURCHASES,CASH_ADVANCE,
                  PURCHASES_FREQUENCY,ONEOFF_PURCHASES_FREQUENCY,PURCHASES_INSTALLMENTS_FREQUENCY,CASH_ADVANCE_FREQUENCY,CASH_ADVANCE_TRX
                  ,PURCHASES_TRX,CREDIT_LIMIT,PAYMENTS,MINIMUM_PAYMENTS,PRC_FULL_PAYMENT,TENURE) ~ cluster__7, data = CC_cluss))

library(psych)
frameCC2_cluss = data.frame(CC_cluss)
Cds <- describe(cbind(BALANCE,BALANCE_FREQUENCY,PURCHASES,ONEOFF_PURCHASES,INSTALLMENTS_PURCHASES,CASH_ADVANCE,
                      PURCHASES_FREQUENCY,ONEOFF_PURCHASES_FREQUENCY,PURCHASES_INSTALLMENTS_FREQUENCY,CASH_ADVANCE_FREQUENCY,CASH_ADVANCE_TRX
                      ,PURCHASES_TRX,CREDIT_LIMIT,PAYMENTS,MINIMUM_PAYMENTS,PRC_FULL_PAYMENT,TENURE) ~ cluster__7, data = CC_cluss)

meanCC <- rbind(Cds$'1'[,7], Cds$'2'[,7], Cds$'3'[,7], Cds$'4'[,7], Cds$'5'[,7],Cds$'6'[,7],Cds$'7'[,7])

colnames(meanCC) <- c("BALANCE","BALANCE_FREQUENCY","PURCHASES","ONEOFF_PURCHASES","INSTALLMENTS_PURCHASES","CASH_ADVANCE",
                    "PURCHASES_FREQUENCY","ONEOFF_PURCHASES_FREQUENCY","PURCHASES_INSTALLMENTS_FREQUENCY","CASH_ADVANCE_FREQUENCY","CASH_ADVANCE_TRX"
                    ,"PURCHASES_TRX","CREDIT_LIMIT","PAYMENTS","MINIMUM_PAYMENTS","PRC_FULL_PAYMENT","TENURE")

rownames(meanCC) <- c("cluster 1", "cluster 2", "cluster 3", "cluster 4", "cluster 5", "cluster 6", "cluster 7")


############################

#Ch1 = hclust(CC2_dist, method = "single" )
#plot(Ch1, cex = 0.6, hang = -1)
#rect.hclust(Ch1, k = 7, border = 2:5)
#sub_grp_3 <- cutree(Ch1, k = 7)

#Ch1_cluss <- cbind(CC2, c1 = sub_grp_3)
#summary(aov(cbind(BALANCE,BALANCE_FREQUENCY,PURCHASES,ONEOFF_PURCHASES,INSTALLMENTS_PURCHASES,CASH_ADVANCE,
                 # PURCHASES_FREQUENCY,ONEOFF_PURCHASES_FREQUENCY,PURCHASES_INSTALLMENTS_FREQUENCY,CASH_ADVANCE_FREQUENCY,CASH_ADVANCE_TRX
                  #,PURCHASES_TRX,CREDIT_LIMIT,PAYMENTS,MINIMUM_PAYMENTS,PRC_FULL_PAYMENT,TENURE) ~ c1, data = Ch1_cluss))

#frameCh1_cluss = data.frame(Ch1_cluss)
#Ch1d <- describe(cbind(BALANCE,BALANCE_FREQUENCY,PURCHASES,ONEOFF_PURCHASES,INSTALLMENTS_PURCHASES,CASH_ADVANCE,
                      #PURCHASES_FREQUENCY,ONEOFF_PURCHASES_FREQUENCY,PURCHASES_INSTALLMENTS_FREQUENCY,CASH_ADVANCE_FREQUENCY,CASH_ADVANCE_TRX
                      #,PURCHASES_TRX,CREDIT_LIMIT,PAYMENTS,MINIMUM_PAYMENTS,PRC_FULL_PAYMENT,TENURE) ~ c1, data = Ch1_cluss)

###################

Ch4 = hclust(CC2_dist, method = "ward.D" )
plot(Ch4, cex = 0.6, hang = -1)
rect.hclust(Ch4, k = 7, border = 2:5)
sub_grp_6 <- cutree(Ch4, k = 6)

CC6_cluss <- cbind(CC2, cluster__6 = sub_grp_6)
summary(aov(cbind(BALANCE,BALANCE_FREQUENCY,PURCHASES,ONEOFF_PURCHASES,INSTALLMENTS_PURCHASES,CASH_ADVANCE,
                  PURCHASES_FREQUENCY,ONEOFF_PURCHASES_FREQUENCY,PURCHASES_INSTALLMENTS_FREQUENCY,CASH_ADVANCE_FREQUENCY,CASH_ADVANCE_TRX
                  ,PURCHASES_TRX,CREDIT_LIMIT,PAYMENTS,MINIMUM_PAYMENTS,PRC_FULL_PAYMENT,TENURE) ~ cluster__6, data = CC6_cluss))

library(psych)
frameCC6_cluss = data.frame(CC6_cluss)
Cds6 <- describe(cbind(BALANCE,BALANCE_FREQUENCY,PURCHASES,ONEOFF_PURCHASES,INSTALLMENTS_PURCHASES,CASH_ADVANCE,
                      PURCHASES_FREQUENCY,ONEOFF_PURCHASES_FREQUENCY,PURCHASES_INSTALLMENTS_FREQUENCY,CASH_ADVANCE_FREQUENCY,CASH_ADVANCE_TRX
                      ,PURCHASES_TRX,CREDIT_LIMIT,PAYMENTS,MINIMUM_PAYMENTS,PRC_FULL_PAYMENT,TENURE) ~ cluster__6, data = CC6_cluss)

meanCC6 <- rbind(Cds6$'1'[,7], Cds6$'2'[,7], Cds6$'3'[,7], Cds6$'4'[,7], Cds6$'5'[,7],Cds6$'6'[,7])

colnames(meanCC6) <- c("BALANCE","BALANCE_FREQUENCY","PURCHASES","ONEOFF_PURCHASES","INSTALLMENTS_PURCHASES","CASH_ADVANCE",
                      "PURCHASES_FREQUENCY","ONEOFF_PURCHASES_FREQUENCY","PURCHASES_INSTALLMENTS_FREQUENCY","CASH_ADVANCE_FREQUENCY","CASH_ADVANCE_TRX"
                      ,"PURCHASES_TRX","CREDIT_LIMIT","PAYMENTS","MINIMUM_PAYMENTS","PRC_FULL_PAYMENT","TENURE")

rownames(meanCC6) <- c("cluster 1", "cluster 2", "cluster 3", "cluster 4", "cluster 5", "cluster 6")


Ch41 = hclust(CC2_dist, method = "ward.D" )
plot(Ch41, cex = 0.6, hang = -1)
rect.hclust(Ch41, k = 10, border = 2:5)
sub_grp_10 <- cutree(Ch41, k = 10)

CC10_cluss <- cbind(CC2, cluster__10 = sub_grp_10)
summary(aov(cbind(BALANCE,BALANCE_FREQUENCY,PURCHASES,ONEOFF_PURCHASES,INSTALLMENTS_PURCHASES,CASH_ADVANCE,
                  PURCHASES_FREQUENCY,ONEOFF_PURCHASES_FREQUENCY,PURCHASES_INSTALLMENTS_FREQUENCY,CASH_ADVANCE_FREQUENCY,CASH_ADVANCE_TRX
                  ,PURCHASES_TRX,CREDIT_LIMIT,PAYMENTS,MINIMUM_PAYMENTS,PRC_FULL_PAYMENT,TENURE) ~ cluster__10, data = CC10_cluss))

library(psych)
frameCC10_cluss = data.frame(CC10_cluss)
Cds10 <- describe(cbind(BALANCE,PURCHASES,ONEOFF_PURCHASES,INSTALLMENTS_PURCHASES,
                       PURCHASES_FREQUENCY,ONEOFF_PURCHASES_FREQUENCY,PURCHASES_INSTALLMENTS_FREQUENCY,CASH_ADVANCE_FREQUENCY
                       ,PURCHASES_TRX,PAYMENTS,MINIMUM_PAYMENTS,PRC_FULL_PAYMENT,TENURE) ~ cluster__10, data = CC10_cluss)

meanCC10 <- rbind(Cds10$'1'[,10], Cds10$'2'[,10], Cds10$'3'[,10], Cds10$'4'[,10], Cds10$'5'[,10], Cds10$'6'[,10], Cds10$'7'[,10], Cds10$'8'[,10], Cds10$'9'[,10], Cds10$'10'[,10])

colnames(meanCC10) <- c("BALANCE","PURCHASES","ONEOFF_PURCHASES","INSTALLMENTS_PURCHASES",
                       "PURCHASES_FREQUENCY","ONEOFF_PURCHASES_FREQUENCY","PURCHASES_INSTALLMENTS_FREQUENCY","CASH_ADVANCE_FREQUENCY"
                       ,"PURCHASES_TRX","PAYMENTS","MINIMUM_PAYMENTS","PRC_FULL_PAYMENT","TENURE")

rownames(meanCC10) <- c("cluster 1", "cluster 2", "cluster 3", "cluster 4", "cluster 5", "cluster 6", "cluster 7", "cluster 8","cluster 9","cluster 10")
