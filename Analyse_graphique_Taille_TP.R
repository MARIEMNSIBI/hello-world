#######################################################################################################################################
################################################     GRAPHIQUES     ###################################################################
#######################################################################################################################################

a <- read.table(file="RRBLUP_100_25.txt", header=F)
b <- read.table(file="RRBLUP_100_50.txt", header=F)
c <- read.table(file="RRBLUP_100_60.txt", header=F)
d <- read.table(file="RRBLUP_100_75.txt", header=F)

a<-a[,-1]
b<-b[,-1]
c<-c[,-1]
d<-d[,-1]
colnames(a)<-c("Poids_TP25", "Pression_TP25", "Hues_TP25", "Huefond_TP25","Lchair_TP25","ethylene_TP25","IR_TP25","AT_TP25","Glucose_TP25","Fructose_TP25","Malique_TP25")
colnames(b)<-c("Poids_TP50", "Pression_TP50", "Hues_TP50", "Huefond_TP50","Lchair_TP50","ethylene_TP50","IR_TP50","AT_TP50","Glucose_TP50","Fructose_TP50","Malique_TP50")
colnames(a)<-c("Poids_TP60", "Pression_TP60", "Hues_TP60", "Huefond_TP60","Lchair_TP60","ethylene_TP60","IR_TP60","AT_TP60","Glucose_TP60","Fructose_TP60","Malique_TP60")
colnames(c)<-c("Poids_TP75", "Pression_TP75", "Hues_TP75", "Huefond_TP75","Lchair_TP75","ethylene_TP75","IR_TP75","AT_TP75","Glucose_TP75","Fructose_TP75","Malique_TP75")
nbpheno=11
pdf("Taille.pdf", height=10,width=10) 
par(mfrow=c(3,3))
for (i in c(1:nbpheno)){
  
  accuracy<-cbind(a[,i],b[,i],c[,i],d[,i])
  
  names<-c("Poids", "Pression", "Hues", "Huefond","Lchair","ethylene","IR","AT","Glucose","Fructose","Malique")
  nom<-names[i]
  
  colnames(accuracy)<-c("25%","50%","60%","75%")
  colors=c(rep("#FFCC99",1),rep("#FF9999",1),rep("salmon",1),rep("#CC9999",1))
  boxplot(accuracy,xlab="Pourcentages d'individus formant la population d'entrainement",outline=FALSE, ylab="Précision de la prédiction", main=nom, col=colors,las=1,cex.axis=1)
  legend("bottomright", legend = c("Nombre d'individus","140", "112","93","46"), col = c("white","#CC9999","salmon","#FF9999","#FFCC99"), 
         pch = 15, bty = "M", pt.cex = 2, cex = 0.8, text.col = "#993366", horiz = FALSE, inset = c(0.02, 0.02))
}

dev.off()
#Pour l'éthylène

  
  accuracy<-cbind(a[,6],b[,6],c[,6],d[,6])
  
  names<-c("Poids", "Pression", "Hues", "Huefond","Lchair","ethylene","IR","AT","Glucose","Fructose","Malique")
  nom<-names[i]
  
  colnames(accuracy)<-c("25%","50%","60%","75%")
  colors=c(rep("#FFCC99",1),rep("#FF9999",1),rep("salmon",1),rep("#CC9999",1))
  boxplot(accuracy,xlab="Pourcentages d'individus formant la population d'entrainement", ylab="Précision de la prédiction", main="Ethylène", col=colors,las=1,cex.axis=1)
  legend("bottomright", legend = c("Nombre d'individus","140", "112","93","46"), col = c("white","#CC9999","salmon","#FF9999","#FFCC99"), 
         pch = 15, bty = "M", pt.cex = 2, cex = 0.8, text.col = "#993366", horiz = FALSE, inset = c(0.02, 0.02))

  for (i in 1:11)
  {accuracy<-cbind(a[,i],b[,i],c[,i],d[,i])
  
  names<-c("Poids", "Pression", "Hues", "Huefond","Lchair","ethylene","IR","AT","Glucose","Fructose","Malique")
  nom<-names[i]
  boxplot(accuracy,xlab="Pourcentages d'individus formant la population d'entrainement", ylab="Précision de la prédiction", main=nom, col=colors,las=1,cex.axis=1)
  legend("bottomright", legend = c("Nombre d'individus","140", "112","93","46"), col = c("white","#CC9999","salmon","#FF9999","#FFCC99"), 
         pch = 15, bty = "M", pt.cex = 2, cex = 0.8, text.col = "#993366", horiz = FALSE, inset = c(0.02, 0.02))
}
  #Pour Hue_s
  accuracy<-cbind(a[,3],b[,3],c[,3],d[,3])

  colnames(accuracy)<-c("25%","50%","60%","75%")
  colors=c(rep("#FFCC99",1),rep("#FF9999",1),rep("salmon",1),rep("#CC9999",1))
  boxplot(accuracy,xlab="Pourcentages d'individus formant la population d'entrainement", ylab="Précision de la prédiction", main="Angle de teinte",col=colors,las=1,cex.axis=1.2,cex.lab=1.2,cex.main=1.3)
  legend("bottomright", legend = c("Nombre d'individus","140", "112","93","46"), col = c("white","#CC9999","salmon","#FF9999","#FFCC99"), 
         pch = 15, bty = "M", pt.cex = 2, cex = 0.8, text.col = "#993366", horiz = FALSE, inset = c(0.02, 0.02))
  #Pour Ethylene
  accuracy<-cbind(a[,6],b[,6],c[,6],d[,6])
  
  colnames(accuracy)<-c("25%","50%","60%","75%")
  colors=c(rep("#FFCC99",1),rep("#FF9999",1),rep("salmon",1),rep("#CC9999",1))
  boxplot(accuracy,xlab="Pourcentages d'individus formant la population d'entrainement", ylab="Précision de la prédiction", main="Ethylène",col=colors,las=1,cex.axis=1.2,cex.lab=1.2,cex.main=1.3)
  legend("bottomright", legend = c("Nombre d'individus","140", "112","93","46"), col = c("white","#CC9999","salmon","#FF9999","#FFCC99"), 
         pch = 15, bty = "M", pt.cex = 2, cex = 0.8, text.col = "#993366", horiz = FALSE, inset = c(0.02, 0.02))
  #Pour IR
  accuracy<-cbind(a[,7],b[,7],c[,7],d[,7])
  
  colnames(accuracy)<-c("25%","50%","60%","75%")
  colors=c(rep("#FFCC99",1),rep("#FF9999",1),rep("salmon",1),rep("#CC9999",1))
  boxplot(accuracy,xlab="Pourcentages d'individus formant la population d'entrainement", ylab="Précision de la prédiction", main="IR",col=colors,las=1,cex.axis=1.2,cex.lab=1.2,cex.main=1.3)
  legend("bottomright", legend = c("Nombre d'individus","140", "112","93","46"), col = c("white","#CC9999","salmon","#FF9999","#FFCC99"), 
         pch = 15, bty = "M", pt.cex = 2, cex = 0.8, text.col = "#993366", horiz = FALSE, inset = c(0.02, 0.02))
  #Pour AT
  accuracy<-cbind(a[,8],b[,8],c[,8],d[,8])
  
  colnames(accuracy)<-c("25%","50%","60%","75%")
  colors=c(rep("#FFCC99",1),rep("#FF9999",1),rep("salmon",1),rep("#CC9999",1))
  boxplot(accuracy,xlab="Pourcentages d'individus formant la population d'entrainement", ylab="Précision de la prédiction", main="IR",col=colors,las=1,cex.axis=1.2,cex.lab=1.2,cex.main=1.3)
  legend("bottomright", legend = c("Nombre d'individus","140", "112","93","46"), col = c("white","#CC9999","salmon","#FF9999","#FFCC99"), 
         pch = 15, bty = "M", pt.cex = 2, cex = 0.8, text.col = "#993366", horiz = FALSE, inset = c(0.02, 0.02))

  
  
  #Hues
  accuracy_Hues<-cbind(a[,3],b[,3],c[,3],d[,3])
  colnames(accuracy_Hues)<- c("25%","50%","60%","75%")
  #ethylene
  accuracy_ethylene<-cbind(a[,6],b[,6],c[,6],d[,6])
  colnames(accuracy_ethylene)<- c("25%","50%","60%","75%")
  #IR
  accuracy_IR<-cbind(a[,7],b[,7],c[,7],d[,7])
  colnames(accuracy_IR)<- c("25%","50%","60%","75%")
  #AT
  accuracy_AT<-cbind(a[,8],b[,8],c[,8],d[,8])
  colnames(accuracy_AT)<- c("25%","50%","60%","75%")
  #
  acc<-cbind(accuracy_Hues,accuracy_ethylene,accuracy_IR,accuracy_AT)
  dim(acc)#100  16
  colnames(acc)<-c("Angle de teinte","Angle de teinte","Angle de teinte", "Angle de teinte","Ethylène","Ethylène","Ethylène","Ethylène","IR","IR","IR","IR","AT","AT","AT","AT")
  colors=c(rep("#FFCC99",1),rep("#FF9999",1),rep("salmon",1),rep("#CC9999",1))
  boxplot(acc, ylab="Précision de la prédiction", col=colors,las=3,cex.axis=0.7,cex.lab=1.2,cex.main=1.3,font.axis=3)
  
  legend("bottomright", legend = c("Nombre d'individus","140", "112","93","46"), col = c("white","#FFCC99","#FF9999","salmon","#CC9999"), 
         pch = 15, bty = "M", pt.cex = 1, cex = 0.8, text.col = "#000000", horiz = FALSE, inset = c(0.02, 0.02))  
  
  