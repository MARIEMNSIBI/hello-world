library(plyr)

a <- read.table(file="RRBLUP_75_100.txt", header=F)
b <- read.table(file="RRBLUP_CDmean_75_100.txt", header=F)


a<-a[,-1]
b<-b[,-1]


colnames(a)<-c("Poids", "Pression","Hues","Huefond","Lchair","ethylene","IR","AT","Glucose","Fructose","Malique")
colnames(b)<-c("Poids", "Pression","Hues","Huefond","Lchair","ethylene","IR","AT","Glucose","Fructose","Malique")


accuracy<-rbind.fill(b,a)
accuracy<-cbind(a,b)
head(accuracy)
dim(accuracy)#100  22
#Poids
accuracy_Poids<-accuracy[,c(1,12)]
colnames(accuracy_Poids)<- c("Poids","Poids")
#Pression
accuracy_Pression<-accuracy[,c(2,13)]
colnames(accuracy_Pression)<- c("Pression","Pression")
#Hues
accuracy_Hues<-accuracy[,c(3,14)]
colnames(accuracy_Hues)<- c("Hues","Hues")
#Huefond
accuracy_Huefond<-accuracy[,c(4,15)]
colnames(accuracy_Huefond)<- c("Huefond","Huefond")
#Lchair
accuracy_Lchair<-accuracy[,c(5,16)]
colnames(accuracy_Lchair)<- c("Lchair","Lchair")
#ethylene
accuracy_ethylene<-accuracy[,c(6,17)]
colnames(accuracy_ethylene)<- c("Ethylène","Ethylène")
#IR
accuracy_IR<-accuracy[,c(7,18)]
colnames(accuracy_IR)<- c("IR","IR")
#AT
accuracy_AT<-accuracy[,c(8,19)]
colnames(accuracy_AT)<- c("AT","AT")
#Glucose
accuracy_Glucose<-accuracy[,c(9,20)]
colnames(accuracy_Glucose)<- c("Glucose","Glucose")
#Fructose
accuracy_Fructose<-accuracy[,c(10,21)]
colnames(accuracy_Fructose)<- c("Fructose","Fructose")
#Malique
accuracy_Malique<-accuracy[,c(11,22)]
colnames(accuracy_Malique)<- c("Malique","Malique")
acc<-cbind(accuracy_Poids,accuracy_Pression,accuracy_Hues,accuracy_Huefond,accuracy_Lchair,accuracy_ethylene,accuracy_IR,accuracy_AT,accuracy_Glucose,accuracy_Fructose,accuracy_Malique)
dim(acc)#100  22
pdf("Optimisation_CDmean.pdf", height=10,width=10) 
names(acc)<-c("Poids du fruit", "Poids du fruit","Pression", "Pression", "Hue_s","Hue_s", "Hue_fond","Hue_fond","L_chair","L_chair","Ethylène","Ethylène","IR","IR","AT","AT","Glucose","Glucose","Fructose","Fructose","Acide malique","Acide malique")
  colors=c("#FF9999","#66CC99")
  boxplot(acc, ylab="Précision de la prédiction", col=colors,las=3,cex.axis=0.7)
  
  legend("bottomright", legend = c("Random","CDmean"), col = c("#FF9999","#66CC99"), 
         pch = 15, bty = "m", pt.cex = 1, cex = 0.8, text.col = "#000000", horiz = FALSE, inset = c(0.02, 0.02))
  dev.off()
  acc<-cbind(accuracy_Hues,accuracy_ethylene,accuracy_IR,accuracy_AT)
  names(acc)<-c("Angle de teinte", "Angle de teinte", "Ethylène", "Ethylène", "IR", "IR", "AT", "AT")
  boxplot(acc, ylab="Précision de la prédiction", col=colors,las=3,cex.axis=0.7,font.lab=2)
############################################################
#################### TESTS STATS ###########################
############################################################
#Glucose
#Rassemblement des Accuracies dans le mÃƒÂªme vecteur 
Accuracy<-c(m[,5], l[,5], k[,5],j[,5],i[,5],h[,5],g[,5],f[,5],e[,5],d[,5],c[,5],b[,5],a[,5])
length(Accuracy)

# Colonne aves le nom des paramètres
vector_a<-rep("10",10)
vector_b<-rep("20",20)
vector_c<-rep("50",50)
vector_d<-rep("100",100)
vector_e<-rep("200",200)
vector_f<-rep("300",300)
vector_g<-rep("400",400)
vector_h<-rep("500",500)
vector_i<-rep("600",600)
vector_j<-rep("700",700)
vector_k<-rep("800",800)
vector_l<-rep("900",900)
vector_m<-rep("1000",1000)

Parametres<-c(vector_a,vector_b,vector_c,vector_d,vector_e,vector_f,vector_g,vector_h,vector_i,vector_j,vector_k,vector_l,vector_m)
length(Parametres)

# Créer le dataframe

dataframe <- data.frame(Parametres = Parametres, Accuracy=Accuracy)
dataframe[1:5,1:2]

############ TUKEY ###########################
a1 <- aov(dataframe$Accuracy ~ dataframe$Parametres)
TukeyHSD_Parametres <- TukeyHSD(x=a1, 'dataframe$Parametres', conf.level=0.95)

test <- lm(dataframe$Accuracy ~ dataframe$Parametres)
library(agricolae)
HSD_para<-HSD.test(test, 'dataframe$Parametres')
HSD_para
########### TEST STATS ##########################
tests = matrix(nrow=14, ncol=13)

for(o in c(1:13)){
  
  if (o==1) z=a else #1000itérations
    if (o==2) z=b else #900itérations
      if (o==3) z=c else #800itérations
        if (o==4) z=d else #700itérations
          if (o==5) z=e else #600itérations
            if (o==6) z=f else #500itérations
              if (o==7) z=g else #400itérations
                if (o==8) z=h else #300itérations
                  if (o==9) z=i else #200itérations
                   if (o==10) z=j else #100itérations  
                    if (o==11) z=k else #50itérations
                      if (o==12) z=l else #20itérations
                        if (o==13) z=m     #10itérations
                        
                        t.test_Poids<-t.test(z[,1],j[,1])
                        ks.test_Poids<-ks.test(z[,1],j[,1])
                        t.test_Pression<-t.test(z[,2],j[,2])
                        ks.test_Pression<-ks.test(z[,2],j[,2])
                        t.test_IR<-t.test(z[,3],j[,3])
                        ks.test_IR<-ks.test(z[,3],j[,3])
                        t.test_AT<-t.test(z[,4],j[,4])
                        ks.test_AT<-ks.test(z[,4],j[,4])
                        t.test_Glucose<-t.test(z[,5],j[,5])
                        ks.test_Glucose<-ks.test(z[,5],j[,5])
                        t.test_Fructose<-t.test(z[,6],j[,6])
                        ks.test_Fructose<-ks.test(z[,6],j[,6])
                        t.test_Malique<-t.test(z[,7],j[,7])
                        ks.test_Malique<-ks.test(z[,7],j[,7])
                        
                        tests[1,o]<-t.test_Poids$p.value
                        tests[2,o]<-ks.test_Poids$p.value 
                        tests[3,o]<-t.test_Pression$p.value
                        tests[4,o]<-ks.test_Pression$p.value
                        tests[5,o]<-t.test_IR$p.value
                        tests[6,o]<-ks.test_IR$p.value 
                        tests[7,o]<-t.test_AT$p.value
                        tests[8,o]<-ks.test_AT$p.value 
                        tests[9,o]<-t.test_Glucose$p.value
                        tests[10,o]<-ks.test_Glucose$p.value 
                        tests[11,o]<-t.test_Fructose$p.value
                        tests[12,o]<-ks.test_Fructose$p.value
                        tests[13,o]<-t.test_Malique$p.value
                        tests[14,o]<-ks.test_Malique$p.value
}
colnames(tests)<-c("1000","900","800","700","600","500","400","300","200", "100", "50", "20","10")
rownames(tests)<-c("t.test_Poids","ks.test_Poids","t.test_Pression","ks.test_Pression","t.test_IR","ks.test_IR","t.test_AT","ks.test_AT","t.test_Glucose","ks.test_Glucose","t.test_Fructose","ks.test_Fructose","t.test_Malique","ks.test_Malique")
write.table(tests, "Tests nb de cycles (t test and ks test).txt", sep=" ", quote=F, col.names=T, row.names=T)
write.csv2(tests, "Tests nb de cycles (t test and ks test).csv")
