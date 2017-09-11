##Markers
data1 <- as.matrix(read.table("FruitSelGen-apricot-PRPER-Lovell-v2-avignon_GoxMo_raw-snps_gatk-filter.txt"))
data1[data1==0] <- -1
data1[data1==1] <- 0
data1[data1==2] <- 1
Markers<-t(data1)
dim(Markers)#61030
##Phenotypes
Pheno<-read.table("Qualitéajustée.txt", header=TRUE, sep="\t")
dim(Pheno)#11
for(i in 1:11)
{Pheno[,i]<-as.numeric(as.character(Pheno[,i]))}
noms_caracteres<-names(Pheno)
Pheno <- as.matrix(Pheno)
## Calculates the realized additive relationship matrix.
#impute missing markers with A.mat
library(rrBLUP)
impute=A.mat(Markers,impute.method="mean",return.imputed=T)
#Imputed markers matrix
Markers_impute=impute$imputed
dim(Markers_impute)#32919
#Additive relationship matrix
impute$A
#define the training and test populations
#training-75% validation-25% (pop totale =186)
out_marker_density = NULL
traits=11
cycles=100
accuracy = matrix(nrow=cycles, ncol=traits)
for(i in c(6103,12206,18309,24412,30515,36618,42721,48824,54927, 61030)){  
for(r in 1:cycles) {
  for(n in c(1:traits)) 
  {
    pick <- as.matrix(sample(1:61030,i))
    pick_M = Markers[,pick]
    pick_M = Markers[,pick]
    impute=A.mat(pick_M,impute.method="mean",return.imputed=T)
    Markers_impute=impute$imputed
    train= as.matrix(sample(1:186, 140))
    test<-setdiff(1:186,train)
    Pheno_train=Pheno[train,]
    m_train=Markers_impute[train,]
    Pheno_valid=Pheno[test,]
    m_valid=Markers_impute[test,]
    
    y= Pheno_train[,n]
    y_answer<-mixed.solve(y, Z=m_train, K=NULL, SE = FALSE, return.Hinv=FALSE)
    
    p = y_answer$u
    e = as.matrix(p)
    pred_y_valid =  m_valid %*% e
    pred_y=(pred_y_valid[,1]) + y_answer$beta
    y_valid = Pheno_valid[,n]
    accuracy[r,n] <-cor(pred_y_valid, y_valid, use="complete" )
    colnames(accuracy)<-colnames(Pheno)
  }
}
  out <- data.frame(rep(i,cycles),accuracy)
  
  names(out) <- c("Markers_number","Poids","Pression","Hues","Huefond",
                       "Lchair","ethylene","IR","AT","Glucose","Fructose","Malique")
  
  out_marker_density <- rbind(out_marker_density,out)
}
colnames(out) <- c("Markers_number","Poids","Pression","Hues","Huefond",
                "Lchair","ethylene","IR","AT","Glucose","Fructose","Malique")

write.table(out_marker_density, "RRBLUP_100_75_Nbre_M.txt", sep=" ", quote=F, col.names=F, row.names=T)

