#Nombre d'itérations
##Markers
data1 <- as.matrix(read.table("FruitSelGen-apricot-PRPER-Lovell-v2-avignon_GoxMo_raw-snps_gatk-filter.txt"))
data1[data1==0] <- -1
data1[data1==1] <- 0
data1[data1==2] <- 1
Markers<-t(data1)
dim(Markers)#61030
##Phenotypes
Pheno<-read.table("Qualité_ajustée.txt", header=TRUE, sep="\t")
Pheno<-Pheno[,-1]
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
#10 CYCLES
traits=	11
cycles= 10
accuracy = matrix(nrow=cycles, ncol=traits)
for(n in c(1: traits)){
  for(r in c(1: cycles)){
    {
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
      colnames(accuracy)<-colnames(Pheno)
      accuracy[r,n] <-cor(pred_y_valid, y_valid, use="complete" )
    }
  }
}
colnames(accuracy)<-colnames(Pheno)
write.table(accuracy, "RRBLUP_75_10.txt", sep=" ", quote=F, col.names=F, row.names=T)
#20 CYCLES
traits=100
cycles= 20
accuracy= matrix(nrow=cycles, ncol=traits)
for(n in c(1: traits)){
  for(r in c(1: cycles)){
    {
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
      colnames(accuracy)<-colnames(Pheno)
      accuracy[r,n] <-cor(pred_y_valid, y_valid, use="complete" )
    }
  }
}
colnames(accuracy)<-colnames(Pheno)
write.table(accuracy, "RRBLUP_75_20.txt", sep=" ", quote=F, col.names=F, row.names=T)
#50 CYCLES
traits=11
cycles= 50
accuracy = matrix(nrow=cycles, ncol=traits)
for(n in c(1: traits)){
  for(r in c(1: cycles)){
    {
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
      colnames(accuracy)<-colnames(Pheno)
      accuracy[r,n] <-cor(pred_y_valid, y_valid, use="complete" )
    }
  }
}
colnames(accuracy)<-colnames(Pheno)
write.table(accuracy, "RRBLUP_75_50.txt", sep=" ", quote=F, col.names=F, row.names=T)
#100 CYCLES
traits=11
cycles= 100
accuracy = matrix(nrow=cycles, ncol=traits)
for(n in c(1: traits)){
  for(r in c(1: cycles)){
    {
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
      colnames(accuracy)<-colnames(Pheno)
      accuracy[r,n] <-cor(pred_y_valid, y_valid, use="complete" )
    }
  }
}
colnames(accuracy)<-colnames(Pheno)
write.table(accuracy, "RRBLUP_75_100.txt", sep=" ", quote=F, col.names=F, row.names=T)
#200 CYCLES
traits=11
cycles= 200
accuracy = matrix(nrow=cycles, ncol=traits)
for(n in c(1: traits)){
  for(r in c(1: cycles)){
    {
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
      colnames(accuracy)<-colnames(Pheno)
      accuracy[r,n] <-cor(pred_y_valid, y_valid, use="complete" )
    }
  }
}
colnames(accuracy)<-colnames(Pheno)
write.table(accuracy, "RRBLUP_75_200.txt", sep=" ", quote=F, col.names=F, row.names=T)

#300 CYCLES
traits=11
cycles= 300
accuracy = matrix(nrow=cycles, ncol=traits)
for(n in c(1: traits)){
  for(r in c(1: cycles)){
    {
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
      colnames(accuracy)<-colnames(Pheno)
      accuracy[r,n] <-cor(pred_y_valid, y_valid, use="complete" )
    }
  }
}
colnames(accuracy)<-colnames(Pheno)
write.table(accuracy, "RRBLUP_75_300.txt", sep=" ", quote=F, col.names=F, row.names=T)
#400 CYCLES
traits=11
cycles= 400
accuracy = matrix(nrow=cycles, ncol=traits)
for(n in c(1: traits)){
  for(r in c(1: cycles)){
    {
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
      colnames(accuracy)<-colnames(Pheno)
      accuracy[r,n] <-cor(pred_y_valid, y_valid, use="complete" )
    }
  }
}
colnames(accuracy)<-colnames(Pheno)
write.table(accuracy, "RRBLUP_75_400.txt", sep=" ", quote=F, col.names=F, row.names=T)
#500 CYCLES
traits=11
cycles= 500
accuracy = matrix(nrow=cycles, ncol=traits)
for(n in c(1: traits)){
  for(r in c(1: cycles)){
    {
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
      colnames(accuracy)<-colnames(Pheno)
      accuracy[r,n] <-cor(pred_y_valid, y_valid, use="complete" )
    }
  }
}
colnames(accuracy)<-colnames(Pheno)
write.table(accuracy, "RRBLUP_75_500.txt", sep=" ", quote=F, col.names=F, row.names=T)
#600 CYCLES
traits=11
cycles= 600
accuracy = matrix(nrow=cycles, ncol=traits)
for(n in c(1: traits)){
  for(r in c(1: cycles)){
    {
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
      colnames(accuracy)<-colnames(Pheno)
      accuracy[r,n] <-cor(pred_y_valid, y_valid, use="complete" )
    }
  }
}
colnames(accuracy)<-colnames(Pheno)
write.table(accuracy, "RRBLUP_75_600.txt", sep=" ", quote=F, col.names=F, row.names=T)
#700 CYCLES
traits=11
cycles= 700
accuracy= matrix(nrow=cycles, ncol=traits)
for(n in c(1: traits)){
  for(r in c(1: cycles)){
    {
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
      colnames(accuracy)<-colnames(Pheno)
      accuracy[r,n] <-cor(pred_y_valid, y_valid, use="complete" )
    }
  }
}
colnames(accuracy)<-colnames(Pheno)
write.table(accuracy, "RRBLUP_75_700.txt", sep=" ", quote=F, col.names=F, row.names=T)
#800 CYCLES
traits=11
cycles= 800
accuracy = matrix(nrow=cycles, ncol=traits)
for(n in c(1: traits)){
  for(r in c(1: cycles)){
    {
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
      colnames(accuracy)<-colnames(Pheno)
      accuracy[r,n] <-cor(pred_y_valid, y_valid, use="complete" )
    }
  }
}
colnames(accuracy)<-colnames(Pheno)
write.table(accuracy, "RRBLUP_75_800.txt", sep=" ", quote=F, col.names=F, row.names=T)
#900 CYCLES
traits=11
cycles= 900
accuracy= matrix(nrow=cycles, ncol=traits)
for(n in c(1: traits)){
  for(r in c(1: cycles)){
    {
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
      colnames(accuracy)<-colnames(Pheno)
      accuracy[r,n] <-cor(pred_y_valid, y_valid, use="complete" )
    }
  }
}
colnames(accuracy)<-colnames(Pheno)
write.table(accuracy, "RRBLUP_75_900.txt", sep=" ", quote=F, col.names=F, row.names=T)
#1000 CYCLES
traits=11
cycles= 1000
accuracy= matrix(nrow=cycles, ncol=traits)
for(n in c(1: traits)){
  for(r in c(1: cycles)){
    {
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
      colnames(accuracy)<-colnames(Pheno)
      accuracy[r,n] <-cor(pred_y_valid, y_valid, use="complete" )
    }
  }
}
colnames(accuracy)<-colnames(Pheno)
write.table(accuracy, "RRBLUP_75_1000.txt", sep=" ", quote=F, col.names=F, row.names=T)
