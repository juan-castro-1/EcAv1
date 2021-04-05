#Paquetes
library(rlist)    
library(openxlsx) 
library(ggplot2)  
library(readxl) 

setwd("C:/Users/juan_/Desktop/Econometría")


##                   EJERCICIO UNO
#PUNTO UNO
rm(df.list)
df.list=list()
for(m in 1:50){
  set.seed(m)
  x1=c(runif(100, 0, 100))
  x2=c(runif(100, 0, 100))
  x3=c(runif(100, 0, 100))
  u=c(rnorm(100, 0, sd=40))
  y=300+x1+x2-x3+u
  df.list= list.append(df.list,data.frame(y, x1, x2, x3, u))
}

#PUNTO DOS
vector_y <- data.frame(df.list[[1]]$y)
vector_x1 <- data.frame(df.list[[1]]$x1)
vector_x2 <- data.frame(df.list[[1]]$x2)
vector_x3 <- data.frame(df.list[[1]]$x3)
vector_u <- data.frame(df.list[[1]]$u)
names(vector_x1)="x1"
names(vector_x2)="x2"
names(vector_x3)="x3"
names(vector_y)="y"
tabla=data.frame(vector_x1, vector_x2, vector_x3)
algo=cor(tabla)
tabla=data.frame(x1,x2,x3)      
library(corrplot)
corrplot(algo, method="color", addCoef.col = "gray", tl.srt = 45)


#PUNTO TRES
rm(df.list.reg)
df.list.reg=list()
for(i in 1:50){
  lm.fit=lm(y~x1+x2+x3, data=df.list[[i]])
  df.list.reg=list.append(df.list.reg, coef(lm.fit))

}

vb=unlist(df.list.reg)         
mb=matrix(vb, nrow=4, ncol=50) 
tmb=t(mb)                       
write.xlsx(tmb, "primera_estimacion.xlsx",)


#PUNTO CUATRO
datab=read.xlsx("primera_estimacion.xlsx")
fix(datab)
library(ggplot2)
ggplot(datab, aes(x=V2, y=V3)) + geom_point()+ xlab("Beta1")+ylab("Beta2")+ggtitle("Grafico 1: Dispersipon de Beta1-Beta2")
ggplot(datab, aes(x=V2, y=V4)) + geom_point()+ xlab("Beta1")+ylab("Beta3")+ggtitle("Grafico 2: Dispersipon de Beta1-Beta3")




#PUNTO CINCO
#uso todo lo mismo de antes pero con "nombre"2
df.list2=list()
for(m in 1:50){
  set.seed(m)
  x1=c(runif(100, 0, 100))
  x2<-scale(matrix( rnorm(100), ncol=1))
  xs<-cbind(scale(x1),x2)
  c1<-var(xs)
  chol1<-solve(chol(c1))
  newx<-xs
  newc<-matrix(
    c(1, 0.987,
      0.987, 1), ncol=2)
  eigen(newc)
  chol2<-chol(newc)
  xm2<-newx%*%chol2*sd(x1)+mean(x1)
  x2<-xm2[,2]
  x3=c(runif(100, 0, 100))
  u=c(rnorm(100, 0, sd=40))
  y=300+x1+x2-x3+u
  df.list2= list.append(df.list2,data.frame(y, x1, x2, x3, u))
}

df.list.reg2=list()
for(i in 1:50){
  lm.fit2=lm(y~x1+x2+x3, data=df.list2[[i]])
  df.list.reg2=list.append(df.list.reg2, coef(lm.fit2))
}
#PUNTO SEIS
vb2=unlist(df.list.reg2)
mb2=matrix(vb2, nrow=4, ncol=50)
tmb2=t(mb2)
write.xlsx(tmb2, "segunda_estimacion.xlsx",)

#PUNTO SIETE
datab2=read.xlsx("segunda_estimacion.xlsx")
fix(datab2)     
ggplot(datab2, aes(x=V2,y=V3)) + geom_point()+ xlab("Beta1")+ylab("Beta2")+ggtitle("Grafico 3: Dispersipon de Beta1-Beta2")
ggplot(datab2, aes(x=V2,y=V4)) + geom_point()+ xlab("Beta1")+ylab("Beta3")+ggtitle("Grafico 4: Dispersipon de Beta1-Beta3")



##                    EJERCICIO DOS

#PUNTO UNO
rm(df.list.reg3)
df.list.reg3=list()
for(i in 1:50){
  lm.fit3=lm(y~x1+x3, data=df.list[[i]])
  df.list.reg3=list.append(df.list.reg3, coef(lm.fit3))
}
vb3=unlist(df.list.reg3)          
mb3=matrix(vb3, nrow=3, ncol=50)  
tmb3=t(mb3)                       
write.xlsx(tmb3, "tercera_estimacion.xlsx",)
datab3=read.xlsx("tercera_estimacion.xlsx")
fix(datab3)                        
ggplot(datab3, aes(x=V2,y=V3)) + geom_point()+ xlab("Beta1")+ylab("Beta3")+ggtitle("Grafico 5: Dispersipon de Beta1-Beta3")

#PUNTO DOS
rm(df.list.reg4)
df.list.reg4=list()
for(i in 1:50){
  lm.fit4=lm(y~x1+x3, data=df.list2[[i]])
  df.list.reg4=list.append(df.list.reg4, coef(lm.fit4))
}
vb4=unlist(df.list.reg4)         
mb4=matrix(vb4, nrow=3, ncol=50) 
tmb4=t(mb4)                       
write.xlsx(tmb4, "cuarta_estimacion.xlsx",)
datab4=read.xlsx("cuarta_estimacion.xlsx")
fix(datab4)                        
ggplot(datab4, aes(x=V2,y=V3)) + geom_point()+ xlab("Beta1")+ylab("Beta3")+ggtitle("Grafico 6: Dispersión de Beta1-Beta3")

#FIN



