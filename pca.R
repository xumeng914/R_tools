load("D:/瑞丰银行工作/ATM/atm_mth6.rda")
M<-atm_mth6[,c(1,2,9:19,21:23,25,29)]
M$rent_y_area[M$rent_y_area==0]<-NA
M$price[M$price==0]<-NA
############################################# 房租缺失值补平均值 ###########################################
fzmean<-mean(M$rent_y_area,na.rm=TRUE)
M$rent_y_area[is.na(M$rent_y_area)]<-fzmean
cbmean<-mean(M$price,na.rm=TRUE)
M$price[is.na(M$price)]<-cbmean
M[,-c(2,3)]
#######################################################################################
length(unique(M$org_no))
jg<-aggregate(M[,-c(1,2,3)],by=list(M$org_no),sum)
#jg2<-aggregate(M[,c(22)],by=list(M$org_no),mean)
#jg_z<-merge(jg,jg2,by="Group.1")
#names(jg_z)[16]<-"use_year"

library(psych)
fa.parallel(jg[,-1],n.iter=7,show.legend = FALSE,main = "检验取主成分个数图")

pc<-principal(jg[,-1],nfactors = 2)
pc
fs<-data.frame(jg$Group.1,pc$scores)


cl <- kmeans(fs[,-1], 6)
plot(fs[,-1], col = cl$cluster,xlab = "第一主成分",ylab="第二主成分",main="聚类分析图")
leb_1<-which(cl$cluster==1)  #### black
leb_2<-which(cl$cluster==2)  ### red
leb_3<-which(cl$cluster==3)  #### green

################################## total_bj ###################################3

cl <- kmeans(fs[-c(leb_1,leb_2),-1], 5)
plot(fs[-c(leb_1,leb_2),-1], col = cl$cluster,xlab = "第一主成分",ylab="第二主成分",main="聚类分析图")


plot(cl$centers, col = 1:5, pch = 8, cex = 2)
symbols(cl$centers[,1],cl$centers[,2],circles =  cl$size,inches = 0.3,xlab = "第一主成分",ylab="第二主成分",main="聚类分析气泡图")
text(cl$centers[,1],cl$centers[,2],c("        1","2","3","        4","5"))

