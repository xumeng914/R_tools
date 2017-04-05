library("ggplot2")
library("gridExtra")
inpath <- "D:/R/ATM"
outpath <- "D:/R/ATM/Result"

jgh<-read.csv(paste(inpath,"/jgh.csv",sep=""),heade=TRUE,sep=",", fileEncoding ="utf8")
jgh$org_name<-as.character(jgh$org_name)
jgh$atm_no<-as.character(jgh$atm_no)
names(jgh)[2]<-"bj36no08"
name<-unique(jgh$bj36no08)
####################################### 12mth ##############################

t<-read.csv(paste(inpath,"/lst_12_mths.csv",sep=""),heade=TRUE,sep=",", fileEncoding ="utf8")
t$bj36no08 <- as.character(t$bj36no08)
t$y_mth <- as.factor(t$y_mth)

tt<-merge(t,jgh,by="bj36no08")

for (i in 1:length(name)){
  
  j<-0
  M<-subset(tt,tt$bj36no08==name[i])
  
  if(nrow(M)>6){
  path <- paste(outpath,"/",jgh[jgh$bj36no08==name[i],1],sep="")
  if(!dir.exists(path)) {dir.create(path)}
  if(!dir.exists(paste(path,"/",name[i],sep=""))) {dir.create(paste(path,"/",name[i],sep=""))}
  write.table(M, paste(path,"/",name[i],"/",name[i],".csv",sep=""),row.names = FALSE,col.names = TRUE, quote = FALSE,sep=",",fileEncoding = "GBK")
#### 取现
  n <- paste(name[i],"号ATM12个月取现金额分布图",sep="")
  p1<-ggplot(M,aes(x=y_mth,y=amt_qx/10000))+
    geom_bar(stat="identity") +
    labs(title=n,x="月份",y="取现金额(万元)")
  ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p1,
          width = 8, height = 6,  dpi = 300) 
  
  n <- paste(name[i],"号ATM12个月取现笔数分布图",sep="")
   p2<-ggplot(M,aes(x=y_mth,y=nbr_qx))+
     geom_point(stat="identity") +  geom_line(aes(x=as.numeric(M$y_mth),y=nbr_qx),size=1)+ 
     labs(title=n,x="月份",y="取现笔数")
   
   ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p2,
           width = 8, height = 6,  dpi = 300) 
#### 存现   
   if(sum(M$amt_cx)!=0){
     j<-1
     n <- paste(name[i],"号ATM12个月存现金额分布图",sep="")
     p3<-ggplot(M,aes(x=y_mth,y=amt_cx/10000))+
       geom_bar(stat="identity") +
       labs(title=n,x="月份",y="存现金额(万元)")
     ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p3,
             width = 8, height = 6,  dpi = 300) 
     
     n <- paste(name[i],"号ATM12个月存现笔数分布图",sep="")
     p4<-ggplot(M,aes(x=y_mth,y=nbr_cx))+
       geom_point(stat="identity") +  geom_line(aes(x=as.numeric(M$y_mth),y=nbr_cx),size=1)+ 
       labs(title=n,x="月份",y="存现笔数")
     
     ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p4,
             width = 8, height = 6,  dpi = 300) 
   }

#### 转账      
   n <- paste(name[i],"号ATM12个月转账金额分布图",sep="")
   p5<-ggplot(M,aes(x=y_mth,y=amt_zz/10000))+
     geom_bar(stat="identity") +
     labs(title=n,x="月份",y="转账金额(万元)")
   ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p5,
           width = 8, height = 6,  dpi = 300) 
   
   n <- paste(name[i],"号ATM12个月转账笔数分布图",sep="")
   p6<-ggplot(M,aes(x=y_mth,y=nbr_zz))+
     geom_point(stat="identity") +  geom_line(aes(x=as.numeric(M$y_mth),y=nbr_zz),size=1)+ 
     labs(title=n,x="月份",y="转账笔数")
   
   ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p6,
           width = 8, height = 6,  dpi = 300) 
   
   #### 取现本代他      
   n <- paste(name[i],"号ATM12个月取现本代他金额分布图",sep="")
   p7<-ggplot(M,aes(x=y_mth,y=amt_qx_bdt/10000))+
     geom_bar(stat="identity") +
     labs(title=n,x="月份",y="取现本代他金额(万元)")
   ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p7,
           width = 8, height = 6,  dpi = 300) 
   
   n <- paste(name[i],"号ATM12个月取现本代他笔数分布图",sep="")
   p8<-ggplot(M,aes(x=y_mth,y=nbr_qx_bdt))+
     geom_point(stat="identity") +  geom_line(aes(x=as.numeric(M$y_mth),y=nbr_qx_bdt),size=1)+ 
     labs(title=n,x="月份",y="取现本代他笔数")
   
   ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p8,
           width = 8, height = 6,  dpi = 300)  
  
   #### 转账本代他      
   n <- paste(name[i],"号ATM12个月转账本代他金额分布图",sep="")
   p9<-ggplot(M,aes(x=y_mth,y=amt_zz_bdt/10000))+
     geom_bar(stat="identity") +
     labs(title=n,x="月份",y="转账本代他金额(万元)")
   ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p9,
           width = 8, height = 6,  dpi = 300) 
   
   n <- paste(name[i],"号ATM12个月转账本代他笔数分布图",sep="")
   p10<-ggplot(M,aes(x=y_mth,y=nbr_zz_bdt))+
     geom_point(stat="identity") +  geom_line(aes(x=as.numeric(M$y_mth),y=nbr_zz_bdt),size=1)+ 
     labs(title=n,x="月份",y="转账本代他笔数")
   
   ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p10,
           width = 8, height = 6,  dpi = 300)     
   
   
   ##### 用钞量
      
   n <- paste(name[i],"号ATM12个月用钞量金额分布图",sep="")
   p11<-ggplot(M,aes(x=y_mth,y=(amt_qx-amt_cx)/10000))+
     geom_bar(stat="identity") +
     labs(title=n,x="月份",y="用钞量金额(万元)")
   ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p11,
           width = 8, height = 6,  dpi = 300) 
   
  }
#jpeg(filename=paste(path,"/",name[i],"/",name[i],".jpg",sep=""),width = 2400, height = 4500, 
#     quality = 100) 
#ifelse(j==1,grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,  ncol=2, nrow=4+j),
#       grid.arrange(p1,p2,p5,p6,p7,p8,p9,p10,  ncol=2, nrow=4+j))
#dev.off()
  print(paste(round(100*i/length(name),2),"%",sep=""))
}
###########################################################################################
####################################### 31day ##############################

t<-read.csv(paste(inpath,"/lst_3_mths.csv",sep=""),heade=TRUE,sep=",", fileEncoding ="utf8")
t$bj36no08 <- as.character(t$bj36no08)


tt<-merge(t,jgh,by="bj36no08")

for (i in 1:length(name)){
  
  j<-0
  M<-subset(tt,tt$bj36no08==name[i])
  
  if(nrow(M)>6){
    path <- paste(outpath,"/",jgh[jgh$bj36no08==name[i],1],sep="")
    if(!dir.exists(path)) {dir.create(path)}
    if(!dir.exists(paste(path,"/",name[i],sep=""))) {dir.create(paste(path,"/",name[i],sep=""))}
    write.table(M, paste(path,"/",name[i],"/",name[i],".csv",sep=""),row.names = FALSE,col.names = TRUE, quote = FALSE,sep=",",fileEncoding = "GBK")
    #### 取现
    n <- paste(name[i],"号ATM每月各日取现金额分布图",sep="")
    p1<-ggplot(M,aes(x=day_of_mth,y=amt_qx/10000))+
      geom_bar(stat="identity") +
      labs(title=n,x="日期",y="取现金额(万元)")
    ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p1,
            width = 8, height = 6,  dpi = 300) 
    
    n <- paste(name[i],"号ATM每月各日取现笔数分布图",sep="")
    p2<-ggplot(M,aes(x=day_of_mth,y=nbr_qx))+
      geom_point(stat="identity") +  geom_line(aes(x=as.numeric(M$day_of_mth),y=nbr_qx),size=1)+ 
      labs(title=n,x="日期",y="取现笔数")
    
    ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p2,
            width = 8, height = 6,  dpi = 300) 
    #### 存现   
    if(sum(M$amt_cx)!=0){
      j<-1
      n <- paste(name[i],"号ATM每月各日存现金额分布图",sep="")
      p3<-ggplot(M,aes(x=day_of_mth,y=amt_cx/10000))+
        geom_bar(stat="identity") +
        labs(title=n,x="日期",y="存现金额(万元)")
      ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p3,
              width = 8, height = 6,  dpi = 300) 
      
      n <- paste(name[i],"号ATM每月各日存现笔数分布图",sep="")
      p4<-ggplot(M,aes(x=day_of_mth,y=nbr_cx))+
        geom_point(stat="identity") +  geom_line(aes(x=as.numeric(M$day_of_mth),y=nbr_cx),size=1)+ 
        labs(title=n,x="日期",y="存现笔数")
      
      ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p4,
              width = 8, height = 6,  dpi = 300) 
    }
    
    #### 转账      
    n <- paste(name[i],"号ATM每月各日转账金额分布图",sep="")
    p5<-ggplot(M,aes(x=day_of_mth,y=amt_zz/10000))+
      geom_bar(stat="identity") +
      labs(title=n,x="日期",y="转账金额(万元)")
    ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p5,
            width = 8, height = 6,  dpi = 300) 
    
    n <- paste(name[i],"号ATM每月各日转账笔数分布图",sep="")
    p6<-ggplot(M,aes(x=day_of_mth,y=nbr_zz))+
      geom_point(stat="identity") +  geom_line(aes(x=as.numeric(M$day_of_mth),y=nbr_zz),size=1)+ 
      labs(title=n,x="日期",y="转账笔数")
    
    ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p6,
            width = 8, height = 6,  dpi = 300) 
    
    #### 取现本代他      
    n <- paste(name[i],"号ATM每月各日取现本代他金额分布图",sep="")
    p7<-ggplot(M,aes(x=day_of_mth,y=amt_qx_bdt/10000))+
      geom_bar(stat="identity") +
      labs(title=n,x="日期",y="取现本代他金额(万元)")
    ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p7,
            width = 8, height = 6,  dpi = 300) 
    
    n <- paste(name[i],"号ATM每月各日取现本代他笔数分布图",sep="")
    p8<-ggplot(M,aes(x=day_of_mth,y=nbr_qx_bdt))+
      geom_point(stat="identity") +  geom_line(aes(x=as.numeric(M$day_of_mth),y=nbr_qx_bdt),size=1)+ 
      labs(title=n,x="日期",y="取现本代他笔数")
    
    ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p8,
            width = 8, height = 6,  dpi = 300)  
    
    #### 转账本代他      
    n <- paste(name[i],"号ATM每月各日转账本代他金额分布图",sep="")
    p9<-ggplot(M,aes(x=day_of_mth,y=amt_zz_bdt/10000))+
      geom_bar(stat="identity") +
      labs(title=n,x="日期",y="转账本代他金额(万元)")
    ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p9,
            width = 8, height = 6,  dpi = 300) 
    
    n <- paste(name[i],"号ATM每月各日转账本代他笔数分布图",sep="")
    p10<-ggplot(M,aes(x=day_of_mth,y=nbr_zz_bdt))+
      geom_point(stat="identity") +  geom_line(aes(x=as.numeric(M$day_of_mth),y=nbr_zz_bdt),size=1)+ 
      labs(title=n,x="日期",y="转账本代他笔数")
    
    ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p10,
            width = 8, height = 6,  dpi = 300)     
    
    
    ##### 用钞量
    
    n <- paste(name[i],"号ATM每月各日用钞量金额分布图",sep="")
    p11<-ggplot(M,aes(x=day_of_mth,y=(amt_qx-amt_cx)/10000))+
      geom_bar(stat="identity") +
      labs(title=n,x="日期",y="用钞量金额(万元)")
    ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p11,
            width = 8, height = 6,  dpi = 300) 
    
  }
  #jpeg(filename=paste(path,"/",name[i],"/",name[i],".jpg",sep=""),width = 2400, height = 4500, 
  #     quality = 100) 
  #ifelse(j==1,grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,  ncol=2, nrow=4+j),
  #       grid.arrange(p1,p2,p5,p6,p7,p8,p9,p10,  ncol=2, nrow=4+j))
  #dev.off()
  print(paste(round(100*i/length(name),2),"%",sep=""))
}



###########################################################################################
####################################### 24hour ##############################

t<-read.csv(paste(inpath,"/mth6_hour.csv",sep=""),heade=TRUE,sep=",", fileEncoding ="utf8")
t$bj36no08 <- as.character(t$bj36no08)


tt<-merge(t,jgh,by="bj36no08")

for (i in 1:length(name)){
  
  j<-0
  M<-subset(tt,tt$bj36no08==name[i])
  
  if(nrow(M)>6){
    path <- paste(outpath,"/",jgh[jgh$bj36no08==name[i],1],sep="")
    if(!dir.exists(path)) {dir.create(path)}
    if(!dir.exists(paste(path,"/",name[i],sep=""))) {dir.create(paste(path,"/",name[i],sep=""))}
    write.table(M, paste(path,"/",name[i],"/",name[i],".csv",sep=""),row.names = FALSE,col.names = TRUE, quote = FALSE,sep=",",fileEncoding = "GBK")
    #### 取现
    n <- paste(name[i],"号ATM24小时取现金额分布图",sep="")
    p1<-ggplot(M,aes(x=occ_hour,y=amt_qx/10000))+
      geom_bar(stat="identity") +
      labs(title=n,x="小时",y="取现金额(万元)")
    ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p1,
            width = 8, height = 6,  dpi = 300) 
    
    n <- paste(name[i],"号ATM24小时取现笔数分布图",sep="")
    p2<-ggplot(M,aes(x=occ_hour,y=nbr_qx))+
      geom_point(stat="identity") +  geom_line(aes(x=as.numeric(M$occ_hour),y=nbr_qx),size=1)+ 
      labs(title=n,x="小时",y="取现笔数")
    
    ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p2,
            width = 8, height = 6,  dpi = 300) 
    #### 存现   
    if(sum(M$amt_cx)!=0){
      j<-1
      n <- paste(name[i],"号ATM24小时存现金额分布图",sep="")
      p3<-ggplot(M,aes(x=occ_hour,y=amt_cx/10000))+
        geom_bar(stat="identity") +
        labs(title=n,x="小时",y="存现金额(万元)")
      ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p3,
              width = 8, height = 6,  dpi = 300) 
      
      n <- paste(name[i],"号ATM24小时存现笔数分布图",sep="")
      p4<-ggplot(M,aes(x=occ_hour,y=nbr_cx))+
        geom_point(stat="identity") +  geom_line(aes(x=as.numeric(M$occ_hour),y=nbr_cx),size=1)+ 
        labs(title=n,x="小时",y="存现笔数")
      
      ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p4,
              width = 8, height = 6,  dpi = 300) 
    }
    
    #### 转账      
    n <- paste(name[i],"号ATM24小时转账金额分布图",sep="")
    p5<-ggplot(M,aes(x=occ_hour,y=amt_zz/10000))+
      geom_bar(stat="identity") +
      labs(title=n,x="小时",y="转账金额(万元)")
    ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p5,
            width = 8, height = 6,  dpi = 300) 
    
    n <- paste(name[i],"号ATM24小时转账笔数分布图",sep="")
    p6<-ggplot(M,aes(x=occ_hour,y=nbr_zz))+
      geom_point(stat="identity") +  geom_line(aes(x=as.numeric(M$occ_hour),y=nbr_zz),size=1)+ 
      labs(title=n,x="小时",y="转账笔数")
    
    ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p6,
            width = 8, height = 6,  dpi = 300) 
    
    #### 取现本代他      
    n <- paste(name[i],"号ATM24小时取现本代他金额分布图",sep="")
    p7<-ggplot(M,aes(x=occ_hour,y=amt_qx_bdt/10000))+
      geom_bar(stat="identity") +
      labs(title=n,x="小时",y="取现本代他金额(万元)")
    ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p7,
            width = 8, height = 6,  dpi = 300) 
    
    n <- paste(name[i],"号ATM24小时取现本代他笔数分布图",sep="")
    p8<-ggplot(M,aes(x=occ_hour,y=nbr_qx_bdt))+
      geom_point(stat="identity") +  geom_line(aes(x=as.numeric(M$occ_hour),y=nbr_qx_bdt),size=1)+ 
      labs(title=n,x="小时",y="取现本代他笔数")
    
    ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p8,
            width = 8, height = 6,  dpi = 300)  
    
    #### 转账本代他      
    n <- paste(name[i],"号ATM24小时转账本代他金额分布图",sep="")
    p9<-ggplot(M,aes(x=occ_hour,y=amt_zz_bdt/10000))+
      geom_bar(stat="identity") +
      labs(title=n,x="小时",y="转账本代他金额(万元)")
    ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p9,
            width = 8, height = 6,  dpi = 300) 
    
    n <- paste(name[i],"号ATM24小时转账本代他笔数分布图",sep="")
    p10<-ggplot(M,aes(x=occ_hour,y=nbr_zz_bdt))+
      geom_point(stat="identity") +  geom_line(aes(x=as.numeric(M$occ_hour),y=nbr_zz_bdt),size=1)+ 
      labs(title=n,x="小时",y="转账本代他笔数")
    
    ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p10,
            width = 8, height = 6,  dpi = 300)     
    
    
    ##### 用钞量
    
    n <- paste(name[i],"号ATM24小时用钞量金额分布图",sep="")
    p11<-ggplot(M,aes(x=occ_hour,y=(amt_qx-amt_cx)/10000))+
      geom_bar(stat="identity") +
      labs(title=n,x="小时",y="用钞量金额(万元)")
    ggsave( file = paste(path,"/",name[i],"/",n,".jpg",sep=""), plot = p11,
            width = 8, height = 6,  dpi = 300) 
    
  }
  #jpeg(filename=paste(path,"/",name[i],"/",name[i],".jpg",sep=""),width = 2400, height = 4500, 
  #     quality = 100) 
  #ifelse(j==1,grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,  ncol=2, nrow=4+j),
  #       grid.arrange(p1,p2,p5,p6,p7,p8,p9,p10,  ncol=2, nrow=4+j))
  #dev.off()
  print(paste(round(100*i/length(name),2),"%",sep=""))
}










