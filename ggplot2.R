#最近一次间隔天数
i<-4
n <- paste(colnames(All)[i],cname[i],sep="")
p1<-ggplot(All, aes(x = All[,i], fill=odue_f))+
  geom_density(alpha=.2)+xlim(0,300)+
  labs(title=paste(n,'概率分布图',sep=""),x=cname[i],y="概率密度",fill="逾期标记")
ggsave(file=paste(outpath,"/",cname[i],".jpg",sep = ""), plot=p1,
       width =8, height = 6, dpi = 300)


#性别
i<-13
n <- paste(colnames(All)[i],cname[i],sep="")
p1<-ggplot(All, aes(x=All[,i], fill=odue_f)) +
  geom_histogram(alpha=.5,position = "fill") +
  labs(title=paste(n,'逾期人数比例图',sep=""),x=cname[i],y="频次",fill="逾期标记")
ggsave(file=paste(outpath,"/",cname[i],"1.jpg",sep = ""), plot=p1,
       width =8, height = 6, dpi = 300)

p2<-ggplot(All, aes(x=All[,i]))+
  geom_bar(binwidth = 1,position = "dodge") + 
  labs(title=paste(n,'频次图',sep=""),x=cname[i],y="频率",fill="逾期标记")
ggsave(file=paste(outpath,"/",cname[i],"2.jpg",sep = ""), plot=p1,
       width =8, height = 6, dpi = 300)


dt<-data.frame(A=as.numeric(table(All[,i])),B=levels(All[,i])) ## excel plot pie 

