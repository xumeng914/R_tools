n=nrow(All_final)
ll<-c(1:n)
All_0.7_no <- sample(ll,7/10*n)
All_0.3_no <-subset(ll,!ll %in% All_0.7_no)
All_0.7 <- All_final[All_0.7_no,]
All_0.7_1 <- subset(All_0.7, All_0.7$ln_act_f==1)
All_0.7_0 <- subset(All_0.7, All_0.7$ln_act_f==0)
All_0.7_0_sample <- All_0.7_0[sample(1:nrow(All_0.7_0), nrow(All_0.7_1)),]
train <- rbind(All_0.7_1,All_0.7_0_sample) 
All_0.3 <- All_final[All_0.3_no,]