
########################################################
#####################  function  #######################
########################################################

FDataPretreatment <- function(preprocess_data,goal_var,p){
  #############查看离散变量分布，计算最大类别占比,已经包含缺失值##########
  #去掉目标变量，以防这一步被剔 
  explain_only<-preprocess_data[,-which(names(preprocess_data)==goal_var)] 
  
  i<-0
  discrete_distri<-data.frame(var_name=vector(),percent=vector(),num=vector())
  for(j in 1:length(explain_only))
  { if( is.character(explain_only[,j]) || is.integer(explain_only[,j]) )
  {
    i=i+1;
    discrete_distri[i,1]<-names(explain_only)[j] ;
    discrete_distri[i,2]<-max(table(explain_only[,j]))/sum(table(explain_only[,j]));
    discrete_distri[i,3]<-j
  }
  }
  
  ##########可剔除的离散变量清单###########
  out_discrete_var<-discrete_distri[discrete_distri$percent>=p, 1]
  
  ###########查看连续变量分布########################
  i<-0
  continu_distri<-data.frame(var_name=vector(),percent=vector(),num=vector())
  for(j in 1:length(explain_only))
  { if(  (is.numeric(explain_only[,j])) & (!is.integer(explain_only[,j]))  )
  {
    i=i+1;
    continu_distri[i,1]<-names(explain_only)[j];
    continu_distri[i,2]<-nrow(subset(explain_only,explain_only[,j]==0,select = c(1)))/nrow(explain_only);
    continu_distri[i,3]<-j
  }
  }
  
  ##########可剔除的连续变量清单###########
  out_continu_var<-continu_distri[continu_distri$percent>=p, 1]
  
  ##############################################################
  #################先剔除上述2步骤中的变量######################
  discrete_continu_f<-names(preprocess_data) %in% c(out_discrete_var,out_continu_var)
  preprocess_remain<-preprocess_data[!discrete_continu_f]   
  #以防变量有重复剔除（方便文档撰写）
  
  ########################单变量显著性检验###########################
  #当自变量是离散型时，卡方检验
  i<-0
  chi<-data.frame(var_name=vector(),pvalue=vector(),num=vector())
  for(j in 1:length(preprocess_remain)) 
  { 
    chi_tab <- xtabs(~preprocess_remain[,j]+preprocess_remain[,which(names(preprocess_remain)==goal_var)], preprocess_remain)
    if(  (is.character(preprocess_remain[,j]) || is.integer(preprocess_remain[,j])) & (min(chi_tab) >=5) ) 
    { 
      
      t<-chisq.test(chi_tab) 
      i=i+1
      chi[i,1]<-names(preprocess_remain)[j]
      chi[i,2]<-t$p.value
      chi[i,3]<-j
    }
  }
  
  ###########单变量显著性检验不通过的变量清单###########
  out_signif_var<-c(chi[chi$pvalue>0.05,1])   
  
  
  #############剔除分布不均匀的变量，和单变量显著性检验不通过的变量#################
  out_flag<-names(preprocess_data) %in% c(out_discrete_var,out_continu_var,out_signif_var)
  preprocess_final<-preprocess_data[!out_flag]   
  
  #####################处理异常值（只针对连续型）########################
  for(j in 1:ncol(preprocess_final))    
  { 
    if( length(unique(preprocess_final[,j]))>250 )
    {
      q_001<-quantile(preprocess_final[,j], 0.001)
      q_999<- quantile(preprocess_final[,j], 0.999)
      
      preprocess_final[preprocess_final[,j]> q_999,j] <- q_999
      if(q_001<0){preprocess_final[preprocess_final[,j]< q_001,j] <- q_001 }
      
    }
  }
  
  return(preprocess_final)
  
}

########################################################
#####################  function end  ###################
########################################################