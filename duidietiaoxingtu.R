
library(ggplot2) 

  a$ln <- as.factor(a$ln)
  a$us <- as.factor(a$us)
  
  ggplot(data = a, mapping = aes(x = ln, y = V, fill = us)) + 
    geom_bar(stat = 'identity', position = 'stack',color = "black")+
    geom_text(aes(label = paste0(round(p,2),"%")), size = 3, colour = 'black', vjust = 3.5, hjust = .5, position = position_stack()) +
    scale_fill_manual(values = c("#0066FF","#C0C0C0"), limits = c('1','0')) +
    facet_grid(. ~ jq)
  
  
  
  
 