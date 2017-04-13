F_mon_num <- function(year,mon){
  
  if(mon==2){
    ifelse (year %% 4 == 0 && year %% 100 != 0 || year %% 400 == 0,mon_num <- 29,mon_num <- 28)   }  
  if(mon %in% c(1,3,5,7,8,10,12)) mon_num <- 31
  if(mon %in% c(4,6,9,11)) mon_num <- 30
  return(mon_num)
}