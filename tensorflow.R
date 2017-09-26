install.packages("tensorflow")   #安装tensorflow，当然也可以用Rstudio手动安装
library(tensorflow)  #加载包

m1 <- matrix(c(1.0, 2.0, 
               3.0, 4.0), byrow=T, nrow=2)
m3 <- tf$constant(matrix(c(1.0, 2.0, 
                           3.0, 4.0), byrow=T, nrow=2))