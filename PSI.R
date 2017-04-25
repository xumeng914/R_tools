a <- read.csv("test_PSI.csv")
a1 <- a[c(1:3000), ]
a2 <- a[c(3001:6000), ]

p1 <- table(a1$level1)/3000
p2 <- table(a2$level1)/3000

PSI <- sum((p1 - p2)/log(p1/p2))
 ## psi小于0.1时候模型稳定性很高，0.1-0.25一般，大于0.25模型稳定性差，建议重做