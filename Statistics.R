x <- pretty(c(-3, 3), 30)
y <- dnorm(x)
plot(x, y, type = "l", xlab = "Normal Deviate", ylab = "Density", 
  yaxs = "i")


pnorm(1.96)  #位于z=1.96左侧的标准正态曲线下方的面积
pnorm(1.96,mean=100,sd=10)

qnorm(0.9,mean=500,sd=100) #均值为500，标准差为100的正态分布的0.9分位点值是多少

rnorm(50,mean=50,sd=10) #生成50个均值为50，标准差为10的正态随机数

#######################################################################
pbinom(9,size=10,prob=0.3)

