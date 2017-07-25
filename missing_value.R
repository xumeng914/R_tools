install.packages(c("VIM","mice"))

# 加载数据集
data(sleep, package="VIM")

# 列出没有缺失值的行
sleep[complete.cases(sleep),]

# 列出有一个或多个缺失值的行
sleep[!complete.cases(sleep),]


sum(is.na(sleep$Dream))
mean(is.na(sleep$Dream))

#函数complete.cases()可以用来识别矩阵或数据框中没有缺失值的行。若每行都包含完整的实例，则返回TRUE的逻辑向量；若每行有一个或多个缺失值，则返回FALSE。
#complete.cases()函数仅将NA和NaN识别为缺失值，无穷值（Inf和-Inf）被当作有效值。
mean(!complete.cases(sleep))


library("VIM")
aggr(sleep, prop=FALSE, numbers=TRUE)
aggr(sleep, prop=TRUE, numbers=TRUE)

matrixplot(sleep) #红色是缺失值，浅色值小，黑色值大


#相关性探索缺失值

x <- as.data.frame(abs(is.na(sleep)))
head(sleep, n=5)
head(x, n=5)

y <- x[which(apply(x,2,sum)>0)]
cor(y)
cor(sleep, y, use="pairwise.complete.obs")


##理性处理不完整数据

# mydata中所有包含缺失数据的行都被删除，然后结果才存储到newdata中
newdata <- na.omit(mydata)
cor(na.omit(sleep))
fit <- lm(Dream ~ Span + Gest, data=na.omit(sleep))
summary(fit)


###多重插补
library(mice)
data(sleep, package="VIM")
imp <- mice(sleep, seed=123)
fit <- with(imp, lm(Dream ~ Span + Gest))
pooled <- pool(fit)
summary(pooled)





