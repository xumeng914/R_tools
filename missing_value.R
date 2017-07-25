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
