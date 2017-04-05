##输入：judgeMatrix 判断矩阵；round 结果约分位数
##输出：权重
weight <- function (judgeMatrix, round=3) {
  n = ncol(judgeMatrix)
  cumProd <- vector(length=n)
  cumProd <- apply(judgeMatrix, 1, prod)  ##求每行连乘积
  weight <- cumProd^(1/n)  ##开n次方(特征向量)
  weight <- weight/sum(weight) ##求权重
  round(weight, round)
}

###注：CRtest调用了weight函数
###输入：judgeMatrix
###输出：CI, CR
CRtest <- function (judgeMatrix, round=3){
  RI <- c(0, 0, 0.58, 0.9, 1.12, 1.24, 1.32, 1.41, 1.45, 1.49, 1.51) #随机一致性指标
  Wi <- weight(judgeMatrix)  ##计算权重
  n <- length(Wi)
  if(n > 11){
    cat("判断矩阵过大,请少于11个指标 \n")
  }
  if (n > 2) {
    W <- matrix(Wi, ncol = 1) 
    judgeW <- judgeMatrix %*% W 
    JudgeW <- as.vector(judgeW)
    la_max <- sum(JudgeW/Wi)/n
    CI = (la_max - n)/(n - 1)
    CR = CI/RI[n]
    cat("\n CI=", round(CI, round), "\n")
    cat("\n CR=", round(CR, round), "\n")
    if (CR <= 0.1) {
      cat(" 通过一致性检验 \n")
      cat("\n Wi: ", round(Wi, round), "\n")
    }
    else {
      cat(" 请调整判断矩阵,使CR<0.1 \n")
      Wi = NULL
    }
  }
  else if (n <= 2) {
    return(Wi)
  }
  consequence <- c(round(CI, round), round(CR, round))
  names(consequence) <- c("CI", "CR")
  consequence
}