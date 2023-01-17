set.seed(10)
x <- rlnorm(n=100,
           meanlog=3,
           sdlog=4)


hist(x,
     breaks=5)
hist(x,
     breaks=10)
hist(x,
     breaks=50)

install.packages("kdensity")
library(kdensity)
density(x)
kdensity(x)

#defaultの設定を変更して、色々試す


error<-rnorm(100,0,1)
z<-01*x-0.3*x^2+0.5*x^5-0.7*x^7+error

install.packages("quantreg")
library(quantreg)

tau  <- c(0.25,0.5,0.75)
# 分位点回帰モデルを推定する
quant_reg  <- rq(z~x, tau=tau)
quant_reg

install.packages("estimatr")
library(estimatr)

ols_reg<-lm_robust(z~x)

y<-vector()
#非線形回帰

x[1]
y[1]
for(i in 1:100){
  if(x[i]+error[i]>=4){
    y[i]<-1
  }else{
    y[i]<-0
  }  
}

probit_reg<-glm(y~x,family=binomial(probit))
#限界効果の形にする必要がある
probit_reg


linear_probit_reg<-lm_robust(y~x)
linear_probit_reg
#線形確率モデルとprobitを比較するためには、係数の解釈を統一する必要がある




install.packages("rsample")
library(rsample)
df<-data.frame(y,x,z,error)
#回数の適正値 サンプル100なら60くらい？詳しくは動画
bootstrap_sample_1<-bootstraps(df,times=60)
bootstrap_sample_2<-bootstraps(df,times=60)
bootstrap_sample_3<-bootstraps(df,times=60)
bootstrap_sample_4<-bootstraps(df,times=60)
bootstrap_sample_5<-bootstraps(df,times=60)

install.packages("glmnet")
library(glmnet)
#familyを設定するのなぜ
x
z
lasso.model.cv <- cv.glmnet(x=x, y=z ,
                            family = "gaussian", alpha = 1)
