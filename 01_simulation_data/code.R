set.seed(10)
# x <- rlnorm(n = 100,
#            meanlog = 3,
#            sdlog = 4 )

x <- rnorm(n = 100,
            mean = 3,
            sd =  4)



png("./output/hist_break_5.png")
hist_x_break_5 <- hist(x,
     breaks = 5)
dev.off()

png("./output/hist_break_10.png")
hist_x_break_10 <-hist(x,
     breaks = 10)
dev.off()

png("./output/hist_break_50.png")
hist_x_break_50 <-hist(x,
     breaks = 50)
dev.off()

# output_dir_path <- here::here("output")
# file_path <- here::here(output_dir_path,"hist_break_5.png")
# ggsave(file = file_path, plot = hist)

install.packages("kdensity")
library(kdensity)

density_x <-density(x)
density_x_bw_20 <-density(x,bw=20)
density_x_bw_5 <-density(x,bw=5)

dev.new()
png("./output/hist_density.png")
hist(x, freq = FALSE, breaks = 10,main="Histgram and Density")
lines(density_x)
lines(density_x_bw_5,col="blue")
lines(density_x_bw_20,col="red")
dev.off()
#densityのbwのnrd0は望ましくないデフォルト,SJが適している宋



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
for(i in 2:20){
  assign(paste0("x_",i),x^i)
}
x_2
df<-data.frame(y,x,z,x_2,x_3,x_4,x_5,
               x_6,x_7,x_8,
               x_9,x_10,x_11,x_12,
               x_13,x_14,x_15,x_16,x_17,x_18
               ,x_19,x_20,error)
View(df)
#回数の適正値 サンプル100なら60くらい？詳しくは動画
bootstrap_sample <- rsample::bootstraps(df,
                                        times=5)
bootstrap_sample_1 <- bootstrap_sample$splits[[1]]
bootstrap_sample_2 <- bootstrap_sample$splits[[2]]
bootstrap_sample_3 <- bootstrap_sample$splits[[3]]
bootstrap_sample_4 <- bootstrap_sample$splits[[4]]
bootstrap_sample_5 <- bootstrap_sample$splits[[5]]
View_bootstrap_sample_1<-as.data.frame(bootstrap_sample_1)
#これを繰り返せばブートストラップサンプルを見れる

install.packages("glmnet")
library(glmnet)
#familyを設定するのなぜ
#変数を付け足す
sample_1_vec_x<-cbind(View_bootstrap_sample_1$x,
             View_bootstrap_sample_1$x_2,
             View_bootstrap_sample_1$x_3,
             View_bootstrap_sample_1$x_4,
             View_bootstrap_sample_1$x_5,
             View_bootstrap_sample_1$x_6,
             View_bootstrap_sample_1$x_7,
             View_bootstrap_sample_1$x_8,
             View_bootstrap_sample_1$x_9,
             View_bootstrap_sample_1$x_10,
             View_bootstrap_sample_1$x_11,
             View_bootstrap_sample_1$x_12,
             View_bootstrap_sample_1$x_13,
             View_bootstrap_sample_1$x_14,
             View_bootstrap_sample_1$x_15,
             View_bootstrap_sample_1$x_16,
             View_bootstrap_sample_1$x_17,
             View_bootstrap_sample_1$x_18,
             View_bootstrap_sample_1$x_19,
             View_bootstrap_sample_1$x_20
             )
lasso.model.cv <- cv.glmnet(x=sample_1_vec_x, y=z ,
                            family = "gaussian", alpha = 1)
