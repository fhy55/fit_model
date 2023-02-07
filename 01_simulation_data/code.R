set.seed(100)

normal_dist_x <- rnorm(n = 100,
            mean = 3,
            sd =  sqrt(2))
log_normal_dist_ｘ<-exp(normal_dist_x)

x<-log_normal_dist_ｘ

#対数正規分布にする y<-exp(x) 対数正規分布は、対数を取ると正規分布になればいい
#
#sample_fracはブートストラップできる
#ブートストラップ 一様分布を

hist(x)

png("./output/hist_break_10.png")
hist_x_break_10 <- hist(x,
     breaks = 10,main="histgram of x break 10")
dev.off()

png("./output/hist_break_20.png")
hist_x_break_20 <-hist(x,
     breaks = 20,main="histgram of x break 20")
dev.off()

png("./output/hist_break_5.png")
hist_x_break_5 <-hist(x,
     breaks = 5,main="histgram of x break 5")
dev.off()

# output_dir_path <- here::here("output")
# file_path <- here::here(output_dir_path,"hist_break_5.png")
# ggsave(file = file_path, plot = hist)

density_x <-density(x)
density_x_bw_20 <-density(x,bw=20)
density_x_bw_10 <-density(x,bw=10)

dev.new()
png("./output/hist_10_density.png")
hist(x, freq = FALSE, breaks = 10,
     main="Histgram break 10 and Density",
     ylim=c(0,0.018))
lines(density_x,col="blue",lwd=2)
lines(density_x_bw_10,col="red",lwd=2)
lines(density_x_bw_20,col="yellow",lwd=2)
legend("topright", legend = c("blue: bw=default","red: bw=10","yellow: bw=20"))
dev.off()

png("./output/hist_20_density.png")
hist(x, freq = FALSE, breaks = 20,
     main="Histgram break 20 and Density",
     ylim=c(0,0.025))
lines(density_x,col="blue",lwd=2)
lines(density_x_bw_10,col="red",lwd=2)
lines(density_x_bw_20,col="yellow",lwd=2)
legend("topright", legend = c("blue: bw=default","red: bw=10","yellow: bw=20"))
dev.off()
#densityのbwのnrd0は望ましくないデフォルト,SJが適している宋

png("./output/hist_50_density.png")
hist(x, freq = FALSE, breaks = 50,
     main="Histgram break 50 and Density",
     ylim=c(0,0.030))
lines(density_x,col="blue",lwd=2)
lines(density_x_bw_10,col="red",lwd=2)
lines(density_x_bw_20,col="yellow",lwd=2)
legend("topright", legend = c("blue: bw=default","red: bw=10","yellow: bw=20"))
dev.off()


normal_dist_x <- rnorm(n = 100,
                       mean = 3,
                       sd = sqrt(2))
log_normal_dist_ｘ<- exp(normal_dist_x)

x <- log_normal_dist_ｘ
error<-rnorm(100,0,200)
#xを最大値で割ることによって、累乗の項が大きすぎて、一乗がつぶれないようにする。
z <- 0.1*x-0.3*(x/500)^3+0.5*(x/500)^5-0.7*(x/500)^7+error

plot(x,z,main="scatter plot x,z")
install.packages("quantreg")
library(quantreg)

tau  <- c(0.25,0.5,0.75)
# 分位点回帰モデルを推定する
quant_reg  <- rq(z~x, tau=tau)
quant_reg

quant_reg_025  <- rq(z~x, tau=0.25)
quant_reg_05  <- rq(z~x, tau=0.5)
quant_reg_075  <- rq(z~x, tau=0.75)

plot(z,x)
install.packages("estimatr")
library(estimatr)

ols_reg<-lm_robust(z~x)

regs <- list("25%分位点回帰"=quant_reg_025,"50%分位点回帰"=quant_reg_05,
             "75%分位点回帰"=quant_reg_075,
             "OLS"=ols_reg)

install.packages("modelsummary")
modelsummary::msummary(regs,fmt = '%.2f')

msummary(regs,fmt = '%.2f',title="time detrend Linear RegとFD estimator比較",coef_map = var_nam)
msummary(regs,fmt = '%.2f',title="P値 time detrend Linear RegとFD estimator比較",coef_map = var_nam,estimate = "p.value")


#非線形回帰

y<-vector()
x[1]
y[1]
for(i in 1:100){
  if(x[i]+error[i]>=4){
    y[i]<-1
  }else{
    y[i]<-0
  }  
}
df<-data.frame(x,y)
View(df)
probit_reg <-glm(df$y~df$x,family=binomial(probit))
plot(df$y)
plot(df$y,df$x,col=c(1,2))
#限界効果の形にする必要がある
x_b <-predict(probit_reg) 
x_b
mean(x_b)
f_y <-dnorm(mean(x_b)) 
dnorm(259.1269)
f_y
marginal_effect_on_average <- f_y*coef(probit_reg)
marginal_effect_on_average

install.packages("mfx")
library(mfx)
probitmfx(formula = y ~ x, data = df)


linear_probit_reg<-lm_robust(y~x)
linear_probit_reg

modelsummary::msummary(regs,fmt = '%.2f')
regs <- list("25%分位点回帰"=quant_reg_025,"50%分位点回帰"=quant_reg_05,
             "75%分位点回帰"=quant_reg_075,
             "OLS"=ols_reg)
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
sample_2_vec_x<-cbind(View_bootstrap_sample_2$x,
                      View_bootstrap_sample_2$x_2,
                      View_bootstrap_sample_2$x_3,
                      View_bootstrap_sample_2$x_4,
                      View_bootstrap_sample_2$x_5,
                      View_bootstrap_sample_2$x_6,
                      View_bootstrap_sample_2$x_7,
                      View_bootstrap_sample_2$x_8,
                      View_bootstrap_sample_2$x_9,
                      View_bootstrap_sample_2$x_10,
                      View_bootstrap_sample_2$x_11,
                      View_bootstrap_sample_2$x_12,
                      View_bootstrap_sample_2$x_13,
                      View_bootstrap_sample_2$x_14,
                      View_bootstrap_sample_2$x_15,
                      View_bootstrap_sample_2$x_16,
                      View_bootstrap_sample_2$x_17,
                      View_bootstrap_sample_2$x_18,
                      View_bootstrap_sample_2$x_19,
                      View_bootstrap_sample_2$x_20
)
sample_3_vec_x<-cbind(View_bootstrap_sample_3$x,
                      View_bootstrap_sample_3$x_2,
                      View_bootstrap_sample_3$x_3,
                      View_bootstrap_sample_3$x_4,
                      View_bootstrap_sample_3$x_5,
                      View_bootstrap_sample_3$x_6,
                      View_bootstrap_sample_3$x_7,
                      View_bootstrap_sample_3$x_8,
                      View_bootstrap_sample_3$x_9,
                      View_bootstrap_sample_3$x_10,
                      View_bootstrap_sample_3$x_11,
                      View_bootstrap_sample_3$x_12,
                      View_bootstrap_sample_3$x_13,
                      View_bootstrap_sample_3$x_14,
                      View_bootstrap_sample_3$x_15,
                      View_bootstrap_sample_3$x_16,
                      View_bootstrap_sample_3$x_17,
                      View_bootstrap_sample_3$x_18,
                      View_bootstrap_sample_3$x_19,
                      View_bootstrap_sample_3$x_20
)

sample_4_vec_x<-cbind(View_bootstrap_sample_4$x,
                      View_bootstrap_sample_4$x_2,
                      View_bootstrap_sample_4$x_3,
                      View_bootstrap_sample_4$x_4,
                      View_bootstrap_sample_4$x_5,
                      View_bootstrap_sample_4$x_6,
                      View_bootstrap_sample_4$x_7,
                      View_bootstrap_sample_4$x_8,
                      View_bootstrap_sample_4$x_9,
                      View_bootstrap_sample_4$x_10,
                      View_bootstrap_sample_4$x_11,
                      View_bootstrap_sample_4$x_12,
                      View_bootstrap_sample_4$x_13,
                      View_bootstrap_sample_4$x_14,
                      View_bootstrap_sample_4$x_15,
                      View_bootstrap_sample_4$x_16,
                      View_bootstrap_sample_4$x_17,
                      View_bootstrap_sample_4$x_18,
                      View_bootstrap_sample_4$x_19,
                      View_bootstrap_sample_4$x_20
)
sample_5_vec_x<-cbind(View_bootstrap_sample_5$x,
                      View_bootstrap_sample_5$x_2,
                      View_bootstrap_sample_5$x_3,
                      View_bootstrap_sample_5$x_4,
                      View_bootstrap_sample_5$x_5,
                      View_bootstrap_sample_5$x_6,
                      View_bootstrap_sample_5$x_7,
                      View_bootstrap_sample_5$x_8,
                      View_bootstrap_sample_5$x_9,
                      View_bootstrap_sample_5$x_10,
                      View_bootstrap_sample_5$x_11,
                      View_bootstrap_sample_5$x_12,
                      View_bootstrap_sample_5$x_13,
                      View_bootstrap_sample_5$x_14,
                      View_bootstrap_sample_5$x_15,
                      View_bootstrap_sample_5$x_16,
                      View_bootstrap_sample_5$x_17,
                      View_bootstrap_sample_5$x_18,
                      View_bootstrap_sample_5$x_19,
                      View_bootstrap_sample_5$x_20
)

lasso.model.cv <- cv.glmnet(x=sample_1_vec_x, y=z ,
                            family = "gaussian", alpha = 1)
lasso.model.cv <- cv.glmnet(x=sample_2_vec_x, y=z ,
                            family = "gaussian", alpha = 1)
