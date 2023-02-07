set.seed(100)

normal_dist_x <- rnorm(n = 100,
            mean = 3,
            sd =  sqrt(2))
log_normal_dist_ｘ<-exp(normal_dist_x)

x<-log_normal_dist_ｘ


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

# 分位点回帰モデルを推定する
normal_dist_x <- rnorm(n = 100,
                       mean = 3,
                       sd = sqrt(2))
log_normal_dist_ｘ<- exp(normal_dist_x)

x <- log_normal_dist_ｘ
error<-rnorm(100,0,200)
z <- 0.1*x-0.3*(x/500)^3+0.5*(x/500)^5-0.7*(x/500)^7+error

plot(x,z,main="scatter plot x,z")
install.packages("quantreg")
library(quantreg)

quant_reg_025  <- rq(z~x, tau=0.25)
quant_reg_05  <- rq(z~x, tau=0.5)
quant_reg_075  <- rq(z~x, tau=0.75)

install.packages("estimatr")
library(estimatr)

ols_reg<-lm_robust(z~x)

regs <- list("25%分位点回帰"=quant_reg_025,"50%分位点回帰"=quant_reg_05,
             "75%分位点回帰"=quant_reg_075,
             "OLS"=ols_reg)

install.packages("modelsummary")
modelsummary::msummary(regs,fmt = '%.2f')


#probit
normal_dist_x <- rnorm(n = 100,
                       mean = 3,
                       sd = sqrt(2))
log_normal_dist_ｘ<- exp(normal_dist_x)

x <- log_normal_dist_ｘ
error<-rnorm(100,0,200)
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
df <- data.frame(x,y)
View(df)
probit_reg <- glm(df$y~df$x,family=binomial(probit))
#限界効果の形にする必要がある
x_b <-predict(probit_reg) 
x_b
mean(x_b)
f_y <-dnorm(mean(x_b)) 
f_y
marginal_effect_on_average <- f_y*coef(probit_reg)
marginal_effect_on_average

#結果を二重チェック
install.packages("mfx")
library(mfx)
probitmfx(formula = y ~ x, data = df)

library(estimatr)
linear_probit_reg<-lm_robust(y~x,data=df)
linear_probit_reg


#bootstrap lasso
install.packages("rsample")
library(rsample)
for(i in 2:20){
  assign(paste0("x_",i),(x/500)^i)
}
df<-data.frame(y,x,z,x_2,x_3,x_4,x_5,
               x_6,x_7,x_8,
               x_9,x_10,x_11,x_12,
               x_13,x_14,x_15,x_16,x_17,x_18
               ,x_19,x_20,error)
View(df)

bootstrap_sample <- rsample::bootstraps(df,
                                        times=5)
bootstrap_sample
bootstrap_sample_1 <- bootstrap_sample$splits[[1]]
bootstrap_sample$splits[[1]]
bootstrap_sample_2 <- bootstrap_sample$splits[[2]]
bootstrap_sample_3 <- bootstrap_sample$splits[[3]]
bootstrap_sample_4 <- bootstrap_sample$splits[[4]]
bootstrap_sample_5 <- bootstrap_sample$splits[[5]]
View_bootstrap_sample_1<-as.data.frame(bootstrap_sample_1)
View_bootstrap_sample_2<-as.data.frame(bootstrap_sample_2)
View_bootstrap_sample_3<-as.data.frame(bootstrap_sample_3)
View_bootstrap_sample_4<-as.data.frame(bootstrap_sample_4)
View_bootstrap_sample_5<-as.data.frame(bootstrap_sample_5)

install.packages("glmnet")
library(glmnet)

Lasso_X_1 <- model.matrix( ~  x + x_2 + x_3+x_4+x_5+x_6+x_7+x_8+x_9+x_10
                   + x_11+x_12+x_13+x_14+x_15+x_16+x_17+x_18+x_19+x_20
                   , data=View_bootstrap_sample_1)[,-1]
Lasso_Y_1 <-  View_bootstrap_sample_1[,"z"]
Lasso_X_2 <- model.matrix( ~  x + x_2 + x_3+x_4+x_5+x_6+x_7+x_8+x_9+x_10
                     + x_11+x_12+x_13+x_14+x_15+x_16+x_17+x_18+x_19+x_20
                     , data=View_bootstrap_sample_2)[,-1]
Lasso_Y_2 <-  View_bootstrap_sample_2[,"z"]
Lasso_X_3 <- model.matrix( ~  x + x_2 + x_3+x_4+x_5+x_6+x_7+x_8+x_9+x_10
                     + x_11+x_12+x_13+x_14+x_15+x_16+x_17+x_18+x_19+x_20
                     , data=View_bootstrap_sample_3)[,-1]
Lasso_Y_3 <-  View_bootstrap_sample_3[,"z"]

Lasso_X_4 <- model.matrix( ~  x + x_2 + x_3+x_4+x_5+x_6+x_7+x_8+x_9+x_10
                     + x_11+x_12+x_13+x_14+x_15+x_16+x_17+x_18+x_19+x_20
                     , data=View_bootstrap_sample_4)[,-1]
Lasso_Y_4 <-  View_bootstrap_sample_4[,"z"]


Lasso_X_5 <- model.matrix( ~  x + x_2 + x_3+x_4+x_5+x_6+x_7+x_8+x_9+x_10
                     + x_11+x_12+x_13+x_14+x_15+x_16+x_17+x_18+x_19+x_20
                     , data=View_bootstrap_sample_5)[,-1]
Lasso_Y_5 <-  View_bootstrap_sample_5[,"z"]

lasso.model.x1 <- glmnet(x=Lasso_X_1, y=Lasso_Y_1 ,
                         family = "gaussian", alpha = 0.5, lambda=c(0.3,0.5,0.7))
lasso.model.x1$beta

lasso.model.x2 <- glmnet(x=Lasso_X_2, y=Lasso_Y_2 ,
                         family = "gaussian", alpha = 0.5, lambda=c(0.3,0.5,0.7))
lasso.model.x2$beta

lasso.model.x3 <- glmnet(x=Lasso_X_1, y=Lasso_Y_3 ,
                         family = "gaussian", alpha = 0.5, lambda=c(0.3,0.5,0.7))
lasso.model.x3$beta

lasso.model.x4 <- glmnet(x=Lasso_X_4, y=Lasso_Y_4 ,
                         family = "gaussian", alpha = 0.5, lambda=c(0.3,0.5,0.7))
lasso.model.x4$beta

lasso.model.x5 <- glmnet(x=Lasso_X_5, y=Lasso_Y_5 ,
                         family = "gaussian", alpha = 0.5, lambda=c(0.3,0.5,0.7))
lasso.model.x5$beta

