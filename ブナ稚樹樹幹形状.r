#### Install packages ####
install.packages("psych")
install.packages("MuMIn")
install.packages("caret") #　交差検証用
install.packages("PerformanceAnalytics")
install.packages("sensemakr")
install.packages("arm")
install.packages("ggplot2")
install.packages("scales")


#多重共線性の把握（相関係数0.7以上の説明変数対は除外する）
library(psych)
cor(R_Beech_Roots[3:21], use="pairwise.complete.obs", method="p") 

#共線性の可視化と線形性の確認
library(PerformanceAnalytics)
vars <- c("Height",
          "S.R",
          "nemagari_percentage",
          "wet_nemagari_mass",
          "buried_trunk_years")
new_data <- R_Beech_Roots[vars]
chart.Correlation(new_data)

####計測項目######
#環境パラメータ：
#1 最大積雪深（2003-04年冬から2020-21年冬までの17冬期の平均）snow_depth
#2 斜面傾斜slope：　

#地上部のパラメータ：
#1 地際直径D0
#2 地際年輪数D0_age
#3 樹高Height
#4 樹幹長Length
#5 地上部重量trunk_weight
#6 主幹の傾きtrunk_inclination

#全体のパラメータ：
#1 全長[樹幹長＋主根長]total_length
#2 LH比L/H
#3 SR比S.R
#4 全重量における地上部重量割合shoot_trunk_%

#主根のパラメータ
#1 主根の斜面方向の伸張巾root width
#2 主根の斜面鉛直方向の深度root depth
#3 地下部重量root_weight
#4 全重量における地下部重量割合root_trunk_%

#埋幹部のパラメータ：
#1 埋幹部buried_trunk_weight
#2 全重量における埋幹部重量割合nemagari_trunk_%
#3 地下部における埋幹部重量割合buried_trunk_.
#4 発芽点直径D_origin
#5 発芽点年輪数D_origin_age
#6 埋幹部の年数buried_trunk_years

#用語の参考論文：chrome-extension://oemmndcbldboiebfnladdacbdfmadadm/https://www.jstage.jst.go.jp/article/jjfs1953/76/1/76_1_18/_pdf

#### ggplot2を用いて作図 #####
### dry mass ####
df_dry_nemagari <- R_Beech_Roots[c("dry_whole_mass",
                            "dry_shoot_mass",
                            "dry_nemagari_mass",
                            "dry_root_mass")]
colnames(df_dry_nemagari) <- c('whole_plant','shoot','nemagari','root')

# dfをlong formatに変換
library(reshape2)
longdrynemagari <- melt(data=df_dry_nemagari,
                  id.vars="whole_plant",
                  value.name="weight")

# scaling lawに合わせてlog10 transformationした上で、グラフ化
library(ggplot2)
plot_drybury <- ggplot(longdrynemagari, aes(log10(whole_plant), log10(weight), color=variable)) + 
  geom_point() + geom_smooth(method="lm",se=FALSE) 
plot_drybury

# scaling lawに合わせてlog10 transformationした上で、指数を求める
lm_buried_weight <- lm(log10(dry_nemagari_mass) ~ log10(dry_whole_mass), data=R_Beech_Roots)
lm_trunk_weight <- lm(log10(dry_shoot_mass) ~ log10(dry_whole_mass), data=R_Beech_Roots)
lm_root_weight <- lm(log10(dry_root_mass) ~ log10(dry_whole_mass), data=R_Beech_Roots)

summary(lm_buried_weight)
summary(lm_trunk_weight)
summary(lm_root_weight)

# 各値の９５％信頼区間を求める　simulate coefficients (King et al., 2000)
library(arm)
sim.lm_trunk_weight <- sim(lm_trunk_weight, 1000)
quantile(sim.lm_trunk_weight@coef[,2], probs = c(0.025, 0.5, 0.975))
quantile(sim.lm_trunk_weight@coef[,1], probs = c(0.025, 0.5, 0.975))
mean(sim.lm_trunk_weight@coef[,2])
mean(sim.lm_trunk_weight@coef[,1])

sim.lm_buried_weight <- sim(lm_buried_weight, 1000)
quantile(sim.lm_buried_weight@coef[,2], probs = c(0.025, 0.5, 0.975))
quantile(sim.lm_buried_weight@coef[,1], probs = c(0.025, 0.5, 0.975))
mean(sim.lm_buried_weight@coef[,2])
mean(sim.lm_buried_weight@coef[,1])

sim.lm_root_weight <- sim(lm_root_weight, 1000)
quantile(sim.lm_root_weight@coef[,2], probs = c(0.025, 0.5, 0.975))
quantile(sim.lm_root_weight@coef[,1], probs = c(0.025, 0.5, 0.975))
mean(sim.lm_root_weight@coef[,2])
mean(sim.lm_root_weight@coef[,1])

### fresh/wet mass #####
df_wet_bury <- R_Beech_Roots[c("wet_whole_mass",
                               "wet_shoot_mass",
                               "wet_nemagari_mass",
                               "wet_root_mass")]

# fix column names for visualization
colnames(df_wet_bury) <- c('whole_plant','shoot','nemagari','root')
longwetbury <- melt(data=df_wet_bury,
                 id.vars="whole_plant",
                 value.name="weight")

plot_wet_bury <- ggplot(longwetbury, aes(log10(whole_plant), log10(weight), color=variable)) + 
                    　　geom_point() + geom_smooth(method="lm",se=FALSE)
plot_wet_bury


# scaling lawに合わせてlog10 transformationした上で、指数を求める
lm_wet_nemagari <- lm(log10(wet_nemagari_mass) ~ log10(wet_whole_mass), data=R_Beech_Roots)
lm_wet_trunk_weight <- lm(log10(wet_shoot_mass) ~ log10(wet_whole_mass), data=R_Beech_Roots)
lm_wet_root_weight <- lm(log10(wet_root_mass) ~ log10(wet_whole_mass), data=R_Beech_Roots)

summary(lm_wet_nemagari)
summary(lm_wet_trunk_weight)
summary(lm_wet_root_weight)

# simulate coefficients (King et al., 2000)
sim.lm_wet_nemagari <- sim(lm_wet_nemagari, 1000)
sim.lm_trunk_weight <- sim(lm_trunk_weight, 1000)
quantile(sim.lm_wet_nemagari@coef[,2], probs = c(0.025, 0.5, 0.975))
quantile(sim.lm_wet_nemagari@coef[,1], probs = c(0.025, 0.5, 0.975))
mean(sim.lm_wet_nemagari@coef[,2])
mean(sim.lm_wet_nemagari@coef[,1])

sim.lm_wet_trunk_weight <- sim(lm_wet_trunk_weight, 1000)
quantile(sim.lm_wet_trunk_weight@coef[,2], probs = c(0.025, 0.5, 0.975))
quantile(sim.lm_wet_trunk_weight@coef[,1], probs = c(0.025, 0.5, 0.975))
mean(sim.lm_wet_trunk_weight@coef[,2])
mean(sim.lm_wet_trunk_weight@coef[,1])

sim.lm_wet_root_weight <- sim(lm_wet_root_weight, 1000)
quantile(sim.lm_wet_root_weight@coef[,2], probs = c(0.025, 0.5, 0.975))
quantile(sim.lm_wet_root_weight@coef[,1], probs = c(0.025, 0.5, 0.975))
mean(sim.lm_wet_root_weight@coef[,2])
mean(sim.lm_wet_root_weight@coef[,1])

### Root to Shoot Ratio #####
df_wet_RS <- R_Beech_Roots[c("wet_whole_mass",
                               "R.S")]

# fix column names for visualization
colnames(df_wet_RS) <- c('whole_plant','RootShoot')

plot_wet_RS <- ggplot(df_wet_RS, aes(log10(whole_plant), log10(RootShoot))) + 
  geom_point(color="red") + geom_smooth(method="lm",se=FALSE,color="red")
plot_wet_RS

# scaling lawに合わせてlog10 transformationした上で、指数を求める
lm_wet_RS <- lm(log10(R.S) ~ log10(wet_whole_mass), data=R_Beech_Roots)
summary(lm_wet_RS)

# simulate coefficients (King et al., 2000)
sim.lm_wet_RS <- sim(lm_wet_RS, 1000)
quantile(sim.lm_wet_RS@coef[,2], probs = c(0.025, 0.5, 0.975))
quantile(sim.lm_wet_RS@coef[,1], probs = c(0.025, 0.5, 0.975))
mean(sim.lm_wet_RS@coef[,2])
mean(sim.lm_wet_RS@coef[,1])

### mass percentage ####
df_wet_percentage <- R_Beech_Roots[c("wet_whole_mass",
                                   "root_percentage",
                                   "nemagari_percentage")]
df_wet_percentage$root_nemagari_percentage <- df_wet_percentage$root_percentage + 
                                              df_wet_percentage$nemagari_percentage

colnames(df_wet_percentage) <- c('whole_plant_mass','root percentage','nemagari percentage','root_nemagari_percentage')

# dfをlong formatに変換
longrootpercentage <- melt(data=df_wet_percentage,
                        id.vars="whole_plant_mass",
                        value.name="percentage")

# scaling lawに合わせてlog10 transformationした上で、グラフ化
library(ggplot2)
plot_rootpercentage <- ggplot(longrootpercentage, aes(log10(whole_plant_mass), percentage, color=variable)) + 
  geom_point() + geom_smooth(method="lm",se=FALSE) 
plot_rootpercentage

# scaling lawに合わせてlog10 transformationした上で、指数を求める
lm_root_percentage <- lm(root_percentage ~ log10(wet_whole_mass), data=R_Beech_Roots)
lm_nemagari_percentage <- lm(nemagari_percentage ~ log10(wet_whole_mass), data=R_Beech_Roots)
lm_root_nemagari_weight <- lm(root_nemagari_percentage ~ log10(whole_plant_mass), data=df_wet_percentage)

summary(lm_root_percentage)
summary(lm_nemagari_percentage)
summary(lm_root_nemagari_weight)

# simulate coefficients (King et al., 2000)
library(arm)
sim.lm_root_percentage <- sim(lm_root_percentage, 1000)
quantile(sim.lm_root_percentage@coef[,2], probs = c(0.025, 0.5, 0.975))
quantile(sim.lm_root_percentage@coef[,1], probs = c(0.025, 0.5, 0.975))
mean(sim.lm_root_percentage@coef[,2])
mean(sim.lm_root_percentage@coef[,1])

sim.lm_nemagari_percentage <- sim(lm_nemagari_percentage, 1000)
quantile(sim.lm_nemagari_percentage@coef[,2], probs = c(0.025, 0.5, 0.975))
quantile(sim.lm_nemagari_percentage@coef[,1], probs = c(0.025, 0.5, 0.975))
mean(sim.lm_nemagari_percentage@coef[,2])
mean(sim.lm_buried_weight@coef[,1])

sim.lm_root_nemagari_weight <- sim(lm_root_nemagari_weight, 1000)
quantile(sim.lm_root_nemagari_weight@coef[,2], probs = c(0.025, 0.5, 0.975))
quantile(sim.lm_root_nemagari_weight@coef[,1], probs = c(0.025, 0.5, 0.975))
mean(sim.lm_root_nemagari_weight@coef[,2])
mean(sim.lm_root_nemagari_weight@coef[,1])

#### #descriptivestats ####
lm_RS <- lm(R.S ~ log10(wet_whole_mass), data=R_Beech_Roots)
lm_nemagariD0age <- lm(wet_whole_mass ~ D0_age, data=R_Beech_Roots)
lm_nemagariD_originage <- lm(wet_whole_mass ~ D_origin_age, data=R_Beech_Roots)
lm_nemagarisize <- lm(log10(wet_nemagari_mass) ~ log10(wet_whole_mass), data=R_Beech_Roots)

summary(lm_RS)
summary(lm_nemagariD0age)
summary(lm_nemagariD_originage)
summary(lm_nemagarisize)

# simulate coefficients (King et al., 2000)
library(arm)
sim.lm_RS <- sim(lm_RS, 1000)
quantile(sim.lm_RS@coef[,2], probs = c(0.025, 0.5, 0.975))
quantile(sim.lm_RS@coef[,1], probs = c(0.025, 0.5, 0.975))
mean(sim.lm_RS@coef[,2])
mean(sim.lm_RS@coef[,1])

sim.lm_nemagariD0age <- sim(lm_nemagariD0age, 1000)
quantile(sim.lm_nemagariD0age@coef[,2], probs = c(0.025, 0.5, 0.975))
quantile(sim.lm_nemagariD0age@coef[,1], probs = c(0.025, 0.5, 0.975))
mean(sim.lm_nemagariD0age@coef[,2])
mean(sim.lm_nemagariD0age@coef[,1])

sim.lm_nemagariD_originage <- sim(lm_nemagariD_originage, 1000)
quantile(sim.lm_nemagariD_originage@coef[,2], probs = c(0.025, 0.5, 0.975))
quantile(sim.lm_nemagariD_originage@coef[,1], probs = c(0.025, 0.5, 0.975))
mean(sim.lm_nemagariD_originage@coef[,2])
mean(sim.lm_nemagariD_originage@coef[,1])

sim.lm_nemagarisize <- sim(lm_nemagarisize, 1000)
quantile(sim.lm_nemagarisize@coef[,2], probs = c(0.025, 0.5, 0.975))
quantile(sim.lm_nemagarisize@coef[,1], probs = c(0.025, 0.5, 0.975))
mean(sim.lm_nemagarisize@coef[,2])
mean(sim.lm_nemagarisize@coef[,1])

####GLM、ガンマ分布######
# model_glm <- glm(Height #応答変数は樹高（地上部の垂直伸長が森林構成種としての基盤）
# ~L.H	#地上部の長さ/樹高
# +root.width	#根の斜面水平方向の伸張幅
# +T.R		　　#T/R比 grams ratio
# +buried_trunk_.　#埋幹部の重量割合
# +buried_trunk_weight　#埋幹部の重量(root_weightと高い相関)
# +buried_trunk_years　#每幹部の年数
# +trunk_inclination　#主幹の傾斜角
# +slope　　　#斜面傾斜
# +snow_depth,　#平均最深積雪
# Gamma)
# 
# #説明変数：多重共線性の把握から、LH比、、TR比、
# #説明変数つづき：タップルートの重量割合、タップルートの重量、タップルートの年数、斜面傾斜、サイトの平均最大積雪深
# #Γ分布のGLM
# 
# #LM、重回帰分析
# model_lm <- lm(Height~
#               L.H
#             +root.width
#             +T.R
#             +buried_trunk_.
#             +buried_trunk_weight
#             +buried_trunk_years
#             +trunk_inclination
#             +slope
#             +snow_depth,
#             data=R_Beech_Roots)
# 
# ### AICによるモデル選択 ####
# library(MuMIn)
# options(na.action = "na.fail")
# ### For GLM with Gamma Distribution 
# dd_glm <- dredge(model_glm, rank="AIC") 
# #最適モデル
# bestmodel_glm <- get.models(dd_glm, 1)[[1]]
# AIC(bestmodel_glm) #190.6157
# summary(bestmodel_glm)
# plot(bestmodel_glm)
# 
# ### For LM with Normal Distribution 
# options(na.action = "na.fail")
# dd_lm <- dredge(model_lm, rank="AIC") 
# #最適モデル
# bestmodel_lm <- get.models(dd_lm, 1)[[1]]
# AIC(bestmodel_lm) #188.9412
# summary(bestmodel_lm)
# plot(bestmodel_lm) ##qqplot（誤差の等分散正規分布）の確認
# 
# # # bootstrap
# # sim(model_lm,100)
# # quantile(sim.lm@coef[,3], probs = c(0.025, 0.975)) #burried_trunk_weight
# # quantile(sim.lm@coef[,5], probs = c(0.025, 0.975)) #T.R
# 
# ### 交差検証 ####
# ### https://www.statology.org/k-fold-cross-validation-in-r/ ###
# library(caret)
# ctrl <- trainControl(method = "cv", number = 5)
# # AIC選択後の各モデルを交差検証（RMSE, Rsquared, MAE）によって比較  
# model_glm <- train(Height ~ buried_trunk_. + buried_trunk_years + 
#                  root.width + T.R , family = Gamma, data = R_Beech_Roots,
#                method = "glm", trControl = ctrl)
# model_lm <- train(Height ~ buried_trunk_. + buried_trunk_weight + 
#                     buried_trunk_years + T.R  , data = R_Beech_Roots,
#                    method = "lm", trControl = ctrl)
# 
# # 交差検証の結果
# # 1）RMSE, RSquared, MAE
# print(model_glm)
# print(model_lm)
# 
# # 2) 最適モデルの変数の再確認
# model_glm$finalModel
# model_lm$finalModel
# 
# # 3) 各リサンプリング後の結果
# model_glm$resample
# model_lm$resample
