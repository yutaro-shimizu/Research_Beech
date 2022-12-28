#### Install packages ####
# install.packages("psych")
# install.packages("MuMIn")
# install.packages("caret") #　交差検証用
# install.packages("performanceAnalytics")

#import data
d <- R_Beech_Roots
attach(d)
names(d)

#多重共線性の把握（相関係数0.7以上の説明変数対は除外する）
library(psych)
cor(d[3:21], use="pairwise.complete.obs", method="p") 

#共線性の可視化と線形性の確認
library(PerformanceAnalytics)
vars <- c("Height",
          "T.R",
          "buried_trunk_.",
          "buried_trunk_weight",
          "buried_trunk_years")
new_data <- R_Beech_Roots[vars]
chart.Correlation(new_data)

######計測項目######
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
#3 TR比T/R

#主根のパラメータ
#1 主根の斜面方向の伸張巾root width
#2 主根の斜面鉛直方向の深度root depth
#3 地下部重量root_weight

#埋幹部のパラメータ：
#1 埋幹部buried_trunk_weight
#2 埋幹部割合buried_trunk_%
#3 発芽点直径D_origine
#4 発芽点年輪数D_origine_age
#5 埋幹部の年数buried_trunk_years

#用語の参考論文：chrome-extension://oemmndcbldboiebfnladdacbdfmadadm/https://www.jstage.jst.go.jp/article/jjfs1953/76/1/76_1_18/_pdf
############

# ggplot2のためdfの形を変換
df_total <- R_Beech_Roots[c("total_weight",
                            "buried_trunk_weight",
                            "trunk_weight",
                            "root_weight")]

df_trunk_age <- R_Beech_Roots[c("buried_trunk_years",
                                "buried_trunk_weight",
                                "trunk_weight",
                                "root_weight")]

df_origin_age <- R_Beech_Roots[c("D_origine_age",
                                 "buried_trunk_weight",
                                 "trunk_weight",
                                 "root_weight")]

# 3つの説明変数に合わせてdfをlong formatに変換
library(reshape2)
longtotal <- melt(data=df_total,
                  id.vars="total_weight",
                  value.name="weight")

longtrunkage <- melt(data=df_trunk_age,
                     id.vars="buried_trunk_years",
                     value.name="weight")

longoriginage <- melt(data=df_origin_age,
                      id.vars="D_origine_age",
                      value.name="weight")

# scaling lawに合わせてlog transformationした上で、グラフ化
plot_total <- ggplot(longtotal, aes(log(total_weight), log(weight), color=variable)) + geom_point() + geom_smooth(method="lm")
plot_originage <- ggplot(longoriginage, aes(log(D_origine_age), log(weight), color=variable)) + geom_point() + geom_smooth(method="lm")
plot_trunkage <- ggplot(longtrunkage, aes(log(buried_trunk_years), log(weight), color=variable)) + geom_point() + geom_smooth(method="lm")

plot_total
plot_originage
plot_trunkage

# scaling lawに合わせてlog transformationした上で、指数を求める
lm_buried_weight <- lm(log(buried_trunk_weight) ~ log(total_weight), data=R_Beech_Roots)
lm_trunk_weight <- lm(log(trunk_weight) ~ log(total_weight), data=R_Beech_Roots)
lm_root_weight <- lm(log(root_weight) ~ log(total_weight), data=R_Beech_Roots)

summary(lm_buried_weight)
summary(lm_trunk_weight)
summary(lm_root_weight)

######GLM、ガンマ分布######
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
# #### AICによるモデル選択 ####
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
# # sim(bestmodel,n.sims=100)
# # quantile(sim.lm@coef[,3], probs = c(0.025, 0.975)) #burried_trunk_weight
# # quantile(sim.lm@coef[,5], probs = c(0.025, 0.975)) #T.R
# 
# #### 交差検証 ####
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