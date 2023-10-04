#### Install packages ####
# install.packages("reshape")
# install.packages("lmodel2")
# install.packages("ggplot2")
# install.packages("ggpmisc")
# install.packages("arm")
# install.packages("psych")
# install.packages("MuMIn")
# install.packages("caret") #　交差検証用
# install.packages("PerformanceAnalytics")
# install.packages("sensemakr")

library(lmodel2)
library(reshape)
library(ggpmisc)
library(arm)
library(ggplot2)

#計測項目######
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
### surface area ####
df_surfacearea <- R_Beech_Roots[c("log10_wet_whole_mass",
                               "log10_top_surfacearea",
                               "log10_bottom_surfacearea")]

# fix column names for visualization
colnames(df_surfacearea) <- c('whole_plant','aboveground','belowground')

longsurfacearea <- melt(data=df_surfacearea,
                    id.vars="whole_plant",
                    value.name="area")

plot_surfacearea <- ggplot(longsurfacearea, aes(whole_plant, value, color=variable)) + 
                 geom_point() + 
                 stat_ma_line(method = "SMA", se=FALSE) +
                 labs(title="A",x = "Log Whole-plant fresh mass (kg)", y = "Log Surface Area (m^2)", colour = " ")
plot_surfacearea

lm_topsurfacearea <- lmodel2(log10(top_surfacearea) ~ log10(wet_whole_mass), data=R_Beech_Roots, "interval", "interval", 99)
lm_bottomsurfacearea <- lmodel2(log10(bottom_surfacearea) ~ log10(wet_whole_mass), data=R_Beech_Roots, "interval", "interval", 99)

lm_topsurfacearea
lm_bottomsurfacearea

### fresh/wet mass #####
#df with three body organs (shoot, root, nemagari)
df_wet_mass3 <- R_Beech_Roots[c("log10_wet_whole_mass",
                               "log10_wet_shoot_mass",
                               "log10_wet_nemagari_mass",
                               "log10_wet_root_mass")]

#df with two body organs (aboveground, belowground)
df_wet_mass2 <- R_Beech_Roots[c("log10_wet_whole_mass",
                                "log10_wet_shoot_mass",
                                "log10_belowground_mass")]

# fix column names for visualization
colnames(df_wet_mass3) <- c('whole_plant','shoot','nemagari','root')
colnames(df_wet_mass2) <- c('whole_plant','aboveground','belowground')


longwet3 <- melt(data=df_wet_mass3, 
                 id.vars="whole_plant",
                 value.name="mass")

longwet2 <- melt(data=df_wet_mass2, 
                 id.vars="whole_plant",
                 value.name="mass")

plot_wet_mass3 <- ggplot(longwet3, aes(whole_plant, value, color=variable)) + 
                  geom_point() + 
                  stat_ma_line(method = "SMA", se=FALSE) +
                  labs(title="C",x = "Log Whole-plant fresh mass (kg)", y = "Log Fresh mass (kg)", colour = " ")
plot_wet_mass3

plot_wet_mass2 <- ggplot(longwet2, aes(whole_plant, value, color=variable)) + 
                  geom_point() + 
                  stat_ma_line(method = "SMA", se=FALSE) +
                  labs(title="B",x = "Log Whole-plant fresh mass (kg)", y = "Log Fresh mass (kg)", colour = " ")
plot_wet_mass2

# scaling lawに合わせてlog10 transformationした上で、指数を求める
lm_wet_nemagari <- lmodel2(log10(wet_nemagari_mass) ~ log10(wet_whole_mass), data=R_Beech_Roots, "interval", "interval", 99)
lm_wet_trunk_weight <- lmodel2(log10(wet_shoot_mass) ~ log10(wet_whole_mass), data=R_Beech_Roots, "interval", "interval", 99)
lm_wet_root_weight <- lmodel2(log10(wet_root_mass) ~ log10(wet_whole_mass), data=R_Beech_Roots, "interval", "interval", 99)

lm_wet_aboveground <- lmodel2(aboveground ~ whole_plant, data=df_wet_mass2, "interval", "interval", 99)
lm_wet_belowground <- lmodel2(belowground ~ whole_plant, data=df_wet_mass2, "interval", "interval", 99)

lm_wet_nemagari
lm_wet_trunk_weight
lm_wet_aboveground
lm_wet_belowground

### dry mass ####
df_dry_nemagari <- R_Beech_Roots[c("log10_dry_whole_mass",
                            "log10_dry_shoot_mass",
                            "log10_dry_nemagari_mass",
                            "log10_dry_root_mass")]
colnames(df_dry_nemagari) <- c('whole_plant','shoot','nemagari','root')

# dfをlong formatに変換
longdrynemagari <- melt(data=df_dry_nemagari,
                  id.vars="whole_plant",
                  value.name="weight")

# scaling lawに合わせてlog10 transformationした上で、グラフ化
plot_drybury <- ggplot(longdrynemagari, aes(whole_plant, value, color=variable)) + 
                geom_point() + 
                stat_ma_line(method = "SMA", se=FALSE)
plot_drybury

# scaling lawに合わせてlog10 transformationした上で、指数を求める
lm_buried_weight <- lmodel2(log10(dry_nemagari_mass) ~ log10(dry_whole_mass), data=R_Beech_Roots, "interval", "interval", 99)
lm_trunk_weight <- lmodel2(log10(dry_shoot_mass) ~ log10(dry_whole_mass), data=R_Beech_Roots, "interval", "interval", 99)
lm_root_weight <- lmodel2(log10(dry_root_mass) ~ log10(dry_whole_mass), data=R_Beech_Roots, "interval", "interval", 99)

lm_buried_weight
lm_trunk_weight
lm_root_weight

### SR #####
df_wet_RS <- R_Beech_Roots[c("wet_whole_mass",
                               "R.S")]

# fix column names for visualization
colnames(df_wet_RS) <- c('whole_plant','RootShoot')

plot_wet_RS <- ggplot(df_wet_RS, aes(log10(whole_plant), log10(RootShoot))) + 
               geom_point(color="red") + 
               stat_ma_line(method = "SMA", se=FALSE, color="red")
plot_wet_RS

# scaling lawに合わせてlog10 transformationした上で、指数を求める
lm_wet_RS <- lmodel2(log10(R.S) ~ log10(wet_whole_mass), data=R_Beech_Roots, "interval", "interval", 99)
lm_wet_RS


# ### mass fraction ####
# df_wet_percentage <- R_Beech_Roots[c("wet_whole_mass",
#                                    "root_percentage",
#                                    "nemagari_percentage")]
# df_wet_percentage$root_nemagari_percentage <- df_wet_percentage$root_percentage + 
#                                               df_wet_percentage$nemagari_percentage
# 
# colnames(df_wet_percentage) <- c('whole_plant_mass','root','nemagari','root + nemagari')
# 
# # dfをlong formatに変換
# longrootpercentage <- melt(data=df_wet_percentage,
#                         id.vars="whole_plant_mass",
#                         value.name="percentage")
# 
# # scaling lawに合わせてlog10 transformationした上で、グラフ化
# plot_rootpercentage <- ggplot(longrootpercentage, aes(log10(whole_plant_mass), percentage, color=variable)) + 
#   geom_point() + stat_ma_line(method = "SMA", se=FALSE) +labs(x = "Log fresh mass (g)", y = "Root fraction (%)", colour = " ")
# plot_rootpercentage
# 
# # scaling lawに合わせてlog10 transformationした上で、指数を求める
# lm_root_percentage <- lm(root_percentage ~ log10(wet_whole_mass), data=R_Beech_Roots)
# lm_nemagari_percentage <- lm(nemagari_percentage ~ log10(wet_whole_mass), data=R_Beech_Roots)
# lm_root_nemagari_weight <- lm(root_nemagari_percentage ~ log10(whole_plant_mass), data=df_wet_percentage)
# 
# summary(lm_root_percentage)
# summary(lm_nemagari_percentage)
# summary(lm_root_nemagari_weight)
# 
# # simulate coefficients (King et al., 2000)
# CI_simulator(lm_root_percentage)
# CI_simulator(lm_nemagari_percentage)
# CI_simulator(lm_root_nemagari_weight)


#### 共線性の可視化と線形性の確認 ####
#多重共線性の把握（相関係数0.7以上の説明変数対は除外する）
# library(psych)
# library(PerformanceAnalytics)
# cor(R_Beech_Roots[3:21], use="pairwise.complete.obs", method="p")
# 
# vars <- c("wet_whole_mass",
#           "wet_nemagari_mass",
#           "top_surfacearea",
#           "bottom_surfacearea")
# new_data <- R_Beech_Roots[vars]
# chart.Correlation(new_data)

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
