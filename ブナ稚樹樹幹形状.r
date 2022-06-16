d <- read.csv("R_Beech_Roots.csv")
attach(d)
names(d)

#多重共線性の把握（相関係数0.7以上の説明変数対は除外する
library(psych)
cor(d[3:21], use="pairwise.complete.obs", method="p") 

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

#GLM、ガンマ分布
model <- glm(
	Height　#応答変数は樹高（地上部の垂直伸長が森林構成種としての基盤）
	~L.H	#地上部の長さ/樹高
	+root.width	#根の斜面水平方向の伸張幅
	+T.R		　　#T/R比
	+buried_trunk_.　#埋幹部の重量割合
	+buried_trunk_weight　#埋幹部の重量(root_weightと高い相関)
	+buried_trunk_years　#每幹部の年数
	+trunk_inclination　#主幹の傾斜角
	+slope　　　#斜面傾斜　
	+snow_depth,　#平均最深積雪
	Gamma)

#説明変数：多重共線性の把握から、LH比、、TR比、
#節目変数つづき：タップルートの重量割合、タップルートの重量、タップルートの年数、斜面傾斜、サイトの平均最大積雪深
#Γ分布のGLM

library(MuMIn)
options(na.action = "na.fail")
dredge(model, rank="AIC") 

#AIC＜2のモデルを有効とする

#最適モデル
summary(
    glm(Height
	 ~ root.width
	 + T.R
	 + buried_trunk_.
	 + buried_trunk_years, 
  Gamma))　

#2ndモデル
summary(
  glm(Height
      ~ root.width
      + T.R
      + buried_trunk_.
      + buried_trunk_years
      + slope, 
      Gamma))　

#3rdモデル
summary(
  glm(Height
      ~ root.width
      + T.R
      + buried_trunk_.
      + buried_trunk_years
      + snow_depth, 
      Gamma))　

#4thモデル
summary(
  glm(Height
      ~ root.width
      + T.R
      + buried_trunk_.
      + buried_trunk_years
      + trunk_inclination, 
      Gamma))　

#5thモデル
summary(
  glm(Height
      ~ root.width
      + T.R
      + buried_trunk_.
      + buried_trunk_years
      + buried_trunk_weight,
      Gamma))　

#6thモデル
summary(
  glm(Height
      ~ root.width
      + T.R
      + buried_trunk_.
      + buried_trunk_years
      + L.H,
      Gamma))　


