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
#1 log10_地上部質量 log10_dry_shoot_mass
#2 log10_埋幹部質量 log10_dry_prostrate_mass
#3 log10_主根質量 log10_dry_root_mass
#4 log10_全体質量 log10_dry_whole_mass
#5 log10_地上部表面積 log10_top_surfacearea
#6 log10_地上部表面積 log10_bottom_surfacearea

#### ggplot2を用いて作図 #####
### surface area ####
df_surfacearea <- R_Beech_Roots[c("log10_dry_whole_mass",
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
                 labs(title="A",x = "Log Whole-plant Dry Mass (kg)", y = "Log Surface Area (m^2)", colour = " ")
plot_surfacearea

lm_topsurfacearea <- lmodel2(log10(top_surfacearea) ~ log10(dry_whole_mass), data=R_Beech_Roots, "interval", "interval", 99)
lm_bottomsurfacearea <- lmodel2(log10(bottom_surfacearea) ~ log10(dry_whole_mass), data=R_Beech_Roots, "interval", "interval", 99)

lm_topsurfacearea
lm_bottomsurfacearea
summary(R_Beech_Roots)

### dry mass ####
df_dry_mass3 <- R_Beech_Roots[c("log10_dry_whole_mass",
                                "log10_dry_shoot_mass",
                                "log10_dry_prostrate_mass",
                                "log10_dry_root_mass")]

#df with two body organs (aboveground, belowground)
df_dry_mass2 <- R_Beech_Roots[c("log10_dry_whole_mass",
                                "log10_dry_shoot_mass",
                                "log10_belowground_mass")]

# fix column names for visualization
colnames(df_dry_mass3) <- c('whole_plant','Shoot','Prostrate Stem','Belowground roots')
colnames(df_dry_mass2) <- c('whole_plant','aboveground','belowground')


longdry3 <- melt(data=df_dry_mass3, 
                 id.vars="whole_plant",
                 value.name="mass")

longdry2 <- melt(data=df_dry_mass2, 
                 id.vars="whole_plant",
                 value.name="mass")

plot_dry_mass3 <- ggplot(longdry3, aes(whole_plant, value, color=variable)) + 
  geom_point() + 
  stat_ma_line(method = "SMA", se=FALSE) +
  labs(title="C",x = "Log Whole-plant Dry mass (kg)", y = "Log Dry mass (kg)", colour = " ")
plot_dry_mass3

plot_dry_mass2 <- ggplot(longdry2, aes(whole_plant, value, color=variable)) + 
  geom_point() + 
  stat_ma_line(method = "SMA", se=FALSE) +
  labs(title="B",x = "Log Whole-plant Dry mass (kg)", y = "Log Dry mass (kg)", colour = " ")
plot_dry_mass2

df_dry_prostrate <- R_Beech_Roots[c("log10_dry_whole_mass",
                            "log10_dry_shoot_mass",
                            "log10_dry_prostrate_mass",
                            "log10_dry_root_mass")]
colnames(df_dry_prostrate) <- c('whole_plant','Shoot','Prostrate Root','Root')

# dfをlong formatに変換
longdryprostrate <- melt(data=df_dry_prostrate,
                  id.vars="whole_plant",
                  value.name="weight")

# scaling lawに合わせてlog10 transformationした上で、グラフ化
plot_dryprostrate <- ggplot(longdryprostrate, aes(whole_plant, value, color=variable)) + 
                geom_point() + 
                stat_ma_line(method = "SMA", se=FALSE)
plot_dryprostrate

# scaling lawに合わせてlog10 transformationした上で、指数を求める
lm_prostrate_weight <- lmodel2(log10(dry_prostrate_mass) ~ log10(dry_whole_mass), data=R_Beech_Roots, "interval", "interval", 99)
lm_trunk_weight <- lmodel2(log10(dry_shoot_mass) ~ log10(dry_whole_mass), data=R_Beech_Roots, "interval", "interval", 99)
lm_root_weight <- lmodel2(log10(dry_root_mass) ~ log10(dry_whole_mass), data=R_Beech_Roots, "interval", "interval", 99)
lm_aboveground <- lmodel2(aboveground ~ whole_plant, data=df_dry_mass2, "interval", "interval", 99)
lm_belowground <- lmodel2(belowground ~ whole_plant, data=df_dry_mass2, "interval", "interval", 99)

lm_prostrate_weight
lm_trunk_weight
lm_root_weight
lm_aboveground
lm_belowground
