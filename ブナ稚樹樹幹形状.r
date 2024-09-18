#### Install packages ####
install.packages("reshape")
install.packages("lmodel2")
install.packages("ggplot2")
install.packages("colorBlindness")
install.packages("ggpmisc") # for rma regression
install.packages("arm")
install.packages("caret") #　交差検証用
install.packages("PerformanceAnalytics")
install.packages("tidyr")
install.packages("scales")

library(lmodel2)
library(reshape)
library(ggpmisc)
library(arm)
library(ggplot2)
library(colorBlindness)
library(tidyr)

#計測項目######
#1 log10_地上部質量 log10_dry_shoot_mass
#2 log10_埋幹部質量 log10_dry_prostrate_mass
#3 log10_主根質量 log10_dry_root_mass
#4 log10_全体質量 log10_dry_whole_mass
#5 log10_地上部表面積 log10_top_surfacearea
#6 log10_地上部表面積 log10_bottom_surfacearea

#### ggplot2を用いて作図 #####
### set blind friendly color palette ###
cbPalette <- c("#009E73", "#0072B2", "#E69F00")

R_Beech_Roots <- R_Beech_Roots[1:15, ] # subset for Karayama site

### surface area ####
df_surfacearea <- R_Beech_Roots[c("log10_dry_whole_mass",
                               "log10_top_surfacearea",
                               "log10_bottom_surfacearea")]

# fix column names for visualization
colnames(df_surfacearea) <- c('whole_plant','Aboveground (Shoot)','Belowground (Prostrate stem + Main root)')

longsurfacearea <- pivot_longer(data = df_surfacearea,
                                cols = c('Aboveground (Shoot)','Belowground (Prostrate stem + Main root)'),
                                names_to = "variable", 
                                values_to = "value")

plot_surfacearea <- ggplot(longsurfacearea, aes(whole_plant, value, color=variable, shape=variable,linetype=variable)) + 
                    geom_point(colour = "black", size = 3.5) +
                    geom_point(aes(colour = variable),size=3)+
                    ylim(-5,-1.8)+
                    stat_ma_line(method = "SMA", se=FALSE) +
                    scale_colour_manual(values=cbPalette) +
  scale_x_continuous(labels = label_number()) +
                    theme_bw(base_size = 20) +
                    labs(title="a",x = "Log Whole-plant Dry Mass (kg)", y = bquote("Log Surface Area "(m^2))) +
                    theme(plot.background = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.grid.major = element_blank(),
                          panel.border = element_rect(size=1.5),  
                          legend.title = element_blank(),
                          aspect.ratio=1,
                          legend.position = c(.36, .9),
                          legend.text = element_text(size = 10))
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

# scaling lawに合わせてlog10 transformationした上で、指数を求める
lm_prostrate_weight <- lmodel2(log10(dry_prostrate_mass) ~ log10(dry_whole_mass), data=R_Beech_Roots, "interval", "interval", 99)
lm_trunk_weight <- lmodel2(log10(dry_shoot_mass) ~ log10(dry_whole_mass), data=R_Beech_Roots, "interval", "interval", 99)
lm_root_weight <- lmodel2(log10(dry_root_mass) ~ log10(dry_whole_mass), data=R_Beech_Roots, "interval", "interval", 99)
lm_aboveground <- lmodel2(log10_dry_shoot_mass ~ log10_dry_whole_mass, data=df_dry_mass2, "interval", "interval", 99)
lm_belowground <- lmodel2(log10_belowground_mass ~ log10_dry_whole_mass, data=df_dry_mass2, "interval", "interval", 99)

lm_prostrate_weight
lm_trunk_weight
lm_root_weight
lm_aboveground
lm_belowground


# fix column names for visualization
colnames(df_dry_mass3) <- c('whole_plant','Shoot','Prostrate stem','Main root')
colnames(df_dry_mass2) <- c('whole_plant','Aboveground (Shoot)','Belowground (Prostrate stem + Main root)')

longdry2 <- pivot_longer(data = df_dry_mass2,
                                cols = c('Aboveground (Shoot)','Belowground (Prostrate stem + Main root)'),
                                names_to = "variable", 
                                values_to = "value")

longdry3 <- pivot_longer(data = df_dry_mass3,
                         cols = c('Shoot','Prostrate stem','Main root'),
                         names_to = "variable", 
                         values_to = "value")
plot_surfacearea <- ggplot(longsurfacearea, aes(whole_plant, value, color=variable, shape=variable,linetype=variable)) + 
  geom_point(colour = "black", size = 3.5) +
  geom_point(aes(colour = variable),size=3)+
  ylim(-5,-1.8)+
  stat_ma_line(method = "SMA", se=FALSE) +
  scale_colour_manual(values=cbPalette) +
  scale_x_continuous(labels = label_number(accuracy=1),
                     breaks = c(-4,-3,-2),
                     minor_breaks = NULL,
                     expand = c(0.05, 0)) +
  theme_bw(base_size = 20) +
  labs(title="a",x = "Log Whole-plant Dry Mass (kg)", y = bquote("Log Surface Area "(m^2))) +
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(size=1.5),  
        legend.title = element_blank(),
        aspect.ratio=1,
        legend.position = c(.39, .9),
        legend.text = element_text(size = 11))
plot_surfacearea
plot_dry_mass2 <- ggplot(longdry2, aes(whole_plant, value, color=variable, shape=variable,linetype=variable)) + 
                  geom_point(colour = "black", size = 3.5) +
                  geom_point(aes(colour = variable),size=3)+
                  ylim(-5,-1.8)+
                  stat_ma_line(method = "SMA", se=FALSE) +
                  scale_colour_manual(values=cbPalette) +
                  scale_x_continuous(labels = label_number(accuracy=1),
                                     breaks = c(-4,-3,-2),
                                     minor_breaks = NULL,
                                     expand = c(0.05, 0)) +
                  theme_bw(base_size = 20) +
                  labs(title="b",x = "Log Whole-plant Dry mass (kg)", y = "Log Dry mass (kg)") + 
                  scale_colour_manual(values=cbPalette) +
                  theme(plot.background = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.border = element_rect(size=1.5),  
                        legend.title = element_blank(),
                        aspect.ratio=1,
                        legend.position = c(.39, .9),
                        legend.text = element_text(size = 11))
plot_dry_mass2

plot_dry_mass3 <- ggplot(longdry3, aes(whole_plant, value, color=variable, shape=variable,linetype=variable)) + 
                  geom_point(color = "black", size = 3.5) +
                  geom_point(aes(color = variable),size=3)+
                  ylim(-5,-1.8)+
                  stat_ma_line(method = "SMA", se=FALSE) +
                  scale_colour_manual(values=cbPalette) +
                  scale_x_continuous(labels = label_number(accuracy=1),
                                     breaks = c(-4,-3,-2),
                                     minor_breaks = NULL,
                                     expand = c(0.05, 0)) +
                  theme_bw(base_size = 20) +
                  labs(title="c",x = "Log Whole-plant Dry mass (kg)", y = "Log Dry mass (kg)") + 
                  scale_colour_manual(values=cbPalette) +
                  scale_fill_discrete(breaks=c('Shoot', 'Prostrate stem', 'Main root')) +
                  theme(plot.background = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.border = element_rect(size=1.5),  
                        legend.title = element_blank(),
                        aspect.ratio=1,
                        legend.position = c(.2, .87),
                        legend.text = element_text(size = 11))

plot_dry_mass3

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
                stat_ma_line(method = "SMA", se=FALSE) + 
                theme_linedraw() + 
                scale_colour_manual(values=cbPalette)+
                theme_linedraw()

plot_dryprostrate

