# Research_Beech

Tracking code changes for *Allometry of young Fagus crenata: prostrate stem reinforces belowground development. Journal of Forest Research, In review.*
This repository includes the dataset of 25 samples and the R code for regression models.
- R_Beech_Roots.csv: cleaned data
- R_Beech_Roots.xlsx: meta file with unused data 
- ブナ稚樹樹幹形状.r: R code

"注：元データは「ブナ稚樹採取個体　生データ」（messenger：6/2/22, 12:26 PM付）、このデータを整形したものをR上の解析に使用した。

整形の方法：
①規約と他の論文に倣って生データをSI単位（mm -> m, g -> kg）に変換 
②アロメトリー式に当てはめるために、log10で対数変換(e.g, dry_shoot_mass --> log10_dry_shoot_mass)
③根の長さのデータは三平方の定理（水平、垂直の長さの各2乗の和の平方）を使って推定
④地上部、地下部の表面積は（地際直径/2 x pi x 高さ）で推定"								
								
								
								
								
								
								
