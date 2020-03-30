
# 必要なパッケージをインストル
targetPackages <- c('remotes', 'tidyverse')
newPackages <- targetPackages[!(targetPackages %in% installed.packages()[,"Package"])]
if(length(newPackages)) install.packages(newPackages)
for(package in targetPackages) library(package, character.only = T)

# ライブラリとデータの読み込み
remotes::install_github("itamarcaspi/experimentdatar")
library(experimentdatar)
library(broom)
library(tidyverse)
data(vouchers)
vouchers

# 回帰式
# 前処理
formula_x_base <- "VOUCH0"

# SVY 電話による調査の有無
# HSVISIT 対面による調査の有無
# DMONTH1-12 調査が何月に行われたか？
# AGE 調査時の学生の年齢
# SEX2 学生の性別
# STRATA1-6,MS 親の社会的地位の分類
formula_x_covariate <- "SVY + HSVISIT + AGE + STRATA1 + STRATA2 + STARTA3 + STRATA4 + STRATA5 + STARTA6 + STARTAMS + D1993 + D1995 + D1997 + DMONTH1 + DMONTH2 + DMONTH3 + DMONTH4 + DMONTH5 + DMONTH6 + DMONTH7 + DMONTH8 + DMONTH9 + DMONTH10 + DMONTH11 + DMONTH12 + SEX2"
formula_y <- c("TOTSCYR", "INSCHL", "PRSCH_C", "USNGSCH", "PRSCHA_1", "FINISH6", "FINISH7", "FINISH8", "REPT6", "REPT", "NREPT", "MARRIED", "HASCHILD", "HOURSUM", "WORKING3")

# formula_y の各要素に対して共変量を含まない回帰式の作成
base_reg_formula <- paste(formula_y, "~", formula_x_base)
names(base_reg_formula) <- paste(formula_y, "base", sep="_")

# formula_y の各要素に対して共変量を含む回帰式の作成
covariate_reg_formula <- paste(formula_y, "~", formula_x_base, "+", formula_x_covariate)
names(covariate_reg_formula) <- paste(formula_y, "covariate", sep = "_")