
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
formula_x_covariate <- "SVY + HSVISIT + AGE + STRATA1 + STRATA2 + STRATA3 + STRATA4 + STRATA5 + STRATA6 + STRATAMS + D1993 + D1995 + D1997 + DMONTH1 + DMONTH2 + DMONTH3 + DMONTH4 + DMONTH5 + DMONTH6 + DMONTH7 + DMONTH8 + DMONTH9 + DMONTH10 + DMONTH11 + DMONTH12 + SEX2"
formula_y <- c("TOTSCYRS", "INSCHL", "PRSCH_C", "USNGSCH", "PRSCHA_1", "FINISH6", "FINISH7", "FINISH8", "REPT6", "REPT", "NREPT", "MARRIED", "HASCHILD", "HOURSUM", "WORKING3")

# formula_y の各要素に対して共変量を含まない回帰式の作成
base_reg_formula <- paste(formula_y, "~", formula_x_base)
names(base_reg_formula) <- paste(formula_y, "base", sep="_")

# formula_y の各要素に対して共変量を含む回帰式の作成
covariate_reg_formula <- paste(formula_y, "~", formula_x_base, "+", formula_x_covariate)
names(covariate_reg_formula) <- paste(formula_y, "covariate", sep = "_")

# モデル式のベクトルを作成
table3_formula <- c(base_reg_formula, covariate_reg_formula)

# モデル式のベクトルをデータフレーム化する
models <- table3_formula %>%
    enframe(name = "model_index", value = "formula")

# 回帰分析を実行
# bogota 1995のデータを抽出
regression_data <- vouchers %>%
    filter(TAB3SMPL == 1, BOG95SMP == 1)

# まとめて回帰分析を実行
df_models <- models %>%
    mutate(model = map(.x = formula, .f = lm, data = regression_data)) %>%
    mutate(lm_result = map(.x = model, .f = tidy))

# モデルの結果を整形
df_results <- df_models %>%
    mutate(formula = as.character(formula)) %>%
    select(formula, model_index, lm_result) %>%
    unnest(cols = c(lm_result))

df_results

# 当選したグループで割り引券がちゃんと使われたのか確認する
# 通学率と奨学金の利用
using_voucher_result <- df_results %>%
    filter(term == "VOUCH0", str_detect(model_index, "PRSCHA_1|USNGSCH")) %>%
    select(model_index, term, estimate, std.error, p.value) %>%
    arrange(model_index)

# PRSCHA_1 をみると、私立学校で６年生を始める比率が共変量を含まない場合でも6%程度上昇している
# くじにはずれていてももともと 87% の学生が私立学校でスタートしているのでもともと数が多い
# 既に入学する確率が高い状態の生徒の間で介入をランダムにアサインしているため変化が少ない

# USNGSCH をみるとどちらのモデルにおいてもおよそ50%の効果が推定されているため、
# 当選グループにおいて何かしらの奨学金を調査期間中に使っている割合が、非当選グループに対して 50% 多いことを示している
# 非当選グループにおける奨学金の利用率は 5% 程度なので当選したことにより多くの生徒が割引研を使い続けていることがわかる

# 割引券には私立学校に"通わせ始める"効果は確認されなかった
using_voucher_result
