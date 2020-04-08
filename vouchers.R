
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

# PRSCHA_1(私立学校で６年生を始める比率) をみると、私立学校で６年生を始める比率が共変量を含まない場合でも6%程度上昇している
# くじにはずれていてももともと 87% の学生が私立学校でスタートしているのでもともと数が多い
# 既に入学する確率が高い状態の生徒の間で介入をランダムにアサインしているため変化が少ない

# USNGSCH(何かしらの奨学金を調査期間中に使っている割合) をみるとどちらのモデルにおいてもおよそ50%の効果が推定されているため、
# 当選グループにおいて何かしらの奨学金を調査期間中に使っている割合が、非当選グループに対して 50% 多いことを示している
# 非当選グループにおける奨学金の利用率は 5% 程度なので当選したことにより多くの生徒が割引研を使い続けていることがわかる

# 割引券には私立学校に"通わせ始める"効果は確認されなかった
using_voucher_result

# 取り出した効果を ggplot で可視化
using_voucher_result %>%
    ggplot(aes(y = estimate, x = model_index)) +
    geom_point() +
    geom_errorbar(aes(ymax = estimate + std.error * 1.96,
                      ymin = estimate - std.error * 1.96,
                      width = 0.1)) +
    geom_hline(yintercept = 0, linetype = 2) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5),
          legend.position = "bottom",
          plot.margin = margin(0.5, 1, 0.5, 1, "cm"))


# 私学や学校に行く傾向を可視化
going_private_results <- df_results %>%
    filter(term == "VOUCH0", str_detect(model_index, "PRSCH_C|INSCHL")) %>%
    select(model_index, term, estimate, std.error, p.value) %>%
    arrange(model_index)

# PRSCH_C を見ると効果量は 0.15 程度という結果になっており、
# 当選したことによって私立学校へ通い続ける生徒が 15 %程度増えていることがわかる

# INSCHL を見ると信頼区間 0 を含む状態になっているので、当選することによって
# 何かしらの学校への通学を増加するような効果は明確ではない。
# また推定結果は 0.01 程度となっているため、効果は非常に小さいものである。

# 当選したことによって学校に通えるような学生が増えることはなかったが、
# 当選したことによって私立の学校に通い続けられる学生は増えた
going_private_results

# 取り出した効果を ggplot で可視化
going_private_results %>%
    ggplot(aes(y = estimate, x = model_index)) +
    geom_point() +
    geom_errorbar(aes(ymax = estimate + std.error * 1.96,
                      ymin = estimate - std.error * 1.96,
                      width = 0.1)) +
    geom_hline(yintercept = 0, linetype = 2) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5),
          legend.position = "bottom",
          plot.margin = margin(0.5, 1, 0.5, 1, "cm"))

# 留学の傾向を可視化
recept_results <- df_results %>%
    filter(term == "VOUCH0", str_detect(model_index, "FINISH|REPT")) %>%
    select(model_index, term, estimate, std.error, p.value) %>%
    arrange(model_index)


# 割引券のしくみへ私立学校に通い続けることは、当選グループで留年する傾向も低くなる
# 6年生で留年したかを示す REPT6 に対する推定効果は -0.06 程度であり、
# 6年生における留年する確率が 6 %程度低いことを示している
# NREPT, REPT のどちらも有意な結果
# NREPT : 3年間で何回留年したか
# REPT : 調査までに一度でも留年したか
# 6,7,8年生の修了を表すFINISH6-8はどれも有意な結果となっている。
# 8年生での卒業は一度でも留年すれば不可能になるため差が大きくなっている
recept_results

# 取り出した効果を ggplot で可視化
recept_results %>%
    ggplot(aes(y = estimate, x = model_index)) +
    geom_point() +
    geom_errorbar(aes(ymax = estimate + std.error * 1.96,
                      ymin = estimate - std.error * 1.96,
                      width = 0.1)) +
    geom_hline(yintercept = 0, linetype = 2) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5),
          legend.position = "bottom",
          plot.margin = margin(0.5, 1, 0.5, 1, "cm"))

# これらの結果から割引券には私立学校への通学を維持すること、留年しないようにすること
# 割引券を受け取った生徒がより学費が高く教育の質が高い学校にいった為に
# 留年しなくなったというように見ることもできる
# これらの原因を特定するために、覚醒とと学校選択についてのデータが必要だがデータセットにないため切り分けできない

# 性別による効果差の分析
# Angrist(2002)のTable.4 & 6 bogota 1995の再現

# table 4に必要なデータの抜き出し
data_tablel4_bogata1995 <- vouchers %>%
  filter(BOG95SMP == 1, TAB3SMPL == 1,
         !is.na(SCYFNSH), !is.na(FINISH6), !is.na(PRSCHA_1),
         !is.na(REPT6), !is.na(NREPT), !is.na(INSCHL),
         !is.na(FINISH7),
         !is.na(PRSCH_C), !is.na(FINISH8), !is.na(PRSCHA_2),
         !is.na(TOTSCYRS), !is.na(REPT)
  ) %>%
  select(VOUCH0, SVY, HSVISIT, DJAMUNDI, PHONE, AGE,
         STRATA1:STRATA6, STRATAMS, DBOGOTA, D1993, D1995, D1997,
         DMONTH1:DMONTH12, SEX_MISS, FINISH6, FINISH7, FINISH8,
         REPT6, REPT, NREPT, SEX2, TOTSCYRS, MARRIED, HASCHILD,
         HOURSUM,WORKING3, INSCHL,PRSCH_C,USNGSCH,PRSCHA_1)

# 女子生徒のデータだけ取り出し
female_regression_data <- data_tablel4_bogata1995 %>%
    filter(SEX2 == 0)

### まとめて回帰分析を実行
df_models <- models %>%
  mutate(model = map(.x = formula, .f = lm, data = female_regression_data)) %>%
  mutate(lm_result = map(.x = model, .f = tidy))

### モデルの結果を整形
df_results_female <- df_models %>%
  mutate(formula = as.character(formula),
         gender = "female") %>%
  select(formula, model_index, lm_result, gender) %>%
  unnest(cols = c(lm_result))

# 男子生徒のデータだけ取り出し
male_regression_data <- data_tablel4_bogata1995 %>%
    filter(SEX2 == 1)

### まとめて回帰分析を実行
df_models <- models %>%
  mutate(model = map(.x = formula, .f = lm, data = male_regression_data)) %>%
  mutate(lm_result = map(.x = model, .f = tidy))

### モデルの結果を整形
df_results_male <- df_models %>%
  mutate(formula = as.character(formula),
         gender = "male") %>%
  select(formula, model_index, lm_result, gender) %>%
  unnest(cols = c(lm_result))

## 通学傾向への分析結果の可視化
### PRSCHA_1,USNGSCHに対する分析結果を抜き出す
using_voucher_results_gender <- rbind(df_results_male, df_results_female) %>%
  filter(term == "VOUCH0", str_detect(model_index, "PRSCHA_1|USNGSCH")) %>%
  select(gender, model_index, term, estimate, std.error, p.value) %>%
  arrange(gender, model_index) %>%
  filter(str_detect(model_index, "covariate"))

### ggplotによる可視化
# PRSCHA_1：私立学校で６年生を始める比率
# 　男子学生だと 9% の効果があったが女子生徒は 2% 程度であった
# 　2%程度だと統計的に有意な差とは言えないため私立学校へ通わせる効果はない
# 　という可能性を否定できない
# 　統計的に有意だったとしても効果量は男子生徒の 1/4 以下なため、
# 　私立通学を増加させる効果が非常に小さく性別によって効果が大きく異なっている
# 　統計的に有意な差って何? → 箱ひげが 0 を含むかどうか
# USNGSCH：何かしらの奨学金を調査期間中に使っている割合
# 　男子生徒では 44% 程度、女子生徒では 55% 程度となっており
#   女子生徒の方が当選時に奨学金を利用する傾向が強い
using_voucher_results_gender %>%
  filter(str_detect(model_index, "covariate")) %>%
  ggplot(aes(y = estimate, x = model_index)) +
  geom_point() +
  geom_errorbar(aes(ymax = estimate + std.error*1.96,
                    ymin = estimate - std.error*1.96,
                    width = 0.1)) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(0.5,1,0.5,1, "cm")) +
  facet_grid(gender ~ .)

## 留年と通学年数への分析結果の可視化
### PRSCH_C,INSCHL,REPT,TOTSCYRS,FINISHに対する分析結果を抜き出す
going_private_results_gender <- rbind(df_results_male, df_results_female) %>%
  filter(term == "VOUCH0",
         str_detect(model_index, "PRSCH_C|INSCHL|REPT|TOTSCYRS|FINISH")) %>%
  select(gender, model_index, term, estimate, std.error, p.value) %>%
  arrange(model_index)

### ggplotによる可視化
# PRSCH_C：当選したことによって私立学校へ通い続ける生徒
#  男子生徒が 14% 女子生徒が17% で私立学校に通学させる効果は
#  女子生徒のほうが効果が高い
# INSCHL：当選することによって公立学校への通学を増加
#  男女両方有意な差はない
# REPT6：6年生で留年したかを示す
#  男子生徒で 8 % 女子学生で 3% 程度の現象となっているが統計的に有意な結果にはなっていない
# NREPT : 3年間で何回留年したか
# REPT : 調査までに一度でも留年したか
# FINISH6-8：6,7,8年生の修了を表す
#  男子、女子ともに FINISH8 で 10% 増加しており、より高い学年を修了できるようになっている
#
# 総合すると女子生徒は通学を維持する効果は高いが留年に対する効果はほぼない
# 女子生徒が私立学校に通い続けられない原因が学力や留年以外の要因に起因している
# ことを示唆する。経済リソースが分配されにくく公立への転校や労働により家計を助ける
# ことを求められている可能性
# 割引券により労働時間の減少が予想される
going_private_results_gender %>%
  filter(str_detect(model_index, "covariate")) %>%
  ggplot(aes(y = estimate, x = model_index)) +
  geom_point() +
  geom_errorbar(aes(ymax = estimate + std.error*1.96,
                    ymin = estimate - std.error*1.96,
                    width = 0.1)) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(0.5,1,0.5,1, "cm")) +
  facet_grid(gender ~ .)


### HOURに対する分析結果を抜き出す
working_hour_results_gender <- rbind(df_results_male, df_results_female) %>%
  filter(term == "VOUCH0", str_detect(model_index, "HOUR")) %>%
  select(gender, model_index, term, estimate, std.error, p.value) %>%
  arrange(gender, model_index)

### ggplotによる可視化
# HOURSUM：労働時間
# 男子生徒では 0.6 時間の減少ですが有意差はない
# 一方女子生徒は 2.1 時間の減少でかつ統計的に有意な結果となっている。
#
# 割引券により労働が抑えられている、もしくは私学の勉強量の増加により労働できない
working_hour_results_gender %>%
  filter(str_detect(model_index, "covariate")) %>%
  ggplot(aes(y = estimate, x = model_index)) +
  geom_point() +
  geom_errorbar(aes(ymax = estimate + std.error*1.96,
                    ymin = estimate - std.error*1.96,
                    width = 0.1)) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(0.5,1,0.5,1, "cm")) +
  facet_grid(. ~ gender)

