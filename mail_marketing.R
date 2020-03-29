# 必要なパッケージをインストル
targetPackages <- c('tidyverse')
newPackages <- targetPackages[!(targetPackages %in% installed.packages()[,"Package"])]
if(length(newPackages)) install.packages(newPackages)
for(package in targetPackages) library(package, character.only = T)

email_data <- read_csv("http://www.minethatdata.com/Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv")

# 女性向けメールデータが送られた人をfilterによって除去
male_df <- email_data %>%
filter(segment != "Womens E-Mail") %>%

# 男性向けメールが配信されたかどうか判定
mutate(treatment = if_else(segment == "Mens E-Mail", 1, 0))

# TODO: printだと列が省略されてしまうのでデータの全貌が掴めない。Rで全ての列を表示する方法を調べる
male_df

# group_by と summariseによって集計による比較
summary_by_segment <- male_df %>%
    # データのグループ化: ２分割
    group_by(treatment) %>%
    summarise(
            # 各グループごとの売り上げが発生する確率
            conversion_rate = mean(conversion),
            # グループごとの spend の平均
            spend_mean = mean(spend),
            # グループごとのデータ数
            count = n()
        )

summary_by_segment

# 男性向けメールが配信されたグループの購買データを取得
mens_mail_spend <- male_df %>%
    filter(treatment==1) %>%
    pull(spend)

# メールが配信されなかったグループの購買データを取得
no_mail_spend <- male_df %>%
    filter(treatment==0) %>%
    pull(spend)

# ２つのグループ平均の差に対して有意差検定を実行
# var.equal = TRUEにより２つのグループが同じ分散を持つことを仮定する
rct_test <- t.test(mens_mail_spend, no_mail_spend, var.equal = TRUE)

# t = 5.3001, df = 42611, p-value = 1.163e-07
# p 値が非常に小さく統計的に有意なものであることを示唆
# メールの配信によってのみ起こるものであると確認できた
rct_test

### バイアスのあるデータを作成し、セレクションバイアスの効果を確認する ###
# 再現性のため乱数シード固定
set.seed(1)

obs_rate_c <- 0.5
obs_rate_t <- 0.5

# 潜在的に購入意欲が高いと考えられるユーザに対してのみメールを多く配信した場合を考える
biased_data <- male_df %>%
    # 各グループでは、
    # 昨年の購入額(historyが 300　より大きく、最後の購入月?(単位不明)が 6 よりも小さく、接触チャネルが複数ある
    # のいずれかに当てはまるものを選んで削除、
    # のいずれかに当てはまらないものを選んで削除
    # している
    mutate(obs_rate_c = if_else(
            (history > 300) | (recency < 6) | (channel == "Multichannel"), obs_rate_c, 1
        ),
        obs_rate_t = if_else(
            (history > 300) | (recency < 6) | (channel == "Multichannel"), 1, obs_rate_t
        ),
        random_number = runif(
            n = NROW(male_df))) %>%
                filter(
                    (treatment == 0 & random_number < obs_rate_c) |
                    (treatment == 1 & random_number < obs_rate_t)
                )


# group_by と summariseによって集計による比較
biased_summary_by_segment <- biased_data %>%
    # データのグループ化: ２分割
    group_by(treatment) %>%
    summarise(
            # 各グループごとの売り上げが発生する確率
            conversion_rate = mean(conversion),
            # グループごとの spend の平均
            spend_mean = mean(spend),
            # グループごとのデータ数
            count = n()
        )

# biasなしに比べて、 conversion_rate, spend_mean の差が広がっていることが確認できる
biased_summary_by_segment

# 男性向けメールが配信されたグループの購買データを取得
biased_mens_mail_spend <- biased_data %>%
    filter(treatment==1) %>%
    pull(spend)

# メールが配信されなかったグループの購買データを取得
biased_no_mail_spend <- biased_data %>%
    filter(treatment==0) %>%
    pull(spend)

biased_rct_test <- t.test(biased_mens_mail_spend, biased_no_mail_spend, var.equal = TRUE)

# p 値はより小さくなり、統計的に有意な差があることが示すが分析自体が正しいものを示す結果ではない
biased_rct_test

# biasのあるデータでの回帰分析をおこなう
# 回帰式は以下
# Spend_i = Beta_0 + Beta_treatment * treatment_i + Beta_history * history_i
biased_regression <- lm(data = biased_data, formula = spend ~ treatment + history)

# 分析結果のレポート
# treatmentの t 検定における p 値は 2.25e-07 と非常に小さな値になっているので
# 帰無仮説(メール送信の効果はない)を棄却できる.
# メールを送信することで売り上げが平均 0.9 上昇すると解釈できる
summary(biased_regression)

# 回帰分析のほとんどは Coefficients 以外の情報を気にしないため、
# broom ライブラリを使って出力を Coefficients に限定する
library("broom")

biased_regression_coefficient <- tidy(biased_regression)

biased_regression_coefficient

# RCTデータでの単回帰
rct_regression <- lm(data = male_df, formula = spend ~ treatment)

# RCTでの Beta_treatment の値は 0.770 程度　一方でバイアスのあるデータでは 0.9 となっており、
# セレクションバイアスにより効果が過剰に推定されていることがわかる
rct_regression_coefficient <- summary(rct_regression) %>% tidy()

# 共変量 X をモデルに加え重回帰分析を行う
nonrct_multiple_regression <- lm(data = biased_data, formula = spend ~ treatment + recency + channel + history)
nonrct_multiple_regression_coefficient <- tidy(nonrct_multiple_regression)

# Beta_treatment は 0.847 となりRCTデータにおける結果に近づいた
nonrct_multiple_regression_coefficient

# OVBの確認
formula_vector <- c(spend ~ treatment + recency + channel, # historyを脱落させたモデル A
                    spend ~ treatment + recency + channel + history, # もともとのモデル
                    history ~ treatment + channel + recency # historyに関する相関
)

# 名前をつける
names(formula_vector) <- paste("reg", LETTERS[1:3], sep="_")
names(formula_vector)

# モデル式をそれぞれデータフレーム変換
models <- formula_vector %>%
    enframe(name="model_index", value="formula")

# まとめて回帰分析を実施
df_models <- models %>%
    # .x で受け取ったデータの各要素に .f に関数を実施
    mutate(model = map(.x = formula, .f = lm, data = biased_data)) %>%
    mutate(lm_result = map(.x = model, .f = tidy))

# モデルの結果を整形
df_results <- df_models %>%
    mutate(formula = as.character(formula)) %>%
    # 3つの列を指定
    select(formula, model_index, lm_result) %>%
    # 指定した列のデータフレームを展開してdf_resultに保存
    unnest(cols = c(lm_result))

# 確認したいのは Beta_1 - Alpha_1 と Gamma_1 * Beta_4 の値
# 各モデルで treatment のパラメータを抜き出す
treatment_coefficient <- df_results %>%
    filter(term == "treatment") %>%
    # カラム名を指定して抜き出し
    pull(estimate)

# モデル B から history のパラメータを抜き出す
history_coefficient <- df_results %>%
    filter(model_index == "reg_B",
            term == "history") %>%
    pull(estimate)

# OVB の確認
# Beta_2 * Gamma_1
OVB <- history_coefficient * treatment_coefficient[3]
# Alpha_1 - Beta_1
coefficient_gap <- treatment_coefficient[1] - treatment_coefficient[2]

# 共変量を追加したモデルとしなかったモデルにおいて推定される効果の差が OVB の式の結果と一致することがわかった
print(OVB)
print(":")
print(coefficient_gap)

# 入れてはいけない変数を入れてみる
# visit と介入との相関
correlation_visit_treatment <- lm(
    data = biased_data,
    formula = treatment ~ visit + channel + recency + history) %>% tidy()

# 相関 0.144 という有意な結果が得られる
correlation_visit_treatment

# visitを入れた回帰分析を実施
bad_control_regression <- lm(
    data=biased_data,
    formula = spend ~ treatment + channel + recency + history + visit
)%>%tidy()

# もともと購買傾向の弱いユーザにメールを配信して来訪を促し購買を促進する
# メール配信されたグループは
#   メール配信がされなくともサイトへ来訪するような購買傾向の強いユーザと
#   メールがあるからサイトへ来訪するような購買傾向の弱いユーザ
# メールが配信されなかったグループはもともとの購買傾向が強いユーザのみがサイトへ来訪している
# 介入後にサイトに来訪したユーザ間で比較を行うとメール配信がされなかったグループの方が平均売上が低い
bad_control_regression