# 必要なパッケージをインストール
targetPackages <- c('cobalt', 'WeightIt', 'MatchIt', 'Matching')
newPackages <- targetPackages[!(targetPackages %in% installed.packages()[,"Package"])]
if(length(newPackages)) install.packages(newPackages)
for(package in targetPackages) library(package, character.only = T)

# (2) tidyverseの読み出し
library("tidyverse")
library("broom")

# (3) データの読み込み
email_data <- read_csv("http://www.minethatdata.com/Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv")

# (4) 女性向けメールが配信されたデータを削除したデータを作成
male_df <- email_data %>%
  filter(segment != "Womens E-Mail") %>% # 女性向けメールが配信されたデータを削除
  mutate(treatment = ifelse(segment == "Mens E-Mail", 1, 0)) # 介入を表すtreatment変数を追加

# (5) セレクションバイアスのあるデータを作成
## seedを固定する
set.seed(1)

## 条件に反応するサンプルの量を半分にする
obs_rate_c <- 0.5
obs_rate_t <- 0.5

## バイアスのあるデータを作成
biased_data <- male_df %>%
  mutate(obs_rate_c = ifelse( (history > 300) | (recency < 6) | (channel == "Multichannel"), obs_rate_c, 1),
         obs_rate_t = ifelse( (history > 300) | (recency < 6) | (channel == "Multichannel"), 1, obs_rate_t),
         random_number = runif(n = NROW(male_df))) %>%
  filter( (treatment == 0 & random_number < obs_rate_c ) |
            (treatment == 1 & random_number < obs_rate_t) )

# (6) 傾向スコアの推定
ps_model <- glm(data = biased_data,
                formula = treatment ~ recency + history + channel,
                family = binomial)


# (7) 傾向スコアマッチング
## ライブラリの読み込み
library("MatchIt")

## 傾向スコアを利用したマッチング
m_near <- matchit(formula = treatment ~ recency + history + channel,
                  data = biased_data,
                  method = "nearest",
                  replace = TRUE)


## マッチング後のデータを作成
matched_data <- match.data(m_near)

## マッチング後のデータで効果の推定
PSM_result <- lm(data = matched_data,
                 formula = spend ~ treatment) %>%
  tidy()

PSM_result

# (8) 逆確率重み付き推定（IPW）
## ライブラリの読み込み
library("WeightIt")

## 重みの推定
weighting <- weightit(treatment ~ recency + history + channel,
              data = biased_data,
              method = "ps",
              estimand = "ATE")

## 重み付きデータでの効果の推定
IPW_result <- lm(data = biased_data,
                 formula = spend ~ treatment,
                 weights = weighting$weights) %>%
  tidy()

IPW_result