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