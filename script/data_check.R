library(tidyverse)

# 一時的に読み取ったデータを確認する
df <- read_csv(file = 'data/total_result_df.csv')

# 警告は合作4人以上のものに対して出てくる。特に今回は問題無し
problems(df)
df[3571,]

# track_nameにremixとはいっているものを抽出する
remix_tracks <- df %>% 
  filter(grepl(pattern = "remix", x=tolower(track_name)))

# Remixアーティストを確認
remix_tracks <- remix_tracks %>% 
  mutate(remix_artist = str_split(track_name, pattern = "- ", n = 2, simplify = TRUE)[,2])
View(remix_tracks)

# データ確認 ----

## 1. 長さが短い版とそうでないものが混在している ----
### duration_msが短いものを採用する。
### Edit(短)だったり、DJ Length(長)だったりの表記があるのでduration_msで判断する
remix_tracks %>% 
  filter(grepl("Your Poison", track_name))

## 2. 同じ曲が違うアルバムに収録されていて重複が存在する ----
remix_tracks %>% 
  filter(grepl("Quiet", track_name))

### artistが本来とは異なる場合があるので同じtrack_nameの場合はPopulationが一番高いものを正とする
remix_tracks %>% 
  filter(grepl("A-Bomb", track_name))

## 3. Remixの表記は()で囲まれているものと" - "で区切るものがある ----
remix_tracks %>% 
  filter(grepl(" - ", track_name))

remix_tracks %>% 
  filter(grepl("remix)", tolower(track_name)))

# Remix データ前処理 ----

## (xxxxx Remix) を抜き出す ----
remix_tracks <- remix_tracks %>% 
  mutate(remix_artist_1 = str_extract(track_name, pattern = "\\(.*?\\)")) %>% 
  mutate(remix_artist_1 = str_replace(remix_artist_1, fixed(")"), "")) %>% 
  mutate(remix_artist_1 = str_replace(remix_artist_1, fixed("("), ""))

## remix_artist, remix_artist_1を結合する(remix_artistがNAの場合remix_artist_1を用いる) ----
remix_tracks <- remix_tracks %>% 
  mutate(remix_artist = if_else(
    is.na(remix_artist), remix_artist_1, remix_artist)
  )

## 今までの条件でremix_artistにRemixの文字がないデータはremix_artist_1を用いる ----
### 以下のようなデータが該当
remix_tracks %>% 
  filter(track_id == "3mhQhNDw3HRODWYmQ72KIF")

remix_tracks <- remix_tracks %>% 
  mutate(remix_artist = if_else(
    str_detect(tolower(remix_artist), "remix"),
    remix_artist,
    remix_artist_1
  ))

## 何故かremix_artistにデータが無い行を削除 ----
### 以下のようなデータが該当 これはまとめて抜きづらい・・・
remix_tracks %>% 
  filter(track_id == "0XyxzZxTNY7yoo0fDm2sQs")

### これに該当するのはわずか0.6%なので問題はないと判断
nrow(remix_tracks %>% 
  filter(
    str_detect(tolower(remix_artist), "remix")
  )
) / nrow(remix_tracks)

remix_tracks <- remix_tracks %>% 
  filter(str_detect(tolower(remix_artist), "remix"))


## non Remixデータ前処理 ####
nonremix_tracks <- df %>% 
  filter(!str_detect(tolower(track_name), "remix"))

### Remixではない方は、普段のpopulation値を測りたいので合作していないものだけに絞る(一応絞る場合と絞らない場合みたいから両方保持しておくか)
### Remixの方は、Remix対象が合作曲でも問題無し。普段合作しない時とその人が何かしら合作したときの差を見たいため。

nonremix_tracks_single <- nonremix_tracks %>% 
  filter(is.na(`2`))


### 曲名被りが多いので、GROUPBYでいちばん高いPopulationのものを選択する ####
### EditやCut Mixとの被りが発生している楽曲も、population値が大きいものを採択する
### ほとんどは" - "がはいっているものとはいっていないもので別れているので、" - "で楽曲文字列を分けて、再度GROUP BYする
nonremix_tracks_single <- nonremix_tracks_single %>% 
  mutate(track_name = str_split(track_name, pattern = " - ", n = 2, simplify = TRUE)[,1])


# 楽曲名&アーティスト名毎に最大のpopularityを抽出
nonremix_tracks_single_maxpopularity <- nonremix_tracks_single %>% 
  group_by(tolower(track_name), `1`) %>% 
  summarise(popularity = max(popularity)) %>% 
  ungroup()
nonremix_tracks_single_maxpopularity <- nonremix_tracks_single_maxpopularity %>% rename("track_name" = `tolower(track_name)`)


nonremix_tracks_single_1 <- inner_join(
  nonremix_tracks_single %>% mutate(track_name = tolower(track_name)),
  nonremix_tracks_single_maxpopularity %>% mutate(track_name = tolower(track_name)),
  by = c("track_name", "1", "popularity")
)

nonremix_track_single_2 <- nonremix_tracks_single_1 %>% distinct(track_name, `1`, popularity,.keep_all = TRUE)


### アーティストごとに平均popularityを算出する
artist_avg_popularity <- nonremix_track_single_2 %>% 
  group_by(`1`) %>% 
  summarise(avg_popularity = mean(popularity))

rm(nonremix_tracks_single, nonremix_tracks_single_1, nonremix_tracks_single_maxpopularity)
## Remixデータ前処理 ####
remix_tracks %>% select(remix_artist)


### remix_artistカラムから誰がRemixしたのかを取得したいので、" remix"でsplitしてアーティスト名を抜き出し、nonremix_track_single_2とマージする
### ２名以上のRemixやアーティスト名+コンセプト みたいな形で書かれているものは若干数抜け落ちる
remix_tracks_1 <- remix_tracks %>% 
  mutate(remix_artist = str_split(tolower(remix_artist), pattern = " remix", n = 2, simplify = TRUE)[,1])

remix_tracks_maxpopularity <- remix_tracks_1 %>% 
  group_by(tolower(track_name), remix_artist) %>% 
  summarise(popularity = max(popularity)) %>% 
  ungroup()

remix_tracks_1 <- inner_join(
  remix_tracks_1 %>% mutate(track_name = tolower(track_name)),
  remix_tracks_maxpopularity %>% rename(track_name = `tolower(track_name)`),
  by = c("track_name", "remix_artist", "popularity")
)

artist_avg_popularity <- artist_avg_popularity %>% rename(artist = `1`)

remix_artist_comparison_df <- inner_join(
  remix_tracks_1,
  artist_avg_popularity %>% mutate(artist = tolower(artist)),
  by = c("remix_artist" = "artist")
) %>% 
  select(remix_artist,avg_popularity,track_name, popularity, release_date, `1`) %>% 
  rename(main_artist = `1`)

### アーティストの平均popularityとRemix時のpopularityを結合したもの
remix_artist_comparison_df


### 統計量を取らずにRemixとそうでないものでフラグを立てるDataFrameを作成する ####
target_remix_tracks <- remix_artist_comparison_df %>% select(remix_artist, track_name, popularity, release_date) %>% rename(artist = remix_artist)
target_remix_tracks

target_nonremix_tracks <- nonremix_track_single_2 %>% 
  rename(artist = `1`) %>% 
  filter(tolower(artist) %in% c(target_remix_tracks$artist)) %>% 
  select(artist, track_name, popularity, release_date) %>% 
  mutate(artist = tolower(artist))

target_tracks <- rbind(
  target_nonremix_tracks %>% mutate(remix_flag = 0),
  target_remix_tracks %>% mutate(remix_flag = 1)
)

target_tracks %>% 
  group_by(remix_flag) %>% 
  summarize(n = n())


remix_tracks_1 %>% 
  filter(track_name == "lyra - lroken minds remix")

target_tracks %>% 
  group_by(remix_flag) %>% 
  summarise(n = n())

## データ書き出し

write_csv(remix_artist_comparison_df, file="data/remix_artist_comparison_df.csv")
write_csv(target_tracks, file="data/target_tracks.csv")
write_csv(artist_avg_popularity, file="data/artist_avg_popularity.csv")
