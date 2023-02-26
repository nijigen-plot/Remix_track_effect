# https://dev.classmethod.jp/articles/about-using-of-spotify-api/ Web APIを用いた情報取得のやり方
# R spotify API ライブラリ https://github.com/charlie86/spotifyr

# Spotify API の認証 ----
# install.packages("spotifyr")
library(spotifyr)
# 環境変数は作業ディレクトリに.Renvironファイルを作成し設定する
SPOTIFY_CLIENT_ID <- Sys.getenv("SPOTIFY_CLIENT_ID")
SPOTIFY_CLIENT_SECRET <- Sys.getenv("SPOTIFY_CLIENT_SECRET")

access_token <- get_spotify_access_token(
  client_id = SPOTIFY_CLIENT_ID,
  client_secret = SPOTIFY_CLIENT_SECRET
)

# Get Data ----
library(tidyverse)
library(readr)
## Dataを取得したいアーティストのID tibbleを作成 ----
artists_df <- read_csv(file ='data/Spotify_Artist_list.csv')
# artist_linkからartist_idを抜き出す
artists_df1 <- artists_df %>% 
  mutate(artist_id = str_split(artist_link, pattern = "artist/", n = 2, simplify = TRUE)[,2]) %>% 
  mutate(artist_id = str_split(artist_id, pattern = fixed("?"), n = 2, simplify = TRUE)[,1])

# トラックの複数から成るアーティストデータを横にしたTibbleを作る
create_artist_tibble = function(x) {
  as_tibble(t(x[3]))
}

# 1アーティストIDからそのアーティストの楽曲データを取得する
getTrackData <- function( artist_id, access_token ) {
  if ((class(artist_id) == "character") == FALSE | (class(access_token) == "character") == FALSE)   {
    stop("文字列データを入力してください")
  }
  
  artist_releases <- get_artist_albums(
    id = artist_id,
    # appears onは取得しない。他人のシングル,アルバムの参加であれば他人のアーティスト楽曲データのalbum,singleから取得できる。V.Aは取得しないことにする
    include_groups = c("album", "single"),
    limit = 50,
    authorization = access_token
  )
  
  df <- tibble()
  for (album_id in artist_releases$id) {
    album_tracks <- get_album_tracks(
      id = album_id,
      # album,singleで50曲を超えるのはよっぽどないので問題無し
      limit = 50,
      authorization = access_token
    )
    track_data <- get_tracks(
      ids = album_tracks$id,
      authorization = access_token
    )
    track_info_df <- tibble(
      track_id = track_data$id,
      track_name = track_data$name, # 楽曲名
      duration_ms = track_data$duration_ms, # 楽曲の長さ(Intro等短いものを削除するのに使用)
      popularity = track_data$popularity, # 人気度
      release_date = track_data$album.release_date, # リリース日
      release_date_precision = track_data$album.release_date_precision, # リリース日時単位
      isrc = track_data$external_ids.isrc # ISRCコード
    )
    track_artist_info_df <- purrr::map(album_tracks$artists, create_artist_tibble) %>% 
      bind_rows()
    temporary_df <- bind_cols(track_info_df, track_artist_info_df)
    df <- bind_rows(df, temporary_df)
    Sys.sleep(1)
  }
  return (df)
}

# 全てのアーティストIDから楽曲データを取得する
pacman::p_load(progress)
pb <- progress_bar$new(total = nrow(artists_df1))
# 空のTibbleを用意
total_result_df <- tibble()
for (aid in artists_df1$artist_id) {
  pb$tick()
  print(aid)
  result <- getTrackData(aid, access_token)
  total_result_df <- bind_rows(total_result_df, result)
  Sys.sleep(1)
}


# 途中でとまったのでそこから再開
which(artists_df1$artist_id == "3mHNSwsSiKmmKN2nMudyNq")
artists_df2 <- artists_df1[64:nrow(artists_df1),]

for (aid in artists_df2$artist_id) {
  pb$tick()
  print(aid)
  result <- getTrackData(aid, access_token)
  total_result_df <- bind_rows(total_result_df, result)
  Sys.sleep(1)
}

# 書き出し
write_csv(
  total_result_df,
  file = 'data/total_result_df.csv'
)
