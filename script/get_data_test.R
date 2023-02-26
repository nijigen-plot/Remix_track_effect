library(getPass)
library(spotifyr)
library(tidyverse)
library(readr)
# 環境変数は作業ディレクトリに.Renvironファイルを作成し設定する
SPOTIFY_CLIENT_ID <- Sys.getenv("SPOTIFY_CLIENT_ID")
SPOTIFY_CLIENT_SECRET <- Sys.getenv("SPOTIFY_CLIENT_SECRET")

access_token <- get_spotify_access_token(
  client_id = SPOTIFY_CLIENT_ID,
  client_secret = SPOTIFY_CLIENT_SECRET
)

album_tracks <- get_album_tracks(
  id = "1PJnJLthf0Sed1XJcj4mlr",
  # album,singleで50曲を超えるのはよっぽどないので問題無し
  limit = 50,
  authorization = access_token
)
album_tracks

# 二重リストになっている
album_tracks$artists

# こうするとアルバムトラック１つ目のアーティストデータを横にしたTibbleが出来上がる
as_tibble(t(album_tracks$artists[[1]][3]))

# それぞれのトラックについてTibbleを作って結合したい
artist_map = function(x) {
  as_tibble(t(x[3]))
}

# 最後にパイプ演算子でbind_rowsを渡してあげれば全部くっつく
test <- purrr::map(album_tracks$artists, artist_map) %>% 
  bind_rows()

temporary_df <- tibble(
  track_id = track_data$id,
  track_name = track_data$name, # 楽曲名
  duration_ms = track_data$duration_ms, # 楽曲の長さ(Intro等短いものを削除するのに使用)
  popularity = track_data$popularity, # 人気度
  release_date = track_data$album.release_date, # リリース日
  release_date_precision = track_data$album.release_date_precision, # リリース日時単位
  isrc = track_data$external_ids.isrc # ISRCコード
)

track_data <- get_tracks(
  ids = album_tracks$id,
  authorization = access_token
)

temporary_df <- tibble(
  track_id = track_data$id,
  track_name = track_data$name, # 楽曲名
  duration_ms = track_data$duration_ms, # 楽曲の長さ(Intro等短いものを削除するのに使用)
  popularity = track_data$popularity, # 人気度
  release_date = track_data$album.release_date, # リリース日
  release_date_precision = track_data$album.release_date_precision, # リリース日時単位
  isrc = track_data$external_ids.isrc # ISRCコード
)

bind_cols(temporary_df,test)
