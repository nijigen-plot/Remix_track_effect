library(tidyverse)
library(withr)
## データの読み込み ####
adf <- read_csv(file = "data/remix_artist_comparison_df.csv")
df <- read_csv(file = "data/target_tracks.csv")
rdf <- read_csv(file = "data/artist_avg_popularity.csv")

adf <- adf %>% 
  mutate(popularity_diff = popularity - avg_popularity)

adf <- inner_join(
  adf,
  rdf,
  by = c("main_artist" = "artist"),
  suffix = c("_r", "_o")
)

adf <- adf %>% mutate(ro_diff = avg_popularity_o - avg_popularity_r)
adf <- adf %>% mutate(original_artist_famous = if_else(ro_diff > 0, 1, 0))



adf <- adf %>% mutate(avg_p_bin = cut(
  avg_popularity_r,
  breaks = c(
    -1,
    quantile(adf$avg_popularity_r, 0.25),
    quantile(adf$avg_popularity_r, 0.5),
    quantile(adf$avg_popularity_r, 0.75),
    quantile(adf$avg_popularity_r, 1)
  ),
  labels = c('~25%', '26~50%', '51~75%', '~100%')
))


# リネーム
adf <- adf %>% rename(
  remixer_avg_popularity = avg_popularity_r,
  origin_artist = main_artist,
  diff_release_popularity = popularity_diff,
  remixer_popularity_group = avg_p_bin,
  origin_avg_popularity = avg_popularity_o,
  diff_origin_popularity = ro_diff,
  origin_artist_famous = original_artist_famous
  )



data.frame(
  column_name = c(names(adf)),
  意味 = c(
    "Remixしたアーティスト",
    "Remixしたアーティストの平均Popularity",
    "Remix楽曲名",
    "Popularity",
    "楽曲リリース日",
    "Remixされたアーティスト",
    "Popularity(Remix楽曲 - Remixアーティスト)",
    "diff_release_popularityをパーセンタイル4分割したグループ",
    "Remixされたアーティストの平均Popularity",
    "Popularity(Originアーティスト - Remixアーティスト)",
    "OriginアーティストのほうがRemixアーティストより平均Popularityが高いか"
  )
)

## データ確認 ####
View(adf)
length(unique(adf$remix_artist))
names(adf)

## 集計 ####

### 単純集計 ####
data.frame(
  diff_release_popularity = c(summary(adf$diff_release_popularity))
)
ggplot(data = adf, mapping = aes(x=diff_release_popularity)) +
  theme_classic(base_size = 20, base_family = "sans") + 
  stat_boxplot(lwd=1) + 
  geom_boxplot(outlier.size = 2)


### flag別集計 ####
oaf_agg <- adf %>% 
  select(diff_release_popularity, origin_artist_famous) %>% 
  group_nest(origin_artist_famous) %>% 
  mutate(s = map(data, ~ summary(.))) %>% 
  pull(s, origin_artist_famous)

data.frame(
  group_0 = data.frame(oaf_agg$`0`)$Freq,
  group_1 = data.frame(oaf_agg$`1`)$Freq
)


ggplot(data = adf, mapping = aes(x=diff_release_popularity, y=factor(origin_artist_famous), group=factor(origin_artist_famous), colour = factor(origin_artist_famous))) + 
  theme_classic(base_size = 20, base_family = "sans") + 
  stat_boxplot(lwd=1) + 
  geom_boxplot(outlier.size = 2) + 
  scale_fill_discrete(labels=c("group_0","group_1")) + 
  theme(axis.title.y = element_blank()) + 
  labs(color = "group")
  

### flag x Remixアーティスト平均Popularityグループ別
adf
ogaf_agg <- data.frame(aggregate(diff_release_popularity ~ remixer_popularity_group + origin_artist_famous, adf, summary))
ogaf_agg1 <- cbind(
  ogaf_agg %>% select(remixer_popularity_group, origin_artist_famous),
  data.frame(ogaf_agg$diff_release_popularity)  
)
ogaf_agg1


ggplot(data = adf, mapping = aes(x=diff_release_popularity, y=remixer_popularity_group, group=remixer_popularity_group, colour = remixer_popularity_group)) + 
  theme_classic(base_size = 20, base_family = "sans") + 
  stat_boxplot(lwd=1) + 
  geom_boxplot(outlier.size = 2) + 
  theme(axis.title.y = element_blank(), legend.title = element_blank()) + 
  facet_grid(origin_artist_famous ~ .) + 
  par(pin = par(8, 3))


?par
ggplot(data = adf, mapping = aes(x = remixer_avg_popularity, y = diff_origin_popularity, colour = diff_release_popularity)) + 
  geom_point() + 
  scale_colour_gradient(low = "black", high = "red")

ggplot(data = adf, mapping = aes(x = diff_origin_popularity, y = diff_release_popularity, colour = remixer_popularity_group, group = remixer_popularity_group)) + 
  geom_point()

ggplot(data = adf, mapping = aes(x = diff_origin_popularity, y = diff_release_popularity, colour = remixer_popularity_group)) + 
  theme_bw(base_size = 15, base_family = "sans") + 
  geom_hline(yintercept = 0) + 
  geom_point() + 
  geom_smooth(method = 'loess', colour='black', alpha=0.4) +
  theme(legend.position = "none") + 
  facet_wrap( ~ remixer_popularity_group)



dfs <- data.frame()
for (i in c(unique(adf$origin_artist_famous))) {
  for (g in c(sort(unique(adf$remixer_popularity_group)))) {
    tmp_df <- adf %>% 
      filter(origin_artist_famous == i) %>% 
      filter(remixer_popularity_group == g)
    dfs <- rbind(
      dfs,
      with_seed(1, sample_n(tmp_df, 5))
    )
  }
}

column_list <- c(
  "track_name",
  "popularity",
  "release_date",
  "origin_artist",
  "origin_avg_popularity",
  "remix_artist",
  "remixer_avg_popularity",
  "diff_release_popularity",
  "diff_origin_popularity",
  "remixer_popularity_group",
  "origin_artist_famous"
  )

dfs %>% select(column_list)


tmp <- data.frame(
  remixer_avg_popularity = c(summary(adf$remixer_avg_popularity))
)
tmp
