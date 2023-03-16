# KoNLP package installation
# --------------------------------------------------------------------

install.packages("multilinguer")
library(multilinguer)
install_jdk()

install.packages(c("stringr", "hash", "tau", "Sejong", "RSQLite", "devtools"),
                 type = "binary")

install.packages("remotes")
remotes::install_github("haven-jeon/KoNLP",
                        upgrade = "never",
                        INSTALL_opts = c("--no-multiarch"))

# -------------------------------------------------------------------------

library(KoNLP)

useNIADic()


# extraNoun test
# -------------------------------------------------------------------------
library(dplyr)
text <- tibble(
  value = c("대한민국은 민주공화국이다.",
            "대한민국의 주권은 국민에게 있고, 모든 권력은 국민으로부터 나온다."))
text

extractNoun(text$value)

# unnest_tokens test
# -------------------------------------------------------------------------
library(tidytext)
text %>%
  unnest_tokens(input = value,        # 분석 대상
                output = word,        # 출력 변수명
                token = extractNoun)  # 토큰화 함수


# WordCloud Example
# -------------------------------------------------------------------------
# data import 
raw_text <- readLines("gj.txt", encoding = "UTF-8")

# 기본적인 전처리
library(stringr)

text <- raw_text %>%
  str_replace_all("[^가-힣]", " ") %>%  # 한글만 남기기
  str_squish() %>%                      # 중복 공백 제거
  as_tibble()                           # tibble로 변환

# 명사 기준 토큰화
word_noun <- text %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)

word_noun


# 02-2 --------------------------------------------------------------------

word_noun <- word_noun %>%
  count(word, sort = T) %>%    # 단어 빈도 구해 내림차순 정렬
  filter(str_count(word) > 1)  # 두 글자 이상만 남기기

word_noun


# -------------------------------------------------------------------------
# 상위 20개 단어 추출
top50 <- word_noun %>%
  head(100)

# delete non-Noun word

wordlist <- top50[c(-1, -6, -14, -16),]
# wordlist <- wordlist %>% head(30)

# 막대 그래프 만들기
library(ggplot2)
ggplot(wordlist, aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  labs(x = NULL) +
  theme(text = element_text(family = "AppleGothic"))

# -------------------------------------------------------------------------
# 폰트 설정
library(showtext)
font_add_google(name = "Black Han Sans", family = "blackhansans")
showtext_auto()


# wordcloud 1 --------------------------------------------------------------------
library(RColorBrewer)
library(wordcloud)

pal <- brewer.pal(8,"Set2") #색깔지정
wordcloud(words = wordlist$word,  # 단어
          freq = wordlist$n,   # 빈도
          min.freq = 2,          # 최소 단어 빈도
          max.words = 200,       # 표현 단어 수
          random.order = F,      # 고빈도 단어 중앙 배치
          rot.per = .1,          # 회전 단어 비율
          scale = c(5, 1),     # 단어 크기 범위
          colors = pal,          # 색깔 목록
          family="blackhansans")  # 폰트지정(옵션)

# wordcloud 2 --------------------------------------------------------------------

library(ggwordcloud)

ggplot(wordlist, aes(label = word, size = n, col = n)) +
  geom_text_wordcloud(seed = 1234, family = "blackhansans") +
  scale_radius(limits = c(2, NA),
               range = c(2, 12)) +
  scale_color_gradient(low = "#66aaf2", high = "#004EA1") +
  theme_minimal()

