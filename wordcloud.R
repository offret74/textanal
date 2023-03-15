library(KoNLP)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(wordcloud)

# useSejongDic()
useNIADic()

setwd("/Users/offret/R")

# hangle resolved (only 1 run)
install.packages("extrafont")
library(extrafont)
font_import()


txt<-readLines("gokseong.txt")
txt<-str_replace_all(txt,"\\W"," ")
txt<-gsub("\\d+","",txt)
txt<-gsub("[A-z]","",txt)
txt<-gsub("[[:cntrl:]]","",txt)

nouns<-extractNoun(txt)

#추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount<-table(unlist(nouns))

# 데이터 프레임으로 변환
re_word <- as.data.frame(wordcount, stringsAsFactors = F)

#변수명 수정
re_word<-rename(re_word,word=Var1,freq=Freq)

# 2글자 이상 단어 추출
re_word<-filter(re_word,nchar(word)>=2)

#빈도수가 많은 순으로 20개만 
top_20<-re_word %>%
  arrange(desc(freq)) %>%
  head(20)

pal <- brewer.pal(8,"Set1") #색깔지정
wordcloud(words = top_20$word,  # 단어
          freq = top_20$freq,   # 빈도
          min.freq = 2,          # 최소 단어 빈도
          max.words = 200,       # 표현 단어 수
          random.order = F,      # 고빈도 단어 중앙 배치
          rot.per = .1,          # 회전 단어 비율
          scale = c(4, 0.3),     # 단어 크기 범위
          colors = pal,          # 색깔 목록
          family="AppleGothic")  # 폰트지정(옵션)
