library(rvest)
library(dplyr)
library(multilinguer)
library('tidyverse')
library('reshape2')
library('wordcloud2')
library('rJava') 
Sys.getenv("JAVA_HOME")
library('KoNLP')

NEWS <- read.csv("D:/개인인천시청/데이터/민원 데이터/saeol_minwon_B.csv", encoding = 'euc-kr', header = T)
summary(NEWS)

TEXTFILE <- NEWS$minwon_cn

tbl_TEXTFILE <- TEXTFILE %>% 
  SimplePos09 %>% # 품사구분함수. SimplePos09()는 9개 품사로, SimplePos22()는 22개 품사로 구분 
  melt %>%        # 전체 자료를 tibble 형태로 저장 
  as_tibble %>%   # 전체 자료를 tibble 형태로 저장 
  select(3, 1)    # 실제 분석에 필요한 3열과 1열만 따로 저장 

head(tbl_TEXTFILE)
## 명사형 자료만 골라내어 카운트

tbl_TEXTFILECOUNT0 <- tbl_TEXTFILE %>% # tbl_TEXTFILE 데이터 사용 
  mutate(noun=str_match(value, '([가-힣]+)/N')[,2]) %>% # 명사형(/N) 자료만 골라낸 후 /N 빼고 저장 
  na.omit %>% # 비어있는 값(NA) 제외 
  filter(str_length(noun)>=2) %>%  # '것', '수' 와 같이 별 의미가 없는 의존명사를 제외하기 위하여 한글자 단어는 제외
  count(noun, sort=TRUE)

head(tbl_TEXTFILECOUNT0, 10)



tbl_TEXTFILECOUNT1 <- tbl_TEXTFILECOUNT0 %>% filter(n>=5) %>% filter(!noun %in% c(
  "관련","사진","첨부한", "저장", "갤러리", "정보시스템","유지관리운영지원단", "동영상", "일대", "주변", "미추홀구", "첨부파일"," 복사", "촬영", "설치", "붙여넣으면", "해당", "복사","브라우저","주소창","개월",
  "남구","보관","별도","자료","보관해주시길","담당자분","인천광역시","삭제","스토리지에서","업무처리",
  "구동","신고","공간","사진촬영시간","신고위치","시도새올","한정","촬영한","소음","국민신문","문제",
  "개선", "촬영일시","조치","사람","인천","때문","첨부사진","보장하","실행시","부근","생각","안녕","촬영시간", "해결", "기능", "이용", "요청", "민원" , "상황", "상세위치설명", "똥영상촬영시간","저희"))


## 50대 키워드만 선정
tbl_TEXTFILECOUNT2 <- tbl_TEXTFILECOUNT1[1:50,] 

# 워드클라우드 그리기
wordcloud2(tbl_TEXTFILECOUNT2,fontFamily="Malgun Gothic", size = 0.5, minRotation=0, maxRotation=0)



## top20

names(tbl_TEXTFILECOUNT2) <- c("word", "freq")


top20 <- tbl_TEXTFILECOUNT2 %>%
  arrange(desc(freq)) %>%
  head(20)


#그래프 그리기
library(ggplot2)

order <- arrange(top20, freq)$word               # 빈도 순서 변수 생성

ggplot(data = top20, aes(x = word, y = freq)) +  
  ylim(0, 500) +
  geom_col() + 
  coord_flip() +
  scale_x_discrete(limit = order) +              # 빈도 순서 변수 기준 막대 정렬
  geom_text(aes(label = freq), hjust = -0.3)     # 빈도 표시



