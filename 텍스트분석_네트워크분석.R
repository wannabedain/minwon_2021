library(rvest)
library(dplyr)
library(multilinguer)
library(stringr)
library(textclean)



NEWS <- read.csv("D:/개인인천시청/데이터/민원 데이터/saeol_minwon_B.csv", encoding = 'euc-kr', header = T)
summary(NEWS)

#전처리
news_comment <- NEWS %>%
  select(minwon_cn) %>%
  mutate(minwon_cn = str_replace_all(minwon_cn, "[^가-힣]", " "), 
         minwon_cn = str_squish(minwon_cn), 
         id = row_number())


#토큰화 하기
library(tidytext)
library(KoNLP)

comment_pos <- news_comment %>%
  unnest_tokens(input = minwon_cn,
                output = word,
                token = SimplePos22, # 문장의 단어를 22개의 품사
                drop = F)
comment_pos %>%
  select(minwon_cn, word)


#품사 분리 및 행 구성하기
library(tidyr)
comment_pos <- comment_pos %>%
  seperate_rows(word, sep = "[+]")
comment_pos %>%
  select(word, minwon_cn)


#품사 추출하기
#명사
noun <- comment_pos %>%
  filter(str_detect(word, "/n")) %>%
  mutate(word = str_remove(word, "/.*$"))
noun %>%
  select(word, minwon_cn)



#명사 빈도
noun0 <- noun %>%
  count(word, sort = T)

head(noun0,3)


#일부 명사 제외
noun0 <- noun %>% 
  filter(!word %in% c(
    "관련","사진","첨부한", "저장", "갤러리", "정보시스템","유지관리운영지원단", "동영상", "일대", "주변", "미추홀구",
    "첨부파일"," 복사", "촬영", "설치", "붙여넣으면", "해당", "복사","브라우저","주소창","개월",
    "남구","보관","별도","자료","보관해주시길","담당자분","인천광역시","삭제","스토리지에서","업무처리",
    "구동","신고","공간","사진촬영시간","신고위치","시도새올","한정","촬영한","소음","국민신문","문제", "사진", "관련",
    "개선", "촬영일시","조치","사람","인천","때문","첨부사진","보장하","실행시","부근","생각","안녕","촬영시간", "해결",
    "기능","이용", "요청", "민원" , "상황", "상세위치설명", "동영상촬영시간","저희", "첨부한", "갤러리", "안내", 
    "정보시스템", "지방행정통합", "이후","유지관리운영지원단", "해당", "동영상","부근", "사진한정", "경우", "첨부","이후" ,"앨",
    "하나","사용법", "첨부동영상", "부탁드립니",'거기','오늘','바랍니','갑사합니','손님들이','거기','마스크')) 



'''
#동사 형용사 추출하기
pvpa <- comment_pos %>%
  filter(str_detect(word, "/pv|pa")) %>%
  mutate(word = str_replace(word, "/.*$", "다"))
pvpa %>%
  select(word, minwon_cn)
'''



#추출한 데이터 변형 (글자수 2이상만 추출)
comment <- noun0 %>%
  filter(str_count(word) >= 2) %>% 
  arrange(id)
comment %>%
  select(word, minwon_cn)



#단어 동시 출현 빈도 구하기

library(widyr)
pair <- comment %>%
  pairwise_count(item = word,
                 feature = id,
                 sort = T)
pair

##특정 단어와 자주 함께 사용된 단어 살펴보기
pair2 <- pair %>% filter(item1 == "흡연")



#네트워크 그래프 데이터 만들기
library(tidygraph)
graph_comment <- pair %>%
  filter(n >= 25) %>% 
  as_tbl_graph()
graph_comment



#네트워크 그래프 함수 만들기
library(ggraph)
word_network <- function(x) {            
  ggraph(graph_comment, layout = "fr") +   # 레이아웃
    geom_edge_link(color = "gray50",        # 엣지 색깔
                   alpha = 0.5) +                          # 엣지 명암
    geom_node_point(color = "lightcoral",   # 노드 색깔
                    size = 5) +                             # 노드 크기
    geom_node_text(aes(label = name),       # 텍스트 표시
                   repel = T,                              # 노드밖 표시
                   size = 5,                               # 텍스트 크기
                   family = "nanumgothic") +               # 폰트
    theme_graph()                           # 배경 삭제
}
set.seed(1234)
word_network(graph_comment)








# 유의어 처리하기
comment <- comment %>%
  mutate(word = ifelse(str_detect(word, "감독") &
                         !str_detect(word, "감독상"), "봉준호", word),
         word = ifelse(word == "오르다", "올리다", word),
         word = ifelse(str_detect(word, "축하"), "축하", word))

# 단어 동시 출현 빈도 구하기
pair <- comment %>%
  pairwise_count(item = word,
                 feature = id,
                 sort = T)

# 네트워크 그래프 데이터 만들기
graph_comment <- pair %>%
  filter(n >= 25) %>%
  as_tbl_graph()

# 네트워크 그래프 만들기
set.seed(1234)
word_network(graph_comment) 



'''
# 고유벡터 중심성
set.seed(1234)
graph_comment <- pair %>%
  filter(n >= 10) %>%
  as_tbl_graph(directed = F) %>% # 네트워크 그래프 만들기, 방향성 x
  mutate(eigen=centrality_eigen()) %>%
  write.csv('D:/개인인천시청/데이터/word_centrality_eigen.csv')
'''



# 연결 중심성 / 커뮤니티 변수
set.seed(1234)
graph_comment <- pair %>%
  filter(n >= 25) %>%
  as_tbl_graph(directed = F) %>% # 네트워크 그래프 만들기, 방향성 x
  mutate(centrality = centrality_degree(), #연결 중심성
         group = as.factor(group_infomap())) #커뮤니티 변수 추가 




# 연결 중심성 / 커뮤니티 변수 표현
set.seed(1234)

ggraph(graph_comment, layout = "fr") +    # 레이아웃
  geom_edge_link(color = "gray50",         # 엣지 색깔
                 alpha = 0.5) +                           # 엣지 명암
  geom_node_point(aes(size = centrality,   # 노드 크기
                      color = group),                          # 노드 색깔
                  show.legend = F) +                       # 범례 삭제
  scale_size(range = c(5, 15)) +           # 노드 크기 범위
  geom_node_text(aes(label = name),        # 텍스트 표시
                 repel = T,                               # 노드밖 표시
                 size = 5,                                # 텍스트 크기
                 family = "nanumgothic") +                # 폰트
  theme_graph()                            # 배경 삭제

head(graph_comment)


#네트워크의 주요 단어 살펴보기1 (흡연)
graph_comment %>%
  filter(name == "흡연")

##-> group 1로 나옴 / 그룹1 살펴보기 (같은 커뮤니티)
graph_comment %>%
  filter(group == 5) %>%
  arrange(-centrality) 
data.frame()

### 연결 중심성이 높은 주요 단어
graph_comment %>%
  arrange(-centrality)
data.frame()



#네트워크의 주요 단어 살펴보기2 (담배꽁초)
graph_comment %>%
  filter(name == "담배꽁초")

##-> group 3로 나옴 / 그룹3 살펴보기 (같은 커뮤니티)
graph_comment %>%
  filter(group == 3) %>%
  arrange(-centrality)
data.frame()





graph_comment %>%
  filter(name == "용현동")
##-> group 2로 나옴 / 그룹2 살펴보기 (같은 커뮤니티)
graph_comment %>%
  filter(group == 2) %>%
  arrange(-centrality)
data.frame()


### -> 같이 쓰인거 살펴보기
news_단속 <- news_comment %>%
  filter(str_detect(minwon_cn, "단속") & str_detect(minwon_cn, "게임")) %>%
  select(minwon_cn)

write.csv(news_단속, 'D:/개인인천시청/데이터/news_단속.csv')


#파이계수 구하기
word_cors <- comment %>%
  add_count(word) %>%
  filter(n >= 20) %>%
  pairwise_cor(item = word,
               feature = id,
               sort = T)


# 특정 단어와 관련성이 큰 단어 살펴 보기 
word_cors %>%
  filter(item1 == "흡연")





##파이 계수로 막대 그래프 만들기
# 관심 단어 목록 생성
target <- c("담배", "흡연", "단속", "담배꽁초", "생활불편신고", "피해")

top_cors <- word_cors %>%
  filter(item1 %in% target) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 8)

# 그래프 순서 정하기
top_cors$item1 <- factor(top_cors$item1, levels = target)

library(ggplot2)

ggplot(top_cors, aes(x = reorder_within(item2, correlation, item1),
                     y = correlation,
                     fill = item1)) +
  geom_col(show.legend = F) +
  facet_wrap(~ item1, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))




set.seed(1234)
graph_cors <- top_cors %>%
  filter(correlation >= 0.2) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))


# 파이계수 활용한 그래프 만들기




# 파이계수 활용한 그래프 만들기

set.seed(1234)
ggraph(graph_cors, layout = "fr") +
  geom_edge_link(color = "gray50",
                 aes(edge_alpha = correlation,              # 엣지 명암
                     edge_width = correlation),                 # 엣지 두께
                 show.legend = F) +                         # 범례 삭제
  scale_edge_width(range = c(1, 4)) +        # 엣지 두께 범위
  geom_node_point(aes(size = centrality,
                      color = group),
                  show.legend = F) +
  scale_size(range = c(5, 10)) +
  geom_node_text(aes(label = name),
                 repel = T,
                 size = 5,
                 family = "nanumgothic") +
  theme_graph()



###lda 모델 만들기

#전처리 2번째
news_comment_lda <- NEWS %>%
  select(minwon_cn) %>%
  mutate(minwon_cn = str_replace_all(minwon_cn, "[^가-힣]", " "), 
         minwon_cn = str_squish(minwon_cn), 
         id = row_number()) %>%
  
  #중복 제거
  distinct(minwon_cn, .keep_all = T )%>%
  
  filter(str_count(minwon_cn,boundary("word")) >= 3 )
  
  
  


#명사 추출하기 하기

commnet_lda <- news_comment_lda %>%
  unnest_tokens(input = minwon_cn,
                output = word,
                token = extractNoun, # 명사
                drop = F) %>%
  filter(str_count(word) > 1) %>%

  #중복 단어 제거
  group_by(id) %>%
  distinct(word, keep_all = T) %>%
  ungroup() %>%
  select(id, word)



#빈도가 높은 단어 제거하기
count_Word_lda <- comment %>%
  add_count(word) %>%
  filter(n <= 200) %>%
  select(-n)


#불용어 제거하기, 유의어 처리하기
count_Word_lda %>%
  count(word, sort = T) %>%
  print



#불용어 목록 만들기
stopword <- c("안전신문", "담배피","정도","근처","확인","답변","어디","번길","앨범","앱으로","각종","가능","이것","이곳","그것","이해","한두","가지","기타","감사합니","기관", "문자","제거","본인"," 이상"," 그때","진짜","사이","부착","사이","정확","지역", "문의드", "불구","하루","부터","뻑뻑합니다","생활불편스마트신고에서", "이전", "여러", "우리","지나", "사진들")

#불용어, 유의어 처리하기
count_word_ida <- count_Word_lda %>%
  filter(!word %in% stopword) %>%
  mutate(word = recode(word,
                       "흡연부스" = "흡연실",
                       "학생들이" = "학생들",
                       "청소년들" = "아이들",
                       "아이들이" = "아이들",
                       "주민들이" = "주민",
                       "경찰관" = "경찰",
                       "스트레스" = "불쾌",
                       "처리해" = "처리",
                       "여러번" = "반복",
                       "단속좀" = "단속",
                       "흡연자들이" = "흡연자",
                       "시민들이" = "시민",
                       "노인분" = "노인",
                       "요청드" = "요청",
                       ))


# 불용어 제거 및 유의어 수정
stopword <- tibble(word=c("안전신문", "담배피","정도","근처","확인","답변","어디","번길","앨범","앱으로","각종","가능","이것","이곳","그것","이해","한두","가지","기타","감사합니","기관", "문자","제거","본인"," 이상"," 그때","진짜","사이","부착","사이","정확","지역", "문의드", "불구","하루","부터","뻑뻑합니다","생활불편스마트신고에서", "이전", "여러", "우리","지나", "사진들"))


count_word_ida <- count_word_ida %>%
  filter(!word %in% stopword$word)



#문서별 단어 빈도 구하기
count_word_doc <- count_word_ida %>%
  count(id, word, sort = T)

head(count_word_doc,30)


#DTM 만들기
#install.packages("tm")

dtm_comment <- count_word_doc %>%
  cast_dtm(document = id, term = word, value= n)

dtm_commet ## (documents: 543, terms: 6358)


##DTM 내용 확인하기
library(tm)
as.matrix(dtm_comment[1:8,1:8])



##LDA 모델 만들기
library(topicmodels)

#토픽 모델 만들기
lda_model <- LDA(dtm_comment, 
                 k=8,
                 method = "Gibbs",
                 control = list(seed = 1234))
lda_model


#토픽별 단어 확률
term_topic <- tidy(lda_model, matrix = "beta")
term_topic


#토픽별 단어 수
term_topic %>%
  count(topic)


#특정 단어 확인하기
term_topic %>%
  filter(term == "담배꽁초")

#토픽별 주요 단어 살펴보기
term_topic %>%
  filter(topic == 3) %>%
  arrange(-beta)

## 모든 토픽의 주요 단어 살펴보기
terms(lda_model, 20) %>%
  data.frame()


#토픽별 beta상위 10개
top_term_topic <- term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n=10)


library(scales)
library(ggplot2)
ggplot(top_term_topic,
       aes(x=reorder_within(term, beta, topic),
           y= beta,
           fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~topic, scales = "free", ncol = 2) +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(n.breaks = 4,
                     labels = number_format(accuracy = .01)) +
  labs(x=NULL)




###토픽별 문서 수와 단어 시각화하기
top_terms <- term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n=6, with_ties = F) %>%
  summarise(term = paste(term, collapse = ", "))



## 문서를 토픽별로 분류하기
# 감마 추출하기
doc_topic <- tidy(lda_model, matrix = "gamma")
doc_topic


## 문서별로 확률이 가장 높은 토픽 추출
doc_class <- doc_topic %>%
  group_by(document) %>%
  slice_max(gamma, n=1)













##원문에 확률이 가장 높은 토익 번호 부여
doc_class$document <- as.integer(doc_class$document) #integer


#원문에 토픽 번호 부여
news_comment_topic <-news_comment %>%
  left_join(doc_class, by = c("id" = "document"))

# 결합 확인
news_comment_topic %>%
  select(id, topic)



## 토픽별 문서 수 살퍄보기
#



doc_topic %>%
  group_by(document) %>%
  slice_max(gamma, n=1) %>%
  count(document) %>%
  filter(n >= 2)


### 토픽별 문서 수와 단어 시각화하기
top_terms <- term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n=6, with_ties = F) %>%
  summarise(term = paste(term, collapse = ", "))

###토빅별 문서 빈도 구학

count_topic <- news_comment_topic %>%
  count(topic)

count_topic

##문서 빈도에 주요 단어 결합하기
count_topic_word <- count_topic %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic_name = paste("Topic", topic))

count_topic_word 



## 토픽별 문서 수와 주요 단어로 막대 그래프 만들기
ggplot(count_topic_word, 
       aes(x = reorder(topic_name, n),
           y=n, 
           fill = topic_name)) +
  geom_col(show.legend = F) +
  coord_flip() +
  
  geom_text(aes(label = n),
            hjust = -0.2) +
  geom_text(aes(label = term),
            hjust = 1.03,
            col = "white",
            fontface = "bold") +
  scale_y_continuous(limits = c(0, 200)) +
  labs(x = NULL)



##하이퍼파라미터 튜닝으로  토픽 수 정하기
#install.packages("ldatuning")
library(ldatuning)

models <- FindTopicsNumber(dtm = dtm_comment,
                          topics = 2:20, 
                          return_models = T, 
                          control = list(seed = 1234))
models %>%
  select(topics, Griffiths2004)


FindTopicsNumber_plot(models)


#토픽 수가 8개인 모델 추출하기
optimal_model <- models %>%
  filter(topics == )
