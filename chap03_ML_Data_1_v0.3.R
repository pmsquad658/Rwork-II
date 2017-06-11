# chap03_ML Data

# 텍스트 마이닝(Text Mining)
# - 정형화 되지 않은 텍스트를 대상으로 유용한 정보를 찾는 기술
# - 토픽분석, 연관어 분석, 감성분석 등
# - 텍스트 마이닝에서 가장 중요한 것은 형태소 분석
# - 형태소 분석 : 문장을 분해 가능한 최소한의 단위로 분리하는 작업 
# tm 패키지는 영문 형태소 분석에 성능이 좋은 것으로 알려져 있음 

# 1. sms_spam.csv 가져오기 - stringsAsFactors = FALSE : factor형으로 읽지 않음
sms_data <- read.csv('C:/Rwork-II/data/sms_spam.csv', stringsAsFactors = FALSE)
str(sms_data)
#'data.frame':	5558 obs. of  2 variables:
# $ type: chr  "ham" "ham" "ham" "spam" ...
# $ text: chr

# 2. 분석을 위한 데이터 처리 : sms 문장을 단어 단위로 생성 

# tm 패키지 구 버전 다운로드/설치 - version 3.3.2
install.packages('slam')
install.packages("http://cran.r-project.org/bin/windows/contrib/3.0/tm_0.5-10.zip",repos=NULL)
library(tm) 
install.packages('SnowballC')
library(SnowballC) 

sms_corpus = Corpus(VectorSource(sms_data$text)) # 1) 말뭉치 생성(vector -> corpus 변환) 
sms_corpus # A corpus with 5558 text documents
str(sms_corpus)
inspect(sms_corpus)
sms_corpus[[1]]
# Hope you are having a good week. Just checking in
sms_corpus[[4]]
# complimentary 4 STAR Ibiza Holiday or 짙10,000 cash needs your URGENT collection. 09066364349 NOW from +
# Landline not to lose out! Box434SK38WP150PPM18+

# 명령실행 실행 후 sms_corpus[[1]], sms_corpus[[4]] 로 확인 
sms_corpus = tm_map(sms_corpus, tolower)  # 2) 소문자 변경
sms_corpus = tm_map(sms_corpus, removeNumbers) # 3) 숫자 제거 
sms_corpus = tm_map(sms_corpus, removePunctuation) # 4) 문장부호(콤마 등) 제거 

stopwords("SMART") # 571 # stopwords 에 해당하는 것 검색 
stopwords('english') #174
sms_corpus = tm_map(sms_corpus, removeWords, stopwords("SMART")) # 5) stopwords(the, of, and 등) 제거  
sms_corpus = tm_map(sms_corpus, stripWhitespace) # 6) 여러 공백 제거(stopword 자리 공백 제거)   
sms_corpus = tm_map(sms_corpus, stemDocument) # 7) 유사 단어 어근 처리  # 동명사나 분사를 명사로 변환 
sms_corpus = tm_map(sms_corpus, stripWhitespace) # 8) 여러 공백 제거(어근 처리 공백 제거)
sms_corpus[[4]]
# DocumentTermMatrix 작성 
sms_dtm = DocumentTermMatrix(sms_corpus) # 9) 문서와 단어 집계표 작성
# A document-term matrix (5558 documents, 6822 terms)
# 5558 개 문장 행, 6822 개 단어 컬럼 , 0,1 로 코딩이 됨. 희소행렬 37883047, 채워진것 33629
# 희소행렬 퍼센트 : 100% (채워진게 1프로 이하 )


# 3. 단어 구름 시각화 
install.packages('wordcloud')
library(wordcloud) # 단어 시각화(빈도수에 따라 크기 달라짐)

# 색상 12가지 적용 
pal <- brewer.pal(12,"Paired") # RColorBrewer 패키지 제공(wordcloud 설치 시 함께 설치됨) 12가지 색상을 적용하겠다 

# 전치행렬 수행
sms_mt <- as.matrix(t(sms_dtm)) # 행렬 변경
# 전치행렬로 term-document 으로 바꾼 후, 형을 matrix 로 바꿔 볼 수 있게 만든다 
str(sms_mt)
# $ Terms: chr [1:7692] "008704050406" "0089mi" "0121" "01223585236"
# $ Docs : chr [1:5558] "1" "2" "3" "4" ...  -> docs 는 순번으로 표현 
table(sms_mt[1,]) # 첫번째 순번 단어의 출현 빈도를 보여줌 : 전체 문서에서 3번 출현 
#   0    1 
#5555    3 
table(sms_mt[2,])
#    0    1 
# 5557    1 

# 행 단위(단어수) 합계 -> 내림차순 정렬
rsum <- sort(rowSums(sms_mt), decreasing=TRUE)
# call            dont            free            love            time             day 
#  654             286             275             248             244             242 
# call 이 가장 많이 출현 
# vector에서 칼럼명(단어명) 추출 
myNames <- names(rsum) # rsum 변수에서 칼럼명(단어이름) 추출  
myNames # 단어명 

# 단어와 빈도수를 이용하여 df 생성
df <- data.frame(word=myNames, freq=rsum)
head(df) # word freq
row.names(df) <- NULL

# wordCloud(단어(word), 빈도수(v), 기타 속성)
# random.order=T : 위치 랜덤 여부(F 권장) , rot.per=.1 : 회전수, colors : 색상, family : 글꼴 적용 
wordcloud(df$word, df$freq, min.freq=2, random.order=F, scale=c(4,0.7),
          rot.per=.1, colors=pal, family="malgun") 


########################################
### 단어수 조정 - 단어길이, 가중치 적용 
########################################
sms_dtm = DocumentTermMatrix(sms_corpus) # 9)
sms_dtm  # 현재 길이 40자가 가장 길다 

# 1. 단어길이 : 1 ~ 8
sms_dtm1 = DocumentTermMatrix(sms_corpus,
                              control = list(wordLengths= c(1,8))) # control 옵션으로 길이 제한 
sms_dtm1  # 100 여개 정도 필터링이 되었음 

# DTM -> TDM 변경 
t(sms_dtm1) # (terms: 6156, documents: 5558)
sms_tdm1 <- as.matrix(t(sms_dtm1))
table(sms_tdm1[2,])
#    0    1 
# 5557    1 

# 2. 가중치 : 단어출현빈도로 가중치(비율) 적용 
# - 출현빈도수 -> 비율 가중치 조정 # term frequency- inverse document frequency
sms_dtm2 = DocumentTermMatrix(sms_corpus,
                              control = list(wordLengths= c(1,8),  weighting = weightTfIdf))

sms_dtm2 
# DTM -> TDM 변경 
sms_dtm2_tm <- as.matrix(t(sms_dtm2))
str(sms_tdm2)
table(sms_dtm2_tm[2,])

################################
### DTM 대상 단어 검색 
################################

# 단어 길이로 검색 
terms <- sms_dtm1$dimnames$Terms # as.matrix 로 형변환 하는 게 더 편리함 

terms # 단어만 벡터화를 하고 

# 길이가 5~6자 이상 단어 검색 
library(stringr)
result <- terms[str_length(terms) >= 5 & str_length(terms) <= 6]
result
length(result) # 

# 빈도수 이용 단어 검색
library(tm)
findFreqTerms(sms_dtm1, lowfreq = 40)
