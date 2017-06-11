###############################
## chap03_ML_Data(2)
###############################
# 기계학습을 위한 데이터 셋 생성 

# 1. sms_spam.csv 가져오기 - stringsAsFactors = FALSE : factor형으로 읽지 않음 
sms_data <- read.csv('C:/Rwork-II/data/sms_spam.csv', stringsAsFactors = FALSE)
str(sms_data)


# 2. 분석을 위한 데이터 처리 : sms 문장을 단어 단위로 생성해야 한다. 
library(tm)
library(SnowballC) # stemDocument()함수 제공 
sms_corpus = Corpus(VectorSource(sms_data$text)) # 1) 말뭉치 생성(vector -> corpus 변환) 
sms_corpus = tm_map(sms_corpus, tolower)  # 2) 소문자 변경
sms_corpus = tm_map(sms_corpus, removeNumbers) # 3) 숫자 제거 
sms_corpus = tm_map(sms_corpus, removePunctuation) # 4) 문장부호(콤마 등) 제거 
sms_corpus = tm_map(sms_corpus, removeWords, stopwords("SMART")) # 5) stopwords(the, of, and 등) 제거  
sms_corpus = tm_map(sms_corpus, stripWhitespace) # 6) 여러 공백 제거(stopword 자리 공백 제거)   
sms_corpus = tm_map(sms_corpus, stemDocument) # 7) 유사 단어 어근 처리 
sms_corpus = tm_map(sms_corpus, stripWhitespace) # 8) 여러 공백 제거(어근 처리 공백 제거)   


################################
### DTM 생성 -> X변수 생성  #
################################

# 1. DTM 생성 -> 단어길이 : 2 ~ 8, 출현빈도수로 가중치 
sms_dtm = DocumentTermMatrix(sms_corpus,
                             control = list(wordLengths= c(2,8),  weighting = weightTfIdf))  # 단어의 출현빈도를 weight 로 적용 
sms_dtm 


# 2. DTM -> 원본 변경 
sms_dtm_mat <- as.matrix(sms_dtm)
sms_dtm_mat


# 3. 가중치 -> Factor('YES', 'NO')
convert_Func <- function(x){
  x <- ifelse(x > 0, 1, 0)
  f <- factor(x, levels = c(0,1), labels = c('NO','YES'))
  return(f)
}

# 4. sms_dtm 사용자 함수 적용 
sms_dtm_mat_text <- apply(sms_dtm_mat, 2, convert_Func)  # 컬럼 단위로 어플라이 

sms_dtm_mat_text[,1] # 첫번째 단어  # 행은 문서, 컬럼은 단어 
table(sms_dtm_mat_text[,1])

# 5. data.frame(type+sms_dtm_mat_text)

sms_data_df <- data.frame(sms_data$type, sms_dtm_mat_text)
str(sms_data_df)

# 6. file save
setwd('c:/Rwork-II/data')
write.csv(sms_data_df, 'sms_data_df.csv', quote = F, row.names = F)
