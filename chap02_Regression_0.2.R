# chap02_Regression

#################################
### 1. 주택 가격 예측
#################################
library(MASS) # Boston dataset 제공 

data("Boston")
help("Boston")
str(Boston) # 'data.frame':	506 obs. of  14 variables:
# Boston : 보스턴 시 주택 가격 데이터
# 주택 가격에 영향을 미치는 요소를 분석하고자 하는 목적으로 사용 
# 회귀분석에 활용 
# 범죄율, 학생vs교사 비율 등 보스턴 주택 가격의 중앙값(median value)

#crim : 도시 1인당 범죄율 
#zn : 25,000 평방피트를 초과하는 거주지역 비율
#indus : 비상업지역이 점유하고 있는 토지 비율  
#chas : 찰스강에 대한 더미변수(1:강의 경계 위치, 0:아닌 경우)
#nox : 10ppm 당 농축 일산화질소 
#rm : 주택 1가구당 평균 방의 개수 
#age : 1940년 이전에 건축된 소유주택 비율 
#dis : 5개 보스턴 직업센터까지의 접근성 지수  
#rad : 고속도로 접근성 지수 
#tax : 10,000 달러 당 재산세율 
#ptratio : 도시별 학생/교사 비율 
#black : 자치 도시별 흑인 비율 
#lstat : 하위계층 비율 
#medv : 소유 주택가격 중앙값 (단위 : $1,000)

head(Boston)
# 정규화 
boston_df <- as.data.frame(scale(Boston))
head(boston_df, 3)

set.seed(123)
idx <- sample(1:nrow(boston_df), 300)
trainDF <- boston_df[idx,] # 300  14
testDF <- boston_df[-idx,] # 206  14
dim(trainDF); dim(testDF)

# formula
form <- medv ~ crim+zn+indus+nox+rm+age+dis+rad+tax+ptratio+black+lstat
form2 <- medv ~.

# model 생성 
model_bos <- lm(formula = form, data = trainDF)
model_bos

# -0.04191*crim + 0.09981*zn + 0.01978
names(model_bos)
model_bos$fitted.values[1:5] # 예측치 
# 355        207        260        288        217 
# -0.8169756  0.1442864  1.4498645  0.5376364  0.1419017
# 355 번째 관측치의 예측치(y 값)

model_bos$residuals[1:5] # 예측치 (오차)
# 355         207         260         288         217 
# 0.34587013  0.05873333 -0.62708445 -0.46509248 -0.05848487 
# 355번째 관측치와 예측치의 차. 오차. 오류

trainDF$medv[1:5] # 관측치
# [1] -0.47110550  0.20301974  0.82278004  0.07254389  0.08341687

# 잔차 = 관측치 - 예측치


summary(model_bos) 
# 1. 모델 유의성 검정 : 2.2e-16 < 0.05   # f 검정 통계량
# 2. 모델 설명력 : 0.7462 -> x,y 상관계수   # 통상 0.65 기준으로 잡음
# 3. x 변수 유의성 검정 : sexmale(영향력없음)  # * 세개가 가장 큰 영향력 미침. 

# 5. 회귀모델 성능 평가 - 검정 데이터 이용
pred <- predict(model_bos, testDF)
pred # 검정데이터의 주택가격(medv) 비율척도 예측 


# 상관계수로 예측치 성능 평가  
# 회귀의 경우에만 연속형 변수이기 때문에 confusion matrix 가 아닌 상관계수로 평가 
cor(pred, testDF$medv) #  0.8550106


# 6. 선형회귀분석 잔차검정과 모형진단

# (1) 잔차 독립성(자기상관) 검정 : Durbin-Watson 검정 
# 이전 시점과 현재 시점 간에 상관이 있는가
install.packages('lmtest')
library(lmtest) # 자기상관 진단 패키지 설치 
dwtest(model_bos) # 더빈 왓슨 값(통상 1~3 사이)
p-value = 0.7824 >= 0.05   # 귀무가설 : 서로 연관성이 없다


# (2) 다중공선성 검사 
install.packages('car')
# vif : 분산팽창요인 함수 
library(car)
sqrt(vif(model_bos)) > 2  # nox, tax 제거 팰요
#crim      zn   indus     nox      rm     age     dis     rad     tax ptratio   black   lstat 
#FALSE   FALSE   FALSE    TRUE   FALSE   FALSE   FALSE    TRUE    TRUE   FALSE   FALSE   FALSE 

cor(trainDF[c('nox','dis','rad','tax')])

#nox        dis        rad        tax
#nox  1.0000000 -0.7516044  0.5777546  0.6401367
#dis -0.7516044  1.0000000 -0.4728305 -0.5028322
#rad  0.5777546 -0.4728305  1.0000000  0.9047096
#tax  0.6401367 -0.5028322  0.9047096  1.0000000

# 둘중 뭘 제거하는가? 
# 유의성검정에서 영향력이 적은 변수를 제거 summary 함수에서
# 모델 재 생성

form <- medv ~ crim+zn+indus+rm+age+dis+rad+ptratio+black+lstat
model_bos <- lm(formula = form, data = trainDF)
sqrt(vif(model_bos)) > 2

#crim      zn   indus      rm     age     dis     rad ptratio   black   lstat 
#FALSE   FALSE   FALSE   FALSE   FALSE   FALSE   FALSE   FALSE   FALSE   FALSE


# chap02_Regression_2

#################################
#### 2. 의료비 예측
#################################

# 의료비 예측
# - 의료보험 가입자 1,338명을 대상으로 한 데이터 셋으로 의료비 인상 예측 

# 1. 데이터 셋 가져오기 - insurance.csv
insurance <- read.csv('C:/Rwork-II/data/insurance.csv', header = T)
str(insurance) # sex, smoker 명목척도 -> Factor 형변환(숫자형 의미 적용) 
#'data.frame':	1338 obs. of  7 variables:
#$ age     : 나이 : int  19 18 28 33 32 31 46 37 37 60 ...
#$ sex     : 성별 :(x1) Factor w/ 2 levels "female","male": 1 2 2 2 2 1 1 1 2 1 ...
#$ bmi     : 비도만 지수 : num  27.9 33.8 33 22.7 28.9 ...
#$ children: 자녀수 : int  0 1 3 0 0 0 1 3 2 0 ...
#$ smoker  : 흡연 여부 :(x2) Factor w/ 2 levels "no","yes": 2 1 1 1 1 1 1 1 1 1 ...
#$ region  : 지역 Factor w/ 4 levels "northeast","northwest",..: 4 3 3 2 2 3 3 2 1 2 ...
#$ charges : 의료비(y) : num  16885

# 2. 데이터 탐색

insurance <- insurance[-6]
head(insurance)

# 1) 의료비 분포 보기 
summary(insurance$charges)
# 대표값으로 대칭성 확인  
# 평균 > 중위수 > 최빈수 : 오른쪽 비대칭(왼쪽 기울어짐)
# 평균 < 중위수 < 최빈수 : 왼쪽 비대칭(오른쪽 기울어짐)

install.packages('prettyR') # 최빈수 구하기 위한 패키지
library(prettyR)
Mode(insurance$charges)

hist(insurance$charges)

# 2) 수치형 칼럼 간의 상관관계보기 
cor(insurance[c('age', 'bmi', 'children', 'charges')])
# 큰 상관성은 없는 것으로 나타남

# 3) 상관관계 시각화 
install.packages('psych')
library(psych)
pairs.panels(insurance[c('age', 'bmi', 'children', 'charges')])




# 3. 회귀모델 생성
# 1) 데이터 셋 생성(7:3 비율) 
set.seed(123) # random 결과를 동일하게
idx = sample(1:nrow(insurance), 0.7*nrow(insurance))
training_ins = insurance[idx, ]
testing_ins = insurance[-idx, ]
dim(training_ins) # 936   7
dim(testing_ins) # 402   7

# 2) 회귀모델 생성
model_ins <- lm(charges ~ age + children + bmi + sex + smoker, data=training_ins)
model_ins <- lm(charges ~ ., data=training_ins)
model_ins # 절편과 기울기 보기
model_ins$fitted.values[1:5]
model_ins$residuals[1:5]
training_ins$charges[1:5]
# 관측치 - 예측치 = residuals

# 4. 회귀모델 평가
summary(model_ins) # Adjusted R-squared:  0.7422
# 1. 모델 유의성 검정 : 2.2e-16 < 0.05   # 귀무가설 기각하면 유의함, 0.05 이내의 p 값을 보이면 유의
# 2. 모델 설명력 : 0.7462 -> x,y 상관계수   # 0.65 이상 정도면 설명력이 좋다. 예측력이 좋다. 
# 3. x 변수 유의성 검정 : sexmale(영향력없음)  # 남자인 경우 음의 영향력..

# 5. 회귀모델 성능 평가 - 검정 데이터 이용
pred <- predict(model_ins, testing_ins)
pred # 검정데이터의 의료비(charges) 비율척도 예측 

# 상관계수로 예측치 성능 평가  
cor(pred, testing_ins$charges) # 0.8678934 # 약 87% 예측을 하고 있다 라고 말할 수 있음


# 6. 선형회귀분석 잔차검정과 모형진단

# (1) 잔차 독립성(자기상관) 검정 : Durbin-Watson 검정 
library(lmtest) # 자기상관 진단 패키지 설치 
dwtest(model_ins) # 더빈 왓슨 값(통상 2~4 사이)

#alternative hypothesis: true autocorrelation is greater than 0
# [해설] p < 0.05이면 잔차의 자기상관이 존재한다는 의미이다. 따라서 
# 유의미한 자기상관이 없다고 할 수 있다.DW = 1.9474, p-value = 0.2112>=0.05)


# (2) 다중공선성 검사 
library(car)
sqrt(vif(model_ins)) > 2 # TRUE 
# FALSE    FALSE    FALSE    FALSE    FALSE

####################################################
#  모델 설명력 향상 : 변수 선택법, 상호작용 적용  
####################################################

# 1. 변수 선택법 적용 : 영향력이 있는 설명변수를 선택하는데 효과적이다.  
step(model_ins, direction="both")
# 보통 both 를 많이 씀 
# 1) 전진선택법(forward) : 중요하다고 생각되는 설명변수 부터 모형에 추가하는 방법 
# 2) 후진제거법(backward) : 가장 영향력이 적은 설명변수 부터 제거하는 방법 
# 3) 단계선택법(both) : 전진선택법 + 후진제거법 

# AIC(Akaike’s Information Criterion)/BIC(Bayesian Information Criterion)는 이론적 예측력 지표
# 특히 AIC는 모형의 적합도와 간명성을 동시에 나타내는 지수로 값이 적은 모형 채택, 낮을수록 좋다 
# 참조를 할 수 있는 것이지 절대적인 것은 아님 


model_ins <- lm(charges ~ age + bmi + children + smoker, data=training_ins)
model_ins # 절편과 기울기 보기

# 회귀모델 : 설명력 향상  
summary(model_ins) 

# Adjusted R-squared:  0.7468 로 0.0002 높아짐 


# 2. 고차항 적용
# 비선형 관계에 놓인 변수를 변환
# 숫자형 칼럼 상관계수 
cor(insurance[c('age', 'bmi', 'children', 'charges')])   
age <- training_ins$age
charges <- training_ins$charges
cor(age, charges) 

model <- lm(charges ~ age, data=training_ins)
model

plot(training_ins$age, training_ins$charges)
abline(model, col='red')
# 비선형 관계임을 확인하고 

plot(model, which=1) 
# 잔차 vs 적합값 : U자/역방향(2차항), S자(3차항)  


# 2차항 적용 
model_ins2 <- lm(charges ~ age + I(age**2) + children + bmi + smoker, data=training_ins)
# model_ins2 <- lm(charges ~ age + I(age**2) + I(age**3) + children + bmi + smoker, data=training_ins) # 3차항 적용하면 설명력이 더 낮아짐 
model_ins2 # 절편과 기울기 보기
# y = a*age + a*children + a*bmi + a*bmi^2 + a*smokeryes + b
summary(model_ins2)  


# 3. 상호작용 변수 적용   
# 상호작용이란 독립변수가 다른 독립변수의 값에 따라서 종속변수에 미치는 영향이 달라지는 현상
# 예) 고도비만인 상태에서 흡연유무에 따라서 고도비만이 의료비에 미치는 영향이 달라짐  

training_ins$bmi2 <- ifelse(training_ins$bmi >= 30, 1, 0)
training_ins$bmi2[1:5]



# 상호작용 변수 적용 모델 생성 
model_ins2 <- lm(charges ~ age + children + bmi * smoker, data=training_ins)

model_ins2 <- lm(charges ~ age + children + bmi2 * smoker, data=training_ins) # dummy 화 해서 넣기 
summary(model_ins2) 


###################################
#### 3. 수치예측 관련 모델
###################################
# 모델트리 : 선형회귀모델 방식으로 모델 생성 - RWeka
# - y변수를 수치로 예측
# - 분류모형 + 선형회귀모델 적용

# 설치 후 메모리 로딩 error
# 1. remove.packages('RWeka')
# 2. rebooting
# 3. install

# or

# http://www.oracle.com/technetwork/java/javase/downloads : jdk 최신버전 설치 Java SE Development Kit 8u131
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java/jre1.8.0_131')  # / 는 하나 \\ 는 두개 

 
install.packages('RWeka')
library(RWeka) # M5P()함수 제공 



# 1) 데이터 셋 가져오기
# 포루트갈산의 화이트 와인 실습파일 
wine <- read.csv('C:/Rwork-II/data/whitewines.csv')
str(wine) # 'data.frame':	4898 obs. of  12 variables:
# y변수 : quality(블라인드 테스트를 통해서 0~10 등급으로 와인의 질 적용)
# x변수 : 나머지 11개 변수(화학적 특성)
# -> 산도(acidity), 당도(sugar), 염화물(chlorides),황(sulfur),알코올(alcohol) 등 특성) 

# 2) 데이터 분석/데이터 셋 생성 
hist(wine$quality) # 정규분포 

idx = 1:3700 # 4898 중에서 3700개를 training으로 지정 
training_wine = wine[idx, ]
testing_wine = wine[-idx, ]
dim(training_wine) # 3700   12
dim(testing_wine) # 1198   12


# 3) 모델트리 모델 생성  
model_wine <- M5P(quality ~ ., data = training_wine) 
model_wine

# 4) 모델 성능 평가 - 검정 데이터 이용 
pred2 <- predict(model_wine, testing_wine)

# (1) 요약통계량으로 평가 
summary(pred2)
summary(testing_wine$quality)

# (2) 상관계수로 평가 
cor(pred2, testing_wine$quality) 

######################################
# 4. 로지스틱 회귀분석(Logistic Regression) 
######################################

# 목적 : 일반 회귀분석과 동일하게 종속변수와 독립변수 간의 관계를 나타내어 
# 향후 예측 모델을 생성하는데 있다.

# 차이점 : 종속변수가 범주형 데이터를 대상으로 하며 입력 데이터가 주어졌을 때
# 해당 데이터의결과가 특정 분류로 나눠지기 때문에 분류분석 방법으로 분류된다.
# 유형 : 이항형(종속변수가 2개 범주-Yes/No), 다항형(종속변수가 3개 이상 범주-iris 꽃 종류)
# 다항형 로지스틱 회귀분석 : nnet, rpart 패키지 이용 
# a : 0.6,  b:0.3,  c:0.1 -> a 분류 
# 회귀분석 보다는 분류분석의 범주에 속한다고 볼 수 있음

# 분야 : 의료, 통신, 기타 데이터마이닝

# 선형회귀분석 vs 로지스틱 회귀분석 
# 1. 로지스틱 회귀분석 결과는 0과 1로 나타난다.(이항형)
# 2. 정규분포 대신에 이항분포를 따른다.
# 3. 로직스틱 모형 적용 : 변수[-무한대, +무한대] -> 변수[0,1]사이에 있도록 하는 모형 
#    -> 로짓변환 : 출력범위를 [0,1]로 조정
# 4. 종속변수가 2개 이상인 경우 더미변수(dummy variable)로 변환하여 0과 1를 갖도록한다.
#    예) 혈액형 A인 경우 -> [1,0,0,0] AB(1) -> A,B,O(0)

# y 변수를 먼저 더미화 해 놓고 시작 


# 단계1. 데이터 가져오기
weather = read.csv("c:/Rwork-II/data/weatherAUS.csv", stringsAsFactors = F) 
dim(weather)  # 36881  24
head(weather)
str(weather)

# chr 칼럼, Date, RainToday 칼럼 제거 
weather_df <- weather[, c(-1, -2, -8, -10, -11, -22)]
str(weather_df)

# RainTomorrow 칼럼 -> 로지스틱 회귀분석 결과(0,1)에 맞게 더미변수 생성      
weather_df$RainTomorrow[weather_df$RainTomorrow=='Yes'] <- 1
weather_df$RainTomorrow[weather_df$RainTomorrow=='No'] <- 0
weather_df$RainTomorrow <- as.numeric(weather_df$RainTomorrow)
head(weather_df)

#  단계2.  데이터 셈플링
idx <- sample(1:nrow(weather_df), nrow(weather_df)*0.7)
train <- weather_df[idx, ]
test <- weather_df[-idx, ]
dim(train)


#  단계3.  로지스틱  회귀모델 생성 : 학습데이터 
weater_model <- glm(RainTomorrow ~ ., data = train)
weater_model 
summary(weater_model) 



# 단계4. 로지스틱  회귀모델 예측치 생성 : 검정데이터 
# newdata=test : 새로운 데이터 셋, type="response" : 0~1 확률값으로 예측 
pred <- predict(weater_model, newdata=test, type="response")  
pred 
str(pred) # 비율예측 

cpred <- ifelse(pred >= 0.5 , 1, 0)
table(cpred)

table(cpred, test$RainTomorrow)

# 혼돈 matrix
#cpred    0    1
#0 4044  609
#1   65  590

(4044+590)/ nrow(test)


### ROC Curve를 이용한 모형평가(분류정확도)  ####
# Receiver Operating Characteristic

install.packages("ROCR")
library(ROCR)

# ROCR 패키지 제공 함수 : prediction() -> performance
pr <- prediction(pred, test$RainTomorrow)  # 모형에서 예측한 예측치, 검정데이터의 y 변수 입력 
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)


