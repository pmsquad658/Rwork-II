##########################
## 제2장 연습문제 
##########################

# 문1) iris 데이터 셋을 대상으로 다음과 같이 다중선형회귀 모델을 생성하시오.
# <조건1> y변수 : 5번칼럼, x변수 : 1 ~ 4 칼럼
# <조건2> y변수 대상 더미변수 생성(setosa -> 1, versicolor -> 2, virginica -> 3)
#     힌트) ifelse()함수 이용
# <조건3> 7:3 비율로 데이터 셋 구성(train_iris, test_iris)
# <조건4> 다중공성성 문제 확인 후 변수 제거 : 가장 높은 설명력을 나올 수 있도록 변수 제거  
# <조건5> 조정된 회귀모델 이용 성능평가 : 상관계수 이용

iris
names(iris)
# "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"

table(iris$Species)
#setosa versicolor  virginica 
#50         50         50 

# 1. Species 칼럼 더미화
iris$Species2 <- ifelse(iris$Species == 'setosa', 1, 
                       ifelse(iris$Species == 'versicolor', 2, 3))
head(iris[c('Species', 'Species2')])
tail(iris[c('Species', 'Species2')])

# 2. train/test sample
idx <- sample(1:nrow(iris), nrow(iris)*0.7)
train <- iris[idx,]
test <- iris[-idx,]

# 3. model 생성
form <- Species2 ~ Sepal.Length+ Sepal.Width+  Petal.Length+ Petal.Width
model <- lm(formula = form, data = train)
summary(model) # 설명력, x변수 유의성 검정

# 4. predict
pred <- predict(model, test) #예측치 생성
cor(pred, test$Species2) # 모델 평가

# 5.다중공선성 검사 
library(car)
sqrt(vif(model)) > 2  # 3개 변수가 의심스러운 것으로 나옴

form <- Species2 ~ Sepal.Length+ Petal.Width
model1 <- lm(formula = form, data = train)
sqrt(vif(model1)) > 2  # 3개 변수가 의심스러운 것으로 나옴





# 문2) mtcars 데이터 셋을 이용하여 다음과 조건에 맞게 모델트리와 회귀모델을 각각 생성하시오.
# <조건1> 변수 모델링 : mpg ~ hp + wt
# 변수 설명 : mpg(연비), hp(마력), wt(중량) 
data(mtcars)
str(mtcars)

model1 <- M5P(mpg ~ hp +wt, data=mtcars)
model2 <- lm(mpg ~ hp +wt, data=mtcars)

# <조건2> 회귀계수 확인 
model
model2

# <조건3> 마력과 중량 상호작용 변수 적용
model3 <- lm(mpg ~ hp + wt + hp*wt, data=mtcars)
summary(model3)


# 문3) insurance 데이터 셋을 이용하여 다음과 조건에 맞게 모델트리을 생성하시오.
# <조건1> 7:3 비율 데이터 셋 구성 
# <조건2> 변수 모델링 : charges ~ age + children + bmi + sex + smoker
# <조건3> 모델 성능평가(상관계수 이용) 

insurance <- read.csv('C:/Rwork-II/data/insurance.csv', header = T)
insurance <- insurance[-6]

set.seed(123) # random 결과를 동일하게
idx = sample(1:nrow(insurance), 0.7*nrow(insurance))
training_ins = insurance[idx, ]
testing_ins = insurance[-idx, ]
dim(training_ins) # 936   7
dim(testing_ins) # 402   7

model_ins <- M5P(charges ~ age + children + bmi + sex + smoker, data=training_ins)
model_ins # 절편과 기울기 보기

# 4) 모델 성능 평가 - 검정 데이터 이용 
pred2 <- predict(model_ins, testing_ins)

# (1) 요약통계량으로 평가 
summary(pred2)
summary(testing_ins$charges)

# (2) 상관계수로 평가 
cor(pred2, testing_ins$charges)

