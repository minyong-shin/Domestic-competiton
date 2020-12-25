# 태양광 발전량 예측 대회
- 대회 개요: 과거의 태양광 발전량 데이터를 활용하여 미래의 발전량을 예측하는 대회
- 문제: Day0 ~ Day6 7일치의 데이터를 활용하여 Day7, Day8일의 값을 예측하는 시계열 대회
- 문제를 통한 해결방안: 미래 에너지 예측 모델 고도화 및 관련 산업 촉진


## 평가 척도 및 예측 변수
- Metric: pinballLoss(quantile)
- 예측변수: 각 데이터의 예측 시점에 대한 quantile1 ~ quantile9



## Ideation list

- idea 1: use part test data(해당 아이디어로 픽스)
  - Test 데이터를 부분적으로만 사용하여 + N 일 이후를 예측하는 방법
  - timestamp(30 minute)로 되어 있는 train 데이터를 + N 일 이후를 예측하는 데이터 셋으로 transform
  - 다양한 N시간의 회귀 모델을 만든 뒤 앙상블하는 방식도 고안
    - ex) 24시간 뒤를 예측하는 모델, 48시간 뒤를 예측하는 모델을 만들어서 앙상븡
  - 사용하지 않는 인덱스가 있다면 해당 데이터를 변수화할 수 있는 방법도 고안 필요

- idea 2: use all test data
  - 7일치의 데이터를 컬럼으로 구축한 뒤 컬럼으로 만든 뒤 예측한다
  - 불가능할 듯 -> MOA에서 파생해봤는데 불가능하겠다

- idea 3: Timeseries model
  - arima
  - prophet
  - lstm, bi-lstm

## Commit version list

- 
