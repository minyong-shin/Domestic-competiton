# AI 소설 작가 예측 대회

## 대회 요강 및 성적
- 각 작가의 소설 데이터(TEXT)를 기반으로 어떤 작가인지 예측하는 Multilabel classification
- 성적: 48/287 (상위 16%)


## 분석 방법론
    
- step1
    1. remove stopwords  
    2. Text Tokenizing
    3. TextToSequence
    4. padding(max:500)
    5. simpleDNN
    6. 5fold cross-validation
    
