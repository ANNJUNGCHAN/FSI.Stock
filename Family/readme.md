# EDA
### Data
‘지역 및 연령별 10대 소비(가계자금) 금액 및 금융자산’ 데이터는 농협은행에서 만든 데이터 자료로써, 연령별로는 10대, 20대, 30대, 40대, 50대, 60대, 70대 이상으로 나누고, 대한민국의 지역을 총 17개로 분류 (강원, 경기, 경남, 경북, 광주, 대구, 대전, 부산, 서울, 세종, 울산, 인천, 전남, 전북, 제주, 충남, 충북, 총 8도, 1 특별자치도, 1특별광역시, 1 특별자치시, 6광역시)하여, 2020년 12월 말을 기준으로 공과금 평균 소비금액, 생활자금 평균 소비금액. 세금 평균 소비금액, 연금 평균 소비금액과 같은 소비(가계자금)금액의 평균을 조사하고, 총 저축 평균 잔액, 총 대출 평균잔액과 같은 금융자산을 조사한 데이터 자료이다.
이 때, 분석을 위해 기초적으로 데이터를 변환시켜주었다.

### 관측치
이 때, 연령대와 시도명의 경우, 데이터분석을 용이하게 하기 위해서 1~119까지의 숫자로 라벨링하였다. 그 내역은 아래와 같다.
![image](https://user-images.githubusercontent.com/89781598/193550776-27567113-9798-45da-8e78-c27d6268167b.png)

### 변수
![image](https://user-images.githubusercontent.com/89781598/193550854-3ffa49e0-4ef8-4de6-95c8-1a812bdf3762.png)

### 수치적 요약
![image](https://user-images.githubusercontent.com/89781598/193550916-c007de4c-95bc-4320-babf-241de05f3e31.png)

### 상관관계 분석
- 우리의 목표는 연령별, 지역별 가계자금 금액 및 금융자산에 대한 특징을 알고 싶은 것이므로, 변수들 간의 상관성을 파악하기 위해서 Scatter Plot을 사용하였다.

![image](https://user-images.githubusercontent.com/89781598/193551019-2ff3a7b2-e349-449e-9042-37d13718a94a.png)

- 이를 더 수치적으로 판단하기 위해 상관행렬을 고려하였다.

![image](https://user-images.githubusercontent.com/89781598/193551114-972823be-4b65-4460-b50a-c573a5f92023.png)

이 때, 상관행렬에서 annual pension과 savings가 강한 양의 상관관계를 보이는 것으로 보아, 두 변수는 비슷한 성질을 가지고 있다는 사실을 알 수 있다. 이는 연금과 저축이 같은 성질을 가지고 있는 변수라고도 할 수 있는데, 미래를 위해서 연금을 쌓고 저축을 한다는 것을 봤을 때, 두 변수를 같은 성질을 가진다는 것이 적절하다. 또한, tax와 utility bills와 tax와 loan사이에는 약한 상관관계가 존재하므로, 이 조합 사이에서도 어떠한 관계가 있다는 것을 유추해볼 수 있으며, livingfund는 위의 여러 조합들을 고려하여볼 때, 위의 조합을 제외하고서 큰 상관관계를 보이는 조합이 없는 것으로 판단된다. 이렇게, 변수들 간의 특성이 서로 어느 정도의 상관관계를 가지고 있으므로, PCA나 FA같은 R-technique을 사용하여 새로운 변수를 찾아서 데이터를 분석하는 것이 가능하다고 파악된다. 또한, 상관행렬에서 데이터를 보았을 때 어느 정도의 군집을 이루고 있다고 할 수 있는데, 어떻게 군집을 이루고 있는지는 모르기 때문에, 변수에 특성에 따라서 CA와 같은 Q-technique를 시도해야한다. 그렇기 때문에 우리는 PCA, FA를 모두 사용하고, 그 방법들을 비교해보면서 최적의 분석방법을 찾아야 할 것이다.

### 다변량 정규성 파악
다변량 정규성에 대한 가정은 MLFA를 수행하는데 기본적으로 필요한 가정이다. 이 데이터가 다변량 정규성을 만족하지 않는다면, PCA나 PCFA를 통하여 분석을 실시하여야 하나, 만약 다변량 정규성을 만족한다면, MLFA로도 분석을 시행해 볼 수 있을 것이다. raw data를 놓고 보았을 때 다변량 정규성을 만족하는지 확인하기 위해서 mardia test를 시행하였고, 결과에 대한 qq-plot은 그림 2.2에, 정규성 검정의 수치적인 결과는 표 2.2에 정리되어 있다.

![image](https://user-images.githubusercontent.com/89781598/193551365-9b2b3e29-439b-4f8f-9201-a5d589ecc602.png)

이 때, 왜도와 첨도에서 모두 다변량 정규성을 가지지 않고, 모든 변수에서 일변량 정규성을 가지지 않는다는 사실을 알 수 있다. 이는 다변량 정규성을 만족하지 않아 MLFA를 시행할 수 없다는 결론을 가진다. 하지만, PCA를 통하여 새로운 변수 역할을 하는 주성분이 직교성을 만족하는 고유백터 행렬에 의한 직교변환을 통하여 주성분 점수행렬을 얻고, 그에 대해서 다변량 정규성 검정을 실시하면 약간은 다른 결과를 얻을 수 있다. qq-plot은 그림 2.3에서, 정규성 검정의 수치적인 결과는 표 2.3에서 확인할 수 있다.

![image](https://user-images.githubusercontent.com/89781598/193551437-1e915264-117b-44bb-bd44-b96ca79af68a.png)

주성분 점수행렬에 대한 정규성 검정을 실시하였을 때 역시 다변량 정규성을 따르지 않는다는 사실을 알지만, 3,6번 변수에서 일변량 정규성을 따른다는 사실을 알 수 있다. 이는 상당히 큰 변화를 이루었다고 할 수 있는데, raw data에서는 0%의 변수들이 정규성을 가지고 있지 않았기 때문이다. 그러므로, MLFA도 해볼 가치가 있다고 파악된다.

![image](https://user-images.githubusercontent.com/89781598/193551507-cafc1936-4ce1-446c-a51f-12a05a36ef00.png)

# Principal Component Analysis(PCA)

### 공분산행렬과 상관행렬의 선택
PCA를 시작하기 전에, 지역 및 연령별 10대 소비(가계자금) 금액 및 금융자산 데이터가 PCA 수행과정에서 상관행렬이 적합한지, 공분산행렬이 적합한지를 알아야한다. 이 때, 각 변수들의 단위를 알고, 서로의 공분산의 크기가 크게 차이가 나지 않는다면, 공분산 행렬을 사용하고, 단위를 모르고, 서로의 공분산의 크기가 크게 차이가 난다면, 상관행렬을 사용해야 한다. 이 때, 지역 및 연령별 10대 소비(가계자금) 금액 및 금융자산 데이터는 단위는 모두 ‘원(₩)’으로 모두 같지만, 공분산의 크기가 심하게 차이 나는 것을 확인할 수 있다. 이는 표 3.1에서 확인가능하다. 즉, 상관행렬을 이용하여 PCA를 진행해야 한다는 사실을 안다.

![image](https://user-images.githubusercontent.com/89781598/193552823-6ac62d6e-8e05-4401-8225-edab92bb2462.png)

### 주성분 개수의 선택
지역 및 연령별 10대 소비(가계자금) 금액 및 금융자산 데이터는 총 6개의 변수로 이루어진 데이터이다. 이 변수들의 차원을 줄이기 위해 PCA를 실시하는데, 주성분의 개수를 구하는 방법에는 설명비율과, 이 설명비율을 시각적으로 볼 수 있는 Scree Plot이 존재한다. 설명비율에 대한 결과는 표 3.2에서, Scree Plot은 그림 3.1에서 확인할 수 있다.

![image](https://user-images.githubusercontent.com/89781598/193552901-b7689c96-10fd-417b-9c76-1ccabff45958.png)

이 때, 설명비율은 70%를 넘으면 주성분 분석을 실시할 수 있는데, 주성분 3개의 설명비율을 더하면 79.23%가 되므로, 주성분을 3개 사용하여 분석하는 것이 맞다. 또한, Scree Plot을 생각하면, 팔꿈치가 3번째 주성분에 있다는 것을 확인할 수 있으므로(2~4에서 팔꿈치가 있다는 사실을 알 수 있는데, 현재 Scree Plot에서 팔꿈치를 확인하는 것이 굉장히 까다로우므로, 설명비율을 보았을 때, 2~4번 중 3번에 팔꿈치가 있다는 것이 신빙성이 있는 주장이라고 할 수 있을 것이다.) 3개의 주성분을 사용하는 것이 최적의 방법임을 알 수 있다.

### 주성분 해석

![image](https://user-images.githubusercontent.com/89781598/193552976-ae34177c-a2aa-4984-a8be-71dd7a8d634e.png)

이 때, 첫 번째 주성분의 경우, 모든 변수에 대한 부호가 동일하므로, 이는 모든 변수에 대한 평균 금액을 나타내는 제 1 주성분이라는 것을 할 수 있다. 이 주성분의 설명력은 표 3.2.에서 확인할 수 있는데, 그 값은 38.91%이다.

두 번째 주성분의 경우 두 번째 변수에 대한 주성분 계수의 값이 유독 크다는 것을 알 수 있고, 이에 해당하는 변수는 livingfund(생활자금) 변수이다. 즉, 두 번째 주성분의 경우 생활자금에 대한 정보를 제공해준다는 것을 알 수 있다. 이 주성분의 설명력은 23.22%라는 사실을 알 수 있다.

세 번째 주성분의 경우 첫 번째 변수에 대한 주성분 계수의 값이 유독 크다는 것을 알 수 있고, 이에 해당하는 변수는 utiltiy bills(공과금) 변수이다. 즉, 세 번째 주성분의 경우 공과금에 대한 정보를 제공해준다는 것을 알 수 있고, 설명력은 17.10%라는 것을 알 수 있다.

### 주성분 점수의 산점도

우선, 첫 번째 주성분과 두 번째 주성분으로 이루어진 주성분 점수 산점도를 확인한다. 이 결과는 그림 3.2.에 있다.

![image](https://user-images.githubusercontent.com/89781598/193553088-eee95f7a-4d59-4722-bc6e-b263a6545a02.png)

첫 번째 주성분은 모든 변수들에 대한 평균금액을 나타낸다고 했다. 이는 어떤 한 관측 값의 경제활동을 영위할 수 있는 수준이라고 할 수 있다. 소비금액(가계자금)이 작고 금융자산이 없는 사람들이 오른쪽으로, 소비금액이 크고 금융자산이 많은 사람들이 왼쪽으로 분포할 것이다. 두 번째 변수는 생활 자금에 대해 설명해주는 변수라고 할 수 있는데, 위에 분포할수록 생활 자금 소비가 많은 사람이라는 것을 알 수 있고, 아래에 있을수록 생활자금 소비가 적은 사람이라고 할 수 있다. 오른쪽에는 10대가 몰려있다는 사실을 알 수 있고, 10대는 경제활동을 영위할 수 있는 수준이 낮다고 이야기 할  수 있다. 왼쪽에는 40대, 50대가 몰려있고, 40대 50대는 경제활동을 영위할 수 있는 수준이 높다는 것을 알 수 있다. 마지막으로 30대, 60대, 70대 이상이 가운데에 분포하고 있는데, 이들을 경제활동을 영위할 수 있는 수준이 모든 연령층에서 중간정도에 있다고 볼 수 있다. 즉, 고수준, 중수준 저수준으로 군집을 나눌 수 있다는 것을 확인하였다.

다음으로, 위쪽에는 10대부터 50대가 몰려있다는 사실을 확인할 수 있는데, 10대부터 50대까지는 생활자금 소비가 많다는 것을 알 수 있고, 60대, 70대는 아래쪽에 몰려있어 생활자금 수준이 낮다는 것도 파악이 가능하다.

이 때, 10대와 20대는 경제활동 영위수준이 낮으면서 생활자금 소비가 많은 연령이라고 할 수 있고, 30대부터 50대까지는 경제활동 수준이 높으면서 생활자금 소비가 많은 연령, 60대와 70대 이상은 경제활동 수준이 높으면서 생활자금 소비가 낮은 연령이라고 할 수 있다. 이 때, 아주 특징적인 값은 대전의 10대와 광주의 10대, 세종의 10대라고 할 수 있는데, 이들은 경제활동 영위 수준이 낮으면서 생활자금 소비가 낮은 것으로 보아, 다른 지역 10대보다 경제관념이 높다고 평가할 수 있다.

두 번째로, 첫 번째 주성분과 세 번째 주성분으로 이루어진 주성분 점수 산점도를 확인한다. 이 결과는 그림 3.3.에 있다.

![image](https://user-images.githubusercontent.com/89781598/193553181-b85b3662-ddb3-46f1-9831-645cd61a776c.png)

첫 번째 주성분은 모든 변수들에 대한 평균금액을 나타낸다고 했다. 이는 어떤 한 관측 값의 경제활동을 영위할 수 있는 수준이라고 할 수 있다. 소비금액(가계자금)이 작고 금융자산이 없는 사람들이 오른쪽으로, 소비금액이 크고 금융자산이 많은 사람들이 왼쪽으로 분포할 것이다. 세 번째 변수는 공과금에 대해 설명해주는 변수라고 할 수 있는데, 위에 분포할수록 공과금 소비가 많은 사람이라는 것을 알 수 있고, 아래에 있을수록 공과금 소비가 적은 사람이라고 할 수 있다.

첫 번째 주성분에 관한 오른쪽과 왼쪽의 분포는 위에서 설명했던 바와 같다.

다음으로, 위쪽에는 10대, 60대, 70대 이상이 몰려있다는 사실을 확인할 수 있는데, 10대, 60대, 70대 이상은 공과금 소비가 많다는 것을 알 수 있고, 20대부터 50대까지는 아래쪽에 몰려있어 공과금 소비가 낮다는 것도 파악이 가능하다.

정리하면, 공과금 소비가 많으면서 경제활동 영위수준이 낮은 연령대는 오른쪽 위에서 관측되는 10대들이고, 공과금소비가 적으면서 경제활동 영위수준이 낮은 연령대는 오른쪽 아래에 있는 20대들이며, 경제활동 수준이 높으면서 공과금 소비가 많은 연령은 왼쪽 위에서 관측되는 50대부터 70대들이며, 경제활동 수준이 높으면서 공과금 소비가 낮은 연령은 30대와 40대라고 할 수 있다. 이 때, 마찬가지로 특징적인 관측치를 관측할 수 있는데, 공과금소비가 적으면서 경제영위활동 수준이 낮은 전남의 10대와 세종의 10대이다. 이들은 다른 10대들과는 공과금 소비량이 다르다는 것을 알 수 있다.

마지막으로, 두 번째 주성분과 세 번째 주성분으로 이루어진 주성분 점수 산점도를 확인한다. 이 결과는 그림 3.4.에 있다.

![image](https://user-images.githubusercontent.com/89781598/193553267-8f256b32-7b53-4af7-8f35-75a1ecc2a3a9.png)

두 번째 주성분에 따라 분류하면, 생활자금에 대한 소비에 따라서, 오른쪽에 있는 사람들이 생활자금 소비가 많고, 왼쪽에 있는 사람들이 생활자금 소비가 낮다고 할 수 있다. 또한 위쪽에 있는 사람이 공과금 소비가 크고, 아래쪽에 있는 사람이 공과금 소비가 낮다고 할 수 있다.

이 때, 생활자금에 대한 소비가 크고 공과금에 대한 소비도 큰 연령은 오른쪽 위에서 관측 가능한 10대, 40대 이고, 생활자금에 대한 소비가 크면서 공과금에 대한 소비가 낮은 연령은 오른쪽 아래에서 관측 가능한 30대와 40대이며, 생활자금에 대한 소비가 작으면서 공과금에 대한 소비가 큰 연령은 60대와 70대이다. 이 때, 50대는 가운데에 분포하고 있어, 어느 곳에 속해있다고 확인할 수 없다.

### 주성분 행렬도

이 때, 확실한 분석을 위해서 주성분 행렬도를 만들면, 그림 3.5와 같은 결과가 나오는 것을 확인할 수 있다.

![image](https://user-images.githubusercontent.com/89781598/193553361-b009587e-da42-4681-a73d-95c1bfe2b4b9.png)

주성분 행렬도의 화살표를 먼저 보면, 화살표의 각도가 가까운 변수들은 서로 관련이 큰 변수이고, 화실표의 각도가 먼 변수들은 서로 관련이 없는 변수이다. 이 때, (연금 평균 소비금액과 총 저축 평균 금액), (공과금 평균 소비액, 총 대출 평균 금액, 세금 평균 금액)은 서로 관련이 큰 변수이고, 생활자금 평균금액은 다른 어떤 것과도 관련이 크지 않은 변수라는 사실을 알 수 있다.

이 때, 화살표의 방향에 따라서, 아래로 갈수록 생활자금 평균 소비금액이 크고, 왼쪽 아래로 갈수록 공과금 평균 소비액, 세금평균금액 (우리는 이를 세금과 공과라고 표현한다)과 총 대출 평균금액이 크며, 왼쪽 위로 갈수록 연금 평균 소비금액과 총 저축 평균 잔액이 크다는 사실을 알 수 있다.

또한, 첫 번째 주성분은 경제활동 영위 수준이 고수준인지, 저수준인지를 나타내는 주성분이고, 두 번째 주성분의 경우 생활자금 소비와 관련된 주성분으로, 아래로 갈수록 생활자금 소비가 크다는 사실을 알 수 있다.

이를 기준으로 관측치를 파악하면, 60대, 70대들은 생활자금 소비가 작으면서, 연금 평균 소비금액과 총 저축 평균 잔액이 높다는 것을 알 수 있다. 즉 미래를 위해서 기본적인 생활 소비를 줄이고 연금에 지속적으로 투자를 하며, 미래를 위해서 저축을 많이 해놓는다는 것을 알 수 있다. 50대, 40대들은 공과금 평균 소비금액과 세금 평균 소비금액, 총 대출 평균 금액이 크다는 것을 알 수 있으며, 이는 대출을 받아서 부동산과 같은 자산을 매입하는 가장 활발한 시기라는 것을 암시하게 해준다. 10대부터 20대까지는 생활자금 평균 소비금액이 크다는 사실을 알 수 있다. 이 때, 10대부터 20대까지는 생활자금 소비를 많이하고, 30대부터 50대는 대출을 많이 하면서 지출하는 세금과 공과가 크다는 것을 알 수 있으며, 60대와 70대는 총 저축 평균잔액과 연금 평균 소비금액이 크다는 것도 알 수 있다. 이 때, 예외로 존재하는 세종 10대, 광주 10대, 대전 10대는 생활자금 소비가 크지 않으면서 경제활동 영위수준이 낮다.

# Factor Analysis(FA)

### 공동 인자 개수 구하기

앞에서(1. Data Descriptionⅰ.전반적인 데이터 설명) 우리는 분석을 행할 때 상관행렬을 사용해야한다는 사실을 알 수 있었고, 그러므로, 상관행렬을 스펙트럴 분해하여 설명비율을 구해준 후, 공동 인자의 개수를 확인한다. 그에 대한 수치적 결과는 설명비율로, 시각적인 결과는 Scree Plot을 통해서 확인가능하다.

![image](https://user-images.githubusercontent.com/89781598/193553575-fbdfcc94-0beb-4ccd-984b-53bd0a5a32a8.png)

이 때, 앞에서의 (3. Principal Component Analysis(PCA)ⅱ. 주성분 개수의 선택) 의 결과와 같다는 것을 알 수 있는데, 앞에서의 분석과 지금의 분석에서 모두 스펙트럴 분해를 이용하여 분석하였기 때문이다. 세 번째 공통인자까지의 설명비율이 79.23%이고, Scree Plot의 팔꿈치가 3번에서 이루어지는 것을 확인할 수 있으므로, 3개의 공동인자를 사용하여 FA를 수행한다.

### PCFA

PCFA를 수행하여 인자 적재 그림을 통해 변수들의 특성을 파악해본 후, 인자 점수 그림으로 관측치를 분석하는 절차가 이루어져야 하므로, 우선 인자 적재그림을 통해 변수들의 특성을 파악해본다. 1번째 공통인자와 두 번째 공통인자, 1번째 공통인자와 3번째 공통인자, 2번째 공통인자와 3번째 공통인자에 대한 인자적재그림은 그림 3.2.에서 확인 가능하다.

![image](https://user-images.githubusercontent.com/89781598/193553681-703e41cf-f8f7-4ce6-80b5-ae66c5a9cabb.png)

또한 이에 대한 인자 점수 그림은 그림 4.3에서 확인가능하다.

![image](https://user-images.githubusercontent.com/89781598/193553735-9bf3a81f-22cf-40ff-8b39-54d457739570.png)

이 때, 1번, 2번 공동인자의 인자적재그림에서는 오른쪽 위로 갈수록 세금과 공과, 대출의 소비금액이 크고, 오른쪽 아래로 갈수록 저축액과 연금에 대한 소비가 크다는 것을 알 수 있다. 위쪽으로 갈수록 생활자금 소비가 크다. 인자 점수 그림을 보면, 세금과 공과, 대출의 소비금액이 크면서 생활자금 소비가 큰 연령층은 30대와 40대로 집계되며, 생활자금이 작으면서 연금소비와 저축액이 큰 연령층은 50대부터 70대, 생활자금의 소비가 크며 연금소비와 저축이 적으며 세금과 공과, 대출에 대한 소비가 적은 연령은 10대와 20대로 해석된다.

1번, 3번 공동인자의 인자적재그림에서는 위로 갈수록 공과에 대한 소비가 크고, 오른쪽으로 갈수록 세금에 대한 소비와 저축액이 크며, 연금소비와 대출액이 큰 집단이 모여 있을 것이다. 인자점수그림을 보면, 공과에 대한 소비가 큰 연령대는 10대와 40대부터 70대로 나타나며, 세금에 대한 소비, 저축액, 연금소비, 대출액이 큰 집단은 30대부터 70대 까지인 것으로 나타난다. 20대는 공과에 대한 소비가 작고, 세금, 연금소비가 적으며, 저축액, 대출액도 적은 것으로 나타난다.

2번, 3번 공동인자의 인자적재그림에서는 위쪽으로 갈수록 공과에 대한 소비가 크고, 오른쪽으로 갈수록 생활자금과 세금에 대한 소비와 대출액이 크며, 왼쪽으로 갈수록 연금소비와 저축액이 큰 것으로 나타난다. 이 때, 인자 적재 그림을 확인하면, 공과에 대한 소비가 크면서 생활자금에 대한 소비가 큰 집단은 10대, 연금소비와 저축액이 큰 집단은 60대와 70대, 세금에 대한 소비와 대출액이 큰 집단은 30대와 40대가 특징적으로 나타난다.

모든 것을 종합하여 해석하면, 10대는 공과에 대한 소비가 크면서 생활자금에 대한 소비가 큰 집단이라는 것을 알 수 있다. 20대는 공과에 대한 소비가 적지만, 생활자금에 대한 소비는 크다. 즉, 공과가 10대와 20대의 차이점을 확연하게 드러내준다고 이야기할  수 있다. 30대와 40대는 생활자금 소비가 크면서 대출액이 큰 집단으로 파악된다. 50대부터 70대까지는 생활자금에 대한 소비가 적고 연금소비와 저축액이 큰 연령층으로 파악된다.

이 때 이런 모든 특징을 합한 그래프를 그려보기 위해서 특이값 분해를 통한 인자행렬도를 그리면 그 결과는 아래와 같다. 또한 더 정확한 분석을 위해서 Varimax인자회전을 진행시켰다. 이는 그림 4.4.에서 확인가능하다.

![image](https://user-images.githubusercontent.com/89781598/193553822-2ffb6eca-aa26-4c50-a235-586fba3d86e5.png)

이 때, 인자들은 연금소비와 저축액이 하나의 인자로, 세금과 공과, 대출액이 하나의 인자로, 생활자금 소비가 하나로 묶이는 것을 알 수 있고, 위에서 분석했던 것과 마찬가지로, 10대와 20대는 생활자금을 많이 소비하는 세대로, 30대와 40대는 생활자금을 많이 소비하고, 세금과 공과액이 많으며, 대출을 많이 하는 세대로, 50대부터 70대까지는 저축액이 많고 연금에 대한 소비가 많은 세대로 나타난다.

### MLFA

MLFA를 시행하기 위해서는 다변량 정규성이 먼저 만족되어야 한다. 하지만, 위에서 보았듯이, raw data는 다변량 정규성을 만족하지 못한다는 사실을 안다. 하지만, PCA를 통하여 주성분 점수행렬 데이터에 대한 정규성 검정을 실시하였을 때 역시 다변량 정규성을 따르지 않는다는 사실을 알지만, 3번, 5번 변수에서 일변량 정규성을 따른다는 사실을 알 수 있기에, MLFA를 시행해볼 가치는 있다고 앞에서 이야기 하였다. (2. Data Description,ⅰ. 다변량 정규성 파악) 이에 의거하여 차원 축소된 행렬에 대해 MLFA를 진행한다.

MLFA를 진행하여 총 기여율을 확인하면, 그 결과는 표 4.2와 같다.

![image](https://user-images.githubusercontent.com/89781598/193553916-cd71516c-2434-41ef-ba83-a3f2e811f7b7.png)

MLFA에 대한 가능성을 확인하기 위해 총 기여율을 확인해보았지만, 총 기여율이 6.6%밖에 되지 않는다는 사실을 알 수 있다. 적어도 70%를 넘어야 분석이 가능한데, 이는 거의 분석을 할 수 없는 수준이라는 것을 알 수 있다. PCFA와 비교한 총 기여율은 표 4.3에서 확인할 수 있다.

![image](https://user-images.githubusercontent.com/89781598/193553982-ae1dac66-9984-4df3-8d0f-6fe053f6f93f.png)

그러므로, 총 기여율의 관점에서 PCFA가 월등히 우월하다는 것을 안다. 그 후, 잔차행렬을 비교하여, 어느 분석이 더 좋은 분석인지 확인해야한다. PCFA와 MLFA의 잔차행렬을 비교한 것은 표 4.4에서 확인 할 수 있다.

![image](https://user-images.githubusercontent.com/89781598/193554041-ad213693-5265-4d8d-8f88-00954694d310.png)

잔차행렬의 원소들이 0으로 수렴해야 성능이 좋다고 판단할 수 있다. 이 때, PCFA의 대부분의 원소들은 0에 수렴하지만, MLFA의 원소들은 0에 수렴하는 원소들이 거의 없고, 심지에 거의 1에 가까운 값들을 가지고 있는 원소들도 많다. 잔차행렬을 보았을 때 PCFA의 성능은 MLFA의 성능보다 월등히 뛰어나다는 것을 알 수 있고, MLFA의 성능은 전혀 뛰어나지 않음을 사시하고 있다.

그러므로, MLFA를 시행할 이유가 전혀 없다는 것을 알 수 있다. 이는 설명력이 6.6%밖에 되지 않을뿐더러, 잔차행렬 또한 1에 매우 가깝기 때문에 성능이 아주 나쁘기 때문이다.

# Cluster Analysis(CA)

### 계층적 군집분석
군집분석에는 여러 가지 연결법이 존재하는데, 이를 크게 두 부류로 나누면 계층적 연결법과 비 계층적 연결법이다. 이 때 모든 연결법은 표준화 유클리드 거리와 마할라노비스 거리를 이용하였다. 계층적 연결법은 단일연결법, 완전연결법, 평균연결법, 와드연결법이 존재한다. 이 때, 가장 대중화된 단일연결법과 와드연결법을 사용하기로 한다. 비 계층적 연결법에는 K-평균법과 K-중위수법이 존재하는데, 여기에서, 가장 많이 사용하는 K-연결법을 사용하기로 한다. 단일 연결법을 시행한 결과는 그림 5.1에 있다.

![image](https://user-images.githubusercontent.com/89781598/193554327-acc1a22e-b76d-4523-bc5d-dadec2b7a929.png)

단일 연결법을 사용할 때, 60대와 70대는 각각의 군집을 형성하는것을 알 수 있고, 30대와 40대는 같은 하나의 군집을 형성하며, 50대 또한 하나의 군집을, 20대와 10대도 하나의 군집을 형성하는 것을 알 수 있다. 하지만, 그 외에 관측치들이 난잡하게 섞여있는 것을 봐서, 군집이 확실하게 이루어 지지는 않았다고 할 수 있다.

반면, 와드 연결법을 사용하면, 10대와 20대가 하나의 군집으로 묶이는 것을 확인할 수 있으며, 60대와 70대가 하나의 군집으로, 40대부터 50대까지가 또 하나의 군집으로 묶이고, 30~50대까지, 20대~40대까지 각각 한 군집으로 묶인다.  하지만, 지역에 따라 사뭇 다른 관측치들이 관찰되며, 연령별로 나누어 지는 것 뿐만 아니라, 지역에 따라서도 군집이 나누어지고 있다는 것을 알 수 있다.

다음으로, 마할라노비스 거리를 이용하여 단일 연결법, 와드 연결법을 실시하였고, 그에 대한 결과는 그림 5.2.에 제시되어 있다.

![image](https://user-images.githubusercontent.com/89781598/193554406-ede8f416-9560-4bf9-a779-43283f10d420.png)

단일 연걸법에서는 유클리드 거리를 사용한 단일연결법과 마찬가지로 군집형성이 잘 안되어있다는 것을 확인할 수 있는데, 60대와 70대는 각각의 군집을 형성하는 것을 알 수 있고, 20대 또한 하나의 군집을 형성하는 것을 알 수 있다. 하지만, 10대의 군집에서 다른 연령층이 난잡하게 섞여있고, 30, 40, 50대 또한 난잡하게 섞여있는 것을 알 수 있다. 단일연결법에서는 군집분석이 제대로 이루어지지 않았다는 것을 확인할 수 있다.

와드 연결법을 사용하였을때는 10대와 20대가 한 군집을, 60대와 70대가 한 군집을, 40대, 50대가 한 군집을 이루고 30, 40, 50대가 한 군집을, 마지막으로 20대, 30대, 40대가 한 군집을 이루는 것도 맨 오른쪽에서 확인할 수 있는데, 이는 어느 지역의 20대는 30대, 40대와 비슷한 특성을 갖는다는 것을 짐작해볼 수 있다.

결국 단일연결법으로는 군집을 나누는 것이 불가능에 가깝고, 와드 연결법을 이용해서 군집을 나누는 것이 효과적이라는 사실을 파악하였다. 그렇기 때문에, 와드 연결법에 대한 결과를 나타내면 표 5.1과 같을 것이다.

![image](https://user-images.githubusercontent.com/89781598/193554472-f13309fc-d663-487a-a29c-5204b358bb65.png)

결국 군집은 5개 정도로 나누는 것이 적절하다고 판단된다.

하지만, 군집수를 추정하기 위한 더 좋은 방법은, ccc와 Dindex등을 이용하는 것이다. 이에 대한 결과는 그림 5.3에 있다. 비록 표준화 유클리드 거리와 마할라노비스 거리를 이용하여 군집을 5개정도 형성할 수 있을 것이라고 유추해볼 수 있지만, 더 확실하게 파악하기 위해서는 여러 측도들을 사용해야 한다.

![image](https://user-images.githubusercontent.com/89781598/193554530-55b9b6d0-a9be-49be-aaf8-66b07019fab1.png)

우선 ccc를 이용하여 군집의 수를 추정할 때는 ccc값이 가장 큰 경우의 군집수를 사용하면 되는데, 이 때, ccc가 가장 큰 값은 군집의 수가 7개 일 때이다. Dindex 그래프의 경우 기울기가 급격히 감소하는 구간(빨간 선 그래프), 기울기가 급격히 증가하는 구간(파란 선 그래프)로 군집의 수를 선택하는데, 이 때, 급격히 감소하는 구간은 군집의 수가 6인 구간이라고 할 수 있다. 즉, ccc와 Dindex에서는 군집이 6~7개일 때 가장 좋은 분석이 된다고 판단하고 있다.

이 때, 모든 측도들을 사용하여 평가한 결과는 아래의 표 5.2에서 확인할 수 있다.

![image](https://user-images.githubusercontent.com/89781598/193554584-10cf1195-2855-4313-9eaa-8dc9b808583f.png)

이 때, 8개의 측도에서 2개의 군집으로 나누는 것이 가장 좋다고 이야기하였으며, 그 다음은 7개의 군집으로 나누는 것이 가장 좋은 분석이라고 평가하고 있다. 와드 연결법에서 군집을 5개로 나누었을 때 명확하게 나누어지지 않았던 관측치들이 7개의 군집으로 나누면 더 명확히 구분된다는 것을 이야기해주고 있다.

### 비계층적 군집분석

계층적 연결법에서, 군집을 7개로 나누는 것이 가장 효과적이라고 판단하였다. (물론, 2개의 군집으로 나눈 것이 가장 효율적이라는 결과를 얻었지만, 2개로 나누었을 때 관측치들의 특성이 전혀 나타나지 않으므로, 2번째로 효율이 좋은 7개의 군집으로 나누는 방법을 사용한다.) 이 때, K-평균법을 사용하여 군집을 나누어주면 표 5.3과 같은 결과를 얻는다.

![image](https://user-images.githubusercontent.com/89781598/193554679-5c1dd9a8-56e7-41cb-964f-be36f5d1e95e.png)

또한, 각 군집에 대한 특성을 나타낸 표는 표 5.4에서 확인할 수 있다.

![image](https://user-images.githubusercontent.com/89781598/193554755-341d2539-dcf7-4218-9d29-850dc34f8336.png)

# 결론

이러한 분석이 우리에게 시사하는 바는 무엇인가. 우선 각 군집에 대한 특성을 파악하는 것이 우선일 것이다. 우선 10대들이 분포한 그룹을 파악하여 보자. 10대 광주, 10대 대전, 10대 세종은 그룹 1에, 10대 경남, 10대 대구, 10대 인천, 10대 전남은 그룹 2에, 10대 제주는 그룹 3에 10대 강원, 10대 경기, 10대 경북, 10대 부산, 10대 서울, 10대 울산, 10대 전북, 10대 충남, 10대 충북은 그룹 5에 위치해있고, 이를 쉽게 나타내기 위해서 표 6.1.에 정리하였다.

![image](https://user-images.githubusercontent.com/89781598/193554918-c698d7f7-fd09-4a7b-b10a-39e6912e536c.png)

그룹 1과 그룹 5에는 10대들만 위치해 있고, 그룹 2에는 20대들이 주를 이루며, 그룹 3에는 60대와 70대가 주를 이룬다는 것을 알 수 있다. 그룹 1은 다른 그룹들보다 생활자금 소비액이 적고, 그룹 5에 비해서 저축하는 돈이 많으며, 연금소비액이 크다. 이 말의 의미는, 광주, 대전, 세종에 사는 10대들은 다른 10대들보다 생활자금을 적게 쓰면서 절약을 하고, 미래를 위해서 저축하고, 심지어 연금(보통 부모들이 자녀들을 위해 가입하는 경우가 많을 것이다.)까지 고려하는 것을 알 수 있다. 즉, 다른 10대들보다 광주, 대전, 세종에 사는 10대들이 절약을 하며 미래를 위한 대비를 철저하게 한다고 할 수 있다.

그룹 2는 20대가 대부분을 차지하고, 그들의 특성은 10대들보다 생활자금 소비액이 많으며, 연금이나 저축을 거의 하지 않는다는 것이 특징이다. 다른 10대들보다 저축을 많이 하지만, (빠르면 20대 초반, 느리면 20대 중, 후반에 직장인이 되는 경우가 많으므로) 생활자금 소비액이 많고 연금에 대한 대비를 안하는 것으로 볼 때, 상대적으로 미래에 대한 대비를 하지 않으면서 현재 지출이 큰 10대들이라고 할 수 있다. 즉, 경남과 대구, 인천과 전남에 사는 10대들은 미래에 대한 대비를 거의 하지 않으며, 현재에 돈을 많이 쓰는 사람들이라고 할 수 있을 것이다.

그룹 3는 주로 노년층이 주를 이루고 있으며, 연금과 저축에 대한 대비가 투철하고, 그룹 1보다는 생활자금 소비액이 많지만, 그룹 5에 비해서 생활자금 소비액이 적은 것이 특징이다. 이는 제주도의 특수성 때문에 일어나는 일이다. 제주도는 노년 인구가 많기 때문에, 10대들의 소비패턴에도 노인들의 영향을 크게 받을 것이고, 그 결과 소비는 노년인구만큼 하면서 미래를 위한 준비를 투철하게 한다고 할 수 있다. 그룹 1에 있는 광주, 대전, 세종에 있는 10대들보다 더 많은 소비를 하지만, 노년 인구의 영향을 받아서 미래도 많이 준비한다는 사실도 안다.

마지막으로 그룹 5는 대부분의 10대들의 특징을 나타낸다고 할 수 있는데, 저축과 연금에 대해 전혀 신경 쓰지 않으며, 생활자금 소비도 많은 것이 특징이다.

이를 정리한 것이 표 6.1.1에 존재한다.

![image](https://user-images.githubusercontent.com/89781598/193554967-1a2d6562-0c1f-4938-805b-a6133a4e967d.png)

20대들은 그룹 2에 모두 존재하며, 이들의 특징은 소비를 많이 하고, 다른 30대부터 70대의 연령층보다 미래에 대한 대비를 안 하는 것이 특징이다. 이는 모든 지역에서 공통적으로 나타나는 특성으로, 20대들은 소비를 중시하고, 미래에 대한 준비는 별로 하지 않는 것이 특징이다.

30대의 경우, 그룹 2, 그룹 4, 그룹 6, 그룹 7에 존재하며, 이를 정리한 것이 표 6.2.와 같다.

![image](https://user-images.githubusercontent.com/89781598/193555038-ffc545df-50fd-4cbd-91c1-24f74ad7935f.png)

이 때, 그룹 2는 20대들이 주를 이루는 것이 특징이다. 30대 임에도 불구하고, 아직까지 소비를 중시하는 지역은 경남, 대전, 울산이라는 것을 알 수 있다. 그들은 30대가 들어섰지만, 아직까지 20대의 소비력을 가지고 있다. 그러므로 다른 30대의 사람들보다 저축을 하는 사람들이 매우 적다.

그룹 4같은 경우 현재 우리나라의 부동산 문제를 여실히 보여주고 있다는 것이 느껴진다. 그룹 4의 특징은 바로 대출소비액이 다른 어떤 그룹보다 최대 6배, 최소 2배정도 크다는 것이다. 서울의 부동산 문제로 인해 대출을 받는 사람들이 많고, 그러한 대출을 많이 받는 사람들이 서울에 몰려있다는 것을 알 수 있다. 또한 세금 소비액도 굉장히 높다는 것을 확인할 수 있는데, 이는 각종 부동산에 따른 세금일수도 있고, 수입에 대한 세금일 수도 있다. 즉, 서울의 30대는 30대 중 돈을 가장 많이 벌며, 대출도 많이 한다는 것을 알 수 있다. 그로 인해서 미래에 대한 대비가 투철하고, 30대 중 저축을 가장 많이 한다는 것을 알 수 있다.

그룹 6는 모든 변수들의 값이 다른 그룹과 비교했을 때 평이하다는 사실을 알 수 있다. 즉, 기본적인 30대의 특징이라는 것을 알 수 있다. 다만, 대출이 소비를 중시하는 그룹 2보다는 많지만, 다른 그룹들에 비해서는 한없이 적다는 것을 보여주고 있고, 이는 그룹 6에 있는 사람들이 대출을 하는 일이 거의 없다는 것을 설명해준다. 그룹 6에 속하는 지역은 강원, 경북, 전남, 전북, 충북지역으로 부동산 값이 낮은 광역시가 아닌 지역이다. 즉, 지역의 특수성이 그들의 대출사용 저하를 불러일으킨 것이다.

이에 반해 그룹 7은 서울에 이어서 2번째로 대출에 돈을 많이 쓰는 지역으로, 여기 있는 지역들은 충남을 제외한 모든 곳이 광역시임을 알 수 있다. 그룹 6와는 다르게, 광역시는 부동산 값이 광역시가 아닌 지역보다 높으므로, 대출을 더 많이 사용하는 것을 알 수 있다. 이러한 특성을 정리한 표는 표 6.2.1에 있다.

![image](https://user-images.githubusercontent.com/89781598/193555105-1eed8ca2-dc34-458c-9886-baa2124a74d9.png)

40대들은 그룹 4, 그룹 6, 그룹 7에 위치하며, 이를 명료하게 표로 정리한 것이 표 6.3이다.

![image](https://user-images.githubusercontent.com/89781598/193555152-df5df05c-24fb-4310-9a65-501a16a315cc.png)

40대에 20대처럼 행동하는 사람들은 없다는 것이 표 6.2.가 보여주고 있다. 30대 때 20대처럼 행동하던 경남, 대전, 울산의 사람들이 경남은 그룹 6에, 대전과 울산은 그룹 7에 편입된 모습을 보여준다. 이는 그들이 지방에 따른 특수성을 제외하면, 그 연령에 맞는 가계자금 소비를 하고 금융자산을 가지고 있다는 표현과 일치한다. 여기에서 특별한 관측치는 세종에 사는 40대인데, 세종에 사는 40대는 서울에 사는 40대처럼 행동하는 것을 볼 수 있다. 즉, 대출을 많이 하고 그에 따른 미래에 대한 준비도 많이 한다는 것인데, 이는 40대들이 서울에서 생활하는 것과 세종에서 생활하는 것이 체감상 비슷하다는 것을 유추해볼 수 있는 부분이다. 이를 잘 정리한 것은 표 6.3.1에 존재한다.

![image](https://user-images.githubusercontent.com/89781598/193555198-5ebcfd51-fa6f-40c9-af16-fb3ebde9ffad.png)

30대, 40대에서 이상하게 느끼는 점은 바로 광주광역시와 제주도 지역이다. 광주광역시는 광역시가 아닌 지방이 아닌데 광역시가 아닌 지방의 그룹에 속하여있고, 제주도는 광역시가 아닌데 광역시의 그룹에 속해있다. 이는 광주광역시가 광역시 정도의 생활수준을 요구하지 않고, 광역시가 아닌 지방정도의 생활수준을 요구한다는 것을 알 수 있으며, 반면 제주도는 광역시가 아닌 지역임에도 불구하고 광역시 정도의 생활수준을 가지고 있어야 된다는 것을 시사한다.

정리하면, 40대는 자신의 지역에 맞는 가계자금 소비와 금융자산을 보유하고 있고, 경제적 안정기에 접어들었음을 시사한다.

50대는 그룹 4, 그룹 6, 그룹 7에 속해있다. 이를 간단히 표로 나타낸 것이 표 6.4에 존재한다.

![image](https://user-images.githubusercontent.com/89781598/193555254-4b2efc72-2915-4def-a691-0e8065bbeaf8.png)

이 때, 광역시가 아닌 지역임에도 불구하고 광역시 정도의 생활수준을 가지고 있어야 되는 제주는 50대의 연령층에 이르러 서울 수준의 생활수준을 가지고 있어야 살 수 있는 곳이 되었다는 것을 알 수 있다. 즉, 제주도에 사는 사람들은 연령이 증가함에 따라 계속해서 대출이 증가한다는 것을 알 수 있고, 그에 따른 저축도 지속해서 늘어난다는 사실을 알 수 있다.

이와 반면에, 경기, 울산같은 경우, 오히려 대출이 줄어들고 안정되었다는 사실을 알 수 있다. 즉, 경기와 울산은 연령이 높아지고, 자산이 축적되면 오히려 안정적인 생활이 가능하다고 유추할 수 있다. 이에 대해 정리한 것은 표 6.4.1에 있다.

![image](https://user-images.githubusercontent.com/89781598/193555312-abdbc9f6-b545-47f3-a15f-3f9b7d794afc.png)

60대는 그룹 3과 그룹 7에 존재하는데, 이를 표로 명료하게 나타낸 것이 표 6.5에 존재한다.

![image](https://user-images.githubusercontent.com/89781598/193555430-9ec4ffae-d061-415a-a018-59f1b67fbf2f.png)

이 때, 그룹 3은 대출이 가장 낮은 그룹이고, 그룹 7은 대출이 가장 높은 지역임을 표 5.4에서 알 수 있다. 이는 세종과 제주를 제외한 모든 60대들은 대출을 갚아나가는 안정기에 접어든 반면, 세종과 제주는 아직도 대출액이 많다는 것을 알 수 있다. 이는 서울에 사는 사람들은 수입이 많아서 대출액을 빨리 갚아 나갈 수 있었던 반면, 제주와 세종은 그 정도의 수입은 미치지 못하여 대출액을 갚아나가지 못하고 있는 상황을 의미한다.

70대는 저축액이 가장 많고 연금에 대한 소비가 가장 많은 그룹 3에 위치하여, 노년을 준비하는 모습을 여실히 보여준다.

이러한 분석은 분명 2020년 12월 말에 연령별, 지역별로 조사한 자료이지만, 어느 정도의 지역에 따른 가계소비와 금융자산에 대한 사이클이 존재한다는 것을 시사해준다. 또한, 광역시 인지 아닌지, 서울인지 아닌지에 따라서 그들의 가계소비와 금융자산에 대한 특징이 서로 다르고, 그들의 연령에 따라서도 선명한 특징을 보인다는 것을 알 수 있다.
