# EDA

### Data
“[2021금융데이터경진대회] 동학개미운동 신규 고객 투자 정보“ 데이터는 한국투자증권에서 제공한 데이터 자료로써, 연령별로는 20대, 30대, 40대, 50대로 나누고, 대한민국의 지역을 총 16개로 분류 (강원, 경기, 경남, 경북, 광주, 대구, 대전, 부산, 서울, 울산, 인천, 전남, 전북, 제주, 충남, 충북, 총 8도, 1 특별자치도, 1특별광역시, 6광역시)하여, 2020년 동학개미운동이 일어난 시점을 기준으로 고객 구분 코드, 고객 성별 구분 코드, 동일 나이군 구분 코드, 주소(시,도), 주문일자, 주문시간대, 상품번호, 상품명, 거래소구분코드, 매도매수구분코드, 주문 구분코드. 실 주문단가, 주문수량, 총 체결수량, 총 체결금액, 전일종가, 당일시가, 일중고가, 일중저가, 당일종가, 거래수량, 거래대금, 상한가 여부, 하한가 여부를 조사한 데이터이다.
이 때, 분석을 위해 기초적으로 데이터를 변환시켰다.

### Tidy Data
전체적인 모든 것을 조사할 수 없으므로, 데이터를 필요한 부분만 간소화 시켰다. 우리는 앞의 데이터인 지역 및 연령별 10대소비(가계자금) 금액 및 금융자산 데이터에 관심이 있으므로 이 것과 관계있는 변수들만 남기기로 하고, 나머지 변수들은 제거하기로 하였다. 즉, 고객 구분 코드, 고객 성별 구분 코드, 주문일자, 주문시간대, 상품번호, 상품명, 거래소구분코드, 매도매수구분코드, 주문 구분코드, 상한가 여부, 하한가 여부를 제거하여 데이터를 간소화시켰다. 그 후, 845941개의 데이터를 요약하기 위해서 중앙값을 사용하여 정제하였다. 평균을 쓰지 않고 중앙값을 사용한 이유는, 데이터의 표준편차가 매우 크기 때문에 평균으로는 데이터의 집단적 특성을 드러내기 어렵기 때문이다.

### 관측치
이 때, 연령대와 시도명의 경우, 데이터분석을 용이하게 하기 위해서 1~119까지의 숫자로 라벨링하였다. 그 내역은 아래와 같다.

![image](https://user-images.githubusercontent.com/89781598/193555931-de84cc02-4d86-447c-af69-daf89db1bbd3.png)

즉, 백의 자리 수는 연령대를 나타내고, 십의자리, 일의자리는 시도를 나타낸다고 할 수 있다.

### 변수
변수에 대한 설명은 아래와 같다.

![image](https://user-images.githubusercontent.com/89781598/193556026-ee06cabd-dfdd-4c07-afe8-f065c0f4c0fe.png)

### 수치적 요약
수치적으로 요약하면 아래와 같다.

![image](https://user-images.githubusercontent.com/89781598/193556091-f7d22ee8-7919-4603-9084-9ae4f50075ea.png)

### 상관관계 분석
우리의 목표는 연령별, 지역별 투자의 성향에 대해 알고 싶은 것이므로, 변수들 간의 상관성을 파악하기 위해서 Scatter Plot을 사용할 것이다. 이 결과는 그림 6.2.에 나타나있다.

![image](https://user-images.githubusercontent.com/89781598/193556239-801301c3-5e5e-473c-8d44-21df8acab557.png)

또한, 이를 더 수치적으로 파악하기 위해서 상관행렬을 고려해볼 수도 있다. 이 결과는 표 7.1에서 볼 수 있다.

![image](https://user-images.githubusercontent.com/89781598/193556311-fba8c59c-6661-425d-999c-8157e79042a0.png)

이 때, 상관행렬에서 실주문단가와 전일종가, 당일시가 일중고가, 일중저가 당일종가는 강한 양의 상관관계를 보이고, 거래수량과는 강한 음의 상관관계를 보여준다고 할 수 있다. 또한, 주문수량과 총 체결수량, 총 체결금액은 강한 양의 상관관계를 보여주고, 거래수량과는 약한 양의 상관관계를 보여주는 것을 파악할 수 있다. 이렇게, 변수들 간의 특성이 서로 어느 정도의 상관관계를 가지고 있으므로, PCA나 FA같은 R-technique을 사용하여 새로운 변수를 찾아서 데이터를 분석하는 것이 가능하다고 파악된다. 또한, 상관행렬에서 데이터를 보았을 때 어느 정도의 군집을 이루고 있다고 할 수 있는데, 어떻게 군집을 이루고 있는지는 모르기 때문에, 변수에 특성에 따라서 CA와 같은 Q-technique를 시도해야한다. 그렇기 때문에 우리는 PCA, FA를 모두 사용하고, 그 방법들을 비교해보면서 최적의 분석방법을 찾아야 할 것이다.

### 다변량 정규성 파악
다변량 정규성에 대한 가정은 MLFA를 수행하는데 기본적으로 필요한 가정이다. 이 데이터가 다변량 정규성을 만족하지 않는다면, PCA나 PCFA를 통하여 분석을 실시하여야 하나, 만약 다변량 정규성을 만족한다면, MLFA로도 분석을 시행해 볼 수 있을 것이다. 이때, raw data를 놓고 보았을 때 굉장히 큰 편차를 보여주고, 변수간의 편차가 매우 심하기 때문에, 데이터를 log변환 하였다. 그 후, 다변량 정규성을 만족하는지 확인하기 위해서 mardia test를 시행하였고, 결과에 대한 qq-plot은 그림 2.2에, 정규성 검정의 수치적인 결과는 표 2.2에 정리되어 있다.

![image](https://user-images.githubusercontent.com/89781598/193556412-3b46790f-7d6b-4fc9-9842-c0b3e239e66d.png)

이 때, 왜도와 첨도에서 모두 다변량 정규성을 가지지 않고, 11개의 변수 중 5개의 변수에서 일변량 정규성을 가진다는 사실을 알 수 있다. 이는 원론적으로는 다변량 정규성을 만족하지 않아 MLFA를 시행할 수 없다는 결론을 가진다.

![image](https://user-images.githubusercontent.com/89781598/193556456-08222ec1-b903-49a7-9e06-414d78b1850f.png)

# Principal Component Analysis(PCA)

### 공분산행렬과 상관행렬의 선택
PCA를 시작하기 전에, 데이터가 PCA 수행과정에서 상관행렬이 적합한지, 공분산행렬이 적합한지를 알아야한다. 이 때, 각 변수들의 단위를 알고, 서로의 공분산의 크기가 크게 차이가 나지 않는다면, 공분산 행렬을 사용하고, 단위를 모르고, 서로의 공분산의 크기가 크게 차이가 난다면, 상관행렬을 사용해야 한다. 이 때, “[2021금융데이터경진대회] 동학개미운동 신규 고객 투자 정보“ 데이터는 로그변환을 해 줌으로써 공분산 간의 차이가 크진 않지만, 변수의 단위가 ‘수량’과 ‘원(₩)’으로 서로 다르기에 상관행렬을 이용하여 PCA를 진행해야 한다는 사실을 안다.

![image](https://user-images.githubusercontent.com/89781598/193556621-7d9b5a44-6664-446d-8141-0aed04e8529e.png)

### 주성분 개수의 선택
“[2021금융데이터경진대회] 동학개미운동 신규 고객 투자 정보“ 데이터는 총 11개의 변수로 이루어진 데이터이다. 이 변수들의 차원을 줄이기 위해 PCA를 실시하는데, 주성분의 개수를 구하는 방법에는 설명비율과, 이 설명비율을 시각적으로 볼 수 있는 Scree Plot이 존재한다. 설명비율에 대한 결과는 표 8.2에서, Scree Plot은 그림 7.1에서 확인할 수 있다.

![image](https://user-images.githubusercontent.com/89781598/193556709-6d4d405f-36e2-4800-adb6-ad090f57c4a7.png)


### 주성분 해석
표 3.3은 주성분 계수를 나타내는 표이다. 

![image](https://user-images.githubusercontent.com/89781598/193556789-2cb5195d-0ad8-46c1-a85b-54b398e320c8.png)

이 때, 첫 번째 주성분을 보면, x2,x3에 해당하는 주문수량, 총체결수량, x10,x11에 해당하는 거래수량과 거래대금과는 음의 관계 나머지 변수와는 양의 관계이므로 위 네가지 변수를 제외하고 있으므로 제 1 주성분은 거래된 주식의 가격을 나타내는 성분으로 볼 수 있으며, 표 8.2에서 확일 할 수 있듯이 위 주성분의 설명력은 58.28%이다.

두 번째 주성분의 경우 변수 x2,x3,x4에 해당하는 주문수량, 총체결수량, 총체결금액 등에 크게 반응하고있음을 볼 수 있다. 즉 두 번째 주성분은 체결된 주식의 수량에 대한 정보를 반영하고 있다고 볼 수 있다. 이러한 두번째 주성분의 설명력은 28.18%이다.

세번째 주성분은 변수 x11에 대한 주성분 계수의 값이 유독 크다는 것을 확인할 수 있다. 이 변수는 거래대금을 반영하고 있다. 거래대금은 거래된 주식의 가격과 양의 정보를 모두 포함하는 변수이다. 일반적으로 거래대금의 절대적 크기가 클 수 록 우량주에 속하므로, 주식의 안정성과 관련된 정보를 내포하고 있다고 볼 수 있다. 세번째 주성분의 설명력은 8.26%이다.

### 주성분 점수의 산점도
위 표는 첫 번째 주성분과 두 번째 주성분으로 이루어진 주성분 점수 산점도이다.

![image](https://user-images.githubusercontent.com/89781598/193556886-2c887e8c-14a4-4604-8dde-3451f43c89f0.png)

첫 번째 주 성분은 실주문단가와 총 체결금액, 일중저가, 당일종가 등 주식의 가격에 관한 변수임과 동시에 실제 체결된 가격을 표시하므로, 이는 어떤 한 관측 값이 주식 매입에 대한 최대지불용의를 표시하기 때문에 관측 값의 투자수요를 나타낸다고 볼 수 있다.
왼쪽으로, 투자에 대한 수요도 낮은 축에 속하며, 오른쪽으로  투자 수요도 높은 축에 속한다고 볼 수 있다.

두 번째 주성분은 총 체결 수량 및 거래 수량과 관련된 변수이다. 이는 한 관측 값이 얼마나 많이 주식을 매입하였느냐를 표시해준다. 관측 값이 주식에 대한 수요가 있고

실제 거래를 체결한다면 본인의 소득 수준 하에서 얼마나 많이 매입할 여유가 있는지를 보여주므로 관측 값의 주식투자 여유자금으로 평가할 수 있다. 위 표에서 위쪽에 위치할수록 체결수량이 작으므로 여유자금이 적은 축에 속하고, 아래에 위치할수록 주식 투자에 여유자금이 많은 축에 속한다고 볼 수 있다.

이러한 관점에서 위 표를 바라보면, 20-30대가 표에 왼쪽에 많이 군집해 있으므로, 주식에 대한 투자수요가 낮다고 볼 수 있고, 표에 오른쪽에 분포한 40대 50대는 투자수요가 높은 축에 속한다고 볼 수 있다.

또한, 위쪽에 위치한 20-30대는 주식 투자 여유자금이 적은 축에 속하고, 50대는 여유자금이 많은 축에 속하나, 40대는 지역 간 편차가 있으나 대부분 여유자금은 없으나 수요는 많은 축에 분포에 있다.

이에 따라, 경제활동을 막 시작한 20-30대는 주식에 투자할 여유자금이 부족하며, 주식에 대한 투자수요 또한 낮고, 교육비, 주거비 등 지출이 가장 커지는 시기인 40대에는 여유자금은 부족하나, 수요는 높은 것으로 나타났고, 은퇴를 앞둔 50대는 주식 투자의 여유자금이 충분하며, 투자의 대한 수요 또한 높은 것으로 보였다.

특징적인 값으로는 제주,경북,울산,광주 40대가 타 지역에 비해 투자여유자금이 많은 것으로 나타났고, 50대 제주가 여유자금은 가장 많으나, 투자수요가 가장 적은 것으로 나타났고, 50대 강원도 또한 여유자금은 있으나 투자수요는 적은 것을 보였다.

아래 표는 첫 번째 주성분과 세 번째 주성분으로 이루어진 주성분 점수 산점도이다. 첫 번째 주성분은 앞서 말했듯, 주식 투자에 대한 수요를 표시해 준다.

![image](https://user-images.githubusercontent.com/89781598/193556952-f2d35129-4749-41d8-9940-dcb822dd8569.png)

위 표는 첫 번째 주성분과 세 번째 주성분으로 이루어진 주성분 점수 산점도이다. 첫 번째 주성분은 앞서 말했듯, 주식 투자에 대한 수요를 표시해 준다.

세 번째 주성분은 총 거래대금 및 거래수량이므로 투자처로써 주식의 안정성을 반영하는 것으로 볼 수 있다. 위쪽에 위치할수록 거래대금 및 거래수량이 많으므로 안전자산에 속한다고 볼 수 있고, 아래에 위치할수록 총 거래대금 및 거래수량이 작은 위험자산으로 분리할 수 있다.

이러한 기준으로 세 번째 주성분의 주식의 안정성을 1~3 사이를 고 수준, 1~-1 사이를 중 수준, -1~-3미만을 저수준으로 분리해 보면, 첫 번째 주성분에 의한 투자수요에 상관없이 대부분 중수준에 위험수준의 주식을 선호하는 것으로 평가할 수 있다. 안정성이 높은 주식은 수익률이 낮고, 안정성이 낮은 주식은 수익률이 높은 경향이 있으므로, 대체적으로 중간정도의 안정성과 수익률을 가지는 주식을 선호한다고 볼 수 있다. 즉, 주식에 대한 수요도와 관계없이 대부분 어느 정도의 수익과 안정성을 가지는 균형 잡힌 주식을 선호하는 경향이 크다.

특징적인 값으로는 50대 제주와 강원, 충남과 대구이다. 투자수요가 적은 제주와 강원, 투자수요가 높은 충남과 대구는 각각 주식의 안정성에 대해 정반대의 선호도를 보였다.
제주는 가장 안정적인 투자처를 선호하는데 반해 강원은 안정성이 저수준에 속하는 주식을 선호하였으며, 대구 또한 고수준의 안정성을 띄는 주식을 선호하는데 반해 충남은 가장 낮은 안정성을 가진 주식을 선호했다.

마지막으로, 두번째 주성분과 세번째 주성분으로 이루어진 주성분 점수 산점도이다.

![image](https://user-images.githubusercontent.com/89781598/193556999-c8764289-dff3-464a-8605-655415fcdb3d.png)

두번째 주성분은 주식 투자에 대한 여유자금을 표시하고 있으며, 위 표에선 왼쪽으로 갈 수록 여유자금이 많은 것이다. 세번째 주성분은 주식의 안정성을 표시하고 있으며, 위에서처럼 구간에 따라 고수준, 중수준, 저수준으로 나누어 볼 수 있다.

이에 따라 위 표를 살펴보면, 두 번째 표에서 분석했던 것과 마찬가지로 주식에 투자할 수 있는 여유자금과 관계없이 대부분 안정성과 수익률이 중수준인 주식을 선호하는 것으로 나타났다. 특징적인 값은 두번째 표에서와 같이 50대 제주와 강원, 충남과 대구이다. 여유자금 수준이 높은 축에 속하는데에도 불구하고 제주와 강원 충남과 대구는 안정성에 대한 선호가 반대로 나타났다.

### 주성분 행렬도
이 때, 확실한 분석을 위해서 주성분 행렬도를 만들면, 그림 3.5와 같은 결과가 나오는 것을 확인할 수 있다.

![image](https://user-images.githubusercontent.com/89781598/193557081-726c2bc9-c879-4d35-900b-66f9e8d06ce1.png)

주성분 행렬도의 화살표를 먼저 살펴보면, 화살표의 각도가 가까운 변수들 사이에 큰 상관관계가 있음을 알 수 있고 각도가 먼 변수들은 서로 관련이 작은 변수이다. 즉, 실주문단가, 일중저가, 당일종가 등 주식에 대한 지불용의를 보여주는 변수들 사이에 큰 상관관계가 있고, 총체결수량, 총체결금액, 주문수량 등 주식 투자에 여유자금과 관련된 변수사이에 관계가 있으며, 거래수량과 거래대금 등 총 주식 안정성에 관련된 변수사이에 상관관계가 크다.
또한 세번째 주성분을 이루고있는 거래수량과 거래대금은 고수준, 중수준, 저수준으로 나누어 볼 수 있다.

이를 기준으로 관측치를 파악해보면, 지역에 상관없이 40-50대가 투자수요나 여유자금 면에서 20-30대보다 더 낫다고 볼 수 있다. 20-30대는 경제활동을 막 시작하거나 준비하는 단계이므로 소득이 많지 않고, 노후대비에 대한 동기도 크지 않으므로, 주식 투자에 수요 및 여유자금이 작다. 반면 40-50대는 20-30대에 비해 경제활동이 활발히 이루어져 충분한 소득이 있고 노후준비에 대한 동기가 크므로, 주식투자에 대한 수요가 높지만, 동시에 40대는 교육비, 주거비, 양육비 등 지출이 다른 나이 대에 비해 커지는 시기이므로, 주식투자에 여유자금이 부족하다고 볼 수 있다.

그리고 주식에 대한 수요와 여유자금과 관계없이 안정성과 그에 반비례하는 수익률이 균형잡힌 주식을 선호하는 경향이 있다. 예외적으로 50대 제주와 강원은 충분한 자금에 있음에도 불구하고 투자수요가 작다.

이러한 모든 내용을 요약하면 아래의 표와 같다.

![image](https://user-images.githubusercontent.com/89781598/193557145-eeaa98af-bfca-4040-953a-eefe84f0b0b7.png)

# Factor Analysis(FA)

### 공동 인자 개수 구하기
앞에서(1. Data Descriptionⅰ.전반적인 데이터 설명) 우리는 분석을 행할 때 상관행렬을 사용해야한다는 사실을 알 수 있었고, 그러므로, 상관행렬을 스펙트럴 분해하여 설명비율을 구해준 후, 공동 인자의 개수를 확인한다. 그에 대한 수치적 결과는 설명비율로, 시각적인 결과는 Scree Plot을 통해서 확인가능하다.

![image](https://user-images.githubusercontent.com/89781598/193557418-fa7aa68d-8e06-43eb-94ab-54083914d89e.png)

이 때, 앞에서의 (3. Principal Component Analysis(PCA)ⅱ. 주성분 개수의 선택) 의 결과와 같다는 것을 알 수 있는데, 앞에서의 분석과 지금의 분석에서 모두 스펙트럴 분해를 이용하여 분석하였기 때문이다. 세 번째 공통인자까지의 설명비율이 94.724%이고, Scree Plot의 팔꿈치가 3번에서 이루어지는 것을 확인할 수 있으므로, 3개의 공동인자를 사용하여 FA를 수행한다.

이 때, 공통인자 3개를 분석하면 아래와 같이 나온다.

![image](https://user-images.githubusercontent.com/89781598/193557501-f5999867-c534-4bd1-97fa-39e01512a93a.png)

표 4.3은 공통인자 계수를 나타내는 표이다. 이를 앞에 나와 있는 표 3.3의 주성분 계수와 비교해본다. 이 때, 이 결과 값은 위에 제시된 주성분 계수와 추세가 비슷하다고 할 수 있다. 즉, 같은 결과가 나온다는 것을 암시한다. 즉, PCA의 주성분 계수와 PCFA의 공통 인자의 계수의 특징이 비슷하고, 계수들을 서로 비교해 보았을 때, 의미하는 특징들이 주성분 분석과 같다는 것을 알 수 있다.

### PCFA
PCFA를 수행하여 인자 적재 그림을 통해 변수들의 특성을 파악해본 후, 인자 점수 그림으로 관측치를 분석하는 절차가 이루어져야 하므로, 우선 인자 적재그림을 통해 변수들의 특성을 파악해본다. 1번째 공통인자와 두 번째 공통인자, 1번째 공통인자와 3번째 공통인자, 2번째 공통인자와 3번째 공통인자에 대한 인자적재그림은 그림 4.2.에서 확인 가능하다.

![image](https://user-images.githubusercontent.com/89781598/193557581-dfa60426-5bea-4f2a-a768-ab9131421e7a.png)

또한 이에 대한 인자 점수 그림은 그림 4.3에서 확인가능하다.

![image](https://user-images.githubusercontent.com/89781598/193557626-812c5bba-b489-4ac0-9e41-7320e2b390c2.png)

이 때, 나오는 결과는 위의 주성분 분석과 동일하다는 것을 알 수 있다. 이렇게 PCFA를 진행하는 이유는 앞의 PCA와 결과가 일치하는지를 확인하기 위한 이중검정이었다. 만약 PCFA에서 결과가 달라졌다면 이 연구는 무의마하고, 앞에서 분류한 기준들도 모두 무의미해지지만, 이렇게 PCFA로 다시 검증을 하면서 이 연구가 보다 신뢰성을 얻을 수 있다고 이야기 할 수 있다.

그림 4.4에서는 인자 행렬도를 확인할 수 있다.

![image](https://user-images.githubusercontent.com/89781598/193557663-ffd856fb-cd81-4d8c-a6c0-caca7310019d.png)

인자행렬도를 그려봤을 때 또한 결과가 비슷하게 나온다.

### MLFA
이 때 PCFA에 대한 정당성은 잔차행렬로 파악할 수 있다. 잔차행렬의 원소들이 0으로 수렴해야 성능이 좋다고 판단할 수 있다. 이 때, PCFA의 대부분의 원소들은 0에 수렴하지만, MLFA의 원소들은 0에 수렴하는 원소들이 거의 없고, 심지에 거의 1에 가까운 값들을 가지고 있는 원소들도 많다. 잔차행렬을 보았을 때 PCFA의 성능은 뛰어난 것으로 보인다.

![image](https://user-images.githubusercontent.com/89781598/193557760-1985f168-1586-4108-a3c0-4b46f47fae14.png)

이 때, 우리는 MLFA를 수행해야할 정당성을 얻지 못한다. MLFA는 다변량 정규성을 만족해야 시행할 수 있는데, 앞에서 데이터가 다변량 정규성을 만족하지 못한다는 사실을 알 수 있었고, 잔차행렬로 검정도 해보았을 때, 더욱 MLFA를 하지 말아햐 한다는 주장에 힘을 실어준다.

# Cluster Analysis(CA)

### 계층적 군집분석
군집분석에는 여러 가지 연결법이 존재하는데, 이를 크게 두 부류로 나누면 계층적 연결법과 비 계층적 연결법이다. 이 때 모든 연결법은 표준화 유클리드 거리와 마할라노비스 거리를 이용하였다. 계층적 연결법은 단일연결법, 완전연결법, 평균연결법, 와드연결법이 존재한다. 이 때, 가장 대중화된 단일연결법과 와드연결법을 사용하기로 한다. 비 계층적 연결법에는 K-평균법과 K-중위수법이 존재하는데, 여기에서, 가장 많이 사용하는 K-연결법을 사용하기로 한다. 단일 연결법을 시행한 결과는 그림 9.1에 있다.

![image](https://user-images.githubusercontent.com/89781598/193557978-5792a1db-3be9-4ba7-9f64-f2b54fb50ce9.png)
![image](https://user-images.githubusercontent.com/89781598/193558006-098f86fd-b29e-4fe1-99fa-044a2f4fcefa.png)

단일 연결법으로는 군집 형성이 힘들다는 것을 알 수 있지만, 와드 연결법으로는 8개 정도의 군집이 나타나는 것을 확인할 수 있다.

![image](https://user-images.githubusercontent.com/89781598/193558059-0a08765a-50bf-4a6f-aeb6-a3899a853cbd.png)

마할라노비스 거리를 이용해도 단일 연결법으로는 군집을 도출할 수 없었지만, 와드 연결법으로는 8개 정도의 군집이 형성되는 것을 볼 수 있다.
하지만, 군집수를 추정하기 위한 더 좋은 방법은, ccc와 Dindex등을 이용하는 것이다. 이에 대한 결과는 그림 9.3에 있다. 비록 표준화 유클리드 거리와 마할라노비스 거리를 이용하여 군집을 5개정도 형성할 수 있을 것이라고 유추해볼 수 있지만, 더 확실하게 파악하기 위해서는 여러 측도들을 사용해야 한다.

![image](https://user-images.githubusercontent.com/89781598/193558112-85121d98-cb4f-4295-9930-1c9173cc884c.png)

우선 ccc를 이용하여 군집의 수를 추정할 때는 ccc값이 가장 큰 경우의 군집수를 사용하면 되는데, 이 때, ccc가 가장 큰 값은 군집의 수가 4개 일 때이다. Dindex 그래프의 경우 기울기가 급격히 감소하는 구간(빨간 선 그래프), 기울기가 급격히 증가하는 구간(파란 선 그래프)로 군집의 수를 선택하는데, 이 때, 급격히 감소하는 구간은 군집의 수가 6인 구간이라고 할 수 있다. 즉, ccc와 Dindex에서는 군집이 4~6개의 군비이 가장 좋은 분석이라고 판단된다.

하지만 모든 측도들을 고려해보면, 아래의 표 5.1에서 확인할 수 있다.

![image](https://user-images.githubusercontent.com/89781598/193558164-d0459b89-c820-4158-a915-c70c1d34aef0.png)

이 때, 6개의 측도에서 2개의 군집으로 나누는 것이 가장 좋다고 이야기하였으며, 그 다음은 3개의 군집으로 나누는 것이 가장 좋은 분석이라고 평가하고 있다. 이 때 2개, 3개의 군집으로 나누면 군집이 너무 크게 나누어지므로 특징을 파악할 수 없고, 4개로 추천해주는 측도보다 8의 군집으로 나누는 것을 추천하는 측도가 훨씬 많으므로, 우리는 8개의 측도로 나누어서 관측치의 특성을 파악할 것이다.

### 비계층적 군집분석
계층적 연결법에서, 군집을 8개로 나누는 것이 가장 효과적이라고 판단하였다. (물론, 2개의 군집으로 나눈 것이 가장 효율적이라는 결과를 얻었지만, 2개로 나누었을 때 관측치들의 특성이 전혀 나타나지 않으므로, 2번째로 효율이 좋은 8개의 군집으로 나누는 방법을 사용한다.) 이 때, K-평균법을 사용하여 군집을 나누어주면 표 5.2과 같은 결과를 얻는다.

![image](https://user-images.githubusercontent.com/89781598/193558242-a80eb368-96eb-430e-a38e-dfaaa593a2de.png)

또한, 각 군집에 대한 특성을 나타낸 표는 표 5.3에서 확인할 수 있다.

![image](https://user-images.githubusercontent.com/89781598/193558293-7be208e1-04a4-4456-bd13-b2dadaecb094.png)

또한, 로그 변환을 해제하면 아래와 같다.

![image](https://user-images.githubusercontent.com/89781598/193558335-32bde32a-b1dd-44f8-8f70-a63a26db328a.png)

# 결론
다음 표는 K-평균법 결과로 도출된 각각 군집들의 특성을 나타낸 표에서 각각의 주성분과 높은 연관성을 가진 변수들을 추출하여 군집들간 순위를 나타낸 표이다. 변수와 가장 높은 관련도를 띄는 변수를 1번으로 두고 각각을 비교한 뒤, 합계를 내어 최종 순위를 내리는 방식으로 합계가 가장 적은 그룹이 주성분과 가장 높은 연관성을 가진 것 이라고 할 수 있다.

![image](https://user-images.githubusercontent.com/89781598/193558492-bd59286b-454e-4531-9b62-14b1aabc7234.png)

먼저 <표 10.5>는 첫번째 주성분과 관련된 변수로 각각의 변수들은 전일종가 당일시가 일중저고가등 주식의 가격과 관련된 변수로 주당 주식의 시장가치 및 주당 거래자의 최대 지불용의이므로 이를 주식에 대한 수요도라는의미로 해석했다. 즉 주식이 높은 가격임에도 불구하고 지불용의가 있다는 것은 그만큼 거래자가 자본재로서 주식을 높게 평가하고 수요함을 의미한다고 보는 것이다.

이를 기준으로 정리했을 때, 앞서 본 결과들과 같이 40-50대가 다수 포진한 그룹(그룹1,그룹3,그룹5)이 가장 높은 순위를 차지했다. 다른 통계 결과로 내린 추정과 마찬가지로 대부분의 40-50대가 주식에 대한 수요가 다른 나이대에 비해 크다고 볼 수 있다.

다만 예외적으로 50대 강원,울산,전남,전북,충북은 20-30대 대전,제주,부산,전남 그룹보다 낮은 등수를 보여주었고, 50대 중에서는 제주가 가장 낮은 순위를 차지했다.

20-30대에서는 경북,서울,충남 지역이 타지역의 20-30대에 비해 순위가 높은 그룹에 속했고 경남지역은 20대와 30대 모두 가장 순위가 가장 낮은 그룹에 속하므로 20-30대 통틀어 주식에 대한 수요가 가장 낮다고 볼 수 있다.

지역 사이에는 경남과 경북지역 충남과 충북으로 지역 간 차이가 있다는 것을 볼 수 있다.

나이대별로 나누어보면 대전,충남 지역이 가장 높은 등수를 차지했다.

![image](https://user-images.githubusercontent.com/89781598/193558539-cd388976-deea-4f08-a8a6-928e0d9ba987.png)

표10.6은 두 번째 주성분에 영향을 미치는 변수와 그룹사이의 관계를 순위로 나타낸 표이다.

두 번째 주성분은 주식 주문수량과 총체결수량 및 총체결한금액과 관련된 변수로 전반적으로 주식의 총 구매 수량 및 총 시장가치를 반영한 변수이므로 주식을 구매할 수 있는 여유자금의 크기로 해석했다.

이를 바탕으로 위 표를 통해 알 수 있듯이 40-50대가 속한 그룹(그룹1,그룹2,그룹3,그룹4)가 주식에 대한 수요가 높은 것으로 해석된 것과 마찬가지로 20-30대보다 여유자금의 크기가 더 큰 것으로 보였다. 40대보단 50대가 더 많이 속한 그룹이 높은 등수를 기록했다. 앞선 표에서 다소 낮은 등수를 보였던 50대 제주와 강원,울산,전남,전북,충북지역도 여유자금 측면에선 높은 등수를 기록했다. 또한 20대가 속한 그룹보다 30대가 속한 그룹이 더 높은 등수를 기록했다. 프랑코 모딜라이니의 생애주기 가설에 따른 일반적인 소득의 생애주기를 고려해봤을 때, 나이대별 고정적인 소득 수준과 지출 수준에 따른 결과로 해석할 수 있다.

지역별로 큰 차이를 보이는 것은 앞서 마찬가지로 경북지역과 경남지역이었다.

나이대별로 순위를 정리해 봤을 때, 서울지역이 50대를 제외한 모든 나이대에서 가장 높은 순위를 기록했고, 30대 경북,서울,충남은 40대 경남,인천,전북보다 더 높은 순위를 기록했다.

![image](https://user-images.githubusercontent.com/89781598/193558589-1976216c-6d8c-4156-9b76-70c169968e4a.png)

마지막 표는 세 번째 주성분과 관련된 변수이고, 이는 거래수량과 거래대금을 나타내주는 변수이다. 거래수량과 거래대금이 많은 주식일수록 우량주에 가깝기 때문에 세 번째 주성분은 앞 서, 주식의 안정성을 보여주는 지표로 해석하였다. 위 표를 통해 40-50대가 20-30대에 비해 비교적 위험선호적이라고 볼 수 있다.

이러한 모든 내용을 정리한 것이 아래의 표와 같다.

![image](https://user-images.githubusercontent.com/89781598/193558670-422f42ee-ef94-4bf8-9078-a28bfad8901d.png)

