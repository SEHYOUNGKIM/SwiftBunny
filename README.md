# SwiftBunny
하나씩 추가해 나가겠습니다.  
문제가 있다면 sehyoung66@ajou.ac.kr 으로 부탁해요.  
감사합니다.  

## 1) 다음 뉴스내용만 크롤링
1-1. 일단 워드 클라우드 그리기 위해서 다음에서 뉴스 기사 내용만 가져오는 크롤러 입니다.  
1-2. 워드 클라우드만을 위해 혹은 LDA를 위해 제작하였습니다.  
1-3. daum_news_for_LDA(분야, 최대페이지)를 입력하시고, 분야의 경우 /politics /society /economic 혹은 /economic/consumer과 같이 하시면 됩니다.  
1-4. https://news.daum.net/breakingnews/economic/consumer 이 곳에서 더 많은 분야를 확인해 보세요.  

## 2) 피파온라인 인벤 크롤링 (v1)
2-1. 버전 1 입니다. 그 이유는.. 직접 들어가서 선수정보같은거 크롤링하는거까지 만들기 귀찮아서..  
2-2. B-Cube 스터디 내용에서 업데이트만 한거에요.  
2-3. 입력값은 벡터 형태로 넣으시면 됩니다. 예를 들어 c("호날두", "메시", "손흥민")  
2-4. *selenium*을 사용해야 합니다. 준비해 두세요.  
2-5. for **준모, 종우**  
2-6. e-biz 서버에서 돌릴생각 하지 마세요. 아 이거 셀레니움때문에..ㅎㅎ  

## 3) 네이버 카페 크롤링 (v1)
3-1. 버전 1 입니다.  
3-2. naver_cafe_v1(c("키워드1", "키워드2"), 최대 페이지) 형식으로 입력하셔야 합니다.  
3-3. 역시 selenium을 사용하기 때문에 ebiz 서버에서는 돌리지 마세요.  
3-4. 수빈이와 진아의 네이버 카페 크롤러 질문을 받고 만들었는데 저도 많이 쓸듯 하네요.  
