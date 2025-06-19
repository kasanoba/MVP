# AI 기반 COBOL 소스 일괄 변환 및 개발 지원 에이전트

이 프로젝트는 COBOL 소스 코드를 Python/Java 등으로 변환하고, 변환 이력 관리 및 일괄 변환 기능을 제공하는 Streamlit 기반 웹 애플리케이션입니다.

---

## 주요 기능

- **언어전환**  
  - COBOL 코드를 직접 입력하거나 파일로 업로드하여 Python/Java 등으로 변환
  - OpenAI Chat API 호출 전 기존 변환 이력을 먼저 조회하여 보여줌으로써 자원 과소비 방지
- **변환 이력 조회**  
  - 변환 이력 파일을 선택하여 과거 변환 내역을 조회
- **일괄전환**  
  - 디렉토리 전체 COBOL 파일을 일괄 변환  
  - 디렉토리 현황 조회, 예상 비용 안내, 진행률/실패 파일 안내 등 포함
- **금융권 규정 검토**  
  - (추후 구현 예정)

---

## 폴더 및 파일 구조

```
mvp/
├── data/                  # 변환 이력(jsonl) 저장 폴더
├── services/
│   ├── batch_conversion_service.py   # 디렉토리 분석, 일괄 변환 서비스
│   └── code_conversion_service.py    # 단일 변환, 변환 이력 저장 서비스
├── ui/
│   └── main_ui.py         # Streamlit UI 및 탭별 기능
├── run_app.py             # 앱 실행 진입점
├── requirements.txt       # 필요한 패키지 목록 (아래 참고)
├── .env                   # 환경변수 파일 (API KEY 등)
└── streamlit.sh
```

### requirements.txt
```
streamlit
python-dotenv
openai
aiohttp
requests
azure-storage-blob
tiktoken
```

---

## 앱 실행 방법

1. **필수 패키지 설치**
    ```bash
    pip install -r requirements.txt
    ```

2. **환경변수(.env) 파일 작성**
    ```
    OPENAI_API_KEY=sk-xxxxxxx
    OPENAI_ENDPOINT=https://api.openai.com/v1
    CHAT_MODEL=gpt-4o-mini
    AZURE_STORAGE_CONNECTION_STRING=your_connection_string  # Blob 저장 시 필요
    CONTAINER_NAME=your_container_name                      # Blob 저장 시 필요
    ```

3. **앱 실행**
    ```
    streamlit run run_app.py
    ```
    - `run_app.py`는 내부적으로 `ui/main_ui.py`의 `main()` 함수를 호출하여 Streamlit 앱을 구동합니다.

---

## 사용자 흐름

1. **앱 접속 및 환경변수 확인**
2. **화면 로딩시 Blob service에 컨테이너 생성여부 확인하여 "data"이름으로 생성**
3. **탭 선택**
    - **언어전환**:  
        1. COBOL 코드 입력/파일 업로드  
        2. 기존 변환 결과 조회 및 재사용 여부 결정  
        3. 변환 실행 및 결과 확인
    - **변환 이력 조회**:  
        1. 이력 저장 위치(로컬/클라우드) 선택  
        2. 이력 파일 선택 후 조회 유형(직접입력/파일) 필터링하여 내역 확인
    - **일괄전환**:  
        1. COBOL 소스 ZIP 업로드  
        2. ZIP 파일내의 디렉토리 분석 결과(파일 수, 토큰 수, 예상 비용 등) 확인  
        3. 변환 언어 선택 및 변환 실행  
        4. 결과 ZIP 다운로드
3. **결과 확인 및 추가 작업**

---

## 사용된 기술 스택

- Python 3.11
- Streamlit (웹 UI)  
- OpenAI API (GPT-4o-mini)  
- Azure Blob Storage (옵션, 변환 결과 저장용)  
- python-dotenv (환경변수 관리)  
- 표준 Python 라이브러리 (os, json 등)

---

## 향후 확장 방향

- 다양한 언어(예: C#, Go 등)로의 변환 지원
- 변환 품질 향상을 위한 프롬프트/파라미터 튜닝
- 변환 이력의 DB 저장 및 검색 기능
- 금융권 규정 검토 기능 고도화
- 대용량 파일/대규모 프로젝트 지원(비동기 처리, 분산 처리 등)
- 사용자 인증 및 권한 관리
- 변환 결과의 자동 테스트/검증 기능

---

## 최대 고려사항

- **보안**: API Key 등 민감 정보는 반드시 .env 파일 등 안전한 경로에만 저장
- **확장성**: 서비스 레이어와 UI 분리, 모듈화 구조로 기능 추가/유지보수 용이
- **안정성**: 예외 처리, 실패 파일 안내, 진행률 표시 등 사용자 경험 강화
- **비용 관리**: OpenAI API 사용량/비용 예측 및 안내
- **대용량 처리**: 파일 수/용량이 많을 때도 안정적으로 동작하도록 설계

---
