import os
from dotenv import load_dotenv
import openai
import json
from datetime import datetime

# .env 파일에서 환경 변수 로드
load_dotenv()

# 환경 변수 값 불러오기
OPENAI_API_KEY = os.getenv("OPENAI_API_KEY")
OPENAI_ENDPOINT = os.getenv("OPENAI_ENDPOINT")
CHAT_MODEL = os.getenv("CHAT_MODEL")  # 예: "gpt-4o"


"""
code_conversion_service.py

이 모듈은 COBOL 소스 코드의 단일 파일 변환 기능을 제공합니다.
(예: COBOL → Python, COBOL → Java 등)

[주요 기능]
1. 단일 COBOL 코드 변환 (convert_cobol_code)
   - 입력받은 COBOL 소스 코드를 지정한 언어(Python, Java 등)로 변환합니다.
   - 실제 변환은 OpenAI GPT API 등 외부 변환 엔진을 활용할 수 있습니다.

2. 변환 이력 저장 (save_history)
   - 변환 요청 시 입력 코드, 변환 결과, 변환 언어, 타임스탬프를 이력 파일(jsonl)에 저장합니다.

[함수별 설명]
def convert_cobol_code(cobol_code: str, target_lang: str) -> str
    - 입력:
        cobol_code: 변환할 COBOL 소스 코드(문자열)
        target_lang: 변환 대상 언어 (예: "python", "java")
    - 출력:
        변환된 코드(문자열)

def save_history(input_code: str, output_code: str, lang: str) -> None
    - 입력:
        input_code: 변환 전 COBOL 코드(문자열)
        output_code: 변환된 코드(문자열)
        lang: 변환 언어(예: "python", "java")
    - 출력:
        없음 (이력 파일에 한 줄씩 저장)
"""

# 실제 변환 로직은 OpenAI API 등 외부 서비스와 연동 필요
def convert_cobol_code(cobol_code: str, target_lang: str) -> str:
    """
    COBOL 코드를 GPT-4o(OpenAI)로 Python 또는 Java 코드로 변환합니다.
    """
    if not cobol_code or not target_lang:
        return "# 변환할 COBOL 코드와 대상 언어를 모두 입력하세요."

    # 프롬프트 구성
    prompt = (
        f"다음 COBOL 코드를 {target_lang.capitalize()} 코드로 변환해줘. "
        "주석은 최소화하고, 가독성 좋게 작성해줘.\n\n"
        f"COBOL 코드:\n{cobol_code}"
    )

    try:
        # OpenAI API 클라이언트 설정
        client = openai.OpenAI(
            api_key=OPENAI_API_KEY,
            base_url=OPENAI_ENDPOINT            
        )
        response = client.chat.completions.create(
            model=CHAT_MODEL,
            messages=[
                {"role": "system", "content": "You are a helpful AI developer assistant."},
                {"role": "user", "content": prompt}
            ],
            temperature=0.2,
            max_tokens=2048
        )
        # GPT-4o 응답에서 변환된 코드 추출
        return response.choices[0].message.content.strip()
    except Exception as e:
        print(f"[DEBUG] 변환 중 오류 발생: {e}")
        return f"# 변환 중 오류 발생: {e}"

#변환 이력 저장 
def save_history(input_code, output_code, lang):
    """
    변환 이력을 data 폴더에 일자별 파일로 누적 저장합니다.
    파일명: data/history_YYYYMMDD.jsonl
    각 이력에는 timestamp(시:분:초 포함)를 기록합니다.
    """
    os.makedirs("data", exist_ok=True)
    today = datetime.now().strftime("%Y%m%d")
    file_path = f"data/history_{today}.jsonl"
    log = {
        "timestamp": datetime.now().strftime("%Y-%m-%dT%H:%M:%S"),
        "input": input_code,
        "output": output_code,
        "lang": lang
    }
    with open(file_path, "a", encoding="utf-8") as f:
        f.write(json.dumps(log, ensure_ascii=False) + "\n")