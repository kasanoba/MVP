import os
import openai  # OpenAI API 사용
import json
from dotenv import load_dotenv, find_dotenv  # 환경 변수(.env 파일)를 불러오기 위한 라이브러리
from datetime import datetime  # 날짜와 시간 관련 기능
from azure.storage.blob import BlobServiceClient  # Azure Blob 저장소 연동


# .env 파일에서 환경 변수 로드
load_dotenv(find_dotenv())

# 환경 변수에서 필요한 값 가져오기 (Azure OpenAI용)
AZURE_OPENAI_ENDPOINT = os.getenv("AZURE_OPENAI_ENDPOINT")
AZURE_OPENAI_API_KEY = os.getenv("AZURE_OPENAI_API_KEY")
DEPLOYMENT_NAME = os.getenv("DEPLOYMENT_NAME")

# OpenAI API 설정 (Azure OpenAI용으로 맞춤 설정)
openai.api_type = "azure"
openai.azure_endpoint = AZURE_OPENAI_ENDPOINT
openai.api_version = "2024-12-01-preview"
openai.api_key = AZURE_OPENAI_API_KEY

# Azure Blob Storage 설정 - 연결 문자열 및 컨테이너 이름 가져오기
conn_str = os.getenv("AZURE_STORAGE_CONNECTION_STRING")
container_name = os.getenv("CONTAINER_NAME")

# Blob 저장소를 제어할 수 있는 클라이언트 객체 생성
blob_service_client = BlobServiceClient.from_connection_string(conn_str)

"""
code_conversion_service.py

이 모듈은 COBOL 소스 코드의 단일 파일 변환 기능을 제공합니다.
(예: COBOL → Python, COBOL → Java 등)

[공통 기능]
1. 입력값 또는 선택된 파일을 OpenAI GPT API를 통해 변환
2. local 저장소 또는 Azure Blob Storage에 변환 이력 저장

[주요 기능]
1. 단일 COBOL 코드 변환 (convert_cobol_code)
   - 입력받은 COBOL 소스 코드를 지정한 언어(Python, Java 등)로 변환합니다.
   - 실제 변환은 OpenAI GPT API 등 외부 변환 엔진을 활용할 수 있습니다.

2. 변환 이력 저장 (save_history)
   - 변환 요청 시 입력 코드, 변환 결과, 변환 언어, 타임스탬프등을 이력 파일(jsonl)에 저장합니다.

[함수별 설명]
convert_cobol_code(cobol_code: str, target_lang: str) -> str
    - 입력된 COBOL 코드를 OpenAI GPT 모델을 사용해 Python 또는 Java 코드로 변환합니다.
    - 변환 요청은 프롬프트 기반으로 처리되며, 결과 코드는 가독성 위주로 생성됩니다.

save_history(input_code, output_code, lang, filename=None, storage_type="local")
    - 변환 요청의 입력/출력 코드, 언어, 시간 등의 이력을 저장합니다.
    - 저장 위치는 로컬 또는 Azure Blob Storage 중 선택할 수 있습니다.

save_history_to_blob(log: dict)
    - 변환 이력을 Azure Blob Storage에 JSONL 형식으로 저장합니다.
    - 기존 이력이 있으면 내용을 병합하여 업데이트합니다
            
"""

# COBOL 코드 변환 함수
def convert_cobol_code(cobol_code: str, target_lang: str) -> str:
    """
    사용자가 입력한 COBOL 코드를 OpenAI GPT 모델을 통해 Python 또는 Java 코드로 변환합니다.
    """
    if not cobol_code or not target_lang:
        return "# 변환할 COBOL 코드와 대상 언어를 모두 입력하세요."

    prompt = (
        f"다음 COBOL 코드를 {target_lang.capitalize()} 코드로 변환해줘. "
        "주석은 최소화하고, 가독성 좋게 작성해줘.\n\n"
        f"COBOL 코드:\n{cobol_code}"
    )

    try:
        response = openai.chat.completions.create(
            model=DEPLOYMENT_NAME,
            messages=[
                {"role": "system", "content": "You are a helpful AI developer assistant."},
                {"role": "user", "content": prompt}
            ],
            max_tokens=100,  # 응답 길이 제한
            temperature=0.7  # 창의성 정도 (0.0: 보수적, 1.0: 창의적)
        )
        return response.choices[0].message.content.strip()
    except Exception as e:
        print(f"[DEBUG] 변환 중 오류 발생: {e}")
        return f"# 변환 중 오류 발생: {e}"
    

#변환 이력 저장 
def save_history(input_code, output_code, lang, filename=None, storage_type="local"):
    os.makedirs("data", exist_ok=True)
    today = datetime.now().strftime("%Y%m%d")
    file_path = f"data/history_{today}.jsonl"
    log = {
        "timestamp": datetime.now().strftime("%Y-%m-%dT%H:%M:%S"),
        "input": input_code,
        "output": output_code,
        "lang": lang
    }
    if filename:
        log["filename"] = filename

    if storage_type == "local":
        os.makedirs("data", exist_ok=True)
        today = datetime.now().strftime("%Y-%m-%d")
        filepath = os.path.join("data", f"history_{today}.jsonl")
        with open(filepath, "a", encoding="utf-8") as f:
            f.write(json.dumps(log, ensure_ascii=False) + "\n")
    elif storage_type == "blob":
        save_history_to_blob(log)

    
    
#Azure Blob Storage 저장용    
def save_history_to_blob(log):
    today = datetime.now().strftime("%Y-%m-%d")
    blob_name = f"history_{today}.jsonl"
    blob_client = blob_service_client.get_blob_client(container=container_name, blob=blob_name)

    try:
        existing_data = blob_client.download_blob().readall().decode("utf-8")
        lines = existing_data.splitlines()
    except Exception:
        lines = []

    lines.append(json.dumps(log, ensure_ascii=False))
    new_content = "\n".join(lines)

    blob_client.upload_blob(new_content, overwrite=True)