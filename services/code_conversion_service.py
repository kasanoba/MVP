import os
import openai  # OpenAI API 사용
import json
from dotenv import load_dotenv, find_dotenv  # 환경 변수(.env 파일)를 불러오기 위한 라이브러리
from datetime import datetime  # 날짜와 시간 관련 기능
from azure.storage.blob import BlobServiceClient  # Azure Blob 저장소 연동


# .env 파일을 찾아서 환경 변수로 로드 (API 키 등 외부 노출 금지 정보를 보관)
load_dotenv(find_dotenv())

# Azure OpenAI 설정용 환경 변수 불러오기
AZURE_OPENAI_ENDPOINT = os.getenv("AZURE_OPENAI_ENDPOINT")
AZURE_OPENAI_API_KEY = os.getenv("AZURE_OPENAI_API_KEY")
DEPLOYMENT_NAME = os.getenv("DEPLOYMENT_NAME")

# OpenAI 라이브러리에 Azure OpenAI 설정값 적용
openai.api_type = "azure"
openai.azure_endpoint = AZURE_OPENAI_ENDPOINT
openai.api_version = "2024-12-01-preview"  # 사용할 API 버전
openai.api_key = AZURE_OPENAI_API_KEY

# Azure Blob Storage 연결 설정
conn_str = os.getenv("AZURE_STORAGE_CONNECTION_STRING")  # 연결 문자열
container_name = os.getenv("CONTAINER_NAME")             # 컨테이너 이름

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
    # 입력값 검증: 둘 중 하나라도 누락되면 오류 메시지 반환
    if not cobol_code or not target_lang:
        return "# 변환할 COBOL 코드와 대상 언어를 선택하세요."

    # GPT 모델에 전달할 변환 요청 프롬프트 생성  
    prompt = (
        f"다음 입력은 COBOL 코드입니다. 이를 {target_lang.capitalize()} 코드로 변환해주세요.\n"
        "입력된 텍스트가 명백하게 COBOL 코드와 무관한 일반 문장(예: 일기, 뉴스 등)이라면, "
        "'해당 입력은 COBOL 코드가 아닙니다.\\nCOBOL 코드만 변환이 가능합니다.'라고만 응답하세요.\n"
        "COBOL 주석(`*`)이 있는 경우는 정상적인 코드로 간주하고 변환 대상에 포함하세요.\n"
        "변환된 코드는 전체를 하나의 텍스트 블록으로, 코드 블록 마크다운(```) 없이 출력하세요.\n"
        "최소한의 주석만 포함하며, 설명이 필요한 경우 주석(`#`)으로 표시하세요.\n\n"
        f"COBOL 코드:\n{cobol_code}\n"
    )
    try:
        # OpenAI Chat API 호출
        response = openai.chat.completions.create(
            model=DEPLOYMENT_NAME,  # 배포된 Azure GPT 모델 이름
            messages=[
                {"role": "system", "content": "You are a helpful AI developer assistant."},
                {"role": "user", "content": prompt}
            ],
            max_tokens=1000,     # 반환될 응답 최대 토큰 수
            temperature=0.7     # 창의성 정도 (높을수록 더 다양한 표현)
        )
        # 반환된 결과 텍스트 추출 및 반환
        return response.choices[0].message.content.strip()

    except Exception as e:
        # 에러 발생 시 로그 출력 및 메시지 반환
        print(f"[DEBUG] 변환 중 오류 발생: {e}")
        return f"# 변환 중 오류 발생: {e}"
    

#변환 이력 저장 
def save_history(input_code, output_code, lang, filename=None, storage_type="로컬"):
    """
    변환 이력을 저장합니다. 로컬(jsonl) 또는 Azure Blob Storage 중 선택 가능.
    """    
    os.makedirs("data", exist_ok=True)
    today = datetime.now().strftime("%Y%m%d")
    # file_path = f"data/history_{today}.jsonl"
    filepath = os.path.join("data", f"history_{today}.jsonl")
    # 공통 로그 구조 생성
    log = {
        "timestamp": datetime.now().strftime("%Y-%m-%d %H:%M:%S"),  # 현재 시간
        "input": input_code,    # 입력된 COBOL 코드
        "output": output_code,  # 변환된 코드
        "lang": lang            # 대상 언어
    }
    if filename:
        log["filename"] = filename  # 원본 파일 이름이 있으면 포함

    if storage_type == "로컬":
        # 로컬 저장소: data 폴더 아래 jsonl 파일 생성 (날짜별)
        os.makedirs("data", exist_ok=True)
        today = datetime.now().strftime("%Y-%m-%d")
        filepath = os.path.join("data", f"history_{today}.jsonl")

        # jsonl 형식으로 파일에 한 줄씩 기록
        with open(filepath, "a", encoding="utf-8") as f:
            f.write(json.dumps(log, ensure_ascii=False) + "\n")

    elif storage_type == "클라우드(Blob 저장소)":
        # Azure Blob 저장소에 저장 요청
        save_history_to_blob(log)

    
    
#Azure Blob Storage 저장용    
def save_history_to_blob(log):
    """
    변환 이력을 Azure Blob Storage의 jsonl 파일에 저장합니다.
    """    
    # 저장할 blob 파일 이름 설정 (날짜별)
    today = datetime.now().strftime("%Y-%m-%d")
    blob_name = f"history_{today}.jsonl"

    # 해당 blob에 접근하기 위한 클라이언트 생성
    blob_client = blob_service_client.get_blob_client(container=container_name, blob=blob_name)

    try:
        # 기존 blob에서 내용을 다운로드 (이력이 있으면 병합하기 위함)
        existing_data = blob_client.download_blob().readall().decode("utf-8")
        lines = existing_data.splitlines()
    except Exception:
        # 기존 blob이 없으면 빈 리스트로 초기화
        lines = []

    # 새로운 이력을 추가
    lines.append(json.dumps(log, ensure_ascii=False))
    new_content = "\n".join(lines)

    # 이력을 blob에 업로드 (덮어쓰기 방식)
    blob_client.upload_blob(new_content, overwrite=True)