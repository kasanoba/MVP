import os
import openai  # OpenAI API 사용 위한 라이브러리
import json  # JSON 형식 입출력 처리용
from dotenv import load_dotenv, find_dotenv  # .env 파일에서 환경 변수 로드
from datetime import datetime  # 현재 날짜/시간 처리
from azure.storage.blob import BlobServiceClient  # Azure Blob Storage 연동용
import glob  # 파일 패턴 매칭 및 검색용

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

주요 함수:
- convert_cobol_code : COBOL 코드를 OpenAI GPT를 통해 변환
- save_history       : 변환 요청 이력 저장 (로컬 또는 클라우드)
- save_history_to_blob: 클라우드 저장 전용 함수
- find_existing_history: 기존 이력 중 동일 코드 및 언어 변환 결과 조회
            
"""

# COBOL 코드를 OpenAI GPT를 통해 변환
def convert_cobol_code(cobol_code: str, target_lang: str) -> str:
    """
    사용자가 입력한 COBOL 코드를 OpenAI GPT 모델을 통해 Python 또는 Java 코드로 변환합니다.
    """
    # 입력값 검증: 둘 중 하나라도 누락되면 오류 메시지 반환
    if not cobol_code or not target_lang:
        return "# 변환할 COBOL 코드와 대상 언어를 선택하세요."
    # 타겟 언어에 따른 주석 문법 결정 ('#' or '//')
    comment_prefix = "#" if target_lang.lower() == "python" else "//"

    # GPT 모델에 전달할 변환 요청 프롬프트 생성  
    prompt = (
        f"다음 입력은 COBOL 코드입니다. 이를 {target_lang.capitalize()} 코드로 변환해주세요.\n"
        "단, 입력된 텍스트가 명백히 일기, 뉴스, 일반 문장 등 자연어 기반의 글일 경우에만\n"
        f"'해당 입력은 COBOL 코드가 아닙니다.\\nCOBOL 코드만 변환이 가능합니다.'라고 응답하세요.\n"
        "**CALL 문이나 간단한 구조만으로 이루어진 코드라도 COBOL 형식을 따르면 반드시 변환하세요.**\n"
        "변환된 결과는 반드시 마크다운 코드 블럭 없이 출력하고, 전체 응답은 일반 텍스트 형식이어야 합니다.\n"
        "변환된 코드는 전체를 하나의 텍스트 블록으로, 코드 블록 마크다운(```) 없이 출력하세요.\n"
        f"코드 외의 설명은 모두 '{comment_prefix}' 주석으로 시작해서 작성해주세요.\n"
        "COBOL 주석(`*`)이 포함되어 있어도 정상적인 코드로 간주하고 변환 대상에 포함하세요.\n"
        "코드에는 필요한 최소한의 주석만 포함해주세요.\n\n"
        f"COBOL 코드:\n{cobol_code.strip()}\n"
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
        # GPT 응답에서 변환된 코드 부분만 추출해 반환
        return response.choices[0].message.content.strip()

    except Exception as e:
        # 에러 발생 시 로그 출력 및 메시지 반환
        print(f"[DEBUG] 변환 중 오류 발생: {e}")
        return f"# 변환 중 오류 발생: {e}"
    

# 변환 요청 이력 저장 (로컬 또는 클라우드)
def save_history(input_code, output_code, lang, filename=None, storage_type="로컬"):
    """
    변환 이력을 저장합니다. 로컬(jsonl) 또는 Azure Blob Storage 중 선택 가능.
    """    
    
    # 변환 이력 저장용 디렉토리 생성 (없으면 생성)
    os.makedirs("data", exist_ok=True)
    
    # 현재 날짜(년월일) 문자열 생성 (예: "20230619")
    today = datetime.now().strftime("%Y%m%d")
    # file_path = f"data/history_{today}.jsonl"
    filepath = os.path.join("data", f"history_{today}.jsonl")
    # 변환 이력 로그 항목 구조
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

        # 로컬 저장소 처리: 날짜별 JSONL 파일에 한 줄씩 append
        with open(filepath, "a", encoding="utf-8") as f:
            f.write(json.dumps(log, ensure_ascii=False) + "\n")

    elif storage_type == "클라우드(Blob 저장소)":
        # 클라우드 저장소(Azure Blob)에 저장
        save_history_to_blob(log)

    
    
# 클라우드 저장 전용 함수
def save_history_to_blob(log):
    """
    변환 이력을 Azure Blob Storage의 jsonl 파일에 저장합니다.
    """    
    # Blob 파일명: 날짜별 (예: history_2023-06-19.jsonl)
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

    # 새 로그를 JSON 문자열로 변환해 리스트에 추가
    lines.append(json.dumps(log, ensure_ascii=False))
    new_content = "\n".join(lines)

    # 이력을 blob에 업로드 (덮어쓰기 방식)
    blob_client.upload_blob(new_content, overwrite=True)
    
# 기존 이력 중 동일 코드 및 언어 변환 결과 조회
def find_existing_history(input_code, filename, storage_option, target_lang):
    """
    저장된 변환 이력에서 동일한 입력 코드 및 변환 언어가 일치하는 로그를 검색
    filename은 선택 사항이며, 있을 경우 일치 여부를 추가 확인
    """
    logs = []

    # 저장 위치별 로그 불러오기 (로컬 또는 Blob)
    if storage_option == "로컬":
        # data 폴더 내 history_*.jsonl 파일 리스트를 날짜 내림차순으로 정렬
        file_list = sorted(glob.glob("data/history_*.jsonl"), reverse=True)
        for file_path in file_list:
            try:
                with open(file_path, encoding="utf-8") as f:
                    # 파일 내 각 라인을 JSON으로 파싱해 logs 리스트에 추가
                    for line in f:
                        if line.strip():
                            logs.append(json.loads(line))
            except:
                # 파일 읽기 오류 무시하고 계속 진행
                continue
    else:
        # 클라우드 Blob Storage에서 history_*.jsonl 파일 리스트 조회 후 내용 읽기
        try:
            container_client = blob_service_client.get_container_client(container_name)
            blob_list = [blob.name for blob in container_client.list_blobs() if blob.name.startswith("history_") and blob.name.endswith(".jsonl")]
            blob_list = sorted(blob_list, reverse=True)
            for blob_name in blob_list:
                blob_client = blob_service_client.get_blob_client(container=container_name, blob=blob_name)
                content = blob_client.download_blob().readall().decode("utf-8")
                for line in content.splitlines():
                    if line.strip():
                        logs.append(json.loads(line))
        except:
            # Blob 조회 오류 시 무시
            pass

    # 불러온 로그 중에서 입력값과 언어가 동일한 로그 탐색
    for log in logs:
        if log.get("input", "").strip() == input_code.strip() and log.get("lang") == target_lang:
            # filename 조건이 있으면 비교, 없으면 무시
            if filename is None or log.get("filename") == filename:
                return log

    return None
