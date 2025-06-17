import os
from services.code_conversion_service import convert_cobol_code
from azure.storage.blob import BlobServiceClient

"""
batch_conversion_service.py

이 모듈은 COBOL 소스 파일(.cob, .cbl)의 디렉토리 구조를 분석하고,
일괄 변환(예: Python/Java 등) 기능을 제공합니다.

[주요 기능]
1. 디렉토리 현황 조회 (analyze_cobol_files)
   - 지정한 root 디렉토리 하위의 모든 폴더를 탐색하여
     각 폴더별 COBOL 파일 개수와 전체 용량을 계산합니다.

2. 일괄 변환 (batch_convert_cobol)
   - 지정한 root 디렉토리 하위의 모든 COBOL 파일을 변환하여
     output_root에 원본과 동일한 디렉토리 구조로 저장합니다.
   - 변환 진행률 콜백 및 실패 파일 목록 반환 기능을 포함합니다.
   - storage_option: "local"(로컬 저장), "blob"(Azure Blob Storage 업로드) 선택 가능
"""

# 디렉토리 현황 조회
def analyze_cobol_files(root_dir):
    cobol_exts = ('.cob', '.cbl')
    dir_info = []
    total_files = 0
    total_size = 0
    file_details = []  # (파일 경로, 파일 크기)

    for dirpath, _, filenames in os.walk(root_dir):
        count = 0
        size = 0
        for filename in filenames:
            if filename.lower().endswith(cobol_exts):
                count += 1
                fpath = os.path.join(dirpath, filename)
                try:
                    fsize = os.path.getsize(fpath)
                    size += fsize
                    file_details.append((fpath, fsize))
                except Exception:
                    pass
        if count > 0:
            dir_info.append((os.path.relpath(dirpath, root_dir), count, size))
            total_files += count
            total_size += size

    return dir_info, total_files, total_size, file_details


# 일괄 변환
def batch_convert_cobol(root_dir, target_lang, output_root, progress_callback=None, storage_option="local"):
    cobol_exts = ('.cob', '.cbl')
    ext_map = {"python": ".py", "java": ".java"}
    fail_files = []

    os.makedirs(output_root, exist_ok=True)

    # Azure Blob Storage 연결 (storage_option이 'blob'인 경우만)
    blob_service_client = None
    container_name = None
    if storage_option == "blob":
        conn_str = os.getenv("AZURE_STORAGE_CONNECTION_STRING")
        container_name = os.getenv("CONTAINER_NAME")
        if not conn_str or not container_name:
            raise ValueError("AZURE_STORAGE_CONNECTION_STRING 또는 CONTAINER_NAME 환경변수가 설정되어 있지 않습니다.")
        blob_service_client = BlobServiceClient.from_connection_string(conn_str)
        container_client = blob_service_client.get_container_client(container_name)
        if not container_client.exists():
            container_client.create_container()

    # 총 COBOL 파일 수 집계
    total_files = sum(
        len([f for f in files if f.lower().endswith(cobol_exts)])
        for _, _, files in os.walk(root_dir)
    )
    file_count = 0

    for dirpath, _, filenames in os.walk(root_dir):
        for filename in filenames:
            if filename.lower().endswith(cobol_exts):
                file_count += 1
                src_path = os.path.join(dirpath, filename)
                try:
                    with open(src_path, encoding="utf-8") as f:
                        cobol_code = f.read()

                    converted_code = convert_cobol_code(cobol_code, target_lang)

                    # 변환 결과에 오류 메시지가 포함되어 있으면 실패로 간주
                    if converted_code.startswith("# 변환 중 오류 발생"):
                        fail_files.append((src_path, converted_code.strip()))
                        continue  # 저장하지 않고 다음 파일로

                    rel_path = os.path.relpath(dirpath, root_dir)
                    new_ext = ext_map.get(target_lang, ".txt")
                    new_filename = os.path.splitext(filename)[0] + new_ext

                    if storage_option == "local":
                        save_dir = os.path.join(output_root, rel_path)
                        os.makedirs(save_dir, exist_ok=True)
                        save_path = os.path.join(save_dir, new_filename)
                        with open(save_path, "w", encoding="utf-8") as f:
                            f.write(converted_code)

                    elif storage_option == "blob":
                        blob_path = os.path.join(rel_path, new_filename).replace("\\", "/")
                        blob_client = blob_service_client.get_blob_client(container=container_name, blob=blob_path)
                        blob_client.upload_blob(converted_code, overwrite=True)

                except Exception as e:
                    fail_files.append((src_path, str(e)))

                if progress_callback:
                    progress_callback(file_count, total_files)

    return fail_files
