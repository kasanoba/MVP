import os
from services.code_conversion_service import convert_cobol_code, save_history
from azure.storage.blob import BlobServiceClient

"""
batch_conversion_service.py

이 모듈은 COBOL 소스 파일(.cob, .cbl)의 디렉토리 구조를 분석하고,
일괄 변환(예: Python/Java 등) 기능을 제공합니다.

[주요 기능]
1. 디렉토리 현황 조회 (analyze_cobol_files)
2. 일괄 변환 (batch_convert_cobol)
"""

# 디렉토리 현황 조회
def analyze_cobol_files(root_dir):
    """
    지정한 루트 디렉토리(root_dir)를 탐색하여
    - COBOL 파일(.cob, .cbl) 개수
    - 각 폴더별 파일 개수 및 크기 합계
    - 전체 파일 수 및 전체 크기 합계
    - 각 파일별 경로와 크기 리스트

    를 반환하는 함수입니다.

    Parameters:
        root_dir (str): 탐색할 루트 디렉토리 경로

    Returns:
        dir_info (list of tuples): [(상대폴더경로, 파일개수, 파일크기합), ...]
        total_files (int): 전체 COBOL 파일 수
        total_size (int): 전체 COBOL 파일 크기 합 (바이트 단위)
        file_details (list of tuples): [(파일경로, 파일크기), ...]
    """    
    cobol_exts = ('.cob', '.cbl')  # COBOL 소스 파일 확장자 목록
    dir_info = []                  # 각 폴더별 정보 저장 리스트
    total_files = 0                # 전체 파일 수 누적 변수
    total_size = 0                 # 전체 파일 크기 누적 변수
    file_details = []              # 각 파일별 (경로, 크기) 튜플 저장 리스트

    # os.walk()를 이용해 root_dir 하위 모든 폴더 및 파일 탐색
    for dirpath, _, filenames in os.walk(root_dir):
        count = 0  # 해당 폴더 내 COBOL 파일 수
        size = 0   # 해당 폴더 내 COBOL 파일 총 크기
        
        # 현재 폴더 내 모든 파일 검사
        for filename in filenames:
            # 확장자가 COBOL 소스 파일인지 확인 (대소문자 구분 없음)
            if filename.lower().endswith(cobol_exts):
                count += 1
                fpath = os.path.join(dirpath, filename)  # 절대 경로 생성
                try:
                    fsize = os.path.getsize(fpath)  # 파일 크기 바이트 단위로 얻기
                    size += fsize
                    file_details.append((fpath, fsize))  # 파일 상세 리스트에 추가
                except Exception:
                    # 파일 크기 확인 실패 시 무시하고 진행
                    pass
                
        # 해당 폴더에 COBOL 파일이 하나라도 있으면 폴더 정보 리스트에 추가
        if count > 0:
            # 상대경로로 변경하여 저장 (루트 디렉토리 기준)
            rel_dir = os.path.relpath(dirpath, root_dir)
            dir_info.append((rel_dir, count, size))
            total_files += count
            total_size += size

    # 최종 결과 반환
    return dir_info, total_files, total_size, file_details


# 일괄 변환
def batch_convert_cobol(root_dir, target_lang, output_root, progress_callback=None, storage_option="로컬"):
    """
    지정한 루트 디렉토리(root_dir) 내 COBOL 소스 파일을
    target_lang (예: python, java)으로 일괄 변환하여 output_root 경로에 저장하거나
    Azure Blob Storage에 업로드하는 함수입니다.

    변환 진행 상황을 콜백(progress_callback)으로 전달 가능하며,
    변환 성공한 파일 리스트와 실패한 파일 리스트를 반환합니다.

    Parameters:
        root_dir (str): 변환 대상 COBOL 파일이 있는 루트 디렉토리
        target_lang (str): 변환할 대상 언어 ('python', 'java' 등)
        output_root (str): 변환 결과를 저장할 로컬 출력 루트 디렉토리
        progress_callback (callable, optional): 변환 진행 상태 콜백 (현재 파일 수, 총 파일 수)
        storage_option (str): 'local' 또는 'blob' (Azure Blob Storage 업로드 여부)

    Returns:
        success_files (list): 변환 성공한 파일 경로 리스트
        fail_files (list): 변환 실패한 파일과 오류 메시지 튜플 리스트 [(파일경로, 오류메시지), ...]
    """
    cobol_exts = ('.cob', '.cbl')  # 변환 대상 확장자 지정
    ext_map = {"python": ".py", "java": ".java"}  # 대상 언어별 파일 확장자 매핑
    fail_files = []     # 변환 실패 파일 및 이유 저장 리스트
    success_files = []  # 변환 성공 파일 저장 리스트

    # 출력 루트 디렉토리가 없으면 생성
    os.makedirs(output_root, exist_ok=True)

    # Azure Blob Storage 초기화 (blob 저장 옵션인 경우)
    blob_service_client = None
    container_name = None
    if storage_option == "클라우드(Blob 저장소)":
        conn_str = os.getenv("AZURE_STORAGE_CONNECTION_STRING")  # 환경변수에서 연결 문자열 읽기
        container_name = os.getenv("CONTAINER_NAME")             # 환경변수에서 컨테이너명 읽기
        if not conn_str or not container_name:
            raise ValueError("AZURE_STORAGE_CONNECTION_STRING 또는 CONTAINER_NAME 환경변수가 설정되어 있지 않습니다.")
        # BlobServiceClient 생성
        blob_service_client = BlobServiceClient.from_connection_string(conn_str)
        container_client = blob_service_client.get_container_client(container_name)
        # 컨테이너가 없으면 생성
        if not container_client.exists():
            container_client.create_container()

    # 전체 COBOL 파일 수 집계 (진행률 계산용)
    total_files = sum(
        len([f for f in files if f.lower().endswith(cobol_exts)])
        for _, _, files in os.walk(root_dir)
    )
    file_count = 0  # 현재 처리한 파일 수 카운터

    # 루트 디렉토리 내 모든 파일 탐색
    for dirpath, _, filenames in os.walk(root_dir):
        for filename in filenames:
            if filename.lower().endswith(cobol_exts):  # COBOL 파일만 처리
                file_count += 1
                src_path = os.path.join(dirpath, filename)

                try:
                    # 원본 COBOL 코드 읽기 (UTF-8 인코딩)
                    with open(src_path, encoding="utf-8") as f:
                        cobol_code = f.read()

                    # COBOL 코드를 대상 언어 코드로 변환
                    converted_code = convert_cobol_code(cobol_code, target_lang)

                    # 변환 중 오류 발생 시 실패 리스트에 추가하고 다음 파일 처리
                    if converted_code.startswith("# 변환 중 오류 발생"):
                        fail_files.append((src_path, converted_code.strip()))
                        continue

                    # 변환된 파일 저장을 위한 경로 및 이름 생성
                    rel_path = os.path.relpath(dirpath, root_dir)  # 상대경로
                    new_ext = ext_map.get(target_lang, ".txt")     # 대상 언어 확장자
                    new_filename = os.path.splitext(filename)[0] + new_ext  # 확장자 변경

                    # 로컬 저장 옵션 처리
                    if storage_option == "로컬":
                        save_dir = os.path.join(output_root, rel_path)
                        os.makedirs(save_dir, exist_ok=True)  # 폴더가 없으면 생성
                        save_path = os.path.join(save_dir, new_filename)
                        # 변환된 코드 로컬 파일로 저장
                        with open(save_path, "w", encoding="utf-8") as f:
                            f.write(converted_code)

                        # 변환 이력 저장 (로컬 방식)
                        save_history(
                            input_code=cobol_code,
                            output_code=converted_code,
                            lang=target_lang,
                            filename=os.path.join(rel_path, filename),
                            storage_type="로컬"
                        )

                    # Azure Blob Storage 저장 옵션 처리
                    elif storage_option == "클라우드(Blob 저장소)":
                        # Blob 경로는 슬래시(/) 구분자로 변경
                        blob_path = os.path.join(rel_path, new_filename).replace("\\", "/")
                        blob_client = blob_service_client.get_blob_client(container=container_name, blob=blob_path)
                        # 변환된 코드 Blob에 업로드 (덮어쓰기)
                        blob_client.upload_blob(converted_code, overwrite=True)

                        # 변환 이력 저장 (Blob 방식)
                        save_history(
                            input_code=cobol_code,
                            output_code=converted_code,
                            lang=target_lang,
                            filename=os.path.join(rel_path, filename),
                            storage_type="클라우드(Blob 저장소)"
                        )

                    # 변환 성공한 파일 목록에 추가
                    success_files.append(src_path)

                except Exception as e:
                    # 예외 발생 시 실패 리스트에 (파일경로, 오류 메시지) 추가
                    fail_files.append((src_path, str(e)))

                # 변환 진행 상황 콜백 호출 (파일 처리 횟수, 전체 파일 수)
                if progress_callback:
                    progress_callback(file_count, total_files)

    # 변환 성공한 파일 리스트와 실패한 파일 리스트를 튜플로 반환
    return success_files, fail_files