import os
from services.code_conversion_service import convert_cobol_code

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

[함수별 설명]
def analyze_cobol_files(root_dir: str) -> tuple[list[tuple[str, int, int]], int, int]
    - 입력: root_dir (분석할 최상위 디렉토리 경로)
    - 출력: (dir_info, total_files, total_size)
        dir_info: [(상대경로, 파일개수, 용량), ...]
        total_files: 전체 COBOL 파일 수
        total_size: 전체 COBOL 파일 용량(바이트)

def batch_convert_cobol(
    root_dir: str,
    target_lang: str,
    output_root: str,
    progress_callback: callable = None
) -> list[tuple[str, str]]
    - 입력:
        root_dir: 변환할 COBOL 소스 그룹의 최상위 디렉토리 경로
        target_lang: 변환 언어 (예: "python", "java")
        output_root: 변환된 파일을 저장할 최상위 디렉토리 경로
        progress_callback: 진행률 표시용 콜백 함수(옵션, 호출 시 progress_callback(진행수, 전체수))
    - 출력:
        fail_files: [(실패파일경로, 오류메시지), ...]
"""

#디렉토리 현황 조회 (analyze_cobol_files)
def analyze_cobol_files(root_dir):

    cobol_exts = ('.cob', '.cbl')
    dir_info = []
    total_files = 0
    total_size = 0

    for dirpath, _, filenames in os.walk(root_dir):
        count = 0
        size = 0
        for filename in filenames:
            if filename.lower().endswith(cobol_exts):
                count += 1
                fpath = os.path.join(dirpath, filename)
                try:
                    size += os.path.getsize(fpath)  # 파일 크기 누적
                except Exception:
                    pass  # 파일 크기 측정 실패 시 무시
        if count > 0:
            # (상대경로, 파일개수, 용량) 정보 저장
            dir_info.append((os.path.relpath(dirpath, root_dir), count, size))
            total_files += count
            total_size += size
    return dir_info, total_files, total_size

#일괄 변환 (batch_convert_cobol)
def batch_convert_cobol(root_dir, target_lang, output_root, progress_callback=None):
    
    cobol_exts = ('.cob', '.cbl')
    ext_map = {"python": ".py", "java": ".java"}
    fail_files = []

    os.makedirs(output_root, exist_ok=True)

    # 전체 파일 개수 세기 (진행률 계산용)
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
                    # COBOL 파일 읽기
                    with open(src_path, encoding="utf-8") as f:
                        cobol_code = f.read()
                    # 코드 변환 (GPT API 호출)
                    converted_code = convert_cobol_code(cobol_code, target_lang)
                    # 저장 경로 생성 (원본 구조 유지)
                    rel_path = os.path.relpath(dirpath, root_dir)
                    save_dir = os.path.join(output_root, rel_path)
                    os.makedirs(save_dir, exist_ok=True)
                    new_ext = ext_map.get(target_lang, ".txt")
                    save_path = os.path.join(save_dir, os.path.splitext(filename)[0] + new_ext)
                    # 변환된 코드 저장
                    with open(save_path, "w", encoding="utf-8") as f:
                        f.write(converted_code)
                except Exception as e:
                    # 실패 파일 기록
                    fail_files.append((src_path, str(e)))
                # 진행률 콜백 호출 (UI 연동용)
                if progress_callback:
                    progress_callback(file_count, total_files)
    return fail_files