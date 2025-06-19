import os
from dotenv import load_dotenv, find_dotenv
import streamlit as st
import glob
import json
from azure.storage.blob import BlobServiceClient
import tempfile
import zipfile
import shutil

# 단건 변환 서비스 호출
from services.code_conversion_service import convert_cobol_code, save_history
# 일괄 변환 서비스 호출
from services.batch_conversion_service import analyze_cobol_files, batch_convert_cobol


# 환경변수에서 Blob 연결 정보 읽기
conn_str = os.getenv("AZURE_STORAGE_CONNECTION_STRING")
container_name = os.getenv("CONTAINER_NAME")

# Azure Blob 클라이언트 생성 (이후 blob 접근에 사용)
blob_service_client = BlobServiceClient.from_connection_string(conn_str)


def display_log_item(item, idx):
    """
    변환 이력 항목 하나를 Streamlit UI에 보여주는 함수
    - item: 변환 이력 데이터 딕셔너리 (변환 전후 코드, 변환 언어, 타임스탬프 등 포함)
    - idx: 항목 번호 (UI 표시용)
    
    호출 위치:
    - 변환 이력 조회 탭에서 여러 변환 이력 항목을 반복 출력할 때 호출됨
    
    역할:
    - 변환된 언어, 시간, 파일명(있는 경우) 정보를 상자 스타일로 출력
    - 원본 COBOL 코드와 변환된 코드를 코드 블록으로 보여줌
    """    
    is_uploaded = "filename" in item and item["filename"]
    box_color = "#F96A4F" if is_uploaded else "#4F8BF9"
    filename_info = f"<br><span style='font-weight:bold;'>파일명: {item['filename']}</span>" if is_uploaded else ""
    st.markdown(
        f"""
        <div style="border:2px solid {box_color}; border-radius:8px; padding:16px; margin-bottom:16px; background-color:#f7fafd;">
            <b style="color:{box_color};">{idx}. 변환 언어: {item['lang']} / {item['timestamp']}</b>
            {filename_info}<br>
            <span style="font-weight:bold;">[변환 대상 COBOL 코드]</span>
        </div>
        """,
        unsafe_allow_html=True
    )
    st.code(item["input"], language="cobol")
    st.markdown(
        """
        <div style="background-color:#f7fafd; padding-bottom:8px;">
            <span style="font-weight:bold;">[변환 결과 코드]</span>
        </div>
        """,
        unsafe_allow_html=True
    )
    st.code(item["output"], language=item["lang"])


def make_zip_from_folder(folder_path, zip_path):
    """
    지정된 폴더 내 모든 파일과 하위 폴더를 ZIP 파일로 압축하는 함수
    
    호출 위치:
    - 일괄 변환 탭에서 변환 결과가 저장된 폴더를 ZIP으로 압축할 때 호출됨
    
    인자:
    - folder_path: 압축할 폴더 경로
    - zip_path: 생성할 ZIP 파일 경로
    """    
    with zipfile.ZipFile(zip_path, 'w', zipfile.ZIP_DEFLATED) as zipf:
        for root, _, files in os.walk(folder_path):
            for file in files:
                full_path = os.path.join(root, file)
                arcname = os.path.relpath(full_path, folder_path)
                zipf.write(full_path, arcname)


def main():
    """
    Streamlit 앱 메인 함수
    - 페이지 설정 및 타이틀 설정
    - Azure Blob 컨테이너 존재 여부 확인 및 생성 (최초 1회)
    - 4개의 탭 UI 구성: 언어전환, 변환 이력 조회, 일괄전환, 금융권 규정 검토
    - 각 탭별 기능 처리 및 UI 구성
    """    
    st.set_page_config(page_title="AI 기반 레거시 & 현대 개발 지원 에이전트", layout="wide")
    st.title("AI 기반 레거시 & 현대 개발 지원 에이전트")

    # Azure Blob 컨테이너 존재 확인 및 생성 처리 (앱 최초 실행시만)
    if "container_checked" not in st.session_state:
        try:
            container_client = blob_service_client.get_container_client(container_name)
            if not container_client.exists():
                container_client.create_container()
                # st.info(f"✅ Azure Blob 컨테이너 `{container_name}` 생성됨")
            # else:
                # st.info(f"🔎 Azure Blob 컨테이너 `{container_name}` 이미 존재함")
        except Exception as e:
            st.error(f"❌ 컨테이너 확인/생성 중 오류 발생: {e}")
        st.session_state["container_checked"] = True

    # 탭 이름 리스트 
    tab_titles = [
        "언어전환",
        "변환 이력 조회",
        "일괄전환",
        "금융권 규정 검토"
    ]
    # 세션 상태에 현재 탭 선택 상태 없으면 초기화 (앱 새로고침 대비)
    if "active_tab_index" not in st.session_state:
        st.session_state["active_tab_index"] = 0

    # 탭 UI 하나만 사용 (중복 출력 방지)
    tabs = st.tabs(tab_titles)

    UPLOAD_TMP_DIR = "./tmp_uploaded_zip" # 임시 ZIP 업로드 및 해제 폴더
    os.makedirs(UPLOAD_TMP_DIR, exist_ok=True) 

    # --- 탭별 UI 처리 ---
    # 1. 언어전환
    with tabs[0]:
        col_left, col_right = st.columns([1, 1]) # 좌우 2개 컬럼 분할 UI
        with col_left:
            st.subheader("COBOL 코드 입력")
            
            # 입력 방식, 저장 위치, 변환 언어 선택을 한 줄에 배치
            input_col1, input_col2, input_col3 = st.columns([1, 1, 1])
            with input_col1:
                 # COBOL 코드 입력 방식 선택: 직접 입력 또는 파일 업로드
                input_method = st.radio("입력 방식 선택", ["직접 입력", "파일 업로드"], key="input_method")
            with input_col2:
                # 변환 이력 저장 위치 선택: 로컬 또는 Azure Blob
                storage_option = st.radio("변환 이력 저장 위치 선택", ["로컬", "클라우드(Blob 저장소)"], key="storage_option")
            with input_col3:
                # 변환 언어 선택: python 또는 java
                target_lang = st.selectbox("변환 언어 선택", ["python", "java"], key="target_lang")                   

            cobol_code = ""
            uploaded_filename = None

            if input_method == "직접 입력":
                # 직접 입력인 경우 텍스트 영역으로 코드 입력 받음
                cobol_code = st.text_area("COBOL 코드", height=300, key="cobol_code")
             # 파일 업로드인 경우 업로드 파일 읽어서 텍스트 영역에 표시 (수정 가능)    
            else:
                uploaded_file = st.file_uploader("COBOL 파일 업로드", type=["cob", "cbl", "txt"], key="cobol_file")
                if uploaded_file is not None:
                    uploaded_filename = uploaded_file.name
                    try:
                        cobol_code = uploaded_file.read().decode("utf-8")
                    except Exception:
                        st.error("파일을 읽는 중 오류가 발생했습니다. 인코딩을 확인하세요.")
                        cobol_code = ""
                    cobol_code = st.text_area("업로드된 COBOL 코드(수정 가능)", cobol_code, height=300, key="uploaded_cobol_code")

        with col_right:
            st.subheader("코드 변환 결과")
            run = st.button("코드 변환 실행")
            if run:
                if not cobol_code.strip():
                    st.warning("COBOL 코드를 입력하거나 파일을 업로드하세요.")
                else:
                    with st.spinner("코드 변환 중..."):
                        # 변환 서비스 호출 (코드 변환 + 이력 저장)
                        result_code = convert_cobol_code(cobol_code, target_lang)
                        save_history(cobol_code, result_code, target_lang, uploaded_filename, storage_option)
                        st.code(result_code, language=target_lang)
            else:
                st.code("# 변환된 코드가 여기에 표시됩니다.", language=target_lang)

    # 2. 변환 이력 조회
    with tabs[1]:
        st.subheader("변환 이력 조회")

        col1, col2 = st.columns(2)
        with col1:
            # 변환 이력 저장 위치 선택 (로컬 또는 Blob)
            history_source = st.radio("이력 저장 위치 선택", ["로컬", "클라우드(Blob 저장소)"], horizontal=True, key="history_source")
        with col2:
            # 조회 유형 선택: 모두 / 건별(단일 변환) / 일괄 변환
            filter_mode = st.radio("조회 유형 선택", ["모두", "직접입력", "파일"], horizontal=True, key="history_filter_mode")

         # 조회 유형에 따라 로그 필터링 함수
        def filter_logs_by_mode(logs, mode):
            if mode == "직접입력":
                return [log for log in logs if "filename" not in log]
            elif mode == "파일":
                return [log for log in logs if "filename" in log]
            else:
                return logs

        # 로컬에 저장된 jsonl 파일 리스트 불러오기
        if history_source == "로컬":
            file_list = sorted(glob.glob("data/history_*.jsonl"), reverse=True)
            if not file_list:
                st.info("저장된 변환 이력이 없습니다.")
            else:
                # 사용자에게 파일 선택 UI 제공
                file_names = [os.path.basename(f) for f in file_list]
                selected_file = st.selectbox(
                    "이력 파일 선택",
                    options=["파일을 선택하세요"] + file_names,
                    key="history_file_select"
                )
                if selected_file == "파일을 선택하세요":
                    st.info("이력 파일을 선택하세요.")
                    st.session_state["active_tab_index"] = 1
                else:
                    file_path = os.path.join("data", selected_file)
                    try:
                        with open(file_path, encoding="utf-8") as f:
                            logs = [json.loads(line) for line in f if line.strip()]
                        filtered_logs = filter_logs_by_mode(logs, filter_mode)
                        if not filtered_logs:
                            st.info(f"{filter_mode} 유형의 변환 이력이 없습니다.")
                        else:
                            # 변환 이력 항목 하나씩 출력
                            for idx, item in enumerate(reversed(filtered_logs), 1):
                                display_log_item(item, idx)
                    except Exception as e:
                        st.error(f"이력 파일을 불러오는 중 오류가 발생했습니다: {e}")

        # Azure Blob Storage에서 변환 이력 조회
        elif history_source == "클라우드(Blob 저장소)":
            st.info("클라우드(Blob 저장소)에 저장된 변환 이력을 조회합니다.")

            try:
                container_client = blob_service_client.get_container_client(container_name)
                blob_list = [blob.name for blob in container_client.list_blobs()
                            if blob.name.startswith("history_") and blob.name.endswith(".jsonl")]
                blob_list = sorted(blob_list, reverse=True)

                if not blob_list:
                    st.info("클라우드(Blob 저장소)에 저장된 변환 이력이 없습니다.")
                else:
                    # Blob 이력 파일 선택 UI
                    selected_blob = st.selectbox(
                        "Blob 이력 파일 선택",
                        options=["파일을 선택하세요"] + blob_list,
                        key="blob_history_file_select"
                    )

                    if selected_blob == "파일을 선택하세요":
                        st.info("이력 파일을 선택하세요.")
                    else:
                        blob_client = blob_service_client.get_blob_client(container=container_name, blob=selected_blob)
                        downloaded_bytes = blob_client.download_blob().readall()
                        content = downloaded_bytes.decode("utf-8")
                        logs = [json.loads(line) for line in content.splitlines() if line.strip()]
                        filtered_logs = filter_logs_by_mode(logs, filter_mode)

                        if not filtered_logs:
                            st.info(f"{filter_mode} 유형의 변환 이력이 없습니다.")
                        else:
                            for idx, item in enumerate(reversed(filtered_logs), 1):
                                display_log_item(item, idx)
            except Exception as e:
                st.error(f"클라우드(Blob 저장소)에서 변환 이력을 불러오는 중 오류가 발생했습니다: {e}")

    # 3. 일괄전환
    with tabs[2]:
        st.header("COBOL 소스 ZIP 업로드 및 일괄 변환")

        # ZIP 파일 업로드 받음 (COBOL 소스 파일 묶음)
        uploaded_zip = st.file_uploader("COBOL 소스 ZIP 파일 업로드", type=["zip"])

        if uploaded_zip:
            st.session_state["active_tab_index"] = 2  # 탭 활성화 유지
            zip_path = os.path.join(UPLOAD_TMP_DIR, "uploaded.zip")
            with open(zip_path, "wb") as f:
                f.write(uploaded_zip.read())  # 업로드된 ZIP 파일 저장

            extract_dir = os.path.join(UPLOAD_TMP_DIR, "extracted")
            if os.path.exists(extract_dir):
                shutil.rmtree(extract_dir)  # 기존 해제 폴더 삭제
            os.makedirs(extract_dir)  # 새로 폴더 생성

            try:
                with zipfile.ZipFile(zip_path, 'r') as zip_ref:
                    zip_ref.extractall(extract_dir)   # ZIP 해제
            except zipfile.BadZipFile:
                st.error("유효한 ZIP 파일을 업로드하세요.")
                st.stop()

            # 해제된 폴더 내 서브폴더가 1개면 그걸 root_dir로, 아니면 바로 extract_dir로 설정
            subfolders = [f.path for f in os.scandir(extract_dir) if f.is_dir()]
            root_dir = subfolders[0] if len(subfolders) == 1 else extract_dir

            # batch_conversion_service의 분석 함수 호출: 폴더별 통계, 총 파일 수, 토큰 수, 파일별 토큰 수 리스트 반환
            stats = analyze_cobol_files(root_dir)

            folder_stats, total_files, total_tokens, file_token_list = stats

            st.markdown("### 분석 결과")

            st.markdown("#### 폴더별 파일 수 및 토큰 수")
            folder_data = {
                "폴더명": [f[0] for f in folder_stats],
                "파일 수": [f[1] for f in folder_stats],
                "토큰 수": [f[2] for f in folder_stats]
            }
            st.table(folder_data)

            st.markdown(f"**전체 파일 수:** {total_files}")
            st.markdown(f"**전체 토큰 수:** {total_tokens}")
            
            # 예상 비용 계산 (예: 1000 토큰 당 0.002 USD)
            cost_per_1000_tokens = 0.002
            estimated_cost = (total_tokens / 1000) * cost_per_1000_tokens
            st.markdown(f"**예상 비용: ${estimated_cost:.4f} (1000 토큰 당 ${cost_per_1000_tokens}) (예: 1000 토큰 당 0.002 USD)**")
            
            st.markdown("#### 파일별 토큰 수")
            file_data = {
                "파일 경로": [os.path.relpath(f[0], root_dir) for f in file_token_list],
                "토큰 수": [f[1] for f in file_token_list]
            }
            st.table(file_data)

            target_lang_batch = st.selectbox("일괄 변환 언어 선택", ["python", "java"])

            if st.button("일괄 변환 실행"):
                with st.spinner("일괄 변환 중..."):
                    output_dir = "./batch_output"
                    if os.path.exists(output_dir):
                        shutil.rmtree(output_dir)
                    os.makedirs(output_dir)

                    try:
                        # converted_files = batch_convert_cobol(root_dir, target_lang_batch, output_dir)
                        # st.success(f"총 {len(converted_files)}개 파일 변환 완료!")
                        success_files, fail_files = batch_convert_cobol(root_dir, target_lang_batch, output_dir)
                        st.success(f"총 {len(success_files)}개 파일 변환 완료!")

                        # 변환 결과 ZIP 압축 제공
                        zip_out_path = "./batch_output.zip"
                        make_zip_from_folder(output_dir, zip_out_path)
                        with open(zip_out_path, "rb") as f:
                            st.download_button(
                                label="변환 결과 ZIP 다운로드",
                                data=f,
                                file_name="converted_code.zip",
                                mime="application/zip"
                            )
                    except Exception as e:
                        st.error(f"일괄 변환 중 오류 발생: {e}")

    # 4. 금융권 규정 검토 탭 (비워둠)
    with tabs[3]:
        st.header("금융권 규정 검토")
        st.info("준비 중인 기능입니다.")

if __name__ == "__main__":
    load_dotenv(find_dotenv())
    main()
