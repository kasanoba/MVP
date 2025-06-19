# 표준 라이브러리 및 외부 패키지 임포트
import os
from dotenv import load_dotenv, find_dotenv  # 환경 변수 로드용
import streamlit as st  # Streamlit UI 라이브러리
import glob
import json
from azure.storage.blob import BlobServiceClient  # Azure Blob 저장소 연동
import tempfile
import zipfile
import shutil

# 변환 관련 서비스 모듈 임포트 (단건, 일괄 변환 등)
from services.code_conversion_service import convert_cobol_code, save_history, find_existing_history
from services.batch_conversion_service import analyze_cobol_files, batch_convert_cobol

# Azure Blob 연결 정보 불러오기 (.env 파일 기반)
conn_str = os.getenv("AZURE_STORAGE_CONNECTION_STRING")
container_name = os.getenv("CONTAINER_NAME")

# Azure Blob 클라이언트 인스턴스 생성 (컨테이너 접근용)
blob_service_client = BlobServiceClient.from_connection_string(conn_str)

# 변환 이력을 시각적으로 표시하는 함수
def display_log_item(item, idx):
    """
    변환 이력 항목 하나를 Streamlit UI에 출력
    """
    is_uploaded = "filename" in item and item["filename"]  # 파일 기반 여부 체크
    box_color = "#F96A4F" if is_uploaded else "#4F8BF9"  # 색상 조건부 처리
    filename_info = f"<br><span style='font-weight:bold;'>파일명: {item['filename']}</span>" if is_uploaded else ""
    
    # 이력 요약 정보 출력
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
    st.code(item["input"], language="cobol")  # 원본 COBOL 코드
    st.markdown(
        """
        <div style="background-color:#f7fafd; padding-bottom:8px;">
            <span style="font-weight:bold;">[변환 결과 코드]</span>
        </div>
        """,
        unsafe_allow_html=True
    )
    st.code(item["output"], language=item["lang"])  # 변환된 결과

# 폴더 내 모든 파일을 ZIP으로 압축
def make_zip_from_folder(folder_path, zip_path):
    """
    지정된 폴더와 하위 파일 전체를 ZIP 파일로 압축
    """
    with zipfile.ZipFile(zip_path, 'w', zipfile.ZIP_DEFLATED) as zipf:
        for root, _, files in os.walk(folder_path):
            for file in files:
                full_path = os.path.join(root, file)
                arcname = os.path.relpath(full_path, folder_path)
                zipf.write(full_path, arcname)

# Streamlit 앱 메인 함수 정의
def main():
    """
    Streamlit 앱 실행 진입점 함수
    """
    st.set_page_config(page_title="AI 기반 레거시 & 현대 개발 지원 에이전트", layout="wide")
    st.title("AI 기반 레거시 & 현대 개발 지원 에이전트")

    # Blob 컨테이너 존재 확인 및 필요 시 생성 (최초 1회)
    if "container_checked" not in st.session_state:
        with st.spinner("☁️ Azure Blob 컨테이너 상태 확인 중..."):
            try:
                container_client = blob_service_client.get_container_client(container_name)
                if not container_client.exists():
                    container_client.create_container()
            except Exception as e:
                st.error(f"컨테이너 확인/생성 중 오류 발생: {e}")
            st.session_state["container_checked"] = True

    # 탭 목록 정의 및 초기 탭 인덱스 설정
    tab_titles = ["언어전환", "변환 이력 조회", "일괄전환", "금융권 규정 검토"]
    if "active_tab_index" not in st.session_state:
        st.session_state["active_tab_index"] = 0

    # 탭을 Streamlit radio 버튼으로 대체 (상태 유지)
    selected_tab = st.radio(
        "탭 선택",
        tab_titles,
        index=st.session_state["active_tab_index"],
        horizontal=True
    )
    st.session_state["active_tab_index"] = tab_titles.index(selected_tab)

    # 업로드 ZIP 저장 임시 디렉토리 생성
    UPLOAD_TMP_DIR = "./tmp_uploaded_zip"
    os.makedirs(UPLOAD_TMP_DIR, exist_ok=True)

    # 각 탭에 따른 UI 분기 처리
    if selected_tab == "언어전환":
        # === 단건 언어 변환 기능 ===
        col_left, col_right = st.columns([1, 1])
        with col_left:
            st.subheader("COBOL 코드 입력")

            # 입력방식, 저장위치, 변환언어 선택 UI
            input_col1, input_col2, input_col3 = st.columns([1, 1, 1])
            with input_col1:
                input_method = st.radio("입력 방식 선택", ["직접 입력", "파일 업로드"], key="input_method")
            with input_col2:
                storage_option = st.radio("변환 이력 저장 위치 선택", ["로컬", "클라우드(Blob 저장소)"], key="storage_option")
            with input_col3:
                target_lang = st.selectbox("변환 언어 선택", ["python", "java"], key="target_lang")

            cobol_code = ""
            uploaded_filename = None

            # 입력 방식에 따른 COBOL 코드 수집
            if input_method == "직접 입력":
                cobol_code = st.text_area("COBOL 코드", height=300, key="cobol_code")
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

            # 세션 상태 초기화
            if "existing_log" not in st.session_state:
                st.session_state["existing_log"] = None
            if "result_code" not in st.session_state:
                st.session_state["result_code"] = None

            # 변환 실행 버튼
            run = st.button("코드 변환 실행")
            if run:
                if not cobol_code.strip():
                    st.warning("COBOL 코드를 입력하거나 파일을 업로드하세요.")
                    st.session_state["existing_log"] = None
                    st.session_state["result_code"] = None
                else:
                    # 기존 이력 확인
                    with st.spinner("기존 이력 확인 중..."):
                        existing_log = find_existing_history(cobol_code, uploaded_filename, storage_option, target_lang)

                        if existing_log:
                            # 기존 이력 존재 시 결과 출력
                            st.session_state["existing_log"] = existing_log
                            st.session_state["result_code"] = None
                        else:
                            # 이력 없음 → 변환 실행
                            result_code = convert_cobol_code(cobol_code, target_lang)
                            save_history(cobol_code, result_code, target_lang, uploaded_filename, storage_option)
                            st.session_state["existing_log"] = None
                            st.session_state["result_code"] = result_code

            # 결과 출력
            if st.session_state["existing_log"]:
                st.success("✅ 동일한 입력에 대한 기존 변환 결과를 찾았습니다.")
                st.code(st.session_state["existing_log"]["output"], language=target_lang)
                re_run = st.button("⟳ 이 코드를 새롭게 변환")
                if re_run:
                    with st.spinner("코드 재변환 중..."):
                        result_code = convert_cobol_code(cobol_code, target_lang)
                        save_history(cobol_code, result_code, target_lang, uploaded_filename, storage_option)
                        st.session_state["result_code"] = result_code
                        st.session_state["existing_log"] = None
                    st.code(st.session_state["result_code"], language=target_lang)

            elif st.session_state["result_code"]:
                st.code(st.session_state["result_code"], language=target_lang)
            else:
                st.info("변환을 실행하면 결과가 여기에 표시됩니다.")

    elif selected_tab == "변환 이력 조회":
        # === 변환 이력 조회 탭 ===
        st.subheader("변환 이력 조회")
        col1, col2 = st.columns(2)
        with col1:
            history_source = st.radio("이력 저장 위치 선택", ["로컬", "클라우드(Blob 저장소)"], horizontal=True, key="history_source")
        with col2:
            filter_mode = st.radio("조회 유형 선택", ["모두", "직접입력", "파일"], horizontal=True, key="history_filter_mode")

        # 필터 함수 정의
        def filter_logs_by_mode(logs, mode):
            if mode == "직접입력":
                return [log for log in logs if "filename" not in log]
            elif mode == "파일":
                return [log for log in logs if "filename" in log]
            else:
                return logs

        # 로컬 이력 조회
        if history_source == "로컬":
            file_list = sorted(glob.glob("data/history_*.jsonl"), reverse=True)
            if not file_list:
                st.info("저장된 변환 이력이 없습니다.")
            else:
                file_names = [os.path.basename(f) for f in file_list]
                selected_file = st.selectbox("이력 파일 선택", options=["파일을 선택하세요"] + file_names, key="history_file_select")
                if selected_file == "파일을 선택하세요":
                    st.warning("이력 파일을 선택해야 변환 이력을 볼 수 있습니다.")
                else:
                    file_path = os.path.join("data", selected_file)
                    try:
                        with open(file_path, encoding="utf-8") as f:
                            logs = [json.loads(line) for line in f if line.strip()]
                        filtered_logs = filter_logs_by_mode(logs, filter_mode)
                        if not filtered_logs:
                            st.info(f"{filter_mode} 유형의 변환 이력이 없습니다.")
                        else:
                            for idx, item in enumerate(reversed(filtered_logs), 1):
                                display_log_item(item, idx)
                    except Exception as e:
                        st.error(f"이력 파일을 불러오는 중 오류 발생: {e}")

        # Blob 이력 조회
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
                    selected_blob = st.selectbox("Blob 이력 파일 선택", options=["파일을 선택하세요"] + blob_list, key="blob_history_file_select")
                    if selected_blob != "파일을 선택하세요":
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
                st.error(f"클라우드(Blob 저장소)에서 변환 이력을 불러오는 중 오류 발생: {e}")

    elif selected_tab == "일괄전환":
        # === 일괄 변환 탭 ===
        st.header("COBOL 소스 ZIP 업로드 및 일괄 변환")
        uploaded_zip = st.file_uploader("COBOL 소스 ZIP 파일 업로드", type=["zip"])

        if uploaded_zip:
            zip_path = os.path.join(UPLOAD_TMP_DIR, "uploaded.zip")
            with open(zip_path, "wb") as f:
                f.write(uploaded_zip.read())

            extract_dir = os.path.join(UPLOAD_TMP_DIR, "extracted")
            if os.path.exists(extract_dir):
                shutil.rmtree(extract_dir)
            os.makedirs(extract_dir)

            try:
                with zipfile.ZipFile(zip_path, 'r') as zip_ref:
                    zip_ref.extractall(extract_dir)
            except zipfile.BadZipFile:
                st.error("유효한 ZIP 파일을 업로드하세요.")
                return

            subfolders = [f.path for f in os.scandir(extract_dir) if f.is_dir()]
            root_dir = subfolders[0] if len(subfolders) == 1 else extract_dir

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

            cost_per_1000_tokens = 0.002
            estimated_cost = (total_tokens / 1000) * cost_per_1000_tokens
            st.markdown(f"**예상 비용: ${estimated_cost:.4f} (1000 토큰 당 ${cost_per_1000_tokens})**")

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
                        success_files, fail_files = batch_convert_cobol(root_dir, target_lang_batch, output_dir)
                        st.success(f"총 {len(success_files)}개 파일 변환 완료!")

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

    elif selected_tab == "금융권 규정 검토":
        # === 준비 중 기능 탭 ===
        st.header("금융권 규정 검토")
        st.info("준비 중인 기능입니다.")


# 앱 실행 시 진입점
if __name__ == "__main__":
    load_dotenv(find_dotenv())  # 환경 변수 로드
    main()  # 앱 실행
