import os
from dotenv import load_dotenv
import streamlit as st
import glob
import json


# .env 파일에서 환경 변수 로드
load_dotenv()

# 환경 변수 값 불러오기
OPENAI_API_KEY = os.getenv("OPENAI_API_KEY")
OPENAI_ENDPOINT = os.getenv("OPENAI_ENDPOINT")
CHAT_MODEL = os.getenv("CHAT_MODEL")

# 값이 잘 불러와졌는지 확인 (개발 중에만 사용, 실제 서비스 시 삭제)
print("OPENAI_API_KEY:", OPENAI_API_KEY)
print("OPENAI_ENDPOINT:", OPENAI_ENDPOINT)
print("CHAT_MODEL:", CHAT_MODEL)

# 단건 변환 서비스 호출
from services.code_conversion_service import convert_cobol_code
# 일괄 변환 서비스 호출
from services.batch_conversion_service import analyze_cobol_files, batch_convert_cobol

"""
[주요 기능]
1. 언어전환 (단일 COBOL 코드 변환)
   - COBOL 코드를 직접 입력하거나 파일로 업로드하여 Python/Java 등으로 변환

2. 변환 이력 조회
   - 변환 이력 파일을 선택하여 과거 변환 내역을 조회

3. 일괄전환
   - 디렉토리 전체 COBOL 파일을 일괄 변환
   - 디렉토리 현황 조회, 예상 비용 안내, 진행률/실패 파일 안내 등 포함

4. 금융권 규정 검토 (추후 구현)
   - 금융권 관련 규정 검토 기능

[구성]
- 각 탭별로 Streamlit UI 위젯 및 서비스 함수 호출
- 서비스 레이어와 분리된 구조로 유지보수 용이
"""

def main():
    # 페이지 타이틀 및 레이아웃 설정
    st.set_page_config(page_title="AI 기반 레거시 & 현대 개발 지원 에이전트", layout="wide")
    st.title("AI 기반 레거시 & 현대 개발 지원 에이전트")

    tab1, tab2, tab3, tab4 = st.tabs([
        "언어전환",
        "변환 이력 조회",
        "일괄전환",
        "금융권 규정 검토"
    ])

    # 1. 언어전환 탭: COBOL 코드 입력 및 변환
    with tab1:
        col_left, col_right = st.columns([1, 1])
        with col_left:
            st.subheader("COBOL 코드 입력")
            input_method = st.radio("입력 방식 선택", ["직접 입력", "파일 업로드"], key="input_method")
            cobol_code = ""
            if input_method == "직접 입력":
                cobol_code = st.text_area("COBOL 코드", height=300, key="cobol_code")
            else:
                uploaded_file = st.file_uploader("COBOL 파일 업로드", type=["cob", "txt"], key="cobol_file")
                if uploaded_file is not None:
                    try:
                        cobol_code = uploaded_file.read().decode("utf-8")
                    except Exception:
                        st.error("파일을 읽는 중 오류가 발생했습니다. 인코딩을 확인하세요.")
                        cobol_code = ""
                    cobol_code = st.text_area("업로드된 COBOL 코드(수정 가능)", cobol_code, height=300, key="uploaded_cobol_code")
            target_lang = st.selectbox("변환 언어 선택", ["python", "java"], key="target_lang")
        with col_right:
            st.subheader("코드 변환 결과")
            run = st.button("코드 변환 실행")
            if run:
                if not cobol_code.strip():
                    st.warning("COBOL 코드를 입력하거나 파일을 업로드하세요.")
                else:
                    with st.spinner("코드 변환 중..."):
                        # 서비스 함수 호출 부분
                        result_code = convert_cobol_code(cobol_code, target_lang)
                        result_code = "# (예시) 변환된 코드가 여기에 표시됩니다."
                        st.code(result_code, language=target_lang)
            else:
                st.code("# 변환된 코드가 여기에 표시됩니다.", language=target_lang)

    # 2. 변환 이력 조회 탭: 변환 이력 파일 선택 및 조회
    with tab2:
        st.subheader("변환 이력 조회")
        # data 폴더에서 이력 파일 목록 가져오기
        file_list = sorted(glob.glob("data/history_*.jsonl"), reverse=True)
        if not file_list:
            st.info("저장된 변환 이력이 없습니다.")
        else:
            file_names = [os.path.basename(f) for f in file_list]
            # 최초에는 None, 이후에는 session_state에 저장
            selected_file = st.selectbox(
                "이력 파일 선택",
                options=["파일을 선택하세요"] + file_names,
                key="history_file_select"
            )

            if selected_file == "파일을 선택하세요":
                st.info("이력 파일을 선택하세요.")
            else:
                file_path = os.path.join("data", selected_file)
                with open(file_path, encoding="utf-8") as f:
                    logs = [json.loads(line) for line in f]
                if not logs:
                    st.info("해당 파일에 변환 이력이 없습니다.")
                else:
                    for idx, item in enumerate(reversed(logs), 1):
                        with st.container():
                            st.markdown(
                                f"""
                                <div style="border:2px solid #4F8BF9; border-radius:8px; padding:16px; margin-bottom:16px; background-color:#f7fafd;">
                                    <b style="color:#4F8BF9;">{idx}. 변환 언어: {item['lang']} / {item['timestamp']}</b><br>
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

    # 3. 일괄전환 탭: 디렉토리 전체 COBOL 파일 일괄 변환
    with tab3:
        st.subheader("일괄전환")
        root_dir = st.text_input("변환할 COBOL 소스 그룹의 root 디렉토리 경로를 입력하세요.")
        st.markdown("""
        **[디렉토리 현황 조회 시 아래 항목이 표시됩니다]**
        - 전체 COBOL 파일 수
        - 전체 용량
        - 디렉토리별 파일 개수 및 용량
        - 예상 총 토큰/예상 비용
        """)
        dir_status_clicked = st.button("디렉토리 현황 조회")
        target_lang = st.selectbox("변환 언어 선택", ["python", "java"], key="batch_target_lang")
        output_root = st.text_input("변환된 파일을 저장할 root 디렉토리 경로를 입력하세요.", value="converted_result")

        # 1. 디렉토리 현황 조회
        if dir_status_clicked:
            if not root_dir or not os.path.isdir(root_dir):
                st.error("유효한 root 디렉토리 경로를 입력하세요.")
            else:
                with st.spinner("디렉토리 현황을 분석 중입니다..."):
                    dir_info, total_files, total_size = analyze_cobol_files(root_dir)
                st.write(f"**전체 COBOL 파일 수:** {total_files}")
                st.write(f"**전체 용량:** {total_size/1024:.2f} KB")
                st.write("**디렉토리별 파일 개수:**")
                for rel_path, count, size in dir_info:
                    st.write(f"- `{rel_path}`: {count}개 ({size/1024:.2f} KB)")
                avg_tokens_per_file = 2000  # 예시값
                total_tokens = total_files * avg_tokens_per_file
                price_per_1k = 0.005  # 예시: 1K tokens당 $0.005
                estimated_cost = (total_tokens / 1000) * price_per_1k
                st.info(f"예상 총 토큰: {total_tokens:,} / 예상 비용: ${estimated_cost:.2f} (실제는 파일 크기/내용에 따라 달라질 수 있습니다)")

        # 2. 일괄 변환 실행
        if st.button("일괄 변환 실행"):
            if not root_dir or not os.path.isdir(root_dir):
                st.error("유효한 root 디렉토리 경로를 입력하세요.")
            else:
                progress_bar = st.progress(0, text="일괄 변환 진행 중...")
                def progress_callback(done, total):
                    progress_bar.progress(done / total, text=f"{done}/{total} 변환 완료")
                with st.spinner("일괄 변환을 처리 중입니다..."):
                    fail_files = batch_convert_cobol(root_dir, target_lang, output_root, progress_callback)
                    progress_bar.empty()
                    if not fail_files:
                        st.success("일괄 변환이 완료되었습니다!")
                    else:
                        st.warning(f"일부 파일 변환 실패: {len(fail_files)}건")
                        st.markdown("**실패 파일 목록:**")
                        for f, err in fail_files:
                            st.write(f"- `{f}` / 오류: {err}")
                    st.info(f"변환 결과는 '{output_root}' 폴더에 저장됩니다.")
    # 4. 금융권 규정 검토 탭 (추후 구현)
    with tab4:
        st.subheader("금융권 규정 검토")
        st.info("금융권 규정 검토 기능은 추후 구현 예정입니다.")