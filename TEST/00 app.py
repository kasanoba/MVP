import os
from dotenv import load_dotenv
import streamlit as st

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

# 페이지 타이틀 및 레이아웃 설정
st.set_page_config(page_title="AI 기반 레거시 & 현대 개발 지원 에이전트", layout="wide")

# 상단 타이틀 출력
st.title("AI 기반 레거시 & 현대 개발 지원 에이전트")

# 상단 탭 메뉴 생성
tab1, tab2, tab3, tab4 = st.tabs(["언어전환", "코드설명", "테스트 코드", "금융권 규정 검토"])

with tab1:
    # 좌우 2분할 컬럼 생성
    col_left, col_right = st.columns([1, 1])
    with col_left:
        st.subheader("COBOL 코드 입력")
        # 입력 방식 선택 라디오 버튼
        input_method = st.radio("입력 방식 선택", ["직접 입력", "파일 업로드"], key="input_method")
        cobol_code = ""
        if input_method == "직접 입력":
            # 직접 입력용 텍스트 영역
            cobol_code = st.text_area("COBOL 코드", height=300, key="cobol_code")
        else:
            # 파일 업로드 위젯
            uploaded_file = st.file_uploader("COBOL 파일 업로드", type=["cob", "txt"], key="cobol_file")
            if uploaded_file is not None:
                try:
                    # 파일 내용 읽기 및 디코딩
                    cobol_code = uploaded_file.read().decode("utf-8")
                except Exception:
                    # 인코딩 오류 안내
                    st.error("파일을 읽는 중 오류가 발생했습니다. 인코딩을 확인하세요.")
                    cobol_code = ""
                # 업로드된 코드도 수정 가능하게 텍스트 영역 제공
                cobol_code = st.text_area("업로드된 COBOL 코드(수정 가능)", cobol_code, height=300, key="uploaded_cobol_code")
        # 변환 언어 선택 드롭다운
        target_lang = st.selectbox("변환 언어 선택", ["python", "java"], key="target_lang")
    with col_right:
        st.subheader("코드 변환 결과")
        # 코드 변환 실행 버튼
        run = st.button("코드 변환 실행")
        if run:
            # 입력값이 없을 때 경고 표시
            if not cobol_code.strip():
                st.warning("COBOL 코드를 입력하거나 파일을 업로드하세요.")
            else:
                # 변환 실행 중 로딩 스피너 표시
                with st.spinner("코드 변환 중..."):
                    # 실제 변환 결과를 여기에 출력 (예시)
                    result_code = "# 여기에 변환된 코드가 표시됩니다."
                    st.code(result_code, language=target_lang)
                    # 복사 버튼 예시 (Streamlit은 코드 블록에 복사 버튼 내장)
                    st.button("코드 복사", on_click=st.session_state.setdefault, args=("copied", True))
        else:
            # 기본 안내 코드 블록
            st.code("# 변환된 코드가 여기에 표시됩니다.", language=target_lang)

with tab2:
    st.subheader("코드 설명")
    st.text_area("설명할 코드 입력", height=200, key="explain_code")
    if st.button("코드 설명 생성"):
        st.info("여기에 코드 설명이 표시됩니다.")

with tab3:
    st.subheader("테스트 코드 생성")
    st.text_area("테스트 코드 생성 대상 코드 입력", height=200, key="test_code")
    if st.button("테스트 코드 생성"):
        st.info("여기에 테스트 코드가 표시됩니다.")

with tab4:
    st.subheader("금융권 규정 검토")
    st.text_area("검토할 코드 입력", height=200, key="regulation_code")
    if st.button("규정 검토 실행"):
        st.info("여기에 금융권 규정 검토 결과가 표시됩니다.")