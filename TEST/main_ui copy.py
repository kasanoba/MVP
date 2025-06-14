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

# 각 서비스 함수 import
# from services.code_conversion_service import convert_cobol_code
# from services.code_explanation_service import explain_code
# from services.test_generation_service import generate_test_code
# from services.regulation_review_service import review_regulation

# 페이지 타이틀 및 레이아웃 설정
st.set_page_config(page_title="AI 기반 레거시 & 현대 개발 지원 에이전트", layout="wide")
st.title("AI 기반 레거시 & 현대 개발 지원 에이전트")

tab1, tab2, tab3, tab4 = st.tabs(["언어전환", "코드설명", "테스트 코드", "금융권 규정 검토"])

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
                    # 서비스 함수 호출 부분 주석 처리
                    # result_code = convert_cobol_code(cobol_code, target_lang)
                    result_code = "# (예시) 변환된 코드가 여기에 표시됩니다."
                    st.code(result_code, language=target_lang)
        else:
            st.code("# 변환된 코드가 여기에 표시됩니다.", language=target_lang)

with tab2:
    st.subheader("코드 설명")
    code_to_explain = st.text_area("설명할 코드 입력", height=300, key="explain_code")
    if st.button("코드 설명 생성"):
        if not code_to_explain.strip():
            st.warning("설명할 코드를 입력하세요.")
        else:
            with st.spinner("코드 설명 생성 중..."):
                # 서비스 함수 호출 부분 주석 처리
                # explanation = explain_code(code_to_explain)
                explanation = "# (예시) 코드 설명이 여기에 표시됩니다."
                st.write(explanation)

with tab3:
    st.subheader("테스트 코드 생성")
    code_for_test = st.text_area("테스트 코드 생성 대상 코드 입력", height=300, key="test_code")
    target_lang_test = st.selectbox("테스트 코드 언어 선택", ["python", "java"], key="target_lang_test")
    if st.button("테스트 코드 생성"):
        if not code_for_test.strip():
            st.warning("테스트 코드 생성을 위한 코드를 입력하세요.")
        else:
            with st.spinner("테스트 코드 생성 중..."):
                # 서비스 함수 호출 부분 주석 처리
                # test_code = generate_test_code(code_for_test, target_lang_test)
                test_code = "# (예시) 테스트 코드가 여기에 표시됩니다."
                st.code(test_code, language=target_lang_test)

with tab4:
    st.subheader("금융권 규정 검토")
    code_for_review = st.text_area("검토할 코드 입력", height=300, key="regulation_code")
    target_lang_review = st.selectbox("코드 언어 선택", ["python", "java"], key="target_lang_review")
    if st.button("규정 검토 실행"):
        if not code_for_review.strip():
            st.warning("검토할 코드를 입력하세요.")
        else:
            with st.spinner("금융권 규정 검토 중..."):
                # 서비스 함수 호출 부분 주석 처리
                # review = review_regulation(code_for_review, target_lang_review)
                review = "# (예시) 금융권 규정 검토 결과가 여기에 표시됩니다."
                st.write(review)