import os
import streamlit as st
from openai import AzureOpenAI
from dotenv import load_dotenv

# 환경변수 로드
load_dotenv()

# 환경변수 확인 (디버깅용)
api_key = os.getenv("OPENAI_API_KEY")
endpoint = os.getenv("OPENAI_ENDPOINT")
chat_model = os.getenv("CHAT_MODEL")

# 환경변수 로드 확인
st.sidebar.subheader("설정 정보 (디버깅)")
st.sidebar.write(f"Endpoint: {endpoint}")
st.sidebar.write(f"Model/Deployment: {chat_model}")
st.sidebar.write(f"API Key 존재: {'예' if api_key else '아니오'}")

# Azure OpenAI 클라이언트 초기화
try:
    client = AzureOpenAI(
        api_key=api_key,
        api_version="2024-12-01-preview",  # API 버전 변경
        azure_endpoint=endpoint
    )
    st.sidebar.success("클라이언트 초기화 성공")
except Exception as e:
    st.sidebar.error(f"클라이언트 초기화 실패: {e}")
    st.stop()

# COBOL → Python 변환 함수
def convert_cobol_to_python(cobol_code):
    # 여러 API 버전으로 시도
    api_versions = ["2024-10-21", "2024-08-01-preview", "2024-06-01", "2024-02-01"]
    
    for version in api_versions:
        try:
            # 각 API 버전으로 클라이언트 생성
            temp_client = AzureOpenAI(
                api_key=api_key,
                api_version=version,
                azure_endpoint=endpoint
            )
            
            st.write(f"API 버전 {version} 시도 중...")
            
            # OpenAI API 호출
            response = temp_client.chat.completions.create(
                model=chat_model,
                messages=[
                    {"role": "system", "content": "You are a helpful assistant for code transformation."},
                    {"role": "user", "content": f"Convert the following COBOL code to Python:\n{cobol_code}"}
                ],
                max_tokens=1500,
                temperature=0.7
            )
            
            st.success(f"✅ API 버전 {version} 성공!")
            return response.choices[0].message.content.strip()
            
        except Exception as e:
            st.warning(f"API 버전 {version} 실패: {str(e)[:100]}...")
            continue
    
    return "❌ 모든 API 버전에서 실패했습니다. 배포 이름을 확인해주세요."

# Streamlit UI
st.title("COBOL → Python 코드 변환기")
st.subheader("Azure OpenAI를 활용한 COBOL 코드 변환 도구")

# COBOL 코드 입력
cobol_code = st.text_area("COBOL 코드를 입력하세요:", placeholder="예: DISPLAY 'HELLO, WORLD'.")

# 세션 상태 초기화 (변환 기록 저장용)
if "conversion_history" not in st.session_state:
    st.session_state.conversion_history = []

# 변환 처리
if st.button("코드 변환"):
    if cobol_code.strip():
        with st.spinner("코드 변환 중..."):
            python_code = convert_cobol_to_python(cobol_code)
        
        # 변환 기록에 추가
        st.session_state.conversion_history.append((cobol_code, python_code))
        
        st.subheader("변환된 Python 코드")
        st.code(python_code, language="python")
    else:
        st.error("COBOL 코드를 입력하세요.")

# 변환 기록 표시
if st.session_state.conversion_history:
    st.subheader("변환 기록")
    for i, (cobol, python) in enumerate(st.session_state.conversion_history):
        with st.container():
            st.write(f"**COBOL 코드 {i+1}:**")
            st.code(cobol, language="cobol")
            st.write(f"**변환된 Python 코드 {i+1}:**")
            st.code(python, language="python")
            st.divider()

# 변환 기록 초기화 버튼
if st.session_state.conversion_history:
    if st.button("변환 기록 초기화"):
        st.session_state.conversion_history = []
        st.rerun()