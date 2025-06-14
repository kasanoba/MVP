import streamlit as st
from io import StringIO
import openai
import os
from dotenv import load_dotenv

# Load API key from .env file
load_dotenv()
openai.api_key = os.getenv("OPENAI_API_KEY")

# OpenAI API-based functions
def call_openai_api(prompt, model="dev-gpt-4o-mini"):
    try:
        response = openai.ChatCompletion.create(
            model=model,
            messages=[
                {"role": "system", "content": "You are a helpful assistant for code transformation and analysis."},
                {"role": "user", "content": prompt}
            ],
            max_tokens=1500,
            temperature=0.7
        )
        return response["choices"][0]["message"]["content"].strip()
    except Exception as e:
        return f"Error: {e}"

def convert_code(input_code, target_language):
    prompt = f"""
    Convert the following code to {target_language}:
    {input_code}
    """
    return call_openai_api(prompt)

def generate_comments(input_code):
    prompt = f"""
    Add detailed comments to the following code:
    {input_code}
    """
    return call_openai_api(prompt)

def generate_test_code(input_code):
    prompt = f"""
    Generate unit tests for the following code using pytest:
    {input_code}
    """
    return call_openai_api(prompt)

def perform_rag_analysis(input_code):
    prompt = f"""
    Analyze the following code for compliance with financial regulations, including security, logging, and error handling:
    {input_code}
    """
    return call_openai_api(prompt)

# Streamlit UI
st.title("AI 기반 레거시 & 현대 개발 지원 에이전트")
st.subheader("금융권 특화 코드 변환 및 분석 도구")

# File upload or text input
uploaded_file = st.file_uploader("COBOL 또는 Python 코드 파일 업로드", type=["cbl", "py", "txt"])
code_input = ""
if uploaded_file is not None:
    stringio = StringIO(uploaded_file.getvalue().decode("utf-8"))
    code_input = stringio.read()
else:
    code_input = st.text_area("또는 직접 코드를 입력하세요", height=200)

# Target language selection
target_language = st.selectbox("변환 대상 언어 선택", ["Python", "Java"])

# Process the input code
if st.button("코드 변환 및 분석 실행"):
    if code_input.strip():
        # Perform code conversion
        converted_code = convert_code(code_input, target_language)
        st.subheader("변환된 코드")
        st.code(converted_code, language=target_language.lower())

        # Generate comments
        comments = generate_comments(code_input)
        st.subheader("주석 및 설명")
        st.code(comments, language="python")

        # Generate test code
        test_code = generate_test_code(code_input)
        st.subheader("테스트 코드")
        st.code(test_code, language="python")

        # Perform regulatory analysis
        rag_report = perform_rag_analysis(code_input)
        st.subheader("금융권 규정 검토 결과")
        st.text(rag_report)
    else:
        st.error("코드를 입력하거나 파일을 업로드하세요.")