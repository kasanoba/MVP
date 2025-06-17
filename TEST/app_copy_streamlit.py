import os
import streamlit as st
from dotenv import load_dotenv
import openai

# 환경 변수 로드
load_dotenv()
api_key = os.getenv("OPENAI_API_KEY")
api_version = os.getenv("OPENAI_API_VERSION")
azure_endpoint = os.getenv("OPENAI_API_BASE")
deployment_name = os.getenv("OPENAI_DEPLOYMENT_NAME")

# openai 1.x 방식 클라이언트 생성
client = openai.AzureOpenAI(
    api_key=api_key,
    api_version=api_version,
    azure_endpoint=azure_endpoint
)

def call_openai(prompt, temperature=0.2, max_tokens=1024):
    response = client.chat.completions.create(
        model=deployment_name,
        messages=[{"role": "system", "content": "You are a helpful AI developer assistant."},
                  {"role": "user", "content": prompt}],
        temperature=temperature,
        max_tokens=max_tokens
    )
    return response.choices[0].message.content.strip()

def convert_cobol_code(cobol_code, target_lang):
    if target_lang == "python":
        prompt = f"다음 COBOL 코드를 Python 코드로 변환해줘. 주석은 최소화하고, 가독성 좋게 작성해줘.\n\nCOBOL 코드:\n{cobol_code}"
    elif target_lang == "java":
        prompt = f"다음 COBOL 코드를 Java 코드로 변환해줘. 주석은 최소화하고, 가독성 좋게 작성해줘.\n\nCOBOL 코드:\n{cobol_code}"
    else:
        return "지원하지 않는 언어입니다."
    return call_openai(prompt)

def explain_code(code, target_lang):
    prompt = f"{target_lang.capitalize()} 코드의 주요 로직과 함수에 대해 한글로 자세히 설명해줘. 함수별 역할과 전체 흐름을 요약해줘.\n\n코드:\n{code}"
    return call_openai(prompt, max_tokens=800)

def generate_test_code(code, target_lang):
    if target_lang == "python":
        prompt = f"아래 Python 코드에 대해 pytest 기반의 테스트 코드를 생성해줘. 테스트 함수별로 간단한 설명도 추가해줘.\n\n코드:\n{code}"
    elif target_lang == "java":
        prompt = f"아래 Java 코드에 대해 JUnit 기반의 테스트 코드를 생성해줘. 테스트 메서드별로 간단한 설명도 추가해줘.\n\n코드:\n{code}"
    else:
        return "지원하지 않는 언어입니다."
    return call_openai(prompt, max_tokens=800)

def review_regulation(code, target_lang):
    prompt = f"""아래 {target_lang.capitalize()} 코드가 금융권 개발 규정(보안, 로깅, 오류처리 등)에 부합하는지 검토해줘. 
필요시 개선점도 제안해줘. 
- 보안 취약점
- 로깅 누락
- 예외처리 미흡
- 개인정보 노출
등을 중점적으로 봐줘.

코드:
{code}
"""
    return call_openai(prompt, max_tokens=800)

def generate_documentation(code, target_lang):
    if target_lang == "python":
        prompt = f"아래 Python 코드의 함수와 클래스에 대해 docstring을 추가해줘. 그리고 전체 코드에 대한 Markdown 형식의 간단한 설명도 함께 작성해줘.\n\n코드:\n{code}"
    elif target_lang == "java":
        prompt = f"아래 Java 코드의 메서드와 클래스에 대해 Javadoc 주석을 추가해줘. 그리고 전체 코드에 대한 Markdown 형식의 간단한 설명도 함께 작성해줘.\n\n코드:\n{code}"
    else:
        return "지원하지 않는 언어입니다."
    return call_openai(prompt, max_tokens=800)

def answer_question(code, target_lang, question):
    prompt = f"아래 {target_lang.capitalize()} 코드와 사용자의 질문이 있어. 질문에 대해 한글로 답변해줘.\n\n코드:\n{code}\n\n질문: {question}"
    return call_openai(prompt, max_tokens=600)

st.title("AI 기반 레거시 & 현대 개발 지원 에이전트")

with st.form("main_form"):
    cobol_code = st.text_area("COBOL 코드 입력", height=150)
    target_lang = st.selectbox("변환 언어 선택", ["python", "java"])
    user_question = st.text_input("자연어 질문(선택)")
    submitted = st.form_submit_button("실행")

if submitted and cobol_code.strip():
    with st.spinner("AI가 코드를 분석/생성 중입니다..."):
        converted_code = convert_cobol_code(cobol_code, target_lang)
        explanation = explain_code(converted_code, target_lang)
        test_code = generate_test_code(converted_code, target_lang)
        regulation_review = review_regulation(converted_code, target_lang)
        documentation = generate_documentation(converted_code, target_lang)
        qa_answer = ""
        if user_question.strip():
            qa_answer = answer_question(converted_code, target_lang, user_question)

    st.subheader(f"변환 결과 ({target_lang.capitalize()})")
    st.code(converted_code, language=target_lang)
    st.subheader("코드 설명")
    st.write(explanation)
    st.subheader("테스트 코드")
    st.code(test_code, language=target_lang)
    st.subheader("금융권 규정 검토")
    st.write(regulation_review)
    st.subheader("문서화 (docstring/Markdown)")
    st.write(documentation)
    if qa_answer:
        st.subheader("자연어 질의응답")
        st.write(qa_answer)