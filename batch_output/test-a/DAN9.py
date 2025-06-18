다음은 주어진 COBOL 코드를 Python 코드로 변환한 예입니다.

```python
def main():
    i = 1

    while i <= 9:
        result = 9 * i
        print(f"9 * {i} = {result}")
        i += 1

if __name__ == "__main__":
    main()
```

이 코드는 COBOL의 반복문과 계산 로직을 Python의 `while` 루프와 계산식