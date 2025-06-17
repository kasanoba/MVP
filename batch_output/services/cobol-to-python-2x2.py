아래는 주어진 COBOL 코드를 Python으로 변환한 것입니다.

```python
def gugudan_two():
    i = 1
    print("=== 구구단 2단 ===")
    
    while i <= 9:
        result = 2 * i
        print(f"2 x {i} = {result}")
        i += 1

if __name__ == "__main__":
    gugudan_two()
```

이 코드는 구구단 2