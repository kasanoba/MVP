다음은 제공된 COBOL 코드를 Python 코드로 변환한 것입니다.

```python
def main():
    for i in range(2, 10):
        print(f"Calling multiplication for {i}")
        if i == 2:
            multiply2()
        elif i == 3:
            multiply3()
        elif i == 4:
            multiply4()
        elif i == 5:
            multiply5()
        elif i == 6:
            multiply6()