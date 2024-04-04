# main.py
from dsharp_parser import parser

def main():
    print("D# Language Interpreter")
    while True:
        try:
            s = input('D# > ')
        except EOFError:
            print("\nExiting...")
            break
        if not s.strip():
            continue
        result = parser.parse(s)
        print(result)

if __name__ == "__main__":
    main()
