from dsharp_parser import parser

def main():
    print("D# Language Interpreter")
    while True:
        try:
            s = input('D# > ')
            if not s:
                continue  # Skip empty inputs
            result = parser.parse(s)
            if result is not None:
                print(result)
        except EOFError:
            print("\nExiting...")
            break

if __name__ == "__main__":
    main()
