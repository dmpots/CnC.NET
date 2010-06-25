
all: Lexer.fs Parser.fs
	 @touch ".ALL"
	 echo "rebuild done"

Lexer.fs: Lexer.fsl
	fslex Lexer.fsl

Parser.fs: Parser.fsy
	fsyacc Parser.fsy

clean:
	rm -f Lexer.fs Parser.fs Parser.fsi
