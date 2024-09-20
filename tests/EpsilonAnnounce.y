{
import System.Exit
}

%tokentype { Token }
%token
	a { TokenA }
	b { TokenB }


%%
A:      B b A { $1 + $3 }
A:      B { $1 }

B:	C { $1 }

C: 	D { $1 }

D:	a { 1 }
D:	{ 0 }

{
data Token = TokenA | TokenB

happyError _ = error ""

parse = happyParse . lexer
lexer "" = []
lexer ('a':r) = TokenA:(lexer r)
lexer ('b':r) = TokenB:(lexer r)

main = do
  if and [parse "" == 0,
          parse "a" == 1,
          parse "ab" == 1,
          parse "abb" == 1,
          parse "abba" == 2,
          parse "abab" == 2,
          parse "ababa" == 3]
  then exitSuccess
  else exitFailure
}