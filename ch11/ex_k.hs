-- did this already:
addop = (symbol "+" >> return Plus) <|> (symbol "-" >> return Minus)
mulop = (symbol "*" >> return Mul) <|> (symbol "/" >> return Div)

