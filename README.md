Compiles and runs files written in a language that uses the following syntax. Includes garbage collection and tail-call optimizaiton for functions.

<program> ::=
  | <declaration_list> <expression>
  | <expression>

<declaration_list> ::=
  | <declaration> <declaration_list>
  | <declaration>

<declaration> ::=
  | def <identifier> <parameter_list> = <expression> end

<parameter_list> ::=
  | <identifier> <parameter_list>
  | <identifier>

<expression> ::=
  | <integer>
  | true
  | false
  | <identifier>
  | let <identifier> = <expression> in <expression>
  | if <expression> then <expression> else <expression>
  | after(<expression>)
  | before(<expression>)
  | print(<expression>)
  | isbool(<expression>)
  | isint(<expression>)
  | istuple(<expression>)
  | <expression> + <expression>
  | <expression> - <expression>
  | <expression> * <expression>
  | <expression> < <expression>
  | <expression> > <expression>
  | <expression> = <expression>
  | <expression> && <expression>
  | <expression> || <expression>
  | <expression>[<expression>]
  | <expression> <expression>
  | (<expression>)
  | (<expression>, <expression_list>)

<expression_list> ::=
  | <expression> , <expression_list>
  | <expression>
