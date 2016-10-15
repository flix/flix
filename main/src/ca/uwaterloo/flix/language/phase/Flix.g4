/*
ANTLR grammar file describing the Flix language.

Daniel Ohashi 2016
 */

grammar Flix;

@header {
package ca.uwaterloo.flix.language.phase;
}

//General purpose tokens
fragment NewLine : [\n\r];
fragment SingleLineComment : '//' .*? NewLine;
fragment MultiLineComment : '/*' .*? '*/';

WS : (' ' | '\t' | NewLine | Comment)+;
SC : ';';
Comment : SingleLineComment | MultiLineComment;
Ident : [a-zA-Z⊥⊤⊑⊔⊓▽△⊡][a-zA-Z0-9_$⊥⊑]*;

//Keywords/symbols that always evaluate to one token
FNone : 'None' ;
FNil : 'Nil';
Bot : '⊥';
Top : '⊤';
Wild : '_';
UserError : '???';


//Root rule
start : WS? s_imports? WS? decls? WS?;

optSC : (WS? SC)?;

//Names & Identifiers
nname : Ident ('.' Ident)*;
qname : (nname '/')? Ident;
annotation : '@' Ident;



//Arguments/lists
argument : Ident ':' WS? type;
arguments : argument (WS? ',' WS? argument)*;
params : ('(' WS? arguments? WS? ')' )?;

attribute : Ident WS? ':' WS? type;
attributes : attribute (WS? ',' WS? attribute)*;

index : '{' WS? (Ident (WS? ',' WS? Ident)*)? WS? '}';
indexes : index (WS? ',' WS? index)*;

idents : Ident (WS? ',' WS? Ident)*;

match_rule : 'case' WS pattern WS? '=>' WS? expression SC?;
match_rules : match_rule (WS? match_rule)*;

switch_rule : 'case' WS expression WS? '=>' WS? expression SC?;
switch_rules : switch_rule (WS? switch_rule)*;

typeparam : Ident (WS? ':' WS? type)?;
typeparams : typeparam (WS? ',' WS? typeparam)*;

class_typeparams : type (WS? ',' WS? type)*;

contextBound : Ident '[' WS? class_typeparams WS? ']';
contextBounds : contextBound (WS? ',' WS? contextBound)*;
contextBoundsList : ('<=' WS? contextBounds WS?)?;

functions : decls_function (WS decls_function)*;

//Imports
s_import : 	import_wildcard |
			import_definition |
			import_namespace;
s_imports : s_import (WS? s_import)*;

import_wildcard : 'import' WS nname '/' Wild optSC;
import_definition : 'import' WS nname '/' Ident optSC;
import_namespace : 'import' WS nname optSC;



//Declarations
decl : decls_namespace |
		decls_enum |
		decls_relation |
		decls_lattice |
		decls_index |
		decls_signature |
		decls_external |
		decls_function |
		decls_law |
		decls_class |
		decls_fact |
		decls_rule |
		decls_letlattice;
decls : decl (WS? decl)*;


decls_namespace : 'namespace' WS nname WS? '{' WS? decls? WS? '}' optSC;

decls_enum : 'enum' WS Ident WS? '{' WS? cases WS? (',' WS? cases WS?)* '}' optSC;
cases : 'case' WS Ident tuple?;

decls_relation : 'rel' WS Ident WS? '(' WS? attributes? WS? ')' optSC;

decls_lattice : 'lat' WS Ident WS? '(' WS? attributes? WS? ')' optSC;

decls_index : 'index' WS Ident WS? '(' WS? indexes? WS? ')' optSC;

decls_signature : 'def' WS Ident WS? params WS? ':' WS? type optSC;

decls_external : 'external' WS? 'def' WS Ident WS? params WS? ':' WS? type optSC;

decls_function : (annotation (WS annotation)*)? WS? 'def' WS Ident WS? params WS? ':' WS? type WS? '=' WS? expression optSC;

decls_law : 'law' WS Ident WS? '[' WS? typeparams WS? ']' WS? params WS? ':' WS? type WS? '=' WS? expression optSC;

decls_class : 'class' WS Ident '[' WS? class_typeparams WS? ']' WS? contextBoundsList WS? class_body;

class_body : '{' WS? functions? WS? '}';

decls_fact : predicate WS? '.';

decls_rule : predicate WS? ':-' WS? predicates WS? '.';

elms : expressions;
decls_letlattice : 'let' WS? type '<>' WS? '=' WS? '(' WS? elms WS? ')' optSC;


//Expressions
expression : comparison (WS? logical_ops WS? comparison)?;
expressions : expression (WS? ',' WS? expression)*;

comparison : additive (WS? comparison_ops WS? additive)?;
additive : multiplicative (WS? addve_ops WS? multiplicative)*;
multiplicative : infix (WS? multipve_ops WS? infix)*;
infix : extended (WS? '`' qname  '`' WS? extended)?;
extended : unary (WS? extbin_ops WS? unary)?;
unary : (unary_ops WS? unary) | ascribe;
ascribe : e_fList (WS? ':' WS? type)?;

e_primary : e_letMatch | e_ifThenElse | e_match | e_switch |
				e_tag | e_lambda | e_tuple | e_fNil | e_fNone |
				e_fSome | e_fVec | e_fSet | e_fMap | literals |
				existential | universal | Bot | Top |
				e_unaryLambda | e_wild | e_var | UserError;

e_letMatch : 'let' WS pattern WS? '=' WS? expression WS 'in' WS expression;
e_ifThenElse : 'if' WS? '(' WS? expression WS? ')' WS? expression WS 'else' WS expression;
e_match : 'match' WS expression WS 'with' WS '{' WS? match_rules WS? '}';
e_switch : 'switch' WS expression WS 'with' WS '{' WS? switch_rules WS?'}';

e_apply : e_primary (WS? '(' WS? expressions? WS? ')')?;

e_var : qname;
e_tag : qname '.' Ident (WS? e_tuple)?;
e_tuple : '(' WS? expressions? WS? ')';

e_keyValue : expression WS? '->' WS? expression;
e_keyValues : e_keyValue (WS? ',' WS? e_keyValue)*;

e_wild : Wild;
e_fNil : FNil;
e_fNone : FNone;
e_fSome : 'Some' WS? '(' WS? expression WS? ')';
e_fList : e_apply (WS? '::' WS? expression)?;
e_fVec : '#[' WS? expressions? WS? ']';
e_fSet : '#{' WS? expressions? WS? '}';
e_fMap : '@{' WS? e_keyValues? WS? '}';

e_unaryLambda : Ident WS? '->' WS? expression;
e_lambda : '(' WS? idents WS? ')' WS? '->' WS? expression;

existential : ('∃' | '\\exists') WS? params WS? '.' WS? expression;
universal : ('∀' | '\\forall') WS? params WS? '.' WS? expression;



//Patterns
pattern : simple (WS? '::' WS? pattern)?;
patterns : pattern (WS? ',' WS? pattern)*;
simple : p_fNil | p_fNone | literals | Ident |
		Wild | p_tag | p_tuple | p_fVec | p_fSet | p_fMap;

p_keyValue : pattern WS? '->' WS? pattern;
p_keyValues : p_keyValue (WS? ',' WS? p_keyValue)*;

p_tag : qname '.' Ident (WS? pattern)?;
p_tuple : '(' WS? patterns? WS? ')';

p_fNil : FNil;
p_fNone : FNone;
p_fVec : '#[' WS? patterns? (WS? ',' WS? '...')? WS? ']';
p_fSet : '#{' WS? patterns? (WS? ',' WS? '...')? WS? '}';
p_fMap : '@{' WS? p_keyValues? (WS? ',' WS? '...')? WS? '}';



//Literals
bools : 'true' | 'false';

Chars : '\'' . '\'';

Strs : '"' ( '\"' | ~( '"' | '\n' | '\r' ) )* '"';

Digits : [0-9]+;
negative : '-';

float32 : negative? Digits '.' Digits 'f32';
float64 : negative? Digits '.' Digits 'f64';
floatDefault : negative? Digits '.' Digits;
floats : float32 | float64 | floatDefault;

int8 : negative? Digits 'i8';
int16 : negative? Digits 'i16';
int32 : negative? Digits 'i32';
int64 : negative? Digits 'i64';
bigInt : negative? Digits 'ii';
intDefault : negative? Digits;
ints : int8 | int16 | int32 | int64 | bigInt | intDefault;

literals : bools | Chars | floats | ints | Strs;

 

//Types
primary : lambda | tuple | parametric | qname;

type : primary (WS? '->' WS? type)?;
lambda : '(' WS? type (WS? ',' WS? type)*  WS? ')' WS? '->' WS? type;

tuple_unit : '()';
tuple_singleton : '(' WS? type WS? ')';
tuple_multi : '(' WS? type (WS? ',' WS? type)+ WS? ')';
tuple : tuple_unit | tuple_singleton | tuple_multi;


parametric : qname WS? '[' WS? type (WS? ',' WS? type)* WS? ']';


//Operators
unary_ops : ('+' | '-' | '¬' | '~' | '!');
logical_ops : ('&&' | '||' | '&' | '|' | 
				'==>' | '<==>' | '^' |
				'<<' | '>>' | '∧' | '∨' |
				'→' | '↔');
comparison_ops : ('<=' | '>=' | '<' | '>' |
					'==' | '!=' | '≡');
multipve_ops : ('**' | '*' | '/' | '%');
addve_ops : ('+' | '-');
extbin_ops : ('⊑' | '⊔' | '⊓' | '▽' | '△');



//Predicates
predicate : pred_true | pred_false | pred_ambiguous |
			pred_notequal | pred_equal | pred_loop;
predicates : predicate (WS? ',' WS? predicate)*;


pred_true : 'true';
pred_false : 'false';
pred_ambiguous : qname WS? '(' expressions ')';
pred_equal : Ident WS? ':=' WS? Ident;
pred_notequal : Ident WS? '!=' WS? Ident;
pred_loop : Ident WS? '<-' WS? expression;