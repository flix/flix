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
fragment SingleLineComment : '//' (~['/'\n\r] .*?)?? (NewLine | EOF);
TripleSlashComment : '///' .*? (NewLine | EOF);
tscomment : TripleSlashComment;
fragment MultiLineComment : '/*' .*? '*/';

WS : (' ' | '\t' | NewLine | Comment)+;
SC : ';';
Comment : SingleLineComment | MultiLineComment;

//Keywords/symbols that always evaluate to one token
ENUM : 'enum';
NAMESPACE : 'namespace';
REL : 'rel';
LAT : 'lat';
INDEX : 'index';
DEF : 'def';
EXTERNAL : 'external';
LAW : 'law';
CLASS : 'class';
LET : 'let';
IMPL : 'impl';
FNIL : 'Nil';
SWITCH : 'switch';
MATCH : 'match';
WITH : 'with';
WILD : '_';
CASE : 'case';
IF : 'if';
ELSE : 'else';
IMPORT : 'import';

LowerIdent : [a-z][a-zA-Z0-9_"'"]*;
UpperIdent : [A-Z][a-zA-Z0-9_"'"]*;



//Root rule
start : s_import* decl* WS? EOF;

optSC : (WS? SC)?;

//Names & Identifiers
ident : (UpperIdent | LowerIdent);
nname : ident ('.' ident)*;
lowerqname : (nname '/')? LowerIdent;
upperqname : (nname '/')? UpperIdent;

//Enforce upper and lower case names
annotationName : LowerIdent;
attributeName : LowerIdent;
className : UpperIdent;
definitionName : LowerIdent;
qualifiedDefinitionName : lowerqname;
tableName : UpperIdent;
qualifiedTableName : upperqname;
tagName : UpperIdent | FNIL;
typeName : UpperIdent;
qualifiedTypeName : upperqname;
variableName : LowerIdent;


//Arguments/lists
variableNames : variableName (WS? ',' WS? variableName)*;

argument : variableName ':' WS? type;
arguments : argument (WS? ',' WS? argument)*;
formalparams : ('(' WS? arguments? WS? ')' )?;

attribute : attributeName WS? ':' WS? type;
attributes : attribute (WS? ',' WS? attribute)*;

index : '{' WS? (attributeName (WS? ',' WS? attributeName)*)? WS? '}';
indexes : index (WS? ',' WS? index)*;

match_rule : CASE WS pattern WS? '=>' WS? expression SC?;
match_rules : match_rule (WS? match_rule)*;

switch_rule : CASE WS expression WS? '=>' WS? expression SC?;
switch_rules : switch_rule (WS? switch_rule)*;

typeparam : variableName (WS? ':' WS? type)?;
typeparams : ('[' WS? typeparam (WS? ',' WS? typeparam)* ']')?;

class_typeparams : '[' type (WS? ',' WS? type)* ']';

contextBound : className class_typeparams;
contextBounds : contextBound (WS? ',' WS? contextBound)*;
contextBoundsList : (WS? '=>' WS? contextBounds)?;
implContextBoundsList : (WS? '<=' WS? contextBounds)?;

annotation : '@' annotationName;
annotations : annotation (WS annotation)*;

//Imports
s_import : 	WS? (import_wildcard |
			import_definition |
			import_namespace);

import_wildcard : IMPORT WS nname '/' WILD optSC;
import_definition : IMPORT WS nname '/' ident optSC;
import_namespace : IMPORT WS nname optSC;


//Declarations
decl : decls_namespace |
		decls_enum |
		decls_relation |
		decls_lattice |
		decls_index |
		decls_signature |
		decls_external |
		decls_definition |
		decls_law |
		decls_class |
		decls_fact |
		decls_rule |
		decls_impl |
		decls_letlattice;


decls_namespace : WS? NAMESPACE WS nname WS? '{' WS? decl* WS? '}' optSC;

decls_enum : (WS? tscomment)* WS? ENUM WS typeName typeparams WS? '{' WS? dcases WS? '}' optSC;
dcases : dcase (WS? ',' WS? dcase)*;
dcase : CASE WS tagName tuple?;

decls_relation : (WS? tscomment)* WS? REL WS tableName WS? '(' WS? attributes? WS? ')' optSC;

decls_lattice : (WS? tscomment)* WS? LAT WS tableName WS? '(' WS? attributes? WS? ')' optSC;

decls_index : WS? INDEX WS qualifiedTableName WS? '(' WS? indexes? WS? ')' optSC;

decls_signature : (WS? tscomment)* WS? DEF WS definitionName WS? formalparams WS? ':' WS? type optSC;

decls_external : (WS? tscomment)* WS? EXTERNAL WS? DEF WS definitionName WS? formalparams WS? ':' WS? type optSC;

decls_definition : (WS? tscomment)* WS? annotations? WS? DEF WS definitionName WS? typeparams formalparams WS? ':' WS? type WS? '=' WS? expression optSC;

decls_law : (WS? tscomment)* WS? LAW WS definitionName WS? typeparams WS? formalparams  WS? ':' WS? type WS? '=' WS? expression optSC;

decls_class : (WS? tscomment)* WS? CLASS WS className class_typeparams contextBoundsList WS? class_body;

class_body : '{' WS? (class_decl WS?)* '}';
class_decl : decls_definition | decls_signature | decls_law ;

decls_fact : WS? predicate WS? '.';

decls_rule : WS? predicate WS? ':-' WS? predicates WS? '.';

elms : expressions;
decls_letlattice : WS? LET WS? type '<>' WS? '=' WS? '(' WS? elms WS? ')' optSC;

decls_impl : (WS? tscomment)* WS? IMPL WS className class_typeparams WS? implContextBoundsList WS? decls_impl_body;
decls_impl_body : '{' WS? decls_definition* WS? '}';

//Expressions
expression : ('{' WS? expression WS? '}' WS?) | logical;
logical : comparison (WS? logical_ops WS? comparison)?;
expressions : expression (WS? ',' WS? expression)*;

comparison : additive (WS? comparison_ops WS? additive)?;
additive :  additive WS? addve_ops WS? multiplicative |
			multiplicative;
multiplicative : multiplicative WS? multipve_ops WS? infix
				| infix;
infix : extended (WS? '`' qualifiedDefinitionName  '`' WS? extended)?;
extended : unary (WS? extbin_ops WS? unary)?;

unary : {!( _input.LT(1).getText().equals("-") && //Make sure this isn't just a negative number
		Character.isDigit(_input.LT(2).getText().charAt(0)) )}? (unary_ops WS? unary) 
		| ascribe;
ascribe : e_fList (WS? ':' WS? type)?;

e_primary : e_letMatch | e_ifThenElse | e_match | e_switch |
				e_qname | e_tag | e_lambda | e_tuple | e_fNil |
				 e_fVec | e_fSet | e_fMap | e_literal |
				existential | universal  |
				e_unaryLambda | e_wild | e_sname | e_userError;

e_letMatch : LET WS pattern WS? '=' WS? expression WS? ';' WS? expression;
e_ifThenElse : IF WS? '(' WS? expression WS? ')' WS? expression WS ELSE WS expression;
e_match : MATCH WS expression WS WITH WS '{' WS? match_rules WS? '}';
e_switch : SWITCH WS '{' WS? switch_rules WS?'}';

e_apply : e_primary (WS? '(' WS? expressions? WS? ')')?;

e_unaryLambda : variableName WS? '->' WS? expression;
e_lambda : '(' WS? variableNames WS? ')' WS? '->' WS? expression;

e_literal : literal;
e_sname : variableName;
e_qname : qualifiedDefinitionName;
e_tag : (qualifiedTypeName '.')? tagName (WS? e_tuple)?;
e_tuple : '(' WS? expressions? WS? ')';

e_keyValue : expression WS? '->' WS? expression;
e_keyValues : e_keyValue (WS? ',' WS? e_keyValue)*;

e_userError : '???';
e_wild : WILD;
e_fNil : FNIL;
e_fList : e_apply (WS? '::' WS? expression)?;
e_fVec : '#[' WS? expressions? WS? ']';
e_fSet : '#{' WS? expressions? WS? '}';
e_fMap : '@{' WS? e_keyValues? WS? '}';

existential : ('∃' | '\\exists') WS? formalparams WS? '.' WS? expression;
universal : ('∀' | '\\forall') WS? formalparams WS? '.' WS? expression;



//Patterns
pattern : p_fList;
patterns : pattern (WS? ',' WS? pattern)*;
simple : p_fNil | p_literal | p_variable |
		p_wild | p_tag | p_tuple | p_fVec | p_fSet | p_fMap;

p_keyValue : pattern WS? '->' WS? pattern;
p_keyValues : p_keyValue (WS? ',' WS? p_keyValue)*;

p_literal : literal;
p_tag : (qualifiedTypeName '.')? tagName (WS? pattern)?;
p_tuple : '(' WS? patterns? WS? ')';

p_wild : WILD;
p_fNil : FNIL;
p_variable : variableName;
p_fList : simple (WS? '::' WS? pattern)?;
p_fVec : '#[' WS? patterns? (WS? ',' WS? pattern '...')? WS? ']';
p_fSet : '#{' WS? patterns? (WS? ',' WS? pattern '...')? WS? '}';
p_fMap : '@{' WS? p_keyValues? (WS? ',' WS? pattern '...')? WS? '}';



//Literals
bools : ('true' | 'false');

Chars : '\'' . '\'';
chars : Chars;

Strs : '"' ~[\n\r]*? '"';
strs : Strs;
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

literal : (bools | chars | floats | ints | strs);

 

//Types
primary : arrow | tuple | apply | var | ref;

var : variableName;
ref : qualifiedTypeName;

type : primary (WS? '->' WS? type)?;
arrow : '(' WS? type (WS? ',' WS? type)* WS? ')' WS? '->' WS? type;

tuple_unit : '(' ')';
tuple_singleton : '(' WS? type WS? ')';
tuple_multi : '(' WS? type (WS? ',' WS? type)+ WS? ')';
tuple : tuple_unit | tuple_singleton | tuple_multi;


apply : ref WS? '[' WS? type (WS? ',' WS? type)* WS? ']' WS?;

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
predicate : pred_true | pred_false | pred_filter |
			pred_notequal | pred_table | pred_loop;
predicates : predicate (WS? ',' WS? predicate)*;


pred_true : 'true';
pred_false : 'false';
pred_filter : qualifiedDefinitionName WS? '(' expressions ')';
pred_table : qualifiedTableName WS? '(' expressions ')';
pred_notequal : variableName WS? '!=' WS? variableName;
pred_loop : variableName WS? '<-' WS? expression;