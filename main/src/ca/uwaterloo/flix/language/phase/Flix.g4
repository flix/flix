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
LowerIdent : [a-z][a-zA-Z0-9_"'"]*;
UpperIdent : [A-Z][a-zA-Z0-9_"'"]*;

//Keywords/symbols that always evaluate to one token
FNil : 'Nil';
Wild : '_';

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
tagName : UpperIdent;
typeName : UpperIdent;
qualifiedTypeName : upperqname;
variableName : LowerIdent;


//Arguments/lists
variableNames : variableName (WS? ',' WS? variableName);

argument : variableName ':' WS? type;
arguments : argument (WS? ',' WS? argument)*;
formalparams : ('(' WS? arguments? WS? ')' )?;

attribute : attributeName WS? ':' WS? type;
attributes : attribute (WS? ',' WS? attribute)*;

index : '{' WS? (attributeName (WS? ',' WS? attributeName)*)? WS? '}';
indexes : index (WS? ',' WS? index)*;

idents : ident (WS? ',' WS? ident)*;

match_rule : 'case' WS pattern WS? '=>' WS? expression SC?;
match_rules : match_rule (WS? match_rule)*;

switch_rule : 'case' WS expression WS? '=>' WS? expression SC?;
switch_rules : switch_rule (WS? switch_rule)*;

typeparam : variableName (WS? ':' WS? type)?;
typeparams : ('[' WS? typeparam (WS? ',' WS? typeparam)* ']')?;

class_typeparams : '[' type (WS? ',' WS? type)* ']';

contextBound : className class_typeparams;
contextBounds : contextBound (WS? ',' WS? contextBound)*;
contextBoundsList : (WS? '<=' WS? contextBounds WS?)?;

annotation : '@' annotationName;
annotations : annotation (WS annotation)*;

//Imports
s_import : 	WS? (import_wildcard |
			import_definition |
			import_namespace);

import_wildcard : 'import' WS nname '/' Wild optSC;
import_definition : 'import' WS nname '/' ident optSC;
import_namespace : 'import' WS nname optSC;


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
		decls_letlattice;


decls_namespace : WS? 'namespace' WS nname WS? '{' WS? decl* WS? '}' optSC;

decls_enum : (WS? tscomment)* WS? 'enum' WS typeName typeparams WS? '{' WS? dcases WS? '}' optSC;
dcases : dcase (WS? ',' WS? dcase)*;
dcase : 'case' WS tagName tuple?;

decls_relation : (WS? tscomment)* WS? 'rel' WS tableName WS? '(' WS? attributes? WS? ')' optSC;

decls_lattice : (WS? tscomment)* WS? 'lat' WS tableName WS? '(' WS? attributes? WS? ')' optSC;

decls_index : WS? 'index' WS qualifiedTableName WS? '(' WS? indexes? WS? ')' optSC;

decls_signature : (WS? tscomment)* WS? 'def' WS definitionName WS? formalparams WS? ':' WS? type optSC;

decls_external : (WS? tscomment)* WS? 'external' WS? 'def' WS definitionName WS? formalparams WS? ':' WS? type optSC;

decls_definition : (WS? tscomment)* WS? annotations? WS? 'def' WS definitionName WS? typeparams formalparams WS? ':' WS? type WS? '=' WS? expression optSC;

decls_law : (WS? tscomment)* WS? 'law' WS definitionName WS? typeparams WS? formalparams  WS? ':' WS? type WS? '=' WS? expression optSC;

decls_class : (WS? tscomment)* WS? 'class' WS className class_typeparams WS? contextBoundsList WS? class_body;

class_body : '{' WS? ((decls_definition | decls_signature | decls_law) WS?) '}';

decls_fact : WS? predicate WS? '.';

decls_rule : WS? predicate WS? ':-' WS? predicates WS? '.';

elms : expressions;
decls_letlattice : WS? 'let' WS? type '<>' WS? '=' WS? '(' WS? elms WS? ')' optSC;

decls_impl : (WS? tscomment)* WS? 'impl' WS className class_typeparams WS? contextBoundsList WS? decls_impl_body;
decls_impl_body : '{' WS? decls_definition* WS? '}';

//Expressions
expression : block;
block : ('{' WS? expression WS? '}' WS?) | logical;
logical : comparison (WS? logical_ops WS? comparison)?;
expressions : expression (WS? ',' WS? expression)*;

comparison : additive (WS? comparison_ops WS? additive)?;
additive : multiplicative (WS? addve_ops WS? multiplicative)*;
multiplicative : infix (WS? multipve_ops WS? infix)*;
infix : extended (WS? '`' qualifiedDefinitionName  '`' WS? extended)?;
extended : unary (WS? extbin_ops WS? unary)?;
unary : (unary_ops WS? unary) | ascribe;
ascribe : e_fList (WS? ':' WS? type)?;

e_primary : e_letMatch | e_ifThenElse | e_match | e_switch |
				e_tag | e_lambda | e_tuple | e_fNil |
				 e_fVec | e_fSet | e_fMap | literal |
				existential | universal  | e_qname |
				e_unaryLambda | e_wild | e_sname | e_userError;

e_letMatch : 'let' WS pattern WS? '=' WS? expression WS? ';' WS? expression;
e_ifThenElse : 'if' WS? '(' WS? expression WS? ')' WS? expression WS 'else' WS expression;
e_match : 'match' WS expression WS 'with' WS '{' WS? match_rules WS? '}';
e_switch : 'switch' WS '{' WS? switch_rules WS?'}';

e_apply : e_primary (WS? '(' WS? expressions? WS? ')')?;

e_sname : variableName;
e_qname : qualifiedDefinitionName;
e_tag : (qualifiedTypeName '.')? tagName (WS? e_tuple)?;
e_tuple : '(' WS? expressions? WS? ')';

e_keyValue : expression WS? '->' WS? expression;
e_keyValues : e_keyValue (WS? ',' WS? e_keyValue)*;

e_userError : '???';
e_wild : Wild;
e_fNil : FNil;
e_fList : e_apply (WS? '::' WS? expression)?;
e_fVec : '#[' WS? expressions? WS? ']';
e_fSet : '#{' WS? expressions? WS? '}';
e_fMap : '@{' WS? e_keyValues? WS? '}';

e_unaryLambda : variableName WS? '->' WS? expression;
e_lambda : '(' WS? variableNames WS? ')' WS? '->' WS? expression;

existential : ('∃' | '\\exists') WS? formalparams WS? '.' WS? expression;
universal : ('∀' | '\\forall') WS? formalparams WS? '.' WS? expression;



//Patterns
pattern : simple (WS? '::' WS? pattern)?;
patterns : pattern (WS? ',' WS? pattern)*;
simple : p_fNil | literal | p_variable |
		Wild | p_tag | p_tuple | p_fVec | p_fSet | p_fMap;

p_keyValue : pattern WS? '->' WS? pattern;
p_keyValues : p_keyValue (WS? ',' WS? p_keyValue)*;

p_tag : (qualifiedTypeName '.')? tagName (WS? pattern)?;
p_tuple : '(' WS? patterns? WS? ')';

p_wild : Wild;
p_fNil : FNil;
p_variable : ident;
p_fVec : '#[' WS? patterns? (WS? ',' WS? '...')? WS? ']';
p_fSet : '#{' WS? patterns? (WS? ',' WS? '...')? WS? '}';
p_fMap : '@{' WS? p_keyValues? (WS? ',' WS? '...')? WS? '}';



//Literals
bools : ('true' | 'false');

Chars : '\'' . '\'';

Strs : '"' ~['"'\n\r]* '"';

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

literal : (bools | Chars | floats | ints | Strs);

 

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