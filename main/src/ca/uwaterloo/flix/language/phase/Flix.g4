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

PWS : (' ' | '\t' | NewLine | Comment)+;
ws : (PWS | TripleSlashComment)+;

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
start : s_import* decl* ws? EOF;

optSC : (ws? SC)?;

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
variableNames : variableName (ws? ',' ws? variableName)*;

argument : variableName ':' ws? type;
arguments : argument (ws? ',' ws? argument)*;
formalparams : ('(' ws? arguments? ws? ')' )?;

attribute : attributeName ws? ':' ws? type;
attributes : attribute (ws? ',' ws? attribute)*;

index : '{' ws? (attributeName (ws? ',' ws? attributeName)*)? ws? '}';
indexes : index (ws? ',' ws? index)*;

match_rule : CASE ws pattern ws? '=>' ws? expression SC?;
match_rules : match_rule (ws? match_rule)*;

switch_rule : CASE ws expression ws? '=>' ws? expression SC?;
switch_rules : switch_rule (ws? switch_rule)*;

typeparam : variableName (ws? ':' ws? type)?;
typeparams : ('[' ws? typeparam (ws? ',' ws? typeparam)* ']')?;

class_typeparams : '[' type (ws? ',' ws? type)* ']';

contextBound : className class_typeparams;
contextBounds : contextBound (ws? ',' ws? contextBound)*;
contextBoundsList : (ws? '=>' ws? contextBounds)?;
implContextBoundsList : (ws? '<=' ws? contextBounds)?;

annotation : '@' annotationName;
annotations : annotation (ws annotation)*;

//Imports
s_import : 	ws? (import_wildcard |
			import_definition |
			import_namespace);

import_wildcard : IMPORT ws nname '/' WILD optSC;
import_definition : IMPORT ws nname '/' ident optSC;
import_namespace : IMPORT ws nname optSC;


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


decls_namespace : ws? NAMESPACE ws nname ws? '{' ws? decl* ws? '}' optSC;

decls_enum : (PWS? tscomment)* ws? ENUM ws typeName typeparams ws? '{' ws? dcases ws? '}' optSC;
dcases : dcase (ws? ',' ws? dcase)*;
dcase : CASE ws tagName tuple?;

decls_relation : (PWS? tscomment)* ws? REL ws tableName ws? '(' ws? attributes? ws? ')' optSC;

decls_lattice : (PWS? tscomment)* ws? LAT ws tableName ws? '(' ws? attributes? ws? ')' optSC;

decls_index : ws? INDEX ws qualifiedTableName ws? '(' ws? indexes? ws? ')' optSC;

decls_signature : (PWS? tscomment)* ws? DEF ws definitionName ws? formalparams ws? ':' ws? type optSC;

decls_external : (PWS? tscomment)* ws? EXTERNAL ws? DEF ws definitionName ws? formalparams ws? ':' ws? type optSC;

decls_definition : (PWS? tscomment)* ws? annotations? ws? DEF ws definitionName ws? typeparams formalparams ws? ':' ws? type ws? '=' ws? expression optSC;

decls_law : (PWS? tscomment)* ws? LAW ws definitionName ws? typeparams ws? formalparams  ws? ':' ws? type ws? '=' ws? expression optSC;

decls_class : (PWS? tscomment)* ws? CLASS ws className class_typeparams contextBoundsList ws? class_body;

class_body : '{' ws? (class_decl ws?)* '}';
class_decl : decls_definition | decls_signature | decls_law ;

decls_fact : ws? predicate ws? '.';

decls_rule : ws? predicate ws? ':-' ws? predicates ws? '.';

elms : expressions;
decls_letlattice : ws? LET ws? type '<>' ws? '=' ws? '(' ws? elms ws? ')' optSC;

decls_impl : (PWS? tscomment)* ws? IMPL ws className class_typeparams ws? implContextBoundsList ws? decls_impl_body;
decls_impl_body : '{' ws? decls_definition* ws? '}';

//Expressions
expression : ('{' ws? expression ws? '}' ws?) | logical;
logical : comparison (ws? logical_ops ws? comparison)?;
expressions : expression (ws? ',' ws? expression)*;

comparison : additive (ws? comparison_ops ws? additive)?;
additive :  additive ws? addve_ops ws? multiplicative |
			multiplicative;
multiplicative : multiplicative ws? multipve_ops ws? infix
				| infix;
infix : extended (ws? '`' qualifiedDefinitionName  '`' ws? extended)?;
extended : unary (ws? extbin_ops ws? unary)?;

unary : {!( _input.LT(1).getText().equals("-") && //Make sure this isn't just a negative number
		Character.isDigit(_input.LT(2).getText().charAt(0)) )}? (unary_ops ws? unary) 
		| ascribe;
ascribe : e_fList (ws? ':' ws? type)?;

e_primary : e_letMatch | e_ifThenElse | e_match | e_switch |
				e_qname | e_tag | e_lambda | e_tuple | e_fNil |
				 e_fVec | e_fSet | e_fMap | e_literal |
				existential | universal  |
				e_unaryLambda | e_wild | e_sname | e_userError;

e_letMatch : LET ws pattern ws? '=' ws? expression ws? ';' ws? expression;
e_ifThenElse : IF ws? '(' ws? expression ws? ')' ws? expression ws ELSE ws expression;
e_match : MATCH ws expression ws WITH ws '{' ws? match_rules ws? '}';
e_switch : SWITCH ws '{' ws? switch_rules ws?'}';

e_apply : e_primary (ws? '(' ws? expressions? ws? ')')?;

e_unaryLambda : variableName ws? '->' ws? expression;
e_lambda : '(' ws? variableNames ws? ')' ws? '->' ws? expression;

e_literal : literal;
e_sname : variableName;
e_qname : qualifiedDefinitionName;
e_tag : (qualifiedTypeName '.')? tagName (ws? e_tuple)?;
e_tuple : '(' ws? expressions? ws? ')';

e_keyValue : expression ws? '->' ws? expression;
e_keyValues : e_keyValue (ws? ',' ws? e_keyValue)*;

e_userError : '???';
e_wild : WILD;
e_fNil : FNIL;
e_fList : e_apply (ws? '::' ws? expression)?;
e_fVec : '#[' ws? expressions? ws? ']';
e_fSet : '#{' ws? expressions? ws? '}';
e_fMap : '@{' ws? e_keyValues? ws? '}';

existential : ('∃' | '\\exists') ws? formalparams ws? '.' ws? expression;
universal : ('∀' | '\\forall') ws? formalparams ws? '.' ws? expression;



//Patterns
pattern : p_fList;
patterns : pattern (ws? ',' ws? pattern)*;
simple : p_fNil | p_literal | p_variable |
		p_wild | p_tag | p_tuple | p_fVec | p_fSet | p_fMap;

p_keyValue : pattern ws? '->' ws? pattern;
p_keyValues : p_keyValue (ws? ',' ws? p_keyValue)*;

p_literal : literal;
p_tag : (qualifiedTypeName '.')? tagName (ws? pattern)?;
p_tuple : '(' ws? patterns? ws? ')';

p_wild : WILD;
p_fNil : FNIL;
p_variable : variableName;
p_fList : simple (ws? '::' ws? pattern)?;
p_fVec : '#[' ws? patterns? (ws? ',' ws? pattern '...')? ws? ']';
p_fSet : '#{' ws? patterns? (ws? ',' ws? pattern '...')? ws? '}';
p_fMap : '@{' ws? p_keyValues? (ws? ',' ws? pattern '...')? ws? '}';



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

type : primary (ws? '->' ws? type)?;
arrow : '(' ws? type (ws? ',' ws? type)* ws? ')' ws? '->' ws? type;

tuple_unit : '(' ')';
tuple_singleton : '(' ws? type ws? ')';
tuple_multi : '(' ws? type (ws? ',' ws? type)+ ws? ')';
tuple : tuple_unit | tuple_singleton | tuple_multi;


apply : ref ws? '[' ws? type (ws? ',' ws? type)* ws? ']' ws?;

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
predicates : predicate (ws? ',' ws? predicate)*;


pred_true : 'true';
pred_false : 'false';
pred_filter : qualifiedDefinitionName ws? '(' expressions ')';
pred_table : qualifiedTableName ws? '(' expressions ')';
pred_notequal : variableName ws? '!=' ws? variableName;
pred_loop : variableName ws? '<-' ws? expression;