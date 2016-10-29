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
tscomment : sp TripleSlashComment sp;
fragment MultiLineComment : '/*' .*? '*/';

WS : (' ' | '\t' | NewLine | Comment)+;
SC : ';';
Comment : SingleLineComment | MultiLineComment;
LowerIdent : [a-z][a-zA-Z0-9_"'"]*;
UpperIdent : [A-Z][a-zA-Z0-9_"'"]*;

//Keywords/symbols that always evaluate to one token
FNil : 'Nil';
Wild : '_';

//Empty rule used to generate Source Positions
sp : ;

//Root rule
start : s_import* decl* WS? EOF;

optSC : (WS? SC)?;

//Names & Identifiers
ident : sp (UpperIdent | LowerIdent) sp;
nname : sp ident ('.' ident)* sp;
lowerqname : sp (nname '/')? LowerIdent sp;
upperqname : sp (nname '/')? UpperIdent sp;

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

argument : sp variableName ':' WS? type sp;
arguments : argument (WS? ',' WS? argument)*;
formalparams : ('(' WS? arguments? WS? ')' )?;

attribute : sp attributeName WS? ':' WS? type sp;
attributes : attribute (WS? ',' WS? attribute)*;

index : '{' WS? (attributeName (WS? ',' WS? attributeName)*)? WS? '}';
indexes : index (WS? ',' WS? index)*;

idents : ident (WS? ',' WS? ident)*;

match_rule : 'case' WS pattern WS? '=>' WS? expression SC?;
match_rules : match_rule (WS? match_rule)*;

switch_rule : 'case' WS expression WS? '=>' WS? expression SC?;
switch_rules : switch_rule (WS? switch_rule)*;

typeparam : sp variableName (WS? ':' WS? type)? sp;
typeparams : ('[' WS? typeparam (WS? ',' WS? typeparam)* ']')?;

class_typeparams : '[' type (WS? ',' WS? type)* ']';

contextBound : sp className class_typeparams sp;
contextBounds : contextBound (WS? ',' WS? contextBound)*;
contextBoundsList : (WS? '<=' WS? contextBounds WS?)?;

annotation : sp '@' annotationName sp;
annotations : annotation (WS annotation)*;

//Imports
s_import : 	WS? (import_wildcard |
			import_definition |
			import_namespace);

import_wildcard : sp 'import' WS nname '/' Wild optSC sp;
import_definition : sp 'import' WS nname '/' ident optSC sp;
import_namespace : sp 'import' WS nname optSC sp;


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


decls_namespace : WS? sp 'namespace' WS nname WS? '{' WS? decl* WS? '}' sp optSC;

decls_enum : (WS? tscomment)* WS? sp 'enum' WS typeName typeparams WS? '{' WS? dcases WS? '}' sp optSC;
dcases : dcase (WS? ',' WS? dcase)*;
dcase : sp 'case' WS tagName type? sp;

decls_relation : (WS? tscomment)* WS? sp 'rel' WS tableName WS? '(' WS? attributes? WS? ')' sp optSC;

decls_lattice : (WS? tscomment)* WS? sp 'lat' WS tableName WS? '(' WS? attributes? WS? ')' sp optSC;

decls_index : WS? sp 'index' WS qualifiedTableName WS? '(' WS? indexes? WS? ')' sp optSC;

decls_signature : (WS? tscomment)* WS? sp 'def' WS definitionName WS? formalparams WS? ':' WS? type sp optSC;

decls_external : (WS? tscomment)* WS? sp 'external' WS? 'def' WS definitionName WS? formalparams WS? ':' WS? type sp optSC;

decls_definition : (WS? tscomment)* WS? annotations? WS? sp 'def' WS definitionName WS? typeparams formalparams WS? ':' WS? type WS? '=' WS? expression sp optSC;

decls_law : (WS? tscomment)* WS? sp 'law' WS definitionName WS? typeparams WS? formalparams  WS? ':' WS? type WS? '=' WS? expression sp optSC;

decls_class : (WS? tscomment)* WS? sp 'class' WS className class_typeparams WS? contextBoundsList WS? class_body sp;

class_body : '{' WS? ((decls_definition | decls_signature | decls_law) WS?) '}';

decls_fact : WS? sp predicate WS? '.' sp;

decls_rule : WS? sp predicate WS? ':-' WS? predicates WS? '.' sp;

elms : expressions;
decls_letlattice : WS? sp 'let' WS? type '<>' WS? '=' WS? '(' WS? elms WS? ')' sp optSC;

decls_impl : (WS? tscomment)* WS? sp 'impl' WS className class_typeparams WS? contextBoundsList WS? decls_impl_body sp;
decls_impl_body : '{' WS? decls_definition* WS? '}';

//Expressions
expression : block;
block : ('{' WS? expression WS? '}' WS?) | logical;
logical : comparison (WS? logical_ops WS? comparison)? sp;
expressions : expression (WS? ',' WS? expression)*;

comparison : additive (WS? comparison_ops WS? additive)? sp;
additive : multiplicative (WS? addve_ops WS? multiplicative)* sp;
multiplicative : infix (WS? multipve_ops WS? infix)* sp;
infix : extended (WS? '`' qualifiedDefinitionName  '`' WS? extended)? sp;
extended : unary (WS? extbin_ops WS? unary)? sp;
unary : (sp unary_ops WS? unary sp) | ascribe;
ascribe : e_fList (WS? ':' WS? type)? sp;

e_primary : e_letMatch | e_ifThenElse | e_match | e_switch |
				e_tag | e_lambda | e_tuple | e_fNil |
				 e_fVec | e_fSet | e_fMap | literal |
				existential | universal  | e_qname |
				e_unaryLambda | e_wild | e_sname | e_userError;

e_letMatch : sp 'let' WS pattern WS? '=' WS? expression WS? ';' WS? expression sp;
e_ifThenElse : sp 'if' WS? '(' WS? expression WS? ')' WS? expression WS 'else' WS expression sp;
e_match : sp 'match' WS expression WS 'with' WS '{' WS? match_rules WS? '}' sp;
e_switch : sp 'switch' WS '{' WS? switch_rules WS?'}' sp;

e_apply : e_primary (WS? '(' WS? expressions? WS? ')')? sp;

e_sname : sp variableName sp;
e_qname : sp qualifiedDefinitionName sp;
e_tag : sp (qualifiedTypeName '.')? tagName (WS? e_tuple)? sp;
e_tuple : sp '(' WS? expressions? WS? ')' sp;

e_keyValue : expression WS? '->' WS? expression;
e_keyValues : e_keyValue (WS? ',' WS? e_keyValue)*;

e_userError : sp '???' sp;
e_wild : sp Wild sp;
e_fNil : sp FNil sp;
e_fList : e_apply (WS? '::' WS? expression)? sp;
e_fVec : sp '#[' WS? expressions? WS? ']' sp;
e_fSet : sp '#{' WS? expressions? WS? '}' sp;
e_fMap : sp '@{' WS? e_keyValues? WS? '}' sp;

e_unaryLambda : sp variableName WS? '->' WS? expression sp;
e_lambda : sp '(' WS? variableNames WS? ')' WS? '->' WS? expression sp;

existential : sp ('∃' | '\\exists') WS? formalparams WS? '.' WS? expression sp;
universal : sp ('∀' | '\\forall') WS? formalparams WS? '.' WS? expression sp;



//Patterns
pattern : simple (WS? '::' WS? pattern)? sp;
patterns : pattern (WS? ',' WS? pattern)*;
simple : p_fNil | literal | p_variable |
		Wild | p_tag | p_tuple | p_fVec | p_fSet | p_fMap;

p_keyValue : pattern WS? '->' WS? pattern;
p_keyValues : p_keyValue (WS? ',' WS? p_keyValue)*;

p_tag : sp (qualifiedTypeName '.')? tagName (WS? pattern)? sp;
p_tuple : sp '(' WS? patterns? WS? ')' sp;

p_wild : sp Wild sp;
p_fNil : sp FNil sp;
p_variable : sp ident sp;
p_fVec : sp '#[' WS? patterns? (WS? ',' WS? '...')? WS? ']' sp;
p_fSet : sp '#{' WS? patterns? (WS? ',' WS? '...')? WS? '}' sp;
p_fMap : sp '@{' WS? p_keyValues? (WS? ',' WS? '...')? WS? '}' sp;



//Literals
bools : sp ('true' | 'false') sp;

chars : sp '\'' . '\'' sp;

strs : sp '"' ( '\"' | ~( '"' | '\n' | '\r' ) )* '"' sp;

Digits : [0-9]+;
negative : '-';

float32 : sp negative? Digits '.' Digits 'f32' sp;
float64 : sp negative? Digits '.' Digits 'f64' sp;
floatDefault : sp negative? Digits '.' Digits sp;
floats : float32 | float64 | floatDefault;

int8 : sp negative? Digits 'i8' sp;
int16 : sp negative? Digits 'i16' sp;
int32 : sp negative? Digits 'i32' sp;
int64 : sp negative? Digits 'i64' sp;
bigInt : sp negative? Digits 'ii' sp;
intDefault : sp negative? Digits sp;
ints : int8 | int16 | int32 | int64 | bigInt | intDefault;

literal : sp (bools | chars | floats | ints | strs) sp;

 

//Types
primary : arrow | tuple | apply | var | ref;

var : sp variableName sp;
ref : sp qualifiedTypeName sp;

type : sp primary (WS? '->' WS? type)? sp;
arrow : sp '(' WS? type (WS? ',' WS? type)* WS? ')' WS? '->' WS? type sp;

tuple_unit : sp '(' ')' sp;
tuple_singleton : '(' WS? type WS? ')';
tuple_multi : sp '(' WS? type (WS? ',' WS? type)+ WS? ')' sp;
tuple : tuple_unit | tuple_singleton | tuple_multi;


apply : sp ref WS? '[' WS? type (WS? ',' WS? type)* WS? ']' WS? sp;

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


pred_true : sp 'true' sp;
pred_false : sp 'false' sp;
pred_filter : sp qualifiedDefinitionName WS? '(' expressions ')' sp;
pred_table : sp qualifiedTableName WS? '(' expressions ')' sp;
pred_notequal : sp variableName WS? '!=' WS? variableName sp;
pred_loop : sp variableName WS? '<-' WS? expression sp;