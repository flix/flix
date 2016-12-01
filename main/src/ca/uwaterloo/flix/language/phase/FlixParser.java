// Generated from Flix.g4 by ANTLR 4.5.3

package ca.uwaterloo.flix.language.phase;

import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class FlixParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.5.3", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, T__5=6, T__6=7, T__7=8, T__8=9, 
		T__9=10, T__10=11, T__11=12, T__12=13, T__13=14, T__14=15, T__15=16, T__16=17, 
		T__17=18, T__18=19, T__19=20, T__20=21, T__21=22, T__22=23, T__23=24, 
		T__24=25, T__25=26, T__26=27, T__27=28, T__28=29, T__29=30, T__30=31, 
		T__31=32, T__32=33, T__33=34, T__34=35, T__35=36, T__36=37, T__37=38, 
		T__38=39, T__39=40, T__40=41, T__41=42, T__42=43, T__43=44, T__44=45, 
		T__45=46, T__46=47, T__47=48, T__48=49, T__49=50, T__50=51, T__51=52, 
		T__52=53, T__53=54, T__54=55, T__55=56, T__56=57, T__57=58, T__58=59, 
		T__59=60, T__60=61, T__61=62, T__62=63, T__63=64, T__64=65, T__65=66, 
		T__66=67, T__67=68, T__68=69, T__69=70, TripleSlashComment=71, PWS=72, 
		SC=73, Comment=74, ENUM=75, NAMESPACE=76, REL=77, LAT=78, INDEX=79, DEF=80, 
		EXTERNAL=81, LAW=82, CLASS=83, LET=84, IMPL=85, FNIL=86, SWITCH=87, MATCH=88, 
		WITH=89, WILD=90, CASE=91, IF=92, ELSE=93, IMPORT=94, LowerIdent=95, UpperIdent=96, 
		Chars=97, Strs=98, Digits=99;
	public static final int
		RULE_tscomment = 0, RULE_ws = 1, RULE_start = 2, RULE_optSC = 3, RULE_ident = 4, 
		RULE_nname = 5, RULE_lowerqname = 6, RULE_upperqname = 7, RULE_annotationName = 8, 
		RULE_attributeName = 9, RULE_className = 10, RULE_definitionName = 11, 
		RULE_qualifiedDefinitionName = 12, RULE_tableName = 13, RULE_qualifiedTableName = 14, 
		RULE_tagName = 15, RULE_typeName = 16, RULE_qualifiedTypeName = 17, RULE_variableName = 18, 
		RULE_variableNames = 19, RULE_argument = 20, RULE_arguments = 21, RULE_formalparams = 22, 
		RULE_attribute = 23, RULE_attributes = 24, RULE_index = 25, RULE_indexes = 26, 
		RULE_match_rule = 27, RULE_match_rules = 28, RULE_switch_rule = 29, RULE_switch_rules = 30, 
		RULE_typeparam = 31, RULE_typeparams = 32, RULE_class_typeparams = 33, 
		RULE_contextBound = 34, RULE_contextBounds = 35, RULE_contextBoundsList = 36, 
		RULE_implContextBoundsList = 37, RULE_annotation = 38, RULE_annotations = 39, 
		RULE_s_import = 40, RULE_import_wildcard = 41, RULE_import_definition = 42, 
		RULE_import_namespace = 43, RULE_decl = 44, RULE_decls_namespace = 45, 
		RULE_decls_enum = 46, RULE_dcases = 47, RULE_dcase = 48, RULE_decls_relation = 49, 
		RULE_decls_lattice = 50, RULE_decls_index = 51, RULE_decls_signature = 52, 
		RULE_decls_external = 53, RULE_decls_definition = 54, RULE_decls_law = 55, 
		RULE_decls_class = 56, RULE_class_body = 57, RULE_class_decl = 58, RULE_decls_fact = 59, 
		RULE_decls_rule = 60, RULE_elms = 61, RULE_decls_letlattice = 62, RULE_decls_impl = 63, 
		RULE_decls_impl_body = 64, RULE_expression = 65, RULE_logical = 66, RULE_expressions = 67, 
		RULE_comparison = 68, RULE_additive = 69, RULE_multiplicative = 70, RULE_infix = 71, 
		RULE_extended = 72, RULE_unary = 73, RULE_ascribe = 74, RULE_e_primary = 75, 
		RULE_e_letMatch = 76, RULE_e_ifThenElse = 77, RULE_e_match = 78, RULE_e_switch = 79, 
		RULE_e_apply = 80, RULE_e_unaryLambda = 81, RULE_e_lambda = 82, RULE_e_literal = 83, 
		RULE_e_sname = 84, RULE_e_qname = 85, RULE_e_tag = 86, RULE_e_tuple = 87, 
		RULE_e_keyValue = 88, RULE_e_keyValues = 89, RULE_e_userError = 90, RULE_e_wild = 91, 
		RULE_e_fNil = 92, RULE_e_fList = 93, RULE_e_fVec = 94, RULE_e_fSet = 95, 
		RULE_e_fMap = 96, RULE_existential = 97, RULE_universal = 98, RULE_pattern = 99, 
		RULE_patterns = 100, RULE_simple = 101, RULE_p_keyValue = 102, RULE_p_keyValues = 103, 
		RULE_p_literal = 104, RULE_p_tag = 105, RULE_p_tuple = 106, RULE_p_wild = 107, 
		RULE_p_fNil = 108, RULE_p_variable = 109, RULE_p_fList = 110, RULE_p_fVec = 111, 
		RULE_p_fSet = 112, RULE_p_fMap = 113, RULE_bools = 114, RULE_chars = 115, 
		RULE_strs = 116, RULE_negative = 117, RULE_float32 = 118, RULE_float64 = 119, 
		RULE_floatDefault = 120, RULE_floats = 121, RULE_int8 = 122, RULE_int16 = 123, 
		RULE_int32 = 124, RULE_int64 = 125, RULE_bigInt = 126, RULE_intDefault = 127, 
		RULE_ints = 128, RULE_literal = 129, RULE_primary = 130, RULE_var = 131, 
		RULE_ref = 132, RULE_type = 133, RULE_arrow = 134, RULE_tuple_unit = 135, 
		RULE_tuple_singleton = 136, RULE_tuple_multi = 137, RULE_tuple = 138, 
		RULE_apply = 139, RULE_unary_ops = 140, RULE_logical_ops = 141, RULE_comparison_ops = 142, 
		RULE_multipve_ops = 143, RULE_addve_ops = 144, RULE_extbin_ops = 145, 
		RULE_predicate = 146, RULE_predicates = 147, RULE_pred_true = 148, RULE_pred_false = 149, 
		RULE_pred_filter = 150, RULE_pred_table = 151, RULE_pred_notequal = 152, 
		RULE_pred_loop = 153;
	public static final String[] ruleNames = {
		"tscomment", "ws", "start", "optSC", "ident", "nname", "lowerqname", "upperqname", 
		"annotationName", "attributeName", "className", "definitionName", "qualifiedDefinitionName", 
		"tableName", "qualifiedTableName", "tagName", "typeName", "qualifiedTypeName", 
		"variableName", "variableNames", "argument", "arguments", "formalparams", 
		"attribute", "attributes", "index", "indexes", "match_rule", "match_rules", 
		"switch_rule", "switch_rules", "typeparam", "typeparams", "class_typeparams", 
		"contextBound", "contextBounds", "contextBoundsList", "implContextBoundsList", 
		"annotation", "annotations", "s_import", "import_wildcard", "import_definition", 
		"import_namespace", "decl", "decls_namespace", "decls_enum", "dcases", 
		"dcase", "decls_relation", "decls_lattice", "decls_index", "decls_signature", 
		"decls_external", "decls_definition", "decls_law", "decls_class", "class_body", 
		"class_decl", "decls_fact", "decls_rule", "elms", "decls_letlattice", 
		"decls_impl", "decls_impl_body", "expression", "logical", "expressions", 
		"comparison", "additive", "multiplicative", "infix", "extended", "unary", 
		"ascribe", "e_primary", "e_letMatch", "e_ifThenElse", "e_match", "e_switch", 
		"e_apply", "e_unaryLambda", "e_lambda", "e_literal", "e_sname", "e_qname", 
		"e_tag", "e_tuple", "e_keyValue", "e_keyValues", "e_userError", "e_wild", 
		"e_fNil", "e_fList", "e_fVec", "e_fSet", "e_fMap", "existential", "universal", 
		"pattern", "patterns", "simple", "p_keyValue", "p_keyValues", "p_literal", 
		"p_tag", "p_tuple", "p_wild", "p_fNil", "p_variable", "p_fList", "p_fVec", 
		"p_fSet", "p_fMap", "bools", "chars", "strs", "negative", "float32", "float64", 
		"floatDefault", "floats", "int8", "int16", "int32", "int64", "bigInt", 
		"intDefault", "ints", "literal", "primary", "var", "ref", "type", "arrow", 
		"tuple_unit", "tuple_singleton", "tuple_multi", "tuple", "apply", "unary_ops", 
		"logical_ops", "comparison_ops", "multipve_ops", "addve_ops", "extbin_ops", 
		"predicate", "predicates", "pred_true", "pred_false", "pred_filter", "pred_table", 
		"pred_notequal", "pred_loop"
	};

	private static final String[] _LITERAL_NAMES = {
		null, "'.'", "'/'", "','", "':'", "'('", "')'", "'{'", "'}'", "'=>'", 
		"'['", "']'", "'<='", "'@'", "'='", "':-'", "'<>'", "'`'", "'->'", "'???'", 
		"'::'", "'#['", "'#{'", "'@{'", "'∃'", "'\\exists'", "'∀'", "'\\forall'", 
		"'...'", "'true'", "'false'", "'-'", "'f32'", "'f64'", "'i8'", "'i16'", 
		"'i32'", "'i64'", "'ii'", "'+'", "'¬'", "'~'", "'!'", "'&&'", "'||'", 
		"'&'", "'|'", "'==>'", "'<==>'", "'^'", "'<<'", "'>>'", "'∧'", "'∨'", 
		"'→'", "'↔'", "'>='", "'<'", "'>'", "'=='", "'!='", "'≡'", "'**'", "'*'", 
		"'%'", "'⊑'", "'⊔'", "'⊓'", "'▽'", "'△'", "'<-'", null, null, "';'", null, 
		"'enum'", "'namespace'", "'rel'", "'lat'", "'index'", "'def'", "'external'", 
		"'law'", "'class'", "'let'", "'impl'", "'Nil'", "'switch'", "'match'", 
		"'with'", "'_'", "'case'", "'if'", "'else'", "'import'"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, "TripleSlashComment", 
		"PWS", "SC", "Comment", "ENUM", "NAMESPACE", "REL", "LAT", "INDEX", "DEF", 
		"EXTERNAL", "LAW", "CLASS", "LET", "IMPL", "FNIL", "SWITCH", "MATCH", 
		"WITH", "WILD", "CASE", "IF", "ELSE", "IMPORT", "LowerIdent", "UpperIdent", 
		"Chars", "Strs", "Digits"
	};
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "Flix.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public FlixParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}
	public static class TscommentContext extends ParserRuleContext {
		public TerminalNode TripleSlashComment() { return getToken(FlixParser.TripleSlashComment, 0); }
		public TscommentContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_tscomment; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterTscomment(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitTscomment(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitTscomment(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TscommentContext tscomment() throws RecognitionException {
		TscommentContext _localctx = new TscommentContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_tscomment);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(308);
			match(TripleSlashComment);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class WsContext extends ParserRuleContext {
		public List<TerminalNode> PWS() { return getTokens(FlixParser.PWS); }
		public TerminalNode PWS(int i) {
			return getToken(FlixParser.PWS, i);
		}
		public List<TerminalNode> TripleSlashComment() { return getTokens(FlixParser.TripleSlashComment); }
		public TerminalNode TripleSlashComment(int i) {
			return getToken(FlixParser.TripleSlashComment, i);
		}
		public WsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_ws; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterWs(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitWs(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitWs(this);
			else return visitor.visitChildren(this);
		}
	}

	public final WsContext ws() throws RecognitionException {
		WsContext _localctx = new WsContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_ws);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(311); 
			_errHandler.sync(this);
			_alt = 1;
			do {
				switch (_alt) {
				case 1:
					{
					{
					setState(310);
					_la = _input.LA(1);
					if ( !(_la==TripleSlashComment || _la==PWS) ) {
					_errHandler.recoverInline(this);
					} else {
						consume();
					}
					}
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(313); 
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,0,_ctx);
			} while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER );
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class StartContext extends ParserRuleContext {
		public TerminalNode EOF() { return getToken(FlixParser.EOF, 0); }
		public List<S_importContext> s_import() {
			return getRuleContexts(S_importContext.class);
		}
		public S_importContext s_import(int i) {
			return getRuleContext(S_importContext.class,i);
		}
		public List<DeclContext> decl() {
			return getRuleContexts(DeclContext.class);
		}
		public DeclContext decl(int i) {
			return getRuleContext(DeclContext.class,i);
		}
		public WsContext ws() {
			return getRuleContext(WsContext.class,0);
		}
		public StartContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_start; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterStart(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitStart(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitStart(this);
			else return visitor.visitChildren(this);
		}
	}

	public final StartContext start() throws RecognitionException {
		StartContext _localctx = new StartContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_start);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(318);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,1,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(315);
					s_import();
					}
					} 
				}
				setState(320);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,1,_ctx);
			}
			setState(324);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,2,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(321);
					decl();
					}
					} 
				}
				setState(326);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,2,_ctx);
			}
			setState(328);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(327);
				ws();
				}
			}

			setState(330);
			match(EOF);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class OptSCContext extends ParserRuleContext {
		public TerminalNode SC() { return getToken(FlixParser.SC, 0); }
		public WsContext ws() {
			return getRuleContext(WsContext.class,0);
		}
		public OptSCContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_optSC; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterOptSC(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitOptSC(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitOptSC(this);
			else return visitor.visitChildren(this);
		}
	}

	public final OptSCContext optSC() throws RecognitionException {
		OptSCContext _localctx = new OptSCContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_optSC);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(336);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,5,_ctx) ) {
			case 1:
				{
				setState(333);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(332);
					ws();
					}
				}

				setState(335);
				match(SC);
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class IdentContext extends ParserRuleContext {
		public TerminalNode UpperIdent() { return getToken(FlixParser.UpperIdent, 0); }
		public TerminalNode LowerIdent() { return getToken(FlixParser.LowerIdent, 0); }
		public IdentContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_ident; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterIdent(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitIdent(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitIdent(this);
			else return visitor.visitChildren(this);
		}
	}

	public final IdentContext ident() throws RecognitionException {
		IdentContext _localctx = new IdentContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_ident);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(338);
			_la = _input.LA(1);
			if ( !(_la==LowerIdent || _la==UpperIdent) ) {
			_errHandler.recoverInline(this);
			} else {
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class NnameContext extends ParserRuleContext {
		public List<IdentContext> ident() {
			return getRuleContexts(IdentContext.class);
		}
		public IdentContext ident(int i) {
			return getRuleContext(IdentContext.class,i);
		}
		public NnameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_nname; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterNname(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitNname(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitNname(this);
			else return visitor.visitChildren(this);
		}
	}

	public final NnameContext nname() throws RecognitionException {
		NnameContext _localctx = new NnameContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_nname);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(340);
			ident();
			setState(345);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__0) {
				{
				{
				setState(341);
				match(T__0);
				setState(342);
				ident();
				}
				}
				setState(347);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class LowerqnameContext extends ParserRuleContext {
		public TerminalNode LowerIdent() { return getToken(FlixParser.LowerIdent, 0); }
		public NnameContext nname() {
			return getRuleContext(NnameContext.class,0);
		}
		public LowerqnameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_lowerqname; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterLowerqname(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitLowerqname(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitLowerqname(this);
			else return visitor.visitChildren(this);
		}
	}

	public final LowerqnameContext lowerqname() throws RecognitionException {
		LowerqnameContext _localctx = new LowerqnameContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_lowerqname);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(351);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,7,_ctx) ) {
			case 1:
				{
				setState(348);
				nname();
				setState(349);
				match(T__1);
				}
				break;
			}
			setState(353);
			match(LowerIdent);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class UpperqnameContext extends ParserRuleContext {
		public TerminalNode UpperIdent() { return getToken(FlixParser.UpperIdent, 0); }
		public NnameContext nname() {
			return getRuleContext(NnameContext.class,0);
		}
		public UpperqnameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_upperqname; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterUpperqname(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitUpperqname(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitUpperqname(this);
			else return visitor.visitChildren(this);
		}
	}

	public final UpperqnameContext upperqname() throws RecognitionException {
		UpperqnameContext _localctx = new UpperqnameContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_upperqname);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(358);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,8,_ctx) ) {
			case 1:
				{
				setState(355);
				nname();
				setState(356);
				match(T__1);
				}
				break;
			}
			setState(360);
			match(UpperIdent);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class AnnotationNameContext extends ParserRuleContext {
		public TerminalNode LowerIdent() { return getToken(FlixParser.LowerIdent, 0); }
		public AnnotationNameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_annotationName; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterAnnotationName(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitAnnotationName(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitAnnotationName(this);
			else return visitor.visitChildren(this);
		}
	}

	public final AnnotationNameContext annotationName() throws RecognitionException {
		AnnotationNameContext _localctx = new AnnotationNameContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_annotationName);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(362);
			match(LowerIdent);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class AttributeNameContext extends ParserRuleContext {
		public TerminalNode LowerIdent() { return getToken(FlixParser.LowerIdent, 0); }
		public AttributeNameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_attributeName; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterAttributeName(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitAttributeName(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitAttributeName(this);
			else return visitor.visitChildren(this);
		}
	}

	public final AttributeNameContext attributeName() throws RecognitionException {
		AttributeNameContext _localctx = new AttributeNameContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_attributeName);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(364);
			match(LowerIdent);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ClassNameContext extends ParserRuleContext {
		public TerminalNode UpperIdent() { return getToken(FlixParser.UpperIdent, 0); }
		public ClassNameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_className; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterClassName(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitClassName(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitClassName(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ClassNameContext className() throws RecognitionException {
		ClassNameContext _localctx = new ClassNameContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_className);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(366);
			match(UpperIdent);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class DefinitionNameContext extends ParserRuleContext {
		public TerminalNode LowerIdent() { return getToken(FlixParser.LowerIdent, 0); }
		public DefinitionNameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_definitionName; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterDefinitionName(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitDefinitionName(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitDefinitionName(this);
			else return visitor.visitChildren(this);
		}
	}

	public final DefinitionNameContext definitionName() throws RecognitionException {
		DefinitionNameContext _localctx = new DefinitionNameContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_definitionName);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(368);
			match(LowerIdent);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class QualifiedDefinitionNameContext extends ParserRuleContext {
		public LowerqnameContext lowerqname() {
			return getRuleContext(LowerqnameContext.class,0);
		}
		public QualifiedDefinitionNameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_qualifiedDefinitionName; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterQualifiedDefinitionName(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitQualifiedDefinitionName(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitQualifiedDefinitionName(this);
			else return visitor.visitChildren(this);
		}
	}

	public final QualifiedDefinitionNameContext qualifiedDefinitionName() throws RecognitionException {
		QualifiedDefinitionNameContext _localctx = new QualifiedDefinitionNameContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_qualifiedDefinitionName);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(370);
			lowerqname();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TableNameContext extends ParserRuleContext {
		public TerminalNode UpperIdent() { return getToken(FlixParser.UpperIdent, 0); }
		public TableNameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_tableName; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterTableName(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitTableName(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitTableName(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TableNameContext tableName() throws RecognitionException {
		TableNameContext _localctx = new TableNameContext(_ctx, getState());
		enterRule(_localctx, 26, RULE_tableName);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(372);
			match(UpperIdent);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class QualifiedTableNameContext extends ParserRuleContext {
		public UpperqnameContext upperqname() {
			return getRuleContext(UpperqnameContext.class,0);
		}
		public QualifiedTableNameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_qualifiedTableName; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterQualifiedTableName(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitQualifiedTableName(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitQualifiedTableName(this);
			else return visitor.visitChildren(this);
		}
	}

	public final QualifiedTableNameContext qualifiedTableName() throws RecognitionException {
		QualifiedTableNameContext _localctx = new QualifiedTableNameContext(_ctx, getState());
		enterRule(_localctx, 28, RULE_qualifiedTableName);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(374);
			upperqname();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TagNameContext extends ParserRuleContext {
		public TerminalNode UpperIdent() { return getToken(FlixParser.UpperIdent, 0); }
		public TerminalNode FNIL() { return getToken(FlixParser.FNIL, 0); }
		public TagNameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_tagName; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterTagName(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitTagName(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitTagName(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TagNameContext tagName() throws RecognitionException {
		TagNameContext _localctx = new TagNameContext(_ctx, getState());
		enterRule(_localctx, 30, RULE_tagName);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(376);
			_la = _input.LA(1);
			if ( !(_la==FNIL || _la==UpperIdent) ) {
			_errHandler.recoverInline(this);
			} else {
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TypeNameContext extends ParserRuleContext {
		public TerminalNode UpperIdent() { return getToken(FlixParser.UpperIdent, 0); }
		public TypeNameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeName; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterTypeName(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitTypeName(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitTypeName(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TypeNameContext typeName() throws RecognitionException {
		TypeNameContext _localctx = new TypeNameContext(_ctx, getState());
		enterRule(_localctx, 32, RULE_typeName);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(378);
			match(UpperIdent);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class QualifiedTypeNameContext extends ParserRuleContext {
		public UpperqnameContext upperqname() {
			return getRuleContext(UpperqnameContext.class,0);
		}
		public QualifiedTypeNameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_qualifiedTypeName; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterQualifiedTypeName(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitQualifiedTypeName(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitQualifiedTypeName(this);
			else return visitor.visitChildren(this);
		}
	}

	public final QualifiedTypeNameContext qualifiedTypeName() throws RecognitionException {
		QualifiedTypeNameContext _localctx = new QualifiedTypeNameContext(_ctx, getState());
		enterRule(_localctx, 34, RULE_qualifiedTypeName);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(380);
			upperqname();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class VariableNameContext extends ParserRuleContext {
		public TerminalNode LowerIdent() { return getToken(FlixParser.LowerIdent, 0); }
		public VariableNameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_variableName; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterVariableName(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitVariableName(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitVariableName(this);
			else return visitor.visitChildren(this);
		}
	}

	public final VariableNameContext variableName() throws RecognitionException {
		VariableNameContext _localctx = new VariableNameContext(_ctx, getState());
		enterRule(_localctx, 36, RULE_variableName);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(382);
			match(LowerIdent);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class VariableNamesContext extends ParserRuleContext {
		public List<VariableNameContext> variableName() {
			return getRuleContexts(VariableNameContext.class);
		}
		public VariableNameContext variableName(int i) {
			return getRuleContext(VariableNameContext.class,i);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public VariableNamesContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_variableNames; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterVariableNames(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitVariableNames(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitVariableNames(this);
			else return visitor.visitChildren(this);
		}
	}

	public final VariableNamesContext variableNames() throws RecognitionException {
		VariableNamesContext _localctx = new VariableNamesContext(_ctx, getState());
		enterRule(_localctx, 38, RULE_variableNames);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(384);
			variableName();
			setState(395);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,11,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(386);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(385);
						ws();
						}
					}

					setState(388);
					match(T__2);
					setState(390);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(389);
						ws();
						}
					}

					setState(392);
					variableName();
					}
					} 
				}
				setState(397);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,11,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ArgumentContext extends ParserRuleContext {
		public VariableNameContext variableName() {
			return getRuleContext(VariableNameContext.class,0);
		}
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public WsContext ws() {
			return getRuleContext(WsContext.class,0);
		}
		public ArgumentContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_argument; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterArgument(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitArgument(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitArgument(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ArgumentContext argument() throws RecognitionException {
		ArgumentContext _localctx = new ArgumentContext(_ctx, getState());
		enterRule(_localctx, 40, RULE_argument);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(398);
			variableName();
			setState(399);
			match(T__3);
			setState(401);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(400);
				ws();
				}
			}

			setState(403);
			type();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ArgumentsContext extends ParserRuleContext {
		public List<ArgumentContext> argument() {
			return getRuleContexts(ArgumentContext.class);
		}
		public ArgumentContext argument(int i) {
			return getRuleContext(ArgumentContext.class,i);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public ArgumentsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_arguments; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterArguments(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitArguments(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitArguments(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ArgumentsContext arguments() throws RecognitionException {
		ArgumentsContext _localctx = new ArgumentsContext(_ctx, getState());
		enterRule(_localctx, 42, RULE_arguments);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(405);
			argument();
			setState(416);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,15,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(407);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(406);
						ws();
						}
					}

					setState(409);
					match(T__2);
					setState(411);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(410);
						ws();
						}
					}

					setState(413);
					argument();
					}
					} 
				}
				setState(418);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,15,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class FormalparamsContext extends ParserRuleContext {
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public ArgumentsContext arguments() {
			return getRuleContext(ArgumentsContext.class,0);
		}
		public FormalparamsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_formalparams; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterFormalparams(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitFormalparams(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitFormalparams(this);
			else return visitor.visitChildren(this);
		}
	}

	public final FormalparamsContext formalparams() throws RecognitionException {
		FormalparamsContext _localctx = new FormalparamsContext(_ctx, getState());
		enterRule(_localctx, 44, RULE_formalparams);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(430);
			_la = _input.LA(1);
			if (_la==T__4) {
				{
				setState(419);
				match(T__4);
				setState(421);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,16,_ctx) ) {
				case 1:
					{
					setState(420);
					ws();
					}
					break;
				}
				setState(424);
				_la = _input.LA(1);
				if (_la==LowerIdent) {
					{
					setState(423);
					arguments();
					}
				}

				setState(427);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(426);
					ws();
					}
				}

				setState(429);
				match(T__5);
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class AttributeContext extends ParserRuleContext {
		public AttributeNameContext attributeName() {
			return getRuleContext(AttributeNameContext.class,0);
		}
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public AttributeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_attribute; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterAttribute(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitAttribute(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitAttribute(this);
			else return visitor.visitChildren(this);
		}
	}

	public final AttributeContext attribute() throws RecognitionException {
		AttributeContext _localctx = new AttributeContext(_ctx, getState());
		enterRule(_localctx, 46, RULE_attribute);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(432);
			attributeName();
			setState(434);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(433);
				ws();
				}
			}

			setState(436);
			match(T__3);
			setState(438);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(437);
				ws();
				}
			}

			setState(440);
			type();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class AttributesContext extends ParserRuleContext {
		public List<AttributeContext> attribute() {
			return getRuleContexts(AttributeContext.class);
		}
		public AttributeContext attribute(int i) {
			return getRuleContext(AttributeContext.class,i);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public AttributesContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_attributes; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterAttributes(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitAttributes(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitAttributes(this);
			else return visitor.visitChildren(this);
		}
	}

	public final AttributesContext attributes() throws RecognitionException {
		AttributesContext _localctx = new AttributesContext(_ctx, getState());
		enterRule(_localctx, 48, RULE_attributes);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(442);
			attribute();
			setState(453);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,24,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(444);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(443);
						ws();
						}
					}

					setState(446);
					match(T__2);
					setState(448);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(447);
						ws();
						}
					}

					setState(450);
					attribute();
					}
					} 
				}
				setState(455);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,24,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class IndexContext extends ParserRuleContext {
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public List<AttributeNameContext> attributeName() {
			return getRuleContexts(AttributeNameContext.class);
		}
		public AttributeNameContext attributeName(int i) {
			return getRuleContext(AttributeNameContext.class,i);
		}
		public IndexContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_index; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterIndex(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitIndex(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitIndex(this);
			else return visitor.visitChildren(this);
		}
	}

	public final IndexContext index() throws RecognitionException {
		IndexContext _localctx = new IndexContext(_ctx, getState());
		enterRule(_localctx, 50, RULE_index);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(456);
			match(T__6);
			setState(458);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,25,_ctx) ) {
			case 1:
				{
				setState(457);
				ws();
				}
				break;
			}
			setState(474);
			_la = _input.LA(1);
			if (_la==LowerIdent) {
				{
				setState(460);
				attributeName();
				setState(471);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,28,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(462);
						_la = _input.LA(1);
						if (_la==TripleSlashComment || _la==PWS) {
							{
							setState(461);
							ws();
							}
						}

						setState(464);
						match(T__2);
						setState(466);
						_la = _input.LA(1);
						if (_la==TripleSlashComment || _la==PWS) {
							{
							setState(465);
							ws();
							}
						}

						setState(468);
						attributeName();
						}
						} 
					}
					setState(473);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,28,_ctx);
				}
				}
			}

			setState(477);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(476);
				ws();
				}
			}

			setState(479);
			match(T__7);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class IndexesContext extends ParserRuleContext {
		public List<IndexContext> index() {
			return getRuleContexts(IndexContext.class);
		}
		public IndexContext index(int i) {
			return getRuleContext(IndexContext.class,i);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public IndexesContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_indexes; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterIndexes(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitIndexes(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitIndexes(this);
			else return visitor.visitChildren(this);
		}
	}

	public final IndexesContext indexes() throws RecognitionException {
		IndexesContext _localctx = new IndexesContext(_ctx, getState());
		enterRule(_localctx, 52, RULE_indexes);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(481);
			index();
			setState(492);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,33,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(483);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(482);
						ws();
						}
					}

					setState(485);
					match(T__2);
					setState(487);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(486);
						ws();
						}
					}

					setState(489);
					index();
					}
					} 
				}
				setState(494);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,33,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Match_ruleContext extends ParserRuleContext {
		public TerminalNode CASE() { return getToken(FlixParser.CASE, 0); }
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public PatternContext pattern() {
			return getRuleContext(PatternContext.class,0);
		}
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public TerminalNode SC() { return getToken(FlixParser.SC, 0); }
		public Match_ruleContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_match_rule; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterMatch_rule(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitMatch_rule(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitMatch_rule(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Match_ruleContext match_rule() throws RecognitionException {
		Match_ruleContext _localctx = new Match_ruleContext(_ctx, getState());
		enterRule(_localctx, 54, RULE_match_rule);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(495);
			match(CASE);
			setState(496);
			ws();
			setState(497);
			pattern();
			setState(499);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(498);
				ws();
				}
			}

			setState(501);
			match(T__8);
			setState(503);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,35,_ctx) ) {
			case 1:
				{
				setState(502);
				ws();
				}
				break;
			}
			setState(505);
			expression();
			setState(507);
			_la = _input.LA(1);
			if (_la==SC) {
				{
				setState(506);
				match(SC);
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Match_rulesContext extends ParserRuleContext {
		public List<Match_ruleContext> match_rule() {
			return getRuleContexts(Match_ruleContext.class);
		}
		public Match_ruleContext match_rule(int i) {
			return getRuleContext(Match_ruleContext.class,i);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public Match_rulesContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_match_rules; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterMatch_rules(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitMatch_rules(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitMatch_rules(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Match_rulesContext match_rules() throws RecognitionException {
		Match_rulesContext _localctx = new Match_rulesContext(_ctx, getState());
		enterRule(_localctx, 56, RULE_match_rules);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(509);
			match_rule();
			setState(516);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,38,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(511);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(510);
						ws();
						}
					}

					setState(513);
					match_rule();
					}
					} 
				}
				setState(518);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,38,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Switch_ruleContext extends ParserRuleContext {
		public TerminalNode CASE() { return getToken(FlixParser.CASE, 0); }
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public List<ExpressionContext> expression() {
			return getRuleContexts(ExpressionContext.class);
		}
		public ExpressionContext expression(int i) {
			return getRuleContext(ExpressionContext.class,i);
		}
		public TerminalNode SC() { return getToken(FlixParser.SC, 0); }
		public Switch_ruleContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_switch_rule; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterSwitch_rule(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitSwitch_rule(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitSwitch_rule(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Switch_ruleContext switch_rule() throws RecognitionException {
		Switch_ruleContext _localctx = new Switch_ruleContext(_ctx, getState());
		enterRule(_localctx, 58, RULE_switch_rule);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(519);
			match(CASE);
			setState(520);
			ws();
			setState(521);
			expression();
			setState(523);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(522);
				ws();
				}
			}

			setState(525);
			match(T__8);
			setState(527);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,40,_ctx) ) {
			case 1:
				{
				setState(526);
				ws();
				}
				break;
			}
			setState(529);
			expression();
			setState(531);
			_la = _input.LA(1);
			if (_la==SC) {
				{
				setState(530);
				match(SC);
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Switch_rulesContext extends ParserRuleContext {
		public List<Switch_ruleContext> switch_rule() {
			return getRuleContexts(Switch_ruleContext.class);
		}
		public Switch_ruleContext switch_rule(int i) {
			return getRuleContext(Switch_ruleContext.class,i);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public Switch_rulesContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_switch_rules; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterSwitch_rules(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitSwitch_rules(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitSwitch_rules(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Switch_rulesContext switch_rules() throws RecognitionException {
		Switch_rulesContext _localctx = new Switch_rulesContext(_ctx, getState());
		enterRule(_localctx, 60, RULE_switch_rules);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(533);
			switch_rule();
			setState(540);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,43,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(535);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(534);
						ws();
						}
					}

					setState(537);
					switch_rule();
					}
					} 
				}
				setState(542);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,43,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TypeparamContext extends ParserRuleContext {
		public VariableNameContext variableName() {
			return getRuleContext(VariableNameContext.class,0);
		}
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public TypeparamContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeparam; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterTypeparam(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitTypeparam(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitTypeparam(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TypeparamContext typeparam() throws RecognitionException {
		TypeparamContext _localctx = new TypeparamContext(_ctx, getState());
		enterRule(_localctx, 62, RULE_typeparam);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(543);
			variableName();
			setState(552);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,46,_ctx) ) {
			case 1:
				{
				setState(545);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(544);
					ws();
					}
				}

				setState(547);
				match(T__3);
				setState(549);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(548);
					ws();
					}
				}

				setState(551);
				type();
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TypeparamsContext extends ParserRuleContext {
		public List<TypeparamContext> typeparam() {
			return getRuleContexts(TypeparamContext.class);
		}
		public TypeparamContext typeparam(int i) {
			return getRuleContext(TypeparamContext.class,i);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public TypeparamsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeparams; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterTypeparams(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitTypeparams(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitTypeparams(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TypeparamsContext typeparams() throws RecognitionException {
		TypeparamsContext _localctx = new TypeparamsContext(_ctx, getState());
		enterRule(_localctx, 64, RULE_typeparams);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(574);
			_la = _input.LA(1);
			if (_la==T__9) {
				{
				setState(554);
				match(T__9);
				setState(556);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(555);
					ws();
					}
				}

				setState(558);
				typeparam();
				setState(569);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==T__2 || _la==TripleSlashComment || _la==PWS) {
					{
					{
					setState(560);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(559);
						ws();
						}
					}

					setState(562);
					match(T__2);
					setState(564);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(563);
						ws();
						}
					}

					setState(566);
					typeparam();
					}
					}
					setState(571);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(572);
				match(T__10);
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Class_typeparamsContext extends ParserRuleContext {
		public List<TypeContext> type() {
			return getRuleContexts(TypeContext.class);
		}
		public TypeContext type(int i) {
			return getRuleContext(TypeContext.class,i);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public Class_typeparamsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_class_typeparams; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterClass_typeparams(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitClass_typeparams(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitClass_typeparams(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Class_typeparamsContext class_typeparams() throws RecognitionException {
		Class_typeparamsContext _localctx = new Class_typeparamsContext(_ctx, getState());
		enterRule(_localctx, 66, RULE_class_typeparams);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(576);
			match(T__9);
			setState(577);
			type();
			setState(588);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__2 || _la==TripleSlashComment || _la==PWS) {
				{
				{
				setState(579);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(578);
					ws();
					}
				}

				setState(581);
				match(T__2);
				setState(583);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(582);
					ws();
					}
				}

				setState(585);
				type();
				}
				}
				setState(590);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(591);
			match(T__10);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ContextBoundContext extends ParserRuleContext {
		public ClassNameContext className() {
			return getRuleContext(ClassNameContext.class,0);
		}
		public Class_typeparamsContext class_typeparams() {
			return getRuleContext(Class_typeparamsContext.class,0);
		}
		public ContextBoundContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_contextBound; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterContextBound(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitContextBound(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitContextBound(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ContextBoundContext contextBound() throws RecognitionException {
		ContextBoundContext _localctx = new ContextBoundContext(_ctx, getState());
		enterRule(_localctx, 68, RULE_contextBound);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(593);
			className();
			setState(594);
			class_typeparams();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ContextBoundsContext extends ParserRuleContext {
		public List<ContextBoundContext> contextBound() {
			return getRuleContexts(ContextBoundContext.class);
		}
		public ContextBoundContext contextBound(int i) {
			return getRuleContext(ContextBoundContext.class,i);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public ContextBoundsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_contextBounds; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterContextBounds(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitContextBounds(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitContextBounds(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ContextBoundsContext contextBounds() throws RecognitionException {
		ContextBoundsContext _localctx = new ContextBoundsContext(_ctx, getState());
		enterRule(_localctx, 70, RULE_contextBounds);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(596);
			contextBound();
			setState(607);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,57,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(598);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(597);
						ws();
						}
					}

					setState(600);
					match(T__2);
					setState(602);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(601);
						ws();
						}
					}

					setState(604);
					contextBound();
					}
					} 
				}
				setState(609);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,57,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ContextBoundsListContext extends ParserRuleContext {
		public ContextBoundsContext contextBounds() {
			return getRuleContext(ContextBoundsContext.class,0);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public ContextBoundsListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_contextBoundsList; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterContextBoundsList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitContextBoundsList(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitContextBoundsList(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ContextBoundsListContext contextBoundsList() throws RecognitionException {
		ContextBoundsListContext _localctx = new ContextBoundsListContext(_ctx, getState());
		enterRule(_localctx, 72, RULE_contextBoundsList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(618);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,60,_ctx) ) {
			case 1:
				{
				setState(611);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(610);
					ws();
					}
				}

				setState(613);
				match(T__8);
				setState(615);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(614);
					ws();
					}
				}

				setState(617);
				contextBounds();
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ImplContextBoundsListContext extends ParserRuleContext {
		public ContextBoundsContext contextBounds() {
			return getRuleContext(ContextBoundsContext.class,0);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public ImplContextBoundsListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_implContextBoundsList; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterImplContextBoundsList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitImplContextBoundsList(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitImplContextBoundsList(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ImplContextBoundsListContext implContextBoundsList() throws RecognitionException {
		ImplContextBoundsListContext _localctx = new ImplContextBoundsListContext(_ctx, getState());
		enterRule(_localctx, 74, RULE_implContextBoundsList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(628);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,63,_ctx) ) {
			case 1:
				{
				setState(621);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(620);
					ws();
					}
				}

				setState(623);
				match(T__11);
				setState(625);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(624);
					ws();
					}
				}

				setState(627);
				contextBounds();
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class AnnotationContext extends ParserRuleContext {
		public AnnotationNameContext annotationName() {
			return getRuleContext(AnnotationNameContext.class,0);
		}
		public AnnotationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_annotation; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterAnnotation(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitAnnotation(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitAnnotation(this);
			else return visitor.visitChildren(this);
		}
	}

	public final AnnotationContext annotation() throws RecognitionException {
		AnnotationContext _localctx = new AnnotationContext(_ctx, getState());
		enterRule(_localctx, 76, RULE_annotation);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(630);
			match(T__12);
			setState(631);
			annotationName();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class AnnotationsContext extends ParserRuleContext {
		public List<AnnotationContext> annotation() {
			return getRuleContexts(AnnotationContext.class);
		}
		public AnnotationContext annotation(int i) {
			return getRuleContext(AnnotationContext.class,i);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public AnnotationsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_annotations; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterAnnotations(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitAnnotations(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitAnnotations(this);
			else return visitor.visitChildren(this);
		}
	}

	public final AnnotationsContext annotations() throws RecognitionException {
		AnnotationsContext _localctx = new AnnotationsContext(_ctx, getState());
		enterRule(_localctx, 78, RULE_annotations);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(633);
			annotation();
			setState(639);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,64,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(634);
					ws();
					setState(635);
					annotation();
					}
					} 
				}
				setState(641);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,64,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class S_importContext extends ParserRuleContext {
		public Import_wildcardContext import_wildcard() {
			return getRuleContext(Import_wildcardContext.class,0);
		}
		public Import_definitionContext import_definition() {
			return getRuleContext(Import_definitionContext.class,0);
		}
		public Import_namespaceContext import_namespace() {
			return getRuleContext(Import_namespaceContext.class,0);
		}
		public WsContext ws() {
			return getRuleContext(WsContext.class,0);
		}
		public S_importContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_s_import; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterS_import(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitS_import(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitS_import(this);
			else return visitor.visitChildren(this);
		}
	}

	public final S_importContext s_import() throws RecognitionException {
		S_importContext _localctx = new S_importContext(_ctx, getState());
		enterRule(_localctx, 80, RULE_s_import);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(643);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(642);
				ws();
				}
			}

			setState(648);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,66,_ctx) ) {
			case 1:
				{
				setState(645);
				import_wildcard();
				}
				break;
			case 2:
				{
				setState(646);
				import_definition();
				}
				break;
			case 3:
				{
				setState(647);
				import_namespace();
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Import_wildcardContext extends ParserRuleContext {
		public TerminalNode IMPORT() { return getToken(FlixParser.IMPORT, 0); }
		public WsContext ws() {
			return getRuleContext(WsContext.class,0);
		}
		public NnameContext nname() {
			return getRuleContext(NnameContext.class,0);
		}
		public TerminalNode WILD() { return getToken(FlixParser.WILD, 0); }
		public OptSCContext optSC() {
			return getRuleContext(OptSCContext.class,0);
		}
		public Import_wildcardContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_import_wildcard; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterImport_wildcard(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitImport_wildcard(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitImport_wildcard(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Import_wildcardContext import_wildcard() throws RecognitionException {
		Import_wildcardContext _localctx = new Import_wildcardContext(_ctx, getState());
		enterRule(_localctx, 82, RULE_import_wildcard);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(650);
			match(IMPORT);
			setState(651);
			ws();
			setState(652);
			nname();
			setState(653);
			match(T__1);
			setState(654);
			match(WILD);
			setState(655);
			optSC();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Import_definitionContext extends ParserRuleContext {
		public TerminalNode IMPORT() { return getToken(FlixParser.IMPORT, 0); }
		public WsContext ws() {
			return getRuleContext(WsContext.class,0);
		}
		public NnameContext nname() {
			return getRuleContext(NnameContext.class,0);
		}
		public IdentContext ident() {
			return getRuleContext(IdentContext.class,0);
		}
		public OptSCContext optSC() {
			return getRuleContext(OptSCContext.class,0);
		}
		public Import_definitionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_import_definition; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterImport_definition(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitImport_definition(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitImport_definition(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Import_definitionContext import_definition() throws RecognitionException {
		Import_definitionContext _localctx = new Import_definitionContext(_ctx, getState());
		enterRule(_localctx, 84, RULE_import_definition);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(657);
			match(IMPORT);
			setState(658);
			ws();
			setState(659);
			nname();
			setState(660);
			match(T__1);
			setState(661);
			ident();
			setState(662);
			optSC();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Import_namespaceContext extends ParserRuleContext {
		public TerminalNode IMPORT() { return getToken(FlixParser.IMPORT, 0); }
		public WsContext ws() {
			return getRuleContext(WsContext.class,0);
		}
		public NnameContext nname() {
			return getRuleContext(NnameContext.class,0);
		}
		public OptSCContext optSC() {
			return getRuleContext(OptSCContext.class,0);
		}
		public Import_namespaceContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_import_namespace; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterImport_namespace(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitImport_namespace(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitImport_namespace(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Import_namespaceContext import_namespace() throws RecognitionException {
		Import_namespaceContext _localctx = new Import_namespaceContext(_ctx, getState());
		enterRule(_localctx, 86, RULE_import_namespace);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(664);
			match(IMPORT);
			setState(665);
			ws();
			setState(666);
			nname();
			setState(667);
			optSC();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class DeclContext extends ParserRuleContext {
		public Decls_namespaceContext decls_namespace() {
			return getRuleContext(Decls_namespaceContext.class,0);
		}
		public Decls_enumContext decls_enum() {
			return getRuleContext(Decls_enumContext.class,0);
		}
		public Decls_relationContext decls_relation() {
			return getRuleContext(Decls_relationContext.class,0);
		}
		public Decls_latticeContext decls_lattice() {
			return getRuleContext(Decls_latticeContext.class,0);
		}
		public Decls_indexContext decls_index() {
			return getRuleContext(Decls_indexContext.class,0);
		}
		public Decls_signatureContext decls_signature() {
			return getRuleContext(Decls_signatureContext.class,0);
		}
		public Decls_externalContext decls_external() {
			return getRuleContext(Decls_externalContext.class,0);
		}
		public Decls_definitionContext decls_definition() {
			return getRuleContext(Decls_definitionContext.class,0);
		}
		public Decls_lawContext decls_law() {
			return getRuleContext(Decls_lawContext.class,0);
		}
		public Decls_classContext decls_class() {
			return getRuleContext(Decls_classContext.class,0);
		}
		public Decls_factContext decls_fact() {
			return getRuleContext(Decls_factContext.class,0);
		}
		public Decls_ruleContext decls_rule() {
			return getRuleContext(Decls_ruleContext.class,0);
		}
		public Decls_implContext decls_impl() {
			return getRuleContext(Decls_implContext.class,0);
		}
		public Decls_letlatticeContext decls_letlattice() {
			return getRuleContext(Decls_letlatticeContext.class,0);
		}
		public DeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_decl; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterDecl(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitDecl(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitDecl(this);
			else return visitor.visitChildren(this);
		}
	}

	public final DeclContext decl() throws RecognitionException {
		DeclContext _localctx = new DeclContext(_ctx, getState());
		enterRule(_localctx, 88, RULE_decl);
		try {
			setState(683);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,67,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(669);
				decls_namespace();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(670);
				decls_enum();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(671);
				decls_relation();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(672);
				decls_lattice();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(673);
				decls_index();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(674);
				decls_signature();
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(675);
				decls_external();
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(676);
				decls_definition();
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(677);
				decls_law();
				}
				break;
			case 10:
				enterOuterAlt(_localctx, 10);
				{
				setState(678);
				decls_class();
				}
				break;
			case 11:
				enterOuterAlt(_localctx, 11);
				{
				setState(679);
				decls_fact();
				}
				break;
			case 12:
				enterOuterAlt(_localctx, 12);
				{
				setState(680);
				decls_rule();
				}
				break;
			case 13:
				enterOuterAlt(_localctx, 13);
				{
				setState(681);
				decls_impl();
				}
				break;
			case 14:
				enterOuterAlt(_localctx, 14);
				{
				setState(682);
				decls_letlattice();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Decls_namespaceContext extends ParserRuleContext {
		public TerminalNode NAMESPACE() { return getToken(FlixParser.NAMESPACE, 0); }
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public NnameContext nname() {
			return getRuleContext(NnameContext.class,0);
		}
		public OptSCContext optSC() {
			return getRuleContext(OptSCContext.class,0);
		}
		public List<DeclContext> decl() {
			return getRuleContexts(DeclContext.class);
		}
		public DeclContext decl(int i) {
			return getRuleContext(DeclContext.class,i);
		}
		public Decls_namespaceContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_decls_namespace; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterDecls_namespace(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitDecls_namespace(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitDecls_namespace(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Decls_namespaceContext decls_namespace() throws RecognitionException {
		Decls_namespaceContext _localctx = new Decls_namespaceContext(_ctx, getState());
		enterRule(_localctx, 90, RULE_decls_namespace);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(686);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(685);
				ws();
				}
			}

			setState(688);
			match(NAMESPACE);
			setState(689);
			ws();
			setState(690);
			nname();
			setState(692);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(691);
				ws();
				}
			}

			setState(694);
			match(T__6);
			setState(696);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,70,_ctx) ) {
			case 1:
				{
				setState(695);
				ws();
				}
				break;
			}
			setState(701);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,71,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(698);
					decl();
					}
					} 
				}
				setState(703);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,71,_ctx);
			}
			setState(705);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(704);
				ws();
				}
			}

			setState(707);
			match(T__7);
			setState(708);
			optSC();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Decls_enumContext extends ParserRuleContext {
		public TerminalNode ENUM() { return getToken(FlixParser.ENUM, 0); }
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public TypeNameContext typeName() {
			return getRuleContext(TypeNameContext.class,0);
		}
		public TypeparamsContext typeparams() {
			return getRuleContext(TypeparamsContext.class,0);
		}
		public DcasesContext dcases() {
			return getRuleContext(DcasesContext.class,0);
		}
		public OptSCContext optSC() {
			return getRuleContext(OptSCContext.class,0);
		}
		public List<TscommentContext> tscomment() {
			return getRuleContexts(TscommentContext.class);
		}
		public TscommentContext tscomment(int i) {
			return getRuleContext(TscommentContext.class,i);
		}
		public List<TerminalNode> PWS() { return getTokens(FlixParser.PWS); }
		public TerminalNode PWS(int i) {
			return getToken(FlixParser.PWS, i);
		}
		public Decls_enumContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_decls_enum; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterDecls_enum(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitDecls_enum(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitDecls_enum(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Decls_enumContext decls_enum() throws RecognitionException {
		Decls_enumContext _localctx = new Decls_enumContext(_ctx, getState());
		enterRule(_localctx, 92, RULE_decls_enum);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(716);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,74,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(711);
					_la = _input.LA(1);
					if (_la==PWS) {
						{
						setState(710);
						match(PWS);
						}
					}

					setState(713);
					tscomment();
					}
					} 
				}
				setState(718);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,74,_ctx);
			}
			setState(720);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(719);
				ws();
				}
			}

			setState(722);
			match(ENUM);
			setState(723);
			ws();
			setState(724);
			typeName();
			setState(725);
			typeparams();
			setState(727);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(726);
				ws();
				}
			}

			setState(729);
			match(T__6);
			setState(731);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(730);
				ws();
				}
			}

			setState(733);
			dcases();
			setState(735);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(734);
				ws();
				}
			}

			setState(737);
			match(T__7);
			setState(738);
			optSC();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class DcasesContext extends ParserRuleContext {
		public List<DcaseContext> dcase() {
			return getRuleContexts(DcaseContext.class);
		}
		public DcaseContext dcase(int i) {
			return getRuleContext(DcaseContext.class,i);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public DcasesContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_dcases; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterDcases(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitDcases(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitDcases(this);
			else return visitor.visitChildren(this);
		}
	}

	public final DcasesContext dcases() throws RecognitionException {
		DcasesContext _localctx = new DcasesContext(_ctx, getState());
		enterRule(_localctx, 94, RULE_dcases);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(740);
			dcase();
			setState(751);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,81,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(742);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(741);
						ws();
						}
					}

					setState(744);
					match(T__2);
					setState(746);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(745);
						ws();
						}
					}

					setState(748);
					dcase();
					}
					} 
				}
				setState(753);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,81,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class DcaseContext extends ParserRuleContext {
		public TerminalNode CASE() { return getToken(FlixParser.CASE, 0); }
		public WsContext ws() {
			return getRuleContext(WsContext.class,0);
		}
		public TagNameContext tagName() {
			return getRuleContext(TagNameContext.class,0);
		}
		public TupleContext tuple() {
			return getRuleContext(TupleContext.class,0);
		}
		public DcaseContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_dcase; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterDcase(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitDcase(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitDcase(this);
			else return visitor.visitChildren(this);
		}
	}

	public final DcaseContext dcase() throws RecognitionException {
		DcaseContext _localctx = new DcaseContext(_ctx, getState());
		enterRule(_localctx, 96, RULE_dcase);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(754);
			match(CASE);
			setState(755);
			ws();
			setState(756);
			tagName();
			setState(758);
			_la = _input.LA(1);
			if (_la==T__4) {
				{
				setState(757);
				tuple();
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Decls_relationContext extends ParserRuleContext {
		public TerminalNode REL() { return getToken(FlixParser.REL, 0); }
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public TableNameContext tableName() {
			return getRuleContext(TableNameContext.class,0);
		}
		public OptSCContext optSC() {
			return getRuleContext(OptSCContext.class,0);
		}
		public List<TscommentContext> tscomment() {
			return getRuleContexts(TscommentContext.class);
		}
		public TscommentContext tscomment(int i) {
			return getRuleContext(TscommentContext.class,i);
		}
		public AttributesContext attributes() {
			return getRuleContext(AttributesContext.class,0);
		}
		public List<TerminalNode> PWS() { return getTokens(FlixParser.PWS); }
		public TerminalNode PWS(int i) {
			return getToken(FlixParser.PWS, i);
		}
		public Decls_relationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_decls_relation; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterDecls_relation(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitDecls_relation(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitDecls_relation(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Decls_relationContext decls_relation() throws RecognitionException {
		Decls_relationContext _localctx = new Decls_relationContext(_ctx, getState());
		enterRule(_localctx, 98, RULE_decls_relation);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(766);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,84,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(761);
					_la = _input.LA(1);
					if (_la==PWS) {
						{
						setState(760);
						match(PWS);
						}
					}

					setState(763);
					tscomment();
					}
					} 
				}
				setState(768);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,84,_ctx);
			}
			setState(770);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(769);
				ws();
				}
			}

			setState(772);
			match(REL);
			setState(773);
			ws();
			setState(774);
			tableName();
			setState(776);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(775);
				ws();
				}
			}

			setState(778);
			match(T__4);
			setState(780);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,87,_ctx) ) {
			case 1:
				{
				setState(779);
				ws();
				}
				break;
			}
			setState(783);
			_la = _input.LA(1);
			if (_la==LowerIdent) {
				{
				setState(782);
				attributes();
				}
			}

			setState(786);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(785);
				ws();
				}
			}

			setState(788);
			match(T__5);
			setState(789);
			optSC();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Decls_latticeContext extends ParserRuleContext {
		public TerminalNode LAT() { return getToken(FlixParser.LAT, 0); }
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public TableNameContext tableName() {
			return getRuleContext(TableNameContext.class,0);
		}
		public OptSCContext optSC() {
			return getRuleContext(OptSCContext.class,0);
		}
		public List<TscommentContext> tscomment() {
			return getRuleContexts(TscommentContext.class);
		}
		public TscommentContext tscomment(int i) {
			return getRuleContext(TscommentContext.class,i);
		}
		public AttributesContext attributes() {
			return getRuleContext(AttributesContext.class,0);
		}
		public List<TerminalNode> PWS() { return getTokens(FlixParser.PWS); }
		public TerminalNode PWS(int i) {
			return getToken(FlixParser.PWS, i);
		}
		public Decls_latticeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_decls_lattice; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterDecls_lattice(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitDecls_lattice(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitDecls_lattice(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Decls_latticeContext decls_lattice() throws RecognitionException {
		Decls_latticeContext _localctx = new Decls_latticeContext(_ctx, getState());
		enterRule(_localctx, 100, RULE_decls_lattice);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(797);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,91,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(792);
					_la = _input.LA(1);
					if (_la==PWS) {
						{
						setState(791);
						match(PWS);
						}
					}

					setState(794);
					tscomment();
					}
					} 
				}
				setState(799);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,91,_ctx);
			}
			setState(801);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(800);
				ws();
				}
			}

			setState(803);
			match(LAT);
			setState(804);
			ws();
			setState(805);
			tableName();
			setState(807);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(806);
				ws();
				}
			}

			setState(809);
			match(T__4);
			setState(811);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,94,_ctx) ) {
			case 1:
				{
				setState(810);
				ws();
				}
				break;
			}
			setState(814);
			_la = _input.LA(1);
			if (_la==LowerIdent) {
				{
				setState(813);
				attributes();
				}
			}

			setState(817);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(816);
				ws();
				}
			}

			setState(819);
			match(T__5);
			setState(820);
			optSC();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Decls_indexContext extends ParserRuleContext {
		public TerminalNode INDEX() { return getToken(FlixParser.INDEX, 0); }
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public QualifiedTableNameContext qualifiedTableName() {
			return getRuleContext(QualifiedTableNameContext.class,0);
		}
		public OptSCContext optSC() {
			return getRuleContext(OptSCContext.class,0);
		}
		public IndexesContext indexes() {
			return getRuleContext(IndexesContext.class,0);
		}
		public Decls_indexContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_decls_index; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterDecls_index(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitDecls_index(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitDecls_index(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Decls_indexContext decls_index() throws RecognitionException {
		Decls_indexContext _localctx = new Decls_indexContext(_ctx, getState());
		enterRule(_localctx, 102, RULE_decls_index);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(823);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(822);
				ws();
				}
			}

			setState(825);
			match(INDEX);
			setState(826);
			ws();
			setState(827);
			qualifiedTableName();
			setState(829);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(828);
				ws();
				}
			}

			setState(831);
			match(T__4);
			setState(833);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,99,_ctx) ) {
			case 1:
				{
				setState(832);
				ws();
				}
				break;
			}
			setState(836);
			_la = _input.LA(1);
			if (_la==T__6) {
				{
				setState(835);
				indexes();
				}
			}

			setState(839);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(838);
				ws();
				}
			}

			setState(841);
			match(T__5);
			setState(842);
			optSC();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Decls_signatureContext extends ParserRuleContext {
		public TerminalNode DEF() { return getToken(FlixParser.DEF, 0); }
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public DefinitionNameContext definitionName() {
			return getRuleContext(DefinitionNameContext.class,0);
		}
		public FormalparamsContext formalparams() {
			return getRuleContext(FormalparamsContext.class,0);
		}
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public OptSCContext optSC() {
			return getRuleContext(OptSCContext.class,0);
		}
		public List<TscommentContext> tscomment() {
			return getRuleContexts(TscommentContext.class);
		}
		public TscommentContext tscomment(int i) {
			return getRuleContext(TscommentContext.class,i);
		}
		public List<TerminalNode> PWS() { return getTokens(FlixParser.PWS); }
		public TerminalNode PWS(int i) {
			return getToken(FlixParser.PWS, i);
		}
		public Decls_signatureContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_decls_signature; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterDecls_signature(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitDecls_signature(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitDecls_signature(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Decls_signatureContext decls_signature() throws RecognitionException {
		Decls_signatureContext _localctx = new Decls_signatureContext(_ctx, getState());
		enterRule(_localctx, 104, RULE_decls_signature);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(850);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,103,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(845);
					_la = _input.LA(1);
					if (_la==PWS) {
						{
						setState(844);
						match(PWS);
						}
					}

					setState(847);
					tscomment();
					}
					} 
				}
				setState(852);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,103,_ctx);
			}
			setState(854);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(853);
				ws();
				}
			}

			setState(856);
			match(DEF);
			setState(857);
			ws();
			setState(858);
			definitionName();
			setState(860);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,105,_ctx) ) {
			case 1:
				{
				setState(859);
				ws();
				}
				break;
			}
			setState(862);
			formalparams();
			setState(864);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(863);
				ws();
				}
			}

			setState(866);
			match(T__3);
			setState(868);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(867);
				ws();
				}
			}

			setState(870);
			type();
			setState(871);
			optSC();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Decls_externalContext extends ParserRuleContext {
		public TerminalNode EXTERNAL() { return getToken(FlixParser.EXTERNAL, 0); }
		public TerminalNode DEF() { return getToken(FlixParser.DEF, 0); }
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public DefinitionNameContext definitionName() {
			return getRuleContext(DefinitionNameContext.class,0);
		}
		public FormalparamsContext formalparams() {
			return getRuleContext(FormalparamsContext.class,0);
		}
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public OptSCContext optSC() {
			return getRuleContext(OptSCContext.class,0);
		}
		public List<TscommentContext> tscomment() {
			return getRuleContexts(TscommentContext.class);
		}
		public TscommentContext tscomment(int i) {
			return getRuleContext(TscommentContext.class,i);
		}
		public List<TerminalNode> PWS() { return getTokens(FlixParser.PWS); }
		public TerminalNode PWS(int i) {
			return getToken(FlixParser.PWS, i);
		}
		public Decls_externalContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_decls_external; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterDecls_external(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitDecls_external(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitDecls_external(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Decls_externalContext decls_external() throws RecognitionException {
		Decls_externalContext _localctx = new Decls_externalContext(_ctx, getState());
		enterRule(_localctx, 106, RULE_decls_external);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(879);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,109,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(874);
					_la = _input.LA(1);
					if (_la==PWS) {
						{
						setState(873);
						match(PWS);
						}
					}

					setState(876);
					tscomment();
					}
					} 
				}
				setState(881);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,109,_ctx);
			}
			setState(883);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(882);
				ws();
				}
			}

			setState(885);
			match(EXTERNAL);
			setState(887);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(886);
				ws();
				}
			}

			setState(889);
			match(DEF);
			setState(890);
			ws();
			setState(891);
			definitionName();
			setState(893);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,112,_ctx) ) {
			case 1:
				{
				setState(892);
				ws();
				}
				break;
			}
			setState(895);
			formalparams();
			setState(897);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(896);
				ws();
				}
			}

			setState(899);
			match(T__3);
			setState(901);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(900);
				ws();
				}
			}

			setState(903);
			type();
			setState(904);
			optSC();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Decls_definitionContext extends ParserRuleContext {
		public TerminalNode DEF() { return getToken(FlixParser.DEF, 0); }
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public DefinitionNameContext definitionName() {
			return getRuleContext(DefinitionNameContext.class,0);
		}
		public TypeparamsContext typeparams() {
			return getRuleContext(TypeparamsContext.class,0);
		}
		public FormalparamsContext formalparams() {
			return getRuleContext(FormalparamsContext.class,0);
		}
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public OptSCContext optSC() {
			return getRuleContext(OptSCContext.class,0);
		}
		public List<TscommentContext> tscomment() {
			return getRuleContexts(TscommentContext.class);
		}
		public TscommentContext tscomment(int i) {
			return getRuleContext(TscommentContext.class,i);
		}
		public AnnotationsContext annotations() {
			return getRuleContext(AnnotationsContext.class,0);
		}
		public List<TerminalNode> PWS() { return getTokens(FlixParser.PWS); }
		public TerminalNode PWS(int i) {
			return getToken(FlixParser.PWS, i);
		}
		public Decls_definitionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_decls_definition; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterDecls_definition(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitDecls_definition(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitDecls_definition(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Decls_definitionContext decls_definition() throws RecognitionException {
		Decls_definitionContext _localctx = new Decls_definitionContext(_ctx, getState());
		enterRule(_localctx, 108, RULE_decls_definition);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(912);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,116,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(907);
					_la = _input.LA(1);
					if (_la==PWS) {
						{
						setState(906);
						match(PWS);
						}
					}

					setState(909);
					tscomment();
					}
					} 
				}
				setState(914);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,116,_ctx);
			}
			setState(916);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,117,_ctx) ) {
			case 1:
				{
				setState(915);
				ws();
				}
				break;
			}
			setState(919);
			_la = _input.LA(1);
			if (_la==T__12) {
				{
				setState(918);
				annotations();
				}
			}

			setState(922);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(921);
				ws();
				}
			}

			setState(924);
			match(DEF);
			setState(925);
			ws();
			setState(926);
			definitionName();
			setState(928);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,120,_ctx) ) {
			case 1:
				{
				setState(927);
				ws();
				}
				break;
			}
			setState(930);
			typeparams();
			setState(931);
			formalparams();
			setState(933);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(932);
				ws();
				}
			}

			setState(935);
			match(T__3);
			setState(937);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(936);
				ws();
				}
			}

			setState(939);
			type();
			setState(941);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(940);
				ws();
				}
			}

			setState(943);
			match(T__13);
			setState(945);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,124,_ctx) ) {
			case 1:
				{
				setState(944);
				ws();
				}
				break;
			}
			setState(947);
			expression();
			setState(948);
			optSC();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Decls_lawContext extends ParserRuleContext {
		public TerminalNode LAW() { return getToken(FlixParser.LAW, 0); }
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public DefinitionNameContext definitionName() {
			return getRuleContext(DefinitionNameContext.class,0);
		}
		public TypeparamsContext typeparams() {
			return getRuleContext(TypeparamsContext.class,0);
		}
		public FormalparamsContext formalparams() {
			return getRuleContext(FormalparamsContext.class,0);
		}
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public OptSCContext optSC() {
			return getRuleContext(OptSCContext.class,0);
		}
		public List<TscommentContext> tscomment() {
			return getRuleContexts(TscommentContext.class);
		}
		public TscommentContext tscomment(int i) {
			return getRuleContext(TscommentContext.class,i);
		}
		public List<TerminalNode> PWS() { return getTokens(FlixParser.PWS); }
		public TerminalNode PWS(int i) {
			return getToken(FlixParser.PWS, i);
		}
		public Decls_lawContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_decls_law; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterDecls_law(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitDecls_law(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitDecls_law(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Decls_lawContext decls_law() throws RecognitionException {
		Decls_lawContext _localctx = new Decls_lawContext(_ctx, getState());
		enterRule(_localctx, 110, RULE_decls_law);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(956);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,126,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(951);
					_la = _input.LA(1);
					if (_la==PWS) {
						{
						setState(950);
						match(PWS);
						}
					}

					setState(953);
					tscomment();
					}
					} 
				}
				setState(958);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,126,_ctx);
			}
			setState(960);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(959);
				ws();
				}
			}

			setState(962);
			match(LAW);
			setState(963);
			ws();
			setState(964);
			definitionName();
			setState(966);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,128,_ctx) ) {
			case 1:
				{
				setState(965);
				ws();
				}
				break;
			}
			setState(968);
			typeparams();
			setState(970);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,129,_ctx) ) {
			case 1:
				{
				setState(969);
				ws();
				}
				break;
			}
			setState(972);
			formalparams();
			setState(974);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(973);
				ws();
				}
			}

			setState(976);
			match(T__3);
			setState(978);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(977);
				ws();
				}
			}

			setState(980);
			type();
			setState(982);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(981);
				ws();
				}
			}

			setState(984);
			match(T__13);
			setState(986);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,133,_ctx) ) {
			case 1:
				{
				setState(985);
				ws();
				}
				break;
			}
			setState(988);
			expression();
			setState(989);
			optSC();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Decls_classContext extends ParserRuleContext {
		public TerminalNode CLASS() { return getToken(FlixParser.CLASS, 0); }
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public ClassNameContext className() {
			return getRuleContext(ClassNameContext.class,0);
		}
		public Class_typeparamsContext class_typeparams() {
			return getRuleContext(Class_typeparamsContext.class,0);
		}
		public ContextBoundsListContext contextBoundsList() {
			return getRuleContext(ContextBoundsListContext.class,0);
		}
		public Class_bodyContext class_body() {
			return getRuleContext(Class_bodyContext.class,0);
		}
		public List<TscommentContext> tscomment() {
			return getRuleContexts(TscommentContext.class);
		}
		public TscommentContext tscomment(int i) {
			return getRuleContext(TscommentContext.class,i);
		}
		public List<TerminalNode> PWS() { return getTokens(FlixParser.PWS); }
		public TerminalNode PWS(int i) {
			return getToken(FlixParser.PWS, i);
		}
		public Decls_classContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_decls_class; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterDecls_class(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitDecls_class(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitDecls_class(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Decls_classContext decls_class() throws RecognitionException {
		Decls_classContext _localctx = new Decls_classContext(_ctx, getState());
		enterRule(_localctx, 112, RULE_decls_class);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(997);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,135,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(992);
					_la = _input.LA(1);
					if (_la==PWS) {
						{
						setState(991);
						match(PWS);
						}
					}

					setState(994);
					tscomment();
					}
					} 
				}
				setState(999);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,135,_ctx);
			}
			setState(1001);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1000);
				ws();
				}
			}

			setState(1003);
			match(CLASS);
			setState(1004);
			ws();
			setState(1005);
			className();
			setState(1006);
			class_typeparams();
			setState(1007);
			contextBoundsList();
			setState(1009);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1008);
				ws();
				}
			}

			setState(1011);
			class_body();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Class_bodyContext extends ParserRuleContext {
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public List<Class_declContext> class_decl() {
			return getRuleContexts(Class_declContext.class);
		}
		public Class_declContext class_decl(int i) {
			return getRuleContext(Class_declContext.class,i);
		}
		public Class_bodyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_class_body; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterClass_body(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitClass_body(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitClass_body(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Class_bodyContext class_body() throws RecognitionException {
		Class_bodyContext _localctx = new Class_bodyContext(_ctx, getState());
		enterRule(_localctx, 114, RULE_class_body);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1013);
			match(T__6);
			setState(1015);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,138,_ctx) ) {
			case 1:
				{
				setState(1014);
				ws();
				}
				break;
			}
			setState(1023);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__12 || ((((_la - 71)) & ~0x3f) == 0 && ((1L << (_la - 71)) & ((1L << (TripleSlashComment - 71)) | (1L << (PWS - 71)) | (1L << (DEF - 71)) | (1L << (LAW - 71)))) != 0)) {
				{
				{
				setState(1017);
				class_decl();
				setState(1019);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,139,_ctx) ) {
				case 1:
					{
					setState(1018);
					ws();
					}
					break;
				}
				}
				}
				setState(1025);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(1026);
			match(T__7);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Class_declContext extends ParserRuleContext {
		public Decls_definitionContext decls_definition() {
			return getRuleContext(Decls_definitionContext.class,0);
		}
		public Decls_signatureContext decls_signature() {
			return getRuleContext(Decls_signatureContext.class,0);
		}
		public Decls_lawContext decls_law() {
			return getRuleContext(Decls_lawContext.class,0);
		}
		public Class_declContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_class_decl; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterClass_decl(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitClass_decl(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitClass_decl(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Class_declContext class_decl() throws RecognitionException {
		Class_declContext _localctx = new Class_declContext(_ctx, getState());
		enterRule(_localctx, 116, RULE_class_decl);
		try {
			setState(1031);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,141,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1028);
				decls_definition();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1029);
				decls_signature();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1030);
				decls_law();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Decls_factContext extends ParserRuleContext {
		public PredicateContext predicate() {
			return getRuleContext(PredicateContext.class,0);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public Decls_factContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_decls_fact; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterDecls_fact(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitDecls_fact(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitDecls_fact(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Decls_factContext decls_fact() throws RecognitionException {
		Decls_factContext _localctx = new Decls_factContext(_ctx, getState());
		enterRule(_localctx, 118, RULE_decls_fact);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1034);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1033);
				ws();
				}
			}

			setState(1036);
			predicate();
			setState(1038);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1037);
				ws();
				}
			}

			setState(1040);
			match(T__0);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Decls_ruleContext extends ParserRuleContext {
		public PredicateContext predicate() {
			return getRuleContext(PredicateContext.class,0);
		}
		public PredicatesContext predicates() {
			return getRuleContext(PredicatesContext.class,0);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public Decls_ruleContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_decls_rule; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterDecls_rule(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitDecls_rule(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitDecls_rule(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Decls_ruleContext decls_rule() throws RecognitionException {
		Decls_ruleContext _localctx = new Decls_ruleContext(_ctx, getState());
		enterRule(_localctx, 120, RULE_decls_rule);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1043);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1042);
				ws();
				}
			}

			setState(1045);
			predicate();
			setState(1047);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1046);
				ws();
				}
			}

			setState(1049);
			match(T__14);
			setState(1051);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1050);
				ws();
				}
			}

			setState(1053);
			predicates();
			setState(1055);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1054);
				ws();
				}
			}

			setState(1057);
			match(T__0);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ElmsContext extends ParserRuleContext {
		public ExpressionsContext expressions() {
			return getRuleContext(ExpressionsContext.class,0);
		}
		public ElmsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_elms; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterElms(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitElms(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitElms(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ElmsContext elms() throws RecognitionException {
		ElmsContext _localctx = new ElmsContext(_ctx, getState());
		enterRule(_localctx, 122, RULE_elms);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1059);
			expressions();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Decls_letlatticeContext extends ParserRuleContext {
		public TerminalNode LET() { return getToken(FlixParser.LET, 0); }
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public ElmsContext elms() {
			return getRuleContext(ElmsContext.class,0);
		}
		public OptSCContext optSC() {
			return getRuleContext(OptSCContext.class,0);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public Decls_letlatticeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_decls_letlattice; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterDecls_letlattice(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitDecls_letlattice(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitDecls_letlattice(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Decls_letlatticeContext decls_letlattice() throws RecognitionException {
		Decls_letlatticeContext _localctx = new Decls_letlatticeContext(_ctx, getState());
		enterRule(_localctx, 124, RULE_decls_letlattice);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1062);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1061);
				ws();
				}
			}

			setState(1064);
			match(LET);
			setState(1066);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1065);
				ws();
				}
			}

			setState(1068);
			type();
			setState(1069);
			match(T__15);
			setState(1071);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1070);
				ws();
				}
			}

			setState(1073);
			match(T__13);
			setState(1075);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1074);
				ws();
				}
			}

			setState(1077);
			match(T__4);
			setState(1079);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,152,_ctx) ) {
			case 1:
				{
				setState(1078);
				ws();
				}
				break;
			}
			setState(1081);
			elms();
			setState(1083);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1082);
				ws();
				}
			}

			setState(1085);
			match(T__5);
			setState(1086);
			optSC();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Decls_implContext extends ParserRuleContext {
		public TerminalNode IMPL() { return getToken(FlixParser.IMPL, 0); }
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public ClassNameContext className() {
			return getRuleContext(ClassNameContext.class,0);
		}
		public Class_typeparamsContext class_typeparams() {
			return getRuleContext(Class_typeparamsContext.class,0);
		}
		public ImplContextBoundsListContext implContextBoundsList() {
			return getRuleContext(ImplContextBoundsListContext.class,0);
		}
		public Decls_impl_bodyContext decls_impl_body() {
			return getRuleContext(Decls_impl_bodyContext.class,0);
		}
		public List<TscommentContext> tscomment() {
			return getRuleContexts(TscommentContext.class);
		}
		public TscommentContext tscomment(int i) {
			return getRuleContext(TscommentContext.class,i);
		}
		public List<TerminalNode> PWS() { return getTokens(FlixParser.PWS); }
		public TerminalNode PWS(int i) {
			return getToken(FlixParser.PWS, i);
		}
		public Decls_implContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_decls_impl; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterDecls_impl(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitDecls_impl(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitDecls_impl(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Decls_implContext decls_impl() throws RecognitionException {
		Decls_implContext _localctx = new Decls_implContext(_ctx, getState());
		enterRule(_localctx, 126, RULE_decls_impl);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1094);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,155,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1089);
					_la = _input.LA(1);
					if (_la==PWS) {
						{
						setState(1088);
						match(PWS);
						}
					}

					setState(1091);
					tscomment();
					}
					} 
				}
				setState(1096);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,155,_ctx);
			}
			setState(1098);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1097);
				ws();
				}
			}

			setState(1100);
			match(IMPL);
			setState(1101);
			ws();
			setState(1102);
			className();
			setState(1103);
			class_typeparams();
			setState(1105);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,157,_ctx) ) {
			case 1:
				{
				setState(1104);
				ws();
				}
				break;
			}
			setState(1107);
			implContextBoundsList();
			setState(1109);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1108);
				ws();
				}
			}

			setState(1111);
			decls_impl_body();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Decls_impl_bodyContext extends ParserRuleContext {
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public List<Decls_definitionContext> decls_definition() {
			return getRuleContexts(Decls_definitionContext.class);
		}
		public Decls_definitionContext decls_definition(int i) {
			return getRuleContext(Decls_definitionContext.class,i);
		}
		public Decls_impl_bodyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_decls_impl_body; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterDecls_impl_body(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitDecls_impl_body(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitDecls_impl_body(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Decls_impl_bodyContext decls_impl_body() throws RecognitionException {
		Decls_impl_bodyContext _localctx = new Decls_impl_bodyContext(_ctx, getState());
		enterRule(_localctx, 128, RULE_decls_impl_body);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1113);
			match(T__6);
			setState(1115);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,159,_ctx) ) {
			case 1:
				{
				setState(1114);
				ws();
				}
				break;
			}
			setState(1120);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,160,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1117);
					decls_definition();
					}
					} 
				}
				setState(1122);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,160,_ctx);
			}
			setState(1124);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1123);
				ws();
				}
			}

			setState(1126);
			match(T__7);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExpressionContext extends ParserRuleContext {
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public LogicalContext logical() {
			return getRuleContext(LogicalContext.class,0);
		}
		public ExpressionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_expression; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitExpression(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitExpression(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ExpressionContext expression() throws RecognitionException {
		ExpressionContext _localctx = new ExpressionContext(_ctx, getState());
		enterRule(_localctx, 130, RULE_expression);
		int _la;
		try {
			setState(1141);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,165,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				{
				setState(1128);
				match(T__6);
				setState(1130);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,162,_ctx) ) {
				case 1:
					{
					setState(1129);
					ws();
					}
					break;
				}
				setState(1132);
				expression();
				setState(1134);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(1133);
					ws();
					}
				}

				setState(1136);
				match(T__7);
				setState(1138);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,164,_ctx) ) {
				case 1:
					{
					setState(1137);
					ws();
					}
					break;
				}
				}
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1140);
				logical();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class LogicalContext extends ParserRuleContext {
		public List<ComparisonContext> comparison() {
			return getRuleContexts(ComparisonContext.class);
		}
		public ComparisonContext comparison(int i) {
			return getRuleContext(ComparisonContext.class,i);
		}
		public Logical_opsContext logical_ops() {
			return getRuleContext(Logical_opsContext.class,0);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public LogicalContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_logical; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterLogical(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitLogical(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitLogical(this);
			else return visitor.visitChildren(this);
		}
	}

	public final LogicalContext logical() throws RecognitionException {
		LogicalContext _localctx = new LogicalContext(_ctx, getState());
		enterRule(_localctx, 132, RULE_logical);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1143);
			comparison();
			setState(1153);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,168,_ctx) ) {
			case 1:
				{
				setState(1145);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(1144);
					ws();
					}
				}

				setState(1147);
				logical_ops();
				setState(1149);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,167,_ctx) ) {
				case 1:
					{
					setState(1148);
					ws();
					}
					break;
				}
				setState(1151);
				comparison();
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExpressionsContext extends ParserRuleContext {
		public List<ExpressionContext> expression() {
			return getRuleContexts(ExpressionContext.class);
		}
		public ExpressionContext expression(int i) {
			return getRuleContext(ExpressionContext.class,i);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public ExpressionsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_expressions; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterExpressions(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitExpressions(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitExpressions(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ExpressionsContext expressions() throws RecognitionException {
		ExpressionsContext _localctx = new ExpressionsContext(_ctx, getState());
		enterRule(_localctx, 134, RULE_expressions);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1155);
			expression();
			setState(1166);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,171,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1157);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(1156);
						ws();
						}
					}

					setState(1159);
					match(T__2);
					setState(1161);
					_errHandler.sync(this);
					switch ( getInterpreter().adaptivePredict(_input,170,_ctx) ) {
					case 1:
						{
						setState(1160);
						ws();
						}
						break;
					}
					setState(1163);
					expression();
					}
					} 
				}
				setState(1168);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,171,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ComparisonContext extends ParserRuleContext {
		public List<AdditiveContext> additive() {
			return getRuleContexts(AdditiveContext.class);
		}
		public AdditiveContext additive(int i) {
			return getRuleContext(AdditiveContext.class,i);
		}
		public Comparison_opsContext comparison_ops() {
			return getRuleContext(Comparison_opsContext.class,0);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public ComparisonContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_comparison; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterComparison(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitComparison(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitComparison(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ComparisonContext comparison() throws RecognitionException {
		ComparisonContext _localctx = new ComparisonContext(_ctx, getState());
		enterRule(_localctx, 136, RULE_comparison);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1169);
			additive(0);
			setState(1179);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,174,_ctx) ) {
			case 1:
				{
				setState(1171);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(1170);
					ws();
					}
				}

				setState(1173);
				comparison_ops();
				setState(1175);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,173,_ctx) ) {
				case 1:
					{
					setState(1174);
					ws();
					}
					break;
				}
				setState(1177);
				additive(0);
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class AdditiveContext extends ParserRuleContext {
		public MultiplicativeContext multiplicative() {
			return getRuleContext(MultiplicativeContext.class,0);
		}
		public AdditiveContext additive() {
			return getRuleContext(AdditiveContext.class,0);
		}
		public Addve_opsContext addve_ops() {
			return getRuleContext(Addve_opsContext.class,0);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public AdditiveContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_additive; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterAdditive(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitAdditive(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitAdditive(this);
			else return visitor.visitChildren(this);
		}
	}

	public final AdditiveContext additive() throws RecognitionException {
		return additive(0);
	}

	private AdditiveContext additive(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		AdditiveContext _localctx = new AdditiveContext(_ctx, _parentState);
		AdditiveContext _prevctx = _localctx;
		int _startState = 138;
		enterRecursionRule(_localctx, 138, RULE_additive, _p);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(1182);
			multiplicative(0);
			}
			_ctx.stop = _input.LT(-1);
			setState(1196);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,177,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new AdditiveContext(_parentctx, _parentState);
					pushNewRecursionContext(_localctx, _startState, RULE_additive);
					setState(1184);
					if (!(precpred(_ctx, 2))) throw new FailedPredicateException(this, "precpred(_ctx, 2)");
					setState(1186);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(1185);
						ws();
						}
					}

					setState(1188);
					addve_ops();
					setState(1190);
					_errHandler.sync(this);
					switch ( getInterpreter().adaptivePredict(_input,176,_ctx) ) {
					case 1:
						{
						setState(1189);
						ws();
						}
						break;
					}
					setState(1192);
					multiplicative(0);
					}
					} 
				}
				setState(1198);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,177,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	public static class MultiplicativeContext extends ParserRuleContext {
		public InfixContext infix() {
			return getRuleContext(InfixContext.class,0);
		}
		public MultiplicativeContext multiplicative() {
			return getRuleContext(MultiplicativeContext.class,0);
		}
		public Multipve_opsContext multipve_ops() {
			return getRuleContext(Multipve_opsContext.class,0);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public MultiplicativeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_multiplicative; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterMultiplicative(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitMultiplicative(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitMultiplicative(this);
			else return visitor.visitChildren(this);
		}
	}

	public final MultiplicativeContext multiplicative() throws RecognitionException {
		return multiplicative(0);
	}

	private MultiplicativeContext multiplicative(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		MultiplicativeContext _localctx = new MultiplicativeContext(_ctx, _parentState);
		MultiplicativeContext _prevctx = _localctx;
		int _startState = 140;
		enterRecursionRule(_localctx, 140, RULE_multiplicative, _p);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(1200);
			infix();
			}
			_ctx.stop = _input.LT(-1);
			setState(1214);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,180,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new MultiplicativeContext(_parentctx, _parentState);
					pushNewRecursionContext(_localctx, _startState, RULE_multiplicative);
					setState(1202);
					if (!(precpred(_ctx, 2))) throw new FailedPredicateException(this, "precpred(_ctx, 2)");
					setState(1204);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(1203);
						ws();
						}
					}

					setState(1206);
					multipve_ops();
					setState(1208);
					_errHandler.sync(this);
					switch ( getInterpreter().adaptivePredict(_input,179,_ctx) ) {
					case 1:
						{
						setState(1207);
						ws();
						}
						break;
					}
					setState(1210);
					infix();
					}
					} 
				}
				setState(1216);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,180,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	public static class InfixContext extends ParserRuleContext {
		public List<ExtendedContext> extended() {
			return getRuleContexts(ExtendedContext.class);
		}
		public ExtendedContext extended(int i) {
			return getRuleContext(ExtendedContext.class,i);
		}
		public QualifiedDefinitionNameContext qualifiedDefinitionName() {
			return getRuleContext(QualifiedDefinitionNameContext.class,0);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public InfixContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_infix; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterInfix(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitInfix(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitInfix(this);
			else return visitor.visitChildren(this);
		}
	}

	public final InfixContext infix() throws RecognitionException {
		InfixContext _localctx = new InfixContext(_ctx, getState());
		enterRule(_localctx, 142, RULE_infix);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1217);
			extended();
			setState(1229);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,183,_ctx) ) {
			case 1:
				{
				setState(1219);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(1218);
					ws();
					}
				}

				setState(1221);
				match(T__16);
				setState(1222);
				qualifiedDefinitionName();
				setState(1223);
				match(T__16);
				setState(1225);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,182,_ctx) ) {
				case 1:
					{
					setState(1224);
					ws();
					}
					break;
				}
				setState(1227);
				extended();
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExtendedContext extends ParserRuleContext {
		public List<UnaryContext> unary() {
			return getRuleContexts(UnaryContext.class);
		}
		public UnaryContext unary(int i) {
			return getRuleContext(UnaryContext.class,i);
		}
		public Extbin_opsContext extbin_ops() {
			return getRuleContext(Extbin_opsContext.class,0);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public ExtendedContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_extended; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterExtended(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitExtended(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitExtended(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ExtendedContext extended() throws RecognitionException {
		ExtendedContext _localctx = new ExtendedContext(_ctx, getState());
		enterRule(_localctx, 144, RULE_extended);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1231);
			unary();
			setState(1241);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,186,_ctx) ) {
			case 1:
				{
				setState(1233);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(1232);
					ws();
					}
				}

				setState(1235);
				extbin_ops();
				setState(1237);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,185,_ctx) ) {
				case 1:
					{
					setState(1236);
					ws();
					}
					break;
				}
				setState(1239);
				unary();
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class UnaryContext extends ParserRuleContext {
		public Unary_opsContext unary_ops() {
			return getRuleContext(Unary_opsContext.class,0);
		}
		public UnaryContext unary() {
			return getRuleContext(UnaryContext.class,0);
		}
		public WsContext ws() {
			return getRuleContext(WsContext.class,0);
		}
		public AscribeContext ascribe() {
			return getRuleContext(AscribeContext.class,0);
		}
		public UnaryContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_unary; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterUnary(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitUnary(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitUnary(this);
			else return visitor.visitChildren(this);
		}
	}

	public final UnaryContext unary() throws RecognitionException {
		UnaryContext _localctx = new UnaryContext(_ctx, getState());
		enterRule(_localctx, 146, RULE_unary);
		try {
			setState(1251);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,188,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1243);
				if (!(!( _input.LT(1).getText().equals("-") && //Make sure this isn't just a negative number
						Character.isDigit(_input.LT(2).getText().charAt(0)) ))) throw new FailedPredicateException(this, "!( _input.LT(1).getText().equals(\"-\") && //Make sure this isn't just a negative number\n\t\tCharacter.isDigit(_input.LT(2).getText().charAt(0)) )");
				{
				setState(1244);
				unary_ops();
				setState(1246);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,187,_ctx) ) {
				case 1:
					{
					setState(1245);
					ws();
					}
					break;
				}
				setState(1248);
				unary();
				}
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1250);
				ascribe();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class AscribeContext extends ParserRuleContext {
		public E_fListContext e_fList() {
			return getRuleContext(E_fListContext.class,0);
		}
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public AscribeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_ascribe; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterAscribe(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitAscribe(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitAscribe(this);
			else return visitor.visitChildren(this);
		}
	}

	public final AscribeContext ascribe() throws RecognitionException {
		AscribeContext _localctx = new AscribeContext(_ctx, getState());
		enterRule(_localctx, 148, RULE_ascribe);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1253);
			e_fList();
			setState(1262);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,191,_ctx) ) {
			case 1:
				{
				setState(1255);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(1254);
					ws();
					}
				}

				setState(1257);
				match(T__3);
				setState(1259);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(1258);
					ws();
					}
				}

				setState(1261);
				type();
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class E_primaryContext extends ParserRuleContext {
		public E_letMatchContext e_letMatch() {
			return getRuleContext(E_letMatchContext.class,0);
		}
		public E_ifThenElseContext e_ifThenElse() {
			return getRuleContext(E_ifThenElseContext.class,0);
		}
		public E_matchContext e_match() {
			return getRuleContext(E_matchContext.class,0);
		}
		public E_switchContext e_switch() {
			return getRuleContext(E_switchContext.class,0);
		}
		public E_qnameContext e_qname() {
			return getRuleContext(E_qnameContext.class,0);
		}
		public E_tagContext e_tag() {
			return getRuleContext(E_tagContext.class,0);
		}
		public E_lambdaContext e_lambda() {
			return getRuleContext(E_lambdaContext.class,0);
		}
		public E_tupleContext e_tuple() {
			return getRuleContext(E_tupleContext.class,0);
		}
		public E_fNilContext e_fNil() {
			return getRuleContext(E_fNilContext.class,0);
		}
		public E_fVecContext e_fVec() {
			return getRuleContext(E_fVecContext.class,0);
		}
		public E_fSetContext e_fSet() {
			return getRuleContext(E_fSetContext.class,0);
		}
		public E_fMapContext e_fMap() {
			return getRuleContext(E_fMapContext.class,0);
		}
		public E_literalContext e_literal() {
			return getRuleContext(E_literalContext.class,0);
		}
		public ExistentialContext existential() {
			return getRuleContext(ExistentialContext.class,0);
		}
		public UniversalContext universal() {
			return getRuleContext(UniversalContext.class,0);
		}
		public E_unaryLambdaContext e_unaryLambda() {
			return getRuleContext(E_unaryLambdaContext.class,0);
		}
		public E_wildContext e_wild() {
			return getRuleContext(E_wildContext.class,0);
		}
		public E_snameContext e_sname() {
			return getRuleContext(E_snameContext.class,0);
		}
		public E_userErrorContext e_userError() {
			return getRuleContext(E_userErrorContext.class,0);
		}
		public E_primaryContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_e_primary; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterE_primary(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitE_primary(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_primary(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_primaryContext e_primary() throws RecognitionException {
		E_primaryContext _localctx = new E_primaryContext(_ctx, getState());
		enterRule(_localctx, 150, RULE_e_primary);
		try {
			setState(1283);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,192,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1264);
				e_letMatch();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1265);
				e_ifThenElse();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1266);
				e_match();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(1267);
				e_switch();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(1268);
				e_qname();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(1269);
				e_tag();
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(1270);
				e_lambda();
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(1271);
				e_tuple();
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(1272);
				e_fNil();
				}
				break;
			case 10:
				enterOuterAlt(_localctx, 10);
				{
				setState(1273);
				e_fVec();
				}
				break;
			case 11:
				enterOuterAlt(_localctx, 11);
				{
				setState(1274);
				e_fSet();
				}
				break;
			case 12:
				enterOuterAlt(_localctx, 12);
				{
				setState(1275);
				e_fMap();
				}
				break;
			case 13:
				enterOuterAlt(_localctx, 13);
				{
				setState(1276);
				e_literal();
				}
				break;
			case 14:
				enterOuterAlt(_localctx, 14);
				{
				setState(1277);
				existential();
				}
				break;
			case 15:
				enterOuterAlt(_localctx, 15);
				{
				setState(1278);
				universal();
				}
				break;
			case 16:
				enterOuterAlt(_localctx, 16);
				{
				setState(1279);
				e_unaryLambda();
				}
				break;
			case 17:
				enterOuterAlt(_localctx, 17);
				{
				setState(1280);
				e_wild();
				}
				break;
			case 18:
				enterOuterAlt(_localctx, 18);
				{
				setState(1281);
				e_sname();
				}
				break;
			case 19:
				enterOuterAlt(_localctx, 19);
				{
				setState(1282);
				e_userError();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class E_letMatchContext extends ParserRuleContext {
		public TerminalNode LET() { return getToken(FlixParser.LET, 0); }
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public PatternContext pattern() {
			return getRuleContext(PatternContext.class,0);
		}
		public List<ExpressionContext> expression() {
			return getRuleContexts(ExpressionContext.class);
		}
		public ExpressionContext expression(int i) {
			return getRuleContext(ExpressionContext.class,i);
		}
		public E_letMatchContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_e_letMatch; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterE_letMatch(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitE_letMatch(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_letMatch(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_letMatchContext e_letMatch() throws RecognitionException {
		E_letMatchContext _localctx = new E_letMatchContext(_ctx, getState());
		enterRule(_localctx, 152, RULE_e_letMatch);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1285);
			match(LET);
			setState(1286);
			ws();
			setState(1287);
			pattern();
			setState(1289);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1288);
				ws();
				}
			}

			setState(1291);
			match(T__13);
			setState(1293);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,194,_ctx) ) {
			case 1:
				{
				setState(1292);
				ws();
				}
				break;
			}
			setState(1295);
			expression();
			setState(1297);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1296);
				ws();
				}
			}

			setState(1299);
			match(SC);
			setState(1301);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,196,_ctx) ) {
			case 1:
				{
				setState(1300);
				ws();
				}
				break;
			}
			setState(1303);
			expression();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class E_ifThenElseContext extends ParserRuleContext {
		public TerminalNode IF() { return getToken(FlixParser.IF, 0); }
		public List<ExpressionContext> expression() {
			return getRuleContexts(ExpressionContext.class);
		}
		public ExpressionContext expression(int i) {
			return getRuleContext(ExpressionContext.class,i);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public TerminalNode ELSE() { return getToken(FlixParser.ELSE, 0); }
		public E_ifThenElseContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_e_ifThenElse; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterE_ifThenElse(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitE_ifThenElse(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_ifThenElse(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_ifThenElseContext e_ifThenElse() throws RecognitionException {
		E_ifThenElseContext _localctx = new E_ifThenElseContext(_ctx, getState());
		enterRule(_localctx, 154, RULE_e_ifThenElse);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1305);
			match(IF);
			setState(1307);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1306);
				ws();
				}
			}

			setState(1309);
			match(T__4);
			setState(1311);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,198,_ctx) ) {
			case 1:
				{
				setState(1310);
				ws();
				}
				break;
			}
			setState(1313);
			expression();
			setState(1315);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1314);
				ws();
				}
			}

			setState(1317);
			match(T__5);
			setState(1319);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,200,_ctx) ) {
			case 1:
				{
				setState(1318);
				ws();
				}
				break;
			}
			setState(1321);
			expression();
			setState(1322);
			ws();
			setState(1323);
			match(ELSE);
			setState(1324);
			ws();
			setState(1325);
			expression();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class E_matchContext extends ParserRuleContext {
		public TerminalNode MATCH() { return getToken(FlixParser.MATCH, 0); }
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public TerminalNode WITH() { return getToken(FlixParser.WITH, 0); }
		public Match_rulesContext match_rules() {
			return getRuleContext(Match_rulesContext.class,0);
		}
		public E_matchContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_e_match; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterE_match(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitE_match(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_match(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_matchContext e_match() throws RecognitionException {
		E_matchContext _localctx = new E_matchContext(_ctx, getState());
		enterRule(_localctx, 156, RULE_e_match);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1327);
			match(MATCH);
			setState(1328);
			ws();
			setState(1329);
			expression();
			setState(1330);
			ws();
			setState(1331);
			match(WITH);
			setState(1332);
			ws();
			setState(1333);
			match(T__6);
			setState(1335);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1334);
				ws();
				}
			}

			setState(1337);
			match_rules();
			setState(1339);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1338);
				ws();
				}
			}

			setState(1341);
			match(T__7);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class E_switchContext extends ParserRuleContext {
		public TerminalNode SWITCH() { return getToken(FlixParser.SWITCH, 0); }
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public Switch_rulesContext switch_rules() {
			return getRuleContext(Switch_rulesContext.class,0);
		}
		public E_switchContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_e_switch; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterE_switch(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitE_switch(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_switch(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_switchContext e_switch() throws RecognitionException {
		E_switchContext _localctx = new E_switchContext(_ctx, getState());
		enterRule(_localctx, 158, RULE_e_switch);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1343);
			match(SWITCH);
			setState(1344);
			ws();
			setState(1345);
			match(T__6);
			setState(1347);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1346);
				ws();
				}
			}

			setState(1349);
			switch_rules();
			setState(1351);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1350);
				ws();
				}
			}

			setState(1353);
			match(T__7);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class E_applyContext extends ParserRuleContext {
		public E_primaryContext e_primary() {
			return getRuleContext(E_primaryContext.class,0);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public ExpressionsContext expressions() {
			return getRuleContext(ExpressionsContext.class,0);
		}
		public E_applyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_e_apply; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterE_apply(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitE_apply(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_apply(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_applyContext e_apply() throws RecognitionException {
		E_applyContext _localctx = new E_applyContext(_ctx, getState());
		enterRule(_localctx, 160, RULE_e_apply);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1355);
			e_primary();
			setState(1370);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,209,_ctx) ) {
			case 1:
				{
				setState(1357);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(1356);
					ws();
					}
				}

				setState(1359);
				match(T__4);
				setState(1361);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,206,_ctx) ) {
				case 1:
					{
					setState(1360);
					ws();
					}
					break;
				}
				setState(1364);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,207,_ctx) ) {
				case 1:
					{
					setState(1363);
					expressions();
					}
					break;
				}
				setState(1367);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(1366);
					ws();
					}
				}

				setState(1369);
				match(T__5);
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class E_unaryLambdaContext extends ParserRuleContext {
		public VariableNameContext variableName() {
			return getRuleContext(VariableNameContext.class,0);
		}
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public E_unaryLambdaContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_e_unaryLambda; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterE_unaryLambda(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitE_unaryLambda(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_unaryLambda(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_unaryLambdaContext e_unaryLambda() throws RecognitionException {
		E_unaryLambdaContext _localctx = new E_unaryLambdaContext(_ctx, getState());
		enterRule(_localctx, 162, RULE_e_unaryLambda);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1372);
			variableName();
			setState(1374);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1373);
				ws();
				}
			}

			setState(1376);
			match(T__17);
			setState(1378);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,211,_ctx) ) {
			case 1:
				{
				setState(1377);
				ws();
				}
				break;
			}
			setState(1380);
			expression();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class E_lambdaContext extends ParserRuleContext {
		public VariableNamesContext variableNames() {
			return getRuleContext(VariableNamesContext.class,0);
		}
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public E_lambdaContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_e_lambda; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterE_lambda(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitE_lambda(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_lambda(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_lambdaContext e_lambda() throws RecognitionException {
		E_lambdaContext _localctx = new E_lambdaContext(_ctx, getState());
		enterRule(_localctx, 164, RULE_e_lambda);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1382);
			match(T__4);
			setState(1384);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1383);
				ws();
				}
			}

			setState(1386);
			variableNames();
			setState(1388);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1387);
				ws();
				}
			}

			setState(1390);
			match(T__5);
			setState(1392);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1391);
				ws();
				}
			}

			setState(1394);
			match(T__17);
			setState(1396);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,215,_ctx) ) {
			case 1:
				{
				setState(1395);
				ws();
				}
				break;
			}
			setState(1398);
			expression();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class E_literalContext extends ParserRuleContext {
		public LiteralContext literal() {
			return getRuleContext(LiteralContext.class,0);
		}
		public E_literalContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_e_literal; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterE_literal(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitE_literal(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_literal(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_literalContext e_literal() throws RecognitionException {
		E_literalContext _localctx = new E_literalContext(_ctx, getState());
		enterRule(_localctx, 166, RULE_e_literal);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1400);
			literal();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class E_snameContext extends ParserRuleContext {
		public VariableNameContext variableName() {
			return getRuleContext(VariableNameContext.class,0);
		}
		public E_snameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_e_sname; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterE_sname(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitE_sname(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_sname(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_snameContext e_sname() throws RecognitionException {
		E_snameContext _localctx = new E_snameContext(_ctx, getState());
		enterRule(_localctx, 168, RULE_e_sname);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1402);
			variableName();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class E_qnameContext extends ParserRuleContext {
		public QualifiedDefinitionNameContext qualifiedDefinitionName() {
			return getRuleContext(QualifiedDefinitionNameContext.class,0);
		}
		public E_qnameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_e_qname; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterE_qname(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitE_qname(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_qname(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_qnameContext e_qname() throws RecognitionException {
		E_qnameContext _localctx = new E_qnameContext(_ctx, getState());
		enterRule(_localctx, 170, RULE_e_qname);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1404);
			qualifiedDefinitionName();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class E_tagContext extends ParserRuleContext {
		public TagNameContext tagName() {
			return getRuleContext(TagNameContext.class,0);
		}
		public QualifiedTypeNameContext qualifiedTypeName() {
			return getRuleContext(QualifiedTypeNameContext.class,0);
		}
		public E_tupleContext e_tuple() {
			return getRuleContext(E_tupleContext.class,0);
		}
		public WsContext ws() {
			return getRuleContext(WsContext.class,0);
		}
		public E_tagContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_e_tag; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterE_tag(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitE_tag(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_tag(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_tagContext e_tag() throws RecognitionException {
		E_tagContext _localctx = new E_tagContext(_ctx, getState());
		enterRule(_localctx, 172, RULE_e_tag);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1409);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,216,_ctx) ) {
			case 1:
				{
				setState(1406);
				qualifiedTypeName();
				setState(1407);
				match(T__0);
				}
				break;
			}
			setState(1411);
			tagName();
			setState(1416);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,218,_ctx) ) {
			case 1:
				{
				setState(1413);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(1412);
					ws();
					}
				}

				setState(1415);
				e_tuple();
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class E_tupleContext extends ParserRuleContext {
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public ExpressionsContext expressions() {
			return getRuleContext(ExpressionsContext.class,0);
		}
		public E_tupleContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_e_tuple; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterE_tuple(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitE_tuple(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_tuple(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_tupleContext e_tuple() throws RecognitionException {
		E_tupleContext _localctx = new E_tupleContext(_ctx, getState());
		enterRule(_localctx, 174, RULE_e_tuple);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1418);
			match(T__4);
			setState(1420);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,219,_ctx) ) {
			case 1:
				{
				setState(1419);
				ws();
				}
				break;
			}
			setState(1423);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,220,_ctx) ) {
			case 1:
				{
				setState(1422);
				expressions();
				}
				break;
			}
			setState(1426);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1425);
				ws();
				}
			}

			setState(1428);
			match(T__5);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class E_keyValueContext extends ParserRuleContext {
		public List<ExpressionContext> expression() {
			return getRuleContexts(ExpressionContext.class);
		}
		public ExpressionContext expression(int i) {
			return getRuleContext(ExpressionContext.class,i);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public E_keyValueContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_e_keyValue; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterE_keyValue(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitE_keyValue(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_keyValue(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_keyValueContext e_keyValue() throws RecognitionException {
		E_keyValueContext _localctx = new E_keyValueContext(_ctx, getState());
		enterRule(_localctx, 176, RULE_e_keyValue);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1430);
			expression();
			setState(1432);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1431);
				ws();
				}
			}

			setState(1434);
			match(T__17);
			setState(1436);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,223,_ctx) ) {
			case 1:
				{
				setState(1435);
				ws();
				}
				break;
			}
			setState(1438);
			expression();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class E_keyValuesContext extends ParserRuleContext {
		public List<E_keyValueContext> e_keyValue() {
			return getRuleContexts(E_keyValueContext.class);
		}
		public E_keyValueContext e_keyValue(int i) {
			return getRuleContext(E_keyValueContext.class,i);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public E_keyValuesContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_e_keyValues; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterE_keyValues(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitE_keyValues(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_keyValues(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_keyValuesContext e_keyValues() throws RecognitionException {
		E_keyValuesContext _localctx = new E_keyValuesContext(_ctx, getState());
		enterRule(_localctx, 178, RULE_e_keyValues);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1440);
			e_keyValue();
			setState(1451);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,226,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1442);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(1441);
						ws();
						}
					}

					setState(1444);
					match(T__2);
					setState(1446);
					_errHandler.sync(this);
					switch ( getInterpreter().adaptivePredict(_input,225,_ctx) ) {
					case 1:
						{
						setState(1445);
						ws();
						}
						break;
					}
					setState(1448);
					e_keyValue();
					}
					} 
				}
				setState(1453);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,226,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class E_userErrorContext extends ParserRuleContext {
		public E_userErrorContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_e_userError; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterE_userError(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitE_userError(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_userError(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_userErrorContext e_userError() throws RecognitionException {
		E_userErrorContext _localctx = new E_userErrorContext(_ctx, getState());
		enterRule(_localctx, 180, RULE_e_userError);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1454);
			match(T__18);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class E_wildContext extends ParserRuleContext {
		public TerminalNode WILD() { return getToken(FlixParser.WILD, 0); }
		public E_wildContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_e_wild; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterE_wild(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitE_wild(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_wild(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_wildContext e_wild() throws RecognitionException {
		E_wildContext _localctx = new E_wildContext(_ctx, getState());
		enterRule(_localctx, 182, RULE_e_wild);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1456);
			match(WILD);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class E_fNilContext extends ParserRuleContext {
		public TerminalNode FNIL() { return getToken(FlixParser.FNIL, 0); }
		public E_fNilContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_e_fNil; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterE_fNil(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitE_fNil(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_fNil(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_fNilContext e_fNil() throws RecognitionException {
		E_fNilContext _localctx = new E_fNilContext(_ctx, getState());
		enterRule(_localctx, 184, RULE_e_fNil);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1458);
			match(FNIL);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class E_fListContext extends ParserRuleContext {
		public E_applyContext e_apply() {
			return getRuleContext(E_applyContext.class,0);
		}
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public E_fListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_e_fList; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterE_fList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitE_fList(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_fList(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_fListContext e_fList() throws RecognitionException {
		E_fListContext _localctx = new E_fListContext(_ctx, getState());
		enterRule(_localctx, 186, RULE_e_fList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1460);
			e_apply();
			setState(1469);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,229,_ctx) ) {
			case 1:
				{
				setState(1462);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(1461);
					ws();
					}
				}

				setState(1464);
				match(T__19);
				setState(1466);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,228,_ctx) ) {
				case 1:
					{
					setState(1465);
					ws();
					}
					break;
				}
				setState(1468);
				expression();
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class E_fVecContext extends ParserRuleContext {
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public ExpressionsContext expressions() {
			return getRuleContext(ExpressionsContext.class,0);
		}
		public E_fVecContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_e_fVec; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterE_fVec(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitE_fVec(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_fVec(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_fVecContext e_fVec() throws RecognitionException {
		E_fVecContext _localctx = new E_fVecContext(_ctx, getState());
		enterRule(_localctx, 188, RULE_e_fVec);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1471);
			match(T__20);
			setState(1473);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,230,_ctx) ) {
			case 1:
				{
				setState(1472);
				ws();
				}
				break;
			}
			setState(1476);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,231,_ctx) ) {
			case 1:
				{
				setState(1475);
				expressions();
				}
				break;
			}
			setState(1479);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1478);
				ws();
				}
			}

			setState(1481);
			match(T__10);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class E_fSetContext extends ParserRuleContext {
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public ExpressionsContext expressions() {
			return getRuleContext(ExpressionsContext.class,0);
		}
		public E_fSetContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_e_fSet; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterE_fSet(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitE_fSet(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_fSet(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_fSetContext e_fSet() throws RecognitionException {
		E_fSetContext _localctx = new E_fSetContext(_ctx, getState());
		enterRule(_localctx, 190, RULE_e_fSet);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1483);
			match(T__21);
			setState(1485);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,233,_ctx) ) {
			case 1:
				{
				setState(1484);
				ws();
				}
				break;
			}
			setState(1488);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,234,_ctx) ) {
			case 1:
				{
				setState(1487);
				expressions();
				}
				break;
			}
			setState(1491);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1490);
				ws();
				}
			}

			setState(1493);
			match(T__7);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class E_fMapContext extends ParserRuleContext {
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public E_keyValuesContext e_keyValues() {
			return getRuleContext(E_keyValuesContext.class,0);
		}
		public E_fMapContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_e_fMap; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterE_fMap(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitE_fMap(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_fMap(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_fMapContext e_fMap() throws RecognitionException {
		E_fMapContext _localctx = new E_fMapContext(_ctx, getState());
		enterRule(_localctx, 192, RULE_e_fMap);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1495);
			match(T__22);
			setState(1497);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,236,_ctx) ) {
			case 1:
				{
				setState(1496);
				ws();
				}
				break;
			}
			setState(1500);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,237,_ctx) ) {
			case 1:
				{
				setState(1499);
				e_keyValues();
				}
				break;
			}
			setState(1503);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1502);
				ws();
				}
			}

			setState(1505);
			match(T__7);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExistentialContext extends ParserRuleContext {
		public FormalparamsContext formalparams() {
			return getRuleContext(FormalparamsContext.class,0);
		}
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public ExistentialContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_existential; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterExistential(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitExistential(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitExistential(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ExistentialContext existential() throws RecognitionException {
		ExistentialContext _localctx = new ExistentialContext(_ctx, getState());
		enterRule(_localctx, 194, RULE_existential);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1507);
			_la = _input.LA(1);
			if ( !(_la==T__23 || _la==T__24) ) {
			_errHandler.recoverInline(this);
			} else {
				consume();
			}
			setState(1509);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,239,_ctx) ) {
			case 1:
				{
				setState(1508);
				ws();
				}
				break;
			}
			setState(1511);
			formalparams();
			setState(1513);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1512);
				ws();
				}
			}

			setState(1515);
			match(T__0);
			setState(1517);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,241,_ctx) ) {
			case 1:
				{
				setState(1516);
				ws();
				}
				break;
			}
			setState(1519);
			expression();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class UniversalContext extends ParserRuleContext {
		public FormalparamsContext formalparams() {
			return getRuleContext(FormalparamsContext.class,0);
		}
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public UniversalContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_universal; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterUniversal(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitUniversal(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitUniversal(this);
			else return visitor.visitChildren(this);
		}
	}

	public final UniversalContext universal() throws RecognitionException {
		UniversalContext _localctx = new UniversalContext(_ctx, getState());
		enterRule(_localctx, 196, RULE_universal);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1521);
			_la = _input.LA(1);
			if ( !(_la==T__25 || _la==T__26) ) {
			_errHandler.recoverInline(this);
			} else {
				consume();
			}
			setState(1523);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,242,_ctx) ) {
			case 1:
				{
				setState(1522);
				ws();
				}
				break;
			}
			setState(1525);
			formalparams();
			setState(1527);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1526);
				ws();
				}
			}

			setState(1529);
			match(T__0);
			setState(1531);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,244,_ctx) ) {
			case 1:
				{
				setState(1530);
				ws();
				}
				break;
			}
			setState(1533);
			expression();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class PatternContext extends ParserRuleContext {
		public P_fListContext p_fList() {
			return getRuleContext(P_fListContext.class,0);
		}
		public PatternContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_pattern; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterPattern(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitPattern(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitPattern(this);
			else return visitor.visitChildren(this);
		}
	}

	public final PatternContext pattern() throws RecognitionException {
		PatternContext _localctx = new PatternContext(_ctx, getState());
		enterRule(_localctx, 198, RULE_pattern);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1535);
			p_fList();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class PatternsContext extends ParserRuleContext {
		public List<PatternContext> pattern() {
			return getRuleContexts(PatternContext.class);
		}
		public PatternContext pattern(int i) {
			return getRuleContext(PatternContext.class,i);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public PatternsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_patterns; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterPatterns(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitPatterns(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitPatterns(this);
			else return visitor.visitChildren(this);
		}
	}

	public final PatternsContext patterns() throws RecognitionException {
		PatternsContext _localctx = new PatternsContext(_ctx, getState());
		enterRule(_localctx, 200, RULE_patterns);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1537);
			pattern();
			setState(1548);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,247,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1539);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(1538);
						ws();
						}
					}

					setState(1541);
					match(T__2);
					setState(1543);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(1542);
						ws();
						}
					}

					setState(1545);
					pattern();
					}
					} 
				}
				setState(1550);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,247,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SimpleContext extends ParserRuleContext {
		public P_fNilContext p_fNil() {
			return getRuleContext(P_fNilContext.class,0);
		}
		public P_literalContext p_literal() {
			return getRuleContext(P_literalContext.class,0);
		}
		public P_variableContext p_variable() {
			return getRuleContext(P_variableContext.class,0);
		}
		public P_wildContext p_wild() {
			return getRuleContext(P_wildContext.class,0);
		}
		public P_tagContext p_tag() {
			return getRuleContext(P_tagContext.class,0);
		}
		public P_tupleContext p_tuple() {
			return getRuleContext(P_tupleContext.class,0);
		}
		public P_fVecContext p_fVec() {
			return getRuleContext(P_fVecContext.class,0);
		}
		public P_fSetContext p_fSet() {
			return getRuleContext(P_fSetContext.class,0);
		}
		public P_fMapContext p_fMap() {
			return getRuleContext(P_fMapContext.class,0);
		}
		public SimpleContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_simple; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterSimple(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitSimple(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitSimple(this);
			else return visitor.visitChildren(this);
		}
	}

	public final SimpleContext simple() throws RecognitionException {
		SimpleContext _localctx = new SimpleContext(_ctx, getState());
		enterRule(_localctx, 202, RULE_simple);
		try {
			setState(1560);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,248,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1551);
				p_fNil();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1552);
				p_literal();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1553);
				p_variable();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(1554);
				p_wild();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(1555);
				p_tag();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(1556);
				p_tuple();
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(1557);
				p_fVec();
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(1558);
				p_fSet();
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(1559);
				p_fMap();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class P_keyValueContext extends ParserRuleContext {
		public List<PatternContext> pattern() {
			return getRuleContexts(PatternContext.class);
		}
		public PatternContext pattern(int i) {
			return getRuleContext(PatternContext.class,i);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public P_keyValueContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_p_keyValue; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterP_keyValue(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitP_keyValue(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitP_keyValue(this);
			else return visitor.visitChildren(this);
		}
	}

	public final P_keyValueContext p_keyValue() throws RecognitionException {
		P_keyValueContext _localctx = new P_keyValueContext(_ctx, getState());
		enterRule(_localctx, 204, RULE_p_keyValue);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1562);
			pattern();
			setState(1564);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1563);
				ws();
				}
			}

			setState(1566);
			match(T__17);
			setState(1568);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1567);
				ws();
				}
			}

			setState(1570);
			pattern();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class P_keyValuesContext extends ParserRuleContext {
		public List<P_keyValueContext> p_keyValue() {
			return getRuleContexts(P_keyValueContext.class);
		}
		public P_keyValueContext p_keyValue(int i) {
			return getRuleContext(P_keyValueContext.class,i);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public P_keyValuesContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_p_keyValues; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterP_keyValues(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitP_keyValues(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitP_keyValues(this);
			else return visitor.visitChildren(this);
		}
	}

	public final P_keyValuesContext p_keyValues() throws RecognitionException {
		P_keyValuesContext _localctx = new P_keyValuesContext(_ctx, getState());
		enterRule(_localctx, 206, RULE_p_keyValues);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1572);
			p_keyValue();
			setState(1583);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,253,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1574);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(1573);
						ws();
						}
					}

					setState(1576);
					match(T__2);
					setState(1578);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(1577);
						ws();
						}
					}

					setState(1580);
					p_keyValue();
					}
					} 
				}
				setState(1585);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,253,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class P_literalContext extends ParserRuleContext {
		public LiteralContext literal() {
			return getRuleContext(LiteralContext.class,0);
		}
		public P_literalContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_p_literal; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterP_literal(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitP_literal(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitP_literal(this);
			else return visitor.visitChildren(this);
		}
	}

	public final P_literalContext p_literal() throws RecognitionException {
		P_literalContext _localctx = new P_literalContext(_ctx, getState());
		enterRule(_localctx, 208, RULE_p_literal);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1586);
			literal();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class P_tagContext extends ParserRuleContext {
		public TagNameContext tagName() {
			return getRuleContext(TagNameContext.class,0);
		}
		public QualifiedTypeNameContext qualifiedTypeName() {
			return getRuleContext(QualifiedTypeNameContext.class,0);
		}
		public PatternContext pattern() {
			return getRuleContext(PatternContext.class,0);
		}
		public WsContext ws() {
			return getRuleContext(WsContext.class,0);
		}
		public P_tagContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_p_tag; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterP_tag(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitP_tag(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitP_tag(this);
			else return visitor.visitChildren(this);
		}
	}

	public final P_tagContext p_tag() throws RecognitionException {
		P_tagContext _localctx = new P_tagContext(_ctx, getState());
		enterRule(_localctx, 210, RULE_p_tag);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1591);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,254,_ctx) ) {
			case 1:
				{
				setState(1588);
				qualifiedTypeName();
				setState(1589);
				match(T__0);
				}
				break;
			}
			setState(1593);
			tagName();
			setState(1598);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,256,_ctx) ) {
			case 1:
				{
				setState(1595);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(1594);
					ws();
					}
				}

				setState(1597);
				pattern();
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class P_tupleContext extends ParserRuleContext {
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public PatternsContext patterns() {
			return getRuleContext(PatternsContext.class,0);
		}
		public P_tupleContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_p_tuple; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterP_tuple(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitP_tuple(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitP_tuple(this);
			else return visitor.visitChildren(this);
		}
	}

	public final P_tupleContext p_tuple() throws RecognitionException {
		P_tupleContext _localctx = new P_tupleContext(_ctx, getState());
		enterRule(_localctx, 212, RULE_p_tuple);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1600);
			match(T__4);
			setState(1602);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,257,_ctx) ) {
			case 1:
				{
				setState(1601);
				ws();
				}
				break;
			}
			setState(1605);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__4) | (1L << T__20) | (1L << T__21) | (1L << T__22) | (1L << T__28) | (1L << T__29) | (1L << T__30))) != 0) || ((((_la - 86)) & ~0x3f) == 0 && ((1L << (_la - 86)) & ((1L << (FNIL - 86)) | (1L << (WILD - 86)) | (1L << (LowerIdent - 86)) | (1L << (UpperIdent - 86)) | (1L << (Chars - 86)) | (1L << (Strs - 86)) | (1L << (Digits - 86)))) != 0)) {
				{
				setState(1604);
				patterns();
				}
			}

			setState(1608);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1607);
				ws();
				}
			}

			setState(1610);
			match(T__5);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class P_wildContext extends ParserRuleContext {
		public TerminalNode WILD() { return getToken(FlixParser.WILD, 0); }
		public P_wildContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_p_wild; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterP_wild(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitP_wild(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitP_wild(this);
			else return visitor.visitChildren(this);
		}
	}

	public final P_wildContext p_wild() throws RecognitionException {
		P_wildContext _localctx = new P_wildContext(_ctx, getState());
		enterRule(_localctx, 214, RULE_p_wild);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1612);
			match(WILD);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class P_fNilContext extends ParserRuleContext {
		public TerminalNode FNIL() { return getToken(FlixParser.FNIL, 0); }
		public P_fNilContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_p_fNil; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterP_fNil(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitP_fNil(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitP_fNil(this);
			else return visitor.visitChildren(this);
		}
	}

	public final P_fNilContext p_fNil() throws RecognitionException {
		P_fNilContext _localctx = new P_fNilContext(_ctx, getState());
		enterRule(_localctx, 216, RULE_p_fNil);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1614);
			match(FNIL);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class P_variableContext extends ParserRuleContext {
		public VariableNameContext variableName() {
			return getRuleContext(VariableNameContext.class,0);
		}
		public P_variableContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_p_variable; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterP_variable(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitP_variable(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitP_variable(this);
			else return visitor.visitChildren(this);
		}
	}

	public final P_variableContext p_variable() throws RecognitionException {
		P_variableContext _localctx = new P_variableContext(_ctx, getState());
		enterRule(_localctx, 218, RULE_p_variable);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1616);
			variableName();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class P_fListContext extends ParserRuleContext {
		public SimpleContext simple() {
			return getRuleContext(SimpleContext.class,0);
		}
		public PatternContext pattern() {
			return getRuleContext(PatternContext.class,0);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public P_fListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_p_fList; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterP_fList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitP_fList(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitP_fList(this);
			else return visitor.visitChildren(this);
		}
	}

	public final P_fListContext p_fList() throws RecognitionException {
		P_fListContext _localctx = new P_fListContext(_ctx, getState());
		enterRule(_localctx, 220, RULE_p_fList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1618);
			simple();
			setState(1627);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,262,_ctx) ) {
			case 1:
				{
				setState(1620);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(1619);
					ws();
					}
				}

				setState(1622);
				match(T__19);
				setState(1624);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(1623);
					ws();
					}
				}

				setState(1626);
				pattern();
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class P_fVecContext extends ParserRuleContext {
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public PatternsContext patterns() {
			return getRuleContext(PatternsContext.class,0);
		}
		public PatternContext pattern() {
			return getRuleContext(PatternContext.class,0);
		}
		public P_fVecContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_p_fVec; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterP_fVec(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitP_fVec(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitP_fVec(this);
			else return visitor.visitChildren(this);
		}
	}

	public final P_fVecContext p_fVec() throws RecognitionException {
		P_fVecContext _localctx = new P_fVecContext(_ctx, getState());
		enterRule(_localctx, 222, RULE_p_fVec);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1629);
			match(T__20);
			setState(1631);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,263,_ctx) ) {
			case 1:
				{
				setState(1630);
				ws();
				}
				break;
			}
			setState(1634);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__4) | (1L << T__20) | (1L << T__21) | (1L << T__22) | (1L << T__28) | (1L << T__29) | (1L << T__30))) != 0) || ((((_la - 86)) & ~0x3f) == 0 && ((1L << (_la - 86)) & ((1L << (FNIL - 86)) | (1L << (WILD - 86)) | (1L << (LowerIdent - 86)) | (1L << (UpperIdent - 86)) | (1L << (Chars - 86)) | (1L << (Strs - 86)) | (1L << (Digits - 86)))) != 0)) {
				{
				setState(1633);
				patterns();
				}
			}

			setState(1646);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,267,_ctx) ) {
			case 1:
				{
				setState(1637);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(1636);
					ws();
					}
				}

				setState(1639);
				match(T__2);
				setState(1641);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(1640);
					ws();
					}
				}

				setState(1643);
				pattern();
				setState(1644);
				match(T__27);
				}
				break;
			}
			setState(1649);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1648);
				ws();
				}
			}

			setState(1651);
			match(T__10);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class P_fSetContext extends ParserRuleContext {
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public PatternsContext patterns() {
			return getRuleContext(PatternsContext.class,0);
		}
		public PatternContext pattern() {
			return getRuleContext(PatternContext.class,0);
		}
		public P_fSetContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_p_fSet; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterP_fSet(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitP_fSet(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitP_fSet(this);
			else return visitor.visitChildren(this);
		}
	}

	public final P_fSetContext p_fSet() throws RecognitionException {
		P_fSetContext _localctx = new P_fSetContext(_ctx, getState());
		enterRule(_localctx, 224, RULE_p_fSet);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1653);
			match(T__21);
			setState(1655);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,269,_ctx) ) {
			case 1:
				{
				setState(1654);
				ws();
				}
				break;
			}
			setState(1658);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__4) | (1L << T__20) | (1L << T__21) | (1L << T__22) | (1L << T__28) | (1L << T__29) | (1L << T__30))) != 0) || ((((_la - 86)) & ~0x3f) == 0 && ((1L << (_la - 86)) & ((1L << (FNIL - 86)) | (1L << (WILD - 86)) | (1L << (LowerIdent - 86)) | (1L << (UpperIdent - 86)) | (1L << (Chars - 86)) | (1L << (Strs - 86)) | (1L << (Digits - 86)))) != 0)) {
				{
				setState(1657);
				patterns();
				}
			}

			setState(1670);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,273,_ctx) ) {
			case 1:
				{
				setState(1661);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(1660);
					ws();
					}
				}

				setState(1663);
				match(T__2);
				setState(1665);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(1664);
					ws();
					}
				}

				setState(1667);
				pattern();
				setState(1668);
				match(T__27);
				}
				break;
			}
			setState(1673);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1672);
				ws();
				}
			}

			setState(1675);
			match(T__7);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class P_fMapContext extends ParserRuleContext {
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public P_keyValuesContext p_keyValues() {
			return getRuleContext(P_keyValuesContext.class,0);
		}
		public PatternContext pattern() {
			return getRuleContext(PatternContext.class,0);
		}
		public P_fMapContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_p_fMap; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterP_fMap(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitP_fMap(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitP_fMap(this);
			else return visitor.visitChildren(this);
		}
	}

	public final P_fMapContext p_fMap() throws RecognitionException {
		P_fMapContext _localctx = new P_fMapContext(_ctx, getState());
		enterRule(_localctx, 226, RULE_p_fMap);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1677);
			match(T__22);
			setState(1679);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,275,_ctx) ) {
			case 1:
				{
				setState(1678);
				ws();
				}
				break;
			}
			setState(1682);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__4) | (1L << T__20) | (1L << T__21) | (1L << T__22) | (1L << T__28) | (1L << T__29) | (1L << T__30))) != 0) || ((((_la - 86)) & ~0x3f) == 0 && ((1L << (_la - 86)) & ((1L << (FNIL - 86)) | (1L << (WILD - 86)) | (1L << (LowerIdent - 86)) | (1L << (UpperIdent - 86)) | (1L << (Chars - 86)) | (1L << (Strs - 86)) | (1L << (Digits - 86)))) != 0)) {
				{
				setState(1681);
				p_keyValues();
				}
			}

			setState(1694);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,279,_ctx) ) {
			case 1:
				{
				setState(1685);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(1684);
					ws();
					}
				}

				setState(1687);
				match(T__2);
				setState(1689);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(1688);
					ws();
					}
				}

				setState(1691);
				pattern();
				setState(1692);
				match(T__27);
				}
				break;
			}
			setState(1697);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1696);
				ws();
				}
			}

			setState(1699);
			match(T__7);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class BoolsContext extends ParserRuleContext {
		public BoolsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_bools; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterBools(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitBools(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitBools(this);
			else return visitor.visitChildren(this);
		}
	}

	public final BoolsContext bools() throws RecognitionException {
		BoolsContext _localctx = new BoolsContext(_ctx, getState());
		enterRule(_localctx, 228, RULE_bools);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1701);
			_la = _input.LA(1);
			if ( !(_la==T__28 || _la==T__29) ) {
			_errHandler.recoverInline(this);
			} else {
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class CharsContext extends ParserRuleContext {
		public TerminalNode Chars() { return getToken(FlixParser.Chars, 0); }
		public CharsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_chars; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterChars(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitChars(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitChars(this);
			else return visitor.visitChildren(this);
		}
	}

	public final CharsContext chars() throws RecognitionException {
		CharsContext _localctx = new CharsContext(_ctx, getState());
		enterRule(_localctx, 230, RULE_chars);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1703);
			match(Chars);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class StrsContext extends ParserRuleContext {
		public TerminalNode Strs() { return getToken(FlixParser.Strs, 0); }
		public StrsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_strs; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterStrs(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitStrs(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitStrs(this);
			else return visitor.visitChildren(this);
		}
	}

	public final StrsContext strs() throws RecognitionException {
		StrsContext _localctx = new StrsContext(_ctx, getState());
		enterRule(_localctx, 232, RULE_strs);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1705);
			match(Strs);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class NegativeContext extends ParserRuleContext {
		public NegativeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_negative; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterNegative(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitNegative(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitNegative(this);
			else return visitor.visitChildren(this);
		}
	}

	public final NegativeContext negative() throws RecognitionException {
		NegativeContext _localctx = new NegativeContext(_ctx, getState());
		enterRule(_localctx, 234, RULE_negative);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1707);
			match(T__30);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Float32Context extends ParserRuleContext {
		public List<TerminalNode> Digits() { return getTokens(FlixParser.Digits); }
		public TerminalNode Digits(int i) {
			return getToken(FlixParser.Digits, i);
		}
		public NegativeContext negative() {
			return getRuleContext(NegativeContext.class,0);
		}
		public Float32Context(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_float32; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterFloat32(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitFloat32(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitFloat32(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Float32Context float32() throws RecognitionException {
		Float32Context _localctx = new Float32Context(_ctx, getState());
		enterRule(_localctx, 236, RULE_float32);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1710);
			_la = _input.LA(1);
			if (_la==T__30) {
				{
				setState(1709);
				negative();
				}
			}

			setState(1712);
			match(Digits);
			setState(1713);
			match(T__0);
			setState(1714);
			match(Digits);
			setState(1715);
			match(T__31);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Float64Context extends ParserRuleContext {
		public List<TerminalNode> Digits() { return getTokens(FlixParser.Digits); }
		public TerminalNode Digits(int i) {
			return getToken(FlixParser.Digits, i);
		}
		public NegativeContext negative() {
			return getRuleContext(NegativeContext.class,0);
		}
		public Float64Context(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_float64; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterFloat64(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitFloat64(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitFloat64(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Float64Context float64() throws RecognitionException {
		Float64Context _localctx = new Float64Context(_ctx, getState());
		enterRule(_localctx, 238, RULE_float64);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1718);
			_la = _input.LA(1);
			if (_la==T__30) {
				{
				setState(1717);
				negative();
				}
			}

			setState(1720);
			match(Digits);
			setState(1721);
			match(T__0);
			setState(1722);
			match(Digits);
			setState(1723);
			match(T__32);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class FloatDefaultContext extends ParserRuleContext {
		public List<TerminalNode> Digits() { return getTokens(FlixParser.Digits); }
		public TerminalNode Digits(int i) {
			return getToken(FlixParser.Digits, i);
		}
		public NegativeContext negative() {
			return getRuleContext(NegativeContext.class,0);
		}
		public FloatDefaultContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_floatDefault; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterFloatDefault(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitFloatDefault(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitFloatDefault(this);
			else return visitor.visitChildren(this);
		}
	}

	public final FloatDefaultContext floatDefault() throws RecognitionException {
		FloatDefaultContext _localctx = new FloatDefaultContext(_ctx, getState());
		enterRule(_localctx, 240, RULE_floatDefault);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1726);
			_la = _input.LA(1);
			if (_la==T__30) {
				{
				setState(1725);
				negative();
				}
			}

			setState(1728);
			match(Digits);
			setState(1729);
			match(T__0);
			setState(1730);
			match(Digits);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class FloatsContext extends ParserRuleContext {
		public Float32Context float32() {
			return getRuleContext(Float32Context.class,0);
		}
		public Float64Context float64() {
			return getRuleContext(Float64Context.class,0);
		}
		public FloatDefaultContext floatDefault() {
			return getRuleContext(FloatDefaultContext.class,0);
		}
		public FloatsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_floats; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterFloats(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitFloats(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitFloats(this);
			else return visitor.visitChildren(this);
		}
	}

	public final FloatsContext floats() throws RecognitionException {
		FloatsContext _localctx = new FloatsContext(_ctx, getState());
		enterRule(_localctx, 242, RULE_floats);
		try {
			setState(1735);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,284,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1732);
				float32();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1733);
				float64();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1734);
				floatDefault();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Int8Context extends ParserRuleContext {
		public TerminalNode Digits() { return getToken(FlixParser.Digits, 0); }
		public NegativeContext negative() {
			return getRuleContext(NegativeContext.class,0);
		}
		public Int8Context(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_int8; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterInt8(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitInt8(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitInt8(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Int8Context int8() throws RecognitionException {
		Int8Context _localctx = new Int8Context(_ctx, getState());
		enterRule(_localctx, 244, RULE_int8);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1738);
			_la = _input.LA(1);
			if (_la==T__30) {
				{
				setState(1737);
				negative();
				}
			}

			setState(1740);
			match(Digits);
			setState(1741);
			match(T__33);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Int16Context extends ParserRuleContext {
		public TerminalNode Digits() { return getToken(FlixParser.Digits, 0); }
		public NegativeContext negative() {
			return getRuleContext(NegativeContext.class,0);
		}
		public Int16Context(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_int16; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterInt16(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitInt16(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitInt16(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Int16Context int16() throws RecognitionException {
		Int16Context _localctx = new Int16Context(_ctx, getState());
		enterRule(_localctx, 246, RULE_int16);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1744);
			_la = _input.LA(1);
			if (_la==T__30) {
				{
				setState(1743);
				negative();
				}
			}

			setState(1746);
			match(Digits);
			setState(1747);
			match(T__34);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Int32Context extends ParserRuleContext {
		public TerminalNode Digits() { return getToken(FlixParser.Digits, 0); }
		public NegativeContext negative() {
			return getRuleContext(NegativeContext.class,0);
		}
		public Int32Context(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_int32; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterInt32(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitInt32(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitInt32(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Int32Context int32() throws RecognitionException {
		Int32Context _localctx = new Int32Context(_ctx, getState());
		enterRule(_localctx, 248, RULE_int32);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1750);
			_la = _input.LA(1);
			if (_la==T__30) {
				{
				setState(1749);
				negative();
				}
			}

			setState(1752);
			match(Digits);
			setState(1753);
			match(T__35);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Int64Context extends ParserRuleContext {
		public TerminalNode Digits() { return getToken(FlixParser.Digits, 0); }
		public NegativeContext negative() {
			return getRuleContext(NegativeContext.class,0);
		}
		public Int64Context(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_int64; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterInt64(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitInt64(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitInt64(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Int64Context int64() throws RecognitionException {
		Int64Context _localctx = new Int64Context(_ctx, getState());
		enterRule(_localctx, 250, RULE_int64);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1756);
			_la = _input.LA(1);
			if (_la==T__30) {
				{
				setState(1755);
				negative();
				}
			}

			setState(1758);
			match(Digits);
			setState(1759);
			match(T__36);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class BigIntContext extends ParserRuleContext {
		public TerminalNode Digits() { return getToken(FlixParser.Digits, 0); }
		public NegativeContext negative() {
			return getRuleContext(NegativeContext.class,0);
		}
		public BigIntContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_bigInt; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterBigInt(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitBigInt(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitBigInt(this);
			else return visitor.visitChildren(this);
		}
	}

	public final BigIntContext bigInt() throws RecognitionException {
		BigIntContext _localctx = new BigIntContext(_ctx, getState());
		enterRule(_localctx, 252, RULE_bigInt);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1762);
			_la = _input.LA(1);
			if (_la==T__30) {
				{
				setState(1761);
				negative();
				}
			}

			setState(1764);
			match(Digits);
			setState(1765);
			match(T__37);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class IntDefaultContext extends ParserRuleContext {
		public TerminalNode Digits() { return getToken(FlixParser.Digits, 0); }
		public NegativeContext negative() {
			return getRuleContext(NegativeContext.class,0);
		}
		public IntDefaultContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_intDefault; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterIntDefault(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitIntDefault(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitIntDefault(this);
			else return visitor.visitChildren(this);
		}
	}

	public final IntDefaultContext intDefault() throws RecognitionException {
		IntDefaultContext _localctx = new IntDefaultContext(_ctx, getState());
		enterRule(_localctx, 254, RULE_intDefault);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1768);
			_la = _input.LA(1);
			if (_la==T__30) {
				{
				setState(1767);
				negative();
				}
			}

			setState(1770);
			match(Digits);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class IntsContext extends ParserRuleContext {
		public Int8Context int8() {
			return getRuleContext(Int8Context.class,0);
		}
		public Int16Context int16() {
			return getRuleContext(Int16Context.class,0);
		}
		public Int32Context int32() {
			return getRuleContext(Int32Context.class,0);
		}
		public Int64Context int64() {
			return getRuleContext(Int64Context.class,0);
		}
		public BigIntContext bigInt() {
			return getRuleContext(BigIntContext.class,0);
		}
		public IntDefaultContext intDefault() {
			return getRuleContext(IntDefaultContext.class,0);
		}
		public IntsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_ints; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterInts(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitInts(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitInts(this);
			else return visitor.visitChildren(this);
		}
	}

	public final IntsContext ints() throws RecognitionException {
		IntsContext _localctx = new IntsContext(_ctx, getState());
		enterRule(_localctx, 256, RULE_ints);
		try {
			setState(1778);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,291,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1772);
				int8();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1773);
				int16();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1774);
				int32();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(1775);
				int64();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(1776);
				bigInt();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(1777);
				intDefault();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class LiteralContext extends ParserRuleContext {
		public BoolsContext bools() {
			return getRuleContext(BoolsContext.class,0);
		}
		public CharsContext chars() {
			return getRuleContext(CharsContext.class,0);
		}
		public FloatsContext floats() {
			return getRuleContext(FloatsContext.class,0);
		}
		public IntsContext ints() {
			return getRuleContext(IntsContext.class,0);
		}
		public StrsContext strs() {
			return getRuleContext(StrsContext.class,0);
		}
		public LiteralContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_literal; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterLiteral(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitLiteral(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitLiteral(this);
			else return visitor.visitChildren(this);
		}
	}

	public final LiteralContext literal() throws RecognitionException {
		LiteralContext _localctx = new LiteralContext(_ctx, getState());
		enterRule(_localctx, 258, RULE_literal);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1785);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,292,_ctx) ) {
			case 1:
				{
				setState(1780);
				bools();
				}
				break;
			case 2:
				{
				setState(1781);
				chars();
				}
				break;
			case 3:
				{
				setState(1782);
				floats();
				}
				break;
			case 4:
				{
				setState(1783);
				ints();
				}
				break;
			case 5:
				{
				setState(1784);
				strs();
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class PrimaryContext extends ParserRuleContext {
		public ArrowContext arrow() {
			return getRuleContext(ArrowContext.class,0);
		}
		public TupleContext tuple() {
			return getRuleContext(TupleContext.class,0);
		}
		public ApplyContext apply() {
			return getRuleContext(ApplyContext.class,0);
		}
		public VarContext var() {
			return getRuleContext(VarContext.class,0);
		}
		public RefContext ref() {
			return getRuleContext(RefContext.class,0);
		}
		public PrimaryContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_primary; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterPrimary(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitPrimary(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitPrimary(this);
			else return visitor.visitChildren(this);
		}
	}

	public final PrimaryContext primary() throws RecognitionException {
		PrimaryContext _localctx = new PrimaryContext(_ctx, getState());
		enterRule(_localctx, 260, RULE_primary);
		try {
			setState(1792);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,293,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1787);
				arrow();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1788);
				tuple();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1789);
				apply();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(1790);
				var();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(1791);
				ref();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class VarContext extends ParserRuleContext {
		public VariableNameContext variableName() {
			return getRuleContext(VariableNameContext.class,0);
		}
		public VarContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_var; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterVar(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitVar(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitVar(this);
			else return visitor.visitChildren(this);
		}
	}

	public final VarContext var() throws RecognitionException {
		VarContext _localctx = new VarContext(_ctx, getState());
		enterRule(_localctx, 262, RULE_var);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1794);
			variableName();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class RefContext extends ParserRuleContext {
		public QualifiedTypeNameContext qualifiedTypeName() {
			return getRuleContext(QualifiedTypeNameContext.class,0);
		}
		public RefContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_ref; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterRef(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitRef(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitRef(this);
			else return visitor.visitChildren(this);
		}
	}

	public final RefContext ref() throws RecognitionException {
		RefContext _localctx = new RefContext(_ctx, getState());
		enterRule(_localctx, 264, RULE_ref);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1796);
			qualifiedTypeName();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TypeContext extends ParserRuleContext {
		public PrimaryContext primary() {
			return getRuleContext(PrimaryContext.class,0);
		}
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public TypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_type; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterType(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitType(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitType(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TypeContext type() throws RecognitionException {
		TypeContext _localctx = new TypeContext(_ctx, getState());
		enterRule(_localctx, 266, RULE_type);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1798);
			primary();
			setState(1807);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,296,_ctx) ) {
			case 1:
				{
				setState(1800);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(1799);
					ws();
					}
				}

				setState(1802);
				match(T__17);
				setState(1804);
				_la = _input.LA(1);
				if (_la==TripleSlashComment || _la==PWS) {
					{
					setState(1803);
					ws();
					}
				}

				setState(1806);
				type();
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ArrowContext extends ParserRuleContext {
		public List<TypeContext> type() {
			return getRuleContexts(TypeContext.class);
		}
		public TypeContext type(int i) {
			return getRuleContext(TypeContext.class,i);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public ArrowContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_arrow; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterArrow(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitArrow(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitArrow(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ArrowContext arrow() throws RecognitionException {
		ArrowContext _localctx = new ArrowContext(_ctx, getState());
		enterRule(_localctx, 268, RULE_arrow);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1809);
			match(T__4);
			setState(1811);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1810);
				ws();
				}
			}

			setState(1813);
			type();
			setState(1824);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,300,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1815);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(1814);
						ws();
						}
					}

					setState(1817);
					match(T__2);
					setState(1819);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(1818);
						ws();
						}
					}

					setState(1821);
					type();
					}
					} 
				}
				setState(1826);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,300,_ctx);
			}
			setState(1828);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1827);
				ws();
				}
			}

			setState(1830);
			match(T__5);
			setState(1832);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1831);
				ws();
				}
			}

			setState(1834);
			match(T__17);
			setState(1836);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1835);
				ws();
				}
			}

			setState(1838);
			type();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Tuple_unitContext extends ParserRuleContext {
		public Tuple_unitContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_tuple_unit; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterTuple_unit(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitTuple_unit(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitTuple_unit(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Tuple_unitContext tuple_unit() throws RecognitionException {
		Tuple_unitContext _localctx = new Tuple_unitContext(_ctx, getState());
		enterRule(_localctx, 270, RULE_tuple_unit);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1840);
			match(T__4);
			setState(1841);
			match(T__5);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Tuple_singletonContext extends ParserRuleContext {
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public Tuple_singletonContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_tuple_singleton; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterTuple_singleton(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitTuple_singleton(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitTuple_singleton(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Tuple_singletonContext tuple_singleton() throws RecognitionException {
		Tuple_singletonContext _localctx = new Tuple_singletonContext(_ctx, getState());
		enterRule(_localctx, 272, RULE_tuple_singleton);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1843);
			match(T__4);
			setState(1845);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1844);
				ws();
				}
			}

			setState(1847);
			type();
			setState(1849);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1848);
				ws();
				}
			}

			setState(1851);
			match(T__5);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Tuple_multiContext extends ParserRuleContext {
		public List<TypeContext> type() {
			return getRuleContexts(TypeContext.class);
		}
		public TypeContext type(int i) {
			return getRuleContext(TypeContext.class,i);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public Tuple_multiContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_tuple_multi; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterTuple_multi(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitTuple_multi(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitTuple_multi(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Tuple_multiContext tuple_multi() throws RecognitionException {
		Tuple_multiContext _localctx = new Tuple_multiContext(_ctx, getState());
		enterRule(_localctx, 274, RULE_tuple_multi);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1853);
			match(T__4);
			setState(1855);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1854);
				ws();
				}
			}

			setState(1857);
			type();
			setState(1866); 
			_errHandler.sync(this);
			_alt = 1;
			do {
				switch (_alt) {
				case 1:
					{
					{
					setState(1859);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(1858);
						ws();
						}
					}

					setState(1861);
					match(T__2);
					setState(1863);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(1862);
						ws();
						}
					}

					setState(1865);
					type();
					}
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(1868); 
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,309,_ctx);
			} while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER );
			setState(1871);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1870);
				ws();
				}
			}

			setState(1873);
			match(T__5);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TupleContext extends ParserRuleContext {
		public Tuple_unitContext tuple_unit() {
			return getRuleContext(Tuple_unitContext.class,0);
		}
		public Tuple_singletonContext tuple_singleton() {
			return getRuleContext(Tuple_singletonContext.class,0);
		}
		public Tuple_multiContext tuple_multi() {
			return getRuleContext(Tuple_multiContext.class,0);
		}
		public TupleContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_tuple; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterTuple(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitTuple(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitTuple(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TupleContext tuple() throws RecognitionException {
		TupleContext _localctx = new TupleContext(_ctx, getState());
		enterRule(_localctx, 276, RULE_tuple);
		try {
			setState(1878);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,311,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1875);
				tuple_unit();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1876);
				tuple_singleton();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1877);
				tuple_multi();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ApplyContext extends ParserRuleContext {
		public RefContext ref() {
			return getRuleContext(RefContext.class,0);
		}
		public List<TypeContext> type() {
			return getRuleContexts(TypeContext.class);
		}
		public TypeContext type(int i) {
			return getRuleContext(TypeContext.class,i);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public ApplyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_apply; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterApply(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitApply(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitApply(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ApplyContext apply() throws RecognitionException {
		ApplyContext _localctx = new ApplyContext(_ctx, getState());
		enterRule(_localctx, 278, RULE_apply);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1880);
			ref();
			setState(1882);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1881);
				ws();
				}
			}

			setState(1884);
			match(T__9);
			setState(1886);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1885);
				ws();
				}
			}

			setState(1888);
			type();
			setState(1899);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,316,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1890);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(1889);
						ws();
						}
					}

					setState(1892);
					match(T__2);
					setState(1894);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(1893);
						ws();
						}
					}

					setState(1896);
					type();
					}
					} 
				}
				setState(1901);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,316,_ctx);
			}
			setState(1903);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1902);
				ws();
				}
			}

			setState(1905);
			match(T__10);
			setState(1907);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,318,_ctx) ) {
			case 1:
				{
				setState(1906);
				ws();
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Unary_opsContext extends ParserRuleContext {
		public Unary_opsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_unary_ops; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterUnary_ops(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitUnary_ops(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitUnary_ops(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Unary_opsContext unary_ops() throws RecognitionException {
		Unary_opsContext _localctx = new Unary_opsContext(_ctx, getState());
		enterRule(_localctx, 280, RULE_unary_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1909);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__30) | (1L << T__38) | (1L << T__39) | (1L << T__40) | (1L << T__41))) != 0)) ) {
			_errHandler.recoverInline(this);
			} else {
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Logical_opsContext extends ParserRuleContext {
		public Logical_opsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_logical_ops; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterLogical_ops(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitLogical_ops(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitLogical_ops(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Logical_opsContext logical_ops() throws RecognitionException {
		Logical_opsContext _localctx = new Logical_opsContext(_ctx, getState());
		enterRule(_localctx, 282, RULE_logical_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1911);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__42) | (1L << T__43) | (1L << T__44) | (1L << T__45) | (1L << T__46) | (1L << T__47) | (1L << T__48) | (1L << T__49) | (1L << T__50) | (1L << T__51) | (1L << T__52) | (1L << T__53) | (1L << T__54))) != 0)) ) {
			_errHandler.recoverInline(this);
			} else {
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Comparison_opsContext extends ParserRuleContext {
		public Comparison_opsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_comparison_ops; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterComparison_ops(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitComparison_ops(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitComparison_ops(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Comparison_opsContext comparison_ops() throws RecognitionException {
		Comparison_opsContext _localctx = new Comparison_opsContext(_ctx, getState());
		enterRule(_localctx, 284, RULE_comparison_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1913);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__11) | (1L << T__55) | (1L << T__56) | (1L << T__57) | (1L << T__58) | (1L << T__59) | (1L << T__60))) != 0)) ) {
			_errHandler.recoverInline(this);
			} else {
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Multipve_opsContext extends ParserRuleContext {
		public Multipve_opsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_multipve_ops; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterMultipve_ops(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitMultipve_ops(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitMultipve_ops(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Multipve_opsContext multipve_ops() throws RecognitionException {
		Multipve_opsContext _localctx = new Multipve_opsContext(_ctx, getState());
		enterRule(_localctx, 286, RULE_multipve_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1915);
			_la = _input.LA(1);
			if ( !(((((_la - 2)) & ~0x3f) == 0 && ((1L << (_la - 2)) & ((1L << (T__1 - 2)) | (1L << (T__61 - 2)) | (1L << (T__62 - 2)) | (1L << (T__63 - 2)))) != 0)) ) {
			_errHandler.recoverInline(this);
			} else {
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Addve_opsContext extends ParserRuleContext {
		public Addve_opsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_addve_ops; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterAddve_ops(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitAddve_ops(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitAddve_ops(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Addve_opsContext addve_ops() throws RecognitionException {
		Addve_opsContext _localctx = new Addve_opsContext(_ctx, getState());
		enterRule(_localctx, 288, RULE_addve_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1917);
			_la = _input.LA(1);
			if ( !(_la==T__30 || _la==T__38) ) {
			_errHandler.recoverInline(this);
			} else {
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Extbin_opsContext extends ParserRuleContext {
		public Extbin_opsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_extbin_ops; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterExtbin_ops(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitExtbin_ops(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitExtbin_ops(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Extbin_opsContext extbin_ops() throws RecognitionException {
		Extbin_opsContext _localctx = new Extbin_opsContext(_ctx, getState());
		enterRule(_localctx, 290, RULE_extbin_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1919);
			_la = _input.LA(1);
			if ( !(((((_la - 65)) & ~0x3f) == 0 && ((1L << (_la - 65)) & ((1L << (T__64 - 65)) | (1L << (T__65 - 65)) | (1L << (T__66 - 65)) | (1L << (T__67 - 65)) | (1L << (T__68 - 65)))) != 0)) ) {
			_errHandler.recoverInline(this);
			} else {
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class PredicateContext extends ParserRuleContext {
		public Pred_trueContext pred_true() {
			return getRuleContext(Pred_trueContext.class,0);
		}
		public Pred_falseContext pred_false() {
			return getRuleContext(Pred_falseContext.class,0);
		}
		public Pred_filterContext pred_filter() {
			return getRuleContext(Pred_filterContext.class,0);
		}
		public Pred_notequalContext pred_notequal() {
			return getRuleContext(Pred_notequalContext.class,0);
		}
		public Pred_tableContext pred_table() {
			return getRuleContext(Pred_tableContext.class,0);
		}
		public Pred_loopContext pred_loop() {
			return getRuleContext(Pred_loopContext.class,0);
		}
		public PredicateContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_predicate; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterPredicate(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitPredicate(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitPredicate(this);
			else return visitor.visitChildren(this);
		}
	}

	public final PredicateContext predicate() throws RecognitionException {
		PredicateContext _localctx = new PredicateContext(_ctx, getState());
		enterRule(_localctx, 292, RULE_predicate);
		try {
			setState(1927);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,319,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1921);
				pred_true();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1922);
				pred_false();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1923);
				pred_filter();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(1924);
				pred_notequal();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(1925);
				pred_table();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(1926);
				pred_loop();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class PredicatesContext extends ParserRuleContext {
		public List<PredicateContext> predicate() {
			return getRuleContexts(PredicateContext.class);
		}
		public PredicateContext predicate(int i) {
			return getRuleContext(PredicateContext.class,i);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public PredicatesContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_predicates; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterPredicates(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitPredicates(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitPredicates(this);
			else return visitor.visitChildren(this);
		}
	}

	public final PredicatesContext predicates() throws RecognitionException {
		PredicatesContext _localctx = new PredicatesContext(_ctx, getState());
		enterRule(_localctx, 294, RULE_predicates);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1929);
			predicate();
			setState(1940);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,322,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1931);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(1930);
						ws();
						}
					}

					setState(1933);
					match(T__2);
					setState(1935);
					_la = _input.LA(1);
					if (_la==TripleSlashComment || _la==PWS) {
						{
						setState(1934);
						ws();
						}
					}

					setState(1937);
					predicate();
					}
					} 
				}
				setState(1942);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,322,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Pred_trueContext extends ParserRuleContext {
		public Pred_trueContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_pred_true; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterPred_true(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitPred_true(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitPred_true(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Pred_trueContext pred_true() throws RecognitionException {
		Pred_trueContext _localctx = new Pred_trueContext(_ctx, getState());
		enterRule(_localctx, 296, RULE_pred_true);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1943);
			match(T__28);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Pred_falseContext extends ParserRuleContext {
		public Pred_falseContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_pred_false; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterPred_false(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitPred_false(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitPred_false(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Pred_falseContext pred_false() throws RecognitionException {
		Pred_falseContext _localctx = new Pred_falseContext(_ctx, getState());
		enterRule(_localctx, 298, RULE_pred_false);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1945);
			match(T__29);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Pred_filterContext extends ParserRuleContext {
		public QualifiedDefinitionNameContext qualifiedDefinitionName() {
			return getRuleContext(QualifiedDefinitionNameContext.class,0);
		}
		public ExpressionsContext expressions() {
			return getRuleContext(ExpressionsContext.class,0);
		}
		public WsContext ws() {
			return getRuleContext(WsContext.class,0);
		}
		public Pred_filterContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_pred_filter; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterPred_filter(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitPred_filter(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitPred_filter(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Pred_filterContext pred_filter() throws RecognitionException {
		Pred_filterContext _localctx = new Pred_filterContext(_ctx, getState());
		enterRule(_localctx, 300, RULE_pred_filter);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1947);
			qualifiedDefinitionName();
			setState(1949);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1948);
				ws();
				}
			}

			setState(1951);
			match(T__4);
			setState(1952);
			expressions();
			setState(1953);
			match(T__5);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Pred_tableContext extends ParserRuleContext {
		public QualifiedTableNameContext qualifiedTableName() {
			return getRuleContext(QualifiedTableNameContext.class,0);
		}
		public ExpressionsContext expressions() {
			return getRuleContext(ExpressionsContext.class,0);
		}
		public WsContext ws() {
			return getRuleContext(WsContext.class,0);
		}
		public Pred_tableContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_pred_table; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterPred_table(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitPred_table(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitPred_table(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Pred_tableContext pred_table() throws RecognitionException {
		Pred_tableContext _localctx = new Pred_tableContext(_ctx, getState());
		enterRule(_localctx, 302, RULE_pred_table);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1955);
			qualifiedTableName();
			setState(1957);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1956);
				ws();
				}
			}

			setState(1959);
			match(T__4);
			setState(1960);
			expressions();
			setState(1961);
			match(T__5);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Pred_notequalContext extends ParserRuleContext {
		public List<VariableNameContext> variableName() {
			return getRuleContexts(VariableNameContext.class);
		}
		public VariableNameContext variableName(int i) {
			return getRuleContext(VariableNameContext.class,i);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public Pred_notequalContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_pred_notequal; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterPred_notequal(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitPred_notequal(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitPred_notequal(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Pred_notequalContext pred_notequal() throws RecognitionException {
		Pred_notequalContext _localctx = new Pred_notequalContext(_ctx, getState());
		enterRule(_localctx, 304, RULE_pred_notequal);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1963);
			variableName();
			setState(1965);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1964);
				ws();
				}
			}

			setState(1967);
			match(T__59);
			setState(1969);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1968);
				ws();
				}
			}

			setState(1971);
			variableName();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Pred_loopContext extends ParserRuleContext {
		public VariableNameContext variableName() {
			return getRuleContext(VariableNameContext.class,0);
		}
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public List<WsContext> ws() {
			return getRuleContexts(WsContext.class);
		}
		public WsContext ws(int i) {
			return getRuleContext(WsContext.class,i);
		}
		public Pred_loopContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_pred_loop; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterPred_loop(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitPred_loop(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitPred_loop(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Pred_loopContext pred_loop() throws RecognitionException {
		Pred_loopContext _localctx = new Pred_loopContext(_ctx, getState());
		enterRule(_localctx, 306, RULE_pred_loop);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1973);
			variableName();
			setState(1975);
			_la = _input.LA(1);
			if (_la==TripleSlashComment || _la==PWS) {
				{
				setState(1974);
				ws();
				}
			}

			setState(1977);
			match(T__69);
			setState(1979);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,328,_ctx) ) {
			case 1:
				{
				setState(1978);
				ws();
				}
				break;
			}
			setState(1981);
			expression();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public boolean sempred(RuleContext _localctx, int ruleIndex, int predIndex) {
		switch (ruleIndex) {
		case 69:
			return additive_sempred((AdditiveContext)_localctx, predIndex);
		case 70:
			return multiplicative_sempred((MultiplicativeContext)_localctx, predIndex);
		case 73:
			return unary_sempred((UnaryContext)_localctx, predIndex);
		}
		return true;
	}
	private boolean additive_sempred(AdditiveContext _localctx, int predIndex) {
		switch (predIndex) {
		case 0:
			return precpred(_ctx, 2);
		}
		return true;
	}
	private boolean multiplicative_sempred(MultiplicativeContext _localctx, int predIndex) {
		switch (predIndex) {
		case 1:
			return precpred(_ctx, 2);
		}
		return true;
	}
	private boolean unary_sempred(UnaryContext _localctx, int predIndex) {
		switch (predIndex) {
		case 2:
			return !( _input.LT(1).getText().equals("-") && //Make sure this isn't just a negative number
				Character.isDigit(_input.LT(2).getText().charAt(0)) );
		}
		return true;
	}

	public static final String _serializedATN =
		"\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd\3e\u07c2\4\2\t\2\4"+
		"\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t"+
		"\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31\t\31"+
		"\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\4\36\t\36\4\37\t\37\4 \t \4!"+
		"\t!\4\"\t\"\4#\t#\4$\t$\4%\t%\4&\t&\4\'\t\'\4(\t(\4)\t)\4*\t*\4+\t+\4"+
		",\t,\4-\t-\4.\t.\4/\t/\4\60\t\60\4\61\t\61\4\62\t\62\4\63\t\63\4\64\t"+
		"\64\4\65\t\65\4\66\t\66\4\67\t\67\48\t8\49\t9\4:\t:\4;\t;\4<\t<\4=\t="+
		"\4>\t>\4?\t?\4@\t@\4A\tA\4B\tB\4C\tC\4D\tD\4E\tE\4F\tF\4G\tG\4H\tH\4I"+
		"\tI\4J\tJ\4K\tK\4L\tL\4M\tM\4N\tN\4O\tO\4P\tP\4Q\tQ\4R\tR\4S\tS\4T\tT"+
		"\4U\tU\4V\tV\4W\tW\4X\tX\4Y\tY\4Z\tZ\4[\t[\4\\\t\\\4]\t]\4^\t^\4_\t_\4"+
		"`\t`\4a\ta\4b\tb\4c\tc\4d\td\4e\te\4f\tf\4g\tg\4h\th\4i\ti\4j\tj\4k\t"+
		"k\4l\tl\4m\tm\4n\tn\4o\to\4p\tp\4q\tq\4r\tr\4s\ts\4t\tt\4u\tu\4v\tv\4"+
		"w\tw\4x\tx\4y\ty\4z\tz\4{\t{\4|\t|\4}\t}\4~\t~\4\177\t\177\4\u0080\t\u0080"+
		"\4\u0081\t\u0081\4\u0082\t\u0082\4\u0083\t\u0083\4\u0084\t\u0084\4\u0085"+
		"\t\u0085\4\u0086\t\u0086\4\u0087\t\u0087\4\u0088\t\u0088\4\u0089\t\u0089"+
		"\4\u008a\t\u008a\4\u008b\t\u008b\4\u008c\t\u008c\4\u008d\t\u008d\4\u008e"+
		"\t\u008e\4\u008f\t\u008f\4\u0090\t\u0090\4\u0091\t\u0091\4\u0092\t\u0092"+
		"\4\u0093\t\u0093\4\u0094\t\u0094\4\u0095\t\u0095\4\u0096\t\u0096\4\u0097"+
		"\t\u0097\4\u0098\t\u0098\4\u0099\t\u0099\4\u009a\t\u009a\4\u009b\t\u009b"+
		"\3\2\3\2\3\3\6\3\u013a\n\3\r\3\16\3\u013b\3\4\7\4\u013f\n\4\f\4\16\4\u0142"+
		"\13\4\3\4\7\4\u0145\n\4\f\4\16\4\u0148\13\4\3\4\5\4\u014b\n\4\3\4\3\4"+
		"\3\5\5\5\u0150\n\5\3\5\5\5\u0153\n\5\3\6\3\6\3\7\3\7\3\7\7\7\u015a\n\7"+
		"\f\7\16\7\u015d\13\7\3\b\3\b\3\b\5\b\u0162\n\b\3\b\3\b\3\t\3\t\3\t\5\t"+
		"\u0169\n\t\3\t\3\t\3\n\3\n\3\13\3\13\3\f\3\f\3\r\3\r\3\16\3\16\3\17\3"+
		"\17\3\20\3\20\3\21\3\21\3\22\3\22\3\23\3\23\3\24\3\24\3\25\3\25\5\25\u0185"+
		"\n\25\3\25\3\25\5\25\u0189\n\25\3\25\7\25\u018c\n\25\f\25\16\25\u018f"+
		"\13\25\3\26\3\26\3\26\5\26\u0194\n\26\3\26\3\26\3\27\3\27\5\27\u019a\n"+
		"\27\3\27\3\27\5\27\u019e\n\27\3\27\7\27\u01a1\n\27\f\27\16\27\u01a4\13"+
		"\27\3\30\3\30\5\30\u01a8\n\30\3\30\5\30\u01ab\n\30\3\30\5\30\u01ae\n\30"+
		"\3\30\5\30\u01b1\n\30\3\31\3\31\5\31\u01b5\n\31\3\31\3\31\5\31\u01b9\n"+
		"\31\3\31\3\31\3\32\3\32\5\32\u01bf\n\32\3\32\3\32\5\32\u01c3\n\32\3\32"+
		"\7\32\u01c6\n\32\f\32\16\32\u01c9\13\32\3\33\3\33\5\33\u01cd\n\33\3\33"+
		"\3\33\5\33\u01d1\n\33\3\33\3\33\5\33\u01d5\n\33\3\33\7\33\u01d8\n\33\f"+
		"\33\16\33\u01db\13\33\5\33\u01dd\n\33\3\33\5\33\u01e0\n\33\3\33\3\33\3"+
		"\34\3\34\5\34\u01e6\n\34\3\34\3\34\5\34\u01ea\n\34\3\34\7\34\u01ed\n\34"+
		"\f\34\16\34\u01f0\13\34\3\35\3\35\3\35\3\35\5\35\u01f6\n\35\3\35\3\35"+
		"\5\35\u01fa\n\35\3\35\3\35\5\35\u01fe\n\35\3\36\3\36\5\36\u0202\n\36\3"+
		"\36\7\36\u0205\n\36\f\36\16\36\u0208\13\36\3\37\3\37\3\37\3\37\5\37\u020e"+
		"\n\37\3\37\3\37\5\37\u0212\n\37\3\37\3\37\5\37\u0216\n\37\3 \3 \5 \u021a"+
		"\n \3 \7 \u021d\n \f \16 \u0220\13 \3!\3!\5!\u0224\n!\3!\3!\5!\u0228\n"+
		"!\3!\5!\u022b\n!\3\"\3\"\5\"\u022f\n\"\3\"\3\"\5\"\u0233\n\"\3\"\3\"\5"+
		"\"\u0237\n\"\3\"\7\"\u023a\n\"\f\"\16\"\u023d\13\"\3\"\3\"\5\"\u0241\n"+
		"\"\3#\3#\3#\5#\u0246\n#\3#\3#\5#\u024a\n#\3#\7#\u024d\n#\f#\16#\u0250"+
		"\13#\3#\3#\3$\3$\3$\3%\3%\5%\u0259\n%\3%\3%\5%\u025d\n%\3%\7%\u0260\n"+
		"%\f%\16%\u0263\13%\3&\5&\u0266\n&\3&\3&\5&\u026a\n&\3&\5&\u026d\n&\3\'"+
		"\5\'\u0270\n\'\3\'\3\'\5\'\u0274\n\'\3\'\5\'\u0277\n\'\3(\3(\3(\3)\3)"+
		"\3)\3)\7)\u0280\n)\f)\16)\u0283\13)\3*\5*\u0286\n*\3*\3*\3*\5*\u028b\n"+
		"*\3+\3+\3+\3+\3+\3+\3+\3,\3,\3,\3,\3,\3,\3,\3-\3-\3-\3-\3-\3.\3.\3.\3"+
		".\3.\3.\3.\3.\3.\3.\3.\3.\3.\3.\5.\u02ae\n.\3/\5/\u02b1\n/\3/\3/\3/\3"+
		"/\5/\u02b7\n/\3/\3/\5/\u02bb\n/\3/\7/\u02be\n/\f/\16/\u02c1\13/\3/\5/"+
		"\u02c4\n/\3/\3/\3/\3\60\5\60\u02ca\n\60\3\60\7\60\u02cd\n\60\f\60\16\60"+
		"\u02d0\13\60\3\60\5\60\u02d3\n\60\3\60\3\60\3\60\3\60\3\60\5\60\u02da"+
		"\n\60\3\60\3\60\5\60\u02de\n\60\3\60\3\60\5\60\u02e2\n\60\3\60\3\60\3"+
		"\60\3\61\3\61\5\61\u02e9\n\61\3\61\3\61\5\61\u02ed\n\61\3\61\7\61\u02f0"+
		"\n\61\f\61\16\61\u02f3\13\61\3\62\3\62\3\62\3\62\5\62\u02f9\n\62\3\63"+
		"\5\63\u02fc\n\63\3\63\7\63\u02ff\n\63\f\63\16\63\u0302\13\63\3\63\5\63"+
		"\u0305\n\63\3\63\3\63\3\63\3\63\5\63\u030b\n\63\3\63\3\63\5\63\u030f\n"+
		"\63\3\63\5\63\u0312\n\63\3\63\5\63\u0315\n\63\3\63\3\63\3\63\3\64\5\64"+
		"\u031b\n\64\3\64\7\64\u031e\n\64\f\64\16\64\u0321\13\64\3\64\5\64\u0324"+
		"\n\64\3\64\3\64\3\64\3\64\5\64\u032a\n\64\3\64\3\64\5\64\u032e\n\64\3"+
		"\64\5\64\u0331\n\64\3\64\5\64\u0334\n\64\3\64\3\64\3\64\3\65\5\65\u033a"+
		"\n\65\3\65\3\65\3\65\3\65\5\65\u0340\n\65\3\65\3\65\5\65\u0344\n\65\3"+
		"\65\5\65\u0347\n\65\3\65\5\65\u034a\n\65\3\65\3\65\3\65\3\66\5\66\u0350"+
		"\n\66\3\66\7\66\u0353\n\66\f\66\16\66\u0356\13\66\3\66\5\66\u0359\n\66"+
		"\3\66\3\66\3\66\3\66\5\66\u035f\n\66\3\66\3\66\5\66\u0363\n\66\3\66\3"+
		"\66\5\66\u0367\n\66\3\66\3\66\3\66\3\67\5\67\u036d\n\67\3\67\7\67\u0370"+
		"\n\67\f\67\16\67\u0373\13\67\3\67\5\67\u0376\n\67\3\67\3\67\5\67\u037a"+
		"\n\67\3\67\3\67\3\67\3\67\5\67\u0380\n\67\3\67\3\67\5\67\u0384\n\67\3"+
		"\67\3\67\5\67\u0388\n\67\3\67\3\67\3\67\38\58\u038e\n8\38\78\u0391\n8"+
		"\f8\168\u0394\138\38\58\u0397\n8\38\58\u039a\n8\38\58\u039d\n8\38\38\3"+
		"8\38\58\u03a3\n8\38\38\38\58\u03a8\n8\38\38\58\u03ac\n8\38\38\58\u03b0"+
		"\n8\38\38\58\u03b4\n8\38\38\38\39\59\u03ba\n9\39\79\u03bd\n9\f9\169\u03c0"+
		"\139\39\59\u03c3\n9\39\39\39\39\59\u03c9\n9\39\39\59\u03cd\n9\39\39\5"+
		"9\u03d1\n9\39\39\59\u03d5\n9\39\39\59\u03d9\n9\39\39\59\u03dd\n9\39\3"+
		"9\39\3:\5:\u03e3\n:\3:\7:\u03e6\n:\f:\16:\u03e9\13:\3:\5:\u03ec\n:\3:"+
		"\3:\3:\3:\3:\3:\5:\u03f4\n:\3:\3:\3;\3;\5;\u03fa\n;\3;\3;\5;\u03fe\n;"+
		"\7;\u0400\n;\f;\16;\u0403\13;\3;\3;\3<\3<\3<\5<\u040a\n<\3=\5=\u040d\n"+
		"=\3=\3=\5=\u0411\n=\3=\3=\3>\5>\u0416\n>\3>\3>\5>\u041a\n>\3>\3>\5>\u041e"+
		"\n>\3>\3>\5>\u0422\n>\3>\3>\3?\3?\3@\5@\u0429\n@\3@\3@\5@\u042d\n@\3@"+
		"\3@\3@\5@\u0432\n@\3@\3@\5@\u0436\n@\3@\3@\5@\u043a\n@\3@\3@\5@\u043e"+
		"\n@\3@\3@\3@\3A\5A\u0444\nA\3A\7A\u0447\nA\fA\16A\u044a\13A\3A\5A\u044d"+
		"\nA\3A\3A\3A\3A\3A\5A\u0454\nA\3A\3A\5A\u0458\nA\3A\3A\3B\3B\5B\u045e"+
		"\nB\3B\7B\u0461\nB\fB\16B\u0464\13B\3B\5B\u0467\nB\3B\3B\3C\3C\5C\u046d"+
		"\nC\3C\3C\5C\u0471\nC\3C\3C\5C\u0475\nC\3C\5C\u0478\nC\3D\3D\5D\u047c"+
		"\nD\3D\3D\5D\u0480\nD\3D\3D\5D\u0484\nD\3E\3E\5E\u0488\nE\3E\3E\5E\u048c"+
		"\nE\3E\7E\u048f\nE\fE\16E\u0492\13E\3F\3F\5F\u0496\nF\3F\3F\5F\u049a\n"+
		"F\3F\3F\5F\u049e\nF\3G\3G\3G\3G\3G\5G\u04a5\nG\3G\3G\5G\u04a9\nG\3G\3"+
		"G\7G\u04ad\nG\fG\16G\u04b0\13G\3H\3H\3H\3H\3H\5H\u04b7\nH\3H\3H\5H\u04bb"+
		"\nH\3H\3H\7H\u04bf\nH\fH\16H\u04c2\13H\3I\3I\5I\u04c6\nI\3I\3I\3I\3I\5"+
		"I\u04cc\nI\3I\3I\5I\u04d0\nI\3J\3J\5J\u04d4\nJ\3J\3J\5J\u04d8\nJ\3J\3"+
		"J\5J\u04dc\nJ\3K\3K\3K\5K\u04e1\nK\3K\3K\3K\5K\u04e6\nK\3L\3L\5L\u04ea"+
		"\nL\3L\3L\5L\u04ee\nL\3L\5L\u04f1\nL\3M\3M\3M\3M\3M\3M\3M\3M\3M\3M\3M"+
		"\3M\3M\3M\3M\3M\3M\3M\3M\5M\u0506\nM\3N\3N\3N\3N\5N\u050c\nN\3N\3N\5N"+
		"\u0510\nN\3N\3N\5N\u0514\nN\3N\3N\5N\u0518\nN\3N\3N\3O\3O\5O\u051e\nO"+
		"\3O\3O\5O\u0522\nO\3O\3O\5O\u0526\nO\3O\3O\5O\u052a\nO\3O\3O\3O\3O\3O"+
		"\3O\3P\3P\3P\3P\3P\3P\3P\3P\5P\u053a\nP\3P\3P\5P\u053e\nP\3P\3P\3Q\3Q"+
		"\3Q\3Q\5Q\u0546\nQ\3Q\3Q\5Q\u054a\nQ\3Q\3Q\3R\3R\5R\u0550\nR\3R\3R\5R"+
		"\u0554\nR\3R\5R\u0557\nR\3R\5R\u055a\nR\3R\5R\u055d\nR\3S\3S\5S\u0561"+
		"\nS\3S\3S\5S\u0565\nS\3S\3S\3T\3T\5T\u056b\nT\3T\3T\5T\u056f\nT\3T\3T"+
		"\5T\u0573\nT\3T\3T\5T\u0577\nT\3T\3T\3U\3U\3V\3V\3W\3W\3X\3X\3X\5X\u0584"+
		"\nX\3X\3X\5X\u0588\nX\3X\5X\u058b\nX\3Y\3Y\5Y\u058f\nY\3Y\5Y\u0592\nY"+
		"\3Y\5Y\u0595\nY\3Y\3Y\3Z\3Z\5Z\u059b\nZ\3Z\3Z\5Z\u059f\nZ\3Z\3Z\3[\3["+
		"\5[\u05a5\n[\3[\3[\5[\u05a9\n[\3[\7[\u05ac\n[\f[\16[\u05af\13[\3\\\3\\"+
		"\3]\3]\3^\3^\3_\3_\5_\u05b9\n_\3_\3_\5_\u05bd\n_\3_\5_\u05c0\n_\3`\3`"+
		"\5`\u05c4\n`\3`\5`\u05c7\n`\3`\5`\u05ca\n`\3`\3`\3a\3a\5a\u05d0\na\3a"+
		"\5a\u05d3\na\3a\5a\u05d6\na\3a\3a\3b\3b\5b\u05dc\nb\3b\5b\u05df\nb\3b"+
		"\5b\u05e2\nb\3b\3b\3c\3c\5c\u05e8\nc\3c\3c\5c\u05ec\nc\3c\3c\5c\u05f0"+
		"\nc\3c\3c\3d\3d\5d\u05f6\nd\3d\3d\5d\u05fa\nd\3d\3d\5d\u05fe\nd\3d\3d"+
		"\3e\3e\3f\3f\5f\u0606\nf\3f\3f\5f\u060a\nf\3f\7f\u060d\nf\ff\16f\u0610"+
		"\13f\3g\3g\3g\3g\3g\3g\3g\3g\3g\5g\u061b\ng\3h\3h\5h\u061f\nh\3h\3h\5"+
		"h\u0623\nh\3h\3h\3i\3i\5i\u0629\ni\3i\3i\5i\u062d\ni\3i\7i\u0630\ni\f"+
		"i\16i\u0633\13i\3j\3j\3k\3k\3k\5k\u063a\nk\3k\3k\5k\u063e\nk\3k\5k\u0641"+
		"\nk\3l\3l\5l\u0645\nl\3l\5l\u0648\nl\3l\5l\u064b\nl\3l\3l\3m\3m\3n\3n"+
		"\3o\3o\3p\3p\5p\u0657\np\3p\3p\5p\u065b\np\3p\5p\u065e\np\3q\3q\5q\u0662"+
		"\nq\3q\5q\u0665\nq\3q\5q\u0668\nq\3q\3q\5q\u066c\nq\3q\3q\3q\5q\u0671"+
		"\nq\3q\5q\u0674\nq\3q\3q\3r\3r\5r\u067a\nr\3r\5r\u067d\nr\3r\5r\u0680"+
		"\nr\3r\3r\5r\u0684\nr\3r\3r\3r\5r\u0689\nr\3r\5r\u068c\nr\3r\3r\3s\3s"+
		"\5s\u0692\ns\3s\5s\u0695\ns\3s\5s\u0698\ns\3s\3s\5s\u069c\ns\3s\3s\3s"+
		"\5s\u06a1\ns\3s\5s\u06a4\ns\3s\3s\3t\3t\3u\3u\3v\3v\3w\3w\3x\5x\u06b1"+
		"\nx\3x\3x\3x\3x\3x\3y\5y\u06b9\ny\3y\3y\3y\3y\3y\3z\5z\u06c1\nz\3z\3z"+
		"\3z\3z\3{\3{\3{\5{\u06ca\n{\3|\5|\u06cd\n|\3|\3|\3|\3}\5}\u06d3\n}\3}"+
		"\3}\3}\3~\5~\u06d9\n~\3~\3~\3~\3\177\5\177\u06df\n\177\3\177\3\177\3\177"+
		"\3\u0080\5\u0080\u06e5\n\u0080\3\u0080\3\u0080\3\u0080\3\u0081\5\u0081"+
		"\u06eb\n\u0081\3\u0081\3\u0081\3\u0082\3\u0082\3\u0082\3\u0082\3\u0082"+
		"\3\u0082\5\u0082\u06f5\n\u0082\3\u0083\3\u0083\3\u0083\3\u0083\3\u0083"+
		"\5\u0083\u06fc\n\u0083\3\u0084\3\u0084\3\u0084\3\u0084\3\u0084\5\u0084"+
		"\u0703\n\u0084\3\u0085\3\u0085\3\u0086\3\u0086\3\u0087\3\u0087\5\u0087"+
		"\u070b\n\u0087\3\u0087\3\u0087\5\u0087\u070f\n\u0087\3\u0087\5\u0087\u0712"+
		"\n\u0087\3\u0088\3\u0088\5\u0088\u0716\n\u0088\3\u0088\3\u0088\5\u0088"+
		"\u071a\n\u0088\3\u0088\3\u0088\5\u0088\u071e\n\u0088\3\u0088\7\u0088\u0721"+
		"\n\u0088\f\u0088\16\u0088\u0724\13\u0088\3\u0088\5\u0088\u0727\n\u0088"+
		"\3\u0088\3\u0088\5\u0088\u072b\n\u0088\3\u0088\3\u0088\5\u0088\u072f\n"+
		"\u0088\3\u0088\3\u0088\3\u0089\3\u0089\3\u0089\3\u008a\3\u008a\5\u008a"+
		"\u0738\n\u008a\3\u008a\3\u008a\5\u008a\u073c\n\u008a\3\u008a\3\u008a\3"+
		"\u008b\3\u008b\5\u008b\u0742\n\u008b\3\u008b\3\u008b\5\u008b\u0746\n\u008b"+
		"\3\u008b\3\u008b\5\u008b\u074a\n\u008b\3\u008b\6\u008b\u074d\n\u008b\r"+
		"\u008b\16\u008b\u074e\3\u008b\5\u008b\u0752\n\u008b\3\u008b\3\u008b\3"+
		"\u008c\3\u008c\3\u008c\5\u008c\u0759\n\u008c\3\u008d\3\u008d\5\u008d\u075d"+
		"\n\u008d\3\u008d\3\u008d\5\u008d\u0761\n\u008d\3\u008d\3\u008d\5\u008d"+
		"\u0765\n\u008d\3\u008d\3\u008d\5\u008d\u0769\n\u008d\3\u008d\7\u008d\u076c"+
		"\n\u008d\f\u008d\16\u008d\u076f\13\u008d\3\u008d\5\u008d\u0772\n\u008d"+
		"\3\u008d\3\u008d\5\u008d\u0776\n\u008d\3\u008e\3\u008e\3\u008f\3\u008f"+
		"\3\u0090\3\u0090\3\u0091\3\u0091\3\u0092\3\u0092\3\u0093\3\u0093\3\u0094"+
		"\3\u0094\3\u0094\3\u0094\3\u0094\3\u0094\5\u0094\u078a\n\u0094\3\u0095"+
		"\3\u0095\5\u0095\u078e\n\u0095\3\u0095\3\u0095\5\u0095\u0792\n\u0095\3"+
		"\u0095\7\u0095\u0795\n\u0095\f\u0095\16\u0095\u0798\13\u0095\3\u0096\3"+
		"\u0096\3\u0097\3\u0097\3\u0098\3\u0098\5\u0098\u07a0\n\u0098\3\u0098\3"+
		"\u0098\3\u0098\3\u0098\3\u0099\3\u0099\5\u0099\u07a8\n\u0099\3\u0099\3"+
		"\u0099\3\u0099\3\u0099\3\u009a\3\u009a\5\u009a\u07b0\n\u009a\3\u009a\3"+
		"\u009a\5\u009a\u07b4\n\u009a\3\u009a\3\u009a\3\u009b\3\u009b\5\u009b\u07ba"+
		"\n\u009b\3\u009b\3\u009b\5\u009b\u07be\n\u009b\3\u009b\3\u009b\3\u009b"+
		"\2\4\u008c\u008e\u009c\2\4\6\b\n\f\16\20\22\24\26\30\32\34\36 \"$&(*,"+
		".\60\62\64\668:<>@BDFHJLNPRTVXZ\\^`bdfhjlnprtvxz|~\u0080\u0082\u0084\u0086"+
		"\u0088\u008a\u008c\u008e\u0090\u0092\u0094\u0096\u0098\u009a\u009c\u009e"+
		"\u00a0\u00a2\u00a4\u00a6\u00a8\u00aa\u00ac\u00ae\u00b0\u00b2\u00b4\u00b6"+
		"\u00b8\u00ba\u00bc\u00be\u00c0\u00c2\u00c4\u00c6\u00c8\u00ca\u00cc\u00ce"+
		"\u00d0\u00d2\u00d4\u00d6\u00d8\u00da\u00dc\u00de\u00e0\u00e2\u00e4\u00e6"+
		"\u00e8\u00ea\u00ec\u00ee\u00f0\u00f2\u00f4\u00f6\u00f8\u00fa\u00fc\u00fe"+
		"\u0100\u0102\u0104\u0106\u0108\u010a\u010c\u010e\u0110\u0112\u0114\u0116"+
		"\u0118\u011a\u011c\u011e\u0120\u0122\u0124\u0126\u0128\u012a\u012c\u012e"+
		"\u0130\u0132\u0134\2\16\3\2IJ\3\2ab\4\2XXbb\3\2\32\33\3\2\34\35\3\2\37"+
		" \4\2!!),\3\2-9\4\2\16\16:?\4\2\4\4@B\4\2!!))\3\2CG\u08a6\2\u0136\3\2"+
		"\2\2\4\u0139\3\2\2\2\6\u0140\3\2\2\2\b\u0152\3\2\2\2\n\u0154\3\2\2\2\f"+
		"\u0156\3\2\2\2\16\u0161\3\2\2\2\20\u0168\3\2\2\2\22\u016c\3\2\2\2\24\u016e"+
		"\3\2\2\2\26\u0170\3\2\2\2\30\u0172\3\2\2\2\32\u0174\3\2\2\2\34\u0176\3"+
		"\2\2\2\36\u0178\3\2\2\2 \u017a\3\2\2\2\"\u017c\3\2\2\2$\u017e\3\2\2\2"+
		"&\u0180\3\2\2\2(\u0182\3\2\2\2*\u0190\3\2\2\2,\u0197\3\2\2\2.\u01b0\3"+
		"\2\2\2\60\u01b2\3\2\2\2\62\u01bc\3\2\2\2\64\u01ca\3\2\2\2\66\u01e3\3\2"+
		"\2\28\u01f1\3\2\2\2:\u01ff\3\2\2\2<\u0209\3\2\2\2>\u0217\3\2\2\2@\u0221"+
		"\3\2\2\2B\u0240\3\2\2\2D\u0242\3\2\2\2F\u0253\3\2\2\2H\u0256\3\2\2\2J"+
		"\u026c\3\2\2\2L\u0276\3\2\2\2N\u0278\3\2\2\2P\u027b\3\2\2\2R\u0285\3\2"+
		"\2\2T\u028c\3\2\2\2V\u0293\3\2\2\2X\u029a\3\2\2\2Z\u02ad\3\2\2\2\\\u02b0"+
		"\3\2\2\2^\u02ce\3\2\2\2`\u02e6\3\2\2\2b\u02f4\3\2\2\2d\u0300\3\2\2\2f"+
		"\u031f\3\2\2\2h\u0339\3\2\2\2j\u0354\3\2\2\2l\u0371\3\2\2\2n\u0392\3\2"+
		"\2\2p\u03be\3\2\2\2r\u03e7\3\2\2\2t\u03f7\3\2\2\2v\u0409\3\2\2\2x\u040c"+
		"\3\2\2\2z\u0415\3\2\2\2|\u0425\3\2\2\2~\u0428\3\2\2\2\u0080\u0448\3\2"+
		"\2\2\u0082\u045b\3\2\2\2\u0084\u0477\3\2\2\2\u0086\u0479\3\2\2\2\u0088"+
		"\u0485\3\2\2\2\u008a\u0493\3\2\2\2\u008c\u049f\3\2\2\2\u008e\u04b1\3\2"+
		"\2\2\u0090\u04c3\3\2\2\2\u0092\u04d1\3\2\2\2\u0094\u04e5\3\2\2\2\u0096"+
		"\u04e7\3\2\2\2\u0098\u0505\3\2\2\2\u009a\u0507\3\2\2\2\u009c\u051b\3\2"+
		"\2\2\u009e\u0531\3\2\2\2\u00a0\u0541\3\2\2\2\u00a2\u054d\3\2\2\2\u00a4"+
		"\u055e\3\2\2\2\u00a6\u0568\3\2\2\2\u00a8\u057a\3\2\2\2\u00aa\u057c\3\2"+
		"\2\2\u00ac\u057e\3\2\2\2\u00ae\u0583\3\2\2\2\u00b0\u058c\3\2\2\2\u00b2"+
		"\u0598\3\2\2\2\u00b4\u05a2\3\2\2\2\u00b6\u05b0\3\2\2\2\u00b8\u05b2\3\2"+
		"\2\2\u00ba\u05b4\3\2\2\2\u00bc\u05b6\3\2\2\2\u00be\u05c1\3\2\2\2\u00c0"+
		"\u05cd\3\2\2\2\u00c2\u05d9\3\2\2\2\u00c4\u05e5\3\2\2\2\u00c6\u05f3\3\2"+
		"\2\2\u00c8\u0601\3\2\2\2\u00ca\u0603\3\2\2\2\u00cc\u061a\3\2\2\2\u00ce"+
		"\u061c\3\2\2\2\u00d0\u0626\3\2\2\2\u00d2\u0634\3\2\2\2\u00d4\u0639\3\2"+
		"\2\2\u00d6\u0642\3\2\2\2\u00d8\u064e\3\2\2\2\u00da\u0650\3\2\2\2\u00dc"+
		"\u0652\3\2\2\2\u00de\u0654\3\2\2\2\u00e0\u065f\3\2\2\2\u00e2\u0677\3\2"+
		"\2\2\u00e4\u068f\3\2\2\2\u00e6\u06a7\3\2\2\2\u00e8\u06a9\3\2\2\2\u00ea"+
		"\u06ab\3\2\2\2\u00ec\u06ad\3\2\2\2\u00ee\u06b0\3\2\2\2\u00f0\u06b8\3\2"+
		"\2\2\u00f2\u06c0\3\2\2\2\u00f4\u06c9\3\2\2\2\u00f6\u06cc\3\2\2\2\u00f8"+
		"\u06d2\3\2\2\2\u00fa\u06d8\3\2\2\2\u00fc\u06de\3\2\2\2\u00fe\u06e4\3\2"+
		"\2\2\u0100\u06ea\3\2\2\2\u0102\u06f4\3\2\2\2\u0104\u06fb\3\2\2\2\u0106"+
		"\u0702\3\2\2\2\u0108\u0704\3\2\2\2\u010a\u0706\3\2\2\2\u010c\u0708\3\2"+
		"\2\2\u010e\u0713\3\2\2\2\u0110\u0732\3\2\2\2\u0112\u0735\3\2\2\2\u0114"+
		"\u073f\3\2\2\2\u0116\u0758\3\2\2\2\u0118\u075a\3\2\2\2\u011a\u0777\3\2"+
		"\2\2\u011c\u0779\3\2\2\2\u011e\u077b\3\2\2\2\u0120\u077d\3\2\2\2\u0122"+
		"\u077f\3\2\2\2\u0124\u0781\3\2\2\2\u0126\u0789\3\2\2\2\u0128\u078b\3\2"+
		"\2\2\u012a\u0799\3\2\2\2\u012c\u079b\3\2\2\2\u012e\u079d\3\2\2\2\u0130"+
		"\u07a5\3\2\2\2\u0132\u07ad\3\2\2\2\u0134\u07b7\3\2\2\2\u0136\u0137\7I"+
		"\2\2\u0137\3\3\2\2\2\u0138\u013a\t\2\2\2\u0139\u0138\3\2\2\2\u013a\u013b"+
		"\3\2\2\2\u013b\u0139\3\2\2\2\u013b\u013c\3\2\2\2\u013c\5\3\2\2\2\u013d"+
		"\u013f\5R*\2\u013e\u013d\3\2\2\2\u013f\u0142\3\2\2\2\u0140\u013e\3\2\2"+
		"\2\u0140\u0141\3\2\2\2\u0141\u0146\3\2\2\2\u0142\u0140\3\2\2\2\u0143\u0145"+
		"\5Z.\2\u0144\u0143\3\2\2\2\u0145\u0148\3\2\2\2\u0146\u0144\3\2\2\2\u0146"+
		"\u0147\3\2\2\2\u0147\u014a\3\2\2\2\u0148\u0146\3\2\2\2\u0149\u014b\5\4"+
		"\3\2\u014a\u0149\3\2\2\2\u014a\u014b\3\2\2\2\u014b\u014c\3\2\2\2\u014c"+
		"\u014d\7\2\2\3\u014d\7\3\2\2\2\u014e\u0150\5\4\3\2\u014f\u014e\3\2\2\2"+
		"\u014f\u0150\3\2\2\2\u0150\u0151\3\2\2\2\u0151\u0153\7K\2\2\u0152\u014f"+
		"\3\2\2\2\u0152\u0153\3\2\2\2\u0153\t\3\2\2\2\u0154\u0155\t\3\2\2\u0155"+
		"\13\3\2\2\2\u0156\u015b\5\n\6\2\u0157\u0158\7\3\2\2\u0158\u015a\5\n\6"+
		"\2\u0159\u0157\3\2\2\2\u015a\u015d\3\2\2\2\u015b\u0159\3\2\2\2\u015b\u015c"+
		"\3\2\2\2\u015c\r\3\2\2\2\u015d\u015b\3\2\2\2\u015e\u015f\5\f\7\2\u015f"+
		"\u0160\7\4\2\2\u0160\u0162\3\2\2\2\u0161\u015e\3\2\2\2\u0161\u0162\3\2"+
		"\2\2\u0162\u0163\3\2\2\2\u0163\u0164\7a\2\2\u0164\17\3\2\2\2\u0165\u0166"+
		"\5\f\7\2\u0166\u0167\7\4\2\2\u0167\u0169\3\2\2\2\u0168\u0165\3\2\2\2\u0168"+
		"\u0169\3\2\2\2\u0169\u016a\3\2\2\2\u016a\u016b\7b\2\2\u016b\21\3\2\2\2"+
		"\u016c\u016d\7a\2\2\u016d\23\3\2\2\2\u016e\u016f\7a\2\2\u016f\25\3\2\2"+
		"\2\u0170\u0171\7b\2\2\u0171\27\3\2\2\2\u0172\u0173\7a\2\2\u0173\31\3\2"+
		"\2\2\u0174\u0175\5\16\b\2\u0175\33\3\2\2\2\u0176\u0177\7b\2\2\u0177\35"+
		"\3\2\2\2\u0178\u0179\5\20\t\2\u0179\37\3\2\2\2\u017a\u017b\t\4\2\2\u017b"+
		"!\3\2\2\2\u017c\u017d\7b\2\2\u017d#\3\2\2\2\u017e\u017f\5\20\t\2\u017f"+
		"%\3\2\2\2\u0180\u0181\7a\2\2\u0181\'\3\2\2\2\u0182\u018d\5&\24\2\u0183"+
		"\u0185\5\4\3\2\u0184\u0183\3\2\2\2\u0184\u0185\3\2\2\2\u0185\u0186\3\2"+
		"\2\2\u0186\u0188\7\5\2\2\u0187\u0189\5\4\3\2\u0188\u0187\3\2\2\2\u0188"+
		"\u0189\3\2\2\2\u0189\u018a\3\2\2\2\u018a\u018c\5&\24\2\u018b\u0184\3\2"+
		"\2\2\u018c\u018f\3\2\2\2\u018d\u018b\3\2\2\2\u018d\u018e\3\2\2\2\u018e"+
		")\3\2\2\2\u018f\u018d\3\2\2\2\u0190\u0191\5&\24\2\u0191\u0193\7\6\2\2"+
		"\u0192\u0194\5\4\3\2\u0193\u0192\3\2\2\2\u0193\u0194\3\2\2\2\u0194\u0195"+
		"\3\2\2\2\u0195\u0196\5\u010c\u0087\2\u0196+\3\2\2\2\u0197\u01a2\5*\26"+
		"\2\u0198\u019a\5\4\3\2\u0199\u0198\3\2\2\2\u0199\u019a\3\2\2\2\u019a\u019b"+
		"\3\2\2\2\u019b\u019d\7\5\2\2\u019c\u019e\5\4\3\2\u019d\u019c\3\2\2\2\u019d"+
		"\u019e\3\2\2\2\u019e\u019f\3\2\2\2\u019f\u01a1\5*\26\2\u01a0\u0199\3\2"+
		"\2\2\u01a1\u01a4\3\2\2\2\u01a2\u01a0\3\2\2\2\u01a2\u01a3\3\2\2\2\u01a3"+
		"-\3\2\2\2\u01a4\u01a2\3\2\2\2\u01a5\u01a7\7\7\2\2\u01a6\u01a8\5\4\3\2"+
		"\u01a7\u01a6\3\2\2\2\u01a7\u01a8\3\2\2\2\u01a8\u01aa\3\2\2\2\u01a9\u01ab"+
		"\5,\27\2\u01aa\u01a9\3\2\2\2\u01aa\u01ab\3\2\2\2\u01ab\u01ad\3\2\2\2\u01ac"+
		"\u01ae\5\4\3\2\u01ad\u01ac\3\2\2\2\u01ad\u01ae\3\2\2\2\u01ae\u01af\3\2"+
		"\2\2\u01af\u01b1\7\b\2\2\u01b0\u01a5\3\2\2\2\u01b0\u01b1\3\2\2\2\u01b1"+
		"/\3\2\2\2\u01b2\u01b4\5\24\13\2\u01b3\u01b5\5\4\3\2\u01b4\u01b3\3\2\2"+
		"\2\u01b4\u01b5\3\2\2\2\u01b5\u01b6\3\2\2\2\u01b6\u01b8\7\6\2\2\u01b7\u01b9"+
		"\5\4\3\2\u01b8\u01b7\3\2\2\2\u01b8\u01b9\3\2\2\2\u01b9\u01ba\3\2\2\2\u01ba"+
		"\u01bb\5\u010c\u0087\2\u01bb\61\3\2\2\2\u01bc\u01c7\5\60\31\2\u01bd\u01bf"+
		"\5\4\3\2\u01be\u01bd\3\2\2\2\u01be\u01bf\3\2\2\2\u01bf\u01c0\3\2\2\2\u01c0"+
		"\u01c2\7\5\2\2\u01c1\u01c3\5\4\3\2\u01c2\u01c1\3\2\2\2\u01c2\u01c3\3\2"+
		"\2\2\u01c3\u01c4\3\2\2\2\u01c4\u01c6\5\60\31\2\u01c5\u01be\3\2\2\2\u01c6"+
		"\u01c9\3\2\2\2\u01c7\u01c5\3\2\2\2\u01c7\u01c8\3\2\2\2\u01c8\63\3\2\2"+
		"\2\u01c9\u01c7\3\2\2\2\u01ca\u01cc\7\t\2\2\u01cb\u01cd\5\4\3\2\u01cc\u01cb"+
		"\3\2\2\2\u01cc\u01cd\3\2\2\2\u01cd\u01dc\3\2\2\2\u01ce\u01d9\5\24\13\2"+
		"\u01cf\u01d1\5\4\3\2\u01d0\u01cf\3\2\2\2\u01d0\u01d1\3\2\2\2\u01d1\u01d2"+
		"\3\2\2\2\u01d2\u01d4\7\5\2\2\u01d3\u01d5\5\4\3\2\u01d4\u01d3\3\2\2\2\u01d4"+
		"\u01d5\3\2\2\2\u01d5\u01d6\3\2\2\2\u01d6\u01d8\5\24\13\2\u01d7\u01d0\3"+
		"\2\2\2\u01d8\u01db\3\2\2\2\u01d9\u01d7\3\2\2\2\u01d9\u01da\3\2\2\2\u01da"+
		"\u01dd\3\2\2\2\u01db\u01d9\3\2\2\2\u01dc\u01ce\3\2\2\2\u01dc\u01dd\3\2"+
		"\2\2\u01dd\u01df\3\2\2\2\u01de\u01e0\5\4\3\2\u01df\u01de\3\2\2\2\u01df"+
		"\u01e0\3\2\2\2\u01e0\u01e1\3\2\2\2\u01e1\u01e2\7\n\2\2\u01e2\65\3\2\2"+
		"\2\u01e3\u01ee\5\64\33\2\u01e4\u01e6\5\4\3\2\u01e5\u01e4\3\2\2\2\u01e5"+
		"\u01e6\3\2\2\2\u01e6\u01e7\3\2\2\2\u01e7\u01e9\7\5\2\2\u01e8\u01ea\5\4"+
		"\3\2\u01e9\u01e8\3\2\2\2\u01e9\u01ea\3\2\2\2\u01ea\u01eb\3\2\2\2\u01eb"+
		"\u01ed\5\64\33\2\u01ec\u01e5\3\2\2\2\u01ed\u01f0\3\2\2\2\u01ee\u01ec\3"+
		"\2\2\2\u01ee\u01ef\3\2\2\2\u01ef\67\3\2\2\2\u01f0\u01ee\3\2\2\2\u01f1"+
		"\u01f2\7]\2\2\u01f2\u01f3\5\4\3\2\u01f3\u01f5\5\u00c8e\2\u01f4\u01f6\5"+
		"\4\3\2\u01f5\u01f4\3\2\2\2\u01f5\u01f6\3\2\2\2\u01f6\u01f7\3\2\2\2\u01f7"+
		"\u01f9\7\13\2\2\u01f8\u01fa\5\4\3\2\u01f9\u01f8\3\2\2\2\u01f9\u01fa\3"+
		"\2\2\2\u01fa\u01fb\3\2\2\2\u01fb\u01fd\5\u0084C\2\u01fc\u01fe\7K\2\2\u01fd"+
		"\u01fc\3\2\2\2\u01fd\u01fe\3\2\2\2\u01fe9\3\2\2\2\u01ff\u0206\58\35\2"+
		"\u0200\u0202\5\4\3\2\u0201\u0200\3\2\2\2\u0201\u0202\3\2\2\2\u0202\u0203"+
		"\3\2\2\2\u0203\u0205\58\35\2\u0204\u0201\3\2\2\2\u0205\u0208\3\2\2\2\u0206"+
		"\u0204\3\2\2\2\u0206\u0207\3\2\2\2\u0207;\3\2\2\2\u0208\u0206\3\2\2\2"+
		"\u0209\u020a\7]\2\2\u020a\u020b\5\4\3\2\u020b\u020d\5\u0084C\2\u020c\u020e"+
		"\5\4\3\2\u020d\u020c\3\2\2\2\u020d\u020e\3\2\2\2\u020e\u020f\3\2\2\2\u020f"+
		"\u0211\7\13\2\2\u0210\u0212\5\4\3\2\u0211\u0210\3\2\2\2\u0211\u0212\3"+
		"\2\2\2\u0212\u0213\3\2\2\2\u0213\u0215\5\u0084C\2\u0214\u0216\7K\2\2\u0215"+
		"\u0214\3\2\2\2\u0215\u0216\3\2\2\2\u0216=\3\2\2\2\u0217\u021e\5<\37\2"+
		"\u0218\u021a\5\4\3\2\u0219\u0218\3\2\2\2\u0219\u021a\3\2\2\2\u021a\u021b"+
		"\3\2\2\2\u021b\u021d\5<\37\2\u021c\u0219\3\2\2\2\u021d\u0220\3\2\2\2\u021e"+
		"\u021c\3\2\2\2\u021e\u021f\3\2\2\2\u021f?\3\2\2\2\u0220\u021e\3\2\2\2"+
		"\u0221\u022a\5&\24\2\u0222\u0224\5\4\3\2\u0223\u0222\3\2\2\2\u0223\u0224"+
		"\3\2\2\2\u0224\u0225\3\2\2\2\u0225\u0227\7\6\2\2\u0226\u0228\5\4\3\2\u0227"+
		"\u0226\3\2\2\2\u0227\u0228\3\2\2\2\u0228\u0229\3\2\2\2\u0229\u022b\5\u010c"+
		"\u0087\2\u022a\u0223\3\2\2\2\u022a\u022b\3\2\2\2\u022bA\3\2\2\2\u022c"+
		"\u022e\7\f\2\2\u022d\u022f\5\4\3\2\u022e\u022d\3\2\2\2\u022e\u022f\3\2"+
		"\2\2\u022f\u0230\3\2\2\2\u0230\u023b\5@!\2\u0231\u0233\5\4\3\2\u0232\u0231"+
		"\3\2\2\2\u0232\u0233\3\2\2\2\u0233\u0234\3\2\2\2\u0234\u0236\7\5\2\2\u0235"+
		"\u0237\5\4\3\2\u0236\u0235\3\2\2\2\u0236\u0237\3\2\2\2\u0237\u0238\3\2"+
		"\2\2\u0238\u023a\5@!\2\u0239\u0232\3\2\2\2\u023a\u023d\3\2\2\2\u023b\u0239"+
		"\3\2\2\2\u023b\u023c\3\2\2\2\u023c\u023e\3\2\2\2\u023d\u023b\3\2\2\2\u023e"+
		"\u023f\7\r\2\2\u023f\u0241\3\2\2\2\u0240\u022c\3\2\2\2\u0240\u0241\3\2"+
		"\2\2\u0241C\3\2\2\2\u0242\u0243\7\f\2\2\u0243\u024e\5\u010c\u0087\2\u0244"+
		"\u0246\5\4\3\2\u0245\u0244\3\2\2\2\u0245\u0246\3\2\2\2\u0246\u0247\3\2"+
		"\2\2\u0247\u0249\7\5\2\2\u0248\u024a\5\4\3\2\u0249\u0248\3\2\2\2\u0249"+
		"\u024a\3\2\2\2\u024a\u024b\3\2\2\2\u024b\u024d\5\u010c\u0087\2\u024c\u0245"+
		"\3\2\2\2\u024d\u0250\3\2\2\2\u024e\u024c\3\2\2\2\u024e\u024f\3\2\2\2\u024f"+
		"\u0251\3\2\2\2\u0250\u024e\3\2\2\2\u0251\u0252\7\r\2\2\u0252E\3\2\2\2"+
		"\u0253\u0254\5\26\f\2\u0254\u0255\5D#\2\u0255G\3\2\2\2\u0256\u0261\5F"+
		"$\2\u0257\u0259\5\4\3\2\u0258\u0257\3\2\2\2\u0258\u0259\3\2\2\2\u0259"+
		"\u025a\3\2\2\2\u025a\u025c\7\5\2\2\u025b\u025d\5\4\3\2\u025c\u025b\3\2"+
		"\2\2\u025c\u025d\3\2\2\2\u025d\u025e\3\2\2\2\u025e\u0260\5F$\2\u025f\u0258"+
		"\3\2\2\2\u0260\u0263\3\2\2\2\u0261\u025f\3\2\2\2\u0261\u0262\3\2\2\2\u0262"+
		"I\3\2\2\2\u0263\u0261\3\2\2\2\u0264\u0266\5\4\3\2\u0265\u0264\3\2\2\2"+
		"\u0265\u0266\3\2\2\2\u0266\u0267\3\2\2\2\u0267\u0269\7\13\2\2\u0268\u026a"+
		"\5\4\3\2\u0269\u0268\3\2\2\2\u0269\u026a\3\2\2\2\u026a\u026b\3\2\2\2\u026b"+
		"\u026d\5H%\2\u026c\u0265\3\2\2\2\u026c\u026d\3\2\2\2\u026dK\3\2\2\2\u026e"+
		"\u0270\5\4\3\2\u026f\u026e\3\2\2\2\u026f\u0270\3\2\2\2\u0270\u0271\3\2"+
		"\2\2\u0271\u0273\7\16\2\2\u0272\u0274\5\4\3\2\u0273\u0272\3\2\2\2\u0273"+
		"\u0274\3\2\2\2\u0274\u0275\3\2\2\2\u0275\u0277\5H%\2\u0276\u026f\3\2\2"+
		"\2\u0276\u0277\3\2\2\2\u0277M\3\2\2\2\u0278\u0279\7\17\2\2\u0279\u027a"+
		"\5\22\n\2\u027aO\3\2\2\2\u027b\u0281\5N(\2\u027c\u027d\5\4\3\2\u027d\u027e"+
		"\5N(\2\u027e\u0280\3\2\2\2\u027f\u027c\3\2\2\2\u0280\u0283\3\2\2\2\u0281"+
		"\u027f\3\2\2\2\u0281\u0282\3\2\2\2\u0282Q\3\2\2\2\u0283\u0281\3\2\2\2"+
		"\u0284\u0286\5\4\3\2\u0285\u0284\3\2\2\2\u0285\u0286\3\2\2\2\u0286\u028a"+
		"\3\2\2\2\u0287\u028b\5T+\2\u0288\u028b\5V,\2\u0289\u028b\5X-\2\u028a\u0287"+
		"\3\2\2\2\u028a\u0288\3\2\2\2\u028a\u0289\3\2\2\2\u028bS\3\2\2\2\u028c"+
		"\u028d\7`\2\2\u028d\u028e\5\4\3\2\u028e\u028f\5\f\7\2\u028f\u0290\7\4"+
		"\2\2\u0290\u0291\7\\\2\2\u0291\u0292\5\b\5\2\u0292U\3\2\2\2\u0293\u0294"+
		"\7`\2\2\u0294\u0295\5\4\3\2\u0295\u0296\5\f\7\2\u0296\u0297\7\4\2\2\u0297"+
		"\u0298\5\n\6\2\u0298\u0299\5\b\5\2\u0299W\3\2\2\2\u029a\u029b\7`\2\2\u029b"+
		"\u029c\5\4\3\2\u029c\u029d\5\f\7\2\u029d\u029e\5\b\5\2\u029eY\3\2\2\2"+
		"\u029f\u02ae\5\\/\2\u02a0\u02ae\5^\60\2\u02a1\u02ae\5d\63\2\u02a2\u02ae"+
		"\5f\64\2\u02a3\u02ae\5h\65\2\u02a4\u02ae\5j\66\2\u02a5\u02ae\5l\67\2\u02a6"+
		"\u02ae\5n8\2\u02a7\u02ae\5p9\2\u02a8\u02ae\5r:\2\u02a9\u02ae\5x=\2\u02aa"+
		"\u02ae\5z>\2\u02ab\u02ae\5\u0080A\2\u02ac\u02ae\5~@\2\u02ad\u029f\3\2"+
		"\2\2\u02ad\u02a0\3\2\2\2\u02ad\u02a1\3\2\2\2\u02ad\u02a2\3\2\2\2\u02ad"+
		"\u02a3\3\2\2\2\u02ad\u02a4\3\2\2\2\u02ad\u02a5\3\2\2\2\u02ad\u02a6\3\2"+
		"\2\2\u02ad\u02a7\3\2\2\2\u02ad\u02a8\3\2\2\2\u02ad\u02a9\3\2\2\2\u02ad"+
		"\u02aa\3\2\2\2\u02ad\u02ab\3\2\2\2\u02ad\u02ac\3\2\2\2\u02ae[\3\2\2\2"+
		"\u02af\u02b1\5\4\3\2\u02b0\u02af\3\2\2\2\u02b0\u02b1\3\2\2\2\u02b1\u02b2"+
		"\3\2\2\2\u02b2\u02b3\7N\2\2\u02b3\u02b4\5\4\3\2\u02b4\u02b6\5\f\7\2\u02b5"+
		"\u02b7\5\4\3\2\u02b6\u02b5\3\2\2\2\u02b6\u02b7\3\2\2\2\u02b7\u02b8\3\2"+
		"\2\2\u02b8\u02ba\7\t\2\2\u02b9\u02bb\5\4\3\2\u02ba\u02b9\3\2\2\2\u02ba"+
		"\u02bb\3\2\2\2\u02bb\u02bf\3\2\2\2\u02bc\u02be\5Z.\2\u02bd\u02bc\3\2\2"+
		"\2\u02be\u02c1\3\2\2\2\u02bf\u02bd\3\2\2\2\u02bf\u02c0\3\2\2\2\u02c0\u02c3"+
		"\3\2\2\2\u02c1\u02bf\3\2\2\2\u02c2\u02c4\5\4\3\2\u02c3\u02c2\3\2\2\2\u02c3"+
		"\u02c4\3\2\2\2\u02c4\u02c5\3\2\2\2\u02c5\u02c6\7\n\2\2\u02c6\u02c7\5\b"+
		"\5\2\u02c7]\3\2\2\2\u02c8\u02ca\7J\2\2\u02c9\u02c8\3\2\2\2\u02c9\u02ca"+
		"\3\2\2\2\u02ca\u02cb\3\2\2\2\u02cb\u02cd\5\2\2\2\u02cc\u02c9\3\2\2\2\u02cd"+
		"\u02d0\3\2\2\2\u02ce\u02cc\3\2\2\2\u02ce\u02cf\3\2\2\2\u02cf\u02d2\3\2"+
		"\2\2\u02d0\u02ce\3\2\2\2\u02d1\u02d3\5\4\3\2\u02d2\u02d1\3\2\2\2\u02d2"+
		"\u02d3\3\2\2\2\u02d3\u02d4\3\2\2\2\u02d4\u02d5\7M\2\2\u02d5\u02d6\5\4"+
		"\3\2\u02d6\u02d7\5\"\22\2\u02d7\u02d9\5B\"\2\u02d8\u02da\5\4\3\2\u02d9"+
		"\u02d8\3\2\2\2\u02d9\u02da\3\2\2\2\u02da\u02db\3\2\2\2\u02db\u02dd\7\t"+
		"\2\2\u02dc\u02de\5\4\3\2\u02dd\u02dc\3\2\2\2\u02dd\u02de\3\2\2\2\u02de"+
		"\u02df\3\2\2\2\u02df\u02e1\5`\61\2\u02e0\u02e2\5\4\3\2\u02e1\u02e0\3\2"+
		"\2\2\u02e1\u02e2\3\2\2\2\u02e2\u02e3\3\2\2\2\u02e3\u02e4\7\n\2\2\u02e4"+
		"\u02e5\5\b\5\2\u02e5_\3\2\2\2\u02e6\u02f1\5b\62\2\u02e7\u02e9\5\4\3\2"+
		"\u02e8\u02e7\3\2\2\2\u02e8\u02e9\3\2\2\2\u02e9\u02ea\3\2\2\2\u02ea\u02ec"+
		"\7\5\2\2\u02eb\u02ed\5\4\3\2\u02ec\u02eb\3\2\2\2\u02ec\u02ed\3\2\2\2\u02ed"+
		"\u02ee\3\2\2\2\u02ee\u02f0\5b\62\2\u02ef\u02e8\3\2\2\2\u02f0\u02f3\3\2"+
		"\2\2\u02f1\u02ef\3\2\2\2\u02f1\u02f2\3\2\2\2\u02f2a\3\2\2\2\u02f3\u02f1"+
		"\3\2\2\2\u02f4\u02f5\7]\2\2\u02f5\u02f6\5\4\3\2\u02f6\u02f8\5 \21\2\u02f7"+
		"\u02f9\5\u0116\u008c\2\u02f8\u02f7\3\2\2\2\u02f8\u02f9\3\2\2\2\u02f9c"+
		"\3\2\2\2\u02fa\u02fc\7J\2\2\u02fb\u02fa\3\2\2\2\u02fb\u02fc\3\2\2\2\u02fc"+
		"\u02fd\3\2\2\2\u02fd\u02ff\5\2\2\2\u02fe\u02fb\3\2\2\2\u02ff\u0302\3\2"+
		"\2\2\u0300\u02fe\3\2\2\2\u0300\u0301\3\2\2\2\u0301\u0304\3\2\2\2\u0302"+
		"\u0300\3\2\2\2\u0303\u0305\5\4\3\2\u0304\u0303\3\2\2\2\u0304\u0305\3\2"+
		"\2\2\u0305\u0306\3\2\2\2\u0306\u0307\7O\2\2\u0307\u0308\5\4\3\2\u0308"+
		"\u030a\5\34\17\2\u0309\u030b\5\4\3\2\u030a\u0309\3\2\2\2\u030a\u030b\3"+
		"\2\2\2\u030b\u030c\3\2\2\2\u030c\u030e\7\7\2\2\u030d\u030f\5\4\3\2\u030e"+
		"\u030d\3\2\2\2\u030e\u030f\3\2\2\2\u030f\u0311\3\2\2\2\u0310\u0312\5\62"+
		"\32\2\u0311\u0310\3\2\2\2\u0311\u0312\3\2\2\2\u0312\u0314\3\2\2\2\u0313"+
		"\u0315\5\4\3\2\u0314\u0313\3\2\2\2\u0314\u0315\3\2\2\2\u0315\u0316\3\2"+
		"\2\2\u0316\u0317\7\b\2\2\u0317\u0318\5\b\5\2\u0318e\3\2\2\2\u0319\u031b"+
		"\7J\2\2\u031a\u0319\3\2\2\2\u031a\u031b\3\2\2\2\u031b\u031c\3\2\2\2\u031c"+
		"\u031e\5\2\2\2\u031d\u031a\3\2\2\2\u031e\u0321\3\2\2\2\u031f\u031d\3\2"+
		"\2\2\u031f\u0320\3\2\2\2\u0320\u0323\3\2\2\2\u0321\u031f\3\2\2\2\u0322"+
		"\u0324\5\4\3\2\u0323\u0322\3\2\2\2\u0323\u0324\3\2\2\2\u0324\u0325\3\2"+
		"\2\2\u0325\u0326\7P\2\2\u0326\u0327\5\4\3\2\u0327\u0329\5\34\17\2\u0328"+
		"\u032a\5\4\3\2\u0329\u0328\3\2\2\2\u0329\u032a\3\2\2\2\u032a\u032b\3\2"+
		"\2\2\u032b\u032d\7\7\2\2\u032c\u032e\5\4\3\2\u032d\u032c\3\2\2\2\u032d"+
		"\u032e\3\2\2\2\u032e\u0330\3\2\2\2\u032f\u0331\5\62\32\2\u0330\u032f\3"+
		"\2\2\2\u0330\u0331\3\2\2\2\u0331\u0333\3\2\2\2\u0332\u0334\5\4\3\2\u0333"+
		"\u0332\3\2\2\2\u0333\u0334\3\2\2\2\u0334\u0335\3\2\2\2\u0335\u0336\7\b"+
		"\2\2\u0336\u0337\5\b\5\2\u0337g\3\2\2\2\u0338\u033a\5\4\3\2\u0339\u0338"+
		"\3\2\2\2\u0339\u033a\3\2\2\2\u033a\u033b\3\2\2\2\u033b\u033c\7Q\2\2\u033c"+
		"\u033d\5\4\3\2\u033d\u033f\5\36\20\2\u033e\u0340\5\4\3\2\u033f\u033e\3"+
		"\2\2\2\u033f\u0340\3\2\2\2\u0340\u0341\3\2\2\2\u0341\u0343\7\7\2\2\u0342"+
		"\u0344\5\4\3\2\u0343\u0342\3\2\2\2\u0343\u0344\3\2\2\2\u0344\u0346\3\2"+
		"\2\2\u0345\u0347\5\66\34\2\u0346\u0345\3\2\2\2\u0346\u0347\3\2\2\2\u0347"+
		"\u0349\3\2\2\2\u0348\u034a\5\4\3\2\u0349\u0348\3\2\2\2\u0349\u034a\3\2"+
		"\2\2\u034a\u034b\3\2\2\2\u034b\u034c\7\b\2\2\u034c\u034d\5\b\5\2\u034d"+
		"i\3\2\2\2\u034e\u0350\7J\2\2\u034f\u034e\3\2\2\2\u034f\u0350\3\2\2\2\u0350"+
		"\u0351\3\2\2\2\u0351\u0353\5\2\2\2\u0352\u034f\3\2\2\2\u0353\u0356\3\2"+
		"\2\2\u0354\u0352\3\2\2\2\u0354\u0355\3\2\2\2\u0355\u0358\3\2\2\2\u0356"+
		"\u0354\3\2\2\2\u0357\u0359\5\4\3\2\u0358\u0357\3\2\2\2\u0358\u0359\3\2"+
		"\2\2\u0359\u035a\3\2\2\2\u035a\u035b\7R\2\2\u035b\u035c\5\4\3\2\u035c"+
		"\u035e\5\30\r\2\u035d\u035f\5\4\3\2\u035e\u035d\3\2\2\2\u035e\u035f\3"+
		"\2\2\2\u035f\u0360\3\2\2\2\u0360\u0362\5.\30\2\u0361\u0363\5\4\3\2\u0362"+
		"\u0361\3\2\2\2\u0362\u0363\3\2\2\2\u0363\u0364\3\2\2\2\u0364\u0366\7\6"+
		"\2\2\u0365\u0367\5\4\3\2\u0366\u0365\3\2\2\2\u0366\u0367\3\2\2\2\u0367"+
		"\u0368\3\2\2\2\u0368\u0369\5\u010c\u0087\2\u0369\u036a\5\b\5\2\u036ak"+
		"\3\2\2\2\u036b\u036d\7J\2\2\u036c\u036b\3\2\2\2\u036c\u036d\3\2\2\2\u036d"+
		"\u036e\3\2\2\2\u036e\u0370\5\2\2\2\u036f\u036c\3\2\2\2\u0370\u0373\3\2"+
		"\2\2\u0371\u036f\3\2\2\2\u0371\u0372\3\2\2\2\u0372\u0375\3\2\2\2\u0373"+
		"\u0371\3\2\2\2\u0374\u0376\5\4\3\2\u0375\u0374\3\2\2\2\u0375\u0376\3\2"+
		"\2\2\u0376\u0377\3\2\2\2\u0377\u0379\7S\2\2\u0378\u037a\5\4\3\2\u0379"+
		"\u0378\3\2\2\2\u0379\u037a\3\2\2\2\u037a\u037b\3\2\2\2\u037b\u037c\7R"+
		"\2\2\u037c\u037d\5\4\3\2\u037d\u037f\5\30\r\2\u037e\u0380\5\4\3\2\u037f"+
		"\u037e\3\2\2\2\u037f\u0380\3\2\2\2\u0380\u0381\3\2\2\2\u0381\u0383\5."+
		"\30\2\u0382\u0384\5\4\3\2\u0383\u0382\3\2\2\2\u0383\u0384\3\2\2\2\u0384"+
		"\u0385\3\2\2\2\u0385\u0387\7\6\2\2\u0386\u0388\5\4\3\2\u0387\u0386\3\2"+
		"\2\2\u0387\u0388\3\2\2\2\u0388\u0389\3\2\2\2\u0389\u038a\5\u010c\u0087"+
		"\2\u038a\u038b\5\b\5\2\u038bm\3\2\2\2\u038c\u038e\7J\2\2\u038d\u038c\3"+
		"\2\2\2\u038d\u038e\3\2\2\2\u038e\u038f\3\2\2\2\u038f\u0391\5\2\2\2\u0390"+
		"\u038d\3\2\2\2\u0391\u0394\3\2\2\2\u0392\u0390\3\2\2\2\u0392\u0393\3\2"+
		"\2\2\u0393\u0396\3\2\2\2\u0394\u0392\3\2\2\2\u0395\u0397\5\4\3\2\u0396"+
		"\u0395\3\2\2\2\u0396\u0397\3\2\2\2\u0397\u0399\3\2\2\2\u0398\u039a\5P"+
		")\2\u0399\u0398\3\2\2\2\u0399\u039a\3\2\2\2\u039a\u039c\3\2\2\2\u039b"+
		"\u039d\5\4\3\2\u039c\u039b\3\2\2\2\u039c\u039d\3\2\2\2\u039d\u039e\3\2"+
		"\2\2\u039e\u039f\7R\2\2\u039f\u03a0\5\4\3\2\u03a0\u03a2\5\30\r\2\u03a1"+
		"\u03a3\5\4\3\2\u03a2\u03a1\3\2\2\2\u03a2\u03a3\3\2\2\2\u03a3\u03a4\3\2"+
		"\2\2\u03a4\u03a5\5B\"\2\u03a5\u03a7\5.\30\2\u03a6\u03a8\5\4\3\2\u03a7"+
		"\u03a6\3\2\2\2\u03a7\u03a8\3\2\2\2\u03a8\u03a9\3\2\2\2\u03a9\u03ab\7\6"+
		"\2\2\u03aa\u03ac\5\4\3\2\u03ab\u03aa\3\2\2\2\u03ab\u03ac\3\2\2\2\u03ac"+
		"\u03ad\3\2\2\2\u03ad\u03af\5\u010c\u0087\2\u03ae\u03b0\5\4\3\2\u03af\u03ae"+
		"\3\2\2\2\u03af\u03b0\3\2\2\2\u03b0\u03b1\3\2\2\2\u03b1\u03b3\7\20\2\2"+
		"\u03b2\u03b4\5\4\3\2\u03b3\u03b2\3\2\2\2\u03b3\u03b4\3\2\2\2\u03b4\u03b5"+
		"\3\2\2\2\u03b5\u03b6\5\u0084C\2\u03b6\u03b7\5\b\5\2\u03b7o\3\2\2\2\u03b8"+
		"\u03ba\7J\2\2\u03b9\u03b8\3\2\2\2\u03b9\u03ba\3\2\2\2\u03ba\u03bb\3\2"+
		"\2\2\u03bb\u03bd\5\2\2\2\u03bc\u03b9\3\2\2\2\u03bd\u03c0\3\2\2\2\u03be"+
		"\u03bc\3\2\2\2\u03be\u03bf\3\2\2\2\u03bf\u03c2\3\2\2\2\u03c0\u03be\3\2"+
		"\2\2\u03c1\u03c3\5\4\3\2\u03c2\u03c1\3\2\2\2\u03c2\u03c3\3\2\2\2\u03c3"+
		"\u03c4\3\2\2\2\u03c4\u03c5\7T\2\2\u03c5\u03c6\5\4\3\2\u03c6\u03c8\5\30"+
		"\r\2\u03c7\u03c9\5\4\3\2\u03c8\u03c7\3\2\2\2\u03c8\u03c9\3\2\2\2\u03c9"+
		"\u03ca\3\2\2\2\u03ca\u03cc\5B\"\2\u03cb\u03cd\5\4\3\2\u03cc\u03cb\3\2"+
		"\2\2\u03cc\u03cd\3\2\2\2\u03cd\u03ce\3\2\2\2\u03ce\u03d0\5.\30\2\u03cf"+
		"\u03d1\5\4\3\2\u03d0\u03cf\3\2\2\2\u03d0\u03d1\3\2\2\2\u03d1\u03d2\3\2"+
		"\2\2\u03d2\u03d4\7\6\2\2\u03d3\u03d5\5\4\3\2\u03d4\u03d3\3\2\2\2\u03d4"+
		"\u03d5\3\2\2\2\u03d5\u03d6\3\2\2\2\u03d6\u03d8\5\u010c\u0087\2\u03d7\u03d9"+
		"\5\4\3\2\u03d8\u03d7\3\2\2\2\u03d8\u03d9\3\2\2\2\u03d9\u03da\3\2\2\2\u03da"+
		"\u03dc\7\20\2\2\u03db\u03dd\5\4\3\2\u03dc\u03db\3\2\2\2\u03dc\u03dd\3"+
		"\2\2\2\u03dd\u03de\3\2\2\2\u03de\u03df\5\u0084C\2\u03df\u03e0\5\b\5\2"+
		"\u03e0q\3\2\2\2\u03e1\u03e3\7J\2\2\u03e2\u03e1\3\2\2\2\u03e2\u03e3\3\2"+
		"\2\2\u03e3\u03e4\3\2\2\2\u03e4\u03e6\5\2\2\2\u03e5\u03e2\3\2\2\2\u03e6"+
		"\u03e9\3\2\2\2\u03e7\u03e5\3\2\2\2\u03e7\u03e8\3\2\2\2\u03e8\u03eb\3\2"+
		"\2\2\u03e9\u03e7\3\2\2\2\u03ea\u03ec\5\4\3\2\u03eb\u03ea\3\2\2\2\u03eb"+
		"\u03ec\3\2\2\2\u03ec\u03ed\3\2\2\2\u03ed\u03ee\7U\2\2\u03ee\u03ef\5\4"+
		"\3\2\u03ef\u03f0\5\26\f\2\u03f0\u03f1\5D#\2\u03f1\u03f3\5J&\2\u03f2\u03f4"+
		"\5\4\3\2\u03f3\u03f2\3\2\2\2\u03f3\u03f4\3\2\2\2\u03f4\u03f5\3\2\2\2\u03f5"+
		"\u03f6\5t;\2\u03f6s\3\2\2\2\u03f7\u03f9\7\t\2\2\u03f8\u03fa\5\4\3\2\u03f9"+
		"\u03f8\3\2\2\2\u03f9\u03fa\3\2\2\2\u03fa\u0401\3\2\2\2\u03fb\u03fd\5v"+
		"<\2\u03fc\u03fe\5\4\3\2\u03fd\u03fc\3\2\2\2\u03fd\u03fe\3\2\2\2\u03fe"+
		"\u0400\3\2\2\2\u03ff\u03fb\3\2\2\2\u0400\u0403\3\2\2\2\u0401\u03ff\3\2"+
		"\2\2\u0401\u0402\3\2\2\2\u0402\u0404\3\2\2\2\u0403\u0401\3\2\2\2\u0404"+
		"\u0405\7\n\2\2\u0405u\3\2\2\2\u0406\u040a\5n8\2\u0407\u040a\5j\66\2\u0408"+
		"\u040a\5p9\2\u0409\u0406\3\2\2\2\u0409\u0407\3\2\2\2\u0409\u0408\3\2\2"+
		"\2\u040aw\3\2\2\2\u040b\u040d\5\4\3\2\u040c\u040b\3\2\2\2\u040c\u040d"+
		"\3\2\2\2\u040d\u040e\3\2\2\2\u040e\u0410\5\u0126\u0094\2\u040f\u0411\5"+
		"\4\3\2\u0410\u040f\3\2\2\2\u0410\u0411\3\2\2\2\u0411\u0412\3\2\2\2\u0412"+
		"\u0413\7\3\2\2\u0413y\3\2\2\2\u0414\u0416\5\4\3\2\u0415\u0414\3\2\2\2"+
		"\u0415\u0416\3\2\2\2\u0416\u0417\3\2\2\2\u0417\u0419\5\u0126\u0094\2\u0418"+
		"\u041a\5\4\3\2\u0419\u0418\3\2\2\2\u0419\u041a\3\2\2\2\u041a\u041b\3\2"+
		"\2\2\u041b\u041d\7\21\2\2\u041c\u041e\5\4\3\2\u041d\u041c\3\2\2\2\u041d"+
		"\u041e\3\2\2\2\u041e\u041f\3\2\2\2\u041f\u0421\5\u0128\u0095\2\u0420\u0422"+
		"\5\4\3\2\u0421\u0420\3\2\2\2\u0421\u0422\3\2\2\2\u0422\u0423\3\2\2\2\u0423"+
		"\u0424\7\3\2\2\u0424{\3\2\2\2\u0425\u0426\5\u0088E\2\u0426}\3\2\2\2\u0427"+
		"\u0429\5\4\3\2\u0428\u0427\3\2\2\2\u0428\u0429\3\2\2\2\u0429\u042a\3\2"+
		"\2\2\u042a\u042c\7V\2\2\u042b\u042d\5\4\3\2\u042c\u042b\3\2\2\2\u042c"+
		"\u042d\3\2\2\2\u042d\u042e\3\2\2\2\u042e\u042f\5\u010c\u0087\2\u042f\u0431"+
		"\7\22\2\2\u0430\u0432\5\4\3\2\u0431\u0430\3\2\2\2\u0431\u0432\3\2\2\2"+
		"\u0432\u0433\3\2\2\2\u0433\u0435\7\20\2\2\u0434\u0436\5\4\3\2\u0435\u0434"+
		"\3\2\2\2\u0435\u0436\3\2\2\2\u0436\u0437\3\2\2\2\u0437\u0439\7\7\2\2\u0438"+
		"\u043a\5\4\3\2\u0439\u0438\3\2\2\2\u0439\u043a\3\2\2\2\u043a\u043b\3\2"+
		"\2\2\u043b\u043d\5|?\2\u043c\u043e\5\4\3\2\u043d\u043c\3\2\2\2\u043d\u043e"+
		"\3\2\2\2\u043e\u043f\3\2\2\2\u043f\u0440\7\b\2\2\u0440\u0441\5\b\5\2\u0441"+
		"\177\3\2\2\2\u0442\u0444\7J\2\2\u0443\u0442\3\2\2\2\u0443\u0444\3\2\2"+
		"\2\u0444\u0445\3\2\2\2\u0445\u0447\5\2\2\2\u0446\u0443\3\2\2\2\u0447\u044a"+
		"\3\2\2\2\u0448\u0446\3\2\2\2\u0448\u0449\3\2\2\2\u0449\u044c\3\2\2\2\u044a"+
		"\u0448\3\2\2\2\u044b\u044d\5\4\3\2\u044c\u044b\3\2\2\2\u044c\u044d\3\2"+
		"\2\2\u044d\u044e\3\2\2\2\u044e\u044f\7W\2\2\u044f\u0450\5\4\3\2\u0450"+
		"\u0451\5\26\f\2\u0451\u0453\5D#\2\u0452\u0454\5\4\3\2\u0453\u0452\3\2"+
		"\2\2\u0453\u0454\3\2\2\2\u0454\u0455\3\2\2\2\u0455\u0457\5L\'\2\u0456"+
		"\u0458\5\4\3\2\u0457\u0456\3\2\2\2\u0457\u0458\3\2\2\2\u0458\u0459\3\2"+
		"\2\2\u0459\u045a\5\u0082B\2\u045a\u0081\3\2\2\2\u045b\u045d\7\t\2\2\u045c"+
		"\u045e\5\4\3\2\u045d\u045c\3\2\2\2\u045d\u045e\3\2\2\2\u045e\u0462\3\2"+
		"\2\2\u045f\u0461\5n8\2\u0460\u045f\3\2\2\2\u0461\u0464\3\2\2\2\u0462\u0460"+
		"\3\2\2\2\u0462\u0463\3\2\2\2\u0463\u0466\3\2\2\2\u0464\u0462\3\2\2\2\u0465"+
		"\u0467\5\4\3\2\u0466\u0465\3\2\2\2\u0466\u0467\3\2\2\2\u0467\u0468\3\2"+
		"\2\2\u0468\u0469\7\n\2\2\u0469\u0083\3\2\2\2\u046a\u046c\7\t\2\2\u046b"+
		"\u046d\5\4\3\2\u046c\u046b\3\2\2\2\u046c\u046d\3\2\2\2\u046d\u046e\3\2"+
		"\2\2\u046e\u0470\5\u0084C\2\u046f\u0471\5\4\3\2\u0470\u046f\3\2\2\2\u0470"+
		"\u0471\3\2\2\2\u0471\u0472\3\2\2\2\u0472\u0474\7\n\2\2\u0473\u0475\5\4"+
		"\3\2\u0474\u0473\3\2\2\2\u0474\u0475\3\2\2\2\u0475\u0478\3\2\2\2\u0476"+
		"\u0478\5\u0086D\2\u0477\u046a\3\2\2\2\u0477\u0476\3\2\2\2\u0478\u0085"+
		"\3\2\2\2\u0479\u0483\5\u008aF\2\u047a\u047c\5\4\3\2\u047b\u047a\3\2\2"+
		"\2\u047b\u047c\3\2\2\2\u047c\u047d\3\2\2\2\u047d\u047f\5\u011c\u008f\2"+
		"\u047e\u0480\5\4\3\2\u047f\u047e\3\2\2\2\u047f\u0480\3\2\2\2\u0480\u0481"+
		"\3\2\2\2\u0481\u0482\5\u008aF\2\u0482\u0484\3\2\2\2\u0483\u047b\3\2\2"+
		"\2\u0483\u0484\3\2\2\2\u0484\u0087\3\2\2\2\u0485\u0490\5\u0084C\2\u0486"+
		"\u0488\5\4\3\2\u0487\u0486\3\2\2\2\u0487\u0488\3\2\2\2\u0488\u0489\3\2"+
		"\2\2\u0489\u048b\7\5\2\2\u048a\u048c\5\4\3\2\u048b\u048a\3\2\2\2\u048b"+
		"\u048c\3\2\2\2\u048c\u048d\3\2\2\2\u048d\u048f\5\u0084C\2\u048e\u0487"+
		"\3\2\2\2\u048f\u0492\3\2\2\2\u0490\u048e\3\2\2\2\u0490\u0491\3\2\2\2\u0491"+
		"\u0089\3\2\2\2\u0492\u0490\3\2\2\2\u0493\u049d\5\u008cG\2\u0494\u0496"+
		"\5\4\3\2\u0495\u0494\3\2\2\2\u0495\u0496\3\2\2\2\u0496\u0497\3\2\2\2\u0497"+
		"\u0499\5\u011e\u0090\2\u0498\u049a\5\4\3\2\u0499\u0498\3\2\2\2\u0499\u049a"+
		"\3\2\2\2\u049a\u049b\3\2\2\2\u049b\u049c\5\u008cG\2\u049c\u049e\3\2\2"+
		"\2\u049d\u0495\3\2\2\2\u049d\u049e\3\2\2\2\u049e\u008b\3\2\2\2\u049f\u04a0"+
		"\bG\1\2\u04a0\u04a1\5\u008eH\2\u04a1\u04ae\3\2\2\2\u04a2\u04a4\f\4\2\2"+
		"\u04a3\u04a5\5\4\3\2\u04a4\u04a3\3\2\2\2\u04a4\u04a5\3\2\2\2\u04a5\u04a6"+
		"\3\2\2\2\u04a6\u04a8\5\u0122\u0092\2\u04a7\u04a9\5\4\3\2\u04a8\u04a7\3"+
		"\2\2\2\u04a8\u04a9\3\2\2\2\u04a9\u04aa\3\2\2\2\u04aa\u04ab\5\u008eH\2"+
		"\u04ab\u04ad\3\2\2\2\u04ac\u04a2\3\2\2\2\u04ad\u04b0\3\2\2\2\u04ae\u04ac"+
		"\3\2\2\2\u04ae\u04af\3\2\2\2\u04af\u008d\3\2\2\2\u04b0\u04ae\3\2\2\2\u04b1"+
		"\u04b2\bH\1\2\u04b2\u04b3\5\u0090I\2\u04b3\u04c0\3\2\2\2\u04b4\u04b6\f"+
		"\4\2\2\u04b5\u04b7\5\4\3\2\u04b6\u04b5\3\2\2\2\u04b6\u04b7\3\2\2\2\u04b7"+
		"\u04b8\3\2\2\2\u04b8\u04ba\5\u0120\u0091\2\u04b9\u04bb\5\4\3\2\u04ba\u04b9"+
		"\3\2\2\2\u04ba\u04bb\3\2\2\2\u04bb\u04bc\3\2\2\2\u04bc\u04bd\5\u0090I"+
		"\2\u04bd\u04bf\3\2\2\2\u04be\u04b4\3\2\2\2\u04bf\u04c2\3\2\2\2\u04c0\u04be"+
		"\3\2\2\2\u04c0\u04c1\3\2\2\2\u04c1\u008f\3\2\2\2\u04c2\u04c0\3\2\2\2\u04c3"+
		"\u04cf\5\u0092J\2\u04c4\u04c6\5\4\3\2\u04c5\u04c4\3\2\2\2\u04c5\u04c6"+
		"\3\2\2\2\u04c6\u04c7\3\2\2\2\u04c7\u04c8\7\23\2\2\u04c8\u04c9\5\32\16"+
		"\2\u04c9\u04cb\7\23\2\2\u04ca\u04cc\5\4\3\2\u04cb\u04ca\3\2\2\2\u04cb"+
		"\u04cc\3\2\2\2\u04cc\u04cd\3\2\2\2\u04cd\u04ce\5\u0092J\2\u04ce\u04d0"+
		"\3\2\2\2\u04cf\u04c5\3\2\2\2\u04cf\u04d0\3\2\2\2\u04d0\u0091\3\2\2\2\u04d1"+
		"\u04db\5\u0094K\2\u04d2\u04d4\5\4\3\2\u04d3\u04d2\3\2\2\2\u04d3\u04d4"+
		"\3\2\2\2\u04d4\u04d5\3\2\2\2\u04d5\u04d7\5\u0124\u0093\2\u04d6\u04d8\5"+
		"\4\3\2\u04d7\u04d6\3\2\2\2\u04d7\u04d8\3\2\2\2\u04d8\u04d9\3\2\2\2\u04d9"+
		"\u04da\5\u0094K\2\u04da\u04dc\3\2\2\2\u04db\u04d3\3\2\2\2\u04db\u04dc"+
		"\3\2\2\2\u04dc\u0093\3\2\2\2\u04dd\u04de\6K\4\2\u04de\u04e0\5\u011a\u008e"+
		"\2\u04df\u04e1\5\4\3\2\u04e0\u04df\3\2\2\2\u04e0\u04e1\3\2\2\2\u04e1\u04e2"+
		"\3\2\2\2\u04e2\u04e3\5\u0094K\2\u04e3\u04e6\3\2\2\2\u04e4\u04e6\5\u0096"+
		"L\2\u04e5\u04dd\3\2\2\2\u04e5\u04e4\3\2\2\2\u04e6\u0095\3\2\2\2\u04e7"+
		"\u04f0\5\u00bc_\2\u04e8\u04ea\5\4\3\2\u04e9\u04e8\3\2\2\2\u04e9\u04ea"+
		"\3\2\2\2\u04ea\u04eb\3\2\2\2\u04eb\u04ed\7\6\2\2\u04ec\u04ee\5\4\3\2\u04ed"+
		"\u04ec\3\2\2\2\u04ed\u04ee\3\2\2\2\u04ee\u04ef\3\2\2\2\u04ef\u04f1\5\u010c"+
		"\u0087\2\u04f0\u04e9\3\2\2\2\u04f0\u04f1\3\2\2\2\u04f1\u0097\3\2\2\2\u04f2"+
		"\u0506\5\u009aN\2\u04f3\u0506\5\u009cO\2\u04f4\u0506\5\u009eP\2\u04f5"+
		"\u0506\5\u00a0Q\2\u04f6\u0506\5\u00acW\2\u04f7\u0506\5\u00aeX\2\u04f8"+
		"\u0506\5\u00a6T\2\u04f9\u0506\5\u00b0Y\2\u04fa\u0506\5\u00ba^\2\u04fb"+
		"\u0506\5\u00be`\2\u04fc\u0506\5\u00c0a\2\u04fd\u0506\5\u00c2b\2\u04fe"+
		"\u0506\5\u00a8U\2\u04ff\u0506\5\u00c4c\2\u0500\u0506\5\u00c6d\2\u0501"+
		"\u0506\5\u00a4S\2\u0502\u0506\5\u00b8]\2\u0503\u0506\5\u00aaV\2\u0504"+
		"\u0506\5\u00b6\\\2\u0505\u04f2\3\2\2\2\u0505\u04f3\3\2\2\2\u0505\u04f4"+
		"\3\2\2\2\u0505\u04f5\3\2\2\2\u0505\u04f6\3\2\2\2\u0505\u04f7\3\2\2\2\u0505"+
		"\u04f8\3\2\2\2\u0505\u04f9\3\2\2\2\u0505\u04fa\3\2\2\2\u0505\u04fb\3\2"+
		"\2\2\u0505\u04fc\3\2\2\2\u0505\u04fd\3\2\2\2\u0505\u04fe\3\2\2\2\u0505"+
		"\u04ff\3\2\2\2\u0505\u0500\3\2\2\2\u0505\u0501\3\2\2\2\u0505\u0502\3\2"+
		"\2\2\u0505\u0503\3\2\2\2\u0505\u0504\3\2\2\2\u0506\u0099\3\2\2\2\u0507"+
		"\u0508\7V\2\2\u0508\u0509\5\4\3\2\u0509\u050b\5\u00c8e\2\u050a\u050c\5"+
		"\4\3\2\u050b\u050a\3\2\2\2\u050b\u050c\3\2\2\2\u050c\u050d\3\2\2\2\u050d"+
		"\u050f\7\20\2\2\u050e\u0510\5\4\3\2\u050f\u050e\3\2\2\2\u050f\u0510\3"+
		"\2\2\2\u0510\u0511\3\2\2\2\u0511\u0513\5\u0084C\2\u0512\u0514\5\4\3\2"+
		"\u0513\u0512\3\2\2\2\u0513\u0514\3\2\2\2\u0514\u0515\3\2\2\2\u0515\u0517"+
		"\7K\2\2\u0516\u0518\5\4\3\2\u0517\u0516\3\2\2\2\u0517\u0518\3\2\2\2\u0518"+
		"\u0519\3\2\2\2\u0519\u051a\5\u0084C\2\u051a\u009b\3\2\2\2\u051b\u051d"+
		"\7^\2\2\u051c\u051e\5\4\3\2\u051d\u051c\3\2\2\2\u051d\u051e\3\2\2\2\u051e"+
		"\u051f\3\2\2\2\u051f\u0521\7\7\2\2\u0520\u0522\5\4\3\2\u0521\u0520\3\2"+
		"\2\2\u0521\u0522\3\2\2\2\u0522\u0523\3\2\2\2\u0523\u0525\5\u0084C\2\u0524"+
		"\u0526\5\4\3\2\u0525\u0524\3\2\2\2\u0525\u0526\3\2\2\2\u0526\u0527\3\2"+
		"\2\2\u0527\u0529\7\b\2\2\u0528\u052a\5\4\3\2\u0529\u0528\3\2\2\2\u0529"+
		"\u052a\3\2\2\2\u052a\u052b\3\2\2\2\u052b\u052c\5\u0084C\2\u052c\u052d"+
		"\5\4\3\2\u052d\u052e\7_\2\2\u052e\u052f\5\4\3\2\u052f\u0530\5\u0084C\2"+
		"\u0530\u009d\3\2\2\2\u0531\u0532\7Z\2\2\u0532\u0533\5\4\3\2\u0533\u0534"+
		"\5\u0084C\2\u0534\u0535\5\4\3\2\u0535\u0536\7[\2\2\u0536\u0537\5\4\3\2"+
		"\u0537\u0539\7\t\2\2\u0538\u053a\5\4\3\2\u0539\u0538\3\2\2\2\u0539\u053a"+
		"\3\2\2\2\u053a\u053b\3\2\2\2\u053b\u053d\5:\36\2\u053c\u053e\5\4\3\2\u053d"+
		"\u053c\3\2\2\2\u053d\u053e\3\2\2\2\u053e\u053f\3\2\2\2\u053f\u0540\7\n"+
		"\2\2\u0540\u009f\3\2\2\2\u0541\u0542\7Y\2\2\u0542\u0543\5\4\3\2\u0543"+
		"\u0545\7\t\2\2\u0544\u0546\5\4\3\2\u0545\u0544\3\2\2\2\u0545\u0546\3\2"+
		"\2\2\u0546\u0547\3\2\2\2\u0547\u0549\5> \2\u0548\u054a\5\4\3\2\u0549\u0548"+
		"\3\2\2\2\u0549\u054a\3\2\2\2\u054a\u054b\3\2\2\2\u054b\u054c\7\n\2\2\u054c"+
		"\u00a1\3\2\2\2\u054d\u055c\5\u0098M\2\u054e\u0550\5\4\3\2\u054f\u054e"+
		"\3\2\2\2\u054f\u0550\3\2\2\2\u0550\u0551\3\2\2\2\u0551\u0553\7\7\2\2\u0552"+
		"\u0554\5\4\3\2\u0553\u0552\3\2\2\2\u0553\u0554\3\2\2\2\u0554\u0556\3\2"+
		"\2\2\u0555\u0557\5\u0088E\2\u0556\u0555\3\2\2\2\u0556\u0557\3\2\2\2\u0557"+
		"\u0559\3\2\2\2\u0558\u055a\5\4\3\2\u0559\u0558\3\2\2\2\u0559\u055a\3\2"+
		"\2\2\u055a\u055b\3\2\2\2\u055b\u055d\7\b\2\2\u055c\u054f\3\2\2\2\u055c"+
		"\u055d\3\2\2\2\u055d\u00a3\3\2\2\2\u055e\u0560\5&\24\2\u055f\u0561\5\4"+
		"\3\2\u0560\u055f\3\2\2\2\u0560\u0561\3\2\2\2\u0561\u0562\3\2\2\2\u0562"+
		"\u0564\7\24\2\2\u0563\u0565\5\4\3\2\u0564\u0563\3\2\2\2\u0564\u0565\3"+
		"\2\2\2\u0565\u0566\3\2\2\2\u0566\u0567\5\u0084C\2\u0567\u00a5\3\2\2\2"+
		"\u0568\u056a\7\7\2\2\u0569\u056b\5\4\3\2\u056a\u0569\3\2\2\2\u056a\u056b"+
		"\3\2\2\2\u056b\u056c\3\2\2\2\u056c\u056e\5(\25\2\u056d\u056f\5\4\3\2\u056e"+
		"\u056d\3\2\2\2\u056e\u056f\3\2\2\2\u056f\u0570\3\2\2\2\u0570\u0572\7\b"+
		"\2\2\u0571\u0573\5\4\3\2\u0572\u0571\3\2\2\2\u0572\u0573\3\2\2\2\u0573"+
		"\u0574\3\2\2\2\u0574\u0576\7\24\2\2\u0575\u0577\5\4\3\2\u0576\u0575\3"+
		"\2\2\2\u0576\u0577\3\2\2\2\u0577\u0578\3\2\2\2\u0578\u0579\5\u0084C\2"+
		"\u0579\u00a7\3\2\2\2\u057a\u057b\5\u0104\u0083\2\u057b\u00a9\3\2\2\2\u057c"+
		"\u057d\5&\24\2\u057d\u00ab\3\2\2\2\u057e\u057f\5\32\16\2\u057f\u00ad\3"+
		"\2\2\2\u0580\u0581\5$\23\2\u0581\u0582\7\3\2\2\u0582\u0584\3\2\2\2\u0583"+
		"\u0580\3\2\2\2\u0583\u0584\3\2\2\2\u0584\u0585\3\2\2\2\u0585\u058a\5 "+
		"\21\2\u0586\u0588\5\4\3\2\u0587\u0586\3\2\2\2\u0587\u0588\3\2\2\2\u0588"+
		"\u0589\3\2\2\2\u0589\u058b\5\u00b0Y\2\u058a\u0587\3\2\2\2\u058a\u058b"+
		"\3\2\2\2\u058b\u00af\3\2\2\2\u058c\u058e\7\7\2\2\u058d\u058f\5\4\3\2\u058e"+
		"\u058d\3\2\2\2\u058e\u058f\3\2\2\2\u058f\u0591\3\2\2\2\u0590\u0592\5\u0088"+
		"E\2\u0591\u0590\3\2\2\2\u0591\u0592\3\2\2\2\u0592\u0594\3\2\2\2\u0593"+
		"\u0595\5\4\3\2\u0594\u0593\3\2\2\2\u0594\u0595\3\2\2\2\u0595\u0596\3\2"+
		"\2\2\u0596\u0597\7\b\2\2\u0597\u00b1\3\2\2\2\u0598\u059a\5\u0084C\2\u0599"+
		"\u059b\5\4\3\2\u059a\u0599\3\2\2\2\u059a\u059b\3\2\2\2\u059b\u059c\3\2"+
		"\2\2\u059c\u059e\7\24\2\2\u059d\u059f\5\4\3\2\u059e\u059d\3\2\2\2\u059e"+
		"\u059f\3\2\2\2\u059f\u05a0\3\2\2\2\u05a0\u05a1\5\u0084C\2\u05a1\u00b3"+
		"\3\2\2\2\u05a2\u05ad\5\u00b2Z\2\u05a3\u05a5\5\4\3\2\u05a4\u05a3\3\2\2"+
		"\2\u05a4\u05a5\3\2\2\2\u05a5\u05a6\3\2\2\2\u05a6\u05a8\7\5\2\2\u05a7\u05a9"+
		"\5\4\3\2\u05a8\u05a7\3\2\2\2\u05a8\u05a9\3\2\2\2\u05a9\u05aa\3\2\2\2\u05aa"+
		"\u05ac\5\u00b2Z\2\u05ab\u05a4\3\2\2\2\u05ac\u05af\3\2\2\2\u05ad\u05ab"+
		"\3\2\2\2\u05ad\u05ae\3\2\2\2\u05ae\u00b5\3\2\2\2\u05af\u05ad\3\2\2\2\u05b0"+
		"\u05b1\7\25\2\2\u05b1\u00b7\3\2\2\2\u05b2\u05b3\7\\\2\2\u05b3\u00b9\3"+
		"\2\2\2\u05b4\u05b5\7X\2\2\u05b5\u00bb\3\2\2\2\u05b6\u05bf\5\u00a2R\2\u05b7"+
		"\u05b9\5\4\3\2\u05b8\u05b7\3\2\2\2\u05b8\u05b9\3\2\2\2\u05b9\u05ba\3\2"+
		"\2\2\u05ba\u05bc\7\26\2\2\u05bb\u05bd\5\4\3\2\u05bc\u05bb\3\2\2\2\u05bc"+
		"\u05bd\3\2\2\2\u05bd\u05be\3\2\2\2\u05be\u05c0\5\u0084C\2\u05bf\u05b8"+
		"\3\2\2\2\u05bf\u05c0\3\2\2\2\u05c0\u00bd\3\2\2\2\u05c1\u05c3\7\27\2\2"+
		"\u05c2\u05c4\5\4\3\2\u05c3\u05c2\3\2\2\2\u05c3\u05c4\3\2\2\2\u05c4\u05c6"+
		"\3\2\2\2\u05c5\u05c7\5\u0088E\2\u05c6\u05c5\3\2\2\2\u05c6\u05c7\3\2\2"+
		"\2\u05c7\u05c9\3\2\2\2\u05c8\u05ca\5\4\3\2\u05c9\u05c8\3\2\2\2\u05c9\u05ca"+
		"\3\2\2\2\u05ca\u05cb\3\2\2\2\u05cb\u05cc\7\r\2\2\u05cc\u00bf\3\2\2\2\u05cd"+
		"\u05cf\7\30\2\2\u05ce\u05d0\5\4\3\2\u05cf\u05ce\3\2\2\2\u05cf\u05d0\3"+
		"\2\2\2\u05d0\u05d2\3\2\2\2\u05d1\u05d3\5\u0088E\2\u05d2\u05d1\3\2\2\2"+
		"\u05d2\u05d3\3\2\2\2\u05d3\u05d5\3\2\2\2\u05d4\u05d6\5\4\3\2\u05d5\u05d4"+
		"\3\2\2\2\u05d5\u05d6\3\2\2\2\u05d6\u05d7\3\2\2\2\u05d7\u05d8\7\n\2\2\u05d8"+
		"\u00c1\3\2\2\2\u05d9\u05db\7\31\2\2\u05da\u05dc\5\4\3\2\u05db\u05da\3"+
		"\2\2\2\u05db\u05dc\3\2\2\2\u05dc\u05de\3\2\2\2\u05dd\u05df\5\u00b4[\2"+
		"\u05de\u05dd\3\2\2\2\u05de\u05df\3\2\2\2\u05df\u05e1\3\2\2\2\u05e0\u05e2"+
		"\5\4\3\2\u05e1\u05e0\3\2\2\2\u05e1\u05e2\3\2\2\2\u05e2\u05e3\3\2\2\2\u05e3"+
		"\u05e4\7\n\2\2\u05e4\u00c3\3\2\2\2\u05e5\u05e7\t\5\2\2\u05e6\u05e8\5\4"+
		"\3\2\u05e7\u05e6\3\2\2\2\u05e7\u05e8\3\2\2\2\u05e8\u05e9\3\2\2\2\u05e9"+
		"\u05eb\5.\30\2\u05ea\u05ec\5\4\3\2\u05eb\u05ea\3\2\2\2\u05eb\u05ec\3\2"+
		"\2\2\u05ec\u05ed\3\2\2\2\u05ed\u05ef\7\3\2\2\u05ee\u05f0\5\4\3\2\u05ef"+
		"\u05ee\3\2\2\2\u05ef\u05f0\3\2\2\2\u05f0\u05f1\3\2\2\2\u05f1\u05f2\5\u0084"+
		"C\2\u05f2\u00c5\3\2\2\2\u05f3\u05f5\t\6\2\2\u05f4\u05f6\5\4\3\2\u05f5"+
		"\u05f4\3\2\2\2\u05f5\u05f6\3\2\2\2\u05f6\u05f7\3\2\2\2\u05f7\u05f9\5."+
		"\30\2\u05f8\u05fa\5\4\3\2\u05f9\u05f8\3\2\2\2\u05f9\u05fa\3\2\2\2\u05fa"+
		"\u05fb\3\2\2\2\u05fb\u05fd\7\3\2\2\u05fc\u05fe\5\4\3\2\u05fd\u05fc\3\2"+
		"\2\2\u05fd\u05fe\3\2\2\2\u05fe\u05ff\3\2\2\2\u05ff\u0600\5\u0084C\2\u0600"+
		"\u00c7\3\2\2\2\u0601\u0602\5\u00dep\2\u0602\u00c9\3\2\2\2\u0603\u060e"+
		"\5\u00c8e\2\u0604\u0606\5\4\3\2\u0605\u0604\3\2\2\2\u0605\u0606\3\2\2"+
		"\2\u0606\u0607\3\2\2\2\u0607\u0609\7\5\2\2\u0608\u060a\5\4\3\2\u0609\u0608"+
		"\3\2\2\2\u0609\u060a\3\2\2\2\u060a\u060b\3\2\2\2\u060b\u060d\5\u00c8e"+
		"\2\u060c\u0605\3\2\2\2\u060d\u0610\3\2\2\2\u060e\u060c\3\2\2\2\u060e\u060f"+
		"\3\2\2\2\u060f\u00cb\3\2\2\2\u0610\u060e\3\2\2\2\u0611\u061b\5\u00dan"+
		"\2\u0612\u061b\5\u00d2j\2\u0613\u061b\5\u00dco\2\u0614\u061b\5\u00d8m"+
		"\2\u0615\u061b\5\u00d4k\2\u0616\u061b\5\u00d6l\2\u0617\u061b\5\u00e0q"+
		"\2\u0618\u061b\5\u00e2r\2\u0619\u061b\5\u00e4s\2\u061a\u0611\3\2\2\2\u061a"+
		"\u0612\3\2\2\2\u061a\u0613\3\2\2\2\u061a\u0614\3\2\2\2\u061a\u0615\3\2"+
		"\2\2\u061a\u0616\3\2\2\2\u061a\u0617\3\2\2\2\u061a\u0618\3\2\2\2\u061a"+
		"\u0619\3\2\2\2\u061b\u00cd\3\2\2\2\u061c\u061e\5\u00c8e\2\u061d\u061f"+
		"\5\4\3\2\u061e\u061d\3\2\2\2\u061e\u061f\3\2\2\2\u061f\u0620\3\2\2\2\u0620"+
		"\u0622\7\24\2\2\u0621\u0623\5\4\3\2\u0622\u0621\3\2\2\2\u0622\u0623\3"+
		"\2\2\2\u0623\u0624\3\2\2\2\u0624\u0625\5\u00c8e\2\u0625\u00cf\3\2\2\2"+
		"\u0626\u0631\5\u00ceh\2\u0627\u0629\5\4\3\2\u0628\u0627\3\2\2\2\u0628"+
		"\u0629\3\2\2\2\u0629\u062a\3\2\2\2\u062a\u062c\7\5\2\2\u062b\u062d\5\4"+
		"\3\2\u062c\u062b\3\2\2\2\u062c\u062d\3\2\2\2\u062d\u062e\3\2\2\2\u062e"+
		"\u0630\5\u00ceh\2\u062f\u0628\3\2\2\2\u0630\u0633\3\2\2\2\u0631\u062f"+
		"\3\2\2\2\u0631\u0632\3\2\2\2\u0632\u00d1\3\2\2\2\u0633\u0631\3\2\2\2\u0634"+
		"\u0635\5\u0104\u0083\2\u0635\u00d3\3\2\2\2\u0636\u0637\5$\23\2\u0637\u0638"+
		"\7\3\2\2\u0638\u063a\3\2\2\2\u0639\u0636\3\2\2\2\u0639\u063a\3\2\2\2\u063a"+
		"\u063b\3\2\2\2\u063b\u0640\5 \21\2\u063c\u063e\5\4\3\2\u063d\u063c\3\2"+
		"\2\2\u063d\u063e\3\2\2\2\u063e\u063f\3\2\2\2\u063f\u0641\5\u00c8e\2\u0640"+
		"\u063d\3\2\2\2\u0640\u0641\3\2\2\2\u0641\u00d5\3\2\2\2\u0642\u0644\7\7"+
		"\2\2\u0643\u0645\5\4\3\2\u0644\u0643\3\2\2\2\u0644\u0645\3\2\2\2\u0645"+
		"\u0647\3\2\2\2\u0646\u0648\5\u00caf\2\u0647\u0646\3\2\2\2\u0647\u0648"+
		"\3\2\2\2\u0648\u064a\3\2\2\2\u0649\u064b\5\4\3\2\u064a\u0649\3\2\2\2\u064a"+
		"\u064b\3\2\2\2\u064b\u064c\3\2\2\2\u064c\u064d\7\b\2\2\u064d\u00d7\3\2"+
		"\2\2\u064e\u064f\7\\\2\2\u064f\u00d9\3\2\2\2\u0650\u0651\7X\2\2\u0651"+
		"\u00db\3\2\2\2\u0652\u0653\5&\24\2\u0653\u00dd\3\2\2\2\u0654\u065d\5\u00cc"+
		"g\2\u0655\u0657\5\4\3\2\u0656\u0655\3\2\2\2\u0656\u0657\3\2\2\2\u0657"+
		"\u0658\3\2\2\2\u0658\u065a\7\26\2\2\u0659\u065b\5\4\3\2\u065a\u0659\3"+
		"\2\2\2\u065a\u065b\3\2\2\2\u065b\u065c\3\2\2\2\u065c\u065e\5\u00c8e\2"+
		"\u065d\u0656\3\2\2\2\u065d\u065e\3\2\2\2\u065e\u00df\3\2\2\2\u065f\u0661"+
		"\7\27\2\2\u0660\u0662\5\4\3\2\u0661\u0660\3\2\2\2\u0661\u0662\3\2\2\2"+
		"\u0662\u0664\3\2\2\2\u0663\u0665\5\u00caf\2\u0664\u0663\3\2\2\2\u0664"+
		"\u0665\3\2\2\2\u0665\u0670\3\2\2\2\u0666\u0668\5\4\3\2\u0667\u0666\3\2"+
		"\2\2\u0667\u0668\3\2\2\2\u0668\u0669\3\2\2\2\u0669\u066b\7\5\2\2\u066a"+
		"\u066c\5\4\3\2\u066b\u066a\3\2\2\2\u066b\u066c\3\2\2\2\u066c\u066d\3\2"+
		"\2\2\u066d\u066e\5\u00c8e\2\u066e\u066f\7\36\2\2\u066f\u0671\3\2\2\2\u0670"+
		"\u0667\3\2\2\2\u0670\u0671\3\2\2\2\u0671\u0673\3\2\2\2\u0672\u0674\5\4"+
		"\3\2\u0673\u0672\3\2\2\2\u0673\u0674\3\2\2\2\u0674\u0675\3\2\2\2\u0675"+
		"\u0676\7\r\2\2\u0676\u00e1\3\2\2\2\u0677\u0679\7\30\2\2\u0678\u067a\5"+
		"\4\3\2\u0679\u0678\3\2\2\2\u0679\u067a\3\2\2\2\u067a\u067c\3\2\2\2\u067b"+
		"\u067d\5\u00caf\2\u067c\u067b\3\2\2\2\u067c\u067d\3\2\2\2\u067d\u0688"+
		"\3\2\2\2\u067e\u0680\5\4\3\2\u067f\u067e\3\2\2\2\u067f\u0680\3\2\2\2\u0680"+
		"\u0681\3\2\2\2\u0681\u0683\7\5\2\2\u0682\u0684\5\4\3\2\u0683\u0682\3\2"+
		"\2\2\u0683\u0684\3\2\2\2\u0684\u0685\3\2\2\2\u0685\u0686\5\u00c8e\2\u0686"+
		"\u0687\7\36\2\2\u0687\u0689\3\2\2\2\u0688\u067f\3\2\2\2\u0688\u0689\3"+
		"\2\2\2\u0689\u068b\3\2\2\2\u068a\u068c\5\4\3\2\u068b\u068a\3\2\2\2\u068b"+
		"\u068c\3\2\2\2\u068c\u068d\3\2\2\2\u068d\u068e\7\n\2\2\u068e\u00e3\3\2"+
		"\2\2\u068f\u0691\7\31\2\2\u0690\u0692\5\4\3\2\u0691\u0690\3\2\2\2\u0691"+
		"\u0692\3\2\2\2\u0692\u0694\3\2\2\2\u0693\u0695\5\u00d0i\2\u0694\u0693"+
		"\3\2\2\2\u0694\u0695\3\2\2\2\u0695\u06a0\3\2\2\2\u0696\u0698\5\4\3\2\u0697"+
		"\u0696\3\2\2\2\u0697\u0698\3\2\2\2\u0698\u0699\3\2\2\2\u0699\u069b\7\5"+
		"\2\2\u069a\u069c\5\4\3\2\u069b\u069a\3\2\2\2\u069b\u069c\3\2\2\2\u069c"+
		"\u069d\3\2\2\2\u069d\u069e\5\u00c8e\2\u069e\u069f\7\36\2\2\u069f\u06a1"+
		"\3\2\2\2\u06a0\u0697\3\2\2\2\u06a0\u06a1\3\2\2\2\u06a1\u06a3\3\2\2\2\u06a2"+
		"\u06a4\5\4\3\2\u06a3\u06a2\3\2\2\2\u06a3\u06a4\3\2\2\2\u06a4\u06a5\3\2"+
		"\2\2\u06a5\u06a6\7\n\2\2\u06a6\u00e5\3\2\2\2\u06a7\u06a8\t\7\2\2\u06a8"+
		"\u00e7\3\2\2\2\u06a9\u06aa\7c\2\2\u06aa\u00e9\3\2\2\2\u06ab\u06ac\7d\2"+
		"\2\u06ac\u00eb\3\2\2\2\u06ad\u06ae\7!\2\2\u06ae\u00ed\3\2\2\2\u06af\u06b1"+
		"\5\u00ecw\2\u06b0\u06af\3\2\2\2\u06b0\u06b1\3\2\2\2\u06b1\u06b2\3\2\2"+
		"\2\u06b2\u06b3\7e\2\2\u06b3\u06b4\7\3\2\2\u06b4\u06b5\7e\2\2\u06b5\u06b6"+
		"\7\"\2\2\u06b6\u00ef\3\2\2\2\u06b7\u06b9\5\u00ecw\2\u06b8\u06b7\3\2\2"+
		"\2\u06b8\u06b9\3\2\2\2\u06b9\u06ba\3\2\2\2\u06ba\u06bb\7e\2\2\u06bb\u06bc"+
		"\7\3\2\2\u06bc\u06bd\7e\2\2\u06bd\u06be\7#\2\2\u06be\u00f1\3\2\2\2\u06bf"+
		"\u06c1\5\u00ecw\2\u06c0\u06bf\3\2\2\2\u06c0\u06c1\3\2\2\2\u06c1\u06c2"+
		"\3\2\2\2\u06c2\u06c3\7e\2\2\u06c3\u06c4\7\3\2\2\u06c4\u06c5\7e\2\2\u06c5"+
		"\u00f3\3\2\2\2\u06c6\u06ca\5\u00eex\2\u06c7\u06ca\5\u00f0y\2\u06c8\u06ca"+
		"\5\u00f2z\2\u06c9\u06c6\3\2\2\2\u06c9\u06c7\3\2\2\2\u06c9\u06c8\3\2\2"+
		"\2\u06ca\u00f5\3\2\2\2\u06cb\u06cd\5\u00ecw\2\u06cc\u06cb\3\2\2\2\u06cc"+
		"\u06cd\3\2\2\2\u06cd\u06ce\3\2\2\2\u06ce\u06cf\7e\2\2\u06cf\u06d0\7$\2"+
		"\2\u06d0\u00f7\3\2\2\2\u06d1\u06d3\5\u00ecw\2\u06d2\u06d1\3\2\2\2\u06d2"+
		"\u06d3\3\2\2\2\u06d3\u06d4\3\2\2\2\u06d4\u06d5\7e\2\2\u06d5\u06d6\7%\2"+
		"\2\u06d6\u00f9\3\2\2\2\u06d7\u06d9\5\u00ecw\2\u06d8\u06d7\3\2\2\2\u06d8"+
		"\u06d9\3\2\2\2\u06d9\u06da\3\2\2\2\u06da\u06db\7e\2\2\u06db\u06dc\7&\2"+
		"\2\u06dc\u00fb\3\2\2\2\u06dd\u06df\5\u00ecw\2\u06de\u06dd\3\2\2\2\u06de"+
		"\u06df\3\2\2\2\u06df\u06e0\3\2\2\2\u06e0\u06e1\7e\2\2\u06e1\u06e2\7\'"+
		"\2\2\u06e2\u00fd\3\2\2\2\u06e3\u06e5\5\u00ecw\2\u06e4\u06e3\3\2\2\2\u06e4"+
		"\u06e5\3\2\2\2\u06e5\u06e6\3\2\2\2\u06e6\u06e7\7e\2\2\u06e7\u06e8\7(\2"+
		"\2\u06e8\u00ff\3\2\2\2\u06e9\u06eb\5\u00ecw\2\u06ea\u06e9\3\2\2\2\u06ea"+
		"\u06eb\3\2\2\2\u06eb\u06ec\3\2\2\2\u06ec\u06ed\7e\2\2\u06ed\u0101\3\2"+
		"\2\2\u06ee\u06f5\5\u00f6|\2\u06ef\u06f5\5\u00f8}\2\u06f0\u06f5\5\u00fa"+
		"~\2\u06f1\u06f5\5\u00fc\177\2\u06f2\u06f5\5\u00fe\u0080\2\u06f3\u06f5"+
		"\5\u0100\u0081\2\u06f4\u06ee\3\2\2\2\u06f4\u06ef\3\2\2\2\u06f4\u06f0\3"+
		"\2\2\2\u06f4\u06f1\3\2\2\2\u06f4\u06f2\3\2\2\2\u06f4\u06f3\3\2\2\2\u06f5"+
		"\u0103\3\2\2\2\u06f6\u06fc\5\u00e6t\2\u06f7\u06fc\5\u00e8u\2\u06f8\u06fc"+
		"\5\u00f4{\2\u06f9\u06fc\5\u0102\u0082\2\u06fa\u06fc\5\u00eav\2\u06fb\u06f6"+
		"\3\2\2\2\u06fb\u06f7\3\2\2\2\u06fb\u06f8\3\2\2\2\u06fb\u06f9\3\2\2\2\u06fb"+
		"\u06fa\3\2\2\2\u06fc\u0105\3\2\2\2\u06fd\u0703\5\u010e\u0088\2\u06fe\u0703"+
		"\5\u0116\u008c\2\u06ff\u0703\5\u0118\u008d\2\u0700\u0703\5\u0108\u0085"+
		"\2\u0701\u0703\5\u010a\u0086\2\u0702\u06fd\3\2\2\2\u0702\u06fe\3\2\2\2"+
		"\u0702\u06ff\3\2\2\2\u0702\u0700\3\2\2\2\u0702\u0701\3\2\2\2\u0703\u0107"+
		"\3\2\2\2\u0704\u0705\5&\24\2\u0705\u0109\3\2\2\2\u0706\u0707\5$\23\2\u0707"+
		"\u010b\3\2\2\2\u0708\u0711\5\u0106\u0084\2\u0709\u070b\5\4\3\2\u070a\u0709"+
		"\3\2\2\2\u070a\u070b\3\2\2\2\u070b\u070c\3\2\2\2\u070c\u070e\7\24\2\2"+
		"\u070d\u070f\5\4\3\2\u070e\u070d\3\2\2\2\u070e\u070f\3\2\2\2\u070f\u0710"+
		"\3\2\2\2\u0710\u0712\5\u010c\u0087\2\u0711\u070a\3\2\2\2\u0711\u0712\3"+
		"\2\2\2\u0712\u010d\3\2\2\2\u0713\u0715\7\7\2\2\u0714\u0716\5\4\3\2\u0715"+
		"\u0714\3\2\2\2\u0715\u0716\3\2\2\2\u0716\u0717\3\2\2\2\u0717\u0722\5\u010c"+
		"\u0087\2\u0718\u071a\5\4\3\2\u0719\u0718\3\2\2\2\u0719\u071a\3\2\2\2\u071a"+
		"\u071b\3\2\2\2\u071b\u071d\7\5\2\2\u071c\u071e\5\4\3\2\u071d\u071c\3\2"+
		"\2\2\u071d\u071e\3\2\2\2\u071e\u071f\3\2\2\2\u071f\u0721\5\u010c\u0087"+
		"\2\u0720\u0719\3\2\2\2\u0721\u0724\3\2\2\2\u0722\u0720\3\2\2\2\u0722\u0723"+
		"\3\2\2\2\u0723\u0726\3\2\2\2\u0724\u0722\3\2\2\2\u0725\u0727\5\4\3\2\u0726"+
		"\u0725\3\2\2\2\u0726\u0727\3\2\2\2\u0727\u0728\3\2\2\2\u0728\u072a\7\b"+
		"\2\2\u0729\u072b\5\4\3\2\u072a\u0729\3\2\2\2\u072a\u072b\3\2\2\2\u072b"+
		"\u072c\3\2\2\2\u072c\u072e\7\24\2\2\u072d\u072f\5\4\3\2\u072e\u072d\3"+
		"\2\2\2\u072e\u072f\3\2\2\2\u072f\u0730\3\2\2\2\u0730\u0731\5\u010c\u0087"+
		"\2\u0731\u010f\3\2\2\2\u0732\u0733\7\7\2\2\u0733\u0734\7\b\2\2\u0734\u0111"+
		"\3\2\2\2\u0735\u0737\7\7\2\2\u0736\u0738\5\4\3\2\u0737\u0736\3\2\2\2\u0737"+
		"\u0738\3\2\2\2\u0738\u0739\3\2\2\2\u0739\u073b\5\u010c\u0087\2\u073a\u073c"+
		"\5\4\3\2\u073b\u073a\3\2\2\2\u073b\u073c\3\2\2\2\u073c\u073d\3\2\2\2\u073d"+
		"\u073e\7\b\2\2\u073e\u0113\3\2\2\2\u073f\u0741\7\7\2\2\u0740\u0742\5\4"+
		"\3\2\u0741\u0740\3\2\2\2\u0741\u0742\3\2\2\2\u0742\u0743\3\2\2\2\u0743"+
		"\u074c\5\u010c\u0087\2\u0744\u0746\5\4\3\2\u0745\u0744\3\2\2\2\u0745\u0746"+
		"\3\2\2\2\u0746\u0747\3\2\2\2\u0747\u0749\7\5\2\2\u0748\u074a\5\4\3\2\u0749"+
		"\u0748\3\2\2\2\u0749\u074a\3\2\2\2\u074a\u074b\3\2\2\2\u074b\u074d\5\u010c"+
		"\u0087\2\u074c\u0745\3\2\2\2\u074d\u074e\3\2\2\2\u074e\u074c\3\2\2\2\u074e"+
		"\u074f\3\2\2\2\u074f\u0751\3\2\2\2\u0750\u0752\5\4\3\2\u0751\u0750\3\2"+
		"\2\2\u0751\u0752\3\2\2\2\u0752\u0753\3\2\2\2\u0753\u0754\7\b\2\2\u0754"+
		"\u0115\3\2\2\2\u0755\u0759\5\u0110\u0089\2\u0756\u0759\5\u0112\u008a\2"+
		"\u0757\u0759\5\u0114\u008b\2\u0758\u0755\3\2\2\2\u0758\u0756\3\2\2\2\u0758"+
		"\u0757\3\2\2\2\u0759\u0117\3\2\2\2\u075a\u075c\5\u010a\u0086\2\u075b\u075d"+
		"\5\4\3\2\u075c\u075b\3\2\2\2\u075c\u075d\3\2\2\2\u075d\u075e\3\2\2\2\u075e"+
		"\u0760\7\f\2\2\u075f\u0761\5\4\3\2\u0760\u075f\3\2\2\2\u0760\u0761\3\2"+
		"\2\2\u0761\u0762\3\2\2\2\u0762\u076d\5\u010c\u0087\2\u0763\u0765\5\4\3"+
		"\2\u0764\u0763\3\2\2\2\u0764\u0765\3\2\2\2\u0765\u0766\3\2\2\2\u0766\u0768"+
		"\7\5\2\2\u0767\u0769\5\4\3\2\u0768\u0767\3\2\2\2\u0768\u0769\3\2\2\2\u0769"+
		"\u076a\3\2\2\2\u076a\u076c\5\u010c\u0087\2\u076b\u0764\3\2\2\2\u076c\u076f"+
		"\3\2\2\2\u076d\u076b\3\2\2\2\u076d\u076e\3\2\2\2\u076e\u0771\3\2\2\2\u076f"+
		"\u076d\3\2\2\2\u0770\u0772\5\4\3\2\u0771\u0770\3\2\2\2\u0771\u0772\3\2"+
		"\2\2\u0772\u0773\3\2\2\2\u0773\u0775\7\r\2\2\u0774\u0776\5\4\3\2\u0775"+
		"\u0774\3\2\2\2\u0775\u0776\3\2\2\2\u0776\u0119\3\2\2\2\u0777\u0778\t\b"+
		"\2\2\u0778\u011b\3\2\2\2\u0779\u077a\t\t\2\2\u077a\u011d\3\2\2\2\u077b"+
		"\u077c\t\n\2\2\u077c\u011f\3\2\2\2\u077d\u077e\t\13\2\2\u077e\u0121\3"+
		"\2\2\2\u077f\u0780\t\f\2\2\u0780\u0123\3\2\2\2\u0781\u0782\t\r\2\2\u0782"+
		"\u0125\3\2\2\2\u0783\u078a\5\u012a\u0096\2\u0784\u078a\5\u012c\u0097\2"+
		"\u0785\u078a\5\u012e\u0098\2\u0786\u078a\5\u0132\u009a\2\u0787\u078a\5"+
		"\u0130\u0099\2\u0788\u078a\5\u0134\u009b\2\u0789\u0783\3\2\2\2\u0789\u0784"+
		"\3\2\2\2\u0789\u0785\3\2\2\2\u0789\u0786\3\2\2\2\u0789\u0787\3\2\2\2\u0789"+
		"\u0788\3\2\2\2\u078a\u0127\3\2\2\2\u078b\u0796\5\u0126\u0094\2\u078c\u078e"+
		"\5\4\3\2\u078d\u078c\3\2\2\2\u078d\u078e\3\2\2\2\u078e\u078f\3\2\2\2\u078f"+
		"\u0791\7\5\2\2\u0790\u0792\5\4\3\2\u0791\u0790\3\2\2\2\u0791\u0792\3\2"+
		"\2\2\u0792\u0793\3\2\2\2\u0793\u0795\5\u0126\u0094\2\u0794\u078d\3\2\2"+
		"\2\u0795\u0798\3\2\2\2\u0796\u0794\3\2\2\2\u0796\u0797\3\2\2\2\u0797\u0129"+
		"\3\2\2\2\u0798\u0796\3\2\2\2\u0799\u079a\7\37\2\2\u079a\u012b\3\2\2\2"+
		"\u079b\u079c\7 \2\2\u079c\u012d\3\2\2\2\u079d\u079f\5\32\16\2\u079e\u07a0"+
		"\5\4\3\2\u079f\u079e\3\2\2\2\u079f\u07a0\3\2\2\2\u07a0\u07a1\3\2\2\2\u07a1"+
		"\u07a2\7\7\2\2\u07a2\u07a3\5\u0088E\2\u07a3\u07a4\7\b\2\2\u07a4\u012f"+
		"\3\2\2\2\u07a5\u07a7\5\36\20\2\u07a6\u07a8\5\4\3\2\u07a7\u07a6\3\2\2\2"+
		"\u07a7\u07a8\3\2\2\2\u07a8\u07a9\3\2\2\2\u07a9\u07aa\7\7\2\2\u07aa\u07ab"+
		"\5\u0088E\2\u07ab\u07ac\7\b\2\2\u07ac\u0131\3\2\2\2\u07ad\u07af\5&\24"+
		"\2\u07ae\u07b0\5\4\3\2\u07af\u07ae\3\2\2\2\u07af\u07b0\3\2\2\2\u07b0\u07b1"+
		"\3\2\2\2\u07b1\u07b3\7>\2\2\u07b2\u07b4\5\4\3\2\u07b3\u07b2\3\2\2\2\u07b3"+
		"\u07b4\3\2\2\2\u07b4\u07b5\3\2\2\2\u07b5\u07b6\5&\24\2\u07b6\u0133\3\2"+
		"\2\2\u07b7\u07b9\5&\24\2\u07b8\u07ba\5\4\3\2\u07b9\u07b8\3\2\2\2\u07b9"+
		"\u07ba\3\2\2\2\u07ba\u07bb\3\2\2\2\u07bb\u07bd\7H\2\2\u07bc\u07be\5\4"+
		"\3\2\u07bd\u07bc\3\2\2\2\u07bd\u07be\3\2\2\2\u07be\u07bf\3\2\2\2\u07bf"+
		"\u07c0\5\u0084C\2\u07c0\u0135\3\2\2\2\u014b\u013b\u0140\u0146\u014a\u014f"+
		"\u0152\u015b\u0161\u0168\u0184\u0188\u018d\u0193\u0199\u019d\u01a2\u01a7"+
		"\u01aa\u01ad\u01b0\u01b4\u01b8\u01be\u01c2\u01c7\u01cc\u01d0\u01d4\u01d9"+
		"\u01dc\u01df\u01e5\u01e9\u01ee\u01f5\u01f9\u01fd\u0201\u0206\u020d\u0211"+
		"\u0215\u0219\u021e\u0223\u0227\u022a\u022e\u0232\u0236\u023b\u0240\u0245"+
		"\u0249\u024e\u0258\u025c\u0261\u0265\u0269\u026c\u026f\u0273\u0276\u0281"+
		"\u0285\u028a\u02ad\u02b0\u02b6\u02ba\u02bf\u02c3\u02c9\u02ce\u02d2\u02d9"+
		"\u02dd\u02e1\u02e8\u02ec\u02f1\u02f8\u02fb\u0300\u0304\u030a\u030e\u0311"+
		"\u0314\u031a\u031f\u0323\u0329\u032d\u0330\u0333\u0339\u033f\u0343\u0346"+
		"\u0349\u034f\u0354\u0358\u035e\u0362\u0366\u036c\u0371\u0375\u0379\u037f"+
		"\u0383\u0387\u038d\u0392\u0396\u0399\u039c\u03a2\u03a7\u03ab\u03af\u03b3"+
		"\u03b9\u03be\u03c2\u03c8\u03cc\u03d0\u03d4\u03d8\u03dc\u03e2\u03e7\u03eb"+
		"\u03f3\u03f9\u03fd\u0401\u0409\u040c\u0410\u0415\u0419\u041d\u0421\u0428"+
		"\u042c\u0431\u0435\u0439\u043d\u0443\u0448\u044c\u0453\u0457\u045d\u0462"+
		"\u0466\u046c\u0470\u0474\u0477\u047b\u047f\u0483\u0487\u048b\u0490\u0495"+
		"\u0499\u049d\u04a4\u04a8\u04ae\u04b6\u04ba\u04c0\u04c5\u04cb\u04cf\u04d3"+
		"\u04d7\u04db\u04e0\u04e5\u04e9\u04ed\u04f0\u0505\u050b\u050f\u0513\u0517"+
		"\u051d\u0521\u0525\u0529\u0539\u053d\u0545\u0549\u054f\u0553\u0556\u0559"+
		"\u055c\u0560\u0564\u056a\u056e\u0572\u0576\u0583\u0587\u058a\u058e\u0591"+
		"\u0594\u059a\u059e\u05a4\u05a8\u05ad\u05b8\u05bc\u05bf\u05c3\u05c6\u05c9"+
		"\u05cf\u05d2\u05d5\u05db\u05de\u05e1\u05e7\u05eb\u05ef\u05f5\u05f9\u05fd"+
		"\u0605\u0609\u060e\u061a\u061e\u0622\u0628\u062c\u0631\u0639\u063d\u0640"+
		"\u0644\u0647\u064a\u0656\u065a\u065d\u0661\u0664\u0667\u066b\u0670\u0673"+
		"\u0679\u067c\u067f\u0683\u0688\u068b\u0691\u0694\u0697\u069b\u06a0\u06a3"+
		"\u06b0\u06b8\u06c0\u06c9\u06cc\u06d2\u06d8\u06de\u06e4\u06ea\u06f4\u06fb"+
		"\u0702\u070a\u070e\u0711\u0715\u0719\u071d\u0722\u0726\u072a\u072e\u0737"+
		"\u073b\u0741\u0745\u0749\u074e\u0751\u0758\u075c\u0760\u0764\u0768\u076d"+
		"\u0771\u0775\u0789\u078d\u0791\u0796\u079f\u07a7\u07af\u07b3\u07b9\u07bd";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}