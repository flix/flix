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
		T__66=67, T__67=68, T__68=69, T__69=70, T__70=71, T__71=72, T__72=73, 
		T__73=74, T__74=75, T__75=76, T__76=77, T__77=78, T__78=79, T__79=80, 
		T__80=81, T__81=82, T__82=83, T__83=84, T__84=85, T__85=86, T__86=87, 
		T__87=88, T__88=89, T__89=90, WS=91, SC=92, Comment=93, Ident=94, FNone=95, 
		FNil=96, Bot=97, Top=98, Wild=99, UserError=100, Chars=101, Strs=102, 
		Digits=103;
	public static final int
		RULE_start = 0, RULE_optSC = 1, RULE_nname = 2, RULE_qname = 3, RULE_annotation = 4, 
		RULE_argument = 5, RULE_arguments = 6, RULE_params = 7, RULE_attribute = 8, 
		RULE_attributes = 9, RULE_index = 10, RULE_indexes = 11, RULE_idents = 12, 
		RULE_match_rule = 13, RULE_match_rules = 14, RULE_switch_rule = 15, RULE_switch_rules = 16, 
		RULE_typeparam = 17, RULE_typeparams = 18, RULE_class_typeparams = 19, 
		RULE_contextBound = 20, RULE_contextBounds = 21, RULE_contextBoundsList = 22, 
		RULE_functions = 23, RULE_s_import = 24, RULE_s_imports = 25, RULE_import_wildcard = 26, 
		RULE_import_definition = 27, RULE_import_namespace = 28, RULE_decl = 29, 
		RULE_decls = 30, RULE_decls_namespace = 31, RULE_decls_enum = 32, RULE_cases = 33, 
		RULE_decls_relation = 34, RULE_decls_lattice = 35, RULE_decls_index = 36, 
		RULE_decls_signature = 37, RULE_decls_external = 38, RULE_decls_function = 39, 
		RULE_decls_law = 40, RULE_decls_class = 41, RULE_class_body = 42, RULE_decls_fact = 43, 
		RULE_decls_rule = 44, RULE_elms = 45, RULE_decls_letlattice = 46, RULE_expression = 47, 
		RULE_expressions = 48, RULE_comparison = 49, RULE_additive = 50, RULE_multiplicative = 51, 
		RULE_infix = 52, RULE_extended = 53, RULE_unary = 54, RULE_ascribe = 55, 
		RULE_e_primary = 56, RULE_e_letMatch = 57, RULE_e_ifThenElse = 58, RULE_e_match = 59, 
		RULE_e_switch = 60, RULE_e_apply = 61, RULE_e_var = 62, RULE_e_tag = 63, 
		RULE_e_tuple = 64, RULE_e_keyValue = 65, RULE_e_keyValues = 66, RULE_e_wild = 67, 
		RULE_e_fNil = 68, RULE_e_fNone = 69, RULE_e_fSome = 70, RULE_e_fList = 71, 
		RULE_e_fVec = 72, RULE_e_fSet = 73, RULE_e_fMap = 74, RULE_e_unaryLambda = 75, 
		RULE_e_lambda = 76, RULE_existential = 77, RULE_universal = 78, RULE_pattern = 79, 
		RULE_patterns = 80, RULE_simple = 81, RULE_p_keyValue = 82, RULE_p_keyValues = 83, 
		RULE_p_tag = 84, RULE_p_tuple = 85, RULE_p_fNil = 86, RULE_p_fNone = 87, 
		RULE_p_fVec = 88, RULE_p_fSet = 89, RULE_p_fMap = 90, RULE_bools = 91, 
		RULE_negative = 92, RULE_float32 = 93, RULE_float64 = 94, RULE_floatDefault = 95, 
		RULE_floats = 96, RULE_int8 = 97, RULE_int16 = 98, RULE_int32 = 99, RULE_int64 = 100, 
		RULE_bigInt = 101, RULE_intDefault = 102, RULE_ints = 103, RULE_literals = 104, 
		RULE_primary = 105, RULE_type = 106, RULE_lambda = 107, RULE_tuple_unit = 108, 
		RULE_tuple_singleton = 109, RULE_tuple_multi = 110, RULE_tuple = 111, 
		RULE_parametric = 112, RULE_unary_ops = 113, RULE_logical_ops = 114, RULE_comparison_ops = 115, 
		RULE_multipve_ops = 116, RULE_addve_ops = 117, RULE_extbin_ops = 118, 
		RULE_predicate = 119, RULE_predicates = 120, RULE_pred_true = 121, RULE_pred_false = 122, 
		RULE_pred_ambiguous = 123, RULE_pred_equal = 124, RULE_pred_notequal = 125, 
		RULE_pred_loop = 126;
	public static final String[] ruleNames = {
		"start", "optSC", "nname", "qname", "annotation", "argument", "arguments", 
		"params", "attribute", "attributes", "index", "indexes", "idents", "match_rule", 
		"match_rules", "switch_rule", "switch_rules", "typeparam", "typeparams", 
		"class_typeparams", "contextBound", "contextBounds", "contextBoundsList", 
		"functions", "s_import", "s_imports", "import_wildcard", "import_definition", 
		"import_namespace", "decl", "decls", "decls_namespace", "decls_enum", 
		"cases", "decls_relation", "decls_lattice", "decls_index", "decls_signature", 
		"decls_external", "decls_function", "decls_law", "decls_class", "class_body", 
		"decls_fact", "decls_rule", "elms", "decls_letlattice", "expression", 
		"expressions", "comparison", "additive", "multiplicative", "infix", "extended", 
		"unary", "ascribe", "e_primary", "e_letMatch", "e_ifThenElse", "e_match", 
		"e_switch", "e_apply", "e_var", "e_tag", "e_tuple", "e_keyValue", "e_keyValues", 
		"e_wild", "e_fNil", "e_fNone", "e_fSome", "e_fList", "e_fVec", "e_fSet", 
		"e_fMap", "e_unaryLambda", "e_lambda", "existential", "universal", "pattern", 
		"patterns", "simple", "p_keyValue", "p_keyValues", "p_tag", "p_tuple", 
		"p_fNil", "p_fNone", "p_fVec", "p_fSet", "p_fMap", "bools", "negative", 
		"float32", "float64", "floatDefault", "floats", "int8", "int16", "int32", 
		"int64", "bigInt", "intDefault", "ints", "literals", "primary", "type", 
		"lambda", "tuple_unit", "tuple_singleton", "tuple_multi", "tuple", "parametric", 
		"unary_ops", "logical_ops", "comparison_ops", "multipve_ops", "addve_ops", 
		"extbin_ops", "predicate", "predicates", "pred_true", "pred_false", "pred_ambiguous", 
		"pred_equal", "pred_notequal", "pred_loop"
	};

	private static final String[] _LITERAL_NAMES = {
		null, "'.'", "'/'", "'@'", "':'", "','", "'('", "')'", "'{'", "'}'", "'case'", 
		"'=>'", "'['", "']'", "'<='", "'import'", "'namespace'", "'enum'", "'rel'", 
		"'lat'", "'index'", "'def'", "'external'", "'='", "'law'", "'class'", 
		"':-'", "'let'", "'<>'", "'`'", "'in'", "'if'", "'else'", "'match'", "'with'", 
		"'switch'", "'->'", "'Some'", "'::'", "'#['", "'#{'", "'@{'", "'∃'", "'\\exists'", 
		"'∀'", "'\\forall'", "'...'", "'true'", "'false'", "'-'", "'f32'", "'f64'", 
		"'i8'", "'i16'", "'i32'", "'i64'", "'ii'", "'()'", "'+'", "'¬'", "'~'", 
		"'!'", "'&&'", "'||'", "'&'", "'|'", "'==>'", "'<==>'", "'^'", "'<<'", 
		"'>>'", "'∧'", "'∨'", "'→'", "'↔'", "'>='", "'<'", "'>'", "'=='", "'!='", 
		"'≡'", "'**'", "'*'", "'%'", "'⊑'", "'⊔'", "'⊓'", "'▽'", "'△'", "':='", 
		"'<-'", null, "';'", null, null, "'None'", "'Nil'", "'⊥'", "'⊤'", "'_'", 
		"'???'"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, "WS", "SC", "Comment", "Ident", 
		"FNone", "FNil", "Bot", "Top", "Wild", "UserError", "Chars", "Strs", "Digits"
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
	public static class StartContext extends ParserRuleContext {
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
		}
		public S_importsContext s_imports() {
			return getRuleContext(S_importsContext.class,0);
		}
		public DeclsContext decls() {
			return getRuleContext(DeclsContext.class,0);
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
	}

	public final StartContext start() throws RecognitionException {
		StartContext _localctx = new StartContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_start);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(255);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,0,_ctx) ) {
			case 1:
				{
				setState(254);
				match(WS);
				}
				break;
			}
			setState(258);
			_la = _input.LA(1);
			if (_la==T__14) {
				{
				setState(257);
				s_imports();
				}
			}

			setState(261);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,2,_ctx) ) {
			case 1:
				{
				setState(260);
				match(WS);
				}
				break;
			}
			setState(264);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,3,_ctx) ) {
			case 1:
				{
				setState(263);
				decls();
				}
				break;
			}
			setState(267);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(266);
				match(WS);
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

	public static class OptSCContext extends ParserRuleContext {
		public TerminalNode SC() { return getToken(FlixParser.SC, 0); }
		public TerminalNode WS() { return getToken(FlixParser.WS, 0); }
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
	}

	public final OptSCContext optSC() throws RecognitionException {
		OptSCContext _localctx = new OptSCContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_optSC);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(273);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,6,_ctx) ) {
			case 1:
				{
				setState(270);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(269);
					match(WS);
					}
				}

				setState(272);
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

	public static class NnameContext extends ParserRuleContext {
		public List<TerminalNode> Ident() { return getTokens(FlixParser.Ident); }
		public TerminalNode Ident(int i) {
			return getToken(FlixParser.Ident, i);
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
	}

	public final NnameContext nname() throws RecognitionException {
		NnameContext _localctx = new NnameContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_nname);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(275);
			match(Ident);
			setState(280);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__0) {
				{
				{
				setState(276);
				match(T__0);
				setState(277);
				match(Ident);
				}
				}
				setState(282);
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

	public static class QnameContext extends ParserRuleContext {
		public TerminalNode Ident() { return getToken(FlixParser.Ident, 0); }
		public NnameContext nname() {
			return getRuleContext(NnameContext.class,0);
		}
		public QnameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_qname; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterQname(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitQname(this);
		}
	}

	public final QnameContext qname() throws RecognitionException {
		QnameContext _localctx = new QnameContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_qname);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(286);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,8,_ctx) ) {
			case 1:
				{
				setState(283);
				nname();
				setState(284);
				match(T__1);
				}
				break;
			}
			setState(288);
			match(Ident);
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
		public TerminalNode Ident() { return getToken(FlixParser.Ident, 0); }
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
	}

	public final AnnotationContext annotation() throws RecognitionException {
		AnnotationContext _localctx = new AnnotationContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_annotation);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(290);
			match(T__2);
			setState(291);
			match(Ident);
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
		public TerminalNode Ident() { return getToken(FlixParser.Ident, 0); }
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public TerminalNode WS() { return getToken(FlixParser.WS, 0); }
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
	}

	public final ArgumentContext argument() throws RecognitionException {
		ArgumentContext _localctx = new ArgumentContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_argument);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(293);
			match(Ident);
			setState(294);
			match(T__3);
			setState(296);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(295);
				match(WS);
				}
			}

			setState(298);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final ArgumentsContext arguments() throws RecognitionException {
		ArgumentsContext _localctx = new ArgumentsContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_arguments);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(300);
			argument();
			setState(311);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,12,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(302);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(301);
						match(WS);
						}
					}

					setState(304);
					match(T__4);
					setState(306);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(305);
						match(WS);
						}
					}

					setState(308);
					argument();
					}
					} 
				}
				setState(313);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,12,_ctx);
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

	public static class ParamsContext extends ParserRuleContext {
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
		}
		public ArgumentsContext arguments() {
			return getRuleContext(ArgumentsContext.class,0);
		}
		public ParamsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_params; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterParams(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitParams(this);
		}
	}

	public final ParamsContext params() throws RecognitionException {
		ParamsContext _localctx = new ParamsContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_params);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(325);
			_la = _input.LA(1);
			if (_la==T__5) {
				{
				setState(314);
				match(T__5);
				setState(316);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,13,_ctx) ) {
				case 1:
					{
					setState(315);
					match(WS);
					}
					break;
				}
				setState(319);
				_la = _input.LA(1);
				if (_la==Ident) {
					{
					setState(318);
					arguments();
					}
				}

				setState(322);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(321);
					match(WS);
					}
				}

				setState(324);
				match(T__6);
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
		public TerminalNode Ident() { return getToken(FlixParser.Ident, 0); }
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final AttributeContext attribute() throws RecognitionException {
		AttributeContext _localctx = new AttributeContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_attribute);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(327);
			match(Ident);
			setState(329);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(328);
				match(WS);
				}
			}

			setState(331);
			match(T__3);
			setState(333);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(332);
				match(WS);
				}
			}

			setState(335);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final AttributesContext attributes() throws RecognitionException {
		AttributesContext _localctx = new AttributesContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_attributes);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(337);
			attribute();
			setState(348);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,21,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(339);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(338);
						match(WS);
						}
					}

					setState(341);
					match(T__4);
					setState(343);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(342);
						match(WS);
						}
					}

					setState(345);
					attribute();
					}
					} 
				}
				setState(350);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,21,_ctx);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
		}
		public List<TerminalNode> Ident() { return getTokens(FlixParser.Ident); }
		public TerminalNode Ident(int i) {
			return getToken(FlixParser.Ident, i);
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
	}

	public final IndexContext index() throws RecognitionException {
		IndexContext _localctx = new IndexContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_index);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(351);
			match(T__7);
			setState(353);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,22,_ctx) ) {
			case 1:
				{
				setState(352);
				match(WS);
				}
				break;
			}
			setState(369);
			_la = _input.LA(1);
			if (_la==Ident) {
				{
				setState(355);
				match(Ident);
				setState(366);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,25,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(357);
						_la = _input.LA(1);
						if (_la==WS) {
							{
							setState(356);
							match(WS);
							}
						}

						setState(359);
						match(T__4);
						setState(361);
						_la = _input.LA(1);
						if (_la==WS) {
							{
							setState(360);
							match(WS);
							}
						}

						setState(363);
						match(Ident);
						}
						} 
					}
					setState(368);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,25,_ctx);
				}
				}
			}

			setState(372);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(371);
				match(WS);
				}
			}

			setState(374);
			match(T__8);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final IndexesContext indexes() throws RecognitionException {
		IndexesContext _localctx = new IndexesContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_indexes);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(376);
			index();
			setState(387);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,30,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(378);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(377);
						match(WS);
						}
					}

					setState(380);
					match(T__4);
					setState(382);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(381);
						match(WS);
						}
					}

					setState(384);
					index();
					}
					} 
				}
				setState(389);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,30,_ctx);
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

	public static class IdentsContext extends ParserRuleContext {
		public List<TerminalNode> Ident() { return getTokens(FlixParser.Ident); }
		public TerminalNode Ident(int i) {
			return getToken(FlixParser.Ident, i);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
		}
		public IdentsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_idents; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterIdents(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitIdents(this);
		}
	}

	public final IdentsContext idents() throws RecognitionException {
		IdentsContext _localctx = new IdentsContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_idents);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(390);
			match(Ident);
			setState(401);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,33,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(392);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(391);
						match(WS);
						}
					}

					setState(394);
					match(T__4);
					setState(396);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(395);
						match(WS);
						}
					}

					setState(398);
					match(Ident);
					}
					} 
				}
				setState(403);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final Match_ruleContext match_rule() throws RecognitionException {
		Match_ruleContext _localctx = new Match_ruleContext(_ctx, getState());
		enterRule(_localctx, 26, RULE_match_rule);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(404);
			match(T__9);
			setState(405);
			match(WS);
			setState(406);
			pattern();
			setState(408);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(407);
				match(WS);
				}
			}

			setState(410);
			match(T__10);
			setState(412);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(411);
				match(WS);
				}
			}

			setState(414);
			expression();
			setState(416);
			_la = _input.LA(1);
			if (_la==SC) {
				{
				setState(415);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final Match_rulesContext match_rules() throws RecognitionException {
		Match_rulesContext _localctx = new Match_rulesContext(_ctx, getState());
		enterRule(_localctx, 28, RULE_match_rules);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(418);
			match_rule();
			setState(425);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,38,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(420);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(419);
						match(WS);
						}
					}

					setState(422);
					match_rule();
					}
					} 
				}
				setState(427);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final Switch_ruleContext switch_rule() throws RecognitionException {
		Switch_ruleContext _localctx = new Switch_ruleContext(_ctx, getState());
		enterRule(_localctx, 30, RULE_switch_rule);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(428);
			match(T__9);
			setState(429);
			match(WS);
			setState(430);
			expression();
			setState(432);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(431);
				match(WS);
				}
			}

			setState(434);
			match(T__10);
			setState(436);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(435);
				match(WS);
				}
			}

			setState(438);
			expression();
			setState(440);
			_la = _input.LA(1);
			if (_la==SC) {
				{
				setState(439);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final Switch_rulesContext switch_rules() throws RecognitionException {
		Switch_rulesContext _localctx = new Switch_rulesContext(_ctx, getState());
		enterRule(_localctx, 32, RULE_switch_rules);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(442);
			switch_rule();
			setState(449);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,43,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(444);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(443);
						match(WS);
						}
					}

					setState(446);
					switch_rule();
					}
					} 
				}
				setState(451);
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
		public TerminalNode Ident() { return getToken(FlixParser.Ident, 0); }
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final TypeparamContext typeparam() throws RecognitionException {
		TypeparamContext _localctx = new TypeparamContext(_ctx, getState());
		enterRule(_localctx, 34, RULE_typeparam);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(452);
			match(Ident);
			setState(461);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,46,_ctx) ) {
			case 1:
				{
				setState(454);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(453);
					match(WS);
					}
				}

				setState(456);
				match(T__3);
				setState(458);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(457);
					match(WS);
					}
				}

				setState(460);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final TypeparamsContext typeparams() throws RecognitionException {
		TypeparamsContext _localctx = new TypeparamsContext(_ctx, getState());
		enterRule(_localctx, 36, RULE_typeparams);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(463);
			typeparam();
			setState(474);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,49,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(465);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(464);
						match(WS);
						}
					}

					setState(467);
					match(T__4);
					setState(469);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(468);
						match(WS);
						}
					}

					setState(471);
					typeparam();
					}
					} 
				}
				setState(476);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,49,_ctx);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final Class_typeparamsContext class_typeparams() throws RecognitionException {
		Class_typeparamsContext _localctx = new Class_typeparamsContext(_ctx, getState());
		enterRule(_localctx, 38, RULE_class_typeparams);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(477);
			type();
			setState(488);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,52,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(479);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(478);
						match(WS);
						}
					}

					setState(481);
					match(T__4);
					setState(483);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(482);
						match(WS);
						}
					}

					setState(485);
					type();
					}
					} 
				}
				setState(490);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,52,_ctx);
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

	public static class ContextBoundContext extends ParserRuleContext {
		public TerminalNode Ident() { return getToken(FlixParser.Ident, 0); }
		public Class_typeparamsContext class_typeparams() {
			return getRuleContext(Class_typeparamsContext.class,0);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final ContextBoundContext contextBound() throws RecognitionException {
		ContextBoundContext _localctx = new ContextBoundContext(_ctx, getState());
		enterRule(_localctx, 40, RULE_contextBound);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(491);
			match(Ident);
			setState(492);
			match(T__11);
			setState(494);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(493);
				match(WS);
				}
			}

			setState(496);
			class_typeparams();
			setState(498);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(497);
				match(WS);
				}
			}

			setState(500);
			match(T__12);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final ContextBoundsContext contextBounds() throws RecognitionException {
		ContextBoundsContext _localctx = new ContextBoundsContext(_ctx, getState());
		enterRule(_localctx, 42, RULE_contextBounds);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(502);
			contextBound();
			setState(513);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,57,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(504);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(503);
						match(WS);
						}
					}

					setState(506);
					match(T__4);
					setState(508);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(507);
						match(WS);
						}
					}

					setState(510);
					contextBound();
					}
					} 
				}
				setState(515);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final ContextBoundsListContext contextBoundsList() throws RecognitionException {
		ContextBoundsListContext _localctx = new ContextBoundsListContext(_ctx, getState());
		enterRule(_localctx, 44, RULE_contextBoundsList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(524);
			_la = _input.LA(1);
			if (_la==T__13) {
				{
				setState(516);
				match(T__13);
				setState(518);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(517);
					match(WS);
					}
				}

				setState(520);
				contextBounds();
				setState(522);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,59,_ctx) ) {
				case 1:
					{
					setState(521);
					match(WS);
					}
					break;
				}
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

	public static class FunctionsContext extends ParserRuleContext {
		public List<Decls_functionContext> decls_function() {
			return getRuleContexts(Decls_functionContext.class);
		}
		public Decls_functionContext decls_function(int i) {
			return getRuleContext(Decls_functionContext.class,i);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
		}
		public FunctionsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_functions; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterFunctions(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitFunctions(this);
		}
	}

	public final FunctionsContext functions() throws RecognitionException {
		FunctionsContext _localctx = new FunctionsContext(_ctx, getState());
		enterRule(_localctx, 46, RULE_functions);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(526);
			decls_function();
			setState(531);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,61,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(527);
					match(WS);
					setState(528);
					decls_function();
					}
					} 
				}
				setState(533);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,61,_ctx);
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
	}

	public final S_importContext s_import() throws RecognitionException {
		S_importContext _localctx = new S_importContext(_ctx, getState());
		enterRule(_localctx, 48, RULE_s_import);
		try {
			setState(537);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,62,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(534);
				import_wildcard();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(535);
				import_definition();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(536);
				import_namespace();
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

	public static class S_importsContext extends ParserRuleContext {
		public List<S_importContext> s_import() {
			return getRuleContexts(S_importContext.class);
		}
		public S_importContext s_import(int i) {
			return getRuleContext(S_importContext.class,i);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
		}
		public S_importsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_s_imports; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterS_imports(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitS_imports(this);
		}
	}

	public final S_importsContext s_imports() throws RecognitionException {
		S_importsContext _localctx = new S_importsContext(_ctx, getState());
		enterRule(_localctx, 50, RULE_s_imports);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(539);
			s_import();
			setState(546);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,64,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(541);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(540);
						match(WS);
						}
					}

					setState(543);
					s_import();
					}
					} 
				}
				setState(548);
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

	public static class Import_wildcardContext extends ParserRuleContext {
		public TerminalNode WS() { return getToken(FlixParser.WS, 0); }
		public NnameContext nname() {
			return getRuleContext(NnameContext.class,0);
		}
		public TerminalNode Wild() { return getToken(FlixParser.Wild, 0); }
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
	}

	public final Import_wildcardContext import_wildcard() throws RecognitionException {
		Import_wildcardContext _localctx = new Import_wildcardContext(_ctx, getState());
		enterRule(_localctx, 52, RULE_import_wildcard);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(549);
			match(T__14);
			setState(550);
			match(WS);
			setState(551);
			nname();
			setState(552);
			match(T__1);
			setState(553);
			match(Wild);
			setState(554);
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
		public TerminalNode WS() { return getToken(FlixParser.WS, 0); }
		public NnameContext nname() {
			return getRuleContext(NnameContext.class,0);
		}
		public TerminalNode Ident() { return getToken(FlixParser.Ident, 0); }
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
	}

	public final Import_definitionContext import_definition() throws RecognitionException {
		Import_definitionContext _localctx = new Import_definitionContext(_ctx, getState());
		enterRule(_localctx, 54, RULE_import_definition);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(556);
			match(T__14);
			setState(557);
			match(WS);
			setState(558);
			nname();
			setState(559);
			match(T__1);
			setState(560);
			match(Ident);
			setState(561);
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
		public TerminalNode WS() { return getToken(FlixParser.WS, 0); }
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
	}

	public final Import_namespaceContext import_namespace() throws RecognitionException {
		Import_namespaceContext _localctx = new Import_namespaceContext(_ctx, getState());
		enterRule(_localctx, 56, RULE_import_namespace);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(563);
			match(T__14);
			setState(564);
			match(WS);
			setState(565);
			nname();
			setState(566);
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
		public Decls_functionContext decls_function() {
			return getRuleContext(Decls_functionContext.class,0);
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
	}

	public final DeclContext decl() throws RecognitionException {
		DeclContext _localctx = new DeclContext(_ctx, getState());
		enterRule(_localctx, 58, RULE_decl);
		try {
			setState(581);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,65,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(568);
				decls_namespace();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(569);
				decls_enum();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(570);
				decls_relation();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(571);
				decls_lattice();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(572);
				decls_index();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(573);
				decls_signature();
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(574);
				decls_external();
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(575);
				decls_function();
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(576);
				decls_law();
				}
				break;
			case 10:
				enterOuterAlt(_localctx, 10);
				{
				setState(577);
				decls_class();
				}
				break;
			case 11:
				enterOuterAlt(_localctx, 11);
				{
				setState(578);
				decls_fact();
				}
				break;
			case 12:
				enterOuterAlt(_localctx, 12);
				{
				setState(579);
				decls_rule();
				}
				break;
			case 13:
				enterOuterAlt(_localctx, 13);
				{
				setState(580);
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

	public static class DeclsContext extends ParserRuleContext {
		public List<DeclContext> decl() {
			return getRuleContexts(DeclContext.class);
		}
		public DeclContext decl(int i) {
			return getRuleContext(DeclContext.class,i);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
		}
		public DeclsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_decls; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterDecls(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitDecls(this);
		}
	}

	public final DeclsContext decls() throws RecognitionException {
		DeclsContext _localctx = new DeclsContext(_ctx, getState());
		enterRule(_localctx, 60, RULE_decls);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(583);
			decl();
			setState(590);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,67,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(585);
					_errHandler.sync(this);
					switch ( getInterpreter().adaptivePredict(_input,66,_ctx) ) {
					case 1:
						{
						setState(584);
						match(WS);
						}
						break;
					}
					setState(587);
					decl();
					}
					} 
				}
				setState(592);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,67,_ctx);
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

	public static class Decls_namespaceContext extends ParserRuleContext {
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
		}
		public NnameContext nname() {
			return getRuleContext(NnameContext.class,0);
		}
		public OptSCContext optSC() {
			return getRuleContext(OptSCContext.class,0);
		}
		public DeclsContext decls() {
			return getRuleContext(DeclsContext.class,0);
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
	}

	public final Decls_namespaceContext decls_namespace() throws RecognitionException {
		Decls_namespaceContext _localctx = new Decls_namespaceContext(_ctx, getState());
		enterRule(_localctx, 62, RULE_decls_namespace);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(593);
			match(T__15);
			setState(594);
			match(WS);
			setState(595);
			nname();
			setState(597);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(596);
				match(WS);
				}
			}

			setState(599);
			match(T__7);
			setState(601);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,69,_ctx) ) {
			case 1:
				{
				setState(600);
				match(WS);
				}
				break;
			}
			setState(604);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,70,_ctx) ) {
			case 1:
				{
				setState(603);
				decls();
				}
				break;
			}
			setState(607);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(606);
				match(WS);
				}
			}

			setState(609);
			match(T__8);
			setState(610);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
		}
		public TerminalNode Ident() { return getToken(FlixParser.Ident, 0); }
		public List<CasesContext> cases() {
			return getRuleContexts(CasesContext.class);
		}
		public CasesContext cases(int i) {
			return getRuleContext(CasesContext.class,i);
		}
		public OptSCContext optSC() {
			return getRuleContext(OptSCContext.class,0);
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
	}

	public final Decls_enumContext decls_enum() throws RecognitionException {
		Decls_enumContext _localctx = new Decls_enumContext(_ctx, getState());
		enterRule(_localctx, 64, RULE_decls_enum);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(612);
			match(T__16);
			setState(613);
			match(WS);
			setState(614);
			match(Ident);
			setState(616);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(615);
				match(WS);
				}
			}

			setState(618);
			match(T__7);
			setState(620);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(619);
				match(WS);
				}
			}

			setState(622);
			cases();
			setState(624);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(623);
				match(WS);
				}
			}

			setState(636);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__4) {
				{
				{
				setState(626);
				match(T__4);
				setState(628);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(627);
					match(WS);
					}
				}

				setState(630);
				cases();
				setState(632);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(631);
					match(WS);
					}
				}

				}
				}
				setState(638);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(639);
			match(T__8);
			setState(640);
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

	public static class CasesContext extends ParserRuleContext {
		public TerminalNode WS() { return getToken(FlixParser.WS, 0); }
		public TerminalNode Ident() { return getToken(FlixParser.Ident, 0); }
		public TupleContext tuple() {
			return getRuleContext(TupleContext.class,0);
		}
		public CasesContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_cases; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterCases(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitCases(this);
		}
	}

	public final CasesContext cases() throws RecognitionException {
		CasesContext _localctx = new CasesContext(_ctx, getState());
		enterRule(_localctx, 66, RULE_cases);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(642);
			match(T__9);
			setState(643);
			match(WS);
			setState(644);
			match(Ident);
			setState(646);
			_la = _input.LA(1);
			if (_la==T__5 || _la==T__56) {
				{
				setState(645);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
		}
		public TerminalNode Ident() { return getToken(FlixParser.Ident, 0); }
		public OptSCContext optSC() {
			return getRuleContext(OptSCContext.class,0);
		}
		public AttributesContext attributes() {
			return getRuleContext(AttributesContext.class,0);
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
	}

	public final Decls_relationContext decls_relation() throws RecognitionException {
		Decls_relationContext _localctx = new Decls_relationContext(_ctx, getState());
		enterRule(_localctx, 68, RULE_decls_relation);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(648);
			match(T__17);
			setState(649);
			match(WS);
			setState(650);
			match(Ident);
			setState(652);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(651);
				match(WS);
				}
			}

			setState(654);
			match(T__5);
			setState(656);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,80,_ctx) ) {
			case 1:
				{
				setState(655);
				match(WS);
				}
				break;
			}
			setState(659);
			_la = _input.LA(1);
			if (_la==Ident) {
				{
				setState(658);
				attributes();
				}
			}

			setState(662);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(661);
				match(WS);
				}
			}

			setState(664);
			match(T__6);
			setState(665);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
		}
		public TerminalNode Ident() { return getToken(FlixParser.Ident, 0); }
		public OptSCContext optSC() {
			return getRuleContext(OptSCContext.class,0);
		}
		public AttributesContext attributes() {
			return getRuleContext(AttributesContext.class,0);
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
	}

	public final Decls_latticeContext decls_lattice() throws RecognitionException {
		Decls_latticeContext _localctx = new Decls_latticeContext(_ctx, getState());
		enterRule(_localctx, 70, RULE_decls_lattice);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(667);
			match(T__18);
			setState(668);
			match(WS);
			setState(669);
			match(Ident);
			setState(671);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(670);
				match(WS);
				}
			}

			setState(673);
			match(T__5);
			setState(675);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,84,_ctx) ) {
			case 1:
				{
				setState(674);
				match(WS);
				}
				break;
			}
			setState(678);
			_la = _input.LA(1);
			if (_la==Ident) {
				{
				setState(677);
				attributes();
				}
			}

			setState(681);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(680);
				match(WS);
				}
			}

			setState(683);
			match(T__6);
			setState(684);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
		}
		public TerminalNode Ident() { return getToken(FlixParser.Ident, 0); }
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
	}

	public final Decls_indexContext decls_index() throws RecognitionException {
		Decls_indexContext _localctx = new Decls_indexContext(_ctx, getState());
		enterRule(_localctx, 72, RULE_decls_index);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(686);
			match(T__19);
			setState(687);
			match(WS);
			setState(688);
			match(Ident);
			setState(690);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(689);
				match(WS);
				}
			}

			setState(692);
			match(T__5);
			setState(694);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,88,_ctx) ) {
			case 1:
				{
				setState(693);
				match(WS);
				}
				break;
			}
			setState(697);
			_la = _input.LA(1);
			if (_la==T__7) {
				{
				setState(696);
				indexes();
				}
			}

			setState(700);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(699);
				match(WS);
				}
			}

			setState(702);
			match(T__6);
			setState(703);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
		}
		public TerminalNode Ident() { return getToken(FlixParser.Ident, 0); }
		public ParamsContext params() {
			return getRuleContext(ParamsContext.class,0);
		}
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public OptSCContext optSC() {
			return getRuleContext(OptSCContext.class,0);
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
	}

	public final Decls_signatureContext decls_signature() throws RecognitionException {
		Decls_signatureContext _localctx = new Decls_signatureContext(_ctx, getState());
		enterRule(_localctx, 74, RULE_decls_signature);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(705);
			match(T__20);
			setState(706);
			match(WS);
			setState(707);
			match(Ident);
			setState(709);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,91,_ctx) ) {
			case 1:
				{
				setState(708);
				match(WS);
				}
				break;
			}
			setState(711);
			params();
			setState(713);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(712);
				match(WS);
				}
			}

			setState(715);
			match(T__3);
			setState(717);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(716);
				match(WS);
				}
			}

			setState(719);
			type();
			setState(720);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
		}
		public TerminalNode Ident() { return getToken(FlixParser.Ident, 0); }
		public ParamsContext params() {
			return getRuleContext(ParamsContext.class,0);
		}
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public OptSCContext optSC() {
			return getRuleContext(OptSCContext.class,0);
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
	}

	public final Decls_externalContext decls_external() throws RecognitionException {
		Decls_externalContext _localctx = new Decls_externalContext(_ctx, getState());
		enterRule(_localctx, 76, RULE_decls_external);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(722);
			match(T__21);
			setState(724);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(723);
				match(WS);
				}
			}

			setState(726);
			match(T__20);
			setState(727);
			match(WS);
			setState(728);
			match(Ident);
			setState(730);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,95,_ctx) ) {
			case 1:
				{
				setState(729);
				match(WS);
				}
				break;
			}
			setState(732);
			params();
			setState(734);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(733);
				match(WS);
				}
			}

			setState(736);
			match(T__3);
			setState(738);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(737);
				match(WS);
				}
			}

			setState(740);
			type();
			setState(741);
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

	public static class Decls_functionContext extends ParserRuleContext {
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
		}
		public TerminalNode Ident() { return getToken(FlixParser.Ident, 0); }
		public ParamsContext params() {
			return getRuleContext(ParamsContext.class,0);
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
		public List<AnnotationContext> annotation() {
			return getRuleContexts(AnnotationContext.class);
		}
		public AnnotationContext annotation(int i) {
			return getRuleContext(AnnotationContext.class,i);
		}
		public Decls_functionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_decls_function; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterDecls_function(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitDecls_function(this);
		}
	}

	public final Decls_functionContext decls_function() throws RecognitionException {
		Decls_functionContext _localctx = new Decls_functionContext(_ctx, getState());
		enterRule(_localctx, 78, RULE_decls_function);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(751);
			_la = _input.LA(1);
			if (_la==T__2) {
				{
				setState(743);
				annotation();
				setState(748);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,98,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(744);
						match(WS);
						setState(745);
						annotation();
						}
						} 
					}
					setState(750);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,98,_ctx);
				}
				}
			}

			setState(754);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(753);
				match(WS);
				}
			}

			setState(756);
			match(T__20);
			setState(757);
			match(WS);
			setState(758);
			match(Ident);
			setState(760);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,101,_ctx) ) {
			case 1:
				{
				setState(759);
				match(WS);
				}
				break;
			}
			setState(762);
			params();
			setState(764);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(763);
				match(WS);
				}
			}

			setState(766);
			match(T__3);
			setState(768);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(767);
				match(WS);
				}
			}

			setState(770);
			type();
			setState(772);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(771);
				match(WS);
				}
			}

			setState(774);
			match(T__22);
			setState(776);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(775);
				match(WS);
				}
			}

			setState(778);
			expression();
			setState(779);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
		}
		public TerminalNode Ident() { return getToken(FlixParser.Ident, 0); }
		public TypeparamsContext typeparams() {
			return getRuleContext(TypeparamsContext.class,0);
		}
		public ParamsContext params() {
			return getRuleContext(ParamsContext.class,0);
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
	}

	public final Decls_lawContext decls_law() throws RecognitionException {
		Decls_lawContext _localctx = new Decls_lawContext(_ctx, getState());
		enterRule(_localctx, 80, RULE_decls_law);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(781);
			match(T__23);
			setState(782);
			match(WS);
			setState(783);
			match(Ident);
			setState(785);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(784);
				match(WS);
				}
			}

			setState(787);
			match(T__11);
			setState(789);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(788);
				match(WS);
				}
			}

			setState(791);
			typeparams();
			setState(793);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(792);
				match(WS);
				}
			}

			setState(795);
			match(T__12);
			setState(797);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,109,_ctx) ) {
			case 1:
				{
				setState(796);
				match(WS);
				}
				break;
			}
			setState(799);
			params();
			setState(801);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(800);
				match(WS);
				}
			}

			setState(803);
			match(T__3);
			setState(805);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(804);
				match(WS);
				}
			}

			setState(807);
			type();
			setState(809);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(808);
				match(WS);
				}
			}

			setState(811);
			match(T__22);
			setState(813);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(812);
				match(WS);
				}
			}

			setState(815);
			expression();
			setState(816);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
		}
		public TerminalNode Ident() { return getToken(FlixParser.Ident, 0); }
		public Class_typeparamsContext class_typeparams() {
			return getRuleContext(Class_typeparamsContext.class,0);
		}
		public ContextBoundsListContext contextBoundsList() {
			return getRuleContext(ContextBoundsListContext.class,0);
		}
		public Class_bodyContext class_body() {
			return getRuleContext(Class_bodyContext.class,0);
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
	}

	public final Decls_classContext decls_class() throws RecognitionException {
		Decls_classContext _localctx = new Decls_classContext(_ctx, getState());
		enterRule(_localctx, 82, RULE_decls_class);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(818);
			match(T__24);
			setState(819);
			match(WS);
			setState(820);
			match(Ident);
			setState(821);
			match(T__11);
			setState(823);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(822);
				match(WS);
				}
			}

			setState(825);
			class_typeparams();
			setState(827);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(826);
				match(WS);
				}
			}

			setState(829);
			match(T__12);
			setState(831);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,116,_ctx) ) {
			case 1:
				{
				setState(830);
				match(WS);
				}
				break;
			}
			setState(833);
			contextBoundsList();
			setState(835);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(834);
				match(WS);
				}
			}

			setState(837);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
		}
		public FunctionsContext functions() {
			return getRuleContext(FunctionsContext.class,0);
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
	}

	public final Class_bodyContext class_body() throws RecognitionException {
		Class_bodyContext _localctx = new Class_bodyContext(_ctx, getState());
		enterRule(_localctx, 84, RULE_class_body);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(839);
			match(T__7);
			setState(841);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,118,_ctx) ) {
			case 1:
				{
				setState(840);
				match(WS);
				}
				break;
			}
			setState(844);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,119,_ctx) ) {
			case 1:
				{
				setState(843);
				functions();
				}
				break;
			}
			setState(847);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(846);
				match(WS);
				}
			}

			setState(849);
			match(T__8);
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
		public TerminalNode WS() { return getToken(FlixParser.WS, 0); }
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
	}

	public final Decls_factContext decls_fact() throws RecognitionException {
		Decls_factContext _localctx = new Decls_factContext(_ctx, getState());
		enterRule(_localctx, 86, RULE_decls_fact);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(851);
			predicate();
			setState(853);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(852);
				match(WS);
				}
			}

			setState(855);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final Decls_ruleContext decls_rule() throws RecognitionException {
		Decls_ruleContext _localctx = new Decls_ruleContext(_ctx, getState());
		enterRule(_localctx, 88, RULE_decls_rule);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(857);
			predicate();
			setState(859);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(858);
				match(WS);
				}
			}

			setState(861);
			match(T__25);
			setState(863);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(862);
				match(WS);
				}
			}

			setState(865);
			predicates();
			setState(867);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(866);
				match(WS);
				}
			}

			setState(869);
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
	}

	public final ElmsContext elms() throws RecognitionException {
		ElmsContext _localctx = new ElmsContext(_ctx, getState());
		enterRule(_localctx, 90, RULE_elms);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(871);
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
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public ElmsContext elms() {
			return getRuleContext(ElmsContext.class,0);
		}
		public OptSCContext optSC() {
			return getRuleContext(OptSCContext.class,0);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final Decls_letlatticeContext decls_letlattice() throws RecognitionException {
		Decls_letlatticeContext _localctx = new Decls_letlatticeContext(_ctx, getState());
		enterRule(_localctx, 92, RULE_decls_letlattice);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(873);
			match(T__26);
			setState(875);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(874);
				match(WS);
				}
			}

			setState(877);
			type();
			setState(878);
			match(T__27);
			setState(880);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(879);
				match(WS);
				}
			}

			setState(882);
			match(T__22);
			setState(884);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(883);
				match(WS);
				}
			}

			setState(886);
			match(T__5);
			setState(888);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(887);
				match(WS);
				}
			}

			setState(890);
			elms();
			setState(892);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(891);
				match(WS);
				}
			}

			setState(894);
			match(T__6);
			setState(895);
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

	public static class ExpressionContext extends ParserRuleContext {
		public List<ComparisonContext> comparison() {
			return getRuleContexts(ComparisonContext.class);
		}
		public ComparisonContext comparison(int i) {
			return getRuleContext(ComparisonContext.class,i);
		}
		public Logical_opsContext logical_ops() {
			return getRuleContext(Logical_opsContext.class,0);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final ExpressionContext expression() throws RecognitionException {
		ExpressionContext _localctx = new ExpressionContext(_ctx, getState());
		enterRule(_localctx, 94, RULE_expression);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(897);
			comparison();
			setState(907);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,132,_ctx) ) {
			case 1:
				{
				setState(899);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(898);
					match(WS);
					}
				}

				setState(901);
				logical_ops();
				setState(903);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(902);
					match(WS);
					}
				}

				setState(905);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final ExpressionsContext expressions() throws RecognitionException {
		ExpressionsContext _localctx = new ExpressionsContext(_ctx, getState());
		enterRule(_localctx, 96, RULE_expressions);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(909);
			expression();
			setState(920);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,135,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(911);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(910);
						match(WS);
						}
					}

					setState(913);
					match(T__4);
					setState(915);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(914);
						match(WS);
						}
					}

					setState(917);
					expression();
					}
					} 
				}
				setState(922);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,135,_ctx);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final ComparisonContext comparison() throws RecognitionException {
		ComparisonContext _localctx = new ComparisonContext(_ctx, getState());
		enterRule(_localctx, 98, RULE_comparison);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(923);
			additive();
			setState(933);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,138,_ctx) ) {
			case 1:
				{
				setState(925);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(924);
					match(WS);
					}
				}

				setState(927);
				comparison_ops();
				setState(929);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(928);
					match(WS);
					}
				}

				setState(931);
				additive();
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
		public List<MultiplicativeContext> multiplicative() {
			return getRuleContexts(MultiplicativeContext.class);
		}
		public MultiplicativeContext multiplicative(int i) {
			return getRuleContext(MultiplicativeContext.class,i);
		}
		public List<Addve_opsContext> addve_ops() {
			return getRuleContexts(Addve_opsContext.class);
		}
		public Addve_opsContext addve_ops(int i) {
			return getRuleContext(Addve_opsContext.class,i);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final AdditiveContext additive() throws RecognitionException {
		AdditiveContext _localctx = new AdditiveContext(_ctx, getState());
		enterRule(_localctx, 100, RULE_additive);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(935);
			multiplicative();
			setState(947);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,141,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(937);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(936);
						match(WS);
						}
					}

					setState(939);
					addve_ops();
					setState(941);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(940);
						match(WS);
						}
					}

					setState(943);
					multiplicative();
					}
					} 
				}
				setState(949);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,141,_ctx);
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

	public static class MultiplicativeContext extends ParserRuleContext {
		public List<InfixContext> infix() {
			return getRuleContexts(InfixContext.class);
		}
		public InfixContext infix(int i) {
			return getRuleContext(InfixContext.class,i);
		}
		public List<Multipve_opsContext> multipve_ops() {
			return getRuleContexts(Multipve_opsContext.class);
		}
		public Multipve_opsContext multipve_ops(int i) {
			return getRuleContext(Multipve_opsContext.class,i);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final MultiplicativeContext multiplicative() throws RecognitionException {
		MultiplicativeContext _localctx = new MultiplicativeContext(_ctx, getState());
		enterRule(_localctx, 102, RULE_multiplicative);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(950);
			infix();
			setState(962);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,144,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(952);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(951);
						match(WS);
						}
					}

					setState(954);
					multipve_ops();
					setState(956);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(955);
						match(WS);
						}
					}

					setState(958);
					infix();
					}
					} 
				}
				setState(964);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,144,_ctx);
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

	public static class InfixContext extends ParserRuleContext {
		public List<ExtendedContext> extended() {
			return getRuleContexts(ExtendedContext.class);
		}
		public ExtendedContext extended(int i) {
			return getRuleContext(ExtendedContext.class,i);
		}
		public QnameContext qname() {
			return getRuleContext(QnameContext.class,0);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final InfixContext infix() throws RecognitionException {
		InfixContext _localctx = new InfixContext(_ctx, getState());
		enterRule(_localctx, 104, RULE_infix);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(965);
			extended();
			setState(977);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,147,_ctx) ) {
			case 1:
				{
				setState(967);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(966);
					match(WS);
					}
				}

				setState(969);
				match(T__28);
				setState(970);
				qname();
				setState(971);
				match(T__28);
				setState(973);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(972);
					match(WS);
					}
				}

				setState(975);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final ExtendedContext extended() throws RecognitionException {
		ExtendedContext _localctx = new ExtendedContext(_ctx, getState());
		enterRule(_localctx, 106, RULE_extended);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(979);
			unary();
			setState(989);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,150,_ctx) ) {
			case 1:
				{
				setState(981);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(980);
					match(WS);
					}
				}

				setState(983);
				extbin_ops();
				setState(985);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(984);
					match(WS);
					}
				}

				setState(987);
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
		public TerminalNode WS() { return getToken(FlixParser.WS, 0); }
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
	}

	public final UnaryContext unary() throws RecognitionException {
		UnaryContext _localctx = new UnaryContext(_ctx, getState());
		enterRule(_localctx, 108, RULE_unary);
		int _la;
		try {
			setState(998);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,152,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				{
				setState(991);
				unary_ops();
				setState(993);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(992);
					match(WS);
					}
				}

				setState(995);
				unary();
				}
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(997);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final AscribeContext ascribe() throws RecognitionException {
		AscribeContext _localctx = new AscribeContext(_ctx, getState());
		enterRule(_localctx, 110, RULE_ascribe);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1000);
			e_fList();
			setState(1009);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,155,_ctx) ) {
			case 1:
				{
				setState(1002);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1001);
					match(WS);
					}
				}

				setState(1004);
				match(T__3);
				setState(1006);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1005);
					match(WS);
					}
				}

				setState(1008);
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
		public E_fNoneContext e_fNone() {
			return getRuleContext(E_fNoneContext.class,0);
		}
		public E_fSomeContext e_fSome() {
			return getRuleContext(E_fSomeContext.class,0);
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
		public LiteralsContext literals() {
			return getRuleContext(LiteralsContext.class,0);
		}
		public ExistentialContext existential() {
			return getRuleContext(ExistentialContext.class,0);
		}
		public UniversalContext universal() {
			return getRuleContext(UniversalContext.class,0);
		}
		public TerminalNode Bot() { return getToken(FlixParser.Bot, 0); }
		public TerminalNode Top() { return getToken(FlixParser.Top, 0); }
		public E_unaryLambdaContext e_unaryLambda() {
			return getRuleContext(E_unaryLambdaContext.class,0);
		}
		public E_wildContext e_wild() {
			return getRuleContext(E_wildContext.class,0);
		}
		public E_varContext e_var() {
			return getRuleContext(E_varContext.class,0);
		}
		public TerminalNode UserError() { return getToken(FlixParser.UserError, 0); }
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
	}

	public final E_primaryContext e_primary() throws RecognitionException {
		E_primaryContext _localctx = new E_primaryContext(_ctx, getState());
		enterRule(_localctx, 112, RULE_e_primary);
		try {
			setState(1033);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,156,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1011);
				e_letMatch();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1012);
				e_ifThenElse();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1013);
				e_match();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(1014);
				e_switch();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(1015);
				e_tag();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(1016);
				e_lambda();
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(1017);
				e_tuple();
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(1018);
				e_fNil();
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(1019);
				e_fNone();
				}
				break;
			case 10:
				enterOuterAlt(_localctx, 10);
				{
				setState(1020);
				e_fSome();
				}
				break;
			case 11:
				enterOuterAlt(_localctx, 11);
				{
				setState(1021);
				e_fVec();
				}
				break;
			case 12:
				enterOuterAlt(_localctx, 12);
				{
				setState(1022);
				e_fSet();
				}
				break;
			case 13:
				enterOuterAlt(_localctx, 13);
				{
				setState(1023);
				e_fMap();
				}
				break;
			case 14:
				enterOuterAlt(_localctx, 14);
				{
				setState(1024);
				literals();
				}
				break;
			case 15:
				enterOuterAlt(_localctx, 15);
				{
				setState(1025);
				existential();
				}
				break;
			case 16:
				enterOuterAlt(_localctx, 16);
				{
				setState(1026);
				universal();
				}
				break;
			case 17:
				enterOuterAlt(_localctx, 17);
				{
				setState(1027);
				match(Bot);
				}
				break;
			case 18:
				enterOuterAlt(_localctx, 18);
				{
				setState(1028);
				match(Top);
				}
				break;
			case 19:
				enterOuterAlt(_localctx, 19);
				{
				setState(1029);
				e_unaryLambda();
				}
				break;
			case 20:
				enterOuterAlt(_localctx, 20);
				{
				setState(1030);
				e_wild();
				}
				break;
			case 21:
				enterOuterAlt(_localctx, 21);
				{
				setState(1031);
				e_var();
				}
				break;
			case 22:
				enterOuterAlt(_localctx, 22);
				{
				setState(1032);
				match(UserError);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final E_letMatchContext e_letMatch() throws RecognitionException {
		E_letMatchContext _localctx = new E_letMatchContext(_ctx, getState());
		enterRule(_localctx, 114, RULE_e_letMatch);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1035);
			match(T__26);
			setState(1036);
			match(WS);
			setState(1037);
			pattern();
			setState(1039);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1038);
				match(WS);
				}
			}

			setState(1041);
			match(T__22);
			setState(1043);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1042);
				match(WS);
				}
			}

			setState(1045);
			expression();
			setState(1046);
			match(WS);
			setState(1047);
			match(T__29);
			setState(1048);
			match(WS);
			setState(1049);
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
		public List<ExpressionContext> expression() {
			return getRuleContexts(ExpressionContext.class);
		}
		public ExpressionContext expression(int i) {
			return getRuleContext(ExpressionContext.class,i);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
		}
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
	}

	public final E_ifThenElseContext e_ifThenElse() throws RecognitionException {
		E_ifThenElseContext _localctx = new E_ifThenElseContext(_ctx, getState());
		enterRule(_localctx, 116, RULE_e_ifThenElse);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1051);
			match(T__30);
			setState(1053);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1052);
				match(WS);
				}
			}

			setState(1055);
			match(T__5);
			setState(1057);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1056);
				match(WS);
				}
			}

			setState(1059);
			expression();
			setState(1061);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1060);
				match(WS);
				}
			}

			setState(1063);
			match(T__6);
			setState(1065);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1064);
				match(WS);
				}
			}

			setState(1067);
			expression();
			setState(1068);
			match(WS);
			setState(1069);
			match(T__31);
			setState(1070);
			match(WS);
			setState(1071);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
		}
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
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
	}

	public final E_matchContext e_match() throws RecognitionException {
		E_matchContext _localctx = new E_matchContext(_ctx, getState());
		enterRule(_localctx, 118, RULE_e_match);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1073);
			match(T__32);
			setState(1074);
			match(WS);
			setState(1075);
			expression();
			setState(1076);
			match(WS);
			setState(1077);
			match(T__33);
			setState(1078);
			match(WS);
			setState(1079);
			match(T__7);
			setState(1081);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1080);
				match(WS);
				}
			}

			setState(1083);
			match_rules();
			setState(1085);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1084);
				match(WS);
				}
			}

			setState(1087);
			match(T__8);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
		}
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
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
	}

	public final E_switchContext e_switch() throws RecognitionException {
		E_switchContext _localctx = new E_switchContext(_ctx, getState());
		enterRule(_localctx, 120, RULE_e_switch);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1089);
			match(T__34);
			setState(1090);
			match(WS);
			setState(1091);
			expression();
			setState(1092);
			match(WS);
			setState(1093);
			match(T__33);
			setState(1094);
			match(WS);
			setState(1095);
			match(T__7);
			setState(1097);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1096);
				match(WS);
				}
			}

			setState(1099);
			switch_rules();
			setState(1101);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1100);
				match(WS);
				}
			}

			setState(1103);
			match(T__8);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final E_applyContext e_apply() throws RecognitionException {
		E_applyContext _localctx = new E_applyContext(_ctx, getState());
		enterRule(_localctx, 122, RULE_e_apply);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1105);
			e_primary();
			setState(1120);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,171,_ctx) ) {
			case 1:
				{
				setState(1107);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1106);
					match(WS);
					}
				}

				setState(1109);
				match(T__5);
				setState(1111);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,168,_ctx) ) {
				case 1:
					{
					setState(1110);
					match(WS);
					}
					break;
				}
				setState(1114);
				_la = _input.LA(1);
				if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__5) | (1L << T__26) | (1L << T__30) | (1L << T__32) | (1L << T__34) | (1L << T__36) | (1L << T__38) | (1L << T__39) | (1L << T__40) | (1L << T__41) | (1L << T__42) | (1L << T__43) | (1L << T__44) | (1L << T__46) | (1L << T__47) | (1L << T__48) | (1L << T__57) | (1L << T__58) | (1L << T__59) | (1L << T__60))) != 0) || ((((_la - 94)) & ~0x3f) == 0 && ((1L << (_la - 94)) & ((1L << (Ident - 94)) | (1L << (FNone - 94)) | (1L << (FNil - 94)) | (1L << (Bot - 94)) | (1L << (Top - 94)) | (1L << (Wild - 94)) | (1L << (UserError - 94)) | (1L << (Chars - 94)) | (1L << (Strs - 94)) | (1L << (Digits - 94)))) != 0)) {
					{
					setState(1113);
					expressions();
					}
				}

				setState(1117);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1116);
					match(WS);
					}
				}

				setState(1119);
				match(T__6);
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

	public static class E_varContext extends ParserRuleContext {
		public QnameContext qname() {
			return getRuleContext(QnameContext.class,0);
		}
		public E_varContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_e_var; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterE_var(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitE_var(this);
		}
	}

	public final E_varContext e_var() throws RecognitionException {
		E_varContext _localctx = new E_varContext(_ctx, getState());
		enterRule(_localctx, 124, RULE_e_var);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1122);
			qname();
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
		public QnameContext qname() {
			return getRuleContext(QnameContext.class,0);
		}
		public TerminalNode Ident() { return getToken(FlixParser.Ident, 0); }
		public E_tupleContext e_tuple() {
			return getRuleContext(E_tupleContext.class,0);
		}
		public TerminalNode WS() { return getToken(FlixParser.WS, 0); }
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
	}

	public final E_tagContext e_tag() throws RecognitionException {
		E_tagContext _localctx = new E_tagContext(_ctx, getState());
		enterRule(_localctx, 126, RULE_e_tag);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1124);
			qname();
			setState(1125);
			match(T__0);
			setState(1126);
			match(Ident);
			setState(1131);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,173,_ctx) ) {
			case 1:
				{
				setState(1128);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1127);
					match(WS);
					}
				}

				setState(1130);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final E_tupleContext e_tuple() throws RecognitionException {
		E_tupleContext _localctx = new E_tupleContext(_ctx, getState());
		enterRule(_localctx, 128, RULE_e_tuple);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1133);
			match(T__5);
			setState(1135);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,174,_ctx) ) {
			case 1:
				{
				setState(1134);
				match(WS);
				}
				break;
			}
			setState(1138);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__5) | (1L << T__26) | (1L << T__30) | (1L << T__32) | (1L << T__34) | (1L << T__36) | (1L << T__38) | (1L << T__39) | (1L << T__40) | (1L << T__41) | (1L << T__42) | (1L << T__43) | (1L << T__44) | (1L << T__46) | (1L << T__47) | (1L << T__48) | (1L << T__57) | (1L << T__58) | (1L << T__59) | (1L << T__60))) != 0) || ((((_la - 94)) & ~0x3f) == 0 && ((1L << (_la - 94)) & ((1L << (Ident - 94)) | (1L << (FNone - 94)) | (1L << (FNil - 94)) | (1L << (Bot - 94)) | (1L << (Top - 94)) | (1L << (Wild - 94)) | (1L << (UserError - 94)) | (1L << (Chars - 94)) | (1L << (Strs - 94)) | (1L << (Digits - 94)))) != 0)) {
				{
				setState(1137);
				expressions();
				}
			}

			setState(1141);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1140);
				match(WS);
				}
			}

			setState(1143);
			match(T__6);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final E_keyValueContext e_keyValue() throws RecognitionException {
		E_keyValueContext _localctx = new E_keyValueContext(_ctx, getState());
		enterRule(_localctx, 130, RULE_e_keyValue);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1145);
			expression();
			setState(1147);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1146);
				match(WS);
				}
			}

			setState(1149);
			match(T__35);
			setState(1151);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1150);
				match(WS);
				}
			}

			setState(1153);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final E_keyValuesContext e_keyValues() throws RecognitionException {
		E_keyValuesContext _localctx = new E_keyValuesContext(_ctx, getState());
		enterRule(_localctx, 132, RULE_e_keyValues);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1155);
			e_keyValue();
			setState(1166);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,181,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1157);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1156);
						match(WS);
						}
					}

					setState(1159);
					match(T__4);
					setState(1161);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1160);
						match(WS);
						}
					}

					setState(1163);
					e_keyValue();
					}
					} 
				}
				setState(1168);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,181,_ctx);
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

	public static class E_wildContext extends ParserRuleContext {
		public TerminalNode Wild() { return getToken(FlixParser.Wild, 0); }
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
	}

	public final E_wildContext e_wild() throws RecognitionException {
		E_wildContext _localctx = new E_wildContext(_ctx, getState());
		enterRule(_localctx, 134, RULE_e_wild);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1169);
			match(Wild);
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
		public TerminalNode FNil() { return getToken(FlixParser.FNil, 0); }
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
	}

	public final E_fNilContext e_fNil() throws RecognitionException {
		E_fNilContext _localctx = new E_fNilContext(_ctx, getState());
		enterRule(_localctx, 136, RULE_e_fNil);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1171);
			match(FNil);
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

	public static class E_fNoneContext extends ParserRuleContext {
		public TerminalNode FNone() { return getToken(FlixParser.FNone, 0); }
		public E_fNoneContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_e_fNone; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterE_fNone(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitE_fNone(this);
		}
	}

	public final E_fNoneContext e_fNone() throws RecognitionException {
		E_fNoneContext _localctx = new E_fNoneContext(_ctx, getState());
		enterRule(_localctx, 138, RULE_e_fNone);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1173);
			match(FNone);
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

	public static class E_fSomeContext extends ParserRuleContext {
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
		}
		public E_fSomeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_e_fSome; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterE_fSome(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitE_fSome(this);
		}
	}

	public final E_fSomeContext e_fSome() throws RecognitionException {
		E_fSomeContext _localctx = new E_fSomeContext(_ctx, getState());
		enterRule(_localctx, 140, RULE_e_fSome);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1175);
			match(T__36);
			setState(1177);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1176);
				match(WS);
				}
			}

			setState(1179);
			match(T__5);
			setState(1181);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1180);
				match(WS);
				}
			}

			setState(1183);
			expression();
			setState(1185);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1184);
				match(WS);
				}
			}

			setState(1187);
			match(T__6);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final E_fListContext e_fList() throws RecognitionException {
		E_fListContext _localctx = new E_fListContext(_ctx, getState());
		enterRule(_localctx, 142, RULE_e_fList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1189);
			e_apply();
			setState(1198);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,187,_ctx) ) {
			case 1:
				{
				setState(1191);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1190);
					match(WS);
					}
				}

				setState(1193);
				match(T__37);
				setState(1195);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1194);
					match(WS);
					}
				}

				setState(1197);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final E_fVecContext e_fVec() throws RecognitionException {
		E_fVecContext _localctx = new E_fVecContext(_ctx, getState());
		enterRule(_localctx, 144, RULE_e_fVec);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1200);
			match(T__38);
			setState(1202);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,188,_ctx) ) {
			case 1:
				{
				setState(1201);
				match(WS);
				}
				break;
			}
			setState(1205);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__5) | (1L << T__26) | (1L << T__30) | (1L << T__32) | (1L << T__34) | (1L << T__36) | (1L << T__38) | (1L << T__39) | (1L << T__40) | (1L << T__41) | (1L << T__42) | (1L << T__43) | (1L << T__44) | (1L << T__46) | (1L << T__47) | (1L << T__48) | (1L << T__57) | (1L << T__58) | (1L << T__59) | (1L << T__60))) != 0) || ((((_la - 94)) & ~0x3f) == 0 && ((1L << (_la - 94)) & ((1L << (Ident - 94)) | (1L << (FNone - 94)) | (1L << (FNil - 94)) | (1L << (Bot - 94)) | (1L << (Top - 94)) | (1L << (Wild - 94)) | (1L << (UserError - 94)) | (1L << (Chars - 94)) | (1L << (Strs - 94)) | (1L << (Digits - 94)))) != 0)) {
				{
				setState(1204);
				expressions();
				}
			}

			setState(1208);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1207);
				match(WS);
				}
			}

			setState(1210);
			match(T__12);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final E_fSetContext e_fSet() throws RecognitionException {
		E_fSetContext _localctx = new E_fSetContext(_ctx, getState());
		enterRule(_localctx, 146, RULE_e_fSet);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1212);
			match(T__39);
			setState(1214);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,191,_ctx) ) {
			case 1:
				{
				setState(1213);
				match(WS);
				}
				break;
			}
			setState(1217);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__5) | (1L << T__26) | (1L << T__30) | (1L << T__32) | (1L << T__34) | (1L << T__36) | (1L << T__38) | (1L << T__39) | (1L << T__40) | (1L << T__41) | (1L << T__42) | (1L << T__43) | (1L << T__44) | (1L << T__46) | (1L << T__47) | (1L << T__48) | (1L << T__57) | (1L << T__58) | (1L << T__59) | (1L << T__60))) != 0) || ((((_la - 94)) & ~0x3f) == 0 && ((1L << (_la - 94)) & ((1L << (Ident - 94)) | (1L << (FNone - 94)) | (1L << (FNil - 94)) | (1L << (Bot - 94)) | (1L << (Top - 94)) | (1L << (Wild - 94)) | (1L << (UserError - 94)) | (1L << (Chars - 94)) | (1L << (Strs - 94)) | (1L << (Digits - 94)))) != 0)) {
				{
				setState(1216);
				expressions();
				}
			}

			setState(1220);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1219);
				match(WS);
				}
			}

			setState(1222);
			match(T__8);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final E_fMapContext e_fMap() throws RecognitionException {
		E_fMapContext _localctx = new E_fMapContext(_ctx, getState());
		enterRule(_localctx, 148, RULE_e_fMap);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1224);
			match(T__40);
			setState(1226);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,194,_ctx) ) {
			case 1:
				{
				setState(1225);
				match(WS);
				}
				break;
			}
			setState(1229);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__5) | (1L << T__26) | (1L << T__30) | (1L << T__32) | (1L << T__34) | (1L << T__36) | (1L << T__38) | (1L << T__39) | (1L << T__40) | (1L << T__41) | (1L << T__42) | (1L << T__43) | (1L << T__44) | (1L << T__46) | (1L << T__47) | (1L << T__48) | (1L << T__57) | (1L << T__58) | (1L << T__59) | (1L << T__60))) != 0) || ((((_la - 94)) & ~0x3f) == 0 && ((1L << (_la - 94)) & ((1L << (Ident - 94)) | (1L << (FNone - 94)) | (1L << (FNil - 94)) | (1L << (Bot - 94)) | (1L << (Top - 94)) | (1L << (Wild - 94)) | (1L << (UserError - 94)) | (1L << (Chars - 94)) | (1L << (Strs - 94)) | (1L << (Digits - 94)))) != 0)) {
				{
				setState(1228);
				e_keyValues();
				}
			}

			setState(1232);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1231);
				match(WS);
				}
			}

			setState(1234);
			match(T__8);
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
		public TerminalNode Ident() { return getToken(FlixParser.Ident, 0); }
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final E_unaryLambdaContext e_unaryLambda() throws RecognitionException {
		E_unaryLambdaContext _localctx = new E_unaryLambdaContext(_ctx, getState());
		enterRule(_localctx, 150, RULE_e_unaryLambda);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1236);
			match(Ident);
			setState(1238);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1237);
				match(WS);
				}
			}

			setState(1240);
			match(T__35);
			setState(1242);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1241);
				match(WS);
				}
			}

			setState(1244);
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
		public IdentsContext idents() {
			return getRuleContext(IdentsContext.class,0);
		}
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final E_lambdaContext e_lambda() throws RecognitionException {
		E_lambdaContext _localctx = new E_lambdaContext(_ctx, getState());
		enterRule(_localctx, 152, RULE_e_lambda);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1246);
			match(T__5);
			setState(1248);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1247);
				match(WS);
				}
			}

			setState(1250);
			idents();
			setState(1252);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1251);
				match(WS);
				}
			}

			setState(1254);
			match(T__6);
			setState(1256);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1255);
				match(WS);
				}
			}

			setState(1258);
			match(T__35);
			setState(1260);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1259);
				match(WS);
				}
			}

			setState(1262);
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

	public static class ExistentialContext extends ParserRuleContext {
		public ParamsContext params() {
			return getRuleContext(ParamsContext.class,0);
		}
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final ExistentialContext existential() throws RecognitionException {
		ExistentialContext _localctx = new ExistentialContext(_ctx, getState());
		enterRule(_localctx, 154, RULE_existential);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1264);
			_la = _input.LA(1);
			if ( !(_la==T__41 || _la==T__42) ) {
			_errHandler.recoverInline(this);
			} else {
				consume();
			}
			setState(1266);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,203,_ctx) ) {
			case 1:
				{
				setState(1265);
				match(WS);
				}
				break;
			}
			setState(1268);
			params();
			setState(1270);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1269);
				match(WS);
				}
			}

			setState(1272);
			match(T__0);
			setState(1274);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1273);
				match(WS);
				}
			}

			setState(1276);
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
		public ParamsContext params() {
			return getRuleContext(ParamsContext.class,0);
		}
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final UniversalContext universal() throws RecognitionException {
		UniversalContext _localctx = new UniversalContext(_ctx, getState());
		enterRule(_localctx, 156, RULE_universal);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1278);
			_la = _input.LA(1);
			if ( !(_la==T__43 || _la==T__44) ) {
			_errHandler.recoverInline(this);
			} else {
				consume();
			}
			setState(1280);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,206,_ctx) ) {
			case 1:
				{
				setState(1279);
				match(WS);
				}
				break;
			}
			setState(1282);
			params();
			setState(1284);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1283);
				match(WS);
				}
			}

			setState(1286);
			match(T__0);
			setState(1288);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1287);
				match(WS);
				}
			}

			setState(1290);
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
		public SimpleContext simple() {
			return getRuleContext(SimpleContext.class,0);
		}
		public PatternContext pattern() {
			return getRuleContext(PatternContext.class,0);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final PatternContext pattern() throws RecognitionException {
		PatternContext _localctx = new PatternContext(_ctx, getState());
		enterRule(_localctx, 158, RULE_pattern);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1292);
			simple();
			setState(1301);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,211,_ctx) ) {
			case 1:
				{
				setState(1294);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1293);
					match(WS);
					}
				}

				setState(1296);
				match(T__37);
				setState(1298);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1297);
					match(WS);
					}
				}

				setState(1300);
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

	public static class PatternsContext extends ParserRuleContext {
		public List<PatternContext> pattern() {
			return getRuleContexts(PatternContext.class);
		}
		public PatternContext pattern(int i) {
			return getRuleContext(PatternContext.class,i);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final PatternsContext patterns() throws RecognitionException {
		PatternsContext _localctx = new PatternsContext(_ctx, getState());
		enterRule(_localctx, 160, RULE_patterns);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1303);
			pattern();
			setState(1314);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,214,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1305);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1304);
						match(WS);
						}
					}

					setState(1307);
					match(T__4);
					setState(1309);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1308);
						match(WS);
						}
					}

					setState(1311);
					pattern();
					}
					} 
				}
				setState(1316);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,214,_ctx);
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
		public P_fNoneContext p_fNone() {
			return getRuleContext(P_fNoneContext.class,0);
		}
		public LiteralsContext literals() {
			return getRuleContext(LiteralsContext.class,0);
		}
		public TerminalNode Ident() { return getToken(FlixParser.Ident, 0); }
		public TerminalNode Wild() { return getToken(FlixParser.Wild, 0); }
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
	}

	public final SimpleContext simple() throws RecognitionException {
		SimpleContext _localctx = new SimpleContext(_ctx, getState());
		enterRule(_localctx, 162, RULE_simple);
		try {
			setState(1327);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,215,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1317);
				p_fNil();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1318);
				p_fNone();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1319);
				literals();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(1320);
				match(Ident);
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(1321);
				match(Wild);
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(1322);
				p_tag();
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(1323);
				p_tuple();
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(1324);
				p_fVec();
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(1325);
				p_fSet();
				}
				break;
			case 10:
				enterOuterAlt(_localctx, 10);
				{
				setState(1326);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final P_keyValueContext p_keyValue() throws RecognitionException {
		P_keyValueContext _localctx = new P_keyValueContext(_ctx, getState());
		enterRule(_localctx, 164, RULE_p_keyValue);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1329);
			pattern();
			setState(1331);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1330);
				match(WS);
				}
			}

			setState(1333);
			match(T__35);
			setState(1335);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1334);
				match(WS);
				}
			}

			setState(1337);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final P_keyValuesContext p_keyValues() throws RecognitionException {
		P_keyValuesContext _localctx = new P_keyValuesContext(_ctx, getState());
		enterRule(_localctx, 166, RULE_p_keyValues);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1339);
			p_keyValue();
			setState(1350);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,220,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1341);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1340);
						match(WS);
						}
					}

					setState(1343);
					match(T__4);
					setState(1345);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1344);
						match(WS);
						}
					}

					setState(1347);
					p_keyValue();
					}
					} 
				}
				setState(1352);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,220,_ctx);
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

	public static class P_tagContext extends ParserRuleContext {
		public QnameContext qname() {
			return getRuleContext(QnameContext.class,0);
		}
		public TerminalNode Ident() { return getToken(FlixParser.Ident, 0); }
		public PatternContext pattern() {
			return getRuleContext(PatternContext.class,0);
		}
		public TerminalNode WS() { return getToken(FlixParser.WS, 0); }
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
	}

	public final P_tagContext p_tag() throws RecognitionException {
		P_tagContext _localctx = new P_tagContext(_ctx, getState());
		enterRule(_localctx, 168, RULE_p_tag);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1353);
			qname();
			setState(1354);
			match(T__0);
			setState(1355);
			match(Ident);
			setState(1360);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,222,_ctx) ) {
			case 1:
				{
				setState(1357);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1356);
					match(WS);
					}
				}

				setState(1359);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final P_tupleContext p_tuple() throws RecognitionException {
		P_tupleContext _localctx = new P_tupleContext(_ctx, getState());
		enterRule(_localctx, 170, RULE_p_tuple);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1362);
			match(T__5);
			setState(1364);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,223,_ctx) ) {
			case 1:
				{
				setState(1363);
				match(WS);
				}
				break;
			}
			setState(1367);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__5) | (1L << T__38) | (1L << T__39) | (1L << T__40) | (1L << T__46) | (1L << T__47) | (1L << T__48))) != 0) || ((((_la - 94)) & ~0x3f) == 0 && ((1L << (_la - 94)) & ((1L << (Ident - 94)) | (1L << (FNone - 94)) | (1L << (FNil - 94)) | (1L << (Wild - 94)) | (1L << (Chars - 94)) | (1L << (Strs - 94)) | (1L << (Digits - 94)))) != 0)) {
				{
				setState(1366);
				patterns();
				}
			}

			setState(1370);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1369);
				match(WS);
				}
			}

			setState(1372);
			match(T__6);
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
		public TerminalNode FNil() { return getToken(FlixParser.FNil, 0); }
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
	}

	public final P_fNilContext p_fNil() throws RecognitionException {
		P_fNilContext _localctx = new P_fNilContext(_ctx, getState());
		enterRule(_localctx, 172, RULE_p_fNil);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1374);
			match(FNil);
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

	public static class P_fNoneContext extends ParserRuleContext {
		public TerminalNode FNone() { return getToken(FlixParser.FNone, 0); }
		public P_fNoneContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_p_fNone; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterP_fNone(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitP_fNone(this);
		}
	}

	public final P_fNoneContext p_fNone() throws RecognitionException {
		P_fNoneContext _localctx = new P_fNoneContext(_ctx, getState());
		enterRule(_localctx, 174, RULE_p_fNone);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1376);
			match(FNone);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
		}
		public PatternsContext patterns() {
			return getRuleContext(PatternsContext.class,0);
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
	}

	public final P_fVecContext p_fVec() throws RecognitionException {
		P_fVecContext _localctx = new P_fVecContext(_ctx, getState());
		enterRule(_localctx, 176, RULE_p_fVec);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1378);
			match(T__38);
			setState(1380);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,226,_ctx) ) {
			case 1:
				{
				setState(1379);
				match(WS);
				}
				break;
			}
			setState(1383);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__5) | (1L << T__38) | (1L << T__39) | (1L << T__40) | (1L << T__46) | (1L << T__47) | (1L << T__48))) != 0) || ((((_la - 94)) & ~0x3f) == 0 && ((1L << (_la - 94)) & ((1L << (Ident - 94)) | (1L << (FNone - 94)) | (1L << (FNil - 94)) | (1L << (Wild - 94)) | (1L << (Chars - 94)) | (1L << (Strs - 94)) | (1L << (Digits - 94)))) != 0)) {
				{
				setState(1382);
				patterns();
				}
			}

			setState(1393);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,230,_ctx) ) {
			case 1:
				{
				setState(1386);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1385);
					match(WS);
					}
				}

				setState(1388);
				match(T__4);
				setState(1390);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1389);
					match(WS);
					}
				}

				setState(1392);
				match(T__45);
				}
				break;
			}
			setState(1396);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1395);
				match(WS);
				}
			}

			setState(1398);
			match(T__12);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
		}
		public PatternsContext patterns() {
			return getRuleContext(PatternsContext.class,0);
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
	}

	public final P_fSetContext p_fSet() throws RecognitionException {
		P_fSetContext _localctx = new P_fSetContext(_ctx, getState());
		enterRule(_localctx, 178, RULE_p_fSet);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1400);
			match(T__39);
			setState(1402);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,232,_ctx) ) {
			case 1:
				{
				setState(1401);
				match(WS);
				}
				break;
			}
			setState(1405);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__5) | (1L << T__38) | (1L << T__39) | (1L << T__40) | (1L << T__46) | (1L << T__47) | (1L << T__48))) != 0) || ((((_la - 94)) & ~0x3f) == 0 && ((1L << (_la - 94)) & ((1L << (Ident - 94)) | (1L << (FNone - 94)) | (1L << (FNil - 94)) | (1L << (Wild - 94)) | (1L << (Chars - 94)) | (1L << (Strs - 94)) | (1L << (Digits - 94)))) != 0)) {
				{
				setState(1404);
				patterns();
				}
			}

			setState(1415);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,236,_ctx) ) {
			case 1:
				{
				setState(1408);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1407);
					match(WS);
					}
				}

				setState(1410);
				match(T__4);
				setState(1412);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1411);
					match(WS);
					}
				}

				setState(1414);
				match(T__45);
				}
				break;
			}
			setState(1418);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1417);
				match(WS);
				}
			}

			setState(1420);
			match(T__8);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
		}
		public P_keyValuesContext p_keyValues() {
			return getRuleContext(P_keyValuesContext.class,0);
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
	}

	public final P_fMapContext p_fMap() throws RecognitionException {
		P_fMapContext _localctx = new P_fMapContext(_ctx, getState());
		enterRule(_localctx, 180, RULE_p_fMap);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1422);
			match(T__40);
			setState(1424);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,238,_ctx) ) {
			case 1:
				{
				setState(1423);
				match(WS);
				}
				break;
			}
			setState(1427);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__5) | (1L << T__38) | (1L << T__39) | (1L << T__40) | (1L << T__46) | (1L << T__47) | (1L << T__48))) != 0) || ((((_la - 94)) & ~0x3f) == 0 && ((1L << (_la - 94)) & ((1L << (Ident - 94)) | (1L << (FNone - 94)) | (1L << (FNil - 94)) | (1L << (Wild - 94)) | (1L << (Chars - 94)) | (1L << (Strs - 94)) | (1L << (Digits - 94)))) != 0)) {
				{
				setState(1426);
				p_keyValues();
				}
			}

			setState(1437);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,242,_ctx) ) {
			case 1:
				{
				setState(1430);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1429);
					match(WS);
					}
				}

				setState(1432);
				match(T__4);
				setState(1434);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1433);
					match(WS);
					}
				}

				setState(1436);
				match(T__45);
				}
				break;
			}
			setState(1440);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1439);
				match(WS);
				}
			}

			setState(1442);
			match(T__8);
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
	}

	public final BoolsContext bools() throws RecognitionException {
		BoolsContext _localctx = new BoolsContext(_ctx, getState());
		enterRule(_localctx, 182, RULE_bools);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1444);
			_la = _input.LA(1);
			if ( !(_la==T__46 || _la==T__47) ) {
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
	}

	public final NegativeContext negative() throws RecognitionException {
		NegativeContext _localctx = new NegativeContext(_ctx, getState());
		enterRule(_localctx, 184, RULE_negative);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1446);
			match(T__48);
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
	}

	public final Float32Context float32() throws RecognitionException {
		Float32Context _localctx = new Float32Context(_ctx, getState());
		enterRule(_localctx, 186, RULE_float32);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1449);
			_la = _input.LA(1);
			if (_la==T__48) {
				{
				setState(1448);
				negative();
				}
			}

			setState(1451);
			match(Digits);
			setState(1452);
			match(T__0);
			setState(1453);
			match(Digits);
			setState(1454);
			match(T__49);
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
	}

	public final Float64Context float64() throws RecognitionException {
		Float64Context _localctx = new Float64Context(_ctx, getState());
		enterRule(_localctx, 188, RULE_float64);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1457);
			_la = _input.LA(1);
			if (_la==T__48) {
				{
				setState(1456);
				negative();
				}
			}

			setState(1459);
			match(Digits);
			setState(1460);
			match(T__0);
			setState(1461);
			match(Digits);
			setState(1462);
			match(T__50);
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
	}

	public final FloatDefaultContext floatDefault() throws RecognitionException {
		FloatDefaultContext _localctx = new FloatDefaultContext(_ctx, getState());
		enterRule(_localctx, 190, RULE_floatDefault);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1465);
			_la = _input.LA(1);
			if (_la==T__48) {
				{
				setState(1464);
				negative();
				}
			}

			setState(1467);
			match(Digits);
			setState(1468);
			match(T__0);
			setState(1469);
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
	}

	public final FloatsContext floats() throws RecognitionException {
		FloatsContext _localctx = new FloatsContext(_ctx, getState());
		enterRule(_localctx, 192, RULE_floats);
		try {
			setState(1474);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,247,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1471);
				float32();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1472);
				float64();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1473);
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
	}

	public final Int8Context int8() throws RecognitionException {
		Int8Context _localctx = new Int8Context(_ctx, getState());
		enterRule(_localctx, 194, RULE_int8);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1477);
			_la = _input.LA(1);
			if (_la==T__48) {
				{
				setState(1476);
				negative();
				}
			}

			setState(1479);
			match(Digits);
			setState(1480);
			match(T__51);
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
	}

	public final Int16Context int16() throws RecognitionException {
		Int16Context _localctx = new Int16Context(_ctx, getState());
		enterRule(_localctx, 196, RULE_int16);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1483);
			_la = _input.LA(1);
			if (_la==T__48) {
				{
				setState(1482);
				negative();
				}
			}

			setState(1485);
			match(Digits);
			setState(1486);
			match(T__52);
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
	}

	public final Int32Context int32() throws RecognitionException {
		Int32Context _localctx = new Int32Context(_ctx, getState());
		enterRule(_localctx, 198, RULE_int32);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1489);
			_la = _input.LA(1);
			if (_la==T__48) {
				{
				setState(1488);
				negative();
				}
			}

			setState(1491);
			match(Digits);
			setState(1492);
			match(T__53);
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
	}

	public final Int64Context int64() throws RecognitionException {
		Int64Context _localctx = new Int64Context(_ctx, getState());
		enterRule(_localctx, 200, RULE_int64);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1495);
			_la = _input.LA(1);
			if (_la==T__48) {
				{
				setState(1494);
				negative();
				}
			}

			setState(1497);
			match(Digits);
			setState(1498);
			match(T__54);
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
	}

	public final BigIntContext bigInt() throws RecognitionException {
		BigIntContext _localctx = new BigIntContext(_ctx, getState());
		enterRule(_localctx, 202, RULE_bigInt);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1501);
			_la = _input.LA(1);
			if (_la==T__48) {
				{
				setState(1500);
				negative();
				}
			}

			setState(1503);
			match(Digits);
			setState(1504);
			match(T__55);
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
	}

	public final IntDefaultContext intDefault() throws RecognitionException {
		IntDefaultContext _localctx = new IntDefaultContext(_ctx, getState());
		enterRule(_localctx, 204, RULE_intDefault);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1507);
			_la = _input.LA(1);
			if (_la==T__48) {
				{
				setState(1506);
				negative();
				}
			}

			setState(1509);
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
	}

	public final IntsContext ints() throws RecognitionException {
		IntsContext _localctx = new IntsContext(_ctx, getState());
		enterRule(_localctx, 206, RULE_ints);
		try {
			setState(1517);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,254,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1511);
				int8();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1512);
				int16();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1513);
				int32();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(1514);
				int64();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(1515);
				bigInt();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(1516);
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

	public static class LiteralsContext extends ParserRuleContext {
		public BoolsContext bools() {
			return getRuleContext(BoolsContext.class,0);
		}
		public TerminalNode Chars() { return getToken(FlixParser.Chars, 0); }
		public FloatsContext floats() {
			return getRuleContext(FloatsContext.class,0);
		}
		public IntsContext ints() {
			return getRuleContext(IntsContext.class,0);
		}
		public TerminalNode Strs() { return getToken(FlixParser.Strs, 0); }
		public LiteralsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_literals; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterLiterals(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitLiterals(this);
		}
	}

	public final LiteralsContext literals() throws RecognitionException {
		LiteralsContext _localctx = new LiteralsContext(_ctx, getState());
		enterRule(_localctx, 208, RULE_literals);
		try {
			setState(1524);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,255,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1519);
				bools();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1520);
				match(Chars);
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1521);
				floats();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(1522);
				ints();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(1523);
				match(Strs);
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

	public static class PrimaryContext extends ParserRuleContext {
		public LambdaContext lambda() {
			return getRuleContext(LambdaContext.class,0);
		}
		public TupleContext tuple() {
			return getRuleContext(TupleContext.class,0);
		}
		public ParametricContext parametric() {
			return getRuleContext(ParametricContext.class,0);
		}
		public QnameContext qname() {
			return getRuleContext(QnameContext.class,0);
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
	}

	public final PrimaryContext primary() throws RecognitionException {
		PrimaryContext _localctx = new PrimaryContext(_ctx, getState());
		enterRule(_localctx, 210, RULE_primary);
		try {
			setState(1530);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,256,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1526);
				lambda();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1527);
				tuple();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1528);
				parametric();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(1529);
				qname();
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

	public static class TypeContext extends ParserRuleContext {
		public PrimaryContext primary() {
			return getRuleContext(PrimaryContext.class,0);
		}
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final TypeContext type() throws RecognitionException {
		TypeContext _localctx = new TypeContext(_ctx, getState());
		enterRule(_localctx, 212, RULE_type);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1532);
			primary();
			setState(1541);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,259,_ctx) ) {
			case 1:
				{
				setState(1534);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1533);
					match(WS);
					}
				}

				setState(1536);
				match(T__35);
				setState(1538);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1537);
					match(WS);
					}
				}

				setState(1540);
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

	public static class LambdaContext extends ParserRuleContext {
		public List<TypeContext> type() {
			return getRuleContexts(TypeContext.class);
		}
		public TypeContext type(int i) {
			return getRuleContext(TypeContext.class,i);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
		}
		public LambdaContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_lambda; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterLambda(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitLambda(this);
		}
	}

	public final LambdaContext lambda() throws RecognitionException {
		LambdaContext _localctx = new LambdaContext(_ctx, getState());
		enterRule(_localctx, 214, RULE_lambda);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1543);
			match(T__5);
			setState(1545);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1544);
				match(WS);
				}
			}

			setState(1547);
			type();
			setState(1558);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,263,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1549);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1548);
						match(WS);
						}
					}

					setState(1551);
					match(T__4);
					setState(1553);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1552);
						match(WS);
						}
					}

					setState(1555);
					type();
					}
					} 
				}
				setState(1560);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,263,_ctx);
			}
			setState(1562);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1561);
				match(WS);
				}
			}

			setState(1564);
			match(T__6);
			setState(1566);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1565);
				match(WS);
				}
			}

			setState(1568);
			match(T__35);
			setState(1570);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1569);
				match(WS);
				}
			}

			setState(1572);
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
	}

	public final Tuple_unitContext tuple_unit() throws RecognitionException {
		Tuple_unitContext _localctx = new Tuple_unitContext(_ctx, getState());
		enterRule(_localctx, 216, RULE_tuple_unit);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1574);
			match(T__56);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final Tuple_singletonContext tuple_singleton() throws RecognitionException {
		Tuple_singletonContext _localctx = new Tuple_singletonContext(_ctx, getState());
		enterRule(_localctx, 218, RULE_tuple_singleton);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1576);
			match(T__5);
			setState(1578);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1577);
				match(WS);
				}
			}

			setState(1580);
			type();
			setState(1582);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1581);
				match(WS);
				}
			}

			setState(1584);
			match(T__6);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final Tuple_multiContext tuple_multi() throws RecognitionException {
		Tuple_multiContext _localctx = new Tuple_multiContext(_ctx, getState());
		enterRule(_localctx, 220, RULE_tuple_multi);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1586);
			match(T__5);
			setState(1588);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1587);
				match(WS);
				}
			}

			setState(1590);
			type();
			setState(1599); 
			_errHandler.sync(this);
			_alt = 1;
			do {
				switch (_alt) {
				case 1:
					{
					{
					setState(1592);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1591);
						match(WS);
						}
					}

					setState(1594);
					match(T__4);
					setState(1596);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1595);
						match(WS);
						}
					}

					setState(1598);
					type();
					}
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(1601); 
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,272,_ctx);
			} while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER );
			setState(1604);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1603);
				match(WS);
				}
			}

			setState(1606);
			match(T__6);
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
	}

	public final TupleContext tuple() throws RecognitionException {
		TupleContext _localctx = new TupleContext(_ctx, getState());
		enterRule(_localctx, 222, RULE_tuple);
		try {
			setState(1611);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,274,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1608);
				tuple_unit();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1609);
				tuple_singleton();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1610);
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

	public static class ParametricContext extends ParserRuleContext {
		public QnameContext qname() {
			return getRuleContext(QnameContext.class,0);
		}
		public List<TypeContext> type() {
			return getRuleContexts(TypeContext.class);
		}
		public TypeContext type(int i) {
			return getRuleContext(TypeContext.class,i);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
		}
		public ParametricContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_parametric; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterParametric(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitParametric(this);
		}
	}

	public final ParametricContext parametric() throws RecognitionException {
		ParametricContext _localctx = new ParametricContext(_ctx, getState());
		enterRule(_localctx, 224, RULE_parametric);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1613);
			qname();
			setState(1615);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1614);
				match(WS);
				}
			}

			setState(1617);
			match(T__11);
			setState(1619);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1618);
				match(WS);
				}
			}

			setState(1621);
			type();
			setState(1632);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,279,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1623);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1622);
						match(WS);
						}
					}

					setState(1625);
					match(T__4);
					setState(1627);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1626);
						match(WS);
						}
					}

					setState(1629);
					type();
					}
					} 
				}
				setState(1634);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,279,_ctx);
			}
			setState(1636);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1635);
				match(WS);
				}
			}

			setState(1638);
			match(T__12);
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
	}

	public final Unary_opsContext unary_ops() throws RecognitionException {
		Unary_opsContext _localctx = new Unary_opsContext(_ctx, getState());
		enterRule(_localctx, 226, RULE_unary_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1640);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__48) | (1L << T__57) | (1L << T__58) | (1L << T__59) | (1L << T__60))) != 0)) ) {
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
	}

	public final Logical_opsContext logical_ops() throws RecognitionException {
		Logical_opsContext _localctx = new Logical_opsContext(_ctx, getState());
		enterRule(_localctx, 228, RULE_logical_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1642);
			_la = _input.LA(1);
			if ( !(((((_la - 62)) & ~0x3f) == 0 && ((1L << (_la - 62)) & ((1L << (T__61 - 62)) | (1L << (T__62 - 62)) | (1L << (T__63 - 62)) | (1L << (T__64 - 62)) | (1L << (T__65 - 62)) | (1L << (T__66 - 62)) | (1L << (T__67 - 62)) | (1L << (T__68 - 62)) | (1L << (T__69 - 62)) | (1L << (T__70 - 62)) | (1L << (T__71 - 62)) | (1L << (T__72 - 62)) | (1L << (T__73 - 62)))) != 0)) ) {
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
	}

	public final Comparison_opsContext comparison_ops() throws RecognitionException {
		Comparison_opsContext _localctx = new Comparison_opsContext(_ctx, getState());
		enterRule(_localctx, 230, RULE_comparison_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1644);
			_la = _input.LA(1);
			if ( !(_la==T__13 || ((((_la - 75)) & ~0x3f) == 0 && ((1L << (_la - 75)) & ((1L << (T__74 - 75)) | (1L << (T__75 - 75)) | (1L << (T__76 - 75)) | (1L << (T__77 - 75)) | (1L << (T__78 - 75)) | (1L << (T__79 - 75)))) != 0)) ) {
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
	}

	public final Multipve_opsContext multipve_ops() throws RecognitionException {
		Multipve_opsContext _localctx = new Multipve_opsContext(_ctx, getState());
		enterRule(_localctx, 232, RULE_multipve_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1646);
			_la = _input.LA(1);
			if ( !(_la==T__1 || ((((_la - 81)) & ~0x3f) == 0 && ((1L << (_la - 81)) & ((1L << (T__80 - 81)) | (1L << (T__81 - 81)) | (1L << (T__82 - 81)))) != 0)) ) {
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
	}

	public final Addve_opsContext addve_ops() throws RecognitionException {
		Addve_opsContext _localctx = new Addve_opsContext(_ctx, getState());
		enterRule(_localctx, 234, RULE_addve_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1648);
			_la = _input.LA(1);
			if ( !(_la==T__48 || _la==T__57) ) {
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
	}

	public final Extbin_opsContext extbin_ops() throws RecognitionException {
		Extbin_opsContext _localctx = new Extbin_opsContext(_ctx, getState());
		enterRule(_localctx, 236, RULE_extbin_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1650);
			_la = _input.LA(1);
			if ( !(((((_la - 84)) & ~0x3f) == 0 && ((1L << (_la - 84)) & ((1L << (T__83 - 84)) | (1L << (T__84 - 84)) | (1L << (T__85 - 84)) | (1L << (T__86 - 84)) | (1L << (T__87 - 84)))) != 0)) ) {
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
		public Pred_ambiguousContext pred_ambiguous() {
			return getRuleContext(Pred_ambiguousContext.class,0);
		}
		public Pred_notequalContext pred_notequal() {
			return getRuleContext(Pred_notequalContext.class,0);
		}
		public Pred_equalContext pred_equal() {
			return getRuleContext(Pred_equalContext.class,0);
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
	}

	public final PredicateContext predicate() throws RecognitionException {
		PredicateContext _localctx = new PredicateContext(_ctx, getState());
		enterRule(_localctx, 238, RULE_predicate);
		try {
			setState(1658);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,281,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1652);
				pred_true();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1653);
				pred_false();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1654);
				pred_ambiguous();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(1655);
				pred_notequal();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(1656);
				pred_equal();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(1657);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final PredicatesContext predicates() throws RecognitionException {
		PredicatesContext _localctx = new PredicatesContext(_ctx, getState());
		enterRule(_localctx, 240, RULE_predicates);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1660);
			predicate();
			setState(1671);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,284,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1662);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1661);
						match(WS);
						}
					}

					setState(1664);
					match(T__4);
					setState(1666);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1665);
						match(WS);
						}
					}

					setState(1668);
					predicate();
					}
					} 
				}
				setState(1673);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,284,_ctx);
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
	}

	public final Pred_trueContext pred_true() throws RecognitionException {
		Pred_trueContext _localctx = new Pred_trueContext(_ctx, getState());
		enterRule(_localctx, 242, RULE_pred_true);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1674);
			match(T__46);
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
	}

	public final Pred_falseContext pred_false() throws RecognitionException {
		Pred_falseContext _localctx = new Pred_falseContext(_ctx, getState());
		enterRule(_localctx, 244, RULE_pred_false);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1676);
			match(T__47);
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

	public static class Pred_ambiguousContext extends ParserRuleContext {
		public QnameContext qname() {
			return getRuleContext(QnameContext.class,0);
		}
		public ExpressionsContext expressions() {
			return getRuleContext(ExpressionsContext.class,0);
		}
		public TerminalNode WS() { return getToken(FlixParser.WS, 0); }
		public Pred_ambiguousContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_pred_ambiguous; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterPred_ambiguous(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitPred_ambiguous(this);
		}
	}

	public final Pred_ambiguousContext pred_ambiguous() throws RecognitionException {
		Pred_ambiguousContext _localctx = new Pred_ambiguousContext(_ctx, getState());
		enterRule(_localctx, 246, RULE_pred_ambiguous);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1678);
			qname();
			setState(1680);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1679);
				match(WS);
				}
			}

			setState(1682);
			match(T__5);
			setState(1683);
			expressions();
			setState(1684);
			match(T__6);
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

	public static class Pred_equalContext extends ParserRuleContext {
		public List<TerminalNode> Ident() { return getTokens(FlixParser.Ident); }
		public TerminalNode Ident(int i) {
			return getToken(FlixParser.Ident, i);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
		}
		public Pred_equalContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_pred_equal; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterPred_equal(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitPred_equal(this);
		}
	}

	public final Pred_equalContext pred_equal() throws RecognitionException {
		Pred_equalContext _localctx = new Pred_equalContext(_ctx, getState());
		enterRule(_localctx, 248, RULE_pred_equal);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1686);
			match(Ident);
			setState(1688);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1687);
				match(WS);
				}
			}

			setState(1690);
			match(T__88);
			setState(1692);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1691);
				match(WS);
				}
			}

			setState(1694);
			match(Ident);
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
		public List<TerminalNode> Ident() { return getTokens(FlixParser.Ident); }
		public TerminalNode Ident(int i) {
			return getToken(FlixParser.Ident, i);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final Pred_notequalContext pred_notequal() throws RecognitionException {
		Pred_notequalContext _localctx = new Pred_notequalContext(_ctx, getState());
		enterRule(_localctx, 250, RULE_pred_notequal);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1696);
			match(Ident);
			setState(1698);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1697);
				match(WS);
				}
			}

			setState(1700);
			match(T__78);
			setState(1702);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1701);
				match(WS);
				}
			}

			setState(1704);
			match(Ident);
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
		public TerminalNode Ident() { return getToken(FlixParser.Ident, 0); }
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
	}

	public final Pred_loopContext pred_loop() throws RecognitionException {
		Pred_loopContext _localctx = new Pred_loopContext(_ctx, getState());
		enterRule(_localctx, 252, RULE_pred_loop);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1706);
			match(Ident);
			setState(1708);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1707);
				match(WS);
				}
			}

			setState(1710);
			match(T__89);
			setState(1712);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1711);
				match(WS);
				}
			}

			setState(1714);
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

	public static final String _serializedATN =
		"\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd\3i\u06b7\4\2\t\2\4"+
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
		"\3\2\5\2\u0102\n\2\3\2\5\2\u0105\n\2\3\2\5\2\u0108\n\2\3\2\5\2\u010b\n"+
		"\2\3\2\5\2\u010e\n\2\3\3\5\3\u0111\n\3\3\3\5\3\u0114\n\3\3\4\3\4\3\4\7"+
		"\4\u0119\n\4\f\4\16\4\u011c\13\4\3\5\3\5\3\5\5\5\u0121\n\5\3\5\3\5\3\6"+
		"\3\6\3\6\3\7\3\7\3\7\5\7\u012b\n\7\3\7\3\7\3\b\3\b\5\b\u0131\n\b\3\b\3"+
		"\b\5\b\u0135\n\b\3\b\7\b\u0138\n\b\f\b\16\b\u013b\13\b\3\t\3\t\5\t\u013f"+
		"\n\t\3\t\5\t\u0142\n\t\3\t\5\t\u0145\n\t\3\t\5\t\u0148\n\t\3\n\3\n\5\n"+
		"\u014c\n\n\3\n\3\n\5\n\u0150\n\n\3\n\3\n\3\13\3\13\5\13\u0156\n\13\3\13"+
		"\3\13\5\13\u015a\n\13\3\13\7\13\u015d\n\13\f\13\16\13\u0160\13\13\3\f"+
		"\3\f\5\f\u0164\n\f\3\f\3\f\5\f\u0168\n\f\3\f\3\f\5\f\u016c\n\f\3\f\7\f"+
		"\u016f\n\f\f\f\16\f\u0172\13\f\5\f\u0174\n\f\3\f\5\f\u0177\n\f\3\f\3\f"+
		"\3\r\3\r\5\r\u017d\n\r\3\r\3\r\5\r\u0181\n\r\3\r\7\r\u0184\n\r\f\r\16"+
		"\r\u0187\13\r\3\16\3\16\5\16\u018b\n\16\3\16\3\16\5\16\u018f\n\16\3\16"+
		"\7\16\u0192\n\16\f\16\16\16\u0195\13\16\3\17\3\17\3\17\3\17\5\17\u019b"+
		"\n\17\3\17\3\17\5\17\u019f\n\17\3\17\3\17\5\17\u01a3\n\17\3\20\3\20\5"+
		"\20\u01a7\n\20\3\20\7\20\u01aa\n\20\f\20\16\20\u01ad\13\20\3\21\3\21\3"+
		"\21\3\21\5\21\u01b3\n\21\3\21\3\21\5\21\u01b7\n\21\3\21\3\21\5\21\u01bb"+
		"\n\21\3\22\3\22\5\22\u01bf\n\22\3\22\7\22\u01c2\n\22\f\22\16\22\u01c5"+
		"\13\22\3\23\3\23\5\23\u01c9\n\23\3\23\3\23\5\23\u01cd\n\23\3\23\5\23\u01d0"+
		"\n\23\3\24\3\24\5\24\u01d4\n\24\3\24\3\24\5\24\u01d8\n\24\3\24\7\24\u01db"+
		"\n\24\f\24\16\24\u01de\13\24\3\25\3\25\5\25\u01e2\n\25\3\25\3\25\5\25"+
		"\u01e6\n\25\3\25\7\25\u01e9\n\25\f\25\16\25\u01ec\13\25\3\26\3\26\3\26"+
		"\5\26\u01f1\n\26\3\26\3\26\5\26\u01f5\n\26\3\26\3\26\3\27\3\27\5\27\u01fb"+
		"\n\27\3\27\3\27\5\27\u01ff\n\27\3\27\7\27\u0202\n\27\f\27\16\27\u0205"+
		"\13\27\3\30\3\30\5\30\u0209\n\30\3\30\3\30\5\30\u020d\n\30\5\30\u020f"+
		"\n\30\3\31\3\31\3\31\7\31\u0214\n\31\f\31\16\31\u0217\13\31\3\32\3\32"+
		"\3\32\5\32\u021c\n\32\3\33\3\33\5\33\u0220\n\33\3\33\7\33\u0223\n\33\f"+
		"\33\16\33\u0226\13\33\3\34\3\34\3\34\3\34\3\34\3\34\3\34\3\35\3\35\3\35"+
		"\3\35\3\35\3\35\3\35\3\36\3\36\3\36\3\36\3\36\3\37\3\37\3\37\3\37\3\37"+
		"\3\37\3\37\3\37\3\37\3\37\3\37\3\37\3\37\5\37\u0248\n\37\3 \3 \5 \u024c"+
		"\n \3 \7 \u024f\n \f \16 \u0252\13 \3!\3!\3!\3!\5!\u0258\n!\3!\3!\5!\u025c"+
		"\n!\3!\5!\u025f\n!\3!\5!\u0262\n!\3!\3!\3!\3\"\3\"\3\"\3\"\5\"\u026b\n"+
		"\"\3\"\3\"\5\"\u026f\n\"\3\"\3\"\5\"\u0273\n\"\3\"\3\"\5\"\u0277\n\"\3"+
		"\"\3\"\5\"\u027b\n\"\7\"\u027d\n\"\f\"\16\"\u0280\13\"\3\"\3\"\3\"\3#"+
		"\3#\3#\3#\5#\u0289\n#\3$\3$\3$\3$\5$\u028f\n$\3$\3$\5$\u0293\n$\3$\5$"+
		"\u0296\n$\3$\5$\u0299\n$\3$\3$\3$\3%\3%\3%\3%\5%\u02a2\n%\3%\3%\5%\u02a6"+
		"\n%\3%\5%\u02a9\n%\3%\5%\u02ac\n%\3%\3%\3%\3&\3&\3&\3&\5&\u02b5\n&\3&"+
		"\3&\5&\u02b9\n&\3&\5&\u02bc\n&\3&\5&\u02bf\n&\3&\3&\3&\3\'\3\'\3\'\3\'"+
		"\5\'\u02c8\n\'\3\'\3\'\5\'\u02cc\n\'\3\'\3\'\5\'\u02d0\n\'\3\'\3\'\3\'"+
		"\3(\3(\5(\u02d7\n(\3(\3(\3(\3(\5(\u02dd\n(\3(\3(\5(\u02e1\n(\3(\3(\5("+
		"\u02e5\n(\3(\3(\3(\3)\3)\3)\7)\u02ed\n)\f)\16)\u02f0\13)\5)\u02f2\n)\3"+
		")\5)\u02f5\n)\3)\3)\3)\3)\5)\u02fb\n)\3)\3)\5)\u02ff\n)\3)\3)\5)\u0303"+
		"\n)\3)\3)\5)\u0307\n)\3)\3)\5)\u030b\n)\3)\3)\3)\3*\3*\3*\3*\5*\u0314"+
		"\n*\3*\3*\5*\u0318\n*\3*\3*\5*\u031c\n*\3*\3*\5*\u0320\n*\3*\3*\5*\u0324"+
		"\n*\3*\3*\5*\u0328\n*\3*\3*\5*\u032c\n*\3*\3*\5*\u0330\n*\3*\3*\3*\3+"+
		"\3+\3+\3+\3+\5+\u033a\n+\3+\3+\5+\u033e\n+\3+\3+\5+\u0342\n+\3+\3+\5+"+
		"\u0346\n+\3+\3+\3,\3,\5,\u034c\n,\3,\5,\u034f\n,\3,\5,\u0352\n,\3,\3,"+
		"\3-\3-\5-\u0358\n-\3-\3-\3.\3.\5.\u035e\n.\3.\3.\5.\u0362\n.\3.\3.\5."+
		"\u0366\n.\3.\3.\3/\3/\3\60\3\60\5\60\u036e\n\60\3\60\3\60\3\60\5\60\u0373"+
		"\n\60\3\60\3\60\5\60\u0377\n\60\3\60\3\60\5\60\u037b\n\60\3\60\3\60\5"+
		"\60\u037f\n\60\3\60\3\60\3\60\3\61\3\61\5\61\u0386\n\61\3\61\3\61\5\61"+
		"\u038a\n\61\3\61\3\61\5\61\u038e\n\61\3\62\3\62\5\62\u0392\n\62\3\62\3"+
		"\62\5\62\u0396\n\62\3\62\7\62\u0399\n\62\f\62\16\62\u039c\13\62\3\63\3"+
		"\63\5\63\u03a0\n\63\3\63\3\63\5\63\u03a4\n\63\3\63\3\63\5\63\u03a8\n\63"+
		"\3\64\3\64\5\64\u03ac\n\64\3\64\3\64\5\64\u03b0\n\64\3\64\3\64\7\64\u03b4"+
		"\n\64\f\64\16\64\u03b7\13\64\3\65\3\65\5\65\u03bb\n\65\3\65\3\65\5\65"+
		"\u03bf\n\65\3\65\3\65\7\65\u03c3\n\65\f\65\16\65\u03c6\13\65\3\66\3\66"+
		"\5\66\u03ca\n\66\3\66\3\66\3\66\3\66\5\66\u03d0\n\66\3\66\3\66\5\66\u03d4"+
		"\n\66\3\67\3\67\5\67\u03d8\n\67\3\67\3\67\5\67\u03dc\n\67\3\67\3\67\5"+
		"\67\u03e0\n\67\38\38\58\u03e4\n8\38\38\38\58\u03e9\n8\39\39\59\u03ed\n"+
		"9\39\39\59\u03f1\n9\39\59\u03f4\n9\3:\3:\3:\3:\3:\3:\3:\3:\3:\3:\3:\3"+
		":\3:\3:\3:\3:\3:\3:\3:\3:\3:\3:\5:\u040c\n:\3;\3;\3;\3;\5;\u0412\n;\3"+
		";\3;\5;\u0416\n;\3;\3;\3;\3;\3;\3;\3<\3<\5<\u0420\n<\3<\3<\5<\u0424\n"+
		"<\3<\3<\5<\u0428\n<\3<\3<\5<\u042c\n<\3<\3<\3<\3<\3<\3<\3=\3=\3=\3=\3"+
		"=\3=\3=\3=\5=\u043c\n=\3=\3=\5=\u0440\n=\3=\3=\3>\3>\3>\3>\3>\3>\3>\3"+
		">\5>\u044c\n>\3>\3>\5>\u0450\n>\3>\3>\3?\3?\5?\u0456\n?\3?\3?\5?\u045a"+
		"\n?\3?\5?\u045d\n?\3?\5?\u0460\n?\3?\5?\u0463\n?\3@\3@\3A\3A\3A\3A\5A"+
		"\u046b\nA\3A\5A\u046e\nA\3B\3B\5B\u0472\nB\3B\5B\u0475\nB\3B\5B\u0478"+
		"\nB\3B\3B\3C\3C\5C\u047e\nC\3C\3C\5C\u0482\nC\3C\3C\3D\3D\5D\u0488\nD"+
		"\3D\3D\5D\u048c\nD\3D\7D\u048f\nD\fD\16D\u0492\13D\3E\3E\3F\3F\3G\3G\3"+
		"H\3H\5H\u049c\nH\3H\3H\5H\u04a0\nH\3H\3H\5H\u04a4\nH\3H\3H\3I\3I\5I\u04aa"+
		"\nI\3I\3I\5I\u04ae\nI\3I\5I\u04b1\nI\3J\3J\5J\u04b5\nJ\3J\5J\u04b8\nJ"+
		"\3J\5J\u04bb\nJ\3J\3J\3K\3K\5K\u04c1\nK\3K\5K\u04c4\nK\3K\5K\u04c7\nK"+
		"\3K\3K\3L\3L\5L\u04cd\nL\3L\5L\u04d0\nL\3L\5L\u04d3\nL\3L\3L\3M\3M\5M"+
		"\u04d9\nM\3M\3M\5M\u04dd\nM\3M\3M\3N\3N\5N\u04e3\nN\3N\3N\5N\u04e7\nN"+
		"\3N\3N\5N\u04eb\nN\3N\3N\5N\u04ef\nN\3N\3N\3O\3O\5O\u04f5\nO\3O\3O\5O"+
		"\u04f9\nO\3O\3O\5O\u04fd\nO\3O\3O\3P\3P\5P\u0503\nP\3P\3P\5P\u0507\nP"+
		"\3P\3P\5P\u050b\nP\3P\3P\3Q\3Q\5Q\u0511\nQ\3Q\3Q\5Q\u0515\nQ\3Q\5Q\u0518"+
		"\nQ\3R\3R\5R\u051c\nR\3R\3R\5R\u0520\nR\3R\7R\u0523\nR\fR\16R\u0526\13"+
		"R\3S\3S\3S\3S\3S\3S\3S\3S\3S\3S\5S\u0532\nS\3T\3T\5T\u0536\nT\3T\3T\5"+
		"T\u053a\nT\3T\3T\3U\3U\5U\u0540\nU\3U\3U\5U\u0544\nU\3U\7U\u0547\nU\f"+
		"U\16U\u054a\13U\3V\3V\3V\3V\5V\u0550\nV\3V\5V\u0553\nV\3W\3W\5W\u0557"+
		"\nW\3W\5W\u055a\nW\3W\5W\u055d\nW\3W\3W\3X\3X\3Y\3Y\3Z\3Z\5Z\u0567\nZ"+
		"\3Z\5Z\u056a\nZ\3Z\5Z\u056d\nZ\3Z\3Z\5Z\u0571\nZ\3Z\5Z\u0574\nZ\3Z\5Z"+
		"\u0577\nZ\3Z\3Z\3[\3[\5[\u057d\n[\3[\5[\u0580\n[\3[\5[\u0583\n[\3[\3["+
		"\5[\u0587\n[\3[\5[\u058a\n[\3[\5[\u058d\n[\3[\3[\3\\\3\\\5\\\u0593\n\\"+
		"\3\\\5\\\u0596\n\\\3\\\5\\\u0599\n\\\3\\\3\\\5\\\u059d\n\\\3\\\5\\\u05a0"+
		"\n\\\3\\\5\\\u05a3\n\\\3\\\3\\\3]\3]\3^\3^\3_\5_\u05ac\n_\3_\3_\3_\3_"+
		"\3_\3`\5`\u05b4\n`\3`\3`\3`\3`\3`\3a\5a\u05bc\na\3a\3a\3a\3a\3b\3b\3b"+
		"\5b\u05c5\nb\3c\5c\u05c8\nc\3c\3c\3c\3d\5d\u05ce\nd\3d\3d\3d\3e\5e\u05d4"+
		"\ne\3e\3e\3e\3f\5f\u05da\nf\3f\3f\3f\3g\5g\u05e0\ng\3g\3g\3g\3h\5h\u05e6"+
		"\nh\3h\3h\3i\3i\3i\3i\3i\3i\5i\u05f0\ni\3j\3j\3j\3j\3j\5j\u05f7\nj\3k"+
		"\3k\3k\3k\5k\u05fd\nk\3l\3l\5l\u0601\nl\3l\3l\5l\u0605\nl\3l\5l\u0608"+
		"\nl\3m\3m\5m\u060c\nm\3m\3m\5m\u0610\nm\3m\3m\5m\u0614\nm\3m\7m\u0617"+
		"\nm\fm\16m\u061a\13m\3m\5m\u061d\nm\3m\3m\5m\u0621\nm\3m\3m\5m\u0625\n"+
		"m\3m\3m\3n\3n\3o\3o\5o\u062d\no\3o\3o\5o\u0631\no\3o\3o\3p\3p\5p\u0637"+
		"\np\3p\3p\5p\u063b\np\3p\3p\5p\u063f\np\3p\6p\u0642\np\rp\16p\u0643\3"+
		"p\5p\u0647\np\3p\3p\3q\3q\3q\5q\u064e\nq\3r\3r\5r\u0652\nr\3r\3r\5r\u0656"+
		"\nr\3r\3r\5r\u065a\nr\3r\3r\5r\u065e\nr\3r\7r\u0661\nr\fr\16r\u0664\13"+
		"r\3r\5r\u0667\nr\3r\3r\3s\3s\3t\3t\3u\3u\3v\3v\3w\3w\3x\3x\3y\3y\3y\3"+
		"y\3y\3y\5y\u067d\ny\3z\3z\5z\u0681\nz\3z\3z\5z\u0685\nz\3z\7z\u0688\n"+
		"z\fz\16z\u068b\13z\3{\3{\3|\3|\3}\3}\5}\u0693\n}\3}\3}\3}\3}\3~\3~\5~"+
		"\u069b\n~\3~\3~\5~\u069f\n~\3~\3~\3\177\3\177\5\177\u06a5\n\177\3\177"+
		"\3\177\5\177\u06a9\n\177\3\177\3\177\3\u0080\3\u0080\5\u0080\u06af\n\u0080"+
		"\3\u0080\3\u0080\5\u0080\u06b3\n\u0080\3\u0080\3\u0080\3\u0080\2\2\u0081"+
		"\2\4\6\b\n\f\16\20\22\24\26\30\32\34\36 \"$&(*,.\60\62\64\668:<>@BDFH"+
		"JLNPRTVXZ\\^`bdfhjlnprtvxz|~\u0080\u0082\u0084\u0086\u0088\u008a\u008c"+
		"\u008e\u0090\u0092\u0094\u0096\u0098\u009a\u009c\u009e\u00a0\u00a2\u00a4"+
		"\u00a6\u00a8\u00aa\u00ac\u00ae\u00b0\u00b2\u00b4\u00b6\u00b8\u00ba\u00bc"+
		"\u00be\u00c0\u00c2\u00c4\u00c6\u00c8\u00ca\u00cc\u00ce\u00d0\u00d2\u00d4"+
		"\u00d6\u00d8\u00da\u00dc\u00de\u00e0\u00e2\u00e4\u00e6\u00e8\u00ea\u00ec"+
		"\u00ee\u00f0\u00f2\u00f4\u00f6\u00f8\u00fa\u00fc\u00fe\2\13\3\2,-\3\2"+
		"./\3\2\61\62\4\2\63\63<?\3\2@L\4\2\20\20MR\4\2\4\4SU\4\2\63\63<<\3\2V"+
		"Z\u0792\2\u0101\3\2\2\2\4\u0113\3\2\2\2\6\u0115\3\2\2\2\b\u0120\3\2\2"+
		"\2\n\u0124\3\2\2\2\f\u0127\3\2\2\2\16\u012e\3\2\2\2\20\u0147\3\2\2\2\22"+
		"\u0149\3\2\2\2\24\u0153\3\2\2\2\26\u0161\3\2\2\2\30\u017a\3\2\2\2\32\u0188"+
		"\3\2\2\2\34\u0196\3\2\2\2\36\u01a4\3\2\2\2 \u01ae\3\2\2\2\"\u01bc\3\2"+
		"\2\2$\u01c6\3\2\2\2&\u01d1\3\2\2\2(\u01df\3\2\2\2*\u01ed\3\2\2\2,\u01f8"+
		"\3\2\2\2.\u020e\3\2\2\2\60\u0210\3\2\2\2\62\u021b\3\2\2\2\64\u021d\3\2"+
		"\2\2\66\u0227\3\2\2\28\u022e\3\2\2\2:\u0235\3\2\2\2<\u0247\3\2\2\2>\u0249"+
		"\3\2\2\2@\u0253\3\2\2\2B\u0266\3\2\2\2D\u0284\3\2\2\2F\u028a\3\2\2\2H"+
		"\u029d\3\2\2\2J\u02b0\3\2\2\2L\u02c3\3\2\2\2N\u02d4\3\2\2\2P\u02f1\3\2"+
		"\2\2R\u030f\3\2\2\2T\u0334\3\2\2\2V\u0349\3\2\2\2X\u0355\3\2\2\2Z\u035b"+
		"\3\2\2\2\\\u0369\3\2\2\2^\u036b\3\2\2\2`\u0383\3\2\2\2b\u038f\3\2\2\2"+
		"d\u039d\3\2\2\2f\u03a9\3\2\2\2h\u03b8\3\2\2\2j\u03c7\3\2\2\2l\u03d5\3"+
		"\2\2\2n\u03e8\3\2\2\2p\u03ea\3\2\2\2r\u040b\3\2\2\2t\u040d\3\2\2\2v\u041d"+
		"\3\2\2\2x\u0433\3\2\2\2z\u0443\3\2\2\2|\u0453\3\2\2\2~\u0464\3\2\2\2\u0080"+
		"\u0466\3\2\2\2\u0082\u046f\3\2\2\2\u0084\u047b\3\2\2\2\u0086\u0485\3\2"+
		"\2\2\u0088\u0493\3\2\2\2\u008a\u0495\3\2\2\2\u008c\u0497\3\2\2\2\u008e"+
		"\u0499\3\2\2\2\u0090\u04a7\3\2\2\2\u0092\u04b2\3\2\2\2\u0094\u04be\3\2"+
		"\2\2\u0096\u04ca\3\2\2\2\u0098\u04d6\3\2\2\2\u009a\u04e0\3\2\2\2\u009c"+
		"\u04f2\3\2\2\2\u009e\u0500\3\2\2\2\u00a0\u050e\3\2\2\2\u00a2\u0519\3\2"+
		"\2\2\u00a4\u0531\3\2\2\2\u00a6\u0533\3\2\2\2\u00a8\u053d\3\2\2\2\u00aa"+
		"\u054b\3\2\2\2\u00ac\u0554\3\2\2\2\u00ae\u0560\3\2\2\2\u00b0\u0562\3\2"+
		"\2\2\u00b2\u0564\3\2\2\2\u00b4\u057a\3\2\2\2\u00b6\u0590\3\2\2\2\u00b8"+
		"\u05a6\3\2\2\2\u00ba\u05a8\3\2\2\2\u00bc\u05ab\3\2\2\2\u00be\u05b3\3\2"+
		"\2\2\u00c0\u05bb\3\2\2\2\u00c2\u05c4\3\2\2\2\u00c4\u05c7\3\2\2\2\u00c6"+
		"\u05cd\3\2\2\2\u00c8\u05d3\3\2\2\2\u00ca\u05d9\3\2\2\2\u00cc\u05df\3\2"+
		"\2\2\u00ce\u05e5\3\2\2\2\u00d0\u05ef\3\2\2\2\u00d2\u05f6\3\2\2\2\u00d4"+
		"\u05fc\3\2\2\2\u00d6\u05fe\3\2\2\2\u00d8\u0609\3\2\2\2\u00da\u0628\3\2"+
		"\2\2\u00dc\u062a\3\2\2\2\u00de\u0634\3\2\2\2\u00e0\u064d\3\2\2\2\u00e2"+
		"\u064f\3\2\2\2\u00e4\u066a\3\2\2\2\u00e6\u066c\3\2\2\2\u00e8\u066e\3\2"+
		"\2\2\u00ea\u0670\3\2\2\2\u00ec\u0672\3\2\2\2\u00ee\u0674\3\2\2\2\u00f0"+
		"\u067c\3\2\2\2\u00f2\u067e\3\2\2\2\u00f4\u068c\3\2\2\2\u00f6\u068e\3\2"+
		"\2\2\u00f8\u0690\3\2\2\2\u00fa\u0698\3\2\2\2\u00fc\u06a2\3\2\2\2\u00fe"+
		"\u06ac\3\2\2\2\u0100\u0102\7]\2\2\u0101\u0100\3\2\2\2\u0101\u0102\3\2"+
		"\2\2\u0102\u0104\3\2\2\2\u0103\u0105\5\64\33\2\u0104\u0103\3\2\2\2\u0104"+
		"\u0105\3\2\2\2\u0105\u0107\3\2\2\2\u0106\u0108\7]\2\2\u0107\u0106\3\2"+
		"\2\2\u0107\u0108\3\2\2\2\u0108\u010a\3\2\2\2\u0109\u010b\5> \2\u010a\u0109"+
		"\3\2\2\2\u010a\u010b\3\2\2\2\u010b\u010d\3\2\2\2\u010c\u010e\7]\2\2\u010d"+
		"\u010c\3\2\2\2\u010d\u010e\3\2\2\2\u010e\3\3\2\2\2\u010f\u0111\7]\2\2"+
		"\u0110\u010f\3\2\2\2\u0110\u0111\3\2\2\2\u0111\u0112\3\2\2\2\u0112\u0114"+
		"\7^\2\2\u0113\u0110\3\2\2\2\u0113\u0114\3\2\2\2\u0114\5\3\2\2\2\u0115"+
		"\u011a\7`\2\2\u0116\u0117\7\3\2\2\u0117\u0119\7`\2\2\u0118\u0116\3\2\2"+
		"\2\u0119\u011c\3\2\2\2\u011a\u0118\3\2\2\2\u011a\u011b\3\2\2\2\u011b\7"+
		"\3\2\2\2\u011c\u011a\3\2\2\2\u011d\u011e\5\6\4\2\u011e\u011f\7\4\2\2\u011f"+
		"\u0121\3\2\2\2\u0120\u011d\3\2\2\2\u0120\u0121\3\2\2\2\u0121\u0122\3\2"+
		"\2\2\u0122\u0123\7`\2\2\u0123\t\3\2\2\2\u0124\u0125\7\5\2\2\u0125\u0126"+
		"\7`\2\2\u0126\13\3\2\2\2\u0127\u0128\7`\2\2\u0128\u012a\7\6\2\2\u0129"+
		"\u012b\7]\2\2\u012a\u0129\3\2\2\2\u012a\u012b\3\2\2\2\u012b\u012c\3\2"+
		"\2\2\u012c\u012d\5\u00d6l\2\u012d\r\3\2\2\2\u012e\u0139\5\f\7\2\u012f"+
		"\u0131\7]\2\2\u0130\u012f\3\2\2\2\u0130\u0131\3\2\2\2\u0131\u0132\3\2"+
		"\2\2\u0132\u0134\7\7\2\2\u0133\u0135\7]\2\2\u0134\u0133\3\2\2\2\u0134"+
		"\u0135\3\2\2\2\u0135\u0136\3\2\2\2\u0136\u0138\5\f\7\2\u0137\u0130\3\2"+
		"\2\2\u0138\u013b\3\2\2\2\u0139\u0137\3\2\2\2\u0139\u013a\3\2\2\2\u013a"+
		"\17\3\2\2\2\u013b\u0139\3\2\2\2\u013c\u013e\7\b\2\2\u013d\u013f\7]\2\2"+
		"\u013e\u013d\3\2\2\2\u013e\u013f\3\2\2\2\u013f\u0141\3\2\2\2\u0140\u0142"+
		"\5\16\b\2\u0141\u0140\3\2\2\2\u0141\u0142\3\2\2\2\u0142\u0144\3\2\2\2"+
		"\u0143\u0145\7]\2\2\u0144\u0143\3\2\2\2\u0144\u0145\3\2\2\2\u0145\u0146"+
		"\3\2\2\2\u0146\u0148\7\t\2\2\u0147\u013c\3\2\2\2\u0147\u0148\3\2\2\2\u0148"+
		"\21\3\2\2\2\u0149\u014b\7`\2\2\u014a\u014c\7]\2\2\u014b\u014a\3\2\2\2"+
		"\u014b\u014c\3\2\2\2\u014c\u014d\3\2\2\2\u014d\u014f\7\6\2\2\u014e\u0150"+
		"\7]\2\2\u014f\u014e\3\2\2\2\u014f\u0150\3\2\2\2\u0150\u0151\3\2\2\2\u0151"+
		"\u0152\5\u00d6l\2\u0152\23\3\2\2\2\u0153\u015e\5\22\n\2\u0154\u0156\7"+
		"]\2\2\u0155\u0154\3\2\2\2\u0155\u0156\3\2\2\2\u0156\u0157\3\2\2\2\u0157"+
		"\u0159\7\7\2\2\u0158\u015a\7]\2\2\u0159\u0158\3\2\2\2\u0159\u015a\3\2"+
		"\2\2\u015a\u015b\3\2\2\2\u015b\u015d\5\22\n\2\u015c\u0155\3\2\2\2\u015d"+
		"\u0160\3\2\2\2\u015e\u015c\3\2\2\2\u015e\u015f\3\2\2\2\u015f\25\3\2\2"+
		"\2\u0160\u015e\3\2\2\2\u0161\u0163\7\n\2\2\u0162\u0164\7]\2\2\u0163\u0162"+
		"\3\2\2\2\u0163\u0164\3\2\2\2\u0164\u0173\3\2\2\2\u0165\u0170\7`\2\2\u0166"+
		"\u0168\7]\2\2\u0167\u0166\3\2\2\2\u0167\u0168\3\2\2\2\u0168\u0169\3\2"+
		"\2\2\u0169\u016b\7\7\2\2\u016a\u016c\7]\2\2\u016b\u016a\3\2\2\2\u016b"+
		"\u016c\3\2\2\2\u016c\u016d\3\2\2\2\u016d\u016f\7`\2\2\u016e\u0167\3\2"+
		"\2\2\u016f\u0172\3\2\2\2\u0170\u016e\3\2\2\2\u0170\u0171\3\2\2\2\u0171"+
		"\u0174\3\2\2\2\u0172\u0170\3\2\2\2\u0173\u0165\3\2\2\2\u0173\u0174\3\2"+
		"\2\2\u0174\u0176\3\2\2\2\u0175\u0177\7]\2\2\u0176\u0175\3\2\2\2\u0176"+
		"\u0177\3\2\2\2\u0177\u0178\3\2\2\2\u0178\u0179\7\13\2\2\u0179\27\3\2\2"+
		"\2\u017a\u0185\5\26\f\2\u017b\u017d\7]\2\2\u017c\u017b\3\2\2\2\u017c\u017d"+
		"\3\2\2\2\u017d\u017e\3\2\2\2\u017e\u0180\7\7\2\2\u017f\u0181\7]\2\2\u0180"+
		"\u017f\3\2\2\2\u0180\u0181\3\2\2\2\u0181\u0182\3\2\2\2\u0182\u0184\5\26"+
		"\f\2\u0183\u017c\3\2\2\2\u0184\u0187\3\2\2\2\u0185\u0183\3\2\2\2\u0185"+
		"\u0186\3\2\2\2\u0186\31\3\2\2\2\u0187\u0185\3\2\2\2\u0188\u0193\7`\2\2"+
		"\u0189\u018b\7]\2\2\u018a\u0189\3\2\2\2\u018a\u018b\3\2\2\2\u018b\u018c"+
		"\3\2\2\2\u018c\u018e\7\7\2\2\u018d\u018f\7]\2\2\u018e\u018d\3\2\2\2\u018e"+
		"\u018f\3\2\2\2\u018f\u0190\3\2\2\2\u0190\u0192\7`\2\2\u0191\u018a\3\2"+
		"\2\2\u0192\u0195\3\2\2\2\u0193\u0191\3\2\2\2\u0193\u0194\3\2\2\2\u0194"+
		"\33\3\2\2\2\u0195\u0193\3\2\2\2\u0196\u0197\7\f\2\2\u0197\u0198\7]\2\2"+
		"\u0198\u019a\5\u00a0Q\2\u0199\u019b\7]\2\2\u019a\u0199\3\2\2\2\u019a\u019b"+
		"\3\2\2\2\u019b\u019c\3\2\2\2\u019c\u019e\7\r\2\2\u019d\u019f\7]\2\2\u019e"+
		"\u019d\3\2\2\2\u019e\u019f\3\2\2\2\u019f\u01a0\3\2\2\2\u01a0\u01a2\5`"+
		"\61\2\u01a1\u01a3\7^\2\2\u01a2\u01a1\3\2\2\2\u01a2\u01a3\3\2\2\2\u01a3"+
		"\35\3\2\2\2\u01a4\u01ab\5\34\17\2\u01a5\u01a7\7]\2\2\u01a6\u01a5\3\2\2"+
		"\2\u01a6\u01a7\3\2\2\2\u01a7\u01a8\3\2\2\2\u01a8\u01aa\5\34\17\2\u01a9"+
		"\u01a6\3\2\2\2\u01aa\u01ad\3\2\2\2\u01ab\u01a9\3\2\2\2\u01ab\u01ac\3\2"+
		"\2\2\u01ac\37\3\2\2\2\u01ad\u01ab\3\2\2\2\u01ae\u01af\7\f\2\2\u01af\u01b0"+
		"\7]\2\2\u01b0\u01b2\5`\61\2\u01b1\u01b3\7]\2\2\u01b2\u01b1\3\2\2\2\u01b2"+
		"\u01b3\3\2\2\2\u01b3\u01b4\3\2\2\2\u01b4\u01b6\7\r\2\2\u01b5\u01b7\7]"+
		"\2\2\u01b6\u01b5\3\2\2\2\u01b6\u01b7\3\2\2\2\u01b7\u01b8\3\2\2\2\u01b8"+
		"\u01ba\5`\61\2\u01b9\u01bb\7^\2\2\u01ba\u01b9\3\2\2\2\u01ba\u01bb\3\2"+
		"\2\2\u01bb!\3\2\2\2\u01bc\u01c3\5 \21\2\u01bd\u01bf\7]\2\2\u01be\u01bd"+
		"\3\2\2\2\u01be\u01bf\3\2\2\2\u01bf\u01c0\3\2\2\2\u01c0\u01c2\5 \21\2\u01c1"+
		"\u01be\3\2\2\2\u01c2\u01c5\3\2\2\2\u01c3\u01c1\3\2\2\2\u01c3\u01c4\3\2"+
		"\2\2\u01c4#\3\2\2\2\u01c5\u01c3\3\2\2\2\u01c6\u01cf\7`\2\2\u01c7\u01c9"+
		"\7]\2\2\u01c8\u01c7\3\2\2\2\u01c8\u01c9\3\2\2\2\u01c9\u01ca\3\2\2\2\u01ca"+
		"\u01cc\7\6\2\2\u01cb\u01cd\7]\2\2\u01cc\u01cb\3\2\2\2\u01cc\u01cd\3\2"+
		"\2\2\u01cd\u01ce\3\2\2\2\u01ce\u01d0\5\u00d6l\2\u01cf\u01c8\3\2\2\2\u01cf"+
		"\u01d0\3\2\2\2\u01d0%\3\2\2\2\u01d1\u01dc\5$\23\2\u01d2\u01d4\7]\2\2\u01d3"+
		"\u01d2\3\2\2\2\u01d3\u01d4\3\2\2\2\u01d4\u01d5\3\2\2\2\u01d5\u01d7\7\7"+
		"\2\2\u01d6\u01d8\7]\2\2\u01d7\u01d6\3\2\2\2\u01d7\u01d8\3\2\2\2\u01d8"+
		"\u01d9\3\2\2\2\u01d9\u01db\5$\23\2\u01da\u01d3\3\2\2\2\u01db\u01de\3\2"+
		"\2\2\u01dc\u01da\3\2\2\2\u01dc\u01dd\3\2\2\2\u01dd\'\3\2\2\2\u01de\u01dc"+
		"\3\2\2\2\u01df\u01ea\5\u00d6l\2\u01e0\u01e2\7]\2\2\u01e1\u01e0\3\2\2\2"+
		"\u01e1\u01e2\3\2\2\2\u01e2\u01e3\3\2\2\2\u01e3\u01e5\7\7\2\2\u01e4\u01e6"+
		"\7]\2\2\u01e5\u01e4\3\2\2\2\u01e5\u01e6\3\2\2\2\u01e6\u01e7\3\2\2\2\u01e7"+
		"\u01e9\5\u00d6l\2\u01e8\u01e1\3\2\2\2\u01e9\u01ec\3\2\2\2\u01ea\u01e8"+
		"\3\2\2\2\u01ea\u01eb\3\2\2\2\u01eb)\3\2\2\2\u01ec\u01ea\3\2\2\2\u01ed"+
		"\u01ee\7`\2\2\u01ee\u01f0\7\16\2\2\u01ef\u01f1\7]\2\2\u01f0\u01ef\3\2"+
		"\2\2\u01f0\u01f1\3\2\2\2\u01f1\u01f2\3\2\2\2\u01f2\u01f4\5(\25\2\u01f3"+
		"\u01f5\7]\2\2\u01f4\u01f3\3\2\2\2\u01f4\u01f5\3\2\2\2\u01f5\u01f6\3\2"+
		"\2\2\u01f6\u01f7\7\17\2\2\u01f7+\3\2\2\2\u01f8\u0203\5*\26\2\u01f9\u01fb"+
		"\7]\2\2\u01fa\u01f9\3\2\2\2\u01fa\u01fb\3\2\2\2\u01fb\u01fc\3\2\2\2\u01fc"+
		"\u01fe\7\7\2\2\u01fd\u01ff\7]\2\2\u01fe\u01fd\3\2\2\2\u01fe\u01ff\3\2"+
		"\2\2\u01ff\u0200\3\2\2\2\u0200\u0202\5*\26\2\u0201\u01fa\3\2\2\2\u0202"+
		"\u0205\3\2\2\2\u0203\u0201\3\2\2\2\u0203\u0204\3\2\2\2\u0204-\3\2\2\2"+
		"\u0205\u0203\3\2\2\2\u0206\u0208\7\20\2\2\u0207\u0209\7]\2\2\u0208\u0207"+
		"\3\2\2\2\u0208\u0209\3\2\2\2\u0209\u020a\3\2\2\2\u020a\u020c\5,\27\2\u020b"+
		"\u020d\7]\2\2\u020c\u020b\3\2\2\2\u020c\u020d\3\2\2\2\u020d\u020f\3\2"+
		"\2\2\u020e\u0206\3\2\2\2\u020e\u020f\3\2\2\2\u020f/\3\2\2\2\u0210\u0215"+
		"\5P)\2\u0211\u0212\7]\2\2\u0212\u0214\5P)\2\u0213\u0211\3\2\2\2\u0214"+
		"\u0217\3\2\2\2\u0215\u0213\3\2\2\2\u0215\u0216\3\2\2\2\u0216\61\3\2\2"+
		"\2\u0217\u0215\3\2\2\2\u0218\u021c\5\66\34\2\u0219\u021c\58\35\2\u021a"+
		"\u021c\5:\36\2\u021b\u0218\3\2\2\2\u021b\u0219\3\2\2\2\u021b\u021a\3\2"+
		"\2\2\u021c\63\3\2\2\2\u021d\u0224\5\62\32\2\u021e\u0220\7]\2\2\u021f\u021e"+
		"\3\2\2\2\u021f\u0220\3\2\2\2\u0220\u0221\3\2\2\2\u0221\u0223\5\62\32\2"+
		"\u0222\u021f\3\2\2\2\u0223\u0226\3\2\2\2\u0224\u0222\3\2\2\2\u0224\u0225"+
		"\3\2\2\2\u0225\65\3\2\2\2\u0226\u0224\3\2\2\2\u0227\u0228\7\21\2\2\u0228"+
		"\u0229\7]\2\2\u0229\u022a\5\6\4\2\u022a\u022b\7\4\2\2\u022b\u022c\7e\2"+
		"\2\u022c\u022d\5\4\3\2\u022d\67\3\2\2\2\u022e\u022f\7\21\2\2\u022f\u0230"+
		"\7]\2\2\u0230\u0231\5\6\4\2\u0231\u0232\7\4\2\2\u0232\u0233\7`\2\2\u0233"+
		"\u0234\5\4\3\2\u02349\3\2\2\2\u0235\u0236\7\21\2\2\u0236\u0237\7]\2\2"+
		"\u0237\u0238\5\6\4\2\u0238\u0239\5\4\3\2\u0239;\3\2\2\2\u023a\u0248\5"+
		"@!\2\u023b\u0248\5B\"\2\u023c\u0248\5F$\2\u023d\u0248\5H%\2\u023e\u0248"+
		"\5J&\2\u023f\u0248\5L\'\2\u0240\u0248\5N(\2\u0241\u0248\5P)\2\u0242\u0248"+
		"\5R*\2\u0243\u0248\5T+\2\u0244\u0248\5X-\2\u0245\u0248\5Z.\2\u0246\u0248"+
		"\5^\60\2\u0247\u023a\3\2\2\2\u0247\u023b\3\2\2\2\u0247\u023c\3\2\2\2\u0247"+
		"\u023d\3\2\2\2\u0247\u023e\3\2\2\2\u0247\u023f\3\2\2\2\u0247\u0240\3\2"+
		"\2\2\u0247\u0241\3\2\2\2\u0247\u0242\3\2\2\2\u0247\u0243\3\2\2\2\u0247"+
		"\u0244\3\2\2\2\u0247\u0245\3\2\2\2\u0247\u0246\3\2\2\2\u0248=\3\2\2\2"+
		"\u0249\u0250\5<\37\2\u024a\u024c\7]\2\2\u024b\u024a\3\2\2\2\u024b\u024c"+
		"\3\2\2\2\u024c\u024d\3\2\2\2\u024d\u024f\5<\37\2\u024e\u024b\3\2\2\2\u024f"+
		"\u0252\3\2\2\2\u0250\u024e\3\2\2\2\u0250\u0251\3\2\2\2\u0251?\3\2\2\2"+
		"\u0252\u0250\3\2\2\2\u0253\u0254\7\22\2\2\u0254\u0255\7]\2\2\u0255\u0257"+
		"\5\6\4\2\u0256\u0258\7]\2\2\u0257\u0256\3\2\2\2\u0257\u0258\3\2\2\2\u0258"+
		"\u0259\3\2\2\2\u0259\u025b\7\n\2\2\u025a\u025c\7]\2\2\u025b\u025a\3\2"+
		"\2\2\u025b\u025c\3\2\2\2\u025c\u025e\3\2\2\2\u025d\u025f\5> \2\u025e\u025d"+
		"\3\2\2\2\u025e\u025f\3\2\2\2\u025f\u0261\3\2\2\2\u0260\u0262\7]\2\2\u0261"+
		"\u0260\3\2\2\2\u0261\u0262\3\2\2\2\u0262\u0263\3\2\2\2\u0263\u0264\7\13"+
		"\2\2\u0264\u0265\5\4\3\2\u0265A\3\2\2\2\u0266\u0267\7\23\2\2\u0267\u0268"+
		"\7]\2\2\u0268\u026a\7`\2\2\u0269\u026b\7]\2\2\u026a\u0269\3\2\2\2\u026a"+
		"\u026b\3\2\2\2\u026b\u026c\3\2\2\2\u026c\u026e\7\n\2\2\u026d\u026f\7]"+
		"\2\2\u026e\u026d\3\2\2\2\u026e\u026f\3\2\2\2\u026f\u0270\3\2\2\2\u0270"+
		"\u0272\5D#\2\u0271\u0273\7]\2\2\u0272\u0271\3\2\2\2\u0272\u0273\3\2\2"+
		"\2\u0273\u027e\3\2\2\2\u0274\u0276\7\7\2\2\u0275\u0277\7]\2\2\u0276\u0275"+
		"\3\2\2\2\u0276\u0277\3\2\2\2\u0277\u0278\3\2\2\2\u0278\u027a\5D#\2\u0279"+
		"\u027b\7]\2\2\u027a\u0279\3\2\2\2\u027a\u027b\3\2\2\2\u027b\u027d\3\2"+
		"\2\2\u027c\u0274\3\2\2\2\u027d\u0280\3\2\2\2\u027e\u027c\3\2\2\2\u027e"+
		"\u027f\3\2\2\2\u027f\u0281\3\2\2\2\u0280\u027e\3\2\2\2\u0281\u0282\7\13"+
		"\2\2\u0282\u0283\5\4\3\2\u0283C\3\2\2\2\u0284\u0285\7\f\2\2\u0285\u0286"+
		"\7]\2\2\u0286\u0288\7`\2\2\u0287\u0289\5\u00e0q\2\u0288\u0287\3\2\2\2"+
		"\u0288\u0289\3\2\2\2\u0289E\3\2\2\2\u028a\u028b\7\24\2\2\u028b\u028c\7"+
		"]\2\2\u028c\u028e\7`\2\2\u028d\u028f\7]\2\2\u028e\u028d\3\2\2\2\u028e"+
		"\u028f\3\2\2\2\u028f\u0290\3\2\2\2\u0290\u0292\7\b\2\2\u0291\u0293\7]"+
		"\2\2\u0292\u0291\3\2\2\2\u0292\u0293\3\2\2\2\u0293\u0295\3\2\2\2\u0294"+
		"\u0296\5\24\13\2\u0295\u0294\3\2\2\2\u0295\u0296\3\2\2\2\u0296\u0298\3"+
		"\2\2\2\u0297\u0299\7]\2\2\u0298\u0297\3\2\2\2\u0298\u0299\3\2\2\2\u0299"+
		"\u029a\3\2\2\2\u029a\u029b\7\t\2\2\u029b\u029c\5\4\3\2\u029cG\3\2\2\2"+
		"\u029d\u029e\7\25\2\2\u029e\u029f\7]\2\2\u029f\u02a1\7`\2\2\u02a0\u02a2"+
		"\7]\2\2\u02a1\u02a0\3\2\2\2\u02a1\u02a2\3\2\2\2\u02a2\u02a3\3\2\2\2\u02a3"+
		"\u02a5\7\b\2\2\u02a4\u02a6\7]\2\2\u02a5\u02a4\3\2\2\2\u02a5\u02a6\3\2"+
		"\2\2\u02a6\u02a8\3\2\2\2\u02a7\u02a9\5\24\13\2\u02a8\u02a7\3\2\2\2\u02a8"+
		"\u02a9\3\2\2\2\u02a9\u02ab\3\2\2\2\u02aa\u02ac\7]\2\2\u02ab\u02aa\3\2"+
		"\2\2\u02ab\u02ac\3\2\2\2\u02ac\u02ad\3\2\2\2\u02ad\u02ae\7\t\2\2\u02ae"+
		"\u02af\5\4\3\2\u02afI\3\2\2\2\u02b0\u02b1\7\26\2\2\u02b1\u02b2\7]\2\2"+
		"\u02b2\u02b4\7`\2\2\u02b3\u02b5\7]\2\2\u02b4\u02b3\3\2\2\2\u02b4\u02b5"+
		"\3\2\2\2\u02b5\u02b6\3\2\2\2\u02b6\u02b8\7\b\2\2\u02b7\u02b9\7]\2\2\u02b8"+
		"\u02b7\3\2\2\2\u02b8\u02b9\3\2\2\2\u02b9\u02bb\3\2\2\2\u02ba\u02bc\5\30"+
		"\r\2\u02bb\u02ba\3\2\2\2\u02bb\u02bc\3\2\2\2\u02bc\u02be\3\2\2\2\u02bd"+
		"\u02bf\7]\2\2\u02be\u02bd\3\2\2\2\u02be\u02bf\3\2\2\2\u02bf\u02c0\3\2"+
		"\2\2\u02c0\u02c1\7\t\2\2\u02c1\u02c2\5\4\3\2\u02c2K\3\2\2\2\u02c3\u02c4"+
		"\7\27\2\2\u02c4\u02c5\7]\2\2\u02c5\u02c7\7`\2\2\u02c6\u02c8\7]\2\2\u02c7"+
		"\u02c6\3\2\2\2\u02c7\u02c8\3\2\2\2\u02c8\u02c9\3\2\2\2\u02c9\u02cb\5\20"+
		"\t\2\u02ca\u02cc\7]\2\2\u02cb\u02ca\3\2\2\2\u02cb\u02cc\3\2\2\2\u02cc"+
		"\u02cd\3\2\2\2\u02cd\u02cf\7\6\2\2\u02ce\u02d0\7]\2\2\u02cf\u02ce\3\2"+
		"\2\2\u02cf\u02d0\3\2\2\2\u02d0\u02d1\3\2\2\2\u02d1\u02d2\5\u00d6l\2\u02d2"+
		"\u02d3\5\4\3\2\u02d3M\3\2\2\2\u02d4\u02d6\7\30\2\2\u02d5\u02d7\7]\2\2"+
		"\u02d6\u02d5\3\2\2\2\u02d6\u02d7\3\2\2\2\u02d7\u02d8\3\2\2\2\u02d8\u02d9"+
		"\7\27\2\2\u02d9\u02da\7]\2\2\u02da\u02dc\7`\2\2\u02db\u02dd\7]\2\2\u02dc"+
		"\u02db\3\2\2\2\u02dc\u02dd\3\2\2\2\u02dd\u02de\3\2\2\2\u02de\u02e0\5\20"+
		"\t\2\u02df\u02e1\7]\2\2\u02e0\u02df\3\2\2\2\u02e0\u02e1\3\2\2\2\u02e1"+
		"\u02e2\3\2\2\2\u02e2\u02e4\7\6\2\2\u02e3\u02e5\7]\2\2\u02e4\u02e3\3\2"+
		"\2\2\u02e4\u02e5\3\2\2\2\u02e5\u02e6\3\2\2\2\u02e6\u02e7\5\u00d6l\2\u02e7"+
		"\u02e8\5\4\3\2\u02e8O\3\2\2\2\u02e9\u02ee\5\n\6\2\u02ea\u02eb\7]\2\2\u02eb"+
		"\u02ed\5\n\6\2\u02ec\u02ea\3\2\2\2\u02ed\u02f0\3\2\2\2\u02ee\u02ec\3\2"+
		"\2\2\u02ee\u02ef\3\2\2\2\u02ef\u02f2\3\2\2\2\u02f0\u02ee\3\2\2\2\u02f1"+
		"\u02e9\3\2\2\2\u02f1\u02f2\3\2\2\2\u02f2\u02f4\3\2\2\2\u02f3\u02f5\7]"+
		"\2\2\u02f4\u02f3\3\2\2\2\u02f4\u02f5\3\2\2\2\u02f5\u02f6\3\2\2\2\u02f6"+
		"\u02f7\7\27\2\2\u02f7\u02f8\7]\2\2\u02f8\u02fa\7`\2\2\u02f9\u02fb\7]\2"+
		"\2\u02fa\u02f9\3\2\2\2\u02fa\u02fb\3\2\2\2\u02fb\u02fc\3\2\2\2\u02fc\u02fe"+
		"\5\20\t\2\u02fd\u02ff\7]\2\2\u02fe\u02fd\3\2\2\2\u02fe\u02ff\3\2\2\2\u02ff"+
		"\u0300\3\2\2\2\u0300\u0302\7\6\2\2\u0301\u0303\7]\2\2\u0302\u0301\3\2"+
		"\2\2\u0302\u0303\3\2\2\2\u0303\u0304\3\2\2\2\u0304\u0306\5\u00d6l\2\u0305"+
		"\u0307\7]\2\2\u0306\u0305\3\2\2\2\u0306\u0307\3\2\2\2\u0307\u0308\3\2"+
		"\2\2\u0308\u030a\7\31\2\2\u0309\u030b\7]\2\2\u030a\u0309\3\2\2\2\u030a"+
		"\u030b\3\2\2\2\u030b\u030c\3\2\2\2\u030c\u030d\5`\61\2\u030d\u030e\5\4"+
		"\3\2\u030eQ\3\2\2\2\u030f\u0310\7\32\2\2\u0310\u0311\7]\2\2\u0311\u0313"+
		"\7`\2\2\u0312\u0314\7]\2\2\u0313\u0312\3\2\2\2\u0313\u0314\3\2\2\2\u0314"+
		"\u0315\3\2\2\2\u0315\u0317\7\16\2\2\u0316\u0318\7]\2\2\u0317\u0316\3\2"+
		"\2\2\u0317\u0318\3\2\2\2\u0318\u0319\3\2\2\2\u0319\u031b\5&\24\2\u031a"+
		"\u031c\7]\2\2\u031b\u031a\3\2\2\2\u031b\u031c\3\2\2\2\u031c\u031d\3\2"+
		"\2\2\u031d\u031f\7\17\2\2\u031e\u0320\7]\2\2\u031f\u031e\3\2\2\2\u031f"+
		"\u0320\3\2\2\2\u0320\u0321\3\2\2\2\u0321\u0323\5\20\t\2\u0322\u0324\7"+
		"]\2\2\u0323\u0322\3\2\2\2\u0323\u0324\3\2\2\2\u0324\u0325\3\2\2\2\u0325"+
		"\u0327\7\6\2\2\u0326\u0328\7]\2\2\u0327\u0326\3\2\2\2\u0327\u0328\3\2"+
		"\2\2\u0328\u0329\3\2\2\2\u0329\u032b\5\u00d6l\2\u032a\u032c\7]\2\2\u032b"+
		"\u032a\3\2\2\2\u032b\u032c\3\2\2\2\u032c\u032d\3\2\2\2\u032d\u032f\7\31"+
		"\2\2\u032e\u0330\7]\2\2\u032f\u032e\3\2\2\2\u032f\u0330\3\2\2\2\u0330"+
		"\u0331\3\2\2\2\u0331\u0332\5`\61\2\u0332\u0333\5\4\3\2\u0333S\3\2\2\2"+
		"\u0334\u0335\7\33\2\2\u0335\u0336\7]\2\2\u0336\u0337\7`\2\2\u0337\u0339"+
		"\7\16\2\2\u0338\u033a\7]\2\2\u0339\u0338\3\2\2\2\u0339\u033a\3\2\2\2\u033a"+
		"\u033b\3\2\2\2\u033b\u033d\5(\25\2\u033c\u033e\7]\2\2\u033d\u033c\3\2"+
		"\2\2\u033d\u033e\3\2\2\2\u033e\u033f\3\2\2\2\u033f\u0341\7\17\2\2\u0340"+
		"\u0342\7]\2\2\u0341\u0340\3\2\2\2\u0341\u0342\3\2\2\2\u0342\u0343\3\2"+
		"\2\2\u0343\u0345\5.\30\2\u0344\u0346\7]\2\2\u0345\u0344\3\2\2\2\u0345"+
		"\u0346\3\2\2\2\u0346\u0347\3\2\2\2\u0347\u0348\5V,\2\u0348U\3\2\2\2\u0349"+
		"\u034b\7\n\2\2\u034a\u034c\7]\2\2\u034b\u034a\3\2\2\2\u034b\u034c\3\2"+
		"\2\2\u034c\u034e\3\2\2\2\u034d\u034f\5\60\31\2\u034e\u034d\3\2\2\2\u034e"+
		"\u034f\3\2\2\2\u034f\u0351\3\2\2\2\u0350\u0352\7]\2\2\u0351\u0350\3\2"+
		"\2\2\u0351\u0352\3\2\2\2\u0352\u0353\3\2\2\2\u0353\u0354\7\13\2\2\u0354"+
		"W\3\2\2\2\u0355\u0357\5\u00f0y\2\u0356\u0358\7]\2\2\u0357\u0356\3\2\2"+
		"\2\u0357\u0358\3\2\2\2\u0358\u0359\3\2\2\2\u0359\u035a\7\3\2\2\u035aY"+
		"\3\2\2\2\u035b\u035d\5\u00f0y\2\u035c\u035e\7]\2\2\u035d\u035c\3\2\2\2"+
		"\u035d\u035e\3\2\2\2\u035e\u035f\3\2\2\2\u035f\u0361\7\34\2\2\u0360\u0362"+
		"\7]\2\2\u0361\u0360\3\2\2\2\u0361\u0362\3\2\2\2\u0362\u0363\3\2\2\2\u0363"+
		"\u0365\5\u00f2z\2\u0364\u0366\7]\2\2\u0365\u0364\3\2\2\2\u0365\u0366\3"+
		"\2\2\2\u0366\u0367\3\2\2\2\u0367\u0368\7\3\2\2\u0368[\3\2\2\2\u0369\u036a"+
		"\5b\62\2\u036a]\3\2\2\2\u036b\u036d\7\35\2\2\u036c\u036e\7]\2\2\u036d"+
		"\u036c\3\2\2\2\u036d\u036e\3\2\2\2\u036e\u036f\3\2\2\2\u036f\u0370\5\u00d6"+
		"l\2\u0370\u0372\7\36\2\2\u0371\u0373\7]\2\2\u0372\u0371\3\2\2\2\u0372"+
		"\u0373\3\2\2\2\u0373\u0374\3\2\2\2\u0374\u0376\7\31\2\2\u0375\u0377\7"+
		"]\2\2\u0376\u0375\3\2\2\2\u0376\u0377\3\2\2\2\u0377\u0378\3\2\2\2\u0378"+
		"\u037a\7\b\2\2\u0379\u037b\7]\2\2\u037a\u0379\3\2\2\2\u037a\u037b\3\2"+
		"\2\2\u037b\u037c\3\2\2\2\u037c\u037e\5\\/\2\u037d\u037f\7]\2\2\u037e\u037d"+
		"\3\2\2\2\u037e\u037f\3\2\2\2\u037f\u0380\3\2\2\2\u0380\u0381\7\t\2\2\u0381"+
		"\u0382\5\4\3\2\u0382_\3\2\2\2\u0383\u038d\5d\63\2\u0384\u0386\7]\2\2\u0385"+
		"\u0384\3\2\2\2\u0385\u0386\3\2\2\2\u0386\u0387\3\2\2\2\u0387\u0389\5\u00e6"+
		"t\2\u0388\u038a\7]\2\2\u0389\u0388\3\2\2\2\u0389\u038a\3\2\2\2\u038a\u038b"+
		"\3\2\2\2\u038b\u038c\5d\63\2\u038c\u038e\3\2\2\2\u038d\u0385\3\2\2\2\u038d"+
		"\u038e\3\2\2\2\u038ea\3\2\2\2\u038f\u039a\5`\61\2\u0390\u0392\7]\2\2\u0391"+
		"\u0390\3\2\2\2\u0391\u0392\3\2\2\2\u0392\u0393\3\2\2\2\u0393\u0395\7\7"+
		"\2\2\u0394\u0396\7]\2\2\u0395\u0394\3\2\2\2\u0395\u0396\3\2\2\2\u0396"+
		"\u0397\3\2\2\2\u0397\u0399\5`\61\2\u0398\u0391\3\2\2\2\u0399\u039c\3\2"+
		"\2\2\u039a\u0398\3\2\2\2\u039a\u039b\3\2\2\2\u039bc\3\2\2\2\u039c\u039a"+
		"\3\2\2\2\u039d\u03a7\5f\64\2\u039e\u03a0\7]\2\2\u039f\u039e\3\2\2\2\u039f"+
		"\u03a0\3\2\2\2\u03a0\u03a1\3\2\2\2\u03a1\u03a3\5\u00e8u\2\u03a2\u03a4"+
		"\7]\2\2\u03a3\u03a2\3\2\2\2\u03a3\u03a4\3\2\2\2\u03a4\u03a5\3\2\2\2\u03a5"+
		"\u03a6\5f\64\2\u03a6\u03a8\3\2\2\2\u03a7\u039f\3\2\2\2\u03a7\u03a8\3\2"+
		"\2\2\u03a8e\3\2\2\2\u03a9\u03b5\5h\65\2\u03aa\u03ac\7]\2\2\u03ab\u03aa"+
		"\3\2\2\2\u03ab\u03ac\3\2\2\2\u03ac\u03ad\3\2\2\2\u03ad\u03af\5\u00ecw"+
		"\2\u03ae\u03b0\7]\2\2\u03af\u03ae\3\2\2\2\u03af\u03b0\3\2\2\2\u03b0\u03b1"+
		"\3\2\2\2\u03b1\u03b2\5h\65\2\u03b2\u03b4\3\2\2\2\u03b3\u03ab\3\2\2\2\u03b4"+
		"\u03b7\3\2\2\2\u03b5\u03b3\3\2\2\2\u03b5\u03b6\3\2\2\2\u03b6g\3\2\2\2"+
		"\u03b7\u03b5\3\2\2\2\u03b8\u03c4\5j\66\2\u03b9\u03bb\7]\2\2\u03ba\u03b9"+
		"\3\2\2\2\u03ba\u03bb\3\2\2\2\u03bb\u03bc\3\2\2\2\u03bc\u03be\5\u00eav"+
		"\2\u03bd\u03bf\7]\2\2\u03be\u03bd\3\2\2\2\u03be\u03bf\3\2\2\2\u03bf\u03c0"+
		"\3\2\2\2\u03c0\u03c1\5j\66\2\u03c1\u03c3\3\2\2\2\u03c2\u03ba\3\2\2\2\u03c3"+
		"\u03c6\3\2\2\2\u03c4\u03c2\3\2\2\2\u03c4\u03c5\3\2\2\2\u03c5i\3\2\2\2"+
		"\u03c6\u03c4\3\2\2\2\u03c7\u03d3\5l\67\2\u03c8\u03ca\7]\2\2\u03c9\u03c8"+
		"\3\2\2\2\u03c9\u03ca\3\2\2\2\u03ca\u03cb\3\2\2\2\u03cb\u03cc\7\37\2\2"+
		"\u03cc\u03cd\5\b\5\2\u03cd\u03cf\7\37\2\2\u03ce\u03d0\7]\2\2\u03cf\u03ce"+
		"\3\2\2\2\u03cf\u03d0\3\2\2\2\u03d0\u03d1\3\2\2\2\u03d1\u03d2\5l\67\2\u03d2"+
		"\u03d4\3\2\2\2\u03d3\u03c9\3\2\2\2\u03d3\u03d4\3\2\2\2\u03d4k\3\2\2\2"+
		"\u03d5\u03df\5n8\2\u03d6\u03d8\7]\2\2\u03d7\u03d6\3\2\2\2\u03d7\u03d8"+
		"\3\2\2\2\u03d8\u03d9\3\2\2\2\u03d9\u03db\5\u00eex\2\u03da\u03dc\7]\2\2"+
		"\u03db\u03da\3\2\2\2\u03db\u03dc\3\2\2\2\u03dc\u03dd\3\2\2\2\u03dd\u03de"+
		"\5n8\2\u03de\u03e0\3\2\2\2\u03df\u03d7\3\2\2\2\u03df\u03e0\3\2\2\2\u03e0"+
		"m\3\2\2\2\u03e1\u03e3\5\u00e4s\2\u03e2\u03e4\7]\2\2\u03e3\u03e2\3\2\2"+
		"\2\u03e3\u03e4\3\2\2\2\u03e4\u03e5\3\2\2\2\u03e5\u03e6\5n8\2\u03e6\u03e9"+
		"\3\2\2\2\u03e7\u03e9\5p9\2\u03e8\u03e1\3\2\2\2\u03e8\u03e7\3\2\2\2\u03e9"+
		"o\3\2\2\2\u03ea\u03f3\5\u0090I\2\u03eb\u03ed\7]\2\2\u03ec\u03eb\3\2\2"+
		"\2\u03ec\u03ed\3\2\2\2\u03ed\u03ee\3\2\2\2\u03ee\u03f0\7\6\2\2\u03ef\u03f1"+
		"\7]\2\2\u03f0\u03ef\3\2\2\2\u03f0\u03f1\3\2\2\2\u03f1\u03f2\3\2\2\2\u03f2"+
		"\u03f4\5\u00d6l\2\u03f3\u03ec\3\2\2\2\u03f3\u03f4\3\2\2\2\u03f4q\3\2\2"+
		"\2\u03f5\u040c\5t;\2\u03f6\u040c\5v<\2\u03f7\u040c\5x=\2\u03f8\u040c\5"+
		"z>\2\u03f9\u040c\5\u0080A\2\u03fa\u040c\5\u009aN\2\u03fb\u040c\5\u0082"+
		"B\2\u03fc\u040c\5\u008aF\2\u03fd\u040c\5\u008cG\2\u03fe\u040c\5\u008e"+
		"H\2\u03ff\u040c\5\u0092J\2\u0400\u040c\5\u0094K\2\u0401\u040c\5\u0096"+
		"L\2\u0402\u040c\5\u00d2j\2\u0403\u040c\5\u009cO\2\u0404\u040c\5\u009e"+
		"P\2\u0405\u040c\7c\2\2\u0406\u040c\7d\2\2\u0407\u040c\5\u0098M\2\u0408"+
		"\u040c\5\u0088E\2\u0409\u040c\5~@\2\u040a\u040c\7f\2\2\u040b\u03f5\3\2"+
		"\2\2\u040b\u03f6\3\2\2\2\u040b\u03f7\3\2\2\2\u040b\u03f8\3\2\2\2\u040b"+
		"\u03f9\3\2\2\2\u040b\u03fa\3\2\2\2\u040b\u03fb\3\2\2\2\u040b\u03fc\3\2"+
		"\2\2\u040b\u03fd\3\2\2\2\u040b\u03fe\3\2\2\2\u040b\u03ff\3\2\2\2\u040b"+
		"\u0400\3\2\2\2\u040b\u0401\3\2\2\2\u040b\u0402\3\2\2\2\u040b\u0403\3\2"+
		"\2\2\u040b\u0404\3\2\2\2\u040b\u0405\3\2\2\2\u040b\u0406\3\2\2\2\u040b"+
		"\u0407\3\2\2\2\u040b\u0408\3\2\2\2\u040b\u0409\3\2\2\2\u040b\u040a\3\2"+
		"\2\2\u040cs\3\2\2\2\u040d\u040e\7\35\2\2\u040e\u040f\7]\2\2\u040f\u0411"+
		"\5\u00a0Q\2\u0410\u0412\7]\2\2\u0411\u0410\3\2\2\2\u0411\u0412\3\2\2\2"+
		"\u0412\u0413\3\2\2\2\u0413\u0415\7\31\2\2\u0414\u0416\7]\2\2\u0415\u0414"+
		"\3\2\2\2\u0415\u0416\3\2\2\2\u0416\u0417\3\2\2\2\u0417\u0418\5`\61\2\u0418"+
		"\u0419\7]\2\2\u0419\u041a\7 \2\2\u041a\u041b\7]\2\2\u041b\u041c\5`\61"+
		"\2\u041cu\3\2\2\2\u041d\u041f\7!\2\2\u041e\u0420\7]\2\2\u041f\u041e\3"+
		"\2\2\2\u041f\u0420\3\2\2\2\u0420\u0421\3\2\2\2\u0421\u0423\7\b\2\2\u0422"+
		"\u0424\7]\2\2\u0423\u0422\3\2\2\2\u0423\u0424\3\2\2\2\u0424\u0425\3\2"+
		"\2\2\u0425\u0427\5`\61\2\u0426\u0428\7]\2\2\u0427\u0426\3\2\2\2\u0427"+
		"\u0428\3\2\2\2\u0428\u0429\3\2\2\2\u0429\u042b\7\t\2\2\u042a\u042c\7]"+
		"\2\2\u042b\u042a\3\2\2\2\u042b\u042c\3\2\2\2\u042c\u042d\3\2\2\2\u042d"+
		"\u042e\5`\61\2\u042e\u042f\7]\2\2\u042f\u0430\7\"\2\2\u0430\u0431\7]\2"+
		"\2\u0431\u0432\5`\61\2\u0432w\3\2\2\2\u0433\u0434\7#\2\2\u0434\u0435\7"+
		"]\2\2\u0435\u0436\5`\61\2\u0436\u0437\7]\2\2\u0437\u0438\7$\2\2\u0438"+
		"\u0439\7]\2\2\u0439\u043b\7\n\2\2\u043a\u043c\7]\2\2\u043b\u043a\3\2\2"+
		"\2\u043b\u043c\3\2\2\2\u043c\u043d\3\2\2\2\u043d\u043f\5\36\20\2\u043e"+
		"\u0440\7]\2\2\u043f\u043e\3\2\2\2\u043f\u0440\3\2\2\2\u0440\u0441\3\2"+
		"\2\2\u0441\u0442\7\13\2\2\u0442y\3\2\2\2\u0443\u0444\7%\2\2\u0444\u0445"+
		"\7]\2\2\u0445\u0446\5`\61\2\u0446\u0447\7]\2\2\u0447\u0448\7$\2\2\u0448"+
		"\u0449\7]\2\2\u0449\u044b\7\n\2\2\u044a\u044c\7]\2\2\u044b\u044a\3\2\2"+
		"\2\u044b\u044c\3\2\2\2\u044c\u044d\3\2\2\2\u044d\u044f\5\"\22\2\u044e"+
		"\u0450\7]\2\2\u044f\u044e\3\2\2\2\u044f\u0450\3\2\2\2\u0450\u0451\3\2"+
		"\2\2\u0451\u0452\7\13\2\2\u0452{\3\2\2\2\u0453\u0462\5r:\2\u0454\u0456"+
		"\7]\2\2\u0455\u0454\3\2\2\2\u0455\u0456\3\2\2\2\u0456\u0457\3\2\2\2\u0457"+
		"\u0459\7\b\2\2\u0458\u045a\7]\2\2\u0459\u0458\3\2\2\2\u0459\u045a\3\2"+
		"\2\2\u045a\u045c\3\2\2\2\u045b\u045d\5b\62\2\u045c\u045b\3\2\2\2\u045c"+
		"\u045d\3\2\2\2\u045d\u045f\3\2\2\2\u045e\u0460\7]\2\2\u045f\u045e\3\2"+
		"\2\2\u045f\u0460\3\2\2\2\u0460\u0461\3\2\2\2\u0461\u0463\7\t\2\2\u0462"+
		"\u0455\3\2\2\2\u0462\u0463\3\2\2\2\u0463}\3\2\2\2\u0464\u0465\5\b\5\2"+
		"\u0465\177\3\2\2\2\u0466\u0467\5\b\5\2\u0467\u0468\7\3\2\2\u0468\u046d"+
		"\7`\2\2\u0469\u046b\7]\2\2\u046a\u0469\3\2\2\2\u046a\u046b\3\2\2\2\u046b"+
		"\u046c\3\2\2\2\u046c\u046e\5\u0082B\2\u046d\u046a\3\2\2\2\u046d\u046e"+
		"\3\2\2\2\u046e\u0081\3\2\2\2\u046f\u0471\7\b\2\2\u0470\u0472\7]\2\2\u0471"+
		"\u0470\3\2\2\2\u0471\u0472\3\2\2\2\u0472\u0474\3\2\2\2\u0473\u0475\5b"+
		"\62\2\u0474\u0473\3\2\2\2\u0474\u0475\3\2\2\2\u0475\u0477\3\2\2\2\u0476"+
		"\u0478\7]\2\2\u0477\u0476\3\2\2\2\u0477\u0478\3\2\2\2\u0478\u0479\3\2"+
		"\2\2\u0479\u047a\7\t\2\2\u047a\u0083\3\2\2\2\u047b\u047d\5`\61\2\u047c"+
		"\u047e\7]\2\2\u047d\u047c\3\2\2\2\u047d\u047e\3\2\2\2\u047e\u047f\3\2"+
		"\2\2\u047f\u0481\7&\2\2\u0480\u0482\7]\2\2\u0481\u0480\3\2\2\2\u0481\u0482"+
		"\3\2\2\2\u0482\u0483\3\2\2\2\u0483\u0484\5`\61\2\u0484\u0085\3\2\2\2\u0485"+
		"\u0490\5\u0084C\2\u0486\u0488\7]\2\2\u0487\u0486\3\2\2\2\u0487\u0488\3"+
		"\2\2\2\u0488\u0489\3\2\2\2\u0489\u048b\7\7\2\2\u048a\u048c\7]\2\2\u048b"+
		"\u048a\3\2\2\2\u048b\u048c\3\2\2\2\u048c\u048d\3\2\2\2\u048d\u048f\5\u0084"+
		"C\2\u048e\u0487\3\2\2\2\u048f\u0492\3\2\2\2\u0490\u048e\3\2\2\2\u0490"+
		"\u0491\3\2\2\2\u0491\u0087\3\2\2\2\u0492\u0490\3\2\2\2\u0493\u0494\7e"+
		"\2\2\u0494\u0089\3\2\2\2\u0495\u0496\7b\2\2\u0496\u008b\3\2\2\2\u0497"+
		"\u0498\7a\2\2\u0498\u008d\3\2\2\2\u0499\u049b\7\'\2\2\u049a\u049c\7]\2"+
		"\2\u049b\u049a\3\2\2\2\u049b\u049c\3\2\2\2\u049c\u049d\3\2\2\2\u049d\u049f"+
		"\7\b\2\2\u049e\u04a0\7]\2\2\u049f\u049e\3\2\2\2\u049f\u04a0\3\2\2\2\u04a0"+
		"\u04a1\3\2\2\2\u04a1\u04a3\5`\61\2\u04a2\u04a4\7]\2\2\u04a3\u04a2\3\2"+
		"\2\2\u04a3\u04a4\3\2\2\2\u04a4\u04a5\3\2\2\2\u04a5\u04a6\7\t\2\2\u04a6"+
		"\u008f\3\2\2\2\u04a7\u04b0\5|?\2\u04a8\u04aa\7]\2\2\u04a9\u04a8\3\2\2"+
		"\2\u04a9\u04aa\3\2\2\2\u04aa\u04ab\3\2\2\2\u04ab\u04ad\7(\2\2\u04ac\u04ae"+
		"\7]\2\2\u04ad\u04ac\3\2\2\2\u04ad\u04ae\3\2\2\2\u04ae\u04af\3\2\2\2\u04af"+
		"\u04b1\5`\61\2\u04b0\u04a9\3\2\2\2\u04b0\u04b1\3\2\2\2\u04b1\u0091\3\2"+
		"\2\2\u04b2\u04b4\7)\2\2\u04b3\u04b5\7]\2\2\u04b4\u04b3\3\2\2\2\u04b4\u04b5"+
		"\3\2\2\2\u04b5\u04b7\3\2\2\2\u04b6\u04b8\5b\62\2\u04b7\u04b6\3\2\2\2\u04b7"+
		"\u04b8\3\2\2\2\u04b8\u04ba\3\2\2\2\u04b9\u04bb\7]\2\2\u04ba\u04b9\3\2"+
		"\2\2\u04ba\u04bb\3\2\2\2\u04bb\u04bc\3\2\2\2\u04bc\u04bd\7\17\2\2\u04bd"+
		"\u0093\3\2\2\2\u04be\u04c0\7*\2\2\u04bf\u04c1\7]\2\2\u04c0\u04bf\3\2\2"+
		"\2\u04c0\u04c1\3\2\2\2\u04c1\u04c3\3\2\2\2\u04c2\u04c4\5b\62\2\u04c3\u04c2"+
		"\3\2\2\2\u04c3\u04c4\3\2\2\2\u04c4\u04c6\3\2\2\2\u04c5\u04c7\7]\2\2\u04c6"+
		"\u04c5\3\2\2\2\u04c6\u04c7\3\2\2\2\u04c7\u04c8\3\2\2\2\u04c8\u04c9\7\13"+
		"\2\2\u04c9\u0095\3\2\2\2\u04ca\u04cc\7+\2\2\u04cb\u04cd\7]\2\2\u04cc\u04cb"+
		"\3\2\2\2\u04cc\u04cd\3\2\2\2\u04cd\u04cf\3\2\2\2\u04ce\u04d0\5\u0086D"+
		"\2\u04cf\u04ce\3\2\2\2\u04cf\u04d0\3\2\2\2\u04d0\u04d2\3\2\2\2\u04d1\u04d3"+
		"\7]\2\2\u04d2\u04d1\3\2\2\2\u04d2\u04d3\3\2\2\2\u04d3\u04d4\3\2\2\2\u04d4"+
		"\u04d5\7\13\2\2\u04d5\u0097\3\2\2\2\u04d6\u04d8\7`\2\2\u04d7\u04d9\7]"+
		"\2\2\u04d8\u04d7\3\2\2\2\u04d8\u04d9\3\2\2\2\u04d9\u04da\3\2\2\2\u04da"+
		"\u04dc\7&\2\2\u04db\u04dd\7]\2\2\u04dc\u04db\3\2\2\2\u04dc\u04dd\3\2\2"+
		"\2\u04dd\u04de\3\2\2\2\u04de\u04df\5`\61\2\u04df\u0099\3\2\2\2\u04e0\u04e2"+
		"\7\b\2\2\u04e1\u04e3\7]\2\2\u04e2\u04e1\3\2\2\2\u04e2\u04e3\3\2\2\2\u04e3"+
		"\u04e4\3\2\2\2\u04e4\u04e6\5\32\16\2\u04e5\u04e7\7]\2\2\u04e6\u04e5\3"+
		"\2\2\2\u04e6\u04e7\3\2\2\2\u04e7\u04e8\3\2\2\2\u04e8\u04ea\7\t\2\2\u04e9"+
		"\u04eb\7]\2\2\u04ea\u04e9\3\2\2\2\u04ea\u04eb\3\2\2\2\u04eb\u04ec\3\2"+
		"\2\2\u04ec\u04ee\7&\2\2\u04ed\u04ef\7]\2\2\u04ee\u04ed\3\2\2\2\u04ee\u04ef"+
		"\3\2\2\2\u04ef\u04f0\3\2\2\2\u04f0\u04f1\5`\61\2\u04f1\u009b\3\2\2\2\u04f2"+
		"\u04f4\t\2\2\2\u04f3\u04f5\7]\2\2\u04f4\u04f3\3\2\2\2\u04f4\u04f5\3\2"+
		"\2\2\u04f5\u04f6\3\2\2\2\u04f6\u04f8\5\20\t\2\u04f7\u04f9\7]\2\2\u04f8"+
		"\u04f7\3\2\2\2\u04f8\u04f9\3\2\2\2\u04f9\u04fa\3\2\2\2\u04fa\u04fc\7\3"+
		"\2\2\u04fb\u04fd\7]\2\2\u04fc\u04fb\3\2\2\2\u04fc\u04fd\3\2\2\2\u04fd"+
		"\u04fe\3\2\2\2\u04fe\u04ff\5`\61\2\u04ff\u009d\3\2\2\2\u0500\u0502\t\3"+
		"\2\2\u0501\u0503\7]\2\2\u0502\u0501\3\2\2\2\u0502\u0503\3\2\2\2\u0503"+
		"\u0504\3\2\2\2\u0504\u0506\5\20\t\2\u0505\u0507\7]\2\2\u0506\u0505\3\2"+
		"\2\2\u0506\u0507\3\2\2\2\u0507\u0508\3\2\2\2\u0508\u050a\7\3\2\2\u0509"+
		"\u050b\7]\2\2\u050a\u0509\3\2\2\2\u050a\u050b\3\2\2\2\u050b\u050c\3\2"+
		"\2\2\u050c\u050d\5`\61\2\u050d\u009f\3\2\2\2\u050e\u0517\5\u00a4S\2\u050f"+
		"\u0511\7]\2\2\u0510\u050f\3\2\2\2\u0510\u0511\3\2\2\2\u0511\u0512\3\2"+
		"\2\2\u0512\u0514\7(\2\2\u0513\u0515\7]\2\2\u0514\u0513\3\2\2\2\u0514\u0515"+
		"\3\2\2\2\u0515\u0516\3\2\2\2\u0516\u0518\5\u00a0Q\2\u0517\u0510\3\2\2"+
		"\2\u0517\u0518\3\2\2\2\u0518\u00a1\3\2\2\2\u0519\u0524\5\u00a0Q\2\u051a"+
		"\u051c\7]\2\2\u051b\u051a\3\2\2\2\u051b\u051c\3\2\2\2\u051c\u051d\3\2"+
		"\2\2\u051d\u051f\7\7\2\2\u051e\u0520\7]\2\2\u051f\u051e\3\2\2\2\u051f"+
		"\u0520\3\2\2\2\u0520\u0521\3\2\2\2\u0521\u0523\5\u00a0Q\2\u0522\u051b"+
		"\3\2\2\2\u0523\u0526\3\2\2\2\u0524\u0522\3\2\2\2\u0524\u0525\3\2\2\2\u0525"+
		"\u00a3\3\2\2\2\u0526\u0524\3\2\2\2\u0527\u0532\5\u00aeX\2\u0528\u0532"+
		"\5\u00b0Y\2\u0529\u0532\5\u00d2j\2\u052a\u0532\7`\2\2\u052b\u0532\7e\2"+
		"\2\u052c\u0532\5\u00aaV\2\u052d\u0532\5\u00acW\2\u052e\u0532\5\u00b2Z"+
		"\2\u052f\u0532\5\u00b4[\2\u0530\u0532\5\u00b6\\\2\u0531\u0527\3\2\2\2"+
		"\u0531\u0528\3\2\2\2\u0531\u0529\3\2\2\2\u0531\u052a\3\2\2\2\u0531\u052b"+
		"\3\2\2\2\u0531\u052c\3\2\2\2\u0531\u052d\3\2\2\2\u0531\u052e\3\2\2\2\u0531"+
		"\u052f\3\2\2\2\u0531\u0530\3\2\2\2\u0532\u00a5\3\2\2\2\u0533\u0535\5\u00a0"+
		"Q\2\u0534\u0536\7]\2\2\u0535\u0534\3\2\2\2\u0535\u0536\3\2\2\2\u0536\u0537"+
		"\3\2\2\2\u0537\u0539\7&\2\2\u0538\u053a\7]\2\2\u0539\u0538\3\2\2\2\u0539"+
		"\u053a\3\2\2\2\u053a\u053b\3\2\2\2\u053b\u053c\5\u00a0Q\2\u053c\u00a7"+
		"\3\2\2\2\u053d\u0548\5\u00a6T\2\u053e\u0540\7]\2\2\u053f\u053e\3\2\2\2"+
		"\u053f\u0540\3\2\2\2\u0540\u0541\3\2\2\2\u0541\u0543\7\7\2\2\u0542\u0544"+
		"\7]\2\2\u0543\u0542\3\2\2\2\u0543\u0544\3\2\2\2\u0544\u0545\3\2\2\2\u0545"+
		"\u0547\5\u00a6T\2\u0546\u053f\3\2\2\2\u0547\u054a\3\2\2\2\u0548\u0546"+
		"\3\2\2\2\u0548\u0549\3\2\2\2\u0549\u00a9\3\2\2\2\u054a\u0548\3\2\2\2\u054b"+
		"\u054c\5\b\5\2\u054c\u054d\7\3\2\2\u054d\u0552\7`\2\2\u054e\u0550\7]\2"+
		"\2\u054f\u054e\3\2\2\2\u054f\u0550\3\2\2\2\u0550\u0551\3\2\2\2\u0551\u0553"+
		"\5\u00a0Q\2\u0552\u054f\3\2\2\2\u0552\u0553\3\2\2\2\u0553\u00ab\3\2\2"+
		"\2\u0554\u0556\7\b\2\2\u0555\u0557\7]\2\2\u0556\u0555\3\2\2\2\u0556\u0557"+
		"\3\2\2\2\u0557\u0559\3\2\2\2\u0558\u055a\5\u00a2R\2\u0559\u0558\3\2\2"+
		"\2\u0559\u055a\3\2\2\2\u055a\u055c\3\2\2\2\u055b\u055d\7]\2\2\u055c\u055b"+
		"\3\2\2\2\u055c\u055d\3\2\2\2\u055d\u055e\3\2\2\2\u055e\u055f\7\t\2\2\u055f"+
		"\u00ad\3\2\2\2\u0560\u0561\7b\2\2\u0561\u00af\3\2\2\2\u0562\u0563\7a\2"+
		"\2\u0563\u00b1\3\2\2\2\u0564\u0566\7)\2\2\u0565\u0567\7]\2\2\u0566\u0565"+
		"\3\2\2\2\u0566\u0567\3\2\2\2\u0567\u0569\3\2\2\2\u0568\u056a\5\u00a2R"+
		"\2\u0569\u0568\3\2\2\2\u0569\u056a\3\2\2\2\u056a\u0573\3\2\2\2\u056b\u056d"+
		"\7]\2\2\u056c\u056b\3\2\2\2\u056c\u056d\3\2\2\2\u056d\u056e\3\2\2\2\u056e"+
		"\u0570\7\7\2\2\u056f\u0571\7]\2\2\u0570\u056f\3\2\2\2\u0570\u0571\3\2"+
		"\2\2\u0571\u0572\3\2\2\2\u0572\u0574\7\60\2\2\u0573\u056c\3\2\2\2\u0573"+
		"\u0574\3\2\2\2\u0574\u0576\3\2\2\2\u0575\u0577\7]\2\2\u0576\u0575\3\2"+
		"\2\2\u0576\u0577\3\2\2\2\u0577\u0578\3\2\2\2\u0578\u0579\7\17\2\2\u0579"+
		"\u00b3\3\2\2\2\u057a\u057c\7*\2\2\u057b\u057d\7]\2\2\u057c\u057b\3\2\2"+
		"\2\u057c\u057d\3\2\2\2\u057d\u057f\3\2\2\2\u057e\u0580\5\u00a2R\2\u057f"+
		"\u057e\3\2\2\2\u057f\u0580\3\2\2\2\u0580\u0589\3\2\2\2\u0581\u0583\7]"+
		"\2\2\u0582\u0581\3\2\2\2\u0582\u0583\3\2\2\2\u0583\u0584\3\2\2\2\u0584"+
		"\u0586\7\7\2\2\u0585\u0587\7]\2\2\u0586\u0585\3\2\2\2\u0586\u0587\3\2"+
		"\2\2\u0587\u0588\3\2\2\2\u0588\u058a\7\60\2\2\u0589\u0582\3\2\2\2\u0589"+
		"\u058a\3\2\2\2\u058a\u058c\3\2\2\2\u058b\u058d\7]\2\2\u058c\u058b\3\2"+
		"\2\2\u058c\u058d\3\2\2\2\u058d\u058e\3\2\2\2\u058e\u058f\7\13\2\2\u058f"+
		"\u00b5\3\2\2\2\u0590\u0592\7+\2\2\u0591\u0593\7]\2\2\u0592\u0591\3\2\2"+
		"\2\u0592\u0593\3\2\2\2\u0593\u0595\3\2\2\2\u0594\u0596\5\u00a8U\2\u0595"+
		"\u0594\3\2\2\2\u0595\u0596\3\2\2\2\u0596\u059f\3\2\2\2\u0597\u0599\7]"+
		"\2\2\u0598\u0597\3\2\2\2\u0598\u0599\3\2\2\2\u0599\u059a\3\2\2\2\u059a"+
		"\u059c\7\7\2\2\u059b\u059d\7]\2\2\u059c\u059b\3\2\2\2\u059c\u059d\3\2"+
		"\2\2\u059d\u059e\3\2\2\2\u059e\u05a0\7\60\2\2\u059f\u0598\3\2\2\2\u059f"+
		"\u05a0\3\2\2\2\u05a0\u05a2\3\2\2\2\u05a1\u05a3\7]\2\2\u05a2\u05a1\3\2"+
		"\2\2\u05a2\u05a3\3\2\2\2\u05a3\u05a4\3\2\2\2\u05a4\u05a5\7\13\2\2\u05a5"+
		"\u00b7\3\2\2\2\u05a6\u05a7\t\4\2\2\u05a7\u00b9\3\2\2\2\u05a8\u05a9\7\63"+
		"\2\2\u05a9\u00bb\3\2\2\2\u05aa\u05ac\5\u00ba^\2\u05ab\u05aa\3\2\2\2\u05ab"+
		"\u05ac\3\2\2\2\u05ac\u05ad\3\2\2\2\u05ad\u05ae\7i\2\2\u05ae\u05af\7\3"+
		"\2\2\u05af\u05b0\7i\2\2\u05b0\u05b1\7\64\2\2\u05b1\u00bd\3\2\2\2\u05b2"+
		"\u05b4\5\u00ba^\2\u05b3\u05b2\3\2\2\2\u05b3\u05b4\3\2\2\2\u05b4\u05b5"+
		"\3\2\2\2\u05b5\u05b6\7i\2\2\u05b6\u05b7\7\3\2\2\u05b7\u05b8\7i\2\2\u05b8"+
		"\u05b9\7\65\2\2\u05b9\u00bf\3\2\2\2\u05ba\u05bc\5\u00ba^\2\u05bb\u05ba"+
		"\3\2\2\2\u05bb\u05bc\3\2\2\2\u05bc\u05bd\3\2\2\2\u05bd\u05be\7i\2\2\u05be"+
		"\u05bf\7\3\2\2\u05bf\u05c0\7i\2\2\u05c0\u00c1\3\2\2\2\u05c1\u05c5\5\u00bc"+
		"_\2\u05c2\u05c5\5\u00be`\2\u05c3\u05c5\5\u00c0a\2\u05c4\u05c1\3\2\2\2"+
		"\u05c4\u05c2\3\2\2\2\u05c4\u05c3\3\2\2\2\u05c5\u00c3\3\2\2\2\u05c6\u05c8"+
		"\5\u00ba^\2\u05c7\u05c6\3\2\2\2\u05c7\u05c8\3\2\2\2\u05c8\u05c9\3\2\2"+
		"\2\u05c9\u05ca\7i\2\2\u05ca\u05cb\7\66\2\2\u05cb\u00c5\3\2\2\2\u05cc\u05ce"+
		"\5\u00ba^\2\u05cd\u05cc\3\2\2\2\u05cd\u05ce\3\2\2\2\u05ce\u05cf\3\2\2"+
		"\2\u05cf\u05d0\7i\2\2\u05d0\u05d1\7\67\2\2\u05d1\u00c7\3\2\2\2\u05d2\u05d4"+
		"\5\u00ba^\2\u05d3\u05d2\3\2\2\2\u05d3\u05d4\3\2\2\2\u05d4\u05d5\3\2\2"+
		"\2\u05d5\u05d6\7i\2\2\u05d6\u05d7\78\2\2\u05d7\u00c9\3\2\2\2\u05d8\u05da"+
		"\5\u00ba^\2\u05d9\u05d8\3\2\2\2\u05d9\u05da\3\2\2\2\u05da\u05db\3\2\2"+
		"\2\u05db\u05dc\7i\2\2\u05dc\u05dd\79\2\2\u05dd\u00cb\3\2\2\2\u05de\u05e0"+
		"\5\u00ba^\2\u05df\u05de\3\2\2\2\u05df\u05e0\3\2\2\2\u05e0\u05e1\3\2\2"+
		"\2\u05e1\u05e2\7i\2\2\u05e2\u05e3\7:\2\2\u05e3\u00cd\3\2\2\2\u05e4\u05e6"+
		"\5\u00ba^\2\u05e5\u05e4\3\2\2\2\u05e5\u05e6\3\2\2\2\u05e6\u05e7\3\2\2"+
		"\2\u05e7\u05e8\7i\2\2\u05e8\u00cf\3\2\2\2\u05e9\u05f0\5\u00c4c\2\u05ea"+
		"\u05f0\5\u00c6d\2\u05eb\u05f0\5\u00c8e\2\u05ec\u05f0\5\u00caf\2\u05ed"+
		"\u05f0\5\u00ccg\2\u05ee\u05f0\5\u00ceh\2\u05ef\u05e9\3\2\2\2\u05ef\u05ea"+
		"\3\2\2\2\u05ef\u05eb\3\2\2\2\u05ef\u05ec\3\2\2\2\u05ef\u05ed\3\2\2\2\u05ef"+
		"\u05ee\3\2\2\2\u05f0\u00d1\3\2\2\2\u05f1\u05f7\5\u00b8]\2\u05f2\u05f7"+
		"\7g\2\2\u05f3\u05f7\5\u00c2b\2\u05f4\u05f7\5\u00d0i\2\u05f5\u05f7\7h\2"+
		"\2\u05f6\u05f1\3\2\2\2\u05f6\u05f2\3\2\2\2\u05f6\u05f3\3\2\2\2\u05f6\u05f4"+
		"\3\2\2\2\u05f6\u05f5\3\2\2\2\u05f7\u00d3\3\2\2\2\u05f8\u05fd\5\u00d8m"+
		"\2\u05f9\u05fd\5\u00e0q\2\u05fa\u05fd\5\u00e2r\2\u05fb\u05fd\5\b\5\2\u05fc"+
		"\u05f8\3\2\2\2\u05fc\u05f9\3\2\2\2\u05fc\u05fa\3\2\2\2\u05fc\u05fb\3\2"+
		"\2\2\u05fd\u00d5\3\2\2\2\u05fe\u0607\5\u00d4k\2\u05ff\u0601\7]\2\2\u0600"+
		"\u05ff\3\2\2\2\u0600\u0601\3\2\2\2\u0601\u0602\3\2\2\2\u0602\u0604\7&"+
		"\2\2\u0603\u0605\7]\2\2\u0604\u0603\3\2\2\2\u0604\u0605\3\2\2\2\u0605"+
		"\u0606\3\2\2\2\u0606\u0608\5\u00d6l\2\u0607\u0600\3\2\2\2\u0607\u0608"+
		"\3\2\2\2\u0608\u00d7\3\2\2\2\u0609\u060b\7\b\2\2\u060a\u060c\7]\2\2\u060b"+
		"\u060a\3\2\2\2\u060b\u060c\3\2\2\2\u060c\u060d\3\2\2\2\u060d\u0618\5\u00d6"+
		"l\2\u060e\u0610\7]\2\2\u060f\u060e\3\2\2\2\u060f\u0610\3\2\2\2\u0610\u0611"+
		"\3\2\2\2\u0611\u0613\7\7\2\2\u0612\u0614\7]\2\2\u0613\u0612\3\2\2\2\u0613"+
		"\u0614\3\2\2\2\u0614\u0615\3\2\2\2\u0615\u0617\5\u00d6l\2\u0616\u060f"+
		"\3\2\2\2\u0617\u061a\3\2\2\2\u0618\u0616\3\2\2\2\u0618\u0619\3\2\2\2\u0619"+
		"\u061c\3\2\2\2\u061a\u0618\3\2\2\2\u061b\u061d\7]\2\2\u061c\u061b\3\2"+
		"\2\2\u061c\u061d\3\2\2\2\u061d\u061e\3\2\2\2\u061e\u0620\7\t\2\2\u061f"+
		"\u0621\7]\2\2\u0620\u061f\3\2\2\2\u0620\u0621\3\2\2\2\u0621\u0622\3\2"+
		"\2\2\u0622\u0624\7&\2\2\u0623\u0625\7]\2\2\u0624\u0623\3\2\2\2\u0624\u0625"+
		"\3\2\2\2\u0625\u0626\3\2\2\2\u0626\u0627\5\u00d6l\2\u0627\u00d9\3\2\2"+
		"\2\u0628\u0629\7;\2\2\u0629\u00db\3\2\2\2\u062a\u062c\7\b\2\2\u062b\u062d"+
		"\7]\2\2\u062c\u062b\3\2\2\2\u062c\u062d\3\2\2\2\u062d\u062e\3\2\2\2\u062e"+
		"\u0630\5\u00d6l\2\u062f\u0631\7]\2\2\u0630\u062f\3\2\2\2\u0630\u0631\3"+
		"\2\2\2\u0631\u0632\3\2\2\2\u0632\u0633\7\t\2\2\u0633\u00dd\3\2\2\2\u0634"+
		"\u0636\7\b\2\2\u0635\u0637\7]\2\2\u0636\u0635\3\2\2\2\u0636\u0637\3\2"+
		"\2\2\u0637\u0638\3\2\2\2\u0638\u0641\5\u00d6l\2\u0639\u063b\7]\2\2\u063a"+
		"\u0639\3\2\2\2\u063a\u063b\3\2\2\2\u063b\u063c\3\2\2\2\u063c\u063e\7\7"+
		"\2\2\u063d\u063f\7]\2\2\u063e\u063d\3\2\2\2\u063e\u063f\3\2\2\2\u063f"+
		"\u0640\3\2\2\2\u0640\u0642\5\u00d6l\2\u0641\u063a\3\2\2\2\u0642\u0643"+
		"\3\2\2\2\u0643\u0641\3\2\2\2\u0643\u0644\3\2\2\2\u0644\u0646\3\2\2\2\u0645"+
		"\u0647\7]\2\2\u0646\u0645\3\2\2\2\u0646\u0647\3\2\2\2\u0647\u0648\3\2"+
		"\2\2\u0648\u0649\7\t\2\2\u0649\u00df\3\2\2\2\u064a\u064e\5\u00dan\2\u064b"+
		"\u064e\5\u00dco\2\u064c\u064e\5\u00dep\2\u064d\u064a\3\2\2\2\u064d\u064b"+
		"\3\2\2\2\u064d\u064c\3\2\2\2\u064e\u00e1\3\2\2\2\u064f\u0651\5\b\5\2\u0650"+
		"\u0652\7]\2\2\u0651\u0650\3\2\2\2\u0651\u0652\3\2\2\2\u0652\u0653\3\2"+
		"\2\2\u0653\u0655\7\16\2\2\u0654\u0656\7]\2\2\u0655\u0654\3\2\2\2\u0655"+
		"\u0656\3\2\2\2\u0656\u0657\3\2\2\2\u0657\u0662\5\u00d6l\2\u0658\u065a"+
		"\7]\2\2\u0659\u0658\3\2\2\2\u0659\u065a\3\2\2\2\u065a\u065b\3\2\2\2\u065b"+
		"\u065d\7\7\2\2\u065c\u065e\7]\2\2\u065d\u065c\3\2\2\2\u065d\u065e\3\2"+
		"\2\2\u065e\u065f\3\2\2\2\u065f\u0661\5\u00d6l\2\u0660\u0659\3\2\2\2\u0661"+
		"\u0664\3\2\2\2\u0662\u0660\3\2\2\2\u0662\u0663\3\2\2\2\u0663\u0666\3\2"+
		"\2\2\u0664\u0662\3\2\2\2\u0665\u0667\7]\2\2\u0666\u0665\3\2\2\2\u0666"+
		"\u0667\3\2\2\2\u0667\u0668\3\2\2\2\u0668\u0669\7\17\2\2\u0669\u00e3\3"+
		"\2\2\2\u066a\u066b\t\5\2\2\u066b\u00e5\3\2\2\2\u066c\u066d\t\6\2\2\u066d"+
		"\u00e7\3\2\2\2\u066e\u066f\t\7\2\2\u066f\u00e9\3\2\2\2\u0670\u0671\t\b"+
		"\2\2\u0671\u00eb\3\2\2\2\u0672\u0673\t\t\2\2\u0673\u00ed\3\2\2\2\u0674"+
		"\u0675\t\n\2\2\u0675\u00ef\3\2\2\2\u0676\u067d\5\u00f4{\2\u0677\u067d"+
		"\5\u00f6|\2\u0678\u067d\5\u00f8}\2\u0679\u067d\5\u00fc\177\2\u067a\u067d"+
		"\5\u00fa~\2\u067b\u067d\5\u00fe\u0080\2\u067c\u0676\3\2\2\2\u067c\u0677"+
		"\3\2\2\2\u067c\u0678\3\2\2\2\u067c\u0679\3\2\2\2\u067c\u067a\3\2\2\2\u067c"+
		"\u067b\3\2\2\2\u067d\u00f1\3\2\2\2\u067e\u0689\5\u00f0y\2\u067f\u0681"+
		"\7]\2\2\u0680\u067f\3\2\2\2\u0680\u0681\3\2\2\2\u0681\u0682\3\2\2\2\u0682"+
		"\u0684\7\7\2\2\u0683\u0685\7]\2\2\u0684\u0683\3\2\2\2\u0684\u0685\3\2"+
		"\2\2\u0685\u0686\3\2\2\2\u0686\u0688\5\u00f0y\2\u0687\u0680\3\2\2\2\u0688"+
		"\u068b\3\2\2\2\u0689\u0687\3\2\2\2\u0689\u068a\3\2\2\2\u068a\u00f3\3\2"+
		"\2\2\u068b\u0689\3\2\2\2\u068c\u068d\7\61\2\2\u068d\u00f5\3\2\2\2\u068e"+
		"\u068f\7\62\2\2\u068f\u00f7\3\2\2\2\u0690\u0692\5\b\5\2\u0691\u0693\7"+
		"]\2\2\u0692\u0691\3\2\2\2\u0692\u0693\3\2\2\2\u0693\u0694\3\2\2\2\u0694"+
		"\u0695\7\b\2\2\u0695\u0696\5b\62\2\u0696\u0697\7\t\2\2\u0697\u00f9\3\2"+
		"\2\2\u0698\u069a\7`\2\2\u0699\u069b\7]\2\2\u069a\u0699\3\2\2\2\u069a\u069b"+
		"\3\2\2\2\u069b\u069c\3\2\2\2\u069c\u069e\7[\2\2\u069d\u069f\7]\2\2\u069e"+
		"\u069d\3\2\2\2\u069e\u069f\3\2\2\2\u069f\u06a0\3\2\2\2\u06a0\u06a1\7`"+
		"\2\2\u06a1\u00fb\3\2\2\2\u06a2\u06a4\7`\2\2\u06a3\u06a5\7]\2\2\u06a4\u06a3"+
		"\3\2\2\2\u06a4\u06a5\3\2\2\2\u06a5\u06a6\3\2\2\2\u06a6\u06a8\7Q\2\2\u06a7"+
		"\u06a9\7]\2\2\u06a8\u06a7\3\2\2\2\u06a8\u06a9\3\2\2\2\u06a9\u06aa\3\2"+
		"\2\2\u06aa\u06ab\7`\2\2\u06ab\u00fd\3\2\2\2\u06ac\u06ae\7`\2\2\u06ad\u06af"+
		"\7]\2\2\u06ae\u06ad\3\2\2\2\u06ae\u06af\3\2\2\2\u06af\u06b0\3\2\2\2\u06b0"+
		"\u06b2\7\\\2\2\u06b1\u06b3\7]\2\2\u06b2\u06b1\3\2\2\2\u06b2\u06b3\3\2"+
		"\2\2\u06b3\u06b4\3\2\2\2\u06b4\u06b5\5`\61\2\u06b5\u00ff\3\2\2\2\u0126"+
		"\u0101\u0104\u0107\u010a\u010d\u0110\u0113\u011a\u0120\u012a\u0130\u0134"+
		"\u0139\u013e\u0141\u0144\u0147\u014b\u014f\u0155\u0159\u015e\u0163\u0167"+
		"\u016b\u0170\u0173\u0176\u017c\u0180\u0185\u018a\u018e\u0193\u019a\u019e"+
		"\u01a2\u01a6\u01ab\u01b2\u01b6\u01ba\u01be\u01c3\u01c8\u01cc\u01cf\u01d3"+
		"\u01d7\u01dc\u01e1\u01e5\u01ea\u01f0\u01f4\u01fa\u01fe\u0203\u0208\u020c"+
		"\u020e\u0215\u021b\u021f\u0224\u0247\u024b\u0250\u0257\u025b\u025e\u0261"+
		"\u026a\u026e\u0272\u0276\u027a\u027e\u0288\u028e\u0292\u0295\u0298\u02a1"+
		"\u02a5\u02a8\u02ab\u02b4\u02b8\u02bb\u02be\u02c7\u02cb\u02cf\u02d6\u02dc"+
		"\u02e0\u02e4\u02ee\u02f1\u02f4\u02fa\u02fe\u0302\u0306\u030a\u0313\u0317"+
		"\u031b\u031f\u0323\u0327\u032b\u032f\u0339\u033d\u0341\u0345\u034b\u034e"+
		"\u0351\u0357\u035d\u0361\u0365\u036d\u0372\u0376\u037a\u037e\u0385\u0389"+
		"\u038d\u0391\u0395\u039a\u039f\u03a3\u03a7\u03ab\u03af\u03b5\u03ba\u03be"+
		"\u03c4\u03c9\u03cf\u03d3\u03d7\u03db\u03df\u03e3\u03e8\u03ec\u03f0\u03f3"+
		"\u040b\u0411\u0415\u041f\u0423\u0427\u042b\u043b\u043f\u044b\u044f\u0455"+
		"\u0459\u045c\u045f\u0462\u046a\u046d\u0471\u0474\u0477\u047d\u0481\u0487"+
		"\u048b\u0490\u049b\u049f\u04a3\u04a9\u04ad\u04b0\u04b4\u04b7\u04ba\u04c0"+
		"\u04c3\u04c6\u04cc\u04cf\u04d2\u04d8\u04dc\u04e2\u04e6\u04ea\u04ee\u04f4"+
		"\u04f8\u04fc\u0502\u0506\u050a\u0510\u0514\u0517\u051b\u051f\u0524\u0531"+
		"\u0535\u0539\u053f\u0543\u0548\u054f\u0552\u0556\u0559\u055c\u0566\u0569"+
		"\u056c\u0570\u0573\u0576\u057c\u057f\u0582\u0586\u0589\u058c\u0592\u0595"+
		"\u0598\u059c\u059f\u05a2\u05ab\u05b3\u05bb\u05c4\u05c7\u05cd\u05d3\u05d9"+
		"\u05df\u05e5\u05ef\u05f6\u05fc\u0600\u0604\u0607\u060b\u060f\u0613\u0618"+
		"\u061c\u0620\u0624\u062c\u0630\u0636\u063a\u063e\u0643\u0646\u064d\u0651"+
		"\u0655\u0659\u065d\u0662\u0666\u067c\u0680\u0684\u0689\u0692\u069a\u069e"+
		"\u06a4\u06a8\u06ae\u06b2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}