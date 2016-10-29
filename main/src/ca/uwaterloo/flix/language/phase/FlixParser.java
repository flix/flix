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
		T__87=88, T__88=89, T__89=90, T__90=91, T__91=92, T__92=93, TripleSlashComment=94, 
		WS=95, SC=96, Comment=97, LowerIdent=98, UpperIdent=99, FNil=100, Wild=101, 
		Digits=102;
	public static final int
		RULE_tscomment = 0, RULE_sp = 1, RULE_start = 2, RULE_optSC = 3, RULE_ident = 4, 
		RULE_nname = 5, RULE_lowerqname = 6, RULE_upperqname = 7, RULE_annotationName = 8, 
		RULE_attributeName = 9, RULE_className = 10, RULE_definitionName = 11, 
		RULE_qualifiedDefinitionName = 12, RULE_tableName = 13, RULE_qualifiedTableName = 14, 
		RULE_tagName = 15, RULE_typeName = 16, RULE_qualifiedTypeName = 17, RULE_variableName = 18, 
		RULE_variableNames = 19, RULE_argument = 20, RULE_arguments = 21, RULE_formalparams = 22, 
		RULE_attribute = 23, RULE_attributes = 24, RULE_index = 25, RULE_indexes = 26, 
		RULE_idents = 27, RULE_match_rule = 28, RULE_match_rules = 29, RULE_switch_rule = 30, 
		RULE_switch_rules = 31, RULE_typeparam = 32, RULE_typeparams = 33, RULE_class_typeparams = 34, 
		RULE_contextBound = 35, RULE_contextBounds = 36, RULE_contextBoundsList = 37, 
		RULE_annotation = 38, RULE_annotations = 39, RULE_s_import = 40, RULE_import_wildcard = 41, 
		RULE_import_definition = 42, RULE_import_namespace = 43, RULE_decl = 44, 
		RULE_decls_namespace = 45, RULE_decls_enum = 46, RULE_dcases = 47, RULE_dcase = 48, 
		RULE_decls_relation = 49, RULE_decls_lattice = 50, RULE_decls_index = 51, 
		RULE_decls_signature = 52, RULE_decls_external = 53, RULE_decls_definition = 54, 
		RULE_decls_law = 55, RULE_decls_class = 56, RULE_class_body = 57, RULE_decls_fact = 58, 
		RULE_decls_rule = 59, RULE_elms = 60, RULE_decls_letlattice = 61, RULE_decls_impl = 62, 
		RULE_decls_impl_body = 63, RULE_expression = 64, RULE_block = 65, RULE_logical = 66, 
		RULE_expressions = 67, RULE_comparison = 68, RULE_additive = 69, RULE_multiplicative = 70, 
		RULE_infix = 71, RULE_extended = 72, RULE_unary = 73, RULE_ascribe = 74, 
		RULE_e_primary = 75, RULE_e_letMatch = 76, RULE_e_ifThenElse = 77, RULE_e_match = 78, 
		RULE_e_switch = 79, RULE_e_apply = 80, RULE_e_sname = 81, RULE_e_qname = 82, 
		RULE_e_tag = 83, RULE_e_tuple = 84, RULE_e_keyValue = 85, RULE_e_keyValues = 86, 
		RULE_e_userError = 87, RULE_e_wild = 88, RULE_e_fNil = 89, RULE_e_fList = 90, 
		RULE_e_fVec = 91, RULE_e_fSet = 92, RULE_e_fMap = 93, RULE_e_unaryLambda = 94, 
		RULE_e_lambda = 95, RULE_existential = 96, RULE_universal = 97, RULE_pattern = 98, 
		RULE_patterns = 99, RULE_simple = 100, RULE_p_keyValue = 101, RULE_p_keyValues = 102, 
		RULE_p_tag = 103, RULE_p_tuple = 104, RULE_p_wild = 105, RULE_p_fNil = 106, 
		RULE_p_variable = 107, RULE_p_fVec = 108, RULE_p_fSet = 109, RULE_p_fMap = 110, 
		RULE_bools = 111, RULE_chars = 112, RULE_strs = 113, RULE_negative = 114, 
		RULE_float32 = 115, RULE_float64 = 116, RULE_floatDefault = 117, RULE_floats = 118, 
		RULE_int8 = 119, RULE_int16 = 120, RULE_int32 = 121, RULE_int64 = 122, 
		RULE_bigInt = 123, RULE_intDefault = 124, RULE_ints = 125, RULE_literal = 126, 
		RULE_primary = 127, RULE_var = 128, RULE_ref = 129, RULE_type = 130, RULE_arrow = 131, 
		RULE_tuple_unit = 132, RULE_tuple_singleton = 133, RULE_tuple_multi = 134, 
		RULE_tuple = 135, RULE_apply = 136, RULE_unary_ops = 137, RULE_logical_ops = 138, 
		RULE_comparison_ops = 139, RULE_multipve_ops = 140, RULE_addve_ops = 141, 
		RULE_extbin_ops = 142, RULE_predicate = 143, RULE_predicates = 144, RULE_pred_true = 145, 
		RULE_pred_false = 146, RULE_pred_filter = 147, RULE_pred_table = 148, 
		RULE_pred_notequal = 149, RULE_pred_loop = 150;
	public static final String[] ruleNames = {
		"tscomment", "sp", "start", "optSC", "ident", "nname", "lowerqname", "upperqname", 
		"annotationName", "attributeName", "className", "definitionName", "qualifiedDefinitionName", 
		"tableName", "qualifiedTableName", "tagName", "typeName", "qualifiedTypeName", 
		"variableName", "variableNames", "argument", "arguments", "formalparams", 
		"attribute", "attributes", "index", "indexes", "idents", "match_rule", 
		"match_rules", "switch_rule", "switch_rules", "typeparam", "typeparams", 
		"class_typeparams", "contextBound", "contextBounds", "contextBoundsList", 
		"annotation", "annotations", "s_import", "import_wildcard", "import_definition", 
		"import_namespace", "decl", "decls_namespace", "decls_enum", "dcases", 
		"dcase", "decls_relation", "decls_lattice", "decls_index", "decls_signature", 
		"decls_external", "decls_definition", "decls_law", "decls_class", "class_body", 
		"decls_fact", "decls_rule", "elms", "decls_letlattice", "decls_impl", 
		"decls_impl_body", "expression", "block", "logical", "expressions", "comparison", 
		"additive", "multiplicative", "infix", "extended", "unary", "ascribe", 
		"e_primary", "e_letMatch", "e_ifThenElse", "e_match", "e_switch", "e_apply", 
		"e_sname", "e_qname", "e_tag", "e_tuple", "e_keyValue", "e_keyValues", 
		"e_userError", "e_wild", "e_fNil", "e_fList", "e_fVec", "e_fSet", "e_fMap", 
		"e_unaryLambda", "e_lambda", "existential", "universal", "pattern", "patterns", 
		"simple", "p_keyValue", "p_keyValues", "p_tag", "p_tuple", "p_wild", "p_fNil", 
		"p_variable", "p_fVec", "p_fSet", "p_fMap", "bools", "chars", "strs", 
		"negative", "float32", "float64", "floatDefault", "floats", "int8", "int16", 
		"int32", "int64", "bigInt", "intDefault", "ints", "literal", "primary", 
		"var", "ref", "type", "arrow", "tuple_unit", "tuple_singleton", "tuple_multi", 
		"tuple", "apply", "unary_ops", "logical_ops", "comparison_ops", "multipve_ops", 
		"addve_ops", "extbin_ops", "predicate", "predicates", "pred_true", "pred_false", 
		"pred_filter", "pred_table", "pred_notequal", "pred_loop"
	};

	private static final String[] _LITERAL_NAMES = {
		null, "'.'", "'/'", "','", "':'", "'('", "')'", "'{'", "'}'", "'case'", 
		"'=>'", "'['", "']'", "'<='", "'@'", "'import'", "'namespace'", "'enum'", 
		"'rel'", "'lat'", "'index'", "'def'", "'external'", "'='", "'law'", "'class'", 
		"':-'", "'let'", "'<>'", "'impl'", "'`'", "'if'", "'else'", "'match'", 
		"'with'", "'switch'", "'->'", "'???'", "'::'", "'#['", "'#{'", "'@{'", 
		"'∃'", "'\\exists'", "'∀'", "'\\forall'", "'...'", "'true'", "'false'", 
		"'''", "'\"'", "'\"'", "'\n'", "'\r'", "'-'", "'f32'", "'f64'", "'i8'", 
		"'i16'", "'i32'", "'i64'", "'ii'", "'+'", "'¬'", "'~'", "'!'", "'&&'", 
		"'||'", "'&'", "'|'", "'==>'", "'<==>'", "'^'", "'<<'", "'>>'", "'∧'", 
		"'∨'", "'→'", "'↔'", "'>='", "'<'", "'>'", "'=='", "'!='", "'≡'", "'**'", 
		"'*'", "'%'", "'⊑'", "'⊔'", "'⊓'", "'▽'", "'△'", "'<-'", null, null, "';'", 
		null, null, null, "'Nil'", "'_'"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, "TripleSlashComment", 
		"WS", "SC", "Comment", "LowerIdent", "UpperIdent", "FNil", "Wild", "Digits"
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
			setState(302);
			sp();
			setState(303);
			match(TripleSlashComment);
			setState(304);
			sp();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SpContext extends ParserRuleContext {
		public SpContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_sp; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterSp(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitSp(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitSp(this);
			else return visitor.visitChildren(this);
		}
	}

	public final SpContext sp() throws RecognitionException {
		SpContext _localctx = new SpContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_sp);
		try {
			enterOuterAlt(_localctx, 1);
			{
			}
		}
		catch (RecognitionException re) {
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
		public TerminalNode WS() { return getToken(FlixParser.WS, 0); }
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
			setState(311);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,0,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(308);
					s_import();
					}
					} 
				}
				setState(313);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,0,_ctx);
			}
			setState(317);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,1,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(314);
					decl();
					}
					} 
				}
				setState(319);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,1,_ctx);
			}
			setState(321);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(320);
				match(WS);
				}
			}

			setState(323);
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
			setState(329);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,4,_ctx) ) {
			case 1:
				{
				setState(326);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(325);
					match(WS);
					}
				}

				setState(328);
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
			setState(331);
			sp();
			setState(332);
			_la = _input.LA(1);
			if ( !(_la==LowerIdent || _la==UpperIdent) ) {
			_errHandler.recoverInline(this);
			} else {
				consume();
			}
			setState(333);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
			setState(335);
			sp();
			setState(336);
			ident();
			setState(341);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__0) {
				{
				{
				setState(337);
				match(T__0);
				setState(338);
				ident();
				}
				}
				setState(343);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(344);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
			setState(346);
			sp();
			setState(350);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,6,_ctx) ) {
			case 1:
				{
				setState(347);
				nname();
				setState(348);
				match(T__1);
				}
				break;
			}
			setState(352);
			match(LowerIdent);
			setState(353);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
			setState(355);
			sp();
			setState(359);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,7,_ctx) ) {
			case 1:
				{
				setState(356);
				nname();
				setState(357);
				match(T__1);
				}
				break;
			}
			setState(361);
			match(UpperIdent);
			setState(362);
			sp();
			}
		}
		catch (RecognitionException re) {
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
			setState(366);
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
			setState(368);
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
			setState(370);
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
			setState(372);
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
			setState(374);
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
			setState(376);
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
			setState(380);
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
			setState(382);
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
			setState(384);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
			enterOuterAlt(_localctx, 1);
			{
			setState(386);
			variableName();
			{
			setState(388);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(387);
				match(WS);
				}
			}

			setState(390);
			match(T__2);
			setState(392);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(391);
				match(WS);
				}
			}

			setState(394);
			variableName();
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public VariableNameContext variableName() {
			return getRuleContext(VariableNameContext.class,0);
		}
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
			setState(396);
			sp();
			setState(397);
			variableName();
			setState(398);
			match(T__3);
			setState(400);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(399);
				match(WS);
				}
			}

			setState(402);
			type();
			setState(403);
			sp();
			}
		}
		catch (RecognitionException re) {
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
			_alt = getInterpreter().adaptivePredict(_input,13,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(407);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(406);
						match(WS);
						}
					}

					setState(409);
					match(T__2);
					setState(411);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(410);
						match(WS);
						}
					}

					setState(413);
					argument();
					}
					} 
				}
				setState(418);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,13,_ctx);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
				switch ( getInterpreter().adaptivePredict(_input,14,_ctx) ) {
				case 1:
					{
					setState(420);
					match(WS);
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
				if (_la==WS) {
					{
					setState(426);
					match(WS);
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public AttributeNameContext attributeName() {
			return getRuleContext(AttributeNameContext.class,0);
		}
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
			sp();
			setState(433);
			attributeName();
			setState(435);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(434);
				match(WS);
				}
			}

			setState(437);
			match(T__3);
			setState(439);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(438);
				match(WS);
				}
			}

			setState(441);
			type();
			setState(442);
			sp();
			}
		}
		catch (RecognitionException re) {
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
			setState(444);
			attribute();
			setState(455);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,22,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(446);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(445);
						match(WS);
						}
					}

					setState(448);
					match(T__2);
					setState(450);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(449);
						match(WS);
						}
					}

					setState(452);
					attribute();
					}
					} 
				}
				setState(457);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,22,_ctx);
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
			setState(458);
			match(T__6);
			setState(460);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,23,_ctx) ) {
			case 1:
				{
				setState(459);
				match(WS);
				}
				break;
			}
			setState(476);
			_la = _input.LA(1);
			if (_la==LowerIdent) {
				{
				setState(462);
				attributeName();
				setState(473);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,26,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(464);
						_la = _input.LA(1);
						if (_la==WS) {
							{
							setState(463);
							match(WS);
							}
						}

						setState(466);
						match(T__2);
						setState(468);
						_la = _input.LA(1);
						if (_la==WS) {
							{
							setState(467);
							match(WS);
							}
						}

						setState(470);
						attributeName();
						}
						} 
					}
					setState(475);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,26,_ctx);
				}
				}
			}

			setState(479);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(478);
				match(WS);
				}
			}

			setState(481);
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
			setState(483);
			index();
			setState(494);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,31,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(485);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(484);
						match(WS);
						}
					}

					setState(487);
					match(T__2);
					setState(489);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(488);
						match(WS);
						}
					}

					setState(491);
					index();
					}
					} 
				}
				setState(496);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,31,_ctx);
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
		public List<IdentContext> ident() {
			return getRuleContexts(IdentContext.class);
		}
		public IdentContext ident(int i) {
			return getRuleContext(IdentContext.class,i);
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitIdents(this);
			else return visitor.visitChildren(this);
		}
	}

	public final IdentsContext idents() throws RecognitionException {
		IdentsContext _localctx = new IdentsContext(_ctx, getState());
		enterRule(_localctx, 54, RULE_idents);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(497);
			ident();
			setState(508);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__2 || _la==WS) {
				{
				{
				setState(499);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(498);
					match(WS);
					}
				}

				setState(501);
				match(T__2);
				setState(503);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(502);
					match(WS);
					}
				}

				setState(505);
				ident();
				}
				}
				setState(510);
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitMatch_rule(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Match_ruleContext match_rule() throws RecognitionException {
		Match_ruleContext _localctx = new Match_ruleContext(_ctx, getState());
		enterRule(_localctx, 56, RULE_match_rule);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(511);
			match(T__8);
			setState(512);
			match(WS);
			setState(513);
			pattern();
			setState(515);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(514);
				match(WS);
				}
			}

			setState(517);
			match(T__9);
			setState(519);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(518);
				match(WS);
				}
			}

			setState(521);
			expression();
			setState(523);
			_la = _input.LA(1);
			if (_la==SC) {
				{
				setState(522);
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitMatch_rules(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Match_rulesContext match_rules() throws RecognitionException {
		Match_rulesContext _localctx = new Match_rulesContext(_ctx, getState());
		enterRule(_localctx, 58, RULE_match_rules);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(525);
			match_rule();
			setState(532);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,39,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(527);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(526);
						match(WS);
						}
					}

					setState(529);
					match_rule();
					}
					} 
				}
				setState(534);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,39,_ctx);
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitSwitch_rule(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Switch_ruleContext switch_rule() throws RecognitionException {
		Switch_ruleContext _localctx = new Switch_ruleContext(_ctx, getState());
		enterRule(_localctx, 60, RULE_switch_rule);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(535);
			match(T__8);
			setState(536);
			match(WS);
			setState(537);
			expression();
			setState(539);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(538);
				match(WS);
				}
			}

			setState(541);
			match(T__9);
			setState(543);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(542);
				match(WS);
				}
			}

			setState(545);
			expression();
			setState(547);
			_la = _input.LA(1);
			if (_la==SC) {
				{
				setState(546);
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitSwitch_rules(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Switch_rulesContext switch_rules() throws RecognitionException {
		Switch_rulesContext _localctx = new Switch_rulesContext(_ctx, getState());
		enterRule(_localctx, 62, RULE_switch_rules);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(549);
			switch_rule();
			setState(556);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,44,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(551);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(550);
						match(WS);
						}
					}

					setState(553);
					switch_rule();
					}
					} 
				}
				setState(558);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,44,_ctx);
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public VariableNameContext variableName() {
			return getRuleContext(VariableNameContext.class,0);
		}
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitTypeparam(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TypeparamContext typeparam() throws RecognitionException {
		TypeparamContext _localctx = new TypeparamContext(_ctx, getState());
		enterRule(_localctx, 64, RULE_typeparam);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(559);
			sp();
			setState(560);
			variableName();
			setState(569);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,47,_ctx) ) {
			case 1:
				{
				setState(562);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(561);
					match(WS);
					}
				}

				setState(564);
				match(T__3);
				setState(566);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(565);
					match(WS);
					}
				}

				setState(568);
				type();
				}
				break;
			}
			setState(571);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitTypeparams(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TypeparamsContext typeparams() throws RecognitionException {
		TypeparamsContext _localctx = new TypeparamsContext(_ctx, getState());
		enterRule(_localctx, 66, RULE_typeparams);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(593);
			_la = _input.LA(1);
			if (_la==T__10) {
				{
				setState(573);
				match(T__10);
				setState(575);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(574);
					match(WS);
					}
				}

				setState(577);
				typeparam();
				setState(588);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==T__2 || _la==WS) {
					{
					{
					setState(579);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(578);
						match(WS);
						}
					}

					setState(581);
					match(T__2);
					setState(583);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(582);
						match(WS);
						}
					}

					setState(585);
					typeparam();
					}
					}
					setState(590);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(591);
				match(T__11);
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitClass_typeparams(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Class_typeparamsContext class_typeparams() throws RecognitionException {
		Class_typeparamsContext _localctx = new Class_typeparamsContext(_ctx, getState());
		enterRule(_localctx, 68, RULE_class_typeparams);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(595);
			match(T__10);
			setState(596);
			type();
			setState(607);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__2 || _la==WS) {
				{
				{
				setState(598);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(597);
					match(WS);
					}
				}

				setState(600);
				match(T__2);
				setState(602);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(601);
					match(WS);
					}
				}

				setState(604);
				type();
				}
				}
				setState(609);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(610);
			match(T__11);
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		enterRule(_localctx, 70, RULE_contextBound);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(612);
			sp();
			setState(613);
			className();
			setState(614);
			class_typeparams();
			setState(615);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitContextBounds(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ContextBoundsContext contextBounds() throws RecognitionException {
		ContextBoundsContext _localctx = new ContextBoundsContext(_ctx, getState());
		enterRule(_localctx, 72, RULE_contextBounds);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(617);
			contextBound();
			setState(628);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,58,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(619);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(618);
						match(WS);
						}
					}

					setState(621);
					match(T__2);
					setState(623);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(622);
						match(WS);
						}
					}

					setState(625);
					contextBound();
					}
					} 
				}
				setState(630);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,58,_ctx);
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitContextBoundsList(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ContextBoundsListContext contextBoundsList() throws RecognitionException {
		ContextBoundsListContext _localctx = new ContextBoundsListContext(_ctx, getState());
		enterRule(_localctx, 74, RULE_contextBoundsList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(642);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,62,_ctx) ) {
			case 1:
				{
				setState(632);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(631);
					match(WS);
					}
				}

				setState(634);
				match(T__12);
				setState(636);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(635);
					match(WS);
					}
				}

				setState(638);
				contextBounds();
				setState(640);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,61,_ctx) ) {
				case 1:
					{
					setState(639);
					match(WS);
					}
					break;
				}
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
			setState(644);
			sp();
			setState(645);
			match(T__13);
			setState(646);
			annotationName();
			setState(647);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
			setState(649);
			annotation();
			setState(654);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,63,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(650);
					match(WS);
					setState(651);
					annotation();
					}
					} 
				}
				setState(656);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,63,_ctx);
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
		public TerminalNode WS() { return getToken(FlixParser.WS, 0); }
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
			setState(658);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(657);
				match(WS);
				}
			}

			setState(663);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,65,_ctx) ) {
			case 1:
				{
				setState(660);
				import_wildcard();
				}
				break;
			case 2:
				{
				setState(661);
				import_definition();
				}
				break;
			case 3:
				{
				setState(662);
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
			setState(665);
			sp();
			setState(666);
			match(T__14);
			setState(667);
			match(WS);
			setState(668);
			nname();
			setState(669);
			match(T__1);
			setState(670);
			match(Wild);
			setState(671);
			optSC();
			setState(672);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public TerminalNode WS() { return getToken(FlixParser.WS, 0); }
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
			setState(674);
			sp();
			setState(675);
			match(T__14);
			setState(676);
			match(WS);
			setState(677);
			nname();
			setState(678);
			match(T__1);
			setState(679);
			ident();
			setState(680);
			optSC();
			setState(681);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
			setState(683);
			sp();
			setState(684);
			match(T__14);
			setState(685);
			match(WS);
			setState(686);
			nname();
			setState(687);
			optSC();
			setState(688);
			sp();
			}
		}
		catch (RecognitionException re) {
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
			setState(703);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,66,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(690);
				decls_namespace();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(691);
				decls_enum();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(692);
				decls_relation();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(693);
				decls_lattice();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(694);
				decls_index();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(695);
				decls_signature();
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(696);
				decls_external();
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(697);
				decls_definition();
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(698);
				decls_law();
				}
				break;
			case 10:
				enterOuterAlt(_localctx, 10);
				{
				setState(699);
				decls_class();
				}
				break;
			case 11:
				enterOuterAlt(_localctx, 11);
				{
				setState(700);
				decls_fact();
				}
				break;
			case 12:
				enterOuterAlt(_localctx, 12);
				{
				setState(701);
				decls_rule();
				}
				break;
			case 13:
				enterOuterAlt(_localctx, 13);
				{
				setState(702);
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
			setState(706);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(705);
				match(WS);
				}
			}

			setState(708);
			sp();
			setState(709);
			match(T__15);
			setState(710);
			match(WS);
			setState(711);
			nname();
			setState(713);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(712);
				match(WS);
				}
			}

			setState(715);
			match(T__6);
			setState(717);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,69,_ctx) ) {
			case 1:
				{
				setState(716);
				match(WS);
				}
				break;
			}
			setState(722);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,70,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(719);
					decl();
					}
					} 
				}
				setState(724);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,70,_ctx);
			}
			setState(726);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(725);
				match(WS);
				}
			}

			setState(728);
			match(T__7);
			setState(729);
			sp();
			setState(730);
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
			setState(738);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,73,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(733);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(732);
						match(WS);
						}
					}

					setState(735);
					tscomment();
					}
					} 
				}
				setState(740);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,73,_ctx);
			}
			setState(742);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(741);
				match(WS);
				}
			}

			setState(744);
			sp();
			setState(745);
			match(T__16);
			setState(746);
			match(WS);
			setState(747);
			typeName();
			setState(748);
			typeparams();
			setState(750);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(749);
				match(WS);
				}
			}

			setState(752);
			match(T__6);
			setState(754);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(753);
				match(WS);
				}
			}

			setState(756);
			dcases();
			setState(758);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(757);
				match(WS);
				}
			}

			setState(760);
			match(T__7);
			setState(761);
			sp();
			setState(762);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
			setState(764);
			dcase();
			setState(775);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,80,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(766);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(765);
						match(WS);
						}
					}

					setState(768);
					match(T__2);
					setState(770);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(769);
						match(WS);
						}
					}

					setState(772);
					dcase();
					}
					} 
				}
				setState(777);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,80,_ctx);
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public TerminalNode WS() { return getToken(FlixParser.WS, 0); }
		public TagNameContext tagName() {
			return getRuleContext(TagNameContext.class,0);
		}
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
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
			setState(778);
			sp();
			setState(779);
			match(T__8);
			setState(780);
			match(WS);
			setState(781);
			tagName();
			setState(783);
			_la = _input.LA(1);
			if (_la==T__4) {
				{
				setState(782);
				type();
				}
			}

			setState(785);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
			setState(793);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,83,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(788);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(787);
						match(WS);
						}
					}

					setState(790);
					tscomment();
					}
					} 
				}
				setState(795);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,83,_ctx);
			}
			setState(797);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(796);
				match(WS);
				}
			}

			setState(799);
			sp();
			setState(800);
			match(T__17);
			setState(801);
			match(WS);
			setState(802);
			tableName();
			setState(804);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(803);
				match(WS);
				}
			}

			setState(806);
			match(T__4);
			setState(808);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,86,_ctx) ) {
			case 1:
				{
				setState(807);
				match(WS);
				}
				break;
			}
			setState(811);
			_la = _input.LA(1);
			if (_la==LowerIdent) {
				{
				setState(810);
				attributes();
				}
			}

			setState(814);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(813);
				match(WS);
				}
			}

			setState(816);
			match(T__5);
			setState(817);
			sp();
			setState(818);
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
			setState(826);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,90,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(821);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(820);
						match(WS);
						}
					}

					setState(823);
					tscomment();
					}
					} 
				}
				setState(828);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,90,_ctx);
			}
			setState(830);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(829);
				match(WS);
				}
			}

			setState(832);
			sp();
			setState(833);
			match(T__18);
			setState(834);
			match(WS);
			setState(835);
			tableName();
			setState(837);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(836);
				match(WS);
				}
			}

			setState(839);
			match(T__4);
			setState(841);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,93,_ctx) ) {
			case 1:
				{
				setState(840);
				match(WS);
				}
				break;
			}
			setState(844);
			_la = _input.LA(1);
			if (_la==LowerIdent) {
				{
				setState(843);
				attributes();
				}
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
			match(T__5);
			setState(850);
			sp();
			setState(851);
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
			setState(854);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(853);
				match(WS);
				}
			}

			setState(856);
			sp();
			setState(857);
			match(T__19);
			setState(858);
			match(WS);
			setState(859);
			qualifiedTableName();
			setState(861);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(860);
				match(WS);
				}
			}

			setState(863);
			match(T__4);
			setState(865);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,98,_ctx) ) {
			case 1:
				{
				setState(864);
				match(WS);
				}
				break;
			}
			setState(868);
			_la = _input.LA(1);
			if (_la==T__6) {
				{
				setState(867);
				indexes();
				}
			}

			setState(871);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(870);
				match(WS);
				}
			}

			setState(873);
			match(T__5);
			setState(874);
			sp();
			setState(875);
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
			setState(883);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,102,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(878);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(877);
						match(WS);
						}
					}

					setState(880);
					tscomment();
					}
					} 
				}
				setState(885);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,102,_ctx);
			}
			setState(887);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(886);
				match(WS);
				}
			}

			setState(889);
			sp();
			setState(890);
			match(T__20);
			setState(891);
			match(WS);
			setState(892);
			definitionName();
			setState(894);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,104,_ctx) ) {
			case 1:
				{
				setState(893);
				match(WS);
				}
				break;
			}
			setState(896);
			formalparams();
			setState(898);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(897);
				match(WS);
				}
			}

			setState(900);
			match(T__3);
			setState(902);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(901);
				match(WS);
				}
			}

			setState(904);
			type();
			setState(905);
			sp();
			setState(906);
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
			setState(914);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,108,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(909);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(908);
						match(WS);
						}
					}

					setState(911);
					tscomment();
					}
					} 
				}
				setState(916);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,108,_ctx);
			}
			setState(918);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(917);
				match(WS);
				}
			}

			setState(920);
			sp();
			setState(921);
			match(T__21);
			setState(923);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(922);
				match(WS);
				}
			}

			setState(925);
			match(T__20);
			setState(926);
			match(WS);
			setState(927);
			definitionName();
			setState(929);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,111,_ctx) ) {
			case 1:
				{
				setState(928);
				match(WS);
				}
				break;
			}
			setState(931);
			formalparams();
			setState(933);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(932);
				match(WS);
				}
			}

			setState(935);
			match(T__3);
			setState(937);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(936);
				match(WS);
				}
			}

			setState(939);
			type();
			setState(940);
			sp();
			setState(941);
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
			setState(949);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,115,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(944);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(943);
						match(WS);
						}
					}

					setState(946);
					tscomment();
					}
					} 
				}
				setState(951);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,115,_ctx);
			}
			setState(953);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,116,_ctx) ) {
			case 1:
				{
				setState(952);
				match(WS);
				}
				break;
			}
			setState(956);
			_la = _input.LA(1);
			if (_la==T__13) {
				{
				setState(955);
				annotations();
				}
			}

			setState(959);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(958);
				match(WS);
				}
			}

			setState(961);
			sp();
			setState(962);
			match(T__20);
			setState(963);
			match(WS);
			setState(964);
			definitionName();
			setState(966);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,119,_ctx) ) {
			case 1:
				{
				setState(965);
				match(WS);
				}
				break;
			}
			setState(968);
			typeparams();
			setState(969);
			formalparams();
			setState(971);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(970);
				match(WS);
				}
			}

			setState(973);
			match(T__3);
			setState(975);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(974);
				match(WS);
				}
			}

			setState(977);
			type();
			setState(979);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(978);
				match(WS);
				}
			}

			setState(981);
			match(T__22);
			setState(983);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(982);
				match(WS);
				}
			}

			setState(985);
			expression();
			setState(986);
			sp();
			setState(987);
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
			setState(995);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,125,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(990);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(989);
						match(WS);
						}
					}

					setState(992);
					tscomment();
					}
					} 
				}
				setState(997);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,125,_ctx);
			}
			setState(999);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(998);
				match(WS);
				}
			}

			setState(1001);
			sp();
			setState(1002);
			match(T__23);
			setState(1003);
			match(WS);
			setState(1004);
			definitionName();
			setState(1006);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,127,_ctx) ) {
			case 1:
				{
				setState(1005);
				match(WS);
				}
				break;
			}
			setState(1008);
			typeparams();
			setState(1010);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,128,_ctx) ) {
			case 1:
				{
				setState(1009);
				match(WS);
				}
				break;
			}
			setState(1012);
			formalparams();
			setState(1014);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1013);
				match(WS);
				}
			}

			setState(1016);
			match(T__3);
			setState(1018);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1017);
				match(WS);
				}
			}

			setState(1020);
			type();
			setState(1022);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1021);
				match(WS);
				}
			}

			setState(1024);
			match(T__22);
			setState(1026);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1025);
				match(WS);
				}
			}

			setState(1028);
			expression();
			setState(1029);
			sp();
			setState(1030);
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
			setState(1038);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,134,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1033);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1032);
						match(WS);
						}
					}

					setState(1035);
					tscomment();
					}
					} 
				}
				setState(1040);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,134,_ctx);
			}
			setState(1042);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1041);
				match(WS);
				}
			}

			setState(1044);
			sp();
			setState(1045);
			match(T__24);
			setState(1046);
			match(WS);
			setState(1047);
			className();
			setState(1048);
			class_typeparams();
			setState(1050);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,136,_ctx) ) {
			case 1:
				{
				setState(1049);
				match(WS);
				}
				break;
			}
			setState(1052);
			contextBoundsList();
			setState(1054);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1053);
				match(WS);
				}
			}

			setState(1056);
			class_body();
			setState(1057);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public Decls_definitionContext decls_definition() {
			return getRuleContext(Decls_definitionContext.class,0);
		}
		public Decls_signatureContext decls_signature() {
			return getRuleContext(Decls_signatureContext.class,0);
		}
		public Decls_lawContext decls_law() {
			return getRuleContext(Decls_lawContext.class,0);
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
			setState(1059);
			match(T__6);
			setState(1061);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,138,_ctx) ) {
			case 1:
				{
				setState(1060);
				match(WS);
				}
				break;
			}
			{
			setState(1066);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,139,_ctx) ) {
			case 1:
				{
				setState(1063);
				decls_definition();
				}
				break;
			case 2:
				{
				setState(1064);
				decls_signature();
				}
				break;
			case 3:
				{
				setState(1065);
				decls_law();
				}
				break;
			}
			setState(1069);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1068);
				match(WS);
				}
			}

			}
			setState(1071);
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

	public static class Decls_factContext extends ParserRuleContext {
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public PredicateContext predicate() {
			return getRuleContext(PredicateContext.class,0);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
		enterRule(_localctx, 116, RULE_decls_fact);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1074);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,141,_ctx) ) {
			case 1:
				{
				setState(1073);
				match(WS);
				}
				break;
			}
			setState(1076);
			sp();
			setState(1077);
			predicate();
			setState(1079);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1078);
				match(WS);
				}
			}

			setState(1081);
			match(T__0);
			setState(1082);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitDecls_rule(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Decls_ruleContext decls_rule() throws RecognitionException {
		Decls_ruleContext _localctx = new Decls_ruleContext(_ctx, getState());
		enterRule(_localctx, 118, RULE_decls_rule);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1085);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,143,_ctx) ) {
			case 1:
				{
				setState(1084);
				match(WS);
				}
				break;
			}
			setState(1087);
			sp();
			setState(1088);
			predicate();
			setState(1090);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1089);
				match(WS);
				}
			}

			setState(1092);
			match(T__25);
			setState(1094);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1093);
				match(WS);
				}
			}

			setState(1096);
			predicates();
			setState(1098);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1097);
				match(WS);
				}
			}

			setState(1100);
			match(T__0);
			setState(1101);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		enterRule(_localctx, 120, RULE_elms);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1103);
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitDecls_letlattice(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Decls_letlatticeContext decls_letlattice() throws RecognitionException {
		Decls_letlatticeContext _localctx = new Decls_letlatticeContext(_ctx, getState());
		enterRule(_localctx, 122, RULE_decls_letlattice);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1106);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1105);
				match(WS);
				}
			}

			setState(1108);
			sp();
			setState(1109);
			match(T__26);
			setState(1111);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1110);
				match(WS);
				}
			}

			setState(1113);
			type();
			setState(1114);
			match(T__27);
			setState(1116);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1115);
				match(WS);
				}
			}

			setState(1118);
			match(T__22);
			setState(1120);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1119);
				match(WS);
				}
			}

			setState(1122);
			match(T__4);
			setState(1124);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1123);
				match(WS);
				}
			}

			setState(1126);
			elms();
			setState(1128);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1127);
				match(WS);
				}
			}

			setState(1130);
			match(T__5);
			setState(1131);
			sp();
			setState(1132);
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
		public Decls_impl_bodyContext decls_impl_body() {
			return getRuleContext(Decls_impl_bodyContext.class,0);
		}
		public List<TscommentContext> tscomment() {
			return getRuleContexts(TscommentContext.class);
		}
		public TscommentContext tscomment(int i) {
			return getRuleContext(TscommentContext.class,i);
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
		enterRule(_localctx, 124, RULE_decls_impl);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1140);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,154,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1135);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1134);
						match(WS);
						}
					}

					setState(1137);
					tscomment();
					}
					} 
				}
				setState(1142);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,154,_ctx);
			}
			setState(1144);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1143);
				match(WS);
				}
			}

			setState(1146);
			sp();
			setState(1147);
			match(T__28);
			setState(1148);
			match(WS);
			setState(1149);
			className();
			setState(1150);
			class_typeparams();
			setState(1152);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,156,_ctx) ) {
			case 1:
				{
				setState(1151);
				match(WS);
				}
				break;
			}
			setState(1154);
			contextBoundsList();
			setState(1156);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1155);
				match(WS);
				}
			}

			setState(1158);
			decls_impl_body();
			setState(1159);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
		enterRule(_localctx, 126, RULE_decls_impl_body);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1161);
			match(T__6);
			setState(1163);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,158,_ctx) ) {
			case 1:
				{
				setState(1162);
				match(WS);
				}
				break;
			}
			setState(1168);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,159,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1165);
					decls_definition();
					}
					} 
				}
				setState(1170);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,159,_ctx);
			}
			setState(1172);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1171);
				match(WS);
				}
			}

			setState(1174);
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
		public BlockContext block() {
			return getRuleContext(BlockContext.class,0);
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
		enterRule(_localctx, 128, RULE_expression);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1176);
			block();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class BlockContext extends ParserRuleContext {
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
		}
		public LogicalContext logical() {
			return getRuleContext(LogicalContext.class,0);
		}
		public BlockContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_block; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).enterBlock(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlixListener ) ((FlixListener)listener).exitBlock(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitBlock(this);
			else return visitor.visitChildren(this);
		}
	}

	public final BlockContext block() throws RecognitionException {
		BlockContext _localctx = new BlockContext(_ctx, getState());
		enterRule(_localctx, 130, RULE_block);
		int _la;
		try {
			setState(1191);
			switch (_input.LA(1)) {
			case T__6:
				enterOuterAlt(_localctx, 1);
				{
				{
				setState(1178);
				match(T__6);
				setState(1180);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1179);
					match(WS);
					}
				}

				setState(1182);
				expression();
				setState(1184);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1183);
					match(WS);
					}
				}

				setState(1186);
				match(T__7);
				setState(1188);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,163,_ctx) ) {
				case 1:
					{
					setState(1187);
					match(WS);
					}
					break;
				}
				}
				}
				break;
			case T__4:
			case T__26:
			case T__30:
			case T__32:
			case T__34:
			case T__36:
			case T__38:
			case T__39:
			case T__40:
			case T__41:
			case T__42:
			case T__43:
			case T__44:
			case T__53:
			case T__61:
			case T__62:
			case T__63:
			case T__64:
			case LowerIdent:
			case UpperIdent:
			case FNil:
			case Wild:
				enterOuterAlt(_localctx, 2);
				{
				setState(1190);
				logical();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
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
		public SpContext sp() {
			return getRuleContext(SpContext.class,0);
		}
		public Logical_opsContext logical_ops() {
			return getRuleContext(Logical_opsContext.class,0);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
			setState(1193);
			comparison();
			setState(1203);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,167,_ctx) ) {
			case 1:
				{
				setState(1195);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1194);
					match(WS);
					}
				}

				setState(1197);
				logical_ops();
				setState(1199);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1198);
					match(WS);
					}
				}

				setState(1201);
				comparison();
				}
				break;
			}
			setState(1205);
			sp();
			}
		}
		catch (RecognitionException re) {
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
			setState(1207);
			expression();
			setState(1218);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,170,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1209);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1208);
						match(WS);
						}
					}

					setState(1211);
					match(T__2);
					setState(1213);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1212);
						match(WS);
						}
					}

					setState(1215);
					expression();
					}
					} 
				}
				setState(1220);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,170,_ctx);
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
		public SpContext sp() {
			return getRuleContext(SpContext.class,0);
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
			setState(1221);
			additive();
			setState(1231);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,173,_ctx) ) {
			case 1:
				{
				setState(1223);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1222);
					match(WS);
					}
				}

				setState(1225);
				comparison_ops();
				setState(1227);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1226);
					match(WS);
					}
				}

				setState(1229);
				additive();
				}
				break;
			}
			setState(1233);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public SpContext sp() {
			return getRuleContext(SpContext.class,0);
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitAdditive(this);
			else return visitor.visitChildren(this);
		}
	}

	public final AdditiveContext additive() throws RecognitionException {
		AdditiveContext _localctx = new AdditiveContext(_ctx, getState());
		enterRule(_localctx, 138, RULE_additive);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1235);
			multiplicative();
			setState(1247);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,176,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1237);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1236);
						match(WS);
						}
					}

					setState(1239);
					addve_ops();
					setState(1241);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1240);
						match(WS);
						}
					}

					setState(1243);
					multiplicative();
					}
					} 
				}
				setState(1249);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,176,_ctx);
			}
			setState(1250);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public SpContext sp() {
			return getRuleContext(SpContext.class,0);
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitMultiplicative(this);
			else return visitor.visitChildren(this);
		}
	}

	public final MultiplicativeContext multiplicative() throws RecognitionException {
		MultiplicativeContext _localctx = new MultiplicativeContext(_ctx, getState());
		enterRule(_localctx, 140, RULE_multiplicative);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1252);
			infix();
			setState(1264);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,179,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1254);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1253);
						match(WS);
						}
					}

					setState(1256);
					multipve_ops();
					setState(1258);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1257);
						match(WS);
						}
					}

					setState(1260);
					infix();
					}
					} 
				}
				setState(1266);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,179,_ctx);
			}
			setState(1267);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public SpContext sp() {
			return getRuleContext(SpContext.class,0);
		}
		public QualifiedDefinitionNameContext qualifiedDefinitionName() {
			return getRuleContext(QualifiedDefinitionNameContext.class,0);
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
			setState(1269);
			extended();
			setState(1281);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,182,_ctx) ) {
			case 1:
				{
				setState(1271);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1270);
					match(WS);
					}
				}

				setState(1273);
				match(T__29);
				setState(1274);
				qualifiedDefinitionName();
				setState(1275);
				match(T__29);
				setState(1277);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1276);
					match(WS);
					}
				}

				setState(1279);
				extended();
				}
				break;
			}
			setState(1283);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public SpContext sp() {
			return getRuleContext(SpContext.class,0);
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
			setState(1285);
			unary();
			setState(1295);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,185,_ctx) ) {
			case 1:
				{
				setState(1287);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1286);
					match(WS);
					}
				}

				setState(1289);
				extbin_ops();
				setState(1291);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1290);
					match(WS);
					}
				}

				setState(1293);
				unary();
				}
				break;
			}
			setState(1297);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitUnary(this);
			else return visitor.visitChildren(this);
		}
	}

	public final UnaryContext unary() throws RecognitionException {
		UnaryContext _localctx = new UnaryContext(_ctx, getState());
		enterRule(_localctx, 146, RULE_unary);
		int _la;
		try {
			setState(1308);
			switch (_input.LA(1)) {
			case T__53:
			case T__61:
			case T__62:
			case T__63:
			case T__64:
				enterOuterAlt(_localctx, 1);
				{
				{
				setState(1299);
				sp();
				setState(1300);
				unary_ops();
				setState(1302);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1301);
					match(WS);
					}
				}

				setState(1304);
				unary();
				setState(1305);
				sp();
				}
				}
				break;
			case T__4:
			case T__26:
			case T__30:
			case T__32:
			case T__34:
			case T__36:
			case T__38:
			case T__39:
			case T__40:
			case T__41:
			case T__42:
			case T__43:
			case T__44:
			case LowerIdent:
			case UpperIdent:
			case FNil:
			case Wild:
				enterOuterAlt(_localctx, 2);
				{
				setState(1307);
				ascribe();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
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
		public SpContext sp() {
			return getRuleContext(SpContext.class,0);
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
			setState(1310);
			e_fList();
			setState(1319);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,190,_ctx) ) {
			case 1:
				{
				setState(1312);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1311);
					match(WS);
					}
				}

				setState(1314);
				match(T__3);
				setState(1316);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1315);
					match(WS);
					}
				}

				setState(1318);
				type();
				}
				break;
			}
			setState(1321);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public E_fVecContext e_fVec() {
			return getRuleContext(E_fVecContext.class,0);
		}
		public E_fSetContext e_fSet() {
			return getRuleContext(E_fSetContext.class,0);
		}
		public E_fMapContext e_fMap() {
			return getRuleContext(E_fMapContext.class,0);
		}
		public LiteralContext literal() {
			return getRuleContext(LiteralContext.class,0);
		}
		public ExistentialContext existential() {
			return getRuleContext(ExistentialContext.class,0);
		}
		public UniversalContext universal() {
			return getRuleContext(UniversalContext.class,0);
		}
		public E_qnameContext e_qname() {
			return getRuleContext(E_qnameContext.class,0);
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
			setState(1342);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,191,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1323);
				e_letMatch();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1324);
				e_ifThenElse();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1325);
				e_match();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(1326);
				e_switch();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(1327);
				e_tag();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(1328);
				e_lambda();
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(1329);
				e_tuple();
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(1330);
				e_fNil();
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(1331);
				e_fVec();
				}
				break;
			case 10:
				enterOuterAlt(_localctx, 10);
				{
				setState(1332);
				e_fSet();
				}
				break;
			case 11:
				enterOuterAlt(_localctx, 11);
				{
				setState(1333);
				e_fMap();
				}
				break;
			case 12:
				enterOuterAlt(_localctx, 12);
				{
				setState(1334);
				literal();
				}
				break;
			case 13:
				enterOuterAlt(_localctx, 13);
				{
				setState(1335);
				existential();
				}
				break;
			case 14:
				enterOuterAlt(_localctx, 14);
				{
				setState(1336);
				universal();
				}
				break;
			case 15:
				enterOuterAlt(_localctx, 15);
				{
				setState(1337);
				e_qname();
				}
				break;
			case 16:
				enterOuterAlt(_localctx, 16);
				{
				setState(1338);
				e_unaryLambda();
				}
				break;
			case 17:
				enterOuterAlt(_localctx, 17);
				{
				setState(1339);
				e_wild();
				}
				break;
			case 18:
				enterOuterAlt(_localctx, 18);
				{
				setState(1340);
				e_sname();
				}
				break;
			case 19:
				enterOuterAlt(_localctx, 19);
				{
				setState(1341);
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
			setState(1344);
			sp();
			setState(1345);
			match(T__26);
			setState(1346);
			match(WS);
			setState(1347);
			pattern();
			setState(1349);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1348);
				match(WS);
				}
			}

			setState(1351);
			match(T__22);
			setState(1353);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1352);
				match(WS);
				}
			}

			setState(1355);
			expression();
			setState(1357);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1356);
				match(WS);
				}
			}

			setState(1359);
			match(SC);
			setState(1361);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1360);
				match(WS);
				}
			}

			setState(1363);
			expression();
			setState(1364);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
			setState(1366);
			sp();
			setState(1367);
			match(T__30);
			setState(1369);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1368);
				match(WS);
				}
			}

			setState(1371);
			match(T__4);
			setState(1373);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1372);
				match(WS);
				}
			}

			setState(1375);
			expression();
			setState(1377);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1376);
				match(WS);
				}
			}

			setState(1379);
			match(T__5);
			setState(1381);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1380);
				match(WS);
				}
			}

			setState(1383);
			expression();
			setState(1384);
			match(WS);
			setState(1385);
			match(T__31);
			setState(1386);
			match(WS);
			setState(1387);
			expression();
			setState(1388);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
			setState(1390);
			sp();
			setState(1391);
			match(T__32);
			setState(1392);
			match(WS);
			setState(1393);
			expression();
			setState(1394);
			match(WS);
			setState(1395);
			match(T__33);
			setState(1396);
			match(WS);
			setState(1397);
			match(T__6);
			setState(1399);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1398);
				match(WS);
				}
			}

			setState(1401);
			match_rules();
			setState(1403);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1402);
				match(WS);
				}
			}

			setState(1405);
			match(T__7);
			setState(1406);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
			setState(1408);
			sp();
			setState(1409);
			match(T__34);
			setState(1410);
			match(WS);
			setState(1411);
			match(T__6);
			setState(1413);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1412);
				match(WS);
				}
			}

			setState(1415);
			switch_rules();
			setState(1417);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1416);
				match(WS);
				}
			}

			setState(1419);
			match(T__7);
			setState(1420);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public SpContext sp() {
			return getRuleContext(SpContext.class,0);
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
			setState(1422);
			e_primary();
			setState(1437);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,208,_ctx) ) {
			case 1:
				{
				setState(1424);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1423);
					match(WS);
					}
				}

				setState(1426);
				match(T__4);
				setState(1428);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,205,_ctx) ) {
				case 1:
					{
					setState(1427);
					match(WS);
					}
					break;
				}
				setState(1431);
				_la = _input.LA(1);
				if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__4) | (1L << T__6) | (1L << T__26) | (1L << T__30) | (1L << T__32) | (1L << T__34) | (1L << T__36) | (1L << T__38) | (1L << T__39) | (1L << T__40) | (1L << T__41) | (1L << T__42) | (1L << T__43) | (1L << T__44) | (1L << T__53) | (1L << T__61) | (1L << T__62))) != 0) || ((((_la - 64)) & ~0x3f) == 0 && ((1L << (_la - 64)) & ((1L << (T__63 - 64)) | (1L << (T__64 - 64)) | (1L << (LowerIdent - 64)) | (1L << (UpperIdent - 64)) | (1L << (FNil - 64)) | (1L << (Wild - 64)))) != 0)) {
					{
					setState(1430);
					expressions();
					}
				}

				setState(1434);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1433);
					match(WS);
					}
				}

				setState(1436);
				match(T__5);
				}
				break;
			}
			setState(1439);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		enterRule(_localctx, 162, RULE_e_sname);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1441);
			sp();
			setState(1442);
			variableName();
			setState(1443);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		enterRule(_localctx, 164, RULE_e_qname);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1445);
			sp();
			setState(1446);
			qualifiedDefinitionName();
			setState(1447);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public TagNameContext tagName() {
			return getRuleContext(TagNameContext.class,0);
		}
		public QualifiedTypeNameContext qualifiedTypeName() {
			return getRuleContext(QualifiedTypeNameContext.class,0);
		}
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_tag(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_tagContext e_tag() throws RecognitionException {
		E_tagContext _localctx = new E_tagContext(_ctx, getState());
		enterRule(_localctx, 166, RULE_e_tag);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1449);
			sp();
			setState(1453);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,209,_ctx) ) {
			case 1:
				{
				setState(1450);
				qualifiedTypeName();
				setState(1451);
				match(T__0);
				}
				break;
			}
			setState(1455);
			tagName();
			setState(1460);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,211,_ctx) ) {
			case 1:
				{
				setState(1457);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1456);
					match(WS);
					}
				}

				setState(1459);
				e_tuple();
				}
				break;
			}
			setState(1462);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_tuple(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_tupleContext e_tuple() throws RecognitionException {
		E_tupleContext _localctx = new E_tupleContext(_ctx, getState());
		enterRule(_localctx, 168, RULE_e_tuple);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1464);
			sp();
			setState(1465);
			match(T__4);
			setState(1467);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,212,_ctx) ) {
			case 1:
				{
				setState(1466);
				match(WS);
				}
				break;
			}
			setState(1470);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__4) | (1L << T__6) | (1L << T__26) | (1L << T__30) | (1L << T__32) | (1L << T__34) | (1L << T__36) | (1L << T__38) | (1L << T__39) | (1L << T__40) | (1L << T__41) | (1L << T__42) | (1L << T__43) | (1L << T__44) | (1L << T__53) | (1L << T__61) | (1L << T__62))) != 0) || ((((_la - 64)) & ~0x3f) == 0 && ((1L << (_la - 64)) & ((1L << (T__63 - 64)) | (1L << (T__64 - 64)) | (1L << (LowerIdent - 64)) | (1L << (UpperIdent - 64)) | (1L << (FNil - 64)) | (1L << (Wild - 64)))) != 0)) {
				{
				setState(1469);
				expressions();
				}
			}

			setState(1473);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1472);
				match(WS);
				}
			}

			setState(1475);
			match(T__5);
			setState(1476);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_keyValue(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_keyValueContext e_keyValue() throws RecognitionException {
		E_keyValueContext _localctx = new E_keyValueContext(_ctx, getState());
		enterRule(_localctx, 170, RULE_e_keyValue);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1478);
			expression();
			setState(1480);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1479);
				match(WS);
				}
			}

			setState(1482);
			match(T__35);
			setState(1484);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1483);
				match(WS);
				}
			}

			setState(1486);
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_keyValues(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_keyValuesContext e_keyValues() throws RecognitionException {
		E_keyValuesContext _localctx = new E_keyValuesContext(_ctx, getState());
		enterRule(_localctx, 172, RULE_e_keyValues);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1488);
			e_keyValue();
			setState(1499);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,219,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1490);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1489);
						match(WS);
						}
					}

					setState(1492);
					match(T__2);
					setState(1494);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1493);
						match(WS);
						}
					}

					setState(1496);
					e_keyValue();
					}
					} 
				}
				setState(1501);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,219,_ctx);
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		enterRule(_localctx, 174, RULE_e_userError);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1502);
			sp();
			setState(1503);
			match(T__36);
			setState(1504);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_wild(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_wildContext e_wild() throws RecognitionException {
		E_wildContext _localctx = new E_wildContext(_ctx, getState());
		enterRule(_localctx, 176, RULE_e_wild);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1506);
			sp();
			setState(1507);
			match(Wild);
			setState(1508);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_fNil(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_fNilContext e_fNil() throws RecognitionException {
		E_fNilContext _localctx = new E_fNilContext(_ctx, getState());
		enterRule(_localctx, 178, RULE_e_fNil);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1510);
			sp();
			setState(1511);
			match(FNil);
			setState(1512);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public SpContext sp() {
			return getRuleContext(SpContext.class,0);
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_fList(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_fListContext e_fList() throws RecognitionException {
		E_fListContext _localctx = new E_fListContext(_ctx, getState());
		enterRule(_localctx, 180, RULE_e_fList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1514);
			e_apply();
			setState(1523);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,222,_ctx) ) {
			case 1:
				{
				setState(1516);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1515);
					match(WS);
					}
				}

				setState(1518);
				match(T__37);
				setState(1520);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1519);
					match(WS);
					}
				}

				setState(1522);
				expression();
				}
				break;
			}
			setState(1525);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_fVec(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_fVecContext e_fVec() throws RecognitionException {
		E_fVecContext _localctx = new E_fVecContext(_ctx, getState());
		enterRule(_localctx, 182, RULE_e_fVec);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1527);
			sp();
			setState(1528);
			match(T__38);
			setState(1530);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,223,_ctx) ) {
			case 1:
				{
				setState(1529);
				match(WS);
				}
				break;
			}
			setState(1533);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__4) | (1L << T__6) | (1L << T__26) | (1L << T__30) | (1L << T__32) | (1L << T__34) | (1L << T__36) | (1L << T__38) | (1L << T__39) | (1L << T__40) | (1L << T__41) | (1L << T__42) | (1L << T__43) | (1L << T__44) | (1L << T__53) | (1L << T__61) | (1L << T__62))) != 0) || ((((_la - 64)) & ~0x3f) == 0 && ((1L << (_la - 64)) & ((1L << (T__63 - 64)) | (1L << (T__64 - 64)) | (1L << (LowerIdent - 64)) | (1L << (UpperIdent - 64)) | (1L << (FNil - 64)) | (1L << (Wild - 64)))) != 0)) {
				{
				setState(1532);
				expressions();
				}
			}

			setState(1536);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1535);
				match(WS);
				}
			}

			setState(1538);
			match(T__11);
			setState(1539);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_fSet(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_fSetContext e_fSet() throws RecognitionException {
		E_fSetContext _localctx = new E_fSetContext(_ctx, getState());
		enterRule(_localctx, 184, RULE_e_fSet);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1541);
			sp();
			setState(1542);
			match(T__39);
			setState(1544);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,226,_ctx) ) {
			case 1:
				{
				setState(1543);
				match(WS);
				}
				break;
			}
			setState(1547);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__4) | (1L << T__6) | (1L << T__26) | (1L << T__30) | (1L << T__32) | (1L << T__34) | (1L << T__36) | (1L << T__38) | (1L << T__39) | (1L << T__40) | (1L << T__41) | (1L << T__42) | (1L << T__43) | (1L << T__44) | (1L << T__53) | (1L << T__61) | (1L << T__62))) != 0) || ((((_la - 64)) & ~0x3f) == 0 && ((1L << (_la - 64)) & ((1L << (T__63 - 64)) | (1L << (T__64 - 64)) | (1L << (LowerIdent - 64)) | (1L << (UpperIdent - 64)) | (1L << (FNil - 64)) | (1L << (Wild - 64)))) != 0)) {
				{
				setState(1546);
				expressions();
				}
			}

			setState(1550);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1549);
				match(WS);
				}
			}

			setState(1552);
			match(T__7);
			setState(1553);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_fMap(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_fMapContext e_fMap() throws RecognitionException {
		E_fMapContext _localctx = new E_fMapContext(_ctx, getState());
		enterRule(_localctx, 186, RULE_e_fMap);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1555);
			sp();
			setState(1556);
			match(T__40);
			setState(1558);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,229,_ctx) ) {
			case 1:
				{
				setState(1557);
				match(WS);
				}
				break;
			}
			setState(1561);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__4) | (1L << T__6) | (1L << T__26) | (1L << T__30) | (1L << T__32) | (1L << T__34) | (1L << T__36) | (1L << T__38) | (1L << T__39) | (1L << T__40) | (1L << T__41) | (1L << T__42) | (1L << T__43) | (1L << T__44) | (1L << T__53) | (1L << T__61) | (1L << T__62))) != 0) || ((((_la - 64)) & ~0x3f) == 0 && ((1L << (_la - 64)) & ((1L << (T__63 - 64)) | (1L << (T__64 - 64)) | (1L << (LowerIdent - 64)) | (1L << (UpperIdent - 64)) | (1L << (FNil - 64)) | (1L << (Wild - 64)))) != 0)) {
				{
				setState(1560);
				e_keyValues();
				}
			}

			setState(1564);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1563);
				match(WS);
				}
			}

			setState(1566);
			match(T__7);
			setState(1567);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public VariableNameContext variableName() {
			return getRuleContext(VariableNameContext.class,0);
		}
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_unaryLambda(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_unaryLambdaContext e_unaryLambda() throws RecognitionException {
		E_unaryLambdaContext _localctx = new E_unaryLambdaContext(_ctx, getState());
		enterRule(_localctx, 188, RULE_e_unaryLambda);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1569);
			sp();
			setState(1570);
			variableName();
			setState(1572);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1571);
				match(WS);
				}
			}

			setState(1574);
			match(T__35);
			setState(1576);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1575);
				match(WS);
				}
			}

			setState(1578);
			expression();
			setState(1579);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public VariableNamesContext variableNames() {
			return getRuleContext(VariableNamesContext.class,0);
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_lambda(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_lambdaContext e_lambda() throws RecognitionException {
		E_lambdaContext _localctx = new E_lambdaContext(_ctx, getState());
		enterRule(_localctx, 190, RULE_e_lambda);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1581);
			sp();
			setState(1582);
			match(T__4);
			setState(1584);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1583);
				match(WS);
				}
			}

			setState(1586);
			variableNames();
			setState(1588);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1587);
				match(WS);
				}
			}

			setState(1590);
			match(T__5);
			setState(1592);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1591);
				match(WS);
				}
			}

			setState(1594);
			match(T__35);
			setState(1596);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1595);
				match(WS);
				}
			}

			setState(1598);
			expression();
			setState(1599);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public FormalparamsContext formalparams() {
			return getRuleContext(FormalparamsContext.class,0);
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitExistential(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ExistentialContext existential() throws RecognitionException {
		ExistentialContext _localctx = new ExistentialContext(_ctx, getState());
		enterRule(_localctx, 192, RULE_existential);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1601);
			sp();
			setState(1602);
			_la = _input.LA(1);
			if ( !(_la==T__41 || _la==T__42) ) {
			_errHandler.recoverInline(this);
			} else {
				consume();
			}
			setState(1604);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,238,_ctx) ) {
			case 1:
				{
				setState(1603);
				match(WS);
				}
				break;
			}
			setState(1606);
			formalparams();
			setState(1608);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1607);
				match(WS);
				}
			}

			setState(1610);
			match(T__0);
			setState(1612);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1611);
				match(WS);
				}
			}

			setState(1614);
			expression();
			setState(1615);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public FormalparamsContext formalparams() {
			return getRuleContext(FormalparamsContext.class,0);
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitUniversal(this);
			else return visitor.visitChildren(this);
		}
	}

	public final UniversalContext universal() throws RecognitionException {
		UniversalContext _localctx = new UniversalContext(_ctx, getState());
		enterRule(_localctx, 194, RULE_universal);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1617);
			sp();
			setState(1618);
			_la = _input.LA(1);
			if ( !(_la==T__43 || _la==T__44) ) {
			_errHandler.recoverInline(this);
			} else {
				consume();
			}
			setState(1620);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,241,_ctx) ) {
			case 1:
				{
				setState(1619);
				match(WS);
				}
				break;
			}
			setState(1622);
			formalparams();
			setState(1624);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1623);
				match(WS);
				}
			}

			setState(1626);
			match(T__0);
			setState(1628);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1627);
				match(WS);
				}
			}

			setState(1630);
			expression();
			setState(1631);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public SpContext sp() {
			return getRuleContext(SpContext.class,0);
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitPattern(this);
			else return visitor.visitChildren(this);
		}
	}

	public final PatternContext pattern() throws RecognitionException {
		PatternContext _localctx = new PatternContext(_ctx, getState());
		enterRule(_localctx, 196, RULE_pattern);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1633);
			simple();
			setState(1642);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,246,_ctx) ) {
			case 1:
				{
				setState(1635);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1634);
					match(WS);
					}
				}

				setState(1637);
				match(T__37);
				setState(1639);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1638);
					match(WS);
					}
				}

				setState(1641);
				pattern();
				}
				break;
			}
			setState(1644);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitPatterns(this);
			else return visitor.visitChildren(this);
		}
	}

	public final PatternsContext patterns() throws RecognitionException {
		PatternsContext _localctx = new PatternsContext(_ctx, getState());
		enterRule(_localctx, 198, RULE_patterns);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1646);
			pattern();
			setState(1657);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,249,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1648);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1647);
						match(WS);
						}
					}

					setState(1650);
					match(T__2);
					setState(1652);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1651);
						match(WS);
						}
					}

					setState(1654);
					pattern();
					}
					} 
				}
				setState(1659);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,249,_ctx);
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
		public LiteralContext literal() {
			return getRuleContext(LiteralContext.class,0);
		}
		public P_variableContext p_variable() {
			return getRuleContext(P_variableContext.class,0);
		}
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitSimple(this);
			else return visitor.visitChildren(this);
		}
	}

	public final SimpleContext simple() throws RecognitionException {
		SimpleContext _localctx = new SimpleContext(_ctx, getState());
		enterRule(_localctx, 200, RULE_simple);
		try {
			setState(1669);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,250,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1660);
				p_fNil();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1661);
				literal();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1662);
				p_variable();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(1663);
				match(Wild);
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(1664);
				p_tag();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(1665);
				p_tuple();
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(1666);
				p_fVec();
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(1667);
				p_fSet();
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(1668);
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitP_keyValue(this);
			else return visitor.visitChildren(this);
		}
	}

	public final P_keyValueContext p_keyValue() throws RecognitionException {
		P_keyValueContext _localctx = new P_keyValueContext(_ctx, getState());
		enterRule(_localctx, 202, RULE_p_keyValue);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1671);
			pattern();
			setState(1673);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1672);
				match(WS);
				}
			}

			setState(1675);
			match(T__35);
			setState(1677);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1676);
				match(WS);
				}
			}

			setState(1679);
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitP_keyValues(this);
			else return visitor.visitChildren(this);
		}
	}

	public final P_keyValuesContext p_keyValues() throws RecognitionException {
		P_keyValuesContext _localctx = new P_keyValuesContext(_ctx, getState());
		enterRule(_localctx, 204, RULE_p_keyValues);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1681);
			p_keyValue();
			setState(1692);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,255,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1683);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1682);
						match(WS);
						}
					}

					setState(1685);
					match(T__2);
					setState(1687);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1686);
						match(WS);
						}
					}

					setState(1689);
					p_keyValue();
					}
					} 
				}
				setState(1694);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,255,_ctx);
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public TagNameContext tagName() {
			return getRuleContext(TagNameContext.class,0);
		}
		public QualifiedTypeNameContext qualifiedTypeName() {
			return getRuleContext(QualifiedTypeNameContext.class,0);
		}
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitP_tag(this);
			else return visitor.visitChildren(this);
		}
	}

	public final P_tagContext p_tag() throws RecognitionException {
		P_tagContext _localctx = new P_tagContext(_ctx, getState());
		enterRule(_localctx, 206, RULE_p_tag);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1695);
			sp();
			setState(1699);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,256,_ctx) ) {
			case 1:
				{
				setState(1696);
				qualifiedTypeName();
				setState(1697);
				match(T__0);
				}
				break;
			}
			setState(1701);
			tagName();
			setState(1706);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,258,_ctx) ) {
			case 1:
				{
				setState(1703);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1702);
					match(WS);
					}
				}

				setState(1705);
				pattern();
				}
				break;
			}
			setState(1708);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitP_tuple(this);
			else return visitor.visitChildren(this);
		}
	}

	public final P_tupleContext p_tuple() throws RecognitionException {
		P_tupleContext _localctx = new P_tupleContext(_ctx, getState());
		enterRule(_localctx, 208, RULE_p_tuple);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1710);
			sp();
			setState(1711);
			match(T__4);
			setState(1713);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,259,_ctx) ) {
			case 1:
				{
				setState(1712);
				match(WS);
				}
				break;
			}
			setState(1716);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__4) | (1L << T__38) | (1L << T__39) | (1L << T__40))) != 0) || ((((_la - 99)) & ~0x3f) == 0 && ((1L << (_la - 99)) & ((1L << (UpperIdent - 99)) | (1L << (FNil - 99)) | (1L << (Wild - 99)))) != 0)) {
				{
				setState(1715);
				patterns();
				}
			}

			setState(1719);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1718);
				match(WS);
				}
			}

			setState(1721);
			match(T__5);
			setState(1722);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public TerminalNode Wild() { return getToken(FlixParser.Wild, 0); }
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
		enterRule(_localctx, 210, RULE_p_wild);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1724);
			sp();
			setState(1725);
			match(Wild);
			setState(1726);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitP_fNil(this);
			else return visitor.visitChildren(this);
		}
	}

	public final P_fNilContext p_fNil() throws RecognitionException {
		P_fNilContext _localctx = new P_fNilContext(_ctx, getState());
		enterRule(_localctx, 212, RULE_p_fNil);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1728);
			sp();
			setState(1729);
			match(FNil);
			setState(1730);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public IdentContext ident() {
			return getRuleContext(IdentContext.class,0);
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
		enterRule(_localctx, 214, RULE_p_variable);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1732);
			sp();
			setState(1733);
			ident();
			setState(1734);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitP_fVec(this);
			else return visitor.visitChildren(this);
		}
	}

	public final P_fVecContext p_fVec() throws RecognitionException {
		P_fVecContext _localctx = new P_fVecContext(_ctx, getState());
		enterRule(_localctx, 216, RULE_p_fVec);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1736);
			sp();
			setState(1737);
			match(T__38);
			setState(1739);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,262,_ctx) ) {
			case 1:
				{
				setState(1738);
				match(WS);
				}
				break;
			}
			setState(1742);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__4) | (1L << T__38) | (1L << T__39) | (1L << T__40))) != 0) || ((((_la - 99)) & ~0x3f) == 0 && ((1L << (_la - 99)) & ((1L << (UpperIdent - 99)) | (1L << (FNil - 99)) | (1L << (Wild - 99)))) != 0)) {
				{
				setState(1741);
				patterns();
				}
			}

			setState(1752);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,266,_ctx) ) {
			case 1:
				{
				setState(1745);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1744);
					match(WS);
					}
				}

				setState(1747);
				match(T__2);
				setState(1749);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1748);
					match(WS);
					}
				}

				setState(1751);
				match(T__45);
				}
				break;
			}
			setState(1755);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1754);
				match(WS);
				}
			}

			setState(1757);
			match(T__11);
			setState(1758);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitP_fSet(this);
			else return visitor.visitChildren(this);
		}
	}

	public final P_fSetContext p_fSet() throws RecognitionException {
		P_fSetContext _localctx = new P_fSetContext(_ctx, getState());
		enterRule(_localctx, 218, RULE_p_fSet);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1760);
			sp();
			setState(1761);
			match(T__39);
			setState(1763);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,268,_ctx) ) {
			case 1:
				{
				setState(1762);
				match(WS);
				}
				break;
			}
			setState(1766);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__4) | (1L << T__38) | (1L << T__39) | (1L << T__40))) != 0) || ((((_la - 99)) & ~0x3f) == 0 && ((1L << (_la - 99)) & ((1L << (UpperIdent - 99)) | (1L << (FNil - 99)) | (1L << (Wild - 99)))) != 0)) {
				{
				setState(1765);
				patterns();
				}
			}

			setState(1776);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,272,_ctx) ) {
			case 1:
				{
				setState(1769);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1768);
					match(WS);
					}
				}

				setState(1771);
				match(T__2);
				setState(1773);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1772);
					match(WS);
					}
				}

				setState(1775);
				match(T__45);
				}
				break;
			}
			setState(1779);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1778);
				match(WS);
				}
			}

			setState(1781);
			match(T__7);
			setState(1782);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitP_fMap(this);
			else return visitor.visitChildren(this);
		}
	}

	public final P_fMapContext p_fMap() throws RecognitionException {
		P_fMapContext _localctx = new P_fMapContext(_ctx, getState());
		enterRule(_localctx, 220, RULE_p_fMap);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1784);
			sp();
			setState(1785);
			match(T__40);
			setState(1787);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,274,_ctx) ) {
			case 1:
				{
				setState(1786);
				match(WS);
				}
				break;
			}
			setState(1790);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__4) | (1L << T__38) | (1L << T__39) | (1L << T__40))) != 0) || ((((_la - 99)) & ~0x3f) == 0 && ((1L << (_la - 99)) & ((1L << (UpperIdent - 99)) | (1L << (FNil - 99)) | (1L << (Wild - 99)))) != 0)) {
				{
				setState(1789);
				p_keyValues();
				}
			}

			setState(1800);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,278,_ctx) ) {
			case 1:
				{
				setState(1793);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1792);
					match(WS);
					}
				}

				setState(1795);
				match(T__2);
				setState(1797);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1796);
					match(WS);
					}
				}

				setState(1799);
				match(T__45);
				}
				break;
			}
			setState(1803);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1802);
				match(WS);
				}
			}

			setState(1805);
			match(T__7);
			setState(1806);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		enterRule(_localctx, 222, RULE_bools);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1808);
			sp();
			setState(1809);
			_la = _input.LA(1);
			if ( !(_la==T__46 || _la==T__47) ) {
			_errHandler.recoverInline(this);
			} else {
				consume();
			}
			setState(1810);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		enterRule(_localctx, 224, RULE_chars);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1812);
			sp();
			setState(1813);
			match(T__48);
			setState(1814);
			matchWildcard();
			setState(1815);
			match(T__48);
			setState(1816);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		enterRule(_localctx, 226, RULE_strs);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1818);
			sp();
			setState(1819);
			match(T__49);
			setState(1824);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__0) | (1L << T__1) | (1L << T__2) | (1L << T__3) | (1L << T__4) | (1L << T__5) | (1L << T__6) | (1L << T__7) | (1L << T__8) | (1L << T__9) | (1L << T__10) | (1L << T__11) | (1L << T__12) | (1L << T__13) | (1L << T__14) | (1L << T__15) | (1L << T__16) | (1L << T__17) | (1L << T__18) | (1L << T__19) | (1L << T__20) | (1L << T__21) | (1L << T__22) | (1L << T__23) | (1L << T__24) | (1L << T__25) | (1L << T__26) | (1L << T__27) | (1L << T__28) | (1L << T__29) | (1L << T__30) | (1L << T__31) | (1L << T__32) | (1L << T__33) | (1L << T__34) | (1L << T__35) | (1L << T__36) | (1L << T__37) | (1L << T__38) | (1L << T__39) | (1L << T__40) | (1L << T__41) | (1L << T__42) | (1L << T__43) | (1L << T__44) | (1L << T__45) | (1L << T__46) | (1L << T__47) | (1L << T__48) | (1L << T__50) | (1L << T__53) | (1L << T__54) | (1L << T__55) | (1L << T__56) | (1L << T__57) | (1L << T__58) | (1L << T__59) | (1L << T__60) | (1L << T__61) | (1L << T__62))) != 0) || ((((_la - 64)) & ~0x3f) == 0 && ((1L << (_la - 64)) & ((1L << (T__63 - 64)) | (1L << (T__64 - 64)) | (1L << (T__65 - 64)) | (1L << (T__66 - 64)) | (1L << (T__67 - 64)) | (1L << (T__68 - 64)) | (1L << (T__69 - 64)) | (1L << (T__70 - 64)) | (1L << (T__71 - 64)) | (1L << (T__72 - 64)) | (1L << (T__73 - 64)) | (1L << (T__74 - 64)) | (1L << (T__75 - 64)) | (1L << (T__76 - 64)) | (1L << (T__77 - 64)) | (1L << (T__78 - 64)) | (1L << (T__79 - 64)) | (1L << (T__80 - 64)) | (1L << (T__81 - 64)) | (1L << (T__82 - 64)) | (1L << (T__83 - 64)) | (1L << (T__84 - 64)) | (1L << (T__85 - 64)) | (1L << (T__86 - 64)) | (1L << (T__87 - 64)) | (1L << (T__88 - 64)) | (1L << (T__89 - 64)) | (1L << (T__90 - 64)) | (1L << (T__91 - 64)) | (1L << (T__92 - 64)) | (1L << (TripleSlashComment - 64)) | (1L << (WS - 64)) | (1L << (SC - 64)) | (1L << (Comment - 64)) | (1L << (LowerIdent - 64)) | (1L << (UpperIdent - 64)) | (1L << (FNil - 64)) | (1L << (Wild - 64)) | (1L << (Digits - 64)))) != 0)) {
				{
				setState(1822);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,280,_ctx) ) {
				case 1:
					{
					setState(1820);
					match(T__50);
					}
					break;
				case 2:
					{
					setState(1821);
					_la = _input.LA(1);
					if ( _la <= 0 || ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__49) | (1L << T__51) | (1L << T__52))) != 0)) ) {
					_errHandler.recoverInline(this);
					} else {
						consume();
					}
					}
					break;
				}
				}
				setState(1826);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(1827);
			match(T__49);
			setState(1828);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		enterRule(_localctx, 228, RULE_negative);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1830);
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

	public static class Float32Context extends ParserRuleContext {
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		enterRule(_localctx, 230, RULE_float32);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1832);
			sp();
			setState(1834);
			_la = _input.LA(1);
			if (_la==T__53) {
				{
				setState(1833);
				negative();
				}
			}

			setState(1836);
			match(Digits);
			setState(1837);
			match(T__0);
			setState(1838);
			match(Digits);
			setState(1839);
			match(T__54);
			setState(1840);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		enterRule(_localctx, 232, RULE_float64);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1842);
			sp();
			setState(1844);
			_la = _input.LA(1);
			if (_la==T__53) {
				{
				setState(1843);
				negative();
				}
			}

			setState(1846);
			match(Digits);
			setState(1847);
			match(T__0);
			setState(1848);
			match(Digits);
			setState(1849);
			match(T__55);
			setState(1850);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		enterRule(_localctx, 234, RULE_floatDefault);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1852);
			sp();
			setState(1854);
			_la = _input.LA(1);
			if (_la==T__53) {
				{
				setState(1853);
				negative();
				}
			}

			setState(1856);
			match(Digits);
			setState(1857);
			match(T__0);
			setState(1858);
			match(Digits);
			setState(1859);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		enterRule(_localctx, 236, RULE_floats);
		try {
			setState(1864);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,285,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1861);
				float32();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1862);
				float64();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1863);
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		enterRule(_localctx, 238, RULE_int8);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1866);
			sp();
			setState(1868);
			_la = _input.LA(1);
			if (_la==T__53) {
				{
				setState(1867);
				negative();
				}
			}

			setState(1870);
			match(Digits);
			setState(1871);
			match(T__56);
			setState(1872);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		enterRule(_localctx, 240, RULE_int16);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1874);
			sp();
			setState(1876);
			_la = _input.LA(1);
			if (_la==T__53) {
				{
				setState(1875);
				negative();
				}
			}

			setState(1878);
			match(Digits);
			setState(1879);
			match(T__57);
			setState(1880);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		enterRule(_localctx, 242, RULE_int32);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1882);
			sp();
			setState(1884);
			_la = _input.LA(1);
			if (_la==T__53) {
				{
				setState(1883);
				negative();
				}
			}

			setState(1886);
			match(Digits);
			setState(1887);
			match(T__58);
			setState(1888);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		enterRule(_localctx, 244, RULE_int64);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1890);
			sp();
			setState(1892);
			_la = _input.LA(1);
			if (_la==T__53) {
				{
				setState(1891);
				negative();
				}
			}

			setState(1894);
			match(Digits);
			setState(1895);
			match(T__59);
			setState(1896);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		enterRule(_localctx, 246, RULE_bigInt);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1898);
			sp();
			setState(1900);
			_la = _input.LA(1);
			if (_la==T__53) {
				{
				setState(1899);
				negative();
				}
			}

			setState(1902);
			match(Digits);
			setState(1903);
			match(T__60);
			setState(1904);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		enterRule(_localctx, 248, RULE_intDefault);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1906);
			sp();
			setState(1908);
			_la = _input.LA(1);
			if (_la==T__53) {
				{
				setState(1907);
				negative();
				}
			}

			setState(1910);
			match(Digits);
			setState(1911);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		enterRule(_localctx, 250, RULE_ints);
		try {
			setState(1919);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,292,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1913);
				int8();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1914);
				int16();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1915);
				int32();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(1916);
				int64();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(1917);
				bigInt();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(1918);
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		enterRule(_localctx, 252, RULE_literal);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1921);
			sp();
			setState(1927);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,293,_ctx) ) {
			case 1:
				{
				setState(1922);
				bools();
				}
				break;
			case 2:
				{
				setState(1923);
				chars();
				}
				break;
			case 3:
				{
				setState(1924);
				floats();
				}
				break;
			case 4:
				{
				setState(1925);
				ints();
				}
				break;
			case 5:
				{
				setState(1926);
				strs();
				}
				break;
			}
			setState(1929);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		enterRule(_localctx, 254, RULE_primary);
		try {
			setState(1936);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,294,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1931);
				arrow();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1932);
				tuple();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1933);
				apply();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(1934);
				var();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(1935);
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		enterRule(_localctx, 256, RULE_var);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1938);
			sp();
			setState(1939);
			variableName();
			setState(1940);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		enterRule(_localctx, 258, RULE_ref);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1942);
			sp();
			setState(1943);
			qualifiedTypeName();
			setState(1944);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitType(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TypeContext type() throws RecognitionException {
		TypeContext _localctx = new TypeContext(_ctx, getState());
		enterRule(_localctx, 260, RULE_type);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1946);
			sp();
			setState(1947);
			primary();
			setState(1956);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,297,_ctx) ) {
			case 1:
				{
				setState(1949);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1948);
					match(WS);
					}
				}

				setState(1951);
				match(T__35);
				setState(1953);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1952);
					match(WS);
					}
				}

				setState(1955);
				type();
				}
				break;
			}
			setState(1958);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
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
		enterRule(_localctx, 262, RULE_arrow);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1960);
			sp();
			setState(1961);
			match(T__4);
			setState(1963);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1962);
				match(WS);
				}
			}

			setState(1965);
			type();
			setState(1976);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,301,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1967);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1966);
						match(WS);
						}
					}

					setState(1969);
					match(T__2);
					setState(1971);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1970);
						match(WS);
						}
					}

					setState(1973);
					type();
					}
					} 
				}
				setState(1978);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,301,_ctx);
			}
			setState(1980);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1979);
				match(WS);
				}
			}

			setState(1982);
			match(T__5);
			setState(1984);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1983);
				match(WS);
				}
			}

			setState(1986);
			match(T__35);
			setState(1988);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1987);
				match(WS);
				}
			}

			setState(1990);
			type();
			setState(1991);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		enterRule(_localctx, 264, RULE_tuple_unit);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1993);
			sp();
			setState(1994);
			match(T__4);
			setState(1995);
			match(T__5);
			setState(1996);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitTuple_singleton(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Tuple_singletonContext tuple_singleton() throws RecognitionException {
		Tuple_singletonContext _localctx = new Tuple_singletonContext(_ctx, getState());
		enterRule(_localctx, 266, RULE_tuple_singleton);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1998);
			match(T__4);
			setState(2000);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1999);
				match(WS);
				}
			}

			setState(2002);
			type();
			setState(2004);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(2003);
				match(WS);
				}
			}

			setState(2006);
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
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
		enterRule(_localctx, 268, RULE_tuple_multi);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(2008);
			sp();
			setState(2009);
			match(T__4);
			setState(2011);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(2010);
				match(WS);
				}
			}

			setState(2013);
			type();
			setState(2022); 
			_errHandler.sync(this);
			_alt = 1;
			do {
				switch (_alt) {
				case 1:
					{
					{
					setState(2015);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(2014);
						match(WS);
						}
					}

					setState(2017);
					match(T__2);
					setState(2019);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(2018);
						match(WS);
						}
					}

					setState(2021);
					type();
					}
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(2024); 
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,310,_ctx);
			} while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER );
			setState(2027);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(2026);
				match(WS);
				}
			}

			setState(2029);
			match(T__5);
			setState(2030);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		enterRule(_localctx, 270, RULE_tuple);
		try {
			setState(2035);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,312,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(2032);
				tuple_unit();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(2033);
				tuple_singleton();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(2034);
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public RefContext ref() {
			return getRuleContext(RefContext.class,0);
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
		enterRule(_localctx, 272, RULE_apply);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(2037);
			sp();
			setState(2038);
			ref();
			setState(2040);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(2039);
				match(WS);
				}
			}

			setState(2042);
			match(T__10);
			setState(2044);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(2043);
				match(WS);
				}
			}

			setState(2046);
			type();
			setState(2057);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,317,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(2048);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(2047);
						match(WS);
						}
					}

					setState(2050);
					match(T__2);
					setState(2052);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(2051);
						match(WS);
						}
					}

					setState(2054);
					type();
					}
					} 
				}
				setState(2059);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,317,_ctx);
			}
			setState(2061);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(2060);
				match(WS);
				}
			}

			setState(2063);
			match(T__11);
			setState(2065);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,319,_ctx) ) {
			case 1:
				{
				setState(2064);
				match(WS);
				}
				break;
			}
			setState(2067);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		enterRule(_localctx, 274, RULE_unary_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(2069);
			_la = _input.LA(1);
			if ( !(((((_la - 54)) & ~0x3f) == 0 && ((1L << (_la - 54)) & ((1L << (T__53 - 54)) | (1L << (T__61 - 54)) | (1L << (T__62 - 54)) | (1L << (T__63 - 54)) | (1L << (T__64 - 54)))) != 0)) ) {
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
		enterRule(_localctx, 276, RULE_logical_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(2071);
			_la = _input.LA(1);
			if ( !(((((_la - 66)) & ~0x3f) == 0 && ((1L << (_la - 66)) & ((1L << (T__65 - 66)) | (1L << (T__66 - 66)) | (1L << (T__67 - 66)) | (1L << (T__68 - 66)) | (1L << (T__69 - 66)) | (1L << (T__70 - 66)) | (1L << (T__71 - 66)) | (1L << (T__72 - 66)) | (1L << (T__73 - 66)) | (1L << (T__74 - 66)) | (1L << (T__75 - 66)) | (1L << (T__76 - 66)) | (1L << (T__77 - 66)))) != 0)) ) {
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
		enterRule(_localctx, 278, RULE_comparison_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(2073);
			_la = _input.LA(1);
			if ( !(_la==T__12 || ((((_la - 79)) & ~0x3f) == 0 && ((1L << (_la - 79)) & ((1L << (T__78 - 79)) | (1L << (T__79 - 79)) | (1L << (T__80 - 79)) | (1L << (T__81 - 79)) | (1L << (T__82 - 79)) | (1L << (T__83 - 79)))) != 0)) ) {
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
		enterRule(_localctx, 280, RULE_multipve_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(2075);
			_la = _input.LA(1);
			if ( !(_la==T__1 || ((((_la - 85)) & ~0x3f) == 0 && ((1L << (_la - 85)) & ((1L << (T__84 - 85)) | (1L << (T__85 - 85)) | (1L << (T__86 - 85)))) != 0)) ) {
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
		enterRule(_localctx, 282, RULE_addve_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(2077);
			_la = _input.LA(1);
			if ( !(_la==T__53 || _la==T__61) ) {
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
		enterRule(_localctx, 284, RULE_extbin_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(2079);
			_la = _input.LA(1);
			if ( !(((((_la - 88)) & ~0x3f) == 0 && ((1L << (_la - 88)) & ((1L << (T__87 - 88)) | (1L << (T__88 - 88)) | (1L << (T__89 - 88)) | (1L << (T__90 - 88)) | (1L << (T__91 - 88)))) != 0)) ) {
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
		enterRule(_localctx, 286, RULE_predicate);
		try {
			setState(2087);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,320,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(2081);
				pred_true();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(2082);
				pred_false();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(2083);
				pred_filter();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(2084);
				pred_notequal();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(2085);
				pred_table();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(2086);
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitPredicates(this);
			else return visitor.visitChildren(this);
		}
	}

	public final PredicatesContext predicates() throws RecognitionException {
		PredicatesContext _localctx = new PredicatesContext(_ctx, getState());
		enterRule(_localctx, 288, RULE_predicates);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(2089);
			predicate();
			setState(2100);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,323,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(2091);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(2090);
						match(WS);
						}
					}

					setState(2093);
					match(T__2);
					setState(2095);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(2094);
						match(WS);
						}
					}

					setState(2097);
					predicate();
					}
					} 
				}
				setState(2102);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,323,_ctx);
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		enterRule(_localctx, 290, RULE_pred_true);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(2103);
			sp();
			setState(2104);
			match(T__46);
			setState(2105);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
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
		enterRule(_localctx, 292, RULE_pred_false);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(2107);
			sp();
			setState(2108);
			match(T__47);
			setState(2109);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public QualifiedDefinitionNameContext qualifiedDefinitionName() {
			return getRuleContext(QualifiedDefinitionNameContext.class,0);
		}
		public ExpressionsContext expressions() {
			return getRuleContext(ExpressionsContext.class,0);
		}
		public TerminalNode WS() { return getToken(FlixParser.WS, 0); }
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
		enterRule(_localctx, 294, RULE_pred_filter);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(2111);
			sp();
			setState(2112);
			qualifiedDefinitionName();
			setState(2114);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(2113);
				match(WS);
				}
			}

			setState(2116);
			match(T__4);
			setState(2117);
			expressions();
			setState(2118);
			match(T__5);
			setState(2119);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public QualifiedTableNameContext qualifiedTableName() {
			return getRuleContext(QualifiedTableNameContext.class,0);
		}
		public ExpressionsContext expressions() {
			return getRuleContext(ExpressionsContext.class,0);
		}
		public TerminalNode WS() { return getToken(FlixParser.WS, 0); }
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
		enterRule(_localctx, 296, RULE_pred_table);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(2121);
			sp();
			setState(2122);
			qualifiedTableName();
			setState(2124);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(2123);
				match(WS);
				}
			}

			setState(2126);
			match(T__4);
			setState(2127);
			expressions();
			setState(2128);
			match(T__5);
			setState(2129);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public List<VariableNameContext> variableName() {
			return getRuleContexts(VariableNameContext.class);
		}
		public VariableNameContext variableName(int i) {
			return getRuleContext(VariableNameContext.class,i);
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitPred_notequal(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Pred_notequalContext pred_notequal() throws RecognitionException {
		Pred_notequalContext _localctx = new Pred_notequalContext(_ctx, getState());
		enterRule(_localctx, 298, RULE_pred_notequal);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(2131);
			sp();
			setState(2132);
			variableName();
			setState(2134);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(2133);
				match(WS);
				}
			}

			setState(2136);
			match(T__82);
			setState(2138);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(2137);
				match(WS);
				}
			}

			setState(2140);
			variableName();
			setState(2141);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		public List<SpContext> sp() {
			return getRuleContexts(SpContext.class);
		}
		public SpContext sp(int i) {
			return getRuleContext(SpContext.class,i);
		}
		public VariableNameContext variableName() {
			return getRuleContext(VariableNameContext.class,0);
		}
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitPred_loop(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Pred_loopContext pred_loop() throws RecognitionException {
		Pred_loopContext _localctx = new Pred_loopContext(_ctx, getState());
		enterRule(_localctx, 300, RULE_pred_loop);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(2143);
			sp();
			setState(2144);
			variableName();
			setState(2146);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(2145);
				match(WS);
				}
			}

			setState(2148);
			match(T__92);
			setState(2150);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(2149);
				match(WS);
				}
			}

			setState(2152);
			expression();
			setState(2153);
			sp();
			}
		}
		catch (RecognitionException re) {
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
		"\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd\3h\u086e\4\2\t\2\4"+
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
		"\t\u0097\4\u0098\t\u0098\3\2\3\2\3\2\3\2\3\3\3\3\3\4\7\4\u0138\n\4\f\4"+
		"\16\4\u013b\13\4\3\4\7\4\u013e\n\4\f\4\16\4\u0141\13\4\3\4\5\4\u0144\n"+
		"\4\3\4\3\4\3\5\5\5\u0149\n\5\3\5\5\5\u014c\n\5\3\6\3\6\3\6\3\6\3\7\3\7"+
		"\3\7\3\7\7\7\u0156\n\7\f\7\16\7\u0159\13\7\3\7\3\7\3\b\3\b\3\b\3\b\5\b"+
		"\u0161\n\b\3\b\3\b\3\b\3\t\3\t\3\t\3\t\5\t\u016a\n\t\3\t\3\t\3\t\3\n\3"+
		"\n\3\13\3\13\3\f\3\f\3\r\3\r\3\16\3\16\3\17\3\17\3\20\3\20\3\21\3\21\3"+
		"\22\3\22\3\23\3\23\3\24\3\24\3\25\3\25\5\25\u0187\n\25\3\25\3\25\5\25"+
		"\u018b\n\25\3\25\3\25\3\26\3\26\3\26\3\26\5\26\u0193\n\26\3\26\3\26\3"+
		"\26\3\27\3\27\5\27\u019a\n\27\3\27\3\27\5\27\u019e\n\27\3\27\7\27\u01a1"+
		"\n\27\f\27\16\27\u01a4\13\27\3\30\3\30\5\30\u01a8\n\30\3\30\5\30\u01ab"+
		"\n\30\3\30\5\30\u01ae\n\30\3\30\5\30\u01b1\n\30\3\31\3\31\3\31\5\31\u01b6"+
		"\n\31\3\31\3\31\5\31\u01ba\n\31\3\31\3\31\3\31\3\32\3\32\5\32\u01c1\n"+
		"\32\3\32\3\32\5\32\u01c5\n\32\3\32\7\32\u01c8\n\32\f\32\16\32\u01cb\13"+
		"\32\3\33\3\33\5\33\u01cf\n\33\3\33\3\33\5\33\u01d3\n\33\3\33\3\33\5\33"+
		"\u01d7\n\33\3\33\7\33\u01da\n\33\f\33\16\33\u01dd\13\33\5\33\u01df\n\33"+
		"\3\33\5\33\u01e2\n\33\3\33\3\33\3\34\3\34\5\34\u01e8\n\34\3\34\3\34\5"+
		"\34\u01ec\n\34\3\34\7\34\u01ef\n\34\f\34\16\34\u01f2\13\34\3\35\3\35\5"+
		"\35\u01f6\n\35\3\35\3\35\5\35\u01fa\n\35\3\35\7\35\u01fd\n\35\f\35\16"+
		"\35\u0200\13\35\3\36\3\36\3\36\3\36\5\36\u0206\n\36\3\36\3\36\5\36\u020a"+
		"\n\36\3\36\3\36\5\36\u020e\n\36\3\37\3\37\5\37\u0212\n\37\3\37\7\37\u0215"+
		"\n\37\f\37\16\37\u0218\13\37\3 \3 \3 \3 \5 \u021e\n \3 \3 \5 \u0222\n"+
		" \3 \3 \5 \u0226\n \3!\3!\5!\u022a\n!\3!\7!\u022d\n!\f!\16!\u0230\13!"+
		"\3\"\3\"\3\"\5\"\u0235\n\"\3\"\3\"\5\"\u0239\n\"\3\"\5\"\u023c\n\"\3\""+
		"\3\"\3#\3#\5#\u0242\n#\3#\3#\5#\u0246\n#\3#\3#\5#\u024a\n#\3#\7#\u024d"+
		"\n#\f#\16#\u0250\13#\3#\3#\5#\u0254\n#\3$\3$\3$\5$\u0259\n$\3$\3$\5$\u025d"+
		"\n$\3$\7$\u0260\n$\f$\16$\u0263\13$\3$\3$\3%\3%\3%\3%\3%\3&\3&\5&\u026e"+
		"\n&\3&\3&\5&\u0272\n&\3&\7&\u0275\n&\f&\16&\u0278\13&\3\'\5\'\u027b\n"+
		"\'\3\'\3\'\5\'\u027f\n\'\3\'\3\'\5\'\u0283\n\'\5\'\u0285\n\'\3(\3(\3("+
		"\3(\3(\3)\3)\3)\7)\u028f\n)\f)\16)\u0292\13)\3*\5*\u0295\n*\3*\3*\3*\5"+
		"*\u029a\n*\3+\3+\3+\3+\3+\3+\3+\3+\3+\3,\3,\3,\3,\3,\3,\3,\3,\3,\3-\3"+
		"-\3-\3-\3-\3-\3-\3.\3.\3.\3.\3.\3.\3.\3.\3.\3.\3.\3.\3.\5.\u02c2\n.\3"+
		"/\5/\u02c5\n/\3/\3/\3/\3/\3/\5/\u02cc\n/\3/\3/\5/\u02d0\n/\3/\7/\u02d3"+
		"\n/\f/\16/\u02d6\13/\3/\5/\u02d9\n/\3/\3/\3/\3/\3\60\5\60\u02e0\n\60\3"+
		"\60\7\60\u02e3\n\60\f\60\16\60\u02e6\13\60\3\60\5\60\u02e9\n\60\3\60\3"+
		"\60\3\60\3\60\3\60\3\60\5\60\u02f1\n\60\3\60\3\60\5\60\u02f5\n\60\3\60"+
		"\3\60\5\60\u02f9\n\60\3\60\3\60\3\60\3\60\3\61\3\61\5\61\u0301\n\61\3"+
		"\61\3\61\5\61\u0305\n\61\3\61\7\61\u0308\n\61\f\61\16\61\u030b\13\61\3"+
		"\62\3\62\3\62\3\62\3\62\5\62\u0312\n\62\3\62\3\62\3\63\5\63\u0317\n\63"+
		"\3\63\7\63\u031a\n\63\f\63\16\63\u031d\13\63\3\63\5\63\u0320\n\63\3\63"+
		"\3\63\3\63\3\63\3\63\5\63\u0327\n\63\3\63\3\63\5\63\u032b\n\63\3\63\5"+
		"\63\u032e\n\63\3\63\5\63\u0331\n\63\3\63\3\63\3\63\3\63\3\64\5\64\u0338"+
		"\n\64\3\64\7\64\u033b\n\64\f\64\16\64\u033e\13\64\3\64\5\64\u0341\n\64"+
		"\3\64\3\64\3\64\3\64\3\64\5\64\u0348\n\64\3\64\3\64\5\64\u034c\n\64\3"+
		"\64\5\64\u034f\n\64\3\64\5\64\u0352\n\64\3\64\3\64\3\64\3\64\3\65\5\65"+
		"\u0359\n\65\3\65\3\65\3\65\3\65\3\65\5\65\u0360\n\65\3\65\3\65\5\65\u0364"+
		"\n\65\3\65\5\65\u0367\n\65\3\65\5\65\u036a\n\65\3\65\3\65\3\65\3\65\3"+
		"\66\5\66\u0371\n\66\3\66\7\66\u0374\n\66\f\66\16\66\u0377\13\66\3\66\5"+
		"\66\u037a\n\66\3\66\3\66\3\66\3\66\3\66\5\66\u0381\n\66\3\66\3\66\5\66"+
		"\u0385\n\66\3\66\3\66\5\66\u0389\n\66\3\66\3\66\3\66\3\66\3\67\5\67\u0390"+
		"\n\67\3\67\7\67\u0393\n\67\f\67\16\67\u0396\13\67\3\67\5\67\u0399\n\67"+
		"\3\67\3\67\3\67\5\67\u039e\n\67\3\67\3\67\3\67\3\67\5\67\u03a4\n\67\3"+
		"\67\3\67\5\67\u03a8\n\67\3\67\3\67\5\67\u03ac\n\67\3\67\3\67\3\67\3\67"+
		"\38\58\u03b3\n8\38\78\u03b6\n8\f8\168\u03b9\138\38\58\u03bc\n8\38\58\u03bf"+
		"\n8\38\58\u03c2\n8\38\38\38\38\38\58\u03c9\n8\38\38\38\58\u03ce\n8\38"+
		"\38\58\u03d2\n8\38\38\58\u03d6\n8\38\38\58\u03da\n8\38\38\38\38\39\59"+
		"\u03e1\n9\39\79\u03e4\n9\f9\169\u03e7\139\39\59\u03ea\n9\39\39\39\39\3"+
		"9\59\u03f1\n9\39\39\59\u03f5\n9\39\39\59\u03f9\n9\39\39\59\u03fd\n9\3"+
		"9\39\59\u0401\n9\39\39\59\u0405\n9\39\39\39\39\3:\5:\u040c\n:\3:\7:\u040f"+
		"\n:\f:\16:\u0412\13:\3:\5:\u0415\n:\3:\3:\3:\3:\3:\3:\5:\u041d\n:\3:\3"+
		":\5:\u0421\n:\3:\3:\3:\3;\3;\5;\u0428\n;\3;\3;\3;\5;\u042d\n;\3;\5;\u0430"+
		"\n;\3;\3;\3<\5<\u0435\n<\3<\3<\3<\5<\u043a\n<\3<\3<\3<\3=\5=\u0440\n="+
		"\3=\3=\3=\5=\u0445\n=\3=\3=\5=\u0449\n=\3=\3=\5=\u044d\n=\3=\3=\3=\3>"+
		"\3>\3?\5?\u0455\n?\3?\3?\3?\5?\u045a\n?\3?\3?\3?\5?\u045f\n?\3?\3?\5?"+
		"\u0463\n?\3?\3?\5?\u0467\n?\3?\3?\5?\u046b\n?\3?\3?\3?\3?\3@\5@\u0472"+
		"\n@\3@\7@\u0475\n@\f@\16@\u0478\13@\3@\5@\u047b\n@\3@\3@\3@\3@\3@\3@\5"+
		"@\u0483\n@\3@\3@\5@\u0487\n@\3@\3@\3@\3A\3A\5A\u048e\nA\3A\7A\u0491\n"+
		"A\fA\16A\u0494\13A\3A\5A\u0497\nA\3A\3A\3B\3B\3C\3C\5C\u049f\nC\3C\3C"+
		"\5C\u04a3\nC\3C\3C\5C\u04a7\nC\3C\5C\u04aa\nC\3D\3D\5D\u04ae\nD\3D\3D"+
		"\5D\u04b2\nD\3D\3D\5D\u04b6\nD\3D\3D\3E\3E\5E\u04bc\nE\3E\3E\5E\u04c0"+
		"\nE\3E\7E\u04c3\nE\fE\16E\u04c6\13E\3F\3F\5F\u04ca\nF\3F\3F\5F\u04ce\n"+
		"F\3F\3F\5F\u04d2\nF\3F\3F\3G\3G\5G\u04d8\nG\3G\3G\5G\u04dc\nG\3G\3G\7"+
		"G\u04e0\nG\fG\16G\u04e3\13G\3G\3G\3H\3H\5H\u04e9\nH\3H\3H\5H\u04ed\nH"+
		"\3H\3H\7H\u04f1\nH\fH\16H\u04f4\13H\3H\3H\3I\3I\5I\u04fa\nI\3I\3I\3I\3"+
		"I\5I\u0500\nI\3I\3I\5I\u0504\nI\3I\3I\3J\3J\5J\u050a\nJ\3J\3J\5J\u050e"+
		"\nJ\3J\3J\5J\u0512\nJ\3J\3J\3K\3K\3K\5K\u0519\nK\3K\3K\3K\3K\5K\u051f"+
		"\nK\3L\3L\5L\u0523\nL\3L\3L\5L\u0527\nL\3L\5L\u052a\nL\3L\3L\3M\3M\3M"+
		"\3M\3M\3M\3M\3M\3M\3M\3M\3M\3M\3M\3M\3M\3M\3M\3M\5M\u0541\nM\3N\3N\3N"+
		"\3N\3N\5N\u0548\nN\3N\3N\5N\u054c\nN\3N\3N\5N\u0550\nN\3N\3N\5N\u0554"+
		"\nN\3N\3N\3N\3O\3O\3O\5O\u055c\nO\3O\3O\5O\u0560\nO\3O\3O\5O\u0564\nO"+
		"\3O\3O\5O\u0568\nO\3O\3O\3O\3O\3O\3O\3O\3P\3P\3P\3P\3P\3P\3P\3P\3P\5P"+
		"\u057a\nP\3P\3P\5P\u057e\nP\3P\3P\3P\3Q\3Q\3Q\3Q\3Q\5Q\u0588\nQ\3Q\3Q"+
		"\5Q\u058c\nQ\3Q\3Q\3Q\3R\3R\5R\u0593\nR\3R\3R\5R\u0597\nR\3R\5R\u059a"+
		"\nR\3R\5R\u059d\nR\3R\5R\u05a0\nR\3R\3R\3S\3S\3S\3S\3T\3T\3T\3T\3U\3U"+
		"\3U\3U\5U\u05b0\nU\3U\3U\5U\u05b4\nU\3U\5U\u05b7\nU\3U\3U\3V\3V\3V\5V"+
		"\u05be\nV\3V\5V\u05c1\nV\3V\5V\u05c4\nV\3V\3V\3V\3W\3W\5W\u05cb\nW\3W"+
		"\3W\5W\u05cf\nW\3W\3W\3X\3X\5X\u05d5\nX\3X\3X\5X\u05d9\nX\3X\7X\u05dc"+
		"\nX\fX\16X\u05df\13X\3Y\3Y\3Y\3Y\3Z\3Z\3Z\3Z\3[\3[\3[\3[\3\\\3\\\5\\\u05ef"+
		"\n\\\3\\\3\\\5\\\u05f3\n\\\3\\\5\\\u05f6\n\\\3\\\3\\\3]\3]\3]\5]\u05fd"+
		"\n]\3]\5]\u0600\n]\3]\5]\u0603\n]\3]\3]\3]\3^\3^\3^\5^\u060b\n^\3^\5^"+
		"\u060e\n^\3^\5^\u0611\n^\3^\3^\3^\3_\3_\3_\5_\u0619\n_\3_\5_\u061c\n_"+
		"\3_\5_\u061f\n_\3_\3_\3_\3`\3`\3`\5`\u0627\n`\3`\3`\5`\u062b\n`\3`\3`"+
		"\3`\3a\3a\3a\5a\u0633\na\3a\3a\5a\u0637\na\3a\3a\5a\u063b\na\3a\3a\5a"+
		"\u063f\na\3a\3a\3a\3b\3b\3b\5b\u0647\nb\3b\3b\5b\u064b\nb\3b\3b\5b\u064f"+
		"\nb\3b\3b\3b\3c\3c\3c\5c\u0657\nc\3c\3c\5c\u065b\nc\3c\3c\5c\u065f\nc"+
		"\3c\3c\3c\3d\3d\5d\u0666\nd\3d\3d\5d\u066a\nd\3d\5d\u066d\nd\3d\3d\3e"+
		"\3e\5e\u0673\ne\3e\3e\5e\u0677\ne\3e\7e\u067a\ne\fe\16e\u067d\13e\3f\3"+
		"f\3f\3f\3f\3f\3f\3f\3f\5f\u0688\nf\3g\3g\5g\u068c\ng\3g\3g\5g\u0690\n"+
		"g\3g\3g\3h\3h\5h\u0696\nh\3h\3h\5h\u069a\nh\3h\7h\u069d\nh\fh\16h\u06a0"+
		"\13h\3i\3i\3i\3i\5i\u06a6\ni\3i\3i\5i\u06aa\ni\3i\5i\u06ad\ni\3i\3i\3"+
		"j\3j\3j\5j\u06b4\nj\3j\5j\u06b7\nj\3j\5j\u06ba\nj\3j\3j\3j\3k\3k\3k\3"+
		"k\3l\3l\3l\3l\3m\3m\3m\3m\3n\3n\3n\5n\u06ce\nn\3n\5n\u06d1\nn\3n\5n\u06d4"+
		"\nn\3n\3n\5n\u06d8\nn\3n\5n\u06db\nn\3n\5n\u06de\nn\3n\3n\3n\3o\3o\3o"+
		"\5o\u06e6\no\3o\5o\u06e9\no\3o\5o\u06ec\no\3o\3o\5o\u06f0\no\3o\5o\u06f3"+
		"\no\3o\5o\u06f6\no\3o\3o\3o\3p\3p\3p\5p\u06fe\np\3p\5p\u0701\np\3p\5p"+
		"\u0704\np\3p\3p\5p\u0708\np\3p\5p\u070b\np\3p\5p\u070e\np\3p\3p\3p\3q"+
		"\3q\3q\3q\3r\3r\3r\3r\3r\3r\3s\3s\3s\3s\7s\u0721\ns\fs\16s\u0724\13s\3"+
		"s\3s\3s\3t\3t\3u\3u\5u\u072d\nu\3u\3u\3u\3u\3u\3u\3v\3v\5v\u0737\nv\3"+
		"v\3v\3v\3v\3v\3v\3w\3w\5w\u0741\nw\3w\3w\3w\3w\3w\3x\3x\3x\5x\u074b\n"+
		"x\3y\3y\5y\u074f\ny\3y\3y\3y\3y\3z\3z\5z\u0757\nz\3z\3z\3z\3z\3{\3{\5"+
		"{\u075f\n{\3{\3{\3{\3{\3|\3|\5|\u0767\n|\3|\3|\3|\3|\3}\3}\5}\u076f\n"+
		"}\3}\3}\3}\3}\3~\3~\5~\u0777\n~\3~\3~\3~\3\177\3\177\3\177\3\177\3\177"+
		"\3\177\5\177\u0782\n\177\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080"+
		"\5\u0080\u078a\n\u0080\3\u0080\3\u0080\3\u0081\3\u0081\3\u0081\3\u0081"+
		"\3\u0081\5\u0081\u0793\n\u0081\3\u0082\3\u0082\3\u0082\3\u0082\3\u0083"+
		"\3\u0083\3\u0083\3\u0083\3\u0084\3\u0084\3\u0084\5\u0084\u07a0\n\u0084"+
		"\3\u0084\3\u0084\5\u0084\u07a4\n\u0084\3\u0084\5\u0084\u07a7\n\u0084\3"+
		"\u0084\3\u0084\3\u0085\3\u0085\3\u0085\5\u0085\u07ae\n\u0085\3\u0085\3"+
		"\u0085\5\u0085\u07b2\n\u0085\3\u0085\3\u0085\5\u0085\u07b6\n\u0085\3\u0085"+
		"\7\u0085\u07b9\n\u0085\f\u0085\16\u0085\u07bc\13\u0085\3\u0085\5\u0085"+
		"\u07bf\n\u0085\3\u0085\3\u0085\5\u0085\u07c3\n\u0085\3\u0085\3\u0085\5"+
		"\u0085\u07c7\n\u0085\3\u0085\3\u0085\3\u0085\3\u0086\3\u0086\3\u0086\3"+
		"\u0086\3\u0086\3\u0087\3\u0087\5\u0087\u07d3\n\u0087\3\u0087\3\u0087\5"+
		"\u0087\u07d7\n\u0087\3\u0087\3\u0087\3\u0088\3\u0088\3\u0088\5\u0088\u07de"+
		"\n\u0088\3\u0088\3\u0088\5\u0088\u07e2\n\u0088\3\u0088\3\u0088\5\u0088"+
		"\u07e6\n\u0088\3\u0088\6\u0088\u07e9\n\u0088\r\u0088\16\u0088\u07ea\3"+
		"\u0088\5\u0088\u07ee\n\u0088\3\u0088\3\u0088\3\u0088\3\u0089\3\u0089\3"+
		"\u0089\5\u0089\u07f6\n\u0089\3\u008a\3\u008a\3\u008a\5\u008a\u07fb\n\u008a"+
		"\3\u008a\3\u008a\5\u008a\u07ff\n\u008a\3\u008a\3\u008a\5\u008a\u0803\n"+
		"\u008a\3\u008a\3\u008a\5\u008a\u0807\n\u008a\3\u008a\7\u008a\u080a\n\u008a"+
		"\f\u008a\16\u008a\u080d\13\u008a\3\u008a\5\u008a\u0810\n\u008a\3\u008a"+
		"\3\u008a\5\u008a\u0814\n\u008a\3\u008a\3\u008a\3\u008b\3\u008b\3\u008c"+
		"\3\u008c\3\u008d\3\u008d\3\u008e\3\u008e\3\u008f\3\u008f\3\u0090\3\u0090"+
		"\3\u0091\3\u0091\3\u0091\3\u0091\3\u0091\3\u0091\5\u0091\u082a\n\u0091"+
		"\3\u0092\3\u0092\5\u0092\u082e\n\u0092\3\u0092\3\u0092\5\u0092\u0832\n"+
		"\u0092\3\u0092\7\u0092\u0835\n\u0092\f\u0092\16\u0092\u0838\13\u0092\3"+
		"\u0093\3\u0093\3\u0093\3\u0093\3\u0094\3\u0094\3\u0094\3\u0094\3\u0095"+
		"\3\u0095\3\u0095\5\u0095\u0845\n\u0095\3\u0095\3\u0095\3\u0095\3\u0095"+
		"\3\u0095\3\u0096\3\u0096\3\u0096\5\u0096\u084f\n\u0096\3\u0096\3\u0096"+
		"\3\u0096\3\u0096\3\u0096\3\u0097\3\u0097\3\u0097\5\u0097\u0859\n\u0097"+
		"\3\u0097\3\u0097\5\u0097\u085d\n\u0097\3\u0097\3\u0097\3\u0097\3\u0098"+
		"\3\u0098\3\u0098\5\u0098\u0865\n\u0098\3\u0098\3\u0098\5\u0098\u0869\n"+
		"\u0098\3\u0098\3\u0098\3\u0098\3\u0098\2\2\u0099\2\4\6\b\n\f\16\20\22"+
		"\24\26\30\32\34\36 \"$&(*,.\60\62\64\668:<>@BDFHJLNPRTVXZ\\^`bdfhjlnp"+
		"rtvxz|~\u0080\u0082\u0084\u0086\u0088\u008a\u008c\u008e\u0090\u0092\u0094"+
		"\u0096\u0098\u009a\u009c\u009e\u00a0\u00a2\u00a4\u00a6\u00a8\u00aa\u00ac"+
		"\u00ae\u00b0\u00b2\u00b4\u00b6\u00b8\u00ba\u00bc\u00be\u00c0\u00c2\u00c4"+
		"\u00c6\u00c8\u00ca\u00cc\u00ce\u00d0\u00d2\u00d4\u00d6\u00d8\u00da\u00dc"+
		"\u00de\u00e0\u00e2\u00e4\u00e6\u00e8\u00ea\u00ec\u00ee\u00f0\u00f2\u00f4"+
		"\u00f6\u00f8\u00fa\u00fc\u00fe\u0100\u0102\u0104\u0106\u0108\u010a\u010c"+
		"\u010e\u0110\u0112\u0114\u0116\u0118\u011a\u011c\u011e\u0120\u0122\u0124"+
		"\u0126\u0128\u012a\u012c\u012e\2\r\3\2de\3\2,-\3\2./\3\2\61\62\4\2\64"+
		"\64\66\67\4\288@C\3\2DP\4\2\17\17QV\4\2\4\4WY\4\288@@\3\2Z^\u0955\2\u0130"+
		"\3\2\2\2\4\u0134\3\2\2\2\6\u0139\3\2\2\2\b\u014b\3\2\2\2\n\u014d\3\2\2"+
		"\2\f\u0151\3\2\2\2\16\u015c\3\2\2\2\20\u0165\3\2\2\2\22\u016e\3\2\2\2"+
		"\24\u0170\3\2\2\2\26\u0172\3\2\2\2\30\u0174\3\2\2\2\32\u0176\3\2\2\2\34"+
		"\u0178\3\2\2\2\36\u017a\3\2\2\2 \u017c\3\2\2\2\"\u017e\3\2\2\2$\u0180"+
		"\3\2\2\2&\u0182\3\2\2\2(\u0184\3\2\2\2*\u018e\3\2\2\2,\u0197\3\2\2\2."+
		"\u01b0\3\2\2\2\60\u01b2\3\2\2\2\62\u01be\3\2\2\2\64\u01cc\3\2\2\2\66\u01e5"+
		"\3\2\2\28\u01f3\3\2\2\2:\u0201\3\2\2\2<\u020f\3\2\2\2>\u0219\3\2\2\2@"+
		"\u0227\3\2\2\2B\u0231\3\2\2\2D\u0253\3\2\2\2F\u0255\3\2\2\2H\u0266\3\2"+
		"\2\2J\u026b\3\2\2\2L\u0284\3\2\2\2N\u0286\3\2\2\2P\u028b\3\2\2\2R\u0294"+
		"\3\2\2\2T\u029b\3\2\2\2V\u02a4\3\2\2\2X\u02ad\3\2\2\2Z\u02c1\3\2\2\2\\"+
		"\u02c4\3\2\2\2^\u02e4\3\2\2\2`\u02fe\3\2\2\2b\u030c\3\2\2\2d\u031b\3\2"+
		"\2\2f\u033c\3\2\2\2h\u0358\3\2\2\2j\u0375\3\2\2\2l\u0394\3\2\2\2n\u03b7"+
		"\3\2\2\2p\u03e5\3\2\2\2r\u0410\3\2\2\2t\u0425\3\2\2\2v\u0434\3\2\2\2x"+
		"\u043f\3\2\2\2z\u0451\3\2\2\2|\u0454\3\2\2\2~\u0476\3\2\2\2\u0080\u048b"+
		"\3\2\2\2\u0082\u049a\3\2\2\2\u0084\u04a9\3\2\2\2\u0086\u04ab\3\2\2\2\u0088"+
		"\u04b9\3\2\2\2\u008a\u04c7\3\2\2\2\u008c\u04d5\3\2\2\2\u008e\u04e6\3\2"+
		"\2\2\u0090\u04f7\3\2\2\2\u0092\u0507\3\2\2\2\u0094\u051e\3\2\2\2\u0096"+
		"\u0520\3\2\2\2\u0098\u0540\3\2\2\2\u009a\u0542\3\2\2\2\u009c\u0558\3\2"+
		"\2\2\u009e\u0570\3\2\2\2\u00a0\u0582\3\2\2\2\u00a2\u0590\3\2\2\2\u00a4"+
		"\u05a3\3\2\2\2\u00a6\u05a7\3\2\2\2\u00a8\u05ab\3\2\2\2\u00aa\u05ba\3\2"+
		"\2\2\u00ac\u05c8\3\2\2\2\u00ae\u05d2\3\2\2\2\u00b0\u05e0\3\2\2\2\u00b2"+
		"\u05e4\3\2\2\2\u00b4\u05e8\3\2\2\2\u00b6\u05ec\3\2\2\2\u00b8\u05f9\3\2"+
		"\2\2\u00ba\u0607\3\2\2\2\u00bc\u0615\3\2\2\2\u00be\u0623\3\2\2\2\u00c0"+
		"\u062f\3\2\2\2\u00c2\u0643\3\2\2\2\u00c4\u0653\3\2\2\2\u00c6\u0663\3\2"+
		"\2\2\u00c8\u0670\3\2\2\2\u00ca\u0687\3\2\2\2\u00cc\u0689\3\2\2\2\u00ce"+
		"\u0693\3\2\2\2\u00d0\u06a1\3\2\2\2\u00d2\u06b0\3\2\2\2\u00d4\u06be\3\2"+
		"\2\2\u00d6\u06c2\3\2\2\2\u00d8\u06c6\3\2\2\2\u00da\u06ca\3\2\2\2\u00dc"+
		"\u06e2\3\2\2\2\u00de\u06fa\3\2\2\2\u00e0\u0712\3\2\2\2\u00e2\u0716\3\2"+
		"\2\2\u00e4\u071c\3\2\2\2\u00e6\u0728\3\2\2\2\u00e8\u072a\3\2\2\2\u00ea"+
		"\u0734\3\2\2\2\u00ec\u073e\3\2\2\2\u00ee\u074a\3\2\2\2\u00f0\u074c\3\2"+
		"\2\2\u00f2\u0754\3\2\2\2\u00f4\u075c\3\2\2\2\u00f6\u0764\3\2\2\2\u00f8"+
		"\u076c\3\2\2\2\u00fa\u0774\3\2\2\2\u00fc\u0781\3\2\2\2\u00fe\u0783\3\2"+
		"\2\2\u0100\u0792\3\2\2\2\u0102\u0794\3\2\2\2\u0104\u0798\3\2\2\2\u0106"+
		"\u079c\3\2\2\2\u0108\u07aa\3\2\2\2\u010a\u07cb\3\2\2\2\u010c\u07d0\3\2"+
		"\2\2\u010e\u07da\3\2\2\2\u0110\u07f5\3\2\2\2\u0112\u07f7\3\2\2\2\u0114"+
		"\u0817\3\2\2\2\u0116\u0819\3\2\2\2\u0118\u081b\3\2\2\2\u011a\u081d\3\2"+
		"\2\2\u011c\u081f\3\2\2\2\u011e\u0821\3\2\2\2\u0120\u0829\3\2\2\2\u0122"+
		"\u082b\3\2\2\2\u0124\u0839\3\2\2\2\u0126\u083d\3\2\2\2\u0128\u0841\3\2"+
		"\2\2\u012a\u084b\3\2\2\2\u012c\u0855\3\2\2\2\u012e\u0861\3\2\2\2\u0130"+
		"\u0131\5\4\3\2\u0131\u0132\7`\2\2\u0132\u0133\5\4\3\2\u0133\3\3\2\2\2"+
		"\u0134\u0135\3\2\2\2\u0135\5\3\2\2\2\u0136\u0138\5R*\2\u0137\u0136\3\2"+
		"\2\2\u0138\u013b\3\2\2\2\u0139\u0137\3\2\2\2\u0139\u013a\3\2\2\2\u013a"+
		"\u013f\3\2\2\2\u013b\u0139\3\2\2\2\u013c\u013e\5Z.\2\u013d\u013c\3\2\2"+
		"\2\u013e\u0141\3\2\2\2\u013f\u013d\3\2\2\2\u013f\u0140\3\2\2\2\u0140\u0143"+
		"\3\2\2\2\u0141\u013f\3\2\2\2\u0142\u0144\7a\2\2\u0143\u0142\3\2\2\2\u0143"+
		"\u0144\3\2\2\2\u0144\u0145\3\2\2\2\u0145\u0146\7\2\2\3\u0146\7\3\2\2\2"+
		"\u0147\u0149\7a\2\2\u0148\u0147\3\2\2\2\u0148\u0149\3\2\2\2\u0149\u014a"+
		"\3\2\2\2\u014a\u014c\7b\2\2\u014b\u0148\3\2\2\2\u014b\u014c\3\2\2\2\u014c"+
		"\t\3\2\2\2\u014d\u014e\5\4\3\2\u014e\u014f\t\2\2\2\u014f\u0150\5\4\3\2"+
		"\u0150\13\3\2\2\2\u0151\u0152\5\4\3\2\u0152\u0157\5\n\6\2\u0153\u0154"+
		"\7\3\2\2\u0154\u0156\5\n\6\2\u0155\u0153\3\2\2\2\u0156\u0159\3\2\2\2\u0157"+
		"\u0155\3\2\2\2\u0157\u0158\3\2\2\2\u0158\u015a\3\2\2\2\u0159\u0157\3\2"+
		"\2\2\u015a\u015b\5\4\3\2\u015b\r\3\2\2\2\u015c\u0160\5\4\3\2\u015d\u015e"+
		"\5\f\7\2\u015e\u015f\7\4\2\2\u015f\u0161\3\2\2\2\u0160\u015d\3\2\2\2\u0160"+
		"\u0161\3\2\2\2\u0161\u0162\3\2\2\2\u0162\u0163\7d\2\2\u0163\u0164\5\4"+
		"\3\2\u0164\17\3\2\2\2\u0165\u0169\5\4\3\2\u0166\u0167\5\f\7\2\u0167\u0168"+
		"\7\4\2\2\u0168\u016a\3\2\2\2\u0169\u0166\3\2\2\2\u0169\u016a\3\2\2\2\u016a"+
		"\u016b\3\2\2\2\u016b\u016c\7e\2\2\u016c\u016d\5\4\3\2\u016d\21\3\2\2\2"+
		"\u016e\u016f\7d\2\2\u016f\23\3\2\2\2\u0170\u0171\7d\2\2\u0171\25\3\2\2"+
		"\2\u0172\u0173\7e\2\2\u0173\27\3\2\2\2\u0174\u0175\7d\2\2\u0175\31\3\2"+
		"\2\2\u0176\u0177\5\16\b\2\u0177\33\3\2\2\2\u0178\u0179\7e\2\2\u0179\35"+
		"\3\2\2\2\u017a\u017b\5\20\t\2\u017b\37\3\2\2\2\u017c\u017d\7e\2\2\u017d"+
		"!\3\2\2\2\u017e\u017f\7e\2\2\u017f#\3\2\2\2\u0180\u0181\5\20\t\2\u0181"+
		"%\3\2\2\2\u0182\u0183\7d\2\2\u0183\'\3\2\2\2\u0184\u0186\5&\24\2\u0185"+
		"\u0187\7a\2\2\u0186\u0185\3\2\2\2\u0186\u0187\3\2\2\2\u0187\u0188\3\2"+
		"\2\2\u0188\u018a\7\5\2\2\u0189\u018b\7a\2\2\u018a\u0189\3\2\2\2\u018a"+
		"\u018b\3\2\2\2\u018b\u018c\3\2\2\2\u018c\u018d\5&\24\2\u018d)\3\2\2\2"+
		"\u018e\u018f\5\4\3\2\u018f\u0190\5&\24\2\u0190\u0192\7\6\2\2\u0191\u0193"+
		"\7a\2\2\u0192\u0191\3\2\2\2\u0192\u0193\3\2\2\2\u0193\u0194\3\2\2\2\u0194"+
		"\u0195\5\u0106\u0084\2\u0195\u0196\5\4\3\2\u0196+\3\2\2\2\u0197\u01a2"+
		"\5*\26\2\u0198\u019a\7a\2\2\u0199\u0198\3\2\2\2\u0199\u019a\3\2\2\2\u019a"+
		"\u019b\3\2\2\2\u019b\u019d\7\5\2\2\u019c\u019e\7a\2\2\u019d\u019c\3\2"+
		"\2\2\u019d\u019e\3\2\2\2\u019e\u019f\3\2\2\2\u019f\u01a1\5*\26\2\u01a0"+
		"\u0199\3\2\2\2\u01a1\u01a4\3\2\2\2\u01a2\u01a0\3\2\2\2\u01a2\u01a3\3\2"+
		"\2\2\u01a3-\3\2\2\2\u01a4\u01a2\3\2\2\2\u01a5\u01a7\7\7\2\2\u01a6\u01a8"+
		"\7a\2\2\u01a7\u01a6\3\2\2\2\u01a7\u01a8\3\2\2\2\u01a8\u01aa\3\2\2\2\u01a9"+
		"\u01ab\5,\27\2\u01aa\u01a9\3\2\2\2\u01aa\u01ab\3\2\2\2\u01ab\u01ad\3\2"+
		"\2\2\u01ac\u01ae\7a\2\2\u01ad\u01ac\3\2\2\2\u01ad\u01ae\3\2\2\2\u01ae"+
		"\u01af\3\2\2\2\u01af\u01b1\7\b\2\2\u01b0\u01a5\3\2\2\2\u01b0\u01b1\3\2"+
		"\2\2\u01b1/\3\2\2\2\u01b2\u01b3\5\4\3\2\u01b3\u01b5\5\24\13\2\u01b4\u01b6"+
		"\7a\2\2\u01b5\u01b4\3\2\2\2\u01b5\u01b6\3\2\2\2\u01b6\u01b7\3\2\2\2\u01b7"+
		"\u01b9\7\6\2\2\u01b8\u01ba\7a\2\2\u01b9\u01b8\3\2\2\2\u01b9\u01ba\3\2"+
		"\2\2\u01ba\u01bb\3\2\2\2\u01bb\u01bc\5\u0106\u0084\2\u01bc\u01bd\5\4\3"+
		"\2\u01bd\61\3\2\2\2\u01be\u01c9\5\60\31\2\u01bf\u01c1\7a\2\2\u01c0\u01bf"+
		"\3\2\2\2\u01c0\u01c1\3\2\2\2\u01c1\u01c2\3\2\2\2\u01c2\u01c4\7\5\2\2\u01c3"+
		"\u01c5\7a\2\2\u01c4\u01c3\3\2\2\2\u01c4\u01c5\3\2\2\2\u01c5\u01c6\3\2"+
		"\2\2\u01c6\u01c8\5\60\31\2\u01c7\u01c0\3\2\2\2\u01c8\u01cb\3\2\2\2\u01c9"+
		"\u01c7\3\2\2\2\u01c9\u01ca\3\2\2\2\u01ca\63\3\2\2\2\u01cb\u01c9\3\2\2"+
		"\2\u01cc\u01ce\7\t\2\2\u01cd\u01cf\7a\2\2\u01ce\u01cd\3\2\2\2\u01ce\u01cf"+
		"\3\2\2\2\u01cf\u01de\3\2\2\2\u01d0\u01db\5\24\13\2\u01d1\u01d3\7a\2\2"+
		"\u01d2\u01d1\3\2\2\2\u01d2\u01d3\3\2\2\2\u01d3\u01d4\3\2\2\2\u01d4\u01d6"+
		"\7\5\2\2\u01d5\u01d7\7a\2\2\u01d6\u01d5\3\2\2\2\u01d6\u01d7\3\2\2\2\u01d7"+
		"\u01d8\3\2\2\2\u01d8\u01da\5\24\13\2\u01d9\u01d2\3\2\2\2\u01da\u01dd\3"+
		"\2\2\2\u01db\u01d9\3\2\2\2\u01db\u01dc\3\2\2\2\u01dc\u01df\3\2\2\2\u01dd"+
		"\u01db\3\2\2\2\u01de\u01d0\3\2\2\2\u01de\u01df\3\2\2\2\u01df\u01e1\3\2"+
		"\2\2\u01e0\u01e2\7a\2\2\u01e1\u01e0\3\2\2\2\u01e1\u01e2\3\2\2\2\u01e2"+
		"\u01e3\3\2\2\2\u01e3\u01e4\7\n\2\2\u01e4\65\3\2\2\2\u01e5\u01f0\5\64\33"+
		"\2\u01e6\u01e8\7a\2\2\u01e7\u01e6\3\2\2\2\u01e7\u01e8\3\2\2\2\u01e8\u01e9"+
		"\3\2\2\2\u01e9\u01eb\7\5\2\2\u01ea\u01ec\7a\2\2\u01eb\u01ea\3\2\2\2\u01eb"+
		"\u01ec\3\2\2\2\u01ec\u01ed\3\2\2\2\u01ed\u01ef\5\64\33\2\u01ee\u01e7\3"+
		"\2\2\2\u01ef\u01f2\3\2\2\2\u01f0\u01ee\3\2\2\2\u01f0\u01f1\3\2\2\2\u01f1"+
		"\67\3\2\2\2\u01f2\u01f0\3\2\2\2\u01f3\u01fe\5\n\6\2\u01f4\u01f6\7a\2\2"+
		"\u01f5\u01f4\3\2\2\2\u01f5\u01f6\3\2\2\2\u01f6\u01f7\3\2\2\2\u01f7\u01f9"+
		"\7\5\2\2\u01f8\u01fa\7a\2\2\u01f9\u01f8\3\2\2\2\u01f9\u01fa\3\2\2\2\u01fa"+
		"\u01fb\3\2\2\2\u01fb\u01fd\5\n\6\2\u01fc\u01f5\3\2\2\2\u01fd\u0200\3\2"+
		"\2\2\u01fe\u01fc\3\2\2\2\u01fe\u01ff\3\2\2\2\u01ff9\3\2\2\2\u0200\u01fe"+
		"\3\2\2\2\u0201\u0202\7\13\2\2\u0202\u0203\7a\2\2\u0203\u0205\5\u00c6d"+
		"\2\u0204\u0206\7a\2\2\u0205\u0204\3\2\2\2\u0205\u0206\3\2\2\2\u0206\u0207"+
		"\3\2\2\2\u0207\u0209\7\f\2\2\u0208\u020a\7a\2\2\u0209\u0208\3\2\2\2\u0209"+
		"\u020a\3\2\2\2\u020a\u020b\3\2\2\2\u020b\u020d\5\u0082B\2\u020c\u020e"+
		"\7b\2\2\u020d\u020c\3\2\2\2\u020d\u020e\3\2\2\2\u020e;\3\2\2\2\u020f\u0216"+
		"\5:\36\2\u0210\u0212\7a\2\2\u0211\u0210\3\2\2\2\u0211\u0212\3\2\2\2\u0212"+
		"\u0213\3\2\2\2\u0213\u0215\5:\36\2\u0214\u0211\3\2\2\2\u0215\u0218\3\2"+
		"\2\2\u0216\u0214\3\2\2\2\u0216\u0217\3\2\2\2\u0217=\3\2\2\2\u0218\u0216"+
		"\3\2\2\2\u0219\u021a\7\13\2\2\u021a\u021b\7a\2\2\u021b\u021d\5\u0082B"+
		"\2\u021c\u021e\7a\2\2\u021d\u021c\3\2\2\2\u021d\u021e\3\2\2\2\u021e\u021f"+
		"\3\2\2\2\u021f\u0221\7\f\2\2\u0220\u0222\7a\2\2\u0221\u0220\3\2\2\2\u0221"+
		"\u0222\3\2\2\2\u0222\u0223\3\2\2\2\u0223\u0225\5\u0082B\2\u0224\u0226"+
		"\7b\2\2\u0225\u0224\3\2\2\2\u0225\u0226\3\2\2\2\u0226?\3\2\2\2\u0227\u022e"+
		"\5> \2\u0228\u022a\7a\2\2\u0229\u0228\3\2\2\2\u0229\u022a\3\2\2\2\u022a"+
		"\u022b\3\2\2\2\u022b\u022d\5> \2\u022c\u0229\3\2\2\2\u022d\u0230\3\2\2"+
		"\2\u022e\u022c\3\2\2\2\u022e\u022f\3\2\2\2\u022fA\3\2\2\2\u0230\u022e"+
		"\3\2\2\2\u0231\u0232\5\4\3\2\u0232\u023b\5&\24\2\u0233\u0235\7a\2\2\u0234"+
		"\u0233\3\2\2\2\u0234\u0235\3\2\2\2\u0235\u0236\3\2\2\2\u0236\u0238\7\6"+
		"\2\2\u0237\u0239\7a\2\2\u0238\u0237\3\2\2\2\u0238\u0239\3\2\2\2\u0239"+
		"\u023a\3\2\2\2\u023a\u023c\5\u0106\u0084\2\u023b\u0234\3\2\2\2\u023b\u023c"+
		"\3\2\2\2\u023c\u023d\3\2\2\2\u023d\u023e\5\4\3\2\u023eC\3\2\2\2\u023f"+
		"\u0241\7\r\2\2\u0240\u0242\7a\2\2\u0241\u0240\3\2\2\2\u0241\u0242\3\2"+
		"\2\2\u0242\u0243\3\2\2\2\u0243\u024e\5B\"\2\u0244\u0246\7a\2\2\u0245\u0244"+
		"\3\2\2\2\u0245\u0246\3\2\2\2\u0246\u0247\3\2\2\2\u0247\u0249\7\5\2\2\u0248"+
		"\u024a\7a\2\2\u0249\u0248\3\2\2\2\u0249\u024a\3\2\2\2\u024a\u024b\3\2"+
		"\2\2\u024b\u024d\5B\"\2\u024c\u0245\3\2\2\2\u024d\u0250\3\2\2\2\u024e"+
		"\u024c\3\2\2\2\u024e\u024f\3\2\2\2\u024f\u0251\3\2\2\2\u0250\u024e\3\2"+
		"\2\2\u0251\u0252\7\16\2\2\u0252\u0254\3\2\2\2\u0253\u023f\3\2\2\2\u0253"+
		"\u0254\3\2\2\2\u0254E\3\2\2\2\u0255\u0256\7\r\2\2\u0256\u0261\5\u0106"+
		"\u0084\2\u0257\u0259\7a\2\2\u0258\u0257\3\2\2\2\u0258\u0259\3\2\2\2\u0259"+
		"\u025a\3\2\2\2\u025a\u025c\7\5\2\2\u025b\u025d\7a\2\2\u025c\u025b\3\2"+
		"\2\2\u025c\u025d\3\2\2\2\u025d\u025e\3\2\2\2\u025e\u0260\5\u0106\u0084"+
		"\2\u025f\u0258\3\2\2\2\u0260\u0263\3\2\2\2\u0261\u025f\3\2\2\2\u0261\u0262"+
		"\3\2\2\2\u0262\u0264\3\2\2\2\u0263\u0261\3\2\2\2\u0264\u0265\7\16\2\2"+
		"\u0265G\3\2\2\2\u0266\u0267\5\4\3\2\u0267\u0268\5\26\f\2\u0268\u0269\5"+
		"F$\2\u0269\u026a\5\4\3\2\u026aI\3\2\2\2\u026b\u0276\5H%\2\u026c\u026e"+
		"\7a\2\2\u026d\u026c\3\2\2\2\u026d\u026e\3\2\2\2\u026e\u026f\3\2\2\2\u026f"+
		"\u0271\7\5\2\2\u0270\u0272\7a\2\2\u0271\u0270\3\2\2\2\u0271\u0272\3\2"+
		"\2\2\u0272\u0273\3\2\2\2\u0273\u0275\5H%\2\u0274\u026d\3\2\2\2\u0275\u0278"+
		"\3\2\2\2\u0276\u0274\3\2\2\2\u0276\u0277\3\2\2\2\u0277K\3\2\2\2\u0278"+
		"\u0276\3\2\2\2\u0279\u027b\7a\2\2\u027a\u0279\3\2\2\2\u027a\u027b\3\2"+
		"\2\2\u027b\u027c\3\2\2\2\u027c\u027e\7\17\2\2\u027d\u027f\7a\2\2\u027e"+
		"\u027d\3\2\2\2\u027e\u027f\3\2\2\2\u027f\u0280\3\2\2\2\u0280\u0282\5J"+
		"&\2\u0281\u0283\7a\2\2\u0282\u0281\3\2\2\2\u0282\u0283\3\2\2\2\u0283\u0285"+
		"\3\2\2\2\u0284\u027a\3\2\2\2\u0284\u0285\3\2\2\2\u0285M\3\2\2\2\u0286"+
		"\u0287\5\4\3\2\u0287\u0288\7\20\2\2\u0288\u0289\5\22\n\2\u0289\u028a\5"+
		"\4\3\2\u028aO\3\2\2\2\u028b\u0290\5N(\2\u028c\u028d\7a\2\2\u028d\u028f"+
		"\5N(\2\u028e\u028c\3\2\2\2\u028f\u0292\3\2\2\2\u0290\u028e\3\2\2\2\u0290"+
		"\u0291\3\2\2\2\u0291Q\3\2\2\2\u0292\u0290\3\2\2\2\u0293\u0295\7a\2\2\u0294"+
		"\u0293\3\2\2\2\u0294\u0295\3\2\2\2\u0295\u0299\3\2\2\2\u0296\u029a\5T"+
		"+\2\u0297\u029a\5V,\2\u0298\u029a\5X-\2\u0299\u0296\3\2\2\2\u0299\u0297"+
		"\3\2\2\2\u0299\u0298\3\2\2\2\u029aS\3\2\2\2\u029b\u029c\5\4\3\2\u029c"+
		"\u029d\7\21\2\2\u029d\u029e\7a\2\2\u029e\u029f\5\f\7\2\u029f\u02a0\7\4"+
		"\2\2\u02a0\u02a1\7g\2\2\u02a1\u02a2\5\b\5\2\u02a2\u02a3\5\4\3\2\u02a3"+
		"U\3\2\2\2\u02a4\u02a5\5\4\3\2\u02a5\u02a6\7\21\2\2\u02a6\u02a7\7a\2\2"+
		"\u02a7\u02a8\5\f\7\2\u02a8\u02a9\7\4\2\2\u02a9\u02aa\5\n\6\2\u02aa\u02ab"+
		"\5\b\5\2\u02ab\u02ac\5\4\3\2\u02acW\3\2\2\2\u02ad\u02ae\5\4\3\2\u02ae"+
		"\u02af\7\21\2\2\u02af\u02b0\7a\2\2\u02b0\u02b1\5\f\7\2\u02b1\u02b2\5\b"+
		"\5\2\u02b2\u02b3\5\4\3\2\u02b3Y\3\2\2\2\u02b4\u02c2\5\\/\2\u02b5\u02c2"+
		"\5^\60\2\u02b6\u02c2\5d\63\2\u02b7\u02c2\5f\64\2\u02b8\u02c2\5h\65\2\u02b9"+
		"\u02c2\5j\66\2\u02ba\u02c2\5l\67\2\u02bb\u02c2\5n8\2\u02bc\u02c2\5p9\2"+
		"\u02bd\u02c2\5r:\2\u02be\u02c2\5v<\2\u02bf\u02c2\5x=\2\u02c0\u02c2\5|"+
		"?\2\u02c1\u02b4\3\2\2\2\u02c1\u02b5\3\2\2\2\u02c1\u02b6\3\2\2\2\u02c1"+
		"\u02b7\3\2\2\2\u02c1\u02b8\3\2\2\2\u02c1\u02b9\3\2\2\2\u02c1\u02ba\3\2"+
		"\2\2\u02c1\u02bb\3\2\2\2\u02c1\u02bc\3\2\2\2\u02c1\u02bd\3\2\2\2\u02c1"+
		"\u02be\3\2\2\2\u02c1\u02bf\3\2\2\2\u02c1\u02c0\3\2\2\2\u02c2[\3\2\2\2"+
		"\u02c3\u02c5\7a\2\2\u02c4\u02c3\3\2\2\2\u02c4\u02c5\3\2\2\2\u02c5\u02c6"+
		"\3\2\2\2\u02c6\u02c7\5\4\3\2\u02c7\u02c8\7\22\2\2\u02c8\u02c9\7a\2\2\u02c9"+
		"\u02cb\5\f\7\2\u02ca\u02cc\7a\2\2\u02cb\u02ca\3\2\2\2\u02cb\u02cc\3\2"+
		"\2\2\u02cc\u02cd\3\2\2\2\u02cd\u02cf\7\t\2\2\u02ce\u02d0\7a\2\2\u02cf"+
		"\u02ce\3\2\2\2\u02cf\u02d0\3\2\2\2\u02d0\u02d4\3\2\2\2\u02d1\u02d3\5Z"+
		".\2\u02d2\u02d1\3\2\2\2\u02d3\u02d6\3\2\2\2\u02d4\u02d2\3\2\2\2\u02d4"+
		"\u02d5\3\2\2\2\u02d5\u02d8\3\2\2\2\u02d6\u02d4\3\2\2\2\u02d7\u02d9\7a"+
		"\2\2\u02d8\u02d7\3\2\2\2\u02d8\u02d9\3\2\2\2\u02d9\u02da\3\2\2\2\u02da"+
		"\u02db\7\n\2\2\u02db\u02dc\5\4\3\2\u02dc\u02dd\5\b\5\2\u02dd]\3\2\2\2"+
		"\u02de\u02e0\7a\2\2\u02df\u02de\3\2\2\2\u02df\u02e0\3\2\2\2\u02e0\u02e1"+
		"\3\2\2\2\u02e1\u02e3\5\2\2\2\u02e2\u02df\3\2\2\2\u02e3\u02e6\3\2\2\2\u02e4"+
		"\u02e2\3\2\2\2\u02e4\u02e5\3\2\2\2\u02e5\u02e8\3\2\2\2\u02e6\u02e4\3\2"+
		"\2\2\u02e7\u02e9\7a\2\2\u02e8\u02e7\3\2\2\2\u02e8\u02e9\3\2\2\2\u02e9"+
		"\u02ea\3\2\2\2\u02ea\u02eb\5\4\3\2\u02eb\u02ec\7\23\2\2\u02ec\u02ed\7"+
		"a\2\2\u02ed\u02ee\5\"\22\2\u02ee\u02f0\5D#\2\u02ef\u02f1\7a\2\2\u02f0"+
		"\u02ef\3\2\2\2\u02f0\u02f1\3\2\2\2\u02f1\u02f2\3\2\2\2\u02f2\u02f4\7\t"+
		"\2\2\u02f3\u02f5\7a\2\2\u02f4\u02f3\3\2\2\2\u02f4\u02f5\3\2\2\2\u02f5"+
		"\u02f6\3\2\2\2\u02f6\u02f8\5`\61\2\u02f7\u02f9\7a\2\2\u02f8\u02f7\3\2"+
		"\2\2\u02f8\u02f9\3\2\2\2\u02f9\u02fa\3\2\2\2\u02fa\u02fb\7\n\2\2\u02fb"+
		"\u02fc\5\4\3\2\u02fc\u02fd\5\b\5\2\u02fd_\3\2\2\2\u02fe\u0309\5b\62\2"+
		"\u02ff\u0301\7a\2\2\u0300\u02ff\3\2\2\2\u0300\u0301\3\2\2\2\u0301\u0302"+
		"\3\2\2\2\u0302\u0304\7\5\2\2\u0303\u0305\7a\2\2\u0304\u0303\3\2\2\2\u0304"+
		"\u0305\3\2\2\2\u0305\u0306\3\2\2\2\u0306\u0308\5b\62\2\u0307\u0300\3\2"+
		"\2\2\u0308\u030b\3\2\2\2\u0309\u0307\3\2\2\2\u0309\u030a\3\2\2\2\u030a"+
		"a\3\2\2\2\u030b\u0309\3\2\2\2\u030c\u030d\5\4\3\2\u030d\u030e\7\13\2\2"+
		"\u030e\u030f\7a\2\2\u030f\u0311\5 \21\2\u0310\u0312\5\u0106\u0084\2\u0311"+
		"\u0310\3\2\2\2\u0311\u0312\3\2\2\2\u0312\u0313\3\2\2\2\u0313\u0314\5\4"+
		"\3\2\u0314c\3\2\2\2\u0315\u0317\7a\2\2\u0316\u0315\3\2\2\2\u0316\u0317"+
		"\3\2\2\2\u0317\u0318\3\2\2\2\u0318\u031a\5\2\2\2\u0319\u0316\3\2\2\2\u031a"+
		"\u031d\3\2\2\2\u031b\u0319\3\2\2\2\u031b\u031c\3\2\2\2\u031c\u031f\3\2"+
		"\2\2\u031d\u031b\3\2\2\2\u031e\u0320\7a\2\2\u031f\u031e\3\2\2\2\u031f"+
		"\u0320\3\2\2\2\u0320\u0321\3\2\2\2\u0321\u0322\5\4\3\2\u0322\u0323\7\24"+
		"\2\2\u0323\u0324\7a\2\2\u0324\u0326\5\34\17\2\u0325\u0327\7a\2\2\u0326"+
		"\u0325\3\2\2\2\u0326\u0327\3\2\2\2\u0327\u0328\3\2\2\2\u0328\u032a\7\7"+
		"\2\2\u0329\u032b\7a\2\2\u032a\u0329\3\2\2\2\u032a\u032b\3\2\2\2\u032b"+
		"\u032d\3\2\2\2\u032c\u032e\5\62\32\2\u032d\u032c\3\2\2\2\u032d\u032e\3"+
		"\2\2\2\u032e\u0330\3\2\2\2\u032f\u0331\7a\2\2\u0330\u032f\3\2\2\2\u0330"+
		"\u0331\3\2\2\2\u0331\u0332\3\2\2\2\u0332\u0333\7\b\2\2\u0333\u0334\5\4"+
		"\3\2\u0334\u0335\5\b\5\2\u0335e\3\2\2\2\u0336\u0338\7a\2\2\u0337\u0336"+
		"\3\2\2\2\u0337\u0338\3\2\2\2\u0338\u0339\3\2\2\2\u0339\u033b\5\2\2\2\u033a"+
		"\u0337\3\2\2\2\u033b\u033e\3\2\2\2\u033c\u033a\3\2\2\2\u033c\u033d\3\2"+
		"\2\2\u033d\u0340\3\2\2\2\u033e\u033c\3\2\2\2\u033f\u0341\7a\2\2\u0340"+
		"\u033f\3\2\2\2\u0340\u0341\3\2\2\2\u0341\u0342\3\2\2\2\u0342\u0343\5\4"+
		"\3\2\u0343\u0344\7\25\2\2\u0344\u0345\7a\2\2\u0345\u0347\5\34\17\2\u0346"+
		"\u0348\7a\2\2\u0347\u0346\3\2\2\2\u0347\u0348\3\2\2\2\u0348\u0349\3\2"+
		"\2\2\u0349\u034b\7\7\2\2\u034a\u034c\7a\2\2\u034b\u034a\3\2\2\2\u034b"+
		"\u034c\3\2\2\2\u034c\u034e\3\2\2\2\u034d\u034f\5\62\32\2\u034e\u034d\3"+
		"\2\2\2\u034e\u034f\3\2\2\2\u034f\u0351\3\2\2\2\u0350\u0352\7a\2\2\u0351"+
		"\u0350\3\2\2\2\u0351\u0352\3\2\2\2\u0352\u0353\3\2\2\2\u0353\u0354\7\b"+
		"\2\2\u0354\u0355\5\4\3\2\u0355\u0356\5\b\5\2\u0356g\3\2\2\2\u0357\u0359"+
		"\7a\2\2\u0358\u0357\3\2\2\2\u0358\u0359\3\2\2\2\u0359\u035a\3\2\2\2\u035a"+
		"\u035b\5\4\3\2\u035b\u035c\7\26\2\2\u035c\u035d\7a\2\2\u035d\u035f\5\36"+
		"\20\2\u035e\u0360\7a\2\2\u035f\u035e\3\2\2\2\u035f\u0360\3\2\2\2\u0360"+
		"\u0361\3\2\2\2\u0361\u0363\7\7\2\2\u0362\u0364\7a\2\2\u0363\u0362\3\2"+
		"\2\2\u0363\u0364\3\2\2\2\u0364\u0366\3\2\2\2\u0365\u0367\5\66\34\2\u0366"+
		"\u0365\3\2\2\2\u0366\u0367\3\2\2\2\u0367\u0369\3\2\2\2\u0368\u036a\7a"+
		"\2\2\u0369\u0368\3\2\2\2\u0369\u036a\3\2\2\2\u036a\u036b\3\2\2\2\u036b"+
		"\u036c\7\b\2\2\u036c\u036d\5\4\3\2\u036d\u036e\5\b\5\2\u036ei\3\2\2\2"+
		"\u036f\u0371\7a\2\2\u0370\u036f\3\2\2\2\u0370\u0371\3\2\2\2\u0371\u0372"+
		"\3\2\2\2\u0372\u0374\5\2\2\2\u0373\u0370\3\2\2\2\u0374\u0377\3\2\2\2\u0375"+
		"\u0373\3\2\2\2\u0375\u0376\3\2\2\2\u0376\u0379\3\2\2\2\u0377\u0375\3\2"+
		"\2\2\u0378\u037a\7a\2\2\u0379\u0378\3\2\2\2\u0379\u037a\3\2\2\2\u037a"+
		"\u037b\3\2\2\2\u037b\u037c\5\4\3\2\u037c\u037d\7\27\2\2\u037d\u037e\7"+
		"a\2\2\u037e\u0380\5\30\r\2\u037f\u0381\7a\2\2\u0380\u037f\3\2\2\2\u0380"+
		"\u0381\3\2\2\2\u0381\u0382\3\2\2\2\u0382\u0384\5.\30\2\u0383\u0385\7a"+
		"\2\2\u0384\u0383\3\2\2\2\u0384\u0385\3\2\2\2\u0385\u0386\3\2\2\2\u0386"+
		"\u0388\7\6\2\2\u0387\u0389\7a\2\2\u0388\u0387\3\2\2\2\u0388\u0389\3\2"+
		"\2\2\u0389\u038a\3\2\2\2\u038a\u038b\5\u0106\u0084\2\u038b\u038c\5\4\3"+
		"\2\u038c\u038d\5\b\5\2\u038dk\3\2\2\2\u038e\u0390\7a\2\2\u038f\u038e\3"+
		"\2\2\2\u038f\u0390\3\2\2\2\u0390\u0391\3\2\2\2\u0391\u0393\5\2\2\2\u0392"+
		"\u038f\3\2\2\2\u0393\u0396\3\2\2\2\u0394\u0392\3\2\2\2\u0394\u0395\3\2"+
		"\2\2\u0395\u0398\3\2\2\2\u0396\u0394\3\2\2\2\u0397\u0399\7a\2\2\u0398"+
		"\u0397\3\2\2\2\u0398\u0399\3\2\2\2\u0399\u039a\3\2\2\2\u039a\u039b\5\4"+
		"\3\2\u039b\u039d\7\30\2\2\u039c\u039e\7a\2\2\u039d\u039c\3\2\2\2\u039d"+
		"\u039e\3\2\2\2\u039e\u039f\3\2\2\2\u039f\u03a0\7\27\2\2\u03a0\u03a1\7"+
		"a\2\2\u03a1\u03a3\5\30\r\2\u03a2\u03a4\7a\2\2\u03a3\u03a2\3\2\2\2\u03a3"+
		"\u03a4\3\2\2\2\u03a4\u03a5\3\2\2\2\u03a5\u03a7\5.\30\2\u03a6\u03a8\7a"+
		"\2\2\u03a7\u03a6\3\2\2\2\u03a7\u03a8\3\2\2\2\u03a8\u03a9\3\2\2\2\u03a9"+
		"\u03ab\7\6\2\2\u03aa\u03ac\7a\2\2\u03ab\u03aa\3\2\2\2\u03ab\u03ac\3\2"+
		"\2\2\u03ac\u03ad\3\2\2\2\u03ad\u03ae\5\u0106\u0084\2\u03ae\u03af\5\4\3"+
		"\2\u03af\u03b0\5\b\5\2\u03b0m\3\2\2\2\u03b1\u03b3\7a\2\2\u03b2\u03b1\3"+
		"\2\2\2\u03b2\u03b3\3\2\2\2\u03b3\u03b4\3\2\2\2\u03b4\u03b6\5\2\2\2\u03b5"+
		"\u03b2\3\2\2\2\u03b6\u03b9\3\2\2\2\u03b7\u03b5\3\2\2\2\u03b7\u03b8\3\2"+
		"\2\2\u03b8\u03bb\3\2\2\2\u03b9\u03b7\3\2\2\2\u03ba\u03bc\7a\2\2\u03bb"+
		"\u03ba\3\2\2\2\u03bb\u03bc\3\2\2\2\u03bc\u03be\3\2\2\2\u03bd\u03bf\5P"+
		")\2\u03be\u03bd\3\2\2\2\u03be\u03bf\3\2\2\2\u03bf\u03c1\3\2\2\2\u03c0"+
		"\u03c2\7a\2\2\u03c1\u03c0\3\2\2\2\u03c1\u03c2\3\2\2\2\u03c2\u03c3\3\2"+
		"\2\2\u03c3\u03c4\5\4\3\2\u03c4\u03c5\7\27\2\2\u03c5\u03c6\7a\2\2\u03c6"+
		"\u03c8\5\30\r\2\u03c7\u03c9\7a\2\2\u03c8\u03c7\3\2\2\2\u03c8\u03c9\3\2"+
		"\2\2\u03c9\u03ca\3\2\2\2\u03ca\u03cb\5D#\2\u03cb\u03cd\5.\30\2\u03cc\u03ce"+
		"\7a\2\2\u03cd\u03cc\3\2\2\2\u03cd\u03ce\3\2\2\2\u03ce\u03cf\3\2\2\2\u03cf"+
		"\u03d1\7\6\2\2\u03d0\u03d2\7a\2\2\u03d1\u03d0\3\2\2\2\u03d1\u03d2\3\2"+
		"\2\2\u03d2\u03d3\3\2\2\2\u03d3\u03d5\5\u0106\u0084\2\u03d4\u03d6\7a\2"+
		"\2\u03d5\u03d4\3\2\2\2\u03d5\u03d6\3\2\2\2\u03d6\u03d7\3\2\2\2\u03d7\u03d9"+
		"\7\31\2\2\u03d8\u03da\7a\2\2\u03d9\u03d8\3\2\2\2\u03d9\u03da\3\2\2\2\u03da"+
		"\u03db\3\2\2\2\u03db\u03dc\5\u0082B\2\u03dc\u03dd\5\4\3\2\u03dd\u03de"+
		"\5\b\5\2\u03deo\3\2\2\2\u03df\u03e1\7a\2\2\u03e0\u03df\3\2\2\2\u03e0\u03e1"+
		"\3\2\2\2\u03e1\u03e2\3\2\2\2\u03e2\u03e4\5\2\2\2\u03e3\u03e0\3\2\2\2\u03e4"+
		"\u03e7\3\2\2\2\u03e5\u03e3\3\2\2\2\u03e5\u03e6\3\2\2\2\u03e6\u03e9\3\2"+
		"\2\2\u03e7\u03e5\3\2\2\2\u03e8\u03ea\7a\2\2\u03e9\u03e8\3\2\2\2\u03e9"+
		"\u03ea\3\2\2\2\u03ea\u03eb\3\2\2\2\u03eb\u03ec\5\4\3\2\u03ec\u03ed\7\32"+
		"\2\2\u03ed\u03ee\7a\2\2\u03ee\u03f0\5\30\r\2\u03ef\u03f1\7a\2\2\u03f0"+
		"\u03ef\3\2\2\2\u03f0\u03f1\3\2\2\2\u03f1\u03f2\3\2\2\2\u03f2\u03f4\5D"+
		"#\2\u03f3\u03f5\7a\2\2\u03f4\u03f3\3\2\2\2\u03f4\u03f5\3\2\2\2\u03f5\u03f6"+
		"\3\2\2\2\u03f6\u03f8\5.\30\2\u03f7\u03f9\7a\2\2\u03f8\u03f7\3\2\2\2\u03f8"+
		"\u03f9\3\2\2\2\u03f9\u03fa\3\2\2\2\u03fa\u03fc\7\6\2\2\u03fb\u03fd\7a"+
		"\2\2\u03fc\u03fb\3\2\2\2\u03fc\u03fd\3\2\2\2\u03fd\u03fe\3\2\2\2\u03fe"+
		"\u0400\5\u0106\u0084\2\u03ff\u0401\7a\2\2\u0400\u03ff\3\2\2\2\u0400\u0401"+
		"\3\2\2\2\u0401\u0402\3\2\2\2\u0402\u0404\7\31\2\2\u0403\u0405\7a\2\2\u0404"+
		"\u0403\3\2\2\2\u0404\u0405\3\2\2\2\u0405\u0406\3\2\2\2\u0406\u0407\5\u0082"+
		"B\2\u0407\u0408\5\4\3\2\u0408\u0409\5\b\5\2\u0409q\3\2\2\2\u040a\u040c"+
		"\7a\2\2\u040b\u040a\3\2\2\2\u040b\u040c\3\2\2\2\u040c\u040d\3\2\2\2\u040d"+
		"\u040f\5\2\2\2\u040e\u040b\3\2\2\2\u040f\u0412\3\2\2\2\u0410\u040e\3\2"+
		"\2\2\u0410\u0411\3\2\2\2\u0411\u0414\3\2\2\2\u0412\u0410\3\2\2\2\u0413"+
		"\u0415\7a\2\2\u0414\u0413\3\2\2\2\u0414\u0415\3\2\2\2\u0415\u0416\3\2"+
		"\2\2\u0416\u0417\5\4\3\2\u0417\u0418\7\33\2\2\u0418\u0419\7a\2\2\u0419"+
		"\u041a\5\26\f\2\u041a\u041c\5F$\2\u041b\u041d\7a\2\2\u041c\u041b\3\2\2"+
		"\2\u041c\u041d\3\2\2\2\u041d\u041e\3\2\2\2\u041e\u0420\5L\'\2\u041f\u0421"+
		"\7a\2\2\u0420\u041f\3\2\2\2\u0420\u0421\3\2\2\2\u0421\u0422\3\2\2\2\u0422"+
		"\u0423\5t;\2\u0423\u0424\5\4\3\2\u0424s\3\2\2\2\u0425\u0427\7\t\2\2\u0426"+
		"\u0428\7a\2\2\u0427\u0426\3\2\2\2\u0427\u0428\3\2\2\2\u0428\u042c\3\2"+
		"\2\2\u0429\u042d\5n8\2\u042a\u042d\5j\66\2\u042b\u042d\5p9\2\u042c\u0429"+
		"\3\2\2\2\u042c\u042a\3\2\2\2\u042c\u042b\3\2\2\2\u042d\u042f\3\2\2\2\u042e"+
		"\u0430\7a\2\2\u042f\u042e\3\2\2\2\u042f\u0430\3\2\2\2\u0430\u0431\3\2"+
		"\2\2\u0431\u0432\7\n\2\2\u0432u\3\2\2\2\u0433\u0435\7a\2\2\u0434\u0433"+
		"\3\2\2\2\u0434\u0435\3\2\2\2\u0435\u0436\3\2\2\2\u0436\u0437\5\4\3\2\u0437"+
		"\u0439\5\u0120\u0091\2\u0438\u043a\7a\2\2\u0439\u0438\3\2\2\2\u0439\u043a"+
		"\3\2\2\2\u043a\u043b\3\2\2\2\u043b\u043c\7\3\2\2\u043c\u043d\5\4\3\2\u043d"+
		"w\3\2\2\2\u043e\u0440\7a\2\2\u043f\u043e\3\2\2\2\u043f\u0440\3\2\2\2\u0440"+
		"\u0441\3\2\2\2\u0441\u0442\5\4\3\2\u0442\u0444\5\u0120\u0091\2\u0443\u0445"+
		"\7a\2\2\u0444\u0443\3\2\2\2\u0444\u0445\3\2\2\2\u0445\u0446\3\2\2\2\u0446"+
		"\u0448\7\34\2\2\u0447\u0449\7a\2\2\u0448\u0447\3\2\2\2\u0448\u0449\3\2"+
		"\2\2\u0449\u044a\3\2\2\2\u044a\u044c\5\u0122\u0092\2\u044b\u044d\7a\2"+
		"\2\u044c\u044b\3\2\2\2\u044c\u044d\3\2\2\2\u044d\u044e\3\2\2\2\u044e\u044f"+
		"\7\3\2\2\u044f\u0450\5\4\3\2\u0450y\3\2\2\2\u0451\u0452\5\u0088E\2\u0452"+
		"{\3\2\2\2\u0453\u0455\7a\2\2\u0454\u0453\3\2\2\2\u0454\u0455\3\2\2\2\u0455"+
		"\u0456\3\2\2\2\u0456\u0457\5\4\3\2\u0457\u0459\7\35\2\2\u0458\u045a\7"+
		"a\2\2\u0459\u0458\3\2\2\2\u0459\u045a\3\2\2\2\u045a\u045b\3\2\2\2\u045b"+
		"\u045c\5\u0106\u0084\2\u045c\u045e\7\36\2\2\u045d\u045f\7a\2\2\u045e\u045d"+
		"\3\2\2\2\u045e\u045f\3\2\2\2\u045f\u0460\3\2\2\2\u0460\u0462\7\31\2\2"+
		"\u0461\u0463\7a\2\2\u0462\u0461\3\2\2\2\u0462\u0463\3\2\2\2\u0463\u0464"+
		"\3\2\2\2\u0464\u0466\7\7\2\2\u0465\u0467\7a\2\2\u0466\u0465\3\2\2\2\u0466"+
		"\u0467\3\2\2\2\u0467\u0468\3\2\2\2\u0468\u046a\5z>\2\u0469\u046b\7a\2"+
		"\2\u046a\u0469\3\2\2\2\u046a\u046b\3\2\2\2\u046b\u046c\3\2\2\2\u046c\u046d"+
		"\7\b\2\2\u046d\u046e\5\4\3\2\u046e\u046f\5\b\5\2\u046f}\3\2\2\2\u0470"+
		"\u0472\7a\2\2\u0471\u0470\3\2\2\2\u0471\u0472\3\2\2\2\u0472\u0473\3\2"+
		"\2\2\u0473\u0475\5\2\2\2\u0474\u0471\3\2\2\2\u0475\u0478\3\2\2\2\u0476"+
		"\u0474\3\2\2\2\u0476\u0477\3\2\2\2\u0477\u047a\3\2\2\2\u0478\u0476\3\2"+
		"\2\2\u0479\u047b\7a\2\2\u047a\u0479\3\2\2\2\u047a\u047b\3\2\2\2\u047b"+
		"\u047c\3\2\2\2\u047c\u047d\5\4\3\2\u047d\u047e\7\37\2\2\u047e\u047f\7"+
		"a\2\2\u047f\u0480\5\26\f\2\u0480\u0482\5F$\2\u0481\u0483\7a\2\2\u0482"+
		"\u0481\3\2\2\2\u0482\u0483\3\2\2\2\u0483\u0484\3\2\2\2\u0484\u0486\5L"+
		"\'\2\u0485\u0487\7a\2\2\u0486\u0485\3\2\2\2\u0486\u0487\3\2\2\2\u0487"+
		"\u0488\3\2\2\2\u0488\u0489\5\u0080A\2\u0489\u048a\5\4\3\2\u048a\177\3"+
		"\2\2\2\u048b\u048d\7\t\2\2\u048c\u048e\7a\2\2\u048d\u048c\3\2\2\2\u048d"+
		"\u048e\3\2\2\2\u048e\u0492\3\2\2\2\u048f\u0491\5n8\2\u0490\u048f\3\2\2"+
		"\2\u0491\u0494\3\2\2\2\u0492\u0490\3\2\2\2\u0492\u0493\3\2\2\2\u0493\u0496"+
		"\3\2\2\2\u0494\u0492\3\2\2\2\u0495\u0497\7a\2\2\u0496\u0495\3\2\2\2\u0496"+
		"\u0497\3\2\2\2\u0497\u0498\3\2\2\2\u0498\u0499\7\n\2\2\u0499\u0081\3\2"+
		"\2\2\u049a\u049b\5\u0084C\2\u049b\u0083\3\2\2\2\u049c\u049e\7\t\2\2\u049d"+
		"\u049f\7a\2\2\u049e\u049d\3\2\2\2\u049e\u049f\3\2\2\2\u049f\u04a0\3\2"+
		"\2\2\u04a0\u04a2\5\u0082B\2\u04a1\u04a3\7a\2\2\u04a2\u04a1\3\2\2\2\u04a2"+
		"\u04a3\3\2\2\2\u04a3\u04a4\3\2\2\2\u04a4\u04a6\7\n\2\2\u04a5\u04a7\7a"+
		"\2\2\u04a6\u04a5\3\2\2\2\u04a6\u04a7\3\2\2\2\u04a7\u04aa\3\2\2\2\u04a8"+
		"\u04aa\5\u0086D\2\u04a9\u049c\3\2\2\2\u04a9\u04a8\3\2\2\2\u04aa\u0085"+
		"\3\2\2\2\u04ab\u04b5\5\u008aF\2\u04ac\u04ae\7a\2\2\u04ad\u04ac\3\2\2\2"+
		"\u04ad\u04ae\3\2\2\2\u04ae\u04af\3\2\2\2\u04af\u04b1\5\u0116\u008c\2\u04b0"+
		"\u04b2\7a\2\2\u04b1\u04b0\3\2\2\2\u04b1\u04b2\3\2\2\2\u04b2\u04b3\3\2"+
		"\2\2\u04b3\u04b4\5\u008aF\2\u04b4\u04b6\3\2\2\2\u04b5\u04ad\3\2\2\2\u04b5"+
		"\u04b6\3\2\2\2\u04b6\u04b7\3\2\2\2\u04b7\u04b8\5\4\3\2\u04b8\u0087\3\2"+
		"\2\2\u04b9\u04c4\5\u0082B\2\u04ba\u04bc\7a\2\2\u04bb\u04ba\3\2\2\2\u04bb"+
		"\u04bc\3\2\2\2\u04bc\u04bd\3\2\2\2\u04bd\u04bf\7\5\2\2\u04be\u04c0\7a"+
		"\2\2\u04bf\u04be\3\2\2\2\u04bf\u04c0\3\2\2\2\u04c0\u04c1\3\2\2\2\u04c1"+
		"\u04c3\5\u0082B\2\u04c2\u04bb\3\2\2\2\u04c3\u04c6\3\2\2\2\u04c4\u04c2"+
		"\3\2\2\2\u04c4\u04c5\3\2\2\2\u04c5\u0089\3\2\2\2\u04c6\u04c4\3\2\2\2\u04c7"+
		"\u04d1\5\u008cG\2\u04c8\u04ca\7a\2\2\u04c9\u04c8\3\2\2\2\u04c9\u04ca\3"+
		"\2\2\2\u04ca\u04cb\3\2\2\2\u04cb\u04cd\5\u0118\u008d\2\u04cc\u04ce\7a"+
		"\2\2\u04cd\u04cc\3\2\2\2\u04cd\u04ce\3\2\2\2\u04ce\u04cf\3\2\2\2\u04cf"+
		"\u04d0\5\u008cG\2\u04d0\u04d2\3\2\2\2\u04d1\u04c9\3\2\2\2\u04d1\u04d2"+
		"\3\2\2\2\u04d2\u04d3\3\2\2\2\u04d3\u04d4\5\4\3\2\u04d4\u008b\3\2\2\2\u04d5"+
		"\u04e1\5\u008eH\2\u04d6\u04d8\7a\2\2\u04d7\u04d6\3\2\2\2\u04d7\u04d8\3"+
		"\2\2\2\u04d8\u04d9\3\2\2\2\u04d9\u04db\5\u011c\u008f\2\u04da\u04dc\7a"+
		"\2\2\u04db\u04da\3\2\2\2\u04db\u04dc\3\2\2\2\u04dc\u04dd\3\2\2\2\u04dd"+
		"\u04de\5\u008eH\2\u04de\u04e0\3\2\2\2\u04df\u04d7\3\2\2\2\u04e0\u04e3"+
		"\3\2\2\2\u04e1\u04df\3\2\2\2\u04e1\u04e2\3\2\2\2\u04e2\u04e4\3\2\2\2\u04e3"+
		"\u04e1\3\2\2\2\u04e4\u04e5\5\4\3\2\u04e5\u008d\3\2\2\2\u04e6\u04f2\5\u0090"+
		"I\2\u04e7\u04e9\7a\2\2\u04e8\u04e7\3\2\2\2\u04e8\u04e9\3\2\2\2\u04e9\u04ea"+
		"\3\2\2\2\u04ea\u04ec\5\u011a\u008e\2\u04eb\u04ed\7a\2\2\u04ec\u04eb\3"+
		"\2\2\2\u04ec\u04ed\3\2\2\2\u04ed\u04ee\3\2\2\2\u04ee\u04ef\5\u0090I\2"+
		"\u04ef\u04f1\3\2\2\2\u04f0\u04e8\3\2\2\2\u04f1\u04f4\3\2\2\2\u04f2\u04f0"+
		"\3\2\2\2\u04f2\u04f3\3\2\2\2\u04f3\u04f5\3\2\2\2\u04f4\u04f2\3\2\2\2\u04f5"+
		"\u04f6\5\4\3\2\u04f6\u008f\3\2\2\2\u04f7\u0503\5\u0092J\2\u04f8\u04fa"+
		"\7a\2\2\u04f9\u04f8\3\2\2\2\u04f9\u04fa\3\2\2\2\u04fa\u04fb\3\2\2\2\u04fb"+
		"\u04fc\7 \2\2\u04fc\u04fd\5\32\16\2\u04fd\u04ff\7 \2\2\u04fe\u0500\7a"+
		"\2\2\u04ff\u04fe\3\2\2\2\u04ff\u0500\3\2\2\2\u0500\u0501\3\2\2\2\u0501"+
		"\u0502\5\u0092J\2\u0502\u0504\3\2\2\2\u0503\u04f9\3\2\2\2\u0503\u0504"+
		"\3\2\2\2\u0504\u0505\3\2\2\2\u0505\u0506\5\4\3\2\u0506\u0091\3\2\2\2\u0507"+
		"\u0511\5\u0094K\2\u0508\u050a\7a\2\2\u0509\u0508\3\2\2\2\u0509\u050a\3"+
		"\2\2\2\u050a\u050b\3\2\2\2\u050b\u050d\5\u011e\u0090\2\u050c\u050e\7a"+
		"\2\2\u050d\u050c\3\2\2\2\u050d\u050e\3\2\2\2\u050e\u050f\3\2\2\2\u050f"+
		"\u0510\5\u0094K\2\u0510\u0512\3\2\2\2\u0511\u0509\3\2\2\2\u0511\u0512"+
		"\3\2\2\2\u0512\u0513\3\2\2\2\u0513\u0514\5\4\3\2\u0514\u0093\3\2\2\2\u0515"+
		"\u0516\5\4\3\2\u0516\u0518\5\u0114\u008b\2\u0517\u0519\7a\2\2\u0518\u0517"+
		"\3\2\2\2\u0518\u0519\3\2\2\2\u0519\u051a\3\2\2\2\u051a\u051b\5\u0094K"+
		"\2\u051b\u051c\5\4\3\2\u051c\u051f\3\2\2\2\u051d\u051f\5\u0096L\2\u051e"+
		"\u0515\3\2\2\2\u051e\u051d\3\2\2\2\u051f\u0095\3\2\2\2\u0520\u0529\5\u00b6"+
		"\\\2\u0521\u0523\7a\2\2\u0522\u0521\3\2\2\2\u0522\u0523\3\2\2\2\u0523"+
		"\u0524\3\2\2\2\u0524\u0526\7\6\2\2\u0525\u0527\7a\2\2\u0526\u0525\3\2"+
		"\2\2\u0526\u0527\3\2\2\2\u0527\u0528\3\2\2\2\u0528\u052a\5\u0106\u0084"+
		"\2\u0529\u0522\3\2\2\2\u0529\u052a\3\2\2\2\u052a\u052b\3\2\2\2\u052b\u052c"+
		"\5\4\3\2\u052c\u0097\3\2\2\2\u052d\u0541\5\u009aN\2\u052e\u0541\5\u009c"+
		"O\2\u052f\u0541\5\u009eP\2\u0530\u0541\5\u00a0Q\2\u0531\u0541\5\u00a8"+
		"U\2\u0532\u0541\5\u00c0a\2\u0533\u0541\5\u00aaV\2\u0534\u0541\5\u00b4"+
		"[\2\u0535\u0541\5\u00b8]\2\u0536\u0541\5\u00ba^\2\u0537\u0541\5\u00bc"+
		"_\2\u0538\u0541\5\u00fe\u0080\2\u0539\u0541\5\u00c2b\2\u053a\u0541\5\u00c4"+
		"c\2\u053b\u0541\5\u00a6T\2\u053c\u0541\5\u00be`\2\u053d\u0541\5\u00b2"+
		"Z\2\u053e\u0541\5\u00a4S\2\u053f\u0541\5\u00b0Y\2\u0540\u052d\3\2\2\2"+
		"\u0540\u052e\3\2\2\2\u0540\u052f\3\2\2\2\u0540\u0530\3\2\2\2\u0540\u0531"+
		"\3\2\2\2\u0540\u0532\3\2\2\2\u0540\u0533\3\2\2\2\u0540\u0534\3\2\2\2\u0540"+
		"\u0535\3\2\2\2\u0540\u0536\3\2\2\2\u0540\u0537\3\2\2\2\u0540\u0538\3\2"+
		"\2\2\u0540\u0539\3\2\2\2\u0540\u053a\3\2\2\2\u0540\u053b\3\2\2\2\u0540"+
		"\u053c\3\2\2\2\u0540\u053d\3\2\2\2\u0540\u053e\3\2\2\2\u0540\u053f\3\2"+
		"\2\2\u0541\u0099\3\2\2\2\u0542\u0543\5\4\3\2\u0543\u0544\7\35\2\2\u0544"+
		"\u0545\7a\2\2\u0545\u0547\5\u00c6d\2\u0546\u0548\7a\2\2\u0547\u0546\3"+
		"\2\2\2\u0547\u0548\3\2\2\2\u0548\u0549\3\2\2\2\u0549\u054b\7\31\2\2\u054a"+
		"\u054c\7a\2\2\u054b\u054a\3\2\2\2\u054b\u054c\3\2\2\2\u054c\u054d\3\2"+
		"\2\2\u054d\u054f\5\u0082B\2\u054e\u0550\7a\2\2\u054f\u054e\3\2\2\2\u054f"+
		"\u0550\3\2\2\2\u0550\u0551\3\2\2\2\u0551\u0553\7b\2\2\u0552\u0554\7a\2"+
		"\2\u0553\u0552\3\2\2\2\u0553\u0554\3\2\2\2\u0554\u0555\3\2\2\2\u0555\u0556"+
		"\5\u0082B\2\u0556\u0557\5\4\3\2\u0557\u009b\3\2\2\2\u0558\u0559\5\4\3"+
		"\2\u0559\u055b\7!\2\2\u055a\u055c\7a\2\2\u055b\u055a\3\2\2\2\u055b\u055c"+
		"\3\2\2\2\u055c\u055d\3\2\2\2\u055d\u055f\7\7\2\2\u055e\u0560\7a\2\2\u055f"+
		"\u055e\3\2\2\2\u055f\u0560\3\2\2\2\u0560\u0561\3\2\2\2\u0561\u0563\5\u0082"+
		"B\2\u0562\u0564\7a\2\2\u0563\u0562\3\2\2\2\u0563\u0564\3\2\2\2\u0564\u0565"+
		"\3\2\2\2\u0565\u0567\7\b\2\2\u0566\u0568\7a\2\2\u0567\u0566\3\2\2\2\u0567"+
		"\u0568\3\2\2\2\u0568\u0569\3\2\2\2\u0569\u056a\5\u0082B\2\u056a\u056b"+
		"\7a\2\2\u056b\u056c\7\"\2\2\u056c\u056d\7a\2\2\u056d\u056e\5\u0082B\2"+
		"\u056e\u056f\5\4\3\2\u056f\u009d\3\2\2\2\u0570\u0571\5\4\3\2\u0571\u0572"+
		"\7#\2\2\u0572\u0573\7a\2\2\u0573\u0574\5\u0082B\2\u0574\u0575\7a\2\2\u0575"+
		"\u0576\7$\2\2\u0576\u0577\7a\2\2\u0577\u0579\7\t\2\2\u0578\u057a\7a\2"+
		"\2\u0579\u0578\3\2\2\2\u0579\u057a\3\2\2\2\u057a\u057b\3\2\2\2\u057b\u057d"+
		"\5<\37\2\u057c\u057e\7a\2\2\u057d\u057c\3\2\2\2\u057d\u057e\3\2\2\2\u057e"+
		"\u057f\3\2\2\2\u057f\u0580\7\n\2\2\u0580\u0581\5\4\3\2\u0581\u009f\3\2"+
		"\2\2\u0582\u0583\5\4\3\2\u0583\u0584\7%\2\2\u0584\u0585\7a\2\2\u0585\u0587"+
		"\7\t\2\2\u0586\u0588\7a\2\2\u0587\u0586\3\2\2\2\u0587\u0588\3\2\2\2\u0588"+
		"\u0589\3\2\2\2\u0589\u058b\5@!\2\u058a\u058c\7a\2\2\u058b\u058a\3\2\2"+
		"\2\u058b\u058c\3\2\2\2\u058c\u058d\3\2\2\2\u058d\u058e\7\n\2\2\u058e\u058f"+
		"\5\4\3\2\u058f\u00a1\3\2\2\2\u0590\u059f\5\u0098M\2\u0591\u0593\7a\2\2"+
		"\u0592\u0591\3\2\2\2\u0592\u0593\3\2\2\2\u0593\u0594\3\2\2\2\u0594\u0596"+
		"\7\7\2\2\u0595\u0597\7a\2\2\u0596\u0595\3\2\2\2\u0596\u0597\3\2\2\2\u0597"+
		"\u0599\3\2\2\2\u0598\u059a\5\u0088E\2\u0599\u0598\3\2\2\2\u0599\u059a"+
		"\3\2\2\2\u059a\u059c\3\2\2\2\u059b\u059d\7a\2\2\u059c\u059b\3\2\2\2\u059c"+
		"\u059d\3\2\2\2\u059d\u059e\3\2\2\2\u059e\u05a0\7\b\2\2\u059f\u0592\3\2"+
		"\2\2\u059f\u05a0\3\2\2\2\u05a0\u05a1\3\2\2\2\u05a1\u05a2\5\4\3\2\u05a2"+
		"\u00a3\3\2\2\2\u05a3\u05a4\5\4\3\2\u05a4\u05a5\5&\24\2\u05a5\u05a6\5\4"+
		"\3\2\u05a6\u00a5\3\2\2\2\u05a7\u05a8\5\4\3\2\u05a8\u05a9\5\32\16\2\u05a9"+
		"\u05aa\5\4\3\2\u05aa\u00a7\3\2\2\2\u05ab\u05af\5\4\3\2\u05ac\u05ad\5$"+
		"\23\2\u05ad\u05ae\7\3\2\2\u05ae\u05b0\3\2\2\2\u05af\u05ac\3\2\2\2\u05af"+
		"\u05b0\3\2\2\2\u05b0\u05b1\3\2\2\2\u05b1\u05b6\5 \21\2\u05b2\u05b4\7a"+
		"\2\2\u05b3\u05b2\3\2\2\2\u05b3\u05b4\3\2\2\2\u05b4\u05b5\3\2\2\2\u05b5"+
		"\u05b7\5\u00aaV\2\u05b6\u05b3\3\2\2\2\u05b6\u05b7\3\2\2\2\u05b7\u05b8"+
		"\3\2\2\2\u05b8\u05b9\5\4\3\2\u05b9\u00a9\3\2\2\2\u05ba\u05bb\5\4\3\2\u05bb"+
		"\u05bd\7\7\2\2\u05bc\u05be\7a\2\2\u05bd\u05bc\3\2\2\2\u05bd\u05be\3\2"+
		"\2\2\u05be\u05c0\3\2\2\2\u05bf\u05c1\5\u0088E\2\u05c0\u05bf\3\2\2\2\u05c0"+
		"\u05c1\3\2\2\2\u05c1\u05c3\3\2\2\2\u05c2\u05c4\7a\2\2\u05c3\u05c2\3\2"+
		"\2\2\u05c3\u05c4\3\2\2\2\u05c4\u05c5\3\2\2\2\u05c5\u05c6\7\b\2\2\u05c6"+
		"\u05c7\5\4\3\2\u05c7\u00ab\3\2\2\2\u05c8\u05ca\5\u0082B\2\u05c9\u05cb"+
		"\7a\2\2\u05ca\u05c9\3\2\2\2\u05ca\u05cb\3\2\2\2\u05cb\u05cc\3\2\2\2\u05cc"+
		"\u05ce\7&\2\2\u05cd\u05cf\7a\2\2\u05ce\u05cd\3\2\2\2\u05ce\u05cf\3\2\2"+
		"\2\u05cf\u05d0\3\2\2\2\u05d0\u05d1\5\u0082B\2\u05d1\u00ad\3\2\2\2\u05d2"+
		"\u05dd\5\u00acW\2\u05d3\u05d5\7a\2\2\u05d4\u05d3\3\2\2\2\u05d4\u05d5\3"+
		"\2\2\2\u05d5\u05d6\3\2\2\2\u05d6\u05d8\7\5\2\2\u05d7\u05d9\7a\2\2\u05d8"+
		"\u05d7\3\2\2\2\u05d8\u05d9\3\2\2\2\u05d9\u05da\3\2\2\2\u05da\u05dc\5\u00ac"+
		"W\2\u05db\u05d4\3\2\2\2\u05dc\u05df\3\2\2\2\u05dd\u05db\3\2\2\2\u05dd"+
		"\u05de\3\2\2\2\u05de\u00af\3\2\2\2\u05df\u05dd\3\2\2\2\u05e0\u05e1\5\4"+
		"\3\2\u05e1\u05e2\7\'\2\2\u05e2\u05e3\5\4\3\2\u05e3\u00b1\3\2\2\2\u05e4"+
		"\u05e5\5\4\3\2\u05e5\u05e6\7g\2\2\u05e6\u05e7\5\4\3\2\u05e7\u00b3\3\2"+
		"\2\2\u05e8\u05e9\5\4\3\2\u05e9\u05ea\7f\2\2\u05ea\u05eb\5\4\3\2\u05eb"+
		"\u00b5\3\2\2\2\u05ec\u05f5\5\u00a2R\2\u05ed\u05ef\7a\2\2\u05ee\u05ed\3"+
		"\2\2\2\u05ee\u05ef\3\2\2\2\u05ef\u05f0\3\2\2\2\u05f0\u05f2\7(\2\2\u05f1"+
		"\u05f3\7a\2\2\u05f2\u05f1\3\2\2\2\u05f2\u05f3\3\2\2\2\u05f3\u05f4\3\2"+
		"\2\2\u05f4\u05f6\5\u0082B\2\u05f5\u05ee\3\2\2\2\u05f5\u05f6\3\2\2\2\u05f6"+
		"\u05f7\3\2\2\2\u05f7\u05f8\5\4\3\2\u05f8\u00b7\3\2\2\2\u05f9\u05fa\5\4"+
		"\3\2\u05fa\u05fc\7)\2\2\u05fb\u05fd\7a\2\2\u05fc\u05fb\3\2\2\2\u05fc\u05fd"+
		"\3\2\2\2\u05fd\u05ff\3\2\2\2\u05fe\u0600\5\u0088E\2\u05ff\u05fe\3\2\2"+
		"\2\u05ff\u0600\3\2\2\2\u0600\u0602\3\2\2\2\u0601\u0603\7a\2\2\u0602\u0601"+
		"\3\2\2\2\u0602\u0603\3\2\2\2\u0603\u0604\3\2\2\2\u0604\u0605\7\16\2\2"+
		"\u0605\u0606\5\4\3\2\u0606\u00b9\3\2\2\2\u0607\u0608\5\4\3\2\u0608\u060a"+
		"\7*\2\2\u0609\u060b\7a\2\2\u060a\u0609\3\2\2\2\u060a\u060b\3\2\2\2\u060b"+
		"\u060d\3\2\2\2\u060c\u060e\5\u0088E\2\u060d\u060c\3\2\2\2\u060d\u060e"+
		"\3\2\2\2\u060e\u0610\3\2\2\2\u060f\u0611\7a\2\2\u0610\u060f\3\2\2\2\u0610"+
		"\u0611\3\2\2\2\u0611\u0612\3\2\2\2\u0612\u0613\7\n\2\2\u0613\u0614\5\4"+
		"\3\2\u0614\u00bb\3\2\2\2\u0615\u0616\5\4\3\2\u0616\u0618\7+\2\2\u0617"+
		"\u0619\7a\2\2\u0618\u0617\3\2\2\2\u0618\u0619\3\2\2\2\u0619\u061b\3\2"+
		"\2\2\u061a\u061c\5\u00aeX\2\u061b\u061a\3\2\2\2\u061b\u061c\3\2\2\2\u061c"+
		"\u061e\3\2\2\2\u061d\u061f\7a\2\2\u061e\u061d\3\2\2\2\u061e\u061f\3\2"+
		"\2\2\u061f\u0620\3\2\2\2\u0620\u0621\7\n\2\2\u0621\u0622\5\4\3\2\u0622"+
		"\u00bd\3\2\2\2\u0623\u0624\5\4\3\2\u0624\u0626\5&\24\2\u0625\u0627\7a"+
		"\2\2\u0626\u0625\3\2\2\2\u0626\u0627\3\2\2\2\u0627\u0628\3\2\2\2\u0628"+
		"\u062a\7&\2\2\u0629\u062b\7a\2\2\u062a\u0629\3\2\2\2\u062a\u062b\3\2\2"+
		"\2\u062b\u062c\3\2\2\2\u062c\u062d\5\u0082B\2\u062d\u062e\5\4\3\2\u062e"+
		"\u00bf\3\2\2\2\u062f\u0630\5\4\3\2\u0630\u0632\7\7\2\2\u0631\u0633\7a"+
		"\2\2\u0632\u0631\3\2\2\2\u0632\u0633\3\2\2\2\u0633\u0634\3\2\2\2\u0634"+
		"\u0636\5(\25\2\u0635\u0637\7a\2\2\u0636\u0635\3\2\2\2\u0636\u0637\3\2"+
		"\2\2\u0637\u0638\3\2\2\2\u0638\u063a\7\b\2\2\u0639\u063b\7a\2\2\u063a"+
		"\u0639\3\2\2\2\u063a\u063b\3\2\2\2\u063b\u063c\3\2\2\2\u063c\u063e\7&"+
		"\2\2\u063d\u063f\7a\2\2\u063e\u063d\3\2\2\2\u063e\u063f\3\2\2\2\u063f"+
		"\u0640\3\2\2\2\u0640\u0641\5\u0082B\2\u0641\u0642\5\4\3\2\u0642\u00c1"+
		"\3\2\2\2\u0643\u0644\5\4\3\2\u0644\u0646\t\3\2\2\u0645\u0647\7a\2\2\u0646"+
		"\u0645\3\2\2\2\u0646\u0647\3\2\2\2\u0647\u0648\3\2\2\2\u0648\u064a\5."+
		"\30\2\u0649\u064b\7a\2\2\u064a\u0649\3\2\2\2\u064a\u064b\3\2\2\2\u064b"+
		"\u064c\3\2\2\2\u064c\u064e\7\3\2\2\u064d\u064f\7a\2\2\u064e\u064d\3\2"+
		"\2\2\u064e\u064f\3\2\2\2\u064f\u0650\3\2\2\2\u0650\u0651\5\u0082B\2\u0651"+
		"\u0652\5\4\3\2\u0652\u00c3\3\2\2\2\u0653\u0654\5\4\3\2\u0654\u0656\t\4"+
		"\2\2\u0655\u0657\7a\2\2\u0656\u0655\3\2\2\2\u0656\u0657\3\2\2\2\u0657"+
		"\u0658\3\2\2\2\u0658\u065a\5.\30\2\u0659\u065b\7a\2\2\u065a\u0659\3\2"+
		"\2\2\u065a\u065b\3\2\2\2\u065b\u065c\3\2\2\2\u065c\u065e\7\3\2\2\u065d"+
		"\u065f\7a\2\2\u065e\u065d\3\2\2\2\u065e\u065f\3\2\2\2\u065f\u0660\3\2"+
		"\2\2\u0660\u0661\5\u0082B\2\u0661\u0662\5\4\3\2\u0662\u00c5\3\2\2\2\u0663"+
		"\u066c\5\u00caf\2\u0664\u0666\7a\2\2\u0665\u0664\3\2\2\2\u0665\u0666\3"+
		"\2\2\2\u0666\u0667\3\2\2\2\u0667\u0669\7(\2\2\u0668\u066a\7a\2\2\u0669"+
		"\u0668\3\2\2\2\u0669\u066a\3\2\2\2\u066a\u066b\3\2\2\2\u066b\u066d\5\u00c6"+
		"d\2\u066c\u0665\3\2\2\2\u066c\u066d\3\2\2\2\u066d\u066e\3\2\2\2\u066e"+
		"\u066f\5\4\3\2\u066f\u00c7\3\2\2\2\u0670\u067b\5\u00c6d\2\u0671\u0673"+
		"\7a\2\2\u0672\u0671\3\2\2\2\u0672\u0673\3\2\2\2\u0673\u0674\3\2\2\2\u0674"+
		"\u0676\7\5\2\2\u0675\u0677\7a\2\2\u0676\u0675\3\2\2\2\u0676\u0677\3\2"+
		"\2\2\u0677\u0678\3\2\2\2\u0678\u067a\5\u00c6d\2\u0679\u0672\3\2\2\2\u067a"+
		"\u067d\3\2\2\2\u067b\u0679\3\2\2\2\u067b\u067c\3\2\2\2\u067c\u00c9\3\2"+
		"\2\2\u067d\u067b\3\2\2\2\u067e\u0688\5\u00d6l\2\u067f\u0688\5\u00fe\u0080"+
		"\2\u0680\u0688\5\u00d8m\2\u0681\u0688\7g\2\2\u0682\u0688\5\u00d0i\2\u0683"+
		"\u0688\5\u00d2j\2\u0684\u0688\5\u00dan\2\u0685\u0688\5\u00dco\2\u0686"+
		"\u0688\5\u00dep\2\u0687\u067e\3\2\2\2\u0687\u067f\3\2\2\2\u0687\u0680"+
		"\3\2\2\2\u0687\u0681\3\2\2\2\u0687\u0682\3\2\2\2\u0687\u0683\3\2\2\2\u0687"+
		"\u0684\3\2\2\2\u0687\u0685\3\2\2\2\u0687\u0686\3\2\2\2\u0688\u00cb\3\2"+
		"\2\2\u0689\u068b\5\u00c6d\2\u068a\u068c\7a\2\2\u068b\u068a\3\2\2\2\u068b"+
		"\u068c\3\2\2\2\u068c\u068d\3\2\2\2\u068d\u068f\7&\2\2\u068e\u0690\7a\2"+
		"\2\u068f\u068e\3\2\2\2\u068f\u0690\3\2\2\2\u0690\u0691\3\2\2\2\u0691\u0692"+
		"\5\u00c6d\2\u0692\u00cd\3\2\2\2\u0693\u069e\5\u00ccg\2\u0694\u0696\7a"+
		"\2\2\u0695\u0694\3\2\2\2\u0695\u0696\3\2\2\2\u0696\u0697\3\2\2\2\u0697"+
		"\u0699\7\5\2\2\u0698\u069a\7a\2\2\u0699\u0698\3\2\2\2\u0699\u069a\3\2"+
		"\2\2\u069a\u069b\3\2\2\2\u069b\u069d\5\u00ccg\2\u069c\u0695\3\2\2\2\u069d"+
		"\u06a0\3\2\2\2\u069e\u069c\3\2\2\2\u069e\u069f\3\2\2\2\u069f\u00cf\3\2"+
		"\2\2\u06a0\u069e\3\2\2\2\u06a1\u06a5\5\4\3\2\u06a2\u06a3\5$\23\2\u06a3"+
		"\u06a4\7\3\2\2\u06a4\u06a6\3\2\2\2\u06a5\u06a2\3\2\2\2\u06a5\u06a6\3\2"+
		"\2\2\u06a6\u06a7\3\2\2\2\u06a7\u06ac\5 \21\2\u06a8\u06aa\7a\2\2\u06a9"+
		"\u06a8\3\2\2\2\u06a9\u06aa\3\2\2\2\u06aa\u06ab\3\2\2\2\u06ab\u06ad\5\u00c6"+
		"d\2\u06ac\u06a9\3\2\2\2\u06ac\u06ad\3\2\2\2\u06ad\u06ae\3\2\2\2\u06ae"+
		"\u06af\5\4\3\2\u06af\u00d1\3\2\2\2\u06b0\u06b1\5\4\3\2\u06b1\u06b3\7\7"+
		"\2\2\u06b2\u06b4\7a\2\2\u06b3\u06b2\3\2\2\2\u06b3\u06b4\3\2\2\2\u06b4"+
		"\u06b6\3\2\2\2\u06b5\u06b7\5\u00c8e\2\u06b6\u06b5\3\2\2\2\u06b6\u06b7"+
		"\3\2\2\2\u06b7\u06b9\3\2\2\2\u06b8\u06ba\7a\2\2\u06b9\u06b8\3\2\2\2\u06b9"+
		"\u06ba\3\2\2\2\u06ba\u06bb\3\2\2\2\u06bb\u06bc\7\b\2\2\u06bc\u06bd\5\4"+
		"\3\2\u06bd\u00d3\3\2\2\2\u06be\u06bf\5\4\3\2\u06bf\u06c0\7g\2\2\u06c0"+
		"\u06c1\5\4\3\2\u06c1\u00d5\3\2\2\2\u06c2\u06c3\5\4\3\2\u06c3\u06c4\7f"+
		"\2\2\u06c4\u06c5\5\4\3\2\u06c5\u00d7\3\2\2\2\u06c6\u06c7\5\4\3\2\u06c7"+
		"\u06c8\5\n\6\2\u06c8\u06c9\5\4\3\2\u06c9\u00d9\3\2\2\2\u06ca\u06cb\5\4"+
		"\3\2\u06cb\u06cd\7)\2\2\u06cc\u06ce\7a\2\2\u06cd\u06cc\3\2\2\2\u06cd\u06ce"+
		"\3\2\2\2\u06ce\u06d0\3\2\2\2\u06cf\u06d1\5\u00c8e\2\u06d0\u06cf\3\2\2"+
		"\2\u06d0\u06d1\3\2\2\2\u06d1\u06da\3\2\2\2\u06d2\u06d4\7a\2\2\u06d3\u06d2"+
		"\3\2\2\2\u06d3\u06d4\3\2\2\2\u06d4\u06d5\3\2\2\2\u06d5\u06d7\7\5\2\2\u06d6"+
		"\u06d8\7a\2\2\u06d7\u06d6\3\2\2\2\u06d7\u06d8\3\2\2\2\u06d8\u06d9\3\2"+
		"\2\2\u06d9\u06db\7\60\2\2\u06da\u06d3\3\2\2\2\u06da\u06db\3\2\2\2\u06db"+
		"\u06dd\3\2\2\2\u06dc\u06de\7a\2\2\u06dd\u06dc\3\2\2\2\u06dd\u06de\3\2"+
		"\2\2\u06de\u06df\3\2\2\2\u06df\u06e0\7\16\2\2\u06e0\u06e1\5\4\3\2\u06e1"+
		"\u00db\3\2\2\2\u06e2\u06e3\5\4\3\2\u06e3\u06e5\7*\2\2\u06e4\u06e6\7a\2"+
		"\2\u06e5\u06e4\3\2\2\2\u06e5\u06e6\3\2\2\2\u06e6\u06e8\3\2\2\2\u06e7\u06e9"+
		"\5\u00c8e\2\u06e8\u06e7\3\2\2\2\u06e8\u06e9\3\2\2\2\u06e9\u06f2\3\2\2"+
		"\2\u06ea\u06ec\7a\2\2\u06eb\u06ea\3\2\2\2\u06eb\u06ec\3\2\2\2\u06ec\u06ed"+
		"\3\2\2\2\u06ed\u06ef\7\5\2\2\u06ee\u06f0\7a\2\2\u06ef\u06ee\3\2\2\2\u06ef"+
		"\u06f0\3\2\2\2\u06f0\u06f1\3\2\2\2\u06f1\u06f3\7\60\2\2\u06f2\u06eb\3"+
		"\2\2\2\u06f2\u06f3\3\2\2\2\u06f3\u06f5\3\2\2\2\u06f4\u06f6\7a\2\2\u06f5"+
		"\u06f4\3\2\2\2\u06f5\u06f6\3\2\2\2\u06f6\u06f7\3\2\2\2\u06f7\u06f8\7\n"+
		"\2\2\u06f8\u06f9\5\4\3\2\u06f9\u00dd\3\2\2\2\u06fa\u06fb\5\4\3\2\u06fb"+
		"\u06fd\7+\2\2\u06fc\u06fe\7a\2\2\u06fd\u06fc\3\2\2\2\u06fd\u06fe\3\2\2"+
		"\2\u06fe\u0700\3\2\2\2\u06ff\u0701\5\u00ceh\2\u0700\u06ff\3\2\2\2\u0700"+
		"\u0701\3\2\2\2\u0701\u070a\3\2\2\2\u0702\u0704\7a\2\2\u0703\u0702\3\2"+
		"\2\2\u0703\u0704\3\2\2\2\u0704\u0705\3\2\2\2\u0705\u0707\7\5\2\2\u0706"+
		"\u0708\7a\2\2\u0707\u0706\3\2\2\2\u0707\u0708\3\2\2\2\u0708\u0709\3\2"+
		"\2\2\u0709\u070b\7\60\2\2\u070a\u0703\3\2\2\2\u070a\u070b\3\2\2\2\u070b"+
		"\u070d\3\2\2\2\u070c\u070e\7a\2\2\u070d\u070c\3\2\2\2\u070d\u070e\3\2"+
		"\2\2\u070e\u070f\3\2\2\2\u070f\u0710\7\n\2\2\u0710\u0711\5\4\3\2\u0711"+
		"\u00df\3\2\2\2\u0712\u0713\5\4\3\2\u0713\u0714\t\5\2\2\u0714\u0715\5\4"+
		"\3\2\u0715\u00e1\3\2\2\2\u0716\u0717\5\4\3\2\u0717\u0718\7\63\2\2\u0718"+
		"\u0719\13\2\2\2\u0719\u071a\7\63\2\2\u071a\u071b\5\4\3\2\u071b\u00e3\3"+
		"\2\2\2\u071c\u071d\5\4\3\2\u071d\u0722\7\64\2\2\u071e\u0721\7\65\2\2\u071f"+
		"\u0721\n\6\2\2\u0720\u071e\3\2\2\2\u0720\u071f\3\2\2\2\u0721\u0724\3\2"+
		"\2\2\u0722\u0720\3\2\2\2\u0722\u0723\3\2\2\2\u0723\u0725\3\2\2\2\u0724"+
		"\u0722\3\2\2\2\u0725\u0726\7\64\2\2\u0726\u0727\5\4\3\2\u0727\u00e5\3"+
		"\2\2\2\u0728\u0729\78\2\2\u0729\u00e7\3\2\2\2\u072a\u072c\5\4\3\2\u072b"+
		"\u072d\5\u00e6t\2\u072c\u072b\3\2\2\2\u072c\u072d\3\2\2\2\u072d\u072e"+
		"\3\2\2\2\u072e\u072f\7h\2\2\u072f\u0730\7\3\2\2\u0730\u0731\7h\2\2\u0731"+
		"\u0732\79\2\2\u0732\u0733\5\4\3\2\u0733\u00e9\3\2\2\2\u0734\u0736\5\4"+
		"\3\2\u0735\u0737\5\u00e6t\2\u0736\u0735\3\2\2\2\u0736\u0737\3\2\2\2\u0737"+
		"\u0738\3\2\2\2\u0738\u0739\7h\2\2\u0739\u073a\7\3\2\2\u073a\u073b\7h\2"+
		"\2\u073b\u073c\7:\2\2\u073c\u073d\5\4\3\2\u073d\u00eb\3\2\2\2\u073e\u0740"+
		"\5\4\3\2\u073f\u0741\5\u00e6t\2\u0740\u073f\3\2\2\2\u0740\u0741\3\2\2"+
		"\2\u0741\u0742\3\2\2\2\u0742\u0743\7h\2\2\u0743\u0744\7\3\2\2\u0744\u0745"+
		"\7h\2\2\u0745\u0746\5\4\3\2\u0746\u00ed\3\2\2\2\u0747\u074b\5\u00e8u\2"+
		"\u0748\u074b\5\u00eav\2\u0749\u074b\5\u00ecw\2\u074a\u0747\3\2\2\2\u074a"+
		"\u0748\3\2\2\2\u074a\u0749\3\2\2\2\u074b\u00ef\3\2\2\2\u074c\u074e\5\4"+
		"\3\2\u074d\u074f\5\u00e6t\2\u074e\u074d\3\2\2\2\u074e\u074f\3\2\2\2\u074f"+
		"\u0750\3\2\2\2\u0750\u0751\7h\2\2\u0751\u0752\7;\2\2\u0752\u0753\5\4\3"+
		"\2\u0753\u00f1\3\2\2\2\u0754\u0756\5\4\3\2\u0755\u0757\5\u00e6t\2\u0756"+
		"\u0755\3\2\2\2\u0756\u0757\3\2\2\2\u0757\u0758\3\2\2\2\u0758\u0759\7h"+
		"\2\2\u0759\u075a\7<\2\2\u075a\u075b\5\4\3\2\u075b\u00f3\3\2\2\2\u075c"+
		"\u075e\5\4\3\2\u075d\u075f\5\u00e6t\2\u075e\u075d\3\2\2\2\u075e\u075f"+
		"\3\2\2\2\u075f\u0760\3\2\2\2\u0760\u0761\7h\2\2\u0761\u0762\7=\2\2\u0762"+
		"\u0763\5\4\3\2\u0763\u00f5\3\2\2\2\u0764\u0766\5\4\3\2\u0765\u0767\5\u00e6"+
		"t\2\u0766\u0765\3\2\2\2\u0766\u0767\3\2\2\2\u0767\u0768\3\2\2\2\u0768"+
		"\u0769\7h\2\2\u0769\u076a\7>\2\2\u076a\u076b\5\4\3\2\u076b\u00f7\3\2\2"+
		"\2\u076c\u076e\5\4\3\2\u076d\u076f\5\u00e6t\2\u076e\u076d\3\2\2\2\u076e"+
		"\u076f\3\2\2\2\u076f\u0770\3\2\2\2\u0770\u0771\7h\2\2\u0771\u0772\7?\2"+
		"\2\u0772\u0773\5\4\3\2\u0773\u00f9\3\2\2\2\u0774\u0776\5\4\3\2\u0775\u0777"+
		"\5\u00e6t\2\u0776\u0775\3\2\2\2\u0776\u0777\3\2\2\2\u0777\u0778\3\2\2"+
		"\2\u0778\u0779\7h\2\2\u0779\u077a\5\4\3\2\u077a\u00fb\3\2\2\2\u077b\u0782"+
		"\5\u00f0y\2\u077c\u0782\5\u00f2z\2\u077d\u0782\5\u00f4{\2\u077e\u0782"+
		"\5\u00f6|\2\u077f\u0782\5\u00f8}\2\u0780\u0782\5\u00fa~\2\u0781\u077b"+
		"\3\2\2\2\u0781\u077c\3\2\2\2\u0781\u077d\3\2\2\2\u0781\u077e\3\2\2\2\u0781"+
		"\u077f\3\2\2\2\u0781\u0780\3\2\2\2\u0782\u00fd\3\2\2\2\u0783\u0789\5\4"+
		"\3\2\u0784\u078a\5\u00e0q\2\u0785\u078a\5\u00e2r\2\u0786\u078a\5\u00ee"+
		"x\2\u0787\u078a\5\u00fc\177\2\u0788\u078a\5\u00e4s\2\u0789\u0784\3\2\2"+
		"\2\u0789\u0785\3\2\2\2\u0789\u0786\3\2\2\2\u0789\u0787\3\2\2\2\u0789\u0788"+
		"\3\2\2\2\u078a\u078b\3\2\2\2\u078b\u078c\5\4\3\2\u078c\u00ff\3\2\2\2\u078d"+
		"\u0793\5\u0108\u0085\2\u078e\u0793\5\u0110\u0089\2\u078f\u0793\5\u0112"+
		"\u008a\2\u0790\u0793\5\u0102\u0082\2\u0791\u0793\5\u0104\u0083\2\u0792"+
		"\u078d\3\2\2\2\u0792\u078e\3\2\2\2\u0792\u078f\3\2\2\2\u0792\u0790\3\2"+
		"\2\2\u0792\u0791\3\2\2\2\u0793\u0101\3\2\2\2\u0794\u0795\5\4\3\2\u0795"+
		"\u0796\5&\24\2\u0796\u0797\5\4\3\2\u0797\u0103\3\2\2\2\u0798\u0799\5\4"+
		"\3\2\u0799\u079a\5$\23\2\u079a\u079b\5\4\3\2\u079b\u0105\3\2\2\2\u079c"+
		"\u079d\5\4\3\2\u079d\u07a6\5\u0100\u0081\2\u079e\u07a0\7a\2\2\u079f\u079e"+
		"\3\2\2\2\u079f\u07a0\3\2\2\2\u07a0\u07a1\3\2\2\2\u07a1\u07a3\7&\2\2\u07a2"+
		"\u07a4\7a\2\2\u07a3\u07a2\3\2\2\2\u07a3\u07a4\3\2\2\2\u07a4\u07a5\3\2"+
		"\2\2\u07a5\u07a7\5\u0106\u0084\2\u07a6\u079f\3\2\2\2\u07a6\u07a7\3\2\2"+
		"\2\u07a7\u07a8\3\2\2\2\u07a8\u07a9\5\4\3\2\u07a9\u0107\3\2\2\2\u07aa\u07ab"+
		"\5\4\3\2\u07ab\u07ad\7\7\2\2\u07ac\u07ae\7a\2\2\u07ad\u07ac\3\2\2\2\u07ad"+
		"\u07ae\3\2\2\2\u07ae\u07af\3\2\2\2\u07af\u07ba\5\u0106\u0084\2\u07b0\u07b2"+
		"\7a\2\2\u07b1\u07b0\3\2\2\2\u07b1\u07b2\3\2\2\2\u07b2\u07b3\3\2\2\2\u07b3"+
		"\u07b5\7\5\2\2\u07b4\u07b6\7a\2\2\u07b5\u07b4\3\2\2\2\u07b5\u07b6\3\2"+
		"\2\2\u07b6\u07b7\3\2\2\2\u07b7\u07b9\5\u0106\u0084\2\u07b8\u07b1\3\2\2"+
		"\2\u07b9\u07bc\3\2\2\2\u07ba\u07b8\3\2\2\2\u07ba\u07bb\3\2\2\2\u07bb\u07be"+
		"\3\2\2\2\u07bc\u07ba\3\2\2\2\u07bd\u07bf\7a\2\2\u07be\u07bd\3\2\2\2\u07be"+
		"\u07bf\3\2\2\2\u07bf\u07c0\3\2\2\2\u07c0\u07c2\7\b\2\2\u07c1\u07c3\7a"+
		"\2\2\u07c2\u07c1\3\2\2\2\u07c2\u07c3\3\2\2\2\u07c3\u07c4\3\2\2\2\u07c4"+
		"\u07c6\7&\2\2\u07c5\u07c7\7a\2\2\u07c6\u07c5\3\2\2\2\u07c6\u07c7\3\2\2"+
		"\2\u07c7\u07c8\3\2\2\2\u07c8\u07c9\5\u0106\u0084\2\u07c9\u07ca\5\4\3\2"+
		"\u07ca\u0109\3\2\2\2\u07cb\u07cc\5\4\3\2\u07cc\u07cd\7\7\2\2\u07cd\u07ce"+
		"\7\b\2\2\u07ce\u07cf\5\4\3\2\u07cf\u010b\3\2\2\2\u07d0\u07d2\7\7\2\2\u07d1"+
		"\u07d3\7a\2\2\u07d2\u07d1\3\2\2\2\u07d2\u07d3\3\2\2\2\u07d3\u07d4\3\2"+
		"\2\2\u07d4\u07d6\5\u0106\u0084\2\u07d5\u07d7\7a\2\2\u07d6\u07d5\3\2\2"+
		"\2\u07d6\u07d7\3\2\2\2\u07d7\u07d8\3\2\2\2\u07d8\u07d9\7\b\2\2\u07d9\u010d"+
		"\3\2\2\2\u07da\u07db\5\4\3\2\u07db\u07dd\7\7\2\2\u07dc\u07de\7a\2\2\u07dd"+
		"\u07dc\3\2\2\2\u07dd\u07de\3\2\2\2\u07de\u07df\3\2\2\2\u07df\u07e8\5\u0106"+
		"\u0084\2\u07e0\u07e2\7a\2\2\u07e1\u07e0\3\2\2\2\u07e1\u07e2\3\2\2\2\u07e2"+
		"\u07e3\3\2\2\2\u07e3\u07e5\7\5\2\2\u07e4\u07e6\7a\2\2\u07e5\u07e4\3\2"+
		"\2\2\u07e5\u07e6\3\2\2\2\u07e6\u07e7\3\2\2\2\u07e7\u07e9\5\u0106\u0084"+
		"\2\u07e8\u07e1\3\2\2\2\u07e9\u07ea\3\2\2\2\u07ea\u07e8\3\2\2\2\u07ea\u07eb"+
		"\3\2\2\2\u07eb\u07ed\3\2\2\2\u07ec\u07ee\7a\2\2\u07ed\u07ec\3\2\2\2\u07ed"+
		"\u07ee\3\2\2\2\u07ee\u07ef\3\2\2\2\u07ef\u07f0\7\b\2\2\u07f0\u07f1\5\4"+
		"\3\2\u07f1\u010f\3\2\2\2\u07f2\u07f6\5\u010a\u0086\2\u07f3\u07f6\5\u010c"+
		"\u0087\2\u07f4\u07f6\5\u010e\u0088\2\u07f5\u07f2\3\2\2\2\u07f5\u07f3\3"+
		"\2\2\2\u07f5\u07f4\3\2\2\2\u07f6\u0111\3\2\2\2\u07f7\u07f8\5\4\3\2\u07f8"+
		"\u07fa\5\u0104\u0083\2\u07f9\u07fb\7a\2\2\u07fa\u07f9\3\2\2\2\u07fa\u07fb"+
		"\3\2\2\2\u07fb\u07fc\3\2\2\2\u07fc\u07fe\7\r\2\2\u07fd\u07ff\7a\2\2\u07fe"+
		"\u07fd\3\2\2\2\u07fe\u07ff\3\2\2\2\u07ff\u0800\3\2\2\2\u0800\u080b\5\u0106"+
		"\u0084\2\u0801\u0803\7a\2\2\u0802\u0801\3\2\2\2\u0802\u0803\3\2\2\2\u0803"+
		"\u0804\3\2\2\2\u0804\u0806\7\5\2\2\u0805\u0807\7a\2\2\u0806\u0805\3\2"+
		"\2\2\u0806\u0807\3\2\2\2\u0807\u0808\3\2\2\2\u0808\u080a\5\u0106\u0084"+
		"\2\u0809\u0802\3\2\2\2\u080a\u080d\3\2\2\2\u080b\u0809\3\2\2\2\u080b\u080c"+
		"\3\2\2\2\u080c\u080f\3\2\2\2\u080d\u080b\3\2\2\2\u080e\u0810\7a\2\2\u080f"+
		"\u080e\3\2\2\2\u080f\u0810\3\2\2\2\u0810\u0811\3\2\2\2\u0811\u0813\7\16"+
		"\2\2\u0812\u0814\7a\2\2\u0813\u0812\3\2\2\2\u0813\u0814\3\2\2\2\u0814"+
		"\u0815\3\2\2\2\u0815\u0816\5\4\3\2\u0816\u0113\3\2\2\2\u0817\u0818\t\7"+
		"\2\2\u0818\u0115\3\2\2\2\u0819\u081a\t\b\2\2\u081a\u0117\3\2\2\2\u081b"+
		"\u081c\t\t\2\2\u081c\u0119\3\2\2\2\u081d\u081e\t\n\2\2\u081e\u011b\3\2"+
		"\2\2\u081f\u0820\t\13\2\2\u0820\u011d\3\2\2\2\u0821\u0822\t\f\2\2\u0822"+
		"\u011f\3\2\2\2\u0823\u082a\5\u0124\u0093\2\u0824\u082a\5\u0126\u0094\2"+
		"\u0825\u082a\5\u0128\u0095\2\u0826\u082a\5\u012c\u0097\2\u0827\u082a\5"+
		"\u012a\u0096\2\u0828\u082a\5\u012e\u0098\2\u0829\u0823\3\2\2\2\u0829\u0824"+
		"\3\2\2\2\u0829\u0825\3\2\2\2\u0829\u0826\3\2\2\2\u0829\u0827\3\2\2\2\u0829"+
		"\u0828\3\2\2\2\u082a\u0121\3\2\2\2\u082b\u0836\5\u0120\u0091\2\u082c\u082e"+
		"\7a\2\2\u082d\u082c\3\2\2\2\u082d\u082e\3\2\2\2\u082e\u082f\3\2\2\2\u082f"+
		"\u0831\7\5\2\2\u0830\u0832\7a\2\2\u0831\u0830\3\2\2\2\u0831\u0832\3\2"+
		"\2\2\u0832\u0833\3\2\2\2\u0833\u0835\5\u0120\u0091\2\u0834\u082d\3\2\2"+
		"\2\u0835\u0838\3\2\2\2\u0836\u0834\3\2\2\2\u0836\u0837\3\2\2\2\u0837\u0123"+
		"\3\2\2\2\u0838\u0836\3\2\2\2\u0839\u083a\5\4\3\2\u083a\u083b\7\61\2\2"+
		"\u083b\u083c\5\4\3\2\u083c\u0125\3\2\2\2\u083d\u083e\5\4\3\2\u083e\u083f"+
		"\7\62\2\2\u083f\u0840\5\4\3\2\u0840\u0127\3\2\2\2\u0841\u0842\5\4\3\2"+
		"\u0842\u0844\5\32\16\2\u0843\u0845\7a\2\2\u0844\u0843\3\2\2\2\u0844\u0845"+
		"\3\2\2\2\u0845\u0846\3\2\2\2\u0846\u0847\7\7\2\2\u0847\u0848\5\u0088E"+
		"\2\u0848\u0849\7\b\2\2\u0849\u084a\5\4\3\2\u084a\u0129\3\2\2\2\u084b\u084c"+
		"\5\4\3\2\u084c\u084e\5\36\20\2\u084d\u084f\7a\2\2\u084e\u084d\3\2\2\2"+
		"\u084e\u084f\3\2\2\2\u084f\u0850\3\2\2\2\u0850\u0851\7\7\2\2\u0851\u0852"+
		"\5\u0088E\2\u0852\u0853\7\b\2\2\u0853\u0854\5\4\3\2\u0854\u012b\3\2\2"+
		"\2\u0855\u0856\5\4\3\2\u0856\u0858\5&\24\2\u0857\u0859\7a\2\2\u0858\u0857"+
		"\3\2\2\2\u0858\u0859\3\2\2\2\u0859\u085a\3\2\2\2\u085a\u085c\7U\2\2\u085b"+
		"\u085d\7a\2\2\u085c\u085b\3\2\2\2\u085c\u085d\3\2\2\2\u085d\u085e\3\2"+
		"\2\2\u085e\u085f\5&\24\2\u085f\u0860\5\4\3\2\u0860\u012d\3\2\2\2\u0861"+
		"\u0862\5\4\3\2\u0862\u0864\5&\24\2\u0863\u0865\7a\2\2\u0864\u0863\3\2"+
		"\2\2\u0864\u0865\3\2\2\2\u0865\u0866\3\2\2\2\u0866\u0868\7_\2\2\u0867"+
		"\u0869\7a\2\2\u0868\u0867\3\2\2\2\u0868\u0869\3\2\2\2\u0869\u086a\3\2"+
		"\2\2\u086a\u086b\5\u0082B\2\u086b\u086c\5\4\3\2\u086c\u012f\3\2\2\2\u014c"+
		"\u0139\u013f\u0143\u0148\u014b\u0157\u0160\u0169\u0186\u018a\u0192\u0199"+
		"\u019d\u01a2\u01a7\u01aa\u01ad\u01b0\u01b5\u01b9\u01c0\u01c4\u01c9\u01ce"+
		"\u01d2\u01d6\u01db\u01de\u01e1\u01e7\u01eb\u01f0\u01f5\u01f9\u01fe\u0205"+
		"\u0209\u020d\u0211\u0216\u021d\u0221\u0225\u0229\u022e\u0234\u0238\u023b"+
		"\u0241\u0245\u0249\u024e\u0253\u0258\u025c\u0261\u026d\u0271\u0276\u027a"+
		"\u027e\u0282\u0284\u0290\u0294\u0299\u02c1\u02c4\u02cb\u02cf\u02d4\u02d8"+
		"\u02df\u02e4\u02e8\u02f0\u02f4\u02f8\u0300\u0304\u0309\u0311\u0316\u031b"+
		"\u031f\u0326\u032a\u032d\u0330\u0337\u033c\u0340\u0347\u034b\u034e\u0351"+
		"\u0358\u035f\u0363\u0366\u0369\u0370\u0375\u0379\u0380\u0384\u0388\u038f"+
		"\u0394\u0398\u039d\u03a3\u03a7\u03ab\u03b2\u03b7\u03bb\u03be\u03c1\u03c8"+
		"\u03cd\u03d1\u03d5\u03d9\u03e0\u03e5\u03e9\u03f0\u03f4\u03f8\u03fc\u0400"+
		"\u0404\u040b\u0410\u0414\u041c\u0420\u0427\u042c\u042f\u0434\u0439\u043f"+
		"\u0444\u0448\u044c\u0454\u0459\u045e\u0462\u0466\u046a\u0471\u0476\u047a"+
		"\u0482\u0486\u048d\u0492\u0496\u049e\u04a2\u04a6\u04a9\u04ad\u04b1\u04b5"+
		"\u04bb\u04bf\u04c4\u04c9\u04cd\u04d1\u04d7\u04db\u04e1\u04e8\u04ec\u04f2"+
		"\u04f9\u04ff\u0503\u0509\u050d\u0511\u0518\u051e\u0522\u0526\u0529\u0540"+
		"\u0547\u054b\u054f\u0553\u055b\u055f\u0563\u0567\u0579\u057d\u0587\u058b"+
		"\u0592\u0596\u0599\u059c\u059f\u05af\u05b3\u05b6\u05bd\u05c0\u05c3\u05ca"+
		"\u05ce\u05d4\u05d8\u05dd\u05ee\u05f2\u05f5\u05fc\u05ff\u0602\u060a\u060d"+
		"\u0610\u0618\u061b\u061e\u0626\u062a\u0632\u0636\u063a\u063e\u0646\u064a"+
		"\u064e\u0656\u065a\u065e\u0665\u0669\u066c\u0672\u0676\u067b\u0687\u068b"+
		"\u068f\u0695\u0699\u069e\u06a5\u06a9\u06ac\u06b3\u06b6\u06b9\u06cd\u06d0"+
		"\u06d3\u06d7\u06da\u06dd\u06e5\u06e8\u06eb\u06ef\u06f2\u06f5\u06fd\u0700"+
		"\u0703\u0707\u070a\u070d\u0720\u0722\u072c\u0736\u0740\u074a\u074e\u0756"+
		"\u075e\u0766\u076e\u0776\u0781\u0789\u0792\u079f\u07a3\u07a6\u07ad\u07b1"+
		"\u07b5\u07ba\u07be\u07c2\u07c6\u07d2\u07d6\u07dd\u07e1\u07e5\u07ea\u07ed"+
		"\u07f5\u07fa\u07fe\u0802\u0806\u080b\u080f\u0813\u0829\u082d\u0831\u0836"+
		"\u0844\u084e\u0858\u085c\u0864\u0868";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}