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
		T__66=67, T__67=68, T__68=69, T__69=70, TripleSlashComment=71, WS=72, 
		SC=73, Comment=74, ENUM=75, NAMESPACE=76, REL=77, LAT=78, INDEX=79, DEF=80, 
		EXTERNAL=81, LAW=82, CLASS=83, LET=84, IMPL=85, FNIL=86, SWITCH=87, MATCH=88, 
		WITH=89, WILD=90, CASE=91, IF=92, ELSE=93, IMPORT=94, LowerIdent=95, UpperIdent=96, 
		Chars=97, Strs=98, Digits=99;
	public static final int
		RULE_tscomment = 0, RULE_start = 1, RULE_optSC = 2, RULE_ident = 3, RULE_nname = 4, 
		RULE_lowerqname = 5, RULE_upperqname = 6, RULE_annotationName = 7, RULE_attributeName = 8, 
		RULE_className = 9, RULE_definitionName = 10, RULE_qualifiedDefinitionName = 11, 
		RULE_tableName = 12, RULE_qualifiedTableName = 13, RULE_tagName = 14, 
		RULE_typeName = 15, RULE_qualifiedTypeName = 16, RULE_variableName = 17, 
		RULE_variableNames = 18, RULE_argument = 19, RULE_arguments = 20, RULE_formalparams = 21, 
		RULE_attribute = 22, RULE_attributes = 23, RULE_index = 24, RULE_indexes = 25, 
		RULE_match_rule = 26, RULE_match_rules = 27, RULE_switch_rule = 28, RULE_switch_rules = 29, 
		RULE_typeparam = 30, RULE_typeparams = 31, RULE_class_typeparams = 32, 
		RULE_contextBound = 33, RULE_contextBounds = 34, RULE_contextBoundsList = 35, 
		RULE_implContextBoundsList = 36, RULE_annotation = 37, RULE_annotations = 38, 
		RULE_s_import = 39, RULE_import_wildcard = 40, RULE_import_definition = 41, 
		RULE_import_namespace = 42, RULE_decl = 43, RULE_decls_namespace = 44, 
		RULE_decls_enum = 45, RULE_dcases = 46, RULE_dcase = 47, RULE_decls_relation = 48, 
		RULE_decls_lattice = 49, RULE_decls_index = 50, RULE_decls_signature = 51, 
		RULE_decls_external = 52, RULE_decls_definition = 53, RULE_decls_law = 54, 
		RULE_decls_class = 55, RULE_class_body = 56, RULE_class_decl = 57, RULE_decls_fact = 58, 
		RULE_decls_rule = 59, RULE_elms = 60, RULE_decls_letlattice = 61, RULE_decls_impl = 62, 
		RULE_decls_impl_body = 63, RULE_expression = 64, RULE_logical = 65, RULE_expressions = 66, 
		RULE_comparison = 67, RULE_additive = 68, RULE_multiplicative = 69, RULE_infix = 70, 
		RULE_extended = 71, RULE_unary = 72, RULE_ascribe = 73, RULE_e_primary = 74, 
		RULE_e_letMatch = 75, RULE_e_ifThenElse = 76, RULE_e_match = 77, RULE_e_switch = 78, 
		RULE_e_apply = 79, RULE_e_unaryLambda = 80, RULE_e_lambda = 81, RULE_e_literal = 82, 
		RULE_e_sname = 83, RULE_e_qname = 84, RULE_e_tag = 85, RULE_e_tuple = 86, 
		RULE_e_keyValue = 87, RULE_e_keyValues = 88, RULE_e_userError = 89, RULE_e_wild = 90, 
		RULE_e_fNil = 91, RULE_e_fList = 92, RULE_e_fVec = 93, RULE_e_fSet = 94, 
		RULE_e_fMap = 95, RULE_existential = 96, RULE_universal = 97, RULE_pattern = 98, 
		RULE_patterns = 99, RULE_simple = 100, RULE_p_keyValue = 101, RULE_p_keyValues = 102, 
		RULE_p_literal = 103, RULE_p_tag = 104, RULE_p_tuple = 105, RULE_p_wild = 106, 
		RULE_p_fNil = 107, RULE_p_variable = 108, RULE_p_fList = 109, RULE_p_fVec = 110, 
		RULE_p_fSet = 111, RULE_p_fMap = 112, RULE_bools = 113, RULE_chars = 114, 
		RULE_strs = 115, RULE_negative = 116, RULE_float32 = 117, RULE_float64 = 118, 
		RULE_floatDefault = 119, RULE_floats = 120, RULE_int8 = 121, RULE_int16 = 122, 
		RULE_int32 = 123, RULE_int64 = 124, RULE_bigInt = 125, RULE_intDefault = 126, 
		RULE_ints = 127, RULE_literal = 128, RULE_primary = 129, RULE_var = 130, 
		RULE_ref = 131, RULE_type = 132, RULE_arrow = 133, RULE_tuple_unit = 134, 
		RULE_tuple_singleton = 135, RULE_tuple_multi = 136, RULE_tuple = 137, 
		RULE_apply = 138, RULE_unary_ops = 139, RULE_logical_ops = 140, RULE_comparison_ops = 141, 
		RULE_multipve_ops = 142, RULE_addve_ops = 143, RULE_extbin_ops = 144, 
		RULE_predicate = 145, RULE_predicates = 146, RULE_pred_true = 147, RULE_pred_false = 148, 
		RULE_pred_filter = 149, RULE_pred_table = 150, RULE_pred_notequal = 151, 
		RULE_pred_loop = 152;
	public static final String[] ruleNames = {
		"tscomment", "start", "optSC", "ident", "nname", "lowerqname", "upperqname", 
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
		"WS", "SC", "Comment", "ENUM", "NAMESPACE", "REL", "LAT", "INDEX", "DEF", 
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
			setState(306);
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
		enterRule(_localctx, 2, RULE_start);
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
		enterRule(_localctx, 4, RULE_optSC);
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
		enterRule(_localctx, 6, RULE_ident);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(331);
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
		enterRule(_localctx, 8, RULE_nname);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(333);
			ident();
			setState(338);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__0) {
				{
				{
				setState(334);
				match(T__0);
				setState(335);
				ident();
				}
				}
				setState(340);
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
		enterRule(_localctx, 10, RULE_lowerqname);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(344);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,6,_ctx) ) {
			case 1:
				{
				setState(341);
				nname();
				setState(342);
				match(T__1);
				}
				break;
			}
			setState(346);
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
		enterRule(_localctx, 12, RULE_upperqname);
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
		enterRule(_localctx, 14, RULE_annotationName);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(355);
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
		enterRule(_localctx, 16, RULE_attributeName);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(357);
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
		enterRule(_localctx, 18, RULE_className);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(359);
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
		enterRule(_localctx, 20, RULE_definitionName);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(361);
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
		enterRule(_localctx, 22, RULE_qualifiedDefinitionName);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(363);
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
		enterRule(_localctx, 24, RULE_tableName);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(365);
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
		enterRule(_localctx, 26, RULE_qualifiedTableName);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(367);
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
		enterRule(_localctx, 28, RULE_tagName);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(369);
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
		enterRule(_localctx, 30, RULE_typeName);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(371);
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
		enterRule(_localctx, 32, RULE_qualifiedTypeName);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(373);
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
		enterRule(_localctx, 34, RULE_variableName);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(375);
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
		enterRule(_localctx, 36, RULE_variableNames);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(377);
			variableName();
			setState(388);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,10,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(379);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(378);
						match(WS);
						}
					}

					setState(381);
					match(T__2);
					setState(383);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(382);
						match(WS);
						}
					}

					setState(385);
					variableName();
					}
					} 
				}
				setState(390);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,10,_ctx);
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
		enterRule(_localctx, 38, RULE_argument);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(391);
			variableName();
			setState(392);
			match(T__3);
			setState(394);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(393);
				match(WS);
				}
			}

			setState(396);
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitArguments(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ArgumentsContext arguments() throws RecognitionException {
		ArgumentsContext _localctx = new ArgumentsContext(_ctx, getState());
		enterRule(_localctx, 40, RULE_arguments);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(398);
			argument();
			setState(409);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,14,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(400);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(399);
						match(WS);
						}
					}

					setState(402);
					match(T__2);
					setState(404);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(403);
						match(WS);
						}
					}

					setState(406);
					argument();
					}
					} 
				}
				setState(411);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,14,_ctx);
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
		enterRule(_localctx, 42, RULE_formalparams);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(423);
			_la = _input.LA(1);
			if (_la==T__4) {
				{
				setState(412);
				match(T__4);
				setState(414);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,15,_ctx) ) {
				case 1:
					{
					setState(413);
					match(WS);
					}
					break;
				}
				setState(417);
				_la = _input.LA(1);
				if (_la==LowerIdent) {
					{
					setState(416);
					arguments();
					}
				}

				setState(420);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(419);
					match(WS);
					}
				}

				setState(422);
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
		enterRule(_localctx, 44, RULE_attribute);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(425);
			attributeName();
			setState(427);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(426);
				match(WS);
				}
			}

			setState(429);
			match(T__3);
			setState(431);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(430);
				match(WS);
				}
			}

			setState(433);
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitAttributes(this);
			else return visitor.visitChildren(this);
		}
	}

	public final AttributesContext attributes() throws RecognitionException {
		AttributesContext _localctx = new AttributesContext(_ctx, getState());
		enterRule(_localctx, 46, RULE_attributes);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(435);
			attribute();
			setState(446);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,23,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(437);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(436);
						match(WS);
						}
					}

					setState(439);
					match(T__2);
					setState(441);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(440);
						match(WS);
						}
					}

					setState(443);
					attribute();
					}
					} 
				}
				setState(448);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,23,_ctx);
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
		enterRule(_localctx, 48, RULE_index);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(449);
			match(T__6);
			setState(451);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,24,_ctx) ) {
			case 1:
				{
				setState(450);
				match(WS);
				}
				break;
			}
			setState(467);
			_la = _input.LA(1);
			if (_la==LowerIdent) {
				{
				setState(453);
				attributeName();
				setState(464);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,27,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(455);
						_la = _input.LA(1);
						if (_la==WS) {
							{
							setState(454);
							match(WS);
							}
						}

						setState(457);
						match(T__2);
						setState(459);
						_la = _input.LA(1);
						if (_la==WS) {
							{
							setState(458);
							match(WS);
							}
						}

						setState(461);
						attributeName();
						}
						} 
					}
					setState(466);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,27,_ctx);
				}
				}
			}

			setState(470);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(469);
				match(WS);
				}
			}

			setState(472);
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
		enterRule(_localctx, 50, RULE_indexes);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(474);
			index();
			setState(485);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,32,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(476);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(475);
						match(WS);
						}
					}

					setState(478);
					match(T__2);
					setState(480);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(479);
						match(WS);
						}
					}

					setState(482);
					index();
					}
					} 
				}
				setState(487);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,32,_ctx);
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
		enterRule(_localctx, 52, RULE_match_rule);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(488);
			match(CASE);
			setState(489);
			match(WS);
			setState(490);
			pattern();
			setState(492);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(491);
				match(WS);
				}
			}

			setState(494);
			match(T__8);
			setState(496);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,34,_ctx) ) {
			case 1:
				{
				setState(495);
				match(WS);
				}
				break;
			}
			setState(498);
			expression();
			setState(500);
			_la = _input.LA(1);
			if (_la==SC) {
				{
				setState(499);
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
		enterRule(_localctx, 54, RULE_match_rules);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(502);
			match_rule();
			setState(509);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,37,_ctx);
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
					match_rule();
					}
					} 
				}
				setState(511);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,37,_ctx);
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
		enterRule(_localctx, 56, RULE_switch_rule);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(512);
			match(CASE);
			setState(513);
			match(WS);
			setState(514);
			expression();
			setState(516);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(515);
				match(WS);
				}
			}

			setState(518);
			match(T__8);
			setState(520);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,39,_ctx) ) {
			case 1:
				{
				setState(519);
				match(WS);
				}
				break;
			}
			setState(522);
			expression();
			setState(524);
			_la = _input.LA(1);
			if (_la==SC) {
				{
				setState(523);
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
		enterRule(_localctx, 58, RULE_switch_rules);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(526);
			switch_rule();
			setState(533);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,42,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(528);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(527);
						match(WS);
						}
					}

					setState(530);
					switch_rule();
					}
					} 
				}
				setState(535);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,42,_ctx);
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
		enterRule(_localctx, 60, RULE_typeparam);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(536);
			variableName();
			setState(545);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,45,_ctx) ) {
			case 1:
				{
				setState(538);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(537);
					match(WS);
					}
				}

				setState(540);
				match(T__3);
				setState(542);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(541);
					match(WS);
					}
				}

				setState(544);
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitTypeparams(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TypeparamsContext typeparams() throws RecognitionException {
		TypeparamsContext _localctx = new TypeparamsContext(_ctx, getState());
		enterRule(_localctx, 62, RULE_typeparams);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(567);
			_la = _input.LA(1);
			if (_la==T__9) {
				{
				setState(547);
				match(T__9);
				setState(549);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(548);
					match(WS);
					}
				}

				setState(551);
				typeparam();
				setState(562);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==T__2 || _la==WS) {
					{
					{
					setState(553);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(552);
						match(WS);
						}
					}

					setState(555);
					match(T__2);
					setState(557);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(556);
						match(WS);
						}
					}

					setState(559);
					typeparam();
					}
					}
					setState(564);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(565);
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
		enterRule(_localctx, 64, RULE_class_typeparams);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(569);
			match(T__9);
			setState(570);
			type();
			setState(581);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__2 || _la==WS) {
				{
				{
				setState(572);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(571);
					match(WS);
					}
				}

				setState(574);
				match(T__2);
				setState(576);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(575);
					match(WS);
					}
				}

				setState(578);
				type();
				}
				}
				setState(583);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(584);
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
		enterRule(_localctx, 66, RULE_contextBound);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(586);
			className();
			setState(587);
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
		enterRule(_localctx, 68, RULE_contextBounds);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(589);
			contextBound();
			setState(600);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,56,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(591);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(590);
						match(WS);
						}
					}

					setState(593);
					match(T__2);
					setState(595);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(594);
						match(WS);
						}
					}

					setState(597);
					contextBound();
					}
					} 
				}
				setState(602);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,56,_ctx);
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
		enterRule(_localctx, 70, RULE_contextBoundsList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(611);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,59,_ctx) ) {
			case 1:
				{
				setState(604);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(603);
					match(WS);
					}
				}

				setState(606);
				match(T__8);
				setState(608);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(607);
					match(WS);
					}
				}

				setState(610);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
		enterRule(_localctx, 72, RULE_implContextBoundsList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(621);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,62,_ctx) ) {
			case 1:
				{
				setState(614);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(613);
					match(WS);
					}
				}

				setState(616);
				match(T__11);
				setState(618);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(617);
					match(WS);
					}
				}

				setState(620);
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
		enterRule(_localctx, 74, RULE_annotation);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(623);
			match(T__12);
			setState(624);
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
		enterRule(_localctx, 76, RULE_annotations);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(626);
			annotation();
			setState(631);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,63,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(627);
					match(WS);
					setState(628);
					annotation();
					}
					} 
				}
				setState(633);
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
		enterRule(_localctx, 78, RULE_s_import);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(635);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(634);
				match(WS);
				}
			}

			setState(640);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,65,_ctx) ) {
			case 1:
				{
				setState(637);
				import_wildcard();
				}
				break;
			case 2:
				{
				setState(638);
				import_definition();
				}
				break;
			case 3:
				{
				setState(639);
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
		public TerminalNode WS() { return getToken(FlixParser.WS, 0); }
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
		enterRule(_localctx, 80, RULE_import_wildcard);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(642);
			match(IMPORT);
			setState(643);
			match(WS);
			setState(644);
			nname();
			setState(645);
			match(T__1);
			setState(646);
			match(WILD);
			setState(647);
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
		enterRule(_localctx, 82, RULE_import_definition);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(649);
			match(IMPORT);
			setState(650);
			match(WS);
			setState(651);
			nname();
			setState(652);
			match(T__1);
			setState(653);
			ident();
			setState(654);
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
		enterRule(_localctx, 84, RULE_import_namespace);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(656);
			match(IMPORT);
			setState(657);
			match(WS);
			setState(658);
			nname();
			setState(659);
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
		enterRule(_localctx, 86, RULE_decl);
		try {
			setState(675);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,66,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(661);
				decls_namespace();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(662);
				decls_enum();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(663);
				decls_relation();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(664);
				decls_lattice();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(665);
				decls_index();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(666);
				decls_signature();
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(667);
				decls_external();
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(668);
				decls_definition();
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(669);
				decls_law();
				}
				break;
			case 10:
				enterOuterAlt(_localctx, 10);
				{
				setState(670);
				decls_class();
				}
				break;
			case 11:
				enterOuterAlt(_localctx, 11);
				{
				setState(671);
				decls_fact();
				}
				break;
			case 12:
				enterOuterAlt(_localctx, 12);
				{
				setState(672);
				decls_rule();
				}
				break;
			case 13:
				enterOuterAlt(_localctx, 13);
				{
				setState(673);
				decls_impl();
				}
				break;
			case 14:
				enterOuterAlt(_localctx, 14);
				{
				setState(674);
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
		enterRule(_localctx, 88, RULE_decls_namespace);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(678);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(677);
				match(WS);
				}
			}

			setState(680);
			match(NAMESPACE);
			setState(681);
			match(WS);
			setState(682);
			nname();
			setState(684);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(683);
				match(WS);
				}
			}

			setState(686);
			match(T__6);
			setState(688);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,69,_ctx) ) {
			case 1:
				{
				setState(687);
				match(WS);
				}
				break;
			}
			setState(693);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,70,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(690);
					decl();
					}
					} 
				}
				setState(695);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,70,_ctx);
			}
			setState(697);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(696);
				match(WS);
				}
			}

			setState(699);
			match(T__7);
			setState(700);
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
		enterRule(_localctx, 90, RULE_decls_enum);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(708);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,73,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(703);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(702);
						match(WS);
						}
					}

					setState(705);
					tscomment();
					}
					} 
				}
				setState(710);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,73,_ctx);
			}
			setState(712);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(711);
				match(WS);
				}
			}

			setState(714);
			match(ENUM);
			setState(715);
			match(WS);
			setState(716);
			typeName();
			setState(717);
			typeparams();
			setState(719);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(718);
				match(WS);
				}
			}

			setState(721);
			match(T__6);
			setState(723);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(722);
				match(WS);
				}
			}

			setState(725);
			dcases();
			setState(727);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(726);
				match(WS);
				}
			}

			setState(729);
			match(T__7);
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
		enterRule(_localctx, 92, RULE_dcases);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(732);
			dcase();
			setState(743);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,80,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(734);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(733);
						match(WS);
						}
					}

					setState(736);
					match(T__2);
					setState(738);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(737);
						match(WS);
						}
					}

					setState(740);
					dcase();
					}
					} 
				}
				setState(745);
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
		public TerminalNode CASE() { return getToken(FlixParser.CASE, 0); }
		public TerminalNode WS() { return getToken(FlixParser.WS, 0); }
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
		enterRule(_localctx, 94, RULE_dcase);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(746);
			match(CASE);
			setState(747);
			match(WS);
			setState(748);
			tagName();
			setState(750);
			_la = _input.LA(1);
			if (_la==T__4) {
				{
				setState(749);
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
		enterRule(_localctx, 96, RULE_decls_relation);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(758);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,83,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(753);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(752);
						match(WS);
						}
					}

					setState(755);
					tscomment();
					}
					} 
				}
				setState(760);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,83,_ctx);
			}
			setState(762);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(761);
				match(WS);
				}
			}

			setState(764);
			match(REL);
			setState(765);
			match(WS);
			setState(766);
			tableName();
			setState(768);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(767);
				match(WS);
				}
			}

			setState(770);
			match(T__4);
			setState(772);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,86,_ctx) ) {
			case 1:
				{
				setState(771);
				match(WS);
				}
				break;
			}
			setState(775);
			_la = _input.LA(1);
			if (_la==LowerIdent) {
				{
				setState(774);
				attributes();
				}
			}

			setState(778);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(777);
				match(WS);
				}
			}

			setState(780);
			match(T__5);
			setState(781);
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
		enterRule(_localctx, 98, RULE_decls_lattice);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(789);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,90,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(784);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(783);
						match(WS);
						}
					}

					setState(786);
					tscomment();
					}
					} 
				}
				setState(791);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,90,_ctx);
			}
			setState(793);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(792);
				match(WS);
				}
			}

			setState(795);
			match(LAT);
			setState(796);
			match(WS);
			setState(797);
			tableName();
			setState(799);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(798);
				match(WS);
				}
			}

			setState(801);
			match(T__4);
			setState(803);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,93,_ctx) ) {
			case 1:
				{
				setState(802);
				match(WS);
				}
				break;
			}
			setState(806);
			_la = _input.LA(1);
			if (_la==LowerIdent) {
				{
				setState(805);
				attributes();
				}
			}

			setState(809);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(808);
				match(WS);
				}
			}

			setState(811);
			match(T__5);
			setState(812);
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
		enterRule(_localctx, 100, RULE_decls_index);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(815);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(814);
				match(WS);
				}
			}

			setState(817);
			match(INDEX);
			setState(818);
			match(WS);
			setState(819);
			qualifiedTableName();
			setState(821);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(820);
				match(WS);
				}
			}

			setState(823);
			match(T__4);
			setState(825);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,98,_ctx) ) {
			case 1:
				{
				setState(824);
				match(WS);
				}
				break;
			}
			setState(828);
			_la = _input.LA(1);
			if (_la==T__6) {
				{
				setState(827);
				indexes();
				}
			}

			setState(831);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(830);
				match(WS);
				}
			}

			setState(833);
			match(T__5);
			setState(834);
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
		enterRule(_localctx, 102, RULE_decls_signature);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(842);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,102,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(837);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(836);
						match(WS);
						}
					}

					setState(839);
					tscomment();
					}
					} 
				}
				setState(844);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,102,_ctx);
			}
			setState(846);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(845);
				match(WS);
				}
			}

			setState(848);
			match(DEF);
			setState(849);
			match(WS);
			setState(850);
			definitionName();
			setState(852);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,104,_ctx) ) {
			case 1:
				{
				setState(851);
				match(WS);
				}
				break;
			}
			setState(854);
			formalparams();
			setState(856);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(855);
				match(WS);
				}
			}

			setState(858);
			match(T__3);
			setState(860);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(859);
				match(WS);
				}
			}

			setState(862);
			type();
			setState(863);
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
		enterRule(_localctx, 104, RULE_decls_external);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(871);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,108,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(866);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(865);
						match(WS);
						}
					}

					setState(868);
					tscomment();
					}
					} 
				}
				setState(873);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,108,_ctx);
			}
			setState(875);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(874);
				match(WS);
				}
			}

			setState(877);
			match(EXTERNAL);
			setState(879);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(878);
				match(WS);
				}
			}

			setState(881);
			match(DEF);
			setState(882);
			match(WS);
			setState(883);
			definitionName();
			setState(885);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,111,_ctx) ) {
			case 1:
				{
				setState(884);
				match(WS);
				}
				break;
			}
			setState(887);
			formalparams();
			setState(889);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(888);
				match(WS);
				}
			}

			setState(891);
			match(T__3);
			setState(893);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(892);
				match(WS);
				}
			}

			setState(895);
			type();
			setState(896);
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
		enterRule(_localctx, 106, RULE_decls_definition);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(904);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,115,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
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
					tscomment();
					}
					} 
				}
				setState(906);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,115,_ctx);
			}
			setState(908);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,116,_ctx) ) {
			case 1:
				{
				setState(907);
				match(WS);
				}
				break;
			}
			setState(911);
			_la = _input.LA(1);
			if (_la==T__12) {
				{
				setState(910);
				annotations();
				}
			}

			setState(914);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(913);
				match(WS);
				}
			}

			setState(916);
			match(DEF);
			setState(917);
			match(WS);
			setState(918);
			definitionName();
			setState(920);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,119,_ctx) ) {
			case 1:
				{
				setState(919);
				match(WS);
				}
				break;
			}
			setState(922);
			typeparams();
			setState(923);
			formalparams();
			setState(925);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(924);
				match(WS);
				}
			}

			setState(927);
			match(T__3);
			setState(929);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(928);
				match(WS);
				}
			}

			setState(931);
			type();
			setState(933);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(932);
				match(WS);
				}
			}

			setState(935);
			match(T__13);
			setState(937);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,123,_ctx) ) {
			case 1:
				{
				setState(936);
				match(WS);
				}
				break;
			}
			setState(939);
			expression();
			setState(940);
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
		enterRule(_localctx, 108, RULE_decls_law);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(948);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,125,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(943);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(942);
						match(WS);
						}
					}

					setState(945);
					tscomment();
					}
					} 
				}
				setState(950);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,125,_ctx);
			}
			setState(952);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(951);
				match(WS);
				}
			}

			setState(954);
			match(LAW);
			setState(955);
			match(WS);
			setState(956);
			definitionName();
			setState(958);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,127,_ctx) ) {
			case 1:
				{
				setState(957);
				match(WS);
				}
				break;
			}
			setState(960);
			typeparams();
			setState(962);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,128,_ctx) ) {
			case 1:
				{
				setState(961);
				match(WS);
				}
				break;
			}
			setState(964);
			formalparams();
			setState(966);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(965);
				match(WS);
				}
			}

			setState(968);
			match(T__3);
			setState(970);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(969);
				match(WS);
				}
			}

			setState(972);
			type();
			setState(974);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(973);
				match(WS);
				}
			}

			setState(976);
			match(T__13);
			setState(978);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,132,_ctx) ) {
			case 1:
				{
				setState(977);
				match(WS);
				}
				break;
			}
			setState(980);
			expression();
			setState(981);
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
		enterRule(_localctx, 110, RULE_decls_class);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(989);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,134,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(984);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(983);
						match(WS);
						}
					}

					setState(986);
					tscomment();
					}
					} 
				}
				setState(991);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,134,_ctx);
			}
			setState(993);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(992);
				match(WS);
				}
			}

			setState(995);
			match(CLASS);
			setState(996);
			match(WS);
			setState(997);
			className();
			setState(998);
			class_typeparams();
			setState(999);
			contextBoundsList();
			setState(1001);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1000);
				match(WS);
				}
			}

			setState(1003);
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
		enterRule(_localctx, 112, RULE_class_body);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1005);
			match(T__6);
			setState(1007);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,137,_ctx) ) {
			case 1:
				{
				setState(1006);
				match(WS);
				}
				break;
			}
			setState(1015);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__12 || ((((_la - 71)) & ~0x3f) == 0 && ((1L << (_la - 71)) & ((1L << (TripleSlashComment - 71)) | (1L << (WS - 71)) | (1L << (DEF - 71)) | (1L << (LAW - 71)))) != 0)) {
				{
				{
				setState(1009);
				class_decl();
				setState(1011);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,138,_ctx) ) {
				case 1:
					{
					setState(1010);
					match(WS);
					}
					break;
				}
				}
				}
				setState(1017);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(1018);
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
		enterRule(_localctx, 114, RULE_class_decl);
		try {
			setState(1023);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,140,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1020);
				decls_definition();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1021);
				decls_signature();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1022);
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
			setState(1026);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1025);
				match(WS);
				}
			}

			setState(1028);
			predicate();
			setState(1030);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1029);
				match(WS);
				}
			}

			setState(1032);
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
			setState(1035);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1034);
				match(WS);
				}
			}

			setState(1037);
			predicate();
			setState(1039);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1038);
				match(WS);
				}
			}

			setState(1041);
			match(T__14);
			setState(1043);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1042);
				match(WS);
				}
			}

			setState(1045);
			predicates();
			setState(1047);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1046);
				match(WS);
				}
			}

			setState(1049);
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
		enterRule(_localctx, 120, RULE_elms);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1051);
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
			setState(1054);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1053);
				match(WS);
				}
			}

			setState(1056);
			match(LET);
			setState(1058);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1057);
				match(WS);
				}
			}

			setState(1060);
			type();
			setState(1061);
			match(T__15);
			setState(1063);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1062);
				match(WS);
				}
			}

			setState(1065);
			match(T__13);
			setState(1067);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1066);
				match(WS);
				}
			}

			setState(1069);
			match(T__4);
			setState(1071);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,151,_ctx) ) {
			case 1:
				{
				setState(1070);
				match(WS);
				}
				break;
			}
			setState(1073);
			elms();
			setState(1075);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1074);
				match(WS);
				}
			}

			setState(1077);
			match(T__5);
			setState(1078);
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
			setState(1086);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,154,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1081);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1080);
						match(WS);
						}
					}

					setState(1083);
					tscomment();
					}
					} 
				}
				setState(1088);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,154,_ctx);
			}
			setState(1090);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1089);
				match(WS);
				}
			}

			setState(1092);
			match(IMPL);
			setState(1093);
			match(WS);
			setState(1094);
			className();
			setState(1095);
			class_typeparams();
			setState(1097);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,156,_ctx) ) {
			case 1:
				{
				setState(1096);
				match(WS);
				}
				break;
			}
			setState(1099);
			implContextBoundsList();
			setState(1101);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1100);
				match(WS);
				}
			}

			setState(1103);
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
			setState(1105);
			match(T__6);
			setState(1107);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,158,_ctx) ) {
			case 1:
				{
				setState(1106);
				match(WS);
				}
				break;
			}
			setState(1112);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,159,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1109);
					decls_definition();
					}
					} 
				}
				setState(1114);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,159,_ctx);
			}
			setState(1116);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1115);
				match(WS);
				}
			}

			setState(1118);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
		enterRule(_localctx, 128, RULE_expression);
		int _la;
		try {
			setState(1133);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,164,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				{
				setState(1120);
				match(T__6);
				setState(1122);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,161,_ctx) ) {
				case 1:
					{
					setState(1121);
					match(WS);
					}
					break;
				}
				setState(1124);
				expression();
				setState(1126);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1125);
					match(WS);
					}
				}

				setState(1128);
				match(T__7);
				setState(1130);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,163,_ctx) ) {
				case 1:
					{
					setState(1129);
					match(WS);
					}
					break;
				}
				}
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1132);
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
		enterRule(_localctx, 130, RULE_logical);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1135);
			comparison();
			setState(1145);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,167,_ctx) ) {
			case 1:
				{
				setState(1137);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1136);
					match(WS);
					}
				}

				setState(1139);
				logical_ops();
				setState(1141);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,166,_ctx) ) {
				case 1:
					{
					setState(1140);
					match(WS);
					}
					break;
				}
				setState(1143);
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitExpressions(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ExpressionsContext expressions() throws RecognitionException {
		ExpressionsContext _localctx = new ExpressionsContext(_ctx, getState());
		enterRule(_localctx, 132, RULE_expressions);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1147);
			expression();
			setState(1158);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,170,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1149);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1148);
						match(WS);
						}
					}

					setState(1151);
					match(T__2);
					setState(1153);
					_errHandler.sync(this);
					switch ( getInterpreter().adaptivePredict(_input,169,_ctx) ) {
					case 1:
						{
						setState(1152);
						match(WS);
						}
						break;
					}
					setState(1155);
					expression();
					}
					} 
				}
				setState(1160);
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
		enterRule(_localctx, 134, RULE_comparison);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1161);
			additive();
			setState(1171);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,173,_ctx) ) {
			case 1:
				{
				setState(1163);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1162);
					match(WS);
					}
				}

				setState(1165);
				comparison_ops();
				setState(1167);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,172,_ctx) ) {
				case 1:
					{
					setState(1166);
					match(WS);
					}
					break;
				}
				setState(1169);
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
		public MultiplicativeContext multiplicative() {
			return getRuleContext(MultiplicativeContext.class,0);
		}
		public Addve_opsContext addve_ops() {
			return getRuleContext(Addve_opsContext.class,0);
		}
		public AdditiveContext additive() {
			return getRuleContext(AdditiveContext.class,0);
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
		enterRule(_localctx, 136, RULE_additive);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1173);
			multiplicative();
			setState(1183);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,176,_ctx) ) {
			case 1:
				{
				setState(1175);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1174);
					match(WS);
					}
				}

				setState(1177);
				addve_ops();
				setState(1179);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,175,_ctx) ) {
				case 1:
					{
					setState(1178);
					match(WS);
					}
					break;
				}
				setState(1181);
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

	public static class MultiplicativeContext extends ParserRuleContext {
		public InfixContext infix() {
			return getRuleContext(InfixContext.class,0);
		}
		public Multipve_opsContext multipve_ops() {
			return getRuleContext(Multipve_opsContext.class,0);
		}
		public MultiplicativeContext multiplicative() {
			return getRuleContext(MultiplicativeContext.class,0);
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
		enterRule(_localctx, 138, RULE_multiplicative);
		int _la;
		try {
			setState(1197);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,179,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1185);
				if (!(!( _input.LT(2).getText().equals("/") && //Make sure we aren't accessing a namespace
										Character.isLetter(_input.LT(3).getText().charAt(0)) ))) throw new FailedPredicateException(this, "!( _input.LT(2).getText().equals(\"/\") && //Make sure we aren't accessing a namespace\n\t\t\t\t\t\tCharacter.isLetter(_input.LT(3).getText().charAt(0)) )");
				setState(1186);
				infix();
				setState(1188);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1187);
					match(WS);
					}
				}

				setState(1190);
				multipve_ops();
				setState(1192);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,178,_ctx) ) {
				case 1:
					{
					setState(1191);
					match(WS);
					}
					break;
				}
				setState(1194);
				multiplicative();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1196);
				infix();
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
		enterRule(_localctx, 140, RULE_infix);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1199);
			extended();
			setState(1211);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,182,_ctx) ) {
			case 1:
				{
				setState(1201);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1200);
					match(WS);
					}
				}

				setState(1203);
				match(T__16);
				setState(1204);
				qualifiedDefinitionName();
				setState(1205);
				match(T__16);
				setState(1207);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,181,_ctx) ) {
				case 1:
					{
					setState(1206);
					match(WS);
					}
					break;
				}
				setState(1209);
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitExtended(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ExtendedContext extended() throws RecognitionException {
		ExtendedContext _localctx = new ExtendedContext(_ctx, getState());
		enterRule(_localctx, 142, RULE_extended);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1213);
			unary();
			setState(1223);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,185,_ctx) ) {
			case 1:
				{
				setState(1215);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1214);
					match(WS);
					}
				}

				setState(1217);
				extbin_ops();
				setState(1219);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,184,_ctx) ) {
				case 1:
					{
					setState(1218);
					match(WS);
					}
					break;
				}
				setState(1221);
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitUnary(this);
			else return visitor.visitChildren(this);
		}
	}

	public final UnaryContext unary() throws RecognitionException {
		UnaryContext _localctx = new UnaryContext(_ctx, getState());
		enterRule(_localctx, 144, RULE_unary);
		try {
			setState(1233);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,187,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1225);
				if (!(!( _input.LT(1).getText().equals("-") && //Make sure this isn't just a negative number
						Character.isDigit(_input.LT(2).getText().charAt(0)) ))) throw new FailedPredicateException(this, "!( _input.LT(1).getText().equals(\"-\") && //Make sure this isn't just a negative number\n\t\tCharacter.isDigit(_input.LT(2).getText().charAt(0)) )");
				{
				setState(1226);
				unary_ops();
				setState(1228);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,186,_ctx) ) {
				case 1:
					{
					setState(1227);
					match(WS);
					}
					break;
				}
				setState(1230);
				unary();
				}
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1232);
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitAscribe(this);
			else return visitor.visitChildren(this);
		}
	}

	public final AscribeContext ascribe() throws RecognitionException {
		AscribeContext _localctx = new AscribeContext(_ctx, getState());
		enterRule(_localctx, 146, RULE_ascribe);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1235);
			e_fList();
			setState(1244);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,190,_ctx) ) {
			case 1:
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
				match(T__3);
				setState(1241);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1240);
					match(WS);
					}
				}

				setState(1243);
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
		enterRule(_localctx, 148, RULE_e_primary);
		try {
			setState(1265);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,191,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1246);
				e_letMatch();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1247);
				e_ifThenElse();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1248);
				e_match();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(1249);
				e_switch();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(1250);
				e_tag();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(1251);
				e_lambda();
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(1252);
				e_tuple();
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(1253);
				e_fNil();
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(1254);
				e_fVec();
				}
				break;
			case 10:
				enterOuterAlt(_localctx, 10);
				{
				setState(1255);
				e_fSet();
				}
				break;
			case 11:
				enterOuterAlt(_localctx, 11);
				{
				setState(1256);
				e_fMap();
				}
				break;
			case 12:
				enterOuterAlt(_localctx, 12);
				{
				setState(1257);
				e_literal();
				}
				break;
			case 13:
				enterOuterAlt(_localctx, 13);
				{
				setState(1258);
				existential();
				}
				break;
			case 14:
				enterOuterAlt(_localctx, 14);
				{
				setState(1259);
				universal();
				}
				break;
			case 15:
				enterOuterAlt(_localctx, 15);
				{
				setState(1260);
				e_qname();
				}
				break;
			case 16:
				enterOuterAlt(_localctx, 16);
				{
				setState(1261);
				e_unaryLambda();
				}
				break;
			case 17:
				enterOuterAlt(_localctx, 17);
				{
				setState(1262);
				e_wild();
				}
				break;
			case 18:
				enterOuterAlt(_localctx, 18);
				{
				setState(1263);
				e_sname();
				}
				break;
			case 19:
				enterOuterAlt(_localctx, 19);
				{
				setState(1264);
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
		enterRule(_localctx, 150, RULE_e_letMatch);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1267);
			match(LET);
			setState(1268);
			match(WS);
			setState(1269);
			pattern();
			setState(1271);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1270);
				match(WS);
				}
			}

			setState(1273);
			match(T__13);
			setState(1275);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,193,_ctx) ) {
			case 1:
				{
				setState(1274);
				match(WS);
				}
				break;
			}
			setState(1277);
			expression();
			setState(1279);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1278);
				match(WS);
				}
			}

			setState(1281);
			match(SC);
			setState(1283);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,195,_ctx) ) {
			case 1:
				{
				setState(1282);
				match(WS);
				}
				break;
			}
			setState(1285);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
		enterRule(_localctx, 152, RULE_e_ifThenElse);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1287);
			match(IF);
			setState(1289);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1288);
				match(WS);
				}
			}

			setState(1291);
			match(T__4);
			setState(1293);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,197,_ctx) ) {
			case 1:
				{
				setState(1292);
				match(WS);
				}
				break;
			}
			setState(1295);
			expression();
			setState(1297);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1296);
				match(WS);
				}
			}

			setState(1299);
			match(T__5);
			setState(1301);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,199,_ctx) ) {
			case 1:
				{
				setState(1300);
				match(WS);
				}
				break;
			}
			setState(1303);
			expression();
			setState(1304);
			match(WS);
			setState(1305);
			match(ELSE);
			setState(1306);
			match(WS);
			setState(1307);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
		enterRule(_localctx, 154, RULE_e_match);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1309);
			match(MATCH);
			setState(1310);
			match(WS);
			setState(1311);
			expression();
			setState(1312);
			match(WS);
			setState(1313);
			match(WITH);
			setState(1314);
			match(WS);
			setState(1315);
			match(T__6);
			setState(1317);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1316);
				match(WS);
				}
			}

			setState(1319);
			match_rules();
			setState(1321);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1320);
				match(WS);
				}
			}

			setState(1323);
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
		enterRule(_localctx, 156, RULE_e_switch);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1325);
			match(SWITCH);
			setState(1326);
			match(WS);
			setState(1327);
			match(T__6);
			setState(1329);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1328);
				match(WS);
				}
			}

			setState(1331);
			switch_rules();
			setState(1333);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1332);
				match(WS);
				}
			}

			setState(1335);
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
		enterRule(_localctx, 158, RULE_e_apply);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1337);
			e_primary();
			setState(1352);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,208,_ctx) ) {
			case 1:
				{
				setState(1339);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1338);
					match(WS);
					}
				}

				setState(1341);
				match(T__4);
				setState(1343);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,205,_ctx) ) {
				case 1:
					{
					setState(1342);
					match(WS);
					}
					break;
				}
				setState(1346);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,206,_ctx) ) {
				case 1:
					{
					setState(1345);
					expressions();
					}
					break;
				}
				setState(1349);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1348);
					match(WS);
					}
				}

				setState(1351);
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
		enterRule(_localctx, 160, RULE_e_unaryLambda);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1354);
			variableName();
			setState(1356);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1355);
				match(WS);
				}
			}

			setState(1358);
			match(T__17);
			setState(1360);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,210,_ctx) ) {
			case 1:
				{
				setState(1359);
				match(WS);
				}
				break;
			}
			setState(1362);
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
		enterRule(_localctx, 162, RULE_e_lambda);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1364);
			match(T__4);
			setState(1366);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1365);
				match(WS);
				}
			}

			setState(1368);
			variableNames();
			setState(1370);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1369);
				match(WS);
				}
			}

			setState(1372);
			match(T__5);
			setState(1374);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1373);
				match(WS);
				}
			}

			setState(1376);
			match(T__17);
			setState(1378);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,214,_ctx) ) {
			case 1:
				{
				setState(1377);
				match(WS);
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
		enterRule(_localctx, 164, RULE_e_literal);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1382);
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
		enterRule(_localctx, 166, RULE_e_sname);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1384);
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
		enterRule(_localctx, 168, RULE_e_qname);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1386);
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
		enterRule(_localctx, 170, RULE_e_tag);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1391);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,215,_ctx) ) {
			case 1:
				{
				setState(1388);
				qualifiedTypeName();
				setState(1389);
				match(T__0);
				}
				break;
			}
			setState(1393);
			tagName();
			setState(1398);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,217,_ctx) ) {
			case 1:
				{
				setState(1395);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1394);
					match(WS);
					}
				}

				setState(1397);
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_tuple(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_tupleContext e_tuple() throws RecognitionException {
		E_tupleContext _localctx = new E_tupleContext(_ctx, getState());
		enterRule(_localctx, 172, RULE_e_tuple);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1400);
			match(T__4);
			setState(1402);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,218,_ctx) ) {
			case 1:
				{
				setState(1401);
				match(WS);
				}
				break;
			}
			setState(1405);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,219,_ctx) ) {
			case 1:
				{
				setState(1404);
				expressions();
				}
				break;
			}
			setState(1408);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1407);
				match(WS);
				}
			}

			setState(1410);
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
		enterRule(_localctx, 174, RULE_e_keyValue);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1412);
			expression();
			setState(1414);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1413);
				match(WS);
				}
			}

			setState(1416);
			match(T__17);
			setState(1418);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,222,_ctx) ) {
			case 1:
				{
				setState(1417);
				match(WS);
				}
				break;
			}
			setState(1420);
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
		enterRule(_localctx, 176, RULE_e_keyValues);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1422);
			e_keyValue();
			setState(1433);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,225,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
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
					match(T__2);
					setState(1428);
					_errHandler.sync(this);
					switch ( getInterpreter().adaptivePredict(_input,224,_ctx) ) {
					case 1:
						{
						setState(1427);
						match(WS);
						}
						break;
					}
					setState(1430);
					e_keyValue();
					}
					} 
				}
				setState(1435);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,225,_ctx);
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
		enterRule(_localctx, 178, RULE_e_userError);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1436);
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
		enterRule(_localctx, 180, RULE_e_wild);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1438);
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
		enterRule(_localctx, 182, RULE_e_fNil);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1440);
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
		enterRule(_localctx, 184, RULE_e_fList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1442);
			e_apply();
			setState(1451);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,228,_ctx) ) {
			case 1:
				{
				setState(1444);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1443);
					match(WS);
					}
				}

				setState(1446);
				match(T__19);
				setState(1448);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,227,_ctx) ) {
				case 1:
					{
					setState(1447);
					match(WS);
					}
					break;
				}
				setState(1450);
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitE_fVec(this);
			else return visitor.visitChildren(this);
		}
	}

	public final E_fVecContext e_fVec() throws RecognitionException {
		E_fVecContext _localctx = new E_fVecContext(_ctx, getState());
		enterRule(_localctx, 186, RULE_e_fVec);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1453);
			match(T__20);
			setState(1455);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,229,_ctx) ) {
			case 1:
				{
				setState(1454);
				match(WS);
				}
				break;
			}
			setState(1458);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,230,_ctx) ) {
			case 1:
				{
				setState(1457);
				expressions();
				}
				break;
			}
			setState(1461);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1460);
				match(WS);
				}
			}

			setState(1463);
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
		enterRule(_localctx, 188, RULE_e_fSet);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1465);
			match(T__21);
			setState(1467);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,232,_ctx) ) {
			case 1:
				{
				setState(1466);
				match(WS);
				}
				break;
			}
			setState(1470);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,233,_ctx) ) {
			case 1:
				{
				setState(1469);
				expressions();
				}
				break;
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
		enterRule(_localctx, 190, RULE_e_fMap);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1477);
			match(T__22);
			setState(1479);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,235,_ctx) ) {
			case 1:
				{
				setState(1478);
				match(WS);
				}
				break;
			}
			setState(1482);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,236,_ctx) ) {
			case 1:
				{
				setState(1481);
				e_keyValues();
				}
				break;
			}
			setState(1485);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1484);
				match(WS);
				}
			}

			setState(1487);
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
			setState(1489);
			_la = _input.LA(1);
			if ( !(_la==T__23 || _la==T__24) ) {
			_errHandler.recoverInline(this);
			} else {
				consume();
			}
			setState(1491);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,238,_ctx) ) {
			case 1:
				{
				setState(1490);
				match(WS);
				}
				break;
			}
			setState(1493);
			formalparams();
			setState(1495);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1494);
				match(WS);
				}
			}

			setState(1497);
			match(T__0);
			setState(1499);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,240,_ctx) ) {
			case 1:
				{
				setState(1498);
				match(WS);
				}
				break;
			}
			setState(1501);
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
			setState(1503);
			_la = _input.LA(1);
			if ( !(_la==T__25 || _la==T__26) ) {
			_errHandler.recoverInline(this);
			} else {
				consume();
			}
			setState(1505);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,241,_ctx) ) {
			case 1:
				{
				setState(1504);
				match(WS);
				}
				break;
			}
			setState(1507);
			formalparams();
			setState(1509);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1508);
				match(WS);
				}
			}

			setState(1511);
			match(T__0);
			setState(1513);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,243,_ctx) ) {
			case 1:
				{
				setState(1512);
				match(WS);
				}
				break;
			}
			setState(1515);
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
		enterRule(_localctx, 196, RULE_pattern);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1517);
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
			setState(1519);
			pattern();
			setState(1530);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,246,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1521);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1520);
						match(WS);
						}
					}

					setState(1523);
					match(T__2);
					setState(1525);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1524);
						match(WS);
						}
					}

					setState(1527);
					pattern();
					}
					} 
				}
				setState(1532);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,246,_ctx);
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
		enterRule(_localctx, 200, RULE_simple);
		try {
			setState(1542);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,247,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1533);
				p_fNil();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1534);
				p_literal();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1535);
				p_variable();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(1536);
				p_wild();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(1537);
				p_tag();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(1538);
				p_tuple();
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(1539);
				p_fVec();
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(1540);
				p_fSet();
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(1541);
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
			setState(1544);
			pattern();
			setState(1546);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1545);
				match(WS);
				}
			}

			setState(1548);
			match(T__17);
			setState(1550);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1549);
				match(WS);
				}
			}

			setState(1552);
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
			setState(1554);
			p_keyValue();
			setState(1565);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,252,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1556);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1555);
						match(WS);
						}
					}

					setState(1558);
					match(T__2);
					setState(1560);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1559);
						match(WS);
						}
					}

					setState(1562);
					p_keyValue();
					}
					} 
				}
				setState(1567);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,252,_ctx);
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
		enterRule(_localctx, 206, RULE_p_literal);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1568);
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
		enterRule(_localctx, 208, RULE_p_tag);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1573);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,253,_ctx) ) {
			case 1:
				{
				setState(1570);
				qualifiedTypeName();
				setState(1571);
				match(T__0);
				}
				break;
			}
			setState(1575);
			tagName();
			setState(1580);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,255,_ctx) ) {
			case 1:
				{
				setState(1577);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1576);
					match(WS);
					}
				}

				setState(1579);
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
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FlixVisitor ) return ((FlixVisitor<? extends T>)visitor).visitP_tuple(this);
			else return visitor.visitChildren(this);
		}
	}

	public final P_tupleContext p_tuple() throws RecognitionException {
		P_tupleContext _localctx = new P_tupleContext(_ctx, getState());
		enterRule(_localctx, 210, RULE_p_tuple);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1582);
			match(T__4);
			setState(1584);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,256,_ctx) ) {
			case 1:
				{
				setState(1583);
				match(WS);
				}
				break;
			}
			setState(1587);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__4) | (1L << T__20) | (1L << T__21) | (1L << T__22) | (1L << T__28) | (1L << T__29) | (1L << T__30))) != 0) || ((((_la - 86)) & ~0x3f) == 0 && ((1L << (_la - 86)) & ((1L << (FNIL - 86)) | (1L << (WILD - 86)) | (1L << (LowerIdent - 86)) | (1L << (UpperIdent - 86)) | (1L << (Chars - 86)) | (1L << (Strs - 86)) | (1L << (Digits - 86)))) != 0)) {
				{
				setState(1586);
				patterns();
				}
			}

			setState(1590);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1589);
				match(WS);
				}
			}

			setState(1592);
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
		enterRule(_localctx, 212, RULE_p_wild);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1594);
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
		enterRule(_localctx, 214, RULE_p_fNil);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1596);
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
		enterRule(_localctx, 216, RULE_p_variable);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1598);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
		enterRule(_localctx, 218, RULE_p_fList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1600);
			simple();
			setState(1609);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,261,_ctx) ) {
			case 1:
				{
				setState(1602);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1601);
					match(WS);
					}
				}

				setState(1604);
				match(T__19);
				setState(1606);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1605);
					match(WS);
					}
				}

				setState(1608);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
		enterRule(_localctx, 220, RULE_p_fVec);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1611);
			match(T__20);
			setState(1613);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,262,_ctx) ) {
			case 1:
				{
				setState(1612);
				match(WS);
				}
				break;
			}
			setState(1616);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__4) | (1L << T__20) | (1L << T__21) | (1L << T__22) | (1L << T__28) | (1L << T__29) | (1L << T__30))) != 0) || ((((_la - 86)) & ~0x3f) == 0 && ((1L << (_la - 86)) & ((1L << (FNIL - 86)) | (1L << (WILD - 86)) | (1L << (LowerIdent - 86)) | (1L << (UpperIdent - 86)) | (1L << (Chars - 86)) | (1L << (Strs - 86)) | (1L << (Digits - 86)))) != 0)) {
				{
				setState(1615);
				patterns();
				}
			}

			setState(1628);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,266,_ctx) ) {
			case 1:
				{
				setState(1619);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1618);
					match(WS);
					}
				}

				setState(1621);
				match(T__2);
				setState(1623);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1622);
					match(WS);
					}
				}

				setState(1625);
				pattern();
				setState(1626);
				match(T__27);
				}
				break;
			}
			setState(1631);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1630);
				match(WS);
				}
			}

			setState(1633);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
		enterRule(_localctx, 222, RULE_p_fSet);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1635);
			match(T__21);
			setState(1637);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,268,_ctx) ) {
			case 1:
				{
				setState(1636);
				match(WS);
				}
				break;
			}
			setState(1640);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__4) | (1L << T__20) | (1L << T__21) | (1L << T__22) | (1L << T__28) | (1L << T__29) | (1L << T__30))) != 0) || ((((_la - 86)) & ~0x3f) == 0 && ((1L << (_la - 86)) & ((1L << (FNIL - 86)) | (1L << (WILD - 86)) | (1L << (LowerIdent - 86)) | (1L << (UpperIdent - 86)) | (1L << (Chars - 86)) | (1L << (Strs - 86)) | (1L << (Digits - 86)))) != 0)) {
				{
				setState(1639);
				patterns();
				}
			}

			setState(1652);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,272,_ctx) ) {
			case 1:
				{
				setState(1643);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1642);
					match(WS);
					}
				}

				setState(1645);
				match(T__2);
				setState(1647);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1646);
					match(WS);
					}
				}

				setState(1649);
				pattern();
				setState(1650);
				match(T__27);
				}
				break;
			}
			setState(1655);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1654);
				match(WS);
				}
			}

			setState(1657);
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
		public List<TerminalNode> WS() { return getTokens(FlixParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(FlixParser.WS, i);
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
		enterRule(_localctx, 224, RULE_p_fMap);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1659);
			match(T__22);
			setState(1661);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,274,_ctx) ) {
			case 1:
				{
				setState(1660);
				match(WS);
				}
				break;
			}
			setState(1664);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__4) | (1L << T__20) | (1L << T__21) | (1L << T__22) | (1L << T__28) | (1L << T__29) | (1L << T__30))) != 0) || ((((_la - 86)) & ~0x3f) == 0 && ((1L << (_la - 86)) & ((1L << (FNIL - 86)) | (1L << (WILD - 86)) | (1L << (LowerIdent - 86)) | (1L << (UpperIdent - 86)) | (1L << (Chars - 86)) | (1L << (Strs - 86)) | (1L << (Digits - 86)))) != 0)) {
				{
				setState(1663);
				p_keyValues();
				}
			}

			setState(1676);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,278,_ctx) ) {
			case 1:
				{
				setState(1667);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1666);
					match(WS);
					}
				}

				setState(1669);
				match(T__2);
				setState(1671);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1670);
					match(WS);
					}
				}

				setState(1673);
				pattern();
				setState(1674);
				match(T__27);
				}
				break;
			}
			setState(1679);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1678);
				match(WS);
				}
			}

			setState(1681);
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
		enterRule(_localctx, 226, RULE_bools);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1683);
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
		enterRule(_localctx, 228, RULE_chars);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1685);
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
		enterRule(_localctx, 230, RULE_strs);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1687);
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
		enterRule(_localctx, 232, RULE_negative);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1689);
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
		enterRule(_localctx, 234, RULE_float32);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1692);
			_la = _input.LA(1);
			if (_la==T__30) {
				{
				setState(1691);
				negative();
				}
			}

			setState(1694);
			match(Digits);
			setState(1695);
			match(T__0);
			setState(1696);
			match(Digits);
			setState(1697);
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
		enterRule(_localctx, 236, RULE_float64);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1700);
			_la = _input.LA(1);
			if (_la==T__30) {
				{
				setState(1699);
				negative();
				}
			}

			setState(1702);
			match(Digits);
			setState(1703);
			match(T__0);
			setState(1704);
			match(Digits);
			setState(1705);
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
		enterRule(_localctx, 238, RULE_floatDefault);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1708);
			_la = _input.LA(1);
			if (_la==T__30) {
				{
				setState(1707);
				negative();
				}
			}

			setState(1710);
			match(Digits);
			setState(1711);
			match(T__0);
			setState(1712);
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
		enterRule(_localctx, 240, RULE_floats);
		try {
			setState(1717);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,283,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1714);
				float32();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1715);
				float64();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1716);
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
		enterRule(_localctx, 242, RULE_int8);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1720);
			_la = _input.LA(1);
			if (_la==T__30) {
				{
				setState(1719);
				negative();
				}
			}

			setState(1722);
			match(Digits);
			setState(1723);
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
		enterRule(_localctx, 244, RULE_int16);
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
		enterRule(_localctx, 246, RULE_int32);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1732);
			_la = _input.LA(1);
			if (_la==T__30) {
				{
				setState(1731);
				negative();
				}
			}

			setState(1734);
			match(Digits);
			setState(1735);
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
		enterRule(_localctx, 248, RULE_int64);
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
		enterRule(_localctx, 250, RULE_bigInt);
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
		enterRule(_localctx, 252, RULE_intDefault);
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
			}
		}
		catch (RecognitionException re) {
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
		enterRule(_localctx, 254, RULE_ints);
		try {
			setState(1760);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,290,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1754);
				int8();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1755);
				int16();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1756);
				int32();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(1757);
				int64();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(1758);
				bigInt();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(1759);
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
		enterRule(_localctx, 256, RULE_literal);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1767);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,291,_ctx) ) {
			case 1:
				{
				setState(1762);
				bools();
				}
				break;
			case 2:
				{
				setState(1763);
				chars();
				}
				break;
			case 3:
				{
				setState(1764);
				floats();
				}
				break;
			case 4:
				{
				setState(1765);
				ints();
				}
				break;
			case 5:
				{
				setState(1766);
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
		enterRule(_localctx, 258, RULE_primary);
		try {
			setState(1774);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,292,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1769);
				arrow();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1770);
				tuple();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1771);
				apply();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(1772);
				var();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(1773);
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
		enterRule(_localctx, 260, RULE_var);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1776);
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
		enterRule(_localctx, 262, RULE_ref);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1778);
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
		enterRule(_localctx, 264, RULE_type);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1780);
			primary();
			setState(1789);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,295,_ctx) ) {
			case 1:
				{
				setState(1782);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1781);
					match(WS);
					}
				}

				setState(1784);
				match(T__17);
				setState(1786);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1785);
					match(WS);
					}
				}

				setState(1788);
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
		enterRule(_localctx, 266, RULE_arrow);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1791);
			match(T__4);
			setState(1793);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1792);
				match(WS);
				}
			}

			setState(1795);
			type();
			setState(1806);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,299,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1797);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1796);
						match(WS);
						}
					}

					setState(1799);
					match(T__2);
					setState(1801);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1800);
						match(WS);
						}
					}

					setState(1803);
					type();
					}
					} 
				}
				setState(1808);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,299,_ctx);
			}
			setState(1810);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1809);
				match(WS);
				}
			}

			setState(1812);
			match(T__5);
			setState(1814);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1813);
				match(WS);
				}
			}

			setState(1816);
			match(T__17);
			setState(1818);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1817);
				match(WS);
				}
			}

			setState(1820);
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
		enterRule(_localctx, 268, RULE_tuple_unit);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1822);
			match(T__4);
			setState(1823);
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
		enterRule(_localctx, 270, RULE_tuple_singleton);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1825);
			match(T__4);
			setState(1827);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1826);
				match(WS);
				}
			}

			setState(1829);
			type();
			setState(1831);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1830);
				match(WS);
				}
			}

			setState(1833);
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
		enterRule(_localctx, 272, RULE_tuple_multi);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1835);
			match(T__4);
			setState(1837);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1836);
				match(WS);
				}
			}

			setState(1839);
			type();
			setState(1848); 
			_errHandler.sync(this);
			_alt = 1;
			do {
				switch (_alt) {
				case 1:
					{
					{
					setState(1841);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1840);
						match(WS);
						}
					}

					setState(1843);
					match(T__2);
					setState(1845);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1844);
						match(WS);
						}
					}

					setState(1847);
					type();
					}
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(1850); 
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,308,_ctx);
			} while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER );
			setState(1853);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1852);
				match(WS);
				}
			}

			setState(1855);
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
		enterRule(_localctx, 274, RULE_tuple);
		try {
			setState(1860);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,310,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1857);
				tuple_unit();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1858);
				tuple_singleton();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1859);
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
		enterRule(_localctx, 276, RULE_apply);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1862);
			ref();
			setState(1864);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1863);
				match(WS);
				}
			}

			setState(1866);
			match(T__9);
			setState(1868);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1867);
				match(WS);
				}
			}

			setState(1870);
			type();
			setState(1881);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,315,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1872);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1871);
						match(WS);
						}
					}

					setState(1874);
					match(T__2);
					setState(1876);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1875);
						match(WS);
						}
					}

					setState(1878);
					type();
					}
					} 
				}
				setState(1883);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,315,_ctx);
			}
			setState(1885);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1884);
				match(WS);
				}
			}

			setState(1887);
			match(T__10);
			setState(1889);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,317,_ctx) ) {
			case 1:
				{
				setState(1888);
				match(WS);
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
		enterRule(_localctx, 278, RULE_unary_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1891);
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
		enterRule(_localctx, 280, RULE_logical_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1893);
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
		enterRule(_localctx, 282, RULE_comparison_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1895);
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
		enterRule(_localctx, 284, RULE_multipve_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1897);
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
		enterRule(_localctx, 286, RULE_addve_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1899);
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
		enterRule(_localctx, 288, RULE_extbin_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1901);
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
		enterRule(_localctx, 290, RULE_predicate);
		try {
			setState(1909);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,318,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1903);
				pred_true();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1904);
				pred_false();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1905);
				pred_filter();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(1906);
				pred_notequal();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(1907);
				pred_table();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(1908);
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
		enterRule(_localctx, 292, RULE_predicates);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1911);
			predicate();
			setState(1922);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,321,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1913);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1912);
						match(WS);
						}
					}

					setState(1915);
					match(T__2);
					setState(1917);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1916);
						match(WS);
						}
					}

					setState(1919);
					predicate();
					}
					} 
				}
				setState(1924);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,321,_ctx);
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
		enterRule(_localctx, 294, RULE_pred_true);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1925);
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
		enterRule(_localctx, 296, RULE_pred_false);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1927);
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
		enterRule(_localctx, 298, RULE_pred_filter);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1929);
			qualifiedDefinitionName();
			setState(1931);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1930);
				match(WS);
				}
			}

			setState(1933);
			match(T__4);
			setState(1934);
			expressions();
			setState(1935);
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
		enterRule(_localctx, 300, RULE_pred_table);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1937);
			qualifiedTableName();
			setState(1939);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1938);
				match(WS);
				}
			}

			setState(1941);
			match(T__4);
			setState(1942);
			expressions();
			setState(1943);
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
		enterRule(_localctx, 302, RULE_pred_notequal);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1945);
			variableName();
			setState(1947);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1946);
				match(WS);
				}
			}

			setState(1949);
			match(T__59);
			setState(1951);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1950);
				match(WS);
				}
			}

			setState(1953);
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
		enterRule(_localctx, 304, RULE_pred_loop);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1955);
			variableName();
			setState(1957);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1956);
				match(WS);
				}
			}

			setState(1959);
			match(T__69);
			setState(1961);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,327,_ctx) ) {
			case 1:
				{
				setState(1960);
				match(WS);
				}
				break;
			}
			setState(1963);
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
			return multiplicative_sempred((MultiplicativeContext)_localctx, predIndex);
		case 72:
			return unary_sempred((UnaryContext)_localctx, predIndex);
		}
		return true;
	}
	private boolean multiplicative_sempred(MultiplicativeContext _localctx, int predIndex) {
		switch (predIndex) {
		case 0:
			return !( _input.LT(2).getText().equals("/") && //Make sure we aren't accessing a namespace
								Character.isLetter(_input.LT(3).getText().charAt(0)) );
		}
		return true;
	}
	private boolean unary_sempred(UnaryContext _localctx, int predIndex) {
		switch (predIndex) {
		case 1:
			return !( _input.LT(1).getText().equals("-") && //Make sure this isn't just a negative number
				Character.isDigit(_input.LT(2).getText().charAt(0)) );
		}
		return true;
	}

	public static final String _serializedATN =
		"\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd\3e\u07b0\4\2\t\2\4"+
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
		"\t\u0097\4\u0098\t\u0098\4\u0099\t\u0099\4\u009a\t\u009a\3\2\3\2\3\3\7"+
		"\3\u0138\n\3\f\3\16\3\u013b\13\3\3\3\7\3\u013e\n\3\f\3\16\3\u0141\13\3"+
		"\3\3\5\3\u0144\n\3\3\3\3\3\3\4\5\4\u0149\n\4\3\4\5\4\u014c\n\4\3\5\3\5"+
		"\3\6\3\6\3\6\7\6\u0153\n\6\f\6\16\6\u0156\13\6\3\7\3\7\3\7\5\7\u015b\n"+
		"\7\3\7\3\7\3\b\3\b\3\b\5\b\u0162\n\b\3\b\3\b\3\t\3\t\3\n\3\n\3\13\3\13"+
		"\3\f\3\f\3\r\3\r\3\16\3\16\3\17\3\17\3\20\3\20\3\21\3\21\3\22\3\22\3\23"+
		"\3\23\3\24\3\24\5\24\u017e\n\24\3\24\3\24\5\24\u0182\n\24\3\24\7\24\u0185"+
		"\n\24\f\24\16\24\u0188\13\24\3\25\3\25\3\25\5\25\u018d\n\25\3\25\3\25"+
		"\3\26\3\26\5\26\u0193\n\26\3\26\3\26\5\26\u0197\n\26\3\26\7\26\u019a\n"+
		"\26\f\26\16\26\u019d\13\26\3\27\3\27\5\27\u01a1\n\27\3\27\5\27\u01a4\n"+
		"\27\3\27\5\27\u01a7\n\27\3\27\5\27\u01aa\n\27\3\30\3\30\5\30\u01ae\n\30"+
		"\3\30\3\30\5\30\u01b2\n\30\3\30\3\30\3\31\3\31\5\31\u01b8\n\31\3\31\3"+
		"\31\5\31\u01bc\n\31\3\31\7\31\u01bf\n\31\f\31\16\31\u01c2\13\31\3\32\3"+
		"\32\5\32\u01c6\n\32\3\32\3\32\5\32\u01ca\n\32\3\32\3\32\5\32\u01ce\n\32"+
		"\3\32\7\32\u01d1\n\32\f\32\16\32\u01d4\13\32\5\32\u01d6\n\32\3\32\5\32"+
		"\u01d9\n\32\3\32\3\32\3\33\3\33\5\33\u01df\n\33\3\33\3\33\5\33\u01e3\n"+
		"\33\3\33\7\33\u01e6\n\33\f\33\16\33\u01e9\13\33\3\34\3\34\3\34\3\34\5"+
		"\34\u01ef\n\34\3\34\3\34\5\34\u01f3\n\34\3\34\3\34\5\34\u01f7\n\34\3\35"+
		"\3\35\5\35\u01fb\n\35\3\35\7\35\u01fe\n\35\f\35\16\35\u0201\13\35\3\36"+
		"\3\36\3\36\3\36\5\36\u0207\n\36\3\36\3\36\5\36\u020b\n\36\3\36\3\36\5"+
		"\36\u020f\n\36\3\37\3\37\5\37\u0213\n\37\3\37\7\37\u0216\n\37\f\37\16"+
		"\37\u0219\13\37\3 \3 \5 \u021d\n \3 \3 \5 \u0221\n \3 \5 \u0224\n \3!"+
		"\3!\5!\u0228\n!\3!\3!\5!\u022c\n!\3!\3!\5!\u0230\n!\3!\7!\u0233\n!\f!"+
		"\16!\u0236\13!\3!\3!\5!\u023a\n!\3\"\3\"\3\"\5\"\u023f\n\"\3\"\3\"\5\""+
		"\u0243\n\"\3\"\7\"\u0246\n\"\f\"\16\"\u0249\13\"\3\"\3\"\3#\3#\3#\3$\3"+
		"$\5$\u0252\n$\3$\3$\5$\u0256\n$\3$\7$\u0259\n$\f$\16$\u025c\13$\3%\5%"+
		"\u025f\n%\3%\3%\5%\u0263\n%\3%\5%\u0266\n%\3&\5&\u0269\n&\3&\3&\5&\u026d"+
		"\n&\3&\5&\u0270\n&\3\'\3\'\3\'\3(\3(\3(\7(\u0278\n(\f(\16(\u027b\13(\3"+
		")\5)\u027e\n)\3)\3)\3)\5)\u0283\n)\3*\3*\3*\3*\3*\3*\3*\3+\3+\3+\3+\3"+
		"+\3+\3+\3,\3,\3,\3,\3,\3-\3-\3-\3-\3-\3-\3-\3-\3-\3-\3-\3-\3-\3-\5-\u02a6"+
		"\n-\3.\5.\u02a9\n.\3.\3.\3.\3.\5.\u02af\n.\3.\3.\5.\u02b3\n.\3.\7.\u02b6"+
		"\n.\f.\16.\u02b9\13.\3.\5.\u02bc\n.\3.\3.\3.\3/\5/\u02c2\n/\3/\7/\u02c5"+
		"\n/\f/\16/\u02c8\13/\3/\5/\u02cb\n/\3/\3/\3/\3/\3/\5/\u02d2\n/\3/\3/\5"+
		"/\u02d6\n/\3/\3/\5/\u02da\n/\3/\3/\3/\3\60\3\60\5\60\u02e1\n\60\3\60\3"+
		"\60\5\60\u02e5\n\60\3\60\7\60\u02e8\n\60\f\60\16\60\u02eb\13\60\3\61\3"+
		"\61\3\61\3\61\5\61\u02f1\n\61\3\62\5\62\u02f4\n\62\3\62\7\62\u02f7\n\62"+
		"\f\62\16\62\u02fa\13\62\3\62\5\62\u02fd\n\62\3\62\3\62\3\62\3\62\5\62"+
		"\u0303\n\62\3\62\3\62\5\62\u0307\n\62\3\62\5\62\u030a\n\62\3\62\5\62\u030d"+
		"\n\62\3\62\3\62\3\62\3\63\5\63\u0313\n\63\3\63\7\63\u0316\n\63\f\63\16"+
		"\63\u0319\13\63\3\63\5\63\u031c\n\63\3\63\3\63\3\63\3\63\5\63\u0322\n"+
		"\63\3\63\3\63\5\63\u0326\n\63\3\63\5\63\u0329\n\63\3\63\5\63\u032c\n\63"+
		"\3\63\3\63\3\63\3\64\5\64\u0332\n\64\3\64\3\64\3\64\3\64\5\64\u0338\n"+
		"\64\3\64\3\64\5\64\u033c\n\64\3\64\5\64\u033f\n\64\3\64\5\64\u0342\n\64"+
		"\3\64\3\64\3\64\3\65\5\65\u0348\n\65\3\65\7\65\u034b\n\65\f\65\16\65\u034e"+
		"\13\65\3\65\5\65\u0351\n\65\3\65\3\65\3\65\3\65\5\65\u0357\n\65\3\65\3"+
		"\65\5\65\u035b\n\65\3\65\3\65\5\65\u035f\n\65\3\65\3\65\3\65\3\66\5\66"+
		"\u0365\n\66\3\66\7\66\u0368\n\66\f\66\16\66\u036b\13\66\3\66\5\66\u036e"+
		"\n\66\3\66\3\66\5\66\u0372\n\66\3\66\3\66\3\66\3\66\5\66\u0378\n\66\3"+
		"\66\3\66\5\66\u037c\n\66\3\66\3\66\5\66\u0380\n\66\3\66\3\66\3\66\3\67"+
		"\5\67\u0386\n\67\3\67\7\67\u0389\n\67\f\67\16\67\u038c\13\67\3\67\5\67"+
		"\u038f\n\67\3\67\5\67\u0392\n\67\3\67\5\67\u0395\n\67\3\67\3\67\3\67\3"+
		"\67\5\67\u039b\n\67\3\67\3\67\3\67\5\67\u03a0\n\67\3\67\3\67\5\67\u03a4"+
		"\n\67\3\67\3\67\5\67\u03a8\n\67\3\67\3\67\5\67\u03ac\n\67\3\67\3\67\3"+
		"\67\38\58\u03b2\n8\38\78\u03b5\n8\f8\168\u03b8\138\38\58\u03bb\n8\38\3"+
		"8\38\38\58\u03c1\n8\38\38\58\u03c5\n8\38\38\58\u03c9\n8\38\38\58\u03cd"+
		"\n8\38\38\58\u03d1\n8\38\38\58\u03d5\n8\38\38\38\39\59\u03db\n9\39\79"+
		"\u03de\n9\f9\169\u03e1\139\39\59\u03e4\n9\39\39\39\39\39\39\59\u03ec\n"+
		"9\39\39\3:\3:\5:\u03f2\n:\3:\3:\5:\u03f6\n:\7:\u03f8\n:\f:\16:\u03fb\13"+
		":\3:\3:\3;\3;\3;\5;\u0402\n;\3<\5<\u0405\n<\3<\3<\5<\u0409\n<\3<\3<\3"+
		"=\5=\u040e\n=\3=\3=\5=\u0412\n=\3=\3=\5=\u0416\n=\3=\3=\5=\u041a\n=\3"+
		"=\3=\3>\3>\3?\5?\u0421\n?\3?\3?\5?\u0425\n?\3?\3?\3?\5?\u042a\n?\3?\3"+
		"?\5?\u042e\n?\3?\3?\5?\u0432\n?\3?\3?\5?\u0436\n?\3?\3?\3?\3@\5@\u043c"+
		"\n@\3@\7@\u043f\n@\f@\16@\u0442\13@\3@\5@\u0445\n@\3@\3@\3@\3@\3@\5@\u044c"+
		"\n@\3@\3@\5@\u0450\n@\3@\3@\3A\3A\5A\u0456\nA\3A\7A\u0459\nA\fA\16A\u045c"+
		"\13A\3A\5A\u045f\nA\3A\3A\3B\3B\5B\u0465\nB\3B\3B\5B\u0469\nB\3B\3B\5"+
		"B\u046d\nB\3B\5B\u0470\nB\3C\3C\5C\u0474\nC\3C\3C\5C\u0478\nC\3C\3C\5"+
		"C\u047c\nC\3D\3D\5D\u0480\nD\3D\3D\5D\u0484\nD\3D\7D\u0487\nD\fD\16D\u048a"+
		"\13D\3E\3E\5E\u048e\nE\3E\3E\5E\u0492\nE\3E\3E\5E\u0496\nE\3F\3F\5F\u049a"+
		"\nF\3F\3F\5F\u049e\nF\3F\3F\5F\u04a2\nF\3G\3G\3G\5G\u04a7\nG\3G\3G\5G"+
		"\u04ab\nG\3G\3G\3G\5G\u04b0\nG\3H\3H\5H\u04b4\nH\3H\3H\3H\3H\5H\u04ba"+
		"\nH\3H\3H\5H\u04be\nH\3I\3I\5I\u04c2\nI\3I\3I\5I\u04c6\nI\3I\3I\5I\u04ca"+
		"\nI\3J\3J\3J\5J\u04cf\nJ\3J\3J\3J\5J\u04d4\nJ\3K\3K\5K\u04d8\nK\3K\3K"+
		"\5K\u04dc\nK\3K\5K\u04df\nK\3L\3L\3L\3L\3L\3L\3L\3L\3L\3L\3L\3L\3L\3L"+
		"\3L\3L\3L\3L\3L\5L\u04f4\nL\3M\3M\3M\3M\5M\u04fa\nM\3M\3M\5M\u04fe\nM"+
		"\3M\3M\5M\u0502\nM\3M\3M\5M\u0506\nM\3M\3M\3N\3N\5N\u050c\nN\3N\3N\5N"+
		"\u0510\nN\3N\3N\5N\u0514\nN\3N\3N\5N\u0518\nN\3N\3N\3N\3N\3N\3N\3O\3O"+
		"\3O\3O\3O\3O\3O\3O\5O\u0528\nO\3O\3O\5O\u052c\nO\3O\3O\3P\3P\3P\3P\5P"+
		"\u0534\nP\3P\3P\5P\u0538\nP\3P\3P\3Q\3Q\5Q\u053e\nQ\3Q\3Q\5Q\u0542\nQ"+
		"\3Q\5Q\u0545\nQ\3Q\5Q\u0548\nQ\3Q\5Q\u054b\nQ\3R\3R\5R\u054f\nR\3R\3R"+
		"\5R\u0553\nR\3R\3R\3S\3S\5S\u0559\nS\3S\3S\5S\u055d\nS\3S\3S\5S\u0561"+
		"\nS\3S\3S\5S\u0565\nS\3S\3S\3T\3T\3U\3U\3V\3V\3W\3W\3W\5W\u0572\nW\3W"+
		"\3W\5W\u0576\nW\3W\5W\u0579\nW\3X\3X\5X\u057d\nX\3X\5X\u0580\nX\3X\5X"+
		"\u0583\nX\3X\3X\3Y\3Y\5Y\u0589\nY\3Y\3Y\5Y\u058d\nY\3Y\3Y\3Z\3Z\5Z\u0593"+
		"\nZ\3Z\3Z\5Z\u0597\nZ\3Z\7Z\u059a\nZ\fZ\16Z\u059d\13Z\3[\3[\3\\\3\\\3"+
		"]\3]\3^\3^\5^\u05a7\n^\3^\3^\5^\u05ab\n^\3^\5^\u05ae\n^\3_\3_\5_\u05b2"+
		"\n_\3_\5_\u05b5\n_\3_\5_\u05b8\n_\3_\3_\3`\3`\5`\u05be\n`\3`\5`\u05c1"+
		"\n`\3`\5`\u05c4\n`\3`\3`\3a\3a\5a\u05ca\na\3a\5a\u05cd\na\3a\5a\u05d0"+
		"\na\3a\3a\3b\3b\5b\u05d6\nb\3b\3b\5b\u05da\nb\3b\3b\5b\u05de\nb\3b\3b"+
		"\3c\3c\5c\u05e4\nc\3c\3c\5c\u05e8\nc\3c\3c\5c\u05ec\nc\3c\3c\3d\3d\3e"+
		"\3e\5e\u05f4\ne\3e\3e\5e\u05f8\ne\3e\7e\u05fb\ne\fe\16e\u05fe\13e\3f\3"+
		"f\3f\3f\3f\3f\3f\3f\3f\5f\u0609\nf\3g\3g\5g\u060d\ng\3g\3g\5g\u0611\n"+
		"g\3g\3g\3h\3h\5h\u0617\nh\3h\3h\5h\u061b\nh\3h\7h\u061e\nh\fh\16h\u0621"+
		"\13h\3i\3i\3j\3j\3j\5j\u0628\nj\3j\3j\5j\u062c\nj\3j\5j\u062f\nj\3k\3"+
		"k\5k\u0633\nk\3k\5k\u0636\nk\3k\5k\u0639\nk\3k\3k\3l\3l\3m\3m\3n\3n\3"+
		"o\3o\5o\u0645\no\3o\3o\5o\u0649\no\3o\5o\u064c\no\3p\3p\5p\u0650\np\3"+
		"p\5p\u0653\np\3p\5p\u0656\np\3p\3p\5p\u065a\np\3p\3p\3p\5p\u065f\np\3"+
		"p\5p\u0662\np\3p\3p\3q\3q\5q\u0668\nq\3q\5q\u066b\nq\3q\5q\u066e\nq\3"+
		"q\3q\5q\u0672\nq\3q\3q\3q\5q\u0677\nq\3q\5q\u067a\nq\3q\3q\3r\3r\5r\u0680"+
		"\nr\3r\5r\u0683\nr\3r\5r\u0686\nr\3r\3r\5r\u068a\nr\3r\3r\3r\5r\u068f"+
		"\nr\3r\5r\u0692\nr\3r\3r\3s\3s\3t\3t\3u\3u\3v\3v\3w\5w\u069f\nw\3w\3w"+
		"\3w\3w\3w\3x\5x\u06a7\nx\3x\3x\3x\3x\3x\3y\5y\u06af\ny\3y\3y\3y\3y\3z"+
		"\3z\3z\5z\u06b8\nz\3{\5{\u06bb\n{\3{\3{\3{\3|\5|\u06c1\n|\3|\3|\3|\3}"+
		"\5}\u06c7\n}\3}\3}\3}\3~\5~\u06cd\n~\3~\3~\3~\3\177\5\177\u06d3\n\177"+
		"\3\177\3\177\3\177\3\u0080\5\u0080\u06d9\n\u0080\3\u0080\3\u0080\3\u0081"+
		"\3\u0081\3\u0081\3\u0081\3\u0081\3\u0081\5\u0081\u06e3\n\u0081\3\u0082"+
		"\3\u0082\3\u0082\3\u0082\3\u0082\5\u0082\u06ea\n\u0082\3\u0083\3\u0083"+
		"\3\u0083\3\u0083\3\u0083\5\u0083\u06f1\n\u0083\3\u0084\3\u0084\3\u0085"+
		"\3\u0085\3\u0086\3\u0086\5\u0086\u06f9\n\u0086\3\u0086\3\u0086\5\u0086"+
		"\u06fd\n\u0086\3\u0086\5\u0086\u0700\n\u0086\3\u0087\3\u0087\5\u0087\u0704"+
		"\n\u0087\3\u0087\3\u0087\5\u0087\u0708\n\u0087\3\u0087\3\u0087\5\u0087"+
		"\u070c\n\u0087\3\u0087\7\u0087\u070f\n\u0087\f\u0087\16\u0087\u0712\13"+
		"\u0087\3\u0087\5\u0087\u0715\n\u0087\3\u0087\3\u0087\5\u0087\u0719\n\u0087"+
		"\3\u0087\3\u0087\5\u0087\u071d\n\u0087\3\u0087\3\u0087\3\u0088\3\u0088"+
		"\3\u0088\3\u0089\3\u0089\5\u0089\u0726\n\u0089\3\u0089\3\u0089\5\u0089"+
		"\u072a\n\u0089\3\u0089\3\u0089\3\u008a\3\u008a\5\u008a\u0730\n\u008a\3"+
		"\u008a\3\u008a\5\u008a\u0734\n\u008a\3\u008a\3\u008a\5\u008a\u0738\n\u008a"+
		"\3\u008a\6\u008a\u073b\n\u008a\r\u008a\16\u008a\u073c\3\u008a\5\u008a"+
		"\u0740\n\u008a\3\u008a\3\u008a\3\u008b\3\u008b\3\u008b\5\u008b\u0747\n"+
		"\u008b\3\u008c\3\u008c\5\u008c\u074b\n\u008c\3\u008c\3\u008c\5\u008c\u074f"+
		"\n\u008c\3\u008c\3\u008c\5\u008c\u0753\n\u008c\3\u008c\3\u008c\5\u008c"+
		"\u0757\n\u008c\3\u008c\7\u008c\u075a\n\u008c\f\u008c\16\u008c\u075d\13"+
		"\u008c\3\u008c\5\u008c\u0760\n\u008c\3\u008c\3\u008c\5\u008c\u0764\n\u008c"+
		"\3\u008d\3\u008d\3\u008e\3\u008e\3\u008f\3\u008f\3\u0090\3\u0090\3\u0091"+
		"\3\u0091\3\u0092\3\u0092\3\u0093\3\u0093\3\u0093\3\u0093\3\u0093\3\u0093"+
		"\5\u0093\u0778\n\u0093\3\u0094\3\u0094\5\u0094\u077c\n\u0094\3\u0094\3"+
		"\u0094\5\u0094\u0780\n\u0094\3\u0094\7\u0094\u0783\n\u0094\f\u0094\16"+
		"\u0094\u0786\13\u0094\3\u0095\3\u0095\3\u0096\3\u0096\3\u0097\3\u0097"+
		"\5\u0097\u078e\n\u0097\3\u0097\3\u0097\3\u0097\3\u0097\3\u0098\3\u0098"+
		"\5\u0098\u0796\n\u0098\3\u0098\3\u0098\3\u0098\3\u0098\3\u0099\3\u0099"+
		"\5\u0099\u079e\n\u0099\3\u0099\3\u0099\5\u0099\u07a2\n\u0099\3\u0099\3"+
		"\u0099\3\u009a\3\u009a\5\u009a\u07a8\n\u009a\3\u009a\3\u009a\5\u009a\u07ac"+
		"\n\u009a\3\u009a\3\u009a\3\u009a\2\2\u009b\2\4\6\b\n\f\16\20\22\24\26"+
		"\30\32\34\36 \"$&(*,.\60\62\64\668:<>@BDFHJLNPRTVXZ\\^`bdfhjlnprtvxz|"+
		"~\u0080\u0082\u0084\u0086\u0088\u008a\u008c\u008e\u0090\u0092\u0094\u0096"+
		"\u0098\u009a\u009c\u009e\u00a0\u00a2\u00a4\u00a6\u00a8\u00aa\u00ac\u00ae"+
		"\u00b0\u00b2\u00b4\u00b6\u00b8\u00ba\u00bc\u00be\u00c0\u00c2\u00c4\u00c6"+
		"\u00c8\u00ca\u00cc\u00ce\u00d0\u00d2\u00d4\u00d6\u00d8\u00da\u00dc\u00de"+
		"\u00e0\u00e2\u00e4\u00e6\u00e8\u00ea\u00ec\u00ee\u00f0\u00f2\u00f4\u00f6"+
		"\u00f8\u00fa\u00fc\u00fe\u0100\u0102\u0104\u0106\u0108\u010a\u010c\u010e"+
		"\u0110\u0112\u0114\u0116\u0118\u011a\u011c\u011e\u0120\u0122\u0124\u0126"+
		"\u0128\u012a\u012c\u012e\u0130\u0132\2\r\3\2ab\4\2XXbb\3\2\32\33\3\2\34"+
		"\35\3\2\37 \4\2!!),\3\2-9\4\2\16\16:?\4\2\4\4@B\4\2!!))\3\2CG\u0894\2"+
		"\u0134\3\2\2\2\4\u0139\3\2\2\2\6\u014b\3\2\2\2\b\u014d\3\2\2\2\n\u014f"+
		"\3\2\2\2\f\u015a\3\2\2\2\16\u0161\3\2\2\2\20\u0165\3\2\2\2\22\u0167\3"+
		"\2\2\2\24\u0169\3\2\2\2\26\u016b\3\2\2\2\30\u016d\3\2\2\2\32\u016f\3\2"+
		"\2\2\34\u0171\3\2\2\2\36\u0173\3\2\2\2 \u0175\3\2\2\2\"\u0177\3\2\2\2"+
		"$\u0179\3\2\2\2&\u017b\3\2\2\2(\u0189\3\2\2\2*\u0190\3\2\2\2,\u01a9\3"+
		"\2\2\2.\u01ab\3\2\2\2\60\u01b5\3\2\2\2\62\u01c3\3\2\2\2\64\u01dc\3\2\2"+
		"\2\66\u01ea\3\2\2\28\u01f8\3\2\2\2:\u0202\3\2\2\2<\u0210\3\2\2\2>\u021a"+
		"\3\2\2\2@\u0239\3\2\2\2B\u023b\3\2\2\2D\u024c\3\2\2\2F\u024f\3\2\2\2H"+
		"\u0265\3\2\2\2J\u026f\3\2\2\2L\u0271\3\2\2\2N\u0274\3\2\2\2P\u027d\3\2"+
		"\2\2R\u0284\3\2\2\2T\u028b\3\2\2\2V\u0292\3\2\2\2X\u02a5\3\2\2\2Z\u02a8"+
		"\3\2\2\2\\\u02c6\3\2\2\2^\u02de\3\2\2\2`\u02ec\3\2\2\2b\u02f8\3\2\2\2"+
		"d\u0317\3\2\2\2f\u0331\3\2\2\2h\u034c\3\2\2\2j\u0369\3\2\2\2l\u038a\3"+
		"\2\2\2n\u03b6\3\2\2\2p\u03df\3\2\2\2r\u03ef\3\2\2\2t\u0401\3\2\2\2v\u0404"+
		"\3\2\2\2x\u040d\3\2\2\2z\u041d\3\2\2\2|\u0420\3\2\2\2~\u0440\3\2\2\2\u0080"+
		"\u0453\3\2\2\2\u0082\u046f\3\2\2\2\u0084\u0471\3\2\2\2\u0086\u047d\3\2"+
		"\2\2\u0088\u048b\3\2\2\2\u008a\u0497\3\2\2\2\u008c\u04af\3\2\2\2\u008e"+
		"\u04b1\3\2\2\2\u0090\u04bf\3\2\2\2\u0092\u04d3\3\2\2\2\u0094\u04d5\3\2"+
		"\2\2\u0096\u04f3\3\2\2\2\u0098\u04f5\3\2\2\2\u009a\u0509\3\2\2\2\u009c"+
		"\u051f\3\2\2\2\u009e\u052f\3\2\2\2\u00a0\u053b\3\2\2\2\u00a2\u054c\3\2"+
		"\2\2\u00a4\u0556\3\2\2\2\u00a6\u0568\3\2\2\2\u00a8\u056a\3\2\2\2\u00aa"+
		"\u056c\3\2\2\2\u00ac\u0571\3\2\2\2\u00ae\u057a\3\2\2\2\u00b0\u0586\3\2"+
		"\2\2\u00b2\u0590\3\2\2\2\u00b4\u059e\3\2\2\2\u00b6\u05a0\3\2\2\2\u00b8"+
		"\u05a2\3\2\2\2\u00ba\u05a4\3\2\2\2\u00bc\u05af\3\2\2\2\u00be\u05bb\3\2"+
		"\2\2\u00c0\u05c7\3\2\2\2\u00c2\u05d3\3\2\2\2\u00c4\u05e1\3\2\2\2\u00c6"+
		"\u05ef\3\2\2\2\u00c8\u05f1\3\2\2\2\u00ca\u0608\3\2\2\2\u00cc\u060a\3\2"+
		"\2\2\u00ce\u0614\3\2\2\2\u00d0\u0622\3\2\2\2\u00d2\u0627\3\2\2\2\u00d4"+
		"\u0630\3\2\2\2\u00d6\u063c\3\2\2\2\u00d8\u063e\3\2\2\2\u00da\u0640\3\2"+
		"\2\2\u00dc\u0642\3\2\2\2\u00de\u064d\3\2\2\2\u00e0\u0665\3\2\2\2\u00e2"+
		"\u067d\3\2\2\2\u00e4\u0695\3\2\2\2\u00e6\u0697\3\2\2\2\u00e8\u0699\3\2"+
		"\2\2\u00ea\u069b\3\2\2\2\u00ec\u069e\3\2\2\2\u00ee\u06a6\3\2\2\2\u00f0"+
		"\u06ae\3\2\2\2\u00f2\u06b7\3\2\2\2\u00f4\u06ba\3\2\2\2\u00f6\u06c0\3\2"+
		"\2\2\u00f8\u06c6\3\2\2\2\u00fa\u06cc\3\2\2\2\u00fc\u06d2\3\2\2\2\u00fe"+
		"\u06d8\3\2\2\2\u0100\u06e2\3\2\2\2\u0102\u06e9\3\2\2\2\u0104\u06f0\3\2"+
		"\2\2\u0106\u06f2\3\2\2\2\u0108\u06f4\3\2\2\2\u010a\u06f6\3\2\2\2\u010c"+
		"\u0701\3\2\2\2\u010e\u0720\3\2\2\2\u0110\u0723\3\2\2\2\u0112\u072d\3\2"+
		"\2\2\u0114\u0746\3\2\2\2\u0116\u0748\3\2\2\2\u0118\u0765\3\2\2\2\u011a"+
		"\u0767\3\2\2\2\u011c\u0769\3\2\2\2\u011e\u076b\3\2\2\2\u0120\u076d\3\2"+
		"\2\2\u0122\u076f\3\2\2\2\u0124\u0777\3\2\2\2\u0126\u0779\3\2\2\2\u0128"+
		"\u0787\3\2\2\2\u012a\u0789\3\2\2\2\u012c\u078b\3\2\2\2\u012e\u0793\3\2"+
		"\2\2\u0130\u079b\3\2\2\2\u0132\u07a5\3\2\2\2\u0134\u0135\7I\2\2\u0135"+
		"\3\3\2\2\2\u0136\u0138\5P)\2\u0137\u0136\3\2\2\2\u0138\u013b\3\2\2\2\u0139"+
		"\u0137\3\2\2\2\u0139\u013a\3\2\2\2\u013a\u013f\3\2\2\2\u013b\u0139\3\2"+
		"\2\2\u013c\u013e\5X-\2\u013d\u013c\3\2\2\2\u013e\u0141\3\2\2\2\u013f\u013d"+
		"\3\2\2\2\u013f\u0140\3\2\2\2\u0140\u0143\3\2\2\2\u0141\u013f\3\2\2\2\u0142"+
		"\u0144\7J\2\2\u0143\u0142\3\2\2\2\u0143\u0144\3\2\2\2\u0144\u0145\3\2"+
		"\2\2\u0145\u0146\7\2\2\3\u0146\5\3\2\2\2\u0147\u0149\7J\2\2\u0148\u0147"+
		"\3\2\2\2\u0148\u0149\3\2\2\2\u0149\u014a\3\2\2\2\u014a\u014c\7K\2\2\u014b"+
		"\u0148\3\2\2\2\u014b\u014c\3\2\2\2\u014c\7\3\2\2\2\u014d\u014e\t\2\2\2"+
		"\u014e\t\3\2\2\2\u014f\u0154\5\b\5\2\u0150\u0151\7\3\2\2\u0151\u0153\5"+
		"\b\5\2\u0152\u0150\3\2\2\2\u0153\u0156\3\2\2\2\u0154\u0152\3\2\2\2\u0154"+
		"\u0155\3\2\2\2\u0155\13\3\2\2\2\u0156\u0154\3\2\2\2\u0157\u0158\5\n\6"+
		"\2\u0158\u0159\7\4\2\2\u0159\u015b\3\2\2\2\u015a\u0157\3\2\2\2\u015a\u015b"+
		"\3\2\2\2\u015b\u015c\3\2\2\2\u015c\u015d\7a\2\2\u015d\r\3\2\2\2\u015e"+
		"\u015f\5\n\6\2\u015f\u0160\7\4\2\2\u0160\u0162\3\2\2\2\u0161\u015e\3\2"+
		"\2\2\u0161\u0162\3\2\2\2\u0162\u0163\3\2\2\2\u0163\u0164\7b\2\2\u0164"+
		"\17\3\2\2\2\u0165\u0166\7a\2\2\u0166\21\3\2\2\2\u0167\u0168\7a\2\2\u0168"+
		"\23\3\2\2\2\u0169\u016a\7b\2\2\u016a\25\3\2\2\2\u016b\u016c\7a\2\2\u016c"+
		"\27\3\2\2\2\u016d\u016e\5\f\7\2\u016e\31\3\2\2\2\u016f\u0170\7b\2\2\u0170"+
		"\33\3\2\2\2\u0171\u0172\5\16\b\2\u0172\35\3\2\2\2\u0173\u0174\t\3\2\2"+
		"\u0174\37\3\2\2\2\u0175\u0176\7b\2\2\u0176!\3\2\2\2\u0177\u0178\5\16\b"+
		"\2\u0178#\3\2\2\2\u0179\u017a\7a\2\2\u017a%\3\2\2\2\u017b\u0186\5$\23"+
		"\2\u017c\u017e\7J\2\2\u017d\u017c\3\2\2\2\u017d\u017e\3\2\2\2\u017e\u017f"+
		"\3\2\2\2\u017f\u0181\7\5\2\2\u0180\u0182\7J\2\2\u0181\u0180\3\2\2\2\u0181"+
		"\u0182\3\2\2\2\u0182\u0183\3\2\2\2\u0183\u0185\5$\23\2\u0184\u017d\3\2"+
		"\2\2\u0185\u0188\3\2\2\2\u0186\u0184\3\2\2\2\u0186\u0187\3\2\2\2\u0187"+
		"\'\3\2\2\2\u0188\u0186\3\2\2\2\u0189\u018a\5$\23\2\u018a\u018c\7\6\2\2"+
		"\u018b\u018d\7J\2\2\u018c\u018b\3\2\2\2\u018c\u018d\3\2\2\2\u018d\u018e"+
		"\3\2\2\2\u018e\u018f\5\u010a\u0086\2\u018f)\3\2\2\2\u0190\u019b\5(\25"+
		"\2\u0191\u0193\7J\2\2\u0192\u0191\3\2\2\2\u0192\u0193\3\2\2\2\u0193\u0194"+
		"\3\2\2\2\u0194\u0196\7\5\2\2\u0195\u0197\7J\2\2\u0196\u0195\3\2\2\2\u0196"+
		"\u0197\3\2\2\2\u0197\u0198\3\2\2\2\u0198\u019a\5(\25\2\u0199\u0192\3\2"+
		"\2\2\u019a\u019d\3\2\2\2\u019b\u0199\3\2\2\2\u019b\u019c\3\2\2\2\u019c"+
		"+\3\2\2\2\u019d\u019b\3\2\2\2\u019e\u01a0\7\7\2\2\u019f\u01a1\7J\2\2\u01a0"+
		"\u019f\3\2\2\2\u01a0\u01a1\3\2\2\2\u01a1\u01a3\3\2\2\2\u01a2\u01a4\5*"+
		"\26\2\u01a3\u01a2\3\2\2\2\u01a3\u01a4\3\2\2\2\u01a4\u01a6\3\2\2\2\u01a5"+
		"\u01a7\7J\2\2\u01a6\u01a5\3\2\2\2\u01a6\u01a7\3\2\2\2\u01a7\u01a8\3\2"+
		"\2\2\u01a8\u01aa\7\b\2\2\u01a9\u019e\3\2\2\2\u01a9\u01aa\3\2\2\2\u01aa"+
		"-\3\2\2\2\u01ab\u01ad\5\22\n\2\u01ac\u01ae\7J\2\2\u01ad\u01ac\3\2\2\2"+
		"\u01ad\u01ae\3\2\2\2\u01ae\u01af\3\2\2\2\u01af\u01b1\7\6\2\2\u01b0\u01b2"+
		"\7J\2\2\u01b1\u01b0\3\2\2\2\u01b1\u01b2\3\2\2\2\u01b2\u01b3\3\2\2\2\u01b3"+
		"\u01b4\5\u010a\u0086\2\u01b4/\3\2\2\2\u01b5\u01c0\5.\30\2\u01b6\u01b8"+
		"\7J\2\2\u01b7\u01b6\3\2\2\2\u01b7\u01b8\3\2\2\2\u01b8\u01b9\3\2\2\2\u01b9"+
		"\u01bb\7\5\2\2\u01ba\u01bc\7J\2\2\u01bb\u01ba\3\2\2\2\u01bb\u01bc\3\2"+
		"\2\2\u01bc\u01bd\3\2\2\2\u01bd\u01bf\5.\30\2\u01be\u01b7\3\2\2\2\u01bf"+
		"\u01c2\3\2\2\2\u01c0\u01be\3\2\2\2\u01c0\u01c1\3\2\2\2\u01c1\61\3\2\2"+
		"\2\u01c2\u01c0\3\2\2\2\u01c3\u01c5\7\t\2\2\u01c4\u01c6\7J\2\2\u01c5\u01c4"+
		"\3\2\2\2\u01c5\u01c6\3\2\2\2\u01c6\u01d5\3\2\2\2\u01c7\u01d2\5\22\n\2"+
		"\u01c8\u01ca\7J\2\2\u01c9\u01c8\3\2\2\2\u01c9\u01ca\3\2\2\2\u01ca\u01cb"+
		"\3\2\2\2\u01cb\u01cd\7\5\2\2\u01cc\u01ce\7J\2\2\u01cd\u01cc\3\2\2\2\u01cd"+
		"\u01ce\3\2\2\2\u01ce\u01cf\3\2\2\2\u01cf\u01d1\5\22\n\2\u01d0\u01c9\3"+
		"\2\2\2\u01d1\u01d4\3\2\2\2\u01d2\u01d0\3\2\2\2\u01d2\u01d3\3\2\2\2\u01d3"+
		"\u01d6\3\2\2\2\u01d4\u01d2\3\2\2\2\u01d5\u01c7\3\2\2\2\u01d5\u01d6\3\2"+
		"\2\2\u01d6\u01d8\3\2\2\2\u01d7\u01d9\7J\2\2\u01d8\u01d7\3\2\2\2\u01d8"+
		"\u01d9\3\2\2\2\u01d9\u01da\3\2\2\2\u01da\u01db\7\n\2\2\u01db\63\3\2\2"+
		"\2\u01dc\u01e7\5\62\32\2\u01dd\u01df\7J\2\2\u01de\u01dd\3\2\2\2\u01de"+
		"\u01df\3\2\2\2\u01df\u01e0\3\2\2\2\u01e0\u01e2\7\5\2\2\u01e1\u01e3\7J"+
		"\2\2\u01e2\u01e1\3\2\2\2\u01e2\u01e3\3\2\2\2\u01e3\u01e4\3\2\2\2\u01e4"+
		"\u01e6\5\62\32\2\u01e5\u01de\3\2\2\2\u01e6\u01e9\3\2\2\2\u01e7\u01e5\3"+
		"\2\2\2\u01e7\u01e8\3\2\2\2\u01e8\65\3\2\2\2\u01e9\u01e7\3\2\2\2\u01ea"+
		"\u01eb\7]\2\2\u01eb\u01ec\7J\2\2\u01ec\u01ee\5\u00c6d\2\u01ed\u01ef\7"+
		"J\2\2\u01ee\u01ed\3\2\2\2\u01ee\u01ef\3\2\2\2\u01ef\u01f0\3\2\2\2\u01f0"+
		"\u01f2\7\13\2\2\u01f1\u01f3\7J\2\2\u01f2\u01f1\3\2\2\2\u01f2\u01f3\3\2"+
		"\2\2\u01f3\u01f4\3\2\2\2\u01f4\u01f6\5\u0082B\2\u01f5\u01f7\7K\2\2\u01f6"+
		"\u01f5\3\2\2\2\u01f6\u01f7\3\2\2\2\u01f7\67\3\2\2\2\u01f8\u01ff\5\66\34"+
		"\2\u01f9\u01fb\7J\2\2\u01fa\u01f9\3\2\2\2\u01fa\u01fb\3\2\2\2\u01fb\u01fc"+
		"\3\2\2\2\u01fc\u01fe\5\66\34\2\u01fd\u01fa\3\2\2\2\u01fe\u0201\3\2\2\2"+
		"\u01ff\u01fd\3\2\2\2\u01ff\u0200\3\2\2\2\u02009\3\2\2\2\u0201\u01ff\3"+
		"\2\2\2\u0202\u0203\7]\2\2\u0203\u0204\7J\2\2\u0204\u0206\5\u0082B\2\u0205"+
		"\u0207\7J\2\2\u0206\u0205\3\2\2\2\u0206\u0207\3\2\2\2\u0207\u0208\3\2"+
		"\2\2\u0208\u020a\7\13\2\2\u0209\u020b\7J\2\2\u020a\u0209\3\2\2\2\u020a"+
		"\u020b\3\2\2\2\u020b\u020c\3\2\2\2\u020c\u020e\5\u0082B\2\u020d\u020f"+
		"\7K\2\2\u020e\u020d\3\2\2\2\u020e\u020f\3\2\2\2\u020f;\3\2\2\2\u0210\u0217"+
		"\5:\36\2\u0211\u0213\7J\2\2\u0212\u0211\3\2\2\2\u0212\u0213\3\2\2\2\u0213"+
		"\u0214\3\2\2\2\u0214\u0216\5:\36\2\u0215\u0212\3\2\2\2\u0216\u0219\3\2"+
		"\2\2\u0217\u0215\3\2\2\2\u0217\u0218\3\2\2\2\u0218=\3\2\2\2\u0219\u0217"+
		"\3\2\2\2\u021a\u0223\5$\23\2\u021b\u021d\7J\2\2\u021c\u021b\3\2\2\2\u021c"+
		"\u021d\3\2\2\2\u021d\u021e\3\2\2\2\u021e\u0220\7\6\2\2\u021f\u0221\7J"+
		"\2\2\u0220\u021f\3\2\2\2\u0220\u0221\3\2\2\2\u0221\u0222\3\2\2\2\u0222"+
		"\u0224\5\u010a\u0086\2\u0223\u021c\3\2\2\2\u0223\u0224\3\2\2\2\u0224?"+
		"\3\2\2\2\u0225\u0227\7\f\2\2\u0226\u0228\7J\2\2\u0227\u0226\3\2\2\2\u0227"+
		"\u0228\3\2\2\2\u0228\u0229\3\2\2\2\u0229\u0234\5> \2\u022a\u022c\7J\2"+
		"\2\u022b\u022a\3\2\2\2\u022b\u022c\3\2\2\2\u022c\u022d\3\2\2\2\u022d\u022f"+
		"\7\5\2\2\u022e\u0230\7J\2\2\u022f\u022e\3\2\2\2\u022f\u0230\3\2\2\2\u0230"+
		"\u0231\3\2\2\2\u0231\u0233\5> \2\u0232\u022b\3\2\2\2\u0233\u0236\3\2\2"+
		"\2\u0234\u0232\3\2\2\2\u0234\u0235\3\2\2\2\u0235\u0237\3\2\2\2\u0236\u0234"+
		"\3\2\2\2\u0237\u0238\7\r\2\2\u0238\u023a\3\2\2\2\u0239\u0225\3\2\2\2\u0239"+
		"\u023a\3\2\2\2\u023aA\3\2\2\2\u023b\u023c\7\f\2\2\u023c\u0247\5\u010a"+
		"\u0086\2\u023d\u023f\7J\2\2\u023e\u023d\3\2\2\2\u023e\u023f\3\2\2\2\u023f"+
		"\u0240\3\2\2\2\u0240\u0242\7\5\2\2\u0241\u0243\7J\2\2\u0242\u0241\3\2"+
		"\2\2\u0242\u0243\3\2\2\2\u0243\u0244\3\2\2\2\u0244\u0246\5\u010a\u0086"+
		"\2\u0245\u023e\3\2\2\2\u0246\u0249\3\2\2\2\u0247\u0245\3\2\2\2\u0247\u0248"+
		"\3\2\2\2\u0248\u024a\3\2\2\2\u0249\u0247\3\2\2\2\u024a\u024b\7\r\2\2\u024b"+
		"C\3\2\2\2\u024c\u024d\5\24\13\2\u024d\u024e\5B\"\2\u024eE\3\2\2\2\u024f"+
		"\u025a\5D#\2\u0250\u0252\7J\2\2\u0251\u0250\3\2\2\2\u0251\u0252\3\2\2"+
		"\2\u0252\u0253\3\2\2\2\u0253\u0255\7\5\2\2\u0254\u0256\7J\2\2\u0255\u0254"+
		"\3\2\2\2\u0255\u0256\3\2\2\2\u0256\u0257\3\2\2\2\u0257\u0259\5D#\2\u0258"+
		"\u0251\3\2\2\2\u0259\u025c\3\2\2\2\u025a\u0258\3\2\2\2\u025a\u025b\3\2"+
		"\2\2\u025bG\3\2\2\2\u025c\u025a\3\2\2\2\u025d\u025f\7J\2\2\u025e\u025d"+
		"\3\2\2\2\u025e\u025f\3\2\2\2\u025f\u0260\3\2\2\2\u0260\u0262\7\13\2\2"+
		"\u0261\u0263\7J\2\2\u0262\u0261\3\2\2\2\u0262\u0263\3\2\2\2\u0263\u0264"+
		"\3\2\2\2\u0264\u0266\5F$\2\u0265\u025e\3\2\2\2\u0265\u0266\3\2\2\2\u0266"+
		"I\3\2\2\2\u0267\u0269\7J\2\2\u0268\u0267\3\2\2\2\u0268\u0269\3\2\2\2\u0269"+
		"\u026a\3\2\2\2\u026a\u026c\7\16\2\2\u026b\u026d\7J\2\2\u026c\u026b\3\2"+
		"\2\2\u026c\u026d\3\2\2\2\u026d\u026e\3\2\2\2\u026e\u0270\5F$\2\u026f\u0268"+
		"\3\2\2\2\u026f\u0270\3\2\2\2\u0270K\3\2\2\2\u0271\u0272\7\17\2\2\u0272"+
		"\u0273\5\20\t\2\u0273M\3\2\2\2\u0274\u0279\5L\'\2\u0275\u0276\7J\2\2\u0276"+
		"\u0278\5L\'\2\u0277\u0275\3\2\2\2\u0278\u027b\3\2\2\2\u0279\u0277\3\2"+
		"\2\2\u0279\u027a\3\2\2\2\u027aO\3\2\2\2\u027b\u0279\3\2\2\2\u027c\u027e"+
		"\7J\2\2\u027d\u027c\3\2\2\2\u027d\u027e\3\2\2\2\u027e\u0282\3\2\2\2\u027f"+
		"\u0283\5R*\2\u0280\u0283\5T+\2\u0281\u0283\5V,\2\u0282\u027f\3\2\2\2\u0282"+
		"\u0280\3\2\2\2\u0282\u0281\3\2\2\2\u0283Q\3\2\2\2\u0284\u0285\7`\2\2\u0285"+
		"\u0286\7J\2\2\u0286\u0287\5\n\6\2\u0287\u0288\7\4\2\2\u0288\u0289\7\\"+
		"\2\2\u0289\u028a\5\6\4\2\u028aS\3\2\2\2\u028b\u028c\7`\2\2\u028c\u028d"+
		"\7J\2\2\u028d\u028e\5\n\6\2\u028e\u028f\7\4\2\2\u028f\u0290\5\b\5\2\u0290"+
		"\u0291\5\6\4\2\u0291U\3\2\2\2\u0292\u0293\7`\2\2\u0293\u0294\7J\2\2\u0294"+
		"\u0295\5\n\6\2\u0295\u0296\5\6\4\2\u0296W\3\2\2\2\u0297\u02a6\5Z.\2\u0298"+
		"\u02a6\5\\/\2\u0299\u02a6\5b\62\2\u029a\u02a6\5d\63\2\u029b\u02a6\5f\64"+
		"\2\u029c\u02a6\5h\65\2\u029d\u02a6\5j\66\2\u029e\u02a6\5l\67\2\u029f\u02a6"+
		"\5n8\2\u02a0\u02a6\5p9\2\u02a1\u02a6\5v<\2\u02a2\u02a6\5x=\2\u02a3\u02a6"+
		"\5~@\2\u02a4\u02a6\5|?\2\u02a5\u0297\3\2\2\2\u02a5\u0298\3\2\2\2\u02a5"+
		"\u0299\3\2\2\2\u02a5\u029a\3\2\2\2\u02a5\u029b\3\2\2\2\u02a5\u029c\3\2"+
		"\2\2\u02a5\u029d\3\2\2\2\u02a5\u029e\3\2\2\2\u02a5\u029f\3\2\2\2\u02a5"+
		"\u02a0\3\2\2\2\u02a5\u02a1\3\2\2\2\u02a5\u02a2\3\2\2\2\u02a5\u02a3\3\2"+
		"\2\2\u02a5\u02a4\3\2\2\2\u02a6Y\3\2\2\2\u02a7\u02a9\7J\2\2\u02a8\u02a7"+
		"\3\2\2\2\u02a8\u02a9\3\2\2\2\u02a9\u02aa\3\2\2\2\u02aa\u02ab\7N\2\2\u02ab"+
		"\u02ac\7J\2\2\u02ac\u02ae\5\n\6\2\u02ad\u02af\7J\2\2\u02ae\u02ad\3\2\2"+
		"\2\u02ae\u02af\3\2\2\2\u02af\u02b0\3\2\2\2\u02b0\u02b2\7\t\2\2\u02b1\u02b3"+
		"\7J\2\2\u02b2\u02b1\3\2\2\2\u02b2\u02b3\3\2\2\2\u02b3\u02b7\3\2\2\2\u02b4"+
		"\u02b6\5X-\2\u02b5\u02b4\3\2\2\2\u02b6\u02b9\3\2\2\2\u02b7\u02b5\3\2\2"+
		"\2\u02b7\u02b8\3\2\2\2\u02b8\u02bb\3\2\2\2\u02b9\u02b7\3\2\2\2\u02ba\u02bc"+
		"\7J\2\2\u02bb\u02ba\3\2\2\2\u02bb\u02bc\3\2\2\2\u02bc\u02bd\3\2\2\2\u02bd"+
		"\u02be\7\n\2\2\u02be\u02bf\5\6\4\2\u02bf[\3\2\2\2\u02c0\u02c2\7J\2\2\u02c1"+
		"\u02c0\3\2\2\2\u02c1\u02c2\3\2\2\2\u02c2\u02c3\3\2\2\2\u02c3\u02c5\5\2"+
		"\2\2\u02c4\u02c1\3\2\2\2\u02c5\u02c8\3\2\2\2\u02c6\u02c4\3\2\2\2\u02c6"+
		"\u02c7\3\2\2\2\u02c7\u02ca\3\2\2\2\u02c8\u02c6\3\2\2\2\u02c9\u02cb\7J"+
		"\2\2\u02ca\u02c9\3\2\2\2\u02ca\u02cb\3\2\2\2\u02cb\u02cc\3\2\2\2\u02cc"+
		"\u02cd\7M\2\2\u02cd\u02ce\7J\2\2\u02ce\u02cf\5 \21\2\u02cf\u02d1\5@!\2"+
		"\u02d0\u02d2\7J\2\2\u02d1\u02d0\3\2\2\2\u02d1\u02d2\3\2\2\2\u02d2\u02d3"+
		"\3\2\2\2\u02d3\u02d5\7\t\2\2\u02d4\u02d6\7J\2\2\u02d5\u02d4\3\2\2\2\u02d5"+
		"\u02d6\3\2\2\2\u02d6\u02d7\3\2\2\2\u02d7\u02d9\5^\60\2\u02d8\u02da\7J"+
		"\2\2\u02d9\u02d8\3\2\2\2\u02d9\u02da\3\2\2\2\u02da\u02db\3\2\2\2\u02db"+
		"\u02dc\7\n\2\2\u02dc\u02dd\5\6\4\2\u02dd]\3\2\2\2\u02de\u02e9\5`\61\2"+
		"\u02df\u02e1\7J\2\2\u02e0\u02df\3\2\2\2\u02e0\u02e1\3\2\2\2\u02e1\u02e2"+
		"\3\2\2\2\u02e2\u02e4\7\5\2\2\u02e3\u02e5\7J\2\2\u02e4\u02e3\3\2\2\2\u02e4"+
		"\u02e5\3\2\2\2\u02e5\u02e6\3\2\2\2\u02e6\u02e8\5`\61\2\u02e7\u02e0\3\2"+
		"\2\2\u02e8\u02eb\3\2\2\2\u02e9\u02e7\3\2\2\2\u02e9\u02ea\3\2\2\2\u02ea"+
		"_\3\2\2\2\u02eb\u02e9\3\2\2\2\u02ec\u02ed\7]\2\2\u02ed\u02ee\7J\2\2\u02ee"+
		"\u02f0\5\36\20\2\u02ef\u02f1\5\u0114\u008b\2\u02f0\u02ef\3\2\2\2\u02f0"+
		"\u02f1\3\2\2\2\u02f1a\3\2\2\2\u02f2\u02f4\7J\2\2\u02f3\u02f2\3\2\2\2\u02f3"+
		"\u02f4\3\2\2\2\u02f4\u02f5\3\2\2\2\u02f5\u02f7\5\2\2\2\u02f6\u02f3\3\2"+
		"\2\2\u02f7\u02fa\3\2\2\2\u02f8\u02f6\3\2\2\2\u02f8\u02f9\3\2\2\2\u02f9"+
		"\u02fc\3\2\2\2\u02fa\u02f8\3\2\2\2\u02fb\u02fd\7J\2\2\u02fc\u02fb\3\2"+
		"\2\2\u02fc\u02fd\3\2\2\2\u02fd\u02fe\3\2\2\2\u02fe\u02ff\7O\2\2\u02ff"+
		"\u0300\7J\2\2\u0300\u0302\5\32\16\2\u0301\u0303\7J\2\2\u0302\u0301\3\2"+
		"\2\2\u0302\u0303\3\2\2\2\u0303\u0304\3\2\2\2\u0304\u0306\7\7\2\2\u0305"+
		"\u0307\7J\2\2\u0306\u0305\3\2\2\2\u0306\u0307\3\2\2\2\u0307\u0309\3\2"+
		"\2\2\u0308\u030a\5\60\31\2\u0309\u0308\3\2\2\2\u0309\u030a\3\2\2\2\u030a"+
		"\u030c\3\2\2\2\u030b\u030d\7J\2\2\u030c\u030b\3\2\2\2\u030c\u030d\3\2"+
		"\2\2\u030d\u030e\3\2\2\2\u030e\u030f\7\b\2\2\u030f\u0310\5\6\4\2\u0310"+
		"c\3\2\2\2\u0311\u0313\7J\2\2\u0312\u0311\3\2\2\2\u0312\u0313\3\2\2\2\u0313"+
		"\u0314\3\2\2\2\u0314\u0316\5\2\2\2\u0315\u0312\3\2\2\2\u0316\u0319\3\2"+
		"\2\2\u0317\u0315\3\2\2\2\u0317\u0318\3\2\2\2\u0318\u031b\3\2\2\2\u0319"+
		"\u0317\3\2\2\2\u031a\u031c\7J\2\2\u031b\u031a\3\2\2\2\u031b\u031c\3\2"+
		"\2\2\u031c\u031d\3\2\2\2\u031d\u031e\7P\2\2\u031e\u031f\7J\2\2\u031f\u0321"+
		"\5\32\16\2\u0320\u0322\7J\2\2\u0321\u0320\3\2\2\2\u0321\u0322\3\2\2\2"+
		"\u0322\u0323\3\2\2\2\u0323\u0325\7\7\2\2\u0324\u0326\7J\2\2\u0325\u0324"+
		"\3\2\2\2\u0325\u0326\3\2\2\2\u0326\u0328\3\2\2\2\u0327\u0329\5\60\31\2"+
		"\u0328\u0327\3\2\2\2\u0328\u0329\3\2\2\2\u0329\u032b\3\2\2\2\u032a\u032c"+
		"\7J\2\2\u032b\u032a\3\2\2\2\u032b\u032c\3\2\2\2\u032c\u032d\3\2\2\2\u032d"+
		"\u032e\7\b\2\2\u032e\u032f\5\6\4\2\u032fe\3\2\2\2\u0330\u0332\7J\2\2\u0331"+
		"\u0330\3\2\2\2\u0331\u0332\3\2\2\2\u0332\u0333\3\2\2\2\u0333\u0334\7Q"+
		"\2\2\u0334\u0335\7J\2\2\u0335\u0337\5\34\17\2\u0336\u0338\7J\2\2\u0337"+
		"\u0336\3\2\2\2\u0337\u0338\3\2\2\2\u0338\u0339\3\2\2\2\u0339\u033b\7\7"+
		"\2\2\u033a\u033c\7J\2\2\u033b\u033a\3\2\2\2\u033b\u033c\3\2\2\2\u033c"+
		"\u033e\3\2\2\2\u033d\u033f\5\64\33\2\u033e\u033d\3\2\2\2\u033e\u033f\3"+
		"\2\2\2\u033f\u0341\3\2\2\2\u0340\u0342\7J\2\2\u0341\u0340\3\2\2\2\u0341"+
		"\u0342\3\2\2\2\u0342\u0343\3\2\2\2\u0343\u0344\7\b\2\2\u0344\u0345\5\6"+
		"\4\2\u0345g\3\2\2\2\u0346\u0348\7J\2\2\u0347\u0346\3\2\2\2\u0347\u0348"+
		"\3\2\2\2\u0348\u0349\3\2\2\2\u0349\u034b\5\2\2\2\u034a\u0347\3\2\2\2\u034b"+
		"\u034e\3\2\2\2\u034c\u034a\3\2\2\2\u034c\u034d\3\2\2\2\u034d\u0350\3\2"+
		"\2\2\u034e\u034c\3\2\2\2\u034f\u0351\7J\2\2\u0350\u034f\3\2\2\2\u0350"+
		"\u0351\3\2\2\2\u0351\u0352\3\2\2\2\u0352\u0353\7R\2\2\u0353\u0354\7J\2"+
		"\2\u0354\u0356\5\26\f\2\u0355\u0357\7J\2\2\u0356\u0355\3\2\2\2\u0356\u0357"+
		"\3\2\2\2\u0357\u0358\3\2\2\2\u0358\u035a\5,\27\2\u0359\u035b\7J\2\2\u035a"+
		"\u0359\3\2\2\2\u035a\u035b\3\2\2\2\u035b\u035c\3\2\2\2\u035c\u035e\7\6"+
		"\2\2\u035d\u035f\7J\2\2\u035e\u035d\3\2\2\2\u035e\u035f\3\2\2\2\u035f"+
		"\u0360\3\2\2\2\u0360\u0361\5\u010a\u0086\2\u0361\u0362\5\6\4\2\u0362i"+
		"\3\2\2\2\u0363\u0365\7J\2\2\u0364\u0363\3\2\2\2\u0364\u0365\3\2\2\2\u0365"+
		"\u0366\3\2\2\2\u0366\u0368\5\2\2\2\u0367\u0364\3\2\2\2\u0368\u036b\3\2"+
		"\2\2\u0369\u0367\3\2\2\2\u0369\u036a\3\2\2\2\u036a\u036d\3\2\2\2\u036b"+
		"\u0369\3\2\2\2\u036c\u036e\7J\2\2\u036d\u036c\3\2\2\2\u036d\u036e\3\2"+
		"\2\2\u036e\u036f\3\2\2\2\u036f\u0371\7S\2\2\u0370\u0372\7J\2\2\u0371\u0370"+
		"\3\2\2\2\u0371\u0372\3\2\2\2\u0372\u0373\3\2\2\2\u0373\u0374\7R\2\2\u0374"+
		"\u0375\7J\2\2\u0375\u0377\5\26\f\2\u0376\u0378\7J\2\2\u0377\u0376\3\2"+
		"\2\2\u0377\u0378\3\2\2\2\u0378\u0379\3\2\2\2\u0379\u037b\5,\27\2\u037a"+
		"\u037c\7J\2\2\u037b\u037a\3\2\2\2\u037b\u037c\3\2\2\2\u037c\u037d\3\2"+
		"\2\2\u037d\u037f\7\6\2\2\u037e\u0380\7J\2\2\u037f\u037e\3\2\2\2\u037f"+
		"\u0380\3\2\2\2\u0380\u0381\3\2\2\2\u0381\u0382\5\u010a\u0086\2\u0382\u0383"+
		"\5\6\4\2\u0383k\3\2\2\2\u0384\u0386\7J\2\2\u0385\u0384\3\2\2\2\u0385\u0386"+
		"\3\2\2\2\u0386\u0387\3\2\2\2\u0387\u0389\5\2\2\2\u0388\u0385\3\2\2\2\u0389"+
		"\u038c\3\2\2\2\u038a\u0388\3\2\2\2\u038a\u038b\3\2\2\2\u038b\u038e\3\2"+
		"\2\2\u038c\u038a\3\2\2\2\u038d\u038f\7J\2\2\u038e\u038d\3\2\2\2\u038e"+
		"\u038f\3\2\2\2\u038f\u0391\3\2\2\2\u0390\u0392\5N(\2\u0391\u0390\3\2\2"+
		"\2\u0391\u0392\3\2\2\2\u0392\u0394\3\2\2\2\u0393\u0395\7J\2\2\u0394\u0393"+
		"\3\2\2\2\u0394\u0395\3\2\2\2\u0395\u0396\3\2\2\2\u0396\u0397\7R\2\2\u0397"+
		"\u0398\7J\2\2\u0398\u039a\5\26\f\2\u0399\u039b\7J\2\2\u039a\u0399\3\2"+
		"\2\2\u039a\u039b\3\2\2\2\u039b\u039c\3\2\2\2\u039c\u039d\5@!\2\u039d\u039f"+
		"\5,\27\2\u039e\u03a0\7J\2\2\u039f\u039e\3\2\2\2\u039f\u03a0\3\2\2\2\u03a0"+
		"\u03a1\3\2\2\2\u03a1\u03a3\7\6\2\2\u03a2\u03a4\7J\2\2\u03a3\u03a2\3\2"+
		"\2\2\u03a3\u03a4\3\2\2\2\u03a4\u03a5\3\2\2\2\u03a5\u03a7\5\u010a\u0086"+
		"\2\u03a6\u03a8\7J\2\2\u03a7\u03a6\3\2\2\2\u03a7\u03a8\3\2\2\2\u03a8\u03a9"+
		"\3\2\2\2\u03a9\u03ab\7\20\2\2\u03aa\u03ac\7J\2\2\u03ab\u03aa\3\2\2\2\u03ab"+
		"\u03ac\3\2\2\2\u03ac\u03ad\3\2\2\2\u03ad\u03ae\5\u0082B\2\u03ae\u03af"+
		"\5\6\4\2\u03afm\3\2\2\2\u03b0\u03b2\7J\2\2\u03b1\u03b0\3\2\2\2\u03b1\u03b2"+
		"\3\2\2\2\u03b2\u03b3\3\2\2\2\u03b3\u03b5\5\2\2\2\u03b4\u03b1\3\2\2\2\u03b5"+
		"\u03b8\3\2\2\2\u03b6\u03b4\3\2\2\2\u03b6\u03b7\3\2\2\2\u03b7\u03ba\3\2"+
		"\2\2\u03b8\u03b6\3\2\2\2\u03b9\u03bb\7J\2\2\u03ba\u03b9\3\2\2\2\u03ba"+
		"\u03bb\3\2\2\2\u03bb\u03bc\3\2\2\2\u03bc\u03bd\7T\2\2\u03bd\u03be\7J\2"+
		"\2\u03be\u03c0\5\26\f\2\u03bf\u03c1\7J\2\2\u03c0\u03bf\3\2\2\2\u03c0\u03c1"+
		"\3\2\2\2\u03c1\u03c2\3\2\2\2\u03c2\u03c4\5@!\2\u03c3\u03c5\7J\2\2\u03c4"+
		"\u03c3\3\2\2\2\u03c4\u03c5\3\2\2\2\u03c5\u03c6\3\2\2\2\u03c6\u03c8\5,"+
		"\27\2\u03c7\u03c9\7J\2\2\u03c8\u03c7\3\2\2\2\u03c8\u03c9\3\2\2\2\u03c9"+
		"\u03ca\3\2\2\2\u03ca\u03cc\7\6\2\2\u03cb\u03cd\7J\2\2\u03cc\u03cb\3\2"+
		"\2\2\u03cc\u03cd\3\2\2\2\u03cd\u03ce\3\2\2\2\u03ce\u03d0\5\u010a\u0086"+
		"\2\u03cf\u03d1\7J\2\2\u03d0\u03cf\3\2\2\2\u03d0\u03d1\3\2\2\2\u03d1\u03d2"+
		"\3\2\2\2\u03d2\u03d4\7\20\2\2\u03d3\u03d5\7J\2\2\u03d4\u03d3\3\2\2\2\u03d4"+
		"\u03d5\3\2\2\2\u03d5\u03d6\3\2\2\2\u03d6\u03d7\5\u0082B\2\u03d7\u03d8"+
		"\5\6\4\2\u03d8o\3\2\2\2\u03d9\u03db\7J\2\2\u03da\u03d9\3\2\2\2\u03da\u03db"+
		"\3\2\2\2\u03db\u03dc\3\2\2\2\u03dc\u03de\5\2\2\2\u03dd\u03da\3\2\2\2\u03de"+
		"\u03e1\3\2\2\2\u03df\u03dd\3\2\2\2\u03df\u03e0\3\2\2\2\u03e0\u03e3\3\2"+
		"\2\2\u03e1\u03df\3\2\2\2\u03e2\u03e4\7J\2\2\u03e3\u03e2\3\2\2\2\u03e3"+
		"\u03e4\3\2\2\2\u03e4\u03e5\3\2\2\2\u03e5\u03e6\7U\2\2\u03e6\u03e7\7J\2"+
		"\2\u03e7\u03e8\5\24\13\2\u03e8\u03e9\5B\"\2\u03e9\u03eb\5H%\2\u03ea\u03ec"+
		"\7J\2\2\u03eb\u03ea\3\2\2\2\u03eb\u03ec\3\2\2\2\u03ec\u03ed\3\2\2\2\u03ed"+
		"\u03ee\5r:\2\u03eeq\3\2\2\2\u03ef\u03f1\7\t\2\2\u03f0\u03f2\7J\2\2\u03f1"+
		"\u03f0\3\2\2\2\u03f1\u03f2\3\2\2\2\u03f2\u03f9\3\2\2\2\u03f3\u03f5\5t"+
		";\2\u03f4\u03f6\7J\2\2\u03f5\u03f4\3\2\2\2\u03f5\u03f6\3\2\2\2\u03f6\u03f8"+
		"\3\2\2\2\u03f7\u03f3\3\2\2\2\u03f8\u03fb\3\2\2\2\u03f9\u03f7\3\2\2\2\u03f9"+
		"\u03fa\3\2\2\2\u03fa\u03fc\3\2\2\2\u03fb\u03f9\3\2\2\2\u03fc\u03fd\7\n"+
		"\2\2\u03fds\3\2\2\2\u03fe\u0402\5l\67\2\u03ff\u0402\5h\65\2\u0400\u0402"+
		"\5n8\2\u0401\u03fe\3\2\2\2\u0401\u03ff\3\2\2\2\u0401\u0400\3\2\2\2\u0402"+
		"u\3\2\2\2\u0403\u0405\7J\2\2\u0404\u0403\3\2\2\2\u0404\u0405\3\2\2\2\u0405"+
		"\u0406\3\2\2\2\u0406\u0408\5\u0124\u0093\2\u0407\u0409\7J\2\2\u0408\u0407"+
		"\3\2\2\2\u0408\u0409\3\2\2\2\u0409\u040a\3\2\2\2\u040a\u040b\7\3\2\2\u040b"+
		"w\3\2\2\2\u040c\u040e\7J\2\2\u040d\u040c\3\2\2\2\u040d\u040e\3\2\2\2\u040e"+
		"\u040f\3\2\2\2\u040f\u0411\5\u0124\u0093\2\u0410\u0412\7J\2\2\u0411\u0410"+
		"\3\2\2\2\u0411\u0412\3\2\2\2\u0412\u0413\3\2\2\2\u0413\u0415\7\21\2\2"+
		"\u0414\u0416\7J\2\2\u0415\u0414\3\2\2\2\u0415\u0416\3\2\2\2\u0416\u0417"+
		"\3\2\2\2\u0417\u0419\5\u0126\u0094\2\u0418\u041a\7J\2\2\u0419\u0418\3"+
		"\2\2\2\u0419\u041a\3\2\2\2\u041a\u041b\3\2\2\2\u041b\u041c\7\3\2\2\u041c"+
		"y\3\2\2\2\u041d\u041e\5\u0086D\2\u041e{\3\2\2\2\u041f\u0421\7J\2\2\u0420"+
		"\u041f\3\2\2\2\u0420\u0421\3\2\2\2\u0421\u0422\3\2\2\2\u0422\u0424\7V"+
		"\2\2\u0423\u0425\7J\2\2\u0424\u0423\3\2\2\2\u0424\u0425\3\2\2\2\u0425"+
		"\u0426\3\2\2\2\u0426\u0427\5\u010a\u0086\2\u0427\u0429\7\22\2\2\u0428"+
		"\u042a\7J\2\2\u0429\u0428\3\2\2\2\u0429\u042a\3\2\2\2\u042a\u042b\3\2"+
		"\2\2\u042b\u042d\7\20\2\2\u042c\u042e\7J\2\2\u042d\u042c\3\2\2\2\u042d"+
		"\u042e\3\2\2\2\u042e\u042f\3\2\2\2\u042f\u0431\7\7\2\2\u0430\u0432\7J"+
		"\2\2\u0431\u0430\3\2\2\2\u0431\u0432\3\2\2\2\u0432\u0433\3\2\2\2\u0433"+
		"\u0435\5z>\2\u0434\u0436\7J\2\2\u0435\u0434\3\2\2\2\u0435\u0436\3\2\2"+
		"\2\u0436\u0437\3\2\2\2\u0437\u0438\7\b\2\2\u0438\u0439\5\6\4\2\u0439}"+
		"\3\2\2\2\u043a\u043c\7J\2\2\u043b\u043a\3\2\2\2\u043b\u043c\3\2\2\2\u043c"+
		"\u043d\3\2\2\2\u043d\u043f\5\2\2\2\u043e\u043b\3\2\2\2\u043f\u0442\3\2"+
		"\2\2\u0440\u043e\3\2\2\2\u0440\u0441\3\2\2\2\u0441\u0444\3\2\2\2\u0442"+
		"\u0440\3\2\2\2\u0443\u0445\7J\2\2\u0444\u0443\3\2\2\2\u0444\u0445\3\2"+
		"\2\2\u0445\u0446\3\2\2\2\u0446\u0447\7W\2\2\u0447\u0448\7J\2\2\u0448\u0449"+
		"\5\24\13\2\u0449\u044b\5B\"\2\u044a\u044c\7J\2\2\u044b\u044a\3\2\2\2\u044b"+
		"\u044c\3\2\2\2\u044c\u044d\3\2\2\2\u044d\u044f\5J&\2\u044e\u0450\7J\2"+
		"\2\u044f\u044e\3\2\2\2\u044f\u0450\3\2\2\2\u0450\u0451\3\2\2\2\u0451\u0452"+
		"\5\u0080A\2\u0452\177\3\2\2\2\u0453\u0455\7\t\2\2\u0454\u0456\7J\2\2\u0455"+
		"\u0454\3\2\2\2\u0455\u0456\3\2\2\2\u0456\u045a\3\2\2\2\u0457\u0459\5l"+
		"\67\2\u0458\u0457\3\2\2\2\u0459\u045c\3\2\2\2\u045a\u0458\3\2\2\2\u045a"+
		"\u045b\3\2\2\2\u045b\u045e\3\2\2\2\u045c\u045a\3\2\2\2\u045d\u045f\7J"+
		"\2\2\u045e\u045d\3\2\2\2\u045e\u045f\3\2\2\2\u045f\u0460\3\2\2\2\u0460"+
		"\u0461\7\n\2\2\u0461\u0081\3\2\2\2\u0462\u0464\7\t\2\2\u0463\u0465\7J"+
		"\2\2\u0464\u0463\3\2\2\2\u0464\u0465\3\2\2\2\u0465\u0466\3\2\2\2\u0466"+
		"\u0468\5\u0082B\2\u0467\u0469\7J\2\2\u0468\u0467\3\2\2\2\u0468\u0469\3"+
		"\2\2\2\u0469\u046a\3\2\2\2\u046a\u046c\7\n\2\2\u046b\u046d\7J\2\2\u046c"+
		"\u046b\3\2\2\2\u046c\u046d\3\2\2\2\u046d\u0470\3\2\2\2\u046e\u0470\5\u0084"+
		"C\2\u046f\u0462\3\2\2\2\u046f\u046e\3\2\2\2\u0470\u0083\3\2\2\2\u0471"+
		"\u047b\5\u0088E\2\u0472\u0474\7J\2\2\u0473\u0472\3\2\2\2\u0473\u0474\3"+
		"\2\2\2\u0474\u0475\3\2\2\2\u0475\u0477\5\u011a\u008e\2\u0476\u0478\7J"+
		"\2\2\u0477\u0476\3\2\2\2\u0477\u0478\3\2\2\2\u0478\u0479\3\2\2\2\u0479"+
		"\u047a\5\u0088E\2\u047a\u047c\3\2\2\2\u047b\u0473\3\2\2\2\u047b\u047c"+
		"\3\2\2\2\u047c\u0085\3\2\2\2\u047d\u0488\5\u0082B\2\u047e\u0480\7J\2\2"+
		"\u047f\u047e\3\2\2\2\u047f\u0480\3\2\2\2\u0480\u0481\3\2\2\2\u0481\u0483"+
		"\7\5\2\2\u0482\u0484\7J\2\2\u0483\u0482\3\2\2\2\u0483\u0484\3\2\2\2\u0484"+
		"\u0485\3\2\2\2\u0485\u0487\5\u0082B\2\u0486\u047f\3\2\2\2\u0487\u048a"+
		"\3\2\2\2\u0488\u0486\3\2\2\2\u0488\u0489\3\2\2\2\u0489\u0087\3\2\2\2\u048a"+
		"\u0488\3\2\2\2\u048b\u0495\5\u008aF\2\u048c\u048e\7J\2\2\u048d\u048c\3"+
		"\2\2\2\u048d\u048e\3\2\2\2\u048e\u048f\3\2\2\2\u048f\u0491\5\u011c\u008f"+
		"\2\u0490\u0492\7J\2\2\u0491\u0490\3\2\2\2\u0491\u0492\3\2\2\2\u0492\u0493"+
		"\3\2\2\2\u0493\u0494\5\u008aF\2\u0494\u0496\3\2\2\2\u0495\u048d\3\2\2"+
		"\2\u0495\u0496\3\2\2\2\u0496\u0089\3\2\2\2\u0497\u04a1\5\u008cG\2\u0498"+
		"\u049a\7J\2\2\u0499\u0498\3\2\2\2\u0499\u049a\3\2\2\2\u049a\u049b\3\2"+
		"\2\2\u049b\u049d\5\u0120\u0091\2\u049c\u049e\7J\2\2\u049d\u049c\3\2\2"+
		"\2\u049d\u049e\3\2\2\2\u049e\u049f\3\2\2\2\u049f\u04a0\5\u008aF\2\u04a0"+
		"\u04a2\3\2\2\2\u04a1\u0499\3\2\2\2\u04a1\u04a2\3\2\2\2\u04a2\u008b\3\2"+
		"\2\2\u04a3\u04a4\6G\2\2\u04a4\u04a6\5\u008eH\2\u04a5\u04a7\7J\2\2\u04a6"+
		"\u04a5\3\2\2\2\u04a6\u04a7\3\2\2\2\u04a7\u04a8\3\2\2\2\u04a8\u04aa\5\u011e"+
		"\u0090\2\u04a9\u04ab\7J\2\2\u04aa\u04a9\3\2\2\2\u04aa\u04ab\3\2\2\2\u04ab"+
		"\u04ac\3\2\2\2\u04ac\u04ad\5\u008cG\2\u04ad\u04b0\3\2\2\2\u04ae\u04b0"+
		"\5\u008eH\2\u04af\u04a3\3\2\2\2\u04af\u04ae\3\2\2\2\u04b0\u008d\3\2\2"+
		"\2\u04b1\u04bd\5\u0090I\2\u04b2\u04b4\7J\2\2\u04b3\u04b2\3\2\2\2\u04b3"+
		"\u04b4\3\2\2\2\u04b4\u04b5\3\2\2\2\u04b5\u04b6\7\23\2\2\u04b6\u04b7\5"+
		"\30\r\2\u04b7\u04b9\7\23\2\2\u04b8\u04ba\7J\2\2\u04b9\u04b8\3\2\2\2\u04b9"+
		"\u04ba\3\2\2\2\u04ba\u04bb\3\2\2\2\u04bb\u04bc\5\u0090I\2\u04bc\u04be"+
		"\3\2\2\2\u04bd\u04b3\3\2\2\2\u04bd\u04be\3\2\2\2\u04be\u008f\3\2\2\2\u04bf"+
		"\u04c9\5\u0092J\2\u04c0\u04c2\7J\2\2\u04c1\u04c0\3\2\2\2\u04c1\u04c2\3"+
		"\2\2\2\u04c2\u04c3\3\2\2\2\u04c3\u04c5\5\u0122\u0092\2\u04c4\u04c6\7J"+
		"\2\2\u04c5\u04c4\3\2\2\2\u04c5\u04c6\3\2\2\2\u04c6\u04c7\3\2\2\2\u04c7"+
		"\u04c8\5\u0092J\2\u04c8\u04ca\3\2\2\2\u04c9\u04c1\3\2\2\2\u04c9\u04ca"+
		"\3\2\2\2\u04ca\u0091\3\2\2\2\u04cb\u04cc\6J\3\2\u04cc\u04ce\5\u0118\u008d"+
		"\2\u04cd\u04cf\7J\2\2\u04ce\u04cd\3\2\2\2\u04ce\u04cf\3\2\2\2\u04cf\u04d0"+
		"\3\2\2\2\u04d0\u04d1\5\u0092J\2\u04d1\u04d4\3\2\2\2\u04d2\u04d4\5\u0094"+
		"K\2\u04d3\u04cb\3\2\2\2\u04d3\u04d2\3\2\2\2\u04d4\u0093\3\2\2\2\u04d5"+
		"\u04de\5\u00ba^\2\u04d6\u04d8\7J\2\2\u04d7\u04d6\3\2\2\2\u04d7\u04d8\3"+
		"\2\2\2\u04d8\u04d9\3\2\2\2\u04d9\u04db\7\6\2\2\u04da\u04dc\7J\2\2\u04db"+
		"\u04da\3\2\2\2\u04db\u04dc\3\2\2\2\u04dc\u04dd\3\2\2\2\u04dd\u04df\5\u010a"+
		"\u0086\2\u04de\u04d7\3\2\2\2\u04de\u04df\3\2\2\2\u04df\u0095\3\2\2\2\u04e0"+
		"\u04f4\5\u0098M\2\u04e1\u04f4\5\u009aN\2\u04e2\u04f4\5\u009cO\2\u04e3"+
		"\u04f4\5\u009eP\2\u04e4\u04f4\5\u00acW\2\u04e5\u04f4\5\u00a4S\2\u04e6"+
		"\u04f4\5\u00aeX\2\u04e7\u04f4\5\u00b8]\2\u04e8\u04f4\5\u00bc_\2\u04e9"+
		"\u04f4\5\u00be`\2\u04ea\u04f4\5\u00c0a\2\u04eb\u04f4\5\u00a6T\2\u04ec"+
		"\u04f4\5\u00c2b\2\u04ed\u04f4\5\u00c4c\2\u04ee\u04f4\5\u00aaV\2\u04ef"+
		"\u04f4\5\u00a2R\2\u04f0\u04f4\5\u00b6\\\2\u04f1\u04f4\5\u00a8U\2\u04f2"+
		"\u04f4\5\u00b4[\2\u04f3\u04e0\3\2\2\2\u04f3\u04e1\3\2\2\2\u04f3\u04e2"+
		"\3\2\2\2\u04f3\u04e3\3\2\2\2\u04f3\u04e4\3\2\2\2\u04f3\u04e5\3\2\2\2\u04f3"+
		"\u04e6\3\2\2\2\u04f3\u04e7\3\2\2\2\u04f3\u04e8\3\2\2\2\u04f3\u04e9\3\2"+
		"\2\2\u04f3\u04ea\3\2\2\2\u04f3\u04eb\3\2\2\2\u04f3\u04ec\3\2\2\2\u04f3"+
		"\u04ed\3\2\2\2\u04f3\u04ee\3\2\2\2\u04f3\u04ef\3\2\2\2\u04f3\u04f0\3\2"+
		"\2\2\u04f3\u04f1\3\2\2\2\u04f3\u04f2\3\2\2\2\u04f4\u0097\3\2\2\2\u04f5"+
		"\u04f6\7V\2\2\u04f6\u04f7\7J\2\2\u04f7\u04f9\5\u00c6d\2\u04f8\u04fa\7"+
		"J\2\2\u04f9\u04f8\3\2\2\2\u04f9\u04fa\3\2\2\2\u04fa\u04fb\3\2\2\2\u04fb"+
		"\u04fd\7\20\2\2\u04fc\u04fe\7J\2\2\u04fd\u04fc\3\2\2\2\u04fd\u04fe\3\2"+
		"\2\2\u04fe\u04ff\3\2\2\2\u04ff\u0501\5\u0082B\2\u0500\u0502\7J\2\2\u0501"+
		"\u0500\3\2\2\2\u0501\u0502\3\2\2\2\u0502\u0503\3\2\2\2\u0503\u0505\7K"+
		"\2\2\u0504\u0506\7J\2\2\u0505\u0504\3\2\2\2\u0505\u0506\3\2\2\2\u0506"+
		"\u0507\3\2\2\2\u0507\u0508\5\u0082B\2\u0508\u0099\3\2\2\2\u0509\u050b"+
		"\7^\2\2\u050a\u050c\7J\2\2\u050b\u050a\3\2\2\2\u050b\u050c\3\2\2\2\u050c"+
		"\u050d\3\2\2\2\u050d\u050f\7\7\2\2\u050e\u0510\7J\2\2\u050f\u050e\3\2"+
		"\2\2\u050f\u0510\3\2\2\2\u0510\u0511\3\2\2\2\u0511\u0513\5\u0082B\2\u0512"+
		"\u0514\7J\2\2\u0513\u0512\3\2\2\2\u0513\u0514\3\2\2\2\u0514\u0515\3\2"+
		"\2\2\u0515\u0517\7\b\2\2\u0516\u0518\7J\2\2\u0517\u0516\3\2\2\2\u0517"+
		"\u0518\3\2\2\2\u0518\u0519\3\2\2\2\u0519\u051a\5\u0082B\2\u051a\u051b"+
		"\7J\2\2\u051b\u051c\7_\2\2\u051c\u051d\7J\2\2\u051d\u051e\5\u0082B\2\u051e"+
		"\u009b\3\2\2\2\u051f\u0520\7Z\2\2\u0520\u0521\7J\2\2\u0521\u0522\5\u0082"+
		"B\2\u0522\u0523\7J\2\2\u0523\u0524\7[\2\2\u0524\u0525\7J\2\2\u0525\u0527"+
		"\7\t\2\2\u0526\u0528\7J\2\2\u0527\u0526\3\2\2\2\u0527\u0528\3\2\2\2\u0528"+
		"\u0529\3\2\2\2\u0529\u052b\58\35\2\u052a\u052c\7J\2\2\u052b\u052a\3\2"+
		"\2\2\u052b\u052c\3\2\2\2\u052c\u052d\3\2\2\2\u052d\u052e\7\n\2\2\u052e"+
		"\u009d\3\2\2\2\u052f\u0530\7Y\2\2\u0530\u0531\7J\2\2\u0531\u0533\7\t\2"+
		"\2\u0532\u0534\7J\2\2\u0533\u0532\3\2\2\2\u0533\u0534\3\2\2\2\u0534\u0535"+
		"\3\2\2\2\u0535\u0537\5<\37\2\u0536\u0538\7J\2\2\u0537\u0536\3\2\2\2\u0537"+
		"\u0538\3\2\2\2\u0538\u0539\3\2\2\2\u0539\u053a\7\n\2\2\u053a\u009f\3\2"+
		"\2\2\u053b\u054a\5\u0096L\2\u053c\u053e\7J\2\2\u053d\u053c\3\2\2\2\u053d"+
		"\u053e\3\2\2\2\u053e\u053f\3\2\2\2\u053f\u0541\7\7\2\2\u0540\u0542\7J"+
		"\2\2\u0541\u0540\3\2\2\2\u0541\u0542\3\2\2\2\u0542\u0544\3\2\2\2\u0543"+
		"\u0545\5\u0086D\2\u0544\u0543\3\2\2\2\u0544\u0545\3\2\2\2\u0545\u0547"+
		"\3\2\2\2\u0546\u0548\7J\2\2\u0547\u0546\3\2\2\2\u0547\u0548\3\2\2\2\u0548"+
		"\u0549\3\2\2\2\u0549\u054b\7\b\2\2\u054a\u053d\3\2\2\2\u054a\u054b\3\2"+
		"\2\2\u054b\u00a1\3\2\2\2\u054c\u054e\5$\23\2\u054d\u054f\7J\2\2\u054e"+
		"\u054d\3\2\2\2\u054e\u054f\3\2\2\2\u054f\u0550\3\2\2\2\u0550\u0552\7\24"+
		"\2\2\u0551\u0553\7J\2\2\u0552\u0551\3\2\2\2\u0552\u0553\3\2\2\2\u0553"+
		"\u0554\3\2\2\2\u0554\u0555\5\u0082B\2\u0555\u00a3\3\2\2\2\u0556\u0558"+
		"\7\7\2\2\u0557\u0559\7J\2\2\u0558\u0557\3\2\2\2\u0558\u0559\3\2\2\2\u0559"+
		"\u055a\3\2\2\2\u055a\u055c\5&\24\2\u055b\u055d\7J\2\2\u055c\u055b\3\2"+
		"\2\2\u055c\u055d\3\2\2\2\u055d\u055e\3\2\2\2\u055e\u0560\7\b\2\2\u055f"+
		"\u0561\7J\2\2\u0560\u055f\3\2\2\2\u0560\u0561\3\2\2\2\u0561\u0562\3\2"+
		"\2\2\u0562\u0564\7\24\2\2\u0563\u0565\7J\2\2\u0564\u0563\3\2\2\2\u0564"+
		"\u0565\3\2\2\2\u0565\u0566\3\2\2\2\u0566\u0567\5\u0082B\2\u0567\u00a5"+
		"\3\2\2\2\u0568\u0569\5\u0102\u0082\2\u0569\u00a7\3\2\2\2\u056a\u056b\5"+
		"$\23\2\u056b\u00a9\3\2\2\2\u056c\u056d\5\30\r\2\u056d\u00ab\3\2\2\2\u056e"+
		"\u056f\5\"\22\2\u056f\u0570\7\3\2\2\u0570\u0572\3\2\2\2\u0571\u056e\3"+
		"\2\2\2\u0571\u0572\3\2\2\2\u0572\u0573\3\2\2\2\u0573\u0578\5\36\20\2\u0574"+
		"\u0576\7J\2\2\u0575\u0574\3\2\2\2\u0575\u0576\3\2\2\2\u0576\u0577\3\2"+
		"\2\2\u0577\u0579\5\u00aeX\2\u0578\u0575\3\2\2\2\u0578\u0579\3\2\2\2\u0579"+
		"\u00ad\3\2\2\2\u057a\u057c\7\7\2\2\u057b\u057d\7J\2\2\u057c\u057b\3\2"+
		"\2\2\u057c\u057d\3\2\2\2\u057d\u057f\3\2\2\2\u057e\u0580\5\u0086D\2\u057f"+
		"\u057e\3\2\2\2\u057f\u0580\3\2\2\2\u0580\u0582\3\2\2\2\u0581\u0583\7J"+
		"\2\2\u0582\u0581\3\2\2\2\u0582\u0583\3\2\2\2\u0583\u0584\3\2\2\2\u0584"+
		"\u0585\7\b\2\2\u0585\u00af\3\2\2\2\u0586\u0588\5\u0082B\2\u0587\u0589"+
		"\7J\2\2\u0588\u0587\3\2\2\2\u0588\u0589\3\2\2\2\u0589\u058a\3\2\2\2\u058a"+
		"\u058c\7\24\2\2\u058b\u058d\7J\2\2\u058c\u058b\3\2\2\2\u058c\u058d\3\2"+
		"\2\2\u058d\u058e\3\2\2\2\u058e\u058f\5\u0082B\2\u058f\u00b1\3\2\2\2\u0590"+
		"\u059b\5\u00b0Y\2\u0591\u0593\7J\2\2\u0592\u0591\3\2\2\2\u0592\u0593\3"+
		"\2\2\2\u0593\u0594\3\2\2\2\u0594\u0596\7\5\2\2\u0595\u0597\7J\2\2\u0596"+
		"\u0595\3\2\2\2\u0596\u0597\3\2\2\2\u0597\u0598\3\2\2\2\u0598\u059a\5\u00b0"+
		"Y\2\u0599\u0592\3\2\2\2\u059a\u059d\3\2\2\2\u059b\u0599\3\2\2\2\u059b"+
		"\u059c\3\2\2\2\u059c\u00b3\3\2\2\2\u059d\u059b\3\2\2\2\u059e\u059f\7\25"+
		"\2\2\u059f\u00b5\3\2\2\2\u05a0\u05a1\7\\\2\2\u05a1\u00b7\3\2\2\2\u05a2"+
		"\u05a3\7X\2\2\u05a3\u00b9\3\2\2\2\u05a4\u05ad\5\u00a0Q\2\u05a5\u05a7\7"+
		"J\2\2\u05a6\u05a5\3\2\2\2\u05a6\u05a7\3\2\2\2\u05a7\u05a8\3\2\2\2\u05a8"+
		"\u05aa\7\26\2\2\u05a9\u05ab\7J\2\2\u05aa\u05a9\3\2\2\2\u05aa\u05ab\3\2"+
		"\2\2\u05ab\u05ac\3\2\2\2\u05ac\u05ae\5\u0082B\2\u05ad\u05a6\3\2\2\2\u05ad"+
		"\u05ae\3\2\2\2\u05ae\u00bb\3\2\2\2\u05af\u05b1\7\27\2\2\u05b0\u05b2\7"+
		"J\2\2\u05b1\u05b0\3\2\2\2\u05b1\u05b2\3\2\2\2\u05b2\u05b4\3\2\2\2\u05b3"+
		"\u05b5\5\u0086D\2\u05b4\u05b3\3\2\2\2\u05b4\u05b5\3\2\2\2\u05b5\u05b7"+
		"\3\2\2\2\u05b6\u05b8\7J\2\2\u05b7\u05b6\3\2\2\2\u05b7\u05b8\3\2\2\2\u05b8"+
		"\u05b9\3\2\2\2\u05b9\u05ba\7\r\2\2\u05ba\u00bd\3\2\2\2\u05bb\u05bd\7\30"+
		"\2\2\u05bc\u05be\7J\2\2\u05bd\u05bc\3\2\2\2\u05bd\u05be\3\2\2\2\u05be"+
		"\u05c0\3\2\2\2\u05bf\u05c1\5\u0086D\2\u05c0\u05bf\3\2\2\2\u05c0\u05c1"+
		"\3\2\2\2\u05c1\u05c3\3\2\2\2\u05c2\u05c4\7J\2\2\u05c3\u05c2\3\2\2\2\u05c3"+
		"\u05c4\3\2\2\2\u05c4\u05c5\3\2\2\2\u05c5\u05c6\7\n\2\2\u05c6\u00bf\3\2"+
		"\2\2\u05c7\u05c9\7\31\2\2\u05c8\u05ca\7J\2\2\u05c9\u05c8\3\2\2\2\u05c9"+
		"\u05ca\3\2\2\2\u05ca\u05cc\3\2\2\2\u05cb\u05cd\5\u00b2Z\2\u05cc\u05cb"+
		"\3\2\2\2\u05cc\u05cd\3\2\2\2\u05cd\u05cf\3\2\2\2\u05ce\u05d0\7J\2\2\u05cf"+
		"\u05ce\3\2\2\2\u05cf\u05d0\3\2\2\2\u05d0\u05d1\3\2\2\2\u05d1\u05d2\7\n"+
		"\2\2\u05d2\u00c1\3\2\2\2\u05d3\u05d5\t\4\2\2\u05d4\u05d6\7J\2\2\u05d5"+
		"\u05d4\3\2\2\2\u05d5\u05d6\3\2\2\2\u05d6\u05d7\3\2\2\2\u05d7\u05d9\5,"+
		"\27\2\u05d8\u05da\7J\2\2\u05d9\u05d8\3\2\2\2\u05d9\u05da\3\2\2\2\u05da"+
		"\u05db\3\2\2\2\u05db\u05dd\7\3\2\2\u05dc\u05de\7J\2\2\u05dd\u05dc\3\2"+
		"\2\2\u05dd\u05de\3\2\2\2\u05de\u05df\3\2\2\2\u05df\u05e0\5\u0082B\2\u05e0"+
		"\u00c3\3\2\2\2\u05e1\u05e3\t\5\2\2\u05e2\u05e4\7J\2\2\u05e3\u05e2\3\2"+
		"\2\2\u05e3\u05e4\3\2\2\2\u05e4\u05e5\3\2\2\2\u05e5\u05e7\5,\27\2\u05e6"+
		"\u05e8\7J\2\2\u05e7\u05e6\3\2\2\2\u05e7\u05e8\3\2\2\2\u05e8\u05e9\3\2"+
		"\2\2\u05e9\u05eb\7\3\2\2\u05ea\u05ec\7J\2\2\u05eb\u05ea\3\2\2\2\u05eb"+
		"\u05ec\3\2\2\2\u05ec\u05ed\3\2\2\2\u05ed\u05ee\5\u0082B\2\u05ee\u00c5"+
		"\3\2\2\2\u05ef\u05f0\5\u00dco\2\u05f0\u00c7\3\2\2\2\u05f1\u05fc\5\u00c6"+
		"d\2\u05f2\u05f4\7J\2\2\u05f3\u05f2\3\2\2\2\u05f3\u05f4\3\2\2\2\u05f4\u05f5"+
		"\3\2\2\2\u05f5\u05f7\7\5\2\2\u05f6\u05f8\7J\2\2\u05f7\u05f6\3\2\2\2\u05f7"+
		"\u05f8\3\2\2\2\u05f8\u05f9\3\2\2\2\u05f9\u05fb\5\u00c6d\2\u05fa\u05f3"+
		"\3\2\2\2\u05fb\u05fe\3\2\2\2\u05fc\u05fa\3\2\2\2\u05fc\u05fd\3\2\2\2\u05fd"+
		"\u00c9\3\2\2\2\u05fe\u05fc\3\2\2\2\u05ff\u0609\5\u00d8m\2\u0600\u0609"+
		"\5\u00d0i\2\u0601\u0609\5\u00dan\2\u0602\u0609\5\u00d6l\2\u0603\u0609"+
		"\5\u00d2j\2\u0604\u0609\5\u00d4k\2\u0605\u0609\5\u00dep\2\u0606\u0609"+
		"\5\u00e0q\2\u0607\u0609\5\u00e2r\2\u0608\u05ff\3\2\2\2\u0608\u0600\3\2"+
		"\2\2\u0608\u0601\3\2\2\2\u0608\u0602\3\2\2\2\u0608\u0603\3\2\2\2\u0608"+
		"\u0604\3\2\2\2\u0608\u0605\3\2\2\2\u0608\u0606\3\2\2\2\u0608\u0607\3\2"+
		"\2\2\u0609\u00cb\3\2\2\2\u060a\u060c\5\u00c6d\2\u060b\u060d\7J\2\2\u060c"+
		"\u060b\3\2\2\2\u060c\u060d\3\2\2\2\u060d\u060e\3\2\2\2\u060e\u0610\7\24"+
		"\2\2\u060f\u0611\7J\2\2\u0610\u060f\3\2\2\2\u0610\u0611\3\2\2\2\u0611"+
		"\u0612\3\2\2\2\u0612\u0613\5\u00c6d\2\u0613\u00cd\3\2\2\2\u0614\u061f"+
		"\5\u00ccg\2\u0615\u0617\7J\2\2\u0616\u0615\3\2\2\2\u0616\u0617\3\2\2\2"+
		"\u0617\u0618\3\2\2\2\u0618\u061a\7\5\2\2\u0619\u061b\7J\2\2\u061a\u0619"+
		"\3\2\2\2\u061a\u061b\3\2\2\2\u061b\u061c\3\2\2\2\u061c\u061e\5\u00ccg"+
		"\2\u061d\u0616\3\2\2\2\u061e\u0621\3\2\2\2\u061f\u061d\3\2\2\2\u061f\u0620"+
		"\3\2\2\2\u0620\u00cf\3\2\2\2\u0621\u061f\3\2\2\2\u0622\u0623\5\u0102\u0082"+
		"\2\u0623\u00d1\3\2\2\2\u0624\u0625\5\"\22\2\u0625\u0626\7\3\2\2\u0626"+
		"\u0628\3\2\2\2\u0627\u0624\3\2\2\2\u0627\u0628\3\2\2\2\u0628\u0629\3\2"+
		"\2\2\u0629\u062e\5\36\20\2\u062a\u062c\7J\2\2\u062b\u062a\3\2\2\2\u062b"+
		"\u062c\3\2\2\2\u062c\u062d\3\2\2\2\u062d\u062f\5\u00c6d\2\u062e\u062b"+
		"\3\2\2\2\u062e\u062f\3\2\2\2\u062f\u00d3\3\2\2\2\u0630\u0632\7\7\2\2\u0631"+
		"\u0633\7J\2\2\u0632\u0631\3\2\2\2\u0632\u0633\3\2\2\2\u0633\u0635\3\2"+
		"\2\2\u0634\u0636\5\u00c8e\2\u0635\u0634\3\2\2\2\u0635\u0636\3\2\2\2\u0636"+
		"\u0638\3\2\2\2\u0637\u0639\7J\2\2\u0638\u0637\3\2\2\2\u0638\u0639\3\2"+
		"\2\2\u0639\u063a\3\2\2\2\u063a\u063b\7\b\2\2\u063b\u00d5\3\2\2\2\u063c"+
		"\u063d\7\\\2\2\u063d\u00d7\3\2\2\2\u063e\u063f\7X\2\2\u063f\u00d9\3\2"+
		"\2\2\u0640\u0641\5$\23\2\u0641\u00db\3\2\2\2\u0642\u064b\5\u00caf\2\u0643"+
		"\u0645\7J\2\2\u0644\u0643\3\2\2\2\u0644\u0645\3\2\2\2\u0645\u0646\3\2"+
		"\2\2\u0646\u0648\7\26\2\2\u0647\u0649\7J\2\2\u0648\u0647\3\2\2\2\u0648"+
		"\u0649\3\2\2\2\u0649\u064a\3\2\2\2\u064a\u064c\5\u00c6d\2\u064b\u0644"+
		"\3\2\2\2\u064b\u064c\3\2\2\2\u064c\u00dd\3\2\2\2\u064d\u064f\7\27\2\2"+
		"\u064e\u0650\7J\2\2\u064f\u064e\3\2\2\2\u064f\u0650\3\2\2\2\u0650\u0652"+
		"\3\2\2\2\u0651\u0653\5\u00c8e\2\u0652\u0651\3\2\2\2\u0652\u0653\3\2\2"+
		"\2\u0653\u065e\3\2\2\2\u0654\u0656\7J\2\2\u0655\u0654\3\2\2\2\u0655\u0656"+
		"\3\2\2\2\u0656\u0657\3\2\2\2\u0657\u0659\7\5\2\2\u0658\u065a\7J\2\2\u0659"+
		"\u0658\3\2\2\2\u0659\u065a\3\2\2\2\u065a\u065b\3\2\2\2\u065b\u065c\5\u00c6"+
		"d\2\u065c\u065d\7\36\2\2\u065d\u065f\3\2\2\2\u065e\u0655\3\2\2\2\u065e"+
		"\u065f\3\2\2\2\u065f\u0661\3\2\2\2\u0660\u0662\7J\2\2\u0661\u0660\3\2"+
		"\2\2\u0661\u0662\3\2\2\2\u0662\u0663\3\2\2\2\u0663\u0664\7\r\2\2\u0664"+
		"\u00df\3\2\2\2\u0665\u0667\7\30\2\2\u0666\u0668\7J\2\2\u0667\u0666\3\2"+
		"\2\2\u0667\u0668\3\2\2\2\u0668\u066a\3\2\2\2\u0669\u066b\5\u00c8e\2\u066a"+
		"\u0669\3\2\2\2\u066a\u066b\3\2\2\2\u066b\u0676\3\2\2\2\u066c\u066e\7J"+
		"\2\2\u066d\u066c\3\2\2\2\u066d\u066e\3\2\2\2\u066e\u066f\3\2\2\2\u066f"+
		"\u0671\7\5\2\2\u0670\u0672\7J\2\2\u0671\u0670\3\2\2\2\u0671\u0672\3\2"+
		"\2\2\u0672\u0673\3\2\2\2\u0673\u0674\5\u00c6d\2\u0674\u0675\7\36\2\2\u0675"+
		"\u0677\3\2\2\2\u0676\u066d\3\2\2\2\u0676\u0677\3\2\2\2\u0677\u0679\3\2"+
		"\2\2\u0678\u067a\7J\2\2\u0679\u0678\3\2\2\2\u0679\u067a\3\2\2\2\u067a"+
		"\u067b\3\2\2\2\u067b\u067c\7\n\2\2\u067c\u00e1\3\2\2\2\u067d\u067f\7\31"+
		"\2\2\u067e\u0680\7J\2\2\u067f\u067e\3\2\2\2\u067f\u0680\3\2\2\2\u0680"+
		"\u0682\3\2\2\2\u0681\u0683\5\u00ceh\2\u0682\u0681\3\2\2\2\u0682\u0683"+
		"\3\2\2\2\u0683\u068e\3\2\2\2\u0684\u0686\7J\2\2\u0685\u0684\3\2\2\2\u0685"+
		"\u0686\3\2\2\2\u0686\u0687\3\2\2\2\u0687\u0689\7\5\2\2\u0688\u068a\7J"+
		"\2\2\u0689\u0688\3\2\2\2\u0689\u068a\3\2\2\2\u068a\u068b\3\2\2\2\u068b"+
		"\u068c\5\u00c6d\2\u068c\u068d\7\36\2\2\u068d\u068f\3\2\2\2\u068e\u0685"+
		"\3\2\2\2\u068e\u068f\3\2\2\2\u068f\u0691\3\2\2\2\u0690\u0692\7J\2\2\u0691"+
		"\u0690\3\2\2\2\u0691\u0692\3\2\2\2\u0692\u0693\3\2\2\2\u0693\u0694\7\n"+
		"\2\2\u0694\u00e3\3\2\2\2\u0695\u0696\t\6\2\2\u0696\u00e5\3\2\2\2\u0697"+
		"\u0698\7c\2\2\u0698\u00e7\3\2\2\2\u0699\u069a\7d\2\2\u069a\u00e9\3\2\2"+
		"\2\u069b\u069c\7!\2\2\u069c\u00eb\3\2\2\2\u069d\u069f\5\u00eav\2\u069e"+
		"\u069d\3\2\2\2\u069e\u069f\3\2\2\2\u069f\u06a0\3\2\2\2\u06a0\u06a1\7e"+
		"\2\2\u06a1\u06a2\7\3\2\2\u06a2\u06a3\7e\2\2\u06a3\u06a4\7\"\2\2\u06a4"+
		"\u00ed\3\2\2\2\u06a5\u06a7\5\u00eav\2\u06a6\u06a5\3\2\2\2\u06a6\u06a7"+
		"\3\2\2\2\u06a7\u06a8\3\2\2\2\u06a8\u06a9\7e\2\2\u06a9\u06aa\7\3\2\2\u06aa"+
		"\u06ab\7e\2\2\u06ab\u06ac\7#\2\2\u06ac\u00ef\3\2\2\2\u06ad\u06af\5\u00ea"+
		"v\2\u06ae\u06ad\3\2\2\2\u06ae\u06af\3\2\2\2\u06af\u06b0\3\2\2\2\u06b0"+
		"\u06b1\7e\2\2\u06b1\u06b2\7\3\2\2\u06b2\u06b3\7e\2\2\u06b3\u00f1\3\2\2"+
		"\2\u06b4\u06b8\5\u00ecw\2\u06b5\u06b8\5\u00eex\2\u06b6\u06b8\5\u00f0y"+
		"\2\u06b7\u06b4\3\2\2\2\u06b7\u06b5\3\2\2\2\u06b7\u06b6\3\2\2\2\u06b8\u00f3"+
		"\3\2\2\2\u06b9\u06bb\5\u00eav\2\u06ba\u06b9\3\2\2\2\u06ba\u06bb\3\2\2"+
		"\2\u06bb\u06bc\3\2\2\2\u06bc\u06bd\7e\2\2\u06bd\u06be\7$\2\2\u06be\u00f5"+
		"\3\2\2\2\u06bf\u06c1\5\u00eav\2\u06c0\u06bf\3\2\2\2\u06c0\u06c1\3\2\2"+
		"\2\u06c1\u06c2\3\2\2\2\u06c2\u06c3\7e\2\2\u06c3\u06c4\7%\2\2\u06c4\u00f7"+
		"\3\2\2\2\u06c5\u06c7\5\u00eav\2\u06c6\u06c5\3\2\2\2\u06c6\u06c7\3\2\2"+
		"\2\u06c7\u06c8\3\2\2\2\u06c8\u06c9\7e\2\2\u06c9\u06ca\7&\2\2\u06ca\u00f9"+
		"\3\2\2\2\u06cb\u06cd\5\u00eav\2\u06cc\u06cb\3\2\2\2\u06cc\u06cd\3\2\2"+
		"\2\u06cd\u06ce\3\2\2\2\u06ce\u06cf\7e\2\2\u06cf\u06d0\7\'\2\2\u06d0\u00fb"+
		"\3\2\2\2\u06d1\u06d3\5\u00eav\2\u06d2\u06d1\3\2\2\2\u06d2\u06d3\3\2\2"+
		"\2\u06d3\u06d4\3\2\2\2\u06d4\u06d5\7e\2\2\u06d5\u06d6\7(\2\2\u06d6\u00fd"+
		"\3\2\2\2\u06d7\u06d9\5\u00eav\2\u06d8\u06d7\3\2\2\2\u06d8\u06d9\3\2\2"+
		"\2\u06d9\u06da\3\2\2\2\u06da\u06db\7e\2\2\u06db\u00ff\3\2\2\2\u06dc\u06e3"+
		"\5\u00f4{\2\u06dd\u06e3\5\u00f6|\2\u06de\u06e3\5\u00f8}\2\u06df\u06e3"+
		"\5\u00fa~\2\u06e0\u06e3\5\u00fc\177\2\u06e1\u06e3\5\u00fe\u0080\2\u06e2"+
		"\u06dc\3\2\2\2\u06e2\u06dd\3\2\2\2\u06e2\u06de\3\2\2\2\u06e2\u06df\3\2"+
		"\2\2\u06e2\u06e0\3\2\2\2\u06e2\u06e1\3\2\2\2\u06e3\u0101\3\2\2\2\u06e4"+
		"\u06ea\5\u00e4s\2\u06e5\u06ea\5\u00e6t\2\u06e6\u06ea\5\u00f2z\2\u06e7"+
		"\u06ea\5\u0100\u0081\2\u06e8\u06ea\5\u00e8u\2\u06e9\u06e4\3\2\2\2\u06e9"+
		"\u06e5\3\2\2\2\u06e9\u06e6\3\2\2\2\u06e9\u06e7\3\2\2\2\u06e9\u06e8\3\2"+
		"\2\2\u06ea\u0103\3\2\2\2\u06eb\u06f1\5\u010c\u0087\2\u06ec\u06f1\5\u0114"+
		"\u008b\2\u06ed\u06f1\5\u0116\u008c\2\u06ee\u06f1\5\u0106\u0084\2\u06ef"+
		"\u06f1\5\u0108\u0085\2\u06f0\u06eb\3\2\2\2\u06f0\u06ec\3\2\2\2\u06f0\u06ed"+
		"\3\2\2\2\u06f0\u06ee\3\2\2\2\u06f0\u06ef\3\2\2\2\u06f1\u0105\3\2\2\2\u06f2"+
		"\u06f3\5$\23\2\u06f3\u0107\3\2\2\2\u06f4\u06f5\5\"\22\2\u06f5\u0109\3"+
		"\2\2\2\u06f6\u06ff\5\u0104\u0083\2\u06f7\u06f9\7J\2\2\u06f8\u06f7\3\2"+
		"\2\2\u06f8\u06f9\3\2\2\2\u06f9\u06fa\3\2\2\2\u06fa\u06fc\7\24\2\2\u06fb"+
		"\u06fd\7J\2\2\u06fc\u06fb\3\2\2\2\u06fc\u06fd\3\2\2\2\u06fd\u06fe\3\2"+
		"\2\2\u06fe\u0700\5\u010a\u0086\2\u06ff\u06f8\3\2\2\2\u06ff\u0700\3\2\2"+
		"\2\u0700\u010b\3\2\2\2\u0701\u0703\7\7\2\2\u0702\u0704\7J\2\2\u0703\u0702"+
		"\3\2\2\2\u0703\u0704\3\2\2\2\u0704\u0705\3\2\2\2\u0705\u0710\5\u010a\u0086"+
		"\2\u0706\u0708\7J\2\2\u0707\u0706\3\2\2\2\u0707\u0708\3\2\2\2\u0708\u0709"+
		"\3\2\2\2\u0709\u070b\7\5\2\2\u070a\u070c\7J\2\2\u070b\u070a\3\2\2\2\u070b"+
		"\u070c\3\2\2\2\u070c\u070d\3\2\2\2\u070d\u070f\5\u010a\u0086\2\u070e\u0707"+
		"\3\2\2\2\u070f\u0712\3\2\2\2\u0710\u070e\3\2\2\2\u0710\u0711\3\2\2\2\u0711"+
		"\u0714\3\2\2\2\u0712\u0710\3\2\2\2\u0713\u0715\7J\2\2\u0714\u0713\3\2"+
		"\2\2\u0714\u0715\3\2\2\2\u0715\u0716\3\2\2\2\u0716\u0718\7\b\2\2\u0717"+
		"\u0719\7J\2\2\u0718\u0717\3\2\2\2\u0718\u0719\3\2\2\2\u0719\u071a\3\2"+
		"\2\2\u071a\u071c\7\24\2\2\u071b\u071d\7J\2\2\u071c\u071b\3\2\2\2\u071c"+
		"\u071d\3\2\2\2\u071d\u071e\3\2\2\2\u071e\u071f\5\u010a\u0086\2\u071f\u010d"+
		"\3\2\2\2\u0720\u0721\7\7\2\2\u0721\u0722\7\b\2\2\u0722\u010f\3\2\2\2\u0723"+
		"\u0725\7\7\2\2\u0724\u0726\7J\2\2\u0725\u0724\3\2\2\2\u0725\u0726\3\2"+
		"\2\2\u0726\u0727\3\2\2\2\u0727\u0729\5\u010a\u0086\2\u0728\u072a\7J\2"+
		"\2\u0729\u0728\3\2\2\2\u0729\u072a\3\2\2\2\u072a\u072b\3\2\2\2\u072b\u072c"+
		"\7\b\2\2\u072c\u0111\3\2\2\2\u072d\u072f\7\7\2\2\u072e\u0730\7J\2\2\u072f"+
		"\u072e\3\2\2\2\u072f\u0730\3\2\2\2\u0730\u0731\3\2\2\2\u0731\u073a\5\u010a"+
		"\u0086\2\u0732\u0734\7J\2\2\u0733\u0732\3\2\2\2\u0733\u0734\3\2\2\2\u0734"+
		"\u0735\3\2\2\2\u0735\u0737\7\5\2\2\u0736\u0738\7J\2\2\u0737\u0736\3\2"+
		"\2\2\u0737\u0738\3\2\2\2\u0738\u0739\3\2\2\2\u0739\u073b\5\u010a\u0086"+
		"\2\u073a\u0733\3\2\2\2\u073b\u073c\3\2\2\2\u073c\u073a\3\2\2\2\u073c\u073d"+
		"\3\2\2\2\u073d\u073f\3\2\2\2\u073e\u0740\7J\2\2\u073f\u073e\3\2\2\2\u073f"+
		"\u0740\3\2\2\2\u0740\u0741\3\2\2\2\u0741\u0742\7\b\2\2\u0742\u0113\3\2"+
		"\2\2\u0743\u0747\5\u010e\u0088\2\u0744\u0747\5\u0110\u0089\2\u0745\u0747"+
		"\5\u0112\u008a\2\u0746\u0743\3\2\2\2\u0746\u0744\3\2\2\2\u0746\u0745\3"+
		"\2\2\2\u0747\u0115\3\2\2\2\u0748\u074a\5\u0108\u0085\2\u0749\u074b\7J"+
		"\2\2\u074a\u0749\3\2\2\2\u074a\u074b\3\2\2\2\u074b\u074c\3\2\2\2\u074c"+
		"\u074e\7\f\2\2\u074d\u074f\7J\2\2\u074e\u074d\3\2\2\2\u074e\u074f\3\2"+
		"\2\2\u074f\u0750\3\2\2\2\u0750\u075b\5\u010a\u0086\2\u0751\u0753\7J\2"+
		"\2\u0752\u0751\3\2\2\2\u0752\u0753\3\2\2\2\u0753\u0754\3\2\2\2\u0754\u0756"+
		"\7\5\2\2\u0755\u0757\7J\2\2\u0756\u0755\3\2\2\2\u0756\u0757\3\2\2\2\u0757"+
		"\u0758\3\2\2\2\u0758\u075a\5\u010a\u0086\2\u0759\u0752\3\2\2\2\u075a\u075d"+
		"\3\2\2\2\u075b\u0759\3\2\2\2\u075b\u075c\3\2\2\2\u075c\u075f\3\2\2\2\u075d"+
		"\u075b\3\2\2\2\u075e\u0760\7J\2\2\u075f\u075e\3\2\2\2\u075f\u0760\3\2"+
		"\2\2\u0760\u0761\3\2\2\2\u0761\u0763\7\r\2\2\u0762\u0764\7J\2\2\u0763"+
		"\u0762\3\2\2\2\u0763\u0764\3\2\2\2\u0764\u0117\3\2\2\2\u0765\u0766\t\7"+
		"\2\2\u0766\u0119\3\2\2\2\u0767\u0768\t\b\2\2\u0768\u011b\3\2\2\2\u0769"+
		"\u076a\t\t\2\2\u076a\u011d\3\2\2\2\u076b\u076c\t\n\2\2\u076c\u011f\3\2"+
		"\2\2\u076d\u076e\t\13\2\2\u076e\u0121\3\2\2\2\u076f\u0770\t\f\2\2\u0770"+
		"\u0123\3\2\2\2\u0771\u0778\5\u0128\u0095\2\u0772\u0778\5\u012a\u0096\2"+
		"\u0773\u0778\5\u012c\u0097\2\u0774\u0778\5\u0130\u0099\2\u0775\u0778\5"+
		"\u012e\u0098\2\u0776\u0778\5\u0132\u009a\2\u0777\u0771\3\2\2\2\u0777\u0772"+
		"\3\2\2\2\u0777\u0773\3\2\2\2\u0777\u0774\3\2\2\2\u0777\u0775\3\2\2\2\u0777"+
		"\u0776\3\2\2\2\u0778\u0125\3\2\2\2\u0779\u0784\5\u0124\u0093\2\u077a\u077c"+
		"\7J\2\2\u077b\u077a\3\2\2\2\u077b\u077c\3\2\2\2\u077c\u077d\3\2\2\2\u077d"+
		"\u077f\7\5\2\2\u077e\u0780\7J\2\2\u077f\u077e\3\2\2\2\u077f\u0780\3\2"+
		"\2\2\u0780\u0781\3\2\2\2\u0781\u0783\5\u0124\u0093\2\u0782\u077b\3\2\2"+
		"\2\u0783\u0786\3\2\2\2\u0784\u0782\3\2\2\2\u0784\u0785\3\2\2\2\u0785\u0127"+
		"\3\2\2\2\u0786\u0784\3\2\2\2\u0787\u0788\7\37\2\2\u0788\u0129\3\2\2\2"+
		"\u0789\u078a\7 \2\2\u078a\u012b\3\2\2\2\u078b\u078d\5\30\r\2\u078c\u078e"+
		"\7J\2\2\u078d\u078c\3\2\2\2\u078d\u078e\3\2\2\2\u078e\u078f\3\2\2\2\u078f"+
		"\u0790\7\7\2\2\u0790\u0791\5\u0086D\2\u0791\u0792\7\b\2\2\u0792\u012d"+
		"\3\2\2\2\u0793\u0795\5\34\17\2\u0794\u0796\7J\2\2\u0795\u0794\3\2\2\2"+
		"\u0795\u0796\3\2\2\2\u0796\u0797\3\2\2\2\u0797\u0798\7\7\2\2\u0798\u0799"+
		"\5\u0086D\2\u0799\u079a\7\b\2\2\u079a\u012f\3\2\2\2\u079b\u079d\5$\23"+
		"\2\u079c\u079e\7J\2\2\u079d\u079c\3\2\2\2\u079d\u079e\3\2\2\2\u079e\u079f"+
		"\3\2\2\2\u079f\u07a1\7>\2\2\u07a0\u07a2\7J\2\2\u07a1\u07a0\3\2\2\2\u07a1"+
		"\u07a2\3\2\2\2\u07a2\u07a3\3\2\2\2\u07a3\u07a4\5$\23\2\u07a4\u0131\3\2"+
		"\2\2\u07a5\u07a7\5$\23\2\u07a6\u07a8\7J\2\2\u07a7\u07a6\3\2\2\2\u07a7"+
		"\u07a8\3\2\2\2\u07a8\u07a9\3\2\2\2\u07a9\u07ab\7H\2\2\u07aa\u07ac\7J\2"+
		"\2\u07ab\u07aa\3\2\2\2\u07ab\u07ac\3\2\2\2\u07ac\u07ad\3\2\2\2\u07ad\u07ae"+
		"\5\u0082B\2\u07ae\u0133\3\2\2\2\u014a\u0139\u013f\u0143\u0148\u014b\u0154"+
		"\u015a\u0161\u017d\u0181\u0186\u018c\u0192\u0196\u019b\u01a0\u01a3\u01a6"+
		"\u01a9\u01ad\u01b1\u01b7\u01bb\u01c0\u01c5\u01c9\u01cd\u01d2\u01d5\u01d8"+
		"\u01de\u01e2\u01e7\u01ee\u01f2\u01f6\u01fa\u01ff\u0206\u020a\u020e\u0212"+
		"\u0217\u021c\u0220\u0223\u0227\u022b\u022f\u0234\u0239\u023e\u0242\u0247"+
		"\u0251\u0255\u025a\u025e\u0262\u0265\u0268\u026c\u026f\u0279\u027d\u0282"+
		"\u02a5\u02a8\u02ae\u02b2\u02b7\u02bb\u02c1\u02c6\u02ca\u02d1\u02d5\u02d9"+
		"\u02e0\u02e4\u02e9\u02f0\u02f3\u02f8\u02fc\u0302\u0306\u0309\u030c\u0312"+
		"\u0317\u031b\u0321\u0325\u0328\u032b\u0331\u0337\u033b\u033e\u0341\u0347"+
		"\u034c\u0350\u0356\u035a\u035e\u0364\u0369\u036d\u0371\u0377\u037b\u037f"+
		"\u0385\u038a\u038e\u0391\u0394\u039a\u039f\u03a3\u03a7\u03ab\u03b1\u03b6"+
		"\u03ba\u03c0\u03c4\u03c8\u03cc\u03d0\u03d4\u03da\u03df\u03e3\u03eb\u03f1"+
		"\u03f5\u03f9\u0401\u0404\u0408\u040d\u0411\u0415\u0419\u0420\u0424\u0429"+
		"\u042d\u0431\u0435\u043b\u0440\u0444\u044b\u044f\u0455\u045a\u045e\u0464"+
		"\u0468\u046c\u046f\u0473\u0477\u047b\u047f\u0483\u0488\u048d\u0491\u0495"+
		"\u0499\u049d\u04a1\u04a6\u04aa\u04af\u04b3\u04b9\u04bd\u04c1\u04c5\u04c9"+
		"\u04ce\u04d3\u04d7\u04db\u04de\u04f3\u04f9\u04fd\u0501\u0505\u050b\u050f"+
		"\u0513\u0517\u0527\u052b\u0533\u0537\u053d\u0541\u0544\u0547\u054a\u054e"+
		"\u0552\u0558\u055c\u0560\u0564\u0571\u0575\u0578\u057c\u057f\u0582\u0588"+
		"\u058c\u0592\u0596\u059b\u05a6\u05aa\u05ad\u05b1\u05b4\u05b7\u05bd\u05c0"+
		"\u05c3\u05c9\u05cc\u05cf\u05d5\u05d9\u05dd\u05e3\u05e7\u05eb\u05f3\u05f7"+
		"\u05fc\u0608\u060c\u0610\u0616\u061a\u061f\u0627\u062b\u062e\u0632\u0635"+
		"\u0638\u0644\u0648\u064b\u064f\u0652\u0655\u0659\u065e\u0661\u0667\u066a"+
		"\u066d\u0671\u0676\u0679\u067f\u0682\u0685\u0689\u068e\u0691\u069e\u06a6"+
		"\u06ae\u06b7\u06ba\u06c0\u06c6\u06cc\u06d2\u06d8\u06e2\u06e9\u06f0\u06f8"+
		"\u06fc\u06ff\u0703\u0707\u070b\u0710\u0714\u0718\u071c\u0725\u0729\u072f"+
		"\u0733\u0737\u073c\u073f\u0746\u074a\u074e\u0752\u0756\u075b\u075f\u0763"+
		"\u0777\u077b\u077f\u0784\u078d\u0795\u079d\u07a1\u07a7\u07ab";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}