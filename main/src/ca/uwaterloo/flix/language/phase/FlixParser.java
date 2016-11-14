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
		RULE_annotation = 36, RULE_annotations = 37, RULE_s_import = 38, RULE_import_wildcard = 39, 
		RULE_import_definition = 40, RULE_import_namespace = 41, RULE_decl = 42, 
		RULE_decls_namespace = 43, RULE_decls_enum = 44, RULE_dcases = 45, RULE_dcase = 46, 
		RULE_decls_relation = 47, RULE_decls_lattice = 48, RULE_decls_index = 49, 
		RULE_decls_signature = 50, RULE_decls_external = 51, RULE_decls_definition = 52, 
		RULE_decls_law = 53, RULE_decls_class = 54, RULE_class_body = 55, RULE_class_decl = 56, 
		RULE_decls_fact = 57, RULE_decls_rule = 58, RULE_elms = 59, RULE_decls_letlattice = 60, 
		RULE_decls_impl = 61, RULE_decls_impl_body = 62, RULE_expression = 63, 
		RULE_logical = 64, RULE_expressions = 65, RULE_comparison = 66, RULE_additive = 67, 
		RULE_multiplicative = 68, RULE_infix = 69, RULE_extended = 70, RULE_unary = 71, 
		RULE_ascribe = 72, RULE_e_primary = 73, RULE_e_letMatch = 74, RULE_e_ifThenElse = 75, 
		RULE_e_match = 76, RULE_e_switch = 77, RULE_e_apply = 78, RULE_e_literal = 79, 
		RULE_e_sname = 80, RULE_e_qname = 81, RULE_e_tag = 82, RULE_e_tuple = 83, 
		RULE_e_keyValue = 84, RULE_e_keyValues = 85, RULE_e_userError = 86, RULE_e_wild = 87, 
		RULE_e_fNil = 88, RULE_e_fList = 89, RULE_e_fVec = 90, RULE_e_fSet = 91, 
		RULE_e_fMap = 92, RULE_e_unaryLambda = 93, RULE_e_lambda = 94, RULE_existential = 95, 
		RULE_universal = 96, RULE_pattern = 97, RULE_patterns = 98, RULE_simple = 99, 
		RULE_p_keyValue = 100, RULE_p_keyValues = 101, RULE_p_literal = 102, RULE_p_tag = 103, 
		RULE_p_tuple = 104, RULE_p_wild = 105, RULE_p_fNil = 106, RULE_p_variable = 107, 
		RULE_p_fList = 108, RULE_p_fVec = 109, RULE_p_fSet = 110, RULE_p_fMap = 111, 
		RULE_bools = 112, RULE_chars = 113, RULE_strs = 114, RULE_negative = 115, 
		RULE_float32 = 116, RULE_float64 = 117, RULE_floatDefault = 118, RULE_floats = 119, 
		RULE_int8 = 120, RULE_int16 = 121, RULE_int32 = 122, RULE_int64 = 123, 
		RULE_bigInt = 124, RULE_intDefault = 125, RULE_ints = 126, RULE_literal = 127, 
		RULE_primary = 128, RULE_var = 129, RULE_ref = 130, RULE_type = 131, RULE_arrow = 132, 
		RULE_tuple_unit = 133, RULE_tuple_singleton = 134, RULE_tuple_multi = 135, 
		RULE_tuple = 136, RULE_apply = 137, RULE_unary_ops = 138, RULE_logical_ops = 139, 
		RULE_comparison_ops = 140, RULE_multipve_ops = 141, RULE_addve_ops = 142, 
		RULE_extbin_ops = 143, RULE_predicate = 144, RULE_predicates = 145, RULE_pred_true = 146, 
		RULE_pred_false = 147, RULE_pred_filter = 148, RULE_pred_table = 149, 
		RULE_pred_notequal = 150, RULE_pred_loop = 151;
	public static final String[] ruleNames = {
		"tscomment", "start", "optSC", "ident", "nname", "lowerqname", "upperqname", 
		"annotationName", "attributeName", "className", "definitionName", "qualifiedDefinitionName", 
		"tableName", "qualifiedTableName", "tagName", "typeName", "qualifiedTypeName", 
		"variableName", "variableNames", "argument", "arguments", "formalparams", 
		"attribute", "attributes", "index", "indexes", "match_rule", "match_rules", 
		"switch_rule", "switch_rules", "typeparam", "typeparams", "class_typeparams", 
		"contextBound", "contextBounds", "contextBoundsList", "annotation", "annotations", 
		"s_import", "import_wildcard", "import_definition", "import_namespace", 
		"decl", "decls_namespace", "decls_enum", "dcases", "dcase", "decls_relation", 
		"decls_lattice", "decls_index", "decls_signature", "decls_external", "decls_definition", 
		"decls_law", "decls_class", "class_body", "class_decl", "decls_fact", 
		"decls_rule", "elms", "decls_letlattice", "decls_impl", "decls_impl_body", 
		"expression", "logical", "expressions", "comparison", "additive", "multiplicative", 
		"infix", "extended", "unary", "ascribe", "e_primary", "e_letMatch", "e_ifThenElse", 
		"e_match", "e_switch", "e_apply", "e_literal", "e_sname", "e_qname", "e_tag", 
		"e_tuple", "e_keyValue", "e_keyValues", "e_userError", "e_wild", "e_fNil", 
		"e_fList", "e_fVec", "e_fSet", "e_fMap", "e_unaryLambda", "e_lambda", 
		"existential", "universal", "pattern", "patterns", "simple", "p_keyValue", 
		"p_keyValues", "p_literal", "p_tag", "p_tuple", "p_wild", "p_fNil", "p_variable", 
		"p_fList", "p_fVec", "p_fSet", "p_fMap", "bools", "chars", "strs", "negative", 
		"float32", "float64", "floatDefault", "floats", "int8", "int16", "int32", 
		"int64", "bigInt", "intDefault", "ints", "literal", "primary", "var", 
		"ref", "type", "arrow", "tuple_unit", "tuple_singleton", "tuple_multi", 
		"tuple", "apply", "unary_ops", "logical_ops", "comparison_ops", "multipve_ops", 
		"addve_ops", "extbin_ops", "predicate", "predicates", "pred_true", "pred_false", 
		"pred_filter", "pred_table", "pred_notequal", "pred_loop"
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
		"'law'", "'class'", "'let'", "'imple'", "'Nil'", "'switch'", "'match'", 
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
			setState(304);
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
			setState(309);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,0,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(306);
					s_import();
					}
					} 
				}
				setState(311);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,0,_ctx);
			}
			setState(315);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,1,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(312);
					decl();
					}
					} 
				}
				setState(317);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,1,_ctx);
			}
			setState(319);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(318);
				match(WS);
				}
			}

			setState(321);
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
			setState(327);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,4,_ctx) ) {
			case 1:
				{
				setState(324);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(323);
					match(WS);
					}
				}

				setState(326);
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
			setState(329);
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
			setState(331);
			ident();
			setState(336);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__0) {
				{
				{
				setState(332);
				match(T__0);
				setState(333);
				ident();
				}
				}
				setState(338);
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
			setState(342);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,6,_ctx) ) {
			case 1:
				{
				setState(339);
				nname();
				setState(340);
				match(T__1);
				}
				break;
			}
			setState(344);
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
			setState(349);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,7,_ctx) ) {
			case 1:
				{
				setState(346);
				nname();
				setState(347);
				match(T__1);
				}
				break;
			}
			setState(351);
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
			setState(357);
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
			setState(359);
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
			setState(361);
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
			setState(363);
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
			setState(365);
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
		enterRule(_localctx, 28, RULE_tagName);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(367);
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
		enterRule(_localctx, 30, RULE_typeName);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(369);
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
			setState(371);
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
			setState(373);
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
			enterOuterAlt(_localctx, 1);
			{
			setState(375);
			variableName();
			{
			setState(377);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(376);
				match(WS);
				}
			}

			setState(379);
			match(T__2);
			setState(381);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(380);
				match(WS);
				}
			}

			setState(383);
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
			setState(385);
			variableName();
			setState(386);
			match(T__3);
			setState(388);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(387);
				match(WS);
				}
			}

			setState(390);
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
			setState(392);
			argument();
			setState(403);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,13,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(394);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(393);
						match(WS);
						}
					}

					setState(396);
					match(T__2);
					setState(398);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(397);
						match(WS);
						}
					}

					setState(400);
					argument();
					}
					} 
				}
				setState(405);
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
		enterRule(_localctx, 42, RULE_formalparams);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(417);
			_la = _input.LA(1);
			if (_la==T__4) {
				{
				setState(406);
				match(T__4);
				setState(408);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,14,_ctx) ) {
				case 1:
					{
					setState(407);
					match(WS);
					}
					break;
				}
				setState(411);
				_la = _input.LA(1);
				if (_la==LowerIdent) {
					{
					setState(410);
					arguments();
					}
				}

				setState(414);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(413);
					match(WS);
					}
				}

				setState(416);
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
			setState(419);
			attributeName();
			setState(421);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(420);
				match(WS);
				}
			}

			setState(423);
			match(T__3);
			setState(425);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(424);
				match(WS);
				}
			}

			setState(427);
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
			setState(429);
			attribute();
			setState(440);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,22,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(431);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(430);
						match(WS);
						}
					}

					setState(433);
					match(T__2);
					setState(435);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(434);
						match(WS);
						}
					}

					setState(437);
					attribute();
					}
					} 
				}
				setState(442);
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
		enterRule(_localctx, 48, RULE_index);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(443);
			match(T__6);
			setState(445);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,23,_ctx) ) {
			case 1:
				{
				setState(444);
				match(WS);
				}
				break;
			}
			setState(461);
			_la = _input.LA(1);
			if (_la==LowerIdent) {
				{
				setState(447);
				attributeName();
				setState(458);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,26,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(449);
						_la = _input.LA(1);
						if (_la==WS) {
							{
							setState(448);
							match(WS);
							}
						}

						setState(451);
						match(T__2);
						setState(453);
						_la = _input.LA(1);
						if (_la==WS) {
							{
							setState(452);
							match(WS);
							}
						}

						setState(455);
						attributeName();
						}
						} 
					}
					setState(460);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,26,_ctx);
				}
				}
			}

			setState(464);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(463);
				match(WS);
				}
			}

			setState(466);
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
			setState(468);
			index();
			setState(479);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,31,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(470);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(469);
						match(WS);
						}
					}

					setState(472);
					match(T__2);
					setState(474);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(473);
						match(WS);
						}
					}

					setState(476);
					index();
					}
					} 
				}
				setState(481);
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
			setState(482);
			match(CASE);
			setState(483);
			match(WS);
			setState(484);
			pattern();
			setState(486);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(485);
				match(WS);
				}
			}

			setState(488);
			match(T__8);
			setState(490);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(489);
				match(WS);
				}
			}

			setState(492);
			expression();
			setState(494);
			_la = _input.LA(1);
			if (_la==SC) {
				{
				setState(493);
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
			setState(496);
			match_rule();
			setState(503);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,36,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(498);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(497);
						match(WS);
						}
					}

					setState(500);
					match_rule();
					}
					} 
				}
				setState(505);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,36,_ctx);
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
			setState(506);
			match(CASE);
			setState(507);
			match(WS);
			setState(508);
			expression();
			setState(510);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(509);
				match(WS);
				}
			}

			setState(512);
			match(T__8);
			setState(514);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(513);
				match(WS);
				}
			}

			setState(516);
			expression();
			setState(518);
			_la = _input.LA(1);
			if (_la==SC) {
				{
				setState(517);
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
			setState(520);
			switch_rule();
			setState(527);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,41,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(522);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(521);
						match(WS);
						}
					}

					setState(524);
					switch_rule();
					}
					} 
				}
				setState(529);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,41,_ctx);
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
			setState(530);
			variableName();
			setState(539);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,44,_ctx) ) {
			case 1:
				{
				setState(532);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(531);
					match(WS);
					}
				}

				setState(534);
				match(T__3);
				setState(536);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(535);
					match(WS);
					}
				}

				setState(538);
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
			setState(561);
			_la = _input.LA(1);
			if (_la==T__9) {
				{
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
				typeparam();
				setState(556);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==T__2 || _la==WS) {
					{
					{
					setState(547);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(546);
						match(WS);
						}
					}

					setState(549);
					match(T__2);
					setState(551);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(550);
						match(WS);
						}
					}

					setState(553);
					typeparam();
					}
					}
					setState(558);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(559);
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
			setState(563);
			match(T__9);
			setState(564);
			type();
			setState(575);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__2 || _la==WS) {
				{
				{
				setState(566);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(565);
					match(WS);
					}
				}

				setState(568);
				match(T__2);
				setState(570);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(569);
					match(WS);
					}
				}

				setState(572);
				type();
				}
				}
				setState(577);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(578);
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
			setState(580);
			className();
			setState(581);
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
			setState(583);
			contextBound();
			setState(594);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,55,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(585);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(584);
						match(WS);
						}
					}

					setState(587);
					match(T__2);
					setState(589);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(588);
						match(WS);
						}
					}

					setState(591);
					contextBound();
					}
					} 
				}
				setState(596);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,55,_ctx);
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
			setState(608);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,59,_ctx) ) {
			case 1:
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
				match(T__11);
				setState(602);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(601);
					match(WS);
					}
				}

				setState(604);
				contextBounds();
				setState(606);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,58,_ctx) ) {
				case 1:
					{
					setState(605);
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
		enterRule(_localctx, 72, RULE_annotation);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(610);
			match(T__12);
			setState(611);
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
		enterRule(_localctx, 74, RULE_annotations);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(613);
			annotation();
			setState(618);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,60,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(614);
					match(WS);
					setState(615);
					annotation();
					}
					} 
				}
				setState(620);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,60,_ctx);
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
		enterRule(_localctx, 76, RULE_s_import);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(622);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(621);
				match(WS);
				}
			}

			setState(627);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,62,_ctx) ) {
			case 1:
				{
				setState(624);
				import_wildcard();
				}
				break;
			case 2:
				{
				setState(625);
				import_definition();
				}
				break;
			case 3:
				{
				setState(626);
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
		enterRule(_localctx, 78, RULE_import_wildcard);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(629);
			match(IMPORT);
			setState(630);
			match(WS);
			setState(631);
			nname();
			setState(632);
			match(T__1);
			setState(633);
			match(WILD);
			setState(634);
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
		enterRule(_localctx, 80, RULE_import_definition);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(636);
			match(IMPORT);
			setState(637);
			match(WS);
			setState(638);
			nname();
			setState(639);
			match(T__1);
			setState(640);
			ident();
			setState(641);
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
		enterRule(_localctx, 82, RULE_import_namespace);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(643);
			match(IMPORT);
			setState(644);
			match(WS);
			setState(645);
			nname();
			setState(646);
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
		enterRule(_localctx, 84, RULE_decl);
		try {
			setState(661);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,63,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(648);
				decls_namespace();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(649);
				decls_enum();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(650);
				decls_relation();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(651);
				decls_lattice();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(652);
				decls_index();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(653);
				decls_signature();
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(654);
				decls_external();
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(655);
				decls_definition();
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(656);
				decls_law();
				}
				break;
			case 10:
				enterOuterAlt(_localctx, 10);
				{
				setState(657);
				decls_class();
				}
				break;
			case 11:
				enterOuterAlt(_localctx, 11);
				{
				setState(658);
				decls_fact();
				}
				break;
			case 12:
				enterOuterAlt(_localctx, 12);
				{
				setState(659);
				decls_rule();
				}
				break;
			case 13:
				enterOuterAlt(_localctx, 13);
				{
				setState(660);
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
		enterRule(_localctx, 86, RULE_decls_namespace);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(664);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(663);
				match(WS);
				}
			}

			setState(666);
			match(NAMESPACE);
			setState(667);
			match(WS);
			setState(668);
			nname();
			setState(670);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(669);
				match(WS);
				}
			}

			setState(672);
			match(T__6);
			setState(674);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,66,_ctx) ) {
			case 1:
				{
				setState(673);
				match(WS);
				}
				break;
			}
			setState(679);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,67,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(676);
					decl();
					}
					} 
				}
				setState(681);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,67,_ctx);
			}
			setState(683);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(682);
				match(WS);
				}
			}

			setState(685);
			match(T__7);
			setState(686);
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
		enterRule(_localctx, 88, RULE_decls_enum);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(694);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,70,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(689);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(688);
						match(WS);
						}
					}

					setState(691);
					tscomment();
					}
					} 
				}
				setState(696);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,70,_ctx);
			}
			setState(698);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(697);
				match(WS);
				}
			}

			setState(700);
			match(ENUM);
			setState(701);
			match(WS);
			setState(702);
			typeName();
			setState(703);
			typeparams();
			setState(705);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(704);
				match(WS);
				}
			}

			setState(707);
			match(T__6);
			setState(709);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(708);
				match(WS);
				}
			}

			setState(711);
			dcases();
			setState(713);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(712);
				match(WS);
				}
			}

			setState(715);
			match(T__7);
			setState(716);
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
		enterRule(_localctx, 90, RULE_dcases);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(718);
			dcase();
			setState(729);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,77,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(720);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(719);
						match(WS);
						}
					}

					setState(722);
					match(T__2);
					setState(724);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(723);
						match(WS);
						}
					}

					setState(726);
					dcase();
					}
					} 
				}
				setState(731);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,77,_ctx);
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
		enterRule(_localctx, 92, RULE_dcase);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(732);
			match(CASE);
			setState(733);
			match(WS);
			setState(734);
			tagName();
			setState(736);
			_la = _input.LA(1);
			if (_la==T__4) {
				{
				setState(735);
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
		enterRule(_localctx, 94, RULE_decls_relation);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(744);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,80,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(739);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(738);
						match(WS);
						}
					}

					setState(741);
					tscomment();
					}
					} 
				}
				setState(746);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,80,_ctx);
			}
			setState(748);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(747);
				match(WS);
				}
			}

			setState(750);
			match(REL);
			setState(751);
			match(WS);
			setState(752);
			tableName();
			setState(754);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(753);
				match(WS);
				}
			}

			setState(756);
			match(T__4);
			setState(758);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,83,_ctx) ) {
			case 1:
				{
				setState(757);
				match(WS);
				}
				break;
			}
			setState(761);
			_la = _input.LA(1);
			if (_la==LowerIdent) {
				{
				setState(760);
				attributes();
				}
			}

			setState(764);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(763);
				match(WS);
				}
			}

			setState(766);
			match(T__5);
			setState(767);
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
		enterRule(_localctx, 96, RULE_decls_lattice);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(775);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,87,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(770);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(769);
						match(WS);
						}
					}

					setState(772);
					tscomment();
					}
					} 
				}
				setState(777);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,87,_ctx);
			}
			setState(779);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(778);
				match(WS);
				}
			}

			setState(781);
			match(LAT);
			setState(782);
			match(WS);
			setState(783);
			tableName();
			setState(785);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(784);
				match(WS);
				}
			}

			setState(787);
			match(T__4);
			setState(789);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,90,_ctx) ) {
			case 1:
				{
				setState(788);
				match(WS);
				}
				break;
			}
			setState(792);
			_la = _input.LA(1);
			if (_la==LowerIdent) {
				{
				setState(791);
				attributes();
				}
			}

			setState(795);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(794);
				match(WS);
				}
			}

			setState(797);
			match(T__5);
			setState(798);
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
		enterRule(_localctx, 98, RULE_decls_index);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(801);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(800);
				match(WS);
				}
			}

			setState(803);
			match(INDEX);
			setState(804);
			match(WS);
			setState(805);
			qualifiedTableName();
			setState(807);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(806);
				match(WS);
				}
			}

			setState(809);
			match(T__4);
			setState(811);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,95,_ctx) ) {
			case 1:
				{
				setState(810);
				match(WS);
				}
				break;
			}
			setState(814);
			_la = _input.LA(1);
			if (_la==T__6) {
				{
				setState(813);
				indexes();
				}
			}

			setState(817);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(816);
				match(WS);
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
		enterRule(_localctx, 100, RULE_decls_signature);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(828);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,99,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(823);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(822);
						match(WS);
						}
					}

					setState(825);
					tscomment();
					}
					} 
				}
				setState(830);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,99,_ctx);
			}
			setState(832);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(831);
				match(WS);
				}
			}

			setState(834);
			match(DEF);
			setState(835);
			match(WS);
			setState(836);
			definitionName();
			setState(838);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,101,_ctx) ) {
			case 1:
				{
				setState(837);
				match(WS);
				}
				break;
			}
			setState(840);
			formalparams();
			setState(842);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(841);
				match(WS);
				}
			}

			setState(844);
			match(T__3);
			setState(846);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(845);
				match(WS);
				}
			}

			setState(848);
			type();
			setState(849);
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
		enterRule(_localctx, 102, RULE_decls_external);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(857);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,105,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(852);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(851);
						match(WS);
						}
					}

					setState(854);
					tscomment();
					}
					} 
				}
				setState(859);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,105,_ctx);
			}
			setState(861);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(860);
				match(WS);
				}
			}

			setState(863);
			match(EXTERNAL);
			setState(865);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(864);
				match(WS);
				}
			}

			setState(867);
			match(DEF);
			setState(868);
			match(WS);
			setState(869);
			definitionName();
			setState(871);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,108,_ctx) ) {
			case 1:
				{
				setState(870);
				match(WS);
				}
				break;
			}
			setState(873);
			formalparams();
			setState(875);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(874);
				match(WS);
				}
			}

			setState(877);
			match(T__3);
			setState(879);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(878);
				match(WS);
				}
			}

			setState(881);
			type();
			setState(882);
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
		enterRule(_localctx, 104, RULE_decls_definition);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(890);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,112,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(885);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(884);
						match(WS);
						}
					}

					setState(887);
					tscomment();
					}
					} 
				}
				setState(892);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,112,_ctx);
			}
			setState(894);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,113,_ctx) ) {
			case 1:
				{
				setState(893);
				match(WS);
				}
				break;
			}
			setState(897);
			_la = _input.LA(1);
			if (_la==T__12) {
				{
				setState(896);
				annotations();
				}
			}

			setState(900);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(899);
				match(WS);
				}
			}

			setState(902);
			match(DEF);
			setState(903);
			match(WS);
			setState(904);
			definitionName();
			setState(906);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,116,_ctx) ) {
			case 1:
				{
				setState(905);
				match(WS);
				}
				break;
			}
			setState(908);
			typeparams();
			setState(909);
			formalparams();
			setState(911);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(910);
				match(WS);
				}
			}

			setState(913);
			match(T__3);
			setState(915);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(914);
				match(WS);
				}
			}

			setState(917);
			type();
			setState(919);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(918);
				match(WS);
				}
			}

			setState(921);
			match(T__13);
			setState(923);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(922);
				match(WS);
				}
			}

			setState(925);
			expression();
			setState(926);
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
		enterRule(_localctx, 106, RULE_decls_law);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(934);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,122,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(929);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(928);
						match(WS);
						}
					}

					setState(931);
					tscomment();
					}
					} 
				}
				setState(936);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,122,_ctx);
			}
			setState(938);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(937);
				match(WS);
				}
			}

			setState(940);
			match(LAW);
			setState(941);
			match(WS);
			setState(942);
			definitionName();
			setState(944);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,124,_ctx) ) {
			case 1:
				{
				setState(943);
				match(WS);
				}
				break;
			}
			setState(946);
			typeparams();
			setState(948);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,125,_ctx) ) {
			case 1:
				{
				setState(947);
				match(WS);
				}
				break;
			}
			setState(950);
			formalparams();
			setState(952);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(951);
				match(WS);
				}
			}

			setState(954);
			match(T__3);
			setState(956);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(955);
				match(WS);
				}
			}

			setState(958);
			type();
			setState(960);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(959);
				match(WS);
				}
			}

			setState(962);
			match(T__13);
			setState(964);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(963);
				match(WS);
				}
			}

			setState(966);
			expression();
			setState(967);
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
		enterRule(_localctx, 108, RULE_decls_class);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(975);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,131,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(970);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(969);
						match(WS);
						}
					}

					setState(972);
					tscomment();
					}
					} 
				}
				setState(977);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,131,_ctx);
			}
			setState(979);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(978);
				match(WS);
				}
			}

			setState(981);
			match(CLASS);
			setState(982);
			match(WS);
			setState(983);
			className();
			setState(984);
			class_typeparams();
			setState(986);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,133,_ctx) ) {
			case 1:
				{
				setState(985);
				match(WS);
				}
				break;
			}
			setState(988);
			contextBoundsList();
			setState(990);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(989);
				match(WS);
				}
			}

			setState(992);
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
		enterRule(_localctx, 110, RULE_class_body);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(994);
			match(T__6);
			setState(996);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,135,_ctx) ) {
			case 1:
				{
				setState(995);
				match(WS);
				}
				break;
			}
			setState(1004);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__12 || ((((_la - 71)) & ~0x3f) == 0 && ((1L << (_la - 71)) & ((1L << (TripleSlashComment - 71)) | (1L << (WS - 71)) | (1L << (DEF - 71)) | (1L << (LAW - 71)))) != 0)) {
				{
				{
				setState(998);
				class_decl();
				setState(1000);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,136,_ctx) ) {
				case 1:
					{
					setState(999);
					match(WS);
					}
					break;
				}
				}
				}
				setState(1006);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(1007);
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
		enterRule(_localctx, 112, RULE_class_decl);
		try {
			setState(1012);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,138,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1009);
				decls_definition();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1010);
				decls_signature();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1011);
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
		enterRule(_localctx, 114, RULE_decls_fact);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1015);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1014);
				match(WS);
				}
			}

			setState(1017);
			predicate();
			setState(1019);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1018);
				match(WS);
				}
			}

			setState(1021);
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
		enterRule(_localctx, 116, RULE_decls_rule);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1024);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1023);
				match(WS);
				}
			}

			setState(1026);
			predicate();
			setState(1028);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1027);
				match(WS);
				}
			}

			setState(1030);
			match(T__14);
			setState(1032);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1031);
				match(WS);
				}
			}

			setState(1034);
			predicates();
			setState(1036);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1035);
				match(WS);
				}
			}

			setState(1038);
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
		enterRule(_localctx, 118, RULE_elms);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1040);
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
		enterRule(_localctx, 120, RULE_decls_letlattice);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1043);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1042);
				match(WS);
				}
			}

			setState(1045);
			match(LET);
			setState(1047);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1046);
				match(WS);
				}
			}

			setState(1049);
			type();
			setState(1050);
			match(T__15);
			setState(1052);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1051);
				match(WS);
				}
			}

			setState(1054);
			match(T__13);
			setState(1056);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1055);
				match(WS);
				}
			}

			setState(1058);
			match(T__4);
			setState(1060);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1059);
				match(WS);
				}
			}

			setState(1062);
			elms();
			setState(1064);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1063);
				match(WS);
				}
			}

			setState(1066);
			match(T__5);
			setState(1067);
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
		enterRule(_localctx, 122, RULE_decls_impl);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1075);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,152,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1070);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1069);
						match(WS);
						}
					}

					setState(1072);
					tscomment();
					}
					} 
				}
				setState(1077);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,152,_ctx);
			}
			setState(1079);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1078);
				match(WS);
				}
			}

			setState(1081);
			match(IMPL);
			setState(1082);
			match(WS);
			setState(1083);
			className();
			setState(1084);
			class_typeparams();
			setState(1086);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,154,_ctx) ) {
			case 1:
				{
				setState(1085);
				match(WS);
				}
				break;
			}
			setState(1088);
			contextBoundsList();
			setState(1090);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1089);
				match(WS);
				}
			}

			setState(1092);
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
		enterRule(_localctx, 124, RULE_decls_impl_body);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1094);
			match(T__6);
			setState(1096);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,156,_ctx) ) {
			case 1:
				{
				setState(1095);
				match(WS);
				}
				break;
			}
			setState(1101);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,157,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1098);
					decls_definition();
					}
					} 
				}
				setState(1103);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,157,_ctx);
			}
			setState(1105);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1104);
				match(WS);
				}
			}

			setState(1107);
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
		enterRule(_localctx, 126, RULE_expression);
		int _la;
		try {
			setState(1122);
			switch (_input.LA(1)) {
			case T__6:
				enterOuterAlt(_localctx, 1);
				{
				{
				setState(1109);
				match(T__6);
				setState(1111);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1110);
					match(WS);
					}
				}

				setState(1113);
				expression();
				setState(1115);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1114);
					match(WS);
					}
				}

				setState(1117);
				match(T__7);
				setState(1119);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,161,_ctx) ) {
				case 1:
					{
					setState(1118);
					match(WS);
					}
					break;
				}
				}
				}
				break;
			case T__4:
			case T__18:
			case T__20:
			case T__21:
			case T__22:
			case T__23:
			case T__24:
			case T__25:
			case T__26:
			case T__28:
			case T__29:
			case T__30:
			case T__38:
			case T__39:
			case T__40:
			case T__41:
			case LET:
			case FNIL:
			case SWITCH:
			case MATCH:
			case WILD:
			case IF:
			case LowerIdent:
			case UpperIdent:
			case Chars:
			case Strs:
			case Digits:
				enterOuterAlt(_localctx, 2);
				{
				setState(1121);
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
		enterRule(_localctx, 128, RULE_logical);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1124);
			comparison();
			setState(1134);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,165,_ctx) ) {
			case 1:
				{
				setState(1126);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1125);
					match(WS);
					}
				}

				setState(1128);
				logical_ops();
				setState(1130);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1129);
					match(WS);
					}
				}

				setState(1132);
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
		enterRule(_localctx, 130, RULE_expressions);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1136);
			expression();
			setState(1147);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,168,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1138);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1137);
						match(WS);
						}
					}

					setState(1140);
					match(T__2);
					setState(1142);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1141);
						match(WS);
						}
					}

					setState(1144);
					expression();
					}
					} 
				}
				setState(1149);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,168,_ctx);
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
		enterRule(_localctx, 132, RULE_comparison);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1150);
			additive();
			setState(1160);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,171,_ctx) ) {
			case 1:
				{
				setState(1152);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1151);
					match(WS);
					}
				}

				setState(1154);
				comparison_ops();
				setState(1156);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1155);
					match(WS);
					}
				}

				setState(1158);
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
		enterRule(_localctx, 134, RULE_additive);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1162);
			multiplicative();
			setState(1172);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,174,_ctx) ) {
			case 1:
				{
				setState(1164);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1163);
					match(WS);
					}
				}

				setState(1166);
				addve_ops();
				setState(1168);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1167);
					match(WS);
					}
				}

				setState(1170);
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
		enterRule(_localctx, 136, RULE_multiplicative);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1174);
			infix();
			setState(1184);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,177,_ctx) ) {
			case 1:
				{
				setState(1176);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1175);
					match(WS);
					}
				}

				setState(1178);
				multipve_ops();
				setState(1180);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1179);
					match(WS);
					}
				}

				setState(1182);
				multiplicative();
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
		enterRule(_localctx, 138, RULE_infix);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1186);
			extended();
			setState(1198);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,180,_ctx) ) {
			case 1:
				{
				setState(1188);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1187);
					match(WS);
					}
				}

				setState(1190);
				match(T__16);
				setState(1191);
				qualifiedDefinitionName();
				setState(1192);
				match(T__16);
				setState(1194);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1193);
					match(WS);
					}
				}

				setState(1196);
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
		enterRule(_localctx, 140, RULE_extended);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1200);
			unary();
			setState(1210);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,183,_ctx) ) {
			case 1:
				{
				setState(1202);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1201);
					match(WS);
					}
				}

				setState(1204);
				extbin_ops();
				setState(1206);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1205);
					match(WS);
					}
				}

				setState(1208);
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
		enterRule(_localctx, 142, RULE_unary);
		int _la;
		try {
			setState(1219);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,185,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				{
				setState(1212);
				unary_ops();
				setState(1214);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1213);
					match(WS);
					}
				}

				setState(1216);
				unary();
				}
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1218);
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
		enterRule(_localctx, 144, RULE_ascribe);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1221);
			e_fList();
			setState(1230);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,188,_ctx) ) {
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
				match(T__3);
				setState(1227);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1226);
					match(WS);
					}
				}

				setState(1229);
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
		enterRule(_localctx, 146, RULE_e_primary);
		try {
			setState(1251);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,189,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1232);
				e_letMatch();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1233);
				e_ifThenElse();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1234);
				e_match();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(1235);
				e_switch();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(1236);
				e_tag();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(1237);
				e_lambda();
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(1238);
				e_tuple();
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(1239);
				e_fNil();
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(1240);
				e_fVec();
				}
				break;
			case 10:
				enterOuterAlt(_localctx, 10);
				{
				setState(1241);
				e_fSet();
				}
				break;
			case 11:
				enterOuterAlt(_localctx, 11);
				{
				setState(1242);
				e_fMap();
				}
				break;
			case 12:
				enterOuterAlt(_localctx, 12);
				{
				setState(1243);
				e_literal();
				}
				break;
			case 13:
				enterOuterAlt(_localctx, 13);
				{
				setState(1244);
				existential();
				}
				break;
			case 14:
				enterOuterAlt(_localctx, 14);
				{
				setState(1245);
				universal();
				}
				break;
			case 15:
				enterOuterAlt(_localctx, 15);
				{
				setState(1246);
				e_qname();
				}
				break;
			case 16:
				enterOuterAlt(_localctx, 16);
				{
				setState(1247);
				e_unaryLambda();
				}
				break;
			case 17:
				enterOuterAlt(_localctx, 17);
				{
				setState(1248);
				e_wild();
				}
				break;
			case 18:
				enterOuterAlt(_localctx, 18);
				{
				setState(1249);
				e_sname();
				}
				break;
			case 19:
				enterOuterAlt(_localctx, 19);
				{
				setState(1250);
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
		enterRule(_localctx, 148, RULE_e_letMatch);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1253);
			match(LET);
			setState(1254);
			match(WS);
			setState(1255);
			pattern();
			setState(1257);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1256);
				match(WS);
				}
			}

			setState(1259);
			match(T__13);
			setState(1261);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1260);
				match(WS);
				}
			}

			setState(1263);
			expression();
			setState(1265);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1264);
				match(WS);
				}
			}

			setState(1267);
			match(SC);
			setState(1269);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1268);
				match(WS);
				}
			}

			setState(1271);
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
		enterRule(_localctx, 150, RULE_e_ifThenElse);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1273);
			match(IF);
			setState(1275);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1274);
				match(WS);
				}
			}

			setState(1277);
			match(T__4);
			setState(1279);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1278);
				match(WS);
				}
			}

			setState(1281);
			expression();
			setState(1283);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1282);
				match(WS);
				}
			}

			setState(1285);
			match(T__5);
			setState(1287);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1286);
				match(WS);
				}
			}

			setState(1289);
			expression();
			setState(1290);
			match(WS);
			setState(1291);
			match(ELSE);
			setState(1292);
			match(WS);
			setState(1293);
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
		enterRule(_localctx, 152, RULE_e_match);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1295);
			match(MATCH);
			setState(1296);
			match(WS);
			setState(1297);
			expression();
			setState(1298);
			match(WS);
			setState(1299);
			match(WITH);
			setState(1300);
			match(WS);
			setState(1301);
			match(T__6);
			setState(1303);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1302);
				match(WS);
				}
			}

			setState(1305);
			match_rules();
			setState(1307);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1306);
				match(WS);
				}
			}

			setState(1309);
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
		enterRule(_localctx, 154, RULE_e_switch);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1311);
			match(SWITCH);
			setState(1312);
			match(WS);
			setState(1313);
			match(T__6);
			setState(1315);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1314);
				match(WS);
				}
			}

			setState(1317);
			switch_rules();
			setState(1319);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1318);
				match(WS);
				}
			}

			setState(1321);
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
		enterRule(_localctx, 156, RULE_e_apply);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1323);
			e_primary();
			setState(1338);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,206,_ctx) ) {
			case 1:
				{
				setState(1325);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1324);
					match(WS);
					}
				}

				setState(1327);
				match(T__4);
				setState(1329);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,203,_ctx) ) {
				case 1:
					{
					setState(1328);
					match(WS);
					}
					break;
				}
				setState(1332);
				_la = _input.LA(1);
				if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__4) | (1L << T__6) | (1L << T__18) | (1L << T__20) | (1L << T__21) | (1L << T__22) | (1L << T__23) | (1L << T__24) | (1L << T__25) | (1L << T__26) | (1L << T__28) | (1L << T__29) | (1L << T__30) | (1L << T__38) | (1L << T__39) | (1L << T__40) | (1L << T__41))) != 0) || ((((_la - 84)) & ~0x3f) == 0 && ((1L << (_la - 84)) & ((1L << (LET - 84)) | (1L << (FNIL - 84)) | (1L << (SWITCH - 84)) | (1L << (MATCH - 84)) | (1L << (WILD - 84)) | (1L << (IF - 84)) | (1L << (LowerIdent - 84)) | (1L << (UpperIdent - 84)) | (1L << (Chars - 84)) | (1L << (Strs - 84)) | (1L << (Digits - 84)))) != 0)) {
					{
					setState(1331);
					expressions();
					}
				}

				setState(1335);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1334);
					match(WS);
					}
				}

				setState(1337);
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
		enterRule(_localctx, 158, RULE_e_literal);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1340);
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
		enterRule(_localctx, 160, RULE_e_sname);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1342);
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
		enterRule(_localctx, 162, RULE_e_qname);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1344);
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
		enterRule(_localctx, 164, RULE_e_tag);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1349);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,207,_ctx) ) {
			case 1:
				{
				setState(1346);
				qualifiedTypeName();
				setState(1347);
				match(T__0);
				}
				break;
			}
			setState(1351);
			tagName();
			setState(1356);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,209,_ctx) ) {
			case 1:
				{
				setState(1353);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1352);
					match(WS);
					}
				}

				setState(1355);
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
		enterRule(_localctx, 166, RULE_e_tuple);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1358);
			match(T__4);
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
			setState(1363);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__4) | (1L << T__6) | (1L << T__18) | (1L << T__20) | (1L << T__21) | (1L << T__22) | (1L << T__23) | (1L << T__24) | (1L << T__25) | (1L << T__26) | (1L << T__28) | (1L << T__29) | (1L << T__30) | (1L << T__38) | (1L << T__39) | (1L << T__40) | (1L << T__41))) != 0) || ((((_la - 84)) & ~0x3f) == 0 && ((1L << (_la - 84)) & ((1L << (LET - 84)) | (1L << (FNIL - 84)) | (1L << (SWITCH - 84)) | (1L << (MATCH - 84)) | (1L << (WILD - 84)) | (1L << (IF - 84)) | (1L << (LowerIdent - 84)) | (1L << (UpperIdent - 84)) | (1L << (Chars - 84)) | (1L << (Strs - 84)) | (1L << (Digits - 84)))) != 0)) {
				{
				setState(1362);
				expressions();
				}
			}

			setState(1366);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1365);
				match(WS);
				}
			}

			setState(1368);
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
		enterRule(_localctx, 168, RULE_e_keyValue);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1370);
			expression();
			setState(1372);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1371);
				match(WS);
				}
			}

			setState(1374);
			match(T__17);
			setState(1376);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1375);
				match(WS);
				}
			}

			setState(1378);
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
		enterRule(_localctx, 170, RULE_e_keyValues);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1380);
			e_keyValue();
			setState(1391);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,217,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1382);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1381);
						match(WS);
						}
					}

					setState(1384);
					match(T__2);
					setState(1386);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1385);
						match(WS);
						}
					}

					setState(1388);
					e_keyValue();
					}
					} 
				}
				setState(1393);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,217,_ctx);
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
		enterRule(_localctx, 172, RULE_e_userError);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1394);
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
		enterRule(_localctx, 174, RULE_e_wild);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1396);
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
		enterRule(_localctx, 176, RULE_e_fNil);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1398);
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
		enterRule(_localctx, 178, RULE_e_fList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1400);
			e_apply();
			setState(1409);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,220,_ctx) ) {
			case 1:
				{
				setState(1402);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1401);
					match(WS);
					}
				}

				setState(1404);
				match(T__19);
				setState(1406);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1405);
					match(WS);
					}
				}

				setState(1408);
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
		enterRule(_localctx, 180, RULE_e_fVec);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1411);
			match(T__20);
			setState(1413);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,221,_ctx) ) {
			case 1:
				{
				setState(1412);
				match(WS);
				}
				break;
			}
			setState(1416);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__4) | (1L << T__6) | (1L << T__18) | (1L << T__20) | (1L << T__21) | (1L << T__22) | (1L << T__23) | (1L << T__24) | (1L << T__25) | (1L << T__26) | (1L << T__28) | (1L << T__29) | (1L << T__30) | (1L << T__38) | (1L << T__39) | (1L << T__40) | (1L << T__41))) != 0) || ((((_la - 84)) & ~0x3f) == 0 && ((1L << (_la - 84)) & ((1L << (LET - 84)) | (1L << (FNIL - 84)) | (1L << (SWITCH - 84)) | (1L << (MATCH - 84)) | (1L << (WILD - 84)) | (1L << (IF - 84)) | (1L << (LowerIdent - 84)) | (1L << (UpperIdent - 84)) | (1L << (Chars - 84)) | (1L << (Strs - 84)) | (1L << (Digits - 84)))) != 0)) {
				{
				setState(1415);
				expressions();
				}
			}

			setState(1419);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1418);
				match(WS);
				}
			}

			setState(1421);
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
		enterRule(_localctx, 182, RULE_e_fSet);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1423);
			match(T__21);
			setState(1425);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,224,_ctx) ) {
			case 1:
				{
				setState(1424);
				match(WS);
				}
				break;
			}
			setState(1428);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__4) | (1L << T__6) | (1L << T__18) | (1L << T__20) | (1L << T__21) | (1L << T__22) | (1L << T__23) | (1L << T__24) | (1L << T__25) | (1L << T__26) | (1L << T__28) | (1L << T__29) | (1L << T__30) | (1L << T__38) | (1L << T__39) | (1L << T__40) | (1L << T__41))) != 0) || ((((_la - 84)) & ~0x3f) == 0 && ((1L << (_la - 84)) & ((1L << (LET - 84)) | (1L << (FNIL - 84)) | (1L << (SWITCH - 84)) | (1L << (MATCH - 84)) | (1L << (WILD - 84)) | (1L << (IF - 84)) | (1L << (LowerIdent - 84)) | (1L << (UpperIdent - 84)) | (1L << (Chars - 84)) | (1L << (Strs - 84)) | (1L << (Digits - 84)))) != 0)) {
				{
				setState(1427);
				expressions();
				}
			}

			setState(1431);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1430);
				match(WS);
				}
			}

			setState(1433);
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
		enterRule(_localctx, 184, RULE_e_fMap);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1435);
			match(T__22);
			setState(1437);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,227,_ctx) ) {
			case 1:
				{
				setState(1436);
				match(WS);
				}
				break;
			}
			setState(1440);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__4) | (1L << T__6) | (1L << T__18) | (1L << T__20) | (1L << T__21) | (1L << T__22) | (1L << T__23) | (1L << T__24) | (1L << T__25) | (1L << T__26) | (1L << T__28) | (1L << T__29) | (1L << T__30) | (1L << T__38) | (1L << T__39) | (1L << T__40) | (1L << T__41))) != 0) || ((((_la - 84)) & ~0x3f) == 0 && ((1L << (_la - 84)) & ((1L << (LET - 84)) | (1L << (FNIL - 84)) | (1L << (SWITCH - 84)) | (1L << (MATCH - 84)) | (1L << (WILD - 84)) | (1L << (IF - 84)) | (1L << (LowerIdent - 84)) | (1L << (UpperIdent - 84)) | (1L << (Chars - 84)) | (1L << (Strs - 84)) | (1L << (Digits - 84)))) != 0)) {
				{
				setState(1439);
				e_keyValues();
				}
			}

			setState(1443);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1442);
				match(WS);
				}
			}

			setState(1445);
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
		enterRule(_localctx, 186, RULE_e_unaryLambda);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1447);
			variableName();
			setState(1449);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1448);
				match(WS);
				}
			}

			setState(1451);
			match(T__17);
			setState(1453);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1452);
				match(WS);
				}
			}

			setState(1455);
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
		enterRule(_localctx, 188, RULE_e_lambda);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1457);
			match(T__4);
			setState(1459);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1458);
				match(WS);
				}
			}

			setState(1461);
			variableNames();
			setState(1463);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1462);
				match(WS);
				}
			}

			setState(1465);
			match(T__5);
			setState(1467);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1466);
				match(WS);
				}
			}

			setState(1469);
			match(T__17);
			setState(1471);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1470);
				match(WS);
				}
			}

			setState(1473);
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
		enterRule(_localctx, 190, RULE_existential);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1475);
			_la = _input.LA(1);
			if ( !(_la==T__23 || _la==T__24) ) {
			_errHandler.recoverInline(this);
			} else {
				consume();
			}
			setState(1477);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,236,_ctx) ) {
			case 1:
				{
				setState(1476);
				match(WS);
				}
				break;
			}
			setState(1479);
			formalparams();
			setState(1481);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1480);
				match(WS);
				}
			}

			setState(1483);
			match(T__0);
			setState(1485);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1484);
				match(WS);
				}
			}

			setState(1487);
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
		enterRule(_localctx, 192, RULE_universal);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1489);
			_la = _input.LA(1);
			if ( !(_la==T__25 || _la==T__26) ) {
			_errHandler.recoverInline(this);
			} else {
				consume();
			}
			setState(1491);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,239,_ctx) ) {
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
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1498);
				match(WS);
				}
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
		enterRule(_localctx, 194, RULE_pattern);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1503);
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
		enterRule(_localctx, 196, RULE_patterns);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1505);
			pattern();
			setState(1516);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,244,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1507);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1506);
						match(WS);
						}
					}

					setState(1509);
					match(T__2);
					setState(1511);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1510);
						match(WS);
						}
					}

					setState(1513);
					pattern();
					}
					} 
				}
				setState(1518);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,244,_ctx);
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
		enterRule(_localctx, 198, RULE_simple);
		try {
			setState(1528);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,245,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1519);
				p_fNil();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1520);
				p_literal();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1521);
				p_variable();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(1522);
				p_wild();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(1523);
				p_tag();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(1524);
				p_tuple();
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(1525);
				p_fVec();
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(1526);
				p_fSet();
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(1527);
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
		enterRule(_localctx, 200, RULE_p_keyValue);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1530);
			pattern();
			setState(1532);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1531);
				match(WS);
				}
			}

			setState(1534);
			match(T__17);
			setState(1536);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1535);
				match(WS);
				}
			}

			setState(1538);
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
		enterRule(_localctx, 202, RULE_p_keyValues);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1540);
			p_keyValue();
			setState(1551);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,250,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1542);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1541);
						match(WS);
						}
					}

					setState(1544);
					match(T__2);
					setState(1546);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1545);
						match(WS);
						}
					}

					setState(1548);
					p_keyValue();
					}
					} 
				}
				setState(1553);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,250,_ctx);
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
		enterRule(_localctx, 204, RULE_p_literal);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1554);
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
		enterRule(_localctx, 206, RULE_p_tag);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1559);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,251,_ctx) ) {
			case 1:
				{
				setState(1556);
				qualifiedTypeName();
				setState(1557);
				match(T__0);
				}
				break;
			}
			setState(1561);
			tagName();
			setState(1566);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,253,_ctx) ) {
			case 1:
				{
				setState(1563);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1562);
					match(WS);
					}
				}

				setState(1565);
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
		enterRule(_localctx, 208, RULE_p_tuple);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1568);
			match(T__4);
			setState(1570);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,254,_ctx) ) {
			case 1:
				{
				setState(1569);
				match(WS);
				}
				break;
			}
			setState(1573);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__4) | (1L << T__20) | (1L << T__21) | (1L << T__22) | (1L << T__28) | (1L << T__29) | (1L << T__30))) != 0) || ((((_la - 86)) & ~0x3f) == 0 && ((1L << (_la - 86)) & ((1L << (FNIL - 86)) | (1L << (WILD - 86)) | (1L << (LowerIdent - 86)) | (1L << (UpperIdent - 86)) | (1L << (Chars - 86)) | (1L << (Strs - 86)) | (1L << (Digits - 86)))) != 0)) {
				{
				setState(1572);
				patterns();
				}
			}

			setState(1576);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1575);
				match(WS);
				}
			}

			setState(1578);
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
		enterRule(_localctx, 210, RULE_p_wild);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1580);
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
		enterRule(_localctx, 212, RULE_p_fNil);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1582);
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
		enterRule(_localctx, 214, RULE_p_variable);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1584);
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
		enterRule(_localctx, 216, RULE_p_fList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1586);
			simple();
			setState(1595);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,259,_ctx) ) {
			case 1:
				{
				setState(1588);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1587);
					match(WS);
					}
				}

				setState(1590);
				match(T__19);
				setState(1592);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1591);
					match(WS);
					}
				}

				setState(1594);
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
		enterRule(_localctx, 218, RULE_p_fVec);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1597);
			match(T__20);
			setState(1599);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,260,_ctx) ) {
			case 1:
				{
				setState(1598);
				match(WS);
				}
				break;
			}
			setState(1602);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__4) | (1L << T__20) | (1L << T__21) | (1L << T__22) | (1L << T__28) | (1L << T__29) | (1L << T__30))) != 0) || ((((_la - 86)) & ~0x3f) == 0 && ((1L << (_la - 86)) & ((1L << (FNIL - 86)) | (1L << (WILD - 86)) | (1L << (LowerIdent - 86)) | (1L << (UpperIdent - 86)) | (1L << (Chars - 86)) | (1L << (Strs - 86)) | (1L << (Digits - 86)))) != 0)) {
				{
				setState(1601);
				patterns();
				}
			}

			setState(1614);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,264,_ctx) ) {
			case 1:
				{
				setState(1605);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1604);
					match(WS);
					}
				}

				setState(1607);
				match(T__2);
				setState(1609);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1608);
					match(WS);
					}
				}

				setState(1611);
				pattern();
				setState(1612);
				match(T__27);
				}
				break;
			}
			setState(1617);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1616);
				match(WS);
				}
			}

			setState(1619);
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
		enterRule(_localctx, 220, RULE_p_fSet);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1621);
			match(T__21);
			setState(1623);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,266,_ctx) ) {
			case 1:
				{
				setState(1622);
				match(WS);
				}
				break;
			}
			setState(1626);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__4) | (1L << T__20) | (1L << T__21) | (1L << T__22) | (1L << T__28) | (1L << T__29) | (1L << T__30))) != 0) || ((((_la - 86)) & ~0x3f) == 0 && ((1L << (_la - 86)) & ((1L << (FNIL - 86)) | (1L << (WILD - 86)) | (1L << (LowerIdent - 86)) | (1L << (UpperIdent - 86)) | (1L << (Chars - 86)) | (1L << (Strs - 86)) | (1L << (Digits - 86)))) != 0)) {
				{
				setState(1625);
				patterns();
				}
			}

			setState(1638);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,270,_ctx) ) {
			case 1:
				{
				setState(1629);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1628);
					match(WS);
					}
				}

				setState(1631);
				match(T__2);
				setState(1633);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1632);
					match(WS);
					}
				}

				setState(1635);
				pattern();
				setState(1636);
				match(T__27);
				}
				break;
			}
			setState(1641);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1640);
				match(WS);
				}
			}

			setState(1643);
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
		enterRule(_localctx, 222, RULE_p_fMap);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1645);
			match(T__22);
			setState(1647);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,272,_ctx) ) {
			case 1:
				{
				setState(1646);
				match(WS);
				}
				break;
			}
			setState(1650);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__4) | (1L << T__20) | (1L << T__21) | (1L << T__22) | (1L << T__28) | (1L << T__29) | (1L << T__30))) != 0) || ((((_la - 86)) & ~0x3f) == 0 && ((1L << (_la - 86)) & ((1L << (FNIL - 86)) | (1L << (WILD - 86)) | (1L << (LowerIdent - 86)) | (1L << (UpperIdent - 86)) | (1L << (Chars - 86)) | (1L << (Strs - 86)) | (1L << (Digits - 86)))) != 0)) {
				{
				setState(1649);
				p_keyValues();
				}
			}

			setState(1662);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,276,_ctx) ) {
			case 1:
				{
				setState(1653);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1652);
					match(WS);
					}
				}

				setState(1655);
				match(T__2);
				setState(1657);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1656);
					match(WS);
					}
				}

				setState(1659);
				pattern();
				setState(1660);
				match(T__27);
				}
				break;
			}
			setState(1665);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1664);
				match(WS);
				}
			}

			setState(1667);
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
		enterRule(_localctx, 224, RULE_bools);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1669);
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
		enterRule(_localctx, 226, RULE_chars);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1671);
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
		enterRule(_localctx, 228, RULE_strs);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1673);
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
		enterRule(_localctx, 230, RULE_negative);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1675);
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
		enterRule(_localctx, 232, RULE_float32);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1678);
			_la = _input.LA(1);
			if (_la==T__30) {
				{
				setState(1677);
				negative();
				}
			}

			setState(1680);
			match(Digits);
			setState(1681);
			match(T__0);
			setState(1682);
			match(Digits);
			setState(1683);
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
		enterRule(_localctx, 234, RULE_float64);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1686);
			_la = _input.LA(1);
			if (_la==T__30) {
				{
				setState(1685);
				negative();
				}
			}

			setState(1688);
			match(Digits);
			setState(1689);
			match(T__0);
			setState(1690);
			match(Digits);
			setState(1691);
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
		enterRule(_localctx, 236, RULE_floatDefault);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1694);
			_la = _input.LA(1);
			if (_la==T__30) {
				{
				setState(1693);
				negative();
				}
			}

			setState(1696);
			match(Digits);
			setState(1697);
			match(T__0);
			setState(1698);
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
		enterRule(_localctx, 238, RULE_floats);
		try {
			setState(1703);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,281,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1700);
				float32();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1701);
				float64();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1702);
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
		enterRule(_localctx, 240, RULE_int8);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1706);
			_la = _input.LA(1);
			if (_la==T__30) {
				{
				setState(1705);
				negative();
				}
			}

			setState(1708);
			match(Digits);
			setState(1709);
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
		enterRule(_localctx, 242, RULE_int16);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1712);
			_la = _input.LA(1);
			if (_la==T__30) {
				{
				setState(1711);
				negative();
				}
			}

			setState(1714);
			match(Digits);
			setState(1715);
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
		enterRule(_localctx, 244, RULE_int32);
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
		enterRule(_localctx, 246, RULE_int64);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1724);
			_la = _input.LA(1);
			if (_la==T__30) {
				{
				setState(1723);
				negative();
				}
			}

			setState(1726);
			match(Digits);
			setState(1727);
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
		enterRule(_localctx, 248, RULE_bigInt);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1730);
			_la = _input.LA(1);
			if (_la==T__30) {
				{
				setState(1729);
				negative();
				}
			}

			setState(1732);
			match(Digits);
			setState(1733);
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
		enterRule(_localctx, 250, RULE_intDefault);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1736);
			_la = _input.LA(1);
			if (_la==T__30) {
				{
				setState(1735);
				negative();
				}
			}

			setState(1738);
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
		enterRule(_localctx, 252, RULE_ints);
		try {
			setState(1746);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,288,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1740);
				int8();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1741);
				int16();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1742);
				int32();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(1743);
				int64();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(1744);
				bigInt();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(1745);
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
		enterRule(_localctx, 254, RULE_literal);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1753);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,289,_ctx) ) {
			case 1:
				{
				setState(1748);
				bools();
				}
				break;
			case 2:
				{
				setState(1749);
				chars();
				}
				break;
			case 3:
				{
				setState(1750);
				floats();
				}
				break;
			case 4:
				{
				setState(1751);
				ints();
				}
				break;
			case 5:
				{
				setState(1752);
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
		enterRule(_localctx, 256, RULE_primary);
		try {
			setState(1760);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,290,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1755);
				arrow();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1756);
				tuple();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1757);
				apply();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(1758);
				var();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(1759);
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
		enterRule(_localctx, 258, RULE_var);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1762);
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
		enterRule(_localctx, 260, RULE_ref);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1764);
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
		enterRule(_localctx, 262, RULE_type);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1766);
			primary();
			setState(1775);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,293,_ctx) ) {
			case 1:
				{
				setState(1768);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1767);
					match(WS);
					}
				}

				setState(1770);
				match(T__17);
				setState(1772);
				_la = _input.LA(1);
				if (_la==WS) {
					{
					setState(1771);
					match(WS);
					}
				}

				setState(1774);
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
		enterRule(_localctx, 264, RULE_arrow);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1777);
			match(T__4);
			setState(1779);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1778);
				match(WS);
				}
			}

			setState(1781);
			type();
			setState(1792);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,297,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1783);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1782);
						match(WS);
						}
					}

					setState(1785);
					match(T__2);
					setState(1787);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1786);
						match(WS);
						}
					}

					setState(1789);
					type();
					}
					} 
				}
				setState(1794);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,297,_ctx);
			}
			setState(1796);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1795);
				match(WS);
				}
			}

			setState(1798);
			match(T__5);
			setState(1800);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1799);
				match(WS);
				}
			}

			setState(1802);
			match(T__17);
			setState(1804);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1803);
				match(WS);
				}
			}

			setState(1806);
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
		enterRule(_localctx, 266, RULE_tuple_unit);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1808);
			match(T__4);
			setState(1809);
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
		enterRule(_localctx, 268, RULE_tuple_singleton);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1811);
			match(T__4);
			setState(1813);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1812);
				match(WS);
				}
			}

			setState(1815);
			type();
			setState(1817);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1816);
				match(WS);
				}
			}

			setState(1819);
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
		enterRule(_localctx, 270, RULE_tuple_multi);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1821);
			match(T__4);
			setState(1823);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1822);
				match(WS);
				}
			}

			setState(1825);
			type();
			setState(1834); 
			_errHandler.sync(this);
			_alt = 1;
			do {
				switch (_alt) {
				case 1:
					{
					{
					setState(1827);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1826);
						match(WS);
						}
					}

					setState(1829);
					match(T__2);
					setState(1831);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1830);
						match(WS);
						}
					}

					setState(1833);
					type();
					}
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(1836); 
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,306,_ctx);
			} while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER );
			setState(1839);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1838);
				match(WS);
				}
			}

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
		enterRule(_localctx, 272, RULE_tuple);
		try {
			setState(1846);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,308,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1843);
				tuple_unit();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1844);
				tuple_singleton();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1845);
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
		enterRule(_localctx, 274, RULE_apply);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1848);
			ref();
			setState(1850);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1849);
				match(WS);
				}
			}

			setState(1852);
			match(T__9);
			setState(1854);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1853);
				match(WS);
				}
			}

			setState(1856);
			type();
			setState(1867);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,313,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1858);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1857);
						match(WS);
						}
					}

					setState(1860);
					match(T__2);
					setState(1862);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1861);
						match(WS);
						}
					}

					setState(1864);
					type();
					}
					} 
				}
				setState(1869);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,313,_ctx);
			}
			setState(1871);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1870);
				match(WS);
				}
			}

			setState(1873);
			match(T__10);
			setState(1875);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,315,_ctx) ) {
			case 1:
				{
				setState(1874);
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
		enterRule(_localctx, 276, RULE_unary_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1877);
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
		enterRule(_localctx, 278, RULE_logical_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1879);
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
		enterRule(_localctx, 280, RULE_comparison_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1881);
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
		enterRule(_localctx, 282, RULE_multipve_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1883);
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
		enterRule(_localctx, 284, RULE_addve_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1885);
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
		enterRule(_localctx, 286, RULE_extbin_ops);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1887);
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
		enterRule(_localctx, 288, RULE_predicate);
		try {
			setState(1895);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,316,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1889);
				pred_true();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1890);
				pred_false();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1891);
				pred_filter();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(1892);
				pred_notequal();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(1893);
				pred_table();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(1894);
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
		enterRule(_localctx, 290, RULE_predicates);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1897);
			predicate();
			setState(1908);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,319,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1899);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1898);
						match(WS);
						}
					}

					setState(1901);
					match(T__2);
					setState(1903);
					_la = _input.LA(1);
					if (_la==WS) {
						{
						setState(1902);
						match(WS);
						}
					}

					setState(1905);
					predicate();
					}
					} 
				}
				setState(1910);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,319,_ctx);
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
		enterRule(_localctx, 292, RULE_pred_true);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1911);
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
		enterRule(_localctx, 294, RULE_pred_false);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1913);
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
		enterRule(_localctx, 296, RULE_pred_filter);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1915);
			qualifiedDefinitionName();
			setState(1917);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1916);
				match(WS);
				}
			}

			setState(1919);
			match(T__4);
			setState(1920);
			expressions();
			setState(1921);
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
		enterRule(_localctx, 298, RULE_pred_table);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1923);
			qualifiedTableName();
			setState(1925);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1924);
				match(WS);
				}
			}

			setState(1927);
			match(T__4);
			setState(1928);
			expressions();
			setState(1929);
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
		enterRule(_localctx, 300, RULE_pred_notequal);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1931);
			variableName();
			setState(1933);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1932);
				match(WS);
				}
			}

			setState(1935);
			match(T__59);
			setState(1937);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1936);
				match(WS);
				}
			}

			setState(1939);
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
		enterRule(_localctx, 302, RULE_pred_loop);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1941);
			variableName();
			setState(1943);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1942);
				match(WS);
				}
			}

			setState(1945);
			match(T__69);
			setState(1947);
			_la = _input.LA(1);
			if (_la==WS) {
				{
				setState(1946);
				match(WS);
				}
			}

			setState(1949);
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
		"\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd\3e\u07a2\4\2\t\2\4"+
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
		"\t\u0097\4\u0098\t\u0098\4\u0099\t\u0099\3\2\3\2\3\3\7\3\u0136\n\3\f\3"+
		"\16\3\u0139\13\3\3\3\7\3\u013c\n\3\f\3\16\3\u013f\13\3\3\3\5\3\u0142\n"+
		"\3\3\3\3\3\3\4\5\4\u0147\n\4\3\4\5\4\u014a\n\4\3\5\3\5\3\6\3\6\3\6\7\6"+
		"\u0151\n\6\f\6\16\6\u0154\13\6\3\7\3\7\3\7\5\7\u0159\n\7\3\7\3\7\3\b\3"+
		"\b\3\b\5\b\u0160\n\b\3\b\3\b\3\t\3\t\3\n\3\n\3\13\3\13\3\f\3\f\3\r\3\r"+
		"\3\16\3\16\3\17\3\17\3\20\3\20\3\21\3\21\3\22\3\22\3\23\3\23\3\24\3\24"+
		"\5\24\u017c\n\24\3\24\3\24\5\24\u0180\n\24\3\24\3\24\3\25\3\25\3\25\5"+
		"\25\u0187\n\25\3\25\3\25\3\26\3\26\5\26\u018d\n\26\3\26\3\26\5\26\u0191"+
		"\n\26\3\26\7\26\u0194\n\26\f\26\16\26\u0197\13\26\3\27\3\27\5\27\u019b"+
		"\n\27\3\27\5\27\u019e\n\27\3\27\5\27\u01a1\n\27\3\27\5\27\u01a4\n\27\3"+
		"\30\3\30\5\30\u01a8\n\30\3\30\3\30\5\30\u01ac\n\30\3\30\3\30\3\31\3\31"+
		"\5\31\u01b2\n\31\3\31\3\31\5\31\u01b6\n\31\3\31\7\31\u01b9\n\31\f\31\16"+
		"\31\u01bc\13\31\3\32\3\32\5\32\u01c0\n\32\3\32\3\32\5\32\u01c4\n\32\3"+
		"\32\3\32\5\32\u01c8\n\32\3\32\7\32\u01cb\n\32\f\32\16\32\u01ce\13\32\5"+
		"\32\u01d0\n\32\3\32\5\32\u01d3\n\32\3\32\3\32\3\33\3\33\5\33\u01d9\n\33"+
		"\3\33\3\33\5\33\u01dd\n\33\3\33\7\33\u01e0\n\33\f\33\16\33\u01e3\13\33"+
		"\3\34\3\34\3\34\3\34\5\34\u01e9\n\34\3\34\3\34\5\34\u01ed\n\34\3\34\3"+
		"\34\5\34\u01f1\n\34\3\35\3\35\5\35\u01f5\n\35\3\35\7\35\u01f8\n\35\f\35"+
		"\16\35\u01fb\13\35\3\36\3\36\3\36\3\36\5\36\u0201\n\36\3\36\3\36\5\36"+
		"\u0205\n\36\3\36\3\36\5\36\u0209\n\36\3\37\3\37\5\37\u020d\n\37\3\37\7"+
		"\37\u0210\n\37\f\37\16\37\u0213\13\37\3 \3 \5 \u0217\n \3 \3 \5 \u021b"+
		"\n \3 \5 \u021e\n \3!\3!\5!\u0222\n!\3!\3!\5!\u0226\n!\3!\3!\5!\u022a"+
		"\n!\3!\7!\u022d\n!\f!\16!\u0230\13!\3!\3!\5!\u0234\n!\3\"\3\"\3\"\5\""+
		"\u0239\n\"\3\"\3\"\5\"\u023d\n\"\3\"\7\"\u0240\n\"\f\"\16\"\u0243\13\""+
		"\3\"\3\"\3#\3#\3#\3$\3$\5$\u024c\n$\3$\3$\5$\u0250\n$\3$\7$\u0253\n$\f"+
		"$\16$\u0256\13$\3%\5%\u0259\n%\3%\3%\5%\u025d\n%\3%\3%\5%\u0261\n%\5%"+
		"\u0263\n%\3&\3&\3&\3\'\3\'\3\'\7\'\u026b\n\'\f\'\16\'\u026e\13\'\3(\5"+
		"(\u0271\n(\3(\3(\3(\5(\u0276\n(\3)\3)\3)\3)\3)\3)\3)\3*\3*\3*\3*\3*\3"+
		"*\3*\3+\3+\3+\3+\3+\3,\3,\3,\3,\3,\3,\3,\3,\3,\3,\3,\3,\3,\5,\u0298\n"+
		",\3-\5-\u029b\n-\3-\3-\3-\3-\5-\u02a1\n-\3-\3-\5-\u02a5\n-\3-\7-\u02a8"+
		"\n-\f-\16-\u02ab\13-\3-\5-\u02ae\n-\3-\3-\3-\3.\5.\u02b4\n.\3.\7.\u02b7"+
		"\n.\f.\16.\u02ba\13.\3.\5.\u02bd\n.\3.\3.\3.\3.\3.\5.\u02c4\n.\3.\3.\5"+
		".\u02c8\n.\3.\3.\5.\u02cc\n.\3.\3.\3.\3/\3/\5/\u02d3\n/\3/\3/\5/\u02d7"+
		"\n/\3/\7/\u02da\n/\f/\16/\u02dd\13/\3\60\3\60\3\60\3\60\5\60\u02e3\n\60"+
		"\3\61\5\61\u02e6\n\61\3\61\7\61\u02e9\n\61\f\61\16\61\u02ec\13\61\3\61"+
		"\5\61\u02ef\n\61\3\61\3\61\3\61\3\61\5\61\u02f5\n\61\3\61\3\61\5\61\u02f9"+
		"\n\61\3\61\5\61\u02fc\n\61\3\61\5\61\u02ff\n\61\3\61\3\61\3\61\3\62\5"+
		"\62\u0305\n\62\3\62\7\62\u0308\n\62\f\62\16\62\u030b\13\62\3\62\5\62\u030e"+
		"\n\62\3\62\3\62\3\62\3\62\5\62\u0314\n\62\3\62\3\62\5\62\u0318\n\62\3"+
		"\62\5\62\u031b\n\62\3\62\5\62\u031e\n\62\3\62\3\62\3\62\3\63\5\63\u0324"+
		"\n\63\3\63\3\63\3\63\3\63\5\63\u032a\n\63\3\63\3\63\5\63\u032e\n\63\3"+
		"\63\5\63\u0331\n\63\3\63\5\63\u0334\n\63\3\63\3\63\3\63\3\64\5\64\u033a"+
		"\n\64\3\64\7\64\u033d\n\64\f\64\16\64\u0340\13\64\3\64\5\64\u0343\n\64"+
		"\3\64\3\64\3\64\3\64\5\64\u0349\n\64\3\64\3\64\5\64\u034d\n\64\3\64\3"+
		"\64\5\64\u0351\n\64\3\64\3\64\3\64\3\65\5\65\u0357\n\65\3\65\7\65\u035a"+
		"\n\65\f\65\16\65\u035d\13\65\3\65\5\65\u0360\n\65\3\65\3\65\5\65\u0364"+
		"\n\65\3\65\3\65\3\65\3\65\5\65\u036a\n\65\3\65\3\65\5\65\u036e\n\65\3"+
		"\65\3\65\5\65\u0372\n\65\3\65\3\65\3\65\3\66\5\66\u0378\n\66\3\66\7\66"+
		"\u037b\n\66\f\66\16\66\u037e\13\66\3\66\5\66\u0381\n\66\3\66\5\66\u0384"+
		"\n\66\3\66\5\66\u0387\n\66\3\66\3\66\3\66\3\66\5\66\u038d\n\66\3\66\3"+
		"\66\3\66\5\66\u0392\n\66\3\66\3\66\5\66\u0396\n\66\3\66\3\66\5\66\u039a"+
		"\n\66\3\66\3\66\5\66\u039e\n\66\3\66\3\66\3\66\3\67\5\67\u03a4\n\67\3"+
		"\67\7\67\u03a7\n\67\f\67\16\67\u03aa\13\67\3\67\5\67\u03ad\n\67\3\67\3"+
		"\67\3\67\3\67\5\67\u03b3\n\67\3\67\3\67\5\67\u03b7\n\67\3\67\3\67\5\67"+
		"\u03bb\n\67\3\67\3\67\5\67\u03bf\n\67\3\67\3\67\5\67\u03c3\n\67\3\67\3"+
		"\67\5\67\u03c7\n\67\3\67\3\67\3\67\38\58\u03cd\n8\38\78\u03d0\n8\f8\16"+
		"8\u03d3\138\38\58\u03d6\n8\38\38\38\38\38\58\u03dd\n8\38\38\58\u03e1\n"+
		"8\38\38\39\39\59\u03e7\n9\39\39\59\u03eb\n9\79\u03ed\n9\f9\169\u03f0\13"+
		"9\39\39\3:\3:\3:\5:\u03f7\n:\3;\5;\u03fa\n;\3;\3;\5;\u03fe\n;\3;\3;\3"+
		"<\5<\u0403\n<\3<\3<\5<\u0407\n<\3<\3<\5<\u040b\n<\3<\3<\5<\u040f\n<\3"+
		"<\3<\3=\3=\3>\5>\u0416\n>\3>\3>\5>\u041a\n>\3>\3>\3>\5>\u041f\n>\3>\3"+
		">\5>\u0423\n>\3>\3>\5>\u0427\n>\3>\3>\5>\u042b\n>\3>\3>\3>\3?\5?\u0431"+
		"\n?\3?\7?\u0434\n?\f?\16?\u0437\13?\3?\5?\u043a\n?\3?\3?\3?\3?\3?\5?\u0441"+
		"\n?\3?\3?\5?\u0445\n?\3?\3?\3@\3@\5@\u044b\n@\3@\7@\u044e\n@\f@\16@\u0451"+
		"\13@\3@\5@\u0454\n@\3@\3@\3A\3A\5A\u045a\nA\3A\3A\5A\u045e\nA\3A\3A\5"+
		"A\u0462\nA\3A\5A\u0465\nA\3B\3B\5B\u0469\nB\3B\3B\5B\u046d\nB\3B\3B\5"+
		"B\u0471\nB\3C\3C\5C\u0475\nC\3C\3C\5C\u0479\nC\3C\7C\u047c\nC\fC\16C\u047f"+
		"\13C\3D\3D\5D\u0483\nD\3D\3D\5D\u0487\nD\3D\3D\5D\u048b\nD\3E\3E\5E\u048f"+
		"\nE\3E\3E\5E\u0493\nE\3E\3E\5E\u0497\nE\3F\3F\5F\u049b\nF\3F\3F\5F\u049f"+
		"\nF\3F\3F\5F\u04a3\nF\3G\3G\5G\u04a7\nG\3G\3G\3G\3G\5G\u04ad\nG\3G\3G"+
		"\5G\u04b1\nG\3H\3H\5H\u04b5\nH\3H\3H\5H\u04b9\nH\3H\3H\5H\u04bd\nH\3I"+
		"\3I\5I\u04c1\nI\3I\3I\3I\5I\u04c6\nI\3J\3J\5J\u04ca\nJ\3J\3J\5J\u04ce"+
		"\nJ\3J\5J\u04d1\nJ\3K\3K\3K\3K\3K\3K\3K\3K\3K\3K\3K\3K\3K\3K\3K\3K\3K"+
		"\3K\3K\5K\u04e6\nK\3L\3L\3L\3L\5L\u04ec\nL\3L\3L\5L\u04f0\nL\3L\3L\5L"+
		"\u04f4\nL\3L\3L\5L\u04f8\nL\3L\3L\3M\3M\5M\u04fe\nM\3M\3M\5M\u0502\nM"+
		"\3M\3M\5M\u0506\nM\3M\3M\5M\u050a\nM\3M\3M\3M\3M\3M\3M\3N\3N\3N\3N\3N"+
		"\3N\3N\3N\5N\u051a\nN\3N\3N\5N\u051e\nN\3N\3N\3O\3O\3O\3O\5O\u0526\nO"+
		"\3O\3O\5O\u052a\nO\3O\3O\3P\3P\5P\u0530\nP\3P\3P\5P\u0534\nP\3P\5P\u0537"+
		"\nP\3P\5P\u053a\nP\3P\5P\u053d\nP\3Q\3Q\3R\3R\3S\3S\3T\3T\3T\5T\u0548"+
		"\nT\3T\3T\5T\u054c\nT\3T\5T\u054f\nT\3U\3U\5U\u0553\nU\3U\5U\u0556\nU"+
		"\3U\5U\u0559\nU\3U\3U\3V\3V\5V\u055f\nV\3V\3V\5V\u0563\nV\3V\3V\3W\3W"+
		"\5W\u0569\nW\3W\3W\5W\u056d\nW\3W\7W\u0570\nW\fW\16W\u0573\13W\3X\3X\3"+
		"Y\3Y\3Z\3Z\3[\3[\5[\u057d\n[\3[\3[\5[\u0581\n[\3[\5[\u0584\n[\3\\\3\\"+
		"\5\\\u0588\n\\\3\\\5\\\u058b\n\\\3\\\5\\\u058e\n\\\3\\\3\\\3]\3]\5]\u0594"+
		"\n]\3]\5]\u0597\n]\3]\5]\u059a\n]\3]\3]\3^\3^\5^\u05a0\n^\3^\5^\u05a3"+
		"\n^\3^\5^\u05a6\n^\3^\3^\3_\3_\5_\u05ac\n_\3_\3_\5_\u05b0\n_\3_\3_\3`"+
		"\3`\5`\u05b6\n`\3`\3`\5`\u05ba\n`\3`\3`\5`\u05be\n`\3`\3`\5`\u05c2\n`"+
		"\3`\3`\3a\3a\5a\u05c8\na\3a\3a\5a\u05cc\na\3a\3a\5a\u05d0\na\3a\3a\3b"+
		"\3b\5b\u05d6\nb\3b\3b\5b\u05da\nb\3b\3b\5b\u05de\nb\3b\3b\3c\3c\3d\3d"+
		"\5d\u05e6\nd\3d\3d\5d\u05ea\nd\3d\7d\u05ed\nd\fd\16d\u05f0\13d\3e\3e\3"+
		"e\3e\3e\3e\3e\3e\3e\5e\u05fb\ne\3f\3f\5f\u05ff\nf\3f\3f\5f\u0603\nf\3"+
		"f\3f\3g\3g\5g\u0609\ng\3g\3g\5g\u060d\ng\3g\7g\u0610\ng\fg\16g\u0613\13"+
		"g\3h\3h\3i\3i\3i\5i\u061a\ni\3i\3i\5i\u061e\ni\3i\5i\u0621\ni\3j\3j\5"+
		"j\u0625\nj\3j\5j\u0628\nj\3j\5j\u062b\nj\3j\3j\3k\3k\3l\3l\3m\3m\3n\3"+
		"n\5n\u0637\nn\3n\3n\5n\u063b\nn\3n\5n\u063e\nn\3o\3o\5o\u0642\no\3o\5"+
		"o\u0645\no\3o\5o\u0648\no\3o\3o\5o\u064c\no\3o\3o\3o\5o\u0651\no\3o\5"+
		"o\u0654\no\3o\3o\3p\3p\5p\u065a\np\3p\5p\u065d\np\3p\5p\u0660\np\3p\3"+
		"p\5p\u0664\np\3p\3p\3p\5p\u0669\np\3p\5p\u066c\np\3p\3p\3q\3q\5q\u0672"+
		"\nq\3q\5q\u0675\nq\3q\5q\u0678\nq\3q\3q\5q\u067c\nq\3q\3q\3q\5q\u0681"+
		"\nq\3q\5q\u0684\nq\3q\3q\3r\3r\3s\3s\3t\3t\3u\3u\3v\5v\u0691\nv\3v\3v"+
		"\3v\3v\3v\3w\5w\u0699\nw\3w\3w\3w\3w\3w\3x\5x\u06a1\nx\3x\3x\3x\3x\3y"+
		"\3y\3y\5y\u06aa\ny\3z\5z\u06ad\nz\3z\3z\3z\3{\5{\u06b3\n{\3{\3{\3{\3|"+
		"\5|\u06b9\n|\3|\3|\3|\3}\5}\u06bf\n}\3}\3}\3}\3~\5~\u06c5\n~\3~\3~\3~"+
		"\3\177\5\177\u06cb\n\177\3\177\3\177\3\u0080\3\u0080\3\u0080\3\u0080\3"+
		"\u0080\3\u0080\5\u0080\u06d5\n\u0080\3\u0081\3\u0081\3\u0081\3\u0081\3"+
		"\u0081\5\u0081\u06dc\n\u0081\3\u0082\3\u0082\3\u0082\3\u0082\3\u0082\5"+
		"\u0082\u06e3\n\u0082\3\u0083\3\u0083\3\u0084\3\u0084\3\u0085\3\u0085\5"+
		"\u0085\u06eb\n\u0085\3\u0085\3\u0085\5\u0085\u06ef\n\u0085\3\u0085\5\u0085"+
		"\u06f2\n\u0085\3\u0086\3\u0086\5\u0086\u06f6\n\u0086\3\u0086\3\u0086\5"+
		"\u0086\u06fa\n\u0086\3\u0086\3\u0086\5\u0086\u06fe\n\u0086\3\u0086\7\u0086"+
		"\u0701\n\u0086\f\u0086\16\u0086\u0704\13\u0086\3\u0086\5\u0086\u0707\n"+
		"\u0086\3\u0086\3\u0086\5\u0086\u070b\n\u0086\3\u0086\3\u0086\5\u0086\u070f"+
		"\n\u0086\3\u0086\3\u0086\3\u0087\3\u0087\3\u0087\3\u0088\3\u0088\5\u0088"+
		"\u0718\n\u0088\3\u0088\3\u0088\5\u0088\u071c\n\u0088\3\u0088\3\u0088\3"+
		"\u0089\3\u0089\5\u0089\u0722\n\u0089\3\u0089\3\u0089\5\u0089\u0726\n\u0089"+
		"\3\u0089\3\u0089\5\u0089\u072a\n\u0089\3\u0089\6\u0089\u072d\n\u0089\r"+
		"\u0089\16\u0089\u072e\3\u0089\5\u0089\u0732\n\u0089\3\u0089\3\u0089\3"+
		"\u008a\3\u008a\3\u008a\5\u008a\u0739\n\u008a\3\u008b\3\u008b\5\u008b\u073d"+
		"\n\u008b\3\u008b\3\u008b\5\u008b\u0741\n\u008b\3\u008b\3\u008b\5\u008b"+
		"\u0745\n\u008b\3\u008b\3\u008b\5\u008b\u0749\n\u008b\3\u008b\7\u008b\u074c"+
		"\n\u008b\f\u008b\16\u008b\u074f\13\u008b\3\u008b\5\u008b\u0752\n\u008b"+
		"\3\u008b\3\u008b\5\u008b\u0756\n\u008b\3\u008c\3\u008c\3\u008d\3\u008d"+
		"\3\u008e\3\u008e\3\u008f\3\u008f\3\u0090\3\u0090\3\u0091\3\u0091\3\u0092"+
		"\3\u0092\3\u0092\3\u0092\3\u0092\3\u0092\5\u0092\u076a\n\u0092\3\u0093"+
		"\3\u0093\5\u0093\u076e\n\u0093\3\u0093\3\u0093\5\u0093\u0772\n\u0093\3"+
		"\u0093\7\u0093\u0775\n\u0093\f\u0093\16\u0093\u0778\13\u0093\3\u0094\3"+
		"\u0094\3\u0095\3\u0095\3\u0096\3\u0096\5\u0096\u0780\n\u0096\3\u0096\3"+
		"\u0096\3\u0096\3\u0096\3\u0097\3\u0097\5\u0097\u0788\n\u0097\3\u0097\3"+
		"\u0097\3\u0097\3\u0097\3\u0098\3\u0098\5\u0098\u0790\n\u0098\3\u0098\3"+
		"\u0098\5\u0098\u0794\n\u0098\3\u0098\3\u0098\3\u0099\3\u0099\5\u0099\u079a"+
		"\n\u0099\3\u0099\3\u0099\5\u0099\u079e\n\u0099\3\u0099\3\u0099\3\u0099"+
		"\2\2\u009a\2\4\6\b\n\f\16\20\22\24\26\30\32\34\36 \"$&(*,.\60\62\64\66"+
		"8:<>@BDFHJLNPRTVXZ\\^`bdfhjlnprtvxz|~\u0080\u0082\u0084\u0086\u0088\u008a"+
		"\u008c\u008e\u0090\u0092\u0094\u0096\u0098\u009a\u009c\u009e\u00a0\u00a2"+
		"\u00a4\u00a6\u00a8\u00aa\u00ac\u00ae\u00b0\u00b2\u00b4\u00b6\u00b8\u00ba"+
		"\u00bc\u00be\u00c0\u00c2\u00c4\u00c6\u00c8\u00ca\u00cc\u00ce\u00d0\u00d2"+
		"\u00d4\u00d6\u00d8\u00da\u00dc\u00de\u00e0\u00e2\u00e4\u00e6\u00e8\u00ea"+
		"\u00ec\u00ee\u00f0\u00f2\u00f4\u00f6\u00f8\u00fa\u00fc\u00fe\u0100\u0102"+
		"\u0104\u0106\u0108\u010a\u010c\u010e\u0110\u0112\u0114\u0116\u0118\u011a"+
		"\u011c\u011e\u0120\u0122\u0124\u0126\u0128\u012a\u012c\u012e\u0130\2\f"+
		"\3\2ab\3\2\32\33\3\2\34\35\3\2\37 \4\2!!),\3\2-9\4\2\16\16:?\4\2\4\4@"+
		"B\4\2!!))\3\2CG\u0884\2\u0132\3\2\2\2\4\u0137\3\2\2\2\6\u0149\3\2\2\2"+
		"\b\u014b\3\2\2\2\n\u014d\3\2\2\2\f\u0158\3\2\2\2\16\u015f\3\2\2\2\20\u0163"+
		"\3\2\2\2\22\u0165\3\2\2\2\24\u0167\3\2\2\2\26\u0169\3\2\2\2\30\u016b\3"+
		"\2\2\2\32\u016d\3\2\2\2\34\u016f\3\2\2\2\36\u0171\3\2\2\2 \u0173\3\2\2"+
		"\2\"\u0175\3\2\2\2$\u0177\3\2\2\2&\u0179\3\2\2\2(\u0183\3\2\2\2*\u018a"+
		"\3\2\2\2,\u01a3\3\2\2\2.\u01a5\3\2\2\2\60\u01af\3\2\2\2\62\u01bd\3\2\2"+
		"\2\64\u01d6\3\2\2\2\66\u01e4\3\2\2\28\u01f2\3\2\2\2:\u01fc\3\2\2\2<\u020a"+
		"\3\2\2\2>\u0214\3\2\2\2@\u0233\3\2\2\2B\u0235\3\2\2\2D\u0246\3\2\2\2F"+
		"\u0249\3\2\2\2H\u0262\3\2\2\2J\u0264\3\2\2\2L\u0267\3\2\2\2N\u0270\3\2"+
		"\2\2P\u0277\3\2\2\2R\u027e\3\2\2\2T\u0285\3\2\2\2V\u0297\3\2\2\2X\u029a"+
		"\3\2\2\2Z\u02b8\3\2\2\2\\\u02d0\3\2\2\2^\u02de\3\2\2\2`\u02ea\3\2\2\2"+
		"b\u0309\3\2\2\2d\u0323\3\2\2\2f\u033e\3\2\2\2h\u035b\3\2\2\2j\u037c\3"+
		"\2\2\2l\u03a8\3\2\2\2n\u03d1\3\2\2\2p\u03e4\3\2\2\2r\u03f6\3\2\2\2t\u03f9"+
		"\3\2\2\2v\u0402\3\2\2\2x\u0412\3\2\2\2z\u0415\3\2\2\2|\u0435\3\2\2\2~"+
		"\u0448\3\2\2\2\u0080\u0464\3\2\2\2\u0082\u0466\3\2\2\2\u0084\u0472\3\2"+
		"\2\2\u0086\u0480\3\2\2\2\u0088\u048c\3\2\2\2\u008a\u0498\3\2\2\2\u008c"+
		"\u04a4\3\2\2\2\u008e\u04b2\3\2\2\2\u0090\u04c5\3\2\2\2\u0092\u04c7\3\2"+
		"\2\2\u0094\u04e5\3\2\2\2\u0096\u04e7\3\2\2\2\u0098\u04fb\3\2\2\2\u009a"+
		"\u0511\3\2\2\2\u009c\u0521\3\2\2\2\u009e\u052d\3\2\2\2\u00a0\u053e\3\2"+
		"\2\2\u00a2\u0540\3\2\2\2\u00a4\u0542\3\2\2\2\u00a6\u0547\3\2\2\2\u00a8"+
		"\u0550\3\2\2\2\u00aa\u055c\3\2\2\2\u00ac\u0566\3\2\2\2\u00ae\u0574\3\2"+
		"\2\2\u00b0\u0576\3\2\2\2\u00b2\u0578\3\2\2\2\u00b4\u057a\3\2\2\2\u00b6"+
		"\u0585\3\2\2\2\u00b8\u0591\3\2\2\2\u00ba\u059d\3\2\2\2\u00bc\u05a9\3\2"+
		"\2\2\u00be\u05b3\3\2\2\2\u00c0\u05c5\3\2\2\2\u00c2\u05d3\3\2\2\2\u00c4"+
		"\u05e1\3\2\2\2\u00c6\u05e3\3\2\2\2\u00c8\u05fa\3\2\2\2\u00ca\u05fc\3\2"+
		"\2\2\u00cc\u0606\3\2\2\2\u00ce\u0614\3\2\2\2\u00d0\u0619\3\2\2\2\u00d2"+
		"\u0622\3\2\2\2\u00d4\u062e\3\2\2\2\u00d6\u0630\3\2\2\2\u00d8\u0632\3\2"+
		"\2\2\u00da\u0634\3\2\2\2\u00dc\u063f\3\2\2\2\u00de\u0657\3\2\2\2\u00e0"+
		"\u066f\3\2\2\2\u00e2\u0687\3\2\2\2\u00e4\u0689\3\2\2\2\u00e6\u068b\3\2"+
		"\2\2\u00e8\u068d\3\2\2\2\u00ea\u0690\3\2\2\2\u00ec\u0698\3\2\2\2\u00ee"+
		"\u06a0\3\2\2\2\u00f0\u06a9\3\2\2\2\u00f2\u06ac\3\2\2\2\u00f4\u06b2\3\2"+
		"\2\2\u00f6\u06b8\3\2\2\2\u00f8\u06be\3\2\2\2\u00fa\u06c4\3\2\2\2\u00fc"+
		"\u06ca\3\2\2\2\u00fe\u06d4\3\2\2\2\u0100\u06db\3\2\2\2\u0102\u06e2\3\2"+
		"\2\2\u0104\u06e4\3\2\2\2\u0106\u06e6\3\2\2\2\u0108\u06e8\3\2\2\2\u010a"+
		"\u06f3\3\2\2\2\u010c\u0712\3\2\2\2\u010e\u0715\3\2\2\2\u0110\u071f\3\2"+
		"\2\2\u0112\u0738\3\2\2\2\u0114\u073a\3\2\2\2\u0116\u0757\3\2\2\2\u0118"+
		"\u0759\3\2\2\2\u011a\u075b\3\2\2\2\u011c\u075d\3\2\2\2\u011e\u075f\3\2"+
		"\2\2\u0120\u0761\3\2\2\2\u0122\u0769\3\2\2\2\u0124\u076b\3\2\2\2\u0126"+
		"\u0779\3\2\2\2\u0128\u077b\3\2\2\2\u012a\u077d\3\2\2\2\u012c\u0785\3\2"+
		"\2\2\u012e\u078d\3\2\2\2\u0130\u0797\3\2\2\2\u0132\u0133\7I\2\2\u0133"+
		"\3\3\2\2\2\u0134\u0136\5N(\2\u0135\u0134\3\2\2\2\u0136\u0139\3\2\2\2\u0137"+
		"\u0135\3\2\2\2\u0137\u0138\3\2\2\2\u0138\u013d\3\2\2\2\u0139\u0137\3\2"+
		"\2\2\u013a\u013c\5V,\2\u013b\u013a\3\2\2\2\u013c\u013f\3\2\2\2\u013d\u013b"+
		"\3\2\2\2\u013d\u013e\3\2\2\2\u013e\u0141\3\2\2\2\u013f\u013d\3\2\2\2\u0140"+
		"\u0142\7J\2\2\u0141\u0140\3\2\2\2\u0141\u0142\3\2\2\2\u0142\u0143\3\2"+
		"\2\2\u0143\u0144\7\2\2\3\u0144\5\3\2\2\2\u0145\u0147\7J\2\2\u0146\u0145"+
		"\3\2\2\2\u0146\u0147\3\2\2\2\u0147\u0148\3\2\2\2\u0148\u014a\7K\2\2\u0149"+
		"\u0146\3\2\2\2\u0149\u014a\3\2\2\2\u014a\7\3\2\2\2\u014b\u014c\t\2\2\2"+
		"\u014c\t\3\2\2\2\u014d\u0152\5\b\5\2\u014e\u014f\7\3\2\2\u014f\u0151\5"+
		"\b\5\2\u0150\u014e\3\2\2\2\u0151\u0154\3\2\2\2\u0152\u0150\3\2\2\2\u0152"+
		"\u0153\3\2\2\2\u0153\13\3\2\2\2\u0154\u0152\3\2\2\2\u0155\u0156\5\n\6"+
		"\2\u0156\u0157\7\4\2\2\u0157\u0159\3\2\2\2\u0158\u0155\3\2\2\2\u0158\u0159"+
		"\3\2\2\2\u0159\u015a\3\2\2\2\u015a\u015b\7a\2\2\u015b\r\3\2\2\2\u015c"+
		"\u015d\5\n\6\2\u015d\u015e\7\4\2\2\u015e\u0160\3\2\2\2\u015f\u015c\3\2"+
		"\2\2\u015f\u0160\3\2\2\2\u0160\u0161\3\2\2\2\u0161\u0162\7b\2\2\u0162"+
		"\17\3\2\2\2\u0163\u0164\7a\2\2\u0164\21\3\2\2\2\u0165\u0166\7a\2\2\u0166"+
		"\23\3\2\2\2\u0167\u0168\7b\2\2\u0168\25\3\2\2\2\u0169\u016a\7a\2\2\u016a"+
		"\27\3\2\2\2\u016b\u016c\5\f\7\2\u016c\31\3\2\2\2\u016d\u016e\7b\2\2\u016e"+
		"\33\3\2\2\2\u016f\u0170\5\16\b\2\u0170\35\3\2\2\2\u0171\u0172\7b\2\2\u0172"+
		"\37\3\2\2\2\u0173\u0174\7b\2\2\u0174!\3\2\2\2\u0175\u0176\5\16\b\2\u0176"+
		"#\3\2\2\2\u0177\u0178\7a\2\2\u0178%\3\2\2\2\u0179\u017b\5$\23\2\u017a"+
		"\u017c\7J\2\2\u017b\u017a\3\2\2\2\u017b\u017c\3\2\2\2\u017c\u017d\3\2"+
		"\2\2\u017d\u017f\7\5\2\2\u017e\u0180\7J\2\2\u017f\u017e\3\2\2\2\u017f"+
		"\u0180\3\2\2\2\u0180\u0181\3\2\2\2\u0181\u0182\5$\23\2\u0182\'\3\2\2\2"+
		"\u0183\u0184\5$\23\2\u0184\u0186\7\6\2\2\u0185\u0187\7J\2\2\u0186\u0185"+
		"\3\2\2\2\u0186\u0187\3\2\2\2\u0187\u0188\3\2\2\2\u0188\u0189\5\u0108\u0085"+
		"\2\u0189)\3\2\2\2\u018a\u0195\5(\25\2\u018b\u018d\7J\2\2\u018c\u018b\3"+
		"\2\2\2\u018c\u018d\3\2\2\2\u018d\u018e\3\2\2\2\u018e\u0190\7\5\2\2\u018f"+
		"\u0191\7J\2\2\u0190\u018f\3\2\2\2\u0190\u0191\3\2\2\2\u0191\u0192\3\2"+
		"\2\2\u0192\u0194\5(\25\2\u0193\u018c\3\2\2\2\u0194\u0197\3\2\2\2\u0195"+
		"\u0193\3\2\2\2\u0195\u0196\3\2\2\2\u0196+\3\2\2\2\u0197\u0195\3\2\2\2"+
		"\u0198\u019a\7\7\2\2\u0199\u019b\7J\2\2\u019a\u0199\3\2\2\2\u019a\u019b"+
		"\3\2\2\2\u019b\u019d\3\2\2\2\u019c\u019e\5*\26\2\u019d\u019c\3\2\2\2\u019d"+
		"\u019e\3\2\2\2\u019e\u01a0\3\2\2\2\u019f\u01a1\7J\2\2\u01a0\u019f\3\2"+
		"\2\2\u01a0\u01a1\3\2\2\2\u01a1\u01a2\3\2\2\2\u01a2\u01a4\7\b\2\2\u01a3"+
		"\u0198\3\2\2\2\u01a3\u01a4\3\2\2\2\u01a4-\3\2\2\2\u01a5\u01a7\5\22\n\2"+
		"\u01a6\u01a8\7J\2\2\u01a7\u01a6\3\2\2\2\u01a7\u01a8\3\2\2\2\u01a8\u01a9"+
		"\3\2\2\2\u01a9\u01ab\7\6\2\2\u01aa\u01ac\7J\2\2\u01ab\u01aa\3\2\2\2\u01ab"+
		"\u01ac\3\2\2\2\u01ac\u01ad\3\2\2\2\u01ad\u01ae\5\u0108\u0085\2\u01ae/"+
		"\3\2\2\2\u01af\u01ba\5.\30\2\u01b0\u01b2\7J\2\2\u01b1\u01b0\3\2\2\2\u01b1"+
		"\u01b2\3\2\2\2\u01b2\u01b3\3\2\2\2\u01b3\u01b5\7\5\2\2\u01b4\u01b6\7J"+
		"\2\2\u01b5\u01b4\3\2\2\2\u01b5\u01b6\3\2\2\2\u01b6\u01b7\3\2\2\2\u01b7"+
		"\u01b9\5.\30\2\u01b8\u01b1\3\2\2\2\u01b9\u01bc\3\2\2\2\u01ba\u01b8\3\2"+
		"\2\2\u01ba\u01bb\3\2\2\2\u01bb\61\3\2\2\2\u01bc\u01ba\3\2\2\2\u01bd\u01bf"+
		"\7\t\2\2\u01be\u01c0\7J\2\2\u01bf\u01be\3\2\2\2\u01bf\u01c0\3\2\2\2\u01c0"+
		"\u01cf\3\2\2\2\u01c1\u01cc\5\22\n\2\u01c2\u01c4\7J\2\2\u01c3\u01c2\3\2"+
		"\2\2\u01c3\u01c4\3\2\2\2\u01c4\u01c5\3\2\2\2\u01c5\u01c7\7\5\2\2\u01c6"+
		"\u01c8\7J\2\2\u01c7\u01c6\3\2\2\2\u01c7\u01c8\3\2\2\2\u01c8\u01c9\3\2"+
		"\2\2\u01c9\u01cb\5\22\n\2\u01ca\u01c3\3\2\2\2\u01cb\u01ce\3\2\2\2\u01cc"+
		"\u01ca\3\2\2\2\u01cc\u01cd\3\2\2\2\u01cd\u01d0\3\2\2\2\u01ce\u01cc\3\2"+
		"\2\2\u01cf\u01c1\3\2\2\2\u01cf\u01d0\3\2\2\2\u01d0\u01d2\3\2\2\2\u01d1"+
		"\u01d3\7J\2\2\u01d2\u01d1\3\2\2\2\u01d2\u01d3\3\2\2\2\u01d3\u01d4\3\2"+
		"\2\2\u01d4\u01d5\7\n\2\2\u01d5\63\3\2\2\2\u01d6\u01e1\5\62\32\2\u01d7"+
		"\u01d9\7J\2\2\u01d8\u01d7\3\2\2\2\u01d8\u01d9\3\2\2\2\u01d9\u01da\3\2"+
		"\2\2\u01da\u01dc\7\5\2\2\u01db\u01dd\7J\2\2\u01dc\u01db\3\2\2\2\u01dc"+
		"\u01dd\3\2\2\2\u01dd\u01de\3\2\2\2\u01de\u01e0\5\62\32\2\u01df\u01d8\3"+
		"\2\2\2\u01e0\u01e3\3\2\2\2\u01e1\u01df\3\2\2\2\u01e1\u01e2\3\2\2\2\u01e2"+
		"\65\3\2\2\2\u01e3\u01e1\3\2\2\2\u01e4\u01e5\7]\2\2\u01e5\u01e6\7J\2\2"+
		"\u01e6\u01e8\5\u00c4c\2\u01e7\u01e9\7J\2\2\u01e8\u01e7\3\2\2\2\u01e8\u01e9"+
		"\3\2\2\2\u01e9\u01ea\3\2\2\2\u01ea\u01ec\7\13\2\2\u01eb\u01ed\7J\2\2\u01ec"+
		"\u01eb\3\2\2\2\u01ec\u01ed\3\2\2\2\u01ed\u01ee\3\2\2\2\u01ee\u01f0\5\u0080"+
		"A\2\u01ef\u01f1\7K\2\2\u01f0\u01ef\3\2\2\2\u01f0\u01f1\3\2\2\2\u01f1\67"+
		"\3\2\2\2\u01f2\u01f9\5\66\34\2\u01f3\u01f5\7J\2\2\u01f4\u01f3\3\2\2\2"+
		"\u01f4\u01f5\3\2\2\2\u01f5\u01f6\3\2\2\2\u01f6\u01f8\5\66\34\2\u01f7\u01f4"+
		"\3\2\2\2\u01f8\u01fb\3\2\2\2\u01f9\u01f7\3\2\2\2\u01f9\u01fa\3\2\2\2\u01fa"+
		"9\3\2\2\2\u01fb\u01f9\3\2\2\2\u01fc\u01fd\7]\2\2\u01fd\u01fe\7J\2\2\u01fe"+
		"\u0200\5\u0080A\2\u01ff\u0201\7J\2\2\u0200\u01ff\3\2\2\2\u0200\u0201\3"+
		"\2\2\2\u0201\u0202\3\2\2\2\u0202\u0204\7\13\2\2\u0203\u0205\7J\2\2\u0204"+
		"\u0203\3\2\2\2\u0204\u0205\3\2\2\2\u0205\u0206\3\2\2\2\u0206\u0208\5\u0080"+
		"A\2\u0207\u0209\7K\2\2\u0208\u0207\3\2\2\2\u0208\u0209\3\2\2\2\u0209;"+
		"\3\2\2\2\u020a\u0211\5:\36\2\u020b\u020d\7J\2\2\u020c\u020b\3\2\2\2\u020c"+
		"\u020d\3\2\2\2\u020d\u020e\3\2\2\2\u020e\u0210\5:\36\2\u020f\u020c\3\2"+
		"\2\2\u0210\u0213\3\2\2\2\u0211\u020f\3\2\2\2\u0211\u0212\3\2\2\2\u0212"+
		"=\3\2\2\2\u0213\u0211\3\2\2\2\u0214\u021d\5$\23\2\u0215\u0217\7J\2\2\u0216"+
		"\u0215\3\2\2\2\u0216\u0217\3\2\2\2\u0217\u0218\3\2\2\2\u0218\u021a\7\6"+
		"\2\2\u0219\u021b\7J\2\2\u021a\u0219\3\2\2\2\u021a\u021b\3\2\2\2\u021b"+
		"\u021c\3\2\2\2\u021c\u021e\5\u0108\u0085\2\u021d\u0216\3\2\2\2\u021d\u021e"+
		"\3\2\2\2\u021e?\3\2\2\2\u021f\u0221\7\f\2\2\u0220\u0222\7J\2\2\u0221\u0220"+
		"\3\2\2\2\u0221\u0222\3\2\2\2\u0222\u0223\3\2\2\2\u0223\u022e\5> \2\u0224"+
		"\u0226\7J\2\2\u0225\u0224\3\2\2\2\u0225\u0226\3\2\2\2\u0226\u0227\3\2"+
		"\2\2\u0227\u0229\7\5\2\2\u0228\u022a\7J\2\2\u0229\u0228\3\2\2\2\u0229"+
		"\u022a\3\2\2\2\u022a\u022b\3\2\2\2\u022b\u022d\5> \2\u022c\u0225\3\2\2"+
		"\2\u022d\u0230\3\2\2\2\u022e\u022c\3\2\2\2\u022e\u022f\3\2\2\2\u022f\u0231"+
		"\3\2\2\2\u0230\u022e\3\2\2\2\u0231\u0232\7\r\2\2\u0232\u0234\3\2\2\2\u0233"+
		"\u021f\3\2\2\2\u0233\u0234\3\2\2\2\u0234A\3\2\2\2\u0235\u0236\7\f\2\2"+
		"\u0236\u0241\5\u0108\u0085\2\u0237\u0239\7J\2\2\u0238\u0237\3\2\2\2\u0238"+
		"\u0239\3\2\2\2\u0239\u023a\3\2\2\2\u023a\u023c\7\5\2\2\u023b\u023d\7J"+
		"\2\2\u023c\u023b\3\2\2\2\u023c\u023d\3\2\2\2\u023d\u023e\3\2\2\2\u023e"+
		"\u0240\5\u0108\u0085\2\u023f\u0238\3\2\2\2\u0240\u0243\3\2\2\2\u0241\u023f"+
		"\3\2\2\2\u0241\u0242\3\2\2\2\u0242\u0244\3\2\2\2\u0243\u0241\3\2\2\2\u0244"+
		"\u0245\7\r\2\2\u0245C\3\2\2\2\u0246\u0247\5\24\13\2\u0247\u0248\5B\"\2"+
		"\u0248E\3\2\2\2\u0249\u0254\5D#\2\u024a\u024c\7J\2\2\u024b\u024a\3\2\2"+
		"\2\u024b\u024c\3\2\2\2\u024c\u024d\3\2\2\2\u024d\u024f\7\5\2\2\u024e\u0250"+
		"\7J\2\2\u024f\u024e\3\2\2\2\u024f\u0250\3\2\2\2\u0250\u0251\3\2\2\2\u0251"+
		"\u0253\5D#\2\u0252\u024b\3\2\2\2\u0253\u0256\3\2\2\2\u0254\u0252\3\2\2"+
		"\2\u0254\u0255\3\2\2\2\u0255G\3\2\2\2\u0256\u0254\3\2\2\2\u0257\u0259"+
		"\7J\2\2\u0258\u0257\3\2\2\2\u0258\u0259\3\2\2\2\u0259\u025a\3\2\2\2\u025a"+
		"\u025c\7\16\2\2\u025b\u025d\7J\2\2\u025c\u025b\3\2\2\2\u025c\u025d\3\2"+
		"\2\2\u025d\u025e\3\2\2\2\u025e\u0260\5F$\2\u025f\u0261\7J\2\2\u0260\u025f"+
		"\3\2\2\2\u0260\u0261\3\2\2\2\u0261\u0263\3\2\2\2\u0262\u0258\3\2\2\2\u0262"+
		"\u0263\3\2\2\2\u0263I\3\2\2\2\u0264\u0265\7\17\2\2\u0265\u0266\5\20\t"+
		"\2\u0266K\3\2\2\2\u0267\u026c\5J&\2\u0268\u0269\7J\2\2\u0269\u026b\5J"+
		"&\2\u026a\u0268\3\2\2\2\u026b\u026e\3\2\2\2\u026c\u026a\3\2\2\2\u026c"+
		"\u026d\3\2\2\2\u026dM\3\2\2\2\u026e\u026c\3\2\2\2\u026f\u0271\7J\2\2\u0270"+
		"\u026f\3\2\2\2\u0270\u0271\3\2\2\2\u0271\u0275\3\2\2\2\u0272\u0276\5P"+
		")\2\u0273\u0276\5R*\2\u0274\u0276\5T+\2\u0275\u0272\3\2\2\2\u0275\u0273"+
		"\3\2\2\2\u0275\u0274\3\2\2\2\u0276O\3\2\2\2\u0277\u0278\7`\2\2\u0278\u0279"+
		"\7J\2\2\u0279\u027a\5\n\6\2\u027a\u027b\7\4\2\2\u027b\u027c\7\\\2\2\u027c"+
		"\u027d\5\6\4\2\u027dQ\3\2\2\2\u027e\u027f\7`\2\2\u027f\u0280\7J\2\2\u0280"+
		"\u0281\5\n\6\2\u0281\u0282\7\4\2\2\u0282\u0283\5\b\5\2\u0283\u0284\5\6"+
		"\4\2\u0284S\3\2\2\2\u0285\u0286\7`\2\2\u0286\u0287\7J\2\2\u0287\u0288"+
		"\5\n\6\2\u0288\u0289\5\6\4\2\u0289U\3\2\2\2\u028a\u0298\5X-\2\u028b\u0298"+
		"\5Z.\2\u028c\u0298\5`\61\2\u028d\u0298\5b\62\2\u028e\u0298\5d\63\2\u028f"+
		"\u0298\5f\64\2\u0290\u0298\5h\65\2\u0291\u0298\5j\66\2\u0292\u0298\5l"+
		"\67\2\u0293\u0298\5n8\2\u0294\u0298\5t;\2\u0295\u0298\5v<\2\u0296\u0298"+
		"\5z>\2\u0297\u028a\3\2\2\2\u0297\u028b\3\2\2\2\u0297\u028c\3\2\2\2\u0297"+
		"\u028d\3\2\2\2\u0297\u028e\3\2\2\2\u0297\u028f\3\2\2\2\u0297\u0290\3\2"+
		"\2\2\u0297\u0291\3\2\2\2\u0297\u0292\3\2\2\2\u0297\u0293\3\2\2\2\u0297"+
		"\u0294\3\2\2\2\u0297\u0295\3\2\2\2\u0297\u0296\3\2\2\2\u0298W\3\2\2\2"+
		"\u0299\u029b\7J\2\2\u029a\u0299\3\2\2\2\u029a\u029b\3\2\2\2\u029b\u029c"+
		"\3\2\2\2\u029c\u029d\7N\2\2\u029d\u029e\7J\2\2\u029e\u02a0\5\n\6\2\u029f"+
		"\u02a1\7J\2\2\u02a0\u029f\3\2\2\2\u02a0\u02a1\3\2\2\2\u02a1\u02a2\3\2"+
		"\2\2\u02a2\u02a4\7\t\2\2\u02a3\u02a5\7J\2\2\u02a4\u02a3\3\2\2\2\u02a4"+
		"\u02a5\3\2\2\2\u02a5\u02a9\3\2\2\2\u02a6\u02a8\5V,\2\u02a7\u02a6\3\2\2"+
		"\2\u02a8\u02ab\3\2\2\2\u02a9\u02a7\3\2\2\2\u02a9\u02aa\3\2\2\2\u02aa\u02ad"+
		"\3\2\2\2\u02ab\u02a9\3\2\2\2\u02ac\u02ae\7J\2\2\u02ad\u02ac\3\2\2\2\u02ad"+
		"\u02ae\3\2\2\2\u02ae\u02af\3\2\2\2\u02af\u02b0\7\n\2\2\u02b0\u02b1\5\6"+
		"\4\2\u02b1Y\3\2\2\2\u02b2\u02b4\7J\2\2\u02b3\u02b2\3\2\2\2\u02b3\u02b4"+
		"\3\2\2\2\u02b4\u02b5\3\2\2\2\u02b5\u02b7\5\2\2\2\u02b6\u02b3\3\2\2\2\u02b7"+
		"\u02ba\3\2\2\2\u02b8\u02b6\3\2\2\2\u02b8\u02b9\3\2\2\2\u02b9\u02bc\3\2"+
		"\2\2\u02ba\u02b8\3\2\2\2\u02bb\u02bd\7J\2\2\u02bc\u02bb\3\2\2\2\u02bc"+
		"\u02bd\3\2\2\2\u02bd\u02be\3\2\2\2\u02be\u02bf\7M\2\2\u02bf\u02c0\7J\2"+
		"\2\u02c0\u02c1\5 \21\2\u02c1\u02c3\5@!\2\u02c2\u02c4\7J\2\2\u02c3\u02c2"+
		"\3\2\2\2\u02c3\u02c4\3\2\2\2\u02c4\u02c5\3\2\2\2\u02c5\u02c7\7\t\2\2\u02c6"+
		"\u02c8\7J\2\2\u02c7\u02c6\3\2\2\2\u02c7\u02c8\3\2\2\2\u02c8\u02c9\3\2"+
		"\2\2\u02c9\u02cb\5\\/\2\u02ca\u02cc\7J\2\2\u02cb\u02ca\3\2\2\2\u02cb\u02cc"+
		"\3\2\2\2\u02cc\u02cd\3\2\2\2\u02cd\u02ce\7\n\2\2\u02ce\u02cf\5\6\4\2\u02cf"+
		"[\3\2\2\2\u02d0\u02db\5^\60\2\u02d1\u02d3\7J\2\2\u02d2\u02d1\3\2\2\2\u02d2"+
		"\u02d3\3\2\2\2\u02d3\u02d4\3\2\2\2\u02d4\u02d6\7\5\2\2\u02d5\u02d7\7J"+
		"\2\2\u02d6\u02d5\3\2\2\2\u02d6\u02d7\3\2\2\2\u02d7\u02d8\3\2\2\2\u02d8"+
		"\u02da\5^\60\2\u02d9\u02d2\3\2\2\2\u02da\u02dd\3\2\2\2\u02db\u02d9\3\2"+
		"\2\2\u02db\u02dc\3\2\2\2\u02dc]\3\2\2\2\u02dd\u02db\3\2\2\2\u02de\u02df"+
		"\7]\2\2\u02df\u02e0\7J\2\2\u02e0\u02e2\5\36\20\2\u02e1\u02e3\5\u0112\u008a"+
		"\2\u02e2\u02e1\3\2\2\2\u02e2\u02e3\3\2\2\2\u02e3_\3\2\2\2\u02e4\u02e6"+
		"\7J\2\2\u02e5\u02e4\3\2\2\2\u02e5\u02e6\3\2\2\2\u02e6\u02e7\3\2\2\2\u02e7"+
		"\u02e9\5\2\2\2\u02e8\u02e5\3\2\2\2\u02e9\u02ec\3\2\2\2\u02ea\u02e8\3\2"+
		"\2\2\u02ea\u02eb\3\2\2\2\u02eb\u02ee\3\2\2\2\u02ec\u02ea\3\2\2\2\u02ed"+
		"\u02ef\7J\2\2\u02ee\u02ed\3\2\2\2\u02ee\u02ef\3\2\2\2\u02ef\u02f0\3\2"+
		"\2\2\u02f0\u02f1\7O\2\2\u02f1\u02f2\7J\2\2\u02f2\u02f4\5\32\16\2\u02f3"+
		"\u02f5\7J\2\2\u02f4\u02f3\3\2\2\2\u02f4\u02f5\3\2\2\2\u02f5\u02f6\3\2"+
		"\2\2\u02f6\u02f8\7\7\2\2\u02f7\u02f9\7J\2\2\u02f8\u02f7\3\2\2\2\u02f8"+
		"\u02f9\3\2\2\2\u02f9\u02fb\3\2\2\2\u02fa\u02fc\5\60\31\2\u02fb\u02fa\3"+
		"\2\2\2\u02fb\u02fc\3\2\2\2\u02fc\u02fe\3\2\2\2\u02fd\u02ff\7J\2\2\u02fe"+
		"\u02fd\3\2\2\2\u02fe\u02ff\3\2\2\2\u02ff\u0300\3\2\2\2\u0300\u0301\7\b"+
		"\2\2\u0301\u0302\5\6\4\2\u0302a\3\2\2\2\u0303\u0305\7J\2\2\u0304\u0303"+
		"\3\2\2\2\u0304\u0305\3\2\2\2\u0305\u0306\3\2\2\2\u0306\u0308\5\2\2\2\u0307"+
		"\u0304\3\2\2\2\u0308\u030b\3\2\2\2\u0309\u0307\3\2\2\2\u0309\u030a\3\2"+
		"\2\2\u030a\u030d\3\2\2\2\u030b\u0309\3\2\2\2\u030c\u030e\7J\2\2\u030d"+
		"\u030c\3\2\2\2\u030d\u030e\3\2\2\2\u030e\u030f\3\2\2\2\u030f\u0310\7P"+
		"\2\2\u0310\u0311\7J\2\2\u0311\u0313\5\32\16\2\u0312\u0314\7J\2\2\u0313"+
		"\u0312\3\2\2\2\u0313\u0314\3\2\2\2\u0314\u0315\3\2\2\2\u0315\u0317\7\7"+
		"\2\2\u0316\u0318\7J\2\2\u0317\u0316\3\2\2\2\u0317\u0318\3\2\2\2\u0318"+
		"\u031a\3\2\2\2\u0319\u031b\5\60\31\2\u031a\u0319\3\2\2\2\u031a\u031b\3"+
		"\2\2\2\u031b\u031d\3\2\2\2\u031c\u031e\7J\2\2\u031d\u031c\3\2\2\2\u031d"+
		"\u031e\3\2\2\2\u031e\u031f\3\2\2\2\u031f\u0320\7\b\2\2\u0320\u0321\5\6"+
		"\4\2\u0321c\3\2\2\2\u0322\u0324\7J\2\2\u0323\u0322\3\2\2\2\u0323\u0324"+
		"\3\2\2\2\u0324\u0325\3\2\2\2\u0325\u0326\7Q\2\2\u0326\u0327\7J\2\2\u0327"+
		"\u0329\5\34\17\2\u0328\u032a\7J\2\2\u0329\u0328\3\2\2\2\u0329\u032a\3"+
		"\2\2\2\u032a\u032b\3\2\2\2\u032b\u032d\7\7\2\2\u032c\u032e\7J\2\2\u032d"+
		"\u032c\3\2\2\2\u032d\u032e\3\2\2\2\u032e\u0330\3\2\2\2\u032f\u0331\5\64"+
		"\33\2\u0330\u032f\3\2\2\2\u0330\u0331\3\2\2\2\u0331\u0333\3\2\2\2\u0332"+
		"\u0334\7J\2\2\u0333\u0332\3\2\2\2\u0333\u0334\3\2\2\2\u0334\u0335\3\2"+
		"\2\2\u0335\u0336\7\b\2\2\u0336\u0337\5\6\4\2\u0337e\3\2\2\2\u0338\u033a"+
		"\7J\2\2\u0339\u0338\3\2\2\2\u0339\u033a\3\2\2\2\u033a\u033b\3\2\2\2\u033b"+
		"\u033d\5\2\2\2\u033c\u0339\3\2\2\2\u033d\u0340\3\2\2\2\u033e\u033c\3\2"+
		"\2\2\u033e\u033f\3\2\2\2\u033f\u0342\3\2\2\2\u0340\u033e\3\2\2\2\u0341"+
		"\u0343\7J\2\2\u0342\u0341\3\2\2\2\u0342\u0343\3\2\2\2\u0343\u0344\3\2"+
		"\2\2\u0344\u0345\7R\2\2\u0345\u0346\7J\2\2\u0346\u0348\5\26\f\2\u0347"+
		"\u0349\7J\2\2\u0348\u0347\3\2\2\2\u0348\u0349\3\2\2\2\u0349\u034a\3\2"+
		"\2\2\u034a\u034c\5,\27\2\u034b\u034d\7J\2\2\u034c\u034b\3\2\2\2\u034c"+
		"\u034d\3\2\2\2\u034d\u034e\3\2\2\2\u034e\u0350\7\6\2\2\u034f\u0351\7J"+
		"\2\2\u0350\u034f\3\2\2\2\u0350\u0351\3\2\2\2\u0351\u0352\3\2\2\2\u0352"+
		"\u0353\5\u0108\u0085\2\u0353\u0354\5\6\4\2\u0354g\3\2\2\2\u0355\u0357"+
		"\7J\2\2\u0356\u0355\3\2\2\2\u0356\u0357\3\2\2\2\u0357\u0358\3\2\2\2\u0358"+
		"\u035a\5\2\2\2\u0359\u0356\3\2\2\2\u035a\u035d\3\2\2\2\u035b\u0359\3\2"+
		"\2\2\u035b\u035c\3\2\2\2\u035c\u035f\3\2\2\2\u035d\u035b\3\2\2\2\u035e"+
		"\u0360\7J\2\2\u035f\u035e\3\2\2\2\u035f\u0360\3\2\2\2\u0360\u0361\3\2"+
		"\2\2\u0361\u0363\7S\2\2\u0362\u0364\7J\2\2\u0363\u0362\3\2\2\2\u0363\u0364"+
		"\3\2\2\2\u0364\u0365\3\2\2\2\u0365\u0366\7R\2\2\u0366\u0367\7J\2\2\u0367"+
		"\u0369\5\26\f\2\u0368\u036a\7J\2\2\u0369\u0368\3\2\2\2\u0369\u036a\3\2"+
		"\2\2\u036a\u036b\3\2\2\2\u036b\u036d\5,\27\2\u036c\u036e\7J\2\2\u036d"+
		"\u036c\3\2\2\2\u036d\u036e\3\2\2\2\u036e\u036f\3\2\2\2\u036f\u0371\7\6"+
		"\2\2\u0370\u0372\7J\2\2\u0371\u0370\3\2\2\2\u0371\u0372\3\2\2\2\u0372"+
		"\u0373\3\2\2\2\u0373\u0374\5\u0108\u0085\2\u0374\u0375\5\6\4\2\u0375i"+
		"\3\2\2\2\u0376\u0378\7J\2\2\u0377\u0376\3\2\2\2\u0377\u0378\3\2\2\2\u0378"+
		"\u0379\3\2\2\2\u0379\u037b\5\2\2\2\u037a\u0377\3\2\2\2\u037b\u037e\3\2"+
		"\2\2\u037c\u037a\3\2\2\2\u037c\u037d\3\2\2\2\u037d\u0380\3\2\2\2\u037e"+
		"\u037c\3\2\2\2\u037f\u0381\7J\2\2\u0380\u037f\3\2\2\2\u0380\u0381\3\2"+
		"\2\2\u0381\u0383\3\2\2\2\u0382\u0384\5L\'\2\u0383\u0382\3\2\2\2\u0383"+
		"\u0384\3\2\2\2\u0384\u0386\3\2\2\2\u0385\u0387\7J\2\2\u0386\u0385\3\2"+
		"\2\2\u0386\u0387\3\2\2\2\u0387\u0388\3\2\2\2\u0388\u0389\7R\2\2\u0389"+
		"\u038a\7J\2\2\u038a\u038c\5\26\f\2\u038b\u038d\7J\2\2\u038c\u038b\3\2"+
		"\2\2\u038c\u038d\3\2\2\2\u038d\u038e\3\2\2\2\u038e\u038f\5@!\2\u038f\u0391"+
		"\5,\27\2\u0390\u0392\7J\2\2\u0391\u0390\3\2\2\2\u0391\u0392\3\2\2\2\u0392"+
		"\u0393\3\2\2\2\u0393\u0395\7\6\2\2\u0394\u0396\7J\2\2\u0395\u0394\3\2"+
		"\2\2\u0395\u0396\3\2\2\2\u0396\u0397\3\2\2\2\u0397\u0399\5\u0108\u0085"+
		"\2\u0398\u039a\7J\2\2\u0399\u0398\3\2\2\2\u0399\u039a\3\2\2\2\u039a\u039b"+
		"\3\2\2\2\u039b\u039d\7\20\2\2\u039c\u039e\7J\2\2\u039d\u039c\3\2\2\2\u039d"+
		"\u039e\3\2\2\2\u039e\u039f\3\2\2\2\u039f\u03a0\5\u0080A\2\u03a0\u03a1"+
		"\5\6\4\2\u03a1k\3\2\2\2\u03a2\u03a4\7J\2\2\u03a3\u03a2\3\2\2\2\u03a3\u03a4"+
		"\3\2\2\2\u03a4\u03a5\3\2\2\2\u03a5\u03a7\5\2\2\2\u03a6\u03a3\3\2\2\2\u03a7"+
		"\u03aa\3\2\2\2\u03a8\u03a6\3\2\2\2\u03a8\u03a9\3\2\2\2\u03a9\u03ac\3\2"+
		"\2\2\u03aa\u03a8\3\2\2\2\u03ab\u03ad\7J\2\2\u03ac\u03ab\3\2\2\2\u03ac"+
		"\u03ad\3\2\2\2\u03ad\u03ae\3\2\2\2\u03ae\u03af\7T\2\2\u03af\u03b0\7J\2"+
		"\2\u03b0\u03b2\5\26\f\2\u03b1\u03b3\7J\2\2\u03b2\u03b1\3\2\2\2\u03b2\u03b3"+
		"\3\2\2\2\u03b3\u03b4\3\2\2\2\u03b4\u03b6\5@!\2\u03b5\u03b7\7J\2\2\u03b6"+
		"\u03b5\3\2\2\2\u03b6\u03b7\3\2\2\2\u03b7\u03b8\3\2\2\2\u03b8\u03ba\5,"+
		"\27\2\u03b9\u03bb\7J\2\2\u03ba\u03b9\3\2\2\2\u03ba\u03bb\3\2\2\2\u03bb"+
		"\u03bc\3\2\2\2\u03bc\u03be\7\6\2\2\u03bd\u03bf\7J\2\2\u03be\u03bd\3\2"+
		"\2\2\u03be\u03bf\3\2\2\2\u03bf\u03c0\3\2\2\2\u03c0\u03c2\5\u0108\u0085"+
		"\2\u03c1\u03c3\7J\2\2\u03c2\u03c1\3\2\2\2\u03c2\u03c3\3\2\2\2\u03c3\u03c4"+
		"\3\2\2\2\u03c4\u03c6\7\20\2\2\u03c5\u03c7\7J\2\2\u03c6\u03c5\3\2\2\2\u03c6"+
		"\u03c7\3\2\2\2\u03c7\u03c8\3\2\2\2\u03c8\u03c9\5\u0080A\2\u03c9\u03ca"+
		"\5\6\4\2\u03cam\3\2\2\2\u03cb\u03cd\7J\2\2\u03cc\u03cb\3\2\2\2\u03cc\u03cd"+
		"\3\2\2\2\u03cd\u03ce\3\2\2\2\u03ce\u03d0\5\2\2\2\u03cf\u03cc\3\2\2\2\u03d0"+
		"\u03d3\3\2\2\2\u03d1\u03cf\3\2\2\2\u03d1\u03d2\3\2\2\2\u03d2\u03d5\3\2"+
		"\2\2\u03d3\u03d1\3\2\2\2\u03d4\u03d6\7J\2\2\u03d5\u03d4\3\2\2\2\u03d5"+
		"\u03d6\3\2\2\2\u03d6\u03d7\3\2\2\2\u03d7\u03d8\7U\2\2\u03d8\u03d9\7J\2"+
		"\2\u03d9\u03da\5\24\13\2\u03da\u03dc\5B\"\2\u03db\u03dd\7J\2\2\u03dc\u03db"+
		"\3\2\2\2\u03dc\u03dd\3\2\2\2\u03dd\u03de\3\2\2\2\u03de\u03e0\5H%\2\u03df"+
		"\u03e1\7J\2\2\u03e0\u03df\3\2\2\2\u03e0\u03e1\3\2\2\2\u03e1\u03e2\3\2"+
		"\2\2\u03e2\u03e3\5p9\2\u03e3o\3\2\2\2\u03e4\u03e6\7\t\2\2\u03e5\u03e7"+
		"\7J\2\2\u03e6\u03e5\3\2\2\2\u03e6\u03e7\3\2\2\2\u03e7\u03ee\3\2\2\2\u03e8"+
		"\u03ea\5r:\2\u03e9\u03eb\7J\2\2\u03ea\u03e9\3\2\2\2\u03ea\u03eb\3\2\2"+
		"\2\u03eb\u03ed\3\2\2\2\u03ec\u03e8\3\2\2\2\u03ed\u03f0\3\2\2\2\u03ee\u03ec"+
		"\3\2\2\2\u03ee\u03ef\3\2\2\2\u03ef\u03f1\3\2\2\2\u03f0\u03ee\3\2\2\2\u03f1"+
		"\u03f2\7\n\2\2\u03f2q\3\2\2\2\u03f3\u03f7\5j\66\2\u03f4\u03f7\5f\64\2"+
		"\u03f5\u03f7\5l\67\2\u03f6\u03f3\3\2\2\2\u03f6\u03f4\3\2\2\2\u03f6\u03f5"+
		"\3\2\2\2\u03f7s\3\2\2\2\u03f8\u03fa\7J\2\2\u03f9\u03f8\3\2\2\2\u03f9\u03fa"+
		"\3\2\2\2\u03fa\u03fb\3\2\2\2\u03fb\u03fd\5\u0122\u0092\2\u03fc\u03fe\7"+
		"J\2\2\u03fd\u03fc\3\2\2\2\u03fd\u03fe\3\2\2\2\u03fe\u03ff\3\2\2\2\u03ff"+
		"\u0400\7\3\2\2\u0400u\3\2\2\2\u0401\u0403\7J\2\2\u0402\u0401\3\2\2\2\u0402"+
		"\u0403\3\2\2\2\u0403\u0404\3\2\2\2\u0404\u0406\5\u0122\u0092\2\u0405\u0407"+
		"\7J\2\2\u0406\u0405\3\2\2\2\u0406\u0407\3\2\2\2\u0407\u0408\3\2\2\2\u0408"+
		"\u040a\7\21\2\2\u0409\u040b\7J\2\2\u040a\u0409\3\2\2\2\u040a\u040b\3\2"+
		"\2\2\u040b\u040c\3\2\2\2\u040c\u040e\5\u0124\u0093\2\u040d\u040f\7J\2"+
		"\2\u040e\u040d\3\2\2\2\u040e\u040f\3\2\2\2\u040f\u0410\3\2\2\2\u0410\u0411"+
		"\7\3\2\2\u0411w\3\2\2\2\u0412\u0413\5\u0084C\2\u0413y\3\2\2\2\u0414\u0416"+
		"\7J\2\2\u0415\u0414\3\2\2\2\u0415\u0416\3\2\2\2\u0416\u0417\3\2\2\2\u0417"+
		"\u0419\7V\2\2\u0418\u041a\7J\2\2\u0419\u0418\3\2\2\2\u0419\u041a\3\2\2"+
		"\2\u041a\u041b\3\2\2\2\u041b\u041c\5\u0108\u0085\2\u041c\u041e\7\22\2"+
		"\2\u041d\u041f\7J\2\2\u041e\u041d\3\2\2\2\u041e\u041f\3\2\2\2\u041f\u0420"+
		"\3\2\2\2\u0420\u0422\7\20\2\2\u0421\u0423\7J\2\2\u0422\u0421\3\2\2\2\u0422"+
		"\u0423\3\2\2\2\u0423\u0424\3\2\2\2\u0424\u0426\7\7\2\2\u0425\u0427\7J"+
		"\2\2\u0426\u0425\3\2\2\2\u0426\u0427\3\2\2\2\u0427\u0428\3\2\2\2\u0428"+
		"\u042a\5x=\2\u0429\u042b\7J\2\2\u042a\u0429\3\2\2\2\u042a\u042b\3\2\2"+
		"\2\u042b\u042c\3\2\2\2\u042c\u042d\7\b\2\2\u042d\u042e\5\6\4\2\u042e{"+
		"\3\2\2\2\u042f\u0431\7J\2\2\u0430\u042f\3\2\2\2\u0430\u0431\3\2\2\2\u0431"+
		"\u0432\3\2\2\2\u0432\u0434\5\2\2\2\u0433\u0430\3\2\2\2\u0434\u0437\3\2"+
		"\2\2\u0435\u0433\3\2\2\2\u0435\u0436\3\2\2\2\u0436\u0439\3\2\2\2\u0437"+
		"\u0435\3\2\2\2\u0438\u043a\7J\2\2\u0439\u0438\3\2\2\2\u0439\u043a\3\2"+
		"\2\2\u043a\u043b\3\2\2\2\u043b\u043c\7W\2\2\u043c\u043d\7J\2\2\u043d\u043e"+
		"\5\24\13\2\u043e\u0440\5B\"\2\u043f\u0441\7J\2\2\u0440\u043f\3\2\2\2\u0440"+
		"\u0441\3\2\2\2\u0441\u0442\3\2\2\2\u0442\u0444\5H%\2\u0443\u0445\7J\2"+
		"\2\u0444\u0443\3\2\2\2\u0444\u0445\3\2\2\2\u0445\u0446\3\2\2\2\u0446\u0447"+
		"\5~@\2\u0447}\3\2\2\2\u0448\u044a\7\t\2\2\u0449\u044b\7J\2\2\u044a\u0449"+
		"\3\2\2\2\u044a\u044b\3\2\2\2\u044b\u044f\3\2\2\2\u044c\u044e\5j\66\2\u044d"+
		"\u044c\3\2\2\2\u044e\u0451\3\2\2\2\u044f\u044d\3\2\2\2\u044f\u0450\3\2"+
		"\2\2\u0450\u0453\3\2\2\2\u0451\u044f\3\2\2\2\u0452\u0454\7J\2\2\u0453"+
		"\u0452\3\2\2\2\u0453\u0454\3\2\2\2\u0454\u0455\3\2\2\2\u0455\u0456\7\n"+
		"\2\2\u0456\177\3\2\2\2\u0457\u0459\7\t\2\2\u0458\u045a\7J\2\2\u0459\u0458"+
		"\3\2\2\2\u0459\u045a\3\2\2\2\u045a\u045b\3\2\2\2\u045b\u045d\5\u0080A"+
		"\2\u045c\u045e\7J\2\2\u045d\u045c\3\2\2\2\u045d\u045e\3\2\2\2\u045e\u045f"+
		"\3\2\2\2\u045f\u0461\7\n\2\2\u0460\u0462\7J\2\2\u0461\u0460\3\2\2\2\u0461"+
		"\u0462\3\2\2\2\u0462\u0465\3\2\2\2\u0463\u0465\5\u0082B\2\u0464\u0457"+
		"\3\2\2\2\u0464\u0463\3\2\2\2\u0465\u0081\3\2\2\2\u0466\u0470\5\u0086D"+
		"\2\u0467\u0469\7J\2\2\u0468\u0467\3\2\2\2\u0468\u0469\3\2\2\2\u0469\u046a"+
		"\3\2\2\2\u046a\u046c\5\u0118\u008d\2\u046b\u046d\7J\2\2\u046c\u046b\3"+
		"\2\2\2\u046c\u046d\3\2\2\2\u046d\u046e\3\2\2\2\u046e\u046f\5\u0086D\2"+
		"\u046f\u0471\3\2\2\2\u0470\u0468\3\2\2\2\u0470\u0471\3\2\2\2\u0471\u0083"+
		"\3\2\2\2\u0472\u047d\5\u0080A\2\u0473\u0475\7J\2\2\u0474\u0473\3\2\2\2"+
		"\u0474\u0475\3\2\2\2\u0475\u0476\3\2\2\2\u0476\u0478\7\5\2\2\u0477\u0479"+
		"\7J\2\2\u0478\u0477\3\2\2\2\u0478\u0479\3\2\2\2\u0479\u047a\3\2\2\2\u047a"+
		"\u047c\5\u0080A\2\u047b\u0474\3\2\2\2\u047c\u047f\3\2\2\2\u047d\u047b"+
		"\3\2\2\2\u047d\u047e\3\2\2\2\u047e\u0085\3\2\2\2\u047f\u047d\3\2\2\2\u0480"+
		"\u048a\5\u0088E\2\u0481\u0483\7J\2\2\u0482\u0481\3\2\2\2\u0482\u0483\3"+
		"\2\2\2\u0483\u0484\3\2\2\2\u0484\u0486\5\u011a\u008e\2\u0485\u0487\7J"+
		"\2\2\u0486\u0485\3\2\2\2\u0486\u0487\3\2\2\2\u0487\u0488\3\2\2\2\u0488"+
		"\u0489\5\u0088E\2\u0489\u048b\3\2\2\2\u048a\u0482\3\2\2\2\u048a\u048b"+
		"\3\2\2\2\u048b\u0087\3\2\2\2\u048c\u0496\5\u008aF\2\u048d\u048f\7J\2\2"+
		"\u048e\u048d\3\2\2\2\u048e\u048f\3\2\2\2\u048f\u0490\3\2\2\2\u0490\u0492"+
		"\5\u011e\u0090\2\u0491\u0493\7J\2\2\u0492\u0491\3\2\2\2\u0492\u0493\3"+
		"\2\2\2\u0493\u0494\3\2\2\2\u0494\u0495\5\u0088E\2\u0495\u0497\3\2\2\2"+
		"\u0496\u048e\3\2\2\2\u0496\u0497\3\2\2\2\u0497\u0089\3\2\2\2\u0498\u04a2"+
		"\5\u008cG\2\u0499\u049b\7J\2\2\u049a\u0499\3\2\2\2\u049a\u049b\3\2\2\2"+
		"\u049b\u049c\3\2\2\2\u049c\u049e\5\u011c\u008f\2\u049d\u049f\7J\2\2\u049e"+
		"\u049d\3\2\2\2\u049e\u049f\3\2\2\2\u049f\u04a0\3\2\2\2\u04a0\u04a1\5\u008a"+
		"F\2\u04a1\u04a3\3\2\2\2\u04a2\u049a\3\2\2\2\u04a2\u04a3\3\2\2\2\u04a3"+
		"\u008b\3\2\2\2\u04a4\u04b0\5\u008eH\2\u04a5\u04a7\7J\2\2\u04a6\u04a5\3"+
		"\2\2\2\u04a6\u04a7\3\2\2\2\u04a7\u04a8\3\2\2\2\u04a8\u04a9\7\23\2\2\u04a9"+
		"\u04aa\5\30\r\2\u04aa\u04ac\7\23\2\2\u04ab\u04ad\7J\2\2\u04ac\u04ab\3"+
		"\2\2\2\u04ac\u04ad\3\2\2\2\u04ad\u04ae\3\2\2\2\u04ae\u04af\5\u008eH\2"+
		"\u04af\u04b1\3\2\2\2\u04b0\u04a6\3\2\2\2\u04b0\u04b1\3\2\2\2\u04b1\u008d"+
		"\3\2\2\2\u04b2\u04bc\5\u0090I\2\u04b3\u04b5\7J\2\2\u04b4\u04b3\3\2\2\2"+
		"\u04b4\u04b5\3\2\2\2\u04b5\u04b6\3\2\2\2\u04b6\u04b8\5\u0120\u0091\2\u04b7"+
		"\u04b9\7J\2\2\u04b8\u04b7\3\2\2\2\u04b8\u04b9\3\2\2\2\u04b9\u04ba\3\2"+
		"\2\2\u04ba\u04bb\5\u0090I\2\u04bb\u04bd\3\2\2\2\u04bc\u04b4\3\2\2\2\u04bc"+
		"\u04bd\3\2\2\2\u04bd\u008f\3\2\2\2\u04be\u04c0\5\u0116\u008c\2\u04bf\u04c1"+
		"\7J\2\2\u04c0\u04bf\3\2\2\2\u04c0\u04c1\3\2\2\2\u04c1\u04c2\3\2\2\2\u04c2"+
		"\u04c3\5\u0090I\2\u04c3\u04c6\3\2\2\2\u04c4\u04c6\5\u0092J\2\u04c5\u04be"+
		"\3\2\2\2\u04c5\u04c4\3\2\2\2\u04c6\u0091\3\2\2\2\u04c7\u04d0\5\u00b4["+
		"\2\u04c8\u04ca\7J\2\2\u04c9\u04c8\3\2\2\2\u04c9\u04ca\3\2\2\2\u04ca\u04cb"+
		"\3\2\2\2\u04cb\u04cd\7\6\2\2\u04cc\u04ce\7J\2\2\u04cd\u04cc\3\2\2\2\u04cd"+
		"\u04ce\3\2\2\2\u04ce\u04cf\3\2\2\2\u04cf\u04d1\5\u0108\u0085\2\u04d0\u04c9"+
		"\3\2\2\2\u04d0\u04d1\3\2\2\2\u04d1\u0093\3\2\2\2\u04d2\u04e6\5\u0096L"+
		"\2\u04d3\u04e6\5\u0098M\2\u04d4\u04e6\5\u009aN\2\u04d5\u04e6\5\u009cO"+
		"\2\u04d6\u04e6\5\u00a6T\2\u04d7\u04e6\5\u00be`\2\u04d8\u04e6\5\u00a8U"+
		"\2\u04d9\u04e6\5\u00b2Z\2\u04da\u04e6\5\u00b6\\\2\u04db\u04e6\5\u00b8"+
		"]\2\u04dc\u04e6\5\u00ba^\2\u04dd\u04e6\5\u00a0Q\2\u04de\u04e6\5\u00c0"+
		"a\2\u04df\u04e6\5\u00c2b\2\u04e0\u04e6\5\u00a4S\2\u04e1\u04e6\5\u00bc"+
		"_\2\u04e2\u04e6\5\u00b0Y\2\u04e3\u04e6\5\u00a2R\2\u04e4\u04e6\5\u00ae"+
		"X\2\u04e5\u04d2\3\2\2\2\u04e5\u04d3\3\2\2\2\u04e5\u04d4\3\2\2\2\u04e5"+
		"\u04d5\3\2\2\2\u04e5\u04d6\3\2\2\2\u04e5\u04d7\3\2\2\2\u04e5\u04d8\3\2"+
		"\2\2\u04e5\u04d9\3\2\2\2\u04e5\u04da\3\2\2\2\u04e5\u04db\3\2\2\2\u04e5"+
		"\u04dc\3\2\2\2\u04e5\u04dd\3\2\2\2\u04e5\u04de\3\2\2\2\u04e5\u04df\3\2"+
		"\2\2\u04e5\u04e0\3\2\2\2\u04e5\u04e1\3\2\2\2\u04e5\u04e2\3\2\2\2\u04e5"+
		"\u04e3\3\2\2\2\u04e5\u04e4\3\2\2\2\u04e6\u0095\3\2\2\2\u04e7\u04e8\7V"+
		"\2\2\u04e8\u04e9\7J\2\2\u04e9\u04eb\5\u00c4c\2\u04ea\u04ec\7J\2\2\u04eb"+
		"\u04ea\3\2\2\2\u04eb\u04ec\3\2\2\2\u04ec\u04ed\3\2\2\2\u04ed\u04ef\7\20"+
		"\2\2\u04ee\u04f0\7J\2\2\u04ef\u04ee\3\2\2\2\u04ef\u04f0\3\2\2\2\u04f0"+
		"\u04f1\3\2\2\2\u04f1\u04f3\5\u0080A\2\u04f2\u04f4\7J\2\2\u04f3\u04f2\3"+
		"\2\2\2\u04f3\u04f4\3\2\2\2\u04f4\u04f5\3\2\2\2\u04f5\u04f7\7K\2\2\u04f6"+
		"\u04f8\7J\2\2\u04f7\u04f6\3\2\2\2\u04f7\u04f8\3\2\2\2\u04f8\u04f9\3\2"+
		"\2\2\u04f9\u04fa\5\u0080A\2\u04fa\u0097\3\2\2\2\u04fb\u04fd\7^\2\2\u04fc"+
		"\u04fe\7J\2\2\u04fd\u04fc\3\2\2\2\u04fd\u04fe\3\2\2\2\u04fe\u04ff\3\2"+
		"\2\2\u04ff\u0501\7\7\2\2\u0500\u0502\7J\2\2\u0501\u0500\3\2\2\2\u0501"+
		"\u0502\3\2\2\2\u0502\u0503\3\2\2\2\u0503\u0505\5\u0080A\2\u0504\u0506"+
		"\7J\2\2\u0505\u0504\3\2\2\2\u0505\u0506\3\2\2\2\u0506\u0507\3\2\2\2\u0507"+
		"\u0509\7\b\2\2\u0508\u050a\7J\2\2\u0509\u0508\3\2\2\2\u0509\u050a\3\2"+
		"\2\2\u050a\u050b\3\2\2\2\u050b\u050c\5\u0080A\2\u050c\u050d\7J\2\2\u050d"+
		"\u050e\7_\2\2\u050e\u050f\7J\2\2\u050f\u0510\5\u0080A\2\u0510\u0099\3"+
		"\2\2\2\u0511\u0512\7Z\2\2\u0512\u0513\7J\2\2\u0513\u0514\5\u0080A\2\u0514"+
		"\u0515\7J\2\2\u0515\u0516\7[\2\2\u0516\u0517\7J\2\2\u0517\u0519\7\t\2"+
		"\2\u0518\u051a\7J\2\2\u0519\u0518\3\2\2\2\u0519\u051a\3\2\2\2\u051a\u051b"+
		"\3\2\2\2\u051b\u051d\58\35\2\u051c\u051e\7J\2\2\u051d\u051c\3\2\2\2\u051d"+
		"\u051e\3\2\2\2\u051e\u051f\3\2\2\2\u051f\u0520\7\n\2\2\u0520\u009b\3\2"+
		"\2\2\u0521\u0522\7Y\2\2\u0522\u0523\7J\2\2\u0523\u0525\7\t\2\2\u0524\u0526"+
		"\7J\2\2\u0525\u0524\3\2\2\2\u0525\u0526\3\2\2\2\u0526\u0527\3\2\2\2\u0527"+
		"\u0529\5<\37\2\u0528\u052a\7J\2\2\u0529\u0528\3\2\2\2\u0529\u052a\3\2"+
		"\2\2\u052a\u052b\3\2\2\2\u052b\u052c\7\n\2\2\u052c\u009d\3\2\2\2\u052d"+
		"\u053c\5\u0094K\2\u052e\u0530\7J\2\2\u052f\u052e\3\2\2\2\u052f\u0530\3"+
		"\2\2\2\u0530\u0531\3\2\2\2\u0531\u0533\7\7\2\2\u0532\u0534\7J\2\2\u0533"+
		"\u0532\3\2\2\2\u0533\u0534\3\2\2\2\u0534\u0536\3\2\2\2\u0535\u0537\5\u0084"+
		"C\2\u0536\u0535\3\2\2\2\u0536\u0537\3\2\2\2\u0537\u0539\3\2\2\2\u0538"+
		"\u053a\7J\2\2\u0539\u0538\3\2\2\2\u0539\u053a\3\2\2\2\u053a\u053b\3\2"+
		"\2\2\u053b\u053d\7\b\2\2\u053c\u052f\3\2\2\2\u053c\u053d\3\2\2\2\u053d"+
		"\u009f\3\2\2\2\u053e\u053f\5\u0100\u0081\2\u053f\u00a1\3\2\2\2\u0540\u0541"+
		"\5$\23\2\u0541\u00a3\3\2\2\2\u0542\u0543\5\30\r\2\u0543\u00a5\3\2\2\2"+
		"\u0544\u0545\5\"\22\2\u0545\u0546\7\3\2\2\u0546\u0548\3\2\2\2\u0547\u0544"+
		"\3\2\2\2\u0547\u0548\3\2\2\2\u0548\u0549\3\2\2\2\u0549\u054e\5\36\20\2"+
		"\u054a\u054c\7J\2\2\u054b\u054a\3\2\2\2\u054b\u054c\3\2\2\2\u054c\u054d"+
		"\3\2\2\2\u054d\u054f\5\u00a8U\2\u054e\u054b\3\2\2\2\u054e\u054f\3\2\2"+
		"\2\u054f\u00a7\3\2\2\2\u0550\u0552\7\7\2\2\u0551\u0553\7J\2\2\u0552\u0551"+
		"\3\2\2\2\u0552\u0553\3\2\2\2\u0553\u0555\3\2\2\2\u0554\u0556\5\u0084C"+
		"\2\u0555\u0554\3\2\2\2\u0555\u0556\3\2\2\2\u0556\u0558\3\2\2\2\u0557\u0559"+
		"\7J\2\2\u0558\u0557\3\2\2\2\u0558\u0559\3\2\2\2\u0559\u055a\3\2\2\2\u055a"+
		"\u055b\7\b\2\2\u055b\u00a9\3\2\2\2\u055c\u055e\5\u0080A\2\u055d\u055f"+
		"\7J\2\2\u055e\u055d\3\2\2\2\u055e\u055f\3\2\2\2\u055f\u0560\3\2\2\2\u0560"+
		"\u0562\7\24\2\2\u0561\u0563\7J\2\2\u0562\u0561\3\2\2\2\u0562\u0563\3\2"+
		"\2\2\u0563\u0564\3\2\2\2\u0564\u0565\5\u0080A\2\u0565\u00ab\3\2\2\2\u0566"+
		"\u0571\5\u00aaV\2\u0567\u0569\7J\2\2\u0568\u0567\3\2\2\2\u0568\u0569\3"+
		"\2\2\2\u0569\u056a\3\2\2\2\u056a\u056c\7\5\2\2\u056b\u056d\7J\2\2\u056c"+
		"\u056b\3\2\2\2\u056c\u056d\3\2\2\2\u056d\u056e\3\2\2\2\u056e\u0570\5\u00aa"+
		"V\2\u056f\u0568\3\2\2\2\u0570\u0573\3\2\2\2\u0571\u056f\3\2\2\2\u0571"+
		"\u0572\3\2\2\2\u0572\u00ad\3\2\2\2\u0573\u0571\3\2\2\2\u0574\u0575\7\25"+
		"\2\2\u0575\u00af\3\2\2\2\u0576\u0577\7\\\2\2\u0577\u00b1\3\2\2\2\u0578"+
		"\u0579\7X\2\2\u0579\u00b3\3\2\2\2\u057a\u0583\5\u009eP\2\u057b\u057d\7"+
		"J\2\2\u057c\u057b\3\2\2\2\u057c\u057d\3\2\2\2\u057d\u057e\3\2\2\2\u057e"+
		"\u0580\7\26\2\2\u057f\u0581\7J\2\2\u0580\u057f\3\2\2\2\u0580\u0581\3\2"+
		"\2\2\u0581\u0582\3\2\2\2\u0582\u0584\5\u0080A\2\u0583\u057c\3\2\2\2\u0583"+
		"\u0584\3\2\2\2\u0584\u00b5\3\2\2\2\u0585\u0587\7\27\2\2\u0586\u0588\7"+
		"J\2\2\u0587\u0586\3\2\2\2\u0587\u0588\3\2\2\2\u0588\u058a\3\2\2\2\u0589"+
		"\u058b\5\u0084C\2\u058a\u0589\3\2\2\2\u058a\u058b\3\2\2\2\u058b\u058d"+
		"\3\2\2\2\u058c\u058e\7J\2\2\u058d\u058c\3\2\2\2\u058d\u058e\3\2\2\2\u058e"+
		"\u058f\3\2\2\2\u058f\u0590\7\r\2\2\u0590\u00b7\3\2\2\2\u0591\u0593\7\30"+
		"\2\2\u0592\u0594\7J\2\2\u0593\u0592\3\2\2\2\u0593\u0594\3\2\2\2\u0594"+
		"\u0596\3\2\2\2\u0595\u0597\5\u0084C\2\u0596\u0595\3\2\2\2\u0596\u0597"+
		"\3\2\2\2\u0597\u0599\3\2\2\2\u0598\u059a\7J\2\2\u0599\u0598\3\2\2\2\u0599"+
		"\u059a\3\2\2\2\u059a\u059b\3\2\2\2\u059b\u059c\7\n\2\2\u059c\u00b9\3\2"+
		"\2\2\u059d\u059f\7\31\2\2\u059e\u05a0\7J\2\2\u059f\u059e\3\2\2\2\u059f"+
		"\u05a0\3\2\2\2\u05a0\u05a2\3\2\2\2\u05a1\u05a3\5\u00acW\2\u05a2\u05a1"+
		"\3\2\2\2\u05a2\u05a3\3\2\2\2\u05a3\u05a5\3\2\2\2\u05a4\u05a6\7J\2\2\u05a5"+
		"\u05a4\3\2\2\2\u05a5\u05a6\3\2\2\2\u05a6\u05a7\3\2\2\2\u05a7\u05a8\7\n"+
		"\2\2\u05a8\u00bb\3\2\2\2\u05a9\u05ab\5$\23\2\u05aa\u05ac\7J\2\2\u05ab"+
		"\u05aa\3\2\2\2\u05ab\u05ac\3\2\2\2\u05ac\u05ad\3\2\2\2\u05ad\u05af\7\24"+
		"\2\2\u05ae\u05b0\7J\2\2\u05af\u05ae\3\2\2\2\u05af\u05b0\3\2\2\2\u05b0"+
		"\u05b1\3\2\2\2\u05b1\u05b2\5\u0080A\2\u05b2\u00bd\3\2\2\2\u05b3\u05b5"+
		"\7\7\2\2\u05b4\u05b6\7J\2\2\u05b5\u05b4\3\2\2\2\u05b5\u05b6\3\2\2\2\u05b6"+
		"\u05b7\3\2\2\2\u05b7\u05b9\5&\24\2\u05b8\u05ba\7J\2\2\u05b9\u05b8\3\2"+
		"\2\2\u05b9\u05ba\3\2\2\2\u05ba\u05bb\3\2\2\2\u05bb\u05bd\7\b\2\2\u05bc"+
		"\u05be\7J\2\2\u05bd\u05bc\3\2\2\2\u05bd\u05be\3\2\2\2\u05be\u05bf\3\2"+
		"\2\2\u05bf\u05c1\7\24\2\2\u05c0\u05c2\7J\2\2\u05c1\u05c0\3\2\2\2\u05c1"+
		"\u05c2\3\2\2\2\u05c2\u05c3\3\2\2\2\u05c3\u05c4\5\u0080A\2\u05c4\u00bf"+
		"\3\2\2\2\u05c5\u05c7\t\3\2\2\u05c6\u05c8\7J\2\2\u05c7\u05c6\3\2\2\2\u05c7"+
		"\u05c8\3\2\2\2\u05c8\u05c9\3\2\2\2\u05c9\u05cb\5,\27\2\u05ca\u05cc\7J"+
		"\2\2\u05cb\u05ca\3\2\2\2\u05cb\u05cc\3\2\2\2\u05cc\u05cd\3\2\2\2\u05cd"+
		"\u05cf\7\3\2\2\u05ce\u05d0\7J\2\2\u05cf\u05ce\3\2\2\2\u05cf\u05d0\3\2"+
		"\2\2\u05d0\u05d1\3\2\2\2\u05d1\u05d2\5\u0080A\2\u05d2\u00c1\3\2\2\2\u05d3"+
		"\u05d5\t\4\2\2\u05d4\u05d6\7J\2\2\u05d5\u05d4\3\2\2\2\u05d5\u05d6\3\2"+
		"\2\2\u05d6\u05d7\3\2\2\2\u05d7\u05d9\5,\27\2\u05d8\u05da\7J\2\2\u05d9"+
		"\u05d8\3\2\2\2\u05d9\u05da\3\2\2\2\u05da\u05db\3\2\2\2\u05db\u05dd\7\3"+
		"\2\2\u05dc\u05de\7J\2\2\u05dd\u05dc\3\2\2\2\u05dd\u05de\3\2\2\2\u05de"+
		"\u05df\3\2\2\2\u05df\u05e0\5\u0080A\2\u05e0\u00c3\3\2\2\2\u05e1\u05e2"+
		"\5\u00dan\2\u05e2\u00c5\3\2\2\2\u05e3\u05ee\5\u00c4c\2\u05e4\u05e6\7J"+
		"\2\2\u05e5\u05e4\3\2\2\2\u05e5\u05e6\3\2\2\2\u05e6\u05e7\3\2\2\2\u05e7"+
		"\u05e9\7\5\2\2\u05e8\u05ea\7J\2\2\u05e9\u05e8\3\2\2\2\u05e9\u05ea\3\2"+
		"\2\2\u05ea\u05eb\3\2\2\2\u05eb\u05ed\5\u00c4c\2\u05ec\u05e5\3\2\2\2\u05ed"+
		"\u05f0\3\2\2\2\u05ee\u05ec\3\2\2\2\u05ee\u05ef\3\2\2\2\u05ef\u00c7\3\2"+
		"\2\2\u05f0\u05ee\3\2\2\2\u05f1\u05fb\5\u00d6l\2\u05f2\u05fb\5\u00ceh\2"+
		"\u05f3\u05fb\5\u00d8m\2\u05f4\u05fb\5\u00d4k\2\u05f5\u05fb\5\u00d0i\2"+
		"\u05f6\u05fb\5\u00d2j\2\u05f7\u05fb\5\u00dco\2\u05f8\u05fb\5\u00dep\2"+
		"\u05f9\u05fb\5\u00e0q\2\u05fa\u05f1\3\2\2\2\u05fa\u05f2\3\2\2\2\u05fa"+
		"\u05f3\3\2\2\2\u05fa\u05f4\3\2\2\2\u05fa\u05f5\3\2\2\2\u05fa\u05f6\3\2"+
		"\2\2\u05fa\u05f7\3\2\2\2\u05fa\u05f8\3\2\2\2\u05fa\u05f9\3\2\2\2\u05fb"+
		"\u00c9\3\2\2\2\u05fc\u05fe\5\u00c4c\2\u05fd\u05ff\7J\2\2\u05fe\u05fd\3"+
		"\2\2\2\u05fe\u05ff\3\2\2\2\u05ff\u0600\3\2\2\2\u0600\u0602\7\24\2\2\u0601"+
		"\u0603\7J\2\2\u0602\u0601\3\2\2\2\u0602\u0603\3\2\2\2\u0603\u0604\3\2"+
		"\2\2\u0604\u0605\5\u00c4c\2\u0605\u00cb\3\2\2\2\u0606\u0611\5\u00caf\2"+
		"\u0607\u0609\7J\2\2\u0608\u0607\3\2\2\2\u0608\u0609\3\2\2\2\u0609\u060a"+
		"\3\2\2\2\u060a\u060c\7\5\2\2\u060b\u060d\7J\2\2\u060c\u060b\3\2\2\2\u060c"+
		"\u060d\3\2\2\2\u060d\u060e\3\2\2\2\u060e\u0610\5\u00caf\2\u060f\u0608"+
		"\3\2\2\2\u0610\u0613\3\2\2\2\u0611\u060f\3\2\2\2\u0611\u0612\3\2\2\2\u0612"+
		"\u00cd\3\2\2\2\u0613\u0611\3\2\2\2\u0614\u0615\5\u0100\u0081\2\u0615\u00cf"+
		"\3\2\2\2\u0616\u0617\5\"\22\2\u0617\u0618\7\3\2\2\u0618\u061a\3\2\2\2"+
		"\u0619\u0616\3\2\2\2\u0619\u061a\3\2\2\2\u061a\u061b\3\2\2\2\u061b\u0620"+
		"\5\36\20\2\u061c\u061e\7J\2\2\u061d\u061c\3\2\2\2\u061d\u061e\3\2\2\2"+
		"\u061e\u061f\3\2\2\2\u061f\u0621\5\u00c4c\2\u0620\u061d\3\2\2\2\u0620"+
		"\u0621\3\2\2\2\u0621\u00d1\3\2\2\2\u0622\u0624\7\7\2\2\u0623\u0625\7J"+
		"\2\2\u0624\u0623\3\2\2\2\u0624\u0625\3\2\2\2\u0625\u0627\3\2\2\2\u0626"+
		"\u0628\5\u00c6d\2\u0627\u0626\3\2\2\2\u0627\u0628\3\2\2\2\u0628\u062a"+
		"\3\2\2\2\u0629\u062b\7J\2\2\u062a\u0629\3\2\2\2\u062a\u062b\3\2\2\2\u062b"+
		"\u062c\3\2\2\2\u062c\u062d\7\b\2\2\u062d\u00d3\3\2\2\2\u062e\u062f\7\\"+
		"\2\2\u062f\u00d5\3\2\2\2\u0630\u0631\7X\2\2\u0631\u00d7\3\2\2\2\u0632"+
		"\u0633\5$\23\2\u0633\u00d9\3\2\2\2\u0634\u063d\5\u00c8e\2\u0635\u0637"+
		"\7J\2\2\u0636\u0635\3\2\2\2\u0636\u0637\3\2\2\2\u0637\u0638\3\2\2\2\u0638"+
		"\u063a\7\26\2\2\u0639\u063b\7J\2\2\u063a\u0639\3\2\2\2\u063a\u063b\3\2"+
		"\2\2\u063b\u063c\3\2\2\2\u063c\u063e\5\u00c4c\2\u063d\u0636\3\2\2\2\u063d"+
		"\u063e\3\2\2\2\u063e\u00db\3\2\2\2\u063f\u0641\7\27\2\2\u0640\u0642\7"+
		"J\2\2\u0641\u0640\3\2\2\2\u0641\u0642\3\2\2\2\u0642\u0644\3\2\2\2\u0643"+
		"\u0645\5\u00c6d\2\u0644\u0643\3\2\2\2\u0644\u0645\3\2\2\2\u0645\u0650"+
		"\3\2\2\2\u0646\u0648\7J\2\2\u0647\u0646\3\2\2\2\u0647\u0648\3\2\2\2\u0648"+
		"\u0649\3\2\2\2\u0649\u064b\7\5\2\2\u064a\u064c\7J\2\2\u064b\u064a\3\2"+
		"\2\2\u064b\u064c\3\2\2\2\u064c\u064d\3\2\2\2\u064d\u064e\5\u00c4c\2\u064e"+
		"\u064f\7\36\2\2\u064f\u0651\3\2\2\2\u0650\u0647\3\2\2\2\u0650\u0651\3"+
		"\2\2\2\u0651\u0653\3\2\2\2\u0652\u0654\7J\2\2\u0653\u0652\3\2\2\2\u0653"+
		"\u0654\3\2\2\2\u0654\u0655\3\2\2\2\u0655\u0656\7\r\2\2\u0656\u00dd\3\2"+
		"\2\2\u0657\u0659\7\30\2\2\u0658\u065a\7J\2\2\u0659\u0658\3\2\2\2\u0659"+
		"\u065a\3\2\2\2\u065a\u065c\3\2\2\2\u065b\u065d\5\u00c6d\2\u065c\u065b"+
		"\3\2\2\2\u065c\u065d\3\2\2\2\u065d\u0668\3\2\2\2\u065e\u0660\7J\2\2\u065f"+
		"\u065e\3\2\2\2\u065f\u0660\3\2\2\2\u0660\u0661\3\2\2\2\u0661\u0663\7\5"+
		"\2\2\u0662\u0664\7J\2\2\u0663\u0662\3\2\2\2\u0663\u0664\3\2\2\2\u0664"+
		"\u0665\3\2\2\2\u0665\u0666\5\u00c4c\2\u0666\u0667\7\36\2\2\u0667\u0669"+
		"\3\2\2\2\u0668\u065f\3\2\2\2\u0668\u0669\3\2\2\2\u0669\u066b\3\2\2\2\u066a"+
		"\u066c\7J\2\2\u066b\u066a\3\2\2\2\u066b\u066c\3\2\2\2\u066c\u066d\3\2"+
		"\2\2\u066d\u066e\7\n\2\2\u066e\u00df\3\2\2\2\u066f\u0671\7\31\2\2\u0670"+
		"\u0672\7J\2\2\u0671\u0670\3\2\2\2\u0671\u0672\3\2\2\2\u0672\u0674\3\2"+
		"\2\2\u0673\u0675\5\u00ccg\2\u0674\u0673\3\2\2\2\u0674\u0675\3\2\2\2\u0675"+
		"\u0680\3\2\2\2\u0676\u0678\7J\2\2\u0677\u0676\3\2\2\2\u0677\u0678\3\2"+
		"\2\2\u0678\u0679\3\2\2\2\u0679\u067b\7\5\2\2\u067a\u067c\7J\2\2\u067b"+
		"\u067a\3\2\2\2\u067b\u067c\3\2\2\2\u067c\u067d\3\2\2\2\u067d\u067e\5\u00c4"+
		"c\2\u067e\u067f\7\36\2\2\u067f\u0681\3\2\2\2\u0680\u0677\3\2\2\2\u0680"+
		"\u0681\3\2\2\2\u0681\u0683\3\2\2\2\u0682\u0684\7J\2\2\u0683\u0682\3\2"+
		"\2\2\u0683\u0684\3\2\2\2\u0684\u0685\3\2\2\2\u0685\u0686\7\n\2\2\u0686"+
		"\u00e1\3\2\2\2\u0687\u0688\t\5\2\2\u0688\u00e3\3\2\2\2\u0689\u068a\7c"+
		"\2\2\u068a\u00e5\3\2\2\2\u068b\u068c\7d\2\2\u068c\u00e7\3\2\2\2\u068d"+
		"\u068e\7!\2\2\u068e\u00e9\3\2\2\2\u068f\u0691\5\u00e8u\2\u0690\u068f\3"+
		"\2\2\2\u0690\u0691\3\2\2\2\u0691\u0692\3\2\2\2\u0692\u0693\7e\2\2\u0693"+
		"\u0694\7\3\2\2\u0694\u0695\7e\2\2\u0695\u0696\7\"\2\2\u0696\u00eb\3\2"+
		"\2\2\u0697\u0699\5\u00e8u\2\u0698\u0697\3\2\2\2\u0698\u0699\3\2\2\2\u0699"+
		"\u069a\3\2\2\2\u069a\u069b\7e\2\2\u069b\u069c\7\3\2\2\u069c\u069d\7e\2"+
		"\2\u069d\u069e\7#\2\2\u069e\u00ed\3\2\2\2\u069f\u06a1\5\u00e8u\2\u06a0"+
		"\u069f\3\2\2\2\u06a0\u06a1\3\2\2\2\u06a1\u06a2\3\2\2\2\u06a2\u06a3\7e"+
		"\2\2\u06a3\u06a4\7\3\2\2\u06a4\u06a5\7e\2\2\u06a5\u00ef\3\2\2\2\u06a6"+
		"\u06aa\5\u00eav\2\u06a7\u06aa\5\u00ecw\2\u06a8\u06aa\5\u00eex\2\u06a9"+
		"\u06a6\3\2\2\2\u06a9\u06a7\3\2\2\2\u06a9\u06a8\3\2\2\2\u06aa\u00f1\3\2"+
		"\2\2\u06ab\u06ad\5\u00e8u\2\u06ac\u06ab\3\2\2\2\u06ac\u06ad\3\2\2\2\u06ad"+
		"\u06ae\3\2\2\2\u06ae\u06af\7e\2\2\u06af\u06b0\7$\2\2\u06b0\u00f3\3\2\2"+
		"\2\u06b1\u06b3\5\u00e8u\2\u06b2\u06b1\3\2\2\2\u06b2\u06b3\3\2\2\2\u06b3"+
		"\u06b4\3\2\2\2\u06b4\u06b5\7e\2\2\u06b5\u06b6\7%\2\2\u06b6\u00f5\3\2\2"+
		"\2\u06b7\u06b9\5\u00e8u\2\u06b8\u06b7\3\2\2\2\u06b8\u06b9\3\2\2\2\u06b9"+
		"\u06ba\3\2\2\2\u06ba\u06bb\7e\2\2\u06bb\u06bc\7&\2\2\u06bc\u00f7\3\2\2"+
		"\2\u06bd\u06bf\5\u00e8u\2\u06be\u06bd\3\2\2\2\u06be\u06bf\3\2\2\2\u06bf"+
		"\u06c0\3\2\2\2\u06c0\u06c1\7e\2\2\u06c1\u06c2\7\'\2\2\u06c2\u00f9\3\2"+
		"\2\2\u06c3\u06c5\5\u00e8u\2\u06c4\u06c3\3\2\2\2\u06c4\u06c5\3\2\2\2\u06c5"+
		"\u06c6\3\2\2\2\u06c6\u06c7\7e\2\2\u06c7\u06c8\7(\2\2\u06c8\u00fb\3\2\2"+
		"\2\u06c9\u06cb\5\u00e8u\2\u06ca\u06c9\3\2\2\2\u06ca\u06cb\3\2\2\2\u06cb"+
		"\u06cc\3\2\2\2\u06cc\u06cd\7e\2\2\u06cd\u00fd\3\2\2\2\u06ce\u06d5\5\u00f2"+
		"z\2\u06cf\u06d5\5\u00f4{\2\u06d0\u06d5\5\u00f6|\2\u06d1\u06d5\5\u00f8"+
		"}\2\u06d2\u06d5\5\u00fa~\2\u06d3\u06d5\5\u00fc\177\2\u06d4\u06ce\3\2\2"+
		"\2\u06d4\u06cf\3\2\2\2\u06d4\u06d0\3\2\2\2\u06d4\u06d1\3\2\2\2\u06d4\u06d2"+
		"\3\2\2\2\u06d4\u06d3\3\2\2\2\u06d5\u00ff\3\2\2\2\u06d6\u06dc\5\u00e2r"+
		"\2\u06d7\u06dc\5\u00e4s\2\u06d8\u06dc\5\u00f0y\2\u06d9\u06dc\5\u00fe\u0080"+
		"\2\u06da\u06dc\5\u00e6t\2\u06db\u06d6\3\2\2\2\u06db\u06d7\3\2\2\2\u06db"+
		"\u06d8\3\2\2\2\u06db\u06d9\3\2\2\2\u06db\u06da\3\2\2\2\u06dc\u0101\3\2"+
		"\2\2\u06dd\u06e3\5\u010a\u0086\2\u06de\u06e3\5\u0112\u008a\2\u06df\u06e3"+
		"\5\u0114\u008b\2\u06e0\u06e3\5\u0104\u0083\2\u06e1\u06e3\5\u0106\u0084"+
		"\2\u06e2\u06dd\3\2\2\2\u06e2\u06de\3\2\2\2\u06e2\u06df\3\2\2\2\u06e2\u06e0"+
		"\3\2\2\2\u06e2\u06e1\3\2\2\2\u06e3\u0103\3\2\2\2\u06e4\u06e5\5$\23\2\u06e5"+
		"\u0105\3\2\2\2\u06e6\u06e7\5\"\22\2\u06e7\u0107\3\2\2\2\u06e8\u06f1\5"+
		"\u0102\u0082\2\u06e9\u06eb\7J\2\2\u06ea\u06e9\3\2\2\2\u06ea\u06eb\3\2"+
		"\2\2\u06eb\u06ec\3\2\2\2\u06ec\u06ee\7\24\2\2\u06ed\u06ef\7J\2\2\u06ee"+
		"\u06ed\3\2\2\2\u06ee\u06ef\3\2\2\2\u06ef\u06f0\3\2\2\2\u06f0\u06f2\5\u0108"+
		"\u0085\2\u06f1\u06ea\3\2\2\2\u06f1\u06f2\3\2\2\2\u06f2\u0109\3\2\2\2\u06f3"+
		"\u06f5\7\7\2\2\u06f4\u06f6\7J\2\2\u06f5\u06f4\3\2\2\2\u06f5\u06f6\3\2"+
		"\2\2\u06f6\u06f7\3\2\2\2\u06f7\u0702\5\u0108\u0085\2\u06f8\u06fa\7J\2"+
		"\2\u06f9\u06f8\3\2\2\2\u06f9\u06fa\3\2\2\2\u06fa\u06fb\3\2\2\2\u06fb\u06fd"+
		"\7\5\2\2\u06fc\u06fe\7J\2\2\u06fd\u06fc\3\2\2\2\u06fd\u06fe\3\2\2\2\u06fe"+
		"\u06ff\3\2\2\2\u06ff\u0701\5\u0108\u0085\2\u0700\u06f9\3\2\2\2\u0701\u0704"+
		"\3\2\2\2\u0702\u0700\3\2\2\2\u0702\u0703\3\2\2\2\u0703\u0706\3\2\2\2\u0704"+
		"\u0702\3\2\2\2\u0705\u0707\7J\2\2\u0706\u0705\3\2\2\2\u0706\u0707\3\2"+
		"\2\2\u0707\u0708\3\2\2\2\u0708\u070a\7\b\2\2\u0709\u070b\7J\2\2\u070a"+
		"\u0709\3\2\2\2\u070a\u070b\3\2\2\2\u070b\u070c\3\2\2\2\u070c\u070e\7\24"+
		"\2\2\u070d\u070f\7J\2\2\u070e\u070d\3\2\2\2\u070e\u070f\3\2\2\2\u070f"+
		"\u0710\3\2\2\2\u0710\u0711\5\u0108\u0085\2\u0711\u010b\3\2\2\2\u0712\u0713"+
		"\7\7\2\2\u0713\u0714\7\b\2\2\u0714\u010d\3\2\2\2\u0715\u0717\7\7\2\2\u0716"+
		"\u0718\7J\2\2\u0717\u0716\3\2\2\2\u0717\u0718\3\2\2\2\u0718\u0719\3\2"+
		"\2\2\u0719\u071b\5\u0108\u0085\2\u071a\u071c\7J\2\2\u071b\u071a\3\2\2"+
		"\2\u071b\u071c\3\2\2\2\u071c\u071d\3\2\2\2\u071d\u071e\7\b\2\2\u071e\u010f"+
		"\3\2\2\2\u071f\u0721\7\7\2\2\u0720\u0722\7J\2\2\u0721\u0720\3\2\2\2\u0721"+
		"\u0722\3\2\2\2\u0722\u0723\3\2\2\2\u0723\u072c\5\u0108\u0085\2\u0724\u0726"+
		"\7J\2\2\u0725\u0724\3\2\2\2\u0725\u0726\3\2\2\2\u0726\u0727\3\2\2\2\u0727"+
		"\u0729\7\5\2\2\u0728\u072a\7J\2\2\u0729\u0728\3\2\2\2\u0729\u072a\3\2"+
		"\2\2\u072a\u072b\3\2\2\2\u072b\u072d\5\u0108\u0085\2\u072c\u0725\3\2\2"+
		"\2\u072d\u072e\3\2\2\2\u072e\u072c\3\2\2\2\u072e\u072f\3\2\2\2\u072f\u0731"+
		"\3\2\2\2\u0730\u0732\7J\2\2\u0731\u0730\3\2\2\2\u0731\u0732\3\2\2\2\u0732"+
		"\u0733\3\2\2\2\u0733\u0734\7\b\2\2\u0734\u0111\3\2\2\2\u0735\u0739\5\u010c"+
		"\u0087\2\u0736\u0739\5\u010e\u0088\2\u0737\u0739\5\u0110\u0089\2\u0738"+
		"\u0735\3\2\2\2\u0738\u0736\3\2\2\2\u0738\u0737\3\2\2\2\u0739\u0113\3\2"+
		"\2\2\u073a\u073c\5\u0106\u0084\2\u073b\u073d\7J\2\2\u073c\u073b\3\2\2"+
		"\2\u073c\u073d\3\2\2\2\u073d\u073e\3\2\2\2\u073e\u0740\7\f\2\2\u073f\u0741"+
		"\7J\2\2\u0740\u073f\3\2\2\2\u0740\u0741\3\2\2\2\u0741\u0742\3\2\2\2\u0742"+
		"\u074d\5\u0108\u0085\2\u0743\u0745\7J\2\2\u0744\u0743\3\2\2\2\u0744\u0745"+
		"\3\2\2\2\u0745\u0746\3\2\2\2\u0746\u0748\7\5\2\2\u0747\u0749\7J\2\2\u0748"+
		"\u0747\3\2\2\2\u0748\u0749\3\2\2\2\u0749\u074a\3\2\2\2\u074a\u074c\5\u0108"+
		"\u0085\2\u074b\u0744\3\2\2\2\u074c\u074f\3\2\2\2\u074d\u074b\3\2\2\2\u074d"+
		"\u074e\3\2\2\2\u074e\u0751\3\2\2\2\u074f\u074d\3\2\2\2\u0750\u0752\7J"+
		"\2\2\u0751\u0750\3\2\2\2\u0751\u0752\3\2\2\2\u0752\u0753\3\2\2\2\u0753"+
		"\u0755\7\r\2\2\u0754\u0756\7J\2\2\u0755\u0754\3\2\2\2\u0755\u0756\3\2"+
		"\2\2\u0756\u0115\3\2\2\2\u0757\u0758\t\6\2\2\u0758\u0117\3\2\2\2\u0759"+
		"\u075a\t\7\2\2\u075a\u0119\3\2\2\2\u075b\u075c\t\b\2\2\u075c\u011b\3\2"+
		"\2\2\u075d\u075e\t\t\2\2\u075e\u011d\3\2\2\2\u075f\u0760\t\n\2\2\u0760"+
		"\u011f\3\2\2\2\u0761\u0762\t\13\2\2\u0762\u0121\3\2\2\2\u0763\u076a\5"+
		"\u0126\u0094\2\u0764\u076a\5\u0128\u0095\2\u0765\u076a\5\u012a\u0096\2"+
		"\u0766\u076a\5\u012e\u0098\2\u0767\u076a\5\u012c\u0097\2\u0768\u076a\5"+
		"\u0130\u0099\2\u0769\u0763\3\2\2\2\u0769\u0764\3\2\2\2\u0769\u0765\3\2"+
		"\2\2\u0769\u0766\3\2\2\2\u0769\u0767\3\2\2\2\u0769\u0768\3\2\2\2\u076a"+
		"\u0123\3\2\2\2\u076b\u0776\5\u0122\u0092\2\u076c\u076e\7J\2\2\u076d\u076c"+
		"\3\2\2\2\u076d\u076e\3\2\2\2\u076e\u076f\3\2\2\2\u076f\u0771\7\5\2\2\u0770"+
		"\u0772\7J\2\2\u0771\u0770\3\2\2\2\u0771\u0772\3\2\2\2\u0772\u0773\3\2"+
		"\2\2\u0773\u0775\5\u0122\u0092\2\u0774\u076d\3\2\2\2\u0775\u0778\3\2\2"+
		"\2\u0776\u0774\3\2\2\2\u0776\u0777\3\2\2\2\u0777\u0125\3\2\2\2\u0778\u0776"+
		"\3\2\2\2\u0779\u077a\7\37\2\2\u077a\u0127\3\2\2\2\u077b\u077c\7 \2\2\u077c"+
		"\u0129\3\2\2\2\u077d\u077f\5\30\r\2\u077e\u0780\7J\2\2\u077f\u077e\3\2"+
		"\2\2\u077f\u0780\3\2\2\2\u0780\u0781\3\2\2\2\u0781\u0782\7\7\2\2\u0782"+
		"\u0783\5\u0084C\2\u0783\u0784\7\b\2\2\u0784\u012b\3\2\2\2\u0785\u0787"+
		"\5\34\17\2\u0786\u0788\7J\2\2\u0787\u0786\3\2\2\2\u0787\u0788\3\2\2\2"+
		"\u0788\u0789\3\2\2\2\u0789\u078a\7\7\2\2\u078a\u078b\5\u0084C\2\u078b"+
		"\u078c\7\b\2\2\u078c\u012d\3\2\2\2\u078d\u078f\5$\23\2\u078e\u0790\7J"+
		"\2\2\u078f\u078e\3\2\2\2\u078f\u0790\3\2\2\2\u0790\u0791\3\2\2\2\u0791"+
		"\u0793\7>\2\2\u0792\u0794\7J\2\2\u0793\u0792\3\2\2\2\u0793\u0794\3\2\2"+
		"\2\u0794\u0795\3\2\2\2\u0795\u0796\5$\23\2\u0796\u012f\3\2\2\2\u0797\u0799"+
		"\5$\23\2\u0798\u079a\7J\2\2\u0799\u0798\3\2\2\2\u0799\u079a\3\2\2\2\u079a"+
		"\u079b\3\2\2\2\u079b\u079d\7H\2\2\u079c\u079e\7J\2\2\u079d\u079c\3\2\2"+
		"\2\u079d\u079e\3\2\2\2\u079e\u079f\3\2\2\2\u079f\u07a0\5\u0080A\2\u07a0"+
		"\u0131\3\2\2\2\u0148\u0137\u013d\u0141\u0146\u0149\u0152\u0158\u015f\u017b"+
		"\u017f\u0186\u018c\u0190\u0195\u019a\u019d\u01a0\u01a3\u01a7\u01ab\u01b1"+
		"\u01b5\u01ba\u01bf\u01c3\u01c7\u01cc\u01cf\u01d2\u01d8\u01dc\u01e1\u01e8"+
		"\u01ec\u01f0\u01f4\u01f9\u0200\u0204\u0208\u020c\u0211\u0216\u021a\u021d"+
		"\u0221\u0225\u0229\u022e\u0233\u0238\u023c\u0241\u024b\u024f\u0254\u0258"+
		"\u025c\u0260\u0262\u026c\u0270\u0275\u0297\u029a\u02a0\u02a4\u02a9\u02ad"+
		"\u02b3\u02b8\u02bc\u02c3\u02c7\u02cb\u02d2\u02d6\u02db\u02e2\u02e5\u02ea"+
		"\u02ee\u02f4\u02f8\u02fb\u02fe\u0304\u0309\u030d\u0313\u0317\u031a\u031d"+
		"\u0323\u0329\u032d\u0330\u0333\u0339\u033e\u0342\u0348\u034c\u0350\u0356"+
		"\u035b\u035f\u0363\u0369\u036d\u0371\u0377\u037c\u0380\u0383\u0386\u038c"+
		"\u0391\u0395\u0399\u039d\u03a3\u03a8\u03ac\u03b2\u03b6\u03ba\u03be\u03c2"+
		"\u03c6\u03cc\u03d1\u03d5\u03dc\u03e0\u03e6\u03ea\u03ee\u03f6\u03f9\u03fd"+
		"\u0402\u0406\u040a\u040e\u0415\u0419\u041e\u0422\u0426\u042a\u0430\u0435"+
		"\u0439\u0440\u0444\u044a\u044f\u0453\u0459\u045d\u0461\u0464\u0468\u046c"+
		"\u0470\u0474\u0478\u047d\u0482\u0486\u048a\u048e\u0492\u0496\u049a\u049e"+
		"\u04a2\u04a6\u04ac\u04b0\u04b4\u04b8\u04bc\u04c0\u04c5\u04c9\u04cd\u04d0"+
		"\u04e5\u04eb\u04ef\u04f3\u04f7\u04fd\u0501\u0505\u0509\u0519\u051d\u0525"+
		"\u0529\u052f\u0533\u0536\u0539\u053c\u0547\u054b\u054e\u0552\u0555\u0558"+
		"\u055e\u0562\u0568\u056c\u0571\u057c\u0580\u0583\u0587\u058a\u058d\u0593"+
		"\u0596\u0599\u059f\u05a2\u05a5\u05ab\u05af\u05b5\u05b9\u05bd\u05c1\u05c7"+
		"\u05cb\u05cf\u05d5\u05d9\u05dd\u05e5\u05e9\u05ee\u05fa\u05fe\u0602\u0608"+
		"\u060c\u0611\u0619\u061d\u0620\u0624\u0627\u062a\u0636\u063a\u063d\u0641"+
		"\u0644\u0647\u064b\u0650\u0653\u0659\u065c\u065f\u0663\u0668\u066b\u0671"+
		"\u0674\u0677\u067b\u0680\u0683\u0690\u0698\u06a0\u06a9\u06ac\u06b2\u06b8"+
		"\u06be\u06c4\u06ca\u06d4\u06db\u06e2\u06ea\u06ee\u06f1\u06f5\u06f9\u06fd"+
		"\u0702\u0706\u070a\u070e\u0717\u071b\u0721\u0725\u0729\u072e\u0731\u0738"+
		"\u073c\u0740\u0744\u0748\u074d\u0751\u0755\u0769\u076d\u0771\u0776\u077f"+
		"\u0787\u078f\u0793\u0799\u079d";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}