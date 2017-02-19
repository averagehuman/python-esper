grammar EsperEPL2Grammar;

@header {
"""
/*
 ***************************************************************************************
 *  Copyright (C) 2006 EsperTech, Inc. All rights reserved.                            *
 *  http://www.espertech.com/esper                                                     *
 *  http://www.espertech.com                                                           *
 *  ---------------------------------------------------------------------------------- *
 *  The software in this package is published under the terms of the GPL license       *
 *  a copy of which has been included with this distribution in the license.txt file.  *
 ***************************************************************************************
 */ 
"""
}

@members {
# provide nice error messages
paraphrases = []

# static information initialized once
lexerTokenParaphases = {}
parserTokenParaphases = {}
parserKeywordSet = set()
afterScriptTokens = set()

@classmethod
def getKeywords(cls):
    cls.getParserTokenParaphrases()
    return cls.parserKeywordSet;

@classmethod
def getLexerTokenParaphrases(cls):
    if not cls.lexerTokenParaphases:
        cls.lexerTokenParaphases[IDENT] =  "an identifier"
        cls.lexerTokenParaphases[cls.FOLLOWED_BY] =  "an followed-by '->'"
        cls.lexerTokenParaphases[cls.EQUALS] =  "an equals '='"
        cls.lexerTokenParaphases[cls.SQL_NE] =  "a sql-style not equals '<>'"
        cls.lexerTokenParaphases[cls.QUESTION] =  "a questionmark '?'"
        cls.lexerTokenParaphases[cls.LPAREN] =  "an opening parenthesis '('"
        cls.lexerTokenParaphases[cls.RPAREN] =  "a closing parenthesis ')'"
        cls.lexerTokenParaphases[cls.LBRACK] =  "a left angle bracket '['"
        cls.lexerTokenParaphases[cls.RBRACK] =  "a right angle bracket ']'"
        cls.lexerTokenParaphases[cls.LCURLY] =  "a left curly bracket '{'"
        cls.lexerTokenParaphases[cls.RCURLY] =  "a right curly bracket '}'"
        cls.lexerTokenParaphases[cls.COLON] =  "a colon ':'"
        cls.lexerTokenParaphases[cls.COMMA] =  "a comma ','"
        cls.lexerTokenParaphases[cls.EQUAL] =  "an equals compare '=='"
        cls.lexerTokenParaphases[cls.LNOT] =  "a not '!'"
        cls.lexerTokenParaphases[cls.BNOT] =  "a binary not '~'"
        cls.lexerTokenParaphases[cls.NOT_EQUAL] =  "a not equals '!='"
        cls.lexerTokenParaphases[cls.DIV] =  "a division operator '\'"
        cls.lexerTokenParaphases[cls.DIV_ASSIGN] =  "a division assign '/='"
        cls.lexerTokenParaphases[cls.PLUS] =  "a plus operator '+'"
        cls.lexerTokenParaphases[cls.PLUS_ASSIGN] =  "a plus assign '+='"
        cls.lexerTokenParaphases[cls.INC] =  "an increment operator '++'"
        cls.lexerTokenParaphases[cls.MINUS] =  "a minus '-'"
        cls.lexerTokenParaphases[cls.MINUS_ASSIGN] =  "a minus assign '-='"
        cls.lexerTokenParaphases[cls.DEC] =  "a decrement operator '--'"
        cls.lexerTokenParaphases[cls.STAR] =  "a star '*'"
        cls.lexerTokenParaphases[cls.STAR_ASSIGN] =  "a star assign '*='"
        cls.lexerTokenParaphases[cls.MOD] =  "a modulo"
        cls.lexerTokenParaphases[cls.MOD_ASSIGN] =  "a modulo assign"
        cls.lexerTokenParaphases[cls.GE] =  "a greater equals '>='"
        cls.lexerTokenParaphases[cls.GT] =  "a greater then '>'"
        cls.lexerTokenParaphases[cls.LE] =  "a less equals '<='"
        cls.lexerTokenParaphases[cls.LT] =  "a lesser then '<'"
        cls.lexerTokenParaphases[cls.BXOR] =  "a binary xor '^'"
        cls.lexerTokenParaphases[cls.BXOR_ASSIGN] =  "a binary xor assign '^='"
        cls.lexerTokenParaphases[cls.BOR] =  "a binary or '|'"
        cls.lexerTokenParaphases[cls.BOR_ASSIGN] =  "a binary or assign '|='"
        cls.lexerTokenParaphases[cls.LOR] =  "a logical or '||'"
        cls.lexerTokenParaphases[cls.BAND] =  "a binary and '&'"
        cls.lexerTokenParaphases[cls.BAND_ASSIGN] =  "a binary and assign '&='"
        cls.lexerTokenParaphases[cls.LAND] =  "a logical and '&&'"
        cls.lexerTokenParaphases[cls.SEMI] =  "a semicolon ';'"
        cls.lexerTokenParaphases[cls.DOT] =  "a dot '.'"
    return cls.lexerTokenParaphases

@classmethod
def getParserTokenParaphrases(cls):
    if not cls.parserTokenParaphases:
        cls.parserTokenParaphases[cls.CREATE] =  "'create'"
        cls.parserTokenParaphases[cls.WINDOW] =  "'window'"
        cls.parserTokenParaphases[cls.IN_SET] =  "'in'"
        cls.parserTokenParaphases[cls.BETWEEN] =  "'between'"
        cls.parserTokenParaphases[cls.LIKE] =  "'like'"
        cls.parserTokenParaphases[cls.REGEXP] =  "'regexp'"
        cls.parserTokenParaphases[cls.ESCAPE] =  "'escape'"
        cls.parserTokenParaphases[cls.OR_EXPR] =  "'or'"
        cls.parserTokenParaphases[cls.AND_EXPR] =  "'and'"
        cls.parserTokenParaphases[cls.NOT_EXPR] =  "'not'"
        cls.parserTokenParaphases[cls.EVERY_EXPR] =  "'every'"
        cls.parserTokenParaphases[cls.EVERY_DISTINCT_EXPR] =  "'every-distinct'"
        cls.parserTokenParaphases[cls.WHERE] =  "'where'"
        cls.parserTokenParaphases[cls.AS] =  "'as'"
        cls.parserTokenParaphases[cls.SUM] =  "'sum'"
        cls.parserTokenParaphases[cls.AVG] =  "'avg'"
        cls.parserTokenParaphases[cls.MAX] =  "'max'"
        cls.parserTokenParaphases[cls.MIN] =  "'min'"
        cls.parserTokenParaphases[cls.COALESCE] =  "'coalesce'"
        cls.parserTokenParaphases[cls.MEDIAN] =  "'median'"
        cls.parserTokenParaphases[cls.STDDEV] =  "'stddev'"
        cls.parserTokenParaphases[cls.AVEDEV] =  "'avedev'"
        cls.parserTokenParaphases[cls.COUNT] =  "'count'"
        cls.parserTokenParaphases[cls.SELECT] =  "'select'"
        cls.parserTokenParaphases[cls.CASE] =  "'case'"
        cls.parserTokenParaphases[cls.ELSE] =  "'else'"
        cls.parserTokenParaphases[cls.WHEN] =  "'when'"
        cls.parserTokenParaphases[cls.THEN] =  "'then'"
        cls.parserTokenParaphases[cls.END] =  "'end'"
        cls.parserTokenParaphases[cls.FROM] =  "'from'"
        cls.parserTokenParaphases[cls.OUTER] =  "'outer'"
        cls.parserTokenParaphases[cls.INNER] =  "'inner'"
        cls.parserTokenParaphases[cls.JOIN] =  "'join'"
        cls.parserTokenParaphases[cls.LEFT] =  "'left'"
        cls.parserTokenParaphases[cls.RIGHT] =  "'right'"
        cls.parserTokenParaphases[cls.FULL] =  "'full'"
        cls.parserTokenParaphases[cls.ON] =  "'on'"
        cls.parserTokenParaphases[cls.IS] =  "'is'"
        cls.parserTokenParaphases[cls.BY] =  "'by'"
        cls.parserTokenParaphases[cls.GROUP] =  "'group'"
        cls.parserTokenParaphases[cls.HAVING] =  "'having'"
        cls.parserTokenParaphases[cls.ALL] =  "'all'"
        cls.parserTokenParaphases[cls.ANY] =  "'any'"
        cls.parserTokenParaphases[cls.SOME] =  "'some'"
        cls.parserTokenParaphases[cls.OUTPUT] =  "'output'"
        cls.parserTokenParaphases[cls.EVENTS] =  "'events'"
        cls.parserTokenParaphases[cls.FIRST] =  "'first'"
        cls.parserTokenParaphases[cls.LAST] =  "'last'"
        cls.parserTokenParaphases[cls.INSERT] =  "'insert'"
        cls.parserTokenParaphases[cls.INTO] =  "'into'"
        cls.parserTokenParaphases[cls.ORDER] =  "'order'"
        cls.parserTokenParaphases[cls.ASC] =  "'asc'"
        cls.parserTokenParaphases[cls.DESC] =  "'desc'"
        cls.parserTokenParaphases[cls.RSTREAM] =  "'rstream'"
        cls.parserTokenParaphases[cls.ISTREAM] =  "'istream'"
        cls.parserTokenParaphases[cls.IRSTREAM] =  "'irstream'"
        cls.parserTokenParaphases[cls.SCHEMA] =  "'schema'"
        cls.parserTokenParaphases[cls.UNIDIRECTIONAL] =  "'unidirectional'"
        cls.parserTokenParaphases[cls.RETAINUNION] =  "'retain-union'"
        cls.parserTokenParaphases[cls.RETAININTERSECTION] =  "'retain-intersection'"
        cls.parserTokenParaphases[cls.PATTERN] =  "'pattern'"
        cls.parserTokenParaphases[cls.SQL] =  "'sql'"
        cls.parserTokenParaphases[cls.METADATASQL] =  "'metadatasql'"
        cls.parserTokenParaphases[cls.PREVIOUS] =  "'prev'"
        cls.parserTokenParaphases[cls.PREVIOUSTAIL] =  "'prevtail'"
        cls.parserTokenParaphases[cls.PREVIOUSCOUNT] =  "'prevcount'"
        cls.parserTokenParaphases[cls.PREVIOUSWINDOW] =  "'prevwindow'"
        cls.parserTokenParaphases[cls.PRIOR] =  "'prior'"
        cls.parserTokenParaphases[cls.EXISTS] =  "'exists'"
        cls.parserTokenParaphases[cls.WEEKDAY] =  "'weekday'"
        cls.parserTokenParaphases[cls.LW] =  "'lastweekday'"
        cls.parserTokenParaphases[cls.INSTANCEOF] =  "'instanceof'"
        cls.parserTokenParaphases[cls.TYPEOF] =  "'typeof'"
        cls.parserTokenParaphases[cls.CAST] =  "'cast'"
        cls.parserTokenParaphases[cls.CURRENT_TIMESTAMP] =  "'current_timestamp'"
        cls.parserTokenParaphases[cls.DELETE] =  "'delete'"
        cls.parserTokenParaphases[cls.DISTINCT] =  "'distinct'"
        cls.parserTokenParaphases[cls.SNAPSHOT] =  "'snapshot'"
        cls.parserTokenParaphases[cls.SET] =  "'set'"
        cls.parserTokenParaphases[cls.VARIABLE] =  "'variable'"
        cls.parserTokenParaphases[cls.TABLE] =  "'table'"
        cls.parserTokenParaphases[cls.INDEX] =  "'index'"
        cls.parserTokenParaphases[cls.UNTIL] =  "'until'"
        cls.parserTokenParaphases[cls.AT] =  "'at'"
        cls.parserTokenParaphases[cls.TIMEPERIOD_YEAR] =  "'year'"
        cls.parserTokenParaphases[cls.TIMEPERIOD_YEARS] =  "'years'"
        cls.parserTokenParaphases[cls.TIMEPERIOD_MONTH] =  "'month'"
        cls.parserTokenParaphases[cls.TIMEPERIOD_MONTHS] =  "'months'"
        cls.parserTokenParaphases[cls.TIMEPERIOD_WEEK] =  "'week'"
        cls.parserTokenParaphases[cls.TIMEPERIOD_WEEKS] =  "'weeks'"
        cls.parserTokenParaphases[cls.TIMEPERIOD_DAY] =  "'day'"
        cls.parserTokenParaphases[cls.TIMEPERIOD_DAYS] =  "'days'"
        cls.parserTokenParaphases[cls.TIMEPERIOD_HOUR] =  "'hour'"
        cls.parserTokenParaphases[cls.TIMEPERIOD_HOURS] =  "'hours'"
        cls.parserTokenParaphases[cls.TIMEPERIOD_MINUTE] =  "'minute'"
        cls.parserTokenParaphases[cls.TIMEPERIOD_MINUTES] =  "'minutes'"
        cls.parserTokenParaphases[cls.TIMEPERIOD_SEC] =  "'sec'"
        cls.parserTokenParaphases[cls.TIMEPERIOD_SECOND] =  "'second'"
        cls.parserTokenParaphases[cls.TIMEPERIOD_SECONDS] =  "'seconds'"
        cls.parserTokenParaphases[cls.TIMEPERIOD_MILLISEC] =  "'msec'"
        cls.parserTokenParaphases[cls.TIMEPERIOD_MILLISECOND] =  "'millisecond'"
        cls.parserTokenParaphases[cls.TIMEPERIOD_MILLISECONDS] =  "'milliseconds'"
        cls.parserTokenParaphases[cls.TIMEPERIOD_MICROSEC] =  "'usec'"
        cls.parserTokenParaphases[cls.TIMEPERIOD_MICROSECOND] =  "'microsecond'"
        cls.parserTokenParaphases[cls.TIMEPERIOD_MICROSECONDS] =  "'microseconds'"
        cls.parserTokenParaphases[cls.BOOLEAN_TRUE] =  "'true'"
        cls.parserTokenParaphases[cls.BOOLEAN_FALSE] =  "'false'"
        cls.parserTokenParaphases[cls.VALUE_NULL] =  "'null'"
        cls.parserTokenParaphases[cls.ROW_LIMIT_EXPR] =  "'limit'"
        cls.parserTokenParaphases[cls.OFFSET] =  "'offset'"
        cls.parserTokenParaphases[cls.UPDATE] =  "'update'"
        cls.parserTokenParaphases[cls.MATCH_RECOGNIZE] =  "'match_recognize'"
        cls.parserTokenParaphases[cls.MEASURES] =  "'measures'"
        cls.parserTokenParaphases[cls.DEFINE] =  "'define'"
        cls.parserTokenParaphases[cls.PARTITION] =  "'partition'"
        cls.parserTokenParaphases[cls.MATCHES] =  "'matches'"
        cls.parserTokenParaphases[cls.AFTER] =  "'after'"
        cls.parserTokenParaphases[cls.FOR] =  "'for'"
        cls.parserTokenParaphases[cls.WHILE] =  "'while'"
        cls.parserTokenParaphases[cls.MERGE] =  "'merge'"
        cls.parserTokenParaphases[cls.MATCHED] =  "'matched'"
        cls.parserTokenParaphases[cls.CONTEXT] =  "'context'"
        cls.parserTokenParaphases[cls.START] =  "'start'"
        cls.parserTokenParaphases[cls.END] =  "'end'"
        cls.parserTokenParaphases[cls.INITIATED] =  "'initiated'"
        cls.parserTokenParaphases[cls.TERMINATED] =  "'terminated'"
        cls.parserTokenParaphases[cls.USING] =  "'using'"
        cls.parserTokenParaphases[cls.EXPRESSIONDECL] =  "'expression'"
        cls.parserTokenParaphases[cls.NEWKW] =  "'new'"
        cls.parserTokenParaphases[cls.DATAFLOW] =  "'dataflow'"
        cls.parserTokenParaphases[cls.VALUES] =  "'values'"
        cls.parserTokenParaphases[cls.CUBE] =  "'cube'"
        cls.parserTokenParaphases[cls.ROLLUP] =  "'rollup'"
        cls.parserTokenParaphases[cls.GROUPING] =  "'grouping'"
        cls.parserTokenParaphases[cls.GROUPING_ID] =  "'grouping_id'"
        cls.parserTokenParaphases[cls.SETS] =  "'sets'"

        cls.parserKeywordSet = set(cls.parserTokenParaphases.values())
    return cls.parserTokenParaphases

@classmethod
def getAfterScriptTokens(cls):
    if not cls.afterScriptTokens:
        cls.afterScriptTokens.add(cls.CREATE)
        cls.afterScriptTokens.add(cls.EXPRESSIONDECL)
        cls.afterScriptTokens.add(cls.SELECT)
        cls.afterScriptTokens.add(cls.INSERT)
        cls.afterScriptTokens.add(cls.ON)
        cls.afterScriptTokens.add(cls.DELETE)
        cls.afterScriptTokens.add(cls.UPDATE)
        cls.afterScriptTokens.add(cls.ATCHAR)
    return cls.afterScriptTokens
}

//----------------------------------------------------------------------------
// Start Rules
//----------------------------------------------------------------------------
startPatternExpressionRule : (annotationEnum | expressionDecl)* patternExpression EOF;
	
startEPLExpressionRule : (annotationEnum | expressionDecl)* eplExpression EOF;

startEventPropertyRule : eventProperty EOF;

startJsonValueRule : jsonvalue EOF;

//----------------------------------------------------------------------------
// Expression Declaration
//----------------------------------------------------------------------------
expressionDecl : EXPRESSIONDECL classIdentifier? (array=LBRACK RBRACK)? expressionDialect? name=IDENT (LPAREN columnList? RPAREN)? (alias=IDENT FOR)? expressionDef;

expressionDialect : d=IDENT COLON;
	
expressionDef :	LCURLY expressionLambdaDecl? expression RCURLY 		
		| LBRACK stringconstant RBRACK 
		;

expressionLambdaDecl : (i=IDENT | (LPAREN columnList RPAREN)) (GOES | FOLLOWED_BY);

//----------------------------------------------------------------------------
// Annotations
//----------------------------------------------------------------------------
annotationEnum : ATCHAR classIdentifier ( '(' ( elementValuePairsEnum | elementValueEnum )? ')' )?;
    
elementValuePairsEnum : elementValuePairEnum (COMMA elementValuePairEnum)*;
    
elementValuePairEnum : keywordAllowedIdent '=' elementValueEnum;
    
elementValueEnum : annotationEnum
		| elementValueArrayEnum 
		| constant
		| v=IDENT
		| classIdentifier
    		;

elementValueArrayEnum : '{' (elementValueEnum (',' elementValueEnum)*)? (',')? '}';
    
//----------------------------------------------------------------------------
// EPL expression
//----------------------------------------------------------------------------
eplExpression : contextExpr? 
		(selectExpr
		| createWindowExpr
		| createIndexExpr
		| createVariableExpr
		| createTableExpr
		| createSchemaExpr
		| createContextExpr
		| createExpressionExpr
		| onExpr
		| updateExpr
		| createDataflow
		| fafDelete
		| fafUpdate
		| fafInsert) forExpr?
		;
	
contextExpr : CONTEXT i=IDENT;
	
selectExpr :    (INTO intoTableExpr)?
		(INSERT insertIntoExpr)? 
		SELECT selectClause
		(FROM fromClause)?
		matchRecog?
		(WHERE whereClause)?
		(GROUP BY groupByListExpr)? 
		(HAVING havingClause)?
		(OUTPUT outputLimit)?
		(ORDER BY orderByListExpr)?
		(ROW_LIMIT_EXPR rowLimit)?
		;
	
onExpr : ON onStreamExpr
	(onDeleteExpr | onSelectExpr (onSelectInsertExpr+ outputClauseInsert?)? | onSetExpr | onUpdateExpr | onMergeExpr)
	;
	
onStreamExpr : (eventFilterExpression | patternInclusionExpression) (AS i=IDENT | i=IDENT)?;

updateExpr : UPDATE ISTREAM updateDetails;
	
updateDetails :	classIdentifier (AS i=IDENT | i=IDENT)? SET onSetAssignmentList (WHERE whereClause)?;

onMergeExpr : MERGE INTO? n=IDENT (AS i=IDENT | i=IDENT)? (WHERE whereClause)? mergeItem+;

mergeItem : (mergeMatched | mergeUnmatched);
	
mergeMatched : WHEN MATCHED (AND_EXPR expression)? mergeMatchedItem+;

mergeMatchedItem : THEN (
		  ( u=UPDATE SET onSetAssignmentList) (WHERE whereClause)?
		  | d=DELETE (WHERE whereClause)? 
		  | mergeInsert
		  )
		  ;		

mergeUnmatched : WHEN NOT_EXPR MATCHED (AND_EXPR expression)? mergeUnmatchedItem+;
	
mergeUnmatchedItem : THEN mergeInsert;		
	
mergeInsert : INSERT (INTO classIdentifier)? (LPAREN columnList RPAREN)? SELECT selectionList (WHERE whereClause)?;
	
onSelectExpr	
@init  {paraphrases.append("on-select clause")}
@after {paraphrases.pop()}
		: (INSERT insertIntoExpr)?		
		SELECT (AND_EXPR? d=DELETE)? DISTINCT? selectionList
		onExprFrom?
		(WHERE whereClause)?		
		(GROUP BY groupByListExpr)?
		(HAVING havingClause)?
		(ORDER BY orderByListExpr)?
		(ROW_LIMIT_EXPR rowLimit)?
		;
	
onUpdateExpr	
@init  {paraphrases.append("on-update clause")}
@after {paraphrases.pop()}
		: UPDATE n=IDENT (AS i=IDENT | i=IDENT)? SET onSetAssignmentList (WHERE whereClause)?;

onSelectInsertExpr
@init  {paraphrases.append("on-select-insert clause")}
@after {paraphrases.pop()}
		: INSERT insertIntoExpr SELECT selectionList onSelectInsertFromClause? (WHERE whereClause)?;
	
onSelectInsertFromClause
		: FROM propertyExpression (AS i=IDENT | i=IDENT)?;

outputClauseInsert : OUTPUT (f=FIRST | a=ALL);
	
onDeleteExpr	
@init  {paraphrases.append("on-delete clause")}
@after {paraphrases.pop()}
		: DELETE onExprFrom (WHERE whereClause)?;
	
onSetExpr
@init  {paraphrases.append("on-set clause")}
@after {paraphrases.pop()}
		: SET onSetAssignmentList;

onSetAssignmentList : onSetAssignment (COMMA onSetAssignment)*;
	
onSetAssignment : eventProperty EQUALS expression | expression;
		
onExprFrom : FROM n=IDENT (AS i=IDENT | i=IDENT)?;

createWindowExpr : CREATE WINDOW i=IDENT viewExpressions? (ru=RETAINUNION|ri=RETAININTERSECTION)? AS? 
		  (
		  	createWindowExprModelAfter		  
		  |   	LPAREN createColumnList RPAREN
		  )		
		  (i1=INSERT (WHERE expression)? )?;

createWindowExprModelAfter : (SELECT createSelectionList FROM)? classIdentifier;
		
createIndexExpr : CREATE (u=IDENT)? INDEX n=IDENT ON w=IDENT LPAREN createIndexColumnList RPAREN;
	
createIndexColumnList : createIndexColumn (COMMA createIndexColumn)*;	

createIndexColumn : c=IDENT t=IDENT?;	

createVariableExpr : CREATE c=IDENT? VARIABLE classIdentifier (arr=LBRACK p=IDENT? RBRACK)? n=IDENT (EQUALS expression)?;

createTableExpr : CREATE TABLE n=IDENT AS? LPAREN createTableColumnList RPAREN; 

createTableColumnList : createTableColumn (COMMA createTableColumn)*;

createTableColumn : n=IDENT (createTableColumnPlain | builtinFunc | libFunction) p=IDENT? k=IDENT? (propertyExpressionAnnotation | annotationEnum)*;

createTableColumnPlain : classIdentifier (b=LBRACK p=IDENT? RBRACK)?;

createColumnList 	
@init  {paraphrases.append("column list")}
@after {paraphrases.pop()}
		: createColumnListElement (COMMA createColumnListElement)*;
	
createColumnListElement : classIdentifier (VALUE_NULL | (classIdentifier (b=LBRACK p=IDENT? RBRACK)?)) ;

createSelectionList 	
@init  {paraphrases.append("select clause")}
@after {paraphrases.pop()}
		: createSelectionListElement (COMMA createSelectionListElement)* ;

createSelectionListElement : s=STAR
			     | eventProperty (AS i=IDENT)?
			     | constant AS i=IDENT;

createSchemaExpr : CREATE keyword=IDENT? createSchemaDef;

createSchemaDef : SCHEMA name=IDENT AS? 
		  (
			variantList
		  |   	LPAREN createColumnList? RPAREN 
		  ) createSchemaQual*;

fafDelete : DELETE FROM classIdentifier (AS i=IDENT | i=IDENT)? (WHERE whereClause)?;

fafUpdate : UPDATE updateDetails;

fafInsert : INSERT insertIntoExpr VALUES LPAREN expressionList RPAREN;

createDataflow : CREATE DATAFLOW name=IDENT AS? gopList;
	
gopList : gop gop*;
	
gop : annotationEnum* (opName=IDENT | s=SELECT) gopParams? gopOut? LCURLY gopDetail? COMMA? RCURLY
                | createSchemaExpr COMMA;	
	
gopParams : LPAREN gopParamsItemList RPAREN;
	
gopParamsItemList : gopParamsItem (COMMA gopParamsItem)*;
		
gopParamsItem :	(n=classIdentifier | gopParamsItemMany) gopParamsItemAs?;

gopParamsItemMany : LPAREN classIdentifier (COMMA classIdentifier) RPAREN;

gopParamsItemAs : AS a=IDENT;

gopOut : FOLLOWED_BY gopOutItem (COMMA gopOutItem)*;

gopOutItem : n=classIdentifier gopOutTypeList?;
	
gopOutTypeList : LT gopOutTypeParam (COMMA gopOutTypeParam)* GT;	

gopOutTypeParam : (gopOutTypeItem | q=QUESTION);

gopOutTypeItem : classIdentifier gopOutTypeList?;

gopDetail : gopConfig (COMMA gopConfig)*;

gopConfig : SELECT (COLON|EQUALS) LPAREN selectExpr RPAREN
                | n=IDENT (COLON|EQUALS) (expression | jsonobject | jsonarray);

createContextExpr : CREATE CONTEXT name=IDENT AS? createContextDetail;
	
createExpressionExpr : CREATE expressionDecl;

createContextDetail : createContextChoice
                | contextContextNested COMMA contextContextNested (COMMA contextContextNested)*;
	
contextContextNested : CONTEXT name=IDENT AS? createContextChoice;
	
createContextChoice : START (ATCHAR i=IDENT | r1=createContextRangePoint) (END r2=createContextRangePoint)?
		| INITIATED (BY)? createContextDistinct? (ATCHAR i=IDENT AND_EXPR)? r1=createContextRangePoint (TERMINATED (BY)? r2=createContextRangePoint)?
		| PARTITION (BY)? createContextPartitionItem (COMMA createContextPartitionItem)* 
		| createContextGroupItem (COMMA createContextGroupItem)* FROM eventFilterExpression
		| COALESCE (BY)? createContextCoalesceItem (COMMA createContextCoalesceItem)* g=IDENT number (p=IDENT)?;
	
createContextDistinct :	DISTINCT LPAREN expressionList? RPAREN;
	
createContextRangePoint : createContextFilter 
                | patternInclusionExpression (ATCHAR i=IDENT)?
                | crontabLimitParameterSet
                | AFTER timePeriod;
		
createContextFilter : eventFilterExpression (AS? i=IDENT)?;

createContextPartitionItem : eventProperty ((AND_EXPR|COMMA) eventProperty)* FROM eventFilterExpression;
	
createContextCoalesceItem : libFunctionNoClass FROM eventFilterExpression;

createContextGroupItem : GROUP BY? expression AS i=IDENT;	

createSchemaQual : i=IDENT columnList;

variantList : variantListElement (COMMA variantListElement)*;

variantListElement : STAR 
                | classIdentifier;


intoTableExpr
@init  {paraphrases.append("into-table clause")}
@after {paraphrases.pop()}
		: TABLE i=IDENT;

insertIntoExpr
@init  {paraphrases.append("insert-into clause")}
@after {paraphrases.pop()}
		: (i=ISTREAM | r=RSTREAM | ir=IRSTREAM)? INTO classIdentifier (LPAREN columnList? RPAREN)?;
		
columnList : IDENT (COMMA IDENT)*;
	
fromClause 
@init  {paraphrases.append("from clause")}
@after {paraphrases.pop()}
		: streamExpression (regularJoin | outerJoinList);
	
regularJoin : (COMMA streamExpression)*;
	
outerJoinList :	outerJoin (outerJoin)*;

outerJoin
@init  {paraphrases.append("outer join")}
@after {paraphrases.pop()}
		: (
	          ((tl=LEFT|tr=RIGHT|tf=FULL) OUTER)? 
	          | (i=INNER)
	        ) JOIN streamExpression outerJoinIdent?;

outerJoinIdent : ON outerJoinIdentPair (AND_EXPR outerJoinIdentPair)*;
	
outerJoinIdentPair : eventProperty EQUALS eventProperty ;

whereClause
@init  {paraphrases.append("where clause")}
@after {paraphrases.pop()}
		: evalOrExpression;
	
selectClause
@init  {paraphrases.append("select clause")}
@after {paraphrases.pop()}
		: (s=RSTREAM | s=ISTREAM | s=IRSTREAM)? d=DISTINCT? selectionList;

selectionList :	selectionListElement (COMMA selectionListElement)*;

selectionListElement : s=STAR
                | streamSelector
                | selectionListElementExpr;
	
selectionListElementExpr : expression selectionListElementAnno? (AS? keywordAllowedIdent)?;

selectionListElementAnno : ATCHAR i=IDENT;
	
streamSelector : s=IDENT DOT STAR (AS i=IDENT)?;
	
streamExpression : (eventFilterExpression | patternInclusionExpression | databaseJoinExpression | methodJoinExpression )
		viewExpressions? (AS i=IDENT | i=IDENT)? (u=UNIDIRECTIONAL)? (ru=RETAINUNION|ri=RETAININTERSECTION)?;
		
forExpr : FOR i=IDENT (LPAREN expressionList? RPAREN)?;


patternInclusionExpression : PATTERN annotationEnum* LBRACK patternExpression RBRACK;
	
databaseJoinExpression
@init  {paraphrases.append("relational data join")}
@after {paraphrases.pop()}
		: SQL COLON i=IDENT LBRACK (s=STRING_LITERAL | s=QUOTED_STRING_LITERAL) (METADATASQL (s2=STRING_LITERAL | s2=QUOTED_STRING_LITERAL))? RBRACK;	
	
methodJoinExpression
@init  {paraphrases.append("method invocation join")}
@after {paraphrases.pop()}
    		: i=IDENT COLON classIdentifier (LPAREN expressionList? RPAREN)?;

viewExpressions 
@init  {paraphrases.append("view specifications")}
@after {paraphrases.pop()}
		: (DOT viewExpressionWNamespace (DOT viewExpressionWNamespace)*) 
		| (HASHCHAR viewExpressionOptNamespace (HASHCHAR viewExpressionOptNamespace)*);

viewExpressionWNamespace : ns=IDENT COLON viewWParameters;

viewExpressionOptNamespace : (ns=IDENT COLON)? viewWParameters;

viewWParameters : (i=IDENT|m=MERGE) (LPAREN expressionWithTimeList? RPAREN)?;

groupByListExpr
@init  {paraphrases.append("group-by clause")}
@after {paraphrases.pop()}
		: groupByListChoice (COMMA groupByListChoice)*;

groupByListChoice : e1=expression | groupByCubeOrRollup | groupByGroupingSets;

groupByCubeOrRollup : (CUBE | ROLLUP) LPAREN groupByCombinableExpr (COMMA groupByCombinableExpr)* RPAREN;

groupByGroupingSets : GROUPING SETS LPAREN groupBySetsChoice (COMMA groupBySetsChoice)* RPAREN;

groupBySetsChoice : groupByCubeOrRollup | groupByCombinableExpr;
		
groupByCombinableExpr : e1=expression | LPAREN (expression (COMMA expression)*)? RPAREN;

orderByListExpr
@init  {paraphrases.append("order by clause")}
@after {paraphrases.pop()}
		: orderByListElement (COMMA orderByListElement)*;

orderByListElement
		: expression (a=ASC|d=DESC)?;

havingClause
@init  {paraphrases.append("having clause")}
@after {paraphrases.pop()}
		: evalOrExpression;

outputLimit
@init  {paraphrases.append("output rate clause")}
@after {paraphrases.pop()}
		: outputLimitAfter?
 	       (k=ALL|k=FIRST|k=LAST|k=SNAPSHOT)? 
	        (
	          ( ev=EVERY_EXPR 
		    ( 
		      timePeriod
		    | (number | i=IDENT) (e=EVENTS)
		    )
		  )
		  |
		  ( at=AT crontabLimitParameterSet)
		  |
		  ( wh=WHEN expression (THEN onSetExpr)? )
		  |
		  ( t=WHEN TERMINATED (AND_EXPR expression)? (THEN onSetExpr)? )
		  |
	        ) 
	        outputLimitAndTerm?;
	
outputLimitAndTerm : AND_EXPR WHEN TERMINATED (AND_EXPR expression)? (THEN onSetExpr)?;

outputLimitAfter : a=AFTER (timePeriod | number EVENTS);	

rowLimit
@init  {paraphrases.append("row limit clause")}
@after {paraphrases.pop()}
		: (n1=numberconstant | i1=IDENT) ((c=COMMA | o=OFFSET) (n2=numberconstant | i2=IDENT))?;	

crontabLimitParameterSet : LPAREN expressionWithTimeList RPAREN;			

whenClause : (WHEN expression THEN expression);

elseClause : (ELSE expression);

//----------------------------------------------------------------------------
// Match recognize
//----------------------------------------------------------------------------
//
// Lowest precedence is listed first, order is (highest to lowest):  
// Single-character-ERE duplication * + ? {m,n}
// Concatenation
// Anchoring ^ $
// Alternation  |
//
matchRecog : MATCH_RECOGNIZE LPAREN matchRecogPartitionBy? matchRecogMeasures matchRecogMatchesSelection? matchRecogMatchesAfterSkip? matchRecogPattern 
		matchRecogMatchesInterval? matchRecogDefine? RPAREN ;

matchRecogPartitionBy : PARTITION BY expression (COMMA expression)*;		
		
matchRecogMeasures : MEASURES matchRecogMeasureItem (COMMA matchRecogMeasureItem)*;
	
matchRecogMeasureItem : expression (AS (i=IDENT)? )?;
	
matchRecogMatchesSelection : ALL MATCHES;
		
matchRecogPattern : PATTERN LPAREN matchRecogPatternAlteration RPAREN;
	
matchRecogMatchesAfterSkip : AFTER i1=keywordAllowedIdent i2=keywordAllowedIdent i3=keywordAllowedIdent i4=keywordAllowedIdent i5=keywordAllowedIdent;

matchRecogMatchesInterval : i=IDENT timePeriod (OR_EXPR t=TERMINATED)?;
		
matchRecogPatternAlteration : matchRecogPatternConcat (o=BOR matchRecogPatternConcat)*;	

matchRecogPatternConcat : matchRecogPatternUnary+;	

matchRecogPatternUnary : matchRecogPatternPermute | matchRecogPatternNested | matchRecogPatternAtom;

matchRecogPatternNested : LPAREN matchRecogPatternAlteration RPAREN (s=STAR | s=PLUS | s=QUESTION)? matchRecogPatternRepeat?;

matchRecogPatternPermute : MATCH_RECOGNIZE_PERMUTE LPAREN matchRecogPatternAlteration (COMMA matchRecogPatternAlteration)* RPAREN;
		
matchRecogPatternAtom :	i=IDENT ((s=STAR | s=PLUS | s=QUESTION) (reluctant=QUESTION)? )? matchRecogPatternRepeat?;
	
matchRecogPatternRepeat : LCURLY e1=expression? comma=COMMA? e2=expression? RCURLY;

matchRecogDefine : DEFINE matchRecogDefineItem (COMMA matchRecogDefineItem)*;	

matchRecogDefineItem : i=IDENT AS expression;	

//----------------------------------------------------------------------------
// Expression
//----------------------------------------------------------------------------
expression : caseExpression;

caseExpression : {paraphrases.append("case expression"); }  CASE whenClause+ elseClause? END {paraphrases.pop()}
		| {paraphrases.append("case expression"); }  CASE expression whenClause+ elseClause? END {paraphrases.pop()}
		| evalOrExpression;

evalOrExpression : evalAndExpression (op=OR_EXPR evalAndExpression)*;

evalAndExpression : bitWiseExpression (op=AND_EXPR bitWiseExpression)*;

bitWiseExpression : negatedExpression ( (BAND|BOR|BXOR) negatedExpression)* ;		

negatedExpression : evalEqualsExpression 
		| NOT_EXPR evalEqualsExpression;		

evalEqualsExpression : evalRelationalExpression ( 
			    (eq=EQUALS
			      |  is=IS
			      |  isnot=IS NOT_EXPR
			      |  sqlne=SQL_NE
			      |  ne=NOT_EQUAL
			     ) 
		       (
			evalRelationalExpression
			|  (a=ANY | a=SOME | a=ALL) ( (LPAREN expressionList? RPAREN) | subSelectGroupExpression )
		       )
		     )*;

evalRelationalExpression : concatenationExpr ( 
			( 
			  ( 
			    (r=LT|r=GT|r=LE|r=GE) 
			    	(
			    	  concatenationExpr
			    	  | (g=ANY | g=SOME | g=ALL) ( (LPAREN expressionList? RPAREN) | subSelectGroupExpression )
			    	)
			    	
			  )*
			)  
			| (n=NOT_EXPR)? 
			(
				// Represent the optional NOT prefix using the token type by
				// testing 'n' and setting the token type accordingly.
				(in=IN_SET
					  (l=LPAREN | l=LBRACK) expression	// brackets are for inclusive/exclusive
						(
							( col=COLON (expression) )		// range
							|
							( (COMMA expression)* )		// list of values
						)
					  (r=RPAREN | r=RBRACK)	
					)
				| inset=IN_SET inSubSelectQuery
				| between=BETWEEN betweenList
				| like=LIKE concatenationExpr (ESCAPE stringconstant)?
				| regex=REGEXP concatenationExpr
			)	
		);
	
inSubSelectQuery : subQueryExpr;
			
concatenationExpr : additiveExpression ( c=LOR additiveExpression ( LOR additiveExpression)* )?;

additiveExpression : multiplyExpression ( (PLUS|MINUS) multiplyExpression )*;

multiplyExpression : unaryExpression ( (STAR|DIV|MOD) unaryExpression )*;
	
unaryExpression : MINUS eventProperty
		| constant
		| substitutionCanChain
		| inner=LPAREN expression RPAREN chainedFunction?
		| builtinFunc
		| eventPropertyOrLibFunction
		| arrayExpression
		| rowSubSelectExpression 
		| existsSubSelectExpression
		| NEWKW LCURLY newAssign (COMMA newAssign)* RCURLY
		| NEWKW classIdentifier LPAREN (expression (COMMA expression)*)? RPAREN chainedFunction?
		| b=IDENT LBRACK expression (COMMA expression)* RBRACK chainedFunction?
		| jsonobject
		;

substitutionCanChain : substitution chainedFunction?;
	
chainedFunction : d=DOT libFunctionNoClass (d=DOT libFunctionNoClass)*;
	
newAssign : eventProperty (EQUALS expression)?;
	
rowSubSelectExpression : subQueryExpr chainedFunction?;

subSelectGroupExpression : subQueryExpr;

existsSubSelectExpression : EXISTS subQueryExpr;

subQueryExpr 
@init  {paraphrases.append("subquery")}
@after {paraphrases.pop()}
		: LPAREN  SELECT DISTINCT? selectionList FROM subSelectFilterExpr (WHERE whereClause)? (GROUP BY groupByListExpr)? RPAREN;
	
subSelectFilterExpr
@init  {paraphrases.append("subquery filter specification")}
@after {paraphrases.pop()}
		: eventFilterExpression viewExpressions? (AS i=IDENT | i=IDENT)? (ru=RETAINUNION|ri=RETAININTERSECTION)?;
		
arrayExpression : LCURLY (expression (COMMA expression)* )? RCURLY chainedFunction?;

builtinFunc : SUM LPAREN (ALL | DISTINCT)? expressionListWithNamed RPAREN   			#builtin_sum
		| AVG LPAREN (ALL | DISTINCT)? expressionListWithNamed RPAREN			#builtin_avg
		| COUNT LPAREN (a=ALL | d=DISTINCT)? expressionListWithNamed RPAREN		#builtin_cnt
		| MEDIAN LPAREN (ALL | DISTINCT)? expressionListWithNamed RPAREN		#builtin_median
		| STDDEV LPAREN (ALL | DISTINCT)? expressionListWithNamed RPAREN		#builtin_stddev
		| AVEDEV LPAREN (ALL | DISTINCT)? expressionListWithNamed RPAREN		#builtin_avedev
		| firstLastWindowAggregation							#builtin_firstlastwindow
		| COALESCE LPAREN expression COMMA expression (COMMA expression)* RPAREN	#builtin_coalesce
		| PREVIOUS LPAREN expression (COMMA expression)? RPAREN chainedFunction?	#builtin_prev
		| PREVIOUSTAIL LPAREN expression (COMMA expression)? RPAREN chainedFunction?	#builtin_prevtail
		| PREVIOUSCOUNT LPAREN expression RPAREN					#builtin_prevcount
		| PREVIOUSWINDOW LPAREN expression RPAREN chainedFunction?			#builtin_prevwindow
		| PRIOR LPAREN expression COMMA eventProperty RPAREN				#builtin_prior
		| GROUPING LPAREN expression RPAREN						#builtin_grouping
		| GROUPING_ID LPAREN expressionList RPAREN					#builtin_groupingid
		// MIN and MAX can also be "Math.min" static function and "min(price)" aggregation function and "min(a, b, c...)" built-in function
		// therefore handled in code via libFunction as below
		| INSTANCEOF LPAREN expression COMMA classIdentifier (COMMA classIdentifier)* RPAREN	#builtin_instanceof
		| TYPEOF LPAREN expression RPAREN							#builtin_typeof
		| CAST LPAREN expression (COMMA | AS) classIdentifier (COMMA expressionNamedParameter)? RPAREN chainedFunction?	#builtin_cast
		| EXISTS LPAREN eventProperty RPAREN						#builtin_exists
		| CURRENT_TIMESTAMP (LPAREN RPAREN)? chainedFunction?				#builtin_currts
		| ISTREAM LPAREN RPAREN								#builtin_istream
		;
	
firstLastWindowAggregation : (q=FIRST | q=LAST | q=WINDOW) LPAREN expressionListWithNamed? RPAREN chainedFunction?;
		
eventPropertyOrLibFunction : eventProperty | libFunction;
	
libFunction: libFunctionWithClass (DOT libFunctionNoClass)*;
				
libFunctionWithClass : ((classIdentifier DOT funcIdentInner) | funcIdentTop) (l=LPAREN libFunctionArgs? RPAREN)?;

libFunctionNoClass : funcIdentChained (l=LPAREN libFunctionArgs? RPAREN)?;	

funcIdentTop : escapableIdent
		| MAX 
		| MIN;

funcIdentInner : escapableIdent
		| LAST 
		| FIRST
		| WINDOW;

funcIdentChained : escapableIdent
		| LAST 
		| FIRST
		| WINDOW
		| MAX 
		| MIN 
		| WHERE 
		| SET 
		| AFTER 
		| BETWEEN;
	
libFunctionArgs : (ALL | DISTINCT)? libFunctionArgItem (COMMA libFunctionArgItem)*;
	
libFunctionArgItem : expressionLambdaDecl? expressionWithNamed;

betweenList : concatenationExpr AND_EXPR concatenationExpr;

//----------------------------------------------------------------------------
// Pattern event expressions / event pattern operators
//   Operators are: followed-by (->), or, and, not, every, where
//   Lowest precedence is listed first, order is (lowest to highest):  ->, or, and, not/every, within.
//   On the atomic level an expression has filters, and observer-statements.
//----------------------------------------------------------------------------
patternExpression
@init  {paraphrases.append("pattern expression")}
@after {paraphrases.pop()}
		: followedByExpression;

followedByExpression : orExpression (followedByRepeat)*;
	
followedByRepeat : (f=FOLLOWED_BY | (g=FOLLOWMAX_BEGIN expression FOLLOWMAX_END)) orExpression;
	
orExpression : andExpression (o=OR_EXPR andExpression)*;

andExpression :	matchUntilExpression (a=AND_EXPR matchUntilExpression)*;

matchUntilExpression : (r=matchUntilRange)? qualifyExpression (UNTIL until=qualifyExpression)?;

qualifyExpression : ((e=EVERY_EXPR | n=NOT_EXPR | d=EVERY_DISTINCT_EXPR distinctExpressionList) matchUntilRange? )? guardPostFix;

guardPostFix : (atomicExpression | l=LPAREN patternExpression RPAREN) ((wh=WHERE guardWhereExpression) | (wi=WHILE guardWhileExpression))?;
		
distinctExpressionList : LPAREN distinctExpressionAtom (COMMA distinctExpressionAtom)* RPAREN;

distinctExpressionAtom : expressionWithTime;

atomicExpression : observerExpression | patternFilterExpression;
		
observerExpression : ns=IDENT COLON (nm=IDENT | a=AT) LPAREN expressionListWithNamedWithTime? RPAREN;

guardWhereExpression : IDENT COLON IDENT LPAREN (expressionWithTimeList)? RPAREN;
	
guardWhileExpression : LPAREN expression RPAREN;

// syntax is [a:b] or [:b] or [a:] or [a]
matchUntilRange : LBRACK ( low=expression (c1=COLON high=expression?)? | c2=COLON upper=expression) RBRACK;
	
//----------------------------------------------------------------------------
// Filter expressions
//   Operators are the usual bunch =, <, >, =<, >= 
//	 Ranges such as 'property in [a,b]' are allowed and ([ and )] distinguish open/closed range endpoints
//----------------------------------------------------------------------------
eventFilterExpression
@init  {paraphrases.append("filter specification")}
@after {paraphrases.pop()}
    :   (i=IDENT EQUALS)? classIdentifier (LPAREN expressionList? RPAREN)? propertyExpression?;
    
propertyExpression : propertyExpressionAtomic (propertyExpressionAtomic)*;

propertyExpressionAtomic : LBRACK propertyExpressionSelect? expression propertyExpressionAnnotation? (AS n=IDENT)? (WHERE where=expression)? RBRACK;
       	
propertyExpressionSelect : SELECT propertySelectionList FROM;
		
propertyExpressionAnnotation : ATCHAR n=IDENT (LPAREN v=IDENT RPAREN);
	
propertySelectionList : propertySelectionListElement (COMMA propertySelectionListElement)*;

propertySelectionListElement : s=STAR
		| propertyStreamSelector
		| expression (AS keywordAllowedIdent)?;
	
propertyStreamSelector : s=IDENT DOT STAR (AS i=IDENT)?;

patternFilterExpression
@init  {paraphrases.append("filter specification")}
@after {paraphrases.pop()}
    		: (i=IDENT EQUALS)? classIdentifier (LPAREN expressionList? RPAREN)? propertyExpression? patternFilterAnnotation?;
       	
patternFilterAnnotation : ATCHAR i=IDENT (LPAREN number RPAREN)?;

classIdentifier : i1=escapableStr (DOT i2=escapableStr)*;
		
slashIdentifier : (d=DIV)? i1=escapableStr (DIV i2=escapableStr)*;

expressionListWithNamed : expressionWithNamed (COMMA expressionWithNamed)*;

expressionListWithNamedWithTime : expressionWithNamedWithTime (COMMA expressionWithNamedWithTime)*;

expressionWithNamed : expressionNamedParameter | expressionWithTime;

expressionWithNamedWithTime : expressionNamedParameterWithTime | expressionWithTimeInclLast;

expressionNamedParameter : IDENT COLON (expression | LPAREN expressionList? RPAREN);

expressionNamedParameterWithTime : IDENT COLON (expressionWithTime | LPAREN expressionWithTimeList? RPAREN);

expressionList : expression (COMMA expression)*;
   	
expressionWithTimeList : expressionWithTimeInclLast (COMMA expressionWithTimeInclLast)*;

expressionWithTime : lastWeekdayOperand
		| timePeriod
		| expressionQualifyable
		| rangeOperand
		| frequencyOperand
		| lastOperator
		| weekDayOperator
		| numericParameterList
		| STAR
		| propertyStreamSelector
		;

expressionWithTimeInclLast : lastOperand
		| expressionWithTime
		;

expressionQualifyable : expression (a=ASC|d=DESC|s=TIMEPERIOD_SECONDS|s=TIMEPERIOD_SECOND|s=TIMEPERIOD_SEC)?;
		
lastWeekdayOperand : LW;
	
lastOperand : LAST;

frequencyOperand : STAR DIV (number|i=IDENT|substitution);

rangeOperand : (n1=number|i1=IDENT|s1=substitution) COLON (n2=number|i2=IDENT|s2=substitution);

lastOperator : (number|i=IDENT|substitution) LAST;

weekDayOperator : (number|i=IDENT|substitution) WEEKDAY;

numericParameterList : LBRACK numericListParameter (COMMA numericListParameter)* RBRACK;

numericListParameter : rangeOperand
		| frequencyOperand
		| numberconstant;
	    
eventProperty : eventPropertyAtomic (DOT eventPropertyAtomic)*;
	
eventPropertyAtomic : eventPropertyIdent (
			lb=LBRACK ni=number RBRACK (q=QUESTION)?
			|
			lp=LPAREN (s=STRING_LITERAL | s=QUOTED_STRING_LITERAL) RPAREN (q=QUESTION)?
			|
			q1=QUESTION 
			)?;
		
eventPropertyIdent : ipi=keywordAllowedIdent (ESCAPECHAR DOT ipi2=keywordAllowedIdent?)*;
	
keywordAllowedIdent : i1=IDENT
		| i2=TICKED_STRING_LITERAL
		| AT
		| COUNT
		| ESCAPE
    		| EVERY_EXPR
		| SCHEMA
		| SUM
		| AVG
		| MAX
		| MIN
		| COALESCE
		| MEDIAN
		| STDDEV
		| AVEDEV
		| EVENTS
		| FIRST
		| LAST
		| WHILE
		| MERGE
		| MATCHED
		| UNIDIRECTIONAL
		| RETAINUNION
		| RETAININTERSECTION
		| UNTIL
		| PATTERN
		| SQL
		| METADATASQL
		| PREVIOUS
		| PREVIOUSTAIL
		| PRIOR
		| WEEKDAY
		| LW
		| INSTANCEOF
		| TYPEOF
		| CAST
		| SNAPSHOT
		| VARIABLE
		| TABLE
		| INDEX
		| WINDOW
		| LEFT
		| RIGHT
		| OUTER
		| FULL
		| JOIN
		| DEFINE
		| PARTITION
		| MATCHES
		| CONTEXT
		| FOR
		| USING;
		
escapableStr : i1=IDENT | i2=EVENTS | i3=TICKED_STRING_LITERAL;
	
escapableIdent : IDENT | t=TICKED_STRING_LITERAL;

timePeriod : (	yearPart monthPart? weekPart? dayPart? hourPart? minutePart? secondPart? millisecondPart? microsecondPart?
		| monthPart weekPart? dayPart? hourPart? minutePart? secondPart? millisecondPart? microsecondPart?
		| weekPart dayPart? hourPart? minutePart? secondPart? millisecondPart? microsecondPart?
		| dayPart hourPart? minutePart? secondPart? millisecondPart? microsecondPart?
		| hourPart minutePart? secondPart? millisecondPart? microsecondPart?
		| minutePart secondPart? millisecondPart? microsecondPart?
		| secondPart millisecondPart? microsecondPart?
		| millisecondPart microsecondPart?
		| microsecondPart 
		);

yearPart : (numberconstant|i=IDENT|substitution) (TIMEPERIOD_YEARS | TIMEPERIOD_YEAR);

monthPart : (numberconstant|i=IDENT|substitution) (TIMEPERIOD_MONTHS | TIMEPERIOD_MONTH);

weekPart : (numberconstant|i=IDENT|substitution) (TIMEPERIOD_WEEKS | TIMEPERIOD_WEEK);

dayPart : (numberconstant|i=IDENT|substitution) (TIMEPERIOD_DAYS | TIMEPERIOD_DAY);

hourPart : (numberconstant|i=IDENT|substitution) (TIMEPERIOD_HOURS | TIMEPERIOD_HOUR);

minutePart : (numberconstant|i=IDENT|substitution) (TIMEPERIOD_MINUTES | TIMEPERIOD_MINUTE | MIN);
	
secondPart : (numberconstant|i=IDENT|substitution) (TIMEPERIOD_SECONDS | TIMEPERIOD_SECOND | TIMEPERIOD_SEC);
	
millisecondPart : (numberconstant|i=IDENT|substitution) (TIMEPERIOD_MILLISECONDS | TIMEPERIOD_MILLISECOND | TIMEPERIOD_MILLISEC);
	
microsecondPart : (numberconstant|i=IDENT|substitution) (TIMEPERIOD_MICROSECONDS | TIMEPERIOD_MICROSECOND | TIMEPERIOD_MICROSEC);

number : IntegerLiteral | FloatingPointLiteral;

substitution : q=QUESTION (COLON slashIdentifier)?;
	
constant : numberconstant
		| stringconstant
		| t=BOOLEAN_TRUE 
		| f=BOOLEAN_FALSE 
		| nu=VALUE_NULL;

numberconstant : (m=MINUS | p=PLUS)? number;

stringconstant : sl=STRING_LITERAL
		| qsl=QUOTED_STRING_LITERAL;

//----------------------------------------------------------------------------
// JSON
//----------------------------------------------------------------------------
jsonvalue : constant 
		| jsonobject
		| jsonarray;

jsonobject : LCURLY jsonmembers RCURLY;
	
jsonarray : LBRACK jsonelements? RBRACK;

jsonelements : jsonvalue (COMMA jsonvalue)* (COMMA)?;
	
jsonmembers : jsonpair (COMMA jsonpair)* (COMMA)?;
	 
jsonpair : (stringconstant | keywordAllowedIdent) COLON jsonvalue;

//----------------------------------------------------------------------------
// LEXER
//----------------------------------------------------------------------------

// Tokens
CREATE:'create';
WINDOW:'window';
IN_SET:'in';
BETWEEN:'between';
LIKE:'like';
REGEXP:'regexp';
ESCAPE:'escape';
OR_EXPR:'or';
AND_EXPR:'and';
NOT_EXPR:'not';
EVERY_EXPR:'every';
EVERY_DISTINCT_EXPR:'every-distinct';
WHERE:'where';
AS:'as';	
SUM:'sum';
AVG:'avg';
MAX:'max';
MIN:'min';
COALESCE:'coalesce';
MEDIAN:'median';
STDDEV:'stddev';
AVEDEV:'avedev';
COUNT:'count';
SELECT:'select';
CASE:'case';
ELSE:'else';
WHEN:'when';
THEN:'then';
END:'end';
FROM:'from';
OUTER:'outer';
INNER:'inner';
JOIN:'join';
LEFT:'left';
RIGHT:'right';
FULL:'full';
ON:'on';	
IS:'is';
BY:'by';
GROUP:'group';
HAVING:'having';
DISTINCT:'distinct';
ALL:'all';
ANY:'any';
SOME:'some';
OUTPUT:'output';
EVENTS:'events';
FIRST:'first';
LAST:'last';
INSERT:'insert';
INTO:'into';
VALUES:'values';
ORDER:'order';
ASC:'asc';
DESC:'desc';
RSTREAM:'rstream';
ISTREAM:'istream';
IRSTREAM:'irstream';
SCHEMA:'schema';
UNIDIRECTIONAL:'unidirectional';
RETAINUNION:'retain-union';
RETAININTERSECTION:'retain-intersection';
PATTERN:'pattern';
SQL:'sql';
METADATASQL:'metadatasql';
PREVIOUS:'prev';
PREVIOUSTAIL:'prevtail';
PREVIOUSCOUNT:'prevcount';
PREVIOUSWINDOW:'prevwindow';
PRIOR:'prior';
EXISTS:'exists';
WEEKDAY:'weekday';
LW:'lastweekday';
INSTANCEOF:'instanceof';
TYPEOF:'typeof';
CAST:'cast';
CURRENT_TIMESTAMP:'current_timestamp';
DELETE:'delete';
SNAPSHOT:'snapshot';
SET:'set';
VARIABLE:'variable';
TABLE:'table';
UNTIL:'until';
AT:'at';
INDEX:'index';
TIMEPERIOD_YEAR:'year';
TIMEPERIOD_YEARS:'years';
TIMEPERIOD_MONTH:'month';
TIMEPERIOD_MONTHS:'months';
TIMEPERIOD_WEEK:'week';
TIMEPERIOD_WEEKS:'weeks';
TIMEPERIOD_DAY:'day';
TIMEPERIOD_DAYS:'days';
TIMEPERIOD_HOUR:'hour';
TIMEPERIOD_HOURS:'hours';
TIMEPERIOD_MINUTE:'minute';
TIMEPERIOD_MINUTES:'minutes';
TIMEPERIOD_SEC:'sec';
TIMEPERIOD_SECOND:'second';
TIMEPERIOD_SECONDS:'seconds';	
TIMEPERIOD_MILLISEC:'msec';
TIMEPERIOD_MILLISECOND:'millisecond';
TIMEPERIOD_MILLISECONDS:'milliseconds';
TIMEPERIOD_MICROSEC:'usec';
TIMEPERIOD_MICROSECOND:'microsecond';
TIMEPERIOD_MICROSECONDS:'microseconds';
BOOLEAN_TRUE:'true';
BOOLEAN_FALSE:'false';
VALUE_NULL:'null';
ROW_LIMIT_EXPR:'limit';
OFFSET:'offset';
UPDATE:'update';
MATCH_RECOGNIZE:'match_recognize';
MATCH_RECOGNIZE_PERMUTE:'match_recognize_permute';
MEASURES:'measures';
DEFINE:'define';
PARTITION:'partition';
MATCHES:'matches';
AFTER:'after';	
FOR:'for';	
WHILE:'while';	
USING:'using';
MERGE:'merge';
MATCHED:'matched';
EXPRESSIONDECL:'expression';
NEWKW:'new';
START:'start';
CONTEXT:'context';
INITIATED:'initiated';
TERMINATED:'terminated';
DATAFLOW:'dataflow';
CUBE:'cube';
ROLLUP:'rollup';
GROUPING:'grouping';
GROUPING_ID:'grouping_id';
SETS:'sets';

// Operators
FOLLOWMAX_BEGIN : '-[';
FOLLOWMAX_END   : ']>';
FOLLOWED_BY 	: '->';
GOES 		: '=>';
EQUALS 		: '=';
SQL_NE 		: '<>';
QUESTION 	: '?';
LPAREN 		: '(';
RPAREN 		: ')';
LBRACK 		: '[';
RBRACK 		: ']';
LCURLY 		: '{';
RCURLY 		: '}';
COLON 		: ':';
COMMA 		: ',';
EQUAL 		: '==';
LNOT 		: '!';
BNOT 		: '~';
NOT_EQUAL 	: '!=';
DIV 		: '/';
DIV_ASSIGN 	: '/=';
PLUS 		: '+';
PLUS_ASSIGN	: '+=';
INC 		: '++';
MINUS 		: '-';
MINUS_ASSIGN 	: '-=';
DEC 		: '--';
STAR 		: '*';
STAR_ASSIGN 	: '*=';
MOD 		: '%';
MOD_ASSIGN 	: '%=';
GE 		: '>=';
GT 		: '>';
LE 		: '<=';
LT 		: '<';
BXOR 		: '^';
BXOR_ASSIGN 	: '^=';
BOR		: '|';
BOR_ASSIGN 	: '|=';
LOR		: '||';
BAND 		: '&';
BAND_ASSIGN 	: '&=';
LAND 		: '&&';
SEMI 		: ';';
DOT 		: '.';
NUM_LONG	: '\u18FF';  // assign bogus unicode characters so the token exists
NUM_DOUBLE	: '\u18FE';
NUM_FLOAT	: '\u18FD';
ESCAPECHAR	: '\\';
ESCAPEBACKTICK	: '`';
ATCHAR		: '@';
HASHCHAR	: '#';

// Whitespace -- ignored
WS	:	(	' '
		|	'\t'
		|	'\f'
			// handle newlines
		|	(
				'\r'    // Macintosh
			|	'\n'    // Unix (the right way)
			)
		)+		
		-> channel(HIDDEN)
	;

// Single-line comments
SL_COMMENT
	:	'//'
		(~('\n'|'\r'))* ('\n'|'\r'('\n')?)?
		-> channel(HIDDEN)
	;

// multiple-line comments
ML_COMMENT
    	:   	'/*' (.)*? '*/'
		-> channel(HIDDEN)
    	;

TICKED_STRING_LITERAL
    :   '`' ( EscapeSequence | ~('`'|'\\') )* '`'
    ;

QUOTED_STRING_LITERAL
    :   '\'' ( EscapeSequence | ~('\''|'\\') )* '\''
    ;

STRING_LITERAL
    :  '"' ( EscapeSequence | ~('\\'|'"') )* '"'
    ;

fragment
EscapeSequence	:	'\\'
		(	'n'
		|	'r'
		|	't'
		|	'b'
		|	'f'
		|	'"'
		|	'\''
		|	'\\'
		|	UnicodeEscape
		|	OctalEscape
		|	. // unknown, leave as it is
		)
    ;    

// an identifier.  Note that testLiterals is set to true!  This means
// that after we match the rule, we look in the literals table to see
// if it's a literal or really an identifer
IDENT	
	:	('a'..'z'|'_'|'$') ('a'..'z'|'_'|'0'..'9'|'$')*
	;

IntegerLiteral
    :   DecimalIntegerLiteral
    |   HexIntegerLiteral
    |   OctalIntegerLiteral
    |   BinaryIntegerLiteral
    ;
 
FloatingPointLiteral
    :   DecimalFloatingPointLiteral
    |   HexadecimalFloatingPointLiteral
    ;

fragment
OctalEscape
    :   '\\' ('0'..'3') ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7')
    ;

fragment
UnicodeEscape
    :   '\\' 'u' HexDigit HexDigit HexDigit HexDigit
    ;
    
fragment
DecimalIntegerLiteral
    :   DecimalNumeral IntegerTypeSuffix?
    ;

fragment
HexIntegerLiteral
    :   HexNumeral IntegerTypeSuffix?
    ;

fragment
OctalIntegerLiteral
    :   OctalNumeral IntegerTypeSuffix?
    ;

fragment
BinaryIntegerLiteral
    :   BinaryNumeral IntegerTypeSuffix?
    ;

fragment
IntegerTypeSuffix
    :   [lL]
    ;

fragment
DecimalNumeral
    :   '0'
    |   ('0')* NonZeroDigit (Digits? | Underscores Digits)
    ;

fragment
Digits
    :   Digit (DigitOrUnderscore* Digit)?
    ;

fragment
Digit
    :   '0'
    |   NonZeroDigit
    ;

fragment
NonZeroDigit
    :   [1-9]
    ;

fragment
DigitOrUnderscore
    :   Digit
    |   '_'
    ;

fragment
Underscores
    :   '_'+
    ;

fragment
HexNumeral
    :   '0' [xX] HexDigits
    ;

fragment
HexDigits
    :   HexDigit (HexDigitOrUnderscore* HexDigit)?
    ;

fragment
HexDigit
    :   [0-9a-fA-F]
    ;

fragment
HexDigitOrUnderscore
    :   HexDigit
    |   '_'
    ;

fragment
OctalNumeral
    :   '0' Underscores? OctalDigits
    ;

fragment
OctalDigits
    :   OctalDigit (OctalDigitOrUnderscore* OctalDigit)?
    ;

fragment
OctalDigit
    :   [0-7]
    ;

fragment
OctalDigitOrUnderscore
    :   OctalDigit
    |   '_'
    ;

fragment
BinaryNumeral
    :   '0' [bB] BinaryDigits
    ;

fragment
BinaryDigits
    :   BinaryDigit (BinaryDigitOrUnderscore* BinaryDigit)?
    ;

fragment
BinaryDigit
    :   [01]
    ;

fragment
BinaryDigitOrUnderscore
    :   BinaryDigit
    |   '_'
    ;

fragment
DecimalFloatingPointLiteral
    :   Digits '.' Digits? ExponentPart? FloatTypeSuffix?
    |   '.' Digits ExponentPart? FloatTypeSuffix?
    |   Digits ExponentPart FloatTypeSuffix?
    |   Digits FloatTypeSuffix
    ;

fragment
ExponentPart
    :   ExponentIndicator SignedInteger
    ;

fragment
ExponentIndicator
    :   [eE]
    ;

fragment
SignedInteger
    :   Sign? Digits
    ;

fragment
Sign
    :   [+-]
    ;

fragment
FloatTypeSuffix
    :   [fFdD]
    ;

fragment
HexadecimalFloatingPointLiteral
    :   HexSignificand BinaryExponent FloatTypeSuffix?
    ;

fragment
HexSignificand
    :   HexNumeral '.'?
    |   '0' [xX] HexDigits? '.' HexDigits
    ;

fragment
BinaryExponent
    :   BinaryExponentIndicator SignedInteger
    ;

fragment
BinaryExponentIndicator
    :   [pP]
    ;    
