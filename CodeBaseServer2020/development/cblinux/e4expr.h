/* e4expr.h   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#define   E4MAX_PARMS          3
#define   E4MAX_STACK_ENTRIES 65
#define   E4MAX_CALC_NAME     65

struct EXPR4CALCSt;

/******************************************************************************/

/* This structure is wrapped around an EXPR4CALC calculation structure to create
   a total.  Note:  a total structure is then wrapped by a total output object
   */

typedef struct TOTAL4st
{
   LINK4 link ;
            /* A total is a calculation for easy removal/name changes */
   S4CONV( struct EXPR4CALCSt *calcPtr, struct EXPR4CALCSt *calc_ptr ) ;

   /* These two members are used to keep track of when the total should be reset
      each time the reset expression is evaluated it's value is compared to the
      value stored in lastResetValue, if they are different the total is reset
      and the stored value is updated */
   S4CONV( EXPR4 *resetExpression, EXPR4 *reset_expression ) ;
   S4CONV( char  *lastResetValue, char  *last_reset_value ) ;

   /* normally a total is 'incremented' appropriately for every record in the
      relation set.  In some cases you only want a subset of these records to
      apply to the total.  The addCondition and lastAddValue function
      similarly to the reset condition but selectively enable or disable
      updating the total for the current record */
   S4CONV( EXPR4 *addCondition, EXPR4 *add_condition ) ;
   S4CONV( char  *lastAddValue, char  *last_add_value ) ;
   short logcon;      /* flag which indicates that the conditional total is
                         based on a logical condition, not a changed value */
   short donce;

#ifndef S4OFF_REPORT
   struct _OBJECTStruct *obj; /* report object associated with the total */
   struct REPORT4St *report;  /* report containing the total */
#endif

   S4CONV( short totalType, short total_type ) ;
                      /* total4lowest, total4highest, total4sum, */
                      /* total4count, total4average */

   short  lookahead;

   double total ;     /* Running Total */
   double low ;       /* Lowest Value */
   double high ;      /* Hightest Value */
   long count;        /* used internally for computing the average */
} TOTAL4 ;

/******************************************************************************/

typedef struct
{
   void   (S4PTR *functionPtr)(void) ;  /*void needed for SCO */
   char   S4PTR *name ;
   short  code ;
   unsigned char   nameLen ;
   char   priority ;
   char   returnType ;
   #ifdef S4UNIX
      signed char  numParms ;  /* -1 means that the number is flexible */
   #else
      S4CONV( signed char numParms, signed char num_parms ) ;  /* -1 means that the number is flexible */
   #endif
   char   type[E4MAX_PARMS] ;   /* Parameter types */
}  E4FUNCTIONS ;

typedef struct
{
   const unsigned char S4PTR *ptr ;     /* The original string */
   int pos, len ;      /* Source expression position and length */
} S4SCAN ;


#define EXTEND4OFF        0
#define EXTEND4AVAIL      1
#define EXTEND4ALLOCATED  2

typedef struct
{
   char S4PTR *ptr ;
   unsigned int pos, len ;
   // by default, use a fixed stack buffer for constants.  If this buffer is exceded and
   // doExtend is set to EXTEND4AVAIL (current default), then allocate memory for buffer
   // and set doExtend to EXTEND4ALLOCATED
   int doExtend ;           // set to EXTEND4OFF, EXTEND4AVAIL, OR EXTEND4ALLOCATED
   CODE4 S4PTR *codeBase ;
} S4STACK ;

typedef struct
{
   EXPR4 expr ;
   S4STACK constants ;
   S4SCAN scan ;          /* Character Expression */
   S4STACK op ;            /* Operation stack */
   CODE4 S4PTR *codeBase ;
} E4PARSE ;

typedef struct EXPR4CALCSt
{
   LINK4 link ;
   EXPR4 S4PTR *expr ;
   TOTAL4 *total ; /* If calculation is for a total. */
   char name[E4MAX_CALC_NAME] ;
   int curResultPos ;
} EXPR4CALC ;

extern char S4PTR *expr4buf ;  /* Pointer to CODE4 Working memory */
/* expr4buf_len no longer exists... */ /* extern unsigned expr4buf_len ;*/ /* Length, e4execute() assumes length long enough */

extern char S4PTR * S4PTR *expr4 ;          /* Expression Parms; Points to next parm to be placed */

extern EXPR4  S4PTR *expr4ptr ;     /* Points to expression being evaluated */
extern E4INFO S4PTR *expr4infoPtr ;
extern char   S4PTR *expr4constants ; /* Points to constant info */

// AS Mar 10/08 - added to support dateTime type
#define EXPR4NUM_FUNCTIONS 164
#if !defined( OLEDB5BUILD ) || defined( S4SERVER ) || defined( S4JOINT_OLEDB_DLL )
   extern const E4FUNCTIONS v4functions[EXPR4NUM_FUNCTIONS] ;
#endif

/* AS Aug 22/01 - Moved these to d4defs.h since they are exposed to user, and need for dual dlls */

/* POSITIONAL CODES FOR EXPRESSION FUNCTIONS */
#define  E4LAST_FIELD       E4FIELD_MEMO
#define  E4FIRST_LOG        (E4LAST_FIELD + 3)  /* Range of Logical Operators */
#define  E4LAST_LOG         (E4FIRST_LOG + 5)
#define  E4FIRST_OPERATOR   (E4LAST_LOG + 1)  /* Range of Other Operators */
#define  E4LAST_OPERATOR    (E4FIRST_OPERATOR + 52)
#define  E4COMPARE_START    (E4FIRST_OPERATOR + 12)
#define  E4COMPARE_END      (E4COMPARE_START + 35)
#define  E4FIRST_FUNCTION   (E4COMPARE_END + 6)  /* Start of the List of Functions */

/* Codes for Immediate Data in Compile String */
#define E4FIELD_STR        0
#define E4FIELD_WSTR       (E4FIELD_STR + 1)
#define E4FIELD_WSTR_LEN   (E4FIELD_WSTR + 1)
#define E4FIELD_STR_CAT    (E4FIELD_WSTR_LEN + 1)
#define E4FIELD_WSTR_CAT   (E4FIELD_STR_CAT + 1)
#define E4FIELD_LOG        (E4FIELD_WSTR_CAT + 1)
#define E4FIELD_DATE_D     (E4FIELD_LOG + 1)
#define E4FIELD_DATE_S     (E4FIELD_DATE_D + 1)
#define E4FIELD_NUM_D      (E4FIELD_DATE_S + 1)
#define E4FIELD_NUM_S      (E4FIELD_NUM_D + 1)
#define E4FIELD_CUR        (E4FIELD_NUM_S + 1)
#define E4FIELD_DOUB       (E4FIELD_CUR + 1)
#define E4FIELD_INT        (E4FIELD_DOUB + 1)
#define E4FIELD_UNS_INT    (E4FIELD_INT + 1)
#define E4FIELD_SHORT      (E4FIELD_UNS_INT + 1)
#define E4FIELD_UNS_SHORT  (E4FIELD_SHORT + 1)
#define E4FIELD_DTTIME     (E4FIELD_UNS_SHORT + 1)
#define E4FIELD_INT_D      (E4FIELD_DTTIME + 1)
#define E4FIELD_CUR_D      (E4FIELD_INT_D + 1)
#define E4FIELD_I8         (E4FIELD_CUR_D + 1)
#define E4FIELD_DBDATE     (E4FIELD_I8 + 1)
#define E4FIELD_DBTIME     (E4FIELD_DBDATE + 1)
#define E4FIELD_DBTIMESTAMP (E4FIELD_DBTIME + 1)

#define E4FIELD_MEMO       (23)          /* HPUX aCC support */
#if E4FIELD_MEMO != E4FIELD_DBTIMESTAMP + 1
   #error number functions in array mismatch(1)
#endif

#define E4DOUBLE           (E4FIELD_MEMO + 1)
#define E4STRING           (E4DOUBLE + 1)
// AS 04/20/00 needed to examine these directly somneimes
//#define E4LOG_LOW          (E4STRING + 1)
//#define E4LOG_HIGH         (E4LOG_LOW + 3)
#define E4TRUE             (E4STRING + 1)
#define E4FALSE            (E4TRUE + 2)
#define E4NOT              (E4FALSE + 2)
#define E4OR               (E4NOT + 2)
#define E4AND              (E4OR + 1)
#define E4CONCATENATE      (E4AND + 1)
#define E4CONCAT_TRIM      (E4CONCATENATE + 3)

#define E4CONCAT_TWO       (E4CONCAT_TRIM + 4)
#define E4NOT_EQUAL        (E4CONCAT_TWO + 4)
#define E4GREATER_EQ       (E4NOT_EQUAL + 7)
#define E4LESS_EQ          (E4GREATER_EQ + 6)
#define E4EQUAL            (E4LESS_EQ + 6)
#define E4GREATER          (E4EQUAL + 6)
#define E4LESS             (E4GREATER + 5)

#define E4CHR              (E4FIELD_MEMO + 62)
/*#define E4CHR            (E4LESS + 10)*/
#if E4CHR != E4LESS + 10
   #error number functions in array mismatch(2)
#endif

#define E4DEL              (E4CHR + 1)
#define E4STR              (E4DEL + 1)
#define E4STRZERO          (E4STR + 2)
#define E4SUBSTR           (E4STRZERO + 1)
#define E4TIME             (E4SUBSTR + 1)
#define E4UPPER            (E4TIME + 1)
#define E4DTOS             (E4UPPER + 1)
// AS Mar 10/08 - added to support dateTime type
#define E4DTOC             (E4DTOS + 3)
#define E4SPACE            (E4DTOC + 2)
#define E4TRIM             (E4SPACE + 1)
#define E4LTRIM            (E4TRIM + 1)
#define E4ALLTRIM          (E4LTRIM + 1)
#define E4LEFT             (E4ALLTRIM + 1)
#define E4RIGHT            (E4LEFT + 1)
#define E4PADL             (E4RIGHT + 1)
#define E4PADR             (E4PADL + 1)
#define E4IIF              (E4PADR + 1)

// AS Mar 10/08 - added to support dateTime type
#define E4DATETIME         E4CHR + 25
#define E4STOD             E4DATETIME + 1
/*#define E4STOD           (E4IIF + 4)*/
#if E4STOD != E4IIF + 5
   #error number functions in array mismatch(3)
#endif

#define E4CTOD             (E4STOD + 1)
#define E4DELETED          (E4CTOD + 8)
//#define E4NOT_DELETED      (E4DELETED + 1)
#define E4RECCOUNT         (E4DELETED + 1)
#define E4RECNO            (E4RECCOUNT + 1)
#define E4L2BIN            (E4RECNO + 2)
#define E4CALC_FUNCTION    (E4L2BIN + 1)
#define E4TOTAL            (E4CALC_FUNCTION + 1)
#define E4DESCEND          (E4TOTAL + 2)

#define E4NUM_DESCEND_FUNCTIONS 17
#define E4NUM_ASCEND_FUNCTIONS 18

#define E4ASCEND           (E4DESCEND + E4NUM_DESCEND_FUNCTIONS)

#if EXPR4NUM_FUNCTIONS != E4ASCEND + E4NUM_ASCEND_FUNCTIONS + 1
   #error number functions in array mismatch(4)
#endif

#define  E4DONE              -2
#define  E4NO_FUNCTION       -3
#define  E4COMMA             -4
#define  E4L_BRACKET         -5
#define  E4ANOTHER_PARM      -6

/* Interface Functions */
#ifdef __cplusplus
   extern "C" {
#endif
#ifndef S4CLIENT
   S4EXPORT int S4FUNCTION expr4context( EXPR4 *, DATA4 * ) ;
#endif

/* EXTERNAL FUNCTIONS : */

#define expr4data( e4 ) ( (e4)->data )
S4EXPORT DATA4 S4PTR* S4FUNCTION expr4dataCB( EXPR4 * ) ;
S4EXPORT double S4FUNCTION expr4double( EXPR4 S4PTR * ) ;
S4EXPORT int S4FUNCTION expr4double2( EXPR4 S4PTR *, double * ) ;
#define expr4free( e4 ) ( u4free( (e4)->exprWorkBuf ), ((e4)->exprWorkBuf = 0), u4free( e4 ), (e4) = 0 )
S4EXPORT void S4FUNCTION expr4freeCB( EXPR4 * ) ;
#define expr4len( e4 ) ( (e4)->len )
S4EXPORT long S4FUNCTION expr4lenCB( EXPR4 * ) ;  /* LY 99/08/12 : updated for Sandage fix */
S4EXPORT EXPR4 S4PTR *S4FUNCTION expr4parseLow( DATA4 S4PTR *, const char S4PTR *, TAG4FILE * ) ;
S4EXPORT S4CONST char S4PTR *S4FUNCTION expr4source( const EXPR4 S4PTR * ) ;
S4EXPORT const char S4PTR *S4FUNCTION expr4str( EXPR4 S4PTR * ) ;
S4EXPORT int S4FUNCTION expr4true( EXPR4 S4PTR * ) ;
#define expr4type( e4 ) ( (e4)->type )
S4EXPORT short S4FUNCTION expr4typeCB( EXPR4 * ) ;
S4EXPORT int S4FUNCTION expr4vary( EXPR4 S4PTR *, char S4PTR * S4PTR * ) ;
#ifndef S4INLINE
   S4EXPORT EXPR4 S4PTR *S4FUNCTION expr4parse( DATA4 S4PTR *, char S4PTR * ) ;
#endif
S4EXPORT const E4FUNCTIONS *S4FUNCTION e4functions();

/* Relate Module and Report Writer Functions */
S4EXPORT EXPR4CALC *S4FUNCTION code4calcCreate( CODE4 S4PTR *, EXPR4 S4PTR *, const char S4PTR * ) ;
S4EXPORT void S4FUNCTION code4calcReset( CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION expr4execute( EXPR4 S4PTR *, const int, void S4PTR * S4PTR * ) ;
S4EXPORT void S4FUNCTION expr4functions( const E4FUNCTIONS S4PTR * S4PTR *) ;
S4EXPORT EXPR4 S4PTR *S4FUNCTION expr4calcParse( DATA4 S4PTR *, const char S4PTR * ) ;
S4EXPORT void S4FUNCTION expr4calcDelete( CODE4 *c4, EXPR4CALC S4PTR * ) ;
S4EXPORT void S4FUNCTION expr4calcMassage( EXPR4CALC S4PTR * );
S4EXPORT EXPR4CALC *S4FUNCTION expr4calcLookup( CODE4 S4PTR *, DATA4 *d4, const char S4PTR *, const unsigned ) ;
S4EXPORT int S4FUNCTION expr4calcNameChange( EXPR4 S4PTR * S4PTR *, const char S4PTR *, const char S4PTR * ) ;
S4EXPORT void S4FUNCTION expr4calcResultPos( EXPR4CALC S4PTR *, const int ) ;
// AS Oct 10/01 - e4lookup() must be exported always since it is available externally and reqd. for build with def file
// #ifndef S4OFF_REPORT
S4EXPORT int S4FUNCTION e4lookup( const unsigned char S4PTR *, const int, const int, const int ) ;
// #endif

/* INTERNAL FUNCTIONS : */
int expr4start( EXPR4 * ) ;
int expr4reset( EXPR4 * ) ;
int expr4reset( EXPR4 * ) ;
S4EXPORT int S4FUNCTION expr4keyLen( EXPR4 S4PTR * ) ;
S4EXPORT int S4FUNCTION expr4keyLenFromType( int, int, CODE4 S4PTR * ) ;
#ifndef S4CLIENT
   S4EXPORT int S4FUNCTION expr4key( EXPR4 S4PTR *, char S4PTR * S4PTR *, TAG4FILE * ) ;
   int expr4keyConvert( EXPR4 *, char **, const int, const int, TAG4FILE * ) ;
#endif
S4EXPORT short S4FUNCTION expr4nullLow( const EXPR4 S4PTR *, const short ) ;
#define expr4null( e4 ) ( expr4nullLow( (e4), 1 ) )

/* terminator to indicate last entry of function array */
#define E4TERMINATOR -1

/* Parsing Functions */
int e4addConstant( E4PARSE *, const int, const void *, const unsigned int ) ;
E4INFO *e4functionAdd( EXPR4 *, const int ) ;
int e4getOperator( E4PARSE *, int * ) ;
int expr4parseExpr( E4PARSE * ) ;
int expr4parseFunction( E4PARSE *, const char *, const int ) ;
int expr4parseValue( E4PARSE * ) ;
int expr4trueCheck( E4PARSE * ) ;

#ifdef __cplusplus
   }
#endif

/* Execute Functions */
void e4add( void ) ;
void e4addDate( void ) ;
void e4alltrim( void ) ;
void e4and( void ) ;
void e4calcFunction( void ) ;
void e4calcTotal( void ) ;
void e4chr( void ) ;
void e4concatSpecial( char ) ;
void e4concatTrim( void ) ;
void e4concatTwo( void ) ;
void e4contain( void ) ;
void e4copyConstant( void ) ;
void e4copyParm( void ) ;
void e4ctod( void ) ;
void e4date( void ) ;
void e4day( void ) ;
void e4dayDoub( void ) ;
void e4del( void ) ;
void e4deleted( void ) ;
// void e4notDeleted( void ) ;
void e4divide( void ) ;
void e4dtoc( void ) ;
void e4dtocDoub( void ) ;
void e4dtosDoub( void ) ;
void e4dttos( void ) ;  // AS Mar 10/08 - added to support dateTime type
void e4dateTime( void ) ;
void e4equal( void ) ;
void e4equalCur( void ) ;
void e4equalDtTime( void ) ;
void e4false( void ) ;
void e4fieldCopy( void ) ;
// AS July 27/01 - field value is stored in intel ordering always, so need a special function to extract
// the value in local double ordering format
void e4fieldDoubD( void ) ;
void e4fieldDateD( void ) ;
void e4fieldLog( void ) ;
void e4fieldMemo( void ) ;
void e4fieldCurD( void ) ;
void e4fieldIntD( void ) ;
void e4fieldNumD( void ) ;
void e4greater( void ) ;
void e4greaterCur( void ) ;
void e4greaterDtTime( void ) ;
void e4greaterDoub( void ) ;
void e4greaterEq( void ) ;
void e4greaterEqCur( void ) ;
void e4greaterEqDtTime( void ) ;
void e4greaterEqDoub( void ) ;
void e4iif( void ) ;
void e4iifStr( void ) ;
void e4less( void ) ;
void e4lessCur( void ) ;
void e4lessDtTime( void ) ;
void e4lessDoub( void ) ;
void e4lessEq( void ) ;
void e4lessEqCur( void ) ;
void e4lessEqDtTime( void ) ;
void e4lessEqDoub( void ) ;
void e4ltrim( void ) ;
void e4month( void ) ;
void e4monthDoub( void ) ;
void e4multiply( void ) ;
void e4newFunction( void ) ;
void e4nop( void ) ;
void e4not( void ) ;
void e4notEqual( void ) ;
void e4notEqualCur( void ) ;
void e4notEqualDtTime( void ) ;
void e4or( void ) ;
void e4fieldAdd( void ) ;
void e4parmRemove( void ) ;
void e4wstrLenCon( void ) ;
void e4padLeft( void ) ;
void e4padRight( void ) ;
void e4power( void ) ;
void e4recCount( void ) ;
void e4recno( void ) ;
void e4stod( void ) ;
void e4str( void ) ;
S4EXPORT void e4strZero( void ) ;
S4EXPORT void e4wideToStr( void ) ;
void e4space( void ) ;
void e4sub( void ) ;
void e4subDate( void ) ;
void e4substr( void ) ;
void e4time( void ) ;
void e4trim( void ) ;
void expr4trueFunction( void ) ;
void e4upper( void ) ;
void e4val( void ) ;
#ifndef S4UNIX
   void e4l2bin( void ) ;
#endif
void e4year( void ) ;
void e4yearDoub( void ) ;
void e4pageno( void ) ;
void e4descend( void ) ;
#ifdef S4CLIPPER
   void e4descendBinary( void ) ;
#else
   #define e4descendBinary e4descend
#endif
void e4ascend( void ) ;
