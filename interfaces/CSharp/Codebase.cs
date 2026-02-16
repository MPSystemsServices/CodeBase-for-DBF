/* codebase.cs (c)Copyright Sequiter Software Inc., 1988-2002.  All rights reserved. */

// AS Mar 23/06 - resolve WinCE vs. regular code discrepencies by handling marshalling via defines
/*
#define MARSHAL4I4 [MarshalAs(UnmanagedType.I4)]
#define MARSHAL4I2 [MarshalAs(UnmanagedType.I2)]
#define MARSHAL4U1 [MarshalAs(UnmanagedType.U1)]
#define MARSHAL4U2 [MarshalAs(UnmanagedType.U2)]
#define MARSHAL4U4 [MarshalAs(UnmanagedType.U4)]
#define MARSHAL4R8 [MarshalAs(UnmanagedType.R8)]
#define MARSHAL4LPSTR [MarshalAs(UnmanagedType.LPStr)]
#define MARSHAL4LPWSTR [MarshalAs(UnmanagedType.LPWStr)]
#define MARSHAL4LPARRAY [MarshalAs(UnmanagedType.LPArray)]
#define MARSHAL4LPSTRUCT [MarshalAs(UnmanagedType.LPStruct)]
*/

namespace CodeBase
{
   #pragma warning disable 1591

   using System;
   using System.Runtime.InteropServices;
   using System.Text ;  /* LY 2002/09/24 : for StringBuilder */

   public class Error4
   {
      protected IntPtr code4 ;  // This is the most used member and is need in error handling.

      public const int cp0 = 0 ;   /* this is the same as cp437 (i.e. US oem sequence) */
      public const int cp437 = 1 ;    /* this is the same as cp0 (i.e. US oem sequence) */
      public const int cp1252 = 3 ;

      public const short collate4machine = 1 ;
      public const short collate4general = 1001 ;
      public const short collate4special = 1002 ;

      public const int r4success = 0 ;
      public const int r4found = 1 ;    /* Primary Key Match */
      public const int r4after = 2 ;
      public const int r4eof = 3 ;
      public const int r4bof = 4 ;
      public const int r4entry = 5 ;    /* No index file entry or no record (go) */
      public const int r4descending = 10 ;
      public const int r4unique = 20 ;    /* Key is not unique, do not write/append */
      public const int r4uniqueContinue = 25 ; /* Key is not unique, write/append anyway */
      public const int r4locked = 50 ;
      public const int r4noCreate = 60 ;    /* Could not create file */
      public const int r4noOpen = 70 ;    /* Could not open file */
      public const int r4noTag = 80 ;    /* DataIndex::seek, with no default tag */
      public const int r4terminate = 90 ;    /* no relation match with terminate set */
      public const int r4inactive = 110 ;    /* transactional state */
      public const int r4active = 120 ;    /* transactional state */
      public const int r4rollback = 130 ;    /* transactional state */
      public const int r4authorize  = 140 ;    /* lacks authorization rights to perform action */
      public const int r4connected = 150 ;
      public const int r4logOn = 160 ;
      public const int r4logOpen = 170 ;
      public const int r4logOff = 180 ;
      public const ushort r4null = 190 ;
      public const int r4autoIncrement = 195 ;
      public const int r4autoTimestamp = 200 ;
      public const int r4done = 210 ;

      public const int e4lock = -50 ;
      public const int e4fieldName = -210 ;
      public const int e4unique = -340 ;
      public const int e4info = -910 ;
      public const int e4parm = -930 ;
      public const int e4struct = -970 ;

      public const int INVALID4HANDLE = -1 ;

      public const int lock4read = 0 ;
      public const int lock4write = 1 ;
      public const int lock4any = 2 ;

      public const int LOG4ALWAYS = 2 ;
      public const int LOG4ON = 1 ;
      public const int LOG4TRANS = 0 ;

      public const int OPEN4DENY_NONE = 0 ;
      public const int OPEN4DENY_WRITE = 2 ;
      public const int OPEN4DENY_RW = 1 ;

      public const int OPT4EXCLUSIVE = -1 ;
      public const int OPT4OFF = 0 ;
      public const int OPT4ALL = 1 ;

      public const int relate4blank = 105 ;
      public const int relate4skipRec = 106 ;
      public const int relate4terminate = 107 ;
      public const int relate4exact = 108 ;
      public const int relate4scan = 109 ;
      public const int relate4approx = 110 ;

      public const char r4bin = 'B' ;
      public const char r4double = 'B' ;
      public const char r4str = 'C' ;
      public const char r4dateDoub = 'd' ;
      public const char r4date = 'D' ;
      public const char r4float = 'F' ;
      public const char r4gen = 'G' ;
      public const char r4int = 'I' ;
      public const char r4log = 'L' ;
      public const char r4memo = 'M' ;
      public const char r4numDoub = 'n' ;
      public const char r4num = 'N' ;
      public const char r4dateTime = 'T' ;
      public const char r4dateTimeMilli = '7' ;
      public const char r5wstr = 'W' ;
      public const char r4unicode = r5wstr ;
      public const char r4memoBin = 'X' ;
      public const char r4currency = 'Y' ;
      public const char r4charBin = 'Z' ;

      public const int sort4machine = 0 ;
      public const int sort4general = 1 ;

      public const short LOCK4OFF = 0 ;
      public const short LOCK4ALL = 1 ;
      public const short LOCK4DATA = 2 ;

      public const short WAIT4EVER = -1 ;

      /* using 2nd error numbers under !S4OFF_STRING */
      public const int E60523 = 60119 ;
      public const int E60525 = 60121 ;
      public const int E60601 = 60025 ;
      public const int E60603 = 60027 ;
      public const int E60281 = 60023 ;
      public const int E60282 = 60024 ;
      public const int E60201 = 60021 ;
      public const int E60982 = 60225 ;
      public const int E60983 = 60226 ;
      public const int E60984 = 60227 ;
      public const int E60991 = 60221 ;
      public const int E61203 = 60107 ;
      public const int E61204 = 60108 ;

      /* functions used by classes in addition to Code4 */
      [DllImport("c4dll.dll")]
      protected static extern int c4getLockEnforce(IntPtr code4 ) ;
      [DllImport("c4dll.dll")]
      protected static extern short code4accessMode(IntPtr code4, [MarshalAs(UnmanagedType.I4)] int accessMode);

      /* functions used by classes in addition to Data4 */
      [DllImport("c4dll.dll")]
      protected static extern short d4changed( IntPtr data4, [MarshalAs(UnmanagedType.I2)] short flag ) ;
      [DllImport("c4dll.dll")]
      protected static extern int d4recNoLow( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      protected static extern short d4numFields( IntPtr data4 ) ;

      /* functions used by classes Field4, Field4info, Field4memo */
      [DllImport("c4dll.dll")]
      protected static extern IntPtr d4fieldJ(IntPtr data4, [MarshalAs(UnmanagedType.I2)] short number);
      [DllImport("c4dll.dll")]
      protected static extern IntPtr d4field(IntPtr data4, [MarshalAs(UnmanagedType.LPStr)] string name);
      [DllImport("c4dll.dll")]
      protected static extern int f4decimals( IntPtr field4 ) ;
      [DllImport("c4dll.dll")]
      protected static extern uint f4len( IntPtr field4 ) ;
      [DllImport("c4dll.dll")]   /* LY 2002/09/24 : replaced f4name() (see Field4::name())*/
      protected static extern void f4nameW( IntPtr field4, System.Text.StringBuilder buff ) ;
      [DllImport("c4dll.dll")]
      protected static extern int f4null( IntPtr field4 ) ;
      [DllImport("c4dll.dll")]
      protected static extern char f4type( IntPtr field4 ) ;

      [DllImport("c4dll.dll")]
      protected static extern void u4freeDefault( int ptr ) ;

      [DllImport("c4dll.dll")]
      protected static extern int error4set( IntPtr code4, [MarshalAs(UnmanagedType.I4)] int errorCode ) ;
      [DllImport("c4dll.dll")]
      protected static extern int error4number2( IntPtr code4, [MarshalAs(UnmanagedType.I4)] int errorCode ) ;
      [DllImport("c4dll.dll")]
      protected static extern short code4errorCode( IntPtr code4, [MarshalAs(UnmanagedType.I2)] short errorCode ) ;
      [DllImport("c4dll.dll")]
      private static extern short error4VB( IntPtr code4, [MarshalAs(UnmanagedType.I2)] short errorCode ,[MarshalAs(UnmanagedType.I4)] int extraInfo ) ;
      [DllImport("c4dll.dll")]
      private static extern int error4describeDefault( IntPtr code4, [MarshalAs(UnmanagedType.I4)] int er, [MarshalAs(UnmanagedType.I4)] int er2,
         [MarshalAs(UnmanagedType.LPStr)] string p1, [MarshalAs(UnmanagedType.LPStr)] string p2, [MarshalAs(UnmanagedType.LPStr)] string p3) ;
      [DllImport("c4dll.dll")]
      private static extern int error4file( IntPtr code4, [MarshalAs(UnmanagedType.LPStr)] string fileName ,[MarshalAs(UnmanagedType.I4)] int overwrite ) ;
      [DllImport("c4dll.dll")]
      private static extern void error4exitTest( IntPtr code4 ) ;
      [DllImport("c4dll.dll")]   /* LY 2002/09/24 : changed from error4text() */
      private static extern void error4textW( IntPtr code4, [MarshalAs(UnmanagedType.I4)] int errCode, StringBuilder buff ) ;

      public int throwError( short errorCode, int extraInfo )
      {
         return error4VB( code4, errorCode, extraInfo) ;
      }
      public int error( int er, int er2, string p1, string p2, string p3 ) { return error4describeDefault( code4, er, er2, p1, p2, p3 ) ; }
      public int errorSet( int c ) { return error4set( code4, c ) ; }

      public int errorFile( string fileName, int overwrite ) { return error4file( code4, fileName, overwrite ); }
      public int errorNumber2(int errorCode) { return error4number2(code4, errorCode); }
      public void exitTest()    { error4exitTest( code4 ) ; }
      public string errorText( int c )
      {
         StringBuilder textBuff = new StringBuilder( 256 ) ;
         error4textW( code4, c, textBuff ) ;
         return textBuff.ToString() ;
      }
   }

   public class Expr4
   {
      private IntPtr expr;  //Contains a pointer to the EXPR4 structure in the CodeBase DLL

      public Expr4() { expr = IntPtr.Zero; }
      public Expr4( IntPtr ex )   { expr = ex ; }

      [DllImport("c4dll.dll")]
      private static extern IntPtr expr4parseLow( IntPtr data4,
         [MarshalAs(UnmanagedType.LPStr)] string expression,
         [MarshalAs(UnmanagedType.I4)] int tagFile
         ) ;
      [DllImport("c4dll.dll")]
      private static extern double expr4double( IntPtr expr ) ;
      [DllImport("c4dll.dll")]
      private static extern IntPtr expr4dataCB(IntPtr expr);
      [DllImport("c4dll.dll")]
      private static extern void expr4freeCB(IntPtr expr);
      [DllImport("c4dll.dll")]
      private static extern int expr4lenCB(IntPtr expr);
      [DllImport("c4dll.dll")]
      private static extern short expr4nullLow(IntPtr expr, [MarshalAs(UnmanagedType.I2)]  short forAdd);
      [DllImport("c4dll.dll")]   /* LY 2002/09/24 : replaced expr4source() */
      private static extern void expr4sourceW(IntPtr expr, StringBuilder buff);
      [DllImport("c4dll.dll")]   /* LY 2002/09/24 : replaced expr4str() */
      private static extern void expr4strW(IntPtr expr, StringBuilder buff);
      [DllImport("c4dll.dll")]
      private static extern short expr4typeCB(IntPtr expr);
      [DllImport("c4dll.dll")]
      private static extern int expr4true(IntPtr expr);

      public IntPtr EXPR4
      {
         get
         {
            return( expr );
         }
      }

      public Expr4( Data4 d, string expression )
      {
         expr = expr4parseLow( d.dat(), expression, 0 ) ;
      }
      public static explicit operator double( Expr4 a ) { return expr4double( a.expr ) ; }

      public Data4 data()
      {
         Data4 temp = new Data4( expr4dataCB( expr ) ) ;
         return temp ;
      }
      public void free() { expr4freeCB(expr); expr = IntPtr.Zero; }
      public int len()                    { return expr4lenCB( expr ) ; }
      public int parse( Data4 db, string expression )
      {
         int rc ;
         if ( db.dat() == IntPtr.Zero )
         {
            Error4 err = new Error4() ;
            rc = err.throwError( Error4.e4struct, Error4.E60603 ) ;
         }
         else if (expr != IntPtr.Zero)
         {
            Error4 err = new Error4() ;
            rc = err.throwError( Error4.e4info, Error4.E60601 ) ;
         }
         else
         {
            expr = expr4parseLow( db.dat(), expression, 0 ) ;
            rc = db.getErrorCode() ;
         }
         return rc ;
      }
      public int isValid() { return (expr != IntPtr.Zero ? 1 : 0); }
      public short isNull()               { return expr4nullLow( expr, 1 ) ; }
      public string source()
      {
         StringBuilder srcBuff = new StringBuilder( 512 ) ;
         expr4sourceW( expr, srcBuff ) ;
         return srcBuff.ToString() ;
      }
      public string str()
      {
         StringBuilder strBuff = new StringBuilder( this.len() + 1 ) ;
         strBuff.Append( ' ', this.len() ) ;
         expr4strW( expr, strBuff ) ;
         // LY Mar 3/05 : Do not pass Expr4::len() to ::ToString() (OutOfRange exception will occur if expression uses trim function and returned string is shorter than Expr4::len())
         return strBuff.ToString() ;
      }

      public double toDouble()                   { return expr4double( expr ) ; }
      public char type()                 { return (char) expr4typeCB( expr ) ; }
      public int isTrue()                 { return expr4true( expr ) ; }
   }

   public class Code4 : Error4
   {
      private short init_p ;

      //properties of the Code4 structure
      [DllImport("c4dll.dll")]
      private static extern int c4getCreateTemp(IntPtr code4);
      [DllImport("c4dll.dll")]
      private static extern int c4getErrCreate(IntPtr code4);
      [DllImport("c4dll.dll")]
      private static extern int c4getErrExpr(IntPtr code4);
      [DllImport("c4dll.dll")]
      private static extern int c4getErrFieldName(IntPtr code4);
      [DllImport("c4dll.dll")]
      private static extern int c4getErrGo(IntPtr code4);
      [DllImport("c4dll.dll")]
      private static extern int c4getErrOpen(IntPtr code4);
      [DllImport("c4dll.dll")]
      private static extern int c4getErrRelate(IntPtr code4);
      [DllImport("c4dll.dll")]
      private static extern int c4getErrSkip(IntPtr code4);
      [DllImport("c4dll.dll")]
      private static extern int c4getErrTagName(IntPtr code4);
      [DllImport("c4dll.dll")]
      private static extern int c4getFileFlush(IntPtr code4);
      [DllImport("c4dll.dll")]
      private static extern void c4setCreateTemp( IntPtr code4, [MarshalAs(UnmanagedType.I4)] int createTemp  ) ;
      [DllImport("c4dll.dll")]
      private static extern void c4setErrCreate( IntPtr code4, [MarshalAs(UnmanagedType.I4)] int errCreate ) ;
      [DllImport("c4dll.dll")]
      private static extern void c4setErrExpr( IntPtr code4, [MarshalAs(UnmanagedType.I4)] int errExpr ) ;
      [DllImport("c4dll.dll")]
      private static extern void c4setErrFieldName( IntPtr code4, [MarshalAs(UnmanagedType.I4)] int errFieldName ) ;
      [DllImport("c4dll.dll")]
      private static extern void c4setErrGo( IntPtr code4, [MarshalAs(UnmanagedType.I4)] int errGo ) ;
      [DllImport("c4dll.dll")]
      private static extern void c4setErrOpen( IntPtr code4, [MarshalAs(UnmanagedType.I4)] int errOpen ) ;
      [DllImport("c4dll.dll")]
      private static extern void c4setErrRelate( IntPtr code4, [MarshalAs(UnmanagedType.I4)] int errRelate ) ;
      [DllImport("c4dll.dll")]
      private static extern void c4setErrSkip( IntPtr code4, [MarshalAs(UnmanagedType.I4)] int errSkip ) ;
      [DllImport("c4dll.dll")]
      private static extern void c4setErrTagName( IntPtr code4, [MarshalAs(UnmanagedType.I4)] int errTagName ) ;
      [DllImport("c4dll.dll")]
      private static extern void c4setFileFlush( IntPtr code4, [MarshalAs(UnmanagedType.I4)] int fileFlush ) ;
      [DllImport("c4dll.dll")]
      private static extern void c4setLockEnforce( IntPtr code4, [MarshalAs(UnmanagedType.I4)] int lockEnforce ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4autoOpen( IntPtr code4, [MarshalAs(UnmanagedType.I4)] int openMode ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4codePage( IntPtr code4, [MarshalAs(UnmanagedType.I2)] short codepage ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4collate( IntPtr code4, [MarshalAs(UnmanagedType.I2)] short collation ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4collateUnicode( IntPtr code4, [MarshalAs(UnmanagedType.I2)] short collation ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4collatingSequence( IntPtr code4, [MarshalAs(UnmanagedType.I2)] short collation ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4compatibility( IntPtr code4, [MarshalAs(UnmanagedType.I2)] short version ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4errDefaultUnique( IntPtr code4, [MarshalAs(UnmanagedType.I2)] short errDefaultUnique ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4errOff( IntPtr code4, short mode ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4hWnd( IntPtr code4, [MarshalAs(UnmanagedType.I4)] int hWnd ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4indexBlockSize(IntPtr code4);
      [DllImport("c4dll.dll")]
      private static extern short code4indexBlockSizeSet( IntPtr code4, [MarshalAs(UnmanagedType.I2)] short blockSize ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4lockAttempts( IntPtr code4, [MarshalAs(UnmanagedType.I2)] short lockAttempts ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4lockAttemptsSingle( IntPtr code4, [MarshalAs(UnmanagedType.I2)] short lockAttemptsSingle ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4lockDelay( IntPtr code4, [MarshalAs(UnmanagedType.I4)] int lockDelay ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4log( IntPtr code4, [MarshalAs(UnmanagedType.I2)] short log ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4memExpandBlock( IntPtr code4, [MarshalAs(UnmanagedType.I2)] short memExpandBlock ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4memExpandData( IntPtr code4, [MarshalAs(UnmanagedType.I2)] short memExpandData ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4memExpandIndex( IntPtr code4, [MarshalAs(UnmanagedType.I2)] short memExpandIndex ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4memExpandLock( IntPtr code4, [MarshalAs(UnmanagedType.I2)] short memExpandLock ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4memExpandTag( IntPtr code4, [MarshalAs(UnmanagedType.I2)] short memExpandTag ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4memSizeBlock( IntPtr code4, [MarshalAs(UnmanagedType.U4)] int memSizeBlock ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4memSizeBuffer( IntPtr code4, [MarshalAs(UnmanagedType.U4)] int memSizeBuffer ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4memSizeMemo( IntPtr code4, [MarshalAs(UnmanagedType.U2)] short memSizeMemo ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4memSizeMemoExpr( IntPtr code4, [MarshalAs(UnmanagedType.U4)] int memSizeMemoExpr ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4memSizeSortBuffer( IntPtr code4, [MarshalAs(UnmanagedType.U4)] int memSizeSortBuffer ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4memSizeSortPool( IntPtr code4, [MarshalAs(UnmanagedType.U4)] int memSizeSortPool ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4memStartBlock( IntPtr code4, [MarshalAs(UnmanagedType.I2)] short memStartBlock ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4memStartData( IntPtr code4, [MarshalAs(UnmanagedType.I4)] int memStartBlock ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4memStartIndex( IntPtr code4, [MarshalAs(UnmanagedType.I2)] short memStartIndex ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4memStartLock( IntPtr code4, [MarshalAs(UnmanagedType.I2)] short memStartLock ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4memStartMax( IntPtr code4, [MarshalAs(UnmanagedType.I4)] int memStartMax ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4memStartTag( IntPtr code4, [MarshalAs(UnmanagedType.I2)] short memStartTag ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4optimize( IntPtr code4, [MarshalAs(UnmanagedType.I2)] short optimize ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4optimizeWrite( IntPtr code4, [MarshalAs(UnmanagedType.I2)] short optimizeWrite ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4readLock( IntPtr code4, [MarshalAs(UnmanagedType.I2)] short readLock ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4readOnly( IntPtr code4, [MarshalAs(UnmanagedType.I2)] short readOnly ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4safety( IntPtr code4, [MarshalAs(UnmanagedType.I2)] short safety ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4singleOpen( IntPtr code4, [MarshalAs(UnmanagedType.I2)] short singleOpen ) ;

      public IntPtr cod()   { return code4 ; }

      public short accessMode
      {
         get
         {
            return code4accessMode( code4, -5 ) ;
         }
         set
         {
            code4accessMode( code4, value ) ;
         }
      }
      public short autoOpen
      {
         get
         {
            return code4autoOpen( code4, -5 ) ;
         }
         set
         {
            code4autoOpen( code4, value ) ;
         }
      }
      public short codePage
      {
         get
         {
            return code4codePage( code4, -5 ) ;
         }
         set
         {
            code4codePage( code4, value ) ;
         }
      }
      public short collate
      {
         get
         {
            return code4collate( code4, -1 ) ;
         }
         set
         {
            code4collate( code4, value ) ;
         }
      }
      public short collateUnicode
      {
         get
         {
            return code4collateUnicode( code4, -1 ) ;
         }
         set
         {
            code4collateUnicode( code4, value ) ;
         }
      }
      public short collatingSequence
      {
         get
         {
            return code4collatingSequence( code4, -5 ) ;
         }
         set
         {
            code4collatingSequence( code4, value ) ;
         }
      }
      public short compatibility
      {
         get
         {
            return code4compatibility( code4, -5 ) ;
         }
         set
         {
            code4compatibility( code4, value ) ;
         }
      }
      public int createTemp
      {
         get
         {
            return c4getCreateTemp( code4 ) ;
         }
         set
         {
            c4setCreateTemp( code4, value ) ;
         }
      }

      public int errCreate
      {
         get
         {
            return c4getErrCreate( code4 ) ;
         }
         set
         {
            c4setErrCreate( code4, value ) ;
         }
      }

      public short errDefaultUnique
      {
         get
         {
            return code4errDefaultUnique( code4, -5 ) ;
         }
         set
         {
            code4errDefaultUnique( code4, value ) ;
         }
      }

      public int errExpr
      {
         get
         {
            return c4getErrExpr( code4 ) ;
         }
         set
         {
            c4setErrExpr( code4, value ) ;
         }
      }

      public int errFieldName
      {
         get
         {
            return c4getErrFieldName( code4 ) ;
         }
         set
         {
            c4setErrFieldName( code4, value ) ;
         }
      }

      public int errGo
      {
         get
         {
            return c4getErrGo( code4 ) ;
         }
         set
         {
            c4setErrGo( code4, value ) ;
         }
      }

      public short errOff
      {
         get
         {
            return code4errOff( code4, -5 ) ;
         }
         set
         {
            code4errOff( code4, value ) ;
         }
      }

      public int errOpen
      {
         get
         {
            return c4getErrOpen( code4 ) ;
         }
         set
         {
            c4setErrOpen( code4, value ) ;
         }
      }

      public short errorCode
      {
         get
         {
            return code4errorCode( code4, -5 ) ;
         }
         set
         {
            error4set( code4, value ) ;
         }
      }

      public int errRelate
      {
         get
         {
            return c4getErrRelate( code4 ) ;
         }
         set
         {
            c4setErrRelate( code4, value ) ;
         }
      }

      public int errSkip
      {
         get
         {
            return c4getErrSkip( code4 ) ;
         }
         set
         {
            c4setErrSkip( code4, value ) ;
         }
      }

      public int errTagName
      {
         get
         {
            return c4getErrTagName( code4 ) ;
         }
         set
         {
            c4setErrTagName( code4, value ) ;
         }
      }

      public int fileFlush
      {
         get
         {
            return c4getFileFlush( code4 ) ;
         }
         set
         {
            c4setFileFlush( code4, value ) ;
         }
      }

      public int hWnd
      {
         get
         {
            return code4hWnd( code4, -5 ) ;
         }
         set
         {
            code4hWnd( code4, value ) ;
         }
      }

      public short indexBlockSize
      {
         get
         {
            return code4indexBlockSize( code4 ) ;
         }
         set
         {
            code4indexBlockSizeSet( code4, value ) ;
         }
      }

      public short lockAttempts
      {
         get
         {
            return code4lockAttempts( code4, -5 ) ;
         }
         set
         {
            code4lockAttempts( code4, value ) ;
         }
      }

      public short lockAttemptsSingle
      {
         get
         {
            return code4lockAttemptsSingle( code4, -5 ) ;
         }
         set
         {
            code4lockAttemptsSingle( code4, value ) ;
         }
      }

      public int lockDelay
      {
         get
         {
            return code4lockDelay( code4, -5 ) ;
         }
         set
         {
            code4lockDelay( code4, value ) ;
         }
      }

      public int lockEnforce
      {
         get
         {
            return c4getLockEnforce( code4 ) ;
         }
         set
         {
            c4setLockEnforce( code4, value ) ;
         }
      }

      public short log
      {
         get
         {
            return code4log( code4, -5 ) ;
         }
         set
         {
            code4log( code4, value ) ;
         }
      }

      public short memExpandBlock
      {
         get
         {
            return code4memExpandBlock( code4, -5 ) ;
         }
         set
         {
            code4memExpandBlock( code4, value ) ;
         }
      }

      public short memExpandData
      {
         get
         {
            return code4memExpandData( code4, -5 ) ;
         }
         set
         {
            code4memExpandData( code4, value ) ;
         }
      }

      public short memExpandIndex
      {
         get
         {
            return code4memExpandIndex( code4, -5 ) ;
         }
         set
         {
            code4memExpandIndex( code4, value ) ;
         }
      }

      public short memExpandLock
      {
         get
         {
            return code4memExpandLock( code4, -5 ) ;
         }
         set
         {
            code4memExpandLock( code4, value ) ;
         }
      }

      public short memExpandTag
      {
         get
         {
            return code4memExpandTag( code4, -5 ) ;
         }
         set
         {
            code4memExpandTag( code4, value ) ;
         }
      }


      public int memSizeBlock
      {
         get
         {
            return code4memSizeBlock( code4, -5 ) ;
         }
         set
         {
            code4memSizeBlock( code4, value ) ;
         }
      }

      public int memSizeBuffer
      {
         get
         {
            return code4memSizeBuffer( code4, -5 ) ;
         }
         set
         {
            code4memSizeBuffer( code4, value ) ;
         }
      }

      public short memSizeMemo
      {
         get
         {
            return code4memSizeMemo( code4, -5 ) ;
         }
         set
         {
            code4memSizeMemo( code4, value ) ;
         }
      }

      public int memSizeMemoExpr
      {
         get
         {
            return code4memSizeMemoExpr( code4, -5 ) ;
         }
         set
         {
            code4memSizeMemoExpr( code4, value ) ;
         }
      }

      public int memSizeSortBuffer
      {
         get
         {
            return code4memSizeSortBuffer( code4, -5 ) ;
         }
         set
         {
            code4memSizeSortBuffer( code4, value ) ;
         }
      }

      public int memSizeSortPool
      {
         get
         {
            return code4memSizeSortPool( code4, -5 ) ;
         }
         set
         {
            code4memSizeSortPool( code4, value ) ;
         }
      }

      public short memStartBlock
      {
         get
         {
            return code4memStartBlock( code4, -5 ) ;
         }
         set
         {
            code4memStartBlock( code4, value ) ;
         }
      }

      public int memStartData
      {
         get
         {
            return code4memStartData( code4, -5 ) ;
         }
         set
         {
            code4memStartData( code4, value ) ;
         }
      }

      public short memStartIndex
      {
         get
         {
            return code4memStartIndex( code4, -5 ) ;
         }
         set
         {
            code4memStartIndex( code4, value ) ;
         }
      }

      public short memStartLock
      {
         get
         {
            return code4memStartLock( code4, -5 ) ;
         }
         set
         {
            code4memStartLock( code4, value ) ;
         }
      }

      public int memStartMax
      {
         get
         {
            return code4memStartMax( code4, -5 ) ;
         }
         set
         {
            code4memStartMax( code4, value ) ;
         }
      }

      public short memStartTag
      {
         get
         {
            return code4memStartTag( code4, -5 ) ;
         }
         set
         {
            code4memStartTag( code4, value ) ;
         }
      }

      public short optimize
      {
         get
         {
            return code4optimize( code4, -5 ) ;
         }
         set
         {
            code4optimize( code4, value ) ;
         }
      }

      public short optimizeWrite
      {
         get
         {
            return code4optimizeWrite( code4, -5 ) ;
         }
         set
         {
            code4optimizeWrite( code4, value ) ;
         }
      }

      public short readLock
      {
         get
         {
            return code4readLock( code4, -5 ) ;
         }
         set
         {
            code4readLock( code4, value ) ;
         }
      }

      public short readOnly
      {
         get
         {
            return code4readOnly( code4, -5 ) ;
         }
         set
         {
            code4readOnly( code4, value ) ;
         }
      }

      public short safety
      {
         get
         {
            return code4safety( code4, -5 ) ;
         }
         set
         {
            code4safety( code4, value ) ;
         }
      }

      public short singleOpen
      {
         get
         {
            return code4singleOpen( code4, -5 ) ;
         }
         set
         {
            code4singleOpen( code4, value ) ;
         }
      }

      //Functions of the Code4
      [DllImport("c4dll.dll")]
      private static extern void code4autoIncrementStart( IntPtr code4, [MarshalAs(UnmanagedType.R8)] double autoIncCurrentVal ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4calcCreate(IntPtr code4,
         IntPtr expr, [MarshalAs(UnmanagedType.LPStr)] string name ) ;
      [DllImport("c4dll.dll")]
      private static extern void code4calcReset( IntPtr code4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4close( IntPtr code4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4connect( IntPtr code4,
         [MarshalAs(UnmanagedType.LPStr)] string serverId, [MarshalAs(UnmanagedType.LPStr)] string processId,
         [MarshalAs(UnmanagedType.LPStr)] string userName, [MarshalAs(UnmanagedType.LPStr)] string password,
         [MarshalAs(UnmanagedType.LPStr)] string protocol ) ;
      [DllImport("c4dll.dll")]
      private static extern IntPtr code4data( IntPtr code4, [MarshalAs(UnmanagedType.LPStr)] string alias ) ;
      [DllImport("c4dll.dll")]   /* LY 2002/09/24 : changed from code4dateFormat() */
      private static extern void code4dateFormatW( IntPtr code4, StringBuilder buff ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4dateFormatSet( IntPtr code4, [MarshalAs(UnmanagedType.LPStr)] string format ) ;
		[DllImport("c4dll.dll")]
		private static extern int code4encryptInit(IntPtr code4, byte[] key, short keyLen);
		[DllImport("c4dll.dll")]
		private static extern void code4encryptFile(IntPtr code4, short encryptFlag);
		[DllImport("c4dll.dll")]
      private static extern void code4exit( IntPtr code4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4flush( IntPtr code4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4flushFiles( IntPtr code4 ) ;
      [DllImport("c4dll.dll")]   /* LY 2002/09/24 : changed from code4indexExtension() */
      private static extern void code4indexExtensionW( IntPtr code4, StringBuilder buff ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4allocLow( [MarshalAs(UnmanagedType.I4)] int inititize, [MarshalAs(UnmanagedType.LPStr)] string protocol, [MarshalAs(UnmanagedType.I4)] int version ) ;
      // CS 2007/09/18 Add code4initVB.
      [DllImport("c4dll.dll")]
      private static extern IntPtr code4initVB( );
      [DllImport("c4dll.dll")]
      private static extern int code4initUndo(IntPtr code4);
      [DllImport("c4dll.dll")]
      private static extern void code4largeOn( IntPtr code4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4lock( IntPtr code4 ) ;
      [DllImport("c4dll.dll")]
      private static extern void code4lockClear( IntPtr code4 ) ;
      [DllImport("c4dll.dll")]   /* LY 2002/09/24 : changed from code4lockFileName() */
      private static extern void code4lockFileNameW( IntPtr code4, StringBuilder buff ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4lockItem( IntPtr code4 ) ;
      [DllImport("c4dll.dll")]   /* LY 2002/09/24 : changed from code4lockUserId() */
      private static extern void code4lockUserIdW( IntPtr code4, StringBuilder buff ) ;
      [DllImport("c4dll.dll")]   /* LY 2002/09/24 : changed from code4lockNetworkId */
      private static extern void code4lockNetworkIdW( IntPtr code4, StringBuilder buff ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4logCreate( IntPtr code4,
         [MarshalAs(UnmanagedType.LPStr)] string name, [MarshalAs(UnmanagedType.LPStr)] string userId ) ;
      [DllImport("c4dll.dll")]   /* LY 2002/09/25 : changed from code4logFileName() */
      private static extern void code4logFileNameW( IntPtr code4, StringBuilder buff ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4logOpen( IntPtr code4,
         [MarshalAs(UnmanagedType.LPStr)] string name, [MarshalAs(UnmanagedType.LPStr)] string userId ) ;
      [DllImport("c4dll.dll")]
      private static extern void code4logOpenOff( IntPtr code4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4optAll( IntPtr code4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4optStart( IntPtr code4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4optSuspend( IntPtr code4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4timeout( IntPtr code4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4timeoutSet( IntPtr code4, [MarshalAs(UnmanagedType.I4)] int l ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4tranCommit( IntPtr code4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4tranRollback( IntPtr code4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4tranStart( IntPtr code4 ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4tranStatusCB( IntPtr code4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4unlock( IntPtr code4 ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4unlockAutoCB( IntPtr code4 ) ;
      [DllImport("c4dll.dll")]
      private static extern short code4unlockAutoSetCB( IntPtr code4, [MarshalAs(UnmanagedType.I2)] short c ) ;
      [DllImport("c4dll.dll")]
      private static extern int code4verifySet( IntPtr code4, [MarshalAs(UnmanagedType.LPStr)] string value ) ;
      // CS 2007/09/18
      [DllImport("c4dll.dll")]
      private static extern int error4callback(IntPtr code4, error4callbackFunction errorCallback);

      public Code4( )
      {
         init() ;
      }
      public Code4( int initialize )
      {
         init_p = 0 ;
         if ( initialize != 0 )
            init() ;
      }

      public void autoIncrementStart( double autoIncCurrentVal ) { code4autoIncrementStart( code4, autoIncCurrentVal ) ; }
      public int calcCreate( Expr4 ex, string name ) { return ( code4calcCreate( code4, ex.EXPR4, name ) != 0 ? r4success : -1 ) ; }
      public void calcReset() { code4calcReset( code4 ) ; }
      public int  closeAll()  { return code4close( code4 ) ; }
      public int connect( string serverId, string processId, string userName, string password, string protocol)
      {
         return code4connect( code4, serverId, processId, userName, password, protocol ) ;
      }
      public Data4 data( string alias )
      {
         Data4 r = new Data4( code4data( code4, alias ) ) ;
         return r ;
      }
      public string dateFormat
      {
         get
         {
            /* 20 = LEN4DATE_FORMAT */
            StringBuilder dateBuff = new StringBuilder( 20 ) ;
            code4dateFormatW( code4, dateBuff ) ;
            return dateBuff.ToString() ;
         }
         set
         {
            code4dateFormatSet( code4, value ) ;
         }
      }

      public delegate void error4callbackFunction([MarshalAs(UnmanagedType.I4)]int code4, [MarshalAs(UnmanagedType.I4)]int er, [MarshalAs(UnmanagedType.I4)]int er2,
[MarshalAs(UnmanagedType.LPStr)]string p1, [MarshalAs(UnmanagedType.LPStr)]string p2, [MarshalAs(UnmanagedType.LPStr)]string p3);

      public void errorCallback(error4callbackFunction errorCallback)
      {
         error4callback(code4, new error4callbackFunction(errorCallback));
      }

		public void encryptFile(short encryptFlag) { code4encryptFile(code4, encryptFlag); }
		public int encryptInit(byte[] key, short keyLen) { return code4encryptInit(code4, key, keyLen); }
		public void exit() { code4exit(code4); }
      public int flushFiles()  { return code4flush( code4 ) ; }
      public string indexExtension()
      {
         StringBuilder extBuff = new StringBuilder( 3 ) ;
         code4indexExtensionW( code4, extBuff ) ;
         return extBuff.ToString( 0, 3 ) ;
      }
      public int init()
      {
         if ( init_p != 0 )
            code4initUndo( code4 ) ;
         // CS 2007/09/18 Call code4initVB rather than code4allocLow.
         code4 = code4initVB();
         if ( code4 != IntPtr.Zero )
            init_p = 1 ;
         else
            init_p = 0 ;
         return ( init_p != 0 ? 1 : 0 ) ;
      }
      public int initUndo()
      {
         int rc = r4success ;
         if ( init_p != 0 )
         {
            rc = code4initUndo( code4 ) ;
            if (rc == r4success)
            {
               init_p = 0 ;
               code4 = IntPtr.Zero;
            }
         }
         return rc ;
      }
      public void largeOn() { code4largeOn( code4 ) ; }

      // 'lock' is C# keyword - replaced 'lock()' with 'lockGroup'
      public int lockGroup()  { return code4lock( code4 ) ; }
      public void lockClear()           { code4lockClear( code4 ) ; }
      public string lockFileName()
      {
         /* 250 = LEN4PATH */
         StringBuilder nameBuff = new StringBuilder( 250 ) ;
         code4lockFileNameW( code4, nameBuff ) ;
         return nameBuff.ToString() ;
      }
      public int lockItem()            { return code4lockItem( code4 ) ; }
      public string lockUserId()
      {
         /* 250 = LEN4PATH */
         StringBuilder idBuff = new StringBuilder( 250 ) ;
         code4lockUserIdW( code4, idBuff ) ;
         return idBuff.ToString() ;
      }
      public string lockNetworkId()
      {
         /* 250 = LEN4PATH */
         StringBuilder idBuff = new StringBuilder( 250 ) ;
         code4lockNetworkIdW( code4, idBuff ) ;
         return idBuff.ToString() ;
      }
      public int logCreate( string name, string userId ) { return code4logCreate( code4, name, userId) ; }
      public string logFileName()
      {
         /* 250 = LEN4PATH */
         StringBuilder nameBuff = new StringBuilder( 250 ) ;
         code4logFileNameW( code4, nameBuff ) ;
         return nameBuff.ToString() ;
      }
      public int logOpen( string name, string userId ) { return code4logOpen( code4, name, userId ) ; }
      public void logOpenOff() { code4logOpenOff( code4 ) ; }
      public int optAll()  { return code4optAll( code4 ) ; }
      public int optStart()  { return code4optStart( code4 ) ; }
      public int optSuspend() { return code4optSuspend( code4 ) ; }
      public int timeout
      {
         get
         {
            return code4timeout( code4 ) ;
         }
         set
         {
            code4timeoutSet( code4, value ) ;
         }
      }

      public int tranCommit()    { return code4tranCommit( code4 ) ; }
      public int tranRollback()  { return code4tranRollback( code4 ) ; }
      public int tranStart()     { return code4tranStart( code4 ) ; }
      public short tranStatus()    { return code4tranStatusCB( code4 ); }
      public int unlock()        { return code4unlock( code4 ) ; }
      public short unlockAuto
      {
         get
         {
            return code4unlockAutoCB( code4 ) ;
         }
         set
         {
            code4unlockAutoSetCB( code4, value ) ;
         }
      }
      public void verifySet( string val )   { code4verifySet( code4, val ) ; }
   }

   public class Data4 : Error4
   {
      private IntPtr data ;

      [DllImport("c4dll.dll")]   /* LY 2002/09/25 : replaced d4alias() */
      private static extern void d4aliasW( IntPtr data4, StringBuilder buff ) ;
      [DllImport("c4dll.dll")]
      private static extern void d4aliasSet( IntPtr data4, [MarshalAs(UnmanagedType.LPStr)] string alias ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4append( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4appendBlank( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern short d4appendStart( IntPtr data4, [MarshalAs(UnmanagedType.I2)] short useMemo ) ;
      [DllImport("c4dll.dll")]
      private static extern void d4blank( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4bof( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4bottom( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4check( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern IntPtr d4create( IntPtr code4, [MarshalAs(UnmanagedType.LPStr)] string name, IntPtr fieldInfo, IntPtr tagInfo );
      [DllImport("c4dll.dll")]
      private static extern int d4close( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern IntPtr d4code( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern void d4delete( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4deleted( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4eof( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4fieldNumber( IntPtr data4, [MarshalAs(UnmanagedType.LPStr)] string fieldName ) ;
      [DllImport("c4dll.dll")]
      private static extern IntPtr d4fieldsAdd( IntPtr data4, [MarshalAs(UnmanagedType.I2)] short nFields, IntPtr fieldInfo);
      [DllImport("c4dll.dll")]   /* LY 2002/09/25 : replaced d4fileName() */
      private static extern void d4fileNameW( IntPtr data4, StringBuilder buff ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4flush( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4freeBlocks( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4goLow( IntPtr data4, [MarshalAs(UnmanagedType.I4)] int recNo, [MarshalAs(UnmanagedType.I2)] short goForWrite ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4goBof( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4goEof( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4lockInternal( IntPtr data4, [MarshalAs(UnmanagedType.I4)] int recNum, [MarshalAs(UnmanagedType.U1)] byte doUnlock, [MarshalAs(UnmanagedType.I4)] int lockType ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4lockAdd( IntPtr data4, [MarshalAs(UnmanagedType.I4)] int recNum ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4lockAddAppend( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4lockAddFile( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4lockAddAll( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4lockAllInternal( IntPtr data4, [MarshalAs(UnmanagedType.U1)] byte doUnlock ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4lockAppendInternal( IntPtr data4, [MarshalAs(UnmanagedType.U1)] byte doUnlock ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4lockFileInternal( IntPtr data4, [MarshalAs(UnmanagedType.U1)] byte doUnlock, [MarshalAs(UnmanagedType.I4)] int lockType ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4log( IntPtr data4, [MarshalAs(UnmanagedType.I4)] int logging ) ;
      [DllImport("c4dll.dll")]
      private static extern short d4logStatusCB( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4memoCompress(IntPtr data4);
      [DllImport("c4dll.dll")]
      private static extern IntPtr d4modifyStructure(IntPtr data4, IntPtr fieldInfo, IntPtr tagInfo );
      [DllImport("c4dll.dll")]
      private static extern IntPtr d4open( IntPtr code4, [MarshalAs(UnmanagedType.LPStr)] string name ) ;
      [DllImport("c4dll.dll")]
      private static extern IntPtr d4openClone( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4optimize( IntPtr data4, [MarshalAs(UnmanagedType.I4)]  int opt ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4optimizeWrite( IntPtr data4, [MarshalAs(UnmanagedType.I4)]  int opt ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4pack( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern double d4position( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4positionSet( IntPtr data4, [MarshalAs(UnmanagedType.R8)] double pos ) ;
      [DllImport("c4dll.dll")]
      private static extern void d4recall( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4recCountDo2( IntPtr data4, [MarshalAs(UnmanagedType.I2)] short assumeLocked ) ;
      [DllImport("c4dll.dll")]
      private static extern uint d4recWidthLow( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4refresh( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4refreshRecord( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4reindex( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4remove( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4seek( IntPtr data4, [MarshalAs(UnmanagedType.LPStr)] string key ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4seekN( IntPtr data4, [MarshalAs(UnmanagedType.LPStr)] string key, [MarshalAs(UnmanagedType.I2)] short len ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4seekDouble( IntPtr data4, [MarshalAs(UnmanagedType.R8)] double key ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4seekNext( IntPtr data4, [MarshalAs(UnmanagedType.LPStr)] string key ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4seekNextN( IntPtr data4, [MarshalAs(UnmanagedType.LPStr)] string key, [MarshalAs(UnmanagedType.I2)] short len ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4seekNextDouble( IntPtr data4, [MarshalAs(UnmanagedType.R8)] double key ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4seekNextUnicodeN( IntPtr data4, [MarshalAs(UnmanagedType.LPWStr)] string key, [MarshalAs(UnmanagedType.I2)] short len ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4seekUnicodeN( IntPtr data4, [MarshalAs(UnmanagedType.LPWStr)] string key, [MarshalAs(UnmanagedType.I2)] short len ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4skip( IntPtr data4, [MarshalAs(UnmanagedType.I4)] int number ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4tagSync( IntPtr data4, IntPtr tag4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4tagSelect( IntPtr data4, IntPtr tag4 ) ;
      [DllImport("c4dll.dll")]
      private static extern IntPtr d4tagSelected( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4top( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4unlock( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4writeLow( IntPtr data4,
         [MarshalAs(UnmanagedType.I4)] int number,
         [MarshalAs(UnmanagedType.I4)] int unlock,
         [MarshalAs(UnmanagedType.I4)] int doLock
         ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4zap( IntPtr data4, [MarshalAs(UnmanagedType.I4)] int first, [MarshalAs(UnmanagedType.I4)] int last ) ;

      public Data4() { data = IntPtr.Zero; }
      public Data4( Data4 d ) { data = d.data ; code4 = d.code4 ; }

      /* here because of Field4::data(), but do we really want this? */
      /* yes we do as it is also needed in the relation module. */
      public Data4( IntPtr d ) { data = d ; code4 = d4code( d ) ; }

      public Data4( Code4 code, string name )   { data = d4open( code.cod(), name ) ; if ( data != IntPtr.Zero ) code4 = d4code( data ) ; }

      public IntPtr cod()   { return code4 ; }

      public string alias
      {
         get
         {
            /* 33 = LEN4DATA_ALIAS + 1 */
            StringBuilder buff = new StringBuilder( 33 ) ;
            d4aliasW( data, buff ) ;
            return buff.ToString() ;
         }
         set
         {
            d4aliasSet( data, value ) ;
         }
      }
      public IntPtr dat()   { return data ; }

      public int append()  { return d4append( data ) ; }
      public int appendBlank() { return d4appendBlank( data ) ; }
      public short appendStart( short memo ){ return d4appendStart( data, memo ) ; }
      public short appendStart( ) { return( d4appendStart( data, 0 ) ); }
      public void blank() { d4blank( data ) ; }
      public int bof() { return d4bof( data ) ; }
      public int bottom() { return d4bottom( data ) ; }
      public short changed
      {
         get
         {
            return d4changed( data, -1 ) ;
         }
         set
         {
            d4changed( data, value ) ;
         }
      }
      public int checkIndex() { return d4check( data ) ; }
      public int close()
      {
         if (data != IntPtr.Zero)
         {
            int rc = d4close( data ) ;
            data = IntPtr.Zero;
            return rc;
         }
         else
            return 0 ;
      }

      public int create( ref Code4 code, string name, ref Field4info f, ref Tag4info t)
      {
         data = d4create( code.cod(), name, f.fields(), t.tags() ) ;
         if (data != IntPtr.Zero)
            code4 = code.cod() ;
         return code.errorCode ;
      }
      public int create( ref Code4 code, string name, ref Field4info f)
      {
         data = d4create(code.cod(), name, f.fields(), IntPtr.Zero);
         if (data != IntPtr.Zero)
            code4 = code.cod() ;
         return code.errorCode ;
      }

      public void deleteRec()              { d4delete( data ) ; }
      public int deleted()          { return d4deleted( data ) ; }
      public int eof()              { return d4eof( data ) ; }
      public int fieldNumber( string name ) { return d4fieldNumber( data, name ) ; }
      public int fieldsAdd( ref Field4info f )
      {
            // 2006/01/26 CS Add Data4.fieldsAdd method.
            IntPtr new_data = d4fieldsAdd( data, (short)f.numFields(), f.fields() ) ;
            if (new_data != IntPtr.Zero)
            {
                     // success
                     data = new_data ;
                     return 0;
            }
            else
            {
                     //fail
                     return -1;
            }
      }
      public string fileName()
      {
         /* 250 = LEN4PATH */
         StringBuilder nameBuff = new StringBuilder( 250 ) ;
         d4fileNameW( data, nameBuff ) ;
         return nameBuff.ToString() ;
      }
      public int flush()            { return d4flush( data ) ; }
      public int freeBlocks()       { return d4freeBlocks( data ) ; }
      public int getErrorCode()     { return code4errorCode( code4, -5 ) ; }
      public int go( int rec )       { return d4goLow( data, rec, 1 ) ; }
      public int goBof()             { return d4goBof( data ) ; }
      public int goEof()             { return d4goEof( data ) ; }
      public Index4 index( string name )
      {
         Index4 idx = new Index4() ;
         idx.init( this, name ) ;
         return idx ;
      }
      public int isValid()                  { return ( data != IntPtr.Zero ? 1 : 0 ) ; }
      public int lockRecord( int recNum )  { return d4lockInternal( data, recNum, 1, 1 ) ; }
      public int lockAdd( int recnum )     { return d4lockAdd( data, recnum ) ; }
      public int lockAddAppend()            { return d4lockAddAppend( data ) ; }
      public int lockAddFile()              { return d4lockAddFile( data ) ; }
      public int lockAddAll()               { return d4lockAddAll( data ) ; }
      public int lockAll()                  { return d4lockAllInternal( data, 1 ) ; }
      public int lockAppend()               { return d4lockAppendInternal( data, 1 ) ; }
      public int lockFile()                 { return d4lockFileInternal( data, 1, 1 ) ; }
      public int log
      {
         set
         {
            d4log( data, value ) ;
         }
         get
         {
            return d4log( data, -1 ) ;
         }
      }
      public int logStatus()                { return (int)d4logStatusCB( data ) ; }
      public int memoCompress()             { return d4memoCompress( data ) ; }

      public int modifyStructure(ref Field4info f, ref Tag4info t)
      {
         data = d4modifyStructure(data, f.fields(), t.tags());
         return getErrorCode();
      }
      public int modifyStructure(ref Field4info f)
      {
         data = d4modifyStructure(data, f.fields(), IntPtr.Zero);
         return getErrorCode();
      }

      public int numFields()                { return (int)d4numFields( data ) ; }
      public int open( ref Code4 code, string name )
      {
         data =  d4open( code.cod(), name ) ;
         if (data != IntPtr.Zero)
            code4 = code.cod() ;
         return ( (code.errOpen == 0) ? (code.errorCode) : (code.errorCode < 0 ? (int) code.errorCode : 0 )) ;
      }
      public int openClone( Data4 d )
      {
         data = d4openClone( d.dat() ) ;
         if (data != IntPtr.Zero)
         {
            code4 = d.code4 ;
            return getErrorCode() ;
         }
         else
            return -1 ;
      }
      public int optimize( int optFlag ) { return d4optimize( data, optFlag ) ; }
      public int optimizeWrite( int optFlag ) { return d4optimizeWrite( data,  optFlag ) ; }
      public int pack()                     { return d4pack( data ) ; }
      public double position
      {
         get
         {
            return d4position( data ) ;
         }
         set
         {
            d4positionSet( data, value ) ;
         }
      }
      public void recall()           { d4recall( data ) ; }
      public int recCount()          { return d4recCountDo2( data, 0 ) ; }
      public int recNo()             { return d4recNoLow( data ) ; }
      public uint recWidth()         { return d4recWidthLow( data ) ; }
      public int refresh()           { return d4refresh( data ) ; }
      public int refreshRecord()     { return d4refreshRecord( data ) ; }
      public int reindex()           { return d4reindex( data ) ; }

      public int remove()
      {
         int rc = d4remove( data ) ;
         if ( rc == 0 )
            data = IntPtr.Zero;
         return rc ;
      }
      public int seek( string ptr )  { return d4seek( data, ptr ) ; }
      public int seek( string ptr, short len ) { return d4seekN( data, ptr, len ) ; }
      public int seek( double d )   { return d4seekDouble( data, d ) ; }
      public int seekUnicode( string ptr )   { return d4seekUnicodeN( data, ptr, (short)(ptr.Length * 2) ) ; }
      public int seekUnicode( string ptr, short len )   { return d4seekUnicodeN( data, ptr, len ) ; }
      public int seekNext( string ptr ) { return d4seekNext( data, ptr ) ; }
      public int seekNext( string ptr, short len ) { return d4seekNextN( data, ptr, len ) ; }
      public int seekNext( double d ) { return d4seekNextDouble( data, d ) ; }
      public int seekNextUnicode( string ptr )  { return d4seekNextUnicodeN( data, ptr, (short)(ptr.Length * 2) ) ; }
      public int seekNextUnicode( string ptr, short len )  { return d4seekNextUnicodeN( data, ptr, len ) ; }
      public void select() { d4tagSelect(data, IntPtr.Zero); }
      public void select( Tag4 tag )  { d4tagSelect( data, tag.tg() ) ; }
      public void select( string tagName )
      {
         if( tagName.Length != 0 )
         {
            Tag4 tag = new Tag4( this, tagName ) ;
            d4tagSelect( data, tag.tg() ) ;
         }
         else
         {
            d4tagSelect(data, IntPtr.Zero);
         }
      }
      public int skip( int n )   { return d4skip( data, n ) ; }
      public int skip() { return d4skip( data, 1) ; }
      public int tagSync()             { return d4tagSync( data, d4tagSelected( data ) ) ; }
      public int top()                 { return d4top( data ) ; }
      public int unlock()              { return d4unlock( data ) ; }
      public int write( int rec )     { return d4writeLow( data, rec, 0, 1 ) ; }
      public int write()               { return d4writeLow( data, -1, 0, 1 ) ; }
      public int zap( int f, int last ) { return d4zap( data, f, last ) ; }
      public int zap()                  { return d4zap( data, 1, 1000000000 ) ; }
   } ;

   public class Index4 : Error4
   {
      private IntPtr index ;
      private Data4 D4 ;

      [DllImport("c4dll.dll")]
      private static extern IntPtr i4openW( IntPtr data4, [MarshalAs(UnmanagedType.LPStr)] string name ) ;
      [DllImport("c4dll.dll")]
      private static extern int i4close( IntPtr index4 ) ;
      [DllImport("c4dll.dll")]
      private static extern IntPtr i4create( IntPtr data4, [MarshalAs(UnmanagedType.LPStr)] string name, IntPtr tagInfo );
      [DllImport("c4dll.dll")]   /* LY 2002/09/25 : replaced i4fileName() */
      private static extern void i4fileNameW( IntPtr index4, StringBuilder buff ) ;
      [DllImport("c4dll.dll")]
      private static extern IntPtr d4index( IntPtr data4, [MarshalAs(UnmanagedType.LPStr)] string name ) ;
      [DllImport("c4dll.dll")]
      private static extern int i4reindex( IntPtr index4 ) ;
      //      [DllImport("c4dll.dll")]
      //      private static extern int i4tag( IntPtr index4, [MarshalAs(UnmanagedType.LPStr)] string name ) ;

      public IntPtr cod()     { return code4 ; }
      public IntPtr idx()     { return index ; }

      public Index4() { index = IntPtr.Zero; }
      public Index4( Data4 d, string name )
      {
         index = i4openW( d.dat(), name ) ;
         if (index != IntPtr.Zero)
         {
            code4 = d.cod() ;
            D4 = d ;
         }
      }
      public int close()
      {
         int rc =  i4close( index ) ;
         D4 = null ;
         index = IntPtr.Zero;
         code4 = IntPtr.Zero;
         return rc;
      }
      public int create( ref Data4 data, string name, ref Tag4info t)
      {
         int rc ;

         code4 = data.cod() ;
         if (data.dat() == IntPtr.Zero)
         {
            rc = throwError( e4struct, 60709 ) ;
         }
         else
         {
            index = i4create( data.dat(), name, t.tags() ) ;
            if ( index != IntPtr.Zero )
            {
               D4 = data ;
            }
            rc = data.getErrorCode() ;
         }
         return rc ;
      }
      public string fileName()
      {
         /* 250 = LEN4PATH */
         StringBuilder nameBuff = new StringBuilder( 250 ) ;
         i4fileNameW( index, nameBuff ) ;
         return nameBuff.ToString() ;
      }
      public void init( Data4 d, string name )
      {
         index = d4index( d.dat(), name );
         if (index != IntPtr.Zero)
         {
            code4 = d.cod() ;
            D4 = d ;
         }
      }
      public int isValid() { return (index != IntPtr.Zero ? 1 : 0); }
      public int open( Data4 d, string file )
      {
         int rc ;

         code4 = d.cod() ;
         if (d.dat() == IntPtr.Zero)
         {
            rc = throwError( e4struct, 60709 ) ;
         }
         else
         {
            index = i4openW( d.dat(), file ) ;
            if (index != IntPtr.Zero)
            {
               D4 = d ;
            }
            rc = d.getErrorCode() ;
         }
         return rc ;
      }
      public int reindex()                { return i4reindex( index ) ; }
      public Tag4 tag( string name )      { return new Tag4( D4, name ) ; }
   } ;

   public class Tag4 : Error4
   {
      private IntPtr tag ;
      private IntPtr data ;

      [DllImport("c4dll.dll")]   /* LY 2002/09/25 : replaced t4alias() */
      private static extern void t4aliasW( IntPtr tag4, StringBuilder buff ) ;
      [DllImport("c4dll.dll")]
      private static extern int t4close( IntPtr tag4 ) ;
      [DllImport("c4dll.dll")]
      private static extern short t4descending( IntPtr tag4 ) ;
      [DllImport("c4dll.dll")]   /* LY 2002/09/25 : replaced t4exprCB() */
      private static extern void t4exprW( IntPtr tag4, StringBuilder buff ) ;
      [DllImport("c4dll.dll")]   /* LY 2002/09/25 : replaced t4filterCB */
      private static extern void t4filterW( IntPtr tag4, StringBuilder buff ) ;
      [DllImport("c4dll.dll")]
      private static extern IntPtr d4tag(IntPtr data4, [MarshalAs(UnmanagedType.LPStr)] string name);
      [DllImport("c4dll.dll")]
      private static extern IntPtr d4tagSelected(IntPtr data4);
      [DllImport("c4dll.dll")]
      private static extern IntPtr d4tagNext( IntPtr data4, IntPtr tag4 ) ;
      [DllImport("c4dll.dll")]
      private static extern IntPtr d4tagPrev(IntPtr data4, IntPtr tag4);
      [DllImport("c4dll.dll")]
      private static extern IntPtr t4openLow( IntPtr data4, IntPtr index4, [MarshalAs(UnmanagedType.LPStr)] string name ) ;
      [DllImport("c4dll.dll")]
      private static extern int t4seekN( IntPtr tag4, [MarshalAs(UnmanagedType.LPStr)] string seekValue, [MarshalAs(UnmanagedType.I2)] short len, [MarshalAs(UnmanagedType.I2)] short doDataPosition ) ;
      [DllImport("c4dll.dll")]
      private static extern int t4seekNW( IntPtr tag4, [MarshalAs(UnmanagedType.LPWStr)] string seekValue, [MarshalAs(UnmanagedType.I2)] short len, [MarshalAs(UnmanagedType.I2)] short doDataPosition ) ;
      [DllImport("c4dll.dll")]
      private static extern short t4unique( IntPtr tag4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int t4uniqueSet( IntPtr tag4, [MarshalAs(UnmanagedType.I2)] short unique  ) ;

      public Tag4() { tag = IntPtr.Zero; }
      public Tag4( Data4 d, string name )
      {
         init( d, name );
         if (tag != IntPtr.Zero)
            code4 = d.cod() ;
      }

      public Tag4( IntPtr dataPointer, IntPtr tagPointer )
      {

         tag = tagPointer ;
      }

      public IntPtr tg()      { return tag ; }

      public string alias()
      {
         /* 11 = LEN4TAG_ALIAS + 1 */
         StringBuilder buff = new StringBuilder( 11 ) ;
         t4aliasW( tag, buff ) ;
         return buff.ToString() ;
      }
      public int close()
      {
         int rc = t4close( tag ) ;
         tag = IntPtr.Zero;
         code4 = IntPtr.Zero ;
         return rc ;
      }
      public int descending()
      {
         int rc;

         if (tag == IntPtr.Zero)
         {
            throwError( e4struct, 60913 ) ;
            rc = 0 ;
         }
         else
            rc = t4descending( tag ) ;
         return rc ;
      }
      public string expr()
      {
         StringBuilder buff = new StringBuilder( 512 ) ;
         t4exprW( tag, buff ) ;
         return buff.ToString() ;
      }
      public string filter()
      {
         StringBuilder buff = new StringBuilder( 512 ) ;
         t4filterW( tag, buff ) ;
         return buff.ToString() ;
      }
      public void init( Data4 d, string name )
      {
         if ( name.Length != 0 )
            tag = d4tag( d.dat(), name ) ;
         else
         {
            tag = d4tagSelected( d.dat() ) ;

            if (tag == IntPtr.Zero)
               tag = d4tagNext( d.dat(), IntPtr.Zero ) ;
         }
         if ( tag != IntPtr.Zero )
         {
            data = d.dat() ;
            code4 = d.cod() ;
         }
         else
         {
            data = IntPtr.Zero;
            code4 = IntPtr.Zero ;
         }
      }
      public void initFirst( Data4 d )
      {
         tag = d4tagNext(d.dat(), IntPtr.Zero);
         if (tag != IntPtr.Zero)
         {
            data = d.dat() ;
            code4 = d.cod() ;
         }
         else
         {
            data = IntPtr.Zero;
            code4 = IntPtr.Zero;
         }
      }
      public void initLast( Data4 d )
      {
         tag = d4tagPrev(d.dat(), IntPtr.Zero);
         if (tag != IntPtr.Zero)
         {
            data = d.dat() ;
            code4 = d.cod() ;
         }
         else
         {
             data = IntPtr.Zero;
            code4 = IntPtr.Zero;
         }
      }
      public void initNext()     { if( isValid() != 0 ) tag = d4tagNext( data, tag ) ; }
      public void initPrev()     { if( isValid() != 0 ) tag = d4tagPrev( data, tag ) ; }
      public void initSelected( Data4 d )
      {
         tag = d4tagSelected( d.dat() ) ;
         if (tag != IntPtr.Zero)
         {
            data = d.dat() ;
            code4 = d.cod() ;
         }
         else
         {
            data = IntPtr.Zero;
            code4 = IntPtr.Zero;
         }
      }
      public int isValid() { return (tag != IntPtr.Zero ? 1 : 0); }
      public void open( Data4 d, string name )
      {
         tag = t4openLow(d.dat(), IntPtr.Zero, name);
         if (tag != IntPtr.Zero)
         {
            data = d.dat() ;
            code4 = d.cod() ;
         }
         else
         {
            data = IntPtr.Zero;
            code4 = IntPtr.Zero;
         }
      }
      public void open( Data4 d, Index4 i, string name )
      {
         tag = t4openLow( d.dat(), i.idx(), name ) ;
         if (tag != IntPtr.Zero)
         {
            data = d.dat() ;
            code4 = d.cod() ;
         }
         else
         {
            data = IntPtr.Zero;
            code4 = IntPtr.Zero;
         }
      }
      public int seekN( string seekValue, short len, short doDataPosition )
      {
         return t4seekN( tag, seekValue, len, doDataPosition ) ;
      }
      public int seekUnicodeN( string seekValue, short len, short doDataPosition )
      {
         return t4seekNW( tag, seekValue, len, doDataPosition ) ;
      }
      public short unique
      {
         set
         {
            t4uniqueSet( tag, value ) ;
         }
         get
         {
            return t4unique( tag ) ;
         }
      }
   } ;


   public class Field4 : Error4
   {
      private IntPtr field ;
      private IntPtr db ;

      private const int r4cdx = 200 ;

      [DllImport("c4dll.dll")]
      private static extern int code4indexFormat( IntPtr code4  ) ;
      [DllImport("c4dll.dll")]
      private static extern int d4lockTest( IntPtr data4, [MarshalAs(UnmanagedType.I4)] int recNo, [MarshalAs(UnmanagedType.I4)] int lockType  ) ;
      [DllImport("c4dll.dll")]
      private static extern byte d4versionCB( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern void f4assignBytesW( IntPtr field4, [MarshalAs(UnmanagedType.LPArray)] byte[] val, [MarshalAs(UnmanagedType.I4)] int len ) ;
      [DllImport("c4dll.dll")]
      private static extern void f4assignCurrency( IntPtr field4, [MarshalAs(UnmanagedType.LPStr)] string val ) ;
      [DllImport("c4dll.dll")]
      private static extern void f4assignDateTime( IntPtr field4, [MarshalAs(UnmanagedType.LPStr)] string val ) ;
      [DllImport("c4dll.dll")]
      private static extern void f4assignDouble( IntPtr field4, [MarshalAs(UnmanagedType.R8)] double val ) ;
      [DllImport("c4dll.dll")]
      private static extern void f4assignField( IntPtr fieldTo, IntPtr fieldFrom ) ;
      [DllImport("c4dll.dll")]
      private static extern void f4assignN( IntPtr field4, [MarshalAs(UnmanagedType.LPStr)] string val, [MarshalAs(UnmanagedType.U4)] uint len ) ;
      [DllImport("c4dll.dll")]
      private static extern void f4assignNotNull( IntPtr field4 ) ;
      [DllImport("c4dll.dll")]
      private static extern void f4assignNull( IntPtr field4 ) ;
      [DllImport("c4dll.dll")]
      private static extern void f4assignInt( IntPtr field4, [MarshalAs(UnmanagedType.I4)] int val ) ;
      [DllImport("c4dll.dll")]
      private static extern void f4assignUnicode( IntPtr field4, [MarshalAs(UnmanagedType.LPWStr)] string val ) ;
      [DllImport("c4dll.dll")]
      private static extern void f4bytesW( IntPtr field4, [MarshalAs(UnmanagedType.LPArray)] byte[] val, [MarshalAs(UnmanagedType.I4)] int len ) ;
      [DllImport("c4dll.dll")]   /* LY 2002/09/25 : replaced f4currency() */
      private static extern void f4currencyW( IntPtr field4, [MarshalAs(UnmanagedType.I2)] short numDec, StringBuilder buff ) ;
      [DllImport("c4dll.dll")]
      private static extern char f4char( IntPtr field4 ) ;
      [DllImport("c4dll.dll")]   /* LY 2002/09/25 : replaced f4dateTime() */
      private static extern void f4dateTimeW( IntPtr field4, StringBuilder buff ) ;
      [DllImport("c4dll.dll")]
      private static extern double f4double( IntPtr field4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int f4int( IntPtr field4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int f4number( IntPtr field4 ) ;
      [DllImport("c4dll.dll")]   /* LY 2002/09/25 : replaced f4str() */
      private static extern void f4strW( IntPtr field4, byte[] buff ) ;
      [DllImport("c4dll.dll")]   // LY Jun 23/04 : replaced f4strUnicode()
      private static extern void f4strUnicodeW( IntPtr field4, [MarshalAs(UnmanagedType.LPWStr)] string buff ) ;
      [DllImport("c4dll.dll")]
      private static extern int f4true( IntPtr field4 ) ;

      public IntPtr cod()              { return code4 ; }
      public IntPtr dat()              { return db ; }
      public IntPtr fld()              { return field ; }

      // AS Feb 23/06 - also support as an int...makes conversion avoidance (d4fieldNumber returns int)
      public Field4( Data4 d, int j )
      {
         field =  d4fieldJ( d.dat(), (short)j ) ;
         if (field != IntPtr.Zero)
         {
            db = d.dat() ;
            code4 = d.cod() ;
         }
      }
      public Field4( Data4 d, short j )
      {
         field =  d4fieldJ( d.dat(), j ) ;
         if ( field != IntPtr.Zero )
         {
            db = d.dat() ;
            code4 = d.cod() ;
         }
      }
      public Field4( Data4 d, string name )
      {
         field = d4field( d.dat(), name ) ;
         if (field != IntPtr.Zero)
         {
            db = d.dat() ;
            code4 = d.cod() ;
         }
      }
      public Field4( ref Field4 f )
      {
         field = d4fieldJ( f.dat(), (short)f.number() ) ;
         if (field != IntPtr.Zero)
         {
            db = f.dat() ;
            code4 = f.code4 ;
         }
      }

      /* not allowed to overload int and double -- added 'get" */
      public char getChar()         { return f4char( field ) ; }
      public int getInt()           { return f4int( field ) ; }
      public double getDouble()     { return f4double(field) ; }

      public void assign( string data )   { f4assignN( field, data, (uint)data.Length ) ; }
      public void assign( string data, uint len )   { f4assignN( field, data, len ) ; }
      public void assignBytes( byte[] data, int len ) { f4assignBytesW( field, data, len ) ; }
      public void assignCurrency( string data )    { f4assignCurrency( field, data ) ; }
      public void assignDateTime( string data )    { f4assignDateTime( field, data ) ; }
      public void assignDouble( double data )    { f4assignDouble( field, data ) ; }
      public void assignField( ref Field4 f )   { f4assignField( field, f.fld() ) ; }
      public void assignInt( int data )    { f4assignInt( field, data ) ; }
      public void assignNull()             { f4assignNull( field ) ; }
      public void assignUnicode( string str )   { f4assignUnicode( field, str ) ; }
      public void bytes( byte[] data, int len ) { f4bytesW( field, data, len ) ; }
      public void changed()
      {
         if (field == IntPtr.Zero)
            throwError( e4struct, E61203 );
         else
            d4changed( db, 1 ) ;
         if ( d4versionCB( db ) == 0x30 )
            f4assignNotNull( field ) ;
      }
      public Data4 data()
      {
         Data4 dt = new Data4() ;

         if (field == IntPtr.Zero)
            throwError( e4struct, 61204 ) ;
         else
            dt = new Data4( db ) ;

         return dt ;
      }

      public string currency( short numDec )
      {
         StringBuilder buff = new StringBuilder( 20 ) ;
         f4currencyW( field, numDec, buff ) ;
         return buff.ToString(0, Math.Min(20, buff.Length)).TrimEnd('\0');
      }
      public string dateTime()
      {
         StringBuilder buff = new StringBuilder( 21 ) ;
         f4dateTimeW( field, buff ) ;
         return buff.ToString(0, Math.Min(20, buff.Length)).TrimEnd('\0');  // CS 2010/08/10
      }
      public int decimals()           { return f4decimals( field ) ; }
      public int init( Data4 d, string name )
      {
         field = d4field( d.dat(), name ) ;
         if (field != IntPtr.Zero)
         {
            db = d.dat() ;
            code4 = d.cod() ;
            return 0 ;
         }
         else
         {
            db = IntPtr.Zero;
            code4 = IntPtr.Zero;
            return -1 ;
         }
      }
      public int init( Data4 d, short j )
      {
         field = d4fieldJ( d.dat(), j ) ;
         if (field != IntPtr.Zero)
         {
            db = d.dat() ;
            code4 = d.cod() ;
            return 0 ;
         }
         else
         {
            db = IntPtr.Zero;
            code4 = IntPtr.Zero;
            return  -1 ;
         }
      }

      public short isBinaryField()
      {
         switch ( f4type( field ) )
         {
            case r4int:
            case r4double:
            case r4currency:
            case r4dateTime:
               /*case r5i2:
               case r5ui2:
               case r5ui4:
               case r5i8:
               case r5ui8:*/
               return 1 ;
            default:
               break ;
         }
         return 0;
      }

      public int isNull()          { return f4null( field ) ; }
      public int isValid() { return (field != IntPtr.Zero ? 1 : 0); }
      public int isTrue()        { return f4true( field ) ; }
      public uint len()      { return f4len( field ) ; }
      public uint len1()     { return f4len( field ) ; }
      public int lockCheck()
      {
         if ( code4accessMode( code4, -5 ) != OPEN4DENY_NONE)
            return r4success ;
         if ( c4getLockEnforce( code4 ) == 0 || d4recNoLow( db ) <= 0 )
            return r4success ;
         // Check to see if the record is locked.
         if ( d4lockTest( db, d4recNoLow( db ), lock4write ) != 0 )
            return r4success ;
         return throwError( e4lock, E60201 ) ;
      }

      public string name()
      {
         /* LY 2002/09/24 : .NET corrupting FIELD4 struct when using
            f4name() (requires calling ::name() on >6 fields to appear) */
         StringBuilder nameBuff = new StringBuilder( 10 ) ;
         f4nameW( field, nameBuff ) ;
         if ( nameBuff.Length < 10 )
            return nameBuff.ToString() ;
         else
            return nameBuff.ToString( 0, 10 ) ;
      }

      public int number()           { return f4number( field ) ; }

      public char type()            { return f4type( field ) ; }
      public string str()
      {
         int tempLen = (int) this.len() ;
         byte[] strBuff = new byte[tempLen];
         switch ( f4type( field ) )
         {
            case r4int:
               int ival = f4int( field ) ;
               return ival.ToString() ;
            case r4currency:
               if ( code4indexFormat( code4 ) == r4cdx )
               {
                  StringBuilder buff = new StringBuilder( 20 ) ;
                  f4currencyW( field, 2, buff ) ;
                  return buff.ToString() ;
               }
               else
               {
                  f4strW( field, strBuff ) ;
                  return System.Text.Encoding.Default.GetString(strBuff);
               }
            case r4dateTime:
               if ( code4indexFormat( code4 ) == r4cdx )
               {
                  StringBuilder buff = new StringBuilder( 16 ) ;
                  f4dateTimeW( field, buff ) ;
                  return buff.ToString() ;
               }
               else
               {
                  f4strW( field, strBuff ) ;
                  return System.Text.Encoding.Default.GetString(strBuff);
               }
            case r4double:
               if ( code4indexFormat( code4 ) == r4cdx )
               {
                  double dval = f4double( field ) ;
                  return dval.ToString() ;
               }
               else
               {
                  f4strW( field, strBuff ) ;
                  return System.Text.Encoding.Default.GetString(strBuff);
               }
            case r4unicode:
               // CS 2006/12/12 Trim trailing nulls.
               if ( code4indexFormat( code4 ) == r4cdx )
               {
                  String buff = new String( ' ', tempLen / 2 + tempLen % 2 ) ;
                  f4strUnicodeW( field, buff ) ;
                  return buff.TrimEnd('\0') ;
               }
               else
               {
                  f4strW( field, strBuff ) ;
                  return System.Text.Encoding.Default.GetString(strBuff);
               }
            default :
               f4strW( field, strBuff ) ;
               return System.Text.Encoding.Default.GetString(strBuff);
         }
      }

   } ;


   public class Field4info : Error4
   {
      private IntPtr fldInfo ;
      private int fldInfoSize ;

      [DllImport("c4dll.dll")]
      private static extern IntPtr f4infoAdd(
         IntPtr code4,
         IntPtr fldInfo,
         [MarshalAs(UnmanagedType.I4)] int fldInfoSize,
         [MarshalAs(UnmanagedType.LPStr)] string name,
         [MarshalAs(UnmanagedType.U2)] char type,
         [MarshalAs(UnmanagedType.I4)] int len,
         [MarshalAs(UnmanagedType.I4)] int dec,
         [MarshalAs(UnmanagedType.I4)] int nulls ) ;
      [DllImport("c4dll.dll")]
      private static extern IntPtr f4infoInit(
         IntPtr code4,
         [MarshalAs(UnmanagedType.LPStr)] string name,
         [MarshalAs(UnmanagedType.U2)] char type,
         [MarshalAs(UnmanagedType.I4)] int len,
         [MarshalAs(UnmanagedType.I4)] int dec,
         [MarshalAs(UnmanagedType.I4)] int nulls ) ;
      [DllImport("c4dll.dll")]
      private static extern int f4infoDel(
         IntPtr code4,
         IntPtr fldInfo,
         [MarshalAs(UnmanagedType.I4)] int fldInfoSize,
         [MarshalAs(UnmanagedType.I4)] int fldNum ) ;
      [DllImport("c4dll.dll")]
      private static extern int f4infoFree(
         IntPtr fldInfo,
         [MarshalAs(UnmanagedType.I4)] int fldInfoSize ) ;
      [DllImport("c4dll.dll")]
      private static extern void f4infoName(
         IntPtr code4,
         IntPtr fldInfo,
         [MarshalAs(UnmanagedType.I4)] int fldNum,
         StringBuilder buff) ;

      public Field4info( ref Code4 code )
      {
         code4 = code.cod() ;
         fldInfo = IntPtr.Zero;
         fldInfoSize = 0 ;
      }

      public Field4info( Data4 d )
      {
         if (d.dat() == IntPtr.Zero)
         {
            Error4 err = new Error4() ;
            err.throwError( Error4.e4struct, Error4.E60282 ) ;
         }
         fldInfo = IntPtr.Zero;
         fldInfoSize = 0 ;
         code4 = d.cod() ;
         add( d ) ;
      }

      ~Field4info()
      {
         if (fldInfo != IntPtr.Zero)
            free() ;
      }
      public int add( string name, char type, int len, int dec, int options )
      {
         IntPtr temp = f4infoAdd( code4, fldInfo, fldInfoSize, name, type, len, dec, options ) ;
         if (temp != IntPtr.Zero)
         {
            fldInfo = temp ;
            fldInfoSize++ ;
            return 0 ;
         }
         else
            return -1 ;
      }
      public int add( Field4 field)
      {
         return add( field.name(), field.type(), (int)field.len(), field.decimals(), (ushort)field.isNull() ) ;
      }
      public int del( string alias )
      {
         StringBuilder nameBuff = new StringBuilder( 10 ) ;

         for( int i = 0 ; i < fldInfoSize ; i++ )
         {
            f4infoName( code4, fldInfo, i, nameBuff ) ;
            if ( nameBuff.ToString().ToUpper() == alias.ToUpper() )
            {
               del( i ) ;
               return 0 ;
            }
         }
         error( e4fieldName, E60281, alias, "", "" ) ;
         return -1 ;
      }
      public int add( Data4 data)
      {
         IntPtr f ;
         short i, numFields ;
         //char[] temp ;
         /* LY 2002/09/24 : changed from string */
         StringBuilder currName = new StringBuilder( 10 ) ;

         numFields = d4numFields( data.dat() ) ;
         for( i = 1 ; i <= numFields ; i++ )
         {
            f = d4fieldJ( data.dat(), i ) ;
            f4nameW( f, currName ) ;   /* LY 2002/09/24 : changed from f4name() */
            /* LY 2002/09/24 : added ::toString() */
            if ( add( currName.ToString(), f4type( f ), (int)f4len( f ),
               f4decimals(f), (ushort)f4null( f ) ) < 0 )
               return -1 ;
         }
         return 0 ;
      }

      public int del( int index )
      {
         if( index >= fldInfoSize || index < 0 )
         {
            throwError( e4parm, E60281 ) ;
            return -1 ;
         }

         if ( f4infoDel( code4, fldInfo, fldInfoSize, index ) == 0 )
         {
            fldInfoSize-- ;
            return 0 ;
         }
         else
            return -1 ;
      }

      public void free()
      {
         f4infoFree( fldInfo, fldInfoSize ) ;
         fldInfo = IntPtr.Zero;
      }

      public int numFields()      { return fldInfoSize ; }
      public IntPtr fields() { return fldInfo ; }
   } ;

   public class Tag4info : Error4
   {
      [DllImport("c4dll.dll")]
      private static extern IntPtr d4nextIndex(IntPtr code4, IntPtr data4, IntPtr index4);
      [DllImport("c4dll.dll")]
      private static extern IntPtr i4tagInfo(IntPtr index4) ;
      [DllImport("c4dll.dll")]
      private static extern IntPtr t4infoAdd(
         IntPtr code4,
         IntPtr tag4info,
         [MarshalAs(UnmanagedType.I4)] int size,
         [MarshalAs(UnmanagedType.LPStr)] string name,
         [MarshalAs(UnmanagedType.LPStr)] string expr,
         [MarshalAs(UnmanagedType.LPStr)] string filter,
         [MarshalAs(UnmanagedType.I2)] short unique,
         [MarshalAs(UnmanagedType.U2)] ushort desc ) ;
      [DllImport("c4dll.dll")]
      private static extern int t4infoDel(
         IntPtr code4,
         IntPtr tag4info,
         [MarshalAs(UnmanagedType.I4)] int size,
         [MarshalAs(UnmanagedType.I4)] int index ) ;
      [DllImport("c4dll.dll")]
      private static extern ushort t4infoDescend(
         IntPtr code4,
         IntPtr tag4info,
         [MarshalAs(UnmanagedType.I4)] int index ) ;
      [DllImport("c4dll.dll")]
      private static extern void t4infoExpr(
         IntPtr code4,
         IntPtr tag4info,
         [MarshalAs(UnmanagedType.I4)] int index,
         StringBuilder buff ) ;
      [DllImport("c4dll.dll")]
      private static extern void t4infoFilter(
         IntPtr code4,
         IntPtr tag4info,
         [MarshalAs(UnmanagedType.I4)] int index,
         StringBuilder buff ) ;
      [DllImport("c4dll.dll")]
      private static extern void t4infoFree(
         IntPtr tag4info,
         [MarshalAs(UnmanagedType.I4)] int size ) ;
      [DllImport("c4dll.dll")]
      private static extern void t4infoFreeInternal(
         IntPtr tag4info);
      [DllImport("c4dll.dll")]
      private static extern void t4infoName(
         IntPtr code4,
         IntPtr tag4info,
         [MarshalAs(UnmanagedType.I4)] int index,
         StringBuilder buff ) ;
      [DllImport("c4dll.dll")]
      private static extern int t4infoSize(
         IntPtr code4,
         IntPtr tag4info ) ;
      [DllImport("c4dll.dll")]
      private static extern short t4infoUnique(
         IntPtr code4,
         IntPtr tag4info,
         [MarshalAs(UnmanagedType.I4)] int index ) ;
      [DllImport("c4dll.dll")]
      private static extern IntPtr t4tagInfo(
         IntPtr tag4);

      private IntPtr tagInfo ;
      private int tagInfoSize ;

      public Tag4info( Code4 code )
      {
         tagInfoSize = 0 ;
         code4 = code.cod() ;
         tagInfo = IntPtr.Zero ;
      }
      public Tag4info( Data4 d )
      {
         IntPtr index4ptr = IntPtr.Zero;
         if (d.dat() == IntPtr.Zero)
            throwError( e4struct, E60983 ) ;

         tagInfoSize = 0 ;
         code4 = d.cod() ;
         tagInfo = IntPtr.Zero;

         for( index4ptr = d4nextIndex( code4, d.dat(), index4ptr ) ;
            index4ptr != IntPtr.Zero;
            index4ptr = d4nextIndex( code4, d.dat(), index4ptr ) )
         {
            addIndex_tags( index4ptr ) ;
         }
      }
      public Tag4info( Index4 index )
      {
         if (index.idx() == IntPtr.Zero)
            throwError( e4struct, E60983 ) ;

         tagInfoSize = 0 ;
         code4 = index.cod() ;
         tagInfo = IntPtr.Zero;

         addIndex_tags( index.idx() ) ;
      }
      ~Tag4info()
      {
         if (tagInfo != IntPtr.Zero)
            free() ;
      }

      public int add( string name, string expr, string filter, short uniq, short desc )
      {
         IntPtr temp = t4infoAdd( code4, tagInfo, tagInfoSize, name, expr, filter, uniq, (ushort)desc ) ;

         if (temp != IntPtr.Zero)
         {
            tagInfo = temp ;
            tagInfoSize++ ;
            return 0 ;
         }

         return -1 ;
      }

      public int add( Tag4 tag )
      {
         IntPtr tagInfoInternal ;
         int i = 0 ;
         string name ;
         //string expr, filter ;
         StringBuilder tempName = new StringBuilder( 10 ) ;
         StringBuilder tempExpr = new StringBuilder( 512 ) ;
         StringBuilder tempFilter = new StringBuilder( 512 ) ;

         if ( tag.isValid() == 0 )
         {
            throwError( e4struct, E60991 ) ;
            return -1 ;
         }

         tagInfoInternal = t4tagInfo( tag.tg() ) ;
         if (tagInfoInternal == IntPtr.Zero)
         {
            throwError( e4struct, E60991 ) ;
            return -1 ;
         }

         name = tag.alias() ;
         t4infoName( code4, tagInfoInternal, i, tempName ) ;
         while( name.ToUpper() != tempName.ToString().ToUpper()
            || i < tagInfoSize )
         {
            i++ ;
            t4infoName( code4, tagInfoInternal, i, tempName ) ;
         }

         t4infoExpr( code4, tagInfoInternal, i, tempExpr ) ;
         t4infoFilter( code4, tagInfoInternal, i, tempFilter ) ;
         add( name, tempExpr.ToString(), tempFilter.ToString(), t4infoUnique( code4, tagInfoInternal, i),
            (short)t4infoDescend( code4, tagInfoInternal, i ) ) ;
         t4infoFreeInternal( tagInfoInternal ) ;

         return 0 ;
      }

      private int addIndex_tags( IntPtr index4 )
      {
         IntPtr tagInfoInternal;
         int i, numTags ;
         // string currName, currExpr, currFilter ;
         StringBuilder tempName = new StringBuilder( 10 ) ;
         StringBuilder tempExpr = new StringBuilder( 512 ) ;
         StringBuilder tempFilter = new StringBuilder( 512 ) ;

         tagInfoInternal = i4tagInfo( index4 ) ;
         if (tagInfoInternal == IntPtr.Zero)
            throwError( e4struct, E60984 ) ;
         numTags = t4infoSize( code4, tagInfoInternal ) ;
         for ( i = 0 ; i < numTags ; i++ )
         {
            t4infoName( code4, tagInfoInternal, i, tempName ) ;
            t4infoExpr( code4, tagInfoInternal, i, tempExpr ) ;
            t4infoFilter( code4, tagInfoInternal, i, tempFilter ) ;
            add( tempName.ToString(), tempExpr.ToString(), tempFilter.ToString(),
               t4infoUnique( code4, tagInfoInternal, i ),
               (short)t4infoDescend( code4, tagInfoInternal, i ) ) ;
         }
         t4infoFreeInternal( tagInfoInternal ) ;

         return 0 ;
      }

      public int del( int index )
      {
          if (code4 == IntPtr.Zero)
         {
            throwError( e4struct, E60982 ) ;
            return -1 ;
         }
         if ( index >= t4infoSize( code4, tagInfo ) || index < 0 )
         {
            throwError( e4parm, E60982 ) ;
            return -1 ;
         }
         if ( t4infoDel( code4, tagInfo, tagInfoSize, index ) == 0 )
         {
            tagInfoSize-- ;
            return 0 ;
         }
         else
            return -1 ;
      }
      public int del( string name )
      {
         int i ;
         string temp = name.ToUpper() ;
         StringBuilder tempName = new StringBuilder( 10 ) ;
         temp.PadRight( 10, ' ' ) ;

         for( i = 0 ; i < tagInfoSize ; i++ )
         {
            t4infoName( code4, tagInfo, i, tempName ) ;
            if ( temp.Substring( 0, 10 ).ToUpper() == tempName.ToString().ToUpper() )
            {
               del( i ) ;
               return 0 ;
            }
         }

         error( e4parm, E60982, name, "", "" ) ;
         return -1 ;
      }

      public void free()
      {
         t4infoFree( tagInfo, tagInfoSize ) ;
         tagInfo = IntPtr.Zero;
      }
      public int  numTags()   { return tagInfoSize ; }
      public IntPtr tags() { return tagInfo ; }
   } ;


   public class Field4memo : Error4
   {
      private IntPtr field ;
      private IntPtr db ;
      private int currentMemoPos, finalMemoLen ;

      [DllImport("c4dll.dll")]
      private static extern void f4memoAssign(
         IntPtr field4,
         [MarshalAs(UnmanagedType.LPStr)] string val
         ) ;
      [DllImport("c4dll.dll")]
      private static extern int f4memoAssignBytesW(
         IntPtr field4,
         [MarshalAs(UnmanagedType.LPArray)] byte[] val,
         [MarshalAs(UnmanagedType.U4)] uint len
         ) ;
      [DllImport("c4dll.dll")]
      private static extern void f4memoAssignN(
         IntPtr field4,
         [MarshalAs(UnmanagedType.LPStr)] string val,
         [MarshalAs(UnmanagedType.U4)] uint len
         ) ;
      [DllImport("c4dll.dll")]
      private static extern void f4memoAssignUnicode(
         IntPtr field4,
         [MarshalAs(UnmanagedType.LPWStr)] string val
         ) ;
      [DllImport("c4dll.dll")]
      private static extern uint f4memoBytesW(
         IntPtr field4,
         [MarshalAs(UnmanagedType.LPArray)] byte[] val,
         [MarshalAs(UnmanagedType.U4)] uint len
         ) ;
      [DllImport("c4dll.dll")]
      private static extern void f4memoChanged(
         IntPtr field4
         ) ;
      [DllImport("c4dll.dll")]
      private static extern void f4memoFree(
         IntPtr field4
         ) ;
      [DllImport("c4dll.dll")]
      private static extern uint f4memoLen(
         IntPtr field4
         ) ;
      [DllImport("c4dll.dll")]
      private static extern void f4memoReadW(
         IntPtr field4,
         [MarshalAs(UnmanagedType.U4)] uint readLen,
         [MarshalAs(UnmanagedType.U4)] uint startPos,
         StringBuilder buff
         ) ;
      [DllImport("c4dll.dll")]
      private static extern int f4memoSetLenW(
         IntPtr field4,
         [MarshalAs(UnmanagedType.U4)] uint newLen
         ) ;
      [DllImport("c4dll.dll")]   /* LY 2002/09/25 : replaced f4memoStr() */
      private static extern void f4memoStrW(
         IntPtr field4,
         byte[] buff
         );
      [DllImport("c4dll.dll")]   /* LY 2002/09/25 : replaced f4memoStr() */
      private static extern int f4memoReadPart(
         IntPtr field4,
         byte[] buff,
         [MarshalAs(UnmanagedType.I4)] int readLen,
         [MarshalAs(UnmanagedType.I4)] int startPos
         );
      [DllImport("c4dll.dll")]
      private static extern void f4memoUnicodeStrW(
         IntPtr field4,
         [MarshalAs(UnmanagedType.LPWStr)] string buff
         ) ;
      [DllImport("c4dll.dll")]
      private static extern int f4memoWritePart(
         IntPtr field4,
         [MarshalAs(UnmanagedType.LPStr)] string dataToWrite,
         [MarshalAs(UnmanagedType.U4)] uint dataLen,
         [MarshalAs(UnmanagedType.I4)] int memoLen,
         [MarshalAs(UnmanagedType.I4)] int offset
         ) ;
      [DllImport("c4dll.dll")]
      private static extern int f4memoWriteFinish(
         IntPtr data4
         ) ;

      public IntPtr cod()              { return code4 ; }
      public IntPtr dat()              { return db ; }
      public IntPtr fld()              { return field ; }

      public Field4memo( Data4 d, short i )
      {
         field =  d4fieldJ( d.dat(), i ) ;
         if (field != IntPtr.Zero)
         {
            db = d.dat() ;
            code4 = d.cod() ;
         }
      }
      public Field4memo( Data4 d, string name )
      {
         field = d4field( d.dat(), name ) ;
         if (field != IntPtr.Zero)
         {
            db = d.dat() ;
            code4 = d.cod() ;
         }
      }
      public Field4memo( Field4 f )
      {
         field = f.fld() ;
         if (field != IntPtr.Zero)
         {
            db = f.dat() ;
            code4 = f.cod() ;
         }
      }
      public Field4memo( Field4memo f )
      {
         field = f.fld() ;
         if (field != IntPtr.Zero)
         {
            db = f.dat() ;
            code4 = f.cod() ;
         }
      }

      public void assign( string data )   { f4memoAssign( field, data ) ; }
      public void assign( string data, uint len )   { f4memoAssignN( field, data, len ) ; }
      public int assignBytes( byte[] data, uint len )   { return f4memoAssignBytesW( field, data, len ) ; }
      public uint bytes( byte[] data, uint len )   { return f4memoBytesW( field, data, len ) ; }
      public void changed()
      {
         if (field == IntPtr.Zero)
            throwError( e4struct, E60523 ) ;
         else
            f4memoChanged( field ) ;
      }

      public void free()               { f4memoFree( field ) ; }
      public int isNull()          { return f4null( field ) ; }
      public int isValid() { return (field != IntPtr.Zero ? 1 : 0); }
      public uint len()                { return f4memoLen( field ) ; }
      public int setLen( uint newLen )
      {
         if (field == IntPtr.Zero)
            return throwError( e4struct, E60525 ) ;
         else
            return f4memoSetLenW( field, newLen ) ;
      }

      public void assignUnicode( string str )  { f4memoAssignUnicode( field, str ) ; }

      //      char S4PTR*ptr()        { return f4memoPtr( field ) ; }
      //      char const S4PTR*ptr1() const { return f4memoPtr( field ) ; }
      public string str()
      {
         int tempLen = (int)this.len();
         byte[] buff = new byte[tempLen];
         f4memoStrW(field, buff);
         return System.Text.Encoding.Default.GetString(buff);
      }

      public string str(int readLen, int startPos)
      {
         byte[] buff = new byte[readLen];
         int bytesRead = f4memoReadPart(field, buff, readLen, startPos);
         return System.Text.Encoding.Default.GetString(buff);
      }

      public string strUnicode()
      {
         int tempLen = (int)this.len() ;
         string buff = new String( ' ', tempLen / 2 + tempLen % 2 ) ;
         f4memoUnicodeStrW( field, buff ) ;
         return buff ;
      }

      // AS 03/29/00 Added partial read/write support independent of normal mechanism
      public string read( uint readLen, uint startPos )
      {
         StringBuilder buff = new StringBuilder( (int)readLen + 1 ) ;
         f4memoReadW( field, readLen, startPos, buff ) ;
         return buff.ToString() ;
      }
      public int write( string ptrToData, uint lenToWrite )
      {
         int rc = f4memoWritePart( field, ptrToData, lenToWrite, finalMemoLen, currentMemoPos ) ;
         currentMemoPos += (int)lenToWrite ;
         return rc ;
      }
      public void writeStart( int memoLen ) { currentMemoPos = 0 ; finalMemoLen = memoLen ; }
      public int writeFinish() { return f4memoWriteFinish( db ) ; }
   } ;

   /*  public class Date4 : Error4
     {
        private int dt ;  // C char pointer
        private int result ; // for ::format(), ::timeNow()

        [DllImport("c4dll.dll")]
        private static extern int date4alloc( [MarshalAs(UnmanagedType.LPStr)]UCT int code4, [MarshalAs(UnmanagedType.LPStr)] string pict ) ;
        [DllImport("c4dll.dll")]
        private static extern void date4assign( int ptr, [MarshalAs(UnmanagedType.I4)] int d ) ;
        [DllImport("c4dll.dll")]
        private static extern void date4assignW( int ptr, [MarshalAs(UnmanagedType.LPStr)] string d ) ;
        [DllImport("c4dll.dll")]
        private static extern string date4cdow( int ptr ) ;
        [DllImport("c4dll.dll")]
        private static extern string date4cmonth( int ptr ) ;
        [DllImport("c4dll.dll")]
        private static extern int date4day( int ptr ) ;
        [DllImport("c4dll.dll")]
        private static extern int date4dow( int ptr ) ;
        [DllImport("c4dll.dll")]
        private static extern void date4init( int ptr, [MarshalAs(UnmanagedType.LPStr)] string d, [MarshalAs(UnmanagedType.LPStr)] string pict ) ;
        [DllImport("c4dll.dll")]
        private static extern double date4formatMdx( int ptr ) ;
        [DllImport("c4dll.dll")]
        private static extern string date4formatW( int ptr, int result, [MarshalAs(UnmanagedType.LPStr)] string pict ) ;
        [DllImport("c4dll.dll")]
        private static extern int date4long( int ptr ) ;
        [DllImport("c4dll.dll")]
        private static extern int date4month( int ptr ) ;
        [DllImport("c4dll.dll")]
        private static extern string date4timeNowW( int ptr ) ;
        [DllImport("c4dll.dll")]
        private static extern void date4today( int ptr ) ;
        [DllImport("c4dll.dll")]
        private static extern int date4year( int ptr ) ;

        public Date4()
        {
           dt = date4alloc( code4, "CCYYMMDD" ) ;
        }
        ~Date4()
        {
           if ( dt != 0 )
              u4free( dt ) ;
        }
        public Date4( int l )
        {
           dt = date4alloc( code4, "CCYYMMDD" ) ;
           if ( dt != 0 )
              date4assign(dt, l) ;
        }
        public Date4( string d )
        {
           dt = date4alloc( code4, "CCYYMMDD" ) ;
           if ( dt != 0 )
              date4assignW( dt, d ) ;
        }
        public Date4( string d, string format )
        {
           dt = date4alloc( code4, "CCYYMMDD" ) ;
           if ( dt != 0 )
              date4init( dt, d, format ) ;
        }
        public Date4( Date4 s )
        {
           dt = date4alloc( code4, "CCYYMMDD" ) ;
           if ( dt != 0 )
              date4assign( dt, s.getInt() ) ;
        }
  // can't overload Date4 &operator =( const Str4 &s ) { assign( s ) ; return *this ; }
        public int getInt()
        {
           if ( dt != 0 )
              return date4long( dt ) ;
           else
              return 0 ;
        }
        public double getdouble()
        {
           if ( dt != 0 )
              return date4formatMdx( dt ) ;
           else
              return 1.0E100 ;
        }
        public static int operator +( Date4 d, int l )
        {
           if ( d.dt != 0 )
              return ( date4long( d.dt )+l ) ;
           else
              return 0 ;
        }
        public static int operator -( Date4 d, int l )
        {
           if ( d.dt != 0 )
              return ( date4long( d.dt )-l ) ;
           else
              return 0 ;
        }
  // can't overload void operator +=( const long l ) { date4assign( ptr(), date4long( ptr() )+l ) ; }
        public void add( int l )
        {
           if ( dt != 0 )
              date4assign( dt, date4long( dt ) + l ) ;
        }
  //   "     "      void operator -=( const long l ) { date4assign( ptr(), date4long( ptr() )-l ) ; }
        public void subtract( int l )
        {
           if ( dt != 0 )
              date4assign( dt, date4long( dt ) - l ) ;
        }

  // problem: unary operators ++ and -- must return same type as parameter,
  //          but C++ Date ++ and -- return long values
  //      public static Date4 operator ++( Date4 d )     { d.add( 1 ) ; return d.getInt() ; }
  //      public static Date4 operator --( Date4 d )     { d.subtract( 1 ) ; return d.getInt() ; }
        public void assign( int l )
        {
           if ( dt != 0 )
              date4assign( dt , l ) ;
        }
        public void assign( string d )
        {
           if ( dt != 0 )
              date4assignW( dt, d ) ;
        }
        public void assign( string p, string pict )
        {
           if ( dt != 0 )
              date4init( dt, p, pict ) ;
        }
  //      void assign( Str4 &s )           { Str4::assign( s ) ; }
        public string cdow()
        {
           if ( dt != 0 )
              return date4cdow( dt ) ;
           else
              return " " ;
        }
        public string cmonth()
        {
           if ( dt != 0 )
              return date4cmonth( dt ) ;
           else
              return " " ;
        }
        public int day()
        {
           if ( dt != 0 )
              return date4day( dt ) ; // Day of month  ( 1-31 )
           else
              return 0 ;
        }
        public int dow()
        {
           if ( dt != 0 )
              return date4dow( dt ) ; // Day of week   ( 1-7 )
           else
              return 0 ;
        }
        public string format( string pict )
        {
           result = date4alloc( cb, pict ) ;
           if ( result != 0 )
           {
              string temp = date4formatW( dt, result, pict ) ;
              u4free( result ) ;
              return temp ;
           }
           else
              return " " ;
        }
        public int isLeap()
        {
           if ( dt != 0 )
           {
              int y = date4year( dt ) ;
              return ( y%4 == 0 && y%100 != 0 || y%400 == 0 ) ?  1 : 0 ;
           }
           else
              return -1 ;
        }
  //      unsigned long len() const        { return 8 ; }
  //      unsigned long len1() const       { return 8 ; }
        public int month()
        {
           if ( dt != 0 )
              return date4month( dt ) ;  // Month of year ( 1-12 )
           else
              return 0 ;
        }
        public string timeNow()
        {
           result = date4alloc( cb, "HH:MM:SS" ) ;
           if ( result != 0 )
           {
              string temp = date4timeNowW( result ) ;
              u4free( result ) ;
              return temp ;
           }
           else
              return " " ;
        }
        public void today()              { date4today( dt ) ; }
        public int year()                { return date4year( dt ) ; }
  //      char S4PTR*ptr()                 { return  dt ; }
  //      char const S4PTR*ptr1() const    { return  dt ; }
     } ;*/

   public class Relate4 : Error4
   {
      [DllImport("c4dll.dll")]
      private static extern IntPtr relate4createSlave(
         IntPtr relate4,
         IntPtr data4,
         [MarshalAs(UnmanagedType.LPStr)] string masterExpr,
         IntPtr tag );
      [DllImport("c4dll.dll")]
      private static extern IntPtr relate4dataCB( IntPtr relate4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int relate4dataTag( IntPtr relate4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int relate4doOne( IntPtr relate4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int relate4errorAction( IntPtr relate4, int value ) ;
      [DllImport("c4dll.dll")]
      private static extern IntPtr relate4masterCB( IntPtr relate4 ) ;
      [DllImport("c4dll.dll")]   /* LY 2002/09/25 : replaced relate4masterExprCB() */
      private static extern void relate4masterExprW( IntPtr relate4, StringBuilder buff ) ;
      [DllImport("c4dll.dll")]
      private static extern int relate4matchLen( IntPtr relate4, int value ) ;
      [DllImport("c4dll.dll")]
      private static extern int relate4type( IntPtr relate4, int value ) ;
      [DllImport("c4dll.dll")]
      protected static extern int relate4next( ref IntPtr relate4 ) ;

      public Relate4() { relate = IntPtr.Zero; }
      public Relate4( Relate4 master, Data4 slave, string masterExpr, Tag4 t )
      { relate = relate4createSlave( master.rl(), slave.dat(), masterExpr, t.tg() ) ; }
      public Relate4( Relate4 r ) { relate = r.rl() ; }
      public Relate4( IntPtr rel ) { relate = rel ; }
      public Data4 data()
      {
         if ( relate != IntPtr.Zero )
            return new Data4( relate4dataCB(relate) ) ;
         else
            return new Data4() ;
      }
      public Tag4 dataTag()
      {
         if (relate == IntPtr.Zero)
            return new Tag4() ;

         Data4 dataTemp = new Data4( relate4dataCB( relate ) ) ;
         return new Tag4( dataTemp, "" ) ;
      }
      public int doOne()  { return relate4doOne( relate ) ; }
      public int errorAction( int a ) { return relate4errorAction( relate, a ) ; }
      public int init( Relate4 master, Data4 slave, string masterExpr, Tag4 t )
      {
         relate = relate4createSlave( master.relate, slave.dat(), masterExpr, t.tg() ) ;
         code4 = slave.cod() ;
         return code4errorCode( code4, -5 ) ;
      }

      public int isValid() { return (relate != IntPtr.Zero ? 1 : 0); }
      public Relate4 master()
      {
         if (relate != IntPtr.Zero)
            return new Relate4( relate4masterCB(relate) );
         else
            return new Relate4( ) ;
      }

      public string masterExpr()
      {
         StringBuilder buff = new StringBuilder( 512 ) ;
         relate4masterExprW( relate, buff ) ;
         return buff.ToString() ;
      }

      public int matchLen( int p ) { return relate4matchLen( relate, p ) ; }
      public int type( int p )   { return relate4type( relate, p ) ; }

      public IntPtr rl() { return relate ; }

      protected IntPtr relate ;
   } ;

   public class Relate4iterator : Relate4
   {
      public Relate4iterator() { relate = IntPtr.Zero; }
      public Relate4iterator( Relate4 r )   { relate = r.rl() ; }

      public int next()           { return ( relate4next( ref relate ) != 2 ? 1 : 0 ) ; }
      public int nextPosition()   { return relate4next( ref relate ) ; }
   } ;

   public class Relate4set: Relate4
   {
      [DllImport("c4dll.dll")]
      private static extern int relate4bottom( IntPtr relate4 ) ;
      [DllImport("c4dll.dll")]
      private static extern void relate4changed( IntPtr relate4 ) ;
      [DllImport("c4dll.dll")]
      private static extern uint relate4count( IntPtr relate4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int relate4doAll( IntPtr relate4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int relate4eof( IntPtr relate4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int relate4free( IntPtr relate4, int value ) ;
      [DllImport("c4dll.dll")]
      private static extern IntPtr relate4init( IntPtr data4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int relate4lockAdd( IntPtr relate4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int relate4optimizeable( IntPtr relate4 ) ;
      [DllImport("c4dll.dll")]
      private static extern int relate4querySet( IntPtr relate4, [MarshalAs(UnmanagedType.LPStr)] string query ) ;
      [DllImport("c4dll.dll")]
      private static extern int relate4skip( IntPtr relate4, [MarshalAs(UnmanagedType.I4)] int l ) ;
      [DllImport("c4dll.dll")]
      private static extern int relate4skipEnable( IntPtr relate4, [MarshalAs(UnmanagedType.I4)] int doEnable ) ;
      [DllImport("c4dll.dll")]
      private static extern int relate4sortSet( IntPtr relate4, [MarshalAs(UnmanagedType.LPStr)] string sort ) ;
      [DllImport("c4dll.dll")]
      private static extern int relate4top( IntPtr relate4 ) ;

      public Relate4set( Data4 data ) { relate = relate4init( data.dat() ) ; }
      public Relate4set() { relate = IntPtr.Zero; }
      public Relate4set( Relate4 r ) { relate = r.rl() ; }

      public int bottom()             { return relate4bottom( relate ) ; }
      public void changed()           { relate4changed( relate ) ; }
      public uint count()             { return relate4count( relate ) ; }
      public int doAll()              { return relate4doAll( relate ) ; }
      public int eof()                { return relate4eof( relate ) ; }
      public int free() { int rc = relate4free(relate, 0); relate = IntPtr.Zero; return rc; }
      public int free(int p) { int rc = relate4free(relate, p); relate = IntPtr.Zero; return rc; }
      public int init( Data4 data )
      {
         relate = relate4init( data.dat() ) ;
         code4 = data.cod() ;
         return code4errorCode( code4, -5 ) ;
      }
      public int lockAdd()  { return relate4lockAdd( relate ) ; }
      public int optimizeable()    { return relate4optimizeable( relate ) ; }
      public int querySet( string p )  { return relate4querySet( relate, p ) ; }
      public int skip( int n )    { return relate4skip( relate, n ) ; }
      public int skip()    { return relate4skip( relate, 1 ) ; }
      public int skipEnable( int doEnable )  { return relate4skipEnable( relate, doEnable ) ; }
      public int sortSet( string sort )   { return relate4sortSet( relate, sort ) ; }
      public int top()                    { return relate4top( relate ) ; }
   } ;

}

