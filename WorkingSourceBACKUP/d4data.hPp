/* d4data.hpp (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#ifndef __DATA4HEADER
   #define __DATA4HEADER

#ifdef __BORLANDC__
   #pragma warn -hid
#endif

#if defined(_MSC_VER) && !defined(S4WINCE) && !defined(S4WIN64)
   #if _MSC_VER >= 900
      #pragma pack(push,1)
   #else
      #pragma pack(1)
   #endif
#else
   #ifdef __BORLANDC__
      #pragma pack(1)
   #endif
#endif

#ifdef S4WIN32
   #define s4memset memset
#endif
#ifdef S4OS2
  #ifdef __BORLANDC__
     #define s4memset memset
  #endif
#endif

#ifdef __TURBOC__
  #pragma warn -inl
#endif

#ifndef S4WINTEL
   #define s4memset memset
#endif

#ifndef s4memset
   #define s4memset _fmemset
#endif

#ifndef S4INLINE2
   #define S4INLINE2 inline
#endif

#include "e4string.hpp"

#ifdef E4PARM_HIGH
   #define DEBUG4VOID( a,b )   { if ( a ) { error4( 0, e4struct, ( b ) ); return ; } }
   #define DEBUG4INT( a,b )    { if ( a ) return error4( 0, e4struct, ( b ) ) ; }
   #define DEBUG4PTR( a,b )    { if ( a ) { error4( 0, e4struct, ( b ) ) ; return ( 0 ) ; } }
#else
   #define DEBUG4VOID( a,b )
   #define DEBUG4INT( a,b )
   #define DEBUG4PTR( a,b )
#endif

#ifdef D4DLL_CPP
   extern CODE4 *g_c4 ;
   extern HANDLE hG_c4 ;
#endif

class S4CLASS Data4 ;
class S4CLASS Index4 ;
class S4CLASS Str4len ;
class S4CLASS Str4ptr ;
class S4CLASS Field4info ;
class S4CLASS Tag4info ;
class S4CLASS List4 ;
class S4CLASS File4 ;
class S4CLASS Tag4 ;
class S4CLASS Date4 ;
class S4CLASS Code4 ;
class S4CLASS Str4flex ;
class S4CLASS Expr4
{
public:
   EXPR4 S4PTR *expr ;

   Expr4()                    { expr = 0 ; }
   Expr4( EXPR4 S4PTR *ex )   { expr = ex ; }
   Expr4( Data4 d, const char * ) ;
   // CS 2006/09/05 Remove conditional TURBOC code
   operator double() const { return expr4double( expr ) ; }

   Data4 data() const ;
   void free()                { expr4free( expr ) ; expr = 0 ; }
   int len()                  { return expr4len( expr ) ; }
   int parse( Data4, const char * ) ;
   int isValid() const        { return expr != 0 ; }
   short isNull() const       { return expr4null( expr ) ; }
   const char S4PTR*source() const  { return expr4source( expr ) ; }
   const char S4PTR*str()     { return expr4str( expr ) ; }
   int type()                 { return expr4type( expr ) ; }
   const char *vary()         { char *result ; int rc ; rc = expr4vary( expr, &result ) ; return result ; }

   #ifdef S4USE_TRUE
      int true()              { return expr4true( expr ) ; }
   #else
      int isTrue()            { return expr4true( expr ) ; }
   #endif
} ;

class S4CLASS Code4 : public CODE4
{
protected:
   MEM4 *stringTypes[16] ;
   friend class Str4flex ;
   short init_p ;  /* CS 1999/09/21 true if class is initialized */
   #ifdef D4DLL_CPP
      S4INLINE2 int allocDll( const char *dllName ) ;
      S4INLINE2 int initUndoDll() ;
   public:
      Code4( const char *dllName = 0 ) { init_p = 0 ;  if ( dllName ) init( dllName ) ; }

      Code4( Code4& ) ;           /* Illegal operation, force link error */
      Code4 &operator = ( Code4 & ) ;    /* Illegal operation, force link error */

      unsigned short getActionCode() { return code4actionCode( c4 ) ; }
      int calcCreate( Expr4 ex, const char *name ) { return ( code4calcCreate( c4, ex.expr, name ) ) ? r4success : -1 ; }
      void calcReset()            { code4calcReset( c4 ) ; }
      int  closeAll()
         #ifdef D4DLL_CPP
            {
               WaitForSingleObject( hD4List, INFINITE ) ;
               ResetEvent( hD4List ) ;
               closeAllList = 1 ;
               for ( DATA4* d4 = (DATA4 *)l4first( &d4list ); d4 != 0 ; d4 = (DATA4 *)l4first( &d4list ) )
                  d4close( d4 ) ;   /* use wrapper version of d4close() to free memory from d4***() */
               closeAllList = 0 ;
               SetEvent( hD4List ) ;

               return code4close( c4 ) ;
            }
         #else
            { return code4close( c4 ) ; }
         #endif
      short collate(short type)   { return code4collate( c4, type ) ; }
      short collateUnicode(short type)   { return code4collateUnicode( c4, type ) ; }
      int compressConfigure( short fileLevel, short comLevel, short minLength )  { return code4compressConfigure( c4, fileLevel, comLevel, minLength ) ;  }
      int compressConfigureServer( short fileLevel, short comLevel, short minLength )  { return code4compressConfigureServer( c4, fileLevel, comLevel, minLength ) ;  }
      int connect( S4CONST char *serverId=DEF4SERVER_ID, S4CONST char *processId=DEF4PROCESS_ID,
                   S4CONST char *userName = "PUBLIC", S4CONST char *password = 0, S4CONST char *protocol = ( char * )PROT4DEFAULT )
          { return code4connect( c4, serverId, processId, userName, password, protocol ) ; }
      Data4 data( const char * ) ;
      const char S4PTR* dateFormat()  { return (const char *)code4dateFormat( c4 ) ; }
      int dateFormat( const char *format ) { return code4dateFormatSet( c4, format ) ; }
      int error( const int er, const long er2 = 0L, const char *p1=0, const char *p2 = 0, const char *p3 = 0 )
                          { return error4describe( c4, er, er2, p1, p2, p3 ) ; }
      int errorFile( S4CONST char *fileName, int overwrite=1 ) { return error4file( c4,fileName,overwrite ); }
      int errorSet( int c = 0 ) { return error4set( c4, c ) ; }
      const char S4PTR* errorText( const long c ) { return (const char *)error4text( c4, c ) ; }
      void exit()                { code4exit( c4 ) ; }
      void exitTest()            { error4exitTest( c4 ) ; }
      int flushFiles()           { return code4flush( c4 ) ; }
      const char S4PTR* indexExtension()  { return (const char *)code4indexExtension( c4 ) ; }
         S4INLINE2 int   init( const char *dllName ) ;
      void largeOn()             { code4largeOn( c4 ) ; }

      int initUndo()
      {
         int rc = r4success ;
         if (init_p)
         {
            rc = initUndoDll() ;
            if (rc == r4success)
               init_p = 0 ;
         }
         return rc ;
      }

      int lock()                 { return code4lock( c4 ) ; }
      void lockClear()           { code4lockClear( c4 ) ; }
      const char *lockFileName() {return (const char *)code4lockFileName( c4 ); }
      long lockItem()            { return code4lockItem( c4 ) ; }
      #ifdef S4CLIENT
         const char S4PTR* lockUserId()    { return code4lockUserId( c4 ) ; }
         const char S4PTR* lockNetworkId() { return code4lockNetworkId( c4 ) ; }
      #endif
      #ifdef S4SERVER
         const char S4PTR* lockUserId()    { return code4lockUserId( c4 ) ; }
         const char S4PTR* lockNetworkId() { return code4lockNetworkId( c4 ) ; }
      #endif
      #if defined( S4STAND_ALONE ) && !defined( S4OFF_WRITE ) && !defined( S4OFF_TRAN )
         int logCreate( const char *name,const char *userId ) { return code4logCreate( c4, name,userId ) ; }
         const char S4PTR* logFileName() { return (const char *)code4logFileName( c4 ) ; }
         int logOpen( const char *name, const char *userId ) { return code4logOpen( c4, name, userId ) ; }
         void logOpenOff() { code4logOpenOff( c4 ) ; }
      #endif
      short indexBlockSize()  { return code4indexBlockSize( c4 ) ; }
      short indexBlockSize( short val )  { return code4indexBlockSizeSet( c4, val) ; }
      int memoCompress( short flag )  { return code4memoCompress( c4, flag ) ; }
      int optAll()               { return code4optAll( c4 ) ; }
      int optStart()             { return code4optStart( c4 ) ; }
      int optSuspend()           { return code4optSuspend( c4 ) ; }
      long timeout()             { return code4timeout( c4 ); }
      void timeout( long l )     { code4timeoutSet( c4, l ) ; }
      int tranCommit()           { return code4tranCommit( c4 ) ; }
      int tranRollback()         { return code4tranRollback( c4 ) ; }
      int tranStart()            { return code4tranStart( c4 ) ; }
      int tranStatus()           { return  code4tranStatus( c4 ); }
      int unlock()               { return code4unlock( c4 ) ; }
      int unlockAuto()           { return code4unlockAuto( c4 ) ; }
      void unlockAuto( short c ) { code4unlockAutoSet( c4, c ) ; }
      void verifySet( const char *value )   { code4verifySet( c4, value ) ; }

      int getAccessMode()           { return c4getAccessMode( c4 ) ; }
      void setAccessMode( int val ) { c4setAccessMode( c4, val ) ; }
      int getAutoOpen()             { return c4getAutoOpen( c4 ) ; }
      void setAutoOpen( int val )   { c4setAutoOpen( c4, val ) ; }
      int getCodePage()             { return c4getCodePage( c4 ) ; }
      void setCodePage( int val )   { c4setCodePage( c4, val ) ; }
      int getCollatingSequence()    { return code4collatingSequence( c4, -5 ) ; }
      void setCollatingSequence( int val )   { code4collatingSequence( c4, val ) ; }
      short getCompatibility()               { return c4getCompatibility( c4 ) ; }
      void setCompatibility( short val )     { c4setCompatibility( c4, val ) ; }
      Bool5 getCompressedMemos()               { return c4getCompressedMemos( c4 ) ; }
      void setCompressedMemos( Bool5 val )     { c4setCompressedMemos( c4, val ) ; }
      int getCreateTemp()           { return code4createTemp( c4, -5 ) ; }
      void setCreateTemp( int val ) { code4createTemp( c4, val ) ; }
      int getErrCreate()            { return code4errCreate( c4, -5 ) ; }
      void setErrCreate( int val )  { code4errCreate( c4, val ) ; }
      int getErrDefaultUnique()     { return code4errDefaultUnique( c4, -5 ) ; }
      void setErrDefaultUnique( int val ) { code4errDefaultUnique( c4, val ) ; }
      int getErrExpr()              { return code4errExpr( c4, -5 ) ; }
      void setErrExpr( int val )    { code4errExpr( c4, val ) ; }
      int getErrFieldName()         { return code4errFieldName( c4, -5 ) ; }
      void setErrFieldName( int val )  { code4errFieldName( c4, val ) ; }
      int getErrGo()                { return code4errGo( c4, -5 ) ; }
      void setErrGo( int val )      { code4errGo( c4, val ) ; }
      int getErrOff()               { return code4errOff( c4, -5 ) ; }
      void setErrOff( int val )     { code4errOff( c4, val ) ; }
      int getErrOpen()              { return code4errOpen( c4, -5 ) ; }
      void setErrOpen( int val )    { code4errOpen( c4, val ) ; }
      int getErrorCode()            { return code4errorCode( c4, -5 ) ; }
      void setErrorCode( int val )  { code4errorCode( c4, val ) ; }
      int getErrRelate()            { return code4errRelate( c4, -5 ) ; }
      void setErrRelate( int val )  { code4errRelate( c4, val ) ; }
      int getErrSkip()              { return code4errSkip( c4, -5 ) ; }
      void setErrSkip( int val )    { code4errSkip( c4, val ) ; }
      int getErrTagName()           { return code4errTagName( c4, -5 ) ; }
      void setErrTagName( int val ) { code4errTagName( c4, val ) ; }
      int getFileFlush()            { return c4getFileFlush( c4 ) ; }
      void setFileFlush( int val )  { c4setFileFlush( c4, val ) ; }
      HWND getHWnd()                { return (HWND) code4hWnd( c4, -5 ) ; }
      void setHWnd( HWND hWnd )     { code4hWnd( c4, (long)hWnd ) ; }
      int getLockAttempts()         { return code4lockAttempts( c4, -5 ) ; }
      void setLockAttempts( int val )  { code4lockAttempts( c4, val ) ; }
      int getLockAttemptsSingle()   { return code4lockAttemptsSingle( c4, -5 ) ; }
      void setLockAttemptsSingle( int val )  { code4lockAttemptsSingle( c4, val ) ; }
      unsigned int getLockDelay()   { return code4lockDelay( c4, -5 ) ; }
      void setLockDelay( unsigned int val )  { code4lockDelay( c4, val ) ; }
      int getLockEnforce()          { return code4lockEnforce( c4, -5 ) ; }
      void setLockEnforce( int val )   { code4lockEnforce( c4, val ) ; }
      int getLog()                  { return code4log( c4, -5 ) ; }
      void setLog( int val )        { code4log( c4, val ) ; }
      int getMemExpandBlock()       { return code4memExpandBlock( c4, -5 ) ; }
      void setMemExpandBlock( int val )   { code4memExpandBlock( c4, val ) ; }
      int getMemExpandData()        { return code4memExpandData( c4, -5 ) ; }
      void setMemExpandData( int val ) { code4memExpandData( c4, val ) ; }
      int getMemExpandIndex()       { return code4memExpandIndex( c4, -5 ) ; }
      void setMemExpandIndex( int val )   { code4memExpandIndex( c4, val ) ; }
      int getMemExpandLock()        { return code4memExpandLock( c4, -5 ) ; }
      void setMemExpandLock( int val ) { code4memExpandLock( c4, val ) ; }
      int getMemExpandTag()         { return code4memExpandTag( c4, -5 ) ; }
      void setMemExpandTag( int val )  { code4memExpandTag( c4, val ) ; }
      unsigned int getMemSizeBlock()   { return code4memSizeBlock( c4, -5 ) ; }
      void setMemSizeBlock( unsigned int val )  { code4memSizeBlock( c4, val ) ; }
      unsigned int getMemSizeBuffer()  { return code4memSizeBuffer( c4, -5 ) ; }
      void setMemSizeBuffer( unsigned int val ) { code4memSizeBuffer( c4, val ) ; }
      unsigned int getMemSizeMemo() { return code4memSizeMemo( c4, -5 ) ; }
      void setMemSizeMemo( unsigned int val )   { code4memSizeMemo( c4, val ) ; }
      unsigned int getMemSizeMemoExpr()   { return code4memSizeMemoExpr( c4, -5 ) ; }
      void setMemSizeMemoExpr( unsigned int val )  { code4memSizeMemoExpr( c4, val ) ; }
      unsigned int getMemSizeSortBuffer() { return code4memSizeSortBuffer( c4, -5 ) ; }
      void setMemSizeSortBuffer( unsigned int val )   { code4memSizeSortBuffer( c4, val ) ; }
      unsigned int getMemSizeSortPool()   { return code4memSizeSortPool( c4, -5 ) ; }
      void setMemSizeSortPool( unsigned int val )  { code4memSizeSortPool( c4, val ) ; }
      int getMemStartBlock()        { return code4memStartBlock( c4, -5 ) ; }
      void setMemStartBlock( int val ) { code4memStartBlock( c4, val ) ; }
      int getMemStartData()         { return code4memStartData( c4, -5 ) ; }
      void setMemStartData( int val )  { code4memStartData( c4, val ) ; }
      int getMemStartIndex()        { return code4memStartIndex( c4, -5 ) ; }
      void setMemStartIndex( int val ) { code4memStartIndex( c4, val ) ; }
      int getMemStartLock()         { return code4memStartLock( c4, -5 ) ; }
      void setMemStartLock( int val )  { code4memStartLock( c4, val ) ; }
      long getMemStartMax()         { return code4memStartMax( c4, -5 ) ; }
      void setMemStartMax( long val )  { code4memStartMax( c4, val ) ; }
      int getMemStartTag()          { return code4memStartTag( c4, -5 ) ; }
      void setMemStartTag( int val )   { code4memStartTag( c4, val ) ; }
      int getOptimize()             { return c4getOptimize( c4 ) ; }
      void setOptimize( int val )   { c4setOptimize( c4, val ) ; }
      int getOptimizeWrite()        { return c4getOptimizeWrite( c4 ) ; }
      void setOptimizeWrite( int val ) { c4setOptimizeWrite( c4, val ) ; }
      int setReadLock()             { return code4readLock( c4, -5 ) ; }
      void getReadLock( int val )   { code4readLock( c4, val ) ; }
      int getReadOnly()             { return code4readOnly( c4, -5 ) ; }
      void setReadOnly( int val )   { code4readOnly( c4, val ) ; }
      int getSafety()               { return c4getSafety( c4 ) ; }
      void setSafety( int val )     { c4setSafety( c4, val ) ; }
      int getSingleOpen()           { return c4getSingleOpen( c4 ) ; }
      void setSingleOpen( int val ) { c4setSingleOpen( c4, val ) ; }

   #else
   public:
      Code4( int initialize = 1 ) { init_p = 0 ;  if ( initialize ) init() ; }

      Code4( Code4& ) ;           /* Illegal operation, force link error */
      Code4 &operator = ( Code4 & ) ;    /* Illegal operation, force link error */

      unsigned short getActionCode() { return code4actionCode( this ) ; }
      int additionalFunction( long functionNumber, void *infoIn, long infoInLen, void *infoOut, long *infoOutLen )
         { return code4additionalFunction( this, functionNumber, infoIn, infoInLen, infoOut, infoOutLen ) ; }
      // AS Mar 7/03 - support for C++
      void autoIncrementStart( double val ) { code4autoIncrementStart( this, val ) ; }
      int calcCreate( Expr4 ex, const char *name ) { return ( code4calcCreate( this, ex.expr, name ) ) ? r4success : -1 ; }
      void calcReset()            { code4calcReset( this ) ; }
      int  closeAll()             { return code4close( this ) ; }
      short collate(short type)   { return code4collate( this, type ) ; }
      short collateUnicode(short type)   { return code4collateUnicode( this, type ) ; }
      int compress( short flag ) { return code4compress( this, flag ) ; }
      int compressConfigure( short fileLevel, short comLevel, short minComLength )  { return code4compressConfigure( this, fileLevel, comLevel, minComLength ) ; }
      int compressConfigureServer( short fileLevel, short comLevel, short minComLength )  { return code4compressConfigureServer( this, fileLevel, comLevel, minComLength ) ; }
      int connect( S4CONST char *serverId=DEF4SERVER_ID, S4CONST char *processId=DEF4PROCESS_ID,
                   S4CONST char *userName = "PUBLIC", S4CONST char *password = 0, S4CONST char *protocol = ( char * )PROT4DEFAULT )
          { return code4connect( this, serverId, processId, userName, password, protocol ) ; }
      Data4 data( const char * ) ;
      const char S4PTR* dateFormat()  { return code4dateFormat( this ) ; }
      int dateFormat( const char *format ) { return code4dateFormatSet( this, format ) ; }
      int encryptConnection( int level, int numBits ) { return code4encryptConnection( this, level, numBits ) ; }
      int encryptFile( short encryptFlag ) { return code4encryptFile( this, encryptFlag ); }  // CS 2003/04/25 Add
      int encryptInit( const void *key, short keyLen ) { return code4encryptInit( this, key, keyLen ); }  // CS 2003/04/25 Add
      int error( const int er, const long er2 = 0L, const char *p1=0, const char *p2 = 0, const char *p3 = 0 )
                          { return error4describe( this, er, er2, p1, p2, p3 ) ; }
      void errorCallback(ERROR_CALLBACK errorCallback)    { error4callback(this, errorCallback); }  // CS 2003/03/07
      int errorFile( S4CONST char *fileName, int overwrite=1 ) { return error4file( this,fileName,overwrite ); }
      const char S4PTR* errorLastDescription()  { return error4lastDescription( this ) ; }
      int errorSet( int c = 0 )  { return error4set( this, c ) ; }
      const char S4PTR* errorText( const long c ) { return error4text( this, c ) ; }
      void exit()                { code4exit( this ) ; }
      void exitTest()            { error4exitTest( this ) ; }
      int flushFiles()           { return code4flush( this ) ; }
      short indexBlockSize()     { return code4indexBlockSize( this ) ; }
      short indexBlockSize( short blockSize )   { return code4indexBlockSizeSet( this, blockSize ) ; }
      const char S4PTR* indexExtension()  { return code4indexExtension( this ) ; }
      S4INLINE2 int   init() ;
      void largeOn()             { code4largeOn( this ) ; }

      int initUndo()
      {
         int rc = r4success ;
         if (init_p)
         {
            rc = code4initUndo( this ) ;
            if (rc == r4success)
               init_p = 0 ;
         }
         return rc ;
      }

      // AS Jan 26/06 - Support for limiting the key size
      void limitKeySizeSet( short val )   { code4limitKeySizeSet( this, val ) ; }
      int lock()                 { return code4lock( this ) ; }
      void lockClear()           { code4lockClear( this ) ; }
      const char *lockFileName() {return code4lockFileName( this ); }
      long lockItem()            { return code4lockItem( this ) ; }
      #ifdef S4CLIENT
         const char S4PTR* lockUserId()    { return code4lockUserId( this ) ; }
         const char S4PTR* lockNetworkId() { return code4lockNetworkId( this ) ; }
      #endif
      #ifdef S4SERVER
         const char S4PTR* lockUserId()    { return code4lockUserId( this ) ; }
         const char S4PTR* lockNetworkId() { return code4lockNetworkId( this ) ; }
      #endif
      #if defined( S4STAND_ALONE ) && !defined( S4OFF_WRITE ) && !defined( S4OFF_TRAN )
         int logCreate( const char *name,const char *userId ) { return code4logCreate( this, name,userId ) ; }
         const char S4PTR* logFileName() { return code4logFileName( this ) ; }
         int logOpen( const char *name, const char *userId ) { return code4logOpen( this, name, userId ) ; }
         void logOpenOff() { code4logOpenOff( this ) ; }
      #endif
      #ifdef S4CLIENT
         // CS 2007/10/12 Change 1st param to short; make same as C function.
         int logConn( short level, const char *logFileName )  { return code4logConn( this, level, logFileName ) ; }
      #endif
      // AS Mar 7/03 - support for C++
      void memoCompress( short flag ) { code4memoCompress( this, flag ) ; }
      int optAll()               { return code4optAll( this ) ; }
      int optStart()             { return code4optStart( this ) ; }
      int optSuspend()           { return code4optSuspend( this ) ; }
      int ping()                 { return code4ping( this ) ; }
      void setAcceptTimeOut( long val )   { c4setAcceptTimeOut( this, val ) ; }
      void setShareCloneLocks( short val ) { c4setShareCloneLocks( this, val ) ; }  // LY Apr 16/03
      long timeout()             { return code4timeout( this ); }
      void timeout( long l )     { code4timeoutSet( this, l ) ; }
      int tranCommit()           { return code4tranCommit( this ) ; }
      int tranRollback()         { return code4tranRollback( this ) ; }
      int tranStart()            { return code4tranStart( this ) ; }
      int tranStatus()           { return  code4tranStatus( this ); }
      int unlock()               { return code4unlock( this ) ; }
      int unlockAuto()           { return code4unlockAuto( this ) ; }
      void unlockAuto( short c ) { code4unlockAutoSet( this, c ) ; }
      int validate( const char *validateTableName, Bool5 deleteTemp )   { return code4validate( this, validateTableName, deleteTemp ) ; }
      void verifySet( const char *value )   { code4verifySet( this, value ) ; }
   #endif   /* D4DLL_CPP */
} ;

class S4CLASS Data4
{
public:
   DATA4 S4PTR *data ;
   Data4()                           { data = 0 ; }
   Data4( DATA4 S4PTR*d )            { data = d ; }
   Data4( Code4& code, const char *name )  { data = d4open( &code, name ) ; }
   operator DATA4 *()  const         { return data; }
   const char   S4PTR* alias()             { return d4alias( data ) ; }
   DATA4 S4PTR *dat()                { return data; }
   void alias( const char S4PTR *ptr )   { d4aliasSet( dat(),ptr ) ; }
   int append()                   { return d4append( data ) ; }
   int appendBlank()              { return d4appendBlank( data ) ; }
        short appendStart( short memo = 0 ){ return d4appendStart( data, memo ) ; }
   void blank()                   {d4blank( data ) ; }
   int bof()                      { return d4bof( data ) ; }
   int bottom()                   { return d4bottom( data ) ; }
   short changed( short flag = -1 ) { return d4changed( data, flag ) ; }
   //CJ MetroWerks compiler lists "check" as a keyword.
   int checkIndex()                { return d4check( data ) ; }
   int close() ;
   S4INLINE2 int compress( Data4 d, const char *compressName, short blockSize ) ;
   // CS 2007/10/12 Make 2nd param short; make same as C function.
   int copyTable(const char *destFolder, short includeIndex)   { return d4copyTable(data, destFolder, includeIndex); }
   int create( Code4& code, S4CONST char *name, FIELD4INFO f[], TAG4INFO t[])
      #ifdef D4DLL_CPP
         { data = d4create( &code, name, f, t ) ; return code4errorCode( &code, -5 ) ; }
      #else
         { data = d4create( &code, name, f, t ) ; return code.errorCode ; }
      #endif
   int create( Code4& code, S4CONST char *name, FIELD4INFO f[])
      #ifdef D4DLL_CPP
         { data = d4create( &code, name, f, 0 ) ; return code4errorCode( &code, -5 ) ; }
      #else
         { data = d4create( &code, name, f, 0 ) ; return code.errorCode ; }
      #endif
   int create( Code4& code, S4CONST char *name, Field4info& f );
   int create( Code4& code, S4CONST char *name, Field4info& f, Tag4info& t );

   void deleteRec()                 { d4delete( data ) ; }
   int deleted()                    { return d4deleted( data ) ; }
   int eof()                        { return d4eof( data ) ; }
   int fieldNumber( const char *name ) { return d4fieldNumber( data, name ) ; }
   int fieldsAdd( short nFields, FIELD4INFO f[] ) ;
   int fieldsAdd( Field4info& f ) ;
   int fieldsRemove( short nFields, char *fieldNames[] )
      #ifdef D4DLL_CPP
         {
            DATA4 *newData = d4fieldsRemove(data,nFields,fieldNames);
            if ( newData )
            {
               if ( newData != data )
               {
                  data = newData ;
                  return 0 ;
               }
            }
            return g_c4->c4getErrorCode( data->c4 ) ;
         }
      #else
         { d4fieldsRemove(&data,nFields,fieldNames);  return data->codeBase->errorCode; }
      #endif
   const char S4PTR* fileName()     { return d4fileName( data ) ; }
   int flush()                      { return d4flush( data ) ; }
   int freeBlocks()               { return d4freeBlocks( data ) ; }
   int go( const long rec )       { return d4go( data, rec ) ; }
   int goBof()                    { return d4goBof( data ) ; }
   int goEof()                    { return d4goEof( data ) ; }
   Index4 index( const char * ) ;
   int isValid()
      #ifdef D4DLL_CPP
         { return ( data != 0 ) ; } /* LY 2002/02/18 : data != 0 after Relate4set::free(), Code4::closeAll() */
      #else
         { return ( data == 0 ? 0 : data->dataFile != 0 ) ; } /* LY 2002/02/18 : data != 0 after Relate4set::free(), Code4::closeAll() */
      #endif
   int lock( const long recNum )
      #ifdef D4DLL_CPP
         { return d4lock( data, recNum ) ; }
      #else
         { return d4lockInternal( data, recNum, 1 ) ; }
      #endif
   int lockAdd( long recnum )     { return d4lockAdd( data, recnum ) ; }
   int lockAddAppend()            { return d4lockAddAppend( data ) ; }
   int lockAddFile()              { return d4lockAddFile( data ) ; }
   int lockAddAll()               { return d4lockAddAll( data ) ; }
   int lockAll()
      #ifdef D4DLL_CPP
         { return d4lockAll( data ) ; }
      #else
         { return d4lockAllInternal( data, 1 ) ; }
      #endif

   int lockAppend()
      #ifdef D4DLL_CPP
         { return d4lockAppend( data ) ; }
      #else
         { return d4lockAppendInternal( data, 1 ) ; }
      #endif
   int lockFile()
      #ifdef D4DLL_CPP
         { return d4lockFile( data ) ; }
      #else
         { return d4lockFileInternal( data, 1 ) ; }
      #endif

   int log( int l )               { return d4log( data, l ) ; }
   int log()                      { return d4log( data, -1 ) ; }
   int memoCompress()             { return d4memoCompress( data ) ; }
   int modifyStructure(FIELD4INFO *fields)  { return modifyStructure(fields, 0); }
   int modifyStructure(Field4info& fields);
   int modifyStructure(FIELD4INFO *fields, TAG4INFO *tags);
   int modifyStructure(Field4info& fields, Tag4info& tags);
   int numFields()                { return d4numFields( data ) ; }
   int open( Code4 &code, S4CONST char *name )
      #ifdef D4DLL_CPP
         { data =  d4open( &code, name ) ; return ( (code4errOpen( &code, -5 ) == 0) ? (code4errorCode( &code, -5 )) : (code4errorCode( &code, -5 ) < 0 ? code4errorCode( &code, -5 ) : 0 )) ; }
      #else
         { data =  d4open( &code, name ) ; return ( (code.errOpen == 0) ? (code.errorCode) : (code.errorCode < 0 ? code.errorCode : 0 )) ; }
      #endif
   S4INLINE2 int openClone( Data4 ) ;
   int optimize( const int optFlag ) { return d4optimize( data, optFlag ) ; }
   int optimizeWrite( const int optFlag ) { return d4optimizeWrite( data,  optFlag ) ; }
   int pack()                     { return d4pack( data ) ; }
   short packWithProgress( REINDEX_CALLBACK callback, long milliseconds )  { return d4packWithProgress( data, callback, milliseconds ) ; }
   // CS 2006/09/05 Remove conditional TURBOC code
   double position()           { return d4position( data ) ; }
   int position( const double pos ) { return d4positionSet( data, pos ) ; }
   long readBuffer( long numRecsToBuf, short doMemos )
      {  return d4readBuffer( data, numRecsToBuf, doMemos ) ; }
   long readBufferConfigure( long flags ) { return d4readBufferConfigure( data, flags ) ; }
   void recall()                  { d4recall( data ) ; }
   long recCount()                { return d4recCount( data ) ; }
   long recNo()                   { return d4recNo( data ) ; }
   char S4PTR* record()           { return d4record( data ) ; }
   // AS May 28/02 - don't use the exposed function with AES encryption be cause we modify the record
   // buffer to contain extra unused space to land on a record boundary
   #ifndef S4INTERNAL_COMPILE_CHECK
      unsigned long recWidth()        { return d4recWidth( data ) ; }
   #endif
   int refresh()                  { return d4refresh( data ) ; }
   int refreshRecord()            { return d4refreshRecord( data ) ; }
   int reindex()                  { return d4reindex( data ) ; }
   int reindexWithProgress( REINDEX_CALLBACK callback, long milliseconds )
                                  { return d4reindexWithProgress( data, callback, milliseconds ) ; }
   int remove()                   { int rc = d4remove( data ) ;
                                    if (!rc) data = 0 ; return rc ; }
   int seek( S4CONST char *ptr )  { return d4seek( data, ptr ) ; }
   int seek( S4CONST char *ptr, short len ) { return d4seekN( data, ptr, len ) ; }
   int seek( S4CONST double d )   { return d4seekDouble( data, d ) ; }
   int seek( S4CONST LONGLONG LL )   { return d4seekLongLong( data, LL ) ; }
   #ifndef S4CB51
      int seekNext( const char *ptr ) { return d4seekNext( data, ptr ) ; }
      int seekNext( const char *ptr, short len ) { return d4seekNextN( data, ptr, len ) ; }
      int seekNext( const double d ) { return d4seekNextDouble( data, d ) ; }
      int seekNext( const LONGLONG LL )   { return d4seekNextLongLong( data, LL ) ; }
   #endif
   void select()                  { d4tagSelect( data, 0 ) ; }
   void select( Tag4 ) ;
   void select( const char * ) ;
   int skip( const long n = 1 )   { return d4skip( data, n ) ; }
   int tagSync()                  { return d4tagSync( data, d4tagSelected( data ) ) ; }
   int top()                      { return d4top( data ) ; }
   int unlock()                   { return d4unlock( data ) ; }
   int unlockAppend()
   #ifdef S4OFF_MULTI  // LY Dec 11/03
      { return e4notSupported ; }
   #else
      { return d4unlockAppend( data ) ; }  // LY Apr 4/03
   #endif
   #ifndef S4MACINTOSH  // LY Jul 19/04
      // AS Mar 7/03 - support for C++
      long versionNumber()           { return d4versionNumber( data ) ; }
   #endif
   int write( long rec = -1 )     { return d4write( data, rec ) ; }
   long writeBuffer( long numRecsToBuf )
      { return d4writeBuffer( data, numRecsToBuf ) ; }
   int zap( long f = 1, long last = 1000000000L ) { return d4zap( data, f, last ) ; }
} ;

class S4CLASS Index4
{
public:
   INDEX4 S4PTR *index ;
public:
   Index4()                       { index = 0 ; }
   Index4( INDEX4 S4PTR *in )     { index = in ; }
   Index4( Data4 d, S4CONST char *name = NULL ) { index = i4open( d.dat(), name ) ; }
   int close()                    { int rc =  i4close( index ) ; index = 0 ; return rc; }
   int create( Data4 d, S4CONST char *name, S4CONST TAG4INFO *info ) ;
   int create( Data4 d, S4CONST TAG4INFO *info ) ;
   int create( Data4 d, S4CONST char *name, Tag4info& info) ;
   int create( Data4 d, Tag4info& info ) ;

   int create( Data4 d, S4CONST char *name, S4CONST TAG4INFO *tagData, REINDEX_CALLBACK callback, long milliseconds )
                                  { return i4createWithProgress( d.data, name, tagData, callback, milliseconds ) ; }
   int create( Data4 d, S4CONST TAG4INFO *tagData, REINDEX_CALLBACK callback, long milliseconds )
                                  { return create(d, 0, tagData, callback, milliseconds ) ; }
   int create( Data4 d, S4CONST char *name, Tag4info& tagData, REINDEX_CALLBACK callback, long milliseconds) ;
   int create( Data4 d, Tag4info& tagData, REINDEX_CALLBACK callback, long milliseconds ) ;

   Data4 data() ; // { DEBUG4PTR( index == 0, 60704 ) return Data4( index->data ) ; }
   const char S4PTR* fileName()   { return i4fileName( index ) ; }
   void init( Data4 d, const char *name = NULL ) { index = d4index( d.dat(), name );  }
   int isValid()                  { return index != 0 ; }
   S4INLINE2 int open( Data4 d, S4CONST char *file ) ;
   int reindex()                  { return i4reindex( index ) ; }
   Tag4 tag( const char * ) ;
   int tagAdd( const TAG4INFO *newTag ) { return i4tagAdd( this->index, newTag ) ; }
} ;

class S4CLASS Tag4
{
public:
   TAG4 S4PTR *tag ;
   Tag4()                         { tag = NULL ; }
   Tag4( TAG4 S4PTR *tg )         { tag = tg ; }
   Tag4( Data4 d, const char * const name = NULL ) { init(d,name); }
   operator TAG4*() const         { return tag ; }
   #ifndef S4SERVER
      const char S4PTR* alias()   { return t4alias( tag ) ; }
   #else
      const char S4PTR* alias()   { DEBUG4PTR( tag == 0, 60901 ) return tag->tagFile->alias ; }
   #endif
   #ifdef S4CLIPPER
       int close()                { return t4close( tag ) ; }
   #endif
//   long count()                   { return t4count( tag ) ; }  // CS 2008/01/18
   #ifndef S4CLIENT
      int descending() ;
   #endif

   const char S4PTR* expr()       { return t4expr( tag ) ; }
   const char S4PTR* filter()     { return t4filter( tag ) ; }
   void init( Data4, const char *name = NULL ) ;
   void initFirst( Data4 d )      { tag = d4tagNext( d.dat(), NULL ) ; }
   void initLast( Data4 d )       { tag = d4tagPrev( d.dat(), NULL ) ; }
   void initNext()
      #ifdef D4DLL_CPP
         { if( isValid() ) tag = d4tagNext( tag->d4, tag ) ; }
      #else
         { if( isValid() ) tag = d4tagNext( tag->index->data, tag ) ; }
      #endif
   void initPrev()
      #ifdef D4DLL_CPP
         { if( isValid() ) tag = d4tagPrev( tag->d4, tag ) ; }
      #else
         { if( isValid() ) tag = d4tagPrev( tag->index->data, tag ) ; }
      #endif
   void initSelected( Data4 d )   { tag = d4tagSelected( d.dat() ) ; }
   int isValid()                  { return tag!=0; } ;
   #if defined(S4CLIPPER) || defined (S4CLIENT)
      void open( Data4 d, const char *name ) { tag = t4open( d.dat(), NULL, name ); }
      void open( Data4 d, Index4 i, const char *name ) { tag = t4open( d.dat(), i.index, name ) ; }
   #endif
   int remove() ;
   int seekN( const char S4PTR* seekValue, const short seekValueLen, const short doDataPosition )
                                  { return t4seekN( tag, seekValue, seekValueLen, doDataPosition ) ; }
   int unique()                   { return t4unique( tag ) ; }
   int unique( const short uniqueCode ) { return t4uniqueSet( tag, uniqueCode ) ; }
} ;

#ifndef S4JNI  /* LY 99/07/08 */
class S4CLASS Str4len ;

class S4CLASS Str4
{
   private:
     char S4PTR *assignPtr()   { return ptr(); } ;
   public:
   char v4buffer[257] ;
   #ifdef __ZTC__
      virtual operator char() const ;
      // AS Jul 27/05 - float field support (C++)
      // virtual operator float() const ;
      virtual operator double() const ;
      virtual operator int() const ;
   #else
      operator char() const ;
      // AS Jul 27/05 - float field support (C++)
      // operator float() const ;
      operator double() const ;
      operator int() const ;
   #endif
   virtual operator long() const ;
   char& operator[]( int ) ;
   int operator==( Str4& ) const ;  /* Equal, including length */
   int operator!=( Str4& s ) const { return ! operator==( s ) ; }
   int operator< ( Str4& ) const ;
   int operator> ( Str4& ) const ;
   int operator>=( Str4& s ) const { return ! operator<( s ) ; }
   int operator<=( Str4& s ) const { return ! operator>( s ) ; }
   Str4& operator =( const Str4& s ) { assign( s ) ;  return *this ; }
   int add( Str4& ) ;
   int add( char * ) ;
   int assign( const char S4PTR * ) ;
   int assign( const char S4PTR *, const unsigned ) ;
   int assign( const Str4& ) ;
   // AS Jul 27/05 - float field support (C++)
   void assignFloat( float, int newLen = -1, int n_dec = -1 ) ;
   void assignDouble( double, int newLen = -1, int n_dec = -1 ) ;
   void assignLong( long, int newLen = -1, int zeros_in_front = 0 ) ;
   int at( Str4& ) const ;
   int encode( char *, char *, char * ) ;
   char *endPtr() ;
   int  insert( Str4&, unsigned pos = 0 ) ;
   void lower() ;
   unsigned ncpy( char *, unsigned ) ;
   int  replace( Str4&, unsigned pos = 0 ) ;
   void set( int ) ;
   void trim() ;
   void upper() ;

   /* Get information */
   Str4len left( unsigned int ) const ;
   Str4len right( unsigned int ) const ;
   Str4len substr( unsigned int, unsigned int ) const ;
   #ifdef S4USE_TRUE
      int true() const ;
   #else
      int isTrue() const ;
   #endif
   virtual void      changed()         {}
   virtual int       decimals() const  { return 0 ; }
   virtual short isBinaryField() { return 0 ; }
   virtual unsigned long len() const ;
   virtual unsigned long len1() const ;
   #ifndef S4OFF_ENFORCE_LOCK
      // Returns r4success if its ok, otherwise returns -1
      virtual int    lockCheck()       { return r4success ; }
   #endif
   virtual unsigned  maximum() const   { return (unsigned)len() ; }
   virtual char S4PTR*ptr() = 0 ;
   virtual const char S4PTR * ptr1() const = 0 ;
   virtual const char S4PTR * str() ;
   virtual int setLen( unsigned )      { return -1 ; }
   virtual int setMax( unsigned )      { return -1 ; }
} ;

class S4CLASS Str4char : public Str4
{
public:
   Str4char( char ch = 0 ) ;
   Str4char( Str4char &s ) { assign( s ) ; }
   Str4char& operator =( const Str4& s ) { assign( s ) ;  return *this ;  }
   char S4PTR*ptr()           { return &c ; }
   char const S4PTR*ptr1() const { return &c ; }
   unsigned long len() const       { return 1 ; }
   unsigned long len1() const      { return 1 ; }
private:
   char c ;
} ;

class S4CLASS Str4ptr : public Str4
{
public:
   Str4ptr( const char *ptr )        { error4describe( 0, e4parm, (E61093), "Cannot initialize Str4 object with a const char *", 0, 0 ) ; }
   Str4ptr( char *ptr )        { p = ptr ; }
   Str4ptr( Str4ptr &s )       { p = s.ptr() ; }
   Str4ptr& operator =( const Str4& s ) { assign( s ) ;  return *this ;  }
   char S4PTR*ptr()            { return p ; }
   char const S4PTR*ptr1() const { return p ; }
   char *p ;
} ;

class S4CLASS Str4len : public Str4ptr
{
public:
   Str4len( const void *ptr, unsigned ptrLen ) : Str4ptr( ( char * )ptr ) { curLen =  ptrLen; }
   // CS 2007/11/02 Add const keyword to fix error in some OSs (i.e. Linux).
   // AS Nov 16/07 - This fix breaks Microsoft compiler under Windows...make it conditional
   #ifdef S4UNIX
      Str4len( const Str4len &s ) : Str4ptr( ( char * )s.ptr() ) { curLen =  s.len(); }
   #else
      Str4len( Str4len &s ) : Str4ptr( ( char * )s.ptr() ) { curLen =  s.len(); }
   #endif
   Str4len& operator =( const Str4& s ) { assign( s ) ;  return *this ;  }
   unsigned long len() const    { return curLen ; }
   unsigned long len1() const   { return curLen ; }
   unsigned long curLen ;
} ;

class S4CLASS Str4max : public Str4len
{
public:
   Str4max( void *ptr, unsigned m ) : Str4len( ptr,m ) { maxLen = m; }
   Str4max( Str4max& s ) : Str4len( s ) { assign(s) ; }
   Str4max& operator =( const Str4& s ) { assign( s ) ;  return *this ;  }
   unsigned maximum()          { return maxLen ; }
   int setLen( unsigned ) ;
   unsigned maxLen ;
} ;

/* The following classes  are always guaranteed to be null ended. */
class S4CLASS Str4ten : public Str4
{
public:
   Str4ten()                   { curLen = 0 ; buf[0] = 0 ; buf[10] = 0 ; }
   Str4ten( const char *p )    { curLen = 0 ; buf[10] = 0 ; assign( p ) ; }
   Str4ten( const Str4 &s )    { curLen = 0 ; buf[10]=0 ; assign( s ) ; }
   Str4ten( Str4ten &s )       { curLen = 0 ; buf[10]=0 ; assign( s ) ; }

   unsigned long len() const        { return curLen ; }
   unsigned long len1() const       { return curLen ; }
   unsigned maximum() const    { return 10 ; }
   char S4PTR*ptr()            { return &(buf[0]) ; }
   char const S4PTR*ptr1() const { return buf ; }
   int setLen( unsigned ) ;

   unsigned curLen ;
private:
   char buf[12] ;
} ;

class S4CLASS Str4large: public Str4
{
public:
   Str4large() ;
   Str4large( const char * ) ;
   Str4large( Str4 & ) ;
   Str4large( Str4large & ) ;
   Str4large& operator =( const Str4& s ) { assign( s ) ;  return *this ;  }

   unsigned maximum()              { return 255 ; }
   unsigned long len() const       { return curLen; }
   unsigned long len1() const      { return curLen; }
   char S4PTR*ptr()                { return buf ; }
   char const S4PTR*ptr1() const   { return buf ; }
   int setLen( unsigned ) ;

   unsigned curLen ;
private:
   char buf[256] ;
} ;

class S4CLASS Str4flex : public Str4max
{
public:
   Str4flex( Code4 & ) ;
   Str4flex( Str4flex& ) ;
  ~Str4flex() ;
   Str4flex &operator =( const Str4 &s ) { assign( s ) ; return *this ; }

   void free() ;
   int setMax( unsigned long ) ;
   const char S4PTR *str()             { return ptr() ; }

   Code4 S4PTR *codeBase ;
private:
   int setLen( unsigned ) ;
} ;

class S4CLASS Field4 : public Str4
{
private:
   char S4PTR *assignPtr()       { return f4assignPtr( field ) ; }
public:
   Field4()                            { field =  0 ; }
   Field4( Data4 d, short j )          { field =  d4fieldJ( d.dat(), j ) ; }
   Field4( Data4 d, const char *name ) { field = d4field( d.dat(), name ) ; }
   Field4( const Field4& f )
      {
         // LY Sep 28/04 : changed from field to f.field
         if ( f.field )  /* LY 2003/10/23 : avoid problem where f is created by Field4() */
         {
            #ifdef D4DLL_CPP
               field = d4field( f.field->d4, f4name( f.field ) ) ;
            #else
               // AS Oct 31/03 support for long field names
               init( f.field->data, f.field->longName ) ;
            #endif
         }
         else
            field = 0 ;
      }
   Field4( FIELD4 *f )                 { field =  f ; }

   Field4& operator =( const Field4& f ) ;
   Field4& operator =( Str4& s ) { assign( s ) ;  return *this ;  }  // CS 2002/12/13 not const
   operator int() const          { return f4int(field) ; }
   operator long() const         { return f4long(field) ; }
   operator double() const       { return f4double(field) ; }
   // AS Jul 27/05 - float field support (C++)
   operator float() const        { return f4float(field) ; }
   operator DWORDLONG() const    { return f4longLong( field ) ; }

   int assign(const char S4PTR *str)  { return assign(str, (unsigned int)strlen(str)); }  // CS 2006/06/06 cast to uint to fix warning
   int assign(Str4& s)                { return assign(s.str()); }  // CS 2002/12/13 add assign functions
   int assign(const char S4PTR *str, const unsigned len)
   {
      f4memoAssignN(field, str, len);  /* LY 2003/02/06 : changed from f4assignN() (Field4memo::assign()) */
      switch (f4type(field))
      {
         case r4memo:
         case r4memoBin:
            break;
         default:
            if (len > f4len(field))
               return -1;
      }
      return r4success;
   }

   void assignCurrency( const char *data )  { f4assignCurrency( field, data ) ; }
   void assignDateTime( const char *data )  { f4assignDateTime( field, data ) ; }
   void assignField( Field4& f ) { f4assignField( field, f.field ) ; }
   void assignLongLong( DWORDLONG data )     { f4assignLongLong( field, data ) ; }
   void assignNull()             { f4assignNull( field ) ; }
   void assignUnicode( const WSTR5 *str )   { f4assignUnicode( field, str ) ; }
   void changed() ;
   char *currency( short numDec )              { return f4currency( field, numDec ) ; }  // CS 2003/09/05 Add
   Data4 data() ; // { DEBUG4PTR( field == 0, E61204 ) return Data4( field->data ) ; }
   const char *dateTime() { return f4dateTime( field ) ; }  // CS 2003/01/24 Add
   int decimals() const          { return f4decimals( field ) ; }
   int init( Data4 d, const char *name )
      { field = d4field( d.dat(), name ) ; return ( field == 0 ) ? -1 : 0 ; }
   int init( Data4 d, short j )
      { field = d4fieldJ( d.dat(), j ) ;   return ( field == 0 ) ? -1 : 0 ; }
   short isBinaryField() ;
   int isNull() const            { return f4null( field ) ; }
   int isValid() const           { return field!=0; }
   unsigned long len() const     { return f4len( field ) ; }
   unsigned long len1() const    { return f4len( field ) ; }
   #ifndef S4OFF_ENFORCE_LOCK
      int lockCheck() ;
   #endif
   const char S4PTR * name()     { return f4name( field ) ; }
   int number() ;
   int type() const              { return f4type( field ) ; }
   char S4PTR * ptr()            { return f4ptr( field ) ; }
   char const S4PTR * ptr1() const { return f4ptr( field ) ; }
   const char *str() ;

   FIELD4    S4PTR *field ;
} ;
#endif   /* !S4JNI  LY 99/07/08 */

class S4CLASS Field4info
{
public:
   Field4info( Code4 & ) ;
   Field4info( Data4 ) ;

   ~Field4info() ;
   Field4info( Field4info & ) ;  // Illegal operation, force link error
   Field4info &operator =( Field4info& ) ;   // Illegal operation, force link error
   int add( const char *, char, int len = 0, int dec = 0, unsigned short allowNulls = 0 ) ;
   #ifndef S4JNI  /* LY 99/07/08 */
      int add( Field4 ) ;
      int del( const char * ) ;
   #endif
   int add( Data4 ) ;
   int del( int ) ;
   void free() ;
   FIELD4INFO *fields() { return field ; }
   const FIELD4INFO *operator[]( int ) ;
   int numFields()      { return size ; } ;
private:
   Code4 S4PTR *codeBase ;
   FIELD4INFO *field ;
   int size ;
   unsigned length ;
} ;

class S4CLASS Tag4info
{
private:
   char *expr, *filt ;
public:
   Tag4info( Code4 & ) ;
   Tag4info( Data4 ) ;
   Tag4info( Index4 ) ;
   ~Tag4info() ;
   Tag4info( Tag4info& ) ;     // Illegal action, force a link error
   Tag4info &operator =( Tag4info & ) ;  // Illegal action, force a link error

   int add( const char *, const char *, const char *filter = NULL, short uniq = 0, unsigned short desc = 0 ) ;
   int add( Tag4 ) ;
   int del( int ) ;
   #ifndef S4JNI  /* LY 99/07/08 */
      int del( const char * ) ;
   #endif
   TAG4INFO * tags() { return tag ; }
   void free() ;
   int  numTags()    { return size ; } ;
private:
   int  addIndex_tags( INDEX4 * ) ;
   char S4PTR * getName( TAG4INFO *, int tagPos = 0 ) ;
   const char S4PTR* getExpr( TAG4INFO *, int tagPos = 0 ) ;
   const char S4PTR* getFilter( TAG4INFO *, int tagPos = 0 ) ;
   short getUniqueKey( TAG4INFO *, int tagPos = 0 ) ;
   unsigned short getDescendKey( TAG4INFO *, int tagPos = 0 ) ;

   Code4 S4PTR *codeBase ;
   TAG4INFO *tag ;
   int size ;
   unsigned length ;
   friend class Data4 ;
   friend class Index4 ;
} ;

#ifndef S4JNI  /* LY 99/07/08 */
class S4CLASS Field4memo : public Field4
{
public:
   Field4memo() ;
   Field4memo( Data4 &, int ) ;
   Field4memo( Data4 &, const char * ) ;
   Field4memo( Field4 ) ;

   void changed() ;
   void free()                { f4memoFree( field ) ; }
   unsigned long len() const  { return f4memoLen( field ) ; }
   unsigned long len1() const { return f4memoLen( field ) ; }
   int  setLen( unsigned ) ;

   int assignFile( const char *fileName ) { return f4memoAssignFile( field, fileName ) ; }
   void assignUnicode( const WSTR5 *str )  { f4memoAssignUnicode( field, str ) ; }
   int memoFile( const char *fileName ) { return f4memoFile( field, fileName ) ; }

   char S4PTR*ptr()        { return f4memoPtr( field ) ; }
   char const S4PTR*ptr1() const { return f4memoPtr( field ) ; }
   // AS May 30/03 - if not actually a memo field, the Field4::str() functions should be called.
   const char S4PTR*str()
      {
         // AS Feb 22/06 - fix for dual dll
         #ifdef D4DLL_CPP
            return f4memoStr( field ) ;
         #else
            // AS Dec 5/05 - fix for OFF_MEMO
            #ifdef S4OFF_MEMO
               return Field4::str() ;
            #else
               return ( ( field->memo == 0 ) ? ( Field4::str() ) :  (f4memoStr( field )) )  ;
            #endif
         #endif
      }

   // AS 03/29/00 Added partial read/write support independent of normal mechanism
   unsigned int read( char *outBuffer, unsigned int readLen, unsigned int startPos ) ;
   int write( char *ptrToData, unsigned int lenToWrite ) ;
   void writeStart( long memoLen ) { currentMemoPos = 0 ; finalMemoLen = memoLen ; }
   int writeFinish()
   #ifdef D4DLL_CPP
      { return f4memoWriteFinish( (DATA4 *)field->d4 ) ; }
   #else
      { return f4memoWriteFinish( field->data ) ; }
   #endif
private:
   long currentMemoPos, finalMemoLen ;

} ;

class S4CLASS Date4 : public Str4
{
public:
   Date4() ;
   Date4( long ) ;
   Date4( const char S4PTR * ) ;
   Date4( const char S4PTR *, char S4PTR * ) ;
   Date4( Date4 &s ) { assign( s ) ; }
   Date4 &operator =( const Str4 &s ) { assign( s ) ; return *this ; }
   operator long()                  { return date4long( ptr() ) ; }
   // CS 2006/09/05 Remove conditional TURBOC code
   operator double()             { return date4formatMdx( ptr() ) ; }
   long operator +( const long l )  { return ( date4long( ptr() )+l ) ; }
   long operator -( const long l )  { return ( date4long( ptr() )-l ) ; }
   void operator +=( const long l ) { date4assign( ptr(), date4long( ptr() )+l ) ; }
   void operator -=( const long l ) { date4assign( ptr(), date4long( ptr() )-l ) ; }
   long operator ++()               { *this += 1L ; return long( *this ) ; }
   long operator ++( int )          { *this += 1L ; return long( *this )-1L ; }
   long operator --()               { *this -= 1L ; return long( *this ) ; }
   long operator --( int )          { *this -= 1L ; return long( *this )+1L ; }
   void assign( const long l )      { date4assign( ptr(),l ) ; }
   void assign( const char *p )     { u4ncpy( ptr(), p, sizeof( dt ) ) ; }
   void assign( const char *p, char *pict ) { date4init( ptr(), p, pict ) ; }
   /* LY 2001/07/29 : changed to const Str4 to avoid ambiguity error with assign(long) in operator = under Solaris 64-bit */
   void assign( const Str4 &s )     { Str4::assign( s ) ; }
   const char S4PTR * cdow()        { return date4cdow( ptr() ) ; }
   const char S4PTR * cmonth()      { return date4cmonth( ptr() ) ; }
   int day()                        { return date4day( ptr() ) ; }    /* Day of month  ( 1-31 ) */
   int dow()                        { return date4dow( ptr() ) ; }    /* Day of week   ( 1-7 ) */
   void format( char *result, char *pict ) { date4format( ptr(), result, pict ) ; }
   const char *format( char * ) ;
   int isLeap() const               { long y = date4year( dt ) ; return ( y%4 == 0 && y%100 != 0 || y%400 == 0 ) ?  1 : 0 ; }
   unsigned long len() const        { return 8 ; }
   unsigned long len1() const       { return 8 ; }
   int month()                      { return date4month( ptr() ) ; }/* Month of year ( 1-12 ) */
   static void timeNow(char *time)  { date4timeNow( time ) ; }
   void today()                     { date4today( ptr() ) ; }
   int year()                       { return date4year( ptr() ) ; } ;
   char S4PTR*ptr()                 { return  dt ; }
   char const S4PTR*ptr1() const    { return  dt ; }
   const char* str() ;  /* LY 2001/06/04 : Date4::str() was missing */
private:
   char dt[9] ;
} ;
#endif   /* !S4JNI  LY 99/07/08 */

class S4CLASS File4 : public FILE4
{
public:
   File4()
      #ifdef D4DLL_CPP
         { f4 = 0 ; }
      #else
         { hand = INVALID4HANDLE ; }
      #endif

   File4( Code4 &code, S4CONST char *namestr, int doAlloc = 0 ) { open( code, namestr, doAlloc ) ; }
   File4( File4& ) ;     /* Illegal operation, force link error */

   #ifdef D4DLL_CPP
      int close()
      {
         int rc = file4close( f4 ) ;
         file4free( &f4 ) ;
         return rc ;
      }

      int compressCreate( File4 &sourceFile, S4CONST char *namestr = 0, short blockSize = 0)
         {
            long rc = 0 ;

            rc = sourceFile.code4->file4alloc( &f4 ) ;
            if ( f4 == 0 )
               return rc ;

            c4getFuncPtr( sourceFile.code4->hInst, "file4close", (void **)&file4close, &rc ) ;
            c4getFuncPtr( sourceFile.code4->hInst, "file4compressCreate", (void **)&file4compressCreate, &rc ) ;
            c4getFuncPtr( sourceFile.code4->hInst, "file4compressOpen", (void **)&file4compressOpen, &rc ) ;
            c4getFuncPtr( sourceFile.code4->hInst, "file4create", (void **)&file4create, &rc ) ;
            c4getFuncPtr( sourceFile.code4->hInst, "file4flush", (void **)&file4flush, &rc ) ;
            c4getFuncPtr( sourceFile.code4->hInst, "file4free", (void **)&file4free, &rc ) ;
            c4getFuncPtr( sourceFile.code4->hInst, "file4lenLow2", (void **)&file4lenLow2, &rc ) ;
            c4getFuncPtr( sourceFile.code4->hInst, "file4lenSetLow2", (void **)&file4lenSetLow2, &rc ) ;
            c4getFuncPtr( sourceFile.code4->hInst, "file4lockInternal", (void **)&file4lockInternal, &rc ) ;
            c4getFuncPtr( sourceFile.code4->hInst, "file4nameLow", (void **)&file4nameLow, &rc ) ;
            c4getFuncPtr( sourceFile.code4->hInst, "file4open", (void **)&file4open, &rc ) ;
            c4getFuncPtr( sourceFile.code4->hInst, "file4optimizeLow", (void **)&file4optimizeLow, &rc ) ;
            c4getFuncPtr( sourceFile.code4->hInst, "file4optimizeWrite", (void **)&file4optimizeWrite, &rc ) ;
            c4getFuncPtr( sourceFile.code4->hInst, "file4read", (void **)&file4read, &rc ) ;
            c4getFuncPtr( sourceFile.code4->hInst, "file4readAll", (void **)&file4readAll, &rc ) ;
            c4getFuncPtr( sourceFile.code4->hInst, "file4refresh", (void **)&file4refresh, &rc ) ;
            c4getFuncPtr( sourceFile.code4->hInst, "file4replace", (void **)&file4replace, &rc ) ;
            c4getFuncPtr( sourceFile.code4->hInst, "file4unlockInternal", (void **)&file4unlockInternal, &rc ) ;
            c4getFuncPtr( sourceFile.code4->hInst, "file4write", (void **)&file4write, &rc ) ;

            c4getFuncPtr( sourceFile.code4->hInst, "file4seqReadAlloc", (void **)&file4seqReadAlloc, &rc ) ;
            c4getFuncPtr( sourceFile.code4->hInst, "file4seqReadFree", (void **)&file4seqReadFree, &rc ) ;
            c4getFuncPtr( sourceFile.code4->hInst, "file4seqReadInit", (void **)&file4seqReadInit, &rc ) ;

            c4getFuncPtr( sourceFile.code4->hInst, "file4seqWriteAlloc", (void **)&file4seqWriteAlloc, &rc ) ;
            c4getFuncPtr( sourceFile.code4->hInst, "file4seqWriteFree", (void **)&file4seqWriteFree, &rc ) ;
            c4getFuncPtr( sourceFile.code4->hInst, "file4seqWriteInit", (void **)&file4seqWriteInit, &rc ) ;

            if ( rc != 0 )
            {
               file4free( &f4 ) ;
               return rc ;
            }

            rc = file4compressCreate( f4, sourceFile.f4, namestr, blockSize ) ;
            if ( rc != 0 )
               file4free( &f4 ) ;
            code4 = sourceFile.code4 ;

            return rc ;
         }

      int compressOpen( Code4 &code, S4CONST char *namestr = 0)
         {
            long rc = 0 ;

            rc = code.file4alloc( &f4 ) ;
            if ( f4 == 0 )
               return rc ;

            c4getFuncPtr( code.hInst, "file4close", (void **)&file4close, &rc ) ;
            c4getFuncPtr( code.hInst, "file4compressCreate", (void **)&file4compressCreate, &rc ) ;
            c4getFuncPtr( code.hInst, "file4compressOpen", (void **)&file4compressOpen, &rc ) ;
            c4getFuncPtr( code.hInst, "file4create", (void **)&file4create, &rc ) ;
            c4getFuncPtr( code.hInst, "file4flush", (void **)&file4flush, &rc ) ;
            c4getFuncPtr( code.hInst, "file4free", (void **)&file4free, &rc ) ;
            c4getFuncPtr( code.hInst, "file4lenLow2", (void **)&file4lenLow2, &rc ) ;
            c4getFuncPtr( code.hInst, "file4lenSetLow2", (void **)&file4lenSetLow2, &rc ) ;
            c4getFuncPtr( code.hInst, "file4lockInternal", (void **)&file4lockInternal, &rc ) ;
            c4getFuncPtr( code.hInst, "file4nameLow", (void **)&file4nameLow, &rc ) ;
            c4getFuncPtr( code.hInst, "file4open", (void **)&file4open, &rc ) ;
            c4getFuncPtr( code.hInst, "file4optimizeLow", (void **)&file4optimizeLow, &rc ) ;
            c4getFuncPtr( code.hInst, "file4optimizeWrite", (void **)&file4optimizeWrite, &rc ) ;
            c4getFuncPtr( code.hInst, "file4read", (void **)&file4read, &rc ) ;
            c4getFuncPtr( code.hInst, "file4readAll", (void **)&file4readAll, &rc ) ;
            c4getFuncPtr( code.hInst, "file4refresh", (void **)&file4refresh, &rc ) ;
            c4getFuncPtr( code.hInst, "file4replace", (void **)&file4replace, &rc ) ;
            c4getFuncPtr( code.hInst, "file4unlockInternal", (void **)&file4unlockInternal, &rc ) ;
            c4getFuncPtr( code.hInst, "file4write", (void **)&file4write, &rc ) ;

            c4getFuncPtr( code.hInst, "file4seqReadAlloc", (void **)&file4seqReadAlloc, &rc ) ;
            c4getFuncPtr( code.hInst, "file4seqReadFree", (void **)&file4seqReadFree, &rc ) ;
            c4getFuncPtr( code.hInst, "file4seqReadInit", (void **)&file4seqReadInit, &rc ) ;

            c4getFuncPtr( code.hInst, "file4seqWriteAlloc", (void **)&file4seqWriteAlloc, &rc ) ;
            c4getFuncPtr( code.hInst, "file4seqWriteFree", (void **)&file4seqWriteFree, &rc ) ;
            c4getFuncPtr( code.hInst, "file4seqWriteInit", (void **)&file4seqWriteInit, &rc ) ;

            if ( rc != 0 )
            {
               file4free( &f4 ) ;        /* LY 2002/03/14 : free memory alloc'd by file4alloc() */
               return rc ;
            }

            rc = file4compressOpen( f4, code.c4, namestr ) ;
            if ( rc != 0 )
               file4free( &f4 ) ;        /* LY 2002/03/14 : free memory alloc'd by file4alloc() */
            code4 = &code ;

            return rc ;
         }

      // AS May 24/02 - created file4createInternal for internal use to indicate file types
      int create( Code4 &code, S4CONST char *namestr = 0, const int doAlloc = 0 )
         {
            long rc = 0 ;

            rc = code.file4alloc( &f4 ) ;
            if ( f4 == 0 )
               return rc ;

            c4getFuncPtr( code.hInst, "file4close", (void **)&file4close, &rc ) ;
            c4getFuncPtr( code.hInst, "file4compressCreate", (void **)&file4compressCreate, &rc ) ;
            c4getFuncPtr( code.hInst, "file4compressOpen", (void **)&file4compressOpen, &rc ) ;
            c4getFuncPtr( code.hInst, "file4create", (void **)&file4create, &rc ) ;
            c4getFuncPtr( code.hInst, "file4flush", (void **)&file4flush, &rc ) ;
            c4getFuncPtr( code.hInst, "file4free", (void **)&file4free, &rc ) ;
            c4getFuncPtr( code.hInst, "file4lenLow2", (void **)&file4lenLow2, &rc ) ;
            c4getFuncPtr( code.hInst, "file4lenSetLow2", (void **)&file4lenSetLow2, &rc ) ;
            c4getFuncPtr( code.hInst, "file4lockInternal", (void **)&file4lockInternal, &rc ) ;
            c4getFuncPtr( code.hInst, "file4nameLow", (void **)&file4nameLow, &rc ) ;
            c4getFuncPtr( code.hInst, "file4open", (void **)&file4open, &rc ) ;
            c4getFuncPtr( code.hInst, "file4optimizeLow", (void **)&file4optimizeLow, &rc ) ;
            c4getFuncPtr( code.hInst, "file4optimizeWrite", (void **)&file4optimizeWrite, &rc ) ;
            c4getFuncPtr( code.hInst, "file4read", (void **)&file4read, &rc ) ;
            c4getFuncPtr( code.hInst, "file4readAll", (void **)&file4readAll, &rc ) ;
            c4getFuncPtr( code.hInst, "file4refresh", (void **)&file4refresh, &rc ) ;
            c4getFuncPtr( code.hInst, "file4replace", (void **)&file4replace, &rc ) ;
            c4getFuncPtr( code.hInst, "file4unlockInternal", (void **)&file4unlockInternal, &rc ) ;
            c4getFuncPtr( code.hInst, "file4write", (void **)&file4write, &rc ) ;

            c4getFuncPtr( code.hInst, "file4seqReadAlloc", (void **)&file4seqReadAlloc, &rc ) ;
            c4getFuncPtr( code.hInst, "file4seqReadFree", (void **)&file4seqReadFree, &rc ) ;
            c4getFuncPtr( code.hInst, "file4seqReadInit", (void **)&file4seqReadInit, &rc ) ;

            c4getFuncPtr( code.hInst, "file4seqWriteAlloc", (void **)&file4seqWriteAlloc, &rc ) ;
            c4getFuncPtr( code.hInst, "file4seqWriteFree", (void **)&file4seqWriteFree, &rc ) ;
            c4getFuncPtr( code.hInst, "file4seqWriteInit", (void **)&file4seqWriteInit, &rc ) ;

            if ( rc != 0 )
            {
               file4free( &f4 ) ;        /* LY 2002/03/14 : free memory alloc'd by file4alloc() */
               return rc ;
            }

            rc = file4create( f4, code.c4, namestr, doAlloc ) ;
            if ( rc != 0 )
               file4free( &f4 ) ;        /* LY 2002/03/14 : free memory alloc'd by file4alloc() */
            code4 = &code ;

            return rc ;
         }

      int flush()                { return file4flush( f4 ) ; }
      int isValid() const  { return ( f4 != 0 ) ; }
      long len()  { return (long)file4len( f4 ) ; }
      long len1() { return (long)file4len( f4 ) ; }
      #ifndef S4INTERNAL_COMPILE_CHECK
         int setLen( const long l ) { return file4lenSet( f4, l ) ; }
      #endif
      int lock( const long start, const long bytes ) { return file4lockInternal( f4, start, 0, bytes, 0 ) ; }
      const char S4PTR*fileName() const   { return (const char *)file4name( f4 ) ; }
      // AS May 24/02 - created file4openLow for internal use to indicate file types
      int open( Code4 &code, S4CONST char *namestr, const int doAlloc = 0 )
         {
            long rc = 0 ;

            rc = code.file4alloc( &f4 ) ;
            if ( f4 == 0 )
               return rc ;

            c4getFuncPtr( code.hInst, "file4close", (void **)&file4close, &rc ) ;
            c4getFuncPtr( code.hInst, "file4create", (void **)&file4create, &rc ) ;
            c4getFuncPtr( code.hInst, "file4flush", (void **)&file4flush, &rc ) ;
            c4getFuncPtr( code.hInst, "file4free", (void **)&file4free, &rc ) ;
            c4getFuncPtr( code.hInst, "file4lenLow2", (void **)&file4lenLow2, &rc ) ;
            c4getFuncPtr( code.hInst, "file4lenSetLow2", (void **)&file4lenSetLow2, &rc ) ;
            c4getFuncPtr( code.hInst, "file4lockInternal", (void **)&file4lockInternal, &rc ) ;
            c4getFuncPtr( code.hInst, "file4nameLow", (void **)&file4nameLow, &rc ) ;
            c4getFuncPtr( code.hInst, "file4open", (void **)&file4open, &rc ) ;
            c4getFuncPtr( code.hInst, "file4optimizeLow", (void **)&file4optimizeLow, &rc ) ;
            c4getFuncPtr( code.hInst, "file4optimizeWrite", (void **)&file4optimizeWrite, &rc ) ;
            c4getFuncPtr( code.hInst, "file4read", (void **)&file4read, &rc ) ;
            c4getFuncPtr( code.hInst, "file4readAll", (void **)&file4readAll, &rc ) ;
            c4getFuncPtr( code.hInst, "file4refresh", (void **)&file4refresh, &rc ) ;
            c4getFuncPtr( code.hInst, "file4replace", (void **)&file4replace, &rc ) ;
            c4getFuncPtr( code.hInst, "file4unlockInternal", (void **)&file4unlockInternal, &rc ) ;
            c4getFuncPtr( code.hInst, "file4write", (void **)&file4write, &rc ) ;

            c4getFuncPtr( code.hInst, "file4seqReadAlloc", (void **)&file4seqReadAlloc, &rc ) ;
            c4getFuncPtr( code.hInst, "file4seqReadFree", (void **)&file4seqReadFree, &rc ) ;
            c4getFuncPtr( code.hInst, "file4seqReadInit", (void **)&file4seqReadInit, &rc ) ;

            c4getFuncPtr( code.hInst, "file4seqWriteAlloc", (void **)&file4seqWriteAlloc, &rc ) ;
            c4getFuncPtr( code.hInst, "file4seqWriteFree", (void **)&file4seqWriteFree, &rc ) ;
            c4getFuncPtr( code.hInst, "file4seqWriteInit", (void **)&file4seqWriteInit, &rc ) ;

            if ( rc != 0 )
            {
               file4free( &f4 ) ;
               return rc ;
            }

            rc = file4open( f4, code.c4, namestr, doAlloc ) ;
            if ( rc != 0 )
               file4free( &f4 ) ;        /* LY 2002/03/14 : free memory alloc'd by file4alloc() */
            code4 = &code ;
                                return rc;
         }
      int optimize( const int optFlag, const int fileType ) { return file4optimize( f4, optFlag, fileType ) ; }
      int optimizeWrite( const int optFlag ) { return file4optimizeWrite( f4, optFlag ) ; }
      #ifndef S4INTERNAL_COMPILE_CHECK
         unsigned read( const long pos, void *ptr, const unsigned len ) { return file4read( f4, pos, ptr, len ) ; }
         #ifndef S4JNI  /* LY 99/07/08 */
            unsigned read( const long pos, Str4 &string ) { return read( pos, ( void * )string.ptr(), (unsigned)string.len() ); }
            unsigned readAll( const long pos, Str4 &string ) { return readAll( pos, ( void * )string.ptr(), (unsigned)string.len() ) ; }
         #endif
         unsigned readAll( const long pos, void *ptr, const unsigned len ) { return file4readAll( f4, pos, ptr, len ) ;  }
      #endif
      int refresh()              { return file4refresh( f4 ) ; }
      int replace( File4 &newFile )
         {
            int rc ;

            rc = file4replace( f4, newFile.f4 ) ;
            if ( rc != 0 )
               return rc ;

            file4free( &newFile.f4 ) ;

            return rc ;
         }
      int unlock( long posStart, long numBytes ) { return file4unlockInternal( f4, posStart, 0, numBytes, 0 ) ; }
      #ifndef S4INTERNAL_COMPILE_CHECK
         int write( const long pos, const void S4PTR* ptr, const unsigned len ) { return file4write( f4, pos, ptr, len ) ;  }
         int write( const long pos, const char *nullended ) { return write( pos, nullended, strlen( nullended ) ) ; }
         #ifndef S4JNI  /* LY 99/07/08 */
                           int write( const long pos, Str4 &s ) { return write( pos, ( void * )s.ptr(), (unsigned)s.len() ) ; }
         #endif
      #endif
   #else
      int close()                { return file4close( this ) ; }
      // AS May 24/02 - created file4createInternal for internal use to indicate file types
      int create( Code4 &code, S4CONST char *namestr = 0, const int doAlloc = 0 )   { return file4createInternal( this, &code, namestr, doAlloc, OPT4NONE ) ; }
      int flush()                { return file4flush( this ) ; }
      int isValid() const        { return ( hand != INVALID4HANDLE ) ; }
      long len()                 { return file4longGetLo( file4lenLow( this ) ) ; }
      long len1()                { return file4longGetLo( file4lenLow( this ) ) ; }
      #ifndef S4INTERNAL_COMPILE_CHECK
         int setLen( const long l ) { return file4lenSet( this, l ) ; }
      #endif
      int lock( const long start, const long bytes ) { return file4lockInternal( this, start, 0, bytes, 0 ) ; }
      const char S4PTR*fileName() const {return name; }
      // AS May 24/02 - created file4openLow for internal use to indicate file types
      // CS 2006/07/21 change file4openInternal to file4open - make same as C
      int open( Code4 &code, S4CONST char *namestr, const int doAlloc = 0 ) { return file4open( this, &code, namestr, doAlloc ) ; }
      int optimize( const int optFlag, const int fileType ) { return file4optimize( this, optFlag, fileType ) ; }
      int optimizeWrite( const int optFlag ) { return file4optimizeWrite( this, optFlag ) ; }
      #ifndef S4INTERNAL_COMPILE_CHECK
         unsigned read( const long pos, void *ptr, const unsigned len ) { return file4read( this, pos, ptr, len ) ; }
         #ifndef S4JNI  /* LY 99/07/08 */
            unsigned read( const long pos, Str4 &string ) { return read( pos, ( void * )string.ptr(), (unsigned)string.len() ); }
            unsigned readAll( const long pos, Str4 &string ) { return readAll( pos, ( void * )string.ptr(), (unsigned)string.len() ) ; }
         #endif
         unsigned readAll( const long pos, void *ptr, const unsigned len ) { return file4readAll( this, pos, ptr, len ) ;  }
      #endif
      int refresh()              { return file4refresh( this ) ; }
      int replace( File4 &newFile ) { return file4replace( this, &newFile ) ; }
      int unlock( long posStart, long numBytes ) { return file4unlockInternal( this, posStart, 0, numBytes, 0 ) ; }
      #ifndef S4INTERNAL_COMPILE_CHECK
         int write( const long pos, const void S4PTR* ptr, const unsigned len ) { return file4write( this, pos, ptr, len ) ;  }
         int write( const long pos, const char *nullended ) { return write( pos, nullended, (unsigned int)strlen( nullended ) ) ; }  // CS 2006/06/06 cast to uint to fix warning
         #ifndef S4JNI  /* LY 99/07/08 */
                           int write( const long pos, Str4 &s ) { return write( pos, ( void * )s.ptr(), (unsigned)s.len() ) ; }
         #endif
      #endif
   #endif
} ;

#ifndef S4INTERNAL_COMPILE_CHECK
class S4CLASS File4seqRead : public FILE4SEQ_READ
{
public:
   #ifdef D4DLL_CPP
      File4seqRead( File4 &f, const long startPos, void *buf, const unsigned bufLen )
         {
            long rc = 0 ;

            rc = f.file4seqReadAlloc( &f4seqRead ) ;

            if ( rc == 0 )
            {
               rc = f.file4seqReadInit( f4seqRead, f.f4, startPos, buf, bufLen ) ;

               if ( rc == 0 )
               {
                  c4getFuncPtr( f.code4->hInst, "file4seqRead", (void **)&file4seqRead, &rc ) ;
                  c4getFuncPtr( f.code4->hInst, "file4seqReadFree", (void **)&file4seqReadFree, &rc ) ;
                  c4getFuncPtr( f.code4->hInst, "file4seqReadAll", (void **)&file4seqReadAll, &rc ) ;
               }
               else
                  f.file4seqReadFree( &f4seqRead ) ;
            }
         }
      File4seqRead()  {}
         ~File4seqRead()   { if ( f4seqRead ) file4seqReadFree( &f4seqRead ) ; }

      #ifndef S4JNI  /* LY 99/07/08 */
         File4seqRead& operator>>( Str4 &string )
         {
            unsigned numRead = read( string );
            string.setLen(numRead);
            return *this;
         }
      #endif

      void init( File4 &f, long startPos, void *buf, const unsigned bufLen )
         {
            long rc = 0 ;

            rc = f.file4seqReadAlloc( &f4seqRead ) ;

            if ( rc == 0 )
            {
               rc = f.file4seqReadInit( f4seqRead, f.f4, startPos, buf, bufLen ) ;

               if ( rc == 0 )
               {
                  c4getFuncPtr( f.code4->hInst, "file4seqRead", (void **)&file4seqRead, &rc ) ;
                  c4getFuncPtr( f.code4->hInst, "file4seqReadFree", (void **)&file4seqReadFree, &rc ) ;
                  c4getFuncPtr( f.code4->hInst, "file4seqReadAll", (void **)&file4seqReadAll, &rc ) ;
               }
               else
                  f.file4seqReadFree( &f4seqRead ) ;
            }
         }
      unsigned read( void *ptr, const unsigned len )  { return file4seqRead( f4seqRead, ptr, len ) ; }
      #ifndef S4JNI  /* LY 99/07/08 */
         unsigned read( Str4 &s )   { return file4seqRead( f4seqRead, s.ptr(), (unsigned)s.len() ); }
         int readAll( Str4 &s )  { return file4seqReadAll( f4seqRead, s.ptr(), (unsigned)s.len() ); }
      #endif
      int readAll( void *ptr, const unsigned len ) { return file4seqReadAll( f4seqRead, ptr, len ); }
   #else
      File4seqRead( File4 &f, const long startPos, void *buf, const unsigned bufLen )
         { file4seqReadInit( this, &f, startPos, buf, bufLen ) ; }
      File4seqRead()  {}

      #ifndef S4JNI  /* LY 99/07/08 */
         File4seqRead& operator>>( Str4 &string )
         {
            unsigned numRead = read( string );
            string.setLen(numRead);
            return *this;
         }
      #endif

      void init( File4 &f, long startPos, void *buf, const unsigned bufLen )  { file4seqReadInit( this, &f, startPos, buf, bufLen ) ; }
      unsigned read( void *ptr, const unsigned len )  { return file4seqRead( this, ptr, len ) ; }
      #ifndef S4JNI  /* LY 99/07/08 */
         unsigned read( Str4 &s )   { return file4seqRead( this, s.ptr(), (unsigned)s.len() ); }
         int readAll( Str4 &s )  { return file4seqReadAll( this, s.ptr(), (unsigned)s.len() ); }
      #endif
      int readAll( void *ptr, const unsigned len ) { return file4seqReadAll( this, ptr, len ); }
   #endif   /* D4DLL_CPP */
} ;

class S4CLASS File4seqWrite : public FILE4SEQ_WRITE
{
public:
   #ifdef D4DLL_CPP
      File4seqWrite( File4 &f, const long startPos = 0, void *buf = 0, const unsigned bufLen = 0 )
      {
         long rc = 0 ;

         rc = f.file4seqWriteAlloc( &f4seqWrite ) ;

         if ( rc == 0 )
         {
            rc = f.file4seqWriteInit( f4seqWrite, f.f4, startPos, buf, bufLen ) ;

            if ( rc == 0 )
            {
               c4getFuncPtr( f.code4->hInst, "file4seqWrite", (void **)&file4seqWrite, &rc ) ;
               c4getFuncPtr( f.code4->hInst, "file4seqWriteFlush", (void **)&file4seqWriteFlush, &rc ) ;
               c4getFuncPtr( f.code4->hInst, "file4seqWriteFree", (void **)&file4seqWriteFree, &rc ) ;
               c4getFuncPtr( f.code4->hInst, "file4seqWriteRepeat", (void **)&file4seqWriteRepeat, &rc ) ;
            }
            else
               f.file4seqWriteFree( &f4seqWrite ) ;
         }
      }
      File4seqWrite() {}
      ~File4seqWrite()  { if ( f4seqWrite ) file4seqWriteFree( &f4seqWrite ) ; }
      #ifndef S4JNI  /* LY 99/07/08 */
         File4seqWrite &operator<<( Str4 &buf ) { write( buf ) ; return *this ; }
      #endif
      File4seqWrite &operator<<( const long longVal ) ;
      File4seqWrite &operator<<( const char *buf ) { write( buf, strlen( buf ) ); return *this ; }

      void init( File4 &f, const long startPos, void *buf, const unsigned bufLen )
      {
         long rc = 0 ;

         rc = f.file4seqWriteAlloc( &f4seqWrite ) ;

         if ( rc == 0 )
         {
            rc = f.file4seqWriteInit( f4seqWrite, f.f4, startPos, buf, bufLen ) ;

            if ( rc == 0 )
            {
               c4getFuncPtr( f.code4->hInst, "file4seqWrite", (void **)&file4seqWrite, &rc ) ;
               c4getFuncPtr( f.code4->hInst, "file4seqWriteFlush", (void **)&file4seqWriteFlush, &rc ) ;
               c4getFuncPtr( f.code4->hInst, "file4seqWriteFree", (void **)&file4seqWriteFree, &rc ) ;
               c4getFuncPtr( f.code4->hInst, "file4seqWriteRepeat", (void **)&file4seqWriteRepeat, &rc ) ;
            }
            else
               f.file4seqWriteFree( &f4seqWrite ) ;
         }
      }
      int flush() { return file4seqWriteFlush( f4seqWrite ); }
      int repeat( const long numRepeat, const char ch )  { return file4seqWriteRepeat( f4seqWrite, numRepeat, ch ); }
      int write( const void *info, const unsigned infoLen ) { return file4seqWrite( f4seqWrite, info, infoLen ) ; }
      int write( const char *info ) { return file4seqWrite( f4seqWrite, info, strlen( info ) ); }
      #ifndef S4JNI  /* LY 99/07/08 */
         int write( Str4 &s ) { return file4seqWrite( f4seqWrite, s.ptr(), (unsigned)s.len() ) ; }
      #endif
   #else
      File4seqWrite( File4 &f, const long startPos = 0, void *buf = 0, const unsigned bufLen = 0 )
          { file4seqWriteInit( this, &f, startPos, buf, bufLen ) ; }
      File4seqWrite() {}
      #ifndef S4JNI  /* LY 99/07/08 */
         File4seqWrite &operator<<( Str4 &buf ) { write( buf ) ; return *this ; }
      #endif
      File4seqWrite &operator<<( const long longVal ) ;
      File4seqWrite &operator<<( const char *buf ) { write( buf, (unsigned int)strlen( buf ) ); return *this ; }  // CS 2006/06/06 cast to uint to fix warning

      void init( File4 &f, const long startPos, void *buf, const unsigned bufLen )
          { file4seqWriteInit( this, &f, startPos, buf, bufLen ) ; }
      int flush()                 { return file4seqWriteFlush( this ); }
      int repeat( const long numRepeat, const char ch ) { return file4seqWriteRepeat( this, numRepeat, ch ); }
      int write( const void *info, const unsigned infoLen ) { return file4seqWrite( this, info, infoLen ) ; }
      int write( const char *info ) { return file4seqWrite( this, info, (unsigned int)strlen( info ) ); }  // CS 2006/06/06 cast to uint to fix warning
      #ifndef S4JNI  /* LY 99/07/08 */
         int write( Str4 &s ) { return file4seqWrite( this, s.ptr(), (unsigned)s.len() ) ; }
      #endif
   #endif   /* D4DLL_CPP */
} ;
#endif /* S4INTERNAL_COMPILE_CHECK */

class S4CLASS List4 : public LIST4
{
public:
   List4()                     { init() ; }
/*
   these result in error when attempting to derives classes
   List4( List4 & ) ;          // illegal operation, force link error
   List4 &operator =( List4 & );      // illegal operation, force link error
*/
   void *add( void *item )     { l4add( this, item ) ; return item ; }
   void *addAfter( void *anchor, void *item ) { l4addAfter( this, anchor, item ) ; return item; }
   void *addBefore( void *anchor, void *item ) { l4addBefore( this, anchor, item ) ; return item; }
   void S4PTR*first()          { return l4first( this ) ; }
   void S4PTR*last()           { return l4last( this ) ; }
   void S4PTR*next( void *item = NULL ) { return l4next( this, item ) ; }
   unsigned long numNodes()    { return nLink ; }
   void S4PTR*pop()            { return l4pop( this ) ; }
   void S4PTR*prev( void *item ) { return l4prev( this, item ) ; }
   void remove( void *item )   { l4remove( this, item ) ; }
   /* LY 99/10/28 : ::selected may fail l4verifySelected() if not initialized */
   void init()                 { lastNode = 0 ; nLink = 0 ; selected = 0 ; }
} ;

class S4CLASS Sort4 : public SORT4
{
public:
   Sort4() {} ;
   Sort4( Code4 &code, int sLen, int otherLen = 0 ) { sort4init( this, &code, sLen, otherLen ) ; }
   Sort4( Sort4 & ) ;      // Illegal operation, force link error
   Sort4 &operator =( Sort4& ) ;  // Illegal operation, force link error
   void *result, *resultOther ;
   S4LONG  resultRec ;  /* LY 2001/07/29 : changed from long for 64-bit */

   void assignCmp( S4CMP_FUNCTION_PTR f )
      #ifdef D4DLL_CPP
         { sort4assignCmp2( s4, f ) ; }
      #else
         { sort4assignCmp( this, f ) ; }
      #endif
   int free()
      #ifdef D4DLL_CPP
         {
            int rc = sort4free( s4 ) ;
            sort4free2( &s4 ) ;
            return rc ;
         }
      #else
         { return sort4free( this ) ; }
      #endif
   int get()
      #ifdef D4DLL_CPP
         { return sort4get( s4, &resultRec, ( void S4PTR*S4PTR* )&result, ( void S4PTR*S4PTR* )&resultOther ) ; }
      #else
         { return sort4get( this, &resultRec, ( void S4PTR*S4PTR* )&result, ( void S4PTR*S4PTR* )&resultOther ) ; }
      #endif
   int getInit()
      #ifdef D4DLL_CPP
         { return sort4getInit( s4 ) ; }
      #else
         { return sort4getInit( this ) ; }
      #endif
   int init( Code4 &code, int sLen, int otherLen = 0 ) { return sort4init( this, &code, sLen, otherLen ) ; }
   int put( void *sortInfo, void *otherInfo = NULL, long rec = 0L )
      #ifdef D4DLL_CPP
         { return sort4put( s4, rec, sortInfo, otherInfo ) ; }
      #else
         { return sort4put( this, rec, sortInfo, otherInfo ) ; }
      #endif
} ;

class S4CLASS Mem4
{
public:
   Mem4() { mem = 0; }
   Mem4( Code4 &code, int start, int size, int expand, int temp = 1 )
       { mem = mem4create( &code, start, size, expand, temp ) ; }
   Mem4( MEM4 S4PTR * m ) { this->mem = m ; }
   MEM4 S4PTR *mem ;
   void S4PTR*alloc()     { return mem4alloc( mem ) ; }
   int create( Code4 &code, int unitStart, int unitSize, int unitExpand, int makeTemp = 1 )
       { mem = mem4create( &code, unitStart, unitSize, unitExpand, makeTemp ) ; return ( mem==0 ) ? -1 : 0; }
   void free( void *ptr ) { mem4free( mem, ptr ) ; }
   int isValid()          { return mem != 0 ; }
   void release()         { mem4release( mem ) ; }
} ;

class S4CLASS Relate4
{
public:
   Relate4() { relate = 0 ; }
   Relate4( Relate4 master, Data4 slave, char *masterExpr, Tag4 t )
       { relate = relate4createSlave( master.relate, slave.dat(),masterExpr, t.tag ) ; }
   Relate4( RELATE4 S4PTR *r ) { relate = r ; }
   Data4 data()
      #ifdef D4DLL_CPP
         { return ( relate ) ? Data4( ( DATA4 * )relate->d4 ) : Data4( ( DATA4 * )NULL ); }
      #else
         { return ( relate ) ? Data4( ( DATA4 * )relate->data ) : Data4( ( DATA4 * )NULL ); }
      #endif
   Tag4 dataTag()
      #ifdef D4DLL_CPP
         { return ( relate ) ? Tag4( ( TAG4 * )relate->t4 ) : Tag4( ( TAG4 * )NULL ); }
      #else
         { return ( relate ) ? Tag4( ( TAG4 * )relate->dataTag ) : Tag4( ( TAG4 * )NULL ); }
      #endif
   int doOne()         { return relate4doOne( relate ) ; }
   int errorAction( int a ) { return relate4errorAction( relate, a ) ; }
   int init( Relate4 master, Data4 slave, char *masterExpr, Tag4 t ) ;

   int isValid()
      #ifdef D4DLL_CPP
         { return ( relate ? relate->r4 != NULL : 0 ) ; }
      #else
         { return relate != NULL ; }
      #endif
   Relate4 master()
      #ifdef D4DLL_CPP
         { return ( relate ) ? Relate4( ( RELATE4 * )relate->masterR4 ):Relate4( ( RELATE4 * )NULL ) ; }
      #else
         { return ( relate ) ? Relate4( ( RELATE4 * )relate->master ):Relate4( ( RELATE4 * )NULL ) ; }
      #endif

   #ifndef S4CB51
      const char S4PTR *masterExpr() { return relate4masterExpr( relate ) ; }
   #endif

   int matchLen( int p )
      #ifdef D4DLL_CPP
         { return relate4matchLenVB( relate, p ) ; }
      #else
         { return relate4matchLen( relate, p ) ; }
      #endif
   int type( int p )   { return relate4type( relate, p ) ; }

   RELATE4 S4PTR *relate ;
} ;

class S4CLASS Relate4iterator : public Relate4
{
public:
  Relate4iterator()                  { relate = 0 ; }
  Relate4iterator( class Relate4 r ) { relate = r.relate ; }
  int next()                         { return relate4next( &relate ) !=2 ; }
  int nextPosition()                 { return relate4next( &relate ) ; }
} ;

class S4CLASS Relate4set:public Relate4
{
public:
   Relate4set( Data4 data ) { relate = relate4init( data.dat() ) ; }
   Relate4set()          {relate = 0; }
   Relate4set( RELATE4 S4PTR *r ) { relate = r ; }

   int bottom()             { return relate4bottom( relate ) ; }
   int callbackInit( QUERY_CALLBACK callback, long milliseconds )
      { return relate4callbackInit( relate, callback, milliseconds ) ; }
   int callbackInitUndo( )  { return relate4callbackInitUndo( relate ) ; }
   // AS Added Nov 14/05
   int querySetCallbackInit( QUERY_SET_CALLBACK callback )
      { return relate4querySetCallbackInit( relate, callback ) ; }
   int querySetCallbackInitUndo( )  { return relate4querySetCallbackInitUndo( relate ) ; }
   void changed()           { relate4changed( relate ) ; }
   unsigned long count()  { return relate4count( relate ) ; }
   int doAll()              { return relate4doAll( relate ) ; }
   int eof()                { return relate4eof( relate ) ; }
   int free( int p = 0 )    { int rc = relate4free( relate, p ) ; relate = 0; return rc; }
   int init( Data4 data ) ;
   int lockAdd()            { return relate4lockAdd( relate ) ; }
   #ifndef S4CB51
      int optimizeable()    { return relate4optimizeable( relate ) ; }
   #endif
   int querySet( S4CONST char *p = NULL )  { return relate4querySet( relate, p ) ; }
   long readBuffer( long numRecsToBuf, short doMemos )
      { return relate4readBuffer( relate, numRecsToBuf, doMemos ) ; }
   int retain( int flag )   { return relate4retain( relate, flag ) ; }
   int skip( long l = 1 )   { return relate4skip( relate, l ) ; }
   int skipEnable( int doEnable = 1 )
      #ifdef D4DLL_CPP
         { return relate4skipEnableVB( relate, doEnable ); }
      #else
         { return relate4skipEnable( relate, doEnable ); }
      #endif
   int skipMaster( long l = 1 )  { return relate4skipMaster( relate, l ) ; }
   int sortSet( const char *sort=NULL ) {return relate4sortSet( relate, sort ); }
   int top()                { return relate4top( relate ) ; }
} ;


#ifdef S4CLIENT
class S4CLASS Pipe4recv
{
   private:
   PIPE4RECV S4PTR *pipeRecv ;

   public:
   Pipe4recv() { pipeRecv = 0 ; }
   Pipe4recv( Code4& c4, const char *pipeName ) { pipeRecv = 0 ; open( c4, pipeName ) ; }

   void close()   { pipe4recvClose( pipeRecv ) ; pipeRecv = 0 ; }
   int isValid()  { return ( pipeRecv ? 1 : 0 ) ; }
   int open( Code4& c4, const char *pipeName )
   {
      if ( pipeRecv )
         close() ;
      pipeRecv = pipe4recvOpen( &c4, pipeName ) ;
      return ( !pipeRecv ? r4success : c4.errorCode ) ;
   }
   short recvMessage( unsigned long *msgLen, void **msg )   { return pipe4recvMessage( pipeRecv, msgLen, msg ) ; }
} ;


class S4CLASS Pipe4send
{
   private:
   PIPE4SEND S4PTR *pipeSend ;

   public:
   Pipe4send() { pipeSend = 0 ; }
   Pipe4send( Code4& c4, const char *pipeName ) { pipeSend = 0 ; open( c4, pipeName ) ; }

   void close()   { pipe4sendClose( pipeSend ) ; pipeSend = 0 ; }
   int isValid()  { return ( pipeSend ? 1 : 0 ) ; }
   int open( Code4& c4, const char *pipeName )
   {
      if ( pipeSend )
         close() ;
      pipeSend = pipe4sendOpen( &c4, pipeName ) ;
      return( !pipeSend ? r4success : c4.errorCode ) ;
   }
   short sendMessage( char *message )  { return pipe4sendMessage( pipeSend, message ) ; }
   short sendMessage( void *message, unsigned long messageLen )  { return pipe4sendMessageN( pipeSend, message, messageLen ) ; }
} ;
#endif


S4INLINE2 Data4 Code4::data( const char *alias )
{
   Data4 r ;
   r.data = code4data( this,alias ) ;
   return r ;
}

// CS 1999/09/21 The init_p member signals that the object has been initialized.
// If it has, call initUndo first to avoid memory leaks.
#ifdef D4DLL_CPP
S4INLINE2 int Code4::init( const char *dllName )
#else
S4INLINE2 int Code4::init( void )
#endif
{
   if ( init_p )
      #ifdef D4DLL_CPP
         initUndoDll( ) ;
      #else
         code4initUndo( this ) ;
      #endif

   #ifdef D4DLL_CPP
      int rc = allocDll( dllName ) ;
   #else
      int rc = code4init( this ) ;
   #endif
   if ( rc == r4success )
      init_p = 1 ;
   else
      init_p = 0 ;
   s4memset( stringTypes, 0, sizeof( stringTypes ) ) ;
   return rc ;
}

// CS 2000/10/26 Do nothing if db already closed.
S4INLINE2 int Data4::close()
{
   if ( data )
   {
      int rc = d4close( data ) ;
      data = 0;
      return rc;
   }
   else
      return 0 ;
}

S4INLINE2 int Data4::create( Code4& code, S4CONST char *name, Field4info& f, Tag4info& t)
{
   data = d4create(&code, name, f.fields(), t.tags());
   #ifdef D4DLL_CPP
      return code.c4getErrorCode( code.c4 ) ;
   #else
      return code.errorCode;
   #endif
}

S4INLINE2 int Data4::create( Code4& code, S4CONST char *name, Field4info& f)
{
   data = d4create(&code, name, f.fields(), 0);
   #ifdef D4DLL_CPP
      return code.c4getErrorCode( code.c4 ) ;
   #else
      return code.errorCode;
   #endif
}

S4INLINE2 int Data4::fieldsAdd( short nFields, FIELD4INFO f[] )
{
   DATA4 *newData = d4fieldsAdd( data, nFields, f ) ;
   if (newData)
      data = newData;
   #ifdef D4DLL_CPP
      /* LY 2003/12/31 : changed from data->c4 */
      return g_c4->c4getErrorCode( data->c4->c4 ) ;
   #else
      return data->codeBase->errorCode;
   #endif
}

S4INLINE2 int Data4::fieldsAdd( Field4info& f )
{
   return fieldsAdd( (short)f.numFields(), f.fields() ) ;
}


S4INLINE2 int Data4::modifyStructure(Field4info& fields)
{
   return modifyStructure(fields.fields(), 0);
}

S4INLINE2 int Data4::modifyStructure(FIELD4INFO *fields, TAG4INFO *tags)
{
   DATA4 *newData = d4modifyStructure( data, fields, tags ) ;
   if (newData)
      data = newData;
   #ifdef D4DLL_CPP
      /* LY 2003/12/31 : changed from data->c4 */
      return g_c4->c4getErrorCode( data->c4->c4 ) ;
   #else
      return data->codeBase->errorCode;
   #endif
}

S4INLINE2 int Data4::modifyStructure(Field4info& fields, Tag4info& tags)
{
   return modifyStructure(fields.fields(), tags.tags());
}


S4INLINE2 Index4 Data4::index( const char *name=NULL )
{
   return Index4( d4index( data, name ) ) ;
}


S4INLINE2 int Data4::compress( Data4 d, const char *compressName, short blockSize )
{
   data = d4compress( d.dat(), compressName, blockSize ) ;

   // AS Feb 22/06 dual dll fix
   if ( data != 0 )
   {
      #ifdef D4DLL_CPP
         /* LY 2003/12/31 : changed from data->c4 */
         return g_c4->c4getErrorCode( data->c4->c4 ) ;
      #else
         return data->codeBase->errorCode;
      #endif
   }
   else
      return -1 ;
}


S4INLINE2 int Data4::openClone( Data4 d )
{
   data = d4openClone( d.dat() ) ;

   if ( data != 0 )
      #ifdef D4DLL_CPP
         /* LY 2003/12/31 : changed from d.data->c4 */
         return g_c4->c4getErrorCode( d.data->c4->c4 ) ;
      #else
         return data->codeBase->errorCode ;
      #endif
   else
      return -1 ;
}

S4INLINE2 void Data4::select( Tag4 tag )
{
   d4tagSelect( data, tag.tag ) ;
}

S4INLINE2 void Data4::select( const char *tagName )
{
   if( tagName != NULL )
   {
      TAG4 *tag = d4tag( data, tagName ) ;
      d4tagSelect( data, tag ) ;
   }
   else
      d4tagSelect( data, NULL ) ;
}

S4INLINE2 Expr4::Expr4( Data4 d, const char *ex )
{
   expr = expr4parse( d.dat(),ex ) ;
}

S4INLINE2 int Expr4::parse( Data4 d, const char *ex )
{
   int rc ;
   #ifdef E4PARM_HIGH
      if ( d.dat() == 0 )
         rc = error4( 0, e4struct, ( E60603 ) ) ;
      else
   #endif
   #ifdef E4DEBUG
      if ( expr )
         error4( d.dat()->codeBase, e4info, E60601 ) ;
      else
   #endif
   {
      expr = expr4parse( d.dat(), ex ) ;
      #ifdef D4DLL_CPP
         /* LY 2003/12/31 : changed from d.data->c4 */
         rc = g_c4->c4getErrorCode( d.data->c4->c4 ) ;
      #else
         rc = d.dat()->codeBase->errorCode ;
      #endif
   }
   return rc ;
}

/* S4INLINE2 Data4 Expr4::data()const
   {
      DEBUG4PTR( expr == 0, E60602 )
      return Data4( expr->data ) ;
   }
*/
#ifndef S4JNI  /* LY 99/07/08 */
S4INLINE2 void Field4::changed()
{
   #ifdef E4PARM_HIGH
      if ( field == 0 )
         error4( 0, e4struct, ( E61203 ) );
      else
   #endif
   #ifdef D4DLL_CPP
      d4changed( field->d4, 1 ) ;
   #else
      d4changed( field->data, 1 ) ;
   #endif
   #ifdef S4CLIENT_OR_FOX
      #ifdef D4DLL_CPP
         if ( d4versionCB( field->d4 ) == 0x30 )
      #else
         if ( d4version( field->data ) == 0x30 )
      #endif
         f4assignNotNull( field ) ;
   #endif
}

S4INLINE2 Field4& Field4::operator =( const Field4& f )
{
   #ifdef D4DLL_CPP
      field = d4field( f.field->d4, f4name( f.field ) ) ;
   #else
      // AS Oct 31/03 support for long field names
      init( f.field->data, f.field->longName ) ;
   #endif
   return *this;
}

S4INLINE2 int Field4::number()
{
   int rc ;
   #ifdef E4PARM_HIGH
      if ( field == 0 )
         rc = error4( 0, e4struct, ( E61210 ) ) ;
      else
   #endif
   #ifdef D4DLL_CPP
      rc = d4fieldNumber( field->d4, name() ) ;
   #else
      rc = d4fieldNumber( field->data, name() ) ;
   #endif
   return rc ;
}
#endif   /* !S4JNI  LY 99/07/08 */

#ifdef __BORLANDC__
   #pragma warn .aus
   #pragma warn .par
#endif
S4INLINE2 int Index4::create( Data4 d, Tag4info& info )
{
   int rc ;
   #ifdef E4PARM_HIGH
   if ( d.dat() == 0 )
      rc = error4( 0, e4struct, ( E60702 ) ) ;
   else
   {
   #endif
   index = i4create( d.dat(), 0, info.tags()) ;
   #ifdef D4DLL_CPP
      /* LY 2003/12/31 : changed from d.data->c4 */
      rc = g_c4->c4getErrorCode( d.data->c4->c4 ) ;
   #else
      rc = d.dat()->codeBase->errorCode ;
   #endif
   #ifdef E4PARM_HIGH
   }
   #endif
   return rc;
}

S4INLINE2 int Index4::create( Data4 d, S4CONST char *name, S4CONST TAG4INFO *info )
{
   int rc ;
   #ifdef E4PARM_HIGH
      if ( d.dat() == 0 )
         rc = error4( 0, e4struct, ( 60702L ) ) ;
      else
   #endif
   {
      index = i4create( d.dat(), name, info ) ;
      #ifdef D4DLL_CPP
         /* LY 2003/12/31 : changed from d.data->c4 */
         rc = g_c4->c4getErrorCode( d.data->c4->c4 ) ;
       #else
         rc = d.dat()->codeBase->errorCode ;
      #endif
   }
   return rc ;
}

S4INLINE2 int Index4::create( Data4 d, S4CONST TAG4INFO *info )
{
   int rc ;
   #ifdef E4PARM_HIGH
      if ( d.dat() == 0 )
         rc = error4( 0, e4struct, ( 60702L ) ) ;
      else
   #endif
   {
      index = i4create( d.dat(), 0, info) ;
      #ifdef D4DLL_CPP
         /* LY 2003/12/31 : changed from d.data->c4 */
         rc = g_c4->c4getErrorCode( d.data->c4->c4 ) ;
      #else
         rc = d.dat()->codeBase->errorCode ;
      #endif
   }
   return rc ;
}

S4INLINE2 int Index4::create( Data4 d, S4CONST char *name, Tag4info& info )
{
   int rc ;
   #ifdef E4PARM_HIGH
      if ( d.dat() == 0 )
         rc = error4( 0, e4struct, ( E60702 ) ) ;
      else
   #endif
   {
      index = i4create( d.dat(), name, info.tags() ) ;
      #ifdef D4DLL_CPP
         /* LY 2003/12/31 : changed from d.data->c4 */
         rc = g_c4->c4getErrorCode( d.data->c4->c4 ) ;
      #else
         rc = d.dat()->codeBase->errorCode ;
      #endif
   }
   return rc ;
}

S4INLINE2 int Index4::create( Data4 d, S4CONST char *name, Tag4info& tagData, REINDEX_CALLBACK callback, long milliseconds)
{
   return create(d, name, tagData.tags(), callback, milliseconds ) ;
}

S4INLINE2 int Index4::create( Data4 d, Tag4info& tagData, REINDEX_CALLBACK callback, long milliseconds )
{
   return create(d, 0, tagData.tags(), callback, milliseconds ) ;
}


S4INLINE2 int Index4::open( Data4 d, S4CONST char *file = NULL )
{
   int rc ;
   #ifdef E4PARM_HIGH
      if ( d.dat() == 0 )
         rc = error4( 0, e4struct, ( 60709L ) ) ;
      else
   #endif
   {
      index = i4open( d.dat(), file ) ;
      #ifdef D4DLL_CPP
         /* LY 2003/12/31 : changed from d.data->c4 */
         rc = g_c4->c4getErrorCode( d.data->c4->c4 ) ;
      #else
         rc = d.dat()->codeBase->errorCode ;
      #endif
   }
   return rc ;
}

S4INLINE2 Tag4 Index4::tag( const char *name )
{
   Tag4 t ;
   t.tag = i4tag( index,name ) ;
   return t ;
}

S4INLINE2 int Relate4::init( Relate4 master, Data4 slave, char *masterExpr, Tag4 t )
{
   int rc ;
   #ifdef E4PARM_HIGH
      if ( slave.dat() == 0 )
         rc = error4( 0, e4struct, ( E61301 ) ) ;
      else
   #endif
   {
      relate = relate4createSlave( master.relate, slave.dat(), masterExpr, t.tag ) ;
      #ifdef D4DLL_CPP
         /* LY 2003/12/31 : changed from slave.data->c4 */
         rc = g_c4->c4getErrorCode( slave.data->c4->c4 ) ;
      #else
         rc = slave.dat()->codeBase->errorCode ;
      #endif
   }
   return rc ;
}

S4INLINE2 int Relate4set::init( Data4 data )
{
   int rc ;
   #ifdef E4PARM_HIGH
      if ( data.dat() == 0 )
         rc = error4( 0, e4struct, ( E61302 ) ) ;
      else
   #endif
   {
      relate = relate4init( data.dat() ) ;
      #ifdef D4DLL_CPP
         /* LY 2003/12/31 : changed from data.data->c4 */
         rc = g_c4->c4getErrorCode( data.data->c4->c4 ) ;
      #else
         rc = data.dat()->codeBase->errorCode ;
      #endif
   }
   return rc ;
}

#ifndef S4CLIENT
S4INLINE2 int Tag4::descending()
{
   #ifdef E4PARM_HIGH
      if ( tag == 0 )
      {
         error4( 0, e4struct, ( 60913L ) ) ;
         return 0 ;
      }
   #endif
   #ifdef D4DLL_CPP
      return t4descending( tag ) ;
   #else
      return tfile4isDescending( tag->tagFile ) ;
   #endif
}
#endif

S4INLINE2 int Tag4::remove()
{
   int rc = i4tagRemove( tag ) ;
   if ( rc == r4success )
      tag = 0 ;
   return rc ;
}

#if !defined(S4OFF_REPORT) && !defined(D4DLL_CPP)
   REPORT4 * S4FUNCTION  report4retrieve( Code4 &, char *, int, char * ) ;
   RELATE4 * S4FUNCTION relate4retrieve( Code4 &, char *, int, char * ) ;
   int S4FUNCTION relate4save( Relate4set &, char *, int ) ;
#endif /* NOT S4OFF_REPORT */

#ifdef D4DLL_CPP
S4INLINE2 int Code4::allocDll( const char *dllName )
{
   long rc = 0 ;

   hInst = LoadLibrary( dllName ) ;
   if ( hInst == NULL )
      return e4fileFind ;

   c4getFuncPtr( hInst, "code4allocLow", (void **)&code4alloc, &rc ) ;
   if ( code4alloc != 0 )
   {
      c4 = code4alloc( 1, DEF4PROTOCOL, S4VERSION ) ;
      if ( c4 == 0 )
         rc = -1 ;
   }

   c4getFuncPtr( hInst, "code4accessMode", (void **)&(code4accessMode), &rc ) ;
   c4getFuncPtr( hInst, "code4actionCode", (void **)&(code4actionCode), &rc ) ;
   c4getFuncPtr( hInst, "code4autoOpen", (void **)&(code4autoOpen), &rc ) ;
   c4getFuncPtr( hInst, "code4calcCreate", (void **)&(code4calcCreate), &rc ) ;
   c4getFuncPtr( hInst, "code4calcReset", (void **)&(code4calcReset), &rc ) ;
   c4getFuncPtr( hInst, "code4codePage", (void **)&(code4codePage), &rc ) ;
   c4getFuncPtr( hInst, "code4collate", (void **)&(code4collate), &rc ) ;
   c4getFuncPtr( hInst, "code4collatingSequence", (void **)&(code4collatingSequence), &rc ) ;
   c4getFuncPtr( hInst, "code4collateUnicode", (void **)&(code4collateUnicode), &rc ) ;
   c4getFuncPtr( hInst, "code4compatibility", (void **)&(code4compatibility), &rc ) ;
   c4getFuncPtr( hInst, "code4compress", (void **)&(code4compress), &rc ) ;
   c4getFuncPtr( hInst, "code4compressConfigure", (void **)&(code4compressConfigure), &rc ) ;
   c4getFuncPtr( hInst, "code4connect", (void **)&(code4connect), &rc ) ;
   c4getFuncPtr( hInst, "code4close", (void **)&(code4close), &rc ) ;
   c4getFuncPtr( hInst, "code4createTemp", (void **)&(code4createTemp), &rc ) ;
   c4getFuncPtr( hInst, "code4dateFormat", (void **)&(code4dateFormat), &rc ) ;
   c4getFuncPtr( hInst, "code4dateFormatSet", (void **)&(code4dateFormatSet), &rc ) ;
   #if defined(S4ENCRYPT_HOOK) || defined(S4ENCRYPT_COM) || defined(S4PREPROCESS_COM) || defined(S4PREPROCESS_FILE)
      c4getFuncPtr( hInst, "code4encryptConnection", (void **)&(code4encryptConnection), &rc ) ;
      c4getFuncPtr( hInst, "code4encryptFile", (void **)&(code4encryptFile), &rc ) ;
   #endif
   c4getFuncPtr( hInst, "code4errCreate", (void **)&(code4errCreate), &rc ) ;
   c4getFuncPtr( hInst, "code4errDefaultUnique", (void **)&(code4errDefaultUnique), &rc ) ;
   c4getFuncPtr( hInst, "code4errExpr", (void **)&(code4errExpr), &rc ) ;
   c4getFuncPtr( hInst, "code4errFieldName", (void **)&(code4errFieldName), &rc ) ;
   c4getFuncPtr( hInst, "code4errGo", (void **)&(code4errGo), &rc ) ;
   c4getFuncPtr( hInst, "code4errOff", (void **)&(code4errOff), &rc ) ;
   c4getFuncPtr( hInst, "code4errorCode", (void **)&(code4errorCode), &rc ) ;
   c4getFuncPtr( hInst, "code4errOpen", (void **)&(code4errOpen), &rc ) ;
   c4getFuncPtr( hInst, "code4errRelate", (void **)&(code4errRelate), &rc ) ;
   c4getFuncPtr( hInst, "code4errSkip", (void **)&(code4errSkip), &rc ) ;
   c4getFuncPtr( hInst, "code4errTagName", (void **)&(code4errTagName), &rc ) ;
   c4getFuncPtr( hInst, "code4exit", (void **)&(code4exit), &rc ) ;
   c4getFuncPtr( hInst, "code4flush", (void **)&(code4flush), &rc ) ;
   c4getFuncPtr( hInst, "code4fileFlush", (void **)&(code4fileFlush), &rc ) ;
   c4getFuncPtr( hInst, "code4hWnd", (void **)&(code4hWnd), &rc ) ;
   c4getFuncPtr( hInst, "code4indexBlockSize", (void **)&(code4indexBlockSize), &rc ) ;
   c4getFuncPtr( hInst, "code4indexBlockSizeSet", (void **)&(code4indexBlockSizeSet), &rc ) ;
   c4getFuncPtr( hInst, "code4indexExtension", (void **)&(code4indexExtension), &rc ) ;
   c4getFuncPtr( hInst, "code4indexFormat", (void **)&(code4indexFormat), &rc ) ;
   c4getFuncPtr( hInst, "code4initUndo", (void **)&(code4initUndo), &rc ) ;
   c4getFuncPtr( hInst, "code4largeOn", (void **)&(code4largeOn), &rc ) ;
   c4getFuncPtr( hInst, "code4lock", (void **)&(code4lock), &rc ) ;
   c4getFuncPtr( hInst, "code4lockAttempts", (void **)&(code4lockAttempts), &rc ) ;
   c4getFuncPtr( hInst, "code4lockAttemptsSingle", (void **)&(code4lockAttemptsSingle), &rc ) ;
   c4getFuncPtr( hInst, "code4lockClear", (void **)&(code4lockClear), &rc ) ;
   c4getFuncPtr( hInst, "code4lockDelay", (void **)&(code4lockDelay), &rc ) ;
   c4getFuncPtr( hInst, "code4lockEnforce", (void **)&(code4lockEnforce), &rc ) ;
   c4getFuncPtr( hInst, "code4lockFileName", (void **)&(code4lockFileName), &rc ) ;
   c4getFuncPtr( hInst, "code4lockItem", (void **)&(code4lockItem), &rc ) ;
   c4getFuncPtr( hInst, "code4lockNetworkId", (void **)&(code4lockNetworkId), &rc ) ;
   c4getFuncPtr( hInst, "code4lockUserId", (void **)&(code4lockUserId), &rc ) ;
   c4getFuncPtr( hInst, "code4log", (void **)&(code4log), &rc ) ;
   c4getFuncPtr( hInst, "code4logCreate", (void **)&(code4logCreate), &rc ) ;
   c4getFuncPtr( hInst, "code4logFileName", (void **)&(code4logFileName), &rc ) ;
   c4getFuncPtr( hInst, "code4logOpen", (void **)&(code4logOpen), &rc ) ;
   c4getFuncPtr( hInst, "code4logOpenOff", (void **)&(code4logOpenOff), &rc ) ;
   c4getFuncPtr( hInst, "code4memoCompress", (void **)&(code4memoCompress), &rc ) ;
   c4getFuncPtr( hInst, "code4memExpandBlock", (void **)&(code4memExpandBlock), &rc ) ;
   c4getFuncPtr( hInst, "code4memExpandData", (void **)&(code4memExpandData), &rc ) ;
   c4getFuncPtr( hInst, "code4memExpandIndex", (void **)&(code4memExpandIndex), &rc ) ;
   c4getFuncPtr( hInst, "code4memExpandLock", (void **)&(code4memExpandLock), &rc ) ;
   c4getFuncPtr( hInst, "code4memExpandTag", (void **)&(code4memExpandTag), &rc ) ;
   c4getFuncPtr( hInst, "code4memSizeBlock", (void **)&(code4memSizeBlock), &rc ) ;
   c4getFuncPtr( hInst, "code4memSizeBuffer", (void **)&(code4memSizeBuffer), &rc ) ;
   c4getFuncPtr( hInst, "code4memSizeMemo", (void **)&(code4memSizeMemo), &rc ) ;
   c4getFuncPtr( hInst, "code4memSizeMemoExpr", (void **)&(code4memSizeMemoExpr), &rc ) ;
   c4getFuncPtr( hInst, "code4memSizeSortBuffer", (void **)&(code4memSizeSortBuffer), &rc ) ;
   c4getFuncPtr( hInst, "code4memSizeSortPool", (void **)&(code4memSizeSortPool), &rc ) ;
   c4getFuncPtr( hInst, "code4memStartBlock", (void **)&(code4memStartBlock), &rc ) ;
   c4getFuncPtr( hInst, "code4memStartData", (void **)&(code4memStartData), &rc ) ;
   c4getFuncPtr( hInst, "code4memStartIndex", (void **)&(code4memStartIndex), &rc ) ;
   c4getFuncPtr( hInst, "code4memStartLock", (void **)&(code4memStartLock), &rc ) ;
   c4getFuncPtr( hInst, "code4memStartMax", (void **)&(code4memStartMax), &rc ) ;
   c4getFuncPtr( hInst, "code4memStartTag", (void **)&(code4memStartTag), &rc ) ;
   c4getFuncPtr( hInst, "code4optAll", (void **)&(code4optAll), &rc ) ;
   c4getFuncPtr( hInst, "code4optimize", (void **)&(code4optimize), &rc ) ;
   c4getFuncPtr( hInst, "code4optimizeWrite", (void **)&(code4optimizeWrite), &rc ) ;
   c4getFuncPtr( hInst, "code4optStart", (void **)&(code4optStart), &rc ) ;
   c4getFuncPtr( hInst, "code4optSuspend", (void **)&(code4optSuspend), &rc ) ;
   c4getFuncPtr( hInst, "code4ping", (void **)&(code4ping), &rc ) ;
   c4getFuncPtr( hInst, "code4readLock", (void **)&(code4readLock), &rc ) ;
   c4getFuncPtr( hInst, "code4readOnly", (void **)&(code4readOnly), &rc ) ;
   c4getFuncPtr( hInst, "code4safety", (void **)&(code4safety), &rc ) ;
   c4getFuncPtr( hInst, "code4singleOpen", (void **)&(code4singleOpen), &rc ) ;
   c4getFuncPtr( hInst, "code4timeout", (void **)&(code4timeout), &rc ) ;
   c4getFuncPtr( hInst, "code4timeoutSet", (void **)&(code4timeoutSet), &rc ) ;
   c4getFuncPtr( hInst, "code4tranCommit", (void **)&(code4tranCommit), &rc ) ;
   c4getFuncPtr( hInst, "code4tranRollback", (void **)&(code4tranRollback), &rc ) ;
   c4getFuncPtr( hInst, "code4tranStart", (void **)&(code4tranStart), &rc ) ;
   c4getFuncPtr( hInst, "code4tranStatusCB", (void **)&(code4tranStatusCB), &rc ) ;
   c4getFuncPtr( hInst, "code4unlock", (void **)&(code4unlock), &rc ) ;
   c4getFuncPtr( hInst, "code4unlockAutoCB", (void **)&(code4unlockAutoCB), &rc ) ;
   c4getFuncPtr( hInst, "code4unlockAutoSetCB", (void **)&(code4unlockAutoSetCB), &rc ) ;
   c4getFuncPtr( hInst, "code4verifySet", (void **)&(code4verifySet), &rc ) ;
   c4getFuncPtr( hInst, "code4validate", (void **)&(code4validate), &rc ) ;

   c4getFuncPtr( hInst, "c4getAccessMode", (void **)&(c4getAccessMode), &rc ) ;
   c4getFuncPtr( hInst, "c4getAutoOpen", (void **)&(c4getAutoOpen), &rc ) ;
   c4getFuncPtr( hInst, "c4getCodePage", (void **)&(c4getCodePage), &rc ) ;
   c4getFuncPtr( hInst, "c4getCompatibility", (void **)&(c4getCompatibility), &rc ) ;
   c4getFuncPtr( hInst, "c4getCompressedMemos", (void **)&(c4getCompressedMemos), &rc ) ;
   c4getFuncPtr( hInst, "c4getCreateTemp", (void **)&(c4getCreateTemp), &rc ) ;
   c4getFuncPtr( hInst, "c4getErrCreate", (void **)&(c4getErrCreate), &rc ) ;
   c4getFuncPtr( hInst, "c4getErrDefaultUnique", (void **)&(c4getErrDefaultUnique), &rc ) ;
   c4getFuncPtr( hInst, "c4getErrExpr", (void **)&(c4getErrExpr), &rc ) ;
   c4getFuncPtr( hInst, "c4getErrFieldName", (void **)&(c4getErrFieldName), &rc ) ;
   c4getFuncPtr( hInst, "c4getErrGo", (void **)&(c4getErrGo), &rc ) ;
   c4getFuncPtr( hInst, "c4getErrOff", (void **)&(c4getErrOff), &rc ) ;
   c4getFuncPtr( hInst, "c4getErrOpen", (void **)&(c4getErrOpen), &rc ) ;
   c4getFuncPtr( hInst, "c4getErrorCode", (void **)&(c4getErrorCode), &rc ) ;
   c4getFuncPtr( hInst, "c4getErrRelate", (void **)&(c4getErrRelate), &rc ) ;
   c4getFuncPtr( hInst, "c4getErrSkip", (void **)&(c4getErrSkip), &rc ) ;
   c4getFuncPtr( hInst, "c4getErrTagName", (void **)&(c4getErrTagName), &rc ) ;
   c4getFuncPtr( hInst, "c4getFileFlush", (void **)&(c4getFileFlush), &rc ) ;
   c4getFuncPtr( hInst, "c4getLockAttempts", (void **)&(c4getLockAttempts), &rc ) ;
   c4getFuncPtr( hInst, "c4getLockAttemptsSingle", (void **)&(c4getLockAttemptsSingle), &rc ) ;
   c4getFuncPtr( hInst, "c4getLockDelay", (void **)&(c4getLockDelay), &rc ) ;
   c4getFuncPtr( hInst, "c4getLockEnforce", (void **)&(c4getLockEnforce), &rc ) ;
   c4getFuncPtr( hInst, "c4getLog", (void **)&(c4getLog), &rc ) ;
   c4getFuncPtr( hInst, "c4getOptimize", (void **)&(c4getOptimize), &rc ) ;
   c4getFuncPtr( hInst, "c4getOptimizeWrite", (void **)&(c4getOptimizeWrite), &rc ) ;
   c4getFuncPtr( hInst, "c4getReadLockDo", (void **)&(c4getReadLockDo), &rc ) ;
   c4getFuncPtr( hInst, "c4getReadOnlyDo", (void **)&(c4getReadOnlyDo), &rc ) ;
   c4getFuncPtr( hInst, "c4getSafety", (void **)&(c4getSafety), &rc ) ;
   c4getFuncPtr( hInst, "c4getSingleOpen", (void **)&(c4getSingleOpen), &rc ) ;

   c4getFuncPtr( hInst, "c4setAccessMode", (void **)&(c4setAccessMode), &rc ) ;
   c4getFuncPtr( hInst, "c4setAcceptTimeOut", (void **)&(c4setAcceptTimeOut), &rc ) ;
   c4getFuncPtr( hInst, "c4setAutoOpen", (void **)&(c4setAutoOpen), &rc ) ;
   c4getFuncPtr( hInst, "c4setCodePage", (void **)&(c4setCodePage), &rc ) ;
   c4getFuncPtr( hInst, "c4setCompatibility", (void **)&(c4setCompatibility), &rc ) ;
   c4getFuncPtr( hInst, "c4setCompressedMemos", (void **)&(c4setCompressedMemos), &rc ) ;
   c4getFuncPtr( hInst, "c4setCreateTemp", (void **)&(c4setCreateTemp), &rc ) ;
   c4getFuncPtr( hInst, "c4setErrCreate", (void **)&(c4setErrCreate), &rc ) ;
   c4getFuncPtr( hInst, "c4setErrDefaultUnique", (void **)&(c4setErrDefaultUnique), &rc ) ;
   c4getFuncPtr( hInst, "c4setErrExpr", (void **)&(c4setErrExpr), &rc ) ;
   c4getFuncPtr( hInst, "c4setErrFieldName", (void **)&(c4setErrFieldName), &rc ) ;
   c4getFuncPtr( hInst, "c4setErrGo", (void **)&(c4setErrGo), &rc ) ;
   c4getFuncPtr( hInst, "c4setErrOff", (void **)&(c4setErrOff), &rc ) ;
   c4getFuncPtr( hInst, "c4setErrOpen", (void **)&(c4setErrOpen), &rc ) ;
   c4getFuncPtr( hInst, "c4setErrorCode", (void **)&(c4setErrorCode), &rc ) ;
   c4getFuncPtr( hInst, "c4setErrRelate", (void **)&(c4setErrRelate), &rc ) ;
   c4getFuncPtr( hInst, "c4setErrSkip", (void **)&(c4setErrSkip), &rc ) ;
   c4getFuncPtr( hInst, "c4setErrTagName", (void **)&(c4setErrTagName), &rc ) ;
   c4getFuncPtr( hInst, "c4setFileFlush", (void **)&(c4setFileFlush), &rc ) ;
   c4getFuncPtr( hInst, "c4setLockAttempts", (void **)&(c4setLockAttempts), &rc ) ;
   c4getFuncPtr( hInst, "c4setLockAttemptsSingle", (void **)&(c4setLockAttemptsSingle), &rc ) ;
   c4getFuncPtr( hInst, "c4setLockDelay", (void **)&(c4setLockDelay), &rc ) ;
   c4getFuncPtr( hInst, "c4setLockEnforce", (void **)&(c4setLockEnforce), &rc ) ;
   c4getFuncPtr( hInst, "c4setLog", (void **)&(c4setLog), &rc ) ;
   c4getFuncPtr( hInst, "c4setOptimize", (void **)&(c4setOptimize), &rc ) ;
   c4getFuncPtr( hInst, "c4setOptimizeWrite", (void **)&(c4setOptimizeWrite), &rc ) ;
   c4getFuncPtr( hInst, "c4setReadLockDo", (void **)&(c4setReadLockDo), &rc ) ;
   c4getFuncPtr( hInst, "c4setReadOnlyDo", (void **)&(c4setReadOnlyDo), &rc ) ;
   c4getFuncPtr( hInst, "c4setSafety", (void **)&(c4setSafety), &rc ) ;
   c4getFuncPtr( hInst, "c4setSingleOpen", (void **)&(c4setSingleOpen), &rc ) ;

   c4getFuncPtr( hInst, "c4atod", (void **)&(c4atod), &rc ) ;
   c4getFuncPtr( hInst, "c4atoi", (void **)&(c4atoi), &rc ) ;
   c4getFuncPtr( hInst, "c4atol", (void **)&(c4atol), &rc ) ;
   c4getFuncPtr( hInst, "c4dtoa45", (void **)&(c4dtoa45), &rc ) ;
   c4getFuncPtr( hInst, "c4encode", (void **)&(c4encode), &rc ) ;
   c4getFuncPtr( hInst, "c4ltoa45", (void **)&(c4ltoa45), &rc ) ;
   c4getFuncPtr( hInst, "c4trimN", (void **)&(c4trimN), &rc ) ;
   c4getFuncPtr( hInst, "c4upper", (void **)&(c4upper), &rc ) ;

   c4getFuncPtr( hInst, "date4assignLow", (void **)&(date4assignLow), &rc ) ;
   c4getFuncPtr( hInst, "date4cdow", (void **)&(date4cdow), &rc ) ;
   c4getFuncPtr( hInst, "date4cmonth", (void **)&(date4cmonth), &rc ) ;
   c4getFuncPtr( hInst, "date4dow", (void **)&(date4dow), &rc ) ;
   c4getFuncPtr( hInst, "date4format", (void **)&(date4format), &rc ) ;
   c4getFuncPtr( hInst, "date4formatMdx", (void **)&(date4formatMdx), &rc ) ;
   c4getFuncPtr( hInst, "date4init", (void **)&(date4init), &rc ) ;
   c4getFuncPtr( hInst, "date4isLeap", (void **)&(date4isLeap), &rc ) ;
   c4getFuncPtr( hInst, "date4long", (void **)&(date4long), &rc ) ;
   c4getFuncPtr( hInst, "date4today", (void **)&(date4today), &rc ) ;
   c4getFuncPtr( hInst, "date4timeNow", (void **)&(date4timeNow), &rc ) ;

   c4getFuncPtr( hInst, "error4callback", (void **)&(error4callback), &rc ) ;
   c4getFuncPtr( hInst, "error4default", (void **)&(error4default), &rc ) ;
   c4getFuncPtr( hInst, "error4describeDefault", (void **)&(error4describeDefault), &rc ) ;
   c4getFuncPtr( hInst, "error4describeVB", (void **)&(error4describeVB), &rc ) ;
   c4getFuncPtr( hInst, "error4exitTest", (void **)&(error4exitTest), &rc ) ;
   c4getFuncPtr( hInst, "error4file", (void **)&(error4file), &rc ) ;
   c4getFuncPtr( hInst, "error4lastDescription", (void **)&(error4lastDescription), &rc ) ;
   c4getFuncPtr( hInst, "error4set", (void **)&(error4set), &rc ) ;
   c4getFuncPtr( hInst, "error4text", (void **)&(error4text), &rc ) ;

   c4getFuncPtr( hInst, "file4alloc", (void **)&(file4alloc), &rc ) ;

   c4getFuncPtr( hInst, "l4addAfter", (void **)&(l4addAfter), &rc ) ;
   c4getFuncPtr( hInst, "l4addBefore", (void **)&(l4addBefore), &rc ) ;
   c4getFuncPtr( hInst, "l4prev", (void **)&(l4prev), &rc ) ;
   c4getFuncPtr( hInst, "l4remove", (void **)&(l4remove), &rc ) ;

   c4getFuncPtr( hInst, "mem4allocDefault", (void **)&(mem4allocDefault), &rc ) ;
   c4getFuncPtr( hInst, "mem4createDefault", (void **)&(mem4createDefault), &rc ) ;
   c4getFuncPtr( hInst, "mem4freeDefault", (void **)&(mem4freeDefault), &rc ) ;
   c4getFuncPtr( hInst, "mem4release", (void **)&(mem4release), &rc ) ;

   c4getFuncPtr( hInst, "sort4alloc", (void **)&(sort4alloc), &rc ) ;
   c4getFuncPtr( hInst, "sort4free2", (void **)&(sort4free2), &rc ) ;
   c4getFuncPtr( hInst, "sort4init", (void **)&(sort4init), &rc ) ;

   c4getFuncPtr( hInst, "u4allocDefault", (void **)&(u4allocDefault), &rc ) ;
   c4getFuncPtr( hInst, "u4allocAgainDefault", (void **)&(u4allocAgainDefault), &rc ) ;
   c4getFuncPtr( hInst, "u4allocErDefault", (void **)&(u4allocErDefault), &rc ) ;
   c4getFuncPtr( hInst, "u4allocFreeDefault", (void **)&(u4allocFreeDefault), &rc ) ;
   c4getFuncPtr( hInst, "u4freeDefault", (void **)&(u4freeDefault), &rc ) ;
   c4getFuncPtr( hInst, "u4memCpy", (void **)&(u4memCpy), &rc ) ;
   c4getFuncPtr( hInst, "u4nameChar", (void **)&(u4nameChar), &rc ) ;
   c4getFuncPtr( hInst, "u4nameExt", (void **)&(u4nameExt), &rc ) ;
   c4getFuncPtr( hInst, "u4namePiece", (void **)&(u4namePiece), &rc ) ;
   c4getFuncPtr( hInst, "u4ncpy", (void **)&(u4ncpy), &rc ) ;
   c4getFuncPtr( hInst, "u4remove", (void **)&(u4remove), &rc ) ;
   c4getFuncPtr( hInst, "u4yymmdd", (void **)&(u4yymmdd), &rc ) ;

   c4getFuncPtr( hInst, "v4Cstring", (void **)&(v4Cstring), &rc ) ;
   c4getFuncPtr( hInst, "v4Cstringfree", (void **)&(v4Cstringfree), &rc ) ;

   l4init( &d4list ) ;
   hD4List = CreateEvent( 0, 1, 1, "hD4List" ) ;

   if ( rc == -1 )
   {
      if ( c4 != 0 && code4initUndo != 0 )
         code4initUndo( c4 ) ;
      FreeLibrary( hInst ) ;
      return e4result ;
   }

   if ( !hG_c4 )
      hG_c4 = CreateEvent( 0, 1, 1, "hG_c4" ) ;
   ResetEvent( hG_c4 ) ;
   if ( g_c4 == 0 )
   {
      g_c4 = this ;
   }
   SetEvent( hG_c4 ) ;

   return r4success ;
}

S4INLINE2 int Code4::initUndoDll()
{
   int rc = 0 ;
   WaitForSingleObject( hD4List, INFINITE ) ;
   ResetEvent( hD4List ) ;
   closeAllList = 1 ;
   for ( DATA4* d4 = (DATA4 *)l4first( &d4list ); d4 != 0 ; d4 = (DATA4 *)l4first( &d4list ) )
      d4close( d4 ) ;   /* use wrapper version of d4close() to free memory from d4***() */
   closeAllList = 0 ;
   SetEvent( hD4List ) ;

   WaitForSingleObject( hG_c4, INFINITE ) ;
   ResetEvent( hG_c4 ) ;
   if ( this != g_c4 )
   {
      rc = code4initUndo( c4 ) ;
      if ( rc != 0 )
         return rc ;
      FreeLibrary( hInst ) ; /* LY 2002/02/15 : missing FreeLibrary */
      CloseHandle( hD4List ) ;
      SetEvent( hG_c4 ) ;
   }
   else
   {
      rc = g_c4->code4initUndo( g_c4->c4 ) ;
      if ( rc != 0 )
      {
         SetEvent( hG_c4 ) ;
         return rc ;
      }
      FreeLibrary( g_c4->hInst ) ;   /* LY 2002/02/15 : missing FreeLibrary */
      CloseHandle( g_c4->hD4List ) ;
      /* LY 2002/05/29 : if two processes running, not signalling hG_c4 before
         closing will block other process */
      SetEvent( hG_c4 ) ;
      CloseHandle( hG_c4 ) ;  /* LY 2002/02/20 : not closing hG_c4 */
      hG_c4 = NULL ;  // CS 2012/09/14
      g_c4 = 0 ;
   }

   return rc ;
}
#endif   /* D4DLL_CPP */

#undef s4memset

#ifdef __TURBOC__
  #pragma warn .inl
#endif

#if defined(_MSC_VER) && !defined(S4WINCE) && !defined(S4WIN64)
   #if _MSC_VER >= 900
      #pragma pack(pop)
   #else
      #pragma pack()
   #endif
#else
   #ifdef __BORLANDC__
      #pragma pack()
   #endif
#endif

#ifdef __BORLANDC__
   #pragma warn .hid
#endif

#endif /* __DATA4HEADER */

