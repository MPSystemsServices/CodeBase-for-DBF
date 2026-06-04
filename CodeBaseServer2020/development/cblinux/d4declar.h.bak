/* d4declar.h  (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

/* AS 09/22/98 for MicroSoft compilers, at least, make functions intrinsic... */
#ifdef _MSC_VER
   #ifndef _FAST
      #ifdef S4WINCE
         #pragma intrinsic( memcpy, strcpy )
      #else
         #pragma intrinsic( memcmp, memset, memcpy, strcmp, strlen, strcpy )
      #endif
   #endif
   // AS VC 7.x compiler warning...
   #pragma warning( disable : 4290 )
#endif
#ifdef __BORLANDC__
   #pragma warn -ill
   #pragma intrinsic memcmp
   #pragma intrinsic memset
   #pragma intrinsic memcpy
   #pragma intrinsic strcmp
   #pragma intrinsic strlen
   #pragma intrinsic strcpy
   #pragma warn .ill
#endif

#ifdef S4PALM
//   #define c4atof FplAToF
   #define p4atol StrAToI

   void* c4memcpy(void* dstP, const void* srcP, Int16 n);
   #define c4memset(ptr,val,len) MemSet(ptr,len,val)

   #define c4sprintf StrPrintF
   #define c4strcat StrCat
   #define c4strchr StrChr
   #define c4strcmp StrCompare
   #define c4strcpy StrCopy
   #define c4stricmp StrCaselessCompare
   #define c4strlen StrLen
   #define c4strncat StrNCat
   #define c4strncmp StrNCompare
   #define c4strncpy StrNCopy
   #define c4strnicmp StrNCaselessCompare

   #define c4toupper TxtGlueUpperChar
#else
   #ifdef S4NO_ATOF
      #ifdef __cplusplus
         extern "C" {
      #endif
      S4EXPORT double S4FUNCTION c4atof(const char *) ;
      #ifdef __cplusplus
         }
      #endif
   #else
      #define c4atof atof
   #endif
   #define p4atol atol

   #define c4memcpy memcpy
   #define c4memset memset

   #define c4sprintf sprintf
   #define c4strcat strcat
   #define c4strchr strchr
   #define c4strcmp strcmp
   #define c4strncmp strncmp
   #define c4strcpy strcpy
   #define c4strlen strlen
   #define c4strncat strncat
   #define c4strncpy strncpy
   #define c4strnicmp strnicmp

   #define c4toupper toupper

   // BCR 08/18/00 - stricmp() is not supported under all operating systems. Use stricmp where it is available,
   // otherwise use c4stricmp() (defined in c4.c). On LINUX stricmp() is strcasecmp().
   #ifndef S4WINCE
       #ifdef S4UNIX  /* LY 2001/07/18 : changed from S4LINUX */
           #define c4stricmp( a, b ) strcasecmp( a, b )
       #else
           #define c4stricmp( a, b ) stricmp( a, b )
       #endif
   #endif
#endif

#ifdef S4WINDOWS  // CS 2000/12/05 moved from D4DEFS.H
   #ifdef __TURBOC__
      #if __TURBOC__ == 0x297         /* Borland C++ 2.0 */
         #define M4PRINT sprintf
      #else
         #define M4PRINT wsprintf
      #endif
   #else
      #define M4PRINT wsprintf
   #endif
#else
   #define M4PRINT c4sprintf            /* DOS */
#endif

#ifdef S4NO_MEMMOVE
   #ifdef __cplusplus
      extern "C" {
   #endif
   void *c4memmove(void *, const void *, size_t) ;
   #ifdef __cplusplus
      }
   #endif
#else
   #ifdef S4PALM
      #define c4memmove MemMove
   #else
      #define c4memmove memmove
   #endif
#endif

#ifdef S4MEMCMP
   #ifdef __cplusplus
      extern "C" {
   #endif
      int c4memcmp(const void *, const void *, size_t) ;
   #ifdef __cplusplus
   }
   #endif
#else
   #ifdef S4PALM
      #define c4memcmp MemCmp
   #else
      #define c4memcmp memcmp
   #endif
#endif

#ifdef __cplusplus
   extern "C" {
#endif

#if defined( S4WIN16 ) || defined( S4WIN32 )
   typedef short (CALLBACK* REINDEX_CALLBACK)(double);
#else
   typedef short ( * REINDEX_CALLBACK)(double);
#endif

#if defined(S4PALM) || ( defined( S4LANGUAGE ) && ( ( defined( S4GERMAN ) && !defined( S4FOX ) ) || ( !defined( S4GERMAN ) ) ) ) || defined( S4ANSI )
   int S4CALL u4memcmp( S4CMP_PARM, S4CMP_PARM, size_t ) ;
#else
   #define u4memcmp c4memcmp
#endif

/* Possibly Unsupported functions */

#ifdef S4NO_ECVT
   char *f4ecvt( double, int, int *, int * ) ;
   char *ecvt( double, int, int*, int* ) ;
#endif
#ifdef S4NO_FCVT
   char *fcvt( double, int, int*, int* ) ;
   char *f4fcvt( double, int, int *, int * ) ;
#endif
#ifdef S4NO_STRNICMP
   int strnicmp(char *, char *, size_t ) ;
#endif
#ifdef S4NO_WCSLEN
   /* LY 2001/07/13 : changed to c4wcslen for systems with 4 byte wchar_t */
   size_t c4wcslen( const WSTR5 * ) ;  /* CS 1999/10/07 */
#else
   #define c4wcslen( a ) wcslen( a )
#endif
#ifdef S4NO_TOUPPER
   char toupper(char) ;
#endif

#ifdef S4SERVER
   struct SERVER4CLIENTSt ;
#endif

/* EXTERNAL FUNCTIONS : */

/* CONVERSIONS */
#ifdef S4WIN32    /* LY 00/01/24: for 32-bit only */
char *c4longlongToA( char *out, LONGLONG val ) ;  // not exported...
#endif
//#ifdef S4PALM
//   S4EXPORT FloatType S4FUNCTION c4atod( const char S4PTR *, const int ) ;
//   S4EXPORT void S4FUNCTION c4atod2( char S4PTR *, int, FloatType S4PTR *);
//#else
   S4EXPORT double S4FUNCTION c4atod( const char S4PTR *, const int ) ;
   S4EXPORT void S4FUNCTION c4atod2( char S4PTR *, int, double S4PTR *);
//#endif
S4EXPORT int S4FUNCTION c4atoi( const char S4PTR *, const int ) ;
S4EXPORT long S4FUNCTION c4atol( const char S4PTR *, const int ) ;
S4EXPORT unsigned long S4FUNCTION c4atoul( const char S4PTR *, const int ) ;
S4EXPORT long S4FUNCTION date4yymmddToJulianLong( const int year, const int month, const int day ) ;
#ifdef S4WIN32
   S4EXPORT LONGLONG S4FUNCTION c4atoLongLong( const char *str, const int l ) ;
#endif
#if defined(S4WINCE) || defined(S4PALM)
   unsigned long c4strtoul( const char * ) ;
#endif
S4EXPORT void S4FUNCTION c4utoa( unsigned short *from) ; /* LY 2003/05/27 : moved out of #ifdef S4UNICODE for f4strUnicode() */
#ifdef S4UNICODE  // CS 2001/06/22
   S4EXPORT void S4FUNCTION c4atou(const char *, unsigned short *,  int ) ;
   S4EXPORT int S4FUNCTION c4strspn( const char *, const char * ) ;  /* LY 00/06/30 */
   __int64 c4atoi64( const char * ) ;
#endif
#if defined(S4WINCE) || defined( S4MACINTOSH )
   S4EXPORT int S4FUNCTION c4stricmp(const char *, const char *) ;
#endif
S4EXPORT void S4FUNCTION c4encode( char S4PTR *, const char S4PTR *, char S4PTR *, const char S4PTR * ) ;
S4EXPORT void S4FUNCTION c4trimN( char S4PTR *, int ) ;
S4EXPORT long S4FUNCTION c4ytoj( const int y ) ;
S4EXPORT int S4FUNCTION c4monDy( const int year, const int days,  int *monthPtr,  int *dayPtr ) ;

/* CODE4 */
// AS Sept 4/02 - new function to allow client side calling of a user-side dll on server
// AS Sept. 26/02 - log connection info
void code4logInfoDo( CODE4 *c4, char *info ) ;
#ifdef E4FILE_LINE
   #define code4logInfo( c4, info ) ( code4fileNameSet( __FILE__ ), code4lineNoSet( __LINE__ ), code4logInfoDo( (c4), (info) ) )
#else
   #define code4logInfo( c4, info ) ( code4logInfoDo( (c4), (info) ) )
#endif
#ifdef S4CLIENT
   S4EXPORT int S4FUNCTION code4logConn( CODE4 *c4, short level, const char *name ) ;
#endif
S4EXPORT int S4FUNCTION code4additionalFunction( CODE4 *c4, long functionNumber, void *infoIn, long infoLenIn, void *infoOut, long *infoLenOut ) ;
S4EXPORT CODE4 S4PTR *S4FUNCTION code4allocLow( int, const char S4PTR *, long ) ;
S4EXPORT CODE4 S4PTR *S4FUNCTION code4initAllocLow( const char S4PTR * ) ;
S4EXPORT int S4FUNCTION code4compressConfigure( CODE4 *c4, short compressLevel, short minLength ) ;
#ifdef S4PREPROCESS_FILE
   #if defined( S4ENCRYPT_DLL ) || defined( S4DLL_BUILD_ENCRYPT )
      short code4encryptFileGet( CODE4 *c4 ) ;
      #define code4getPreprocessFile( c4 ) ( code4encryptFileGet( c4 ) )
      #define code4getPreprocessBlockSize( c4 ) ( (c4)->encrypt->blockSize )
      #define code4getPreprocessInit( c4 ) ( (c4)->encrypt->preprocessInit )
      short code4encryptFileHook( CODE4 *c4, struct FILE4St *file, void *encryptInit  ) ;
      void code4fileDecrypt( FILE4 *f4, long pos, const void *ptrIn, long ptrInLen, void *ptrOut ) ;
      void code4fileEncrypt( FILE4 *f4, long pos, const void *ptrIn, long ptrInLen, void *ptrOut ) ;
      void *code4filePreprocess( FILE4 *f4, FILE4LONG pos, const void *ptr, unsigned long len ) ;
      void code4filePreprocessDone( FILE4 *f4 ) ;
      int code4serverPreprocessInit( CODE4 *c4, const char *userId, const char *password ) ;
   #else
      #define code4serverPreprocessInit( c4, userId, pwd ) code4serverEncryptInit( (c4), (userId), (pwd) )
      #define code4getPreprocessInit( c4 ) ( preprocess4getInit( &((c4)->preprocess) )
      #define code4getPreprocessBlockSize( c4 ) ( preprocess4getBlockSize( &((c4)->preprocess) ) )
      #define code4encryptFileHook( c4, file, encryptInit  ) ( preprocess4fileHook( &((c4)->preprocess), file, preprocess4getInit( &((c4)->preprocess) ) ) )
      #define code4fileDecrypt( f4, pos, ptrIn, ptrInLen, ptrOut ) file4postProcess( &(f4)->codeBase->preprocess, preprocess4getInit( &((f4)->codeBase->preprocess) ), (f4), file4longGetLo( pos ), (ptr), (urc), (ptr) )
      #define code4filePreprocess( f4, pos, ptr, len ) file4preprocess( (f4), (pos), (ptr), (len) )
      #define code4filePreprocessDone( f4 ) file4preprocessDone( f4 )
   #endif
   int code4setPreprocessFile( CODE4 *, short ) ;
   // void *file4preprocess( FILE4 *f4, FILE4LONG pos, const void *ptr, const unsigned long len ) ;
   // void file4preprocessDone( FILE4 *f4 ) ;
#else
   // #define code4getPreprocessFile( c4 )
   // #define code4setPreprocessFile( c4, val )
#endif
#ifdef S4PREPROCESS_COM
   #if defined( S4ENCRYPT_DLL )
      void con4lowPreprocessInitUndo( CODE4 *c4, CONNECT4LOW *conLow ) ;
      int con4lowPreProcess( CODE4 *c4, void *preprocess, unsigned char *message, int len ) ;
      int con4lowPostProcess( CODE4 *c4, void *preprocess, unsigned char *message, int len ) ;
      short con4lowGetEncryptRead( CODE4 *c4, CONNECT4LOW *conLow ) ;
      short con4lowGetEncryptWrite( CODE4 *c4, CONNECT4LOW *conLow ) ;
      // #define preprocess4getRead( c4, conLow ) con4lowGetEncryptRead( c4, conLow )
      // #define connect4lowPostProcess( c4, prep, mess, len ) con4lowPostProcess( (c4), (prep), (mess), (len) )
   #endif
#endif
#ifndef S4SERVER
   S4EXPORT int S4FUNCTION code4compressConfigureServer( CODE4 *c4, short compressLevel, short minLength ) ;
   S4EXPORT int S4FUNCTION code4compress( CODE4 *c4, short flag ) ;
#endif
S4EXPORT DATA4 S4PTR *S4FUNCTION code4data( CODE4 S4PTR *, const char S4PTR * ) ;
S4EXPORT const char S4PTR * S4FUNCTION code4indexExtension( CODE4 S4PTR * ) ;
S4EXPORT const char S4PTR * S4FUNCTION code4memoExtension( CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION code4initLow( CODE4 S4PTR *, const char S4PTR *, long, long, char ) ;
S4EXPORT CODE4 S4PTR* S4FUNCTION code4initP( void ) ;
S4EXPORT CODE4 S4PTR* S4FUNCTION code4initVB( void ) ;
S4EXPORT int S4FUNCTION code4initUndo( CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION code4lock( CODE4 S4PTR * ) ;
S4EXPORT void S4FUNCTION code4largeOn( CODE4 S4PTR * ) ;
S4EXPORT void S4FUNCTION code4autoIncrementStart( CODE4 S4PTR *, double ) ;
// AS Oct 24/03 - Support for file_extended with MDX
#if defined(S4FILE_EXTENDED) && !defined(S4CLIPPER)
   void code4largeOnLow( CODE4 S4PTR * ) ;
#endif
S4EXPORT void S4FUNCTION tran4freeLocks( CODE4 S4PTR *, SINGLE4DISTANT S4PTR *t ) ;
S4EXPORT void S4FUNCTION code4lockClear( CODE4 S4PTR * ) ;
S4EXPORT const char S4PTR *S4FUNCTION code4lockFileName( CODE4 S4PTR * ) ;
S4EXPORT long S4FUNCTION code4lockItem( CODE4 S4PTR * ) ;
S4EXPORT const char S4PTR *S4FUNCTION code4lockNetworkId( CODE4 S4PTR * ) ;
S4EXPORT const char S4PTR *S4FUNCTION code4lockUserId( CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION code4optStart( CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION code4optSuspend( CODE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION code4unlock( CODE4 S4PTR * ) ;
#ifdef S4CB51
   S4EXPORT int S4FUNCTION code4freeBlocks( CODE4 S4PTR * ) ;
#endif
S4EXPORT int S4FUNCTION code4swapLogFile( CODE4 *, char *, int ) ;
#ifdef S4CLIENT
   #define code4alloc( protocol ) ( code4allocLow( (protocol), DEF4PROTOCOL, S4VERSION ) )
   #define code4init( c4 ) ( code4initLow( (c4), DEF4PROTOCOL, S4VERSION, sizeof( CODE4 ), 0 ) )
   #define code4initAlloc() ( code4initAllocLow( DEF4PROTOCOL ) )
   S4EXPORT int S4FUNCTION code4serverRestart( CODE4 S4PTR * ) ;  /* for testing only */
   S4EXPORT int S4FUNCTION code4serverCrash( CODE4 S4PTR * ) ;  /* for testing only */
   S4EXPORT char S4PTR *S4FUNCTION code4tables( CODE4 S4PTR *, const char S4PTR * ) ;
   S4EXPORT char S4PTR *S4FUNCTION code4serverCurrentDirectory( CODE4 S4PTR * ) ;
   S4EXPORT DATA4 S4PTR *S4FUNCTION code4directory( CODE4 S4PTR *, char S4PTR * ) ;
   S4EXPORT int S4FUNCTION code4serverShutdown( CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION code4connectAcceptNew( CODE4 S4PTR *, short ) ;
   S4EXPORT int S4FUNCTION code4connectCutAll( CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION code4connectCut( CODE4 S4PTR *, const char * ) ;
   S4EXPORT int S4FUNCTION code4serverCloseFiles( CODE4 S4PTR * ) ;
   S4EXPORT PIPE4RECV S4PTR * S4FUNCTION pipe4recvOpen( CODE4 S4PTR *, const char S4PTR * ) ;
   S4EXPORT PIPE4SEND * S4FUNCTION pipe4sendOpen( CODE4 S4PTR *, const char S4PTR * ) ;
   S4EXPORT void S4FUNCTION pipe4recvClose( PIPE4RECV S4PTR * ) ;
   S4EXPORT void S4FUNCTION pipe4sendClose( PIPE4SEND S4PTR * ) ;
   S4EXPORT short S4FUNCTION pipe4sendMessageN( PIPE4SEND S4PTR *, void S4PTR *, unsigned long ) ;
   S4EXPORT short S4FUNCTION pipe4sendMessage( PIPE4SEND S4PTR *, char S4PTR *) ;
   S4EXPORT short S4FUNCTION pipe4recvMessage( PIPE4RECV S4PTR *, unsigned long S4PTR *, void S4PTR * S4PTR * ) ;
#else
   #define code4alloc( protocol ) ( code4allocLow( (protocol), 0, S4VERSION ) )
   #define code4init( c4 ) ( code4initLow( (c4), 0, S4VERSION, sizeof( CODE4 ), 0 ) )
   #define code4initAlloc() ( code4initAlloclow( 0 ) )
#endif
#ifdef S4UTIL
   S4EXPORT int S4FUNCTION code4passwordSet( CODE4 S4PTR *, const char S4PTR *, const char S4PTR *, const char S4PTR * ) ;  /* for utility only */
#endif
#ifdef S4SERVER
   S4EXPORT int S4FUNCTION code4unlockAuto( CODE4 * ) ;
#else
   S4EXPORT int S4FUNCTION code4close( CODE4 S4PTR * ) ;
   S4EXPORT long S4FUNCTION code4connectionId( CODE4 * ) ;  // CS 2001/05/17
   S4EXPORT short S4FUNCTION code4isConnected( CODE4 *c4 ) ;
   S4EXPORT int S4FUNCTION code4connect( CODE4 S4PTR *, const char S4PTR *, const char S4PTR *, const char S4PTR *, const char S4PTR *, const char S4PTR * ) ;
   S4EXPORT const char S4PTR *S4FUNCTION code4dateFormat( CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION code4dateFormatSet( CODE4 S4PTR *, const char S4PTR * ) ;
   #ifndef S4PALM
      S4EXPORT void S4FUNCTION code4exit( CODE4 S4PTR * ) ;
   #endif
   S4EXPORT int S4FUNCTION code4flush( CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION code4info( CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION code4infoRetrieve( CODE4 S4PTR *, S4LONG S4PTR *, unsigned short S4PTR *, S4LONG S4PTR *, int S4PTR * ) ;
   S4EXPORT char *S4FUNCTION code4serverConfigName( CODE4 S4PTR * ) ;
   S4EXPORT const char S4PTR * S4FUNCTION code4serverName( CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION code4optAll( CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION code4logCreate( CODE4 S4PTR *, const char S4PTR *, const char S4PTR * ) ;
   S4EXPORT const char S4PTR *S4FUNCTION code4logFileName( CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION code4logOpen( CODE4 S4PTR *, const char S4PTR *, const char S4PTR * ) ;
   S4EXPORT void S4FUNCTION code4logOpenOff( CODE4 S4PTR * ) ;
   #ifdef S4INLINE
      #ifdef S4OFF_MULTI
         #define code4unlockAuto( c4 ) ( 0 )
      #else
         #define code4unlockAuto( c4 ) ( (c4)->c4trans.trans.unlockAuto )
      #endif
   #else
      int S4FUNCTION code4unlockAuto( CODE4 * ) ;
   #endif
   S4EXPORT short S4FUNCTION code4unlockAutoCB( CODE4 * ) ;
   S4EXPORT void S4FUNCTION code4unlockAutoSetCB( CODE4 *, short ) ;
#endif
#ifndef S4STAND_ALONE
   S4EXPORT DATA4 S4PTR *S4FUNCTION code4connectionStatus( CODE4 S4PTR * ) ;
#endif

S4EXPORT int S4FUNCTION currency4compare( const CURRENCY4 *, const CURRENCY4 * ) ;
/* LY 2001/07/28 : changed from long* to S4LONG* for 64-bit */
S4EXPORT int S4FUNCTION date4timeCompare( const S4LONG *, const S4LONG * ) ;
/* for testing */
S4EXPORT int S4FUNCTION currency4add( CURRENCY4 S4PTR *, const CURRENCY4 S4PTR *, const CURRENCY4 S4PTR * ) ;
S4EXPORT int S4FUNCTION currency4subtract( CURRENCY4 S4PTR *, const CURRENCY4 S4PTR *, const CURRENCY4 S4PTR * ) ;
S4EXPORT int S4FUNCTION currency4multiplyShort( CURRENCY4 S4PTR *, const CURRENCY4 S4PTR *, const short  ) ;
S4EXPORT int S4FUNCTION currency4divideShort( CURRENCY4 S4PTR *, const CURRENCY4 S4PTR *, const short ) ;
S4EXPORT int S4FUNCTION c4currencyToA( char S4PTR *, int, const CURRENCY4 S4PTR *, short, int * ) ;
S4EXPORT int S4FUNCTION c4atoCurrency( CURRENCY4 S4PTR *, const char S4PTR *, int ) ;
S4EXPORT void S4FUNCTION f4assignCurrency( FIELD4 S4PTR *, const char S4PTR * ) ;
S4EXPORT char S4PTR *S4FUNCTION f4currency( const FIELD4 S4PTR *, short ) ;
S4EXPORT char S4PTR *S4FUNCTION f4dateTime( const FIELD4 S4PTR * ) ;
S4EXPORT void S4FUNCTION f4assignDateTime( FIELD4 S4PTR *, const char S4PTR * ) ;

/* DATA4 */
S4EXPORT S4CONST char S4PTR * S4FUNCTION d4alias( S4CONST DATA4 S4PTR * ) ;
S4EXPORT void S4FUNCTION d4aliasSet( DATA4 S4PTR *, const char S4PTR * ) ;
void d4aliasFix( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4append( DATA4 S4PTR * ) ;
// AS 09/13/99 sped up OLE-DB with special append function...
S4EXPORT int S4FUNCTION d4appendOledb( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4appendBlank( DATA4 S4PTR * ) ;
S4EXPORT short S4FUNCTION d4appendStartLow( DATA4 S4PTR *, short ) ;
S4EXPORT short S4FUNCTION d4appendStart( DATA4 S4PTR *, short ) ;
// AS Dec 9/02 - centralized coding for auto-increment field
// void d4assignAutoIncrementField( DATA4 *data ) ;
S4EXPORT void S4FUNCTION d4blank( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4bof( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4bottom( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4check( DATA4 S4PTR * ) ;
#ifdef S4SERVER
   int d4closeTemp( DATA4 * ) ;
   #define d4lowAccessMode( d4 ) ( (d4)->dataFile->file.lowAccessMode )
#else
   S4EXPORT int S4FUNCTION d4lowAccessMode( DATA4 S4PTR * ) ;
#endif
#if defined( S4SERVER )
   #define d4codePage( d4 ) ( (d4)->dataFile->codePage )
#else
   S4EXPORT short S4FUNCTION d4codePage( DATA4 S4PTR * ) ;
#endif
S4EXPORT int S4FUNCTION d4close( DATA4 S4PTR * ) ;
S4EXPORT DATA4 S4PTR * S4FUNCTION d4create( CODE4 S4PTR *, const char S4PTR *, const FIELD4INFO S4PTR *, const TAG4INFO S4PTR * ) ;
S4EXPORT DATA4 S4PTR * S4FUNCTION d4createLow( CODE4 S4PTR *, const char S4PTR *, const FIELD4INFO S4PTR *, const TAG4INFO S4PTR * ) ;
S4EXPORT DATA4 S4PTR * S4FUNCTION d4createTemp( CODE4 S4PTR *, const FIELD4INFO S4PTR *, const TAG4INFO S4PTR * ) ;
S4EXPORT void S4FUNCTION d4delete( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4eof( DATA4 S4PTR * ) ;
// AS Oct 21/02 - added to support seek without data movement and record transfer
int d4seekServer( DATA4 *data, const void *searchData, const short len, const int fromCurrentPos, const short seekType, Bool5 doDataPosition ) ;
// AS Aug 30/02 - support for d4seekNext and multi-fetch skip
int d4skipFetchLow( DATA4 *data, short mode, const long nSkip, const char *seekKey, short keyLen, double dKey ) ;
int d4fetchFromBuffer( DATA4 *data, int newBufPos ) ;
S4EXPORT FIELD4 S4PTR * S4FUNCTION d4field( DATA4 S4PTR *, const char S4PTR * ) ;
S4EXPORT FIELD4 S4PTR * S4FUNCTION d4fieldNull( DATA4 S4PTR * ) ;
S4EXPORT FIELD4 S4PTR * S4FUNCTION d4fieldJ( DATA4 S4PTR *, short ) ;
S4EXPORT int S4FUNCTION d4fieldNumber( DATA4 S4PTR *, const char S4PTR * ) ;
S4EXPORT DATA4 S4PTR * S4FUNCTION d4fieldsAdd( DATA4 S4PTR *, short, FIELD4INFO S4PTR * ) ;
S4EXPORT DATA4 S4PTR * S4FUNCTION d4fieldsRemove( DATA4 S4PTR * S4PTR *, short, char S4PTR * S4PTR * ) ;
S4EXPORT const char S4PTR *S4FUNCTION d4fileName( DATA4 S4PTR * ) ;
S4EXPORT void S4FUNCTION d4replace( DATA4 *keep, DATA4 *newFiles, int doIndexes ) ;
// AS Oct 23/02 - Need to use d4flush() in places in server where we want to also perform update which
// dfile4flush() doesn't do.  In paricular with table validation.
// #ifdef S4SERVER
//    #define d4flush( d4 ) ( dfile4flush( (d4)->dataFile ) )
// #else
   S4EXPORT int S4FUNCTION d4flush( DATA4 S4PTR * ) ;
   #ifndef S4OFF_WRITE
      int d4flushData( DATA4 * ) ;
   #endif
// #endif

S4EXPORT int S4FUNCTION d4freeBlocks( DATA4 S4PTR * ) ;

S4EXPORT int S4FUNCTION d4goLow( DATA4 S4PTR *, const long, short ) ;
S4EXPORT int S4FUNCTION d4goDirectRead( DATA4 S4PTR *, const long, char S4PTR *buffer, int fromDisk ) ;
S4EXPORT int S4FUNCTION d4goLow2( DATA4 S4PTR *, const long, short, Bool5 ) ;
#define d4go( d4, recNo ) ( d4goLow( (d4), (recNo), 1 ) )
S4EXPORT int S4FUNCTION d4goBof( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4goEof( DATA4 S4PTR * ) ;
S4EXPORT INDEX4 S4PTR * S4FUNCTION d4index( DATA4 S4PTR *, const char S4PTR * ) ;
S4EXPORT INDEX4 S4PTR * S4FUNCTION t4index( TAG4 S4PTR * ) ;
#ifndef S4SERVER
   // AS Jul 01/02 - Function to perform seek with optional data position
   S4EXPORT int S4FUNCTION t4seekN( TAG4 S4PTR *, const char S4PTR *, const short inputKeyLen, short ) ;
#endif
// AS Oct 22/02 - support for t4seekN in client/server
int t4seekNdo( DATA4 *data, TAG4 *tag, const char *seekValue, const short inputKeyLen, short doDataPosition ) ;
short d4seekNLow( DATA4 *data, const char *inputKey, const short inputKeyLen, short doDataPosition ) ;

S4EXPORT int S4FUNCTION d4lockAllVB( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4lockAppendVB( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4lockFileVB( DATA4 * ) ;
S4EXPORT int S4FUNCTION d4lockVB( DATA4 *, const long ) ;
#ifndef S4COMP_OFF_MULTI
   S4EXPORT int S4FUNCTION d4lockAdd( DATA4 S4PTR *, long ) ;
   S4EXPORT int S4FUNCTION d4lockAddAll( DATA4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION d4lockAddAppend( DATA4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION d4lockAddFile( DATA4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION d4lockAll( DATA4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION d4lockAllInternal( DATA4 S4PTR *, Bool5 ) ;
   S4EXPORT int S4FUNCTION d4lockAppendRecord( DATA4 S4PTR *, int ) ;
   S4EXPORT int S4FUNCTION d4lockAppend( DATA4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION d4lockAppendInternal( DATA4 S4PTR *, Bool5 ) ;
   S4EXPORT int S4FUNCTION d4lockFile( DATA4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION d4lock( DATA4 S4PTR *, const long ) ;
   #ifdef __cplusplus
      S4EXPORT int S4FUNCTION d4lockFileInternal( DATA4 *, Bool5, Lock4type lockType = lock4write ) ;
      S4EXPORT int S4FUNCTION d4lockInternal( DATA4 *, const long, Bool5, Lock4type lockType = lock4write ) ;
      S4EXPORT int S4FUNCTION d4lockTest( DATA4 S4PTR *, const long, Lock4type lockType = lock4write ) ;
   #else
      S4EXPORT int S4FUNCTION d4lockFileInternal( DATA4 *, Bool5, enum Lock4type lockType ) ;
      S4EXPORT int S4FUNCTION d4lockInternal( DATA4 *, const long, Bool5, enum Lock4type lockType ) ;
      S4EXPORT int S4FUNCTION d4lockTest( DATA4 S4PTR *, const long, enum Lock4type lockType ) ;
   #endif

   #ifndef S4INTERNAL_COMPILE_CHECK
/*      #define d4lockFile( d4 ) d4lockFileInternal( (d4), 1 ) */
/*      #define d4lockAppend( d4 ) d4lockAppendInternal( (d4), 1 ) */
/*      #define d4lockAll( d4 ) d4lockAllInternal( (d4), 1 ) */
/*      #define d4lock( d4, rec ) d4lockInternal( (d4), (rec), 1 ) */
   #endif

   S4EXPORT int S4FUNCTION d4lockTestAppend( DATA4 S4PTR * ) ;
#endif
#ifndef S4SERVER
   S4EXPORT int S4FUNCTION d4log( DATA4 S4PTR *, const int ) ;
#endif
#ifndef S4COMP_OFF_MEMO
   S4EXPORT int S4FUNCTION d4memoCompress( DATA4 S4PTR * ) ;
#endif
#ifdef S4SERVER
   #ifdef S4CLIENT_OR_FOX
      #define d4numFields( d4 ) ((((d4)->fields[(d4)->dataFile->nFields-1].type == '0') ? ((d4)->dataFile->nFields - 1) : ((d4)->dataFile->nFields)))
   #else
      #define d4numFields( d4 ) ((d4)->dataFile->nFields)
   #endif
#else
   S4EXPORT short int S4FUNCTION d4numFields( DATA4 S4PTR * ) ;
#endif
S4EXPORT DATA4 S4PTR *S4FUNCTION d4open( CODE4 S4PTR *, const char S4PTR * ) ;
#if !defined( S4OFF_WRITE ) && !defined( S4OFF_TRAN ) && !defined( S4CLIENT )
   void d4openConcludeSetupTransactions( DATA4 * ) ;
   int d4logClose( DATA4 *data ) ;
   int d4logCreate( CODE4 *c4, const char *name, const FIELD4INFO *fieldData, const TAG4INFO *tagInfo ) ;
#endif
void d4openConcludeSetupTransactions( DATA4 *d4 ) ;
S4EXPORT DATA4 S4PTR *S4FUNCTION d4openClone( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4optimizeWrite( DATA4 S4PTR *, const int ) ;
S4EXPORT int S4FUNCTION d4pack( DATA4 S4PTR * ) ;
S4EXPORT double S4FUNCTION d4position( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4position2( DATA4 S4PTR *, double S4PTR * ) ;
S4EXPORT long S4FUNCTION d4recCountDo2( DATA4 S4PTR *, Bool5 ) ;
S4EXPORT long S4FUNCTION d4recCountDo( DATA4 S4PTR * ) ;
#ifndef S4SERVER
   S4EXPORT long S4FUNCTION d4recNoLow( DATA4 S4PTR * ) ;
#endif
#if defined( S4DLL_BUILD ) || defined( S4SERVER ) || defined( S4JOINT_OLEDB_DLL )
   #define d4recNo( d4 )   ( (d4)->recNum )
   #define d4record( d4 )  ( (d4)->record )
#else
   #define d4recNo( d4 )  d4recNoLow( d4 )
   #define d4record( d4 ) d4recordLow( d4 )
#endif
// AS May 28/02 - don't use the exposed function with block preprocessing because we modify the record
// buffer to contain extra unused space to land on a record boundary
#ifndef S4INTERNAL_COMPILE_CHECK
   #define d4recWidth( d4 ) d4recWidthLow( d4 )
#endif
S4EXPORT char S4PTR *S4FUNCTION d4recordLow( DATA4 S4PTR * ) ;
#ifdef S4SERVER
   #define d4recNoSet( d4, val ) ( (d4)->recNum = (val) )
   #define d4recordSet( d4, rec ) ( (d4)->record = (rec) )
#else
   S4EXPORT void S4FUNCTION d4recNoSet( DATA4 S4PTR *, long recNo ) ;
   S4EXPORT void S4FUNCTION d4recordSet( DATA4 S4PTR *, char S4PTR *newRecPtr ) ;
#endif
/* -- made inline
   S4EXPORT char S4PTR *S4FUNCTION d4recordOld( DATA4 S4PTR * ) ;
*/
#define d4recordOld( d4 ) ((d4)->recordOld)
S4EXPORT unsigned long S4FUNCTION d4recWidthLow( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4refresh( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4skip( DATA4 S4PTR *, const long ) ;
S4EXPORT int S4FUNCTION d4top( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4unappend( DATA4 * ) ;  /* internal only */
S4EXPORT int S4FUNCTION d4writeLow( DATA4 S4PTR *, const long, const int, const int ) ;
S4EXPORT int S4FUNCTION d4zap( DATA4 S4PTR *, const long, const long ) ;
#ifdef S4OFF_MULTI
   // AS 06/09/00 was not compiling in S4OFF_MULTI
   #define d4recCountAssumeLocked( d4 ) ( dfile4recCount( ( d4->dataFile ), 0 ) )
   #define d4recCount( d4 ) ( dfile4recCount( ( d4->dataFile ), 0 ) )
#else
   #define d4recCountAssumeLocked( d4 ) ( d4recCountDo2( d4, ASSUME4LOCKED ) )
   #define d4recCount( d4 ) ( d4recCountDo2( d4, !ASSUME4LOCKED ) )
#endif
#ifdef S4STAND_ALONE
   // don't use this for odbc in stand-alone... - since we use the same library for odbc and regular codebase, base on a CODE4 member
   // this is for the following reason:  In stand-alone version of ODBC, there is no sharing of threads, but often the ODBC driver
   // will open and then close data files, then re-open them within the same transaction, and expect the updates to have occurred.
   // because of this, we want to assume that all active DATA4 objects are within the same transaction and can share state between
   // themselves.  This means that a given application will always see all changes it has performed.

   #define dfile4setMinCount( d4, val ) ( ((d4)->c4->minCountOff) ? 0 : ( (d4)->minCount = (val) ) )
   #define dfile4getMinCount( d4 ) ( ((d4)->c4->minCountOff) ? (d4)->numRecs : ( (d4)->minCount ) )
#else
   #define dfile4setMinCount( d4, val ) ( (d4)->minCount = (val) )
   #define dfile4getMinCount( d4 ) ( (d4)->minCount )
#endif
#ifndef S4COMP_OFF_INDEX
   S4EXPORT int S4FUNCTION d4reindex( DATA4 S4PTR * ) ;
   S4EXPORT short S4FUNCTION d4reindexWithProgress( DATA4 S4PTR *data, REINDEX_CALLBACK callback, long milliseconds ) ;
   S4EXPORT int S4FUNCTION d4seek( DATA4 S4PTR *, const char S4PTR * ) ;
   S4EXPORT int S4FUNCTION d4seekDouble( DATA4 S4PTR *, const double ) ;
   S4EXPORT short S4FUNCTION d4seekN( DATA4 S4PTR *, const char S4PTR *, const short ) ;
   S4EXPORT TAG4 S4PTR *S4FUNCTION d4tag( DATA4 S4PTR *, const char S4PTR * const ) ;
   S4EXPORT TAG4 S4PTR *S4FUNCTION d4tagDefault( DATA4 S4PTR * ) ;
   S4EXPORT TAG4 S4PTR *S4FUNCTION d4tagNext( DATA4 S4PTR *, TAG4 S4PTR * ) ;
   S4EXPORT TAG4 S4PTR *S4FUNCTION d4tagPrev( DATA4 S4PTR *, TAG4 S4PTR * ) ;
   S4EXPORT void S4FUNCTION d4tagSelect( DATA4 S4PTR *, TAG4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION d4tagSelectP( DATA4 S4PTR *, TAG4 S4PTR * ) ;
   S4EXPORT TAG4 S4PTR *S4FUNCTION d4tagSelected( DATA4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION d4tagSync( DATA4 S4PTR *, TAG4 S4PTR * const ) ;
   S4EXPORT int S4FUNCTION d4numTags(DATA4 S4PTR *);
#endif /* S4COMP_OFF_INDEX */
#ifndef S4COMP_OFF_MULTI
   #ifndef S4SINGLE
      S4EXPORT int S4FUNCTION d4unlockLow( DATA4 *, long, char ) ;
   #endif
   #ifndef S4SERVER
      S4EXPORT int S4FUNCTION d4unlock( DATA4 S4PTR * ) ;
   #endif
   S4EXPORT int S4FUNCTION d4unlockRecord( DATA4 *, long ) ;  /* exported for SQL */
   int dfile4unlockRecordDo( DATA4FILE *, long ) ;
#endif
#ifndef S4CB51
   S4EXPORT int S4FUNCTION d4seekNext( DATA4 S4PTR *, const char S4PTR * ) ;
   S4EXPORT int S4FUNCTION d4seekNextDouble( DATA4 S4PTR *, const double ) ;
   S4EXPORT short S4FUNCTION d4seekNextN( DATA4 S4PTR *, const char S4PTR *, const short ) ;
#endif  /* !S4CB51 */
#ifndef S4COMP_OFF_MULTI
   S4EXPORT int S4FUNCTION dfile4lockTestFile( DATA4FILE S4PTR *, const long, const long, enum Lock4type ) ;
   #ifndef S4CLIENT
      int dfile4lockTestFileInternal( DATA4FILE *, const long, const long, enum Lock4type ) ;
   #endif
#endif
#ifdef S4CLIENT
   #ifndef S4COMP_OFF_MULTI
      S4EXPORT int S4FUNCTION d4lockTestFile( DATA4 S4PTR * ) ;  /* testing only */
   #endif
#else
   #ifndef S4COMP_OFF_MULTI
      #define d4lockTestFile( d4 ) ( dfile4lockTestFile( (d4)->dataFile, data4clientId( d4 ), data4serverId( d4 ), lock4write ) )
      #ifndef S4OFF_MULTI
         #define d4lockIndex( d4 ) ( dfile4lockIndex( (d4)->dataFile, data4serverId( d4 ) ) )
         S4EXPORT int S4FUNCTION d4lockIndexExport( DATA4 * ) ;   // for ODBC
      #endif
   #endif
   S4EXPORT int S4FUNCTION dfile4remove( DATA4FILE S4PTR * ) ;
#endif /* !S4CLIENT */
#if defined( S4OLEDB_OR_NOT_SERVER ) || defined( S4ODBC_BUILD ) || defined( E4MISC )
   S4EXPORT int S4FUNCTION d4deleted( DATA4 S4PTR * ) ;
#endif
#ifdef E4MISC
   #define d4deletedInternal( d4 ) ( d4deleted( (d4) ) )
#else
   #define d4deletedInternal( d4 ) ( *(d4)->record != ' ' )
#endif
S4EXPORT FIELD4INFO S4PTR * S4FUNCTION d4fieldInfo( DATA4 S4PTR * ) ;
S4EXPORT int S4FUNCTION d4remove( DATA4 S4PTR * ) ;
#define d4write( a, b ) ( d4writeLow( a, b, 0, 1 ) )
#define d4changedVoid( d4, flag ) ( (d4)->recordChanged = (flag) )
S4EXPORT short S4FUNCTION d4changed( DATA4 S4PTR *, short ) ;
#ifdef S4SERVER
   S4EXPORT long S4FUNCTION d4positionSet( DATA4 S4PTR *, const double ) ;
   // similar to regular d4changed, but no return code - more efficient, used internally
#else
   S4EXPORT int S4FUNCTION d4optimize( DATA4 S4PTR *, const int ) ;
   S4EXPORT int S4FUNCTION d4positionSet( DATA4 S4PTR *, const double ) ;
   S4EXPORT void S4FUNCTION d4recall( DATA4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION d4refreshRecord( DATA4 S4PTR * ) ;
   #ifdef S4CB51
      S4EXPORT int S4FUNCTION d4lock_group( DATA4 S4PTR *, const long S4PTR *, const int ) ;
   #endif
#endif /* S4SERVER */
#ifndef S4OFF_MULTI
   S4EXPORT int S4FUNCTION dfile4lockTestAppend( DATA4FILE S4PTR *, const long, const long ) ;
//   S4EXPORT int S4FUNCTION dfile4unlockRecord( DATA4FILE *, const long, const long, const long ) ;
#endif /* S4OFF_MULTI */

S4EXPORT short S4FUNCTION d4logStatusCB( DATA4 * ) ;
#ifdef S4OFF_TRAN
   #define d4logStatus( d4 ) ( 0 )
#else
   #ifdef S4CLIENT
      #define d4logStatus( d4 ) ( 0 )
   #else
      #define d4logStatus( d4 ) ( (d4)->logVal == 0 ? 0 : 1 )
   #endif
#endif

/* DATE4 */
#define date4assign( a, b ) ( date4assignLow( (a), (b), 0 ) )
S4EXPORT int S4FUNCTION date4assignLow( char S4PTR *, const long, int ) ;
S4EXPORT S4CONST char *S4FUNCTION date4cdow( const char S4PTR * ) ;
S4EXPORT S4CONST char *S4FUNCTION date4cmonth( const char S4PTR * ) ;
S4EXPORT int S4FUNCTION date4dow( const char S4PTR * ) ;
S4EXPORT void S4FUNCTION date4format( const char S4PTR *, char S4PTR *, char S4PTR * ) ;/* 'dt' may be 'result'*/
S4EXPORT double S4FUNCTION date4formatMdx( const char S4PTR * ) ;
S4EXPORT int S4FUNCTION date4formatMdx2( const char S4PTR * , double S4PTR * ) ;
S4EXPORT void S4FUNCTION date4init( char S4PTR *, const char S4PTR *, char S4PTR * ) ;
S4EXPORT int S4FUNCTION date4isLeap( const char S4PTR * ) ;
S4EXPORT long S4FUNCTION date4long( const char S4PTR * ) ;
S4EXPORT void S4FUNCTION date4timeNow( char S4PTR * ) ;
S4EXPORT void S4FUNCTION date4today( char S4PTR * ) ;
#define date4day( datePtr )    ( (int)c4atol( (datePtr) + 6, 2 ) )
#define date4month( datePtr )  ( (int)c4atol( (datePtr) + 4, 2 ) )
#define date4year( yearPtr )   ( (int)c4atol( (yearPtr), 4 ) )

/* ERROR4 */
S4EXPORT void S4FUNCTION error4callback( CODE4 S4PTR *c4, ERROR_CALLBACK errorCallback ) ;  // CS 2001/03/26 add
S4EXPORT int S4FUNCTION error4default( CODE4 S4PTR *, const int, const long ) ;
S4EXPORT int S4FUNCTION error4describeDefault( CODE4 S4PTR *, const int, const long, const char S4PTR *, const char S4PTR *, const char S4PTR * ) ;
S4EXPORT int S4FUNCTION error4describeExecute( CODE4 S4PTR *, const int, const long, const char S4PTR *, const char S4PTR *, const char S4PTR * ) ;
S4EXPORT int S4FUNCTION error4file( CODE4 S4PTR *, S4CONST char S4PTR *, const int ) ;
S4EXPORT void S4FUNCTION error4hook( CODE4 S4PTR *, int, long, const char S4PTR *, const char S4PTR *, const char S4PTR * ) ;
S4EXPORT int S4FUNCTION error4set( CODE4 S4PTR *, const int ) ;
S4EXPORT long S4FUNCTION error4set2( CODE4 S4PTR *, const long ) ;
S4EXPORT const char *S4FUNCTION error4lastDescription(CODE4 *c4);
S4EXPORT const char S4PTR * S4FUNCTION error4text( CODE4 S4PTR *, const long ) ;
#ifdef S4CB51
   S4EXPORT S4CONST char S4PTR *S4FUNCTION e4text( const int ) ;
   S4EXPORT int S4FUNCTION e4describe( CODE4 S4PTR *, int, const char S4PTR *, const char S4PTR *, const char S4PTR * ) ;
   S4EXPORT void S4FUNCTION e4hook( CODE4 S4PTR *, int, const char S4PTR *, const char S4PTR *, const char S4PTR * ) ;
   S4EXPORT int S4FUNCTION e4log( CODE4 S4PTR *, const char S4PTR * ) ;
   #define e4exitTest( c4 ) ( error4exitTest( c4 ) )
   #ifndef S4SERVER
      S4EXPORT void S4FUNCTION e4severe( const int, const char S4PTR * ) ;
      #ifndef S4PALM
         S4EXPORT void S4FUNCTION error4exitTest( CODE4 S4PTR * ) ;
      #endif
      #ifdef E4VBASIC
         S4EXPORT void S4FUNCTION e4severe_vbasic( int, const char S4PTR * ) ;
      #endif
   #endif /* S4SERVER */
#endif /* S4CB51 */
#ifndef S4SERVER
   S4EXPORT void S4FUNCTION error4exitTest( CODE4 S4PTR * ) ;
#endif /* S4SERVER */
#ifdef E4STACK
   S4EXPORT int S4FUNCTION error4stackDefault( CODE4 S4PTR *, const int, const long ) ;
#endif
#ifdef S4COM_PRINT
   void S4FUNCTION debug4display( const char *str ) ;
   const char *s4connectionPrint(const int type ) ;
#endif

/* FIELD4 */
S4EXPORT void S4FUNCTION f4assign( FIELD4 S4PTR *, const char S4PTR * ) ;
S4EXPORT void S4FUNCTION f4assignChar( FIELD4 S4PTR *, const int ) ;
S4EXPORT void S4FUNCTION f4assignDouble( FIELD4 S4PTR *, const double ) ;
S4EXPORT void S4FUNCTION f4assignField( FIELD4 S4PTR *, const FIELD4 S4PTR * ) ;
S4EXPORT void S4FUNCTION f4assignInt( FIELD4 S4PTR *, const int ) ;
S4EXPORT void S4FUNCTION f4assignLong( FIELD4 S4PTR *, const long ) ;
#ifdef S4WIN32
   S4EXPORT void S4FUNCTION f4assignLongLong( FIELD4 S4PTR *, const LONGLONG ) ;
#endif
S4EXPORT void S4FUNCTION f4assignN( FIELD4 S4PTR *, const char S4PTR *, const unsigned int ) ;
S4EXPORT void S4FUNCTION f4assignNull( FIELD4 S4PTR * ) ;
S4EXPORT char S4PTR * S4FUNCTION f4assignPtr( FIELD4 S4PTR * ) ;
S4EXPORT void S4FUNCTION f4assignWideString( FIELD4 S4PTR *, const WSTR5 * ) ;
S4EXPORT void S4FUNCTION f4assignUnicode( FIELD4 S4PTR *, const WSTR5 * ) ;
S4EXPORT short S4FUNCTION f4memoAssignUnicode( FIELD4 S4PTR *, const WSTR5 * ) ;
S4EXPORT void S4FUNCTION f4blank( FIELD4 S4PTR * ) ;
S4EXPORT int S4FUNCTION f4char( const FIELD4 S4PTR * ) ;
S4EXPORT DATA4 S4PTR *S4FUNCTION f4data( const FIELD4 S4PTR * ) ;
S4EXPORT int S4FUNCTION f4decimals( const FIELD4 S4PTR * ) ;
S4EXPORT double S4FUNCTION f4double( const FIELD4 S4PTR * ) ;
S4EXPORT int S4FUNCTION f4double2( const FIELD4 S4PTR *, double S4PTR * ) ;
S4EXPORT int S4FUNCTION f4int( const FIELD4 S4PTR * ) ;
S4EXPORT unsigned long S4FUNCTION f4len( const FIELD4 S4PTR * ) ;
#define f4lenInternal( fld ) ( (fld)->len )
S4EXPORT long S4FUNCTION f4long( const FIELD4 S4PTR * ) ;
#ifndef S4COMP_OFF_MEMO
   S4EXPORT int S4FUNCTION f4memoAssign( FIELD4 S4PTR *, const char S4PTR * ) ;
   S4EXPORT int S4FUNCTION f4memoAssignN( FIELD4 S4PTR *, const char S4PTR *, const unsigned int ) ;
   S4EXPORT int S4FUNCTION f4memoFree( FIELD4 S4PTR * ) ;
   S4EXPORT unsigned long S4FUNCTION f4memoLen( FIELD4 S4PTR * ) ;
   S4EXPORT unsigned long S4FUNCTION f4memoNcpy( FIELD4 S4PTR *, char S4PTR *, const unsigned int ) ;
   S4EXPORT char S4PTR * S4FUNCTION f4memoPtr( FIELD4 S4PTR * ) ;
   S4EXPORT S4CONST char S4PTR * S4FUNCTION f4memoStr( FIELD4 S4PTR * ) ;
   S4EXPORT void S4FUNCTION f4memoAssignField( FIELD4 S4PTR *, FIELD4 S4PTR * ) ;
#endif
S4EXPORT int S4FUNCTION f4memoFile( FIELD4 S4PTR *field, S4CONST char S4PTR *fileNameTo ) ;
S4EXPORT int S4FUNCTION f4memoAssignFile( FIELD4 S4PTR *field, S4CONST char S4PTR *fileNameFrom ) ;
int quad5longDivideShort( void *result, const void *c1, const short c2 ) ;
void quad5longAdd( QUAD5LONG *result, const QUAD5LONG *c1, const QUAD5LONG *c2 ) ;
S4EXPORT S4CONST char S4PTR * S4FUNCTION f4name( S4CONST FIELD4 S4PTR * ) ;
S4EXPORT unsigned long S4FUNCTION f4ncpy( FIELD4 S4PTR *, char S4PTR *, const unsigned int ) ;
S4EXPORT int S4FUNCTION f4null( const FIELD4 S4PTR * ) ;
S4EXPORT int S4FUNCTION f4nullable( const FIELD4 S4PTR * ) ;   // AS Feb 1/02 - Need this exported for ODBC
S4EXPORT int S4FUNCTION f4number( const FIELD4 S4PTR * ) ;
#if defined( S4SERVER ) || defined( S4ODBC_BUILD )
   #define f4ptr( f4 )  ( (f4)->data->record + (f4)->offset )
#else
   S4EXPORT char S4PTR * S4FUNCTION f4ptr( const FIELD4 S4PTR * ) ;
#endif
S4EXPORT char S4PTR * S4FUNCTION f4str( FIELD4 S4PTR * ) ;
S4EXPORT int S4FUNCTION f4true( const FIELD4 S4PTR * ) ;
S4EXPORT int S4FUNCTION f4type( const FIELD4 S4PTR * ) ;
int f4typeInternal( const FIELD4 S4PTR * ) ;
#ifdef S4CLIENT_OR_FOX  /*Internal function */
   S4EXPORT void S4FUNCTION f4assignNotNull( FIELD4 *field ) ;
#endif

/* FILE4 */
S4EXPORT int    S4FUNCTION file4close( FILE4 S4PTR * ) ;
// AS May 24/02 - exposed file4open only to users now to allow file type considerations to play a larger role
// (in particular to decide whether or not to preprocess files - only preprocess data/index/memos)
#ifndef S4INTERNAL_COMPILE_CHECK
   S4EXPORT int    S4FUNCTION file4create( FILE4 S4PTR *, CODE4 S4PTR *, S4CONST char S4PTR *, const int ) ;
#endif
int file4createInternal( FILE4 *, CODE4 *, S4CONST char *, const int, char ) ;
S4EXPORT int    S4FUNCTION file4flush( FILE4 S4PTR * ) ;
S4EXPORT FILE4LONG S4FUNCTION file4lenLow( FILE4 S4PTR * ) ;
// AS Sep. 17/01 - Must use function to change temporary setting to update validation tables
void file4setTemporary( FILE4 *file, Bool5 flag, Bool5 isDataFile ) ;
#define file4getTemporary( file ) ((file)->isTemporary)
#if defined(S4WIN32) || defined(S4UNIX) || defined(S4PALM)
   S4EXPORT int S4FUNCTION file4exists( const char *fileName ) ;
#endif

#ifdef S4FILE_EXTENDED
   int file4lenSetLow( FILE4 S4PTR *, FILE4LONG ) ;
#else
   S4EXPORT int S4FUNCTION file4lenSetLow( FILE4 S4PTR *, FILE4LONG ) ;
#endif
#ifdef S4FILE_EXTENDED
   S4EXPORT int    S4FUNCTION file4lenSet( FILE4 *, long ) ;
#else
   #define file4lenSet( f4, len ) file4lenSetLow( f4, len )
#endif
#ifndef S4INTERNAL_COMPILE_CHECK
   #define file4lock( f4, a, b ) file4lockInternal( (f4), (unsigned long)a, 0, (unsigned long)b, 0 )
#endif
S4EXPORT int    S4FUNCTION file4lockInternal( FILE4 S4PTR *, unsigned long, long, unsigned long, long ) ;
#define file4name( f4 ) ( (f4)->name )

// AS May 24/02 - exposed file4open only to users now to allow file type considerations to play a larger role
// (in particular to decide whether or not to preprocess files - only preprocess data/index/memos)
#ifndef S4INTERNAL_COMPILE_CHECK
   S4EXPORT int    S4FUNCTION file4open( FILE4 S4PTR *, CODE4 S4PTR *, S4CONST char S4PTR *, const int ) ;
#endif
int file4openInternal( FILE4 *, CODE4 *, S4CONST char *, const int, char fileType ) ;
S4EXPORT int    S4FUNCTION file4openTest( FILE4 S4PTR * ) ;
S4EXPORT int    S4FUNCTION file4optimizeLow( FILE4 S4PTR *, const int, const int, const long, const void S4PTR * ) ;
#define file4optimize( f4, i1, i2 ) ( file4optimizeLow( (f4), (i1), (i2), 0, 0 ) )
S4EXPORT int    S4FUNCTION file4optimizeWrite( FILE4 S4PTR *, const int ) ;
int file4readAllInternal( FILE4 *, FILE4LONG, void *, unsigned ) ;
unsigned file4readInternal( FILE4 *, FILE4LONG, void *, unsigned ) ;
#ifndef S4INTERNAL_COMPILE_CHECK
   S4EXPORT unsigned int S4FUNCTION file4read( FILE4 S4PTR *, const long, void S4PTR *, const unsigned int ) ;
   S4EXPORT int    S4FUNCTION file4readAll( FILE4 S4PTR *, const long, void S4PTR *, const unsigned int ) ;
#endif
S4EXPORT int    S4FUNCTION file4readError( FILE4 S4PTR *, FILE4LONG, unsigned, const char S4PTR * ) ;
S4EXPORT int    S4FUNCTION file4refresh( FILE4 S4PTR * ) ;
S4EXPORT int    S4FUNCTION file4replace( FILE4 S4PTR *, FILE4 S4PTR * ) ;
#ifndef S4INTERNAL_COMPILE_CHECK
   #define file4unlock( f4, a, b ) file4unlockInternal( (f4), (unsigned long)(a), 0, (unsigned long)(b), 0 )
#endif
S4EXPORT int    S4FUNCTION file4unlockInternal( FILE4 S4PTR *, unsigned long, long, unsigned long, long ) ;
int file4writeInternal( FILE4 *, FILE4LONG, const void *, unsigned ) ;
#ifndef S4INTERNAL_COMPILE_CHECK
   S4EXPORT int    S4FUNCTION file4write( FILE4 S4PTR *, const long, const void S4PTR *, const unsigned int ) ;
#endif
#ifdef S4CB51
   S4EXPORT int    S4FUNCTION file4temp( FILE4 S4PTR *, CODE4 S4PTR *, char *, const int ) ;
#else
   S4EXPORT int    S4FUNCTION file4temp( FILE4 S4PTR *, CODE4 S4PTR *, char *, const int ) ;
   #ifdef E4LOCK_HOOK
      S4EXPORT int S4FUNCTION file4lockHook( CODE4 S4PTR *, const char S4PTR *, long, long, int ) ;
   #endif
#endif
/* LY 2001/09/19 : new functions for d4dll.c */
S4EXPORT int S4FUNCTION file4alloc( FILE4 S4PTR* S4PTR* ) ;
S4EXPORT unsigned long S4FUNCTION file4lenLow2( FILE4 S4PTR * ) ;
S4EXPORT int S4FUNCTION file4lenSetLow2( FILE4 S4PTR *, long ) ;
S4EXPORT const char * S4FUNCTION file4nameLow( FILE4 S4PTR * ) ;
S4EXPORT void S4FUNCTION file4free( FILE4 S4PTR * S4PTR * ) ;

/* FILE4SEQ_READ */
S4EXPORT int S4FUNCTION file4seqReadInitDo( FILE4SEQ_READ S4PTR *, FILE4 S4PTR *, FILE4LONG, void S4PTR *, const unsigned, const int ) ;
#ifndef S4INTERNAL_COMPILE_CHECK
   S4EXPORT int S4FUNCTION file4seqReadInit( FILE4SEQ_READ S4PTR *, FILE4 S4PTR *, long, void S4PTR *, const unsigned ) ;
#endif
#ifdef S4ADVANCE_READ
   void file4seqReadInitUndo( const FILE4SEQ_READ * ) ;
#endif
S4EXPORT unsigned int S4FUNCTION file4seqRead( FILE4SEQ_READ S4PTR *, void S4PTR *, unsigned ) ;
S4EXPORT int    S4FUNCTION file4seqReadAll( FILE4SEQ_READ S4PTR *, void S4PTR *, const unsigned int ) ;

/* FILE4SEQ_WRITE */
int file4seqWriteInitLow( FILE4SEQ_WRITE *seqWrite, FILE4 *file, FILE4LONG startOffset, void *ptr, const unsigned ptrLen ) ;
#ifndef S4INTERNAL_COMPILE_CHECK
   S4EXPORT int    S4FUNCTION file4seqWriteInit( FILE4SEQ_WRITE S4PTR *, FILE4 S4PTR *, const long, void S4PTR *, const unsigned int ) ;
#endif
S4EXPORT int    S4FUNCTION file4seqWrite( FILE4SEQ_WRITE S4PTR *, const void S4PTR *, const unsigned int ) ;
S4EXPORT int    S4FUNCTION file4seqWriteFlush( FILE4SEQ_WRITE S4PTR * ) ;
S4EXPORT int    S4FUNCTION file4seqWriteRepeat( FILE4SEQ_WRITE S4PTR *, const long, const char ) ;

#ifdef S4STAND_ALONE
   // AS Jan 15/03 - Support for generic file compression
   S4EXPORT int S4FUNCTION file4compressOpen( FILE4 S4PTR *, CODE4 S4PTR *, S4CONST char S4PTR * ) ;
   S4EXPORT int S4FUNCTION file4compressCreate( FILE4 S4PTR *, FILE4 S4PTR *, S4CONST char S4PTR *, short blockSize ) ;
#endif


/* INDEX4 */
#ifndef S4COMP_OFF_INDEX
   S4EXPORT int S4FUNCTION i4addEntryToGroupFile( INDEX4 *i4, const char *tagName ) ;
   S4EXPORT int S4FUNCTION i4close( INDEX4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION i4closeLow( INDEX4 S4PTR * ) ;
   S4EXPORT INDEX4 S4PTR *S4FUNCTION i4create( DATA4 S4PTR *, const char S4PTR *, const TAG4INFO S4PTR * ) ; /* 0 name -> productn */
   S4EXPORT const char S4PTR *S4FUNCTION i4fileName( INDEX4 S4PTR * ) ;
   S4EXPORT const char S4PTR *S4FUNCTION t4fileName( TAG4 S4PTR * ) ;
   S4EXPORT INDEX4 S4PTR *S4FUNCTION i4open( DATA4 S4PTR *, const char S4PTR * ) ;
   S4EXPORT int S4FUNCTION i4reindex( INDEX4 S4PTR * ) ;
   S4EXPORT TAG4 S4PTR *S4FUNCTION i4tag( INDEX4 S4PTR *, const char S4PTR * ) ;
   S4EXPORT int S4FUNCTION i4tagAdd( INDEX4 S4PTR *, const TAG4INFO S4PTR * ) ;
   S4EXPORT TAG4INFO *S4FUNCTION i4tagInfo( INDEX4 * ) ;
   S4EXPORT int S4FUNCTION i4tagRemove( TAG4 S4PTR * ) ;
   #ifdef S4CLIPPER
      void i4removeTagEntryFromGroupFile( INDEX4 *index, const char *fullPathTagNameIn ) ;
   #endif
   S4EXPORT int S4FUNCTION d4indexesRemove( DATA4 S4PTR * ) ;
   int i4indexRemove( INDEX4 * ) ;
#endif /* S4COMP_OFF_INDEX */

/* AS 07/27/99 -> support for generic collating for Unicode and character fields... */
// S4EXPORT T4VFP *S4FUNCTION t4getVfpInfo( TAG4 S4PTR * ) ;  // required by ole-db
S4EXPORT int S4FUNCTION expr4getReturnType( EXPR4 S4PTR *, int ) ;

#ifdef S4JAVA
   // AS July 26/02 - Need to have output messageType == final message for receiveMessage to work correctly
   S4EXPORT int S4FUNCTION java4processMessage( CODE4 *, struct SERVER4CLIENTSt *, short * ) ;
#endif /* S4JAVA */

/* LIST4 */
#ifndef E4LINK
   #ifndef S4DATA_ALIG2
      //#define L4ADD_AFTER_INLINE
   #endif
#endif
#ifdef L4ADD_AFTER_INLINE
  /* callen many times made into define for speed improvements */
  #define l4addAfter( l4, anchor, item )\
   ((((l4)->lastNode==0) ?((l4)->lastNode = (LINK4 *)(item), LINK4PTR(item)->p = (LINK4 *)(item), LINK4PTR(item)->n = (LINK4 *)(item), 0 ):\
     ( LINK4PTR(item)->p = (LINK4 *)(anchor), LINK4PTR(item)->n = LINK4PTR(anchor)->n, LINK4PTR(anchor)->n->p= (LINK4 *)(item),\
     LINK4PTR(anchor)->n = (LINK4 *)(item), (((anchor) == (void *)(l4)->lastNode ) ? ((l4)->lastNode = (LINK4 *)(item),0) : 0 ))),(l4)->nLink++)
#else
   S4EXPORT void S4FUNCTION l4addAfter(   LIST4 S4PTR *, void S4PTR *, void S4PTR * ) ;
#endif
S4EXPORT void S4FUNCTION l4addBefore( LIST4 S4PTR *, void S4PTR *, void S4PTR * ) ;
#define l4init( l4 ) ( (void)( (l4)->selected = 0, (l4)->nLink = 0, (l4)->lastNode = 0 ) )
#define l4numNodes( l4 ) ( (l4)->nLink )
S4EXPORT void S4PTR *S4FUNCTION l4prev( const LIST4 S4PTR *, const void S4PTR * ) ;
S4EXPORT void S4PTR *S4FUNCTION l4remove( LIST4 S4PTR *, void S4PTR * ) ;
S4EXPORT void S4PTR* S4FUNCTION l4firstLow( const LIST4 S4PTR * ) ;  /* Returns 0 if none */
S4EXPORT void S4PTR* S4FUNCTION l4lastLow( const LIST4 S4PTR * ) ;   /* Returns 0 if none */
S4EXPORT void S4PTR* S4FUNCTION l4nextLow( const LIST4 S4PTR *, const void S4PTR * ) ;  /* Returns 0 if none */
S4EXPORT void S4PTR* S4FUNCTION l4popLow( LIST4 S4PTR * ) ;  /* Returns 0 if none */
S4EXPORT void S4FUNCTION l4addLow( LIST4 S4PTR *, void S4PTR * ) ;
#ifdef E4LINK
   #define l4first( l4 )         ( l4firstLow( l4 ) )
   #define l4last( l4 )          ( l4lastLow( l4 ) )
   #define l4next( list, link )  ( l4nextLow( (list), (link) ) )
   #define l4add( list, item )   ( l4addLow( (list), (item) ) )
   #define l4pop( list )         ( l4popLow( list ) )
#else
   #define l4first( l4 )         ((void *)( ( (l4)->lastNode == 0 ) ? 0 : ( (l4)->lastNode->n ) ))
   #define l4last( l4 )          ((void *)( (l4)->lastNode ))
   #define l4next( list, link )  ((void *)( ( (void *)(link) == (list)->lastNode ) ? 0 : ( (void *)(link) == 0 ) ? l4first( list ) : (void *)(((LINK4 *)(link))->n) ))
   #define l4add( list, item )   ( l4addAfter( (list), (list)->lastNode, (item) ) )
   #define l4pop( list )         ( l4remove( (list), (list)->lastNode ) )
#endif

#define single4init( s4 ) ( (s4)->nextLink = 0 )
#define single4next( s4 ) ( (s4)->nextLink )
#define single4initIterate( s4 ) ( (s4)->nextLink )
#define single4add( s4, s4add ) ( (s4add)->nextLink = (s4)->nextLink, (s4)->nextLink = s4add )

#define single4distantToSingle( s4 ) ( (SINGLE4 *)((s4)->nextLink) )
#define single4distantToItem( s4 ) ( (SINGLE4 *)((s4)->nextLink->nextLink) )
#define single4distantNext( s4 ) ( (s4)->nextLink = (s4)->nextLink->nextLink )
#define single4distantInitIterate( s4, p ) ( (s4)->nextLink = (SINGLE4DISTANT *)(p) )
#define single4distantPop( s4 ) ( (s4)->nextLink->nextLink = (s4)->nextLink->nextLink->nextLink )
#define single4distantPopWithCheck( s4 ) ( (s4)->nextLink ? ( (s4)->nextLink->nextLink ? ( single4distantRemove( s4 ) ) : 0 ) : 0 )

/* MEM4 */
S4EXPORT void S4PTR *S4FUNCTION mem4allocDefault( MEM4 S4PTR *, int ) ;  /* 0 Parm causes 0 return */  // CS 2001/02/14 always define
S4EXPORT void S4PTR *S4FUNCTION mem4allocErrDefault( MEM4 S4PTR *, CODE4 S4PTR *, int ) ;  /* 0 Parm causes 0 return */
S4EXPORT int S4FUNCTION mem4checkMemory( void ) ;
S4EXPORT void S4PTR *S4FUNCTION mem4createAllocDefault( CODE4 S4PTR *, MEM4 S4PTR * S4PTR *, int, const unsigned int, int, const int, int ) ;
S4EXPORT MEM4 S4PTR *S4FUNCTION mem4createDefault( CODE4 S4PTR *, int, const unsigned int, int, const int ) ;
S4EXPORT int S4FUNCTION mem4freeCheck( const int ) ;
S4EXPORT int S4FUNCTION mem4freeDefault( MEM4 S4PTR *, void S4PTR * ) ;
S4EXPORT void S4FUNCTION mem4release( MEM4 S4PTR * ) ;

/* TAG4 */
#ifndef S4COMP_OFF_INDEX
   S4EXPORT char S4PTR *S4FUNCTION t4alias( TAG4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION t4close( TAG4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION t4uniqueModify( TAG4 S4PTR *, int ) ;
   #define t4open( a, b, c ) t4openLow( (a), (b), (c), 0 )
   S4EXPORT TAG4 *S4FUNCTION t4openCB(DATA4 *, char * ) ;
   S4EXPORT TAG4 S4PTR *S4FUNCTION t4openLow( DATA4 S4PTR *, INDEX4 S4PTR *, const char S4PTR *, const char S4PTR * ) ;
   S4EXPORT short int S4FUNCTION t4unique( const TAG4 S4PTR * ) ;
   S4EXPORT TAG4 S4PTR *S4FUNCTION t4create( DATA4 S4PTR *, const TAG4INFO S4PTR *, INDEX4 S4PTR *, int ) ;
   #ifdef S4CLIPPER
      int tfile4writeHeader( TAG4FILE *, enum index4headerWrite ) ;
   #endif
   #ifdef S4CLIENT
      #define t4filter( t4 ) ( t4filterLow( t4 ) )
   #else
      #define t4filter( t4 ) ( ( (t4)->tagFile->filter == 0 ? 0 : (t4)->tagFile->filter->source ) )
   #endif
   S4EXPORT const char S4PTR *S4FUNCTION t4filterCB( TAG4 * ) ;
   #ifndef S4SERVER
      S4EXPORT int S4FUNCTION t4uniqueSet( TAG4 S4PTR *, const short ) ;
      S4EXPORT S4CONST char S4PTR *S4FUNCTION t4exprLow( TAG4 S4PTR * ) ;
      S4EXPORT S4CONST char S4PTR *S4FUNCTION t4filterLow( TAG4 S4PTR * ) ;
      #ifdef S4CLIENT
         #define t4expr( t4 ) ( t4exprLow( t4 ) )
      #else
         /* 'source' members are constant, so can use defines */
         #define t4expr( t4 )   ( (t4)->tagFile->expr->source )
      #endif
      S4EXPORT const char S4PTR *S4FUNCTION t4exprCB( TAG4 * ) ;
   #endif
   S4EXPORT unsigned short int S4FUNCTION tfile4isDescending( TAG4FILE * ) ;  /* for SQL */
   S4EXPORT short S4FUNCTION t4descending( TAG4 * ) ;
   S4EXPORT int S4FUNCTION tfile4stokExport( TAG4FILE *t4, char *out, const char *in, int len ) ;  /* for OLE-DB/ODBC */
   S4EXPORT void S4FUNCTION tfile4dtokExport( TAG4FILE *t4, char *out, const double key ) ;  /* for OODBC */
#endif /* S4COMP_OFF_INDEX */

/* UTIL4 */
S4EXPORT int S4FUNCTION u4allocAgainDefault( CODE4 S4PTR *, char S4PTR * S4PTR *, unsigned int S4PTR *, const unsigned int ) ;
S4EXPORT void S4PTR *S4FUNCTION u4allocDefault( long ) ;
S4EXPORT void S4PTR *S4FUNCTION u4allocErDefault( CODE4 S4PTR *, long ) ;
S4EXPORT void S4PTR *S4FUNCTION u4allocFreeDefault( CODE4 S4PTR *, long ) ;
S4EXPORT int S4FUNCTION u4freeDefault( void S4PTR * ) ;
S4EXPORT int S4FUNCTION u4nameChar( unsigned char ) ;
S4EXPORT int S4FUNCTION u4nameCreateMultiDirectories( char S4PTR *, unsigned int, const char S4PTR *, const char S4PTR *, const char S4PTR * ) ;
S4EXPORT void S4FUNCTION u4trim( char * ) ;
/* u4nameCurrent for utils */
S4EXPORT int S4FUNCTION u4nameCurrent( char S4PTR *, const int, const char S4PTR * ) ;
S4EXPORT int S4FUNCTION u4nameCurrentExtended( char S4PTR *, const int, const char S4PTR *, const char S4PTR * ) ;
#ifdef S4WIDECHAR
   S4EXPORT int S4FUNCTION u4nameExtWide( WSTR5 S4PTR *, int, const WSTR5 S4PTR *, const int ) ;
   S4EXPORT int S4FUNCTION u4nameWideRemoveGivenExtension( WSTR5 S4PTR *, const WSTR5 S4PTR * ) ;
#endif
#ifdef S4SERVER
   S4EXPORT int S4FUNCTION u4pathSet( const char *defPath ) ;
#endif
S4EXPORT int S4FUNCTION u4nameRemoveGivenExtension( char S4PTR *, const char S4PTR * ) ;
S4EXPORT int S4FUNCTION u4nameExt( char S4PTR *, int, const char S4PTR *, const int ) ;
S4EXPORT int S4FUNCTION u4namePiece( char S4PTR *, const unsigned int, const char S4PTR *, const int, const int ) ;
S4EXPORT int S4FUNCTION u4namecmp( const char *string1, const char *string2, short ignoreCase ) ;
S4EXPORT int S4FUNCTION u4namencmp( const char *string1, const char *string2, size_t count, short ignoreCase ) ;
#ifdef S4MACINTOSH
   char *u4getMacPath(CODE4 *c4, char *buf, int buflen ) ;
#endif
S4EXPORT unsigned long S4FUNCTION u4ncpy( char S4PTR *, const char S4PTR *, const unsigned int ) ;
S4EXPORT int S4FUNCTION u4remove( const char S4PTR * ) ;  /* used for testing */
S4EXPORT long S4FUNCTION u4switch( void ) ;  /* used for example start-up verification */
S4EXPORT long S4FUNCTION u4switch2( void ) ;  /* used for example start-up verification (for more switches) */
#ifdef S4COMPILE_TEST
   #ifndef S4OFF_WRITE
      S4EXPORT void S4FUNCTION u4yymmdd( char S4PTR * ) ;
   #endif
#else
   S4EXPORT void S4FUNCTION u4yymmdd( char S4PTR * ) ;
#endif

// used for ODBC
S4EXPORT int S4FUNCTION collate4convertCharToKey( COLLATE4 S4PTR *collate, char S4PTR *keyResult, const char S4PTR *unicodeFrom, unsigned int fromLen, int isUnicode, int *lenOut ) ;

#ifdef S4COMPRESS
   #define c4getCompress( c4 ) (1)
#else
   #define c4getCompress( c4 ) (0)
#endif

// AS Jan 24/03 - isolate encryption to a dll
S4EXPORT void S4PTR *S4FUNCTION c4getPreprocess( CODE4 S4PTR * c4 ) ;
S4EXPORT Bool5 S4FUNCTION c4getFileAccessed( CODE4 S4PTR *c4 ) ;

#ifdef S4SERVER
   #define c4getCodePage( c4 ) ( (c4)->codePage )
   #define c4setCodePage( c4, val ) ( (c4)->codePage = (val) )
   #define code4unlockAutoSet( c4, val ) ( (c4)->currentClient->trans.unlockAuto = (val) )
   #define c4getDoRemove( c4 ) ( (c4)->currentClient->doRemove )
   #define c4setDoRemove( c4, i ) ( (c4)->currentClient->doRemove = (i) )
   #define c4getSafety( c4 ) ( (c4)->safety )
   #define c4setSafety( c4, i ) ( (c4)->safety = (i) )
   #define c4setCollatingSequence( c4, i ) ( (c4)->collatingSequence = (i) )
   #define c4setCompatibility( c4, i ) ( (c4)->compatibility = (i) )
   #define i4getIndexFile( i4 ) ((i4)->indexFile)
   #define c4setErrorCode(c4, val ) ( (c4)->errorCodeDefault = (val) )
   #define c4getErrTagName( c4 ) ( (c4)->errTagName )
   #define c4setErrDefaultUnique( c4, val ) ( (c4)->errDefaultUnique = (val) )
   #define c4setErrOpen( c4, val ) ( (c4)->errOpen = (val) )
   #define c4setLockAttempts( c4, val ) ( (c4)->lockAttempts = (val) )
   #define c4setErrTagName( c4, val ) ( (c4)->errTagName = (val) )
   #define c4setErrFieldName( c4, val ) ( (c4)->errFieldName = (val) )
   #define c4getErrDefaultUnique( c4 ) ( (c4)->errDefaultUnique )
   #define c4getFileFlush( c4 ) ( (c4)->fileFlush )
   #define c4getCollatingSequence( c4 ) ( (c4)->collatingSequence )
   #define c4getErrOpen( c4 ) ( (c4)->errOpen )
   #define c4getOledbSchemaCreate( c4 ) ( (c4)->oledbSchemaCreate )
   #define c4setOledbSchemaCreate( c4, val ) ( (c4)->oledbSchemaCreate = (val) )
   #define c4getCatalogClient( c4 ) ( (c4)->catalogClient )
   #define c4getCurrentClient( c4 ) ( (c4)->currentClient )
   #define c4setCurrentClient( c4, val ) ( (c4)->currentClient = (val) )
   #define c4getAccessMode( c4 ) ( (c4)->accessMode )
   #define c4setAccessMode( c4, val ) ( (c4)->accessMode = (val) )
   #define c4getCompatibility( c4 ) ( (c4)->compatibility )
   #define c4getLog( c4 ) ( (c4)->log )
   #define c4getAutoOpen( c4 ) ( (c4)->autoOpen )
   #define c4setAutoOpen( c4, val ) ( (c4)->autoOpen = (val) )
   #define c4setCreateTemp( c4, val ) ( (c4)->createTemp = (val) )
   #define c4getCreateTemp( c4 ) ( (c4)->createTemp )
   #define c4getLockAttempts( c4 ) ( (c4)->lockAttempts )
   #ifdef S4ODBC_STAND
      // it is never invalid in stand/alone ODBC case
      #define c4accessMutexCountZero( c4 ) ( 0 )
      #define c4accessMutexCountNotZero( c4 ) ( 0 )
      #define c4accessMutexCountOne( c4 ) ( 0 )
      #define c4setAccessMutexCount( c4, val ) ( 0 )
      #define c4getAccessMutexCount( c4 ) ( 0 )
   #else
      #define c4accessMutexCountZero( c4 ) ( (c4)->accessMutexCount == 0)
      #define c4accessMutexCountNotZero( c4 ) ( (c4)->accessMutexCount != 0)
      #define c4accessMutexCountOne( c4 ) ( (c4)->accessMutexCount == 1)
      #define c4setAccessMutexCount( c4, val ) ( (c4)->accessMutexCount = (val))
      #define c4getAccessMutexCount( c4 ) ( (c4)->accessMutexCount )
   #endif
   S4EXPORT void S4FUNCTION c4setRunAsService( CODE4 S4PTR *, int ) ;
   S4EXPORT int S4FUNCTION c4getLargeOn(const CODE4 S4PTR * ) ;
   #define error4code( a ) ((a) == 0 ? -1 : (((a)->currentClient == 0 || c4accessMutexCountZero( a )) ? (a)->errorCodeDefault : (a)->currentClient->errorCode ))
   #define error4code2( a ) ((a) == 0 ? -1 : (((a)->currentClient == 0 || c4accessMutexCountZero( a )) ? (a)->errorCode2Default : (a)->currentClient->errorCode2 ))
   S4EXPORT int S4FUNCTION c4getAutoRecover(const CODE4 S4PTR * ) ;
   #define c4setAttemptUpgrade( c4, val ) ( (c4)->attemptUpgrade = (val ) )
#else
   S4EXPORT void S4FUNCTION c4setAttemptUpgrade( CODE4 S4PTR *, short ) ;
   S4EXPORT void S4FUNCTION c4setTransFileName( CODE4 S4PTR *, char S4PTR * ) ;
   #if defined( S4DLL_BUILD ) || defined( S4LIB_BUILD )
      #define i4getIndexFile( i4 )     ((i4)->indexFile)
      #define c4getReadLock( c4 )      ( (c4)->readLock )
      #define c4setReadLock( c4, val ) ( (c4)->readLock = (val) )
      #define c4getReadOnly( c4 )      ( (c4)->readOnly )
      #define c4setReadOnly( c4, val ) ( (c4)->readOnly = (val) )
      #define error4code( c4 )         ( (c4)->errorCode )
      #define error4code2( c4 )        ( (c4)->errorCode2 )
      #ifdef S4OFF_MULTI
         #define code4unlockAutoSet( c4, val ) ( 0 )
      #else
         #define code4unlockAutoSet( c4, val ) ( (c4)->c4trans.trans.unlockAuto = (val) )
      #endif
   #else
      #define i4getIndexFile( i4 )          i4getIndexFileDo( i4 )
      #define c4getReadLock( c4 )           c4getReadLockDo( (c4) )
      #define c4setReadLock( c4, val )      c4setReadLockDo( (c4), (val) )
      #define c4getReadOnly( c4 )           c4getReadOnlyDo( (c4) )
      #define c4setReadOnly( c4, val )      c4setReadOnlyDo( (c4), (val) )
      #define error4code( c4 )              code4getErrorCode( c4 )
      #define error4code2( c4 )             code4getErrorCode2( c4 )
      #define code4unlockAutoSet( c4, val ) code4unlockAutoSetDo( (c4), (val) )
   #endif

   S4EXPORT void S4FUNCTION code4unlockAutoSetDo( CODE4 *, int ) ;
   S4EXPORT short S4FUNCTION code4getErrorCode( const CODE4 * ) ;
   S4EXPORT long S4FUNCTION code4getErrorCode2( const CODE4 * ) ;

   /* Visual Basic and Delphi functions */
   #ifdef S4DLL
      S4EXPORT short S4FUNCTION code4autoOpen( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4codePage( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4collatingSequence( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4compatibility( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4createTemp( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4errCreate( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4defaultUniqueError( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4errDefaultUnique( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4errorCode( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4errorCode2( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4exclusive( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4accessMode( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4exprError( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4errExpr( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4fieldNameError( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4errFieldName( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4fileFlush( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4goError( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4errGo( CODE4 *cb, short value ) ;
      S4EXPORT long  S4FUNCTION code4hInst( CODE4 *cb, long value ) ;
      S4EXPORT long  S4FUNCTION code4hWnd( CODE4 *cb, long value ) ;
      S4EXPORT short S4FUNCTION code4lockAttempts( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4lockAttemptsSingle( CODE4 *cb, short value ) ;
      S4EXPORT long  S4FUNCTION code4lockDelay( CODE4 *cb, long value ) ;
      S4EXPORT short S4FUNCTION code4lockEnforce( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4log( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4memExpandBlock( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4memExpandData( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4memExpandIndex( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4memExpandLock( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4memExpandTag( CODE4 *cb, short value ) ;
      S4EXPORT long  S4FUNCTION code4memSizeBlock( CODE4 *cb, long value ) ;
      S4EXPORT long  S4FUNCTION code4memSizeBuffer( CODE4 *cb, long value ) ;
      S4EXPORT short S4FUNCTION code4memSizeMemo( CODE4 *cb, short value ) ;
      S4EXPORT long  S4FUNCTION code4memSizeMemoExpr( CODE4 *cb, long value ) ;
      S4EXPORT long  S4FUNCTION code4memSizeSortBuffer( CODE4 *cb, long value ) ;
      S4EXPORT long  S4FUNCTION code4memSizeSortPool( CODE4 *cb, long value ) ;
      S4EXPORT short S4FUNCTION code4memStartBlock( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4memStartData( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4memStartIndex( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4memStartLock( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4memStartTag( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4offError( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4errOff( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4openError( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4errOpen( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4optimize( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4optimizeWrite( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4readLock( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4readOnly( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4relateError( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4errRelate( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4safety( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4singleOpen( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4errSkip( CODE4 *cb, short value ) ;
      S4EXPORT short S4FUNCTION code4errTagName( CODE4 *cb, short value ) ;
   #endif /* S4DLL */

   /* AS Oct 24/01 - used in session5.cpp, make available always */
   S4EXPORT long  S4FUNCTION code4memStartMax( CODE4 *cb, long value ) ;

   S4EXPORT short S4FUNCTION code4collate( CODE4 S4PTR *c4, short collateType ) ;
   S4EXPORT short S4FUNCTION code4collateUnicode( CODE4 S4PTR *c4, short collateType ) ;


   /* functions used to set and get CODE4 flags from outside of a DLL in cases
      where the structures are unknown (eg. index independent program) */
   /* cannot be defines */

   S4EXPORT int S4FUNCTION c4getAccessMode( const CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION c4getAutoOpen( const CODE4 S4PTR * ) ;
   S4EXPORT short S4FUNCTION c4getCompatibility( const CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION c4getCreateTemp( const CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION c4getDoRemove( const CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION c4getClipperKeyLen( const CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION c4getErrorCode( const CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION c4getErrCreate( const CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION c4getErrExpr( const CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION c4getErrFieldName( const CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION c4getErrGo( const CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION c4getErrOpen( const CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION c4getErrOff( const CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION c4getErrRelate( const CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION c4getErrSkip( const CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION c4getErrTagName( const CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION c4getLockAttempts( const CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION c4getLockAttemptsSingle( const CODE4 S4PTR * ) ;   /* LY 2001/09/06 */
   S4EXPORT int S4FUNCTION c4getLockDelay( const CODE4 S4PTR * ) ;   /* LY 2001/09/06 */
   S4EXPORT int S4FUNCTION c4getLockEnforce( const CODE4 S4PTR * ) ;
   #if defined(S4WIN32) && defined(__cplusplus) /* CS 1999/09/20 */
      S4EXPORT Mem5zeroAllocator *S4FUNCTION c4getMemZeroAllocator( const CODE4 S4PTR *) ;
   #endif
   S4EXPORT char S4FUNCTION c4getOledbSchemaCreate( const CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION c4getOptimize( const CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION c4getOptimizeWrite( const CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION c4getReadLockDo( const CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION c4getReadOnlyDo( const CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION c4getSafety( const CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION c4getSingleOpen( const CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION c4getErrDefaultUnique( const CODE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION c4getFileFlush( const CODE4 S4PTR * ) ;
   S4EXPORT void S4FUNCTION c4setFileFlush( CODE4 S4PTR *, int ) ;
   S4EXPORT int S4FUNCTION c4getCollatingSequence( const CODE4 S4PTR * ) ;
   S4EXPORT char *S4FUNCTION c4getAppVerify( const CODE4 S4PTR * ) ;

   #ifdef S4CLIENT
      S4EXPORT CONNECT4 S4PTR *S4FUNCTION c4getClientConnect( CODE4 S4PTR * ) ;
   #endif

   #if !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE )
      S4EXPORT void S4FUNCTION c4setLog( CODE4 *c4, int val ) ;
   #endif

   #if defined( S4STAND_ALONE ) && !defined( S4OFF_TRAN ) && !defined( S4OFF_WRITE )
      S4EXPORT int S4FUNCTION c4getLogOpen( const CODE4 S4PTR * ) ;
      S4EXPORT void S4FUNCTION c4setLogOpen( CODE4 S4PTR *, int ) ;
   #endif

   S4EXPORT int S4FUNCTION c4getLog( const CODE4 S4PTR * ) ;
   S4EXPORT void S4FUNCTION c4setLog( CODE4 S4PTR *, int ) ;

/*
   #ifdef S4SERVER
      S4EXPORT SERVER4CLIENT S4PTR *S4FUNCTION c4getCatalogClient( CODE4 S4PTR * ) ;
      S4EXPORT SERVER4CLIENT S4PTR *S4FUNCTION c4getCurrentClient( CODE4 S4PTR * ) ;
   #endif
*/
/* LY 4/21/99 : deleted code4setCatalog for Cam S. */
   #ifndef S4CLIPPER
      S4EXPORT INDEX4FILE S4PTR *S4FUNCTION i4getIndexFileDo( const INDEX4 S4PTR * ) ;
   #endif

   S4EXPORT const char *S4FUNCTION t4getExprSource( TAG4 S4PTR * ) ;

   S4EXPORT void S4FUNCTION c4setAccessMode( CODE4 S4PTR *, char ) ;
   S4EXPORT void S4FUNCTION c4setAutoOpen( CODE4 S4PTR *, int ) ;
   S4EXPORT void S4FUNCTION c4setCompatibility( CODE4 S4PTR *, short ) ;
   S4EXPORT void S4FUNCTION c4setCodePage( CODE4 S4PTR *, int ) ;
   S4EXPORT int S4FUNCTION c4getCodePage( const CODE4 S4PTR * ) ;
   S4EXPORT void S4FUNCTION c4setCollatingSequence( CODE4 S4PTR *, int ) ;
   S4EXPORT void S4FUNCTION c4setCreateTemp( CODE4 S4PTR *, int ) ;
   S4EXPORT void S4FUNCTION code4verifySet( CODE4 S4PTR *, const char S4PTR * ) ;
   S4EXPORT void S4FUNCTION c4setDoRemove( CODE4 S4PTR *, int ) ;
   S4EXPORT void S4FUNCTION c4setErrorCode( CODE4 S4PTR *, int ) ;
   S4EXPORT void S4FUNCTION c4setErrCreate( CODE4 S4PTR *, int ) ;
   S4EXPORT void S4FUNCTION c4setErrExpr( CODE4 S4PTR *, int ) ;
   S4EXPORT void S4FUNCTION c4setErrFieldName( CODE4 S4PTR *, int ) ;
   S4EXPORT void S4FUNCTION c4setErrGo( CODE4 S4PTR *, int ) ;
   S4EXPORT void S4FUNCTION c4setErrOff( CODE4 S4PTR *, int ) ;
   S4EXPORT void S4FUNCTION c4setErrOpen( CODE4 S4PTR *, int ) ;
   S4EXPORT void S4FUNCTION c4setErrRelate( CODE4 S4PTR *, int ) ;
   S4EXPORT void S4FUNCTION c4setErrSkip( CODE4 S4PTR *, int ) ;
   S4EXPORT void S4FUNCTION c4setErrTagName( CODE4 S4PTR *, int ) ;
   #ifdef S4WINDOWS
      S4EXPORT void S4FUNCTION c4setHWnd( CODE4 S4PTR *, HWND ) ;
   #else
      S4EXPORT void S4FUNCTION c4setHWnd( CODE4 S4PTR *, void S4PTR * ) ;
   #endif
   S4EXPORT void S4FUNCTION c4setLockAttempts( CODE4 S4PTR *, int ) ;
   S4EXPORT void S4FUNCTION c4setLockAttemptsSingle( CODE4 S4PTR *, int ) ;   /* LY 2001/09/06 */
   S4EXPORT void S4FUNCTION c4setLockDelay( CODE4 S4PTR *, int ) ;   /* LY 2001/09/06 */
   S4EXPORT void S4FUNCTION c4setLockEnforce( CODE4 S4PTR *, int ) ;
   S4EXPORT void S4FUNCTION c4setOledbSchemaCreate( CODE4 S4PTR *, char ) ;
   S4EXPORT void S4FUNCTION c4setMemSizeBuffer( CODE4 S4PTR *, unsigned ) ;
   S4EXPORT void S4FUNCTION c4setOptimize( CODE4 S4PTR *, int ) ;
   S4EXPORT void S4FUNCTION c4setOptimizeWrite( CODE4 S4PTR *, int ) ;
   S4EXPORT void S4FUNCTION c4setReadLockDo( CODE4 S4PTR *, char ) ;
   S4EXPORT void S4FUNCTION c4setReadOnlyDo( CODE4 S4PTR *, int ) ;
   S4EXPORT void S4FUNCTION c4setSafety( CODE4 S4PTR *, char ) ;
   S4EXPORT void S4FUNCTION c4setSingleOpen( CODE4 S4PTR *, short ) ;
   S4EXPORT void S4FUNCTION c4setErrDefaultUnique( CODE4 S4PTR *, short ) ;
   S4EXPORT void S4FUNCTION c4setMinCountOff( CODE4 S4PTR *, int ) ;
   S4EXPORT void S4FUNCTION c4setAcceptTimeOut( CODE4 S4PTR *, long ) ;
#endif /* S4SERVER */

S4EXPORT short S4FUNCTION code4indexBlockSizeSet( CODE4 S4PTR *, short ) ;
#ifndef S4SERVER
   S4EXPORT short S4FUNCTION code4indexBlockSize( CODE4 S4PTR *) ;
#endif

#define D4goRead( d4, val ) ( D4go( (d4), (val), 0, 0 ) )

#if !defined(S4OFF_WRITE)
   // AS 05/24/00 I4create is used in stand alone...
   /* AS 07/21/99 - added parm for win 95/98 to avoid endless laze writes */
   int I4create( DATA4 *, const char *, TAG4INFO *, char, char, char, char, int, short, enum Collate4name, enum Collate4name ) ;
   /* AS 07/21/99 - added extra parm for win 95/98 to avoid endless laze writes */
   S4EXPORT INDEX4 * S4FUNCTION I4createOpen( DATA4 S4PTR *, const char *, TAG4INFO S4PTR *, char, char, char, char, char, short, enum Collate4name, enum Collate4name ) ;
#endif

S4EXPORT int S4FUNCTION D4writeDirect( DATA4 S4PTR *d4, long recIn, char S4PTR *buffer ) ;  // used for ODBC
S4EXPORT int S4FUNCTION D4appendDirect( DATA4 S4PTR *data, char S4PTR *buffer ) ;  // used for ODBC

#define D4goWrite( d4, val ) ( D4go( (d4), (val), 1, 0 ) )

#if !defined(S4OFF_WRITE) && defined(S4SERVER)
   S4EXPORT int S4FUNCTION I4tagAdd( INDEX4 S4PTR *, TAG4INFO S4PTR * ) ;
#endif

/* AS 04/05/01 - if the client requests the open after a create and it is temp, mark it as such - added a parm */
INDEX4 *I4open( DATA4 *, const char *, char, int, char, char, char, char, char, char ) ;

#ifdef S4SERVER
   #ifdef S4COMTHREADS
      unsigned __stdcall d4server( void * ) ;
   #else
      unsigned d4server( void * ) ;
   #endif
   #ifdef S4WIN32
      __declspec(dllexport) long FAR PASCAL MainWndProc( HWND, UINT, UINT, LONG ) ;
   #else
      #ifndef S4UNIX
         long FAR PASCAL _export MainWndProc( HWND, UINT, UINT, LONG ) ;
      #endif
   #endif
   /* AS June 05/01 - need to check for a valid currentClient on the functions below*/
   #define c4getErrCreate( c4 )      ( (c4)->currentClient == 0 ? (c4)->errCreateDefault : (c4)->currentClient->errCreate )
   #define c4setErrCreate( c4, val ) ( (c4)->currentClient == 0 ? ((c4)->errCreateDefault = (val)) : ((c4)->currentClient->errCreate = (val)) )
   #define c4setReadLock( c4, val )  ( (c4)->currentClient == 0 ? ((c4)->readLockDefault = (val)) : ((c4)->currentClient->readLock = (val)) )
   #define c4getReadLock( c4 )       ( (c4)->currentClient == 0 ? (c4)->readLockDefault : (c4)->currentClient->readLock )

   #define c4getReadOnly( c4 )       ( (c4)->currentClient == 0 ? (c4)->readOnlyDefault : (c4)->currentClient->readOnly )
   #define c4setReadOnly( c4, val )  ( (c4)->currentClient == 0 ? ((c4)->readOnlyDefault = (val)) : ((c4)->currentClient->readOnly = (val)) )

   #ifndef S4UNIX
      #ifdef __cplusplus
         extern "C" {
      #endif
         // CS 2002/06/12  Export these functions
         S4EXPORT BOOL S4FUNCTION InitApplication( HINSTANCE ) ;
         S4EXPORT HWND S4FUNCTION InitInstance( HINSTANCE, int ) ;
         S4EXPORT void S4FUNCTION setServer4global(SERVER4 *ptr);
         S4EXPORT void S4FUNCTION setService(void);
         S4EXPORT BOOL server4taskbarIcon(DWORD operation, const char *tip);
      #ifdef __cplusplus
         }
      #endif
      LRESULT CALLBACK AboutProc( HWND, UINT, WPARAM, LPARAM ) ;
   #endif

   /* AS 07/21/99 - added parm for win 95/98 to avoid endless laze writes */
   S4EXPORT DATA4 *S4FUNCTION D4createOpen( CODE4 *, const char *, const FIELD4INFO *, TAG4INFO *, char, char, unsigned short, int, short, short, Collate4name, Collate4name ) ;
   int D4check( DATA4 * ) ;
   long D4recCount( DATA4 * ) ;
   int D4close( DATA4 * ) ;
   int D4bottom( DATA4 * ) ;
   int D4top( DATA4 * ) ;
   double D4position( DATA4 * ) ;
   int D4positionSet( DATA4 *, double ) ;
   // AS 09/17/99 - made inline...
   // int D4go( DATA4 *, const long, int, Bool5 ) ;
   #ifdef S4OFF_SECURITY
      #define D4go( d4, recno, setOldRecord, assumeLocked ) d4goLow2( (d4), (recno), (setOldRecord), (assumeLocked) )
   #else
      #define D4go( d4, recno, setOldRecord, assumeLocked ) \
         ( ( account4userAllowRead( &(d4)->codeBase->currentClient->account, (d4) ) == FALSE ) ?\
            e4authorize : d4goLow2( (d4), (recno), (setOldRecord), (assumeLocked) ) )
   #endif

   /* AS 07/21/99 - added extra parm for win 95/98 to avoid endless laze writes */
   S4EXPORT DATA4 *S4FUNCTION D4open( CODE4 *, const char *, short, short, short, short, short, int, unsigned short, unsigned short ) ;
   /* AS 07/21/99 - added parm for win 95/98 to avoid endless laze writes */
   int D4seekN( DATA4 *, TAG4 *, const char *, const short, unsigned short int ) ;
   int D4seekDouble( DATA4 *, TAG4 *, const double, unsigned short int ) ;
   int D4tagSync( DATA4 *, TAG4 * ) ;
   int D4skip( DATA4 *, TAG4 *, const long, short int *, long ) ;
   int D4unlock( DATA4 * ) ;
   #ifdef S4OFF_SECURITY
      #define D4appendOledb( d4 ) { d4appendOledb( data ) )
   #else
      #define D4appendOledb( d4 ) ( ( account4userAllowAppend( &((d4)->codeBase->currentClient->account), (d4) ) == FALSE ) ? e4authorize : d4appendOledb( data ) )
   #endif
   #ifdef S4JAVA
      int Server4clientTranCommit(struct SERVER4CLIENTSt *client);
      int Server4clientTranRollback(struct SERVER4CLIENTSt *client);  // CS 2002/05/01
      int Server4clientTranStart(struct SERVER4CLIENTSt *client);
   #endif
   int Server4clientTranAddUser( struct SERVER4CLIENTSt *, long, const long, const char *, const unsigned short int, const char *, TCP4ADDRESS ) ;
   int Client4unlock( struct SERVER4CLIENTSt * ) ;
   /* AS 07/21/99 - added extra parm for win 95/98 to avoid endless laze writes */
   #ifndef S4OFF_WRITE
      int D4create( CODE4 *, const char *, const FIELD4INFO *, TAG4INFO *, DATA4 * *, char, char, int, short, short, Collate4name, Collate4name ) ;
      int D4reindex( DATA4 * ) ;
      int I4reindex( INDEX4 * ) ;
      int D4pack( DATA4 * ) ;
      int D4remove( DATA4 * ) ;
      int D4zap( DATA4 *, long, long ) ;
      int D4memoCompress( DATA4 * ) ;
      int D4write( DATA4 *, long, const int, const int ) ;
      int D4append( DATA4 *, long, long ) ;
      DATA4 *D4fieldsRemove( DATA4 **, int, char * ) ;
      int D4indexesRemove( DATA4 * ) ;
      #define D4appendCurrent( d4 ) ( D4append( (d4), (d4)->clientId, (d4)->serverId ) )
      #define D4writeCurrent( d4, i1, doLock ) ( D4write( (d4), (i1), 0, (doLock) ) )
   #endif
   #define c4systemPath( c4 ) ( (c4)->server->systemPath )
#else
   #define c4systemPath( c4 ) ( "" )
   #define D4close( d4 ) ( d4close( d4 ) )
   #define D4recCount( d4 ) ( d4recCount( d4 ) )
   #define D4fieldsRemove( d4, n, nm ) ( d4fieldsRemove( (d4), (n), &(nm) ) )
   // AS 09/13/99 sped up OLE-DB with special append function...
   #define D4appendCurrent( d4 ) ( d4appendOledb( d4 ) )
   #define D4appendOledb( d4 ) (d4appendOledb( d4 ) )
   #define D4writeCurrent( d4, rec, doLock ) ( d4writeLow( (d4), (rec), 0, (doLock) ) )
   #define D4go( d4, rec, setOld, assumeLocked ) ( d4goLow2( (d4), (rec), (setOld), assumeLocked ) )
   #define D4remove( d4 ) ( d4remove( d4 ) )
   #define D4indexesRemove( d4 ) ( d4indexesRemove( d4 ) )
#endif /* S4SERVER */


/* INTERNAL FUNCTIONS : */

#ifndef S4OFF_INDEX
   S4EXPORT int S4FUNCTION t4keyLenExported( TAG4 S4PTR * ) ;
#endif

S4EXPORT TAG4FILE * S4FUNCTION t4getTagFile( TAG4 S4PTR * ) ;

#ifndef S4STAND_ALONE
   /* AS June 5/01 - see c4com.h CONNECTION4OPEN_INDEX_INFO_OUT structure for info on changes */
   int client4indexSetup( CODE4 *, DATA4 *, DATA4FILE *, unsigned int, unsigned short, const char *, unsigned int, const char *, INDEX4 * ) ;
#endif

void e4applyAscend( int keyType, char *buffer, unsigned long len ) ;
S4EXPORT long S4FUNCTION time4long( const char *, int ) ;   /* LY 00/11/10 : exported for t4expr.c */

S4EXPORT unsigned short S4FUNCTION code4actionCode( CODE4 S4PTR * ) ;
S4EXPORT double S4FUNCTION code4status( CODE4 S4PTR * ) ;

#if !defined( S4CLIENT ) && !defined( S4OFF_INDEX )
   /* B4BLOCK */
   B4BLOCK *b4alloc( TAG4FILE *, const B4NODE ) ;
   int b4calcBlanks( const unsigned char *, const int, const unsigned char ) ;
   int b4doFlush( B4BLOCK * ) ;
   #define b4flush( b4 ) ( (b4)->changed ? b4doFlush( b4 ) : 0 )
   int b4free( B4BLOCK * ) ;
   void b4goEof( B4BLOCK * ) ;
   #ifndef S4INLINE /* CS 2000/01/05 macro by same name defined in D4INLINE.H */
      unsigned char *b4keyKey( B4BLOCK *, const int ) ;
      int b4lastpos( const B4BLOCK * ) ;
      int b4leaf( const B4BLOCK * ) ;
   #endif
   int b4dataLeaf( void *, TAG4FILE * ) ;
   #ifdef S4FOX
      #ifdef E4PARM_LOW
         S4EXPORT unsigned long S4FUNCTION b4recNo( const B4BLOCK S4PTR *, const int ) ;
      #else
         #define b4recNo( b4, i ) ((unsigned long)(( (b4)->header.nodeAttribute >= 2 ) ? x4recNo( (b4), (i) ) : \
          ( x4reverseLong( (void *)(((unsigned char *)&(b4)->nodeHdr) + \
          (i) * (2 * sizeof(S4LONG) + (b4)->tag->header.keyLen) + (b4)->tag->header.keyLen )))))
      #endif
   #endif
   int b4remove( B4BLOCK *) ;
   #ifndef S4CLIPPER
      void b4nodeGetFilePosition( INDEX4FILE *, const B4NODE, FILE4LONG * ) ;
      void b4nodeSetFromFilePosition( INDEX4FILE *, B4NODE *, FILE4LONG ) ;
      void b4nodeSubtractBlocks( B4NODE *, INDEX4FILE *, int ) ;
      void b4nodeAddBlocks( B4NODE *, INDEX4FILE *, int ) ;
   #endif
   void b4getFilePosition( B4BLOCK *, FILE4LONG * ) ;
   /* AS Nov 13/02 - export for dot */
   S4EXPORT int S4FUNCTION b4skip( B4BLOCK *, int ) ;
   int b4seek( B4BLOCK *, const char *, const int ) ;
   int tfile4unique( TAG4FILE *, const short int ) ;
   #if !defined( S4OFF_INDEX ) && !defined( S4CLIENT ) && defined( S4FOX )
      int tfile4stok( TAG4FILE *t4, char *buf, const char *str, int len ) ;
      void tfile4dtok( TAG4FILE *t4, char *buf, const double dkey ) ;
   #endif
   #define tfile4keyLen( tagFile ) ( (tagFile)->header.keyLen )

   #ifdef E4INDEX_VERIFY
      int b4verify( B4BLOCK *b4 ) ;
   #endif
   #if defined( S4CLIPPER ) && defined( E4DEBUG )
      void b4verifyPointers( const B4BLOCK *b4 ) ;
   #endif

   #ifdef S4FOX
      /* LY 2000/04/06 - unsigned long to S4UNSIGNED_LONG for 64-bit */
      int  b4brReplace( B4BLOCK *, const unsigned char *, const S4UNSIGNED_LONG ) ;
      int  b4calcDups( const unsigned char *, const unsigned char *, const int ) ;
      #ifndef S4INLINE /* CS 2000/01/05 macro by same name defined in D4INLINE.H */
         int  b4go( B4BLOCK *, const int ) ;
         int  b4insert( B4BLOCK *, const void *, const unsigned long, const unsigned long, const char ) ;
      #endif
      int  b4insertLeaf( B4BLOCK *, const void *, const S4UNSIGNED_LONG ) ;
      int  b4insertBranch( B4BLOCK *, const void *, const S4UNSIGNED_LONG, const S4UNSIGNED_LONG, const char ) ;
      void b4leafInit( B4BLOCK * ) ;
      int  b4leafSeek( B4BLOCK *, const char *, const int ) ;
      int  b4reindex( B4BLOCK * ) ;
      int  b4removeLeaf( B4BLOCK * ) ;
      /* LY 2000/04/07 - unsigned long to S4UNSIGNED_LONG for 64-bit */
      int  b4rBrseek( B4BLOCK *, const char *, const int, const S4UNSIGNED_LONG ) ;
      #ifdef S4FOX
         #define b4top( b4 ) ( (b4)->keyOn = 0, b4leaf( b4 ) ? ( (b4)->curDupCnt = 0,\
           ((b4)->curPos = (char *)&(b4)->header + i4blockSize( (b4)->tag->indexFile ) - (b4)->tag->header.keyLen + x4trailCnt( (b4), 0), 0)): 0)
      #else
         int  b4top( B4BLOCK * ) ;
      #endif
      int  tfile4branchSplit( TAG4FILE *, B4BLOCK *, B4BLOCK * ) ;
      int  tfile4init( TAG4FILE *, INDEX4 *, FILE4LONG, unsigned char * ) ;
      int  tfile4leafSplit( TAG4FILE *, B4BLOCK *, B4BLOCK * ) ;
      int  tfile4rSeek( TAG4FILE *, void *, int, long ) ;
      #ifndef S4FOX
         int  S4CALL t4descMemcmp( S4CMP_PARM, S4CMP_PARM, size_t ) ;
      #endif
      #ifdef S4DATA_ALIGN
         #define X4DUPCNT_NOT_INLINE
      #else
         #ifdef S4DO_BYTEORDER
            #define X4DUPCNT_NOT_INLINE
         #else
            #ifdef E4PARM_LOW
               #define X4DUPCNT_NOT_INLINE
            #else
               #ifdef E4ANALYZE
                  #define X4DUPCNT_NOT_INLINE
               #endif
            #endif
         #endif
      #endif
      #ifdef X4DUPCNT_NOT_INLINE
         /* AS Nov 13/02 - export for dot */
         S4EXPORT int S4FUNCTION x4dupCnt( const B4BLOCK *, const int ) ;
      #else
         #define x4dupCntSmallInfo( b4, num ) (( *((unsigned long *)( b4->data + num * b4->nodeHdr.infoLen )) >> (b4->nodeHdr.recNumLen) ) & b4->nodeHdr.dupByteCnt )
         #define x4dupCntLargeInfo( b4, num ) (( *((unsigned long *)( b4->data + num * b4->nodeHdr.infoLen + 2 )) >> (b4->nodeHdr.recNumLen - 16) ) & b4->nodeHdr.dupByteCnt )
         #define x4dupCnt( b4, numInBlock ) ( (int)( ( b4->nodeHdr.infoLen > 4 ) ? x4dupCntLargeInfo( b4, numInBlock ) : x4dupCntSmallInfo( b4, numInBlock ) ) )
      #endif
      int x4putInfo( const B4NODE_HEADER *, void *, const S4UNSIGNED_LONG, const int, const int ) ;
      #ifdef S4DATA_ALIGN
         #define X4RECNO_NOT_INLINE
      #else
         #ifdef S4DO_BYTEORDER
            #define X4RECNO_NOT_INLINE
         #else
            #ifdef E4PARM_LOW
               #define X4RECNO_NOT_INLINE
            #endif
         #endif
      #endif
      #ifdef X4RECNO_NOT_INLINE
         S4UNSIGNED_LONG x4recNo( const B4BLOCK *, const int ) ;
      #else
         #define x4recNo( b4, numInBlock ) ( *((S4UNSIGNED_LONG *)( (b4)->data + (numInBlock) * (b4)->nodeHdr.infoLen )) & *(unsigned S4LONG *)&(b4)->nodeHdr.recNumMask[0] )
      #endif
      #ifdef S4DATA_ALIGN
         #define X4TRAILCNT_NOT_INLINE
      #else
         #ifdef S4DO_BYTEORDER
            #define X4TRAILCNT_NOT_INLINE
         #else
            #ifdef E4PARM_LOW
               #define X4TRAILCNT_NOT_INLINE
            #else
               #ifdef E4ANALYZE
                  #define X4TRAILCNT_NOT_INLINE
               #endif
            #endif
         #endif
      #endif
      #ifdef X4TRAILCNT_NOT_INLINE
         /* AS Nov 13/02 - export for dot */
         S4EXPORT int S4FUNCTION x4trailCnt( const B4BLOCK *, const int ) ;
      #else
         #define x4trailCntGetLptrShort( b4, num ) ( (unsigned long *)( (b4)->data + (num) * (b4)->nodeHdr.infoLen ) )
         #define x4trailCntGetPosShort( b4 ) ( (b4)->nodeHdr.recNumLen + (b4)->nodeHdr.dupCntLen )
         #define x4trailCntGetLptrLong( b4, num ) ( (unsigned long *)( (b4)->data + (num) * (b4)->nodeHdr.infoLen + 2 ) )
         #define x4trailCntGetPosLong( b4 ) ( (b4)->nodeHdr.recNumLen - 16 + (b4)->nodeHdr.dupCntLen )
         #define x4trailCnt( b4, num ) ( \
            ( (b4)->nodeHdr.infoLen > 4 ) ?  (int)( ( *(x4trailCntGetLptrLong( b4, num )) >> (x4trailCntGetPosLong( b4 )) ) & (b4)->nodeHdr.trailByteCnt ) \
            : (int)( ( *(x4trailCntGetLptrShort((b4),(num))) >> (x4trailCntGetPosShort(b4)) ) & (b4)->nodeHdr.trailByteCnt ) )
      #endif
      /* AS Nov 13/02 - export for dot */
      S4EXPORT B4KEY_DATA S4PTR * S4FUNCTION b4key( B4BLOCK S4PTR *, const int ) ;
      #define b4numKeys( b4 ) ( (b4)->header.nKeys )
   #else
      #define b4numKeys( b4 ) ( (b4)->nKeys )
      S4EXPORT B4KEY_DATA S4PTR *S4FUNCTION b4key( const B4BLOCK S4PTR *, const int ) ;
   #endif /* S4FOX */

   #ifdef S4MDX
      int b4insert( B4BLOCK *, const void *, const unsigned long ) ;
      int tfile4init( TAG4FILE *, INDEX4 *, T4DESC * ) ;
   #endif /* S4MDX */

   S4EXPORT int S4FUNCTION tfile4lock( TAG4FILE S4PTR *, const long ) ; /*Move outside the S4CLIPPER to facilitate index independant OLE-DB DLL*/
   S4EXPORT int S4FUNCTION tfile4unlock( TAG4FILE S4PTR *, const long ) ;

   #ifdef S4CLIPPER
      int b4append( B4BLOCK *, const unsigned long ) ;
      int b4insert( B4BLOCK *, const void *, const unsigned long, const unsigned long ) ;
      int b4room( const B4BLOCK * ) ;
      int tfile4doVersionCheck( TAG4FILE *, int, int ) ;
      int tfile4close( TAG4FILE *, DATA4FILE * ) ;
      unsigned long tfile4extend( TAG4FILE * ) ;
      int tfile4updateHeader( TAG4FILE * ) ;
      void tfile4removeBranch( TAG4FILE *, B4BLOCK * ) ;
      int b4append2( B4BLOCK *, const void *, const unsigned long, const unsigned long ) ;
      int tfile4getReplaceEntry( TAG4FILE *, B4KEY_DATA *, B4BLOCK * ) ;
      int tfile4shrink( TAG4FILE *, unsigned long ) ;
   #else
      int tfile4free( TAG4FILE *tagFile ) ;
   #endif /* S4CLIPPER */
#endif /* #if !defined( S4CLIENT ) && !defined( S4OFF_INDEX ) */

S4EXPORT int S4FUNCTION c4clip( char *, int, int ) ;
char *c4descend( char *, const char *, int ) ; /* exported for OLEDB */
char *c4descendBinary( char *, const char *, int ) ;
void c4dtoaClipper( double, char *, int, int ) ;
S4EXPORT int  S4FUNCTION c4dtoa45( double, char *, int, int ) ; /* need to export for ++ API */
S4EXPORT void S4FUNCTION c4lower( char * ) ;
S4EXPORT void S4FUNCTION c4ltoa45( long, char *, int ) ;
S4EXPORT void S4FUNCTION c4upper( char * ) ;

#ifdef S4SERVER
   #if defined( S4COM_PRINT ) && defined( S4TESTING ) && defined( E4FILE_LINE )
      void code4enterExclusiveDo( CODE4 *, struct SERVER4CLIENTSt *, const char *file, int line, int doDelay = 1 ) ;
      void code4exitExclusiveDo( CODE4 *, struct SERVER4CLIENTSt *, const char *file, int line ) ;
      #define code4enterExclusive( cb, client, doDelay ) code4enterExclusiveDo( (cb), (client), __FILE__, __LINE__, (doDelay) )
      #define code4exitExclusive( cb, client ) code4exitExclusiveDo( (cb), (client), __FILE__, __LINE__ )

      // don't worry about veirfy first/last when printing - at least not at this point
      #define code4enterExclusiveVerifyFirst( c4, client, val ) code4enterExclusiveDo( (c4), (client), __FILE__, __LINE__, (val) )
      #define code4exitExclusiveVerifyLast( c4, client ) code4exitExclusiveDo( (c4), (client), __FILE__, __LINE__ )
   #else
      S4EXPORT void S4FUNCTION code4enterExclusive( CODE4 S4PTR *, struct SERVER4CLIENTSt S4PTR *, int doDelay = 1 ) ;
      S4EXPORT void S4FUNCTION code4exitExclusive( CODE4 S4PTR *, struct SERVER4CLIENTSt S4PTR * ) ;
      #ifdef E4ANALYZE
         S4EXPORT void S4FUNCTION code4enterExclusiveVerifyFirst( CODE4 S4PTR *, struct SERVER4CLIENTSt S4PTR *, int doDelay ) ;
         S4EXPORT void S4FUNCTION code4exitExclusiveVerifyLast( CODE4 S4PTR *, struct SERVER4CLIENTSt S4PTR * ) ;
      #else
         #define code4enterExclusiveVerifyFirst( c4, client, val ) ( code4enterExclusive( (c4), (client), (val) ) )
         #define code4exitExclusiveVerifyLast( c4, client ) ( code4exitExclusive( (c4), (client) ) )
      #endif
   #endif
#endif

void code4memStartMaxSet( CODE4 *, const int ) ;
/* AS 09/01/98 code4numCodeBase critical to be fast, don't implement as function */
extern unsigned int numCode4 ;
#define code4numCodeBase() ( numCode4 )
int code4optRestart( CODE4 * ) ;
int code4unlockDo( LIST4 * ) ;
int code4verify( CODE4 *, int ) ;
S4EXPORT long S4FUNCTION code4version( CODE4 S4PTR * ) ;

#ifdef S4SERVER
   int code4dataFileCloseAll( CODE4 * ) ;
   DATA4 *code4idData( CODE4 *, const long, const long ) ;
   TRAN4 *code4idDataTrans( const CODE4 *, const long, const long ) ;
   const char *code4idDataAlias( const CODE4 *, const long, const long ) ;
#else
   #define code4idData( c4, b, c ) ( tran4data( code4trans(c4), (b), (c) ) )
   #define code4idDataTrans( c4, a, b ) ( (code4trans(c4) ) )
   #define code4idDataAlias( c4, a, b ) ( d4alias(tran4data( code4trans(c4), (a), (b) )) )
#endif

#ifdef S4LOCK_HOOK
   int code4lockHook( CODE4 *, const char *, const char *, const char *, long, int ) ;
#endif

#ifdef S4TIMEOUT_HOOK
   int code4timeoutHook( CODE4 *, int, long ) ;
#endif

/* used for internal testing only */
S4EXPORT int S4FUNCTION file4seqWriteDelay( FILE4SEQ_WRITE S4PTR * ) ;
S4EXPORT enum index4format S4FUNCTION code4indexFormat( CODE4 S4PTR * ) ;
S4EXPORT long S4FUNCTION code4serverOS( CODE4 S4PTR * ) ;
// AS July 2/02 - make available all the time...
S4EXPORT int S4FUNCTION code4ping( CODE4 S4PTR * ) ;
#ifndef S4OFF_TRAN
   #ifdef S4STAND_ALONE
      S4EXPORT int S4FUNCTION code4transFileEnable( CODE4TRANS S4PTR *, const char S4PTR *, const int ) ;
   #else
      int code4transFileEnable( CODE4TRANS *, const char *, const int ) ;
      S4EXPORT int S4FUNCTION code4transFileEnableBackup( CODE4TRANS *c4trans, const char *backupLogName, const int doCreate ) ;
   #endif
#endif

#define dfile4changed( a ) ( (a)->fileChanged = 1 )
int dfile4check( DATA4FILE * ) ;
int dfile4close( DATA4FILE * ) ;
int dfile4closeLow( DATA4FILE * ) ;
int dfile4create( CODE4 *, const char *, const FIELD4INFO *, const TAG4INFO *, DATA4 * * ) ;
DATA4FILE * dfile4data( CODE4 *, const char * ) ;
int dfile4flush( DATA4FILE * ) ;
int dfile4flushData( DATA4FILE * ) ;
int dfile4flushIndex( DATA4FILE * ) ;
int dfile4goData( DATA4FILE *, long, void *, int ) ;
int dfile4memoCompress( DATA4FILE *, DATA4 * ) ;
#ifndef S4OFF_MULTI
   int dfile4memoUnlock( DATA4FILE * ) ;
   int dfile4lockMemo( DATA4FILE * ) ;
#endif
DATA4FILE *dfile4open( CODE4 *, DATA4 *, const char *, char * * ) ;
S4CONST char *dfile4name( S4CONST DATA4FILE * ) ;
S4EXPORT S4CONST char S4PTR * S4FUNCTION d4fullPath( S4CONST DATA4 S4PTR * ) ;   // FOR ODBC - equivalent of dfile4name, but using DATA4 pointer
int dfile4read( DATA4FILE *, long, char *, int ) ;
int dfile4readOld( DATA4FILE *, long ) ;
S4EXPORT long S4FUNCTION dfile4recCount( DATA4FILE S4PTR *, const long ) ;  /* exported for single-user version (d4recCount replacement) */
//#define dfile4recordPosition( d4, rec ) ( (unsigned long)(d4)->headerLen + (unsigned long)(d4)->recWidth * ( (rec) - 1 ) )
#ifndef S4CLIENT
   #ifdef S4WIN32
      #ifdef S4FILE_EXTENDED
         #define dfile4recordPosition( d4, recNo ) ( file4longCoerce((DWORDLONG)((d4)->recWidth) * (DWORDLONG)(recNo-1) + (DWORDLONG)((d4)->headerLen) ))
      #else
         #define dfile4recordPosition( d4, rec ) ( (unsigned long)(d4)->headerLen + (unsigned long)(d4)->recWidth * ( (rec) - 1 ) )
      #endif
   #else
      S4EXPORT FILE4LONG S4FUNCTION dfile4recordPosition( DATA4FILE *, long ) ;
   #endif
#endif

#define dfile4recWidth( d4 ) ((unsigned int)(d4)->recWidth)
int dfile4refresh( DATA4FILE * ) ;

#ifndef S4OFF_INDEX
   int dfile4reindex( DATA4FILE * ) ;
   TAG4FILE *dfile4tag( DATA4FILE *, const char * const ) ;
   TAG4FILE *dfile4tagDefault( DATA4FILE * ) ;
   TAG4FILE *dfile4tagNext( DATA4FILE *, TAG4FILE * ) ;
   TAG4FILE *dfile4tagPrev( DATA4FILE *, TAG4FILE * ) ;
   int dfile4tagSelect( DATA4FILE *, TAG4FILE * ) ;
   TAG4FILE *dfile4tagSelected( DATA4FILE * ) ;
   int dfile4updateIndexes( DATA4FILE * ) ;
#endif /* S4OFF_INDEX */
int dfile4updateHeader( DATA4FILE *, int, int, Bool5 ) ;

#if !defined(S4CLIENT) && !defined(S4OFF_WRITE) && !defined( S4OFF_MULTI )
   double dfile4getAutoIncrementValue( DATA4FILE * ) ;
#endif

int dfile4verify( DATA4FILE *, int ) ;
int dfile4writeData( DATA4FILE *, const long, const char * ) ;

#if !defined( S4CLIPPER ) && !defined( S4COMP_OFF_INDEX )
   INDEX4FILE * dfile4index( DATA4FILE *, const char * ) ;
#endif

#ifndef S4OFF_MULTI
   #ifdef S4CLIENT
      #ifdef __cplusplus
         int dfile4lock( DATA4FILE *, const long, const long, const long, Lock4type lockType = lock4write ) ;
      #else
         int dfile4lock( DATA4FILE *, const long, const long, const long, enum Lock4type lockType ) ;
      #endif
   #else
      int dfile4lock( DATA4FILE *, const long, const long, const long ) ;
   #endif
   int dfile4lockAll( DATA4FILE *, const long, const long ) ;
   int dfile4lockAppend( DATA4FILE *, const long, const long ) ;
   int dfile4lockAppendRecord( DATA4FILE *, const long, const long ) ;
   S4EXPORT int S4FUNCTION dfile4lockIndex( DATA4FILE *, const long ) ;
   #ifdef S4CLIENT
      #ifdef __cplusplus
         int dfile4lockTest( DATA4FILE *, const long, const long, const long, enum Lock4type lockType = lock4write ) ;
      #else
         int dfile4lockTest( DATA4FILE *, const long, const long, const long, enum Lock4type lockType ) ;
      #endif
   #else
      int dfile4lockTest( DATA4FILE *, const long, const long, enum Lock4type ) ;
   #endif
   /* AS Nov 13/02 - export for dot */
   S4EXPORT int S4FUNCTION dfile4lockTestIndex( DATA4FILE *, const long ) ;
   int dfile4lockTestRecs( DATA4FILE *, const long, const long ) ;
   int dfile4unlockFile( DATA4FILE *, const long, const long ) ;
   int dfile4unlockAppend( DATA4FILE *, const long, const long ) ;
   #ifdef S4CLIENT
      #ifdef __cplusplus
         int dfile4lockFile( DATA4FILE *, const long, const long, DATA4 *, Lock4type lockType = lock4write ) ;
      #else
         int dfile4lockFile( DATA4FILE *, const long, const long, DATA4 *, enum Lock4type ) ;
      #endif
      int dfile4unlockRecords( DATA4FILE *, const long, const long ) ;
   #else
      int dfile4lockFile( DATA4FILE *, const long, const long, enum Lock4type ) ;
   #endif
#endif /* S4OFF_MULTI */

#ifdef S4DOS
   int d4negativeLockTest( CODE4 * ) ;
#endif

#ifdef S4CLIENT
   int dfile4remove( DATA4FILE * ) ;
#else
   int dfile4optimize( DATA4FILE *, const int ) ;
   int dfile4optimizeWrite( DATA4FILE *, const int ) ;
   int dfile4packData( DATA4FILE * ) ;
   int dfile4unappendData( DATA4FILE *, const long, const long ) ;
   int dfile4zapData( DATA4FILE *, long, long ) ;
   #if !defined( S4INDEX_OFF ) && !defined( S4OFF_MULTI )
      // required for ODBC
      S4EXPORT int S4FUNCTION d4unlockIndex( DATA4 * ) ;
      S4EXPORT int S4FUNCTION dfile4unlockIndex( DATA4FILE *, const long ) ;
   #endif
#endif

#ifndef S4OFF_SECURITY
   int d4authorize( DATA4 *, struct SERVER4CLIENTSt * ) ;
#endif

#if !defined( S4OFF_TRAN ) && !defined( S4CLIENT )
   int d4startMiniTransactionIfRequired( DATA4 *data ) ;
   int d4transEnabled( DATA4 *data, Bool5 checkActiveStatus ) ;
#endif

S4EXPORT void S4FUNCTION d4blankLow( DATA4 *, char * ) ;
int d4getTables( DATA4 *data, const char *path ) ;
int d4goData( DATA4 *, long ) ;
int d4goVirtual( DATA4 *, const long, const int, const void *, void *, const int ) ;
/* AS Apr 10/02 - New function for advance-reading client/server */
int d4goVirtual2( DATA4 *data, const long recNo, const int rc, short recLocked, const void *record, const int fromCurrentPos, const char *memos ) ;
int d4read( DATA4 *, const long, char * ) ;
int d4readOld( DATA4 *, const long ) ;
S4EXPORT long S4FUNCTION d4readBuffer( DATA4 S4PTR *, long, short ) ;
/* AS Dec 17/02 - New function for configuring advance-reading client/server */
S4EXPORT int S4FUNCTION d4readBufferConfigure( DATA4 *data, long flags ) ;
S4EXPORT long S4FUNCTION d4writeBuffer( DATA4 S4PTR *, long ) ;
#ifdef S4CLIENT
   // AS May 21/02 - code to support auto-transfer of memo fields
   int d4goVirtualDoMemos( DATA4 *data, const char *memos ) ;
   /* AS Apr 10/02 - New function for advance-reading client/server */
   int f4memoReadSet( FIELD4 *field, long memoLen, const char *contents ) ;
   void d4batchReadFree( DATA4 *data ) ;
   void d4readBufferReset( DATA4 * ) ;   // used to reset the read buffer to indicate not-read (but leave buffers in place)
   int d4skipFetchBuffer( DATA4 *data, char *out, long lenLeft, int nSkip ) ;
   int d4writeBufferDo( DATA4 * ) ;
   void d4batchWriteFree( DATA4 * ) ;
   int d4skipFetchMultiple( DATA4 *data, short mode, const long nSkip ) ;
   void code4writeBufferFlush( CODE4 *c4 ) ;
   void code4writeBufferReset( CODE4 *c4 ) ;
#endif
S4EXPORT int S4FUNCTION d4recCountLessEq( DATA4 S4PTR *, long ) ;
S4EXPORT int S4FUNCTION d4recCountLess( DATA4 S4PTR *, long ) ;
S4EXPORT int S4FUNCTION d4recCountGreater( DATA4 S4PTR *, long ) ;
int d4tagUniqueSync( DATA4 * ) ;
int d4tagSyncDo( DATA4 S4PTR *, TAG4 S4PTR * const, int ) ;
#ifndef S4OFF_MULTI
   /* for next 4 functions, use '-1' for data4 clientId if appropriate
      read. for server only */
   #ifdef S4SERVER
      int S4FUNCTION d4unlockAppend( DATA4 *, long clientId ) ;  // CS 2001/01/16 add S4FUNCTION
      /* AS Nov 13/02 - export for dot */
      S4EXPORT int S4FUNCTION d4unlockData( DATA4 *, long clientId ) ;
      S4EXPORT int S4FUNCTION d4unlockRecords( DATA4 *, long clientId ) ;
      S4EXPORT int S4FUNCTION d4unlockFile( DATA4 *, long clientId ) ;
   #else
      #ifdef __cplusplus
         /* AS Nov 13/02 - export for dot */
         S4EXPORT int S4FUNCTION d4unlockData( DATA4 *, long clientId = -1 ) ;
         S4EXPORT int S4FUNCTION d4unlockAppend( DATA4 *, long clientId = -1 ) ;
         S4EXPORT int S4FUNCTION d4unlockRecords( DATA4 *, long clientId = -1 ) ;
         S4EXPORT int S4FUNCTION d4unlockFile( DATA4 *, long clientId = -1 ) ;
      #else
         S4EXPORT int S4FUNCTION d4unlockData( DATA4 *, long clientId ) ;
         S4EXPORT int S4FUNCTION d4unlockAppend( DATA4 *, long clientId ) ;
         S4EXPORT int S4FUNCTION d4unlockRecords( DATA4 *, long clientId ) ;
         S4EXPORT int S4FUNCTION d4unlockFile( DATA4 *, long clientId ) ;
      #endif
   #endif
#endif

#ifndef S4OFF_WRITE
   S4EXPORT int S4FUNCTION d4update( DATA4 * ) ;  // CS 2001/03/27
   int d4updateRecord( DATA4 *, const int, const int ) ;
#endif

#ifdef S4STAND_ALONE
S4EXPORT void S4FUNCTION d4versionCheck( DATA4 *data ) ;  // used for ole-db in stand-alone case
/* AS Aug 15/01 - Function to retrieve a version number associated with the data file, which increases when
   the data file changes
*/
#endif

#if defined( S4WIN32 ) || !defined( S4STAND_ALONE )
   S4EXPORT long S4FUNCTION d4versionNumber( DATA4 *data ) ;
   #ifdef S4STAND_ALONE
      void dfile4versionIncrement( DATA4FILE *dataFile ) ;
   #else
      #define dfile4versionIncrement( d4 ) ( (d4)->versionNumber++ )
   #endif
#endif

int d4verify( DATA4 *, const int ) ;

// AS Feb 6/03 - Not available in client mode.
#if !defined( S4CLIENT ) && defined( S4FOX )
   #define d4compatibility( d4 ) ( (d4)->dataFile->compatibility )
#endif

/* #ifdef S4FOX  LY 00/10/23 : req'd for c4dll32.def */
   S4EXPORT int S4FUNCTION d4compatibilityExport( DATA4 S4PTR *d4 ) ;
/*#endif*/
#define d4version( d4 ) ( (d4)->dataFile->version )

#ifdef S4CLIENT
   int d4localLockSet( DATA4 *, const long ) ;
   int dfile4registerLocked( DATA4FILE *, const long ) ;
   //CJ - 06/08/01 This function is a static void function does not need to be declared. ( MAC error)
   //void d4unlockClientData( DATA4 * ) ;
#else
   /* AS Nov 13/02 - export for dot */
   S4EXPORT int S4FUNCTION d4packData( DATA4 * ) ;
   int d4writeData( DATA4 *, const long, const int ) ;
   int d4writeKeys( DATA4 *, const long ) ;
   /* AS Nov 13/02 - export for dot */
   S4EXPORT int S4FUNCTION d4zapData( DATA4 *, const long, const long ) ;
   #ifndef S4OFF_MULTI
      int dfile4registerLocked( DATA4FILE *, const long, int ) ;
      #ifndef S4OFF_MEMO
         int d4validateMemoIds( DATA4 * ) ;
      #endif
   #endif
#endif

#ifndef S4OFF_MULTI
   #ifdef S4STAND_ALONE
      #define d4lockTestIndex( d4 ) ( dfile4lockTestIndex( (d4)->dataFile, 1L ) )
   #endif
   S4EXPORT int S4FUNCTION d4lockTestIndexExport( DATA4 * ) ;  // for ODBC
#endif

#ifdef S4SERVER
   long d4skipRecno( DATA4 *, long ) ;
#else
#endif

S4EXPORT long S4FUNCTION error4number2( const long ) ;
void error4logAppend( CODE4 *c4, int, long, const char *, const char *, const char * ) ;
S4EXPORT void S4FUNCTION error4out( CODE4 *, int, long, const char *, const char *, const char * ) ;
#ifndef S4CB51
   S4CONST char * e4text( const int ) ;
#endif

#ifdef E4FILE_LINE
   extern const char *s4fileName ;
   extern int s4lineNo ;
#endif /* E4FILE_LINE */
S4EXPORT int S4FUNCTION code4lineNo( void ) ;
S4EXPORT const char *S4FUNCTION code4fileName( void ) ;
S4EXPORT void S4FUNCTION code4lineNoSet( int ) ;
S4EXPORT void S4FUNCTION code4fileNameSet( const char * ) ;

unsigned file4readLow( FILE4 *, FILE4LONG, void *, const unsigned ) ;
int file4writeLow( FILE4 *, FILE4LONG, const void *, unsigned, const int, const int, const int ) ;
// AS May 24/02 - created file4createInternal for internal use to indicate file types
int file4tempLow( FILE4 *, CODE4 *, const int, int, const char *, char ) ;
int file4changeSize( FILE4 *, FILE4LONG ) ;

#ifdef S4WRITE_DELAY
   int file4writeDelay( FILE4 *, FILE4LONG, const void *, const unsigned, S4DELAY_FUNCTION *, void * ) ;
   int file4writeDelayFlush( FILE4 *, const int ) ;
   #ifndef S4OFF_OPTIMIZE
      int file4writeOpt( FILE4 *, unsigned long, const void *, const unsigned, int doDelay, S4DELAY_FUNCTION *, void * ) ;
   #endif
   #ifdef S4USE_INT_DELAY
      int _cdecl file4writeDelayMain( void * ) ;
   #else
      void _cdecl file4writeDelayMain( void * ) ;
   #endif
#else
   #ifndef S4OFF_OPTIMIZE
      int file4writeOpt( FILE4 *, unsigned long, const void *, const unsigned, int doDelay, void *, void * ) ;
   #endif
#endif /* S4WRITE_DELAY */

#ifdef S4READ_ADVANCE
   S4EXPORT int S4FUNCTION file4advanceRead( FILE4 *, unsigned, void *, const unsigned, S4ADVANCE_FUNCTION *, void * ) ;
   void file4advanceReadWriteOver( FILE4 *, unsigned long, const unsigned, const void *, const int ) ;
   int file4advanceCancel( FILE4 * ) ;
   void opt4advanceReadBuf( FILE4 *, unsigned long, unsigned ) ;
   #ifdef S4USE_INT_DELAY
      int _cdecl file4advanceReadMain( void * ) ;
   #else
      void _cdecl file4advanceReadMain( void * ) ;
   #endif
#endif /* S4READ_ADVANCE */

S4EXPORT int S4FUNCTION f4memoWritePart( FIELD4 *field, char *dataToWrite, unsigned int dataLen, long memoLen, long offset ) ;
S4EXPORT int S4FUNCTION f4memoWriteFinish( DATA4 *data ) ;
S4EXPORT unsigned int S4FUNCTION f4memoReadPart( FIELD4 S4PTR *, char S4PTR *, unsigned int, unsigned int ) ;
#ifndef S4OFF_MEMO
   S4EXPORT void S4FUNCTION d4memoResetExport( DATA4 S4PTR *data ) ;
   S4EXPORT int S4FUNCTION f4memoResetExport( FIELD4 S4PTR *field ) ;
   int f4memoFlush( FIELD4 * ) ;
   int f4memoRead( FIELD4 * ) ;       /* Validates memo id's first */
   int f4memoReadLow( FIELD4 * ) ;   /* Assumes the current memo id is valid */
   int f4memoReset( FIELD4 * ) ;      /* Resets to 'Unknown state' */
   int f4memoUpdate( FIELD4 * ) ;
   int f4memoWrite( FIELD4 * ) ;
#endif

#ifndef S4OFF_WRITE
   S4EXPORT int S4FUNCTION f4memoSetLen( FIELD4 *, const unsigned int ) ; /* need to export for ++ API */
#endif

#ifndef S4OFF_INDEX
   #ifndef S4CLIPPER
      INDEX4 * index4create( DATA4FILE *, const char *, const TAG4INFO * ) ; /* 0 name -> productn */
      INDEX4FILE * index4open( DATA4 *, const char *, INDEX4 * ) ;
      int index4close( INDEX4FILE * ) ;
   #else
      void index4swapBlockClipper(void *, int, int ) ;
   #endif
    //#ifdef OLEDB5BUILD
      S4EXPORT int S4FUNCTION tfile4versionCheckFree( TAG4FILE * ) ;
    //#endif

   #ifdef S4CLIENT
      int i4setup( CODE4 *, DATA4 *, const char *, const char *, int, INDEX4 * ) ;
   #else
      S4EXPORT int S4FUNCTION tfile4versionCheck( TAG4FILE S4PTR *, const int, const int ) ;
      int i4check( INDEX4 * ) ;
      void i4deleteRemoveKeys( INDEX4 * ) ;
      // AS Jan 31/03 - Function only available in this instance.
      #if !defined( S4OFF_WRITE ) && defined( S4CLIPPER )
         int i4flush( INDEX4 * ) ;
      #endif
      int i4readBlock( FILE4 *, const B4NODE, B4BLOCK *, B4BLOCK * ) ;
      int i4shrink( INDEX4 *, long ) ;  /* Returns a block of disk space */
      int i4unlock( INDEX4 * ) ;
      int i4update( INDEX4 * ) ;
      int i4updateHeader( INDEX4 * ) ;
      S4EXPORT int S4FUNCTION i4versionCheck( INDEX4 S4PTR *, const int, const int ) ;
      #ifndef S4OFF_TRAN
         TAG4KEY_REMOVED *t4keyFind( TAG4 *, unsigned long, char * ) ;
      #endif
      #ifdef S4CLIPPER
         int i4setup( CODE4 *, DATA4 *, const char *, int ) ;
         int tfile4lockTest( TAG4FILE * ) ;
         int i4lock( INDEX4 * ) ;
         #define t4versionCheck( t4, a, b ) ( tfile4versionCheck( (t4)->tagFile, (a), (b) ) )
      #else
         int index4flush( INDEX4FILE * ) ;
         int index4lock( INDEX4FILE *, const long ) ;
         S4EXPORT int S4FUNCTION index4isProduction( INDEX4FILE * ) ;
         #ifdef S4OFF_MULTI
            #define index4lockTest( i4 ) (1)
         #else
            #define index4lockTest( i4 ) (((i4)->file.lowAccessMode != OPEN4DENY_NONE ) ? 1 : ( (i4)->fileLocked ? 1 : 0 ) )
         #endif
         int index4shrink( INDEX4FILE *, B4NODE ) ;  /* Returns a block of disk space */
         int index4unlock( INDEX4FILE *, const long ) ;
         int index4update( INDEX4FILE * ) ;
         int index4updateHeader( INDEX4FILE * ) ;
         int index4versionCheck( INDEX4FILE *, const int ) ;
         B4NODE index4extend( INDEX4FILE * ) ;   /* Allocates a block at the end of the file */
         #ifdef S4OFF_OPTIMIZE
            #define t4versionCheck( t4, doSeek, updateVersion ) (i4versionCheck( (t4)->index, doSeek, updateVersion ) )
         #else
            #define t4versionCheck( t4, doSeek, updateVersion ) (( (t4)->index->indexFile->file.doBuffer == 0 ) ? i4versionCheck( (t4)->index, doSeek, updateVersion ) : 0)
         #endif
      #endif  /* S4CLIPPER */
   #endif /* S4CLIENT */
#endif /* S4OFF_INDEX */
#ifndef S4CLIENT
   #ifndef S4OFF_MULTI
      int lock4groupVerify( LOCK4GROUP *, const int ) ;
      int lock4groupLock( LOCK4GROUP * ) ;
      int lock4groupUnlock( LOCK4GROUP * ) ;
   #endif /* S4OFF_MULTI */
#endif /* S4CLIENT */

S4EXPORT int S4FUNCTION l4check( const LIST4 S4PTR * ) ;
S4EXPORT int S4FUNCTION l4seek( const LIST4 S4PTR *, const void S4PTR * ) ;

int l4lockCheck( void ) ;
void l4lockRemove( int, long, long ) ;
void l4lockSave( int, long, long ) ;

S4EXPORT void S4FUNCTION mem4init( void ) ;
S4EXPORT int S4FUNCTION mem4reset( void ) ;
#ifdef S4MEM_DBF
   INDEX4 *mem4index( void ) ;
#endif
#ifdef S4SEMAPHORE
   #ifdef S4WIN32
      /* AS 11/19/98 - Borland C++ gives compiler error because EnterCriticalSection returns void... */
      /*   #define mem4start(c4) ( ( memoryInitialized == 0 ) ? -1 : EnterCriticalSection( &critical4memory ) ) */
      /*   #define mem4stop(c4)  ( ( memoryInitialized == 0 ) ?  0 : LeaveCriticalSection( &critical4memory ) ) */
      #ifdef E4ANALYZE
         int mem4start( CODE4 * ) ;
         int mem4stop( CODE4 * ) ;
      #else
         #define mem4start(c4) ( ( memoryInitialized == 0 ) ? -1 : (EnterCriticalSection( &critical4memory ), 0 ) )
         #define mem4stop(c4)  ( ( memoryInitialized == 0 ) ?  0 : (LeaveCriticalSection( &critical4memory ), 0 ) )
      #endif
   #endif
   #ifdef S4LINUX  /* LY 2002/02/20 */
      /* LY 2003/07/09 : too many params to functions */
      int S4FUNCTION mem4start( CODE4* ) ;
      int S4FUNCTION mem4stop( CODE4* ) ;
   #endif
#endif
#define mem4alloc( a ) ( mem4allocZero( a ) )
#define mem4allocErr( a, b ) ( mem4allocErrZero( (a), (b) ) )
#ifdef S4MEM_PRINT
   // AS 11/10/00 - need to have critical sections around code so that the mem4 file/line globals don't get
   // trounced by other threads calling these functions
   void *S4FUNCTION mem4printAllocZero( MEM4 *memoryType, const char *file, int line ) ;
   #define mem4allocZero( a ) mem4printAllocZero( (a), __FILE__, __LINE__ )

   void *mem4printAllocErrZero( MEM4 *memoryType, CODE4 *c4, const char *file, int line ) ;
   #define mem4allocErrZero( a, b ) mem4printAllocErrZero( (a), (b), __FILE__, __LINE__ )

   void *mem4printAllocNoZero( MEM4 *memoryType, const char *file, int line ) ;
   #define mem4allocNoZero( a ) mem4printAllocNoZero( (a), __FILE__, __LINE__ )

   void * mem4printAllocErrNoZero( MEM4 *memoryType, CODE4 *c4, const char *file, int line ) ;
   #define mem4allocErrNoZero( a, b ) mem4printAllocErrNoZero( (a), (b), __FILE__, __LINE__ )

   Y4CHUNK *mem4printAllocChunk( MEM4 *typePtr, const char *file, int line ) ;
   #define mem4allocChunk( a ) mem4printAllocChunk( (a), __FILE__, __LINE__ )

   int S4FUNCTION mem4printFree( MEM4 *memoryType, void *freePtr, const char *file, int line ) ;
   #define mem4free( a, b ) ( mem4printFree( (a), (b), __FILE__, __LINE__ ), (b) = 0 )
#else
   #define mem4allocZero( a ) mem4allocDefault( a, 1 )
   #define mem4allocErrZero( a, b ) mem4allocErrDefault( a, b, 1 )
   #define mem4allocNoZero( a ) mem4allocDefault( a, 0 )
   #define mem4allocErrNoZero( a, b ) mem4allocErrDefault( a, b, 0 )
   #define mem4allocChunk( a ) mem4allocChunkDefault( a )
   #define mem4free( a, b ) ( mem4freeDefault( a, b ), b = 0 )
#endif
#define mem4createAlloc( a, b, c, d, e, f ) mem4createAllocZero( a, b, c, d, e, f )
#ifdef S4MEM_PRINT
   S4EXPORT MEM4 S4PTR *S4FUNCTION mem4printCreate( CODE4 S4PTR *, int, const unsigned int, int, const int, const char *file, int line ) ;
   #define mem4create( a, b, c, d, e ) mem4printCreate( (a), (b), (c), (d), (e), __FILE__, __LINE__ )

   S4EXPORT void S4PTR *S4FUNCTION mem4printCreateAlloc( CODE4 S4PTR *, MEM4 S4PTR * S4PTR *, int, const unsigned int, int, const int, int doZero, const char *file, int line ) ;
   #define mem4createAllocZero( a, b, c, d, e, f ) mem4printCreateAlloc( (a), (b), (c), (d), (e), (f), 1, __FILE__, __LINE__ )
   #define mem4createAllocNoZero( a, b, c, d, e, f ) mem4printCreateAlloc( (a), (b), (c), (d), (e), (f), 0, __FILE__, __LINE__ )
#else
   #define mem4create( a, b, c, d, e ) mem4createDefault( a, b, c, d, e )
   #define mem4createAllocZero( a, b, c, d, e, f ) mem4createAllocDefault( a, b, c, d, e, f, 1 )
   #define mem4createAllocNoZero( a, b, c, d, e, f ) mem4createAllocDefault( a, b, c, d, e, f, 0 )
#endif

// AS Jun 20/02 - compression and decompression run-time linkage functionality if available...
int c4compress( CODE4 *c4, void *outputBuf, unsigned long *outLen, const void *inputBuf, unsigned long inputLen, int level, Bool5 ) ;
int c4uncompress( CODE4 *c4, void *outputBuf, unsigned long *outLen, const void *inputBuf, unsigned long inputLen ) ;
#ifdef S4TESTING
   // AS Jun 20/02 do regular test without 'special options'
   S4EXPORT void S4FUNCTION c4testSetSpecial( Bool5 val ) ;
#endif
S4EXPORT int S4FUNCTION code4memoCompress( CODE4 *c4, short flag ) ;
// AS Nov 26/02 - New function for data file compression
S4EXPORT DATA4 * S4FUNCTION d4compress( DATA4 S4PTR *data, const char S4PTR *compressedName, short blockSize ) ;
#if defined( S4STAND_ALONE ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
   int file4compressInit( FILE4 *file, COMPRESS4DATA *compress, long compressHeaderOffset ) ;
   void file4compressInitUndo( FILE4 *file ) ;
   int file4copy( FILE4 *to, FILE4 *from ) ;
#endif
#ifndef S4OFF_MEMO
   #ifndef S4CLIENT
      int memo4fileCheck( MEMO4FILE * ) ;
      int memo4fileCreate( MEMO4FILE *, CODE4 *, DATA4FILE *, const char * );
      int memo4fileOpen( MEMO4FILE *, DATA4FILE *, char * ) ;
      // AS Jun 25/02 - new paramater for potentially compressed memo fields
      int memo4fileReadPart( MEMO4FILE *, long , char * *, unsigned int *, unsigned long, const unsigned int, long *, Bool5 ) ;
      int memo4fileWrite( MEMO4FILE *, long *, const char *, const unsigned int ) ;
      int memo4fileWritePart( MEMO4FILE *, long *, const char *, const long, const long, const unsigned int, const long ) ;
      #ifdef S4MFOX
         int memo4fileDump( MEMO4FILE *, const long, const char *, const unsigned int, const long, const long ) ;
         #define memo4fileRead( m4, id, pp, pl, tp ) memo4fileReadPart( (m4), (id), (pp), (pl), (0L), (UINT_MAX - 100), (tp), (1) )
      #else
         int memo4fileDump( MEMO4FILE *, const long, const char *, const unsigned int, const long ) ;
         int memo4fileRead( MEMO4FILE *, long, char **, unsigned int * ) ;
         #ifndef S4MNDX
            int memo4fileChainFlush( MEMO4FILE *, MEMO4CHAIN_ENTRY * ) ;
            int memo4fileChainSkip( MEMO4FILE *, MEMO4CHAIN_ENTRY * ) ;
            #ifndef S4SERVER
               S4EXPORT int S4FUNCTION f4memoCheck( MEMO4FILE S4PTR *, DATA4 S4PTR * ) ;
            #endif
         #endif /* S4MNDX  */
      #endif /* S4MFOX  */
   #endif /* S4CLIENT */

   int memo4fileLock( MEMO4FILE * ) ;
   int memo4fileUnlock( MEMO4FILE * ) ;
   // AS 09/06/99 --> update to unsigned long for large files...
   unsigned long memo4lenPart( MEMO4FILE *, long ) ;
   // AS Dec 4/02 - capability to write memos on boundary
   #ifdef S4PREPROCESS_FILE
      int memo4fileWriteBoundary( MEMO4FILE *f4memo, const char *ptr, const unsigned len, FILE4LONG pos ) ;
   #endif
#endif /* S4OFF_MEMO */

#ifndef S4OFF_INDEX
   #ifdef S4SERVER
      #define t4uniqueSetLow( t4, val, dum ) ( (t4)->errUnique = ( val ) )
   #else
      S4EXPORT int S4FUNCTION t4uniqueSetLow( TAG4 *, const short, const char ) ;
   #endif

   #ifdef S4CLIENT
      void tfile4free( TAG4FILE * ) ;
      void t4free( TAG4 *, Bool5 ) ;
   #endif
   #ifndef S4CLIENT
      S4EXPORT void S4FUNCTION tfile4descending( TAG4FILE *, const unsigned short int ) ;
      int t4addCalc( TAG4 *, long ) ; /* Calculates expression and adds */
      #define tfile4alias( t4 ) ( (t4)->alias )
      S4EXPORT int S4FUNCTION tfile4bottom( TAG4FILE * ) ;
      int t4check( TAG4 * ) ;
      #if !defined( OLEDB5BUILD ) || defined( S4JOINT_OLEDB_DLL )
         #ifdef S4HAS_DESCENDING
            #define tfile4dskip( t4, num ) (((t4)->header.descending ) ? (-tfile4skip( (t4), -(num) )) : ( tfile4skip( (t4), (num))))
         #else
            #define tfile4dskip( t4, num ) ( tfile4skip( (t4), (num) ) )
         #endif
      #else
         #define tfile4dskip( t4, num ) ( tfile4dskipExport( (t4), (num) ) )
      #endif
      S4EXPORT long S4FUNCTION tfile4dskipExport( TAG4FILE S4PTR *, long ) ;
      int tfile4down( TAG4FILE * ) ;
      int tfile4dump( TAG4FILE *, int, const int ) ;
      S4EXPORT int S4FUNCTION tfile4empty( TAG4FILE S4PTR * ) ;  // exported for ODBC
      // AS 10/13/00 - exportable tfile4exprKey... for ODBC
      S4EXPORT int S4FUNCTION tfile4exprKeyExport( TAG4FILE S4PTR *, unsigned char S4PTR * S4PTR * ) ;

      #ifdef S4CLIPPER
         S4EXPORT int S4FUNCTION tfile4exprKey( TAG4FILE S4PTR *, unsigned char S4PTR * S4PTR * ) ;
      #else
         #ifdef E4PARM_LOW
            S4EXPORT int S4FUNCTION tfile4exprKey( TAG4FILE S4PTR *, unsigned char S4PTR * S4PTR * ) ;
         #else
            #define tfile4exprKey( tag, ptrPtr ) ( expr4key( (tag)->expr, (char **)(ptrPtr), (tag) ) )
         #endif
      #endif
      int tfile4freeAll( TAG4FILE * ) ;
      int tfile4freeSaved( TAG4FILE * ) ;
      S4EXPORT int S4FUNCTION tfile4go( TAG4FILE *, const unsigned char *, const unsigned long, const int ) ;
      int tfile4goEof( TAG4FILE * ) ;
      int tfile4go2( TAG4FILE *, const unsigned char *, const unsigned long, const int ) ;
      int tfile4initSeekConv( TAG4FILE *, int ) ;    /* Initialize 'stok' and 'dtok' */
      S4EXPORT char * S4FUNCTION t4getKey( TAG4 S4PTR * ) ;
      S4EXPORT char * S4FUNCTION tfile4key( TAG4FILE * ) ;
      B4KEY_DATA *tfile4keyData( TAG4FILE * ) ;              /* The current key */
      int tfile4outOfDate( TAG4FILE * ) ;
      int tfile4position2( TAG4FILE *, double * ) ;
      double tfile4position( TAG4FILE * ) ;              /* Returns the position as a percent */
      double tfile4positionDbl( TAG4FILE * ) ;              /* Returns the position as a percent */
      int tfile4positionSet( TAG4FILE *, const double ) ;  /* Positions a percentage */
      #ifdef E4ANALYZE
         S4EXPORT B4BLOCK * S4FUNCTION tfile4block( TAG4FILE * ) ;
      #else
         #define tfile4block( t4 ) ( (B4BLOCK *)(t4)->blocks.lastNode )
      #endif
      S4EXPORT unsigned long S4FUNCTION tfile4recNo( TAG4FILE * ) ;
      S4EXPORT int S4FUNCTION tfile4eof( TAG4FILE S4PTR *t4 ) ;
      int tfile4removeCurrent( TAG4FILE * ) ;        /* Remove the current key */
      int tfile4remove( TAG4FILE *, const unsigned char *, const unsigned long ) ;  /* Remove specified key */
      int tfile4removeCalc( TAG4FILE *, unsigned long ) ; /* Calculates expression and removes */
      int tfile4rlBottom( TAG4FILE * ) ;
      int tfile4rlTop( TAG4FILE * ) ;
      S4EXPORT int S4FUNCTION tfile4seek( TAG4FILE *, const void *, const int ) ;    /* r4success, r4found, r4after, r4eof */
      #ifdef S4SERVER
         #ifdef S4FOX
            #define code4collateName( c4 ) ( (c4)->collateName )
            #define code4collateNameUnicode( c4 ) ( (c4)->collateNameUnicode )
         #else
            #define code4collateName( c4 ) ( collate4none )
            #define code4collateNameUnicode( c4 ) ( collate4none )
         #endif
      #else
         S4EXPORT enum Collate4name S4FUNCTION code4collateName( CODE4 * ) ;
         S4EXPORT enum Collate4name S4FUNCTION code4collateNameUnicode( CODE4 * ) ;
      #endif
      S4EXPORT void S4FUNCTION code4collateNameSet( CODE4 *, enum Collate4name ) ;
      S4EXPORT void S4FUNCTION code4collateNameUnicodeSet( CODE4 *, enum Collate4name ) ;
      #ifdef S4FOX
         /* AS 07/27/99 -> support for generic collating for Unicode and character fields... */
         // #ifdef S4VFP_KEY
         int collate4setupReadFromDisk( CODE4 *c4, enum Collate4name collateName ) ;
         void collate4setupUnicodeFromChar( COLLATE4 * ) ;
         void collate4initUndo() ;
         #define tfile4vfpKey( tag ) ( ( collation4get( (tag)->collateName )->collateType != collate4machineByteOrder ) )
         // #endif
         int tfile4setCollatingSeq( TAG4FILE *, enum Collate4name collateName, Bool5 setHeader ) ;
         // int tfile4setCodePage( TAG4FILE *, const int ) ;
      #endif
      S4EXPORT COLLATE4 * S4FUNCTION collation4getExport( enum Collate4name ) ;
      S4EXPORT enum Collate4name S4FUNCTION t4getCollateName( TAG4 *tag ) ;
      S4EXPORT long S4FUNCTION tfile4skip( TAG4FILE *, long ) ;
      S4EXPORT int S4FUNCTION tfile4top( TAG4FILE * ) ;
      int tfile4type( TAG4FILE * ) ;
      int tfile4up( TAG4FILE * ) ;
      int tfile4update( TAG4FILE * ) ;
      int tfile4upToRoot( TAG4FILE * ) ;
      S4EXPORT int S4FUNCTION t4reindex( TAG4 S4PTR * ) ;
      #ifdef S4CLIPPER
         int tfile4add( TAG4FILE *, unsigned char *, const unsigned long, short int ) ;  /* Returns r4unique, r4success, r4repeat */
         int tfile4balance( TAG4FILE *, B4BLOCK *, int ) ;
         B4BLOCK *tfile4split( TAG4FILE *, B4BLOCK *, const int ) ;
         int tfile4flush( TAG4FILE * ) ;
      #else
         int tfile4addDo( TAG4FILE *, const unsigned char *, unsigned long, short int ) ;
         #ifdef S4FOX
            int tfile4add( TAG4FILE *, const unsigned char *, const unsigned long, short int ) ;  /* Returns r4unique, r4success, r4repeat */
         #else
            #define tfile4add( t4, k, r, e ) ( tfile4addDo( (t4), (k), (r), (e) ) )
         #endif
         B4BLOCK *tfile4split( TAG4FILE *, B4BLOCK * ) ;
      #endif
   #endif /* S4CLIENT */
#endif /* S4OFF_INDEX */

#if !defined( S4CLIENT ) && (defined( E4ANALYZE ) || defined( S4TESTING ))
   // AS Jun 24/02 - function to perform data movement counting for testing
   S4EXPORT void S4FUNCTION code4countPosReset( CODE4 *c4 ) ;  // reset count to 0
   S4EXPORT unsigned long S4FUNCTION code4countPosGet( CODE4 *c4 ) ;  // return current count
#endif

#ifdef S4TESTING
   #ifdef S4WINTEL
      S4EXPORT void S4FUNCTION u4terminate( void );
   #endif
   #ifndef S4SERVER
      void u4setField( const char *, const char *, const long, const char *, const char * ) ;
      int ats4readInfo( char *, void *, const unsigned int ) ;
      S4EXPORT void S4FUNCTION ats4setSuiteStatus( const char * ) ;
   #endif
   S4EXPORT void S4FUNCTION u4writeErr( const char S4PTR *, int ) ;
   void u4writeOut( char *, int, long  ) ;
#else
   #ifdef S4TRACK_FILES
      void u4writeErr( const char *, int ) ;
   #else
      S4EXPORT void S4FUNCTION u4writeErr( const char S4PTR *, int ) ;
   #endif
#endif

#ifdef S4MAX
   long u4allocated( void ) ;
   long u4max( void ) ;
#else
   #ifdef S4SERVER
      long u4allocated( void ) ;
      unsigned short u4openFiles( void ) ;
   #endif
#endif

#ifdef S4MEM_PRINT
   extern const char *m4fileName ;
   extern int m4lineNo ;

   #ifdef S4CONSOLE
      #define code4memLineNoSet( val ) ( m4lineNo = val )
      #define code4memFileNameSet( val ) ( m4fileName = val )
   #else
      S4EXPORT void S4FUNCTION code4memLineNoSet( int ) ;
      S4EXPORT void S4FUNCTION code4memFileNameSet( const char S4PTR * ) ;
   #endif
#endif

S4EXPORT void S4FUNCTION u4delayHundredth( const unsigned int ) ;  /* exported for testing */
#ifndef S4INLINE /* CS 2000/01/05 macro by same name defined in D4INLINE.H */
   void u4delaySec( void ) ;
#endif
S4EXPORT long S4FUNCTION u4getMaxIndexSize() ;
#if !defined( S4WINCE ) && !defined( S4PALM )
   char *u4environ( char *, const int ) ;
#endif
#if   defined( S4WINTEL )
   FILE4LONG u4filelength( HANDLE ) ;
#elif defined( S4PALM )
   FILE4LONG u4filelength( FileHand ) ;
#else
   FILE4LONG u4filelength( int ) ;
#endif
S4EXPORT void S4FUNCTION u4nameMake( char *, const int, const char *, const char *, const char * ) ;
S4EXPORT void S4FUNCTION u4nameMakeFindDrive( char *, const int, const char *, const char * ) ;
S4EXPORT const char * S4FUNCTION u4nameFindFileName( const char S4PTR *, int ) ;
S4EXPORT int S4FUNCTION u4namePath( char S4PTR *, const unsigned int, const char S4PTR * ) ;
S4EXPORT int S4FUNCTION u4namePathSpecial( char S4PTR *, const unsigned int, const char S4PTR * ) ;
int u4nameRetExt( char *, const int, const char * ) ;
#ifdef S4WINTEL
   S4EXPORT void S4FUNCTION u4nameReplaceFwdSlash( char *buf, int len ) ;
#endif
#ifndef S4NO_RENAME
   S4EXPORT int S4FUNCTION u4rename( const char *, const char * ) ;
#endif

#define u4allocErr( a, b ) ( u4allocEr( (a), (b) ) )

#ifdef S4MEM_PRINT
   S4EXPORT void S4PTR *S4FUNCTION u4printAlloc( long, const char *file, int line ) ;
   #define u4alloc( a ) u4printAlloc( (a), __FILE__, __LINE__ )

   S4EXPORT void S4PTR *S4FUNCTION u4printAllocEr( CODE4 S4PTR *, long, const char *file, int line ) ;
   #define u4allocEr( a, b ) u4printAllocEr( (a), (b), __FILE__, __LINE__ )

   S4EXPORT void S4PTR *S4FUNCTION u4printAllocFree( CODE4 S4PTR *, long, const char *file, int line ) ;
   #define u4allocFree( a, b ) u4printAllocFree( (a), (b), __FILE__, __LINE__ )

   S4EXPORT int S4FUNCTION u4printAllocAgain( CODE4 S4PTR *, char S4PTR * S4PTR *, unsigned int S4PTR *, const unsigned int, const char *file, int line ) ;
   #define u4allocAgain( a, b, c, d )  u4printAllocAgain( (a), (b), (c), (d), __FILE__, __LINE__ )

   S4EXPORT int S4FUNCTION u4printFree( void S4PTR *, const char *file, int line ) ;
   #define u4free( a ) u4printFree( (a), __FILE__, __LINE__ )
#else
   #define u4alloc( a ) ( u4allocDefault( a ) )
   #define u4allocEr( a, b ) ( u4allocErDefault( a, b ) )
   #define u4allocFree( a, b ) ( u4allocFreeDefault( a, b ) )
   #define u4allocAgain( a, b, c, d ) ( u4allocAgainDefault( a, b, c, d ) )
   #define u4free( a ) ( u4freeDefault( a ), a = 0 )
#endif

/* MISC */
S4EXPORT short S4FUNCTION x4reverseShort( const void S4PTR * ) ;  /* exported for communications DLL */
S4EXPORT S4LONG S4FUNCTION x4reverseLong( const void S4PTR * ) ;  /* exported for OLEDB */
#ifndef S4NO_LONGLONG
   S4EXPORT LONGLONG S4FUNCTION x4reverseLongLong( const void S4PTR * ) ;  /* exported for OLEDB */
#endif
double S4FUNCTION x4reverseDouble( const void S4PTR * ) ;
unsigned char S4FUNCTION x4reverseByte( const void S4PTR * ) ;  /* LY 2001/07/26 */

#ifndef S4FOX
   #ifndef S4CLIPPER
      int S4CALL c4bcdCmp( S4CMP_PARM, S4CMP_PARM, size_t ) ;
      int S4CALL t4cmpDoub( S4CMP_PARM, S4CMP_PARM, size_t ) ;
   #endif
#endif

#ifdef E4ANALYZE_ALL
   int file4writePart( const void *, FILE4 *, unsigned long, unsigned ) ;
   int file4cmpPart( CODE4 *, void *, FILE4 *, unsigned long, unsigned ) ;
   int file4cmp( FILE4 * ) ;
   int file4partLenSet( FILE4 *, unsigned long ) ;
#endif

/* VISUAL BASIC */
#if defined(E4VBASIC) && !defined(S4SERVER)
   S4EXPORT int c4parm_check( const void S4PTR *, int, const long ) ;
#endif

/* AS Aug 26/02 - no longer used...
// #if defined( S4SERVER ) && ( defined( S4ODBC_BUILD ) || defined( S4ODBC_ENABLED ) )
//    int mutex4initShare( MUTEX4 *mutex, const char *mutexName ) ;
//    void mutex4initUndo( MUTEX4 *mutex ) ;
//    #define mutex4release( m4 ) ( ( !ReleaseMutex( (m4)->mutex ) ) ? error4( 0, e4result, E96928 ) : 0 )
//    #define mutex4wait( l4 ) ( ( (l4)->isValid == 0 ) ? error4( 0, e4parm, E96931 ) :\
//         ( ( WaitForSingleObject( (l4)->mutex, INFINITE ) == WAIT_FAILED ) ?  error4( 0, e4result, E96931 ) : 0 ) )
// #endif*/

#ifndef S4OFF_THREAD
   S4EXPORT int S4FUNCTION semaphore4init( SEMAPHORE4 S4PTR * ) ;
   S4EXPORT int S4FUNCTION semaphore4initShared( SEMAPHORE4 S4PTR *, const char S4PTR * ) ;
   S4EXPORT int S4FUNCTION semaphore4initUndo( SEMAPHORE4 S4PTR * ) ;
   // AS 09/10/99 --> causing slow-down in client...
   // S4EXPORT void S4FUNCTION semaphore4release( SEMAPHORE4 * ) ;
   // S4EXPORT int S4FUNCTION semaphore4wait( SEMAPHORE4 *, int ) ;
   #define semaphore4wait( s4, waitSecs ) ( WaitForSingleObject( (s4)->handle, (waitSecs) < 0 ? INFINITE : (waitSecs)*1000 ) == WAIT_OBJECT_0 ? 1 : 0 )
   #define semaphore4release( s4 )   ( ReleaseSemaphore( (s4)->handle, 1, 0 ) == FALSE ? error4( 0, e4semaphore, E96936 ) : 0 )

#endif

#ifdef TIMER5OUT
   S4EXPORT void S4FUNCTION code4timerStart( CODE4 *, char * ) ;
   S4EXPORT void S4FUNCTION code4timerStop( CODE4 * ) ;
#endif /* TIMER5OUT */

#if defined( S4SERVER ) && ( defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD ) )
   // currently only odbc / server uses shared memory...
   void share4initUndo( SHARE4 *share ) ;
   SHARE4 *share4init( CODE4 *c4, const char *name, int size ) ;
   int share4putData( SHARE4 *share, int offset, void *buffer, int len ) ;
   int share4getData( SHARE4 *share, int offset, void *buffer, int len ) ;
#endif

S4EXPORT void S4FUNCTION t4dblToFoxExport( char *, const double ) ;  // CS 2000/08/09 for calling from 'C' tests

#ifdef S4ENCRYPT_DLL
   // AS Jan 23/03 - Encryption enabling functions generally available, may return e4notSupported
   S4EXPORT int S4FUNCTION code4encryptInit( struct CODE4St *c4, const void *key, short keyLen ) ;
   S4EXPORT int S4FUNCTION code4encryptFile( struct CODE4St *, short flag ) ;
#endif

#ifdef __cplusplus
   }
#endif

/* The following functions are NOT included in the extern "C" section
   for a reason. They called by indexing functions by pointer. In
   order for this to work, the functions much not be extern "C". */

#if ( defined( S4LANGUAGE ) && ( ( defined( S4GERMAN ) && !defined( S4FOX ) ) || ( !defined( S4GERMAN ) ) ) ) || defined( S4ANSI )
   int S4CALL u4memcmp( S4CMP_PARM, S4CMP_PARM, size_t ) ;
#endif

void t4noChangeStr( COLLATE4 *, char *, const char *, const int, int * ) ;
void c4bcdFromA( COLLATE4 *, char *, const char *, const int, int * ) ; /* Removed from S4MDX to facilitate index independent OLEDB */

#ifdef S4MDX
   void t4strToDateMdx( COLLATE4 *, char *, const char *, const int, int * ) ;
   void c4bcdFromD( char *, const double ) ;
   void t4noChangeDouble( char *, const double ) ;
#endif

void t4dblToDbDate( char *result, const double d ) ;
void t4dtstrToDbDate( COLLATE4 *, char *result, const char *inputPtr, const int inputPtrLen, int * ) ;
void t4unicodeToMachine( COLLATE4 *collate, char *output, const char *input, const int numBytes, int * ) ;
void t4dblToFox( char *, const double ) ;
void t4shortToFox( char *result, const short *val ) ;
#if !defined( S4CLIENT ) && defined( S4FOX )
   void t4strToCur( COLLATE4 *, char *, const char *, const int, int * ) ;
   void t4strToDbTimeStamp( COLLATE4 *, char *result, const char *input, const int len, int * ) ;
   void t4strToUnsignedInt( COLLATE4 *, char *result, const char *input, const int len, int * ) ;
   void t4dblToUnsignedInt( char *result, const double d ) ;
   void t4dblToInt( char *result, const double d ) ;
#endif /* !defined( S4CLIENT ) && defined( S4FOX ) */
#if !defined( S4CLIENT ) && ( defined( S4FOX ) || ( defined( OLEDB5BUILD ) && !defined( S4SERVER ) ) )
   void t4strToDateTime( COLLATE4 *, char *result, const char *input, const int len, int * ) ;
   void t4strToInt( COLLATE4 *, char *result, const char *input, const int len, int * ) ;
#endif


#if defined( S4FOX ) || defined( S4CLIENT ) || ( defined( OLEDB5BUILD ) && !defined( S4SERVER ) )
   void t4curToFox( char *, const CURRENCY4 * ) ;
   void t4intToFox( char *, const S4LONG * ) ;  /* LY 2001/07/28 : changed from long* to S4LONG* for 64-bit */
   void t4unsignedIntToFox( char *, const unsigned long * ) ;
   void t4foxToUnsignedInt( char *result, const unsigned long *val ) ;
   void t4dblToCurFox( char *, const double ) ;
   void t4dblToCur( char *, const double ) ;
   /* LY 2001/07/28 : changed from long* for 64-bit */
   void t4dateTimeToFox( char *, const S4LONG * ) ;
#endif
#if !defined( S4NO_LONGLONG) && ( defined( S4FOX ) || defined( OLEDB5BUILD ) || defined( S4CLIENT ) )
   void t4i8ToFox( char *, const LONGLONG * ) ;
   void t4foxToI8( char *, const char * ) ;
#endif
/* LY 4/2x/99 */
#ifdef S4MACINTOSH
   void t4i8ToFox( char *, const long long * ) ;
   /* LY 00/01/26 : for newer CodeWarrior compilers */
   #if (__MWERKS__ > 0x2000)
      #define S4CTOPSTR(val) C2PStr(val)
   #else
      #define S4CTOPSTR(val) CtoPstr(val)
   #endif
#endif

void t4foxToDbl( char *result, const char *from ) ;
void t4foxToDateTime( long *result, const char *input ) ;
void t4foxToDbDate( char *, const char  * ) ;
void t4foxToDbTime( char *, const char  * ) ;
void t4foxToDbTimeStamp( char *, const char  * ) ;
void t4dbDateToFox( char *, const DBDATE * ) ;
void t4dbTimeToFox( char *, const DBTIME * ) ;
void t4dbTimeStampToFox( char *, const DBTIMESTAMP * ) ;
S4EXPORT TAG4FILE S4PTR * S4FUNCTION t4tagFile( TAG4 S4PTR * ) ;
S4EXPORT int S4FUNCTION t4infoN( TAG4 S4PTR * ) ;
S4EXPORT void S4FUNCTION t4exprContextSet( TAG4 S4PTR *, DATA4 S4PTR * ) ;
S4EXPORT FIELD4 S4PTR * S4FUNCTION t4field( TAG4 S4PTR * ) ;
S4EXPORT int S4FUNCTION tfile4exprNullLow( TAG4FILE S4PTR * ) ;


/* AS 07/27/99 -> support for generic collating for Unicode and character fields... */
// S4EXPORT int S4CALL u4keycmp( S4CMP_PARM, S4CMP_PARM, size_t, size_t, size_t, T4VFP * ) ;
int u4keycmpPartial( S4CMP_PARM dataPtr, S4CMP_PARM searchPtr, size_t sLen, size_t dLen, size_t trailCnt, COLLATE4 *collate, const char *initialValue, int initialLen ) ;
S4EXPORT int S4CALL u4keycmp( S4CMP_PARM, S4CMP_PARM, size_t, size_t, size_t, COLLATE4 *coll ) ;

#ifdef S4CLIENT_OR_FOX
   void t4convertSubSortCompressUnicode( COLLATE4 *, char *, const char *, const int, int * ) ;
#endif

#ifdef S4FOX

   /* collating sequence tables */
   #ifdef S4GENERAL
      extern unsigned char v4general[2][256] ;
      extern compressedChars v4generalComp[4] ;
   #endif

   /* codepage tables */
      extern unsigned char CodePage_1252[128] ;   /* the default */
   #ifdef S4CODEPAGE_437
      extern unsigned char CodePage_437[128] ;
   #endif

   S4EXPORT int S4FUNCTION expr4currency( const EXPR4 * ) ;
   void t4dtstrToFox( COLLATE4 *, char *, const char *, const int, int * ) ;
   void t4strToFox( COLLATE4 *, char *, const char *, const int, int * ) ;
   void t4strToLog( COLLATE4 *, char *, const char *, const int, int * ) ;
   // void t4convertMachineChar( COLLATE4 *, char *, const char *, const int, int * ) ;
   void t4convertSimpleChar( COLLATE4 *, char *, const char *, const int, int * ) ;  // AS Dec 30/02 - support for simple collations
   void t4convertSubSortCompressChar( COLLATE4 *, char *, const char *, const int, int * ) ;

   // new function conversions for OLE-DB
   void t4strToTime( COLLATE4 *, char *, const char *, const int, int * ) ;
   /* LY 99/5/6 */
   #ifdef S4WIN32
      void t4strToLongLong( COLLATE4 *, char *, const char *, const int, int * ) ;
      void t4dblToLongLong( char *result, const double input ) ;
   #endif

   /* TRANSLATING FUNCTION TO SUPPORT VISUAL FOXPRO COLLATING SEQUENCES */
/* AS 07/27/99 -> support for generic collating for Unicode and character fields... */
//   #ifdef S4VFP_KEY
//      int t4strToVFPKey( char *, const char *, const int, const int, T4VFP * ) ;
//   #endif
#endif
S4EXPORT int S4FUNCTION expr4currency( const EXPR4 * ) ;

#ifdef S4CLIPPER
   void t4strToDoub( COLLATE4 *, char *, const char *, const int, int * ) ;
   void t4strToClip( COLLATE4 *, char *, const char *, const int, int * ) ;
   void t4dateDoubToStr( char *, const double ) ;
#endif

/* AS Sept 14/01 new functionality for validating the tables when CODE4 is initialized */
// AS Oct 22/02 - make available to be called in server automatically even in non-multi
/* LY 2002/11/19 : moved code4validate() to have supported and unsupported definitions for dual DLL */
#ifdef __cplusplus
   extern "C" {
#endif
   S4EXPORT int S4FUNCTION code4validate( CODE4 *c4, const char *validateTableName, Bool5 deleteTemp ) ;
#ifdef __cplusplus
   }
#endif
#if !defined( S4CLIENT ) && !defined( S4OFF_INDEX )
   int code4validateAddClose( CODE4 *c4, const char *fullPathTableName, Bool5 isTempIn ) ;
   int code4validateAddOpen( CODE4 *c4, const char *fullPathTableName, Bool5 isTempIn ) ;
   int code4validateUndo( CODE4 *c4 ) ;
   int code4validateModifyTemp( CODE4 *c4, const char *fullPathTableName, Bool5 isTempIn ) ;
#endif /* !defined( S4CLIENT ) && !defined( S4OFF_INDEX ) */

// AS Sept. 19/02 - modify to include input buffer length to avoid access violations, and error if buf not big enough
int u4createCopyrightFromStamp( char *, int ) ;

#ifdef S4FLAT_THUNK
   // functions defined in THUNK4.C
   #ifdef __cplusplus
      extern "C" {
   #endif
   S4EXPORT short S4FUNCTION codethunk4getIndexExtension(CODE4 *c4, char *str) ;
   S4EXPORT short S4FUNCTION codethunk4getLockFileName(CODE4 *c4, char *str) ;
   S4EXPORT short S4FUNCTION codethunk4getLockNetworkId(CODE4 *c4, char *str) ;
   S4EXPORT short S4FUNCTION codethunk4getLockUserId(CODE4 *c4, char *str) ;
   S4EXPORT short S4FUNCTION codethunk4getDateFormat(CODE4 *c4, char *str) ;
   S4EXPORT short S4FUNCTION codethunk4getLogFileName(CODE4 *c4, char *str) ;
   S4EXPORT short S4FUNCTION fthunk4getCurrency(FIELD4 *field, short numDec, char *str) ;
   S4EXPORT short S4FUNCTION fthunk4getDateTime(FIELD4 *field, char *str) ;
   S4EXPORT short S4FUNCTION dthunk4getAlias(DATA4 *data, char *str) ;
   S4EXPORT short S4FUNCTION dthunk4getFileName(DATA4 *data, char *str) ;
   S4EXPORT short S4FUNCTION dthunk4getRecordLow(DATA4 *data, char *str) ;
   S4EXPORT short S4FUNCTION datethunk4getcdow( char *date, char *str) ;
   S4EXPORT short S4FUNCTION datethunk4getcmonth( char *date, char *str) ;
   S4EXPORT short S4FUNCTION errorthunk4getText(CODE4 *c4, long, char *str) ;
   S4EXPORT short S4FUNCTION fthunk4getMemoStr(FIELD4 *field, char *str) ;
   S4EXPORT short S4FUNCTION fthunk4getName(FIELD4 *field, char *str) ;
   S4EXPORT short S4FUNCTION fthunk4getStr(FIELD4 *field, char *str) ;
   S4EXPORT short S4FUNCTION ithunk4getFileName(INDEX4 *i4, char *str) ;
   S4EXPORT short S4FUNCTION tthunk4getFileName(TAG4 *t4, char *str) ;
   S4EXPORT short S4FUNCTION tthunk4getAlias(TAG4 *t4, char *str) ;
   S4EXPORT short S4FUNCTION tthunk4getExprLow(TAG4 *t4, char *str) ;
   S4EXPORT short S4FUNCTION tthunk4getFilterLow(TAG4 *t4, char *str) ;
   S4EXPORT short S4FUNCTION tthunk4getFilterCB(TAG4 *t4, char *str) ;
   S4EXPORT short S4FUNCTION tthunk4getExprCB(TAG4 *t4, char *str) ;
   S4EXPORT short S4FUNCTION exprthunk4getSource(EXPR4 *expr, char *str) ;
   S4EXPORT short S4FUNCTION exprthunk4getStr(EXPR4 *expr, char *str) ;
   #ifdef __cplusplus
      }
   #endif
#endif

#if defined(S4WINCE) && defined(E4VBASIC)
   S4EXPORT int S4FUNCTION field4infoInitW( CODE4 *, unsigned short *, unsigned short *, int, int, int ) ;
   int field4infoInit( CODE4 *, char *, unsigned short, int, int, int ) ;
   S4EXPORT int S4FUNCTION tag4infoInitW( CODE4 *, unsigned short *, unsigned short *, unsigned short *, int, int ) ;
   int tag4infoInit( CODE4 *, char *, char *, char *, int, int ) ;
   S4EXPORT int S4FUNCTION field4infoAddW( CODE4 *, unsigned short *, unsigned short *, int, int, int ) ;
   int field4infoAdd( CODE4 *, char *, unsigned short, int, int, int ) ;
   S4EXPORT int S4FUNCTION tag4infoAddW( CODE4 *, unsigned short *, unsigned short *, unsigned short *, int, int ) ;
   int tag4infoAdd( CODE4 *, char *, char *, char *, int, int ) ;

   S4EXPORT short S4FUNCTION code4compatibility( CODE4 *, short ) ;
   S4EXPORT DATA4* S4FUNCTION code4dataVBCE( CODE4 *, unsigned short * ) ;
   S4EXPORT unsigned short* S4FUNCTION code4dateFormatVBCE( CODE4 * ) ;
   S4EXPORT int S4FUNCTION code4dateFormatSetVBCE( CODE4 *, unsigned short * ) ;
   S4EXPORT unsigned short* S4FUNCTION code4indexExtensionVBCE( CODE4 * ) ;
   S4EXPORT unsigned short* S4FUNCTION code4lockFileNameVBCE( CODE4 * ) ;
   S4EXPORT unsigned short* S4FUNCTION code4lockNetworkIdVBCE( CODE4 * ) ;
   S4EXPORT unsigned short* S4FUNCTION code4lockUserIdVBCE( CODE4 * ) ;

   S4EXPORT unsigned short* S4FUNCTION d4aliasVBCE( DATA4 * ) ;
   S4EXPORT void S4FUNCTION d4aliasSetVBCE( DATA4 *, unsigned short * ) ;
   S4EXPORT DATA4* S4FUNCTION d4createVBCE( CODE4 *, unsigned short * ) ;
   S4EXPORT FIELD4* S4FUNCTION d4fieldVBCE( DATA4 *, unsigned short * ) ;
   S4EXPORT int S4FUNCTION d4fieldInfoVBCE( DATA4 * ) ;
   S4EXPORT int S4FUNCTION d4fieldNumberVBCE( DATA4 *, unsigned short * ) ;
   S4EXPORT unsigned short* S4FUNCTION d4fileNameVBCE( DATA4 * ) ;
   S4EXPORT INDEX4 *S4FUNCTION d4indexVBCE( DATA4 *, unsigned short * ) ;
   S4EXPORT DATA4* S4FUNCTION d4openVBCE( CODE4 *, unsigned short * ) ;
   S4EXPORT unsigned short* S4FUNCTION d4positionVBCE( DATA4 * ) ;
   S4EXPORT int S4FUNCTION d4positionSetVBCE( DATA4 *, unsigned short * ) ;
   S4EXPORT long S4FUNCTION d4recCountVBCE( DATA4 * ) ;  /* LY 00/04/14 */
   S4EXPORT int S4FUNCTION d4seekVBCE( DATA4 *, unsigned short * ) ;
   S4EXPORT int S4FUNCTION d4seekNVBCE( DATA4 *, unsigned short *, const short ) ;
   S4EXPORT int S4FUNCTION d4seekNextVBCE( DATA4 *, unsigned short * ) ;
   S4EXPORT int S4FUNCTION d4seekNextNVBCE( DATA4 *, unsigned short *, const short ) ;
   S4EXPORT int S4FUNCTION d4seekDoubleVBCE( DATA4 *, unsigned short * ) ;
   S4EXPORT int S4FUNCTION d4seekNextDoubleVBCE( DATA4 *, unsigned short * ) ;
   S4EXPORT TAG4* S4FUNCTION d4tagVBCE( DATA4 *, unsigned short * ) ;

   S4EXPORT void S4FUNCTION date4assignVBCE( unsigned short *, long ) ;
   S4EXPORT unsigned short* S4FUNCTION date4cdowVBCE( unsigned short * ) ;
   S4EXPORT unsigned short* S4FUNCTION date4cmonthVBCE( unsigned short * ) ;
   S4EXPORT int S4FUNCTION date4dayVBCE( unsigned short * ) ;
   S4EXPORT int S4FUNCTION date4dowVBCE( unsigned short * ) ;
   S4EXPORT void S4FUNCTION date4formatVBCE( unsigned short *, unsigned short *, unsigned short * ) ;
   S4EXPORT void S4FUNCTION date4initVBCE( unsigned short *, unsigned short *, unsigned short * ) ;
   S4EXPORT int S4FUNCTION date4isLeapVBCE( unsigned short * ) ;
   S4EXPORT long S4FUNCTION date4longVBCE( unsigned short * ) ;
   S4EXPORT int S4FUNCTION date4monthVBCE( unsigned short * ) ;
   S4EXPORT void S4FUNCTION date4timeNowVBCE( unsigned short * ) ;
   S4EXPORT void S4FUNCTION date4todayVBCE( unsigned short * ) ;
   S4EXPORT int S4FUNCTION date4yearVBCE( unsigned short * ) ;

   S4EXPORT int S4FUNCTION error4describeVBCE( CODE4 *, const int, const long, unsigned short *, unsigned short *, unsigned short * ) ;
   S4EXPORT int S4FUNCTION error4fileVBCE( CODE4 *, unsigned short *, const int ) ;
   S4EXPORT unsigned short* S4FUNCTION error4textVBCE( CODE4 *, const long ) ;

   S4EXPORT EXPR4* S4FUNCTION expr4parseVBCE( DATA4 *, unsigned short * ) ;
   S4EXPORT unsigned short* S4FUNCTION expr4sourceVBCE( const EXPR4 * ) ;
   S4EXPORT unsigned short* S4FUNCTION expr4strVBCE( EXPR4 * ) ;

   S4EXPORT void S4FUNCTION f4assignNVBCE( FIELD4 *, unsigned short *, signed short) ;
   S4EXPORT void S4FUNCTION f4assignCurrencyVBCE( FIELD4 *, unsigned short * ) ;
   S4EXPORT void S4FUNCTION f4assignDateTimeVBCE( FIELD4 *, unsigned short * ) ;
   S4EXPORT void S4FUNCTION f4assignDoubleVBCE( FIELD4 *, unsigned short * ) ;
   S4EXPORT unsigned short* S4FUNCTION f4currencyVBCE( FIELD4 *, short ) ;
   S4EXPORT unsigned short* S4FUNCTION f4dateTimeVBCE( FIELD4 * ) ;
   S4EXPORT unsigned short* S4FUNCTION f4doubleVBCE( FIELD4 * ) ;
   S4EXPORT int S4FUNCTION f4memoAssignVBCE( FIELD4 *, unsigned short * ) ;
   S4EXPORT int S4FUNCTION f4memoAssignNVBCE( FIELD4 *, unsigned short *, unsigned ) ;
   S4EXPORT unsigned short* S4FUNCTION f4memoStrVBCE( FIELD4 * ) ;
   S4EXPORT unsigned short* S4FUNCTION f4nameVBCE( FIELD4 * ) ;
   S4EXPORT unsigned short* S4FUNCTION f4strVBCE( FIELD4 * ) ;

   S4EXPORT INDEX4* S4FUNCTION i4createVBCE( DATA4 *, unsigned short * ) ;
   S4EXPORT unsigned short* S4FUNCTION i4fileNameVBCE( INDEX4 * ) ;
   S4EXPORT INDEX4* S4FUNCTION i4openVBCE( DATA4 *, unsigned short * ) ;
   S4EXPORT TAG4* S4FUNCTION i4tagVBCE( INDEX4 *, unsigned short * ) ;
   S4EXPORT int S4FUNCTION i4tagInfoVBCE( INDEX4 * ) ;


   S4EXPORT unsigned short* S4FUNCTION t4aliasVBCE( TAG4 * ) ;
   S4EXPORT unsigned short* S4FUNCTION t4exprVBCE( TAG4 * ) ;
   S4EXPORT unsigned short* S4FUNCTION t4filterVBCE( TAG4 * ) ;

   S4EXPORT void S4FUNCTION tag4infoFree( CODE4 * ) ;
   S4EXPORT void S4FUNCTION field4infoFree( CODE4 * ) ;
#endif


// AS Jan 16/01 - support logging from non-oledb apps
#ifndef OLEDB5BUILD
   #ifdef S4OLEDEBUG_PRINT
      extern "C" {
         void log5enable() ;
         void log5( const char *ptrToWrite ) ;
         void log5closeFile() ;
         void log5query( const char *ptrToWrite ) ;
         void out5riidInfo( REFIID riid ) ;
         void log4invokeDebugger() ;
         void log4invokeDebuggerDo() ;
      }
   #endif
#endif


// AS Aug 13/02 - Support for indicating porting issues for new features
#ifdef S4WIN32
   // As porting issues are addressed, the assert5port() calls themselves are removed
   #define assert5port( val )
#endif
