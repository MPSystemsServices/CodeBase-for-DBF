/* t4test.h   (c)Copyright Sequiter Software Inc., 1988-1997.  All rights reserved. */

/***********************************************************************
      Conditional Testing

      For some tests, there exists a compile define and constant to indicate
      what testing should be done.

      The define is used to exclude code calling non-existing functions
      (to avoid compile errors).

      The runtime constant defines specify whether or not to perform the testing.

      If the runtime switch is on but the compile switch is not, the
      test will fail with a runtime error.

      If the runtimes switch is not set the test will always succeed.

      All copmile and runtime switches are named similarily, and for
      each pair only different in case.  Also included for each pair
      is a comment describing what they test.

      Procedure for adding a switch:
         1.  List and document the switch in the documentation section below
         2.  Include the switch and constant in each of the #define os sections
             set as appropriate (often new tests are for WIN32 only).
         3.  In the t4test() section of the actual test put #ifdef to
             handle the cases.
             e.g. (from t4rel2.c)

            #ifdef S4UNUSUAL_TESTING
               int S4FUNCTION t4rel2( D4DISPLAY *display )
            #else
               int S4FUNCTION t4test( D4DISPLAY *display )
            #endif
            {
               #ifndef T4COUNT
                  if ( t4count == 1 )  // means we want to test count, but forgot to define T4COUNT
                  {
                     t4severe( &cb, t4errGeneral, ".5 - failed to compile with T4COUNT but t4count set to true" ) ;
                  }
                  else
                  {
                     display->y += 2 ;
                     d4displayStr( display, "T4REL2:   REQUIRES T4COUNT DEFINED", 1) ;
                     d4displayStr( display, "", 1) ;
                  }
               #else
                  if ( t4count == 1 )  // means we want to test count at run time
                  {
                     ...  // the actual testing code
                  }
                  else
                  {
                     display->y += 2 ;
                     d4displayStr( display, "T4REL2:   REQUIRES t4count runtime switch to be set to true", 1) ;
                     d4displayStr( display, "", 1) ;
                  }
               #endif

         4.  If required, include the #ifdef in the test file as required to avoid
             compile errors (by removing other sections of the test)

***********************************************************************

      Test Compile and RunTime switches

      T4COUNT - t4count    :  Tests functions relate4count()
      T4RMASTER - t4count  :  Tests function relate4skipMaster()
      T4BUFFER - t4buffer  :  Tests functions d4readBuffer()/relate4readBuffer()
      T4SEEK - t4seek    :  Tests function t4seekN()
      T4INDEX_TEMP - t4indexTemp : Tests temporary non-updating indexes.  Note that some of this testing does
                                   multi-code4 lock testing which does not work on some platforms
      T4MEMO_COMPRESS - t4memoCompress : Tests compressed memo entries
      T4DATA_COMPRESS - t4dataCompress : Tests compressed data files
      T4REUSE_DELETED - t4reuseDeleted : Tests automatic reuse of deleted records
      T4RETAIN - t4retain : relate4retain test
      T4TIME_MILLI - t4timeMilli : Tests support for r4dateTimeMill type, and r4autoTimestamp feature.
      T4LONG_FIELD_NAME - t4longFieldName : Tests long field names (up to 255 characters)

***********************************************************************/


#ifdef S4WIN32
   #ifndef S4CLIPPER // LY Oct 18/04 : relate4count() not supported under Clipper at this time
      #define T4COUNT
      const t4count = 1 ;
   #else
      const t4count = 0 ;
   #endif
   #ifndef S4CLIENT
      #define T4RMASTER
      const t4rmaster = 1 ;
   #else
      const t4rmaster = 0 ;
   #endif
   #define T4BUFFER
   const t4buffer = 1 ;
   #define T4SEEK
   const t4seek = 1 ;
   // AS Apr 7/04 - also don't support for clipper - at least for now
   #if !defined( S4WINCE ) && !defined( S4CLIPPER )
      #define T4INDEX_TEMP
      const t4indexTemp = 1 ;
   #endif
   #ifdef S4COMPRESS
      // AS Apr 2/04 - t4memoCompress only for fox (or client)
      #if defined( S4CLIENT_OR_FOX )
         #define T4MEMO_COMPRESS
         const t4memoCompress = 1 ;
      #else
         const t4memoCompress = 0 ;
      #endif
      /* LY May 21/04 - removed S4STAND_ALONE, changed S4FOX to S4CLIENT_OR_FOX for added client/server compression */
      #if defined( S4CLIENT_OR_FOX ) && !defined( S4OFF_WRITE )
         // currently only supported in this instance
         #define T4DATA_COMPRESS
         const t4dataCompress = 1 ;
      #else
         const t4dataCompress = 0 ;
      #endif
   #else
      const t4memoCompress = 0 ;
      const t4dataCompress = 0 ;
   #endif
   // AS Dec 15/03 - not supported in clipper because the tag file name issue (end up with many files of same name)
   /* LY July 26/03 : added S4WIN64 */
   #if defined( S4CLIPPER ) || defined( S4WIN64 )
      // #define T4REUSE_DELETED
      const t4reuseDeleted = 0 ;
   #else
      #define T4REUSE_DELETED
      const t4reuseDeleted = 1 ;
   #endif
   #define T4RETAIN
   const t4retain = 1 ;
   #ifndef S4WINCE   /* LY 2003/12/11 : not supported at this time */
      #define T4TIME_MILLI
   #endif
   const t4timeMilli = 1 ;
   #ifdef S4CLIENT_OR_FOX
      #define T4LONG_FIELD_NAME
      const t4longFieldName = 1 ;
   #else
      const t4longFieldName = 0 ;
   #endif
#endif

#ifdef S4MACINTOSH
   // #define T4COUNT  - new feature not supported for Mac
   const t4count = 0 ;  // don't run
   // #define T4BUFFER  - new feature not supported for Mac
   const t4buffer = 0 ;  // don't run
   const t4seek = 0 ;
   const t4memoCompress = 0
   const t4dataCompress = 0 ;
   const t4reuseDeleted = 0 ;
   const t4retain = 0 ;
   const t4longFieldName = 0 ;
#endif

#ifdef S4UNIX
   /* LY July 26/03 : added int to declarations */
   // #define T4COUNT  - new feature not supported for Unix
   const int t4count = 0 ;  // don't run
   // #define T4BUFFER  - new feature not supported for Unix
   const int t4buffer = 0 ;  // don't run
   const int t4seek = 0 ;
   const int t4memoCompress = 0
   const int t4dataCompress = 0 ;
   const int t4reuseDeleted = 0 ;
   const int t4retain = 0 ;
   const t4longFieldName = 0 ;
#endif




/* r4ndx removed from library, but not test programs, so put in here to avoid errors */
#define r4ndx             203

#define S4MAX_SERVER_NAME_SIZE S4SERVER_NAME_SIZE


/* #define S4TEST_KEEP_FILES */

/*#define S4EXTRA_SHUTDOWN*/

#ifndef S4TEST_INCLUDED

#define S4TEST_INCLUDED

#ifdef S4TESTING
   #ifdef E4PAUSE
      #ifndef E4DO_PAUSE
         #error For automated testing E4PAUSE should not be defined.
      #endif
   #endif
#endif

#if defined(S4TESTING)  && defined(S4MACINTOSH)
   #define S4UNUSUAL_TESTING
#endif


#ifndef S4WINCE
   #include <time.h>
#endif

#ifdef S4OS2PM
   #define IDR_MAIN     1
   #define IDM_ABOUT    9600
   #define IDM_DO_TEST  9700
   #define EM_CODEBASE_ERROR 9800
   #define ID_OK        1
   #define ID_CANCEL    2
   #define MESSAGELEN          50
   #define MSGBOXID        1001
   #define IDS_APPNAME     2
   #define IDS_UNTITLED    7

   extern HWND hwndMainFrame ;
   extern HWND hwndMain ;
   extern HDC  hdcMain ;
   extern HAB  hab ;
   extern HMQ  hmq ;
   extern CHAR szAppName[MAXNAMEL] ;
   extern CHAR szUntitled[MESSAGELEN] ;
#else
   #ifdef S4WINTEL
/*      #include <windows.h>*/
   #else
      #define HWND unsigned
      #define HANDLE unsigned
   #endif
#endif

#ifdef S4WINCE
   #define  E4_ERROR_CDS     L"CODEBASE SEVERE ERROR"
#else
   #define  E4_ERROR_CDS     "CODEBASE SEVERE ERROR"
#endif

#ifdef S4UNIX
   #include "p4port.h"
#endif

#ifdef S4EXCEPTIONS
   #include <eh.h>
   #include <iostream.h>
   #include <process.h>
#endif

#define  T4_ERROR_WAR     "\r\n\r\nTest Warning Number"
#define  T4_ERROR_SEV     "\r\n\r\nTest Failure Number"

#define t4errField      -801
#define t4errAppend     -802
#define t4errCount      -803
#define t4errGo         -804
#define t4errData       -805
#define t4errAppendBlank  -806
#define t4errWrite      -807
#define t4errClose      -808
#define t4errLock       -809
#define t4errUnlock     -809
#define t4errRecno      -810
#define t4errDeleted    -811
#define t4errPack       -812
#define t4errZap        -813
#define t4errNFields    -814
#define t4errName       -815
#define t4errMemory     -816
#define t4errGeneral    -817
#define t4errSort       -818
#define t4errCheck      -819
#define t4errTag        -820
#define t4errSeek       -821
#define t4errReindex    -822
#define t4errSkip       -823
#define t4errParm       -824
#define t4errExpr       -825
#define t4errMemo       -826
#define t4errFlush      -827
#define t4errOpt        -828
#define t4errDate       -829
#define t4errPosition   -830
#define t4errRelate     -831
#define t4errRead       -832
#define t4errTrans      -833
#define t4errTop        -834
#define t4errConnect    -835
#define t4errInit       -836
#define t4errProp       -837
#define t4errAuthorize  -838
#define t4errIndex      -839
#define t4errStatus     -840

typedef struct
{
   #ifdef S4WINDOWS
      // AS Jan 15/03 - Need to be able to re-retrieve the source pointers in some cases.
      // The previous coding was altering the input paramaters.  Instead make a copyu
      char parmsCopy[512] ;
      LPSTR  ptr ;
   #else
      int          nParms ;
      int          iParm ;
      char       **parms ;
   #endif
} D4PARSE_STR;



typedef struct
{
   HWND      hWnd ;
   #ifndef S4DOS
      #ifdef S4WIN32
         HINSTANCE hInst ;
      #else
         unsigned hInst ;
      #endif
   #endif
   D4PARSE_STR  parseStr ;
   int          x,y ;

   #ifdef S4WINDOWS
      TEXTMETRIC   tm ;
      LPMSG        lpmsg ;
      MSG          msg ;  /* Last Message */
      int          didClose ;
      int          didQuit ;
      HCURSOR      hSaveCursor ;
   #endif
   #ifdef S4OS2PM
      FONTMETRICS  tm ;
      int          didClose ;
      int          didQuit ;
      int          do_box ;
      char         str[80] ;
      int          isNewLine ;
      int          len ;
      int          displaySet ;
   #endif
} D4DISPLAY;

#ifdef S4TESTING
   #ifdef S4WINDOWS
      #ifdef S4MEM_PRINT
         extern D4DISPLAY *mem4displayPtr ;
      #endif
   #endif
#endif

#ifdef S4TEMP
   extern D4DISPLAY mem4display ;
#else
   #ifdef S4OS2PM
      extern D4DISPLAY mem4display ;
   #endif
#endif


#ifdef DLL
   extern HINSTANCE test_hInst ;
#endif

#ifdef S4WINDOWS
extern MSG msg ;
extern TEXTMETRIC tm ;
#endif

#ifdef __cplusplus
   extern "C" {
#endif

void  d4displayStart( void ) ;
int   d4displayGetch( D4DISPLAY * ) ;
void  d4displayInit( D4DISPLAY *, HWND ) ;
void  d4displayStr( D4DISPLAY *, const char *, int ) ;
void  d4displayPtr( D4DISPLAY *, void *, int ) ;
void  d4displayNum( D4DISPLAY *, long, int ) ;
int   d4displayQuit( D4DISPLAY * ) ;
void  d4displayWait( D4DISPLAY * ) ;

// AS Jan 15/03 - moved from t4memo2 to allow generic testing
int file4compareFile4( FILE4 *file1, FILE4 *file2, Bool5 ) ;
void file4compare( CODE4 *c4, const char *file1name, const char *file2name ) ;

#ifdef S4WINDOWS
   #ifdef S4WINCE
      void d4parseStringInit( D4PARSE_STR *, LPTSTR ) ;
      #define TextOut(a,b,c,d,e) ExtTextOut((a),(b),(c),0,NULL,(d),(e),NULL)
   #else
      void d4parseStringInit( D4PARSE_STR *, LPSTR ) ;
   #endif
#else
   void d4parseStringInit( D4PARSE_STR *, int , char ** ) ;
#endif

char * d4parseStringNParm( D4PARSE_STR * ) ;

void e4errorOut( const char * ) ;

int S4FUNCTION t4test( D4DISPLAY * ) ;

#ifdef S4UNUSUAL_TESTING
   //test program one function calls
   int S4FUNCTION t4all( D4DISPLAY * ) ;
   int S4FUNCTION t4append( D4DISPLAY * ) ;
   int S4FUNCTION t4approx( D4DISPLAY * ) ;
   int S4FUNCTION t4bcd( D4DISPLAY * ) ;
   int S4FUNCTION t4calc( D4DISPLAY * ) ; /* LY 2003/01/22 : added missing declaration */
   int S4FUNCTION t4currcy( D4DISPLAY * ) ;
   int S4FUNCTION t4code( D4DISPLAY * ) ;
   int S4FUNCTION t4code2( D4DISPLAY * ) ;
   int S4FUNCTION t4code3( D4DISPLAY * ) ;
   int S4FUNCTION t4code4( D4DISPLAY * ) ;
   int S4FUNCTION t4com( D4DISPLAY * ) ;
   int S4FUNCTION t4comit2( D4DISPLAY * ) ;
   int S4FUNCTION t4commit( D4DISPLAY * ) ;
   int S4FUNCTION t4conect( D4DISPLAY * ) ;
   int S4FUNCTION t4conv( D4DISPLAY * ) ;
   int S4FUNCTION t4conf( D4DISPLAY * ) ;
   int S4FUNCTION t4copy1( D4DISPLAY * ) ;
   int S4FUNCTION t4copy2( D4DISPLAY * ) ;
   int S4FUNCTION t4crash( D4DISPLAY * ) ;
   int S4FUNCTION t4crbad( D4DISPLAY * ) ;
   int S4FUNCTION t4crbad2( D4DISPLAY * ) ;
   int S4FUNCTION t4data( D4DISPLAY * ) ;
   int S4FUNCTION t4data2( D4DISPLAY * ) ;
   int S4FUNCTION t4date( D4DISPLAY * ) ;
   int S4FUNCTION t4desc1( D4DISPLAY * ) ;
   int S4FUNCTION t4desc2( D4DISPLAY * ) ;
   int S4FUNCTION t4desc3( D4DISPLAY * ) ;
   int S4FUNCTION t4edit( D4DISPLAY * ) ; /* LY 2003/01/22 : added missing declaration */
   int S4FUNCTION t4eof( D4DISPLAY * ) ;
   int S4FUNCTION t4error( D4DISPLAY * ) ;
   int S4FUNCTION t4excl( D4DISPLAY * ) ;
   int S4FUNCTION t4exprTest( D4DISPLAY * ) ;
   int S4FUNCTION t4fail( D4DISPLAY * ) ;
   int S4FUNCTION t4file( D4DISPLAY * ) ;
   int S4FUNCTION t4disc( D4DISPLAY * ) ;
   //test program two function calls
   int S4FUNCTION t4group( D4DISPLAY * ) ;
   int S4FUNCTION t4g_fld( D4DISPLAY * ) ;
   int S4FUNCTION t4g_fld2( D4DISPLAY * ) ;
   int S4FUNCTION t4indexTest( D4DISPLAY * ) ;
   int S4FUNCTION t4index2( D4DISPLAY * ) ;
   int S4FUNCTION t4index3( D4DISPLAY * ) ;
   int S4FUNCTION t4index4( D4DISPLAY * ) ;
   int S4FUNCTION t4index5( D4DISPLAY * ) ;
   int S4FUNCTION t4index6( D4DISPLAY * ) ;
   int S4FUNCTION t4index7( D4DISPLAY * ) ;
   int S4FUNCTION t4index8( D4DISPLAY * ) ;
   int S4FUNCTION t4index9( D4DISPLAY * ) ;
   int S4FUNCTION t4largeTest( D4DISPLAY * ) ;
   int S4FUNCTION t4indx1( D4DISPLAY * ) ;
   int S4FUNCTION t4indx2( D4DISPLAY * ) ;
   int S4FUNCTION t4indx9( D4DISPLAY * ) ;
   int S4FUNCTION t4list1( D4DISPLAY * ) ;
   int S4FUNCTION t4list2( D4DISPLAY * ) ;
   int S4FUNCTION t4lock( D4DISPLAY * ) ;
   int S4FUNCTION t4lock2( D4DISPLAY * ) ;
   int S4FUNCTION t4lock3( D4DISPLAY * ) ;
   int S4FUNCTION t4lock4( D4DISPLAY * ) ;
   int S4FUNCTION t4log( D4DISPLAY * ) ;  /* LY 2003/01/21 : added missing declaration */
   int S4FUNCTION t4mask( D4DISPLAY * ) ;
   int S4FUNCTION t4max( D4DISPLAY * ) ;
   int S4FUNCTION t4memoTest( D4DISPLAY * ) ;
   int S4FUNCTION t4memo1( D4DISPLAY * ) ;
   int S4FUNCTION t4memo2( D4DISPLAY * ) ;
   int S4FUNCTION t4mul( D4DISPLAY * ) ;
   int S4FUNCTION t4mul3( D4DISPLAY * ) ;
   int S4FUNCTION t4mul4( D4DISPLAY * ) ;
   int S4FUNCTION t4openTest( D4DISPLAY * ) ;
   int S4FUNCTION t4opt( D4DISPLAY * ) ;
   //test program three function calls
   int S4FUNCTION t4opt2( D4DISPLAY * ) ;
   int S4FUNCTION t4pack_z( D4DISPLAY * ) ;
   int S4FUNCTION t4positi( D4DISPLAY * ) ;
   int S4FUNCTION t4read( D4DISPLAY * ) ;
   int S4FUNCTION t4reinde( D4DISPLAY * ) ;
   int S4FUNCTION t4rel1( D4DISPLAY * ) ;
   int S4FUNCTION t4rel19( D4DISPLAY * ) ;
   int S4FUNCTION t4rel18( D4DISPLAY * ) ;
   int S4FUNCTION t4rel20( D4DISPLAY * ) ;
   int S4FUNCTION t4rel21( D4DISPLAY * ) ;
   int S4FUNCTION t4rel22( D4DISPLAY * ) ;
   int S4FUNCTION t4rel23( D4DISPLAY * ) ;
   int S4FUNCTION t4rel24( D4DISPLAY * ) ;
   int S4FUNCTION t4rel25( D4DISPLAY * ) ;
   int S4FUNCTION t4rel26( D4DISPLAY * ) ;
   int S4FUNCTION t4rel30( D4DISPLAY * ) ;
   int S4FUNCTION t4rel41( D4DISPLAY * ) ;
   int S4FUNCTION t4relc( D4DISPLAY * ) ; /* LY 2003/01/22 : added missing declaration */
   int S4FUNCTION t4relext( D4DISPLAY * ) ;  /* LY 2003/01/22 : added missing declaration */
   int S4FUNCTION t4remove( D4DISPLAY * ) ;
   int S4FUNCTION t4rlate1( D4DISPLAY * ) ;
   int S4FUNCTION t4rlate2( D4DISPLAY * ) ;
   int S4FUNCTION t4rlate3( D4DISPLAY * ) ;
   int S4FUNCTION t4rlate4( D4DISPLAY * ) ;
   int S4FUNCTION t4ronly( D4DISPLAY * ) ;
   int S4FUNCTION t4seekTest( D4DISPLAY * ) ;
   int S4FUNCTION t4seekdb( D4DISPLAY * ) ;
   int S4FUNCTION t4skip( D4DISPLAY * ) ;
   int S4FUNCTION t4skip2( D4DISPLAY * ) ;
   int S4FUNCTION t4skip3( D4DISPLAY * ) ;
   int S4FUNCTION t4sort( D4DISPLAY * ) ;
   int S4FUNCTION t4spin( D4DISPLAY * ) ;
   int S4FUNCTION t4sort2( D4DISPLAY * ) ;
   int S4FUNCTION t4stat( D4DISPLAY * ) ;
   //test program four function calls
   //test program four is not completely utilized if additional tests are
   //added the test will be to test program four.
   int S4FUNCTION t4uniq2( D4DISPLAY * ) ;
   int S4FUNCTION t4uniqueTest( D4DISPLAY * ) ;
   int S4FUNCTION t4write( D4DISPLAY * ) ;
   int S4FUNCTION t4ascend( D4DISPLAY * ) ;
   int S4FUNCTION t4clone( D4DISPLAY * ) ;
   int S4FUNCTION t4cur( D4DISPLAY * ) ;
   int S4FUNCTION t4currcy( D4DISPLAY * ) ;
   int S4FUNCTION t4dttime( D4DISPLAY * ) ;
   int S4FUNCTION t4fieldTest( D4DISPLAY * ) ;
   int S4FUNCTION t4rlate5( D4DISPLAY * ) ;
   int S4FUNCTION t4opt3( D4DISPLAY * ) ;
   int S4FUNCTION t4comit3( D4DISPLAY * ) ;
   int S4FUNCTION t4write2( D4DISPLAY * ) ;
   int S4FUNCTION t4write3( D4DISPLAY * ) ;
   int S4FUNCTION t4les1( D4DISPLAY * ) ;
   int S4FUNCTION t4lidx( D4DISPLAY * ) ;
   int S4FUNCTION t4vfp1( D4DISPLAY * ) ;
   int S4FUNCTION t4vfp2( D4DISPLAY * ) ;
   int S4FUNCTION t4vfp3( D4DISPLAY * ) ;
   int S4FUNCTION t4vfp4( D4DISPLAY * ) ;
   int S4FUNCTION t4vfp5( D4DISPLAY * ) ;
   int S4FUNCTION t4vfp6( D4DISPLAY * ) ;
   int S4FUNCTION t4vfp7( D4DISPLAY * ) ;
   //test program five function calls
   //test program five is not completely utilized if additional tests are
   //added the test will be to test program four then five.
   int S4FUNCTION t4rlatek( D4DISPLAY * ) ;
   int S4FUNCTION t4relx1( D4DISPLAY * ) ;
   int S4FUNCTION t4relx11( D4DISPLAY * ) ;
   int S4FUNCTION t4relx2( D4DISPLAY * ) ;
   int S4FUNCTION t4relx22( D4DISPLAY * ) ;
   int S4FUNCTION t4relx3( D4DISPLAY * ) ;
   int S4FUNCTION t4relx4( D4DISPLAY * ) ;
   int S4FUNCTION t4relx44( D4DISPLAY * ) ;
   int S4FUNCTION t4relx5( D4DISPLAY * ) ;
   int S4FUNCTION t4relx6( D4DISPLAY * ) ;
   int S4FUNCTION t4relx66( D4DISPLAY * ) ;
   int S4FUNCTION t4relx67( D4DISPLAY * ) ;
   int S4FUNCTION t4relx7( D4DISPLAY * ) ;
   int S4FUNCTION t4relx8( D4DISPLAY * ) ;
   int S4FUNCTION t4relx9( D4DISPLAY * ) ;
   int S4FUNCTION t4relx99( D4DISPLAY * ) ;
   int S4FUNCTION t4crbad2( D4DISPLAY * ) ;
   int S4FUNCTION t4relxA( D4DISPLAY * ) ;
   int S4FUNCTION t4relxB( D4DISPLAY * ) ;
   int S4FUNCTION t4cmprss( D4DISPLAY * ) ;  // LY Jun 30/04
   // LY Feb 5/05 : added missing declarations
   int S4FUNCTION t4memo3( D4DISPLAY * ) ;
   int S4FUNCTION t4thread( D4DISPLAY * ) ;
   int S4FUNCTION t4memovr( D4DISPLAY * ) ;
   int S4FUNCTION t4code5( D4DISPLAY * ) ;
   int S4FUNCTION t4indx3( D4DISPLAY * ) ;
#endif

void  t4warn( int errCode, char *desc ) ;
#ifndef T4OLE
   #define t4severe( c4, code, desc ) t4severeLow( (c4), (code), (desc), __FILE__, __LINE__ )
   void t4severeLow( CODE4 *, int, const char *, const char *, int ) ;
#endif

#ifdef S4EXCEPTIONS
   void t4exception( unsigned int ) ;
#endif

#ifdef S4EXTRA_SHUTDOWN
   void test4localShutdown(void) ;
#endif

#ifdef __cplusplus
   }
#endif

extern int s4allocOff ;

#ifdef E4ANALYZE
   #define do5free( ptr ) free5( ptr )
#else
   #define do5free( ptr ) g_pIMalloc->Free( ptr )
#endif

#ifndef ODBC4CONFIG_TABLE
   // AS Mar 6/03 - Support for simpler install - do not open registry if configuration datafile is found
   #define ODBC4CONFIG_TABLE "CONFIG4ODBC.DBF"
   // #define ODBC4CONFIG_TABLE_LOCK_DBQ "DBQ"
   #define ODBC4CONFIG_TABLE_LOCK_ATTEMPTS "LOCK_ATT"
   #define ODBC4CONFIG_TABLE_COLLATION "COLLATION"
   #define ODBC4CONFIG_TABLE_TRIM_BLANKS "TRIMBLANKS"
   #define ODBC4CONFIG_TABLE_EXPOSE_RECNO "EXP_RECNO"
   // AS Jun 26/03 - new field
   #define ODBC4CONFIG_TABLE_TRANS "ODBCTRANS"
#endif

#endif S4TEST_INCLUDED
