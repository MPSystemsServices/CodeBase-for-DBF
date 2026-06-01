/* d4defs.h   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#ifndef D4DEFS_INC
#define D4DEFS_INC

#ifdef S4WINCE
   #define S4WIN32
#endif

#ifdef S4WIN64
   #define S4WIN32
#endif

#ifdef S4SERVER
   #ifndef S4JAVA
      #define S4JAVA
   #endif
   #ifndef S4HALFJAVA
      #define S4HALFJAVA
   #endif
   // AS Aug 27/02 - don't support with ODBC build
   #if defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD ) || defined( S4DLL_BUILD_ENCRYPT )
      #ifndef NO_OLEDB5BUILD
         #define NO_OLEDB5BUILD
      #endif
      #ifdef OLEDB5BUILD
         #undef OLEDB5BUILD
      #endif
   #endif
   // CS 2005/09/29 Removed automatic definition of OLEDB5BUILD
   #ifndef S4LOCK_HASH
      #define S4LOCK_HASH
   #endif
   #ifdef S4TIMEOUT_HOOK
      /* timeout hook does not work if compiled with server */
      #undef S4TIMEOUT_HOOK
   #endif
#endif

// define to do some extra index error checking and automatic repairing
//   #define EXCEPTION4REINDEX

#if defined( E4ANALYZE ) && !defined( EXCEPTION4REINDEX ) && !defined( S4OFF_EXCEPTION_REINDEX )
   // catches indexing errors not otherwise caught
   #define EXCEPTION4REINDEX
#endif

// must be defined early to avoid other switches getting compiled in...
// S4ODBC_BUILD is used to build the special CodeBase DLL used by the ODBC engine
// S4ODBC_ENABLED is used to build a CodeServer which supports ODBC clients
// AS Mar 20/03 - enabled transactions in client/server odbc
#if defined( S4ODBC_BUILD ) && defined( S4SERVER )
   #if defined( S4OFF_ODBC_TRANS ) && !defined( S4OFF_TRAN )
      #define S4OFF_TRAN
   #endif

   #define S4OFF_COMMUNICATIONS
   #ifndef S4OFF_TRAN
      // AS Apr 28/03 - made trans-shared a run-time switch
      // #ifndef S4TRANS_FILE_SHARED
      //    #define S4TRANS_FILE_SHARED
      // #endif
      #ifndef S4TRANS_FULL_LOCK_OFF
         #define S4TRANS_FULL_LOCK_OFF
      #endif
   #endif
#endif

// AS May 5/03 - ODBC uses exclusive files, regular stand/alone supports sharing
#if defined( S4STAND_ALONE ) && !defined( S4ODBC_BUILD )
   // in stand-alone mode, the transaction file is opened exclusive (for now)
   // it is also shared sometimes in the server version (namely via SIMBA)
   // AS Jun 20/03 removed this switch altogether, choice of sharing of log files is now completely run-time
   // #ifndef S4TRANS_FILE_SHARED
   //    #define S4TRANS_FILE_SHARED
   // #endif
   // we don't currently use S4TRANS_FULL_LOCK_OFF in stand-alone, even with ODBC
#endif

// AS Mar 20/03 - enabled transactions in client/server odbc
#if defined( S4ODBC_ENABLED ) && defined( S4SERVER ) && !defined( S4OFF_ODBC_TRANS )
   // AS Apr 28/03 - made trans-shared a run-time switch
   // #ifndef S4TRANS_FILE_SHARED
   //    #define S4TRANS_FILE_SHARED
   // #endif
   #ifndef S4TRANS_FULL_LOCK_OFF
      #define S4TRANS_FULL_LOCK_OFF
   #endif
#endif

#if !defined( S4OFF_CONTROLS ) && ( !defined( S4DLL_BUILD ) || defined( S4WINCE ) || defined( PASCAL_DOS ) || defined( S4SERVER ) )
   #define S4OFF_CONTROLS
#endif

#ifdef S4OFF_MULTI
   #ifdef OLEDB5BUILD
      /* ole-db requires multi-user */
      #undef OLEDB5BUILD
   #endif
#endif

#if (defined(S4DLL_BUILD) || defined(S4LIB_BUILD)) && !defined(__cplusplus)
   #if defined(__BORLANDC__)
      #error Must perform C++ compile when building the CodeBase library. Use the -P compiler option.
   #elif defined(_MSC_VER)
      #error Must perform C++ compile when building the CodeBase library. Use the -TP compiler option.
   #else
      #error Must perform C++ compile when building the CodeBase library.
   #endif
#endif

#ifdef OLEDB5BUILD
   #ifndef MSQL7COMPATABILITY
      #define MSQL7COMPATABILITY
   #endif
   #ifndef __cplusplus
      #error must build OLEDB as C++ (compiler build says compile is C, not C++)
   #endif
   #ifdef S4WIN32
      #include <windows.h>
   #endif
   #define S4OLEDB_OR_NOT_SERVER
   #ifndef S5USE_EXTENDED_TYPES
      #define S5USE_EXTENDED_TYPES
   #endif
#endif

#if !defined( S4SERVER ) && !defined( S4OLEDB_OR_NOT_SERVER )
   #define S4OLEDB_OR_NOT_SERVER
#endif

#if !defined( S4DLL_BUILD ) && !defined( S4LIB_BUILD ) && !defined( S4EXE_BUILD )
   #define S4EXE_BUILD
#endif

#ifdef S4WIN32
   #ifdef __BORLANDC__
      #if __BORLANDC__ < 0x500
         #ifndef S4OFF_THREAD
            #define S4OFF_THREAD
         #endif
      #else
         #define S4WIDECHAR
      #endif
   #else
      #define S4WIDECHAR
   #endif
#endif

#if defined( S4SERVER ) && !defined( S4WIDECHAR )
   #define S4WIDECHAR
#endif

#ifdef _MSC_VER
   #if _MSC_VER < 1000
      #ifndef S4OFF_THREAD
         #define S4OFF_THREAD
      #endif
   #endif
   // AS Dec 13/05 - under Windows getenv is becoming deprecated...
   // CS 2010/05/19 When building for WinCE, the old functions seem
   // to be available but the new ones are not. e.g. sprintf vs sprintf_s
   #if _MSC_VER >= 1400 && !defined(S4WINCE)
      #define S4WINDOWS_VS5_PLUS
   #endif
#endif

#ifdef __cplusplus
   /* only use CodeBase++ ::true() functions on older compilers
      otherwise use ::isTrue() replacment */

   #ifdef __BORLANDC__
      #if __BORLANDC__ < 0x500
         #define S4USE_TRUE
      #endif
   #endif

   #ifdef _MSC_VER
      #if _MSC_VER < 1010
         #define S4USE_TRUE
      #endif
   #endif
#endif

#ifndef S4INLINE
   #define S4INLINE
#endif

#if !defined( S4UNIX ) && !defined( S4MACINTOSH ) && !defined( S4OS2 ) && !defined( S4PALM ) && !defined( _MSC_VER )
   #define P4ARGS_USED
#endif

#if defined(_MSC_VER)
   #if defined(_WIN32) && !defined(S4WIN32)
      #error Must compile with S4WIN32 defined
   #endif
   #if !defined(_WIN32) && defined(S4WIN32)
      #error Cannot compile with S4WIN32 defined
   #endif
#endif

#if defined(__TURBOC__)
   #if defined(__WIN32__) && !defined(S4WIN32)
      #error Must compile with S4WIN32 defined
   #endif
   #if !defined(__WIN32__) && defined(S4WIN32)
      #error Cannot compile with S4WIN32 defined
   #endif
#endif

#ifdef S4SERVER
   #ifdef S4CB51
      #error - CodeBase Server cannot be built with S4CB51. Comment the S4CB51 switch.
   #endif

   /* for 5.1 version, ... */
   #define S4OFF_REPORT
#endif

/* catalog files not supported in version 6.0 */
#define S4OFF_CATALOG

#ifdef S4CB51
   #ifndef S4API_51_ONLY
      #ifndef S4OFF_TRAN
         #define S4OFF_TRAN
      #endif
      #ifndef S4OFF_CATALOG
         #define S4OFF_CATALOG
      #endif
      #ifndef S4OFF_SECURITY
         #define S4OFF_SECURITY
      #endif
      #ifndef S4OFF_ENFORCE_LOCK
         #define S4OFF_ENFORCE_LOCK
      #endif
   #endif
#endif

#ifdef S4UTILS
   #ifndef S4OFF_ENFORCE_LOCK
      #define S4OFF_ENFORCE_LOCK
   #endif
#endif

/* equivalency defines should go first... */

#ifdef S4OFF_INDEX
   #ifndef S4INDEX_OFF
      #define S4INDEX_OFF
   #endif
#endif

#ifdef S4INDEX_OFF
   #ifndef S4OFF_INDEX
      #define S4OFF_INDEX
   #endif
#endif

#ifdef S4OPTIMIZE_OFF
   #ifndef S4OFF_OPTIMIZE
      #define S4OFF_OPTIMIZE
   #endif
#endif

#ifdef S4OFF_OPTIMIZE
   #ifndef S4OPTIMIZE_OFF
      #define S4OPTIMIZE_OFF
   #endif
#endif

#ifdef S4OFF_MEMO
   #ifndef S4MEMO_OFF
      #define S4MEMO_OFF
   #endif
#endif

#ifdef S4MEMO_OFF
   #ifndef S4OFF_MEMO
      #define S4OFF_MEMO
   #endif
#endif

#ifdef S4OFF_MULTI
   #ifndef S4SINGLE
      #define S4SINGLE
   #endif
#endif

#ifdef S4SINGLE
   #ifndef S4OFF_MULTI
      #define S4OFF_MULTI
   #endif
#endif

#if defined(E4VBASIC) && defined(S4SERVER)
   #undef E4VBASIC
#endif

#ifndef S4OFF_PASCAL
   #ifndef S4PASCAL
      #define S4PASCAL
   #endif
#endif

/* end of equivalency defines */

#ifndef S4DISTRIBUTED
   #define S4DISTRIBUTED
#endif

#ifdef S4SERVER
   #ifdef S4OFF_SECURITY
      #undef S4OFF_SECURITY
   #endif
#else
   #ifndef S4OFF_CATALOG
      #define S4OFF_CATALOG
   #endif
   #ifndef S4OFF_SECURITY
      #define S4OFF_SECURITY
   #endif
#endif

#if defined( S4OFF_WRITE ) && !defined( S4OFF_TRAN )
   #define S4OFF_TRAN
#endif

#if defined( S4FOX ) && !defined( S4CLIENT_OR_FOX )
   #define S4CLIENT_OR_FOX
#endif

#ifdef S4CLIENT
   #ifndef S4OFF_WRITE_DELAY
      #define S4OFF_WRITE_DELAY
   #endif
   #ifndef S4OFF_READ_ADVANCE
      #define S4OFF_READ_ADVANCE
   #endif
   #ifndef S4CLIENT_OR_FOX
      #define S4CLIENT_OR_FOX
   #endif
   #ifndef S4OFF_OPTIMIZE
      #define S4OFF_OPTIMIZE
   #endif
   #ifndef S4OPTIMIZE_OFF
      #define S4OPTIMIZE_OFF
   #endif
   #ifdef S4OFF_INDEX
      #pragma message("Warning: Indexing support cannot be omitted in CodeBase client applications.")
      #undef S4OFF_INDEX
   #endif
   #ifdef S4INDEX_OFF
      #undef S4INDEX_OFF
   #endif
   #ifdef S4OFF_MEMO
      #pragma message("Warning: Memo field support cannot be omitted in CodeBase client applications.")
      #undef S4OFF_MEMO
   #endif
   #ifdef S4MEMO_OFF
      #undef S4MEMO_OFF
   #endif
   #ifdef S4OFF_MULTI
      #pragma message("Warning: Multi-user support cannot be omitted in CodeBase client applications.")
      #undef S4OFF_MULTI
   #endif
   #ifdef S4SINGLE
      #undef S4SINGLE
   #endif
#else
   #if !defined( S4MDX ) && !defined( S4FOX ) && !defined( S4CLIPPER )
      #error - Must compile with one of the indexing options (S4MDX, S4FOX, OR S4CLIPPER)
   #endif

   #if !defined( S4SERVER ) && !defined( S4STAND_ALONE )
      #error - Must compile with one of the configuration options (S4SERVER, S4CLIENT, S4STAND_ALONE)
   #endif
#endif

#if !defined(S4WIN16) && !defined(S4WIN32) && !defined(S4OS2) && !defined(S4UNIX) && !defined(S4MACINTOSH) && !defined(S4PASCAL_WIN) && !defined(S4DOS) && !defined(S4PALM)
   #error NO OPERATING SYSTEM SELECTED (S4WIN16/S4WIN32/S4DOS/S4UNIX/...)
#endif

#if !defined(S4UNIX) && !defined(S4MACINTOSH) && !defined(S4PALM)
  #define S4WINTEL   /*This will include DOS and OS2 also */
#endif

#if defined( S4WIN16 ) && !defined( S4WINDOWS )
   #define S4WINDOWS
#endif

#ifdef S4WIN32
   #if !defined( S4STAND_ALONE ) && !defined( S4OFF_THREAD ) && !defined( S4COMM_THREAD ) && !defined( S4OFF_COMMUNICATIONS )
      #define S4COMM_THREAD
   #endif
   #if !defined( S4CONSOLE ) && !defined( S4WINDOWS )
      #define S4WINDOWS
   #endif
   #ifdef __BORLANDC__
      /* borland's atof() causes problems... */
      #define S4NO_ATOF
       // LY Jan 5/05 : Borland's fcvt() not returning same results as
       // Microsoft's fcvt() e.g. Borland's fcvt() not rounding up 22.5 to 23
       // if asking for two digits returned
      #define S4NO_FCVT
   #endif
   #ifndef S4SEMAPHORE
      #define S4SEMAPHORE
   #endif
   #ifndef S4OFF_THREAD
      #if !defined( S4OFF_WRITE_DELAY ) && !defined( S4WRITE_DELAY )
         #define S4WRITE_DELAY
      #endif
      #if !defined( S4OFF_READ_ADVANCE ) && !defined( S4READ_ADVANCE )
         #define S4READ_ADVANCE
      #endif
   #endif
#endif

#ifdef S4OFF_WRITE_DELAY
   #ifdef S4WRITE_DELAY
      #undef S4WRITE_DELAY
   #endif
#endif

#ifdef S4OFF_READ_ADVANCE
   #ifdef S4READ_ADVANCE
      #undef S4READ_ADVANCE
   #endif
#endif

#ifdef S4READ_ADVANCE
   #ifndef S4ADVANCE_READ
      #define S4ADVANCE_READ
   #endif
#endif

#ifdef S4WRITE_DELAY
   // AS Dec 11/02 - Renamed for clarification
   #define S4DELAY_WRITE_MT
#else
   // AS Dec 11/02 - Renamed for clarification
   #ifdef S4READ_ADVANCE
      #define S4DELAY_WRITE_MT
   #endif
#endif

#if !defined( S4PROFILE ) && defined( S4SERVER ) && defined( S4WIN32 ) && !defined( S4COMTHREADS ) && !defined( S4OFF_THREAD )
   #define S4COMTHREADS
#endif

#ifdef S4OS2
   #define S4NO_STRUPR
   #if !defined( S4CONSOLE ) && !defined( S4OS2PM ) && !defined( S4WINDOWS )
      #define S4WINDOWS
   #endif
#endif

#if defined( S4DOS ) && !defined( S4CONSOLE )
   #define S4CONSOLE
#endif

#if defined( S4UNIX ) && !defined( S4CONSOLE )
   #define S4CONSOLE
#endif

#ifdef S4STAND_ALONE
   #ifndef S4OFF_TRAN
      #define S4STAND_ALONE_TRANS
   #endif
   #ifdef S4UNIX
      #define S4OFF_COMMUNICATIONS
   #endif
   #ifdef S4OS2
      #define S4OFF_COMMUNICATIONS
   #endif
   /* #ifndef S4UTILS */ /* switch not needed anymore unless CodeUtil connects to the server */
      #ifndef S4OFF_COMMUNICATIONS
         #define S4OFF_COMMUNICATIONS
      #endif
   /* #endif */
#endif

#if !defined( S4MACINTOSH ) && !defined( S4WINDOWS ) && !defined( S4OS2PM ) && !defined( S4CODE_SCREENS ) && !defined( S4CONSOLE ) && !defined(S4PALM)
   #error - Must compile with an output option( S4WINDOWS, S4OS2PM, S4CODE_SCREENS or S4CONSOLE )
#endif

#if defined(S4DOS) && defined(S4WIN16)
   #error Both S4DOS and S4WIN16 switches set - only one is allowed.
#endif

#if defined(S4DOS) && defined(S4WIN32)
   #error Both S4DOS and S4WIN32 switches set - only one is allowed.
#endif

#if defined(S4WIN16) && defined(S4WIN32)
   #error Both S4WIN16 and S4WIN32 switches set - only one is allowed.
#endif

#if defined( S4WINDOWS ) && defined( S4CONSOLE )
   #error - Both S4WINDOWS and S4CONSOLE switches set - only one is allowed.
#endif

#if defined( S4WINDOWS ) && defined( S4OS2PM )
   #error - Both S4WINDOWS and S4OS2PM switches set - only one is allowed.
#endif

#if defined( S4CONSOLE ) && defined( S4OS2PM )
   #error - Both S4CONSOLE and S4OS2PM switches set - only one is allowed.
#endif

#if defined( S4STAND_ALONE ) && defined( S4CLIENT )
   #error - Both S4STAND_ALONE and S4CLIENT switches set - only one is allowed.
#endif

#if defined( S4SERVER ) && defined( S4CLIENT )
   #error - Both S4CLIENT and S4SERVER switches set - only one is allowed.
#endif

#if defined( S4STAND_ALONE ) && defined( S4SERVER )
   #error - Both S4SERVER and S4STAND_ALONE switches set - only one is allowed.
#endif

#if defined( S4MDX ) && defined( S4FOX )
  #error - Both S4MDX and S4FOX switches set - only one is allowed.
#endif

#if defined( S4MDX ) && defined( S4CLIPPER )
  #error - Both S4MDX and S4CLIPPER switches set - only one is allowed.
#endif

#if defined( S4FOX ) && defined( S4CLIPPER )
  #error - Both S4FOX and S4CLIPPER switches set - only one is allowed.
#endif

#ifndef S4OFF_COMPRESS
   // Must have one defined but not both.
   #if defined(S4COMPRESS_ZLIB) && defined(S4COMPRESS_QUICKLZ)
      #error - Both S4COMPRESS_ZLIB and S4COMPRESS_QUICKLZ switches set - only one is allowed.
   #endif

   #if !defined( S4COMPRESS_ZLIB ) && !defined( S4COMPRESS_QUICKLZ )
      #error - Must compile with a compression option (S4COMPRESS_ZLIB or S4COMPRESS_QUICKLZ)
   #endif
#endif

/*   OS2 2.0 SUPPORT */
#ifndef S4UNIX    /* LY 99/11/29 : avoid if using AIX */
   #ifdef __OS2__       /* Watcom 386, Borland C++ for OS/2 */
      #define S4OS2
   #endif
   #ifdef __IBMC__  /* IBM C SET/2 */
      #define S4OS2
      #define S4IBMOS2
   #endif
   #ifdef __IBMCPP__  /* IBM C++ SET/2 */
      #define S4OS2
      #define S4IBMOS2
   #endif
#endif

#ifdef __OS2__
   #ifndef S4OS2
      #define S4OS2
   #endif
   typedef unsigned HANDLE;
   #ifdef __BORLANDC__
      #if __BORLANDC__ == 0x400
         #define S4MEMCMP
      #endif
   #endif
#endif

#if defined( S4OS2 ) && !defined( __WATCOMC__ )
   #define INCL_DOSSEMAPHORES
   #ifndef S4OS2PM
      #define INCL_NOPMAPI    /* don't include PM.H */
   #endif
   #include <os2.h>
#endif

/*                                                         Watcom                Microsoft, Metaware             Borland */
#if !defined( S4OS2 ) && ( defined( S4WINTEL ) || defined(S4PALM) )
   #if !defined( _SIZE_T_DEFINED_ ) && !defined( _SIZE_T_DEFINED ) && !defined( _SIZE_T )
      typedef unsigned size_t ;
      #define _SIZE_T_DEFINED_           /* Used to resolve Watcom 386 warning */
      #define _SIZE_T
   #endif
#endif

#ifdef S4DOS
   typedef int HANDLE ;
#endif

#ifdef S4UNIX
   typedef int HANDLE ;
   #define S4DIR   '/'
   #define S4DIRW L'/'
   #define S4DIR2   '/'
   #define S4DIR2W L'/'
   #define INVALID4HANDLE (-1)  /* UNIX */
#else
   #ifdef S4MACINTOSH
      #define S4DIR   ':'
      #define S4DIRW L':'
      #define S4DIR2  ':'
      #define S4DIR2W L':'
      #define INVALID4HANDLE (-1)  /* MAC */
      #define LPTSTR char *
   #else
      #define S4DIR   '\\'
      #define S4DIRW L'\\'
      #if defined(S4DOS)
         #define INVALID4HANDLE (-1)  /* DOS */
      #elif defined(S4OS2)
         #define INVALID4HANDLE (-1)  /* OS/2 */
      #elif defined(S4PALM)
         #define INVALID4HANDLE (0)
      #elif defined(S4WIN16)
         #define INVALID4HANDLE HFILE_ERROR  /* WIN16 */
      // LY 2003/07/31 #elif defined(S4WIN64)
      //    #define INVALID4HANDLE NULL  /* WIN64 */
      #else
         #define INVALID4HANDLE INVALID_HANDLE_VALUE  /* WIN32 */
      #endif
      #define S4DIR2 '/'
      #define S4DIR2W '//'
   #endif
#endif

#ifndef _A_RDONLY
   #define _A_RDONLY 0x01                  /* Read-only attribute  */
#endif

#ifdef __HIGHC__           /* align structures to word alignment, Metaware */
  #pragma Align_members(1)
  #pragma Off(Char_default_unsigned)
#endif

#ifdef __SC__
   #pragma SC align 1      /* align structures to word alignment, Symantec */
#endif

#ifdef __WATCOMC__
   #pragma off(unreferenced)
#endif

/* Visual FoxPro Support */
#ifdef S4FOX
   // AS Jun 20/03 - Always support S4GENERAL now, helps with easing building
   #ifndef S4GENERAL
      #define S4GENERAL
   #endif
   #ifdef S4GENERAL
      // AS Jun 25/03 - not compatible with testing
      #ifndef S4TESTING
         #ifndef S4USE_GENERAL_TAGS_IN_RELATE
            #define S4USE_GENERAL_TAGS_IN_RELATE
         #endif
      #endif
      #ifndef S4VFP_KEY
         #define S4VFP_KEY
      #endif
      #ifndef S4CODEPAGE_437
         #define S4CODEPAGE_437
      #endif
      #ifndef S4CODEPAGE_1252
         #define S4CODEPAGE_1252
      #endif
      #ifndef S4CODEPAGE_1250
         #define S4CODEPAGE_1250
      #endif
   #endif
   #ifdef S4LANGUAGE
      #error - International Language Support is done via Collating Sequences
   #endif
#else
   #ifdef S4GENERAL
      #error - S4GENERAL switch should only be set with S4FOX.
   #endif
   #ifdef S4CODEPAGE_437
      #error - CodePage Support is only compatible with S4FOX.
   #endif
   #ifdef S4CODEPAGE_1252
      #error - CodePage 1252 Support is only compatible with S4FOX.
   #endif
   #ifdef S4CODEPAGE_1250
      #error - CodePage 1250 Support is only compatible with S4FOX.
   #endif
#endif

                           /* Foreign language support */
#ifndef S4FOX   /* VFP uses codepages and collating sequences */
   #ifdef S4GERMAN
      #define S4LANGUAGE
   #endif

   #ifdef S4FRENCH
      #define S4LANGUAGE
      #define S4VMAP
   #endif

   #ifdef S4SWEDISH
      #define S4LANGUAGE
      #define S4VMAP
   #endif

   #ifdef S4FINNISH
      #define S4LANGUAGE
      #define S4VMAP
   #endif

   #ifdef S4ANSI
      #define S4VMAP
   #endif
#endif

#ifdef S4DLL_BUILD
   #define __DLL__ 1
   // AS Nov 25/02 - S4DLL_BUILD builds must also define S4DLL
   #define S4DLL
#endif

#if !defined(S4DOS) && !defined(S4WIN16) && !defined(S4WIN32) && !defined(S4MACINTOSH)
   /* LY 2001/07/27 : changed S4LINUX to S4UNIX since SOLARIS' wchar_t is 4 bytes */
   #if (!defined(_WCHAR_T) && !defined(_WCHAR_T_DEFINED)) || defined(S4UNIX)  /* CS 1999/10/07 */
      #define S4NO_WCSLEN
   #endif
#else
   #if defined(_MSC_VER) && (defined(S4DOS) || defined(S4WIN16))
      #define S4NO_WCSLEN
   #else
      #if defined(__BORLANDC__)
         #if __BORLANDC__ < 0x500
            #define S4NO_WCSLEN
         #endif
      #endif
      #if !defined(_WCHAR_T) && !defined(_WCHAR_T_DEFINED)  /* CS 1999/10/07 */
         #define S4NO_WCSLEN
      #endif
   #endif
#endif

#ifdef S4WIN32
   typedef WCHAR WSTR5 ;
#else
   /* LY 2001/07/27 : changed !S4LINUX to !S4UNIX since SOLARIS' wchar_t is 4 bytes */
   #if (defined(_WCHAR_T) || defined(_WCHAR_T_DEFINED)) && !defined(S4UNIX)
      typedef wchar_t WSTR5 ;
   #else
      typedef unsigned short WSTR5 ;
   #endif
#endif

#if defined(S4DOS) || defined(S4WIN16) || defined(S4PALM)
   #define S4NO_LONGLONG
#else
   #ifdef __BORLANDC__
      #if __BORLANDC__ < 0x0500
         #define S4NO_LONGLONG
      #endif
   #endif
#endif

/* LY 2001/07/18 : changed from S4LINUX for 64-bit Solaris */
#if !defined(S4NO_LONGLONG) && ( defined(S4UNIX) || defined( S4MACINTOSH ) )
   typedef long long __int64 ;
   typedef long long LONGLONG ;
   typedef unsigned long long ULONGLONG ;
   #ifdef S4MACINTOSH   // LY Nov 8/04 : changed from S4UNIX to S4MACINTOSH
      #define S4SEMAPHORE /* LY 2002/02/20 */
   #endif
#endif

#ifdef S4UNIX
   #define S4ERRNO           /* use global variable, 'errno' */
   #define S4NO_DUP          /* use if dup() not found/not required */
/* #define S4LSEEK        */ /* use if lseek() cannot seek past EOF */
   #define S4CASE_SEN
   #define S4STRUCT_PAD

   #ifndef S4OFF_BLAST
      #define S4OFF_BLAST
   #endif

   #ifndef S4NO_INET_ATON
      #define S4NO_INET_ATON
   #endif

   #ifndef S4OFF_REPORT
      #define S4OFF_REPORT
   #endif

   // #ifndef S4OFF_REPORT
   //    #error - CodeReporter not supported under Unix. Set S4OFF_REPORT.
   // #endif
   // #ifndef S4OFF_THREAD
   //    #error - Multithreading not supported under Unix. Set S4OFF_THREAD.
   // #endif
   // #ifndef S4OFF_THREAD
   //    #define S4UNIX_THREADS
   // #endif

   // LY Sep 10/04 : moved from preceding #ifdef block
   #define S4SEMAPHORE /* LY 2002/02/20 */
#endif

#ifndef S4WIN32
   #ifndef  S4OFF_THREAD
      #define S4OFF_THREAD  // CS 2001/02/16
   #endif
#endif

// as 02/06/00 - We currently do not support blasting...
#ifndef S4OFF_BLAST
   #define S4OFF_BLAST
#endif

#ifdef S4MACINTOSH
   #define S4BYTEORDER_3210
   #define S4BYTE_SWAP
   #define S4NO_FCVT
   #define S4NO_ECVT
   #define S4NO_FILELENGTH
   #define S4NO_STRUPR
   #define S4NO_STRLWR
   // #define S4NO_STRNICMP
   #define S4CASE_SEN
   #define S4STRUCT_PAD
   #ifndef S4OFF_REPORT
      #error - CodeReporter not supported under Macintosh. Set S4OFF_REPORT.
   #endif
   #ifndef S4OFF_THREAD
      #error - Multithreading not supported under Macintosh. Set S4OFF_THREAD
   #endif
   long MAClseek(int, long, int, int) ;
#endif

#ifdef S4WINCE
   #define S4DATA_ALIGN
   #define S4DATA_ALIG2
   #define S4NO_ATOF
   #define ecvt(a,b,c,d) _ecvt( (a),(b),(c),(d) )
   #define fcvt(a,b,c,d) _fcvt( (a),(b),(c),(d) )
   //#define S4NO_FCVT
   #define S4NO_STRLWR
   #define S4NO_STRUPR
   #define S4NO_STRNICMP
   #if !(UNDER_CE >= 211)
      #define S4NO_TOUPPER
   #endif
   #define S4UNICODE
/*   #ifndef S4OFF_REPORT   // CS 2000/06/05 remove this
      #error - CodeReporter not supported under Windows CE. Set S4OFF_REPORT.
   #endif*/
   #ifndef S4OFF_MULTI
      //#error - Multiuser is not supported under Windows CE. Set S4OFF_MULTI.
      #define S4MUTEX4LOCK /* LY 2002/11/13 */
   #endif
   #ifndef S4OFF_OPTIMIZE
      #error - Optimization is not supported under Windows CE. Set S4OFF_OPTIMIZE.
   #endif
/*   #ifndef S4OFF_TRAN
      #error - Transaction Processing is not supported under Windows CE. Set S4OFF_TRAN.
   #endif  */
#endif

#ifdef S4BYTEORDER_3210
   #define S4DO_BYTEORDER
#endif

#ifdef S4BYTEORDER_2301
   #define S4DO_BYTEORDER
#endif

#ifdef S4WIN32
   #define S4NO_FILELENGTH
   #ifndef INVALID_HANDLE_VALUE
      #define INVALID_HANDLE_VALUE (HANDLE)-1    /* for Windows NT CreateFile */
   #endif
#else
   #ifdef S4WIN16
      #define INVALID_HANDLE_VALUE (HANDLE)-1
   #endif
   #if defined( __TURBOC__ ) && !defined( __DLL__ ) && defined( S4DLL )
      #ifdef S4CBPP
         #define S4CLASS huge
      #endif
      #define S4FUNCTION far pascal
   #endif

   #ifdef _MSC_VER
      #if _MSC_VER == 600
         #define S4NO_NEGATIVE_LOCK
      #endif
      #if !defined( __DLL__ ) && defined( S4DLL )
         #ifdef S4CBPP
            #define S4CLASS
         #endif
         #define S4FUNCTION far pascal
      #endif
   #endif

   #if defined( __ZTC__ ) && defined( S4DLL ) && !defined( __DLL__ )
      #ifdef S4CBPP
         #define S4CLASS
      #endif
      #define S4FUNCTION far pascal
   #endif
#endif

#ifdef S4PASCAL_DOS
   #define S4PASCAL
#endif

#ifdef S4PASCAL_WIN
   #define S4PASCAL
#endif

#ifdef __DLL__
   #ifdef S4WIN32
      // CS 2002/10/28 Remove auto define S4DLL
      #ifdef S4CBPP
         #ifdef _MSC_VER
            #define S4CLASS __declspec(dllexport)
         #else
            #define S4CLASS _export
         #endif
      #endif
   #else
      #ifdef S4OS2
         #ifdef S4CBPP
            #define S4CLASS _export
         #endif
         #define S4FUNCTION _export
      #else
         // CS 2002/10/28 Remove auto define S4DLL
         #ifdef S4CBPP
            #define S4CLASS _export
         #endif
         #define S4FUNCTION far pascal _export
      #endif
   #endif
#endif

#if defined( S4STATIC ) && defined( S4DLL )
   #error - Both S4STATIC and S4DLL switches set - only one is allowed.
#endif

#ifdef S4WIN32
   #ifdef S4DLL
      #ifdef _MSC_VER
         #define S4FUNCTION __stdcall
      #endif
      #ifdef __BORLANDC__
         #define S4FUNCTION __stdcall _export
      #endif

      #ifdef S4DLL_BUILD
         #define S4EXPORT __declspec(dllexport)
      #else
         #define S4EXPORT __declspec(dllimport)
      #endif
   #endif
#endif

#ifdef __BORLANDC__
//   #if __BORLANDC__ < 0x500  // CS 2000/01/10  Do we need this?
      #ifdef S4EXPORT
         #undef S4EXPORT
      #endif
//   #endif
#endif

#ifndef S4FUNCTION
   #define S4FUNCTION
#endif

#ifndef S4CLASS
   #define S4CLASS
#endif

#ifndef S4EXPORT
   #define S4EXPORT
#endif

#ifdef S4DLL
   #ifdef S4WIN32
      #define S4CALL _cdecl
   #else
      #ifdef S4OS2
         #define S4CALL
      #else
         #define S4CALL far _cdecl _export
      #endif
   #endif
#else
   #ifdef _MSC_VER
      #define S4CALL _cdecl S4FUNCTION
   #else
      #ifdef __ZTC__
         #define S4CALL _cdecl S4FUNCTION
      #else
         #define S4CALL S4FUNCTION
      #endif
   #endif
#endif

#ifdef S4WINDOWS
   typedef unsigned int UINT ;
#endif

#ifndef TRUE
   #define TRUE 1
#endif
#ifndef FALSE
   #define FALSE 0
#endif

#if !defined( S4WIN32 ) && defined( S4DLL ) && !defined( S4OS2 )
   #define S4PTR far
#endif

#ifndef S4PTR
   #define S4PTR
#endif

// AS Feb 9/06 - moved together
#if defined(S4SERVER) || defined(S4CLIENT) || !defined( S4OFF_THREAD )
   #ifndef TIME4STATUS
      #define TIME4STATUS
   #endif
#endif

#ifdef S4CLIPPER
   #define N4OTHER
   /* default is to use dBASE III+/Clipper memo file formats if using .NTX index file formats */
   #define S4MNDX
   #define S4HAS_DESCENDING
   // AS Feb 9/06 - added clipper support for packwithstatus
   // #ifdef TIME4STATUS
      // not supported for clipper
   //    #undef TIME4STATUS
   // #endif
#endif

#ifdef S4FOX
   /* default is to use foxpro memo file formats if using foxpro index file formats */
   #define S4MFOX
   #define S4HAS_DESCENDING
#endif
// AS Apr 18/04 - required in client as well...
#ifdef S4CLIENT_OR_FOX
   // AS Apr 5/04 - make this a definition instead of being hard coded
   #define NULL4FLAGS_FIELD_NAME "_NullFlags"
#endif

#ifdef S4MDX
   #define S4MMDX
#endif

#ifdef S4MNDX
   #define   MEMO4SIZE   0x200
#endif

/* AS 01/21/98 --> defines the overhead amount used which limits the maximum
   size of a memo field (i.e. to contain memo header info) */
#define MEMO4OVERHEAD 128

#ifdef S4WINTEL
   typedef  const void S4PTR *  S4CMP_PARM ;
#else
   #define  S4CMP_PARM  const void *
#endif

#ifdef S4SERVER
   #ifndef S4OFF_REPORT
      #define S4OFF_REPORT
   #endif
#endif

#ifdef S4DOS
   #ifndef S4LOW_MEMORY
      #define S4LOW_MEMORY
   #endif
#endif

#ifdef S4OFF_MULTI
   #ifndef S4OFF_ENFORCE_LOCK
      #define S4OFF_ENFORCE_LOCK
   #endif
#endif

#ifdef S4TEST
   #ifndef S4TESTING
      #define S4TESTING
   #endif
   #ifndef E4HOOK
      #define E4HOOK
   #endif
   #ifndef S4NO_OUT
      #define S4NO_OUT
   #endif
#endif


// AS Aug 15/02 - don't pause if S4TESTING is defined
#ifdef S4TESTING
   // AS Nov 12/02 - allow errors with testing if requested
   // AS Jan 14/03 - turn pause switch on if off
   #ifdef E4DO_PAUSE
      #ifndef E4PAUSE
         #define E4PAUSE
      #endif
   #else
      #ifdef E4PAUSE
         #undef E4PAUSE
      #endif
   #endif
#endif

#ifdef S4COMPILE_TEST
   #ifdef S4OFF_MULTI
      #define S4COMP_OFF_MULTI
   #endif
   #ifdef S4OFF_MEMO
      #define S4COMP_OFF_MEMO
   #endif
   #ifdef S4OFF_INDEX
      #define S4COMP_OFF_INDEX
   #endif
#endif

#ifdef E4DEBUG
   #ifndef E4ANALYZE
      #define E4ANALYZE
   #endif
   #ifndef E4PARM_HIGH
      #define E4PARM_HIGH
   #endif
   #ifndef E4MISC
      #define E4MISC
   #endif
   #ifndef S4TESTING
      #ifndef E4STOP_CRITICAL
         #define E4STOP_CRITICAL
      #endif
   #endif
#endif

#if !defined( E4STOP_UNRECOVERABLE ) && !defined( S4PALM )
   #define E4STOP_UNRECOVERABLE
#endif

#ifdef E4OFF
   #ifndef E4OFF_ERROR
      #define E4OFF_ERROR
   #endif
   #ifndef E4ERROR_OFF
      #define E4ERROR_OFF
   #endif
   #ifndef E4OFF_STRING
      #define E4OFF_STRING
   #endif
#endif

#ifdef E4ANALYZE_ALL
   #ifndef E4ANALYZE
      #define E4ANALYZE
   #endif
   #ifndef E4INDEX_VERIFY
      #define E4INDEX_VERIFY
   #endif
   #ifndef E4LOCK
      #define E4LOCK
   #endif
   #ifdef S4TESTING
      /* WARNING this define only works with CodeBase test programs, else failures abound */
      #define S4MEM_TEST_OLEDB
   #endif
   // AS Jun 5/06 - more specific mutex checking
   #ifndef E4MUTEX_CHECK
      #define E4MUTEX_CHECK
   #endif
#endif

#ifdef S4SERVER
   #ifdef S4TESTING
      #define S4SLOW
   #endif
#endif

#if defined( E4ANALYZE_ALL ) && !defined( E4LINK ) && !defined( E4NO_LINK )
   #define E4LINK
#endif

#ifdef E4LINK
   #ifndef E4DEBUG
      #define E4DEBUG
   #endif
#endif

#ifdef E4ANALYZE
   #ifndef E4PARM_LOW
      #define E4PARM_LOW
   #endif
#endif

#ifdef S4OFF_INLINE
   #undef S4OFF_INLINE
#endif

#define WAIT4EVER -1
#define WAIT4NONE -2
#ifndef S4STAND_ALONE
/* connections are removed if the server is left to run overnight, so it
   looks like dead-checking is not required, thus don't define it...*/
/* #define S4DEAD_CHECK */
#endif

#ifndef S4SERVER
   #ifdef S4TESTING
      #define ATS_FILENAME_REC "RECINFO.ATS"
      #define ATS_FILENAME_CS "CSINFO.ATS"
   #endif
#endif

#ifdef S4SERVER
   #define MEMORY4START_CONNECT_LOW 20
   #define MEMORY4EXPAND_CONNECT_LOW 10
   #define MEMORY4START_CONNECT 20
   #define MEMORY4EXPAND_CONNECT 10
   #define MEMORY4START_CONNECT_BUFFER 20
   #define MEMORY4EXPAND_CONNECT_BUFFER 4
   #ifdef S4DEAD_CHECK
      #define DEAD4CHECK_SERVER_WAITTIME 4000
   #endif
#endif

#ifdef S4CLIENT
   #define MEMORY4START_CONNECT_LOW 2
   #define MEMORY4EXPAND_CONNECT_LOW 5
   #define MEMORY4START_CONNECT 2
   #define MEMORY4EXPAND_CONNECT 5
   #define MEMORY4START_CONNECT_BUFFER 2
   #define MEMORY4EXPAND_CONNECT_BUFFER 2
   #ifdef S4DEAD_CHECK
      #define INTER4WAITTIME 10
      #define DEAD4CHECK_LOOP    100
      #define DEAD4CHECK_TIMEOUT 1500
   #endif
#endif

#define TYPE4TEMP 'T'
#define TYPE4PERMANENT 'P'
#define TYPE4SCHEMA 'S'

#ifdef S4DLL
   #ifndef S4WINCE
      #define sort4assignCmp(s4,f)  (s4)->cmp = (S4CMP_FUNCTION S4PTR *) MakeProcInstance((FARPROC) f, (HINSTANCE)(s4)->codeBase->hInst)
   #else
      #define sort4assignCmp(s4,f)  (s4)->cmp = (S4CMP_FUNCTION S4PTR *) (f)
   #endif
#else
   #define sort4assignCmp(s4,f)  (s4)->cmp = (S4CMP_FUNCTION S4PTR *) (f)
#endif

/*#define TRAN4INACTIVE  110*/
/*#define TRAN4ACTIVE    120*/
/*#ifdef S4DISTRIBUTED*/
/*   #define TRAN4PARTIAL  130*/
/*#endif*/
/* #define TRAN4ROLLBACK  7 -- defined in c4trans.h */

#define S4NUM_STRING_TYPES 16

#ifndef S4SINGLE
   #define CON4LOCK                 100
   #define CON4UNLOCK               200
#endif
#define CON4WRITE                   300
#define CON4GO                      400
#define CON4SKIP                    500
#define CON4SEEK                    600
#define CON4SEEK_DBL                650
/* AS Sep 7/11 c/s support for seek long long */
#define CON4SEEK_LONGLONG           675
#define CON4START                   700
#define CON4COMMIT_PHASE_ONE        800
#define CON4COMMIT_PHASE_TWO        900
#define CON4COMMIT_BOTH_PHASES      950
#define CON4ROLLBACK               1000
#define CON4OPEN                   1100
#define CON4CLOSE                  1200
#define CON4RECCOUNT               1300
#ifndef S4SINGLE
   #define CON4LOCK_CONFIRM        1400
   #define CON4LOCK_GROUP          1500
#endif
/* #define CON4ABORT               1600 */
#define CON4CONNECT                1700
#define CON4DISCONNECT             1800
/* #define CON4DISCONNECTED        1900 */
#define CON4PACK                   2000
#define CON4ZAP                    2100
#define CON4CREATE                 2200
#define CON4CANCEL                 2300
#define CON4RELATE_INIT            2400
#define CON4RELATE_TOP             2500
#define CON4RELATE_BOTTOM          2600
#define CON4RELATE_DO              2700
#define CON4RELATE_DO_ONE          2800
#define CON4RELATE_FREE            2900
/* #define CON4RELATE_CHANGED      2950 */
#ifndef S4SINGLE
   #define CON4RELATE_LOCK         3000
   #define CON4RELATE_UNLOCK       3100
#endif
#define CON4RELATE_SKIP            3200
#define CON4INDEX_CREATE           3300
#define CON4INDEX_OPEN             3400
#define CON4INDEX_CLOSE            3500
#define CON4POSITION               3600
#define CON4POSITION_SET           3700
#define CON4REINDEX                3800
#define CON4CHECK                  3900
#define CON4TOP                    4000
#define CON4BOTTOM                 4100
#define CON4APPEND                 4200
#define CON4MEMO_COMPRESS          4300
#define CON4MEMO                   4400
#define CON4INFO                   4500
#define CON4UNIQUE_SET             4600
/*#define CON4PASSWORD             4700 */
#define CON4TRANS_INIT             4800
#define CON4RELATE_OPT             4900
#define CON4SYSTEM                 5000
#define CON4TAG_SYNCH              5100
#define CON4DATE_FORMAT            5200
#define CON4TRAN_EOF               5300
#define CON4TRAN_EOF_HALT          5400
#define CON4TRAN_RESTART           5500
#define CON4INDEX_FORMAT           5600
#define CON4INDEX_INFO             5700
#define CON4INDEX_REINDEX          5800
#define CON4ACK                    5900
#define CON4INDEX_FNAME            6000
#define CON4DATA_FNAME             6100
#define CON4ADD_TAG                6200
#define CON4CATALOG                6300
#define CON4REMOVE                 6400
#define CON4PASSWORD_SET           6500
#define CON4RESTART                6600
#define CON4CALC_CREATE            6700
#define CON4CALC_RESET             6800
#define CON4TAG_OPEN               6900
#define CON4CRASH                  7000
#define CON4CREATE_AUX_CONNECTION  7100
#define CON4CONFIG_NAME            7200
#define CON4SHUTDOWN               7300
#define CON4CONNECT_ACCEPT_NEW     7400
#define CON4CONNECT_CUT_ALL        7500
#define CON4CONNECT_CUT            7600
#define CON4CLOSE_FILES            7700
#define CON4TAG_FNAME              7800
#define CON4SERVER_OS              7900
#define CON4DATA_CODEPAGE          8000
#define CON4REMOVE_TAG             8100
// #define CON4FIELDS_ADD             8200
#define CON4VERSION                8300
// AS Apr 15/03 - support for unlock append by client
#define CON4UNLOCK_APPEND          8400

// AS Nov 7/03 - Modified d4flush behaviour in client/server to actually perform a physical file flush
// AS Dec 4/03 - was conflicting with UNLOCK_APPEND
#define CON4FLUSH                  8500

// AS May 17/04 - functionality to copmress the data file...
#define CON4DATA_COMPRESS          8600

/* AS Oct 25/05 - support for tag functionality in client/server */
#define CON4TAG_BOTTOM             8700
#define CON4TAG_COUNT              8800
#define CON4TAG_SEEK               8900
#define CON4TAG_SKIP               9000
#define CON4TAG_TOP                9100
#define CON4TAG_POSITION           9200
#define CON4TAG_POSITION_SET       9300
// #define CON4TAG_KEY                9400  - AS Feb9/09 - replaced functionality with code to get key every time instead
#define CON4TAG_EXPR_KEY           9500
#define CON4TAG_GO                 9600
#define CON4TAG_EOF                9700
// AS Feb 12/09 - new function(s) for tag key cacheing
#define CON4TAG_CACHE_SKIP         9750
// AS Jun 11/07 - new function to copy a database
#define CON4COPY                   9800
#define CON4MODIFY                 9900


#define STREAM4START_COUNT        10000

#define STREAM4DISCONNECT         10000
#define STREAM4UNLOCK_ALL         10010
#define STREAM4LOCKED_INFO        10020
#define STREAM4RECONNECT          10030
#define STREAM4UNLOCK_DATA        10040
#define STREAM4PING               10050
#define STREAM4STATUS             10060
#define STREAM4TABLES             10070
#define STREAM4CURRENT_DIRECTORY  10080
#define STREAM4DIRECTORY          10090
#define STREAM4BUSY_STATUS        10100
#define STREAM4CLIENT_PIPE_RECV        10110
#define STREAM4CLIENT_PIPE_SEND        10120
#define STREAM4CLIENT_PIPE_SEND_CHECK  10130
#define STREAM4CLIENT_PIPE_SEND_CANCEL 10140
#define STREAM4CLIENT_PIPE_OPEN_RECV   10150
#define STREAM4CLIENT_PIPE_OPEN_SEND   10160
#define STREAM4CLIENT_PIPE_CLOSE_RECV  10170
#define STREAM4RECONNECT_NEW           10180

#define STREAM4BLAST_TEST_WRITE   11100
#define STREAM4BLAST_TEST_READ    11200
#define STREAM4SET_SLOW_DELAY     11300
#define STREAM4SWAP_LOGFILE       11400

#define MSG5DB_SESSION_DATA_CLOSE     12000
#define MSG5DB_SESSION_DATA_OPEN      12100
#define MSG5DB_SESSION_DATA_CREATE    12200
#define MSG5DB_ROW_REQUEST_ARRAY      12400
#define MSG5DB_REQUEST_DEFERRED_FIELD 12500
#define MSG5DB_ROW_REQUEST_SEQUENTIAL 12600
#define MSG5DB_INDEX_REQUEST_KEY      12700
#define MSG5DB_INDEX_ROW_REQUEST      12800
#define MSG5DB_INDEX_ROW_REQUEST_KEYS 12900
#define MSG5DB_COLUMN_INFO            13000
#define MSG5DB_UPDATE_FIELDSET        13100
#define MSG5DB_RECCOUNT               13200
#define MSG5DB_SESSION_INDEX_CLOSE    13300
#define MSG5DB_SESSION_INDEX_OPEN     13400
#define MSG5DB_INDEX_COLUMN_INFO      13500
#define MSG5DB_INDEX_UPDATE_FIELDSET  13600
#define MSG5DB_SESSION_SCHEMA_OPEN    13700
#define MSG5DB_CURRENT_DIRECTORY      13800
#define MSG5DB_SET_RESTRICTIONS       13900
#define MSG5DB_ADD_DIRECTORY          14000
#define MSG5DB_TAG_SELECT             14100
#define MSG5DB_SCHEMA_SEEK            14200
#define MSG5DB_SCHEMA_REQUEST_SEQ     14300
#define MSG5DB_SCHEMA_GET_NEXT        14400
#define MSG5DB_FIND_OR_ADD_ENTRY      14500
#define MSG5DB_ADD_TAG                14600
#define MSG5DB_INDEX_REMOVE           14700
#define MSG5DB_TBL_REMOVE_INDEXES     14800
#define MSG5DB_TBL_REMOVE             14900
#define MSG5DB_SESSION_DATA_SEEK      15000
#define MSG5DB_SESSION_DATA_WRITE     15100
#define MSG5DB_SESSION_DATA_WRITE_DONE 15200
#define MSG5LEN                       15300
#define MSG5DB_SESSION_DATA_DELETE    15400
#define MSG5DB_SESSION_DATA_DEFAULTS  15500
#define MSG5DB_TBL_WRITE_RESULT_INC   15600
#define MSG5DB_INDEX_SET_RANGE        15700
#define MSG5DB_SESSION_ISO_LEVEL      15800
#define MSG5DB_SESSION_IN_TRANSACTION 15900
#define MSG5DB_ADD_COLUMN             16000
#define MSG5DB_REMOVE_COLUMN          16100
#define MSG5DB_ROWSET_DESTROY         16200
#define MSG5DB_SCHEMA_POPULATE_INDEXES 16300
#define MSG5DB_SCHEMA_POPULATE_COLUMNS 16400
#define MSG5DB_FETCHNEXTRECNO         16500
#define MSG5DB_SCHEMA_REQUEST_DIRECT  16600
#define MSG5DB_INTEGRATED_TAG_SELECT  16700
#define MSG5DB_INTEGRATED_ROW_REQUEST 16800
#define MSG5DB_INDEX_DESTROY_RANGE    16900
// #define MSG5DB_INTEGRATED_TAG_EXISTS  16900

#define MSG4TEST_SEND_RECEIVE      17000
#define MSG5DB_SESSION_UNLOCK_ALL  17100

// AS Apr 12/02 block reading of records for relate skip and d4skip
#define MSG5SKIP                   17200
#define MSG5RELATE_SKIP_MULTI      17300
#define MSG5WRITE_BATCH            17400

// AS May 13/02 data compression and preprocess
#define MSG5COMPRESS               17500
// #define MSG5PREPROCESS_P           17600
// #define MSG5PREPROCESS_PK          17700
#define MSG5PREPROCESS_B           17800
#define MSG5PREPROCESS_DISABLE     17900
// AS Sept. 3/02 - new functionality
#define MSG5ADD_FUNC               18000
// AS Oct 10/02 - client/server relate4count
#define MSG5RELATE_COUNT           18100

#ifdef S4JAVA
   #define JAVA4SINGLE_OPEN_SETTING 0

   /* same as in c4comws.h */
   #ifdef E4OFF_STRING
      #define S4SOCK_BASE_ERROR_NUM   88000L
   #else
      #define S4SOCK_BASE_ERROR_DEF   E88004
   #endif

   #define java4lockAll    -2
   #define java4lockAppend  0
   #define java4lockFile   -1

   #define java4blankField       0
   #define java4nullField       -1
   #define java4defaultFieldLen -2
   #define java4shortLen        -3
   #define java4longLen         -4
   #define java4noChange        -5
   #define java4doubleField     -6

   #define java4lenVariable      0

   /* start count is used to decide when to process a message (i.e. if > this value */
   #define JAVA4START_COUNT      20000

   #define JAVA4LOCK_GROUP       20100
   #define JAVA4UNLOCK           20200
   #define JAVA4WRITE            20300
   #define JAVA4WRITE2           20350
   #define JAVA4GO               20400
   #define JAVA4SKIP             20500
   #define JAVA4SEEK_N           20600
   #define JAVA4SEEK_DBL         20650
   #define JAVA4OPEN             21100
   #define JAVA4CLOSE            21200
   #define JAVA4RECCOUNT         21300
   #define JAVA4CONNECT          21700
   #define JAVA4DISCONNECT       21800
   #define JAVA4PACK             22000
   #define JAVA4CREATE           22200
   #define JAVA4INDEX_CREATE     23300
   #define JAVA4INDEX_OPEN       23400
   #define JAVA4POSITION         23600
   #define JAVA4POSITION_SET     23700
   #define JAVA4REINDEX          23800
   #define JAVA4TOP              24000
   #define JAVA4BOTTOM           24100
   #define JAVA4APPEND           24200
   #define JAVA4APPEND2          24250
   #define JAVA4ACCESS_MODE_SET  28000
   #define JAVA4READ_LOCK_SET    28300
   #define JAVA4STATUS_CODE      29000
   #define JAVA4STATUS_FIELDS    29100
   #define JAVA4READ_ONLY_SET    28400
   #define JAVA4REGISTER_FIELD   28600
   #define JAVA4REGISTER_FIELD2  28650
   #define JAVA4SAFETY_SET       28700
   #define JAVA4SELECT_TAG       28900
   #define JAVA4DEFAULT_UNIQUE_SET 28200
   #define JAVA4SELECT_DATA      28800
   #define JAVA4BLANK            28100
   #define JAVA4RECNO            28500
   #define JAVA4UNLOCK_AUTO_SET  29200
   #define JAVA4GET_FIELD_INFO   29300
   #define JAVA4REFRESH          29400

   #define JAVA4TRANSTART        29500   // CS 2002/04/30
   #define JAVA4TRANCOMMIT       29510
   #define JAVA4TRANROLLBACK     29520
#endif /* S4JAVA */

#define TIME4OUT 5

#ifndef S4SINGLE
   #define LOCK4OFF       0
   #define LOCK4ALL       1
   #define LOCK4DATA      2
   #define LOCK4APPEND   10
   #define LOCK4FILE     20
   #define LOCK4RECORD   30
   #define LOCK4INDEX    40
#endif

#ifndef S4STAND_ALONE
   #ifdef S4SPX
      #error IPX/SPX communication protocol not supported
   #endif

   #if defined( S4UNIX ) && defined( S4WINSOCK )
      #error Windows Sockets not supported under UNIX
   #endif

   #if !defined( S4UNIX ) && defined( S4BERKSOCK )
      #error Berkeley Sockets only supported under UNIX
   #endif

   #if !defined( S4WINSOCK ) && !defined( S4BERKSOCK ) && !defined( S4MAC_TCP ) && !defined( S4MACOT_TCP )
      #error Must compile with one of the communication options (S4WINSOCK, S4BERKSOCK, OR S4MAC_TCP)
   #endif

   #ifdef S4SPX
      #ifdef S4SERVER
         #define DEF4PROTOCOL "S4SPX.DLL"
      #else
         #define DEF4PROTOCOL "C4SPX.DLL"
      #endif
   #endif

   #ifdef S4WINSOCK
      #ifdef S4SERVER
         #define DEF4PROTOCOL "S4SOCK.DLL"
      #else
         #define DEF4PROTOCOL "C4SOCK.DLL"
      #endif
   #endif

   #if defined(S4BERKSOCK) || defined(S4MAC_TCP) || defined(S4MACOT_TCP)
      #define DEF4PROTOCOL NULL
   #endif

   #ifndef S4STAND_ALONE
      // AS May 5/11 - used if S4OFF_THREAD is defined in client
      #if defined( S4WIN32 ) && defined( S4CLIENT )
         // AS Jan 8/02 - The timeout interval to use on the reconnection only -- still waits the amount of ACCEPT_TIME
         // below (or as set by c4->acceptTimeOut), but does so in CON4LOW_ACCEPT_INTERVAL time chunks.
         #define CON4LOW_ACCEPT_INTERVAL 2
      #endif
      #ifdef S4TESTING
         // AS Aug 02/01 - test t4code5 was timing out on 60 secs because it sets up thousands of connections
         // faster than a 'debug' server could process them.
         #define CON4LOW_ACCEPT_TIME  300
      #else
         // In the server case, reduce this time to a very small time so that if a client gets stuck
         // the server is not continuously waiting for it...
         #ifdef S4SERVER
            #define CON4LOW_ACCEPT_TIME  3
         #else
            #define CON4LOW_ACCEPT_TIME  60
         #endif
      #endif
   #endif

   #define MEMORY4EXPAND_SIGNAL_ROUTINE 20
   #define MEMORY4START_SIGNAL_ROUTINE 10
   #define MEMORY4START_WRITE_MEMORY 10
   #define MEMORY4EXPAND_WRITE_MEMORY 20
   #define READ4MESSAGE_BUFFER_LEN 4096
   #define READ4MESSAGE_NUM_BUFFER 2
   #define WRITE4MESSAGE_NUM_BUFFER 10
   #define WRITE4MESSAGE_BUFFER_LEN 4096
   #define CONNECT4SPECIAL 1
   #define CONNECT4IDLE 2
   #define CONNECT4NORMAL 3
   #define CONNECT4WORKING 4
   #define CONNECT4SHUTDOWN 5
   // AS Sep 3/03 - To improve sequencing, do not allow disconnect while the connection is in the retrieving state
   // this happens when we are retrieving the message but have not exclusively taken the client yet because the
   // message has not yet arrived.  To fix a sequencing problem.
   #define CONNECT4RETRIEVING 6
   // # times client attempts connection to server...
   #define CONNECT4MAX_TRY 5

   #ifdef S4CLIENT
      #define MEMORY4START_EVENT 4
      #define MEMORY4EXPAND_EVENT 8
   #endif

   #ifdef S4SERVER
      #define MEMORY4START_EVENT 20
      #define MEMORY4EXPAND_EVENT 40
   #endif

   #define S4MAX_WRITES 2
#endif
#ifndef DEF4PROTOCOL
   #define DEF4PROTOCOL ""
#endif

#define LEN4ACCOUNT_ID   20
#define LEN4PASSWORD     20
#define LEN4GARBAGE     518
#define LEN4DATA_ALIAS   32
#define LEN4TABLE_NAME LEN4DATA_ALIAS + 4
#define LEN4TAG_ALIAS    10


//CJ June 22/01 for consistancy between different platforms LEN4PATH must be the same value.
// otherwise problem will occur during Client/Server comunications.
#if   defined( MAX_PATH )
   // #define LEN4PATH        MAX_PATH  // CS 2000/04/09 Use Microsoft's recommended size
   #define LEN4PATH 250 // all client/server structures are based on a value of 250
#elif defined( S4PALM )
   #define LEN4PATH        dmDBNameLength  // CS 2000/04/09
#else
   #define LEN4PATH        250
#endif

#define LEN4DATE_FORMAT  20
#ifdef S4CLIENT
   #define LEN4PROTOCOL    128
   #ifndef S4OFF_CATALOG
      #define LEN4USER_ID   32
   #endif
#endif

// used wihin transaction processing only
#define LEN4TRANS_USERID       10
#define LEN4TRANS_NETID        20
#define LEN5GUID         16

/* LOG4OFF is only allowed for internal system files (eg. schema tables) which are never
   logged.  All other files are logged when a transaction is occurring
   AS Aug 13/01 - Setting to allow logging into the backup log file only (only if backup avail, else == LOG4ON)
   - This is actually set to equivalent of 'LOG4TRANS' with the added feature that the log file is deleted upon
   - startup
*/

// with the primary log file disabled, it still get temporarily enabled in order to log transactions
#define log4enabled     0
#define log4disabled    1
#define log4tempEnabled 2
// AS Aug 13/01 - Setting to allow logging into the backup log file only, not the primary log file
#define log4compressed  3
#define log4tempUncompressed  4

// AS Jul 5/02 - New setting to keep log file in a permanently compressed state - does not allow
// for backup/restore log utilities to run, but does allow for transactions and server recovery.
#define LOG4COMPRESS   5
#define LOG4BACK_ONLY  4
#define LOG4OFF        3
#define LOG4ALWAYS     2
#define LOG4ON         1
#define LOG4TRANS      0

#ifdef S4FOX
   // AS 06/30/99 -- updated to include blocksize, multiplier...
   #define LEN4HEADER_WR_TAG_INDEX 0x1C
   #define LEN4HEADER_WR 0x10
#endif

#define OPEN4DENY_NONE  0
#define OPEN4DENY_RW    1
#define OPEN4DENY_WRITE 2
#define OPEN4SPECIAL    3    /* for internal use only */

#define OPT4EXCLUSIVE  -1
#define OPT4OFF         0
#define OPT4ALL         1

/* used for the # of lists in the priority chain */
#define OPT4NUM_LISTS       5

#define AUTH4ALLOW 'Y'
#define AUTH4DISALLOW ' '

#define PROT4DEFAULT 0

// compressed/preprocess flags are bits, so must be a power or 2
#define r4compressed 1
#define r4preprocessed 2
#define r4noPreprocess 0
#define r4preprocessPW 1
#define r4preprocessPK 2

#define r4queued 1
#define r4inUse  2
#define r4finished 3
#define r4canceled 4

#define r4quit   9

#define r4balanced 3
#define r4continueBalancing 4

#define r4restart  1
#define r4shutdown 2

#define r4complete 2
#define r4down 1
#define r4same 0

// AS Sep 25/01 - add support for seeking without locking if r4after returned
#define r4lockOnSuccess 2

// AS Jun 2/03 - new defines for use with memo field expressions in tags
#define r4uninitializedMemoTagExpression 0
#define r4noMemoTagExpression 1
#define r4memoTagExpression   2

/* Integer Return Codes */
#define r4off              -2
#define r4success           0
#define r4found             1     /* Primary Key Match */
#define r4after             2
#define r4eof               3
#define r4bof               4
#define r4entry             5     /* No index file entry or no record (go) */
#define r4noRecords         6
#define r4delay             7
#define r4database          8
#define r4descending       10
#define r4candidate        15
#define r4unique           20     /* Key is not unique, do not write/append */
#define r4batchUnique      21     /* Key is not unique, do not write/append (on batch) */
#define r4uniqueContinue   25 /* Key is not unique, write/append anyway */
#define r4batchUniqueContinue   26 /* Key is not unique, write/append anyway (on batch) */
/* #define r4keep             30 no longer used */
/* #define r4ignore           40 no longer used */
#define r4locked           50
#define r4noCreate         60     /* Could not create file */
#define r4noExist          64     /* File does not exist */
#define r4noOpen           70     /* Could not open file */
#define r4noTag            80     /* DataIndex::seek, with no default tag */
#define r4terminate        90     /* no relation match with terminate set */
#define r4exit            100     /* a function is requesting program termination */
#define r4continue        105     /* used internally */
#define r4inactive        110     /* transactional state */
#define r4partial         115     /* transactional state */
#define r4active          120     /* transactional state */
#define r4rollback        130     /* transactional state */
#define r4authorize       140     /* lacks authorization rights to perform action */
#define r4connected       150
#define r4logOn           160
#define r4logOpen         170
#define r4logOff          180
#define r4null            190
#define r4autoIncrement   195
// AS Mar 11/03 - support for auto-timestamp added
#define r4autoTimestamp   200
#define r4done            210
#define r4pending         215
#define r4deleted         220
#define r4timeout         225
#define r4invalid         230
#define r4schema          240
#define r4open            245
#define r4skipped         247
#define r4blankTcpAddress 250
#define r4errNetwork      260  /* network not properly setup */
#define r4hostNotFound    270  /* host name cannot be resolved to address */
#define r4hostUnavailable 280  /* host exists, but can't be reached */
#define r4numSockets      290  /* number of sockets is maxed out */
#define r4connectTimeOut  300  /* socket timed out waiting for connection */
#define r4noServerOnHost  305  /* host available, but no server running */
#define r4noConnect       310  /* error connecting, reason unknown */
#define r4notConnected    315  /* client to client messaging, attempt to send to non-connected client */



/* backward compatibility redefinitions */
#define r4no_records      r4noRecords
#define r4unique_continue r4uniqueContinue
#define r4no_create       r4noCreate
#define r4no_open         r4noOpen
#define r4no_tag          r4noTag

// AS Jun 24/02 - New functionality to skip forward in the master table only
#define r4skip 1
#define r4noSkip 2

/* collating sequence support */
#define sort4machine 0
#define sort4general 1
#define sort4croatian 2
#define sort4croatianUpper 3
#define sort4spanish 4


/* settings for batch reading - used as flags, so must be powers of 2 to allow oring together */
#define r4batchSkip       0x01
#define r4batchTop        0x02
#define r4batchBottom     0x04
#define r4batchSeekNext   0x08
#define r4batchSeekMatch  0x10
#define r4batchSeek       0x20

// internal modes to indicate what type of batch operation is currently being requested
#define BATCH4SKIP        0
#define BATCH4TOP        -1
#define BATCH4BOTTOM     -2
#define BATCH4SEEKNEXT   -3
#define BATCH4SEEKMATCH  -4
#define BATCH4SEEK       -5

#ifdef S4CLIENT_OR_FOX
   #define COLLATION4INFO_TABLE_NAME "coll4inf"
   #define COLLATION4TABLE_NAME "collate4"
   #define COLLATION4COMPRESS_TABLE_NAME "compres4"

   // 255 is also used in some cases to indicate there are no tail bytes for the character
   #define NO4TAIL_BYTES 0xFF
   #define NO4TAIL_BYTES_UNICODE 0xFFFF

   // 255 is also used in some cases to indicate an expansion/compression character
   #define EXPAND4CHAR_TO_TWO_BYTES 0xFF
   #define EXPAND4UNICODE_TO_TWO_BYTES 0xFFFF

   // need 1 extra byte per character (for subsort) normally
   #define NEED4ONE_EXTRA_BYTE 1

   // to indicate unused entries (or must generate)
   #define UNUSED4 0
   #define UNUSED4ARRAY 0
   #define MUST4GENERATE_ARRAY 0
   #define MUST4LOAD_ARRAY 0
   #define MUST4LOAD 0
#endif



/* codepage support */
#define cp0      0    // this is the same as cp437 (i.e. US oem sequence)
#define cp437    1    // this is the same as cp0 (i.e. US oem sequence)
// AS Jun 9/04 - add support for codepage 850
#define cp850    2
#define cp1252   3
// AS Dec 30/02 - Added support for CodePage 1250
// Added cp0004 to indicate unknown codepage 4, potentially for backwards support for existing data files
#define cp0004 4
// AS Feb 6/03 CodePage for Easter European is actually -56
#define cp1250   -56


/* General Disk Access Errors */
#define e4close    -10
#define e4create   -20
#define e4len      -30
#define e4lenSet   -40
#define e4lock     -50
#define e4open     -60
#define e4permiss  -61
#define e4access   -62
#define e4numFiles -63
#define e4fileFind -64
/* e4exclusive means that exclusive access was required but file not open in that mode */
#define e4exclusive -65
// #define e4driveLocked -66
// #define e4driveLocked e4permiss
// #define e4sharing -67
// #define e4sharing -e4permiss
#define e4instance -69
#define e4read     -70
#define e4remove   -80
#define e4rename   -90
#define e4unlock  -110
#define e4write   -120

/* Database Specific Errors */
#define e4data      -200
#define e4fieldName -210     /* Invalid field name */
#define e4fieldType -220
#define e4recordLen -230
#define e4append    -240
#define e4seek      -250

/* Index File Specific Errors */
#define e4entry      -300     /* Tag entry not located */
#define e4index      -310
#define e4tagName    -330
#define e4unique     -340     /* Key is not unique */
#define e4batchUnique  -345     /* Key is not unique on batch update */
#define e4tagInfo    -350     /* tag information is invalid */
#define e4candidate  -360     /* key is not unique/non-null */

/* Expression Errors */
#define e4commaExpected  -400
#define e4complete       -410
#define e4dataName       -420
#define e4lengthErr      -422
#define e4notConstant    -425
#define e4numParms       -430
#define e4overflow       -440 /* Overflow while evaluating expression */
#define e4rightMissing   -450
#define e4typeSub        -460
#define e4unrecFunction  -470
#define e4unrecOperator  -480
#define e4unrecValue     -490
#define e4unterminated   -500
#define e4tagExpr        -510 /* Expression is invalid for use in a tag - eg. DAT4 pointers */

/* Optimization Errors */
#define e4opt         -610
#define e4optSuspend  -620
#define e4optFlush    -630

/* thread management and communications errors */
#define e4event       -650
#define e4outstanding -660
#define e4signal      -670
#define e4semaphore   -680

/* Relation Errors */
#define e4relate      -710
#define e4lookupErr   -720
#define e4relateRefer -730   /* relation referred to does not exist or not initialized */

/* Report Errors */
#define e4report           -810
#define e4styleCreate      -811
#define e4styleSelect      -812
#define e4styleIndex       -813
#define e4areaCreate       -814
#define e4groupCreate      -815
#define e4groupExpr        -816
#define e4totalCreate      -817
#define e4objCreate        -818
#define e4repWin           -819
#define e4repOut           -820
#define e4repSave          -821
#define e4repRet           -822
#define e4repData          -823
/* backward compatibility redefinitions */
#define e4style_create     e4styleCreate
#define e4style_select     e4styleSelect
#define e4style_Index      e4styleIndex
#define e4area_create      e4areaCreate
#define e4group_create     e4groupCreate
#define e4group_expr       e4groupExpr
#define e4total_create     e4totalCreate
#define e4obj_create       e4objCreate
#define e4rep_win          e4repWin
#define e4rep_out          e4repOut
#define e4rep_save         e4repSave
#define e4rep_ret          e4repRet
#define e4rep_data         e4repData

/* Critical Errors */
#define e4info      -910  /* Unexpected information in internal variable */
#define e4memory    -920  /* Out of memory */
#define e4parm      -930  /* Unexpected parameter */
#define e4parmNull  e4parm_null
#define e4parm_null -935  /* Unexpected parameter - null input */
#define e4demo      -940  /* Exceeded maximum record number for demo */
#define e4result    -950  /* Unexpected result */
#define e4verify    -960
#define e4struct    -970
#ifdef S4FOX
   #define e4compatibility -980 /* AS Jan 16/03 - added for internal use only with FoxPro index format incompatibilities */
#endif

/* Library Errors */
/* call to library function calls not supported */
#define e4notIndex     -1010  /* S4OFF_INDEX */
#define e4notMemo      -1020  /* S4OFF_MEMO */
#define e4notRename    -1030  /* S4NO_RENAME */
#define e4notWrite     -1040  /* S4OFF_WRITE */
#define e4notClipper   -1050  /* S4CLIPPER */
#define e4notLock      -1060  /* S4LOCK_HOOK */
/* #define e4notHook      -1070  E4HOOK */
#define e4notSupported -1090  /* generally not supported (maybe due to server set-up) */
#define e4version      -1095  /* version mismatch */

/* MEMO errors */
#define e4memoCorrupt -1110
#define e4memoCreate  -1120

/* transaction errors */
#define e4transViolation -1200
#define e4trans       -1210
#define e4rollback    -1220
#define e4commit      -1230
#define e4transAppend -1240
#define e4transStatus -1250

/* communications errors */
#define e4corrupt     -1300
#define e4connection  -1310
// AS Sep 5/03 - new error, occurs if the server encounters a gpf in a message and thus disconnects the 'bad' client.
#define e4badClient   -1311
#define e4socket      -1320
#define e4net         -1330
#define e4loadlib     -1340
#define e4timeOut     -1350
#define e4message     -1360
#define e4packetLen   -1370
#define e4packet      -1380
#define e4connect     -1390
// AS May 31/02 - server may require clients to use preprocessing
#define e4preprocessRequired   -1395
// AS Jan 22/03 - case where client and server have mismatching encryption levels (8 bit vs. 256 bit)
#define e4preprocessMismatch   -1396

/* miscellaneous errors */
#define e4max               -1400

// AS 03/25/99 e4codeBase just means error occurred somewhere, map to -1
#define e4codeBase          -1
#define e4name              -1420
#define e4authorize         -1430
#define e4invalidUserId     -1440
#define e4invalidPassword   -1450
#define e4invalidTcpAddress -1460
#define e4connectDenied     -1470
#define e4invalidLicence    -1480


/* e4packet means the packet is corrupted */

/* all server-specific error >2100, not only e4server returned to client */

#define e4server      -2100
#define e4config      -2110
/* #define e4cat         -2120  -- not used currently */

#define r4passwordEncryption r4preprocessPW
#define r4publicEncryption r4preprocessPK
#define r4noEncryption r4noPreprocess
#define e4encryptMismatch e4preprocessMismatch

#define E4DEMO_MAX 200

/* garbage between expression and filter is length: */
#ifdef S4FOX
   #define I4MULTIPLY       1
   #define B4DO_BACK_LEVEL  3
   #define I4MAX_KEY_SIZE 240
   #define I4MAX_KEY_SIZE_COMPATIBLE 240
   #define F4MAX_NUMERIC   20
   #define F4MAX_DECIMAL   19
   #define F4DECIMAL_OFFSET 1
#else
   #define I4MULTIPLY     512
   #define F4DECIMAL_OFFSET 2
#endif
#ifdef S4CLIPPER
   #define F4MAX_NUMERIC   19
   #define F4MAX_DECIMAL   15
   #define I4MAX_KEY_SIZE 338
   #define I4MAX_KEY_SIZE_COMPATIBLE 338
#endif
#ifdef S4MDX
   #define F4MAX_NUMERIC   20
   #define F4MAX_DECIMAL   18
   /* for MDX the verify size is what is physically allowed to be dBASE
      compatible.  However, (eg. oledb) uses larger key sizes, which work
      but are incompatible */
   #define I4MAX_KEY_SIZE_COMPATIBLE 102
   #define I4MAX_KEY_SIZE 240
#endif
#ifndef I4MAX_KEY_SIZE
   #define I4MAX_KEY_SIZE 102
   #define I4MAX_KEY_SIZE_COMPATIBLE 102
#endif

#define E4ACCURACY     1.0e-13
// AS Sep 17/03 - Actually, MDX is accurate up to 20 digits with its BCD implementation
// some conversions were working incorrectly.  Plus this is only used in MDX
// #define E4ACCURACY_DIGITS  15
#ifdef S4MDX
   #define E4ACCURACY_DIGITS  20
#endif

/* if S4NO_NEGATIVE_LOCK is defined, there is no dBASE IV compatibility */

// the byte in the file to lock to avoid contention with compression writing
#define L4LOCK_POS_COMPRESS   0xEFFFFFFFL

#ifdef S4CLIENT
   #define L4LOCK_POS     1000000000L
#else
   #ifndef S4SINGLE
      #ifdef S4CLIPPER
         #define L4LOCK_POS     1000000000L
      #endif
      #ifdef S4FOX
         #define L4LOCK_POS_OLD 0x40000000L
         #define L4LOCK_POS     0x7FFFFFFEL
      #endif
      #ifdef S4MDX
         #ifdef S4NO_NEGATIVE_LOCK
            #define L4LOCK_POS_OLD 1000000000L
            #define L4LOCK_POS     2000000000L
         #else
            #define L4LOCK_POS_OLD 0x40000000L
            #define L4LOCK_POS     0xEFFFFFFFL
         #endif
      #endif
   #endif
#endif

#define LINK4PTR(p) ((LINK4 *)(p))

/* *** WARNING -- don't change I4MAX_EXPR_SIZE since code in many places
   depends on it's value to write to disk properly */

#ifdef S4MDX
   #define I4MAX_EXPR_SIZE 220
#endif
#ifdef S4FOX
   #define I4MAX_EXPR_SIZE 255
#endif
#ifdef S4CLIPPER
   #define I4MAX_EXPR_SIZE 255
#endif

#ifdef S4CLIPPER
   #define B4BLOCK_SIZE_INTERNAL 1024
#endif

#ifdef S4FOX
   #define B4BLOCK_SIZE_INTERNAL 512
#endif

#define CONNECTION4BUFFER_LEN 4096
#define E4DEBUG_INT 0x5281
#define S4SERVER_NAME_SIZE 80

#ifdef S4CB51
   #ifndef S4CONST
      #define S4CONST
   #endif
#else
   #define S4CONST const
#endif

#ifdef S4SERVER
   #ifdef S4OFF_MEMO
      #error - CodeBase Server incorrectly built with S4OFF_MEMO
   #endif
   #ifdef S4OFF_INDEX
      #error - CodeBase Server incorrectly built with S4OFF_INDEX
   #endif
   #ifdef S4OFF_WRITE
      #error - CodeBase Server incorrectly built with S4OFF_WRITE
   #endif
   #ifdef S4OFF_MULTI
      #error - CodeBase Server incorrectly built with S4OFF_MULTI
   #endif
   #ifdef S4OFF_TRAN
      // first version of simba does not support transaction processing
      #ifndef S4ODBC_BUILD
         #error - CodeBase Server incorrectly built with S4OFF_TRAN
      #endif
   #endif
   #ifdef S4OS2
      #error - CodeBase Server incorrectly built with S4OS2
   #endif
   #ifdef S4CONTROLS
      #error - CodeBase Server incorrectly built with S4CONTROLS
   #endif
   #ifdef S4VB_DOS
      #error - CodeBase Server incorrectly built with S4VB_DOS
   #endif
   #ifdef S4NETBIOS
      #error - CodeBase Server incorrectly built with S4NETBIOS
   #endif
   #ifdef S4COMFILE
      #error - CodeBase Server incorrectly built with S4COMFILE
   #endif
   #ifndef E4HOOK
      #error - CodeBase Server incorrectly built without E4HOOK
   #endif
   #ifdef S4VBX
      #error - CodeBase Server incorrectly built with S4VBX
   #endif
   #ifdef S4SQL
      #error - CodeBase Server incorrectly built with S4SQL
   #endif
   #ifdef S4OS2DLL
      #error - CodeBase Server incorrectly built with S4OS2DLL
   #endif
   #ifdef S4OS2PM
      #error - CodeBase Server incorrectly built with S4OS2PM
   #endif
   #ifdef S4MACINTOSH
      #error - CodeBase Server incorrectly built with S4MACINTOSH
   #endif
   #ifdef S4CODE_SCREENS
      #error - CodeBase Server incorrectly built with S4CODE_SCREENS
   #endif
   #ifdef S4PASCAL_DOS
      #error - CodeBase Server incorrectly built with S4PASCAL_DOS
   #endif
   #ifdef S4PASCAL_WIN
      #error - CodeBase Server incorrectly built with S4PASCAL_WIN
   #endif
   #ifdef S4OFF_COMMUNICATIONS
      /* allow for testing purposes to compile without communications, also for odbc servicing dll */
      #if !defined( S4TESTING ) && !defined( S4ODBC_BUILD )
         #error - CodeBase Server incorrectly built with S4OFF_COMMUNICATIONS
      #endif
   #endif
   #ifdef S4NT_DOS
      #error - CodeBase Server incorrectly built with S4NT_DOS
   #endif
   #ifdef S4DEBUG_LOG
      #error - CodeBase Server incorrectly built with S4DEBUG_LOG
   #endif
#endif

#ifdef S4NWSDK
   #ifdef S4DOS
      #error S4NWSDK unsupported in DOS configuration
   #endif
#endif

#define even4up( a ) ( (a) + ( ( (a) / 2 ) * 2 != (a) ) )
#define even4down( a ) ( ( (a) >> 1 ) << 1 )
#define q4oddUp( a ) ( (a) + 2 * ( (((a)/4)*4) == (a) ) )
#define q4up( a ) ( (a) + 2 * ( (((a)/4)*4) != (a) ) )
#define quad4oddUp(a)  q4oddUp(even4up(a)) /* returns 'a' so that 'a' mod 4 = 2 */
#define quad4up(a) q4up(even4up(a) )  /* returns 'a' so that 'a' mod 4 = 0 */

#ifndef S4CONV_REP  /* required for building crep2.exe */
#ifdef S4CB51
   #ifndef S4DLL_BUILD
      #ifndef S4LIB_BUILD
         #ifndef S4CBPP
            #define S4CONV( a, b )  b    /* arguement 'b' is the old naming convention */
         #else
            #define S4CONV( a, b )  union { a ; b ; }  /* creating union declaration */
         #endif
      #endif
   #endif
#endif
#endif

#ifndef S4CONV
   #define S4CONV( a, b )  a
#endif

#ifdef S4CONV
   #ifdef S4CB51
      #ifndef S4CBPP
         #define lastNode  last_node
         #define errorCode error_code
      #else
         #define createError        create_error
         #define defaultUniqueError default_unique_error
         #define exprError          expr_error
         #define fieldNameError     field_name_error
         #define goError            go_error
         #define offError           off_error
         #define openError          open_error
         #define relateError        relate_error
         #define skipError          skip_error
         #define tagNameError       tag_name_error
      #endif
   #endif
#endif

#ifdef S464BIT
   #define S4LONG int
   #define S4UNSIGNED_LONG unsigned int
#else
   #define S4LONG long
   #define S4UNSIGNED_LONG unsigned long
#endif

#ifdef S4READ_ADVANCE
   #define AR4EMPTY  1
   #define AR4SET    2
   #define AR4FULL   3
#endif

// BR 2001/04/11 No gui for unix
#ifdef S4UNIX
   #define S4OFF_SERVER_GUI
#endif

#ifdef S4SERVER
   #ifndef S4MAX_USERS
      /* unlimited users */
      #define S4MAX_USERS 0
   #endif
   #ifndef S4OFF_SERVER_GUI  // CS 2001/03/28
      #ifndef S4SERVER_GUI
         #define S4SERVER_GUI
      #endif
   #endif
#endif

// AS Oct 24/03 - Support for file_extended with MDX
#ifdef S4WIN32
   #ifdef S4FILE_EXTENDED
      // LY Mar 28/05 : allow S4FILE_EXTENDED for Clipper, so large-enabled file
      // optimization works correctly (large file support not tested)
      // #ifndef S4CLIPPER
         #ifdef _MSC_VER
            #if _MSC_VER < 1100
               #undef S4FILE_EXTENDED
            #endif
         #endif
      // #else
      //    #undef S4FILE_EXTENDED
      // #endif
   #else
      // LY Mar 28/05 : allow S4FILE_EXTENDED for Clipper, so large-enabled file
      // optimization works correctly (large file support not tested)
      // #ifndef S4CLIPPER
         #ifdef _MSC_VER
            #if _MSC_VER >= 1100  /* only for Visual C++ 5 and higher */
               #ifndef S4WINCE
                  #define S4FILE_EXTENDED
               #endif
            #endif
         #else
            #define S4FILE_EXTENDED
         #endif
      // #endif
   #endif
   #ifdef __BORLANDC__
      #if __BORLANDC__ <= 0x500
         #ifdef S4FILE_EXTENDED
            #undef S4FILE_EXTENDED
         #endif
      #endif
   #endif
#else   /* we only support extended length files with WIN 32 and 64-bit UNIX machines*/
   #ifndef S464BIT
      #ifdef S4UNIX
         #ifndef S4CLIPPER
            /* LY 2001/07/18 : removed !S4LINUX */
            #ifndef S4NO_LINUX_LFS  /* LY 2001/09/13 */
               #if (_LFS_LARGEFILE == 1)  /* environment supports 64-bit POSIX API functions */
                  #define S4FILE_EXTENDED
                  #ifndef _LARGEFILE_SOURCE  /* LY 00/05/12 - redef warnings on Solaris */
                     #define _LARGEFILE_SOURCE
                  #endif
               #endif
            #endif
         #endif
      #else
         #ifdef S4FILE_EXTENDED
            #undef S4FILE_EXTENDED
         #endif
      #endif
   #else
      #ifndef S4CLIPPER
         #ifndef S4FILE_EXTENDED
            #define S4FILE_EXTENDED
            #ifndef _LARGEFILE_SOURCE  /* LY 2002/08/20 */
               #define _LARGEFILE_SOURCE
            #endif
         #endif
      #else
         #ifdef S4FILE_EXTENDED
            #undef S4FILE_EXTENDED
         #endif
      #endif
   #endif
#endif

#ifdef S4FILE_EXTENDED
   /* Need enough room for both a large starting offset and room to grow, use middle value */
   /* this value is the hi-byte offset used for locking when using large files */
   #define S4LARGE_FILE_OFFSET 0x3FFFFFFF
#endif

#ifndef S4OFF_COMMUNICATIONS
   #define WS4MAX_PENDING_CONNECTS 10
   #ifndef S4UNIX
      #define S4NO_INET_ATON      /* A unix address conversion function */
   #endif
#endif

#ifdef S4SPEED_TEST
   #ifdef E4ANALYZE
      #undef E4ANALYZE
   #endif
   #ifdef E4DEBUG
      #undef E4DEBUG
   #endif
   #ifdef E4LINK
      #undef E4LINK
   #endif
   #ifdef E4PARM_HIGH
      #undef E4PARM_HIGH
   #endif
   #ifdef E4PARM_LOW
      #undef E4PARM_LOW
   #endif
   #ifdef E4MISC
      #undef E4MISC
   #endif
   #ifdef E4OFF_STRING
      #undef E4OFF_STRING
   #endif
   #ifdef E4INDEX_VERIFY
      #undef E4INDEX_VERIFY
   #endif
   #ifdef E4LOCK
      #undef E4LOCK
   #endif
   #ifndef S4CLIENT
      #ifdef S4OFF_OPTIMIZE
         #error should not compile with both S4OFF_OPTIMIZE and S4SPEED_TEST
      #endif
      #ifndef S4OFF_TRAN
         #error should not compile without S4OFF_TRAN and S4SPEED_TEST
      #endif
   #endif
   #ifdef S4OFF_THREAD
      #error should not compile with both S4OFF_THREAD and S4SPEED_TEST
   #endif
#endif

/* use Bool5 instead because it allows porting (esp. communications) to know
   whether or not bytes need to be swapped.
   0 is false, all else is true */
#define Bool5 unsigned char

#define false5 0
#define true5  1

#ifdef S4TRACK_FILES
   #define S4TRACK_FILES_OR_SERVER
#else
   #ifdef S4SERVER
      #define S4TRACK_FILES_OR_SERVER
   #endif
#endif

#ifdef OLEDB5BUILD
   #ifdef E4ANALYZE
      #define assert5( p ) if( !(p) ) assert5info( __FILE__,  __LINE__  )
      void assert5info( char *fName, int lineNo ) ;
   #else
      #define assert5( p )
   #endif
   #define assert5always( p )  if( !(p) ) throw Err5internal(0)
#else
   #ifdef E4ANALYZE
      #define assert5( p )  ( (p) ? 0 : error4( 0, e4result, E99999 ) )
   #else
      #define assert5( p )
   #endif
   #define assert5always( p ) ( (p) ? 0 :  error4( 0, e4result, E99999 ) )
#endif

#ifdef E4PARM_LOW
   #define assert5parmLow( p )  ( (p) ? 0 : error4( 0, e4result, E99999 ) )
#else
   #define assert5parmLow( p )
#endif

#define tran4notRollbackOrCommit 10
#define tran4rollbackOrCommit    20

#ifdef OLEDB5BUILD
   #ifdef S4DLL
      #define S5EXPORT __declspec( dllexport )
   #else
      #define S5EXPORT
   #endif

   #ifdef S4TESTING
      #ifndef S4CONFORMANCE
         #define S4CONFORMANCE
      #endif
   #endif
#endif

#ifdef S4CR2
   #ifdef S4CR2_BUILD   /* building the CodeReporter DLL */
      #define CR2EXPORT __declspec( dllexport )
   #else
      #define CR2EXPORT __declspec( dllimport )
   #endif
#else
   #define CR2EXPORT
#endif

#define MDX4EXT   "mdx"
#define MDX4EXTW L"mdx"
#define CDX4EXT   "cdx"
#define CDX4EXTW L"cdx"
#define NTX4EXT   "ntx"
#define NTX4EXTW L"ntx"
#define CGP4EXT   "cgp"
#define CGP4EXTW L"cgp"
#define DBT4EXT   "dbt"
#define DBT4EXTW L"dbt"
#define FPT4EXT   "fpt"
#define FPT4EXTW L"fpt"

#ifndef S4CLIENT
   #ifndef S4OFF_MEMO
      #ifdef S4MFOX
         #define MEMO4EXT   "fpt"
         #define MEMO4EXTW L"fpt"
      #else
         #define MEMO4EXT   "dbt"
         #define MEMO4EXTW L"dbt"
      #endif
   #endif

   #ifndef S4OFF_INDEX
      #ifdef S4CLIPPER
         #define GROUP4EXT CGP4EXT
         #define TAG4EXT NTX4EXT
         #define INDEX4EXT ""
      #else
         #define TAG4EXT ""
      #endif
      #ifdef S4FOX
         #define INDEX4EXT CDX4EXT
      #endif
      #ifdef S4MDX
         #define INDEX4EXT MDX4EXT
      #endif
   #endif
#endif



#define DBF4EXT   "dbf"
#define DBF4EXTW L"dbf"

#ifdef OLEDB5BUILD
   #ifndef E4DEBUG
      #define OLEDB5SAFE
   #endif
#endif

#define JULIAN4ADJUSTMENT 1721425L

// -1 OR (MAX ULONG -1) is used to indicate an invalid block id
#ifdef S464BIT  /* LY 2001/07/20 : node val is unsigned int in 64-bit */
   #define INVALID4BLOCK_ID UINT_MAX
#else
   #define INVALID4BLOCK_ID ULONG_MAX
#endif
#define root4needsReading( root ) ( b4node(root) == 0L || b4nodeInvalid(root) )

// AS Oct 9/02 - whether Windows or Console does not matter.
#ifdef S4WINTEL
   #define S4WIN_ALLOC
#endif

#define ASSUME4LOCKED 1

// start offset at 1000, use 1st 1000 entries for the real index values.
// these other values indicate evaluation required...(i.e. determine
// codepage)
#define collate4general 1001
#define collate4special 1002

#if defined( S4SERVER ) && ( defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD ) )
   // setup the shared memory space:
   // byte 1 = 'accept connections' flag
   #define SHARE4MEM_ACCEPT_OFFSET 0
   #define SHARE4MEM_ACCEPT_LEN 1
   // bytes 2-5 = 'transaction file in use' flag (to co-ordinate transaction file between odbc and regular codebase server)
   // we use the area to store the thread # of the process.  If we get a conflict, we wait for a short timeout, then
   // if it is still in use we check that the registered thread is still active.  If it is not, we currently error out,
   // but the alternative 'future' option is to rollback and just continue on.
   // #define SHARE4MEM_TRANSACTION_OFFSET 2
   // #define SHARE4MEM_TRANSACTION_LEN 4

   #define SHARE4MEM_LARGE_OFFSET SHARE4MEM_ACCEPT_LEN
   #define SHARE4MEM_LARGE_LEN    1
   #define SHARE4MEM_CONFIG_NAME_OFFSET  SHARE4MEM_LARGE_OFFSET + SHARE4MEM_LARGE_LEN
   #define SHARE4MEM_CONFIG_NAME_LEN LEN4PATH
   // AS Apr 24/03 - Support for clientId < -2, to allow multiple server processes (odbc) - store # in shared memory
   // store as a long
   #define SHARE4MEM_SERVER_COUNT_LEN 4
   #define SHARE4MEM_SERVER_COUNT_OFFSET SHARE4MEM_LARGE_OFFSET + SHARE4MEM_LARGE_LEN + SHARE4MEM_CONFIG_NAME_LEN

   #define SHARE4MEM_LEN  SHARE4MEM_LARGE_LEN + SHARE4MEM_ACCEPT_LEN + SHARE4MEM_CONFIG_NAME_LEN + SHARE4MEM_SERVER_COUNT_LEN

   // For the odbc transaction file co-ordination, we only need a long to store the # active transactions
   #define ODBC4TRANS_SHARE_LEN 4
   // AS Mar 27/03 - Keep these values low, it is much easier to handle then.
   #define TRAN4LOCK_START_COMMIT 100000L
   // a lock is held for every transaction in progress
   #define TRAN4LOCK_START_TRANS  200000L
   // maximum number of simultaneous transactions
   #define TRAN4LOCK_NUM_USERS  1000
#endif

// tese data types are excluded from ODBC support as long as these are defined...
#define ODBC4EXCLUDE_R5I8
#define ODBC4EXCLUDE_R5GUID

// o/s info, so client can determine server o/s (win32 vs unix)
#define OS4UNKNOWN 0x00
#define OS4WIN32   0x01
#define OS4UNIX    0x02

// used for tracking progress of slow items (reindex, pack, etc).  Currently on reindex covered
#define ACTION4NONE              0
#define ACTION4REINDEX           1
#define ACTION4INITIALIZING  32767

// ODBC registry entries...
#define ODBC4LOCK_ATTEMPTS  "Locking Attempts"
#define ODBC4COLLATION      "Collating Sequence"
#define ODBC4PROCESS_NAME   "CB4ODBC.EXE"
#define ODBC4TRIM_BLANKS    "Trim Blanks"
#define ODBC4EXPOSE_RECNO   "Expose Recno"
// AS Apr 28/03 - exposed odbc transactions via Registry in Server
#define ODBC4EXPOSE_TRANS   "Transactions"
// AS Jun 2/04 - also support for registry setting to disable caching of open tables.
#define ODBC4DISABLE_CACHE  "Disable Table Open Cache"
#define ODBC4CODE_PAGE "Code Page"
// LY Mar 22/05 : set CODE4.memStartMax
#define ODBC4MEM_START_MAX  "Optimization Buffer Maximum"

#define UPGRADE4READ_TO_READWRITE 1

// AS 08/14/00 - moved from d4date.c for mdx odbc
#define  JULIAN4ADJUSTMENT    1721425L  /* 0000/12/31 AD, or 1/12/31 BC (year 0 AD == 1 BC) */
#define  S4NULL_DATE          1.0E100   /* may not compile on some Op.Sys. */
                                        /* Set to MAXDOUBLE ?              */

// blank dates are exposed under ODBC as different in MDX that FOX.  This is because in FOX blank dates appear at the
// beginning of the index, and in MDX they appear at the end.  The dates must be exposed in order or queries return
// incorrect results
#ifdef S4MDX
   #define ODBC4BLANK_DATE_YEAR 9999
#else
   #define ODBC4BLANK_DATE_YEAR 1899
#endif

#ifndef UINT_MAX
   #define UINT_MAX 0xFFFF
#endif
#ifndef INT_MAX
   #define INT_MAX 0x7FFF
#endif
#ifndef USHRT_MAX
   #define USHRT_MAX 0xFFFF
#endif
#ifndef ULONG_MAX
   #define ULONG_MAX 0xFFFFFFFF
#endif

// AS Feb 9/06 - moved together
// #if !defined( S4OFF_THREAD )
//    #ifndef TIME4STATUS
//       #define TIME4STATUS  // CS 2001/01/15 define by default
//    #endif
// #endif

// AS Apr 24/04 - Also needed by utils, just generally define
// #ifdef S4SERVER
   // the id used by the server to indicate it's own transactional operations (opening account file, etc.)
   #define SERVER4CLIENT_ID -2L
// #endif

/* AS Sept 14/01 new functionality for validating the tables when CODE4 is initialized */
// AS Oct 22/02 - make available to be called in multi-user (if user calls directly)
#define VALIDATE4NAME "VALID4"

#if !defined( S4CLIENT ) && defined( S4TESTING ) && !defined( VALID4ENABLE )
   #ifndef VALID4ENABLE
      // in single user mode, don't enable this if disabled so that t4python can run
      #if defined( S4OFF_MULTI ) && !defined( VALID4DISABLE )
         #define VALID4ENABLE
      #endif
   #endif
#endif

/* AS Aug 22/01 - Moved these from e4expr.h since they are exposed to user, and needed for dual dll support */

/* Types normally used for Function/Operator parameters and returns */
/* all r5... are CodeBase specific, for OLE-DB types */
/* r4charBin and r4memoBin get converted to 'C' and 'M' respectively.
   the bin part affects another area of the field information */

#define  r4bin         'B'
#define  r4double      'B'
#define  r4str         'C'
#define  r4dateDoub    'd'
#define  r4date        'D'
#define  r4float       'F'
// AS Jul 20/05 - Support for new field type binary float
#define  r4floatBin    'H'
#define  r4gen         'G'
#define  r4int         'I'
#define  r4log         'L'
#define  r4memo        'M'
#define  r4numDoub     'n'
#define  r4num         'N'
#define S5USE_EXTENDED_TYPES
/* same as wide string but includes a length of data in # characters at the end of the field (at end so that memcmp
   and indexing, etc. is easy)
   All extra bytes between the end of the field and the length bytes are stored as NULLS.
   Note that the length bytes are not stored in the index since they are not needed there and it
   would vastly reduce the compressability of indexes by FoxPro.
*/
#define  r5wstrLen     'O'
// in indexes, r5ui4, r5i2, r5ui2 are all treated as 4 byte integers.
#define  r5ui4         'P'
#define  r5i2          'Q'
#define  r5ui2         'R'
/* #define  r5ui4      r4int */
/* #define  r5i2       r4int */
/* #define  r5ui2      r4int */

#define  r4dateTime    'T'
/* guid required for schema tables, to pass conformance tests */
#define  r5guid        'V'
#define  r5wstr        'W'
#define  r4unicode     r5wstr  // CS 1999/09/13
#define  r4memoBin     'X'
#define  r4currency    'Y'
#define  r4charBin     'Z'

#define  r4system      '0'     /* used by FoxPro for null field value field */
#define  r5i8          '1'     /* 8 byte long signed value (LONGLONG) */
#define  r5dbDate      '2'     /* struct DBDATE (6 bytes) */
#define  r5dbTime      '3'     /* struct DBTIME (6 bytes) */
#define  r5dbTimeStamp '4'     /* struct DBTIMESTAMP (16 bytes) */
#define  r5date        '5'     /* 'automation' date (double value representing date/time) */
#define  r5ui8         '6'     /* used for 'Cardinality' in indexes rowset */
// AS Mar 10/03 - new type added - is equivalent to r4dateTime, but supports to millisecond accuracy
#define  r4dateTimeMilli '7'

#define r4num_doub        r4numDoub
#define r4date_doub       r4dateDoub

// AS Jun 11/02 - added support for re-using of deleted rows. - if a tag of this name exists...
// AS Dec 15/03 - not supported in clipper because the tag file name issue (end up with many files of same name)
#ifndef S4CLIPPER
   #define DEL4REUSE_TAG_NAME "DEL4REUSE"
#endif

// AS Jan 17/02 - allow to print info on a selected tag only... -- don't define in order to track all tags
#ifdef I4PRINT
   #define I4PRINT_TAG_NAME "TRNSDATE"
#endif

// AS Feb 17/04 - compression should always be available now, evein in stand/alone
// #if !defined( S4STAND_ALONE ) && defined( S4WIN32 ) && !defined( S4COMPRESS )
// AS Dec 6/06 - allow to turn compression off in stand-alone
#if defined( S4WIN32 ) && !defined( S4WIN64 ) && !defined( S4COMPRESS ) && !defined( S4OFF_COMPRESS )
   // always use compressed messages if possible
   #define S4COMPRESS
#endif

#ifdef S4COMPRESS
   // AS Jul 16/03 - declaration was missing for client/server (resides in c4code.c)
   #define COMPRESS4MAX_LENGTH 16384
#endif


// AS Jul 28/03 Always compile with S4ENCRYPT_HOOK now.  If the encryption dll is not available or the functions
// are not called it is just disabled.
// AS Sep 11/03 - don't auto-define in server odbc build
// AS Sep 23/06 - enable for the odbc build...
//#if defined( S4SERVER ) && (defined( S4ODBC_ENABLED ) || defined( S4ODBC_BUILD ))
//#else
   // AS Nov 12/04 - support for mdx encryption...now don't default to encryption for clipper
   // LY Mar 30/04 : added S4CLIPPER so code4encryptConnection is exported
   // AS Apr 24/06 -- don't support encryption in Clipper Stand/Alone (yet)
   #if !defined( S4ENCRYPT_HOOK ) && (defined( S4CLIENT_OR_FOX ) || defined( S4MDX ) || (defined( S4CLIPPER ) && defined( S4CLIENT) ) )
      #define S4ENCRYPT_HOOK
   #endif
//#endif


#if defined( S4WIN32 ) && (defined( S4ENCRYPT_HOOK ) || defined( S4ENCRYPT_COM ) ) && !defined( S4DLL_BUILD_ENCRYPT )
   #ifndef ENCRYPT4NO_DLL
      // in this instance we use dynamic loading of encryption if available
      #define S4ENCRYPT_DLL
   #endif
#endif

#ifdef S4ENCRYPT_HOOK
   #ifndef S4PREPROCESS_FILE
      #define S4PREPROCESS_FILE
   #endif
   #ifndef S4ENCRYPT_FILE
      #define S4ENCRYPT_FILE
   #endif
#endif

#ifdef S4ENCRYPT_COM
   #ifndef S4PREPROCESS_COM
      #define S4PREPROCESS_COM
   #endif
#endif

#if defined( S4ENCRYPT_COM ) || defined( S4ENCRYPT_HOOK )
   #ifdef S4STAND_ALONE
      // AS Jan 16/06 - issue here with u4dll.dll.  If we are using u4dll.dll (utilities) with encryption, we should be liking back
      // to the u4dll from the encryption dll.  This can only be done if the encryption dll is built to link back to u4dll.dll.
      // therefore there are new encryption dll's that link back to the u4dll.dll with a 'u' attached to their names.
      // those dll's are built to link back to u4dll.
      #ifdef S4UTILS
         #define ENC4DLL8BIT_NAME "encrypt8u.dll"
         #define ENC4DLL256BIT_NAME "encrypt4u.dll"
      #else
         #define ENC4DLL8BIT_NAME "encrypt8.dll"
         #define ENC4DLL256BIT_NAME "encrypt4.dll"
      #endif
   #endif
   #ifdef S4SERVER
      // AS Oct 2/06 - for odbc components, we need a special dll that links back to the ODBC CodeBase components
      #ifdef S4ODBC_BUILD
         #define ENC4DLL8BIT_NAME "encrypt8so.dll"
         #define ENC4DLL256BIT_NAME "encrypt4so.dll"
      #else
         #define ENC4DLL8BIT_NAME "encrypt8s.dll"
         #define ENC4DLL256BIT_NAME "encrypt4s.dll"
      #endif
   #endif
   #ifdef S4CLIENT
      #define ENC4DLL8BIT_NAME "encrypt8c.dll"
      #define ENC4DLL256BIT_NAME "encrypt4c.dll"
   #endif
#endif

// AS Sep 15/04 - support removedKeys between TAG4 structures (at least in stand-alone)
#ifdef S4STAND_ALONE
   #define SHARE4TAG_REMOVE
#endif

// AS Aug 28/02 - was in wrong place (not at end of file)
#endif /* D4DEFS_INC */

