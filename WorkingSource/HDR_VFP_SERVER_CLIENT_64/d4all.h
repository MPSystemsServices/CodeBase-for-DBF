/* d4all.h   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#ifndef D4ALL_INC
#define D4ALL_INC

/**********************************************************************/
/**********            USER SWITCH SETTINGS AREA            ***********/

/* CodeBase configuration */
#define S4CLIENT
/* #define S4STAND_ALONE */

/* Index File compatibility options */
#ifndef S4CLIENT
/* #define S4CLIPPER */
#define S4FOX
/* #define S4MDX     */
#endif
#define _CRT_SECURE_NO_WARNINGS 1
/* Output selection (alternatives to default) */
/* #define S4CONSOLE */

/* Specify Library Type (choose one) */
/* #define S4STATIC */
#define S4DLL

/* Choose Operating System */
#define S4WIN32       /* for Windows NT and Windows 95 */
/* #define S4WINCE      */   /* for Windows CE */
/* #define S4UNIX       */   /* requires CodeBase Portability version */
/* #define S4MACINTOSH  */   /* requires CodeBase Mac version */
/* #define S4WIN64     */   /* for IA-64 Windows */

/* Selected default communications option */
/* #define S4BERKSOCK */
/* #define S4MAC_TCP  */
/* #define S4MACOT_TCP */
   #define S4WINSOCK

/* Alterable CodeBase Global Defines */
#define DEF4SERVER_ID "localhost"
#define DEF4PROCESS_ID "23165"

/* General Configuration Options */
/* #define S4LOCK_HOOK    */
/* #define S4MAX          */
/* #define S4SAFE         */
/* #define S4TIMEOUT_HOOK */

/* #define S4ENCRYPT_COM */
/* #define S4ENCRYPT_HOOK */
/* #define S4ENCRYPT_AES */
/* #define S4ENCRYPT_PUBLIC */
/* #define S4TEST_KEEP_FILES */
/* #define S4INTUIT */

/* Error Configuration Options */
/* #define E4ANALYZE    */
/* #define E4DEBUG      */
/* #define E4HOOK       */
/* #define E4LINK       */
/* #define E4MISC       */
   #define E4VBASIC
/* #define E4OFF        */
/* #define E4OFF_STRING */
   #define E4PARM_HIGH
   #define E4PAUSE
/* #define E4DO_PAUSE */
/* #define E4FILE_LINE  */
/* #define E4STOP       */
/* #define E4STOP_CRITICAL  */
/* #define E4MAC_ALERT 4444 */

/* Library Reducing Switches */
/* #define S4OFF_INDEX    */
/* #define S4OFF_MEMO     */
/* #define S4OFF_MULTI    */
/* #define S4OFF_OPTIMIZE */
 #define S4OFF_REPORT
/* #define S4OFF_TRAN     */
/* #define S4OFF_WRITE    */
#ifdef S4STAND_ALONE
#define S4OFF_COMPRESS
/* Above is turned off because we aren't using the zlib.dll tool to */
/* compress the contents of memo fields. Sept. 13, 2023. J. Heuer. */
/* #define S4OFF_THREAD   */
#endif
#ifdef S4CLIENT
#define S4OFF_COMPRESS
#endif

#ifdef S4FOX
   /* FoxPro collating sequence support (select none, some or all) */
   #define S4GENERAL       /* Supports German FoxPro 2.5a and Visual FoxPro with general collating sequences */
   #ifdef S4GENERAL
      /* #define S4USE_GENERAL_TAGS_IN_RELATE  */
   #endif

   /* FoxPro codepage support (select none, some or all) */
   #define S4CODEPAGE_437   /* U.S. MS-DOS CodePage */
   #define S4CODEPAGE_1252  /* WINDOWS ANSI CodePage */
   #define S4CODEPAGE_1250  /* WINDOWS Eastern European */
#else
   /* Spoken Language Switches */
   /* #define S4ANSI         */
   /* #define S4DICTIONARY   */
   /* #define S4FINNISH      */
   /* #define S4FRENCH       */
   /* #define S4GERMAN       */
   /* #define S4NORWEGIAN    */
   /* #define S4SCANDINAVIAN */
   /* #define S4SWEDISH      */
#endif

#ifndef S4OFF_COMPRESS
   #define S4COMPRESS_ZLIB
   //#define S4COMPRESS_QUICKLZ
#endif

#ifdef S4MACINTOSH
   #define S4CARBON_APP
#endif

/* Special switches required for internal testing - required in conjunction with d4switch */
/* #define S4TESTING */
/* #define S4OFF_ENCRYPT_TEST */

#define S4VERSION 6503014
#define WINDOWS_IGNORE_PACKING_MISMATCH 1
#include "d4inc.h"

#endif /* D4ALL_INC */
