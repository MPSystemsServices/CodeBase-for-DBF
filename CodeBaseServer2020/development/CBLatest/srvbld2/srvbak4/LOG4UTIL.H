/* log4util.h   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#ifndef __cplusplus
   #if defined(__BORLANDC__)
      #error Must perform C++ compile. Use the -P compiler option.
   #elif defined(_MSC_VER)
      #error Must perform C++ compile. Use the -TP compiler option.
   #else
      #error Must perform C++ compile.
   #endif
#endif

#include "u4trans.h"

#ifdef S4OLEDEBUG_PRINT
   #include "util5.hpp"
#endif

#define OPENFIELDSIZE 19
#define OPENFIELDTAGSIZE 4
#define TRANFIELDSIZE 3
#define TRANFIELDTAGSIZE 2
#define CREATEFIELDSIZE 3
#define CREATETAGSIZE 2
#define INITFIELDSIZE 9
#define INITTAGSIZE 2
#define VOIDFIELDSIZE 3
#define VOIDTAGSIZE 2

extern FIELD4INFO voidFields[VOIDFIELDSIZE] ;
extern TAG4INFO voidTags[VOIDTAGSIZE] ;
extern FIELD4INFO openFields[OPENFIELDSIZE] ;
extern TAG4INFO openTags[OPENFIELDTAGSIZE] ;
extern FIELD4INFO tranFields[TRANFIELDSIZE] ;
extern TAG4INFO tranTags[TRANFIELDTAGSIZE] ;
extern FIELD4INFO createFields[CREATEFIELDSIZE] ;
extern TAG4INFO createTags[CREATETAGSIZE] ;
extern FIELD4INFO initFields[INITFIELDSIZE] ;
extern TAG4INFO initTags[INITTAGSIZE] ;

extern char *statusStringBlank ;
extern char *statusStringMessg ;
extern char *statusStringBckSp ;
extern char *statusStringFormt ;
/* BCR 12/07/00 -- statusStringSpace is declared in log4util.c */
//extern char *statusStringSpace ;

#ifdef S4DLL_BUILD
   //extern FILE *stdout, *stderr ;
   extern unsigned int messageType ;
#endif

#ifdef S4WINDOWS
   extern HWND hWndStatus;  /* status window handle */
#else
   extern long hWndStatus;
#endif

extern short fractionProgress ;
extern bool silent ;

/****************************************************************************
**::  Global scope types and variables
****************************************************************************/

#define UTIL4ADD        0
#define UTIL4ADDR       1
#define UTIL4ALL        2
#define UTIL4INCLUDE    3
#define UTIL4LOG        4
#define UTIL4MISC       5
#define UTIL4MULTIPLE   6
#define UTIL4NETID      7
#define UTIL4ORIGINAL   8
#define UTIL4OUTDIR     9
#define UTIL4OVERWRITE  10
#define UTIL4REMOVE     11
#define UTIL4TIME       12
#define UTIL4USERID     13
#define UTIL4BACKLOG    14
#define UTIL4COMPRESS   15
#define UTIL4EXCLUSIVE  16
#define UTIL4INDEX      17
#define UTIL4FORCE      18
#define UTIL4PREPEND    19
#define UTIL4PRIMARY    20
#define UTIL4ENCKEY     21
#ifdef S4UTIL_CONCURRENCY
   #define UTIL4CONFIG     22
#endif

static UTIL4FLAG u4flags[] =
{
   /* 0*/ { "ADd",       0, 2, 2,  3, 0, 0, 0, 0, "[-add <data.dbf> [<original.dbf>] [<output.chg>]]\r\n" },
   /* 1*/ { "ADDR",      0, 4, 2,  2, 0, 0, 0, 0, "[-addr <target.dbf> [<destination.dbf>]]\r\n" },
   /* 2*/ { "ALl",       0, 2, 1,  1, 1, 0, 0, 0, "[-all {OFF | ON}]\r\n"            },
   /* 3*/ { "Include",   0, 1, 2,  6, 0, 0, 0, 0, "[-include [ALL] [INITIAL] [MEMO] [OPEN] [ROLLBACK] [TEMP] [UPDATE]]\r\n" },
   /* 4*/ { "Log",       0, 1, 1,  1, 1, 0, 0, 0, "{-log <server.log>}\r\n"          },
   /* 5*/ { "MIsclen",   0, 2, 1,  1, 1, 0, 0, 0, "[-misclen <#>]\r\n"               },
   /* 6*/ { "MUltiple",  0, 2, 1,  1, 1, 0, 0, 0, "[-multiple {OFF | ON}]\r\n"       },
   /* 7*/ { "Netidlen",  0, 1, 1,  1, 1, 0, 0, 0, "[-netidlen <#>]\r\n"              },
   /* 8*/ { "ORiginal",  0, 2, 1,  1, 1, 0, 0, 0, "[-original {OFF | ON}]\r\n"       },
   /* 9*/ { "OUtdir",    0, 2, 1,  1, 1, 0, 0, 0, "[-outdir <output directory>]\r\n" },
   /*10*/ { "OVerwrite", 0, 2, 1,  1, 1, 0, 0, 0, "[-overwrite {OFF | ON}]\r\n"      },
   /*11*/ { "Remove",    0, 1, 2,  1, 1, 0, 0, 0, "[-remove <data.dbf>]\r\n"         },
   /*12*/ { "Time",      0, 1, 1,  1, 1, 0, 0, 0, "[-time {OFF | ON}]\r\n"           },
   /*13*/ { "Useridlen", 0, 1, 1,  1, 1, 0, 0, 0, "[-useridlen <#>]\r\n"             },
   /*14*/ { "Backlog",   0, 1, 1,  1, 1, 0, 0, 0, "{-backlog <backup.log>}\r\n"      },
   /*15*/ { "COMpress",  0, 3, 1,  1, 1, 0, 0, 0, "[-compress {OFF | ON}]\r\n"       },
   /*16*/ { "Exclusive", 0, 1, 1,  1, 1, 0, 0, 0, "[-exclusive {OFF | ON}]\r\n"      },
   /*17*/ { "Index",     0, 1, 1,  1, 1, 0, 0, 0, "[-index {OFF | ON}]\r\n"          },
   /*18*/ { "Force",     0, 1, 1,  1, 1, 0, 0, 0, "[-force {OFF | ON}]\r\n"          },
   /*19*/ { "PREpend",   0, 3, 1,  1, 1, 0, 0, 0, "{-prepend <prepend.log>}\r\n"     },
   /*20*/ { "PRImary",   0, 3, 1,  1, 1, 0, 0, 0, "[-primary { OFF | ON }]\r\n" },
   /*21*/ { "ENCkey",    0, 3, 1,  1, 1, 0, 0, 0, "[-enckey <@encryptionkey.dat>]\r\n" },  // CS 2009/05/27
#ifdef S4UTIL_CONCURRENCY
   /*22*/ { "Config",    0, 1, 1, -1, 0, 0, 0, 0, "{-config [<config.dbf>]}\r\n"     },
#endif
} ;

#ifdef S4UTIL_CONCURRENCY
   static int u4flagsNum = 23 ;
#else
   static int u4flagsNum = 22 ;
#endif


void askPhaseOneQuestion(DATA4 *, TRAN4 *) ;
void calculateDisplayProgress( unsigned S4LONG position, unsigned S4LONG totalLength, short section ) ;
short findIncompleteTran(CODE4 *c4, DATA4 *openDbf, DATA4 *tranDbf, DATA4 *initDbf, DATA4 *voidDbf ) ;
int getLogFileName(CODE4 *c4, UTIL4FLAG *u4flag, char **logFileName, char **prependFileName, char **serverName, char **protocol, char **processId) ;
void freeField4( FIELD4INFO *fields, short numFields) ;
void freeTag4( TAG4INFO *tags, short numTags) ;
short getFileInfoFromLogEntry( char *data, FIELD4INFO **origFields, TAG4INFO **origTags, short *logNumFields, short *logNumIndexes ) ;
int setupVoidTransUsage( DATA4 *voidDbf, S4LONG *position, short *oneOrTwo ) ;
int setEncryptionKey(CODE4 *c4, UTIL4FLAG *u4flags);

#ifdef S4DLL_BUILD
   #ifdef __cplusplus
      extern "C" {
   #endif
   S4EXPORT int S4FUNCTION  mainLog4Back(int argc, char *argv[], int hWnd ) ;
   S4EXPORT int S4FUNCTION  mainLog4Dbf(int argc, char *argv[], int hWnd ) ;
   S4EXPORT int S4FUNCTION  mainLog4Fix(int argc, char *argv[], int hWnd ) ;
   S4EXPORT int S4FUNCTION  mainLog4Res(int argc, char *argv[], int hWnd ) ;
   S4EXPORT short S4FUNCTION  log4recoverBackup( char *prependLogFile, char *backupLogFile, char *pathToBackup, short saveChanges, short silentMode, int hWnd ) ;
   #ifdef __cplusplus
      }
   #endif
#endif


#include "push4.h"
class Tran4combined
{
public:
   Tran4combined( short switchValue ) ;
   Tran4combined( char *prependFile, char *backupFile, bool silentMode ) ;
   Tran4combined( char *tranFile ) ;
   ~Tran4combined() ;

   CODE4 *code1, *code2, *workingCode4 ;
   TRAN4 *tran1, *tran2, *working ;
   char ppFile[LEN4PATH] ;
   char *backFile ;
   short oneOrTwo ;
   bool topOrBottom, initialized, silent ;
   const char *outMess[5] ;

   S4LONG lockTranFileforProcess( CODE4 *c4, char *outFileName ) ;
   S4LONG clearAndReplace( char *newPrependFile ) ;
   int init( char *logfile1, char *logfile2, bool create ) ;

   //Move Functions
   S4LONG top() ;
   S4LONG bottom() ;
   S4LONG next() ;
   S4LONG previous() ;
   S4LONG refresh() ;
   S4LONG getTranPosition(short *isPrePend ) ;
   S4LONG setTranPosition( S4LONG position, short OneOrTwo ) ;
   void closeFiles() ;


   //Information functions
   inline short type() { return( tran4type(working) ) ; }
   inline S4LONG id() { return( tran4id(working) ) ; }
   inline S4LONG clientId() { return( tran4clientId(working) ) ; }
   inline S4LONG clientDataId() { return( tran4clientDataId(working) ) ; }
   inline S4LONG serverDataId() { return( tran4serverDataId(working) ) ; }
   inline S4LONG dataLen() { return( tran4len(working) ) ; }
   inline S4LONG year() { return( working->header.time.year ) ; }
   inline LOG4HEADER *header() { return( &(working->header) ) ; }
   inline char month() { return( working->header.time.month) ; }
   inline char day() { return( working->header.time.day ) ; }
   inline S4LONG secs() { return( working->header.time.seconds ) ; }
   inline void *data() { return( tran4getData(working, 0L ) ) ; }
   S4LONG totalLen() ;
   S4LONG totalToPosition( ) ;

   //Retrival functions
   TRAN4 *getTran( ) { return ( working ? working : 0 ) ; }
   TRAN4FILE *getTranFile() { return( workingCode4 ? &(workingCode4->transFile) : 0 ) ; }
   CODE4 *getCode4( ) { return( working ? working->c4trans->c4 : 0 ) ; }
   CODE4 *get2Code4() { return( code2 ) ; }
   CODE4 *get1Code4() { return( code1 ) ; }
} ;
#include "pop4.h"

bool IsThisVoidTrans( Tran4combined *combined, DATA4 *voidDbf, S4LONG *position, short *oneOrTwo, int *voidRc ) ;
S4LONG findIncompleteCombinedTran(Tran4combined *combined, DATA4 *openDbf, DATA4 *tranDbf, DATA4 *initDbf, DATA4 *voidDbf ) ;
