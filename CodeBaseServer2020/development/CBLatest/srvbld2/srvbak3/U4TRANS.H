/* u4trans.h   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#ifdef S4UTILS

   #include "push4.h"
   typedef struct UTIL4FLAG_st
   {
      char  *name;
      char   caseSens;   /* true if name is case sensitive */
      unsigned int    len;        /* minimum length flag has to match name */
      int    multiples;  /* multiple occurences of flag: 2 == add, 1 == keep, 0 == discard multiples, -1 == error on multiples */
      int    values;     /* number of values flag has following: 0 == none, +x == 0<n<=+x, -x == 0<=n<=+x */
      int    exact;      /* if true then for value: x == n=+x */
      int    found;      /* true if flag has been found */
      char **ptr;        /* first byte of each string == nth occurence if multiples == add and !exact */
      unsigned int numPtr;     /* number of index entries for ptr times 4 */
      char  *descrip;    /* help description of flag */
   } UTIL4FLAG;
   #include "pop4.h"

   #ifdef __cplusplus
      extern "C" {
   #endif

   #ifdef S4UNIX
      char *ltoa( long lVal, char *ptr, int num) ;
   #endif

   #ifdef S4WINDOWS
      void util4out(CODE4 *c4, FILE *stream, const char *str[], int numStr, HWND hWnd=NULL);
   #else
      void util4out(CODE4 *c4, FILE *stream, const char *str[], int numStr, long hWnd=0);  // CS 2002/10/17
   #endif
   void util4printCommandLine(CODE4 *c4, UTIL4FLAG u4[], int numFlags, char *programName);
   void util4flagsFree(UTIL4FLAG u4[], int numFlags);
   int util4parseCommandLine(CODE4 *c4, UTIL4FLAG u4[], int numFlags, char* argv[], int argc);
   char *util4rightJ(char *str, int len);
   int util4checkOnOffCommandLine(CODE4 *c4, UTIL4FLAG u4[], int flag, int def);
   long util4checkRangeCommandLine(CODE4 *c4, UTIL4FLAG u4[], int flag, long def, long lRange, long hRange);
   void askPhaseOneQuestion(DATA4 *tranDbf, TRAN4 *t4 );
   #ifdef S4WINDOWS
      void S4FUNCTION sendStatusMessage( HWND, int ) ;
      void S4FUNCTION sendCloseMessage( HWND ) ;
      S4EXPORT int S4FUNCTION  getLogDatabases( char *logFileName, int hWndListBox ) ;
   #endif

   #if !defined(S4OFF_TRAN) && !defined(S4OFF_COMMUNICATIONS)
      #define r4noServer 5

      int tran4serverEof( TRAN4 S4PTR *, unsigned long S4PTR * ) ;
      int tran4serverEofHalt( TRAN4 S4PTR *, unsigned long S4PTR * ) ;
   #endif
   #ifdef __cplusplus
      }
   #endif
#endif
