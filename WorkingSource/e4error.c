/* e4error.c   (c)Copyright Sequiter Software Inc., 1988-2001. All rights reserved. */

#include "d4all.h"

#ifdef S4PALM
   #include "e4palm.h"
#endif

#ifdef __TURBOC__
   #pragma hdrstop
#endif   /* __TUROBC__ */

#ifdef S4WINTEL
   #if !defined(S4WINDOWS) && !defined(S4WINCE)    /* LY 00/07/04 */
      #include <conio.h>
   #endif  /* not S4WINDOWS */
#endif   /* S4WINTEL */

#ifdef S4OS2PM
   #define  E4MSGBOXID 9513
#endif

#ifdef S4VB_DOS
  #define V4ERROR  1
  #define V4SEVERE 2
#endif

#ifdef S4OS2PM
   #ifndef E4ERROR_OFF
      #ifndef E4OFF_STRING
    extern ERROR4INFO_ARRAY *error4array ;
      #endif
   #endif
#endif
int S4FUNCTION error4file( CODE4 *c4, S4CONST char *name, const int overwrite )
{
   int rc, oldOpenError, oldCreateError, oldSafety ;

   if ( c4->errorLog != 0 )
      return -1 ;

   c4->errorLog = (FILE4 *)u4allocEr( c4, sizeof( FILE4 ) ) ;
   if ( c4->errorLog == 0 )
      return e4memory ;
   // LY 2003/07/31 #ifdef S4WIN64 /* LY 00/09/20 */
   //    c4->errorLog->hand = NULL ;
   // #else
      c4->errorLog->hand = INVALID4HANDLE ;
   // #endif

   oldSafety = c4->safety ;
   if ( overwrite == 0 )
   {
      oldOpenError = c4->errOpen ;
      c4->errOpen = 0 ;
      // AS May 24/02 - created file4openInternal for internal use to indicate file types
      rc = file4openInternal( c4->errorLog, c4, name, 1, OPT4NONE ) ;
      c4->errOpen = oldOpenError ;
      if ( rc != r4noOpen && rc != r4noExist )  /* if open failed, try create */
         return rc ;
      c4->safety = 1 ; /* file shouldn't exist */
   }
   else
      c4->safety = 0 ; /* ignore setting for overwrite */

   oldCreateError = c4getErrCreate( c4 ) ;
   c4setErrCreate( c4, 0 ) ;
   // AS May 24/02 - created file4createInternal for internal use to indicate file types
   rc = file4createInternal( c4->errorLog, c4, name, 1, OPT4NONE ) ;
   c4setErrCreate( c4, oldCreateError ) ;
   c4->safety = oldSafety ;

   return rc ;
}


void S4FUNCTION error4callback( CODE4 S4PTR *c4, ERROR_CALLBACK errorCallback )  // CS 2001/03/26 add
{
   c4->errorCallback = errorCallback ;
}


static void error4logAppendNewLine( FILE4 *errorLog )
{
   FILE4LONG len ;

   len = file4lenLow( errorLog ) ;
   file4writeInternal( errorLog, len, "\r\n", 2 ) ;
}

void error4logAppend( CODE4 *c4, int errCode1, long errCode2, const char *desc1, const char *desc2, const char *desc3 )
{
   int slen ;
   char num[11] ;
   const char *ptr ;
   FILE4 *errorLog ;
   FILE4LONG pos ;
   char buffer[9];
   char dateStr[32];

   if ( c4->errorLog == 0 )
      return ;

   // LY 2003/07/31 #ifdef S4WIN64 /* LY 00/09/20 */
   //    if ( c4->errorLog->hand == NULL )
   // #else
      if ( c4->errorLog->hand == INVALID4HANDLE )
   // #endif
      return ;

   /* in case one of the file writes fails, avoid endless error loop */
   errorLog = c4->errorLog ;
   c4->errorLog = 0 ;

   date4today( buffer ) ;
   date4format( buffer, dateStr, "CCYY/MM/DD  " ) ;
   date4timeNow( buffer ) ;
   c4strncat( dateStr, sizeof( dateStr ), buffer, 8 ) ;  // AS Dec 13/05 vs 5.0 fixes

   pos = file4lenLow( errorLog ) ;
   file4writeInternal( errorLog, pos, dateStr, c4strlen( dateStr ) ) ;
   error4logAppendNewLine( errorLog ) ;

   // AS Oct 5/04 if the errCodes are both 0, don't log it, just log the description - allows flexibility with general logging
   if ( errCode1 != 0 || errCode2 != 0 )
   {
      c4memset( num, 0, sizeof( num ) ) ;
      c4ltoa45( (long)errCode1, num, sizeof( num ) - 1 ) ;

      pos = file4lenLow( errorLog ) ;
      file4writeInternal( errorLog, pos, num, c4strlen( num ) ) ;
      error4logAppendNewLine( errorLog ) ;

      ptr = e4text( errCode1 ) ;
      if ( ptr != 0 )
      {
         pos = file4lenLow( errorLog ) ;
         file4writeInternal( errorLog, pos, ptr, c4strlen( ptr ) ) ;
         error4logAppendNewLine( errorLog ) ;
      }

      c4memset( num, 0, sizeof( num ) ) ;
      c4ltoa45( error4number2( errCode2 ), num, sizeof( num ) - 1 ) ;
      pos = file4lenLow( errorLog ) ;
      file4writeInternal( errorLog, pos, num, c4strlen( num ) ) ;
      error4logAppendNewLine( errorLog ) ;

      #ifndef E4OFF_STRING
         ptr = error4text( c4, errCode2 ) ;
         if ( ptr != 0 )
         {
            pos = file4lenLow( errorLog ) ;
            file4writeInternal( errorLog, pos, ptr, c4strlen( ptr ) ) ;
            error4logAppendNewLine( errorLog ) ;
         }
      #endif
   }

   #ifdef E4FILE_LINE
      if ( s4fileName != 0 )
      {
         pos = file4lenLow( errorLog ) ;
         file4writeInternal( errorLog, pos, code4fileName(), strlen( code4fileName() ) ) ;
         memset( num, 0, sizeof( num ) ) ;
         c4ltoa45( code4lineNo(), num, sizeof( num ) - 1 ) ;
         pos = file4lenLow( errorLog ) ;
         file4writeInternal( errorLog, pos, num, strlen( num ) ) ;
         error4logAppendNewLine( errorLog ) ;
      }
   #endif

   #ifdef S4SERVER
      // AS Mar 3/10 ... try to get info about the client
      char clientInfo[80] ;
      if ( c4 == 0 )
         strcpy( clientInfo, "CODE4 is null, no client info is available" ) ;
      else
      {
         if ( c4->currentClient == 0 )
            strcpy( clientInfo, "CurrentClient is not set, no client info is available" ) ;
         else
         {
            // the tcp address will be set if it ever connected...
            strcpy( clientInfo, "Client Info: ip address: " ) ;
            tcp4addressToChar( clientInfo+strlen(clientInfo), c4->currentClient->clientTcpAddress ) ;
         }
      }
      pos = file4lenLow( errorLog ) ;
      file4writeInternal( errorLog, pos, clientInfo, strlen( clientInfo ) ) ;
      error4logAppendNewLine( errorLog ) ;

      // AS Mar 15/10 ... and info about memory allocation
      sprintf( clientInfo, "Memory: total allocated: %ld, num allocations %ld", u4allocated(), u4numAllocated() ) ;
      pos = file4lenLow( errorLog ) ;
      file4writeInternal( errorLog, pos, clientInfo, strlen( clientInfo ) ) ;
      error4logAppendNewLine( errorLog ) ;
   #endif

   #ifdef EXCEPTION4REINDEX
      char exceptionString[] = "Exception error contained invalid paramater (probably memory corruption)" ;
   #endif

   if ( desc1 != 0 )
   {
      #ifdef EXCEPTION4REINDEX
      // paramaters may be bad here, only use if ok...
      try
      {
      #endif
         slen = c4strlen( desc1 ) ;
      #ifdef EXCEPTION4REINDEX
      }
      catch( ... )
      {
         // default to another error
         desc1 = exceptionString ;
         slen = strlen( desc1 ) ;
      }
      #endif

      pos = file4lenLow( errorLog ) ;
      file4writeInternal( errorLog, pos, desc1, slen ) ;
      error4logAppendNewLine( errorLog ) ;
   }

   if ( desc2 != 0 )
   {
      #ifdef EXCEPTION4REINDEX
         // paramaters may be bad here, only use if ok...
         try
         {
         #endif
          slen = c4strlen( desc2 ) ;
         #ifdef EXCEPTION4REINDEX
         }
         catch( ... )
         {
          // default to another error
          desc2 = exceptionString ;
          slen = strlen( desc2 ) ;
         }
      #endif
      pos = file4lenLow( errorLog ) ;
      file4writeInternal( errorLog, pos, desc2, slen ) ;
      error4logAppendNewLine( errorLog ) ;
   }

   if ( desc3 != 0 )
   {
      #ifdef EXCEPTION4REINDEX
         // paramaters may be bad here, only use if ok...
         try
         {
         #endif
          slen = c4strlen( desc3 ) ;
         #ifdef EXCEPTION4REINDEX
         }
         catch( ... )
         {
          // default to another error
          desc3 = exceptionString ;
          slen = strlen( desc3 ) ;
         }
      #endif
      pos = file4lenLow( errorLog ) ;
      file4writeInternal( errorLog, pos, desc3, slen ) ;
      error4logAppendNewLine( errorLog ) ;
   }
   error4logAppendNewLine( errorLog ) ;
   error4logAppendNewLine( errorLog ) ;

   c4->errorLog = errorLog ;
}

#if !defined(S4SERVER) && defined(S4CONSOLE) && defined(E4PAUSE)
   static void error4pause( void )
   {
      #ifndef S4WINTEL
         getchar() ;
      #else
         getch() ;
      #endif
   }
#endif

#ifdef S4WINDOWS
   #ifdef __TURBOC__
      #if __TURBOC__ == 0x297   /* if Borland C++ 2.0 */
    #ifdef __cplusplus
       extern "C"{ void FAR PASCAL FatalAppExit(WORD,LPSTR) ; }
    #else
       void FAR PASCAL FatalAppExit(WORD,LPSTR) ;
    #endif  /* __cplusplus */
      #endif  /* __TUROBC__ == 0x297 */
   #endif  /* __TUROBC__ */

   #ifndef __SC__
   #ifdef __ZTC__
      #ifdef __cplusplus
    extern "C"{ void FAR PASCAL FatalAppExit(unsigned short,LPSTR) ; }
      #else
    void FAR PASCAL FatalAppExit(unsigned short,LPSTR) ;
      #endif  /* __cplusplus */
   #endif  /* __ZTC__ */
   #endif  /* __SC__ */

   #ifdef _MSC_VER
      #if _MSC_VER == 600
    #ifdef __cplusplus
       extern "C"{ void FAR PASCAL FatalAppExit(WORD,LPSTR) ; }
    #else
       void FAR PASCAL FatalAppExit(WORD,LPSTR) ;
    #endif  /* __cplusplus */
      #endif  /* _MSC_VER == 600 */
   #endif  /* _MSC_VER */
#endif   /* S4WINDOWS */

extern S4CONST char *bad4data ;

// moved from inline to code in server case to avoid possible exception error
#if defined (S4SERVER) || !defined( S4INLINE )
   int S4FUNCTION error4code( CODE4 *c4 )
   {
      #ifdef S4SERVER
         if ( c4 == 0 )
            return -1 ;

         SERVER4CLIENT *curClient = c4->currentClient ;
         if ( curClient == 0 || c4accessMutexCountZero( c4 ) )
            return c4->errorCodeDefault ;
         else
            return curClient->errorCode ;
      #else
         #ifdef E4PARM_LOW
            if ( c4 == 0 )
               return error4( 0, e4parm_null, E96601 ) ;
         #endif
         return c4->errorCode ;
      #endif
   }
#endif

#ifdef S4SERVER
   long error4code2( CODE4 *c4 )
   {
      if ( c4 == 0 )
         return -1 ;

      SERVER4CLIENT *curClient = c4->currentClient ;
      if ( curClient == 0 || c4accessMutexCountZero( c4 ) )
         return c4->errorCode2Default ;
      else
         return curClient->errorCode2 ;
   }
#endif


int S4FUNCTION error4set( CODE4 *c4, const int newErrCode )
{
   #ifdef E4PARM_LOW
      if ( c4 == 0 )
         return error4( 0, e4parm_null, E96601 ) ;
   #endif

   int oldErrCode = error4code( c4 ) ;
   #ifdef S4SERVER
      if ( c4->currentClient != 0 )
         c4->currentClient->errorCode = newErrCode ;
   #else
      c4->errorCode = newErrCode ;
   #endif
   return oldErrCode ;
}

long S4FUNCTION error4set2( CODE4 *c4, const long newErrCode2 )
{
   long oldErrCode2 ;

   #ifdef E4PARM_LOW
      if ( c4 == 0 )
         return error4( 0, e4parm_null, E96601 ) ;
   #endif

   oldErrCode2 = error4code2( c4 ) ;
   #ifdef S4SERVER
      if (c4->currentClient != 0 )
         c4->currentClient->errorCode2 = newErrCode2 ;
   #else
      c4->errorCode2 = newErrCode2 ;
   #endif
   return oldErrCode2 ;
}


#ifdef S4OS2PM
S4EXPORT void S4FUNCTION error4out( CODE4 *c4, int errCode1, long errCode2, const char *desc1, const char *desc2, const char *desc3 )
{
   int pos = 0 , descNumber = 1 ;
   const char *ptr, *errPtr ;
   HAB   e4hab ;
   HMQ   e4hmq ;
   char errorStr[100] ;
   #ifndef E4ERROR_OFF
      #ifndef E4OFF_STRING
         const char *tPtr ;
      #endif
   #endif

   if ( c4 != 0 )
      error4set( c4, errCode1 ) ;

   #ifndef E4ERROR_OFF
      if ( c4 != 0 )
         if ( c4->errOff )
            return ;

      #ifdef S4ODBC_BUILD
         #ifdef E4ANALYZE
            // if we get to the point of outputting an error, we have a serious flaw.  catch this...
            iMsg = MessageBox( GetActiveWindow(), szBuf, "Assertion Error", MB_YESNOCANCEL | MB_DEFBUTTON2 | MB_ICONSTOP | MB_TASKMODAL ) ;
            if (IDYES == iMsg)
            {
               /* force an exception to invoke the debugger */
               int i = 0 ;
               i = 1 / i ;   // manually through debugger change i to 1 to continue...
            }
            else if ( IDCANCEL == iMsg )
            {
               DebugBreak() ;
            }
         #endif
      #endif

      if (errCode1 != 0)
      {
         c4strcpy( errorStr, sizeof( errorStr ), E4_ERROR ) ; // AS Dec 13/05 vs 5.0 fixes
         c4strcat( errorStr, sizeof( errorStr ), " #: " ) ;
         pos = strlen( errorStr ) ;
         c4ltoa45( (long)errCode1, (char *)errorStr + pos, 5 ) ;
         pos += 5 ;

         errorStr[pos++] = '\r' ;
         errorStr[pos++] = '\n' ;

         c4strcpy( errorStr + pos, sizeof( errorStr ) - pos, E4_ERROR ) ;
         c4strcat( errorStr + pos, sizeof( errorStr ) - pos, " #: " ) ;
         pos = strlen( errorStr ) ;
         c4ltoa45( error4number2( errCode2 ), (char *)errorStr + pos, 6 ) ;
         pos += 6 ;

         errorStr[pos++] = '\r' ;
         errorStr[pos++] = '\n' ;

         errPtr = e4text( errCode1 ) ;
         if ( errPtr != 0 )
         {
            c4strcpy( errorStr + pos, sizeof( errorStr ) - pos, errPtr ) ;
            pos += strlen( errPtr ) ;
            errorStr[pos++] = '\r\n' ;
         }
      }

      #ifndef E4OFF_STRING
         tPtr = error4text( c4, errCode2 ) ;
         if ( tPtr != 0 )
         {
            c4strcpy( errorStr + pos, sizeof( errorStr ) - pos, tPtr ) ;
            pos += strlen( tPtr ) ;
            errorStr[pos++] = '\r' ;
            errorStr[pos++] = '\n' ;
         }
      #endif

      ptr = desc1 ;
      while ( (ptr != (char *) 0) && (descNumber <= 3 ) )
      {
         // AS 03/23/00 this was incorrect, sometimes not stopping when should be
         if ( strlen(ptr)+pos+3 >= sizeof(errorStr) )
            break ;
         c4strcpy( errorStr + pos, sizeof( errorStr ) - pos, ptr ) ;
         pos += strlen(ptr) ;
         errorStr[pos++] = '\r' ;
         errorStr[pos++] = '\n' ;
         if ( descNumber++ == 1 )
            ptr = desc2 ;
         else
            ptr = desc3 ;
      }

      #ifdef E4FILE_LINE
         if ( code4fileName() != 0 )
            if ( strlen( code4fileName() )+pos+8 < sizeof(errorStr) )
            {
               c4strcpy( errorStr + pos, sizeof( errorStr ) - pos, code4fileName() ) ;
               pos += strlen( code4fileName() ) ;
               errorStr[pos++] = ' ' ;
               c4ltoa45( code4lineNo(), (char *)errorStr+pos, 6 ) ;
               pos += 5 ;
               errorStr[pos++] = '\r' ;
               errorStr[pos++] = '\n' ;
            }
      #endif

      errorStr[pos] = 0 ;

      /* In case the application has done no PM Initialization, set up an
    instance to allow for the error output to occur */

      e4hab = WinInitialize(0) ;
      if ( e4hab == NULLHANDLE )
         return ;

      e4hmq = WinCreateMsgQueue(e4hab, 0) ;

      if ( e4hmq == NULLHANDLE )
      {
         WinTerminate(e4hab) ;
         return ;
      }

      /* And print out the error via a desktop message box */
      WinMessageBox(HWND_DESKTOP, HWND_DESKTOP, errorStr, "Error", E4MSGBOXID, MB_OK | MB_MOVEABLE | MB_CUACRITICAL ) ;
      WinDestroyMsgQueue( e4hmq );
      WinTerminate(e4hab) ;
   #endif  /* E4ERROR_OFF */
}
#endif   /* S4OS2PM */

#if defined(S4WINDOWS)
   S4EXPORT void S4FUNCTION error4out( CODE4 *c4, int errCode1, long errCode2, const char *desc1, const char *desc2, const char *desc3 )
   {
      #ifndef E4ERROR_OFF
         char errorStr[257] ;
         LPTSTR pErrStr;
         const char *ptr, *errPtr ;
         #ifdef UNICODE  // CS 2002/07/01
            WCHAR errUStr[257] ;
         #endif
      #endif
      int pos=0,  descNumber = 1 ;
      #ifdef S4TESTING
         #ifdef S4SERVER
            WORD wType ;
         #endif
      #else
         #if !defined(S4WINCE) && !defined(S4PALM) && !defined(E4ERROR_OFF)
            WORD wType ;
         #endif
      #endif
      #ifndef E4ERROR_OFF
         #ifndef E4OFF_STRING
            const char *tPtr ;
         #endif
      #endif

      #ifndef E4ERROR_OFF
         #ifdef UNICODE
            pErrStr = errUStr;
         #else
            pErrStr = errorStr;
         #endif
      #endif

      if ( c4 != 0 )
         error4set( c4, errCode1 ) ;

      #ifndef E4ERROR_OFF
         if ( c4 != 0 )
            if ( c4->errOff )
               return ;

         if (errCode1 != 0)
         {
            c4memset( errorStr, ' ', sizeof( errorStr ) - 1 ) ;

            c4strcpy( errorStr, sizeof( errorStr ), E4_ERROR ) ;  // AS Dec 13/05 vs 5.0 fixes
            c4strcat( errorStr, sizeof( errorStr ), " #: " ) ;
            pos = c4strlen( errorStr ) ;
            c4ltoa45( (long)errCode1, (char *)errorStr + pos, 5 ) ;
            pos += 5 ;

            #ifndef S4PALM
               errorStr[pos++] = '\r' ;
            #endif
            errorStr[pos++] = '\n' ;

            c4strcpy( errorStr + pos, sizeof( errorStr ) - pos, E4_ERROR ) ;
            c4strcat( errorStr + pos, sizeof( errorStr ) - pos, " #: " ) ;
            pos = c4strlen( errorStr ) ;
            c4ltoa45( error4number2( errCode2 ), (char *)errorStr + pos, 6 ) ;
            pos += 6 ;

            errorStr[pos++] = '\r' ;
            errorStr[pos++] = '\n' ;

            errPtr = e4text( errCode1 ) ;
            if ( errPtr != 0 )
            {
               c4strcpy( errorStr + pos, sizeof( errorStr ) - pos, errPtr ) ;
               pos += c4strlen( errPtr ) ;
               #ifndef S4PALM
                  errorStr[pos++] = '\r' ;
               #endif
               errorStr[pos++] = '\n' ;
            }

            #ifndef E4OFF_STRING
               tPtr = error4text( c4, errCode2 ) ;
               if ( tPtr != 0 )
               {
                  c4strcpy( errorStr + pos, sizeof( errorStr ) - pos, tPtr ) ;
                  pos += c4strlen( tPtr ) ;
                  #ifndef S4PALM
                     errorStr[pos++] = '\r' ;
                  #endif
                  errorStr[pos++] = '\n' ;
               }
            #endif
         }

         ptr = desc1 ;
         while ( (ptr != (char *) 0) && (descNumber <= 3 ) )
         {
            // AS 03/23/00 this was incorrect, sometimes not stopping when should be
            if ( c4strlen( ptr ) + pos + 3 >= sizeof( errorStr ) )
               break ;
            c4strcpy( errorStr + pos, sizeof( errorStr ) - pos, ptr ) ;
            pos += c4strlen(ptr) ;
            errorStr[pos++] = '\r' ;
            errorStr[pos++] = '\n' ;
            if ( descNumber++ == 1 )
               ptr = desc2 ;
            else
               ptr = desc3 ;
         }

         #ifdef E4FILE_LINE
            if ( s4fileName != 0 )
               if ( strlen(s4fileName)+pos+8 < sizeof(errorStr) )
               {
                  c4strcpy( errorStr + pos, sizeof( errorStr ) - pos, s4fileName ) ;
                  pos += strlen( s4fileName ) ;
                  errorStr[pos++] = ' ' ;
                  c4ltoa45( s4lineNo, (char *)errorStr+pos, 6 ) ;
                  pos += 6 ;
                  errorStr[pos++] = '\r' ;
                  errorStr[pos++] = '\n' ;
               }
         #endif

         errorStr[pos] = 0 ;

         #ifdef S4TESTING
            u4writeErr( errorStr, 1 ) ;
         #endif

         #if !defined(S4WINCE) && !defined(S4PALM)
            OemToAnsi( errorStr, errorStr ) ;
         #endif

         #ifdef E4DEBUG_STOP
            DebugBreak() ;
         #endif

         #ifdef S4TESTING
            #ifdef S4SERVER
               wType = MB_OK | MB_ICONSTOP ;
               if ( errCode1 == e4memory )
                  wType |= MB_SYSTEMMODAL ;
               /* server should not error, so ok to leave in */
               // do not do a message box if no c4 or if a service
               if ( c4 != 0 )
                  if ( c4->runAsService == 0 )
                  {
                     if ( MessageBox( 0, pErrStr, E4_ERROR_COD, wType ) == 0 )
                        FatalAppExit( 0, E4_MEMORY_ERR ) ;
                  }
            #endif
         #else
            #if defined(S4WINCE)
               // do not do a message box if no c4 or if a service
               // AS Feb 14/06 - for windows ce, do a box otherwise we cannot get the output...
               // if ( c4 != 0 )
               {
                  #ifdef S4SERVER
                     if ( c4->runAsService == 0 )
                  #endif
                  {
                     c4atou(errorStr, errUStr, 257) ;
                     pErrStr = errUStr;
                     MessageBox( 0, pErrStr, E4_ERROR_COD, MB_OK | MB_ICONSTOP ) ;
                  }
               }
            #else
               #ifdef S4SERVER
                  // do not do a message box if a service
                  if ( c4 != 0 )  // check c4 only if server
                     if ( c4->runAsService == 0 )
               #endif
               {
                  wType = MB_OK | MB_ICONSTOP ;
                  if ( errCode1 == e4memory )
                     wType |= MB_SYSTEMMODAL ;
                  HWND hWnd = 0;
                  #ifndef S4CODE_SCREENS  /* LY 2004/02/25 */
                     if ( c4 )
                        hWnd = c4->hWnd ;
                  #endif
                  #ifdef UNICODE
                     c4atou(errorStr, errUStr, 257) ;
                     pErrStr = errUStr;
                  #endif

                  int mbResult;
                  mbResult = MessageBox( hWnd, pErrStr, E4_ERROR_COD, wType );
                  if ( mbResult == 0 )
                  {
                     if (GetLastError() == ERROR_INVALID_WINDOW_HANDLE)
                     {
                        // CS 2002/07/01 MessageBox only failed because the window handle was invalid.
                        // Try again with a NULL window hande.
                        mbResult = MessageBox( NULL, pErrStr, E4_ERROR_COD, wType );
                     }
                  }
                  if ( mbResult == 0 )
                     FatalAppExit( 0, TEXT(E4_MEMORY_ERR) ) ;
               }
            #endif
         #endif
      #endif
   }
#endif   /* S4WINDOWS  */
/* MessageBox(NULL, "ERROR HERE", E4_ERROR_COD, MP_SYSTEMMODAL); */

#ifndef S4CONSOLE
#ifdef S4MACINTOSH
/* S4MACINTOSH */
S4EXPORT void S4FUNCTION error4out( CODE4 *c4, int errCode1, long errCode2, const char *desc1, const char *desc2, const char *desc3 )
{
   #ifdef S4TESTING
      char errorStr[257] ;
      const char *errPtr ;
      int pos=0 ;
   #endif
   #ifndef E4ERROR_OFF
      #ifndef E4OFF_STRING
    char const *tPtr ;
      #endif
   #endif
   char const * ptr ;
   int descNumber = 1 ;
   Str255 macStr, macStra="\p", macStrb="\p", macStrc="\p";
   short itemHit ;
   AlertTHndl hALRT ;
   AlertTPtr  pALRT ;

   if ( c4 != 0 )
      error4set( c4, errCode1 ) ;

   #ifndef E4ERROR_OFF
      if ( c4 != 0 )
    if ( c4->errOff )
       return ;

      if (errCode1 != 0)
      {
         sprintf( (char *)&macStr, "  %d, %ld \r", errCode1, error4number2( errCode2 ) ) ;
         ptr = e4text( errCode1 ) ;
         if ( ptr != 0 )
            sprintf( (char *)&macStr + strlen((char *)macStr), "%s\r",e4text( errCode1 ) ) ;
         #ifndef E4OFF_STRING
            tPtr = error4text( c4, errCode2 ) ;
            if ( tPtr != 0 )
               sprintf( (char *)&macStr+strlen((char *)macStr), "%s",error4text( c4, errCode2 ) ) ;
         #endif
         S4CTOPSTR( (char *)&macStr );  /* convert C string to Pascal string */
      }

      if (desc1 != (char*)0 )
      {
         strcpy((char *)&macStra, desc1 );
         S4CTOPSTR((char *)&macStra) ;
      }

      #ifndef E4FILE_LINE
         if (desc2 != (char*)0 )
         {
            strcpy((char *)&macStrb, desc2 );
            S4CTOPSTR((char *)&macStrb) ;
         }

         if (desc3 != (char*)0 )
         {
            strcpy((char *)&macStrc, desc3 );
            S4CTOPSTR((char *)&macStrc) ;
         }
      #else /*since we can only support 4 lines total, we'll have to combine two*/
         if (desc2 != (char*)0 )
         {
            strcpy((char *)&macStrb, desc2 );
            if (desc3 != (char*)0 )
            {
               strcat((char *)&macStrb,"\r");
               strcat((char *)&macStrb,"desc3");
            }
            S4CTOPSTR((char *)&macStrb) ;
         }
         else if (desc3 != (char*)0 )
         {
            strcpy((char *)&macStrb,"desc3");
            S4CTOPSTR((char *)&macStrb) ;
         }

         if ( s4fileName != 0 )
         {
            sprintf( (char *)&macStrc, "File: %s Line:%d\r\r\r", s4fileName, s4lineNo ) ;
            S4CTOPSTR( (char *)&macStrc );  /* convert C string to Pascal string */
         }
      #endif
      ParamText(macStr, macStra, macStrb, macStrc ) ;
      itemHit = StopAlert( E4MAC_ALERT, 0 ) ;
      ParamText(0,0,0,0);
   #endif  /* E4ERROR_OFF */
}
#endif
#endif

#ifdef S4PALM
   S4EXPORT void S4FUNCTION error4out( CODE4 *c4, int errCode1, long errCode2, const char *desc1, const char *desc2, const char *desc3 )
   {
   }
#endif

#ifdef S4CONSOLE
   S4EXPORT void S4FUNCTION error4out( CODE4 *c4, int errCode1, long errCode2, const char *desc1, const char *desc2, const char *desc3 )
   {
      #ifdef S4TESTING
         char errorStr[257] ;
         const char *errPtr ;
         int pos=0 ;
      #endif
      #ifndef E4ERROR_OFF
         #ifndef E4OFF_STRING
            char const *tPtr ;
         #endif
      #endif
      char const * ptr ;
      int descNumber = 1 ;

      if ( c4 != 0 )
         error4set( c4, errCode1 ) ;

      #ifndef E4ERROR_OFF
         if ( c4 != 0 )
            if ( c4->errOff )
               return ;

         #ifdef S4TESTING
            if (errCode1 != 0)
            {
               memset( errorStr, ' ', sizeof( errorStr ) - 1 ) ;

               strcpy( errorStr, E4_ERROR ) ;
               strcat( errorStr, " #: " ) ;
               pos = strlen( errorStr ) ;
               c4ltoa45( (long)errCode1, (char *)errorStr + pos, 5 ) ;
               pos += 5 ;

               errorStr[pos++] = '\r' ;
               errorStr[pos++] = '\n' ;

               strcpy( errorStr + pos, E4_ERROR ) ;
               strcat( errorStr + pos, " #: " ) ;
               pos = strlen( errorStr ) ;
               c4ltoa45( error4number2( errCode2 ), (char *)errorStr + pos, 6 ) ;
               pos += 6 ;

               errorStr[pos++] = '\r' ;
               errorStr[pos++] = '\n' ;

               errPtr = e4text( errCode1 ) ;
               if ( errPtr != 0 )
               {
                  strcpy( errorStr + pos, errPtr ) ;
                  pos += strlen( errPtr ) ;
                  errorStr[pos++] = '\r' ;
                  errorStr[pos++] = '\n' ;
               }

               #ifndef E4OFF_STRING
                  tPtr = error4text( c4, errCode2 ) ;
                  if ( tPtr != 0 )
                  {
                     strcpy( errorStr + pos, tPtr ) ;
                     pos += strlen( tPtr ) ;
                     errorStr[pos++] = '\r' ;
                     errorStr[pos++] = '\n' ;
                  }
               #endif
            }

            ptr = desc1 ;
            while ( (ptr != (char *) 0) && (descNumber <= 3 ) )
            {
               // AS 03/23/00 this was incorrect, sometimes not stopping when should be
               if ( strlen( ptr ) + pos + 3 >= sizeof( errorStr ) )
                  break ;
               strcpy( errorStr+pos, ptr ) ;
               pos += strlen(ptr) ;
               errorStr[pos++] = '\r' ;
               errorStr[pos++] = '\n' ;
               if ( descNumber++ == 1 )
                  ptr = desc2 ;
               else
                  ptr = desc3 ;
            }

            #ifdef E4FILE_LINE
               if ( s4fileName != 0 )
                  if ( strlen(s4fileName)+pos+8 < sizeof(errorStr) )
                  {
                     strcpy( errorStr + pos, s4fileName ) ;
                     pos += strlen( s4fileName ) ;
                     errorStr[pos++] = ' ' ;
                     c4ltoa45( s4lineNo, (char *)errorStr+pos, 6 ) ;
                     pos += 6 ;
                     errorStr[pos++] = '\r' ;
                     errorStr[pos++] = '\n' ;
                  }
            #endif

            errorStr[pos] = 0 ;

            u4writeErr( errorStr, 1 ) ;
         #else
            if (errCode1 != 0)
            {
               fprintf( stderr, E4_ERROR_NUM ) ;
               fprintf( stderr, "  %d, %ld \r\n", errCode1, error4number2( errCode2 ) ) ;
               ptr = e4text( errCode1 ) ;
               if ( ptr != 0 )
                  fprintf( stderr, "%s\r\n",e4text( errCode1 ) ) ;
               #ifndef E4OFF_STRING
                  tPtr = error4text( c4, errCode2 ) ;
                  if ( tPtr != 0 )
                     fprintf( stderr, "%s\r\n",error4text( c4, errCode2 ) ) ;
               #endif
            }

            ptr = desc1 ;
            while ( (ptr != (char *) 0) && (descNumber <= 3 ) )
            {
               fprintf( stderr, "\r\r\n" ) ;
               fprintf( stderr, ptr ) ;
               if ( descNumber++ == 1 )
                  ptr = desc2 ;
               else
                  ptr = desc3 ;
            }

            fprintf( stderr, "\r\r\n" ) ;
            #ifdef E4FILE_LINE
               if ( s4fileName != 0 )
                  fprintf( stderr, "File: %s Line:%d\r\r\n", s4fileName, s4lineNo ) ;
            #endif
         #endif  /* S4TESTING */
      #endif  /* E4ERROR_OFF */
   }
#endif

#ifdef E4FILE_LINE
   /* use globals to hold the current file name and line number */
   const char *s4fileName = 0 ;
   int s4lineNo = 0 ;

   int   S4FUNCTION code4lineNo( void )
   {
      return s4lineNo ;
   }

   const char *S4FUNCTION code4fileName( void )
   {
      return s4fileName ;
   }

   void S4FUNCTION code4lineNoSet( int val )
   {
      s4lineNo = val ;
   }

   void S4FUNCTION code4fileNameSet( const char *ptr )
   {
      s4fileName = ptr ;
   }
#endif

#ifndef E4FILE_LINE
/*int S4FUNCTION e4( CODE4 *c4, int errCode, const char *desc )*/
/*{*/
/*   return error4describe( c4, errCode, 0L, desc, 0, 0 ) ;*/
/*}*/
#endif

static void error4storeDescription(CODE4 *c4, const char *s1, const char *s2, const char *s3)
{
   // CS 2002/04/24
   // Store the description strings, separated by a new line, to
   // c4->lastErrorDescription. Any or all description pointers can be null.
   // If all pointers are null, c4->lastErrorDescription is set to null.

   // free the existing string, if it has been previously allocated
   if (c4->lastErrorDescription)
      u4free(c4->lastErrorDescription) ;
   c4->lastErrorDescription = 0 ;

   // if additional description was provided
   if ( s1 || s2 || s3 )
   {
      int descriptionLength = 0 ;
      if ( s1 )
         descriptionLength += strlen( s1 ) ;
      if ( s2 )
         descriptionLength += strlen( s2 ) ;
      if ( s3 )
         descriptionLength += strlen( s3 ) ;

      descriptionLength += 6 ;
      c4->lastErrorDescription = (char *)u4allocFree( c4, descriptionLength ) ;

      if (s1)
         c4strcpy( c4->lastErrorDescription, descriptionLength , s1 ) ;  // AS Dec 13/05 vs 5.0 fixes

      if (s2)
      {
         if ( strlen( c4->lastErrorDescription ) > 0 )
            c4strcat( c4->lastErrorDescription, descriptionLength, "\r\n" ) ;
         c4strcat( c4->lastErrorDescription, descriptionLength, s2 ) ;
      }

      if (s3)
      {
         if ( strlen( c4->lastErrorDescription) > 0 )
            c4strcat( c4->lastErrorDescription, descriptionLength, "\r\n" ) ;
         c4strcat( c4->lastErrorDescription, descriptionLength, s3 ) ;
      }
   }
}


#ifdef E4STACK
   int S4FUNCTION error4stackDefault( CODE4 *c4, const int errCode1, const long errCode2 )
   {
      return error4describeExecute( c4, errCode1, errCode2, 0, 0, 0 ) ;
   }
#endif

int S4FUNCTION error4default( CODE4 *c4, const int errCode1, const long errCode2 )
{
   return error4describeExecute( c4, errCode1, errCode2, 0, 0, 0 ) ;
}

int S4FUNCTION error4describeDefault( CODE4 *c4, const int errCode1, const long errCode2, const char *s1, const char *s2, const char *s3 )
{
   return error4describeExecute( c4, errCode1, errCode2, s1, s2, s3 ) ;
}

int S4FUNCTION error4describeExecute( CODE4 *c4, const int errCode1, const long errCode2, const char *s1, const char *s2, const char *s3 )
{
   #ifndef S4SERVER
      if ( c4 != 0 )
         error4storeDescription(c4, s1, s2, s3);
   #endif

   #ifdef E4HOOK
      #ifndef S4SERVER
         if ( c4 != 0 )
         {
            error4set( c4, errCode1 ) ;
            error4set2( c4, errCode2 ) ;
         }
      #endif

      error4hook( c4, errCode1, errCode2, s1, s2, s3 ) ;
      return errCode1 ;
   #else
      int doCallback = 0;

      /* display error local to operating system */
      if ( c4 != 0 )
      {
         if ( c4->errorLog != 0 )
         {
            if ( c4->errorLog->hand != INVALID4HANDLE )  // CS 2000/03/26
               error4logAppend( c4, errCode1, errCode2, s1, s2, s3 ) ;
         }

         if (c4->errorCallback)  // CS 2002/04/24
            doCallback = 1;
      }

      // CS 2002/04/23 move up - set error codes before callback
      if ( c4 )
      {
         error4set( c4, errCode1 ) ;
         error4set2( c4, errCode2 ) ;
      }

      if (doCallback)
      {
         // CS 2009/10/20 Don't translate number 2. If the application wants a display number, it can call error4number2().
         c4->errorCallback(c4, errCode1, errCode2, s1, s2, s3);
      } // if doCallBack
      else
      {
         error4out( c4, errCode1, errCode2, s1, s2, s3 ) ;

         #if !defined( E4ERROR_OFF ) && defined( S4CONSOLE ) && defined( E4PAUSE )
            if (c4)
            {
               if (c4->errOff == 0)
               {
                  error4pause() ;
               }
            }
            else
               error4pause() ;
         #endif
      } // else doCallBack

      #ifdef E4STOP
         code4exit( c4 ) ;
      #endif

      #ifdef E4STOP_CRITICAL
         if ( c4 == 0 )
            code4exit( c4 ) ;
         switch ( errCode1 )
         {
            case e4parm:
            case e4parm_null:
               code4exit( c4 ) ;
         }
      #endif

      #ifdef E4STOP_UNRECOVERABLE
         // AS 05/26/99 --> don't want to stop only if c4 is 0...
         // if ( c4 == 0 )
         //    code4exit( c4 ) ;
         switch ( errCode1 )
         {
            case e4info:
            case e4memory:
            case e4result:
            case e4struct:
               code4exit( c4 ) ;
         }

         if ( errCode1 == e4demo )
            code4exit( c4 ) ;
      #endif

      return errCode1 ;
   #endif
}

#if !defined( S4SERVER ) && !defined( S4PALM )
void S4FUNCTION error4exitTest( CODE4 *c4 )
{
   if ( c4 == 0 )
      return ;
   if ( error4code( c4 ) < 0 )
      code4exit( c4 ) ;
}
#endif

#ifdef S4CB51
#ifndef S4SERVER
void S4FUNCTION e4severe( const int errCode, const char *desc )
{
   #ifdef E4HOOK
      error4hook( 0, errCode, 0, desc, 0, 0 ) ;
   #else
      error4out( 0, errCode, 0, desc, 0, 0 ) ;
      #ifdef S4CONSOLE
      #ifdef E4PAUSE
    error4pause() ;
      #endif
      #endif
   #endif
   code4exit( 0 ) ;
}
#endif

int S4FUNCTION e4describe( CODE4 *c4, int errCode, const char *s1, const char *s2, const char *s3 )
{
   return error4describe( c4, errCode, 0L, s1, s2, s3 ) ;
}
#endif
