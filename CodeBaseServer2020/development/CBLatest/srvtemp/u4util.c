/* u4util.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#if !defined( S4UNIX ) && defined( __TURBOC__ )
   #pragma hdrstop
#endif

#ifdef S4TESTING
   #if defined( S4WINDOWS ) && !defined( S4WIN32 )
      #include <dos.h>
   #endif
   extern FILE4 s4test ;
#endif

#if defined( S4WINTEL ) && !defined( S4WIN32 )
   #include <sys\timeb.h>
#endif

#ifdef S4UNIX
   #include <sys/times.h>
   #if !defined( S4LINUX ) && defined( S4NO_USLEEP )
      #ifdef S4NO_SELECT
         #include <stropts.h>
         #include <poll.h>
      #else
         #include <sys/select.h>
      #endif
   #endif
#endif

#ifdef S4MACINTOSH
   #include <Timer.h>
#endif

#ifdef S4WINDOWS
   #ifdef __cplusplus
      extern "C" {
   #endif

   long S4FUNCTION v4Cstring(char *) ;
   void S4FUNCTION v4Cstringfree(char *) ;

   #ifdef __cplusplus
      }
   #endif
#endif


// AS NOV 24/05 this is now used in the regular compile...OLEDB5BUILD not required
// moved from u4util.c
#if defined( E4ANALYZE )
   void assert5info( char *fName, int lineNo )
   {
      #ifdef S4OLEDEBUG_PRINT
         log5( "assert5() failure, file:  " ) ;
         log5( fName ) ;
         char lineBuf[11] ;
         memset( lineBuf, 0, sizeof( lineBuf ) ) ;
         log5( "  line #" ) ;
         c4ltoa45( lineNo, lineBuf, 10 ) ;
         log5( lineBuf ) ;
         log5( "\r\n" ) ;
      #endif
      #ifdef OLEDB5BUILD
         throw Err5internal( 0 ) ;
      #else
         error4( 0, e4info, E94501 ) ;
      #endif
   }
#endif

int S4FUNCTION u4allocAgainDefault( CODE4 *c4, char **ptrPtr, unsigned *wasOldLen, const unsigned newLen )
{
   /* '*wasOldLen' contains the old len and will contain the new.
      'newLen' contains the new length.
      memory is only allocated if the  'newLen' paramater is greater than the
      wasOldLen paramater.
   */
   char *newPtr ;

   #ifdef E4PARM_HIGH
      if ( *ptrPtr == 0 && *wasOldLen != 0 )
         return error4( c4, e4parm, E94501 ) ;
   #endif

   if ( newLen <= *wasOldLen )
      return 0 ;

   newPtr = (char *)u4allocFreeDefault( c4, (long)newLen ) ;

   if ( newPtr == 0 )
      return error4stack( c4, e4memory, E94501 ) ;

   if ( *ptrPtr != 0 )
   {
      c4memcpy( newPtr, *ptrPtr, (size_t) *wasOldLen ) ;
      u4free( *ptrPtr ) ;
   }
   *ptrPtr = newPtr ;
   *wasOldLen = newLen ;
   return 0 ;
}



/* LY July 9/03 : added S4LUPACH */
#if defined( S4SEMAPHORE ) && defined( E4MISC ) && !defined( S4LUPACH )
   extern CRITICAL_SECTION critical4memory ;
   extern int memoryInitialized;
#endif



#ifdef P4ARGS_USED
   #pragma argsused
#endif
void *S4FUNCTION u4allocFreeDefault( CODE4 *c4, long n )
{
   void *ptr ;
   #if !defined( S4OFF_COMMUNICATIONS ) && defined( S4SERVER )
      SERVER4CLIENT *client ;
   #endif

   #if defined( S4SEMAPHORE ) && defined( E4MISC )
      /* the debug u4alloc uses global tracking pointers, so semaphore */
      mem4start( c4 ) ;
   #endif

   ptr = (void *)u4allocDefault( n ) ;

   #ifndef S4OFF_COMMUNICATIONS
      /*    go through all of the SERVER4CLIENTs, and if they are not in an*/
      /*         active state (i.e. no worker thread working on them), then*/
      /*         free up their SERVER4CLIENT.CONNECT.buffer and set len to 0*/
      /*         then retry allocation (left in)*/
      if ( ptr == NULL )
      {
         #ifdef S4SERVER
            server4clientListReserve( c4->server ) ;
            client = server4clientGetFirst( c4->server ) ;
            while( client )
            {
               if ( client->connect.workState == CONNECT4IDLE )
               {
                  connection4clear(&client->connection) ;
                  // AS July 25/02 - If the buffer is null, don't free it - was gpf'ing here
                  if ( client->connection.bufferLen != 0 && client->connection.buffer != 0 )
                  {
                     client->connection.bufferLen = 0 ;
                     client->connection.buffer = ((char *)(client->connection.buffer)) - 4 ;  /* CS 2000/02/14 */
                     u4free(client->connection.buffer) ;
                  }
               }
               client = server4clientGetNext( c4->server, client ) ;
            }
            server4clientListRelease( c4->server ) ;
            ptr = (void *)u4allocDefault( n ) ;
         #endif
      }
   #endif

   #ifndef S4OFF_OPTIMIZE
      if ( ptr == 0 && c4 )
         if ( c4->hasOpt )
         {
            code4optSuspend( c4 ) ;
            ptr = (void *)u4allocDefault( n ) ;
            code4optRestart( c4 ) ;
         }
   #endif

   #ifdef S4SEMAPHORE
      #ifdef E4MISC
         mem4stop( c4 ) ;
      #endif
   #endif

   return ptr ;
}

#ifndef S4PALM
   void S4FUNCTION u4delayHundredth( const unsigned int numHundredths )
   {
      #if !defined( S4WIN32 ) && !defined( S4UNIX )
         int sec ;
         unsigned int hundredths ;
      #endif
      #if defined( S4WINTEL ) && !defined( S4WIN32 )
         struct timeb oldTime, newTime ;
      #endif
      #ifdef S4WIN16
         #ifndef S4CLIENT
            MSG msg;
         #endif
      #endif
      #ifdef S4UNIX
         #ifdef S4NO_USLEEP
            #ifndef S4NO_SELECT
               struct timeval waitTime ;
            #endif
         #else
            int sec ;
            u_int hundredths ;
         #endif
      #endif
      #ifdef S4MACINTOSH
          UnsignedWide oldTime, newTime ;
      #endif

      #if defined( S4UNIX ) && defined( S4NO_USLEEP ) && !defined( S4NO_SELECT )
         waitTime.tv_sec = numHundredths / 100 ;
         waitTime.tv_usec = (numHundredths % 100 ) * 10000 ;
      #endif

      #if defined( S4WINTEL ) && !defined( S4WIN32 )
         ftime( &oldTime) ;
         sec = numHundredths / 100 ;
         hundredths = numHundredths % 100 ;
      #endif

      #ifdef S4MACINTOSH
         Microseconds( &oldTime ) ;
         sec = numHundredths / 429497 ;
         hundredths = numHundredths % 429497 ;
      #endif

      #if   defined( S4WIN16 )
         for ( ;; )
         {
            /* Give some other application a chance to run. */
            #ifndef S4CLIENT
               if ( PeekMessage( &msg, (HWND)NULL, (UINT)0, (UINT)0, PM_REMOVE ) )
               {
                  /* can't allow re-entrancy, so if not a paint message, just
                     discard.  Allow paints messages to give up control to other
                     apps, but client's calling CodeBase calls on WM_PAINT
                     messages may fail */
                  if ( msg.message == WM_PAINT )
                  {
                     TranslateMessage((LPMSG)&msg) ;
                     DispatchMessage((LPMSG)&msg) ;
                  }
               }
            #endif /* S4CLIENT */

            /* allow device drivers to run... */
            _asm mov ax, 0x1689
            _asm mov bl, 1
            _asm int 0x2f

            ftime( &newTime ) ;

            if ( newTime.time - oldTime.time > ( sec + 1 ) )  /* > 1 secord over */
               break ;

            if ( newTime.time - oldTime.time >= sec )
               if ( ( (long)(newTime.millitm - oldTime.millitm ) / 10 ) > (long)hundredths )
                  break ;
        }
      #elif defined( S4UNIX )
         #ifdef S4NO_USLEEP
            #ifdef S4NO_SELECT
               poll(0, NULL, numHundredths * 10 ) ;
            #else
               select(0, NULL, NULL, NULL, &waitTime ) ;
            #endif
         #else
            if (numHundredths >= 100)
            {
               sec = numHundredths / 100 ;
               hundredths = (numHundredths % 100)*10000 ;
               sleep(sec) ;
            }
            else
               hundredths = numHundredths * 10000 ;
            usleep(hundredths ) ;
         #endif
      #elif defined( S4WIN32 )
         Sleep( 10 * numHundredths ) ;
      #else
         for ( ;; )
         {
            #if   defined( S4WINTEL )
               ftime( &newTime ) ;

               if ( newTime.time - oldTime.time >= sec )
                  if ( (unsigned int)(( (newTime.millitm - oldTime.millitm ) / 10 )) > hundredths )
                     break ;
            #elif defined( S4MACINTOSH )
                Microseconds(&newTime ) ;
                if (newTime.hi - oldTime.hi > (sec + 1) )
                   break ;
                if (newTime.hi - oldTime.hi >= sec )
                   if( (newTime.lo - oldTime.lo ) > hundredths )
                       break;
            #else
               #error No delay defined for this configuration.
            #endif
         }
      #endif
      return ;
   }
#endif

#ifndef S4OFF_MULTI
#ifndef S4INLINE
/* delays one second, allows other windows applications to run, etc. */
   void u4delaySec()
   {
      u4delayHundredth( 100 ) ;
   }
#endif
#endif  // !S4OFF_MULTI

long S4FUNCTION u4getMaxIndexSize()
{
   return ( I4MAX_KEY_SIZE ) ;
}

#if !defined( S4WINCE ) && !defined( S4PALM )
   // AS Dec 13/05 - under Windows getenv is becoming deprecated...
   char *u4environ( CODE4 *c4, char *find, const int doErr )
   {
      #ifndef S4WINDOWS_VS5_PLUS
         char *env ;
      #endif

      #ifdef E4PARM_HIGH
         if ( find == 0 )
         {
            error4( 0, e4parm_null, E94503 ) ;
            return 0 ;
         }
      #endif

      // AS Dec 13/05 - under Windows getenv is becoming deprecated...
      #ifdef S4WINDOWS_VS5_PLUS
         size_t len ;
         errno_t rc = getenv_s( &len, c4->env, sizeof( c4->env ), find ) ;
         if ( rc != 0 )
         {
            if ( doErr )
               error4describe( 0, e4info, E84501, find, 0, 0 ) ;
            return 0 ;
         }
         return c4->env ;
      #else
         env = getenv( find ) ;

         if ( env == 0 && doErr )
         {
            error4describe( 0, e4info, E84501, find, 0, 0 ) ;
            return 0 ;
         }

         return env ;
      #endif
   }
#endif



unsigned long S4FUNCTION u4ncpy( char *to, const char *from, const unsigned int l )
{
   unsigned i, len ;

   #ifdef E4PARM_HIGH
      if ( l == 0 )
      {
         error4( 0, e4parm, E94502 ) ;
         return 0 ;
      }
   #endif

   len = l - 1 ;
   for ( i = 0;; i++ )
   {
      if ( i >= len )
      {
         to[len] = 0 ;
         return len ;
      }
      to[i] = from[i] ;
      if ( from[i] == 0 )
         return i ;
   }
}



#ifndef S4INLINE
   int S4FUNCTION u4ptrEqual( const void *p1, const void *p2 )
   {
      return( p1 == p2 ) ;
   }
#endif



#ifdef S4TESTING
   #ifdef S4WINTEL
      S4EXPORT void S4FUNCTION u4terminate( void )
      {
         #ifdef S4WIN32
            #ifdef S4WINCE /* LY 2002/08/28 */
               ExitThread( 1 ) ;
            #else
               ExitProcess(1) ;
            #endif
         #else
            union REGS terminate ;

            terminate.h.ah = 0x4C ;

            intdos( &terminate, &terminate ) ;
         #endif
      }
   #endif  /* S4WINDOWS */

   #ifndef S4SERVER
      void u4setField( const char *serverId, const char *database, const long recno, const char *fieldName, const char *newValue )
      {
         #ifndef S4OFF_WRITE
         CODE4 cb ;
         DATA4 *data ;
         FIELD4 *field ;
         int openAttempts ;

         if ( database == 0 || recno == 0 || fieldName == 0 || newValue == 0 )
            return ;

         // AS May 3/07 - this shouldn't fail, but sometimes it was, causing problems...
         int rc ;
         #ifdef S4CLIENT   //CJ- 25/11/99- setting the skipCom variable in client server causes the ats to fail in client/server
            rc = code4initLow( &cb, DEF4PROTOCOL, S4VERSION, code4structSize(&cb), 0 ) ;
         #else
            rc = code4initLow( &cb, DEF4PROTOCOL, S4VERSION, code4structSize(&cb), 1 ) ;
         #endif
         if ( rc != 0 )
            error4( 0, e4info, E94504 ) ;
         cb.errOff = 1 ;
         // AS Jul 2/03 disable file optimization since it can slow things down for such a short operation
         code4optSuspend( &cb ) ;

         #ifdef S4CLIENT
            if( code4connect( &cb, serverId, 0, "S4TESTING", "S4TESTING", 0 ) != r4success )
            {
               code4close(&cb) ;
               code4initUndo(&cb) ;
               return ;
            }
         #else
            code4logOpenOff( &cb ) ;
         #endif

         #ifndef S4OFF_MULTI
            cb.accessMode = OPEN4DENY_RW ;       /* open exclusively */
         #endif
         cb.errOpen = 0 ;
         cb.autoOpen = 0 ;

         data = d4open( &cb, database ) ;
         for ( openAttempts = 0 ; ( data == NULL ) && ( openAttempts < 300 ) ; openAttempts++ )
         {
            // AS clear out the error if applicable...
            error4set( &cb, 0 ) ;
            u4delayHundredth( 100 ) ;   /* wait one second and try again */
            data = d4open( &cb, database ) ;
         }
         if ( data == NULL )
         {
            code4close(&cb) ;
            code4initUndo(&cb) ;
            return ;   /* should return an error or something  */
         }

         #ifndef S4OFF_MULTI
            cb.lockAttempts = 300 ;
            cb.lockDelay = 100 ;     /* shouldn't have to wait--just in case */

            if ( d4lockFile( data ) != 0 )
            {
               d4close( data ) ;
               code4close(&cb) ;
               code4initUndo(&cb) ;
               return ;
            }
         #endif

         if ( (field = d4field( data, fieldName )) == 0 )
         {
            d4close( data ) ;
            code4close(&cb) ;
            code4initUndo(&cb) ;
            return ;
         }

         if ( d4go( data, recno ) != 0 )
         {
            d4close( data ) ;
            code4close(&cb) ;
            code4initUndo(&cb) ;
            return ;
         }

         f4assign( field, newValue ) ;

         if ( d4flushData( data ) != 0 )
         {
            d4close( data ) ;
            code4close(&cb) ;
            code4initUndo(&cb) ;
            return ;
         }

         d4close( data ) ;
         code4close(&cb) ;
         code4initUndo(&cb) ;
         #endif /* S4OFF_WRITE */
      }

      int ats4readInfo( char *filename, void *info, const unsigned int len )
      {
         CODE4 cb ;
         FILE4 f ;
         FILE4LONG fpos ;

         // AS May 1/07 - the code4init may fail...it shouldn't but it could...
         if ( code4initLow( &cb, DEF4PROTOCOL, S4VERSION, code4structSize(&cb), 1 ) != 0 )
            return -1 ;
         #ifndef S4CLIENT
            code4logOpenOff( &cb ) ;
            code4optSuspend( &cb ) ;  // don't use optimization ...
         #endif

         cb.errOff = 1 ;
         cb.errOpen = 0 ;   /* avoid endless loop due to error calling this function... */
         // AS May 24/02 - created file4openLow for internal use to indicate file types
         if ( file4openInternal( &f, &cb, filename, 1, OPT4NONE ) )
         {
            code4initUndo( &cb ) ;
            return 0 ;   /* info could not be read */
         }

         /* LY July 7/03 : changed from 0 to 0L for Linux compiler */
         file4longAssign( fpos, 0, 0L ) ;

         file4readInternal( &f, fpos, info, len ) ;
         file4close( &f ) ;

         code4close( &cb ) ;
         code4initUndo( &cb ) ;

         return 1 ;   /* info successfully read */
      }

      S4EXPORT void S4FUNCTION ats4setSuiteStatus( const char *newValue )
      {
         ATS4RECINFO info ;
         const char *fieldStatus = "STATUS" ;
         const char *fieldRDate = "RDATE" ;
         #ifdef S4CLIENT
            char buf[100] ;
         #endif
         char *serverId, rdate[8] ;

         if ( newValue == 0 )
            return ;

         serverId = 0 ;

         #ifdef S4CLIENT
         if ( getenv( "CSNAME" ) )
            serverId = getenv( "CSNAME" ) ;
         else
         {
            if ( ats4readInfo( ATS_FILENAME_CS, buf, sizeof( buf ) ) )
               serverId = buf ;
         }
         #endif

         date4today( rdate ) ;

         #ifndef S4WINCE   /* LY 2002/08/28 */
            if( getenv( "T4FLAG" ) )
            {
               if ( strcmp( getenv( "T4FLAG" ), "T" ) == 0 )
               {
                  u4setField( serverId, getenv( "T4SUITE" ), atol( getenv( "T4RECNO" ) ), fieldStatus, newValue ) ;
                  u4setField( serverId, getenv( "T4SUITE" ), atol( getenv( "T4RECNO" ) ), fieldRDate, rdate ) ;
               }
            }
            else
         #endif
         {
            if ( ats4readInfo( ATS_FILENAME_REC, &info, sizeof( info ) ) )
            {
               u4setField( serverId, (char *)info.T4SUITE, atol((char *)info.T4RECNO), fieldStatus, newValue ) ;
               u4setField( serverId, (char *)info.T4SUITE, atol((char *)info.T4RECNO), fieldRDate, rdate ) ;
            }
         }
      }
   #endif /* S4SERVER */
#endif /* S4TESTING */



// AS May 4/07 - support for in4temp in testing case as well
#if defined( S4MEM_PRINT ) || defined( S4TESTING )
   int in4temp = 0 ;
#endif
#ifdef S4MEM_PRINT
   char *write4buf = 0 ;
   FILE4SEQ_WRITE file4seq ;
   FILE4SEQ_WRITE *file4seqPtr ;
#endif



#ifdef S4TESTING
   #ifdef S4SERVER
      static int isInit = 0 ;
      #ifndef S4OFF_THREAD
         static CRITICAL_SECTION crit4 ;
      #endif
   #endif

   void u4writeOut( char *out, int newLine, long lenIn )
   {
      int errCode, flushCode, loop ;
      unsigned char val, intVal ;
      FILE4LONG len ;
      FILE4LONG pos ;
      char buf[10] ;
      #ifdef S4TESTING
         #ifndef S4SERVER
            int undoLocal = 0 ;
         #endif
      #endif

      #ifdef S4SERVER
         if ( isInit == 0 )
         {
            #ifndef S4OFF_THREAD
               InitializeCriticalSection( &crit4 ) ;
            #endif
            isInit = 1 ;
         }
      #endif

      // AS May 4/07 - support for in4temp in testing case as well
      #if defined( S4MEM_PRINT ) || defined( S4TESTING )
         if ( in4temp == 1 )
            return ;
      #endif

      if( s4test.hand == INVALID4HANDLE )  /* even for non-testing just ensure else return */
         return ;

      #ifdef S4SERVER
         #ifndef S4OFF_THREAD
            EnterCriticalSection( &crit4 ) ;
         #endif
      #endif

      if ( s4test.codeBase != 0 )
      {
         errCode = error4code( s4test.codeBase ) ;
         error4set( s4test.codeBase, 0 ) ;
      }

      len = file4lenLow( &s4test ) ;

      if ( file4longError( len ) != 0 )
      {
         #ifdef S4MEM_PRINT
            if ( write4buf == 0 )
            {
               in4temp = 1 ;
               write4buf = (char *)u4alloc( 40000L ) ;
               if ( write4buf != 0 )
               {
                  file4seqWriteInitLow( &file4seq, &s4test, len, write4buf, (unsigned int)40000 ) ;
                  file4seqPtr = &file4seq ;
               }
               in4temp = 0 ;
            }
         #endif

         if ( newLine )
         {
            #ifdef S4MEM_PRINT
               if ( write4buf != 0 )
               {
                  file4seqWrite( &file4seq, "\r\n", 2 ) ;
                  file4longAdd( &len, 2 ) ;
               }
               else
               {
            #endif
               file4longAssignLong( pos, len ) ;
               file4writeInternal( &s4test, pos, "\r\n", 2 ) ;
               file4longAdd( &len, 2 ) ;
            #ifdef S4MEM_PRINT
               }
            #endif
         }
         #ifdef S4MEM_PRINT
            if ( write4buf != 0 )
            {
               for ( loop = 0 ; loop < lenIn ; loop++ )
               {
                  val = out[loop] ;
                  if ( val >= '0' && val <= '9' )
                  {
                     file4seqWrite( &file4seq, &val, 1 ) ;
                     file4longAdd( &len, 1 ) ;
                  }
                  else
                  {
                     buf[0] = '0' ;
                     buf[1] = 'x' ;
                     intVal = val / 100 ;
                     buf[2] = intVal + '0' ;
                     val -= intVal * 100 ;
                     intVal = val / 10 ;
                     buf[3] = intVal + '0' ;
                     val -= intVal * 10 ;
                     buf[4] = val + '0' ;
                     buf[5] = ' ' ;
                     file4seqWrite( &file4seq, buf, 6 ) ;
                     file4longAdd( &len, 6 ) ;
                  }
               }
            }
            else
            {
         #endif
            flushCode = s4test.codeBase->fileFlush ;
            s4test.codeBase->fileFlush = 1 ;
            for ( loop = 0 ; loop < lenIn ; loop++ )
            {
               val = out[loop] ;
               if ( val >= '0' && val <= '9' )
               {
                  file4longAssignLong( pos, len ) ;
                  file4writeInternal( &s4test, pos, &val, 1 ) ;
                  file4longAdd( &len, 1 ) ;
               }
               else
               {
                  intVal = val / 100 ;
                  buf[0] = '0' ;
                  buf[1] = 'x' ;
                  buf[2] = intVal + '0' ;
                  val -= intVal * 100 ;
                  intVal = val / 10 ;
                  buf[3] = intVal + '0' ;
                  val -= intVal * 10 ;
                  buf[4] = val + '0' ;
                  buf[4] = ' ' ;
                  file4longAssignLong( pos, len ) ;
                  file4writeInternal( &s4test, pos, (void *)buf, 6 ) ;
                  file4longAdd( &len, 6 ) ;
               }

            }
            file4flush( &s4test ) ;
            s4test.codeBase->fileFlush = flushCode ;
         #ifdef S4MEM_PRINT
            }
         #endif
      }

      error4set( s4test.codeBase, errCode ) ;

      #ifdef S4SERVER
         #ifndef S4OFF_THREAD
            LeaveCriticalSection( &crit4 ) ;
         #endif
      #endif
   }
#endif



#ifdef S4TESTING
   /* LY July 9/03 : added static */
   static int ats4errorFlag = 0 ;   /* used to prevent infinite loops */
#endif



#if defined(S4TESTING) || defined(S4TRACK_FILES)

extern int resetInProgress ;

/* S4TESTING or S4TRACK_FILES */
void S4FUNCTION u4writeErr( const char *errStr, int newLine )
{
   int errCode, flushCode ;
   FILE4LONG len, len2 ;
   #ifdef S4TESTING
      CODE4 cb ;
      int undoLocal = 0 ;
   #endif

   #ifdef S4SERVER
      if ( isInit == 0 )
      {
         #ifndef S4OFF_THREAD
            InitializeCriticalSection( &crit4 ) ;
         #endif
         isInit = 1 ;
      }
   #endif

   #ifdef S4TESTING
      #ifndef S4SERVER
         if ( ats4errorFlag == 0 )
         {
            ats4errorFlag = 1 ;
            ats4setSuiteStatus( "E" ) ;
            ats4errorFlag = 0 ;
         }
      #endif
   #endif

   // AS May 4/07 - support for in4temp in testing case as well
   #if defined( S4MEM_PRINT ) || defined( S4TESTING )
      if ( in4temp == 1 || resetInProgress == 1 )
         return ;
   #endif

   #ifdef S4TESTING
      #ifdef S4SERVER
         #ifndef S4OFF_THREAD
            EnterCriticalSection( &crit4 ) ;
         #endif
      #endif
   #endif

   if ( s4test.hand == INVALID4HANDLE )
   {
      #ifdef S4TESTING
         // #ifndef S4SERVER
            /* reopen the log if it has been previously closed:   */
            //#ifdef S4MEM_PRINT
               in4temp = 1 ;
            //#endif
            // AS May 7/07 the code4init may fail, in which case just skip...
            if ( code4initLow( &cb, DEF4PROTOCOL, S4VERSION, code4structSize(&cb), 1 ) != 0 )
               return ;
            /* the code4initLow() call should have opened or created s4test  */
            #ifdef S4STAND_ALONE
               code4logOpenOff( &cb ) ;
            #endif
            if( s4test.hand == INVALID4HANDLE )
            {
               #ifndef S4SERVER
                  code4close(&cb) ;
               #endif
               code4initUndo(&cb) ;
               #ifdef S4SERVER
                  #ifndef S4OFF_THREAD
                     LeaveCriticalSection( &crit4 ) ;
                  #endif
               #endif
               //#ifdef S4MEM_PRINT
                  in4temp = 0 ;
               //#endif
               return ;
            }
            //#ifdef S4MEM_PRINT
               in4temp = 0 ;
            //#endif
            undoLocal = 1 ;
         // #endif
      #endif
   }

   if( s4test.hand == INVALID4HANDLE )  /* even for non-testing just ensure else return */
   {
      #ifdef S4SERVER
         #ifndef S4OFF_THREAD
            LeaveCriticalSection( &crit4 ) ;
         #endif
      #endif
      return ;
   }

   // AS 12/15/99 if s4test.codeBase is 0, then there is a serious problem because the
   // handle is valid, but the FILE4 structure is not properly initialized...
   // was getting in endless recursive function calls in this case...
   if ( s4test.codeBase == 0 )
      return ;

   errCode = error4code( s4test.codeBase ) ;
   error4set( s4test.codeBase, 0 ) ;
   // AS Jan 17/06 - also disable the errOut to avoid getting into endless loop...
   s4test.codeBase->errOff = 1 ;

   len = file4lenLow( &s4test ) ;

   if( file4longError( len ) >= 0 )
   {
      #ifdef S4MEM_PRINT
         if ( write4buf == 0 )
         {
            in4temp = 1 ;
            write4buf = (char *)u4alloc( 40000L ) ;
            if ( write4buf != 0 )
            {
               file4seqWriteInitLow( &file4seq, &s4test, len, write4buf, (unsigned int)40000 ) ;
               file4seqPtr = &file4seq ;
            }
            in4temp = 0 ;
         }
      #endif

      if ( newLine )
      {
         #ifdef S4MEM_PRINT
            if ( write4buf != 0 )
            {
               in4temp = 1 ;
               file4seqWrite( &file4seq, "\r\n", 2 ) ;
               file4longAdd( &len, 2 ) ;
               in4temp = 0 ;
            }
            else
            {
         #endif
               file4writeInternal( &s4test, len, "\r\n", 2 ) ;
               file4longAdd( &len, 2 ) ;
         #ifdef S4MEM_PRINT
            }
         #endif
      }

      #ifdef S4MEM_PRINT
         if ( write4buf != 0 )
         {
            in4temp = 1 ;
            file4seqWrite( &file4seq, errStr, strlen( errStr ) ) ;
            in4temp = 0 ;
         }
         else
         {
      #endif
            // AS May 16/03 - It is possible for s4test->codeBase to be nulled out
            // I think if we lsot client connection.
            if ( s4test.codeBase != 0 )
            {
               flushCode = s4test.codeBase->fileFlush ;
               s4test.codeBase->fileFlush = 1 ;
               file4longAssignLong( len2, len ) ;
               file4longAdd( &len2, strlen( errStr ) ) ;
               file4lenSetLow( &s4test, len2 ) ;
               file4writeInternal( &s4test, len, (void *)errStr, strlen( errStr ) ) ;
               file4flush( &s4test ) ;
               if ( s4test.codeBase != 0 )
                  s4test.codeBase->fileFlush = flushCode ;
            }
      #ifdef S4MEM_PRINT
         }
      #endif
   }

   s4test.codeBase->errOff = 0 ;
   error4set( s4test.codeBase, errCode ) ;

   #ifdef S4TESTING
      if ( undoLocal )
      {
         #ifndef S4SERVER
            code4close(&cb) ;
         #endif
         code4initUndo(&cb) ;
      }

      #ifdef S4SERVER
         #ifndef S4OFF_THREAD
            LeaveCriticalSection( &crit4 ) ;
         #endif
      #endif
   #endif
}

#endif /* defined(S4TESTING) || defined(S4TRACK_FILES) */




void S4FUNCTION u4yymmdd( char *yymmdd )
{
   #if   defined(S4WINCE) || defined(S4WIN32)
      SYSTEMTIME st ;
      GetLocalTime( &st ) ;
      // AS 04/30/98 year 2000 issue, fox uses '0x00', dBASE '0x64'
      // AS June 04/01 - Clipper follows Fox of using 0x00, but was coded
      // to do same as MDX.  This does not actually cause any issues because
      // both Clipper and CodeBase ignore this value, but there are some
      // potential problems with other 3rd party products which may use this value.
      // #ifdef S4FOX
      #ifndef S4MDX
         yymmdd[0] = (char)(st.wYear % 100) ;
      #else
         yymmdd[0] = (char)(st.wYear - 1900) ;
      #endif
      yymmdd[1] = (char)st.wMonth ;
      yymmdd[2] = (char)st.wDay ;
   #elif defined( S4PALM )
      DateTimeType timeNow;
      TimSecondsToDateTime(TimGetSeconds(),&timeNow);
      #ifdef S4FOX
         yymmdd[0] = (char)(timeNow.year % 100) ;
      #else
         yymmdd[0] = (char)(timeNow.year - 1900) ;
      #endif
      yymmdd[1] = (char)timeNow.month ;
      yymmdd[2] = (char)timeNow.day ;
   #elif defined( S4UNIX_THREADS )
      time_t timeVal ;
      struct tm result ;

      time( (time_t *) &timeVal ) ;
      localtime_r( (time_t *) &timeVal, &result) ;
      // AS 04/30/98 year 2000 issue, fox uses '0x00', dBASE '0x64'
      #ifdef S4FOX
         yymmdd[0] = (char)(result.tm_year % 100) ;
      #else
         yymmdd[0] = (char)result.tm_year ;
      #endif
      yymmdd[1] = (char)(result.tm_mon + 1) ;
      yymmdd[2] = (char)result.tm_mday ;
   #else
      time_t timeVal ;
      struct tm *tmPtr ;

      time( (time_t *) &timeVal ) ;
      tmPtr =  localtime( (time_t *) &timeVal ) ;
      /* AS 04/30/98 year 2000 issue, fox uses '0x00', dBASE '0x64' */
      #ifdef S4FOX
         yymmdd[0] = (char)(tmPtr->tm_year % 100) ;
      #else
         yymmdd[0] = (char)tmPtr->tm_year ;
      #endif
      yymmdd[1] = (char)(tmPtr->tm_mon + 1) ;
      yymmdd[2] = (char)tmPtr->tm_mday ;
   #endif
}



int S4FUNCTION u4remove( const char *ptr )
{
   #ifdef S4UNICODE
      WSTR5 name[256] ;
   #endif

   #ifdef E4PARM_LOW
      if ( ptr == 0 )
         return error4( 0, e4parm_null, E94504 ) ;
   #endif

   #if defined( S4PALM )
      return FileDelete( 0, (char *)ptr );
   #elif defined( S4UNICODE )
      c4atou(ptr, name, 256) ;

      #ifdef S4WINCE
         DWORD err ;
         for ( short numTries = 0 ;; numTries++ )
         {
      #endif
            BOOL blres = DeleteFile( name ) ;
            if ( blres )
               return 0 ;
            err = GetLastError() ;
      #ifdef S4WINCE
            if ( numTries > 10 )  // wait up to 5 seconds
               break ;
            Bool5 doBreak = 0 ;
            switch( err )  // only retry on certain errors which occur with this problem.
            {
               case ERROR_ACCESS_DENIED:
               case ERROR_DEVICE_NOT_AVAILABLE:
               case ERROR_DEVICE_REMOVED:
               case ERROR_FILE_NOT_FOUND:
                  break ;
               default:
                  doBreak = 1 ;
                  break ;
            }
            if ( doBreak )
               break ;
            Sleep( 500 ) ;
         }
      #endif
      return GetLastError() * -1 ;
   #elif defined( S4NO_REMOVE )
      return( unlink( ptr ) ) ;
   #else
      #if defined( S4MACINTOSH ) && defined( S4TESTING ) // LY Aug 26/04
         int count = 0 ;
         int rc = remove( ptr ) ;
         while ( rc && count++ < 1000 )
            rc = remove( ptr ) ;

         return rc ;
      #else
         int rc = remove( ptr ) ;
         // AS Dec 1/06 - provide a short delay here...dual core window xp systems appear to get stuck here due to a buffering issue
         u4delayHundredth( 5 ) ;
         return rc ;
      #endif
   #endif
}

#ifdef S4PALM
   static int palm4rename( const char *oldName, const char *newName )
   {
      unsigned char buffer[1024];
      Int32 numRead;
      FileHand dest,src;
      dest = FileOpen( 0, (char *)newName, 0, 0, fileModeReadWrite | fileModeExclusive | fileModeAnyTypeCreator, 0 );
      if (dest == 0)
         return -1;
      src = FileOpen( 0, (char *)oldName, 0, 0, fileModeReadOnly | fileModeTemporary | fileModeExclusive | fileModeAnyTypeCreator, 0 );
      if (src == 0)
      {
         FileClose(dest);
         return -1;
      }

      do
      {
         numRead = FileRead(src, buffer, 1, sizeof(buffer),0);
         FileWrite(dest, buffer, 1, numRead, 0);
      } while (numRead == sizeof(buffer));

      FileClose(src);
      FileClose(dest);
      return 0;
   }
#endif



int S4FUNCTION u4rename( const char *oldName, const char *newName )
{
   #ifdef S4NO_RENAME
      return 0 ;
   #else
      #ifdef S4UNICODE
         WSTR5 from[256], to[256] ;
      #endif
      #ifdef S4NO_RENAME
         char  buf[250] ;

         c4memset( (void *)buf, 0, sizeof(buf) ) ;
      #endif

      #ifdef E4PARM_LOW
         if ( oldName == 0 || newName == 0 )
            return error4( 0, e4parm_null, E94505 ) ;
      #endif

      #if   defined( S4PALM )
         return palm4rename( oldName, newName ) ;
      #elif defined( S4UNICODE )
         c4atou(oldName, from, 256) ;
         c4atou(newName, to, 256 ) ;
         // AS Feb 17/06 - for Windows CE, it is possible to get errors if the card is not ready...retry if access denied error
         // Microsoft knowledgebase article #811693
         BOOL bRes ;
         #ifdef S4WINCE
            for ( short numTries = 0 ;; numTries++ )
            {
         #endif
               bRes = MoveFile(from, to ) ;
         #ifdef S4WINCE
               if ( bRes == 0 )
               {
                  DWORD err = GetLastError() ;
                  if ( numTries > 10 )  // wait up to 5 seconds
                     break ;
                  Bool5 doBreak = 0 ;
                  switch( err )  // only retry on certain errors which occur with this problem.
                  {
                     case ERROR_ACCESS_DENIED:
                     case ERROR_DEVICE_NOT_AVAILABLE:
                     case ERROR_DEVICE_REMOVED:
                     case ERROR_FILE_NOT_FOUND:
                        break ;
                     default:
                        doBreak = 1 ;
                        break ;
                  }
                  if ( doBreak )
                     break ;
                  Sleep( 500 ) ;
               }
               else
                  break ;
            }
         #endif
         return bRes ;
      #else
         #ifdef S4NO_RENAME
            #ifdef S4UNIX
               c4memcpy( (void *)buf, "mv ", 3 ) ; /* system rename or move call */
            #else
               c4memcpy( (void *)buf, "rename ", 7 ) ; /* system copy call */
            #endif
            c4strcat( buf, oldName ) ;
            c4strcat( buf, " " ) ;
            c4strcat( buf, newName ) ;
            return system( buf ) ;
         #else
            #ifdef S4MACINTOSH
               remove( newName ) ;
            #endif
            return rename( oldName, newName ) ;
         #endif
      #endif
   #endif
}



#ifdef S4VB_DOS
   int S4FUNCTION u4ncpy_v( char *to, char *from, int len )
   {
      return u4ncpy( StringAddress(to), from, (unsigned)len ) ;
   }

   int S4FUNCTION u4ncpy_v2( char *to, char *from, int len )
   {
      return u4ncpy( to, c4str(from), len ) ;
   }
#endif



#ifdef S4WINTEL
   #ifndef S4WIN64   /* LY 00/09/21 */
      long S4FUNCTION v4Cstring(char *s)
      {
         char *d = 0 ;

         if( s )
         {
            // AS Dec 13/05 - don't use this function.  It is deprecated and inefficient
            size_t len = strlen( s ) + 1 ;
            d=  (char *)u4alloc( len );
            // strcpy(d,s);
            memcpy( d, s, len ) ;
         }
         return (long) d;
      }

      void S4FUNCTION v4Cstringfree(char *s)
      {
         if( s )
         {
           u4free(s);
         }
      }
   #endif
#endif   /* S4WINDOWS */

#ifdef S4PALM
   int S4CALL u4memcmp( S4CMP_PARM s1, S4CMP_PARM s2, size_t len )
   {
      return MemCmp(s1,s2,len) ;
   }
#endif
#ifndef S4FOX
   /*   memory comparison routines for foreign languages */

   /*  v4map structure :  There are three lines of numbers in each row.
                          The upper line (commented out) represents ascii
                          characters.  The middle line represents the
                          ascii value.  The lower line is the precedence value
                          of the corresponding ascii value as compared to
                          the other ascii values. There are two v4map
                          structures: one uses the ANSI (Windows) char set,
                          which will handle English, French and German.
                          The second is an OEM (ASCII) set for German and
                          French character sets.
   */
   #ifdef S4ANSI
      unsigned char v4map[256] =
      {
         /*     ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤ */
         /*     0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 */
              194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,

         /*     ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤ */
         /*    16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 */
              210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,

         /*         !   "   #   $   %   &   '   (   )   *   +   ,   -   .   / */
         /*    32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47 */
                1,156,162,170,157,135,154,163,123,124,136,132,167,133,152,153,

         /*     0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ? */
         /*    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63 */
                3,  4,  5,  6,  7,  8,  9, 10, 11, 12,168,169,129,131,130,171,

         /*     @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O */
         /*    64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79 */
              174, 13, 20, 21, 23, 24, 29, 30, 31, 32, 37, 39, 40, 41, 42, 44,

         /*     P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _ */
         /*    80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95 */
               50, 51, 52, 53, 54, 55, 60, 61, 62, 63, 66,125,172,126,151,173,

         /*     `   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o */
         /*    96, 97, 98, 99,100,101,102,103,104,105,106,107,108,109,110,111 */
              164, 67, 74, 75, 77, 78, 83, 84, 85, 86, 91, 93, 94, 95, 96, 98,

         /*     p   q   r   s   t   u   v   w   x   y   z   {   |   }       ≤ */
         /*   112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127 */
              104,105,106,107,109,110,115,116,117,118,122,127,155,128,150,226,

         /*     ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤ */
         /*   128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143 */
              227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,

         /*     ≤   `   '   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤ */
         /*   144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159 */
              243,165,166,244,245,246,247,248,249,250,251,252,253,254,255,255,

         /*         ≠   õ   ú XXX  ù    |      " XXX   ¶   Æ   ™   - XXX XXX */
         /*   160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175 */
                2,175,158,159,160,161,176,177,178,179,181,147,149,134,180,183,

         /*     ¯   Ò   ˝ XXX   '   Ê    XXX   , XXX   ß   Ø   ¨   ´ XXX   ® */
         /*   176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191 */
              140,139,142,143,184,185,186,187,188,141,182,148,144,145,146,189,

         /*     A   A   A   A   é   è   í   Ä   E   ê   E   E   I   I   I   I */
         /*   192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207 */
               16, 15, 17, 19, 14, 18,192, 22, 27, 26, 28, 25, 35, 34, 36, 33,

         /*     D   •   O   O   O   O   ô   X   0   U   U   U   ö   Y   b   · */
         /*   208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223 */
               38, 43, 47, 46, 48, 49, 45,137,190, 58, 57, 59, 56, 64, 65,108,

         /*     Ö   †   É   a   Ñ   Ü   ë   á   ä   Ç   à   â   ç   °   å   ã */
         /*   224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239 */
               70, 69, 71, 73, 68, 72,193, 76, 81, 80, 82, 79, 89, 88, 90, 87,

         /*     Â   §   ï   ¢   ì   o   î   ˆ   0   ó   £   ñ   Å   y   b   ò */
         /*   240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255 */
               92, 97,101,100,102,103, 99,138,191,113,112,114,111,120,121,119,
      } ;

      #ifndef S4LANGUAGE
         int S4CALL u4memcmp( S4CMP_PARM s1, S4CMP_PARM s2, size_t len )
         {
            unsigned int i ;

            for (i=0; i<len; i++)
               if ( ((unsigned char *)s1)[i] != ((unsigned char *)s2)[i] )
               {
                  if ( v4map[((unsigned char *)s1)[i]] < v4map[((unsigned char *)s2)[i]] )
                     return -1 ;
                  return 1 ;
               }

            return 0 ;
         }
      #endif
   #endif

   #if defined( S4LANGUAGE ) && defined( S4GERMAN ) && defined( S4ANSI )
      typedef struct
      {
         unsigned char extChar ;
         char expChars[3] ;
      } LANGUAGE_CONVERT ;

      LANGUAGE_CONVERT v4table[] =
      {
         { 246, "oe" },  /* î */
         { 223, "ss" },  /* · */
         { 228, "ae" },  /* Ñ */
         { 252, "ue" },  /* Å */
         { 235, "ee" },  /* â */
         #ifdef S4CLIPPER
            { 196, "AE" },  /* é */
            { 214, "OE" },  /* ô */
            { 220, "UE" },  /* ö */
         #else
            { 196, "Ae" },
            { 214, "Oe" },
            { 220, "Ue" },
         #endif
         { 233, "e " },  /* Ç */
         { 226, "a " },  /* É */
         { 224, "a " },  /* Ö */
         { 229, "a " },  /* Ü */
         { 234, "e " },  /* à */
         { 232, "e " },  /* ä */
         { 238, "i " },  /* å */
         { 236, "i " },  /* ç */
         { 197, "A " },  /* è */
         /* The angstrom is not indexed in German correctly, so this is used instead*/
         { 244, "o " },  /* ì */
         { 242, "o " },  /* ï */
         { 251, "u " },  /* ñ */
         { 249, "u " },  /* ó */
         { 255, "y " },  /* ò */
         { 225, "a " },  /* † */
         { 237, "i " },  /* ° */
         { 243, "o " },  /* ¢ */
         { 250, "u " },  /* £ */
         { 241, "n " },  /* § */
         {   0, "  " },   /* A blank entry to make the u4valid work better */
      };
   #endif

   #if defined( S4LANGUAGE ) && defined( S4GERMAN ) && !defined( S4ANSI ) && defined( S4CLIPPER )
      unsigned char v4map[256] =
      {
         /*      ,  ,  ,  ,  ,  ,  ,  ,  ,   ,   ,  ,  ,   ,  ,   */
         /*     0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 */
                0,  1,  2,  3,  4,  5,  6,  7,  8, 30,  9, 10, 11, 31, 12, 13,

         /*     ,  ,  ,  ,  ,  ,  ,  ,  ,  ,EOF,  ,  ,  ,  ,   */
         /*    16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 */
               14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,

         /*      ,  !,  ",  #,  $,  %,  &,  ',  (,  ),  *,  +,  ,,  -,  .,  / */
         /*    32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47 */
               32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,

         /*     0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  :,  ;,  <,  =,  >,  ? */
         /*    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63 */
               48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,

         /*     @,  A,  B,  C,  D,  E,  F,  G,  H,  I,  J,  K,  L,  M,  N,  O */
         /*    64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79 */
               64, 65, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80,

         /*     P,  Q,  R,  S,  T,  U,  V,  W,  X,  Y,  Z,  [,  \,  ],  ^,  _ */
         /*    80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95 */
               82, 83, 84, 85, 86, 87, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98,

         /*     `,  a,  b,  c,  d,  e,  f,  g,  h,  i,  j,  k,  l,  m,  n,  o */
         /*    96, 97, 98, 99,100,101,102,103,104,105,106,107,108,109,110,111 */
               99,100,102,103,104,105,106,107,108,109,110,111,112,113,114,115,

         /*     p,  q,  r,  s,  t,  u,  v,  w,  x,  y,  z,  {,  |,  },   ,   */
         /*   112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127 */
              117,118,119,120,122,123,125,126,127,128,129,130,131,132,133,134,

         /*     Ä,  Å,  Ç,  É,  Ñ,  Ö,  Ü,  á,  à,  â,  ä,  ã,  å,  ç,  é,  è */
         /*   128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143 */
              135,124,136,137,101,138,139,140,141,142,143,144,145,146, 66,147,

         /*     ê,  ë,  í,  ì,  î,  ï,  ñ,  ó,  ò,  ô,  ö,  õ,  ú,  ù,  û,  ü */
         /*   144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159 */
              148,149,150,151,116,152,153,154,155, 81, 88,156,157,158,159,160,

         /*     †,  °,  ¢,  £,  §,  •,  ¶,  ß,  ®,  ©,  ™,  ´,  ¨,  ≠,  Æ,  Ø */
         /*   160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175 */
              161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,

         /*     ∞,  ±,  ≤,  ≥,  ¥,  µ,  ∂,  ∑,  ∏,  π,  ∫,  ª,  º,  Ω,  æ,  ø */
         /*   176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191 */
              177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,

         /*     ¿,  ¡,  ¬,  √,  ƒ,  ≈,  ∆,  «,  »,  …,   ,  À,  Ã,  Õ,  Œ,  œ */
         /*   192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207 */
              193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,

         /*     –,  —,  “,  ”,  ‘,  ’,  ÷,  ◊,  ÿ,  Ÿ,  ⁄,  €,  ‹,  ›,  ﬁ,  ﬂ */
         /*   208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223 */
              209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,

         /*     ‡,  ·,  ‚,  „,  ‰,  Â,  Ê,  Á,  Ë,  È,  Í,  Î,  Ï,  Ì,  Ó,  Ô */
         /*   224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239 */
              225,121,226,227,228,229,230,231,232,233,234,235,236,237,238,239,

         /*     ,  Ò,  Ú,  Û,  Ù,  ı,  ˆ,  ˜,  ¯,  ˘,  ˙,  ˚,  ¸,  ˝,  ˛,    */
         /*   240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255 */
              240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,
      } ;
   #endif
   #if defined( S4LANGUAGE ) && defined( S4GERMAN ) && !defined( S4ANSI ) && !defined( S4CLIPPER )
      unsigned char v4map[256] =
      {
         /*      ,  ,  ,  ,  ,  ,  ,  ,  ,   ,   ,  ,  ,   ,  ,   */
         /*     0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 */
                0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,

         /*     ,  ,  ,  ,  ,  ,  ,  ,  ,  ,EOF,  ,  ,  ,  ,   */
         /*    16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 */
               16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,

         /*      ,  !,  ",  #,  $,  %,  &,  ',  (,  ),  *,  +,  ,,  -,  .,  / */
         /*    32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47 */
               32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,

         /*     0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  :,  ;,  <,  =,  >,  ? */
         /*    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63 */
              158,159,160,161,162,163,164,165,166,167, 48, 49, 50, 51, 52, 53,

         /*     @,  A,  B,  C,  D,  E,  F,  G,  H,  I,  J,  K,  L,  M,  N,  O */
         /*    64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79 */
               54,168,172,173,175,176,178,179,180,181,182,183,184,185,186,188,

         /*     P,  Q,  R,  S,  T,  U,  V,  W,  X,  Y,  Z,  [,  \,  ],  ^,  _ */
         /*    80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95 */
              190,191,192,193,194,195,197,198,199,200,201, 55, 56, 57, 58, 59,

         /*     `,  a,  b,  c,  d,  e,  f,  g,  h,  i,  j,  k,  l,  m,  n,  o */
         /*    96, 97, 98, 99,100,101,102,103,104,105,106,107,108,109,110,111 */
               60,202,210,211,213,214,219,220,221,222,227,228,229,230,231,233,

         /*     p,  q,  r,  s,  t,  u,  v,  w,  x,  y,  z,  {,  |,  },   ,   */
         /*   112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127 */
              239,240,241,242,244,245,250,251,252,253,255, 61, 62, 63, 64, 65,

         /*     Ä,  Å,  Ç,  É,  Ñ,  Ö,  Ü,  á,  à,  â,  ä,  ã,  å,  ç,  é,  è */
         /*   128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143 */
              174,249,215,205,206,204,207,212,217,218,216,226,225,224,169,170,

         /*     ê,  ë,  í,  ì,  î,  ï,  ñ,  ó,  ò,  ô,  ö,  õ,  ú,  ù,  û,  ü */
         /*   144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159 */
              177,208,171,236,237,235,248,247,254,189,196, 66, 67, 68, 69, 70,

         /*     †,  °,  ¢,  £,  §,  •,  ¶,  ß,  ®,  ©,  ™,  ´,  ¨,  ≠,  Æ,  Ø */
         /*   160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175 */
              203,223,234,246,232,187,209,238, 71, 72, 73, 74, 75, 76, 77, 78,

         /*     ∞,  ±,  ≤,  ≥,  ¥,  µ,  ∂,  ∑,  ∏,  π,  ∫,  ª,  º,  Ω,  æ,  ø */
         /*   176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191 */
               79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94,

         /*     ¿,  ¡,  ¬,  √,  ƒ,  ≈,  ∆,  «,  »,  …,   ,  À,  Ã,  Õ,  Œ,  œ */
         /*   192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207 */
               95, 96, 97, 98, 99,100,101,102,103,104,105,106,107,108,109,110,

         /*     –,  —,  “,  ”,  ‘,  ’,  ÷,  ◊,  ÿ,  Ÿ,  ⁄,  €,  ‹,  ›,  ﬁ,  ﬂ */
         /*   208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223 */
              111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,

         /*     ‡,  ·,  ‚,  „,  ‰,  Â,  Ê,  Á,  Ë,  È,  Í,  Î,  Ï,  Ì,  Ó,  Ô */
         /*   224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239 */
              127,243,128,129,130,131,132,133,134,135,136,137,138,139,140,141,

         /*     ,  Ò,  Ú,  Û,  Ù,  ı,  ˆ,  ˜,  ¯,  ˘,  ˙,  ˚,  ¸,  ˝,  ˛,    */
         /*   240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255 */
              142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157
      } ;
   #endif

   #if defined( S4LANGUAGE ) && defined( S4GERMAN ) && !defined( S4ANSI )
      typedef struct
      {
         unsigned char extChar ;
         char expChars[3] ;
      } LANGUAGE_CONVERT ;

      LANGUAGE_CONVERT v4table[] =
      {
         { 148, "oe" },
         { 225, "ss" },
         { 132, "ae" },
         { 129, "ue" },
         { 130, "ee" },
         #ifdef S4CLIPPER
            { 142, "AE" },
            { 153, "OE" },
            { 154, "UE" },
         #else
            { 142, "Ae" },
            { 153, "Oe" },
            { 154, "Ue" },
         #endif
         { 131, "a " },
         { 133, "a " },
         { 134, "a " },
         { 136, "e " },
         { 137, "e " },
         { 138, "e " },
         { 140, "i " },
         { 141, "i " },
         { 143, "A " },
         /* The angstrom is not indexed in German correctly, so this is used instead*/
         { 147, "o " },
         { 149, "o " },
         { 150, "u " },
         { 151, "u " },
         { 152, "y " },
         { 160, "a " },
         { 161, "i " },
         { 162, "o " },
         { 163, "u " },
         { 164, "n " },
         {   0, "  " },   /* A blank entry to make the u4valid work better */
      } ;
   #endif /* S4ANSI */

   #if defined( S4LANGUAGE ) && defined( S4GERMAN )
      #ifdef S4DICTIONARY
         /* sort method uses the dictionary sort order */
         int S4CALL u4memcmp( S4CMP_PARM s1, S4CMP_PARM s2, size_t len )
         {
            int i ;

            for ( i = 0 ; i < len ; i++ )
               if ( ((unsigned char *)s1)[i] != ((unsigned char *)s2)[i] )
               {
                  if ( v4map[((unsigned char *)s1)[i]] < v4map[((unsigned char *)s2)[i]] )
                     return -1 ;
                  return 1 ;
               }

            return 0 ;
         }
      #else   /* if PHONEBOOK SORT (default) */
         /* sort method uses the phone book sort order */
         static unsigned char *u4valid( unsigned char * parm )
         {
            int x = 0 ;

            while ( v4table[x].extChar != 0 )
            {
               if ( v4table[x].extChar == *(unsigned char *)parm )
                  return (unsigned char *)v4table[x].expChars ;
               x++ ;
            }
            return parm ;
         }

         int S4CALL u4memcmp( S4CMP_PARM s1, S4CMP_PARM s2, size_t len )
         {
            short s1Ext, s2Ext ;
            unsigned char *compare1, *compare2 ;
            unsigned char *string1Ptr, *string2Ptr ;

            string1Ptr = (unsigned char *)s1 ;
            string2Ptr = (unsigned char *)s2 ;

            while( len-- )
            {
               if( *string1Ptr != *string2Ptr )
               {
                  /* The characters are not equal.  Check for extended characters
                     as in German the extended characters are equivalent to
                     expanded characters  */

                  s1Ext = ( (short)*string1Ptr > 127 ) ;
                  s2Ext = ( (short)*string2Ptr > 127 ) ;

                  // AS 06/14/99 - if both were extended there were failures.
                  // changes.60 fix #183
                  if( s1Ext || s2Ext )
                  {
                     /* Only one is an extended character. Check to see if valid and
                     expand to full length */

                     compare1 = (s1Ext) ? u4valid(string1Ptr) : string1Ptr ;
                     compare2 = (s2Ext) ? u4valid(string2Ptr) : string2Ptr ;

                     /* Expansion has been done to one string (maximum 2 chars in expansion) */
                     /* now compare the two characters */

                     if ( compare1[0] == compare2[0] )    /* do if first chars equal */
                     {
                        /* if there are not two valid second chars to check */
                        if ( compare1[1] == ' ' || compare2[1] == ' ' ||
                             compare1[1] == 0   || compare2[1] == 0 )
                        {
                           if (v4map[*string1Ptr] < v4map[*string2Ptr])  return -1 ;
                           return 1 ;
                        }

                        if ( compare1[1] == compare2[1] )
                        {
                           (s1Ext) ? string2Ptr++ : string1Ptr++ ;
                           if (len) len-- ;
                        }
                        else
                        {
                           if (v4map[*(compare1+1)] < v4map[*(compare2+1)])  return -1 ;
                           return 1 ;
                        }
                     }
                     else
                     {
                        if (v4map[*compare1] < v4map[*compare2])  return -1 ;
                        return 1 ;
                     }
                  }
                  else
                  {
                     /* Neither character is extended so return according to
                        v4map[].  */
                     if (v4map[*string1Ptr] < v4map[*string2Ptr])  return -1 ;
                     return 1 ;
                  }
               }
               /* Characters are equal. Increment the pointers and loop again. */

               string1Ptr++ ;
               string2Ptr++ ;
            }
            return 0 ;
         }
      #endif  /* S4DICTIONARY */
   #endif

   #ifdef S4LANGUAGE
      #ifdef S4FRENCH
         #ifndef S4ANSI
            /* This mapping is for French. */
            unsigned char v4map[256] =
            {
               /*      ,  ,  ,  ,  ,  ,  ,  ,  ,   ,   ,  ,  ,   ,  ,   */
               /*     0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 */
                      0,140,141,142,143,144,145,146,147,  1,148,149,150,  2,151,152,

               /*     ,  ,  ,  ,  ,  ,  ,  ,  ,  ,EOF,  ,  ,  ,  ,   */
               /*    16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 */
                    153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,

               /*      ,  !,  ",  #,  $,  %,  &,  ',  (,  ),  *,  +,  ,,  -,  .,  / */
               /*    32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47 */
                      3,117,132,123,124,109,122,134, 98, 99,110,107,112,108,111,118,

               /*     0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  :,  ;,  <,  =,  >,  ? */
               /*    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63 */
                      4,  5,  6,  7,  8,  9, 10, 11, 12, 13,114,113,104,106,105,116,

               /*     @,  A,  B,  C,  D,  E,  F,  G,  H,  I,  J,  K,  L,  M,  N,  O */
               /*    64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79 */
                    121, 14, 17, 18, 20, 21, 23, 24, 25, 26, 27, 28, 29, 30, 31, 33,

               /*     P,  Q,  R,  S,  T,  U,  V,  W,  X,  Y,  Z,  [,  \,  ],  ^,  _ */
               /*    80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95 */
                     35, 36, 37, 38, 39, 40, 42, 43, 44, 45, 46,100,119,101,135,137,

               /*     `,  a,  b,  c,  d,  e,  f,  g,  h,  i,  j,  k,  l,  m,  n,  o */
               /*    96, 97, 98, 99,100,101,102,103,104,105,106,107,108,109,110,111 */
                    133, 47, 53, 54, 56, 57, 62, 63, 64, 65, 70, 71, 72, 73, 74, 76,

               /*     p,  q,  r,  s,  t,  u,  v,  w,  x,  y,  z,  {,  |,  },   ,   */
               /*   112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127 */
                     81, 82, 83, 84, 86, 87, 92, 93, 94, 95, 97,102,120,103,136,169,

               /*     Ä,  Å,  Ç,  É,  Ñ,  Ö,  Ü,  á,  à,  â,  ä,  ã,  å,  ç,  é,  è */
               /*   128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143 */
                     19, 88, 58, 49, 48, 50, 51, 55, 59, 60, 61, 66, 67, 68, 15, 16,

               /*     ê,  ë,  í,  ì,  î,  ï,  ñ,  ó,  ò,  ô,  ö,  õ,  ú,  ù,  û,  ü */
               /*   144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159 */
                     22,130,131, 78, 77, 79, 89, 90, 96, 34, 41,125,126,127,129,128,

               /*     †,  °,  ¢,  £,  §,  •,  ¶,  ß,  ®,  ©,  ™,  ´,  ¨,  ≠,  Æ,  Ø */
               /*   160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175 */
                     52, 69, 80, 91, 75, 32,138,139,115,170,171,172,173,174,175,176,

               /*     ∞,  ±,  ≤,  ≥,  ¥,  µ,  ∂,  ∑,  ∏,  π,  ∫,  ª,  º,  Ω,  æ,  ø */
               /*   176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191 */
                    177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,

               /*     ¿,  ¡,  ¬,  √,  ƒ,  ≈,  ∆,  «,  »,  …,   ,  À,  Ã,  Õ,  Œ,  œ */
               /*   192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207 */
                    193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,

               /*     –,  —,  “,  ”,  ‘,  ’,  ÷,  ◊,  ÿ,  Ÿ,  ⁄,  €,  ‹,  ›,  ﬁ,  ﬂ */
               /*   208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223 */
                    209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,

               /*     ‡,  ·,  ‚,  „,  ‰,  Â,  Ê,  Á,  Ë,  È,  Í,  Î,  Ï,  Ì,  Ó,  Ô */
               /*   224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239 */
                    225, 85,226,227,228,229,230,231,232,233,234,235,236,237,238,239,

               /*     ,  Ò,  Ú,  Û,  Ù,  ı,  ˆ,  ˜,  ¯,  ˘,  ˙,  ˚,  ¸,  ˝,  ˛,    */
               /*   240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255 */
                    240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,
            } ;

         #endif /* ifndef S4ANSI */

         int S4CALL u4memcmp( S4CMP_PARM s1, S4CMP_PARM s2, size_t len )
         {
            int i ;

            for ( i = 0 ; i < len ; i++ )
               if ( ((unsigned char *)s1)[i] != ((unsigned char *)s2)[i] )
               {
                  if ( v4map[((unsigned char *)s1)[i]] < v4map[((unsigned char *)s2)[i]] )
                     return -1 ;
                  return 1 ;
               }

            return 0 ;
         }
      #endif  /* S4FRENCH */

      #ifdef S4SWEDISH
         #ifndef S4ANSI
            /* This mapping is for Swedish. */
            unsigned char v4map[256] =
            {
               /*      ,  ,  ,  ,  ,  ,  ,  ,  ,   ,   ,  ,  ,   ,  ,   */
               /*     0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 */
                      0,140,141,142,143,144,145,146,147,  1,148,149,150,  2,151,152,

               /*     ,  ,  ,  ,  ,  ,  ,  ,  ,  ,EOF,  ,  ,  ,  ,   */
               /*    16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 */
                    153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,

               /*      ,  !,  ",  #,  $,  %,  &,  ',  (,  ),  *,  +,  ,,  -,  .,  / */
               /*    32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47 */
                      3,119,132,125,126,111,124,134,100,101,112,109,114,110,113,120,

               /*     0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  :,  ;,  <,  =,  >,  ? */
               /*    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63 */
                      4,  5,  6,  7,  8,  9, 10, 11, 12, 13,116,115,106,108,107,118,

               /*     @,  A,  B,  C,  D,  E,  F,  G,  H,  I,  J,  K,  L,  M,  N,  O */
               /*    64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79 */
                    123, 14, 15, 16, 18, 19, 21, 22, 23, 24, 25, 26, 27, 28, 29, 31,

               /*     P,  Q,  R,  S,  T,  U,  V,  W,  X,  Y,  Z,  [,  \,  ],  ^,  _ */
               /*    80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95 */
                     32, 33, 34, 35, 36, 37, 39, 40, 41, 42, 43,102,121,103,135,137,

               /*     `,  a,  b,  c,  d,  e,  f,  g,  h,  i,  j,  k,  l,  m,  n,  o */
               /*    96, 97, 98, 99,100,101,102,103,104,105,106,107,108,109,110,111 */
                    133, 48, 52, 53, 55, 56, 61, 62, 63, 64, 69, 70, 71, 72, 73, 75,

               /*     p,  q,  r,  s,  t,  u,  v,  w,  x,  y,  z,  {,  |,  },   ,   */
               /*   112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127 */
                     79, 80, 81, 82, 84, 85, 90, 91, 92, 93, 95,104,122,105,136,169,

               /*     Ä,  Å,  Ç,  É,  Ñ,  Ö,  Ü,  á,  à,  â,  ä,  ã,  å,  ç,  é,  è */
               /*   128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143 */
                     17, 86, 57, 49, 97, 50, 96, 54, 58, 59, 60, 65, 66, 67, 45, 44,

               /*     ê,  ë,  í,  ì,  î,  ï,  ñ,  ó,  ò,  ô,  ö,  õ,  ú,  ù,  û,  ü */
               /*   144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159 */
                     20, 98, 46, 76, 99, 77, 87, 88, 94, 47, 38,127,128,129,131,130,

               /*     †,  °,  ¢,  £,  §,  •,  ¶,  ß,  ®,  ©,  ™,  ´,  ¨,  ≠,  Æ,  Ø */
               /*   160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175 */
                     51, 68, 78, 89, 74, 30,138,139,117,170,171,172,173,174,175,176,

               /*     ∞,  ±,  ≤,  ≥,  ¥,  µ,  ∂,  ∑,  ∏,  π,  ∫,  ª,  º,  Ω,  æ,  ø */
               /*   176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191 */
                    177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,

               /*     ¿,  ¡,  ¬,  √,  ƒ,  ≈,  ∆,  «,  »,  …,   ,  À,  Ã,  Õ,  Œ,  œ */
               /*   192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207 */
                    193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,

               /*     –,  —,  “,  ”,  ‘,  ’,  ÷,  ◊,  ÿ,  Ÿ,  ⁄,  €,  ‹,  ›,  ﬁ,  ﬂ */
               /*   208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223 */
                    209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,

               /*     ‡,  ·,  ‚,  „,  ‰,  Â,  Ê,  Á,  Ë,  È,  Í,  Î,  Ï,  Ì,  Ó,  Ô */
               /*   224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239 */
                    225, 83,226,227,228,229,230,231,232,233,234,235,236,237,238,239,

               /*     ,  Ò,  Ú,  Û,  Ù,  ı,  ˆ,  ˜,  ¯,  ˘,  ˙,  ˚,  ¸,  ˝,  ˛,    */
               /*   240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255 */
                    240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,
            } ;
         #endif /* S4ANSI */

         int S4CALL u4memcmp( S4CMP_PARM s1, S4CMP_PARM s2, size_t len )
         {
            int i ;

            for ( i = 0 ; i < len ; i++ )
               if ( ((unsigned char *)s1)[i] != ((unsigned char *)s2)[i] )
               {
                  if ( v4map[((unsigned char *)s1)[i]] < v4map[((unsigned char *)s2)[i]] )
                     return -1 ;
                  return 1 ;
               }

            return 0 ;
         }
      #endif  /* S4SWEDISH */

      #ifdef S4FINNISH
         #ifndef S4ANSI
            /* This mapping is for Finnish. */
            unsigned char v4map[256] =
            {
               /*      ,  ,  ,  ,  ,  ,  ,  ,  ,   ,   ,  ,  ,   ,  ,   */
               /*     0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 */
                      0,140,141,142,143,144,145,146,147,  1,148,149,150,  2,151,152,

               /*     ,  ,  ,  ,  ,  ,  ,  ,  ,  ,EOF,  ,  ,  ,  ,   */
               /*    16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 */
                    153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,

               /*      ,  !,  ",  #,  $,  %,  &,  ',  (,  ),  *,  +,  ,,  -,  .,  / */
               /*    32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47 */
                      3,119,132,125,126,111,124,134,100,101,112,109,114,110,113,120,

               /*     0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  :,  ;,  <,  =,  >,  ? */
               /*    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63 */
                      4,  5,  6,  7,  8,  9, 10, 11, 12, 13,116,115,106,108,107,118,

               /*     @,  A,  B,  C,  D,  E,  F,  G,  H,  I,  J,  K,  L,  M,  N,  O */
               /*    64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79 */
                    123, 14, 15, 16, 18, 19, 21, 22, 23, 24, 25, 26, 27, 28, 29, 31,

               /*     P,  Q,  R,  S,  T,  U,  V,  W,  X,  Y,  Z,  [,  \,  ],  ^,  _ */
               /*    80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95 */
                     32, 33, 34, 35, 36, 37, 39, 40, 41, 42, 43,102,121,103,135,137,

               /*     `,  a,  b,  c,  d,  e,  f,  g,  h,  i,  j,  k,  l,  m,  n,  o */
               /*    96, 97, 98, 99,100,101,102,103,104,105,106,107,108,109,110,111 */
                    133, 48, 52, 53, 55, 56, 61, 62, 63, 64, 69, 70, 71, 72, 73, 75,

               /*     p,  q,  r,  s,  t,  u,  v,  w,  x,  y,  z,  {,  |,  },   ,   */
               /*   112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127 */
                     79, 80, 81, 82, 84, 85, 90, 91, 92, 93, 95,104,122,105,136,169,

               /*     Ä,  Å,  Ç,  É,  Ñ,  Ö,  Ü,  á,  à,  â,  ä,  ã,  å,  ç,  é,  è */
               /*   128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143 */
                     17, 86, 57, 49, 97, 50, 96, 54, 58, 59, 60, 65, 66, 67, 45, 44,

               /*     ê,  ë,  í,  ì,  î,  ï,  ñ,  ó,  ò,  ô,  ö,  õ,  ú,  ù,  û,  ü */
               /*   144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159 */
                     20, 98, 46, 76, 99, 77, 87, 88, 94, 47, 38,127,128,129,131,130,

               /*     †,  °,  ¢,  £,  §,  •,  ¶,  ß,  ®,  ©,  ™,  ´,  ¨,  ≠,  Æ,  Ø */
               /*   160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175 */
                     51, 68, 78, 89, 74, 30,138,139,117,170,171,172,173,174,175,176,

               /*     ∞,  ±,  ≤,  ≥,  ¥,  µ,  ∂,  ∑,  ∏,  π,  ∫,  ª,  º,  Ω,  æ,  ø */
               /*   176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191 */
                    177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,

               /*     ¿,  ¡,  ¬,  √,  ƒ,  ≈,  ∆,  «,  »,  …,   ,  À,  Ã,  Õ,  Œ,  œ */
               /*   192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207 */
                    193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,

               /*     –,  —,  “,  ”,  ‘,  ’,  ÷,  ◊,  ÿ,  Ÿ,  ⁄,  €,  ‹,  ›,  ﬁ,  ﬂ */
               /*   208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223 */
                    209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,

               /*     ‡,  ·,  ‚,  „,  ‰,  Â,  Ê,  Á,  Ë,  È,  Í,  Î,  Ï,  Ì,  Ó,  Ô */
               /*   224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239 */
                    225, 83,226,227,228,229,230,231,232,233,234,235,236,237,238,239,

               /*     ,  Ò,  Ú,  Û,  Ù,  ı,  ˆ,  ˜,  ¯,  ˘,  ˙,  ˚,  ¸,  ˝,  ˛,    */
               /*   240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255 */
                    240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,
            } ;

         #endif /* S4ANSI */

         int S4CALL u4memcmp( S4CMP_PARM s1, S4CMP_PARM s2, size_t len )
         {
            int i ;

            for ( i = 0 ; i < len ; i++ )
               if ( ((unsigned char *)s1)[i] != ((unsigned char *)s2)[i] )
               {
                  if ( v4map[((unsigned char *)s1)[i]] < v4map[((unsigned char *)s2)[i]] )
                     return -1 ;
                  return 1 ;
               }

            return 0 ;
         }
      #endif  /* S4FINNISH */

      #ifdef S4NORWEGIAN
         #ifndef S4ANSI
            /* This mapping is for Norwegian. */
            unsigned char v4map[256] =
            {
               /*      ,  ,  ,  ,  ,  ,  ,  ,  ,   ,   ,  ,  ,   ,  ,   */
               /*     0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 */
                      0,140,141,142,143,144,145,146,147,  1,148,149,150,  2,151,152,

               /*     ,  ,  ,  ,  ,  ,  ,  ,  ,  ,EOF,  ,  ,  ,  ,   */
               /*    16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 */
                    153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,

               /*      ,  !,  ",  #,  $,  %,  &,  ',  (,  ),  *,  +,  ,,  -,  .,  / */
               /*    32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47 */
                      3,119,132,125,126,111,124,134,100,101,112,109,114,110,113,120,

               /*     0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  :,  ;,  <,  =,  >,  ? */
               /*    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63 */
                      4,  5,  6,  7,  8,  9, 10, 11, 12, 13,116,115,106,108,107,118,

               /*     @,  A,  B,  C,  D,  E,  F,  G,  H,  I,  J,  K,  L,  M,  N,  O */
               /*    64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79 */
                    123, 14, 15, 16, 18, 19, 21, 22, 23, 24, 25, 26, 27, 28, 29, 31,

               /*     P,  Q,  R,  S,  T,  U,  V,  W,  X,  Y,  Z,  [,  \,  ],  ^,  _ */
               /*    80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95 */
                     32, 33, 34, 35, 36, 37, 39, 40, 41, 42, 43,102,121,103,135,137,

               /*     `,  a,  b,  c,  d,  e,  f,  g,  h,  i,  j,  k,  l,  m,  n,  o */
               /*    96, 97, 98, 99,100,101,102,103,104,105,106,107,108,109,110,111 */
                    133, 48, 52, 53, 55, 56, 61, 62, 63, 64, 69, 70, 71, 72, 73, 75,

               /*     p,  q,  r,  s,  t,  u,  v,  w,  x,  y,  z,  {,  |,  },   ,   */
               /*   112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127 */
                     79, 80, 81, 82, 84, 85, 90, 91, 92, 93, 95,104,122,105,136,169,

               /*     Ä,  Å,  Ç,  É,  Ñ,  Ö,  Ü,  á,  à,  â,  ä,  ã,  å,  ç,  é,  è */
               /*   128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143 */
                     17, 86, 57, 49, 97, 50, 96, 54, 58, 59, 60, 65, 66, 67, 45, 44,

               /*     ê,  ë,  í,  ì,  î,  ï,  ñ,  ó,  ò,  ô,  ö,  õ,  ú,  ù,  û,  ü */
               /*   144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159 */
                     20, 98, 46, 76, 99, 77, 87, 88, 94, 47, 38,127,128,129,131,130,

               /*     †,  °,  ¢,  £,  §,  •,  ¶,  ß,  ®,  ©,  ™,  ´,  ¨,  ≠,  Æ,  Ø */
               /*   160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175 */
                     51, 68, 78, 89, 74, 30,138,139,117,170,171,172,173,174,175,176,

               /*     ∞,  ±,  ≤,  ≥,  ¥,  µ,  ∂,  ∑,  ∏,  π,  ∫,  ª,  º,  Ω,  æ,  ø */
               /*   176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191 */
                    177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,

               /*     ¿,  ¡,  ¬,  √,  ƒ,  ≈,  ∆,  «,  »,  …,   ,  À,  Ã,  Õ,  Œ,  œ */
               /*   192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207 */
                    193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,

               /*     –,  —,  “,  ”,  ‘,  ’,  ÷,  ◊,  ÿ,  Ÿ,  ⁄,  €,  ‹,  ›,  ﬁ,  ﬂ */
               /*   208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223 */
                    209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,

               /*     ‡,  ·,  ‚,  „,  ‰,  Â,  Ê,  Á,  Ë,  È,  Í,  Î,  Ï,  Ì,  Ó,  Ô */
               /*   224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239 */
                    225, 83,226,227,228,229,230,231,232,233,234,235,236,237,238,239,

               /*     ,  Ò,  Ú,  Û,  Ù,  ı,  ˆ,  ˜,  ¯,  ˘,  ˙,  ˚,  ¸,  ˝,  ˛,    */
               /*   240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255 */
                    240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,
            } ;
         #endif /* S4ANSI */

         int S4CALL u4memcmp( S4CMP_PARM s1, S4CMP_PARM s2, size_t len )
         {
            int i ;

            for ( i = 0 ; i < len ; i++ )
               if ( ((unsigned char *)s1)[i] != ((unsigned char *)s2)[i] )
               {
                  if ( v4map[((unsigned char *)s1)[i]] < v4map[((unsigned char *)s2)[i]] )
                     return -1 ;
                  return 1 ;
               }

            return 0 ;
         }

      #endif /* S4NORWEGIAN */
   #endif /* S4LANGUAGE  */

   #ifdef S4POLISH_WIN
      #ifndef S4ANSI
         /* This mapping is for Polish. */
         unsigned char v4map[256] =
         {
            /*      ,  ,  ,  ,  ,  ,  ,  ,  ,   ,   ,  ,  ,   ,  ,   */
            /*     0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 */
                   0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,

            /*     ,  ,  ,  ,  ,  ,  ,  ,  ,  ,EOF,  ,  ,  ,  ,   */
            /*    16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 */
                  16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,

            /*      ,  !,  ",  #,  $,  %,  &,  ',  (,  ),  *,  +,  ,,  -,  .,  / */
            /*    32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47 */
                  32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,

            /*     0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  :,  ;,  <,  =,  >,  ? */
            /*    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63 */
                  48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,

            /*     @,  A,  B,  C,  D,  E,  F,  G,  H,  I,  J,  K,  L,  M,  N,  O */
            /*    64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79 */
                  64, 65, 67, 68, 70, 71, 73, 74, 75, 76, 77, 78, 79, 81, 82, 84,

            /*     P,  Q,  R,  S,  T,  U,  V,  W,  X,  Y,  Z,  [,  \,  ],  ^,  _ */
            /*    80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95 */
                  86, 87, 88, 89, 91, 92, 93, 94, 95, 96, 97,100,101,102,103,104,

            /*     `,  a,  b,  c,  d,  e,  f,  g,  h,  i,  j,  k,  l,  m,  n,  o */
            /*    96, 97, 98, 99,100,101,102,103,104,105,106,107,108,109,110,111 */
                 105,106,108,109,111,112,114,115,116,117,118,119,120,122,123,125,

            /*     p,  q,  r,  s,  t,  u,  v,  w,  x,  y,  z,  {,  |,  },  ~,   */
            /*   112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127 */
                 127,128,129,130,132,133,134,135,136,137,138,141,142,143,144,145,

            /*     Ä,  Å,  Ç,  É,  Ñ,  Ö,  Ü,  á,  à,  â,  ä,  ã,  å,  ç,  é,  è */
            /*   128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143 */
                 146,147,148,149,150,151,152,153,154,155,156,157, 90,158,159, 98,

            /*     ê,  ë,  í,  ì,  î,  ï,  ñ,  ó,  ò,  ô,  ö,  õ,  ú,  ù,  û,  ü */
            /*   144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159 */
                 160,161,162,163,164,165,166,167,168,169,170,171,131,172,173,139,

            /*     †,  °,  ¢,  £,  §,  •,  ¶,  ß,  ®,  ©,  ™,  ´,  ¨,  ≠,  Æ,  Ø */
            /*   160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175 */
                 174,175,176, 80,177, 66,178,179,180,181,182,183,184,185,186, 99,

            /*     ∞,  ±,  ≤,  ≥,  ¥,  µ,  ∂,  ∑,  ∏,  π,  ∫,  ª,  º,  Ω,  æ,  ø */
            /*   176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191 */
                 187,188,189,121,190,191,192,193,194,107,195,196,197,198,199,140,

            /*     ¿,  ¡,  ¬,  √,  ƒ,  ≈,  ∆,  «,  »,  …,   ,  À,  Ã,  Õ,  Œ,  œ */
            /*   192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207 */
                 200,201,202,203,204,205, 69,206,207,208, 72,209,210,211,212,213,

            /*     –,  —,  “,  ”,  ‘,  ’,  ÷,  ◊,  ÿ,  Ÿ,  ⁄,  €,  ‹,  ›,  ﬁ,  ﬂ */
            /*   208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223 */
                 214, 83,215, 85,216,217,218,219,220,221,222,223,224,225,226,227,

            /*     ‡,  ·,  ‚,  „,  ‰,  Â,  Ê,  Á,  Ë,  È,  Í,  Î,  Ï,  Ì,  Ó,  Ô */
            /*   224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239 */
                 228,229,230,231,232,233,110,234,235,236,113,237,238,239,240,241,

            /*     ,  Ò,  Ú,  Û,  Ù,  ı,  ˆ,  ˜,  ¯,  ˘,  ˙,  ˚,  ¸,  ˝,  ˛,  ˇ */
            /*   240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255 */
                 242,124,243,126,244,245,246,247,248,249,250,251,252,253,254,255,
         } ;
      #endif /* ifndef S4ANSI */

      int S4CALL u4memcmp( S4CMP_PARM s1, S4CMP_PARM s2, size_t len )
      {
         int i ;

         for (i=0; i<len; i++)
            if ( ((unsigned char *)s1)[i] != ((unsigned char *)s2)[i] )
            {
                    if ( v4map[((unsigned char *)s1)[i]] < v4map[((unsigned char *)s2)[i]] )
                       return -1 ;
                    return 1 ;
            }

         return 0 ;
      }
   #endif  /*   ifdef S4POLISH_WIN  */

   // should never get called because already defined in fox...
   int S4CALL u4keycmp( S4CMP_PARM dataPtr, S4CMP_PARM searchPtr, size_t sLen, size_t dLen, size_t trailCnt, COLLATE4 *collate )
   {
      assert5( 0 ) ;  // just assert out if ever called...
      return 0 ;
   }


   // should never get called because already defined in fox...
   S4EXPORT COLLATE4 * S4FUNCTION collation4getExport( Collate4name collateName )
   {
      assert5( 0 ) ;  // just assert out if ever called...
      return 0 ;  // no collation
   }
#else /* S4FOX */




   #ifndef S4OFF_INDEX
      void t4convertSubSortCompressChar( COLLATE4 *collate, char *result, const char *input, const int lenIn, int *lenOut )
      {
          /* for translations with compressions/expansions, and sub-sorts
              char tailCharacters[maxKeySize/2] ;

             for each 'character' in the unconverted key
             {
                if marked as compression/expansion
                {
                   if expansion
                      translate the head and (tail characters if required) of both expanded characters
                      continue to next input character
                   if compression
                      if next input character matches the required character for compression
                      translate the head and (tail character if required) of compressed key
                      continue to (next +1) input character (i.e. skip the 2nd compressed character)
                   // if here, just a normal character, so fall through... (i.e. non-compressed)
                }

                translate the head and (tail character if required)
                continue to next input character
             }

             copy tailCharacters to end of outputKey

         */

         assert5( collate != 0 && result != 0 && input != 0 && lenIn >= 0 ) ;

         unsigned int len = lenIn ;
         *lenOut = 2 * lenIn ;  // we do a 2 to 1 conversion for this collation...

         Translate4arrayChar *translateArray = (Translate4arrayChar *)collate->charToKeyTranslationArray ;
         assert5( translateArray != 0 ) ;

         unsigned char *tailCharacters ;
         unsigned char tailCharacterBuffer[I4MAX_KEY_SIZE*2] ;  // array used to temporarily store the tail characters

         unsigned short resultHeadIndex = 0 ;  // index into the (head part) of the result array
         unsigned short resultTailIndex = 0 ;  // index into the tail array

         /* AS 07/28/99 --> FoxPro compatibility requires us to remove trailing blanks before
            conversion, otherwise sort sequence comes out incorrect.  (even though ' ' sorts
            after some other characters, it sorts before all others when it is by itself).

            Do this by reducing the input length for any trailing spaces.
         */
         // AS 09/21/99 --> inputIndex unsigned, for negative, must check at ULONG_MAX...
         // CS 2001/04/19 change ULONG_MAX to UINT_MAX
         for ( unsigned int inputIndex = len - 1 ; ( inputIndex != UINT_MAX && input[inputIndex] == ' ' ) ; inputIndex-- )
            len-- ;

         // AS Oct 30/03 - There existed a general collating sequence partial key search
         // problem.  The basic problem was that the tail characters were interfering with the
         // partial match (with partial seeks the tail characters are always ignored)  Therefore,
         // a change was made so that the key conversion would not create tail characters in this case.
         Bool5 doTails = 1 ;
         int verifyLen ;
         if ( collate->lenIn == 0 )
            verifyLen = *lenOut ;
         else
            verifyLen = collate->lenIn*2 ;
         if ( collate->considerPartialSeek == 1 && collate->maxKeyLen > (verifyLen + collate->hasNull) )  // is a partial seek
            doTails = 0 ;

         // AS 04/20/00 in some rare cases (i.e. large ODBC fields), may have large keys to convert, in which case this
         // work-around of allocating/freeing is slow, but works (but if you are performing ops on large fields it is slow anyway)
         Bool5 mustFreeTail = 0 ;
         if ( doTails )
         {
            if ( len < (I4MAX_KEY_SIZE*2) )  // room to use buffer
               tailCharacters = tailCharacterBuffer ;
            else
            {
               tailCharacters = (unsigned char *)u4alloc( len ) ;
               if ( tailCharacters == 0 )
               {
                  error4( 0, e4memory, E84907 ) ;  // have not coded compression yet, not needed yet for any supported collations
                  return ;
               }
               mustFreeTail = 1 ;
            }
         }

         for ( unsigned int charIndex = 0 ; charIndex < len ; charIndex++ )
         {
            unsigned char charToCollate = input[charIndex] ;
            if ( translateArray[charToCollate].headChar == collate->expandOrCompressChar )
            {
               // means we have an expansion or compression, the sub-sort character indicates
               // the element number of the compression array which applies for this character...

               Expansion4compressionArray *compressArray = (Expansion4compressionArray *)collate->charToKeyCompressionArray ;
               unsigned int compressIndex = translateArray[charToCollate].tailChar ;

               if ( compressArray[compressIndex].type == expand4 )
               {
                  result[resultHeadIndex++] = translateArray[compressArray[compressIndex].expComp.expansion.resultingChar1ForExpansionIndex].headChar ;
                  if ( doTails )
                  {
                     unsigned char tailChar = translateArray[compressArray[compressIndex].expComp.expansion.resultingChar1ForExpansionIndex].tailChar ;
                     if ( tailChar != collate->noTailChar )  // means we have a tail character...
                        tailCharacters[resultTailIndex++] = tailChar ;
                  }
                  result[resultHeadIndex++] = translateArray[compressArray[compressIndex].expComp.expansion.resultingChar2ForExpansionIndex].headChar ;
                  if ( doTails )
                  {
                     unsigned char tailChar = translateArray[compressArray[compressIndex].expComp.expansion.resultingChar2ForExpansionIndex].tailChar ;
                     if ( tailChar != collate->noTailChar )  // means we have a tail character...
                        tailCharacters[resultTailIndex++] = tailChar ;
                  }
               }
               else
               {
                  assert5( compressArray[compressIndex].type == compress4 ) ;
                  error4( 0, e4notSupported, E84907 ) ;  // have not coded compression yet, not needed yet for any supported collations
                  if ( mustFreeTail )
                     u4free( tailCharacters ) ;
                  return ;
               }
            }
            else
            {
               result[resultHeadIndex++] = translateArray[charToCollate].headChar ;
               if ( doTails )
               {
                  unsigned char tailChar = translateArray[charToCollate].tailChar ;
                  if ( tailChar != collate->noTailChar )  // means we have a tail character...
                  {
                     // AS 07/06/00 - actual array size given by lenIn, not len, since len does not include trimmed blanks...
                     // assert5( resultTailIndex < len ) ;  // ensure we do not exceed array size
                     // AS 07/06/00 - it can actually happen that we exceed the trail markers - eg. doing a partial seek.
                     // in that case, just ignore the remaining trailing bytres...
                     // assert5( resultTailIndex < lenIn ) ;  // ensure we do not exceed array size
                     if ( resultTailIndex < lenIn )
                        tailCharacters[resultTailIndex++] = tailChar ;
                  }
               }

            }
         }

         if ( doTails )
         {
            // now copy the tail characters in...
            unsigned short maxCopy = *lenOut - resultHeadIndex ;
            unsigned short amountToCopy = ( maxCopy < resultTailIndex ? maxCopy : resultTailIndex ) ;  /* LY 99/08/09 : VC++ 1.52 */
            c4memcpy( result + resultHeadIndex, tailCharacters, amountToCopy ) ;

            // and set the rest of the bytes to NULL
            assert5( amountToCopy <= maxCopy ) ;
            c4memset( result + resultHeadIndex + amountToCopy, 0, maxCopy - amountToCopy ) ;
            if ( mustFreeTail )
               u4free( tailCharacters ) ;
         }
         else
         {
            unsigned short zeroLen = *lenOut - resultHeadIndex ;
            c4memset( result + resultHeadIndex, 0, zeroLen ) ;
         }
      }



      // AS Dec 30/02 - added simple collation support
      void t4convertSimpleChar( COLLATE4 *collate, char *result, const char *input, const int lenIn, int *lenOut )
      {
          /* for translations with compressions/expansions, and sub-sorts
              char tailCharacters[maxKeySize/2] ;

             for each 'character' in the unconverted key
             {
                if marked as compression/expansion
                {
                   if expansion
                      translate the head and (tail characters if required) of both expanded characters
                      continue to next input character
                   if compression
                      if next input character matches the required character for compression
                      translate the head and (tail character if required) of compressed key
                      continue to (next +1) input character (i.e. skip the 2nd compressed character)
                   // if here, just a normal character, so fall through... (i.e. non-compressed)
                }

                translate the head and (tail character if required)
                continue to next input character
             }

             copy tailCharacters to end of outputKey

         */

         assert5( collate != 0 && result != 0 && input != 0 && lenIn >= 0 ) ;

         unsigned int len = lenIn ;
         *lenOut = lenIn ;  // we do a 1 to 1 conversion for this collation...

         Translate4arrayChar *translateArray = (Translate4arrayChar *)collate->charToKeyTranslationArray ;
         assert5( translateArray != 0 ) ;

         unsigned short resultHeadIndex = 0 ;  // index into the (head part) of the result array

         /* AS 07/28/99 --> FoxPro compatibility requires us to remove trailing blanks before
            conversion, otherwise sort sequence comes out incorrect.  (even though ' ' sorts
            after some other characters, it sorts before all others when it is by itself).

            Do this by reducing the input length for any trailing spaces.
         */
         // AS 09/21/99 --> inputIndex unsigned, for negative, must check at ULONG_MAX...
         // CS 2001/04/19 change ULONG_MAX to UINT_MAX
         // AS Mar 10/05 - for simple mappings, do all the characters, even the blank characters...they may have different values too
         // for ( unsigned int inputIndex = len - 1 ; ( inputIndex != UINT_MAX && input[inputIndex] == ' ' ) ; inputIndex-- )
         //    len-- ;

         for ( unsigned int charIndex = 0 ; charIndex < len ; charIndex++ )
         {
            unsigned char charToCollate = input[charIndex] ;
            result[resultHeadIndex++] = translateArray[charToCollate].headChar ;
         }

         // and set the rest of the bytes to NULL
         unsigned short amountToCopy = *lenOut - resultHeadIndex ;

         c4memset( result + resultHeadIndex, 0, amountToCopy ) ;
      }



      static int collate4setupReadCharCollationArray( COLLATE4 *collate, CODE4 *c4, long startRecNo, long endRecNo )
      {
         if ( endRecNo < startRecNo || endRecNo < 0 || startRecNo < 0 )
            return -1 ;

         DATA4 *collation = d4open( c4, COLLATION4TABLE_NAME ) ;
         if ( collation == 0 )
            return -1 ;

         FIELD4 *transCodeField = d4field( collation, "TRANS_CODE" ) ;
         FIELD4 *subSortCodeField = d4field( collation, "SBSORTCODE" ) ;

         if ( error4code( c4 ) < 0 )  // missing field
         {
            d4close( collation ) ;
            return -1 ;
         }

         long numRecs = endRecNo - startRecNo + 1 ;
         collate->charToKeyTranslationArray = (char *)u4alloc( numRecs * ( 1 + collate->keySizeCharPerCharAdd ) ) ;
         if ( collate->charToKeyTranslationArray == 0 )
         {
            d4close( collation ) ;
            return -1 ;
         }
         collate->didAllocChar = 1 ;

         long arrayIndex = 0 ;
         for( long recNo = startRecNo ; recNo <= endRecNo ; recNo++ )
         {
            if ( d4go( collation, recNo ) != 0 )
            {
               d4close( collation ) ;
               return -1 ;
            }
            assert5( arrayIndex < ( numRecs  * ( 1 + collate->keySizeCharPerCharAdd )) ) ;
            unsigned short transCode = f4int( transCodeField ) ;
            if ( transCode > 255 )  // invalid character
            {
               d4close( collation ) ;
               return -1 ;
            }
            ((char *)collate->charToKeyTranslationArray)[arrayIndex++] = (char)transCode ;

            // LY Jan 5/05 : replace direct check of COLLATE4.keySizeCharPerCharAdd with collate4simpleMapping()
            // if ( collate->keySizeCharPerCharAdd == 1 )  // means need sub-sort code too
            if ( !collate4simpleMapping( collate ) )  // means need sub-sort code too
            {
               unsigned short subCode = f4int( subSortCodeField ) ;
               if ( subCode > 255 )  // invalid character
               {
                  d4close( collation ) ;
                  return -1 ;
               }
               ((char *)collate->charToKeyTranslationArray)[arrayIndex++] = (char)subCode ;
            }
         }

         d4close( collation ) ;
         return 0 ;
      }



      static int collate4setupReadCharCompressionArray( COLLATE4 *collate, CODE4 *c4, long startRecNo, long endRecNo )
      {
         if ( endRecNo < startRecNo || endRecNo < 0 || startRecNo < 0 )
            return -1 ;

         DATA4 *compress = d4open( c4, COLLATION4COMPRESS_TABLE_NAME ) ;
         if ( compress == 0 )
            return -1 ;

         FIELD4 *isExpandField = d4field( compress, "IS_EXPAND" ) ;
         FIELD4 *char1field = d4field( compress, "CHAR1" ) ;
         FIELD4 *char2field = d4field( compress, "CHAR2" ) ;
         FIELD4 *subSortModfield = d4field( compress, "SUBSORTMOD" ) ;

         if ( error4code( c4 ) < 0 )  // missing field
         {
            d4close( compress ) ;
            return -1 ;
         }

         long numRecs = endRecNo - startRecNo + 2 ;  // +2 because we need an additional 'done4' entry

         Expansion4compressionArray *compressArray = (Expansion4compressionArray *)u4alloc( numRecs * sizeof( Expansion4compressionArray ) ) ;
         if ( compressArray == 0 )
         {
            d4close( compress ) ;
            return -1 ;
         }
         collate->charToKeyCompressionArray = compressArray ;

         long arrayIndex = 0 ;
         for( long recNo = startRecNo ; recNo <= endRecNo ; recNo++, arrayIndex++ )
         {
            assert5( arrayIndex < numRecs ) ;
            if ( d4go( compress, recNo ) != 0 )
            {
               d4close( compress ) ;
               return -1 ;
            }
            if ( f4true( isExpandField ) )
            {
               compressArray[arrayIndex].type = expand4 ;
               unsigned short char1 = f4int( char1field ) ;
               assert5( char1 < 256 ) ;  // ensure a valid character ;
               unsigned short char2 = f4int( char2field ) ;
               assert5( char2 < 256 ) ;  // ensure a valid character ;
               compressArray[arrayIndex].expComp.expansion.resultingChar1ForExpansionIndex = char1 ;
               compressArray[arrayIndex].expComp.expansion.resultingChar2ForExpansionIndex = char2 ;
            }
            else
            {
               compressArray[arrayIndex].type = compress4 ;
               unsigned short char1 = f4int( char1field ) ;
               assert5( char1 < 256 ) ;  // ensure a valid character ;
               unsigned short char2 = f4int( char2field ) ;
               assert5( char2 < 256 ) ;  // ensure a valid character ;
               compressArray[arrayIndex].expComp.compression.nextCharacterToIndicateCompressionIndex = char1 ;
               compressArray[arrayIndex].expComp.compression.resultingCharForCompressionIndex = char2 ;
            }
         }

         compressArray[arrayIndex].type = done4 ;  // indicate at end of array...

         d4close( compress ) ;
         return 0 ;
      }



      S4EXPORT COLLATE4 * S4FUNCTION collation4getExport( Collate4name collateName )
      {
         // required by OLE-DB to retrieve collating info
         return collation4get( collateName ) ;
      }



      int collate4setupReadFromDisk( CODE4 *c4, Collate4name collateName )
      {
         // reads a collation from disk files...
         /*
             // collateName can be used to obtain an index into the static collationArray as
             // well as the record number of the COLLATION_INFO TABLE
             open COLLATION_INFO TABLE
             go to collate4infoTableIndex( collateName )
             if COLLATION_INFO TABLE.isUnicode == TRUE
             {
                read in collationArray[collate4arrayIndex(collateName)].unicodeToTranslationKeyArray
                read in collationArray[collate4arrayIndex(collateName)].unicodeToKeyCompressionArray
                set collationArray[collate4arrayIndex(collateName)].collateType
                set collationArray[collate4arrayIndex(collateName)].collateState to unicodeConversionLoaded
             }
             else
             {
                read in collationArray[collate4arrayIndex(collateName)].charToTranslationKeyArray
                read in collationArray[collate4arrayIndex(collateName)].charToKeyCompressionArray
                set collationArray[collate4arrayIndex(collateName)].collateType
                set collationArray[collate4arrayIndex(collateName)].collateState to charConversionLoaded
             }
         */

         DATA4 *collationInfo = d4open( c4, COLLATION4INFO_TABLE_NAME ) ;
         if ( collationInfo == 0 )
            return -1 ;

         COLLATE4 *collate = collation4get( collateName ) ;

         long collInfoRecno = collate4infoTableRecno( collateName ) ;

         if ( d4go( collationInfo, collInfoRecno ) != 0 )
         {
            d4close( collationInfo ) ;
            return -1 ;
         }

         FIELD4 *isUnicodeField = d4field( collationInfo, "ISUNICODE" ) ;
         FIELD4 *startCollationRecNoField = d4field( collationInfo, "COLLSTART" ) ;
         FIELD4 *endCollationRecNoField = d4field( collationInfo, "COLLEND" ) ;
         FIELD4 *startCompressRecNoField = d4field( collationInfo, "COMPSTART" ) ;
         FIELD4 *endCompressRecNoField = d4field( collationInfo, "COMPEND" ) ;
         FIELD4 *expansionReqdField = d4field( collationInfo, "EXPANREQD" ) ;
         FIELD4 *compressReqdField = d4field( collationInfo, "COMPREQD" ) ;
         FIELD4 *subSortsReqdField = d4field( collationInfo, "SUBREQD" ) ;
         FIELD4 *compressCharField = d4field( collationInfo, "COMPCHAR" ) ;
         FIELD4 *noTailCharField = d4field( collationInfo, "NOTAILCHAR" ) ;
         FIELD4 *transReqdField = d4field( collationInfo, "TRANSREQD" ) ;
         FIELD4 *lossInfoField = d4field( collationInfo, "LOSSINFO" ) ;

         if ( error4code( c4 ) < 0 )  // i.e. if a field was not found
         {
            d4close( collationInfo ) ;
            return -1 ;
         }

         // determine keysSizeCharPerCharAdd : == + 1 if subsorts or expansion are required.
         if ( f4true( subSortsReqdField ) || f4true( expansionReqdField ) )
            collate->keySizeCharPerCharAdd = 1 ;
         else
            collate->keySizeCharPerCharAdd = 0 ;

         // get the expansion/compression character
         collate->expandOrCompressChar = (unsigned short)f4int( compressCharField ) ;
         collate->expandOrCompressUnicode = collate->expandOrCompressChar ;

         // get the no-tail character
         collate->noTailChar = (unsigned short)f4int( noTailCharField ) ;
         collate->noTailUnicode = collate->noTailChar ;
         collate->lossOfData = collate->lossOfData ;

         if ( f4true( lossInfoField ) == 0 )
            collate->lossOfData = 1 ;
         else
            collate->lossOfData = 0 ;

         // determine the collation type...
         if ( f4true( transReqdField ) == 0 )  // no translation required - must be machine collation
         {
            collate->collateType = collate4machineByteOrder ;
         }
         else
         {
            if ( f4true( subSortsReqdField ) )
            {
               if ( f4true( expansionReqdField ) || f4true( compressReqdField ) )
               {
                  collate->collateType = collate4subSortCompress ;
               }
               else
               {
                  collate->collateType = collate4subSort ;
               }
            }
            else
            {
               if ( f4true( expansionReqdField ) || f4true( compressReqdField ) )
               {
                  collate->collateType = collate4compress ;
               }
               else
               {
                  collate->collateType = collate4simple ;
               }
            }
         }

         int rc = 0 ;

         if ( f4true( isUnicodeField ) )
         {
            // read unicode collation info
            // rc = collate4setupReadUnicode Array() ;
            return error4( c4, e4notSupported, E84907 ) ;  // not coded yet
         }
         else
         {
            // read character collation info
            rc = collate4setupReadCharCollationArray( collate, c4, f4long( startCollationRecNoField ), f4long( endCollationRecNoField ) ) ;
            if ( rc == 0 )
               rc = collate4setupReadCharCompressionArray( collate, c4, f4long( startCompressRecNoField ), f4long( endCompressRecNoField ) ) ;
         }

         d4close( collationInfo ) ;
         return rc ;
      }



      void collate4setupUnicodeFromChar( COLLATE4 *collate )
      {
         // creates unicode arrays based on character arrays for the collation...

         assert5( collate->charToKeyTranslationArray != 0 ) ;
         assert5( collate->unicodeToKeyTranslationArray == 0 ) ;

         // BCR 09/22/00 -- 4 byte alignment on LINUX, maybe other systems as well.
         // LY Aug 16/04 : added S4MACINTOSH
         #if defined( S4DATA_ALIGN ) || defined( S4UNIX ) || defined( S4MACINTOSH ) /* LY 00/08/15 : t4indx3 */
            unsigned char bytesPerChar = 2 + even4up(collate->keySizeCharPerCharAdd) ;
            collate->unicodeToKeyTranslationArray = u4alloc( bytesPerChar * 0x10000 ) ;
         #else
         // # bytes per character is:  2 for unicode, + (2 * keySizeCharPerCharAdd)
         // AS 08/15/00 - was allocaing 4 bytes for a 3 byte array.
            unsigned char bytesPerChar = 2 + collate->keySizeCharPerCharAdd ;
            collate->unicodeToKeyTranslationArray = u4alloc( bytesPerChar * 0x10000 ) ;
         #endif
         if ( collate->unicodeToKeyTranslationArray == 0 )
         {
            error4( 0, e4memory, E94501 ) ;
            return ;
         }

         if ( collate->collateType == collate4subSort || collate->collateType == collate4subSortCompress )
         {
            // the model of the collation depends on the model of the character collation...
            Translate4arrayUnicode *unicodeArray = (Translate4arrayUnicode *)collate->unicodeToKeyTranslationArray ;
            Translate4arrayChar *charArray = (Translate4arrayChar *)collate->charToKeyTranslationArray ;

            // we want the first 256 entries of the array to correspond to the character collation,
            // and all the rest to be a simple 1 to 1 machine mapping.
            unsigned short arrayIndex ;
            for ( arrayIndex = 0 ; arrayIndex < 256 ; arrayIndex++ )
            {
               // AS 07/30/99 --> need to reverse the number to make it be in memory compare order
               unicodeArray[arrayIndex].headChar = charArray[arrayIndex].headChar ;
               #ifndef S4BYTE_SWAP  /* LY 2001/07/27 */
                  unicodeArray[arrayIndex].headChar = x4reverseShort( &unicodeArray[arrayIndex].headChar ) ;
               #endif
               unicodeArray[arrayIndex].tailChar = charArray[arrayIndex].tailChar ;
            }

            for ( arrayIndex = 256 ;; arrayIndex++ )
            {
               // AS 07/30/99 --> need to reverse the number to make it be in memory compare order
               #ifdef S4BYTE_SWAP
                  unicodeArray[arrayIndex].headChar = arrayIndex ;
               #else
                  unicodeArray[arrayIndex].headChar = x4reverseShort( &arrayIndex ) ;
               #endif
               unicodeArray[arrayIndex].tailChar = (unsigned char)collate->noTailChar ;
               if ( arrayIndex == 0xFFFF )  // done, cannot increment or we roll over.
                  break ;
            }
         }
         else
         {
            // model does not include sub-sorts - don't need to copy sub-sorts over.
            error4( 0, e4notSupported, E84907 ) ;  // not coded yet
            return ;
         }

         if ( collate->charToKeyCompressionArray != 0 )
         {
            // count # of elements in compressionArray ;
            unsigned int numCompressEntries = 0 ;

            // we don't need to include the 'done4' entry for unicode, since only used in counting for this code here
            Expansion4compressionArray *charExpansionArray = (Expansion4compressionArray *)collate->charToKeyCompressionArray ;
            while ( charExpansionArray[numCompressEntries].type != done4 )
               numCompressEntries++ ;

            collate->unicodeToKeyCompressionArray = u4alloc( sizeof( Expansion4compressionArray ) * numCompressEntries ) ;
            if ( collate->unicodeToKeyCompressionArray == 0 )
            {
               error4( 0, e4memory, E94501 ) ;
               return ;
            }

            // just need to copy the compression table over...
            c4memcpy( collate->unicodeToKeyCompressionArray, collate->charToKeyCompressionArray, sizeof( Expansion4compressionArray ) * numCompressEntries ) ;
         }

         collate->didAllocUnicode = 1 ;

         #ifdef S4BYTE_SWAP  /* LY 2001/07/27 */
            collate->expandOrCompressUnicode = collate->expandOrCompressChar ;
         #else
            collate->expandOrCompressUnicode = x4reverseShort( &collate->expandOrCompressChar ) ;
         #endif
         collate->noTailUnicode = collate->noTailChar ;  // not reversed, 1 byte only...
         collate->lossOfData = collate->lossOfData ;
      }



      void collate4initUndo()
      {
         // free up all collation allocations...
         for ( int collateIndex = 0 ; collateIndex < NUM4AVAIL_COLLATION_ENTRIES ; collateIndex++ )
         {
            COLLATE4 *collate = &collationArray[collateIndex] ;

            if ( collate->didAllocChar )
            {
               u4free( collate->charToKeyTranslationArray ) ;
               collate->charToKeyTranslationArray = 0 ;
               if ( collate->charToKeyCompressionArray != 0 )
               {
                  u4free( collate->charToKeyCompressionArray ) ;
                  collate->charToKeyCompressionArray = 0 ;
               }
               collate->didAllocChar = 0 ;
            }

            if ( collate->didAllocUnicode )
            {
               u4free( collate->unicodeToKeyTranslationArray ) ;
               collate->unicodeToKeyTranslationArray = 0 ;
               if ( collate->unicodeToKeyCompressionArray != 0 )
               {
                  u4free( collate->unicodeToKeyCompressionArray ) ;
                  collate->unicodeToKeyCompressionArray = 0 ;
               }
               collate->didAllocUnicode = 0 ;
            }
         }
      }


      /*
      #ifdef S4VFP_KEY
         int t4strToVFPKey( char *dest, const char *src, const int src_l, const int max_l, T4VFP *vfp )
         {
            int i, j, head, tail, len ;
            unsigned char index ;
            translatedChars *v4translatedChar = vfp->tablePtr ;
            compressedChars *v4compressedChar = vfp->compPtr ;
            unsigned char *v4codePage = vfp->cpPtr ;

            #ifdef E4PARM_LOW
               if ( max_l < 1 || src_l > max_l )
                  return error4( 0, e4parm, E91643 ) ;
            #endif

            if ( src_l * 2 < max_l )   // partial key: don't need to worry about tail characters
            {
               tail = len = src_l ;

               head = 0 ;
               for ( i = 0 ; (i < len && head < max_l) ; i++ )
               {
                  index = ( (unsigned char)src[i] < 128 ) ? (unsigned char)src[i] : v4codePage[(unsigned char)src[i]-128] ;
                  if ( v4translatedChar[0][index] != EXPAND4CHAR_TO_TWO_BYTES )
                     dest[head++] = v4translatedChar[0][index];
                  else
                  {
                     // translate both characters
                     for ( j = 0 ; ( j < 2 && head < max_l ) ; j++ )
                        dest[head++] = v4translatedChar[0][v4compressedChar[v4translatedChar[1][index]][j]] ;
                  }
               }

               return head ;
            }
            else
            {
               len = src_l ;

               for ( i = len - 1 ; ( i >= 0 && src[i] == ' ' ) ; i-- )   // remove trailing spaces
                  len-- ;

               tail = len ;

               //   Normal key is as follows <head bytes><tail bytes> where the head bytes indicate
               //   the key itself (characters considered identical get sorted as same head value),
               //   and the tail bytes are used to sub-sort on identical head key values.
               //   eg. actual sort order is:
               //      ed
               //      êd
               //      ef
               //      êf
               //   - the 'e' always comes before the 'ê', but outside of themselves they are
               //     considered to be equivalent... (which is why you get a mixture of the
               //     character instead always all 'ê' characters being after all 'e' characters
               //
               //   However, in the case of multi-byte characters, what we do is shift the tail
               //   bytes over to make room for more head bytes due to expansion.  This means that
               //   in some cases some of the tail bytes just get trimmed off.

               for ( i = 0 ; ( i < len && tail < max_l ) ; i++ )
               {
                  index = ( (unsigned char)src[i] < 128 ) ? (unsigned char)src[i] : v4codePage[(unsigned char)src[i]-128] ;
                  if ( v4translatedChar[0][index] == EXPAND4CHAR_TO_TWO_BYTES )
                  {
                     /// a double byte expansion character, make room in the head bytes by shifting
                     //   over the start position of the tail bytes.
                     //
                     tail++ ;
                  }
               }

               head = 0 ;

               // generate the key using the translation tables...
               for ( i = 0 ; ( i < len && head < max_l ) ; i++ )
               {
                  // the implementation is:
                  //   - there is a common sort table used no matter what code page is used.
                  //   - therefore, we need to transform all the various code page values to
                  //     be equivalent to a common code which is then used to look up into the
                  //     common sort page.
                  //     eg.  'a' may be code 65 in machine code page, and code 78 in windows
                  //     code page.  the 'a' in machine code page gets translated to a 78,
                  //     which then can be looked up in the sort sequence.
                  //
                  //     Note that the first 128 characters are common among all code pages, so
                  //     we don't need to do a transformation on them.  It is only the latter 128
                  //     characters which need to be looked up -- thus the code below of whether
                  //     or not value < 128  (if < 128, use the character directly)

                  index = ( (unsigned char)src[i] < 128 ) ? (unsigned char)src[i] : v4codePage[(unsigned char)src[i]-128] ;

                  if ( v4translatedChar[0][index] != EXPAND4CHAR_TO_TWO_BYTES )
                  {
                     // means that we don't have a 2-byte character expansion
                     //
                     // do a direct lookup into the sort table for the head byte of this character
                     //   in the key...
                     //
                     dest[head++] = v4translatedChar[0][index];

                     if ( ( v4translatedChar[1][index] != NO4TAIL_BYTES ) && ( tail < max_l ) )
                     {
                        /// != NO4TAIL_BYTES, means we have a tail character, so insert it and increment
                        //   the tail position counter

                        dest[tail++] = v4translatedChar[1][index];
                     }
                  }
                  else
                  {
                     // we have a 2-byte expansion character, so translate each character in the compression
                     //   array

                     // get the index into the compreseed character array...
                     int compressedArrayIndex = v4translatedChar[1][index] ;
                     for ( j = 0 ; ( j < 2 && head < max_l ) ; j++ )
                     {
                        dest[head++] = v4translatedChar[0][v4compressedChar[compressedArrayIndex][j]] ;

                        // the 2nd array entry (eg. v4general[1] gives the tail byte character if any...
                        int secondArrayEntryForExpandedCharacter = v4translatedChar[1][v4compressedChar[compressedArrayIndex][j]] ;
                        if ( ( secondArrayEntryForExpandedCharacter != NO4TAIL_BYTES ) && ( tail < max_l ) )
                        {
                           // != NO4TAIL_BYTES, means we have a tail character, so insert it and increment
                           //   the tail position counter

                           dest[tail++] = secondArrayEntryForExpandedCharacter ;
                        }
                     }
                  }
               }

               // pad out the remaining (should be same as tag's pChar)
               memset( (void *)&dest[tail], 0, max_l-tail ) ;
               return max_l ;
            }
         }
      #endif // S4VFP_KEY
      */

      int u4keycmpPartial( S4CMP_PARM dataPtr, S4CMP_PARM searchPtr, size_t sLen, size_t dLen, size_t trailCnt, COLLATE4 *collate, const char *initialValue, int initialLen )
      {
         // used as a more general key coparison.  In particular it resolves the following partial key problem with general
         // collating sequences:  spaces are trimmed off the search expression in general otherwise the value may not be
         // found even if it is there due to how spaces are handled in general collating sequence.  Therefore, what we do
         // here is we have additional paramaters - original search string, original length.  If it is indeed a partial seek
         // and there are trailing blanks, we slightly modify the seek to take care of this instance.
         unsigned char *data = (unsigned char *)dataPtr ;
         unsigned char *search = (unsigned char *)searchPtr ;
         unsigned sIndex ;
         int skippedFlag = 0 ;

         // if ( vfp->sortType == sort4machine )   /* machine doesn't need to handle translated keys */
         if ( collate->collateType == collate4machineByteOrder )
            return c4memcmp( dataPtr, searchPtr, sLen ) ;

         /* no special double characters at this point so don't worry about them */
         unsigned dIndex = 0 ;
         for ( sIndex = 0 ; sIndex < sLen && search[sIndex] >= 10 && dIndex < dLen ; sIndex++ )   /* compare the heads */
         {
            if ( data[dIndex] != search[sIndex] )
            {
               /* AS 07/26/99

                  the ' ' character gets sorted at the beginning of the sequence.
                  however, the '\t' character, which gets converted to the same code,
                  gets put between 0x16 and 0x18 in the sequence.  This code piece was
                  incorrectly thinking that the tab character was a space character, and
                  thus producing a d4check() failure on a 1-byte key with all sequences
                  (t4indx3.c).

                  should not be an issue becuase the blank actually gets collated as a
                  '0' when it is a trailing byte, and it thus compares to the correct value...
                  therefore just skip the check...

                  if ( search[sIndex] == 17 )
                  {
                     if ( data[dIndex] >= 10 )
                        return 1 ;
                     continue ;
                  }
               */
               if ( data[dIndex] > search[sIndex] )
                  return 1 ;
               return -1 ;
            }
            dIndex++ ;
         }

         // AS 05/02/00 here is where to consider partial seek (we don't consider tails in that case)
         if ( initialValue != 0 )  // this is at least potential partial seek
         {
            // now, if the key was fully expanded (i.e. no blanks), the final length will take
            // this into account.
            if ( sLen < dLen )  // currently only support partial seek in this order
            {
               unsigned int fullExpansionLen = initialLen + initialLen * collate->keySizeCharPerCharAdd ;
               if ( fullExpansionLen > sLen )  // is a partial seek
               {
                  // effectively need to consider 2 things here.  first case is that the key we
                  // found has only trailing blanks remaining.  In that case, the head bytes will
                  // be just '0'.  The other possibility is that the values contained are blank
                  // (i.e. 17)
                  int numBlanksInPartialKey = 0 ;
                  int startPos = initialLen - 1 ;
                  for ( ; startPos >= 0 ; startPos-- )
                  {
                     if ( initialValue[startPos] != ' ' )
                        break ;
                     numBlanksInPartialKey++ ;
                  }
                  for ( ; numBlanksInPartialKey > 0 ; numBlanksInPartialKey -- )
                  {
                     if ( data[dIndex] != 17 && data[dIndex] != 0 )  // for sure not blank, so not found, our full key is 'greater'
                        return 1 ;
                     dIndex++ ;
                  }
                  // if here, we must have a match because all were null or blank...
                  return 0 ;
               }
            }
         }

         if ( dIndex < dLen && data[dIndex] >= 10 )   /* remember we skipped some characters */
              skippedFlag = 1 ;

         while ( dIndex < dLen && data[dIndex] >= 10 )   /* advance data to it's tail */
         {
            dIndex++ ;
         }

         for ( ; sIndex < sLen && search[sIndex] < 10 ; sIndex++ )   /* compare the tails */
         {
            if ( dIndex < dLen )
            {
               if ( data[dIndex] != search[sIndex] )
               {
                  if ( skippedFlag || data[dIndex] > search[sIndex] )
                     return 1 ;
                  return -1 ;
               }
            }
            else
            {
               if ( trailCnt <= 0 || search[sIndex] != 0 )
               {
                  if ( !skippedFlag )
                     return -1 ;
                  return 1 ;
               }
               trailCnt-- ;
            }
            dIndex++ ;
         }

         return 0 ;
      }



      /* AS 07/27/99 -> support for generic collating for Unicode and character fields... */
      // int S4CALL u4keycmp( S4CMP_PARM dataPtr, S4CMP_PARM searchPtr, size_t sLen, size_t dLen, size_t trailCnt, T4VFP *vfp )
      int S4CALL u4keycmp( S4CMP_PARM dataPtr, S4CMP_PARM searchPtr, size_t sLen, size_t dLen, size_t trailCnt, COLLATE4 *collate )
      {
         return u4keycmpPartial( dataPtr, searchPtr, sLen, dLen, trailCnt, collate, 0, 0 ) ;
      }

      /* Visual FoxPro uses collating sequences and codepages to support International Languages */

      // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
      // #ifdef out permanently...
      #ifdef S4OLD_GENERAL
      #ifdef S4GENERAL
         /* v4general - supports the general collating sequence */

         /* AS 06/29/99 --> updated based on apparent discrepencies between Fox and CodeBase
            1.  0x00 should get collated as 0x10
            2.  139 should be collated as 0x18 or 24
            3.  247 should be collated as 85, not 125
            4.  255 should be collated as 125, not 118

         */

         /*
            if ( v4general[0][code] == EXPAND4CHAR_TO_TWO_BYTES, it means that the character needs to be
               expanded to a 2-byte code.  If that is the case, then
               v4general[1][code] gives the index into the compressed character array
               of what the 2 character sequence should be.
            in all other cases
               v4general[0][code] gives the head byte of the key and
               v4general[1][code] gives the trail byte of the key
            if v4general[1][code] == NO4TAIL_BYTES, this means that there is no tail byte for the
               given character, so it is no byte is inserted into the tail bytes, and
               the tail byte incrementor does not get incremented (all the tail bytes
               are basically compressed onto the left hand side of the start of the
               tail bytes)
         */

         translatedChars v4general[2] =
         {
            {
               /*     ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤ */
               /*     0,   1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 */
                      16, 16, 16, 16, 16, 16, 16, 16, 16, 17, 16, 16, 16, 16, 16, 16,

               /*     ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤ */
               /*    16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 */
                     16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,

               /*         !   "   #   $   %   &   '   (   )   *   +   ,   -   .   / */
               /*    32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47 */
                     17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,

               /*     0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ? */
               /*    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63 */
                     86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 33, 34, 35, 36, 37, 38,

               /*     @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O */
               /*    64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79 */
                     39, 96, 97, 98,100,102,103,104,105,106,107,108,109,111,112,114,

               /*     P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _ */
               /*    80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95 */
                    115,116,117,118,119,120,122,123,124,125,126, 40, 41, 42, 43, 44,

               /*     `   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o */
               /*    96, 97, 98, 99,100,101,102,103,104,105,106,107,108,109,110,111 */
                     45, 96, 97, 98,100,102,103,104,105,106,107,108,109,111,112,114,

               /*     p   q   r   s   t   u   v   w   x   y   z   {   |   }       ≤ */
               /*   112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127 */
                    115,116,117,118,119,120,122,123,124,125,126, 46, 47, 48, 49, 16,

               /*     ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤                        ≤   ≤   ≤   ≤ */
               /*   128,129,130,131,132,133,134,135,136,137,138,139,                     140,141,142,143 */
                     16, 16, 24, 50, 19, 51, 52, 53, 54, 55,118, 24,EXPAND4CHAR_TO_TWO_BYTES, 16, 16, 16,

               /*     ≤   `   '   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤   ≤                         ≤   ≤   ≤   ≤ */
               /*   144,145,146,147,148,149,150,151,152,153,154,155,                      156,157,158,159 */
                     16, 24, 24, 19, 19, 56, 30, 30, 57, 58,118, 24,EXPAND4CHAR_TO_TWO_BYTES, 16, 16,125,

               /*         ≠   õ   ú XXX  ù    |      " XXX   ¶   Æ   ™   - XXX XXX */
               /*   160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175 */
                     32, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 19, 69, 30, 70, 71,

               /*     ¯   Ò   ˝ XXX   '   Ê    XXX   , XXX   ß   Ø   ¨   ´ XXX   ® */
               /*   176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191 */
                     72, 73, 88, 89, 74, 75, 76, 77, 78, 87, 79, 19, 80, 81, 82, 83,

               /*     A   A   A   A   é   è                        í   Ä   E   ê   E   E   I   I   I   I */
               /*   192,193,194,195,196,197,                     198,199,200,201,202,203,204,205,206,207 */
                     96, 96, 96, 96, 96, 96,EXPAND4CHAR_TO_TWO_BYTES, 98,102,102,102,102,106,106,106,106,

               /*     D   •   O   O   O   O   ô   X   0   U   U   U   ö   Y                        b                        · */
               /*   208,209,210,211,212,213,214,215,216,217,218,219,220,221,                     222,                     223 */
                    101,112,114,114,114,114,114, 84,129,120,120,120,120,125,EXPAND4CHAR_TO_TWO_BYTES,EXPAND4CHAR_TO_TWO_BYTES,

               /*     Ö   †   É   a   Ñ   Ü                        ë   á   ä   Ç   à   â   ç   °   å   ã */
               /*   224,225,226,227,228,229,                     230,231,232,233,234,235,236,237,238,239 */
                     96, 96, 96, 96, 96, 96,EXPAND4CHAR_TO_TWO_BYTES, 98,102,102,102,102,106,106,106,106,

               /*     Â   §   ï   ¢   ì   o   î   ˆ   0   ó   £   ñ   Å   y                       b   ò */
               /*   240,241,242,243,244,245,246,247,248,249,250,251,252,253,                    254,255 */
                    101,112,114,114,114,114,114,85,129,120,120,120,120,125,EXPAND4CHAR_TO_TWO_BYTES,125,
             },
             {
               /*               0,            1,            2,            3,            4,            5, */
                    NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,

               /*               6,            7,            8,            9,           10,           11, */
                    NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,

               /*              12,           13,           14,           15 */
                    NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,

               /*              16,           17,           18,           19,           20,           21, */
                    NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,

               /*              22,           23,           24,           25,           26,           27, */
                    NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,

               /*              28,           29,           30,           31 */
                    NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,

               /*              32,           33,           34,           35,           36,           37, */
                    NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,

               /*              38,           39,           40,           41,           42,           43,           44,           45,           46,           47 */
                    NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,

               /*              48,           49,           50,           51,           52,           53, */
                    NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,

               /*              54,           55,           56,           57,           58,           59,           60,           61,           62,           63 */
                    NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,

               /*              64, 65,           66, 67,           68, 69,           70,           71, */
                    NO4TAIL_BYTES,  0,NO4TAIL_BYTES,  0,NO4TAIL_BYTES,  0,NO4TAIL_BYTES,NO4TAIL_BYTES,

               /*              72, 73,           74,           75,           76,           77, 78, 79 */
                    NO4TAIL_BYTES,  0,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,  0,  0,

               /*              80,           81,           82, 83,           84, 85,           86,           87, */
                    NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,  0,NO4TAIL_BYTES,  0,NO4TAIL_BYTES,NO4TAIL_BYTES,

               /*              88, 89,           90,           91,           92,           93,           94,           95 */
                    NO4TAIL_BYTES,  0,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,

               /*              96, 97,           98, 99,          100,101,          102,          103,          104, */
                    NO4TAIL_BYTES,  0,NO4TAIL_BYTES,  0,NO4TAIL_BYTES,  0,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,

               /*   105,          106,          107,          108,          109,110,111 */
                      0,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,  0,  0,

               /*             112,          113,          114,115,          116,117,          118, */
                    NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,  0,NO4TAIL_BYTES,  0,NO4TAIL_BYTES,

               /*             119,          120,121,          122,          123,          124,          125,          126,          127 */
                    NO4TAIL_BYTES,NO4TAIL_BYTES,  0,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,

               /*             128,          129,          130,          131,          132,          133, */
                    NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,

               /*             134,          135,          136,          137,138,          139,140,          141,          142,          143 */
                    NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,  8,NO4TAIL_BYTES,  0,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,

               /*             144,          145,          146,          147,          148,          149, */
                    NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,

               /*             150,          151,          152,          153,154,          155,156,          157,          158,159 */
                    NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,  8,NO4TAIL_BYTES,  0,NO4TAIL_BYTES,NO4TAIL_BYTES,  4,

               /*   160,          161,          162,          163,          164,          165,          166, */
                      1,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,

               /*             167,          168,          169,          170,          171,          172,          173,          174,          175 */
                    NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,

               /*             176,          177,          178,          179,          180,          181, */
                    NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,

               /*             182,          183,          184,          185,          186,          187,          188,          189,          190,          191 */
                    NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,NO4TAIL_BYTES,

               /*   192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207 */
                      2,  1,  3,  5,  4,  6,  1,  7,  2,  1,  3,  4,  2,  1,  3,  4,

               /*             208,209,210,211,212,213,214,          215,          216,217,218,219,220,221,222,223 */
                    NO4TAIL_BYTES,  5,  2,  1,  3,  5,  4,NO4TAIL_BYTES,NO4TAIL_BYTES,  2,  1,  3,  4,  1,  2,  3,

               /*   224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239 */
                      2,  1,  3,  5,  4,  6,  1,  7,  2,  1,  3,  4,  2,  1,  3,  4,

               /*             240,241,242,243,244,245,246,          247,          248,249,250,251,252,253,254,255 */
                    NO4TAIL_BYTES,  5,  2,  1,  3,  5,  4,NO4TAIL_BYTES,NO4TAIL_BYTES,  2,  1,  3,  4,  1,  2,  4,
             }
          } ;

         compressedChars v4generalComp[4] =
         {
            { 79, 69 },   /* "oe" */
            { 65, 69 },   /* "ae" */
            { 84, 72 },   /* "th" */
            { 83, 83 },   /* "ss" */
         } ;
      #endif /* S4GENERAL */

      /* 1252 - Supports The WINDOWS ANSI CodePage */
      /* Note: all collating sequence translation tables use CodePage 1252 directly;
               therefore, this CodePage table has a one-to-one mapping */
      /* i.e. 128 is tranformed to 128 (no change) for all chars... */

      /* Note: this is the default CodePage if no CodePage is selected (i.e., cp0 ) */

      unsigned char CodePage_1252[128] =
      {
         /*   128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143 */
              128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,

         /*   144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159 */
              144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,

         /*   160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175 */
              160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,

         /*   176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191 */
              176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,

         /*   192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207 */
              192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,

         /*   208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223 */
              208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,

         /*   224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239 */
              224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,

         /*   240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255 */
              240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,
      } ;

      #ifdef S4CODEPAGE_437
         /* 437 - Supports The U.S. MS-DOS CodePage */

         /* transforms the upper value to the lower value before placing into index.
            thus if datafile has a '128', a '199' gets placed into the index key
            and the general sort sequence is done after this tranformation
         */
         unsigned char CodePage_437[] =
         {
            /*   128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143 */
                 199,252,233,226,228,224,229,231,234,235,232,239,238,236,196,197,

            /*   144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159 */
                 201,230,198,244,246,242,251,249,255,214,220,  1,  1,  1,  1,  1,

            /*   160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175 */
                 225,237,243,250,241,209,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,

            /*   176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191 */
                   1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,

            /*   192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207 */
                   1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,

            /*   208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223 */
                   1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,

            /*   224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239 */
                   1,223,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,

            /*   240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255 */
                   1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
         } ;
      #endif  /* S4CODEPAGE_437 */
      #endif /* S4OLD_GENERAL */
   #endif /* !S4OFF_INDEX */
#endif  /* S4FOX */
