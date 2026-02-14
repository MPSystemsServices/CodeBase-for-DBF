/* c4util.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

/* contains security utility routines */

#include "d4all.h"

#ifdef __TURBOC__
   #pragma hdrstop
#endif  /* __TUROBC__ */

#ifdef S4CLIENT
   void code4serverSetSlowDelay( CODE4 *c4, long delayInHundredths )
   {
      CONNECT4 *connect = &c4->clientConnect ;

      connect4sendShort( connect, STREAM4SET_SLOW_DELAY ) ;
      connect4sendLong( connect, delayInHundredths ) ;
   }



   DATA4 *S4FUNCTION code4directory( CODE4 *c4, char *directory )
   {
      short rc ;
      char *name ;
      DATA4 *data ;
      int oldAccessMode ;
      CONNECT4 *connect = &c4->clientConnect ;

      connect4sendShort( connect, STREAM4DIRECTORY ) ;
      connect4sendString( connect, directory ) ;
      connect4sendFlush( connect ) ;

      rc = connect4receiveShort( connect ) ;
      if ( rc < 0 )
      {
         error4( c4, rc, E96701 ) ;
         return NULL ;
      }
      name = connect4receiveString( connect ) ;

      oldAccessMode = c4->accessMode ;
      c4->accessMode = OPEN4DENY_RW ;
      c4->openForCreate = 2 ;
      data = d4open( c4, name ) ;
      c4->openForCreate = 0 ;
      c4->accessMode = oldAccessMode ;

      u4free( name ) ;

      return data ;
   }
#endif



int S4FUNCTION code4swapLogFile( CODE4 *c4, char *logName, int logNameLen )
{
   #ifdef S4CLIENT
      // used by backup/auto-recovery to swap (on the fly) the backup log file being used by the server.
      // this allows for the backup program on the backup computer to update its copies of the backup files
      // so that if it needs to be started less recovery work needs to be done.
      // inputs are 'logName, a character array to place the swapped out log file (i.e. the old one which should
      // be used for backup purposes).  len is the length of the name, and must be at least LEN4PATH

      assert5( logNameLen >= LEN4PATH ) ;  // ensure there is room for the name
      CONNECT4 *connect = &c4->clientConnect ;

      connect4sendShort( connect, STREAM4SWAP_LOGFILE ) ;
      connect4sendFlush( connect ) ;

      short rc = connect4receiveShort( connect ) ;
      if ( rc != 0 && rc != e4len )  // on 'e4len' we give back the name of the log file that caused the problem.
      {
         // handle timeout case
         if ( c4->errorCode == r4timeout )
            return r4timeout ;
         // AS June 01/01 a STREAM4DISCONNECT may come down the line if the server is shut down
         if ( rc == STREAM4DISCONNECT )
         {
            c4->defaultServer.connectionFailure = e4connection ;
            return error4( c4, e4connection, E90160 ) ;
         }
         return rc ;
      }

      short len = connect4receiveShort( connect ) ;
      assert5( len < LEN4PATH ) ;  // should not be larger..., need 1 byte to null-terminate
      connect4receive( connect, logName, len, code4timeoutVal( c4 ) ) ;
      logName[len] = 0 ;

      return rc ;
   #else
      return e4notSupported;
   #endif
}



#ifdef S4CLIENT
   char * S4FUNCTION code4serverCurrentDirectory( CODE4 *c4 )
   {
      short len ;
      char *name ;

      connect4sendShort( &c4->clientConnect, STREAM4CURRENT_DIRECTORY ) ;
      connect4sendFlush( &c4->clientConnect ) ;

      len = connect4receiveShort( &c4->clientConnect ) ;
      name = (char *)u4allocFree( c4, len + 1 ) ;  // extra byte for trailing null
      connect4receive( &c4->clientConnect, name, len, code4timeoutVal( c4 ) ) ;
      name[len] = 0 ;

      return name ;
   }



   int S4FUNCTION code4serverCloseFiles( CODE4 *c4 )
   {
      CONNECTION4 *connection = &c4->defaultServer ;
      if ( connection == 0 )
         return error4( c4, e4connection, E81102 ) ;
      connection4assign( connection, CON4CLOSE_FILES, 0L, 0L ) ;
      connection4sendMessage( connection ) ;
      int rc = connection4receiveMessage( connection ) ;
      if ( rc < 0 )
         return error4stack( c4, rc, E96701 ) ;

      rc = connection4status( connection ) ;
      if ( rc < 0 )
         return connection4error( connection, c4, rc, E96701 ) ;

      return rc ;
   }



   int S4FUNCTION code4serverRestart( CODE4 *c4 )
   {
      CONNECTION4 *connection = &c4->defaultServer ;
      if ( connection == 0 )
         return error4( c4, e4connection, E81102 ) ;
      connection4assign( connection, CON4RESTART, 0L, 0L ) ;
      connection4sendMessage( connection ) ;
      int rc = connection4receiveMessage( connection ) ;
      if ( rc < 0 )
         return error4stack( c4, rc, E96701 ) ;

      rc = connection4status( connection ) ;
      if ( rc < 0 )
         return connection4error( connection, c4, rc, E96701 ) ;

      return rc ;
   }



   int S4FUNCTION code4serverCrash( CODE4 *c4 )
   {
      /* for testing only, causes a server crash */
      CONNECTION4 *connection = &c4->defaultServer ;
      if ( connection == 0 )
         return error4( c4, e4connection, E81102 ) ;
      connection4assign( connection, CON4CRASH, 0L, 0L ) ;
      connection4sendMessage( connection ) ;
      int rc = connection4receiveMessage( connection ) ;
      if ( rc < 0 )
         return error4stack( c4, rc, E96701 ) ;

      rc = connection4status( connection ) ;
      if ( rc < 0 )
         return connection4error( connection, c4, rc, E96701 ) ;

      return rc ;
   }



   int S4FUNCTION code4connectAcceptNew( CODE4 *c4, short settingIn )
   {
      /* input needs to be short for visual basic compatability */
      unsigned char setting = (settingIn == 0 ) ? 0 : 1 ;

      CONNECTION4 *connection = &c4->defaultServer ;
      if ( connection == 0 )
         return error4( c4, e4connection, E81102 ) ;
      connection4assign( connection, CON4CONNECT_ACCEPT_NEW, 0L, 0L ) ;
      connection4addData( connection, &setting, sizeof( unsigned char ), 0 ) ;
      connection4sendMessage( connection ) ;
      int rc = connection4receiveMessage( connection ) ;
      if ( rc < 0 )
         return error4stack( c4, rc, E96701 ) ;

      rc = connection4status( connection ) ;
      if ( rc < 0 )
         return connection4error( connection, c4, rc, E96701 ) ;

      return rc ;
   }



   int S4FUNCTION code4connectCutAll( CODE4 *c4 )
   {
      CONNECTION4 *connection = &c4->defaultServer ;
      if ( connection == 0 )
         return error4( c4, e4connection, E81102 ) ;
      connection4assign( connection, CON4CONNECT_CUT_ALL, 0L, 0L ) ;
      connection4sendMessage( connection ) ;
      int rc = connection4receiveMessage( connection ) ;
      if ( rc < 0 )
         return error4stack( c4, rc, E96701 ) ;

      rc = connection4status( connection ) ;
      if ( rc < 0 )
         return connection4error( connection, c4, rc, E96701 ) ;

      return rc ;
   }



   int S4FUNCTION code4connectCut( CODE4 *c4, const char *connId )
   {
      // AS 12/06/99 - changed function, takes a character connId not a long, now the
      // value input is a character string up to 20 characters in length...
      // assume the input is a pointer to a 20 character buffer.  Does not need to be
      // null terminated...

      CONNECTION4 *connection = &c4->defaultServer ;
      if ( connection == 0 )
         return error4( c4, e4connection, E81102 ) ;
      connection4assign( connection, CON4CONNECT_CUT, 0L, 0L ) ;

      connection4addData( connection, connId, 20, 0 ) ;
      // and null terminate the string...
      char nullChar = 0 ;
      connection4addData( connection, &nullChar, 1, 0 ) ;
      connection4sendMessage( connection ) ;
      int rc = connection4receiveMessage( connection ) ;
      if ( rc < 0 )
         return error4stack( c4, rc, E96701 ) ;

      rc = connection4status( connection ) ;
      if ( rc < 0 )
         return connection4error( connection, c4, rc, E96701 ) ;

      return rc ;
   }



   int S4FUNCTION code4serverShutdown( CODE4 *c4 )
   {
      code4close( c4 ) ;  // this helps code4initUndo() later to not have outstanding data4's.

      CONNECTION4 *connection = &c4->defaultServer ;
      if ( connection == 0 )
         return error4( c4, e4connection, E81102 ) ;
      connection4assign( connection, CON4SHUTDOWN, 0L, 0L ) ;
      connection4sendMessage( connection ) ;

      /* Ignore any connection die messages, since that is ok (it just means
         that connection went down before we got our message back).  Note
         that we must wait for a reply though, to ensure the message got through. */

      c4->errOff = 1 ;
      int rc = connection4receiveMessage( connection ) ;
      if ( rc < 0 )
      {
         error4set( c4, 0 ) ;
         return rc ;
      }

      rc = connection4status( connection ) ;
      if ( rc < 0 )
      {
         error4set( c4, 0 ) ;
         return rc ;
      }
      connection4initUndo( connection ) ;
      c4->defaultServer.connected = 0 ;

      return 0 ;
   }



   char * S4FUNCTION code4tables( CODE4 *c4, const char *path )
   {
      /*
         DESCRIPTION

         Returns a char pointer containing a name of table for all available tables
           in the input path directory.

         RETURNS

         The returned name must be freed by the caller using u4free()

         NOTES

         For simplicity, return '.dbf' extension files.

         This functionality is used by the schema tables as well (for OLE-DB)
      */

      if ( error4code( c4 ) < 0 )
         return 0 ;

      connect4sendShort( &c4->clientConnect, STREAM4TABLES ) ;
      short len = strlen( path ) ;
      connect4sendShort( &c4->clientConnect, len ) ;
      connect4send( &c4->clientConnect, path, len ) ;
      connect4sendFlush( &c4->clientConnect ) ;

      len = connect4receiveShort( &c4->clientConnect ) ;
      char *name = (char *)u4allocFree( c4, len+1 ) ;
      connect4receive( &c4->clientConnect, name, len, code4timeoutVal( c4 ) ) ;
      name[len] = 0 ;

      return name ;
   }



   DATA4 *S4FUNCTION code4connectionStatus( CODE4 *c4 )
   {
      if ( error4code( c4 ) < 0 )
         return 0 ;

      connect4sendShort( &c4->clientConnect, STREAM4STATUS ) ;
      connect4sendFlush( &c4->clientConnect ) ;

      short rc = connect4receiveShort( &c4->clientConnect ) ;
      if ( rc < 0 )
      {
         error4( c4, rc, E96701 ) ;
         return NULL ;
      }

      short len = connect4receiveShort( &c4->clientConnect ) ;
      /* retrieves the name for d4open() */
      char *name = (char *)u4alloc( len + 1 ) ;

      connect4receive( &c4->clientConnect, name, len, code4timeoutVal( c4 ) ) ;
      name[len] = 0 ;

      int oldAccessMode = c4->accessMode ;
      c4->accessMode = OPEN4DENY_RW ;
      c4->openForCreate = 1 ;
      DATA4 *data = d4open( c4, name ) ;
      c4->openForCreate = 0 ;
      c4->accessMode = oldAccessMode ;

      u4free( name ) ;

      return data ;
   }
#endif /* S4CLIENT */



#ifdef S4SERVER
   DATA4 *S4FUNCTION code4connectionStatus( CODE4 *c4 )
   {
      FIELD4INFO statusFields[] =
      {
         { "ACCOUNT_ID", r4str, LEN4ACCOUNT_ID, 0 },
         { "WINLOG_ID", r4str, LEN4ACCOUNT_ID, 0 },   // for ODBC clients, windows login id only available
         { "TCPADDRESS", r4str, 15, 0 },
         { "CONNECT_ID", r4num, 20, 0 },  // AS 12/06/99 -- account id must be 20 to handle odbc clients...
         { "PATH",       r4str, LEN4PATH, 0 },
         { "TABLE",      r4str, LEN4TABLE_NAME, 0 },
         { "FILETYPE",   r4str, 1, 0 },   /* TYPE4PERMANENT, TYPE4TEMP, TYPE4SCHEMA */
         { "RECCOUNT",   r4num, 10, 0 },
         { "NUMFIELDS",  r4num, 4, 0 },
         { "REC_WIDTH",  r4num, 5, 0 },
         { "READ_ONLY",  r4log, 1, 0 },
         { "ACCESSMODE", r4num, 2, 0 },
         { "LOCKTYPE",   r4num, 2, 0 },   /* 0 means no locks for this client on this file */
         { "LOCKRECNO",  r4num, 10, 0 },  /* if locktype is LOCK4RECORD */
         { 0,0,0,0 },
      } ;

      TAG4INFO statusTags[] =
      {
            /* connections (account+address+connection id) */
         { "CONNECT",  "IIF(WINLOG_ID = '   ',UPPER(ACCOUNT_ID),UPPER(WINLOG_ID)) + TCPADDRESS + ASCEND(CONNECT_ID)", "ACCOUNT_ID <> '   ' .OR. WINLOG_ID <> '   '", r4uniqueContinue, 0 },
            /* connections with tables (account+address+connection id+path+table) */
         { "CONNECTT", "IIF(WINLOG_ID = '   ',UPPER(ACCOUNT_ID),UPPER(WINLOG_ID)) + TCPADDRESS + ASCEND(CONNECT_ID) + TRIM(LEFT(PATH,140)) + TABLE", "ACCOUNT_ID <> '   ' .OR. WINLOG_ID <> '   '", r4uniqueContinue, 0 },
            /* open tables (path+table) */
         { "TABLETAG", "TRIM(LEFT(PATH,200)) + TABLE", "LEFT(ACCOUNT_ID,1) <> ' ' .AND. LEFT(TABLE,1) <> ' '", r4uniqueContinue, 0 },
            /* open tables and associated connections (path+table+account+address+connection id) */
         { "TABLE_C",  "TRIM(LEFT(PATH,125)) + TABLE + IIF(WINLOG_ID = '   ',UPPER(ACCOUNT_ID),UPPER(WINLOG_ID)) + TCPADDRESS + ASCEND(CONNECT_ID)", "ACCOUNT_ID <> '   ' .OR. WINLOG_ID <> '   '", r4uniqueContinue, 0 },
            /* lock information: table, connection info, lock info */
         { "LOCK_TCL", "TRIM(LEFT(PATH,125)) + TABLE + IIF(WINLOG_ID = '   ',UPPER(ACCOUNT_ID),UPPER(WINLOG_ID)) + TCPADDRESS + ASCEND(CONNECT_ID) + ASCEND(LOCKTYPE) + ASCEND(LOCKRECNO)",
               "LEFT(TABLE,1) <> ' ' .AND. LOCKTYPE <> 0 .AND. (ACCOUNT_ID <> '   ' .OR. WINLOG_ID <> '   ')",  0, 0 },
            /* table, lock info, connection info */
         { "LOCK_TLC", "TRIM(LEFT(PATH,125)) + TABLE + ASCEND(LOCKTYPE) + ASCEND(LOCKRECNO) + IIF(WINLOG_ID = '   ',UPPER(ACCOUNT_ID),UPPER(WINLOG_ID)) + TCPADDRESS + ASCEND(CONNECT_ID)",
               "LEFT(TABLE,1) <> ' ' .AND. LOCKTYPE <> 0 .AND. (ACCOUNT_ID <> '   ' .OR. WINLOG_ID <> '   ')",  0, 0 },
            /* connection info, table, lock info */
         { "LOCK_CTL", "IIF(WINLOG_ID = '   ',UPPER(ACCOUNT_ID),UPPER(WINLOG_ID)) + TCPADDRESS + ASCEND(CONNECT_ID) + TRIM(LEFT(PATH,125)) + TABLE + ASCEND(LOCKTYPE) + ASCEND(LOCKRECNO)",
               "LEFT(TABLE,1) <> ' ' .AND. LOCKTYPE <> 0 .AND. (ACCOUNT_ID <> '   ' .OR. WINLOG_ID <> '   ')",  0, 0 },
            /* connection info, lock info, table */
         { "LOCK_CLT", "IIF(WINLOG_ID = '   ',UPPER(ACCOUNT_ID),UPPER(WINLOG_ID)) + TCPADDRESS + ASCEND(CONNECT_ID) + ASCEND(LOCKTYPE) + ASCEND(LOCKRECNO) + TRIM(LEFT(PATH,125)) + TABLE",
               "LEFT(TABLE,1) <> ' ' .AND. LOCKTYPE <> 0 .AND. (ACCOUNT_ID <> '   ' .OR. WINLOG_ID <> '   ')",  0, 0 },
            /* lock info, table, connection info */
         { "LOCK_LTC", "ASCEND(LOCKTYPE) + ASCEND(LOCKRECNO) + TRIM(LEFT(PATH,125)) + TABLE + IIF(WINLOG_ID = '   ',UPPER(ACCOUNT_ID),UPPER(WINLOG_ID)) + TCPADDRESS + ASCEND(CONNECT_ID)",
               "LEFT(TABLE,1) <> ' ' .AND. LOCKTYPE <> 0 .AND. (ACCOUNT_ID <> '   ' .OR. WINLOG_ID <> '   ')",  0, 0 },
            /* lock info, connection info, table */
         { "LOCK_LCT", "ASCEND(LOCKTYPE) + ASCEND(LOCKRECNO) + IIF(WINLOG_ID = '   ',UPPER(ACCOUNT_ID),UPPER(WINLOG_ID)) + TCPADDRESS + ASCEND(CONNECT_ID) + TRIM(LEFT(PATH,125)) + TABLE",
               "LEFT(TABLE,1) <> ' ' .AND. LOCKTYPE <> 0 .AND. (ACCOUNT_ID <> '   ' .OR. WINLOG_ID <> '   ')",  0, 0 },
         { 0,0,0,0,0 }
      } ;

      char done, appendDone, recordDone ;
      Lock4 *recordOn ;

      // AS 07/28/00 - mdx version has max key size of 100, some of the tags have sizes of up to 231.
      // work around this by marking the index as 'oledb-schema' which bypasses these limits...
      #ifdef S4MDX
         int oldOledbSchemaCreate = c4->oledbSchemaCreate ;
         c4->oledbSchemaCreate = 1 ;
      #endif
      DATA4 *data = d4createTemp( c4, statusFields, statusTags ) ;
      #ifdef S4MDX
         c4->oledbSchemaCreate = oldOledbSchemaCreate ;
      #endif
      if ( data == NULL )
         return NULL ;

      data->dataFile->singleClient = c4->currentClient ;
      data->trans = &c4->currentClient->trans ;

      FIELD4 *accountId =  d4field( data, "ACCOUNT_ID" ) ;
      FIELD4 *winLogId =  d4field( data, "WINLOG_ID" ) ;
      FIELD4 *tcpAddress = d4field( data, "TCPADDRESS" ) ;
      FIELD4 *connectId =  d4field( data, "CONNECT_ID" ) ;
      FIELD4 *path =       d4field( data, "PATH" ) ;
      FIELD4 *table =      d4field( data, "TABLE" ) ;
      FIELD4 *fileType =   d4field( data, "FILETYPE" ) ;
      FIELD4 *recCount =   d4field( data, "RECCOUNT" ) ;
      FIELD4 *numFields =  d4field( data, "NUMFIELDS" ) ;
      FIELD4 *recWidth =   d4field( data, "REC_WIDTH" ) ;
      FIELD4 *readOnly =   d4field( data, "READ_ONLY" ) ;
      FIELD4 *accessMode = d4field( data, "ACCESSMODE" ) ;
      FIELD4 *lockType =   d4field( data, "LOCKTYPE" ) ;
      FIELD4 *lockRecno =  d4field( data, "LOCKRECNO" ) ;

      SERVER4 *server = c4->server ;

      server4clientListReserve( server ) ;

      char buf[LEN4PATH] ;
      char buf2[LEN4PATH+LEN4TABLE_NAME] ;
      char buf3[LEN4TABLE_NAME] ;

      /* now go through all the clients and add all the info */
      SERVER4CLIENT *client ;
      for ( client = 0 ;; )
      {
         if ( client == c4->catalogClient )  /* done at the end */
            break ;

         client = server4clientGetNext( server, client ) ;
         if ( client == 0 )
            client = c4->catalogClient ;

         int hasData = 0 ;

         LIST4 *list = tran4dataList( &client->trans ) ;
         if ( list != 0 )
         {
            int oldClientId, didEntry = 0 ;
            DATA4 *dataOn ;

            for ( dataOn = NULL ;; )
            {
               if ( dataOn != 0 )
                  dataOn->clientId = oldClientId ;

               dataOn = (DATA4 *)l4next( list, dataOn ) ;
               if ( dataOn == 0 )
                  break ;

               oldClientId = dataOn->clientId ;
               dataOn->clientId = 0 ;  /* set to 0 to do a general query... */
               hasData = 1 ;
               /* now add a record for each lock */
               recordOn = 0 ;

               for( done = 0, recordDone = 0, appendDone = 0 ; done != 1 ; )
               {
                  /* lock types to consider: */
                  /*    append lock          */
                  /*    file lock            */
                  /*    record locks         */
                  /*    index lock           */

                  d4appendStart( data, 0 ) ;
                  f4blank( lockRecno ) ;  // clear out for file/append
                  f4assignInt( lockType, 0L ) ;  // this gets set to 0 if no lock, init to that now

                  if ( d4lockTestFile( dataOn ) == 1 )
                  {
                     f4assignInt( lockType, LOCK4FILE ) ;
                     done = 1 ;   /* a file lock means all is locked, so done */
                  }
                  else
                  {
                     if ( appendDone == 0 && d4lockTestAppend( dataOn ) == 1 )
                     {
                        f4assignInt( lockType, LOCK4APPEND ) ;
                        appendDone = 1 ;   /* done this lock test */
                     }
                     else
                     {
                        /*
                        AS 06/26/97 index locks unverifiable, and not settable, so don't include

                        if ( indexDone == 0 && d4lockTestIndex( dataOn ) == 1 ) // try that
                        {
                           f4assignInt( lockType, LOCK4INDEX ) ;
                           indexDone = 1 ;   // done this lock test
                        }
                        else
                        */
                        {
                           /* try the next record lock */
                           if ( d4lockTest( dataOn, -1L, lock4any ) == 1 )
                           {
                              /* means there are record locks, get the next one */
                              for( ;; )
                              {
                                 if ( recordOn ==  0 )  /* means just starting */
                                 {
                                    recordOn = (Lock4 *)(Single4lock *)(dataOn->lockedRecords.initIterate()) ;
                                    if ( recordOn == 0 )
                                    {
                                       done = 1 ;
                                       break ;
                                    }
                                 }
                                 else
                                    recordOn = (Lock4 *)(Single4lock *)(((Single4lock *)recordOn)->next()) ;
                                 if ( recordOn ==  0 )  /* means done */
                                 {
                                    done = 1 ;
                                    break ;
                                 }
                                 break ;
   //                              if ( dataOn == tran4data( &client->trans, recordOn->id.serverId, recordOn->clientId) ) /* our data4 */
   //                                 break ;
                              }
                              if ( done == 1 )  // done, so quit loop
                              {
                                 if ( didEntry == 1 )  /* if didEntry == 0, means we had no entry, add one now */
                                    break ;
                              }
                              else
                              {
                                 recordDone = 1 ;
                                 f4assignInt( lockType, LOCK4RECORD ) ;
                                 f4assignLong( lockRecno, recordOn->recNum ) ;
                              }
                           }
                           else /* we are done, add a record if no locks recorded */
                           {
                              done = 1 ;
                              f4assignInt( lockType, 0L ) ;
                              if ( appendDone || recordDone )
                              {
                                 if ( didEntry == 1 )  /* if didEntry == 0, means we had no entry, add one now */
                                    break ;   /* don't record this no-lock entry */
                              }
                           }
                        }
                     }
                  }

                  f4assign( accountId, client->account.accountId ) ;
                  f4assign( tcpAddress, client->account.tcpAddress ) ;
                  f4assignLong( connectId, client->id ) ;
                  u4nameCurrent( buf2, sizeof( buf2 ), dataOn->dataFile->file.name ) ;
                  u4namePath( buf, sizeof( buf ), buf2 ) ;
                  f4assign( path, buf ) ;
                  u4namePiece(buf3, sizeof( buf3 ), buf2, 0, 1 ) ;
                  f4assign( table, buf3 ) ;
                  if ( dataOn->dataFile->file.isTemp == 1 )
                     f4assignChar( fileType, TYPE4TEMP ) ;
                  else
                     f4assignChar( fileType, TYPE4PERMANENT ) ;
                  /* else if schema ... */
                  f4assignLong( recCount, d4recCount( dataOn ) ) ;
                  f4assignInt( numFields, d4numFields( dataOn ) ) ;
                  f4assignInt( recWidth, d4recWidth( dataOn ) ) ;
                  if ( dataOn->readOnly )
                     f4assignChar( readOnly, 'T' ) ;
                  else
                     f4assignChar( readOnly, 'F' ) ;
                  f4assignInt( accessMode, (char)dataOn->accessMode ) ;
                  d4append( data ) ;
               }
            }
            didEntry = 1 ;
         }

         if ( hasData == 0 )  /* client has no datafiles -- add a record anyway */
         {
            d4appendStart( data, 0 ) ;
            d4blank( data ) ;
            f4assign( accountId, client->account.accountId ) ;
            f4assign( tcpAddress, client->account.tcpAddress ) ;
            f4assignLong( connectId, client->id ) ;
            f4assignInt( lockType, 0L ) ;  // this gets set to 0 if no lock, init to that now
            d4append( data ) ;
         }
      }

      /* now do all the data files with no clients (i.e. bufferred open) */

      DATA4FILE *dataFileOn = 0 ;
      for( ;; )
      {
         dataFileOn = (DATA4FILE *)l4next( &c4->dataFileList, dataFileOn ) ;
         if ( !dataFileOn )
            break ;

         if ( dataFileOn->userCount == 0 )
         {
            d4appendStart( data, 0 ) ;
            d4blank( data ) ;
            f4assignInt( lockType, 0L ) ;  /* this gets set to 0 if no lock, init to that now */
            u4nameCurrent( buf2, sizeof( buf2 ), dataFileOn->file.name ) ;
            u4namePath( buf, sizeof( buf ), buf2 ) ;
            f4assign( path, buf ) ;
            u4namePiece(buf3, sizeof( buf3 ), buf2, 0, 1 ) ;
            f4assign( table, buf3 ) ;

            if ( dataFileOn->file.isTemp == 1 )
               f4assignChar( fileType, TYPE4TEMP ) ;
            else
               f4assignChar( fileType, TYPE4PERMANENT ) ;

            /* else if schema ... */
            f4assignLong( recCount, dfile4recCount( dataFileOn, 0L ) ) ;
            f4assignInt( numFields, dataFileOn->nFields ) ;
            f4assignInt( recWidth, dfile4recWidth( dataFileOn ) ) ;
            if ( dataFileOn->file.isReadOnly )
               f4assignChar( readOnly, 'T' ) ;
            else
               f4assignChar( readOnly, 'F' ) ;
            f4assignInt( accessMode, (char)dataFileOn->file.lowAccessMode ) ;
            if ( dataFileOn->fileServerWriteLock || ( dataFileOn->fileReadLocks.initIterate() != 0 ) )
               f4assignInt( lockType, LOCK4FILE ) ;
            else
            {
               if ( dataFileOn->appendServerLock )
                  f4assignInt( lockType, LOCK4APPEND ) ;
               else
               {
                  if ( dataFileOn->recordLockWriteCount > 0 || dataFileOn->recordLockReadCount )
                     f4assignInt( lockType, LOCK4RECORD ) ;
                  else
                     f4assignInt( lockType, 0L ) ;
               }
            }
            d4append( data ) ;
         }
      }

      server4clientListRelease( server ) ;

      #if defined( S4ODBC_ENABLED )
         // now do all the ODBC clients...
         void *odbcClientsHandle ;
         short clientRc = odbc4getClientsList( &odbcClientsHandle ) ;
         if ( clientRc != 0 )
         {
            d4close( data ) ;
            return 0 ;
         }

         short clientHandle = odbc4getClientsFirst( odbcClientsHandle ) ;
         for ( ;; )
         {
            if ( clientHandle == -1 )  // done...
               break ;

            // add record for the ODBC client...
            d4appendStart( data, 0 ) ;
            d4blank( data ) ;
            TCP4ADDRESS tcpAddressData ;

            // note that for ODBC we use the computer id, not the user id... userid not avail...
            f4assign( winLogId, odbc4getAccountId( odbcClientsHandle, clientHandle ) ) ;
            char tcpAddressChar[16] ;
            odbc4getTcpAddress( odbcClientsHandle, clientHandle, &tcpAddressData ) ;
            tcp4addressToChar( tcpAddressChar, tcpAddressData ) ;
            f4assign( tcpAddress, tcpAddressChar ) ;
            // use the actual address of the pointer for the LAM User so can always re-fetch it... in case of disconnect, etc.
            LONGLONG lamUserId = odbc4getLamUserId( odbcClientsHandle, clientHandle ) ;
            f4assignLongLong( connectId, lamUserId ) ;

            f4assignInt( lockType, 0L ) ;  // this gets set to 0 if no lock, init to that now
            d4append( data ) ;
            clientHandle = odbc4getClientsNext( odbcClientsHandle, clientHandle ) ;
         }
         odbc4freeClientsList( odbcClientsHandle ) ;
      #endif

      return data ;
   }
#endif /* S4SERVER */
