/* d4data.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifdef __TURBOC__
   #pragma hdrstop
#endif  /* __TUROBC__ */

#ifndef S4WINCE   /* LY 2002/08/28 */
   #include <time.h>
#endif

/* LY 2001/08/21 : added ifdef to avoid compile error on non-WIN32 */
#if defined(S4WIN32) && !defined(S4STAND_ALONE)
   #include <sys\types.h>
   #include <sys\stat.h>
#endif

#ifndef S4INLINE
   #ifdef S4STAND_ALONE
      unsigned long S4FUNCTION data4serverId( DATA4 *d4 )
      {
         return d4->clientId ;
      }



      unsigned long data4clientId( DATA4 *d4 )
      {
         return d4->clientId ;
      }
   #endif



   #ifdef S4SERVER
      unsigned long S4FUNCTION data4serverId( DATA4 *d4 )
      {
         return d4->serverId ;
      }



      unsigned long data4clientId( DATA4 *d4 )
      {
         return d4->clientId ;
      }
   #endif



   #ifdef S4CLIENT
      unsigned long S4FUNCTION data4serverId( DATA4 *d4 )
      {
         return d4->dataFile->serverId ;
      }



      unsigned long data4clientId( DATA4 *d4 )
      {
         return d4->clientId ;
      }
   #endif
#endif  /* S4INLINE */



S4CONST char *S4FUNCTION d4alias( S4CONST DATA4 *data )
{
   #ifdef E4VBASIC
      if ( c4parm_check( data, 2, E93301 ) )
         return (char *) NULL;
   #endif  /* E4VBASIC */

   #ifdef E4PARM_HIGH
      if ( data == 0 )
      {
         error4( 0, e4parm_null, E93301 ) ;
         return 0 ;
      }
   #endif
   return data->alias ;
}



/* Search the DATA4.alias string for spaces, and replace with underscores */
void d4aliasFix( DATA4 S4PTR *data )
{
   int i;
   #ifdef E4PARM_HIGH
      if ( data == 0 )
      {
         error4( 0, e4parmNull, E93302 ) ;
         return ;
      }
   #endif

   for ( i = 0 ; i < sizeof( data->alias ) ; i++ )
      if ( (data->alias)[i] == ' ' )
         (data->alias)[i] = '_' ;
}



void S4FUNCTION d4aliasSet( DATA4 *data, const char * newAlias )
{
   #ifdef E4PARM_HIGH
      if ( data == 0 || newAlias == 0 )
      {
         error4( 0, e4parm_null, E93302 ) ;
         return ;
      }
   #endif

   #ifdef E4VBASIC
      if ( c4parm_check( data, 2, E93302 ) )
         return ;
   #endif  /* E4VBASIC */

   if ( u4namecmp( data->alias, newAlias, data->codeBase->ignoreCase ) )
   {
      u4ncpy( data->alias, newAlias, sizeof( data->alias ) ) ;
      d4aliasFix( data ) ;

      #ifdef S4CLIENT
         data->aliasSet = 1 ;
      #endif
   }

   return ;
}



void S4FUNCTION d4blankLow( DATA4 *data, char *record )
{
   assert5( record != 0 && data != 0 ) ;

   if ( data->recordBlank != 0 )
      c4memcpy( record, data->recordBlank, (unsigned int)dfile4recWidth( data->dataFile ) ) ;
   else
      c4memset( record, ' ', (unsigned int)dfile4recWidth( data->dataFile ) ) ;

   #ifndef S4OFF_MEMO
      // AS 06/09/00 was not compiling in S4OFF_MULTI
      #ifndef S4OFF_MULTI
         // AS 02/24/00 was not marking memo ids as valid, thus sometimes was re-reading these in causing problems.
         data->memoValidated = 1 ;
      #endif

      if ( data->fieldsMemo != 0 )
      {
         for ( int memoIndex = 0 ; memoIndex < data->dataFile->nFieldsMemo ; memoIndex++ )
         {
            f4memoFree( data->fieldsMemo[memoIndex].field ) ;
         }
      }
   #endif /* !S4OFF_MEMO */

   return ;
}



void S4FUNCTION d4blank( DATA4 *data )
{
   #ifdef E4VBASIC
      if ( c4parm_check( data, 2, E93303 ) )
         return ;
   #endif  /* E4VBASIC */

   #ifdef E4PARM_HIGH
      if ( data == 0 )
      {
         error4( 0, e4parm_null, E93303 ) ;
         return ;
      }
   #endif

   #ifndef S4SERVER
      #ifndef S4OFF_ENFORCE_LOCK
         if ( data->codeBase->lockEnforce && data->recNum > 0L )
            if ( d4lockTest( data, data->recNum ) != 1 )
            {
               error4( data->codeBase, e4lock, E93303 ) ;
               return ;
            }
      #endif
   #endif

   d4blankLow( data, data->record ) ;

   data->recordChanged = 1 ;

   return ;
}



int S4FUNCTION d4bof( DATA4 *data )
{
   #ifdef E4VBASIC
      if ( c4parm_check( data, 2, E93304 ) )
         return -1;
   #endif  /* E4VBASIC */

   #ifdef E4PARM_HIGH
      if ( data == 0 )
         return error4( 0, e4parm_null, E93304 ) ;
   #endif

   if ( error4code( data->codeBase ) < 0 )
      return e4codeBase ;

   return data->bofFlag ;
}



#ifndef S4OFF_WRITE
   void S4FUNCTION d4delete( DATA4 *data )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E93305 ) )
            return ;
      #endif  /* E4VBASIC */

      #ifdef E4PARM_HIGH
         if ( data == 0 )
         {
            error4( 0, e4parm_null, E93305 ) ;
            return ;
         }
      #endif
      #ifdef E4MISC
         if ( data->record[0] != ' ' && data->record[0] != '*' )
         {
            error4( data->codeBase, e4info, E83301 ) ;
            return ;
         }
      #endif

      if ( data->record[0] != '*' )
      {
         #ifndef S4SERVER
            #ifndef S4OFF_ENFORCE_LOCK
               if ( data->codeBase->lockEnforce && data->recNum > 0L )
                  if ( d4lockTest( data, data->recNum ) != 1 )
                  {
                     error4( data->codeBase, e4lock, E93305 ) ;
                     return ;
                  }
            #endif
         #endif
           data->record[0] = '*' ;
         data->recordChanged = 1 ;
      }

      return ;
   }
#endif



#if defined( S4OLEDB_OR_NOT_SERVER ) || defined( S4ODBC_BUILD ) || defined( E4MISC )
   int S4FUNCTION d4deleted( DATA4 *data )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E93306 ) )
            return -1;
      #endif  /* E4VBASIC */
      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E93306 ) ;
      #endif
      #ifdef E4MISC
         if ( data->record[0] != ' ' && data->record[0] != '*' )
            return error4( data->codeBase, e4info, E83301 ) ;
      #endif

      return *data->record != ' ' ;
   }
#endif /* S4OLEDB_OR_NOT_SERVER */



int S4FUNCTION d4eof( DATA4 *data )
{
   #ifdef E4VBASIC
      if ( c4parm_check( data, 2, E93307 ) )
         return -1;
   #endif  /* E4VBASIC */

   #ifdef E4PARM_HIGH
      if ( data == 0 )
         return error4( 0, e4parm_null, E93307 ) ;
   #endif

   if ( error4code( data->codeBase ) < 0 )
      return e4codeBase ;

   return data->eofFlag ;
}



const char *S4FUNCTION d4fileName( DATA4 *data )
{
   #ifdef S4CLIENT
      CONNECTION4 *connection ;
      int rc ;
   #endif

   #ifdef E4PARM_HIGH
      if ( data == 0 )
      {
         error4( 0, e4parm_null, E93205 ) ;
         return 0 ;
      }
   #endif

   #ifdef E4VBASIC
      if ( c4parm_check( data, 2, E40139 ) )
         return 0 ;
   #endif

   #ifdef S4CLIENT
      if ( error4code( data->codeBase ) < 0 )
         return 0 ;

      connection = data->dataFile->connection ;
      connection4assign( connection, CON4DATA_FNAME, data4clientId( data ), data4serverId( data ) ) ;
      connection4sendMessage( connection ) ;
      rc = connection4receiveMessage( connection ) ;
      if ( rc < 0 )
      {
         #ifdef E4STACK
            error4stack( data->codeBase, rc, E95501 ) ;
         #endif
         return 0 ;
      }

      rc = connection4status( connection ) ;
      if ( rc < 0 )
      {
         connection4error( connection, data->codeBase, rc, E95501 ) ;
         return 0 ;
      }

      return connection4data( connection ) ;
   #else
      return data->dataFile->file.name ;
   #endif
}



short S4FUNCTION d4logStatusCB( DATA4 *data )
{
   if (!data)
      return 0 ;
   return d4logStatus(data) ;
}



#if !defined( S4OFF_TRAN ) && defined( S4STAND_ALONE ) && !defined( S4OFF_WRITE )
   int S4FUNCTION d4log( DATA4 *data, const int logging )
   {
      /* can change LOG4ON-->LOG4TRANS or LOG4TRANS-->LOG4ON, also new support for LOG4OFF for internal
         (i.e. schema) tables which we do not ever want to log.
      */
      int oldVal ;

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E93319 ) ;
      #endif

      /* AS 12/10/98 added support for hard-offing schema tables.  This overrides everything to
         disallow logging on these files.
      */

      oldVal = ( data->logVal == 0 ) ? 0 : 1 ;

      if ( logging == LOG4OFF )
      {
         data->logVal = LOG4OFF ;
         return oldVal ;
      }

      if ( code4transEnabled( data->codeBase ) == 0 )
      {
         if ( logging == -1 )
            return r4logOff ;
         else
            return error4( data->codeBase, e4trans, E83807 ) ;
      }

      if ( data->logVal == LOG4ALWAYS )
         return r4logOn ;

      if ( logging != -1 )
      {
         if ( logging )
         {
            if ( data->logVal == LOG4TRANS )
               data->logVal = LOG4ON ;
         }
         else
         {
            if ( data->logVal == LOG4ON )
               data->logVal = LOG4TRANS ;
         }
      }

      return oldVal ;
   }
#endif /* #if !defined( S4OFF_TRAN ) && defined( S4STAND_ALONE ) && !defined( S4OFF_WRITE ) */



#ifndef S4SERVER
   short int S4FUNCTION d4numFields( DATA4 *data )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E93308 ) )
            return -1;
      #endif  /* E4VBASIC */

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E93308 ) ;
      #endif

      #ifdef S4CLIENT_OR_FOX
         if ( data->fields[data->dataFile->nFields-1].type == '0' )   /* null flags field */
            return data->dataFile->nFields - 1 ;
      #endif

      return data->dataFile->nFields ;
   }
#endif



#ifndef S4CLIENT
   /*  currently this function is not used...
   int d4read( DATA4 *data, const long recNum, char *ptr )
   {
      #ifdef E4PARM_LOW
         if ( data == 0 || recNum <= 0 || ptr == 0 )
            return error4( 0, e4parm, E93309 ) ;
      #endif

      return dfile4read( data->dataFile, recNum, ptr, 0 ) ;
   }
   */



   int d4readOld( DATA4 *data, const long recNum )
   {
      int rc ;

      #ifdef E4PARM_LOW
         if ( data == 0 || recNum <= 0 )
            return error4( 0, e4parm, E93310 ) ;
      #endif

      if ( error4code( data->codeBase ) < 0 )
         return e4codeBase ;

      if ( recNum <= 0 )
      {
         data->recNumOld = recNum ;
         d4blankLow( data, data->recordOld ) ;
      }

      if ( data->recNumOld == recNum )
         return 0 ;

      data->recNumOld = -1 ;
      // AS 05/20/99 --> don't actually want to read 'from disk' in this case.  We just want
      // the old record, whatever it may be (if bufferred internal, that is ok) - t5transmm.cpp
      // AS 05/20/99 --> actually, we do want to read from disk.  The conditional is that it
      // flushes changes to disk first.  Basically, we want to read the 'latest' that anybody
      // else has done (don't use our 'read' buffer).  If we have it write bufferred, must ensure
      // that we get that one...
      rc = dfile4read( data->dataFile, recNum, data->recordOld, 1 ) ;
      if ( rc < 0 )
         return -1 ;
      if ( rc > 0 )
         d4blankLow( data, data->recordOld ) ;
      data->recNumOld = recNum ;

      return 0 ;
   }
#endif /* !S4CLIENT */



#if !defined( S4SERVER ) && !defined( S4OFF_WRITE )
   void S4FUNCTION d4recall( DATA4 *data )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E93311 ) )
            return ;
      #endif  /* E4VBASIC */

      #ifdef E4PARM_HIGH
         if ( data == 0 )
         {
            error4( 0, e4parm_null, E93311 ) ;
            return ;
         }
      #endif
      #ifdef E4MISC
         if ( data->record[0] != ' ' && data->record[0] != '*' )
         {
            error4( data->codeBase, e4info, E83301 ) ;
            return ;
         }
      #endif

      if ( *data->record != ' ' )
      {
         #ifndef S4SERVER
            #ifndef S4OFF_ENFORCE_LOCK
               if ( data->codeBase->lockEnforce && data->recNum > 0L )
                  if ( d4lockTest( data, data->recNum, lock4write ) != 1 )
                  {
                     error4( data->codeBase, e4lock, E93311 ) ;
                     return ;
                  }
            #endif
         #endif
         *data->record = ' ' ;
         data->recordChanged = 1 ;
      }

      return ;
   }
#endif /* #if !defined( S4SERVER ) && !defined( S4OFF_WRITE ) */



int S4FUNCTION d4recCountLessEq( DATA4 *data, long count )
{
   /* returns true if the input count is <= record count */

   long count2 = data->count ;
   if ( count2 > dfile4getMinCount( data->dataFile ) ) /* transaction rollback possibility */
      count2 = dfile4getMinCount( data->dataFile ) ;
   if ( count <= count2 )
      return 1 ;
   data->count = d4recCount( data ) ;
   if ( data->count < 0 )
      return error4( data->codeBase, (short int)data->count, E93318 ) ;
   if ( count <= data->count )
      return 1 ;
   return 0 ;
}



int S4FUNCTION d4recCountLess( DATA4 *data, long count )
{
   /* returns true if the input count is < record count */

   long count2 = data->count ;
   if ( count2 > dfile4getMinCount( data->dataFile ) ) /* transaction rollback possibility */
      count2 = dfile4getMinCount( data->dataFile ) ;
   if ( count < count2 )
      return 1 ;
   data->count = d4recCount( data ) ;
   if ( data->count < 0 )
      return error4( data->codeBase, (short int)data->count, E93318 ) ;
   if ( count < data->count )
      return 1 ;
   return 0 ;
}



int S4FUNCTION d4recCountGreater( DATA4 *data, long count )
{
   /* returns true if the input count is > record count */

   long count2 = data->count ;
   if ( count2 > dfile4getMinCount( data->dataFile ) ) /* transaction rollback possibility */
      count2 = dfile4getMinCount( data->dataFile ) ;
   if ( count <= count2 )
      return 0 ;
   data->count = d4recCount( data ) ;
   if ( data->count < 0 )
      return error4( data->codeBase, (short int)data->count, E93318 ) ;
   if ( count > data->count )
      return 1 ;
   return 0 ;
}



#ifndef S4OFF_MULTI
   long S4FUNCTION d4recCountDo( DATA4 *data )
   {
      // AS d4recCountDo is exporeted, cannot modify, but call new d4recCountDo2 instead
      return d4recCountDo2( data, 0 ) ;
   }
// AS Oct 24/02 - include d4recCountDo2 in off-multi due to def inclusion
#endif



long S4FUNCTION d4recCountDo2( DATA4 *data, Bool5 assumeLocked )
{
   /* note that S4OFF_MULTI version just calls dfile4recCount since checking of
      locks and serverId are not required */

   // AS 09/15/99 -- added assumeLocked flag for when called during append, where
   // we wouldn't be at this point if the append bytes were not locked.  This improves
   // performance, esp. OLE-DB by as much as 1% or more in cases where performance
   // is critical.
   #ifdef E4VBASIC
      if ( c4parm_check( data, 2, E93312 ) )
         return -1L ;
   #endif  /* E4VBASIC */

   #ifdef E4PARM_HIGH
      if ( data == 0 )
         return error4( 0, e4parm_null, E93312 ) ;
   #endif

   if ( data->dataFile->numRecs >= 0L )
   {
      #ifndef S4OFF_MULTI
         // AS Aug 9/02 - Need to have the capability to determine the clientId and serverId of the
         // source DATA4 for the server's relation DATA4 module.  In particular, we want to allow
         // for record counts to be accessible for clients who have a transaction in progress and
         // then use a relation to access those fields.
         if ( assumeLocked == !ASSUME4LOCKED )
         {
            int isLocked = d4lockTestAppend( data ) ;
            #ifdef S4SERVER
               if ( isLocked != 1 && data->relationsSourceClientId != 0 )
               {
                  // Try with the clients rouce client and server id to see if locked there
                  long saveClientId = data->clientId ;
                  long saveServerId = data->serverId ;
                  data->clientId = data->relationsSourceClientId ;
                  data->serverId = data->relationsSourceServerId ;
                  isLocked = d4lockTestAppend( data ) ;
                  data->clientId = saveClientId ;
                  data->serverId = saveServerId ;

               }
            #endif
            if ( isLocked != 1 )
               return dfile4getMinCount( data->dataFile ) ;
         }
      #endif

      return data->dataFile->numRecs ;
   }

   return dfile4recCount( data->dataFile, data4serverId( data ) ) ;
}



#ifndef S4SERVER
   long S4FUNCTION d4recNoLow( DATA4 *data )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E93313 ) )
            return -1L;
      #endif  /* E4VBASIC */

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E93313 ) ;
      #endif

      return data->recNum ;
   }


   char *S4FUNCTION d4recordLow( DATA4 *data )
   {
      #ifdef E4PARM_HIGH
         if ( data == 0 )
         {
            error4( 0, e4parm_null, E93314 ) ;
            return 0 ;
         }
      #endif

      return data->record ;
   }
#endif /* !S4SERVER */



#ifdef S4STAND_ALONE
   int S4FUNCTION d4lowAccessMode( DATA4 *d4 )
   {
      // for odbc
      // AS 06/09/00 was not compiling in S4OFF_MULTI
      #ifdef S4OFF_MULTI
         return OPEN4DENY_RW ;
      #else
         return d4->dataFile->file.lowAccessMode ;
      #endif
   }



   void S4FUNCTION d4recNoSet( DATA4 *d4, long recNo )
   {
      // for odbc
      d4->recNum = recNo ;
   }



   void S4FUNCTION d4recordSet( DATA4 *data, char *newRecPtr )
   {
      // used by ODBC
      #ifdef E4PARM_LOW
         if ( data == 0 || newRecPtr == 0 )
         {
            error4( 0, e4parm_null, E93314 ) ;
            return ;
         }
      #endif

      data->record = newRecPtr ;
   }
#endif /* S4STAND_ALONE */



unsigned long S4FUNCTION d4recWidthLow( DATA4 *data )
{
   #ifdef E4PARM_HIGH
      if ( data == 0 )
      {
         error4( 0, e4parm_null, E93316 ) ;
         return 0 ;
      }
   #endif

   #ifdef S4PREPROCESS_FILE
      // AS May 29/02 - dfile4recWidth returns in incorrect value in the instance where block preprocessing
      // is being used because we have padded the record buffer out.
      if ( data->exposedRecordLen == 0 )
      {
         // calculate it by adding up the field widths
         for ( unsigned short fieldIndex = 0 ; fieldIndex < data->dataFile->nFields ; fieldIndex++ )
         {
            data->exposedRecordLen += data->fields[fieldIndex].len ;
         }
      }

      return data->exposedRecordLen ;
   #else
      return dfile4recWidth( data->dataFile ) ;
   #endif
}



#ifdef P4ARGS_USED
   #pragma argsused
#endif
int d4verify( DATA4 *d4, const int subs )
{
   #ifndef S4OFF_WRITE
      #ifdef E4ANALYZE
         int rc ;
      #endif
   #endif

   if ( d4 == 0 )
      return error4( d4->codeBase, e4parm_null, E93317 ) ;

   if ( d4->trans == 0 )
      return error4( 0, e4struct, E93317 ) ;

   #ifndef S4OFF_WRITE
      #ifdef E4ANALYZE
         if ( subs == 1 )
            if ( ( rc = tran4verify( d4->trans, 1 ) ) < 0 )
               return rc ;
      #endif
   #endif

   if ( d4->link.n == 0 || d4->link.p == 0 )
      return error4( d4->codeBase, e4struct, E93317 ) ;

   #ifndef S4CLIENT
      if ( d4->dataFile == 0 )
         return error4( d4->codeBase, e4struct, E93317 ) ;

      #ifdef E4ANALYZE
         if ( subs == 1 )
            return dfile4verify( d4->dataFile, 0 ) ;
      #endif
   #endif

   return 0 ;
}


/* LY 00/11/16 : exporting function due to failure of Microsoft IA-64 compiler */
#if defined( S4FILE_EXTENDED ) && !defined( S464BIT ) && !defined( S4UNIX ) && (!defined(__cplusplus) || defined(S4WIN64))
   FILE4LONG S4FUNCTION file4longCoerce( LONGLONG val )
   {
      // assert5( sizeof( val ) == sizeof( FILE4LONG ) ) ;
      return *(((FILE4LONG *)(&val))) ;
   }
#endif



#if !defined( S4SERVER ) && !defined( S4ODBC_BUILD )
   short S4FUNCTION d4codePage( DATA4 *data )
   {
      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E93316 ) ;
      #endif

      if ( error4code( data->codeBase ) < 0 )
         return e4codeBase ;

      #ifdef S4CLIENT
         if ( data->dataFile->codePageRead != 1 )
         {
            CONNECTION4 *connection = data->dataFile->connection ;
            connection4assign( connection, CON4DATA_CODEPAGE, data4clientId( data ), data4serverId( data ) ) ;
            connection4sendMessage( connection ) ;
            int rc = connection4receiveMessage( connection ) ;
            if ( rc < 0 )
               return rc ;

            rc = connection4status( connection ) ;
            if ( rc < 0 )
            {
               connection4error( connection, data->codeBase, rc, E95501 ) ;
               return rc ;
            }

            data->dataFile->codePage = rc ;
            data->dataFile->codePageRead = 1 ;
         }
      #endif

      return data->dataFile->codePage ;
   }
#endif

// available in client/server or stand alone if WIN32
#if defined( S4WIN32 ) || !defined( S4STAND_ALONE )
   #ifdef S4STAND_ALONE
      void dfile4versionIncrement( DATA4FILE *dataFile )
      {
         // used to increment the version number of the file and update the time stamp
         // on the file.  This allows for other processes to check if the data file
         // has been updated

         dataFile->versionNumber++ ;
         // if the file is not open exclusive and we are updating file times, then do so
         #ifndef S4OFF_MULTI
            if ( dataFile->file.lowAccessMode != OPEN4DENY_RW && dataFile->c4->updateFileTime == 1 )
            {
               FILETIME fileTime;
               SYSTEMTIME systemTime;

               GetSystemTime(&systemTime);              // gets current time
               SystemTimeToFileTime(&systemTime, &fileTime);  // converts to file time format
               SetFileTime(dataFile->file.hand, 0, 0, &fileTime);  // sets lasystemTime-write time for file
            }
         #endif
      }
   #endif


   long S4FUNCTION d4versionNumber( DATA4 *data )
   {
      /* AS Aug 15/01 - Function to retrieve a version number associated with the data file, which increases when
         the data file changes

         Every time the data file changes the version number is incremented.
         We also check here to see if it was externally modified, which also increments the version number

         We only return to the user incremented values.  So, if they previously were returned a '3' and the
         file has since changed, they are returned a '4'.  The internal versionNumber is reset by this function
         to == the value returned to the user.  This keeps the number from getting too large as well as making
         it easy to track if the file has changed
      */

      #ifdef S4CLIENT
         CONNECTION4VERSION_INFO_OUT *info ;
         CONNECTION4 *connection ;
         int rc ;
         CODE4 *c4 = data->codeBase ;

         connection = data->dataFile->connection ;
         if ( connection == 0 )
            return e4connection ;

         connection4assign( connection, CON4VERSION, 0, data->dataFile->serverId ) ;
         connection4sendMessage( connection ) ;
         rc = connection4receiveMessage( connection ) ;
         if ( rc < 0 )
            return rc ;
         rc = connection4status( connection ) ;
         if ( rc != 0 )
            return connection4error( connection, c4, rc, E91102 ) ;

         if ( connection4len( connection ) != sizeof( CONNECTION4VERSION_INFO_OUT ) )
            return error4( c4, e4packetLen, E91102 ) ;
         info = (CONNECTION4VERSION_INFO_OUT *)connection4data( connection ) ;
         long versionNum = ntohl5(info->versionNumber) ;
         if ( versionNum < 0 )
            return -1L ;
         return versionNum ;
      #else
         // once the version number is requested, ensure that we always update it on disk so other users can
         // check if has changed (in particular this applies for the server)
         DATA4FILE *dataFile = data->dataFile ;
         #ifdef S4STAND_ALONE
            data->codeBase->updateFileTime = 1 ;
         #endif

         // in windows 32 bit try to check the physical date time as well if not open exclusive
         #if defined( S4WIN32 ) && !defined( S4OFF_MULTI )
            if ( dataFile->file.lowAccessMode == OPEN4DENY_NONE )   // not denying writes, so other users may have changed it
            {
               // means file may have been changed externally, so update the date stamp
               FILETIME ftWrite ;

               // if _stat returns error, file doesn't exist, just assume no date change since it is not available
               if ( GetFileTime( dataFile->file.hand, 0, 0, &ftWrite ) )
               {
                  /*
                     #if defined( E4ANALYZE ) && defined( S4TESTING )
                        SYSTEMTIME systemTime ;
                        FILETIME ftLocal ;
                        FileTimeToLocalFileTime(&ftWrite, &ftLocal ) ;
                        FileTimeToSystemTime(&ftLocal, &systemTime);
                        char str[120];
                        wsprintf(str, "\n%02d/%02d/%d  %02d:%02d:%02d.%03d", systemTime.wDay, systemTime.wMonth, systemTime.wYear, systemTime.wHour, systemTime.wMinute, systemTime.wSecond, systemTime.wMilliseconds);
                        #ifdef S4CONSOLE
                           printf( str ) ;
                        #endif
                     #endif
                  */
                  // compare the file date with that we have now
                  if ( memcmp( &dataFile->timeStamp, &ftWrite, sizeof( FILETIME ) ) != 0 )  // means the timestamp did change
                  {
                     dataFile->timeStamp = ftWrite ;
                     dataFile->versionNumber++ ;
                  }
               }
            }
         #endif

         if ( dataFile->versionNumber > dataFile->versionNumberOld )  // was changed
         {
            // reset the version number to the 'old' number + 1, and then return it.
            // this helps to keep the number from overflowing if many changes occur to the file
            dataFile->versionNumber = ++dataFile->versionNumberOld ;
         }

         return dataFile->versionNumber ;
      #endif
   }
#endif /* #if defined( S4WIN32 ) || !defined( S4STAND_ALONE ) */


#ifdef S4VB_DOS
   char * d4alias_v( DATA4 *d4 )
   {
      return v4str( d4alias(d4) ) ;
   }



   void d4aliasSet( DATA4 *d4, const char *alias )
   {
      d4aliasSet( d4, c4str(alias) ) ;
   }
#endif


#ifdef S4FOX
   int S4FUNCTION d4compatibilityExport( DATA4 *d4 )
   {
      return d4->dataFile->compatibility ;
   }
#endif


#ifdef S4CLIENT
   void d4batchReadFree( DATA4 *data )
   {
      DATA4BATCH_READ *batch = &data->batchRead ;

      if ( batch->readRecsToBuf != 0 )
      {
         assert5( batch->readAdvanceBuf != 0 ) ;
         if ( batch->memos != 0 )
         {
            MEMO4BATCH *batchOn = batch->memos ;
            for ( int onEntry = batch->readRecsToBuf - 1 ; onEntry >= 0 ; onEntry-- )
            {
               if ( batchOn->memos != 0 )
               {
                  for ( int onMemo = data->dataFile->nFieldsMemo - 1 ; onMemo >= 0 ; onMemo -- )
                  {
                     if ( batchOn->memos[onMemo].contents != 0 )
                     {
                        u4free( batchOn->memos[onMemo].contents ) ;
                        batchOn->memos[onMemo].contents = 0 ;
                        batchOn->memos[onMemo].contentsLen = 0 ;
                        batchOn->memos[onMemo].contentsAllocLen = 0 ;
                     }
                  }
               }
               batchOn++ ;
            }

            u4free( batch->memos ) ;
            batch->memos = 0 ;
            if ( batch->memoBatchEntry != 0 )
            {
               u4free( batch->memoBatchEntry ) ;
               batch->memoBatchEntry = 0 ;
            }
         }

         u4free( batch->readAdvanceBuf ) ;
         batch->readAdvanceBuf = 0 ;
         batch->readRecsToBuf = 0 ;
         d4readBufferReset( data ) ;
      }

      assert5( batch->readRecsToBuf == 0 ) ;
   }



   static int d4batchReadAlloc( DATA4 *data, long numRecs, short doMemos )
   {
      DATA4BATCH_READ *batch = &data->batchRead ;

      batch->readAdvanceBuf = (char *)u4allocFree( data->codeBase, (d4recWidth( data ) + sizeof( long )) * numRecs ) ;
      if ( batch->readAdvanceBuf == 0 )
         return e4memory ;

      int numMemos = data->dataFile->nFieldsMemo ;

      if ( doMemos != 0 && numMemos != 0 )
      {
         // allocate memory for memo fields
         // AS Sept 5/02 - may already be allocated, in which case free...
         if ( batch->memos != 0 )
         {
            u4free( batch->memos ) ;
            batch->memos = 0 ;
         }
         batch->memos = (MEMO4BATCH *)u4allocFree( data->codeBase, numRecs * sizeof( MEMO4BATCH ) ) ;
         if ( batch->memos == 0 )
         {
            d4batchReadFree( data ) ;
            return e4memory ;
         }

         if ( batch->memoBatchEntry != 0 )
         {
            u4free( batch->memoBatchEntry ) ;
            batch->memoBatchEntry = 0 ;
         }
         batch->memoBatchEntry = (MEMO4BATCH_ENTRY *)u4allocFree( data->codeBase, numRecs * sizeof( MEMO4BATCH_ENTRY ) * numMemos ) ;
         if ( batch->memoBatchEntry == 0 )
         {
            d4batchReadFree( data ) ;
            return e4memory ;
         }
         // and set up the memo entries
         for ( int memoLoop = 0 ; memoLoop < numRecs ; memoLoop++ )
         {
            batch->memos[memoLoop].memos = batch->memoBatchEntry + (numMemos * memoLoop) ;
         }
      }

      return 0 ;
   }
#endif /* S4CLIENT */



/* AS Dec 17/02 - New function for configuring advance-reading client/server */
S4EXPORT int S4FUNCTION d4readBufferConfigure( DATA4 *data, long flags )
{
   // allows specific configuration of the batch reading.  In particular, the following flags can
   // be set or not.  The flags must be set to on for the feature to be enabled, if left out
   // the feature is disabled.
   //
   // if the configure function is not called, the default (which is set when a data file is opened) is as follows:
   //    r4batchSkip |  r4batchTop | r4batchBottom | r4batchSeekNext | r4batchSeekMatch
   //
   // r4batchSkip      - enable batching on calls to d4skip()
   // r4batchTop       - enable batching on d4top()
   // r4batchBottom    - enable batching on d4bottom()
   // r4batchSeekNext  - enable batching on calls to d4seekNext(), only records matching the seek key are retrieved
   // r4batchSeek      - enable batching on calls to d4seek(), only records matching the seek key are retrieved
   // r4batchSeekMatch - enable batching on calls to d4seek(), the next 'numRecsToBuf' records are retrieved without
   //                    checking if they match the seek key.
   //
   // note that r4batchSeek and r4batchSeekMatch are mutually exclusive
   // note that you must set all flags you want when calling this function; for example setting the flags to 'r4batchTop'
   // implicitly disables batching on skip, bottom and seek.
   //

   #ifdef E4PARM_HIGH
      if ( data == 0 )
         return error4( 0, e4parm_null, E93316 ) ;
      if ( (flags & r4batchSeek) && (flags & r4batchSeekMatch ) )  // invalid to set both r4batchSeek and r4batchSeekNext
         return error4describe( 0, e4parm, E93316, "invalid flags paramater settig both r4seek and r4seekMatch", 0, 0 ) ;
   #endif

   #ifdef S4CLIENT
      data->readBatchMode = flags ;
   #endif
   return 0 ;
}



/* AS Apr 10/02 - New function for advance-reading client/server */
S4EXPORT long S4FUNCTION d4readBuffer( DATA4 *data, long numRecsToBuf, short doMemos )
{
   #ifdef S4CLIENT
      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E93316 ) ;
         if ( numRecsToBuf < -1 || doMemos < 0 || doMemos > 1 )
            return error4( 0, e4parm, E93316 ) ;
      #endif

      if ( error4code( data->codeBase ) < 0 )
         return e4codeBase ;

      // AS May 2/02 - relax this constraint
      // if ( code4indexFormat( data->codeBase ) != r4cdx )
      //   return error4( data->codeBase, e4notSupported, E90913 ) ;

      if ( numRecsToBuf == -1 )
         return data->batchRead.readRecsToBuf ;

      if ( numRecsToBuf == 0 )
      {
         d4batchReadFree( data ) ;
         return data->batchRead.readRecsToBuf ;
      }

      // AS May 21/02 - New functionality, if numRecsToBuf == 1, doMemos indicates what should happen
      // for memo field transferrance on single record read transfers (d4go, d4top, d4bottom, d4seek, etc.)
      // in this case, only this functionality is affected
      if ( numRecsToBuf == 1 )
      {
         data->includeMemos = 1 ;
         return 0 ;
      }

      if ( data->batchRead.readRecsToBuf != 0 )  // posibly need to reset buffer
      {
         if( data->batchRead.readRecsToBuf < numRecsToBuf )  // if >= just re-use existing buffer
         {
            // in this instance we need to set up the bufferring...
            u4free( data->batchRead.readAdvanceBuf ) ;
            data->batchRead.readAdvanceBuf = 0 ;
         }
      }

      if ( data->batchRead.readAdvanceBuf == 0 )
      {
         int rc = d4batchReadAlloc( data, numRecsToBuf, doMemos ) ;
         if ( rc != 0 )
            return rc ;
      }

      data->batchRead.readRecsToBuf = numRecsToBuf ;
      data->batchRead.doMemos = (unsigned char)doMemos ;
      d4readBufferReset( data ) ;
      return data->batchRead.readRecsToBuf ;
   #else
      // not S4CLIENT
      return 0 ;  // CS 2002/04/24  must always export function
   #endif
}



#ifdef S4CLIENT
   void d4readBufferReset( DATA4 *data )
   {
      // used to reset the read buffer to indicate not-read (but leave buffers in place)
      data->batchRead.readBufNum = 0 ;
      data->batchRead.readBufPos = -1 ;
      data->batchRead.readBufRecNoOn = -1 ;
      data->batchRead.readBufDirection = 0 ;
   }
#endif
