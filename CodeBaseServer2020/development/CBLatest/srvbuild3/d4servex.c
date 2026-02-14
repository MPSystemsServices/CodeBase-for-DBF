/* d4servex.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

/* server exported functions */

#include "d4all.h"

#ifdef S4SERVER

#ifndef S4OFF_WRITE
   /* AS 07/21/99 - added parm for win 95/98 to avoid endless laze writes */
   int D4create
   (
      CODE4 *c4,
      const char *name,
      const FIELD4INFO *fieldInfo,
      TAG4INFO *tagInfo,
      DATA4 **data,
      char accessMode,
      char safety,
      int fileFlush,
      short collatingSequence,
      short codePage,
      Collate4name collateName,
      Collate4name collateNameUnicode
   )
   {
      #ifdef E4PARM_LOW
         if ( c4 == 0 )
            return error4( 0, e4parm_null, E70177 ) ;
      #endif

      SERVER4CLIENT *client = c4->currentClient ;

      if ( client->server->doDataCreate == 0 )
         return e4notSupported ;

      #ifndef S4OFF_SECURITY
         if ( account4userAllowCreate( &client->account, c4->createTemp, FALSE ) == FALSE )
            return e4authorize ;
         if ( safety == 0 )
            if ( account4userAllowCreate( &client->account, c4->createTemp, TRUE ) == FALSE )
               safety = 1 ;
      #endif

      #ifndef S4OFF_TRAN
         // AS 09/25/00 - want to log the creation message now...
         // int oldStatus ;
         // if ( name != 0 )
         // {
         //    oldStatus = code4tranStatus( c4 ) ;
         //    code4tranStatusSet( c4, r4off ) ;
         // }
      #endif

      int oldAccessMode = c4->accessMode ;
      int oldSafety = c4->safety ;
      int oldFileFlush = c4->fileFlush ;
      #ifdef S4FOX
         short oldCollatingSequence = c4->collatingSequence ;
         short oldCodePage = c4->codePage ;
         Collate4name oldCollateName = c4->collateName ;
         Collate4name oldCollateNameUnicode = c4->collateNameUnicode ;
      #endif

      c4->accessMode = accessMode ;
      c4->safety = safety ;
      c4->fileFlush = fileFlush ;
      #ifdef S4FOX
         c4->collateNameUnicode = collateNameUnicode ;
         c4->collateName = collateName ;
         c4->codePage = codePage ;
         c4->collatingSequence = collatingSequence ;
      #endif

      // AS Jan 20/03 - There are now other cases where the compatibility gets bumped up, so save it always
      #ifdef S4FOX
         int oldCompatibility = c4->compatibility ;

         #if defined( S4PREPROCESS_FILE )
            // preprocessing only supported with 0x30 mode, set if not already done...
            // AS Jul 16/03 - if we are not actually preprocessing this file, don't up the compatibility
            if ( code4getPreprocessFile( c4 ) == 1 )
               c4->compatibility = 30 ;
         #endif
      #endif

      int rc = dfile4create( c4, name, fieldInfo, tagInfo, data ) ;

      #ifdef S4FOX
         if ( rc == e4compatibility && c4->compatibility < 30 && error4code( c4 ) == 0 )  // Jan 20/03 maybe a compatibility problem, returns 0 but does not fail in that case
         {
            c4->compatibility = 30 ;
            rc = dfile4create( c4, name, fieldInfo, tagInfo, data ) ;
         }

         // AS Jan 20/03 - There are now other cases where the compatibility gets bumped up, so save it always
         c4->compatibility = oldCompatibility ;
      #endif

      #ifdef S4FOX
         c4->collateNameUnicode = oldCollateNameUnicode ;
         c4->collateName = oldCollateName ;
         c4->codePage = oldCodePage ;
         c4->collatingSequence = oldCollatingSequence ;
      #endif
      c4->fileFlush = oldFileFlush ;
      c4->safety = oldSafety ;
      c4->accessMode = oldAccessMode ;

      #ifndef S4OFF_TRAN
         // AS 09/25/00 - want to log the creation message now...
         // if ( name != 0 )
         //    code4tranStatusSet( c4, oldStatus ) ;
      #endif

      if ( rc < 0 )
         return rc ;

      return 0 ;
   }



   DATA4 *S4FUNCTION D4createOpen
   (
      CODE4 *c4,
      const char *name,
      const FIELD4INFO *fieldInfo,
      TAG4INFO *tagInfo,
      char accessMode,
      char safety,
      unsigned short compatibility,
      int fileFlush,
      short collatingSequence,
      short codePage,
      Collate4name collateName,
      Collate4name collateNameUnicode
   )
   {
      DATA4 *data = 0 ;

      if ( D4create( c4, name, fieldInfo, tagInfo, name == 0 ? &data : 0 , accessMode, safety,
           fileFlush, collatingSequence, codePage, collateName, collateNameUnicode ) != 0 )
         return 0 ;

      if ( data )
         return data ;

      return D4open( c4, name, accessMode, c4->errDefaultUnique, c4->log, 1, fileFlush, c4getReadOnly( c4 ), 0, compatibility ) ;
   }


   // AS May 17/04 - client functionality to compress the data file...
   int D4compress( DATA4 *data, const char *compressedTableName, short blockSize, char safety )
   {
      CODE4 *c4 = data->codeBase ;
      SERVER4CLIENT *client = c4->currentClient ;

      if ( client->server->doDataCreate == 0 )
         return e4notSupported ;

      #ifndef S4OFF_SECURITY
         if ( account4userAllowCreate( &client->account, c4->createTemp, FALSE ) == FALSE )
            return e4authorize ;
         if ( safety == 0 )
            if ( account4userAllowCreate( &client->account, c4->createTemp, TRUE ) == FALSE )
               safety = 1 ;
      #endif

      int rc = 0 ;
      // if table is open but safety is off (keep open flag on), then close it...
      if ( safety == 0 )
      {
         char buf[258] ;
         format4createName( buf, sizeof( buf ), compressedTableName ) ;
         rc = dfile4verifyNotAlreadyOpen( c4, buf ) ;
      }

      if ( rc == 0 )
      {
         char oldSafety = c4getSafety( c4 ) ;
         c4setSafety( c4, safety ) ;
         DATA4 *compressedTable = d4compress( data, compressedTableName, blockSize ) ;
         c4setSafety( c4, oldSafety ) ;
         rc = error4code( c4 ) ;
         if ( compressedTable != 0 )
         {
            // ensure the table is actually physically closed so that the compression buffers can be set up properly on a re-open
            int oldKeepOpen = c4->server->keepOpen ;
            c4->server->keepOpen = 0 ;
            d4close( compressedTable ) ;
            c4->server->keepOpen = oldKeepOpen ;
         }
         else
            if ( error4code( c4 ) == 0 )
               rc = -1 ;
      }
      return rc ;
   }
#endif /* !S4OFF_WRITE */



#ifndef S4OFF_WRITE
   int D4remove( DATA4 *data )
   {
      CODE4 *c4 = data->codeBase ;
      SERVER4CLIENT *client = c4->currentClient ;

      #ifndef S4OFF_SECURITY
         if ( file4getTemporary( &data->dataFile->file ) == 0 )  // don't disallow removal of temp files
            if ( account4userAllowCreate( &client->account, 0, TRUE ) == FALSE )
               return e4authorize ;
      #endif

      int rc ;
      DATA4FILE *dfile = data->dataFile ;
      if ( dfile->userCount == 1 )
      {
         int oldKeepOpen = client->server->keepOpen ;
         client->server->keepOpen = 1 ;
         #ifdef S4CLIPPER
            // AS 11/27/00 - doRemove must be set on client structure
            // c4->doRemove = 1 ;
            c4setDoRemove( c4, 1 ) ;
         #endif

         #ifndef S4OFF_COMMUNICATIONS
            /* AS 11/04/98 -- changed, shouldn't need to call back to server4client function here */
            /* AS 02/09/99 - the clientId not necessarily corresponding like this.  clientId is the client's
               id on the DATA4, which isn't used for OLE-DB */
            //assert5( data->clientId == connection4clientId( &client->connection ) ) ;
         #endif /* not S4OFF_COMMUNICATIONS */

         rc = D4close( data ) ;
         client->server->keepOpen = oldKeepOpen ;
         /* AS 07/22/99 changed, was accessing data->dataFile instead of dfile on next line - but data may be NULL at this point */

         #ifdef S4CLIPPER
            // AS 11/27/00 - doRemove must be set on client structure
            // c4->doRemove = 0 ;
            // AS Dec 15/03 - this has changed now, we leave the remove off and call dfile4remove() explicitly.
            c4setDoRemove( c4, 0 ) ;
         #else
            // AS Dec 16/03 - clipper does automatically
            if ( rc == 0 && file4getTemporary( &dfile->file ) == 0 ) /* if file is temp, removed automatically */
               rc = dfile4remove( dfile ) ;
         #endif
      }
      else
         rc = r4open ;

      return rc ;
   }
#endif /* !S4OFF_WRITE */


#ifndef S4OFF_WRITE
   int D4indexesRemove( DATA4 *data )
   {
      #ifndef S4OFF_SECURITY
         if ( file4getTemporary( &data->dataFile->file ) == 0 )  // don't disallow removal of temp files
            if ( account4userAllowCreate( &data->codeBase->currentClient->account, 0, TRUE ) == FALSE )
               return e4authorize ;
      #endif

      return d4indexesRemove( data ) ;
   }
#endif /* !S4OFF_WRITE */



#ifndef S4OFF_WRITE
   DATA4 *D4fieldsRemove( DATA4 **data, int nFields, char *colName )
   {
      #ifndef S4OFF_SECURITY
         if ( file4getTemporary( &((*data)->dataFile->file )) == 0 )  // don't disallow removal of temp files
            if ( account4userAllowCreate( &((*data)->codeBase->currentClient->account), 0, TRUE ) == FALSE )
            {
               error4( (*data)->codeBase, e4authorize, E91530 ) ;
               return 0 ;
            }
      #endif

      return d4fieldsRemove( data, nFields, &colName ) ;
   }
#endif /* !S4OFF_WRITE */



long D4recCount( DATA4 *data )
{
   #ifndef S4OFF_SECURITY
      if ( account4userAllowRead( &data->codeBase->currentClient->account, data ) == FALSE )
         return e4authorize ;
   #endif

   return d4recCount( data ) ;
}



int D4close( DATA4 *data )
{
   return d4close( data ) ;
}



int D4bottom( DATA4 *data )
{
   #ifndef S4OFF_SECURITY
      if ( account4userAllowRead( &data->codeBase->currentClient->account, data ) == FALSE )
         return e4authorize ;
   #endif

   return d4bottom( data ) ;
}



double D4position( DATA4 *data )
{
   #ifndef S4OFF_SECURITY
      if ( account4userAllowRead( &data->codeBase->currentClient->account, data ) == FALSE )
         return e4authorize ;
   #endif

   return d4position( data ) ;
}



int D4positionSet( DATA4 *data, double pos )
{
   #ifndef S4OFF_SECURITY
      if ( account4userAllowRead( &data->codeBase->currentClient->account, data ) == FALSE )
         return e4authorize ;
   #endif

   return d4positionSet( data, pos ) ;
}



#ifndef S4OFF_WRITE
   int D4reindex( DATA4 *data )
   {
      if ( data->codeBase->currentClient->server->doReindex == 0 )
         return e4notSupported ;

      #ifndef S4OFF_SECURITY
         if ( account4userAllowCompress( &data->codeBase->currentClient->account, data ) == FALSE )
            return e4authorize ;
      #endif

      if ( data->readOnly == 1 )
         return error4( data->codeBase, e4info, E80606 ) ;

      return d4reindex( data ) ;
   }



   int D4pack( DATA4 *data )
   {
      if ( data->codeBase->currentClient->server->doPack == 0 )
         return e4notSupported ;

      #ifndef S4OFF_SECURITY
         if ( account4userAllowCompress( &data->codeBase->currentClient->account, data ) == FALSE )
            return e4authorize ;
      #endif

      if ( data->readOnly == 1 )
         return error4( data->codeBase, e4info, E80606 ) ;

      return d4pack( data ) ;
   }
#endif /* !S4OFF_WRITE */



int D4top( DATA4 *data )
{
   #ifndef S4OFF_SECURITY
      if ( account4userAllowRead( &data->codeBase->currentClient->account, data ) == FALSE )
         return e4authorize ;
   #endif

   return d4top( data ) ;
}



#ifndef S4OFF_WRITE
   int D4memoCompress( DATA4 *data )
   {
      #ifndef S4OFF_SECURITY
         if ( account4userAllowCompress( &data->codeBase->currentClient->account, data ) == FALSE )
            return e4authorize ;
      #endif

      return d4memoCompress( data ) ;
   }
#endif /* !S4OFF_WRITE */



#ifndef S4OFF_WRITE
   int D4zap( DATA4 *data, long start, long end )
   {
      #ifndef S4OFF_SECURITY
         if ( account4userAllowDelete( &data->codeBase->currentClient->account, data ) == FALSE )
            return e4authorize ;
         if ( account4userAllowCompress( &data->codeBase->currentClient->account, data ) == FALSE )
            return e4authorize ;
      #endif

      return d4zap( data, start, end ) ;
   }
#endif /* !S4OFF_WRITE */



DATA4 *S4FUNCTION D4open
(
   CODE4 *c4,
   const char *name,
   short accessMode,
   short errDefaultUnique,
   short doLog,
   short openForCreate,
   short fileFlush, /* AS 07/21/99 - added for win 95/98 to avoid endless laze writes */
   int readOnly,
   unsigned short singleOpen,
   unsigned short compatibility
)
{
   switch( errDefaultUnique )
   {
      case r4unique:
      case r4uniqueContinue:
      case e4unique:
         break ;
      default:
         error4( c4, e4unique, E81718 ) ;
         return 0 ;
   }

   int oldAccessMode = c4->accessMode ;
   int oldLog = c4->log ;

   if ( c4->log == LOG4ON )   /* means to use client setting */
      c4->log = doLog ;
   #ifdef S4TESTING
      // AS 02/26/01 - allow to turn logging off of 't4suite'
      if ( doLog == 0 )
      {
         // AS Sept 3/02 - Also ok if andrew.t
         if ( strnicmp( name, "h:\\codebase.t", 13 ) == 0 || strnicmp( name, "h:\\andrew.t", 11 ) == 0  )  // in that directory, so ignore
            c4->log = 0 ;
      }
   #endif

   int oldSingleClient = c4->singleClient ;
   int oldUniqueError = c4->errDefaultUnique ;
   int oldFileFlush = c4->fileFlush ;

   switch( c4->accessMode )
   {
      case OPEN4DENY_NONE:
         c4->accessMode = (char)accessMode ;
         break ;
      case OPEN4DENY_WRITE:
         if ( accessMode == OPEN4DENY_RW )
            c4->accessMode = (char)accessMode ;
         break ;
      default:
         break ;
   }
   c4->singleClient = accessMode ;
   c4->errDefaultUnique = errDefaultUnique ;
   c4->readOnlyRequest = readOnly ;
   c4->fileFlush = fileFlush ;

   #ifdef S4CLIPPER
      int oldAutoOpen = c4->autoOpen ;
      if ( openForCreate == 2 )  /* means no tag info, so don't open index file on create */
         c4->autoOpen = 0 ;
   #endif

   DATA4 *data = server4open( c4->currentClient->server, c4->currentClient, name, singleOpen, compatibility ) ;
   #ifdef S4CLIPPER
      c4->autoOpen = oldAutoOpen ;
   #endif

   c4->fileFlush = oldFileFlush ;
   c4->log = oldLog ;
   c4->errDefaultUnique = oldUniqueError ;
   c4->singleClient = oldSingleClient ;
   c4->accessMode = oldAccessMode ;
   c4->readOnlyRequest = 0 ;
   if ( data == 0 )
   {
      if ( error4code( c4 ) == 0 )
         error4set( c4, e4name ) ;
      return 0 ;
   }

   if ( readOnly == 1 )
      data->readOnly = 1 ;
   data->accessMode = accessMode ;

   return data ;
}



int D4seekN( DATA4 *data, TAG4 *tag, const char *str, const short len, unsigned short int fromCurrentPos )
{
   #ifndef S4OFF_SECURITY
      if ( account4userAllowRead( &data->codeBase->currentClient->account, data ) == FALSE )
         return e4authorize ;
   #endif

   d4tagSelect( data, tag ) ;

   if ( fromCurrentPos == 1 )
      return d4seekNextN( data, str, len ) ;
   else
      return d4seekN( data, str, len ) ;
}



int D4seekDouble( DATA4 *data, TAG4 *tag, const double dkey, unsigned short int fromCurrentPos )
{
   #ifndef S4OFF_SECURITY
      if ( account4userAllowRead( &data->codeBase->currentClient->account, data ) == FALSE )
         return e4authorize ;
   #endif

   d4tagSelect( data, tag ) ;

   if ( fromCurrentPos == 1 )
      return d4seekNextDouble( data, dkey ) ;
   else
      return d4seekDouble( data, dkey ) ;
}



/* AS Sep 7/11 c/s support for seek long long */
int D4seekLongLong( DATA4 *data, TAG4 *tag, const LONGLONG dkey, unsigned short int fromCurrentPos )
{
   #ifndef S4OFF_SECURITY
      if ( account4userAllowRead( &data->codeBase->currentClient->account, data ) == FALSE )
         return e4authorize ;
   #endif

   d4tagSelect( data, tag ) ;

   if ( fromCurrentPos == 1 )
      return d4seekNextLongLong( data, dkey ) ;
   else
      return d4seekLongLong( data, dkey ) ;
}



int D4tagSync( DATA4 *data, TAG4 *tag )
{
   #ifndef S4OFF_SECURITY
      if ( account4userAllowRead( &data->codeBase->currentClient->account, data ) == FALSE )
         return e4authorize ;
   #endif

   return d4tagSync( data, tag ) ;
}



int D4skip( DATA4 *data, TAG4 *tag, const long nSkip, short int *outSkipRc, long startPos )
{
   #ifndef S4OFF_SECURITY
      if ( account4userAllowRead( &data->codeBase->currentClient->account, data ) == FALSE )
         return e4authorize ;
   #endif

   int rc = 0 ;

   d4tagSelect( data, tag ) ;

   if ( tag == 0 )
   {
      if ( nSkip != 0L )
         if ( d4recCountLessEq( data, 1L ) == 0 )  /* count == 0 */
         {
            if ( nSkip < 0L )
               rc = d4goBof( data ) ;
            else
               rc = d4goEof( data ) ;
         }
   }

   long rec ;

   if ( rc == 0 )
   {
      if ( startPos > data->dataFile->minCount )
      {
         if ( d4recCountLessEq( data, startPos ) == 0 )
         {
            rc = d4goEof( data ) ;
            if ( rc == r4eof )
               rc = 0 ;
         }
         else
            if ( data->recNum != startPos )
               rc = d4go( data, startPos ) ;
      }
      else
         if ( data->recNum != startPos )
            rc = d4go( data, startPos ) ;

      if ( rc == 0 )
      {
         rec = d4skipRecno( data, nSkip ) ;
         if ( rec <= 0 )
         {
            if ( rec == -2L )  /* eof signal */
            {
               // AS 09/13/99 fixed problem... if tag is empty, r4eof is returned and bof flag is set to 1
               // change this case to r4noRecords...
               if ( tag != 0 && data->bofFlag == 1 )
                  rc = r4noRecords ;
               else
                  rc = r4eof ;
            }
            else if ( rec == -3L )  /* bof signal */
               rc = r4bof ;
            else if ( rec == -4L )  /* bof/eof signal */
               rc = r4noRecords ;
            else if ( rec < 0L )
               rc = (int)rec ;
         }
      }
   }

   *outSkipRc = rc ;
   if ( rc == 0 )
   {
      rc = d4go( data, rec ) ;
      /* must synch up if unique tag due to possible transactions yielding
         multiple entries for the same record */
      if ( rc == 0 && tag != 0 )
         if ( t4unique( tag ) != 0 )
            rc = d4tagSyncDo( data, tag, nSkip > 0 ? 1 : -1 ) ;
   }

   return rc ;
}



int D4unlock( DATA4 *data )
{
   // AS Apr 15/03 - support for new lockId for shared clone locking
   return d4unlockLow( data, data4lockId( data ), 0 ) ;
}



#if !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE )
   int I4reindex( INDEX4 *index )
   {
      #ifndef S4OFF_SECURITY
         if ( account4userAllowCompress( &index->data->codeBase->currentClient->account, index->data ) == FALSE )
            return e4authorize ;
      #endif

      return i4reindex( index ) ;
   }


   int S4FUNCTION I4tagAdd( INDEX4 *i4, TAG4INFO *tag )
   {
      #ifndef S4OFF_SECURITY
         if ( account4userAllowIndex( &i4->data->codeBase->currentClient->account, i4->data ) == FALSE )
            return e4authorize ;
      #endif

      // AS 11/28/00 - made i4tagAdd available in clipper (for odbc)
      // #ifdef S4CLIPPER
      //    if ( i4create( i4->data, NULL, tag ) == NULL )
      //       return -1 ;
      //    return 0 ;
      // #else
         return i4tagAdd( i4, tag ) ;
      // #endif
   }
#endif /* !S4OFF_WRITE */



int D4check( DATA4 *data )
{
   #ifndef S4OFF_SECURITY
      if ( account4userAllowCompress( &data->codeBase->currentClient->account, data ) == FALSE )
         return e4authorize ;
   #endif

   return d4check( data ) ;
}



#ifndef S4OFF_WRITE
   int D4write( DATA4 *d4, long recIn, const int unlock, const int doLock )
   {
      #ifndef S4OFF_SECURITY
         if ( account4userAllowUpdate( &d4->codeBase->currentClient->account, d4 ) == FALSE )
            return e4authorize ;
         if ( d4->record[0] == '*' && d4->recordOld[0] == ' ' )  // attempt to delete
            if ( account4userAllowDelete( &d4->codeBase->currentClient->account, d4 ) == FALSE )
               return e4authorize ;
      #endif
      int rc = 0 ;

      if ( recIn == -1 )  // means use current record
         recIn = d4recNo( d4 ) ;

      if ( d4lockTest( d4, recIn, lock4write ) != 1 )
      {
         // AS May 24/06 - We need to unlock as well...t4lock.c update
         // the problem is that otherwise the records do not get unlocked (when client calls server4clientWrite)
         // rc = d4lockInternal( d4, recIn, 0 ) ;
         rc = d4lockInternal( d4, recIn, 1 ) ;
         if ( rc != 0 )
            return rc ;
      }

      if ( rc == 0 )
         rc = d4writeLow( d4, recIn, unlock, doLock ) ;

      return rc ;
   }



   int D4append( DATA4 *data, long clientId, long serverId )
   {
      #ifndef S4OFF_SECURITY
         if ( account4userAllowAppend( &data->codeBase->currentClient->account, data ) == FALSE )
            return e4authorize ;
      #endif

      int rc = d4lockAppendRecord( data, 0 ) ;
      if ( rc == 0 )
         rc = d4append( data ) ;
      return rc ;
   }
#endif /* !S4OFF_WRITE */


/*
int D4appendOledb( DATA4 *data, long clientId, long serverId )
{
   // AS 09/22/99 -- OLE-DB specific more efficient function - no locking...
   #ifndef S4OFF_SECURITY
      if ( account4userAllowAppend( &data->codeBase->currentClient->account, data ) == FALSE )
         return e4authorize ;
   #endif

   return d4appendOledb( data ) ;
}
*/



int Server4clientTranAddUser( SERVER4CLIENT *client, long version, const long connectionId, const char *userName, const unsigned short int nameLen, const char *pass, TCP4ADDRESS tcpAddress )
{
   // CS 2002/06/18 remove version checking
   int rc ;
   #ifndef S4OFF_SECURITY
      rc = account4userInit( &client->account, &client->server->accounts, &client->server->privileges, tcpAddress, userName, pass ) ;
      if ( rc < 0 )
         return rc ;
   #endif  /* S4OFF_SECURITY */

   #ifndef S4OFF_TRAN
      if ( client->transEnabled != 1 )
      {
         rc = tran4addUser( &client->trans, connectionId, userName, nameLen ) ;
         if ( rc >= 0 )
            client->transEnabled = 1 ;
      }
   #endif

   return 0 ;
}



int Client4unlock( SERVER4CLIENT *client )
{
   int saveRc = 0 ;

   for ( DATA4 *data = 0 ;; )
   {
      data = (DATA4 *)l4next( tran4dataList( &client->trans ), data ) ;
      if ( data == 0 )
         break ;
      long saveId = data->clientId ;
      data->clientId = 0 ;
      int rc = D4unlock( data ) ;
      data->clientId = saveId ;
      if ( rc != 0 )
         saveRc = rc ;
   }

   return saveRc ;
}
#endif /* S4SERVER */
