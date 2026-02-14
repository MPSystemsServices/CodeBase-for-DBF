/* d4zap.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifndef S4OFF_WRITE
int S4FUNCTION d4zap( DATA4 *d4, const long r1, const long r2 )
{
   int rc ;
   CODE4 *c4 ;
   #ifdef S4CLIENT
      CONNECTION4 *connection ;
      CONNECTION4ZAP_INFO_IN *info ;
      CONNECTION4ZAP_INFO_OUT *out ;
   #endif

   if ( r2 < r1 )   /* simple no records to remove -- get before parm check */
      return 0 ;

   #ifdef E4PARM_HIGH
      if ( d4 == 0 )
         return error4( 0, e4parm_null, E94604 ) ;
      if ( r1 < 1 || r2 < 1 )
         return error4( d4->codeBase, e4parm, E94604 ) ;
   #endif

   #ifdef E4VBASIC
      if ( c4parm_check( d4, 2, E94604 ) )
         return -1 ;
   #endif

   c4 = d4->codeBase ;
   if ( error4code( c4 ) < 0 )
      return e4codeBase ;

   if ( d4->readOnly == 1 )
      return error4describe( c4, e4write, E80606, d4alias( d4 ), 0, 0 ) ;

   #ifdef S4CLIENT
      connection = d4->dataFile->connection ;
      if ( connection == 0 )
         return e4connection ;

      d4->count = -1 ;
      d4->dataFile->numRecs = -1 ;

      connection4assign( connection, CON4ZAP, data4clientId( d4 ), data4serverId( d4 ) ) ;
      connection4addData( connection, NULL, sizeof( CONNECTION4ZAP_INFO_IN ), (void **)&info ) ;
      info->recStart = htonl5(r1) ;
      info->recStop = htonl5(r2) ;
      rc = connection4repeat( connection ) ;
      if ( rc == r4locked )
         return r4locked ;
      if ( rc < 0 )
         return connection4error( connection, c4, rc, E94604 ) ;

      if ( connection4len( connection ) != sizeof( CONNECTION4ZAP_INFO_OUT ) )
         return error4( c4, e4packetLen, E95702 ) ;
      out = (CONNECTION4ZAP_INFO_OUT *)connection4data( connection ) ;
      if ( out->lockedDatafile  )
         d4->dataFile->fileLock = d4 ;

      if ( rc == 0 )
      {
         if ( d4recCount( d4 ) == 0L )
            d4->bofFlag = d4->eofFlag = 1 ;
         else
            d4->bofFlag = d4->eofFlag = 0 ;
      }

      d4->recNum = -1 ;
      d4->recNumOld = -1 ;
      /* BCR 11/10/00 -- Fields should be set to null as in the non-client case,
         instead of just spaces */
      d4blankLow( d4, d4->record ) ;
      //memset( d4->record, ' ', dfile4recWidth( d4->dataFile ) ) ;

      return rc ;
   #else
      #ifndef S4SINGLE
         rc = d4lockAllInternal( d4, 1 ) ;   /* returns -1 if error4code( codeBase ) < 0 */
         if ( rc )
            return rc ;
      #endif

      #ifndef S4OFF_TRAN
         if ( d4transEnabled( d4, 0 ) )   // always log if data logging is on, not just if 'active'
            if ( tran4active( c4, d4 ) != 0 )
               return error4( d4->codeBase, e4transViolation, E81503 ) ;
      #endif

      rc = d4zapData( d4, r1, r2 ) ;   /* returns -1 if error4code( codeBase ) < 0 */
      if ( rc == 0 )
      {
         if ( d4recCount( d4 ) == 0 )
            d4->bofFlag = d4->eofFlag = 1 ;
         else
            d4->bofFlag = d4->eofFlag = 0 ;

         #ifndef S4INDEX_OFF
            rc = d4reindex( d4 ) ;
         #endif
         dfile4updateHeader( d4->dataFile, 1, 1, 0 ) ;
      }

      return rc ;
   #endif
}

#ifndef S4CLIENT
int d4zapData( DATA4 *data, const long startRec, const long endRec )
{
   int rc ;
   #ifndef S4OFF_TRAN
      #ifndef S4CLIENT
         TRAN4 *trans ;
         long connectionId ;
      #endif
   #endif

   #ifdef E4PARM_HIGH
      if ( data == 0 )
         return error4( 0, e4parm_null, E94605 ) ;
      if ( startRec < 1 || endRec < 1 )
         return error4( data->codeBase, e4parm, E94605 ) ;
   #endif

   #ifndef S4SINGLE
      rc = d4lockFileInternal( data, 1 ) ;
      if ( rc )
         return rc ;
   #endif

   rc = d4update( data ) ;   /* returns -1 if error4code( codeBase ) < 0 */
   if ( rc )
      return rc ;

   #ifndef S4OFF_TRAN
      #ifndef S4CLIENT
         // AS 02/16/01 - base on whether the data file is being logged, not transaction logging
         // if ( code4transEnabled( data->codeBase ) )
         if ( d4transEnabled( data, 0 ) )   // always log if data logging is on, not just if 'active'
         {
            trans = code4trans( data->codeBase ) ;
            #ifdef S4STAND_ALONE
               connectionId = 0L ;
            #else
               connectionId = data->codeBase->currentClient->id ;
            #endif
            rc = tran4set( trans, trans->currentTranStatus, -1L, connectionId, TRAN4ZAP,
                 2 * sizeof(S4LONG), data4clientId( data ), data4serverId( data ) ) ;
            if ( rc < 0 )
               return rc ;
            rc = tran4putData( trans, (void *)&startRec, sizeof( startRec ) ) ;
            if ( rc < 0 )
               return rc ;
            rc = tran4putData( trans, (void *)&endRec, sizeof( endRec ) ) ;
            if ( rc < 0 )
               return rc ;
            rc = tran4lowAppend( trans, 0, 1 ) ;
            if ( rc < 0 )
               return rc ;
         }
      #endif  /* S4CLIENT */
   #endif  /* S4OFF_TRAN */

   rc = dfile4zapData( data->dataFile, startRec, endRec ) ;

   if ( rc < 0 )
      return rc ;

   data->recNum = -1 ;
   data->recNumOld = -1 ;
   d4blankLow( data, data->record ) ;

   return rc ;
}

int dfile4zapData( DATA4FILE *d4, long startRec, long endRec )
{
   long curCount, iRec ;
   char *rdBuf, *wrBuf, *record ;
   FILE4SEQ_READ rd ;
   FILE4LONG len ;
   FILE4SEQ_WRITE wr ;
   unsigned  bufSize ;
   int rc ;
   CODE4 *c4 ;

   #ifdef E4PARM_LOW
      if ( d4 == 0 )
         return error4( 0, e4parm_null, E91102 ) ;
      if ( startRec < 0 || endRec < 0 )
         return error4( d4->c4, e4parm, E91102 ) ;
   #endif

   c4 = d4->c4 ;
   d4->fileChanged = 1 ;

   #ifdef E4ANALYZE
      rdBuf = wrBuf = record = 0 ;
      if ( c4 == 0 )
         return error4( 0, e4parm, E91102 ) ;
   #endif

   if ( startRec == 0 )
      startRec = 1 ;

   curCount = dfile4recCount( d4, -2L ) ;
   if ( curCount < 0 )
      return -1 ;
   if ( startRec > curCount )
      return 0 ;
   if ( endRec < startRec )
      return 0 ;
   if ( endRec > curCount )
      endRec = curCount ;

   rdBuf = wrBuf = 0 ;
   bufSize = c4->memSizeBuffer ;

   record = (char *)u4allocFree( c4, (long)d4->recWidth ) ;
   if ( record == 0 )
      return error4stack( c4, e4memory, E91102 ) ;

   for ( ; bufSize > d4->recWidth ; bufSize -= 0x800 )
   {
      rdBuf = (char *)u4allocFree( c4, (long)bufSize ) ;
      if ( rdBuf == 0 )
         continue ;

      wrBuf = (char *)u4allocFree( c4, (long)bufSize ) ;
      if ( wrBuf )
         break ;

      u4free( rdBuf ) ;
      rdBuf = 0 ;
   }
   file4seqReadInitDo( &rd, &d4->file, dfile4recordPosition( d4, endRec + 1 ), rdBuf, (rdBuf == 0) ? 0 : bufSize, 1 ) ;
   file4seqWriteInitLow( &wr, &d4->file, dfile4recordPosition( d4, startRec ), wrBuf, (wrBuf == 0) ? 0 : bufSize ) ;

   for ( iRec= endRec+1L; iRec <= curCount; iRec++ )
   {
      file4seqReadAll( &rd, record, d4->recWidth ) ;
      file4seqWrite( &wr, record, d4->recWidth ) ;
   }

   file4seqWrite( &wr, "\032", 1 ) ;
   rc = file4seqWriteFlush( &wr ) ;
   #ifdef S4ADVANCE_READ
      file4seqReadInitUndo( &rd ) ;
   #endif
   u4free( rdBuf ) ;
   u4free( wrBuf ) ;
   u4free( record ) ;

   if ( rc < 0 )
      return -1 ;

   d4->numRecs = curCount - ( endRec - startRec + 1 ) ;
   dfile4setMinCount( d4, ( curCount - ( endRec - startRec + 1 ) ) ) ;

   len = dfile4recordPosition( d4, d4->numRecs + 1L ) ;
   file4longAdd( &len, 1 ) ;
   return file4lenSetLow( &d4->file, len ) ;
}
#endif
#endif  /* S4OFF_WRITE */
