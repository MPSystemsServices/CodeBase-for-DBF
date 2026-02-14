/* d4pack.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifndef S4OFF_WRITE
int S4FUNCTION d4pack( DATA4 *d4 )
{
   #ifdef S4CLIENT
      int rc ;
      CONNECTION4PACK_INFO_OUT *out ;
      CONNECTION4 *connection ;
   #else
      int rc ;
   #endif
   CODE4 *c4 ;

   #ifdef E4VBASIC
      if ( c4parm_check( d4, 2, E94601 ) )
         return 0 ;
   #endif

   #ifdef E4PARM_HIGH
      if ( d4 == 0 )
         return error4( 0, e4parm_null, E94601 ) ;
   #endif

   c4 = d4->codeBase ;

   if ( error4code( c4 ) < 0 )
      return e4codeBase ;

   if ( d4->readOnly == 1 )
      return error4describe( c4, e4write, E80606, d4alias( d4 ), 0, 0 ) ;

   #ifdef S4CLIENT
      rc = d4update( d4 ) ;   /* returns -1 if error4code( codeBase ) < 0 */
      if ( rc )
         return rc ;

      connection = d4->dataFile->connection ;
      if ( connection == 0 )
         return error4stack( c4, e4connection, E94601 ) ;

      d4->count = -1 ;
      d4->dataFile->numRecs = -1 ;
      connection4assign( connection, CON4PACK, data4clientId( d4 ), data4serverId( d4 ) ) ;
      rc = connection4repeat( connection ) ;
      if ( rc == r4locked )
         return r4locked ;
      if ( rc < 0 )
         return connection4error( connection, c4, rc, E94601 ) ;

      if ( connection4len( connection ) != sizeof( CONNECTION4PACK_INFO_OUT ) )
         return error4( c4, e4packetLen, E94601 ) ;
      out = (CONNECTION4PACK_INFO_OUT *)connection4data( connection ) ;
      if ( out->lockedDatafile )
         d4->dataFile->fileLock = d4 ;

      d4->recNum = -1 ;
      d4->recNumOld = -1 ;
      memset( d4->record, ' ', dfile4recWidth( d4->dataFile ) ) ;

      return rc ;
   #else
      #ifndef S4SINGLE
         rc = d4lockAllInternal( d4, 1 ) ;   /* returns -1 if error4code( codeBase ) < 0 */
         if ( rc )
            return rc ;
      #endif

      #ifndef S4OFF_TRAN
         if ( d4transEnabled( d4, 0 ) )   // always log if data logging is on, not just if 'active'
            if ( tran4active( d4->codeBase, d4 ) != 0 )
               return error4( c4, e4transViolation, E81518 ) ;
      #endif

      rc = d4packData( d4 ) ;  /* returns -1 if error4code( codeBase ) < 0 */
      if ( rc == 0 )
      {
         if ( d4recCount( d4 ) == 0 )
            d4->bofFlag = d4->eofFlag = 1 ;
         else
            d4->bofFlag = d4->eofFlag = 0 ;
         #ifndef S4INDEX_OFF
            rc = d4reindex( d4 ) ;
         #endif
         if ( rc == 0 )
            dfile4updateHeader( d4->dataFile, 1, 1, 0 ) ;
      }

      return rc ;
   #endif  /* S4CLIENT */
}

#ifndef S4CLIENT
int d4packData( DATA4 *d4 )
{
   int rc ;
   #ifndef S4CLIENT
      #ifndef S4OFF_TRAN
         TRAN4 *trans ;
         long connectionId ;
      #endif
   #endif

   #ifdef E4PARM_HIGH
      if ( d4 == 0 )
         return error4( 0, e4parm_null, E94602 ) ;
   #endif

   rc = d4update( d4 ) ;   /* returns -1 if error4code( codeBase ) < 0 */
   if ( rc )
      return rc ;

   #ifndef S4SINGLE
      rc = d4lockFileInternal( d4, 1 ) ;
      if ( rc )
         return rc ;
   #endif

   #ifndef S4CLIENT
      #ifndef S4OFF_TRAN
         // AS 02/16/01 - base on whether the data file is being logged, not transaction logging
         // if ( code4transEnabled( data->codeBase ) )
         if ( d4transEnabled( d4, 0 ) )   // always log if data logging is on, not just if 'active'
         {
            trans = code4trans( d4->codeBase ) ;
            #ifdef S4STAND_ALONE
               connectionId = 0L ;
            #else
               connectionId = d4->codeBase->currentClient->id ;
            #endif
            if (  tran4set( trans, trans->currentTranStatus, -1L, connectionId, TRAN4PACK,
                 0, data4clientId( d4 ), data4serverId( d4 ) ) == 0 )
               tran4lowAppend( trans, "\0", 1 ) ;
         }
      #endif
   #endif

   rc = dfile4packData( d4->dataFile ) ;

   d4->recNum = -1 ;
   d4->recNumOld = -1 ;
   c4memset( d4->record, ' ', dfile4recWidth( d4->dataFile ) ) ;

   return rc ;
}

int dfile4packData( DATA4FILE *d4 )
{
   char *rdBuf, *wrBuf, *record ;
   FILE4SEQ_READ   rd ;
   FILE4SEQ_WRITE  wr ;
   long newCount, curCount, iRec ;
   int  rc ;
   unsigned bufSize ;
   CODE4 *c4 ;
   FILE4LONG len ;

   #ifdef E4PARM_LOW
      if ( d4 == 0 )
         return error4( 0, e4parm_null, E91102 ) ;
   #endif

   c4 = d4->c4 ;

   d4->fileChanged = 1 ;

   rdBuf = wrBuf = 0 ;
   bufSize = c4->memSizeBuffer ;

   for ( ; bufSize > d4->recWidth; bufSize -= 0x800 )
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

   newCount = 0L ;
   curCount = dfile4recCount( d4, -2L ) ;

   record = (char *)u4allocFree( c4, (long)d4->recWidth ) ;
   if ( record == 0 )
      return -1 ;

   file4seqReadInitDo( &rd, &d4->file, dfile4recordPosition( d4, 1L ), rdBuf, ( rdBuf == 0 ) ? 0 : bufSize, 1 ) ;
   file4seqWriteInitLow( &wr, &d4->file, dfile4recordPosition( d4, 1L ), wrBuf, ( wrBuf == 0 ) ? 0 : bufSize) ;

   for ( iRec= 1L; iRec <= curCount; iRec++ )
   {
      file4seqReadAll( &rd, record, d4->recWidth ) ;
      if ( record[0] == ' ' )
      {
         file4seqWrite( &wr, record, d4->recWidth ) ;
         newCount++ ;
      }
   }

   u4free( record ) ;

   file4seqWrite( &wr, "\032", 1 ) ;
   rc = file4seqWriteFlush( &wr ) ;
   #ifdef S4ADVANCE_READ
      file4seqReadInitUndo( &rd ) ;
   #endif
   u4free( rdBuf ) ;
   u4free( wrBuf ) ;
   if ( rc < 0 )
      return -1 ;

   d4->numRecs = newCount ;
   dfile4setMinCount( d4, newCount ) ;

   len = dfile4recordPosition( d4, newCount + 1L ) ;
   file4longAdd( &len, 1 ) ;
   return file4lenSetLow( &d4->file, len ) ;
}
#endif

#endif  /* S4OFF_WRITE */
