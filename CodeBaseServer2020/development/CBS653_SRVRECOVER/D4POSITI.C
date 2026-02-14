/* d4positi.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

double S4FUNCTION d4position( DATA4 *data )
{
   #ifdef S4CLIENT
      int rc ;
      CONNECTION4 *connection ;
      CONNECTION4DATA_POS_IN *info ;
   #else
      long count ;
      #ifndef S4OFF_INDEX
         int len, rc ;
         unsigned char *result ;
      #endif
   #endif

   #ifndef S4OFF_INDEX
      TAG4 *tag ;
   #endif

   #ifdef E4VBASIC
      if ( c4parm_check( data, 2, E94701 ) )
         return -1.0 ;
   #endif

   #ifdef E4PARM_HIGH
      if ( data == 0 )
         return (double)error4( 0, e4parm_null, E94701 ) ;
   #endif

   if( error4code( data->codeBase ) < 0 )
      return -1.0 ;

   if ( d4eof( data ) )
      return 1.1 ;

   #ifdef S4CLIENT
      connection = data->dataFile->connection ;
      if ( connection == 0 )
         return e4connection ;

      rc = connection4assign( connection, CON4POSITION, data4clientId( data ), data4serverId( data ) ) ;
      if ( rc < 0 )
         return rc ;
      connection4addData( connection, NULL, sizeof( CONNECTION4DATA_POS_IN ), (void **)&info ) ;
      #ifdef S4OFF_INDEX
         info->usesTag = 0 ;
      #else
         tag = data->tagSelected ;
         if ( tag == 0 )
            info->usesTag = 0 ;
         else
         {
            info->usesTag = 1 ;
            memcpy( info->tagName, tag->tagFile->alias, LEN4TAG_ALIAS  ) ;
            info->tagName[LEN4TAG_ALIAS] = 0 ;
         }
      #endif
      info->startRecno = htonl5(data->recNum) ;
      connection4sendMessage( connection ) ;
      rc = connection4receiveMessage( connection ) ;
      if ( rc < 0 )
         return error4stack( data->codeBase, rc, E94701 ) ;
      rc = connection4status( connection ) ;
      if ( rc < 0 )
         return connection4error( connection, data->codeBase, rc, E94701 ) ;
      if ( connection4len( connection ) != sizeof( CONNECTION4DATA_POS_OUT ) )
         return error4( data->codeBase, e4packetLen, E94701 ) ;
      return(ntohd( ((CONNECTION4DATA_POS_OUT *)connection4data( connection ))->position ) ) ;
   #else
      #ifndef S4OFF_INDEX
         tag = data->tagSelected ;
         if ( tag == 0 || data->recNum <= 0L )
         {
      #endif
         count = d4recCount( data ) ;
         if ( count < 0 )
            return -1.0 ;
         if ( count == 0 || data->recNum <= 0L )
            return 0.0 ;

         return (double)( data->recNum - 1 ) / ( count - (count != 1 ) ) ;
      #ifndef S4OFF_INDEX
         }
         else
         {
            expr4context( tag->tagFile->expr, data ) ;
            len = tfile4exprKey( tag->tagFile, &result ) ;
            if ( len < 0 )
               return -1.0 ;
            t4versionCheck( tag, 0, 0 ) ;
            rc = tfile4go( tag->tagFile, result, data->recNum, 0 ) ;
            if ( rc != 0  && rc != r4eof && rc != r4after )
               return -1.0 ;
            return tfile4position( tag->tagFile ) ;
         }
      #endif
   #endif
}

int S4FUNCTION d4position2( DATA4 *data, double *result )
{
   *result = d4position( data ) ;
   if ( *result < 0.0 )
      return -1;
   return 0;
}

#ifdef S4SERVER
long S4FUNCTION d4positionSet( DATA4 *data, const double per )
#else
int S4FUNCTION d4positionSet( DATA4 *data, const double per )
#endif
{
   int rc ;
   CODE4 *c4 ;
   #ifdef S4CLIENT
      CONNECTION4 *connection ;
      CONNECTION4DATA_POS_SET_IN *info ;
      CONNECTION4GO_INFO_OUT *out ;
      long recNo ;
   #else
      long newRec ;
      long count ;
   #endif
   #ifndef S4OFF_INDEX
      TAG4 *tag ;
   #endif

   #ifdef E4VBASIC
      if ( c4parm_check( data, 2, E94702 ) )
         return -1 ;
   #endif

   #ifdef E4PARM_HIGH
      if ( data == 0 )
         return error4( 0, e4parm_null, E94702 ) ;
   #endif

   c4 = data->codeBase ;
   if( error4code( c4 ) < 0 )
      return e4codeBase ;

   if ( per > 1.0 )
      return d4goEof( data ) ;
   if ( per <= 0 )
      return d4top( data ) ;

   #ifdef S4CLIENT
      connection = data->dataFile->connection ;
      if ( connection == 0 )
         return e4connection ;

      rc = connection4assign( connection, CON4POSITION_SET, data4clientId( data ), data4serverId( data ) ) ;
      if ( rc < 0 )
         return error4stack( c4, rc, E94702 ) ;
      connection4addData( connection, NULL, sizeof( CONNECTION4DATA_POS_SET_IN ), (void **)&info ) ;
      info->position = htond(per) ;
      #ifdef S4OFF_INDEX
         info->usesTag = 0 ;
      #else
         tag = data->tagSelected ;
         if ( tag == 0 )
            info->usesTag = 0 ;
         else
         {
            info->usesTag = 1 ;
            memcpy( info->tagName, tag->tagFile->alias, LEN4TAG_ALIAS  ) ;
            info->tagName[LEN4TAG_ALIAS] = 0 ;
         }
      #endif
      rc = connection4repeat( connection ) ;
      if ( rc == r4locked )
         return r4locked ;
      if ( rc < 0 )
         return connection4error( connection, c4, rc, E94702 ) ;
      if ( connection4len( connection ) < sizeof( CONNECTION4GO_INFO_OUT ) )
         return error4( c4, e4packetLen, E94702 ) ;
      out = (CONNECTION4GO_INFO_OUT *)connection4data( connection ) ;
      recNo = ntohl5(out->recNo) ;
      if ( recNo )
      {
         if ( (long)connection4len( connection ) != (long)sizeof( CONNECTION4GO_INFO_OUT ) + (long)dfile4recWidth( data->dataFile ) )
            return error4( c4, e4packetLen, E94802 ) ;
         return d4goVirtual( data, recNo, rc, out, connection, 0 ) ;  /* maybe r4locked, or whatever */
      }
      else
         return d4goEof( data ) ;
   #else
      #ifndef S4OFF_INDEX
         tag = data->tagSelected ;
         if ( tag == 0 )
         {
      #endif
         count = d4recCount( data ) ;
         if ( count <= 0L )
            return d4goEof( data ) ;

         newRec = (long)( per * ( (double)count - 1 ) + 1.5 ) ;
         if ( newRec > count )
            newRec = count ;
      #ifndef S4OFF_INDEX
         }
         else
         {
            rc = tfile4positionSet( tag->tagFile, per ) ;
            if ( rc )
               return rc ;
            if ( rc == r4eof )
               return d4goEof( data ) ;

            newRec = tfile4recNo( tag->tagFile ) ;
         }
      #endif
      rc = d4go( data, newRec ) ;
      if ( rc != 0 )
         return rc ;
      #ifndef S4OFF_INDEX
         if ( tag != 0 )
            rc = d4tagSyncDo( data, tag, 1 ) ;
      #endif
      #ifndef S4OFF_MULTI
         if ( rc != 0 )  /* failed, try synching the other way */
         {
            rc = d4go( data, newRec ) ;
            #ifndef S4OFF_INDEX
               #ifndef S4OFF_TRAN
                  /* if transactions are enabled, we do not remove keys from an index file when
                     records are changed to avoid unique problems caused by another user.
                     The result of this is we may need to re-sync ourselves in that case - namely,
                     we do not want to position to a record indicated by a 'removed' entry in
                     a tag.
                  */
                  if ( rc == 0 )
                     if ( code4transEnabled( c4 ) )
                        if ( t4unique( tag ) != 0 )
                           #ifdef S4SERVER
                              if ( dfile4lockTestFile( data->dataFile, data4clientId( data ), data4serverId( data ), lock4write ) != 1 )
                                 if ( dfile4lockTestFile( data->dataFile, data4clientId( data ), data4serverId( data ), lock4read ) != 1 )
                           #else
                              if ( d4lockTestFile( data ) != 1 )
                           #endif
                              rc = d4tagSyncDo( data, tag, -1 ) ;
               #endif
            #endif
         }
      #endif
      return rc ;
   #endif
}
