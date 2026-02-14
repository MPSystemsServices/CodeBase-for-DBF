/* d4remove.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

int S4FUNCTION d4remove( DATA4 *data )
{
   #ifdef E4PARM_HIGH
      if ( data == 0 )
         return error4( 0, e4parm_null, E96401 ) ;
   #endif

   CODE4 *c4 = data->codeBase;

   /* for c/s, server takes care of determining this info... */
   #ifdef S4STAND_ALONE
      #ifndef S4OFF_TRAN
         if ( d4transEnabled( data, 0 ) )   // always log if data logging is on, not just if 'active'
            if ( tran4active( c4, data ) != 0 )
               return error4( c4, e4transViolation, E81521 ) ;
      #endif
   #endif

   /*
      // AS 05/18/99 --> this test has been relaxed.  It turns out that the
      // operating system will fail the remove if there is an access problem.
      // AS 05/18/99 --> was not checking that was exclusively opened -- only allow if exclusively
      // opened...

      #ifndef S4CLIENT
         if ( data->dataFile->file.lowAccessMode != OPEN4DENY_RW )
            return error4describe( c4, e4write, E81306, d4alias( data ), 0, 0 ) ;
      #endif
   */

   c4setDoRemove( c4, 1 ) ;
   int rc = d4close( data ) ;
   c4setDoRemove( c4, 0 ) ;

   return rc ;
}



#ifdef S4CLIENT
   int dfile4remove( DATA4FILE *data )
   {
      CODE4 *c4 ;
      int finalRc ;
      INDEX4FILE *i4 ;

      #ifdef E4PARM_LOW
         if ( data == 0 )
            return error4( 0, e4parm_null, E96401 ) ;
      #endif

      #ifdef E4ANALYZE
         if ( data->userCount <= 0 )
            return error4( 0, e4struct, E96401 ) ;
      #endif

      c4 = data->c4 ;
      finalRc = error4set( c4, 0 ) ;

      data->userCount-- ;
      if ( data->userCount == 0 )
      {
         if ( data->info != 0 )
         {
            u4free( data->info ) ;
            data->info = 0 ;
         }

         if ( data->connection == 0 )
            finalRc = e4connection ;
         else
         {
            connection4assign( data->connection, CON4REMOVE, 0, data->serverId ) ;
            connection4sendMessage( data->connection ) ;
            finalRc = connection4receiveMessage( data->connection ) ;
            if ( finalRc >= 0 )
               finalRc = connection4status( data->connection ) ;
         }

         #ifndef S4OFF_INDEX
            for ( ;; )
            {
               i4 = (INDEX4FILE *)l4first( &data->indexes ) ;
               if ( i4 == 0 )
                  break ;
               index4close( i4 ) ;
            }
         #endif

         l4remove( &c4->dataFileList, data ) ;
         #ifdef S4LOCK_HASH
            delete data->lockHash ;
         #endif
         mem4free( c4->data4fileMemory, data ) ;
         error4set( c4, finalRc ) ;
         return finalRc ;
      }
      else
         return error4( 0, e4remove, E86402 ) ;
   }
#endif  /* S4CLIENT */



#ifdef S4SERVER
   int d4clearTables( DATA4FILE *data )
   {
      CODE4 *c4 ;
      #ifndef S4OFF_CATALOG
         TAG4 *tag ;
         CATALOG4 *catalog ;
         #ifndef S4OFF_SECURITY
            TAG4 *secTag ;
            DATA4 *table ;
         #endif
      #endif

      c4 = data->c4 ;

      #ifndef S4OFF_CATALOG
         catalog = c4->catalog ;
         if ( catalog != 0 )
            if ( catalog->catalogStatus )
            {
               tag = d4tag( catalog->data, "PATH" ) ;
               if ( tag == 0 )
                  return error4( c4, e4info, E81403 ) ;
               d4tagSelect( catalog->data, tag ) ;
               #ifndef S4OFF_SECURITY
                  table = c4->server->tableAuth ;
                  if ( table != 0 )
                  {
                     secTag = d4tag( table, "ALIAS" ) ;
                     if ( secTag == 0 )
                        return error4( c4, e4info, E70220 ) ;
                     d4tagSelect( table, secTag ) ;
                  }
               #endif
               for ( ;; )
               {
                  if ( error4code( c4 ) != 0 )
                     return -1 ;
                  if ( d4seek( catalog->data, data->file.name ) == 0 )
                  {
                     #ifndef S4OFF_SECURITY
                        if ( table != 0 )
                        {
                           if ( d4seek( table, cat4alias( catalog ) ) == 0 )
                           {
                              d4delete( table ) ;
                              d4update( table ) ;
                           }
                        }
                     #endif
                     d4delete( catalog->data ) ;
                     d4update( catalog->data ) ;
                  }
               }
            }
      #endif

      return 0 ;
   }
#endif  /* S4SERVER */



#ifndef S4CLIENT
   int S4FUNCTION dfile4remove( DATA4FILE *data )
   {
      int rc ;
      #ifndef S4OFF_INDEX
         #ifdef S4CLIPPER
            TAG4FILE *tagOn ;
         #else
            INDEX4FILE *indexOn ;
         #endif
      #endif

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E96401 ) ;
      #endif

      if ( data->userCount != 0 )
         return error4( 0, e4parm_null, E86402 ) ;

      #ifdef S4SERVER
         /* remove entries in all tables */
         d4clearTables( data ) ;
      #endif

      #ifndef S4OFF_INDEX
         #ifdef S4CLIPPER
            for( tagOn = 0;; )
            {
               tagOn = dfile4tagNext( data, tagOn ) ;
               if ( tagOn == 0 )
                  break ;

               file4setTemporary( &tagOn->file, 1, 0 ) ;
            }
         #else
            for( indexOn = 0 ;; )
            {
               indexOn = (INDEX4FILE *)l4next( &data->indexes, indexOn ) ;
               if ( indexOn == 0 )
                  break ;

               file4setTemporary( &indexOn->file, 1, 0 ) ;
            }
         #endif
      #endif

      file4setTemporary( &data->file, 1, 1 ) ;

      #ifndef S4OFF_MEMO
         if (data->nFieldsMemo )
            file4setTemporary( &data->memoFile.file, 1, 0 ) ;
      #endif

      rc = dfile4closeLow( data ) ;

      return rc ;
   }
#endif  /* !S4CLIENT */
