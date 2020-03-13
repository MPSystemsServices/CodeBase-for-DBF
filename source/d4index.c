/* *********************************************************************************************** */
/* Copyright (C) 1999-2015 by Sequiter, Inc., 9644-54 Ave, NW, Suite 209, Edmonton, Alberta Canada.*/
/* This program is free software: you can redistribute it and/or modify it under the terms of      */
/* the GNU Lesser General Public License as published by the Free Software Foundation, version     */
/* 3 of the License.                                                                               */
/*                                                                                                 */
/* This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;       */
/* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.       */
/* See the GNU Lesser General Public License for more details.                                     */
/*                                                                                                 */
/* You should have received a copy of the GNU Lesser General Public License along with this        */
/* program. If not, see <https://www.gnu.org/licenses/>.                                           */
/* *********************************************************************************************** */

/* d4index.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifdef __TURBOC__
   #pragma hdrstop
#endif

#if defined( TIME4STATUS ) && !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE ) && !defined( S4UNIX ) && !defined( S4MACINTOSH )
   #include <process.h>
#endif

#ifdef P4ARGS_USED
   #pragma argsused
#endif
int S4FUNCTION d4freeBlocks( DATA4 *data )
{
   #ifdef S4INDEX_OFF
      return 0 ;
   #else
      #ifndef S4CLIENT
         int rc ;
         TAG4 *tagOn ;
      #endif

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E93001 ) ;
      #endif

      #ifdef S4CLIENT
         return 0 ;
      #else

         rc = 0 ;
         for( tagOn = 0 ;; )
         {
            tagOn = (TAG4 *)d4tagNext( data, tagOn ) ;
            if ( tagOn == 0 )
               return rc ;
            if ( tfile4freeAll( tagOn->tagFile ) < 0 )
               rc = -1 ;
         }
      #endif /* S4CLIENT */
   #endif
}

/* should compare full name, adding extension if required */
#ifdef P4ARGS_USED
   #pragma argsused
#endif
INDEX4 *S4FUNCTION d4index( DATA4 *data, const char *indexName )
{
   #ifdef S4INDEX_OFF
      return 0 ;
   #else
      char *current ;
      char indexLookup[258], ext[4] = { 0, 0, 0, 0 } ;
      #ifndef S4CLIENT
         char indexLookup2[258] ;
      #endif
      CODE4* c4 ;
      INDEX4 *indexOn ;
      unsigned int i, extIndex ;
      int doAlias, hasExt, hasPath ;

      #ifdef E4PARM_HIGH
         if ( data == 0 )
         {
            error4( 0, e4parm_null, E93002 ) ;
            return 0 ;
         }
      #endif

      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E93002 ) )
            return 0 ;
      #endif  /* E4VBASIC */

      c4 = data->codeBase ;

      if ( indexName == 0 )
         doAlias = 1 ;
      else
      {
         if ( indexName[0] == 0 )
            doAlias = 1 ;
         else
            doAlias = 0 ;
      }

      if ( doAlias == 1 )
         u4ncpy( indexLookup, d4alias( data ), sizeof(indexLookup) ) ;
      else
         u4ncpy( indexLookup, indexName, sizeof(indexLookup) ) ;

      u4nameExt( indexLookup, sizeof(indexLookup), code4indexExtension( data->codeBase ), 0 ) ;

      for( hasPath = 0, i = 0, hasExt = 0 ; i < sizeof( indexLookup ) ; i++ )
      {
         switch( indexLookup[i] )
         {
            case 0:  /* done */
               i = sizeof( indexLookup ) ;
               break ;
            #ifndef S4MACINTOSH
                case ':':
            #endif
            case S4DIR:
               i = sizeof( indexLookup ) ;
               hasPath = 1 ;
               break ;
            case '.':
               // AS 01/04/00 maybe name is ".\" or "..\", in which case no extension...
               if ( indexLookup[i+1] != '.' && indexLookup[i+1] != S4DIR )
               {
                  hasExt = 1 ;
                  c4memset( ext, 0, sizeof( ext ) ) ;
                  extIndex = 0 ;
               }
               break ;
            default:
               if ( hasExt == 1 )  /* part of extension, so copy */
                  if ( extIndex < 3 )  /* ignore rest */
                  {
                     ext[extIndex] = indexLookup[i] ;
                     extIndex++ ;
                  }
               break ;
         }
      }

      for( indexOn = 0 ;; )
      {
         indexOn = (INDEX4 *)l4next( &data->indexes, indexOn) ;
         if ( indexOn == 0 )
            return 0 ;
         #ifdef S4CLIENT
            current = indexOn->alias ;
         #else
            current = indexOn->accessName ;
            if ( current[0] == 0 )  /* use data file name */
            {
               u4namePiece( indexLookup2, sizeof(indexLookup2), data->dataFile->file.name, hasPath, 0 ) ;
               current = indexLookup2 ;
            }
            else if ( hasPath == 0 )  // AS 11/08/00 - added path to accessName to allow for correct adding of tags
            {
               u4namePiece( indexLookup2, sizeof(indexLookup2), indexOn->accessName, hasPath, hasExt ) ;
               if ( hasExt )
                  u4nameExt( indexLookup2, sizeof(indexLookup2), code4indexExtension( c4 ), 0 ) ;  // AS 12/19/00 use code4indexExtension, not 'ntx'
               current = indexLookup2 ;
            }
         #endif

         if ( !u4namecmp( current, indexLookup, c4->ignoreCase ) )    /* check out data->alias? */
            return indexOn ;

         if ( doAlias == 1 )   /* check with just alias (no extension) */
         {
            if ( !u4namecmp( current, d4alias( data ), c4->ignoreCase ) )
               return indexOn ;
         }
         else     /* also check in case of extension specific */
         {
            /* do by not examining the extension (last 4 bytes) of copied memory
               this is to avoid the non-upper-case possible input scenario */

            /* but first ensure that the extension is normalized (if it exists) */
            /* 03/25/96 AS */
            if ( hasExt )
            {
               if ( u4namencmp( code4indexExtension( data->codeBase ), ext, 3, c4->ignoreCase ) != 0 )
                  continue ;   /* means extension is different, so must include in examination */
            }

            if ( !u4namencmp( current, indexLookup, c4strlen( indexLookup ) - 4, c4->ignoreCase ) )
               if ( current[c4strlen(indexLookup) - 4] == '.' || current[c4strlen(indexLookup) - 4] == '\0' )
                  return indexOn ;
         }
      }
   #endif
}

#ifndef N4OTHER
#ifdef P4ARGS_USED
   #pragma argsused
#endif
INDEX4FILE *dfile4index( DATA4FILE *data, const char *indexName )
{
   #ifdef S4INDEX_OFF
      return 0 ;
   #else
      char indexLookup[258] ;
      INDEX4FILE *indexOn ;

      #ifdef E4PARM_LOW
         if ( data == 0 || indexName == 0 )
         {
            error4( 0, e4parm_null, E91102 ) ;
            return 0 ;
         }
      #endif

      #ifdef S4CLIENT
         #ifdef E4ANALYZE
            if ( strlen( indexName ) >= sizeof( indexLookup ) )
            {
               error4( 0, e4struct, E91102 ) ;
               return 0 ;
            }
         #endif
         u4ncpy( indexLookup, indexName, sizeof( indexLookup ) ) ;
      #else
         u4nameCurrent( indexLookup, sizeof( indexLookup ), indexName ) ;
      #endif
      for( indexOn = 0 ;; )
      {
         indexOn = (INDEX4FILE *)l4next( &data->indexes, indexOn ) ;
         if ( indexOn == 0 )
            return 0 ;
         #ifdef S4CLIENT
            if ( !u4namecmp( indexLookup, indexOn->accessName, data->c4->ignoreCase ) )    /* check out data->alias? */
               return indexOn ;
         #else
            if ( !u4namecmp( indexLookup, indexOn->file.name, data->c4->ignoreCase ) )    /* check out data->alias? */
               return indexOn ;
         #endif
      }
   #endif
}
#endif

#ifdef S4WIN32
   // AS Feb 9/06 - added clipper support for packwithstatus
   #if defined( TIME4STATUS ) && !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE )
      // AS Jun 30/03 - moved to d4data.h, for support for d4packWithProgress

      void __cdecl d4reindexMonitor(void *callbackInfo)
      {
         // CS 2001/01/15 This function runs in a thread and polls
         // the CODE4 at certain intervals for reindex status
         // and calls the callback function.
         #ifdef S4CLIENT
            int rc ;
            CODE4 c4 ;
            CODE4 *cb = &c4 ;
            CODE4 *cbReindex = ((REINDEX4CALLBACK*)callbackInfo)->data->codeBase ;
         #else
            CODE4 *cb = ((REINDEX4CALLBACK*)callbackInfo)->data->codeBase ;
         #endif
         short( __stdcall *callback )( double ) = ((REINDEX4CALLBACK*)callbackInfo)->callback ;
         long sleepInterval = ((REINDEX4CALLBACK*)callbackInfo)->sleepInterval ;
         short *reindexDone = &( ((REINDEX4CALLBACK*)callbackInfo)->reindexDone ) ;
         short *callbackStarted = &( ((REINDEX4CALLBACK*)callbackInfo)->callbackStarted ) ;

         #ifdef S4CLIENT  // in client/server, the server is polled with a separate connection
            rc = code4init(cb);
            // AS Apr 13/06 - also may need to send the application stamp...
            if ( cbReindex->applicationVerify != 0 )
               code4verifySet( cb, cbReindex->applicationVerify ) ;
            if (rc == r4success)
               rc = code4connect(cb, cb->defaultServer.serverName, cb->defaultServer.port, cb->defaultServer.userName, cb->defaultServer.password, DEF4PROTOCOL);
            else
               cb = 0 ;
         #endif

         *callbackStarted = 1 ;  // tell the calling process that this thread has started

         #ifdef S4CLIENT  // if the 2nd connection could not be established, send rc to callback
            if (rc != r4success)
            {
               callback( (double)rc ) ;
               if (cb)
               {
                  code4initUndo(cb);
                  cb = 0;
               }
            }
         #endif

         #ifdef S4STAND_ALONE  // wait for reindex to start
            while ( cb->actionCode != ACTION4REINDEX && !(*reindexDone) )
            {
               // AS Jan 19/07 - create a u4sleep() which will delay a short period (fix problem when CodeBase run as a high-priority thread)
               // AS Apr 5/07 - adjust...don't sleep unless the code4 is running as high priority
               u4sleep( cb ) ;
            }
         #endif

         double percent ;
         while ( ! (*reindexDone) )
         {
            if (cb)
            {
               percent = code4status( cb ) ;
               if ( percent < 1.0 )
                  callback( percent );
            }
            else  // if C/S connection could not be est, send 0% to callback at given interval
               callback( 0.0 );
            Sleep( sleepInterval );
         }
         callback( 1.0 );

         u4free( callbackInfo ) ;
         #ifdef S4CLIENT
            if (cb)
               code4initUndo( cb ) ;
         #endif
      }

      void __cdecl d4reindexThread( void *info )
      {
         // CS CS 2001/01/15
         // This function here so that d4reindex can be called as a thread.

         short *reindexDone = &( ((REINDEX4CALLBACK*)info)->reindexDone ) ;
         DATA4 *data = ((REINDEX4CALLBACK*)info)->data ;

         #ifdef S4CLIENT
            short *callbackStarted = &( ((REINDEX4CALLBACK*)info)->callbackStarted ) ;

            while ( *callbackStarted == 0 )  // wait for callback thread to connect to the server
            {
               // AS Apr 5/07 - adjust...don't sleep unless the code4 is running as high priority
               // AS Jan 19/07 - create a u4sleep() which will delay a short period (fix problem when CodeBase run as a high-priority thread)
               u4sleep( data->codeBase ) ;
            }
         #endif
         d4reindex( data ) ;
         *reindexDone = 1;
      }
   #endif

   short S4FUNCTION d4reindexWithProgress( DATA4 S4PTR *data, REINDEX_CALLBACK callback, long milliseconds )
   {
      #ifndef S4OFF_INDEX
         // AS Feb 9/06 - added clipper support for packwithstatus
         #if defined( TIME4STATUS )
            if ( callback == 0 )
               return d4reindex( data ) ;

            #ifdef E4PARM_HIGH
               if ( data == 0 )
                  return error4( 0, e4parmNull, E93004 ) ;
            #endif

            #ifdef E4VBASIC
               if ( c4parm_check( data, 2, E93004 ) )
                  return -1 ;
            #endif

            #ifdef S4OFF_WRITE
               return error4( data->codeBase, e4notWrite, E93004 ) ;
            #else
               if ( milliseconds <= 0 )
                  return error4(data->codeBase, e4parm, E93004 ) ;

               REINDEX4CALLBACK *r4info = (REINDEX4CALLBACK*)u4alloc( sizeof( REINDEX4CALLBACK ) ) ;
               if (!r4info)
                  return error4( data->codeBase, e4memory, E93004 ) ;

               data->codeBase->actionCode = ACTION4INITIALIZING;

               r4info->data = data ;
               r4info->callback = callback ;
               r4info->sleepInterval = milliseconds ;
               r4info->callbackStarted = 0 ;
               r4info->reindexDone = 0 ;

               // begin thread to poll CodeBase status
               if (_beginthread(d4reindexMonitor, 0, (void *)r4info) == -1)
                  return error4(data->codeBase, e4result, E93004 ) ;

               // begin thread to call d4reindex
               if (_beginthread(d4reindexThread, 0, (void *)r4info) == -1)
                  return error4(data->codeBase, e4result, E93004 ) ;

               // LY Jan 6/05 : added !r4info->reindexDone (avoid infinite
               // loop if reindexing finishes before reaching here)
               while ( !r4info->reindexDone && ( r4info->callbackStarted == 0
                  || data->codeBase->actionCode == ACTION4NONE ) )
                  {
                     // AS Jan 19/07 - create a u4sleep() which will delay a short period (fix problem when CodeBase run as a high-priority thread)
                     // AS Apr 5/07 - adjust...don't sleep unless the code4 is running as high priority
                     u4sleep( data->codeBase ) ;
                  }
            #endif  // !OFF_WRITE
         #else  // TIME4STATUS
            callback(0.0);
            int rc = d4reindex( data ) ;
            callback(1.0);
            return rc;
         #endif
      #endif  // !S4OFF_INDEX

      return r4success ;
   }
#endif

#ifndef S4OFF_WRITE
int S4FUNCTION d4reindex( DATA4 *data )
{
   #ifdef S4INDEX_OFF
      return 0 ;
   #else
      int rc ;
      CODE4 *c4 ;
      #ifdef S4CLIENT
         CONNECTION4 *connection ;
         CONNECTION4REINDEX_INFO_OUT *out ;
      #else
         INDEX4 *indexOn ;
         int oldSchemaCreate ;
         #ifdef S4LOW_MEMORY
            #ifndef S4OFF_OPTIMIZE
               int hasOpt ;
            #endif
         #endif
      #endif

      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E93004 ) )
            return -1 ;
      #endif  /* E4VBASIC */

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E93004 ) ;
      #endif

      c4 = data->codeBase ;
      if ( error4code( c4 ) < 0 )
         return e4codeBase ;

      #ifndef S4OFF_WRITE
         rc = d4updateRecord( data, 0, 1 ) ;
         if ( rc )
            return rc ;
      #endif

      if ( data->readOnly == 1 )
         return error4describe( c4, e4write, E80606, d4alias( data ), 0, 0 ) ;

      rc = 0 ;

      #ifdef S4CLIENT
         // Apr 25/02 - ensure batched writes get flushed first
         code4writeBufferFlush( c4 ) ;
         if ( error4code( c4 ) < 0 )  // check if write buffer flush returned an error
            return error4code( c4 ) ;

         connection = data->dataFile->connection ;
         if ( connection == 0 )
         {
            #ifdef TIME4STATUS
               c4->actionCode = ACTION4NONE ;
            #endif
            return e4connection ;
         }

         #ifdef TIME4STATUS
            c4->actionCode = ACTION4REINDEX ;
         #endif
         connection4assign( connection, CON4REINDEX, data4clientId( data ), data4serverId( data ) ) ;
         rc = connection4repeat( connection ) ;
         // AS Jan 25/06 - if the code > 0, don't create an error, just set error4 and return  .. r4unique codes
         if ( rc > 0 )
         {
            #ifdef TIME4STATUS
               c4->actionCode = ACTION4NONE ;
            #endif
            error4set( c4, rc ) ;
            return rc ;
         }
         if ( rc != 0 )
         {
            #ifdef TIME4STATUS
               c4->actionCode = ACTION4NONE ;
            #endif
            return connection4error( connection, c4, rc, E93004) ;
         }

         if ( connection4len( connection ) != sizeof( CONNECTION4REINDEX_INFO_OUT ) )
         {
            #ifdef TIME4STATUS
               c4->actionCode = ACTION4NONE ;
            #endif
            return error4( c4, e4packetLen, E93004 ) ;
         }
         out = (CONNECTION4REINDEX_INFO_OUT *)connection4data( connection ) ;
         if ( out->lockedDatafile )
         {
            // AS May 27/03 - change for cloned locking, store the lockid/serverid, not the data4 itself
            // data->dataFile->fileLock = data ;
            data->dataFile->fileLockServerId = data4serverId( data ) ;
            data->dataFile->fileLockLockId = data4lockId( data ) ;
         }

         data->recNum = -1 ;
         data->recNumOld = -1 ;
         d4blankLow( data, data->record ) ;
      #else
         #ifdef S4LOW_MEMORY
            #ifndef S4OFF_OPTIMIZE
               hasOpt = c4->hasOpt && c4->opt.numBuffers ;
               if ( hasOpt )
                  code4optSuspend( c4 ) ;
            #endif
         #endif

         oldSchemaCreate = c4->oledbSchemaCreate ;
         c4->oledbSchemaCreate = 1 ;   /* for d4reindex() don't check lengths.  Only check on create */
         for ( indexOn = 0 ;; )
         {
            indexOn = (INDEX4 *)l4next( &data->indexes, indexOn ) ;
            if ( indexOn == 0 )
               break ;
            rc = i4reindex( indexOn ) ;
            if ( rc != 0 )  /* error or r4unique */
               break ;
         }
         c4->oledbSchemaCreate = oldSchemaCreate ;

         #ifdef S4LOW_MEMORY
            #ifndef S4OFF_OPTIMIZE
               if ( hasOpt )
                  code4optRestart( c4 ) ;
            #endif
         #endif
      #endif /* S4CLIENT */

      #ifdef TIME4STATUS
         c4->actionCode = ACTION4NONE ;
      #endif

      #ifdef S4UTILS
         // AS July 16/02 - the existing utility reindexed data files once for every open - so if there were
         // 100 clients with a large table open, there would be 100 reindexes of this table.  Instead I modified
         // this to only re-index if it wasn't already done...
         if ( rc == 0 )
            data->dataFile->didReindex = 1 ;
      #endif
      return rc ;
   #endif
}
#endif  /* S4OFF_WRITE */

#ifdef S4VB_DOS

INDEX4 *d4index_v( DATA4 *d4, char *indexName )
{
   return d4index( d4, c4str(indexName) ) ;
}

#endif
