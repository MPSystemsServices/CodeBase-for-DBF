/* i4index.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifdef __TURBOC__
   #pragma hdrstop
#endif

#ifdef I4PRINT
   #include <sys\timeb.h>
   #include <time.h>
#endif



#ifndef S4CLIENT
   static INDEX4 *i4openLocal( DATA4 *d4, const char *fileName ) ;
#endif



#ifndef S4INDEX_OFF
   #ifdef S4CLIENT
      static INDEX4 *i4setup2( CODE4 *c4, INDEX4FILE *i4file, DATA4 *d4, const char *name, INDEX4 *i4old, int doFull )
      {
         /* if i4old is set to a value, then it is used instead of creating a new i4 */
         /* if doFull is set to false and i4old is valid, a tag update between i4file
            and i4 are done only */

         INDEX4 *i4 ;

         if ( i4old == 0 )
         {
            if ( c4->indexMemory == 0 )
            {
               c4->indexMemory = mem4create( c4, c4->memStartIndex, sizeof(INDEX4), c4->memExpandIndex, 0 ) ;
               if ( c4->indexMemory == 0 )
                  return 0 ;
            }

            i4 = (INDEX4 *)mem4allocZero( c4->indexMemory ) ;
            i4->codeBase = c4 ;
            i4->data = d4 ;
            i4->indexFile = i4file ;
            l4add( &d4->indexes, i4 ) ;
            u4ncpy( i4->alias, name, sizeof( i4->alias ) - 1 ) ;
         }
         else
            i4 = i4old ;

         for ( TAG4FILE *tagOn = 0 ;; )
         {
            tagOn = (TAG4FILE *)l4next( &i4file->tags, tagOn ) ;
            if ( tagOn == 0 )
               break ;
            if ( doFull == 0 )   /* check that tag doesn't already exit */
            {
               Bool5 doUpdate = 1 ;
               for ( TAG4 *tag2 = 0 ;; )
               {
                  tag2 = (TAG4 *)l4next( &i4->tags, tag2 ) ;
                  if ( tag2 == 0 )
                     break ;
                  if ( tag2->tagFile == tagOn )
                  {
                     doUpdate = 0 ;
                     break ;
                  }
               }
               if ( doUpdate == 0 )
                  continue ;
            }
            TAG4 *tag = (TAG4 *)mem4allocZero( c4->tagMemory ) ;
            if ( tag == 0 )
               return 0 ;
            tag->tagFile = tagOn ;
            tag->index = i4 ;
            tag->errUnique = tagOn->errUniqueHold ;
            l4add( &i4->tags, tag ) ;
            // AS Oct 25/05 - keep track of additional information for the tag...low level tag functions
            tag->tagFile->refData = d4 ;
         }

         // AS Feb 7/03 - Need for locking control as well as transactions
         // #ifndef S4OFF_TRAN
            i4->isValid = 1 ;
         // #endif
         return i4 ;
      }
   #endif /* S4CLIENT */



   #ifdef S4CLIENT
      int i4setup( CODE4 *c4, DATA4 *d4, const char *name, const char *accessName, int autoOpened, INDEX4 *i4old )
      {
         /* if i4old is set to a value, then it is used instead of creating a new i4
            now set up the tags */
         if ( c4->tagMemory == 0 )
         {
            c4->tagMemory = mem4create( c4, c4->memStartTag, sizeof(TAG4), c4->memExpandTag, 0 ) ;
            if ( c4->tagMemory == 0 )
            {
               d4close( d4 ) ;
               return 0 ;
            }
         }

         // AS May 2/02 - The code from Oct 24/01 does not work in certain scenarios.
         // In particular, the accessNmae used for i4file must first consider input i4old
         // which is input when we want to add the tag to an existing index.  Therefore,
         // use i4old accessName as a higher priority
         const char *iAccessName = 0 ;
         if ( i4old != 0 )
            iAccessName = i4old->indexFile->accessName ;
         else
            iAccessName = accessName ;

         if ( iAccessName == 0 )  // AS Jun 27/02 - we are using iAccessName now, not accessName
            iAccessName = name ;

         for ( INDEX4FILE *i4file = 0 ;; )
         {
            i4file = (INDEX4FILE *)l4next( &d4->dataFile->indexes, i4file ) ;
            if ( i4file == 0 )
               break ;
            // AS Oct 24/01 - If the index name is input 'name', then only set up the index file with that
            // name.  This allows 2 DATA4's to share the DATA4FILEs while not sharing the same indexes.
            // if ( accessName == 0 )
            //    accessName = name ;
            // AS Need to consider that the case of the iAccessName and the i4file->accessName may differ
            // if ( name != 0 && iAccessName != 0 && c4strcmp( iAccessName, i4file->accessName ) != 0 )
            if ( c4->ignoreCase == 1 )
            {
               if ( name != 0 && iAccessName != 0 && c4stricmp( iAccessName, i4file->accessName ) != 0 )
               {
                  // didn't match, so continue on...
                  continue ;
               }
            }
            else
            {
               if ( name != 0 && iAccessName != 0 && c4strcmp( iAccessName, i4file->accessName ) != 0 )
               {
                  // didn't match, so continue on...
                  continue ;
               }
            }
            int addIndex = 1 ;
            int addTags = 0 ;
            INDEX4 *i4 ;
            for ( i4 = 0 ;; )   /* only set up for i4file's not in i4's of d4 */
            {
               i4 = (INDEX4 *)l4next( &d4->indexes, i4 ) ;
               if ( i4 == 0 )
                  break ;
               if ( i4->indexFile == i4file )
               {
                  if ( l4numNodes( &i4->tags ) < l4numNodes( &i4file->tags ) )
                     addTags = 1 ;
                  addIndex = 0 ;
                  break ;
               }
            }
            if ( addIndex == 0 )
            {
               if ( addTags == 0 )
                  continue ;
            }
            else
               i4file->userCount++ ;
            i4 = i4setup2( c4, i4file, d4, name, i4old, addIndex ) ;
            if ( i4 == 0 )
            {
               d4close( d4 ) ;
               return 0 ;
            }
            i4file->autoOpened = autoOpened ;    /* if in i4open, must be a manual open */
         }

         return 0 ;
      }
   #endif /* S4CLIENT */



   #if defined( S4CLIPPER ) && !defined( S4CLIENT )
      #ifdef P4ARGS_USED
         #pragma argsused
      #endif
      int i4setup( CODE4 *c4, DATA4 *d4, const char *name, int autoOpened )
      {
         TAG4FILE *tagFile ;
         TAG4 *tag ;
         DATA4FILE *dfile ;
         INDEX4 *i4 = 0 ;

         dfile = d4->dataFile ;

         for ( tagFile = 0 ;; )
         {
            tagFile = (TAG4FILE *)l4next( &dfile->tagfiles, tagFile ) ;
            if ( tagFile == 0 )
               break ;
            // AS Oct 12/04 - We need to track the index access name associated with the TAG4FILE in case the indexfile gets opened again.
            // in particular in client/server to keep the index associated with the correct structure in case it is opened again by another client.
            INDEX4 *i4 = d4index( d4, tagFile->indexAccessName ) ;
            if ( i4 == 0 )
            {
               i4 = (INDEX4 *)mem4allocZero( c4->indexMemory ) ;
               i4->codeBase = c4 ;
               i4->data = d4 ;
               l4add( &d4->indexes, i4 ) ;
               u4ncpy( i4->accessName, tagFile->indexAccessName, sizeof( i4->accessName ) - 1 ) ;
            }
            tag = (TAG4 *)mem4allocZero( c4->tagMemory ) ;
            if ( tag == 0 )
               return 0 ;
            tag->tagFile = tagFile ;
            tag->tagFile->userCount++ ;
            tag->index = i4 ;
            #ifdef S4SERVER
               tag->errUnique = tfile4unique( tag->tagFile, (short int)d4->codeBase->errDefaultUnique ) ;
            #endif
            l4add( &i4->tags, tag ) ;
         }

         return 0 ;
      }
   #endif /* #if defined( S4CLIPPER ) && !defined( S4CLIENT ) */



   int S4FUNCTION i4close( INDEX4 *i4 )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( i4, 0, E91701 ) )
            return -1 ;
      #endif

      #ifdef E4PARM_HIGH
         if ( i4 == 0 )
            return error4( 0, e4parm_null, E91701 ) ;
         if ( i4->codeBase == 0 )
            return error4( 0, e4parm, E91701 ) ;

         #if !defined( S4CLIPPER ) && !defined( S4CLIENT )
            // users are not allowed to close production index files
            if ( index4isProduction( i4->indexFile ) )
            {
               // AS 05/19/99 --> able to relax this constraint, check if the user
               // really did 'auto-open' it (is openMdx 1) in s/a...
               #ifdef S4STAND_ALONE
                  if ( i4->indexFile->dataFile->openMdx == 1 )
               #endif
                     return error4( i4->codeBase, e4notSupported, E81721 ) ;
            }
         #endif
      #endif

      // AS Feb 12/03 - Moved from i4closeLow(), where we want to allow closing in some instances.
      #if !defined( S4CLIPPER ) && !defined( S4CLIENT ) && !defined( S4OFF_TRAN )
         // AS Jun 20/03 - was checking wrong flag
         #if defined( S4SERVER ) && defined( S4ODBC_BUILD )
            if ( i4->codeBase->server->odbcTrans == 1 )  // odbc build, no trans available
         #endif
            {
               if ( i4->isValid == 1 ) /* if invalid (failed create/open) then allow close */
                  if ( code4transEnabled( i4->codeBase ) && code4trans( i4->codeBase )->currentTranStatus == r4active )  /* disallow on current active only */
                     return error4( i4->codeBase, e4transViolation, E81522 ) ;
            }
      #endif

      return i4closeLow( i4 ) ;
   }



   #ifdef S4CLIENT
      void tfile4free( TAG4FILE *tagOn )
      {
         assert5( tagOn != 0 ) ;
         CODE4 *c4 = tagOn->codeBase ;
         assert5( c4 != 0 ) ;
         if ( tagOn->exprPtr != 0 )
         {
            u4free( tagOn->exprPtr ) ;
            tagOn->exprPtr = 0 ;
         }
         if ( tagOn->filterPtr != 0 )
         {
            u4free( tagOn->filterPtr ) ;
            tagOn->filterPtr = 0 ;
         }
         if ( tagOn->currentKey != 0 )
         {
            u4free( tagOn->currentKey ) ;
            tagOn->currentKey = 0 ;
            tagOn->currentKeyLen = 0 ;
            tagOn->currentKeyAllocLen = 0 ;
         }
         tfile4freeCache( tagOn, 1 ) ;
         mem4free( c4->tagFileMemory, tagOn ) ;
      }
   #endif /* S4CLIENT */



   #ifdef S4CLIENT
      void t4free( TAG4 *tag, Bool5 freeTagFile )
      {
         assert5( tag != 0 ) ;
         CODE4 *c4 = tag->tagFile->codeBase ;
         assert5( c4 != 0 ) ;
         if ( freeTagFile )
         {
            l4remove( &tag->index->indexFile->tags, tag->tagFile ) ;
            tfile4free( tag->tagFile ) ;
         }
         mem4free( c4->tagMemory, tag ) ;
      }
   #endif /* S4CLIENT */



   #ifdef S4CLIENT
      int index4close( INDEX4FILE *i4 )
      {
         #ifdef E4PARM_LOW
            if ( i4 == 0 )
               return error4( 0, e4parm_null, E91702 ) ;
         #endif

         #ifdef E4ANALYZE
            #ifndef S4OFF_TRAN
               if ( i4->userCount <= 0 && i4->isValid == 1 )   /* is Valid is 0 if we are closing due to a failed open */
                  return error4( i4->codeBase, e4struct, E81702 ) ;
            #endif
            if ( i4->codeBase == 0 )
               return error4( i4->codeBase, e4struct, E91702 ) ;
         #endif

         CODE4 *c4 = i4->codeBase ;
         int saveRc = error4set( c4, 0 ) ;
         int finalRc = 0 ;

         CONNECTION4 *connection ;
         TAG4FILE *tagOn ;
         int rc ;

         if ( i4->isValid == 1 )
         {
            i4->userCount-- ;
            assert5( i4->userCount >= 0 ) ;
         }

         if ( i4->userCount == 0 || i4->isValid == 0 )
         {
            // AS 01/29/01 - If there are no tags, do not need to close index
            // this happens when tags are removed via i4tagRemove.
            // if ( i4->autoOpened == 0 )   /* must manually close ... */
            if ( l4numNodes( &i4->tags ) != 0 && i4->autoOpened == 0 )   /* must manually close ... */
            {
               #ifndef S4OFF_TRAN
                  // AS Jun 20/03 - was checking wrong flag
                  #if defined( S4SERVER ) && defined( S4ODBC_BUILD )
                     if ( c4->server->odbcTrans == 1 )  // odbc build, no trans available
                  #endif
                        if ( code4transEnabled( c4 ) && code4trans( c4 )->currentTranStatus == r4active )  /* disallow on current active only */
                        {
                           if ( saveRc >= 0 )
                              return error4( c4, e4transViolation, E81522 ) ;
                           error4set( c4, saveRc ) ;
                           return saveRc ;
                        }
               #endif
               connection = i4->dataFile->connection ;
               #ifdef E4ANALYZE
                  if ( connection == 0 )
                  {
                     if ( saveRc == 0 )
                        finalRc = error4( c4, e4struct, E91702 ) ;
                  }
                  else
                  {
               #endif
                     CONNECTION4CLOSE_INDEX_INFO_IN info ;

                     connection4assign( connection, CON4INDEX_CLOSE, i4->clientId, i4->serverId ) ;
                     // AS May 23/03 - Also requires the doRemove flag if non-auto-open and we are removing
                     long removeFlag = 0 ;
                     if ( i4->autoOpened == 0 && c4getDoRemove( c4 ) == 1 )
                        removeFlag = 1 ;
                     info.doRemove = (short)removeFlag ;
                     short nameLen = strlen( i4->accessName ) + 1 ;
                     info.nameLen = htons5( nameLen ) ;
                     connection4addData( connection, &info, sizeof( CONNECTION4CLOSE_INDEX_INFO_IN ), NULL ) ;
                     connection4addData( connection, i4->accessName, nameLen, NULL ) ;
                     connection4sendMessage( connection ) ;
                     rc = connection4receiveMessage( connection ) ;
                     if ( rc < 0 )
                        finalRc = error4( c4, rc, E81701 ) ;
                     else
                     {
                        rc = connection4status( connection ) ;
                        if ( rc != 0 )
                           if ( saveRc == 0 )
                              finalRc = connection4error( connection, c4, rc, E91702 ) ;
                     }
               #ifdef E4ANALYZE
                  }
               #endif
            }
            for( ;; )
            {
               tagOn = (TAG4FILE *)l4pop( &i4->tags ) ;
               if ( tagOn == 0 )
                  break ;
               tfile4free( tagOn ) ;
            }
            l4remove( &i4->dataFile->indexes, i4 ) ;
            mem4free( c4->index4fileMemory, i4 ) ;
            i4 = 0 ;
            if ( saveRc < 0 )
               error4set( c4, saveRc ) ;
            return finalRc ;
         }

         return 0 ;
      }
   #endif /* S4CLIENT */



   #ifdef S4CLIENT
      int S4FUNCTION i4closeLow( INDEX4 *i4 )
      {
         CODE4 *c4 = i4->codeBase ;

         int finalRc = 0 ;

         #ifndef S4OFF_WRITE
            if ( i4->data )
               if ( d4update( i4->data ) < 0 )
                  finalRc = error4set( c4, 0 ) ;
         #endif

         #ifdef E4ANALYZE
            if ( i4->data == 0 )
               return error4( 0, e4struct, E91701 ) ;
            if ( i4->data->dataFile == 0 )
               return error4( 0, e4struct, E91701 ) ;
         #endif

         if ( i4->data->tagSelected != 0 )
         {
            if ( i4->data->tagSelected->index == i4 )
               i4->data->tagSelected = 0 ;
         }

         for( ;; )
         {
            TAG4 *tagOn = (TAG4 *)l4pop( &i4->tags ) ;
            if ( tagOn == 0 )
               break ;
            t4free( tagOn, 0 ) ;  // index4close will free up the TAG4FILE structures
         }

         l4remove( &i4->data->indexes, i4 ) ;
         int rc = index4close( i4->indexFile ) ;
         mem4free( c4->indexMemory, i4 ) ;
         i4 = 0 ;

         if ( rc < 0 )
            return rc ;

         return finalRc ;
      }
   #endif /* S4CLIENT */



   const char *S4FUNCTION t4fileName( TAG4 *t4 )
   {
      #ifdef S4CLIENT
         CONNECTION4 *connection ;
         int rc ;
      #endif

      #ifdef E4PARM_HIGH
         if ( t4 == 0 )
         {
            error4( 0, e4parm_null, E91720 ) ;
            return 0 ;
         }
      #endif

      #ifdef S4CLIENT
         if ( error4code( t4->index->codeBase ) < 0 )
            return 0 ;

         connection = t4->index->data->dataFile->connection ;
         connection4assign( connection, CON4TAG_FNAME, data4clientId( t4->index->data ), data4serverId( t4->index->data ) ) ;
         connection4addData( connection, t4->tagFile->alias, strlen( t4->tagFile->alias ) + 1, NULL ) ;
         connection4sendMessage( connection ) ;
         rc = connection4receiveMessage( connection ) ;
         if ( rc < 0 )
         {
            #ifdef E4STACK
               error4stack( t4->index->codeBase, rc, E95501 ) ;
            #endif
            return 0 ;
         }

         rc = connection4status( connection ) ;
         if ( rc < 0 )
         {
            connection4error( connection, t4->index->codeBase, rc, E95501 ) ;
            return 0 ;
         }

         return connection4data( connection ) ;
      #else
         #ifdef S4CLIPPER
            return t4->tagFile->file.name ;
         #else
            return i4fileName( t4->index ) ;
         #endif
      #endif
   }



   const char *S4FUNCTION i4fileName( INDEX4 *i4 )
   {
      #ifdef S4CLIENT
         CONNECTION4 *connection ;
         int rc ;
      #else
         #ifdef S4CLIPPER
            // AS Jun 14/06 - in this case change to return the physical tag file name instead...
            TAG4 *tag ;
         #endif
      #endif

      #ifdef E4PARM_HIGH
         if ( i4 == 0 )
         {
            error4( 0, e4parm_null, E91720 ) ;
            return 0 ;
         }
      #endif

      #ifdef S4CLIENT
         if ( error4code( i4->codeBase ) < 0 )
            return 0 ;

         connection = i4->data->dataFile->connection ;
         connection4assign( connection, CON4INDEX_FNAME, data4clientId( i4->data ), data4serverId( i4->data ) ) ;
         connection4addData( connection, i4->indexFile->accessName, strlen( i4->indexFile->accessName ) + 1, NULL ) ;
         connection4sendMessage( connection ) ;
         rc = connection4receiveMessage( connection ) ;
         if ( rc < 0 )
         {
            #ifdef E4STACK
               error4stack( i4->codeBase, rc, E95501 ) ;
            #endif
            return 0 ;
         }

         rc = connection4status( connection ) ;
         if ( rc < 0 )
         {
            connection4error( connection, i4->codeBase, rc, E95501 ) ;
            return 0 ;
         }

         return connection4data( connection ) ;
      #else
         #ifdef S4CLIPPER
            // AS May 26/06 - was not properly using this path value...it needs to be a stored string
            if ( i4->nameWithPath[0] != 0 )
               return i4->nameWithPath ;
            if ( l4numNodes( &i4->tags ) == 1 )
            {
               // AS Jun 14/06 - in this case change to return the physical tag file name instead...
               // this isn't documented as to what should happen but this is a better implementation.
               tag = (TAG4 *)(i4->tags.lastNode) ;
               return t4fileName( tag ) ;
               // return tag->alias ;
            }
            else
               return d4fileName( i4->data ) ;
         #else
            return i4->indexFile->file.name ;
         #endif /* S4CLIPPER */
      #endif /* S4CLIENT */
   }



   #if !defined( S4CLIPPER ) && !defined( S4CLIENT )
      int tfile4free( TAG4FILE *tagFile )
      {
         // frees all memory associated with a tagFile.  Assumes already removed from lists.
         CODE4 *c4 = tagFile->codeBase ;
         if ( tfile4freeAll( tagFile ) < 0 )
            return -1 ;
         if ( tagFile->expr != 0 )
         {
            expr4free( tagFile->expr ) ;
            tagFile->expr = 0 ;
         }
         if ( tagFile->filter != 0 )
         {
            expr4free( tagFile->filter ) ;
            tagFile->filter = 0 ;
         }
         #ifdef S4FOX
            mem4release( tagFile->builtKeyMemory ) ;
            tagFile->builtKeyMemory = 0 ;
         #endif
         mem4free( c4->tagFileMemory, tagFile ) ;

         return 0 ;
      }
   #endif /* #if !defined( S4CLIPPER ) && !defined( S4CLIENT ) */



   #if !defined( S4CLIPPER ) && !defined( S4CLIENT )
      int S4FUNCTION i4closeLow( INDEX4 *i4 )
      {
         int rc, finalRc ;
         TAG4 *tagOn ;
         CODE4 *c4 ;

         c4 = i4->codeBase ;

         // AS Feb 12/03 - Moved check to i4close(), this way we support closing if we
         // are removing the index file due to deleting a tag (which is supported within a transaction)
         // #ifndef S4OFF_TRAN
         //    if ( i4->isValid == 1 ) /* if invalid (failed create/open) then allow close */
         //       if ( code4transEnabled( c4 ) )
         //          if ( code4trans( c4 )->currentTranStatus == r4active )  /* disallow on current active only */
         //             return error4( c4, e4transViolation, E81522 ) ;
         // #endif

         finalRc = 0 ;

         #ifndef S4OFF_WRITE
            #ifndef S4OFF_TRAN
               if ( i4->isValid == 1 ) /* if invalid (failed create/open) then allow close */
            #endif
            if ( i4->data )
               if ( d4update( i4->data ) < 0 )
                  finalRc = error4set( c4, 0 ) ;
         #endif

         for( ;; )
         {
            tagOn = (TAG4 *)l4pop( &i4->tags ) ;
            if ( tagOn == 0 )
               break ;
            #ifndef S4OFF_TRAN
               #ifdef E4ANALYZE
                  #ifdef SHARE4TAG_REMOVE
                     if ( tagOn->tagFile->removedKeys.nLink != 0 )
                  #else
                     if ( tagOn->removedKeys.nLink != 0 )
                  #endif
                        return error4( c4, e4info, E91701 ) ;
               #endif
            #endif
            if ( i4->data->tagSelected == tagOn )  /* can't have a tag selected from a closed index */
               i4->data->tagSelected = 0 ;
            /* 06/10/96 AS expression data mismatch problem --> if same data4 re-allocated */
            if ( tagOn->tagFile->expr != 0 ) /* could be NULL if improper open or if unsupported expression (read-only) */
               if ( tagOn->tagFile->expr->data == i4->data )
                  tagOn->tagFile->expr->data = 0 ;
            mem4free( c4->tagMemory, tagOn ) ;
            tagOn = 0 ;
         }

         if ( i4->indexFile != 0 )
         {
            /* 05/30/96 AS --> t4code4.c failure if creating production index file with keepopen true (mdx c/s) */
            #ifndef S4OFF_MULTI  // CS 2000/12/08
               index4unlock( i4->indexFile, data4serverId( i4->data ) ) ;
            #endif
            rc = index4close( i4->indexFile ) ;
            if ( rc != 0 )
               finalRc = rc ;
         }

         if ( i4->link.n != 0 )
            l4remove( &i4->data->indexes, i4 ) ;
         mem4free( c4->indexMemory, i4 ) ;
         i4 = 0 ;

         if ( finalRc != 0 )
         {
            error4set( c4, finalRc ) ;
            return finalRc ;
         }

         return 0 ;
      }
   #endif /* #if !defined( S4CLIPPER ) && !defined( S4CLIENT ) */



   #if !defined( S4CLIPPER ) && !defined( S4CLIENT )
      int index4close( INDEX4FILE *i4 )
      {
         int finalRc ;
         CODE4 *c4 ;
         TAG4FILE *tagOn ;
         #ifndef S4SINGLE
            int saveAttempts ;
         #endif

         #ifdef E4PARM_LOW
            if ( i4 == 0 )
               return error4( 0, e4parm_null, E91702 ) ;
         #endif
         #ifdef E4ANALYZE
            if ( i4->userCount < 0 )
               return error4( i4->codeBase, e4struct, E81702 ) ;
         #endif

         // AS 02/16/00 index4isProduction may fail if i4->isValid is false, in which case we do not use the
         // setting anyway
         int isProduction = 0 ;
         if ( i4->isValid )
            isProduction = index4isProduction( i4 ) ;
         // AS 05/19/99 --> relax close constraint in the case of being isProduction but not opened as production...
         #ifdef S4STAND_ALONE
            if ( isProduction )
               if ( i4->dataFile->openMdx == 0 )
                  isProduction = 0 ;
         #endif

         if ( i4->isValid == 0 || ( i4->userCount <= 1 && isProduction == 0 ) || ( i4->userCount == 0 && i4->dataFile->userCount == 0 ) )
         {
            c4 = i4->codeBase ;

            finalRc = error4code( c4 ) ;
            #ifndef S4SINGLE
               saveAttempts = c4->lockAttempts ;
               c4->lockAttempts = WAIT4EVER ;
            #endif

            #ifndef S4OFF_WRITE
               if ( index4update( i4 ) < 0 )
                  finalRc = error4set( c4, 0 ) ;
            #endif

            #ifndef S4SINGLE
               if ( index4unlock( i4, 0UL ) < 0 )
                  finalRc = error4set( c4, 0 ) ;
            #endif

            #ifdef S4FOX
               if ( i4->tagIndex )
                  if ( i4->tagIndex->header.typeCode >= 64 )  /* compound index */
            #endif
            for( ;; )
            {
               tagOn = (TAG4FILE *)l4pop( &i4->tags ) ;
               if ( tagOn == 0 )
                  break ;
               if ( tfile4free( tagOn ) < 0 )
               {
                  finalRc = error4set( c4, 0 ) ;
                  break ;
               }
            }

            #ifdef S4FOX
               if ( i4->tagIndex != 0 )
               {
                  if ( tfile4freeAll( i4->tagIndex ) < 0 )
                     finalRc = error4set( c4, 0 ) ;
                  else
                  {
                     if ( i4->tagIndex->expr != 0 )
                     {
                        expr4free( i4->tagIndex->expr ) ;
                        i4->tagIndex->expr = 0 ;
                     }
                     if ( i4->tagIndex->filter != 0 )
                     {
                        expr4free( i4->tagIndex->filter ) ;
                        i4->tagIndex->filter = 0 ;
                     }
                     #ifdef S4FOX
                        mem4release( i4->tagIndex->builtKeyMemory ) ;
                        i4->tagIndex->builtKeyMemory = 0 ;
                     #endif
                     mem4free( c4->tagFileMemory, i4->tagIndex ) ;
                     i4->tagIndex = 0 ;
                  }
               }
            #endif

            mem4release( i4->blockMemory ) ;

            if ( file4openTest( &i4->file ) )
            {
               if ( i4->dataFile )
                  l4remove( &i4->dataFile->indexes, i4 ) ;
               #ifndef S4CLIPPER
                  // AS 03/06/00 - was not correct in server, should be getDoRemove...
                  if ( c4getDoRemove( c4 ) == 1 )
                  {
                     // AS Sep. 17/01 - changed isTemp to be done via a function for code4validate() capability
                     file4setTemporary( &i4->file, 1, 0 ) ;
                  }
               #endif
               if ( file4close( &i4->file ) < 0 )
                  finalRc = error4set( c4, 0 ) ;
            }

            mem4free( c4->index4fileMemory, i4 ) ;
            i4 = 0 ;
            #ifndef S4SINGLE
               c4->lockAttempts = saveAttempts ;
            #endif
            error4set( c4, (short)finalRc ) ;
            return finalRc ;
         }
         else
         {
            i4->userCount-- ;
            return 0 ;
         }
      }
   #endif /* #if !defined( S4CLIPPER ) && !defined( S4CLIENT ) */



   #if !defined( S4CLIPPER ) && !defined( S4CLIENT ) && !defined( S4OFF_WRITE )
      B4NODE index4extend( INDEX4FILE *i4 )
      {
         /*
            S4FOX/S4MDX - returns the unit # of B4BLOCK_SIZE offset of an available block
                    (i.e. multiply by B4BLOCK_SIZE to get the physical file offset)
         */
         B4NODE invalidNode ;
         b4nodeSetInvalid( &invalidNode ) ;
         #ifdef E4PARM_LOW
            if ( i4 == 0 )
            {
               error4( 0, e4parm_null, E91703 ) ;
               return invalidNode ;
            }
         #endif

         CODE4 *c4 = i4->codeBase ;
         if ( error4code( c4 ) < 0 )
            return invalidNode ;

         #ifdef S4FOX
            TAG4FILE *tagIndex = i4->tagIndex ;
            #ifdef E4ANALYZE
               if ( tagIndex->header.version == i4->versionOld )
               {
                  error4( c4, e4index, E91703 ) ;
                  return invalidNode ;
               }
            #endif

            B4NODE oldEof ;

            Bool5 freeListAvailable = 1 ;

            #ifdef S4COMIX_NO_EOF
               // for 2.5 or 2.6 compatible files, don't use free list entry in file (use local copy only)
               if ( i4->dataFile->compatibility != 30 )
               {
                  if ( b4node( tagIndex->availBlock ) == 0 )  // use this item instead... to avoid index bloat
                     freeListAvailable = 0 ;
               }
               else
            #endif
               {
                  b4nodeAssignNode( &oldEof, tagIndex->header.freeList ) ;
                  if ( b4node( oldEof ) == 0L )  /* case where no free list */
                     freeListAvailable = 0 ;
               }

            if ( freeListAvailable == 0 )
            {
               b4nodeAssignNode( &oldEof, i4->eof ) ;
               // i4->eof += B4BLOCK_SIZE ;
               b4nodeAddBlocks( &i4->eof, i4, 1 ) ;
            }
            else
            {
               #ifdef S4COMIX_NO_EOF
                  if ( i4->dataFile->compatibility != 30 )
                  {
                     assert5( b4node( tagIndex->availBlock ) != 0 ) ;
                     b4nodeAssignNode( &oldEof, tagIndex->availBlock ) ;
                     b4nodeAssignLong( &tagIndex->availBlock, 0 ) ;
                  }
                  else
               #endif
                  {
                     FILE4LONG pos ;
                     b4nodeGetFilePosition( i4, tagIndex->header.freeList, &pos ) ;

                     // AS 07/10/00 the Comix driver stores the free-list info always in the same position, bytes 5-8...
                     #ifdef S4COMIX
                        if ( i4->dataFile->compatibility != 30 )
                        {
                           file4longAdd( &pos, 4 ) ;
                        }
                     #endif

                     unsigned len = file4readInternal( &i4->file, pos, (char *)&tagIndex->header.freeList, sizeof( tagIndex->header.freeList ) ) ;

                     #ifdef S4BYTE_SWAP
                        tagIndex->header.freeList.node = x4reverseLong( &tagIndex->header.freeList ) ;
                     #endif

                     if ( error4code( c4 ) < 0 )
                        return invalidNode ;

                     switch( len )
                     {
                        case 0:
                           #ifdef E4ANALYZE
                              error4( c4, e4index, E91703 ) ;
                              return invalidNode ;
                           #else  /* else fix up */
                              b4nodeAssignLong( &tagIndex->header.freeList, 0L ) ;
                              b4nodeAssignNode( &oldEof, i4->eof ) ;
                              b4nodeAddBlocks( &i4->eof, i4, 1 ) ;
                              break ;
                           #endif
                        case sizeof(tagIndex->header.freeList):
                           break ;
                        default:
                           file4readError( &i4->file, pos, sizeof(tagIndex->header.freeList), "index4extend" ) ;
                           return invalidNode ;
                     }
                  }
            }

            // AS May 3/07 - more tracking...
            #ifdef I4PRINT
               struct _timeb mTime ;
               char *outTime, dump[255] ;
               _ftime( &mTime ) ;
               outTime = ctime( &( mTime.time ) ) ;

               sprintf( dump, "index4extend, i4 = %ld, extended fileBlock: %uld, new freelist root: %uld time: %s \r\n", (unsigned long)i4, (long)b4node(oldEof), (long)b4node(tagIndex->header.freeList), outTime ) ;
               log5( dump );
            #endif
         #else
            B4NODE oldEof = i4->header.freeList ;

            if( i4->header.freeList == 0L )  /* case where no free list */
            {
               oldEof = i4->header.eof ;
               i4->header.eof = i4->header.eof + i4->header.blockRw / I4MULTIPLY ;
            }
            else
            {
               FILE4LONG pos ;
               file4longAssign( pos, i4->header.freeList*I4MULTIPLY + sizeof(S4LONG), 0 ) ;
               unsigned len = file4readInternal( &i4->file, pos, (char *)&i4->header.freeList, sizeof(i4->header.freeList)) ;

               #ifdef S4BYTE_SWAP
                  i4->header.freeList = x4reverseLong( (void *)&i4->header.freeList ) ;
               #endif

               if ( error4code( c4 ) < 0 )
                  return invalidNode ;

               switch( len )
               {
                  case 0:
                     #ifdef E4ANALYZE
                        error4( c4, e4index, E91703 ) ;
                        return invalidNode ;
                     #else   /* try to fix up */
                        i4->header.freeList = 0L ;
                        oldEof = i4->header.eof ;
                        i4->header.eof = i4->header.eof + i4->header.blockRw / I4MULTIPLY ;
                        break ;
                     #endif
                  case sizeof( i4->header.freeList ):
                     break ;
                  default:
                     file4readError( &i4->file, pos, sizeof(i4->header.freeList), "index4extend" ) ;
                     return invalidNode ;
               }
            }
         #endif /* S4FOX */

         return oldEof ;
      }
   #endif /* #if !defined( S4CLIPPER ) && !defined( S4CLIENT ) && !defined( S4OFF_WRITE ) */



   #if !defined( S4CLIPPER ) && !defined( S4CLIENT ) && !defined( S4OFF_WRITE )
      int index4flush( INDEX4FILE *i4 )
      {
         #ifdef E4PARM_LOW
            if ( i4 == 0 )
               return error4( 0, e4parm_null, E91704 ) ;
         #endif

         int rc = index4update( i4 ) ;
         if ( file4flush( &i4->file ) < 0 )
            rc = -1 ;

         return rc ;
      }
   #endif /* #if !defined( S4CLIPPER ) && !defined( S4CLIENT ) && !defined( S4OFF_WRITE ) */



   #if !defined( S4CLIPPER ) && !defined( S4CLIENT ) && !defined( S4OFF_WRITE )
      int index4update( INDEX4FILE *i4 )
      {
         #ifdef E4PARM_LOW
            if ( i4 == 0 )
               return error4( 0, e4parm_null, E91705 ) ;
         #endif

         if ( error4code( i4->codeBase ) < 0 )
            return e4codeBase ;

         #ifndef S4SINGLE
            // AS May 10/01 - If the index is opened exclusive, was not marked as
            // 'fileLocked', and thus the update was not occurring.  End result
            // was that the free block chain was not updated in this case (esp.
            // with i4tagRemove()), resulting in lost index blocks.
            // if ( i4->fileLocked != 0 )
            if ( i4->fileLocked != 0 || i4->file.lowAccessMode == OPEN4DENY_RW )
            {
         #endif
               int rc = index4updateHeader( i4 ) ;
               if ( rc < 0 )
                  return error4stack( i4->codeBase, (short)rc, E91705 ) ;
               #ifdef S4FOX
                  rc = tfile4update(i4->tagIndex) ;
                  if ( rc < 0 )
                     return error4stack( i4->codeBase, (short)rc, E91705 ) ;
                  if ( i4->tagIndex->header.typeCode >= 64 )  /* compound index */
               #endif

               for ( TAG4FILE *tagOn = 0 ;; )
               {
                  tagOn = (TAG4FILE *)l4next( &i4->tags, tagOn ) ;
                  if ( tagOn == 0 )
                     break ;
                  rc = tfile4update( tagOn ) ;
                  if ( rc < 0 )
                     return error4stack( i4->codeBase, (short)rc, E91705 ) ;
                  b4nodeSetInvalid( &tagOn->header.root ) ;
               }
         #ifndef S4SINGLE
            }
         #endif

         return 0 ;
      }
   #endif /* #if !defined( S4CLIPPER ) && !defined( S4CLIENT ) && !defined( S4OFF_WRITE ) */



   #ifdef S4CLIENT
      static INDEX4 *i4openClient( DATA4 *d4, const char *fileName )
      {
         CODE4 *c4 = d4->codeBase ;

         unsigned char buf[258] ;

         if ( fileName == 0 )
            u4ncpy( (char *)buf, d4->dataFile->accessName, sizeof( buf ) - 1 ) ;
         else
         {
            if ( strlen( fileName ) > sizeof( buf ) - 1 )
            {
               error4( c4, e4name, E91706 ) ;
               return 0 ;
            }
            strcpy( (char*)buf, fileName ) ;
         }

         if ((code4serverOS( c4 ) & 0x07) == OS4WIN32)
            c4upper( (char*)buf ) ;
         INDEX4 *i4 = d4index( d4, (char S4PTR*)buf ) ;

         if ( i4 != 0 )   /* duplicates not allowed */
            error4( c4, e4instance, E91706 ) ;

         INDEX4FILE *i4file = index4open( d4, (char S4PTR*)buf, 0 ) ;

         if ( error4code( c4 ) < 0 )
            return 0 ;
         i4 = d4index( d4, (char S4PTR*)buf ) ;

         if ( i4file == 0 )
         {
            if ( i4 == 0 )
               return 0 ;
            #ifdef E4ANALYZE
               if ( i4->indexFile == 0 )
               {
                  error4( c4, e4info, E91706 ) ;
                  return 0 ;
               }
            #endif
            i4file = i4->indexFile ;
            i4file->clientId = data4clientId( d4 ) ;
            i4file->serverId = data4serverId( d4 ) ;
         }
         else  /* indexfile already exists so set up another index4 structure */
         {
            i4 = i4setup2( c4, i4file, d4, (char S4PTR*)buf, 0, 1 ) ;
            if ( i4 == 0 )
            {
               index4close( i4file ) ;
               return 0 ;
            }
         }

         #ifdef E4ANALYZE
            if ( i4->indexFile != i4file )
            {
               i4->isValid = 0 ;
               i4close( i4 ) ;
               error4( c4, e4info, E91706 ) ;
               return 0 ;
            }
         #endif

         i4->codeBase = c4 ;
         return i4 ;
      }
   #endif /* S4CLIENT */



   #if !defined( S4CLIPPER ) && !defined( S4CLIENT )
      static INDEX4 *i4openLocal( DATA4 *d4, const char *fileName )
      {
         CODE4 *c4 = d4->codeBase ;

         // AS Jun 2/03 - Added recording if data file has a tag with an expression referring to a memo field
         d4->hasMemoExpr = r4uninitializedMemoTagExpression ;  // just reset, it gets set automatically if required

         if ( c4->indexMemory == 0 )
         {
            c4->indexMemory = mem4create( c4, c4->memStartIndex, sizeof(INDEX4), c4->memExpandIndex, 0 ) ;
            if ( c4->indexMemory == 0 )
               return 0 ;
         }

         // provide more error tracking
         INDEX4 *i4 = (INDEX4 *)mem4allocErrZero( c4->indexMemory, c4 ) ;
         if ( i4 == 0 )
         {
            #ifdef ERROR4STACK
               error4stack( c4, e4memory, E91706 ) ;
            #endif
            return 0 ;
         }

         i4->data = d4 ;
         i4->codeBase = c4 ;

         if ( fileName != 0 )
         {
            #ifdef E4MISC
               if ( strlen( fileName ) > sizeof( i4->accessName ) )
               {
                  error4describe( c4, e4name, E91706, fileName, 0, 0 ) ;
                  i4closeLow( i4 ) ;
                  return 0 ;
               }
            #endif
            u4ncpy( i4->accessName, fileName, sizeof( i4->accessName ) - 1 ) ;
         }
         #ifdef S4STAND_ALONE
            else
            {
               u4namePiece( i4->accessName, sizeof( i4->accessName ), d4->alias, 0, 0 ) ;
            }
         #endif

         #ifdef S4PREPROCESS_FILE
            // AS 02/12/01 - want to turn off errOpen in case it is a preprocess problem.  If it is another problem, we
            // will catch it on the alternate open.
            int oldErrOff = c4->errOff ;
            // AS Aug 25/03 - Don't do this if the data file does not support preporcessing
            short oldPreprocess = code4getPreprocessFile( c4 ) ;
            if ( oldPreprocess == 0 && d4->dataFile->file.preprocessed == 1 )
               c4->errOff = 1 ;
            // AS Apr 24/06 - fix for encryption...change the preprocess setting if the data file is not preprocessed (don't open index as preprocessed in that case)
            if ( oldPreprocess == 1 && d4->dataFile->file.preprocessed == 0 )
               code4setPreprocessFile( c4, 0 ) ;
         #endif
         i4->indexFile = index4open( d4, fileName, i4 ) ;
         #ifdef S4PREPROCESS_FILE
            if ( oldPreprocess == 1 && d4->dataFile->file.preprocessed == 0 )  // AS May 24/06 - only conditionally put it back on, it may not be enabled
               code4setPreprocessFile( c4, oldPreprocess ) ;
            c4->errOff = oldErrOff ;
         #endif
         if ( i4->indexFile == 0 )
         {
            #ifdef S4PREPROCESS_FILE
               // AS 02/12/01 - If the open fails, it may be due to preprocessing, try opening the file in the opposite mode
               // AS Aug 25/03 - Don't do this if the data file does not support preporcessing
               if ( oldPreprocess == 0 && d4->dataFile->file.preprocessed == 1 )
               {
                  code4setPreprocessFile( c4, ( oldPreprocess ? 0 : 1 ) ) ;  // just put to the opposite number
                  error4set( c4, 0 ) ;
                  i4->indexFile = index4open( d4, fileName, i4 ) ;
                  code4setPreprocessFile( c4, oldPreprocess ) ;
               }
               if ( i4->indexFile == 0 )
               {
                  i4closeLow( i4 ) ;
                  return 0 ;
               }
            #else
               i4closeLow( i4 ) ;
               return 0 ;
            #endif
         }

         l4add( &d4->indexes, i4 ) ;

         for ( TAG4FILE *tagFile = 0 ;; )
         {
            tagFile = (TAG4FILE *)l4next( &i4->indexFile->tags, tagFile ) ;
            if ( tagFile == 0 )
               break ;

            // provide more error tracking
            TAG4 *tag = (TAG4 *)mem4allocErrZero( c4->tagMemory, c4 ) ;
            if ( tag == 0 )
            {
               i4closeLow( i4 ) ;
               error4( c4, e4memory, E91706 ) ;
               return 0 ;
            }
            tag->index = i4 ;
            tag->tagFile = tagFile ;

            #ifdef S4FOX
               if ( tag->tagFile->header.typeCode & 0x04 )  /* r4/e4 candidate */
               {
                  if ( c4->errDefaultUnique == e4unique )
                     tag->errUnique = e4candidate ;
                  else
                     tag->errUnique = r4candidate ;
               }
               else
                  if ( tag->tagFile->header.typeCode & 0x01 )
            #else
               if ( tag->tagFile->header.unique )
            #endif
                  tag->errUnique = c4->errDefaultUnique ;


            l4add( &i4->tags, tag ) ;
         }

         // AS Feb 7/03 - Need for locking control as well as transactions
         // #ifndef S4OFF_TRAN
            i4->isValid = 1 ;
         // #endif

         #ifdef S4SERVER
            /* AS 04/05/01 - if the client requests the open after a create and it is temp, mark it as such */
            if ( c4->createTemp == 1 )  // means the client is requesting this open as temp part of i4create
            {
               // AS Sep. 17/01 - changed isTemp to be done via a function for code4validate() capability
               file4setTemporary( &i4->indexFile->file, 1, 0 ) ;
            }
            // AS Jan 1/03 - support for creating temporary non updating indexes
            if ( c4->createTemp == 2 )  // means the client is requesting this open as temporary non-updating
            {
               file4setTemporary( &i4->indexFile->file, 1, 0 ) ;
               i4->indexFile->nonUpdateable = 1 ;
               i4->indexFile->file.isReadOnly = 1 ;
            }
         #endif
         return i4 ;
      }
   #endif /* #if !defined( S4CLIPPER ) && !defined( S4CLIENT ) */


   INDEX4 *S4FUNCTION i4open( DATA4 *d4, const char *fileName )
   {
      #ifdef E4PARM_HIGH
         if ( d4 == 0 )
         {
            error4( 0, e4parm_null, E91706 ) ;
            return 0 ;
         }
      #endif

      #ifdef E4VBASIC
         if ( c4parm_check( d4, 2, E91706 ) )
            return 0 ;
      #endif

      if ( error4code( d4->codeBase ) < 0 )
         return 0 ;

      #ifdef S4CLIENT
         return i4openClient( d4, fileName ) ;
      #else
         INDEX4 *index = i4openLocal( d4, fileName ) ;
         // AS May 30/03 - if this is a production index being opened manually with a null
         // filename, we should be registering it as production
         if ( index != 0 && fileName == 0 && d4->dataFile->openMdx == 0 )
         {
            #ifdef S4FOX
               if ( d4->dataFile->hasMdxMemo & 0x01 )
            #endif
            #ifdef S4MDX
               if ( index->indexFile->header.isProduction )
            #endif
            #ifdef S4CLIPPER
               // clipper, if we have opened an index, it is automatically production...
               // if ( index->indexFile->header.isProduction )
            #endif
                  d4->dataFile->openMdx = 1 ;
         }

         return index ;
      #endif
   }



   #ifdef S4CLIENT
      INDEX4FILE *index4open( DATA4 *d4, const char *fileName, INDEX4 *index )
      {
         /* for the client, if index is not null, then the function only verifies the
            existance of the index file and does not actually open it */
         int rc ;
         CONNECTION4OPEN_INDEX_INFO_OUT *info ;
         CONNECTION4OPEN_INDEX_INFO_IN *dataIn ;
         CONNECTION4 *connection ;
         short len ;
         INDEX4 *indexLoop ;

         #ifdef E4PARM_LOW
            if ( d4 == 0 )
            {
               error4( 0, e4parm_null, E91707 ) ;
               return 0 ;
            }
         #endif

         CODE4 *c4 = d4->codeBase ;

         INDEX4FILE *i4 = dfile4index( d4->dataFile, fileName ) ;
         if ( i4 != 0 )
         {
            /* allowed if current data4 does not have a pointer to index */
            for ( indexLoop = 0 ;; )
            {
               indexLoop = (INDEX4 *)l4next( &d4->indexes, indexLoop ) ;
               if ( indexLoop == 0 )
                  break ;
               if ( indexLoop->indexFile == i4 )
               {
                  error4( c4, e4instance, E91707 ) ;
                  return 0 ;
               }
            }
            i4->userCount++ ;
            return i4 ;
         }

         #ifdef E4VBASIC
            if ( c4parm_check( d4, 2, E91707 ) )
               return 0 ;
         #endif

         if ( index != 0 )   /* just wanted to verify existance of... */
            return 0 ;
         c4 = d4->dataFile->c4 ;
         if ( error4code( c4 ) < 0 )
            return 0 ;

         if ( c4->index4fileMemory == 0 )
            c4->index4fileMemory = mem4create( c4, c4->memStartIndexFile, sizeof(INDEX4FILE), c4->memExpandIndexFile, 0 ) ;
         if ( c4->index4fileMemory == 0 )
             return 0 ;

         if ( c4->tagMemory == 0 )
         {
            c4->tagMemory = mem4create( c4, c4->memStartTag, sizeof(TAG4), c4->memExpandTag, 0 ) ;
            if ( c4->tagMemory == 0 )
               return 0 ;
         }

         if ( c4->tagFileMemory == 0 )
         {
            c4->tagFileMemory = mem4create( c4, c4->memStartTagFile, sizeof(TAG4FILE), c4->memExpandTagFile, 0 ) ;
            if ( c4->tagFileMemory == 0 )
               return 0 ;
         }

         connection = d4->dataFile->connection ;
         connection4assign( connection, CON4INDEX_OPEN, data4clientId( d4 ), data4serverId( d4 ) ) ;
         connection4addData( connection, NULL, sizeof( CONNECTION4OPEN_INDEX_INFO_IN ), (void **)&dataIn ) ;
         connection = d4->dataFile->connection ;
         if ( connection == 0 )
         {
            error4( c4, e4connection, E81704 ) ;
            return 0 ;
         }
         // AS 04/04/01 - Was not sending the 'temporary open on create' to the server
         // AS Jan 1/03 - added non-updating temporary indexes - temp val of 2
         // if ( c4->createTemp == 1 && c4->openForCreate == 1 )
         if ( c4->createTemp != 0 && c4->openForCreate == 1 )
            dataIn->createTemp = c4->createTemp ;


         len = (short)(strlen( (char*)fileName ) + 1) ;
         dataIn->nameLen = htons5( (short)len ) ;
         dataIn->openForCreate = htons5( c4->openForCreate ) ;
         dataIn->fileFlush = htons5( c4->fileFlush ) ;

         #ifdef S4SINGLE
            dataIn->exclusiveClient = 1 ;
         #else
            if ( c4->singleOpen == OPEN4DENY_RW )
               dataIn->accessMode = htons5( OPEN4DENY_RW ) ;
            else
               dataIn->accessMode = htons5( c4->accessMode ) ;
         #endif

         dataIn->readOnly = c4->readOnly ;
         dataIn->safety = c4->safety ;  /* for catalog */
         dataIn->errDefaultUnique = htons5( c4->errDefaultUnique ) ;

         connection4addData( connection, fileName, len, NULL ) ;
         connection4sendMessage( connection ) ;
         rc = connection4receiveMessage( connection ) ;
         if ( rc < 0 )
         {
            error4( c4, rc, E81701 ) ;
            return 0 ;
         }
         rc = connection4status( connection ) ;
         if ( rc != 0 )
         {
            if ( rc < 0 )
            {
               if ( c4->errOpen == 0 )
               {
                  if ( error4code( c4 ) >= 0 )
                     error4set( c4, r4noOpen ) ;
               }
               else
                  connection4error( connection, c4, rc, E91707 ) ;
            }
            if ( rc == r4noOpen )  // case where server detected index already open (same index but different identifier)
               error4( c4, e4instance, E91706 ) ;
            return 0 ;
         }

         #ifdef E4MISC
            if ( connection4len( connection ) < sizeof( info ) )
            {
               error4( c4, e4packetLen, E91707 ) ;
               return 0 ;
      }
         #endif

         info = (CONNECTION4OPEN_INDEX_INFO_OUT *)connection4data( connection ) ;
         /* AS June 5/01 - see c4com.h CONNECTION4OPEN_INDEX_INFO_OUT structure for info on changes */
         const char *indexAlias = connection4data( connection ) + sizeof(CONNECTION4OPEN_INDEX_INFO_OUT) ;

         rc = client4indexSetup( c4, d4, d4->dataFile, ntohs5(info->numTags), ntohs5(info->fullPathNameLen), indexAlias,
              (unsigned int)connection4len( connection ) - sizeof(CONNECTION4OPEN_INDEX_INFO_OUT), (char*)fileName, 0 ) ;

         if ( rc < 0 )
         {
            error4( c4, e4connection, E91707 ) ;
            return 0 ;
         }

         if ( rc == r4noOpen )  // means client already has it open
         {
            if ( c4->errOpen == 0 )
                error4set( c4, r4noOpen ) ;
            else
                error4( c4, e4instance, E91706 ) ;
            return 0 ;
         }

         /* in the client case, a null is a valid return code as the index is later extracted */
         i4setup( c4, d4, (char*)fileName, 0, 0, 0 ) ;
         return 0 ;
      }

   #endif /* S4CLIENT */


   #if !defined( S4CLIPPER ) && !defined( S4CLIENT )
      INDEX4FILE *index4open( DATA4 *d4, const char *fileName, INDEX4 *index )
      {
         #ifdef E4PARM_LOW
            if ( d4 == 0 )
            {
               error4( 0, e4parm_null, E91707 ) ;
               return 0 ;
            }
            #ifndef S4CLIENT
               if ( index == 0 )
               {
                  error4( 0, e4parm_null, E91707 ) ;
                  return 0 ;
               }
            #endif
         #endif

         #ifdef E4VBASIC
            if ( c4parm_check( d4, 2, E91707 ) )
               return 0 ;
         #endif

         CODE4 *c4 = d4->codeBase ;

         #ifdef E4ANALYZE
            if ( code4indexExtension( c4 ) == 0 )
            {
               error4( c4, e4struct, E91707 ) ;
               return 0 ;
            }
         #endif

         DATA4FILE *dfile = d4->dataFile ;
         char buf[258] ;
         if ( fileName == 0 )
            u4ncpy( (char *)buf, dfile->file.name, sizeof( buf ) - 1 ) ;
         else
         {
            int rc = u4nameCurrent( (char *)buf, sizeof( buf ), fileName ) ;
            if ( rc < 0 )
            {
               error4( c4, rc, E94509 ) ;  /* from u4nameCurrent */
               return 0 ;
            }
         }

         u4nameExt( (char *)buf, sizeof( buf ), code4indexExtension( c4 ), ( fileName == 0 ? 1 : 0 ) ) ;

         INDEX4FILE *i4 = dfile4index( dfile, (char *)buf ) ;
         if ( i4 != 0 )
         {
            #ifndef S4SERVER
               /* allowed if current data4 does not have a pointer to index */
               for ( INDEX4 *indexLoop = 0 ;; )
               {
                  indexLoop = (INDEX4 *)l4next( &d4->indexes, indexLoop ) ;
                  if ( indexLoop == 0 )
                     break ;
                  if ( indexLoop->indexFile == i4 )
                  {
                     error4( c4, e4instance, E91707 ) ;
                     return 0 ;
                  }
               }
            #endif
            i4->userCount++ ;
            return i4 ;
         }

         if ( error4code( c4 ) < 0 )
            return 0 ;

         if ( c4->index4fileMemory == 0 )
            c4->index4fileMemory = mem4create( c4, c4->memStartIndexFile, sizeof(INDEX4FILE), c4->memExpandIndexFile, 0 ) ;
         if ( c4->index4fileMemory == 0 )
             return 0 ;

         if ( c4->tagMemory == 0 )
         {
            c4->tagMemory = mem4create( c4, c4->memStartTag, sizeof(TAG4), c4->memExpandTag, 0 ) ;
            if ( c4->tagMemory == 0 )
               return 0 ;
         }

         if ( c4->tagFileMemory == 0 )
         {
            c4->tagFileMemory = mem4create( c4, c4->memStartTagFile, sizeof(TAG4FILE), c4->memExpandTagFile, 0 ) ;
            if ( c4->tagFileMemory == 0 )
               return 0 ;
         }

         // provide more error tracking
         i4 = (INDEX4FILE *)mem4allocErrZero( c4->index4fileMemory, c4 ) ;
         if ( i4 == 0 )
         {
            #ifdef ERROR4STACK
               error4stack( c4, e4memory, E91707 ) ;
            #endif
            return 0 ;
         }

         /* next line for this function duration only since the return of this
            function is assigned into index->indexFile but upper level.
            Therefore, we still must clean up ourselves if failure */
         index->indexFile = i4 ;
         i4->dataFile = dfile ;
         i4->codeBase = c4 ;
         int rc ;

         #ifdef S4FOX
            // AS 06/30/99 set defaults for block size & multiplier...
            i4->multiplier = 1 ;
            i4->blockSize = B4BLOCK_SIZE_INTERNAL ;
            #ifdef E4MISC
               for ( INDEX4FILE *i4ptr = 0 ;; )
               {
                  i4ptr = (INDEX4FILE *)l4next( &dfile->indexes, i4ptr ) ;
                  if ( i4ptr == 0 )
                     break ;
                  if ( !c4memcmp( i4ptr->file.name, buf, (size_t)strlen( (char *)buf ) ) )
                  {
                     mem4free( c4->index4fileMemory, i4 ) ;
                     index->indexFile = 0 ;
                     error4( c4, e4parm, E81703 ) ;
                     return 0 ;
                  }
               }
            #endif

            // AS May 24/02 - created file4openLow for internal use to indicate file types
            rc = file4openInternal( &i4->file, c4, (char *)buf, 1, OPT4INDEX ) ;
            if ( rc )
            {
               index->indexFile = 0 ;
               mem4free( c4->index4fileMemory, i4 ) ;
               return 0 ;
            }

            // provide more error tracking
            i4->tagIndex = (TAG4FILE *)mem4allocErrZero( c4->tagFileMemory, c4 ) ;
            if ( i4->tagIndex == 0 )
            {
               file4close( &i4->file ) ;
               index->indexFile = 0 ;
               mem4free( c4->index4fileMemory, i4 ) ;
               error4( c4, e4memory, E91707 ) ;
               return 0 ;
            }

            l4add( &dfile->indexes, i4 ) ;
            FILE4LONG pos ;
            /* LY July 7/03 : changed from 0 to 0L for Linux compiler */
            file4longAssign( pos, 0, 0L ) ;
            if ( fileName == 0 )
            {
               if ( tfile4init( i4->tagIndex, index, pos, (unsigned char *)"" ) < 0 )
               {
                  index4close( i4 ) ;
                  return 0 ;
               }
            }
            else
            {
               u4namePiece( (char *)buf, 258, fileName, 0, 0 ) ;  /* get the tagName based on the fileName */
               if ( tfile4init( i4->tagIndex, index, pos, (unsigned char *)buf) < 0 )
               {
                  index4close( i4 ) ;
                  return 0 ;
               }
            }

            assert5( i4multiplier( i4 ) != 0 ) ;   // ensure set properly or we get a divide by zero error
            b4nodeSetFromFilePosition( i4, &i4->eof, file4lenLow( &i4->file ) ) ;

            /* Perform some checks */
            if ( b4node( i4->tagIndex->header.root ) == 0L || b4nodeInvalid( i4->tagIndex->header.root ) || i4->tagIndex->header.typeCode < 32 )
            {
               #ifdef E4ANALYZE_ALL
                  error4describe( c4, e4index, E81714, buf, 0, 0 ) ;
               #endif
               index4close( i4 ) ;
               #ifndef E4ANALYZE_ALL
                  error4describe( c4, e4index, E81714, (char *)buf, 0, 0 ) ;
               #endif
               return 0 ;
            }

            i4->versionOld = i4->tagIndex->header.version ;
            i4->versionReadUnlocked = i4->versionOld ;
            i4->blockMemory = mem4create( c4, c4->memStartBlock, sizeof(B4BLOCK) + i4blockSize( i4 )
                            - sizeof(B4STD_HEADER) - sizeof(B4NODE_HEADER), c4->memExpandBlock, 0 ) ;
            if ( i4->blockMemory == 0 )
            {
               index4close( i4 ) ;
               return 0 ;
            }

            /* do an initial block allocation to make sure minimal is allocated while optimization is suspended */
            B4BLOCK *b4 = (B4BLOCK *)mem4allocErrZero( i4->blockMemory, c4 ) ;
            if ( b4 == 0 )
            {
               index4close( i4  ) ;
               error4( c4, e4memory, E91707 ) ;
               return 0 ;
            }
            else
            {
               mem4free( i4->blockMemory, b4 ) ;
               b4 = 0 ;
            }

            #ifndef S4SINGLE
               /* disable locking */
               long oldFileLock = i4->fileLocked ;
               i4->fileLocked = data4serverId( d4 ) ;
            #endif

            rc = tfile4top( i4->tagIndex ) ;
            if ( rc < 0 )
            {
               #ifndef S4SINGLE
                  i4->fileLocked = oldFileLock ;
               #endif
               index4close( i4 ) ;
               return 0 ;
            }

            TAG4FILE *tagFile ;
            /* if we have a compound index, then load the tags, otherwise this is the only tag */
            if ( i4->tagIndex->header.typeCode >= 64 )
            {
               if ( b4numKeys( tfile4block( i4->tagIndex ) ) )
                  do
                  {
                     // provide more error tracking
                     tagFile = (TAG4FILE *)mem4allocErrZero( c4->tagFileMemory, c4 ) ;
                     if ( tagFile == 0 )
                     {
                        index4close( i4 ) ;
                        error4( c4, e4memory, E91707 ) ;
                        #ifndef S4SINGLE
                           i4->fileLocked = oldFileLock ;
                        #endif
                        return 0 ;
                     }

                     int oldErrExpr = c4->errExpr ;
                     if ( i4->file.isReadOnly == 1 ) /* special case, allow to open invalid expression tags, since not appending */
                        c4->errExpr = 0 ;

                     B4NODE node ;
                     b4nodeAssignLong( &node, b4recNo( tfile4block(i4->tagIndex), tfile4block( i4->tagIndex)->keyOn ) ) ;
                     FILE4LONG pos ;
                     b4nodeGetFilePosition( i4, node, &pos ) ;
                     if ( tfile4init( tagFile, index, pos, tfile4keyData( i4->tagIndex )->value ) < 0 )
                     {
                        if ( i4->file.isReadOnly == 1 ) /* special case, allow to open invalid expression tags, since not appending */
                           error4set( c4, 0 ) ;
                        else
                        {
                           #ifndef S4SINGLE
                              i4->fileLocked = oldFileLock ;
                           #endif
                           // AS Jan 1/03 - was failing to free up the allocated tag above in this case
                           assert5( tagFile->link.n == 0 ) ;  // shouldn't be on the list, if it is, don't need to free it
                           mem4free( c4->tagFileMemory, tagFile ) ;
                           index4close( i4 ) ;
                           return 0 ;
                        }
                     }

                     c4->errExpr = oldErrExpr ;
                     // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
                     // if ( tfile4setCodePage( tagFile, d4->codePage ) < 0 )
                     // {
                     //    error4( c4, e4index, E91642 ) ;
                     //    index4close( i4 ) ;
                     //    return 0 ;
                     // }

                     l4add( &i4->tags, tagFile ) ;
                  } while ( tfile4skip( i4->tagIndex, 1L ) == 1L ) ;
            }
            else
            {
               #ifdef E4MISC
                  if ( fileName == 0 )
                  {
                     index4close( i4 ) ;
                     error4( c4, e4index, E81715 ) ;
                  }
               #endif
               tagFile = i4->tagIndex ;
               l4add( &i4->tags, i4->tagIndex ) ;   /* if an .idx, add single tag */
            }
            #ifndef S4SINGLE
               i4->fileLocked = oldFileLock ;
            #endif
         #endif

         #ifndef S4FOX
            #ifdef E4MISC
               for ( INDEX4FILE *i4ptr = 0 ;; )
               {
                  i4ptr = (INDEX4FILE *)l4next(&dfile->indexes, i4ptr) ;
                  if ( i4ptr == 0 )
                     break ;
                  if ( !c4memcmp( i4ptr->file.name, buf, (size_t) strlen(buf) ) )
                  {
                     error4( c4, e4parm, E81703 ) ;
                     return 0 ;
                  }
               }
            #endif

            // AS May 24/02 - created file4openLow for internal use to indicate file types
            rc = file4openInternal( &i4->file, c4, buf, 1, OPT4INDEX ) ;
            if ( rc )
            {
               index->indexFile = 0 ;
               mem4free( c4->index4fileMemory, i4 ) ;
               return 0 ;
            }

            FILE4LONG pos ;
            file4longAssign( pos, 0, 0 ) ;
            if ( file4readAllInternal( &i4->file, pos, &i4->header, sizeof(I4HEADER) ) < 0 )
            {
               file4close( &i4->file ) ;
               index->indexFile = 0 ;
               mem4free( c4->index4fileMemory, i4 ) ;
               return 0 ;
            }

            #ifdef S4BYTE_SWAP
               i4->header.blockChunks = x4reverseShort( (void *)&i4->header.blockChunks ) ;
               i4->header.blockRw = x4reverseShort( (void *)&i4->header.blockRw ) ;
               i4->header.slotSize = x4reverseShort( (void *)&i4->header.slotSize ) ;
               i4->header.numTags = x4reverseShort( (void *)&i4->header.numTags ) ;
               i4->header.eof = x4reverseLong( (void *)&i4->header.eof ) ;
               i4->header.freeList = x4reverseLong( (void *)&i4->header.freeList ) ;
            #endif

            l4add( &dfile->indexes, i4 ) ;

            /* Perform some checks */
            if ( i4->header.blockRw != i4->header.blockChunks * 512  ||
                 i4->header.blockChunks <= 0 ||
                 i4->header.blockChunks > 63 ||
                 i4->header.numTags < 0  || i4->header.numTags > 47 ||
                 i4->header.eof <= 0L )
            {
               index4close( i4 ) ;
               error4describe( c4, e4index, E81716, buf, 0, 0 ) ;
               return 0 ;
            }

            file4longAssign( pos, 544, 0 ) ;
            T4DESC tagInfo[47] ;
            if ( file4readAllInternal( &i4->file, pos, tagInfo, sizeof(tagInfo)) < 0 )
            {
               index4close( i4 ) ;
               return 0 ;
            }

            for ( int iTag = 0; iTag < (int) i4->header.numTags; iTag++ )
            {
               // provide more error tracking
               TAG4FILE *tagFile = (TAG4FILE *)mem4allocErrZero( c4->tagFileMemory, c4 ) ;
               if ( tagFile == 0 )
               {
                  index4close( i4 ) ;
                  #ifdef E4STACK
                     error4stack( c4, e4memory, E91707 ) ;
                  #endif
                  return 0 ;
               }

               #ifdef S4BYTE_SWAP
                  tagInfo[iTag].headerPos = x4reverseLong( (void *)&tagInfo[iTag].headerPos ) ;
                  tagInfo[iTag].x1000 = 0x1000 ;
               #endif

               if ( tfile4init( tagFile, index, tagInfo + iTag ) < 0 )
               {
                  index4close( i4 ) ;
                  return 0 ;
               }

               l4add( &i4->tags, tagFile ) ;
            }

            i4->blockMemory = mem4create( c4, c4->memStartBlock, sizeof(B4BLOCK) + i4->header.blockRw -
                              sizeof(B4KEY_DATA) - sizeof(short) - sizeof(char[6]), c4->memExpandBlock, 0 ) ;
            if ( i4->blockMemory == 0 )
            {
               index4close( i4 ) ;
               return 0 ;
            }
         #endif

         #ifndef S4OFF_OPTIMIZE
            // AS 06/26/00 - was not marking index with owner for optimization purposes
            // file4optimize( &i4->file, c4->optimize, OPT4INDEX ) ;
            file4optimizeLow( &i4->file, c4->optimize, OPT4INDEX, 0, i4 ) ;
         #endif

         i4->userCount++ ;
         i4->isValid = 1 ;
         return i4 ;
      }
   #endif /* #if !defined( S4CLIPPER ) && !defined( S4CLIENT ) */

   #if !defined( S4CLIPPER )
      // !S4CLIPPER
      TAG4 *S4FUNCTION i4tag( INDEX4 *i4, const char *tagName )
      {
         char tagLookup[LEN4TAG_ALIAS+1] ;
         TAG4 *tagOn ;

         #ifdef E4VBASIC
            if ( c4parm_check( i4, 0, E91709 ) )
               return 0 ;
         #endif

         #ifdef E4PARM_HIGH
            if ( i4 == 0 )
            {
               error4( 0, e4parm_null, E91709 ) ;
               return 0 ;
            }
            if ( tagName == 0 )
            {
               error4( i4->codeBase, e4parm_null, E91709 ) ;
               return 0 ;
            }
         #endif

         u4ncpy( tagLookup, tagName, sizeof( tagLookup )) ;
         c4upper( tagLookup ) ;

         for( tagOn = 0 ;; )
         {
            tagOn = (TAG4 *)l4next( &i4->tags, tagOn ) ;
            if ( tagOn == 0 )
               break ;
            if ( c4strcmp( tagOn->tagFile->alias, tagLookup ) == 0 )
               return tagOn ;
         }

         if ( i4->codeBase->errTagName )
            error4describe( i4->codeBase, e4tagName, E91709, tagName, 0, 0 ) ;
         return 0 ;
      }
   #endif /* #if !defined( S4CLIPPER ) */



   #if !defined( S4CLIPPER ) && !defined( S4CLIENT ) && !defined( S4OFF_WRITE )
      int index4shrink( INDEX4FILE *i4, B4NODE blockNo )
      {
         // blockNo is physical file byte offset of block to be shrunk
         #ifdef E4PARM_LOW
            if ( i4 == 0 )
               return error4( 0, e4parm_null, E91708 ) ;
            if ( b4nodeInvalid( blockNo ) )
               return error4( i4->codeBase, e4parm, E91708 ) ;
         #endif

         if ( error4code( i4->codeBase ) < 0 )
            return e4codeBase ;

         #ifdef I4PRINT
            struct _timeb mTime ;
            char *outTime, dump[255] ;
            _ftime( &mTime ) ;
            outTime = ctime( &( mTime.time ) ) ;

            sprintf( dump, "index4shrink, i4 = %ld, fileBlock: %uld, lockFlag: %ld time: %s \r\n", (unsigned long)i4, (long)b4node(blockNo), (long)i4->fileLocked, outTime ) ;
            log5( dump );
         #endif

         #ifdef S4FOX
            Bool5 writeFreeList = 1 ;
            #ifdef S4COMIX_NO_EOF
               // for 2.5 or 2.6 compatible files, don't use free list entry in file (use local copy only)
               if ( i4->dataFile->compatibility != 30 )
               {
                  // if the block is at the 'eof' position, then lets shrink the index file...
                  B4NODE currentLastBlock ;
                  b4nodeAssignNode( &currentLastBlock, i4->eof ) ;
                  b4nodeSubtractBlocks( &currentLastBlock, i4, 1 ) ;
                  if ( b4node( currentLastBlock ) == b4node( blockNo ) )
                  {
                     b4nodeAssignNode( &i4->eof, blockNo ) ;
                     // may be able to make the file shorter yet, if the next avail block is now at file end...
                     b4nodeAssignNode( &currentLastBlock, i4->eof ) ;
                     b4nodeSubtractBlocks( &currentLastBlock, i4, 1 ) ;
                     if ( b4node( currentLastBlock ) == b4node( i4->tagIndex->availBlock ) )
                     {
                        b4nodeAssignNode( &i4->eof, i4->tagIndex->availBlock ) ;
                        b4nodeAssignLong( &i4->tagIndex->availBlock, 0 ) ;
                     }
                     // and now shrink the index file...
                     FILE4LONG pos ;
                     b4nodeGetFilePosition( i4, i4->eof, &pos ) ;

                     int rc = file4lenSetLow( &i4->file, pos ) ;
                     if ( rc < 0 )
                        return error4stack( r4->codeBase, (short)rc, E92102 ) ;
                  }
                  else
                  {
                     // to avoid bloat, make the link available internally...
                     b4nodeAssignNode( &i4->tagIndex->availBlock, blockNo ) ;
                     writeFreeList = 0 ;
                  }
               }
            #endif /* S4COMIX_NO_EOF */
            if ( writeFreeList == 1 )
            {
               #ifdef S4BYTE_SWAP
                  i4->tagIndex->header.freeList.node = x4reverseLong( (void *)&i4->tagIndex->header.freeList ) ;
               #endif
               #ifdef E4DEBUG
                  // the blocks should be on a B4BLOCK_SIZE boundary...
                  // unsigned long blockVal = blockNo / 512 ;
                  // blockVal *= 512 ;
                  // assert5( blockVal == blockNo ) ;
               #endif
               FILE4LONG pos ;
               b4nodeGetFilePosition( i4, blockNo, &pos ) ;
               #ifdef S4COMIX
                  // comix uses bytes 5-8 of the block for the free list...
                  if ( i4->dataFile->compatibility != 30 )
                  {
                     file4longAdd( &pos, 4 ) ;
                  }
               #endif
               int rc = file4writeInternal( &i4->file, pos, (char *)&i4->tagIndex->header.freeList, sizeof(i4->tagIndex->header.freeList) ) ;
               if ( rc < 0 )
                  return error4stack( i4->codeBase, (short)rc, E91708 ) ;
               b4nodeAssignNode( &i4->tagIndex->header.freeList, blockNo ) ;
            }
         #else
            #ifdef S4BYTE_SWAP
               i4->header.freeList = x4reverseLong( (void *)&i4->header.freeList ) ;
            #endif
            FILE4LONG pos ;
            b4nodeGetFilePosition( i4, blockNo, &pos ) ;
            file4longAdd( &pos, sizeof(S4LONG) ) ;
            int rc = file4writeInternal( &i4->file, pos, (char *)&i4->header.freeList, sizeof(i4->header.freeList) ) ;
            if ( rc < 0 )
               return error4stack( i4->codeBase, rc, E91708 ) ;
            i4->header.freeList = blockNo ;
         #endif

         return 0 ;
      }
   #endif /* #if !defined( S4CLIPPER ) && !defined( S4CLIENT ) && !defined( S4OFF_WRITE ) */



   #if !defined( S4CLIPPER ) && !defined( S4CLIENT ) && !defined( S4OFF_WRITE )
      int i4updateHeader( INDEX4 *i4 )
      {
         /* Updates the header if the version has changed */
         return index4updateHeader( i4->indexFile ) ;
      }
   #endif /* #if !defined( S4CLIPPER ) && !defined( S4CLIENT ) && !defined( S4OFF_WRITE ) */



   #if !defined( S4CLIPPER ) && !defined( S4CLIENT ) && !defined( S4OFF_WRITE )
      int index4updateHeader( INDEX4FILE *i4 )
      {
         FILE4LONG pos ;
         #ifdef S4MDX
            int rc ;
            TAG4FILE *tagOn ;
            #ifdef S4BYTE_SWAP
               I4HEADER swap ;
            #endif
         #endif

         #ifdef E4PARM_LOW
            if ( i4 == 0 )
               return error4( 0, e4parm_null, E91710 ) ;
         #endif

         if ( error4code( i4->codeBase ) < 0 )
            return e4codeBase ;

         // AS Dec 31/02 - added non-updating temporary indexes
         if ( i4->nonUpdateable == 1 )
            return 0 ;

         #ifdef S4FOX
            if ( i4->tagIndex == 0 )   /* index file not complete */
               return 0 ;

            if ( i4->versionOld != i4->tagIndex->header.version )
            {
               #ifdef S4BYTE_SWAP
                  i4->tagIndex->header.root.node = x4reverseLong( (void *)&i4->tagIndex->header.root ) ;
                  i4->tagIndex->header.freeList.node = x4reverseLong( (void *)&i4->tagIndex->header.freeList ) ;
                  i4->tagIndex->header.keyLen = x4reverseShort( (void *)&i4->tagIndex->header.keyLen ) ;
               #else
                  i4->tagIndex->header.version = (unsigned long)x4reverseLong( (void *)&i4->tagIndex->header.version ) ;
               #endif

               /* LY July 7/03 : changed from 0 to 0L for Linux compiler */
               file4longAssign( pos, 0, 0L ) ;
               if ( file4writeInternal( &i4->file, pos, (char *)&i4->tagIndex->header, LEN4HEADER_WR ) < 0 )
                  return -1;

               #ifdef S4BYTE_SWAP
                  i4->tagIndex->header.root.node = x4reverseLong( (void *)&i4->tagIndex->header.root ) ;
                  i4->tagIndex->header.freeList.node = x4reverseLong( (void *)&i4->tagIndex->header.freeList ) ;
                  i4->tagIndex->header.keyLen = x4reverseShort( (void *)&i4->tagIndex->header.keyLen ) ;
               #else
                  i4->tagIndex->header.version = (unsigned long)x4reverseLong( (void *)&i4->tagIndex->header.version ) ;
               #endif
               i4->versionOld = i4->tagIndex->header.version ;
               i4->versionReadUnlocked = i4->versionOld ;
            }
         #else
            if ( i4->changed )
            {
               #ifdef S4BYTE_SWAP
                  memcpy( (void *)&swap, (void *)&i4->header, sizeof(I4HEADER) ) ;

                  swap.blockChunks = x4reverseShort( (void *)&swap.blockChunks ) ;
                  swap.blockRw = x4reverseShort( (void *)&swap.blockRw ) ;
                  swap.slotSize = x4reverseShort( (void *)&swap.slotSize ) ;
                  swap.numTags = x4reverseShort( (void *)&swap.numTags ) ;
                  swap.eof = x4reverseLong( (void *)&swap.eof ) ;
                  swap.freeList = x4reverseLong( (void *)&swap.freeList ) ;

                  rc = file4write( &i4->file, 0L, (char *)&swap, sizeof( I4HEADER ) ) ;
                  if ( rc < 0 )
                     return error4stack( i4->codeBase, rc, E91710 ) ;
               #else
                  file4longAssign( pos, 0, 0 ) ;
                  rc = file4writeInternal( &i4->file, pos, (char *)&i4->header, sizeof( I4HEADER ) ) ;
                  if ( rc < 0 )
                     return error4stack( i4->codeBase, rc, E91710 ) ;
               #endif
               i4->changed = 0 ;
               for( tagOn = 0 ;; )
               {
                  tagOn = (TAG4FILE *)l4next( &i4->tags, tagOn ) ;
                  if ( tagOn == 0 )
                     break ;
                  if ( tagOn->changed == 1 )
                  {
                     tagOn->header.version++ ;
                     tagOn->changed = 0 ;
                     b4nodeGetFilePosition( i4, tagOn->headerOffset, &pos ) ;
                     file4longAdd( &pos, 20L ) ;
                     rc = file4writeInternal( &i4->file, pos, &tagOn->header.version, sizeof(char) ) ;
                     if ( rc < 0 )
                        return error4stack( i4->codeBase, rc, E91710 ) ;
                     if ( tagOn->hadKeys != tagOn->hasKeys )
                     {
                        /* just update the tag to record that it has keys (no other update yet) */
                        b4nodeGetFilePosition( i4, tagOn->headerOffset, &pos ) ;
                        file4longAdd( &pos, 222 + sizeof( T4HEADER ) ) ;
                        rc = file4writeInternal( &i4->file, pos, &tagOn->hasKeys, sizeof(char) ) ;
                        if ( rc < 0 )
                           return error4stack( i4->codeBase, rc, E91710 ) ;
                        tagOn->hadKeys = tagOn->hasKeys ;
                     }
                  }
               }
            }
         #endif

         return 0 ;
      }
   #endif /* #if !defined( S4CLIPPER ) && !defined( S4CLIENT ) && !defined( S4OFF_WRITE ) */



   #if !defined( S4CLIPPER ) && !defined( S4CLIENT )
      #ifdef P4ARGS_USED
         #pragma argsused
      #endif
      int S4FUNCTION i4versionCheck( INDEX4 *i4, const int doSeek, const int updateVersion )
      {
         #ifndef S4SINGLE
            TAG4 *saveTag ;
            TAG4FILE *tagOn ;
            int needSeek ;
            B4BLOCK *b4 ;
            int rc ;

            #ifdef E4PARM_LOW
               if ( i4 == 0 )
                  return error4( 0, e4parm_null, E91711 ) ;
            #endif

            if ( error4code( i4->codeBase ) < 0 )
               return e4codeBase ;

            // AS Oct 11/05 - new paramater to improve performance...pass selected tag in if doing lookup for reading only...
            saveTag = d4tagSelected( i4->data ) ;

            // AS Aug 4/06 - If the tag is not associated with this index, just mark it as null...we don't need to re-read it in this case
            // happened if a data file had 2 open indexes.
            if ( saveTag != 0 )  // AS Aug 28/06 - avoid gpf
               if ( saveTag->index != i4 )
                  saveTag = 0 ;

            // AS Oct 11/05 - new paramater to improve performance...pass selected tag in if doing lookup for reading only...
            rc = index4versionCheck( i4->indexFile, updateVersion, (doSeek == 1 ? (saveTag) : (0)) ) ;
            if ( rc < 0 )
               return error4stack( 0, (short)rc, E91711 ) ;
            if ( rc == 0 )   /* version did not change */
               return 0 ;

            needSeek = 0 ;
            if ( saveTag != 0 )
            {
               #ifdef I4PRINT
                  #ifdef I4PRINT_TAG_NAME
                     if ( stricmp( saveTag->tagFile->alias, I4PRINT_TAG_NAME ) == 0 )
                  #endif
                  {
                     log5( "saving block position\r\n" ) ;
                  }
               #endif
               /* remember the old position */
               if ( doSeek )
               {
                  b4 = (B4BLOCK *)saveTag->tagFile->blocks.lastNode ;
                  if ( b4 != 0 )
                  {
                     if ( tfile4eof( saveTag->tagFile ) )
                        needSeek = 2 ;
                     else
                        #ifdef S4FOX
                           if ( b4leaf( b4 ) && b4numKeys( b4 ) != 0 )
                        #else
                           if ( b4leaf( b4 ) && b4numKeys( b4 ) != 0 && b4->keyOn < b4numKeys( b4 ) )
                        #endif
                           {
                              // AS Jun 8/04 - support for larger key sizes
                              #ifdef S4FOX
                                 int len = saveTag->tagFile->header.keyLen + sizeof(S4LONG) ;
                              #else
                                 int len = saveTag->tagFile->header.keyLen + 2 * sizeof(S4LONG) ;
                              #endif
                              if ( i4->codeBase->savedKeyLen < (unsigned)len )  // AS Jul 12/04 - compile warning fix
                              {
                                 if ( u4allocAgain( i4->codeBase, &i4->codeBase->savedKey, &i4->codeBase->savedKeyLen, len ) < 0 )
                                    return e4memory ;
                              }

                              memcpy( i4->codeBase->savedKey, (void *)(b4key( b4, b4->keyOn )), len ) ;
                              needSeek = 1 ;
                           }
                  }
               }
            }

            for ( tagOn = 0 ;; )
            {
               tagOn = (TAG4FILE *)l4next( &i4->indexFile->tags, tagOn ) ;
               if ( tagOn == 0 )
                  break ;
               if ( tfile4freeAll( tagOn ) < 0 )  /* Should be a memory operation only */
                  return error4( i4->codeBase, e4result, E91711 ) ;
            }

            switch ( needSeek )
            {
               case 1:
                  #ifdef I4PRINT
                     #ifdef I4PRINT_TAG_NAME
                        if ( stricmp( saveTag->tagFile->alias, I4PRINT_TAG_NAME ) == 0 )
                     #endif
                     {
                        log5( "re-going to saved position\r\n" ) ;
                     }
                  #endif
                  tfile4go( saveTag->tagFile, ((B4KEY_DATA *)i4->codeBase->savedKey)->value, ((B4KEY_DATA *)i4->codeBase->savedKey)->num, 0 ) ;
                  break ;
               case 2:
                  #ifdef I4PRINT
                     #ifdef I4PRINT_TAG_NAME
                        if ( stricmp( saveTag->tagFile->alias, I4PRINT_TAG_NAME ) == 0 )
                     #endif
                     {
                        log5( "going to eof position\r\n" ) ;
                     }
                  #endif
                  tfile4goEof( saveTag->tagFile ) ;
                  break ;
               default:
                  break ;
            }
         #endif

         return 0 ;
      }
   #endif /* #if !defined( S4CLIPPER ) && !defined( S4CLIENT ) */



   #if !defined( S4CLIPPER ) && !defined( S4CLIENT )
      #ifdef P4ARGS_USED
         #pragma argsused
      #endif
      // AS Oct 11/05 - new paramater to improve performance...pass selected tag in if doing lookup for reading only...
      // if tagForReadOnly is not null, only that tag is checked (dBase version, avoid lookups for every tag)
      // !S4CLIPPER, !S4CLIENT
      int index4versionCheck( INDEX4FILE *i4, const int updateVersion, const TAG4 *tagForReadOnly )
      {
         /* Reads the header, checks the version to see if the blocks need to be freed. */
         /* returns 1 if version needs to be updated, else 0 */
         #ifdef S4SINGLE
            return 0 ;
         #else
            #ifndef S4FOX
               TAG4FILE *tagOn ;
            #endif
            int rc ;
            FILE4LONG pos ;

            #ifdef E4PARM_LOW
               if ( i4 == 0 )
                  return error4( 0, e4parm_null, E91712 ) ;
            #endif

            if ( error4code( i4->codeBase ) < 0 )
               return e4codeBase ;

            if ( index4lockTest( i4 ) == 1 )
            {
               #ifdef I4PRINT
                  struct _timeb timebuffer;
                  char *timeline;
                  char timeBuf[255];

                  _ftime( &timebuffer );
                  timeline = ctime( & ( timebuffer.time ) );

                  sprintf( timeBuf, "time: %.19s.%hu %s", timeline, timebuffer.millitm, &timeline[20] );

                  // struct _timeb mTime ;
                  // char *outTime, dump[255] ;
                  // _ftime( &mTime ) ;
                  // outTime = ctime( &( mTime.time ) ) ;

                  char dump[255] ;

                  #ifdef S4FOX
                     sprintf( dump, "id = %ld, version = %ld", (long)i4, (long)i4->versionOld ) ;
                  #else
                     sprintf( dump, "id = %ld", (long)i4 ) ;
                  #endif

                  strcat( dump, "index4versionCheck::index already locked, time: " ) ;
                  strcat( dump, timeBuf ) ;
                  strcat( dump, "\r\n" ) ;
                  log5( dump );
               #endif
               return 0 ;
            }

            #ifndef S4OFF_OPTIMIZE
               /* make sure read from disk unless file locked, etc. */
               if ( i4->file.doBuffer )  /* also makes sure 'opt' should exist */
                  i4->codeBase->opt.forceCurrent = 1 ;
            #endif

            /* LY July 7/03 : changed from 0 to 0L for Linux compiler */
            file4longAssign( pos, 0, 0L ) ;
            #ifdef S4FOX
               rc = file4readAllInternal( &i4->file, pos, &i4->tagIndex->header, LEN4HEADER_WR ) ;
               #ifndef S4OFF_OPTIMIZE
                  if ( i4->file.doBuffer )
                     i4->codeBase->opt.forceCurrent = 0 ;
               #endif
               if ( rc < 0 )
                  return error4stack( i4->codeBase, (short)rc, E91712 ) ;

               #ifdef S4BYTE_SWAP
                  i4->tagIndex->header.root.node = x4reverseLong( (void *)&i4->tagIndex->header.root ) ;
                  i4->tagIndex->header.freeList.node = x4reverseLong( (void *)&i4->tagIndex->header.freeList ) ;
                  /* version is already in non-intel form */
                  i4->tagIndex->header.keyLen = x4reverseShort( (void *)&i4->tagIndex->header.keyLen ) ;
               #else
                  i4->tagIndex->header.version = (unsigned long)x4reverseLong( (void *)&i4->tagIndex->header.version ) ;
               #endif

               #ifdef I4PRINT
                  struct _timeb timebuffer;
                  char *timeline;
                  char timeBuf[255];

                  _ftime( &timebuffer );
                  timeline = ctime( & ( timebuffer.time ) );

                  sprintf( timeBuf, "time: %.19s.%hu %s", timeline, timebuffer.millitm, &timeline[20] );


                  // struct _timeb mTime ;
                  // char *outTime, dump[255] ;
                  // _ftime( &mTime ) ;
                  // outTime = ctime( &( mTime.time ) ) ;
                  char dump[255];

                  sprintf( dump, "index4versionCheck: id = %ld, updateVersion = %ld, versionOld = %ld versionNew = %ld, time: %s\r\n", (long)i4, (long)updateVersion, (long)i4->versionOld, (long)i4->tagIndex->header.version, timeBuf ) ;
                  log5( dump ) ;
               #endif

               if ( i4->tagIndex->header.version == i4->versionOld )
                  return 0 ;

               if ( updateVersion == 0 )
               {
                  /* AS 06/13/97 if updateVersion false, then reading, so if version
                     == to read value, allow to continue, just reset regular version */
                  if ( i4->tagIndex->header.version == i4->versionReadUnlocked )
                  {
                     i4->tagIndex->header.version = i4->versionOld ;
                     return 0 ;
                  }

                  /* AS 06/13/97 Keep a read version which is based on last read here, but not
                     guaranteed for write operation since the write-order of the index
                     file is not guaranteed (version updated first or after index file),
                     which can only be verified if the index file is locked */
                  i4->versionReadUnlocked = i4->tagIndex->header.version ;
                  i4->tagIndex->header.version = i4->versionOld ;
               }
               else
               {
                  i4->versionOld = i4->tagIndex->header.version ;
                  i4->versionReadUnlocked = i4->versionOld ;
               }
            #else
               rc = file4readAllInternal( &i4->file, pos, &i4->header, sizeof(I4HEADER) ) ;
               #ifndef S4OFF_OPTIMIZE
                  if ( i4->file.doBuffer )
                     i4->codeBase->opt.forceCurrent = 0 ;
               #endif
               if ( rc < 0 )
                  return error4stack( i4->codeBase, rc, E91712 ) ;

               #ifdef S4BYTE_SWAP
                  i4->header.blockChunks = x4reverseShort( (void *)&i4->header.blockChunks ) ;
                  i4->header.blockRw = x4reverseShort( (void *)&i4->header.blockRw ) ;
                  i4->header.slotSize = x4reverseShort( (void *)&i4->header.slotSize ) ;
                  i4->header.numTags = x4reverseShort( (void *)&i4->header.numTags ) ;
                  i4->header.eof = x4reverseLong( (void *)&i4->header.eof ) ;
                  i4->header.freeList = x4reverseLong( (void *)&i4->header.freeList ) ;
               #endif
               #ifndef S4OFF_OPTIMIZE
                  /* make sure read from disk unless file locked, etc. */
                  if ( i4->file.doBuffer )  /* also makes sure 'opt' should exist */
                    i4->codeBase->opt.forceCurrent = 1 ;
               #endif
               for( tagOn = 0 ;; )
               {
                  // AS Oct 11/05 - new paramater to improve performance...pass selected tag in if doing lookup for reading only...
                  if ( tagForReadOnly != 0 )
                     tagOn = tagForReadOnly->tagFile ;
                  else
                     tagOn = (TAG4FILE *)l4next( &i4->tags, tagOn ) ;

                  // tagOn = (TAG4FILE *)l4next( &i4->tags, tagOn ) ;
                  if ( tagOn == 0 )
                     break ;

                  /* AS 07/30/98 not updating root, can cause problems because
                        in some cases here it is not set from other user, and
                        thus gets corrupted, esp. if we lock the index
                  */

                  b4nodeGetFilePosition( i4, tagOn->headerOffset, &pos ) ;
                  rc = file4readAllInternal( &i4->file, pos, &tagOn->header.root, sizeof( S4LONG ) ) ;
                  if ( rc < 0 )
                  {
                     #ifndef S4OFF_OPTIMIZE
                        if ( i4->file.doBuffer )
                           i4->codeBase->opt.forceCurrent = 0 ;
                     #endif
                     return error4stack( i4->codeBase, rc, E91712 ) ;
                  }
                  #ifdef S4BYTE_SWAP
                     tagOn->header.root = x4reverseLong( (void *)&tagOn->header.root ) ;
                  #endif

                  /* AS 07/30/98 end of changes, inserted lines between comments */

                  b4nodeGetFilePosition( i4, tagOn->headerOffset, &pos ) ;
                  file4longAdd( &pos, 20L ) ;
                  rc = file4readAllInternal( &i4->file, pos, &tagOn->header.version, sizeof(char) ) ;
                  if ( rc < 0 )
                  {
                     #ifndef S4OFF_OPTIMIZE
                        if ( i4->file.doBuffer )
                           i4->codeBase->opt.forceCurrent = 0 ;
                     #endif
                     return error4stack( i4->codeBase, rc, E91712 ) ;
                  }
                  b4nodeGetFilePosition( i4, tagOn->headerOffset, &pos ) ;
                  file4longAdd( &pos, 222 + sizeof( T4HEADER ) ) ;
                  rc = file4readAllInternal( &i4->file, pos, &tagOn->hasKeys, sizeof(char) ) ;
                  if ( rc < 0 )
                  {
                     #ifndef S4OFF_OPTIMIZE
                        if ( i4->file.doBuffer )
                           i4->codeBase->opt.forceCurrent = 0 ;
                     #endif
                     return error4stack( i4->codeBase, rc, E91712 ) ;
                  }
                  tagOn->hadKeys = tagOn->hasKeys ;
                  if ( tagForReadOnly != 0 )
                     break ;
               }

               #ifndef S4OFF_OPTIMIZE
                  if ( i4->file.doBuffer )
                     i4->codeBase->opt.forceCurrent = 0 ;
               #endif
            #endif
            return 1 ;
         #endif
      }
   #endif /* #if !defined( S4CLIPPER ) && !defined( S4CLIENT ) */



   #ifndef S4CLIENT
      // !S4CLIENT
      // need a special version check
      int S4FUNCTION tfile4versionCheckFree( TAG4FILE *t4 )
      {
         #ifdef S4CLIPPER
            return tfile4doVersionCheck( t4, 0, 0 ) ;
         #else
            int rc ;
            TAG4FILE *tagOn ;
            INDEX4FILE *i4;

            i4 = t4->indexFile ;
            // AS Oct 11/05 - new paramater to improve performance...pass selected tag in if doing lookup for reading only...
            rc = index4versionCheck( i4, 0, 0 ) ;
            if ( rc <= 0 )
              return rc ;
            // else means version changed

            for ( tagOn = 0 ;; )
            {
              tagOn = (TAG4FILE *)l4next( &i4->tags, tagOn ) ;
              if ( tagOn == 0 )
                 break ;
              if ( tfile4freeAll( tagOn ) < 0 )  /* Should be a memory operation only */
                 return error4( i4->codeBase, e4result, E91711 ) ;
            }

            return 0 ;
         #endif
      }
   #endif

   #ifdef S4CLIPPER
      /* This function closes all the tags corresponding with the index */
      // S4CLIPPER
      int S4FUNCTION i4closeLow( INDEX4 *i4 )
      {
         TAG4 *tagOn ;
         int oldOpenErr, rc ;
         char buf[258] ;
         #ifndef S4SINGLE
            int saveAttempts ;
         #endif

         CODE4 *c4 = i4->codeBase ;

         int finalRc = error4code( c4 ) ;
         #ifndef S4OFF_WRITE
            if ( i4->data )
               if ( d4update( i4->data ) < 0 )
                  finalRc = error4set( c4, 0 ) ;
         #endif

         #ifndef S4SINGLE
            saveAttempts = c4->lockAttempts ;
            c4->lockAttempts = WAIT4EVER ;
            if ( i4unlock(i4) < 0 )
               finalRc = error4set( c4, 0 ) ;
         #endif

         for( ;; )
         {
            tagOn = (TAG4 *)l4pop( &i4->tags ) ;
            if ( tagOn == 0 )
               break ;
            if ( i4->data->tagSelected == tagOn )   /* can't have a non-existant tag selected */
               i4->data->tagSelected = 0 ;

            // AS Oct 20/04 - need an internal close to bypass the transaction check for internal close requests (temporary files)
            t4closeLow( tagOn ) ;
            tagOn = 0 ;
         }

         // AS 11/27/00 - doRemove must be set on client structure
         // if ( c4->doRemove == 1 )
         if ( c4getDoRemove( c4 ) == 1 )
         {
            oldOpenErr = c4->errOpen ;
            c4->errOpen = 0 ;
            // AS 06/14/99 -- wasn't working if index file not in current directory...
            char dataFilePath[LEN4PATH] ;
            assert5( i4->data->dataFile->file.name != 0 ) ;
            u4namePath( dataFilePath, sizeof( dataFilePath ), i4->data->dataFile->file.name ) ;
            u4nameCurrentExtended( buf, sizeof( buf ), i4->accessName, dataFilePath ) ;
            u4nameExt( buf, sizeof( buf ), GROUP4EXT, 1 ) ;
            // AS May 24/02 - created file4openLow for internal use to indicate file types
            rc = file4openInternal( &i4->file, c4, buf, 0, OPT4INDEX ) ;
            if ( rc == 0 )
            {
               // AS Sep. 17/01 - changed isTemp to be done via a function for code4validate() capability
               file4setTemporary( &i4->file, 1, 0 ) ;
               file4close( &i4->file ) ;
            }
            c4->errOpen = oldOpenErr ;
         }

         if ( i4->data )
            l4remove( &i4->data->indexes, i4 ) ;

         mem4free( c4->indexMemory, i4 ) ;
         i4 = 0 ;
         #ifndef S4SINGLE
            c4->lockAttempts = saveAttempts ;
         #endif
         error4set( c4, finalRc ) ;
         return  finalRc ;
      }



      #ifndef S4OFF_WRITE
         /* This function flushes all the tags corresponding with the index */
         /* S4CLIPPER, not S4OFF_WRITE */
         int i4flush( INDEX4 *i4 )
         {
            TAG4 *tagOn ;
            int rc ;

            #ifdef E4PARM_LOW
               if ( i4 == 0 )
                  return error4( 0, e4parm_null, E91713 ) ;
            #endif

            #ifndef S4SINGLE
               if ( i4->data->dataFile->indexLocked == 1 )
            #endif
               for ( tagOn = 0 ;; )
               {
                  tagOn = (TAG4 *)l4next( &i4->tags, tagOn ) ;
                  if ( tagOn == 0 )
                     break ;
                  rc = tfile4flush( tagOn->tagFile ) ;
                  if ( rc < 0 )
                     return error4stack( i4->codeBase, rc, E91713 ) ;
                  tagOn->tagFile->header.root = INVALID4BLOCK_ID ;
               }
               return 0 ;
         }



         /* S4CLIPPER, not S4OFF_WRITE */
         int i4update( INDEX4 *i4 )
         {
            TAG4 *tagOn ;
            int rc ;

            #ifdef E4PARM_LOW
               if ( i4 == 0 )
                  return error4( 0, e4parm_null, E91705 ) ;
            #endif

            if ( error4code( i4->codeBase ) < 0 )
               return e4codeBase ;

            #ifndef S4SINGLE
               if ( i4->data->dataFile->indexLocked == 1 )
            #endif
               for ( tagOn = 0 ;; )
               {
                  tagOn = (TAG4 *)l4next( &i4->tags, tagOn ) ;
                  if ( tagOn == 0 )
                     break ;
                  rc = tfile4update( tagOn->tagFile ) ;
                  if ( rc < 0 )
                     return error4stack( i4->codeBase, rc, E91705 ) ;
                  tagOn->tagFile->header.root = INVALID4BLOCK_ID ;
               }
               return 0 ;
         }
      #endif /* not S4OFF_WRITE */


      #ifndef S4CLIENT
         // AS Apr 24/06 - fix for encryption (was applied in cdx/mdx, not clipper)
         // CS 2008/03/20 function returns int
         static int i4openLocalLow( INDEX4 *i4, DATA4 *d4, CODE4 *c4, char *buf )
         {
            FILE4SEQ_READ seqread ;
            char buffer[1024], tNames[258], firstByte, tagBuf[258] ;
            FILE4LONG tPos ;

            #ifdef S4SERVER
               if ( c4->createTemp == 1 )
                  file4setTemporary( &i4->file, 1, 0 ) ;
            #endif

            file4longAssign( tPos, 0, 0 ) ;
            file4seqReadInitDo( &seqread, &i4->file, tPos, buffer, sizeof(buffer), 0 ) ;

            int pos = 0L ;
            int rc = file4seqReadAll( &seqread, &firstByte, sizeof( firstByte ) ) ;
            if ( rc )
               return rc ;

            #ifdef S4CB51
               int numFiles ;
               // AS 02/07/00 if not compiling with CB51 backwards compatibility, assume old group file not possible.
               if ( firstByte < 65 )   /* old format - potential problem if >= 65 files in an old format file. */
               {
                  numFiles = firstByte ;
                  if ( file4seqReadAll( &seqread, &firstByte, sizeof( firstByte ) ) )
                     return -1 ;

                  for ( int i = 0 ; i < numFiles ; i++ )
                  {
                     if ( file4seqReadAll( &seqread, &len, sizeof( len ) ) )
                        return -1 ;
                     }
                     #ifdef E4MISC
                        if ( (unsigned)len > file4lenLow( &i4->file ) )  /* must be bad file type */
                           return error4( c4, e4info, E81709 ) ;
                     #endif
                     pos += sizeof( len ) ;
                     rc = u4namePath( tagBuf, sizeof( tagBuf ), buf ) ;
                     tagBuf[rc+len] = '\0' ;
                     if ( sizeof( tagBuf ) > rc + len ) /* make sure room to read */
                        rc = file4seqReadAll( &seqread, tagBuf+rc, len ) ;
                     else
                        rc = -1 ;

                     if ( rc )
                        return rc ;

                     pos += len ;
                     if  ( t4open( d4, i4, tagBuf ) == 0 )
                        return -1 ;
                  }

                  file4close( &i4->file ) ;
               }
               else
            #endif
            {
               file4longAssign( tPos, 0, 0 ) ;
               file4seqReadInitDo( &seqread, &i4->file, tPos, buffer, sizeof(buffer), 0 ) ;
               int saveLen = 0 ;

               int len ;
               for( len = sizeof( tNames ) ; len == sizeof( tNames ) ; )
               {
                  len = file4seqRead( &seqread, tNames, sizeof( tNames )) ;
                  if ( len < sizeof( tNames ) )
                  {
                     if ( len == 0 )
                        break ;
                  }
                  int iPos = 0 ;  // CS 2006/06/22 Declare outside of for loop.
                  for( iPos = 0, pos = 0 ; pos < len ; )
                  {
                     switch( tNames[pos] )
                     {
                        /* cases where the values are ignored, or found name */
                        // AS 01/24/00 - 'blanks' should be considered valid after all.
                        // for example, can have directory names with blanks in them...
                        // this was causing a problem when I had files in a directory
                        // that had blanks in them.
                        // case ' ':
                        case '\r':
                        case '\n':
                        case '\t':
                        case '\x1A':
                           if ( iPos < pos )  /* try to open the file */
                           {
                              int tempLen = pos - iPos ;
                              if ( saveLen == 0 )
                              {
                                 rc = u4namePath( tagBuf, sizeof( tagBuf ), buf ) ;
                                 tagBuf[rc + tempLen] = '\0' ;
                              }
                              else
                                 rc = saveLen ;

                              // AS 06/14/99 --> problems when .cgp file includes full path info...
                              // memcpy( tagBuf + rc, &tNames[iPos], tempLen ) ;
                              tNames[tempLen+iPos] = 0 ;
                              /* LY 00/11/23 : if index name spans size of tNames,
                              first half of index name not being used */
                              char tagNameWithPath[LEN4PATH] ;
                              if ( saveLen > 0 )
                              {
                                 c4strcpy( tagNameWithPath, sizeof( tagNameWithPath ), tagBuf ) ;   // AS Jan 3/06 VS 2005 clipper fix
                                 c4strcat( tagNameWithPath, sizeof( tagNameWithPath ), &tNames[iPos] ) ;   // AS Jan 3/06 VS 2005 clipper fix
                                 saveLen = 0 ;
                              }
                              else
                              {
                                 u4nameCurrentExtended( tagNameWithPath, sizeof( tagNameWithPath ), &tNames[iPos], buf ) ;
                              }

                              if  ( t4open( d4, i4, tagNameWithPath ) == 0 )
                                 return -1 ;
                           }
                           iPos = ++pos ;
                           break ;

                        /* case where a name is attempted to be read in */
                        default:
                           pos++ ;
                     }
                  }
                  int tempLen = pos - iPos ;
                  rc = u4namePath( tagBuf, sizeof( tagBuf ), buf ) ;
                  tagBuf[rc+tempLen] = '\0' ;
                  // AS 06/14/99 --> problems when .cgp file includes full path info...
                  // memcpy( tagBuf + rc, &tNames[iPos], tempLen ) ;
                  tNames[tempLen+iPos] = 0 ;
                  char tagNameWithPath[LEN4PATH] ;
                  u4nameCurrentExtended( tagNameWithPath, sizeof( tagNameWithPath ), &tNames[iPos], buf ) ;
                  c4strcpy( tagBuf, sizeof( tagBuf ), tagNameWithPath ) ;   // AS Jan 3/06 VS 2005 clipper fix
                  saveLen = rc + tempLen ;
               }

               if ( ( saveLen - rc ) > 0 )  /* try to open the file */
               {
                  if ( t4open( d4, i4, tagBuf ) == 0 )
                     return -1 ;
               }
            }

            return 0 ;
         }

         /* S4CLIPPER, !S4CLIENT */
         static INDEX4 *i4openLocal( DATA4 *d4, const char *fileName )
         {
            INDEX4 *i4 ;
            CODE4 *c4 ;
            TAG4 *tag ;
            int rc ;
            char buf[258], ext[4] ;   /* LY 00/09/01 : make ext[4] for null terminator */

            c4 = d4->codeBase ;
            if ( error4code( c4 ) < 0 )
               return 0 ;

            // AS Jun 2/03 - Added recording if data file has a tag with an expression referring to a memo field
            d4->hasMemoExpr = r4uninitializedMemoTagExpression ;  // just reset, it gets set automatically if required

            #ifdef S4STAND_ALONE
               // AS Jun 13/06 - if the index is already open, don't allow it to be opened again...otherwise for example it could get
               // updated twice if records are added...
               // not sure about server case, possibly alrady handled elsewhere
               if ( fileName == 0 )
                  u4ncpy( buf, d4->dataFile->file.name, sizeof( buf ) - 1 ) ;
               else
               {
                  int rc = u4nameCurrent( (char *)buf, sizeof( buf ), fileName ) ;
                  if ( rc < 0 )
                  {
                     error4( c4, rc, E94509 ) ;  /* from u4nameCurrent */
                     return 0 ;
                  }
               }

               u4nameExt( (char *)buf, sizeof( buf ), code4indexExtension( c4 ), ( fileName == 0 ? 1 : 0 ) ) ;

               i4 = d4index( d4, (char *)buf ) ;

               if ( i4 != 0 )
               {
                  #ifndef S4SERVER
                     /* allowed if current data4 does not have a pointer to index */
                     for ( INDEX4 *indexLoop = 0 ;; )
                     {
                        indexLoop = (INDEX4 *)l4next( &d4->indexes, indexLoop ) ;
                        if ( indexLoop == 0 )
                           break ;
                        if ( stricmp( indexLoop->nameWithPath, i4->nameWithPath ) == 0 )
                        {
                           error4( c4, e4instance, E91707 ) ;
                           return 0 ;
                        }
                     }
                  #endif
                  assert5( 0 ) ;   // how do we handle this?  There is no usercount
                  // i4->userCount++ ;
                  return i4 ;
               }
            #endif





            if ( fileName == 0 )
            {
               u4ncpy( buf, d4->dataFile->file.name, sizeof( buf ) - 1 ) ;
               u4nameExt( buf, sizeof( buf ), GROUP4EXT, 1 ) ;
            }
            else
            {
               u4ncpy( buf, fileName, sizeof( buf ) - 1 ) ;
               c4memset( ext, 0, 4 ) ;   /* LY 00/09/01 : ensure ext is null-terminated */
               rc = u4nameRetExt( ext, 3, buf ) ;
               if ( rc )  /* extension provided */
               {
                  if ( rc == 3 )
                  {
                     // AS 06/14/00 due to CS changes, case problems here, make case insensitive
                     // if ( c4memcmp( ext, TAG4EXT, 3 ) == 0 )
                     /* BCR 10/10/00 -- use c4stricmp for unix compatibility */
                     if ( c4stricmp( ext, TAG4EXT ) == 0 )
                     {
                        tag = t4open( d4, (INDEX4 *)0, fileName ) ;
                        if ( tag == 0 )
                           return 0 ;
                        // AS Feb 7/03 - Need for locking control as well as transactions
                        // #ifndef S4OFF_TRAN
                           tag->index->isValid = 1 ;
                        // #endif
                        return tag->index ;
                     }
                  }
               }
               else
                  u4nameExt( buf, sizeof(buf), GROUP4EXT, 0 ) ;
            }

            if ( c4->indexMemory == 0 )
            {
               c4->indexMemory = mem4create( c4, c4->memStartIndex, sizeof( INDEX4 ), c4->memExpandIndex, 0 ) ;
               if ( c4->indexMemory == 0 )
                  return 0 ;
            }

            if ( c4->tagMemory == 0 )
            {
               c4->tagMemory = mem4create( c4, c4->memStartTag, sizeof( TAG4 ), c4->memExpandTag, 0 ) ;
               if ( c4->tagMemory == 0 )
                  return 0 ;
            }

            if ( c4->tagFileMemory == 0 )
            {
               c4->tagFileMemory = mem4create( c4, c4->memStartTagFile, sizeof( TAG4FILE ), c4->memExpandTagFile, 0 ) ;
               if ( c4->tagFileMemory == 0 )
                  return 0 ;
            }

            i4 = (INDEX4 *)mem4allocZero( c4->indexMemory ) ;
            if ( i4 == 0 )
               return 0 ;

            // AS Jun 8/06 - store the full index path for retrieval with i4fileName
            u4nameCurrent( i4->nameWithPath, sizeof( i4->nameWithPath ), buf ) ;

            i4->codeBase = c4 ;
            i4->data = d4 ;
            l4add( &d4->indexes, i4 ) ;
            if ( fileName == 0 )  // AS 11/07/00 - accessName needs to include path so tags opened in correct location...
               u4namePiece( i4->accessName, sizeof( i4->accessName ), buf, 1, 0 ) ;
            else
               c4strcpy( i4->accessName, sizeof( i4->accessName ), fileName ) ;  // AS Jan 3/06 VS 2005 clipper fix
            c4upper( i4->accessName ) ;

            // AS Apr 24/06 - fix for encryption (was applied in cdx/mdx, not clipper)
            #ifdef S4PREPROCESS_FILE
               // AS 02/12/01 - want to turn off errOpen in case it is a preprocess problem.  If it is another problem, we
               // will catch it on the alternate open.
               int oldErrOff = c4->errOff ;
               // AS Aug 25/03 - Don't do this if the data file does not support preporcessing
               short oldPreprocess = code4getPreprocessFile( c4 ) ;
               if ( oldPreprocess == 0 && d4->dataFile->file.preprocessed == 1 )
                  c4->errOff = 1 ;
               // also change the preprocess setting if the data file is not preprocessed (don't open index as preprocessed in that case)
               if ( oldPreprocess == 1 && d4->dataFile->file.preprocessed == 0 )
                  code4setPreprocessFile( c4, 0 ) ;
            #endif

            // AS May 24/02 - created file4openLow for internal use to indicate file types
            rc =  file4openInternal( &i4->file, c4, buf, 1, OPT4INDEX ) ;
            if ( rc == 0 )
            {
               rc = i4openLocalLow( i4, d4, c4, buf ) ;
               file4close( &i4->file ) ;
            }
            #ifdef S4PREPROCESS_FILE
               code4setPreprocessFile( c4, oldPreprocess ) ;
               c4->errOff = oldErrOff ;
            #endif
            if ( rc != 0 )
            {
               #ifdef S4PREPROCESS_FILE
                  // AS 02/12/01 - If the open fails, it may be due to preprocessing, try opening the file in the opposite mode
                  // AS Aug 25/03 - Don't do this if the data file does not support preporcessing
                  if ( oldPreprocess == 0 && d4->dataFile->file.preprocessed == 1 )
                  {
                     code4setPreprocessFile( c4, ( oldPreprocess ? 0 : 1 ) ) ;  // just put to the opposite number
                     error4set( c4, 0 ) ;
                     rc =  file4openInternal( &i4->file, c4, buf, 1, OPT4INDEX ) ;
                     if ( rc == 0 )
                     {
                        rc = i4openLocalLow( i4, d4, c4, buf ) ;
                        file4close( &i4->file ) ;
                     }
                     code4setPreprocessFile( c4, oldPreprocess ) ;
                  }
                  if ( rc != 0 )
                  {
               #endif
                     i4closeLow( i4 ) ;
                     if ( c4->errOpen )
                        error4( c4, e4open, E81708 ) ;
                     // AS Jun 22/05 - at least for odbc, we need to return r4noOpen if r4noExist is returned (the index just couldn't
                     // be found)
                     // AS Jan 20/06 - update for client/server build
                     if ( error4code( c4 ) == r4noExist )
                        error4set( c4, r4noOpen ) ;
                     return 0 ;
               #ifdef S4PREPROCESS_FILE
                  }
               #endif
            }

            // AS Feb 7/03 - Need for locking control as well as transactions
            // #ifndef S4OFF_TRAN
               i4->isValid = 1 ;
            // #endif

            return i4 ;
         }
      #endif /* !S4CLIENT */



      /* S4CLIPPER */
      TAG4 *S4FUNCTION i4tag( INDEX4 *i4, const char *tagName )
      {
         char tagLookup[LEN4TAG_ALIAS+1] ;
         TAG4 *tagOn ;

         #ifdef E4VBASIC
            if ( c4parm_check( i4, 0, E91709 ) )
               return 0 ;
         #endif

         #ifdef E4PARM_HIGH
            if ( i4 == 0 || tagName == 0 )
            {
               error4( 0, e4parm_null, E91709 ) ;
               return 0 ;
            }
         #endif

         u4ncpy( tagLookup, tagName, sizeof( tagLookup ) - 1 ) ;
         c4upper( tagLookup ) ;

         for( tagOn = 0 ;; )
         {
            tagOn = (TAG4 *)l4next( &i4->tags, tagOn) ;
            if ( tagOn == 0 )
               break ;
            if ( c4strcmp( tagOn->tagFile->alias, tagLookup) == 0 )
               return tagOn ;
         }

         if ( i4->codeBase->errTagName )
            error4describe( i4->codeBase, e4tagName, E91709, tagName, 0, 0 ) ;
         return 0 ;
      }
   #endif  /* S4CLIPPER  */



   #if !defined(S4CLIPPER) && !defined(S4CLIENT)
      int S4FUNCTION index4isProduction( INDEX4FILE *i4 )
      {
         #ifdef S4FOX
            char ext[3] ;
            int l1, l2, count ;
         #endif
         #ifdef E4PARM_LOW
            if ( i4 == 0 )
               return error4( 0, e4parm_null, E91714 ) ;
         #endif

         #ifdef S4MDX
            /* AS 04/07/99 --> changed, was checking datafile not index marker in stand-alone mdx
                  #ifndef S4SERVER  // (old equivalent)
            */
            #ifdef S4CLIENT
               return i4->dataFile->openMdx ;
            #else
               return i4->header.isProduction ;
            #endif
         #endif

         #ifdef S4FOX
            /* AS 05/19/99 --> changed, was checking datafile not index marker in stand-alone fox
                  #ifdef S4SERVER  // (old equivalent)
            */
            #ifndef S4CLIENT
               // AS 03/06/00 -- hasMdxMemo for fox == 0x02 for memo, so can't examine directly
               #ifdef S4FOX
                  if ( i4->dataFile->hasMdxMemo & 0x01 )
               #else
                  if ( i4->dataFile->hasMdxMemo )
               #endif
            #else
               if ( i4->dataFile->openMdx )
            #endif
            {
               /* AS 08/04/97 fix #94 */
               /* first check extension */
               if ( u4nameRetExt( ext, sizeof( ext ), i4->file.name ) != 3 )  // does not match
                  return 0 ;
               if ( c4memcmp( code4indexExtension( i4->codeBase ), ext, 3 ) != 0 )
                  return 0 ;

               if ( i4->file.name == 0 )   /* most likely a failure during open/create now closing */
                  return 0 ;
               l1 = c4strlen( i4->file.name ) ;
               if ( l1 == 0 )
                  return 0 ;
               for ( count = l1 - 1 ;; count-- )
               {
                  if ( count <= 0 )
                     break ;
                  if ( i4->file.name[count] == '.' )
                  {
                     l1 = count ;
                     break ;
                  }
               }
               l2 = c4strlen( i4->dataFile->file.name ) ;
               if ( l2 == 0 )
                  return 0 ;
               for ( count = l2 - 1 ;; count-- )
               {
                  if ( count <= 0 )
                     break ;
                  if ( i4->dataFile->file.name[count] == '.' )
                  {
                     l2 = count ;
                     break ;
                  }
               }
               if ( l1 == l2 )
                  return( c4memcmp( i4->file.name, i4->dataFile->file.name, (unsigned)l1 ) ? 0 : 1 ) ;
            }
            return 0 ;
         #endif
      }
   #endif /* !S4CLIENT && !S4CLIPPER */



   #ifdef S4WIN32
      /* AS 07/21/99 - added extra parm for win 95/98 to avoid endless laze writes */
      /* AS 04/05/01 - if the client requests the open after a create and it is temp, mark it as such - added extra parm */
      INDEX4 *I4open( DATA4 *data, const char *name, char accessMode, int errDefaultUnique, char readOnly, char singleClient, char openForCreate, char fileFlush, char safety, char createTemp )
      {
         // AS 01/07/00 --> added setting CODE4.errorCode to r4noOpen if the index is not actually
         // opened (i.e. it was already open)
         CODE4 *c4 = data->codeBase ;
         if ( error4code( c4 ) < 0 )
            return 0 ;
         error4set( c4, 0 ) ;
         INDEX4 *index = d4index( data, name ) ;

         if ( index != 0 )
         {
            // indicate to caller that the index was not opened, but was already open
            error4set( c4, r4noOpen ) ;
            return index ;
         }

         #ifdef S4CLIPPER
            // AS 01/07/00 --> in clipper, it is possible that the input name refers to an '.ntx' file, and not
            // an index.  In that case we need to see if the tag is possibly already open, and if it is, and it
            // has a parent index with only 1 tag, then return that index.

            char ext[4] ;
            u4nameRetExt( ext, sizeof( ext ), name ) ;
            c4upper( ext ) ;
            if ( c4memcmp( ext, "NTX", 3 ) == 0 )
            {
               // create a tag file name to compare with...
               char tagFileName[LEN4PATH] ;
               u4nameCurrent( tagFileName, LEN4PATH, name ) ;
               for ( TAG4 *tagOn = 0 ;; )
               {
                  tagOn = d4tagNext( data, tagOn ) ;
                  if ( tagOn == 0 )  // done
                     break ;
                  // AS Need to consider that the case of the iAccessName and the i4file->accessName may differ
                  Bool5 isEqual = 0 ;
                  if ( c4->ignoreCase == 1 )
                  {
                     if ( c4stricmp( tagOn->tagFile->file.name, tagFileName ) == 0 )
                        isEqual = 1 ;
                  }
                  else
                  {
                     if ( c4strcmp( tagOn->tagFile->file.name, tagFileName ) == 0 )
                        isEqual = 1 ;
                  }

                  if ( isEqual )
                  {
                     // indicate to caller that the index was not opened, but was already open
                     // AS 01/10/00  -- this occurs if client explicitly is opening index which was
                     // only previously opened via fact that index was open on server already through data
                     // file already open.  In that case, there is no index handle on the client side, so
                     // we need to ensure that the accessName here matches the input name here which is what
                     // the client will use...
                     c4strcpy( tagOn->index->accessName, sizeof( tagOn->index->accessName ), name ) ;  // AS Jan 3/06 VS 2005 clipper fix
                     error4set( c4, r4noOpen ) ;
                     return tagOn->index ;
                  }
               }
            }
         #endif

         int oldAccessMode = c4->accessMode ;

         switch( c4->accessMode )
         {
            case OPEN4DENY_NONE:
               c4->accessMode = accessMode ;
               break ;
            case OPEN4DENY_WRITE:
               if ( accessMode == OPEN4DENY_RW )
                  c4->accessMode = accessMode ;
               break ;
            default:
               break ;
         }

         #ifdef S4SERVER
            int oldSingleClient = c4->singleClient ;
            c4->singleClient = singleClient ;
            /* AS 04/05/01 - if the client requests the open after a create and it is temp, mark it as such */
            int oldCreateTemp = c4->createTemp ;
            if ( openForCreate == 1 )
               c4->createTemp = createTemp ;
            else
               c4->createTemp = 0 ;  // never open and set to temp if not an open for create
         #endif
         int oldUniqueError = c4->errDefaultUnique ;
         /* AS 07/21/99 - added for win 95/98 to avoid endless laze writes */
         int oldFileFlush = c4->fileFlush ;

         c4->errDefaultUnique = errDefaultUnique ;
         /* AS 07/21/99 - added for win 95/98 to avoid endless laze writes */
         c4->fileFlush = fileFlush ;

         index = i4open( data, name ) ;

         c4->fileFlush = oldFileFlush ;
         c4->errDefaultUnique = oldUniqueError ;
         c4->accessMode = oldAccessMode ;

         #ifdef S4SERVER
            c4->createTemp = oldCreateTemp ;
            c4->singleClient = oldSingleClient ;
            c4->readOnlyRequest = 0 ;
         #endif

         return index ;
      }
   #endif
#endif /* S4INDEX_OFF */



#if defined(S4BYTE_SWAP) && defined(S4CLIPPER)
   void index4swapBlockClipper(void *swap, int keysmax, int grouplen )
   {
      char *swapPtr = (char *) swap ;  /* LY 99/06/21 : explicit cast */
      int j ;
      #ifdef S4DATA_ALIGN
         S4LONG longVal[2] ;
      #endif

      for ( j = -1 ; j <= keysmax ; j++ ) /*-1 to handle nKeys */
      {
         *(short *)swapPtr = x4reverseShort( (void *)swapPtr ) ;
         swapPtr += sizeof(short) ;
      }

      #ifdef S4DATA_ALIGN
         if (   ( keysmax%2) || (grouplen%4) )
            for ( j = 0 ; j <= keysmax ; j++ )
            {
               memcpy( (void *)longVal, swapPtr, sizeof(longVal) ) ;
               longVal[0] = x4reverseLong( (void *)&longVal[0] ) ;
               longVal[1] = x4reverseLong( (void *)&longVal[1] ) ;
               memcpy( swapPtr, (void *)longVal, sizeof(longVal));
            }
         else
      #endif
      for ( j = 0 ; j <= keysmax ; j++ )
      {
         *(S4LONG *)swapPtr = x4reverseLong( (void *)swapPtr ) ;
         *(S4LONG *)(swapPtr + sizeof(S4LONG))= x4reverseLong( (void *)(swapPtr+sizeof(S4LONG)) ) ;
         swapPtr += grouplen ;
      }
   }
#endif /* S4BYTE_SWAP && S4CLIPPER */


int S4FUNCTION expr4getReturnType( EXPR4 *expr, int infoOn )
{
   return v4functions[expr->info[infoOn].functionI].returnType ;
}
