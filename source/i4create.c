/* i4create.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifdef __TURBOC__
   #pragma hdrstop
#endif

// CS 2008/02/08 support i4createWithProgress
#ifdef S4WIN32
   #if defined( TIME4STATUS ) && !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE )
           #include <process.h>

      void __cdecl i4createMonitor(void *callbackInfo)
      {
         // CS 2008/02/22 This function runs in a thread and
         // polls the CODE4 at certain intervals for index
         // create status and calls the callback function.
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

      void __cdecl i4createThread( void *info )
      {
         // This function here so that i4create can be called as a thread.

         short *reindexDone = &( ((REINDEX4CALLBACK*)info)->reindexDone ) ;
         DATA4 *data = ((REINDEX4CALLBACK*)info)->data ;
         const char *indexFileName = ((REINDEX4CALLBACK*)info)->indexFileName ;
         const TAG4INFO *tags = ((REINDEX4CALLBACK*)info)->tags ;

         #ifdef S4CLIENT
            short *callbackStarted = &( ((REINDEX4CALLBACK*)info)->callbackStarted ) ;

            while ( *callbackStarted == 0 )  // wait for callback thread to connect to the server
            {
               // AS Apr 5/07 - adjust...don't sleep unless the code4 is running as high priority
               // AS Jan 19/07 - create a u4sleep() which will delay a short period (fix problem when CodeBase run as a high-priority thread)
               u4sleep( data->codeBase ) ;
            }
         #endif
         i4create( data, indexFileName, tags ) ;
         *reindexDone = 1;
      }
   #endif

   short S4FUNCTION i4createWithProgress( DATA4 S4PTR *data, const char *fileName, const TAG4INFO *tagData, REINDEX_CALLBACK callback, long milliseconds )
   {
      #ifndef S4OFF_INDEX
         #if defined( TIME4STATUS )
            if ( callback == 0 )
            {
               i4create( data, fileName, tagData ) ;
               return error4code( data->codeBase ) ;
            }

            #ifdef E4PARM_HIGH
               if ( data == 0 )
                  return error4( 0, e4parmNull, E95301 ) ;
            #endif

            #ifdef E4VBASIC
               if ( c4parm_check( data, 2, E95301 ) )
                  return -1 ;
            #endif

            #ifdef S4OFF_WRITE
               return error4( data->codeBase, e4notWrite, E95301 ) ;
            #else
               if ( milliseconds <= 0 )
                  return error4(data->codeBase, e4parm, E95301 ) ;

               REINDEX4CALLBACK *r4info = (REINDEX4CALLBACK*)u4alloc( sizeof( REINDEX4CALLBACK ) ) ;
               if (!r4info)
                  return error4( data->codeBase, e4memory, E95301 ) ;

               data->codeBase->actionCode = ACTION4INITIALIZING;

               r4info->data = data ;
               r4info->callback = callback ;
               r4info->sleepInterval = milliseconds ;
               r4info->callbackStarted = 0 ;
               r4info->reindexDone = 0 ;
               r4info->indexFileName = fileName ;
               r4info->tags = tagData ;

               // begin thread to poll CodeBase status
               if (_beginthread(i4createMonitor, 0, (void *)r4info) == -1)
                  return error4(data->codeBase, e4result, E93004 ) ;

               // begin thread to call i4create
               if (_beginthread(i4createThread, 0, (void *)r4info) == -1)
                  return error4(data->codeBase, e4result, E93004 ) ;

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
            INDEX4 *index = i4create( data, fileName, tagData ) ;
            callback(1.0);
            if (index == 0)
            {
               return data->codeBase->errorCode;
            }
         #endif
      #endif  // !S4OFF_INDEX

      return r4success ;
   }
#endif

#if !defined( S4OFF_INDEX ) && !defined( S4OFF_WRITE )
   // !S4OFF_INDEX, !S4OFF_WRITE
   int S4FUNCTION i4addEntryToGroupFile( INDEX4 *i4, const char *tagName )
   {
      // used to add a single new tag entry to a group file...- just appends...
      #ifdef S4CLIPPER
         CODE4 *c4 = i4->codeBase ;

         char buf[LEN4PATH] ;
         // open the group file...
         // AS 06/30/99 - was not taking data file path into account on accessName which is based from there...
         char dataFilePath[LEN4PATH] ;
         assert5( i4->data->dataFile->file.name != 0 ) ;
         u4namePath( dataFilePath, sizeof( dataFilePath ), i4->data->dataFile->file.name ) ;
         u4nameCurrentExtended( buf, sizeof( buf ), i4->accessName, dataFilePath ) ;
         u4nameExt( buf, sizeof( buf ), GROUP4EXT, 1 ) ;
         // AS Apr 28/04 - use internal version
         if ( file4openInternal( &i4->file, c4, buf, 0, OPT4OTHER ) != 0 )  // can't open group file - major failure...
            return error4( c4, e4open, E95301 ) ;
         // AS 11/14/00 if the last character of the file is not '\r' or \'n', then we need
         // to insert a line terminator, else it is ok
         // LY Mar 28/05 : changed for 64-bit FILE4LONG
         // if ( file4lenLow( &i4->file ) != 0 )
         if ( !file4longEqualZero( file4lenLow( &i4->file ) ) )
         {
            // LY Mar 28/05 : changed for 64-bit FILE4LONG
            // if ( file4readLow( &i4->file, file4lenLow( &i4->file )-1, buf, 1 ) != 1 )
            FILE4LONG newPos ;
            file4longAssignLong( newPos, file4lenLow( &i4->file ) ) ;
            file4longSubtract( &newPos, 1 ) ;
            if ( file4readLow( &i4->file, newPos, buf, 1 ) != 1 )
               return error4( c4, e4write, E90603 ) ;
         }

         if ( buf[0] != '\r' && buf[0] != '\n' )
         {
            if ( file4writeInternal( &i4->file, file4lenLow( &i4->file ), "\r\n", 2 ) != 0 )
               return error4( c4, e4write, E90603 ) ;
         }

         if ( file4writeInternal( &i4->file, file4lenLow( &i4->file ), tagName, c4strlen( tagName ) ) != 0 )
            return error4( c4, e4write, E90603 ) ;
         // AS 11/14/00 - include line terminator always
         if ( file4writeInternal( &i4->file, file4lenLow( &i4->file ), "\r\n", 2 ) != 0 )
            return error4( c4, e4write, E90603 ) ;

         file4close( &i4->file ) ;
      #endif
      return 0 ;
   }



   static INDEX4 *i4createLow( DATA4 *, const char *, const TAG4INFO * ) ;



   // !S4OFF_INDEX, !S4OFF_WRITE
   INDEX4 *S4FUNCTION i4create( DATA4 *d4, const char *fileName, const TAG4INFO *tagData )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( d4, 2, E95301 ) )
            return 0 ;
      #endif

      #ifdef E4PARM_HIGH
         if ( d4 == 0 || tagData == 0 )
         {
            error4( 0, e4parm_null, E95301 ) ;
            return 0 ;
         }
      #endif

      CODE4 *c4 = d4->codeBase ;

      #ifdef E4MISC
         /* AS 04/09/99 - added check in e4misc case to verify field names are unique. */
         for ( const TAG4INFO *tagOuter = tagData ;; tagOuter++ )
         {
            if ( tagOuter->name == 0 || tagOuter->name[0] == 0 )
               break ;
            for ( const TAG4INFO *tagInner = tagOuter + 1 ;; tagInner++ )
            {
               if ( tagInner->name == 0 || tagInner->name[0] == 0 )
                  break ;
               if ( strcmp( tagOuter->name, tagInner->name ) == 0 )
               {
                  error4( c4, e4parm, E81713 ) ;
                  return 0 ;
               }
            }
         }
      #endif

      #ifndef S4CLIENT
         // AS Jun 2/03 - Added recording if data file has a tag with an expression referring to a memo field
         d4->hasMemoExpr = r4uninitializedMemoTagExpression ;  // just reset, it gets set automatically if required
      #endif

      // AS Jun 12/02 - Added support for re-use of deleted records if a tag name called 'DEL4REUSE' is created.
      // We look through the tag data here and change it if that tag name is found.
      // AS Dec 15/03 - not supported in clipper because the tag file name issue (end up with many files of same name)
      /* LY July 7/03 : added !S4LUPACH */
      #if !defined( S4CLIPPER )  // LY Sep 2/04 : && !defined( S4LUPACH )
         #if !defined( S4WIN32 ) && !defined( S4UNIX )   // LY Sep 2/04
            #ifndef S4MACINTOSH  // LY Jul 20/04
               assert5port( "Support for re-using deleted records, for porting there might be an issue of the casing of the tag name (especially for Clipper compatibility)" ) ;
            #endif
         #endif
         TAG4INFO *tagDataToUse = 0 ;
         char *reuseTagName = DEL4REUSE_TAG_NAME ;
         char *reuseExpr = "RECNO()" ;
         char *reuseFilter = "DELETED()" ;
         short reuseTag = -1 ;
      #endif

      short tagCount ;
      for( tagCount = 0 ; tagData[tagCount].name != 0; tagCount++ )
      {
         /* LY July 7/03 : added !S4LUPACH */
         #if !defined( S4CLIPPER )  // LY Sep 2/04 : && !defined( S4LUPACH )
            if ( c4stricmp( tagData[tagCount].name, DEL4REUSE_TAG_NAME ) == 0 )  /* LY 2002/10/04 : replaced stricmp() */
               reuseTag = tagCount ;
         #endif
      }

      // AS Aug 26/02 - if tagCount is 0, just return
      if ( tagCount == 0 )
         return 0 ;

      // AS Dec 15/03 - not supported in clipper because the tag file name issue (end up with many files of same name)
      /* LY July 7/03 : added !S4LUPACH */
      #if !defined( S4CLIPPER )  // LY Sep 2/04 : && !defined( S4LUPACH )
         if ( reuseTag != -1 )
         {
            // we make a copy of the tag data, replacing the info as required...
            tagDataToUse = (TAG4INFO *)u4allocFree( c4, ( (long)tagCount + 1L ) * sizeof( TAG4INFO ) ) ;
            if ( tagDataToUse == 0 )
               return 0 ;
            for( int tagLoop = 0 ; tagLoop < tagCount ; tagLoop++ )
            {
               if ( tagLoop == reuseTag )
               {
                  tagDataToUse[tagLoop].name = reuseTagName ;
                  tagDataToUse[tagLoop].expression = reuseExpr ;
                  tagDataToUse[tagLoop].filter = reuseFilter ;
                  tagDataToUse[tagLoop].unique = 0 ;
                  tagDataToUse[tagLoop].descending = 0 ;
               }
               else  // just copy the info from the existing tag
               {
                  tagDataToUse[tagLoop].name = tagData[tagLoop].name ;
                  tagDataToUse[tagLoop].expression = tagData[tagLoop].expression ;
                  tagDataToUse[tagLoop].filter = tagData[tagLoop].filter ;
                  tagDataToUse[tagLoop].unique = tagData[tagLoop].unique ;
                  tagDataToUse[tagLoop].descending = tagData[tagLoop].descending ;
               }
            }
            tagData = tagDataToUse ;
         }
      #endif

      #if !defined( S4OFF_OPTIMIZE ) && !defined( S4CLIENT )
         #ifdef S4LOW_MEMORY
            int hasOpt ;
            if ( c4->hasOpt )
            {
               hasOpt = 1 ;
               code4optSuspend( c4 ) ;
            }
            else
               hasOpt = 0 ;
         #endif
      #endif

      // LY Apr 18/05 : under .NET, passing zero to i4create() results in "0"
      const char *namePtr = 0 ;
      if ( fileName )
         if ( strlen( fileName ) )
            namePtr = fileName ;
      INDEX4 *index = i4createLow( d4, namePtr, tagData ) ;
      // AS Dec 15/03 - not supported in clipper because the tag file name issue (end up with many files of same name)
      /* LY July 7/03 : added !S4LUPACH */
      #if !defined( S4CLIPPER )  // LY Sep 2/04 : && !defined( S4LUPACH )
         if ( tagDataToUse != 0 )
         {
            u4free( tagDataToUse ) ;
            tagDataToUse = 0 ;
         }
      #endif

      // AS Dec 15/03 - not supported in clipper because the tag file name issue (end up with many files of same name)
      #if !defined( S4CLIENT ) && !defined( S4CLIPPER )
         if ( index != 0 && reuseTag != -1 )
            d4->dataFile->appendTag = dfile4tag( d4->dataFile, DEL4REUSE_TAG_NAME ) ;  // it should exist, don't error
      #endif

      #if !defined( S4OFF_OPTIMIZE ) && !defined( S4CLIENT )
         #ifndef S4CLIPPER
            if ( index != 0 )
               file4optimizeLow( &index->indexFile->file, c4->optimize, OPT4INDEX, 0, index->indexFile ) ;
         #endif
         #ifdef S4LOW_MEMORY
            if ( hasOpt )
               code4optRestart( c4 ) ;
         #endif
      #endif

      return index ;
   }



   #ifdef S4CLIENT
      // !S4OFF_INDEX, !S4OFF_WRITE, S4CLIENT
      static INDEX4 *i4createLow( DATA4 *d4, const char *fileName, const TAG4INFO *tagData )
      {
         CODE4 *c4 = d4->codeBase ;
         int tagIndex ;

         #ifdef E4PARM_HIGH
            /* AS 04/22/99 --> add check of tagData in E4PARM_HIGH case */
            for( tagIndex = 0 ; tagData[tagIndex].name != 0; tagIndex++ )
            {
               if ( tagData[tagIndex].descending )
                  if ( tagData[tagIndex].descending != r4descending )
                  {
                     error4describe( c4, e4tagInfo, E85302, tagData[tagIndex].name, 0, 0 ) ;
                     return 0 ;
                  }
            }
         #endif

         if ( fileName != 0 )
         {
            if ( strlen( fileName ) == 0 )   /* empty name disallowed */
            {
               error4describe( c4, e4name, E81717, fileName, 0, 0 ) ;
               return 0 ;
            }
            if ( dfile4index( d4->dataFile, fileName ) )
            {
               error4describe( c4, e4name, E81703, fileName, 0, 0 ) ;
               return 0 ;
            }

            char ext[4] ;
            if ( code4indexFormat( c4 ) == r4ntx )   /* disallow .ntx extensions */
            {
               u4nameRetExt( ext, 3, fileName ) ;
               if ( memcmp( ext, NTX4EXT, 3 ) == 0 )
               {
                  ext[sizeof(ext)-1] = 0 ;
                  error4describe( c4, e4name, E81720, fileName, ext, 0 ) ;
                  return 0 ;
               }
            }
         }

         if ( d4->dataFile == 0 )
            return 0 ;
         if ( error4code( c4 ) < 0 )
            return 0 ;

         error4set( c4, 0 ) ;  /* Make sure it is not 'r4unique' or 'r4noCreate'. */

         CONNECTION4 *connection = d4->dataFile->connection ;
         if ( connection == 0 )
         {
            error4( c4, e4parm, E95301 ) ;
            return 0 ;
         }
         connection4assign( connection, CON4INDEX_CREATE, data4clientId( d4 ), data4serverId( d4 ) ) ;
         CONNECTION4INDEX_CREATE_INFO_IN *dataIn ;
         connection4addData( connection, NULL, sizeof(CONNECTION4INDEX_CREATE_INFO_IN), (void **)&dataIn ) ;

         int tagCount = 0 ;
         for( tagCount = 0 ; tagData[tagCount].name != 0; tagCount++ )
         {
            ;
         }

         // AS Jan 1/03 - added non-updating temporary indexes
         dataIn->createTemp = 0 ;  // default to 0
         if ( fileName == 0 )
            dataIn->isProduction = 1 ;
         else
         {
            u4ncpy( dataIn->indexFileName, fileName, LEN4PATH ) ;
            // AS Jan 1/03 - added non-updating temporary indexes
            if ( c4getCreateTemp( c4 ) == 2 )  // if name is non-null and create temp is 2...
               dataIn->createTemp = 2 ;
         }

         dataIn->collatingSequence = htons5( c4->collatingSequence ) ;
         dataIn->collateName = htons5( (short)c4->collateName ) ;
         dataIn->collateNameUnicode = htons5( (short)c4->collateNameUnicode ) ;

         dataIn->numTags = htons5(tagCount) ;
         dataIn->safety = c4->safety ;
         /* AS 07/21/99 - added for win 95/98 to avoid endless laze writes */
         dataIn->fileFlush = c4->fileFlush ;
         // use the fact that createIndexMultiplier always 1 or 1024 to determine if fox compatible...
         // AS Jul 6/06 a value of 512 is also acceptable (large file requested, but no change to index size)
         // assert5( c4->foxCreateIndexMultiplier == 1 || c4->foxCreateIndexMultiplier == 1024 ) ;
         if ( code4indexFormat( c4 ) == r4cdx )
         {
            assert5( c4indexMultiplierGet( c4 ) == 1 || c4indexMultiplierGet( c4 ) == 1024 || c4indexMultiplierGet( c4 ) == 512 ) ;
            if ( c4indexMultiplierGet( c4 ) == 1 )  // means use FoxPro defaults...
               dataIn->foxCreateIndexBlockSize = htons5( 0 ) ;   // means to use defaults...
            else
               dataIn->foxCreateIndexBlockSize = htons5( c4->foxCreateIndexBlockSize ) ;
         }
         else
            dataIn->foxCreateIndexBlockSize = htons5( 0 ) ;   // means to use defaults...
         dataIn->readOnly = c4->readOnly ;  /* catalog purposes */
         short len = 0 ;
         short offset = sizeof( CONNECTION4INDEX_CREATE_INFO_IN ) ;

         for ( tagIndex = 0 ; tagIndex != tagCount ; tagIndex++ )
         {
            len = strlen( tagData[tagIndex].name ) + 1 ;
            offset += sizeof( CONNECTION4TAG_INFO ) ;
            CONNECTION4TAG_INFO *tinfo ;
            connection4addData( connection, NULL, sizeof(CONNECTION4TAG_INFO), (void **)&tinfo ) ;
            tinfo->name.offset = htons5(offset) ;
            unsigned int len2 = strlen( tagData[tagIndex].expression ) + 1 ;
            offset += len ;
            tinfo->expression.offset = htons5(offset) ;
            offset += len2 ;
            unsigned int len3 ;
            if ( tagData[tagIndex].filter == 0 )
            {
               len3 = 0 ;
               tinfo->filter.offset = 0 ;
            }
            else
            {
               len3 = (short)strlen( tagData[tagIndex].filter ) + 1 ;
               tinfo->filter.offset = htons5(offset) ;
            }
            offset += len3 ;
            tinfo->unique = htons5(tagData[tagIndex].unique) ;
            tinfo->descending = htons5(tagData[tagIndex].descending) ;
            connection4addData( connection, tagData[tagIndex].name, len, NULL ) ;
            connection4addData( connection, tagData[tagIndex].expression, len2, NULL ) ;
            if ( len3 != 0 )
               connection4addData( connection, tagData[tagIndex].filter, len3, NULL ) ;
         }

         connection4sendMessage( connection ) ;
         int rc = connection4receiveMessage( connection ) ;
         if ( rc < 0 )
         {
            #ifdef E4STACK
               error4stack( c4, rc, E95301 ) ;
            #endif
            return 0 ;
         }
         if ( connection4type( connection ) != CON4INDEX_CREATE )
         {
            error4( c4, e4connection, E81705 ) ;
            return 0 ;
         }

         rc = connection4status( connection ) ;
         if ( rc < 0 )
         {
            connection4errorDescribe( connection, c4, rc, E95301, fileName, 0, 0 ) ;
            return 0 ;
         }

         if ( rc != 0 )  // AS Apr 23/04 - need to set the error code in this case... (t4index2.c)
         {
            error4set( c4, rc ) ;
            if ( rc != r4uniqueContinue )
               return 0 ;
         }

         if ( connection4len( connection ) != sizeof( CONNECTION4INDEX_CREATE_INFO_OUT ) )
         {
            error4( c4, e4packetLen, E95301 ) ;
            return 0 ;
         }

         CONNECTION4INDEX_CREATE_INFO_OUT *dataOut = ( CONNECTION4INDEX_CREATE_INFO_OUT *)connection4data( connection ) ;

         if ( dataOut->lockedDatafile )
         {
            // AS May 27/03 - change for cloned locking, store the lockid/serverid, not the data4 itself
            d4->dataFile->fileLockServerId = data4serverId( d4 ) ;
            d4->dataFile->fileLockLockId = data4lockId( d4 ) ;
         }

         c4->openForCreate = 1 ;
         INDEX4 *i4 = 0 ;
         char buf[258] ;
         if ( fileName == 0 )
         {
            switch( code4indexFormat( c4 ) )
            {
               case r4ntx:
                  for( tagIndex = 0 ; tagData[tagIndex].name != 0; tagIndex++ )
                  {
                     if ( tagIndex == 0 )
                     {
                        TAG4 *tag = t4openLow( d4, 0, tagData[tagIndex].name, d4->alias ) ;
                        if ( tag == 0 )
                           break ;
                        i4 = tag->index ;
                        if ( i4 == 0 )
                           break ;
                        u4ncpy( i4->alias, d4->alias, sizeof( i4->alias ) ) ;
                        // AS 11/17/99 - the data file access name may include the extension.
                        // don't include it for the index name...
                        // u4ncpy( i4->indexFile->accessName, d4->dataFile->accessName, sizeof( i4->indexFile->accessName ) ) ;
                        // AS 01/06/00 --> turns out that server in this case uses the alias as the accessName since the data4 full path not available.
                        // therefore do same here (eg. d4create"c:\test" with tags was failing clipper)
                        // u4namePiece( i4->indexFile->accessName, sizeof( i4->indexFile->accessName ), d4->dataFile->accessName, 1, 0 ) ;
                        u4namePiece( i4->indexFile->accessName, sizeof( i4->indexFile->accessName ), d4->alias, 1, 0 ) ;
                     }
                     else
                     {
                        TAG4 *tag = t4open( d4, i4, tagData[tagIndex].name ) ;
                        if ( tag == 0 )
                           break ;
                     }
                  }
                  break ;
               default:
                  u4namePiece( buf, sizeof( buf ), dfile4name( d4->dataFile ), 1, 0 ) ;
                  u4nameExt( buf, sizeof(buf), code4indexExtension( d4->codeBase ), 1 ) ;
                  i4 = i4open( d4, buf ) ;
                  break ;
            }
         }
         else
         {
            i4 = i4open( d4, fileName ) ;
         }

         c4->openForCreate = 0 ;

         if ( i4 != 0 )
         {
            for ( tagIndex = 0 ; tagData[tagIndex].name != 0 ; tagIndex++ )
            {
               /* AS 10/28/99 - clipper version, tag info may include path info - strip this off... */
               /* AS 10/18/00 - tag names of 10 characters were failing... */
               /* char tagName[LEN4TAG_ALIAS] ; */
               char tagName[LEN4TAG_ALIAS+1] ;
               u4namePiece( tagName, sizeof( tagName ), tagData[tagIndex].name, 0, 0 ) ;
               TAG4 *tagPtr = d4tag( d4, tagName ) ;
               if ( tagPtr == 0 )
               {
                  error4describe( c4, e4name, E81406, d4alias( d4 ), tagData[tagIndex].name, 0 ) ;
                  i4closeLow( i4 ) ;
                  return 0 ;
               }
               t4uniqueSetLow( tagPtr, tagData[tagIndex].unique, 0 ) ;
            }
         }

         d4->recNum = -1 ;
         d4->recNumOld = -1 ;
         /* BCR 11/10/00 -- Fields should be set to null as in the non-client case,
         instead of just spaces */
         d4blankLow( d4, d4->record ) ;
         //memset( d4->record, ' ', dfile4recWidth( d4->dataFile ) ) ;


         // AS Feb 7/03 - Need for locking control as well as transactions
         // #ifndef S4OFF_TRAN
            if ( i4 != 0 )
               i4->isValid = 1 ;
         // #endif
         return i4 ;
      }
   #else



      #ifndef S4CLIPPER
         // !S4OFF_INDEX, !S4OFF_WRITE, !S4CLIENT, !S4CLIPPER
         static int i4canCreateIndex( DATA4 *d4, const char *fileName, const char *fileNameFormatted )
         {
            CODE4 *c4 = d4->codeBase ;

            #ifndef S4OFF_MULTI
               if ( fileName == 0 )  /* must have file open exclusively, since the lock handling changes, potentially causing multi-user failures */
               #ifdef S4SERVER
                  if ( d4->accessMode != OPEN4DENY_RW )
                  {
                     error4( c4, e4create, E81306 ) ;
                     return 0 ;
                  }
               #else /* S4SERVER else */
                  #ifdef S4FOX
                     if ( d4compatibility( d4 ) != 30 )    // new locking scheme does not conflict
                  #endif
                     if ( d4->dataFile->file.lowAccessMode != OPEN4DENY_RW )
                     {
                        error4( c4, e4exclusive, E81306 ) ;
                        return 0 ;
                     }
               #endif /* S4SERVER else */
            #endif  /* S4OFF_MULTI */

            #ifdef E4ANALYZE
               if ( code4indexExtension( d4->codeBase ) == 0 )
               {
                  error4( c4, e4struct, E91707 ) ;
                  return 0 ;
               }
            #endif

            if ( dfile4index( d4->dataFile, fileNameFormatted ) )
            {
               error4describe( c4, e4name, E81703, fileNameFormatted, 0, 0 ) ;
               return 0 ;
            }

            if ( error4code( c4 ) < 0 )
               return 0 ;

            if ( d4->dataFile == 0 )
               return 0 ;

            return 1 ;
         }



         // !S4OFF_INDEX, !S4OFF_WRITE, !S4CLIENT, !S4CLIPPER
         static void i4formatFileName( DATA4 *d4, char *nameOut, int nameOutLen, const char *nameIn )
         {
            if ( nameIn )
               u4nameCurrent( nameOut, nameOutLen, nameIn ) ;
            else
               u4nameCurrent( nameOut, nameOutLen, dfile4name( d4->dataFile ) ) ;
            u4nameExt( nameOut, nameOutLen, code4indexExtension( d4->codeBase ), ( nameIn == 0 ? 1 : 0 ) ) ;
         }



         // !S4OFF_INDEX, !S4OFF_WRITE, !S4CLIENT, !S4CLIPPER
         static int i4createIndexStructures( DATA4 *d4, INDEX4 **i4, INDEX4FILE **indexFile )
         {
            CODE4 *c4 = d4->codeBase ;

            (*i4) = (INDEX4 *)mem4createAllocZero( c4, &c4->indexMemory, c4->memStartIndex, sizeof(INDEX4), c4->memExpandIndex, 0 ) ;
            if ( (*i4) == 0 )
               return e4memory ;

            (*i4)->codeBase = c4 ;
            (*i4)->data = d4 ;

            (*indexFile) = (INDEX4FILE *)mem4createAllocZero( c4, &c4->index4fileMemory, c4->memStartIndexFile, sizeof(INDEX4FILE), c4->memExpandIndexFile, 0 ) ;
            if ( (*indexFile) == 0 )
               return e4memory ;

            #ifdef S4FOX
               (*indexFile)->blockSize = B4BLOCK_SIZE_INTERNAL ;  // default
               (*indexFile)->multiplier = 1 ;  // default
            #endif

            (*indexFile)->codeBase = c4 ;
            (*indexFile)->dataFile = d4->dataFile ;

            (*indexFile)->userCount = 1 ;
            (*i4)->indexFile = (*indexFile) ;

            return 0 ;
         }



         #ifdef S4STAND_ALONE
            // !S4OFF_INDEX, !S4OFF_WRITE, !S4CLIENT, !S4CLIPPER, S4STAND_ALONE
            // AS Nov 1/2013 - allow access of i4setFileName for access from d4data.c
            int i4setFileName( INDEX4 *i4, DATA4 *d4, const char *fileName )
            {
               if ( fileName == 0 )
                  u4namePiece( i4->accessName, sizeof( i4->accessName ), d4->alias, 0, 0 ) ;
               else
               {
                  #ifdef E4MISC
                     if ( strlen( fileName ) > sizeof( i4->accessName ) - 1 )
                     {
                        /* mark the file as temporary so it gets deleted since creation failed */
                        // AS Sep. 17/01 - changed isTemp to be done via a function for code4validate() capability
                        file4setTemporary( &i4->indexFile->file, 1, 0 ) ;
                        i4closeLow( i4 ) ;
                        return error4( d4->codeBase, e4name, E95301 ) ;
                     }
                  #endif
                  u4ncpy( i4->accessName, fileName, sizeof( i4->accessName ) - 1 ) ;
               }

               return 0 ;
            }
         #endif



         #ifdef S4FOX
            // !S4OFF_INDEX, !S4OFF_WRITE, !S4CLIENT, !S4CLIPPER, S4FOX
            static int i4setupIndexMemory( INDEX4FILE *indexFile, CODE4 *c4 )
            {
               // AS Jul 6/06 - fix for client/server
               indexFile->blockMemory = mem4create( c4, c4->memStartBlock,
                    (sizeof(B4BLOCK)) + c4indexBlockSizeGet( c4 ) - (sizeof(B4STD_HEADER)) - (sizeof(B4NODE_HEADER)), c4->memExpandBlock, 0 ) ;

               if ( indexFile->blockMemory == 0 )
                  return e4memory ;

               if ( c4->tagMemory == 0 )
               {
                  c4->tagMemory = mem4create( c4, c4->memStartTag, sizeof(TAG4), c4->memExpandTag, 0 ) ;
                  if ( c4->tagMemory == 0 )
                     return e4memory ;
               }

               if ( c4->tagFileMemory == 0 )
               {
                  c4->tagFileMemory = mem4create( c4, c4->memStartTagFile, sizeof(TAG4FILE), c4->memExpandTagFile, 0 ) ;
                  if ( c4->tagFileMemory == 0 )
                     return e4memory ;
               }

               indexFile->tagIndex = (TAG4FILE *) mem4allocZero( c4->tagFileMemory ) ;
               if ( indexFile->tagIndex == 0 )
                     return e4memory ;

               return 0 ;
            }



            // !S4OFF_INDEX, !S4OFF_WRITE, !S4CLIENT, !S4CLIPPER, S4FOX
            static int i4initTagIndex( TAG4FILE *tagIndex, DATA4 *d4, INDEX4FILE *indexFile, char *indexName )
            {
               CODE4 *c4 = d4->codeBase ;
               #ifdef S4FOX
                  #ifdef S4DATA_ALIGN
                     unsigned int size, delta ;
                  #endif
               #endif

               tagIndex->codeBase = c4 ;
               tagIndex->indexFile = indexFile ;
               tagIndex->header.typeCode = 0xE0 ;  /* compound, compact */
               tagIndex->header.filterLen = 1 ;
               tagIndex->header.filterPos = 1 ;
               tagIndex->header.exprLen = 1 ;
               tagIndex->header.exprPos = 0 ;
               tagIndex->header.keyLen = 10 ;
               tagIndex->header.signature = 0x01 ;

               // AS 06/30/99 - new multiplier and blocksize members...
               if ( c4indexBlockSizeGet( c4 ) != B4BLOCK_SIZE_INTERNAL || c4indexMultiplierGet( c4 ) != 1 )
               {
                  // requesting CodeBase specific create with modified block size and multiplier...
                  if ( ( c4indexBlockSizeGet( c4 ) % c4indexMultiplierGet( c4 ) ) != 0 || c4indexMultiplierGet( c4 ) > 1024 )  // there is a remainder - this is an error...- current max of multiplier is 1024
                     return error4( c4, e4index, 85305 ) ;
                  if ( ( c4indexBlockSizeGet( c4 ) % B4BLOCK_SIZE_INTERNAL) != 0 || c4indexBlockSizeGet( c4 ) < B4BLOCK_SIZE_INTERNAL )  // must be multiple of 512
                     return error4( c4, e4index, 85305 ) ;
                  tagIndex->header.codeBaseNote = 0xABCD ;  // indicates CodeBase specific and to use values...
                  tagIndex->header.blockSize = c4indexBlockSizeGet( c4 ) ;
                  tagIndex->header.multiplier = c4indexMultiplierGet( c4 ) ;
                  indexFile->blockSize = c4indexBlockSizeGet( c4 ) ;
                  indexFile->multiplier = c4indexMultiplierGet( c4 ) ;
               }

               #ifdef S4DATA_ALIGN
                  size = (unsigned int)sizeof(S4LONG) + tagIndex->header.keyLen ;
                  delta = sizeof(void *) - size % sizeof(void *) ;
                  tagIndex->builtKeyMemory = mem4create( c4, 3, size + delta, 2, 0 ) ;
               #else
                  tagIndex->builtKeyMemory = mem4create( c4, 3, (unsigned int)sizeof(S4LONG) + tagIndex->header.keyLen + 1, 2, 0 ) ;
               #endif

               if ( tfile4setCollatingSeq( tagIndex, collate4machine, 1 ) < 0 )     /* tag of tags is always machine */
                  return -1 ;

               // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
               // if ( tfile4setCodePage( tagIndex, d4->codePage ) < 0 )
               //    return -1 ;

               u4namePiece( tagIndex->alias, sizeof(indexFile->tagIndex->alias), indexName, 0, 0 ) ;

               return 0 ;
            }



            // !S4OFF_INDEX, !S4OFF_WRITE, !S4CLIENT, !S4CLIPPER, S4FOX
            static int i4initTagRegular( const TAG4INFO *tagInfo, TAG4 **tagPtr, DATA4 *d4, INDEX4 *i4 )
            {
               /*

                  ERRORS

                  returns < 0.  Also, tagPtr will be set to the erroneous tag, otherwise tagPtr is set to
                  0 on exit.
               */
               CODE4 *c4 = d4->codeBase ;

               (*tagPtr) = (TAG4 *)mem4allocZero( c4->tagMemory ) ;
               if ( (*tagPtr) == 0 )
                  return -1 ;

               (*tagPtr)->tagFile = (TAG4FILE *)mem4allocZero( c4->tagFileMemory ) ;
               if ( (*tagPtr)->tagFile == 0 )
                  return -1 ;

               TAG4FILE *tagFile = (*tagPtr)->tagFile ;

               (*tagPtr)->index = i4 ;
               tagFile->codeBase = c4 ;
               tagFile->indexFile = i4->indexFile ;

               u4ncpy( tagFile->alias, tagInfo->name, sizeof( tagFile->alias ) ) ;
               c4upper( tagFile->alias ) ;
               u4trim( tagFile->alias ) ;

               tagFile->header.signature = 0x01 ;
               tagFile->header.typeCode = 0x60 ;  /* compact */
               if ( tagInfo->unique )
               {
                  #ifdef S4FOX
                     if ( tagInfo->unique == r4candidate || tagInfo->unique == e4candidate )
                        tagFile->header.typeCode += 0x04 ;
                     else
                  #endif
                     tagFile->header.typeCode += 0x01 ;
                  (*tagPtr)->errUnique = tagInfo->unique ;

                  #ifdef S4FOX
                     if ( tagInfo->unique != e4unique && tagInfo->unique != r4unique &&
                          tagInfo->unique != r4uniqueContinue && tagInfo->unique != r4candidate &&
                          tagInfo->unique != e4candidate )
                  #else
                     if ( tagInfo->unique != e4unique && tagInfo->unique != r4unique && tagInfo->unique != r4uniqueContinue )
                  #endif
                  {
                     return error4describe( c4, e4tagInfo, E85301, tagInfo->name, 0, 0 ) ;
                  }
               }
               if ( tagInfo->descending)
               {
                  tagFile->header.descending = 1 ;
                  #ifdef E4PARM_HIGH
                     if ( tagInfo->descending != r4descending )
                        return error4describe( c4, e4tagInfo, E85302, tagInfo->name, 0, 0 ) ;
                  #endif
               }

               if ( tagInfo->expression == 0 )
                  return error4describe( c4, e4tagInfo, E85303, tagInfo->name, tagInfo->expression, 0 ) ;

               tagFile->expr = expr4parseLow( d4, tagInfo->expression, tagFile ) ;
               if ( tagFile->expr == 0 )
               {
                  if ( error4code( c4 ) >= 0 )
                     return error4( c4, e4memory, E95301 ) ;
                  return -1 ;
               }

               #ifdef S4FOX
                  // AS 10/01/99 --> typeCode of 0x02 for nullable...
                  if ( expr4nullLow( tagFile->expr, 0 ) )  // means nullable expression...
                     tagFile->header.typeCode += 0x02 ;
               #endif

               tagFile->header.exprLen = (short) (c4strlen( tagFile->expr->source ) + 1) ;
               if ( tagFile->header.exprLen > I4MAX_EXPR_SIZE )
                  return error4describe( c4, e4tagInfo, E85304, tagInfo->name, tagInfo->expression, 0 ) ;

               #ifdef S4FOX
                  /* if tfile4set functions fail, an error4code() is generated by them */
                  assert5( tagFile->expr != 0 ) ;  // required for collation determination

                  Collate4name collateNameForTag ;

                  // AS 08/07/99 --> isUnicode needs to be set here as well...
                  if ( tagFile->expr->type == r5wstr || tagFile->expr->type == r5wstrLen )
                     tagFile->isUnicode = 1 ;

                  if ( tagFile->isUnicode )
                     collateNameForTag = c4->collateNameUnicode ;
                  else
                     collateNameForTag = c4->collateName ;

                  // AS 02/07/00 it is invalid to have 'no' collation, since collation4get() will access random
                  // memory in that case because collateion4arrayIndex uses collateNameForTag - 1 (or 0-1 = -1)
                  if( collateNameForTag != collate4none )
                  {
                     COLLATE4 *collate = collation4get( collateNameForTag ) ;

                     if ( collate->collateType == collate4unknown )
                     {
                        // means need to load from disk...
                        if ( collate4setupReadFromDisk( c4, collateNameForTag ) != 0 )
                           return error4( tagFile->codeBase, e4notSupported, E84907 ) ;
                        if ( tagFile->isUnicode )  // if character and not loaded, load now...
                           if ( collate->unicodeToKeyTranslationArray == 0 )
                              collate4setupUnicodeFromChar( collate ) ;
                     }
                  }

                  if ( tfile4setCollatingSeq( tagFile, collateNameForTag, 1 ) < 0 )
                     return -1 ;

                  // AS May 12/06 - I need to set the collation before the expression parsing because due to a change, if the
                  // expression is on a tag and the ASCEND or DESCEND opererators are used we collate based on the input collation
                  // since we can't actually do this, here we re-evaluate the expression now that the collation has been set
                  // (only if there is an ascend operator and we are not collate4machine)
                  if ( tagFile->hasAscendOrDescend == 1 && tagFile->collateName != collate4machine && tagFile->collateName != collate4none )
                  {
                     expr4free( tagFile->expr ) ;
                     tagFile->expr = expr4parseLow( d4, tagInfo->expression, tagFile ) ;
                     if ( tagFile->expr == 0 )
                     {
                        if ( error4code( c4 ) >= 0 )
                           return error4( c4, e4memory, E95301 ) ;
                        return -1 ;
                     }
                  }

                  // AS Dec 5/05 - fix for OFF_MEMO
                  #ifndef S4OFF_MEMO
                     // AS Feb 11/04 - need to know if an expression includes a memo field
                     if ( tagFile->expr->hasMemoField == 1 && tagFile->collateName != collate4none )
                     {
                        // assume/verify it is only a simple field
                        // LY Jan 7/05 : replaced direct check of COLLATE4.keySizeCharPerAdd with collate4simpleMapping()
                        // short numExtraCharsPerChar = collation4get( tagFile->collateName )->keySizeCharPerCharAdd ;
                        // if ( numExtraCharsPerChar > 0 )  // need to reduce the memo key size down for collation support
                        COLLATE4 *currCollate = collation4get( tagFile->collateName ) ;
                        if ( !collate4simpleMapping( currCollate ) )
                        {
                           if ( tagFile->expr->info[0].len >= I4MAX_KEY_SIZE_COMPATIBLE - 1 )
                           {
                              short numExtraCharsPerChar = currCollate->keySizeCharPerCharAdd ;
                              tagFile->expr->info[0].len /= (1 + numExtraCharsPerChar) ;
                              tagFile->expr->len /= (1 + numExtraCharsPerChar) ;
                           }
                        }
                     }
                  #endif
                  // AS 07/27/99 -- this has been superseded by more simple collations for foxPro
                  // if ( tfile4setCodePage( tagFile, d4->codePage ) < 0 )
                  //    return -1 ;
               #endif

               if ( tagInfo->filter != 0 )
               {
                  if ( *( tagInfo->filter ) != '\0' )
                  {
                     tagFile->header.typeCode += 0x08 ;
                     tagFile->filter = expr4parseLow( d4, tagInfo->filter, tagFile ) ;
                     if ( tagFile->filter )
                     {
                        if (expr4type( tagFile->filter ) != r4log )
                        {
                           expr4free( tagFile->filter ) ;
                           tagFile->filter = 0;
                           return error4describe( c4, e4create, E80905, expr4source( tagFile->filter ), 0, 0 ) ;
                        }
                        tagFile->header.filterLen = (short)c4strlen( tagFile->filter->source ) ;
                     }
                  }
               }

               tagFile->header.filterLen++ ;  /* minimum of 1, for the '\0' */

               if ( tagFile->header.filterLen > I4MAX_EXPR_SIZE )
                  return error4describe( c4, e4tagInfo, E85304, tagInfo->name, tagInfo->filter, 0 ) ;

               tagFile->header.filterPos = tagFile->header.exprLen ;

               if ( error4code( c4 ) < 0 )
                  return -1 ;
               l4add( &i4->indexFile->tags, tagFile ) ;
               l4add( &i4->tags, (*tagPtr) ) ;
               (*tagPtr) = 0 ;

               return 0 ;
            }
         #endif /* S4FOX */



         // !S4OFF_INDEX, !S4OFF_WRITE, !S4CLIENT, !S4CLIPPER
         static int i4createReindex( INDEX4 *i4, CODE4 *c4 )
         {
            int rc = i4reindex( i4 ) ;

            if ( rc == r4unique || rc == r4uniqueContinue || rc < 0 )
            {
               // AS Mar 26/04 - this is not necessarily a unique error...
               switch( rc )
               {
                  case r4unique:  // need to return -1 for error purposes
                     error4set( c4, r4unique ) ;
                     return -1 ;
                  case r4uniqueContinue:  // need to return -1 for error purposes
                     error4set( c4, r4uniqueContinue ) ;
                     return 0 ;
                  default:
                     return rc ;

               }
            }

            if ( rc == r4locked )   /* means data file was not opened exclusive and other users using - cannot succeed on create */
            {
               error4set( c4, e4lock ) ;
               return -1 ;
            }

            return 0 ;
         }



         // !S4OFF_INDEX, !S4OFF_WRITE, !S4CLIENT, !S4CLIPPER
         static INDEX4 *i4createLow( DATA4 *d4, const char *fileName, const TAG4INFO *tagData )
         {
            char buf[258] ;
            i4formatFileName( d4, buf, sizeof( buf ), fileName ) ;

            if ( i4canCreateIndex( d4, fileName, buf ) == 0 )
               return 0 ;

            CODE4 *c4 = d4->codeBase ;
            int createTemp = c4getCreateTemp( c4 ) ;
            error4set( c4, 0 ) ;  /* Make sure it is not 'r4unique' or 'r4noCreate'. */

            #ifndef S4SINGLE
               // AS Jan 10/03 - allow creating indexes without locking
               if ( fileName != 0 )  // if fileName is 0, we ignore the setting and create production indexes
               {
                  if ( createTemp != 2 )  // means create without locking
                  {
                     if ( d4lockFileInternal( d4, 1 ) )
                     {
                        error4( c4, e4lock, E85309 ) ;
                        return 0 ;
                     }
                  }
               }
            #endif

            INDEX4FILE *indexFile ;
            INDEX4 *i4 ;
            if ( i4createIndexStructures( d4, &i4, &indexFile ) < 0 )
               return 0 ;

            #ifdef S4PREPROCESS_FILE
               short oldPreprocess = code4getPreprocessFile( c4 ) ;
               // AS Apr 24/03 - Only attempt to set if it mismatches - avoid loading when not required
               if ( d4->dataFile->file.preprocessed != oldPreprocess )
                  code4setPreprocessFile( c4, d4->dataFile->file.preprocessed ) ; // set to same setting as for data file
            #endif
            // AS May 24/02 - created file4createInternal for internal use to indicate file types
            int rc = file4createInternal( &indexFile->file, c4, buf, 1, OPT4INDEX ) ;
            #ifdef S4PREPROCESS_FILE
               if ( d4->dataFile->file.preprocessed != oldPreprocess )
                  code4setPreprocessFile( c4, oldPreprocess ) ;  // just put to the opposite number
            #endif
            if ( rc )
            {
               if ( rc > 0 )
                  error4set( c4, rc ) ;
                /* don't mark file as temporary here because it could have failed for safety reasons - in any case, create failed, so don't bother */
               i4closeLow( i4 ) ;
               return 0 ;
            }

            // AS 03/08/01 - optimize the file on creation so that it will create faster...  esp. since it
            // may require a reindex of a large portion of records.
            file4optimizeLow( &indexFile->file, c4->optimize, OPT4INDEX, 0, indexFile ) ;

            DATA4FILE *data = d4->dataFile ;
            l4add( &data->indexes, indexFile ) ;
            l4add( &d4->indexes, i4 ) ;

            #ifdef S4STAND_ALONE
               if ( i4setFileName( i4, d4, fileName ) < 0 )
                  return 0 ;
            #endif

            #ifdef S4FOX
               if ( i4setupIndexMemory( indexFile, c4 ) < 0 )
               {
                  /* mark the file as temporary so it gets deleted since creation failed */
                  // AS Sep. 17/01 - changed isTemp to be done via a function for code4validate() capability
                  file4setTemporary( &indexFile->file, 1, 0 ) ;
                  return 0 ;
               }

               if ( i4initTagIndex( indexFile->tagIndex, d4, indexFile, buf ) < 0 )
               {
                  /* mark the file as temporary so it gets deleted since creation failed */
                  // AS Sep. 17/01 - changed isTemp to be done via a function for code4validate() capability
                  file4setTemporary( &indexFile->file, 1, 0 ) ;
                  i4closeLow( i4 ) ;
                  return 0 ;
               }

               TAG4 *tagPtr = 0 ;
               for ( int i = 0; tagData[i].name; i++ )
               {
                  if ( i4initTagRegular( &(tagData[i]), &tagPtr, d4, i4 ) < 0 )
                  {
                     if ( tagPtr != 0 )
                     {
                        if ( tagPtr->tagFile != 0 )
                           mem4free( c4->tagFileMemory, tagPtr->tagFile ) ;
                        mem4free( c4->tagMemory, tagPtr ) ;
                     }
                     /* mark the file as temporary so it gets deleted since creation failed */
                     // AS Sep. 17/01 - changed isTemp to be done via a function for code4validate() capability
                     file4setTemporary( &indexFile->file, 1, 0 ) ;
                     i4closeLow( i4 );
                     return 0 ;
                  }
               }
            #else /* S4FOX else */
               indexFile->header.two = 2 ;
               u4yymmdd( indexFile->header.createDate ) ;

               if ( fileName == 0 )
                  indexFile->header.isProduction = 1 ;

               indexFile->header.numSlots = 0x30 ;
               indexFile->header.slotSize = 0x20 ;

               u4namePiece( indexFile->header.dataName, sizeof( indexFile->header.dataName ), data->file.name, 0, 0 ) ;
               indexFile->header.blockChunks = (short)(c4->memSizeBlock/512) ;

               #ifdef E4MISC
                  if ( indexFile->header.blockChunks < 2 || indexFile->header.blockChunks > 63 )   /* disallowed for compatibility reasons */
                  {
                     error4describe( c4, e4info, E85305, fileName, 0, 0 ) ;
                     /* mark the file as temporary so it gets deleted since creation failed */
                     // AS Sep. 17/01 - changed isTemp to be done via a function for code4validate() capability
                     file4setTemporary( &indexFile->file, 1, 0 ) ;
                     i4closeLow( i4 );
                     return 0 ;
                  }
               #endif

               indexFile->header.blockRw = (short)(indexFile->header.blockChunks * I4MULTIPLY) ;
               indexFile->blockMemory = mem4create( c4, c4->memStartBlock, (sizeof(B4BLOCK)) + indexFile->header.blockRw -
                  (sizeof(B4KEY_DATA)) - (sizeof(short)) - (sizeof(char[6])), c4->memExpandBlock, 0 ) ;

               if ( indexFile->blockMemory == 0 )
               {
                  /* mark the file as temporary so it gets deleted since creation failed */
                  // AS Sep. 17/01 - changed isTemp to be done via a function for code4validate() capability
                  file4setTemporary( &indexFile->file, 1, 0 ) ;
                  i4closeLow( i4 ) ;
                  return 0 ;
               }

               TAG4 *tagPtr = 0 ;
               TAG4FILE *tFile = 0 ;
               for ( int i = 0 ; tagData[i].name ; i++ )
               {
                  indexFile->header.numTags++ ;

                  if ( c4->tagMemory == 0 )
                  {
                     c4->tagMemory = mem4create( c4, c4->memStartTag, sizeof(TAG4), c4->memExpandTag, 0 ) ;
                     if ( c4->tagMemory == 0 )
                        break ;
                  }

                  if ( c4->tagFileMemory == 0 )
                  {
                     c4->tagFileMemory = mem4create( c4, c4->memStartTagFile, sizeof(TAG4FILE), c4->memExpandTagFile, 0 ) ;
                     if ( c4->tagFileMemory == 0 )
                        break ;
                  }

                  tagPtr = (TAG4 *)mem4allocZero( c4->tagMemory ) ;
                  if ( tagPtr == 0 )
                     break ;
                  c4memset( (void *)tagPtr,0, sizeof(TAG4) ) ;
                  tagPtr->index = i4 ;

                  tagPtr->tagFile = (TAG4FILE *)mem4allocZero( c4->tagFileMemory ) ;
                  if ( tagPtr->tagFile == 0 )
                     break ;
                  tFile = tagPtr->tagFile ;
                  c4memset( (void *)tFile,0, sizeof(TAG4FILE) ) ;
                  tFile->codeBase = c4 ;
                  tFile->indexFile = indexFile ;

                  u4ncpy( tFile->alias, tagData[i].name, sizeof(tFile->alias) ) ;
                  c4upper( tFile->alias ) ;
                  u4trim( tFile->alias ) ;

                  tFile->header.typeCode  = 0x10 ;
                  if ( tagData[i].unique )
                  {
                     tFile->header.typeCode += 0x40 ;
                     tFile->header.unique = 0x4000 ;
                     tagPtr->errUnique = tagData[i].unique ;

                     #ifdef S4FOX
                        if ( tagData[i].unique != e4unique && tagData[i].unique != r4unique &&
                             tagData[i].unique != r4uniqueContinue && tagData[i].unique != r4candidate &&
                             tagData[i].unique != e4candidate )
                     #else
                        if ( tagData[i].unique != e4unique && tagData[i].unique != r4unique &&tagData[i].unique != r4uniqueContinue )
                     #endif
                     {
                        error4describe( c4, e4tagInfo, E85301, tagData[i].name, 0, 0 ) ;
                        break ;
                     }
                  }
                  if ( tagData[i].descending)
                  {
                     tFile->header.typeCode += 0x08 ;
                     #ifdef E4PARM_HIGH
                        if ( tagData[i].descending != r4descending )
                        {
                           error4describe( c4, e4tagInfo, E85302, tagData[i].name, 0, 0 ) ;
                           break ;
                        }
                     #endif
                  }

                  if ( tagData[i].expression == 0 )
                  {
                     error4describe( c4, e4tagInfo, E85303, tagData[i].name, tagData[i].expression, 0 ) ;
                     break ;
                  }

                  tFile->expr = expr4parseLow( d4, tagData[i].expression, tFile ) ;
                  if ( tFile->expr == 0 )
                  {
                     if ( error4code( c4 ) >= 0 )
                        error4( c4, e4memory, E95301 ) ;
                     break ;
                  }
                  if ( expr4type( tFile->expr ) == r4log )  /* disallowed in MDX */
                  {
                     error4( c4, e4tagInfo, E82901 ) ;
                     break ;
                  }

                  if ( tagData[i].filter != 0 )
                     if ( *(tagData[i].filter) != '\0' )
                        tFile->filter = expr4parseLow( d4, tagData[i].filter, tFile ) ;

                  if ( error4code( c4 ) < 0 )
                     break ;
                  l4add( &indexFile->tags, tFile ) ;
                  l4add( &i4->tags, tagPtr ) ;
               }

               if ( error4code( c4 ) < 0 )
               {
                  if ( tagPtr != 0 )
                  {
                     if ( tFile != 0 )
                     {
                        if ( tFile->expr != 0 )
                           expr4free( tFile->expr ) ;
                        if ( tFile->filter != 0 )
                           expr4free( tFile->filter ) ;
                        mem4free( c4->tagFileMemory, tFile ) ;
                     }
                     mem4free( c4->tagMemory, tagPtr ) ;
                  }
                  /* mark the file as temporary so it gets deleted since creation failed */
                  // AS Sep. 17/01 - changed isTemp to be done via a function for code4validate() capability
                  file4setTemporary( &indexFile->file, 1, 0 ) ;
                  i4closeLow( i4 );
                  return 0 ;
               }

               if ( indexFile->header.numTags > 47 )
               {
                  /* mark the file as temporary so it gets deleted since creation failed */
                  // AS Sep. 17/01 - changed isTemp to be done via a function for code4validate() capability
                  file4setTemporary( &indexFile->file, 1, 0 ) ;
                  i4closeLow( i4 );
                  error4describe( c4, e4tagInfo, E85306, fileName, 0, 0 ) ;
                  return 0 ;
               }
            #endif /* S4FOX else */

            i4->indexFile = indexFile ;
            if ( i4createReindex( i4, c4 ) < 0 )
            {
               /* mark the file as temporary so it gets deleted since creation failed */
               // AS Sep. 17/01 - changed isTemp to be done via a function for code4validate() capability
               file4setTemporary( &indexFile->file, 1, 0 ) ;
               i4closeLow( i4 ) ;
               return 0 ;
            }

            if ( fileName == 0 )
            {
               if ( error4code( c4 ) >= 0 && error4code( c4 ) != r4unique )
               {
                  data->hasMdxMemo |= 1 ;  /* or for fox 3.0 which uses this setting with 0x02 bit for memo */
                  data->openMdx = 1 ;

                  FILE4LONG pos ;
                  /* LY July 7/03 : changed from 0 to 0L for Linux compiler */
                  file4longAssign( pos, ( 4 + sizeof( S4LONG ) + 2 * sizeof( short ) + sizeof( char[16] ) ), 0L ) ;
                  #ifdef S4FOX
                     file4writeInternal( &data->file, pos, &(data->hasMdxMemo), sizeof( char ) ) ;
                  #else
                     file4writeInternal( &data->file, pos, &(data->hasMdxMemo), sizeof( data->hasMdxMemo ) ) ;
                  #endif
               }
            }

            if ( error4code( c4 ) < 0 || error4code( c4 ) == r4unique )
            {
               /* mark the file as temporary so it gets deleted since creation failed */
               // AS Sep. 17/01 - changed isTemp to be done via a function for code4validate() capability
               file4setTemporary( &indexFile->file, 1, 0 ) ;
               i4closeLow( i4 ) ;
               return 0 ;
            }

            // AS Dec 31/02 - added non-updating temporary indexes
            // AS Jan 13/03 - in server we need to close and re-open...
            #ifdef S4STAND_ALONE
               if ( fileName != 0 )  // if fileName is 0, we ignore the setting and create production indexes
                  if ( createTemp == 2 )
                  {
                     // AS Ensure that the file gets updated to disk in case there are outstanding writes
                     dfile4flushIndex( d4->dataFile ) ;
                     indexFile->nonUpdateable = 1 ;
                     indexFile->file.isTemporary = 1 ;
                     indexFile->file.isReadOnly = 1 ;
                  }
            #endif

            // AS Feb 7/03 - Need for locking control as well as transactions
            /* LY 2003/06/24 : restored !S4OFF_TRAN, added !S4OFF_MULTI */
            // AS Jul 22/03 - need defined if either off_tran is false, or off_multi is false, not and
            #if !defined( S4OFF_TRAN ) || !defined( S4OFF_MULTI )
               i4->isValid = 1 ;
            #endif
            indexFile->isValid = 1 ;
            return i4 ;
         }



         // !S4OFF_INDEX, !S4OFF_WRITE, !S4CLIENT, !S4CLIPPER
         TAG4 *S4FUNCTION t4create( DATA4 *d4, const TAG4INFO *tagData, INDEX4 *i4ndx, int useTempTagFileNames )
         {
            error4( 0, e4notSupported, E95302 ) ;
            return 0 ;
         }
      #else


         // !S4OFF_INDEX, !S4OFF_WRITE, !S4CLIENT, S4CLIPPER
         static INDEX4 *i4createLow( DATA4 *d4, const char *fileName, const TAG4INFO *tagData )
         {
            #ifdef E4PARM_HIGH
               if ( fileName )
               {
                  if ( d4index( d4, fileName ) )
                  {
                     error4describe( d4->codeBase, e4name, E81703, fileName, 0, 0 ) ;
                     return 0 ;
                  }
                  // u4namePiece( buf, sizeof( buf ), fileName, 0, 1 ) ;
               }
            #endif

            CODE4 *c4 = d4->codeBase ;
            if ( error4code( c4 ) < 0 )
               return 0 ;
            error4set( c4, 0 ) ;  /* Make sure it is not 'r4unique' or 'r4noCreate'. */

            if ( fileName != 0 )
            {
               if ( code4indexFormat( c4 ) == r4ntx )   /* disallow .ntx extensions */
               {
                  char ext[4] ;
                  u4nameRetExt( ext, 3, fileName ) ;
                  // AS Jan 24/02 - Was not null-terminating file extension, so was not always executing correctly
                  ext[3] = 0 ;
                  // AS 06/14/00 - due to CS changes, case sensitivity problem now... change to case insensitive comparison
                  // if ( memcmp( ext, TAG4EXT, 3 ) == 0 )
                  /* BCR 10/10/00 -- no stricmp on linux, use c4stricmp */
                  if ( c4stricmp( ext, TAG4EXT ) == 0 )
                  {
                     ext[sizeof(ext)-1] = 0 ;
                     error4describe( c4, e4name, E81720, fileName, ext, 0 ) ;
                     return 0 ;
                  }
               }
            }

            #ifndef S4SINGLE
               if ( d4lockFileInternal( d4, 1 ) != 0 )
                  return 0 ;
            #endif

            INDEX4 *i4 = (INDEX4 *)mem4createAllocZero( c4, &c4->indexMemory, c4->memStartIndex, sizeof(INDEX4), c4->memExpandIndex, 0 ) ;

            if ( i4 == 0 )
            {
               #ifdef E4STACK
                  error4stack( c4, e4memory, E95301 ) ;
               #endif
               return 0 ;
            }

            i4->codeBase = c4 ;
            i4->data = d4 ;

            int rc ;
            if ( fileName )  /* create a group file */
            {
               char buf[258] ;
               c4memset( buf, 0, sizeof( buf ) ) ;

               u4ncpy( buf, fileName, sizeof( buf ) ) ;
               u4ncpy( i4->accessName, fileName, sizeof( i4->accessName ) ) ;

               u4nameExt( buf, sizeof( buf ), GROUP4EXT, 0 ) ;

               // AS May 24/02 - created file4createInternal for internal use to indicate file types
               rc = file4createInternal( &i4->file, c4, buf, 1, OPT4INDEX ) ;
               if ( rc )
               {
                  if ( rc > 0 )
                     error4set( c4, rc ) ;

                  file4close( &i4->file ) ;
                  return 0 ;
               }

               FILE4LONG pos ;
               file4longAssign( pos, 0, 0 ) ;
               FILE4SEQ_WRITE seqwrite ;
               char buffer[1024] ;
               file4seqWriteInitLow( &seqwrite, &i4->file, pos, buffer, sizeof( buffer ) ) ;

               /* create the group file */
               for ( int i = 0; tagData[i].name; i++ )
               {
                  // AS 05/27/99 -- this code looked buggy at best, modified to make simpler
                  // char name[10] ;
                  // u4namePiece( name, 10, tagData[i].name, 0, 0 ) ;
                  // c4upper( name ) ;
                  // int flen = strlen( name ) ;
                  // file4seqWrite( &seqwrite, tagData[i].name, strlen( tagData[i].name ) - flen ) ;
                  // file4seqWrite( &seqwrite, name, flen ) ;
                  file4seqWrite( &seqwrite, tagData[i].name, c4strlen( tagData[i].name ) ) ;
                  file4seqWrite( &seqwrite, "\r\n", 2 ) ;
               }

               file4seqWriteFlush( &seqwrite ) ;

               file4close ( &i4->file ) ;

               #ifndef S4SINGLE
                  if ( rc )
                  {
                     if ( rc > 0 )
                        error4set( c4, rc ) ;
                     return 0 ;
                  }
               #endif
               // AS May 26/06 - was not properly using this path value...it needs to be a stored string
               // i4->path = buf ;
               u4nameCurrent( i4->nameWithPath, sizeof( i4->nameWithPath ), buf ) ;
            }
            else
            {
               u4ncpy( i4->accessName, d4->alias, sizeof( i4->accessName ) ) ;
            }

            l4add( &d4->indexes, i4 ) ;

            /* now create the actual tag files */
            for ( int i = 0 ; tagData[i].name ; i++ )
            {
               if ( t4create( d4, &tagData[i], i4, (c4->createTemp == 1 && fileName == 0 ) ? 1 : 0 ) == 0 )
               {
                  i4closeLow( i4 ) ;
                  return 0 ;
               }
            }

            if ( error4code( c4 ) < 0 || error4code( c4 ) == r4unique )
            {
               i4closeLow( i4 ) ;
               return 0 ;
            }

            rc = i4reindex( i4 ) ;
            // AS Mar 29/04 - improved returns and error handling...
            if ( rc == r4unique || rc == r4uniqueContinue || rc < 0 )
            {
               switch( rc )
               {
                  case r4unique:  // need to return -1 for error purposes
                     i4closeLow( i4 ) ;
                     error4set( c4, r4unique ) ;
                     return 0 ;
                  case r4uniqueContinue:  // need to return -1 for error purposes
                     error4set( c4, r4uniqueContinue ) ;
                     break ;
                  default:
                     i4closeLow( i4 ) ;
                     return 0 ;
               }
            }

            // AS Feb 7/03 - Need for locking control as well as transactions
            // #ifndef S4OFF_TRAN
               i4->isValid = 1 ;
            // #endif
            return i4 ;
         }



         // !S4OFF_INDEX, !S4OFF_WRITE, !S4CLIENT, S4CLIPPER
         TAG4 *S4FUNCTION t4create( DATA4 *d4, const TAG4INFO *tagData, INDEX4 *i4ndx, int useTempTagFileNames )
         {
            /* this function does not reindex if an 'i4ndx' is passed as a parameter */
            /* this allows several creations before an actual reindex must occur */
            /* if useTempTagFileNames is true then the tag files created use temporary names */

            #ifdef E4VBASIC
               if ( c4parm_check( d4, 2, E95302 ) )
                  return 0 ;
            #endif

            #ifdef E4PARM_HIGH
               if ( d4 == 0 || tagData == 0 )
               {
                  error4( 0, e4parm, E95302 ) ;
                  return 0 ;
               }
            #endif

            char buf[258] ;
            u4namePiece( buf, sizeof( buf ), tagData->name, 0, 0 ) ;
            u4trim( buf ) ;

            CODE4 *c4 = d4->codeBase ;
            if ( error4code( c4 ) < 0 )
               return 0 ;

            int oldTagErr = c4->errTagName ;
            c4->errTagName = 0 ;
            if ( d4tag( d4, buf ) )
            {
               error4describe( c4, e4name, E85308, buf, 0, 0 ) ;
               c4->errTagName = oldTagErr ;
               return 0 ;
            }
            c4->errTagName = oldTagErr ;

            error4set( c4, 0 ) ;  /* Make sure it is not 'r4unique' or 'r4noCreate'. */

            #ifndef S4OFF_OPTIMIZE
               #ifdef S4LOW_MEMORY
                  int hasOpt ;
                  if ( c4->hasOpt )
                  {
                     hasOpt = 1 ;
                     code4optSuspend( c4 ) ;
                  }
                  else
                     hasOpt = 0 ;
               #endif
            #endif

            INDEX4 *i4 ;

            if ( i4ndx == 0 )   /* must create an index for the tag */
            {
               if ( c4->indexMemory == 0 )
                  c4->indexMemory = mem4create( c4, c4->memStartIndex, sizeof(INDEX4),
                                                 c4->memExpandIndex, 0 ) ;
               if ( c4->indexMemory == 0 )
                  return 0 ;
               i4 = (INDEX4 *) mem4allocZero( c4->indexMemory ) ;
               if ( i4 == 0 )
               {
                  #ifdef E4STACK
                     error4stack( c4, e4memory, E95302 ) ;
                  #endif
                  return 0 ;
               }
               i4->data = d4 ;
               i4->codeBase = c4 ;
               #ifdef E4PARM_HIGH
                  u4namePiece( buf, sizeof( buf ), i4->accessName, 0, 0 ) ;
                  if ( d4index( d4, buf ) )
                  {
                     error4( d4->codeBase, e4parm, E81704 ) ;
                     return 0 ;
                  }
               #endif
               u4namePiece( i4->accessName, sizeof( i4->accessName ), tagData->name, 0, 0 ) ;
               u4trim( i4->accessName ) ;
            }
            else
               i4 = i4ndx ;

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

            TAG4 *t4 = (TAG4 *)mem4allocZero( c4->tagMemory ) ;
            if ( t4 == 0 )
            {
               #ifdef E4STACK
                  error4stack( c4, e4memory, E95302 ) ;
               #endif
               return 0 ;
            }

            t4->index = i4 ;

            t4->tagFile = (TAG4FILE *) mem4allocZero( c4->tagFileMemory ) ;
            if ( t4->tagFile == 0 )
            {
               #ifdef E4STACK
                  error4stack( c4, e4memory, E95302 ) ;
               #endif
               return 0 ;
            }

            TAG4FILE *tfile = t4->tagFile ;
            tfile->codeBase = c4 ;
            tfile->userCount = 1 ;

            if ( tfile->blockMemory == 0 )
               tfile->blockMemory = mem4create( c4, c4->memStartBlock, sizeof(B4BLOCK) + B4BLOCK_SIZE_INTERNAL -
                  (sizeof(B4KEY_DATA)) - (sizeof(short)) - (sizeof(char[2])), c4->memExpandBlock, 0 ) ;

            if ( tfile->blockMemory == 0 )
            {
               if ( tfile != 0 )
                  mem4free( c4->tagFileMemory, tfile ) ;
               mem4free( c4->tagMemory, t4 ) ;
               return 0 ;
            }

            char *bufPtr = 0 ;
            int rc ;
            if ( buf[0] != 0 )
            {
               // AS May 26/06 - was not properly using this path value...it needs to be a stored string
               // if ( i4ndx != 0 && i4->path != 0)
               if ( i4ndx != 0 && i4ndx->nameWithPath[0] != 0)
               {
                  // AS 06/14/99 -- if tag data contains path info, there were potential problems here...
                  // rc = u4namePath( buf, sizeof(buf), i4ndx->path ) ;
                  // u4ncpy( buf+rc, tagData->name, sizeof(buf)-rc - 1 ) ;
                  u4nameCurrentExtended( buf, sizeof( buf ), tagData->name, i4ndx->nameWithPath ) ;
               }
               else
               {
                  // AS 11/07/00 use the accessName which may include path info in this case...
                  // u4ncpy( buf, tagData->name, sizeof(buf) - 1 ) ;
                  u4nameCurrentExtended( buf, sizeof( buf ), tagData->name, i4ndx->accessName ) ;
               }

               u4trim( buf ) ;

               u4nameExt( buf, sizeof(buf), TAG4EXT, 0 ) ;

               u4namePiece( tfile->alias, sizeof( tfile->alias ), tagData->name, 0, 0 ) ;
               u4trim( tfile->alias ) ;
               bufPtr = buf ;
            }

            #ifdef E4ANALYZE
               if ( tfile == 0 )
               {
                  mem4free( c4->tagMemory, t4 ) ;
                  error4( c4, e4info, E95302 ) ;
                  return 0 ;
               }
            #endif

            if ( useTempTagFileNames )
            {
               #ifndef S4OFF_OPTIMIZE
                  // AS 06/09/99 there is a problem here that the file is not actually created.  In that
                  // case there are no tag file names available and we get a failure when attempting to
                  // use these non-available names for aliases...
                  // can get around problem by forcing current...
                  int oldForceCurrent = c4->opt.forceCurrent ;
                  c4->opt.forceCurrent = 1 ;
               #endif
               // AS May 24/02 - created file4createInternal for internal use to indicate file types
               rc = file4createInternal( &tfile->file, c4, 0, 1, OPT4INDEX ) ;
               #ifndef S4OFF_OPTIMIZE
                  c4->opt.forceCurrent = oldForceCurrent ;
               #endif
            }
            else
            {
               // AS May 24/02 - created file4createInternal for internal use to indicate file types
               rc = file4createInternal( &tfile->file, c4, buf, 1, OPT4INDEX ) ;
            }

            if ( rc )
            {
               // AS 11/27/00 was not removing entry from .cgp file, so it was created out of date!
               if ( !useTempTagFileNames )
               {
                  int rcsv = error4set( c4, 0 ) ;
                  i4removeTagEntryFromGroupFile( i4, buf ) ;
                  error4set( c4, rcsv ) ;
               }
               t4close( t4 ) ;
               if ( rc > 0 )
                  error4set( c4, rc ) ;
               return 0 ;
            }

            if ( bufPtr == 0 )  // temporary tag name
            {
               // AS 06/09/99 there is a problem here that the file is not actually created.  In that
               // case there are no tag file names available and we get a failure when attempting to
               // use these non-available names for aliases...
               #ifndef S4OFF_OPTIMIZE
                  assert5( tfile->file.fileCreated == 1 ) ;  // ensure the tag name is available...
               #endif

               u4namePiece( tfile->alias, sizeof( tfile->alias ), tfile->file.name, 0, 0 ) ;
               u4trim( tfile->alias ) ;
            }

            #ifndef S4OFF_OPTIMIZE
               file4optimizeLow( &tfile->file, c4->optimize, OPT4INDEX, 0, tfile ) ;
               #ifdef S4LOW_MEMORY
                  if ( hasOpt )
                     code4optRestart( c4 ) ;
               #endif
            #endif

            if ( tagData->unique )
            {
               tfile->header.unique = 0x01 ;

               t4->errUnique = tagData->unique ;

               #ifdef E4PARM_HIGH
                  #ifdef S4FOX
                     if ( tagData->unique != e4unique && tagData->unique != r4unique
                          tagData->unique != r4uniqueContinue && tagData->unique != r4candidate &&
                          tagData[i].unique != e4candidate )
                  #else
                     if ( tagData->unique != e4unique && tagData->unique != r4unique && tagData->unique != r4uniqueContinue )
                  #endif
                  {
                     t4close( t4 ) ;
                     error4describe( c4, e4tagInfo, E85301, tagData->name, 0, 0 ) ;
                     return 0 ;
                  }
               #endif
            }

            if ( tagData->descending)
            {
               tfile->header.descending = 1 ;
               #ifdef E4PARM_HIGH
                  if ( tagData->descending != r4descending )
                  {
                     t4close( t4 ) ;
                     error4describe( c4, e4tagInfo, E85302, tagData->name, 0, 0 ) ;
                     return 0 ;
                  }
               #endif
            }

            #ifdef E4PARM_HIGH
               if ( tagData->expression == 0 )
               {
                  t4close( t4 ) ;
                  error4describe( c4, e4tagInfo, E85303, tagData->name, tagData->expression, 0 ) ;
                  return 0 ;
               }
            #endif

            if ( (short) (c4strlen( tagData->expression ) + 1) > I4MAX_EXPR_SIZE )
            {
               t4close( t4 ) ;
               error4describe( c4, e4tagInfo, E85304, tagData->name, tagData->expression, 0 ) ;
               return 0 ;
            }
            tfile->expr = expr4parseLow( d4, tagData->expression, tfile ) ;
            if ( tfile->expr == 0 )
            {
               t4close( t4 ) ;
               return 0 ;
            }

            if ( tagData->filter != 0 )
            {
               if ( ( c4strlen( tagData->filter ) + 1 ) > I4MAX_EXPR_SIZE )
               {
                  t4close( t4 ) ;
                  error4describe( c4, e4tagInfo, E85304, tagData->name, tagData->filter, 0 ) ;
                  return 0 ;
               }
               if ( *( tagData->filter ) != '\0' )
               {
                  tfile->filter = expr4parseLow( d4, tagData->filter, tfile ) ;
                  if ( tfile->filter )
                  {
                     if (expr4type( tfile->filter ) != r4log )
                     {
                        error4describe( c4, e4create, E80905, expr4source( tfile->filter ), 0, 0 ) ;
                        expr4free( tfile->filter ) ;
                        tfile->filter = 0;
                        return 0 ;
                     }
                  }
               }
            }

            tfile->header.eof = 0 ;
            tfile->header.root = 1024 ;
            tfile->header.keyLen = c4->numericStrLen ;
            tfile->header.keyDec = c4->decimals ;
            if ( tfile->expr->type == r4num )
            {
               tfile->header.keyLen = tfile->expr->keyLen ;
               tfile->header.keyDec = tfile->expr->keyDec ;
            }

            if ( error4code( c4 ) < 0 )
            {
               t4close( t4 ) ;
               return 0 ;
            }

            /* add the tag to the index list */
            l4add( &i4->tags, t4 ) ;

            if ( i4ndx == 0 )   /* single create, so reindex now */
            {
               if ( t4reindex( t4 ) == r4unique )
               {
                  t4close( t4 ) ;
                  error4set( c4, r4unique ) ;
                  return 0 ;
               }

               l4add( &i4->data->indexes, i4 ) ;
            }

            l4add( &d4->dataFile->tagfiles, tfile ) ;
            if ( d4->dataFile->indexLocked == 1 )   /* index locked, so lock this tag as well */
               tfile4lock( tfile, data4serverId( d4 ) ) ;
            // AS Feb 7/03 - Need for locking control as well as transactions
            // #ifndef S4OFF_TRAN
               t4->isValid = 1 ;
            // #endif
            return t4 ;
         }
      #endif   /* S4CLIPPER */

      #ifdef S4WIN32  // CS 2000/04/12
         /* AS 07/21/99 - added parm for win 95/98 to avoid endless laze writes */
         // !S4OFF_INDEX, !S4OFF_WRITE, !S4CLIENT, S4WIN32
         INDEX4 * S4FUNCTION I4createOpen
         (
            DATA4 *data,
            const char *name,
            TAG4INFO *tagInfo,
            char accessMode,
            char safety,
            char createTemp,
            char fileFlush,
            char readOnly,
            short collatingSequence,
            Collate4name collateName,
            Collate4name collateNameUnicode
         )
         {
            if ( I4create( data, name, tagInfo, accessMode, safety, createTemp, readOnly, fileFlush, collatingSequence, collateName, collateNameUnicode ) != 0 )
               return 0 ;

            CODE4 *c4 = data->codeBase ;
            #ifdef S4CLIPPER
               char tagNameBuf[LEN4PATH] ;

               if ( name == 0 )  // use tag name only...
               {
                  c4strcpy( tagNameBuf, sizeof( tagNameBuf ), tagInfo[0].name ) ;  // AS Jan 3/06 VS 2005 clipper fix
                  u4nameExt( tagNameBuf, sizeof( tagNameBuf ), NTX4EXT, 0 ) ;
                  name =tagNameBuf ;
               }
            #endif

            // AS 06/09/00 was not compiling in S4OFF_MULTI
            #ifndef S4OFF_MULTI
               // AS 06/11/99 open the index file same mode as data file is opened in...
               int oldAccessMode = c4->accessMode ;
               c4->accessMode = (char)data->dataFile->file.lowAccessMode ;
            #endif

            #ifdef S4SERVER
               INDEX4 *index = I4open( data, name, accessMode, c4->errDefaultUnique, c4getReadOnly( c4 ), c4->singleClient, 1, fileFlush, c4->safety, c4->createTemp ) ;
            #else
               INDEX4 *index = I4open( data, name, accessMode, c4->errDefaultUnique, c4getReadOnly( c4 ), 0, 1, fileFlush, c4->safety, 0 ) ;
            #endif

            #ifndef S4OFF_MULTI
               c4->accessMode = oldAccessMode ;
            #endif

            return index ;
         }



         /* AS 07/21/99 - added parm for win 95/98 to avoid endless laze writes */
         // !S4OFF_INDEX, !S4OFF_WRITE, !S4CLIENT, S4WIN32
         int I4create
         (
            DATA4 *data,
            const char *name,
            TAG4INFO *tagInfo,
            char accessMode,
            char safety,
            char createTemp,
            char readOnly,
            int fileFlush,
            short collatingSequence,
            Collate4name collateName,
            Collate4name collateNameUnicode
         )
         {
            #ifndef S4OFF_SECURITY
               if ( account4userAllowIndex( &data->codeBase->currentClient->account, data ) == FALSE )
                  return e4authorize ;
            #endif

            CODE4 *c4 = data->codeBase ;
            #ifdef S4SERVER
               if ( c4->currentClient->server->doIndexCreate == 0 )
                  return e4notSupported ;
            #endif

            int oldTemp = c4->createTemp ;
            int oldAccessMode = c4->accessMode ;
            int oldSafety = c4->safety ;
            int oldFileFlush = c4->fileFlush ;
            #ifdef S4FOX
               short oldCollatingSequence = c4->collatingSequence ;
               Collate4name oldCollateName = c4->collateName ;
               Collate4name oldCollateNameUnicode = c4->collateNameUnicode ;
            #endif

            c4->createTemp = createTemp ;
            c4->safety = safety ;
            c4->accessMode = OPEN4DENY_RW ;
            c4->fileFlush = fileFlush ;
            #ifdef S4FOX
               c4->collateNameUnicode = collateNameUnicode ;
               c4->collateName = collateName ;
               c4->collatingSequence = collatingSequence ;
            #endif

            INDEX4 *i4 = i4create( data, name, tagInfo ) ;

            #ifdef S4FOX
               c4->collateNameUnicode = oldCollateNameUnicode ;
               c4->collateName = oldCollateName ;
               c4->collatingSequence = oldCollatingSequence ;
            #endif
            c4->createTemp = oldTemp ;
            c4->fileFlush = oldFileFlush ;
            c4->safety = oldSafety ;
            c4->accessMode = oldAccessMode ;

            if ( i4 == 0 )
               return e4create ;
            else
            {
               // AS Feb 10/03 - Allow to close if within a transaction...
               #ifndef S4OFF_TRAN
                  int oldStatus = code4tranStatus( c4 ) ;
                  code4tranStatusSet( c4, r4off ) ;
               #endif
               int rc = i4closeLow( i4 ) ;
               #ifndef S4OFF_TRAN
                  code4tranStatusSet( c4, oldStatus ) ;
               #endif
               // AS Apr 23/04 - need to return r4uniqueContinue (or the error4code setting) if that is the error code and all else is ok...t4index2
               if ( rc == 0 )
                  return error4code( c4 ) ;

               return rc ;
            }
         }
      #endif  // WIN32
   #endif /* !S4CLIENT */
#endif /* !S4OFF_INDEX && !S4OFF_WRITE */
