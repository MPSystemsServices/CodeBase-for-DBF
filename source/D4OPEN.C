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

/* d4open.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifdef __TURBOC__
   #pragma hdrstop
#endif

#ifdef S4OFF_MEMO
   extern char f4memoNullChar ;
#endif

#ifndef S4MACINTOSH
   #ifdef S4UNIX
      #include <sys/stat.h>
      #include <sys/types.h>
   #else
      #if !defined(S4WINCE) && !defined(S4PALM)
         #include "sys\stat.h"
      #endif
   #endif
#endif

#define LEN4NAME_ALLOC_LEN LEN4PATH + 8

static DATA4FILE *data4reopen( DATA4FILE *, char ** ) ;

static DATA4 *d4openInit( CODE4 *c4 )
{
   DATA4 *d4 ;

   #ifdef E4VBASIC
      if ( c4parm_check( c4, 1, E94301 ) )
         return 0 ;
   #endif

   if ( error4code( c4 ) < 0 )
      return 0 ;

   #ifdef E4ANALYZE
      if ( c4->debugInt != 0x5281 )
      {
         error4( 0, e4result, E81301 ) ;
         return 0 ;
      }
   #endif

   #ifdef S4DEMO_DURATION
      /* This switch imposes a limitation whereby the application can only
         open a table with within a set amount of time after the application
         starts. The value of S4DEMO_DURATION the size of the window, in
         minutes, to allow opens. */
      static time_t startTime = time();
      time_t nowTime = time();
      if ((nowTime == (time_t)-1) || ((nowTime - startTime) > (S4DEMO_DURATION * 60)))
      {
         error4(c4,e4demo,E94301);
         return 0;
      }
   #endif

   #ifdef S4DOS
      if ( d4negativeLockTest( c4 ) != r4success )
      {
         error4( c4, e4lock, E86203 ) ;
         return 0 ;
      }
   #endif

   #if defined( S4STAND_ALONE ) && !defined( S4OFF_TRAN )
      if ( c4->logOpen )
      {
         int rc = code4logOpen( c4, 0, 0 ) ;
         if ( rc < 0 )
            return 0 ;
         else
            error4set( c4, 0 ) ;   /* remove r4open if it already existed */
      }
   #endif

   #if defined( S4SERVER ) && defined( E4ANALYZE )
      if ( c4->currentClient == 0 || c4accessMutexCountZero( c4 ) )
      {
         error4( c4, e4struct, E94301 ) ;
         return 0 ;
      }
      if ( c4->currentClient->trans.c4trans == 0 )
      {
         error4( c4, e4struct, E94301 ) ;
         return 0 ;
      }
   #endif

   if ( c4->dataMemory == 0 )
   {
      c4->dataMemory = mem4create( c4, c4->memStartData, sizeof(DATA4), c4->memExpandData, 0 ) ;
      if ( c4->dataMemory == 0 )
      {
         #ifdef E4STACK
            error4stack( c4, e4memory, E94301 ) ;
         #endif
         return 0 ;
      }
   }
   d4 = (DATA4 *)mem4allocZero( c4->dataMemory ) ;
   if ( d4 == 0 )
   {
      #ifdef E4STACK
         error4stack( c4, e4memory, E94301 ) ;
      #endif
      return 0 ;
   }

   #ifdef E4VBASIC
      d4->debugInt = E4DEBUG_INT ;
   #endif
   d4->codeBase = c4 ;

   #ifdef S4SERVER
      // AS For ODBC, we want to assign the client->id to this value because we do not overwrite it, and it will
      // tell us which DATA4's belong to which session
      if ( c4->odbc == 1 && c4->currentClient != 0 )
         d4->clientId = c4->currentClient->id + 1 ;  /* should get overridden at a later point, unless this is a server-only data file */
      else
         d4->clientId = 1L ;  /* should get overridden at a later point, unless this is a server-only data file */
      d4->serverId = c4->server->serverDataCount ;
      // AS Apr 28/03 - made trans-shared a run-time switch
      if ( c4->transShared == 1 )
      {
         // AS Apr 21/03 in this case, we want the server id to be unique for each process, so offset by the TRAN4MAX_USERS value
         c4->server->serverDataCount += TRAN4MAX_USERS ;
      }
      else
         c4->server->serverDataCount++ ;
      d4->trans = &c4->currentClient->trans ;
      l4add( tran4dataList( d4->trans ), d4 ) ;
   #else
      d4->trans = &c4->c4trans.trans ;
      l4add( tran4dataList( (&(c4->c4trans.trans)) ), d4 ) ;
   #endif

   return d4 ;
}



static void d4openConcludeAllocateRecord( DATA4 *d4 )
{
   // allocates and assigns the record structures.
   // in case of error c4->errorCode is set to an error value on return
   long recWidth, recWidth2 ;

   assert5( d4 != 0 ) ;

   CODE4 *c4 = d4->codeBase ;
   assert5( c4 != 0 ) ;
   if ( error4code( c4 ) < 0 )
      return ;

   recWidth = dfile4recWidth( d4->dataFile ) ;

   #ifdef S4DATA_ALIGN
      #ifdef S4WIN64
         recWidth2 = (recWidth+1) + (8L - (recWidth+1L)%8L)%8L ;  /* LY 00/11/02 : align at 8 byte boundaries */
      #else
         recWidth2 = (recWidth+1) + (4L - (recWidth+1L)%4L)%4L ;  /* LY 99/07/23 : align at 4 byte boundaries*/
      #endif
   #else
      recWidth2 = recWidth +1 ;
   #endif  /* LY 99/12/06 : moved #endif from after if-block to here */

   if ( ( ( recWidth2 + 50L ) * 2L + (long)sizeof( FIELD4 ) * (long)d4->dataFile->nFields ) > (long)UINT_MAX )  /* try allocating records and fields together, 50 bytes for overhead */
   {
      /* AS Apr 10/02 - New function for advance-reading client/server */
      #ifdef S4CLIENT
         d4->groupRecordAlloc = (char *)u4allocFree( c4, (recWidth2) * 4L + (long)sizeof( FIELD4 ) * (long)d4->dataFile->nFields ) ;
      #else
         d4->groupRecordAlloc = (char *)u4allocFree( c4, (recWidth2) * 3L + (long)sizeof( FIELD4 ) * (long)d4->dataFile->nFields ) ;
      #endif
      if ( d4->groupRecordAlloc != 0 )
      {
         d4->record = d4->groupRecordAlloc ;
         d4->recordOld = d4->groupRecordAlloc + recWidth2 ;
         d4->recordBlank = d4->groupRecordAlloc + 2 * (recWidth2) ;
         #ifdef S4CLIENT
            d4->batchRead.recordExtra = d4->groupRecordAlloc + 3 * (recWidth2) ;
            d4->fields = (FIELD4 *)(d4->groupRecordAlloc + 4 * ( recWidth2 ) ) ;
         #else
            d4->fields = (FIELD4 *)(d4->groupRecordAlloc + 3 * ( recWidth2 ) ) ;
         #endif
      }
   }

   if ( d4->groupRecordAlloc == 0 )
   {
      d4->record = (char *)u4allocFree( c4, recWidth + 1 ) ;
      d4->recordOld = (char *)u4allocFree( c4, recWidth + 1 ) ;
      d4->recordBlank = (char *)u4allocFree( c4, recWidth + 1 ) ;
      #ifdef S4CLIENT
         d4->batchRead.recordExtra = (char *)u4allocFree( c4, recWidth + 1 ) ;
      #endif
      d4->fields = (FIELD4 *)u4allocFree( c4, sizeof( FIELD4 ) * (long)d4->dataFile->nFields ) ;
   }
}



#ifndef S4OFF_MEMO
   static void d4openConcludeSetupMemo( DATA4 *d4 )
   {
      // in case of error c4->errorCode is set to an error value on return
      assert5( d4 != 0 ) ;

      CODE4 *c4 = d4->codeBase ;
      assert5( c4 != 0 ) ;
      if ( ( error4code( c4 ) < 0 ) )
         return ;

      if ( d4->dataFile->nFieldsMemo > 0 && !( error4code( c4 ) < 0 ) )
      {
         unsigned short memoIndex = 0 ;

         d4->fieldsMemo = (F4MEMO *)u4allocFree( c4, (long)sizeof(F4MEMO) * d4->dataFile->nFieldsMemo ) ;
         if ( d4->fieldsMemo == 0 )  // failure to allocated
            return ;

         for ( unsigned short fieldIndex = 0 ; fieldIndex < d4->dataFile->nFields ; fieldIndex++ )
         {
            short fieldType = d4->fields[fieldIndex].type ;
            if ( fieldType == r4memo || fieldType == r4gen || ( fieldType == r4bin && s5mdx ) )
            {
               #ifdef E4ANALYZE
                  if ( memoIndex >= d4->dataFile->nFieldsMemo )  /* means we mis-counted somewhere, so didn't allocate enough memory */
                     error4describe( c4, e4struct, E94301, d4->alias, 0, 0 ) ;
               #endif
               d4->fields[fieldIndex].memo = d4->fieldsMemo + memoIndex ;
               d4->fieldsMemo[memoIndex].status = 1 ;
               d4->fieldsMemo[memoIndex].field = d4->fields + fieldIndex ;
               memoIndex++ ;
            }
         }
      }
   }
#endif /* !S4OFF_MEMO */



static void d4openConcludeSetupFields( DATA4 *d4, char *info )
{
   // in case of error c4->errorCode is set to an error value on return

   assert5( d4 != 0 ) ;

   if ( ( error4code( d4->codeBase ) < 0 ) )
      return ;

   unsigned long recOffset = 1 ;  // start after the 'deleted' first byte of record
   #ifdef S4CLIENT_OR_FOX
      unsigned short nullCount = 0 ;
   #endif

   int fieldHeaderSize = 32 ;
   #ifdef S4MDX
      // AS 09/21/00 - dbase 7 (version 4 files) have field info lengths of 48
      if ( d4->dataFile->version == 4 )
         fieldHeaderSize = 48 ;
   #endif

   #ifdef S4CLIENT_OR_FOX
      // AS Oct 27/03 - long field names support
      long offset = 0 ;
   #endif

   for ( unsigned short fieldIndex = 0 ; fieldIndex < d4->dataFile->nFields ; fieldIndex++ )
   {
      const FIELD4IMAGE *image ;
      // AS Nov 24/03 - Long field name support only available in fox
      #ifdef S4CLIENT_OR_FOX
         // AS Oct 27/03 - long field names support
         if ( d4->dataFile->longFieldNamesSupported == 1 )
         {
            image = (const FIELD4IMAGE *)(d4->dataFile->info + offset - 11 ) ;
            // LY Jun 29/04 : changed from short *len to short len, added different
            // assignment method for S4DATA_ALIGN
            short len ;
            #ifdef S4DATA_ALIGN
               // LY Jul 9/04 : cast &len to char* to avoid alignment error from numeric assembly operator in release mode
               memcpy( (char *)&len, d4->dataFile->info + offset + sizeof( FIELD4IMAGE ) - 11, sizeof( short ) ) ;
            #else
               len = *((short *)(d4->dataFile->info + offset + sizeof( FIELD4IMAGE ) - 11)) ;
            #endif
            d4->fields[fieldIndex].longName = (d4->dataFile->info + offset + sizeof( FIELD4IMAGE ) - 11 + sizeof( short )) ;
            // LY Jun 29/04 : changed from *len to len
            offset += (sizeof( FIELD4IMAGE ) - 11 + len + sizeof( short )) ;
         }
         else
      #endif
      {
         image = (FIELD4IMAGE *)( info + fieldIndex * fieldHeaderSize ) ;
         u4ncpy( d4->fields[fieldIndex].shortName, image->name, sizeof( d4->fields->shortName ) ) ;
         d4->fields[fieldIndex].longName = d4->fields[fieldIndex].shortName ;
      }
      c4upper( d4->fields[fieldIndex].longName ) ; /* LY 2004/02/10 : failing comparison in d4fieldNumber() */
      char fieldBuf[2] ;

      u4ncpy( fieldBuf, &image->type, 2 ) ;
      c4upper( fieldBuf ) ;
      d4->fields[fieldIndex].type = *fieldBuf ;
      short fieldType = d4->fields[fieldIndex].type ;
      #ifdef S4CLIENT_OR_FOX
         if ( d4version( d4 ) == 0x30 )  /* FOX 3.0 */
         {
            d4->fields[fieldIndex].null = ( image->nullBinary & 0x02 ) ? 1 : 0 ;
            #ifdef S4CLIENT_OR_FOX
               // if ( d4->dataFile->autoIncrementSupported )   // autoIncrement field included, so look if this is the field...
                  if ( image->nullBinary & 0x08 )
                  {
                     // AS Mar 2/04 - does not apply to client - this is caught by server if required
                     #ifndef S4CLIENT
                        // this field is an auto-increment field...
                        // AS Feb 3/04 - if FoxPro compatible autoIncrementSupported will be false... error out because we are not compatible
                        if ( d4->dataFile->autoIncrementSupported == 0 )
                        {
                           // AS Feb 5/04 - if file is read-only, this is ok (we can read their autoincrement fields, but can't increment them)
                           if ( d4->dataFile->file.isReadOnly == 0 )
                           {
                              error4describe( d4->codeBase, e4notSupported, E94301, "FoxPro 8.0 AutoIncrement field not supported", d4->alias, d4->fields[fieldIndex].shortName ) ;
                              return ;
                           }
                        }
                        else
                     #endif
                     {
                        d4->fields[fieldIndex].autoIncrement = 1 ;
                        d4->autoIncrementField = &d4->fields[fieldIndex] ;
                     }
                  }

               // if ( d4->dataFile->autoTimestampSupported )   // autoIncrement field included, so look if this is the field...
                  if ( image->nullBinary & 0x10 )
                  {
                     // this field is an auto-timestamp field...
                     d4->fields[fieldIndex].autoTimestamp = 1 ;
                     d4->autoTimestampField = &d4->fields[fieldIndex] ;
                  }
            #endif

            if ( d4->fields[fieldIndex].null == 1 )
            {
               d4->fields[fieldIndex].nullBit = nullCount ;

               // AS 05/24/00 for efficiency, track null info better...
               d4->fields[fieldIndex].nullBitByteNum = d4->fields[fieldIndex].nullBit / 8 ;
               unsigned short int offset = ( d4->fields[fieldIndex].nullBit - ( d4->fields[fieldIndex].nullBitByteNum * 8 ) ) ;
               d4->fields[fieldIndex].nullBitMask = 0x01 << offset ;

               nullCount++ ;
            }
            if ( image->nullBinary & 0x04 )
               d4->fields[fieldIndex].binary = 1 ;
            else
            {
               if ( fieldType == r4memo || fieldType == r4gen )  /* memo fields are also stored binary */
                  d4->fields[fieldIndex].binary = 2 ;
               else
                  d4->fields[fieldIndex].binary = 0 ;
            }
         }
      #endif

      switch( fieldType )
      {
         #ifdef S4CLIENT_OR_FOX
            case r4int:
         #endif
         case r4log:
         case r4date:
            d4->fields[fieldIndex].len = image->len ;
            break ;
         case r4double:  /* same as r4bin */
            if ( d4version( d4 ) == 0x30 )  /* double */
            {
               d4->fields[fieldIndex].len = image->len ;
               d4->fields[fieldIndex].dec = image->dec ;
            }
            else  /* binary */
               d4->fields[fieldIndex].len = image->len ;
            break ;
         case r4num:
         case r4float:
         #ifdef S4CLIENT_OR_FOX
            case r4currency:
            case r4dateTime:
            case r4dateTimeMilli:  // AS Mar 10/03 - ms support in datetime
            // AS Jul 21/05 - Support for new field type binary float
            case r4floatBin:
         #endif
            d4->fields[fieldIndex].len = image->len ;
            d4->fields[fieldIndex].dec = image->dec ;
            break ;
         case r4memo:
         case r4gen:
            d4->fields[fieldIndex].len = image->len ;
            break ;
         #if defined(S4FOX) || defined(S4CLIENT)
            // AS 05/24/00 for efficiency, track null info better...
            case r4system:
               assert5( d4->nullFlags == 0 ) ;  // should only be 1 instance...
               d4->nullFlags = &(d4->fields[fieldIndex]) ;
         #endif
         default:
            d4->fields[fieldIndex].len = image->len + ( image->dec << 8 ) ;
            break ;
      }

      #ifdef E4VBASIC
         d4->fields[fieldIndex].debugInt = E4DEBUG_INT ;
      #endif
      d4->fields[fieldIndex].offset = recOffset ;
      recOffset += d4->fields[fieldIndex].len ;
      d4->fields[fieldIndex].data = d4 ;
   }
}



static void d4openConcludeBlankRecord( DATA4 *d4 )
{
   // in case of error c4->errorCode is set to an error value on return

   assert5( d4 != 0 ) ;

   if ( ( error4code( d4->codeBase ) < 0 ) )
      return ;

   /* set up the blank field buffer before calling d4blank() */
   /* because some field types record blanks as 0, must do field by field */
   char *savePtr = d4->record ;
   d4->record = d4->recordBlank ;
   d4->record[0] = ' ' ;   /* reset the deleted flag */
   for ( unsigned short fieldIndex = d4numFields( d4 ) ; fieldIndex > 0 ; fieldIndex-- )
      f4blank( d4fieldJ( d4, fieldIndex ) ) ;
   d4->record = savePtr ;

   d4blank( d4 ) ;

   long recWidth = dfile4recWidth( d4->dataFile ) ;

   c4memcpy( d4->recordOld, d4->record, (unsigned)recWidth ) ;
   d4->recordChanged = 0 ;

   d4->record[recWidth] = 0 ;
   d4->recordOld[recWidth] = 0 ;
}



#ifndef S4OFF_INDEX
   static void d4openConcludeSetupIndex( DATA4 *d4, const char *name )
   {
      // in case of error c4->errorCode is set to an error value on return

      #if !defined( S4CLIENT ) && !defined( S4SERVER ) && !defined( S4CLIPPER )
         int oldSingleOpen ;
      #endif

      assert5( d4 != 0 ) ;

      CODE4 *c4 = d4->codeBase ;
      if ( ( error4code( c4 ) < 0 ) )
         return ;

      #ifdef S4CLIENT
         /* client will get all the index tags if autoOpen set to 1, else
            will get none -- this is an undocumented side-effect */
         if ( d4->dataFile->indexes.nLink > 0 && c4->autoOpen == 1 )
         {
            char nameBuf[LEN4NAME_ALLOC_LEN] ;
            // AS Oct 24/01 - If the index name is input 'name', then only set up the index file with that
            // name.  This allows 2 DATA4's to share the DATA4FILEs while not sharing the same indexes.
            u4namePiece( nameBuf, sizeof( nameBuf ), name, 0, 0 ) ;
            char name2[LEN4PATH] ;
            u4namePiece( name2, sizeof( name2 ), name, 1, 0 ) ;
            if ( i4setup( c4, d4, nameBuf, name2, 1, 0 ) < 0 )
               return ;
         }
      #else
         #ifdef S4CLIPPER
            if ( c4->autoOpen )
            {
               if ( d4->dataFile->userCount > 1 )  /* already open, just set up tags */
               {
                  char nameBuf[LEN4NAME_ALLOC_LEN] ;
                  u4namePiece( nameBuf, sizeof( nameBuf ), name, 0, 0 ) ;
                  if ( i4setup( c4, d4, nameBuf, 1 ) < 0 )
                     return ;
               }
               else
               {
                  #ifdef S4SERVER
                     /* if a temp file, tags already available... */
                     if ( file4getTemporary( &d4->dataFile->file ) == 1 )
                     {
                        char nameBuf[LEN4NAME_ALLOC_LEN] ;
                        u4namePiece( nameBuf, sizeof( nameBuf ), name, 0, 0 ) ;
                        if ( i4setup( c4, d4, nameBuf, 1 ) < 0 )
                           return ;
                     }
                  #endif
                  INDEX4 *i4 = i4open( d4, 0 ) ;
                  if ( i4 == 0 )
                  {
                     #ifdef S4SERVER
                         /* server version, if no .cgp file then don't open index */
                        // AS 10/01/99 --> ensure that it really is an 'open failure' not a corrupt index...
                        if ( ( error4code( c4 ) > -70 && error4code( c4 ) <= -60 ) || error4code( c4 ) > 0 )
                        error4set( c4, 0 ) ;
                     #endif
                     return ;
                  }
               }
            }
         #else /* S4CLIPPER else */
            d4->dataFile->openMdx = 0 ;
            if ( ( d4->dataFile->hasMdxMemo & 0x01 ) && c4->autoOpen )
            {
               #ifndef S4SERVER
                  oldSingleOpen = c4->singleOpen ;
                  c4->singleOpen = OPEN4SPECIAL ;
               #endif
               INDEX4 *i4 = i4open( d4, 0 ) ;
               #ifndef S4SERVER
                  c4->singleOpen = oldSingleOpen ;
               #endif
               if ( i4 == 0 )
               {
                  // in this instance, the index file is missing...
                  // AS 04/08/99 --> change for if errOpen false no error else server fails
                  // incorrectly when opening account4.dbf if indexes don't exist.
                  if ( c4->errOpen == 0 )
                     error4set( c4, r4noOpen ) ;
                  else
                  {
                     // AS 06/30/99 don't reset error code if error already set...
                     if ( error4code( c4 ) == 0 )
                        error4( c4, e4open, E94301 ) ;
                  }
                  return ;
               }
               #ifdef S4MDX
                  if ( !i4->indexFile->header.isProduction )
                  {
                     i4closeLow( i4 ) ;
                     // in this instance, the index file is missing...
                     // should not really error out if errOpen false, so change this...
                     if ( c4->errOpen == 0 )
                        error4set( c4, r4noOpen ) ;
                     else
                     {
                        // AS 06/30/99 don't reset error code if error already set...
                        if ( error4code( c4 ) == 0 )
                        {
                           // CS 2005/07/18 add descriptive message
                           error4describe( c4, e4open, E94301, "cannot auto-open non-production MDX index", 0, 0 ) ;
                        }
                     }
                     return ;
                  }
               #endif
               d4->dataFile->openMdx = 1 ;
            }
         #endif /* S4CLIPPER else */
      #endif  /* S4CLIENT */
   }
#endif  /* S4OFF_INDEX */



#if !defined( S4OFF_WRITE ) && !defined( S4OFF_TRAN ) && !defined( S4CLIENT )
   void d4openConcludeSetupTransactions( DATA4 *d4 )
   {
      #ifdef S4MACINTOSH
         char macNameBuf[258] ;
      #endif
      #ifdef S4WINCE    /* LY 00/03/07 */
         //HANDLE bufHandle ;
         BY_HANDLE_FILE_INFORMATION bufHandleInfo ;
         //unsigned short nameUBuf[258] ;
         S4LONG tempLong ;
         FILETIME fileT ;
         LONGLONG ll ;
      #endif
      // in case of error c4->errorCode is set to an error value on return
      assert5( d4 != 0 ) ;

      CODE4 *c4 = d4->codeBase ;
      if ( ( error4code( c4 ) < 0 ) )
         return ;

      // AS Apr 29/03 - transcations are run-time in odbc now
      // AS Jun 20/03 - was checking wrong flag
      #if defined( S4SERVER ) && defined( S4ODBC_BUILD )
         if ( c4->server->odbcTrans == 0 )  // odbc build, no trans available
            return ;
      #endif

      if ( code4transEnabled( c4 ) )
      {
         S4LONG connectionId ;
         #ifdef S4STAND_ALONE
            connectionId = 0L ;
         #else
            connectionId = c4->currentClient->id ;
         #endif

         TRAN4 *trans = code4trans( c4 ) ;

         char nameBuf[LEN4NAME_ALLOC_LEN] ;
         if ( u4nameCurrent( nameBuf, sizeof( nameBuf ), dfile4name( d4->dataFile ) ) < 0 )
            return ;

         if ( u4nameExt( nameBuf, sizeof( nameBuf ), DBF4EXT, 0 ) < 0 )
            return ;

         #ifdef S4MACINTOSH
            u4getMacPath( c4, macNameBuf, 256-strlen(nameBuf) ) ;
            strcat(macNameBuf, nameBuf ) ;
            strcpy(nameBuf, macNameBuf);
         #endif

         int tranCode ;

         // AS 02/27/01 - if the file is physicall marked as 'isTemp' then also do this...
         if ( c4->createTemp == 1 || file4getTemporary( &d4->dataFile->file ) == 1 )
         {
            // AS 03/19/01 - also log closes for temporary opens... - at least for now for utilities to run
            tranCode = TRAN4OPEN_TEMP ;
         }
         else
         {
            // AS 02/01/01 - Mark in data4 whether was logged so know if need to log the close message
            tranCode = TRAN4OPEN ;
         }

         #ifdef S4TESTING
            // AS 02/26/01 - allow to turn logging off of 't4suite' - and don't log the open either
            if ( c4->log == 0 )
            {
               // AS May 28/04 - or the ANDREW.T
               if ( strnicmp( nameBuf, "h:\\codebase.t", 13 ) == 0 || strnicmp( nameBuf, "h:\\andrew.t", 13 ) == 0 )  // in that directory, so ignore
                  return ;
            }
         #endif

         short nameLen = strlen( nameBuf ) ;
         if ( tran4set( trans, trans->currentTranStatus, -1L, connectionId, tranCode, (unsigned)nameLen + 19, data4clientId( d4 ), data4serverId( d4 ) ) < 0 )
            return ;

         assert5( sizeof( nameLen ) == sizeof( short ) ) ;   // in tran file, 2 bytes for length
         tran4putData( trans, &nameLen, sizeof( short ) ) ;
         tran4putData( trans, nameBuf, (unsigned)nameLen ) ;
         S4LONG rcl = dfile4recWidth( d4->dataFile ) ;
         tran4putData( trans, &rcl, 4 ) ;
         short rc = d4numFields( d4 ) ;
         tran4putData( trans, &rc, 2 ) ;
         rcl = d4recCount( d4 ) ;
         tran4putData( trans, &rcl, 4 ) ;
         #ifdef S4MACINTOSH
            HParamBlockRec MacFile ;

            MacFile.fileParam.ioFDirIndex = 0 ;
            MacFile.fileParam.ioNamePtr = (StringPtr)&d4->dataFile->file.macSpec.name ;
            MacFile.fileParam.ioVRefNum = d4->dataFile->file.macSpec.vRefNum ;
            MacFile.fileParam.ioDirID = d4->dataFile->file.macSpec.parID ;
            if (PBHGetFInfoSync((HParmBlkPtr)&MacFile) < 0)
            {
               error4( c4, e4open, E94301 ) ;
               return ;
            }
            tran4putData( trans, &MacFile.fileParam.ioFlMdDat, 4 ) ;
         #else
            #ifdef S4WINCE
               // AS Feb 17/06 - for Windows CE, it is possible to get errors if the card is not ready...retry if access denied error
               // Microsoft knowledgebase article #811693
               for ( short numTries = 0 ;; numTries++ )
               {
                  BOOL blRes = GetFileInformationByHandle( d4->dataFile->file.hand, &bufHandleInfo ) ;
                  if ( blRes == 0 )
                  {
                     DWORD err = GetLastError() ;
                     if ( numTries > 10 )  // wait up to 5 seconds
                        break ;
                     Bool5 doBreak = 0 ;
                     switch( err )  // only retry on certain errors which occur with this problem.
                     {
                        case ERROR_ACCESS_DENIED:
                        case ERROR_DEVICE_NOT_AVAILABLE:
                        case ERROR_DEVICE_REMOVED:
                        case ERROR_FILE_NOT_FOUND:
                           break ;
                        default:
                           doBreak = 1 ;
                           break ;
                     }
                     if ( doBreak )
                        break ;
                     Sleep( 500 ) ;
                  }
                  else
                     break ;
               }
               fileT = bufHandleInfo.ftLastAccessTime ;
               ll = (DWORD) fileT.dwHighDateTime ;
               ll = ll << 32 ;
               ll += (DWORD) fileT.dwLowDateTime ;
               tempLong = (time_t) ((ll - 116444736000000000)/10000000) ;
               time_t bufATime = (time_t) tempLong ;
               tran4putData( trans, &bufATime, 4 ) ;
            #else
               struct stat bufStat ;
               // AS July 17/02 - It may be possible that the file doesn't exist or can't be accessed (e.g. temporary non-created file)
               // in this case, just ignore the error and have blank info in the transaction file
               // if ( stat( nameBuf, &bufStat ) != 0 )
               // {
               //    error4( c4, e4open, E94301 ) ;
               //    return ;
               // }
               memset( &bufStat, 0, sizeof( bufStat ) ) ;
               stat( nameBuf, &bufStat ) ;
               tran4putData( trans, &bufStat.st_atime, 4 ) ;
            #endif
         #endif
         tran4putData( trans, &d4->dataFile->yy, 3 ) ;
         if ( tran4lowAppend( trans, 0, 0 ) != 0 )
            return ;

         // if all was successful, mark as logged
         if ( c4->createTemp == 1 || file4getTemporary( &d4->dataFile->file ) == 1 )
            d4->openWasLogged = 1 ;
         else
            d4->openWasLogged = 1 ;

      }
   }
#endif /* if !defined( S4OFF_WRITE ) && !defined( S4OFF_TRAN ) && !defined( S4CLIENT ) */



static int d4openConclude( DATA4 *d4, const char *name, char *info )
{
   CODE4 *c4 ;

   assert5( d4 != 0 ) ;
   c4 = d4->codeBase ;
   assert5( c4 != 0 ) ;
   if ( error4code( c4 ) < 0 )
      return e4codeBase ;

   error4set( c4, 0 ) ; // in case of 70 r4open cases...

   #ifndef S4CLIENT
      #ifndef S4OFF_MULTI
         l4add( &d4->dataFile->data4list, &d4->dataFileLink ) ;
      #endif
   #endif

   #ifdef E4ANALYZE
      if ( d4->dataFile->nFields == 0 )
         return error4describe( c4, e4struct, E94301, name, 0, 0 ) ;
   #endif

   u4namePiece( d4->alias, sizeof( d4->alias ), name, 0, 0 ) ;
   d4->alias[ sizeof( d4->alias ) - 1 ] = 0 ;
   d4aliasFix( d4 ) ;

   d4openConcludeAllocateRecord( d4 ) ;
   d4openConcludeSetupFields( d4, info ) ;
   #ifndef S4OFF_MEMO
      d4openConcludeSetupMemo( d4 ) ;
   #endif

   if ( error4code( c4 ) < 0 )
      return -1 ;

   d4->recNum = d4->recNumOld = -1 ;

   d4openConcludeBlankRecord( d4 ) ;
   #ifndef S4OFF_INDEX
      d4openConcludeSetupIndex( d4, name ) ;
   #endif

   // AS Dec 15/03 - not supported in clipper because the tag file name issue (end up with many files of same name)
   #if !defined( S4CLIENT ) && !defined( S4OFF_INDEX ) && !defined( S4CLIPPER )
      // AS Jun 11/02 - added support for re-using of deleted rows.
      if ( d4->dataFile->appendTag == 0 )
      {
         int oldErrCode = c4->errTagName ;
         c4->errTagName = 0 ;
         d4->dataFile->appendTag = dfile4tag( d4->dataFile, DEL4REUSE_TAG_NAME ) ;
         c4->errTagName = oldErrCode ;
      }
   #endif

   if ( error4code( c4 ) != 0 )  // also return -1 if r4open occurs (expected index does not exist... */
      return -1 ;

   #ifndef S4SERVER
      c4->clientDataCount++ ;
      // AS Apr 15/03 - Support for sharing locks on clones, due this via lockId
      d4->clientId = d4->lockId = c4->clientDataCount ;
   #endif

   #if !defined( S4OFF_WRITE ) && !defined( S4OFF_TRAN ) && !defined( S4CLIENT )
      d4openConcludeSetupTransactions( d4 ) ;
   #endif

   if ( error4code( c4 ) < 0 )
      return -1 ;

   #ifdef S4SERVER
      d4->accessMode = c4->singleClient ;
      if ( d4->accessMode == OPEN4DENY_RW )
      {
         d4->dataFile->exclusiveOpen = d4 ;
         d4->dataFile->singleClient = c4->currentClient ;
      }
   #endif

   #ifdef S4CLIENT
      // AS Oct 12/01 - Was not setting readOnly on DATA4 for client in case
      // where file goes from full access to read-only request.
      if ( d4->readOnly == 0 && c4getReadOnly( c4 ) )
         d4->readOnly = c4getReadOnly( c4 ) ;
      // AS Dec 17/02 - default configure the read bufferring
      d4readBufferConfigure( d4, r4batchSkip |  r4batchTop | r4batchBottom | r4batchSeekNext | r4batchSeekMatch ) ;
   #else
      /* 07/30/96 AS --> previously just check c4 setting.  should instead
         check file read only as well (but still allow read-only override
         for server handling */
      if ( d4->dataFile->file.isReadOnly == 1 )   /* file is read-only attribute, so mark as read-only */
         d4->readOnly = 1 ;
      else
         d4->readOnly = c4getReadOnly( c4 ) ;
   #endif

   #if !defined( S4OFF_TRAN ) && !defined( S4CLIENT )
      // AS Nov 12/02 - we may want to mark a file as no-logging even if transactions are not enabled.
      // In particular, we may have the compress log option on where the log file gets created at some point.
      // Therefore, set this value all the time anyway as a marker for the data file.
      // AS Nov 20/02 - This applies for the server only 0 there is no log-compress in stand-alone.
      // AS Apr 29/03 - transcations are run-time in odbc now
      // AS Jun 20/03 - was checking wrong flag
      #if defined( S4SERVER ) && defined( S4ODBC_BUILD )
         if ( c4->server->odbcTrans == 1 )  // odbc build, no trans available
      #endif
         #ifdef S4STAND_ALONE
            if ( code4transEnabled( d4->codeBase ) == 1 )
         #endif
               d4->logVal = c4->log ;
   #endif

   return 0 ;
}



DATA4 *S4FUNCTION d4open( CODE4 *c4, const char *name )
{
   char *info ;
   DATA4 *d4 ;

   #ifdef E4PARM_HIGH
      if ( c4 == 0 || name == 0 )
      {
         error4( 0, e4parm_null, E94301 ) ;
         return 0 ;
      }
   #endif

   d4 = d4openInit( c4 ) ;
   if ( d4 == 0)
      return 0 ;

   #if defined( S4PREPROCESS_FILE ) && !defined( S4CLIENT )
      // AS 02/12/01 - want to turn off errOpen in case it is an encrption problem.  If it is another problem, we
      // will catch it on the alternate open.
      Bool5 changedPreprocessionFlag = 0 ;  // true if we successfulling opened in opposite mode - need to set for opening memo/index files
      int oldErrOff = c4->errOff ;
      c4->errOff = 1 ;
   #endif
   d4->dataFile = dfile4open( c4, d4, name, &info ) ;
   #if defined( S4PREPROCESS_FILE ) && !defined( S4CLIENT )
      c4->errOff = oldErrOff ;
   #endif
   if ( d4->dataFile == 0 )
   {
      #if defined( S4PREPROCESS_FILE ) && !defined( S4CLIENT )
         // AS Sep 23/03 - for ODBC don't automatically enable preprocessing
         if ( c4->odbc == 1
         #ifdef S4ENCRYPT_DLL // LY Jul 16/04
            && c4->encrypt == 0
         #else
            && !c4->encryptInit
         #endif
         )
         {
            d4close( d4 ) ;
            return 0 ;
         }
         int rcSv = error4set( c4, 0 ) ;
         // try opening the file in the opposite mode
         short oldPreprocess = code4getPreprocessFile( c4 ) ;
         // AS Dec 3/03 - we want the ability to set off the encryption error, in particular when we attempt to load it internally
         assert5( c4->errEncrypt == 1 ) ;  // it should be set to 1
         c4->errEncrypt = 0 ;
         int rc = code4setPreprocessFile( c4, ( oldPreprocess ? 0 : 1 ) ) ;  // just put to the opposite number
         c4->errEncrypt = 1 ;
         if ( rc != 0 )  // in particular in server, it may return e4notSupported
         {
            // AS Jul 16/04 - just means not accessible, reset error code
            // AS and close the d4 in this case...
            d4close( d4 ) ;
            // AS Jan 30/04 - we also need to return a full error to the user here if available.
            if ( c4->errOpen )
               error4describe( c4, rcSv, E94301, name, (char *)0, (char *)0 ) ;
            else
               error4set( c4, rcSv ) ;
            return 0 ;
         }
         d4->dataFile = dfile4open( c4, d4, name, &info ) ;
         code4setPreprocessFile( c4, oldPreprocess ) ;
         if ( d4->dataFile == 0 )
         {
            d4close( d4 ) ;
            return 0 ;
         }
         changedPreprocessionFlag = 1 ;
      #else
         d4close( d4 ) ;
         return 0 ;
      #endif
   }

   #if defined( S4PREPROCESS_FILE ) && !defined( S4CLIENT )
      // AS Apr 23/03 - Don't record old value unless we changed - to avoid error on change due to not loading dll
      short oldPreprocess ;
      if ( changedPreprocessionFlag )
      {
         oldPreprocess = code4getPreprocessFile( c4 ) ;
         if ( changedPreprocessionFlag )  // we changed it before, so swap it again here.
            code4setPreprocessFile( c4, ( oldPreprocess ? 0 : 1 ) ) ;  // just put to the opposite number
      }
   #endif
   int rc = d4openConclude( d4, name, info ) ;
   #if defined( S4PREPROCESS_FILE ) && !defined( S4CLIENT )
      if ( changedPreprocessionFlag )
         code4setPreprocessFile( c4, oldPreprocess ) ;
   #endif

   if ( rc < 0 )
   {
      d4close( d4 ) ;
      return 0 ;
   }

   #ifdef ACMEDEMO
      if ( strcmp( d4alias( d4 ), "CB6DEMO" ) != 0 )
      {
         code4initUndo( c4 ) ;
         return 0 ;
      }
   #endif

   // AS Dec 4/02 - if is already valid (did not actually open), then don't need to log validation info...
   // AS 05/04/99 -- was previously marking d4->dataFile as valid too early, not getting properly closed...
   #ifndef S4CLIENT
      if ( d4->dataFile->valid == 0 )
      {
         d4->dataFile->valid = 1 ;
         // AS Dec 4/02 - moved validate code to this point (from dfile4open) where table is actually marked as valid - to ensure invalid
         // tables don't get left with markers in place
         #ifndef S4OFF_INDEX
            if ( c4->validationTable != 0 )
               code4validateAddOpen( c4, d4->dataFile->file.name, file4getTemporary( &d4->dataFile->file ) ) ;
         #endif
      }
   #endif /* #if defined( S4OFF_MULTI ) && !defined( S4CLIENT ) */

   // AS Nov 20/02 - track for preprocessing - AS Jan 28/03 - moved to later, we automatically enable encryption
   // if required on successful open
   #if defined( S4PREPROCESS_FILE ) && defined( S4STAND_ALONE )
      c4->fileAccessed = 1 ;
   #endif

   return d4 ;
}



DATA4 *S4FUNCTION d4openClone( DATA4 *dataOld )
{
   DATA4 *d4 ;
   CODE4 *c4 ;
   int rc ;
   char *info ;
   #ifndef S4OFF_INDEX
      TAG4 *tagNew, *tagOld ;
      #ifdef S4CLIENT
         INDEX4 *i4, *i42 ;
      #else
         #ifdef S4CLIPPER
            TAG4 *t4, *t42 ;
         #else
            INDEX4 *i4 ;
         #endif
      #endif
   #endif
   #ifndef S4SERVER
      int oldSingleOpen ;
   #endif

   #ifdef E4PARM_HIGH
      if ( dataOld == 0 )
      {
         error4( 0, e4parm_null, E94301 ) ;
         return 0 ;
      }
   #endif

   c4 = dataOld->codeBase ;
   d4 = d4openInit( c4 ) ;
   if ( d4 == 0 )
      return 0 ;
   #ifndef S4SERVER
      oldSingleOpen = c4->singleOpen ;
      c4->singleOpen = OPEN4DENY_NONE ;
      // AS Oct 9/03 - also relax the read-only setting in the non-server case if the file is physically read-only
      #ifdef S4STAND_ALONE
         int oldReadOnly = c4->readOnly ;
         if ( dataOld->dataFile->file.isReadOnly == 1 )
            c4->readOnly = 1 ;
      #endif
   #endif
   d4->dataFile = data4reopen( dataOld->dataFile, &info ) ;
   if ( d4->dataFile == 0 )
   {
      #ifndef S4SERVER
         c4->singleOpen = oldSingleOpen ;
         #ifdef S4STAND_ALONE
            c4->readOnly = oldReadOnly ;
         #endif
      #endif
      d4close( d4 ) ;
      return 0 ;
   }
   rc = d4openConclude( d4, dfile4name( d4->dataFile ), info ) ;
   #ifndef S4SERVER
      c4->singleOpen = oldSingleOpen ;
      #ifdef S4STAND_ALONE
         c4->readOnly = oldReadOnly ;
      #endif
      // AS Apr 15/03 - Support for sharing locks on clones, due this via lockId
      if ( c4->shareCloneLocks == 1 )
         d4->lockId = dataOld->lockId ;
   #endif
   if ( rc < 0 )
   {
      d4close( d4 ) ;
      return 0 ;
   }

   /* AS 03/03/97, need to open non-production indexes as well, esp. for relate
      module in client/server */

   #ifndef S4OFF_INDEX
      #ifdef S4CLIENT
         for ( i4 = 0 ;; )
         {
            i4 = (INDEX4 *)l4next( &dataOld->indexes, i4 ) ;
            if ( i4 == NULL )
               break ;
            for( i42 = 0 ;; )   /* see if exists first (i.e. production) */
            {
               i42 = (INDEX4 *)l4next( &d4->indexes, i42 ) ;
               if ( i42 == 0 )
               {
                  #ifdef S4CLIENT
                     i4open( d4, i4->alias ) ;
                  #else
                     i4open( d4, i4->accessName ) ;
                  #endif
                  break ;
               }
               if ( i42->indexFile == i4->indexFile )  /* don't open */
                  break ;
            }
         }
      #else
         #ifdef S4CLIPPER
            for ( t4 = 0 ;; )
            {
               t4 = d4tagNext( dataOld, t4 ) ;
               if ( t4 == NULL )
                  break ;
               for( t42 = 0 ;; )   /* see if exists first (i.e. production) */
               {
                  t42 = d4tagNext( d4, t42 ) ;
                  if ( t42 == 0 ) /* open */
                  {
                     i4open( d4, t4->tagFile->file.name ) ;
                     break ;
                  }
                  if ( t42->tagFile == t4->tagFile )  /* don't open */
                     break ;
               }
            }
         #else
            /* AS 04/30/99 -- production index file does not get opened either if autoOpen false, so take this into
               account by checking if we have any indexes now.  If we do, then don't re-open
               production index, else we must do it as well...*/
            Bool5 cloneHasProductionIndexOpened = 0 ;
            if ( l4first( &d4->indexes ) != 0 )
               cloneHasProductionIndexOpened = 1 ;
            for ( i4 = 0 ;; )
            {
               i4 = (INDEX4 *)l4next( &dataOld->indexes, i4 ) ;
               if ( i4 == NULL )
                  break ;
               if ( !index4isProduction( i4->indexFile ) || (cloneHasProductionIndexOpened == 0 ))
               {
                  /* not production or clone didn't open production, so didn't get opened -- open now... */
                  #ifdef S4CLIENT
                     i4open( d4, i4->alias ) ;
                  #else
                     // AS Oct 03/01 If the data file was originally opened from outside the current
                     // directory, the i4->accessName does not sufficiently describe the index, so use
                     // the physical file name instead.
                     // i4open( d4, i4->accessName ) ;
                     i4open( d4, i4->indexFile->file.name ) ;
                  #endif
               }
            }
         #endif /* S4CLIPPER */
      #endif /* S4CLIENT */
      /* now go through all the tags, and set the unique settings to the same as the old data */
      for ( tagOld = 0 ;; )
      {
         tagOld = d4tagNext( dataOld, tagOld ) ;
         if ( tagOld == 0 )
            break ;
         for ( tagNew = 0 ;; )
         {
            tagNew = d4tagNext( d4, tagNew ) ;
            if ( tagNew == 0 )   /* shouldn't happen - means an index tag got lost */
            {
               d4close( d4 ) ;
               error4( c4, e4info, E94301 ) ;
               return 0 ;
            }
            if ( tagNew->tagFile == tagOld->tagFile )
               break ;
         }
         tagNew->errUnique = tagOld->errUnique ;
      }
   #endif /* S4OFF_INDEX */

   return d4 ;
}



#if !defined( S4OFF_INDEX ) && defined( S4CLIENT )
   /* AS June 5/01 - see c4com.h CONNECTION4OPEN_INDEX_INFO_OUT structure for info on changes */
   int client4indexSetup( CODE4 *c4, DATA4 *d4, DATA4FILE *data, unsigned int numTags, unsigned short fullIndexNameLen, const char *info, unsigned int iLen, const char *indexAlias, INDEX4 *i4ndx )
   {
      unsigned int i ;
      TAG4FILE *tag, *first ;
      INDEX4FILE *i4file ;
      long infoLen ;
      DATA4FILE *oldDataFile ;
      int doTags ;

      #ifdef E4PARM_LOW
         if ( c4 == 0 || d4 == 0 || data == 0 || info == 0 )
            return error4( c4, e4parm_null, E94302 ) ;
      #endif

      if ( numTags == 0 )
         return 0 ;

      infoLen = iLen ;

      /* AS June 5/01 - see c4com.h CONNECTION4OPEN_INDEX_INFO_OUT structure for info on changes */
      const char *nameLookup = info ;  // this lookup name will be null ended since the null is sent from the server
      if ( fullIndexNameLen != 0 )  // will be 0 in the clipper case where we don't do this check
      {
         /* extract the full index name here, and do a lookup on it to see if we have it open */
         info += fullIndexNameLen ;  // update the info pointer to bypass the name now
         iLen -= fullIndexNameLen ;
         INDEX4FILE *indexOn ;
         for( indexOn = 0 ;; )
         {
            indexOn = (INDEX4FILE *)l4next( &data->indexes, indexOn) ;
            if ( indexOn == 0 )
               break ;
            if ( strcmp( indexOn->fullIndexName, nameLookup ) == 0 )  // found duplicate
               return r4noOpen ;
         }
      }

      if ( c4->index4fileMemory == 0 )
      {
         c4->index4fileMemory = mem4create( c4, c4->memStartIndexFile, sizeof(INDEX4FILE), c4->memExpandIndexFile, 0 ) ;
         if ( c4->index4fileMemory == 0 )
            return e4memory ;
      }

      if ( c4->tagFileMemory == 0 )
      {
         c4->tagFileMemory = mem4create( c4, c4->memStartTagFile, sizeof(TAG4FILE), c4->memExpandTagFile, 0 ) ;
         if ( c4->tagFileMemory == 0 )
            return e4memory ;
      }

      oldDataFile = d4->dataFile ; ;
      d4->dataFile = data ;
      /* passing non-null into index4open will ensure that an actual open
         does not occur, but simply a check will occur ... */
      if ( i4ndx != 0 )
         i4file = i4ndx->indexFile ;
      else
         i4file = index4open( d4, indexAlias, (INDEX4 *)1 ) ;
      if ( i4file == 0 )
      {
         i4file = (INDEX4FILE *)mem4allocZero( c4->index4fileMemory ) ;
         if ( i4file == 0 )
         {
            d4->dataFile = oldDataFile ;
            return error4stack( c4, e4memory, E94302 ) ;
         }

         i4file->codeBase = c4 ;
         i4file->autoOpened = 1 ;
         i4file->dataFile = data ;
         i4file->clientId = data4clientId( d4 ) ;
         i4file->serverId = data4serverId( d4 ) ;
         d4->dataFile = oldDataFile ;

         if ( strlen( indexAlias ) > sizeof( i4file->accessName ) )
            return error4describe( c4, e4name, E91102, indexAlias, 0, 0 ) ;
         strcpy( i4file->accessName, indexAlias ) ;

         l4add( &data->indexes, i4file ) ;
         doTags = 1 ;
      }
      else
         doTags = 0 ;

      /* AS June 5/01 - see c4com.h CONNECTION4OPEN_INDEX_INFO_OUT structure for info on changes */
      if ( fullIndexNameLen != 0 )  // will be 0 in the clipper case where we don't do this check
         strcpy( i4file->fullIndexName, nameLookup ) ;

      /* only execute next if i4file was not blank and i4ndx was blank */
      if ( i4ndx != 0 || doTags == 1 )   /* new index file, or add to existing */
      {
         for ( i = 0 ; i < numTags ; i++ )
         {
            tag = (TAG4FILE *)mem4allocZero( c4->tagFileMemory ) ;
            if ( tag == 0 )
               return e4memory ;
            infoLen -= LEN4TAG_ALIAS ;
            if ( infoLen < 0 )
               return e4connection ;
            memcpy( tag->alias, info, LEN4TAG_ALIAS ) ;
            tag->indexFile = i4file ;
            tag->codeBase = c4 ;
            info += LEN4TAG_ALIAS ;
            tag->errUniqueHold = ntohs5(*(short *)info) ;
            info += sizeof( short int ) ;
            first = (TAG4FILE *)l4first( &i4file->tags ) ;
            if ( first == 0 )
               l4add( &i4file->tags, tag ) ;
            else
               l4addBefore( &i4file->tags, first, tag ) ;
         }
      }
      else
         d4->dataFile = oldDataFile ;

      i4file->isValid = 1 ;
      return 0 ;
   }
#endif  /* #if !defined( S4OFF_INDEX ) && defined( S4CLIENT ) */



#ifdef S4SERVER
   static void dfile4accesses( const DATA4FILE *d4, int *readMode, int *accessMode, int *otherUsers )
   {
      /* what is the maximal read and access setting on the data file */
      /* does not report results for current client */
      SERVER4CLIENT *client ;
      DATA4 *data ;
      CODE4 *c4 = d4->c4 ;

      *readMode = 1 ;
      *accessMode = OPEN4DENY_NONE ;
      *otherUsers = 0 ;

      // AS Aug 2/02 - should always have exclusive c4 if at this point... verify this
      #if defined( E4ANALYZE ) && defined( S4SERVER )
         assert5( c4accessMutexCountNotZero( c4 ) ) ;
      #endif

      /* reserve the client list during this process */
      server4clientListReserve( c4->server ) ;

      for ( client = 0 ;; )
      {
         client = server4clientGetNext( c4->server, client ) ;
         if ( client == 0 )
            break ;
         if ( client == c4->currentClient )
            continue ;
         // AS Aug 2/02 - potentially causing gpf here if dataList is null (should never happen, but
         // was occurring in a strange case - maybe memory was corrupted already)
         LIST4 *list = tran4dataList( &client->trans ) ;
         if ( list == 0 )
            continue ;
         for ( data = 0 ;; )
         {
            data = (DATA4 *)l4next( list, data ) ;
            if ( data == 0 )
               break ;
            if ( data->dataFile == d4 )
            {
               *otherUsers = 1 ;
               if ( data->readOnly == 0 )
                  *readMode = 0 ;
               if ( *accessMode != OPEN4DENY_RW)
                  if ( data->accessMode != *accessMode )
                     if ( data->accessMode != OPEN4DENY_NONE )
                        *accessMode = data->accessMode ;
               if ( *readMode == 0 && *accessMode == OPEN4DENY_RW )  /* maximal already */
               {
                  server4clientListRelease( c4->server ) ;
                  return ;
               }
            }
         }
      }

      server4clientListRelease( c4->server ) ;
      return ;
   }



   static int dfile4checkAccess( const DATA4FILE *d4, int accessRequested, int readOnly )
   {
      /* returns 1 if file can be accessed in desired mode */
      int maxAccess, maxRead, otherUsers ;

      if ( d4->userCount == 0 )
         return 1 ;

      /* first get maximal access and read modes of other users */
      dfile4accesses( d4, &maxRead, &maxAccess, &otherUsers ) ;

      if ( otherUsers == 0 )   /* no other users, so any requests are ok */
         return 1 ;

      if ( accessRequested == OPEN4DENY_RW )   /* others users accessing, so no */
         return 0 ;

      if ( maxAccess == OPEN4DENY_RW )   /* other user is disallowing our access */
         return 0 ;

      if ( readOnly == 0 )   /* need write access */
      {
         if ( maxAccess != OPEN4DENY_NONE )
            return 0 ;
         /* maxAccess is DENY_NONE, so continue */
         switch( accessRequested )
         {
            case OPEN4DENY_NONE:
               return 1 ;
            case OPEN4DENY_WRITE:
               if ( maxRead == 1 )   /* others only reading, so ok */
                  return 1 ;
            /* fall through, and any other case, access denied */
            default:
               return 0 ;
         }
      }

      /* is readOnly, so deny_write allowable by others */
      switch( maxAccess )
      {
         case OPEN4DENY_RW:
            return 0 ;
         case OPEN4DENY_WRITE:
            switch ( accessRequested )
            {
               case OPEN4DENY_NONE:
                  return 1 ;
               case OPEN4DENY_WRITE:
                  if ( maxRead == 1 )
                     return 1 ;
               /* fall through or default, no access */
               default:
                  return 0 ;
            }
         default:
            break ;
      }

      return 1 ;
   }
#endif  /* S4SERVER */


#if !defined( S4CLIENT ) && !defined( S4OFF_MULTI )
   static int data4reopenCloseData( DATA4FILE **d4 )
   {
      // if the data file is not in use but needs to be openend in a different mode than it is currently opened in,
      // this function is often called (eg. was opened in readOnly, but we now want to open it in read/write)
      int rc = dfile4closeLow( *d4 ) ;
      if ( rc != 0 )
         return rc ;
      #ifndef S4OFF_INDEX
         #ifdef S4CLIPPER
            TAG4FILE *t4file ;
            for ( t4file = 0 ;; )
            {
               t4file = (TAG4FILE *)l4next( &(*d4)->tagfiles, t4file ) ;
               if ( t4file == 0 )
                  break ;
               rc = tfile4close( t4file, *d4 ) ;
               if ( rc < 0 )
                  return rc ;
            }
         #else
            if ( (*d4)->indexes.nLink != ((unsigned int)(*d4)->hasMdxMemo & 0x01 ) )
            {
               INDEX4FILE *i4file ;
               for ( i4file = 0 ;; )
               {
                  i4file = (INDEX4FILE *)l4next( &(*d4)->indexes, i4file ) ;
                  if ( i4file == 0 )
                     break ;
                  if ( index4isProduction( i4file ) == 1 )
                     continue ;
                  #ifdef E4ANALYZE
                     unsigned long nCheck = (*d4)->indexes.nLink ;
                  #endif
                  rc = index4close( i4file ) ;
                  #ifdef E4ANALYZE
                     if ( nCheck != (*d4)->indexes.nLink + 1 )
                        return error4describe( (*d4)->c4, e4result, E91102, dfile4name( *d4 ), 0, 0 ) ;
                  #endif
                  if ( rc < 0 )
                     return rc ;
               }
            }
         #endif /* S4CLIPPER */
      #endif /* S4OFF_INDEX */
      *d4 = 0 ;

      return 0 ;
   }
#endif /* !defined( S4CLIENT ) && !defined( S4OFF_MULTI ) */



#ifndef S4CLIENT
   static int data4reopenCheckIfMustClose( DATA4FILE **d4 )
   {
      // if the data file is not in use but needs to be openend in a different mode than it is currently opened in,
      // this function is often called (eg. was opened in readOnly, but we now want to open it in read/write)
      #ifndef S4OFF_MULTI
         CODE4 *c4 = (*d4)->c4 ;
         // AS from ODBC in particular stand/alone where we have opened the file in readonly mode physically, but
         // we may want to open in non-read-only
         Bool5 needToOpenInUpdatedMode = ( (*d4)->file.lowAccessMode != c4->accessMode ) ? 1 : 0 ;
         if ( needToOpenInUpdatedMode == 0 && (*d4)->file.isReadOnly == 1 )
         {
            #ifdef S4SERVER
               if ( c4->readOnlyRequest != 1 )
            #else
               if ( c4getReadOnly( c4 ) != 1 )
            #endif
                  needToOpenInUpdatedMode = 1 ;
         }

         if ( needToOpenInUpdatedMode )  /* need to open in updated mode */
         {
            int rc = data4reopenCloseData( d4 ) ;
            if ( rc != 0 )
               return -1 ;
         }
      #endif /* S4OFF_MULTI */
      return 0 ;
   }
#endif /* !S4CLIENT */



#ifndef S4SERVER
   static int data4reopenVerifyOnlyOneInstance( S4CONST DATA4FILE *d4 )
   {
      CODE4 *c4 = d4->c4 ;
      if ( c4->singleOpen != OPEN4DENY_NONE )   /* only one instance allowed... */
      {
         #ifndef S4OFF_TRAN
            /* verify that data4 not on the closed data list if within a transaction (which is allowed) */
            if ( code4tranStatus( c4 ) == r4active )
            {
               LIST4 *list = tran4dataList( code4trans( c4 ) ) ;
               DATA4 *data4 ;
               for ( data4 = 0 ;; )
               {
                  data4 = (DATA4 *)l4next( list, data4 ) ;
                  if ( data4 == 0 )
                     break ;
                  if ( data4->dataFile == d4 )
                     return error4describe( c4, e4instance, E91102, dfile4name( d4 ), 0, 0 ) ;
               }
               #ifdef E4ANALYZE
                  /* ensure that the datafile exists somewhere! */
                  list = &( code4trans( c4 )->closedDataFiles ) ;
                  for ( data4 = 0 ;; )
                  {
                     data4 = (DATA4 *)l4next( list, data4 ) ;
                     if ( data4 == 0 )
                        return error4describe( c4, e4struct, E91102, dfile4name( d4 ), 0, 0 ) ;
                     if ( data4->dataFile == d4 )
                        break ;
                  }
               #endif
            }
            else
         #endif /* S4OFF_TRAN */
         {
            return error4describe( c4, e4instance, E91102, dfile4name( d4 ), 0, 0 ) ;
         }
      }

      return 0 ;
   }
#endif



static int data4reopenVerifyDesiredAccessAvail( S4CONST DATA4FILE *d4 )
{
   // AS 06/28/00 - returns < 0 if a conflict, will return '1' in cases where desired access
   // is not available, but an upgraded open could be attempted (i.e. and then replace FILE4)
   /* verify that the desired access level is available in terms of the actual physical open */
   CODE4 *c4 = d4->c4 ;
   int canUpgrade = 0 ;

   #ifndef S4OFF_MULTI
      /* AS 08/21/97 Also disallow open if file is open in read-only level at low-level, but we want it to be open as read-write */

      #ifdef S4CLIENT
         if ( d4->readOnly == 1 )
      #else
         if ( d4->file.isReadOnly == 1 )
      #endif
         {
            #ifdef S4SERVER
               /* AS 02/02/99 - for server, check readOnlyRequest, not c4->readOnly */
               if ( c4->readOnlyRequest != 1 )
            #else
               if ( c4getReadOnly( c4 ) != 1 )
            #endif
               {
                  // AS 06/28/00 for ODBC need to sometimes open a different DATA4FILE
                  // becuase at least under ADO it is opened read-only first.
                  // for now at least this feature is not available in CodeBase client which is not used by ODBC.
                  #ifndef S4CLIENT
                     if ( c4->attemptUpgrade == 1 )
                        canUpgrade =  UPGRADE4READ_TO_READWRITE ;
                     else
                  #endif
                     return error4describe( c4, e4instance, E84307, dfile4name( d4 ), 0, 0 ) ;
               }
         }

      switch( c4->accessMode )
      {
         case OPEN4DENY_NONE:
            break ;
         case OPEN4DENY_RW:
            #ifdef S4CLIENT
               if ( d4->accessMode != OPEN4DENY_RW )
            #else
               if ( d4->file.lowAccessMode != OPEN4DENY_RW )
            #endif
                  return error4describe( c4, e4instance, E84307, dfile4name( d4 ), 0, 0 ) ;
            break ;
         case OPEN4DENY_WRITE:
            #ifdef S4CLIENT
               if ( d4->accessMode == OPEN4DENY_NONE )
            #else
               if ( d4->file.lowAccessMode == OPEN4DENY_NONE )
            #endif
                  return error4describe( c4, e4instance, E84307, dfile4name( d4 ), 0, 0 ) ;
            break ;
         default:
               return error4describe( c4, e4instance, E82502, dfile4name( d4 ), 0, 0 ) ;
      }
   #endif /* S4OFF_MULTI */

   #ifdef S4SERVER
      /* singleClient is the client's requested access mode */
      if ( d4 != 0 )
      {
         // AS 06/29/99 --> was using c4getReadOnly instead of readOnlyRequest...
         // if ( dfile4checkAccess( d4, c4->singleClient, c4getReadOnly( c4 ) ) == 0 )  /* access denied */
         if ( dfile4checkAccess( d4, c4->singleClient, c4->readOnlyRequest ) == 0 )  /* access denied */
           return error4describe( c4, e4instance, E91102, dfile4name( d4 ), 0, 0 ) ;
      }
   #endif

   return canUpgrade ;
}



#ifndef S4CLIENT
   #ifdef S4CLIPPER
      static int tag4attemptUpgrade( TAG4FILE *t4, DATA4FILE *d4 )
      {
         // recursive function that attempts to upgrade ourselves and if successful do the 'next' tag
         if ( t4 == 0 )  // done
            return 0 ;

         FILE4 tagFile ;

         // AS Apr 28/04 - use internal version
         int rc = file4openInternal( &tagFile, t4->codeBase, t4->file.name, 1, OPT4INDEX ) ;

         if ( rc != 0 )  // couldn't open
            return rc ;

         // try our neighbour
         rc = tag4attemptUpgrade( (TAG4FILE *)l4next( &d4->tagfiles, t4 ), d4 ) ;
         if ( rc != 0 )  // means failed, we need to close our file and return failure
         {
            file4close( &tagFile ) ;
            return rc ;
         }

         // on success we need to close original file and copy over the new open

         file4close( &t4->file ) ;
         c4memcpy( &t4->file, &tagFile, sizeof( FILE4 ) ) ;
         return 0 ;
      }
   #else
      static int index4attemptUpgrade( INDEX4FILE *i4 )
      {
         // recursive function that attempts to upgrade ourselves and if successful do the 'next' index
         if ( i4 == 0 )  // done
            return 0 ;

         FILE4 indexFile ;

         // AS May 24/02 - created file4openLow for internal use to indicate file types
         // AS Apr 28/04 - actually this is an index file
         int rc = file4openInternal( &indexFile, i4->codeBase, i4->file.name, 1, OPT4INDEX ) ;

         if ( rc != 0 )  // couldn't open
            return rc ;

         // try our neighbour
         rc = index4attemptUpgrade( (INDEX4FILE *)l4next( &i4->dataFile->indexes, i4 ) ) ;
         if ( rc != 0 )  // means failed, we need to close our file and return failure
         {
            file4close( &indexFile ) ;
            return rc ;
         }

         // on success we need to close original file and copy over the new open

         file4close( &i4->file ) ;
         c4memcpy( &i4->file, &indexFile, sizeof( FILE4 ) ) ;
         return 0 ;
      }
   #endif

   static int index4reopenAttemptUpgrade( DATA4FILE *d4 )
   {
      // This function assumes it is called by data4reopenAttemptUpgrade, and thus the c4->accessMode
      // values have already been set, we just need to try reopening the files are return the result.
      #ifdef S4CLIPPER
         // do each tag file
         return tag4attemptUpgrade( (TAG4FILE *)l4first( &d4->tagfiles ), d4 ) ;
      #else
         // do each index file
         return index4attemptUpgrade( (INDEX4FILE *)l4first( &d4->indexes ) ) ;
      #endif
   }


   static int data4reopenAttemptUpgrade( DATA4FILE *d4, int upgradeRequired )
   {
      // attempts to upgrade the openmode of a file.  Happens in ODBC when a user has a read-only handle open
      // and then wants to start modifying the contents while keeping the read-open open.  Solution is to
      // re-open the file at the low-level, and if it succeeds, close the previous instance.
      // returns 1 if cannot re-open in upgraded mode, 0 if success

      if ( upgradeRequired != UPGRADE4READ_TO_READWRITE )
         return 1 ;

      #ifdef S4OFF_MULTI
      // BCR 10/19/00 -- no lowAccessMode with OFF_MULTI
      // AS 10/19/00 - in off-multi should never get called because file should already
      // be opened in exclusive mode.  Just error out in debug mode, if it does get called
      // investigate further
         assert5( 0 ) ;
         return 1;
      #else
         // need to upgrade the open mode from read only to read-write
         CODE4 *c4 = d4->c4 ;
         int oldReadOnly = c4getReadOnly( c4 ) ;
         c4setReadOnly( c4, 0 ) ;
         int oldAccessMode = c4->accessMode ;
         c4->accessMode = (char)d4->file.lowAccessMode ;


         // upgrade data file
         FILE4 dataFile ;
         #ifndef S4OFF_MEMO
            FILE4 memoFile ;
         #endif
         // AS May 24/02 - created file4openLow for internal use to indicate file types
         int rc = file4openInternal( &dataFile, c4, d4->file.name, 1, OPT4DBF ) ;

         if ( rc != 0 )  // couldn't open
         {
            c4->accessMode = oldAccessMode ;
            c4setReadOnly( c4, oldReadOnly ) ;
            return 1 ;
         }

         #ifndef S4OFF_MEMO
            // AS 04/04/01 - Need to consider index and memo files as well...
            // upgrade memo file (if any)
            if ( d4->nFieldsMemo != 0 )
            {
               // AS May 24/02 - created file4openLow for internal use to indicate file types
               int rc = file4openInternal( &memoFile, c4, d4->memoFile.file.name, 1, OPT4OTHER ) ;

               if ( rc != 0 )  // couldn't open
               {
                  file4close( &dataFile ) ;
                  c4->accessMode = oldAccessMode ;
                  c4setReadOnly( c4, oldReadOnly ) ;
                  return 1 ;
               }
            }
         #endif

         // and do index files...
         rc = index4reopenAttemptUpgrade( d4 ) ;

         c4->accessMode = oldAccessMode ;
         c4setReadOnly( c4, oldReadOnly ) ;

         if ( rc != 0 )
         {
            file4close( &dataFile ) ;
            #ifndef S4OFF_MEMO
               file4close( &memoFile ) ;
            #endif
            return 1 ;
         }

         file4close( &d4->file ) ;
         c4memcpy( &d4->file, &dataFile, sizeof( FILE4 ) ) ;

         #ifndef S4OFF_MEMO
            if ( d4->nFieldsMemo != 0 )
            {
               file4close( &d4->memoFile.file ) ;
               c4memcpy( &d4->memoFile.file, &memoFile, sizeof( FILE4 ) ) ;
            }
         #endif

         return 0 ;
      #endif /* !OFF_MULTI */
   }
#endif /* !S4CLIENT */



static DATA4FILE *data4reopen( DATA4FILE *d4, char **info )
{
   if ( d4 == 0 )
      return 0 ;

   CODE4 *c4 = d4->c4 ;
   int rc = 0 ;

   #ifndef S4CLIENT
      #ifdef S4SERVER
         // AS May 16/03 - To fix a server-side closing issue - after create but before open it was possible that
         // the server would close the file, and if it was temporary it was being deleted.
         if ( d4->userCount == -1 )
            d4->userCount = 0 ;
      #endif
      if ( d4->userCount == 0 )
      {
         rc = data4reopenCheckIfMustClose( &d4 ) ;
         if ( rc != 0 )
            return 0 ;
      }
      else
      {
   #endif /* S4CLIENT */
         #ifndef S4SERVER
            rc = data4reopenVerifyOnlyOneInstance( d4 ) ;
            if ( rc != 0 )
               return 0 ;
         #endif
         #ifdef E4ANALYZE
            if ( d4->info == 0 )
            {
               error4describe( c4, e4struct, E91102, dfile4name( d4 ), 0, 0 ) ;
               return 0 ;
            }
         #endif

         rc = data4reopenVerifyDesiredAccessAvail( d4 ) ;
         #ifndef S4CLIENT
            if ( rc == 1 && c4->attemptUpgrade == 1 )
            {
               // AS 06/28/00 try to upgrade the open by opening the file in the new mode and then
               // replace existing FILE4...
               rc = data4reopenAttemptUpgrade( d4, rc ) ;
            }
         #endif
         if ( rc != 0 )
            return 0 ;
   #ifndef S4CLIENT
      }
   #endif

   if ( d4 != 0 )
   {
      #ifdef S4SERVER
         // AS May 16/03 - To fix a server-side closing issue - after create but before open it was possible that
         // the server would close the file, and if it was temporary it was being deleted.
         if ( d4->userCount == -1 )
            d4->userCount = 0 ;
      #endif
      d4->userCount++ ;
      *info = d4->info ;
      #ifdef E4ANALYZE
         if ( d4->nFields == 0 )
         {
            error4describe( c4, e4struct, E91102, dfile4name( d4 ), 0, 0 ) ;
            return 0 ;
         }
      #endif
      return d4 ;
   }

   return 0 ;
}



static int dfile4openVerifyInputs( CODE4 *c4, DATA4 *data, const char *name, char **info )
{
   #ifdef E4VBASIC
      if ( c4parm_check( c4, 1, E91102 ) )
         return -1 ;
   #endif

   #ifdef E4PARM_LOW
      if ( c4 == 0 || name == 0 || info == 0 )
         return error4( c4, e4parm_null, E91102 ) ;
      #ifdef S4CLIENT
         if ( data == 0 )
            return error4( c4, e4parm_null, E91102 ) ;
      #endif
   #endif

   if ( error4code( c4 ) < 0 )
      return -1 ;

   #ifdef E4ANALYZE
      if ( c4->debugInt != 0x5281 )
         return error4( 0, e4result, E81301 ) ;
   #endif

   #ifdef S4FOX
      if( !( c4->compatibility == 30 || c4->compatibility == 26 || c4->compatibility == 25 ) )  /* invalid setting */
         return error4( c4, e4open, E91102 ) ;
   #endif

   #ifdef S4CLIENT
      if ( strlen( name ) > LEN4PATH )
      {
         error4describe( c4, e4name, E84301, name, 0, 0 ) ;
         return 0 ;
      }
   #endif

   return 0 ;
}



static DATA4FILE *dfile4openGetData4ifOpen( CODE4 *c4, const char *name, char **info )
{
   DATA4FILE *d4file ;

   d4file = dfile4data( c4, name ) ;
   if ( d4file == 0 )
      return 0 ;

   return data4reopen( d4file, info ) ;
}



#ifdef S4CLIENT
   static CONNECTION4OPEN_INFO_OUT *dfile4openFromServer( CODE4 *c4, const char *name, DATA4 *data )
   {
      CONNECTION4OPEN_INFO_IN *dataIn ;
      int rc ;

      if ( !c4->defaultServer.connected )
      {
         rc = code4connect( c4, 0, DEF4PROCESS_ID, 0, 0, 0 ) ;
         if ( rc == 0 )
         {
            if ( !c4->defaultServer.connected )
            {
               error4describe( c4, e4connection, E84302, DEF4SERVER_ID, DEF4PROCESS_ID, 0 ) ;
               return 0 ;
            }
         }
         if ( rc != 0 )
         {
            if ( c4->defaultServer.connected )
            {
               connection4initUndo( &c4->defaultServer ) ;
               /* connection4free( c4->defaultServer ) ; */
               c4->defaultServer.connected = 0 ;
            }
            if ( error4code( c4 ) >= 0 )   /* probably r4connected, which should now become an error */
               error4describe( c4, e4connection, E81001, DEF4SERVER_ID, DEF4PROCESS_ID, 0 ) ;
            return 0 ;
         }
      }
      CONNECTION4 *connection = &c4->defaultServer ;
      if ( connection == 0 )
      {
         error4( c4, e4connection, E84303 ) ;
         return 0 ;
      }
      connection4assign( connection, CON4OPEN, data->trans->dataIdCount,0 ) ;
      data->trans->dataIdCount++ ;

      int len3 = strlen( name ) + 1 ;
      if ( len3 > LEN4PATH )
         len3 = LEN4PATH ;

      /* get the dataIn ptr, which is part of the to-send buffer */
      connection4addData( connection, NULL, sizeof( CONNECTION4OPEN_INFO_IN ), (void **)&dataIn ) ;
      /* memset( dataIn, 0, sizeof( CONNECTION4OPEN_INFO_IN ) ) ; */
      memcpy( dataIn->name, name, len3 ) ;
      dataIn->name[LEN4PATH] = 0 ;

      #ifdef S4OFF_MULTI
         dataIn->exclusiveClient = 1 ;
      #else
         dataIn->accessMode = htons5(c4->accessMode) ;
      #endif

      dataIn->readOnly = htons5( c4getReadOnly( c4 ) ) ;
      dataIn->errDefaultUnique = htons5( c4->errDefaultUnique ) ;
      dataIn->openForCreate = htons5( c4->openForCreate ) ;
      /* AS 07/21/99 - added for win 95/98 to avoid endless laze writes */
      dataIn->fileFlush = htons5( c4->fileFlush ) ;
      dataIn->singleOpen = htons5( c4->singleOpen ) ;
      dataIn->log = htons5( c4->log ) ;
      dataIn->compatibility = htons5( c4->compatibility ) ;

      connection4sendMessage( connection ) ;
      rc = connection4receiveMessage( connection ) ;
      if ( rc < 0 )
      {
         error4( c4, rc, E91102 ) ;
         return 0 ;
      }
      if ( connection4type( connection ) != CON4OPEN )
      {
         error4( c4, e4connection, E84304 ) ;
         return 0 ;
      }

      rc = connection4status( connection ) ;
      if ( rc < 0 )
      {
         if ( c4->errOpen == 0 )
            error4set( c4, r4noOpen ) ;
         else
            connection4errorDescribe( connection, c4, rc, E91102, name, 0, 0 ) ;
         return 0 ;
      }

      if ( connection4len( connection ) < sizeof( CONNECTION4OPEN_INFO_OUT ) )
      {
         error4( c4, e4connection, E84305 ) ;
         return 0 ;
      }

      return (CONNECTION4OPEN_INFO_OUT *)connection4data( connection ) ;
   }



   /* S4CLIENT */
   static int dfile4setupFromInfo( DATA4FILE *d4, const char *name, const CONNECTION4OPEN_INFO_OUT *dataInfo, DATA4 *data )
   {
      // returns < 0 if error, or fieldDataLen if no error

      CODE4 *c4 = d4->c4 ;
      assert5( c4 != 0 ) ;
      if ( error4code( c4 ) < 0 )
         return -1 ;

      if ( strlen( name ) > sizeof( d4->accessName ) )
         return error4describe( c4, e4name, E91102, name, 0, 0 ) ;
      strcpy( d4->accessName, name ) ;

      CONNECTION4 *connection = &c4->defaultServer ;

      d4->connection = connection ;
      // AS Jun 7/06 - recWidth is not large enough...in particular for large files...  also move to a better spot for byte alignment
      // d4->recWidth = ntohs5(dataInfo->recWidth) ;
      d4->recWidth = ntohl5(dataInfo->recWidth) ;
      d4->headerLen = ntohs5(dataInfo->headerLen) ;
      d4->version = dataInfo->version ;
      d4->serverId = ntohl5(dataInfo->serverId) ;
      // CJ July 17/01 - remember to byteswap readonly flag as many places compare to 1 instead of 0 and anything else.
      data->readOnly = ntohs5( dataInfo->readOnly ) ;
      // AS Oct 31/03 - Support for long field names
      d4->longFieldNamesSupported = (unsigned char)ntohs5( dataInfo->longFieldNameSupported ) ;

      // AS Oct 12/01 - We also need to store the readOnly flag on the DATA4FILE structure since it affects future
      // opens.
      d4->readOnly = data->readOnly ;

      // AS Jun 17/03 - Pass the CodePage as well, may be required for expression module on client
      d4->codePage = ntohs5( dataInfo->codePage ) ;
      d4->infoLen = ntohs5(dataInfo->infoLen) ;
      d4->info = (char *)u4allocFree( c4, d4->infoLen ) ;
      if ( d4->info == 0 )
         return e4memory ;
      int len2 = sizeof( CONNECTION4OPEN_INFO_OUT ) ;
      memcpy( d4->info, connection4data( connection ) + len2, d4->infoLen ) ;
      len2 += d4->infoLen ;

      #ifndef S4OFF_INDEX
         /* index file information... */
         if ( c4->autoOpen == 1 && ntohs5(dataInfo->numTags) > 0 )
         {
            char indexName[LEN4NAME_ALLOC_LEN] ;
            u4namePiece( indexName, sizeof(indexName), name, 1, 0 ) ;
            // AS Aug 29/06 - set error code for caller in this case...
            int rc = client4indexSetup( c4, data, d4, ntohs5(dataInfo->numTags), ntohs5(dataInfo->fullPathNameLen), connection4data( connection ) + len2, (unsigned int)connection4len( connection ) - len2, indexName, 0 ) ;
            if ( rc < 0 )
               return error4describe( c4, rc, E91102, name, 0, 0 ) ;
         }
      #endif

      return d4->infoLen ;
   }
#endif /* S4CLIENT */



#ifndef S4CLIENT
   static int dfile4setup( DATA4FILE *d4, char *name, unsigned long *recordLenFromHeader )
   {
      // returns < 0 if error, or fieldDataLen if no error
      #ifdef S4DATA_ALIGN  /* LY 2001/07/21 */
         double tempDbl ;
      #endif
      CODE4 *c4 = d4->c4 ;
      assert5( c4 != 0 ) ;
      if ( error4code( c4 ) < 0 )
         return -1 ;

      FILE4LONG pos ;
      /* LY July 7/03 : changed from 0 to 0L for Linux compiler */
      file4longAssign( pos, 0, 0L ) ;

      DATA4HEADER_FULL fullHeader ;
      if ( file4readAllInternal( &d4->file, pos, &fullHeader, sizeof( fullHeader ) ) < 0 )
         return -1 ;

      #ifdef S4BYTE_SWAP
         fullHeader.numRecs = x4reverseLong( (void *)&fullHeader.numRecs ) ;
         fullHeader.headerLen = x4reverseShort( (void *)&fullHeader.headerLen ) ;
         fullHeader.recordLen = x4reverseShort( (void *)&fullHeader.recordLen ) ;
         /* LY 2001/07/21 : wasn't swapping auto-increment */
         #ifdef S4DATA_ALIGN
            tempDbl = x4reverseDouble( (double*)fullHeader.autoIncrementVal ) ;
            memcpy( fullHeader.autoIncrementVal, (char*)&tempDbl, sizeof(double) ) ;
         #else
            fullHeader.autoIncrementVal = x4reverseDouble( (void*)&fullHeader.autoIncrementVal ) ;
         #endif
      #endif

      // AS Apr 19/06 - fix for large records...if it lands on a 64k buffer point, the stored recordLen will be 0...
      // so moved code later after we calculate the result
      #ifdef S4DEMO
         if ( fullHeader.numRecs > 200L)
            return error4( c4, e4demo, 0 ) ;
      #endif

      d4->hasMdxMemo = fullHeader.hasMdxMemo ;

      c4memcpy( (void *)&d4->version, (void *)&fullHeader.version, (4+(sizeof(S4LONG))+(sizeof(short))) ) ;
      #ifdef S4PREPROCESS_FILE
         // AS 05/24/02 for preprocess need to be able to recover this value to write on block boundaries on header
         d4->savedVersionNumber = d4->version ;
      #endif
      // AS 03/01/01 - new way of handling version -
      if ( d4->version == 0x31 )   // 0x31 means CodeBase specific extended - but internall treat this as 0x30...
      {
         // ensure we support all features
         // if more flags are found, it means that the given data file is not compatible with this version...
         char flags[8] ;
         c4memset( flags, 0, sizeof( flags )) ;
         if ( fullHeader.flags[0] == 1 )
            flags[0] = 1 ;   // means contains autoIncrement

         #ifndef S4MACINTOSH  // LY Jul 16/04
            #ifndef S4LUPACH /* LY July 7/03 */
               assert5port( "added compressed memo entries support" ) ;
               // AS June 19/02 - added support for memo field compression
               if ( fullHeader.flags[1] == 1 )
                  flags[1] = 1 ;   // means contains memo field compression

               assert5port( "added compressed memo entries support" ) ;
               // AS Nov 26/02 - added support for data file compression
               if ( fullHeader.flags[2] == 1 )
                  flags[2] = 1 ;   // means contains memo field compression
            #endif

            assert5port( "added autoTimestampField support" ) ;
            // AS Mar 11/03 - support for new feature r4autoTimestamp
            if ( fullHeader.flags[3] == 1 )
               flags[3] = 1 ;   // means contains autoTimestamp

            // AS Oct 27/03 - long field names support
            assert5port( "added long field name support" ) ;
            if ( fullHeader.flags[4] == 1 )
               flags[4] = 1 ;   // means contains long field names
         #endif

         assert5( sizeof( flags ) == sizeof( fullHeader.flags ) ) ;
         if ( c4memcmp( flags, fullHeader.flags, sizeof( flags ) ) != 0 )  // mismatch, we do not support the additional flags
            return error4describe( c4, e4data, E83817, name, dfile4name( d4 ), (char *)0 ) ;
         d4->version = 0x30 ;

         #if defined( S4FOX ) && !defined( S4CLIENT )
            if ( fullHeader.flags[0] == 1 )
               d4->autoIncrementSupported = 1 ;   // the data file has autoIncrement
            if ( fullHeader.flags[1] == 1 )
               d4->compressedMemosSupported = 1 ;   // the data file may contain compressed memos
            if ( fullHeader.flags[2] == 1 )
               d4->compressedDataSupported = 1 ;   // the data file is compressed
            if ( fullHeader.flags[3] == 1 )
               d4->autoTimestampSupported = 1 ;   // the data file contains an auto-timestamp field
            if ( fullHeader.flags[4] == 1 )
               d4->longFieldNamesSupported = 1 ;   // the data file contains long field names
         #endif
      }

      #ifdef S4FOX
         assert5( c4->compatibility == 30 || c4->compatibility == 26 || c4->compatibility == 25 ) ;
         if ( d4->version == 0x30 )
            d4->compatibility = 30 ;
         else
         {
            if ( c4->compatibility == 30 )
               d4->compatibility = 25 ;
            else
               d4->compatibility = c4->compatibility ;
         }

         // assert5( data != 0 ) ;
         d4->codePage = fullHeader.codePage ;
      #endif


      #ifdef S4MDX
         if ( d4->version == 4 )  // we don't support
            return error4( c4, e4notSupported, E86403 ) ;
      #endif

      if ( c4->largeFileOffset == 0 )
      {
         // AS Apr 19/06 - fix for large records...if it lands on a 64k buffer point, the stored recordLen will be 0...
         // AS 07/18/00 - avoid divide by zero exception... - was not catching this soon enough
         if ( fullHeader.recordLen == 0 )  /* divide by zero invalid, must be an invalid file */
            return error4describe( c4, e4data, E83805, name, dfile4name( d4 ), (char *)0 ) ;

         // AS 09/13/99 --> track # records before file becomes large...
         #if defined( S4FILE_EXTENDED )
            // AS 09/13/99 give a 10 record buffer as point where start checking, just as a pre-caution...
            d4->numRecsBeforeFileLong = ( ( ULONG_MAX - fullHeader.headerLen ) / fullHeader.recordLen ) - 10 ;
         #endif

         /* fullHeader.recordLen is not necessarily accurate with large files */
         FILE4LONG tLen = file4lenLow( &d4->file ) ;
         file4longSubtract( &tLen, fullHeader.headerLen ) ;
         file4longDivide( tLen, fullHeader.recordLen ) ;

         /* tLen now should be the same as #records in file */
         /* either error or > sizeof( long ), which we don't support (i.e. #recs > sizeof(long)), or just invalid */
         if ( fullHeader.numRecs == -1L )  /* invalid */
            return error4describe( c4, e4data, E83805, name, dfile4name( d4 ), (char *)0 ) ;
         if ( file4longGetHi( tLen ) != 0 )  /* since tLen == # recs in file, cannot be greater than 4 gigs so invalid if is... */
            return error4describe( c4, e4data, E83805, name, dfile4name( d4 ), (char *)0 ) ;

         long numRecordsBasedOnFileLength = (long)file4longGetLo( tLen ) ;

         /* if the file is opened deny write/exclusively, and this was the
            first open, then verify that the record count matches the file
            length (i.e. to avoid data file corruption) */
         if ( c4->accessMode == OPEN4DENY_WRITE || c4->accessMode == OPEN4DENY_RW )
         {
            // AS May 17/04 - client/server functionality to copmress the data file...
            #if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE )
               if ( d4->compressedDataSupported == 0 )
            #endif
                  if ( fullHeader.numRecs != numRecordsBasedOnFileLength )
                     return error4describe( c4, e4data, E83805, dfile4name( d4 ), 0, 0 ) ;  // CS 2003/01/22 Use only dfile4name() to prevent duplicate file name on error message
         }
         else
         {
            #ifdef E4MISC
               // AS 04/22/99 -- added to check only under E4MISC.  I believe that some operating
               // systems may not update the file length such that we get a proper read on it,
               // so don't bother in that case...

               // AS May 17/04 - client/server functionality to copmress the data file...
               #if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE )
                  if ( d4->compressedDataSupported == 0 )
               #endif
                     if ( fullHeader.numRecs > ( numRecordsBasedOnFileLength + 1 ) )  /* means # records off by more than 1 */
                     {
                        // not necessarily a problem - try locking the append bytes and re-checking...
                        // it is possible that we may lock ourselves out if our own process holds the lock,
                        // therefore just try once, if r4locked assume all ok...
                        #ifndef S4OFF_MULTI
                           int oldLockAttempts = c4->lockAttempts ;
                           c4->lockAttempts = 1 ;
                           int rc = dfile4lockAppend( d4, 1, 1 ) ;
                           c4->lockAttempts = oldLockAttempts ;
                           if ( rc == 0 )  // should be locked now, so retry, else just skip test...
                        #endif
                           {
                              // the # recs shouldn't have changed, (or at least not be smaller), so just
                              // re-examine the file length...
                              tLen = file4lenLow( &d4->file ) ;
                              file4longSubtract( &tLen, fullHeader.headerLen ) ;
                              file4longDivide( tLen, fullHeader.recordLen ) ;
                              #ifndef S4OFF_MULTI
                                 dfile4unlockAppend( d4, 1, 1 ) ;
                              #endif

                              if ( fullHeader.numRecs > ( numRecordsBasedOnFileLength + 1 ) )  /* means # records off by more than 1 */
                                 return error4describe( c4, e4data, E83805, name, dfile4name( d4 ), (char *)0 ) ;
                           }
                     }
            #endif /* E4MISC */
         }
      }

      // AS 09/21/00 - dBase 7 support - at least partially support...
      unsigned fieldDataLen ;
      FILE4LONG tLen ;
      /* LY July 7/03 : changed from 0 to 0L for Linux compiler */
      file4longAssign( tLen, sizeof( fullHeader ), 0L ) ;
      #ifdef S4MDX
         if ( d4->version == 4 )
         {
            // there are an extra 36 bytes of extra header info in the dBase 7 format...
            fieldDataLen = fullHeader.headerLen - sizeof(fullHeader) - 36 ;
            file4longAdd( &tLen, 36 ) ;
         }
         else
         {
            fieldDataLen = fullHeader.headerLen-sizeof(fullHeader) ;
         }
      #else
         fieldDataLen = fullHeader.headerLen-sizeof(fullHeader) ;
      #endif

      if ( fullHeader.headerLen <= sizeof(fullHeader) )
         return error4describe( c4, e4data, E83805, name, dfile4name( d4 ), (char *)0 ) ;

      d4->info = (char *)u4allocFree( c4, (long)fieldDataLen ) ;
      d4->infoLen = fieldDataLen ;
      d4->headerLen = fullHeader.headerLen ;
      d4->recordLen = fullHeader.recordLen ;
      #ifdef S4PREPROCESS_FILE
         // AS May 24/02 - Need these values to pad to 32 bytes (for block preprocessing)
         d4->preprocess_hasMdxMemo = fullHeader.hasMdxMemo ;
         d4->preprocess_codePage = fullHeader.codePage ;
         d4->preprocess_zero2[0] = fullHeader.zero2[0] ;
         d4->preprocess_zero2[1] = fullHeader.zero2[1] ;
      #endif
      c4memcpy( d4->flags, fullHeader.flags, 16 ) ;  // also copy over the flags and autoIncrement value
      if ( d4->info == 0 )
         return e4memory ;

      if ( file4readAllInternal( &d4->file, tLen, d4->info, fieldDataLen ) < 0 )
         return error4describe( c4, e4data, E84306, name, 0, 0 ) ;

      if ( error4code( c4 ) < 0 )
         return -1 ;

      #ifndef S4OFF_MEMO
         int hasMemo ;
         if ( d4->version == 0x30 )  /* visual FP 3.0 */
            hasMemo = fullHeader.hasMdxMemo & 0x02 ;
         else
            hasMemo = d4->version & 0x80 ;
         if ( hasMemo )
         {
            u4nameExt( name, LEN4NAME_ALLOC_LEN, MEMO4EXT, 1 ) ;
            if ( memo4fileOpen( &d4->memoFile, d4, name ) < 0 )
               return -1 ;
         }
      #endif

      *recordLenFromHeader = fullHeader.recordLen ;
      return fieldDataLen ;
   }
#endif /* !S4CLIENT */



static int dfile4setupFields( DATA4FILE *d4, const char *name, const unsigned long recordLenFromHeader )
{
   CODE4 *c4 = d4->c4 ;
   assert5( c4 != 0 ) ;
   if ( error4code( c4 ) < 0 )
      return -1 ;

   assert5( d4->nFieldsMemo == 0 ) ;

   int fieldHeaderSize = 32 ;
   #ifdef S4MDX
      // AS 09/21/00 - dbase 7 (version 4 files) have field info lengths of 48
      if ( d4->version == 4 )
         fieldHeaderSize = 48 ;
   #endif

   /* LY 2003/12/16 - Moved declaration of offset out of for-loop to avoid resetting it to 0 during loop */
   #ifdef S4CLIENT_OR_FOX
      // AS Oct 27/03 - long field names support
      long offset = 0 ;
   #endif
   for ( int fieldIndex = 0 ; fieldIndex < d4->nFields ; fieldIndex++ )
   {
      int imageLen ;
      int imageDec ;
      int imageType ;
      const char *imageName ;

      #ifdef S4MDX
         if ( d4->version == 4 )
         {
            const FIELD4IMAGE_MDX4 *image = (const FIELD4IMAGE_MDX4 *)(d4->info + fieldIndex * fieldHeaderSize ) ;
            imageLen = image->len ;
            imageDec = image->dec ;
            imageType = image->type ;
            imageName = image->name ;
         }
         else
         {
            const FIELD4IMAGE *image = (const FIELD4IMAGE *)(d4->info + fieldIndex * fieldHeaderSize ) ;
            imageLen = image->len ;
            imageDec = image->dec ;
            imageType = image->type ;
            imageName = image->name ;
         }
      #else
         const FIELD4IMAGE *image ;
         // AS Oct 27/03 - long field names support
         // AS Nov 24/03 - Fox only
         #ifdef S4CLIENT_OR_FOX
            if ( d4->longFieldNamesSupported == 1 )
            {
               image = (const FIELD4IMAGE *)(d4->info + offset - 11 ) ;
               // LY Jun 29/04 : changed from short *len to short len, added different
               // assignment method for S4DATA_ALIGN
               short len ;
               #ifdef S4DATA_ALIGN
                  // LY Jul 9/04 : cast &len to char* to avoid alignment error from numeric assembly operator in release mode
                  memcpy( (char *)&len, d4->info + offset + sizeof( FIELD4IMAGE ) - 11, sizeof( short ) ) ;
               #else
                  len = *((short *)(d4->info + offset + sizeof( FIELD4IMAGE ) - 11)) ;
               #endif
               imageName = (d4->info + offset + sizeof( FIELD4IMAGE ) - 11 + sizeof( short )) ;
               // LY JUn 29/04 : changed from *len to len
               offset += (sizeof( FIELD4IMAGE ) - 11 + len + sizeof( short )) ;
            }
            else
         #endif
         {
            image = (const FIELD4IMAGE *)(d4->info + fieldIndex * fieldHeaderSize ) ;
            imageName = image->name ;
         }
         imageLen = image->len ;
         imageDec = image->dec ;
         imageType = image->type ;
      #endif

      switch( imageType )
      {
         case r4memo:
         case r4gen:
            d4->nFieldsMemo++ ;
            #ifdef E4MISC
               if ( code4indexFormat( c4 ) == r4cdx )
               {
                  if ( imageLen != 4 && imageLen != 10 )
                     return error4describe( c4, e4data, E80501, name, dfile4name( d4 ), 0 ) ;
               }
               else
               {
                  if ( imageLen != 10 )
                     return error4describe( c4, e4data, E80501, name, dfile4name( d4 ), 0 ) ;
               }
            #endif
            // AS May 9/05 - the recWidth is obtained directly from the server in client mode
            #ifndef S4CLIENT
               d4->recWidth += imageLen ;
            #endif
            break ;
         case r4num:
         case r4float:
         case r4log:
         case r4date:
            // AS May 9/05 - the recWidth is obtained directly from the server in client mode
            #ifndef S4CLIENT
               d4->recWidth += imageLen ;
            #endif
            break ;
         case r5guid:
            // AS Apr 5/07 - VFP 9.0 adds this field type 'V' - for variable character / variable character (binary) field types
            // AS Nov 24/04 - use standard error handling here, not an assertion...esp. can happen due to encryption
            if ( s5fox && d4->version == 0x32 ) // vfp 9
            {
               // appears to be implemented as a static charcter field of some type
               // if ( imageLen != 10 )
               //    return error4describe( c4, e4data, E80501, name, dfile4name( d4 ), 0 ) ;
            }
            else
            {
               if ( imageLen != LEN5GUID )
                  return error4describe( c4, e4data, E80501, name, dfile4name( d4 ), 0 ) ;
            }
            // AS May 9/05 - the recWidth is obtained directly from the server in client mode
            #ifndef S4CLIENT
               d4->recWidth += imageLen ;
            #endif
            break ;
         case r5date:
            // AS Nov 24/04 - use standard error handling here, not an assertion...esp. can happen due to encryption
            if ( imageLen != sizeof( double ) )
               return error4describe( c4, e4data, E80501, name, dfile4name( d4 ), 0 ) ;
            // AS May 9/05 - the recWidth is obtained directly from the server in client mode
            #ifndef S4CLIENT
               d4->recWidth += imageLen ;
            #endif
            break ;
         case r5ui8:  // AS 01/02/00 - used for clipper -- specs change
         case r5i8:  // AS 01/04/00 - make generally avail for clipper...
            // AS Nov 24/04 - use standard error handling here, not an assertion...esp. can happen due to encryption
            if ( imageLen != 8 )
               return error4describe( c4, e4data, E80501, name, dfile4name( d4 ), 0 ) ;
            // AS May 9/05 - the recWidth is obtained directly from the server in client mode
            #ifndef S4CLIENT
               d4->recWidth += imageLen ;
            #endif
            break ;
         case r5ui4:
         case r4int:
            // AS Nov 24/04 - use standard error handling here, not an assertion...esp. can happen due to encryption
            if ( imageLen != 4 )
               return error4describe( c4, e4data, E80501, name, dfile4name( d4 ), 0 ) ;
            // AS May 9/05 - the recWidth is obtained directly from the server in client mode
            #ifndef S4CLIENT
               d4->recWidth += imageLen ;
            #endif
            break ;
         case r5ui2:
            // AS Nov 24/04 - use standard error handling here, not an assertion...esp. can happen due to encryption
            if ( imageLen != 2 )
               return error4describe( c4, e4data, E80501, name, dfile4name( d4 ), 0 ) ;
            // AS May 9/05 - the recWidth is obtained directly from the server in client mode
            #ifndef S4CLIENT
               d4->recWidth += imageLen ;
            #endif
            break ;
         case r5i2:
            // AS Apr 5/07 - VFP 9.0 adds a new type 'Q'...it is of type varbinary...should be the same as a memo perhaps?
               if ( s5fox && d4->version == 0x32 ) // vfp 9
               {
                  // appears to be implemented as a binary static charcter field of some type
                  // if ( imageLen != 10 )
                  //    return error4describe( c4, e4data, E80501, name, dfile4name( d4 ), 0 ) ;
               }
               else
               {
                  // AS Nov 24/04 - use standard error handling here, not an assertion...esp. can happen due to encryption
                  if ( imageLen != 2 )
                     return error4describe( c4, e4data, E80501, name, dfile4name( d4 ), 0 ) ;
               }
               // AS May 9/05 - the recWidth is obtained directly from the server in client mode
               #ifndef S4CLIENT
                  d4->recWidth += imageLen ;
               #endif
            break ;
         case r5wstr:
         case r5wstrLen:
         case r4str:
            // AS May 9/05 - the recWidth is obtained directly from the server in client mode
            #ifndef S4CLIENT
               // AS Jul 15/05 - replace old code to fix problem...
               d4->recWidth += ( imageLen + (imageDec << 8) ) ;
            #endif
            break ;
         case r4double:   /* r4bin and r4double the same */
            // AS Apr 5/07 - VFP 9.0 sets the version to 0x32, but it still supports these field types...use a > sign instead...
            // if ( s5fox && ( d4->version == 0x30 || c4->oledbSchemaCreate == 1 ) )
            if ( s5fox && ( d4->version >= 0x30 || c4->oledbSchemaCreate == 1 ) )
            {
               // AS May 9/05 - the recWidth is obtained directly from the server in client mode
               #ifndef S4CLIENT
                  d4->recWidth += imageLen ;
               #endif
            }
            else
            {
               if ( s5mdx )
               {
                  d4->nFieldsMemo++ ;
                  // AS May 9/05 - the recWidth is obtained directly from the server in client mode
                  #ifndef S4CLIENT
                     d4->recWidth += imageLen ;
                  #endif
               }
               else
               {
                  // AS Apr 5/07 - VFP 9.0 sets the version to 0x32, but it still supports these field types...use a > sign instead...
                  // if ( d4->version != 0x30 )  /* 2.5 data files disallowed these fields */
                  if ( d4->version < 0x30 )  /* 2.5 data files disallowed these fields */
                     return error4describe( c4, e4data, E80501, name, dfile4name( d4 ), 0 ) ;
               }
            }
            break ;
         #ifdef S4CLIENT_OR_FOX
            // AS Jul 21/05 - Support for new field type binary float
            case r4floatBin:
            case r4currency:
            case r4dateTime:
            case r4dateTimeMilli:  // AS Mar 10/03 - ms support in datetime
               // AS Apr 5/07 - VFP 9.0 sets the version to 0x32, but it still supports these field types...use a > sign instead...
               // if ( d4->version != 0x30 )  /* 2.5 data files disallowed these fields */
               if ( d4->version < 0x30 )  /* 2.5 data files disallowed these fields */
                  return error4describe( c4, e4data, E80501, name, dfile4name( d4 ), 0 ) ;
               // AS May 9/05 - the recWidth is obtained directly from the server in client mode
               #ifndef S4CLIENT
                  d4->recWidth += imageLen ;
               #endif
               break ;
            case r4system:  /* null-fields/system field */
               // AS Apr 5/07 - vfp 9, the version is 0x32 but should still be valid
               // if ( ( d4->version != 0x30 ) || ( c4memcmp( imageName, NULL4FLAGS_FIELD_NAME, 10 ) != 0 ) )  /* not visual FP 3.0 */
               if ( ( d4->version < 0x30 ) || ( c4memcmp( imageName, NULL4FLAGS_FIELD_NAME, 10 ) != 0 ) )  /* not visual FP 3.0 */
                  return error4describe( c4, e4data, E80501, name, dfile4name( d4 ), 0 ) ;
               // AS May 9/05 - the recWidth is obtained directly from the server in client mode
               #ifndef S4CLIENT
                  d4->recWidth += imageLen ;
               #endif
               break ;
         #endif
         default:
            #ifdef S4CLIENT_OR_FOX
               switch( imageType )
               {
                  case r4currency:
                  case r4dateTime:
                  case r4dateTimeMilli:  // AS Mar 10/03 - ms support in datetime
                     // AS May 9/05 - the recWidth is obtained directly from the server in client mode
                     #ifndef S4CLIENT
                        d4->recWidth += imageLen ;
                     #endif
                     break ;
                  case r5dbDate:
                  case r5dbTime:
                  case r5dbTimeStamp:
                     #ifdef S5USE_EXTENDED_TYPES
                        // AS May 9/05 - the recWidth is obtained directly from the server in client mode
                        #ifndef S4CLIENT
                           d4->recWidth += imageLen ;
                        #endif
                     #else
                        return error4describe( c4, e4fieldType, E80501, name, dfile4name( d4 ), 0 ) ;
                     #endif
                     break ;
                  default:
                     return error4describe( c4, e4fieldType, E80501, name, dfile4name( d4 ), 0 ) ;
               }
            #else
               return error4describe( c4, e4fieldType, E80501, name, dfile4name( d4 ), 0 ) ;
            #endif /* S4CLIENT_OR_FOX */
      }
   }

   #ifndef S4CLIENT
      // AS May 24/02 - allow to be different due to placing records on a boundary
      #ifdef S4PREPROCESS_FILE
         if ( d4->file.preprocessed == 1 )
         {
            short blockSize = code4getPreprocessBlockSize( c4 ) ;
            if ( blockSize > 1 )
            {
               // use the calculated method instead of the value stored in the data file because we may be
               // using large files in which case the header value is not large enough and is thus ignored.
               if ( ( d4->recWidth % blockSize ) != 0 )
                  d4->recWidth += (blockSize - ( d4->recWidth % blockSize ) ) ;
               // AS May 29/03 - Old encryption method sometimes has an an alternate boundary
               if ( c4->largeFileOffset == 0 )   /* for large files, allow large record widths - i.e. ignore the input recordLenFromHeader*/
                  if ( d4->recWidth != recordLenFromHeader )
                     d4->recWidth = recordLenFromHeader ;
            }
         }
      #endif
      if ( c4->largeFileOffset == 0 )   /* for large files, allow large record widths - i.e. ignore the input recordLenFromHeader*/
      {
         if ( d4->recWidth != recordLenFromHeader )
            return error4describe( c4, e4data, E91102, name, dfile4name( d4 ), 0 ) ;
      }
      // AS Apr 19/06 - for large files do check here...
      else
      {
         // AS Apr 19/06 - fix for large records...if it lands on a 64k buffer point, the stored recordLen will be 0...
         if ( d4->recWidth == 0 )  /* divide by zero invalid, must be an invalid file */
            return error4describe( c4, e4data, E83805, name, dfile4name( d4 ), (char *)0 ) ;

         #if defined( S4FILE_EXTENDED )
            d4->numRecsBeforeFileLong = ( ( ULONG_MAX - d4->headerLen ) / d4->recWidth ) - 10 ;
         #endif
      }
   #endif

   return 0 ;
}



static short dfile4openSetNumFields( DATA4FILE *d4, unsigned fieldDataLen )
{
   /* count the number of fields */
   unsigned int count ;

   int fieldHeaderSize = 32 ;
   #ifdef S4MDX
      // AS 09/21/00 - dbase 7 (version 4 files) have field info lengths of 48
      if ( d4->version == 4 )
         fieldHeaderSize = 48 ;
   #endif

   // AS Nov 24/03 - Long field name support only available in fox
   #ifdef S4CLIENT_OR_FOX
      // AS Oct 28/03 - support for long field names
      if ( d4->longFieldNamesSupported == 1 )
      {
         long offset = 0 ;

         for ( count = 0 ;; count++ )
         {
            // AS Jan 26/04 - for use with preprocessing, look for the 0x0D marker instead
            if ( (char)d4->info[offset] == 0xD || offset == (d4->infoLen - 2) )  // subtract 2 for the extra 2 bytes written at end of header
               break ;
            // LY Jul 9/04 : changed declaration of len from short*
            short len ;
            #ifdef S4DATA_ALIGN  /* LY 2003/12/16 */
               // LY Jul 9/04 : removed short* from d4->info... to avoid alignment error from numeric assembly operator in release mode
               memcpy( (char *)&len, d4->info + offset + sizeof( FIELD4IMAGE ) - 11, sizeof( short ) ) ;
            #else
               len = *(short *)(d4->info + offset + sizeof( FIELD4IMAGE ) - 11) ;
            #endif
            offset += (sizeof( FIELD4IMAGE ) - 11 + len + sizeof( short )) ;
            // AS Apr 5/04 - help to avoid problems further...
            if ( offset > d4->headerLen || offset < 0 ) // we went over somehow, maybe encrypted?
               return error4describe( d4->c4, e4data, E83805, dfile4name( d4 ), 0, 0 ) ;
         }
         d4->nFields = count ;
      }
      else
   #endif
   {
      for ( count = 0 ; d4->info[count] != 0xD ; count += fieldHeaderSize )
      {
         // AS 12/07/00 - relax this constraint and always check it...
         /* if count is > fieldDataLen, then somehow the 0xD got lost, so give error */
         if ( count > fieldDataLen )
            return error4describe( d4->c4, e4data, E83805, dfile4name( d4 ), 0, 0 ) ;
      }

      d4->nFields = (short)( count / fieldHeaderSize ) ;
   }

   #ifdef E4ANALYZE
      if ( d4->nFields == 0 )
         return error4describe( d4->c4, e4data, E84309, dfile4name( d4 ), 0, 0 ) ;
   #endif

   return 0 ;
}



// AS Nov 26/02 - Support for data file compression
// AS May 17/04 - client/server functionality to copmress the data file...
#if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
   static int d4compressInit( DATA4FILE *data )
   {
      // read in the compression info...
      if ( data->c4->openForCreate == 1 )  // we are still creating table, so nothing to initialize yet.
         return 0 ;
      // AS Jul 31/03 - new version 2 available for large files
      short version ;
      FILE4LONG pos ;
      file4longAssign( pos, data->headerLen, 0 ) ;
      if ( file4readAllInternal( &data->file, pos, &version, sizeof( short ) ) != 0 )
         return -1 ;
      data->compressInfo = (COMPRESS4HANDLER *)u4allocFree( data->c4, sizeof( COMPRESS4HANDLER ) ) ;
      if ( data->compressInfo == 0 )
         return -1 ;

      long sizeInfo ;
      // AS Apr 10/06 - fix variable not initialized
      unsigned long numOffsets = 0 ;

      switch( version )
      {
         case 1:
            // set up the short compress info structure
            file4longAdd( &pos, 2 * sizeof( short ) ) ;
            if ( file4readAllInternal( &data->file, pos, &numOffsets, sizeof( long ) ) != 0 )
               return -1 ;
            data->compressInfo->isLongOrCompress = 0 ;
            sizeInfo = sizeof( COMPRESS4DATA_SHORT ) + (numOffsets - 1) * sizeof( long ) ;  // -1 because sizeof() includes 1 entry
            data->compressInfo->shortCompress = (COMPRESS4DATA_SHORT *)u4allocFree( data->c4, sizeInfo ) ;
            file4longAssign( pos, data->headerLen, 0 ) ;
            if ( file4readAllInternal( &data->file, pos, data->compressInfo->shortCompress, sizeInfo ) != 0 )
               return -1 ;
            data->file.isReadOnly = 1 ;  // don't allow writing to compressed tables
            // AS Dec 5/05 - fix for OFF_MEMO
            #ifndef S4OFF_MEMO
               if ( data->memoFile.file.hand != 0 )
                  data->memoFile.file.isReadOnly = 1 ;
            #endif
            break ;
         case 2:
            // set up the long compress info structure
            file4longAdd( &pos, 2 * sizeof( short ) ) ;
            if ( file4readAllInternal( &data->file, pos, &numOffsets, sizeof( long ) ) != 0 )
               return -1 ;
            data->compressInfo->isLongOrCompress = 1 ;
            sizeInfo = sizeof( COMPRESS4DATA_LONG ) + (numOffsets - 1) * sizeof( FILE4LONG ) ;  // -1 because sizeof() includes 1 entry
            data->compressInfo->longCompress = (COMPRESS4DATA_LONG *)u4allocFree( data->c4, sizeInfo ) ;
            file4longAssign( pos, data->headerLen, 0 ) ;
            if ( file4readAllInternal( &data->file, pos, data->compressInfo->shortCompress, sizeInfo ) != 0 )
               return -1 ;
            data->file.isReadOnly = 1 ;  // don't allow writing to compressed tables
            // AS Dec 5/05 - fix for OFF_MEMO
            #ifndef S4OFF_MEMO
               if ( data->memoFile.file.hand != 0 )
                  data->memoFile.file.isReadOnly = 1 ;
            #endif
            break ;
         case 3:
            // a more complicated scenario, we need to count up all the available offsets by skipping through...
            {
               // the way we setup the structure is that we read the entire writeArray into a single entry (makes indexing very easy)
               // and then each writeBlock->writeArray points into the large array
               // set up the write compress info structure
               data->compressInfo->isLongOrCompress = 2 ;
               // read in the first struture first...
               COMPRESS4WRITE_BLOCK blockOn ;
               file4longAssign( pos, data->headerLen + sizeof( COMPRESS4WRITE_HEADER ) - sizeof(COMPRESS4WRITE_BLOCK), 0 ) ;
               if ( file4readAllInternal( &data->file, pos, &blockOn, sizeof( unsigned long ) + sizeof( FILE4LONG ) ) != 0 )
                  return -1 ;
               unsigned long firstOffsets = numOffsets = blockOn.numEntries ;  // AS Apr 21/07 - line got lost...

               while ( !file4longEqualZero( blockOn.nextBlock ) )
               {
                  if ( file4readAllInternal( &data->file, blockOn.nextBlock, &blockOn, sizeof( unsigned long ) + sizeof( FILE4LONG ) ) != 0 )
                     return -1 ;
                  numOffsets += blockOn.numEntries ;
               }

               data->compressInfo->writeCompress = (COMPRESS4WRITE_HEADER *)u4allocFree( data->c4, sizeof( COMPRESS4WRITE_HEADER ) ) ;
               if ( data->compressInfo->writeCompress == 0 )
                  return -1 ;
               file4longAssign( pos, data->headerLen, 0 ) ;
               if ( file4readAllInternal( &data->file, pos, data->compressInfo->writeCompress, sizeof( COMPRESS4WRITE_HEADER ) ) != 0 )
                  return -1 ;
               sizeInfo = (numOffsets) * sizeof( COMPRESS4WRITE_ARRAY_ENTRY ) ;  //
               data->compressInfo->writeCompress->firstArrayBlock.writeArray = (COMPRESS4WRITE_ARRAY_ENTRY *)u4allocFree( data->c4, sizeInfo ) ;
               if ( data->compressInfo->writeCompress->firstArrayBlock.writeArray == 0 )
                  return -1 ;

               // AS Jan 18/06 - generalize this code.  In particular in multi-user another user may add an entry to the array.  In
               // that case we need to read this array at run time.
               // now read the remaining arrays (if any)
               if ( file4compressInitArrayOffsets( &data->file, data->compressInfo->writeCompress, pos ) != 0 )
                  return -1 ;
            }
            break ;
         default:
            return -1 ;
            break ;
      }

      return file4compressInit( &data->file, data->compressInfo, data->headerLen ) ;
   }
#endif /* #if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS ) */



#ifdef P4ARGS_USED
   #pragma argsused
#endif
DATA4FILE *dfile4open( CODE4 *c4, DATA4 *data, const char *nameIn, char **info )
{
   unsigned fieldDataLen ;
   #ifdef S4CLIENT
      CONNECTION4OPEN_INFO_OUT *dataInfo ;
   #endif

   if ( dfile4openVerifyInputs( c4, data, nameIn, info ) < 0 )
      return 0 ;

   error4set( c4, 0 ) ;  // sometimes need to use '70' code for failed open

   S4CONST char *name ;  // CS 2000/06/06 S4CONST for S4CB51 compile
   #ifdef S4CLIENT
      name = nameIn ;
   #else
      char nameBuf[LEN4NAME_ALLOC_LEN] ;
      u4nameCurrent( nameBuf, sizeof( nameBuf ), nameIn ) ;
      u4nameExt( nameBuf, sizeof(nameBuf), DBF4EXT, 0 ) ;

      name = nameBuf ;
   #endif

   DATA4FILE *d4 = dfile4openGetData4ifOpen( c4, name, info ) ;
   if ( d4 != 0 )  // was already opoen, so just return back to caller */
      return d4 ;
   if ( error4code( c4 ) < 0 )
      return 0 ;

   #ifdef S4CLIENT
      dataInfo = dfile4openFromServer( c4, name, data ) ;
      if ( dataInfo == 0 )
         return 0 ;
   #endif  /* S4CLIENT */

   if ( c4->data4fileMemory == 0 )
   {
      c4->data4fileMemory = mem4create( c4, c4->memStartDataFile, sizeof(DATA4FILE), c4->memExpandDataFile, 0 ) ;
      if ( c4->data4fileMemory == 0 )
      {
         error4( c4, e4memory, E91102 ) ;
         return 0 ;
      }
   }
   d4 = (DATA4FILE *)mem4allocZero( c4->data4fileMemory ) ;
   if ( d4 == 0 )
   {
      error4( c4, e4memory, E91102 ) ;
      return 0 ;
   }

   #ifdef S4LOCK_HASH
      d4->lockHash = new Hash4lock() ;
   #endif

   d4->c4 = c4 ;
   d4->userCount = 1 ;

   #ifndef S4CLIENT
      #ifndef S4OFF_MEMO
         // LY 2003/07/31 #ifdef S4WIN64 /* LY 00/09/20 */
         //    d4->memoFile.file.hand = NULL ;
         // #else
            d4->memoFile.file.hand = INVALID4HANDLE ;
         // #endif
      #endif
      // AS May 24/02 - created file4openLow for internal use to indicate file types
      if ( file4openInternal( &d4->file, c4, name, 1, OPT4DBF ) != 0 )
      {
         dfile4close( d4 ) ;
         return 0 ;
      }
   #endif

   l4add( &c4->dataFileList, &d4->link ) ;
   unsigned long recordLenFromHeader = 0 ;

   #ifdef S4CLIENT
      fieldDataLen = dfile4setupFromInfo( d4, name, dataInfo, data ) ;
   #else
      fieldDataLen = dfile4setup( d4, nameBuf, &recordLenFromHeader ) ;
   #endif

   if ( error4code( c4 ) != 0 )  // 70 means r4open, means failed, or < 0 means failed - fielddataLen unsigned, can't use as error
   {
      dfile4close( d4 ) ;
      return 0 ;
   }

   d4->numRecs = -1L ;
   *info = d4->info ;

   if ( dfile4openSetNumFields( d4, fieldDataLen ) < 0 )
   {
      dfile4close( d4 ) ;
      return 0 ;
   }

   // AS May 9/05 - the recWidth in the client version is already transferred over in dfile4setup.  It may differ from the field
   // width from the field count due to encryption, so don't recalculate.
   d4->nFieldsMemo = 0 ;

   #ifdef S4CLIENT
      if ( dfile4setupFields( d4, name, recordLenFromHeader ) < 0 )
   #else
      d4->recWidth = 1 ;

      if ( dfile4setupFields( d4, nameBuf, recordLenFromHeader ) < 0 )
   #endif
   {
      dfile4close( d4 ) ;
      return 0 ;
   }

   #ifdef S4CLIENT
      d4->accessMode = c4->accessMode ;
   #else
      // AS 05/04/99 -- was previously marking d4->dataFile as valid too early, not getting properly closed...
      // d4->valid = 1 ;   /* valid, so low closes will leave open. */
   #endif

   // AS May 17/04 - client/server functionality to copmress the data file...
   #if !defined( S4CLIENT ) && defined( S4FOX ) && !defined( S4OFF_WRITE ) && defined( S4COMPRESS )
      if ( d4->compressedDataSupported == 1 )
         if ( d4compressInit( d4 ) < 0 )
         {
            dfile4close( d4 ) ;
            return 0 ;
         }
   #endif


   #ifndef S4OFF_OPTIMIZE
      file4optimizeLow( &d4->file, c4->optimize, OPT4DBF, d4->recWidth, d4 ) ;
   #endif

   // AS Dec 4/02 - moved validate code to point where table is actually marked as valid - to ensure invalid
   // tables don't get left with markers in place
   /* AS Sept 14/01 new functionality for validating the tables when CODE4 is initialized */
   // AS Oct 22/02 - make available to be called in multi-user (if user calls directly)
   // #if !defined( S4CLIENT )
   //    if ( c4->validationTable != 0 )
   //       code4validateAddOpen( c4, d4->file.name, file4getTemporary( &d4->file ) ) ;
   // #endif /* #if defined( S4OFF_MULTI ) && !defined( S4CLIENT ) */

   return d4 ;
}



#ifdef S4VB_DOS
DATA4 * d4open_v( CODE4 *c4, char *name )
{
   return d4open( c4, c4str(name) ) ;
}
#endif

