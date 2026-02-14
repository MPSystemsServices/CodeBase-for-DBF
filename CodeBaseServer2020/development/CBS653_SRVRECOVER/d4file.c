/* d4file.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifdef __TURBOC__
   #pragma hdrstop
#endif  /* __TUROBC__ */

#ifndef S4CLIENT
   /* fromDisk set to 1 if ensure that a disk read is done, instead of a buffer read */
   #ifdef P4ARGS_USED
      #pragma argsused
   #endif
   int dfile4read( DATA4FILE *data, long recNum, char *ptr, int fromDisk )
   {
      #ifdef S4CLIENT
         return dfile4goData( data, recNum, ptr, 1 ) ;
      #else
         unsigned len ;

         #ifdef E4PARM_LOW
            if ( data == 0 || recNum <= 0 || ptr == 0 )
               return error4( 0, e4parm_null, E91102 ) ;
         #endif

         if ( error4code( data->c4 ) < 0 )
            return e4codeBase ;

         #ifndef S4OPTIMIZE_OFF
            /* make sure read from disk unless file locked, etc. */
            if ( fromDisk )
               if ( data->file.doBuffer )
                  data->c4->opt.forceCurrent = 1 ;
         #endif
         len = file4readInternal( &data->file, dfile4recordPosition( data, recNum ), ptr, data->recWidth ) ;
         #ifndef S4OPTIMIZE_OFF
            if ( fromDisk )
               if ( data->file.doBuffer )
                  data->c4->opt.forceCurrent = 0 ;
         #endif

         if ( error4code( data->c4 ) < 0 )
            return error4code( data->c4 ) ;

         if ( len != data->recWidth )
            return r4entry ;

         return 0 ;
      #endif
   }
#endif  /* S4CLIENT */

/* set serverId to -2 to get the actual count if possible for example,
   b4leafInit needs to know how many may potentially exist */
#ifdef P4ARGS_USED
   #pragma argsused
#endif
long S4FUNCTION dfile4recCount( DATA4FILE *data, const long serverId )
{
   #ifdef S4CLIENT
      int rc ;
      CONNECTION4 *connection ;
      CONNECTION4RECCOUNT_INFO_OUT *info ;
   #else
      unsigned len ;
      FILE4LONG pos ;
   #endif
   S4LONG tmpCount ;

   #ifdef E4PARM_HIGH
      /* PARM_HIGH because called directly in S4OFF_MULTI case */
      if ( data == 0 )
         return error4( 0, e4parm_null, E91102 ) ;
   #endif

   if ( error4code( data->c4 ) < 0 )
      return e4codeBase ;

   #ifdef S4CLIENT
      /* client checks current count in d4recCount */
      connection = data->connection ;
      if ( connection == 0 )
         return e4connection ;

      connection4assign( connection, CON4RECCOUNT, 0, data->serverId ) ;
      connection4sendMessage( connection ) ;
      rc = connection4receiveMessage( connection ) ;
      if ( rc < 0 )
         return rc ;
      rc = connection4status( connection ) ;
      if ( rc != 0 )
         return connection4error( connection, data->c4, rc, E91102 ) ;

      if ( connection4len( connection ) != sizeof( CONNECTION4RECCOUNT_INFO_OUT ) )
         return error4( data->c4, e4packetLen, E91102 ) ;
      info = (CONNECTION4RECCOUNT_INFO_OUT *)connection4data( connection ) ;
      tmpCount = ntohl5(info->recCount) ;
      if ( tmpCount < 0 )
         return -1L ;
      data->minCount = tmpCount ;
      #ifndef S4SINGLE
         if ( info->appendLocked )
      #endif  /* S4SINGLE */
            data->numRecs = tmpCount ;
   #else
      if ( data->numRecs >= 0L )  /* we have an accurate count - append bytes locked */
      {
         #ifndef S4SINGLE
            if ( serverId == -2L )
               return data->numRecs ;
            /* minCount is used when transactions taking place - i.e. when the serverId matches the lock, it is accurate */
            // AS 6/17/98 should use the numRecs when locked, not vice versa - schema unsupported conformance test found this...
//            if ( dfile4lockTestFile( data, 0, serverId, lock4write ) || dfile4lockTestAppend( data, 0, serverId ) == 1 )
            if (!( dfile4lockTestFile( data, 0, serverId, lock4write ) || dfile4lockTestAppend( data, 0, serverId ) == 1 ))
               return dfile4getMinCount( data ) ;
            else
         #endif
               return data->numRecs ;
      }

      /* must do a read from disk because either no append lock held or we haven't read the value since the append lock was obtained */
      file4longAssign( pos, 4, 0 ) ;
      len = file4readInternal( &data->file, pos, &tmpCount, sizeof(S4LONG) ) ;
      #ifdef S4BYTE_SWAP
         tmpCount = x4reverseLong((void *)&tmpCount) ;
      #endif
      if ( tmpCount < 0L || len != sizeof( S4LONG ) )
         return -1L ;

      #ifndef S4SINGLE
         /* if anybody has an append lock, then the count is accurate
            since numRecs was < 0, nobody has appended anything yet, so
            we don't need to worry about transactions and minCount, just assign */
         if ( dfile4lockTestFile( data, 0, 0, lock4write ) == 1 || dfile4lockTestAppend( data, 0, 0 ) == 1 )
         {
            #ifdef S4CLIPPER
               //#ifdef E4ANALYZE
                  /* AS 03/15/99 - check the file length and make sure that # records matches that value as
                     well (i.e. verify via Clipper that flushed properly)  */
                  int lengthCount = (file4lenLow( &data->file ) - data->headerLen) / data->recWidth ;
                  if ( lengthCount > tmpCount )
                  {
                     tmpCount = lengthCount ;  // override using file length, clipper does not always update record count promptly
//                     return error4( data->c4, e4result, E91102 ) ;
                  }
               //#endif
            #endif
            data->numRecs = tmpCount ;
         }
      #else
         data->numRecs = tmpCount ;  /* as if locked... */
      #endif  /* S4SINGLE */

      /* this minCount is used as the actual record count in instances where
         transactions are taking place and the append bytes are locked, and
         data handles of other datafiles are performing the access */
      dfile4setMinCount( data, tmpCount ) ;    /* used for multi-user ensured sequencing */
   #endif

   return tmpCount ;
}



#ifndef S4INLINE
   unsigned long dfile4recordPosition( DATA4FILE *data, const long rec )
   {
      #ifdef E4PARM_LOW
         if ( data == 0 || rec <= 0L )
            return error4( 0, e4parm, E91102 ) ;
      #endif

      return (unsigned long)data->headerLen + (unsigned long)data->recWidth * ( rec - 1 ) ;
   }



   unsigned int dfile4recWidth( DATA4FILE *data )
   {
      #ifdef E4PARM_LOW
         if ( data == 0 )
            return error4( 0, e4parm_null, E91102 ) ;
      #endif

      return (unsigned int)data->recWidth ;
   }
#endif /* not S4INLINE */



S4CONST char *dfile4name( S4CONST DATA4FILE *data )
{
   #ifdef E4PARM_LOW
      if ( data == 0 )
      {
         error4( 0, e4parm_null, E93205 ) ;
         return 0 ;
      }
   #endif
   #ifdef S4CLIENT
      return data->accessName ;
   #else
      return data->file.name ;
   #endif
}



S4CONST char * S4FUNCTION d4fullPath( S4CONST DATA4 *d4 )
{
   // FOR ODBC - equivalent of dfile4name, but using DATA4 pointer
   return dfile4name( d4->dataFile ) ;
}



#if !defined(S4CLIENT) && !defined(S4OFF_WRITE) && !defined( S4OFF_MULTI )
   double dfile4getAutoIncrementValue( DATA4FILE *data )
   {
      // returns the current autoIncrementValue...
      FILE4LONG pos ;
      file4longAssign( pos, 20, 0 ) ;
      double val ;
      int len = file4readInternal( &data->file, pos, &val, sizeof( double ) ) ;
      if ( len != sizeof( double ) )
         return error4( data->c4, e4read, E91102 ) ;
      return val ;
   }
#endif  /* !S4OFF_WRITE && !S4CLIENT */



#if !defined(S4CLIENT) && !defined(S4OFF_WRITE)
   int dfile4updateHeader( DATA4FILE *data, int doTimeStamp, int doCount, Bool5 doAutoIncrement )
   {
      FILE4LONG pos ;
      int len ;   // AS 04/12/01 - changed from unsigned - it should be signed as it can be negative to indicate error

      #ifdef E4PARM_LOW
         if ( data == 0 )
            return error4( 0, e4parm_null, E91102 ) ;
      #endif

      if ( error4code( data->c4 ) < 0 )
         return e4codeBase ;

      if ( code4trans( data->c4 )->currentTranStatus == r4active || code4trans( data->c4 )->currentTranStatus == r4rollback )   /* delay to avoid append rollback problems */
         return 0 ;

      #ifdef E4ANALYZE
         #ifndef S4SINGLE
            #ifndef S4SERVER
               /* note that the server doesn't need it locked since that is a data level, not
                  a data4file level lock for server --> can't make this check */
               if  ( doCount )
                  if ( dfile4lockTestFile( data, 0, 0, lock4write ) == 0 && dfile4lockTestAppend( data, 0, 0 ) == 0 )
                     return error4( data->c4, e4info, E83201 ) ;
            #endif
         #endif
      #endif

      // AS 03/05/01 - don't write out the 'version' since this value is used internally different for auto-increment
      if ( doTimeStamp )
      {
         data->doDate = 1 ;
         file4longAssign( pos, 1, 0 ) ;
         len = 3 + ( sizeof(S4LONG) ) + ( sizeof( short ) ) ;
      }
      else
      {
         file4longAssign( pos, 4, 0 ) ;
         len = ( sizeof(S4LONG) ) + ( sizeof( short ) ) ;
      }

      // data->numRecs < 0 when we don't contain a lock on that area - also don't write
      // auto-increment value in that case...
      if ( !doCount || data->numRecs < 0 )
         len -= (sizeof( data->numRecs ) + sizeof( data->headerLen ) ) ;
      #if defined( S4FOX ) && !defined( S4CLIENT )
         else
         {
            if ( doAutoIncrement && data->autoIncrementSupported )  // update the auto-increment value...
            {
               // also write the updated autoIncrementVal, means indirectly also write the headerLen, recordLen, and flags
               len += 18 ;
            }
         }
      #endif

      #ifdef S4BYTE_SWAP
         data->numRecs = x4reverseLong( (void *)&data->numRecs ) ;
         data->headerLen = x4reverseShort( (void *)&data->headerLen ) ;
      #endif
      len = file4writeInternal( &data->file, pos, (char *)&data->version + file4longGetLo( pos ), len ) ;
      #ifdef S4BYTE_SWAP
         data->numRecs = x4reverseLong( (void *)&data->numRecs ) ;
         data->headerLen = x4reverseShort( (void *)&data->headerLen ) ;
      #endif

      if ( len < 0 )
         return -1 ;

      if ( doCount )
         dfile4setMinCount( data, data->numRecs ) ;

      data->fileChanged = 0 ;
      return 0 ;
   }
#endif  /* !S4OFF_WRITE && !S4CLIENT */



#ifndef S4CLIENT
   #ifndef S4WIN32
      FILE4LONG S4FUNCTION dfile4recordPosition( DATA4FILE *d4, long rec )
      {
         FILE4LONG val ;

         file4longAssign( val, (unsigned long)(d4)->recWidth, 0 ) ;
         file4longMultiply( val, ( (rec) - 1 ) ) ;
         file4longAdd( &val, (d4)->headerLen ) ;

         return val ;
      }
   #endif
#endif /* not S4CLIENT */
