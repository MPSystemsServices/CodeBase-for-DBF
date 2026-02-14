/* d4data.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifdef __TURBOC__
   #pragma hdrstop
#endif  /* __TUROBC__ */

#ifndef S4INLINE
   #ifdef S4STAND_ALONE
      unsigned long S4FUNCTION data4serverId( DATA4 *d4 )
      {
         return d4->clientId ;
      }



      unsigned long data4clientId( DATA4 *d4 )
      {
         return d4->clientId ;
      }
   #endif



   #ifdef S4SERVER
      unsigned long S4FUNCTION data4serverId( DATA4 *d4 )
      {
         return d4->serverId ;
      }



      unsigned long data4clientId( DATA4 *d4 )
      {
         return d4->clientId ;
      }
   #endif



   #ifdef S4CLIENT
      unsigned long S4FUNCTION data4serverId( DATA4 *d4 )
      {
         return d4->dataFile->serverId ;
      }



      unsigned long data4clientId( DATA4 *d4 )
      {
         return d4->clientId ;
      }
   #endif
#endif  /* S4INLINE */



S4CONST char *S4FUNCTION d4alias( S4CONST DATA4 *data )
{
   #ifdef E4VBASIC
      if ( c4parm_check( data, 2, E93301 ) )
         return (char *) NULL;
   #endif  /* E4VBASIC */

   #ifdef E4PARM_HIGH
      if ( data == 0 )
      {
         error4( 0, e4parm_null, E93301 ) ;
         return 0 ;
      }
   #endif
   return data->alias ;
}



/* Search the DATA4.alias string for spaces, and replace with underscores */
void d4aliasFix( DATA4 S4PTR *data )
{
   int i;
   #ifdef E4PARM_HIGH
      if ( data == 0 )
      {
         error4( 0, e4parmNull, E93302 ) ;
         return ;
      }
   #endif

   for ( i = 0 ; i < sizeof( data->alias ) ; i++ )
      if ( (data->alias)[i] == ' ' )
         (data->alias)[i] = '_' ;
}



void S4FUNCTION d4aliasSet( DATA4 *data, const char * newAlias )
{
   #ifdef E4PARM_HIGH
      if ( data == 0 || newAlias == 0 )
      {
         error4( 0, e4parm_null, E93302 ) ;
         return ;
      }
   #endif

   #ifdef E4VBASIC
      if ( c4parm_check( data, 2, E93302 ) )
         return ;
   #endif  /* E4VBASIC */

   if ( u4namecmp( data->alias, newAlias, data->codeBase->ignoreCase ) )
   {
      u4ncpy( data->alias, newAlias, sizeof( data->alias ) ) ;
      d4aliasFix( data ) ;

      #ifdef S4CLIENT
         data->aliasSet = 1 ;
      #endif
   }

   return ;
}



void S4FUNCTION d4blankLow( DATA4 *data, char *record )
{
   assert5( record != 0 && data != 0 ) ;

   if ( data->recordBlank != 0 )
      c4memcpy( record, data->recordBlank, (unsigned int)dfile4recWidth( data->dataFile ) ) ;
   else
      c4memset( record, ' ', (unsigned int)dfile4recWidth( data->dataFile ) ) ;

   #ifndef S4OFF_MEMO
      // AS 06/09/00 was not compiling in S4OFF_MULTI
      #ifndef S4OFF_MULTI
         // AS 02/24/00 was not marking memo ids as valid, thus sometimes was re-reading these in causing problems.
         data->memoValidated = 1 ;
      #endif

      if ( data->fieldsMemo != 0 )
      {
         for ( int memoIndex = 0 ; memoIndex < data->dataFile->nFieldsMemo ; memoIndex++ )
         {
            f4memoFree( data->fieldsMemo[memoIndex].field ) ;
         }
      }
   #endif /* !S4OFF_MEMO */

   return ;
}



void S4FUNCTION d4blank( DATA4 *data )
{
   #ifdef E4VBASIC
      if ( c4parm_check( data, 2, E93303 ) )
         return ;
   #endif  /* E4VBASIC */

   #ifdef E4PARM_HIGH
      if ( data == 0 )
      {
         error4( 0, e4parm_null, E93303 ) ;
         return ;
      }
   #endif

   #ifndef S4SERVER
      #ifndef S4OFF_ENFORCE_LOCK
         if ( data->codeBase->lockEnforce && data->recNum > 0L )
            if ( d4lockTest( data, data->recNum ) != 1 )
            {
               error4( data->codeBase, e4lock, E93303 ) ;
               return ;
            }
      #endif
   #endif

   d4blankLow( data, data->record ) ;

   data->recordChanged = 1 ;

   return ;
}



int S4FUNCTION d4bof( DATA4 *data )
{
   #ifdef E4VBASIC
      if ( c4parm_check( data, 2, E93304 ) )
         return -1;
   #endif  /* E4VBASIC */

   #ifdef E4PARM_HIGH
      if ( data == 0 )
         return error4( 0, e4parm_null, E93304 ) ;
   #endif

   if ( error4code( data->codeBase ) < 0 )
      return e4codeBase ;

   return data->bofFlag ;
}



#ifndef S4OFF_WRITE
   void S4FUNCTION d4delete( DATA4 *data )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E93305 ) )
            return ;
      #endif  /* E4VBASIC */

      #ifdef E4PARM_HIGH
         if ( data == 0 )
         {
            error4( 0, e4parm_null, E93305 ) ;
            return ;
         }
      #endif
      #ifdef E4MISC
         if ( data->record[0] != ' ' && data->record[0] != '*' )
         {
            error4( data->codeBase, e4info, E83301 ) ;
            return ;
         }
      #endif

      if ( data->record[0] != '*' )
      {
         #ifndef S4SERVER
            #ifndef S4OFF_ENFORCE_LOCK
               if ( data->codeBase->lockEnforce && data->recNum > 0L )
                  if ( d4lockTest( data, data->recNum ) != 1 )
                  {
                     error4( data->codeBase, e4lock, E93305 ) ;
                     return ;
                  }
            #endif
         #endif
           data->record[0] = '*' ;
         data->recordChanged = 1 ;
      }

      return ;
   }
#endif



#if defined( S4OLEDB_OR_NOT_SERVER ) || defined( S4ODBC_BUILD )
   int S4FUNCTION d4deleted( DATA4 *data )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E93306 ) )
            return -1;
      #endif  /* E4VBASIC */
      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E93306 ) ;
      #endif
      #ifdef E4MISC
         if ( data->record[0] != ' ' && data->record[0] != '*' )
            return error4( data->codeBase, e4info, E83301 ) ;
      #endif

      return *data->record != ' ' ;
   }
#endif /* S4OLEDB_OR_NOT_SERVER */



int S4FUNCTION d4eof( DATA4 *data )
{
   #ifdef E4VBASIC
      if ( c4parm_check( data, 2, E93307 ) )
         return -1;
   #endif  /* E4VBASIC */

   #ifdef E4PARM_HIGH
      if ( data == 0 )
         return error4( 0, e4parm_null, E93307 ) ;
   #endif

   if ( error4code( data->codeBase ) < 0 )
      return e4codeBase ;

   return data->eofFlag ;
}



const char *S4FUNCTION d4fileName( DATA4 *data )
{
   #ifdef S4CLIENT
      CONNECTION4 *connection ;
      int rc ;
   #endif

   #ifdef E4PARM_HIGH
      if ( data == 0 )
      {
         error4( 0, e4parm_null, E93205 ) ;
         return 0 ;
      }
   #endif

   #ifdef E4VBASIC
      if ( c4parm_check( data, 2, E40139 ) )
         return 0 ;
   #endif

   #ifdef S4CLIENT
      if ( error4code( data->codeBase ) < 0 )
         return 0 ;

      connection = data->dataFile->connection ;
      connection4assign( connection, CON4DATA_FNAME, data4clientId( data ), data4serverId( data ) ) ;
      connection4sendMessage( connection ) ;
      rc = connection4receiveMessage( connection ) ;
      if ( rc < 0 )
      {
         #ifdef E4STACK
            error4stack( data->codeBase, rc, E95501 ) ;
         #endif
         return 0 ;
      }

      rc = connection4status( connection ) ;
      if ( rc < 0 )
      {
         connection4error( connection, data->codeBase, rc, E95501 ) ;
         return 0 ;
      }

      return connection4data( connection ) ;
   #else
      return data->dataFile->file.name ;
   #endif
}



short S4FUNCTION d4logStatusCB( DATA4 *data )
{
   if (!data)
      return 0 ;
   return d4logStatus(data) ;
}



#if !defined( S4OFF_TRAN ) && defined( S4STAND_ALONE ) && !defined( S4OFF_WRITE )
   int S4FUNCTION d4log( DATA4 *data, const int logging )
   {
      /* can change LOG4ON-->LOG4TRANS or LOG4TRANS-->LOG4ON, also new support for LOG4OFF for internal
         (i.e. schema) tables which we do not ever want to log.
      */
      int oldVal ;

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E93319 ) ;
      #endif

      /* AS 12/10/98 added support for hard-offing schema tables.  This overrides everything to
         disallow logging on these files.
      */

      oldVal = ( data->logVal == 0 ) ? 0 : 1 ;

      if ( logging == LOG4OFF )
      {
         data->logVal = LOG4OFF ;
         return oldVal ;
      }

      if ( code4transEnabled( data->codeBase ) == 0 )
      {
         if ( logging == -1 )
            return r4logOff ;
         else
            return error4( data->codeBase, e4trans, E83807 ) ;
      }

      if ( data->logVal == LOG4ALWAYS )
         return r4logOn ;

      if ( logging != -1 )
      {
         if ( logging )
         {
            if ( data->logVal == LOG4TRANS )
               data->logVal = LOG4ON ;
         }
         else
         {
            if ( data->logVal == LOG4ON )
               data->logVal = LOG4TRANS ;
         }
      }

      return oldVal ;
   }
#endif /* #if !defined( S4OFF_TRAN ) && defined( S4STAND_ALONE ) && !defined( S4OFF_WRITE ) */



#ifndef S4SERVER
   short int S4FUNCTION d4numFields( DATA4 *data )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E93308 ) )
            return -1;
      #endif  /* E4VBASIC */

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E93308 ) ;
      #endif

      #ifdef S4CLIENT_OR_FOX
         if ( data->fields[data->dataFile->nFields-1].type == '0' )   /* null flags field */
            return data->dataFile->nFields - 1 ;
      #endif

      return data->dataFile->nFields ;
   }
#endif



#ifndef S4CLIENT
   /*  currently this function is not used...
   int d4read( DATA4 *data, const long recNum, char *ptr )
   {
      #ifdef E4PARM_LOW
         if ( data == 0 || recNum <= 0 || ptr == 0 )
            return error4( 0, e4parm, E93309 ) ;
      #endif

      return dfile4read( data->dataFile, recNum, ptr, 0 ) ;
   }
   */



   int d4readOld( DATA4 *data, const long recNum )
   {
      int rc ;

      #ifdef E4PARM_LOW
         if ( data == 0 || recNum <= 0 )
            return error4( 0, e4parm, E93310 ) ;
      #endif

      if ( error4code( data->codeBase ) < 0 )
         return e4codeBase ;

      if ( recNum <= 0 )
      {
         data->recNumOld = recNum ;
         d4blankLow( data, data->recordOld ) ;
      }

      if ( data->recNumOld == recNum )
         return 0 ;

      data->recNumOld = -1 ;
      // AS 05/20/99 --> don't actually want to read 'from disk' in this case.  We just want
      // the old record, whatever it may be (if bufferred internal, that is ok) - t5transmm.cpp
      // AS 05/20/99 --> actually, we do want to read from disk.  The conditional is that it
      // flushes changes to disk first.  Basically, we want to read the 'latest' that anybody
      // else has done (don't use our 'read' buffer).  If we have it write bufferred, must ensure
      // that we get that one...
      rc = dfile4read( data->dataFile, recNum, data->recordOld, 1 ) ;
      if ( rc < 0 )
         return -1 ;
      if ( rc > 0 )
         d4blankLow( data, data->recordOld ) ;
      data->recNumOld = recNum ;

      return 0 ;
   }
#endif /* !S4CLIENT */



#if !defined( S4SERVER ) && !defined( S4OFF_WRITE )
   void S4FUNCTION d4recall( DATA4 *data )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E93311 ) )
            return ;
      #endif  /* E4VBASIC */

      #ifdef E4PARM_HIGH
         if ( data == 0 )
         {
            error4( 0, e4parm_null, E93311 ) ;
            return ;
         }
      #endif
      #ifdef E4MISC
         if ( data->record[0] != ' ' && data->record[0] != '*' )
         {
            error4( data->codeBase, e4info, E83301 ) ;
            return ;
         }
      #endif

      if ( *data->record != ' ' )
      {
         #ifndef S4SERVER
            #ifndef S4OFF_ENFORCE_LOCK
               if ( data->codeBase->lockEnforce && data->recNum > 0L )
                  if ( d4lockTest( data, data->recNum, lock4write ) != 1 )
                  {
                     error4( data->codeBase, e4lock, E93311 ) ;
                     return ;
                  }
            #endif
         #endif
         *data->record = ' ' ;
         data->recordChanged = 1 ;
      }

      return ;
   }
#endif /* #if !defined( S4SERVER ) && !defined( S4OFF_WRITE ) */



int S4FUNCTION d4recCountLessEq( DATA4 *data, long count )
{
   /* returns true if the input count is <= record count */

   long count2 = data->count ;
   if ( count2 > dfile4getMinCount( data->dataFile ) ) /* transaction rollback possibility */
      count2 = dfile4getMinCount( data->dataFile ) ;
   if ( count <= count2 )
      return 1 ;
   data->count = d4recCount( data ) ;
   if ( data->count < 0 )
      return error4( data->codeBase, (short int)data->count, E93318 ) ;
   if ( count <= data->count )
      return 1 ;
   return 0 ;
}



int S4FUNCTION d4recCountLess( DATA4 *data, long count )
{
   /* returns true if the input count is < record count */

   long count2 = data->count ;
   if ( count2 > dfile4getMinCount( data->dataFile ) ) /* transaction rollback possibility */
      count2 = dfile4getMinCount( data->dataFile ) ;
   if ( count < count2 )
      return 1 ;
   data->count = d4recCount( data ) ;
   if ( data->count < 0 )
      return error4( data->codeBase, (short int)data->count, E93318 ) ;
   if ( count < data->count )
      return 1 ;
   return 0 ;
}



int S4FUNCTION d4recCountGreater( DATA4 *data, long count )
{
   /* returns true if the input count is > record count */

   long count2 = data->count ;
   if ( count2 > dfile4getMinCount( data->dataFile ) ) /* transaction rollback possibility */
      count2 = dfile4getMinCount( data->dataFile ) ;
   if ( count <= count2 )
      return 0 ;
   data->count = d4recCount( data ) ;
   if ( data->count < 0 )
      return error4( data->codeBase, (short int)data->count, E93318 ) ;
   if ( count > data->count )
      return 1 ;
   return 0 ;
}



#ifndef S4OFF_MULTI
   long S4FUNCTION d4recCountDo( DATA4 *data )
   {
      // AS d4recCountDo is exporeted, cannot modify, but call new d4recCountDo2 instead
      return d4recCountDo2( data, 0 ) ;
   }



   long S4FUNCTION d4recCountDo2( DATA4 *data, Bool5 assumeLocked )
   {
      /* note that S4OFF_MULTI version just calls dfile4recCount since checking of
         locks and serverId are not required */

      // AS 09/15/99 -- added assumeLocked flag for when called during append, where
      // we wouldn't be at this point if the append bytes were not locked.  This improves
      // performance, esp. OLE-DB by as much as 1% or more in cases where performance
      // is critical.
      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E93312 ) )
            return -1L ;
      #endif  /* E4VBASIC */

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E93312 ) ;
      #endif

      if ( data->dataFile->numRecs >= 0L )
      {
         #ifndef S4OFF_MULTI
            if ( assumeLocked == !ASSUME4LOCKED && d4lockTestAppend( data ) != 1 )
               return dfile4getMinCount( data->dataFile ) ;
         #endif

         return data->dataFile->numRecs ;
      }

      return dfile4recCount( data->dataFile, data4serverId( data ) ) ;
   }
#endif



#ifndef S4SERVER
   long S4FUNCTION d4recNoLow( DATA4 *data )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( data, 2, E93313 ) )
            return -1L;
      #endif  /* E4VBASIC */

      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E93313 ) ;
      #endif

      return data->recNum ;
   }


   char *S4FUNCTION d4recordLow( DATA4 *data )
   {
      #ifdef E4PARM_HIGH
         if ( data == 0 )
         {
            error4( 0, e4parm_null, E93314 ) ;
            return 0 ;
         }
      #endif

      return data->record ;
   }
#endif /* !S4SERVER */



#ifdef S4STAND_ALONE
   int S4FUNCTION d4lowAccessMode( DATA4 *d4 )
   {
      // for odbc
      // AS 06/09/00 was not compiling in S4OFF_MULTI
      #ifdef S4OFF_MULTI
         return OPEN4DENY_RW ;
      #else
         return d4->dataFile->file.lowAccessMode ;
      #endif
   }



   void S4FUNCTION d4recNoSet( DATA4 *d4, long recNo )
   {
      // for odbc
      d4->recNum = recNo ;
   }



   void S4FUNCTION d4recordSet( DATA4 *data, char *newRecPtr )
   {
      // used by ODBC
      #ifdef E4PARM_LOW
         if ( data == 0 || newRecPtr == 0 )
         {
            error4( 0, e4parm_null, E93314 ) ;
            return ;
         }
      #endif

      data->record = newRecPtr ;
   }
#endif /* S4STAND_ALONE */



unsigned long S4FUNCTION d4recWidthLow( DATA4 *data )
{
   #ifdef E4PARM_HIGH
      if ( data == 0 )
      {
         error4( 0, e4parm_null, E93316 ) ;
         return 0 ;
      }
   #endif

   return dfile4recWidth( data->dataFile ) ;
}



#ifdef P4ARGS_USED
   #pragma argsused
#endif
int d4verify( DATA4 *d4, const int subs )
{
   #ifndef S4OFF_WRITE
      #ifdef E4ANALYZE
         int rc ;
      #endif
   #endif

   if ( d4 == 0 )
      return error4( d4->codeBase, e4parm_null, E93317 ) ;

   if ( d4->trans == 0 )
      return error4( 0, e4struct, E93317 ) ;

   #ifndef S4OFF_WRITE
      #ifdef E4ANALYZE
         if ( subs == 1 )
            if ( ( rc = tran4verify( d4->trans, 1 ) ) < 0 )
               return rc ;
      #endif
   #endif

   if ( d4->link.n == 0 || d4->link.p == 0 )
      return error4( d4->codeBase, e4struct, E93317 ) ;

   #ifndef S4CLIENT
      if ( d4->dataFile == 0 )
         return error4( d4->codeBase, e4struct, E93317 ) ;

      #ifdef E4ANALYZE
         if ( subs == 1 )
            return dfile4verify( d4->dataFile, 0 ) ;
      #endif
   #endif

   return 0 ;
}


/* LY 00/11/16 : exporting function due to failure of Microsoft IA-64 compiler */
#if defined( S4FILE_EXTENDED ) && !defined( S464BIT ) && !defined( S4UNIX ) && (!defined(__cplusplus) || defined(S4WIN64))
   FILE4LONG S4FUNCTION file4longCoerce( LONGLONG val )
   {
      // assert5( sizeof( val ) == sizeof( FILE4LONG ) ) ;
      return *(((FILE4LONG *)(&val))) ;
   }
#endif



#if !defined( S4SERVER ) && !defined( S4ODBC_BUILD )
   short S4FUNCTION d4codePage( DATA4 *data )
   {
      #ifdef E4PARM_HIGH
         if ( data == 0 )
            return error4( 0, e4parm_null, E93316 ) ;
      #endif

      if ( error4code( data->codeBase ) < 0 )
         return e4codeBase ;

      #ifdef S4CLIENT
         if ( data->dataFile->codePageRead != 1 )
         {
            CONNECTION4 *connection = data->dataFile->connection ;
            connection4assign( connection, CON4DATA_CODEPAGE, data4clientId( data ), data4serverId( data ) ) ;
            connection4sendMessage( connection ) ;
            int rc = connection4receiveMessage( connection ) ;
            if ( rc < 0 )
               return rc ;

            rc = connection4status( connection ) ;
            if ( rc < 0 )
            {
               connection4error( connection, data->codeBase, rc, E95501 ) ;
               return rc ;
            }

            data->dataFile->codePage = rc ;
            data->dataFile->codePageRead = 1 ;
         }
      #endif

      return data->dataFile->codePage ;
   }
#endif



#ifdef S4VB_DOS
   char * d4alias_v( DATA4 *d4 )
   {
      return v4str( d4alias(d4) ) ;
   }



   void d4aliasSet( DATA4 *d4, const char *alias )
   {
      d4aliasSet( d4, c4str(alias) ) ;
   }
#endif


#ifdef S4FOX
   int S4FUNCTION d4compatibilityExport( DATA4 *d4 )
   {
      return d4->dataFile->compatibility ;
   }
#endif
