/* l4log.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifdef S4SERVER
#ifdef S4DEBUG_LOG

int S4FUNCTION code4logInit( CODE4 *c4, const char *logName )
{
   int len, rc, oldExclusive ;

   #ifdef E4PARM_LOW
      if ( c4 == 0 || logName == 0 )
         return error4( c4, e4parm_null, E93401 ) ;
   #endif

   len = strlen( logName ) ;

   #ifdef E4PARM_LOW
      if ( len == 0 )
         return error4( c4, e4parm, E93401 ) ;
   #endif

   if ( c4->logFileName != 0 )
   {
      u4free( c4->logFileName ) ;
      c4->logFileName = 0 ;
   }

   c4->logFileName = (char *)u4allocFree( c4, len + 1 ) ;
   if ( c4->logFileName == 0 )
      return error4stack( c4, e4memory, E93401 ) ;
   memcpy( c4->logFileName, logName, len ) ;

   oldExclusive = c4->accessMode ;
   c4->accessMode = OPEN4DENY_RW ;
   rc = file4open( &c4->logFile, c4, c4->logFileName, 0 ) ;
   c4->accessMode = oldExclusive ;
   if ( rc < 0 )
      return error4stack( c4, e4memory, E93401 ) ;

   c4->log = 1 ;

   return 0 ;
}

int S4FUNCTION code4logInitUndo( CODE4 *c4 )
{
   int rc ;

   #ifdef E4PARM_LOW
      if ( c4 == 0 )
         return error4( c4, e4parm_null, E93402 ) ;
   #endif

   if ( c4->log )
   {
      rc = file4close( &c4->logFile ) ;
      if ( rc < 0 )
         return error4stack( c4, (short)rc, E93402 ) ;
      c4->log = 0 ;
   }


   if ( c4->logFileName != 0 )
   {
      u4free( c4->logFileName ) ;
      c4->logFileName = 0 ;
   }

   return 0 ;
}

int S4FUNCTION code4log( CODE4 *c4, const MESSAGE4 *message )
{
   int len, rc ;
   long pos ;

   #ifdef E4PARM_LOW
      if ( c4 == 0 || message == 0 )
         return error4( c4, e4parm_null, E93403 ) ;
   #endif

   len = message->len ;

   pos = file4len( &c4->logFile ) ;
   if ( pos < 0 )
      return error4stack( c4, (short)pos, E93403 ) ;
   rc = file4write( &c4->logFile, pos, &len, sizeof( len ) ) ;
   if ( rc < 0 )
      return error4stack( c4, (short)rc, E93403 ) ;
   pos = file4len( &c4->logFile ) ;
   if ( pos < 0 )
      return error4stack( c4, (short)pos, E93403 ) ;
   rc = file4write( &c4->logFile, pos, message->packet, len ) ;
   if ( rc < 0 )
      return error4stack( c4, (short)rc, E93403 ) ;

   return 0 ;
}

#endif /* S4DEBUG_LOG */
#endif /* S4SERVER */
