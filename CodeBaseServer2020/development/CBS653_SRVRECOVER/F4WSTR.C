/* f4wstr.c (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

/* wide-string support functions for CodeBase */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifndef S4OFF_WRITE
   // CS 1999/09/09
   void S4FUNCTION f4assignUnicode( FIELD4 *field, const WSTR5 *wideString )
   {
      f4assignWideString( field, wideString ) ;
   }

   void S4FUNCTION f4assignWideString( FIELD4 *field, const WSTR5 *wideString )
   {
      #ifdef E4PARM_HIGH
         if ( field == 0 )
         {
            error4( 0, e4parm_null, E90533 ) ;
            return ;
         }

         if ( wideString == 0 )
         {
            error4( field->data->codeBase, e4parm_null, E90533 ) ;
            return ;
         }
      #endif

      #ifdef E4ANALYZE
         if ( field->data == 0 )
         {
            error4( 0, e4struct, E90533 ) ;
            return ;
         }
         if ( field->data->codeBase == 0 )
         {
            error4( 0, e4struct, E90533 ) ;
            return ;
         }
      #endif

      if ( error4code( field->data->codeBase ) < 0 )
         return ;

      #ifndef S4SERVER
         #ifndef S4OFF_ENFORCE_LOCK
            if ( field->data->codeBase->lockEnforce && field->data->recNum > 0L )
               if ( d4lockTest( field->data, field->data->recNum, lock4write ) != 1 )
               {
                  error4( field->data->codeBase, e4lock, E90533 ) ;
                  return ;
               }
         #endif
      #endif

      if ( field->type == r5wstr )
      {
         /* LY 2001/07/13 : changed to c4wcslen for 4 byte wchar on Linux */
         f4assignN( field, (char *)wideString, (unsigned)(c4wcslen(wideString) * 2) ) ;
         return ;
      }

      if ( field->type == r5wstrLen )
      {
         /* LY 2001/07/13 : changed to c4wcslen for 4 byte wchar on Linux */
         unsigned len = (unsigned)c4wcslen(wideString) * 2 ;
         if ( len > field->len - sizeof( unsigned short ) )
            len = field->len - sizeof( unsigned short ) ;
         /* LY 2001/07/13 : changed to c4wcslen for 4 byte wchar on Linux */
         f4assignN( field, (char *)wideString, (unsigned)(c4wcslen(wideString) * 2) ) ;
         unsigned short *lenPos = (unsigned short *)(f4ptr( field ) + field->len - sizeof( unsigned short )) ;
         *lenPos = len / 2 ;
         return ;
      }

      /* else unsupported type */

      error4( field->data->codeBase, e4parm, E81409 ) ;
   }

   short S4FUNCTION f4memoAssignUnicode( FIELD4 *field, const WSTR5 *wideString )
   {
      #ifdef E4PARM_HIGH
         if ( field == 0 )
            return error4( 0, e4parm_null, E90518 ) ;

         if ( wideString == 0 )
            return error4( field->data->codeBase, e4parm_null, E90518 ) ;
      #endif

      #if !defined(S4SERVER) && !defined(S4OFF_ENFORCE_LOCK)
         if ( field->data->codeBase->lockEnforce && field->data->recNum > 0L )
            if ( d4lockTest( field->data, field->data->recNum, lock4write ) != 1 )
               return error4( field->data->codeBase, e4lock, E90519 ) ;
      #endif

      switch( f4type( field ) )
      {
         case r4memo :
         case r4gen :
         #if defined(S4MDX) || defined(S4CLIENT)
            case r4bin :
         #endif
            /* LY 2001/07/13 : changed to c4wcslen for 4 byte wchar on Linux */
            return f4memoAssignN(field, (const char *)wideString, c4wcslen( wideString ) * 2 ) ;
         default :
            f4assignWideString( field, wideString ) ;
      }

      return r4success ;
   }
#endif /* S4OFF_WRITE */
