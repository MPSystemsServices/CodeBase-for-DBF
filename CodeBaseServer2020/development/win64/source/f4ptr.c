/* f4ptr.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#if !defined( S4SERVER ) && !defined( S4ODBC_BUILD )
   char *S4FUNCTION f4ptr( const FIELD4 *field )
   {
      #ifdef E4VBASIC
         if ( c4parm_check( (void *)field, 3, E90532 ) )
            return 0 ;
      #endif

      #ifdef E4PARM_HIGH
         if ( field == 0 )
         {
            error4( 0, e4parm_null, E90532 ) ;
            return 0 ;
         }
      #endif

      return ( field->data->record + field->offset ) ;
   }
#endif
