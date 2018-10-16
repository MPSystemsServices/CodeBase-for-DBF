/* f4ptr.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

// AS Jun 13/03 - update for odbc, make f4ptr generically available, use internal version
#if !defined( S4SERVER )
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
