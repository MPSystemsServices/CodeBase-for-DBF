/* f4true.c (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved.

   Returns a true or false.
*/

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

int S4FUNCTION f4true( const FIELD4 *field )
{
   #ifdef E4VBASIC
      if ( c4parm_check( (void *)field, 3, E90537 ) )
         return -1 ;
   #endif

   #ifdef E4PARM_HIGH
      if ( field == 0 )
         return error4( 0, e4parm_null, E90537 ) ;
      if ( f4typeInternal( field ) != r4log )
         error4( field->data->codeBase, e4parm, E81409 ) ;
   #endif

   switch ( *f4ptr( field ) )
   {
      case 'Y':
      case 'y':
      case 'T':
      case 't':
         return 1;
   }
   return 0 ;
}
