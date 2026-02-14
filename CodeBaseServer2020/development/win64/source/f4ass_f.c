/* f4ass_f.c (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifndef S4OFF_WRITE
void S4FUNCTION f4assignField( FIELD4 *fieldTo, const FIELD4 *fieldFrom )
{
   #ifdef E4PARM_HIGH
      if ( fieldTo == 0 || fieldFrom == 0 )
      {
         error4( 0, e4parm_null, E90501 ) ;
         return ;
      }
   #endif

   if ( error4code( fieldTo->data->codeBase ) < 0 )
      return ;

   #ifndef S4SERVER
      #ifndef S4OFF_ENFORCE_LOCK
         if ( fieldTo->data->codeBase->lockEnforce && fieldTo->data->recNum > 0L )
            if ( d4lockTest( fieldTo->data, fieldTo->data->recNum, lock4write ) != 1 )
            {
               error4( fieldTo->data->codeBase, e4lock, E90501 ) ;
               return ;
            }
      #endif
   #endif

   #ifdef S4CLIENT_OR_FOX
      /* first check if null, if so then assign null */
      if ( f4null( fieldFrom ) )
         f4assignNull( fieldTo ) ;
      else
   #endif
      switch( fieldTo->type )
      {
         case r4str:
         case r5wstr:
            f4assignN( fieldTo, f4ptr( fieldFrom ), fieldFrom->len ) ;
            break ;
         case r5wstrLen:
            if ( fieldTo->len != fieldFrom->len || fieldTo->type != fieldFrom->type )  // lens must match or it too complicated...
               error4( fieldTo->data->codeBase, e4notSupported, E90501 ) ;
            f4assignN( fieldTo, f4ptr( fieldFrom ), fieldFrom->len ) ;
            break ;
         case r5i8:
         case r5ui8:
         case r5dbDate:
         case r5dbTime:
         case r5dbTimeStamp:
         case r5ui4:
         case r5i2:
         case r5ui2:
         case r5guid:
            if ( fieldTo->type != fieldFrom->type )  // too complicated - don't bother for now, can if add later if desired
               error4( fieldTo->data->codeBase, e4notSupported, E90501 ) ;
            f4assignN( fieldTo, f4ptr( fieldFrom ), fieldFrom->len ) ;
            break ;
         case r4num:
         case r4float:
            if( fieldFrom->len == fieldTo->len && fieldFrom->dec == fieldTo->dec &&
                (fieldFrom->type == r4num || fieldFrom->type == r4float) )
               c4memcpy( f4assignPtr( fieldTo ), f4ptr( fieldFrom ), fieldTo->len ) ;
            else
               f4assignDouble( fieldTo, f4double( fieldFrom ) ) ;
            break ;
         case r4log:
            if( fieldFrom->type == r4log )
               *f4assignPtr( fieldTo ) = *f4ptr( fieldFrom ) ;
            break ;
         case r4date:
         #ifdef S4CLIENT_OR_FOX
            case r4currency:
            case r4dateTime:
            case r4int:
            case r4double:
         #endif
            if( fieldFrom->type == fieldTo->type )
               c4memcpy( f4assignPtr( fieldTo ), f4ptr( fieldFrom ), fieldTo->len ) ;
            break ;
         default:  // probably memo
            error4( fieldTo->data->codeBase, e4parm, E80501 ) ;
      }
}
#endif  /* S4OFF_WRITE */

