/* f4long.c (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifndef S4OFF_WRITE
   void S4FUNCTION f4assignLong( FIELD4 *field, const long lValue )
   {
      CODE4 *c4 ;

      #ifdef E4VBASIC
         if ( c4parm_check( (void *)field, 3, E90516 ) )
            return ;
      #endif

      #ifdef E4PARM_HIGH
         if ( field == 0 )
         {
            error4( 0, e4parm_null, E90516 ) ;
            return ;
         }
         switch( field->type )
         {
            case r4log:
            #ifdef S4CLIENT_OR_FOX
               case r4system:
               case r4dateTime:
            #endif
               error4( field->data->codeBase, e4parm, E81409 ) ;
               return ;
            default:
               break ;
         }
      #endif

      c4 = field->data->codeBase ;

      if ( error4code( c4 ) < 0 )
         return ;

      #ifndef S4SERVER
         #ifndef S4OFF_ENFORCE_LOCK
            if ( c4->lockEnforce && field->data->recNum > 0L )
               if ( d4lockTest( field->data, field->data->recNum, lock4write ) != 1 )
               {
                  error4( c4, e4lock, E90516 ) ;
                  return ;
               }
         #endif
      #endif

      switch ( field->type )
      {
         case r4date:
            date4assign( f4assignPtr( field ), lValue ) ;
            break ;
         case r4int:
         case r5ui2:  // treat same as int
         case r5i2:  // treat same as int
         case r5ui4:  // treat same as int
            f4assignInt( field, (int)lValue ) ;
            break ;
         #ifdef S4CLIENT_OR_FOX
            case r4double:
               f4assignDouble( field, (double)lValue ) ;
               break ;
            case r4memo:
            case r4memoBin:
            case r4gen:
               if ( f4len( field ) == 4 )
                  #ifdef S4BYTE_SWAP
                     *((S4LONG *)f4assignPtr( field )) = x4reverseLong(&lValue) ;
                  #else
                     #ifdef S4DATA_ALIGN  /* LY 00/11/06 : changed from S4WINCE */
                        memcpy( f4assignPtr( field ), &lValue, 4 ) ;  /* LY 00/01/31 */
                     #else
                        *((S4LONG *)f4assignPtr( field )) = lValue ;
                     #endif
                  #endif
               else
                  c4ltoa45( lValue, f4assignPtr( field ), field->len ) ;
               break ;
         #endif
         default:
            if ( field->dec == 0 )
               c4ltoa45( lValue, f4assignPtr( field ), field->len ) ;
            else
               f4assignDouble( field, (double)lValue ) ;
            break ;
      }
   }



   #ifdef S4WIN32    /* LY 00/01/24 : for 32-bit only */
   void S4FUNCTION f4assignLongLong( FIELD4 *field, const LONGLONG lValue )
   {
      /* AS 12/06/99 for simba/administrator, need to assign a very large value to 20 byte
         numeric field...

         currently only support assigning to Numeric field types len 20 with no decimals for now...
      */

      CODE4 *c4 ;

      #ifdef E4VBASIC
         if ( c4parm_check( (void *)field, 3, E90516 ) )
            return ;
      #endif

      #ifdef E4PARM_HIGH
         if ( field == 0 )
         {
            error4( 0, e4parm_null, E90516 ) ;
            return ;
         }
         if ( field->type != r4num || field->dec != 0 || field->len != 20 )
         {
            error4( field->data->codeBase, e4parm, E81409 ) ;
            return ;
         }
      #endif

      c4 = field->data->codeBase ;

      if ( error4code( c4 ) < 0 )
         return ;

      #ifndef S4SERVER
         #ifndef S4OFF_ENFORCE_LOCK
            if ( c4->lockEnforce && field->data->recNum > 0L )
               if ( d4lockTest( field->data, field->data->recNum, lock4write ) != 1 )
               {
                  error4( c4, e4lock, E90516 ) ;
                  return ;
               }
         #endif
      #endif

      // must be field type r4num if here...
      assert5( field->len == 20 && field->dec == 0 ) ;  // c4longlongToA assumptions...
      c4longlongToA( f4assignPtr( field ), lValue ) ;
   }
   #endif  /* S4WIN32 */
#endif



long S4FUNCTION f4long( const FIELD4 *field )
{
   #ifdef S4CLIENT_OR_FOX
      const char *ptr ;
   #endif
   #ifdef S4DATA_ALIGN  /* LY 00/11/06 : changed from S4WINCE */
      long lrc ;  /* LY 99/08/24 : changed S4LONG to long */
      char *fldPtr ;
   #endif
   #ifdef E4VBASIC
      if ( c4parm_check( (void *)field, 3, E90517 ) )
         return 0L ;
   #endif

   #ifdef E4PARM_HIGH
      if ( field == 0 )
      {
         error4( 0, e4parm_null, E90517 ) ;
         return -1L ;
      }
      switch( field->type )
      {
         case r4log:
         #ifdef S4CLIENT_OR_FOX
            case r4system:
            case r4dateTime:
         #endif
            error4( field->data->codeBase, e4parm, E81409 ) ;
            return -1L ;
         default:
            break ;
      }
   #endif

   switch( field->type )
   {
      case r4date:
         return date4long( f4ptr( field ) ) ;
      case r4int:
      case r5ui4:  // treat same as int
         #ifdef S4BYTE_SWAP
            return x4reverseLong( f4ptr( field) ) ;
         #else
            #ifdef S4DATA_ALIGN  /* LY 00/11/06 : changed from S4WINCE */
               fldPtr = f4ptr( field ) ;
               memcpy( (char *) &lrc, fldPtr, sizeof(long) ) ;  /* LY 99/9/21 : &lrc */
               return lrc ;
            #else
               return *( (S4LONG *)f4ptr( field ) ) ;
            #endif
         #endif
      // AS 06/09/00 ui2 was coming back as signed... fixed
      case r5ui2:  // treat same as int
         return * (unsigned short *)( f4ptr(field ) ) ;
      case r5i2:  // treat same as int
         return * (short *)( f4ptr(field ) ) ;
      #ifdef S4CLIENT_OR_FOX
         case r4memo:
         case r4memoBin:
         case r4gen:
            if ( f4len( field ) == 4 )
               #ifdef S4BYTE_SWAP
                  return x4reverseLong(f4ptr(field)) ;
               #else
                  #ifdef S4DATA_ALIGN  /* L.Y. 1999/2/9 : problem with CE-VB */  /* LY 00/11/06 : changed from S4WINCE */
                  {
                     fldPtr = f4ptr( field ) ;
                     /* LY 99/08/24 : changed S4LONG to long */
                     memcpy( (char *) &lrc, field->data->record + field->offset, sizeof(long) ) ;  /* LY 99/9/21 : &lrc */
                     return lrc ;
                  }
                  #else
                     return *((S4LONG *)f4ptr( field )) ;
                  #endif
               #endif
            break ;
         case r4double:
            #ifdef S4DATA_ALIGN  /* LY 00/07/24 : for S4WINCE */
               double retDbl ;
               memcpy( &retDbl, f4ptr( field ), sizeof(double) ) ;
               return (long) retDbl ;
            #else
               return (long)(*((double *)f4ptr( field ))) ;
            #endif
         case r4currency:
            ptr = f4currency( field, 0 ) ;
            return c4atol( ptr, c4strlen( ptr ) ) ;
      #endif
      default:
         break ;
   }
   return c4atol( f4ptr( field ), field->len ) ;
}
