/* f4double.c (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

/* Returns the value of the corresponding field as a double.
   Only defined for 'Numeric' fields and 'Character' fields
   containing numeric data.
*/

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifndef S4OFF_WRITE
   void S4FUNCTION f4assignDouble( FIELD4 *field, const double dValue )
   {
      #ifdef S4CLIENT_OR_FOX
         char currencyBuffer[21] ;
         #ifdef S4DATA_ALIGN
            int tempInt ;  /* LY 00/07/03 */
         #endif
      #endif
      #ifdef E4VBASIC
         if ( c4parm_check ( field, 3, E90504 ) )
            return ;
      #endif

      #ifdef E4PARM_HIGH
         if ( field == 0 )
         {
            error4( 0, e4parm_null, E90504 ) ;
            return ;
         }
         switch( field->type )
         {
            case r4log:
            #ifdef S4CLIENT_OR_FOX
               case r4system:
               case r4dateTime:
            #endif
            case r5wstrLen:
            case r5wstr:
            #ifdef S5USE_EXTENDED_TYPES
               case r5i8:
               case r5ui8:
               case r5ui4:
               case r5i2:
               case r5ui2:
               case r5dbDate:
               case r5dbTime:
               case r5dbTimeStamp:
            #endif
               error4( field->data->codeBase, e4parm, E81409 ) ;
               return ;
            default:
               break ;
         }
      #endif

      if ( error4code( field->data->codeBase ) < 0 )
         return ;

      #ifndef S4SERVER
         #ifndef S4OFF_ENFORCE_LOCK
            if ( field->data->codeBase->lockEnforce && field->data->recNum > 0L )
               if ( d4lockTest( field->data, field->data->recNum, lock4write ) != 1 )
               {
                  error4( field->data->codeBase, e4lock, E90504 ) ;
                  return ;
               }
         #endif
      #endif

      switch( field->type )
      {
         case r4date:
            date4assign( f4assignPtr( field ), (long)dValue ) ;
            break ;
         #ifdef S4CLIENT_OR_FOX
            /* LY 00/07/03 : for strict data boundaries */
            #ifdef S4DATA_ALIGN
               case r4double:
                  memcpy( f4assignPtr( field ), &dValue, sizeof(double) ) ;
                  break ;
               case r4int:
                  tempInt = (int) dValue ;
                  memcpy( f4assignPtr( field ), &tempInt, sizeof(int) ) ;
                  break ;
            #else
               case r4double:
                  *((double *)f4assignPtr( field )) = dValue ;
                  break ;
               case r4int:
                  *((int *)f4assignPtr( field )) = (int)dValue ;
                  break ;
            #endif
            case r4currency:
               c4dtoa45( dValue, currencyBuffer, 20, 4 ) ;
               currencyBuffer[20] = 0 ;
               f4assignCurrency( field, currencyBuffer ) ;
               break ;
         #endif
         default:
            c4dtoa45( dValue, f4assignPtr( field ), field->len, field->dec ) ;
            break ;
      }
   }
#endif

double S4FUNCTION f4double( const FIELD4 *field )
{
   #ifdef S4CLIENT_OR_FOX
      const char *ptr ;
   #endif

   #ifdef E4VBASIC
      if ( c4parm_check( (void *)field, 3, E90505 ) )
         return (double)0 ;
   #endif

   #ifdef E4PARM_HIGH
      if ( field == 0 )
      {
         error4( 0, e4parm_null, E90505 ) ;
         return -1.0 ;
      }
      switch( field->type )
      {
         case r4log:
         #ifdef S4CLIENT_OR_FOX
            case r4system:
            case r4dateTime:
         #endif
            error4( field->data->codeBase, e4parm, E81409 ) ;
            return (double)0 ;
         default:
            break ;
      }
   #endif

   switch( field->type )
   {
      case r4date:
         return (double)date4long( f4ptr( field ) ) ;
      #ifdef S4CLIENT_OR_FOX
         case r4double:
            #ifdef S4DATA_ALIGN  /* LY 00/07/24 : for S4WINCE */
               double retDbl ;
               memcpy( &retDbl, f4ptr( field ), sizeof(double) ) ;
               return retDbl ;
            #else
               #ifdef S4BYTE_SWAP  /* LY 00/12/15 */
                  return x4reverseDouble((double *)f4ptr( field )) ;
               #else
                  return *((double *)f4ptr( field )) ;
               #endif
            #endif
         case r4int:
            #ifdef S4DATA_ALIGN  /* LY 00/07/24 : for S4WINCE */
               int retInt ;
               memcpy( &retInt, f4ptr( field ), sizeof(int) ) ;
               return (double) retInt ;
            #else
               return (double)(*((int *)f4ptr( field ))) ;
            #endif
         case r4currency:
            ptr = f4currency( field, 4 ) ;
            return c4atod( ptr, c4strlen( ptr ) ) ;
      #endif
   }

   /* Convert the field data into a 'double' */
   return c4atod( f4ptr( field ), field->len ) ;
}

int S4FUNCTION f4double2( const FIELD4 *field, double *result )
{
   #ifdef E4PARM_HIGH
      if ( field == 0 )
      {
         error4( 0, e4parm_null, E90506 ) ;
         return -1 ;
      }
   #endif

   *result = f4double( field ) ;
   return 0 ;
}
