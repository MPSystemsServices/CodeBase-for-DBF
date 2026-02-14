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

// AS Jul 20/05 - Support for new field type binary float
void S4FUNCTION f4assignFloat( FIELD4 *field, const float fValue )
{
   // AS Feb 8/06 - compiler fix
   #if !defined( S4OFF_WRITE ) && defined( S4CLIENT_OR_FOX )
      #ifdef S4DATA_ALIGN
         int tempInt ;  /* LY 00/07/03 */
      #endif
      #ifdef S4BYTE_SWAP  /* LY 2001/07/21 */
         double tempDbl ;
         int tempInt2 ;
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
      #endif

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

      if( field->type == r4floatBin )
      {
         #ifdef S4DATA_ALIGN
            #ifdef S4BYTE_SWAP  /* LY 2001/07/21 */
               tempDbl = x4reverseFloat( &fValue ) ;
               memcpy( f4assignPtr( field ), &tempDbl, sizeof(float) ) ;
            #else
               memcpy( f4assignPtr( field ), &fValue, sizeof(float) ) ;
            #endif
            // AS Dec 11/07 - fix for data_align
            return ;
            // break ;
         #else
            #ifdef S4BYTE_SWAP  /* LY 2002/08/11 */
               tempDbl = x4reverseFloat( &fValue ) ;
               *((float *)f4assignPtr( field )) = tempDbl ;
            #else
               *((float *)f4assignPtr( field )) = fValue ;
            #endif
            return ;
         #endif
      }
      error4( field->data->codeBase, e4parm, E81409 ) ;
   #else
      error4( field->data->codeBase, e4notSupported, E90504 ) ;
   #endif /* S4CLIENT_OR_FOX else */
   return ;
}


#ifndef S4OFF_WRITE
   void S4FUNCTION f4assignDouble( FIELD4 *field, const double dValue )
   {
      #ifdef S4CLIENT_OR_FOX
         char currencyBuffer[21] ;
         #ifdef S4DATA_ALIGN
            int tempInt ;  /* LY 00/07/03 */
         #endif
         #ifdef S4BYTE_SWAP  /* LY 2001/07/21 */
            double tempDbl ;
            int tempInt2 ;
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
               case r4dateTimeMilli:  // AS Mar 10/03 - ms support in datetime
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
                  #ifdef S4BYTE_SWAP  /* LY 2001/07/21 */
                     tempDbl = x4reverseDouble( &dValue ) ;
                     memcpy( f4assignPtr( field ), &tempDbl, sizeof(double) ) ;
                  #else
                     memcpy( f4assignPtr( field ), &dValue, sizeof(double) ) ;
                  #endif
                  break ;
               case r4int:
                  tempInt = (int) dValue ;
                  #ifdef S4BYTE_SWAP  /* LY 2001/07/21 */
                     tempInt2 = x4reverseLong( &tempInt ) ;
                     memcpy( f4assignPtr( field ), &tempInt2, sizeof(int) ) ;
                  #else
                     memcpy( f4assignPtr( field ), &tempInt, sizeof(int) ) ;
                  #endif
                  break ;
            #else
               case r4double:
                  #ifdef S4BYTE_SWAP  /* LY 2002/08/11 */
                     tempDbl = x4reverseDouble( &dValue ) ;
                     *((double *)f4assignPtr( field )) = tempDbl ;
                  #else
                     *((double *)f4assignPtr( field )) = dValue ;
                  #endif
                  break ;
               case r4int:
                  #ifdef S4BYTE_SWAP  /* LY 2002/08/11 */
                     tempInt2 = (int) dValue ;
                     *((int *)f4assignPtr( field )) = x4reverseLong( &tempInt2 ) ;
                  #else
                     *((int *)f4assignPtr( field )) = (int)dValue ;
                  #endif
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

// AS Jul 21/05 - Support for new field type binary float
float S4FUNCTION f4float( const FIELD4 *field )
{
   // AS Jan 20/06 - compile warnings fix
   #ifdef S4CLIENT_OR_FOX
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
      #endif

      if( field->type == r4floatBin )
      {
         #ifdef S4DATA_ALIGN  /* LY 00/07/24 : for S4WINCE */
            float retDbl ;
            memcpy( &retDbl, f4ptr( field ), sizeof(float) ) ;
            #ifdef S4BYTE_SWAP  /* LY 2001/07/21 */
               return x4reverseFloat( &retDbl ) ;
            #else
               return retDbl ;
            #endif
         #else
            /* LY 2002/08/11 : uncomment code; fails t4field.c if x4reverseFloat not used */
            // CJ July 4/01 The value should have already be swaped during retrieval of the data.
            #ifdef S4BYTE_SWAP  /* LY 00/12/15 */
               return x4reverseFloat((float *)f4ptr( field )) ;
            #else
               return *((float *)f4ptr( field )) ;
            #endif
         #endif
      }
   #endif

   error4( field->data->codeBase, e4parm, E81409 ) ;
   return (float)0 ;
}




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
            case r4dateTimeMilli:  // AS Mar 10/03 - ms support in datetime
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
               #ifdef S4BYTE_SWAP  /* LY 2001/07/21 */
                  return x4reverseDouble( &retDbl ) ;
               #else
                  return retDbl ;
               #endif
            #else
               /* LY 2002/08/11 : uncomment code; fails t4field.c if x4reverseDouble not used */
               // CJ July 4/01 The value should have already be swaped during retrieval of the data.
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
               #ifdef S4BYTE_SWAP  /* LY 2001/07/21 */
                  return (double) x4reverseLong( &retInt ) ;
               #else
                  return (double) retInt ;
               #endif
            #else
               #ifdef S4BYTE_SWAP  /* LY 2002/08/11 */
                  return (double) x4reverseLong( (int *)f4ptr( field ) ) ;
               #else
                  return (double)(*((int *)f4ptr( field ))) ;
               #endif
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
