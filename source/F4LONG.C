/* *********************************************************************************************** */
/* Copyright (C) 1999-2015 by Sequiter, Inc., 9644-54 Ave, NW, Suite 209, Edmonton, Alberta Canada.*/
/* This program is free software: you can redistribute it and/or modify it under the terms of      */
/* the GNU Lesser General Public License as published by the Free Software Foundation, version     */
/* 3 of the License.                                                                               */
/*                                                                                                 */
/* This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;       */
/* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.       */
/* See the GNU Lesser General Public License for more details.                                     */
/*                                                                                                 */
/* You should have received a copy of the GNU Lesser General Public License along with this        */
/* program. If not, see <https://www.gnu.org/licenses/>.                                           */
/* *********************************************************************************************** */

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
      #ifdef S4BYTE_SWAP   /* LY 2001/06/19 */
         S4LONG tempLong, tempLong2 ;
      #endif

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
               case r4dateTimeMilli:  // AS Mar 10/03 - ms support in datetime
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
            // AS Jul 21/05 - Support for new field type binary float
            case r4floatBin:
               f4assignFloat( field, (float)lValue ) ;
               break ;
            case r4double:
               f4assignDouble( field, (double)lValue ) ;
               break ;
            case r4memo:
            case r4memoBin:
            case r4gen:
               if ( f4len( field ) == 4 )
               {
               #ifdef S4BYTE_SWAP
                  /* LY 2001/06/19 : if lValue 64-bit, converts to 32-bit */
                  tempLong2 = (S4LONG) lValue ;
                  #ifdef S4DATA_ALIGN  /* LY 2001/06/19 */
                     tempLong = x4reverseLong( &tempLong2 ) ;
                     memcpy( f4assignPtr( field ), &tempLong, sizeof(tempLong) ) ;  /* LY 00/01/31 */
                  #else
                     *((S4LONG *)f4assignPtr( field )) = x4reverseLong(&tempLong2) ;
                  #endif
               #else
                  #ifdef S4DATA_ALIGN  /* LY 00/11/06 : changed from S4WINCE */
                     memcpy( f4assignPtr( field ), &lValue, 4 ) ;  /* LY 00/01/31 */
                  #else
                     *((S4LONG *)f4assignPtr( field )) = lValue ;
                  #endif
               #endif
               }
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
      /* LY May 17/04 : added support for r5ui8 and r5i8 */

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

         if ( ( field->type == r4num && ( field->dec != 0 || field->len != 20 ) )   // LY May 17/04 : changed from != r4num ||
            || ( field->type != r4num && field->type != r5ui8 && field->type != r5i8 ) )  // added !r5ui8 and !r5i8
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

      /* LY May 17/04 : reserve old code for r4num, added support for r5i8 and r5ui8 */
      if ( field->type == r4num )
      {
         // must be field type r4num if here...
         assert5( field->len == 20 && field->dec == 0 ) ;  // c4longlongToA assumptions...
         c4longlongToA( f4assignPtr( field ), lValue ) ;
      }
      else
         // LY Aug 23/04 : added S4BYTE_SWAP
         #ifdef S4BYTE_SWAP
            {
               long long tempLongLong = x4reverseLongLong( &lValue ) ;
               memcpy( f4assignPtr( field ), &tempLongLong, sizeof(tempLongLong) ) ;
            }
         #else
            memcpy( f4assignPtr( field ), &lValue, sizeof(lValue) ) ;
         #endif
   }
   #endif  /* S4WIN32 */
#endif



long S4FUNCTION f4long( const FIELD4 *field )
{
   #ifdef S4CLIENT_OR_FOX
      const char *ptr ;
   #endif
   #ifdef S4DATA_ALIGN  /* LY 00/11/06 : changed from S4WINCE */
      // long lrc ;  /* LY 99/08/24 : changed S4LONG to long */
      S4LONG tempLong ; /* LY 2001/06/19 */
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
            case r4dateTimeMilli:  // AS Mar 10/03 - ms support in datetime
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
            #ifdef S4DATA_ALIGN  /* LY 2001/06/19 */
               memcpy( (char *) &tempLong, f4ptr( field ), sizeof(tempLong) ) ;
               return (long) x4reverseLong( &tempLong ) ;
            #else
               return x4reverseLong( f4ptr( field) ) ;
            #endif
         #else
            #ifdef S4DATA_ALIGN  /* LY 00/11/06 : changed from S4WINCE */
               /* LY 2001/06/19 : changed lrc to tempLong */
               memcpy( (char *) &tempLong, f4ptr( field ), sizeof(tempLong) ) ;  /* LY 99/9/21 : &lrc */
               return (long) tempLong ;
            #else
               return *( (S4LONG *)f4ptr( field ) ) ;
            #endif
         #endif
      // AS 06/09/00 ui2 was coming back as signed... fixed
      case r5ui2:  // treat same as int
         // LY Jun 30/04 : added S4DATA_ALIGN
         #ifdef S4DATA_ALIGN
         {
            unsigned short tempShort ;
            memcpy( (char *)&tempShort, f4ptr( field ), sizeof( unsigned short ) ) ;
            return tempShort ;
         }
         #elif defined( S4BYTE_SWAP )  // LY Aug 23/04
            long tempLong = 0 ;
            memcpy( &tempLong, f4ptr(field), sizeof(short) ) ;
            return x4reverseLong( &tempLong ) ;
         #else
            return * (unsigned short *)( f4ptr(field ) ) ;
         #endif
      case r5i2:  // treat same as int
         return * (short *)( f4ptr(field ) ) ;
      #ifdef S4CLIENT_OR_FOX
         case r4memo:
         case r4memoBin:
         case r4gen:
            if ( f4len( field ) == 4 )
               #ifdef S4BYTE_SWAP
                  #ifdef S4DATA_ALIGN  /* L.Y. 2001/06/19 */
                  {
                     memcpy( (char *) &tempLong, f4ptr( field ), sizeof(tempLong) ) ;
                     return (long) x4reverseLong( &tempLong ) ;
                  }
                  #else
                     return x4reverseLong(f4ptr(field)) ;
                  #endif
               #else
                  #ifdef S4DATA_ALIGN  /* L.Y. 1999/2/9 : problem with CE-VB */  /* LY 00/11/06 : changed from S4WINCE */
                  {
                     /* LY 99/08/24 : changed S4LONG to long */
                     /* LY 2001/06/19 : changed lrc to tempLong */
                     memcpy( (char *) &tempLong, f4ptr( field ), sizeof(tempLong) ) ;  /* LY 99/9/21 : &lrc */
                     return (long) tempLong ;
                  }
                  #else
                     return *((S4LONG *)f4ptr( field )) ;
                  #endif
               #endif
            break ;
         // AS Jul 21/05 - Support for new field type binary float
         case r4floatBin:
            #ifdef S4DATA_ALIGN  /* LY 00/07/24 : for S4WINCE */
               float retFloat ;
               memcpy( &retFloat, f4ptr( field ), sizeof(float) ) ;
               #ifdef S4BYTE_SWAP  /* LY 2001/07/27 */
                  return (long) x4reverseFloat( &retFloat ) ;
               #else
                  return (long) retFloat ;
               #endif
            #else
               #ifdef S4BYTE_SWAP  /* LY 2002/08/11 */
                  return (long) x4reverseFloat( (float *)f4ptr( field ) ) ;
               #else
                  return (long)(*((float *)f4ptr( field ))) ;
               #endif
            #endif
         case r4double:
            #ifdef S4DATA_ALIGN  /* LY 00/07/24 : for S4WINCE */
               double retDbl ;
               memcpy( &retDbl, f4ptr( field ), sizeof(double) ) ;
               #ifdef S4BYTE_SWAP  /* LY 2001/07/27 */
                  return (long) x4reverseDouble( &retDbl ) ;
               #else
                  return (long) retDbl ;
               #endif
            #else
               #ifdef S4BYTE_SWAP  /* LY 2002/08/11 */
                  return (long) x4reverseDouble( (double *)f4ptr( field ) ) ;
               #else
                  return (long)(*((double *)f4ptr( field ))) ;
               #endif
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


LONGLONG S4FUNCTION f4longLong( FIELD4 *field )
{
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
   #endif

   if ( field->type == r5ui8 || field->type == r5i8 )
   {
      #ifdef S4DATA_ALIGN  // LY Jun 30/04
         LONGLONG tempLongLong ;
         memcpy( (char *)&tempLongLong, f4ptr( field ), sizeof( LONGLONG ) ) ;
         return tempLongLong ;
      #else
         #ifdef S4BYTE_SWAP   // LY Aug 23/04
            return x4reverseLongLong( f4ptr( field ) ) ;
         #else
            return *(LONGLONG *)f4ptr( field ) ;
         #endif
      #endif
   }
   else
      return f4long( field ) ;
}
