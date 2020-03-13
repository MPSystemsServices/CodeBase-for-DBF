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

/*  c4bcd.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */
/*                binary digit is:   xxxx xxxx  xxxx xx01                */
/*                                   sig dig    |||| ||||                */
/*                                              |||| ||always 01         */
/*                                              |length                  */
/*                                              sign                     */

#include "d4all.h"

#ifdef __TURBOC__
   #pragma hdrstop
#endif  /* __TUROBC__ */

#ifdef S4MDX

/* MDX */
#ifdef P4ARGS_USED
   #pragma argsused
#endif
int S4CALL c4bcdCmp( S4CMP_PARM aPtr, S4CMP_PARM bPtr, size_t dummyLen )
{
   int aSign, bSign, aLen, bLen, aSigDig, bSigDig, compareLen, rc ;

   if ( ((C4BCD *)aPtr)->digitInfo  & 0x80 )  /* get digit sign */
      aSign = -1 ;
   else
      aSign =  1 ;

   if ( ((C4BCD *)bPtr)->digitInfo & 0x80 )
      bSign = -1 ;
   else
      bSign =  1 ;

   if ( aSign != bSign )
      return aSign ;

   aLen = ( (((C4BCD *)aPtr)->digitInfo >> 2) & 0x1F ) ; /* get digit length */
   bLen = ( (((C4BCD *)bPtr)->digitInfo >> 2) & 0x1F ) ;

   if ( aLen == 0 )
      aSigDig = 0 ;
   else
      aSigDig = ((C4BCD *)aPtr)->sigDig ;  /* get digit significant digit */

   if ( bLen == 0 )
      bSigDig = 0 ;
   else
      bSigDig = ((C4BCD *)bPtr)->sigDig ;

   if ( aSigDig != bSigDig )
   {
      if ( aSigDig < bSigDig )
         return -aSign ;
      else
         return aSign ;
   }

   compareLen = (aLen < bLen) ? bLen : aLen ;  /* Use Max */

   compareLen = (compareLen+1)/2 ;

   rc = c4memcmp( ((C4BCD *)aPtr)->bcd, ((C4BCD *)bPtr)->bcd, compareLen ) ;
   if ( aSign < 0 )
      return -rc ;

   return rc ;
}

/* MDX */
void  c4bcdFromD( char *result, const double doub )
{
   char tempStr[258], *ptr ;
   int sign, dec, len, pos ;

   #ifdef S4NO_ECVT
      ptr = f4ecvt( doub, E4ACCURACY_DIGITS, &dec, &sign ) ;
   #else
      ptr = ecvt( doub, E4ACCURACY_DIGITS, &dec, &sign ) ;
   #endif

   if ( sign )
   {
      pos = 1 ;
      tempStr[0] = '-' ;
   }
   else
      pos = 0 ;

   if ( dec < 0 )
   {
      dec = -dec ;
      len = dec+1+pos ;
      c4memcpy( tempStr+len, ptr, E4ACCURACY_DIGITS ) ;
      c4memset( tempStr+pos, '0', len-pos ) ;
      tempStr[pos] = '.' ;

      c4bcdFromA( 0, result, tempStr, E4ACCURACY_DIGITS + len, 0 ) ;
   }
   else
   {
      c4memcpy( tempStr+pos, ptr, dec ) ;
      pos += dec ;
      if ( dec < E4ACCURACY_DIGITS )
      {
         tempStr[pos++] = '.' ;

         len = E4ACCURACY_DIGITS - dec ;
         c4memcpy( tempStr+pos, ptr+dec, len ) ;
         pos += len ;
      }
      c4bcdFromA( 0, result, tempStr, pos, 0 ) ;
   }
}

#endif  /* S4MDX */
