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

/* c4long.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

/* this file is included directly... */

#ifndef S4NO_LONGLONG
char *c4longlongToA( char *out, LONGLONG val )
{
   // assumes out is large enough to hold a longlong conversion (i.e. 21 bytes if null included)
   // returns a pointer to the result, which will somewhere in 'out'
   int isNeg ;

   // LY Jan 18/05 : remove code for putting null terminator at 21st char
   // (function currently used in f4assignLongLong() in numeric fields of len
   // 20; therefore, putting null at 21st char could overwrite other fields)
   // out += 20 ;
   char *begin = out ;
   out += 19 ;

   // out[0] = 0 ;  // place the null byte
   // out-- ;

   if ( val < 0 )
      isNeg = 1 ;
   else
      isNeg = 0 ;

   if ( val == 0 )
   {
      out[0] = '0' ;
      return out ;
   }

   while ( val != 0 )
   {
      char valMod10 = (char)( val % 10 ) ;
      out[0] = '0' + (isNeg ? -valMod10 : valMod10 ) ;
      val = val / 10 ;
      out-- ;
   }

   if ( isNeg )
   {
      out[0] = '-' ;
      out-- ;
   }

   // LY Jan 18/05 : if out not originally empty (e.g. updating existing record)
   // and val requires less than 20 chars, leading chars should be assigned
   if ( out > begin )
      memset( begin, ' ', out - begin ) ;

   return out + 1 ;
}
#endif
