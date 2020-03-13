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

/* s4string.cpp/cxx (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.hpp"
#ifndef S4JNI  /* LY 99/07/08 */
#ifdef __TURBOC__
   #pragma hdrstop
#endif  /* __TUROBC__ */

static int  num_per_type[16] = { 8, 6, 4, 3, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1};

static int  calc_type( unsigned l )
{
   long n = 16 ;
   int  i = 0 ;

   while ( (long) l > n )
   {
      i++ ;
      n <<= 1 ;
   }
   return i ;
}

static unsigned calc_max( int i_type )
{
   unsigned result =  1 << (unsigned) (i_type+4) ;
   if ( result <= 0xFE00 )
      return result ;
   return 0xFE00 ;
}

Str4flex::Str4flex( Code4 &cb ) : Str4max(0,0)
{
   codeBase =  &cb ;
}

Str4flex::Str4flex( Str4flex& a ) : Str4max(0,0)
{
   codeBase =  a.codeBase ;
   if ( setLen( a.curLen ) < 0 )  return ;
   memcpy( p, a.p, a.curLen ) ;
}

Str4flex::~Str4flex()
{
   free() ;
}


void Str4flex::free()
{
   DEBUG4VOID(codeBase == 0, E61052)
   if ( p != 0 )
   {
      int i_type = calc_type( maxLen ) ;
      mem4free( codeBase->stringTypes[i_type], p ) ;
      p =  0 ;
   }
   curLen = 0  ;
   maxLen = 0 ;  /* LY 00/06/29 : fix #240 */
}

int Str4flex::setMax( unsigned long n )
{
   DEBUG4INT(codeBase == 0, E61053)
   #ifdef D4DLL_CPP
      if ( code4errorCode( codeBase, -5 ) )  return -1 ;
   #else
      if ( codeBase->errorCode )  return -1 ;
   #endif
   if ( n >= UINT_MAX )  return error4( codeBase, e4memory, E61053 ) ;

   if ( n == 0 )
   {
      free() ;
      return curLen =  maxLen =  0 ;
   }
   if ( curLen == 0 )  free() ;

   int i_type =  calc_type( n+1 ) ;
   unsigned new_max =  calc_max(i_type)-1 ;
   if ( new_max < n )  return error4( codeBase, e4memory, E61053 ) ;

   #ifdef E4DEBUG
      if ( i_type > 16 )
         error4( codeBase, e4parm, E61053 ) ;
      if ( calc_type(new_max) != i_type )
         error4( codeBase, e4parm, E61053 ) ;
   #endif

   if ( codeBase->stringTypes[i_type] == 0 )
   {
      codeBase->stringTypes[i_type] = mem4create( 0, num_per_type[i_type], new_max+1, num_per_type[i_type], 0 ) ;
      if ( codeBase->stringTypes[i_type] == 0 )  return error4( codeBase, e4memory, E61053 );
   }

   char *new_p =  (char *)mem4allocZero( codeBase->stringTypes[i_type] ) ;
   if ( new_p == 0 )
      return error4( codeBase, e4memory, E61053 ) ;

   if ( curLen > new_max )
      curLen =  new_max ;
   memcpy( new_p, p, curLen ) ;

   free() ;
   maxLen =  new_max ;
   p =  new_p ;
   p[maxLen] =  0 ;

   return 0 ;
}
#endif   /* !S4JNI  LY 99/07/08 */
