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

/*ex28.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( )
{
   char to[10] ;

   /* The result put into 'to' will be "C B A" */
   c4encode( to, "ABC", "3 2 1", "123" ) ;
   printf("Result = %s\n", to) ;

   /* The result put into 'to' will be "19901230" */
   c4encode( to, "30-12/1990", "789A4512", "123456789A" ) ;
   printf("Result = %s\n", to) ;

   /* The result put into 'to' will be "12/30/90" */
   c4encode( to, "19901230", "EF/GH/CD", "ABCDEFGH" ) ;
   printf("Result = %s\n", to) ;

   return ;
}