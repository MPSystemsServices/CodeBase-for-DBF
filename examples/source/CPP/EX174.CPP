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

#include "d4all.hpp"

void main()
{
   Str4ten name( "BOB" ) ;
   Str4ptr address( "1234 Anytown Street Apt. 12" ) ;

   /* the following cout does not work correctly, since the internal 
      CodeBase buffer used for name.str( ) is overwritten by the address.str()
      function call */
   cout << name.str( ) << " lives at " << address.str( ) <<endl ; 
 
   // these two lines produce the desired result
   cout << name.str( ) << " lives at " ;
   cout << address.str( ) << endl ; 
}
