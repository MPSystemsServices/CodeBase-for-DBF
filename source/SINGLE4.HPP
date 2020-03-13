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


class S4CLASS Single4
{
   // cannot have any virtual functions - we don't want to set up a virtual jump table
   // or else casts to/from Single4 can't work properly
public:
   inline void init() { nextLink = 0; }
   inline Single4 *next() { return nextLink ; }
   inline Single4 *initIterate() { return (Single4 *) nextLink ; }
   inline int isLastLink() { return ( nextLink->nextLink == 0 ) ; }  // means element we are at is the last on the chain
   inline void add( Single4 *single )
   {
      single->nextLink  = nextLink;
      nextLink = single;
   }
private:
   Single4 *nextLink ;
} ;

class S4CLASS Single4distant
{
public:
   inline Single4 *toItem() { return (Single4 *) nextLink->nextLink ; }
   inline void next() { nextLink = nextLink->nextLink ; }
   inline void initIterate( Single4 *p ) { nextLink = (Single4distant *)p ; }
   inline void remove() { nextLink->nextLink = nextLink->nextLink->nextLink ; }
   inline void removeWithCheck() { if( nextLink ) if( nextLink->nextLink ) remove() ; }
   inline int isLastLink() { return ( nextLink->nextLink->nextLink == 0 ) ; }  // means element we are at is the last on the chain
private:
   Single4distant *nextLink ;
} ;


int single4find( Single4 *, Single4 * ) ;  /* currently in m4mem2.c */
