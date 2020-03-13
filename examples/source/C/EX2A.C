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

/*ex2a.c*/
#include "d4all.h"  

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

FIELD4INFO fields[] =  
{
    { "NAME_FLD", 'C', 20, 0 },
    { "AGE_FLD", 'N', 3, 0 },
    { 0,0,0,0 }
};
 
void main( void ) 
{
    CODE4 cb ;

    code4init( &cb ) ;
    cb.codePage = cp1252 ;  
    cb.collatingSequence = sort4general ;

    /* All database created will be stamped as using code page 1252. */
    /* All index files created will be sorted according to the general */
    /*  collating sequence. */

    code4initUndo( &cb ) ;
}

