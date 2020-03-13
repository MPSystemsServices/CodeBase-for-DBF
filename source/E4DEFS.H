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

/* e4defs.h   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

/* contains 2nd error number info */


/* ABCDE:
   A = SEVERITY NUMBER
   BC = SOURCE FILE #
   DE = LOCAL ERROR #
*/


#define E80101  80101   /* already connected */
#define E80102  80102   /* part number out of sequence */

#define E90101  90101   /* connection4initUndo */
#define E90102  90102   /* socket4initUndo */
#define E90103  90103   /* socket4init */
#define E90104  90104   /* connection4init */
#define E90105  90105   /* connection4setStatus */
#define E90106  90106   /* connection4assign */
#define E90107  90107   /* socket4connected */
#define E90108  90108   /* socket4connect */
#define E90109  90109   /* socket4alloc */
#define E90110  90110   /* connection4alloc */
#define E90111  90111   /* connection4disconnect */
#define E90112  90112   /* connection4type */
#define E90113  90113   /* connection4status */
#define E90114  90114   /* connection4setLen */
#define E90115  90115   /* connection4len */
#define E90116  90116   /* connection4dataOffset */
#define E90117  90117   /* connection4receive */
#define E90118  90118   /* socket4receive */
#define E90119  90119   /* connection4send */
#define E90120  90120   /* connection4clear */
#define E90121  90121   /* connection4addData */
#define E90122  90122   /* d4socket */
#define E90123  90123   /* connection4clientId */
#define E90124  90124   /* connection4id */
#define E90125  90125   /* connection4serverId */
#define E90126  90126   /* connection4readLock */
#define E90127  90127   /* connection4enforceLock */
