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

/* c4hook.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif  /* __TUROBC__ */
#endif  /* S4UNIX */


#ifdef S4TIMEOUT_HOOK
int code4timeoutHook( CODE4 *c4, int numAttempts, long elapsedTime )
{
   return 0 ;
}
#endif



#ifdef S4LOCK_HOOK
int code4lockHook( CODE4 *c4, const char *fileName, const char *userId, const char *networkId, long item, int numAttempts )
{
   return 0 ;
}
#endif



#ifndef S4TEST
#ifdef E4HOOK
/* Uncomment and add alternative error display code here.
   Function error4hook() may be placed in your own separate source code
   file, but ensure that the same function prototype as below is used.
void S4FUNCTION error4hook( CODE4 *c4, int errCode, long errCode2, const char *desc1, const char *desc2, const char *desc3 )
{
   return ;
}
*/
#endif
#endif
