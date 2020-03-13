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

/* FILE s4stamp.cpp (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */


#if defined(S4CLIENT) && defined(E4DEBUG)
   STAMP5 CustomerStamp={
                           {"2BOR!2BHMLT",404},
                           {"SECURITY   ",  4}, 1,
                           {"CONNECTIONS",  4}, 10,
                           {"DISPLAYINFO",  4}, 0,
                           {"CUSTOMERNAM", 50}, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",
                           {"SERIAL     ", 20}, "xxxxxxxxxxxxxxxxxxx",
                           {"ENDCOPYDATE",  5}, "CCYY",
                           {"PROVIDERNAM", 50}, "CodeBase_Client",
                           {"UUIDINFO   ", 39}, "{45B8B480-F768-11d2-99AF-0060083DEECE}",
                           {"ADDITIONADD", 26}, "",
                           {"IDNTDEFAULT",  6}, "False", //Only Values are "True" or "False" any thing else is considerd "False"
                           {"BUILDNUMBER",  sizeof(long)}, 0  // CS 2002/06/17
                        } ;
#else
   STAMP5 CustomerStamp={
                           {"2BOR!2BHMLT",404},
                           {"SECURITY   ",  4}, 1,
                           {"CONNECTIONS",  4}, 10,
                           {"DISPLAYINFO",  4}, 0,
                           {"CUSTOMERNAM", 50}, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",
                           {"SERIAL     ", 20}, "xxxxxxxxxxxxxxxxxxx",
                           {"ENDCOPYDATE",  5}, "CCYY",
                           {"PROVIDERNAM", 50}, "CodeBase",
                           {"UUIDINFO   ", 39}, "{33E17311-1716-11d0-894E-0020AF312DC7}",
                           {"ADDITIONADD", 26}, "",
                           {"IDNTDEFAULT",  6}, "False", //Only Values are "True" or "False" any thing else is considerd "False"
                           {"BUILDNUMBER",  sizeof(long)}, 0  // CS 2002/06/17
                        } ;
#endif
/* CJ - Description of the stamp.
   The stamp is need in all OLEDB provider DLLs and SERVER dlls.
   The stamp will contain members that may not be used in certain cases.  However the
   stamp will contain all members for ease of use for the stamping utilities.  The
   "CONNECTIONS" member is only used in the SERVER case and the "PROVIDERNAM", "UUIDINFO", and
   "ADDITIONADD" members are only used in the OLEDB provider DLL. All other members are used
   in both.

   There is a seperate CLIENT Version of the stamp to provide a different Provider name and
   UUID value for the Client version of the OLEDB provider to allow both to be run on the same
   machine for debugging purposes.
*/

// AS Sept. 19/02 - modify to include input buffer length to avoid access violations, and error if buf not big enough
int u4createCopyrightFromStamp( char *buffer, int bufferLen )
{
   //CJ ***NOTE*** It is very important that if you change the string below you will need to also
   // change the string in the LicenceStamp.exe program as one is not derived from the other.

   char sprintfString[] = "(c) Sequiter Software Inc., 1988-%s. LICENSE NOTICE: This CodeBase software is licensed for use with applications developed by %s only.  Serial Number: %s" ;

   int requiredLen = strlen(sprintfString) + strlen(CustomerStamp.endCopyrightDate) + strlen(CustomerStamp.customerName) + strlen(CustomerStamp.serialNumber) ;
   if ( requiredLen > bufferLen )
      return e4len ;

   sprintf( buffer, sprintfString, CustomerStamp.endCopyrightDate, CustomerStamp.customerName, CustomerStamp.serialNumber) ;
   return 0 ;
}
