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

/* stamp5.hpp   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#ifndef STAMP5_INC
#define STAMP5_INC

/* When adding new members to the structure above make sure you cahnge the
   length value in the searchString ENTRYINFO5 member to match the total
   length of structure */
typedef struct STAMP5st
{
   ENTRYINFO5 searchString ;
   ENTRYINFO5 securityOptionInfo ;
   int securityOption;
   ENTRYINFO5 maxConnectionsInfo ;
   int maxConnections;
   ENTRYINFO5 displayErrorInfoInfo;
   int displayErrorInfo;
   ENTRYINFO5 customerNameInfo ;
   char customerName[50];
   ENTRYINFO5 serialNumberInfo ;
   char serialNumber[20];
   ENTRYINFO5 endCopyrightDateInfo ;
   char endCopyrightDate[5];
   ENTRYINFO5 ProviderNameInfo ;
   char ProviderName[50];
   ENTRYINFO5 UUIDinfo ;
   char UUID[39] ;
   ENTRYINFO5 ProviderDescriptionInfo ;
   char ProviderDescription[26] ;
   ENTRYINFO5 ProviderIRIdentityInfo ;
   char ProviderIRIdentity[6] ;
   ENTRYINFO5 BuildNumberInfo ;
   long BuildNumber ;
}STAMP5;

#ifdef OLEDB5BUILD
   extern STAMP5 CustomerStamp ;
#endif

#endif
