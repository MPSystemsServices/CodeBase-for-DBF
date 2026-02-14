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
}STAMP5;

#ifdef OLEDB5BUILD
   extern STAMP5 CustomerStamp ;
#endif

#endif
