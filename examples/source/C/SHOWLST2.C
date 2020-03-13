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

/***********************************************************************\
*                                                                       *
*   SHOWLST2.C    Copyright (C) 1999 Sequiter Software Inc.             *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 4 */

#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

CODE4 codeBase;
DATA4 *dataFile = 0;
FIELD4 *item, *minOnHand, *onHand;
TAG4 *itemTag, *minOnHandTag, *onHandTag, *onHandLowTag;

TAG4INFO  tagInfo[] =
{
   {"I_ITEM","ITEM",".NOT. DELETED()",0,0},
   {"I_MIN","MIN_ON_HND",0,0,0},
   {"I_ONHAND","ON_HAND",0,0,0},
   {"I_ONHNDL","ON_HAND","ON_HAND < MIN_ON_HND",0,0},
   {0,0,0,0,0},
};

void OpenDataFile(void)
{
   codeBase.autoOpen = 0;
   codeBase.safety = 0;
   codeBase.accessMode = OPEN4DENY_RW;

   dataFile = d4open(&codeBase,"invent.dbf");

   i4create(dataFile,NULL,tagInfo);

   item = d4field(dataFile,"ITEM");
   minOnHand = d4field(dataFile,"MIN_ON_HND");
   onHand = d4field(dataFile,"ON_HAND");

   itemTag = d4tag(dataFile,"I_ITEM");
   minOnHandTag = d4tag(dataFile,"I_MIN");
   onHandTag = d4tag(dataFile,"I_ONHAND");
   onHandLowTag = d4tag(dataFile,"I_ONHNDL");
}

void PrintRecords(void)
{
   int rc, onHandValue, minOnHandValue;
   char itemStr[21];

   printf("Order by tag: %s ---------------------------\n", t4alias(d4tagSelected(dataFile)));
   for(rc = d4top(dataFile);rc == r4success;rc = d4skip(dataFile, 1L))
   {
      f4ncpy(item,itemStr,sizeof(itemStr));
      onHandValue = f4int(onHand);
      minOnHandValue = f4int(minOnHand);

      printf("Item: %20s",itemStr);
      printf("On Hand: %4d ",onHandValue);
      printf("Minimum: %4d ",minOnHandValue);
      printf("\n");
   }
}

int main(void)
{
   code4init(&codeBase);

   OpenDataFile();

   d4tagSelect(dataFile,itemTag);
   PrintRecords();

   d4tagSelect(dataFile,onHandTag);
   PrintRecords();

   d4tagSelect(dataFile,minOnHandTag);
   PrintRecords();

   d4tagSelect(dataFile,onHandLowTag);
   PrintRecords();

   code4close(&codeBase);
   code4initUndo(&codeBase);
   return 0;
}
