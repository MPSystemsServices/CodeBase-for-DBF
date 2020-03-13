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
*   LIST1.C    Copyright (C) 1999 Sequiter Software Inc.             *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 9 */

#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

typedef struct
{
   LINK4   link;
   int     age;
} AGES;

void printList(LIST4 *);

void main(void)
{

   LIST4   ageList;
   AGES    firstAge,middleAge,lastAge;

   memset(&ageList,0,sizeof(ageList));

   firstAge.age = 3;
   middleAge.age = 5;
   lastAge.age = 7;

   l4add(&ageList,&middleAge);
   l4addBefore(&ageList,&middleAge,&firstAge);
   l4addAfter(&ageList,&middleAge,&lastAge);

   printList(&ageList);

   l4remove(&ageList,(void *) &middleAge);

   printList(&ageList);

   l4pop(&ageList);

   printList(&ageList);
}

void printList(LIST4 *list)
{
   AGES    *agePtr;

   printf("\nThere are %d links\n",l4numNodes(list));

   agePtr =(AGES *) l4first(list);
   while(agePtr != NULL)
   {
      printf("%d\n",agePtr->age);
      agePtr = (AGES *) l4next(list,agePtr);
   }

   agePtr = (AGES *) list->selected;
}
