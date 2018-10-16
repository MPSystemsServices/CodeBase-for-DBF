/***********************************************************************\
*                                                                       *
*   RELATE3.C     Copyright (C) 1999 Sequiter Software Inc.             *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 6 */

#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

CODE4   codeBase;
DATA4   *student,*enrollment,*classes;
FIELD4  *studentId,*firstName,*lastName,*age,*classCode,*classTitle;
TAG4    *codeTag,*idTag;
RELATE4 *classRel,*studentRel,*enrollRel;

void printStudents(void) ;
void openDataFiles(void) ;

void openDataFiles(void)
{

   code4init(&codeBase);

   student = d4open(&codeBase,"student");
   enrollment = d4open(&codeBase,"enroll");
   classes = d4open(&codeBase,"classes");

   studentId = d4field(student,"ID");
   firstName = d4field(student,"F_NAME");
   lastName  = d4field(student,"L_NAME");
   age = d4field(student,"AGE");
   classCode = d4field(classes,"CODE");
   classTitle = d4field(classes,"TITLE");

   idTag = d4tag(student,"STU_ID");
   codeTag = d4tag(enrollment,"ENR_CODE");

   error4exitTest(&codeBase);
}

void printStudents(void)
{
   printf("        %s ",f4str(firstName));
   printf("%s ",f4str(lastName));
   printf("%s ",f4str(studentId));
   printf("%s \n",f4str(age));
}

void setRelation(void)
{
   classRel = relate4init(classes);

   enrollRel = relate4createSlave(classRel,enrollment,"CODE",codeTag);

   studentRel = relate4createSlave(enrollRel,student,"STU_ID_TAG",idTag);
}

void printStudentList(char *expr,long direction)
{
   int rc, endValue;

   relate4querySet(classRel,expr);
   relate4sortSet(classRel,"student->L_NAME + student->F_NAME");

   relate4type(enrollRel,relate4scan);

   if(direction > 0)
   {
      rc = relate4top(classRel);
      endValue = r4eof;
   }
   else
   {
      rc = relate4bottom(classRel);
      endValue = r4bof;
   }

   printf("\n%s",f4str(classCode));
   printf("  %s\n",f4str(classTitle));

   for (;rc != endValue; rc = relate4skip(classRel,direction))
      printStudents();
}

void main(void)
{
   openDataFiles();

   setRelation();

   printStudentList("CODE = 'MATH114 '",1L);
   printStudentList("CODE = 'CMPT411 '",-1L);

   code4unlock(&codeBase);
   relate4free(classRel,0);

   code4close(&codeBase);
   code4initUndo(&codeBase);
   exit(1);
}
