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
*   D4TEST.C     Copyright (C) 1998 Sequiter Software Inc.             *
*                                                                       *
\***********************************************************************/

#include "d4all.h"

void AddNewRecord(char *, char *, char *, int, int, double, char * ) ;
void ConnectToServer( char *, char * ) ;
int  OpenDataFile( void ) ;
void ParseCmdLine( int ) ;
void PrintRecords( void ) ;

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif


CODE4    codeBase;
DATA4    *dataFile = 0;
FIELD4   *fName, *lName, *address, *age, *birthDate, *married,
	 *amount, *comment;


FIELD4INFO  field_info [] =
{
   {"F_NAME",r4str,10,0},      
   {"L_NAME",r4str,10,0},
   {"ADDRESS",r4str,15,0},
   {"AGE",r4num,2,0},          
   {"BIRTH_DATE",r4date,8,0},
   {"MARRIED",r4log,1,0},      
   {"AMOUNT",r4num,7,2},
   {"COMMENT",r4memo,10,0},    
   {0,0,0,0},
};


void AddNewRecord(char *fNameStr
		 ,char*lNameStr
		 ,char *addressStr
		 ,int ageValue
		 ,int married_value
		 ,double amountValue
		 ,char *commentStr)
{

   d4appendStart(dataFile,0);

   f4assign(fName,fNameStr);
   f4assign(lName,lNameStr);
   f4assign(address,addressStr);

   f4assignInt(age,ageValue);

   if(married_value)
      f4assign(married,"T");
   else
      f4assign(married,"F");

   f4assignDouble(amount,amountValue);
   f4memoAssign(comment,commentStr);

   d4append(dataFile);
}


void ConnectToServer( char *serverName, char *processId ) 
{
   int rc ;

   printf( "\nAttempting to connect to server ..." ) ;

   rc = code4connect( &codeBase, serverName, processId, NULL, NULL, NULL ) ;

   if ( rc != r4success )
   {
      printf("\nD4TEST could not connect to the server. Ensure that " 
	     "the CodeBase database \nserver is running. If the "
	     "server is running, check that you have specified the\n" 
	     "correct server name, that your network software "
	     "supports the current protocol,\nand that you have the "
	     "necessary software drivers loaded for that protocol.\n"  );

      code4initUndo( &codeBase ) ;
      exit(1) ;
   }
   else
      printf( "\nSuccessful connection.\n\n" ) ;
}

int OpenDataFile( )
{
   dataFile = d4create(&codeBase,"C4TEST.DBF",field_info,0);
   
   if( dataFile )
   {    
	fName = d4field(dataFile,"F_NAME");
	lName = d4field(dataFile,"L_NAME");
	address = d4field(dataFile,"ADDRESS");
	age = d4field(dataFile,"AGE");
	birthDate = d4field(dataFile,"BIRTH_DATE");
	married = d4field(dataFile,"MARRIED");
	amount = d4field(dataFile,"AMOUNT");
	comment = d4field(dataFile,"COMMENT");
		
		return 1 ;
	}
	
	return 0 ;
}

void ParseCmdLine( int argc ) 
{
   if( argc < 2 )
   {
      printf( "\nInvalid command-line.\n" ) ;
      #ifdef S4WIN32
	 printf( "\nUsage:    D4TEST32 <Host Name> [Process ID]\n" ) ;
      #else
	 printf( "\nUsage:    D4TEST <Server Name>\n" ) ;
      #endif
      exit(1) ;
   }
}


void PrintRecords( )
{

   int        rc,ageValue;
   double     amountValue;
   char       nameStr[25];
   char       addressStr[20];
   char       dateStr[9];
   char       marriedStr[2];
   const char *commentStr;

   for(rc = d4top(dataFile);rc == r4success
		      ;rc = d4skip(dataFile, 1L))
   {
      u4ncpy(nameStr,f4str(fName)
			     ,sizeof(nameStr));
      u4ncpy(addressStr,f4str(address)
			   ,sizeof(addressStr));
      ageValue = f4int(age);
      amountValue = f4double(amount);
 
      u4ncpy(dateStr,f4str(birthDate)
			      ,sizeof(dateStr));
      u4ncpy(marriedStr,f4str(married),
			    sizeof(marriedStr));
      commentStr = f4memoStr(comment);

      printf("------------------------------\n");
      printf("Name     : %20s\n",nameStr);
      printf("Address  : %15s\n",addressStr);
      printf("Age : %3d   Married : %1s\n"
			  ,ageValue,marriedStr);
      printf("Comment: %s\n",commentStr);
      printf("Purchased this year : $%5.2lf \n"
				   ,amountValue);

      printf("\n");
   }
}


int main( int argc, char **argv )
{

   ParseCmdLine( argc ) ;

   code4init(&codeBase);

   ConnectToServer( argv[1], argv[2] );

   codeBase.safety = 0;

   if( OpenDataFile() )
	{
	AddNewRecord("Sarah"
		 ,"Webber"
			 ,"132-43 St."
			 ,32
			 ,1
			 ,147.99
			 ,"New Customer");

	AddNewRecord("John"
		  ,"Albridge"
		  ,"1232-76 Ave."
		  ,12
		  ,0
		  ,98.99
		  ,"");
	
	PrintRecords();
	
	code4close(&codeBase);
   }
   
   printf( "End of connection test.\n" ) ;

   code4initUndo( &codeBase ) ;

   return 0;
}


