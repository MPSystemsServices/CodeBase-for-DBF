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
*   D4TEST.CPP     Copyright (C) 1998 Sequiter Software Inc.            *
*                                                                       *
\***********************************************************************/

#include "d4all.hpp"

void AddNewRecord(char *, char *, char *, int, int, double, char * ) ;
void ConnectToServer( char *, char * ) ;
int  OpenDataFile( void ) ;
void ParseCmdLine( int ) ;
void PrintRecords( void ) ;

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif


Code4       codeBase;
Data4       dataFile = 0;
Field4memo  fName, lName, address, age, married, 
	    amount, comment;


FIELD4INFO  field_info [] =
{
   {"F_NAME",r4str,10,0},      
   {"L_NAME",r4str,10,0},
   {"ADDRESS",r4str,15,0},
   {"AGE",r4num,2,0},          
   {"MARRIED",r4log,1,0},      
   {"AMOUNT",r4num,7,2},
   {"COMMENT",r4memo,10,0},    
   {0,0,0,0},
};
	 

void AddNewRecord(char *fNameStr
		 ,char*lNameStr
		 ,char *addressStr
		 ,int ageValue
		 ,int marriedValue
		 ,double amountValue
		 ,char *commentStr)
{

   dataFile.appendStart(0);

   fName.assign(fNameStr);
   lName.assign(lNameStr);
   address.assign(addressStr);

   age.assignLong(ageValue);

   if(marriedValue)
      married.assign("T");
   else
      married.assign("F");

   amount.assignDouble(amountValue);
   comment.assign(commentStr);

   dataFile.append();
}


void ConnectToServer( char * serverName, char *processId ) 
{
   int rc ;

   printf( "\nAttempting to connect to server ..." ) ;

   rc = codeBase.connect(serverName, processId);

   if ( rc != r4success )
   {
      printf("\nD4TEST could not connect to the server. Ensure that " 
	     "the CodeBase database \nserver is running. If the "
	     "server is running, check that you have specified the\n" 
	     "correct server name, that your network software "
	     "supports the current protocol,\nand that you have the "
	     "necessary software drivers loaded for that protocol.\n"  );

      codeBase.initUndo() ;
      exit(1) ;
   }
   else
      printf( "\nSuccessful connection.\n\n" ) ;
}

int OpenDataFile( )
{
	if( dataFile.create( codeBase,"C4TEST.DBF",field_info,0) == r4success )
   {    
	fName.init(dataFile,"F_NAME");
	lName.init(dataFile,"L_NAME");
	address.init(dataFile,"ADDRESS");
	age.init(dataFile,"AGE");
	married.init(dataFile,"MARRIED");
	amount.init(dataFile,"AMOUNT");
	comment.init(dataFile,"COMMENT");
		
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
	 printf( "\nUsage:    D4TEST <Host Name> [Process ID]\n" ) ;
      #else
	 printf( "\nUsage:    D4TEST <Server Name>\n" ) ;
      #endif
      
      exit(1);
   }
}


void PrintRecords( )
{

   Date4 bDate;
   Str4ten purchased;
   Str4large name;

   for( int rc = dataFile.top();rc == r4success
		      ;rc = dataFile.skip(1L))
   {
      name.assign(fName);
      name.trim();
      name.add(" ");
      name.add(lName);
      purchased.assignDouble((double)amount, 8, 2);

      cout << "------------------------------" << endl;
      cout << "Name     : " << name.ptr() << endl;
      cout << "Address  : " << address.str() << endl;
      cout << "Age : " << (int) age << " Married : " 
	   << married.str( ) << endl;
      cout << "Comment: " << comment.str() << endl;
      cout << "Purchased this year:$" << purchased.ptr() << endl;
      cout << endl;
   }
}


int main( int argc, char **argv )
{

   ParseCmdLine( argc ) ;

   codeBase.init();

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
	
	codeBase.closeAll();
   }
   
   cout <<  "End of connection test" << endl ;

   codeBase.initUndo() ;

   return 0;
}


