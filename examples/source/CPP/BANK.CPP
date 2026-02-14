// BANK.CPP
// Creates a new data file for TRANSFER.CPP

#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

Code4    codeBase ;
Data4    dataFile ;
Field4   acctNo, balance ;
Tag4     acctTag, balTag ;
FIELD4INFO  fieldInfo [] =
{
   {"ACCT_NO",r4num,5,0},
   {"BALANCE",r4num,8,2},
   {0,0,0,0},
};

TAG4INFO tagInfo [] =
{
   {"ACCT_TAG","ACCT_NO",0,0,0},
   {"BAL_TAG","BALANCE",0,0,0},
   {0,0,0,0,0},
};

void  OpenDataFile( void )
{
   dataFile.create( codeBase,"BANKDATA.DBF", fieldInfo, tagInfo ) ;

   acctNo.init( dataFile, "ACCT_NO" ) ;
   balance.init( dataFile, "BALANCE" ) ;

   acctTag.init( dataFile, "ACCT_TAG" ) ;
   balTag.init( dataFile, "BAL_TAG" ) ;
}

void PrintRecords( void )
{
   cout << "printrecords" << endl;
   for( int rc = dataFile.top( ); rc == r4success;
   rc = dataFile.skip( ))
   {
      cout << "-------------------------------" << endl ;
      cout << "Account Number: " << (long) acctNo << endl ;
      cout << "Balance       : " << (double) balance << endl ;
   }
}

void AddNewRecord(long acct, double bal )
{  dataFile.lockAll( ) ;
   dataFile.appendStart( ) ;
   dataFile.blank( ) ;

   acctNo.assignLong( acct ) ;
   balance.assignDouble( bal ) ;

   dataFile.append( ) ;
   dataFile.unlock( ) ;
}

int main( void )
{
   codeBase.errOpen = 1;
   codeBase.safety = 0;

   OpenDataFile( ) ;


   AddNewRecord(12345,600.00);

   AddNewRecord(55555,300.00);

   PrintRecords( ) ;

   codeBase.closeAll( ) ;
   codeBase.initUndo( ) ;
   return 0 ;
}

