//ex124.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

typedef struct myStructSt
{
   char id[6] ;
   int checkSum ;
   char password[15] ;
} MY_STRUCT ;

int readUserInfo( File4 &file, MY_STRUCT *ms, int user)
{
   int rc = file.readAll( user*sizeof(MY_STRUCT), ms, sizeof(MY_STRUCT) ) ;
   if( rc != 0 )
   {
      cout << "Could not read user #" << user << endl ;
      return rc ;
   }
   return 0 ;
}

void main( )
{
   Code4 cb ;
   File4 file( cb, "PASS.FIL" ) ;
   MY_STRUCT info ;
   for (int user = 0 ; user <= 2 ; user++ )
      readUserInfo( file, &info, user ) ;
   cb.initUndo( ) ;
}
