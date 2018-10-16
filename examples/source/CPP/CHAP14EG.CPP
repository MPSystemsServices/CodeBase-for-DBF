//chap14eg.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

int openFileEx( Code4 &codeBase,Data4 data, char *name)
{
   int oldAccessMode = codeBase.accessMode ;
   codeBase.accessMode = OPEN4DENY_RW ;

   data.open( codeBase, name ) ;

   codeBase.accessMode = oldAccessMode ;

   return data.isValid( ) ;
}

void main( )
{
   Code4 cb ;
   Data4 db ;
   openFileEx( cb, db, "INFO" ) ;
   cb.closeAll( ) ;
   cb.initUndo( ) ;
}
