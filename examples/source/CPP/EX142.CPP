//ex142.cpp
#include "d4all.hpp"

extern unsigned _stklen = 10000 ;

void listFilesInRelation( Relate4set rel )
{
   Relate4iterator relation( rel ) ;

   for( relation ; relation.isValid( ) ; relation.next( ) )
      cout << endl << relation.data( ).alias( ) << endl ;
}

void main( )
{
   Code4 cb ;
   Data4 enroll( cb, "ENROLL" ) ;
   Data4 master( cb, "CLASSES" ) ;
   Data4 student( cb, "STUDENT" ) ;

   Tag4 enrollTag( enroll, "ENR_CODE" ) ;
   Tag4 studentTag( student, "STU_ID" ) ;
   Tag4 classTag( master, "CODE_TAG" ) ;

   Relate4set MasterRelation( master ) ;
   Relate4 relation1( MasterRelation, enroll, "CODE", enrollTag ) ;
   Relate4 relation2( relation1, student, "STU_ID_TAG", studentTag ) ;

   relation1.type( relate4scan ) ;

   listFilesInRelation( MasterRelation ) ;

   MasterRelation.free( ) ;
   cb.initUndo( ) ;
}
