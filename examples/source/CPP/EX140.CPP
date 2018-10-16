// ex140.cpp
#include "d4all.hpp"

extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   Data4 enroll( cb, "ENROLL" ) ;
   Data4 master( cb, "CLASSES" ) ;
   Data4 student( cb, "STUDENT" ) ;

   Tag4 enrollTag( enroll, "ENR_CODE" ) ;
   Tag4 studentTag( student, "STU_ID" ) ;

   Relate4set MasterRelation( master ) ;
   Relate4 relation1( MasterRelation, enroll, "CODE", enrollTag ) ;
   Relate4 relation2( relation1, student, "STU_ID_TAG", studentTag ) ;

   relation1.type( relate4scan ) ;
   MasterRelation.sortSet( "LEFT(STUDENT->L_NAME,8)+ENROLL->CODE" ) ;

   Field4 classCode( master, "CODE" ) ;
   Field4 classTitle( master, "TITLE" ) ;
   Field4 enrollStudentId( enroll, "STU_ID_TAG") ;
   Field4 studentName( student, "L_NAME" ) ;

   cb.exitTest( ) ;

   for(int rc = MasterRelation.top( ); rc != r4eof
   ; rc = MasterRelation.skip( ) )
   {
      cout << studentName.str( ) << " " ; // only one Str4::str per stmt.
      cout << enrollStudentId.str( ) << " " ;
      cout << classCode.str( ) << " " ;
      cout << classTitle.str( ) << endl ;
   }

   cout << "Number of records in " << master.alias( ) << " "
        << master.recCount( ) << endl ;

   MasterRelation.free( ) ;
   cb.initUndo( ) ;
}
