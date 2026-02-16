using System;
using CodeBase;
 
namespace c4ap
{
   class ex140
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         Data4 enroll = new Data4( cb, "ENROLL" ) ;
         Data4 master = new Data4( cb, "CLASSES" ) ;
         Data4 student = new Data4( cb, "STUDENT" ) ;
 
         Tag4 enrollTag = new Tag4( enroll, "ENR_CODE" ) ;
         Tag4 studentTag = new Tag4( student, "STU_ID" ) ;
 
         Relate4set MasterRelation = new Relate4set( master ) ;
         Relate4 relation1 = new Relate4( MasterRelation, enroll, "CODE", enrollTag ) ;
         Relate4 relation2 = new Relate4( relation1, student, "STU_ID_TAG", studentTag ) ;
 
         relation1.type( Code4.relate4scan ) ;
         MasterRelation.sortSet( "STUDENT->L_NAME,8,0)+ENROLL->CODE" ) ;
 
         Field4 classCode = new Field4( master, "CODE" ) ;
         Field4 classTitle = new Field4( master, "TITLE" ) ;
         Field4 enrollStudentId = new Field4( enroll, "STU_ID_TAG") ;
         Field4 studentName = new Field4( student, "L_NAME" ) ;
 
         cb.exitTest( ) ;
 
         for(int rc = MasterRelation.top( ); rc != Code4.r4eof
         ; rc = MasterRelation.skip( 1 ) )
         {
            Console.WriteLine( "{0} ", studentName.str( ) ) ; // only one Str4::str per stmt.
            Console.WriteLine( "{0} ", enrollStudentId.str( ) ) ;
            Console.WriteLine( "{0} ", classCode.str( ) ) ;
            Console.WriteLine( "{0}\n", classTitle.str( ) ) ;
         }
 
         Console.WriteLine( "Number of records in {0} {1}", master.alias, master.recCount( ) ) ;
 
         MasterRelation.free( ) ;
         cb.initUndo( ) ;
      }
   }
}