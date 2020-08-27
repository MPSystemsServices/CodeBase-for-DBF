using System;
using CodeBase;
 
namespace c4ap
{
   class ex142
   {
      static void listFilesInRelation( Relate4set rel )
      {
         Relate4iterator relation = new Relate4iterator( rel ) ;
 
         for( ; relation.isValid( ) != 0 ; relation.next( ) )
            Console.WriteLine( "{0}\n", relation.data( ).alias ) ;
      }
 
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         Data4 enroll = new Data4( cb, "ENROLL" ) ;
         Data4 master = new Data4( cb, "CLASSES" ) ;
         Data4 student = new Data4( cb, "STUDENT" ) ;
 
         Tag4 enrollTag = new Tag4( enroll, "ENR_CODE" ) ;
         Tag4 studentTag = new Tag4( student, "STU_ID" ) ;
         Tag4 classTag = new Tag4( master, "CODE_TAG" ) ;
 
         Relate4set MasterRelation = new Relate4set( master ) ;
         Relate4 relation1 = new Relate4( MasterRelation, enroll, "CODE", enrollTag ) ;
         Relate4 relation2 = new Relate4( relation1, student, "STU_ID_TAG", studentTag ) ;
 
         relation1.type( Code4.relate4scan ) ;
 
         listFilesInRelation( MasterRelation ) ;
 
         MasterRelation.free( ) ;
         cb.initUndo( ) ;
      }
   }
}