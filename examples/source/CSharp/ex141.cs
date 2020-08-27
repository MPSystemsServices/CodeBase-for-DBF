using System;
using CodeBase;
 
namespace c4ap
{
   class ex141
   {
      static int seekMaster( Relate4 r, Tag4 masterTag, string seekKey )
      {
         Data4 master = r.master( ).data( ) ;
         master.select( masterTag ) ;
         int rc = master.seek( seekKey ) ; // seek for the requested value
 
         if( rc == Code4.r4success )
            r.doOne( ) ; // position the slave data file to the appropriate
         // record
         return rc ;
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
 
         seekMaster( relation1, classTag, "CMPT401" ) ;
         MasterRelation.free( ) ;
         cb.initUndo( ) ;
      }
   }
}