using System ;
using CodeBase ;

namespace c4ap
{
   class ex15
   {
      static int createFiles( ref Code4 cb )
      {
         Field4info fields = new Field4info( ref cb ) ;

         fields.add( "NAME", 'C', 20, 0, 0 ) ;
         fields.add( "AGE", 'N', 3, 0, 0 ) ;
         fields.add( "BIRTHDATE", 'D', 0, 0, 0 ) ;

         Tag4info tags = new Tag4info( cb ) ;
         tags.add( "INF1_NME", "NAME", "", 0, 0 ) ;

         cb.safety = 0 ; // Turn off safety -- overwrite files
         Data4 data = new Data4() ;
         data.create( ref cb, "INFO1.DBF", ref fields, ref tags ) ;

         return cb.errorCode ;
      }

      [STAThread]
      static void Main(string[] args)
      {
         Code4 code = new Code4() ;
         createFiles( ref code ) ;
         code.initUndo( ) ;
      }
   }
}