using System ;
using CodeBase ;

namespace c4ap
{
   class ex1
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 codeBase = new Code4() ;
         codeBase.accessMode = Code4.OPEN4DENY_RW ;
         codeBase.safety = 0 ;  // Ensure the create overwrites any existing file

         Field4info fields = new Field4info( ref codeBase )  ;

         fields.add( "NAME", 'C', 20, 0, 0 ) ;
         fields.add( "AGE", 'N', 3, 0, 0 ) ;
         fields.add( "BIRTHDATE", 'D', 8, 0, 0 ) ;

         Data4 newDataFile = new Data4() ;
         newDataFile.create( ref codeBase, "NEWDBF", ref fields ) ;

         newDataFile.close( ) ;

         // open in shared mode
         codeBase.accessMode = Code4.OPEN4DENY_NONE ;
         newDataFile.open( ref codeBase, "NEWDBF" ) ;

         // ... some other code ...

         codeBase.closeAll( ) ;
         codeBase.initUndo( ) ;
      }
   }
}