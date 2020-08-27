using System ;
using CodeBase ;

namespace c4ap
{
   class ex37
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 data = new Data4() ;
         Data4 secondFile = new Data4() ;
         Field4info fieldsFirst = new Field4info( ref cb ) ;
         fieldsFirst.add( "NAME_FIELD", 'C', 20, 0, 0 ) ;
         fieldsFirst.add( "AGE_FIELD", 'N', 3, 0, 0 ) ;
         fieldsFirst.add( "BIRTH_DATE", 'D', 8, 0, 0 ) ;

         data.create( ref cb, "FIRSTDBF", ref fieldsFirst ) ;
         cb.exitTest( ) ;

         // initialize fields object with the fields of FIRSTDBF.DBF
         Field4info fields = new Field4info( data ) ;
         // add a new field
         fields.add( "NEW_FLD", 'C', 20, 0, 0 ) ;

         cb.safety = 0 ;       // overwrite the file if it exists
         secondFile.create( ref cb, "NEWDBF", ref fields ) ;

         if( cb.errorCode != 0 )
            Console.WriteLine( "An error occurred, NEWDBF not created" ) ;
         else
            Console.WriteLine( "Created successfully!" ) ;

         cb.closeAll( ) ;
         cb.initUndo( ) ;
      }
   }
}