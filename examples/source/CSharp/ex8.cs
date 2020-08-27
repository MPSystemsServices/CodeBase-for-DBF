using System ;
using CodeBase ;

namespace c4ap
{
   class ex8
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         cb.errOpen = 0 ;
         // no error message is displayed if NO_FILE does not exist
         Data4 data = new Data4( cb, "NO_FILE" ) ;

         if( data.isValid() == 0 )
         {
            // Data file does not exist
            cb.safety = 0 ;
            Field4info fields = new Field4info( ref cb ) ;
            fields.add( "NAME_FLD", 'C', 20, 0, 0 ) ;
            fields.add( "AGE_FLD", 'N', 3, 0, 0 ) ;
            data.create( ref cb, "NO_FILE", ref fields ) ;
            if( data.isValid( ) == 0 )
               Console.WriteLine( "Could not create NO_FILE" ) ;
         }
         cb.initUndo( ) ;
      }
   }
}