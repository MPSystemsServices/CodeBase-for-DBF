using System ;
using CodeBase ;

namespace c4ap
{
   class ex45
   {
      static void displayAlias( Data4 d )
      {
         if ( d.isValid( ) != 0 )
            Console.WriteLine( d.alias + " is valid." ) ;
      }

      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 data = new Data4( cb, "INFO" ) ;

         displayAlias( data ) ;
         cb.initUndo( ) ;
      }
   }
}