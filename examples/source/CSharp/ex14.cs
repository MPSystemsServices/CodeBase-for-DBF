using System ;
using CodeBase ;

namespace c4ap
{
   class ex14
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         cb.readOnly = 1 ;
         Tag4 tag = new Tag4() ;

         // This example assumes that drive W is a read-only drive.
         Data4 prices = new Data4( cb, "w:\\datafile.DBF" ) ;
         cb.exitTest( ) ;

         tag.initFirst( prices ) ;
         prices.select( tag ) ;
         if( prices.seek( "SMITH" ) == 0 )
            Console.WriteLine( "SMITH is found" ) ;
         else
            Console.WriteLine( "SMITH is not found" ) ;

         prices.close( ) ;
         cb.initUndo( ) ;
      }
   }
}