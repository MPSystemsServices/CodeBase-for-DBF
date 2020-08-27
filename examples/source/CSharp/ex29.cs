using System ;
using CodeBase ;

namespace c4ap
{
   class ex29
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 data = new Data4( cb, "INFO.DBF" ) ;

         string st = data.alias ;
         string sp = "INFO" ;
         if( st == sp )
            Console.WriteLine( "This is always true" ) ;

         cb.initUndo() ;
      }
   }
}