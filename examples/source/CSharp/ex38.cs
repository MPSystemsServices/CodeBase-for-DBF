using System ;
using CodeBase ;

namespace c4ap
{
   class ex38
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 codeBase = new Code4() ;
         Data4 file = new Data4( codeBase, "INFO" ) ;

         codeBase.optStart( ) ;

         int count = 0 ;

         for( file.top( ) ; file.eof( ) == 0 ; file.skip( 1 ) )
            if( file.deleted( ) != 0 )
               count++ ;

         Console.WriteLine( "\"INFO\" has " + count + " deleted records" ) ;

         codeBase.initUndo( ) ;
      }
   }
}