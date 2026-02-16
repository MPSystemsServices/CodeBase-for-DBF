using System ;
using CodeBase ;

namespace c4ap
{
   class ex58
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 data = new Data4( cb, "INFO" ) ;
         Tag4 firstTag = new Tag4() ;
         firstTag.initFirst( data ) ;

         data.select( firstTag ) ; //select the first tag of the first opened index

         int count = 0 ;
         for( data.top( ); data.eof( ) == 0 ; data.skip( 1 ) )
         {
            Console.Write( "position in the tag: " + ++count ) ;
            Console.WriteLine( "      Record Position: " + data.recNo( ) ) ;
         }
         cb.initUndo( ) ;
      }
   }
}