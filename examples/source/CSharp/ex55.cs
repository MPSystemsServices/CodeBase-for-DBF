using System ;
using CodeBase ;

namespace c4ap
{
   class ex55
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 data = new Data4( cb, "INFO" ) ;
         Tag4 firstTag = new Tag4() ;
         firstTag.initFirst( data ) ;

         cb.exitTest( ) ;

         data.select( firstTag ) ;  // select the first tag of the first open index.
         data.position = .25 ;   // move one quarter through the index file.
         Console.WriteLine( "Record number: " + data.recNo() ) ;

         Console.WriteLine( "The current position is: " + data.position ) ;

         cb.initUndo( ) ;
      }
   }
}