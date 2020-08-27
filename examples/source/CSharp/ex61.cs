using System ;
using CodeBase ;

namespace c4ap
{
   class ex61
   {
      static void update( Data4 d )
      {
         if ( d.changed != 0 )
            Console.WriteLine( "Changes not discarded." ) ;
         else
            d.refreshRecord( ) ;
      }

      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 data = new Data4( cb, "INFO" ) ;
         data.top( ) ;
         data.lockRecord(1) ;
         Field4 field = new Field4( data, "NAME" ) ;
         field.assign( "Marvin" ) ;
         update( data ) ;
         data.skip( ) ;
         update( data ) ;
         cb.initUndo( ) ;
      }
   }
}