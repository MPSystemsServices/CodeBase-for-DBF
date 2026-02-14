using System ;
using CodeBase ;

namespace c4ap
{
   class ex23
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 data1 = new Data4( cb, "DATA1" ) ;
         Data4 data2 = new Data4( cb, "DATA2" ) ;

         data1.top( ) ;
         data2.top( ) ;

         data1.lockAddFile( ) ;
         data2.lockAddAppend( ) ;

         int numRecords = data2.recCount( ) ;
         data2.lockAdd( numRecords ) ;
         data2.lockAdd( numRecords-1 ) ;
         data2.lockAdd( numRecords-2 ) ;

         if( cb.lockGroup( ) == Code4.r4success )
            Console.WriteLine( "All locks were successfully performed" ) ;

         cb.initUndo( ) ;
      }
   }
}