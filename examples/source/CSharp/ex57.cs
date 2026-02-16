using System ;
using CodeBase ;

namespace c4ap
{
   class ex57
   {
      static int recsInFile( ref Code4 cb, string fileName )
      {
         Data4 data = new Data4( cb, fileName ) ;
         if ( cb.errorCode != 0 )
            return -1 ; // an error occurred

         int count = data.recCount( ) ; // save the record count

         data.close( ) ;               // close the data file
         return count ;
      }

      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         string name = "INFO" ;
         int rc = recsInFile( ref cb, name ) ;
         Console.WriteLine( rc + " records in the file" ) ;
         cb.initUndo( ) ;
      }
   }
}