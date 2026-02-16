using System ;
using CodeBase ;

namespace c4ap
{
   class ex48
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 data = new Data4( cb, "INFO" ) ;

         /* Lock all of the records in the data file as well as
            the append bytes all at once.  Existing locks are
            removed. */

         int rc = data.lockFile( ) ;
         if ( rc == Code4.r4success )
         {
            Console.Write( "Other users can read this data file, " ) ;
            Console.Write( "but can make no modifications until\n" ) ;
            Console.WriteLine( "this lock is removed." ) ;
         }

         cb.initUndo( ) ; // implicitly unlock the file.
      }
   }
}