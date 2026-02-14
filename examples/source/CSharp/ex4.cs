using System ;
using CodeBase ;

namespace c4ap
{
   class ex4
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;

         // Do not add duplicate records to unique tags or the data file and 
         // return r4unique when attempted.
         cb.errDefaultUnique = Code4.r4unique ; 

         Data4 data = new Data4( cb, "INFO" ) ;

         data.top( ) ;
         data.appendStart( ) ;     
         int rc = data.append( ) ; // append a duplicate copy of the top record
   
         if ( rc == Code4.r4unique ) 
            Console.WriteLine( "Attempt to add a duplicate record failed." ) ;
         else
         {
            Console.WriteLine( "Attempt to add a duplicate record succeeded" ) ;
            Console.WriteLine( "Record in both data and index file" ) ;
         }

         data.close( ) ;
         cb.initUndo( ) ;
      }
   }
}