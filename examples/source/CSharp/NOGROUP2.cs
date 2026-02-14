using System;
using CodeBase;

namespace c4ap
{
	class NOGROUP
	{
		static Code4  codeBase  = new Code4() ;
		static Data4  dataFile = new Data4() ;
		static Tag4   nameTag = new Tag4() ;
		static Tag4   ageTag = new Tag4() ;
		static Tag4   idTag = new Tag4() ;

		static void printRecords( )
		{

		}

		[STAThread]
		static void Main(string[] args)
		{
			codeBase.connect("","","","","") ;

			if ( codeBase.indexExtension().ToUpper().Equals( "NTX" ) )
			{
				codeBase.autoOpen = 0 ;
				codeBase.safety = 0 ;

				dataFile.open( ref codeBase, "STUDENT.DBF" ) ;

				nameTag.open( dataFile, "STU_NAME" ) ;
				ageTag.open( dataFile, "STU_AGE" ) ;
				idTag.open( dataFile, "STU_ID" ) ;

				dataFile.select( nameTag ) ;
				printRecords();

				codeBase.closeAll( ) ;
			}
			else
				Console.Error.WriteLine( "This example is only for Clipper." ) ;

			codeBase.initUndo( ) ;
		}
	}
}