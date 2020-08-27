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

		[STAThread]
		static void Main(string[] args)
		{
			codeBase.connect("","","","","");

			if ( codeBase.indexExtension().ToUpper().Equals( "NTX" ) )
			{
				codeBase.autoOpen = 0;
				codeBase.safety = 0;
				codeBase.accessMode = Code4.OPEN4DENY_RW ;

				Tag4info tagInfo = new Tag4info( codeBase ) ;
				tagInfo.add( "STU_NAME", "L_NAME+F_NAME", "", 0, 0 );
				tagInfo.add( "STU_AGE", "AGE", "", 0, 0 ) ;
				tagInfo.add( "STU_ID", "ID", "", 0, 0 ) ;

				dataFile.open( ref codeBase, "STUDENT.DBF" ) ;
				Index4 index  = new Index4() ;
				index.create( ref dataFile, "", ref tagInfo ) ;

				nameTag.init( dataFile, "STU_NAME" ) ;
				ageTag.init( dataFile, "STU_AGE" ) ;
				idTag.init(dataFile, "STU_ID" );

				codeBase.closeAll( ) ;
			}
			else
			{
				Console.Error.WriteLine( "This example is only for Clipper." ) ;
			}

			codeBase.initUndo( ) ;
		}
	}
}
