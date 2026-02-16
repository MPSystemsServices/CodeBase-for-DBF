using System;
using CodeBase;

namespace c4ap
{
	class DELETION
	{
		static void printDeleteStatus(int status,long recNo)
		{
			if(status != 0)
				Console.WriteLine( "Record {0} - DELETED\n", recNo ) ;
			else
				Console.WriteLine( "Record {0} - NOT DELETED\n", recNo ) ;
		}

		static void printRecords( Data4 dataFile)
		{
			int  rc = 0, status;
			int recNo;

			Console.WriteLine( "\n" ) ;

			dataFile.top( ) ;
			while( rc != Code4.r4eof )
			{
				recNo = dataFile.recNo( ) ;
				status = dataFile.deleted( ) ;
				printDeleteStatus( status, recNo ) ;
				rc = dataFile.skip( ) ;
			}
		}

		[STAThread]
		static void Main(string[] args)
		{
			Code4 codeBase = new Code4();
			Data4 dataFile = new Data4();
			Field4info fieldInfo = new Field4info( ref codeBase ) ;
			fieldInfo.add( "DUMMY", 'C', 10, 0, 0 ) ;
			fieldInfo.add( "MEMO", 'M', 10, 0, 0 ) ;

			int count;

			codeBase.safety = 0 ;
			codeBase.errCreate = 0 ;
			codeBase.lockEnforce = 1 ;

			dataFile.create(ref codeBase, "TUTOR5", ref fieldInfo ) ;
			codeBase.exitTest( ) ;

			for(count = 0; count < 5; count ++)
				dataFile.appendBlank( ) ;

			printRecords( dataFile ) ;

			dataFile.lockAll( ) ;
			dataFile.go( 3 ) ;
			dataFile.deleteRec( ) ;
			dataFile.go( 1 ) ;
			dataFile.deleteRec( ) ;
			printRecords( dataFile ) ;

			dataFile.go( 3 ) ;
			dataFile.recall( ) ;
			printRecords( dataFile ) ;

			dataFile.pack( ) ;
			dataFile.memoCompress( ) ;
			printRecords( dataFile ) ;

			codeBase.closeAll( ) ;
			codeBase.initUndo( ) ;
		}
	}
}