using System;
using CodeBase;

namespace c4ap
{
	class BANK
	{
		static Code4 codeBase = new Code4() ;
		static Data4 dataFile = new Data4() ;
		static Field4 acctNo ;
		static Field4 balance ;
		static Tag4 acctTag = new Tag4() ;
		static Tag4 balTag = new Tag4() ;

		static Field4info fieldInfo = new Field4info( ref codeBase ) ;
		static Tag4info tagInfo = new Tag4info( codeBase ) ;

		static void  OpenDataFile( )
		{
			fieldInfo.add( "ACCT_NO", Code4.r4num, 5, 0, 0) ;
			fieldInfo.add( "BALANCE", Code4.r4num, 8, 2, 0) ;

			tagInfo.add( "ACCT_TAG", "ACCT_NO", "", 0, 0 ) ;
			tagInfo.add( "BAL_TAG",  "BALANCE", "", 0, 0 ) ;

			dataFile.create( ref codeBase, "BANKDATA.DBF", ref fieldInfo, ref tagInfo ) ;

			acctNo = new Field4( dataFile, "ACCT_NO" ) ;
			balance = new Field4( dataFile, "BALANCE" ) ;

			acctTag.init( dataFile, "ACCT_TAG" ) ;
			balTag.init( dataFile, "BAL_TAG" ) ;
		}

		static void PrintRecords( )
		{
			Console.WriteLine("printrecords\n") ;
			for( int rc = dataFile.top( ); rc == Code4.r4success;
			rc = dataFile.skip( ))
			{
				Console.WriteLine("-------------------------------\n" +
					"Account Number: {0}\nBalance       : {1}\n",
					acctNo.str( ), balance.str( ) ) ;
			}
		}

		static void AddNewRecord(int acct, double bal )
		{
			dataFile.lockAll( ) ;
			dataFile.appendStart( ) ;
			dataFile.blank( ) ;

			acctNo.assignInt( acct ) ;
			balance.assignDouble( bal ) ;

			dataFile.append( ) ;
			dataFile.unlock( ) ;
		}

		[STAThread]
		static void Main(string[] args)
		{
			codeBase.errOpen = 1;
			codeBase.safety = 0;

			OpenDataFile( ) ;

			AddNewRecord(12345,600.00);

			AddNewRecord(55555,300.00);

			PrintRecords( ) ;

			codeBase.closeAll( ) ;
			codeBase.initUndo( ) ;
		}
	}
}