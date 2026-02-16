using System;
using CodeBase;

namespace c4ap
{
	class TRANSFER
	{
		static Code4 codeBase  = new Code4() ;
		static Data4 dataFile  = new Data4() ;
		static Field4 acctNo ;
		static Field4 balance ;
		static Tag4 acctTag = new Tag4() ;
		static Tag4 balTag = new Tag4();

		#if !(S4CLIENT)
		static void OpenLogFile(  )
		{
			int rc ;

			rc = codeBase.logOpen( "", "user1" );
			if ( rc == Code4.r4noOpen )
			{
				codeBase.errorCode = 0 ;
				codeBase.logCreate( "", "user1" ) ;
			}
		}
		#endif

		static void OpenDataFile( )
		{
			dataFile.open( ref codeBase, "bank.dbf" ) ;
			acctNo = new Field4( dataFile, "ACCT_NO" ) ;
			balance = new Field4( dataFile, "BALANCE" ) ;

			acctTag.init( dataFile, "ACCT_TAG" ) ;
			balTag.init( dataFile, "BAL_TAG" ) ;
		}

		static int Credit( int toAcct, double amt )
		{
			dataFile.select( acctTag ) ;
			int rc = dataFile.seek( toAcct ) ;

			if (rc != Code4.r4success ) return rc;
			double newBal = balance.getDouble() + amt ;
			balance.assignDouble( newBal ) ;
			return Code4.r4success ;
		}

		static int Debit( int fromAcct, double amt )
		{
			return Credit( fromAcct, -amt ) ;
		}

		static void Transfer( int fromAcct, int toAcct, double amt )
		{
			dataFile.lockAll( ) ;
			codeBase.tranStart() ;

			int rc1 = Debit( fromAcct, amt ) ;
			int rc2 = Credit( toAcct, amt );

			if (rc1 == Code4.r4success && rc2 == Code4.r4success)
				codeBase.tranCommit() ;
			else
				codeBase.tranRollback() ;
			dataFile.unlock( ) ;
		}

		static void PrintRecords( )
		{
			for( int rc = dataFile.top(); rc == Code4.r4success; rc = dataFile.skip())
			{
				Console.WriteLine( "-----------------------------------\n" +
					"Account Number: {0}\nBalance       : {1}\n", acctNo.getInt(), balance.getDouble() ) ;
			}
			Console.WriteLine( "=========================================" ) ;
		}

		[STAThread]
		static void Main(string[] args)
		{
			codeBase.errOpen = 0 ;
			codeBase.safety = 0 ;
			codeBase.lockAttempts = 5 ;
			codeBase.lockEnforce = 1 ;

			#if !(S4CLIENT)
				OpenLogFile() ;
			#endif

			OpenDataFile() ;

			PrintRecords() ;

			// The account number 56789 doesn't exist in the database,
			// the transfer is aborted and database is not affected

			Transfer( 12345, 56789, 200.00 ) ;
			PrintRecords() ;

			// Both accounts exist so the transfer is completed
			// and the database is updated

			Transfer( 12345, 55555, 150.50 ) ;
			PrintRecords() ;

			codeBase.initUndo( ) ;
		}
	}
}
